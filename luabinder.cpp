/*
    Contributed by Elie 'Syniurge' Morisse (syniurge@gmail.com)
 
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdlib.h>
#include <string.h>

#include <iostream>
#include <fstream>
#include <sstream>
#include <stack>
#include <queue>
#include <deque>
#include <vector>
#include <map>
#include <unordered_map>

#include <boost/format.hpp>
#include <boost/filesystem.hpp>

#include <clang-c/Index.h>

#ifndef PATH_MAX
# define PATH_MAX 4096 // TODO: use Boost filesystem
#endif

using namespace std;

// 1st command line parameter, output folder
char outputFolder [PATH_MAX];

// 2nd command line parameter, cut out all declarations that are outside this folder
char constraintInputFolder [PATH_MAX];

// Name used e.g for the Luabind global register function and its header
std::string globalNamespace = ""; // = first namespace declaration encountered

// Some nice 21st-century wrapper
class CXStringEx : public CXString {
public:
	CXStringEx () {}
	CXStringEx (const CXString s) { data = s.data; private_flags = s.private_flags; }
	~CXStringEx () { clang_disposeString(*this); }
	
	CXStringEx& operator= (const CXString& s) { data = s.data; private_flags = s.private_flags; return *this; }
	operator CXString() { return (*static_cast<CXString *> (this)); }
};

// I have looked everywhere, libclang doesn't provide a way to differentiate overloaded operators from other methods like clang does, but that's maybe because they have a standard name in the AST which is easily recognizable anyway (WARNING: but will it stay that way?)

enum CXOverloadedOperatorKind {
	CXOO_None = 0,
#define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
	CXOO_##Name,
	// NOTE: I don't have any example of overloaded new, delete, () or [] operator
#include "clang/Basic/OperatorKinds.def"
	CXOO_Invalid
};

// The Clang code that identifies the OverloadedOperatorKind is fairly obscure and complex, so I wrote my own simple function which should normally be doing the same thing afaiu

class CXOverloadedOperatorKindMap : public std::unordered_map<std::decay<std::string>::type, CXOverloadedOperatorKind> {
public:
	CXOverloadedOperatorKindMap() : std::unordered_map<std::decay<std::string>::type, CXOverloadedOperatorKind>() {
		insert(std::pair<std::string, CXOverloadedOperatorKind>("", CXOO_None));
#define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
		insert(std::pair<std::string, CXOverloadedOperatorKind>(Spelling, CXOO_##Name));
#include "clang/Basic/OperatorKinds.def"
// 		insert(std::pair<std::string, CXOverloadedOperatorKind>("", CXOO_Invalid));
	}
} _CXOverloadedOperatorKindMap;

CXOverloadedOperatorKind clang_getCursorOverloadedOperatorKind (CXCursor cursor) {
	CXStringEx cursorSpelling = clang_getCursorSpelling(cursor);
	
	std::string operatorKindSpelling = clang_getCString(cursorSpelling);
	if (operatorKindSpelling.substr(0, strlen("operator")) != "operator")
		return _CXOverloadedOperatorKindMap[""];
	operatorKindSpelling.erase(0, strlen("operator"));
	
	return _CXOverloadedOperatorKindMap[operatorKindSpelling];
}

// Reverse map handy because the LUA equivalents of the C++ operators have the same name when they exist

class CXOverloadedOperatorKindReverseMap : public std::map<CXOverloadedOperatorKind, std::string> {
public:
	CXOverloadedOperatorKindReverseMap() : std::map<CXOverloadedOperatorKind, std::string>() {
// 		insert(std::pair<CXOverloadedOperatorKind, std::string>(CXOO_None, ""));
#define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
		insert(std::pair<CXOverloadedOperatorKind, std::string>(CXOO_##Name, Spelling));
#include "clang/Basic/OperatorKinds.def"
	}
} _CXOverloadedOperatorKindReverseMap;

std::string clang_getTemplateArgumentsSpelling (CXCursor cursor);
std::string clang_getCursorFullSemanticSpelling (CXCursor cursor) { // needs a better name
	CXStringEx cursorSpelling = clang_getCursorSpelling (cursor);
	CXCursor templateDecl = clang_getSpecializedCursorTemplate (cursor);
	
	return (boost::format("%1%%2%") % clang_getCString (cursorSpelling) % clang_getTemplateArgumentsSpelling (cursor)).str();
}

// TODO: it's a bit dumb to generate the "namespace::namespace::class::" every time instead of only when needed
std::string FullSemanticPrefix (CXCursor cursor) {
	std::string result = "";
	
	CXCursor semanticParent = clang_getCursorSemanticParent (cursor);
	std::string semanticParentName = clang_getCursorFullSemanticSpelling (semanticParent);
	
	while (!clang_Cursor_isNull(semanticParent) && !clang_isTranslationUnit(semanticParent.kind)) { // NOTE: some semantic parents have empty names, like anonymous enum
		if (clang_getCursorKind (semanticParent) != CXCursor_EnumDecl // in c++ (before c++11) enum values can't be accessed via enumname::
				&& !semanticParentName.empty ()) {
			result.insert(0, "::");
			result.insert(0, semanticParentName);
		}
		
		semanticParent = clang_getCursorSemanticParent (semanticParent);
		semanticParentName = clang_getCursorFullSemanticSpelling (semanticParent);
	}
	
	return result;
}

// Another missing feature that other people have mentioned: clang_getTypeKindSpelling only returns the spelling of the internal enum, and there seems to be no way to obtain something like "int" for the type int. This seems really basic and yet I've double-checked every occurrence of CXString in Index.h.
// libclang leaves me no choice but to reconstruct entirely every type name.

// ulterior NOTE: actually Clang has an extensive type printer (TypePrinter.cpp) tunable with PrintingPolicy that isn't but could be exposed through libclang, but it lacks one important feature for our usage which is to not print the default arguments of a template specialization
// the changes needed for that are quite complex as mentioned elsewhere, so I'll keep my redundant reconstruction code for now
// also PrintingPolicy needs to have more options to fit the needs of luabinder

// The reference I had to use for the built-in types - CXTypeKind correspondances was clang/AST/BuiltinTypes.def which unfortunately couldn't be used directly like OperatorKinds.def

const char * clang_getBuiltinTypeKindCSpelling (CXTypeKind kind) {
	switch (kind) {
	case CXType_Void:
		return "void";
	case CXType_Bool:
		return "bool";
	case CXType_Char_U:
		return "char";
	case CXType_UChar:
		return "unsigned char";
	case CXType_Char16:
		return "char16_t";
	case CXType_Char32:
		return "char32_t";
	case CXType_UShort:
		return "unsigned short";
	case CXType_UInt:
		return "unsigned int";
	case CXType_ULong:
		return "unsigned long";
	case CXType_ULongLong:
		return "unsigned long long";
	case CXType_UInt128:
		return "__uint128_t";
	case CXType_Char_S:
		return "char";
	case CXType_SChar:
		return "signed char";
	case CXType_WChar:
		return "wchar_t";
	case CXType_Short:
		return "short";
	case CXType_Int:
		return "int";
	case CXType_Long:
		return "long";
	case CXType_LongLong:
		return "long long";
	case CXType_Int128:
		return "__int128_t";
	case CXType_Float:
		return "float";
	case CXType_Double:
		return "double";
	case CXType_LongDouble:
		return "long double";
// 	case CXType_NullPtr:
// 		return "nullptr";
	default:
		return "";
	}
}

bool clang_isBuiltinTypeKind (CXTypeKind kind) {
	switch (kind) {
	case CXType_Void:
	case CXType_Bool:
	case CXType_Char_U:
	case CXType_UChar:
	case CXType_Char16:
	case CXType_Char32:
	case CXType_UShort:
	case CXType_UInt:
	case CXType_ULong:
	case CXType_ULongLong:
	case CXType_UInt128:
	case CXType_Char_S:
	case CXType_SChar:
	case CXType_WChar:
	case CXType_Short:
	case CXType_Int:
	case CXType_Long:
	case CXType_LongLong:
	case CXType_Int128:
	case CXType_Float:
	case CXType_Double:
	case CXType_LongDouble:
// 	case CXType_NullPtr:
		return true;
	default:
		return false;
	}
}

// NOTE: no support for variadic functions (the ones that take a variable number of arguments, i.e with ... iirc), but Luabind don't support them anyway
std::string clang_getArgsTypesSpelling (CXType type);

bool isNonCppIdentifierChar (char c) { return ! (isalnum (c) || c == '_'); }

// HACK HACK HACK

// There seems to be a bug somewhere in libclang or clang, since there is a VisitClassTemplateSpecializationDecl implementation in CIndex.cpp, and that implementation is supposed to visit the specialization parameters, but a visitChildren on the type declaration despite being spelled for example "std::pair" cannot find any children, am I doing something wrong?

// FunkMonkey implemented many missing features in libclang such as a working way to retrieve the parameters of a template specialization
// Its full patch against clang 3.1 can be found at: https://github.com/FunkMonkey/libClang/compare/master...dev_templates
// it also contains a function to check if a method is const, but I already do that with the USR

// Unfortunately it wouldn't be wise to put these features directly in luabinder, because they require linking against Clang's static libs which impose -fno-rtti and -fno-exceptions (exceptions are needed by Boost, the RTTI is currently used for the luabind::Nodes)
// Hence if you want to build luabinder, you'll need to rebuild clang after applying FunkMonkey's patch (provided with luabinder.cpp)
// To build clang/llvm refer to http://clang.llvm.org/get_started.html, compilation time about 30 mins (the SVN version may be incompatible with the patch, use the clang 3.1 release)

// enum CXChildVisitResult clang_templateParametersVisitor (CXCursor cursor, CXCursor parent, CXClientData client_data) {	
// // 	if (clang_getCursorKind (cursor) == CXCursor_TemplateTypeParameter) // NOTE: this only works for the template declaration, not its specializations
// 		cout << clang_getCString (clang_getCursorSpelling (cursor)) << endl;
// 	
// 	return CXChildVisit_Continue;
// }

// MORE HACK

// And.... FunkMonkey's functions proved to be not enough
// C++ templates and Clang have led me to undreamed horizons, and while most the writing of luabinder went smoothly I've spent 1.25 days of my life trying to to find a way to cut out the default arguments in template specialization declarations
// After trying to modify libclang in many ways, what I began to suspect was finally confirmed by http://clang-developers.42468.n3.nabble.com/Adding-template-default-parameter-with-as-typed-explicit-parameter-tp4026939.html
// that the non-default arguments are lost after Sema deduces the true arguments
// therefore, as said in the thread, it's Clang itself that must be modified to preserve the non-default arguments

// one of the problem is that the same deduced template specialization type could have been deduced from different template specialization declarations, for example:

// template <typename T, typename U>
// class machin {
// 	machin() {}
// }
// 
// template <typename U> machinbool <bool, U>;
// 
// typedef machin<bool, int> machinintbool;
// typedef machinbool<int> machinintboolalias;

// and since Sema resolves every type to its full canonical form and Clang cares mainly about that, it's natural that the argument hierarchy is lost
// Deep changes or hacks will be needed to preserve that hierarchy without breaking "type canonicalness".

std::string clang_getTypeCSpelling (CXType type, std::string semanticAccessPrefix = "", bool fromPointer = false, bool isConstFunction = false);

std::string clang_getTemplateArgumentsSpelling (CXCursor cursor) { // with the < >
	// NOTE: currently only works with template specializations, could me made to work with templates and partial specializations
	
	CXCursor templateDecl = clang_getSpecializedCursorTemplate (cursor);
	
	if (clang_Cursor_isNull (templateDecl))
		return ""; // not a template specialization
	
	// Here come the functions added by FunkMonkey
	unsigned numArgs = clang_getTemplateSpecializationNumArguments (cursor);
	
	std::string argsSpelling = "<";
	for (unsigned i = 0; i < numArgs; ++i) {
		if (clang_isTemplateSpecializationArgumentDefault (cursor, i))
			break;
		
		if (i != 0)
			argsSpelling += ", ";
		argsSpelling += clang_getTypeCSpelling (clang_getTemplateArgumentValueAsType (clang_getTemplateSpecializationArgument (cursor, i)));
	}	
	argsSpelling += ">";
	
	return argsSpelling;
}

// New lazy version that uses Clang's mindpower instead of reimplementing everything (defaults arguments, non-type parameters, ...)
// std::string clang_getTemplateArgumentsSpelling (CXCursor cursor) {
// // 	CXCursor templateDecl = clang_getSpecializedCursorTemplate (cursor);
// // 	
// // 	if (clang_Cursor_isNull (templateDecl))
// // 		return ""; // not a templateDecl
// 	
// 	CXStringEx argsSpelling = clang_getTemplateSpecializationArgumentsSpellingHack (cursor);
// 	return clang_getCString (argsSpelling);
// }

bool clang_isConstQualifiedFromUSR (CXCursor cursor);

// HACK: in the luabind cursor visitor we "twist " fromPointer to make it think that a function member is a pointer, since we need the type of &function
std::string clang_getTypeCSpelling (CXType type, std::string semanticAccessPrefix, bool fromPointer, bool isConstFunction) { // semanticAccessPrefix, fromPointer, and isConstFunction are somewhat hackish and only used for function types, semanticAccessPrefix and isConstFunction can only be determined from the cursor
	std::string typeSpelling;
	
	switch (type.kind) {
	case CXType_Unexposed:
	case CXType_Record:
	case CXType_Enum:
	case CXType_Typedef:
	case CXType_TemplateSpecialization:
	case CXType_Elaborated: // NOTE: it seems that many template specializations are CXType_Elaborated?
		{
			CXCursor typeDecl = clang_getTypeDeclaration (type);
			CXStringEx typeDeclSpellingCXString =  clang_getCursorSpelling (typeDecl);
// 			typeSpelling = (boost::format("%2%%1%") % clang_getCString (typeDeclSpellingCXString) % FullSemanticPrefix (typeDecl)).str();
			
// 			if (type.kind == CXType_TemplateSpecialization) { // WARNING: in vanilla libclang it seems that all template specializations are CXType_Unexposed, no idea if that's an universal truth
				typeSpelling = (boost::format("%2%%1%%3%") % clang_getCString (typeDeclSpellingCXString) % FullSemanticPrefix (typeDecl) % clang_getTemplateArgumentsSpelling (typeDecl)).str();
// 			}
		}
		break;
		
	case CXType_Pointer:
		typeSpelling = clang_getTypeCSpelling (clang_getPointeeType (type), semanticAccessPrefix, true);
		typeSpelling += " *";
		break;
		
	case CXType_LValueReference:
	case CXType_RValueReference:
		typeSpelling = clang_getTypeCSpelling (clang_getPointeeType (type));
		typeSpelling += "&";
		break;
		
	case CXType_FunctionProto:
 	case CXType_FunctionNoProto:
		{
			std::string resultTypeSpelling = clang_getTypeCSpelling (clang_getResultType (type));
			
// 			if (*(std::find_if (resultTypeSpelling.begin(), resultTypeSpelling.end(), &isNonCppIdentifierChar)) != '\0') // if true it's a compound name, e.g "unsigned int" or "void *", it should be enclosed within parenthesis
// 				resultTypeSpelling = (boost::format("(%1%)") % resultTypeSpelling).str();
			// NOTE: Actually this was wrong, compound type names for results MUST NOT be within parenthesis, or compilation of the register functions will fail, I'm not sure why though
			
			typeSpelling = (boost::format("(%1% (%3%%4%) (%2%)%5%)") % resultTypeSpelling % clang_getArgsTypesSpelling (type) % semanticAccessPrefix % (fromPointer ? "*" : "") % (isConstFunction ? " const" : "")).str();
		}
		break;
		
	default:
		 typeSpelling = clang_getBuiltinTypeKindCSpelling (type.kind);
	}
	
	if (clang_isConstQualifiedType (type))
		typeSpelling.insert (0, "const ");
	
	return typeSpelling;
}


std::string clang_getArgsTypesSpelling (CXType type) { // type is assumed to be a function
	int numArgs = clang_getNumArgTypes (type);
	std::string argsTypes = "";
	
	for (unsigned i = 0; i < numArgs; ++i) {
		if (i != 0)
			argsTypes += ", ";
		
		argsTypes += clang_getTypeCSpelling (clang_getArgType (type, i));
	}
	
	return argsTypes;
}

// Yet another important missing feature in libclang I'm not the first to stumble upon: despite the existence of clang_isConstQualifiedType and clang_isVolatileQualifiedType, there's no way to know whether a class method is const or not 
// The following workaround was taken from a Stack Overflow thread http://stackoverflow.com/questions/12026551/how-to-find-out-whether-a-member-function-is-const-or-volatile-with-libclang

// There's a mistake however, the bangLocation is the last # and not the first occurrence, which is for the first argument. Also int or unsigned int isn't safe, libclang put the flags as a char in the USR.

char parseUSRString (const std::string& usrString) {
	size_t bangLocation = usrString.rfind('#');
	if (bangLocation == std::string::npos || bangLocation == usrString.length() - 1)
		return 0;
	bangLocation++;
	return usrString[bangLocation];

// 	*isConst = x & 0x1;
// 	*isVolatile = x & 0x4;
// 	*isRestrict = x & 0x2;
}

bool clang_isConstQualifiedFromUSR (CXCursor cursor) {
	CXStringEx cursorUsr = clang_getCursorUSR (cursor);
// 	cout << clang_getCString(cursorUsr) << endl;
	return ((parseUSRString (clang_getCString (cursorUsr)) & 0x1) != 0);
}

// The following function isn't mine and was taken from https://svn.boost.org/trac/boost/ticket/1976
boost::filesystem::path naive_uncomplete (boost::filesystem::path const path, boost::filesystem::path const base) {
    if (path.has_root_path()){
        if (path.root_path() != base.root_path()) {
            return path;
        } else {
            return naive_uncomplete(path.relative_path(), base.relative_path());
        }
    } else {
        if (base.has_root_path()) {
            throw "cannot uncomplete a path relative path from a rooted base";
        } else {
            typedef boost::filesystem::path::const_iterator path_iterator;
            path_iterator path_it = path.begin();
            path_iterator base_it = base.begin();
            while ( path_it != path.end() && base_it != base.end() ) {
                if (*path_it != *base_it) break;
                ++path_it; ++base_it;
            }
            boost::filesystem::path result;
            for (; base_it != base.end(); ++base_it) {
                result /= "..";
            }
            for (; path_it != path.end(); ++path_it) {
                result /= *path_it;
            }
            return result;
        }
    }
}

namespace luabind {
	
	// Even though it might look obscure at first, this approach is more elegant and problem-proof than in the previous version where I handled tab depth, post def commas, namespace/class/scope/etc. declarations in a straightforward way
	// Basically it's a hierarchy of properly tabbed text nodes that are filled by the Clang cursor visitor but the text is added to the parent node (with prefix and suffix, and if the contents aren't empty, e.g if it's not a completely private class) only when the node is destroyed, hence a node can have multiple children that handles different sections but the sections won't overlap
	
	class Node : public std::stringstream {
	public:
		Node (Node* _parent, const std::string& _prefix, const std::string& _suffix, bool _discardIfEmpty = true) : parent(_parent), prefix(_prefix), suffix(_suffix), discardIfEmpty(_discardIfEmpty), tabDepth(0), isFinalized(false) {}
		
		// SEMI HACK: the prefix and suffix have to be added when the destructor of LuabindRegisterNode is called, but calling virtual functions (onNotDiscarded) from destructors is pointless because all information about the derived types is supposed to be lost at this point
		// TODO: There may be a better way to do this but I've spent too much time wrapping my head already
		
		bool isFinalized;
		inline void Finalize() {
			if (isFinalized)
				return;
			
			if (!discardIfEmpty || !str().empty()) {
				// UGLY, didn't realize that this isn't really a stream that we want, but something where stuff could be inserted, so if I had to redo it Node wouldn't derive from stringstream nor anything else but would simply manipulate a string member TODO also str() makes new copies every goddamn time
				std::string c (str());
				seekp (0, ios::beg);
				if (!prefix.empty())
					*this << prefix << endl;
				*this << c;
				if (!suffix.empty())
					*this << suffix << endl;
			}
			
			isFinalized = true;
		}
		
		virtual ~Node () {
			Finalize();
			if (!discardIfEmpty || !str().empty()) {
// 				onNotDiscarded(); // doesn't work, calls Node's version instead of the derived one
				
				if (parent != nullptr)
					*parent += *this;
			}
		}
		
		Node & operator += (const std::string &s) { *this << std::string(tabDepth, '\t') << s << endl; return *this; }
		
		Node & operator += (std::istream &t) { // NOTE: the stream position is altered by this function
			std::string s; 
			
			t.seekg(0, ios::beg);
			while (std::getline (t, s))
				*this += s;
			
			return *this;
		}
		
		Node & operator += (const boost::format &f) {
			std::istringstream iss(f.str());
			return operator+= (iss);
		}
		
// 		virtual void onNotDiscarded() {}
		
		Node* parent;
		std::string prefix;
		std::string suffix;
		bool discardIfEmpty;
		
		unsigned tabDepth;
	};
	
	struct ComplexConstantTableNode : public Node {
		 // Code inside the register function to add all constants of non-int types (FIXME: change the name?) after the module node and its class declarations
		 // NOTE: these constants can't be added through .def_readonly or .enum (which is for int only!)
		
		ComplexConstantTableNode (Node *_parent) : Node(_parent, "\nobject g = globals(L);", "", true) {}
	};	
	struct ComplexConstantNode : public Node { ComplexConstantNode (Node *_parent); };
	
	struct LuabindRegisterNode : public Node {
		static std::queue<std::string> registerFunctionHub;
		
		std::ostream &output;
		std::string registerFunctionName;
		
		ComplexConstantTableNode *constantTable;
		
		LuabindRegisterNode (std::ostream &_output, const std::string &_registerFunctionName) : Node(nullptr, (boost::format("inline void %1% (lua_State *L) {\n\tusing namespace luabind;\n") % _registerFunctionName).str(), "}", true), output(_output), registerFunctionName(_registerFunctionName), constantTable(new ComplexConstantTableNode(this)) { tabDepth = 1; }
		
		virtual ~LuabindRegisterNode() {
			delete constantTable;
			
			bool discardNode = !discardIfEmpty || !str().empty();
			
			Finalize();
			
			if (discardNode) {
				registerFunctionHub.push(registerFunctionName);
				output << str();
			}
		}
		
// 		virtual void onNotDiscarded() { registerFunctionHub.push(registerFunctionName); output << str(); }
	};
	std::queue<std::string> LuabindRegisterNode::registerFunctionHub;
	
	ComplexConstantNode::ComplexConstantNode (Node *_parent) : Node(_parent, "", "", true) {
		// NOTE: this assumes that there's a root LuabindRegisterNode
		while (typeid(*parent) != typeid(LuabindRegisterNode))
			parent = parent->parent; // these nodes are added anywhere in the hierarchy, so must find their way to the register function
		parent = static_cast<LuabindRegisterNode *>(parent)->constantTable;
	}
	
	inline void AddPostdefCommaIfNeeded (Node &node);
	
	struct DequeNode : public Node { // used to queue all function declarations, and to choose either the non-overloaded or the overloaded form
		typedef std::deque<Node *> _Deque;
		typedef _Deque::iterator iterator;
		
		_Deque nodes;
		
		DequeNode (Node *_parent) : Node (_parent, "", "") {}
		virtual ~DequeNode() {
 			while (!nodes.empty()) {
				delete nodes.front();
				nodes.pop_front();
			}
			
			AddPostdefCommaIfNeeded(*this);
		}
	};
	struct _FunctionDeque { 
		DequeNode * functions;
		
		_FunctionDeque(Node *_parent) : functions(new DequeNode(_parent)) {}
	};
	
	struct ModuleNode : public Node { ModuleNode (Node* _parent) : Node(_parent, "module(L)\n[", "];", true) { tabDepth = 1; } };
	struct NamespaceNode : public Node, public _FunctionDeque {
		NamespaceNode (Node* _parent, const std::string &_namespaceSpelling) : Node(_parent, (boost::format("namespace_(\"%1%\")\n[") % _namespaceSpelling).str(), "]", true), _FunctionDeque(this), namespaceSpelling(_namespaceSpelling) { tabDepth = 1; }
		virtual ~NamespaceNode () { delete functions; }
		
		std::string namespaceSpelling;
	};
	struct ScopeNode : public Node, public _FunctionDeque { ScopeNode (Node* _parent) : Node(_parent, ".scope\n[", "]", true), _FunctionDeque(this) { tabDepth = 1; } virtual ~ScopeNode () { delete functions; } }; // NOTE: typo in Luabind doc, they added a ; at the end of the scope
	struct EnumNode : public Node { EnumNode (Node* _parent, const std::string &enumSpelling) : Node(_parent, (boost::format(".enum_(\"%1%\")\n[") % enumSpelling).str(), "]", true) { tabDepth = 1; } };
	struct ClassNode : public Node, public _FunctionDeque {
		ClassNode (Node* _parent, CXCursor classCursor, CX_CXXAccessSpecifier _accessSpecifier) : Node(_parent, "", "", false), _FunctionDeque(this), className(clang_getCString(CXStringEx (clang_getCursorSpelling(classCursor)))), semanticPrefix(FullSemanticPrefix(classCursor)), bases(""), scope(new ScopeNode(this)), constants(new EnumNode(this, "__constants__")), numBases(0), accessSpecifier(_accessSpecifier) {
			ClassNode *classParent = dynamic_cast<ClassNode *> (parent);
			if (classParent != nullptr)
				parent = classParent->scope;
			
			tabDepth = 1;
		}
		
 		virtual ~ClassNode () {
			if (numBases >= 2)
				bases += ">";
			prefix = (boost::format ("class_<%2%%1%%3%>(\"%1%\")") % className % semanticPrefix % bases).str();
			
			delete functions; delete constants; delete scope;
			
			AddPostdefCommaIfNeeded(*this);
		}
		
		ScopeNode* scope;
		EnumNode* constants;
		
		std::string className;
		std::string semanticPrefix;
		
		std::string bases;
		unsigned numBases;
		
		inline void AddBase (CXCursor base) {
			if (numBases == 0)
				bases = ", ";
			else if (numBases == 1)
				bases.insert(2, "bases<");
			else
				bases += ", ";
			CXStringEx baseSpelling = clang_getCursorSpelling(base);
			bases += FullSemanticPrefix(base); bases += clang_getCString(baseSpelling);
			
			++numBases;
		}
		
		CX_CXXAccessSpecifier accessSpecifier;
	};
	
	struct EnumConstNode : public Node {
		EnumConstNode (Node* _parent) : Node(_parent, "", "") { tabDepth = 0; }
		virtual ~EnumConstNode() { AddPostdefCommaIfNeeded(*this); }
	};
	
	struct FunctionNode : public Node {
		std::string functionSpelling;
		
		std::string nonOverloadedDecl;
		std::string overloadedDecl;
		
		bool overloaded;
		
		Node *realParent;
		
		FunctionNode (Node* _parent, const std::string &_functionSpelling, bool isStatic = false) : Node(_parent, "", ""), realParent(_parent), functionSpelling(_functionSpelling), overloaded(false) {
			tabDepth = 0;
			
			if (isStatic && typeid(*parent) == typeid(ClassNode)) {
				parent = dynamic_cast<ClassNode *>(parent)->scope;
				realParent = parent;
			}
			
			 _FunctionDeque *functiondeque = dynamic_cast<_FunctionDeque *>(parent);
				 
			if (functiondeque != nullptr) {
				parent = functiondeque->functions;
				
				DequeNode::iterator it = functiondeque->functions->nodes.begin();
				while (it != functiondeque->functions->nodes.end()) {
					FunctionNode *it_f = static_cast<FunctionNode *> (*it);
					if (it_f->functionSpelling == functionSpelling) {
						overloaded = true;
						it_f->overloaded = true;
					}
					++it;
				}
				
				functiondeque->functions->nodes.push_back(this);
			}
		}
		virtual ~FunctionNode () {
			if (!nonOverloadedDecl.empty()) {
				if (typeid(*realParent) == typeid(ClassNode)) {
					nonOverloadedDecl.insert(0, ".");
					overloadedDecl.insert(0, ".");
				}
				
				if (overloaded)
					(*this) += overloadedDecl;
				else
					(*this) += nonOverloadedDecl;
				
				if (typeid(*realParent) != typeid(ClassNode))
					AddPostdefCommaIfNeeded(*this);
			}
		}
	};
	
	inline void AddPostdefCommaIfNeeded (Node &node) {
		if (node.str().empty())
			return;
		
		 if (typeid(*node.parent) != typeid(ClassNode)
				&& (!node.parent->str().empty())) { // slightly hackish way to add the comma at the end of the last method and not on a new line
			node.parent->seekp(-1, ios::cur);
			*node.parent << "," << endl;
		}
	}
	
// 	typedef boost::scoped_ptr<Node> Node_p; // weird errors
	typedef Node* Node_p;

	class IndexerClientData : public std::stack<Node_p> {
	public:
		IndexerClientData (CXTranslationUnit _transUnit) : transUnit(_transUnit), currentInputFilename("") {
			boost::filesystem::path outputHubPath = outputFolder;
			boost::filesystem::create_directories (outputHubPath);
			
			boost::filesystem::path hppHubPath = outputHubPath / "LuabindRegister.hpp";
			hppHubFile.open (hppHubPath.native(), ios::out | ios::binary);
			hppHubFile << "#pragma once" << endl << endl << "#include <luabind.hpp>" << endl << "#include <luabind/operator.hpp>" << endl << endl;
			
			boost::filesystem::path cppHubPath = outputHubPath / "LuabindRegister.cpp";
			cppHubFile.open (cppHubPath.native(), ios::out | ios::binary);
			cppHubFile << "#include \"LuabindRegister.hpp\"" << endl << endl;
		}
		
		~IndexerClientData() {
			FinalizeOutputFile ();
			
			hppHubFile << endl << (boost::format("void LuabindRegister_%1% (lua_State *L);") % globalNamespace).str() << endl;
			
			cppHubFile << (boost::format("void LuabindRegister_%1% (lua_State *L) {") % globalNamespace).str() << endl;
			while (!LuabindRegisterNode::registerFunctionHub.empty()) {
				cppHubFile << '\t' << (boost::format ("%1% (L);") % LuabindRegisterNode::registerFunctionHub.front()).str() << endl;
				LuabindRegisterNode::registerFunctionHub.pop();
			}
			cppHubFile << "}" << endl;
			
			cppHubFile.close ();
			hppHubFile.close ();
		}
		
		CXTranslationUnit transUnit;
		std::ofstream hppHubFile, cppHubFile;
		
		std::ofstream outputFile;
		
//  		void push(const Node_p p) { cout << "pushing " << typeid(*p).name() << endl; std::stack<Node_p>::push(p); }
		void pop() { /*cout << "popping " << typeid(*top()).name() << endl;*/ delete top(); std::stack<Node_p>::pop(); }
		
		inline void FinalizeOutputFile() {
			while (!empty())
				pop();
			
			outputFile.close ();
		}

		inline IndexerClientData& operator+= (boost::format& s) {
			*top() += s;
			return (*this);
		}
		
		inline IndexerClientData& operator+= (std::string& s) {
			*top() += s;
			return (*this);
		}
		
		inline IndexerClientData& operator+= (const char* s) {
			*top() += s;
			return (*this);
		}
		
		std::string currentInputFilename;
		
		inline void ChangeOutputFileIfNeeded (const std::string& newInputFilename) {
			if (currentInputFilename.compare (newInputFilename) == 0)
				return;

			// IMPORTANT WARNING: We assume that we always parse a whole file at once, which may be wrong
			if (outputFile.is_open()) {
				FinalizeOutputFile();
			}

			currentInputFilename = newInputFilename;

			std::string outputFilename;

			{
				boost::filesystem::path outputFolderPath = boost::filesystem::absolute (boost::filesystem::path (outputFolder));
				boost::filesystem::path newOutputFilenamePath = boost::filesystem::path (newInputFilename);

				std::string newOutputFilenamePathFilename = newOutputFilenamePath.filename().native();
				newOutputFilenamePathFilename.insert(0, "luabind_");

				newOutputFilenamePath = boost::filesystem::absolute (newOutputFilenamePath.parent_path() / boost::filesystem::path (newOutputFilenamePathFilename)) ;

				boost::filesystem::path relativeIncludePath = naive_uncomplete (newOutputFilenamePath, outputFolderPath);

				hppHubFile << (boost::format ("#include \"%1%\"") % relativeIncludePath.native()).str() << endl;

				outputFilename = newOutputFilenamePath.native();
			}

			boost::filesystem::create_directories (boost::filesystem::path(outputFilename).parent_path());
			outputFile.open (outputFilename.c_str(), ios::out | ios::binary); // from cplusplus' forum: « Keep in mind that things can go kablooey when not using ios::binary mode on your open file... »
			outputFile.seekp (0, ios::end);

			outputFile << "#pragma once" << endl << endl;

			std::string luabindRegisterSuffix = (boost::filesystem::path(newInputFilename).filename()).native();
			std::replace_if (luabindRegisterSuffix.begin(), luabindRegisterSuffix.end(), &isNonCppIdentifierChar, '_'); // WARNING: doesn't check if the first character is a numeral, which isn't allowed in C/C++ identifiers

			std::string luabindRegisterFunctionName = (boost::format("LuabindRegister_%1%") % luabindRegisterSuffix).str();

			push(new LuabindRegisterNode(outputFile, luabindRegisterFunctionName));
			push(new ModuleNode(top()));
		}
	};
	
	std::string GetCursorFilenameLocation (CXCursor cursor) {
		CXFile cursorFile;
		clang_getSpellingLocation (clang_getCursorLocation (cursor), &cursorFile, NULL, NULL, NULL);
		
		CXStringEx cursorFilename = clang_getFileName (cursorFile);
		char cursorFilenameReal [PATH_MAX];
		realpath (clang_getCString(cursorFilename), &cursorFilenameReal[0]);
		
		return cursorFilenameReal;
	}
	
	bool IsFileInsideInputFolder (const std::string& cursorFilename) {
		return (strncmp(constraintInputFolder, cursorFilename.c_str(), strlen(constraintInputFolder)) == 0);
	}
	
	// Even if a type is defined outside the input folder, it may still need to be bound by Luabind
	// ex.: std::pair<bool, Real> Ogre::Math::intersects (const Ray &ray, const Plane &plane)
	// although here std::pair<bool, Real> is a template specialization hence pedantically its definition is inside the input folder, even though it'll need the external template definition as well
	// so better ex.: std::complex
	// NOTE: Luabind supports out of the box std::string and maybe other basic std types
	void AddTypeDependency (CXType type, IndexerClientData *IndexerClientData) {
		if (clang_isBuiltinTypeKind (type.kind))
			return;
		
		if (type.kind == CXType_Pointer || type.kind == CXType_LValueReference) {
			AddTypeDependency (clang_getPointeeType(type), IndexerClientData);
			return;
		}
		
		std::string typeSpelling = clang_getTypeCSpelling(type);
		
		// Luabind provides a converter for std::string already, in luabind/detail/policy.hpp
		// NOTE: it may be interesting to implement converters rather than bind the class, e.g for std::pair		
		if (typeSpelling == "std::string")
			return;
		
		CXCursor typeDecl = clang_getTypeDeclaration(type);
		std::string typeDeclFilename = GetCursorFilenameLocation (typeDecl);
		
		if (IsFileInsideInputFolder (typeDeclFilename))
			return;
		
	}
	
	void AddEnumConstant (CXCursor cursor, Node* enumNode) {
		EnumConstNode enumConstNode (enumNode);
		
		CXStringEx cursorSpelling = clang_getCursorSpelling(cursor);
		enumConstNode += boost::format ("value(\"%1%\", %2%%1%)") % clang_getCString(cursorSpelling) % FullSemanticPrefix(cursor);
	}
	
	enum CXChildVisitResult CursorVisitor_Enum (CXCursor cursor, CXCursor parent, CXClientData client_data) {
		IndexerClientData* indexerClientData = (IndexerClientData*) client_data;
		
		if (clang_getCursorKind (cursor) == CXCursor_EnumConstantDecl)
			AddEnumConstant (cursor, indexerClientData->top());
		
		return CXChildVisit_Continue;
	}

	// NOTE: We never use CXChildVisit_Recurse since recursivity needs to be handled in a special way
	enum CXChildVisitResult CursorVisitor (CXCursor cursor, CXCursor parent, CXClientData client_data) {
		IndexerClientData* indexerClientData = (IndexerClientData*) client_data;
		CXStringEx cursorSpelling = clang_getCursorSpelling(cursor);
		
		std::string cursorFilename = GetCursorFilenameLocation (cursor);
		
		// We first check if the declaration is located inside the input folder
		if (!IsFileInsideInputFolder (cursorFilename))
			return CXChildVisit_Continue;
		
		// Prepares the output file
		indexerClientData->ChangeOutputFileIfNeeded(std::string(cursorFilename).replace(0, strlen(constraintInputFolder), outputFolder));
		
		ClassNode *classNode = dynamic_cast<ClassNode *>(indexerClientData->top());
		
		if (clang_getCursorKind (cursor) == CXCursor_CXXBaseSpecifier) {
			classNode->AddBase (clang_getCursorReferenced (cursor));
			return CXChildVisit_Continue;
		}
		
		// Stumbled upon public:, protected: or private:
		if (clang_getCursorKind (cursor) == CXCursor_CXXAccessSpecifier) { // It's strange that libclang doesn't provide its own way to get the access control level of any cursor, a.t.m clang_getCXXAccessSpecifier only works with CXCursor_CXXAccessSpecifier
			if (clang_getCXXAccessSpecifier(cursor) == CX_CXXInvalidAccessSpecifier) {
				throw "Invalid access specifier, shouldn't happen";
			} else {
				if (classNode != nullptr)
					classNode->accessSpecifier = clang_getCXXAccessSpecifier(cursor);
				return CXChildVisit_Continue;
			}
		}
		
		// We can only expose public stuff
		if (classNode != nullptr)
			if (classNode->accessSpecifier == CX_CXXPrivate || classNode->accessSpecifier == CX_CXXProtected)
				return CXChildVisit_Continue;
		
		// Checks if this isn't an à-posteriori definition
		if (!clang_equalCursors(clang_getCursorLexicalParent(cursor), clang_getCursorSemanticParent(cursor)))
			return CXChildVisit_Continue;
		
		// Checks if this isn't just a preliminary declaration
		CXCursor cursorDefinition = clang_getCursorDefinition(cursor);
		if (!clang_Cursor_isNull(cursorDefinition) && !clang_isCursorDefinition(cursor) && clang_equalCursors(clang_getCursorLexicalParent(cursorDefinition), clang_getCursorSemanticParent(cursorDefinition))) // the last condition checks whether the definition occurs later but without leaving the same scope declaration
 			return CXChildVisit_Continue;
		
// 		if (!clang_isCursorDefinition())
		
		switch (clang_getCursorKind (cursor)) {
		case CXCursor_Namespace: // FIXME: doesn't handle CXCursor_NamespaceAlias
			if (globalNamespace.empty())
				globalNamespace = clang_getCString (cursorSpelling);

			indexerClientData->push(new NamespaceNode(indexerClientData->top(), clang_getCString (cursorSpelling)));
			clang_visitChildren (cursor, &CursorVisitor, client_data);
			indexerClientData->pop();
			break;
			
		case CXCursor_StructDecl: // NOTE: when a function takes a struct as parameter it may be cooler to use a table rather than an userdata
			indexerClientData->push(new ClassNode(indexerClientData->top(), cursor, CX_CXXPublic));
			clang_visitChildren (cursor, &CursorVisitor, client_data);
			indexerClientData->pop();
			break;
			
		case CXCursor_ClassDecl:
			indexerClientData->push(new ClassNode(indexerClientData->top(), cursor, CX_CXXPrivate));
			clang_visitChildren (cursor, &CursorVisitor, client_data);
			indexerClientData->pop();
			break;
			
		case CXCursor_VarDecl: 
			// e.g static variables nested in classes
			if (classNode != nullptr) { // TODO: we could be binding global variables as well
				CXType cursorType = clang_getCursorType (cursor);
				
				// Special case for const int nested in classes
				if (clang_isConstQualifiedType (cursorType) && cursorType.kind == CXType_Int) {
					AddEnumConstant (cursor, classNode->constants);
					break;
				}

				ComplexConstantNode constantNode (indexerClientData->top());
				std::string luabindSemanticAccess = "";
				
				// not sure if HACK?: we need to reconstruct the hierarchy using the indexerClientData's stack instead of FullSemanticPrefix here
				Node *parentNode = indexerClientData->top();
				while (typeid(*parentNode) != typeid(ModuleNode)) {
					if (NamespaceNode *namespaceNode = dynamic_cast<NamespaceNode *>(parentNode))
						luabindSemanticAccess.insert(0, (boost::format("[\"%1%\"]") % namespaceNode->namespaceSpelling).str());
					else if (ClassNode *classParentNode = dynamic_cast<ClassNode *>(parentNode))
						luabindSemanticAccess.insert(0, (boost::format("[\"%1%\"]") % classParentNode->className).str());
						
					parentNode = parentNode->parent;
				}
				
				constantNode += boost::format ("g%2%[\"%1%\"] = %3%%1%;") % clang_getCString (cursorSpelling) % luabindSemanticAccess % FullSemanticPrefix(cursor);
			}
			break;
			
		case CXCursor_FieldDecl: // NOTE: Luabind docs doesn't mention the binding of anything outside classes apart from functions, but this needs to be checked (there are undocumented stuff like class_info for example)
// 			if (classNode == nullptr) 
// 				break;
			
			{
				CXType cursorType = clang_getCursorType (cursor);
				
				if (clang_isConstQualifiedType (cursorType)) {
					if (cursorType.kind != CXType_Int) // FIXME? it should be anything that isn't a int type
						(*indexerClientData) += boost::format(".def_readonly(\"%1%\", &%2%%1%)") % clang_getCString(cursorSpelling) % FullSemanticPrefix(cursor);
					else {
						if (classNode != nullptr)
							AddEnumConstant (cursor, classNode->constants);
					}
				} else
					(*indexerClientData) += boost::format(".def_readwrite(\"%1%\", &%2%%1%)") % clang_getCString(cursorSpelling) % FullSemanticPrefix(cursor);
			}
			
			break;
			
		case CXCursor_EnumDecl:
			// WARNING : Luabind documentation seems to hint that enums are only supported inside class declarations
			{
				bool classEnumHack = (classNode == nullptr);
			
				if (classEnumHack)
					indexerClientData->push(new ClassNode (indexerClientData->top(), cursor, CX_CXXPublic)); // TODO: enums do not seem to be supported outside classes, but that remained to be checked
				
				indexerClientData->push(new EnumNode (indexerClientData->top(), clang_getCString(cursorSpelling))); // NOTE: the name of the enum is never used in Luabind
				clang_visitChildren (cursor, &CursorVisitor_Enum, client_data);
				indexerClientData->pop();
				
				if (classEnumHack)
					indexerClientData->pop();
			}
			
			break;
			
		case CXCursor_FunctionDecl:
		case CXCursor_CXXMethod:
			{
				FunctionNode* functionNode = new FunctionNode(indexerClientData->top(), clang_getCString (cursorSpelling), clang_CXXMethod_isStatic (cursor) ? true : false);
				CXOverloadedOperatorKind operatorKind = clang_getCursorOverloadedOperatorKind(cursor);
				
 				if (operatorKind == CXOO_None) {
// 					indexerClientData->push (new FunctionNode (indexerClientData->top()));
					functionNode->nonOverloadedDecl = (boost::format ("def(\"%1%\", &%2%%1%)") % clang_getCString (cursorSpelling) % FullSemanticPrefix (cursor)).str();
					functionNode->overloadedDecl = (boost::format ("def(\"%1%\", %3% &%2%%1%)") % clang_getCString (cursorSpelling) % FullSemanticPrefix (cursor) % clang_getTypeCSpelling( clang_getCursorType(cursor), (clang_CXXMethod_isStatic (cursor) ? "" : FullSemanticPrefix (cursor)), true, (!clang_CXXMethod_isStatic (cursor) && clang_isConstQualifiedFromUSR (cursor) ? true : false))).str();
// 					indexerClientData->pop ();
				} else {
					switch (operatorKind) {
					case CXOO_Plus:
					case CXOO_Minus:
					case CXOO_Star:
					case CXOO_Slash:
					case CXOO_EqualEqual:
					case CXOO_Less:
					case CXOO_LessEqual:
						// TODO / FIXME: needs support for non-intrusive operator declarations, but that is going to mean a lot of changes
						// The operators in Ogre are often defined in files other than the one where the class is declared, so this will need some sort of node tree and to not write anything until everything is parsed
						// see also http://www.rasterbar.com/products/luabind/docs.html#splitting-class-registrations
						// but since the class is defined once we might as well not split the definition
						// actually I'm not even sure Luabind supports operators like operator + (Real &, Vector3 &) since Real is float
						if (classNode != nullptr && clang_getNumArgTypes (clang_getCursorType (cursor)) > 0) {
// 							indexerClientData->push (new FunctionNode (indexerClientData->top()));
							
							unsigned int rightArgIndex = (clang_getNumArgTypes (clang_getCursorType (cursor)) == 1) ? 0 : 1;
					
							CXType argType = clang_getArgType (clang_getCursorType (cursor), rightArgIndex);
							std::string argTypeSpelling = clang_getTypeCSpelling (argType);
							
							// This is a HACK, the compiler complains when the argument is not enclosed within other<> and is const
							// TODO the right way is to remove the const, this is where PrintingPolicy and maybe saying byebye to clang_getTypeCSpelling should be considered (Clang knows how to reconstruct types too)
							if (clang_isBuiltinTypeKind (argType.kind))
								argTypeSpelling = clang_getBuiltinTypeKindCSpelling (argType.kind);
							
							bool isArgTypeComplex = (argType.kind == CXType_Unexposed || argType.kind == CXType_Record || argType.kind == CXType_LValueReference);
							
							if (isArgTypeComplex) {
								argTypeSpelling = (boost::format ("other<%1%>") % argTypeSpelling).str();
							} else if (*(std::find_if (argTypeSpelling.begin(), argTypeSpelling.end(), &isNonCppIdentifierChar)) != '\0') {
// 								argTypeSpelling = (boost::format ("(%1%)") % argTypeSpelling).str();
								// NOTE: MUST NOT BE inside parenthesis
							}
							
							bool isConst = clang_isConstQualifiedFromUSR (cursor) || clang_isConstQualifiedType (clang_getCursorResultType (cursor));// NOTE: not sure if I understood Luabind's doc correctly, « If your operator is const (or, when defined as a free function, takes a const reference to the class itself) you have to use const_self instead of self. »
							
							functionNode->nonOverloadedDecl = (boost::format ("def(%3% %1% %2%())") % _CXOverloadedOperatorKindReverseMap[operatorKind] % argTypeSpelling % (isConst ? "const_self" : "self")).str();
							functionNode->overloadedDecl = functionNode->nonOverloadedDecl;
						
// 							indexerClientData->pop ();
						}
						break;
						// TODO: call() and __tostring
					default:
						break;
					}
				}
			}
			break;
			
		case CXCursor_Constructor:
			// NOTE: this is another way to retrieve the arguments of a function, I'm leaving it as a reminder
			{
// 				indexerClientData->push (new FunctionNode (indexerClientData->top())); // not really needed, it's always inside a class
				
				int numArgs = clang_Cursor_getNumArguments (cursor);
				std::string argsTypes = "";
				
				for (unsigned i = 0; i < numArgs; ++i) {
					if (i != 0)
						argsTypes += ", ";
					
					argsTypes +=  clang_getTypeCSpelling (clang_getCursorType (clang_Cursor_getArgument (cursor, i)));
				}
				
				(*indexerClientData) += boost::format (".def(constructor<%1%>())") % argsTypes;
				
// 				indexerClientData->pop();
			}
			break;
			
		default:
			break;
		}
		
		return CXChildVisit_Continue;
	}
}

using namespace luabind;

// TODO: find out if we ever stumble upon a template specialization cursor in vanilla libclang
// enum CXChildVisitResult CursorVisitorTest (CXCursor cursor, CXCursor parent, CXClientData client_data) {
// 	return CXChildVisit_Continue;
// }

int main (int argc, const char **argv) {
	if (argc < 4) {
		cout << "Usage: " << "luabinder" << " <output folder> <input folder constraint> <Clang command-line parameters...>" << '\n';
		cout << '\t' << '\n';
		cout << '\t' << "Although many header files outside the input folder will be parsed, bindings will be generated only for the files inside the input folder constraint, with the exception of types for function arguments or return values and template specializations (e.g std::pair<bool, float>)." << '\n';
		exit(1);
	}
	
	realpath (argv[1], &outputFolder[0]);
// 	globalNamespace = basename(constraintInputFolder);
	
	realpath (argv[2], &constraintInputFolder[0]);
	
	CXIndex idx = clang_createIndex(0, 0);
	
	CXTranslationUnit transUnit = clang_parseTranslationUnit (idx, NULL, &argv[3], argc - 3, NULL, 0, CXTranslationUnit_Incomplete);
	IndexerClientData _indexerClientData (transUnit);
	
	for (unsigned i = 0, n = clang_getNumDiagnostics(transUnit); i != n; ++i) {
		CXDiagnostic diag = clang_getDiagnostic(transUnit, i);
		CXString str = clang_formatDiagnostic(diag, clang_defaultDiagnosticDisplayOptions());
		
		cerr << clang_getCString(str) << endl;
		clang_disposeString(str);
	}
	
	unsigned result = clang_visitChildren (clang_getTranslationUnitCursor (transUnit), &CursorVisitor, &_indexerClientData);
	
	clang_disposeTranslationUnit(transUnit);
	clang_disposeIndex(idx);
	
	return result;
}