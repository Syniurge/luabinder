luabinder
=========

luabinder is a libclang-based generator for Luabind. Thanks to the C++ understanding of Clang and its AST it automatically generates elegant Luabind register functions from headers for:
 - namespaces
 - classes
 - methods and global functions
 - enums
 - constants
 - operators
 - template specializations of templates declared inside the input folder as well as of some external templates whenever they are required by functions, e.g std::pair<bool, float>

Usage
========

    luabinder <output folder> <input folder constraint> <Clang command-line parameters...>

As in a standard compiler call, every header included by the input file is parsed by libclang, but bindings will be generated only for the files located inside the input folder "constraint", with the exception of types for function arguments or return values and template specializations. It creates headers that mirror the input folder structure (e.g bindings for classes in OgreEntity.h goes into a luabindOgreEntity.h in the output folder), and a global header and register function.

Ex.:

    luabinder ./LuabindOgre /usr/local/include/OGRE /usr/local/include/OGRE/Ogre.h

will create bindings for almost every "Luabind-able" declarations in Ogre headers.

When you have to expose a substancial part of libraries such as Ogre, Bullet, cAudio, libRocket, Recast ... writing bindings can take a very long time, and doing it the other way around by simply removing and commenting lines can save weeks of work.


NOTE: a frequent mistake is to believe that development tools released under the GPL such as Bison requires your project to be released under the GPL as well. That is not true, only if you wish to release a binary of Luabinder does the GPL requires you to release the modified source code of Luabinder.


Building
========

You'll first need to apply libclang-3.1_templates_funkmonkey.patch to libclang's source code and recompile it before building luabinder (refer to http://clang.llvm.org/get_started.html for LLVM and Clang building instructions).

At the time of writing (April 2013) libclang still doesn't expose template specialization parameters, and libclang-3.1_templates_funkmonkey.patch was generated shortly before LLVM/Clang 3.2, it may not last forever.

    mkdir build
    cmake .. && make
    sudo make install


Todo
======

Luabind is already pretty comprehensive, but two TODO remain:

 - support for operators declared outside classes (e.g operator+ (Vector3&, Float)), but even though it's fairly commonly used in Ogre for example, I'm not sure whether Luabind supports them or not.
 - removing needlessly wordy default template arguments in template specialization bindings, but this is actually due to a Clang limitation, since its Sema component always fully deduces the arguments of template specialization types and information about the original arguments is lost.

Also besides funkmonkey's patch I had to implement missing features in libclang, which doesn't expose many parts of Clang, This makes the code quite messy and it could use a cleanup once they are made available through libclang.
