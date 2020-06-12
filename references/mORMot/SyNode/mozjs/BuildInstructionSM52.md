# SpiderMonkey 52
Main information about SpiderMonkey is [here](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey)

## Build instruction
Official Mozilla instruction is [here](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Build_Documentation)
Let's describe the details

### Preparing
In first step you must install the next items:
  - Visual Studio(2013 for SpiderMonkey 45, or 2015 for SpiderMonkey 52).
  - [MozillaBuild](https://wiki.mozilla.org/MozillaBuild)
  - [Mercurial](https://www.mercurial-scm.org/downloads). At the moment of writing this instruction last version(4.1) could not download source code.
The source code was downloaded by version 4.0

The next step is downloading SpiderMonkey source code:
  - *(recommended method)* Download from [official release page](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Releases).
At the moment of writing this instruction, version 52 was absent in this page, so we could not use this method. But 45 version was present.
  - Download directly from [repository](https://hg.mozilla.org/releases) *(the version may be unstable)*. It is placed at folder `mozilla-esr<version_number>`

### Modification of SpiderMonkey source code for Delphi compability
Our main task is to translate everything that will be imported to Delphi into `extern “C”` mode, and to write wrappers to things which cannot be translated

Usually it is enough to edit this files:
 - js\public\Initialization.h
 - js\src\jsfriendapi.h
 - js\src\jsapi.h
 
by adding `extern "C"` to functions that we need, and in files:
 - js\src\jsapi.h
 - js\src\jsapi.cpp

we must to write the wrappers to functions which we cannot import directly(that ones which use class instances).

For realization in Delphi we need to have an idea about internal structure of SpiderMonkey instances.
The simplest way is to create C++ project in Visual Studio and run it in debug mode.
Then we can create variables of unclear types and examine them in "Watch" window.
Interesting for us information is `sizeof(variable)` and offset of addresses of fields of variables.
This information is enough to understand structure.

For working with objects which allocated in stack we need to write wrappers, which works with `void*` type and apply `reinterpret_cast` to them

For SpiderMonkey 52 we created a patch `Delphi patch 52.patch` (for version from repository, *for version from release page it may be could not be applied)

### Building
Then run MozillaBuild. It located in the directory where we install it (`C:\mozilla-build` by default). There are different *.bat files in this directory.
For SpiderMonkey 45 we need `start-shell-msvc2013.bat`(for 32-bit version) or `start-shell-msvc2013-x64.bat`(for 64-bit version)
For SpiderMonkey 52 we need `start-shell-msvc2015.bat`(for 32-bit version) or `start-shell-msvc2015-x64.bat`(for 64-bit version)
Now we can see MozillaBuild console.
Then we go to the source code folder(we downloaded them to `d:\mozilla-releases\mozilla-esr52` folder).

    cd d:
    cd mozilla-releases\mozilla-esr52
    cd js\src

Then we recommend to create new folder for binary files location

    mkdir obj
    cd obj

For 64-bit version we create `obj64` folder
Then run build configurator:

    ../configure --enable-ctypes --disable-jemalloc [--disable-sm-promise]

For 64-bit version:

    ../configure --enable-ctypes --disable-jemalloc [--disable-sm-promise] --host=x86_64-pc-mingw32 --target=x86_64-pc-mingw32
    
 `--enable-ctypes` activate [ctypes](https://developer.mozilla.org/en-US/docs/Mozilla/js-ctypes)(Mozilla realization of [ffi](https://github.com/ffi/ffi) in JS layer).
 
 `--disable-jemalloc` deactivate jemalloc(memory manager). We use FastMM and if conflicts with jemalloc - there are AV on application exit.

 `--disable-sm-promise` deactivate promises if you don't need them. At the moment of writing this instruction deactivation of promises fail build.
If you not deactivate promises you must set promises callbacs `SetEnqueuePromiseJobCallback`, `SetGetIncumbentGlobalCallback` and `SetAsyncTaskCallbacks` else you get AV when work with promise.

Then we can run build

    mozmake.exe

After successfully build binary files will be located in `dist\bin` folder. We need to take all `*.dll` from here.
In order to be aligned with Linux version, the name of the mozjs-52.dll file should be changed to synmozjs52.dll