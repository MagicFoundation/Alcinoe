#  The server-side JavaScript execution using the SpiderMonkey library with nodeJS modules support

- win32/64 target (FPC 3.1.1, Delphi), linux 64 (FPC 3.1.1) 
- based on SpiderMonkey52, almost full support of ES6
- a remote debugger protocol, can be debugged remotely using Firefox - see `SyNode\Samples\02 - Bindings`
- CommonJS modules, compatible with NPM modules
- native modules can be implemented using Delphi/FPC (as a dll/so) - see `SyNode\Samples\01 - Dll Modules`
- JavaScript prototype definition based on Delphi RTTI (supported both "new" and old)

## SpiderMonkey library 

### SpiderMonkey 52 (recommended)

Precompiled binary can be downloaded here:

  - Win x32: https://unitybase.info/media/files/synmozjs52x32dlls.zip
  - Win x64: https://unitybase.info/media/files/synmozjs52x64dlls.zip
  - Linux x64: https://unitybase.info/media/files/libsynmozjs52.zip

Or compiled from sources as described [in  instructions inside mozjs folder](/mozjs)

### SpiderMonkey 45 (not supported)

Precompiled binary can be downloaded here:

 - x32: https://unitybase.info/downloads/mozjs-45.zip
 - x64: https://unitybase.info/downloads/mozjs-45-x64.zip
  
### Embadding SyNode JavaScript files into executable
Files from `core_modules` folder can be embadded into executable by enabling `CORE_MODULES_IN_RES`
define in the `SyNode.inc` file (enabled by default).

Compiled `core_modules.res` resources is commited into the git repositiry.

To create your own resource pack use a `/tool/core_res` tool to prepare files for embadding.

To compile rc -> res we use mingq tools under linux
```
sudo apt install mingw-w64 mingw-w64-tools
```

```bash
cd SyNode
./tools/core_res -i ./core_modules/ -o ./.resources/
cd ./resources
x86_64-w64-mingw32-windres ./core_res.rc ../core_modules.res
```

From inside JS files embadded into resources can be evaluated using `process.binding('modules').runInThisContextRes` function

Form Pascal code using `TSMEngine.EvaluateRes()` function



