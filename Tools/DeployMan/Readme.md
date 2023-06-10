DeployMan
=========
                                 
Simplify the deployment of files and folders for iOS and 
Android apps written in Delphi. It is especially useful 
if you need to deploy a lot of files, such as 3rd party SDKs. 
This tool was inspired by [GrijjyDeployMan](https://github.com/grijjy/GrijjyDeployMan) 
made by Allen Drennan
 
Usage
-----

```
  DeployMan.exe
    -DProj: Path to the project file (*.dproj).
    -Paths: <LocalPath>|<IncludeSubDirs>|<RemotePath>. Separate paths with ';'.
    -Configurations: Default Debug;Release. Separate Configurations with ';'.
    -Platforms: Default Android;Android64;iOSDevice64;iOSSimARM64. Separate Platforms with ';'.
    -DProjNormalizer: Path to the Alcinoe DProjNormalizer tool.
    -NoInteraction: Non-interactive mode.
```

Example
-------

```
  DeployMan.exe^
    -DProj=c:\MyProject\MyProject.dproj^
    -Paths=c:\MyProject\MyDirA|true|./MyDirA;c:\MyProject\MyFileB.json|false|./MyFileB.jsonB^
    -DProjNormalizer=c:\Alcinoe\Tools\DeployProjNormalizer\DeployProjNormalizer.exe^
    -Platforms=iOSDevice64
```
    
You can also check the ALNotificationService Demo and in particular the script:
[{Alcinoe}\Demos\ALNotificationService\_source\ios\DeployMan.bat](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALNotificationService/_source/ios/DeployMan.bat)