Check DProjNormalizer/DeployProjNormalizer
------------------------------------------

* Delete everything in this directory except 
    clean.bat 
    README.md
    Project1.dproj.normalize.bat
    Project1.deployproj.normalize.bat
* Make a new multi platform blank application (Name it Project1)
* Enable SKIA on Project1
* Building > Delphi compiler > Compiling
    Android 64 bit > Release
      Generate Android 32-bit and 64 bit binaries (armeabi-v7a + arm64-v8a): TRUE
    MacOS ARM > release
      Generate macos universal binary file (x86_64 + arm64): TRUE
* Save Project1  
* Build each plateform in debug (ctrl+f9)
* Build each plateform in release (ctrl+f9)
* Deploy each plateform in debug (ctrl+alt+shif+f9)
* Deploy each plateform in release (ctrl+alt+shif+f9)
* Save Project1 
* Study the diffs with:
    AndroidManifest.template.xml
    Entitlement.TemplateiOS.xml
    Entitlement.TemplateOSX.xml
    info.plist.TemplateiOS.xml
    info.plist.TemplateOSX.xml
    Android\Release\AndroidManifest.xml
    Android64\Release\AndroidManifest.xml
    OSX64\Release\Project1.info.plist
    OSX64\Release\Project1.entitlements
    OSXARM64\Release\Project1.info.plist
    OSXARM64\Release\Project1.entitlements
    iOSDevice64\Release\Project1.info.plist
    iOSDevice64\Release\Project1.entitlements
    iOSSimARM64\Release\Project1.info.plist
    iOSSimARM64\Release\Project1.entitlements
  and report any found diffs in your project(s)
* run Project1.deployproj.normalize.bat and make a diff compare with 
  the new generated deployproj vs deployproj.bak
  NOTE: If you see some missing nodes, try to deploy again 
  in configuration of the missing nodes and compare again.
  You can ignore the node:
    * This node appear when deploying for the Application store only
      <DeployFile Condition="'$(Config)'=='Release'" Include="Android\Release\libProject1.so">
        <DeployClass>ProjectOutput_Android32</DeployClass>
        <LocalCommand/>
        <Operation>1</Operation>
        <Overwrite>True</Overwrite>
        <RemoteCommand/>
        <RemoteDir>Project1\library\lib\armeabi-v7a\</RemoteDir>
        <RemoteName>libProject1.so</RemoteName>
      </DeployFile>    
    * <ItemGroup Condition="&#39;$(Platform)&#39;==&#39;Linux64&#39;"/>
    * <ItemGroup Condition="&#39;$(Platform)&#39;==&#39;Win64x&#39;"/>
    * <PropertyGroup>
        <DeviceId Condition="&#39;$(Platform)&#39;==&#39;Android&#39;">2B011FDH200DHE</DeviceId>
        <DeviceId Condition="&#39;$(Platform)&#39;==&#39;Android64&#39;">2B011FDH200DHE</DeviceId>
        <DeviceId Condition="&#39;$(Platform)&#39;==&#39;iOSDevice64&#39;"/>
        <DeviceId Condition="&#39;$(Platform)&#39;==&#39;iOSSimARM64&#39;">BFD2217C-51A5-4855-BA03-B430731C177E</DeviceId>
      </PropertyGroup>
    * This node appear when deploying for the Application store only
      <PropertyGroup>
        <OSX64_OutputDir Condition="'$(Platform)'=='OSXARM64'">OSX64\Release\</OSX64_OutputDir>
      </PropertyGroup>
* rename Project1.deployproj.bak in Project1.deployproj.bak.tmp
* Run Project1.dproj.normalize.bat
* Study the diffs between Project1.dproj and the previous commited version
* Study the diffs between Project1.dproj.bak and the previous commited version
* Run Project1.deployproj.normalize.bat
* Study the diffs between Project1.deployproj and the previous commited version
* Study the diffs between Project1.deployproj.bak and the previous commited version
* rename Project1.deployproj.bak.tmp in Project1.deployproj.bak
* Run clean.bat
* Commit all