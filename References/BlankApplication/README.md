Check template files
--------------------

* Delete everything in this directory except 
    clean.bat 
    README.md
    Project1.dproj.normalize.bat
    Project1.deployproj.normalize.bat
* Make a new multi platform blank application (Name it Project1)
* Build each plateform in release
* Deploy each platforms in release (ctrl+alt+shif+f9)
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
* Commit only the previous template files
  
  
Check DProjNormalizer
---------------------

* Delete everything in this directory except 
    clean.bat 
    README.md
    Project1.dproj.normalize.bat
    Project1.deployproj.normalize.bat
* Make a new multi platform blank application (Name it Project1)
* Build each plateform in debug (ctrl+f9)
* Build each plateform in release (ctrl+f9)
* Deploy each plateform in debug (ctrl+alt+shif+f9)
* Deploy each plateform in release (ctrl+alt+shif+f9)
* Run clean.bat
* Run Project1.dproj.normalize.bat
* Study the diffs with previous commited version
* Commit


Check DeployProjNormalizer
--------------------------

* Delete everything in this directory except 
    clean.bat 
    README.md
    Project1.dproj.normalize.bat
    Project1.deployproj.normalize.bat
* Make a new multi platform blank application (Name it Project1)
* Save Project1  
* Enable SKIA on Project1
* Save Project1  
* Build each plateform in debug (ctrl+f9)
* Build each plateform in release (ctrl+f9)
* Deploy each plateform in debug (ctrl+alt+shif+f9)
* Deploy each plateform in release (ctrl+alt+shif+f9)
* run Project1.deployproj.normalize.bat and make a diff compare with 
  the new generated deployproj vs deployproj.bak. 
  NOTE: If you see some missing nodes, try to deploy again 
  in configuration of the missing nodes and compare again.
  You can ignore the node:
    <ItemGroup Condition="&#39;$(Platform)&#39;==&#39;Linux64&#39;"/>
    <PropertyGroup>
      <DeviceId Condition="&#39;$(Platform)&#39;==&#39;Android&#39;">2B011FDH200DHE</DeviceId>
      <DeviceId Condition="&#39;$(Platform)&#39;==&#39;Android64&#39;">2B011FDH200DHE</DeviceId>
      <DeviceId Condition="&#39;$(Platform)&#39;==&#39;iOSDevice64&#39;"/>
      <DeviceId Condition="&#39;$(Platform)&#39;==&#39;iOSSimARM64&#39;">BFD2217C-51A5-4855-BA03-B430731C177E</DeviceId>
    </PropertyGroup>
* close Delphi
* save somewhere Project1.deployproj
* Run Project1.dproj.normalize.bat
* Build each plateform in debug (ctrl+f9)
* Build each plateform in release (ctrl+f9)
* Deploy each plateform in debug (ctrl+alt+shif+f9)
* Deploy each plateform in release (ctrl+alt+shif+f9)
* run Project1.deployproj.normalize.bat and make a diff compare with 
  the new generated deployproj vs deployproj.bak.
* if you do not detect any difference then it's ok else you need
  to update the source code of DeployProjNormalizer