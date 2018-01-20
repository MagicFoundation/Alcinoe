SmartMobileStudio Client to mORMot
==================================

Please ensure you first copied the latest version of those files
from CrossPlatform folder into the SMS shared library folder, i.e.
c:\ProgramData\Optimale Systemer AS\Smart Mobile Studio\Libraries\

- SynCrossPlatformCrypto.pas
- SynCrossPlatformRest.pas
- SynCrossPlatformSpecific.pas

For this, simply run CopySynCrossPlatformUnits.bat

Those units are needed as external content, to compile the 
demo application.

As an alternative, you may copy those units to the projects root
folder, but you forget to synchronize the units from the official
source code repository later.

### Server Needed

In order to let this client sample application run as exepected,
you need to compile and run the RegressionTestsServer.dpr program,
as available in "27 - CrossPlatform Clients" folder.

This server will publish TSQLRecordPeople remote ORM access,
with TSQLRestServerAuthenticationDefault authentication.



