//////////////////////////////////////
// to create a new release.keystore //
//////////////////////////////////////

"c:\Program Files\Eclipse Adoptium\jdk-11.0.16.101-hotspot\bin\keytool.exe" -genkey -v ^
-keystore "C:\Dev\WinSuite\WinDating\kiskis\_build\source\android\keystore\release.keystore" ^
-storepass <KEYSTORE_PASSWORD> ^
-alias "<ALIAS_NAME>" ^
-keypass <ALIAS_PASSWORD> ^
-dname "cn=<ORGANIZATION_NAME>" ^
-keyalg RSA ^
-sigalg SHA512withRSA ^
-keysize 2048 ^
-validity 900000

!! do not forget to update the file PWD with the choosen <ALIAS_NAME>/<KEYSTORE_PASSWORD> and <ALIAS_PASSWORD> !!



//////////////////////////////////////////////////////////
// To list which certificates are in a release.keystore //
//////////////////////////////////////////////////////////

"c:\Program Files\Eclipse Adoptium\jdk-11.0.16.101-hotspot\bin\keytool.exe" -list -v ^
-keystore "C:\Dev\WinSuite\WinDating\kiskis\_build\source\android\keystore\release.keystore"



///////////////////////////////////////
// To get a certificate fingerprint: //
///////////////////////////////////////

"c:\Program Files\Eclipse Adoptium\jdk-11.0.16.101-hotspot\bin\keytool.exe" -exportcert -list -v ^
-alias <ALIAS_NAME> -keystore "C:\Dev\WinSuite\WinDating\kiskis\_build\source\android\keystore\release.keystore"
