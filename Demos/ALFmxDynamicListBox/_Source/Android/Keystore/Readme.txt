//////////////////////////////////////
// to create a new release.keystore //
//////////////////////////////////////

keytool.exe -genkey -v ^
-keystore "release.keystore" ^
-storepass <KEYSTORE_PASSWORD> ^
-alias "<ALIAS_NAME>" ^
-dname "cn=<ORGANIZATION_NAME>" ^
-keyalg RSA ^
-sigalg SHA512withRSA ^
-keysize 2048 ^
-validity 900000

!! do not forget to update the file PWD with the choosen <ALIAS_NAME>/<KEYSTORE_PASSWORD> !!



//////////////////////////////////////////////////////////
// To list which certificates are in a release.keystore //
//////////////////////////////////////////////////////////

keytool.exe -list -v -keystore "release.keystore"
