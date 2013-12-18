msxsl TestLib.xml interface.xsl -v -o Testlib_Intf.pas
msxsl TestLib.xml server.xsl -v -o TestLib_Server.pas
msxsl TestLib.xml client.xsl -v -o TestLib_Client.pas
rem pause