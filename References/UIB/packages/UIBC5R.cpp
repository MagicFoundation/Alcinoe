//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("UIBC5R.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\source\UIB.pas");
USEUNIT("..\source\UIBase.pas");
USEUNIT("..\source\UIBConst.pas");
USEUNIT("..\source\UIBDataSet.pas");
USEUNIT("..\source\UIBError.pas");
USEUNIT("..\source\UIBLib.pas");
USEUNIT("..\source\UIBMetaData.pas");
USEUNIT("..\source\UIBSQLParser.pas");
USEPACKAGE("Vcldb50.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
