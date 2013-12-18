//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\source\UIBTransactionEdit.pas", uibtransactionedit, UIBTransactionEditForm);
USEFORMNS("..\source\UIBDatabaseEdit.pas", uibdatabaseedit, UIBDatabaseEditForm);
USEFORMNS("..\source\UIBSQLEdit.pas", uibsqledit, UIBSQLEditForm);
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



 