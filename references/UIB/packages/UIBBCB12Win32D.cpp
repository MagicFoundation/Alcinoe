//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\source\uibtransactionedit.pas", Uibtransactionedit, UIBTransactionEditForm);
USEFORMNS("..\source\uibdatabaseedit.pas", Uibdatabaseedit, UIBDatabaseEditForm);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

// Source du paquet.
//---------------------------------------------------------------------------


#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
	return 1;
}
//---------------------------------------------------------------------------
