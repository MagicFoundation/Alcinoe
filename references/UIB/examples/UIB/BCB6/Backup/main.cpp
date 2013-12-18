//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "UIB"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::GoClick(TObject *Sender)
{
  Memo->Clear();
  Backup->Run();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::BackupVerbose(TObject *Sender,
      AnsiString Message)
{
  Memo->Lines->Add(Message);        
}
//---------------------------------------------------------------------------
