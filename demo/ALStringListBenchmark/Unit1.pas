unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, AlScrollBar, ALMemo, ALButton;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label5: TLabel;
    ALButton1: TALButton;
    ALButton2: TALButton;
    ALButton3: TALButton;
    ALButton4: TALButton;
    procedure ALButtonPaint(Sender: TObject; var continue: Boolean);
    procedure FormClick(Sender: TObject);
    procedure ALButton1Click(Sender: TObject);
    procedure ALButton2Click(Sender: TObject);
    procedure ALButton3Click(Sender: TObject);
    procedure ALButton4Click(Sender: TObject);
  private
  public
  end;

var Form1: TForm1;

implementation

uses alFcnSkin,
     alStringList,
     alFcnString;

{$R *.dfm}

{*********************************************************************}
procedure TForm1.ALButtonPaint(Sender: TObject; var continue: Boolean);
begin
  paintAlButtonBlueSkin(sender, Continue);
end;

{******************************************}
procedure TForm1.FormClick(Sender: TObject);
begin
  Windows.SetFocus(0);
end;

{***********************************************}
procedure TForm1.ALButton1Click(Sender: TObject);
Var Lst1: TALStringList;
    StartDate: DWORD;
    i: integer;
begin
  StartDate := GetTickCount;
  Lst1 := TALStringList.create;
  try
    Lst1.Sorted := True;
    Lst1.Duplicates := DupIgnore;

    for I := 1 to 10000 do
      Lst1.Values[AlRandomStr(5)] := AlRandomStr(5);

  finally
    Lst1.free;
  end;
  Showmessage('Process 10.000 Values[x]:=y in ' + inttostr(GetTickCount - StartDate) + ' ms');
end;

{**********************************************}
procedure TForm1.ALButton2Click(Sender: TObject);
Var Lst1: TStringList;
    StartDate: DWORD;
    i: integer;
begin
  Showmessage('You can go to take a coffee during the process!');
  StartDate := GetTickCount;
  Lst1 := TStringList.create;
  try
    Lst1.Sorted := False;

    for I := 1 to 10000 do
      Lst1.Values[AlRandomStr(5)] := AlRandomStr(5);

  finally
    Lst1.free;
  end;
  Showmessage('Process 10.000 Values[x]:=y in ' + inttostr(GetTickCount - StartDate) + ' ms');
end;


{***********************************************}
procedure TForm1.ALButton3Click(Sender: TObject);
Var Lst1: TALAVLStringList;
    StartDate: DWORD;
    i: integer;
begin
  StartDate := GetTickCount;
  Lst1 := TALAVLStringList.create;
  try

    for I := 1 to 100000 do
      Lst1.Values[AlRandomStr(5)] := AlRandomStr(5);

  finally
    Lst1.free;
  end;
  Showmessage('Process 100.000 Values[x]:=y in ' + inttostr(GetTickCount - StartDate) + ' ms');
end;


{***********************************************}
procedure TForm1.ALButton4Click(Sender: TObject);
Var Lst1: TStringList;
    StartDate: DWORD;
    i: integer;
begin
  Showmessage('You can go to in holidays during the process!');
  StartDate := GetTickCount;
  Lst1 := TStringList.create;
  try
    Lst1.Sorted := False;

    for I := 1 to 100000 do
      Lst1.Values[AlRandomStr(5)] := AlRandomStr(5);

  finally
    Lst1.free;
  end;
  Showmessage('Process 100.000 Values[x]:=y in ' + inttostr(GetTickCount - StartDate) + ' ms');
end;

end.
