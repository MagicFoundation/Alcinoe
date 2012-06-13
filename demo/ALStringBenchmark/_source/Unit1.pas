unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, AlScrollBar, ALMemo, ALButton,
  OleCtrls, SHDocVw, ComObj;

type
  TForm1 = class(TForm)
    ALButton1: TALButton;
    ALButton2: TALButton;
    ALButton3: TALButton;
    ALButton4: TALButton;
    ALButton5: TALButton;
    ALButton6: TALButton;
    ALButton7: TALButton;
    ALButton8: TALButton;
    ALButton9: TALButton;
    ALButton10: TALButton;
    ALButton11: TALButton;
    ALButton12: TALButton;
    ALButton13: TALButton;
    ALButton14: TALButton;
    Panel2: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Panel3: TPanel;
    PanelWebBrowser: TPanel;
    procedure ALButtonPaint(Sender: TObject; var continue: Boolean);
    procedure FormClick(Sender: TObject);
    procedure ALButton1Click(Sender: TObject);
    procedure ALButton2Click(Sender: TObject);
    procedure ALButton3Click(Sender: TObject);
    procedure ALButton4Click(Sender: TObject);
    procedure ALButton5Click(Sender: TObject);
    procedure ALButton6Click(Sender: TObject);
    procedure ALButton7Click(Sender: TObject);
    procedure ALButton8Click(Sender: TObject);
    procedure ALButton9Click(Sender: TObject);
    procedure ALButton10Click(Sender: TObject);
    procedure ALButton11Click(Sender: TObject);
    procedure ALButton12Click(Sender: TObject);
    procedure ALButton13Click(Sender: TObject);
    procedure ALButton14Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var Form1: TForm1;

implementation

uses alFcnSkin,
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
Var S1: AnsiString;
     i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 100 do begin
    S1 := AlStringReplace(S1,AlRandomStr(1),AlRandomStr(8),[]);
    S1 := AlStringReplace(S1,AlRandomStr(3),AlRandomStr(3),[]);
    S1 := AlStringReplace(S1,AlRandomStr(4),AlRandomStr(6),[RfReplaceAll]);
    S1 := AlStringReplace(S1,AlRandomStr(3),AlRandomStr(12),[RfIgnoreCase]);
    S1 := AlStringReplace(S1,AlRandomStr(2),AlRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 100 do begin
    S1 := AlStringReplace(S1,AlRandomStr(1),AlRandomStr(8),[]);
    S1 := AlStringReplace(S1,AlRandomStr(15),AlRandomStr(3),[]);
    S1 := AlStringReplace(S1,AlRandomStr(35),AlRandomStr(6),[RfReplaceAll]);
    S1 := AlStringReplace(S1,AlRandomStr(12),AlRandomStr(12),[RfIgnoreCase]);
    S1 := AlStringReplace(S1,AlRandomStr(11),AlRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 100 do begin
    S1 := AlStringReplace(S1,AlRandomStr(1),AlRandomStr(8),[]);
    S1 := AlStringReplace(S1,AlRandomStr(3),AlRandomStr(3),[]);
    S1 := AlStringReplace(S1,AlRandomStr(4),AlRandomStr(6),[RfReplaceAll]);
    S1 := AlStringReplace(S1,AlRandomStr(3),AlRandomStr(12),[RfIgnoreCase]);
    S1 := AlStringReplace(S1,AlRandomStr(2),AlRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 100 do begin
    S1 := AlStringReplace(S1,AlRandomStr(1),AlRandomStr(8),[]);
    S1 := AlStringReplace(S1,AlRandomStr(3),AlRandomStr(3),[]);
    S1 := AlStringReplace(S1,AlRandomStr(4),AlRandomStr(6),[RfReplaceAll]);
    S1 := AlStringReplace(S1,AlRandomStr(3),AlRandomStr(12),[RfIgnoreCase]);
    S1 := AlStringReplace(S1,AlRandomStr(2),AlRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStr(100000);
  For i := 0 to 100 do begin
    S1 := AlStringReplace(S1,AlRandomStr(1),AlRandomStr(8),[]);
    S1 := AlStringReplace(S1,AlRandomStr(3),AlRandomStr(3),[]);
    S1 := AlStringReplace(S1,AlRandomStr(4),AlRandomStr(6),[RfReplaceAll]);
    S1 := AlStringReplace(S1,AlRandomStr(3),AlRandomStr(12),[RfIgnoreCase]);
    S1 := AlStringReplace(S1,AlRandomStr(2),AlRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{**********************************************}
procedure TForm1.ALButton2Click(Sender: TObject);
Var S1: AnsiString;
     i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,AlRandomStr(1),AlRandomStr(8),[]);
    S1 := StringReplace(S1,AlRandomStr(3),AlRandomStr(3),[]);
    S1 := StringReplace(S1,AlRandomStr(4),AlRandomStr(6),[RfReplaceAll]);
    S1 := StringReplace(S1,AlRandomStr(3),AlRandomStr(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,AlRandomStr(2),AlRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,AlRandomStr(1),AlRandomStr(8),[]);
    S1 := StringReplace(S1,AlRandomStr(15),AlRandomStr(3),[]);
    S1 := StringReplace(S1,AlRandomStr(35),AlRandomStr(6),[RfReplaceAll]);
    S1 := StringReplace(S1,AlRandomStr(12),AlRandomStr(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,AlRandomStr(11),AlRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,AlRandomStr(1),AlRandomStr(8),[]);
    S1 := StringReplace(S1,AlRandomStr(3),AlRandomStr(3),[]);
    S1 := StringReplace(S1,AlRandomStr(4),AlRandomStr(6),[RfReplaceAll]);
    S1 := StringReplace(S1,AlRandomStr(3),AlRandomStr(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,AlRandomStr(2),AlRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,AlRandomStr(1),AlRandomStr(8),[]);
    S1 := StringReplace(S1,AlRandomStr(3),AlRandomStr(3),[]);
    S1 := StringReplace(S1,AlRandomStr(4),AlRandomStr(6),[RfReplaceAll]);
    S1 := StringReplace(S1,AlRandomStr(3),AlRandomStr(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,AlRandomStr(2),AlRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStr(100000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,AlRandomStr(1),AlRandomStr(8),[]);
    S1 := StringReplace(S1,AlRandomStr(3),AlRandomStr(3),[]);
    S1 := StringReplace(S1,AlRandomStr(4),AlRandomStr(6),[RfReplaceAll]);
    S1 := StringReplace(S1,AlRandomStr(3),AlRandomStr(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,AlRandomStr(2),AlRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton3Click(Sender: TObject);
Var S1: AnsiString;
     i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 1000 do begin
    ALPosEx(AlRandomStr(1),S1,1+random(50));
    ALPosEx(AlRandomStr(3),S1,1+random(50));
    ALPosEx(AlRandomStr(8),S1,1+random(50));
    ALPosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 1000 do begin
    ALPosEx(AlRandomStr(1),S1,1+random(50));
    ALPosEx(AlRandomStr(3),S1,1+random(50));
    ALPosEx(AlRandomStr(8),S1,1+random(50));
    ALPosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 1000 do begin
    ALPosEx(AlRandomStr(1),S1,1+random(50));
    ALPosEx(AlRandomStr(3),S1,1+random(50));
    ALPosEx(AlRandomStr(8),S1,1+random(50));
    ALPosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 1000 do begin
    ALPosEx(AlRandomStr(1),S1,1+random(50));
    ALPosEx(AlRandomStr(3),S1,1+random(50));
    ALPosEx(AlRandomStr(8),S1,1+random(50));
    ALPosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(300000);
  For i := 0 to 1000 do begin
    ALPosEx(AlRandomStr(1),S1,1+random(50));
    ALPosEx(AlRandomStr(3),S1,1+random(50));
    ALPosEx(AlRandomStr(8),S1,1+random(50));
    ALPosEx(AlRandomStr(20),S1,1+random(50));
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton4Click(Sender: TObject);
Var S1: AnsiString;
     i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 1000 do begin
    PosEx(AlRandomStr(1),S1,1+random(50));
    PosEx(AlRandomStr(3),S1,1+random(50));
    PosEx(AlRandomStr(8),S1,1+random(50));
    PosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 1000 do begin
    PosEx(AlRandomStr(1),S1,1+random(50));
    PosEx(AlRandomStr(3),S1,1+random(50));
    PosEx(AlRandomStr(8),S1,1+random(50));
    PosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 1000 do begin
    PosEx(AlRandomStr(1),S1,1+random(50));
    PosEx(AlRandomStr(3),S1,1+random(50));
    PosEx(AlRandomStr(8),S1,1+random(50));
    PosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 1000 do begin
    PosEx(AlRandomStr(1),S1,1+random(50));
    PosEx(AlRandomStr(3),S1,1+random(50));
    PosEx(AlRandomStr(8),S1,1+random(50));
    PosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(300000);
  For i := 0 to 1000 do begin
    PosEx(AlRandomStr(1),S1,1+random(50));
    PosEx(AlRandomStr(3),S1,1+random(50));
    PosEx(AlRandomStr(8),S1,1+random(50));
    PosEx(AlRandomStr(20),S1,1+random(50));
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{**********************************************}
procedure TForm1.ALButton5Click(Sender: TObject);
Var S1: AnsiString;
     i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 1000 do begin
    ALPos(AlRandomStr(1),S1);
    ALPos(AlRandomStr(3),S1);
    ALPos(AlRandomStr(8),S1);
    ALPos(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 1000 do begin
    ALPos(AlRandomStr(1),S1);
    ALPos(AlRandomStr(3),S1);
    ALPos(AlRandomStr(8),S1);
    ALPos(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 1000 do begin
    ALPos(AlRandomStr(1),S1);
    ALPos(AlRandomStr(3),S1);
    ALPos(AlRandomStr(8),S1);
    ALPos(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 1000 do begin
    ALPos(AlRandomStr(1),S1);
    ALPos(AlRandomStr(3),S1);
    ALPos(AlRandomStr(8),S1);
    ALPos(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(300000);
  For i := 0 to 1000 do begin
    ALPos(AlRandomStr(1),S1);
    ALPos(AlRandomStr(3),S1);
    ALPos(AlRandomStr(8),S1);
    ALPos(AlRandomStr(20),S1);
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton6Click(Sender: TObject);
Var S1: AnsiString;
     i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 1000 do begin
    Pos(AlRandomStr(1),S1);
    Pos(AlRandomStr(3),S1);
    Pos(AlRandomStr(8),S1);
    Pos(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 1000 do begin
    Pos(AlRandomStr(1),S1);
    Pos(AlRandomStr(3),S1);
    Pos(AlRandomStr(8),S1);
    Pos(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 1000 do begin
    Pos(AlRandomStr(1),S1);
    Pos(AlRandomStr(3),S1);
    Pos(AlRandomStr(8),S1);
    Pos(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 1000 do begin
    Pos(AlRandomStr(1),S1);
    Pos(AlRandomStr(3),S1);
    Pos(AlRandomStr(8),S1);
    Pos(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(300000);
  For i := 0 to 1000 do begin
    Pos(AlRandomStr(1),S1);
    Pos(AlRandomStr(3),S1);
    Pos(AlRandomStr(8),S1);
    Pos(AlRandomStr(20),S1);
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton7Click(Sender: TObject);
Var S1: AnsiString;
     i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 10000 do begin
    AlCompareText(S1,AlRandomStr(25));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,S1);
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 10000 do begin
    AlCompareText(S1,AlRandomStr(25));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,S1);
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 10000 do begin
    AlCompareText(S1,AlRandomStr(25));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,S1);
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 10000 do begin
    AlCompareText(S1,AlRandomStr(25));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,S1);
  end;

  S1 := ALRandomStr(100000);
  For i := 0 to 10000 do begin
    AlCompareText(S1,AlRandomStr(25));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,S1);
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton8Click(Sender: TObject);
Var S1: AnsiString;
     i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 10000 do begin
    CompareText(S1,AlRandomStr(25));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,S1);
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 10000 do begin
    CompareText(S1,AlRandomStr(25));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,S1);
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 10000 do begin
    CompareText(S1,AlRandomStr(25));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,S1);
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 10000 do begin
    CompareText(S1,AlRandomStr(25));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,S1);
  end;

  S1 := ALRandomStr(100000);
  For i := 0 to 10000 do begin
    CompareText(S1,AlRandomStr(25));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,AlRandomStr(50));
    CompareText(S1,S1);
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton9Click(Sender: TObject);
Var i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  For i := 0 to 10000 do begin
    AlUpperCase(AlRandomStr(5));
    AlUpperCase(AlRandomStr(25));
    AlUpperCase(AlRandomStr(300));
    AlUpperCase(AlRandomStr(3000));
    AlUpperCase(AlRandomStr(10000));
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton10Click(Sender: TObject);
Var i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  For i := 0 to 10000 do begin
    UpperCase(AlRandomStr(5));
    UpperCase(AlRandomStr(25));
    UpperCase(AlRandomStr(300));
    UpperCase(AlRandomStr(3000));
    UpperCase(AlRandomStr(10000));
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton11Click(Sender: TObject);
Var i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  For i := 0 to 10000 do begin
    AlLowerCase(AlRandomStr(5));
    AlLowerCase(AlRandomStr(25));
    AlLowerCase(AlRandomStr(300));
    AlLowerCase(AlRandomStr(3000));
    AlLowerCase(AlRandomStr(10000));
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton12Click(Sender: TObject);
Var i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  For i := 0 to 10000 do begin
    LowerCase(AlRandomStr(5));
    LowerCase(AlRandomStr(25));
    LowerCase(AlRandomStr(300));
    LowerCase(AlRandomStr(3000));
    LowerCase(AlRandomStr(10000));
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton13Click(Sender: TObject);
Var S1: AnsiString;
     i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 10000 do begin
    AlCopyStr(S1,1+Random(25), 1+Random(10));
    AlCopyStr(S1,1+Random(50), 1+Random(4));
    AlCopyStr(S1,1+Random(50), 1+Random(30));
    AlCopyStr(S1,1+Random(40), 1+Random(1));
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 10000 do begin
    AlCopyStr(S1,1+Random(60), 1+Random(100));
    AlCopyStr(S1,1+Random(100), 1+Random(15));
    AlCopyStr(S1,1+Random(150), 1+Random(200));
    AlCopyStr(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 10000 do begin
    AlCopyStr(S1,1+Random(1000), 1+Random(1000));
    AlCopyStr(S1,1+Random(100), 1+Random(200));
    AlCopyStr(S1,1+Random(3000), 1+Random(3000));
    AlCopyStr(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 10000 do begin
    AlCopyStr(S1,1+Random(2000), 1+Random(1000));
    AlCopyStr(S1,1+Random(500), 1+Random(200));
    AlCopyStr(S1,1+Random(4000), 1+Random(4000));
    AlCopyStr(S1,1+Random(10), 1+Random(1));
  end;

  S1 := ALRandomStr(100000);
  For i := 0 to 10000 do begin
    AlCopyStr(S1,1+Random(80000), 1+Random(1000));
    AlCopyStr(S1,1+Random(500), 1+Random(200));
    AlCopyStr(S1,1+Random(100000), 1+Random(100000));
    AlCopyStr(S1,1+Random(10), 1+Random(2));
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton14Click(Sender: TObject);
Var S1: AnsiString;
     i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 10000 do begin
    Copy(S1,1+Random(25), 1+Random(10));
    Copy(S1,1+Random(50), 1+Random(4));
    Copy(S1,1+Random(50), 1+Random(30));
    Copy(S1,1+Random(40), 1+Random(1));
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 10000 do begin
    Copy(S1,1+Random(60), 1+Random(100));
    Copy(S1,1+Random(100), 1+Random(15));
    Copy(S1,1+Random(150), 1+Random(200));
    Copy(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 10000 do begin
    Copy(S1,1+Random(1000), 1+Random(1000));
    Copy(S1,1+Random(100), 1+Random(200));
    Copy(S1,1+Random(3000), 1+Random(3000));
    Copy(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 10000 do begin
    Copy(S1,1+Random(2000), 1+Random(1000));
    Copy(S1,1+Random(500), 1+Random(200));
    Copy(S1,1+Random(4000), 1+Random(4000));
    Copy(S1,1+Random(10), 1+Random(1));
  end;

  S1 := ALRandomStr(100000);
  For i := 0 to 10000 do begin
    Copy(S1,1+Random(80000), 1+Random(1000));
    Copy(S1,1+Random(500), 1+Random(200));
    Copy(S1,1+Random(100000), 1+Random(100000));
    Copy(S1,1+Random(10), 1+Random(2));
  end;
  Showmessage(FormatDateTime('nn:ss:zzz',now-StartDate));
end;




{-------------------}
var ie: IWebBrowser2;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
var Url, Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  ie := CreateOleObject('InternetExplorer.Application') as IWebBrowser2;
  SetWindowLong(ie.hwnd, GWL_STYLE, GetWindowLong(ie.hwnd, GWL_STYLE) and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME );
  SetWindowPos(ie.hwnd, HWND_TOP, Left, Top, Width, Height, SWP_FRAMECHANGED);
  windows.setparent(ie.hwnd, PanelWebBrowser.handle);
  ie.Left := maxint; // don't understand why it's look impossible to setup the position
  ie.Top  := maxint; // don't understand why it's look impossible to setup the position
  ie.Width := 100;
  ie.Height := 300;
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  Url := 'http://www.arkadia.com/html/alcinoe_like.html';
  ie.Navigate2(Url,Flags,TargetFrameName,PostData,Headers);
  ie.Visible := true;
end;

{********************************************************************}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    ie.quit;
  except
  end;
  sleep(500);
end;

{$IFDEF DEBUG}
initialization
  ReportMemoryleaksOnSHutdown := True;
{$ENDIF}

end.
