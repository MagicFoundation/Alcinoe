﻿unit Unit1;

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
    ALButton15: TALButton;
    ALButton16: TALButton;
    ALButton17: TALButton;
    ALButton18: TALButton;
    ALButton19: TALButton;
    ALButton20: TALButton;
    ALButton21: TALButton;
    ALButton22: TALButton;
    ALButton23: TALButton;
    ALButton24: TALButton;
    ALButton25: TALButton;
    ALButton26: TALButton;
    ALButton27: TALButton;
    ALButton28: TALButton;
    ALButton29: TALButton;
    ALButton30: TALButton;
    ALButton31: TALButton;
    ALButton32: TALButton;
    ALButton33: TALButton;
    ALButton34: TALButton;
    ALButton35: TALButton;
    ALButton36: TALButton;
    ALButton37: TALButton;
    ALButton38: TALButton;
    ALButton39: TALButton;
    ALButton41: TALButton;
    ALButton42: TALButton;
    ALButton40: TALButton;
    ALButton43: TALButton;
    ALButton44: TALButton;
    ALButton45: TALButton;
    ALButton46: TALButton;
    ALButton47: TALButton;
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
    procedure ALButton15Click(Sender: TObject);
    procedure ALButton17Click(Sender: TObject);
    procedure ALButton18Click(Sender: TObject);
    procedure ALButton19Click(Sender: TObject);
    procedure ALButton20Click(Sender: TObject);
    procedure ALButton21Click(Sender: TObject);
    procedure ALButton22Click(Sender: TObject);
    procedure ALButton23Click(Sender: TObject);
    procedure ALButton24Click(Sender: TObject);
    procedure ALButton25Click(Sender: TObject);
    procedure ALButton26Click(Sender: TObject);
    procedure ALButton27Click(Sender: TObject);
    procedure ALButton28Click(Sender: TObject);
    procedure ALButton29Click(Sender: TObject);
    procedure ALButton30Click(Sender: TObject);
    procedure ALButton31Click(Sender: TObject);
    procedure ALButton39Click(Sender: TObject);
    procedure ALButton42Click(Sender: TObject);
    procedure ALButton37Click(Sender: TObject);
    procedure ALButton38Click(Sender: TObject);
    procedure ALButton36Click(Sender: TObject);
    procedure ALButton16Click(Sender: TObject);
    procedure ALButton32Click(Sender: TObject);
    procedure ALButton41Click(Sender: TObject);
    procedure ALButton33Click(Sender: TObject);
    procedure ALButton34Click(Sender: TObject);
    procedure ALButton35Click(Sender: TObject);
    procedure ALButton40Click(Sender: TObject);
    procedure ALButton44Click(Sender: TObject);
    procedure ALButton43Click(Sender: TObject);
    procedure ALButton45Click(Sender: TObject);
    procedure ALButton47Click(Sender: TObject);
    procedure ALButton46Click(Sender: TObject);
  private
  public
  end;

var Form1: TForm1;

implementation

{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

uses {$IFDEF UNICODE}
     ansiStrings,
     {$ENDIF}
     alFcnSkin,
     alFcnString;

{$R *.dfm}

procedure TForm1.ALButtonPaint(Sender: TObject; var continue: Boolean);
begin
  paintAlButtonBlueSkin(sender, Continue);
end;

procedure TForm1.FormClick(Sender: TObject);
begin
  Windows.SetFocus(0);
end;

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
  Showmessage('2,500 AlStringReplace in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton2Click(Sender: TObject);
Var S1: String;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrU(50);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrU(1),ALRandomStrU(8),[]);
    S1 := StringReplace(S1,ALRandomStrU(3),ALRandomStrU(3),[]);
    S1 := StringReplace(S1,ALRandomStrU(4),ALRandomStrU(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrU(3),ALRandomStrU(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrU(2),ALRandomStrU(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrU(200);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrU(1),ALRandomStrU(8),[]);
    S1 := StringReplace(S1,ALRandomStrU(15),ALRandomStrU(3),[]);
    S1 := StringReplace(S1,ALRandomStrU(35),ALRandomStrU(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrU(12),ALRandomStrU(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrU(11),ALRandomStrU(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrU(3000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrU(1),ALRandomStrU(8),[]);
    S1 := StringReplace(S1,ALRandomStrU(3),ALRandomStrU(3),[]);
    S1 := StringReplace(S1,ALRandomStrU(4),ALRandomStrU(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrU(3),ALRandomStrU(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrU(2),ALRandomStrU(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrU(4000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrU(1),ALRandomStrU(8),[]);
    S1 := StringReplace(S1,ALRandomStrU(3),ALRandomStrU(3),[]);
    S1 := StringReplace(S1,ALRandomStrU(4),ALRandomStrU(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrU(3),ALRandomStrU(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrU(2),ALRandomStrU(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrU(100000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrU(1),ALRandomStrU(8),[]);
    S1 := StringReplace(S1,ALRandomStrU(3),ALRandomStrU(3),[]);
    S1 := StringReplace(S1,ALRandomStrU(4),ALRandomStrU(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrU(3),ALRandomStrU(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrU(2),ALRandomStrU(20),[RfReplaceAll, RfIgnoreCase]);
  end;
  Showmessage('2,500 StringReplace in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton29Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStr(1),ALRandomStr(8),[]);
    S1 := StringReplace(S1,ALRandomStr(3),ALRandomStr(3),[]);
    S1 := StringReplace(S1,ALRandomStr(4),ALRandomStr(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStr(3),ALRandomStr(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStr(2),ALRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStr(1),ALRandomStr(8),[]);
    S1 := StringReplace(S1,ALRandomStr(15),ALRandomStr(3),[]);
    S1 := StringReplace(S1,ALRandomStr(35),ALRandomStr(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStr(12),ALRandomStr(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStr(11),ALRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStr(1),ALRandomStr(8),[]);
    S1 := StringReplace(S1,ALRandomStr(3),ALRandomStr(3),[]);
    S1 := StringReplace(S1,ALRandomStr(4),ALRandomStr(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStr(3),ALRandomStr(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStr(2),ALRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStr(1),ALRandomStr(8),[]);
    S1 := StringReplace(S1,ALRandomStr(3),ALRandomStr(3),[]);
    S1 := StringReplace(S1,ALRandomStr(4),ALRandomStr(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStr(3),ALRandomStr(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStr(2),ALRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStr(100000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStr(1),ALRandomStr(8),[]);
    S1 := StringReplace(S1,ALRandomStr(3),ALRandomStr(3),[]);
    S1 := StringReplace(S1,ALRandomStr(4),ALRandomStr(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStr(3),ALRandomStr(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStr(2),ALRandomStr(20),[RfReplaceAll, RfIgnoreCase]);
  end;
  Showmessage('2,500 StringReplace in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton3Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 2000 do begin
    ALPosEx(AlRandomStr(1),S1,1+random(50));
    ALPosEx(AlRandomStr(3),S1,1+random(50));
    ALPosEx(AlRandomStr(8),S1,1+random(50));
    ALPosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 2000 do begin
    ALPosEx(AlRandomStr(1),S1,1+random(50));
    ALPosEx(AlRandomStr(3),S1,1+random(50));
    ALPosEx(AlRandomStr(8),S1,1+random(50));
    ALPosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 2000 do begin
    ALPosEx(AlRandomStr(1),S1,1+random(50));
    ALPosEx(AlRandomStr(3),S1,1+random(50));
    ALPosEx(AlRandomStr(8),S1,1+random(50));
    ALPosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 2000 do begin
    ALPosEx(AlRandomStr(1),S1,1+random(50));
    ALPosEx(AlRandomStr(3),S1,1+random(50));
    ALPosEx(AlRandomStr(8),S1,1+random(50));
    ALPosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(300000);
  For i := 0 to 2000 do begin
    ALPosEx(AlRandomStr(1),S1,1+random(50));
    ALPosEx(AlRandomStr(3),S1,1+random(50));
    ALPosEx(AlRandomStr(8),S1,1+random(50));
    ALPosEx(AlRandomStr(20),S1,1+random(50));
  end;
  Showmessage('40,000 ALPosEx in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton4Click(Sender: TObject);
Var S1: String;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := AlRandomStrU(50);
  For i := 0 to 2000 do begin
    PosEx(AlRandomStrU(1),S1,1+random(50));
    PosEx(AlRandomStrU(3),S1,1+random(50));
    PosEx(AlRandomStrU(8),S1,1+random(50));
    PosEx(AlRandomStrU(20),S1,1+random(50));
  end;

  S1 := AlRandomStrU(200);
  For i := 0 to 2000 do begin
    PosEx(AlRandomStrU(1),S1,1+random(50));
    PosEx(AlRandomStrU(3),S1,1+random(50));
    PosEx(AlRandomStrU(8),S1,1+random(50));
    PosEx(AlRandomStrU(20),S1,1+random(50));
  end;

  S1 := AlRandomStrU(3000);
  For i := 0 to 2000 do begin
    PosEx(AlRandomStrU(1),S1,1+random(50));
    PosEx(AlRandomStrU(3),S1,1+random(50));
    PosEx(AlRandomStrU(8),S1,1+random(50));
    PosEx(AlRandomStrU(20),S1,1+random(50));
  end;

  S1 := AlRandomStrU(4000);
  For i := 0 to 2000 do begin
    PosEx(AlRandomStrU(1),S1,1+random(50));
    PosEx(AlRandomStrU(3),S1,1+random(50));
    PosEx(AlRandomStrU(8),S1,1+random(50));
    PosEx(AlRandomStrU(20),S1,1+random(50));
  end;

  S1 := AlRandomStrU(300000);
  For i := 0 to 2000 do begin
    PosEx(AlRandomStrU(1),S1,1+random(50));
    PosEx(AlRandomStrU(3),S1,1+random(50));
    PosEx(AlRandomStrU(8),S1,1+random(50));
    PosEx(AlRandomStrU(20),S1,1+random(50));
  end;
  Showmessage('40,000 PosEx in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton30Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 2000 do begin
    PosEx(AlRandomStr(1),S1,1+random(50));
    PosEx(AlRandomStr(3),S1,1+random(50));
    PosEx(AlRandomStr(8),S1,1+random(50));
    PosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 2000 do begin
    PosEx(AlRandomStr(1),S1,1+random(50));
    PosEx(AlRandomStr(3),S1,1+random(50));
    PosEx(AlRandomStr(8),S1,1+random(50));
    PosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 2000 do begin
    PosEx(AlRandomStr(1),S1,1+random(50));
    PosEx(AlRandomStr(3),S1,1+random(50));
    PosEx(AlRandomStr(8),S1,1+random(50));
    PosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 2000 do begin
    PosEx(AlRandomStr(1),S1,1+random(50));
    PosEx(AlRandomStr(3),S1,1+random(50));
    PosEx(AlRandomStr(8),S1,1+random(50));
    PosEx(AlRandomStr(20),S1,1+random(50));
  end;

  S1 := ALRandomStr(300000);
  For i := 0 to 2000 do begin
    PosEx(AlRandomStr(1),S1,1+random(50));
    PosEx(AlRandomStr(3),S1,1+random(50));
    PosEx(AlRandomStr(8),S1,1+random(50));
    PosEx(AlRandomStr(20),S1,1+random(50));
  end;
  Showmessage('40,000 PosEx in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton5Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 2000 do begin
    ALPos(AlRandomStr(1),S1);
    ALPos(AlRandomStr(3),S1);
    ALPos(AlRandomStr(8),S1);
    ALPos(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 2000 do begin
    ALPos(AlRandomStr(1),S1);
    ALPos(AlRandomStr(3),S1);
    ALPos(AlRandomStr(8),S1);
    ALPos(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 2000 do begin
    ALPos(AlRandomStr(1),S1);
    ALPos(AlRandomStr(3),S1);
    ALPos(AlRandomStr(8),S1);
    ALPos(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 2000 do begin
    ALPos(AlRandomStr(1),S1);
    ALPos(AlRandomStr(3),S1);
    ALPos(AlRandomStr(8),S1);
    ALPos(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(300000);
  For i := 0 to 2000 do begin
    ALPos(AlRandomStr(1),S1);
    ALPos(AlRandomStr(3),S1);
    ALPos(AlRandomStr(8),S1);
    ALPos(AlRandomStr(20),S1);
  end;
  Showmessage('40,000 ALPos in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton6Click(Sender: TObject);
Var S1: String;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := AlRandomStrU(50);
  For i := 0 to 2000 do begin
    Pos(AlRandomStrU(1),S1);
    Pos(AlRandomStrU(3),S1);
    Pos(AlRandomStrU(8),S1);
    Pos(AlRandomStrU(20),S1);
  end;

  S1 := AlRandomStrU(200);
  For i := 0 to 2000 do begin
    Pos(AlRandomStrU(1),S1);
    Pos(AlRandomStrU(3),S1);
    Pos(AlRandomStrU(8),S1);
    Pos(AlRandomStrU(20),S1);
  end;

  S1 := AlRandomStrU(3000);
  For i := 0 to 2000 do begin
    Pos(AlRandomStrU(1),S1);
    Pos(AlRandomStrU(3),S1);
    Pos(AlRandomStrU(8),S1);
    Pos(AlRandomStrU(20),S1);
  end;

  S1 := AlRandomStrU(4000);
  For i := 0 to 2000 do begin
    Pos(AlRandomStrU(1),S1);
    Pos(AlRandomStrU(3),S1);
    Pos(AlRandomStrU(8),S1);
    Pos(AlRandomStrU(20),S1);
  end;

  S1 := AlRandomStrU(300000);
  For i := 0 to 2000 do begin
    Pos(AlRandomStrU(1),S1);
    Pos(AlRandomStrU(3),S1);
    Pos(AlRandomStrU(8),S1);
    Pos(AlRandomStrU(20),S1);
  end;
  Showmessage('40,000 Pos in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton31Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 2000 do begin
    POS(AlRandomStr(1),S1);
    POS(AlRandomStr(3),S1);
    POS(AlRandomStr(8),S1);
    POS(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 2000 do begin
    POS(AlRandomStr(1),S1);
    POS(AlRandomStr(3),S1);
    POS(AlRandomStr(8),S1);
    POS(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 2000 do begin
    POS(AlRandomStr(1),S1);
    POS(AlRandomStr(3),S1);
    POS(AlRandomStr(8),S1);
    POS(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 2000 do begin
    POS(AlRandomStr(1),S1);
    POS(AlRandomStr(3),S1);
    POS(AlRandomStr(8),S1);
    POS(AlRandomStr(20),S1);
  end;

  S1 := ALRandomStr(300000);
  For i := 0 to 2000 do begin
    POS(AlRandomStr(1),S1);
    POS(AlRandomStr(3),S1);
    POS(AlRandomStr(8),S1);
    POS(AlRandomStr(20),S1);
  end;
  Showmessage('40,000 Pos in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton21Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TALformatSettings;
    S1: AnsiString;
begin
  ALGetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  For i := 0 to 1000000 do begin
    S1 := ALDatetostr(Random(10000), aFormatSettings);
  end;
  Showmessage('1,000,000 ALDatetostr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton22Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TformatSettings;
    S1: String;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  For i := 0 to 1000000 do begin
    S1 := Datetostr(Random(10000), aFormatSettings);
  end;
  Showmessage('1,000,000 Datetostr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton39Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TformatSettings;
    S1: AnsiString;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  For i := 0 to 1000000 do begin
    S1 := AnsiString(Datetostr(Random(10000), aFormatSettings));
  end;
  Showmessage('1,000,000 Datetostr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton27Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TALformatSettings;
    S1: AnsiString;
    D1: TdateTime;
begin
  ALGetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  S1 := '12/10/2012';
  For i := 0 to 1000000 do begin
    ALTrystrToDateTime(S1, D1, aFormatSettings);
  end;
  Showmessage('1,000,000 ALTrystrToDateTime in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton28Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TformatSettings;
    S1: String;
    D1: TdateTime;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  S1 := '12/10/2012';
  For i := 0 to 1000000 do begin
    TrystrToDateTime(S1, D1, aFormatSettings);
  end;
  Showmessage('1,000,000 TrystrToDateTime in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton42Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TformatSettings;
    S1: ansiString;
    D1: TdateTime;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  S1 := '12/10/2012';
  For i := 0 to 1000000 do begin
    TrystrToDateTime(String(S1), D1, aFormatSettings);
  end;
  Showmessage('1,000,000 TrystrToDateTime in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton17Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: AnsiString;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := ALRandomStr(5,['0','1','2','3','4','5','6','7','8','9']);
    ALStrToInt(S1);
  end;
  Showmessage('10,000,000 ALStrToInt in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton18Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: String;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := ALRandomStrU(5,['0','1','2','3','4','5','6','7','8','9']);
    StrToInt(String(S1));
  end;
  Showmessage('10,000,000 StrToInt in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton37Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: AnsiString;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := ALRandomStr(5,['0','1','2','3','4','5','6','7','8','9']);
    StrToInt(String(S1));
  end;
  Showmessage('10,000,000 StrToInt in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton19Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: AnsiString;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := ALRandomStr(10,['0','1','2','3','4','5','6','7','8','9']);
    ALStrToInt64(S1);
  end;
  Showmessage('10,000,000 ALStrToInt64 in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton20Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: String;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := ALRandomStrU(10,['0','1','2','3','4','5','6','7','8','9']);
    StrToInt64(S1);
  end;
  Showmessage('10,000,000 StrToInt64 in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton38Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: ansiString;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := ALRandomStr(10,['0','1','2','3','4','5','6','7','8','9']);
    StrToInt64(String(S1));
  end;
  Showmessage('10,000,000 StrToInt64 in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton15Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: AnsiString;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := ALInttostr(Random(maxint));
  end;
  Showmessage('10,000,000 ALInttostr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton16Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: String;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := Inttostr(Random(maxint));
  end;
  Showmessage('10,000,000 Inttostr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton36Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: AnsiString;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := AnsiString(Inttostr(Random(maxint)));
  end;
  Showmessage('10,000,000 Inttostr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton7Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(50);
  For i := 0 to 1000 do begin
    AlCompareText(S1,AlRandomStr(25));
    AlCompareText(S1,AlRandomStr(50));
    AlCompareText(S1,S1);
  end;

  S1 := ALRandomStr(200);
  For i := 0 to 1000 do begin
    AlCompareText(S1,AlRandomStr(25));
    AlCompareText(S1,AlRandomStr(200));
    AlCompareText(S1,S1);
  end;

  S1 := ALRandomStr(3000);
  For i := 0 to 1000 do begin
    AlCompareText(S1,AlRandomStr(25));
    AlCompareText(S1,AlRandomStr(3000));
    AlCompareText(S1,S1);
  end;

  S1 := ALRandomStr(4000);
  For i := 0 to 1000 do begin
    AlCompareText(S1,AlRandomStr(25));
    AlCompareText(S1,AlRandomStr(4000));
    AlCompareText(S1,S1);
  end;

  S1 := ALRandomStr(10000);
  For i := 0 to 1000 do begin
    AlCompareText(S1,AlRandomStr(25));
    AlCompareText(S1,AlRandomStr(100000));
    AlCompareText(S1,S1);
  end;
  Showmessage('15,000 AlCompareText in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton8Click(Sender: TObject);
Var S1: String;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALrandomStrU(50);
  For i := 0 to 1000 do begin
    CompareText(S1,ALrandomStrU(25));
    CompareText(S1,ALrandomStrU(50));
    CompareText(S1,S1);
  end;

  S1 := ALrandomStrU(200);
  For i := 0 to 1000 do begin
    CompareText(S1,ALrandomStrU(25));
    CompareText(S1,ALrandomStrU(200));
    CompareText(S1,S1);
  end;

  S1 := ALrandomStrU(3000);
  For i := 0 to 1000 do begin
    CompareText(S1,ALrandomStrU(25));
    CompareText(S1,ALrandomStrU(3000));
    CompareText(S1,S1);
  end;

  S1 := ALrandomStrU(4000);
  For i := 0 to 1000 do begin
    CompareText(S1,ALrandomStrU(25));
    CompareText(S1,ALrandomStrU(4000));
    CompareText(S1,S1);
  end;

  S1 := ALrandomStrU(10000);
  For i := 0 to 1000 do begin
    CompareText(S1,ALrandomStrU(25));
    CompareText(S1,ALrandomStrU(100000));
    CompareText(S1,S1);
  end;
  Showmessage('15,000 CompareText in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton32Click(Sender: TObject);
Var S1: ansiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALrandomStr(50);
  For i := 0 to 1000 do begin
    CompareText(S1,ALrandomStr(25));
    CompareText(S1,ALrandomStr(50));
    CompareText(S1,S1);
  end;

  S1 := ALrandomStr(200);
  For i := 0 to 1000 do begin
    CompareText(S1,ALrandomStr(25));
    CompareText(S1,ALrandomStr(200));
    CompareText(S1,S1);
  end;

  S1 := ALrandomStr(3000);
  For i := 0 to 1000 do begin
    CompareText(S1,ALrandomStr(25));
    CompareText(S1,ALrandomStr(3000));
    CompareText(S1,S1);
  end;

  S1 := ALrandomStr(4000);
  For i := 0 to 1000 do begin
    CompareText(S1,ALrandomStr(25));
    CompareText(S1,ALrandomStr(4000));
    CompareText(S1,S1);
  end;

  S1 := ALrandomStr(10000);
  For i := 0 to 1000 do begin
    CompareText(S1,ALrandomStr(25));
    CompareText(S1,ALrandomStr(100000));
    CompareText(S1,S1);
  end;
  Showmessage('15,000 CompareText in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton25Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TALformatSettings;
    S1: AnsiString;
begin
  ALGetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  For i := 0 to 5000000 do begin
    S1 := ALFloatToStr(random(MaxInt) / random(Maxint), aFormatSettings);
  end;
  Showmessage('5,000,000 ALFloatToStr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton26Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TformatSettings;
    S1: String;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  For i := 0 to 5000000 do begin
    S1 := FloatToStr(random(MaxInt) / random(Maxint), aFormatSettings);
  end;
  Showmessage('5,000,000 FloatToStr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton41Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TformatSettings;
    S1: ansiString;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  For i := 0 to 5000000 do begin
    S1 := ansiString(FloatToStr(random(MaxInt) / random(Maxint), aFormatSettings));
  end;
  Showmessage('5,000,000 FloatToStr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

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
  Showmessage('50,000 AlUpperCase in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton10Click(Sender: TObject);
Var i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  For i := 0 to 10000 do begin
    UpperCase(AlRandomStrU(5));
    UpperCase(AlRandomStrU(25));
    UpperCase(AlRandomStrU(300));
    UpperCase(AlRandomStrU(3000));
    UpperCase(AlRandomStrU(10000));
  end;
  Showmessage('50,000 UpperCase in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton33Click(Sender: TObject);
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
  Showmessage('50,000 UpperCase in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

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
  Showmessage('50,000 AlLowerCase in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton12Click(Sender: TObject);
Var i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  For i := 0 to 10000 do begin
    LowerCase(AlRandomStrU(5));
    LowerCase(AlRandomStrU(25));
    LowerCase(AlRandomStrU(300));
    LowerCase(AlRandomStrU(3000));
    LowerCase(AlRandomStrU(10000));
  end;
  Showmessage('50,000 LowerCase in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton34Click(Sender: TObject);
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
  Showmessage('50,000 LowerCase in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton13Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(500);
  For i := 0 to 100000 do begin
    AlCopyStr(S1,1+Random(25), 1+Random(10));
    AlCopyStr(S1,1+Random(50), 1+Random(4));
    AlCopyStr(S1,1+Random(50), 1+Random(30));
    AlCopyStr(S1,1+Random(40), 1+Random(1));
  end;

  S1 := ALRandomStr(2000);
  For i := 0 to 100000 do begin
    AlCopyStr(S1,1+Random(60), 1+Random(100));
    AlCopyStr(S1,1+Random(100), 1+Random(15));
    AlCopyStr(S1,1+Random(150), 1+Random(200));
    AlCopyStr(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStr(30000);
  For i := 0 to 100000 do begin
    AlCopyStr(S1,1+Random(1000), 1+Random(1000));
    AlCopyStr(S1,1+Random(100), 1+Random(200));
    AlCopyStr(S1,1+Random(3000), 1+Random(3000));
    AlCopyStr(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStr(40000);
  For i := 0 to 100000 do begin
    AlCopyStr(S1,1+Random(2000), 1+Random(1000));
    AlCopyStr(S1,1+Random(500), 1+Random(200));
    AlCopyStr(S1,1+Random(4000), 1+Random(4000));
    AlCopyStr(S1,1+Random(10), 1+Random(1));
  end;

  S1 := ALRandomStr(1000000);
  For i := 0 to 100000 do begin
    AlCopyStr(S1,1+Random(80000), 1+Random(1000));
    AlCopyStr(S1,1+Random(500), 1+Random(200));
    AlCopyStr(S1,1+Random(100000), 1+Random(100000));
    AlCopyStr(S1,1+Random(10), 1+Random(2));
  end;
  Showmessage('2,000,000 AlCopyStr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton14Click(Sender: TObject);
Var S1: String;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrU(500);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(25), 1+Random(10));
    Copy(S1,1+Random(50), 1+Random(4));
    Copy(S1,1+Random(50), 1+Random(30));
    Copy(S1,1+Random(40), 1+Random(1));
  end;

  S1 := ALRandomStrU(2000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(60), 1+Random(100));
    Copy(S1,1+Random(100), 1+Random(15));
    Copy(S1,1+Random(150), 1+Random(200));
    Copy(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStrU(30000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(1000), 1+Random(1000));
    Copy(S1,1+Random(100), 1+Random(200));
    Copy(S1,1+Random(3000), 1+Random(3000));
    Copy(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStrU(40000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(2000), 1+Random(1000));
    Copy(S1,1+Random(500), 1+Random(200));
    Copy(S1,1+Random(4000), 1+Random(4000));
    Copy(S1,1+Random(10), 1+Random(1));
  end;

  S1 := ALRandomStrU(1000000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(80000), 1+Random(1000));
    Copy(S1,1+Random(500), 1+Random(200));
    Copy(S1,1+Random(100000), 1+Random(100000));
    Copy(S1,1+Random(10), 1+Random(2));
  end;
  Showmessage('2,000,000 Copy in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton35Click(Sender: TObject);
Var S1: ansiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStr(500);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(25), 1+Random(10));
    Copy(S1,1+Random(50), 1+Random(4));
    Copy(S1,1+Random(50), 1+Random(30));
    Copy(S1,1+Random(40), 1+Random(1));
  end;

  S1 := ALRandomStr(2000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(60), 1+Random(100));
    Copy(S1,1+Random(100), 1+Random(15));
    Copy(S1,1+Random(150), 1+Random(200));
    Copy(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStr(30000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(1000), 1+Random(1000));
    Copy(S1,1+Random(100), 1+Random(200));
    Copy(S1,1+Random(3000), 1+Random(3000));
    Copy(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStr(40000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(2000), 1+Random(1000));
    Copy(S1,1+Random(500), 1+Random(200));
    Copy(S1,1+Random(4000), 1+Random(4000));
    Copy(S1,1+Random(10), 1+Random(1));
  end;

  S1 := ALRandomStr(1000000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(80000), 1+Random(1000));
    Copy(S1,1+Random(500), 1+Random(200));
    Copy(S1,1+Random(100000), 1+Random(100000));
    Copy(S1,1+Random(10), 1+Random(2));
  end;
  Showmessage('2,000,000 Copy in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

type

  PPROCESS_MEMORY_COUNTERS = ^PROCESS_MEMORY_COUNTERS;
  PROCESS_MEMORY_COUNTERS = record
    cb : DWORD;
    PageFaultCount : DWORD;
    PeakWorkingSetSize : DWORD;
    WorkingSetSize : DWORD; //Task managers MemUsage number
    QuotaPeakPagedPoolUsage : DWORD;
    QuotaPagedPoolUsage : DWORD;
    QuotaPeakNonPagedPoolUsage : DWORD;
    QuotaNonPagedPoolUsage : DWORD;
    PagefileUsage : DWORD; //TaskMan's VM Size number
    PeakPagefileUsage : DWORD;
  end;
  TProcessMemoryCounters = PROCESS_MEMORY_COUNTERS;

function GetProcessMemoryInfo(Process : THandle; var MemoryCounters : TProcessMemoryCounters; cb : DWORD) : BOOL; stdcall; external 'psapi.dll';

function ProcessMemoryUsage(ProcessID : DWORD): DWORD;
var ProcessHandle : THandle;
    MemCounters   : TProcessMemoryCounters;
begin
  Result := 0;
  ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
                               false,
                               ProcessID);
  try
    if GetProcessMemoryInfo(ProcessHandle,
                            MemCounters,
                            sizeof(MemCounters))
    then Result := MemCounters.WorkingSetSize;
  finally
    CloseHandle(ProcessHandle);
  end;
end;

procedure TForm1.ALButton23Click(Sender: TObject);
Var MemUsageAnsiString: AnsiString;
    MemoryUsage: DWORD;
    i: integer;
begin
  MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
  Setlength(MemUsageAnsiString,10000000);
  for i := 0 to length(MemUsageAnsiString) do
    MemUsageAnsiString[i] := 'A';
  Showmessage('Memory used by a AnsiString of 10,000,000 low ASCII chars: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)));
end;

procedure TForm1.ALButton24Click(Sender: TObject);
Var MemUsageString: String;
    MemoryUsage: DWORD;
    i: integer;
begin
  MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
  Setlength(MemUsageString,10000000);
  for i := 0 to length(MemUsageString) do
    MemUsageString[i] := 'A';
  Showmessage('Memory used by a String of 10,000,000 low ASCII chars: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)));
end;

procedure TForm1.ALButton40Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TALformatSettings;
    S1: AnsiString;
begin
  ALGetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  For i := 0 to 1000000 do begin
    S1 := ALFormat('xxx %s xxxx %s xxxx', [AlRandomStr(10), AlRandomStr(20)], aFormatSettings);
    S1 := ALFormat('xxx %*.*f xxx', [random(10), random(5), random(MaxInt) / random(MaxInt)], aFormatSettings);
    S1 := ALFormat('xxx %12.2m', [random(MaxInt) / random(MaxInt)], aFormatSettings);
  end;
  Showmessage('3,000,000 ALFormat in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton43Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TformatSettings;
    S1: String;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  For i := 0 to 1000000 do begin
    S1 := Format('xxx %s xxxx %s xxxx', [AlRandomStrU(10), AlRandomStrU(20)], aFormatSettings);
    S1 := Format('xxx %*.*f xxx', [random(10), random(5), random(MaxInt) / random(MaxInt)], aFormatSettings);
    S1 := Format('xxx %12.2m', [random(MaxInt) / random(MaxInt)], aFormatSettings);
  end;
  Showmessage('3,000,000 Format in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;


procedure TForm1.ALButton44Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TformatSettings;
    S1: AnsiString;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  For i := 0 to 1000000 do begin
    S1 := AnsiString(Format('xxx %s xxxx %s xxxx', [AlRandomStr(10), AlRandomStr(20)], aFormatSettings));
    S1 := AnsiString(Format('xxx %*.*f xxx', [random(10), random(5), random(MaxInt) / random(MaxInt)], aFormatSettings));
    S1 := AnsiString(Format('xxx %12.2m', [random(MaxInt) / random(MaxInt)], aFormatSettings));
  end;
  Showmessage('3,000,000 Format in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;


procedure TForm1.ALButton45Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TALformatSettings;
    S1: AnsiString;
    E1: Extended;
begin
  ALGetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  S1 := '121.23928322';
  For i := 0 to 10000000 do begin
    ALTryStrToFloat(S1, E1, aFormatSettings);
  end;
  Showmessage('10,000,000 ALTrystrFloat in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton46Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TformatSettings;
    S1: String;
    E1: Extended;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  S1 := '121.23928322';
  For i := 0 to 10000000 do begin
    TryStrToFloat(S1, E1, aFormatSettings);
  end;
  Showmessage('10,000,000 ALTrystrFloat in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

procedure TForm1.ALButton47Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    aFormatSettings: TformatSettings;
    S1: AnsiString;
    E1: Extended;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  StartDate := Now;
  S1 := '121.23928322';
  For i := 0 to 10000000 do begin
    TryStrToFloat(String(S1), E1, aFormatSettings);
  end;
  Showmessage('10,000,000 ALTrystrFloat in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
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
  SetMultiByteConversionCodePage(CP_UTF8);
{$ENDIF}

end.

