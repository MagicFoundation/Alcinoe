unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, Shellapi, Soap.EncdDecd,
  System.Diagnostics, system.NetEncoding;

type
  TForm1 = class(TForm)
    ALButton1: TButton;
    ALButton2: TButton;
    ALButton3: TButton;
    ALButton4: TButton;
    ALButton5: TButton;
    ALButton6: TButton;
    ALButton7: TButton;
    ALButton8: TButton;
    ALButton9: TButton;
    ALButton10: TButton;
    ALButton11: TButton;
    ALButton12: TButton;
    ALButton13: TButton;
    ALButton14: TButton;
    ALButton15: TButton;
    ALButton16: TButton;
    ALButton17: TButton;
    ALButton18: TButton;
    ALButton19: TButton;
    ALButton20: TButton;
    ALButton21: TButton;
    ALButton22: TButton;
    ALButton23: TButton;
    ALButton24: TButton;
    ALButton25: TButton;
    ALButton26: TButton;
    ALButton27: TButton;
    ALButton28: TButton;
    ALButton29: TButton;
    ALButton30: TButton;
    ALButton31: TButton;
    ALButton32: TButton;
    ALButton33: TButton;
    ALButton34: TButton;
    ALButton35: TButton;
    ALButton40: TButton;
    ALButton43: TButton;
    ALButton45: TButton;
    ALButton46: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Panel1: TPanel;
    Label1: TLabel;
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
    procedure ALButton16Click(Sender: TObject);
    procedure ALButton32Click(Sender: TObject);
    procedure ALButton33Click(Sender: TObject);
    procedure ALButton34Click(Sender: TObject);
    procedure ALButton35Click(Sender: TObject);
    procedure ALButton40Click(Sender: TObject);
    procedure ALButton43Click(Sender: TObject);
    procedure ALButton45Click(Sender: TObject);
    procedure ALButton46Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
     Alcinoe.StringUtils;

{$R *.dfm}

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
  S1 := ALRandomStrA(50);
  For i := 0 to 100 do begin
    S1 := ALStringReplaceA(S1,ALRandomStrA(1),ALRandomStrA(8),[]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(3),ALRandomStrA(3),[]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(4),ALRandomStrA(6),[RfReplaceAll]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(3),ALRandomStrA(12),[RfIgnoreCase]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(2),ALRandomStrA(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrA(200);
  For i := 0 to 100 do begin
    S1 := ALStringReplaceA(S1,ALRandomStrA(1),ALRandomStrA(8),[]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(15),ALRandomStrA(3),[]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(35),ALRandomStrA(6),[RfReplaceAll]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(12),ALRandomStrA(12),[RfIgnoreCase]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(11),ALRandomStrA(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrA(3000);
  For i := 0 to 100 do begin
    S1 := ALStringReplaceA(S1,ALRandomStrA(1),ALRandomStrA(8),[]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(3),ALRandomStrA(3),[]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(4),ALRandomStrA(6),[RfReplaceAll]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(3),ALRandomStrA(12),[RfIgnoreCase]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(2),ALRandomStrA(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrA(4000);
  For i := 0 to 100 do begin
    S1 := ALStringReplaceA(S1,ALRandomStrA(1),ALRandomStrA(8),[]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(3),ALRandomStrA(3),[]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(4),ALRandomStrA(6),[RfReplaceAll]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(3),ALRandomStrA(12),[RfIgnoreCase]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(2),ALRandomStrA(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrA(100000);
  For i := 0 to 100 do begin
    S1 := ALStringReplaceA(S1,ALRandomStrA(1),ALRandomStrA(8),[]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(3),ALRandomStrA(3),[]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(4),ALRandomStrA(6),[RfReplaceAll]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(3),ALRandomStrA(12),[RfIgnoreCase]);
    S1 := ALStringReplaceA(S1,ALRandomStrA(2),ALRandomStrA(20),[RfReplaceAll, RfIgnoreCase]);
  end;
  Showmessage('2,500 ALStringReplaceA in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton2Click(Sender: TObject);
Var S1: String;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrW(50);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrW(1),ALRandomStrW(8),[]);
    S1 := StringReplace(S1,ALRandomStrW(3),ALRandomStrW(3),[]);
    S1 := StringReplace(S1,ALRandomStrW(4),ALRandomStrW(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrW(3),ALRandomStrW(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrW(2),ALRandomStrW(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrW(200);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrW(1),ALRandomStrW(8),[]);
    S1 := StringReplace(S1,ALRandomStrW(15),ALRandomStrW(3),[]);
    S1 := StringReplace(S1,ALRandomStrW(35),ALRandomStrW(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrW(12),ALRandomStrW(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrW(11),ALRandomStrW(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrW(3000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrW(1),ALRandomStrW(8),[]);
    S1 := StringReplace(S1,ALRandomStrW(3),ALRandomStrW(3),[]);
    S1 := StringReplace(S1,ALRandomStrW(4),ALRandomStrW(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrW(3),ALRandomStrW(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrW(2),ALRandomStrW(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrW(4000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrW(1),ALRandomStrW(8),[]);
    S1 := StringReplace(S1,ALRandomStrW(3),ALRandomStrW(3),[]);
    S1 := StringReplace(S1,ALRandomStrW(4),ALRandomStrW(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrW(3),ALRandomStrW(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrW(2),ALRandomStrW(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrW(100000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrW(1),ALRandomStrW(8),[]);
    S1 := StringReplace(S1,ALRandomStrW(3),ALRandomStrW(3),[]);
    S1 := StringReplace(S1,ALRandomStrW(4),ALRandomStrW(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrW(3),ALRandomStrW(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrW(2),ALRandomStrW(20),[RfReplaceAll, RfIgnoreCase]);
  end;
  Showmessage('2,500 StringReplace in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton29Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrA(50);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrA(1),ALRandomStrA(8),[]);
    S1 := StringReplace(S1,ALRandomStrA(3),ALRandomStrA(3),[]);
    S1 := StringReplace(S1,ALRandomStrA(4),ALRandomStrA(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrA(3),ALRandomStrA(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrA(2),ALRandomStrA(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrA(200);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrA(1),ALRandomStrA(8),[]);
    S1 := StringReplace(S1,ALRandomStrA(15),ALRandomStrA(3),[]);
    S1 := StringReplace(S1,ALRandomStrA(35),ALRandomStrA(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrA(12),ALRandomStrA(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrA(11),ALRandomStrA(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrA(3000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrA(1),ALRandomStrA(8),[]);
    S1 := StringReplace(S1,ALRandomStrA(3),ALRandomStrA(3),[]);
    S1 := StringReplace(S1,ALRandomStrA(4),ALRandomStrA(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrA(3),ALRandomStrA(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrA(2),ALRandomStrA(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrA(4000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrA(1),ALRandomStrA(8),[]);
    S1 := StringReplace(S1,ALRandomStrA(3),ALRandomStrA(3),[]);
    S1 := StringReplace(S1,ALRandomStrA(4),ALRandomStrA(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrA(3),ALRandomStrA(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrA(2),ALRandomStrA(20),[RfReplaceAll, RfIgnoreCase]);
  end;

  S1 := ALRandomStrA(100000);
  For i := 0 to 100 do begin
    S1 := StringReplace(S1,ALRandomStrA(1),ALRandomStrA(8),[]);
    S1 := StringReplace(S1,ALRandomStrA(3),ALRandomStrA(3),[]);
    S1 := StringReplace(S1,ALRandomStrA(4),ALRandomStrA(6),[RfReplaceAll]);
    S1 := StringReplace(S1,ALRandomStrA(3),ALRandomStrA(12),[RfIgnoreCase]);
    S1 := StringReplace(S1,ALRandomStrA(2),ALRandomStrA(20),[RfReplaceAll, RfIgnoreCase]);
  end;
  Showmessage('2,500 StringReplace in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton3Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrA(50);
  For i := 0 to 2000 do begin
    ALPosA(ALRandomStrA(1),S1,1+random(50));
    ALPosA(ALRandomStrA(3),S1,1+random(50));
    ALPosA(ALRandomStrA(8),S1,1+random(50));
    ALPosA(ALRandomStrA(20),S1,1+random(50));
  end;

  S1 := ALRandomStrA(200);
  For i := 0 to 2000 do begin
    ALPosA(ALRandomStrA(1),S1,1+random(50));
    ALPosA(ALRandomStrA(3),S1,1+random(50));
    ALPosA(ALRandomStrA(8),S1,1+random(50));
    ALPosA(ALRandomStrA(20),S1,1+random(50));
  end;

  S1 := ALRandomStrA(3000);
  For i := 0 to 2000 do begin
    ALPosA(ALRandomStrA(1),S1,1+random(50));
    ALPosA(ALRandomStrA(3),S1,1+random(50));
    ALPosA(ALRandomStrA(8),S1,1+random(50));
    ALPosA(ALRandomStrA(20),S1,1+random(50));
  end;

  S1 := ALRandomStrA(4000);
  For i := 0 to 2000 do begin
    ALPosA(ALRandomStrA(1),S1,1+random(50));
    ALPosA(ALRandomStrA(3),S1,1+random(50));
    ALPosA(ALRandomStrA(8),S1,1+random(50));
    ALPosA(ALRandomStrA(20),S1,1+random(50));
  end;

  S1 := ALRandomStrA(300000);
  For i := 0 to 2000 do begin
    ALPosA(ALRandomStrA(1),S1,1+random(50));
    ALPosA(ALRandomStrA(3),S1,1+random(50));
    ALPosA(ALRandomStrA(8),S1,1+random(50));
    ALPosA(ALRandomStrA(20),S1,1+random(50));
  end;
  Showmessage('40,000 ALPosA in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton4Click(Sender: TObject);
Var S1: String;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrW(50);
  For i := 0 to 2000 do begin
    PosEx(ALRandomStrW(1),S1,1+random(50));
    PosEx(ALRandomStrW(3),S1,1+random(50));
    PosEx(ALRandomStrW(8),S1,1+random(50));
    PosEx(ALRandomStrW(20),S1,1+random(50));
  end;

  S1 := ALRandomStrW(200);
  For i := 0 to 2000 do begin
    PosEx(ALRandomStrW(1),S1,1+random(50));
    PosEx(ALRandomStrW(3),S1,1+random(50));
    PosEx(ALRandomStrW(8),S1,1+random(50));
    PosEx(ALRandomStrW(20),S1,1+random(50));
  end;

  S1 := ALRandomStrW(3000);
  For i := 0 to 2000 do begin
    PosEx(ALRandomStrW(1),S1,1+random(50));
    PosEx(ALRandomStrW(3),S1,1+random(50));
    PosEx(ALRandomStrW(8),S1,1+random(50));
    PosEx(ALRandomStrW(20),S1,1+random(50));
  end;

  S1 := ALRandomStrW(4000);
  For i := 0 to 2000 do begin
    PosEx(ALRandomStrW(1),S1,1+random(50));
    PosEx(ALRandomStrW(3),S1,1+random(50));
    PosEx(ALRandomStrW(8),S1,1+random(50));
    PosEx(ALRandomStrW(20),S1,1+random(50));
  end;

  S1 := ALRandomStrW(300000);
  For i := 0 to 2000 do begin
    PosEx(ALRandomStrW(1),S1,1+random(50));
    PosEx(ALRandomStrW(3),S1,1+random(50));
    PosEx(ALRandomStrW(8),S1,1+random(50));
    PosEx(ALRandomStrW(20),S1,1+random(50));
  end;
  Showmessage('40,000 PosEx in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton30Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrA(50);
  For i := 0 to 2000 do begin
    PosEx(ALRandomStrA(1),S1,1+random(50));
    PosEx(ALRandomStrA(3),S1,1+random(50));
    PosEx(ALRandomStrA(8),S1,1+random(50));
    PosEx(ALRandomStrA(20),S1,1+random(50));
  end;

  S1 := ALRandomStrA(200);
  For i := 0 to 2000 do begin
    PosEx(ALRandomStrA(1),S1,1+random(50));
    PosEx(ALRandomStrA(3),S1,1+random(50));
    PosEx(ALRandomStrA(8),S1,1+random(50));
    PosEx(ALRandomStrA(20),S1,1+random(50));
  end;

  S1 := ALRandomStrA(3000);
  For i := 0 to 2000 do begin
    PosEx(ALRandomStrA(1),S1,1+random(50));
    PosEx(ALRandomStrA(3),S1,1+random(50));
    PosEx(ALRandomStrA(8),S1,1+random(50));
    PosEx(ALRandomStrA(20),S1,1+random(50));
  end;

  S1 := ALRandomStrA(4000);
  For i := 0 to 2000 do begin
    PosEx(ALRandomStrA(1),S1,1+random(50));
    PosEx(ALRandomStrA(3),S1,1+random(50));
    PosEx(ALRandomStrA(8),S1,1+random(50));
    PosEx(ALRandomStrA(20),S1,1+random(50));
  end;

  S1 := ALRandomStrA(300000);
  For i := 0 to 2000 do begin
    PosEx(ALRandomStrA(1),S1,1+random(50));
    PosEx(ALRandomStrA(3),S1,1+random(50));
    PosEx(ALRandomStrA(8),S1,1+random(50));
    PosEx(ALRandomStrA(20),S1,1+random(50));
  end;
  Showmessage('40,000 PosEx in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton5Click(Sender: TObject);
Var S1,S2,S3,S4,S5: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrA(50);
  S2 := ALRandomStrA(1);
  S3 := ALRandomStrA(3);
  S4 := ALRandomStrA(8);
  S5 := ALRandomStrA(20);
  For i := 0 to 2000 do begin
    ALPosA(S2,S1);
    ALPosA(S3,S1);
    ALPosA(S4,S1);
    ALPosA(S5,S1);
  end;

  S1 := ALRandomStrA(200);
  S2 := ALRandomStrA(1);
  S3 := ALRandomStrA(3);
  S4 := ALRandomStrA(8);
  S5 := ALRandomStrA(20);
  For i := 0 to 2000 do begin
    ALPosA(S2,S1);
    ALPosA(S3,S1);
    ALPosA(S4,S1);
    ALPosA(S5,S1);
  end;

  S1 := ALRandomStrA(3000);
  S2 := ALRandomStrA(1);
  S3 := ALRandomStrA(3);
  S4 := ALRandomStrA(8);
  S5 := ALRandomStrA(20);
  For i := 0 to 2000 do begin
    ALPosA(S2,S1);
    ALPosA(S3,S1);
    ALPosA(S4,S1);
    ALPosA(S5,S1);
  end;

  S1 := ALRandomStrA(4000);
  S2 := ALRandomStrA(1);
  S3 := ALRandomStrA(3);
  S4 := ALRandomStrA(8);
  S5 := ALRandomStrA(20);
  For i := 0 to 2000 do begin
    ALPosA(S2,S1);
    ALPosA(S3,S1);
    ALPosA(S4,S1);
    ALPosA(S5,S1);
  end;

  S1 := ALRandomStrA(300000);
  S2 := ALRandomStrA(1);
  S3 := ALRandomStrA(3);
  S4 := ALRandomStrA(8);
  S5 := ALRandomStrA(20);
  For i := 0 to 2000 do begin
    ALPosA(S2,S1);
    ALPosA(S3,S1);
    ALPosA(S4,S1);
    ALPosA(S5,S1);
  end;
  Showmessage('40,000 ALPosA in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton6Click(Sender: TObject);
Var S1,S2,S3,S4,S5: String;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrW(50);
  S2 := ALRandomStrW(1);
  S3 := ALRandomStrW(3);
  S4 := ALRandomStrW(8);
  S5 := ALRandomStrW(20);
  For i := 0 to 2000 do begin
    Pos(S2,S1);
    Pos(S3,S1);
    Pos(S4,S1);
    Pos(S5,S1);
  end;

  S1 := ALRandomStrW(200);
  S2 := ALRandomStrW(1);
  S3 := ALRandomStrW(3);
  S4 := ALRandomStrW(8);
  S5 := ALRandomStrW(20);
  For i := 0 to 2000 do begin
    Pos(S2,S1);
    Pos(S3,S1);
    Pos(S4,S1);
    Pos(S5,S1);
  end;

  S1 := ALRandomStrW(3000);
  S2 := ALRandomStrW(1);
  S3 := ALRandomStrW(3);
  S4 := ALRandomStrW(8);
  S5 := ALRandomStrW(20);
  For i := 0 to 2000 do begin
    Pos(S2,S1);
    Pos(S3,S1);
    Pos(S4,S1);
    Pos(S5,S1);
  end;

  S1 := ALRandomStrW(4000);
  S2 := ALRandomStrW(1);
  S3 := ALRandomStrW(3);
  S4 := ALRandomStrW(8);
  S5 := ALRandomStrW(20);
  For i := 0 to 2000 do begin
    Pos(S2,S1);
    Pos(S3,S1);
    Pos(S4,S1);
    Pos(S5,S1);
  end;

  S1 := ALRandomStrW(300000);
  S2 := ALRandomStrW(1);
  S3 := ALRandomStrW(3);
  S4 := ALRandomStrW(8);
  S5 := ALRandomStrW(20);
  For i := 0 to 2000 do begin
    Pos(S2,S1);
    Pos(S3,S1);
    Pos(S4,S1);
    Pos(S5,S1);
  end;
  Showmessage('40,000 Pos in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton31Click(Sender: TObject);
Var S1,S2,S3,S4,S5: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrA(50);
  S2 := ALRandomStrA(1);
  S3 := ALRandomStrA(3);
  S4 := ALRandomStrA(8);
  S5 := ALRandomStrA(20);
  For i := 0 to 2000 do begin
    Pos(S2,S1);
    Pos(S3,S1);
    Pos(S4,S1);
    Pos(S5,S1);
  end;

  S1 := ALRandomStrA(200);
  S2 := ALRandomStrA(1);
  S3 := ALRandomStrA(3);
  S4 := ALRandomStrA(8);
  S5 := ALRandomStrA(20);
  For i := 0 to 2000 do begin
    Pos(S2,S1);
    Pos(S3,S1);
    Pos(S4,S1);
    Pos(S5,S1);
  end;

  S1 := ALRandomStrA(3000);
  S2 := ALRandomStrA(1);
  S3 := ALRandomStrA(3);
  S4 := ALRandomStrA(8);
  S5 := ALRandomStrA(20);
  For i := 0 to 2000 do begin
    Pos(S2,S1);
    Pos(S3,S1);
    Pos(S4,S1);
    Pos(S5,S1);
  end;

  S1 := ALRandomStrA(4000);
  S2 := ALRandomStrA(1);
  S3 := ALRandomStrA(3);
  S4 := ALRandomStrA(8);
  S5 := ALRandomStrA(20);
  For i := 0 to 2000 do begin
    Pos(S2,S1);
    Pos(S3,S1);
    Pos(S4,S1);
    Pos(S5,S1);
  end;

  S1 := ALRandomStrA(300000);
  S2 := ALRandomStrA(1);
  S3 := ALRandomStrA(3);
  S4 := ALRandomStrA(8);
  S5 := ALRandomStrA(20);
  For i := 0 to 2000 do begin
    Pos(S2,S1);
    Pos(S3,S1);
    Pos(S4,S1);
    Pos(S5,S1);
  end;
  Showmessage('40,000 Pos in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton21Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    LFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
begin
  ALGetLocaleFormatSettings(1033, LFormatSettings);
  StartDate := Now;
  For i := 0 to 1000000 do begin
    S1 := ALDateToStrA(Random(10000), LFormatSettings);
  end;
  Showmessage('1,000,000 ALDateToStrA in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton22Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    LFormatSettings: TformatSettings;
    S1: String;
begin
  GetLocaleFormatSettings(1033, LFormatSettings);
  StartDate := Now;
  For i := 0 to 1000000 do begin
    S1 := Datetostr(Random(10000), LFormatSettings);
  end;
  Showmessage('1,000,000 Datetostr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton27Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    LFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
    D1: TdateTime;
begin
  ALGetLocaleFormatSettings(1033, LFormatSettings);
  StartDate := Now;
  S1 := '12/10/2012';
  For i := 0 to 1000000 do begin
    ALTrystrToDateTime(S1, D1, LFormatSettings);
  end;
  Showmessage('1,000,000 ALTrystrToDateTime in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton28Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    LFormatSettings: TformatSettings;
    S1: String;
    D1: TdateTime;
begin
  GetLocaleFormatSettings(1033, LFormatSettings);
  StartDate := Now;
  S1 := '12/10/2012';
  For i := 0 to 1000000 do begin
    TrystrToDateTime(S1, D1, LFormatSettings);
  end;
  Showmessage('1,000,000 TrystrToDateTime in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton17Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: AnsiString;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := ALRandomStrA(5,['0','1','2','3','4','5','6','7','8','9']);
    ALStrToInt(S1);
  end;
  Showmessage('10,000,000 ALStrToInt in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton18Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: String;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := ALRandomStrW(5,['0','1','2','3','4','5','6','7','8','9']);
    StrToInt(S1);
  end;
  Showmessage('10,000,000 StrToInt in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton19Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: AnsiString;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := ALRandomStrA(10,['0','1','2','3','4','5','6','7','8','9']);
    ALStrToInt64(S1);
  end;
  Showmessage('10,000,000 ALStrToInt64 in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton20Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: String;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := ALRandomStrW(10,['0','1','2','3','4','5','6','7','8','9']);
    StrToInt64(S1);
  end;
  Showmessage('10,000,000 StrToInt64 in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton15Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    S1: AnsiString;
begin
  StartDate := Now;
  For i := 0 to 10000000 do begin
    S1 := ALIntToStrA(Random(maxint));
  end;
  Showmessage('10,000,000 ALIntToStrA in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
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

{***********************************************}
procedure TForm1.ALButton7Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrA(50);
  For i := 0 to 1000 do begin
    ALCompareTextA(S1,ALRandomStrA(25));
    ALCompareTextA(S1,ALRandomStrA(50));
    ALCompareTextA(S1,S1);
  end;

  S1 := ALRandomStrA(200);
  For i := 0 to 1000 do begin
    ALCompareTextA(S1,ALRandomStrA(25));
    ALCompareTextA(S1,ALRandomStrA(200));
    ALCompareTextA(S1,S1);
  end;

  S1 := ALRandomStrA(3000);
  For i := 0 to 1000 do begin
    ALCompareTextA(S1,ALRandomStrA(25));
    ALCompareTextA(S1,ALRandomStrA(3000));
    ALCompareTextA(S1,S1);
  end;

  S1 := ALRandomStrA(4000);
  For i := 0 to 1000 do begin
    ALCompareTextA(S1,ALRandomStrA(25));
    ALCompareTextA(S1,ALRandomStrA(4000));
    ALCompareTextA(S1,S1);
  end;

  S1 := ALRandomStrA(10000);
  For i := 0 to 1000 do begin
    ALCompareTextA(S1,ALRandomStrA(25));
    ALCompareTextA(S1,ALRandomStrA(100000));
    ALCompareTextA(S1,S1);
  end;
  Showmessage('15,000 ALCompareTextA in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton8Click(Sender: TObject);
Var S1: String;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrW(50);
  For i := 0 to 1000 do begin
    CompareText(S1,ALRandomStrW(25));
    CompareText(S1,ALRandomStrW(50));
    CompareText(S1,S1);
  end;

  S1 := ALRandomStrW(200);
  For i := 0 to 1000 do begin
    CompareText(S1,ALRandomStrW(25));
    CompareText(S1,ALRandomStrW(200));
    CompareText(S1,S1);
  end;

  S1 := ALRandomStrW(3000);
  For i := 0 to 1000 do begin
    CompareText(S1,ALRandomStrW(25));
    CompareText(S1,ALRandomStrW(3000));
    CompareText(S1,S1);
  end;

  S1 := ALRandomStrW(4000);
  For i := 0 to 1000 do begin
    CompareText(S1,ALRandomStrW(25));
    CompareText(S1,ALRandomStrW(4000));
    CompareText(S1,S1);
  end;

  S1 := ALRandomStrW(10000);
  For i := 0 to 1000 do begin
    CompareText(S1,ALRandomStrW(25));
    CompareText(S1,ALRandomStrW(100000));
    CompareText(S1,S1);
  end;
  Showmessage('15,000 CompareText in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton32Click(Sender: TObject);
Var S1: ansiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrA(50);
  For i := 0 to 1000 do begin
    CompareText(S1,ALRandomStrA(25));
    CompareText(S1,ALRandomStrA(50));
    CompareText(S1,S1);
  end;

  S1 := ALRandomStrA(200);
  For i := 0 to 1000 do begin
    CompareText(S1,ALRandomStrA(25));
    CompareText(S1,ALRandomStrA(200));
    CompareText(S1,S1);
  end;

  S1 := ALRandomStrA(3000);
  For i := 0 to 1000 do begin
    CompareText(S1,ALRandomStrA(25));
    CompareText(S1,ALRandomStrA(3000));
    CompareText(S1,S1);
  end;

  S1 := ALRandomStrA(4000);
  For i := 0 to 1000 do begin
    CompareText(S1,ALRandomStrA(25));
    CompareText(S1,ALRandomStrA(4000));
    CompareText(S1,S1);
  end;

  S1 := ALRandomStrA(10000);
  For i := 0 to 1000 do begin
    CompareText(S1,ALRandomStrA(25));
    CompareText(S1,ALRandomStrA(100000));
    CompareText(S1,S1);
  end;
  Showmessage('15,000 CompareText in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton25Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    LFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
begin
  ALGetLocaleFormatSettings(1033, LFormatSettings);
  StartDate := Now;
  For i := 0 to 5000000 do begin
    S1 := ALFloatToStrA(random(MaxInt) / random(Maxint), LFormatSettings);
  end;
  Showmessage('5,000,000 ALFloatToStrA in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton26Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    LFormatSettings: TformatSettings;
    S1: String;
begin
  GetLocaleFormatSettings(1033, LFormatSettings);
  StartDate := Now;
  For i := 0 to 5000000 do begin
    S1 := FloatToStr(random(MaxInt) / random(Maxint), LFormatSettings);
  end;
  Showmessage('5,000,000 FloatToStr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.ALButton9Click(Sender: TObject);
Var i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  For i := 0 to 10000 do begin
    AlUpperCase(ALRandomStrA(5));
    AlUpperCase(ALRandomStrA(25));
    AlUpperCase(ALRandomStrA(300));
    AlUpperCase(ALRandomStrA(3000));
    AlUpperCase(ALRandomStrA(10000));
  end;
  Showmessage('50,000 AlUpperCase in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{*********************************************}
procedure TForm1.Button1Click(Sender: TObject);
Var i: integer;
    StopWatch: TStopWatch;
begin
  StopWatch := TStopWatch.StartNew;
  For i := 0 to 10000 do begin
    ALBase64EncodeString(ALRandomStrA(5));
    ALBase64EncodeString(ALRandomStrA(25));
    ALBase64EncodeString(ALRandomStrA(300));
    ALBase64EncodeString(ALRandomStrA(3000));
    ALBase64EncodeString(ALRandomStrA(10000));
  end;
  StopWatch.Stop;
  Showmessage('50,000 Base64Encode in: ' + formatFloat('0.,',StopWatch.Elapsed.TotalMilliseconds) + ' ms');
end;

{*********************************************}
procedure TForm1.Button2Click(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function EncodeString(const Input: string): string;
  var
    InStr, OutStr: TStringStream;
  begin
    InStr := TStringStream.Create(Input, TEncoding.UTF8);
    try
      OutStr := TStringStream.Create('');
      try
        EncodeStream(InStr, OutStr);
        Result := OutStr.DataString;
      finally
        OutStr.Free;
      end;
    finally
      InStr.Free;
    end;
  end;

Var i: integer;
    StopWatch: TStopWatch;
begin
  StopWatch := TStopWatch.StartNew;
  For i := 0 to 10000 do begin
    EncodeString(ALRandomStrW(5));
    EncodeString(ALRandomStrW(25));
    EncodeString(ALRandomStrW(300));
    EncodeString(ALRandomStrW(3000));
    EncodeString(ALRandomStrW(10000));
  end;
  StopWatch.Stop;
  Showmessage('50,000 Base64Encode in: ' + formatFloat('0.,',StopWatch.Elapsed.TotalMilliseconds) + ' ms');
end;

{*********************************************}
procedure TForm1.Button3Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrA(50, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
  For i := 0 to 2000 do begin
    ALPosIgnoreCaseA(ALRandomStrA(1),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(3),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(8),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(20),S1,1+random(50));
  end;

  S1 := ALRandomStrA(200, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
  For i := 0 to 2000 do begin
    ALPosIgnoreCaseA(ALRandomStrA(1),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(3),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(8),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(20),S1,1+random(50));
  end;

  S1 := ALRandomStrA(3000, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
  For i := 0 to 2000 do begin
    ALPosIgnoreCaseA(ALRandomStrA(1),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(3),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(8),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(20),S1,1+random(50));
  end;

  S1 := ALRandomStrA(4000, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
  For i := 0 to 2000 do begin
    ALPosIgnoreCaseA(ALRandomStrA(1),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(3),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(8),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(20),S1,1+random(50));
  end;

  S1 := ALRandomStrA(300000, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
  For i := 0 to 2000 do begin
    ALPosIgnoreCaseA(ALRandomStrA(1),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(3),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(8),S1,1+random(50));
    ALPosIgnoreCaseA(ALRandomStrA(20),S1,1+random(50));
  end;
  Showmessage('40,000 ALPosIgnoreCaseA in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{*********************************************}
procedure TForm1.Button4Click(Sender: TObject);
Var S1: String;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrW(50, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
  For i := 0 to 2000 do begin
    ALPosIgnoreCaseW(ALRandomStrW(1),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(3),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(8),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(20),S1,1+random(50));
  end;

  S1 := ALRandomStrW(200, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
  For i := 0 to 2000 do begin
    ALPosIgnoreCaseW(ALRandomStrW(1),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(3),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(8),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(20),S1,1+random(50));
  end;

  S1 := ALRandomStrW(3000, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
  For i := 0 to 2000 do begin
    ALPosIgnoreCaseW(ALRandomStrW(1),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(3),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(8),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(20),S1,1+random(50));
  end;

  S1 := ALRandomStrW(4000, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
  For i := 0 to 2000 do begin
    ALPosIgnoreCaseW(ALRandomStrW(1),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(3),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(8),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(20),S1,1+random(50));
  end;

  S1 := ALRandomStrW(300000, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
  For i := 0 to 2000 do begin
    ALPosIgnoreCaseW(ALRandomStrW(1),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(3),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(8),S1,1+random(50));
    ALPosIgnoreCaseW(ALRandomStrW(20),S1,1+random(50));
  end;
  Showmessage('40,000 ALPosIgnoreCaseA in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton10Click(Sender: TObject);
Var i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  For i := 0 to 10000 do begin
    UpperCase(ALRandomStrW(5));
    UpperCase(ALRandomStrW(25));
    UpperCase(ALRandomStrW(300));
    UpperCase(ALRandomStrW(3000));
    UpperCase(ALRandomStrW(10000));
  end;
  Showmessage('50,000 UpperCase in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton33Click(Sender: TObject);
Var i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  For i := 0 to 10000 do begin
    UpperCase(ALRandomStrA(5));
    UpperCase(ALRandomStrA(25));
    UpperCase(ALRandomStrA(300));
    UpperCase(ALRandomStrA(3000));
    UpperCase(ALRandomStrA(10000));
  end;
  Showmessage('50,000 UpperCase in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton11Click(Sender: TObject);
Var i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  For i := 0 to 10000 do begin
    AlLowerCase(ALRandomStrA(5));
    AlLowerCase(ALRandomStrA(25));
    AlLowerCase(ALRandomStrA(300));
    AlLowerCase(ALRandomStrA(3000));
    AlLowerCase(ALRandomStrA(10000));
  end;
  Showmessage('50,000 AlLowerCase in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton12Click(Sender: TObject);
Var i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  For i := 0 to 10000 do begin
    LowerCase(ALRandomStrW(5));
    LowerCase(ALRandomStrW(25));
    LowerCase(ALRandomStrW(300));
    LowerCase(ALRandomStrW(3000));
    LowerCase(ALRandomStrW(10000));
  end;
  Showmessage('50,000 LowerCase in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton34Click(Sender: TObject);
Var i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  For i := 0 to 10000 do begin
    LowerCase(ALRandomStrA(5));
    LowerCase(ALRandomStrA(25));
    LowerCase(ALRandomStrA(300));
    LowerCase(ALRandomStrA(3000));
    LowerCase(ALRandomStrA(10000));
  end;
  Showmessage('50,000 LowerCase in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton13Click(Sender: TObject);
Var S1: AnsiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrA(500);
  For i := 0 to 100000 do begin
    AlCopyStr(S1,1+Random(25), 1+Random(10));
    AlCopyStr(S1,1+Random(50), 1+Random(4));
    AlCopyStr(S1,1+Random(50), 1+Random(30));
    AlCopyStr(S1,1+Random(40), 1+Random(1));
  end;

  S1 := ALRandomStrA(2000);
  For i := 0 to 100000 do begin
    AlCopyStr(S1,1+Random(60), 1+Random(100));
    AlCopyStr(S1,1+Random(100), 1+Random(15));
    AlCopyStr(S1,1+Random(150), 1+Random(200));
    AlCopyStr(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStrA(30000);
  For i := 0 to 100000 do begin
    AlCopyStr(S1,1+Random(1000), 1+Random(1000));
    AlCopyStr(S1,1+Random(100), 1+Random(200));
    AlCopyStr(S1,1+Random(3000), 1+Random(3000));
    AlCopyStr(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStrA(40000);
  For i := 0 to 100000 do begin
    AlCopyStr(S1,1+Random(2000), 1+Random(1000));
    AlCopyStr(S1,1+Random(500), 1+Random(200));
    AlCopyStr(S1,1+Random(4000), 1+Random(4000));
    AlCopyStr(S1,1+Random(10), 1+Random(1));
  end;

  S1 := ALRandomStrA(1000000);
  For i := 0 to 100000 do begin
    AlCopyStr(S1,1+Random(80000), 1+Random(1000));
    AlCopyStr(S1,1+Random(500), 1+Random(200));
    AlCopyStr(S1,1+Random(100000), 1+Random(100000));
    AlCopyStr(S1,1+Random(10), 1+Random(2));
  end;
  Showmessage('2,000,000 AlCopyStr in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton14Click(Sender: TObject);
Var S1: String;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrW(500);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(25), 1+Random(10));
    Copy(S1,1+Random(50), 1+Random(4));
    Copy(S1,1+Random(50), 1+Random(30));
    Copy(S1,1+Random(40), 1+Random(1));
  end;

  S1 := ALRandomStrW(2000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(60), 1+Random(100));
    Copy(S1,1+Random(100), 1+Random(15));
    Copy(S1,1+Random(150), 1+Random(200));
    Copy(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStrW(30000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(1000), 1+Random(1000));
    Copy(S1,1+Random(100), 1+Random(200));
    Copy(S1,1+Random(3000), 1+Random(3000));
    Copy(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStrW(40000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(2000), 1+Random(1000));
    Copy(S1,1+Random(500), 1+Random(200));
    Copy(S1,1+Random(4000), 1+Random(4000));
    Copy(S1,1+Random(10), 1+Random(1));
  end;

  S1 := ALRandomStrW(1000000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(80000), 1+Random(1000));
    Copy(S1,1+Random(500), 1+Random(200));
    Copy(S1,1+Random(100000), 1+Random(100000));
    Copy(S1,1+Random(10), 1+Random(2));
  end;
  Showmessage('2,000,000 Copy in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton35Click(Sender: TObject);
Var S1: ansiString;
    i: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  S1 := ALRandomStrA(500);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(25), 1+Random(10));
    Copy(S1,1+Random(50), 1+Random(4));
    Copy(S1,1+Random(50), 1+Random(30));
    Copy(S1,1+Random(40), 1+Random(1));
  end;

  S1 := ALRandomStrA(2000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(60), 1+Random(100));
    Copy(S1,1+Random(100), 1+Random(15));
    Copy(S1,1+Random(150), 1+Random(200));
    Copy(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStrA(30000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(1000), 1+Random(1000));
    Copy(S1,1+Random(100), 1+Random(200));
    Copy(S1,1+Random(3000), 1+Random(3000));
    Copy(S1,1+Random(200), 1+Random(1));
  end;

  S1 := ALRandomStrA(40000);
  For i := 0 to 100000 do begin
    Copy(S1,1+Random(2000), 1+Random(1000));
    Copy(S1,1+Random(500), 1+Random(200));
    Copy(S1,1+Random(4000), 1+Random(4000));
    Copy(S1,1+Random(10), 1+Random(1));
  end;

  S1 := ALRandomStrA(1000000);
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

{**********************************************************************************************************************************************}
function GetProcessMemoryInfo(Process : THandle; var MemoryCounters : TProcessMemoryCounters; cb : DWORD) : BOOL; stdcall; external 'psapi.dll';

{****************************************************}
function ProcessMemoryUsage(ProcessID : DWORD): DWORD;
var ProcessHandle : THandle;
    MemCounters   : TProcessMemoryCounters;
begin
  Result := 0;
  ProcessHandle := OpenProcess(
                     PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
                     false,
                     ProcessID);
  try
    if GetProcessMemoryInfo(
         ProcessHandle,
         MemCounters,
         sizeof(MemCounters))
    then Result := MemCounters.WorkingSetSize;
  finally
    CloseHandle(ProcessHandle);
  end;
end;

{************************************************}
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

{************************************************}
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

{************************************************}
procedure TForm1.ALButton40Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    LFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
begin
  ALGetLocaleFormatSettings(1033, LFormatSettings);
  StartDate := Now;
  For i := 0 to 1000000 do begin
    S1 := ALFormatA('xxx %s xxxx %s xxxx', [ALRandomStrA(10), ALRandomStrA(20)], LFormatSettings);
    S1 := ALFormatA('xxx %*.*f xxx', [random(10), random(5), random(MaxInt) / random(MaxInt)], LFormatSettings);
    S1 := ALFormatA('xxx %12.2m', [random(MaxInt) / random(MaxInt)], LFormatSettings);
  end;
  Showmessage('3,000,000 ALFormatA in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton43Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    LFormatSettings: TformatSettings;
    S1: String;
begin
  GetLocaleFormatSettings(1033, LFormatSettings);
  StartDate := Now;
  For i := 0 to 1000000 do begin
    S1 := Format('xxx %s xxxx %s xxxx', [ALRandomStrW(10), ALRandomStrW(20)], LFormatSettings);
    S1 := Format('xxx %*.*f xxx', [random(10), random(5), random(MaxInt) / random(MaxInt)], LFormatSettings);
    S1 := Format('xxx %12.2m', [random(MaxInt) / random(MaxInt)], LFormatSettings);
  end;
  Showmessage('3,000,000 Format in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton45Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    LFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
    E1: Extended;
begin
  ALGetLocaleFormatSettings(1033, LFormatSettings);
  StartDate := Now;
  S1 := '121.23928322';
  For i := 0 to 10000000 do begin
    ALTryStrToFloat(S1, E1, LFormatSettings);
  end;
  Showmessage('10,000,000 ALTrystrFloat in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{************************************************}
procedure TForm1.ALButton46Click(Sender: TObject);
var i: integer;
    StartDate: TdateTime;
    LFormatSettings: TformatSettings;
    S1: String;
    E1: Extended;
begin
  GetLocaleFormatSettings(1033, LFormatSettings);
  StartDate := Now;
  S1 := '121.23928322';
  For i := 0 to 10000000 do begin
    TryStrToFloat(S1, E1, LFormatSettings);
  end;
  Showmessage('10,000,000 ALTrystrFloat in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
