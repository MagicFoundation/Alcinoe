unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  cxLabel, Shellapi;

type
  TForm1 = class(TForm)
    ALButton1: TButton;
    StatusBar1: TStatusBar;
    ALButton3: TButton;
    ALButton4: TButton;
    Label1: TLabel;
    EditKey: TEdit;
    ALMemoDecryptedData: TMemo;
    Label2: TLabel;
    ALMemoCryptedData: TMemo;
    Label3: TLabel;
    ALButton2: TButton;
    ALButton5: TButton;
    ALButton6: TButton;
    ALButton7: TButton;
    ALButton10: TButton;
    ALButton11: TButton;
    ALButton8: TButton;
    ALButton9: TButton;
    ALButton12: TButton;
    ALButton13: TButton;
    Panel7: TPanel;
    cxLabel1: TcxLabel;
    Label15: TcxLabel;
    cxLabel4: TcxLabel;
    cxLabel6: TcxLabel;
    cxWwwArkadiaComLabel: TcxLabel;
    procedure ALButton1Click(Sender: TObject);
    procedure ALButton3Click(Sender: TObject);
    procedure ALButton4Click(Sender: TObject);
    procedure ALButton2Click(Sender: TObject);
    procedure ALButton5Click(Sender: TObject);
    procedure ALButton6Click(Sender: TObject);
    procedure ALButton7Click(Sender: TObject);
    procedure ALButton10Click(Sender: TObject);
    procedure ALButton11Click(Sender: TObject);
    procedure ALButton8Click(Sender: TObject);
    procedure ALButton9Click(Sender: TObject);
    procedure ALButton12Click(Sender: TObject);
    procedure ALButton13Click(Sender: TObject);
    procedure cxWwwArkadiaComLabelClick(Sender: TObject);
  private
  public
  end;

var Form1: TForm1;

implementation

uses math,
     alMisc,
     AlAVLBinaryTree,
     ALString,
     alMime,
     alCipher;

{$R *.dfm}

{*******************************************************}
function internalRandomStr(aLength: Longint): AnsiString;
var X: Longint;
begin
  if aLength <= 0 then exit;
  SetLength(Result, aLength);
  for X:=1 to aLength do Result[X] := ansiChar(Random(256));
end;

{************************************************}
procedure TForm1.ALButton10Click(Sender: TObject);
Var Data: AnsiString;
    Counter: integer;
    StartTime: DWORD;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench MD5';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;
  Counter := 0;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  StartTime := GetTickCount;
  Data := ALRandomStr(250);
  while True do begin
    ALStringHashMD5(Data);
    inc(counter);
    if counter mod 1000 = 0 then begin
      StatusBar1.Panels[0].Text := 'MD5';
      StatusBar1.Panels[1].Text := IntToStr(round(counter / Max(1,((GetTickCount - StartTime) / 1000)))) + ' keys/s';
      StatusBar1.Panels[2].Text := 'Input: '+inttostr(length(Data))+' bytes';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

{***********************************************}
procedure TForm1.ALButton11Click(Sender: TObject);
Var Data: AnsiString;
    Counter: integer;
    StartTime: DWORD;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench SHA1';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;
  Counter := 0;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  StartTime := GetTickCount;
  Data := ALRandomStr(250);
  while True do begin
    ALStringHashSHA1(Data);
    inc(counter);
    if counter mod 1000 = 0 then begin
      StatusBar1.Panels[0].Text := 'SHA1';
      StatusBar1.Panels[1].Text := IntToStr(round(counter / Max(1,((GetTickCount - StartTime) / 1000)))) + ' keys/s';
      StatusBar1.Panels[2].Text := 'Input: '+inttostr(length(Data))+' bytes';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

{************************************************}
procedure TForm1.ALButton12Click(Sender: TObject);
begin
  ALMemoCryptedData.Lines.Text := String(ALCalcHMACSHA1(AnsiString(ALMemoDecryptedData.Lines.Text), AnsiString(EditKey.Text)));
end;

{************************************************}
procedure TForm1.ALButton13Click(Sender: TObject);
begin
  ALMemoCryptedData.Lines.Text := String(ALCalcHMACMD5(AnsiString(ALMemoDecryptedData.Lines.Text), AnsiString(EditKey.Text)));
end;

{***********************************************}
procedure TForm1.ALButton1Click(Sender: TObject);
Var Data: AnsiString;
    Key: AnsiString;
    Counter: integer;
    StartTime: DWORD;
    Str1, Str2: AnsiString;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench AES (EBC)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;
  Counter := 0;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  StartTime := GetTickCount;
  while True do begin
    Data := ALRandomStr(250);
    Key := AlRandomStr(40);
    ALRDLEncryptString(Data, Str1, Key, True);
    ALRDLEncryptString(Str1, Str2, Key, false);
    if str2 <> Data then Raise Exception.Create('!!Abnormal Error!!');
    inc(counter);
    if counter mod 1000 = 0 then begin
      StatusBar1.Panels[0].Text := 'AES (EBC)';
      StatusBar1.Panels[1].Text := IntToStr(round(counter / Max(1,((GetTickCount - StartTime) / 1000)))) + ' encrypts & decrypts /s';
      StatusBar1.Panels[2].Text := 'Input: '+inttostr(length(Data))+' bytes - Key: '+inttostr(length(Key))+' bytes';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

{***********************************************}
procedure TForm1.ALButton3Click(Sender: TObject);
Var Data: AnsiString;
    Key: AnsiString;
    Counter: integer;
    StartTime: DWORD;
    Str1, Str2: AnsiString;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench AES (CBC)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;
  Counter := 0;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  StartTime := GetTickCount;
  while True do begin
    Data := ALRandomStr(250);
    Key := AlRandomStr(40);
    ALRDLEncryptStringCBC(Data, Str1, Key, True);
    ALRDLEncryptStringCBC(Str1, Str2, Key, false);
    if str2 <> Data then Raise Exception.Create('!!Abnormal Error!!');
    inc(counter);
    if counter mod 1000 = 0 then begin
      StatusBar1.Panels[0].Text := 'AES (CBC)';
      StatusBar1.Panels[1].Text := IntToStr(round(counter / Max(1,((GetTickCount - StartTime) / 1000)))) + ' encrypts & decrypts /s';
      StatusBar1.Panels[2].Text := 'Input: '+inttostr(length(Data))+' bytes - Key: '+inttostr(length(Key))+' bytes';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

{***********************************************}
procedure TForm1.ALButton4Click(Sender: TObject);
Var Data: AnsiString;
    Key: AnsiString;
    Counter: integer;
    StartTime: DWORD;
    Str1, Str2: AnsiString;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench Blowfish (EBC)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;
  Counter := 0;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  StartTime := GetTickCount;
  while True do begin
    Data := ALRandomStr(250);
    Key := AlRandomStr(40);
    ALBFEncryptString(Data, Str1, Key, True);
    ALBFEncryptString(Str1, Str2, Key, false);
    if str2 <> Data then Raise Exception.Create('!!Abnormal Error!!');
    inc(counter);
    if counter mod 1000 = 0 then begin
      StatusBar1.Panels[0].Text := 'Blowfish (EBC)';
      StatusBar1.Panels[1].Text := IntToStr(round(counter / Max(1,((GetTickCount - StartTime) / 1000)))) + ' encrypts & decrypts /s';
      StatusBar1.Panels[2].Text := 'Input: '+inttostr(length(Data))+' bytes - Key: '+inttostr(length(Key))+' bytes';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

{***********************************************}
procedure TForm1.ALButton5Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALBFEncryptStringCBC(ALMimeBase64DecodeString(ALTrim(AnsiString(ALMemocryptedData.Lines.Text))), outString, AnsiString(EditKey.Text), False);
  ALMemoDeCryptedData.Lines.Text := String(outString);
end;

{***********************************************}
procedure TForm1.ALButton6Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptString(AnsiString(ALMemoDecryptedData.Lines.Text), outString, AnsiString(EditKey.Text), True);
  ALMemoCryptedData.Lines.Text := String(ALMimeBase64EncodeStringNoCRLF(outString));
end;

{***********************************************}
procedure TForm1.ALButton7Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptString(ALMimeBase64DecodeString(ALTrim(ansiString(ALMemocryptedData.Lines.Text))), outString, ansiString(EditKey.Text), False);
  ALMemoDeCryptedData.Lines.Text := string(outString);
end;

{***********************************************}
procedure TForm1.ALButton8Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptStringCBC(AnsiString(ALMemoDecryptedData.Lines.Text), outString, AnsiString(EditKey.Text), True);
  ALMemoCryptedData.Lines.Text := String(ALMimeBase64EncodeStringNoCRLF(outString));
end;

{***********************************************}
procedure TForm1.ALButton9Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptStringCBC(ALMimeBase64DecodeString(ALTrim(ansiString(ALMemocryptedData.Lines.Text))), outString, ansiString(EditKey.Text), False);
  ALMemoDeCryptedData.Lines.Text := string(outString);
end;

{**********************************************************}
procedure TForm1.cxWwwArkadiaComLabelClick(Sender: TObject);
begin
  ShellExecute(Application.Handle,'open','http://www.arkadia.com',nil,nil, SW_SHOWNORMAL);
end;

{***********************************************}
procedure TForm1.ALButton2Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALBFEncryptStringCBC(AnsiString(ALMemoDecryptedData.Lines.Text), outString, AnsiString(EditKey.Text), True);
  ALMemoCryptedData.Lines.Text := String(ALMimeBase64EncodeStringNoCRLF(outString));
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
