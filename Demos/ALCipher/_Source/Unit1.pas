unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Shellapi,
  System.Generics.Defaults, diagnostics, System.Generics.Collections, dateutils;

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
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ALMemoCollisions: TMemo;
    Label4: TLabel;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    PaintBox1: TPaintBox;
    Button8: TButton;
    Button10: TButton;
    Button11: TButton;
    Button13: TButton;
    Button15: TButton;
    Button12: TButton;
    Button16: TButton;
    Button14: TButton;
    Button17: TButton;
    Button18: TButton;
    Button9: TButton;
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
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
  public
  end;

var Form1: TForm1;

implementation

uses
  math,
  Alcinoe.Common,
  system.hash,
  system.netencoding,
  system.uitypes,
  Alcinoe.AVLBinaryTree,
  Alcinoe.StringUtils,
  Alcinoe.Files,
  Alcinoe.Cipher,
  ZlibExApi,
  ZlibEx;

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
Var LData: AnsiString;
    LTmpData: ansiString;
    LHash: ansiString;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<ansiString,ansistring>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALMD5 (ansiString)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  if ALMemoDecryptedData.Lines.Text = '' then ALMemoDecryptedData.Lines.Text := '&"''(-_)=$*%!:;,';
  ALMemoCryptedData.Lines.Text := string(ALStringHashMD5(ansiString(ALMemoDecryptedData.Lines.Text), true{hexEncode}));

  StatusBar1.Panels[0].Text := 'ALMD5 (ansiString)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<ansiString,ansistring>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrA(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ALStringHashMD5(LData, true{hexEncode});
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;

{************************************************}
procedure TForm1.ALButton11Click(Sender: TObject);
Var LData: AnsiString;
    LTmpData: ansiString;
    LHash: ansiString;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<ansiString,ansistring>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALSHA1 (ansiString)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  if ALMemoDecryptedData.Lines.Text = '' then ALMemoDecryptedData.Lines.Text := '&"''(-_)=$*%!:;,';
  ALMemoCryptedData.Lines.Text := string(ALStringHashSHA1(ansiString(ALMemoDecryptedData.Lines.Text), true{hexEncode}));

  StatusBar1.Panels[0].Text := 'ALSHA1 (ansiString)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<ansiString,ansistring>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrA(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ALStringHashSHA1(LData, true{hexEncode});
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
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
Var LData: AnsiString;
    LKey: AnsiString;
    LCounter: integer;
    LStartTime: DWORD;
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

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  LCounter := 0;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  LStartTime := GetTickCount;
  while True do begin
    LData := ALRandomStrA(250);
    LKey := ALRandomStrA(40);
    ALRDLEncryptStringA(LData, Str1, LKey, TALkeyDerivationFunction.SHA2, True);
    ALRDLEncryptStringA(Str1, Str2, LKey, TALkeyDerivationFunction.SHA2, false);
    if str2 <> LData then Raise Exception.Create('!!Abnormal Error!!');
    inc(LCounter);
    if LCounter mod 1000 = 0 then begin
      StatusBar1.Panels[0].Text := 'AES (EBC)';
      StatusBar1.Panels[1].Text := IntToStr(round(LCounter / Max(1,((GetTickCount - LStartTime) / 1000)))) + ' encrypts & decrypts /s';
      StatusBar1.Panels[2].Text := 'Input: '+inttostr(length(LData))+' bytes - Key: '+inttostr(length(LKey))+' bytes';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

{***********************************************}
procedure TForm1.ALButton3Click(Sender: TObject);
Var LData: AnsiString;
    LKey: AnsiString;
    LCounter: integer;
    LStartTime: DWORD;
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

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  LCounter := 0;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  LStartTime := GetTickCount;
  while True do begin
    LData := ALRandomStrA(250);
    LKey := ALRandomStrA(40);
    ALRDLEncryptStringCBCA(LData, Str1, LKey, TALkeyDerivationFunction.SHA2, True);
    ALRDLEncryptStringCBCA(Str1, Str2, LKey, TALkeyDerivationFunction.SHA2, false);
    if str2 <> LData then Raise Exception.Create('!!Abnormal Error!!');
    inc(LCounter);
    if LCounter mod 1000 = 0 then begin
      StatusBar1.Panels[0].Text := 'AES (CBC)';
      StatusBar1.Panels[1].Text := IntToStr(round(LCounter / Max(1,((GetTickCount - LStartTime) / 1000)))) + ' encrypts & decrypts /s';
      StatusBar1.Panels[2].Text := 'Input: '+inttostr(length(LData))+' bytes - Key: '+inttostr(length(LKey))+' bytes';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

{***********************************************}
procedure TForm1.ALButton4Click(Sender: TObject);
Var LData: AnsiString;
    LKey: AnsiString;
    LCounter: integer;
    LStartTime: DWORD;
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

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  LCounter := 0;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  LStartTime := GetTickCount;
  while True do begin
    LData := ALRandomStrA(250);
    LKey := ALRandomStrA(40);
    ALBFEncryptString(LData, Str1, LKey, True);
    ALBFEncryptString(Str1, Str2, LKey, false);
    if str2 <> LData then Raise Exception.Create('!!Abnormal Error!!');
    inc(LCounter);
    if LCounter mod 1000 = 0 then begin
      StatusBar1.Panels[0].Text := 'Blowfish (EBC)';
      StatusBar1.Panels[1].Text := IntToStr(round(LCounter / Max(1,((GetTickCount - LStartTime) / 1000)))) + ' encrypts & decrypts /s';
      StatusBar1.Panels[2].Text := 'Input: '+inttostr(length(LData))+' bytes - Key: '+inttostr(length(LKey))+' bytes';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

{***********************************************}
procedure TForm1.ALButton5Click(Sender: TObject);
Var LOutString: AnsiString;
begin
  ALBFEncryptStringCBC(ALBase64DecodeString(ALTrim(AnsiString(ALMemocryptedData.Lines.Text))), LOutString, AnsiString(EditKey.Text), False);
  ALMemoDeCryptedData.Lines.Text := String(LOutString);
end;

{***********************************************}
procedure TForm1.ALButton6Click(Sender: TObject);
Var LOutString: AnsiString;
begin
  ALRDLEncryptStringA(AnsiString(ALMemoDecryptedData.Lines.Text), LOutString, AnsiString(EditKey.Text), TALkeyDerivationFunction.SHA2, True);
  ALMemoCryptedData.Lines.Text := String(ALBase64EncodeString(LOutString));
end;

{***********************************************}
procedure TForm1.ALButton7Click(Sender: TObject);
Var LOutString: AnsiString;
begin
  ALRDLEncryptStringA(ALBase64DecodeString(ALTrim(ansiString(ALMemocryptedData.Lines.Text))), LOutString, ansiString(EditKey.Text), TALkeyDerivationFunction.SHA2, False);
  ALMemoDeCryptedData.Lines.Text := string(LOutString);
end;

{***********************************************}
procedure TForm1.ALButton8Click(Sender: TObject);
Var LOutString: AnsiString;
begin
  ALRDLEncryptStringCBCA(AnsiString(ALMemoDecryptedData.Lines.Text), LOutString, AnsiString(EditKey.Text), TALkeyDerivationFunction.SHA2, True);
  ALMemoCryptedData.Lines.Text := String(ALBase64EncodeString(LOutString));
end;

{***********************************************}
procedure TForm1.ALButton9Click(Sender: TObject);
Var LOutString: AnsiString;
begin
  ALRDLEncryptStringCBCA(ALBase64DecodeString(ALTrim(ansiString(ALMemocryptedData.Lines.Text))), LOutString, ansiString(EditKey.Text), TALkeyDerivationFunction.SHA2, False);
  ALMemoDeCryptedData.Lines.Text := string(LOutString);
end;

{**********************************************}
procedure TForm1.Button10Click(Sender: TObject);
Var LData: AnsiString;
    LBase64Data: AnsiString;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Test/Bench ALBase64Encode (ansiString)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  if ALMemoDecryptedData.Lines.Text = '' then ALMemoDecryptedData.Lines.Text := '&"''(-_)=$*%!:;,';
  ALMemoCryptedData.Lines.Text := string(ALBase64EncodeString(ansiString(ALMemoDecryptedData.Lines.Text)));
  ALMemoDecryptedData.Lines.Text := string(ALBase64DecodeString(ansiString(ALMemoCryptedData.Lines.Text)));

  StatusBar1.Panels[0].Text := 'Test/Bench ALBase64Encode (ansiString)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 4096 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LCounter := 0;
  LLastGUIUpdate := now;
  LStopWatch := TstopWatch.Create;
  while True do begin
    LData := ALRandomByteStr(random(4096));
    LStopWatch.Start;
    LBase64Data := ALBase64EncodeString(LData);
    if ALBase64DecodeString(LBase64Data) <> LData then
      raise Exception.Create('Test failed!');
    LStopWatch.Stop;
    inc(LCounter);
    if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
      LLastGUIUpdate := now;
      StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
      StatusBar1.Panels[3].Text := '';
      StatusBar1.Panels[4].Text := '';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

{**********************************************}
procedure TForm1.Button11Click(Sender: TObject);
Var LData: AnsiString;
    LHash: ansiString;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench HexEncode (ansiString)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  if ALMemoDecryptedData.Lines.Text = '' then ALMemoDecryptedData.Lines.Text := '&"''(-_)=$*%!:;,';
  ALMemoCryptedData.Lines.Text := string(ALBinToHexA(ansiString(ALMemoDecryptedData.Lines.Text)));
  ALMemoDecryptedData.Lines.Text := string(ALHexToBin(ansiString(ALMemoCryptedData.Lines.Text)));

  StatusBar1.Panels[0].Text := 'Bench HexEncode (ansiString)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 4096 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LCounter := 0;
  LLastGUIUpdate := now;
  LStopWatch := TstopWatch.Create;
  while True do begin
    LData := ALRandomByteStr(1+random(4096));
    LStopWatch.Start;
    LHash := ALBinToHexA(LData);
    LStopWatch.Stop;
    if ALHexToBin(LHash) <> LData then
      raise Exception.Create('Test failed!');
    inc(LCounter);
    if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
      LLastGUIUpdate := now;
      StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
      StatusBar1.Panels[3].Text := '';
      StatusBar1.Panels[4].Text := '';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

{**********************************************}
procedure TForm1.Button12Click(Sender: TObject);
Var LData: ansiString;
    LTmpData: ansiString;
    LHash: ansiString;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<ansiString,ansistring>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALSHA2 256 (ansiString)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  if ALMemoDecryptedData.Lines.Text = '' then ALMemoDecryptedData.Lines.Text := '&"''(-_)=$*%!:;,';
  ALMemoCryptedData.Lines.Text := string(ALStringHashSHA2(ansistring(ALMemoDecryptedData.Lines.Text), THashSHA2.TSHA2Version.SHA256, true{hexEncode}));

  StatusBar1.Panels[0].Text := 'ALSHA2 256 (ansiString)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<ansiString,ansistring>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrA(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ALStringHashSHA2(LData, THashSHA2.TSHA2Version.SHA256, true{hexEncode});
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;

{**********************************************}
procedure TForm1.Button13Click(Sender: TObject);
Var LData: String;
    LTmpData: String;
    LHash: String;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<String,string>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALMD5 (Unicode)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  if ALMemoDecryptedData.Lines.Text = '' then ALMemoDecryptedData.Lines.Text := '&"''(-_)=$*%!:;,';
  ALMemoCryptedData.Lines.Text := ALStringHashMD5(ALMemoDecryptedData.Lines.Text, Tencoding.UTF8);

  StatusBar1.Panels[0].Text := 'ALMD5 (Unicode)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<String,string>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrW(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ALStringHashMD5(LData, Tencoding.UTF8);
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;

{**********************************************}
procedure TForm1.Button14Click(Sender: TObject);
Var LData: String;
    LTmpData: String;
    LHash: String;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<String,string>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALSHA1 (Unicode)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  if ALMemoDecryptedData.Lines.Text = '' then ALMemoDecryptedData.Lines.Text := '&"''(-_)=$*%!:;,';
  ALMemoCryptedData.Lines.Text := ALStringHashSHA1(ALMemoDecryptedData.Lines.Text, Tencoding.UTF8);

  StatusBar1.Panels[0].Text := 'ALSHA1 (Unicode)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<String,string>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrW(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ALStringHashSHA1(LData, Tencoding.UTF8);
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;

{**********************************************}
procedure TForm1.Button15Click(Sender: TObject);
Var LData: String;
    LBase64Data: String;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Test/Bench ALBase64EncodeU (UNICODE)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  if ALMemoDecryptedData.Lines.Text = '' then ALMemoDecryptedData.Lines.Text := '&"''(-_)=$*%!:;,';
  ALMemoCryptedData.Lines.Text := ALBase64EncodeString(ALMemoDecryptedData.Lines.Text, Tencoding.UTF8);
  ALMemoDecryptedData.Lines.Text := ALBase64DecodeString(ALMemoCryptedData.Lines.Text, Tencoding.UTF8);

  StatusBar1.Panels[0].Text := 'Test/Bench ALBase64EncodeU (UNICODE)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 4096 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LCounter := 0;
  LLastGUIUpdate := now;
  LStopWatch := TstopWatch.Create;
  while True do begin
    LData := ALRandomStrW(random(4096));
    LStopWatch.Start;
    LBase64Data := ALBase64EncodeString(LData, Tencoding.UTF8);
    if ALBase64DecodeString(LBase64Data, Tencoding.UTF8) <> LData then
      raise Exception.Create('Test failed!');
    LStopWatch.Stop;
    inc(LCounter);
    if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
      LLastGUIUpdate := now;
      StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
      StatusBar1.Panels[3].Text := '';
      StatusBar1.Panels[4].Text := '';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

{**********************************************}
procedure TForm1.Button16Click(Sender: TObject);
Var LData: ansiString;
    LTmpData: ansiString;
    LHash: ansiString;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<ansiString,ansistring>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALSHA2 512 (ansiString)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  if ALMemoDecryptedData.Lines.Text = '' then ALMemoDecryptedData.Lines.Text := '&"''(-_)=$*%!:;,';
  ALMemoCryptedData.Lines.Text := string(ALStringHashSHA2(ansistring(ALMemoDecryptedData.Lines.Text), THashSHA2.TSHA2Version.SHA512, true{hexEncode}));

  StatusBar1.Panels[0].Text := 'ALSHA2 512 (ansiString)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<ansiString,ansistring>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrA(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ALStringHashSHA2(LData, THashSHA2.TSHA2Version.SHA512, true{hexEncode});
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;

{**********************************************}
procedure TForm1.Button17Click(Sender: TObject);
Var LData: String;
    LTmpData: String;
    LHash: String;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<String,string>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALSHA2 256 (Unicode)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  if ALMemoDecryptedData.Lines.Text = '' then ALMemoDecryptedData.Lines.Text := '&"''(-_)=$*%!:;,';
  ALMemoCryptedData.Lines.Text := ALStringHashSHA2(ALMemoDecryptedData.Lines.Text, Tencoding.UTF8, THashSHA2.TSHA2Version.SHA256);

  StatusBar1.Panels[0].Text := 'ALSHA2 256 (Unicode)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<String,string>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrW(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ALStringHashSHA2(LData, Tencoding.UTF8, THashSHA2.TSHA2Version.SHA256);
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;

{**********************************************}
procedure TForm1.Button18Click(Sender: TObject);
Var LData: Tbytes;
    LData2: Tbytes;
    LHash: String;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench HexEncode (UNICODE)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  if ALMemoDecryptedData.Lines.Text = '' then ALMemoDecryptedData.Lines.Text := '&"''(-_)=$*%!:;,';
  ALMemoCryptedData.Lines.Text := ALBinToHexW(Tencoding.UTF8.GetBytes(ALMemoDecryptedData.Lines.Text));
  ALMemoDecryptedData.Lines.Text := Tencoding.UTF8.GetString(ALHexToBin(ALMemoCryptedData.Lines.Text));

  StatusBar1.Panels[0].Text := 'Bench HexEncode (UNICODE)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 4096 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LCounter := 0;
  LLastGUIUpdate := now;
  LStopWatch := TstopWatch.Create;
  while True do begin
    LData := ALRandomBytes(1+random(4096));
    LStopWatch.Start;
    LHash := ALBinToHexW(LData);
    LStopWatch.Stop;
    LData2 := ALHexToBin(LHash);
    if (length(LData2) <> length(LData)) or
       (not comparemem(@LData2[0], @LData[0], length(LData2))) then
      raise Exception.Create('Test failed!');
    inc(LCounter);
    if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
      LLastGUIUpdate := now;
      StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
      StatusBar1.Panels[3].Text := '';
      StatusBar1.Panels[4].Text := '';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

{*********************************************}
procedure TForm1.Button1Click(Sender: TObject);
Var LData: AnsiString;
    LTmpData: ansiString;
    LHash: integer;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<integer,ansistring>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench CRC32 (Zlib)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  StatusBar1.Panels[0].Text := 'CRC32 (Zlib)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<integer,ansistring>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrA(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ZCrc32(0, LData[1], length(LData));
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;

{*********************************************}
procedure TForm1.Button2Click(Sender: TObject);
Var LData: AnsiString;
    LTmpData: ansiString;
    LHash: cardinal;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<cardinal,ansistring>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALCRC32';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  StatusBar1.Panels[0].Text := 'ALCRC32';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<cardinal,ansistring>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrA(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ALStringHashCrc32c(LData);
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;

{*********************************************}
procedure TForm1.Button9Click(Sender: TObject);
Var LData: AnsiString;
    LTmpData: ansiString;
    LHash: int64;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<int64,ansistring>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALCRC64';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  StatusBar1.Panels[0].Text := 'ALCRC64';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<int64,ansistring>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrA(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ALStringHashCrc64c(LData);
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;


{***************************}
{$WARN SYMBOL_DEPRECATED OFF}
procedure TForm1.Button3Click(Sender: TObject);
Var LData: AnsiString;
    LTmpData: ansiString;
    LHash: integer;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<integer,ansistring>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench BobJenkinsHash';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  StatusBar1.Panels[0].Text := 'BobJenkinsHash';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<integer,ansistring>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrA(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := BobJenkinsHash(LData[1], Length(LData) * SizeOf(LData[1]), 0);
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;

{*********************************************}
procedure TForm1.Button4Click(Sender: TObject);
Var LData: AnsiString;
    LTmpData: ansiString;
    LHash: int64;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<int64,ansistring>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALFNV-1a (int64)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  StatusBar1.Panels[0].Text := 'ALFNV-1a (int64)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<int64,ansistring>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrA(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ALFnv1aInt64(LData);
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;

{*********************************************}
procedure TForm1.Button5Click(Sender: TObject);
Var LData: AnsiString;
    LTmpData: ansiString;
    LHash: int32;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<int32,ansistring>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALFNV-1a (int32)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  StatusBar1.Panels[0].Text := 'ALFNV-1a (int32)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<int32,ansistring>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrA(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ALFnv1aInt32(LData);
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;

{*********************************************}
procedure TForm1.Button6Click(Sender: TObject);
Var LHash: int32;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    P, X, Y: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALRandom32';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  StatusBar1.Panels[0].Text := 'ALRandom32';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LCounter := 0;
  LLastGUIUpdate := now;
  LStopWatch := TstopWatch.Create;
  EditKey.Visible := False;
  ALMemoDecryptedData.Visible := False;
  ALMemoCryptedData.Visible := False;
  ALMemoCollisions.Visible := False;
  Label4.Visible := False;
  Label2.Visible := False;
  Label1.Visible := False;
  Label3.Visible := False;
  PaintBox1.Visible := True;
  application.ProcessMessages;
  try
    while True do begin
      LStopWatch.Start;
      LHash := ALRandom32(maxint);
      LStopWatch.Stop;
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter)) + ' keys - ' + FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
      P := round(((PaintBox1.Width * PaintBox1.Height) / maxint) * LHash);
      Y := P div PaintBox1.Width;
      X := P mod PaintBox1.Width;
      PaintBox1.Canvas.Pixels[X,Y] := $0000ff;
    end;
  finally
    EditKey.Visible := true;
    ALMemoDecryptedData.Visible := true;
    ALMemoCryptedData.Visible := true;
    ALMemoCollisions.Visible := true;
    Label4.Visible := true;
    Label2.Visible := true;
    Label1.Visible := true;
    Label3.Visible := true;
    PaintBox1.Visible := False;
  end;
end;

{***************************************************************}
Function  Win_MixID(aIDBroker: int32; aIDConfined: int32): Int64;
Begin
  if aIDConfined <> 0 then Result := (int64(aIDBroker) shl 32) or ((int64(aIDConfined) shl 32) shr 32)
  else result := aIDBroker; // this to be able to store the result on a 32 bit value
End;

{*********************************************}
procedure TForm1.Button7Click(Sender: TObject);
Var LHash: int64;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    P, X, Y: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench ALRandom64';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  StatusBar1.Panels[0].Text := 'ALRandom64';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LCounter := 0;
  LLastGUIUpdate := now;
  LStopWatch := TstopWatch.Create;
  EditKey.Visible := False;
  ALMemoDecryptedData.Visible := False;
  ALMemoCryptedData.Visible := False;
  ALMemoCollisions.Visible := False;
  Label4.Visible := False;
  Label2.Visible := False;
  Label1.Visible := False;
  Label3.Visible := False;
  PaintBox1.Visible := True;
  application.ProcessMessages;
  try
    while True do begin
      LStopWatch.Start;
      LHash := ALRandom64(ALMaxint64);
      LStopWatch.Stop;
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter)) + ' keys - ' + FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
      P := round(((PaintBox1.Width * PaintBox1.Height) / ALMaxint64) * LHash);
      Y := P div PaintBox1.Width;
      X := P mod PaintBox1.Width;
      PaintBox1.Canvas.Pixels[X,Y] := $0000ff;
    end;
  finally
    EditKey.Visible := true;
    ALMemoDecryptedData.Visible := true;
    ALMemoCryptedData.Visible := true;
    ALMemoCollisions.Visible := true;
    Label4.Visible := true;
    Label2.Visible := true;
    Label1.Visible := true;
    Label3.Visible := true;
    PaintBox1.Visible := False;
  end;
end;

{*********************************************}
procedure TForm1.Button8Click(Sender: TObject);
Var LData: AnsiString;
    LTmpData: ansiString;
    LHash: ansiString;
    LCounter: integer;
    LStopWatch: TstopWatch;
    LLastGUIUpdate: TDateTime;
    LDictionary: TDictionary<ansiString,ansistring>;
    LCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench BCrypt(12) (ansiString)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  {$IFDEF DEBUG}
  if not ALBCryptSelfTest then raise Exception.Create('ALBCryptSelfTest Failed!');
  {$ENDIF}
  randomize;

  if ALMemoDecryptedData.Lines.Text = '' then ALMemoDecryptedData.Lines.Text := '&"''(-_)=$*%!:;,';
  ALMemoCryptedData.Lines.Text := String(ALBCryptHashPassword(ansiString(ALMemoDecryptedData.Lines.Text), 12));

  StatusBar1.Panels[0].Text := 'Bcrypt(12) (ansiString)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  LDictionary := TDictionary<ansiString,ansistring>.create;
  try
    LCounter := 0;
    LCollision := 0;
    LLastGUIUpdate := now;
    LStopWatch := TstopWatch.Create;
    while True do begin
      LData := ALRandomStrA(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      LStopWatch.Start;
      LHash := ALBCryptHashPassword(LData, 12);
      LStopWatch.Stop;
      if LDictionary.TryGetValue(LHash, LTmpData) then begin
        if LTmpData <> LData then begin
          ALMemoCollisions.Lines.Add(string(LTmpData + '  =  ' + LData));
          inc(LCollision);
        end;
      end
      else LDictionary.Add(LHash, LData);
      inc(LCounter);
      if millisecondsbetween(now, LLastGUIUpdate) > 200 then begin
        LLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (LCounter / (LStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((LStopWatch.Elapsed.TotalMilliseconds / LCounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(LCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', LDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    LDictionary.free;
  end;
end;

{$WARN SYMBOL_DEPRECATED ON}

{***********************************************}
procedure TForm1.ALButton2Click(Sender: TObject);
Var LOutString: AnsiString;
begin
  ALBFEncryptStringCBC(AnsiString(ALMemoDecryptedData.Lines.Text), LOutString, AnsiString(EditKey.Text), True);
  ALMemoCryptedData.Lines.Text := String(ALBase64EncodeString(LOutString));
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
