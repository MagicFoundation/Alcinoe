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
    Button19: TButton;
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
    procedure ALMemoCollisionsChange(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
  private
  public
  end;

var Form1: TForm1;

implementation

uses math,
     alcommon,
     system.hash,
     system.netencoding,
     system.uitypes,
     AlAVLBinaryTree,
     ALString,
     ALFiles,
     alCipher,
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
Var aData: AnsiString;
    aTmpData: ansiString;
    aHash: ansiString;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<ansiString,ansistring>;
    aCollision: integer;
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
  aDictionary := TDictionary<ansiString,ansistring>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStr(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ALStringHashMD5(aData, true{hexEncode});
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;

{***********************************************}
procedure TForm1.ALButton11Click(Sender: TObject);
Var aData: AnsiString;
    aTmpData: ansiString;
    aHash: ansiString;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<ansiString,ansistring>;
    aCollision: integer;
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
  aDictionary := TDictionary<ansiString,ansistring>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStr(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ALStringHashSHA1(aData, true{hexEncode});
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
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

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  Counter := 0;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  StartTime := GetTickCount;
  while True do begin
    Data := ALRandomStr(250);
    Key := AlRandomStr(40);
    ALRDLEncryptString(Data, Str1, Key, SHA2, True);
    ALRDLEncryptString(Str1, Str2, Key, SHA2, false);
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

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

  Counter := 0;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  StartTime := GetTickCount;
  while True do begin
    Data := ALRandomStr(250);
    Key := AlRandomStr(40);
    ALRDLEncryptStringCBC(Data, Str1, Key, SHA2, True);
    ALRDLEncryptStringCBC(Str1, Str2, Key, SHA2, false);
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

  ALMemoCryptedData.Lines.Text := '';
  ALMemoDecryptedData.Lines.Text := '';

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
  ALBFEncryptStringCBC(ALBase64DecodeString(ALTrim(AnsiString(ALMemocryptedData.Lines.Text))), outString, AnsiString(EditKey.Text), False);
  ALMemoDeCryptedData.Lines.Text := String(outString);
end;

{***********************************************}
procedure TForm1.ALButton6Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptString(AnsiString(ALMemoDecryptedData.Lines.Text), outString, AnsiString(EditKey.Text), SHA2, True);
  ALMemoCryptedData.Lines.Text := String(ALBase64EncodeString(outString));
end;

{***********************************************}
procedure TForm1.ALButton7Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptString(ALBase64DecodeString(ALTrim(ansiString(ALMemocryptedData.Lines.Text))), outString, ansiString(EditKey.Text), SHA2, False);
  ALMemoDeCryptedData.Lines.Text := string(outString);
end;

{***********************************************}
procedure TForm1.ALButton8Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptStringCBC(AnsiString(ALMemoDecryptedData.Lines.Text), outString, AnsiString(EditKey.Text), SHA2, True);
  ALMemoCryptedData.Lines.Text := String(ALBase64EncodeString(outString));
end;

{***********************************************}
procedure TForm1.ALButton9Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptStringCBC(ALBase64DecodeString(ALTrim(ansiString(ALMemocryptedData.Lines.Text))), outString, ansiString(EditKey.Text), SHA2, False);
  ALMemoDeCryptedData.Lines.Text := string(outString);
end;

procedure TForm1.ALMemoCollisionsChange(Sender: TObject);
begin

end;

{*********************************************}
procedure TForm1.Button10Click(Sender: TObject);
Var aData: AnsiString;
    aBase64Data: AnsiString;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
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
  aCounter := 0;
  aLastGUIUpdate := now;
  aStopWatch := TstopWatch.Create;
  while True do begin
    aData := ALRandomByteStr(random(4096));
    aStopWatch.Start;
    aBase64Data := ALBase64EncodeString(aData);
    if ALBase64DecodeString(aBase64Data) <> aData then
      raise Exception.Create('Test failed!');
    aStopWatch.Stop;
    inc(acounter);
    if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
      aLastGUIUpdate := now;
      StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
      StatusBar1.Panels[3].Text := '';
      StatusBar1.Panels[4].Text := '';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

procedure TForm1.Button11Click(Sender: TObject);
Var aData: AnsiString;
    aHash: ansiString;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
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
  ALMemoCryptedData.Lines.Text := string(ALBinToHex(ansiString(ALMemoDecryptedData.Lines.Text)));
  ALMemoDecryptedData.Lines.Text := string(ALHexToBin(ansiString(ALMemoCryptedData.Lines.Text)));

  StatusBar1.Panels[0].Text := 'Bench HexEncode (ansiString)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 4096 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  aCounter := 0;
  aLastGUIUpdate := now;
  aStopWatch := TstopWatch.Create;
  while True do begin
    aData := ALRandomByteStr(1+random(4096));
    aStopWatch.Start;
    aHash := ALBinToHex(aData);
    aStopWatch.Stop;
    if ALHexToBin(aHash) <> aData then
      raise Exception.Create('Test failed!');
    inc(acounter);
    if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
      aLastGUIUpdate := now;
      StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
      StatusBar1.Panels[3].Text := '';
      StatusBar1.Panels[4].Text := '';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

procedure TForm1.Button12Click(Sender: TObject);
Var aData: ansiString;
    aTmpData: ansiString;
    aHash: ansiString;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<ansiString,ansistring>;
    aCollision: integer;
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
  aDictionary := TDictionary<ansiString,ansistring>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStr(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ALStringHashSHA2(aData, THashSHA2.TSHA2Version.SHA256, true{hexEncode});
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;

procedure TForm1.Button13Click(Sender: TObject);
Var aData: String;
    aTmpData: String;
    aHash: String;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<String,string>;
    aCollision: integer;
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
  ALMemoCryptedData.Lines.Text := ALStringHashMD5U(ALMemoDecryptedData.Lines.Text, Tencoding.UTF8);

  StatusBar1.Panels[0].Text := 'ALMD5 (Unicode)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  aDictionary := TDictionary<String,string>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStrU(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ALStringHashMD5U(aData, Tencoding.UTF8);
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;

procedure TForm1.Button14Click(Sender: TObject);
Var aData: String;
    aTmpData: String;
    aHash: String;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<String,string>;
    aCollision: integer;
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
  ALMemoCryptedData.Lines.Text := ALStringHashSHA1U(ALMemoDecryptedData.Lines.Text, Tencoding.UTF8);

  StatusBar1.Panels[0].Text := 'ALSHA1 (Unicode)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  aDictionary := TDictionary<String,string>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStrU(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ALStringHashSHA1U(aData, Tencoding.UTF8);
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;

procedure TForm1.Button15Click(Sender: TObject);
Var aData: String;
    aBase64Data: String;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
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
  ALMemoCryptedData.Lines.Text := ALBase64EncodeStringU(ALMemoDecryptedData.Lines.Text, Tencoding.UTF8);
  ALMemoDecryptedData.Lines.Text := ALBase64DecodeStringU(ALMemoCryptedData.Lines.Text, Tencoding.UTF8);

  StatusBar1.Panels[0].Text := 'Test/Bench ALBase64EncodeU (UNICODE)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 4096 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  aCounter := 0;
  aLastGUIUpdate := now;
  aStopWatch := TstopWatch.Create;
  while True do begin
    aData := ALRandomStrU(random(4096));
    aStopWatch.Start;
    aBase64Data := ALBase64EncodeStringU(aData, Tencoding.UTF8);
    if ALBase64DecodeStringU(aBase64Data, Tencoding.UTF8) <> aData then
      raise Exception.Create('Test failed!');
    aStopWatch.Stop;
    inc(acounter);
    if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
      aLastGUIUpdate := now;
      StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
      StatusBar1.Panels[3].Text := '';
      StatusBar1.Panels[4].Text := '';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

procedure TForm1.Button16Click(Sender: TObject);
Var aData: ansiString;
    aTmpData: ansiString;
    aHash: ansiString;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<ansiString,ansistring>;
    aCollision: integer;
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
  aDictionary := TDictionary<ansiString,ansistring>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStr(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ALStringHashSHA2(aData, THashSHA2.TSHA2Version.SHA512, true{hexEncode});
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;

procedure TForm1.Button17Click(Sender: TObject);
Var aData: String;
    aTmpData: String;
    aHash: String;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<String,string>;
    aCollision: integer;
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
  ALMemoCryptedData.Lines.Text := ALStringHashSHA2U(ALMemoDecryptedData.Lines.Text, Tencoding.UTF8, THashSHA2.TSHA2Version.SHA256);

  StatusBar1.Panels[0].Text := 'ALSHA2 256 (Unicode)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 25 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  aDictionary := TDictionary<String,string>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStrU(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ALStringHashSHA2U(aData, Tencoding.UTF8, THashSHA2.TSHA2Version.SHA256);
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;

procedure TForm1.Button18Click(Sender: TObject);
Var aData: Tbytes;
    aData2: Tbytes;
    aHash: String;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
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
  ALMemoCryptedData.Lines.Text := ALBinToHexU(Tencoding.UTF8.GetBytes(ALMemoDecryptedData.Lines.Text));
  ALMemoDecryptedData.Lines.Text := Tencoding.UTF8.GetString(ALHexToBinU(ALMemoCryptedData.Lines.Text));

  StatusBar1.Panels[0].Text := 'Bench HexEncode (UNICODE)';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'Input: 1 to 4096 bytes';
  StatusBar1.Panels[3].Text := '';
  StatusBar1.Panels[4].Text := '';
  ALMemoCollisions.Lines.Clear;
  aCounter := 0;
  aLastGUIUpdate := now;
  aStopWatch := TstopWatch.Create;
  while True do begin
    aData := ALRandomBytes(1+random(4096));
    aStopWatch.Start;
    aHash := ALBinToHexU(aData);
    aStopWatch.Stop;
    aData2 := ALHexToBinU(aHash);
    if (length(aData2) <> length(aData)) or
       (not comparemem(@aData2[0], @aData[0], length(aData2))) then
      raise Exception.Create('Test failed!');
    inc(acounter);
    if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
      aLastGUIUpdate := now;
      StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
      StatusBar1.Panels[3].Text := '';
      StatusBar1.Panels[4].Text := '';
      if (Sender as TButton).Tag = 0 then break;
      application.ProcessMessages;
    end;
  end;
end;

procedure TForm1.Button19Click(Sender: TObject);
begin
  if messageDLG('This operation will take a moment to execute, you will need to be patient.',mtConfirmation,[mbOK,MbCancel],0) = mrOK then begin
    ALTestCRC32Implementation(ALGetModulePath + '..\_source\crc32test.txt');
    Showmessage('Test passed successfully!');
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
Var aData: AnsiString;
    aTmpData: ansiString;
    aHash: integer;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<integer,ansistring>;
    aCollision: integer;
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
  aDictionary := TDictionary<integer,ansistring>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStr(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ZCrc32(0, aData[1], length(aData));
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;

{*********************************************}
procedure TForm1.Button2Click(Sender: TObject);
Var aData: AnsiString;
    aTmpData: ansiString;
    aHash: cardinal;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<cardinal,ansistring>;
    aCollision: integer;
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
  aDictionary := TDictionary<cardinal,ansistring>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStr(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ALStringHashCRC32(aData);
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
Var aData: AnsiString;
    aTmpData: ansiString;
    aHash: int64;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<int64,ansistring>;
    aCollision: integer;
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
  aDictionary := TDictionary<int64,ansistring>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStr(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ALStringHashCRC64(aData);
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;


{***************************}
{$WARN SYMBOL_DEPRECATED OFF}
procedure TForm1.Button3Click(Sender: TObject);
Var aData: AnsiString;
    aTmpData: ansiString;
    aHash: integer;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<integer,ansistring>;
    aCollision: integer;
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
  aDictionary := TDictionary<integer,ansistring>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStr(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := BobJenkinsHash(aData[1], Length(aData) * SizeOf(aData[1]), 0);
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;

{*********************************************}
procedure TForm1.Button4Click(Sender: TObject);
Var aData: AnsiString;
    aTmpData: ansiString;
    aHash: int64;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<int64,ansistring>;
    aCollision: integer;
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
  aDictionary := TDictionary<int64,ansistring>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStr(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ALFnv1aInt64(aData);
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;

{*********************************************}
procedure TForm1.Button5Click(Sender: TObject);
Var aData: AnsiString;
    aTmpData: ansiString;
    aHash: int32;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<int32,ansistring>;
    aCollision: integer;
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
  aDictionary := TDictionary<int32,ansistring>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStr(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ALFnv1aInt32(aData);
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;

{*********************************************}
procedure TForm1.Button6Click(Sender: TObject);
Var aHash: int32;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    P, x, y: integer;
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
  aCounter := 0;
  aLastGUIUpdate := now;
  aStopWatch := TstopWatch.Create;
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
      aStopWatch.Start;
      aHash := ALRandom32(maxint);
      aStopWatch.Stop;
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter)) + ' keys - ' + FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
      P := round(((PaintBox1.Width * PaintBox1.Height) / maxint) * aHash);
      Y := P div PaintBox1.Width;
      X := P mod PaintBox1.Width;
      PaintBox1.Canvas.Pixels[x,y] := $0000ff;
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
Var aHash: int64;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    P, x, y: integer;
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
  aCounter := 0;
  aLastGUIUpdate := now;
  aStopWatch := TstopWatch.Create;
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
      aStopWatch.Start;
      aHash := ALRandom64(ALMaxint64);
      aStopWatch.Stop;
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter)) + ' keys - ' + FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
      P := round(((PaintBox1.Width * PaintBox1.Height) / ALMaxint64) * aHash);
      Y := P div PaintBox1.Width;
      X := P mod PaintBox1.Width;
      PaintBox1.Canvas.Pixels[x,y] := $0000ff;
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

procedure TForm1.Button8Click(Sender: TObject);
Var aData: AnsiString;
    aTmpData: ansiString;
    aHash: ansiString;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: TDateTime;
    aDictionary: TDictionary<ansiString,ansistring>;
    aCollision: integer;
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
  aDictionary := TDictionary<ansiString,ansistring>.create;
  try
    aCounter := 0;
    aCollision := 0;
    aLastGUIUpdate := now;
    aStopWatch := TstopWatch.Create;
    while True do begin
      aData := ALRandomStr(1+random(25), ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']);
      aStopWatch.Start;
      aHash := ALBCryptHashPassword(aData, 12);
      aStopWatch.Stop;
      if aDictionary.TryGetValue(aHash, aTmpData) then begin
        if aTmpData <> aData then begin
          ALMemoCollisions.Lines.Add(string(aTmpData + '  =  ' + aData));
          inc(aCollision);
        end;
      end
      else aDictionary.Add(aHash, aData);
      inc(acounter);
      if millisecondsbetween(now, aLastGUIUpdate) > 200 then begin
        aLastGUIUpdate := now;
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s ('+ FormatFloat('0,.######', ((aStopWatch.Elapsed.TotalMilliseconds / acounter))) + ' ms/key)';
        StatusBar1.Panels[3].Text := inttostr(aCollision) + ' collisions';
        StatusBar1.Panels[4].Text := FormatFloat('#,.', aDictionary.Count) + ' keys';
        if (Sender as TButton).Tag = 0 then break;
        application.ProcessMessages;
      end;
    end;
  finally
    aDictionary.free;
  end;
end;

{$WARN SYMBOL_DEPRECATED ON}

{***********************************************}
procedure TForm1.ALButton2Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALBFEncryptStringCBC(AnsiString(ALMemoDecryptedData.Lines.Text), outString, AnsiString(EditKey.Text), True);
  ALMemoCryptedData.Lines.Text := String(ALBase64EncodeString(outString));
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
