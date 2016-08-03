unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, cxGraphics, ALZlibExApi,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  cxLabel, Shellapi, System.Generics.Defaults, diagnostics,
  System.Generics.Collections, dateutils;

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
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
  public
  end;

var Form1: TForm1;

implementation

uses math,
     alcommon,
     AlAVLBinaryTree,
     ALString,
     alMime,
     alCipher,
     alZlibEx;

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
    aLastGUIUpdate: Extended;
    aDictionary: TDictionary<ansiString,ansistring>;
    aCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench MD5';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;
  StatusBar1.Panels[0].Text := 'MD5';
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
      aHash := ALStringHashMD5(aData, false);
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
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s';
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
    aLastGUIUpdate: Extended;
    aDictionary: TDictionary<ansiString,ansistring>;
    aCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench SHA1';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;
  StatusBar1.Panels[0].Text := 'SHA1';
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
      aHash := ALStringHashSHA1(aData, false);
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
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s';
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
  ALBFEncryptStringCBC(ALBase64DecodeString(ALTrim(AnsiString(ALMemocryptedData.Lines.Text))), outString, AnsiString(EditKey.Text), False);
  ALMemoDeCryptedData.Lines.Text := String(outString);
end;

{***********************************************}
procedure TForm1.ALButton6Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptString(AnsiString(ALMemoDecryptedData.Lines.Text), outString, AnsiString(EditKey.Text), True);
  ALMemoCryptedData.Lines.Text := String(ALBase64EncodeString(outString));
end;

{***********************************************}
procedure TForm1.ALButton7Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptString(ALBase64DecodeString(ALTrim(ansiString(ALMemocryptedData.Lines.Text))), outString, ansiString(EditKey.Text), False);
  ALMemoDeCryptedData.Lines.Text := string(outString);
end;

{***********************************************}
procedure TForm1.ALButton8Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptStringCBC(AnsiString(ALMemoDecryptedData.Lines.Text), outString, AnsiString(EditKey.Text), True);
  ALMemoCryptedData.Lines.Text := String(ALBase64EncodeString(outString));
end;

{***********************************************}
procedure TForm1.ALButton9Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptStringCBC(ALBase64DecodeString(ALTrim(ansiString(ALMemocryptedData.Lines.Text))), outString, ansiString(EditKey.Text), False);
  ALMemoDeCryptedData.Lines.Text := string(outString);
end;

{*********************************************}
procedure TForm1.Button1Click(Sender: TObject);
Var aData: AnsiString;
    aTmpData: ansiString;
    aHash: integer;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: Extended;
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
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s';
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
    aHash: integer;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: Extended;
    aDictionary: TDictionary<integer,ansistring>;
    aCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench CRC32';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;
  StatusBar1.Panels[0].Text := 'CRC32';
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
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s';
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
    aLastGUIUpdate: Extended;
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
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s';
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
    aLastGUIUpdate: Extended;
    aDictionary: TDictionary<int64,ansistring>;
    aCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench FNV-1a (int64)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;
  StatusBar1.Panels[0].Text := 'FNV-1a (int64)';
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
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s';
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
    aHash: integer;
    aCounter: integer;
    aStopWatch: TstopWatch;
    aLastGUIUpdate: Extended;
    aDictionary: TDictionary<integer,ansistring>;
    aCollision: integer;
begin
  if (Sender as TButton).Tag = 1 then begin
    (Sender as TButton).Tag := 0;
    (Sender as TButton).Caption := 'Bench FNV-1a (int32)';
    exit;
  end;
  (Sender as TButton).Tag := 1;
  (Sender as TButton).Caption := 'Stop';
  randomize;
  StatusBar1.Panels[0].Text := 'FNV-1a (int32)';
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
        StatusBar1.Panels[1].Text := FormatFloat('#,.', (acounter / (aStopWatch.Elapsed.TotalMilliseconds / 1000))) + ' keys/s';
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
  ALMemoCryptedData.Lines.Text := String(ALBase64EncodeString(outString));
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
