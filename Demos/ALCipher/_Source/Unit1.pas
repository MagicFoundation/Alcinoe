unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Shellapi,
  System.Generics.Defaults, diagnostics, System.Generics.Collections, dateutils;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    Label1: TLabel;
    EditKey: TEdit;
    ALMemoDecryptedData: TMemo;
    Label2: TLabel;
    ALMemoCryptedData: TMemo;
    Label3: TLabel;
    ALButton10: TButton;
    ALButton11: TButton;
    ALButton12: TButton;
    ALButton13: TButton;
    Button1: TButton;
    Button3: TButton;
    ALMemoCollisions: TMemo;
    Label4: TLabel;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    PaintBox1: TPaintBox;
    Button13: TButton;
    Button12: TButton;
    Button16: TButton;
    Button14: TButton;
    Button17: TButton;
    procedure ALButton10Click(Sender: TObject);
    procedure ALButton11Click(Sender: TObject);
    procedure ALButton12Click(Sender: TObject);
    procedure ALButton13Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
  private
  public
  end;

var Form1: TForm1;

implementation

uses
  System.ZLib,
  System.Math,
  Alcinoe.Common,
  system.hash,
  system.netencoding,
  system.uitypes,
  Alcinoe.StringUtils,
  Alcinoe.FileUtils,
  Alcinoe.Cipher;

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
      LHash := Integer(System.ZLib.Crc32(0, @LData[1], length(LData)));
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

{$WARN SYMBOL_DEPRECATED ON}

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.