{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclPrint.pas.                                                               }
{                                                                                                  }
{ The Initial Developers of the Original Code are unknown.                                         }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ The Initial Developer of the function DPSetDefaultPrinter is Microsoft. Portions created by      }
{ Microsoft are Copyright (C) 2004 Microsoft Corporation. All Rights Reserved.                     }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Matthias Thoma (mthoma)                                                                        }
{   Karl Ivar Hansen                                                                               }
{   Martin Cakrt                                                                                   }
{   Jared Davison                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains print-related classes and functions.                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclPrint;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  Winapi.Windows, System.Classes, Vcl.StdCtrls, System.SysUtils, System.IniFiles,
  {$ELSE ~HAS_UNITSCOPE}
  Windows, Classes, StdCtrls, SysUtils, IniFiles,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase;

const
  CCHBinName = 24;
  CCHPaperName = 64;
  CBinMax = 256;
  CPaperNames = 256;

type
  PWordArray = ^TWordArray;
  TWordArray = array [0..255] of Word;

type
  EJclPrinterError = class(EJclError);

  TJclPrintSet = class(TObject)
  private
    FDevice: PChar;  { TODO : change to string }
    FDriver: PChar;
    FPort: PChar;
    FHandle: THandle;
    FPrinter: Integer;
    FBinArray: PWordArray;
    FNumBins: DWord;
    FPaperArray: PWordArray;
    FNumPapers: DWord;
    FDpiX: Integer;
    FiDpiY: Integer;
    procedure CheckPrinter;
    procedure SetBinArray;
    procedure SetPaperArray;
    function DefaultPaperName(const PaperID: Word): string;
    function GetDevModePrinterDriverVersion: Word;
    function GetDevModePrinterDriver: string;
    function GetDevModePrinterDriverExtra: TDynByteArray;
    function LockDeviceMode: PDeviceMode;
    procedure SetDeviceMode(Creating: Boolean);
    procedure SetPrinterName(const Value: string);
    procedure UnlockDeviceMode;
  protected
    procedure SetOrientation(Orientation: Integer);
    function GetOrientation: Integer;
    procedure SetPaperSize(Size: Integer);
    function GetPaperSize: Integer;
    procedure SetPaperLength(Length: Integer);
    function GetPaperLength: Integer;
    procedure SetPaperWidth(Width: Integer);
    function GetPaperWidth: Integer;
    procedure SetScale(Scale: Integer);
    function GetScale: Integer;
    procedure SetCopies(Copies: Integer);
    function GetCopies: Integer;
    procedure SetBin(Bin: Integer);
    function GetBin: Integer;
    procedure SetPrintQuality(Quality: Integer);
    function GetPrintQuality: Integer;
    procedure SetColor(Color: Integer);
    function GetColor: Integer;
    procedure SetDuplex(Duplex: Integer);
    function GetDuplex: Integer;
    procedure SetYResolution(YRes: Integer);
    function GetYResolution: Integer;
    procedure SetTrueTypeOption(Option: Integer);
    function GetTrueTypeOption: Integer;
    function GetPrinterName: string;
    function GetPrinterPort: string;
    function GetPrinterDriver: string;
    procedure SetBinFromList(BinNum: Word);
    function GetBinIndex: Word;
    procedure SetPaperFromList(PaperNum: Word);
    function GetPaperIndex: Word;
    function ReadFromCustomIni(const PrIniFile: TCustomIniFile; const Section: string): Boolean;
    procedure SaveToCustomIni(const PrIniFile: TCustomIniFile; const Section: string);
    procedure SetPort(Port: string);
    procedure DevModePrinterDriverExtraReinstate(const ExtraData: TDynByteArray;
      const ExtraDataDriverName: string; const ExtraDataDriverVersion: Word);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // use the other implementations
    //function GetBinSourceList: TStringList; overload;
    //function GetPaperList: TStringList; overload;
    procedure GetBinSourceList(List: TStrings); overload;
    procedure GetPaperList(List: TStrings); overload;
    procedure UpdateDeviceMode(const ADeviceMode: PDeviceMode);
    procedure SaveToDefaults;
    procedure SavePrinterAsDefault;
    procedure ResetPrinterDialogs;
    function XInchToDot(const Inches: Double): Integer;
    function YInchToDot(const Inches: Double): Integer;
    function XCmToDot(const Cm: Double): Integer;
    function YCmToDot(const Cm: Double): Integer;
    function CpiToDot(const Cpi, Chars: Double): Integer;
    function LpiToDot(const Lpi, Lines: Double): Integer;
    procedure TextOutInch(const X, Y: Double; const Text: string);
    procedure TextOutCm(const X, Y: Double; const Text: string);
    procedure TextOutCpiLpi(const Cpi, Chars, Lpi, Lines: Double; const Text: string);
    procedure CustomPageSetup(const Width, Height: Double);
    procedure DevModePrinterDriverExtraClear;
    procedure SaveToIniFile(const IniFileName, Section: string); virtual;
    function ReadFromIniFile(const IniFileName, Section: string): Boolean; virtual;
    property Orientation: Integer read GetOrientation write SetOrientation;
    property PaperSize: Integer read GetPaperSize write SetPaperSize;
    property PaperLength: Integer read GetPaperLength write SetPaperLength;
    property PaperWidth: Integer read GetPaperWidth write SetPaperWidth;
    property Scale: Integer read GetScale write SetScale;
    property Copies: Integer read GetCopies write SetCopies;
    property DefaultSource: Integer read GetBin write SetBin;
    property PrintQuality: Integer read GetPrintQuality write SetPrintQuality;
    property Color: Integer read GetColor write SetColor;
    property Duplex: Integer read GetDuplex write SetDuplex;
    property YResolution: Integer read GetYResolution write SetYResolution;
    property TrueTypeOption: Integer read GetTrueTypeOption write SetTrueTypeOption;
    property PrinterName: string read GetPrinterName write SetPrinterName;
    property PrinterPort: string read GetPrinterPort write SetPort;
    property PrinterDriver: string read GetPrinterDriver;
    property BinIndex: Word read GetBinIndex write SetBinFromList;
    property DevModePrinterDriverVersion: Word read GetDevModePrinterDriverVersion;
    property DevModePrinterDriver: string read GetDevModePrinterDriver;
    property DevModePrinterDriverExtra: TDynByteArray read
        GetDevModePrinterDriverExtra;
    property PaperIndex: Word read GetPaperIndex write SetPaperFromList;
    property DpiX: Integer read FDpiX write FDpiX;
    property DpiY: Integer read FiDpiY write FiDpiY;
  end;

  TPrinterData = {$IFDEF SUPPORTS_UNICODE_STRING}RawByteString{$ELSE}AnsiString{$ENDIF};

procedure DirectPrint(const Printer: string; const Data: TPrinterData;
  const DocumentName: string = '');
procedure SetPrinterPixelsPerInch;
function GetPrinterResolution: TPoint;
function CharFitsWithinDots(const Text: string; const Dots: Integer): Integer;
//procedure PrintTextRotation(X, Y: Integer; Rotation: Word; Text: string);
procedure PrintMemo(const Memo: TMemo; const Rect: TRect);

function GetDefaultPrinterName: string;
function DPGetDefaultPrinter(out PrinterName: string): Boolean;
function DPSetDefaultPrinter(const PrinterName: string): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\vcl';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  Vcl.Graphics, Winapi.Messages, Vcl.Printers, Winapi.WinSpool,
  {$ELSE ~HAS_UNITSCOPE}
  Graphics, Messages, Printers, WinSpool,
  {$ENDIF ~HAS_UNITSCOPE}
  JclSysInfo, JclVclResources;

const
  PrintIniPrinterName   = 'PrinterName';
  PrintIniPrinterPort   = 'PrinterPort';
  PrintIniOrientation   = 'Orientation';
  PrintIniPaperSize     = 'PaperSize';
  PrintIniPaperLength   = 'PaperLength';
  PrintIniPaperWidth    = 'PaperWidth';
  PrintIniScale         = 'Scale';
  PrintIniCopies        = 'Copies';
  PrintIniDefaultSource = 'DefaultSource';
  PrintIniPrintQuality  = 'PrintQuality';
  PrintIniColor         = 'Color';
  PrintIniDuplex        = 'Duplex';
  PrintIniYResolution   = 'YResolution';
  PrintIniTTOption      = 'TTOption';

  PrintDriverExtraSize = 'DriverExtraSize';
  PrintDriverExtraData = 'DriverExtraData';
  PrintDriverVersion = 'DriverVersion';
  PrintDriverName = 'DriverName';

  cWindows: PChar = 'windows';
  cDevice = 'device';
  cPrintSpool = 'winspool.drv';

// Misc. functions
procedure DirectPrint(const Printer: string; const Data: TPrinterData; const DocumentName: string);
const
  cRaw = 'RAW';
type
  TDoc_Info_1 = record
    DocName: PChar;
    OutputFile: PChar;
    Datatype: PChar;
  end;
var
  PrinterHandle: THandle;
  DocInfo: TDoc_Info_1;
  BytesWritten: Cardinal;
  Count: Cardinal;
  Defaults: TPrinterDefaults;
begin
  // Defaults added for network printers. Supposedly the last member is ignored
  // by Windows 9x but is necessary for Windows NT. Code was copied from a msg
  // by Alberto Toledo to the C++ Builder techlist and fwd by Theo Bebekis.
  Defaults.pDatatype := cRaw;
  Defaults.pDevMode := nil;
  Defaults.DesiredAccess := PRINTER_ACCESS_USE;
  Count := Length(Data);
  if not OpenPrinter(PChar(Printer), PrinterHandle, @Defaults) then
    raise EJclPrinterError.CreateRes(@RsInvalidPrinter);
  // Fill in the structure with info about this "document"
  if DocumentName = '' then
    DocInfo.DocName := PChar(LoadResString(@RsSpoolerDocName))
  else
    DocInfo.DocName := PChar(DocumentName);
  DocInfo.OutputFile := nil;
  DocInfo.Datatype := cRaw;
  try
    // Inform the spooler the document is beginning
    if StartDocPrinter(PrinterHandle, 1, @DocInfo) = 0 then
      EJclPrinterError.CreateRes(@RsNAStartDocument);
    try
      // Start a page
      if not StartPagePrinter(PrinterHandle) then
        EJclPrinterError.CreateRes(@RsNAStartPage);
      try
        // Send the data to the printer
        if not WritePrinter(PrinterHandle, PAnsiChar(Data), Count * SizeOf(AnsiChar), BytesWritten) then
          EJclPrinterError.CreateRes(@RsNASendData);
      finally
        // End the page
        if not EndPagePrinter(PrinterHandle) then
          EJclPrinterError.CreateRes(@RsNAEndPage);
      end;
    finally
      // Inform the spooler that the document is ending
      if not EndDocPrinter(PrinterHandle) then
        EJclPrinterError.CreateRes(@RsNAEndDocument);
    end;
  finally
    // Tidy up the printer handle
    ClosePrinter(PrinterHandle);
  end;
  // Check to see if correct number of bytes written
  if BytesWritten <> Count * SizeOf(AnsiChar) then
    EJclPrinterError.CreateRes(@RsNATransmission);
end;

procedure SetPrinterPixelsPerInch;
var
  FontSize: Integer;
begin
  FontSize := Printer.Canvas.Font.Size;
  Printer.Canvas.Font.PixelsPerInch := GetDeviceCaps(Printer.Handle, LogPixelsY);
  Printer.Canvas.Font.Size := FontSize;
end;

function GetPrinterResolution: TPoint;
begin
  Result.X := GetDeviceCaps(Printer.Handle, LogPixelsX);
  Result.Y := GetDeviceCaps(Printer.Handle, LogPixelsY);
end;

function CharFitsWithinDots(const Text: string; const Dots: Integer): Integer;
begin
  Result := Length(Text);
  while (Result > 0) and (Printer.Canvas.TextWidth(Copy(Text, 1, Result)) > Dots) do
    Dec(Result);
end;

//WIMDC: The function CanvasTextOutRotation contains a bug in DxGraphics so no need to
//       implement it right now here
(*
procedure PrintTextRotation(X, Y: Integer; Rotation: Word; Text: string);
begin
  CanvasTextOutRotation(Printer.Canvas, X, Y, Rotation, Text);
end;
*)

//WIMDC took the function from DXGraphics and replaced some lines to work with the TStrings class
//      of the memo.

procedure CanvasMemoOut(Canvas: TCanvas; Memo: TMemo; Rect: TRect);
var
  MemoText: PChar;
begin
  MemoText := Memo.Lines.GetText;
  if MemoText <> nil then
    try
      DrawText(Canvas.Handle, MemoText, StrLen(MemoText), Rect,
        DT_LEFT or DT_EXPANDTABS or DT_WORDBREAK);
    finally
      StrDispose(MemoText);
    end;
end;

procedure PrintMemo(const Memo: TMemo; const Rect: TRect);
begin
  CanvasMemoOut(Printer.Canvas, Memo, Rect);
end;

function GetDefaultPrinterName: string;
begin
  DPGetDefaultPrinter(Result);
end;

{ TODO -cHelp : DPGetDefaultPrinter, Author: Microsoft }
// DPGetDefaultPrinter
// Parameters:
//   PrinterName: Return the printer name.
// Returns: True for success, False for failure.

// Source of the original code: Microsoft Knowledge Base Article - 246772
//   http://support.microsoft.com/default.aspx?scid=kb;en-us;246772
function DPGetDefaultPrinter(out PrinterName: string): Boolean;
const
  BUFSIZE = 8192;
type
  TGetDefaultPrinter = function(Buffer: PChar; var Size: DWORD): BOOL; stdcall;
var
  Needed, Returned: DWORD;
  PI2: PPrinterInfo2;
  WinVer: TWindowsVersion;
  hWinSpool: HMODULE;
  GetDefPrint: TGetDefaultPrinter;
  Size: DWORD;
begin
  Result := False;
  PrinterName := '';
  WinVer := GetWindowsVersion;
  // Windows 9x uses EnumPrinters
  if WinVer in [wvWin95, wvWin95OSR2, wvWin98, wvWin98SE, wvWinME] then
  begin
    SetLastError(0);
    Result := EnumPrinters(PRINTER_ENUM_DEFAULT, nil, 2, nil, 0, Needed, Returned);
    if not Result and ((GetLastError <> ERROR_INSUFFICIENT_BUFFER) or (Needed = 0)) then
      Exit;
    GetMem(PI2, Needed);
    try
      Result := EnumPrinters(PRINTER_ENUM_DEFAULT, nil, 2, PI2, Needed, Needed, Returned);
      if Result then
        PrinterName := PI2^.pPrinterName;
    finally
      FreeMem(PI2);
    end;
  end
  else
  // Win NT uses WIN.INI (registry)
  if WinVer in [wvWinNT31, wvWinNT35, wvWinNT351, wvWinNT4] then
  begin
    SetLength(PrinterName, BUFSIZE);
    Result := GetProfileString(cWindows, cDevice, ',,,', PChar(PrinterName), BUFSIZE) > 0;
    if Result then
      PrinterName := Copy(PrinterName, 1, Pos(',', PrinterName) - 1)
    else
      PrinterName := '';
  end
  else
  // >= Win 2000 uses GetDefaultPrinter
  begin
    hWinSpool := SafeLoadLibrary(cPrintSpool);
    if hWinSpool <> 0 then
      try
        {$IFDEF UNICODE}
        @GetDefPrint := GetProcAddress(hWinSpool, 'GetDefaultPrinterW');
        {$ELSE}
        @GetDefPrint := GetProcAddress(hWinSpool, 'GetDefaultPrinterA');
        {$ENDIF UNICODE}
        if not Assigned(GetDefPrint) then
          Exit;
        Size := BUFSIZE;
        SetLength(PrinterName, Size);
        Result := GetDefPrint(PChar(PrinterName), Size);
        if Result then
          SetLength(PrinterName, StrLen(PChar(PrinterName)))
        else
          PrinterName := '';
      finally
        FreeLibrary(hWinSpool);
      end;
  end;
end;

{ TODO -cHelp : DPSetDefaultPrinter, Author: Microsoft }
// DPSetDefaultPrinter
// Parameters:
//   PrinterName: Valid name of existing printer to make default.
// Returns: True for success, False for failure.

// Source of the original code: Microsoft Knowledge Base Article - 246772
//   http://support.microsoft.com/default.aspx?scid=kb;en-us;246772
function DPSetDefaultPrinter(const PrinterName: string): Boolean;
type
  TSetDefaultPrinter = function(APrinterName: PChar): BOOL; stdcall;
var
  Needed: DWORD;
  PI2: PPrinterInfo2;
  WinVer: TWindowsVersion;
  hPrinter: THandle;
  hWinSpool: HMODULE;
  SetDefPrint: TSetDefaultPrinter;
  PrinterStr: string;
begin
  Result := False;
  if PrinterName = '' then
    Exit;
  WinVer := GetWindowsVersion;
  if WinVer in [wvWin95, wvWin95OSR2, wvWin98, wvWin98SE, wvWinME] then
  begin
    Result := OpenPrinter(PChar(PrinterName), hPrinter, nil);
    if Result and (hPrinter <> 0) then
      try
        SetLastError(0);
        Result := GetPrinter(hPrinter, 2, nil, 0, @Needed);
        if not Result and ((GetLastError <> ERROR_INSUFFICIENT_BUFFER) or (Needed = 0)) then
          Exit;
        GetMem(PI2, Needed);
        try
          Result := GetPrinter(hPrinter, 2, PI2, Needed, @Needed);
          if Result then
          begin
            PI2^.Attributes := PI2^.Attributes or PRINTER_ATTRIBUTE_DEFAULT;
            Result := SetPrinter(hPrinter, 2, PI2, 0);
            if Result then
              SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
                LPARAM(cWindows), SMTO_NORMAL, 1000, {$IFDEF RTL230_UP}@{$ENDIF}Needed);
          end;
        finally
          FreeMem(PI2);
        end;
      finally
        ClosePrinter(hPrinter);
      end;
  end
  else
  // Win NT uses WIN.INI (registry)
  if WinVer in [wvWinNT31, wvWinNT35, wvWinNT351, wvWinNT4] then
  begin
    Result := OpenPrinter(PChar(PrinterName), hPrinter, nil);
    if Result and (hPrinter <> 0) then
      try
        SetLastError(0);
        Result := GetPrinter(hPrinter, 2, nil, 0, @Needed);
        if not Result and ((GetLastError <> ERROR_INSUFFICIENT_BUFFER) or (Needed = 0)) then
          Exit;
        GetMem(PI2, Needed);
        try
          Result := GetPrinter(hPrinter, 2, PI2, Needed, @Needed);
          if Result and (PI2^.pDriverName <> nil) and (PI2^.pPortName <> nil) then
          begin
            PrinterStr := PrinterName + ',' + PI2^.pDriverName + ',' + PI2^.pPortName;
            Result := WriteProfileString(cWindows, cDevice, PChar(PrinterStr));
            if Result then
              SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0, 0,
                SMTO_NORMAL, 1000, {$IFDEF RTL230_UP}@{$ENDIF}Needed);
          end;
        finally
          FreeMem(PI2);
        end;
      finally
        ClosePrinter(hPrinter);
      end;
  end
  else
  // >= Win 2000 uses SetDefaultPrinter
  begin
    hWinSpool := SafeLoadLibrary(cPrintSpool);
    if hWinSpool <> 0 then
      try
        {$IFDEF UNICODE}
        @SetDefPrint := GetProcAddress(hWinSpool, 'SetDefaultPrinterW');
        {$ELSE}
        @SetDefPrint := GetProcAddress(hWinSpool, 'SetDefaultPrinterA');
        {$ENDIF UNICODE}
        if Assigned(SetDefPrint) then
          Result := SetDefPrint(PChar(PrinterName));
      finally
        FreeLibrary(hWinSpool);
      end;
  end;
end;

// TJclPrintSet
constructor TJclPrintSet.Create;
begin
  inherited Create;
  FBinArray := nil;
  FPaperArray := nil;
  FPrinter := -99;         { TODO : why -99 }
  GetMem(FDevice, 255);
  GetMem(FDriver, 255);
  GetMem(FPort, 255);
  FHandle := 0;
end;

destructor TJclPrintSet.Destroy;
begin
  if FBinArray <> nil then
    FreeMem(FBinArray, FNumBins * SizeOf(Word));
  if FPaperArray <> nil then
    FreeMem(FPaperArray, FNumPapers * SizeOf(Word));
  if FDevice <> nil then
    FreeMem(FDevice, 255);
  if FDriver <> nil then
    FreeMem(FDriver, 255);
  if FPort <> nil then
    FreeMem(FPort, 255);
  inherited Destroy;
end;

procedure TJclPrintSet.CheckPrinter;
var
  NewHandle: THandle;
  PrinterChanged: Boolean;
  LastDevice, LastDriver, LastPort: string;
begin
  LastDevice := FDevice;
  LastDriver := FDriver;
  LastPort := FPort;
  
  Printer.GetPrinter(FDevice, FDriver, FPort, NewHandle);
  PrinterChanged := (FHandle <> NewHandle) or (LastDevice <> FDevice)
    or (LastDriver <> FDriver) or (LastPort <> FPort) or (FPrinter <> Printer.PrinterIndex);
  FHandle := NewHandle;
  FPrinter := Printer.PrinterIndex;
  Printer.SetPrinter(FDevice, FDriver, FPort, FHandle);
  if PrinterChanged then
    SetDeviceMode(False);
end;

procedure TJclPrintSet.SetBinArray;
var
  NumBinsRec: DWord;
  ADeviceMode: PDeviceMode;
begin
  if FBinArray <> nil then
    FreeMem(FBinArray, FNumBins * SizeOf(Word));
  FBinArray := nil;
  ADeviceMode := LockDeviceMode;
  try
    FNumBins := DeviceCapabilities(FDevice, FPort, DC_Bins, nil, ADeviceMode);
    if FNumBins > 0 then
    begin
      GetMem(FBinArray, FNumBins * SizeOf(Word));
      NumBinsRec := DeviceCapabilities(FDevice, FPort, DC_Bins,
        PChar(FBinArray), ADeviceMode);
      if NumBinsRec <> FNumBins then
        raise EJclPrinterError.CreateRes(@RsRetrievingSource);
    end;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetPaperArray;
var
  NumPapersRec: DWord;
  ADeviceMode: PDeviceMode;
begin
  if FPaperArray <> nil then
    FreeMem(FPaperArray, FNumPapers * SizeOf(Word));
  ADeviceMode := LockDeviceMode;
  try
    FNumPapers := DeviceCapabilities(FDevice, FPort, DC_Papers, nil, ADeviceMode);
    if FNumPapers > 0 then
    begin
      GetMem(FPaperArray, FNumPapers * SizeOf(Word));
      NumPapersRec := DeviceCapabilities(FDevice, FPort, DC_Papers,
        PChar(FPaperArray), ADeviceMode);
      if NumPapersRec <> FNumPapers then
        raise EJclPrinterError.CreateRes(@RsRetrievingPaperSource);
    end
    else
      FPaperArray := nil;
  finally
    UnlockDeviceMode;
  end;
end;

{ TODO : complete this list }
// Since Win32 the strings are stored in the printer driver, no chance to get
// a list from Windows
function TJclPrintSet.DefaultPaperName(const PaperID: Word): string;
begin
  case PaperID of
    dmpaper_Letter:
      Result := LoadResString(@RsPSLetter);
    dmpaper_LetterSmall:
      Result := LoadResString(@RsPSLetter);
    dmpaper_Tabloid:
      Result := LoadResString(@RsPSTabloid);
    dmpaper_Ledger:
      Result := LoadResString(@RsPSLedger);
    dmpaper_Legal:
      Result := LoadResString(@RsPSLegal);
    dmpaper_Statement:
      Result := LoadResString(@RsPSStatement);
    dmpaper_Executive:
      Result := LoadResString(@RsPSExecutive);
    dmpaper_A3:
      Result := LoadResString(@RsPSA3);
    dmpaper_A4:
      Result := LoadResString(@RsPSA4);
    dmpaper_A4Small:
      Result := LoadResString(@RsPSA4);
    dmpaper_A5:
      Result := LoadResString(@RsPSA5);
    dmpaper_B4:
      Result := LoadResString(@RsPSB4);
    dmpaper_B5:
      Result := LoadResString(@RsPSB5);
    dmpaper_Folio:
      Result := LoadResString(@RsPSFolio);
    dmpaper_Quarto:
      Result := LoadResString(@RsPSQuarto);
    dmpaper_10X14:
      Result := LoadResString(@RsPS10x14);
    dmpaper_11X17:
      Result := LoadResString(@RsPS11x17);
    dmpaper_Note:
      Result := LoadResString(@RsPSNote);
    dmpaper_Env_9:
      Result := LoadResString(@RsPSEnv9);
    dmpaper_Env_10:
      Result := LoadResString(@RsPSEnv10);
    dmpaper_Env_11:
      Result := LoadResString(@RsPSEnv11);
    dmpaper_Env_12:
      Result := LoadResString(@RsPSEnv12);
    dmpaper_Env_14:
      Result := LoadResString(@RsPSEnv14);
    dmpaper_CSheet:
      Result := LoadResString(@RsPSCSheet);
    dmpaper_DSheet:
      Result := LoadResString(@RsPSDSheet);
    dmpaper_ESheet:
      Result := LoadResString(@RsPSESheet);
    dmpaper_User:
      Result := LoadResString(@RsPSUser);
  else
    Result := LoadResString(@RsPSUnknown);
  end;
end;

procedure TJclPrintSet.GetBinSourceList(List: TStrings);
type
  TBinName = array [0..CCHBinName - 1] of Char;
  TBinArray = array [1..cBinMax] of TBinName;
  PBinArray = ^TBinArray;
var
  NumBinsRec: DWord;
  BinArray: PBinArray;
  BinStr: string;
  Idx: Integer;
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  BinArray := nil;
  if FNumBins = 0 then
    Exit;
  List.BeginUpdate;
  try
    GetMem(BinArray, FNumBins * SizeOf(TBinName));
    List.Clear;
    ADeviceMode := LockDeviceMode;
    try
      NumBinsRec := DeviceCapabilities(FDevice, FPort, DC_BinNames,
        PChar(BinArray), ADeviceMode);
    finally
      UnlockDeviceMode;
    end;
    if NumBinsRec <> FNumBins then
      raise EJclPrinterError.CreateRes(@RsRetrievingSource);
    for Idx := 1 to NumBinsRec do
    begin
      BinStr := StrPas(BinArray^[Idx]);
      List.Add(BinStr);
    end;
  finally
    List.EndUpdate;
    if BinArray <> nil then
      FreeMem(BinArray, FNumBins * SizeOf(TBinName));
  end;
end;

procedure TJclPrintSet.GetPaperList(List: TStrings);
type
  TPaperName = array [0..CCHPaperName - 1] of Char;
  TPaperArray = array [1..cPaperNames] of TPaperName;
  PPaperArray = ^TPaperArray;
var
  NumPaperRec: DWord;
  PaperArray: PPaperArray;
  PaperStr: string;
  Idx: Integer;
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  PaperArray := nil;
  if FNumPapers = 0 then
    Exit;
  List.BeginUpdate;
  List.Clear;
  try
    GetMem(PaperArray, FNumPapers * SizeOf(TPaperName));
    ADeviceMode := LockDeviceMode;
    try
      NumPaperRec := DeviceCapabilities(FDevice, FPort, DC_PaperNames,
        PChar(PaperArray), ADeviceMode);
    finally
      UnlockDeviceMode;
    end;
    if NumPaperRec <> FNumPapers then
    begin
      for Idx := 1 to FNumPapers do
      begin
        PaperStr := DefaultPaperName(FPaperArray^[Idx - 1]);
        List.Add(PaperStr);
      end;
    end
    else
    begin
      for Idx := 1 to NumPaperRec do
      begin
        PaperStr := StrPas(PaperArray^[Idx]);
        List.Add(PaperStr);
      end;
    end;
  finally
    List.EndUpdate;
    if PaperArray <> nil then
      FreeMem(PaperArray, FNumPapers * SizeOf(TPaperName));
  end;
end;

procedure TJclPrintSet.SetDeviceMode(Creating: Boolean);
var
  Res: TPoint;
  ADeviceMode: PDeviceMode;
  NewHandle: THandle;
begin
  Printer.GetPrinter(FDevice, FDriver, FPort, NewHandle);
  if NewHandle = 0 then
  begin
    Printer.PrinterIndex := Printer.PrinterIndex;
    Printer.GetPrinter(FDevice, FDriver, FPort, NewHandle);
  end;
  FHandle := NewHandle;
  if FHandle <> 0 then
  begin
    ADeviceMode := GlobalLock(FHandle);

    FPrinter := Printer.PrinterIndex;
    UpdateDeviceMode(ADeviceMode);
    //FDeviceMode^.dmFields := 0;
    SetBinArray;
    SetPaperArray;
  end
  else
  begin
    if not Creating then
      raise EJclPrinterError.CreateRes(@RsDeviceMode);
    FPrinter := -99;
  end;
  Res := GetPrinterResolution;
  dpiX := Res.X;
  dpiY := Res.Y;
  if FHandle <> 0 then
    GlobalUnLock(FHandle);
end;

procedure TJclPrintSet.UpdateDeviceMode(const ADeviceMode: PDeviceMode);
var
  DrvHandle: THandle;
  ExtDevCode: Integer;
begin
  // ONLY CALL when ADeviceMode is locked by caller!!!

  //CheckPrinter;
  if OpenPrinter(FDevice, DrvHandle, nil) then
  try
    ADeviceMode^.dmFields := dm_Orientation or dm_PaperSize or
      dm_PaperLength or dm_PaperWidth or
      dm_Scale or dm_Copies or
      dm_DefaultSource or dm_PrintQuality or
      dm_Color or dm_Duplex or
      dm_YResolution or dm_TTOption;
    ExtDevCode := DocumentProperties(0, DrvHandle, FDevice,
      ADeviceMode^, ADeviceMode^,
      DM_IN_BUFFER or DM_OUT_BUFFER);
    if ExtDevCode <> IDOK then
      raise EJclPrinterError.CreateRes(@RsUpdatingPrinter);
  finally
    ClosePrinter(DrvHandle);
  end;
end;

procedure TJclPrintSet.SaveToDefaults;
var
  DrvHandle: THandle;
  ExtDevCode: Integer;
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  OpenPrinter(FDevice, DrvHandle, nil);
  ADeviceMode := LockDeviceMode;
  try
    ExtDevCode := DocumentProperties(0, DrvHandle, FDevice,
      ADeviceMode^, ADeviceMode^, DM_IN_BUFFER or DM_UPDATE);
  finally
    UnlockDeviceMode;
  end;
  if ExtDevCode <> IDOK then
    raise EJclPrinterError.CreateRes(@RsUpdatingPrinter)
  else
    SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, 0);
  ClosePrinter(DrvHandle);
end;

procedure TJclPrintSet.SavePrinterAsDefault;
begin
  CheckPrinter;
  DPSetDefaultPrinter(FDevice);
end;

procedure TJclPrintSet.ResetPrinterDialogs;
begin
  Printer.GetPrinter(FDevice, FDriver, FPort, FHandle);
  Printer.SetPrinter(FDevice, FDriver, FPort, FHandle);
  SetDeviceMode(False);
end;

function TJclPrintSet.XInchToDot(const Inches: Double): Integer;
begin
  Result := Trunc(DpiX * Inches);
end;

function TJclPrintSet.YInchToDot(const Inches: Double): Integer;
begin
  Result := Trunc(DpiY * Inches);
end;

function TJclPrintSet.XCmToDot(const Cm: Double): Integer;
begin
  Result := Trunc(DpiX * (Cm * 2.54));
end;

function TJclPrintSet.YCmToDot(const Cm: Double): Integer;
begin
  Result := Trunc(DpiY * (Cm * 2.54));
end;

function TJclPrintSet.CpiToDot(const Cpi, Chars: Double): Integer;
begin
  Result := Trunc((DpiX * Chars) / Cpi);
end;

function TJclPrintSet.LpiToDot(const Lpi, Lines: Double): Integer;
begin
  Result := Trunc((DpiY * Lpi) / Lines);
end;

procedure TJclPrintSet.TextOutInch(const X, Y: Double; const Text: string);
begin
  Printer.Canvas.TextOut(XInchToDot(X), YInchToDot(Y), Text);
end;

procedure TJclPrintSet.TextOutCm(const X, Y: Double; const Text: string);
begin
  Printer.Canvas.TextOut(XCmToDot(X), YCmToDot(Y), Text);
end;

procedure TJclPrintSet.TextOutCpiLpi(const Cpi, Chars, Lpi, Lines: Double; const Text: string);
begin
  Printer.Canvas.TextOut(CpiToDot(Cpi, Chars), LpiToDot(Lpi, Lines), Text);
end;

procedure TJclPrintSet.CustomPageSetup(const Width, Height: Double);
begin
  PaperSize := dmPaper_User;
  PaperLength := Trunc(254 * Height);
  YResolution := Trunc(DpiY * Height);
  PaperWidth := Trunc(254 * Width);
end;

procedure TJclPrintSet.DevModePrinterDriverExtraClear;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    ADeviceMode^.dmDriverExtra := 0;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.DevModePrinterDriverExtraReinstate(const ExtraData: TDynByteArray;
  const ExtraDataDriverName: string; const ExtraDataDriverVersion: Word);
var
  Src, Dest: PDeviceMode;
  ADeviceModeDriverExtra: PByte;
  NewHandle: THandle;
begin
  CheckPrinter;
    { http://support.microsoft.com/kb/167345
      Using a DEVMODE structure to modify printer settings is more difficult than just changing the fields of the structure. Specifically, a valid DEVMODE structure for a device contains private data that can only be modified by the DocumentProperties() function.
      This article explains how to modify the contents of a DEVMODE structure with the DocumentProperties() function.}

  if FHandle <> 0 then
  begin
    Src := GlobalLock(FHandle);
    try
      if not ((Src^.dmDeviceName = ExtraDataDriverName) and (Src^.dmDriverVersion = ExtraDataDriverVersion)) then
        exit;
        //raise Exception.Create('TJclPrintSet.DevModePrinterDriverExtraReinstate - Driver Private data does not match selected printer');

      NewHandle := GlobalAlloc(GHND, sizeof(DEVMODE) + Length(ExtraData));
      if NewHandle <> 0 then
        try
          Dest := GlobalLock(NewHandle);

          if (Src <> nil) and (Dest <> nil) then
          begin
            Move(Src^, Dest^, Src^.dmSize);
            Dest^.dmDriverExtra := 0;

            Dest^.dmDriverExtra := Length(ExtraData);


            ADeviceModeDriverExtra := PByte(Dest);
            Inc(ADeviceModeDriverExtra, Dest^.dmSize);
            Move(ExtraData[0], ADeviceModeDriverExtra^, dest^.dmDriverExtra);
          end
          else
            raise Exception.Create('TJclPrintSet.DevModePrinterDriverExtraReinstate - GlobalLock failed');
        finally
          GlobalUnlock(NewHandle);
        end;

      Printer.SetPrinter(FDevice, FDriver, FPort, NewHandle);
      FHandle := NewHandle;
      SetDeviceMode(False);
    finally
      GlobalUnlock(FHandle);
    end;
  end
  else
    raise Exception.Create('TJclPrintSet.DevModePrinterDriverExtraReinstate invalid handle');
end;

procedure TJclPrintSet.SaveToIniFile(const IniFileName, Section: string);
var
  PrIniFile: TMemIniFile;
begin
  PrIniFile := TMemIniFile.Create(IniFileName);   // use TMemIniFile as TIniFile truncats longs values
  try
    SaveToCustomIni(PrIniFile, Section);
    PrIniFile.UpdateFile;
  finally
    PrIniFile.Free;
  end;
end;

function TJclPrintSet.ReadFromIniFile(const IniFileName, Section: string): Boolean;
var
  PrIniFile: TMemIniFile;
begin
  PrIniFile := TMemIniFile.Create(IniFileName);     // use TMemIniFile as TIniFile truncats longs values
  try
    Result := ReadFromCustomIni(PrIniFile, Section);
  finally
    PrIniFile.Free;
  end;
end;

procedure TJclPrintSet.SetOrientation(Orientation: Integer);
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    ADeviceMode^.dmOrientation := Orientation;
    Printer.Orientation := TPrinterOrientation(Orientation - 1);
    ADeviceMode^.dmFields := ADeviceMode^.dmFields or DM_ORIENTATION;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetOrientation: Integer;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    Result := ADeviceMode^.dmOrientation;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetPaperSize(Size: Integer);
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    ADeviceMode^.dmPaperSize := Size;
    ADeviceMode^.dmFields := ADeviceMode^.dmFields or DM_PAPERSIZE;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetPaperSize: Integer;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    Result := ADeviceMode^.dmPaperSize;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetPaperLength(Length: Integer);
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    ADeviceMode^.dmPaperLength := Length;
    ADeviceMode^.dmFields := ADeviceMode^.dmFields or DM_PAPERLENGTH;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetPaperLength: Integer;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    Result := ADeviceMode^.dmPaperLength;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetPaperWidth(Width: Integer);
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    ADeviceMode^.dmPaperWidth := Width;
    ADeviceMode^.dmFields := ADeviceMode^.dmFields or DM_PAPERWIDTH;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetPaperWidth: Integer;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    Result := ADeviceMode^.dmPaperWidth;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetScale(Scale: Integer);
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    ADeviceMode^.dmScale := Scale;
    ADeviceMode^.dmFields := ADeviceMode^.dmFields or DM_SCALE;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetScale: Integer;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    Result := ADeviceMode^.dmScale;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetCopies(Copies: Integer);
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    ADeviceMode^.dmCopies := Copies;
    ADeviceMode^.dmFields := ADeviceMode^.dmFields or DM_COPIES;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetCopies: Integer;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    Result := ADeviceMode^.dmCopies;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetBin(Bin: Integer);
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    ADeviceMode^.dmDefaultSource := Bin;
    ADeviceMode^.dmFields := ADeviceMode^.dmFields or DM_DEFAULTSOURCE;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetBin: Integer;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    Result := ADeviceMode^.dmDefaultSource;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetPrintQuality(Quality: Integer);
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    ADeviceMode^.dmPrintQuality := Quality;
    ADeviceMode^.dmFields := ADeviceMode^.dmFields or DM_PRINTQUALITY;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetPrintQuality: Integer;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    Result := ADeviceMode^.dmPrintQuality;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetColor(Color: Integer);
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    ADeviceMode^.dmColor := Color;
    ADeviceMode^.dmFields := ADeviceMode^.dmFields or DM_COLOR;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetColor: Integer;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    Result := ADeviceMode^.dmColor;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetDuplex(Duplex: Integer);
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    ADeviceMode^.dmDuplex := Duplex;
    ADeviceMode^.dmFields := ADeviceMode^.dmFields or DM_DUPLEX;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetDuplex: Integer;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    Result := ADeviceMode^.dmDuplex;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetYResolution(YRes: Integer);
var
  PrintDevMode: PDeviceModeA;
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    PrintDevMode := @ADeviceMode^;
    PrintDevMode^.dmYResolution := YRes;
    ADeviceMode^.dmFields := ADeviceMode^.dmFields or DM_YRESOLUTION;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetYResolution: Integer;
var
  PrintDevMode: PDeviceModeA;
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    PrintDevMode := @ADeviceMode^;
    Result := PrintDevMode^.dmYResolution;
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetTrueTypeOption(Option: Integer);
var
  PrintDevMode: PDeviceModeA;
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    PrintDevMode := @ADeviceMode^;
    PrintDevMode^.dmTTOption := Option;
    ADeviceMode^.dmFields := ADeviceMode^.dmFields or DM_TTOPTION;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetTrueTypeOption: Integer;
var
  PrintDevMode: PDeviceModeA;
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    PrintDevMode := @ADeviceMode^;
    Result := PrintDevMode^.dmTTOption;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetPrinterName: string;
begin
  CheckPrinter;
  Result := StrPas(FDevice);
end;

function TJclPrintSet.GetPrinterPort: string;
begin
  CheckPrinter;
  Result := StrPas(FPort);
end;

function TJclPrintSet.GetPrinterDriver: string;
begin
  CheckPrinter;
  Result := StrPas(FDriver);
end;

procedure TJclPrintSet.SetBinFromList(BinNum: Word);
begin
  CheckPrinter;
  if FNumBins = 0 then
    Exit;
  if BinNum > FNumBins then
    raise EJclPrinterError.CreateRes(@RsIndexOutOfRange)
  else
    DefaultSource := FBinArray^[BinNum];
end;

function TJclPrintSet.GetBinIndex: Word;
var
  Idx: Word;
  ADeviceMode: PDeviceMode;
begin
  Result := 0;
  ADeviceMode := LockDeviceMode;
  try
    for Idx := 0 to FNumBins do
    begin
      if FBinArray^[Idx] = Word(ADeviceMode^.dmDefaultSource) then
      begin
        Result := Idx;
        Break;
      end;
    end;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetDevModePrinterDriverVersion: Word;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    Result := ADeviceMode^.dmDriverVersion;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetDevModePrinterDriver: string;
var
  ADeviceMode: PDeviceMode;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    Result := ADeviceMode^.dmDeviceName;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.GetDevModePrinterDriverExtra: TDynByteArray;
var
  ADeviceMode: PDeviceMode;
  ADeviceModeDriverExtra: PByte;
begin
  CheckPrinter;
  ADeviceMode := LockDeviceMode;
  try
    ADeviceModeDriverExtra := PByte(ADeviceMode);
    Inc(ADeviceModeDriverExtra, ADeviceMode^.dmSize);
    SetLength(Result, ADeviceMode^.dmDriverExtra);
    Move(ADeviceModeDriverExtra^, Result[0], ADeviceMode^.dmDriverExtra);
  finally
    UnlockDeviceMode;
  end;
end;

procedure TJclPrintSet.SetPaperFromList(PaperNum: Word);
begin
  CheckPrinter;
  if FNumPapers = 0 then
    Exit;
  if PaperNum > FNumPapers then
    raise EJclPrinterError.CreateRes(@RsIndexOutOfRangePaper)
  else
    PaperSize := FPaperArray^[PaperNum];
end;

procedure TJclPrintSet.SetPort(Port: string);
begin
  CheckPrinter;
  Port := Port + #0;
  Move(Port[1], FPort^, Length(Port));
  Printer.SetPrinter(FDevice, FDriver, FPort, FHandle);
end;

function TJclPrintSet.GetPaperIndex: Word;
var
  Idx: Word;
  ADeviceMode: PDeviceMode;
begin
  Result := 0;
  ADeviceMode := LockDeviceMode;
  try
    for Idx := 0 to FNumPapers do
    begin
      if FPaperArray^[Idx] = Word(ADeviceMode^.dmPaperSize) then
      begin
        Result := Idx;
        Break;
      end;
    end;
  finally
    UnlockDeviceMode;
  end;
end;

function TJclPrintSet.LockDeviceMode: PDeviceMode;
begin
  if FHandle <> 0 then
  begin
    Result := GlobalLock(FHandle);
    if not assigned(Result) then
      RaiseLastOSError;
  end
  else
    raise Exception.Create('TJclPrintSet.LockDeviceMode invalid FHandle');
end;

function TJclPrintSet.ReadFromCustomIni(const PrIniFile: TCustomIniFile; const Section: string): Boolean;
var
  privData: TMemoryStream;
  privDataExtra: TDynByteArray;
  privDataExtraSize: Integer;
  DevModeDriverName: string;
  DevModeDriverVersion: Word;
begin
  PrinterName := PrIniFile.ReadString(Section, PrintIniPrinterName, PrinterName);
  PrinterPort := PrIniFile.ReadString(Section, PrintIniPrinterPort, PrinterPort);
  Orientation := PrIniFile.ReadInteger(Section, PrintIniOrientation, Orientation);
  PaperSize := PrIniFile.ReadInteger(Section, PrintIniPaperSize, PaperSize);
  PaperLength := PrIniFile.ReadInteger(Section, PrintIniPaperLength, PaperLength);
  PaperWidth := PrIniFile.ReadInteger(Section, PrintIniPaperWidth, PaperWidth);
  Scale := PrIniFile.ReadInteger(Section, PrintIniScale, Scale);
  Copies := PrIniFile.ReadInteger(Section, PrintIniCopies, Copies);
  DefaultSource := PrIniFile.ReadInteger(Section, PrintIniDefaultSource, DefaultSource);
  PrintQuality := PrIniFile.ReadInteger(Section, PrintIniPrintQuality, PrintQuality);
  Color := PrIniFile.ReadInteger(Section, PrintIniColor, Color);
  Duplex := PrIniFile.ReadInteger(Section, PrintIniDuplex, Duplex);
  YResolution := PrIniFile.ReadInteger(Section, PrintIniYResolution, YResolution);
  TrueTypeOption := PrIniFile.ReadInteger(Section, PrintIniTTOption, TrueTypeOption);

  DevModeDriverName := PrIniFile.ReadString(Section, PrintDriverName, '');
  DevModeDriverVersion := Word(PrIniFile.ReadInteger(Section, PrintDriverVersion, 0));
  if (DevModePrinterDriver = DevModeDriverName) and
     (DevModePrinterDriverVersion = DevModeDriverVersion) then
  begin
    privData := TMemoryStream.Create;
    try

      PrIniFile.ReadBinaryStream(Section, PrintDriverExtraData, privData);
      privDataExtraSize := PrIniFile.ReadInteger(Section, PrintDriverExtraSize, 0);
      if (privData.Size = privDataExtraSize) then
      begin
        SetLength(privDataExtra, privDataExtraSize);
        privdata.Read(privDataExtra[0], privDataExtraSize);

        DevModePrinterDriverExtraReinstate(privDataExtra, DevModeDriverName, DevModeDriverVersion);
      end;
    finally
      privData.Free;
    end;
  end;
  Result := True;
end;

procedure TJclPrintSet.SaveToCustomIni(const PrIniFile: TCustomIniFile; const Section: string);
var
  CurrentName: string;
  
  privData: TMemoryStream;
  privDataExtra: TDynByteArray;
begin
    PrIniFile.EraseSection(Section);

    CurrentName := Printer.Printers[Printer.PrinterIndex];
    PrIniFile.WriteString(Section, PrintIniPrinterName, CurrentName);
    PrIniFile.WriteString(Section, PrintIniPrinterPort, PrinterPort);
    PrIniFile.WriteInteger(Section, PrintIniOrientation, Orientation);
    PrIniFile.WriteInteger(Section, PrintIniPaperSize, PaperSize);
    PrIniFile.WriteInteger(Section, PrintIniPaperLength, PaperLength);
    PrIniFile.WriteInteger(Section, PrintIniPaperWidth, PaperWidth);
    PrIniFile.WriteInteger(Section, PrintIniScale, Scale);
    PrIniFile.WriteInteger(Section, PrintIniCopies, Copies);
    PrIniFile.WriteInteger(Section, PrintIniDefaultSource, DefaultSource);
    PrIniFile.WriteInteger(Section, PrintIniPrintQuality, PrintQuality);
    PrIniFile.WriteInteger(Section, PrintIniColor, Color);
    PrIniFile.WriteInteger(Section, PrintIniDuplex, Duplex);
    PrIniFile.WriteInteger(Section, PrintIniYResolution, YResolution);
    PrIniFile.WriteInteger(Section, PrintIniTTOption, TrueTypeOption);

    PrIniFile.WriteString(Section, PrintDriverName, DevModePrinterDriver);
    PrIniFile.WriteInteger(Section, PrintDriverVersion, DevModePrinterDriverVersion);
    PrIniFile.WriteInteger(Section, PrintDriverExtraSize, Length(DevModePrinterDriverExtra));

    privDataExtra := DevModePrinterDriverExtra;

    privData := TMemoryStream.Create;
    try
      privdata.Write(privDataExtra[0], Length(privDataExtra));
      privData.Position := 0;
      PrIniFile.WriteBinaryStream(Section, PrintDriverExtraData, privData);
    finally
      privData.Free;
    end;
end;

procedure TJclPrintSet.SetPrinterName(const Value: string);
var
  NewIndex: Integer;
begin
  if PrinterName <> Value then
  begin
    NewIndex := Printer.Printers.IndexOf(Value);
    if NewIndex <> -1 then
    begin
      Printer.PrinterIndex := NewIndex;
    end;
  end;
  CheckPrinter;
end;

procedure TJclPrintSet.UnlockDeviceMode;
begin
 if FHandle <> 0 then
   GlobalUnLock(FHandle);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
