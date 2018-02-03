{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) - Delphi Tools                                                   }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is PeResource.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit PeResource;

{$I JCL.INC}

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics, ComCtrls, Contnrs,
  JclBase, JclFileUtils, JclPeImage, JclStrings;

type
  PAccelTableEntry = ^TAccelTableEntry;
  ACCELTABLEENTRY = packed record
    fFlags: Word;
    wAnsi: Word;
    wId: Word;
    padding: Word;
  end;  
  {$EXTERNALSYM ACCELTABLEENTRY}
  TAccelTableEntry = ACCELTABLEENTRY;

  PCursorDir = ^TCursorDir;
  CURSORDIR = packed record
    Width: Word;
    Height: Word;
  end;
  {$EXTERNALSYM CURSORDIR}
  TCursorDir = CURSORDIR;

  PCursorShape = ^TCursorShape;
  _CURSORSHAPE = packed record
    xHotSpot: Integer;
    yHotSpot: Integer;
    cx: Integer;
    cy: Integer;
    cbWidth: Integer;
    Planes: Byte;
    BitsPixel: Byte;
  end;
  {$EXTERNALSYM _CURSORSHAPE}
  TCursorShape = _CURSORSHAPE;
  CURSORSHAPE = _CURSORSHAPE;
  {$EXTERNALSYM CURSORSHAPE}

  PLocalHeader = ^TLocalHeader;
  _LOCALHEADER = packed record
    xHotSpot: Word;
    yHotSpot: Word;
  end;
  {$EXTERNALSYM _LOCALHEADER}
  TLocalHeader = _LOCALHEADER;
  LOCALHEADER = _LOCALHEADER;
  {$EXTERNALSYM LOCALHEADER}

  PNewHeader = ^TNewHeader;
  _NEWHEADER = packed record
    Reserved: Word;
    ResType: Word;
    ResCount: Word;
  end;
  {$EXTERNALSYM _NEWHEADER}
  TNewHeader = _NEWHEADER;
  NEWHEADER = _NEWHEADER;
  {$EXTERNALSYM NEWHEADER}

  PIconResdir = ^TIconResdir;
  ICONRESDIR = packed record
    Width: Byte;
    Height: Byte;
    ColorCount: Byte;
    Reserved: Byte;
  end;
  {$EXTERNALSYM ICONRESDIR}
  TIconResdir = ICONRESDIR;

  TResInfo = packed record
    case Integer of
      0: (Icon: TIconResdir);
      1: (Cursor: TCursorDir);
  end;
  {$NODEFINE TResInfo}

  PResDir = ^TResDir;
  _RESDIR = packed record
    ResInfo: TResInfo;
    Planes: Word;
    BitCount: Word;
    BytesInRes: DWORD;
    IconCursorId: Word;
  end;
  {$EXTERNALSYM _RESDIR}
  TResDir = _RESDIR;
  RESDIR = _RESDIR;
  {$EXTERNALSYM RESDIR}

  PDlgTemplate = ^TDlgTemplate;
  DLGTEMPLATE = packed record
    style: DWORD;
    dwExtendedStyle: DWORD;
    cdit: Word;
    x: ShortInt; // short
    y: ShortInt;
    cx: ShortInt;
    cy: ShortInt;
  end;
  {$EXTERNALSYM DLGTEMPLATE}
  TDlgTemplate = DLGTEMPLATE;

  PDlgItemTemplate = ^TDlgItemTemplate;
  DLGITEMTEMPLATE = packed record
    style: DWORD;
    dwExtendedStyle: DWORD;
    x: ShortInt;
    y: ShortInt;
    cx: ShortInt;
    cy: ShortInt;
    id: Word;
  end;
  {$EXTERNALSYM DLGITEMTEMPLATE}
  TDlgItemTemplate = DLGITEMTEMPLATE;

  PMenuHeader = ^TMenuHeader;
  MENUHEADER = packed record
    wVersion: Word;
    cbHeaderSize: Word;
  end;
  {$EXTERNALSYM MENUHEADER}
  TMenuHeader = MENUHEADER;

  PMenuHelpID = ^TMenuHelpID;
  MENUHELPID = packed record
    helpID: DWORD;
  end;
  {$EXTERNALSYM MENUHELPID}
  TMenuHelpID = MENUHELPID;

  PNormalMenuItem = ^TNormalMenuItem;
  NORMALMENUITEM = packed record
    resInfo: WORD;
    menuText: Pointer; // szOrOrd
  end;
  {$EXTERNALSYM NORMALMENUITEM}
  TNormalMenuItem = NORMALMENUITEM;

  PPopupMenuItem = ^TPopupMenuItem;
  POPUPMENUITEM = packed record
    type_: DWORD;
    state: DWORD;
    id: DWORD;
    resInfo: Word;
    menuText: Pointer; // szOrOrd
  end;
  {$EXTERNALSYM POPUPMENUITEM}
  TPopupMenuItem = POPUPMENUITEM;

  PMenuExTemplateHeader = ^TMenuExTemplateHeader;
  MENUEX_TEMPLATE_HEADER = packed record
    wVersion: Word;
    wOffset: Word;
    dwHelpId: DWORD;
  end;
  {$EXTERNALSYM MENUEX_TEMPLATE_HEADER}
  TMenuExTemplateHeader = MENUEX_TEMPLATE_HEADER;

  PMenuExTemplateItem = ^TMenuExTemplateItem;
  MENUEX_TEMPLATE_ITEM = packed record
    dwType: DWORD;
    dwState: DWORD;
    uId: UINT;
    bResInfo: Word;
    szText: array[0..0] of WideChar;
    dwHelpId: DWORD;
  end;
  {$EXTERNALSYM MENUEX_TEMPLATE_ITEM}
  TMenuExTemplateItem = MENUEX_TEMPLATE_ITEM;

  PMessageResourceBlock = ^TMessageResourceBlock;
  _MESSAGE_RESOURCE_BLOCK = packed record
    LowId: ULONG;
    HighId: ULONG;
    OffsetToEntries: ULONG;
  end;
  {$EXTERNALSYM _MESSAGE_RESOURCE_BLOCK}
  TMessageResourceBlock = _MESSAGE_RESOURCE_BLOCK;
  MESSAGE_RESOURCE_BLOCK = _MESSAGE_RESOURCE_BLOCK;
  {$EXTERNALSYM MESSAGE_RESOURCE_BLOCK}

  PMessageResourceData = ^TMessageResourceData;
  _MESSAGE_RESOURCE_DATA = packed record
    NumberOfBlocks: ULONG;
    // Blocks: array[0..0] of TMessageResourceBlock;
  end;
  {$EXTERNALSYM _MESSAGE_RESOURCE_DATA}
  TMessageResourceData = _MESSAGE_RESOURCE_DATA;
  MESSAGE_RESOURCE_DATA = _MESSAGE_RESOURCE_DATA;
  {$EXTERNALSYM MESSAGE_RESOURCE_DATA}

  PMessageResourceEntry = ^TMessageResourceEntry;
  _MESSAGE_RESOURCE_ENTRY = packed record
    Length: Word;
    Flags: Word;
    // Text: array[0..0] of Char;
  end;
  {$EXTERNALSYM _MESSAGE_RESOURCE_ENTRY}
  TMessageResourceEntry = _MESSAGE_RESOURCE_ENTRY;
  MESSAGE_RESOURCE_ENTRY = _MESSAGE_RESOURCE_ENTRY;
  {$EXTERNALSYM MESSAGE_RESOURCE_ENTRY}

(*

Value Meaning
0x0080 Button
0x0081 Edit
0x0082 Static
0x0083 List box
0x0084 Scroll bar
0x0085 Combo box}

  PDlgTemplateEx = ^TDlgTemplateEx;
  DLGTEMPLATEEX = packed record
    dlgVer: WORD;
    signature: WORD;
    helpID: DWORD;
    exStyle: DWORD;
    style: DWORD;
    cDlgItems: WORD;
    x: short;
    y: short;
    cx: short;
    cy: short;
    sz_Or_Ord menu;         // name or ordinal of a menu resource
    sz_Or_Ord windowClass;  // name or ordinal of a window class
    WCHAR  title[titleLen]; // title string of the dialog box
    short  pointsize;       // if DS_SETFONT or DS_SHELLFONT is set
    short  weight;          // if DS_SETFONT or DS_SHELLFONT is set
    short  bItalic;         // if DS_SETFONT or DS_SHELLFONT is set
    WCHAR  font[fontLen];   // if DS_SETFONT or DS_SHELLFONT is set
} DLGTEMPLATEEX;


 typedef struct {
    DWORD  helpID;
    DWORD  exStyle;
    DWORD  style;
    short  x;
    short  y;
    short  cx;
    short  cy;
    WORD   id;
    sz_Or_Ord windowClass; // name or ordinal of a window class
    sz_Or_Ord title;       // title string or ordinal of a resource
    WORD   extraCount;     // bytes of following creation data
} DLGITEMTEMPLATEEX;

struct FONTDIRENTRY {
    WORD dfVersion;
    DWORD dfSize;
    char dfCopyright[60];
    WORD dfType;
    WORD dfPoints;
    WORD dfVertRes;
    WORD dfHorizRes;
    WORD dfAscent;
    WORD dfInternalLeading;
    WORD dfExternalLeading;
    BYTE dfItalic;
    BYTE dfUnderline;
    BYTE dfStrikeOut;
    WORD dfWeight;
    BYTE dfCharSet;
    WORD dfPixWidth;
    WORD dfPixHeight;
    BYTE dfPitchAndFamily;
    WORD dfAvgWidth;
    WORD dfMaxWidth;
    BYTE dfFirstChar;
    BYTE dfLastChar;
    BYTE dfDefaultChar;
    BYTE dfBreakChar;
    WORD dfWidthBytes;
    DWORD dfDevice;
    DWORD dfFace;
    DWORD dfReserved;
    char szDeviceName[];
    char szFaceName[];
};

struct FONTGROUPHDR {
    WORD NumberOfFonts;
    DIRENTRY  DE [1];
};

*)

type
  TPeResKind = (rkAccelerator, rkAvi, rkBitmap, rkCursor, rkData, rkDialog,
    rkHTML, rkIcon, rkMenu, rkMessageTable, rkString, rkVersion, rkUnknown);

  TPeResImage = class;

  TPeResItem = class;

  TPeResItem = class(TPersistent)
  private
    FKind: TPeResKind;
    FList: TObjectList;
    FResImage: TPeResImage;
    FResourceItem: TJclPeResourceItem;
    FStream: TJclPeResourceRawStream;
    function GetItems(Index: Integer): TPeResItem;
    function GetItemCount: Integer;
    function GetStream: TJclPeResourceRawStream;
  protected
    procedure CreateList; virtual;
  public
    constructor Create(AResImage: TPeResImage; AResourceItem: TJclPeResourceItem); virtual;
    destructor Destroy; override;
    function IsList: Boolean; virtual;
    function Offset: Integer;
    function RawData: Pointer;
    function ResName: string; virtual;
    function ResType: TJclPeResourceKind;
    procedure SaveToStream(Stream: TStream); virtual;
    function Size: Integer;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TPeResItem read GetItems; default;
    property Kind: TPeResKind read FKind;
    property ResourceItem: TJclPeResourceItem read FResourceItem;
    property Stream: TJclPeResourceRawStream read GetStream;
  end;

  TJclReResItemClass = class of TPeResItem;

  TPeResUnknown = class(TPeResItem)
  public
    function FileExt: string; dynamic;
    function IsList: Boolean; override;
    function ResName: string; override;
  end;

  TPeGraphicProperties = record
    Width, Height, BitsPerPixel: Integer;
  end;

  TPeResUnkGraphic = class(TPeResUnknown)
  public
    function GraphicProperties: TPeGraphicProperties; virtual; abstract;
  end;

  TPeResUnkStrings = class(TPeResUnknown)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    function FileExt: string; override;
    procedure FillStrings(Strings: TStrings; StripCrLf: Boolean = False); virtual; abstract;
  end;

  TPeResAccelerator = class(TPeResUnkStrings)
  public
    procedure FillStrings(Strings: TStrings; StripCrLf: Boolean = False); override;
  end;

  TPeResAvi = class(TPeResUnknown)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    function FileExt: string; override;
  end;

  TPeResBitmap = class(TPeResUnkGraphic)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    function GraphicProperties: TPeGraphicProperties; override;
    function FileExt: string; override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TPeResCursorItem = class(TPeResUnkGraphic)
  private
    FResInfo: PResDir;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    function FileExt: string; override;
    function GraphicProperties: TPeGraphicProperties; override;
    function ResName: string; override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TPeResCursor = class(TPeResUnknown)
  private
    function GetItems(Index: Integer): TPeResCursorItem;
  protected
    procedure CreateList; override;
  public
    function IsList: Boolean; override;
    property Items[Index: Integer]: TPeResCursorItem read GetItems; default;
  end;

  TPeResDialog = class(TPeResUnknown)
  public
    function CanShowDialog: Boolean;
    function ShowDialog(ParentWnd: HWND): Integer;
  end;

  TPeResDataKind = (dkUnknown, dkDFM, dkPackageDescription, dkPackageInfo);

  TPeResRCData = class(TPeResUnknown)
  private
    FDataKind: TPeResDataKind;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CheckFormat;
    procedure DFMToStrings(Strings: TStrings);
    procedure PackageInfoToStrings(Strings: TStrings);
  public
    constructor Create(AResImage: TPeResImage; AResourceItem: TJclPeResourceItem); override;
    function FileExt: string; override;
    property DataKind: TPeResDataKind read FDataKind;
  end;

  TPeResHTML = class(TPeResUnknown)
  public
    function FileExt: string; override;
    function ResPath: string;
  end;

  TPeResIconItem = class(TPeResCursorItem)
  public
    function FileExt: string; override;
    function GraphicProperties: TPeGraphicProperties; override;
  end;

  TPeResIcon = class(TPeResCursor)
  private
    function GetItems(Index: Integer): TPeResIconItem;
  public
    property Items[Index: Integer]: TPeResIconItem read GetItems; default;
  end;

  TPeResMenu = class(TPeResUnknown)
  end;

  TPeMessageTable = class(TPeResUnkStrings)
  public
    procedure FillStrings(Strings: TStrings; StripCrLf: Boolean = False); override;
  end;

  TPeResString = class(TPeResUnkStrings)
  public
    procedure FillStrings(Strings: TStrings; StripCrLf: Boolean = False); override;
  end;

  TPeResVersion = class(TPeResUnkStrings)
  public
    procedure FillStrings(Strings: TStrings; StripCrLf: Boolean = False); override;
  end;

  TPeResImage = class(TObjectList)
  private
    FCursorEntry: TJclPeResourceList;
    FIconEntry: TJclPeResourceList;
    FImageAttached: Boolean;
    FLibHandle: THandle;
    FPeImage: TJclPeImage;
    function GetFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
    procedure SetPeImage(const Value: TJclPeImage);
    function GetItems(Index: Integer): TPeResItem;
    function GetLibHandle: THandle;
  protected
    procedure CreateList;
    procedure UnloadLib;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    property ImageAttached: Boolean read FImageAttached;
    property Items[Index: Integer]: TPeResItem read GetItems; default;
    property LibHandle: THandle read GetLibHandle;
    property FileName: TFileName read GetFileName write SetFileName;
    property PeImage: TJclPeImage read FPeImage write SetPeImage;
  end;

  function LangNameFromName(const Name: string; ShortName: Boolean = False): string;

implementation

uses
  Consts, JclLocales, JclSysUtils, JclWin32;

resourcestring
  RsPeResAccelerator = 'Accel table';
  RsPeResAVI = 'AVI';
  RsPeResBitmap = 'Bitmap';
  RsPeResCursor = 'Cursor';
  RsPeResData = 'RCData';
  RsPeResDialog = 'Dialog';
  RsPeResHTML = 'HTML';
  RsPeResIcon = 'Icon';
  RsPeResMenu = 'Menu';
  RsPeResMessageTable = 'Message table';
  RsPeResString = 'String';
  RsPeResVersion = 'Version';
  RsNeutralLang = '[Neutral]';
  RsUnknownLang = '[Unknown]';

  RsTranslations = 'Translations:';

var
  JclLocalesList: TJclLocalesList;

function VirtualKeyNameFromCode(KeyCode: Byte): string;
const
  KN002F: array[$00..$2F] of PChar = (
    nil,
    'LBUTTON',
    'RBUTTON',
    'CANCEL',
    'MBUTTON',
    nil, nil, nil, // 05..07
    'BACK',
    'TAB',
    nil, nil, // 0A..0B
    'CLEAR',
    'RETURN',
    nil, nil, // 0E..0F
    'SHIFT ',
    'CONTROL',
    'MENU',
    'PAUSE',
    'CAPITAL',
    'KANA',
    'HANGUL',
    'JUNJA',
    'FINAL',
    'HANJA',
    'KANJI',
    'ESCAPE',
    'CONVERT',
    'NONCONVERT',
    'ACCEPT',
    'MODECHANGE',
    'SPACE',
    'PRIOR',
    'NEXT',
    'END',
    'HOME',
    'LEFT',
    'UP',
    'RIGHT',
    'DOWN',
    'SELECT',
    'PRINT',
    'EXECUTE',
    'SNAPSHOT',
    'INSERT',
    'DELETE',
    'HELP'
    );
  KN5B5D: array[$5B..$5D] of PChar = (
    'LWIN',
    'RWIN',
    'APPS'
    );
  KN6A6F: array[$6A..$6F] of PChar = (
    'MULTIPLY',
    'ADD',
    'SEPARATOR',
    'SUBTRACT',
    'DECIMAL',
    'DIVIDE'
    );
  KNA0A5: array[$A0..$A5] of PChar = (
    'LSHIFT',
    'RSHIFT',
    'LCONTROL',
    'RCONTROL',
    'LMENU',
    'RMENU'
    );
  KNF6FE: array[$F6..$FE] of PChar = (
    'ATTN',
    'CRSEL',
    'EXSEL',
    'EREOF',
    'PLAY',
    'ZOOM',
    'NONAME',
    'PA1',
    'OEM_CLEAR'
    );
begin
  case KeyCode of
    $00..$2F:
      Result := KN002F[KeyCode];
    $30..$39, $41..$5A:
      Result := Chr(KeyCode);
    $5B..$5D:
      Result := KN5B5D[KeyCode];
    $60..$69:
      Result := Format('NUMPAD%d', [KeyCode - $60]);
    $6A..$6F:
      Result := KN6A6F[KeyCode];
    $70..$87:
      Result := Format('F%d', [KeyCode - $6F]);
    $90:
      Result := 'NUMLOCK';
    $91:
      Result := 'SCROLL';
    $A0..$A5:
      Result := KNA0A5[KeyCode];
    $E5:
      Result := 'PROCESSKEY';
    $F6..$FE:
      Result := KNF6FE[KeyCode];
  else
    Result := '';
  end;
  if Result <> '' then Result := 'VK_' + Result;
end;

function LangNameFromName(const Name: string; ShortName: Boolean): string;
var
  LangID: Word;
  Locale: TJclLocaleInfo;
begin
  LangID := PRIMARYLANGID(StrToIntDef(Name, 0));
  if LangID = LANG_NEUTRAL then
    if ShortName then Result := '' else Result := RsNeutralLang
  else
  begin
    Locale := JclLocalesList.ItemFromLangIDPrimary[LangID];
    if Locale <> nil then
      with Locale do if ShortName then
        Result := AbbreviatedLangName else Result := EnglishLangName
    else
      Result := RsUnknownLang;
  end;
end;


function GetResItemKind(Item: TJclPeResourceItem; var Kind: TPeResKind): Boolean;
begin
  Result := True;
  Kind := rkUnknown;
  with Item do
    case ResourceType of
      rtAccelerators:
        Kind := rkAccelerator;
      rtCursorEntry, rtIconEntry, rtFont:
        Result := False;
      rtUserDefined:
        begin
          if Name = 'AVI' then Kind := rkAvi;
          if Name = '2110' then Kind := rkHTML;
        end;
      rtBitmap:
        Kind := rkBitmap;
      rtMenu:
        Kind := rkMenu;
      rtDialog:
        Kind := rkDialog;
      rtString:
        Kind := rkString;
      rtRCData:
        Kind := rkData;
      rtMessageTable:
        Kind := rkMessageTable;
      rtCursor:
        Kind := rkCursor;
      rtIcon:
        Kind := rkIcon;
      rtVersion:
        Kind := rkVersion;
      rtHmtl:
        Kind := rkHTML;
    end;
end;

const
  ResItemClasses: array [TPeResKind] of TJclReResItemClass = (
    TPeResAccelerator,
    TPeResAvi,
    TPeResBitmap,
    TPeResCursor,
    TPeResRCData,
    TPeResDialog,
    TPeResHTML,
    TPeResIcon,
    TPeResMenu,
    TPeMessageTable,
    TPeResString,
    TPeResVersion,
    TPeResUnknown
    );

function WideCharToStr(WStr: PWChar; Len: Integer): string;
begin
  {$IFDEF SUPPORTS_UNICODE}
  SetLength(Result, Len);
  if Len > 0 then
    Move(WStr^, Result[1], Len * SizeOf(WideChar));
  {$ELSE SUPPORTS_UNICODE}
  if Len = 0 then Len := -1;
  Len := WideCharToMultiByte(CP_ACP, 0, WStr, Len, nil, 0, nil, nil);
  SetLength(Result, Len);
  WideCharToMultiByte(CP_ACP, 0, WStr, Len, PChar(Result), Len, nil, nil);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

{ TPeResItem }

constructor TPeResItem.Create(AResImage: TPeResImage; AResourceItem: TJclPeResourceItem);
begin
  FList := TObjectList.Create(True);
  FResImage := AResImage;
  FResourceItem := AResourceItem;
end;

procedure TPeResItem.CreateList;
var
  I, J: Integer;
  Item: TPeResItem;
  ResItem: TJclPeResourceItem;
begin
  with FResourceItem.List do
    for I := 0 to Count - 1 do
    begin
      ResItem := Items[I];
      for J := 0 to ResItem.List.Count - 1 do
      begin
        Item := ResItemClasses[Self.FKind].Create(FResImage, ResItem.List[J]);
        Item.FKind := Self.FKind;
        FList.Add(Item);
      end;
    end;
end;

destructor TPeResItem.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FStream);
  inherited;
end;

function TPeResItem.GetItemCount: Integer;
begin
  if IsList then
  begin
    if FList.Count = 0 then CreateList;
    Result := FList.Count;
  end else
    Result := -1;
end;

function TPeResItem.GetItems(Index: Integer): TPeResItem;
begin
  Result := TPeResItem(FList[Index]);
end;

function TPeResItem.GetStream: TJclPeResourceRawStream;
begin
  if not Assigned(FStream) then
    FStream := TJclPeResourceRawStream.Create(FResourceItem);
  Result := FStream;
end;

function TPeResItem.IsList: Boolean;
begin
  Result := FResourceItem.IsDirectory;
end;

function TPeResItem.Offset: Integer;
begin
  if IsList then
    Result := FResourceItem.Entry^.OffsetToData and not (IMAGE_RESOURCE_DATA_IS_DIRECTORY)
  else
    Result := FResourceItem.DataEntry^.OffsetToData
end;

function TPeResItem.RawData: Pointer;
begin
  Result := FResourceItem.RawEntryData;
end;

function TPeResItem.ResName: string;
const
  ResNames: array [TPeResKind] of PResStringRec = (
    @RsPeResAccelerator,
    @RsPeResAVI,
    @RsPeResBitmap,
    @RsPeResCursor,
    @RsPeResData,
    @RsPeResDialog,
    @RsPeResHTML,
    @RsPeResIcon,
    @RsPeResMenu,
    @RsPeResMessageTable,
    @RsPeResString,
    @RsPeResVersion,
    nil
    );
begin
  if FKind = rkUnknown then
    Result := FResourceItem.ResourceTypeStr
  else
    Result := LoadResString(ResNames[FKind]);
end;

function TPeResItem.ResType: TJclPeResourceKind;
begin
  Result := FResourceItem.ResourceType;
end;

procedure TPeResItem.SaveToStream(Stream: TStream);
begin
  if not IsList then
    Stream.WriteBuffer(RawData^, Size);
end;

function TPeResItem.Size: Integer;
begin
  if IsList then
    Result := 0
  else
    Result := FResourceItem.DataEntry^.Size;
end;

{ TPeResUnknown }

function TPeResUnknown.FileExt: string;
begin
  Result := 'bin';
end;

function TPeResUnknown.IsList: Boolean;
begin
  Result := False;
end;

function TPeResUnknown.ResName: string;
begin
  if StrToIntDef(FResourceItem.Name, 0) = LANG_NEUTRAL then
    Result := FResourceItem.ParentItem.Name
  else
    Result := Format('%s > %s', [FResourceItem.ParentItem.Name, LangNameFromName(FResourceItem.Name)]);
end;

{ TPeResUnkStrings }

procedure TPeResUnkStrings.AssignTo(Dest: TPersistent);
begin
  if (Dest is TStrings) then
    with TStrings(Dest) do
    begin
      BeginUpdate;
      try
        Clear;
        FillStrings(TStrings(Dest));
      finally
        EndUpdate;
      end;
    end
  else
    inherited;
end;

function TPeResUnkStrings.FileExt: string;
begin
  Result := 'txt';
end;

{ TPeResAccelTable }

procedure TPeResAccelerator.FillStrings(Strings: TStrings; StripCrLf: Boolean);
var
  TableEntry: PAccelTableEntry;
  IsLast: Boolean;
  S: string;

  function AnsiToChar(A: Word): string;
  begin
    if A >= 32 then Result := Chr(A) else Result := '';
  end;

begin
  Strings.BeginUpdate;
  try
    TableEntry := RawData;
    repeat
      with TableEntry^ do
      begin
        IsLast := fFlags and $80 <> 0;
        if fFlags and FVIRTKEY <> 0 then
        begin
          S := Format('Virtual Key: %.2u "%s" ', [wAnsi, VirtualKeyNameFromCode(wAnsi)]);
          if fFlags and FSHIFT <> 0 then S := S + 'SHIFT ';
          if fFlags and FCONTROL <> 0 then S := S + 'CTRL ';
          if fFlags and FALT <> 0 then S := S + 'ALT ';
        end else
          S := Format('ANSI character: %.2u "%s" ', [wAnsi, AnsiToChar(wAnsi)]);
        if fFlags and FNOINVERT <> 0 then S := S + 'NOINVERT';
      end;
      Strings.Add(TrimRight(S));
      Inc(TableEntry);
    until IsLast;
  finally
    Strings.EndUpdate;
  end;    
end;

{ TPeResAvi }

{$HINTS OFF}
type
  TDirtyComponent = class(TPersistent)
  private
    FOwner: TComponent;
    FName: TComponentName;
    FTag: Longint;
    FComponents: TList;
    FFreeNotifies: TList;
    FDesignInfo: Longint;
    FVCLComObject: Pointer;
    FComponentState: TComponentState;
  end;
{$HINTS ON}

procedure TPeResAvi.AssignTo(Dest: TPersistent);
begin
  if Dest is TAnimate then
  begin
    Include(TDirtyComponent(Dest).FComponentState, csLoading);
    TAnimate(Dest).ResHandle := FResImage.LibHandle;
    TAnimate(Dest).ResName := FResourceItem.ParentItem.ParameterName;
    Exclude(TDirtyComponent(Dest).FComponentState, csLoading);
    TAnimate(Dest).Reset;
  end
  else
    inherited;
end;

function TPeResAvi.FileExt: string;
begin
  Result := 'avi';
end;

{ TPeResBitmap }

procedure TPeResBitmap.AssignTo(Dest: TPersistent);
var
  MemStream: TMemoryStream;
  BitMap: TBitMap;
begin
  if Dest is TPicture then
  begin
    BitMap := TPicture(Dest).Bitmap;
    MemStream := TMemoryStream.Create;
    try
      SaveToStream(MemStream);
      MemStream.Seek(0, soFromBeginning);
      BitMap.LoadFromStream(MemStream);
    finally
      MemStream.Free;
    end
  end
  else
    inherited;
end;

function TPeResBitmap.FileExt: string;
begin
  Result := 'bmp';
end;

function TPeResBitmap.GraphicProperties: TPeGraphicProperties;
var
  BI: PBitmapInfoHeader;
  BC: PBitmapCoreHeader;
begin
  BI := PBitmapInfoHeader(RawData);
  if BI.biSize = SizeOf(TBitmapInfoHeader) then
  begin
    Result.Width := BI.biWidth;
    Result.Height := BI.biHeight;
    Result.BitsPerPixel := BI.biPlanes * BI.biBitCount;
  end else
  begin
    BC := PBitmapCoreHeader(RawData);
    Result.Width := BC.bcWidth;
    Result.Height := BC.bcHeight;
    Result.BitsPerPixel := BC.bcPlanes * BC.bcBitCount;
  end;
end;

procedure TPeResBitmap.SaveToStream(Stream: TStream);

  function GetDInColors(BitCount: Word): Integer;
  begin
    case BitCount of
      1, 4, 8: Result := 1 shl BitCount;
    else
      Result := 0;
    end;
  end;

var
  BH: TBitmapFileHeader;
  BI: PBitmapInfoHeader;
  BC: PBitmapCoreHeader;
  ClrUsed: Integer;
begin
  FillChar(BH, sizeof(BH), #0);
  BH.bfType := $4D42;
  BH.bfSize := Size + SizeOf(BH);
  BI := PBitmapInfoHeader(RawData);
  if BI.biSize = SizeOf(TBitmapInfoHeader) then
  begin
    ClrUsed := BI.biClrUsed;
    if ClrUsed = 0 then ClrUsed := GetDInColors(BI.biBitCount);
    BH.bfOffBits :=  ClrUsed * SizeOf(TRgbQuad) + SizeOf(TBitmapInfoHeader) + SizeOf(BH);
  end
  else
  begin
    BC := PBitmapCoreHeader(RawData);
    ClrUsed := GetDInColors(BC.bcBitCount);
    BH.bfOffBits :=  ClrUsed * SizeOf(TRGBTriple) + SizeOf(TBitmapCoreHeader) + SizeOf(BH);
  end;
  Stream.Write(BH, SizeOf(BH));
  Stream.Write(RawData^, Size);
end;

{ TPeResCursorItem }

procedure TPeResCursorItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TPicture then
    TPicture(Dest).Icon.Handle := CreateIconFromResource(RawData, Size, ResType = rtIconEntry, $30000)
  else
    inherited;
end;

function TPeResCursorItem.FileExt: string;
begin
  Result := 'cur';
end;

function TPeResCursorItem.GraphicProperties: TPeGraphicProperties;
begin
  with FResInfo^ do
  begin
    Result.Width := ResInfo.Cursor.Width;
    Result.Height := ResInfo.Cursor.Height;
    Result.BitsPerPixel := BitCount * Planes;
  end;
end;

function TPeResCursorItem.ResName: string;
begin
  if FResInfo <> nil then
    with GraphicProperties do
      Result := Format('%d X %d %d bpp', [Width, Height, BitsPerPixel])
  else
    Result := '';
end;

procedure TPeResCursorItem.SaveToStream(Stream: TStream);
begin
  with TIcon.Create do
  try
    Handle := CreateIconFromResource(RawData, Self.Size, ResType = rtIconEntry, $30000);
    SaveToStream(Stream);
  finally
    Free;
  end;
end;
{ TODO : Saving monochrome icons and cursors doesn't work }

{ TPeResCursor }

procedure TPeResCursor.CreateList;
var
  Item: TPeResItem;
  I, J, Cnt: Integer;
  ResData: PResDir;
  ResOrd: DWORD;
  ResList: TJclPeResourceList;
  ItemClass: TJclReResItemClass;
begin
  if ResType = rtCursor then
  begin
    ResList := FResImage.FCursorEntry;
    ItemClass := TPeResCursorItem;
  end else
  begin
    ResList := FResImage.FIconEntry;
    ItemClass := TPeResIconItem;
  end;
  ResData := RawData;
  Cnt := PNewHeader(ResData)^.ResCount;
  Inc(PNewHeader(ResData));
  for I := 1 to Cnt do
  begin
    ResOrd := ResData^.IconCursorId;
    for J := 0 to ResList.Count - 1 do
      if ResOrd = ResList[J].Entry^.Name then
      begin
        Item := ItemClass.Create(FResImage, ResList[J].List[0]);
        Item.FKind := Self.FKind;
        TPeResCursorItem(Item).FResInfo := ResData;
        FList.Add(Item);
      end;
    Inc(ResData);
  end;
end;

function TPeResCursor.GetItems(Index: Integer): TPeResCursorItem;
begin
  Result := TPeResCursorItem(FList[Index]);
end;

function TPeResCursor.IsList: Boolean;
begin
  Result := True;
end;

{ TPeResRCData }

procedure TPeResRCData.AssignTo(Dest: TPersistent);
begin
  if Dest is TStrings then
    with TStrings(Dest) do
    begin
      BeginUpdate;
      try
        Clear;
        case FDataKind of
          dkDFM:
            DFMToStrings(TStrings(Dest));
          dkPackageDescription:
            Add(PWideChar(RawData));
          dkPackageInfo:
            PackageInfoToStrings(TStrings(Dest));
        end;    
      finally
        EndUpdate;
      end;
  end else
    inherited;
end;

procedure TPeResRCData.CheckFormat;
{$IFNDEF DELPHI5_UP}
const
  FilerSignature: array[1..4] of Char = 'TPF0';
var
  Signature: Integer;
{$ENDIF DELPHI5_UP}
begin
  FDataKind := dkUnknown;
  if ResName = 'DESCRIPTION' then
    FDataKind := dkPackageDescription
  else
  if ResName = 'PACKAGEINFO' then
    FDataKind := dkPackageInfo
  else
  begin
    Stream.Seek(0, soFromBeginning);
    {$IFDEF DELPHI5_UP}
    if TestStreamFormat(Stream) = sofBinary then
      FDataKind := dkDFM;
    {$ELSE DELPHI5_UP}
    Signature := 0;
    Stream.Read(Signature, SizeOf(Signature));
    if (Byte(Signature) = $FF) or (Signature = Integer(FilerSignature)) then
      FDataKind := dkDFM;
    {$ENDIF DELPHI5_UP}
  end;
end;

constructor TPeResRCData.Create(AResImage: TPeResImage;
  AResourceItem: TJclPeResourceItem);
begin
  inherited;
  CheckFormat;
end;

procedure TPeResRCData.DFMToStrings(Strings: TStrings);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    Stream.Seek(0, soFromBeginning);
    ObjectBinaryToText(Stream, MemStream);
    MemStream.Seek(0, soFromBeginning);
    Strings.LoadFromStream(MemStream);
  finally
    MemStream.Free;
  end;
end;

function TPeResRCData.FileExt: string;
begin
  if DataKind = dkDFM then
    Result := 'dfm'
  else
    Result := inherited FileExt;
end;

procedure TPeResRCData.PackageInfoToStrings(Strings: TStrings);
var
  I: Integer;
begin
  with TJclPePackageInfo.Create(FResImage.LibHandle) do
  try
    Strings.Add('Contains');
    Strings.Add(StringOfChar('-', 80));
    for I := 0 to ContainsCount - 1 do
      Strings.Add(Format('  %s [%s]', [ContainsNames[I], UnitInfoFlagsToString(ContainsFlags[I])]));
    if RequiresCount > 0 then
    begin
      Strings.Add('');
      Strings.Add('Requires');
      Strings.Add(StringOfChar('-', 80));
      for I := 0 to RequiresCount - 1 do
        Strings.Add(Format('  %s', [RequiresNames[I]]));
    end;    
    Strings.Add('');
    Strings.Add('Package Info flags');
    Strings.Add(StringOfChar('-', 80));
    Strings.Add(Format('Options    : %s', [PackageOptionsToString(Flags)]));
    Strings.Add(Format('Module type: %s', [PackageModuleTypeToString(Flags)]));
    Strings.Add(Format('Producer   : %s', [ProducerToString(Flags)]));
  finally
    Free;
  end;
end;

{ TPeResDialog }

function TPeResDialog.CanShowDialog: Boolean;
begin
  Result := Windows.PDlgTemplate(RawData)^.style and DS_CONTROL = 0;
end;

function TPeResDialog.ShowDialog(ParentWnd: HWND): Integer;
var
  LastFocus: HWND;
  MemHandle: THandle;
  P: Windows.PDlgTemplate;

  function DialogProc(hwndDlg: HWND; uMsg: UINT; W: WPARAM; L: LPARAM): BOOL; stdcall;
  begin
    Result := False;
    case uMsg of
      WM_INITDIALOG:
         Result := True;
      WM_LBUTTONDBLCLK:
        EndDialog(hwndDlg, 0);
      WM_RBUTTONUP:
        EndDialog(hwndDlg, 1);
      WM_SYSCOMMAND:
        if W and $FFF0 = SC_CLOSE then
          EndDialog(hwndDlg, 0);
    end;
  end;

begin
  LastFocus := GetFocus;
  MemHandle := GlobalAlloc(GMEM_ZEROINIT, Size);
  P := GlobalLock(MemHandle);
  Move(RawData^, P^, Size);
  GlobalUnlock(MemHandle);
  Result := DialogBoxIndirect(hinstance, Windows.PDlgTemplate(MemHandle)^,
    ParentWnd, @DialogProc);
  GlobalFree(MemHandle);
  SetFocus(LastFocus);
end;

{ TPeResHTML }

function TPeResHTML.FileExt: string;
begin
  Result := Copy(ExtractFileExt(FResourceItem.ParentItem.ParameterName), 2, 20);
end;

function TPeResHTML.ResPath: string;
begin
  Result := Format('res://%s/%s', [FResImage.FileName, FResourceItem.ParentItem.ParameterName]);
end;

{ TPeResIconItem }

function TPeResIconItem.FileExt: string;
begin
  Result := 'ico';
end;

function TPeResIconItem.GraphicProperties: TPeGraphicProperties;
begin
  with FResInfo^ do
  begin
    Result.Width := ResInfo.Icon.Width;
    Result.Height := ResInfo.Icon.Height;
    Result.BitsPerPixel := BitCount * Planes;
  end;
end;

{ TPeResIcon }

function TPeResIcon.GetItems(Index: Integer): TPeResIconItem;
begin
  Result := TPeResIconItem(FList[Index]);
end;

{ TPeMessageTable }

procedure TPeMessageTable.FillStrings(Strings: TStrings; StripCrLf: Boolean);
var
  Count, I: Integer;
  E: DWORD;
  Block: PMessageResourceBlock;
  Entry: PMessageResourceEntry;
  S: string;
  Text: PChar;
  Data: Pointer;
begin
  Data := RawData;
  Count := PMessageResourceData(Data)^.NumberOfBlocks;
  Block := Data;
  Inc(PMessageResourceData(Block));
  for I := 1 to Count do
  begin
    Entry := PMessageResourceEntry(DWORD(Data) + Block^.OffsetToEntries);
    for E := Block^.LowId to Block^.HighId do
    begin
      with Entry^ do
      begin
        Text := PChar(Entry) + Sizeof(TMessageResourceEntry);
        if Flags = 1 then
          S := WideCharToStr(PWideChar(Text), lstrlenW(PWideChar(Text)))
        else
          SetString(S, PAnsiChar(Text), StrLen(Text));
        if StripCrLf then S := StrRemoveChars(S, CharIsReturn);
        Strings.AddObject(S, Pointer(E));
      end;
      Entry := Pointer(PChar(Entry) + Entry^.Length);
    end;
    Inc(Block);
  end;
end;

{ TPeResString }

procedure TPeResString.FillStrings(Strings: TStrings; StripCrLf: Boolean);
var
  P: PWChar;
  ID: Integer;
  Cnt: Cardinal;
  Len: Word;
  S: string;
begin
  P := RawData;
  Cnt := 0;
  while Cnt < 16 do
  begin
    Len := Word(P^);
    if Len > 0 then
    begin
      Inc(P);
      ID := ((FResourceItem.ParentItem.Entry^.Name - 1) shl 4) + Cnt;
      S := WideCharToStr(P, Len);
      if StripCrLf then S := StrRemoveChars(S, CharIsReturn);
      Strings.AddObject(S, Pointer(ID));
      Inc(P, Len);
    end else
      Inc(P);
    Inc(Cnt);
  end;
end;

{ TPeResVersion }

procedure TPeResVersion.FillStrings(Strings: TStrings; StripCrLf: Boolean);
var
  I: Integer;
begin
  Strings.Clear;
  with TJclFileVersionInfo.Attach(RawData, Size) do
  try
    for I := 0 to LanguageCount - 1 do
    begin
      LanguageIndex := I;
      Strings.Add(Format('[%s] %s', [LanguageIds[I], LanguageNames[I]]));
      Strings.Add(StringOfChar('-', 80));
      Strings.AddStrings(Items);
      Strings.Add(BinFileVersion);
      Strings.Add(OSIdentToString(FileOS));
      Strings.Add(OSFileTypeToString(FileType, FileSubType));
      Strings.Add('');
    end;
    Strings.Add(RsTranslations);
    for I := 0 to TranslationCount - 1 do
      Strings.Add(VersionLanguageId(Translations[I]));
  finally
    Free;
  end;
end;

{ TPeResImage }

procedure TPeResImage.Clear;
begin
  inherited;
  if Assigned(FPeImage) then
  begin
    if not FImageAttached then FreeAndNil(FPeImage) else FPeImage := nil;
  end;
end;

constructor TPeResImage.Create;
begin
  inherited Create(True);
end;

procedure TPeResImage.CreateList;
var
  I: Integer;
  Kind: TPeResKind;
  Item: TJclPeResourceItem;
  ResItem: TPeResItem;
begin
  with FPeImage.ResourceList do
    for I := 0 to Count - 1 do
    begin
      Item := Items[I];
      if GetResItemKind(Item, Kind) then
      begin
        ResItem := TPeResItem.Create(Self, Item);
        ResItem.FKind := Kind;
        Self.Add(ResItem);
      end else
      case Item.ResourceType of
        rtCursorEntry:
          FCursorEntry := Item.List;
        rtIconEntry:
          FIconEntry := Item.List;
      end;
    end;  
end;

destructor TPeResImage.Destroy;
begin
  UnloadLib;
  inherited;
end;

function TPeResImage.GetFileName: TFileName;
begin
  if Assigned(FPeImage) then Result := FPeImage.FileName else Result := '';
end;

function TPeResImage.GetItems(Index: Integer): TPeResItem;
begin
  Result := TPeResItem(inherited Items[Index]);
end;

function TPeResImage.GetLibHandle: THandle;
begin
  if FLibHandle = 0 then
  begin
    FLibHandle := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE);
    if FLibHandle = 0 then RaiseLastOSError;
  end;  
  Result := FLibHandle;
end;

procedure TPeResImage.SetFileName(const Value: TFileName);
begin
  if FileName <> Value then
  begin
    Clear;
    FImageAttached := False;
    FPeImage := TJclPeImage.Create;
    FPeImage.FileName := Value;
    CreateList;
  end;
end;

procedure TPeResImage.SetPeImage(const Value: TJclPeImage);
begin
  Clear;
  FPeImage := Value;
  FImageAttached := True;
  CreateList;
end;

procedure TPeResImage.UnloadLib;
begin
  if FLibHandle <> 0 then
  begin
    FreeLibrary(FLibHandle);
    FLibHandle := 0;
  end;
end;

initialization
  JclLocalesList := TJclLocalesList.Create;

finalization
  FreeAndNil(JclLocalesList);

end.
