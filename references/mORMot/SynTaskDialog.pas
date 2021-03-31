/// implement TaskDialog window (native on Vista/Seven, emulated on XP)
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynTaskDialog;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Ulrich Gerhardt

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

}

interface

{$I Synopse.inc}

{$IFNDEF DELPHI5OROLDER}  // Delphi 6 or newer
  {$ifndef VER140} // Delphi 6
    {$define WITHUXTHEME} // Themes unit exists till Delphi 7
  {$endif}
  {$ifndef ISDELPHI2007ANDUP}
    {$define FIXVCLALTKEY} // fix QC 37403 for Delphi 6/7/2006
  {$endif}
{$ENDIF}

uses
  Windows,
  CommCtrl,
  Classes,
  SysUtils,
  Messages,
  {$ifndef FPC}
  Consts,
  {$endif}
  Menus,
  {$ifdef USETMSPACK}
  AdvGlowButton,
  AdvMenus,
  TaskDialog,
  TaskDialogEx,
  {$endif}
  {$ifdef WITHUXTHEME}
  Themes,
  {$endif}
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Buttons;

var
  /// will map a generic OK picture from SynTaskDialog.res
  BitmapOK: TBitmap;
  /// will map a generic Arrow picture from SynTaskDialog.res
  BitmapArrow: TBitmap;

  /// will map a default font, according to the available
  // - if Calibri is installed, will use it
  // - will fall back to Tahoma otherwise
  DefaultFont: TFont;

{$ifndef USETMSPACK}
  /// is filled once in the initialization block below
  // - you can set this reference to nil to force Delphi dialogs even
  // on Vista/Seven (e.g. make sense if TaskDialogBiggerButtons=true)
  TaskDialogIndirect: function(AConfig: pointer; Res: PInteger;
    ResRadio: PInteger; VerifyFlag: PBOOL): HRESULT; stdcall;

const
  /// match the 1st custom button ID
  mrBtn1 = 100;
  /// match the 2nd custom button ID
  mrBtn2 = 101;
  /// match the 3rd custom button ID
  mrBtn3 = 102;
  /// match the 4th custom button ID
  mrBtn4 = 103;
  /// match the 5th custom button ID
  mrBtn5 = 104;
  /// match the 6th custom button ID
  mrBtn6 = 105;
  /// match the 7th custom button ID
  mrBtn7 = 106;
  /// match the 8th custom button ID
  mrBtn8 = 107;
  /// match the 9th custom button ID
  mrBtn9 = 108;

  /// match the 1st custom radio ID
  mrRad1 = 200;
  /// match the 2nd custom radio ID
  mrRad2 = 201;
  /// match the 3rd custom radio ID
  mrRad3 = 202;
  /// match the 4th custom radio ID
  mrRad4 = 203;
  /// match the 5th custom radio ID
  mrRad5 = 204;
  /// match the 6th custom radio ID
  mrRad6 = 205;
  /// match the 7th custom radio ID
  mrRad7 = 206;
  /// match the 8th custom radio ID
  mrRad8 = 207;
  /// match the 9th custom radio ID
  mrRad9 = 208;

type
  /// the standard kind of common buttons handled by the Task Dialog
  TCommonButton = (
    cbOK, cbYes, cbNo, cbCancel, cbRetry, cbClose);

  /// set of standard kind of common buttons handled by the Task Dialog
  TCommonButtons = set of TCommonButton;

  /// the available main icons for the Task Dialog
  TTaskDialogIcon = (
    tiBlank, tiWarning, tiQuestion, tiError, tiInformation, tiNotUsed, tiShield);

  /// the available footer icons for the Task Dialog
  TTaskDialogFooterIcon = (
    tfiBlank, tfiWarning, tfiQuestion, tfiError, tfiInformation, tfiShield);

  /// the available configuration flags for the Task Dialog
  // - most are standard TDF_* flags used for Vista/Seven native API
  // (see http://msdn.microsoft.com/en-us/library/bb787473(v=vs.85).aspx
  // for TASKDIALOG_FLAGS)
  // - tdfQuery and tdfQueryMasked are custom flags, implemented in pure Delphi
  // code to handle input query
  // - our emulation code will handle only tdfUseCommandLinks,
  // tdfUseCommandLinksNoIcon, and tdfQuery options
  TTaskDialogFlag = (
    tdfEnableHyperLinks, tdfUseHIconMain, tdfUseHIconFooter,
    tdfAllowDialogCancellation, tdfUseCommandLinks, tdfUseCommandLinksNoIcon,
    tdfExpandFooterArea, tdfExpandByDefault, tdfVerificationFlagChecked,
    tdfShowProgressBar, tdfShowMarqueeProgressBar, tdfCallbackTimer,
    tdfPositionRelativeToWindow, tdfRtlLayout, tdfNoDefaultRadioButton,
    tdfCanBeMinimized, tdfQuery, tdfQueryMasked, tdfQueryFieldFocused);

  /// set of available configuration flags for the Task Dialog
  TTaskDialogFlags = set of TTaskDialogFlag;

  PTaskDialog = ^TTaskDialog;

  /// this callback will be triggerred when a task dialog button is clicked
  // - to prevent the task dialog from closing, the application must set
  // ACanClose to FALSE, otherwise the task dialog is closed and the button
  // ID is returned via the original TTaskDialog.Execute() result
  TTaskDialogButtonClickedEvent = procedure(Sender: PTaskDialog;
    AButtonID: integer; var ACanClose: Boolean) of object;

  /// the visual components of this Task Dialog
  // - map low-level TDE_CONTENT...TDE_MAIN_INSTRUCTION constants and
  // the query editor and checkbox
  // - tdeEdit is for the query editor
  // - tdeVerif is for the checkbox
  TTaskDialogElement = (
    tdeContent, tdeExpandedInfo, tdeFooter, tdeMainInstruction,
    tdeEdit, tdeVerif);

  /// the actual form class used for emulation
  TEmulatedTaskDialog = class(TForm)
  protected
    procedure HandleEmulatedButtonClicked(Sender: TObject);
  public
    /// the Task Dialog structure which created the form
    Owner: PTaskDialog;
    /// the labels corresponding to the Task Dialog main elements
    Element: array[tdeContent..tdeMainInstruction] of TLabel;
    /// the Task Dialog selection list
    Combo: TComboBox;
    /// the Task Dialog optional query editor
    Edit: TEdit;
    /// the Task Dialog optional checkbox
    Verif: TCheckBox;
  end;

  /// structure for low-level access to the task dialog implementation
  // - points either to the HWND handle of the new TaskDialog API
  // or to the emulation dialog
  TTaskDialogImplementation = record
    OnButtonClicked: TTaskDialogButtonClickedEvent;
    case Emulated: Boolean of
      False: (Wnd: HWND);
      True:  (Form: TEmulatedTaskDialog);
  end;

  /// implements a TaskDialog
  // - will use the new TaskDialog API under Vista/Seven, and emulate it with
  // pure Delphi code and standard themed VCL components under XP or 2K
  // - create a TTaskDialog object/record on the stack will initialize all
  // its string parameters to '' (it's a SHAME that since Delphi 2009, objects
  // are not initialized any more: we have to define this type as object before
  // Delphi 2009, and as record starting with Delphi 2009)
  // - set the appropriate string parameters, then call Execute() with all
  // additional parameters
  // - RadioRes/SelectionRes/VerifyChecked will be used to reflect the state
  // after dialog execution
  // - here is a typical usage:
  // !var Task: TTaskDialog;
  // !begin
  // !  Task.Inst := 'Saving application settings';
  // !  Task.Content := 'This is the content';
  // !  Task.Radios := 'Store settings in registry'#10'Store settings in XML file';
  // !  Task.Verify := 'Do no ask for this setting next time';
  // !  Task.VerifyChecked := true;
  // !  Task.Footer := 'XML file is perhaps a better choice';
  // !  Task.Execute([],0,[],tiQuestion,tfiInformation,mrRad1);
  // !  ShowMessage(IntToStr(Task.RadioRes)); // mrRad1=Registry, mrRad2=XML
  // !  if Task.VerifyChecked then
  // !    ShowMessage(Task.Verify);
  // !end;
  {$ifdef UNICODE}
  TTaskDialog = record
  {$else}
  TTaskDialog = object
  {$endif}
    /// the main title of the dialog window
    // - if left void, the title of the application main form is used
    Title: string;
    /// the main instruction (first line on top of window)
    // - any '\n' will be converted into a line feed
    // - if left void, the text is taken from the current dialog icon kind
    Inst: string;
    /// the dialog's primary content content text
    // - any '\n' will be converted into a line feed
    Content: string;
    /// a #13#10 or #10 separated list of custom buttons
    // - they will be identified with an ID number starting at 100 (so you
    // may use mrBtn1, mrBtn2, mrBtn3... mrBtn9 constants)
    // - by default, the buttons will be created at the dialog bottom, just
    // like the common buttons
    // - if tdfUseCommandLinks flag is set, the custom buttons will be created
    // as big button in the middle of the dialog window; in this case, any
    // '\n' will be converted as note text (shown with smaller text under native
    // Vista/Seven TaskDialog, or as popup hint within Delphi emulation)
    // - see the AddButton() wrapper method for an easy access
    Buttons: string;
    /// a #13#10 or #10 separated list of custom radio buttons
    // - they will be identified with an ID number starting at 200 (so you
    // may use mrRad1, mrRad2, mrRad3... mrRad9 constants)
    // - aRadioDef parameter can be set to define the default selected value
    // - '\n' will be converted as note text (shown with smaller text under
    // native Vista/Seven TaskDialog, or as popup hint within Delphi emulation)
    Radios: string;
    /// the expanded information content text
    // - any '\n' will be converted into a line feed
    // - the Delphi emulation will always show the Info content (there is no
    // collapse/expand button)
    Info: string;
    /// the button caption to be displayed when the information is collapsed
    // - not used under XP: the Delphi emulation will always show the Info content
    InfoExpanded: string;
    /// the button caption to be displayed when the information is expanded
    // - not used under XP: the Delphi emulation will always show the Info content
    InfoCollapse: string;
    /// the footer content text
    // - any '\n' will be converted into a line feed
    Footer: string;
    /// the text of the bottom most optional checkbox
    Verify: string;
    /// a #13#10 or #10 separated list of items to be selected
    // - if set, a Combo Box will be displayed to select
    // - if tdfQuery is in the flags, the combo box will be in edition mode,
    // and the user will be able to edit the Query text or fill the field
    // with one item of the selection
    // - this selection is not handled via the Vista/Seven TaskDialog, but
    // with our Delphi emulation code (via a TComboBox)
    Selection: string;
    /// some text to be edited
    // - if tdfQuery is in the flags, will contain the default query text
    // - if Selection is set, the
    Query: string;
    /// the selected radio item
    // - first is numeroted 0
    RadioRes: integer;
    /// after execution, contains the selected item from the Selection list
    SelectionRes: integer;
    /// reflect the the bottom most optional checkbox state
    // - if Verify is not '', should be set before execution
    // - after execution, will contain the final checkbox state
    VerifyChecked: BOOL;
    /// low-level access to the task dialog implementation
    Dialog: TTaskDialogImplementation;

    /// launch the TaskDialog form
    // - some common buttons can be set via aCommonButtons
    // - in emulation mode, aFlags will handle only tdfUseCommandLinks,
    // tdfUseCommandLinksNoIcon, and tdfQuery options
    // - will return 0 on error, or the Button ID (e.g. mrOk for the OK button
    // or mrBtn1/100 for the first custom button defined in Buttons string)
    // - if Buttons was defined, aButtonDef can set the selected Button ID
    // - if Radios was defined, aRadioDef can set the selected Radio ID
    // - aDialogIcon and aFooterIcon are used to specify the displayed icons
    // - aWidth can be used to force a custom form width (in pixels)
    // - aParent can be set to any HWND - by default, Application.DialogHandle
    // - if aNonNative is TRUE, the Delphi emulation code will always be used
    // - aEmulateClassicStyle can be set to enforce conformity with the non themed
    // user interface - see @https://synopse.info/forum/viewtopic.php?pid=2867#p2867
    // - aOnButtonClicked can be set to a callback triggerred when a button is
    // clicked
    function Execute(aCommonButtons: TCommonButtons=[];
      aButtonDef: integer=0; aFlags: TTaskDialogFlags=[];
      aDialogIcon: TTaskDialogIcon=tiInformation;
      aFooterIcon: TTaskDialogFooterIcon=tfiWarning;
      aRadioDef: integer=0; aWidth: integer=0; aParent: HWND=0;
      aNonNative: boolean=false; aEmulateClassicStyle: boolean = false;
      aOnButtonClicked: TTaskDialogButtonClickedEvent=nil): integer;

    /// allow a OnButtonClicked callback to change the Task Dialog main elements
    // - note that tdeVerif could be modified only in emulation mode, since
    // the API does not give any runtime access to the checkbox caption
    // - other elements will work in both emulated and native modes
    procedure SetElementText(element: TTaskDialogElement; const Text: string);
    /// wrapper method able to add a custom button to the Task Dialog
    // - will add the expected content to the Buttons text field
    procedure AddButton(const ACaption: string; const ACommandLinkHint: string = '');
  end;

  /// a wrapper around the TTaskDialog.Execute method
  // - used to provide a "flat" access to task dialog parameters
  {$ifdef UNICODE}
  TTaskDialogEx = record
  {$else}
  TTaskDialogEx = object
  {$endif}
    /// the associated main TTaskDialog instance
    Base: TTaskDialog;
    /// some common buttons to be displayed
    CommonButtons: TCommonButtons;
    /// the default button ID
    ButtonDef: integer;
    /// the associated configuration flags for this Task Dialog
    // - in emulation mode, aFlags will handle only tdfUseCommandLinks,
    // tdfUseCommandLinksNoIcon, and tdfQuery options
    Flags: TTaskDialogFlags;
    /// used to specify the dialog icon
    DialogIcon: TTaskDialogIcon;
    /// used to specify the footer icon
    FooterIcon: TTaskDialogFooterIcon;
    /// the default radio button ID
    RadioDef: integer;
    /// can be used to force a custom form width (in pixels)
    Width: integer;
    /// if TRUE, the Delphi emulation code will always be used
    NonNative: boolean;
    /// can be used to enforce conformity with the non themed user interface
    EmulateClassicStyle: boolean;
    /// this event handler will be fired on a button dialog click
    OnButtonClicked: TTaskDialogButtonClickedEvent;
    /// will initialize the dialog parameters
    // - can be used to display some information with less parameters:
    // !var TaskEx: TTaskDialogEx;
    // !  ...
    // !  TaskEx.Init;
    // !  TaskEx.Base.Title := 'Task Dialog Test';
    // !  TaskEx.Base.Inst := 'Callback Test';
    // !  TaskEx.Execute;
    procedure Init;
    /// main (and unique) method showing the dialog itself
    // - is in fact a wrapper around the TTaskDialog.Execute method
    function Execute(aParent: HWND=0): integer;
  end;
{$endif USETMSPACK}

type
{$ifdef USETMSPACK}
  /// a TMS PopupMenu
  TSynPopupMenu = TAdvPopupMenu;

  TSynButtonParent = TAdvGlowButton;
  TSynTaskDialogButton = TAdvGlowButton;
{$else}
  /// a generic VCL popup menu
  TSynPopupMenu = TPopupMenu;

  TSynButtonParent = {$ifdef WITHUXTHEME}TBitBtn{$else}TButton{$endif};
  TSynTaskDialogButton = TButton;
{$endif USETMSPACK}

  /// a generic Button to be used in the User Interface
  // - is always a Themed button: under Delphi 6, since TBitBtn is not themed,
  // it will be a row TButton with no glyph... never mind...
  TSynButton = class(TSynButtonParent)
  protected
{$ifndef USETMSPACK}
    fDropDownMenu: TSynPopupMenu;
{$endif}
  public
    /// create a standard button instance
    // - ModalResult/Default/Cancel properties will be set as exepcted for this
    // kind of button
    constructor CreateKind(Owner: TWinControl; Btn: TCommonButton;
      Left, Right, Width, Height: integer);
    /// set the glyph of the button
    // - set nothing under Delphi 6
    procedure SetBitmap(Bmp: TBitmap);
{$ifndef USETMSPACK}
    /// drop down the associated Popup Menu
    procedure DoDropDown;
    /// the associated Popup Menu to drop down
    property DropDownMenu: TSynPopupMenu read fDropDownMenu write fDropDownMenu;
{$endif}
  end;

/// return the text without the '&' characters within
function UnAmp(const s: string): string;

{$ifndef USETMSPACK}
var
{
  /// if set to TRUE, buttons will be bigger than default
  // - can be useful e.g. for touch screens
  // - will work only for the Delphi emulated version (aNonNative=true) of
  // TSynTask - could be combined with @TaskDialogIndirect := nil;
  TaskDialogBiggerButtons: boolean = false;
}
  /// a default Task Dialog wrapper instance
  // - can be used to display some information with less parameters, just
  // like the TTaskDialogEx.Init method:
  // !var TaskEx: TTaskDialogEx;
  // !  ...
  // !  TaskEx := DefaultTaskDialog;
  // !  TaskEx.Base.Title := 'Task Dialog Test';
  // !  TaskEx.Base.Inst := 'Callback Test';
  // !  TaskEx.Execute;
  DefaultTaskDialog: TTaskDialogEx = (
    DialogIcon: tiInformation;
    FooterIcon: tfiWarning);
{$endif}

implementation

{$R SynTaskDialog.res}


const
  TD_BTNMOD: array[TCommonButton] of Integer = (
    mrOk, mrYes, mrNo, mrCancel, mrRetry, mrAbort);

function TD_BTNS(button: TCommonButton): pointer;
begin
  case button of
    cbOK:     result := @SMsgDlgOK;
    cbYes:    result := @SMsgDlgYes;
    cbNo:     result := @SMsgDlgNo;
    cbCancel: result := @SMsgDlgCancel;
    cbRetry:  result := @SMsgDlgRetry;
    cbClose:  result := @SCloseButton;
        else  result := nil;
  end;
end;

{ TSynButton }

constructor TSynButton.CreateKind(Owner: TWinControl; Btn: TCommonButton;
  Left, Right, Width, Height: integer);
begin
  Create(Owner);
  Parent := Owner;
  SetBounds(Left,Right,Width,Height);
  Caption := LoadResString(TD_BTNS(Btn));
  ModalResult := TD_BTNMOD[Btn];
  case Btn of
    cbOK:     Default := true;
    cbCancel: Cancel := true;
  end;
  case Btn of
    cbOK: SetBitmap(BitmapOK);
  end;
end;

{$ifndef USETMSPACK}
procedure TSynButton.DoDropDown;
begin
  if DropDownMenu<>nil then
    with ClientToScreen(BoundsRect.TopLeft) do
      DropDownMenu.Popup(X,Y+Height);
end;
{$endif}

procedure TSynButton.SetBitmap(Bmp: TBitmap);
begin
  if Bmp<>nil then
    {$ifdef USETMSPACK}
    Picture.Assign(Bmp);
    {$else}
      {$ifdef WITHUXTHEME}
      Glyph := Bmp;
      {$else}
      // Delphi 6 TBitBtn has no theming -> use generic TButton without glyph
      {$endif}
    {$endif}
end;

function UnAmp(const s: string): string;
begin
  result := StripHotkey(s);
end;


{$ifndef USETMSPACK}

const
  TD_ICONS: array[TTaskDialogIcon] of integer = (
    17, 84, 99, 98, 81, 0, 78);
  TD_FOOTERICONS: array[TTaskDialogFooterIcon] of integer = (
    17, 84, 99, 98, 65533, 65532);
  WIN_ICONS: array[TTaskDialogIcon] of PChar = (
    nil, IDI_WARNING, IDI_QUESTION, IDI_ERROR, IDI_INFORMATION, nil, IDI_WINLOGO);
  WIN_FOOTERICONS: array[TTaskDialogFooterIcon] of PChar = (
    nil, IDI_WARNING, IDI_QUESTION, IDI_ERROR, IDI_INFORMATION, IDI_WINLOGO);

function IconMessage(Icon: TTaskDialogIcon): string;
begin
  case Icon of
    tiWarning:   result := SMsgDlgWarning;
    tiQuestion:  result := SMsgDlgConfirm;
    tiError:     result := SMsgDlgError;
    tiInformation, tiShield: result := SMsgDlgInformation;
    else result := '';
  end;
end;

{$ifdef FIXVCLALTKEY}
var
  WndProcHook: HHOOK = 0;

function HookedWndProcFunc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if nCode=HC_ACTION then
    if PCWPSTRUCT(lParam).message=WM_UPDATEUISTATE then
      InvalidateRect(PCWPSTRUCT(lParam).hwnd,nil,false);
  result := CallNextHookEx(WndProcHook,nCode,wParam,lParam);
end;
{$endif}

procedure InitComCtl6;
var OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := sizeof(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  if OSVersionInfo.dwMajorVersion<6 then
    @TaskDialogIndirect := nil else begin
    @TaskDialogIndirect := GetProcAddress(GetModuleHandle(comctl32),'TaskDialogIndirect');
    {$ifdef FIXVCLALTKEY}
    {$WARN SYMBOL_PLATFORM OFF}
    if (DebugHook=0) {$ifdef WITHUXTHEME}and ThemeServices.ThemesEnabled{$endif} then
      WndProcHook := SetWindowsHookEx(WH_CALLWNDPROC,HookedWndProcFunc,0,GetCurrentThreadID);
    {$WARN SYMBOL_PLATFORM ON}
    {$endif}
  end;
end;

type
  /// internal type used for Unicode string storage
  WS = {$ifdef UNICODE}string{$else}WideString{$endif};

function CR(const aText: string): string;
begin
  if pos('\n', aText) = 0 then
    result := aText else
    result := StringReplace(aText, '\n', #10, [rfReplaceAll]);
end;


{ TTaskDialog }

type
  // see http://msdn.microsoft.com/en-us/library/bb787473
  PTASKDIALOG_BUTTON = ^TTASKDIALOG_BUTTON;
  TTASKDIALOG_BUTTON = packed record
    nButtonID: integer;
    pszButtonText: PWideChar;
  end;

  TTASKDIALOGCONFIG = packed record
    cbSize: integer;
    hwndParent: HWND;
    hInstance: THandle;
    dwFlags: cardinal;
    dwCommonButtons: cardinal;
    pszWindowTitle: PWideChar;
    hMainIcon: HICON;
    pszMainInstruction: PWideChar;
    pszContent: PWideChar;
    cButtons: integer;
    pButtons: PTASKDIALOG_BUTTON;
    nDefaultButton: integer;
    cRadioButtons: integer;
    pRadioButtons: PTASKDIALOG_BUTTON;
    nDefaultRadioButton: integer;
    pszVerificationText: PWideChar;
    pszExpandedInformation: PWideChar;
    pszExpandedControlText: PWideChar;
    pszCollapsedControlText: PWideChar;
    hFooterIcon: HICON;
    pszFooter: PWideChar;
    pfCallback: pointer;
    lpCallbackData: pointer;
    cxWidth: integer;
  end;

const
  TDN_BUTTON_CLICKED = 2; // wParam = Button ID


function TaskDialogCallbackProc(hwnd: HWND; uNotification: UINT;
  wParam: WPARAM; lParam: LPARAM; dwRefData: pointer): HRESULT; stdcall;
var ptd: PTaskDialog absolute dwRefData;
    CanClose: Boolean;
begin
  ptd^.Dialog.Wnd := hwnd;
  Result := S_OK;
  case uNotification of
    TDN_BUTTON_CLICKED:
    if Assigned(ptd^.Dialog.OnButtonClicked) then begin
      CanClose := True;
      ptd^.Dialog.OnButtonClicked(ptd,wParam,CanClose);
      if not CanClose then
        Result := S_FALSE;
    end;
  end;
end;

function TTaskDialog.Execute(aCommonButtons: TCommonButtons;
  aButtonDef: integer; aFlags: TTaskDialogFlags;
  aDialogIcon: TTaskDialogIcon; aFooterIcon: TTaskDialogFooterIcon;
  aRadioDef, aWidth: integer; aParent: HWND; aNonNative: boolean;
  aEmulateClassicStyle: boolean; aOnButtonClicked: TTaskDialogButtonClickedEvent): integer;
function GetNextStringLineToWS(var P: PChar): WS;
var S: PChar;
    {$ifndef UNICODE}tmp: string;{$endif}
begin
  if P=nil then
    result := '' else begin
    S := P;
    while S[0]>=' ' do
      inc(S);
    {$ifdef UNICODE}
    SetString(result,P,S-P);
    result := CR(result);
    {$else}
    SetString(tmp,P,S-P);
    result := WideString(CR(tmp));
    {$endif}
    while (S^<>#0) and (S^<' ') do inc(S); // ignore e.g. #13 or #10
    if S^<>#0 then
      P := S else
      P := nil;
  end;
end;
var aHint: string;
function NoCR(const aText: string): string;
var i: integer;
begin
  result := aText;
  aHint := '';
  i := pos('\n',result);
  if i>0 then begin
    aHint := CR(copy(result,i+2,maxInt));
    SetLength(result,i-1);
  end;
end;
function N(const aText: string): WS;
begin
  if aText='' then
    result := '' else
    result := WS(CR(aText));
end;
var RU: array of Ws;
    RUCount: integer;
    But: array of TTASKDIALOG_BUTTON;
procedure AddRU(Text: string; var n: integer; firstID: integer);
var P: PChar;
begin
  if Text='' then
    exit;
  Text := SysUtils.trim(Text);
  P := @Text[1]; // '\n' handling in GetNextStringLineToWS(P) will change P^
  while P<>nil do begin
    if length(RU)<=RUCount then begin
      SetLength(RU,RUCount+16);
      SetLength(But,RUCount+16);
    end;
    RU[RUCount] := GetNextStringLineToWS(P);
    with But[RUCount] do begin
      nButtonID := n+firstID;
      pszButtonText := pointer(RU[RUCount]);
    end;
    inc(n);
    inc(RUCount);
  end;
end;
var Config: TTASKDIALOGCONFIG;
    i, X, Y, XB, IconBorder, FontHeight: integer;
    Par: TWinControl;
    Panel: TPanel;
    CurrTabOrder: TTabOrder;
    Image: TImage;
    Pic: TIcon;
    Bmp: TBitmap;
    List: TStrings;
    B: TCommonButton;
    CommandLink: TSynButton;
    Rad: array of TRadioButton;
function AddLabel(Text: string; BigFont: boolean): TLabel;
var R: TRect;
    W: integer;
begin
  result := TLabel.Create(Dialog.Form);
  result.Parent := Par;
  result.WordWrap := true;
  if BigFont then begin
    if aEmulateClassicStyle then begin
      result.Font.Height := FontHeight-2;
      result.Font.Style := [fsBold]
    end else begin
      result.Font.Height := FontHeight-4;
      result.Font.Color := $B00000;
    end;
  end else
    result.Font.Height := FontHeight;
  Text := CR(Text);
  result.AutoSize := false;
  R.Left := 0;
  R.Top := 0;
  W := aWidth-X-8;
  R.Right := W;
  R.Bottom := result.Height;
  R.Bottom := DrawText(result.Canvas.Handle,pointer(Text),-1,R,DT_CALCRECT or DT_WORDBREAK);
  result.SetBounds(X,Y,W,R.Bottom);
  result.Caption := Text;
  inc(Y,R.Bottom+16);
end;
procedure AddBevel;
var BX: integer;
begin
  with TBevel.Create(Dialog.Form) do begin
    Parent := Par;
    if (Image<>nil) and (Y<Image.Top+Image.Height) then
      BX := X else
      BX := 2;
    SetBounds(BX,Y,aWidth-BX-2,2);
  end;
  inc(Y,16);
end;

function AddBtn(const s: string; ModalResult: integer): TSynTaskDialogButton;
var WB: integer;
begin
  WB := Dialog.Form.Canvas.TextWidth(s)+52;
  dec(XB,WB);
  if XB<X shr 1 then begin
    XB := aWidth-WB;
    inc(Y,32);
  end;
  result := TSynTaskDialogButton.Create(Dialog.Form);
  result.Parent := Par;
    if aEmulateClassicStyle then
      result.SetBounds(XB,Y,WB-10,22) else
      result.SetBounds(XB,Y,WB-12,28);
  result.Caption := s;
  result.ModalResult := ModalResult;
  result.TabOrder := CurrTabOrder;
  result.OnClick := Dialog.Form.HandleEmulatedButtonClicked;
  case ModalResult of
    mrOk: begin
      result.Default := true;
      if aCommonButtons=[cbOk] then
        result.Cancel := true;
    end;
    mrCancel: result.Cancel := true;
  end;
  if ModalResult=aButtonDef then
    Dialog.Form.ActiveControl := result;
end;

begin
  if (byte(aCommonButtons)=0) and (Buttons='') then begin
    aCommonButtons := [cbOk];
    if aButtonDef=0 then
      aButtonDef := mrOk;
  end;
  if Title='' then
    if Application.MainForm=nil then
      Title := Application.Title else
      Title := Application.MainForm.Caption;
  if Inst='' then
    Inst := IconMessage(aDialogIcon);
  if aParent=0 then
    if Screen.ActiveCustomForm<>nil then
      aParent := Screen.ActiveCustomForm.Handle else
      aParent := Application.Handle;
  Dialog.OnButtonClicked := aOnButtonClicked;
  if Assigned(TaskDialogIndirect) and not aNonNative and
     not (tdfQuery in aFlags) and (Selection='') then begin
    Dialog.Emulated := False;
    // use Vista/Seven TaskDialog implementation (not tdfQuery nor Selection)
    FillChar(Config,sizeof(Config),0);
    Config.cbSize := sizeof(Config);
    Config.hwndParent := aParent;
    Config.pszWindowTitle := pointer(N(Title));
    Config.pszMainInstruction := pointer(N(Inst));
    Config.pszContent := pointer(N(Content));
    RUCount := 0;
    AddRU(Buttons,Config.cButtons,mrBtn1);
    AddRU(Radios,Config.cRadioButtons,mrRad1);
    if Config.cButtons>0 then
      Config.pButtons := @But[0];
    if Config.cRadioButtons>0 then
      Config.pRadioButtons := @But[Config.cButtons];
    Config.pszVerificationText := pointer(N(Verify));
    Config.pszExpandedInformation := pointer(N(Info));
    Config.pszExpandedControlText := pointer(N(InfoExpanded));
    Config.pszCollapsedControlText := pointer(N(InfoCollapse));
    Config.pszFooter := pointer(N(Footer));
    Config.dwCommonButtons := byte(aCommonButtons);
    if (Verify<>'') and VerifyChecked then
      include(aFlags,tdfVerificationFlagChecked);
    if (Config.cButtons=0) and (aCommonButtons=[cbOk]) then
      Include(aFlags,tdfAllowDialogCancellation); // just OK -> Esc/Alt+F4 close
    Config.dwFlags := integer(aFlags);
    Config.hMainIcon := TD_ICONS[aDialogIcon];
    Config.hFooterIcon := TD_FOOTERICONS[aFooterIcon];
    Config.nDefaultButton := aButtonDef;
    Config.nDefaultRadioButton := aRadioDef;
    Config.cxWidth := round(aWidth / (LOWORD(GetDialogBaseUnits()) / 4));
    Config.pfCallback := @TaskDialogCallbackProc;
    Config.lpCallbackData := @self;
    if TaskDialogIndirect(@Config,@result,@RadioRes,@VerifyChecked)=S_OK then
      exit; // error (mostly invalid argument) -> execute the VCL emulation
  end;
  // use our native (naive?) Delphi implementation
  Dialog.Emulated := true;
  Dialog.Form := TEmulatedTaskDialog.CreateNew(Application);
  try
    Dialog.Form.Owner := @Self;
    // initialize form properties
    Dialog.Form.BorderStyle := bsDialog;
    Dialog.Form.BorderIcons := [];
    Dialog.Form.Position := poScreenCenter;
    if not aEmulateClassicStyle then
      Dialog.Form.Font := DefaultFont;
    FontHeight := Dialog.Form.Font.Height;
    if aWidth=0 then begin
      aWidth := Dialog.Form.Canvas.TextWidth(Inst);
      if (aWidth>300) or (Dialog.Form.Canvas.TextWidth(Content)>300) or
         (length(Buttons)>40) then
        aWidth := 480 else
        aWidth := 420;
    end;
    Dialog.Form.ClientWidth := aWidth;
    Dialog.Form.Height := 200;
    Dialog.Form.Caption := Title;
    // create a white panel for the main dialog part
    Panel := TPanel.Create(Dialog.Form);
    Panel.Parent := Dialog.Form;
    Panel.Align := alTop;
    Panel.BorderStyle := bsNone;
    Panel.BevelOuter := bvNone;
    if not aEmulateClassicStyle then begin
      {$ifdef HASINLINE}
      Panel.BevelEdges := [beBottom];
      Panel.BevelKind := bkFlat;
      {$endif}
      Panel.Color := clWhite;
      {$ifdef WITHUXTHEME}
      Panel.ParentBackground := false; // clWhite not used otherwise
      {$endif}
    end;
    Par := Panel;
    // handle main dialog icon
    if aEmulateClassicStyle then
      IconBorder := 10 else
      IconBorder := 24;
     if WIN_ICONS[aDialogIcon]<>nil then begin
      Image := TImage.Create(Dialog.Form);
      Image.Parent := Par;
      Image.Picture.Icon.Handle := LoadIcon(0,WIN_ICONS[aDialogIcon]);
      Image.SetBounds(IconBorder,IconBorder,Image.Picture.Icon.Width,Image.Picture.Icon.Height);
      X := Image.Width+IconBorder*2;
      Y := Image.Top;
      if aEmulateClassicStyle then
        inc(Y, 8);
    end else begin
      Image := nil;
      if not aEmulateClassicStyle then
        IconBorder := IconBorder*2;
      X := IconBorder;
      Y := IconBorder;
    end;
    // add main texts (Instruction, Content, Information)
    Dialog.Form.Element[tdeMainInstruction] := AddLabel(Inst,true);
    Dialog.Form.Element[tdeContent] := AddLabel(Content, false);
    if Info<>'' then
      // no information collapse/expand yet: it's always expanded
      Dialog.Form.Element[tdeExpandedInfo] := AddLabel(Info,false);
    // add command links buttons
    if (tdfUseCommandLinks in aFlags) and (Buttons<>'') then
      with TStringList.Create do
      try
        inc(Y,8);
        Text := SysUtils.trim(Buttons);
        for i := 0 to Count-1 do begin
          CommandLink := TSynButton.Create(Dialog.Form);
          with CommandLink do begin
            Parent := Par;
            Font.Height := FontHeight-3;
            if aEmulateClassicStyle then
              SetBounds(X,Y,aWidth-10-X,40) else
              SetBounds(X,Y,aWidth-16-X,40);
            Caption := NoCR(Strings[i]);
            if aHint<>'' then begin
              ShowHint := true;
              Hint := aHint; // note shown as Hint
            end;
            inc(Y,Height+2);
            ModalResult := i+mrBtn1;
            OnClick := Dialog.Form.HandleEmulatedButtonClicked;
            if ModalResult=aButtonDef then
              Dialog.Form.ActiveControl := CommandLink;
            if aEmulateClassicStyle then begin
              Font.Height := FontHeight - 2;
              Font.Style := [fsBold]
            end;
            {$ifdef WITHUXTHEME}
            if aEmulateClassicStyle then begin
              Margin := 7;
              Spacing := 7;
            end else begin
              Margin := 24;
              Spacing := 10;
            end;
            if not (tdfUseCommandLinksNoIcon in aFlags) then
              SetBitmap(BitmapArrow);
            {$endif}
          end;
        end;
        inc(Y,24);
      finally
        Free;
      end;
    // add radio buttons
    if Radios<>'' then
      with TStringList.Create do
      try
        Text := SysUtils.trim(Radios);
        SetLength(Rad,Count);
        for i := 0 to Count-1 do begin
          Rad[i] := TRadioButton.Create(Dialog.Form);
          with Rad[i] do begin
            Parent := Par;
            SetBounds(X+16,Y,aWidth-32-X,6-FontHeight);
            Caption := NoCR(Strings[i]);
            if aHint<>'' then begin
              ShowHint := true;
              Hint := aHint; // note shown as Hint
            end;
            inc(Y,Height);
            if (i=0) or (i+mrRad1=aRadioDef) then
              Checked := true;
          end;
        end;
        inc(Y,24);
      finally
        Free;
      end;
    // add selection list or query editor
    if Selection<>'' then begin
      List := TStringList.Create;
      try
        Dialog.Form.Combo := TComboBox.Create(Dialog.Form);
        with Dialog.Form.Combo do begin
          Parent := Par;
          SetBounds(X,Y,aWidth-32-X,22);
          if tdfQuery in aFlags then
            Style := csDropDown else
            Style := csDropDownList;
          List.Text := trim(Selection);
          Items.Assign(List);
          ItemIndex := List.IndexOf(Query);
        end;
        inc(Y,42);
      finally
        List.Free;
      end;
    end else
      if tdfQuery in aFlags then begin
        Dialog.Form.Edit := TEdit.Create(Dialog.Form);
        with Dialog.Form.Edit do begin
          Parent := Par;
          SetBounds(X,Y,aWidth-16-X,22);
          Text := Query;
          if tdfQueryMasked in aFlags then
            PasswordChar := '*';
        end;
        if tdfQueryFieldFocused in aFlags then
          Dialog.Form.ActiveControl := Dialog.Form.Edit;
        inc(Y,42);
      end;
    // from now we won't add components to the white panel, but to the form
    Panel.Height := Y;
    Par := Dialog.Form;
    // add buttons and verification checkbox
    if (byte(aCommonButtons)<>0) or (Verify<>'') or
       ((Buttons<>'') and not (tdfUseCommandLinks in aFlags)) then begin
      CurrTabOrder := Panel.TabOrder;
      inc(Y, 16);
      XB := aWidth;
      if not (tdfUseCommandLinks in aFlags) then
        with TStringList.Create do
        try
          Text := SysUtils.trim(Buttons);
          for i := Count-1 downto 0 do
            AddBtn(Strings[i],i+mrBtn1);
        finally
          Free;
        end;
      for B := high(B) downto low(B) do
        if B in aCommonButtons then
          AddBtn(LoadResString(TD_BTNS(B)), TD_BTNMOD[B]);
      if Verify<>'' then begin
        Dialog.Form.Verif := TCheckBox.Create(Dialog.Form);
        with Dialog.Form.Verif do begin
          Parent := Par;
          if X+16+Dialog.Form.Canvas.TextWidth(Verify)>XB then begin
            inc(Y,32);
            XB := aWidth;
          end;
          SetBounds(X,Y,XB-X,24);
          Caption := Verify;
          Checked := VerifyChecked;
        end;
      end;
      inc(Y,36);
    end else
      XB := 0;
    // add footer text with optional icon
    if Footer<>'' then begin
      if XB<>0 then
        AddBevel else
        inc(Y,16);
      if WIN_FOOTERICONS[aFooterIcon]<>nil then begin
        Image := TImage.Create(Dialog.Form);
        Image.Parent := Par;
        Pic := TIcon.Create;
        Bmp := TBitmap.Create;
        try
          Pic.Handle := LoadIcon(0,WIN_FOOTERICONS[aFooterIcon]);
          Bmp.Transparent := true;
          Bmp.Canvas.Brush.Color := Dialog.Form.Color;
          Bmp.Width := Pic.Width shr 1;
          Bmp.Height := Pic.Height shr 1;
          DrawIconEx(Bmp.Canvas.Handle,0,0,Pic.Handle,Bmp.Width,Bmp.Height,0,
            Bmp.Canvas.Brush.Handle,DI_NORMAL);
          Image.Picture.Bitmap := Bmp;
          Image.SetBounds(24,Y,Bmp.Width,Bmp.Height);
          X := 40+Bmp.Width;
        finally
          Bmp.Free;
          Pic.Free;
        end;
      end else
        X := 24;
      Dialog.Form.Element[tdeFooter] := AddLabel(Footer,false);
    end;
    // display the form
    Dialog.Form.ClientHeight := Y;
    // retrieve the results
    result := Dialog.Form.ShowModal;
    if Dialog.Form.Combo<>nil then begin
      SelectionRes := Dialog.Form.Combo.ItemIndex;
      Query := Dialog.Form.Combo.Text;
    end else
    if Dialog.Form.Edit<>nil then
      Query := Dialog.Form.Edit.Text;
    if Dialog.Form.Verif<>nil then
      VerifyChecked := Dialog.Form.Verif.Checked;
    RadioRes := 0;
    for i := 0 to high(Rad) do
      if Rad[i].Checked then
        RadioRes := i+mrRad1;
  finally
    FreeAndNil(Dialog.Form);
  end;
end;

procedure TTaskDialog.SetElementText(element: TTaskDialogElement; const Text: string);
const // wParam = element (TASKDIALOG_ELEMENTS), lParam = new element text (LPCWSTR)
  TDM_UPDATE_ELEMENT_TEXT = WM_USER+114;
begin
  case element of
  tdeContent..tdeMainInstruction:
    if Dialog.Emulated then
      Dialog.Form.Element[element].Caption := CR(Text) else
      SendMessageW(Dialog.Wnd,TDM_UPDATE_ELEMENT_TEXT,ord(element),
        {$ifdef UNICODE}NativeInt{$else}integer{$endif}(pointer(WS(Text))));
  tdeEdit:
    if Dialog.Emulated then
      Dialog.Form.Edit.Text := Text; // only in emulation
  tdeVerif:
    if Dialog.Emulated then
      Dialog.Form.Verif.Caption := Text
  end;
end;

procedure TTaskDialog.AddButton(const ACaption: string; const ACommandLinkHint: string = '');
begin
  if Buttons <> '' then
    Buttons := Buttons + sLineBreak;
  Buttons := Buttons + ACaption;
  if ACommandLinkHint <> '' then
    Buttons := Buttons + '\n' + ACommandLinkHint;
end;


{ TEmulatedTaskDialog }

procedure TEmulatedTaskDialog.HandleEmulatedButtonClicked(Sender: TObject);
var btn: TSynTaskDialogButton absolute Sender;
    CanClose: Boolean;
begin
  if Assigned(Owner) and Assigned(Owner.Dialog.OnButtonClicked) then begin
    CanClose := true;
    Owner.Dialog.OnButtonClicked(Owner,btn.ModalResult,CanClose);
    if not CanClose then
      ModalResult := mrNone;
  end;
end;


{ TTaskDialogEx }

procedure TTaskDialogEx.Init;
begin
  self := DefaultTaskDialog;
end;

function TTaskDialogEx.Execute(aParent: HWND): integer;
begin
  Result := Base.Execute(CommonButtons, ButtonDef, Flags, DialogIcon, FooterIcon,
    RadioDef, Width, aParent, NonNative, EmulateClassicStyle, OnButtonClicked);
end;

{$endif USETMSPACK}


initialization
  DefaultFont := TFont.Create;
  DefaultFont.Style := [];
  if Screen.Fonts.IndexOf('Calibri')>=0 then begin
    DefaultFont.Height := -14;
    DefaultFont.Name := 'Calibri';
  end else begin
    if Screen.Fonts.IndexOf('Tahoma')>=0 then
      DefaultFont.Name := 'Tahoma' else
      DefaultFont.Name := 'Arial';
    DefaultFont.Height := -13;
  end;
  {$ifndef USETMSPACK}
  InitComCtl6;
  assert(ord(tdfCanBeMinimized)=15);
  {$endif USETMSPACK}
  BitmapOK := TBitmap.Create;
  BitmapOK.LoadFromResourceName(HInstance,'btnOk'); // SQLite3btnok.bmp
  BitmapOK.Transparent := true;
  BitmapArrow := TBitmap.Create;
  BitmapArrow.LoadFromResourceName(HInstance,'btnArrow'); // SQLite3btnArrow.bmp
  BitmapArrow.Transparent := true;

finalization
  {$ifdef FIXVCLALTKEY}
  if WndProcHook<>0 then
    UnhookWindowsHookEx(WndProcHook);
  {$endif}
  DefaultFont.Free;
  BitmapArrow.Free;
  BitmapOK.Free;
end.
