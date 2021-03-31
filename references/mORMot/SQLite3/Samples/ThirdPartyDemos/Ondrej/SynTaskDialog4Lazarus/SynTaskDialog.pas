/// implement TaskDialog window (native on Vista/Seven, emulated on XP)
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.19
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
  - Ondrej Pokorny (reddwarf)
  - Gergely Kovacs (bikechecker@gmail.com) - FireMonkey port (Delphi XE7)

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

  Version 1.13
  - initial release

  Version 1.15
  - new tdfQueryMasked function to display * in the tdfQuery editor field

  Version 1.16
  - fixed issue when changing the current application with Alt+Tab - see
    https://synopse.info/fossil/tktview?name=01395e5932
  - fixed compiler error when using the unit with runtime packages enabled
    (known compiler issue about string resources, referenced as E2201)
  - default modal dialog parent changed into any current active form
  - added tdfQueryFieldFocused optional flag to focus the input field component
  - some aesthetical rendering changes and code clean-up (e.g. no temporary
    form necessary), thanks to uligerhardt proposals

  Version 1.18
  - fixed label height display when long text is wrapped on several lines
  - bottom buttons use better looking TButton component
  - bottom buttons won't trim expected shortcut definition, in emulated mode
  - added OnButtonClicked property and associated SetElementText() method
  - now compiles and run in Win64 platform (Delphi XE2+)

  Version 1.19 (Ondrej Pokorny)
  - added Lazarus support (native on Windows Vista+, emulated on all other
    platforms - Windows, Linux and OSX tested)
  - added external translation function for the emulated dialog
   (TaskDialog_Translate)
  - tdfAllowDialogCancellation handled in emulated dialog:
    - if not set: Alt+F4 is blocked
    - if set: Esc is allowed
  - tdfPositionRelativeToWindow handled in emulated dialog
  - platform-independent icons are from www.iconsdb.com:
    Icon license:
      This icon is provided as CC0 1.0 Universal (CC0 1.0) Public Domain
      Dedication.
      You can copy, modify, use, distribute this icon, even for commercial
      purposes, all without asking permission with no attribution required,
      but always appreciated.
  - Maybe To-Do: High DPI-aware emulated dialog + icons
  - Just a remark: native dialogs don't work in non-unicode applications
    (Delphi 7 etc.) because the TaskDialogIndirect is not available for
    non-unicode applications (Windows limitation)
    http://msgroups.net/microsoft.public.vc.mfc/getprocaddress-ansi-unicode/571937

}

interface

{$IFDEF CONDITIONALEXPRESSIONS}  // Delphi 6 or newer
  {$ifndef VER140} // Delphi 6
    {$IFNDEF FMX}
    {$define WITHUXTHEME} // Themes unit exists till Delphi 7
    {$ENDIF}
  {$endif}
  {$IF (CompilerVersion >= 25.0)}// Delphi XE4 UP
    {$LEGACYIFEND ON}
  {$IFEND}
  {$IF CompilerVersion >= 20}// Delphi 2009 IP
    {$IFNDEF FMX}
    {$define WITHPOPUPPARENT}
    {$ENDIF}
    {$define WITHNATIVEINT}
  {$IFEND}
{$ENDIF}
{$IFDEF FPC}
  {$MODE DELPHI}
  {$define WITHUXTHEME}
  {$define WITHPOPUPPARENT}
  {$define WITHNATIVEINT}
  {$IFNDEF MSWINDOWS}
    {$define WITHLAZARUSICONS}
  {$ENDIF}
  {$define WITHLAZARUSARROW}
{$ENDIF}

uses
  {$IFDEF FPC}
  LCLType, LCLStrConsts, LCLIntf,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows, CommCtrl, Messages,
  {$ENDIF}
  {$ifdef FPC}
  LResources,
  {$endif}
  Classes, SysUtils,
  {$ifdef USETMSPACK}
  AdvGlowButton, AdvMenus, TaskDialog, TaskDialogEx,
  {$endif}
  {$IFDEF FMX}
  System.UITypes, System.Types, System.UIConsts,
  FMX.Menus, FMX.Types, FMX.Layouts, FMX.ComboEdit,
  FMX.Graphics, FMX.Forms, FMX.Controls, FMX.StdCtrls, FMX.ExtCtrls,
  FMX.ListBox, FMX.Edit, FMX.Objects, FMX.Platform,
  {$IFDEF MSWINDOWS}
  FMX.Platform.Win
  {$ENDIF}
  {$IFDEF MACOS}
  FMX.Platform.Mac
  {$ENDIF}
  {$ELSE}
  {$IFNDEF FPC}Consts,{$ENDIF}
  Menus,
  Graphics, Forms, Controls, StdCtrls, ExtCtrls, Buttons
  {$ENDIF}
  ;

var
  /// will map a generic Arrow picture from SynTaskDialog.res
  {$IFNDEF FMX}
  {$IFDEF WITHLAZARUSARROW}
  BitmapArrow: TPicture;
  {$ELSE}
  BitmapArrow: TBitmap;
  {$ENDIF}
  {$ENDIF}

  /// will map a default font, according to the available
  // - if Calibri is installed, will use it
  // - will fall back to Tahoma otherwise
  DefaultFont: TFont;

{$ifndef USETMSPACK}
{$IFDEF MSWINDOWS}
  /// is filled once in the initialization block below
  // - you can set this reference to nil to force Delphi dialogs even
  // on Vista/Seven (e.g. make sense if TaskDialogBiggerButtons=true)
  TaskDialogIndirect: function(AConfig: pointer; Res: PInteger;
    ResRadio: PInteger; VerifyFlag: PBOOL): HRESULT; stdcall;
{$ENDIF}
type
  {$IFDEF FMX} // width, height, screen position
  TWH = Single;
  {$IFDEF MACOS}
  HWND = Cardinal;
  {$ENDIF}
  {$ELSE}
  TWH = integer;
  {$ENDIF}

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
    tdfCanBeMinimized
    , tdfQuery, tdfQueryMasked, tdfQueryFieldFocused
    );

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
    {$IFDEF FMX}
    FClientHeight : integer;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); virtual;
    constructor CreateNew(AOwner: TComponent; Dummy: NativeInt = 0); virtual;
    procedure FormShow( Sender : TObject );
    {$ELSE}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    {$ENDIF}

  public
    /// the Task Dialog structure which created the form
    Owner: PTaskDialog;
    /// the labels corresponding to the Task Dialog main elements
    Element: array[tdeContent..tdeMainInstruction] of TLabel;
    /// the Task Dialog selection list
    {$IFDEF FMX}
    Combo: TControl;
    {$ELSE}
    Combo: TComboBox;
    {$ENDIF}
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
  // !  Task.Execute([],0,[],tiQuestion,tfiInformation,200);
  // !  ShowMessage(IntToStr(Task.RadioRes)); // 200=Registry, 201=XML
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
    // - they will be identified with an ID number starting at 100
    // - by default, the buttons will be created at the dialog bottom, just
    // like the common buttons
    // - if tdfUseCommandLinks flag is set, the custom buttons will be created
    // as big button in the middle of the dialog window; in this case, any
    // '\n' will be converted as note text (shown with smaller text under native
    // Vista/Seven TaskDialog, or as popup hint within Delphi emulation)
    Buttons: string;
    /// a #13#10 or #10 separated list of custom radio buttons
    // - they will be identified with an ID number starting at 200
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
    VerifyChecked: Boolean;
    /// low-level access to the task dialog implementation
    Dialog: TTaskDialogImplementation;

    /// launch the TaskDialog form
    // - some common buttons can be set via aCommonButtons
    // - in emulation mode, aFlags will handle only tdfUseCommandLinks,
    // tdfUseCommandLinksNoIcon, and tdfQuery options
    // - will return 0 on error, or the Button ID (e.g. mrOk for the OK button
    // or 100 for the first custom button defined in Buttons string)
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
      {%H-}aFooterIcon: TTaskDialogFooterIcon=tfiWarning;
      aRadioDef: integer=0; aWidth: TWH=0; aParent: HWND=0;
      {%H-}aNonNative: boolean=false; aEmulateClassicStyle: boolean = false;
      aOnButtonClicked: TTaskDialogButtonClickedEvent=nil): integer;

    /// allow a OnButtonClicked callback to change the Task Dialog main elements
    // - note that tdeVerif could be modified only in emulation mode, since
    // the API does not give any runtime access to the checkbox caption
    // - other elements will work in both emulated and native modes
    procedure SetElementText(element: TTaskDialogElement; const Text: string);
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

//function for translating the captions
type
  TTaskDialogTranslate = function(const aString: string): string;
var
  TaskDialog_Translate: TTaskDialogTranslate;
  TaskDialog_ForceEmulation : boolean;

procedure TDShowMessage( instr : string; msg:string=''; Title : string = 'Information' );

implementation

{$IFNDEF WITHLAZARUSARROW}
{$R SynTaskDialog.res}
{$ENDIF}

{$IFDEF FMX}
  {$I info.png.pas}
  {$I warning.png.pas}
  {$I help.png.pas}
  {$I error.png.pas}
  {$I blank.png.pas}
  {$I fmxutil.inc.pas}
{$ENDIF}

const
  TD_BTNMOD: array[TCommonButton] of Integer = (
    mrOk, mrYes, mrNo, mrCancel, mrRetry, mrAbort);

{$IFDEF FMX}
{vcl.consts}
  SMsgDlgWarning = 'Warning';
  SMsgDlgError = 'Error';
  SMsgDlgInformation = 'Information';
  SMsgDlgConfirm = 'Confirm';
  SMsgDlgYes = '&Yes';
  SMsgDlgNo = '&No';
  SMsgDlgOK = 'OK';
  SMsgDlgCancel = 'Cancel';
  SMsgDlgHelp = '&Help';
  SMsgDlgHelpNone = 'No help available';
  SMsgDlgHelpHelp = 'Help';
  SMsgDlgAbort = '&Abort';
  SMsgDlgRetry = '&Retry';
  SMsgDlgIgnore = '&Ignore';
  SMsgDlgAll = '&All';
  SMsgDlgNoToAll = 'N&o to All';
  SMsgDlgYesToAll = 'Yes to &All';
  SMsgDlgClose = '&Close';

function TD_BTNS(button: TCommonButton): string;
begin
  case button of
    cbOK:     result := SMsgDlgOK;
    cbYes:    result := SMsgDlgYes;
    cbNo:     result := SMsgDlgNo;
    cbCancel: result := SMsgDlgCancel;
    cbRetry:  result := SMsgDlgRetry;
    cbClose:  result := SMsgDlgClose;
        else  result := '?';
  end;
end;

procedure SetFontSizeFMX( F:TForm );
var
  lbTmp : TLabel;
begin
  lbTmp := TLabel.Create( NIL );
  DefaultFont.Size := ScalingByScreenDPI_N( F ) * lbtmp.Font.Size;
  lbTmp.Free;
end;

{$ELSE}
function TD_BTNS(button: TCommonButton): pointer;
begin
  case button of
    cbOK:     result := {$ifndef fpc}@SMsgDlgOK{$else}@rsMbOK{$endif};
    cbYes:    result := {$ifndef fpc}@SMsgDlgYes{$else}@rsMbYes{$endif};
    cbNo:     result := {$ifndef fpc}@SMsgDlgNo{$else}@rsMbNo{$endif};
    cbCancel: result := {$ifndef fpc}@SMsgDlgCancel{$else}@rsMbCancel{$endif};
    cbRetry:  result := {$ifndef fpc}@SMsgDlgRetry{$else}@rsMbRetry{$endif};
    cbClose:  result := {$ifndef fpc}@SCloseButton{$else}@rsMbClose{$endif};
        else  result := nil;
  end;
end;
{$ENDIF}

function TD_Trans(const aString: string): string;
begin
  if Assigned(TaskDialog_Translate) then
    Result := TaskDialog_Translate(aString)
  else
    Result := aString;
end;

{ TSynButton }

{$ifndef USETMSPACK}
procedure TSynButton.DoDropDown;
begin
  if DropDownMenu<>nil then
    with
      {$IFDEF FMX}
      LocalToScreen(BoundsRect.TopLeft) do
      {$ELSE}
      ClientToScreen(BoundsRect.TopLeft) do
      {$ENDIF}
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
  {$IF DEFINED(FPC) or DEFINED(FMX)}
  {$IFDEF FMX}
  const cHotkeyPrefix = '&';
  {$ENDIF}
  function StripHotkey(const Text: string): string;
  var
    I: Integer;
  begin
    Result := Text;
    I := 1;
    while I <= Length(Result) do
    begin
      if Result[I] = cHotkeyPrefix then
        if SysLocale.FarEast and
          ((I > 1) and (Length(Result)-I >= 2) and
           (Result[I-1] = '(') and (Result[I+2] = ')')) then
          Delete(Result, I-1, 4)
        else
          Delete(Result, I, 1);
      System.Inc(I);
    end;
  end;
  {$IFEND}
begin
  Result := StripHotkey(s);
end;


{$ifndef USETMSPACK}

{$IFDEF WITHLAZARUSICONS}
const
  LAZ_ICONS: array[TTaskDialogIcon] of string = (
    '', 'std_warning32', 'std_question32', 'std_error32', 'std_information32', '', 'std_shield32');
  LAZ_FOOTERICONS: array[TTaskDialogFooterIcon] of string = (
    '', 'std_warning32', 'std_question32', 'std_error32', 'std_information32', 'std_shield32');
{$ENDIF WITHLAZARUSICONS}

{$IFDEF MSWINDOWS}
const
  {$IFDEF FPC}
    {$EXTERNALSYM IDI_APPLICATION}
    IDI_APPLICATION = MakeIntResource(32512);
    {$EXTERNALSYM IDI_HAND}
    IDI_HAND = MakeIntResource(32513);
    {$EXTERNALSYM IDI_QUESTION}
    IDI_QUESTION = MakeIntResource(32514);
    {$EXTERNALSYM IDI_EXCLAMATION}
    IDI_EXCLAMATION = MakeIntResource(32515);
    {$EXTERNALSYM IDI_ASTERISK}
    IDI_ASTERISK = MakeIntResource(32516);
    {$EXTERNALSYM IDI_WINLOGO}
    IDI_WINLOGO = MakeIntResource(32517);
    {$EXTERNALSYM IDI_SHIELD}
    IDI_SHIELD  = MakeIntResource(32518);
    {$EXTERNALSYM IDI_WARNING}
    IDI_WARNING = IDI_EXCLAMATION;
    {$EXTERNALSYM IDI_ERROR}
    IDI_ERROR = IDI_HAND;
    {$EXTERNALSYM IDI_INFORMATION}
    IDI_INFORMATION = IDI_ASTERISK;
  {$ENDIF FPC}

  TD_ICONS: array[TTaskDialogIcon] of integer = (
    17, 84, 99, 98, 81, 0, 78);
  TD_FOOTERICONS: array[TTaskDialogFooterIcon] of integer = (
    17, 84, 99, 98, 65533, 65532);
  WIN_ICONS: array[TTaskDialogIcon] of PChar = (
    nil, IDI_WARNING, IDI_QUESTION, IDI_ERROR, IDI_INFORMATION, nil, IDI_WINLOGO);
  WIN_FOOTERICONS: array[TTaskDialogFooterIcon] of PChar = (
    nil, IDI_WARNING, IDI_QUESTION, IDI_ERROR, IDI_INFORMATION, IDI_WINLOGO);
{$ENDIF MSWINDOWS}

{$IFDEF FMX}
var
  ICON_PNG : array[TTaskDialogIcon] of TBitmap;
{$ENDIF}

function IconMessage(Icon: TTaskDialogIcon): string;
begin
  case Icon of
    tiWarning:   result := {$ifndef fpc}SMsgDlgWarning{$else}rsMtWarning{$endif};
    tiQuestion:  result := {$ifndef fpc}SMsgDlgConfirm{$else}rsMtConfirmation{$endif};
    tiError:     result := {$ifndef fpc}SMsgDlgError{$else}rsMtError{$endif};
    tiInformation, tiShield: result := {$ifndef fpc}SMsgDlgInformation{$else}rsMtInformation{$endif};
    else result := '';
  end;
  result := TD_Trans(result);
end;

{$IFDEF MSWINDOWS}
procedure InitComCtl6;
var OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := sizeof(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  if OSVersionInfo.dwMajorVersion<6 then
    @TaskDialogIndirect := nil else
    @TaskDialogIndirect := GetProcAddress(GetModuleHandle(comctl32),'TaskDialogIndirect');
end;
{$ENDIF}

type
  /// internal type used for Unicode string storage
  WS = {$ifdef UNICODE}string{$else}WideString{$endif};

function _WS(const aString: string): WS;
begin
  {$IFDEF FPC}
  Result := UTF8Decode(aString);
  {$ELSE}
  Result := WS(aString);
  {$ENDIF}
end;

function CR(const aText: string): string;
begin
  if pos('\n', aText) = 0 then
    result := aText else
    result := StringReplace(aText, '\n', #10, [rfReplaceAll]);
end;


{ TTaskDialog }

{$IFDEF MSWINDOWS}
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
{$ENDIF}

function TTaskDialog.Execute(aCommonButtons: TCommonButtons=[];
      aButtonDef: integer=0; aFlags: TTaskDialogFlags=[];
      aDialogIcon: TTaskDialogIcon=tiInformation;
      {%H-}aFooterIcon: TTaskDialogFooterIcon=tfiWarning;
      aRadioDef: integer=0; aWidth: TWH=0; aParent: HWND=0;
      {%H-}aNonNative: boolean=false; aEmulateClassicStyle: boolean = false;
      aOnButtonClicked: TTaskDialogButtonClickedEvent=nil): integer;
function GetNextStringLineToWS(var P: PChar): WS;
var S: PChar;
    {$ifndef UNICODE}tmp: string;{$endif}
begin
  if P=nil then
    result := '' else begin
    S := P;
    while S[0]>=' ' do
      System.inc(S);
    {$ifdef UNICODE}
    SetString(result,P,S-P);
    result := CR(result);
    {$else}
    SetString(tmp,P,S-P);
    result := _WS(CR(tmp));
    {$endif}
    while (S^<>#0) and (S^<' ') do System.inc(S); // ignore e.g. #13 or #10
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
    result := _WS(CR(aText));
end;
{$IFDEF MSWINDOWS}
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
    System.inc(n);
    System.inc(RUCount);
  end;
end;
{$ENDIF}
var
    {$IFDEF MSWINDOWS}
    Config: TTASKDIALOGCONFIG;
    {$ENDIF}
    {$IFDEF FMX}
    Pic : TBitmap;
    {$ELSE}
    {$IFDEF WITHLAZARUSICONS}
    Pic: TPicture;
    {$ELSE}
    Pic: TIcon;
    {$ENDIF}
    {$ENDIF}
    Bmp: TBitmap;
    X, Y, XB, IconBorder, FontHeight: TWH;
    i : integer;
    Par: {$IFDEF FMX} TFMXObject {$ELSE} TWinControl {$ENDIF};
    Panel: {$IFDEF FMX} TRectangle {$ELSE} TPanel {$ENDIF};
    CurrTabOrder: TTabOrder;
    Image: TImage;
    List: TStrings;
    B: TCommonButton;
    CommandLink: TSynButton;
    Rad: array of TRadioButton;

function AddLabel(Text: string; BigFont: boolean): TLabel;
var
    {$IFDEF FMX}
    R: TRectF;
    W: Single;
    {$ELSE}
    R: TRect;
    W: integer;
    {$ENDIF}
begin
  result := TLabel.Create(Dialog.Form);
  result.Parent := Par;
  result.WordWrap := true;
  if BigFont then begin
    if aEmulateClassicStyle then begin
      {$IFDEF FMX}
      result.Font.Size:= FontHeight*1.3;
      result.Font.Style := [TFontStyle.fsBold];
      {$ELSE}
      result.Font.Height := FontHeight-2;
      result.Font.Style := [fsBold];
      {$ENDIF}
    end else begin
      {$IFDEF FMX}
      result.Font.Size:= FontHeight*1.3;
      {$IFDEF MSWINDOWS}
      result.FontColor := $FF0000B0;
      {$ENDIF}
      {$ELSE}
      result.Font.Height := FontHeight-4;
      result.Font.Color := $B00000;
      {$ENDIF}
    end;
  end else
    {$IFDEF FMX}
    result.Font.Size := FontHeight;
    {$ELSE}
    result.Font.Height := FontHeight;
    {$ENDIF}
  Text := CR(Text);
  result.AutoSize := false;
  R.Left := 0;
  R.Top := 0;
  W := aWidth-X-8;
  R.Right := W;
  {$IFDEF FMX}
  Result.StyledSettings := Result.StyledSettings - [ TStyledsetting.FontColor,
                                                     TStyledSetting.Size ];
  if Text = '' then
    R.Height := 1
  else begin
    R.Height := FMXMeasureText( Text, Result, W, true ).Height;
  end;
  {$ELSE}
  R.Bottom := Result.Height;
  {$ENDIF}

  {$IFNDEF FMX}
  DrawText(result.Canvas.Handle,PChar(Text),Length(Text),R,DT_CALCRECT or DT_WORDBREAK);//lazarus does not return box height on OSX (Lazarus bug), the height is stored in the rect in all cases, so we don't need to use the result
  {$ENDIF}

  Result.SetBounds(X,Y,W,R.Bottom);

  Result.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := Text;
  inc(Y,round(R.Bottom)+16);
end;
{$IFNDEF FMX}
//! BEVEL IS NOT AVAILABLE IN FMX...
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
{$ENDIF}
function AddButton(const s: string; ModalResult: integer): TSynTaskDialogButton;
var WB: TWH;
begin
  result := TSynTaskDialogButton.Create(Dialog.Form);
  result.Parent := Par;
  {$IFDEF FMX}
  Result.StyledSettings := Result.StyledSettings - [ TStyledSetting.Size ];
  Result.Font.Size := FontHeight;
  WB := FMXMeasureText( s, Result, 250, false ).Width + 52;
  if WB < 80 then
    WB := 80;
  {$ELSE}
  WB := Dialog.Form.Canvas.TextWidth(s)+52;
  {$ENDIF}
  dec(XB,WB);
  if XB<trunc(X/2) then begin
    XB := aWidth-WB;
    inc(Y,32);
  end;
    if aEmulateClassicStyle then
      result.SetBounds(XB,Y,WB-10,22) else
      result.SetBounds(XB,Y,WB-12,28);
  result.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := s;
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

{$IFDEF FMX}
var
  LI : TListBoxItem;
{$ENDIF}
begin
  if (byte(aCommonButtons)=0) and (Buttons='') then
  begin
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
  Dialog.OnButtonClicked := aOnButtonClicked;

  {$ifdef MSWINDOWS} // WINDOWS API TASKDIALOG
  if aParent=0 then
  {$IFDEF FMX}
    if Screen.ActiveForm <> NIL then
      aParent := WindowHandleToPlatform( Screen.ActiveForm.Handle ).Wnd;
  {$ELSE}
    if Screen.ActiveCustomForm<>nil then
      aParent := Screen.ActiveCustomForm.Handle else
      aParent := {$ifndef fpc}Application.Handle{$else}0{$endif};
  {$ENDIF}
  if not TaskDialog_ForceEmulation and
     Assigned(TaskDialogIndirect) and not aNonNative
     and not (tdfQuery in aFlags) and (Selection='')
  then begin
    Dialog.Emulated := False;
    // use Vista/Seven TaskDialog implementation (not tdfQuery nor Selection)
    FillChar(Config,sizeof(Config),0);
    Config.cbSize := sizeof(Config);
    Config.hwndParent := aParent;
    Config.pszWindowTitle := pointer(N(Title));
    Config.pszMainInstruction := pointer(N(Inst));
    Config.pszContent := pointer(N(Content));
    RUCount := 0;
    AddRU(Buttons,Config.cButtons,100);
    AddRU(Radios,Config.cRadioButtons,200);
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
    Config.dwFlags := Cardinal(aFlags);
    Config.hMainIcon := TD_ICONS[aDialogIcon];
    Config.hFooterIcon := TD_FOOTERICONS[aFooterIcon];
    Config.nDefaultButton := aButtonDef;
    Config.nDefaultRadioButton := aRadioDef;
    Config.cxWidth := round(aWidth);
    Config.pfCallback := @TaskDialogCallbackProc;
    Config.lpCallbackData := @self;
    if TaskDialogIndirect(@Config,@result,@RadioRes,@VerifyChecked)=S_OK then
      exit; // error (mostly invalid argument) -> execute the VCL emulation
  end;
  {$endif MSWINDOWS}

  // use our native (naive?) Delphi implementation
  Dialog.Emulated := true;
  Dialog.Form := TEmulatedTaskDialog.CreateNew(Application);
  try
    {$IFDEF FMX}
    SetFontSizeFMX( Dialog.Form );

    {$ENDIF}
    Dialog.Form.Owner := @Self;
    // initialize form properties
    {$IFDEF FMX}
    Dialog.Form.BorderStyle := TFmxFormBorderStyle.Single;
    if tdfAllowDialogCancellation in aFlags then
      Dialog.Form.BorderIcons := [TBorderIcon.biSystemMenu]
    else
      Dialog.Form.BorderIcons := [];
    if tdfPositionRelativeToWindow in aFlags then
      Dialog.Form.Position := TFormPosition.OwnerFormCenter
    else
      Dialog.Form.Position := TFormPosition.ScreenCenter;
    //if not aEmulateClassicStyle then
    //  Dialog.Form.Font := DefaultFont;
    FontHeight := DefaultFont.Size;
    {$ELSE}
    Dialog.Form.BorderStyle := bsDialog;
    if tdfAllowDialogCancellation in aFlags then
      Dialog.Form.BorderIcons := [biSystemMenu]
    else
      Dialog.Form.BorderIcons := [];
    if tdfPositionRelativeToWindow in aFlags then
      Dialog.Form.Position := poOwnerFormCenter
    else
      Dialog.Form.Position := poScreenCenter;
    if not aEmulateClassicStyle then
      Dialog.Form.Font := DefaultFont;
    FontHeight := Dialog.Form.Font.Height;
    {$ENDIF}
    {$IFDEF FPC}
    if FontHeight = 0 then
      FontHeight := Screen.SystemFont.Height;
    {$ENDIF}
    if aWidth=0 then begin
      aWidth := Dialog.Form.Canvas.TextWidth(Inst);
      if (aWidth>300) or (Dialog.Form.Canvas.TextWidth(Content)>300) or
         (length(Buttons)>40) then
        aWidth := 480 else
        aWidth := 420;
    end;
    Dialog.Form.ClientWidth := round(aWidth);
    Dialog.Form.Height := 200;
    Dialog.Form.Caption := Title;
    // create a white panel for the main dialog part
    {$IFDEF FMX}
    Panel := TRectangle.Create(Dialog.Form);
    {$IFDEF MSWINDOWS}
    Panel.Fill.Color := claWhite;
    {$ELSE}
    Panel.Fill.Kind := TBrushKind.None;
    {$ENDIF}
    Panel.Stroke.Kind := TBrushKind.None;
    Panel.Parent := Dialog.Form;
    Panel.Align := TAlignLayout.Top;
    {$ELSE}
    Panel := TPanel.Create(Dialog.Form);
    Panel.Parent := Dialog.Form;
    Panel.Align := alTop;
    Panel.BorderStyle := bsNone;
    Panel.BevelOuter := bvNone;
    if not aEmulateClassicStyle then
      Panel.Color := clWhite;
    {$ENDIF}
    if not aEmulateClassicStyle then
    begin
      {$ifdef HASINLINE}
      Panel.BevelEdges := [beBottom];
      Panel.BevelKind := bkFlat;
      {$endif}
      {$ifdef WITHUXTHEME}{$ifndef FPC}
      Panel.ParentBackground := false; // clWhite not used otherwise
      {$endif}{$endif}
    end;
    Par := Panel;
    // handle main dialog icon
    if aEmulateClassicStyle then
      IconBorder := 10 else
      IconBorder := 24;

     {$IFDEF FMX}
     if true then
     {$ELSE}
     {$IFDEF WITHLAZARUSICONS}
     if LAZ_ICONS[aDialogIcon]<>'' then
     {$ELSE}
     if WIN_ICONS[aDialogIcon]<>nil then
     {$ENDIF}
     {$ENDIF}
     begin
      Image := TImage.Create(Dialog.Form);
      Image.Parent := Par;

      {$IFDEF FMX}
      Image.Bitmap.Assign( ICON_PNG[ aDialogIcon ] );
      Image.SetBounds(IconBorder,IconBorder,Image.Bitmap.Width,Image.Bitmap.Height);
      X := Image.Width+IconBorder*2;
      Y := Image.Position.Y;
      {$ELSE}
      {$IFDEF WITHLAZARUSICONS}
      Image.Picture.LoadFromLazarusResource(LAZ_ICONS[aDialogIcon]);
      {$ELSE}
      Image.Picture.Icon.Handle := LoadIcon(0,WIN_ICONS[aDialogIcon]);
      {$ENDIF}
      Image.SetBounds(IconBorder,IconBorder,Image.Picture.Icon.Width,Image.Picture.Icon.Height);
      X := Image.Width+IconBorder*2;
      Y := Image.Top;
      {$ENDIF}
      if aEmulateClassicStyle then
        inc(Y, 8);
    end else
    begin
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
            {$IFDEF FMX}
            Font.Size := FontHeight;
            StyledSettings := StyledSettings - [ TStyledSetting.Size ];
            {$ELSE}
            Font.Height := FontHeight-3;
            {$ENDIF}
            if aEmulateClassicStyle then
              SetBounds(X,Y,aWidth-10-X,40) else
              SetBounds(X,Y,aWidth-16-X,40);
            {$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := NoCR(Strings[i]);

            if aHint<>'' then begin
              ShowHint := true;
              Hint := aHint; // note shown as Hint
            end;
            inc(Y,Height+2);
            ModalResult := i+100;
            OnClick := Dialog.Form.HandleEmulatedButtonClicked;
            if ModalResult=aButtonDef then
              Dialog.Form.ActiveControl := CommandLink;
            if aEmulateClassicStyle then begin
              {$IFDEF FMX}
              Font.Size := FontHeight - 2;
              Font.Style := [TFontStyle.fsBold]
              {$ELSE}
              Font.Height := FontHeight - 2;
              Font.Style := [fsBold]
              {$ENDIF}
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
              SetBitmap(BitmapArrow{$IFDEF WITHLAZARUSARROW}.Bitmap{$ENDIF});
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
            SetBounds(X+16,Y,aWidth-32-X,6 {$IFDEF FMX}+{$ELSE}-{$ENDIF}FontHeight);
            {$IFDEF FMX}
            Font.Size := FontHeight;
            StyledSettings := StyledSettings - [ TStyledSetting.Size ];
            Text := NoCR(Strings[i]);
            {$ELSE}
            Caption := NoCR(Strings[i]);
            if aHint<>'' then begin
              ShowHint := true;
              Hint := aHint; // note shown as Hint
            end;
            {$ENDIF}
            inc(Y,Height);
            if (i=0) or (i+200=aRadioDef) then
              {$IFDEF FMX}
              IsChecked := true;
              {$ELSE}
              Checked := true;
              {$ENDIF}
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
        {$IFDEF FMX}
        if tdfQuery in aFlags then begin
          Dialog.Form.Combo := TComboEdit.Create(Dialog.Form);
          TComboEdit( Dialog.Form.Combo ).StyledSettings :=
              TComboEdit( Dialog.Form.Combo ).StyledSettings - [ TStyledSetting.Size ];
          TComboEdit( Dialog.Form.Combo ).Font.Size := FontHeight;
        end else begin
          Dialog.Form.Combo := TComboBox.Create(Dialog.Form);
        end;
        {$ELSE}
        Dialog.Form.Combo := TComboBox.Create(Dialog.Form);
        {$ENDIF}
        with Dialog.Form.Combo do
        begin
          Parent := Par;
          SetBounds(X,Y,aWidth-32-X,22);
          {$IFNDEF FMX}
          if tdfQuery in aFlags then
            Style := csDropDown else
            Style := csDropDownList;
          {$ENDIF}
          List.Text := trim(Selection);
          {$IFDEF FMX}
          if Dialog.Form.Combo is TComboBox then
          begin
            TComboBox(Dialog.Form.Combo).Items.Assign(List);
            if Query <> '' then
              TComboBox(Dialog.Form.Combo).ItemIndex := List.IndexOf(Query)
            else
              TComboBox(Dialog.Form.Combo).ItemIndex := 0;
            TComboBox( Dialog.Form.Combo ).ItemHeight := FontHeight + 8;
            for i := 0 to TComboBox( Dialog.Form.Combo ).Count-1 do
              with TComboBox( Dialog.Form.Combo ).ListBox.ItemByIndex(i) do
              begin
                StyledSettings := StyledSettings - [ TStyledSetting.Size ];
                Font.Size := FontHeight;
                //FindStyleResource('text')
              end;
          end else
          if Dialog.Form.Combo is TComboEdit then
          begin
            TComboEdit(Dialog.Form.Combo).Items.Assign(List);
            TComboEdit(Dialog.Form.Combo).Text := Query;
{            for i := 0 to TComboEdit( Dialog.Form.Combo ).Count-1 do
              TComboEdit( Dialog.Form.Combo ).
              with TComboEdit( Dialog.Form.Combo )ListBox.ItemByIndex(i) do
              begin
                StyledSettings := StyledSettings - [ TStyledSetting.Size ];
                Font.Size := FontHeight;
              end;
}
          end;
          {$ELSE}
          Items.Assign(List);
          if Query <> '' then
            Text := Query
          else
            ItemIndex := 0;
          {$ENDIF}
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
            {$IFDEF FMX}
            Password := true;
            {$ELSE}
            PasswordChar := '*';
            {$ENDIF}
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
            AddButton(Strings[i],i+100);
        finally
          Free;
        end;
      for B := high(B) downto low(B) do
        if B in aCommonButtons then
          {$IFDEF FMX}
          AddButton(TD_Trans(TD_BTNS(B)),TD_BTNMOD[B]);
          {$ELSE}
          AddButton(TD_Trans(LoadResString(TD_BTNS(B))), TD_BTNMOD[B]);
          {$ENDIF}
      if Verify<>'' then begin
        Dialog.Form.Verif := TCheckBox.Create(Dialog.Form);
        with Dialog.Form.Verif do begin
          Parent := Par;
          if X+16+Dialog.Form.Canvas.TextWidth(Verify)>XB then
          begin
            inc(Y,32);
            XB := aWidth;
          end;
          SetBounds(X,Y,XB-X,24);
          {$IFDEF FMX}
          StyledSettings := StyledSettings - [ TStyledSetting.Size ];
          Font.Size := FontHeight;
          Text := Verify;
          IsChecked := VerifyChecked;
          {$ELSE}
          Caption := Verify;
          Checked := VerifyChecked;
          {$ENDIF}
        end;
      end;
      {$IFDEF FMX}
      inc(Y,40);
      {$ELSE}
      inc(Y,36);
      {$ENDIF}
    end else
      XB := 0;
    // add footer text with optional icon
    if Footer<>'' then begin
      {$IFNDEF FMX}
      if XB<>0 then
        AddBevel else
      {$ENDIF}
        inc(Y,16);
      {$IFDEF FMX}
      if true then
      {$ELSE}
      {$IFDEF WITHLAZARUSICONS}
      if LAZ_FOOTERICONS[aFooterIcon]<>'' then
      {$ELSE}
      if WIN_FOOTERICONS[aFooterIcon]<>nil then
      {$ENDIF}
      {$ENDIF}
      begin
        Image := TImage.Create(Dialog.Form);
        Image.Parent := Par;
        {$IFDEF FMX}
        ;
        {$ELSE}
        {$IFDEF WITHLAZARUSICONS}
        Pic := TPicture.Create;
        {$ELSE}
        Pic := TIcon.Create;
        {$ENDIF}
        Bmp := TBitmap.Create;
        {$ENDIF}
        try
          {$IFDEF FMX}
          Image.Bitmap.Assign( ICON_PNG[ TTaskDialogIcon(aFooterIcon) ] );
          {$ELSE}
          Bmp.Transparent := true;
          {$IFDEF WITHLAZARUSICONS}
          Pic.LoadFromLazarusResource(LAZ_FOOTERICONS[aFooterIcon]);
          {$ELSE}
          Pic.Handle := LoadIcon(0,WIN_FOOTERICONS[aFooterIcon]);
          {$ENDIF}
          Bmp.Width := Pic.Width shr 1;
          Bmp.Height := Pic.Height shr 1;
          Bmp.Canvas.Brush.Color := Dialog.Form.Color;
          {$IFDEF FPC}
          if Bmp.Canvas.Brush.Color = clDefault then
            Bmp.Canvas.Brush.Color := clBtnFace;
          Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
          {$ENDIF}
          {$IFDEF WITHLAZARUSICONS}
          Bmp.Canvas.StretchDraw(Rect(0, 0, Bmp.Width, Bmp.Height), Pic.Graphic);
          {$ELSE}
          DrawIconEx(Bmp.Canvas.Handle,0,0,Pic.Handle,Bmp.Width,Bmp.Height,0,
            Bmp.Canvas.Brush.Handle,DI_NORMAL);
          {$ENDIF}
          Image.Picture.Bitmap := Bmp;
          {$ENDIF}

          Image.SetBounds(24,Y,Bmp.Width,Bmp.Height);
          X := 40+Image.Width;
        finally
          Bmp.Free;
          Pic.Free;
        end;
      end else
      begin
        X := 24;
      end;
      Dialog.Form.Element[tdeFooter] := AddLabel(Footer,false);
    end;
    // display the form
    Dialog.Form.ClientHeight := round(Y);
    {$IFDEF FMX}
    Dialog.Form.FClientHeight := round(Y);
    Dialog.Form.OnShow := Dialog.Form.FormShow;
    {$ENDIF}

    //set form parent
    {$IFDEF WITHPOPUPPARENT}
    if aParent <> 0 then
      for I := 0 to Screen.CustomFormCount-1 do
        if Screen.CustomForms[I].Handle = aParent then
        begin
          Dialog.Form.PopupParent := Screen.CustomForms[I];
          Break;
        end;
    if not Assigned(Dialog.Form.PopupParent) then
      Dialog.Form.PopupParent := Screen.ActiveCustomForm;
    if Assigned(Dialog.Form.PopupParent) then
    begin
      Dialog.Form.PopupMode := pmExplicit;
    end;
    {$ENDIF}

    // retrieve the results
    result := Dialog.Form.ShowModal;
    if Dialog.Form.Combo<>nil then begin
    {$IFDEF FMX}
      if Dialog.Form.Combo is TComboBox then
      begin
        SelectionRes := TComboBox(Dialog.Form.Combo).ItemIndex;
        if TComboBox(Dialog.Form.Combo).ItemIndex <> -1 then
          Query := TComboBox(Dialog.Form.Combo).Items[ TComboBox(Dialog.Form.Combo).ItemIndex ];
      end else
      if Dialog.Form.Combo is TComboEdit then
      begin
        SelectionRes := TComboEdit( Dialog.Form.Combo ).Items.IndexOf( TComboEdit( Dialog.Form.Combo ).Text );
        Query := TComboEdit( Dialog.Form.Combo ).Text;
      end;
    {$ELSE}
      SelectionRes := Dialog.Form.Combo.ItemIndex;
      Query := Dialog.Form.Combo.Text;
    {$ENDIF}
    end else
    if Dialog.Form.Edit<>nil then
      Query := Dialog.Form.Edit.Text;
    if Dialog.Form.Verif<>nil then
      VerifyChecked := Dialog.Form.Verif.{$IFDEF FMX}IsChecked{$ELSE}Checked{$ENDIF};
    RadioRes := 0;
    for i := 0 to high(Rad) do
      if Rad[i].{$IFDEF FMX}IsChecked{$ELSE}Checked{$ENDIF} then
        RadioRes := i+200;
  finally
    FreeAndNil(Dialog.Form);
  end;
end;

procedure TTaskDialog.SetElementText(element: TTaskDialogElement; const Text: string);
{$IFDEF MSWINDOWS}
const // wParam = element (TASKDIALOG_ELEMENTS), lParam = new element text (LPCWSTR)
  TDM_UPDATE_ELEMENT_TEXT = WM_USER+114;
{$ENDIF}
begin
  case element of
  tdeContent..tdeMainInstruction:
    if Dialog.Emulated then
      Dialog.Form.Element[element].{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := CR(Text)
    {$IFDEF MSWINDOWS}
    else
      SendMessageW(Dialog.Wnd,TDM_UPDATE_ELEMENT_TEXT,ord(element),
        {$ifdef WITHNATIVEINT}NativeInt{$else}integer{$endif}(pointer(_WS(Text))))
    {$ENDIF};
  tdeEdit:
    if Dialog.Emulated then
      Dialog.Form.Edit.Text := Text; // only in emulation
  tdeVerif:
    if Dialog.Emulated then
      Dialog.Form.Verif.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := Text
  end;
end;


{ TEmulatedTaskDialog }

{$IFDEF FMX}
constructor TEmulatedTaskDialog.CreateNew(AOwner: TComponent; Dummy: NativeInt = 0);
{$ELSE}
constructor TEmulatedTaskDialog.CreateNew(AOwner: TComponent; Num: Integer);
{$ENDIF}
begin
  {$IFDEF FMX}
  inherited CreateNew(AOwner, Dummy);
  {$ELSE}
  inherited CreateNew(AOwner, Num);
  KeyPreview := True;
  {$ENDIF}
end;

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


{$IFDEF FMX}
procedure TEmulatedTaskDialog.FormShow( Sender : TObject );
begin
  // resetting clientheight, did not work properly...
  ClientHeight := FClientHeight;
end;

const
  VK_F4 = vkF4;
  VK_ESCAPE = vkEscape;

procedure TEmulatedTaskDialog.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
{$ELSE}
procedure TEmulatedTaskDialog.KeyDown(var Key: Word; Shift: TShiftState);
{$ENDIF}
begin
  if ({$IFDEF FMX}TBorderIcon.{$ENDIF}biSystemMenu in BorderIcons) then//is Alt+F4/Esc cancellation allowed?
  begin//yes -> cancel on ESC
    if Key = VK_ESCAPE then
      Close;
  end
  {$IFDEF MSWINDOWS}
  else
  begin//no -> block Alt+F4
    if (Key = VK_F4) and (ssAlt in Shift) then//IMPORTANT: native task dialog blocks Alt+F4 to close the dialog -> we have to block it as well
      Key := 0;
  end
  {$ENDIF}
  ;
  inherited;
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

{$IFDEF FMX}
function StrToStream( const s : RawByteString ):TMemoryStream;
begin
  Result := TMemoryStream.Create;
  Result.Write( s[1], length(s) );
  Result.Seek( 0, soFromBeginning );
end;

function StrToBitmap( const s : RawByteString ):TBitmap;
var
  MS : TMemoryStream;
begin
  Result := NIL;
  MS := StrToStream( s );
  try
    Result := TBitmap.CreateFromStream( MS );
  finally
    MS.Free;
  end;
end;
{$ENDIF}

procedure TDShowMessage( instr : string; msg:string=''; Title : string = 'Information' );
var
  TD : TTaskDialog;
begin
  TD.Title := TD_Trans( Title );
  TD.Inst := instr;
  TD.Content := msg;
  TD.Execute( [ cbOk ] );
end;

initialization
  {$ifdef WITHLAZARUSICONS}
  {$I std_lazicons.lrs}
  {$endif}
  {$IFDEF WITHLAZARUSARROW}
  {$I std_lazarrow.lrs}
  {$ENDIF}

  DefaultFont := TFont.Create;
  DefaultFont.Style := [];

  {$IFNDEF FMX}
  if Screen.Fonts.IndexOf('Calibri')>=0 then begin
    {$IFDEF FPC}
    DefaultFont.Size := 11;//Lazarus seems not to like the height property -> we must use Size
    {$ELSE}
    DefaultFont.Height := -14;
    {$ENDIF}
    DefaultFont.Name := 'Calibri';
  end else begin
    if Screen.Fonts.IndexOf('Tahoma')>=0 then
      DefaultFont.Name := 'Tahoma' else
      DefaultFont.Name := 'Arial';

    {$IFDEF FPC}
      //Lazarus seems not to like the height property -> we must use Size
      {$IFDEF DARWIN}
      DefaultFont.Size := 13;
      {$ELSE}
      DefaultFont.Size := 10;
      {$ENDIF}
    {$ELSE}
    DefaultFont.Height := -13;
    {$ENDIF}
  end;
  {$ENDIF}

  {$ifndef USETMSPACK}
  {$IFDEF MSWINDOWS}
  InitComCtl6;
  {$ENDIF}
  assert(ord(tdfCanBeMinimized)=15);
  {$endif USETMSPACK}

  {$IFNDEF FMX}
  {$IFDEF WITHLAZARUSARROW}
  BitmapArrow := TPicture.Create;
  BitmapArrow.LoadFromLazarusResource('std_arrow16');
  {$ELSE}
  BitmapArrow := TBitmap.Create;
  BitmapArrow.LoadFromResourceName(HInstance,'btnArrow');
  BitmapArrow.Transparent := true;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF FMX}
  ICON_PNG[tiBlank] := StrToBitmap( blank_png_str );
  ICON_PNG[tiWarning] := StrToBitmap( warning_png_str );
  ICON_PNG[tiQuestion] := StrToBitmap( help_png_str );
  ICON_PNG[tiError] := StrToBitmap( error_png_str );
  ICON_PNG[tiInformation] := StrToBitmap( info_png_str );
  ICON_PNG[tiNotUsed] := StrToBitmap( blank_png_str );
  ICON_PNG[tiShield] := StrToBitmap( error_png_str );
  {$ENDIF}

finalization
  DefaultFont.Free;
  {$IFNDEF FMX}
  BitmapArrow.Free;
  {$ENDIF}

end.
