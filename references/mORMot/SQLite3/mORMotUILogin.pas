/// some common User Interface functions and dialogs for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotUILogin;

(*
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2021 Arnaud Bouchez
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

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - igors233

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



    Version 1.4 - February 8, 2010
    - whole Synopse SQLite3 database framework released under the GNU Lesser
      General Public License version 3, instead of generic "Public Domain"

    Version 1.5 - February 18, 2010
    - allow to pickup the User Name from a Combo Box

    Version 1.9
    - improved Delphi 2009/2010 UnicodeString compatibility
    - new PassWord method to enter a password e.g. for Cypher (i.e. with no UserName)

    Version 1.13
    - now use TMS component pack only if USETMSPACK global conditional is defined
    - introducing new TSynButton, mapping either to the default TBitButton,
      either to TAdvGlowButton (if USETMSPACK conditional is defined)
    - introducing a new TTaskDialog record/object, to access the new Vista/Seven
      TaskDialog, with a fallback dialog written in Delphi under XP
    - use a best available font (Calibri or Tahoma), and useful bitmaps

    Version 1.15
    - new InputBox global function
    - new QueryMasked parameter to display * in InputBox/InputQuery editor field

    Version 1.16
    - InputBox function will now focus the input field component by default

    Version 1.18
    - renamed SQLite3UILogin.pas to mORMotUILogin.pas
    - added ShowLastClientError() and ShowException() functions
    - added TLoginForm.OnIdleProcess and OnIdleProcessForm callback events,
      ready to be assigned to TSQLRestClientURI.OnIdle property

*)

interface

{$I Synopse.inc} // define HASINLINE CPU32 CPU64

uses
  {$ifdef MSWINDOWS}
    Windows,
    CommCtrl,
  {$endif}
  {$ifdef FPC}
    //LCLProc, LCLIntf, LCLType,
    LCLType,
    LCLIntf,
    LResources,
    SynTaskDialog, Controls, StdCtrls, ExtCtrls, Classes in '.\Samples\ThirdPartyDemos\Ondrej\SynTaskDialog4Lazarus\SynTaskDialog.pas',
  {$else}
    Consts,
    PsAPI,
    SynTaskDialog,
  {$endif FPC}
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Buttons,
  {$ifdef USETMSPACK}
    AdvGlowButton,
    TaskDialog,
    TaskDialogEx,
    AdvToolBarStylers,
    AdvToolBar,
  {$endif USETMSPACK}
  SynGdiPlus,
  SynCommons,
  SynTable,
  mORMot,
  mORMotUI;

type
  /// Form used to Log User and enter its password
  TLoginForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Image1: TImage;
    Label3: TLabel;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    Edit: TWinControl;
    function EditText: string;
    class procedure HandleApplicationException(Sender: TObject; E: Exception);
  public
    /// display the Login dialog window
    class function Login(const aTitle, aText: string; var aUserName, aPassWord: string;
      AllowUserNameChange: boolean; const CSVComboValues: string): boolean;
    /// display the password dialog window
    class function PassWord(const aTitle, aText: string; var aPassWord: string): boolean;
  public
    /// TOnIdleSQLRestClient-like event to process Windows Messages
    // - to be assigned e.g. to TSQLRestClientURI.OnIdle property
    // - global OnIdleProcessCursorChangeTimeout variable is used to display
    // the crHourGlass cursor after a given time elapsed
    class procedure OnIdleProcess(Sender: TSynBackgroundThreadAbstract; ElapsedMS: Integer);
    /// TOnIdleSQLRestClient-like event to process Windows Messages and write a
    // temporary form on screen if it takes too long
    // - to be assigned e.g. to TSQLRestClientURI.OnIdle property
    // - global OnIdleProcessCursorChangeTimeout variable is used to display
    // the crHourGlass cursor after a given time elapsed
    // - global OnIdleProcessTemporaryFormTimeout variable is used to display
    // a temporary form after a given time elapsed
    class procedure OnIdleProcessForm(Sender: TSynBackgroundThreadAbstract; ElapsedMS: Integer);
  end;

var
  /// define when TLoginForm.OnIdleProcess() has to display the crHourGlass cursor
  // after a given time elapsed, in milliseconds
  // - default is 100 ms
  OnIdleProcessCursorChangeTimeout: integer = 100;

  /// define when TLoginForm.OnIdleProcessForm() has to display the temporary
  // form after a given time elapsed, in milliseconds
  // - default is 2000 ms, i.e. 2 seconds
  OnIdleProcessTemporaryFormTimeout: integer = 2000;

  /// define the message text displayed by TLoginForm.OnIdleProcessForm()
  // - default is sOnIdleProcessFormMessage resourcestring, i.e. 'Please wait...'
  OnIdleProcessTemporaryFormMessage: string;


{$ifdef USETMSPACK}

/// create a temporary AdvTaskDialog component, with the current style
function CreateAdvTaskDialog: TAdvTaskDialogEx;

/// create a temporary AdvInputTaskDialog component, with the current style
function CreateAdvInputTaskDialog: TAdvInputTaskDialogEx;

/// retrieve the main toolbar from main form
function GetMainPager: TAdvToolBarPager;

/// retrieve the main styler from main form
function GetMainStyler: TAdvToolBarOfficeStyler;

/// set the style for a form and a its buttons
// - return the toolbar styler found, if any
function SetStyle(Form: TComponent; Styler: TAdvToolBarOfficeStyler=nil): TAdvToolBarOfficeStyler;

{$else}

/// set the style for a form and a its buttons
// - set the Default Font for all components, i.e. Calibri if available
procedure SetStyle(Form: TComponent);

{$endif USETMSPACK}


/// show an (error) message, using a Vista-Style dialog box
procedure ShowMessage(const Msg: string; Error: boolean=false); overload;

/// convert an error message into html compatible equivalency
// - allow to display < > & correctly
function HtmlEscape(const Msg: string): string;

/// show an (error) message, using a Vista-Style dialog box
procedure ShowMessage(const Msg, Inst: string; Error: boolean=false); overload;

/// show an error dialog box, with the corresponding Client-Side information
// - retrieve last error message from Client.LastError* values
function ShowLastClientError(Client: TSQLRestClientURI; const ContextMessage: string='';
  CommonButtons: TCommonButtons=[cbOk]): integer;

/// show an error dialog box, corresponding to a specified exception
function ShowException(E: Exception; CommonButtons: TCommonButtons=[cbOk];
  const ContextMessage: string=''): integer;

/// ask the User to choose Yes or No [and Cancel], using a Vista-Style dialog box
function YesNo(const aQuestion: string; const aConfirm: string =''; withCancel: boolean=true;
  Warning: boolean=false): integer;

/// ask the User to enter some string value
// - if QueryMasked=TRUE, will mask the prompt with '*' chars (e.g. for entering
// a password)
function InputQuery(const ACaption, APrompt: string; var Value: string;
  QueryMasked: boolean=false): Boolean;

/// ask the User to enter some string value
// - if QueryMasked=TRUE, will mask the prompt with '*' chars (e.g. for entering
// a password)
function InputBox(const ACaption, APrompt, ADefault: string; QueryMasked: boolean=false): string;

/// ask the User to select one item from an array of strings
// - return the selected index, -1 if Cancel button was pressed
function InputSelect(const ACaption, APrompt, AItemsText, ASelectedText: string): integer;

/// ask the User to select one enumerate item
// - use internally TEnumType.GetCaption() to retrieve the text to be displayed
// - Index must be an instance of this enumeration type (internaly mapped to a PByte)
function InputSelectEnum(const ACaption, APrompt: string; EnumTypeInfo: PTypeInfo;
  var Index): boolean;

/// ask the User to choose between some Commands
// - return the selected command index, starting numerotation at 100
function Choose(const aTitle, aContent, aFooter: string; const Commands: array of string;
  aFooterIcon: TTaskDialogFooterIcon=tfiInformation): integer; overload;

/// ask the User to choose between some Commands
// - return the selected command index, starting numerotation at 100
// - this overloaded function expect the Content and the Commands to be
// supplied as CSV string (Content as first CSV, then commands)
function Choose(const aTitle, aCSVContent: string): integer; overload;

resourcestring
  sClientServerError = 'An error occured during Client-Server process';

  sOnIdleProcessFormMessage = 'Please wait...';


/// ensure that the program is launched once
// - the main project .dpr source file must contain:
// !begin
// !  Application.Initialize;
// !  EnsureSingleInstance; // program is launched once
// !  Application.CreateForm(TMainForm, MainForm);
// ! ....
procedure EnsureSingleInstance;

type
  PTPanel = ^TPanel;

/// popup a temporary form with a message over all forms
// - caller should execute result.Free and reset the cursor if needed
function CreateTempForm(const aCaption: string;
  aPanelReference: PTPanel=nil; ScreenCursorHourGlass: boolean=false;
  aCaptionColor: integer=clNavy; aCaptionSize: integer=12): TForm;



implementation

{$ifdef FPC}
{$R *.lfm}
{$else}
{$R *.dfm}
{$endif}

{$R SQLite3UILogin.res}

/// popup a temporary form with a message over all forms
function CreateTempForm(const aCaption: string;
  aPanelReference: PTPanel=nil; ScreenCursorHourGlass: boolean=false;
  aCaptionColor: integer=clNavy; aCaptionSize: integer=12): TForm;
var P: TPanel;
begin
  if ScreenCursorHourGlass then
    Screen.Cursor := crHourGlass;
  Result := TForm.Create(nil);
  Result.Width := 400; Result.Height := 100;
  Result.Position := poScreenCenter;
  Result.FormStyle := fsStayOnTop;
  Result.BorderStyle := bsNone;
  P := TPanel.Create(Result);
  with P do begin
    Color := clWhite;
    BorderStyle := bsSingle;
    Parent := Result;
    Font.Name := 'Tahoma'; Font.Size := aCaptionSize;
    Font.Color := aCaptionColor;  Font.Style := [fsBold];
    Caption := aCaption;
    Align := alClient;
  end;
  if aPanelReference<>nil then
    aPanelReference^ := P;
{  with TImage.Create(Result) do begin
    Parent := P;
    Transparent := true;
    Picture.Bitmap.LoadFromResourceName(hInstance,'ROC');
    SetBounds((330-156)div 2,20,156,73);
  end;
  with TLabel.Create(Result) do begin
    Parent := P;
    Font.Name := 'Tahoma'; Font.Size := 11;
    Font.Color := clNavy;  Font.Style := [fsBold];
    Alignment := taCenter;
    Caption := aCaption;
    SetBounds(0,120,330,100);
  end; }
  Result.Show;
  Application.ProcessMessages;
end;

procedure ShowMessage(const Msg: string; Error: boolean=false);
begin
  if Error then
    ShowMessage(HtmlEscape(Msg),SMsgDlgError,Error) else
    ShowMessage(HtmlEscape(Msg),SMsgDlgInformation,Error);
end;

function HtmlEscape(const Msg: string): string;
{$ifdef USETMSPACK}
var i: integer;
    ins: string;
{$endif}
begin
  result := Msg;
{$ifdef USETMSPACK}
  for i := length(Msg) downto 1 do begin
    case Msg[i] of
      '"': ins := '&quot';
      '&': ins := '&amp';
      '<': ins := '&lt';
      '>': ins := '&gt';
      else Continue;
    end;
    result[i] := ';';
    insert(ins,result,i);
   end;
{$endif}
end;


{$ifdef USETMSPACK}

function CreateAdvTaskDialog: TAdvTaskDialogEx;
var Style: TAdvToolBarOfficeStyler;
begin
  result := TAdvTaskDialogEx.Create(Application);
  if Application.MainForm=nil then
    result.Title := Application.Title else
    result.Title := Application.MainForm.Caption;
  result.Options := result.Options+[doHyperlinks];
  result.NonNativeDialog := nndAlways; // we need hyperlinks=HTML -> non native!
  Style := GetMainStyler;
  if Style<>nil then
    result.Appearance := Style.GlowButtonAppearance;
end;

function Choose(const aTitle, aContent, aFooter: string; const Commands: array of string;
  aFooterIcon: TTaskDialogFooterIcon=tfiInformation): integer;
var i: integer;
begin
  with TAdvTaskDialog(CreateAdvTaskDialog) do
  try
    Instruction := aTitle;
    Content := aContent;
    Icon := tiQuestion;
    CustomButtons.Clear;
    for i := 0 to high(Commands) do
      CustomButtons.Add(Commands[i]);
    DefaultButton := 100;
    Options := Options+[doCommandLinks];
    if aFooter<>'' then begin
      FooterIcon := aFooterIcon;
      if aFooterIcon=tfiWarning then
        Footer := '<b>'+SMsgDlgWarning+'</b>:'#13+aFooter else
        Footer := aFooter;
    end;
    result := Execute;
  finally
    Free;
  end;
end;

type
  /// we definitively NEED these properties
  TAdvInputTaskDialogPublished = class(TAdvInputTaskDialogEx)
  published
    property Options;
    property NonNativeDialog;
  end;

function CreateAdvInputTaskDialog: TAdvInputTaskDialogEx;
var Style: TAdvToolBarOfficeStyler;
begin
  result := TAdvInputTaskDialogPublished.Create(Application);
  result.Title := Application.MainForm.Caption;
  TAdvInputTaskDialogPublished(result).Options := TAdvInputTaskDialogPublished(result).Options+[doHyperlinks];
  TAdvInputTaskDialogPublished(result).NonNativeDialog := nndAlways; // we need hyperlinks=HTML -> non native!
  Style := GetMainStyler;
  if Style<>nil then
    result.Appearance := Style.GlowButtonAppearance;
end;

function InputQuery(const ACaption, APrompt: string; var Value: string;
  QueryMasked: boolean=false): Boolean;
begin
  with CreateAdvInputTaskDialog do
  try
    Instruction := ACaption;
    Content := APrompt;
    InputType := itEdit;
    CommonButtons := [cbOk,cbCancel];
    InputText := Value;
    result := false;
    if Execute<>mrOk then
      exit;
    result := true;
    Value := InputText;
  finally
    Free;
  end;
end;

function InputSelect(const ACaption, APrompt, AItemsText, ASelectedText: string): integer;
var Items: TStringList;
begin
  result := -1;
  if AItemsText<>'' then
  with CreateAdvInputTaskDialog do
  try
    Instruction := ACaption;
    Content := APrompt;
    InputType := itComboList;
    CommonButtons := [cbOk,cbCancel];
    Items := TStringList.Create;
    try
      Items.Text := AItemsText;
      InputItems := Items;
      InputText := ASelectedText;
      if Execute<>mrOk then
        exit;
      result := Items.IndexOf(InputText);
    finally
      Items.Free;
    end;
  finally
    Free;
  end;
end;

function Choose(const aTitle, aCSVContent: string): integer; overload;
var Commands: array of string;
    Content: string;
    P: PChar;
begin
  P := pointer(aCSVContent);
  Content := GetNextItemString(P);
  while P<>nil do begin
    SetLength(Commands,length(Commands)+1);
    Commands[high(Commands)] := GetNextItemString(P);
  end;
  result := Choose(aTitle,Content,'',Commands);
end;

{$else}

function Choose(const aTitle, aContent, aFooter: string; const Commands: array of string;
  aFooterIcon: TTaskDialogFooterIcon=tfiInformation): integer;
var Task: TTaskDialog;
    i: integer;
begin
  Task.Inst := aTitle;
  Task.Content := aContent;
  Task.Footer := aFooter;
  for i := 0 to high(Commands) do
    Task.Buttons := Task.Buttons+Commands[i]+#10;
  result := Task.Execute([],100,[tdfUseCommandLinks],tiQuestion,aFooterIcon);
  if result<100 then
    result := -1 else
    dec(result,100);
end;

function InputQuery(const ACaption, APrompt: string; var Value: string;
  QueryMasked: boolean=false): Boolean;
const FLAGS: array[boolean] of TTaskDialogFlags = (
   [tdfQuery,tdfQueryFieldFocused],[tdfQuery,tdfQueryMasked,tdfQueryFieldFocused]);
var Task: TTaskDialog;
begin
  Task.Inst := ACaption;
  Task.Content := APrompt;
  Task.Query := Value;
  result := Task.Execute([cbOk,cbCancel],0,FLAGS[QueryMasked],tiQuestion)=mrOk;
  if result then
    Value := Task.Query;
end;

function InputSelect(const ACaption, APrompt, AItemsText, ASelectedText: string): integer;
var Task: TTaskDialog;
begin
  result := -1;
  if AItemsText='' then
    exit;
  Task.Inst := ACaption;
  Task.Content := APrompt;
  Task.Selection := AItemsText;
  Task.Query := ASelectedText;
  if Task.Execute([cbOk,cbCancel],0,[],tiQuestion)=mrOk then
    result := Task.SelectionRes;
end;

function Choose(const aTitle, aCSVContent: string): integer; overload;
var Task: TTaskDialog;
    P: PChar;
begin
  Task.Inst := aTitle;
  P := pointer(aCSVContent);
  Task.Content := GetNextItemString(P);
  while P<>nil do
    Task.Buttons := Task.Buttons+GetNextItemString(P)+#10;
  result := Task.Execute([],100,[tdfUseCommandLinks],tiQuestion);
  if result<100 then
    result := -1 else
    dec(result,100);
end;

{$endif USETMSPACK}

function InputBox(const ACaption, APrompt, ADefault: string; QueryMasked: boolean=false): string;
begin
  result := ADefault;
  if not InputQuery(ACaption,APrompt,Result,QueryMasked) then
     result := ADefault;
end;

function InputSelectEnum(const ACaption, APrompt: string; EnumTypeInfo: PTypeInfo;
  var Index): boolean;
var i: integer;
begin
  result := false;
  if (EnumTypeInfo<>nil) and (EnumTypeInfo^.Kind=tkEnumeration) then begin
    i := InputSelect(ACaption,APrompt,EnumTypeInfo^.EnumBaseType^.GetCaptionStrings,
     EnumTypeInfo^.EnumBaseType^.GetCaption(PByte(@Index)^));
    if i>=0 then begin
      PByte(@Index)^ := i;
      result := true;
    end;
  end;
end;

procedure ShowMessage(const Msg, Inst: string; Error: boolean=false);
const
  IconError: array[boolean] of TTaskDialogIcon = (tiInformation, tiError);
{$ifndef USETMSPACK}
var Task: TTaskDialog;
{$endif}
begin
{$ifdef USETMSPACK}
  with CreateAdvTaskDialog do
  try
    Instruction := Inst;
    Icon := IconError[Error];
    Content := Msg;
    CommonButtons := [cbOk];
    Execute;
  finally
    Free;
  end;
{$else}
  Task.Inst := Inst;
  Task.Content := Msg;
  Task.Execute([cbOK],mrOk,[],IconError[Error]);
{$endif}
end;

function ShowLastClientError(Client: TSQLRestClientURI; const ContextMessage: string='';
  CommonButtons: TCommonButtons=[cbOk]): integer;
var details: string;
{$ifndef USETMSPACK}
    Task: TTaskDialog;
{$endif}
begin
  if Client<>nil then begin
    if Client.LastErrorMessage<>'' then
      details := UTF8ToString(Client.LastErrorMessage)+#13#10;
    if Client.LastErrorException<>nil then
      details := Format('%sException %s raised - ErrorCode=%d',
        [details,Client.LastErrorException.ClassName,Client.LastErrorCode]);
  end;
{$ifdef USETMSPACK}
  with CreateAdvTaskDialog do
  try
    Instruction := sClientServerError;
    Content := ContextMessage;
    Icon := tiError;
    CommonButtons := [cbOk];
    result := Execute;
  finally
    Free;
  end;
{$else}
  if ContextMessage='' then
    Task.Content := sClientServerError else
    Task.Content := ContextMessage;
  Task.Info := details;
  result := Task.Execute([cbOK],mrOk,[],tiError);
{$endif}
end;

function ShowException(E: Exception; CommonButtons: TCommonButtons;
  const ContextMessage: string): integer;
var details: string;
{$ifndef USETMSPACK}
    Task: TTaskDialog;
{$endif}
begin
  if E=nil then begin
    ShowMessage(ContextMessage,true);
    result := mrNone;
    exit;
  end;
  details := UTF8ToString(ObjectToJSONDebug(E));
{$ifdef USETMSPACK}
  with CreateAdvTaskDialog do
  try
    Instruction := sClientServerError;
    Content := ContextMessage;
    Icon := tiError;
    CommonButtons := [cbOk];
    result := Execute;
  finally
    Free;
  end;
{$else}
  Task.Content := E.Message;
  Task.Inst := ContextMessage;
  Task.Info := details;
  result := Task.Execute(CommonButtons,mrOk,[],tiError);
{$endif}
end;

function YesNo(const aQuestion, aConfirm: string; withCancel: boolean; Warning: boolean): integer;
var Confirm: string;
{$ifndef USETMSPACK}
    Task: TTaskDialog;
{$endif}
const
  IconWarning: array[boolean] of TTaskDialogIcon = (
    tiQuestion, tiWarning);
  BtsCancel: array[boolean] of TCommonButtons = (
    [cbYes, cbNo], [cbYes, cbNo, cbCancel]);
begin
  if aConfirm='' then
    Confirm := SMsgDlgConfirm else
    Confirm := aConfirm;
{$ifdef USETMSPACK}
  with CreateAdvTaskDialog do
  try
    Instruction := Confirm;
    Icon := IconWarning[Warning];
    Content := aQuestion;
    CommonButtons := BtsCancel[withCancel];
    result := Execute;
  finally
    Free;
  end;
{$else}
  Task.Inst := Confirm;
  Task.Content := aQuestion;
  result := Task.Execute(BtsCancel[withCancel],0,[],IconWarning[Warning]);
{$endif}
end;

{$ifdef Crazy}
function FileNameFromWndMatch(const Wnd: HWnd; const AppFileName: string): boolean;
var ProcessID: DWORD;
    hProc: THandle;
    tmp: array[0..MAX_PATH] of char;
begin
  GetWindowThreadProcessId(Wnd, @ProcessId);
  hProc := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, ProcessId);
  if hProc <> 0 then begin
    {$ifdef FPC}
    if GetModuleFileName(hProc, tmp, MAX_PATH) = 0 then
    {$else}
    if GetModuleFileNameEx(hProc, 0, tmp, MAX_PATH) = 0 then
    {$endif}
      tmp[0] := #0;
    CloseHandle(hProc);
    result := ExtractFileName(string(tmp)) = AppFileName;
  end else
    result := false;
end;
{$endif Crazy}

procedure EnsureSingleInstance;
var Wnd: HWnd;
    ToFindClass, WndClass, AppFileName: string;
    tmp: array[byte] of char;
    function FileNameFromWndMatch: boolean;
    var ProcessID: DWORD;
        hProc: THandle;
        tmp: array[0..MAX_PATH] of char;
    begin
      GetWindowThreadProcessId(Wnd, @ProcessId);
      hProc := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, ProcessId);
      if hProc <> 0 then begin
        {$ifdef FPC}
        if GetModuleFileName(hProc, tmp, MAX_PATH) = 0 then
        {$else}
        if GetModuleFileNameEx(hProc, 0, tmp, MAX_PATH) = 0 then
        {$endif}
          tmp[0] := #0;
        CloseHandle(hProc);
        result := ExtractFileName(string(tmp)) = AppFileName;
      end else
        result := false;
    end;
begin
  if Application = nil then
    exit;
  { Try and create a semaphore. If we succeed, then check if the semaphore was
    already present. If it was then a previous instance is floating around.
    Note the OS will free the returned semaphore handle when the app shuts
    so we can forget about it }
  AppFileName := ExtractFileName(ExeVersion.ProgramFileName);
  if (CreateSemaphore(nil, 0, 1, Pointer(AppFileName)) <> 0) and
     (GetLastError = ERROR_ALREADY_EXISTS) then begin
    if GetClassName({$ifdef FPC}Application.MainFormHandle{$else}Application.Handle{$endif}, tmp, high(tmp)) = 0 then
      exit;
    ToFindClass := tmp;
    Wnd := GetWindow({$ifdef FPC}Application.MainFormHandle{$else}Application.Handle{$endif}, GW_HWNDFIRST);
    while Wnd <> 0 do begin
      { Look for the other TApplication window out there }
      if Wnd <> {$ifdef FPC}Application.MainFormHandle{$else}Application.Handle{$endif} then begin
        { Check if got the same class and filename }
        if GetClassName(Wnd, tmp, high(tmp)) = 0 then
          tmp[0] := #0;
        WndClass := tmp;
        if (ToFindClass = WndClass) and FileNameFromWndMatch then begin
          { This technique is used by the VCL: post a message then bring the
            window to the top, before the message gets processed }
          PostMessage(Wnd, WM_SYSCOMMAND, SC_RESTORE, 0);
          SetForegroundWindow(Wnd);
          break;
        end
      end;
      Wnd := GetWindow(Wnd, GW_HWNDNEXT);
    end;
    AppFileName := ''; // avoid memory leak due to Halt brutal execution
    WndClass := '';
    ToFindClass := '';
    Halt; // Stop this new instance
  end
end;

procedure TLoginForm.FormCreate(Sender: TObject);
var P: TSynPicture;
begin
  SetStyle(self);
  TSynButton.CreateKind(Self,cbOK,136,163,75,30).Anchors := [akLeft,akBottom];
  TSynButton.CreateKind(Self,cbCancel,224,163,75,30).Anchors := [akLeft,akBottom];
  P := TSynPicture.Create;
  try
    P.LoadFromResourceName(HInstance,'UILogin'); // SQLite3uilogin.png
    Image1.Picture.Assign(P);
  finally
    P.Free;
  end;
end;

class function TLoginForm.PassWord(const aTitle, aText: string;
  var aPassWord: string): boolean;
var LoginForm: TLoginForm;
begin
  Application.CreateForm(TLoginForm,LoginForm);
  try
    with LoginForm do begin
      Label1.Hide;
      Label3.Caption := aText;
      Label3.Width := LoginForm.ClientWidth;
      Bevel1.Hide;
      ClientHeight := 180;
      LoginForm.Caption := ' '+aTitle;
      result := (LoginForm.ShowModal=mrOk);
      if result then
        aPassWord := SysUtils.trim(LoginForm.Edit2.Text);
    end;
  finally
    LoginForm.Free;
  end;
end;

class function TLoginForm.Login(const aTitle, aText: string; var aUserName, aPassWord: string;
  AllowUserNameChange: boolean; const CSVComboValues: string): boolean;
var T: string;
    i: integer;
    LoginForm: TLoginForm;
begin
  Application.CreateForm(TLoginForm,LoginForm);
  with LoginForm do
  try
    Label3.Caption := aText;
    Label3.Width := LoginForm.ClientWidth;
    Bevel1.Visible := aText<>'';
    Height := 240+Label3.Height;
    Bevel1.Height := Label3.Height;
    if not AllowUserNameChange or (CSVComboValues='') then begin
      Edit := TEdit.Create(LoginForm);
      Edit.Parent := LoginForm;
      TEdit(Edit).Text := aUserName;
    end else begin
      Edit := TComboBox.Create(LoginForm);
      Edit.Parent := LoginForm;
      with TComboBox(Edit) do begin
        Text := aUserName;
        Style := csDropDownList;
        Items.Text := StringReplace(CSVComboValues,',',#13#10,[rfReplaceAll]);
        ItemIndex := Items.IndexOf(aUserName);
      end;
    end;
    {$ifdef FPC}
    // strange, but needed
    Edit.SetBounds(104,Height-Edit2.Height-84,193,22);
    {$else}
    Edit.SetBounds(104,Edit2.Top-32,193,22);
    {$endif}
    Edit.Anchors := [akLeft,akBottom];
    Edit.Enabled := AllowUserNameChange;
    Edit2.Text := '';
    if aTitle='' then
      if Application.MainForm=nil then
        T := Application.Title else
        T := Application.MainForm.Caption else begin
      T := aTitle;
      for i := 1 to length(T) do
        if T[i]<' ' then
          T[i] := ' ';
    end;
    Caption := ' '+T;
    result := (ShowModal=mrOk);
    if result then begin
      aPassWord := SysUtils.trim(Edit2.Text);
      if Edit.Enabled then
        aUserName := SysUtils.trim(EditText);
    end;
  finally
    LoginForm.Free;
  end;
end;

{$ifdef USETMSPACK}

function GetMainPager: TAdvToolBarPager;
var C: integer;
begin
  with Application.MainForm do
    for C := 0 to ComponentCount-1 do begin
      result := pointer(Components[C]);
      if result.InheritsFrom(TAdvToolBarPager) then
        exit;
    end;
  result := nil;
end;

var
 fMainStyler: TAdvToolBarOfficeStyler = nil;

function GetMainStyler: TAdvToolBarOfficeStyler;
var C: integer;
begin
  result := fMainStyler;
  if (result<>nil) or (Application.MainForm=nil) then
    exit;
  with Application.MainForm do
    for C := 0 to ComponentCount-1 do begin
      result := pointer(Components[C]);
      if result.InheritsFrom(TAdvToolBarOfficeStyler) then begin
        fMainStyler := result;
        exit;
      end;
    end;
  result := nil;
end;

function SetStyle(Form: TComponent; Styler: TAdvToolBarOfficeStyler=nil): TAdvToolBarOfficeStyler;
var i: integer;
    C: TComponent;
begin
  if Styler=nil then
    result := GetMainStyler else
    result := Styler;
  if (result=nil) or (Form=nil) then
    exit;
  for i := 0 to Form.ComponentCount-1 do begin
    C := Form.Components[i];
{    if C.InheritsFrom(TAdvToolBarOfficeStyler) then // buggy: TMS :(
      TAdvToolBarOfficeStyler(C).Assign(Style) else }
    if C.InheritsFrom(TAdvGlowButton) then
      TAdvGlowButton(C).Appearance := result.GlowButtonAppearance;
    SetStyle(C,result);
  end;
  if Form.InheritsFrom(TCustomForm) then
    TCustomForm(Form).Color := result.PageAppearance.Color;
  if Form.InheritsFrom(TAdvGlowButton) then
    TAdvGlowButton(Form).Appearance := result.GlowButtonAppearance;
end;

{$else}

procedure SetStyle(Form: TComponent);
var i, f: integer;
    C: TComponent;
    Ctrl: TControl absolute C;
    CL: TClass;
    P: PPropInfo;
    Obj: TObject;
begin
  if Form=nil then
    exit;
  // set form font to global Default Font
  if Form.InheritsFrom(TCustomForm) then
    TCustomForm(Form).Font := DefaultFont;
  // for any component with ParentFont=false, force Calibri if necessary
  if DefaultFont.Name='Calibri' then
  for i := 0 to Form.ComponentCount-1 do begin
    C := Form.Components[i];
    if C.InheritsFrom(TControl) then
    if not TButton(C).ParentFont then begin // trick to access TControl.FParentFont
      CL := PPointer(C)^;
      while (CL<>nil) and (CL<>TComponent) and (CL<>TObject) do begin
        for f := 1 to InternalClassPropInfo(CL,P) do begin
          with P^.PropType^{$ifndef HASDIRECTTYPEINFO}^{$endif} do
          if (Kind=tkClass) and ClassType^.InheritsFrom(TFont) then begin
            Obj := P^.GetObjProp(C);
            if Obj<>nil then
              with TFont(Obj) do
              if Name<>DefaultFont.Name then begin
                Name := DefaultFont.Name;
                Height := Height+(Height div 5);
              end;
          end;
          P := P^.Next;
        end;
        CL := CL.ClassParent; // handle parent published properties
      end;
    end;
  end;
end;

{$endif USETMSPACK}

procedure TLoginForm.FormShow(Sender: TObject);
begin
  if (Edit<>nil) and Edit.Enabled and (EditText='') then
    Edit.SetFocus else
    Edit2.SetFocus;
  SetStyle(self);
end;

function TLoginForm.EditText: string;
begin
  if Edit.InheritsFrom(TComboBox) then
    result := TComboBox(Edit).Text else
    result := TEdit(Edit).Text;
  result := SysUtils.trim(result);
end;

class procedure TLoginForm.HandleApplicationException(Sender: TObject; E: Exception);
begin // so that exception will be shown with new User Interface
  ShowMessage(E.Message,true);
end;

var
  OnIdleProcessCursor: TCursor = crNone;
  OnIdleProcessTempForm: TForm;

class procedure TLoginForm.OnIdleProcess(Sender: TSynBackgroundThreadAbstract; ElapsedMS: Integer);
begin
  Application.ProcessMessages;
  if (ElapsedMS>OnIdleProcessCursorChangeTimeout) and (OnIdleProcessCursor=crNone) then begin
    OnIdleProcessCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
  end else
  if (ElapsedMS<0) and (OnIdleProcessCursor<>crNone) then begin
    Screen.Cursor := OnIdleProcessCursor;
    OnIdleProcessCursor := crNone;
  end;
end;

class procedure TLoginForm.OnIdleProcessForm(Sender: TSynBackgroundThreadAbstract; ElapsedMS: Integer);
var msg: string;
begin
  TLoginForm.OnIdleProcess(Sender,ElapsedMS);
  if (ElapsedMS>OnIdleProcessTemporaryFormTimeout) and
     not Assigned(OnIdleProcessTempForm) then begin
    if OnIdleProcessTemporaryFormMessage='' then
      msg := sOnIdleProcessFormMessage else
      msg := OnIdleProcessTemporaryFormMessage;
    OnIdleProcessTempForm := CreateTempForm(msg);
  end else
  if (ElapsedMS<0) and Assigned(OnIdleProcessTempForm) then
    FreeAndNil(OnIdleProcessTempForm);
end;

initialization
  Gdip.RegisterPictures; // will initialize the Gdip library if necessary
  Application.OnException := TLoginForm.HandleApplicationException;

end.


