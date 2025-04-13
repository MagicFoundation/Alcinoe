unit Alcinoe.FMX.DesignEditors;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Editor.Items.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  System.Classes,
  FMX.Types,
  FMX.Editor.Items,
  DesignIntf,
  DesignEditors,
  DesignMenus,
  StrEdit;

resourcestring
  SItems = 'Items';
  SNextPage = 'Next Page';
  SPrevPage = 'Previous Page';
  SUnnamedPage = 'Unnamed %1:s %0:d';
  SDeleteItem = 'Delete %s';
  SSetActive = 'Set Active';

const
  EDITOR_SET_ACTIVE = 0;

type

  {***********************************}
  TALEditEditor = class(TDefaultEditor)
  protected
    procedure ApplyThemeClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {***********************************}
  TALMemoEditor = class(TDefaultEditor)
  protected
    procedure ApplyThemeClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {*************************************}
  TALButtonEditor = class(TDefaultEditor)
  protected
    procedure ApplyThemeClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {***************************************}
  TALCheckBoxEditor = class(TDefaultEditor)
  protected
    procedure ApplyThemeClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {******************************************}
  TALRadioButtonEditor = class(TDefaultEditor)
  protected
    procedure ApplyThemeClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {*************************************}
  TALSwitchEditor = class(TDefaultEditor)
  protected
    procedure ApplyThemeClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {***************************************}
  TALTrackBarEditor = class(TDefaultEditor)
  protected
    procedure ApplyThemeClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {********************************************}
  TALRangeTrackBarEditor = class(TDefaultEditor)
  protected
    procedure ApplyThemeClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {*****************************************}
  TALPageControllerEditor = class(TItemsEditor)
  private
    FEditorNextPage: Integer;
    FEditorPrevPage: Integer;
    FEditorDeletePage: Integer;
    FVerbCount: Integer;
    function GetActivePageIndex: Integer;
  protected
    procedure DoCreateItem(Sender: TObject); override;
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {****************************************}
  TALPageViewEditor = class(TComponentEditor)
  private
  protected
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Skia.Designtime.pas was not updated and adjust the IFDEF'}
  {$ENDIF}

  {****************************************************}
  TALTextTextPropertyEditor = class(TStringListProperty)
  private
    FStrings: TStrings;
  protected
    function GetStrings: TStrings; override;
    procedure SetStrings(const AValue: TStrings); override;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
  end;

  {************************************************}
  TALGradientPropertyEditor = class(TPropertyEditor)
  protected
    function GetIsDefault: Boolean; override;
  public
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  Vcl.Menus,
  FMX.Design.Items,
  Alcinoe.Common,
  Alcinoe.StringList,
  Alcinoe.FMX.Themes,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.Memo,
  Alcinoe.fmx.common,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.PageController;

{*****************************************************}
function TALEditEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Theme';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{*******************************************}
function TALEditEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{*******************************************************}
procedure TALEditEditor.ApplyThemeClick(Sender: TObject);
begin
  var LTheme := TmenuItem(Sender).Caption;
  LTheme := StringReplace(LTheme, '&','',[rfReplaceALL]);
  ALApplyEditTheme(LTheme, TALEdit(Component));
end;

{**************************************************************************}
procedure TALEditEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LKeys := TALStringListW.create;
  try
    for var LKeyValue in ALEditThemes do
      LKeys.Add(LKeyValue.Key);
    LKeys.Sort;
    for var I := 0 to LKeys.Count - 1 do
      AItem.AddItem(LKeys[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyThemeClick{AOnClick}, 0{hCtx}, ''{AName});
  finally
    ALFreeAndNil(LKeys);
  end;
end;

{*****************************************************}
function TALMemoEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Theme';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{*******************************************}
function TALMemoEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{*******************************************************}
procedure TALMemoEditor.ApplyThemeClick(Sender: TObject);
begin
  var LTheme := TmenuItem(Sender).Caption;
  LTheme := StringReplace(LTheme, '&','',[rfReplaceALL]);
  ALApplyMemoTheme(LTheme, TALMemo(Component));
end;

{**************************************************************************}
procedure TALMemoEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LKeys := TALStringListW.create;
  try
    for var LKeyValue in ALMemoThemes do
      LKeys.Add(LKeyValue.Key);
    LKeys.Sort;
    for var I := 0 to LKeys.Count - 1 do
      AItem.AddItem(LKeys[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyThemeClick{AOnClick}, 0{hCtx}, ''{AName});
  finally
    ALFreeAndNil(LKeys);
  end;
end;

{*******************************************************}
function TALButtonEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Theme';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{*********************************************}
function TALButtonEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{*********************************************************}
procedure TALButtonEditor.ApplyThemeClick(Sender: TObject);
begin
  var LTheme := TmenuItem(Sender).Caption;
  LTheme := StringReplace(LTheme, '&','',[rfReplaceALL]);
  ALApplyButtonTheme(LTheme, TALButton(Component));
end;

{****************************************************************************}
procedure TALButtonEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LKeys := TALStringListW.create;
  try
    for var LKeyValue in ALButtonThemes do
      LKeys.Add(LKeyValue.Key);
    LKeys.Sort;
    for var I := 0 to LKeys.Count - 1 do
      AItem.AddItem(LKeys[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyThemeClick{AOnClick}, 0{hCtx}, ''{AName});
  finally
    ALFreeAndNil(LKeys);
  end;
end;

{*********************************************************}
function TALCheckBoxEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Theme';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{***********************************************}
function TALCheckBoxEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{***********************************************************}
procedure TALCheckBoxEditor.ApplyThemeClick(Sender: TObject);
begin
  var LTheme := TmenuItem(Sender).Caption;
  LTheme := StringReplace(LTheme, '&','',[rfReplaceALL]);
  ALApplyCheckBoxTheme(LTheme, TALCheckBox(Component));
end;

{******************************************************************************}
procedure TALCheckBoxEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LKeys := TALStringListW.create;
  try
    for var LKeyValue in ALCheckBoxThemes do
      LKeys.Add(LKeyValue.Key);
    LKeys.Sort;
    for var I := 0 to LKeys.Count - 1 do
      AItem.AddItem(LKeys[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyThemeClick{AOnClick}, 0{hCtx}, ''{AName});
  finally
    ALFreeAndNil(LKeys);
  end;
end;

{************************************************************}
function TALRadioButtonEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Theme';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{**************************************************}
function TALRadioButtonEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{**************************************************************}
procedure TALRadioButtonEditor.ApplyThemeClick(Sender: TObject);
begin
  var LTheme := TmenuItem(Sender).Caption;
  LTheme := StringReplace(LTheme, '&','',[rfReplaceALL]);
  ALApplyRadioButtonTheme(LTheme, TALRadioButton(Component));
end;

{*********************************************************************************}
procedure TALRadioButtonEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LKeys := TALStringListW.create;
  try
    for var LKeyValue in ALRadioButtonThemes do
      LKeys.Add(LKeyValue.Key);
    LKeys.Sort;
    for var I := 0 to LKeys.Count - 1 do
      AItem.AddItem(LKeys[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyThemeClick{AOnClick}, 0{hCtx}, ''{AName});
  finally
    ALFreeAndNil(LKeys);
  end;
end;

{*******************************************************}
function TALSwitchEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Theme';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{*********************************************}
function TALSwitchEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{*********************************************************}
procedure TALSwitchEditor.ApplyThemeClick(Sender: TObject);
begin
  var LTheme := TmenuItem(Sender).Caption;
  LTheme := StringReplace(LTheme, '&','',[rfReplaceALL]);
  ALApplySwitchTheme(LTheme, TALSwitch(Component));
end;

{****************************************************************************}
procedure TALSwitchEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LKeys := TALStringListW.create;
  try
    for var LKeyValue in ALSwitchThemes do
      LKeys.Add(LKeyValue.Key);
    LKeys.Sort;
    for var I := 0 to LKeys.Count - 1 do
      AItem.AddItem(LKeys[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyThemeClick{AOnClick}, 0{hCtx}, ''{AName});
  finally
    ALFreeAndNil(LKeys);
  end;
end;

{*********************************************************}
function TALTrackBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Theme';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{***********************************************}
function TALTrackBarEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{***********************************************************}
procedure TALTrackBarEditor.ApplyThemeClick(Sender: TObject);
begin
  var LTheme := TmenuItem(Sender).Caption;
  LTheme := StringReplace(LTheme, '&','',[rfReplaceALL]);
  ALApplyTrackBarTheme(LTheme, TALTrackBar(Component));
end;

{******************************************************************************}
procedure TALTrackBarEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LKeys := TALStringListW.create;
  try
    for var LKeyValue in ALTrackBarThemes do
      LKeys.Add(LKeyValue.Key);
    LKeys.Sort;
    for var I := 0 to LKeys.Count - 1 do
      AItem.AddItem(LKeys[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyThemeClick{AOnClick}, 0{hCtx}, ''{AName});
  finally
    ALFreeAndNil(LKeys);
  end;
end;

{**************************************************************}
function TALRangeTrackBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Theme';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{****************************************************}
function TALRangeTrackBarEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{****************************************************************}
procedure TALRangeTrackBarEditor.ApplyThemeClick(Sender: TObject);
begin
  var LTheme := TmenuItem(Sender).Caption;
  LTheme := StringReplace(LTheme, '&','',[rfReplaceALL]);
  ALApplyRangeTrackBarTheme(LTheme, TALRangeTrackBar(Component));
end;

{***********************************************************************************}
procedure TALRangeTrackBarEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LKeys := TALStringListW.create;
  try
    for var LKeyValue in ALRangeTrackBarThemes do
      LKeys.Add(LKeyValue.Key);
    LKeys.Sort;
    for var I := 0 to LKeys.Count - 1 do
      AItem.AddItem(LKeys[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyThemeClick{AOnClick}, 0{hCtx}, ''{AName});
  finally
    ALFreeAndNil(LKeys);
  end;
end;

{***********************************************************************************}
constructor TALPageControllerEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FEditorNextPage := inherited GetVerbCount;
  FEditorPrevPage := FEditorNextPage + 1;
  FEditorDeletePage := FEditorNextPage + 2;
  FVerbCount := FEditorNextPage + 3;
  FAllowChild := False;
  SetLength(FItemsClasses, 1);
  FItemsClasses[0] := TItemClassDesc.Create(TALPageView);
end;

{**********************************************************}
procedure TALPageControllerEditor.DoCreateItem(Sender: TObject);
begin
  inherited;
  if (Component is TALPageController) then
    TALPageController(Component).ActivePageIndex := TALPageController(Component).PageCount - 1;
end;

{************************************************}
function TALPageControllerEditor.GetActivePageIndex: Integer;
begin
  if (Component is TALPageController) then
    Result := TALPageController(Component).ActivePageIndex
  else
    Result := -1;
end;

{********************************************************}
procedure TALPageControllerEditor.ExecuteVerb(Index: Integer);
var
  LControl: TALPageController;

  procedure SelectPage(I: Integer);
  begin
    if I >= LControl.PageCount then I := LControl.PageCount - 1;
    if I < 0 then I := 0;
    if (I >= 0) and (LControl.PageCount > 0) then begin
      LControl.ActivePageIndex := I;
      Designer.SelectComponent(LControl.Pages[I]);
    end
    else
      Designer.SelectComponent(Component);
  end;

begin
  inherited;
  if Component is TALPageController then
  begin
    var LPageIndex := GetActivePageIndex;
    LControl := TALPageController(Component);
    if Index = FEditorNextPage then SelectPage(LPageIndex + 1)
    else if Index = FEditorPrevPage then SelectPage(LPageIndex - 1)
    else if (Index = FEditorDeletePage) and (LPageIndex >= 0) then begin
      LControl.DeletePage(LPageIndex);
      SelectPage(GetActivePageIndex);
    end;
  end;
end;

{***********************************************************}
function TALPageControllerEditor.GetVerb(Index: Integer): string;
var
  S: string;
begin
  Result := Inherited GetVerb(Index);
  if Component is TALPageController then
    if Index = FEditorNextPage then
      Result := SNextPage
    else if Index = FEditorPrevPage then
      Result := SPrevPage
    else if Index = FEditorDeletePage then
    begin
      if GetActivePageIndex >= 0 then
      begin
        S := TALPageController(Component).Pages[GetActivePageIndex].Name;
        if S = '' then
          S := Format(SUnnamedPage, [GetActivePageIndex, TALPageController(Component).Pages[GetActivePageIndex].ClassName])
        else
          S := QuotedStr(S);
        Result := Format(SDeleteItem, [S]);
      end
      else
        Result := Format(SDeleteItem, ['']);
    end;
end;

{*************************************************}
function TALPageControllerEditor.GetVerbCount: Integer;
begin
  Result := FVerbCount;
end;

{********************************************************************************}
procedure TALPageControllerEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  inherited;
  if Component is TALPageController then
  begin
    if Index = FEditorNextPage then
      AItem.Enabled := GetActivePageIndex < TALPageController(Component).PageCount - 1
    else if Index = FEditorPrevPage then
      AItem.Enabled := GetActivePageIndex > 0
    else if Index = FEditorDeletePage then
      AItem.Enabled := GetActivePageIndex >= 0;
  end
  else
    AItem.Visible := False;
end;

{*****************************************************}
procedure TALPageViewEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    EDITOR_SET_ACTIVE:
      if (Component is TALPageView) then
        TALPageView(Component).Active := true;
  end;
end;

{********************************************************}
function TALPageViewEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    EDITOR_SET_ACTIVE:
      Result := SSetActive;
  else
    Result := Format(SItems + ' %d', [Index]);
  end;
end;

{**********************************************}
function TALPageViewEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{*****************************************************************************}
procedure TALPageViewEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  inherited PrepareItem(Index, AItem);
  case Index of
    EDITOR_SET_ACTIVE:
      begin
        AItem.Visible := true;
        if (Component is TALPageView) and
           ((TALPageView(Component).Active) or
            (not (TALPageView(Component).Visible))) then Aitem.Enabled := False
        else aItem.Enabled := True;
      end;
  end;
end;

{***************************************}
procedure TALTextTextPropertyEditor.Edit;
begin
  inherited;
  FreeAndNil(FStrings);
end;

{********************************************************************}
function TALTextTextPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paMultiSelect, paAutoUpdate];
end;

{******************************************************}
function TALTextTextPropertyEditor.GetStrings: TStrings;
begin
  if FStrings = nil then
  begin
    FStrings := TStringList.Create;
    {$IF CompilerVersion >= 31}
    FStrings.Options := FStrings.Options - [TStringsOption.soTrailingLineBreak];
    {$ENDIF}
  end;
  FStrings.Text := GetStrValue;
  Result := FStrings;
end;

{**************************************************}
function TALTextTextPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{*********************************************************************}
procedure TALTextTextPropertyEditor.SetStrings(const AValue: TStrings);
begin
  if AValue.Text.EndsWith(AValue.LineBreak) then
    SetStrValue(AValue.Text.Substring(0, Length(AValue.Text) - Length(AValue.LineBreak)))
  else
    SetStrValue(AValue.Text);
end;

{*****************************************************************}
procedure TALTextTextPropertyEditor.SetValue(const AValue: string);
begin
  SetStrValue(AValue);
end;

{*******************************************************}
function TALGradientPropertyEditor.GetIsDefault: Boolean;
begin
  result := GetValue = '';
end;

{**************************************************}
function TALGradientPropertyEditor.GetValue: string;
begin
  Result := '';
  if PropCount > 0 then begin
    var LObj := TObject(GetOrdValue);
    if Assigned(LObj) and (LObj is TALGradient) then
      result := TALGradient(LObj).cssFormat;
  end;
end;

{*****************************************************************}
procedure TALGradientPropertyEditor.SetValue(const AValue: string);
begin
  if PropCount > 0 then begin
    var LObj := TObject(GetOrdValue);
    if Assigned(LObj) and (LObj is TALGradient) then
      TALGradient(LObj).cssFormat := AValue;
  end;
end;


{*****************}
procedure Register;
begin
  RegisterComponentEditor(TALEdit, TALEditEditor);
  RegisterComponentEditor(TALMemo, TALMemoEditor);
  RegisterComponentEditor(TALButton, TALButtonEditor);
  RegisterComponentEditor(TALCheckBox, TALCheckBoxEditor);
  RegisterComponentEditor(TALRadioButton, TALRadioButtonEditor);
  RegisterComponentEditor(TALSwitch, TALSwitchEditor);
  RegisterComponentEditor(TALTrackBar, TALTrackBarEditor);
  RegisterComponentEditor(TALRangeTrackBar, TALRangeTrackBarEditor);
  RegisterComponentEditor(TALPageController, TALPageControllerEditor);
  RegisterComponentEditor(TALPageView, TALPageViewEditor);
  RegisterPropertyEditor(TypeInfo(string), TALText, 'Text', TALTextTextPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TALGradient), TALBrush, 'Gradient', TALGradientPropertyEditor);
end;

end.
