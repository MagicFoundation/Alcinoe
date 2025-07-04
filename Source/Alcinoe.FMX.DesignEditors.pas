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

  {******************************************}
  TALColorKeyProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  {***********************************}
  TALTextEditor = class(TDefaultEditor)
  protected
    procedure ApplyStyleClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {***********************************}
  TALEditEditor = class(TDefaultEditor)
  protected
    procedure ApplyStyleClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {***********************************}
  TALMemoEditor = class(TDefaultEditor)
  protected
    procedure ApplyStyleClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {*************************************}
  TALButtonEditor = class(TDefaultEditor)
  protected
    procedure ApplyStyleClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {*******************************************}
  TALToggleButtonEditor = class(TDefaultEditor)
  protected
    procedure ApplyStyleClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {***************************************}
  TALCheckBoxEditor = class(TDefaultEditor)
  protected
    procedure ApplyStyleClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {******************************************}
  TALRadioButtonEditor = class(TDefaultEditor)
  protected
    procedure ApplyStyleClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {*************************************}
  TALSwitchEditor = class(TDefaultEditor)
  protected
    procedure ApplyStyleClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {***************************************}
  TALTrackBarEditor = class(TDefaultEditor)
  protected
    procedure ApplyStyleClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {********************************************}
  TALRangeTrackBarEditor = class(TDefaultEditor)
  protected
    procedure ApplyStyleClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {****************************************}
  TALScrollBarEditor = class(TDefaultEditor)
  protected
    procedure ApplyStyleClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {****************************************}
  TALScrollBoxEditor = class(TDefaultEditor)
  protected
    procedure ApplyStyleClick(Sender: TObject); virtual;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {*******************************************}
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

  {*****************************************}
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
  System.Generics.Collections,
  System.SysUtils,
  Vcl.Menus,
  FMX.Design.Items,
  Alcinoe.Common,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.Styles,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.common,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.VideoPlayer,
  Alcinoe.FMX.PageController;

{*****************************************************************************************************************************************}
procedure ALBuildItemMenuHierarchy(const AParentItem: IMenuItem; const ANames: TList<TPair<String, String>>; const AOnClick: TNotifyEvent);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _getItem(const AParentItem: IMenuItem; const ACaption: String): IMenuItem;
  begin
    Result := nil;
    for var i := 0 to AParentItem.Count - 1 do begin
      if ALSameTextW(AParentItem.Items[i].Caption, ACaption) then begin
        Result := AParentItem.Items[i];
        Exit;
      end;
    end;
    Result := AParentItem.AddItem(ACaption{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, nil{AOnClick}, 0{hCtx}, ''{AName});
  end;

begin
  for var I := 0 to ANames.Count - 1 do begin
    var LLst := TALStringListW.Create;
    try
      LLst.LineBreak := '.';
      LLst.Text := ANames[i].Key; // Material3.Original.Filled
      var LItem := AParentItem;
      for var J := 0 to lLst.Count - 2 do
        LItem := _getItem(LItem, lLst[J]);
      LItem := LItem.AddItem(lLst[lLst.Count - 1]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, AOnClick{AOnClick}, 0{hCtx}, ''{AName});
      LItem.Hint := ANames[i].Value; // // Material3.Button.Filled
    finally
      ALFreeAndNil(LLst);
    end;
  end;
end;

{**************************************************************}
function TALColorKeyProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

{*********************************************************}
procedure TALColorKeyProperty.GetValues(Proc: TGetStrProc);
begin
  var LNames := TALStyleManager.Instance.GetColorNames;
  for var I := low(LNames) to high(LNames) do
    Proc(LNames[i]);
end;

{*****************************************************}
function TALTextEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Style';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{*******************************************}
function TALTextEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{*******************************************************}
procedure TALTextEditor.ApplyStyleClick(Sender: TObject);
begin
  var LStyleName := TmenuItem(Sender).Caption;
  LStyleName := StringReplace(LStyleName, '&','',[rfReplaceALL]);
  TALStyleManager.Instance.ApplyTextStyle(LStyleName, TALText(Component));
end;

{**************************************************************************}
procedure TALTextEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LNames := TALStyleManager.Instance.GetTextStyleNames;
  for var I := low(LNames) to high(LNames) do
    AItem.AddItem(LNames[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyStyleClick{AOnClick}, 0{hCtx}, ''{AName});
end;

{*****************************************************}
function TALEditEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Style';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{*******************************************}
function TALEditEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{*******************************************************}
procedure TALEditEditor.ApplyStyleClick(Sender: TObject);
begin
  var LStyleName := TmenuItem(Sender).Caption;
  LStyleName := StringReplace(LStyleName, '&','',[rfReplaceALL]);
  TALStyleManager.Instance.ApplyEditStyle(LStyleName, TALEdit(Component));
end;

{**************************************************************************}
procedure TALEditEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LNames := TALStyleManager.Instance.GetEditStyleNames;
  for var I := low(LNames) to high(LNames) do
    AItem.AddItem(LNames[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyStyleClick{AOnClick}, 0{hCtx}, ''{AName});
end;

{*****************************************************}
function TALMemoEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Style';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{*******************************************}
function TALMemoEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{*******************************************************}
procedure TALMemoEditor.ApplyStyleClick(Sender: TObject);
begin
  var LStyleName := TmenuItem(Sender).Caption;
  LStyleName := StringReplace(LStyleName, '&','',[rfReplaceALL]);
  TALStyleManager.Instance.ApplyMemoStyle(LStyleName, TALMemo(Component));
end;

{**************************************************************************}
procedure TALMemoEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LNames := TALStyleManager.Instance.GetMemoStyleNames;
  for var I := low(LNames) to high(LNames) do
    AItem.AddItem(LNames[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyStyleClick{AOnClick}, 0{hCtx}, ''{AName});
end;

{*******************************************************}
function TALButtonEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Style';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{*********************************************}
function TALButtonEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{*********************************************************}
procedure TALButtonEditor.ApplyStyleClick(Sender: TObject);
begin
  var LStyleName := TmenuItem(Sender).Hint;
  if LStyleName = '' then exit;
  TALStyleManager.Instance.ApplyButtonStyle(LStyleName, TALButton(Component));
end;

{****************************************************************************}
procedure TALButtonEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LLstNames := TList<TPair<String, String>>.Create;
  try
    var LArrNames := TALStyleManager.Instance.GetButtonStyleNames;
    for var I := low(LArrNames) to high(LArrNames) do begin
      var LKey := ALStringReplaceW(LArrNames[i], 'Material3.Button.Icon',            'Material3.Original.Icon',    [RFIgnoreCase]);
      LKey := ALStringReplaceW(LKey,             'Material3.Button.FAB',             'Material3.Original.FAB',     [RFIgnoreCase]);
      LKey := ALStringReplaceW(LKey,             'Material3.Button',                 'Material3.Original.Button',  [RFIgnoreCase]);
      LKey := ALStringReplaceW(LKey,             'Material3.Expressive.Button.Icon', 'Material3.Expressive.Icon',  [RFIgnoreCase]);
      LKey := ALStringReplaceW(LKey,             'Material3.Expressive.Button.FAB',  'Material3.Expressive.FAB',   [RFIgnoreCase]);
      LKey := ALStringReplaceW(LKey,             'Material3.',                       'Material3 ', [RFIgnoreCase]);
      LLstNames.Add(TPair<String, String>.Create(LKey, LArrNames[i]));
    end;
    ALBuildItemMenuHierarchy(AItem{AParentItem}, LLstNames{ANames}, ApplyStyleClick{AOnClick});
  finally
    ALFreeAndNil(LLstNames);
  end;
end;

{*************************************************************}
function TALToggleButtonEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Style';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{***************************************************}
function TALToggleButtonEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{***************************************************************}
procedure TALToggleButtonEditor.ApplyStyleClick(Sender: TObject);
begin
  var LStyleName := TmenuItem(Sender).Hint;
  if LStyleName = '' then exit;
  TALStyleManager.Instance.ApplyToggleButtonStyle(LStyleName, TALToggleButton(Component));
end;

{**********************************************************************************}
procedure TALToggleButtonEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LLstNames := TList<TPair<String, String>>.Create;
  try
    var LArrNames := TALStyleManager.Instance.GetToggleButtonStyleNames;
    for var I := low(LArrNames) to high(LArrNames) do begin
      var LKey := ALStringReplaceW(LArrNames[i], 'Material3.ToggleButton.Icon',            'Material3.Original.Icon',    [RFIgnoreCase]);
      LKey := ALStringReplaceW(LKey,             'Material3.ToggleButton',                 'Material3.Original.ToggleButton',  [RFIgnoreCase]);
      LKey := ALStringReplaceW(LKey,             'Material3.Expressive.ToggleButton.Icon', 'Material3.Expressive.Icon',  [RFIgnoreCase]);
      LKey := ALStringReplaceW(LKey,             'Material3.Expressive.ToggleButton.FAB',  'Material3.Expressive.FAB',   [RFIgnoreCase]);
      LKey := ALStringReplaceW(LKey,             'Material3.',                             'Material3 ', [RFIgnoreCase]);
      LLstNames.Add(TPair<String, String>.Create(LKey, LArrNames[i]));
    end;
    ALBuildItemMenuHierarchy(AItem{AParentItem}, LLstNames{ANames}, ApplyStyleClick{AOnClick});
  finally
    ALFreeAndNil(LLstNames);
  end;
end;

{*********************************************************}
function TALCheckBoxEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Style';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{***********************************************}
function TALCheckBoxEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{***********************************************************}
procedure TALCheckBoxEditor.ApplyStyleClick(Sender: TObject);
begin
  var LStyleName := TmenuItem(Sender).Caption;
  LStyleName := StringReplace(LStyleName, '&','',[rfReplaceALL]);
  TALStyleManager.Instance.ApplyCheckBoxStyle(LStyleName, TALCheckBox(Component));
end;

{******************************************************************************}
procedure TALCheckBoxEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LNames := TALStyleManager.Instance.GetCheckBoxStyleNames;
  for var I := low(LNames) to high(LNames) do
    AItem.AddItem(LNames[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyStyleClick{AOnClick}, 0{hCtx}, ''{AName});
end;

{************************************************************}
function TALRadioButtonEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Style';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{**************************************************}
function TALRadioButtonEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{**************************************************************}
procedure TALRadioButtonEditor.ApplyStyleClick(Sender: TObject);
begin
  var LStyleName := TmenuItem(Sender).Caption;
  LStyleName := StringReplace(LStyleName, '&','',[rfReplaceALL]);
  TALStyleManager.Instance.ApplyRadioButtonStyle(LStyleName, TALRadioButton(Component));
end;

{*********************************************************************************}
procedure TALRadioButtonEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LNames := TALStyleManager.Instance.GetRadioButtonStyleNames;
  for var I := low(LNames) to high(LNames) do
    AItem.AddItem(LNames[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyStyleClick{AOnClick}, 0{hCtx}, ''{AName});
end;

{*******************************************************}
function TALSwitchEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Style';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{*********************************************}
function TALSwitchEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{*********************************************************}
procedure TALSwitchEditor.ApplyStyleClick(Sender: TObject);
begin
  var LStyleName := TmenuItem(Sender).Caption;
  LStyleName := StringReplace(LStyleName, '&','',[rfReplaceALL]);
  TALStyleManager.Instance.ApplySwitchStyle(LStyleName, TALSwitch(Component));
end;

{****************************************************************************}
procedure TALSwitchEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LNames := TALStyleManager.Instance.GetSwitchStyleNames;
  for var I := low(LNames) to high(LNames) do
    AItem.AddItem(LNames[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyStyleClick{AOnClick}, 0{hCtx}, ''{AName});
end;

{*********************************************************}
function TALTrackBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Style';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{***********************************************}
function TALTrackBarEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{***********************************************************}
procedure TALTrackBarEditor.ApplyStyleClick(Sender: TObject);
begin
  var LStyleName := TmenuItem(Sender).Caption;
  LStyleName := StringReplace(LStyleName, '&','',[rfReplaceALL]);
  TALStyleManager.Instance.ApplyTrackBarStyle(LStyleName, TALTrackBar(Component));
end;

{******************************************************************************}
procedure TALTrackBarEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LNames := TALStyleManager.Instance.GetTrackBarStyleNames;
  for var I := low(LNames) to high(LNames) do
    AItem.AddItem(LNames[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyStyleClick{AOnClick}, 0{hCtx}, ''{AName});
end;

{**************************************************************}
function TALRangeTrackBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Style';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{****************************************************}
function TALRangeTrackBarEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{****************************************************************}
procedure TALRangeTrackBarEditor.ApplyStyleClick(Sender: TObject);
begin
  var LStyleName := TmenuItem(Sender).Caption;
  LStyleName := StringReplace(LStyleName, '&','',[rfReplaceALL]);
  TALStyleManager.Instance.ApplyRangeTrackBarStyle(LStyleName, TALRangeTrackBar(Component));
end;

{***********************************************************************************}
procedure TALRangeTrackBarEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LNames := TALStyleManager.Instance.GetRangeTrackBarStyleNames;
  for var I := low(LNames) to high(LNames) do
    AItem.AddItem(LNames[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyStyleClick{AOnClick}, 0{hCtx}, ''{AName});
end;

{**********************************************************}
function TALScrollBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Style';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{************************************************}
function TALScrollBarEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{************************************************************}
procedure TALScrollBarEditor.ApplyStyleClick(Sender: TObject);
begin
  var LStyleName := TmenuItem(Sender).Caption;
  LStyleName := StringReplace(LStyleName, '&','',[rfReplaceALL]);
  TALStyleManager.Instance.ApplyScrollBarStyle(LStyleName, TALScrollBar(Component));
end;

{*******************************************************************************}
procedure TALScrollBarEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LNames := TALStyleManager.Instance.GetScrollBarStyleNames;
  for var I := low(LNames) to high(LNames) do
    AItem.AddItem(LNames[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyStyleClick{AOnClick}, 0{hCtx}, ''{AName});
end;

{**********************************************************}
function TALScrollBoxEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Style';
    else Result := Format(SItems + ' %d', [Index]);
  end;
end;

{************************************************}
function TALScrollBoxEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{************************************************************}
procedure TALScrollBoxEditor.ApplyStyleClick(Sender: TObject);
begin
  var LStyleName := TmenuItem(Sender).Caption;
  LStyleName := StringReplace(LStyleName, '&','',[rfReplaceALL]);
  TALStyleManager.Instance.ApplyScrollBoxStyle(LStyleName, TALScrollBox(Component));
end;

{*******************************************************************************}
procedure TALScrollBoxEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  var LNames := TALStyleManager.Instance.GetScrollBoxStyleNames;
  for var I := low(LNames) to high(LNames) do
    AItem.AddItem(LNames[i]{ACaption}, 0{AShortCut}, false{AChecked}, true{AEnabled}, ApplyStyleClick{AOnClick}, 0{hCtx}, ''{AName});
end;

{***************************************************************************************}
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

{**************************************************************}
procedure TALPageControllerEditor.DoCreateItem(Sender: TObject);
begin
  inherited;
  if (Component is TALPageController) then
    TALPageController(Component).ActivePageIndex := TALPageController(Component).PageCount - 1;
end;

{***********************************************************}
function TALPageControllerEditor.GetActivePageIndex: Integer;
begin
  if (Component is TALPageController) then
    Result := TALPageController(Component).ActivePageIndex
  else
    Result := -1;
end;

{************************************************************}
procedure TALPageControllerEditor.ExecuteVerb(Index: Integer);
var
  LControl: TALPageController;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
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

{***************************************************************}
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

{*****************************************************}
function TALPageControllerEditor.GetVerbCount: Integer;
begin
  Result := FVerbCount;
end;

{************************************************************************************}
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

{******************************************************}
procedure TALPageViewEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    EDITOR_SET_ACTIVE:
      if (Component is TALPageView) then
        TALPageView(Component).Active := true;
  end;
end;

{*********************************************************}
function TALPageViewEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    EDITOR_SET_ACTIVE:
      Result := SSetActive;
  else
    Result := Format(SItems + ' %d', [Index]);
  end;
end;

{***********************************************}
function TALPageViewEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{******************************************************************************}
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
  RegisterComponentEditor(TALText, TALTextEditor);
  RegisterComponentEditor(TALEdit, TALEditEditor);
  RegisterComponentEditor(TALMemo, TALMemoEditor);
  RegisterComponentEditor(TALButton, TALButtonEditor);
  RegisterComponentEditor(TALToggleButton, TALToggleButtonEditor);
  RegisterComponentEditor(TALCheckBox, TALCheckBoxEditor);
  RegisterComponentEditor(TALRadioButton, TALRadioButtonEditor);
  RegisterComponentEditor(TALSwitch, TALSwitchEditor);
  RegisterComponentEditor(TALTrackBar, TALTrackBarEditor);
  RegisterComponentEditor(TALRangeTrackBar, TALRangeTrackBarEditor);
  RegisterComponentEditor(TALScrollBar, TALScrollBarEditor);
  RegisterComponentEditor(TALScrollBox, TALScrollBoxEditor);
  RegisterComponentEditor(TALVertScrollBox, TALScrollBoxEditor);
  RegisterComponentEditor(TALHorzScrollBox, TALScrollBoxEditor);
  RegisterComponentEditor(TALPageController, TALPageControllerEditor);
  RegisterComponentEditor(TALPageView, TALPageViewEditor);
  RegisterPropertyEditor(TypeInfo(string), TALText, 'Text', TALTextTextPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TALGradient), TALBrush, 'Gradient', TALGradientPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TALShadow, 'ColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALFont, 'ColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALTextDecoration, 'ColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALBrush, 'ColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALBrush, 'ImageTintColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALStrokeBrush, 'ColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALStateLayer, 'ColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALBaseEdit.TBaseStateStyle, 'PromptTextColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALBaseEdit.TBaseStateStyle, 'TintColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALBaseEdit, 'PromptTextColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALBaseEdit, 'TintColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALBaseCheckBox.TCheckMarkBrush, 'ColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALCustomTrack.TTrack.TStopIndicatorBrush, 'ColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALImage, 'BackgroundColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALImage, 'LoadingColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALImage, 'TintColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALVideoPlayerSurface, 'BackgroundColorKey', TALColorKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TALVideoPlayerSurface, 'LoadingColorKey', TALColorKeyProperty);
end;

end.
