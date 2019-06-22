unit ALFmxDesignEditors;

{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if FMX.Editor.Items.pas was not updated and adjust the IFDEF'}
{$ENDIF}

interface

uses System.Classes,
     System.Generics.Collections,
     FMX.Types,
     DesignIntf,
     DesignEditors,
     DesignMenus;

resourcestring
  SNewItem = 'Add Item';
  SNewLastItem = 'Add %s';
  SItems = 'Items';
  SNextTab = 'Next Tab';
  SPrevTab = 'Previous Tab';
  SUnnamedTab = 'Unnamed %1:s %0:d';
  SDeleteItem = 'Delete %s';
  SSetActive = 'Set Active';

const
  EDITOR_CREATE_ITEM = 0;
  EDITOR_NEW_ITEM = 1;

const
  EDITOR_SET_ACTIVE = 0;

type

  {***********************}
  TALItemClassDesc = record
    ItemClass: TFmxObjectClass;
    CanContainSimilarItem: Boolean; // Can accept ItemClass Items
    ShowOnlyInMenu: Boolean;
    constructor Create(const AItemClass: TFmxObjectClass;
                       const ACanContaineSimilarItem: Boolean = False;
                       const AShowOnlyInMenu: Boolean = False);
  end;

  {**************************************}
  TALItemsEditor = class(TComponentEditor)
  private
    class var FListOfLastItems: TDictionary<string, Integer>;
  protected
    FAllowChild: Boolean;
    FItemsClasses: array of TALItemClassDesc;
    procedure DoCreateItem(Sender: TObject); virtual;
    function GetIndexOfItemClass: Integer;
    procedure SetIndexOfItemClass(const Value: Integer);
  protected
    function CanShow: Boolean; virtual;
    property IndexOfItemClass: Integer read GetIndexOfItemClass write SetIndexOfItemClass;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  {*****************************************}
  TALTabControlEditor = class(TALItemsEditor)
  private
    FEditorNextTab: Integer;
    FEditorPrevTab: Integer;
    FEditorDeleteTab: Integer;
    FVerbCount: Integer;
    function GetTabIndex: Integer;
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
  TALTabItemEditor = class(TComponentEditor)
  private
  protected
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

procedure Register;

implementation

uses System.SysUtils,
     Vcl.Menus,
     fmx.controls,
     ALFmxTabControl;

{********************************************************************}
constructor TALItemClassDesc.Create(const AItemClass: TFmxObjectClass;
                                    const ACanContaineSimilarItem: Boolean;
                                    const AShowOnlyInMenu: Boolean);
begin
  Self.ItemClass := AItemClass;
  Self.CanContainSimilarItem := ACanContaineSimilarItem;
  Self.ShowOnlyInMenu := AShowOnlyInMenu;
end;

{***************************************}
function TALItemsEditor.CanShow: Boolean;
begin
  Result := True;
end;

{*****************************************************}
procedure TALItemsEditor.DoCreateItem(Sender: TObject);
var
  MenuItem: Vcl.Menus.TMenuItem;
begin
  if Sender is Vcl.Menus.TMenuItem then
  begin
    MenuItem := Sender as Vcl.Menus.TMenuItem;
    if MenuItem.Tag >= 0 then
      IndexOfItemClass := MenuItem.Tag;
  end;
  if (Component is TFmxObject) and (IndexOfItemClass >= 0) and (IndexOfItemClass < Length(FItemsClasses)) then
    Designer.CreateChild(FItemsClasses[IndexOfItemClass].ItemClass, Component);
end;

{***************************************************}
procedure TALItemsEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    EDITOR_NEW_ITEM:
      DoCreateItem(nil);
  end;
end;

{******************************************************}
function TALItemsEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    EDITOR_CREATE_ITEM:
      Result := SNewItem;
    EDITOR_NEW_ITEM:
      begin
        if (IndexOfItemClass >= 0) and (IndexOfItemClass < Length(FItemsClasses)) then
          Result := Format(SNewLastItem, [FItemsClasses[IndexOfItemClass].ItemClass.ClassName])
        else
          Result := Format(SNewLastItem, [IntToStr(IndexOfItemClass)]);
      end;
  else
    Result := Format(SItems + ' %d', [Index]);
  end;
end;

{********************************************}
function TALItemsEditor.GetVerbCount: Integer;
begin
  if CanShow then
    Result := 2
  else
    Result := 0;
end;

{***************************************************}
function TALItemsEditor.GetIndexOfItemClass: Integer;
var
  I: Integer;
begin
  Result := 0;
  if (FListOfLastItems <> nil) and FListOfLastItems.TryGetValue(ClassType.QualifiedClassName, I) then
    Result := I;
end;

{*****************************************************************}
procedure TALItemsEditor.SetIndexOfItemClass(const Value: Integer);
var
  I: Integer;
begin
  if FListOfLastItems = nil then
  begin
    FListOfLastItems := TDictionary<string, Integer>.Create;
  end;
  if FListOfLastItems.TryGetValue(ClassType.QualifiedClassName, I) then
    FListOfLastItems[ClassType.QualifiedClassName] := Value
  else
    FListOfLastItems.Add(ClassType.QualifiedClassName, Value);
end;

{***************************************************************************}
procedure TALItemsEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
var
  I: Integer;
  MenuItem: IMenuItem;
begin
  inherited PrepareItem(Index, AItem);
  case Index of
    EDITOR_CREATE_ITEM:
      begin
        if Length(FItemsClasses) > 1 then
        begin
          AItem.Visible := True;
          for I := 0 to High(FItemsClasses) do
          begin
            MenuItem := AItem.AddItem(FItemsClasses[I].ItemClass.ClassName, 0, False, True, DoCreateItem);
            MenuItem.Tag := I;
            MenuItem := nil;
          end;
        end
        else
          AItem.Visible := False;
      end;
    EDITOR_NEW_ITEM:
      begin
        AItem.Visible := (IndexOfItemClass >= 0) and (IndexOfItemClass < Length(FItemsClasses));
        AItem.Tag := -1;
      end;
  end;
end;

{***********************************************************************************}
constructor TALTabControlEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FEditorNextTab := inherited GetVerbCount;
  FEditorPrevTab := FEditorNextTab + 1;
  FEditorDeleteTab := FEditorNextTab + 2;
  FVerbCount := FEditorNextTab + 3;
  FAllowChild := False;
  SetLength(FItemsClasses, 1);
  FItemsClasses[0] := TALItemClassDesc.Create(TALTabItem);
end;

{**********************************************************}
procedure TALTabControlEditor.DoCreateItem(Sender: TObject);
begin
  inherited;
  if (Component is TALTabControl) then
    TALTabControl(Component).TabIndex := TALTabControl(Component).TabCount - 1;
end;

{************************************************}
function TALTabControlEditor.GetTabIndex: Integer;
begin
  if (Component is TALTabControl) and (TALTabControl(Component).TabIndex >= 0) and
    (TALTabControl(Component).TabIndex < TALTabControl(Component).TabCount) then
    Result := TALTabControl(Component).TabIndex
  else
    Result := -1;
end;

{********************************************************}
procedure TALTabControlEditor.ExecuteVerb(Index: Integer);
var
  Obj: TFmxObject;
  LTabIndex: Integer;
  LControl: TALTabControl;
  procedure SelectTab(I: Integer);
  begin
    if I >= LControl.TabCount then
      I := LControl.TabCount - 1;
    if I < 0 then
      I := -1;
    LControl.TabIndex := I;
    if I >= 0 then
      Designer.SelectComponent(LControl.Tabs[I])
    else
      Designer.SelectComponent(Component);
  end;
begin
  inherited;
  if Component is TALTabControl then
  begin
    LTabIndex := GetTabIndex;
    LControl := TALTabControl(Component);
    if Index = FEditorNextTab then
      SelectTab(LTabIndex + 1)
    else if Index = FEditorPrevTab then
      SelectTab(LTabIndex - 1)
    else if (Index = FEditorDeleteTab) and (LTabIndex >= 0) then
    begin
      Obj := (LControl as IItemsContainer).GetItem(LTabIndex);
      FreeAndNil(Obj);
      SelectTab(LTabIndex);
    end;
  end;
end;

{***********************************************************}
function TALTabControlEditor.GetVerb(Index: Integer): string;
var
  S: string;
begin
  Result := Inherited GetVerb(Index);
  if Component is TALTabControl then
    if Index = FEditorNextTab then
      Result := SNextTab
    else if Index = FEditorPrevTab then
      Result := SPrevTab
    else if Index = FEditorDeleteTab then
    begin
      if GetTabIndex >= 0 then
      begin
        S := TALTabControl(Component).Tabs[GetTabIndex].Name;
        if S = '' then
          S := Format(SUnnamedTab, [GetTabIndex, TALTabControl(Component).Tabs[GetTabIndex].ClassName])
        else
          S := QuotedStr(S);
        Result := Format(SDeleteItem, [S]);
      end
      else
        Result := Format(SDeleteItem, ['']);
    end;
end;

{*************************************************}
function TALTabControlEditor.GetVerbCount: Integer;
begin
  Result := FVerbCount;
end;

{********************************************************************************}
procedure TALTabControlEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  inherited;
  if Component is TALTabControl then
  begin
    if Index = FEditorNextTab then
      AItem.Enabled := GetTabIndex < TALTabControl(Component).TabCount - 1
    else if Index = FEditorPrevTab then
      AItem.Enabled := GetTabIndex > 0
    else if Index = FEditorDeleteTab then
      AItem.Enabled := GetTabIndex >= 0;
  end
  else
    AItem.Visible := False;
end;

{*****************************************************}
procedure TALTabItemEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    EDITOR_SET_ACTIVE:
      if (Component is TALTabItem) then
        TALTabItem(Component).IsSelected := true;
  end;
end;

{********************************************************}
function TALTabItemEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    EDITOR_SET_ACTIVE:
      Result := SSetActive;
  else
    Result := Format(SItems + ' %d', [Index]);
  end;
end;

{**********************************************}
function TALTabItemEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{*****************************************************************************}
procedure TALTabItemEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  inherited PrepareItem(Index, AItem);
  case Index of
    EDITOR_SET_ACTIVE:
      begin
        AItem.Visible := true;
        if (Component is TALTabItem) and
           ((TALTabItem(Component).IsSelected) or
            (not (TALTabItem(Component).Visible))) then Aitem.Enabled := False
        else aItem.Enabled := True;
      end;
  end;
end;

procedure Register;
begin
  RegisterComponentEditor(TALTabControl, TALTabControlEditor);
  RegisterComponentEditor(TALTabItem, TALTabItemEditor);
end;

initialization
  TALItemsEditor.FListOfLastItems := nil;

finalization
  if TALItemsEditor.FListOfLastItems <> nil then
    FreeandNil(TALItemsEditor.FListOfLastItems);

end.
