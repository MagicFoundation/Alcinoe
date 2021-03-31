/// JSON tree view bases on Synopse JSON parser
// - based on JSON TreeView VCL Component from pawel.glowacki@embarcadero.com
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynJSONTreeView;

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

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Esteban Martin (EMartin)

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

  Version 1.18
  - first public release, corresponding to Synopse mORMot Framework 1.18

TODO:
- FPC support (I just use Windows)
- Developed with Delphi 7, not tested with other Delphi versions
}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SysUtils,
  Classes,
  ComCtrls,
  Controls,
  SynCommons,
  SynTable;

type
  ESynJSONTreeView = class(ESynException);
  TSynJSONTreeNode = class;

  /// Tree node types
  // - jtnObject: JSON object, i.e: {"fieldNumber":123,"fieldText":"text","fieldNull":null}
  // - jtnObjectContent: conten of jtnObject, i.e: "fieldNumber":123
  // - jtnArray: JSON array, i.e: [123,"text",{"fieldNumber":123,"fieldText":"text","fieldNull":null},null]
  // - jtnArrayContent: content of jtnArray, i.e: 123 or {"fieldNumber":123,"fieldText":"text","fieldNull":null}
  TSynJSONTreeNodeType = (jtnObject, jtnObjectContent, jtnArray, jtnArrayContent);

  /// Event type for fire event when editing
  TSynJSONTreeViewCustomInputEvent = procedure(Sender: TObject; Node: TSynJSONTreeNode;
                                               var Prompt: RawUTF8; var Value: Variant;
                                               var Handled: Boolean) of object;
  /// Node extension for handling JSON value in the treeview
  TSynJSONTreeNode = class(TTreeNode)
  protected
    fName: RawUTF8;
    fJSONType: TSynJSONTreeNodeType;
    fValue: Variant;
  public
    /// Clone the node
    function Clone: TSynJSONTreeNode;
    /// JSON type in the node
    property JSONType: TSynJSONTreeNodeType read fJSONType write fJSONType;
    /// Name of the JSON value
    property Name: RawUTF8 read FName write fName;
    /// JSON value
    property Value: Variant read fValue write fValue;
  end;

  /// Allowed edition states of treeview
  // - estEdit: allow edit a node
  // - estDelete: allow delete a node
  // - estInsert: allow insert a node
  TSynJSONTreeViewEditionStateType = (estEdit , estDelete, estInsert);
  TSynJSONTreeViewEditionStateSet = set of TSynJSONTreeViewEditionStateType;

  /// TSynJSONTreeView is a treeview supporting mORMOn JSON document (TDocVariantData), the allowed tasks are:
  // - edit a node
  // - delete a node
  // - add a node
  // - save to file
  // - load from file
  // - generate JSON to string variable
  // - implement a custom input for edit/insert node
  TSynJSONTreeView = class(TTreeView)
  private
    fAllowedEditionStates: TSynJSONTreeViewEditionStateSet;
  protected
    fFirsttime: Boolean;
    fJSONDocument: Variant;
    fJSONText: RawUTF8;
    fKey: Integer;
    fOnDblClickBak: TNotifyEvent;
    fOnEditingBak: TTVEditingEvent;
    fOnKeyDownBak: TKeyEvent;
    fOnCustomInput: TSynJSONTreeViewCustomInputEvent;
    fVisibleChildrenCounts: boolean;
    fVisibleByteSizes: boolean;
    /// Add child node from JSON value
    // - aNode: node from which add the child
    // - aJSON: JSON value single or object/array
    // - aNameOrIndex: if it is numeric, then is the index into the object/array, otherwise the name of the container node
    function AddChildFromJSON(const aNode: TSynJSONTreeNode; const aJSON: Variant; const aNameOrIndex: Variant): TSynJSONTreeNode;
    procedure BackupEvents;
    procedure DeleteNode(Node: TSynJSONTreeNode; const aAsk: Boolean = True);
    procedure DoOnCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure DoOnDblClick(Sender: TObject);
    procedure DoOnEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditNode(Node: TSynJSONTreeNode);
    procedure InsertNode(Node: TSynJSONTreeNode; const aJSON: Variant);
    function IsActive: Boolean;
    function IsSimpleJsonValue(v: Variant): boolean;{$ifdef HASINLINE}inline;{$endif}
    procedure ProcessArray(const aCurrNode: TSynJSONTreeNode; const aValue: Variant; const aNameOrIndex: Variant);
    procedure ProcessElement(currNode: TSynJSONTreeNode; arr: Variant; aIndex: integer);
    function ProcessJSONText: boolean;
    procedure ProcessObject(const aCurrNode: TSynJSONTreeNode; const aValue: Variant; const aNameOrIndex: Variant);
    function ProcessPair(currNode: TSynJSONTreeNode; obj: Variant; aIndex: integer): TSynJSONTreeNode;
    /// Assign and process the JSON text
    // - if Value is null then clear the JSON text and the treeview content
    procedure SetJSONText(const Value: RawUTF8);
    /// Assign mORMot JSON Document
    // - if Value is not equal to the previous value then load the JSON into the treeview
    procedure SetSynJSONDocument(const Value: Variant);
    /// Show how many children has a container node
    procedure SetVisibleChildrenCounts(const Value: boolean);
    /// Show byte size of a container node
    procedure SetVisibleByteSizes(const Value: boolean);
    /// Update info of a container node
    procedure UpdateNodeInfo(aNode: TSynJSONTreeNode);
    /// Get the original value from variant value
    function VariantToRaw(const aVarData: Variant): Variant;
    /// Assign source variant to dest variant but considering the dest data type
    procedure VariantToVariant(aSource: Variant; var aDest: Variant);
    /// Convert variant to UTF8 but checking the boolean type and convert it to true/false string
    function VarToUTF8(const aVarData: Variant): RawUTF8;
  public
    constructor Create(AOwner: TComponent); override;

    /// Remove items and JSON text
    procedure ClearAll;
    /// Load from file
    procedure LoadFromFile(const aFileName: TFileName);
    /// Load the JSONText in the treeview, clear if empty
    procedure LoadJSON;
    /// Save to file
    procedure SaveToFile(const aFileName: TFileName);
    /// Generate JSON from the treeview
    function ToJSON: RawUTF8;

    /// JSON DocVariantData
    property JSONDocument: Variant read FJSONDocument write SeTSynJSONDocument;
  published
    /// Allowed state editions
    property AllowedEditionStates: TSynJSONTreeViewEditionStateSet read fAllowedEditionStates write fAllowedEditionStates;
    /// Content the JSON text, when assigned parse the the JSON and load in the treeview
    property JSONText: RawUTF8 read fJSONText write SetJSONText;
    /// Display info over how many children has each node
    property VisibleChildrenCounts: boolean read FVisibleChildrenCounts write SetVisibleChildrenCounts;
    /// Display info over the size of node
    property VisibleByteSizes: boolean read FVisibleByteSizes write SetVisibleByteSizes;
    /// Event fired when editing for custom input
    property OnCustomInput: TSynJSONTreeViewCustomInputEvent read fOnCustomInput write fOnCustomInput;
  end;

implementation

uses
  Windows,
  Variants,
  mORMotUILogin;

resourcestring
  SInvalidJSONValue = 'Invalid JSON value';
  SInvalidJSON = 'Invalid JSON: %';
  SEnterValue = 'Enter Value';
  SInsertNewItem = 'Insert new item';
  SInsertNewItemPrompt = 'Enter JSON:\n\n object: <name>:<value>\n array: [<index>]:<value>\n\n ' +
                         'In array the brackets are required.';
  SRemoveChildren = 'This node has children, are you sure to remove them';
  SDeleteNode = 'Delete node\n\n% ?';

{ TSynJSONTreeView }

procedure TSynJSONTreeView.ClearAll;
begin
  SetJSONText('');
  Items.Clear;
end;

constructor TSynJSONTreeView.Create(AOwner: TComponent);
begin
  inherited;
  OnCreateNodeClass := DoOnCreateNodeClass;
  fAllowedEditionStates := [estEdit, estDelete, estInsert];
  fFirstTime := True;
  FVisibleChildrenCounts := true;
  FVisibleByteSizes := false;
end;

procedure TSynJSONTreeView.LoadJson;
var
  v: Variant;
begin
  BackupEvents;
  Items.Clear;

  if IsActive then
  begin
    v := fJSONDocument;
    Items.Clear;

    if (v._Kind = dvObject) then
      ProcessObject(nil, v, Unassigned)
    else if (v._Kind = dvArray) then
      ProcessArray(nil, v, Unassigned)
    else
      raise ESynJSONTreeView.Create(SInvalidJSONValue);

    FullExpand;
  end;
end;

function TSynJSONTreeView.ProcessPair(currNode: TSynJSONTreeNode; obj: Variant; aIndex: integer): TSynJSONTreeNode;
var
  p: Variant;
begin
  Result := nil;

  p := obj._(aIndex);

  if IsSimpleJsonValue(p) then
  begin
    AddChildFromJSON(currNode, obj, aIndex);
    Exit;
  end;

  if (p._Kind = dvObject) then
    ProcessObject(currNode, _CopyFast(p), obj.Name(aIndex))
  else if (p._Kind = dvArray) then
    ProcessArray(currNode, _CopyFast(p), obj.Name(aIndex))
  else
    raise ESynJSONTreeView.Create(SInvalidJSONValue);
end;

procedure TSynJSONTreeView.ProcessElement(currNode: TSynJSONTreeNode; arr: Variant; aIndex: integer);
var
  v: Variant;
begin
  v := arr._(aIndex);

  if IsSimpleJsonValue(v) then
  begin
    AddChildFromJSON(currNode, arr, aIndex);
    Exit;
  end;

  if (v._Kind = dvObject) then
    ProcessObject(currNode, _CopyFast(v), IntToStr(aIndex))
  else if (v._Kind = dvArray) then
    ProcessArray(currNode, _CopyFast(v), aIndex)
  else
    raise ESynJSONTreeView.Create(SInvalidJSONValue);
end;

procedure TSynJSONTreeView.SetSynJSONDocument(const Value: Variant);
begin
  if fJSONDocument <> Value then
  begin
    fJSONDocument := Value;
    ClearAll;
    if IsActive then
      LoadJson;
  end;
end;

procedure TSynJSONTreeView.SetVisibleByteSizes(const Value: boolean);
begin
  if FVisibleByteSizes <> Value then
  begin
    FVisibleByteSizes := Value;
    LoadJson;
  end;
end;

procedure TSynJSONTreeView.SetVisibleChildrenCounts(const Value: boolean);
begin
  if FVisibleChildrenCounts <> Value then
  begin
    FVisibleChildrenCounts := Value;
    LoadJson;
  end;
end;

function TSynJSONTreeView.VarToUTF8(const aVarData: Variant): RawUTF8;
begin
  if (TVarData(aVarData).VType = varBoolean) then
  begin
    if TVarData(aVarData).VBoolean then
      Result := 'true'
    else
      Result := 'false';
  end else
    Result := VariantToUTF8(aVarData);
end;

function TSynJSONTreeView.ToJSON: RawUTF8;
var
  lW: TTextWriter;

  procedure ProcessNode(aNode: TSynJSONTreeNode);

    procedure CheckLastComma(aW: TTextWriter);
    begin
      if (aW.TextLength > 0) and (not (aW.LastChar in [',','[','{'])) then
        aW.Add(',');
    end;

  var
    lNode: TSynJSONTreeNode;
  begin
    lNode := aNode;
    repeat
      if Assigned(lNode) then begin
        case lNode.JSONType of
          jtnObject:
          begin
            CheckLastComma(lW);
            // numeric name is excluded
            if (lNode.Name <> '') and (StrToIntDef(lNode.Name, MaxInt) = MaxInt) then
              lW.AddFieldName(lNode.Name);
            lW.Add('{');
          end;
          jtnArray:
          begin
            CheckLastComma(lW);
            if (lNode.Name <> '') then
              lW.AddFieldName(lNode.Name);
            lW.Add('[');
          end;
          jtnObjectContent:
          begin
            CheckLastComma(lW);
            lW.AddFieldName(lNode.Name);
            lW.AddVariant(lNode.Value);
            lW.Add(',');
          end;
          jtnArrayContent:
          begin
            CheckLastComma(lW);
            lW.AddVariant(lNode.Value);
            lW.Add(',');
          end;
        end;
      end;
      if lNode.HasChildren then begin
        ProcessNode(TSynJSONTreeNode(lNode.GetFirstChild));
        lW.CancelLastComma;
        case lNode.JSONType of
          jtnObject: lW.Add('}');
          jtnArray: lW.Add(']');
        end;
      end;
      lNode := TSynJSONTreeNode(lNode.GetNextSibling);
    until not Assigned(lNode);
  end;

begin
  lW := TTextWriter.CreateOwnedStream;
  try
    ProcessNode(TSynJSONTreeNode(Items.GetFirstNode));
    lW.CancelLastComma;
    lW.SetText(Result);
  finally
    lW.Free;
  end;
end;

procedure TSynJSONTreeView.DoOnCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TSynJSONTreeNode;
end;

function TSynJSONTreeView.VariantToRaw(const aVarData: Variant): Variant;
var
  lVar: TSQLVar;
  lTmp: RawByteString;
begin
  if VarIs(aVarData, [varBoolean]) then
  begin
    Result := aVarData;
    Exit;
  end;
  VariantToSQLVar(aVarData, lTmp, lVar);
  with lVar do
    case VType of
      ftNull: Result := null;
      ftCurrency: Result := VInt64;
      ftUTF8:
        if Assigned(VText) then
          Result := RawUTF8(PUTF8Char(VText))
        else
          Result := '';
      ftInt64: Result := VInt64;
      ftDouble: Result := VDouble;
  end;
end;

procedure TSynJSONTreeView.DoOnDblClick(Sender: TObject);
begin
  if ReadOnly or (Selected.Count > 0) or (not (estEdit in fAllowedEditionStates)) then
    Exit;

  EditNode(TSynJSONTreeNode(Selected));
  if Assigned(fOnDblClickBak) then
    fOnDblClickBak(Sender);
end;

function TSynJSONTreeView.IsActive: Boolean;
begin
  Result := (fJSONDocument <> Unassigned) and (not VarIsEmptyOrNull(fJSONDocument)) and (fJSONDocument._Kind <> dvUndefined);
end;

function TSynJSONTreeView.IsSimpleJsonValue(v: Variant): boolean;
begin
  Result := VarIsEmptyOrNull(v) or VarIsNumeric(v) or VarIsStr(v) or VarIsStr(v);
end;

procedure TSynJSONTreeView.SetJSONText(const Value: RawUTF8);
begin
  if fJSONText <> Value then
  begin
    fJSONText := Value;
    if fJSONText <> '' then
      ProcessJSONText
    else
    begin
      TDocVariantData(fJSONDocument).Clear;
      fJSONDocument := Unassigned;
    end;
  end;
end;

function TSynJSONTreeView.ProcessJSONText: boolean;
begin
  if IsActive then
    fJSONDocument.Clear;
  TDocVariantData(fJSONDocument).InitJSON(fJSONText);
  if (TDocVariantData(fJSONDocument).Kind = dvUndefined) then
    raise ESynJSONTreeView.CreateUTF8(SInvalidJSON, [fJSONText]);
  Result := IsActive;
end;

procedure TSynJSONTreeView.BackupEvents;
begin
  if not fFirstTime then
    Exit;

  if not Assigned(fOnDblClickBak) and Assigned(OnDblClick) then
    fOnDblClickBak := OnDblClick;
  OnDblClick := DoOnDblClick;

  if not Assigned(fOnEditingBak) and Assigned(OnEditing) then
    fOnEditingBak := OnEditing;
  OnEditing := DoOnEditing;

  if not Assigned(fOnKeyDownBak) and Assigned(OnKeyDown) then
    fOnKeyDownBak := OnKeyDown;
  OnKeyDown := DoOnKeyDown;

  fFirstTime := False;
end;

procedure TSynJSONTreeView.DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ReadOnly or (not (Key in [VK_DELETE, VK_INSERT, VK_RETURN])) or
     ((Key = VK_DELETE) and (not (estDelete in fAllowedEditionStates))) or
     ((Key = VK_RETURN) and (not (estEdit in fAllowedEditionStates))) or
     ((Key = VK_INSERT) and (not (estInsert in fAllowedEditionStates))) then
    Exit;

  case Key of
    VK_DELETE: DeleteNode(TSynJSONTreeNode(Selected));
    VK_INSERT: InsertNode(TSynJSONTreeNode(Selected), Unassigned);
    VK_RETURN: EditNode(TSynJSONTreeNode(Selected));
  end;

  if Assigned(fOnKeyDownBak) then
    fOnKeyDownBak(Sender, Key, Shift);
end;

procedure TSynJSONTreeView.UpdateNodeInfo(aNode: TSynJSONTreeNode);

  procedure UpdateNodeText(aNode: TSynJSONTreeNode; const aText: RawUTF8);
  begin
    if not VarIsNull(aNode.Value) then
      aNode.Text := aText
    else
      aNode.Text := aNode.Name + ': null';
  end;

begin
  case aNode.JSONType of
    jtnObject: begin
      if (not Assigned(aNode.Parent)) or (TSynJSONTreeNode(aNode.Parent).JSONType <> jtnArray) then
        UpdateNodeText(aNode, aNode.Name + ' {}')
      else
        UpdateNodeText(aNode, FormatUTF8('[%] {}', [aNode.Name]));
    end;
    jtnArray:
      if not VarIsNull(aNode.Value) then
        UpdateNodeText(aNode, aNode.Name + ' []');
  end;

  case aNode.JSONType of
    jtnObject, jtnArray: begin
      if VisibleChildrenCounts then
        if not VarIsNull(aNode.Value) then
          UpdateNodeText(aNode, aNode.Text + ' (' + IntToStr(aNode.Value._Count) + ')')
        else
          UpdateNodeText(aNode, '');

      if VisibleByteSizes then
        if not VarIsNull(aNode.Value) then
          UpdateNodeText(aNode, aNode.Text + ' (size: ' + IntToStr(Length(aNode.Value._JSON)) + ' bytes)')
        else
          UpdateNodeText(aNode, '');
    end;
    jtnObjectContent, jtnArrayContent: begin
      UpdateNodeText(aNode, '');
    end;
  end;
end;

procedure TSynJSONTreeView.DeleteNode(Node: TSynJSONTreeNode; const aAsk: Boolean);

  procedure RecalculateChildNames(aNode: TSynJSONTreeNode);
  var
    lNode: TSynJSONTreeNode;
    I: Integer;
  begin
    lNode := TSynJSONTreeNode(aNode.GetFirstChild);
    if not Assigned(lNode) then
      Exit;
    I := 0;
    case lNode.JSONType of
      jtnObject: begin
        repeat
          if Assigned(lNode) then begin
            if (StrToIntDef(lNode.Name, MaxInt) <> MaxInt) then
              lNode.Name := IntToStr(I);
            UpdateNodeInfo(lNode);
            Inc(I);
          end;
          lNode := TSynJSONTreeNode(lNode.getNextSibling);
        until not Assigned(lNode);
      end;
      jtnArrayContent: begin
        repeat
          if Assigned(lNode) then begin
            lNode.Name := IntToStr(I);
            lNode.Text := UTF8ToString(FormatUTF8('[%]: %', [lNode.Name, lNode.Value]));
            Inc(I);
          end;
          lNode := TSynJSONTreeNode(lNode.getNextSibling);
        until not Assigned(lNode);
      end;
    end;
  end;

  procedure UpdateEmptyParent(aParent: TSynJSONTreeNode);
  begin
    if (aParent.Count = 0) then begin
      aParent.Value := NULL;
      case TSynJSONTreeNode(aParent.Parent).JSONType of
        jtnObject: aParent.JSONType := jtnObjectContent;
        jtnArray: aParent.JSONType := jtnArrayContent;
      end;
    end;
  end;

var
  lParent, lDeletedNode: TSynJSONTreeNode;
begin
  if aAsk then
    if (YesNo(UTF8ToString(FormatUTF8(SDeleteNode, [Node.Text])), '', False) <> mrYes) then
      Exit;
  lDeletedNode := Node.Clone;
  lParent := TSynJSONTreeNode(Node.Parent);
  Items.BeginUpdate;
  try
    Node.Delete;
    Node := lDeletedNode;
    // if node name is empty it means that the JSON object is empty
    if (Node.Name = '') then begin
      ClearAll;
      Exit;
    end;
    case Node.JSONType of
      jtnArray, jtnObject: begin
        if (StrToIntDef(Node.Name, MaxInt) <> MaxInt) then
          TDocVariantData(lParent.Value).Delete(StrToInt(Node.Name))
        else
          TDocVariantData(lParent.Value).Delete(Node.Name);
        if (lParent.Count = 0) then
          UpdateEmptyParent(lParent)
        else
          RecalculateChildNames(lParent);
        UpdateNodeInfo(lParent);
      end;
      jtnArrayContent: begin
        TDocVariantData(lParent.Value).Delete(StrToInt(Node.Name));
        if (lParent.Count = 0) then
          UpdateEmptyParent(lParent)
        else
          RecalculateChildNames(lParent);
        UpdateNodeInfo(lParent);
      end;
      jtnObjectContent: begin
        TDocVariantData(lParent.Value).Delete(Node.Name);
        if (lParent.Count = 0) then
          UpdateEmptyParent(lParent);
        UpdateNodeInfo(lParent);
      end;
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TSynJSONTreeView.InsertNode(Node: TSynJSONTreeNode; const aJSON: Variant);
var
  lJSON: Variant;
  lPrompt: RawUTF8;
  lValue: Variant;
  I: Integer;
  lCount: Integer;
  lHandled: Boolean;
begin
  if (aJSON = Unassigned) then begin
    lPrompt := SInsertNewItemPrompt;

    if Assigned(fOnCustomInput) then
      fOnCustomInput(Self, Node, lPrompt, lValue, lHandled);

    if not lHandled then
      lValue := InputBox(SInsertNewItem, lPrompt, '');


    if (lValue = '') then
      Exit;

    if (lValue[1] <> '[') then
      if (lValue[1] <> '{') then
        lValue := '{' + lValue + '}';
    TDocVariantData(lJSON).InitJSON(lValue);
    if (TDocVariantData(lJSON).Kind = dvUndefined) then
      raise ESynJSONTreeView.CreateUTF8(SInvalidJSON, [lValue]);
  end else
    lJSON := aJSON;
  // always will be an object the inserted node
  lCount := lJSON._Count;
  TDocVariantData(TSynJSONTreeNode(Node).Value).AddOrUpdateObject(lJSON);
  for I := 0 to lCount-1 do begin
    lValue := lJSON._(I);
    if IsSimpleJsonValue(lValue) then begin
      AddChildFromJSON(Node, lJSON, I);
    end else
      ProcessObject(Node, _CopyFast(lValue), lJSON.Name(I));
  end;
  UpdateNodeInfo(Node);
end;

function TSynJSONTreeView.AddChildFromJSON(const aNode: TSynJSONTreeNode; const aJSON: Variant; const aNameOrIndex: Variant): TSynJSONTreeNode;
var
  lValue: Variant;
begin
  Result := TSynJSONTreeNode(Items.AddChild(aNode, ''));
  if not VarIsEmpty(aNameOrIndex) and (VariantToIntegerDef(aNameOrIndex, -1) <> -1) then
    lValue := VariantToRaw(aJSON._(aNameOrIndex))
  else
    lValue := aJSON;

  if (TDocVariantData(lValue).Kind <> dvUndefined) then
    Result.Value := _CopyFast(lValue)
  else
    Result.Value := lValue;

  case TDocVariantData(aJSON).Kind of
    dvObject:
      if not VarIsEmpty(aNameOrIndex) then begin
        // is name or index is numeric, the text property is assigned showing the name and value, otherwise is a
        // container node
        if (VariantToIntegerDef(aNameOrIndex, -1) <> -1) then begin
          Result.Name := aJSON.Name(aNameOrIndex);
          Result.JSONType := jtnObjectContent;
          Result.Text := Result.Name + ': ' + VarToUTF8(lValue);
        end else begin
          Result.Name := aNameOrIndex;
          Result.JSONType := jtnObject;
        end;
      end;
    dvArray:
      if not VarIsEmpty(aNameOrIndex) then begin
        // is name or index is numeric, the text property is assigned showing the name and value, otherwise is a
        // container node
        if (VariantToIntegerDef(aNameOrIndex, -1) <> -1) then begin
          Result.Name := aNameOrIndex;
          Result.JSONType := jtnArrayContent;
          Result.Text := IntToStr(aNameOrIndex) + ': ' + VarToUTF8(lValue);
        end else begin
          Result.Name := aNameOrIndex;
          Result.JSONType := jtnArray;
        end;
      end;
  end;
end;

procedure TSynJSONTreeView.ProcessObject(const aCurrNode: TSynJSONTreeNode; const aValue: Variant; const aNameOrIndex: Variant);
var
  lCount: Integer;
  I: Integer;
  lCurrNode: TSynJSONTreeNode;
begin
  lCurrNode := AddChildFromJSON(aCurrNode, aValue, aNameOrIndex);
  UpdateNodeInfo(lCurrNode);
  lCount := aValue._Count;
  for I := 0 to lCount - 1 do
    ProcessPair(lCurrNode, _CopyFast(aValue), I);
end;

procedure TSynJSONTreeView.ProcessArray(const aCurrNode: TSynJSONTreeNode; const aValue: Variant; const aNameOrIndex: Variant);
var
  lCount: Integer;
  I: Integer;
  lCurrNode: TSynJSONTreeNode;
begin
  lCurrNode := AddChildFromJSON(aCurrNode, aValue, aNameOrIndex);
  UpdateNodeInfo(lCurrNode);
  lCount := aValue._Count;
  for I := 0 to lCount - 1 do
    ProcessElement(lCurrNode, aValue, I);
end;

procedure TSynJSONTreeView.EditNode(Node: TSynJSONTreeNode);
var
  lPrompt: RawUTF8;
  lJSON, lValue, lDestValue: Variant;
  lHandled: Boolean;
begin
  lPrompt := UTF8ToString(TSynJSONTreeNode(Selected).Name);
  if (Selected.Text[1] = '[') then
    lPrompt := FormatUTF8('[%]', [lPrompt]);
  lValue := '';
  if not VarIsNull(TSynJSONTreeNode(Selected).Value) then
    lValue := TSynJSONTreeNode(Selected).Value;
  if Assigned(fOnCustomInput) then
    fOnCustomInput(Self, Node, lPrompt, lValue, lHandled);
  if not lHandled then
    lValue := InputBox(SEnterValue, lPrompt, lValue);

  // if not modification, exit
  if (lValue = TSynJSONTreeNode(Selected).Value) then
    Exit;

  // remove children if input data was null
  if (TDocVariantData(lValue).Kind = dvUndefined) and
     ((lValue = '') or IdemPChar(pointer(VariantToUTF8(lValue)), 'NULL')) and
     Selected.HasChildren then begin
    if (YesNo(SRemoveChildren, '', False) = mrYes) then begin
      Selected.DeleteChildren;
      TSynJSONTreeNode(Selected).Value := NULL;
      UpdateNodeInfo(TSynJSONTreeNode(Selected));
      Exit;
    end;
  end;

  TDocVariantData(lJSON).InitJSON(lValue);
  case TDocVariantData(lJSON).Kind of
    dvUndefined: begin
      Selected.Text := FormatUTF8('%: %', [lPrompt, lValue]);
      lDestValue := TSynJSONTreeNode(Selected).Value;
      VariantToVariant(lValue, lDestValue);
      TSynJSONTreeNode(Selected).Value := lDestValue;
      if Assigned(Selected.Parent) then begin
        case TSynJSONTreeNode(Selected.Parent).JSONType of
          jtnObject: TSynJSONTreeNode(Selected).JSONType := jtnObjectContent;
          jtnArray: begin
            TSynJSONTreeNode(Selected).JSONType := jtnArrayContent;
            TDocVariantData(TSynJSONTreeNode(Selected.Parent).Value).Values[StrToInt(TSynJSONTreeNode(Selected).Name)] :=
              TSynJSONTreeNode(Selected).Value;
          end;
        end;
      end;
    end;
  else
    case TDocVariantData(lJSON).Kind of
      dvObject: TSynJSONTreeNode(Selected).JSONType := jtnObject;
      dvArray: TSynJSONTreeNode(Selected).JSONType := jtnArray;
    end;
    Node.Value := _CopyFast(lJSON);
    if Node.HasChildren then
      Node.DeleteChildren;
    InsertNode(Node, lJSON);
    UpdateNodeInfo(Node);
  end;
end;

procedure TSynJSONTreeView.LoadFromFile(const aFileName: TFileName);
begin
  if (aFileName = '') then
    Exit;
  SetJSONText(StringFromFile(aFileName));
  LoadJSON;
end;

procedure TSynJSONTreeView.SaveToFile(const aFileName: TFileName);
var
  lJSON: RawUTF8;
begin
  if (aFileName = '') then
    Exit;
  lJSON := ToJSON;
  FileFromString(lJSON, aFileName);
end;

procedure TSynJSONTreeView.DoOnEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit := (estEdit in fAllowedEditionStates);
  if AllowEdit and Assigned(fOnEditingBak) then
    fOnEditingBak(Sender, Node, AllowEdit);
end;

procedure TSynJSONTreeView.VariantToVariant(aSource: Variant; var aDest: Variant);
var
  lVar: TSQLVar;
  lTmp: RawByteString;
begin
  VariantToSQLVar(aDest, lTmp, lVar);
  with lVar do
    case VType of
      ftNull: aDest := aSource;
      ftCurrency:  aDest := aSource.VInt64;
      ftUTF8: aDest := VariantToUTF8(aSource);
      ftInt64: begin
        if (aSource <> '') then
          if (UpperCaseU(aSource) <> 'TRUE') and  (UpperCaseU(aSource) <> 'FALSE') then begin
            aDest := StrToInt64Def(aSource, MaxInt);
            if (aDest = MaxInt) then
              raise ESynJSONTreeView.CreateUTF8('Invalid input data type: %', [aSource]);
          end else
            if (UpperCaseU(aSource) = 'TRUE') then
              aDest := True
            else
              aDest := False;
      end;
      ftDouble: aDest := aSource.VDouble;
    end;
end;

{ TSynJSONTreeNode }

function TSynJSONTreeNode.Clone: TSynJSONTreeNode;
begin
  Result := TSynJSONTreeNode.Create(nil);
  Result.Name := fName;
  Result.JSONType := fJSONType;
  Result.Value := fValue;
end;

end.

