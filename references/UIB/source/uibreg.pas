(********************************************************************************)
(*                        UNIFIED INTERBASE (UIB)                               *)
(*                                                                              *)
(* The contents of this file are subject to the Mozilla Public License Version  *)
(* 1.1 (the "License"); you may not use this file except in compliance with the *)
(* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *)
(*                                                                              *)
(* Software distributed under the License is distributed on an "AS IS" basis,   *)
(* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *)
(* the specific language governing rights and limitations under the License.    *)
(*                                                                              *)
(* Unit owner : Henri Gourvest <hgourvest@progdigy.com>                         *)
(*                                                                              *)
(********************************************************************************)

unit uibreg;

{$I uib.inc}

interface
uses
  uib, Classes,
  {$IFDEF COMPILER6_UP}
    DesignEditors, DesignIntf,
  {$ELSE}
    DsgnIntf,
  {$ENDIF}
  {$IFNDEF UIBPEVERSION}
     Db,
     {$IFNDEF BCB}
     DSDesign, DBReg,
     {$ENDIF}
  {$ENDIF}
  {$IFDEF UNIX} QForms, QControls{$ELSE} Forms, Controls{$ENDIF};
type
  TUIBDatabaseEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TUIBTransactionEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
{$IFDEF HAVE_SYNEDIT}
  TUIBSQLEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
{$ENDIF HAVE_SYNEDIT}

{$IFDEF HAVE_SYNEDIT}
  TUIBStatementSQLProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
{$ENDIF HAVE_SYNEDIT}

{$IFNDEF UIBPEVERSION}

{$IFNDEF BCB}
  TUIBDSDesigner = class(TDSDesigner)
  public
  {$IFDEF COMPILER10_UP}
    function DoCreateField(const FieldName: WideString; Origin: string): TField; override;
  {$ELSE}
    function DoCreateField(const FieldName: string; Origin: string): TField; override;
  {$ENDIF}
    //constructor Create()
  end;

  TUIBDatasetEditor = class(TDataSetEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
  {$IFDEF HAVE_SYNEDIT}
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  {$ENDIF}
  // there is a bug on Kylix with the property editor.
  {$IFNDEF KYLIX}
    procedure Edit; override;
  {$ENDIF}
  end;
{$ENDIF}

{$IFDEF HAVE_SYNEDIT}
  TUIBDatasetSQLProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
{$ENDIF HAVE_SYNEDIT}
{$ENDIF UIBPEVERSION}

procedure Register;

implementation
uses
{$IFNDEF UIBPEVERSION}
  uibdataSet,
{$ENDIF}
  uibdatabaseedit,uibtransactionedit,
{$IFDEF HAVE_SYNEDIT}
  uibsqledit,
{$ENDIF HAVE_SYNEDIT}
  dialogs, Math;
{$IFDEF UNIX}
{$R ../Resources/uibReg.dcr}
{$ELSE}
{$R ..\Resources\uibReg.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponentEditor(TUIBDatabase, TUIBDatabaseEditor);
  RegisterComponentEditor(TUIBTransaction, TUIBTransactionEditor);
{$IFDEF HAVE_SYNEDIT}
  RegisterComponentEditor(TUIBStatement, TUIBSQLEditor);
{$ENDIF HAVE_SYNEDIT}

{$IFDEF HAVE_SYNEDIT}
  RegisterPropertyEditor(TypeInfo(TStrings), TUIBStatement, 'SQL', TUIBStatementSQLProperty);
{$ENDIF HAVE_SYNEDIT}
  RegisterComponents('UIB', [TUIBDatabase, TUIBTransaction, TUIBQuery,
    TUIBScript, TUIBBackup, TUIBRestore, TUIBSecurity, TUIBRepair,
    TUIBEvents, TUIBConfig, TUIBServerInfo]);
{$IFNDEF UIBPEVERSION}
  RegisterComponents('UIB', [TUIBDataSet]);
{$IFNDEF BCB}
  RegisterComponentEditor(TUIBDataSet, TUIBDatasetEditor);
{$ENDIF}
{$IFDEF HAVE_SYNEDIT}
  RegisterPropertyEditor(TypeInfo(TStrings), TUIBDataset, 'SQL', TUIBDatasetSQLProperty);
{$ENDIF}
  RegisterFields([TUIBBCDField]);
{$ENDIF}
end;

{ TUIBDatabaseEditor }
procedure TUIBDatabaseEditor.ExecuteVerb(Index: Integer);
begin
  with TUIBDatabaseEditForm.Create(Application) do
  try
    Database := TUIBDataBase(Component);
    if ShowModal = mrOk then
      inherited Designer.Modified;
  finally
    Free;
  end;
end;

function TUIBDatabaseEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Database Editor ...';
end;

function TUIBDatabaseEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TUIBTransactionEditor }

procedure TUIBTransactionEditor.ExecuteVerb(Index: Integer);
begin
  with TUIBTransactionEditForm.Create(Application) do
  try
    Transaction := TUIBTransaction(Component);
    if ShowModal = mrOk then
      inherited Designer.Modified;
  finally
    Free;
  end;
end;

function TUIBTransactionEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Transaction Editor ...';
end;

function TUIBTransactionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TUIBSQLEditor }
{$IFDEF HAVE_SYNEDIT}
procedure TUIBSQLEditor.ExecuteVerb(Index: Integer);
begin
  with TUIBSQLEditForm.Create(Application) do
  try
    Statement := TUIBStatement(Component);
    if ShowModal = mrOk then
      inherited Designer.Modified;
  finally
    Free;
  end;
end;
{$ENDIF HAVE_SYNEDIT}

{$IFDEF HAVE_SYNEDIT}
function TUIBSQLEditor.GetVerb(Index: Integer): string;
begin
  Result := 'SQL Editor ...';
end;
{$ENDIF HAVE_SYNEDIT}

{$IFDEF HAVE_SYNEDIT}
function TUIBSQLEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;
{$ENDIF HAVE_SYNEDIT}

{ TUIBStatementSQLProperty }

{$IFDEF HAVE_SYNEDIT}
procedure TUIBStatementSQLProperty.Edit;
begin
  with TUIBSQLEditForm.Create(Application) do
  try
    Statement := TUIBStatement(GetComponent(0));
    if ShowModal = mrOk then
      inherited Designer.Modified;
  finally
    Free;
  end;
end;
{$ENDIF HAVE_SYNEDIT}

{$IFDEF HAVE_SYNEDIT}
function TUIBStatementSQLProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;
{$ENDIF HAVE_SYNEDIT}

{$IFNDEF UIBPEVERSION}

{ TUIBDSDesigner }
{$IFNDEF BCB}
{$IFDEF COMPILER10_UP}
function TUIBDSDesigner.DoCreateField(const FieldName: WideString; Origin: String): TField;
{$ELSE}
function TUIBDSDesigner.DoCreateField(const FieldName: string; Origin: string): TField;
{$ENDIF}
var fieldef: TFieldDef;
begin
  fieldef := Dataset.FieldDefList.FieldByName(FieldName);
  if (fieldef <> nil) and (fieldef is TUIBFieldDef) then
    Result := inherited DoCreateField(FieldName, TUIBFieldDef(fieldef).Origin) else
    Result := inherited DoCreateField(FieldName, Origin);
end;
{$ENDIF}
{ TUIBDatasetEditor }

{$IFNDEF BCB}
procedure TUIBDatasetEditor.ExecuteVerb(Index: Integer);
{$IFDEF HAVE_SYNEDIT}
var Stm: TUIBStatement;
{$ENDIF}
begin
  if Index < inherited GetVerbCount then
    Edit
{$IFDEF HAVE_SYNEDIT}
  else
  with TUIBSQLEditForm.Create(Application) do
  try
    Stm := TUIBStatement.Create(nil);
    try
      Stm.Transaction := TUIBDataSet(Component).Transaction;
      Stm.DataBase := TUIBDataSet(Component).Database;
      Stm.SQL.Assign(TUIBDataSet(Component).SQL);
      Statement := Stm;
      if ShowModal = mrOk then
      begin
        TUIBDataSet(Component).SQL.Assign(Stm.SQL);
        inherited Designer.Modified;
      end;
    finally
      Stm.Free;
    end;
  finally
    Free;
  end;
{$ENDIF}
end;

{$IFDEF HAVE_SYNEDIT}
function TUIBDatasetEditor.GetVerb(Index: Integer): string;
begin
  if Index < inherited GetVerbCount then
    Result := inherited GetVerb(Index) else
    Result := 'UIB SQL Editor ...'
end;
{$ENDIF}

{$IFDEF HAVE_SYNEDIT}
function TUIBDatasetEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;
{$ENDIF}

{$IFNDEF KYLIX}
procedure TUIBDatasetEditor.Edit;
begin
  ShowFieldsEditor(Designer, Component as TDataset, TUIBDSDesigner)
end;
{$ENDIF}

{$ENDIF}

{ TUIBDatasetSQLProperty }

{$IFDEF HAVE_SYNEDIT}
procedure TUIBDatasetSQLProperty.Edit;
var Stm: TUIBStatement;
begin
  with TUIBSQLEditForm.Create(Application) do
  try
    Stm := TUIBStatement.Create(nil);
    try
      Stm.Transaction := TUIBDataSet(GetComponent(0)).Transaction;
      Stm.DataBase := TUIBDataSet(GetComponent(0)).Database;
      Stm.SQL.Assign(TUIBDataSet(GetComponent(0)).SQL);
      Statement := Stm;
      if ShowModal = mrOk then
      begin
        TUIBDataSet(GetComponent(0)).SQL.Assign(Stm.SQL);
        inherited Designer.Modified;
      end;
    finally
      Stm.Free;
    end;
  finally
    Free;
  end;
end;
{$ENDIF HAVE_SYNEDIT}

{$IFDEF HAVE_SYNEDIT}
function TUIBDatasetSQLProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;
{$ENDIF HAVE_SYNEDIT}

{$ENDIF UIBPEVERSION}

end.

