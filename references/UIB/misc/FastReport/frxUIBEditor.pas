{******************************************}
{                                          }
{             FastReport v4.x              }
{      UIB components design editors       }
{                                          }
{         Copyright (c) 2005-2007          }
{            by Pierre Yager.              }
{                                          }
{******************************************}

unit frxUIBEditor;

interface

{$I frx.inc}

implementation

uses
  Windows, Classes, SysUtils, Forms, Dialogs, frxUIBComponents, frxCustomDB,
  frxDsgnIntf, frxRes, uib, uibDataset
{$IFDEF Delphi6}
, Variants
{$ENDIF};


type
  TfrxDatabaseNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    function Edit: Boolean; override;
  end;

  TfrxDatabaseProperty = class(TfrxComponentProperty)
  public
    function GetValue: String; override;
  end;

  TfrxTransactionProperty = class(TfrxComponentProperty)
  public
    function GetValue: String; override;
  end;

{ TfrxDatabaseNameProperty }

function TfrxDatabaseNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paDialog];
end;

function TfrxDatabaseNameProperty.Edit: Boolean;
var
  SaveConnected: Boolean;
begin
  with TOpenDialog.Create(nil) do
  begin
    InitialDir := GetCurrentDir;
    Filter := frxResources.Get('ftDB') + ' (*.gdb)|*.gdb|' +
      frxResources.Get('ftAllFiles') + ' (*.*)|*.*';
    Result := Execute;
    if Result then
      with TfrxUIBDatabase(Component).Database do
      begin
        SaveConnected := Connected;
        Connected := False;
        DatabaseName := FileName;
        Connected := SaveConnected;
      end;
    Free;
  end;
end;

{ TfrxDatabaseProperty }

function TfrxDatabaseProperty.GetValue: String;
var
  db: TfrxUIBDatabase;
begin
  db := TfrxUIBDatabase(GetOrdValue);
  if db = nil then
  begin
    if (UIBComponents <> nil) and (UIBComponents.DefaultDatabase <> nil) then
      Result := UIBComponents.DefaultDatabase.Name
    else
      Result := frxResources.Get('prNotAssigned');
  end
  else
    Result := inherited GetValue;
end;

{ TfrxTransactionProperty }

function TfrxTransactionProperty.GetValue: String;
var
  tr: TfrxUIBTransaction;
begin
  tr := TfrxUIBTransaction(GetOrdValue);
  if tr = nil then
  begin
    if (UIBComponents <> nil) and (UIBComponents.DefaultTransaction <> nil) then
      Result := UIBComponents.DefaultTransaction.Name
    else
      Result := frxResources.Get('prNotAssigned');
  end
  else
    Result := inherited GetValue;
end;

initialization
  frxPropertyEditors.Register(TypeInfo(String), TfrxUIBDataBase, 'DatabaseName',
    TfrxDataBaseNameProperty);

  frxPropertyEditors.Register(TypeInfo(TfrxUIBDatabase), TfrxUIBTransaction, 'Database',
    TfrxDatabaseProperty);

  frxPropertyEditors.Register(TypeInfo(TfrxUIBDatabase), TfrxUIBQuery, 'Database',
    TfrxDatabaseProperty);

  frxPropertyEditors.Register(TypeInfo(TfrxUIBTransaction), TfrxUIBQuery, 'Transaction',
    TfrxTransactionProperty);

  frxHideProperties(TfrxUIBDatabase, 'LoginPrompt');
  frxHideProperties(TfrxUIBQuery, 'Master;Filter;Filtered');

end.
