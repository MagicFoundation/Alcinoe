unit FileCollect;

interface

uses
  Classes,
  SynCommons,
  mORMot;

type
  TFileItem = class;
  TFlileCollection = class(TInterfacedCollection)
  private
    function GetItem(aIndex: Integer): TFileItem;
  protected
    class function GetClass: TCollectionItemClass; override;
  public
    function Add: TFileItem;
    property Items[aIndex: Integer]: TFileItem read GetItem; default;
  end;

  TFileItem = class(TCollectionItem)
  private
    FName:String;
    FSize:Cardinal;
    FModificationDate: TDateTime;
    FVersion: String;
  public
    procedure Assign(Source:TPersistent);override;
  published
    property Name: String read FName write FName;
    property Size: Cardinal read FSize write FSize;
    property ModificationDate: TDateTime read FModificationDate write FModificationDate;
    property Version: String read FVersion write FVersion;
  end;
                      
implementation

{ TFileItem }

procedure TFileItem.Assign(Source: TPersistent);
begin
  if Source is TFileItem then
    with Source as TFileItem do
    begin
      Self.FName:=Name;
      Self.FSize:=Size;
      Self.FModificationDate:=ModificationDate;
      Self.FVersion:=Version;
    end
    else
      inherited Assign(source);
end;

{ TFlileCollection }

function TFlileCollection.Add: TFileItem;
begin
  Result := TFileItem(inherited Add);
end;

class function TFlileCollection.GetClass: TCollectionItemClass;
begin
  Result := TFileItem;
end;

function TFlileCollection.GetItem(aIndex: Integer): TFileItem;
begin
  Result := TFileItem(inherited GetItem(aIndex));
end;

end.
 