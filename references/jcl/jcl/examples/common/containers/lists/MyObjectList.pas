unit MyObjectList;

interface

uses
  JclContainerIntf, JclArrayLists;

type
  TMyObject = class(TObject)
  private
    FInt: Integer;
    FStr: string;
  public
    property Int: Integer read FInt write FInt;
    property Str: string read FStr write FStr;
  end;

  // An ArrayList typed with TMyObject
  IMyObjectList = interface
    ['{DB2B366E-2CA6-4AFC-A2C9-3285D252DC3E}']
    function Add(AObject: TMyObject): Boolean; overload;
    function AddAll(const ACollection: IJclCollection): Boolean; overload;
    procedure Clear;
    function Contains(AObject: TMyObject): Boolean;
    function ContainsAll(const ACollection: IJclCollection): Boolean;
    function Equals(const ACollection: IJclCollection): Boolean;
    function First: IJclIterator;
    function IsEmpty: Boolean;
    function Last: IJclIterator;
    function Remove(AObject: TMyObject): Boolean; overload;
    function RemoveAll(const ACollection: IJclCollection): Boolean;
    function RetainAll(const ACollection: IJclCollection): Boolean;
    function Size: Integer;

    procedure Add(Index: Integer; AObject: TMyObject); overload;
    function AddAll(Index: Integer; const ACollection: IJclCollection): Boolean; overload;
    function GetObject(Index: Integer): TMyObject;
    function IndexOf(AObject: TMyObject): Integer;
    function LastIndexOf(AObject: TMyObject): Integer;
    function Delete(Index: Integer): TMyObject; overload;
    procedure SetObject(Index: Integer; AObject: TMyObject);
    function SubList(First, Count: Integer): IJclList;
  end;

  TMyObjectList = class(TJclArrayList, IMyObjectList)
  protected
    { IJclCollection }
    function Add(AObject: TMyObject): Boolean; overload;
    function AddAll(const ACollection: IJclCollection): Boolean; overload;
    procedure IMyObjectList.Clear = Clear;
    function Contains(AObject: TMyObject): Boolean;
    function IMyObjectList.ContainsAll = ContainsAll;
    function IMyObjectList.Equals = CollectionEquals;
    function IMyObjectList.First = First;
    function IMyObjectList.IsEmpty = IsEmpty;
    function IMyObjectList.Last = Last;
    function Remove(AObject: TMyObject): Boolean; overload;
    function IMyObjectList.RemoveAll = RemoveAll;
    function IMyObjectList.RetainAll = RetainAll;
    function IMyObjectList.Size = Size;
  protected
    { IJclList }
    procedure Add(Index: Integer; AObject: TMyObject); overload;
    function AddAll(Index: Integer; const ACollection: IJclCollection): Boolean; overload;
    function GetObject(Index: Integer): TMyObject;
    function IndexOf(AObject: TMyObject): Integer;
    function LastIndexOf(AObject: TMyObject): Integer;
    function Delete(Index: Integer): TMyObject; overload;
    procedure SetObject(Index: Integer; AObject: TMyObject);
    function IMyObjectList.SubList = SubList;
  end;

implementation

{ TMyObjectList }

procedure TMyObjectList.Add(Index: Integer; AObject: TMyObject);
begin
  inherited Insert(Index, AObject);
end;

function TMyObjectList.Add(AObject: TMyObject): Boolean;
begin
  Result := inherited Add(AObject);
end;

function TMyObjectList.AddAll(const ACollection: IJclCollection): Boolean;
begin
  Result := inherited AddAll(ACollection);
end;

function TMyObjectList.AddAll(Index: Integer; const ACollection: IJclCollection): Boolean;
begin
  Result := inherited InsertAll(Index, ACollection);
end;

function TMyObjectList.Contains(AObject: TMyObject): Boolean;
begin
Result := inherited Contains(AObject);
end;

function TMyObjectList.GetObject(Index: Integer): TMyObject;
begin
  Result := TMyObject(inherited GetObject(Index));
end;

function TMyObjectList.IndexOf(AObject: TMyObject): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

function TMyObjectList.LastIndexOf(AObject: TMyObject): Integer;
begin
  Result := inherited LastIndexOf(AObject);
end;

function TMyObjectList.Remove(AObject: TMyObject): Boolean;
begin
  Result := inherited Remove(AObject);
end;

function TMyObjectList.Delete(Index: Integer): TMyObject;
begin
  Result := TMyObject(inherited Delete(Index));
end;

procedure TMyObjectList.SetObject(Index: Integer; AObject: TMyObject);
begin
  inherited SetObject(Index, AObject);
end;

end.

