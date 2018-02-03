unit MyObjectList;

interface

uses
  JclDCL_Intf, JclArrayList;

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
    function AddAll(ACollection: ICollection): Boolean; overload;
    procedure Clear;
    function Contains(AObject: TMyObject): Boolean;
    function ContainsAll(ACollection: ICollection): Boolean;
    function Equals(ACollection: ICollection): Boolean;
    function First: IIterator;
    function IsEmpty: Boolean;
    function Last: IIterator;
    function Remove(AObject: TMyObject): Boolean; overload;
    function RemoveAll(ACollection: ICollection): Boolean;
    function RetainAll(ACollection: ICollection): Boolean;
    function Size: Integer;

    procedure Add(Index: Integer; AObject: TMyObject); overload;
    function AddAll(Index: Integer; ACollection: ICollection): Boolean; overload;
    function GetObject(Index: Integer): TMyObject;
    function IndexOf(AObject: TMyObject): Integer;
    function LastIndexOf(AObject: TMyObject): Integer;
    function Remove(Index: Integer): TMyObject; overload;
    procedure SetObject(Index: Integer; AObject: TMyObject);
    function SubList(First, Count: Integer): IList;
  end;

  TMyObjectList = class(TJclArrayList, IMyObjectList)
  protected
    { ICollection }
    function Add(AObject: TMyObject): Boolean; overload;
    function AddAll(ACollection: ICollection): Boolean; overload;
    procedure IMyObjectList.Clear = Clear;
    function Contains(AObject: TMyObject): Boolean;
    function IMyObjectList.ContainsAll = ContainsAll;
    function IMyObjectList.Equals = Equals;
    function IMyObjectList.First = First;
    function IMyObjectList.IsEmpty = IsEmpty;
    function IMyObjectList.Last = Last;
    function Remove(AObject: TMyObject): Boolean; overload;
    function IMyObjectList.RemoveAll = RemoveAll;
    function IMyObjectList.RetainAll = RetainAll;
    function IMyObjectList.Size = Size;
  protected
    { IList }
    procedure Add(Index: Integer; AObject: TMyObject); overload;
    function AddAll(Index: Integer; ACollection: ICollection): Boolean; overload;
    function GetObject(Index: Integer): TMyObject;
    function IndexOf(AObject: TMyObject): Integer;
    function LastIndexOf(AObject: TMyObject): Integer;
    function Remove(Index: Integer): TMyObject; overload;
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

function TMyObjectList.AddAll(ACollection: ICollection): Boolean;
begin
  Result := inherited AddAll(ACollection);
end;

function TMyObjectList.AddAll(Index: Integer;
  ACollection: ICollection): Boolean;
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

function TMyObjectList.Remove(Index: Integer): TMyObject;
begin
  Result := TMyObject(inherited Remove(Index));
end;

procedure TMyObjectList.SetObject(Index: Integer; AObject: TMyObject);
begin
  inherited SetObject(Index, AObject);
end;

end.

