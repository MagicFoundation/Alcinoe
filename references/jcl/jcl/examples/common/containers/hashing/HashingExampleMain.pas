unit HashingExampleMain;

interface

uses
  {$IFDEF LINUX}
  QForms, QControls, QStdCtrls,
  {$ENDIF}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  {$ENDIF}
  System.SysUtils, System.Classes;
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$ENDIF}
  SysUtils, Classes;
  {$ENDIF ~HAS_UNITSCOPE}

type
  TMainForm = class(TForm)
    btnIntfIntfHashMap: TButton;
    btnIntfHashSet: TButton;
    btnHashMap: TButton;
    btnHashSet: TButton;
    btnAnsiStrIntfHashMap: TButton;
    btnIntfArraySet: TButton;
    btnArraySet: TButton;
    btnAnsiStrAnsiStrHashMap: TButton;
    btnAnsiStrHashMap: TButton;
    btnAnsiStrHashSet: TButton;
    btnAnsiStrArraySet: TButton;
    memResult: TListBox;
    btnWideStrIntfHashMap: TButton;
    btnWideStrWideStrHashMap: TButton;
    btnWideStrHashSet: TButton;
    btnWideStrArraySet: TButton;
    btnWideStrHashMap: TButton;
    procedure btnIntfIntfHashMapClick(Sender: TObject);
    procedure btnAnsiStrIntfHashMapClick(Sender: TObject);
    procedure btnWideStrIntfHashMapClick(Sender: TObject);
    procedure btnHashMapClick(Sender: TObject);
    procedure btnIntfHashSetClick(Sender: TObject);
    procedure btnHashSetClick(Sender: TObject);
    procedure btnIntfArraySetClick(Sender: TObject);
    procedure btnArraySetClick(Sender: TObject);
    procedure btnAnsiStrAnsiStrHashMapClick(Sender: TObject);
    procedure btnWideStrWideStrHashMapClick(Sender: TObject);
    procedure btnAnsiStrHashMapClick(Sender: TObject);
    procedure btnWideStrHashMapClick(Sender: TObject);
    procedure btnAnsiStrHashSetClick(Sender: TObject);
    procedure btnWideStrHashSetClick(Sender: TObject);
    procedure btnAnsiStrArraySetClick(Sender: TObject);
    procedure btnWideStrArraySetClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  IIntfMyObject = interface
    ['{B2CB604F-4F5F-44D8-A86F-6138CD329B42}']
    function GetInt: Integer;
    function GetStr: string;
    procedure SetInt(Value: Integer);
    procedure SetStr(const Value: string);
    property Int: Integer read GetInt write SetInt;
    property Str: string read GetStr write SetStr;
  end;

  TIntfMyObject = class(TInterfacedObject, IIntfMyObject)
  private
    FInt: Integer;
    FStr: string;
  protected
  { IIntfMyObject }
    function GetInt: Integer;
    function GetStr: string;
    procedure SetInt(Value: Integer);
    procedure SetStr(const Value: string);
  end;

  TMyObject = class(TObject)
  private
    FInt: Integer;
    FStr: string;
  public
    property Int: Integer read FInt write FInt;
    property Str: string read FStr write FStr;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses JclContainerIntf, JclHashMaps, JclHashSets, JclArraySets;

{ TIntfMyObject }

function TIntfMyObject.GetInt: Integer;
begin
  Result := FInt;
end;

function TIntfMyObject.GetStr: string;
begin
  Result := FStr;
end;

procedure TIntfMyObject.SetInt(Value: Integer);
begin
  FInt := Value;
end;

procedure TIntfMyObject.SetStr(const Value: string);
begin
  FStr := Value;
end;

procedure TMainForm.btnIntfIntfHashMapClick(Sender: TObject);
var
  Map: IJclIntfIntfMap;
  MyObject: IIntfMyObject;
  KeyObject: TInterfacedObject;
  It: IJclIntfIterator;
begin
  Map := TJclIntfIntfHashMap.Create(DefaultContainerCapacity);
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  KeyObject := TInterfacedObject.Create;
  Map.PutValue(KeyObject, MyObject);
  MyObject := IIntfMyObject(Map.GetValue(KeyObject));
  memResult.Items.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  It := Map.Values.First;
  while It.HasNext do
    memResult.Items.Add(IIntfMyObject(It.Next).Str);
  memResult.Items.Add('--------------------------------------------------------');
end;

procedure TMainForm.btnAnsiStrIntfHashMapClick(Sender: TObject);
var
  Map: IJclAnsiStrIntfMap;
  MyObject: IIntfMyObject;
begin
  Map := TJclAnsiStrIntfHashMap.Create(DefaultContainerCapacity);
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  Map.PutValue('MyKey', MyObject);
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 43;
  MyObject.Str := 'AnotherString';
  Map.PutValue('MyKey2', MyObject);
  MyObject := IIntfMyObject(Map.GetValue('MyKey2'));
  memResult.Items.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  memResult.Items.Add('--------------------------------------------------------');
end;

procedure TMainForm.btnWideStrIntfHashMapClick(Sender: TObject);
var
  Map: IJclWideStrIntfMap;
  MyObject: IIntfMyObject;
begin
  Map := TJclWideStrIntfHashMap.Create(DefaultContainerCapacity);
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  Map.PutValue('MyKey', MyObject);
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 43;
  MyObject.Str := 'AnotherString';
  Map.PutValue('MyKey2', MyObject);
  MyObject := IIntfMyObject(Map.GetValue('MyKey2'));
  memResult.Items.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  memResult.Items.Add('--------------------------------------------------------');
end;

procedure TMainForm.btnHashMapClick(Sender: TObject);
var
  Map: IJclMap;
  MyObject: TMyObject;
  KeyObject: TObject;
  It: IJclIterator;
begin
  Map := TJclHashMap.Create(DefaultContainerCapacity, False, False);
  MyObject := TMyObject.Create;
  KeyObject := TObject.Create;
  try
    MyObject.Int := 42;
    MyObject.Str := 'MyString';
    Map.PutValue(KeyObject, MyObject);
    MyObject := TMyObject(Map.GetValue(KeyObject));
    memResult.Items.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
    It := Map.Values.First;
    while It.HasNext do
      memResult.Items.Add(TMyObject(It.Next).Str);
    memResult.Items.Add('--------------------------------------------------------');
  finally
    MyObject.Free;
    KeyObject.Free;
  end;
end;

procedure TMainForm.btnIntfHashSetClick(Sender: TObject);
var
  MySet: IJclIntfSet;
  MyObject: IIntfMyObject;
  It: IJclIntfIterator;
begin
  MySet := TJclIntfHashSet.Create(DefaultContainerCapacity);
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  MySet.Add(MyObject);
  MySet.Add(MyObject);
  It := MySet.First;
  while It.HasNext do
    memResult.Items.Add(IIntfMyObject(It.Next).Str);
  memResult.Items.Add(IntToStr(MySet.Size));
  memResult.Items.Add('--------------------------------------------------------');
end;

procedure TMainForm.btnHashSetClick(Sender: TObject);
var
  MySet: IJclSet;
  MyObject: TMyObject;
  It: IJclIterator;
begin
  MySet := TJclHashSet.Create(DefaultContainerCapacity, False);
  MyObject := TMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  MySet.Add(MyObject);
  MySet.Add(MyObject);
  It := MySet.First;
  while It.HasNext do
    memResult.Items.Add(TMyObject(It.Next).Str);
  memResult.Items.Add(IntToStr(MySet.Size));
  memResult.Items.Add('--------------------------------------------------------');
end;

procedure TMainForm.btnIntfArraySetClick(Sender: TObject);
var
  MySet: IJclIntfSet;
  MyObject: IIntfMyObject;
  It: IJclIntfIterator;
begin
  MySet := TJclIntfArraySet.Create(DefaultContainerCapacity);
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  MySet.Add(MyObject);
  MySet.Add(MyObject);
  It := MySet.First;
  while It.HasNext do
    memResult.Items.Add(IIntfMyObject(It.Next).Str);
  memResult.Items.Add(IntToStr(MySet.Size));
  memResult.Items.Add('--------------------------------------------------------');
end;

procedure TMainForm.btnArraySetClick(Sender: TObject);
var
  MySet: IJclSet;
  MyObject: TMyObject;
  It: IJclIterator;
begin
  MySet := TJclArraySet.Create(DefaultContainerCapacity, False);
  MyObject := TMyObject.Create;
  try
    MyObject.Int := 42;
    MyObject.Str := 'MyString';
    MySet.Add(MyObject);
    MySet.Add(MyObject);
    It := MySet.First;
    while It.HasNext do
      memResult.Items.Add(TMyObject(It.Next).Str);
    memResult.Items.Add(IntToStr(MySet.Size));
    memResult.Items.Add('--------------------------------------------------------');
  finally
    MyObject.Free;
  end;
end;

procedure TMainForm.btnAnsiStrAnsiStrHashMapClick(Sender: TObject);
var
  Map: IJclAnsiStrAnsiStrMap;
  It: IJclAnsiStrIterator;
begin
  Map := TJclAnsiStrAnsiStrHashMap.Create(DefaultContainerCapacity);
  Map.PutValue('MyKey1', 'MyString1');
  Map.PutValue('MyKey2', 'MyString2');
  Map.PutValue('MyKey3', 'MyString3');
  It := Map.KeySet.First;
  while It.HasNext do
    memResult.Items.Add(string(It.Next));
  It := Map.Values.First;
  while It.HasNext do
    memResult.Items.Add(string(It.Next));
  Map.PutValue('MyKey2', 'AnotherString2');
  memResult.Items.Add(string(Map.GetValue('MyKey2')));
  memResult.Items.Add('--------------------------------------------------------');
end;

procedure TMainForm.btnWideStrWideStrHashMapClick(Sender: TObject);
var
  Map: IJclWideStrWideStrMap;
  It: IJclWideStrIterator;
begin
  Map := TJclWideStrWideStrHashMap.Create(DefaultContainerCapacity);
  Map.PutValue('MyKey1', 'MyString1');
  Map.PutValue('MyKey2', 'MyString2');
  Map.PutValue('MyKey3', 'MyString3');
  It := Map.KeySet.First;
  while It.HasNext do
    memResult.Items.Add(It.Next);
  It := Map.Values.First;
  while It.HasNext do
    memResult.Items.Add(It.Next);
  Map.PutValue('MyKey2', 'AnotherString2');
  memResult.Items.Add(Map.GetValue('MyKey2'));
  memResult.Items.Add('--------------------------------------------------------');
end;

procedure TMainForm.btnAnsiStrHashMapClick(Sender: TObject);
var
  Map: IJclAnsiStrMap;
  MyObject: TMyObject;
  It: IJclAnsiStrIterator;
begin
  Map := TJclAnsiStrHashMap.Create(DefaultContainerCapacity, False);
  MyObject := TMyObject.Create;
  try
    MyObject.Int := 42;
    MyObject.Str := 'MyString';

    Map.PutValue('MyKey1', MyObject);
    MyObject := TMyObject(Map.GetValue('MyKey1'));
    memResult.Items.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
    It := Map.KeySet.First;
    while It.HasNext do
      memResult.Items.Add(string(It.Next));
    memResult.Items.Add('--------------------------------------------------------');
  finally
    MyObject.Free;
  end;
end;

procedure TMainForm.btnWideStrHashMapClick(Sender: TObject);
var
  Map: IJclWideStrMap;
  MyObject: TMyObject;
  It: IJclWideStrIterator;
begin
  Map := TJclWideStrHashMap.Create(DefaultContainerCapacity, False);
  MyObject := TMyObject.Create;
  try
    MyObject.Int := 42;
    MyObject.Str := 'MyString';

    Map.PutValue('MyKey1', MyObject);
    MyObject := TMyObject(Map.GetValue('MyKey1'));
    memResult.Items.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
    It := Map.KeySet.First;
    while It.HasNext do
      memResult.Items.Add(It.Next);
    memResult.Items.Add('--------------------------------------------------------');
  finally
    MyObject.Free;
  end;
end;

procedure TMainForm.btnAnsiStrHashSetClick(Sender: TObject);
var
  MySet: IJclAnsiStrSet;
  It: IJclAnsiStrIterator;
begin
  MySet := TJclAnsiStrHashSet.Create(DefaultContainerCapacity);
  MySet.Add('MyString');
  MySet.Add('MyString');
  It := MySet.First;
  while It.HasNext do
    memResult.Items.Add(string(It.Next));
  memResult.Items.Add(IntToStr(MySet.Size));
  memResult.Items.Add('--------------------------------------------------------');
end;

procedure TMainForm.btnWideStrHashSetClick(Sender: TObject);
var
  MySet: IJclWideStrSet;
  It: IJclWideStrIterator;
begin
  MySet := TJclWideStrHashSet.Create(DefaultContainerCapacity);
  MySet.Add('MyString');
  MySet.Add('MyString');
  It := MySet.First;
  while It.HasNext do
    memResult.Items.Add(It.Next);
  memResult.Items.Add(IntToStr(MySet.Size));
  memResult.Items.Add('--------------------------------------------------------');
end;

procedure TMainForm.btnAnsiStrArraySetClick(Sender: TObject);
var
  MySet: IJclAnsiStrSet;
  It: IJclAnsiStrIterator;
  I: Integer;
begin
  MySet := TJclAnsiStrArraySet.Create(DefaultContainerCapacity);
  for I := 1 to 8 do
    MySet.Add(AnsiString(IntToStr(I)));
  for I := 8 downto 1 do
    MySet.Add(AnsiString(IntToStr(I)));
  MySet.Add('MyString');
  MySet.Add('MyString');
  It := MySet.First;
  while It.HasNext do
    memResult.Items.Add(string(It.Next));
  memResult.Items.Add(IntToStr(MySet.Size));
  memResult.Items.Add('--------------------------------------------------------');
end;

procedure TMainForm.btnWideStrArraySetClick(Sender: TObject);
var
  MySet: IJclWideStrSet;
  It: IJclWideStrIterator;
  I: Integer;
begin
  MySet := TJclWideStrArraySet.Create(DefaultContainerCapacity);
  for I := 1 to 8 do
    MySet.Add(IntToStr(I));
  for I := 8 downto 1 do
    MySet.Add(IntToStr(I));
  MySet.Add('MyString');
  MySet.Add('MyString');
  It := MySet.First;
  while It.HasNext do
    memResult.Items.Add(It.Next);
  memResult.Items.Add(IntToStr(MySet.Size));
  memResult.Items.Add('--------------------------------------------------------');
end;

end.

