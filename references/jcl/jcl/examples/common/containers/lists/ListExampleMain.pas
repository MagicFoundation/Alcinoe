unit ListExampleMain;

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
    btnIntfArrayList: TButton;
    btnIntfLinkedList: TButton;
    btnIntfVector: TButton;
    btnArrayList: TButton;
    btnLinkedList: TButton;
    btnVector: TButton;
    memResult: TMemo;
    btnMyObjectList: TButton;
    btnAnsiStrArrayList: TButton;
    btnAnsiStrLinkedList: TButton;
    btnAnsiStrVector: TButton;
    btnWideStrArrayList: TButton;
    btnWideStrLinkedList: TButton;
    btnWideStrVector: TButton;
    procedure btnIntfArrayListClick(Sender: TObject);
    procedure btnIntfLinkedListClick(Sender: TObject);
    procedure btnIntfVectorClick(Sender: TObject);
    procedure btnArrayListClick(Sender: TObject);
    procedure btnLinkedListClick(Sender: TObject);
    procedure btnVectorClick(Sender: TObject);
    procedure btnMyObjectListClick(Sender: TObject);
    procedure btnAnsiStrArrayListClick(Sender: TObject);
    procedure btnWideStrArrayListClick(Sender: TObject);
    procedure btnAnsiStrLinkedListClick(Sender: TObject);
    procedure btnWideStrLinkedListClick(Sender: TObject);
    procedure btnAnsiStrVectorClick(Sender: TObject);
    procedure btnWideStrVectorClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  IIntfMyObject = interface
    ['{BA33CBCC-9CB2-4672-BF54-F52C2A0BEFFE}']
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

  IPerson = interface
    ['{755C857B-A9E2-4D9D-8418-541CAEA79679}']
    function GetAge: Integer;
    function GetMarried: Boolean;
    function GetName: string;
    procedure SetAge(Value: Integer);
    procedure SetMarried(Value: Boolean);
    procedure SetName(const Value: string);
    property Age: Integer read GetAge write SetAge;
    property Married: Boolean read GetMarried write SetMarried;
    property Name: string read GetName write SetName;
  end;

  TPerson = class(TInterfacedObject, IPerson)
  private
    FName: string;
    FAge: Integer;
    FMarried: Boolean;
  protected
  { IPerson }
    function GetAge: Integer;
    function GetMarried: Boolean;
    function GetName: string;
    procedure SetAge(Value: Integer);
    procedure SetMarried(Value: Boolean);
    procedure SetName(const Value: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses JclContainerIntf, JclArrayLists, JclLinkedLists, JclVectors, MyObjectList;

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

procedure TMainForm.btnIntfArrayListClick(Sender: TObject);
var
  List, Sub: IJclIntfList;
  MyArray: IJclIntfArray;
  MyObject: IIntfMyObject;
  It: IJclIntfIterator;
  I: Integer;
begin
  memResult.Lines.Clear;
  List := TJclIntfArrayList.Create(DefaultContainerCapacity);
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  List.Add(MyObject);

  MyObject := IIntfMyObject(List.GetObject(0));
  //memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  MyObject := TIntfMyObject.Create;
  MyObject.Int := 41;
  MyObject.Str := 'AnotherString';
  List.Add(MyObject);

  Sub := List.SubList(0, 10);

  // Iteration
  It := Sub.First;
  while It.HasNext do
  begin
    MyObject := IIntfMyObject(It.Next);
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  end;
  // use [] default of Items[]
  MyArray := List as IJclIntfArray;
  for I := 0 to MyArray.Size - 1 do
  begin
    MyObject := IIntfMyObject(MyArray[I]);
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  end;
end;

procedure TMainForm.btnIntfLinkedListClick(Sender: TObject);
var
  List, Sub: IJclIntfList;
  MyObject: IIntfMyObject;
  It: IJclIntfIterator;
begin
  memResult.Lines.Clear;
  List := TJclIntfLinkedList.Create(nil);
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  List.Add(MyObject);
  MyObject := IIntfMyObject(List.GetObject(0));
  memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  MyObject := TIntfMyObject.Create;
  MyObject.Int := 41;
  MyObject.Str := 'AnotherString';
  List.Add(MyObject);

  Sub := List.SubList(1, 10);

  It := Sub.First;
  while It.HasNext do
  begin
    MyObject := IIntfMyObject(It.Next);
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  end;
end;

procedure TMainForm.btnIntfVectorClick(Sender: TObject);
var
  List: IJclIntfList;
  MyObject: IIntfMyObject;
  It: IJclIntfIterator;
  I: Integer;
begin
  memResult.Lines.Clear;
  List := TJclIntfVector.Create(DefaultContainerCapacity);
  try
    MyObject := TIntfMyObject.Create;
    MyObject.Int := 42;
    MyObject.Str := 'MyString';
    List.Add(MyObject);
    MyObject := IIntfMyObject(List.GetObject(0));
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

    MyObject := TIntfMyObject.Create;
    MyObject.Int := 41;
    MyObject.Str := 'AnotherString';
    List.Add(MyObject);
    It := List.First;
    while It.HasNext do
    begin
      MyObject := IIntfMyObject(It.Next);
      memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
    end;
    // Fastest way
    for I := 0 to List.Size - 1 do
    begin
      MyObject := IIntfMyObject(List.Objects[I]);
      memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
    end;
    List.Clear;
  finally
    It := nil; // Force release Iterator before free list !
  end;
end;

procedure TMainForm.btnArrayListClick(Sender: TObject);
var
  List: IJclList;
  MyObject: TMyObject;
  It: IJclIterator;
begin
  memResult.Lines.Clear;
  List := TJclArrayList.Create(DefaultContainerCapacity, True);
  MyObject := TMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  List.Add(MyObject);
  MyObject := TMyObject(List.GetObject(0));
  memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  MyObject := TMyObject.Create;
  MyObject.Int := 41;
  MyObject.Str := 'AnotherString';
  List.Add(MyObject);

  It := List.First;
  while It.HasNext do
  begin
    MyObject := TMyObject(It.Next);
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  end;
  It := List.First;
  while It.HasNext do
  begin
    It.Next;
    It.Remove;
  end;
end;

procedure TMainForm.btnLinkedListClick(Sender: TObject);
var
  List: IJclList;
  MyObject: TMyObject;
  It: IJclIterator;
begin
  memResult.Lines.Clear;
  List := TJclLinkedList.Create(nil, True);
  MyObject := TMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  List.Add(MyObject);
  MyObject := TMyObject(List.GetObject(0));
  memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  MyObject := TMyObject.Create;
  MyObject.Int := 41;
  MyObject.Str := 'AnotherString';
  List.Add(MyObject);

  It := List.First;
  while It.HasNext do
  begin
    MyObject := TMyObject(It.Next);
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  end;
end;

procedure TMainForm.btnVectorClick(Sender: TObject);
var
  List: IJclList;
  MyObject: TMyObject;
  It: IJclIterator;
  I: Integer;
begin
  memResult.Lines.Clear;
  List := TJclVector.Create(DefaultContainerCapacity, True);
  try
    MyObject := TMyObject.Create;
    MyObject.Int := 42;
    MyObject.Str := 'MyString';
    List.Add(MyObject);
    MyObject := TMyObject(List.GetObject(0));
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

    MyObject := TMyObject.Create;
    MyObject.Int := 41;
    MyObject.Str := 'AnotherString';
    List.Add(MyObject);

    It := List.First;
    while It.HasNext do
    begin
      MyObject := TMyObject(It.Next);
      memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
    end;
    // Fastest way
    for I := 0 to List.Size - 1 do
    begin
      MyObject := TMyObject(List.Objects[I]);
      memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
    end;
    List.Clear;
  finally
    It := nil; // Force release Iterator before free list !
    List := nil;
  end;
end;

procedure TMainForm.btnMyObjectListClick(Sender: TObject);
var
  List: IMyObjectList;
  MyObject: TMyObject;
begin
  memResult.Lines.Clear;
  List := TMyObjectList.Create(DefaultContainerCapacity, True);
  MyObject := TMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  List.Add(MyObject);
  memResult.Lines.Add(IntToStr(List.GetObject(0).Int));
  memResult.Lines.Add(List.GetObject(0).Str);
end;

procedure TMainForm.btnAnsiStrArrayListClick(Sender: TObject);
var
  List, Sub: IJclAnsiStrList;
  MyArray: IJclAnsiStrArray;
  It: IJclAnsiStrIterator;
  I: Integer;
  S: string;
begin
  memResult.Lines.Clear;
  List := TJclAnsiStrArrayList.Create(DefaultContainerCapacity);
  List.Add('MyString');

  S := string(List.GetString(0));
  //memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  List.Add('AnotherString');

  Sub := List.SubList(0, 10);
  // Iteration
  It := Sub.First;
  while It.HasNext do
  begin
    S := string(It.Next);
    memResult.Lines.Add(S);
  end;
  // use [] default of Items[]
  MyArray := List as IJclAnsiStrArray;
  for I := 0 to MyArray.Size - 1 do
  begin
    S := string(MyArray[I]);
    memResult.Lines.Add(S);
  end;
end;

procedure TMainForm.btnWideStrArrayListClick(Sender: TObject);
var
  List, Sub: IJclWideStrList;
  MyArray: IJclWideStrArray;
  It: IJclWideStrIterator;
  I: Integer;
  S: string;
begin
  memResult.Lines.Clear;
  List := TJclWideStrArrayList.Create(DefaultContainerCapacity);
  List.Add('MyString');

  S := List.GetString(0);
  //memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  List.Add('AnotherString');

  Sub := List.SubList(0, 10);
  // Iteration
  It := Sub.First;
  while It.HasNext do
  begin
    S := It.Next;
    memResult.Lines.Add(S);
  end;
  // use [] default of Items[]
  MyArray := List as IJclWideStrArray;
  for I := 0 to MyArray.Size - 1 do
  begin
    S := MyArray[I];
    memResult.Lines.Add(S);
  end;
end;

{ TPerson }

function TPerson.GetAge: Integer;
begin
  Result := FAge;
end;

function TPerson.GetMarried: Boolean;
begin
  Result := FMarried;
end;

function TPerson.GetName: string;
begin
  Result := FName;
end;

procedure TPerson.SetAge(Value: Integer);
begin
  FAge := Value;
end;

procedure TPerson.SetMarried(Value: Boolean);
begin
  FMarried := Value;
end;

procedure TPerson.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TMainForm.btnAnsiStrLinkedListClick(Sender: TObject);
var
  List, Sub: IJclAnsiStrList;
  S: string;
  It: IJclAnsiStrIterator;
begin
  memResult.Lines.Clear;
  List := TJclAnsiStrLinkedList.Create(nil);
  List.Add('MyString');
  memResult.Lines.Add(string(List.GetString(0)));

  List.Add('AnotherString');

  Sub := List.SubList(1, 10);

  It := Sub.First;
  while It.HasNext do
  begin
    S := string(It.Next);
    memResult.Lines.Add(S);
  end;
end;

procedure TMainForm.btnWideStrLinkedListClick(Sender: TObject);
var
  List, Sub: IJclWideStrList;
  S: string;
  It: IJclWideStrIterator;
begin
  memResult.Lines.Clear;
  List := TJclWideStrLinkedList.Create(nil);
  List.Add('MyString');
  memResult.Lines.Add(List.GetString(0));

  List.Add('AnotherString');

  Sub := List.SubList(1, 10);

  It := Sub.First;
  while It.HasNext do
  begin
    S := It.Next;
    memResult.Lines.Add(S);
  end;
end;

procedure TMainForm.btnAnsiStrVectorClick(Sender: TObject);
var
  List: IJclAnsiStrList;
  S: string;
  It: IJclAnsiStrIterator;
  I: Integer;
begin
  memResult.Lines.Clear;
  List := TJclAnsiStrVector.Create(DefaultContainerCapacity);
  try
    List.Add('MyString');
    S := string(List.GetString(0));
    memResult.Lines.Add(S);

    List.Add('AnotherString');

    It := List.First;
    while It.HasNext do
    begin
      S := string(It.Next);
      memResult.Lines.Add(S);
    end;
    // Fastest way
    for I := 0 to List.Size - 1 do
    begin
      S := string(List.Strings[I]);
      memResult.Lines.Add(S);
    end;
    List.Clear;
  finally
    It := nil; // Force release Iterator before free list !
    List := nil;
  end;
end;

procedure TMainForm.btnWideStrVectorClick(Sender: TObject);
var
  List: IJclWideStrList;
  S: string;
  It: IJclWideStrIterator;
  I: Integer;
begin
  memResult.Lines.Clear;
  List := TJclWideStrVector.Create(DefaultContainerCapacity);
  try
    List.Add('MyString');
    S := List.GetString(0);
    memResult.Lines.Add(S);

    List.Add('AnotherString');

    It := List.First;
    while It.HasNext do
    begin
      S := It.Next;
      memResult.Lines.Add(S);
    end;
    // Fastest way
    for I := 0 to List.Size - 1 do
    begin
      S := List.Strings[I];
      memResult.Lines.Add(S);
    end;
    List.Clear;
  finally
    It := nil; // Force release Iterator before free list !
    List := nil;
  end;
end;

end.

