unit Unit1;

interface

uses
  {$IFDEF WIN32}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$ENDIF WIN32}
  {$IFDEF LINUX}
  QForms, QStdCtrls, QControls,
  {$ENDIF LINUX}
  SysUtils, Classes;

type
  TfrmPerf = class(TForm)
    gbxCompareList: TGroupBox;
    btnTList: TButton;
    lblAdd: TLabel;
    lblNext: TLabel;
    lblRandom: TLabel;
    lblAdd10: TLabel;
    lblClear: TLabel;
    btnTArrayList: TButton;
    lblArrayAdd: TLabel;
    lblArrayNext: TLabel;
    lblArrayRandom: TLabel;
    lblArrayAdd10: TLabel;
    lblArrayClear: TLabel;
    btnTLinkedList: TButton;
    lblLinkedAdd: TLabel;
    lblLinkedNext: TLabel;
    lblLinkedRandom: TLabel;
    lblLinkedAdd10: TLabel;
    lblLinkedClear: TLabel;
    btnTSpeedList: TButton;
    lblSpeedAdd: TLabel;
    lblSpeedNext: TLabel;
    lblSpeedRandom: TLabel;
    lblSpeedAdd10: TLabel;
    lblSpeedClear: TLabel;
    gbxCompareHash: TGroupBox;
    btnBucketList: TButton;
    lblBucketAdd: TLabel;
    lblBucketRandom: TLabel;
    lblBucketClear: TLabel;
    btnTHashMap: TButton;
    lblHashAdd: TLabel;
    lblHashRandom: TLabel;
    lblHashClear: TLabel;
    btnHashedString: TButton;
    Button2: TButton;
    lblHashedStringAdd: TLabel;
    lblHashedStringRandom: TLabel;
    lblHashedStringClear: TLabel;
    lblStrStrHashAdd: TLabel;
    lblStrStrHashRandom: TLabel;
    lblStrStrHashClear: TLabel;
    procedure btnTArrayListClick(Sender: TObject);
    procedure btnTListClick(Sender: TObject);
    procedure btnTLinkedListClick(Sender: TObject);
    procedure btnTSpeedListClick(Sender: TObject);
    procedure btnTHashMapClick(Sender: TObject);
    procedure btnBucketListClick(Sender: TObject);
    procedure btnHashedStringClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  public
  end;

  TMyObject = class(TInterfacedObject);

var
  frmPerf: TfrmPerf;

implementation

{$I jcl.inc}

{$R *.dfm}

uses
  {$IFDEF DELPHI5_UP}
  Contnrs, IniFiles,
  {$ENDIF DELPHI5_UP}
  Math,
  JclDCL_intf, JclDCLUtil, JclArrayList, JclLinkedList, JclHashMap, JclVector;

const
  ResultFormat = '%.1f ms';
  MsecsPerDay = 24 * 60 * 60 * 1000;

procedure TfrmPerf.btnTArrayListClick(Sender: TObject);
var
  List: IList;
  It: IIterator;
  I, Res: Integer;
  Start: TDateTime;
begin
  Randomize;
  Screen.Cursor := crHourGlass;
  try
    Start := Now;
    List := TJclArrayList.Create(16, False);
    for I := 0 to 2000000 do
      List.Add(TObject(I));
    lblArrayAdd.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    // Fast but Specific ArrayList
    //for I := 0 to List.Size - 1 do
    //  Res := Integer(List.GetObject(I));
    // Slower but same for every IList
    It := List.First;
    while It.HasNext do
      I := Integer(It.Next);
    lblArrayNext.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 200 do
      Res := List.IndexOf(TObject(Random(1000000)));
    lblArrayRandom.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    It := List.First;
    for I := 0 to 10 do
      It.Next;
    for I := 0 to 100 do
      It.Add(TObject(I));
    lblArrayAdd10.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    List.Clear;
    lblArrayClear.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmPerf.btnTListClick(Sender: TObject);
var
  List: TList;
  I, res: Integer;
  Start: TDateTime;
begin
  Randomize;
  Start := Now;
  List := TList.Create;
  Screen.Cursor := crHourGlass;
  try
    for I := 0 to 2000000 do
      List.Add(Pointer(I));
    lblAdd.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to List.Count - 1 do
      Res := Integer(List[I]);
    lblNext.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 200 do
      Res := List.IndexOf(Pointer(Random(1000000)));
    lblRandom.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 100 do
      List.Insert(10, Pointer(I));
    lblAdd10.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    List.Clear;
    lblClear.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    List.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmPerf.btnTLinkedListClick(Sender: TObject);
var
  List: IList;
  I, Res: Integer;
  It: IIterator;
  Start: TDateTime;
begin
  Randomize;
  Screen.Cursor := crHourGlass;
  try
    Start := Now;
    List := TJclLinkedList.Create(nil, False);
    for I := 0 to 2000000 do
      List.Add(TObject(I));
    lblLinkedAdd.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    It := List.First;
    while It.HasNext do
      I := Integer(It.Next);
    lblLinkedNext.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 200 do
      Res := List.IndexOf(TObject(Random(1000000)));
    lblLinkedRandom.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    It := List.First;
    for I := 0 to 10 do
      It.Next;
    for I := 0 to 100 do
      It.Add(TObject(I));
    lblLinkedAdd10.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    List.Clear;
    lblLinkedClear.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmPerf.btnTSpeedListClick(Sender: TObject);
var
  List: TJclVector;
  I, res: Integer;
  Start: TDateTime;
begin
  Randomize;
  Screen.Cursor := crHourGlass;
  Start := Now;
  List := TJclVector.Create(16, False);
  try
    for I := 0 to 2000000 do
      List.Add(TObject(I));
    lblSpeedAdd.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to List.Size - 1 do
      Res := Integer(List.Items[I]);
    lblSpeedNext.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 200 do
      Res := List.IndexOf(TObject(Random(1000000)));
    lblSpeedRandom.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 10 do
    begin
      System.Move(List.Items[10], List.Items[10 + 1],
        (List.Size - 10) * SizeOf(TObject));
      List.Items[10] := TObject(I);
    end;
    lblSpeedAdd10.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    List.Clear;
    lblSpeedClear.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    List.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmPerf.btnTHashMapClick(Sender: TObject);
var
  Map: IMap;
  I, Res: Integer;
  Start: TDateTime;
begin
  Randomize;
  Screen.Cursor := crHourGlass;
  try
    Start := Now;
    Map := JclHashMap.TJclHashMap.Create(256, False);
    for I := 0 to 100000 do
      Map.PutValue(TObject(Random(100000)), TObject(I));
    lblHashAdd.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 100000 do
      Res := Integer(Map.GetValue(TObject(Random(100000))));
    lblHashRandom.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    Map.Clear;
    lblHashClear.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmPerf.btnBucketListClick(Sender: TObject);
{$IFDEF DELPHI6_UP}
var
  I, Res: Integer;
  Start: TDateTime;
  List: TBucketList;
  {$ENDIF DELPHI6_UP}
begin
  {$IFDEF DELPHI6_UP}
  Randomize;
  Screen.Cursor := crHourGlass;
  Start := Now;
  List := TBucketList.Create(bl256);
  try
    for I := 0 to 100000 do
      List.Add(TObject(I), TObject(I));
    lblBucketAdd.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 100000 do
      Res := Integer(List.Data[TObject(Random(100000))]);
    lblBucketRandom.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    List.Clear;
    lblBucketClear.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    List.Free;
    Screen.Cursor := crDefault;
  end;
  {$ENDIF DELPHI6_UP}
end;

function GenId(Value: Integer): string;
begin
  Result := IntToStr(Value);
end;

procedure TfrmPerf.btnHashedStringClick(Sender: TObject);
{$IFDEF DELPHI6_UP}
var
  I: Integer;
  Index: Integer;
  List: THashedStringList;
  Start: TDateTime;
{$ENDIF DELPHI6_UP}
begin
  {$IFDEF DELPHI6_UP}
  Randomize;
  Screen.Cursor := crHourGlass;
  Start := Now;
  List := THashedStringList.Create;
  try
    for I := 0 to 100000 do
      List.Add(GenId(123));
    lblHashedStringAdd.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 100000 do
    begin
      Index := List.IndexOf(GenId(123));
    end;
    lblHashedStringRandom.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    List.Clear;
    lblHashedStringClear.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    List.Free;
    Screen.Cursor := crDefault;
  end;
  {$ENDIF DELPHI6_UP}
end;

procedure TfrmPerf.Button2Click(Sender: TObject);
var
  Map: IStrStrMap;
  I: Integer;
  Res: string;
  Start: TDateTime;
begin
  Randomize;
  Screen.Cursor := crHourGlass;
  try
    Start := Now;
    Map := TJclStrStrHashMap.Create(256);
    for I := 0 to 100000 do
      Map.PutValue(GenId(123), '');
    lblStrStrHashAdd.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 100000 do
      Res := Map.GetValue(GenId(123));
    lblStrStrHashRandom.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    Map.Clear;
    lblStrStrHashClear.Caption := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.

