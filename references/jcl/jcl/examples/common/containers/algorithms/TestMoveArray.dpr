program TestMoveArray;

{$APPTYPE CONSOLE}

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase,
  JclAlgorithms;

type
  IRefCountInterface = interface
    ['{9A7FEBD5-B31C-466D-9D72-7C80801FC232}']
    function GetRefCount: Integer;
    function GetChar: Char;
    procedure SetChar(C: Char);
  end;

  TRefCountObject = class(TInterfacedObject, IInterface, IRefCountInterface)
  private
    FChar: Char;
  public
    function GetRefCount: Integer;
    function GetChar: Char;
    procedure SetChar(C: Char);
  end;

function TRefCountObject.GetChar: Char;
begin
  Result := FChar;
end;

function TRefCountObject.GetRefCount: Integer;
begin
  Result := RefCount;
end;

procedure TRefCountObject.SetChar(C: Char);
begin
  FChar := C;
end;

procedure PrintArray(const TestArray: TDynIInterfaceArray);
var
  I: Integer;
  AInterface: IInterface;
  ARefCountInterface: IRefCountInterface;
begin
  for I := Low(TestArray) to High(TestArray) do
  begin
    AInterface := TestArray[I];
    if AInterface = nil then
      Write('-')
    else
    if not Supports(AInterface, IRefCountInterface, ARefCountInterface) then
      raise Exception.Create('interface error')
    else
      Write(ARefCountInterface.GetChar);
  end;
end;

procedure AssertEqual(const TestInterface, RefInterface: IInterface);
begin
  if TestInterface <> RefInterface then
    raise Exception.Create('equality error');
end;

procedure AssertRefCount(const AInterface: IInterface; RefCount: Integer);
var
  ARefCountInterface: IRefCountInterface;
begin
  if not Supports(AInterface, IRefCountInterface, ARefCountInterface) then
    raise Exception.Create('interface error')
  else
  // 1 additional ref-count because of ARefCountInterface
  if ARefCountInterface.GetRefCount <> (RefCount + 1) then
    raise Exception.CreateFmt('ref count error, exp %d, act %d', [RefCount, ARefCountInterface.GetRefCount - 1]);
end;

procedure CreateTestArray(var TestArray, RefArray: TDynIInterfaceArray);
var
  Item: IRefCountInterface;
  I: Integer;
begin
  // erase previous content
  SetLength(TestArray, 0);
  SetLength(TestArray, 26);
  SetLength(RefArray, 0);
  SetLength(RefArray, 26);
  for I := Low(TestArray) to High(TestArray) do
  begin
    Item := TRefCountObject.Create;
    Item.SetChar(Chr(Ord('A') + I));
    TestArray[I] := Item;
    RefArray[I] := Item;
  end;
  Item := nil;
  for I := Low(RefArray) to High(TestArray) do
    AssertRefCount(RefArray[I], 2);
end;

// case 1: (FromIndex < ToIndex) and (Count <= (ToIndex - FromIndex))
procedure Test1;
var
  TestArray, RefArray: TDynIInterfaceArray;
  I: Integer;
begin
  CreateTestArray(TestArray, RefArray);

  Write('Test1, array is: ');
  PrintArray(TestArray);
  WriteLn;
  WriteLn('Test1, execute "MoveArray(TestArray, 5, 15, 5);"');

  MoveArray(TestArray, 5, 15, 5);

  Write('Test1, array is: ');
  PrintArray(TestArray);
  WriteLn;

  // [    From                            ]
  // [                             To     ]
  for I := 0 to 4 do   // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
  for I := 5 to 9 do // were moved
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], nil);
  end;
  for I := 10 to 14 do // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
  for I := 15 to 19 do // overriden
  begin
    AssertRefCount(RefArray[I], 1);
    AssertEqual(TestArray[I], RefArray[I-10]);
  end;
  for I := 20 to 25 do // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
end;

// case 2: (FromIndex < ToIndex) and (Count <= (ToIndex - FromIndex))
procedure Test2;
var
  TestArray, RefArray: TDynIInterfaceArray;
  I: Integer;
begin
  CreateTestArray(TestArray, RefArray);

  Write('Test2, array is: ');
  PrintArray(TestArray);
  WriteLn;
  WriteLn('Test2, execute "MoveArray(TestArray, 10, 15, 5);"');

  MoveArray(TestArray, 10, 15, 5);

  Write('Test2, array is: ');
  PrintArray(TestArray);
  WriteLn;

  // [          From                      ]
  // [              To                    ]
  for I := 0 to 9 do   // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
  for I := 10 to 14 do // were moved
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], nil);
  end;
  for I := 15 to 19 do // overriden
  begin
    AssertRefCount(RefArray[I], 1);
    AssertEqual(TestArray[I], RefArray[I-5]);
  end;
  for I := 20 to 25 do // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
end;

// case 3: (FromIndex < ToIndex) and (Count > (ToIndex - FromIndex))
procedure Test3;
var
  TestArray, RefArray: TDynIInterfaceArray;
  I: Integer;
begin
  CreateTestArray(TestArray, RefArray);

  Write('Test3, array is: ');
  PrintArray(TestArray);
  WriteLn;
  WriteLn('Test3, execute "MoveArray(TestArray, 5, 10, 10);"');

  MoveArray(TestArray, 5, 10, 10);

  Write('Test3, array is: ');
  PrintArray(TestArray);
  WriteLn;

  // [             From                   ]
  // [                To                  ]
  for I := 0 to 4 do   // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
  for I := 5 to 9 do // were moved
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], nil);
  end;
  for I := 10 to 14 do // were moved
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I-5]);
  end;
  for I := 15 to 19 do // override
  begin
    AssertRefCount(RefArray[I], 1);
    AssertEqual(TestArray[I], RefArray[I-5]);
  end;
  for I := 20 to 25 do // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
end;

// case 4: (FromIndex > ToIndex) and (Count <= (FromIndex - ToIndex))
procedure Test4;
var
  TestArray, RefArray: TDynIInterfaceArray;
  I: Integer;
begin
  CreateTestArray(TestArray, RefArray);

  Write('Test4, array is: ');
  PrintArray(TestArray);
  WriteLn;
  WriteLn('Test4, execute "MoveArray(TestArray, 15, 5, 5);"');

  MoveArray(TestArray, 15, 5, 5);

  Write('Test4, array is: ');
  PrintArray(TestArray);
  WriteLn;

  // [                          From      ]
  // [    To                              ]
  for I := 0 to 4 do   // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
  for I := 5 to 9 do // overriden
  begin
    AssertRefCount(RefArray[I], 1);
    AssertEqual(TestArray[I], RefArray[I+10]);
  end;
  for I := 10 to 14 do // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
  for I := 15 to 19 do // were moved
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], nil);
  end;
  for I := 20 to 25 do // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
end;

// case 5: (FromIndex > ToIndex) and (Count <= (FromIndex - ToIndex))
procedure Test5;
var
  TestArray, RefArray: TDynIInterfaceArray;
  I: Integer;
begin
  CreateTestArray(TestArray, RefArray);

  Write('Test5, array is: ');
  PrintArray(TestArray);
  WriteLn;
  WriteLn('Test5, execute "MoveArray(TestArray, 15, 10, 5);"');

  MoveArray(TestArray, 15, 10, 5);

  Write('Test5, array is: ');
  PrintArray(TestArray);
  WriteLn;

  // [                From                ]
  // [              To                    ]
  for I := 0 to 9 do   // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
  for I := 10 to 14 do // overriden
  begin
    AssertRefCount(RefArray[I], 1);
    AssertEqual(TestArray[I], RefArray[I+5]);
  end;
  for I := 15 to 19 do // were moved
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], nil);
  end;
  for I := 20 to 25 do // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
end;

// case 6: (FromIndex > ToIndex) and (Count > (FromIndex - ToIndex))
procedure Test6;
var
  TestArray, RefArray: TDynIInterfaceArray;
  I: Integer;
begin
  CreateTestArray(TestArray, RefArray);

  Write('Test6, array is: ');
  PrintArray(TestArray);
  WriteLn;
  WriteLn('Test6, execute "MoveArray(TestArray, 10, 5, 10);"');

  MoveArray(TestArray, 10, 5, 10);

  Write('Test6, array is: ');
  PrintArray(TestArray);
  WriteLn;

  // [                 From               ]
  // [                To                  ]
  for I := 0 to 4 do   // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
  for I := 5 to 9 do // overriden
  begin
    AssertRefCount(RefArray[I], 1);
    AssertEqual(TestArray[I], RefArray[I+5]);
  end;
  for I := 10 to 14 do // were moved
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I+5]);
  end;
  for I := 15 to 19 do // were moved
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], nil);
  end;
  for I := 20 to 24 do // untouched
  begin
    AssertRefCount(RefArray[I], 2);
    AssertEqual(TestArray[I], RefArray[I]);
  end;
end;

begin
  try
    Test1;
    WriteLn;
    WriteLn;
    Test2;
    WriteLn;
    WriteLn;
    Test3;
    WriteLn;
    WriteLn;
    Test4;
    WriteLn;
    WriteLn;
    Test5;
    WriteLn;
    WriteLn;
    Test6;
    ReadLn;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.

