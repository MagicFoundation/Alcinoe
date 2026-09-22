unit StringBuilderUnit;

interface

const
  StringBuilderMemoryBlockLength = 1000;

type
  PStringBuilderMemoryBlock = ^TStringBuilderMemoryBlock;
  TStringBuilderMemoryBlock = record
    Data: array[0..StringBuilderMemoryBlockLength - 1] of byte;
    Count: Cardinal;
    Next: PStringBuilderMemoryBlock;
  end;

  { TStringBuilder }

  TStringBuilder = class
  protected
    FHead, FTail: PStringBuilderMemoryBlock;
    FTotalLength: Cardinal;
    function Min(const a, b: Cardinal): Cardinal;
  public
    property Head: PStringBuilderMemoryBlock read FHead;
    property Tail: PStringBuilderMemoryBlock read FTail;
    property TotalLength: Cardinal read FTotalLength;
    constructor Create;
    procedure Add(const aString: string); overload;
    procedure Add(const aStrings: array of string); overload;
    procedure Append(const aString: string);
    procedure AppendLine;
    function ToString: string;
    procedure Clean;
    destructor Destroy; override;
  end;

implementation

function CreateNewBlock: PStringBuilderMemoryBlock;
begin
  New(result);
  result^.Count := 0;
  result^.Next := nil;
end;

{ TStringBuilder }

function TStringBuilder.Min(const a, b: Cardinal): Cardinal;
begin
  if
    a < b
  then
    result := a
  else
    result := b;
end;

constructor TStringBuilder.Create;
begin
  FHead := nil;
  FTail := nil;
  FTotalLength := 0;
end;

procedure TStringBuilder.Add(const aString: string);
var
  bytesLeftInString, bytesToWriteInCurrentBlock, bytesLeftInTailBlock: Cardinal;
  positionInString: PChar;
begin
  if
    nil = Head
  then
  begin
    FHead := CreateNewBlock;
    FTail := Head;
  end;
  bytesLeftInString := Length(aString);
  Inc(FTotalLength, bytesLeftInString);
  positionInString := PChar(aString);
  while
    bytesLeftInString > 0
  do
  begin
    bytesLeftInTailBlock := StringBuilderMemoryBlockLength - Tail^.Count;
    bytesToWriteInCurrentBlock := Min(bytesLeftInString, bytesLeftInTailBlock);
    Move(positionInString^, Tail^.Data[Tail^.Count], bytesToWriteInCurrentBlock);
    Inc(Tail^.Count, bytesToWriteInCurrentBlock);
    Dec(bytesLeftInString, bytesToWriteInCurrentBlock);
    Inc(positionInString, bytesToWriteInCurrentBlock);
    if
      bytesLeftInString > 0
    then
    begin
      Tail^.Next := CreateNewBlock;
      FTail := Tail^.Next;
    end;
  end;
end;

procedure TStringBuilder.Add(const aStrings: array of string);
var
  i: Cardinal;
begin
  for i := 0 to Length(aStrings) - 1 do
    Add(aStrings[i]);
end;

procedure TStringBuilder.Append(const aString: string);
begin
  Add(aString);
end;

procedure TStringBuilder.AppendLine;
begin
  Add(#13#10);
end;

function TStringBuilder.ToString: string;
var
  currentBlock: PStringBuilderMemoryBlock;
  currentCount: Cardinal;
  currentResultPosition: PChar;
begin
  if
    Head <> nil
  then
  begin
    SetLength(result, Totallength);
    currentResultPosition := PChar(result);
    currentBlock := Head;
    while
      currentBlock <> nil
    do
    begin
      currentCount := currentBlock^.Count;
      Move(currentBlock^.Data[0], currentResultPosition^, currentCount);
      Inc(currentResultPosition, currentCount);
      currentBlock := currentBlock^.Next;
    end;
    currentResultPosition^ := #0;
  end
  else
    result := '';
end;

procedure TStringBuilder.Clean;
var
  current, next: PStringBuilderMemoryBlock;
begin
  current := Head;
  while
    current <> nil
  do
  begin
    next := current^.Next;
    Dispose(current);
    current := next;
  end;
  FHead := nil;
  FTail := nil;
  FTotalLength := 0;
end;

destructor TStringBuilder.Destroy;
begin
  Clean;
  inherited Destroy;
end;

end.

