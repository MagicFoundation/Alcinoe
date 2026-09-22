unit StringUsageLogging;

interface

uses
  Generics.Collections;

type
  TStringUsage = TPair<string,Integer>;

function LogStringUsage: TArray<TStringUsage>;
procedure LogStringUsageToFile(const fileName: string);

implementation

uses
  Generics.Defaults, FastMM4, Math, Classes, SysUtils;

type
  PStrRec = ^StrRec;
  StrRec = packed record
  {$IF defined(CPU64BITS)}
    _Padding: Integer;
  {$IFEND}
    codePage: Word;
    elemSize: Word;
    refCnt: Integer;
    length: Integer;
  end;

procedure Callback(APBlock: Pointer; ABlockSize: NativeInt; AUserData: Pointer);
var
  items: TDictionary<string,Integer>;
  count: Integer;
begin
  items := TDictionary<string,Integer>(AUserData);
  if (DetectClassInstance(APBlock) = nil)
    and (DetectStringData(APBlock, ABlockSize) = stUnicodeString) then
  begin
    items.TryGetValue(string(PByte(APBlock) + SizeOf(StrRec)), count);
    items.AddOrSetValue(string(PByte(APBlock) + SizeOf(StrRec)), count + 1);
  end;
end;

function LogStringUsage: TArray<TStringUsage>;
var
  items: TDictionary<string,Integer>;
  comparer: TComparison<TStringUsage>;
begin
  items := TDictionary<string,Integer>.Create;
  try
    WalkAllocatedBlocks(Callback, items);
    Result := items.ToArray;
    comparer :=
      function(const left, right: TStringUsage): Integer
      begin
        Result := -CompareValue(left.Value, right.Value);
      end;
    TArray.Sort<TStringUsage>(Result, IComparer<TStringUsage>(PPointer(@comparer)^));
  finally
    items.Free;
  end;
end;

procedure LogStringUsageToFile(const fileName: string);
var
  item: TStringUsage;
  f: TFileStream;
  b: TBytes;
  overall: Int64;
begin
  f := TFileStream.Create(fileName, fmCreate);
  b := TEncoding.UTF8.GetPreamble;
  f.Write(b[0], Length(b));
  try
    overall := 0;
    for item in LogStringUsage do
    begin
      if item.Value > 1 then
      begin
        b := TEncoding.UTF8.GetBytes(Format('%s x%d'#13#10,[item.Key, item.Value]));
        f.Write(b[0], Length(b));

        Inc(overall, (SizeOf(StrRec) + Length(item.Key) + 1) * item.Value);
      end;
    end;
    b := TEncoding.UTF8.GetBytes(Format(#13#10'Overall memory wasted: %d KB'#13#10, [overall div 1024]));
    f.Write(b[0], Length(b));
  finally
    f.Free;
  end;
end;

end.
