unit StringPool;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

type
  TStringBucket = record
    Hash: Cardinal;
    Value: string;
  end;
  PStringBucket = ^TStringBucket;
  TStringBuckets = array of TStringBucket;

  TStringPool = class
  private
    FBuckets: TStringBuckets;
    FCount: Integer;
    FGrowth: Integer;
    FCapacity: Integer;
    procedure Grow;
  public
    procedure StringIntern(var s: string);

    procedure Clear;
    property Count: Integer read FCount;
  end;

implementation

{ TStringPool }

procedure TStringPool.Clear;
begin
  SetLength(FBuckets, 0);
  FCount := 0;
  FGrowth := 0;
  FCapacity := 0;
end;

procedure TStringPool.Grow;
var
  i, j, n: Integer;
  oldBuckets: TStringBuckets;
begin
  if FCapacity = 0 then
    FCapacity := 32
  else
    FCapacity := FCapacity * 2;
  FGrowth := (FCapacity * 3) div 4 - FCount;

  oldBuckets := FBuckets;
  FBuckets := nil;
  SetLength(FBuckets, FCapacity);

  n := FCapacity - 1;
  for i := 0 to High(oldBuckets) do
  begin
    if oldBuckets[i].Hash = 0 then
      Continue;
    j := oldBuckets[i].Hash and (FCapacity - 1);
    while FBuckets[j].Hash <> 0 do
      j := (j + 1) and n;
    FBuckets[j].Hash := oldBuckets[i].Hash;
    FBuckets[j].Value := oldBuckets[i].Value;
  end;
end;

procedure TStringPool.StringIntern(var s: string);

{$OVERFLOWCHECKS OFF}

  function HashString(const s: string): Cardinal; inline;
  var
    i: Integer;
  begin
    // modified FNV-1a using length as seed
    Result := Length(s);
    for i := 1 to Result do
      Result := (Result xor Ord(s[i])) * 16777619;
  end;

{$OVERFLOWCHECKS ON}

var
  hash: Cardinal;
  i: Integer;
  bucket: PStringBucket;
begin
  if s = '' then
    Exit;

  if FGrowth = 0 then
    Grow;

  hash := HashString(s) shr 6;
  i := hash and (FCapacity - 1);

  repeat
    bucket := @FBuckets[i];
    if (bucket.Hash = hash) and (bucket.Value = s) then
    begin
      s := bucket.Value;
      Exit;
    end
    else if bucket.Hash = 0 then
    begin
      bucket.Hash := hash;
      bucket.Value := s;
      Inc(FCount);
      Dec(FGrowth);
      Exit;
    end;
    i := (i + 1) and (FCapacity - 1);
  until False;
end;

end.
