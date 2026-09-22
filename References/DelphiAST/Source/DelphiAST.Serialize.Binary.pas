 unit DelphiAST.Serialize.Binary;
 
 {$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Classes,
  Generics.Collections,
  DelphiAST.Consts,
  DelphiAST.Classes;

type
  TNodeClass = (ntSyntax, ntCompound, ntValued, ntComment);

  TBinarySerializer = class
  strict private
    FStream     : TStream;
    FStringList : TStringList;
    FStringTable: TDictionary<string,integer>;
  strict protected
    function CheckSignature: boolean;
    function CheckVersion: boolean;
    function CreateNode(nodeClass: TNodeClass; nodeType: TSyntaxNodeType): TSyntaxNode;
    function ReadNode(var node: TSyntaxNode): boolean;
    function ReadNumber(var num: cardinal): Boolean;
    function ReadString(var str: string): boolean;
    function WriteNode(Node: TSyntaxNode): Boolean;
    function WriteNumber(Num: cardinal): Boolean;
    function WriteString(const S: string): Boolean;
  public
    function Read(Stream: TStream; var Root: TSyntaxNode): boolean;
    function Write(Stream: TStream; Root: TSyntaxNode): boolean;
  end;

implementation

uses
  SysUtils;

var
  CSignature: AnsiString = 'DAST binary file'#26;

function TBinarySerializer.CheckSignature: boolean;
var
  sig: AnsiString;
begin
  SetLength(sig, Length(CSignature));
  Result := (FStream.Read(sig[1], Length(CSignature)) = Length(CSignature))
        and (sig = CSignature);
end;

function TBinarySerializer.CheckVersion: boolean;
var
  version: Integer;
begin
  Result := (FStream.Read(version, 4) = 4)
        and ((version AND $FFFF0000) = $01000000);
end;

function TBinarySerializer.CreateNode(nodeClass: TNodeClass; nodeType: TSyntaxNodeType):
  TSyntaxNode;
begin
  case nodeClass of
    ntSyntax:   Result := TSyntaxNode.Create(nodeType);
    ntCompound: Result := TCompoundSyntaxNode.Create(nodeType);
    ntValued:   Result := TValuedSyntaxNode.Create(nodeType);
    ntComment:  Result := TCommentNode.Create(nodeType);
    else raise Exception.Create('TBinarySerializer.CreateNode: Unexpected node class');
  end;
end;

function TBinarySerializer.Read(Stream: TStream; var Root: TSyntaxNode): boolean;
var
  node: TSyntaxNode;
begin
  Result := false;

  FStringList := TStringList.Create;
  try
    FStream := Stream;
    if not CheckSignature then
      Exit;
    if not CheckVersion then
      Exit;
    if not ReadNode(node) then
      Exit;
    Root := node;
  finally FStringList.Free; end;

  Result := true;
end;

function TBinarySerializer.ReadNode(var node: TSyntaxNode): boolean;
var
  childNode: TSyntaxNode;
  i        : Integer;
  nodeClass: TNodeClass;
  num      : cardinal;
  numSub: cardinal;
  str      : string;
begin
  Result := false;
  node := nil;
  if (not ReadNumber(num)) or (num > cardinal(Ord(High(TNodeClass)))) then
    Exit;
  nodeClass := TNodeClass(num);
  if (not ReadNumber(num)) or (num > Ord(High(TSyntaxNodeType))) then
    Exit;
  node := CreateNode(nodeClass, TSyntaxNodeType(num));
  try

    if (not ReadNumber(num)) or (num > cardinal(High(integer))) then
      Exit;
    Node.Col := num;
    if (not ReadNumber(num)) or (num > cardinal(High(integer))) then
      Exit;
    Node.Line := num;

    case nodeClass of
      ntCompound:
        begin
          if (not ReadNumber(num)) or (num > cardinal(High(integer))) then
            Exit;
          TCompoundSyntaxNode(Node).EndCol := num;
          if (not ReadNumber(num)) or (num > cardinal(High(integer))) then
            Exit;
          TCompoundSyntaxNode(Node).EndLine := num;
        end;
      ntValued:
        begin
          if not ReadString(str) then
            Exit;
          TValuedSyntaxNode(Node).Value := str;
        end;
      ntComment:
        begin
          if not ReadString(str) then
            Exit;
          TCommentNode(Node).Text := str;
        end;
    end;

    if not ReadNumber(numSub) then
      Exit;
    for i := 1 to numSub do begin
      if (not ReadNumber(num)) or (num > cardinal(Ord(High(TAttributeName)))) then
        Exit;
      if not ReadString(str) then
        Exit;
      Node.SetAttribute(TAttributeName(num), str);
    end;

    if not ReadNumber(numSub) then
      Exit;
    for i := 1 to numSub do begin
      if not ReadNode(childNode) then
        Exit;
      Node.AddChild(childNode);
    end;

    Result := true;
  finally
    if not Result then begin
      node.Free;
      node := nil;
    end;
  end;
end;

function TBinarySerializer.ReadNumber(var num: cardinal): Boolean;
var
  lowPart: byte;
  shift  : Integer;
begin
  Result := false;

  shift := 0;
  num := 0;
  repeat
    if FStream.Read(lowPart, 1) <> 1 then
      Exit;
    num := num OR ((lowPart AND $7F) SHL shift);
    Inc(shift, 7);
  until (lowPart AND $80) = 0;

  Result := true;
end;

function TBinarySerializer.ReadString(var str: string): boolean;
var
  id: integer;
  len: cardinal;
  u8: UTF8String;
begin
  Result := false;

  if not ReadNumber(len) then
    Exit;
  if (len SHR 24) = $FF then begin
    id := len AND $00FFFFFF;
    if id >= FStringList.Count then
      Exit;
    str := FStringList[id];
  end
  else begin
    SetLength(u8, len);
    if len > 0 then
      if cardinal(FStream.Read(u8[1], len)) <> len then
        Exit;
    str := UTF8ToUnicodeString(u8);
    if Length(Str) > 4 then
      FStringList.Add(str);
  end;

  Result := true;
end;

function TBinarySerializer.Write(Stream: TStream; Root: TSyntaxNode): boolean;
var
  version: Integer;
begin
  Result := false;

  FStringTable := TDictionary<string,integer>.Create;
  try
    FStream := Stream;
    if FStream.Write(CSignature[1], Length(CSignature)) <> Length(CSignature) then
      Exit;
    version := $01000000;
    if FStream.Write(version, 4) <> 4 then
      Exit;
    if not WriteNode(Root) then
      Exit;
  finally FStringTable.Free; end;

  Result := true;
end;

function TBinarySerializer.WriteNode(Node: TSyntaxNode): Boolean;
var
  attr     : TAttributeEntry;
  childNode: TSyntaxNode;
  nodeClass: TNodeClass;
begin
  Result := false;

  if Node is TCompoundSyntaxNode then
    nodeClass := ntCompound
  else if Node is TValuedSyntaxNode then
    nodeClass := ntValued
  else if Node is TCommentNode then
    nodeClass := ntComment
  else
    nodeClass := ntSyntax;

  if not WriteNumber(Ord(nodeClass)) then Exit;
  if not WriteNumber(Ord(Node.Typ)) then Exit;
  if not WriteNumber(Node.Col) then Exit;
  if not WriteNumber(Node.Line) then Exit;

  case nodeClass of
    ntCompound:
      begin
        if not WriteNumber(TCompoundSyntaxNode(Node).EndCol) then Exit;
        if not WriteNumber(TCompoundSyntaxNode(Node).EndLine) then Exit;
      end;
    ntValued:
      if not WriteString(TValuedSyntaxNode(Node).Value) then Exit;
    ntComment:
      if not WriteString(TCommentNode(Node).Text) then Exit;
  end;

  if not WriteNumber(Length(Node.Attributes)) then Exit;
  for attr in Node.Attributes do begin // causes dynamic array assignment, yuck
    if not WriteNumber(Ord(attr.Key)) then Exit;
    if not WriteString(attr.Value) then Exit;
  end;

  if not WriteNumber(Length(Node.ChildNodes)) then Exit;
  for childNode in Node.ChildNodes do // causes dynamic array assignment, yuck
    if not WriteNode(childNode) then Exit;

  Result := true;
end;

function TBinarySerializer.WriteNumber(Num: cardinal): Boolean;
var
  lowPart: byte;
begin
  Result := false;

  repeat
    lowPart := Num AND $7F;
    Num := Num SHR 7;
    if Num <> 0 then
      lowPart := lowPart OR $80;
    if FStream.Write(lowPart, 1) <> 1 then
      Exit;
  until Num = 0;

  Result := true;
end;

function TBinarySerializer.WriteString(const S: string): Boolean;
var
  i: Integer;
  id: integer;
  u8: UTF8String;
begin
  Result := false;

  if (Length(S) > 4) and FStringTable.TryGetValue(S, id) then begin
    if not WriteNumber(cardinal(id) OR $FF000000) then
      Exit;
  end
  else begin
    if Length(S) > 4 then begin
      FStringTable.Add(S, FStringTable.Count);
      if FStringTable.Count > $FFFFFF then
        raise Exception.Create('TBinarySerializer.WriteString: Too many strings!');
    end;
    u8 := UTF8Encode(s);
    i := Length(u8);
    if not WriteNumber(i) then
      Exit;
    if i > 0 then
      if FStream.Write(u8[1], i) <> i then
        Exit;
  end;

  Result := true;
end;

end.
