unit DelphiAST.Writer;

interface

uses
  {$IFDEF FPC}
     StringBuilderUnit,
  {$ENDIF}
  Classes,
  DelphiAST.Classes, SysUtils;

type
  TSyntaxTreeWriter = class
  private
    class procedure NodeToXML(const Builder: TStringBuilder;
      const Node: TSyntaxNode; Formatted: Boolean); static;
  public
    class function ToXML(const Root: TSyntaxNode;
      Formatted: Boolean = False): string; static;

    {$IFNDEF FPC}
      class function ToBinary(const Root: TSyntaxNode; Stream: TStream): Boolean; static;
    {$ENDIF}
  end;

implementation

uses
  Generics.Collections,
  {$IFNDEF FPC}
    DelphiAST.Serialize.Binary,
  {$ENDIF}
  DelphiAST.Consts;

{$I SimpleParser.inc}
{$IFDEF D18_NEWER}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{ TSyntaxTreeWriter }

class procedure TSyntaxTreeWriter.NodeToXML(const Builder: TStringBuilder;
  const Node: TSyntaxNode; Formatted: Boolean);

  function XMLEncode(const Data: string): string;
  var
    i, n: Integer;

    procedure Encode(const s: string);
    begin
      Move(s[1], Result[n], Length(s) * SizeOf(Char));
      Inc(n, Length(s));
    end;

  begin
    SetLength(Result, Length(Data) * 6);
    n := 1;
    for i := 1 to Length(Data) do
      case Data[i] of
        '<': Encode('&lt;');
        '>': Encode('&gt;');
        '&': Encode('&amp;');
        '"': Encode('&quot;');
        '''': Encode('&apos;');
      else
        Result[n] := Data[i];
        Inc(n);
      end;
    SetLength(Result, n - 1);
  end;

  procedure NodeToXMLInternal(const Node: TSyntaxNode; const Indent: string);
  var
    HasChildren: Boolean;
    NewIndent: string;
    Attr: TPair<TAttributeName, string>;
    ChildNode: TSyntaxNode;
  begin
    HasChildren := Node.HasChildren;
    if Formatted then
    begin
      NewIndent := Indent + '  ';
      Builder.Append(Indent);
    end;
    Builder.Append('<' + UpperCase(SyntaxNodeNames[Node.Typ]));

    Builder.Append('  line_seq="' + IntToStr(Node.LineSeq) + '"');

    if Node is TCompoundSyntaxNode then
    begin
      Builder.Append(' begin_line="' + IntToStr(TCompoundSyntaxNode(Node).Line) + '"');
      Builder.Append(' begin_col="' + IntToStr(TCompoundSyntaxNode(Node).Col) + '"');
      Builder.Append(' end_line="' + IntToStr(TCompoundSyntaxNode(Node).EndLine) + '"');
      Builder.Append(' end_col="' + IntToStr(TCompoundSyntaxNode(Node).EndCol) + '"');
    end else
    begin
      Builder.Append(' line="' + IntToStr(Node.Line) + '"');
      Builder.Append(' col="' + IntToStr(Node.Col) + '"');
    end;

    if Node.FileName <> '' then
      Builder.Append('  file="' + XMLEncode(Node.FileName) + '"');

    if Node is TValuedSyntaxNode then
      Builder.Append(' value="' + XMLEncode(TValuedSyntaxNode(Node).Value) + '"');

    for Attr in Node.Attributes do
      Builder.Append(' ' + AttributeNameStrings[Attr.Key] + '="' + XMLEncode(Attr.Value) + '"');
    if HasChildren then
      Builder.Append('>')
    else
      Builder.Append('/>');
    if Formatted then
      Builder.AppendLine;
    for ChildNode in Node.ChildNodes do
      NodeToXMLInternal(ChildNode, NewIndent);
    if HasChildren then
    begin
      if Formatted then
        Builder.Append(Indent);
      Builder.Append('</' + UpperCase(SyntaxNodeNames[Node.Typ]) + '>');
      if Formatted then
        Builder.AppendLine;
    end;
  end;

begin
  NodeToXMLInternal(Node, '');
end;

{$IFNDEF FPC}
class function TSyntaxTreeWriter.ToBinary(const Root: TSyntaxNode; Stream: TStream):
  Boolean;
var
  Writer: TBinarySerializer;
begin
  Writer := TBinarySerializer.Create;
  try
    Result := Writer.Write(Stream, Root);
  finally FreeAndNil(Writer); end;
end;
{$ENDIF}

class function TSyntaxTreeWriter.ToXML(const Root: TSyntaxNode;
  Formatted: Boolean): string;
var
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    NodeToXml(Builder, Root, Formatted);
    Result := '<?xml version="1.0"?>' + sLineBreak + Builder.ToString;
  finally
    Builder.Free;
  end;
end;

end.
