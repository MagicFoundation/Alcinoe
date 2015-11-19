(*
 *                         Super Object Toolkit
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Embarcadero Technologies Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 *)

 unit superxmlparser;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses superobject, classes, supertypes;

type
  TOnProcessingInstruction = procedure(const PI, PIParent: ISuperObject);

function XMLParseString(const data: SOString; pack: Boolean = false; onpi: TOnProcessingInstruction = nil): ISuperObject;
function XMLParseStream(stream: TStream; pack: Boolean = false; onpi: TOnProcessingInstruction = nil): ISuperObject;
function XMLParseFile(const FileName: string; pack: Boolean = false; onpi: TOnProcessingInstruction = nil): ISuperObject;

{$IFDEF UNICODE}
type
  TXMLWriteMethod = reference to procedure(const data: string);
procedure XMLWrite(const node: ISuperObject; const method: TXMLWriteMethod);
{$ENDIF}

const
  xmlname       = '#name';
  xmlattributes = '#attributes';
  xmlchildren   = '#children';
  xmltext       = '#text';

  dtdname = '#name';
  dtdPubidLiteral = '#pubidliteral';
  dtdSystemLiteral = '#systemliteral';


implementation
uses sysutils {$IFNDEF UNIX}, windows{$ENDIF};

const
  XML_SPACE : PSOChar = #32;
//  XML_ARL: PSOChar = '[';
  XML_ARR: PSOChar = ']';
  XML_BIG: PSOChar = '>';
  XML_LOW: PSOChar = '<';
  XML_AMP: PSOChar = '&';
  XML_SQU: PSOChar = '''';
  XML_DQU: PSOChar = '"';

type
  TSuperXMLState = (
    xsStart,                            // |
    xsEatSpaces,                        //
    xsElement,                          // <|
    xsElementName,                      // <[a..z]|
    xsAttributes,                       // <xml |
    xsAttributeName,                    // <xml a|
    xsEqual,                            // |= ...
    xsAttributeValue,                   // = |"...
    xsCloseEmptyElement,                // <xml/|
    xsTryCloseElement,                  // <xml>..<|
    xsCloseElementName,                 // <xml>..</|
    xsChildren,                         // <xml>|
    xsElementString,                    // <xml>  |azer
    xsElementComment,                   // <!|-- ...
    xsElementDocType,                   // <!D|
    xsElementDocTypeName,               // <!DOCTYPE |...
    xsElementDocTypeExternId,           // <!DOCTYPE xml |
    xsElementDocTypeExternIdPublic,     // <!DOCTYPE xml P|
    xsElementDocTypeExternIdSystem,     // <!DOCTYPE xml S|
    xsElementDocTypePubIdLiteral,       // <!DOCTYPE xml SYSTEM |"
    xsElementDocTypeSystemLiteral,      // <!DOCTYPE xml SYSTEM "" |""
    xsElementDocTypeTryIntSubset,
    xsElementDocTypeIntSubset,
    xsElementDocTypeTryClose,
    xsElementDocTypeEat,                //
    xsCloseElementComment,              // <!-- -|->
    xsElementPI,                        // <?|
    xsElementDataPI,                    // not an xml PI
    xsCloseElementPI,                   // <? ?|>
    xsElementCDATA,                     // <![|CDATA[
    xsClodeElementCDATA,                // ]|]>
    xsEscape,                           // &|
    xsEscape_lt,                        // &l|t;
    xsEscape_gt,                        // &g|t;
    xsEscape_amp,                       // &a|mp;
    xsEscape_apos,                      // &a|pos;
    xsEscape_quot,                      // &q|uot;
    xsEscape_char,                      // &#|;
    xsEscape_char_num,                  // &#1|123456;
    xsEscape_char_hex,                  // &#x|000FFff;
    xsEnd);

  TSuperXMLError = (xeSuccess, xeContinue, xeProcessInst, xeError);
  TSuperXMLElementClass = (xcNone, xcElement, xcComment, xcString, xcCdata, xcDocType, xcProcessInst);
  TSuperXMLEncoding = ({$IFNDEF UNIX}xnANSI,{$ENDIF} xnUTF8, xnUnicode);

{$IFDEF UNICODE}
  procedure XMLWrite(const node: ISuperObject; const method: TXMLWriteMethod);
     procedure Escape(const str: string);
    var
      p1, p2: PChar;
      procedure push(const data: string);
      begin
        if p2 > p1 then
          method(Copy(p1, 0, p2-p1));
        Inc(p2);
        p1 := p2;
        if data <> '' then
          method(data);
      end;
    begin
      p1 := PChar(str);
      p2 := p1;

      while True do
        case p2^ of
          '<': push('&lt;');
          '>': push('&gt;');
          '&': push('&amp;');
          '"': push('&quot;');
          #0 :
            begin
              push('');
              Break;
            end;
        else
          inc(p2);
        end;
    end;
  var
    o: ISuperObject;
    ent: TSuperAvlEntry;
  begin
    method('<' + node.S[xmlname]);
    if ObjectIsType(node[xmlattributes], stObject) then
      for ent in node[xmlattributes].AsObject do
      begin
        method(' ' + ent.Name + '="');
        Escape(ent.Value.AsString);
        method('"');
      end;
    if ObjectIsType(node[xmlchildren], stArray) then
    begin
      method('>');
      for o in node[xmlchildren] do
        if ObjectIsType(o, stString) then
          Escape(o.AsString) else
          XMLWrite(o, method);
      method('</' + node.S[xmlname] + '>');
    end else
      method('/>');
  end;
{$ENDIF}

type
  PSuperXMLStack = ^TSuperXMLStack;
  TSuperXMLStack = record
    state: TSuperXMLState;
    savedstate: TSuperXMLState;
    prev: PSuperXMLStack;
    next: PSuperXMLStack;
    clazz: TSuperXMLElementClass;
    obj: ISuperObject;
  end;

  TSuperXMLParser = class
  private
    FStack: PSuperXMLStack;
    FDocType: ISuperObject;
    FError: TSuperXMLError;
    FStr: TSuperWriterString;
    FValue: TSuperWriterString;
    FPosition: Integer;
    FAChar: SOChar;
    FPack: Boolean;
    procedure StackUp;
    procedure StackDown;
    procedure Reset;
    function ParseBuffer(data: PSOChar; var PI, PIParent: ISuperObject; len: Integer = -1): Integer;
  public
    constructor Create(pack: Boolean);
    destructor Destroy; override;
  end;

{ TXMLContext }

constructor TSuperXMLParser.Create(pack: Boolean);
begin
  FDocType := nil;
  FStr := TSuperWriterString.Create;
  FValue := TSuperWriterString.Create;
  StackUp;
  FError := xeSuccess;
  FPack := pack;
end;

destructor TSuperXMLParser.Destroy;
begin
  while FStack <> nil do
    StackDown;
  FStr.Free;
  FValue.Free;
end;

procedure TSuperXMLParser.Reset;
begin
  while FStack <> nil do
    StackDown;
  StackUp;
  FError := xeSuccess;
end;

function TSuperXMLParser.ParseBuffer(data: PSOChar; var PI, PIParent: ISuperObject; len: integer): Integer;
const
  spaces = [#32,#9,#10,#13];
  alphas = ['a'..'z', 'A'..'Z', '_', ':', #161..#255];
  nums = ['0'..'9', '.', '-'];
  hex = nums + ['a'..'f','A'..'F'];
  alphanums = alphas + nums;
  publitteral = [#32, #13, #10, 'a'..'z', 'A'..'Z', '0'..'9', '-', '''', '"', '(', ')',
    '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%'];

  function hexdigit(const x: SOChar): byte;
  begin
    if x <= '9' then
      Result := byte(x) - byte('0') else
      Result := (byte(x) and 7) + 9;
  end;

  procedure putchildrenstr;
  var
    anobject: ISuperObject;
  begin
    anobject := FStack^.obj.AsObject[xmlchildren];
    if anobject = nil then
    begin
      anobject := TSuperObject.Create(stArray);
      FStack^.obj.AsObject[xmlchildren] := anobject;
    end;
    anobject.AsArray.Add(TSuperObject.Create(FValue.Data));
  end;

  procedure AddProperty(const parent, value: ISuperObject; const name: SOString);
  var
    anobject: ISuperObject;
    arr: ISuperObject;
  begin
    anobject := parent.AsObject[name];
    if anobject = nil then
      parent.AsObject[name] := value else
      begin
        if (anobject.DataType = stArray) then
          anobject.AsArray.Add(value) else
          begin
            arr := TSuperObject.Create(stArray);
            arr.AsArray.Add(anobject);
            arr.AsArray.Add(value);
            parent.AsObject[name] := arr;
          end;
      end;
  end;

  procedure packend;
  var
    anobject, anobject2: ISuperObject;
    n: Integer;
  begin
    anobject := FStack^.obj.AsObject[xmlchildren];
    if (anobject <> nil) and (anobject.AsArray.Length = 1) and (anobject.AsArray[0].DataType = stString) then
    begin
      if FStack^.obj.AsObject.count = 2 then // name + children
      begin
        if FStack^.prev <> nil then
          AddProperty(FStack^.prev^.obj, anobject.AsArray[0], FStack^.obj.AsObject.S[xmlname]) else
          begin
            AddProperty(FStack^.obj, anobject.AsArray[0], xmltext);
            FStack^.obj.AsObject.Delete(xmlchildren);
          end;
      end
      else
      begin
        AddProperty(FStack^.obj, anobject.AsArray[0], FStack^.obj.AsObject.S[xmlname]);
        FStack^.obj.AsObject.Delete(xmlchildren);
        if FStack^.prev <> nil then
          AddProperty(FStack^.prev^.obj, FStack^.obj, FStack^.obj.AsObject.S[xmlname]) else
        FStack^.obj.AsObject.Delete(xmlchildren);
        FStack^.obj.AsObject.Delete(xmlname);
      end;
    end else
    begin
      if (anobject <> nil) then
      begin
        for n := 0 to anobject.AsArray.Length - 1 do
        begin
          anobject2 := anobject.AsArray[n];
          if ObjectIsType(anobject2, stObject) then
          begin
            AddProperty(FStack^.obj, anobject2, anobject2.AsObject.S[xmlname]);
            anobject2.AsObject.Delete(xmlname);
          end else
            AddProperty(FStack^.obj, anobject2, xmltext);
        end;
        FStack^.obj.Delete(xmlchildren);
      end;
      if (FStack^.prev <> nil) and (FStack^.obj.AsObject.count > 1) then
      begin
        if (FStack^.obj.AsObject.count = 2) and (FStack^.obj.AsObject[xmltext] <> nil) then
          AddProperty(FStack^.prev^.obj, FStack^.obj.AsObject[xmltext], FStack^.obj.AsObject.S[xmlname]) else
          AddProperty(FStack^.prev^.obj, FStack^.obj, FStack^.obj.AsObject.S[xmlname]);
      end;
      FStack^.obj.Delete(xmlname);
    end;
  end;

var
  c: SOChar;
  read: Integer;
  p: PSOChar;
  anobject: ISuperObject;
label
  redo, err;
begin
  p := data;
  read := 0;
  //Result := 0;
  repeat

    if (read = len) then
    begin
      if (FStack^.prev = nil) and ((FStack^.state = xsEnd) or ((FStack^.state = xsEatSpaces) and (FStack^.savedstate = xsEnd))) then
      begin
        if FPack then
          packend;
        FError := xeSuccess;
      end else
        FError := xeContinue;
      Result := read;
      exit;
    end;
    c := p^;
  redo:
    case FStack^.state of

      xsEatSpaces:
        if {$IFDEF UNICODE}(c < #256) and {$ENDIF} (AnsiChar(c) in spaces) then {nop} else
        begin
          FStack^.state := FStack^.savedstate;
          goto redo;
        end;

      xsStart:
          case c of
            '<': FStack^.state := xsElement;
          else
            goto err;
          end;
      xsElement:
        begin
          case c of
            '?':
              begin
                FStack^.savedstate := xsStart;
                FStack^.state := xsEatSpaces;
                StackUp;
                FStr.Reset;
                FStack^.state := xsElementPI;
                FStack^.clazz := xcProcessInst;
              end;
            '!':
              begin
                FPosition := 0;
                FStack^.state := xsElementComment;
                FStack^.clazz := xcComment;
              end;
          else
            if ((c < #256) and (AnsiChar(c) in alphas)) or (c >= #256) then
            begin
              FStr.Reset;
              FStack^.state := xsElementName;
              FStack^.clazz := xcElement;
              goto redo;
            end else
              goto err;
          end;
        end;
      xsElementPI:
        begin
          if ((c < #256) and (AnsiChar(c) in alphanums)) or (c >= #256) then
            FStr.Append(@c, 1) else
            begin
              FStack^.obj := TSuperObject.Create(stObject);
              FStack^.obj.AsObject.S[xmlname] := FStr.Data;
              FStack^.state := xsEatSpaces;
              if FStr.Data = 'xml' then
                FStack^.savedstate := xsAttributes else
                begin
                  FValue.Reset;
                  FStack^.savedstate := xsElementDataPI;
                end;
              goto redo;
            end;
        end;
      xsElementDataPI:
        begin
          case c of
            '?':
              begin
                FStack^.obj.AsObject.S['data'] := FValue.Data;
                FStack^.state := xsCloseElementPI;
              end;
          else
            FValue.Append(@c, 1);
          end;
        end;
      xsCloseElementPI:
        begin
          if (c <> '>') then goto err;
          PI := FStack^.obj;
          StackDown;
          PIParent := FStack^.obj;
          FError := xeProcessInst;
          Result := read + 1;
          Exit;
        end;
      xsElementName:
        begin
          if ((c < #256) and (AnsiChar(c) in alphanums)) or (c >= #256) then
            FStr.Append(@c, 1) else
            begin
              FStack^.obj := TSuperObject.Create(stObject);
              FStack^.obj.AsObject.S[xmlname] := FStr.Data;
              FStack^.state := xsEatSpaces;
              FStack^.savedstate := xsAttributes;
              goto redo;
            end;
        end;
      xsChildren:
        begin
          case c of
            '<': FStack^.state := xsTryCloseElement;
          else
            FValue.Reset;
            FStack^.state := xsElementString;
            FStack^.clazz := xcString;
            goto redo;
          end;
        end;
      xsCloseEmptyElement:
        begin
          case c of
            '>':
              begin
                FStack^.state := xsEatSpaces;
                FStack^.savedstate := xsEnd;
              end
          else
            goto err;
          end;
        end;
      xsTryCloseElement:
        begin
          case c of
            '/': begin
                   FStack^.state := xsCloseElementName;
                   FPosition := 0;
                   FStr.Reset;
                   FStr.Append(PSoChar(FStack^.obj.AsObject.S[xmlname]));
                 end;
            '!': begin
                   FPosition := 0;
                   FStack^.state := xsElementComment;
                   FStack^.clazz := xcComment;
                 end;
            '?': begin
                   FStack^.savedstate := xsChildren;
                   FStack^.state := xsEatSpaces;
                   StackUp;
                   FStr.Reset;
                   FStack^.state := xsElementPI;
                   FStack^.clazz := xcProcessInst;
                 end
          else
            FStack^.state := xsChildren;
            StackUp;
            if ((c < #256) and (AnsiChar(c) in alphas)) or (c >= #256) then
            begin
              FStr.Reset;
              FStack^.state := xsElementName;
              FStack^.clazz := xcElement;
              goto redo;
            end else
              goto err;
          end;
        end;
      xsCloseElementName:
        begin
          if FStr.Position = FPosition then
          begin
            FStack^.savedstate := xsCloseEmptyElement;
            FStack^.state := xsEatSpaces;
            goto redo;
          end else
          begin
            if (c <> FStr.Data[FPosition]) then goto err;
            inc(FPosition);
          end;
        end;
      xsAttributes:
        begin
          case c of
            '?': begin
                  if FStack^.clazz <> xcProcessInst then goto err;
                  FStack^.state := xsCloseElementPI;
                 end;
            '/': begin
                   FStack^.state := xsCloseEmptyElement;
                 end;
            '>': begin
                   FStack^.state := xsEatSpaces;
                   FStack^.savedstate := xsChildren;
                 end
          else
            if ((c < #256) and (AnsiChar(c) in alphas)) or (c >= #256) then
            begin
              FStr.Reset;
              FStr.Append(@c, 1);
              FStack^.state := xsAttributeName;
            end else
              goto err;
          end;
        end;
      xsAttributeName:
        begin
          if ((c < #256) and (AnsiChar(c) in alphanums)) or (c >= #256) then
            FStr.Append(@c, 1) else
          begin
            // no duplicate attribute
            if FPack then
            begin
              if FStack^.obj.AsObject[FStr.Data] <> nil then
                goto err;
            end else
            begin
              anobject := FStack^.obj.AsObject[xmlattributes];
              if (anobject <> nil) and (anobject.AsObject[FStr.Data] <> nil) then
                goto err;
            end;
            FStack^.state := xsEatSpaces;
            FStack^.savedstate := xsEqual;
            goto redo;
          end;
        end;
      xsEqual:
        begin
          if c <> '=' then goto err;
          FStack^.state := xsEatSpaces;
          FStack^.savedstate := xsAttributeValue;
          FValue.Reset;
          FPosition := 0;
          FAChar := #0;
        end;
      xsAttributeValue:
        begin
          if FAChar <> #0 then
          begin
            if (c = FAChar) then
              begin
                if FPack then
                begin
                  FStack^.obj.AsObject[FStr.Data] := TSuperObject.Create(Fvalue.Data);
                end else
                begin
                  anobject := FStack^.obj.AsObject[xmlattributes];
                  if anobject = nil then
                  begin
                    anobject := TSuperObject.Create(stObject);
                    FStack^.obj.AsObject[xmlattributes] := anobject;
                  end;
                  anobject.AsObject[FStr.Data] := TSuperObject.Create(Fvalue.Data);
                end;
                FStack^.savedstate := xsAttributes;
                FStack^.state := xsEatSpaces;
              end else
            case c of
              '&':
                begin
                  FStack^.state := xsEscape;
                  FStack^.savedstate := xsAttributeValue;
                end;
              #13, #10:
                begin
                  FValue.TrimRight;
                  FValue.Append(XML_SPACE, 1);
                  FStack^.state := xsEatSpaces;
                  FStack^.savedstate := xsAttributeValue;
                end;
            else
              FValue.Append(@c, 1);
            end;

          end else
          begin
            if (c < #256) and (AnsiChar(c) in ['"', '''']) then
            begin
              FAChar := c;
              inc(FPosition);

            end else
              goto err;
          end;
        end;
      xsElementString:
        begin
          case c of
            '<': begin
                   FValue.TrimRight;
                   putchildrenstr;
                   FStack^.state := xsTryCloseElement;
                 end;
            #13, #10:
              begin
                FValue.TrimRight;
                FValue.Append(XML_SPACE, 1);
                FStack^.state := xsEatSpaces;
                FStack^.savedstate := xsElementString;
              end;
            '&':
              begin
                FStack^.state := xsEscape;
                FStack^.savedstate := xsElementString;
              end
          else
            FValue.Append(@c, 1);
          end;
        end;
      xsElementComment:
        begin
          case FPosition of
            0:
              begin
                case c of
                  '-': Inc(FPosition);
                  '[':
                    begin
                     FValue.Reset;
                     FPosition := 0;
                     FStack^.state := xsElementCDATA;
                     FStack^.clazz := xcCdata;
                    end;
                  'D':
                   begin
                     if (FStack^.prev = nil) and (FDocType = nil) then
                     begin
                       FStack^.state := xsElementDocType;
                       FPosition := 0;
                       FStack^.clazz := xcDocType;
                     end else
                       goto err;
                   end;
                else
                  goto err;
                end;
              end;
            1:
              begin
                if c <> '-' then goto err;
                Inc(FPosition);
              end;
          else
            if c = '-' then
            begin
              FPosition := 0;
              FStack^.state := xsCloseElementComment;
            end;
          end;
        end;
      xsCloseElementComment:
        begin
          case FPosition of
          0: begin
               if c <> '-' then
               begin
                 FPosition := 2;
                 FStack^.state := xsElementComment;
               end else
                 Inc(FPosition);
             end;
          1: begin
               if c <> '>' then goto err;
               FStack^.state := xsEatSpaces;
               if FStack^.obj <> nil then
                  FStack^.savedstate := xsChildren else
                  FStack^.savedstate := xsStart;
             end;
          end;
        end;
      xsElementCDATA:
        begin
          case FPosition of
            0: if (c = 'C') then inc(FPosition) else goto err;
            1: if (c = 'D') then inc(FPosition) else goto err;
            2: if (c = 'A') then inc(FPosition) else goto err;
            3: if (c = 'T') then inc(FPosition) else goto err;
            4: if (c = 'A') then inc(FPosition) else goto err;
            5: if (c = '[') then inc(FPosition) else goto err;
          else
            case c of
              ']': begin
                     FPosition := 0;
                     FStack^.state := xsClodeElementCDATA;
                   end;
            else
              FValue.Append(@c, 1);
            end;
          end;
        end;
      xsClodeElementCDATA:
        begin
          case FPosition of
            0: if (c = ']') then
                 inc(FPosition) else
                 begin
                   FValue.Append(XML_ARR, 1);
                   FValue.Append(@c, 1);
                   FPosition := 6;
                   FStack^.state := xsElementCDATA;
                 end;
            1: case c of
               '>':
                 begin
                   putchildrenstr;
                   FStack^.state := xsEatSpaces;
                   FStack^.savedstate := xsChildren;
                 end;
               ']':
                 begin
                   FValue.Append(@c, 1);
                 end;
            else
              FValue.Append(@c, 1);
              FStack^.state := xsElementCDATA;
            end;
          end;
        end;
      xsElementDocType:
        begin
          case FPosition of
            0: if (c = 'O') then inc(FPosition) else goto err;
            1: if (c = 'C') then inc(FPosition) else goto err;
            2: if (c = 'T') then inc(FPosition) else goto err;
            3: if (c = 'Y') then inc(FPosition) else goto err;
            4: if (c = 'P') then inc(FPosition) else goto err;
            5: if (c = 'E') then inc(FPosition) else goto err;
          else
            if (c < #256) and (AnsiChar(c) in spaces) then
            begin
              FStack^.state := xsEatSpaces;
              FStack^.savedstate := xsElementDocTypeName;
              FStr.Reset;
            end else
              goto err;
          end;
        end;
      xsElementDocTypeName:
        begin
          case FStr.Position of
            0: begin
                 case c of
                   '>':
                     begin
                       FStack^.state := xsEatSpaces;
                       FStack^.state := xsStart;
                       FStack^.clazz := xcNone;
                     end
                 else
                   if ((c < #256) and (AnsiChar(c) in alphas)) or (c > #256) then
                     FStr.Append(@c, 1) else
                     goto err;
                 end;
               end;
          else
            if ((c < #256) and (AnsiChar(c) in alphanums)) or (c > #256) then
              FStr.Append(@c, 1) else
              if (c < #256) and (AnsiChar(c) in spaces) then
              begin
                FDocType := TSuperObject.Create(stObject);
                FDocType.AsObject.S[xmlname] := FStr.Data;
                FStack^.state := xsEatSpaces;
                FStack^.savedstate := xsElementDocTypeExternId;
              end else
                goto err;
          end;
        end;
      xsElementDocTypeExternId:
        begin
          case c of
            'P':
              begin
                FPosition := 0;
                FStack^.state := xsElementDocTypeExternIdPublic;
              end;
            'S':
              begin
                FPosition := 0;
                FStack^.state := xsElementDocTypeExternIdSystem;
              end;
            '[':
              begin
                FStack^.savedstate := xsElementDocTypeIntSubset;
                FStack^.state := xsEatSpaces;
              end;
             '>':
               begin
                 FStack^.savedstate := xsStart;
                 FStack^.state := xsEatSpaces
               end
          else
            goto err;
          end;
        end;
      xsElementDocTypeExternIdPublic:
        begin
          case FPosition of
            0: if (c = 'U') then inc(FPosition) else goto err;
            1: if (c = 'B') then inc(FPosition) else goto err;
            2: if (c = 'L') then inc(FPosition) else goto err;
            3: if (c = 'I') then inc(FPosition) else goto err;
            4: if (c = 'C') then inc(FPosition) else goto err;
          else
            if (c < #256) and (AnsiChar(c) in spaces) then
            begin
              FStr.Reset;
              FPosition := 0;
              FStack^.savedstate := xsElementDocTypePubIdLiteral;
              FStack^.state := xsEatSpaces;
            end else
              goto err;
          end;
        end;

      xsElementDocTypeExternIdSystem:
        begin
          case FPosition of
            0: if (c = 'Y') then inc(FPosition) else goto err;
            1: if (c = 'S') then inc(FPosition) else goto err;
            2: if (c = 'T') then inc(FPosition) else goto err;
            3: if (c = 'E') then inc(FPosition) else goto err;
            4: if (c = 'M') then inc(FPosition) else goto err;
          else
            if (c < #256) and (AnsiChar(c) in spaces) then
            begin
              FStr.Reset;
              FPosition := 0;
              FStack^.savedstate := xsElementDocTypeSystemLiteral;
              FStack^.state := xsEatSpaces;
            end else
              goto err;
          end;
        end;
      xsElementDocTypePubIdLiteral:
        begin
          if FPosition = 0 then
            case c of
              '"', '''':
                begin
                  FAChar := c;
                  FPosition := 1;
                end
            else
              goto err;
            end else
            if c = FAChar then
            begin
              FDocType.AsObject.S[dtdPubidLiteral] := FStr.Data;
              FStr.Reset;
              FPosition := 0;
              FStack^.state := xsEatSpaces;
              FStack^.savedstate := xsElementDocTypeSystemLiteral;
            end else
              if (c < #256) and (AnsiChar(c) in publitteral) then
                FStr.Append(@c, 1);
        end;
      xsElementDocTypeSystemLiteral:
        begin
          if FPosition = 0 then
            case c of
              '"', '''':
                begin
                  FAChar := c;
                  FPosition := 1;
                end
            else
              goto err;
            end else
            if c = FAChar then
            begin
              FDocType.AsObject.S[dtdSystemLiteral] := FStr.Data;
              FStack^.state := xsEatSpaces;
              FStack^.savedstate := xsElementDocTypeTryIntSubset;
            end else
              FStr.Append(@c, 1);
        end;

      xsElementDocTypeTryIntSubset:
        begin
          case c of
          '>':
            begin
              FStack^.state := xsEatSpaces;
              FStack^.savedstate := xsStart;
              FStack^.clazz := xcNone;
            end;
          '[':
            begin
              FStack^.state := xsEatSpaces;
              FStack^.savedstate := xsElementDocTypeIntSubset;
            end;
          end;
        end;
      xsElementDocTypeIntSubset:
        begin
          case c of
            ']':
              begin
                FStack^.state := xsEatSpaces;
                FStack^.savedstate := xsElementDocTypeTryClose;
              end;
          end;
        end;
      xsElementDocTypeTryClose:
        begin
          if c = '>' then
          begin
            FStack^.state := xsEatSpaces;
            FStack^.savedstate := xsStart;
            FStack^.clazz := xcNone;
          end else
            goto err;
        end;
      xsEscape:
        begin
          FPosition := 0;
          case c of
            'l': FStack^.state := xsEscape_lt;
            'g': FStack^.state := xsEscape_gt;
            'a': FStack^.state := xsEscape_amp;
            'q': FStack^.state := xsEscape_quot;
            '#': FStack^.state := xsEscape_char;
          else
            goto err;
          end;
        end;
      xsEscape_lt:
        begin
          case FPosition of
            0: begin
                 if c <> 't' then goto err;
                 Inc(FPosition);
               end;
            1: begin
                 if c <> ';' then goto err;
                 FValue.Append(XML_LOW, 1);
                 FStack^.state := FStack^.savedstate;
               end;
          end;
        end;
      xsEscape_gt:
        begin
          case FPosition of
            0: begin
                 if c <> 't' then goto err;
                 Inc(FPosition);
               end;
            1: begin
                 if c <> ';' then goto err;
                 FValue.Append(XML_BIG, 1);
                 FStack^.state := FStack^.savedstate;
               end;
          end;
        end;
      xsEscape_amp:
        begin
          case FPosition of
            0: begin
                 case c of
                   'm': Inc(FPosition);
                   'p': begin
                          FStack^.state := xsEscape_apos;
                          Inc(FPosition);
                        end;
                 else
                   goto err;
                 end;
               end;
            1: begin
                 if c <> 'p' then goto err;
                 Inc(FPosition);
               end;
            2: begin
                 if c <> ';' then goto err;
                 FValue.Append(XML_AMP, 1);
                 FStack^.state := FStack^.savedstate;
               end;
          end;
        end;
      xsEscape_apos:
        begin
          case FPosition of
            0: begin
                 case c of
                   'p': Inc(FPosition);
                   'm': begin
                          FStack^.state := xsEscape_amp;
                          Inc(FPosition);
                        end;
                 else
                   goto err;
                 end;
               end;
            1: begin
                 if c <> 'o' then goto err;
                 Inc(FPosition);
               end;
            2: begin
                 if c <> 's' then goto err;
                 Inc(FPosition);
               end;
            3: begin
                 if c <> ';' then goto err;
                 FValue.Append(XML_SQU, 1);
                 FStack^.state := FStack^.savedstate;
               end;
          end;
        end;
      xsEscape_quot:
        begin
          case FPosition of
            0: begin
                 if c <> 'u' then goto err;
                 Inc(FPosition);
               end;
            1: begin
                 if c <> 'o' then goto err;
                 Inc(FPosition);
               end;
            2: begin
                 if c <> 't' then goto err;
                 Inc(FPosition);
               end;
            3: begin
                 if c <> ';' then goto err;
                 FValue.Append(XML_DQU, 1);
                 FStack^.state := FStack^.savedstate;
               end;
          end;
        end;
      xsEscape_char:
        begin
          if (SOIChar(c) >= 256) then goto err;
          case AnsiChar(c) of
            '0'..'9':
              begin
                FPosition := SOIChar(c) - 48;
                FStack^.state := xsEscape_char_num;
              end;
            'x':
              begin
                FStack^.state := xsEscape_char_hex;
              end
          else
            goto err;
          end;
        end;
      xsEscape_char_num:
        begin
          if (SOIChar(c) >= 256) then goto err;
          case AnsiChar(c) of
            '0'..'9':FPosition := (FPosition * 10) + (SOIChar(c) - 48);
            ';': begin
                   FValue.Append(@FPosition, 1);
                   FStack^.state := FStack^.savedstate;
                 end;
          else
            goto err;
          end;
        end;
      xsEscape_char_hex:
        begin
          if (c >= #256) then goto err;
          if (AnsiChar(c) in hex) then
          begin
            FPosition := (FPosition * 16) + SOIChar(hexdigit(c));
          end else
          if c = ';' then
          begin
            FValue.Append(@FPosition, 1);
            FStack^.state := FStack^.savedstate;
          end else
            goto err;
        end;
      xsEnd:
        begin
          if(FStack^.prev = nil) then Break;
          if FStack^.obj <> nil then
          begin
            if FPack then
            packend else
            begin
              anobject := FStack^.prev^.obj.AsObject[xmlchildren];
              if anobject = nil then
              begin
                anobject := TSuperObject.Create(stArray);
                FStack^.prev^.obj.AsObject[xmlchildren] := anobject;
              end;
              anobject.AsArray.Add(FStack^.obj);
            end;
          end;
          StackDown;
          goto redo;
        end;
    end;
    inc(p);
    inc(read);
  until (c = #0);

  if FStack^.state = xsEnd then
  begin
    if FPack then
      packend;
    FError := xeSuccess;
  end else
    FError := xeError;
  Result := read;
  exit;
err:
  FError := xeError;
  Result := read;
end;

function XMLParseFile(const FileName: string; pack: Boolean; onpi: TOnProcessingInstruction): ISuperObject;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FileName, fmOpenRead, fmShareDenyWrite);
  try
    Result := XMLParseStream(stream, pack, onpi);
  finally
    stream.Free;
  end;
end;

procedure TSuperXMLParser.StackDown;
var
  prev: PSuperXMLStack;
begin
  if FStack <> nil then
  begin
    prev := FStack^.prev;
    FStack^.obj := nil;
    FreeMem(FStack);
    FStack := prev;
    if FStack <> nil then
      FStack^.next := nil;
  end;
end;

procedure TSuperXMLParser.StackUp;
var
  st: PSuperXMLStack;
begin
{$IFDEF FPC}
  st := nil;
{$ENDIF}
  GetMem(st, SizeOf(st^));
  FillChar(st^, SizeOf(st^), 0);
  st^.state := xsEatSpaces;
  st^.savedstate := xsStart;
  st^.prev := FStack;
  if st^.prev <> nil then
    st^.prev^.next := st;
  st^.next := nil;
  st^.obj := nil;
  FStack := st;
end;

function utf8toucs2(src: PAnsiChar; srclen: Integer; dst: PWideChar; unused: PInteger): Integer;
var
  ch: Byte;
  ret: Word;
  min: Cardinal;
  rem, com: integer;
label
  redo;
begin
  Result := 0;
  ret := 0;
  rem := 0;
  min := 0;

  if unused <> nil then
    unused^ := 0;

  if(src = nil) or (srclen = 0) then
  begin
    dst^ := #0;
    Exit;
  end;

  while srclen > 0 do
  begin
    ch := Byte(src^);
    inc(src);
    dec(srclen);

redo:
    if (ch and $80) = 0 then
    begin
      dst^ := WideChar(ch);
      inc(Result);
    end else
    begin
        if((ch and $E0) = $C0) then
        begin
          min := $80;
          rem := 1;
          ret := ch and $1F;
        end else
        if((ch and $F0) = $E0) then
        begin
          min := $800;
          rem := 2;
          ret := ch and $0F;
        end else
          // too large utf8 bloc
          // ignore and continue
          continue;

        com := rem;
        while(rem <> 0) do
        begin
          dec(rem);
          if(srclen = 0) then
          begin
            if unused <> nil then
              unused^ := com;
            Exit;
          end;
          ch := Byte(src^);
          inc(src);
          dec(srclen);
          if((ch and $C0) = $80) then
          begin
            ret := ret shl 6;
            ret := ret or (ch and $3F);
          end else
          begin
            // unterminated utf8 bloc :/
            // try next one
            goto redo;
          end;
        end;

        if (ret >= min) then
        begin
          dst^ := WideChar(ret);
          inc(Result);
        end else
          // too small utf8 bloc
          // ignore and continue
          Continue;
    end;
    inc(dst);
  end;
end;

function XMLParseStream(stream: TStream; pack: Boolean; onpi: TOnProcessingInstruction): ISuperObject;
const
  CP_UTF8 = 65001;
var
  wbuffer: array[0..1023] of SOChar;
  abuffer: array[0..1023] of AnsiChar;
  len, read, cursor: Integer;
  PI, PIParent: ISuperObject;
  bom: array[0..2] of byte;

  encoding: TSuperXMLEncoding;
  encodingstr: string;
  cp: Integer;
  ecp: ISuperObject;

  function getbuffer: Integer;
  var
    size, unusued: Integer;
  begin

    case encoding of
{$IFNDEF UNIX}
      xnANSI:
        begin
          size := stream.Read(abuffer, sizeof(abuffer));
          result := MultiByteToWideChar(cp, 0, @abuffer, size, @wbuffer, sizeof(wbuffer));
        end;
{$ENDIF}
      xnUTF8:
        begin
          size := stream.Read(abuffer, sizeof(abuffer));
          result := utf8toucs2(@abuffer, size, @wbuffer, @unusued);
          if unusued > 0 then
            stream.Seek(-unusued, soFromCurrent);
        end;
      xnUnicode: Result := stream.Read(wbuffer, sizeof(wbuffer)) div sizeof(SOChar);
    else
      Result := 0;
    end;
  end;
label
  redo, retry;
begin
  // init knowned code pages
  ecp := so('{iso-8859-1:   28591,'+
             'iso-8859-2:   28592,'+
             'iso-8859-3:   28593,'+
             'iso-8859-4:   28594,'+
             'iso-8859-5:   28595,'+
             'iso-8859-6:   28596,'+
             'iso-8859-7:   28597,'+
             'iso-8859-8:   28598,'+
             'iso-8859-9:   28599,'+
             'iso 8859-15:  28605,'+
             'iso-2022-jp:  50220,'+
             'shift_jis:    932,'+
             'euc-jp:       20932,'+
             'ascii:        20127,'+
             'windows-1251: 1251,'+
             'windows-1252: 1252}');

  // detect bom
  stream.Seek(0, soFromBeginning);
  len := stream.Read(bom, sizeof(bom));
  if (len >= 2) and (bom[0] = $FF) and (bom[1] = $FE) then
  begin
    encoding := xnUnicode;
    stream.Seek(2, soFromBeginning);
  end else
  if (len = 3) and (bom[0] = $EF) and (bom[1] = $BB) and (bom[2] = $BF) then
  begin
    encoding := xnUTF8;
    cp := CP_UTF8;
  end else
  begin
    encoding := xnUTF8;
    cp := 0;
    stream.Seek(0, soFromBeginning);
  end;

  with TSuperXMLParser.Create(pack) do
  try
    len := getbuffer;
    while len > 0 do
    begin
retry:
      read := ParseBuffer(@wbuffer, PI, PIParent, len);
      cursor := 0;
redo:
      case FError of
        xeContinue: len := getbuffer;
        xeSuccess, xeError: Break;
        xeProcessInst:
          begin
            if (PIParent = nil) and (PI.AsObject.S[xmlname] = 'xml') then
            begin
              if pack then
                encodingstr := LowerCase(trim(PI.S['encoding'])) else
                encodingstr := LowerCase(trim(PI.S[xmlattributes + '.encoding']));
              if (encodingstr <> '') then
              case encoding of
                xnUTF8: if(cp = CP_UTF8) then
                        begin
                          if (encodingstr <> 'utf-8') then
                          begin
                            FError := xeError;
                            Break;
                          end;
                        end else
                        begin
                          cp := ecp.I[encodingstr];
                          if cp > 0 then
                          begin
{$IFNDEF UNIX}
                            encoding := xnANSI;
                            Reset;
                            stream.Seek(0, soFromBeginning);
                            len := getbuffer;
                            goto retry;
{$ELSE}
                            raise Exception.Create('charset not implemented');
{$ENDIF}
                          end;
                        end;
                xnUnicode:
                        if (encodingstr <> 'utf-16') and (encodingstr <> 'unicode') then
                        begin
                          FError := xeError;
                          Break;
                        end;
              end;
            end else
              if Assigned(onpi) then
                onpi(PI, PIParent);

            inc(cursor, read);
            if cursor >= len then
            begin
              len := getbuffer;
              continue;
            end;
            read := ParseBuffer(@wbuffer[cursor], PI, PIParent, len - cursor);
            goto redo;
          end;
      end;
    end;
    if FError = xeSuccess then
      Result := FStack^.obj else
      Result := nil;
  finally
    Free;
  end;
end;

function XMLParseString(const data: SOString; pack: Boolean; onpi: TOnProcessingInstruction): ISuperObject;
var
  PI, PIParent: ISuperObject;
  cursor, read: Integer;
label
  redo;
begin
  with TSuperXMLParser.Create(pack) do
  try
    cursor := 0;
    read := ParseBuffer(PSOChar(data), PI, PIParent);
redo:
    case FError of
      xeSuccess: Result := FStack^.obj;
      xeError: Result := nil;
      xeProcessInst:
        begin
          if Assigned(onpi) then
            onpi(PI, PIParent);
          inc(cursor, read);
          read := ParseBuffer(@data[cursor+1], PI, PIParent);
          goto redo;
        end;
    end;
  finally
    Free;
  end;
end;

end.
