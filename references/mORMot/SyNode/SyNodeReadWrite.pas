unit SyNodeReadWrite;

interface
                     
uses
  SpiderMonkey, Classes, SynCommons;

  // TODO - replace this implemenattion by buffers whed buffers become fully compartible with nodeJS

  /// Used for create JS method for writing data to internal class buffer
  //  JS syntax: write(data: ArrayBuffer|String|Object, [encoding: "utf-8"|"ucs2"|"bin"|"base64"]'
  // inmplementation must provide TTextWriter to write data to
  function SMWrite_impl(cx: PJSContext; argc: uintN; vals: PjsvalVector; dest: TTextWriter): jsval; cdecl;
  /// Used for create JS method for reading data from internal class buffer (aStr)
  //  JS syntax: read([encoding=utf-8: "utf-8"|"ucs2"|"bin"|"base64"])
  // implementation must provide TTextWriter to write data to
  function SMRead_impl(cx: PJSContext; argc: uintN; vals: PjsvalVector; const aStr: RawByteString): jsval; cdecl;

implementation

{$ifdef MSWINDOWS}
uses
  Windows; // CP_UTF8
{$endif}

function SMRead_impl(cx: PJSContext; argc: uintN; vals: PjsvalVector; const aStr: RawByteString): jsval; cdecl;
var
  encoding: RawUTF8;
  len: Integer;
  bufObj: PJSObject;
  bufData: pointer;
  decodedStr: RawByteString;
begin
  if (argc = 0) or ((argc=1) and vals[0].isVoid) then
    encoding := 'utf-8'
  else
    encoding := LowerCase(vals[0].asJSString.ToUTF8(cx));

  if encoding = 'utf-8' then begin
    Result := cx.NewJSString(Pointer(aStr), length(aStr), CP_UTF8).ToJSVal
  end else if encoding = 'Windows-1251' then begin
    Result := cx.NewJSString(Pointer(aStr), length(aStr), 1251).ToJSVal
  end else if encoding = 'ucs2' then begin
    Result := cx.NewJSString(aStr).ToJSVal
  end else if encoding = 'bin2base64' then begin
    decodedStr := BinToBase64(aStr);
    Result := cx.NewJSString(decodedStr).ToJSVal;
  end else if (encoding = 'bin') or (encoding = 'base64') then begin
    if encoding = 'base64' then
      decodedStr := Base64ToBin(aStr)
    else
      decodedStr := aStr;
    len := length(decodedStr);
    bufObj := cx.NewArrayBuffer(len);
    bufData := bufObj.GetArrayBufferData;
    Move(pointer(decodedStr)^, bufData^, len);
    Result := bufObj.ToJSValue;
  end else
    raise ESMException.Create('Invalid encoding');
end;

function SMWrite_impl(cx: PJSContext; argc: uintN; vals: PjsvalVector; dest: TTextWriter): jsval; cdecl;
var
  encoding: RawUTF8;
  len: uint32;
  isShared: boolean;
  bufObj: PJSObject;
  bufData: Puint8Vector;
  tmp1: RawByteString;
  tmp2: SynUnicode;
begin
  if (argc < 1) or (argc>2) then
    raise ESMException.Create('Usage: write(data: ArrayBuffer|String|Object, [encoding: "utf-8"|"ucs2"|"bin"|"base64"]');

  encoding := '';
  if argc=2 then begin
    if (argc = 2) and (vals[1].isString) then
      encoding := LowerCase(vals[1].asJSString.ToUTF8(cx));
  end;

  case cx.TypeOfValue(vals[0]) of
    JSTYPE_STRING: begin
      if (encoding = '') or (encoding = 'utf-8') or (encoding = 'utf8') then // default is utf-8
        vals[0].asJSString.ToUTF8(cx, dest)
      else if (encoding = 'ucs2') then begin
        tmp2 := vals[0].asJSString.ToSynUnicode(cx);
        dest.AddNoJSONEscapeW(pointer(tmp2), length(tmp2));
      end else
        raise ESMException.Create('invalid encoding');
    end;
    JSTYPE_NUMBER:
      if vals[0].isInteger then
        dest.Add(vals[0].asInteger)
      else
        dest.AddDouble(vals[0].asDouble);
    JSTYPE_OBJECT: begin
      bufObj := vals[0].asObject;
      if bufObj=nil then begin
        Result.asBoolean := true;
        Exit;
      end;
      if bufObj.GetBufferDataAndLength(bufData, len) then begin
        if encoding = 'base64' then begin
          tmp1 := BinToBase64(PAnsiChar(bufData), len);
          bufData := Puint8Vector(tmp1);
          len := Length(tmp1);
          encoding := 'bin';
        end;
        if (encoding = '') or (encoding = 'bin') then begin // default is bin
          if len>0 then
            dest.AddNoJSONEscape(pointer(bufData), len)
        end else
          raise ESMException.Create('invalid encoding');
      end else begin
        if (encoding = '') or (encoding = 'utf-8') or (encoding = 'utf8') then // default is utf-8
          vals[0].AddJSON(cx, dest)
        else
          raise ESMException.Create('invalid encoding');
      end;
    end;
    JSTYPE_VOID: begin end; // do nothing
  else
    raise ESMException.Create('invalid parameter type for Writer');
  end;
  result.asBoolean := True;
end;

end.

