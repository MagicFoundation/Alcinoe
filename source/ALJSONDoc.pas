unit ALJSONDoc;

interface
{$IF CompilerVersion >= 23} {Delphi XE2}

uses ALStringList;

Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
                           aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false');

{$IFEND}
implementation
{$IF CompilerVersion >= 23} {Delphi XE2}

uses DBXJSON,
     ALFcnString;

{****************************************************}
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
                           aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false');

  procedure _add2Tstrings(const aName: AnsiString; const aJSONValue: TJSONValue);
  var aJSONArray: TJSONArray;
      aJSONObject: TJSONObject;
      i: integer;
  begin
    if aJSONValue is TJSONString then aLst.add(aName+aLst.NameValueSeparator+ansiString(aJSONValue.Value))
    else if aJSONValue is TJSONNumber then aLst.add(aName+aLst.NameValueSeparator+ansiString(aJSONValue.Value))
    else if aJSONValue is TJSONTrue then aLst.add(aName+aLst.NameValueSeparator+aTrueStr)
    else if aJSONValue is TJSONFalse then aLst.add(aName+aLst.NameValueSeparator+aFalseStr)
    else if aJSONValue is TJSONNull then aLst.add(aName+aLst.NameValueSeparator+aNullStr)
    else if aJSONValue is TJSONArray then begin
      aJSONArray := aJSONValue as TJSONArray;
      for i := 0 to aJSONArray.Size - 1 do
        _add2Tstrings(aName+'['+ALInttoStr(i)+']', aJSONArray.Get(i));
    end
    else if aJSONValue is TJSONObject then begin
      aJSONObject := aJSONValue as TJSONObject;
      for i := 0 to aJSONObject.Size - 1 do
        _add2Tstrings(aName + '.' + AnsiString(aJSONObject.Get(i).JsonString.Value), aJSONObject.Get(i).JsonValue);
    end;
  end;

var aJSONObject: TJSONObject;
    I: integer;

begin
  aJSONObject := TJSONObject.ParseJSONValue(AJsonStr) as TJSONObject;
  try

    for i := 0 to aJSONObject.Size - 1 do
      _add2Tstrings(ansiString(aJSONObject.Get(i).JsonString.Value), aJSONObject.Get(i).JsonValue);

  finally
    aJSONObject.Free;
  end;
end;

{$IFEND}
end.
