{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsJSONPath;

{$I dws.inc}

interface

uses
   SysUtils,
   dwsJSON, dwsUtils, dwsXPlatform, dwsXXHash, dwsStringIterator;

type

   TdwsJSONPathContext = class
      Value : TdwsJSONValue;
      Result : TdwsJSONValueList;
   end;

   TdwsJSONPathOperator = class
      private
         FChild : TdwsJSONPathOperator;

      public
         constructor Create(iter : TStringIterator); overload; virtual;
         destructor Destroy; override;

         procedure Apply(value : TdwsJSONValue; result : TdwsJSONValueList); virtual; abstract;

         property Child : TdwsJSONPathOperator read FChild write FChild;
   end;

   TdwsJSONPathOperatorSelect = class(TdwsJSONPathOperator)
      public
         procedure Apply(value : TdwsJSONValue; result : TdwsJSONValueList); override;
   end;

   TdwsJSONPathOperatorAllProperties = class(TdwsJSONPathOperator)
      public
         procedure Apply(value : TdwsJSONValue; result : TdwsJSONValueList); override;
   end;

   TdwsJSONPathOperatorIndex = class(TdwsJSONPathOperator)
      private
         FIndex : Integer;

      public
         constructor Create(iter : TStringIterator); override;

         procedure Apply(value : TdwsJSONValue; result : TdwsJSONValueList); override;

         property Index : Integer read FIndex write FIndex;
   end;

   TdwsJSONPathOperatorProperty = class(TdwsJSONPathOperator)
      private
         FProp : UnicodeString;

      public
         constructor Create(iter : TStringIterator); override;

         procedure Apply(value : TdwsJSONValue; result : TdwsJSONValueList); override;

         property Prop : UnicodeString read FProp write FProp;
   end;

   TdwsJSONPathOperatorDeepProperty = class(TdwsJSONPathOperatorProperty)
      public
         procedure Apply(value : TdwsJSONValue; result : TdwsJSONValueList); override;
   end;

   TdwsJSONPathQuery = class
      private
         FQuery : String;
         FOperators : TdwsJSONPathOperator;

      protected
         procedure Parse;

      public
         constructor Create(const aQuery : String);
         destructor Destroy; override;

         function Apply(value : TdwsJSONValue) : TdwsJSONValueList;
   end;

   JSONPath = class

      class function Query(const aQuery : String; aJSON : TdwsJSONValue) : TdwsJSONValueList; overload; static;

   end;

   EJSONPathException = class (Exception) end;

function AcquireFromJSONCache(const json : String) : TdwsJSONValue;
procedure ReleaseToJSONCache(const json : String; jv : TdwsJSONValue);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cJSONCacheMaxSize = 256*1024;

var
   vJSONCacheLock : TMultiReadSingleWrite;
   vJSONCache : TNameObjectHash;
   vJSONCacheSize : Integer;

procedure InitializeJSONCache;
begin
   vJSONCacheLock := TMultiReadSingleWrite.Create;
   vJSONCache := TNameObjectHash.Create;
end;

procedure FinalizeJSONCache;
begin
   FreeAndNil(vJSONCacheLock);
   vJSONCache.Clean;
   FreeAndNil(vJSONCache);
end;

function AcquireFromJSONCache(const json : String) : TdwsJSONValue;
var
   h : Cardinal;
   i : Integer;
begin
   Result := nil;
   if Length(json) < cJSONCacheMaxSize div 2 then begin
      h := vJSONCache.HashName(json);
      vJSONCacheLock.BeginWrite;
      try
         i := vJSONCache.BucketHashedIndex[json, h];
         if i >= 0 then begin
            Result := vJSONCache.BucketObject[i] as TdwsJSONValue;
            if Result <> nil then begin
               vJSONCache.Objects[json] := nil;
               Dec(vJSONCacheSize, Length(json));
            end;
         end;
      finally
         vJSONCacheLock.EndWrite;
      end;
   end;
   if Result = nil then
      Result := TdwsJSONValue.ParseString(UnicodeString(json));
end;

procedure ReleaseToJSONCache(const json : String; jv : TdwsJSONValue);
var
   h : Cardinal;
   i : Integer;
begin
   if Length(json) < cJSONCacheMaxSize then begin
      h := vJSONCache.HashName(json);
      vJSONCacheLock.BeginWrite;
      try
         if vJSONCacheSize + Length(json) > cJSONCacheMaxSize then begin
            vJSONCache.Clean;
            vJSONCacheSize := 0;
         end;
         i := vJSONCache.BucketHashedIndex[json, h];
         if i < 0 then begin
            vJSONCache.AddObject(json, jv);
            Inc(vJSONCacheSize, Length(json));
            jv := nil;
         end else if vJSONCache.BucketObject[i] = nil then begin
            vJSONCache.BucketObject[i] := jv;
            Inc(vJSONCacheSize, Length(json));
            jv := nil;
         end;
      finally
         vJSONCacheLock.EndWrite;
      end;
   end;
   jv.Free;
end;

// ------------------
// ------------------ TdwsJSONPathOperator ------------------
// ------------------

// Create
//
constructor TdwsJSONPathOperator.Create(iter : TStringIterator);
begin
   // nothing by default
end;

// Destroy
//
destructor TdwsJSONPathOperator.Destroy;
begin
   FChild.Free;
end;

// ------------------
// ------------------ TdwsJSONPathOperatorSelect ------------------
// ------------------

// Apply
//
procedure TdwsJSONPathOperatorSelect.Apply(value : TdwsJSONValue; result : TdwsJSONValueList);
begin
   if value<>nil then
      result.Add(value);
end;

// ------------------
// ------------------ TdwsJSONPathOperatorAllProperties ------------------
// ------------------

// Apply
//
procedure TdwsJSONPathOperatorAllProperties.Apply(value : TdwsJSONValue; result : TdwsJSONValueList);
var
   i : Integer;
begin
   case value.ValueType of
      jvtObject, jvtArray :
         for i:=0 to value.ElementCount-1 do
            Child.Apply(value.Elements[i], result);
   end;
end;

// ------------------
// ------------------ TdwsJSONPathOperatorIndex ------------------
// ------------------

// Create
//
constructor TdwsJSONPathOperatorIndex.Create(iter : TStringIterator);
begin
   iter.SkipWhiteSpace;
   case iter.Current of
      '0'..'9', '-' :
         FIndex:=iter.CollectInteger;
   else
      raise EJSONPathException.Create('Invalid index');
   end;
end;

// Apply
//
procedure TdwsJSONPathOperatorIndex.Apply(value : TdwsJSONValue; result : TdwsJSONValueList);

   procedure ObjFallBack;
   begin
      Child.Apply(value.Items[Int32ToStrU(Index)], result);
   end;

begin
   case value.ValueType of
      jvtObject :
         ObjFallBack;
      jvtArray :
         if Index>=0 then
            Child.Apply(value.Elements[Index], result)
         else Child.Apply(value.Elements[value.ElementCount+Index], result);
   end;
end;

// ------------------
// ------------------ TdwsJSONPathOperatorProperty ------------------
// ------------------

// Create
//
constructor TdwsJSONPathOperatorProperty.Create(iter : TStringIterator);
begin
   case iter.Current of
      '"', '''' :
         FProp:=UnicodeString(iter.CollectQuotedString);
      '0'..'9', 'a'..'z', 'A'..'Z', '_' : begin
         FProp:=UnicodeString(iter.CollectAlphaNumeric);
         while iter.Current = '_' do begin
            iter.Next;
            FProp := FProp + '_' + UnicodeString(iter.CollectAlphaNumeric);
         end;
      end;
   else
      raise EJSONPathException.Create('Invalid property');
   end;
end;

// Apply
//
procedure TdwsJSONPathOperatorProperty.Apply(value : TdwsJSONValue; result : TdwsJSONValueList);
var
   i : Integer;
begin
   case value.ValueType of
      jvtObject :
         Child.Apply(value.Items[Prop], result);
      jvtArray :
         if TryStrToInt(String(Prop), i) then
            Child.Apply(value.Elements[i], result)
         else for i:=0 to value.ElementCount-1 do
            Apply(value.Elements[i], result);
   end;
end;

// ------------------
// ------------------ TdwsJSONPathOperatorDeepProperty ------------------
// ------------------

// Apply
//
procedure TdwsJSONPathOperatorDeepProperty.Apply(value : TdwsJSONValue; result : TdwsJSONValueList);
var
   i : Integer;
   sub : TdwsJSONValue;
begin
   case value.ValueType of
      jvtObject : begin
         sub:=value.Items[Prop];
         if sub=nil then begin
            for i:=0 to value.ElementCount-1 do
               Apply(value.Elements[i], result);
         end else Child.Apply(sub, result);
      end;
      jvtArray : begin
         if TryStrToInt(String(Prop), i) then begin
            sub:=value.Elements[i];
            if sub<>nil then begin
               Child.Apply(sub, result);
               Exit;
            end;
         end;
         for i:=0 to value.ElementCount-1 do
            Apply(value.Elements[i], result);
      end;
   end;
end;

// ------------------
// ------------------ TdwsJSONPathQuery ------------------
// ------------------

// Create
//
constructor TdwsJSONPathQuery.Create(const aQuery : String);
begin
   FQuery:=aQuery;
   Parse;
end;

// Destroy
//
destructor TdwsJSONPathQuery.Destroy;
begin
   FOperators.Free;
end;

// Apply
//
function TdwsJSONPathQuery.Apply(value : TdwsJSONValue) : TdwsJSONValueList;
begin
   Result:=TdwsJSONValueList.Create;
   FOperators.Apply(value, Result);
end;

// Parse
//
procedure TdwsJSONPathQuery.Parse;
var
   iter : TStringIterator;
   tail, op : TdwsJSONPathOperator;
begin
   tail:=nil;
   iter:=TStringIterator.Create(FQuery);
   try
      while not iter.EOF do begin
         case iter.Current of
            #0..' ' : begin
               iter.Next;
               continue;
            end;
            '.' : begin
               iter.Next;
               if iter.EOF then
                  raise EJSONPathException.Create('Unterminated property selector');
               case iter.Current of
                  '.' : begin
                     iter.Next;
                     if iter.EOF then
                        raise EJSONPathException.Create('Unterminated deep property selector');
                     op:=TdwsJSONPathOperatorDeepProperty.Create(iter);
                  end;
                  '*' : begin
                     iter.Next;
                     op:=TdwsJSONPathOperatorAllProperties.Create(iter);
                  end;
               else
                  op:=TdwsJSONPathOperatorProperty.Create(iter);
               end;
            end;
            '[' : begin
               iter.Next;
               iter.SkipWhiteSpace;
               case iter.Current of
                  '*' : begin
                     iter.Next;
                     iter.SkipWhiteSpace;
                     op:=TdwsJSONPathOperatorAllProperties.Create;
                  end;
                  ']' :
                     op:=TdwsJSONPathOperatorAllProperties.Create;
               else
                  op:=TdwsJSONPathOperatorIndex.Create(iter);
               end;
               iter.SkipWhiteSpace;
               if iter.EOF or (iter.Current<>']') then begin
                  op.Free;
                  raise EJSONPathException.Create('Missing "]"');
               end;
               iter.Next;
            end;
         else
            raise EJSONPathException.CreateFmt('Unsupported character "%s" in JSONPath', [iter.Current]);
         end;
         Assert(op<>nil);
         if tail=nil then
            FOperators:=op
         else tail.FChild:=op;
         tail:=op;
      end;
   finally
      iter.Free;
   end;
   if tail=nil then
      raise EJSONPathException.Create('Empty query')
   else tail.Child:=TdwsJSONPathOperatorSelect.Create;
end;

// ------------------
// ------------------ JSONPath ------------------
// ------------------

// Query (json parsed)
//
class function JSONPath.Query(const aQuery : String; aJSON : TdwsJSONValue) : TdwsJSONValueList;
var
   q : TdwsJSONPathQuery;
begin
   q:=TdwsJSONPathQuery.Create(aQuery);
   try
      Result:=q.Apply(aJSON);
   finally
      q.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   InitializeJSONCache;

finalization

   FinalizeJSONCache;

end.
