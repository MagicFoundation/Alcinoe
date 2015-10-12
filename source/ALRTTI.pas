{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALRTTI
Version:      4.01

Description:  i create the TALRttiTypeEx object because i found then the
              TRTTI.getfields and/or the TRTTI.getmethods was very slow and
              even call internally some criticalsections :(

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :

Link :

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALRtti;

interface

uses Rtti,
     TypInfo,
     AlAvlBinaryTree;

Type

  {********************************}
  TALRttiPropertyEx = class(TObject)
  private
    fRttiProperty: TRttiProperty;
    fPropertyType: TRttiType;
    fAttributes: TArray<TCustomAttribute>;
    function GetName: string;
    function GetPropertyType: TRttiType;
    function GetIsReadable: Boolean;
    function GetIsWritable: Boolean;
  public
    constructor Create(const aRttiProperty: TRttiProperty); Virtual;
    property PropertyType: TRttiType read GetPropertyType;
    function GetValue(Instance: Pointer): TValue;
    procedure SetValue(Instance: Pointer; const AValue: TValue);
    property IsReadable: Boolean read GetIsReadable;
    property IsWritable: Boolean read GetIsWritable;
    property Name: string read GetName;
    function GetAttributes: TArray<TCustomAttribute>; virtual;
  end;

  {**************************************************}
  TALRttiInstancePropertyEx = class(TALRttiPropertyEx)
  private
    function GetDefault: Integer; virtual;
    function GetIndex: Integer; virtual;
    function GetNameIndex: Smallint; virtual;
    function GetPropInfo: PPropInfo; virtual;
  public
    function ToString: string; override;
    property Index: Integer read GetIndex;
    property Default: Integer read GetDefault;
    property NameIndex: Smallint read GetNameIndex;
    property PropInfo: PPropInfo read GetPropInfo;
  end;

  {********************************}
  TALRttiTypeEx = class(TObject)
  private
    fPrivateIndexedProperties: TArray<TRttiIndexedProperty>;
    fPrivateProperties: TArray<TALRttiPropertyEx>;
    fPrivateFields: TArray<TRttiField>;
    fPrivateMethods: TArray<TRttiMethod>;

    fProtectedIndexedProperties: TArray<TRttiIndexedProperty>;
    fProtectedProperties: TArray<TALRttiPropertyEx>;
    fProtectedFields: TArray<TRttiField>;
    fProtectedMethods: TArray<TRttiMethod>;

    fPublicIndexedProperties: TArray<TRttiIndexedProperty>;
    fPublicProperties: TArray<TALRttiPropertyEx>;
    fPublicFields: TArray<TRttiField>;
    fPublicMethods: TArray<TRttiMethod>;

    fPublishedIndexedProperties: TArray<TRttiIndexedProperty>;
    fPublishedProperties: TArray<TALRttiPropertyEx>;
    fPublishedFields: TArray<TRttiField>;
    fPublishedMethods: TArray<TRttiMethod>;

    fAttributes: TArray<TCustomAttribute>;
  protected
    procedure AddMethod(aRttiMethod: TRttiMethod);
    procedure AddField(aRttiField: TRttiField);
    procedure AddProperty(aRttiProperty: TRttiProperty);
    procedure AddIndexedProperty(aRttiIndexedProperty: TRttiIndexedProperty);
    procedure AddAttribute(aAttribute: TCustomAttribute);
  public
    constructor Create; Virtual;
    destructor Destroy; override;
    function GetMethods(const aVisibility: TMemberVisibility): TArray<TRttiMethod>; overload;
    function GetMethods(const AName: string; const aVisibility: TMemberVisibility): TArray<TRttiMethod>; overload;
    function GetFields(const aVisibility: TMemberVisibility): TArray<TRttiField>;
    function GetField(const AName: string; const aVisibility: TMemberVisibility): TRttiField;
    function GetProperties(const aVisibility: TMemberVisibility): TArray<TALRttiPropertyEx>;
    function GetProperty(const AName: string; const aVisibility: TMemberVisibility): TALRttiPropertyEx; overload;
    function GetProperty(const AIndex: integer; const aVisibility: TMemberVisibility): TALRttiPropertyEx; overload;
    function GetIndexedProperties(const aVisibility: TMemberVisibility): TArray<TRttiIndexedProperty>;
    function GetAttributes: TArray<TCustomAttribute>;
  End;

{************************************************************************}
function ALGetRttiTypeEx(aClassName: AnsiString): TALRttiTypeEx; overload;
function ALGetRttiTypeEx(aTypeInfo: PTypeInfo): TALRttiTypeEx; overload;
procedure ALRttiInitialization;
procedure ALRttiFinalization;

{*******************************}
var vALRTTIContext: TRttiContext;

implementation

uses sysutils,
     AlString;

{************************************************}
var vALRttiTypeEXCache: TALStringKeyAVLBinaryTree;

type

  {***********************************************************}
  TALRttiTypeExCacheNode = Class(TALStringKeyAVLBinaryTreeNode)
  public
    RttiTypeEX: TALRttiTypeEx;
    Constructor Create; Override;
    destructor Destroy; Override;
  End;

{****************************************}
constructor TALRttiTypeExCacheNode.Create;
begin
  inherited;
  RttiTypeEX := TALRttiTypeEx.Create;
end;

{****************************************}
destructor TALRttiTypeExCacheNode.Destroy;
begin
  RttiTypeEX.Free;
  inherited;
end;

{***********************************************************************}
constructor TALRttiPropertyEx.Create(const aRttiProperty: TRttiProperty);
begin
  fRttiProperty := aRttiProperty;
  fPropertyType := aRttiProperty.PropertyType;
  fAttributes := aRttiProperty.GetAttributes;
end;

{*****************************************}
function TALRttiPropertyEx.GetName: string;
begin
  result := fRttiProperty.Name;
end;

{****************************************************}
function TALRttiPropertyEx.GetPropertyType: TRttiType;
begin
  result := fPropertyType;
end;

{************************************************}
function TALRttiPropertyEx.GetIsReadable: Boolean;
begin
  result := fRttiProperty.IsReadable;
end;

{************************************************}
function TALRttiPropertyEx.GetIsWritable: Boolean;
begin
  result := fRttiProperty.IsWritable;
end;

{*************************************************************}
function TALRttiPropertyEx.GetValue(Instance: Pointer): TValue;
begin
  result := fRttiProperty.GetValue(Instance);
end;

{****************************************************************************}
procedure TALRttiPropertyEx.SetValue(Instance: Pointer; const AValue: TValue);
begin
  fRttiProperty.SetValue(Instance, AValue);
end;

{*****************************************************************}
function TALRttiPropertyEx.GetAttributes: TArray<TCustomAttribute>;
begin
  result := fAttributes;
end;

{*****************************************************}
function TALRttiInstancePropertyEx.GetDefault: Integer;
begin
  Result := GetPropInfo^.Default;
end;

{***************************************************}
function TALRttiInstancePropertyEx.GetIndex: Integer;
begin
  Result := GetPropInfo^.Index;
end;

{********************************************************}
function TALRttiInstancePropertyEx.GetNameIndex: Smallint;
begin
  Result := GetPropInfo^.NameIndex;
end;

{********************************************************}
function TALRttiInstancePropertyEx.GetPropInfo: PPropInfo;
begin
  result := TRttiInstanceProperty(fRttiProperty).PropInfo;
end;

{**************************************************}
function TALRttiInstancePropertyEx.ToString: string;
begin
  Result := 'property ' + Name + ': ' + PropertyType.Name; // do not localize
end;

{*******************************}
constructor TALRttiTypeEx.Create;
begin
  setlength(fPrivateIndexedProperties, 0);
  setlength(fPrivateProperties, 0);
  setlength(fPrivateFields, 0);
  setlength(fPrivateMethods, 0);

  setlength(fProtectedIndexedProperties, 0);
  setlength(fProtectedProperties, 0);
  setlength(fProtectedFields, 0);
  setlength(fProtectedMethods, 0);

  setlength(fPublicIndexedProperties, 0);
  setlength(fPublicProperties, 0);
  setlength(fPublicFields, 0);
  setlength(fPublicMethods, 0);

  setlength(fPublishedIndexedProperties, 0);
  setlength(fPublishedProperties, 0);
  setlength(fPublishedFields, 0);
  setlength(fPublishedMethods, 0);

  setlength(fAttributes, 0);
end;

{*******************************}
destructor TALRttiTypeEx.Destroy;
var i: integer;
begin
  for i := Low(fPrivateProperties) to High(fPrivateProperties) do fPrivateProperties[i].Free;
  for i := Low(fProtectedProperties) to High(fProtectedProperties) do fProtectedProperties[i].Free;
  for i := Low(fPublicProperties) to High(fPublicProperties) do fPublicProperties[i].Free;
  for i := Low(fPublishedProperties) to High(fPublishedProperties) do fPublishedProperties[i].Free;
end;

{*******************************************************}
procedure TALRttiTypeEx.AddField(aRttiField: TRttiField);
begin
  case aRttiField.Visibility of
    mvPrivate:begin
                setlength(fPrivateFields, length(fPrivateFields)+1);
                fPrivateFields[high(fPrivateFields)] := aRttiField;
              end;
    mvProtected:begin
                  setlength(fPrivateFields, length(fPrivateFields)+1);
                  fPrivateFields[high(fPrivateFields)] := aRttiField;

                  setlength(fProtectedFields, length(fProtectedFields)+1);
                  fProtectedFields[high(fProtectedFields)] := aRttiField;
                end;
    mvPublic:begin
               setlength(fPrivateFields, length(fPrivateFields)+1);
               fPrivateFields[high(fPrivateFields)] := aRttiField;

               setlength(fProtectedFields, length(fProtectedFields)+1);
               fProtectedFields[high(fProtectedFields)] := aRttiField;

               setlength(fPublicFields, length(fPublicFields)+1);
               fPublicFields[high(fPublicFields)] := aRttiField;
             end;
    mvPublished:begin
                  setlength(fPrivateFields, length(fPrivateFields)+1);
                  fPrivateFields[high(fPrivateFields)] := aRttiField;

                  setlength(fProtectedFields, length(fProtectedFields)+1);
                  fProtectedFields[high(fProtectedFields)] := aRttiField;

                  setlength(fPublicFields, length(fPublicFields)+1);
                  fPublicFields[high(fPublicFields)] := aRttiField;

                  setlength(fPublishedFields, length(fPublishedFields)+1);
                  fPublishedFields[high(fPublishedFields)] := aRttiField;
                end;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{*************************************************************************************}
procedure TALRttiTypeEx.AddIndexedProperty(aRttiIndexedProperty: TRttiIndexedProperty);
begin
  case aRttiIndexedProperty.Visibility of
    mvPrivate:begin
                setlength(fPrivateIndexedProperties, length(fPrivateIndexedProperties)+1);
                fPrivateIndexedProperties[high(fPrivateIndexedProperties)] := aRttiIndexedProperty;
              end;
    mvProtected:begin
                  setlength(fPrivateIndexedProperties, length(fPrivateIndexedProperties)+1);
                  fPrivateIndexedProperties[high(fPrivateIndexedProperties)] := aRttiIndexedProperty;

                  setlength(fProtectedIndexedProperties, length(fProtectedIndexedProperties)+1);
                  fProtectedIndexedProperties[high(fProtectedIndexedProperties)] := aRttiIndexedProperty;
                end;
    mvPublic:begin
               setlength(fPrivateIndexedProperties, length(fPrivateIndexedProperties)+1);
               fPrivateIndexedProperties[high(fPrivateIndexedProperties)] := aRttiIndexedProperty;

               setlength(fProtectedIndexedProperties, length(fProtectedIndexedProperties)+1);
               fProtectedIndexedProperties[high(fProtectedIndexedProperties)] := aRttiIndexedProperty;

               setlength(fPublicIndexedProperties, length(fPublicIndexedProperties)+1);
               fPublicIndexedProperties[high(fPublicIndexedProperties)] := aRttiIndexedProperty;
             end;
    mvPublished:begin
                  setlength(fPrivateIndexedProperties, length(fPrivateIndexedProperties)+1);
                  fPrivateIndexedProperties[high(fPrivateIndexedProperties)] := aRttiIndexedProperty;

                  setlength(fProtectedIndexedProperties, length(fProtectedIndexedProperties)+1);
                  fProtectedIndexedProperties[high(fProtectedIndexedProperties)] := aRttiIndexedProperty;

                  setlength(fPublicIndexedProperties, length(fPublicIndexedProperties)+1);
                  fPublicIndexedProperties[high(fPublicIndexedProperties)] := aRttiIndexedProperty;

                  setlength(fPublishedIndexedProperties, length(fPublishedIndexedProperties)+1);
                  fPublishedIndexedProperties[high(fPublishedIndexedProperties)] := aRttiIndexedProperty;
                end;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{**********************************************************}
procedure TALRttiTypeEx.AddMethod(aRttiMethod: TRttiMethod);
begin
  case aRttiMethod.Visibility of
    mvPrivate:begin
                setlength(fPrivateMethods, length(fPrivateMethods)+1);
                fPrivateMethods[high(fPrivateMethods)] := aRttiMethod;
              end;
    mvProtected:begin
                  setlength(fPrivateMethods, length(fPrivateMethods)+1);
                  fPrivateMethods[high(fPrivateMethods)] := aRttiMethod;

                  setlength(fProtectedMethods, length(fProtectedMethods)+1);
                  fProtectedMethods[high(fProtectedMethods)] := aRttiMethod;
                end;
    mvPublic:begin
               setlength(fPrivateMethods, length(fPrivateMethods)+1);
               fPrivateMethods[high(fPrivateMethods)] := aRttiMethod;

               setlength(fProtectedMethods, length(fProtectedMethods)+1);
               fProtectedMethods[high(fProtectedMethods)] := aRttiMethod;

               setlength(fPublicMethods, length(fPublicMethods)+1);
               fPublicMethods[high(fPublicMethods)] := aRttiMethod;
             end;
    mvPublished:begin
                  setlength(fPrivateMethods, length(fPrivateMethods)+1);
                  fPrivateMethods[high(fPrivateMethods)] := aRttiMethod;

                  setlength(fProtectedMethods, length(fProtectedMethods)+1);
                  fProtectedMethods[high(fProtectedMethods)] := aRttiMethod;

                  setlength(fPublicMethods, length(fPublicMethods)+1);
                  fPublicMethods[high(fPublicMethods)] := aRttiMethod;

                  setlength(fPublishedMethods, length(fPublishedMethods)+1);
                  fPublishedMethods[high(fPublishedMethods)] := aRttiMethod;
                end;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{****************************************************************}
procedure TALRttiTypeEx.AddProperty(aRttiProperty: TRttiProperty);

  function _createRttiPropertyEx: TALRttiPropertyEx;
  begin
    if aRttiProperty is TRttiInstanceProperty then result := TALRttiInstancePropertyEx.Create(aRttiProperty)
    else result := TALRttiPropertyEx.Create(aRttiProperty);
  end;

begin
  case aRttiProperty.Visibility of
    mvPrivate:begin
                setlength(fPrivateProperties, length(fPrivateProperties)+1);
                fPrivateProperties[high(fPrivateProperties)] := _createRttiPropertyEx;
              end;
    mvProtected:begin
                  setlength(fPrivateProperties, length(fPrivateProperties)+1);
                  fPrivateProperties[high(fPrivateProperties)] := _createRttiPropertyEx;

                  setlength(fProtectedProperties, length(fProtectedProperties)+1);
                  fProtectedProperties[high(fProtectedProperties)] := _createRttiPropertyEx;
                end;
    mvPublic:begin
               setlength(fPrivateProperties, length(fPrivateProperties)+1);
               fPrivateProperties[high(fPrivateProperties)] := _createRttiPropertyEx;

               setlength(fProtectedProperties, length(fProtectedProperties)+1);
               fProtectedProperties[high(fProtectedProperties)] := _createRttiPropertyEx;

               setlength(fPublicProperties, length(fPublicProperties)+1);
               fPublicProperties[high(fPublicProperties)] := _createRttiPropertyEx;
             end;
    mvPublished:begin
                  setlength(fPrivateProperties, length(fPrivateProperties)+1);
                  fPrivateProperties[high(fPrivateProperties)] := _createRttiPropertyEx;

                  setlength(fProtectedProperties, length(fProtectedProperties)+1);
                  fProtectedProperties[high(fProtectedProperties)] := _createRttiPropertyEx;

                  setlength(fPublicProperties, length(fPublicProperties)+1);
                  fPublicProperties[high(fPublicProperties)] := _createRttiPropertyEx;

                  setlength(fPublishedProperties, length(fPublishedProperties)+1);
                  fPublishedProperties[high(fPublishedProperties)] := _createRttiPropertyEx;
                end;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{*****************************************************************}
procedure TALRttiTypeEx.AddAttribute(aAttribute: TCustomAttribute);
begin
  setlength(fAttributes, length(fAttributes)+1);
  fAttributes[high(fAttributes)] := aAttribute;
end;

{*****************************************************************************************}
function TALRttiTypeEx.GetFields(const aVisibility: TMemberVisibility): TArray<TRttiField>;
begin
  case aVisibility of
    mvPrivate:result := fPrivateFields;
    mvProtected:result := fProtectedFields;
    mvPublic:result := fPublicFields;
    mvPublished:result := fPublishedFields;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{*****************************************************************************************************}
function TALRttiTypeEx.GetField(const AName: string; const aVisibility: TMemberVisibility): TRttiField;
begin
  for Result in GetFields(aVisibility) do
    if SameText(Result.Name, AName) then
      Exit;
  Result := nil;
end;

{**************************************************************************************************************}
function TALRttiTypeEx.GetIndexedProperties(const aVisibility: TMemberVisibility): TArray<TRttiIndexedProperty>;
begin
  case aVisibility of
    mvPrivate:result := fPrivateIndexedProperties;
    mvProtected:result := fProtectedIndexedProperties;
    mvPublic:result := fPublicIndexedProperties;
    mvPublished:result := fPublishedIndexedProperties;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{*******************************************************************************************}
function TALRttiTypeEx.GetMethods(const aVisibility: TMemberVisibility): TArray<TRttiMethod>;
begin
  case aVisibility of
    mvPrivate:result := fPrivateMethods;
    mvProtected:result := fProtectedMethods;
    mvPublic:result := fPublicMethods;
    mvPublished:result := fPublishedMethods;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{****************************************************************************************************************}
function TALRttiTypeEx.GetMethods(const AName: string; const aVisibility: TMemberVisibility): TArray<TRttiMethod>;
var ms: TArray<TRttiMethod>;
    m: TRttiMethod;
    len: Integer;
begin
  ms := GetMethods(aVisibility);
  len := 0;
  for m in ms do
    if SameText(m.Name, AName) then
      Inc(len);
  if len = 0 then
    Exit(nil);
  SetLength(Result, len);
  len := 0;
  for m in ms do
    if SameText(m.Name, AName) then
    begin
      Result[len] := m;
      Inc(len);
    end;
end;

{****************************************************************************************************}
function TALRttiTypeEx.GetProperties(const aVisibility: TMemberVisibility): TArray<TALRttiPropertyEx>;
begin
  case aVisibility of
    mvPrivate:result := fPrivateProperties;
    mvProtected:result := fProtectedProperties;
    mvPublic:result := fPublicProperties;
    mvPublished:result := fPublishedProperties;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{***************************************************************************************************************}
function TALRttiTypeEx.GetProperty(const AName: string; const aVisibility: TMemberVisibility): TALRttiPropertyEx;
begin
  for Result in GetProperties(aVisibility) do
    if SameText(Result.Name, AName) then
      Exit;
  Result := nil;
end;

{*****************************************************************************************************************}
function TALRttiTypeEx.GetProperty(const AIndex: integer; const aVisibility: TMemberVisibility): TALRttiPropertyEx;
begin
  for Result in GetProperties(aVisibility) do
    if TRttiInstanceProperty(Result).Index = AIndex then
      Exit;
  Result := nil;
end;

{*************************************************************}
function TALRttiTypeEx.GetAttributes: TArray<TCustomAttribute>;
begin
  result := fAttributes;
end;

{**************************************************************}
function ALGetRttiTypeEx(aClassName: AnsiString): TALRttiTypeEx;
var aRttiTypeEXCacheNode: Tobject;
begin
  aRttiTypeEXCacheNode := vALRttiTypeEXCache.FindNode(aClassName);
  if not assigned(aRttiTypeEXCacheNode) then raise EALException.Create('Cannot obtain RTTI informations about the class ' + aClassName)
  else result := TALRttiTypeExCacheNode(aRttiTypeEXCacheNode).RttiTypeEX;
end;

{************************************************************}
function ALGetRttiTypeEx(aTypeInfo: PTypeInfo): TALRttiTypeEx;
begin
  result := ALGetRttiTypeEx(aTypeInfo.Name);
end;

{*****************************}
procedure ALRttiInitialization;
var aRttiTypes: TArray<TRttiType>;
    aRttiProperty: TRttiProperty;
    aRttiIndexedProperty: TRttiIndexedProperty;
    aRttiField: TRttiField;
    aRttiMethod: TRttiMethod;
    aAttribute: TCustomAttribute;
    aRttiTypeEXCacheNode: TALRttiTypeExCacheNode;
    i: integer;
begin

  //create vALRTTIContext
  vALRTTIContext := TRttiContext.Create;

  //create vALRttiTypeEXCache
  vALRttiTypeEXCache := TALStringKeyAVLBinaryTree.Create;

  //init vALRttiTypeEXCache
  aRTTITypes := vALRTTIContext.GetTypes;
  for I := Low(aRttiTypes) to High(aRttiTypes) do begin
    aRttiTypeEXCacheNode := TALRttiTypeExCacheNode.Create;
    try

      aRttiTypeEXCacheNode.ID := aRttiTypes[i].Handle.Name;

      for aRttiProperty in aRttiTypes[i].GetProperties do begin
        aRttiProperty.GetAttributes; // to create the customAttribute object
        aRttiTypeEXCacheNode.RttiTypeEX.AddProperty(aRttiProperty);
      end;

      for aRttiIndexedProperty in aRttiTypes[i].GetIndexedProperties do begin
        aRttiIndexedProperty.GetAttributes; // to create the customAttribute object
        aRttiTypeEXCacheNode.RttiTypeEX.AddIndexedProperty(aRttiIndexedProperty);
      end;

      for aRttiField in aRttiTypes[i].GetFields do begin
        aRttiField.GetAttributes; // to create the customAttribute object
        aRttiTypeEXCacheNode.RttiTypeEX.AddField(aRttiField);
      end;

      for aRttiMethod in aRttiTypes[i].GetMethods do begin
        aRttiMethod.GetAttributes; // to create the customAttribute object
        aRttiTypeEXCacheNode.RttiTypeEX.AddMethod(aRttiMethod);
      end;

      for aAttribute in aRttiTypes[i].GetAttributes do
        aRttiTypeEXCacheNode.RttiTypeEX.AddAttribute(aAttribute);

      if not vALRttiTypeEXCache.AddNode(aRttiTypeEXCacheNode) then
        raise Exception.Create('Duplicate name ('+string(aRttiTypeEXCacheNode.ID)+')');

    except
      aRttiTypeEXCacheNode.Free;
      raise;
    end;
  end;

end;

{***************************}
procedure ALRttiFinalization;
Begin
  vALRTTIContext.Free;
  vALRttiTypeEXCache.Free;
End;

end.