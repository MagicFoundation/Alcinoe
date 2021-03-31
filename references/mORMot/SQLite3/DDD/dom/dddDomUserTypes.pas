/// shared DDD Domains: User objects definition
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddDomUserTypes;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):

  
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  SynTests,
  mORMot,
  mORMotDDD,
  dddDomCountry;


{ *********** Address Modeling }

type
  TStreet = type RawUTF8;
  TCityArea = type RawUTF8;
  TCity = type RawUTF8;
  TRegion = type RawUTF8;
  TPostalCode = type RawUTF8;

  /// Address object
  // - we tried to follow a simple but worldwide layout - see
  // http://en.wikipedia.org/wiki/Address_%28geography%29#Address_format
  TAddress = class(TSynAutoCreateFields)
  protected
    fStreet1: TStreet;
    fStreet2: TStreet;
    fCityArea: TCityArea;
    fCity: TCity;
    fRegion: TRegion;
    fCode: TPostalCode;
    fCountry: TCountry;
  public
    function Equals(another: TAddress): boolean; reintroduce;
  published
    property Street1: TStreet read fStreet1 write fStreet1;
    property Street2: TStreet read fStreet2 write fStreet2;
    property CityArea: TCityArea read fCityArea write fCityArea;
    property City: TCity read fCity write fCity;
    property Region: TRegion read fRegion write fRegion;
    property Code: TPostalCode read fCode write fCode;
    property Country: TCountry read fCountry;
  end;

  TAddressObjArray = array of TAddress; 


{ *********** Person / User / Customer Modeling }

type
  TLastName = type RawUTF8;
  TFirstName = type RawUTF8;
  TMiddleName = type RawUTF8;
  TFullName = type RawUTF8;

  /// Person full name
  TPersonFullName = class(TSynPersistent)
  protected
    fFirst: TFirstName;
    fMiddle: TMiddleName;
    fLast: TLastName;
  public
    function Equals(another: TPersonFullName): boolean; reintroduce;
    function FullName(country: TCountryIdentifier=ccUndefined): TFullName; virtual;
  published
    property First: TFirstName read fFirst write fFirst;
    property Middle: TMiddleName read fMiddle write fMiddle;
    property Last: TLastName read fLast write fLast;
  end;

  /// Person birth date
  TPersonBirthDate = class(TSynPersistent)
  protected
    fDate: TDateTime;
  public
    function Equals(another: TPersonBirthDate): boolean; reintroduce;
    function Age: integer; overload;
    function Age(FromDate: TDateTime): integer; overload;
  published
    property Date: TDateTime read fDate write fDate;
  end;
  
  /// Person object
  TPerson = class(TSynAutoCreateFields)
  protected
    fBirthDate: TPersonBirthDate;
    fName: TPersonFullName;
  public
    function Equals(another: TPerson): boolean; reintroduce;
  published
    property Name: TPersonFullName read fName;
    property Birth: TPersonBirthDate read fBirthDate;
  end;

  TPhoneNumber = type RawUTF8;
  TEmailAddress = type RawUTF8;
  TEmailAddressDynArray = array of TEmailAddress;

  /// a Person object, with some contact information
  // - an User is a person, in the context of an application
  TPersonContactable = class(TPerson)
  protected
    fAddress: TAddress;
    fPhone1: TPhoneNumber;
    fPhone2: TPhoneNumber;
    fEmail: TEmailAddress;
  public
    function Equals(another: TPersonContactable): boolean; reintroduce;
    /// built-in simple unit tests
    class procedure RegressionTests(test: TSynTestCase);
  published
    property Address: TAddress read fAddress;
    property Phone1: TPhoneNumber read fPhone1 write fPhone1;
    property Phone2: TPhoneNumber read fPhone2 write fPhone2;
    property Email: TEmailAddress read fEmail write fEmail;
  end;

  TPersonContactableObjArray = array of TPersonContactable;


{ *********** Email Validation Modeling }

type
  /// the status of an email validation process
  TDomUserEmailValidation = (evUnknown, evValidated, evFailed);

  /// how a confirmation email is to be rendered, for email address validation
  // - this information will be available as data context, e.g. to the Mustache
  // template used for rendering of the email body
  TDomUserEmailTemplate = class(TSynPersistent)
  private
    fFileName: RawUTF8;
    fSenderEmail: RawUTF8;
    fSubject: RawUTF8;
    fApplication: RawUTF8;
    fInfo: variant;
  published
    /// the local file name of the Mustache template
    property FileName: RawUTF8 read fFileName write fFileName;
    /// the "sender" field of the validation email
    property SenderEmail: RawUTF8 read fSenderEmail write fSenderEmail;
    /// the "subject" field of the validation email
    property Subject: RawUTF8 read fSubject write fSubject;
    /// the name of the application, currently sending the confirmation
    property Application: RawUTF8 read fApplication write fApplication;
    /// any unstructured additional information, also supplied as data context
    property Info: variant read fInfo write fInfo;
  end;


{ *********** Application User Modeling, with Logon and Email Validation }

type
  TLogonName = type RawUTF8;

  /// an application level-user, whose account would be authenticated per Email
  TUser = class(TPersonContactable)
  private
    fLogonName: TLogonName;
    fEmailValidated: TDomUserEmailValidation;
  published
    /// the logon name would be the main entry point to the application
    property LogonName: TLogonName
      read fLogonName write fLogonName;
    /// will reflect the current state of email validation process for this user
    // - the validation is not handled by this class: this is just a property
    // which reflects the state of TDDDEmailValidationService/IDomUserEmailValidation 
    property EmailValidated: TDomUserEmailValidation
      read fEmailValidated write fEmailValidated;
  end;

  TUserObjArray = array of TUser;

  
implementation


{ TAddress }

function TAddress.Equals(another: TAddress): boolean;
begin
  if (self=nil) or (another=nil) then
    result := another=self else
    result := (another.Street1=Street1) and (another.Street2=Street2) and
      (another.CityArea=CityArea) and (another.City=City) and
      (another.Region=Region) and another.Country.Equals(Country);
end;


{ TPersonFullName }

function TPersonFullName.Equals(another: TPersonFullName): boolean;
begin
  if (self=nil) or (another=nil) then
    result := another=self else
    result := (First=another.First) and (Last=another.Last) and
      (Middle=another.Middle);
end;

function TPersonFullName.FullName(country: TCountryIdentifier): TFullName;
begin // see country-specific http://en.wikipedia.org/wiki/Family_name
  case country of
  ccJP,ccCN,ccTW,ccKP,ccKR,ccVN,ccHU,ccRO:
    // Eastern Order
    result := Trim(Trim(Last+' '+Middle)+' '+First);
  else
    // default Western Order
    result := Trim(Trim(First+' '+Middle)+' '+Last);
  end; 
end;


{ TPersonBirthDate }

function TPersonBirthDate.Age: integer;
begin
  result := Age(SysUtils.Date);
end;

function TPersonBirthDate.Age(FromDate: TDateTime): integer;
var YF,YD,MF,MD,DF,DD: word;
begin
  if (self=nil) or (fDate=0) then
    result := 0 else begin
    DecodeDate(FromDate,YF,MF,DF);
    DecodeDate(fDate,YD,MD,DD);
    result := YF-YD;
    if MF<MD then
      dec(result) else
      if (MF=MD) and (DF<DD) then
        dec(result);
  end;
end;

function TPersonBirthDate.Equals(another: TPersonBirthDate): boolean;
begin
  if (self=nil) or (another=nil) then
    result := another=self else
    result := Date=another.Date;
end;


{ TPerson }

function TPerson.Equals(another: TPerson): boolean;
begin
  if (self=nil) or (another=nil) then
    result := another=self else
    result := Name.Equals(another.Name) and Birth.Equals(another.Birth);
end;


{ TPersonContactable }

function TPersonContactable.Equals(another: TPersonContactable): boolean;
begin
  if (self=nil) or (another=nil) then
    result := another=self else
    result := inherited Equals(Self) and Address.Equals(another.Address) and
      (Phone1=another.Phone1) and (Phone2=another.Phone2) and (Email=another.Email);
end;

class procedure TPersonContactable.RegressionTests(test: TSynTestCase);
var p: TPersonContactable;
    json: RawUTF8;
    valid: boolean;
procedure TestP;
begin
  test.Check(p.Phone2='123456');
  test.Check(p.Name.Last='Smith');
  test.Check(p.Name.First='John');
  test.Check(p.Birth.Age(Iso8601ToDateTime('19821030'))=10);
  test.Check(p.Address.Country.Alpha3='FRA');
end;
begin
  p := TPersonContactable.Create;
  with test do
  try
    p.Phone2 := '123456';
    p.Name.Last := 'Smith';
    p.Name.First := 'John';
    p.Birth.Date := Iso8601ToDateTime('19721029');
    Check(p.Birth.Age>40);
    Check(p.Birth.Age(Iso8601ToDateTime('19821020'))=9);
    Check(p.Birth.Age(Iso8601ToDateTime('19821030'))=10);
    p.Address.Country.Alpha2 := 'FR';
    json := ObjectToJSON(p)+'*';
  finally
    p.Free;
  end;
  p := TPersonContactable.Create;
  with test do
  try
    // FileFromString(JSONReformat(json),'person.json');
    Check(ObjectLoadJSON(p,json));
    TestP;
  finally
    p.Free;
  end;
  p := TPersonContactable.Create;
  with test do
  try
    Check(JSONToObject(p,pointer(json),valid)^='*');
    Check(valid);
    TestP;
  finally
    p.Free;
  end;
end;


initialization
  {$ifndef ISDELPHI2010}
  {$ifndef HASINTERFACERTTI} // circumvent a old FPC bug
  TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TDomUserEmailValidation));
  {$endif}
  {$endif}
  TJSONSerializer.RegisterObjArrayForJSON([
    TypeInfo(TAddressObjArray),TAddress,
    TypeInfo(TPersonContactableObjArray),TPersonContactable,
    TypeInfo(TUserObjArray),TUser]);
end.
