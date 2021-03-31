/// shared DDD Infrastructure: User CQRS Repository via ORM
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddInfraRepoUser;

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
  SynCrypto,
  SynTests,
  SynTable, // for TSynFilter and TSynValidate
  mORMot,
  mORMotDDD,
  dddDomUserTypes,
  dddDomUserCQRS;


{ *********** Implements User Aggregate CQRS Repository via mORMot's RESTful ORM }

type
  /// implements a User CQRS Repository via mORMot's RESTful ORM
  // - this class will use a supplied TSQLRest instance to persist TUser
  // Aggregate Roots, following the IDomUserCommand CQRS methods
  // - each TUser aggregate will be mapped into a TSQLRecordUser ORM table
  TInfraRepoUser = class(TDDDRepositoryRestCommand,IDomUserCommand,IDomUserQuery)
  public
    function SelectByLogonName(const aLogonName: RawUTF8): TCQRSResult;
    function SelectByEmailValidation(aValidationState: TDomUserEmailValidation): TCQRSResult;
    function SelectByLastName(const aName: TLastName; aStartWith: boolean): TCQRSResult;
    function SelectAll: TCQRSResult;
    function Get(out aAggregate: TUser): TCQRSResult;
    function GetAll(out aAggregates: TUserObjArray): TCQRSResult;
    function GetNext(out aAggregate: TUser): TCQRSResult;
    function Add(const aAggregate: TUser): TCQRSResult;
    function Update(const aUpdatedAggregate: TUser): TCQRSResult;
    function HowManyValidatedEmail: integer;
  end;

  /// implements a Factory of User CQRS Repositories via mORMot's RESTful ORM
  // - this class will associate the TUser Aggregate Root with a TSQLRecordUser
  // ORM table, as managed in a given TSQLRest instance
  TInfraRepoUserFactory = class(TDDDRepositoryRestFactory)
  public
    /// initialize the association with the ORM
    constructor Create(aRest: TSQLRest; aOwner: TDDDRepositoryRestManager=nil); reintroduce;
  	/// perform some tests on this Factory/Repository implementation
    class procedure RegressionTests(test: TSynTestCase);
  end;


{ *********** Person / User / Customer Persistence ORM classes }

type
  /// ORM class able to store a TPerson object
  // - the TPerson.Name property has been flattened to Name_* columns as
  // expected by TDDDRepositoryRestFactory.ComputeMapping
  TSQLRecordPerson = class(TSQLRecord)
  protected
    fFirst: RawUTF8;
    fMiddle: RawUTF8;
    fLast: RawUTF8;
    fBirthDate: TDateTime;
  published
    property Name_First: RawUTF8 read fFirst write fFirst;
    property Name_Middle: RawUTF8 read fMiddle write fMiddle;
    property Name_Last: RawUTF8 read fLast write fLast;
    property Birth: TDateTime read fBirthDate;
  end;

  /// ORM class able to store a TPersonContactable object
  // - the TPersonContactable.Address property has been flattened to Address_*
  // columns as expected by TDDDRepositoryRestFactory.ComputeMapping
  TSQLRecordPersonContactable = class(TSQLRecordPerson)
  protected
    fStreet1: RawUTF8;
    fStreet2: RawUTF8;
    fCityArea: RawUTF8;
    fCity: RawUTF8;
    fRegion: RawUTF8;
    fCode: RawUTF8;
    fCountry: integer;
    fEmail: RawUTF8;
    fPhone1: RawUTF8;
    fPhone2: RawUTF8;
  published
    property Address_Street1: RawUTF8 read fStreet1 write fStreet1;
    property Address_Street2: RawUTF8 read fStreet2 write fStreet2;
    property Address_CityArea: RawUTF8 read fCityArea write fCityArea;
    property Address_City: RawUTF8 read fCity write fCity;
    property Address_Region: RawUTF8 read fRegion write fRegion;
    property Address_Code: RawUTF8 read fCode write fCode;
    property Address_Country: integer read fCountry;
    property Phone1: RawUTF8 read fPhone1 write fPhone1;
    property Phone2: RawUTF8 read fPhone2 write fPhone2;
    property Email: RawUTF8 read fEmail write fEmail;
  end;

  /// ORM class used to persist a TUser domain aggregate
  TSQLRecordUser = class(TSQLRecordPersonContactable)
  protected
    fLogonName: RawUTF8;
    fEmailValidated: TDomUserEmailValidation;
  published
    property LogonName: RawUTF8 read fLogonName write fLogonName stored AS_UNIQUE;
    property EmailValidated: TDomUserEmailValidation read fEmailValidated write fEmailValidated;
  end;


  
implementation

{ TInfraRepoUser }

{ in practice, implementing a I*Command interface mainly consist in calling
  the various TDDDRepositoryRestCommand.ORM*() methods, which would perform
  all process on the REST instance using the TSQLRecordUser table mapped to
  the TUser aggregate root
  - purpose of this I*Command interface is to use the loosely typed
    TDDDRepositoryRestCommand.ORM*() methods to match the exact needs of
    the DDD Aggregate class
  - it would also hide the persistence details so that we would be able
    to ignore e.g. what a primary key is, and avoid the "anemic domain model"
    anti-pattern, which is basically CRUD in disguise }

function TInfraRepoUser.SelectByLogonName(
  const aLogonName: RawUTF8): TCQRSResult;
begin
  result := ORMSelectOne('LogonName=?',[aLogonName],(aLogonName=''));
end;

function TInfraRepoUser.SelectByEmailValidation(
  aValidationState: TDomUserEmailValidation): TCQRSResult;
begin
  result := ORMSelectAll('EmailValidated=?',[ord(aValidationState)]);
end;

function TInfraRepoUser.SelectByLastName(const aName: TLastName;
  aStartWith: boolean): TCQRSResult;
begin
  if aStartWith then
    result := ORMSelectAll('Name_Last LIKE ?',[aName+'%'],(aName='')) else
    result := ORMSelectAll('Name_Last=?',[aName],(aName=''));
end;

function TInfraRepoUser.SelectAll: TCQRSResult;
begin
  result := ORMSelectAll('',[]);
end;

function TInfraRepoUser.Get(out aAggregate: TUser): TCQRSResult;
begin
  result := ORMGetAggregate(aAggregate);
end;

function TInfraRepoUser.GetAll(
  out aAggregates: TUserObjArray): TCQRSResult;
begin
  result := ORMGetAllAggregates(aAggregates);
end;

function TInfraRepoUser.GetNext(out aAggregate: TUser): TCQRSResult;
begin
  result := ORMGetNextAggregate(aAggregate);
end;

function TInfraRepoUser.Add(const aAggregate: TUser): TCQRSResult;
begin
  result := ORMAdd(aAggregate);
end;

function TInfraRepoUser.Update(
  const aUpdatedAggregate: TUser): TCQRSResult;
begin
  result := ORMUpdate(aUpdatedAggregate);
end;

function TInfraRepoUser.HowManyValidatedEmail: integer;
begin
  if ORMSelectCount('EmailValidated=%',[ord(evValidated)],[],result)<>cqrsSuccess then
    result := 0;
end;


{ TInfraRepoUserFactory }

constructor TInfraRepoUserFactory.Create(aRest: TSQLRest;
  aOwner: TDDDRepositoryRestManager);
begin
  inherited Create(IDomUserCommand,TInfraRepoUser,TUser,aRest,TSQLRecordUser,aOwner);
  AddFilterOrValidate(['*'],TSynFilterTrim.Create);
  AddFilterOrValidate(['LogonName'],TSynValidateNonVoidText.Create);
end;

class procedure TInfraRepoUserFactory.RegressionTests(test: TSynTestCase);

procedure TestOne(Rest: TSQLRest);
const MAX=1000;
      MOD_EMAILVALID=ord(high(TDomUserEmailValidation))+1;
var cmd: IDomUserCommand;
    qry: IDomUserQuery;
    user: TUser;
    users: TUserObjArray;
    i,usersCount: integer;
    itext: RawUTF8;
    v: TDomUserEmailValidation;
    count: array[TDomUserEmailValidation] of integer;
    msg: string;
begin
  test.Check(Rest.Services.Resolve(IDomUserCommand,cmd));
  user := TUser.Create;
  try
    for i := 1 to MAX do begin
      UInt32ToUtf8(i,itext);
      user.LogonName := '  '+itext; // left '  ' to test TSynFilterTrim.Create
      user.EmailValidated := TDomUserEmailValidation(i mod MOD_EMAILVALID);
      user.Name.Last := 'Last'+itext;
      user.Name.First := 'First'+itext;
      user.Address.Street1 := 'Street '+itext;
      user.Address.Country.Alpha2 := 'fr';
      user.Phone1 := itext;
      test.check(cmd.Add(user)=cqrsSuccess);
    end;
    test.check(cmd.Commit=cqrsSuccess);
  finally
    user.Free;
  end; 
  user := TUser.Create;
  try
    test.Check(Rest.Services.Resolve(IDomUserQuery,qry));
    test.Check(qry.GetCount=0);
    for i := 1 to MAX do begin
      UInt32ToUtf8(i,itext);
      test.Check(qry.SelectByLogonName(itext)=cqrsSuccess);
      test.Check(qry.GetCount=1);
      test.Check(qry.Get(user)=cqrsSuccess);
      test.Check(qry.GetCount=1);
      test.Check(user.LogonName=itext);
      test.Check(user.EmailValidated=TDomUserEmailValidation(i mod MOD_EMAILVALID));
      test.Check(user.Name.Last='Last'+itext);
      test.Check(user.Name.First='First'+itext);
      test.Check(user.Address.Street1='Street '+itext);
      test.Check(user.Address.Country.Alpha2='FR');
      test.Check(user.Phone1=itext);
    end;
    test.Check(Rest.Services.Resolve(IDomUserCommand,cmd));
    try
      usersCount := 0;
      for v := low(TDomUserEmailValidation) to high(TDomUserEmailValidation) do begin
        test.Check(cmd.SelectByEmailValidation(v)=cqrsSuccess);
        ObjArrayClear(users); // should be done, otherwise memory leak
        test.Check(cmd.GetAll(users)=cqrsSuccess);
        test.Check(length(users)>=MAX div MOD_EMAILVALID);
        count[v] := length(users);
        inc(usersCount,length(users));
        for i := 0 to high(users) do begin
          test.Check(users[i].EmailValidated=v);
          test.Check(users[i].LogonName=users[i].Phone1);
          test.Check(users[i].Name.First='First'+users[i].LogonName);
        end;
      end;
      test.Check(cmd.DeleteAll=cqrsSuccess,'delete all evFailed');
      test.check(cmd.Commit=cqrsSuccess);
      ObjArrayClear(users);
      test.Check(cmd.SelectAll=cqrsSuccess);
      test.Check(cmd.GetAll(users)=cqrsSuccess);
      test.Check(length(users)=usersCount-count[evFailed]);
      for i := 0 to high(users) do begin
        test.Check(users[i].LogonName=users[i].Phone1);
        test.Check(users[i].Name.First='First'+users[i].LogonName);
        test.Check(users[i].Address.Country.Iso=250);
      end;
    finally
      ObjArrayClear(users);
    end;
    test.Check(Rest.Services.Resolve(IDomUserCommand,cmd));
    for v := low(TDomUserEmailValidation) to high(TDomUserEmailValidation) do begin
      test.Check(cmd.SelectByEmailValidation(v)=cqrsSuccess);
      if v=evFailed then
        test.Check(cmd.GetCount=0) else
        test.Check(cmd.GetCount=count[v]);
      i := 0;
      while cmd.GetNext(user)=cqrsSuccess do begin
        test.Check(user.EmailValidated=v);
        test.Check(user.Name.First='First'+user.LogonName);
        test.Check(user.Address.Country.Iso=250);
        inc(i);
      end;
      test.Check(i=cmd.GetCount);
    end;
    test.Check(cmd.HowManyValidatedEmail=count[evValidated]);
    user.LogonName := '';
    test.check(cmd.Add(user)=cqrsDDDValidationFailed);
    test.check(cmd.GetLastError=cqrsDDDValidationFailed);
    msg := cmd.GetLastErrorInfo.msg;
    test.check(pos('TUser.LogonName',msg)>0,msg);
  finally
    user.Free;
  end;
end;

var RestServer: TSQLRestServerFullMemory;
    RestClient: TSQLRestClientURI;
begin
  RestServer := TSQLRestServerFullMemory.CreateWithOwnModel([TSQLRecordUser]);
  try // first try directly on server side
    RestServer.ServiceContainer.InjectResolver([TInfraRepoUserFactory.Create(RestServer)],true);
    TestOne(RestServer); // sub function will ensure that all I*Command are released
  finally
    RestServer.Free;
  end;
  RestServer := TSQLRestServerFullMemory.CreateWithOwnModel([TSQLRecordUser]);
  try // then try from a client-server process
    RestServer.ServiceContainer.InjectResolver([TInfraRepoUserFactory.Create(RestServer)],true);
    RestServer.ServiceDefine(TInfraRepoUser,[IDomUserCommand,IDomUserQuery],sicClientDriven);
    test.Check(RestServer.ExportServer);
    RestClient := TSQLRestClientURIDll.Create(TSQLModel.Create(RestServer.Model),@URIRequest);
    try
      RestClient.Model.Owner := RestClient;
      RestClient.ServiceDefine([IDomUserCommand],sicClientDriven);
      TestOne(RestServer);
      RestServer.DropDatabase;
      USEFASTMM4ALLOC := true; // for slightly faster process
      TestOne(RestClient);
    finally
      RestClient.Free;
    end;
  finally
    RestServer.Free;
  end;
end;



end.
