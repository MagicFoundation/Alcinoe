/// shared DDD Infrastructure: implement an email validation service
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddInfraEmail;

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
  SynCommons,
  SynTests,
  SynCrypto,
  SynTable, // for TSynFilter and TSynValidate
  mORMot,
  mORMotDDD,
  dddDomUserTypes,
  dddDomUserInterfaces;

  
{ ****************** Email Verification Service }

type
  /// exception raised during any email process of this DDD's infrastructure
  // implementation
  EDDDEmail = class(EDDDInfraException);

  /// parameters used for the validation link of an email address
  // - may be stored as daemon/service level settings, using e.g. dddInfraSettings
  TDDDEmailRedirection = class(TSynPersistent)
  private
    fSuccessRedirectURI: RawUTF8;
    fRestServerPublicRootURI: RawUTF8;
    fValidationMethodName: RawUTF8;
  published
    /// the public URI which would be accessible from the Internet
    // - may be e.g 'http://publicserver/restroot'
    property RestServerPublicRootURI: RawUTF8
      read fRestServerPublicRootURI write fRestServerPublicRootURI;
    /// the validation method name for the URI
    // - if not set, TDDDEmailValidationService will use 'EmailValidate'
    // - clickable URI would be RestServerPublicRootURI+'/'+ValidationMethodName
    property ValidationMethodName: RawUTF8
      read fValidationMethodName write fValidationMethodName;
    /// the URI on which the browser will be redirected on validation success
    // - you can specify some '%' parameter markers, ordered as logon, email,
    // and validation IP
    // - may be e.g. 'http://publicwebsite/success&logon=%'
    property SuccessRedirectURI: RawUTF8
      read fSuccessRedirectURI write fSuccessRedirectURI;
  end;

  /// parameters used for the validation/verification process of an email address
  // - may be stored as daemon/service level settings, using e.g. dddInfraSettings
  TDDDEmailValidation = class(TSynAutoCreateFields)
  private
    fTemplate: TDomUserEmailTemplate;
    fTemplateFolder: TFileName;
    fRedirection: TDDDEmailRedirection;
  public
    /// will fill some default values in the properties, if none is set
    procedure SetDefaultValuesIfVoid(const aSenderEmail,aApplication,
      aRedirectionURIPublicRoot,aRedirectionURISuccess: RawUTF8);
  published
    /// how the email should be created from a given template
    property Template: TDomUserEmailTemplate read fTemplate;
    /// where the template files are to be found
    property TemplateFolder: TFileName
      read fTemplateFolder write fTemplateFolder;
    /// parameters defining the validation link of an email address
    property Redirection: TDDDEmailRedirection read fRedirection;
  end;

  TSQLRecordEmailAbstract = class;
  TSQLRecordEmailValidation = class;
  TSQLRecordEmailValidationClass = class of TSQLRecordEmailValidation;

  /// abstract parent of any email-related service
  // - will define some common methods to validate an email address
  TDDDEmailServiceAbstract = class(TCQRSQueryObjectRest,IDomUserEmailCheck)
  protected
    fEmailValidate: TSynValidate;
    function CheckEmailCorrect(aEmail: TSQLRecordEmailAbstract;
      var aResult: TCQRSResult): boolean; virtual;
    procedure SetEmailValidate(const Value: TSynValidate); virtual;
  public
    constructor Create(aRest: TSQLRest); override;
    destructor Destroy; override;
    function CheckRecipient(const aEmail: RawUTF8): TCQRSResult; virtual;
    function CheckRecipients(const aEmails: TRawUTF8DynArray): TCQRSResult;
  published
    /// direct access to the email validation instance
    // - you can customize the default TSynValidateEmail to meet your own
    // expectations - once set, it will be owned by this class instance 
    property EmailValidate: TSynValidate read fEmailValidate write SetEmailValidate;
  end;

  /// service used to validate an email address via an URL link to be clicked
  TDDDEmailValidationService = class(TDDDEmailServiceAbstract,
    IDomUserEmailValidation)
  protected
    fRestClass: TSQLRecordEmailValidationClass;
    fEMailer: IDomUserEmailer;
    fTemplate: IDomUserTemplate;
    fValidationSalt: integer;
    fValidationServerRoot: RawUTF8;
    fValidationMethodName: RawUTF8;
    fSuccessRedirectURI: RawUTF8;
    function GetEmailValidation(const aLogonName: RawUTF8): TSQLRecordEmailValidation;
    function GetWithSalt(const aLogonName,aEmail: RawUTF8; aSalt: integer): RawUTF8;
    procedure EmailValidate(Ctxt: TSQLRestServerURIContext);
  public
    /// initialize the validation service for a given ORM persistence
    // - would recognize the TSQLRecordEmailValidation class from aRest.Model
    // - will use aRest.Services for IoC, e.g. EMailer/Template properties 
    constructor Create(aRest: TSQLRest); override;
    /// register the callback URI service
    procedure SetURIForServer(aRestServerPublic: TSQLRestServer;
      aParams: TDDDEmailRedirection); overload;
    /// register the callback URI service
    // - same as the overloaded function, but taking parameters one by one
    procedure SetURIForServer(aRestServerPublic: TSQLRestServer;
      const aRestServerPublicRootURI,aSuccessRedirectURI,aValidationMethodName: RawUTF8); overload;
    /// compute the target URI corresponding to SetURIForServer() parameters
    function ComputeURIForReply(const aLogonName,aEmail: RawUTF8): RawUTF8;
    /// check the supplied parameters, and send an email for validation
    function StartEmailValidation(const aTemplate: TDomUserEmailTemplate;
      const aLogonName,aEmail: RawUTF8): TCQRSResult; virtual;
    /// check if an email has been validated for a given logon
    function IsEmailValidated(const aLogonName,aEmail: RawUTF8): boolean; virtual;
  published
    /// will be injected (and freed) with the emailer service
    property EMailer: IDomUserEmailer read fEmailer;
    /// will be injected (and freed) with the email template service
    property Template: IDomUserTemplate read fTemplate;
  published
    /// the associated ORM class used to store the email validation process
    // - any class inheriting from TSQLRecordEmailValidation in the aRest.Model
    // will be recognized by Create(aRest) to store its information
    // - this temporary storage should not be the main user persistence domain
    property RestClass: TSQLRecordEmailValidationClass read fRestClass;
    /// the validation method name for the URI
    // - if not set, TDDDEmailValidationService will use 'EmailValidate'
    // - clickable URI would be ValidationServerRoot+'/'+ValidationMethodName
    property ValidationURI: RawUTF8 read fValidationMethodName;
    /// the public URI which would be accessible from the Internet
    // - may be e.g 'http://publicserver/restroot'
    property ValidationServerRoot: RawUTF8 read fValidationServerRoot;
  end;

  /// ORM class storing an email in addition to creation/modification timestamps
  // - declared as its own class, since may be reused
  TSQLRecordEmailAbstract = class(TSQLRecordTimed)
  private
    fEmail: RawUTF8;
  published
    /// the stored email address
    property Email: RawUTF8 read fEmail write fEmail;
  end;

  /// ORM class for email validation process
  // - we do not create a whole domain here, just an ORM persistence layer
  // - any class inheriting from TSQLRecordEmailValidation in the Rest.Model
  // will be recognized by TDDDEmailValidationService to store its information
  TSQLRecordEmailValidation = class(TSQLRecordEmailAbstract)
  protected
    fLogon: RawUTF8;
    fRequestTime: TTimeLog;
    fValidationSalt: Integer;
    fValidationTime: TTimeLog;
    fValidationIP: RawUTF8;
  public
    function IsValidated(const aEmail: RawUTF8): Boolean;
  published
    property Logon: RawUTF8 read fLogon write fLogon stored AS_UNIQUE;
    property RequestTime: TTimeLog read fRequestTime write fRequestTime;
    property ValidationSalt: Integer read fValidationSalt write fValidationSalt;
    property ValidationTime: TTimeLog read fValidationTime write fValidationTime;
    property ValidationIP: RawUTF8 read fValidationIP write fValidationIP;
  end;


implementation


{ TDDDEmailServiceAbstract }

constructor TDDDEmailServiceAbstract.Create(aRest: TSQLRest);
begin
  inherited Create(aRest);
  fEmailValidate := TSynValidateEmail.Create;
end;

destructor TDDDEmailServiceAbstract.Destroy;
begin
  fEmailValidate.Free;
  inherited;
end;

function TDDDEmailServiceAbstract.CheckEmailCorrect(
  aEmail: TSQLRecordEmailAbstract; var aResult: TCQRSResult): boolean;
var msg: string;
begin
  if (aEmail<>nil) and fEmailValidate.Process(0,aEmail.Email,msg) and
     aEmail.FilterAndValidate(Rest,msg) then
    result := true else  begin
    CqrsSetResultString(cqrsDDDValidationFailed,msg,aResult);
    result := false;
  end;
end;

function TDDDEmailServiceAbstract.CheckRecipient(
  const aEmail: RawUTF8): TCQRSResult;
var msg: string;
begin
  CqrsBeginMethod(qaNone,result);
  if fEmailValidate.Process(0,aEmail,msg) then
    CqrsSetResult(cqrsSuccess,result) else
    CqrsSetResultString(cqrsDDDValidationFailed,msg,result);
end;

function TDDDEmailServiceAbstract.CheckRecipients(
  const aEmails: TRawUTF8DynArray): TCQRSResult;
var msg: string;
    i: integer;
begin
  CqrsBeginMethod(qaNone,result);
  for i := 0 to high(aEMails) do
    if not fEmailValidate.Process(0,aEmails[i],msg) then begin
      CqrsSetResultString(cqrsDDDValidationFailed,msg,result);
      exit;
    end;
  CqrsSetResult(cqrsSuccess,result);
end;

procedure TDDDEmailServiceAbstract.SetEmailValidate(
  const Value: TSynValidate);
begin
  fEmailValidate.Free;
  fEmailValidate := Value;
end;


{ TDDDEmailValidationService }

constructor TDDDEmailValidationService.Create(aRest: TSQLRest);
var rnd: Int64;
begin
  inherited Create(aRest); // will inject aRest.Services for IoC
  fRestClass := fRest.Model.AddTableInherited(TSQLRecordEmailValidation);
  fRestClass.AddFilterNotVoidText(['Email','Logon']);
  rnd := GetTickCount64*PtrInt(self)*Random(MaxInt);
  fValidationSalt := crc32c(PtrInt(self),@rnd,sizeof(rnd));
end;

function TDDDEmailValidationService.GetWithSalt(const aLogonName,
  aEmail: RawUTF8; aSalt: integer): RawUTF8;
begin
  result := SHA256(FormatUTF8('%'#1'%'#2'%'#3,[aLogonName,aEmail,aSalt]));
end;

function TDDDEmailValidationService.ComputeURIForReply(
  const aLogonName, aEmail: RawUTF8): RawUTF8;
begin
  result := aLogonName+#1+aEmail;
  result := fValidationServerRoot+fValidationMethodName+'/'+
    GetWithSalt(aLogonName,aEmail,fValidationSalt)+
    BinToBase64URI(pointer(result),length(result));
end;

procedure TDDDEmailValidationService.EmailValidate(
  Ctxt: TSQLRestServerURIContext);
var code: RawUTF8;
    logon,email,signature: RawUTF8;
    EmailValidation: TSQLRecordEmailValidation;
begin
  signature := Copy(Ctxt.URIBlobFieldName,1,SHA256DIGESTSTRLEN);
  if length(signature)<>SHA256DIGESTSTRLEN then
    exit;
  code := Base64uriToBin(Copy(Ctxt.URIBlobFieldName,SHA256DIGESTSTRLEN+1,200));
  Split(code,#1,logon,email);
  if (logon='') or (email='') then
    exit;
  EmailValidation := GetEmailValidation(logon);
  if EmailValidation<>nil then
  try
    if signature=GetWithSalt(logon,email,EmailValidation.ValidationSalt) then begin
      EmailValidation.ValidationTime := TimeLogNowUTC;
      EmailValidation.ValidationIP := Ctxt.InHeader['remoteip'];
      if Rest.Update(EmailValidation) then
        Ctxt.Redirect(FormatUTF8(fSuccessRedirectURI,
          [UrlEncode(logon),UrlEncode(email),UrlEncode(EmailValidation.ValidationIP)]));
    end;
  finally
    EmailValidation.Free;
  end;
end;

procedure TDDDEmailValidationService.SetURIForServer(
  aRestServerPublic: TSQLRestServer; aParams: TDDDEmailRedirection);
begin
  if aParams=nil then
    raise EDDDEmail.CreateUTF8('Invalid %.SetURIForServer(%,nil)',
      [self,aRestServerPublic]);
  SetURIForServer(aRestServerPublic,aParams.RestServerPublicRootURI,
    aParams.SuccessRedirectURI,aParams.ValidationMethodName);
end;

procedure TDDDEmailValidationService.SetURIForServer(
  aRestServerPublic: TSQLRestServer; const aRestServerPublicRootURI,
  aSuccessRedirectURI, aValidationMethodName: RawUTF8);
begin
  fSuccessRedirectURI := Trim(aSuccessRedirectURI);
  fValidationServerRoot := IncludeTrailingURIDelimiter(Trim(aRestServerPublicRootURI));
  if (aRestServerPublic=nil) or (fSuccessRedirectURI='') or (fValidationServerRoot='') then
    raise EDDDEmail.CreateUTF8('Invalid %.SetURIForServer(%,"%","%")',
      [self,aRestServerPublic,fValidationServerRoot,fSuccessRedirectURI]);
  if not IdemPChar(pointer(fValidationServerRoot),'HTTP') then
    fValidationServerRoot := 'http://'+fValidationServerRoot;
  fValidationMethodName := Trim(aValidationMethodName);
  if fValidationMethodName='' then
   fValidationMethodName := 'EmailValidate'; // match method name by default
  aRestServerPublic.ServiceMethodRegister(fValidationMethodName,EmailValidate,true);
end;

function TDDDEmailValidationService.GetEmailValidation(const aLogonName: RawUTF8): TSQLRecordEmailValidation;
begin
  result := RestClass.Create(Rest,'Logon=?',[aLogonName]);
  if result.fID=0 then
    FreeAndNil(result);
end;

function TDDDEmailValidationService.IsEmailValidated(const aLogonName,
  aEmail: RawUTF8): boolean;
var EmailValidation: TSQLRecordEmailValidation;
begin
  EmailValidation := GetEmailValidation(aLogonName);
  try
    result := EmailValidation.IsValidated(trim(aEmail));
  finally
    EmailValidation.Free;
  end;
end;

function TDDDEmailValidationService.StartEmailValidation(
  const aTemplate: TDomUserEmailTemplate; const aLogonName, aEmail: RawUTF8): TCQRSResult;
var EmailValidation: TSQLRecordEmailValidation;
    email,msg: RawUTF8;
    context: variant;
begin
  email := Trim(aEmail);
  result := CheckRecipient(email);
  if result<>cqrsSuccess then
    exit; // supplied email address is invalid
  CqrsBeginMethod(qaNone,result);
  EmailValidation := GetEmailValidation(aLogonName);
  try
    if EmailValidation.IsValidated(email) then begin
      CqrsSetResultMsg(cqrsSuccess,'Already validated',result);
      exit;
    end;
    if EmailValidation=nil then begin
      EmailValidation := RestClass.Create;
      EmailValidation.Email := aEmail;
      EmailValidation.Logon := aLogonName;
      if not CheckEmailCorrect(EmailValidation,result) then
        exit;
    end else
      if EmailValidation.Email<>email then
        EmailValidation.Email := email; // allow validation for a new email
    EmailValidation.RequestTime := TimeLogNowUTC;
    EmailValidation.ValidationSalt := fValidationSalt;
    context := EmailValidation.GetSimpleFieldsAsDocVariant(true);
    _ObjAddProps(aTemplate,context);
    _ObjAddProps(['ValidationUri',
      ComputeURIForReply(EmailValidation.Logon,EmailValidation.Email)],context);
    msg := Template.ComputeMessage(context,aTemplate.FileName);
    if msg='' then
      CqrsSetResultMsg(cqrsInvalidContent,
        'Impossible to render template [%]',[aTemplate.FileName],result) else
      if EMailer.SendEmail(TRawUTF8DynArrayFrom([aEmail]),
          aTemplate.SenderEmail,aTemplate.Subject,'',msg)=cqrsSuccess then
        if Rest.AddOrUpdate(EmailValidation)=0 then
          CqrsSetResultError(cqrsDataLayerError) else
          CqrsSetResultMsg(cqrsSuccess,'Validation email sent',result);
  finally
    EmailValidation.Free;
  end;
end;    


{ TSQLRecordEmailValidation }

function TSQLRecordEmailValidation.IsValidated(const aEmail: RawUTF8): Boolean;
begin
  result := (self<>nil) and (ValidationTime<>0) and (Email=aEmail);
end;


{ TDDDEmailValidation }

procedure TDDDEmailValidation.SetDefaultValuesIfVoid(
  const aSenderEmail,aApplication,
  aRedirectionURIPublicRoot,aRedirectionURISuccess: RawUTF8);
begin
  if Template.SenderEmail='' then
    Template.SenderEmail := aSenderEmail;
  if Template.Application='' then
    Template.Application := aApplication;
  if Template.FileName='' then
    Template.FileName := 'EmailValidate.txt';
  if (TemplateFolder='') and
     not FileExists(string(Template.FileName)) then
    FileFromString('Welcome to {{Application}}!'#13#10#13#10+
      'You have registered as "{{Logon}}", using {{EMail}} as contact address.'#13#10#13#10+
      'Please click on the following link to validate your email:'#13#10+
      '{{ValidationUri}}'#13#10#13#10'Best regards from the clouds'#13#10#13#10+
      '(please do not respond to this email)',
      UTF8ToString(Template.FileName));
  if Template.Subject='' then
    Template.Subject := 'Please Validate Your Email';
  if Redirection.RestServerPublicRootURI='' then
    Redirection.RestServerPublicRootURI := aRedirectionURIPublicRoot;
  if Redirection.SuccessRedirectURI='' then
    Redirection.SuccessRedirectURI := aRedirectionURISuccess;
end;

end.
