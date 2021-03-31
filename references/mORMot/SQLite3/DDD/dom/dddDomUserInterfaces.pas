/// shared DDD Domains: User interfaces definition
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddDomUserInterfaces;

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
  SynCommons,
  SysUtils,
  Classes,
  mORMot,
  mORMotDDD,
  dddDomUserTypes;

type
  /// defines a service able to check the correctness of email addresses
  // - will be implemented e.g. by TDDDEmailServiceAbstract and
  // TDDDEmailValidationService as defined in the dddInfraEmail unit
  IDomUserEmailCheck = interface(IInvokable)
    ['{2942BC2D-84F7-4A79-8657-07F0602C3505}']
    /// check if the supplied email address seems correct
    function CheckRecipient(const aEmail: RawUTF8): TCQRSResult;
    /// check if the supplied email addresses seem correct
    function CheckRecipients(const aEmails: TRawUTF8DynArray): TCQRSResult;
  end;

  /// defines a service sending a confirmation email to validate an email address
  // - will be implemented e.g. by TDDDEmailValidationService as defined in
  // the dddInfraEmail unit
  IDomUserEmailValidation = interface(IDomUserEmailCheck)
    ['{20129489-5054-4D4A-84B9-463DB98156B8}']
    /// internal method used to compute the validation URI
    // - will be included as data context to the email template, to create the
    // validation link
    function ComputeURIForReply(const aLogonName,aEmail: RawUTF8): RawUTF8;
    /// initiate an email validation process, using the given template
    function StartEmailValidation(const aTemplate: TDomUserEmailTemplate;
      const aLogonName,aEmail: RawUTF8): TCQRSResult;
    function IsEmailValidated(const aLogonName,aEmail: RawUTF8): boolean;
  end;

  /// defines a generic service able to send emails
  // - will be implemented e.g. by TDDDEmailerDaemon as defined in the
  // dddInfraEmailer unit
  IDomUserEmailer = interface(IInvokable)
    ['{20B88FCA-B345-4D5E-8E07-4581C814AFD9}']
    function SendEmail(const aRecipients: TRawUTF8DynArray;
      const aSender,aSubject,aHeaders,aBody: RawUTF8): TCQRSResult;
  end;

  /// defines a service for generic rendering of a template
  // - will be implemented e.g. via our SynMustache engine by TDDDTemplateAbstract
  // and TDDDTemplateFromFolder as defined in the dddInfraEmailer unit
  IDomUserTemplate = interface(IInvokable)
    ['{378ACC52-46BE-488D-B7ED-3F4E59316DFF}']
    function ComputeMessage(const aContext: variant;
      const aTemplateName: RawUTF8): RawUTF8;
  end;

  
implementation

initialization
  TInterfaceFactory.RegisterInterfaces(
    [TypeInfo(IDomUserEmailValidation),TypeInfo(IDomUserEmailer),
     TypeInfo(IDomUserTemplate)]);
end.
