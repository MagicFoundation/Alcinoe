unit DomUserInterfaces;

interface

uses 
  SysUtils,
  SynCommons,
  mORMot,
  mORMotDDD,
  DomUserTypes;

type
  IDomUserEmailCheck = interface(IInvokable)
    ['{2942BC2D-84F7-4A79-8657-07F0602C3505}']
    function CheckRecipient(const aEmail: RawUTF8): TCQRSResult;
    function CheckRecipients(const aEmails: TRawUTF8DynArray): TCQRSResult; 
  end;

  IDomUserEmailValidation = interface(IDomUserEmailCheck)
    ['{20129489-5054-4D4A-84B9-463DB98156B8}']
    function ComputeURIForReply(const aLogonName,aEmail: RawUTF8): RawUTF8;
    function StartEmailValidation(const aTemplate: TDomUserEmailTemplate;
      const aLogonName,aEmail: RawUTF8): TCQRSResult;
    function IsEmailValidated(const aLogonName,aEmail: RawUTF8): boolean;
  end;

  IDomUserEmailer = interface(IInvokable)
    ['{20B88FCA-B345-4D5E-8E07-4581C814AFD9}']
    function SendEmail(const aRecipients: TRawUTF8DynArray;
      const aSender,aSubject,aHeaders,aBody: RawUTF8): TCQRSResult;
  end;

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