unit Alcinoe.iOSApi.MessageUI;

interface

{$I Alcinoe.inc}

uses
  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  iOSapi.UIKit,
  Macapi.ObjectiveC;

{$M+}

type

  {*********************************}
  MFMessageComposeResult = NSInteger;

const

  {********************************}
  MessageComposeResultCancelled = 0;
  MessageComposeResultSent      = 1;
  MessageComposeResultFailed    = 2;

type

  {*************************************************}
  MFMessageComposeViewControllerDelegate = interface;

  {**************************************************************************}
  MFMessageComposeViewControllerClass = interface(UINavigationControllerClass)
  ['{BF34F7BC-9C09-4D86-9EB0-6D563E0ADFD3}']
    {classe} function canSendText: Boolean; cdecl;
  end;
  MFMessageComposeViewController = interface(UINavigationController)
  ['{8C21FEF5-8AAA-4C51-BE7D-154BB6E11387}']
    function messageComposeDelegate: MFMessageComposeViewControllerDelegate; cdecl;
    procedure setMessageComposeDelegate(messageComposeDelegate: MFMessageComposeViewControllerDelegate); cdecl;
    function recipients: NSArray; cdecl;
    procedure setRecipients(recipients: NSArray); cdecl;
    function body: NSString; cdecl;
    procedure setBody(body: NSString); cdecl;
    function subject: NSString; cdecl;
    procedure setSubject(subject: NSString); cdecl;
  end;
  TMFMessageComposeViewController = class(TOCGenericImport<MFMessageComposeViewControllerClass, MFMessageComposeViewController>) end;

  {*************************************************************}
  MFMessageComposeViewControllerDelegate = interface(IObjectiveC)
  ['{1C9616CB-1EA8-4AC8-A1AA-38A12DB5C26A}']
    procedure messageComposeViewController(controller: MFMessageComposeViewController; didFinishWithResult: MFMessageComposeResult); cdecl;
  end;

  {***********************************************************************************************}
  TMFMessageComposeViewControllerDelegate = class(TOCLocal, MFMessageComposeViewControllerDelegate)
  private
    fMFMessageComposeViewController: MFMessageComposeViewController;
  public
    constructor Create(aMFMessageComposeViewController: MFMessageComposeViewController);
    procedure messageComposeViewController(controller: MFMessageComposeViewController; didFinishWithResult: MFMessageComposeResult); cdecl;
  end;

type

  {******************************}
  MFMailComposeResult = NSInteger;

const

  {*******************************}
  MFMailComposeResultCancelled = 0;
  MFMailComposeResultSaved     = 1;
  MFMailComposeResultSent      = 2;
  MFMailComposeResultFailed    = 3;

type

  {**********************************************}
  MFMailComposeViewControllerDelegate = interface;

  {***********************************************************************}
  MFMailComposeViewControllerClass = interface(UINavigationControllerClass)
  ['{B6292F63-0DE9-4FE7-BEF7-871D5FE75362}']
    function canSendMail: Boolean; cdecl;
  end;
  MFMailComposeViewController = interface(UINavigationController)
  ['{5AD35A29-4418-48D3-AB8A-2F114B4B0EDC}']
    function mailComposeDelegate: MFMailComposeViewControllerDelegate; cdecl;
    procedure setMailComposeDelegate( mailComposeDelegate: MFMailComposeViewControllerDelegate ); cdecl;
    procedure setSubject( subject: NSString ); cdecl;
    procedure setToRecipients( toRecipients: NSArray ); cdecl;
    procedure setCcRecipients( ccRecipients: NSArray ); cdecl;
    procedure setBccRecipients( bccRecipients: NSArray ); cdecl;
    procedure setMessageBody( body: NSString; isHTML: Boolean ); cdecl;
    procedure addAttachmentData( attachment: NSData; mimeType: NSString; fileName: NSString ); cdecl;
  end;
  TMFMailComposeViewController = class(TOCGenericImport<MFMailComposeViewControllerClass, MFMailComposeViewController>)
  end;

  {**********************************************************}
  MFMailComposeViewControllerDelegate = interface(IObjectiveC)
  ['{068352EB-9182-4581-86F5-EAFCE7304E32}']
    procedure mailComposeController(controller: MFMailComposeViewController; didFinishWithResult: MFMailComposeResult; error: NSError); cdecl;
  end;

  {*****************************************************************************************}
  TMFMailComposeViewControllerDelegate = class(TOCLocal, MFMailComposeViewControllerDelegate)
  private
    fMFMailComposeViewController: MFMailComposeViewController;
  public
    constructor Create(aMFMailComposeViewController: MFMailComposeViewController);
    procedure mailComposeController(controller: MFMailComposeViewController; didFinishWithResult: MFMailComposeResult; error: NSError); cdecl;
  end;

const
  libMessageUI = '/System/Library/Frameworks/MessageUI.framework/MessageUI';

implementation

uses
  FMX.Helpers.iOS;

{**************************************************************************************************************************}
constructor TMFMessageComposeViewControllerDelegate.Create(aMFMessageComposeViewController: MFMessageComposeViewController);
begin
  inherited Create;
  fMFMessageComposeViewController := aMFMessageComposeViewController;
end;

{**********************************************************************************************************************************************************************}
procedure TMFMessageComposeViewControllerDelegate.messageComposeViewController(controller: MFMessageComposeViewController; didFinishWithResult: MFMessageComposeResult);
var LWindow: UIWindow;
begin

  LWindow := SharedApplication.keyWindow;
  if Assigned(LWindow) and Assigned(LWindow.rootViewController) then
    LWindow.rootViewController.dismissModalViewControllerAnimated(True{animated});

  fMFMessageComposeViewController.release;
  fMFMessageComposeViewController := nil;

end;

{*****************************************************************************************************************}
constructor TMFMailComposeViewControllerDelegate.Create(aMFMailComposeViewController: MFMailComposeViewController);
begin
  inherited Create;
  fMFMailComposeViewController := aMFMailComposeViewController;
end;

{**********************************************************************************************************************************************************************}
procedure TMFMailComposeViewControllerDelegate.mailComposeController(controller: MFMailComposeViewController; didFinishWithResult: MFMailComposeResult; error: NSError);
var LWindow: UIWindow;
begin

  LWindow := SharedApplication.keyWindow;
  if Assigned(LWindow) and Assigned(LWindow.rootViewController) then
    LWindow.rootViewController.dismissModalViewControllerAnimated(True{animated});

  fMFMailComposeViewController.release;
  fMFMailComposeViewController := nil;

end;

{*************************************************************}
procedure LibMessageUIFakeLoader; cdecl; external libMessageUI;

end.