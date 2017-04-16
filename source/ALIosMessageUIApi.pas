unit ALIosMessageUIApi;

interface

uses iOSapi.Foundation,
     iOSapi.CocoaTypes,
     iOSapi.UIKit,
     Macapi.ObjectiveC;

const

  {************************************************************************}
  libMessageUI = '/System/Library/Frameworks/MessageUI.framework/MessageUI';

  {*********************************}
  //@enum       MessageComposeResult
  //@abstract   Composition result sent to the delegate upon user completion.
  //@discussion This result will inform the client of the user's message composition action.  If the
  //			 user cancels the composition, <tt>MessageComposeResultCancelled</tt> will be sent to the delegate.
  //			 Typically <tt>MessageComposeResultSent</tt> will be sent, but <tt>MessageComposeResultFailed</tt> will
  //			 be sent in the case of failure. </p>Send may only be interpreted as a successful queueing of
  //			 the message for later sending. The actual send will occur when the device is able to send.
  //@constant   MessageComposeResultCancelled   User canceled the composition.
  //@constant   MessageComposeResultSent        User successfully sent/queued the message.
  //@constant   MessageComposeResultFailed      User's attempt to save or send was unsuccessful.
  MessageComposeResultCancelled = 0;
  MessageComposeResultSent      = 1;
  MessageComposeResultFailed    = 2;

type

  //typedef enum MessageComposeResult MessageComposeResult;   // available in iPhone 4.0
  MFMessageComposeResult = NSInteger;

type

  {*************************************************}
  MFMessageComposeViewControllerDelegate = interface;

  {**************************************************************************}
  MFMessageComposeViewControllerClass = interface(UINavigationControllerClass)
  ['{BF34F7BC-9C09-4D86-9EB0-6D563E0ADFD3}']

    //+ (BOOL)canSendText  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_4_0);
    //@method     canSendText
    //@abstract   Returns <tt>YES</tt> if the user has set up the device for sending text only messages.
    //@discussion If the return value is YES, the client can set the recipients and body of the message.
		//      If the return value is NO, the client may notify the user of the failure, or the
		//      client may open an SMS URL via <tt>-[UIApplication openURL:]</tt>.
    {classe} function canSendText: Boolean; cdecl;

    //+ (BOOL)canSendSubject __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_7_0);
    //@method     canSendSubject
    //@abstract   Returns <tt>YES</tt> if the user has set up the device for including subjects in messages.</tt>.

    //+ (BOOL)canSendAttachments __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_7_0);
    //@method     canSendAttachments
    //@abstract   Returns <tt>YES</tt> if the user has set up the device for including attachments in messages.</tt>.

    //+ (BOOL)isSupportedAttachmentUTI:(NSString *)uti __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_7_0);
    //@method     isSupportedAttachmentUTI:
    //@abstract   Returns <tt>YES</tt>if the attachment at the specified URL could be accepted by the current composition.
    //@discussion If the return value is YES, the UTI is acceptable for attachment to a message, a return value of NO
    //            indicates that the given UTI is unsupported.

  end;

  {*****************************************************************}
  //@interface MFMessageComposeViewController : UINavigationController
  //@class       MFMessageComposeViewController
  //@abstract    The MFMessageComposeViewController class provides an interface for editing and sending a message.
  //@discussion  The MFMessageComposeViewController class manages all user interaction.  The client needs to set
  //			  the recipient or recipients.  The client may also set the body of the message. After setup, the
  //			  client needs to only display the view.
  //			  </p>The provided delegate will be informed of the user's composition completion and how they chose
  //			  to complete the operation.
  //			  <p>Prior to use, clients should verify the user has set up the device for sending messages via
  //			  <tt>+[MFMessageComposeViewController canSendText]</tt>.
  MFMessageComposeViewController = interface(UINavigationController)
  ['{8C21FEF5-8AAA-4C51-BE7D-154BB6E11387}']

    //@property(nonatomic,assign,nullable) id<MFMessageComposeViewControllerDelegate> messageComposeDelegate /*__OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_4_0)*/;
    //@property   messageComposeDelegate
    //@abstract   This property is the delegate for the MFMessageComposeViewController method callbacks.
    function messageComposeDelegate: MFMessageComposeViewControllerDelegate; cdecl;
    procedure setMessageComposeDelegate(messageComposeDelegate: MFMessageComposeViewControllerDelegate); cdecl;

    //- (void)disableUserAttachments __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_7_0);
    //@method     disableUserAttachments;
    //@abstract   Calling this method will disable the camera/attachment button in the view controller.  After the controller has been presented,
    //            this call will have no effect.  The camera / attachment button is visible by default.

    //@property(nonatomic,copy,nullable) NSArray<NSString *> *recipients /*__OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_4_0)*/;
    //@property   recipients
    //@abstract   This property sets the initial value of the To field for the message to the specified addresses.
    //@discussion This property will set the initial value of the To field for the message from an NSArray of
    //			 NSString instances specifying the message addresses of recipients. This should be called prior
    //			 to display.
    //			 </p>After the view has been presented to the user, this property will no longer change the value.
    function recipients: NSArray; cdecl;
    procedure setRecipients(recipients: NSArray); cdecl;

    //@property(nonatomic,copy,nullable) NSString *body /*__OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_4_0)*/;
    //@property   body
    //@abstract   This property sets the initial value of the body of the message to the specified content.
    //@discussion This property will set the initial value of the body of the message.  This should be called prior
    //       to display.
    //       </p>After the view has been presented to the user, this property will no longer change the value.
    function body: NSString; cdecl;
    procedure setBody(body: NSString); cdecl;

    //@property(nonatomic,copy,nullable) NSString *subject /*__OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_7_0)*/;
    //@property   subject
    //@abstract   This property sets the initial value of the subject of the message to the specified content.
    //@discussion This property will set the initial value of the subject of the message.  This should be called prior
    //to display.
    //</p>After the view has been presented to the user, this property will no longer change the value.
    function subject: NSString; cdecl;
    procedure setSubject(subject: NSString); cdecl;

    //@property(nonatomic,copy,readonly,nullable) NSArray<NSDictionary *> *attachments /*__OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_7_0)*/;
    //@property   attachments
    //@abstract   This property returns an NSArray of NSDictionaries describing the properties of the current attachments.
    //@discussion This property returns an NSArray of NSDictionaries describing the properties of the current attachments.
    //      See MFMessageComposeViewControllerAttachmentURL, MFMessageComposeViewControllerAttachmentAlternateFilename.

    //- (BOOL)addAttachmentURL:(NSURL *)attachmentURL withAlternateFilename:(nullable NSString *)alternateFilename __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_7_0);
    //@method     addAttachmentURL:withAlternateFilename:
    //@abstract   Returns <tt>YES</tt>if the attachment at the specified URL was added to the composition successfully.
    //@discussion If the return value is YES, the attachment was added to the composition. If the return value is NO,
    //            the attachment was not added to the composition.  All attachment URLs must be file urls.  The file
    //            URL must not be NIL.  The alternate filename will be display to the user in leiu of the attachments URL.
    //            The alternate filename may be NIL.

    //- (BOOL)addAttachmentData:(NSData *)attachmentData typeIdentifier:(NSString *)uti filename:(NSString *)filename __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_7_0);
    //@method     addAttachmentData:typeIdentifier:filename:
    //@abstract   Returns <tt>YES</tt>if the attachment was added to the composition successfully.
    //@discussion If the return value is YES, the attachment was added to the composition. If the return value is NO,
    //the attachment was not added to the composition.  The data and typeIdentifer must be non-nil.  typeIdentifier should be a valid Uniform Type Identifier.

  end;
  TMFMessageComposeViewController = class(TOCGenericImport<MFMessageComposeViewControllerClass, MFMessageComposeViewController>) end;

  {*************************************************************}
  //@protocol MFMessageComposeViewControllerDelegate <NSObject>
  //@protocol    MFMessageComposeViewControllerDelegate
  //@abstract    Protocol for delegate callbacks to MFMessageComposeViewControllerDelegate instances.
  //@discussion  This protocol will be implemented by delegates of MFMessageComposeViewController instances.
  //       It will be called at various times while the user is composing, sending, or canceling
  //       message composition.
  MFMessageComposeViewControllerDelegate = interface(IObjectiveC)
  ['{1C9616CB-1EA8-4AC8-A1AA-38A12DB5C26A}']

    //- (void)messageComposeViewController:(MFMessageComposeViewController *)controller didFinishWithResult:(MessageComposeResult)result;
    //@required
    //@method     messageComposeViewController:didFinishWithResult:
    //@abstract   Delegate callback which is called upon user's completion of message composition.
    //@discussion This delegate callback will be called when the user completes the message composition.
    //		 How the user chose to complete this task will be given as one of the parameters to the
    //		 callback.  Upon this call, the client should remove the view associated with the controller,
    //		 typically by dismissing modally.
    //@param      controller   The MFMessageComposeViewController instance which is returning the result.
    //@param      result       MessageComposeResult indicating how the user chose to complete the composition process.
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

const

  {*******************************}
  //@enum       MFMailComposeResult
  //@abstract   Composition result sent to the delegate upon user completion.
  //@discussion This result will inform of the user's choice in regards to email composition.  If the user
  //            cancels the composition, <tt>MFMailComposeResultCancelled</tt> will be sent to the delegate.
  //            Typically <tt>MFMailComposeResultSent</tt> or <tt>MFMailComposeResultSaved</tt> will
  //            be sent, but <tt>MFMailComposeResultFailed</tt> will be sent in the case of failure.
  //            </p>Send may only be interpreted as a successful queueing of the message for later sending.
  //            The actual send will occur when the device is able to send.
  //@constant   MFMailComposeResultCancelled   User canceled the composition.
  //@constant   MFMailComposeResultSaved       User successfully saved the message.
  //@constant   MFMailComposeResultSent        User successfully sent/queued the message.
  //@constant   MFMailComposeResultFailed      User's attempt to save or send was unsuccessful.
  MFMailComposeResultCancelled = 0;
  MFMailComposeResultSaved     = 1;
  MFMailComposeResultSent      = 2;
  MFMailComposeResultFailed    = 3;

type

  //typedef enum MFMailComposeResult MFMailComposeResult;   // available in iPhone 3.0
  MFMailComposeResult = NSInteger;

type

  {**********************************************}
  MFMailComposeViewControllerDelegate = interface;

  {***********************************************************************}
  MFMailComposeViewControllerClass = interface(UINavigationControllerClass)
  ['{B6292F63-0DE9-4FE7-BEF7-871D5FE75362}']
    //+ (BOOL)canSendMail __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_3_0);
    //@method     canSendMail
    //@abstract   Returns <tt>YES</tt> if the user has set up the device for sending email.
    //@discussion The client may continue to set the recipients and content if the return value was <tt>YES</tt>.  If <tt>NO</tt>
    //            was the result, the client has a couple options.  It may choose to simply notify the user of the inability to
    //            send mail, or it may issue a "mailto" URL via <tt>-[UIApplication openURL:]</tt>.
    function canSendMail: Boolean; cdecl;
  end;

  {***************************************************************}
  //@interface MFMailComposeViewController : UINavigationController
  //@class       MFMailComposeViewController
  //@abstract    The MFMailComposeViewController class provides an interface for editing and sending email.
  //@discussion  The MFMailComposeViewController class manages all user interaction.  The client needs to set the recipient or
  //             recipients.  The client may also set the subject and the body of the message.  Attachments may be added, if
  //             so desired.  After setup, the client needs to only display the view.</p>The provided delegate will be informed
  //             of the user's composition completion and how they chose to complete the operation.<p>Prior to use, clients
  //             should verify the user has set up the device for sending email via <tt>+[MFMailComposeViewController canSendMail]</tt>.
  MFMailComposeViewController = interface(UINavigationController)
  ['{5AD35A29-4418-48D3-AB8A-2F114B4B0EDC}']

    //@property (nonatomic, assign, nullable) id<MFMailComposeViewControllerDelegate> mailComposeDelegate /*__OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_3_0);
    //@property   mailComposeDelegate
    //@abstract   This property is the delegate for the MFMailComposeViewControllerDelegate method callbacks.
    function mailComposeDelegate: MFMailComposeViewControllerDelegate; cdecl;
    procedure setMailComposeDelegate( mailComposeDelegate: MFMailComposeViewControllerDelegate ); cdecl;


    //- (void)setSubject:(NSString *)subject __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_3_0);
    //@method     setSubject:
    //@abstract   This method sets the Subject header for the email message.
    //@discussion This method will set the Subject header for the email message.  This should be called prior to display.
    //            Newlines are removed from the parameter.
    //            </p>After the view has been presented to the user, this method will no longer change the value.
    //@param      subject  A NSString specifying the message's Subject header.
    procedure setSubject( subject: NSString ); cdecl;

    //- (void)setToRecipients:(nullable NSArray<NSString *> *)toRecipients __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_3_0);
    //@method     setToRecipients:
    //@abstract   This method sets the To header for the email message to the specified email addresses.
    //@discussion This method will set the To header for the email message.  This should be called prior to display.
    //            </p>Recipient addresses should be specified as per RFC5322.
    //            </p>After the view has been presented to the user, this method will no longer change the value.
    //@param      toRecipients  A NSArray of NSString instances specifying the email addresses of recipients.
    procedure setToRecipients( toRecipients: NSArray ); cdecl;

    //- (void)setCcRecipients:(nullable NSArray<NSString *> *)ccRecipients __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_3_0);
    //@method     setCcRecipients:
    //@abstract   This method sets the CC header for the email message to the specified email addresses.
    //@discussion This method will set the CC header for the email message.  This should be called prior to display.
    //            </p>Recipient addresses should be specified as per RFC5322.
    //            </p>After the view has been presented to the user, this method will no longer change the value.
    //@param      ccRecipients  A NSArray of NSString instances specifying the email addresses of recipients.
    procedure setCcRecipients( ccRecipients: NSArray ); cdecl;

    //- (void)setBccRecipients:(nullable NSArray<NSString *> *)bccRecipients __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_3_0);
    //@method     setBccRecipients:
    //@abstract   This method sets the BCC header for the email message to the specified email addresses.
    //@discussion This method will set the BCC header for the email message.  This should be called prior to display.
    //            </p>Recipient addresses should be specified as per RFC5322.
    //            </p>After the view has been presented to the user, this method will no longer change the value.
    //@param      bccRecipients    A NSArray of NSString instances specifying the email addresses of recipients.
    procedure setBccRecipients( bccRecipients: NSArray ); cdecl;

    //- (void)setMessageBody:(NSString *)body isHTML:(BOOL)isHTML __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_3_0);
    //@method     setMessageBody:isHTML:
    //@abstract   This method sets the body of the email message to the specified content.
    //@discussion This method will set the body of the email message.  This should be called prior to display.
    //            The user's signature, if specified, will be added after the body content.
    //@param      body A NSString containing the body contents of the email message.
    //@param      isHTML  A boolean value indicating if the body argument is to be interpreted as HTML content.
    procedure setMessageBody( body: NSString; isHTML: Boolean ); cdecl;

    //- (void)addAttachmentData:(NSData *)attachment mimeType:(NSString *)mimeType fileName:(NSString *)filename __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_3_0);
    //@method     addAttachmentData:mimeType:fileName:
    //@abstract   This method adds the specified attachment to the email message.
    //@discussion This method adds the specified attachment to the email message.  This should be called prior to display.
    //            Attachments will be appended to the end of the message.
    //@param      attachment   NSData containing the contents of the attachment.  Must not be <tt>nil</tt>.
    //@param      mimeType     NSString specifying the MIME type for the attachment, as specified by the IANA
    //                            (http://www.iana.org/assignments/media-types/). Must not be <tt>nil</tt>.
    //@param      filename     NSString specifying the intended filename for the attachment.  This is displayed below
    //                            the attachment's icon if the attachment is not decoded when displayed.  Must not be <tt>nil</tt>.
    procedure addAttachmentData( attachment: NSData; mimeType: NSString; fileName: NSString ); cdecl;

  end;

  {*******************************************************************************************************************}
  TMFMailComposeViewController = class(TOCGenericImport<MFMailComposeViewControllerClass, MFMailComposeViewController>)
  end;

  {********************************************************}
  //@protocol MFMailComposeViewControllerDelegate <NSObject>
  //@protocol    MFMailComposeViewControllerDelegate
  //@abstract    Protocol for delegate callbacks to MFMailComposeViewController instances.
  //@discussion  This protocol must be implemented for delegates of MFMailComposeViewController instances.  It will
  //             be called at various times while the user is composing, sending, saving, or canceling email composition.
  MFMailComposeViewControllerDelegate = interface(IObjectiveC)
  ['{068352EB-9182-4581-86F5-EAFCE7304E32}']
    //- (void)mailComposeController:(MFMailComposeViewController *)controller didFinishWithResult:(MFMailComposeResult)result error:(nullable NSError *)error __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_3_0);
    //@method     mailComposeController:didFinishWithResult:error:
    //@abstract   Delegate callback which is called upon user's completion of email composition.
    //@discussion This delegate callback will be called when the user completes the email composition.  How the user chose
    //            to complete this task will be given as one of the parameters to the callback.  Upon this call, the client
    //            should remove the view associated with the controller, typically by dismissing modally.
    //@param      controller   The MFMailComposeViewController instance which is returning the result.
    //@param      result       MFMailComposeResult indicating how the user chose to complete the composition process.
    //@param      error        NSError indicating the failure reason if failure did occur.  This will be <tt>nil</tt> if
    //                         result did not indicate failure.
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


implementation

uses {$IF not defined(CPUARM)}
     Posix.Dlfcn,
     {$ENDIF}
     FMX.Helpers.iOS;

{**************************************************************************************************************************}
constructor TMFMessageComposeViewControllerDelegate.Create(aMFMessageComposeViewController: MFMessageComposeViewController);
begin
  inherited Create;
  fMFMessageComposeViewController := aMFMessageComposeViewController;
end;

{**********************************************************************************************************************************************************************}
procedure TMFMessageComposeViewControllerDelegate.messageComposeViewController(controller: MFMessageComposeViewController; didFinishWithResult: MFMessageComposeResult);
var aWindow: UIWindow;
begin

  aWindow := SharedApplication.keyWindow;
  if Assigned(aWindow) and Assigned(aWindow.rootViewController) then
    aWindow.rootViewController.dismissModalViewControllerAnimated(True{animated});

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
var aWindow: UIWindow;
begin

  aWindow := SharedApplication.keyWindow;
  if Assigned(aWindow) and Assigned(aWindow.rootViewController) then
    aWindow.rootViewController.dismissModalViewControllerAnimated(True{animated});

  fMFMailComposeViewController.release;
  fMFMailComposeViewController := nil;

end;

{$IF defined(CPUARM)}

procedure LibMessageUIFakeLoader; cdecl; external libMessageUI;

{$ELSE}

var iMessageUIModule: THandle;

initialization
iMessageUIModule := dlopen(MarshaledAString(libMessageUI), RTLD_LAZY);

finalization
dlclose(iMessageUIModule);

{$ENDIF}

end.
