unit ALIosFacebookApi;

interface

uses Macapi.ObjectiveC,
     iOSapi.Foundation,
     iOSapi.CocoaTypes,
     iOSapi.UIKit;

{$M+}

Type

  // Specifies the privacy of a group
  FBSDKAppInviteDestination = NSUInteger;

const

  // Deliver to Facebook.
  FBSDKAppInviteDestinationFacebook = 0;
  // Deliver to Messenger
  FBSDKAppInviteDestinationMessenger = 1;

type

  {***************************************************************************}
  // @interface FBSDKAppInviteContent : NSObject <FBSDKCopying, NSSecureCoding>
  // A model for app invite.
  FBSDKAppInviteContentClass = interface(NSObjectClass)
  ['{59360868-F7EF-4514-8228-14AA8FE69F74}']
  end;
  FBSDKAppInviteContent = interface(NSObject)
  ['{AF44BB41-ED41-45FC-ADDB-C22467D911C9}']

    // A URL to a preview image that will be displayed with the app invite
    // This is optional.  If you don't include it a fallback image will be used.
    // @property (nonatomic, copy) NSURL *appInvitePreviewImageURL;
    function appInvitePreviewImageURL: NSURL; cdecl;
    procedure setAppInvitePreviewImageURL(appInvitePreviewImageURL: NSURL); cdecl;

    // An app link target that will be used as a target when the user accept the invite.
    // This is a requirement.
    // @property (nonatomic, copy) NSURL *appLinkURL;
    function appLinkURL: NSURL; cdecl;
    procedure setAppLinkURL(appLinkURL: NSURL); cdecl;

    // - Warning:Use `appInvitePreviewImageURL` instead.
    // @property (nonatomic, copy) NSURL *previewImageURL __attribute__ ((deprecated("use appInvitePreviewImageURL instead")));

    // Promotional code to be displayed while sending and receiving the invite.
    // This is optional. This can be between 0 and 10 characters long and can contain
    // alphanumeric characters only. To set a promo code, you need to set promo text.
    // @property (nonatomic, copy) NSString *promotionCode;
    function promotionCode:  NSString; cdecl;
    procedure setPromotionCode(promotionCode: NSString); cdecl;

    // Promotional text to be displayed while sending and receiving the invite.
    // This is optional. This can be between 0 and 80 characters long and can contain
    // alphanumeric and spaces only.
    // @property (nonatomic, copy) NSString *promotionText;
    function promotionText:  NSString; cdecl;
    procedure setPromotionText(promotionText: NSString); cdecl;

    // Destination for the app invite.
    // This is optional and for declaring destination of the invite.
    // @property FBSDKAppInviteDestination destination;
    function destination: FBSDKAppInviteDestination; cdecl;
    procedure setDestination(destination: FBSDKAppInviteDestination); cdecl;

    // Compares the receiver to another app invite content.
    //  - Parameter content: The other content
    //  - Returns: YES if the receiver's values are equal to the other content's values; otherwise NO
    // - (BOOL)isEqualToAppInviteContent:(FBSDKAppInviteContent *)content;
    function isEqualToAppInviteContent(content: FBSDKAppInviteContent): Boolean; cdecl;

  end;
  TFBSDKAppInviteContent = class(TOCGenericImport<FBSDKAppInviteContentClass, FBSDKAppInviteContent>) end;

  {*******************************************}
  // @interface FBSDKAppInviteDialog : NSObject
  // A dialog for sending App Invites.
  FBSDKAppInviteDialogClass = interface(NSObjectClass)
  ['{3290E4D2-9003-4141-960C-15FF94369CE4}']

    // Convenience method to show a FBSDKAppInviteDialog
    //  - Parameter viewController: A UIViewController to present the dialog from.
    //  - Parameter content: The content for the app invite.
    //  - Parameter delegate: The receiver's delegate.
    // + (instancetype)showFromViewController:(UIViewController *)viewController
    //                            withContent:(FBSDKAppInviteContent *)content
    //                               delegate:(id<FBSDKAppInviteDialogDelegate>)delegate;

    // - Warning:use showFromViewController:withContent:delegate: instead
    // + (instancetype)showWithContent:(FBSDKAppInviteContent *)content delegate:(id<FBSDKAppInviteDialogDelegate>)delegate __attribute__ ((deprecated("use showFromViewController:withContent:delegate: instead")));

  end;
  FBSDKAppInviteDialog = interface(NSObject)
  ['{F8BB14CF-103F-47C3-8508-C12FF1141630}']

    // A UIViewController to present the dialog from.
    // If not specified, the top most view controller will be automatically determined as best as possible.
    // @property (nonatomic, weak) UIViewController *fromViewController;
    function fromViewController: UIViewController; cdecl;
    procedure setFromViewController(fromViewController: UIViewController); cdecl;

    // The receiver's delegate or nil if it doesn't have a delegate.
    // @property (nonatomic, weak) id<FBSDKAppInviteDialogDelegate> delegate;

    // The content for app invite.
    // @property (nonatomic, copy) FBSDKAppInviteContent *content;
    function content: FBSDKAppInviteContent; cdecl;
    procedure setContent(content: FBSDKAppInviteContent); cdecl;

    // A Boolean value that indicates whether the receiver can initiate an app invite.
    // May return NO if the appropriate Facebook app is not installed and is required or an access token is
    // required but not available.  This method does not validate the content on the receiver, so this can be checked before
    // building up the content.
    //  - See:validateWithError:
    //  - Returns: YES if the receiver can show the dialog, otherwise NO.
    // - (BOOL)canShow;
    function canShow: Boolean; cdecl;

    // Begins the app invite from the receiver.
    //  - Returns: YES if the receiver was able to show the dialog, otherwise NO.
    // - (BOOL)show;
    function show: Boolean; cdecl;

    // Validates the content on the receiver.
    //  - Parameter errorRef: If an error occurs, upon return contains an NSError object that describes the problem.
    //  - Returns: YES if the content is valid, otherwise NO.
    // - (BOOL)validateWithError:(NSError *__autoreleasing *)errorRef;

  end;
  TFBSDKAppInviteDialog = class(TOCGenericImport<FBSDKAppInviteDialogClass, FBSDKAppInviteDialog>) end;

  {*************************************************************}
  // @protocol FBSDKSharingContent <FBSDKCopying, NSSecureCoding>
  // A base interface for content to be shared.
  FBSDKSharingContentClass = interface(NSObjectClass)
  ['{F888BB6E-227D-4790-8533-EEBFB1BB750A}']
  end;
  FBSDKSharingContent = interface(NSObject)
  ['{FF54B9AE-76F4-47E8-A0EE-4803D47E4D36}']

    // URL for the content being shared.
    // This URL will be checked for all link meta tags for linking in platform specific ways.  See documentation
    // for App Links (https:// developers.facebook.com/docs/applinks/)
    //  - Returns: URL representation of the content link
    // @property (nonatomic, copy) NSURL *contentURL;
    function contentURL: NSURL; cdecl;
    procedure setContentURL(contentURL: NSURL); cdecl;

    // Hashtag for the content being shared.
    //  - Returns: The hashtag for the content being shared.
    // @property (nonatomic, copy) FBSDKHashtag *hashtag;

    // List of IDs for taggable people to tag with this content.
    // See documentation for Taggable Friends
    // (https:// developers.facebook.com/docs/graph-api/reference/user/taggable_friends)
    //  - Returns: Array of IDs for people to tag (NSString)
    // @property (nonatomic, copy) NSArray *peopleIDs;

    // The ID for a place to tag with this content.
    //  - Returns: The ID for the place to tag
    // @property (nonatomic, copy) NSString *placeID;

    // A value to be added to the referrer URL when a person follows a link from this shared content on feed.
    // - Returns: The ref for the content.
    // @property (nonatomic, copy) NSString *ref;

  end;
  TFBSDKSharingContent = class(TOCGenericImport<FBSDKSharingContentClass, FBSDKSharingContent>) end;

  {*******************************************************************}
  // @interface FBSDKShareLinkContent : NSObject <FBSDKSharingContent>
  // A model for status and link content to be shared.
  FBSDKShareLinkContentClass = interface(FBSDKSharingContentClass)
  ['{CF0DB5C8-54C6-4200-A99D-E835DC0745B6}']
  end;
  FBSDKShareLinkContent = interface(FBSDKSharingContent)
  ['{E93992B4-0243-47AD-93C0-58B0A651E08F}']

    // The description of the link.
    // If not specified, this field is automatically populated by information scraped from the contentURL,
    // typically the title of the page.  This value may be discarded for specially handled links (ex: iTunes URLs).
    //  - Returns: The description of the link
    // @property (nonatomic, copy) NSString *contentDescription;
    function contentDescription: NSString; cdecl;
    procedure setContentDescription(contentDescription: NSString); cdecl;

    // The title to display for this link.
    //  This value may be discarded for specially handled links (ex: iTunes URLs).
    //  - Returns: The link title
    // @property (nonatomic, copy) NSString *contentTitle;
    function contentTitle: NSString; cdecl;
    procedure setContentTitle(contentTitle: NSString); cdecl;

    // The URL of a picture to attach to this content.
    //  - Returns: The network URL of an image
    // @property (nonatomic, copy) NSURL *imageURL;
    function imageURL: NSURL; cdecl;
    procedure setImageURL(imageURL: NSURL); cdecl;

    // Some quote text of the link.
    // If specified, the quote text will render with custom styling on top of the link.
    //  - Returns: The quote text of a link
    // @property (nonatomic, copy) NSString *quote;
    function quote: NSString; cdecl;
    procedure setQuote(quote: NSString); cdecl;

    // Compares the receiver to another link content.
    //  - Parameter content: The other content
    //  - Returns: YES if the receiver's values are equal to the other content's values; otherwise NO
    // - (BOOL)isEqualToShareLinkContent:(FBSDKShareLinkContent *)content;
    function isEqualToShareLinkContent(content: FBSDKShareLinkContent): Boolean; cdecl;

  end;
  TFBSDKShareLinkContent = class(TOCGenericImport<FBSDKShareLinkContentClass, FBSDKShareLinkContent>) end;

  {***********************************************************}
  // The common interface for components that initiate sharing.
  // - See:FBSDKShareDialog
  // - See:FBSDKMessageDialog
  // - See:FBSDKShareAPI
  // @protocol FBSDKSharing <NSObject>
  FBSDKSharingClass = interface(NSObjectClass)
  ['{3835CD8F-7649-4704-A9F3-C83F9BE02209}']
  end;
  FBSDKSharing = interface(NSObject)
  ['{B19665D8-B0CA-4E71-8D7D-F78436226F72}']

    // The receiver's delegate or nil if it doesn't have a delegate.
    // @property (nonatomic, weak) id<FBSDKSharingDelegate> delegate;

    // @property (nonatomic, copy) id<FBSDKSharingContent> shareContent;
    // The content to be shared.
    function shareContent: FBSDKSharingContent; cdecl;
    procedure setShareContent(shareContent: FBSDKSharingContent); cdecl;

    // A Boolean value that indicates whether the receiver should fail if it finds an error with the share content.
    // If NO, the sharer will still be displayed without the data that was mis-configured.  For example, an
    // invalid placeID specified on the shareContent would produce a data error.
    // @property (nonatomic, assign) BOOL shouldFailOnDataError;
    function shouldFailOnDataError: boolean; cdecl;
    procedure setShouldFailOnDataError(shouldFailOnDataError: boolean); cdecl;

    // Validates the content on the receiver.
    //  - Parameter errorRef: If an error occurs, upon return contains an NSError object that describes the problem.
    //  - Returns: YES if the content is valid, otherwise NO.
    // - (BOOL)validateWithError:(NSError **)errorRef;

  end;
  TFBSDKSharing = class(TOCGenericImport<FBSDKSharingClass, FBSDKSharing>) end;

  {********************************************************************}
  // @interface FBSDKSharingDialog : NSObject <FBSDKSharingDialogDialog>
  // A dialog for sharing content on Facebook.
  FBSDKSharingDialogClass = interface(FBSDKSharingClass)
  ['{CF17E43D-D280-468C-9732-54E39CD703D1}']
  end;
  FBSDKSharingDialog = interface(FBSDKSharing)
  ['{000A5A87-5204-4B32-BF78-3CF912582947}']

    // A Boolean value that indicates whether the receiver can initiate a share.
    // May return NO if the appropriate Facebook app is not installed and is required or an access token is
    // required but not available.  This method does not validate the content on the receiver, so this can be checked before
    // building up the content.
    //  - See:[FBSDKSharing validateWithError:]
    //  - Returns: YES if the receiver can share, otherwise NO.
    // - (BOOL)canShow;
    function canShow: Boolean; cdecl;

    // Shows the dialog.
    // - Returns: YES if the receiver was able to begin sharing, otherwise NO.
    // - (BOOL)show;
    function show: Boolean; cdecl;

  end;
  TFBSDKSharingDialog = class(TOCGenericImport<FBSDKSharingDialogClass, FBSDKSharingDialog>) end;

  {************************************************************}
  // @interface FBSDKShareDialog : NSObject <FBSDKSharingDialog>
  // A dialog for sharing content on Facebook.
  FBSDKShareDialogClass = interface(FBSDKSharingDialogClass)
  ['{9E1FD20C-C942-4DF1-8645-D705CCE3196C}']

    // Convenience method to show an FBSDKShareDialog with a fromViewController, content and a delegate.
    //  - Parameter viewController: A UIViewController to present the dialog from, if appropriate.
    //  - Parameter content: The content to be shared.
    //  - Parameter delegate: The receiver's delegate.
    // + (instancetype)showFromViewController:(UIViewController *)viewController
    //                            withContent:(id<FBSDKSharingContent>)content
    //                               delegate:(id<FBSDKSharingDelegate>)delegate;

  end;
  FBSDKShareDialog = interface(FBSDKSharingDialog)
  ['{8E175438-3671-43BE-A9EE-17F6F90AD10F}']

    // A UIViewController to present the dialog from.
    // If not specified, the top most view controller will be automatically determined as best as possible.
    // @property (nonatomic, weak) UIViewController *fromViewController;
    function fromViewController: UIViewController; cdecl;
    procedure setFromViewController(fromViewController: UIViewController); cdecl;

    // The mode with which to display the dialog.
    // @property (nonatomic, assign) FBSDKShareDialogMode mode;
    // Defaults to FBSDKShareDialogModeAutomatic, which will automatically choose the best available mode.

  end;
  TFBSDKShareDialog = class(TOCGenericImport<FBSDKShareDialogClass, FBSDKShareDialog>) end;


implementation

{$IF defined(CPUARM)}

procedure StubProc1; cdecl; external 'Bolts' name 'OBJC_CLASS_$_BFAppLink';
procedure StubProc2; cdecl; external 'FBSDKCoreKit' name 'OBJC_CLASS_$_FBSDKAccessToken';
procedure StubProc3; cdecl; external 'FBSDKShareKit' name 'OBJC_CLASS_$_FBSDKAppInviteContent';
procedure StubProc4; cdecl; external 'FBSDKShareKit' name 'OBJC_CLASS_$_FBSDKAppInviteDialog';
procedure StubProc5; cdecl; external 'FBSDKShareKit' name 'OBJC_CLASS_$_FBSDKSharingContent';
procedure StubProc6; cdecl; external 'FBSDKShareKit' name 'OBJC_CLASS_$_FBSDKShareLinkContent';
procedure StubProc7; cdecl; external 'FBSDKShareKit' name 'OBJC_CLASS_$_FBSDKSharing';
procedure StubProc8; cdecl; external 'FBSDKShareKit' name 'OBJC_CLASS_$_FBSDKSharingDialog';
procedure StubProc9; cdecl; external 'FBSDKShareKit' name 'OBJC_CLASS_$_FBSDKShareDialog';

{$ELSE}

// i don't know how to do under ios simulator :(

{$ENDIF}

end.
