//
// Made from Facebook SDK version 15.1.0
//
unit Alcinoe.iOSApi.FacebookShareKit;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
  //Pleast update <Alcinoe>\Libraries\ios\facebook\ to the last one and then run
  //<Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorIOS.bat
  //and gave the path to <Alcinoe>\Source\Alcinoe.iOSApi.FacebookShareKit.pas to build
  //the compare source file. Then make a diff compare between the new generated
  //Alcinoe.iOSApi.FacebookShareKit.pas and this one to see if the api signature is
  //still the same
  {$MESSAGE WARN 'Check if the api signature of the last version of facebook ShareKit (ios) is still the same'}
{$ENDIF}

uses
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.UIKit;

{$M+}

type

  {*****************************************************************************************************************}
  //https://developers.facebook.com/docs/reference/iossdk/current/FBSDKShareKit/protocols/fbsdksharingvalidation.html
  //@protocol FBSDKSharingValidatable
  FBSDKSharingValidatableClass = interface(NSObjectClass)
    ['{FF27BA4C-2E03-4A27-9D0F-0344E50FAF8B}']
  end;
  FBSDKSharingValidatable = interface(NSObject)
    ['{1B99AA0B-FBBC-4D10-9BB9-3E940BEB43CF}']
  end;
  TFBSDKSharingValidatable = class(TOCGenericImport<FBSDKSharingValidatableClass, FBSDKSharingValidatable>) end;

  {**************************************************************************************************************}
  //https://developers.facebook.com/docs/reference/iossdk/current/FBSDKShareKit/protocols/fbsdksharingcontent.html
  //@protocol FBSDKSharingContent <FBSDKSharingValidatable, NSObject>
  FBSDKSharingContentClass = interface(FBSDKSharingValidatableClass)
    ['{6D37E12D-812B-443D-86D5-394986F61A24}']
  end;
  FBSDKSharingContent = interface(FBSDKSharingValidatable)
    ['{E1FD9F85-E3D2-4741-9AB4-8EC1E959D99C}']
    procedure setContentURL(contentURL: NSURL); cdecl;
    function contentURL: NSURL; cdecl;
  end;
  TFBSDKSharingContent = class(TOCGenericImport<FBSDKSharingContentClass, FBSDKSharingContent>) end;

  {**************************************************************************************************************}
  //https://developers.facebook.com/docs/reference/iossdk/current/FBSDKShareKit/classes/fbsdksharelinkcontent.html
  //@interface FBSDKShareLinkContent : NSObject
  //@interface FBSDKShareLinkContent (SWIFT_EXTENSION(FBSDKShareKit)) <FBSDKSharingContent>
  //@interface FBSDKShareLinkContent (SWIFT_EXTENSION(FBSDKShareKit)) <FBSDKSharingValidatable>
  FBSDKShareLinkContentClass = interface(FBSDKSharingContentClass)
    ['{D1FD77D5-FB85-4D6D-AE16-C90533BC1764}']
  end;
  FBSDKShareLinkContent = interface(FBSDKSharingContent)
    ['{28F34CBE-BF1D-4F66-8D32-B70CC714B073}']
    procedure setQuote(quote: NSString); cdecl;
    function quote : NSString; cdecl;
  end;
  TFBSDKShareLinkContent = class(TOCGenericImport<FBSDKShareLinkContentClass, FBSDKShareLinkContent>) end;

  {***************************************************************************************************************}
  //https://developers.facebook.com/docs/reference/iossdk/current/FBSDKShareKit/protocols/fbsdksharingdelegate.html
  //@protocol FBSDKSharingDelegate
  FBSDKSharingDelegate = interface(IObjectiveC)
    ['{F9619273-B675-436E-ADB2-95F4821E69C0}']
  end;

  {*******************************************************************************************************}
  //https://developers.facebook.com/docs/reference/iossdk/current/FBSDKShareKit/protocols/fbsdksharing.html
  //@protocol FBSDKSharing
  FBSDKSharingClass = interface(NSObjectClass)
    ['{D36FCACA-705A-4E7E-8C13-86CD1445A227}']
  end;
  FBSDKSharing = interface(NSObject)
    ['{30459660-B196-4B8F-82FE-B3E802ED9FF7}']
  end;
  TFBSDKSharing = class(TOCGenericImport<FBSDKSharingClass, FBSDKSharing>) end;

  {*************************************************************************************************************}
  //https://developers.facebook.com/docs/reference/iossdk/current/FBSDKShareKit/protocols/fbsdksharingdialog.html
  //@protocol FBSDKSharingDialog <FBSDKSharing>
  FBSDKSharingDialogClass = interface(FBSDKSharingClass)
    ['{8ABF0432-5DC5-40DB-9DDE-776A042C0AD9}']
  end;
  FBSDKSharingDialog = interface(FBSDKSharing)
    ['{A4F2AC86-2363-4588-8579-BF76AEBC4643}']
    function canShow: Boolean; cdecl;
    function show: Boolean; cdecl;
  end;
  TFBSDKSharingDialog = class(TOCGenericImport<FBSDKSharingDialogClass, FBSDKSharingDialog>) end;

  {***************************}
  FBSDKShareDialog = interface;

  {*********************************************************************************************************}
  //https://developers.facebook.com/docs/reference/iossdk/current/FBSDKShareKit/classes/fbsdksharedialog.html
  //@interface FBSDKShareDialog : NSObject <FBSDKSharingDialog>
  FBSDKShareDialogClass = interface(FBSDKSharingDialogClass)
    ['{3E8B37A4-184F-41B0-BD6C-8248074AE04B}']
    {class} function dialogWithViewController(viewController: UIViewController; withContent: FBSDKSharingContent; delegate: FBSDKSharingDelegate) : FBSDKShareDialog; cdecl;
  end;
  FBSDKShareDialog = interface(FBSDKSharingDialog)
    ['{00AAE9F3-E841-49B7-B8AA-949CD3447E02}']
  end;
  TFBSDKShareDialog = class(TOCGenericImport<FBSDKShareDialogClass, FBSDKShareDialog>) end;

implementation

uses
  Alcinoe.iOSApi.FacebookCoreKit; // [MANDATORY] Because we need it's initialization/finalization section

{***********************************************************************}
procedure FBSDKShareKitLoader; cdecl; external framework 'FBSDKShareKit';

end.
