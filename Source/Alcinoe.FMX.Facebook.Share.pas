(*******************************************************************************
When someone shares from your app, their content appears in their facebok
Timeline and in their friends' Feeds.

Setup (ANDROID)
---------------

https://developers.facebook.com/docs/sharing/android/

1) follow the step described in Alcinoe.FMX.Facebook.Core

2) On android you just need to include the library
     * io.magicfoundation.alcinoe:alcinoe-facebook-share:1.0.0
   in the project. You can do this with the help of AndroidMerger. You can see
   an exemple in <Alcinoe>\Demos\ALFacebookLogin\_source\android\MergeLibraries.bat

3) https://developers.facebook.com/docs/sharing/android/
   https://developers.facebook.com/docs/reference/android/current/class/FacebookContentProvider/
   Add a ContentProvider to your AndroidManifest.xml file and set {APP_ID} to
   your app ID:

    <provider android:authorities="com.facebook.app.FacebookContentProvider{APP_ID}"
              android:name="com.facebook.FacebookContentProvider"
              android:exported="true"/>

4) https://developers.facebook.com/docs/sharing/android/
   If your application targets Android 11 or later, add the following queries
   block to your AndroidManifest.xml file to make the Facebook App visible to
   your App:

    <queries>
      ...
      <provider android:authorities="com.facebook.katana.provider.PlatformProvider" />
      ...
    </queries>


Setup (IOS)
-----------

https://developers.facebook.com/docs/sharing/ios/

1) follow the step described in Alcinoe.FMX.Facebook.Core
*******************************************************************************)
unit Alcinoe.FMX.Facebook.Share;

interface

{$I Alcinoe.inc}

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFacebookShareDialog = class(TObject)
  private
  public
    class function ShowShareLinkDialog(const aLinkUrl: String): boolean;
  end;

implementation

uses
  system.SysUtils,
  {$IF defined(android)}
  Alcinoe.AndroidApi.Facebook,
  Androidapi.Helpers,
  Androidapi.JNI.app,
  Androidapi.JNI.Os,
  Androidapi.JNI.net,
  {$ELSEIF defined(IOS)}
  Macapi.Helpers,
  Alcinoe.iOSApi.FacebookShareKit,
  {$ENDIF}
  Alcinoe.FMX.Facebook.Core; // [MANDATORY] Because we need it's initialization/finalization section

{*****************************************************************************************}
class function TALFacebookShareDialog.ShowShareLinkDialog(const aLinkUrl: String): boolean;
begin

  ALInitFacebook;
  result := False;

  {$REGION 'ANDROID'}
  {$IF defined(android)}

  if TJALFacebookShareLinkDialog.javaclass.canshow then begin
    result := True;
    TJALFacebookShareLinkDialog.javaclass.Show(
      TAndroidHelper.Activity,
      StrToJURI(aLinkUrl), // contentUrl: Jnet_Uri;
      nil) //quote: JString
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  var LFBSDKShareLinkContent := TFBSDKShareLinkContent.create;
  try
    LFBSDKShareLinkContent.setContentURL(StrToNSUrl(aLinkUrl));
    var LFBSDKShareDialog := TFBSDKShareDialog.OCClass.dialogWithViewController(nil{SharedApplication.keyWindow.rootViewController},LFBSDKShareLinkContent,nil);
    try
      if LFBSDKShareDialog.canshow then
        LFBSDKShareDialog.show;
    finally
      //exception when we close the dialog if we release it here
      //LFBSDKShareDialog.release;
    end;
  finally
    LFBSDKShareLinkContent.release;
  end;
  {$ENDIF}
  {$ENDREGION}

end;

end.
