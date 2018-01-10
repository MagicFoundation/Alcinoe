{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit iOSapi.Helpers;

interface

uses
  iOSapi.UIKit, iOSapi.Foundation;

type

  TiOSHelper = class
  private
    class var FCachedScreen: UIScreen;
    class var FCachedDefaultNotificationCenter: NSNotificationCenter;
    class var FCachedCurrentDevice: UIDevice;
    class var FCachedBundle: NSBundle;
  public
    class function SharedApplication: UIApplication;
    class function MainScreen: UIScreen;
    class function MainBundle: NSBundle;
    class function DefaultNotificationCenter: NSNotificationCenter;
    class function CurrentDevice: UIDevice;
  end;

implementation

class function TiOSHelper.SharedApplication: UIApplication;
begin
  Result := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
end;


class function TiOSHelper.CurrentDevice: UIDevice;
begin
  if FCachedCurrentDevice = nil then
    FCachedCurrentDevice := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);
  Result := FCachedCurrentDevice;
end;

class function TiOSHelper.DefaultNotificationCenter: NSNotificationCenter;
begin
  if FCachedDefaultNotificationCenter = nil then
    FCachedDefaultNotificationCenter := TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter);
  Result := FCachedDefaultNotificationCenter;
end;

class function TiOSHelper.MainBundle: NSBundle;
begin
  if FCachedBundle = nil then
    FCachedBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
  Result := FCachedBundle
end;

class function TiOSHelper.MainScreen: UIScreen;
begin
  if FCachedScreen = nil then
    FCachedScreen := TUIScreen.Wrap(TUIScreen.OCClass.mainScreen);
  Result := FCachedScreen;
end;


end.
