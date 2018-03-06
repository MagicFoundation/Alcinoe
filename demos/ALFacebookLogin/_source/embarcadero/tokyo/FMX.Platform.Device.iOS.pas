{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Device.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.Devices, FMX.Platform, FMX.Types, FMX.Forms;

type

  /// <summary>Implementation of <c>IFMXDeviceService</c> for iOS</summary>
  TCocoaTouchDeviceServices = class(TInterfacedObject, IFMXDeviceService)
  protected
    /// <summary>Register service <c>IFMXDeviceService</c> implementation</summary>
    procedure RegisterService; virtual;
    /// <summary>Unregister <c>IFMXDeviceService</c> implementation</summary>
    procedure UnregisterService; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXDeviceService }
    /// <summary>Returns string presentation of device model name</summary>
    function GetModel: string;
    /// <summary>Returns set of device's features</summary>
    function GetFeatures: TDeviceFeatures;
    /// <summary>Returns class of current device</summary>
    function GetDeviceClass: TDeviceInfo.TDeviceClass;
  end;

implementation

uses
  System.Types, Macapi.Helpers, iOSapi.UIKit;

{ TCocoaDeviceServices }

constructor TCocoaTouchDeviceServices.Create;
begin
  inherited;
  RegisterService;
end;

destructor TCocoaTouchDeviceServices.Destroy;
begin
  UnregisterService;
  inherited;
end;

function TCocoaTouchDeviceServices.GetDeviceClass: TDeviceInfo.TDeviceClass;
begin
  if TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).userInterfaceIdiom = UIUserInterfaceIdiomPad then
    Result := TDeviceInfo.TDeviceClass.Tablet
  else
    Result := TDeviceInfo.TDeviceClass.Phone;
end;

function TCocoaTouchDeviceServices.GetFeatures: TDeviceFeatures;
begin
  Result := [TDeviceFeature.HasTouchScreen];
end;

function TCocoaTouchDeviceServices.GetModel: string;
begin
  Result := NSStrToStr(TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).model);
end;

procedure TCocoaTouchDeviceServices.RegisterService;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXDeviceService) then
    TPlatformServices.Current.AddPlatformService(IFMXDeviceService, Self);
end;

procedure TCocoaTouchDeviceServices.UnregisterService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXDeviceService);
end;

end.
