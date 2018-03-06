{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2014-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Switch.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.TypInfo, System.SysUtils, System.Types, iOSapi.UIKit, iOSapi.Foundation, iOSapi.CocoaTypes, Macapi.Helpers,
  FMX.Types, FMX.Presentation.iOS, FMX.StdCtrls, FMX.Presentation.Messages, FMX.Controls.Presentation,
  FMX.Controls.Model;

type

{ TiOSNativeSwitch }

  IFMXUISwitch = interface(UISwitch)
    ['{B82F48FD-C9A1-46EC-AC6D-58DEEC7546F5}']
    { Touches }
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure ValueChanged; cdecl;
  end;

  TiOSNativeSwitch = class(TiOSNativeControl)
  private
    FProcessingTouches: Boolean;
    FOldValue: Boolean;
    function GetModel: TSwitchModel;
    procedure SetValue(const Value: Boolean);
    function GetValue: Boolean;
    function GetView: UISwitch;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
    function DefineModelClass: TDataModelClass; override;
  protected
    { Messages From Model}
    procedure MMValueChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message MM_VALUE_CHANGED;
    { Messages From Controller }
    procedure PMInit(var AMessage: TDispatchMessage); message PM_INIT;
    procedure PMGetAdjustSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_ADJUST_SIZE;
    procedure PMGetAdjustType(var AMessage: TDispatchMessageWithValue<TAdjustType>); message PM_GET_ADJUST_TYPE;
  public
    constructor Create; override;
    procedure ValueChanged; cdecl;
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    property Value: Boolean read GetValue write SetValue;
    property View: UISwitch read GetView;
    property Model: TSwitchModel read GetModel;
  end;

implementation

uses
  Macapi.ObjCRuntime, FMX.Consts, FMX.Controls, FMX.Presentation.Factory;

{ TiOSNativeSwitch }

constructor TiOSNativeSwitch.Create;
begin
  inherited;
  RegisterNativeEventHandler('ValueChanged', UIControlEventValueChanged);
end;

function TiOSNativeSwitch.DefineModelClass: TDataModelClass;
begin
  Result := TSwitchModel;
end;

function TiOSNativeSwitch.GetModel: TSwitchModel;
begin
  Result := inherited GetModel<TSwitchModel>;
end;

function TiOSNativeSwitch.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXUISwitch);
end;

function TiOSNativeSwitch.GetValue: Boolean;
begin
  Result := View.isOn;
end;

function TiOSNativeSwitch.GetView: UISwitch;
begin
  Result := inherited GetView<UISwitch>;
end;

procedure TiOSNativeSwitch.MMValueChanged(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  Value := AMessage.Value;
  Model.Change;
end;

procedure TiOSNativeSwitch.PMGetAdjustSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
  AMessage.Value := TSizeF.Create(View.frame.size.width, View.frame.size.height);
end;

procedure TiOSNativeSwitch.PMGetAdjustType(var AMessage: TDispatchMessageWithValue<TAdjustType>);
begin
  AMessage.Value := TAdjustType.FixedSize;
end;

procedure TiOSNativeSwitch.PMInit(var AMessage: TDispatchMessage);
begin
  Value := Model.Value;
end;

procedure TiOSNativeSwitch.SetValue(const Value: Boolean);
begin
  View.setOn(Value);
end;

procedure TiOSNativeSwitch.touchesBegan(touches: NSSet; withEvent: UIEvent);
begin
  inherited touchesBegan(touches, withEvent);
  FOldValue := Value;
  FProcessingTouches := True;
end;

procedure TiOSNativeSwitch.touchesCancelled(touches: NSSet; withEvent: UIEvent);
begin
  inherited touchesCancelled(touches, withEvent);
  FProcessingTouches := False;
  if FOldValue <> Value then
    ValueChanged;
end;

procedure TiOSNativeSwitch.touchesEnded(touches: NSSet; withEvent: UIEvent);
begin
  inherited touchesEnded(touches, withEvent);
  FProcessingTouches := False;
  if FOldValue <> Value then
    ValueChanged;
end;

procedure TiOSNativeSwitch.ValueChanged;
begin
  if not FProcessingTouches then
  begin
    Model.DisableNotify;
    try
      Model.Value := Value;
      Model.Change;
    finally
      Model.EnableNotify;
    end;
  end;
end;

initialization
  TPresentationProxyFactory.Current.Register(TSwitch, TControlType.Platform, TiOSPresentationProxy<TiOSNativeSwitch>);
finalization
  TPresentationProxyFactory.Current.Unregister(TSwitch, TControlType.Platform, TiOSPresentationProxy<TiOSNativeSwitch>);
end.
