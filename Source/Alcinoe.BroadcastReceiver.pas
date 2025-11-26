unit Alcinoe.BroadcastReceiver;

interface

{$I Alcinoe.inc}

{$IF defined(Android)}
uses
  System.Messaging,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Alcinoe.Androidapi.GraphicsContentViewText;
{$ENDIF}

Type

  {***********************************}
  TALBroadcastReceiver = Class(TObject)
  private
    class function CreateInstance: TALBroadcastReceiver;
    class function GetInstance: TALBroadcastReceiver; static;
  protected
    class var FInstance: TALBroadcastReceiver;
  public
    type
      TCreateInstanceFunc = function: TALBroadcastReceiver;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALBroadcastReceiver read GetInstance;
    class function HasInstance: Boolean; inline;
  public
    {$IF defined(Android)}
    type
      // -------------------------
      // TBroadcastReceivedMessage
      TBroadcastReceivedMessage  = class(TMessage<JIntent>)
      private
        FAction: String;
      public
        constructor Create(const AValue: JIntent; const AAction: String);
        property Action: String read FAction;
      end;
    {$ENDIF}
  private
    {$IF defined(Android)}
    type
      // --------------------------
      // TBroadcastReceiverListener
      TBroadcastReceiverListener = class(TJavaLocal, JALBroadcastReceiverListener)
      private
        FBroadcastReceiver: TALBroadcastReceiver;
      public
        constructor Create(const ABroadcastReceiver: TALBroadcastReceiver);
        procedure onReceive(context: JContext; intent: JIntent); cdecl;
      end;
    {$ENDIF}
  private
    {$IF defined(Android)}
    FBroadcastReceiverListener: TBroadcastReceiverListener;
    {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure deliverPendingBroadcasts;
  End;

implementation

uses
  System.Classes,
  {$IF defined(Android)}
  AndroidApi.Jni.JavaTypes,
  Androidapi.Helpers,
  {$ENDIF}
  Alcinoe.Common;

{********************}
{$IF defined(Android)}
constructor TALBroadcastReceiver.TBroadcastReceivedMessage.Create(const AValue: JIntent; const AAction: String);
begin
  inherited create(AValue);
  FAction := AAction;
end;
{$ENDIF}

{********************}
{$IF defined(Android)}
constructor TALBroadcastReceiver.TBroadcastReceiverListener.Create(const ABroadcastReceiver: TALBroadcastReceiver);
begin
  inherited create;
  FBroadcastReceiver := ABroadcastReceiver;
end;
{$ENDIF}

{********************}
{$IF defined(Android)}
procedure TALBroadcastReceiver.TBroadcastReceiverListener.onReceive(context: JContext; intent: JIntent);
begin
  {$IF defined(debug)}
  ALLog('TALBroadcastReceiver.TBroadcastReceiverListener.onReceive', JStringToString(intent.getAction));
  {$ENDIF}
  TMessageManager.DefaultManager.SendMessage(
    Self, // const Sender: TObject
    TALBroadcastReceiver.TBroadcastReceivedMessage.Create(intent, JStringToString(intent.getAction)), // AMessage: TMessage
    True); // ADispose: Boolean
end;
{$ENDIF}

{**************************************}
constructor TALBroadcastReceiver.Create;
begin
  inherited Create;
  {$IF defined(Android)}
  FBroadcastReceiverListener := TBroadcastReceiverListener.Create(self);
  TJALBroadcastReceiver.JavaClass.setListener(FBroadcastReceiverListener);
  {$ENDIF}
end;

{**************************************}
destructor TALBroadcastReceiver.Destroy;
begin
  {$IF defined(Android)}
  TJALBroadcastReceiver.JavaClass.setListener(nil);
  ALFreeAndNil(FBroadcastReceiverListener);
  {$ENDIF}
  inherited;
end;

{******************************************************}
procedure TALBroadcastReceiver.deliverPendingBroadcasts;
begin
  {$IF defined(Android)}
  TJALBroadcastReceiver.JavaClass.deliverPendingBroadcasts(TAndroidHelper.Context)
  {$ENDIF}
end;

{***********************************************************************}
class function TALBroadcastReceiver.CreateInstance: TALBroadcastReceiver;
begin
  result := TALBroadcastReceiver.Create;
end;

{*************}
//[MultiThread]
class function TALBroadcastReceiver.GetInstance: TALBroadcastReceiver;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
    else begin
      TThread.ForceQueue(nil,
        procedure begin
          If TALBroadcastReceiver.HasInstance then
            TALBroadcastReceiver.Instance.deliverPendingBroadcasts;
        end);
    end;
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALBroadcastReceiver.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.BroadcastReceiver','initialization');
  {$ENDIF}
  TALBroadcastReceiver.FInstance := nil;
  TALBroadcastReceiver.CreateInstanceFunc := @TALBroadcastReceiver.CreateInstance;

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.BroadcastReceiver','finalization');
  {$ENDIF}
  ALFreeAndNil(TALBroadcastReceiver.FInstance);

end.