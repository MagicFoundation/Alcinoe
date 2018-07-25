{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Messaging;

{$MINENUMSIZE 4}
{$H+}

interface

uses
{$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText,
{$ENDIF ANDROID}
{$IFDEF IOS}
  iOSapi.UIKit,
{$ENDIF IOS}
  System.SysUtils, System.Generics.Collections;

type

  /// <summary>Base class for all messages</summary>
  TMessageBase = class abstract;
  TMessage = TMessageBase;
  {$NODEFINE TMessage} // Avoid ambiguity with 'Winapi.Messages.TMessage'

  TMessage<T> = class (TMessage)
  protected
    FValue: T;
  public
    constructor Create(const AValue: T);
    destructor Destroy; override;
    property Value: T read FValue;
  end;

  TObjectMessage<T: class> = class(TMessage<T>)
  protected
    FOwnsObject: Boolean;
  public
    constructor Create(const AValue: T; AOwnsObject: Boolean = True);
    destructor Destroy; override;
  end;

{$IFDEF IOS}
  /// <summary>Received Notification type for NotificationCenter.</summary>
  TMessageReceivedNotification = class(TMessage<UILocalNotification>);
{$ENDIF IOS}

{$IFDEF ANDROID}
  /// <summary>Received Notification type for NotificationCenter.</summary>
  TMessageReceivedNotification = class(TMessage<JIntent>);
{$ENDIF ANDROID}

  TMessageListener = reference to procedure(const Sender: TObject; const M: TMessage);
  TMessageListenerMethod = procedure (const Sender: TObject; const M: TMessage) of object;

  { TMessageManager can have many independent instances, but it
    maintains one global instance accessible by TMessageManager.DefaultManager }
  TMessageManager = class
  protected
  type
    TListenerWithId = record
      Id: Integer;
      Listener: TMessageListener;
      ListenerMethod: TMessageListenerMethod;
    end;
    PListenerWithId = ^TListenerWithId;
    TListenerList = class(TList<TListenerWithId>)
    strict private
      FProcessing: Integer;

      procedure IterateAndSend(const Sender: TObject; const AMessage: TMessage);
      procedure Compact;
    private
      FRemoveCount: Integer;

      procedure Unsubscribe(Index: Integer; Immediate: Boolean); inline;
      procedure SendMessage(const Sender: TObject; const AMessage: TMessage); inline;
      class procedure InternalCopyListener(FromListener, ToListener: PListenerWithId); inline;
    end;
    TListenerRegistry = TObjectDictionary<TClass, TListenerList>;
  private
    FListeners: TListenerRegistry;
    FLastId: Integer;
    procedure RegisterMessageClass(const AMessageClass: TClass);

    { Global instance }
    class var FDefaultManager: TMessageManager;
    class function GetDefaultManager: TMessageManager; static;
    class function SearchListener(const ArrayToSearch: array of TListenerWithId; Id: Integer; AMinValue, AMaxValue: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    class destructor UnInitialize;
    function SubscribeToMessage(const AMessageClass: TClass; const AListener: TMessageListener): Integer; overload;
    function SubscribeToMessage(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod): Integer; overload;
    procedure Unsubscribe(const AMessageClass: TClass; Id: Integer; Immediate: Boolean = False); overload;
    procedure Unsubscribe(const AMessageClass: TClass; const AListener: TMessageListener; Immediate: Boolean = False); overload;
    procedure Unsubscribe(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod; Immediate: Boolean = False); overload;
    procedure SendMessage(const Sender: TObject; AMessage: TMessage); overload; inline;
    procedure SendMessage(const Sender: TObject; AMessage: TMessage; ADispose: Boolean); overload;
    class property DefaultManager: TMessageManager read GetDefaultManager;
  end;

implementation

uses System.Types, System.RTLConsts;

{ TMessageManager }

constructor TMessageManager.Create;
begin
  FListeners := TListenerRegistry.Create([doOwnsValues]);
  FLastId := 1;
end;

destructor TMessageManager.Destroy;
begin
  FListeners.Free;
  inherited;
end;

class function TMessageManager.GetDefaultManager: TMessageManager;
begin
  if FDefaultManager = nil then
    FDefaultManager := TMessageManager.Create;

  Result := FDefaultManager;
end;

class destructor TMessageManager.UnInitialize;
begin
  FreeAndNil(FDefaultManager);
end;

procedure TMessageManager.RegisterMessageClass(const AMessageClass: TClass);
begin
  if not FListeners.ContainsKey(AMessageClass) then
    FListeners.Add(AMessageClass, TListenerList.Create);
end;

function TMessageManager.SubscribeToMessage(const AMessageClass: TClass; const AListener: TMessageListener) : Integer;
var
  L: TListenerWithId;
  Subscribers: TListenerList;
begin
  Result := -1;
  RegisterMessageClass(AMessageClass);
  if FListeners.TryGetValue(AMessageClass, Subscribers) then
  begin
    L.Listener := AListener;
    L.ListenerMethod := nil;
    Inc(FLastId);
    L.Id := FLastId;
    Result := L.Id;
    Subscribers.Add(L);
  end;
end;

function TMessageManager.SubscribeToMessage(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod): Integer;
var
  L: TListenerWithId;
  Subscribers: TListenerList;
begin
  Result := -1;
  RegisterMessageClass(AMessageClass);
  if FListeners.TryGetValue(AMessageClass, Subscribers) then
  begin
    L.Listener := nil;
    L.ListenerMethod := AListenerMethod;
    Inc(FLastId);
    L.Id := FLastId;
    Result := L.Id;
    Subscribers.Add(L);
  end;
end;


procedure TMessageManager.Unsubscribe(const AMessageClass: TClass; const AListener: TMessageListener; Immediate: Boolean);
var
  Subscribers: TListenerList;
  I: Integer;
begin
  if FListeners.TryGetValue(AMessageClass, Subscribers) then
    for I := 0 to Subscribers.Count - 1 do
      if Pointer((@Subscribers.List[I].Listener)^) = Pointer((@AListener)^) then
      begin
        Subscribers.Unsubscribe(I,Immediate);
        Break;
      end;
end;

procedure TMessageManager.Unsubscribe(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod; Immediate: Boolean);
var
  Subscribers: TListenerList;
  I: Integer;
begin
  if FListeners.TryGetValue(AMessageClass, Subscribers) then
    for I := 0 to Subscribers.Count - 1 do
      if TMethod(Subscribers[I].ListenerMethod) = TMethod(AListenerMethod) then
      begin
        Subscribers.Unsubscribe(I,Immediate);
        break;
      end;
end;

procedure TMessageManager.Unsubscribe(const AMessageClass: TClass; Id: Integer; Immediate: Boolean);
var
  Index: Integer;
  Subscribers: TListenerList;
begin
  if FListeners.TryGetValue(AMessageClass, Subscribers) then
  begin
    Index := SearchListener(Subscribers.List, Id, 0, Subscribers.Count - 1);
    if Index >= 0 then
      Subscribers.Unsubscribe(Index, Immediate);
  end;
end;


procedure TMessageManager.SendMessage(const Sender: TObject; AMessage: TMessage; ADispose: Boolean);
var
  Subscribers: TListenerList;
begin
  if AMessage <> nil then
    try
      if FListeners.TryGetValue(AMessage.ClassType, Subscribers) then
        Subscribers.SendMessage(Sender, AMessage);

    finally
      if ADispose then
        AMessage.Free;
    end
  else
    raise Exception.CreateRes(@SArgumentInvalid);
end;

procedure TMessageManager.SendMessage(const Sender: TObject; AMessage: TMessage);
begin
  SendMessage(Sender, AMessage, True);
end;

class function TMessageManager.SearchListener(const ArrayToSearch: array of TListenerWithId; Id: Integer; AMinValue, AMaxValue: Integer): Integer;
var
  IMin, IMid, IMax: Integer;
begin
  if (AMaxValue < AMinValue) then
    Exit(-1);
  IMin := AMinValue;
  IMax := AMaxValue;

  while IMax >= IMin do
  begin
    IMid := (IMax + IMin) shr 1;
    if ArrayToSearch[IMid].Id < Id then
    begin
      IMin := IMid + 1;
    end
    else
      if ArrayToSearch[IMid].Id > Id then
        IMax := IMid - 1
      else
        Exit(IMid);
  end;
  Result := -1;
end;

{ TMessage<T> }

constructor TMessage<T>.Create(const AValue: T);
begin
  FValue := AValue;
end;

destructor TMessage<T>.Destroy;
begin
  inherited;  {C++: Anchor Generic destructor}
end;

{ TObjectMessage<T> }

constructor TObjectMessage<T>.Create(const AValue: T; AOwnsObject: Boolean);
begin
  inherited Create(AValue);
  FOwnsObject := AOwnsObject;
end;

destructor TObjectMessage<T>.Destroy;
begin
  if FOwnsObject then
    FValue.DisposeOf;
  inherited Destroy;
end;

{ TMessageManager.TListenerList }

class procedure TMessageManager.TListenerList.InternalCopyListener(FromListener, ToListener: PListenerWithId);
begin
  ToListener.Id := FromListener.Id;
  ToListener.Listener := FromListener.Listener;
  ToListener.ListenerMethod := FromListener.ListenerMethod;
end;

procedure TMessageManager.TListenerList.IterateAndSend(const Sender: TObject;
  const AMessage: TMessage);
var
  I: Integer;
  Listener: PListenerWithId;
begin
  for I := 0 to Count - 1 do
  begin
    Listener := @List[I];
    if Assigned(Listener.Listener) then
      Listener.Listener(Sender, AMessage)
    else
      if Assigned(Listener.ListenerMethod) then
        TMessageListenerMethod(Listener.ListenerMethod)(Sender, AMessage);
  end;
end;

procedure TMessageManager.TListenerList.SendMessage(const Sender: TObject;
  const AMessage: TMessage);
begin
  if (FProcessing = 0) and (FRemoveCount > 0) and (((FRemoveCount * 100) div Count) > 10) then
    Compact;
  Inc(FProcessing);
  try
    IterateAndSend(Sender, AMessage);
  finally
    Dec(FProcessing);
  end;
end;

procedure TMessageManager.TListenerList.Unsubscribe(Index: Integer;
  Immediate: Boolean);
begin
  if FProcessing > 0 then
  begin
    // Recursive call, no compacting should be performed
    List[Index].Listener := nil;
    List[Index].ListenerMethod := nil;
    Inc(FRemoveCount);
  end
  else
  begin
    if Immediate then
      Delete(Index)
    else
    begin
      List[Index].Listener := nil;
      List[Index].ListenerMethod := nil;
      Inc(FRemoveCount);
      if (FRemoveCount shl 1) > (Count + 4) then
        Compact;
    end;
  end;
end;

procedure TMessageManager.TListenerList.Compact;
var
  I, N: Integer;
  Listener: PListenerWithId;
begin
  N := 0;
  FRemoveCount := 0;
  for I := 0 to Count - 1 do
  begin
    Listener := @List[I];
    if Assigned(Listener.Listener) or Assigned(Listener.ListenerMethod) then
    begin
      if N <> I then
        InternalCopyListener(Listener, @List[N]);
      Inc(N);
    end;
  end;
  Count := N;
end;

end.

