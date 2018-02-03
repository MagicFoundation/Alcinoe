{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{ DUnit Test Unit                                                                                  }
{                                                                                                  }
{ Covers:      JclStrings                                                                          }
{ Last Update: $Date$                                }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{**************************************************************************************************}

unit TestJclNotify;

interface

uses
  TestFramework,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  Types,
  {$ENDIF}
  Classes,
  SysUtils,
  JclNotify;

type
  TJclNotifyBaseImplementations = class (TTestCase)
  private
    procedure CheckListener(listener: TObject; listenerID: string);
  published
    procedure _SingleNotifierAndListener;
    procedure _SingleNotifierAndMultipleFilteredListeners;
    procedure _SingleNotifierAndMultipleListeners;
    procedure _TwoNotifiersAndListeners;
    procedure _TwoNotifiersAndOneListener;
  end;

implementation

type
  IMessage1 = interface
    ['{AAA4DD0C-E8F5-445B-B543-42DE1F7D447D}']
    function GetID: string;
  end;

  IMessage2 = interface
    ['{6BA26CC5-6372-4D19-AB8B-8B5E12873854}']
    function GetID: string;
  end;

type
  TMessage1 = class (TJclBaseNotificationMessage, IMessage1)
  public
    constructor Create(id: string);
  private
    FID: string;
  protected
    function GetID: string;
  end;

  TMessage2 = class (TJclBaseNotificationMessage, IMessage2)
  public
    constructor Create(id: string);
  private
    FID: string;
  protected
    function GetID: string;
  end;

  TDualMessage = class (TJclBaseNotificationMessage, IMessage1, IMessage2)
  public
    constructor Create(id1, id2: string);
  private
    FID1: string;
    FID2: string;
  protected
    function IMessage1.GetID = GetID1;
    function GetID1: string;
    function IMessage2.GetID = GetID2;
    function GetID2: string;
  end;

type
  TAcceptType = (atNone, atMessage1, atMessage2, atEither, atBoth, atAny);
  TListener = class (TJclBaseListener)
  public
    constructor Create(accept: TAcceptType);
    destructor Destroy; override;
  private
    FAccept: TAcceptType;
    FTypes: TList;
    FIDs: TStrings;
    FCheckTypes: TList;
    FCheckIDs: TStrings;
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  public
    procedure AddCheck(msgType: TAcceptType; msgID: string);
  end;

function GetAcceptTypeName(ordinal: Integer): string;
begin
  case TAcceptType(ordinal) of
    atNone:     Result := 'atNone';
    atMessage1: Result := 'atMessage1';
    atMessage2: Result := 'atMessage2';
    atEither:   Result := 'atEither';
    atBoth:     Result := 'atBoth';
    atAny:      Result := 'atAny';
    else        Result := 'at' + IntToStr(ordinal) + '??';
  end;
end;

{ TJclNotifyBaseImplementations }

procedure TJclNotifyBaseImplementations.CheckListener(listener: TObject; listenerID: string);
var
  inst: TListener;
  idx: Integer;
begin
  inst := TListener(listener);
  CheckEquals(inst.FCheckTypes.Count, inst.FTypes.Count, listenerID + ' notification count');

  for idx := 0 to inst.FCheckTypes.Count - 1 do
  begin
    CheckEquals(
      GetAcceptTypeName(Integer(inst.FCheckTypes[idx])),
      GetAcceptTypeName(Integer(inst.FTypes[idx])),
      listenerID + ' notification type[' + IntToStr(idx) + ']'
    );

    CheckEquals(
      inst.FCheckIDs[idx],
      inst.FIDs[idx],
      listenerID + ' notification ID[' + IntToStr(idx) + ']'
    );
  end;
end;

procedure TJclNotifyBaseImplementations._SingleNotifierAndListener;
var
  notifier: IJclNotifier;
  listener: TListener;
begin
  notifier := TJclBaseNotifier.Create;
  listener := TListener.Create(atAny);
  try
    notifier.Add(listener);
  except
    FreeAndNil(listener);
    raise;
  end;

  // what should be there after the notifications have been processed.
  listener.AddCheck(atAny, '1');
  listener.AddCheck(atAny, '2');
  listener.AddCheck(atAny, '3');
  listener.AddCheck(atAny, '4');

  notifier.Notify(TMessage1.Create('0'));         
  notifier.Notify(TMessage2.Create('0'));
  notifier.Notify(TDualMessage.Create('0', '0'));
  notifier.Notify(TJclBaseNotificationMessage.Create);

  CheckListener(listener, 'Listener1');

  notifier := nil;  // will also release the listener
end;

procedure TJclNotifyBaseImplementations._SingleNotifierAndMultipleFilteredListeners;
var
  notifier: IJclNotifier;
  listener1: TListener;
  listener2: TListener;
begin
  notifier := TJclBaseNotifier.Create;
  listener1 := TListener.Create(atMessage1);
  try
    notifier.Add(listener1);
  except
    FreeAndNil(listener1);
    raise;
  end;

  listener2 := TListener.Create(atMessage2);
  try
    notifier.Add(listener2);
  except
    FreeAndNil(listener2);
    raise;
  end;

  // what should be there after the notifications have been processed.
  listener1.AddCheck(atMessage1, 'Msg1.1');
  listener1.AddCheck(atMessage1, 'Msg1.2');
  listener1.AddCheck(atMessage1, 'Msg1.3');

  listener2.AddCheck(atMessage2, 'Msg2.1');
  listener2.AddCheck(atMessage2, 'Msg2.2');
  listener2.AddCheck(atMessage2, 'Msg2.3');

  notifier.Notify(TMessage1.Create('Msg1.1'));              // only accepted by listener1
  notifier.Notify(TMessage2.Create('Msg2.1'));              // only accepted by listener2
  notifier.Notify(TDualMessage.Create('Msg1.2', 'Msg2.2')); // will be accepted by both listeners
  notifier.Notify(TJclBaseNotificationMessage.Create);      // will be ignored by both listeners
  notifier.Notify(TMessage2.Create('Msg2.3'));              // only accepted by listener2
  notifier.Notify(TMessage1.Create('Msg1.3'));              // only accepted by listener1

  CheckListener(listener1, 'Listener1');
  CheckListener(listener2, 'Listener2');

  notifier := nil;  // will also release the listeners
end;

procedure TJclNotifyBaseImplementations._SingleNotifierAndMultipleListeners;
var
  notifier: IJclNotifier;
  listener1: TListener;
  listener2: TListener;
begin
  notifier := TJclBaseNotifier.Create;
  listener1 := TListener.Create(atAny);
  try
    notifier.Add(listener1);
  except
    FreeAndNil(listener1);
    raise;
  end;

  listener2 := TListener.Create(atAny);
  try
    notifier.Add(listener2);
  except
    FreeAndNil(listener2);
    raise;
  end;

  // what should be there after the notifications have been processed.
  listener1.AddCheck(atAny, '1');
  listener1.AddCheck(atAny, '2');
  listener1.AddCheck(atAny, '3');
  listener1.AddCheck(atAny, '4');
  listener2.AddCheck(atAny, '1');
  listener2.AddCheck(atAny, '2');
  listener2.AddCheck(atAny, '3');
  listener2.AddCheck(atAny, '4');

  notifier.Notify(TMessage1.Create('0'));
  notifier.Notify(TMessage2.Create('0'));
  notifier.Notify(TDualMessage.Create('0', '0'));
  notifier.Notify(TJclBaseNotificationMessage.Create);

  CheckListener(listener1, 'Listener1');
  CheckListener(listener2, 'Listener2');

  notifier := nil;  // will also release the listeners
end;

procedure TJclNotifyBaseImplementations._TwoNotifiersAndListeners;
var
  notifier1: IJclNotifier;
  notifier2: IJclNotifier;
  listener1: TListener;
  listener2: TListener;
begin
  notifier1 := TJclBaseNotifier.Create;
  listener1 := TListener.Create(atEither);
  try
    notifier1.Add(listener1);
  except
    FreeAndNil(listener1);
    raise;
  end;

  notifier2 := TJclBaseNotifier.Create;
  listener2 := TListener.Create(atEither);
  try
    notifier2.Add(listener2);
  except
    FreeAndNil(listener2);
    raise;
  end;

  // what should be there after the notifications have been processed.
  listener1.AddCheck(atMessage1, 'Msg1.1');
  listener1.AddCheck(atMessage1, 'Msg1.2');
  listener1.AddCheck(atMessage2, 'Msg2.2');
  listener2.AddCheck(atMessage2, 'Msg2.1');
  listener2.AddCheck(atMessage1, 'Msg1.3');
  listener2.AddCheck(atMessage2, 'Msg2.3');

  notifier1.Notify(TMessage1.Create('Msg1.1'));
  notifier2.Notify(TMessage2.Create('Msg2.1'));
  notifier1.Notify(TDualMessage.Create('Msg1.2', 'Msg2.2'));
  notifier2.Notify(TDualMessage.Create('Msg1.3', 'Msg2.3'));
  notifier1.Notify(TJclBaseNotificationMessage.Create);

  CheckListener(listener1, 'Listener1');
  CheckListener(listener2, 'Listener2');
end;

procedure TJclNotifyBaseImplementations._TwoNotifiersAndOneListener;
var
  notifier1: IJclNotifier;
  notifier2: IJclNotifier;
  listener1: TListener;
begin
  notifier1 := TJclBaseNotifier.Create;
  listener1 := TListener.Create(atEither);
  try
    notifier1.Add(listener1);
  except
    FreeAndNil(listener1);
    raise;
  end;

  notifier2 := TJclBaseNotifier.Create;
  notifier2.Add(listener1);

  // what should be there after the notifications have been processed.
  listener1.AddCheck(atMessage1, 'Msg1.1');
  listener1.AddCheck(atMessage2, 'Msg2.1');
  listener1.AddCheck(atMessage1, 'Msg1.2');
  listener1.AddCheck(atMessage2, 'Msg2.2');
  listener1.AddCheck(atMessage1, 'Msg1.3');
  listener1.AddCheck(atMessage2, 'Msg2.3');

  notifier1.Notify(TMessage1.Create('Msg1.1'));
  notifier2.Notify(TMessage2.Create('Msg2.1'));
  notifier1.Notify(TDualMessage.Create('Msg1.2', 'Msg2.2'));
  notifier2.Notify(TDualMessage.Create('Msg1.3', 'Msg2.3'));
  notifier1.Notify(TJclBaseNotificationMessage.Create);

  CheckListener(listener1, 'Listener1');
end;

{ TMessage1 }

constructor TMessage1.Create(id: string);
begin
  inherited Create;
  FID := id;
end;

function TMessage1.GetID: string;
begin
  Result := FID;
end;

{ TMessage2 }

constructor TMessage2.Create(id: string);
begin
  inherited Create;
  FID := id;
end;

function TMessage2.GetID: string;
begin
  Result := FID;
end;

{ TDualMessage }

constructor TDualMessage.Create(id1, id2: string);
begin
  inherited Create;
  FID1 := id1;
  FID2 := id2;
end;

function TDualMessage.GetID1: string;
begin
  Result := FID1;
end;

function TDualMessage.GetID2: string;
begin
  Result := FID2;
end;

{ TListener }

constructor TListener.Create(accept: TAcceptType);
begin
  inherited Create;
  FAccept := accept;
  FTypes := TList.Create;
  FIDs := TStringList.Create;
  FCheckTypes := TList.Create;
  FCheckIDs := TStringList.Create;
end;

destructor TListener.Destroy;
begin
  FreeAndNil(FCheckIDs);
  FreeAndNil(FCheckTypes);
  FreeAndNil(FIDs);
  FreeAndNil(FTypes);
  inherited Destroy;
end;

procedure TListener.AddCheck(msgType: TAcceptType; msgID: string);
begin
  FCheckTypes.Add(Pointer(Ord(msgType)));
  FCheckIDs.Add(msgID);
end;

procedure TListener.Notification(msg: IJclNotificationMessage);
var
  msg1: IMessage1;
  msg2: IMessage2;
begin
  case FAccept of
    atNone:     ; // notification should be ignored; add nothing
    atMessage1:
      begin
        if Supports(msg, IMessage1, msg1) then
        begin
          FTypes.Add(Pointer(Ord(atMessage1)));
          FIDs.Add(msg1.GetID);
        end;
      end;
    atMessage2:
      begin
        if Supports(msg, IMessage2, msg2) then
        begin
          FTypes.Add(Pointer(Ord(atMessage2)));
          FIDs.Add(msg2.GetID);
        end;
      end;
    atEither:
      begin
        if Supports(msg, IMessage1, msg1) then
        begin
          FTypes.Add(Pointer(Ord(atMessage1)));
          FIDs.Add(msg1.GetID);
        end;
        if Supports(msg, IMessage2, msg2) then
        begin
          FTypes.Add(Pointer(Ord(atMessage2)));
          FIDs.Add(msg2.GetID);
        end;
      end;
    atBoth:
      begin
        if Supports(msg, IMessage1, msg1) then
        begin
          if Supports(msg, IMessage2, msg2) then
          begin
            FTypes.Add(Pointer(Ord(atBoth)));
            FIDs.Add(msg1.GetID + #1 + msg2.GetID);
          end;
        end;
      end;
    atAny:
      begin
        FTypes.Add(Pointer(Ord(atAny)));
        FIDs.Add(IntToStr(FTypes.Count));
      end;
  end;
end;

initialization
  RegisterTest('JCLNotify', TJclNotifyBaseImplementations.Suite);
  
end.
