{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclNotify.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel Bestebroer.                                 }
{ Portions created by Marcel Bestebroer are Copyright Marcel Bestebroer. All rights reserved.      }
{                                                                                                  }
{ Contributors:                                                                                    }
{   -                                                                                              }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains generic JCL notification/listener pattern interfaces and base implementations }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                        $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclNotify;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase,
  {$IFDEF THREADSAFE}
  JclSynch,
  {$ENDIF THREADSAFE}
  {$IFDEF HAS_UNITSCOPE}
  System.Classes;
  {$ELSE ~HAS_UNITSCOPE}
  Classes;
  {$ENDIF ~HAS_UNITSCOPE}

  { The following interfaces provide a basic notifier/listener setup. Whenever code issues a notification through the
    IJclNotifier.Notify method, all listeners registered with the notifier will receive the message (through the
    listener's Notification method). Since this setup doesn't care which or how many listeners are actually responding,
    it can greatly simplify code that need some form of notification. }
type
  // forward declarations
  IJclListener = interface;
  IJclNotificationMessage = interface;
  IJclNotifier = interface;

  IJclListener = interface
    ['{26A52ECC-4C22-4B71-BC88-D0EB98AF4ED5}']
    procedure Notification(msg: IJclNotificationMessage); stdcall;
  end;

  IJclNotificationMessage = interface
    ['{2618CCC6-0C7D-47EE-9A91-7A7F5264385D}']
  end;

  IJclNotifier = interface
    ['{CAAD7814-DD04-497C-91AC-558C2D5BFF81}']
    procedure Add(listener: IJclListener); stdcall;
    procedure Remove(listener: IJclListener); stdcall;
    procedure Notify(msg: IJclNotificationMessage); stdcall;
  end;

  { The following classes provide a basic notifier/listener implementation. Note that using one of these classes does
    not imply the usage of the related classes; the notifier can be used in conjection with any class implementing
    IJclListener and vice versa. }
type
  TJclBaseListener = class (TInterfacedObject, IJclListener)
  public
    { IJclListener }
    procedure Notification(msg: IJclNotificationMessage); virtual; stdcall;
  end;

  TJclBaseNotificationMessage = class (TInterfacedObject, IJclNotificationMessage)
  end;

  TJclBaseNotifier = class (TInterfacedObject, IJclNotifier)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FListeners: TInterfaceList;
    {$IFDEF THREADSAFE}
    FSynchronizer: TJclMultiReadExclusiveWrite;
    {$ENDIF THREADSAFE}
  public
    { IJclNotifier }
    procedure Add(listener: IJclListener); stdcall;
    procedure Notify(msg: IJclNotificationMessage); stdcall;
    procedure Remove(listener: IJclListener); stdcall;
  end;

type
  TJclMethodArray = array of TMethod;

  // base class for all object methods broadcasts
  TJclMethodBroadCast = class
  protected
    FHandlers: TJclMethodArray;
    FHandlerCount: Integer;
    function GetHandler(Index: Integer): TMethod;
  public
    function AddHandler(const AHandler: TMethod): Integer;
    procedure RemoveHandler(const AHandler: TMethod);
    procedure DeleteHandler(Index: Integer);
    property HandlerCount: Integer read FHandlerCount;
  end;

  // This class broadcasts a notification event to a list of handlers
  TJclNotifyEventBroadcast = class(TJclMethodBroadCast)
  protected
    function GetHandler(Index: Integer): TNotifyEvent;
  public
    function AddHandler(const AHandler: TNotifyEvent): Integer;
    procedure RemoveHandler(const AHandler: TNotifyEvent);
    procedure Notify(Sender: TObject);
    property Handlers[Index: Integer]: TNotifyEvent read GetHandler;
  end;

  TJclProcedureEvent = procedure of object;

  // This class broadcasts an event to a list of handlers
  TJclProcedureEventBroadcast = class(TJclMethodBroadCast)
  protected
    function GetHandler(Index: Integer): TJclProcedureEvent;
  public
    function AddHandler(const AHandler: TJclProcedureEvent): Integer;
    procedure RemoveHandler(const AHandler: TJclProcedureEvent);
    procedure CallAllProcedures;
    property Handlers[Index: Integer]: TJclProcedureEvent read GetHandler;
  end;

  TJclBooleanProcedureEvent = procedure(Value: Boolean) of object;

  // This class broadcasts an event to a list of handlers
  TJclBooleanProcedureEventBroadcast = class(TJclMethodBroadCast)
  protected
    function GetHandler(Index: Integer): TJclBooleanProcedureEvent;
  public
    function AddHandler(const AHandler: TJclBooleanProcedureEvent): Integer;
    procedure RemoveHandler(const AHandler: TJclBooleanProcedureEvent);
    procedure CallAllProcedures(Value: Boolean);
    property Handlers[Index: Integer]: TJclBooleanProcedureEvent read GetHandler;
  end;

  TJclBooleanEvent = function: Boolean of object;

  // This class broadcasts a predicate to a list of handlers
  TJclBooleanEventBroadcast = class(TJclMethodBroadCast)
  protected
    function GetHandler(Index: Integer): TJclBooleanEvent;
  public
    function AddHandler(const AHandler: TJclBooleanEvent): Integer;
    procedure RemoveHandler(const AHandler: TJclBooleanEvent);
    function LogicalAnd: Boolean;
    property Handlers[Index: Integer]: TJclBooleanEvent read GetHandler;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils;
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils;
  {$ENDIF ~HAS_UNITSCOPE}

//=== { TJclBaseNotifier } ===================================================

constructor TJclBaseNotifier.Create;
begin
  inherited Create;
  FListeners := TInterfaceList.Create;
  {$IFDEF THREADSAFE}
  FSynchronizer := TJclMultiReadExclusiveWrite.Create(mpReaders);
  {$ENDIF THREADSAFE}
end;

destructor TJclBaseNotifier.Destroy;
begin
  {$IFDEF THREADSAFE}
  FSynchronizer.BeginWrite;
  try
  {$ENDIF THREADSAFE}
  FreeAndNil(FListeners);
  {$IFDEF THREADSAFE}
  finally
    FSynchronizer.EndWrite;
    FreeAndNil(FSynchronizer);
  end;
  {$ENDIF THREADSAFE}
  inherited Destroy;
end;

procedure TJclBaseNotifier.Add(listener: IJclListener);
begin
  {$IFDEF THREADSAFE}
  FSynchronizer.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FListeners.IndexOf(listener) < 0 then
      FListeners.Add(listener);
  {$IFDEF THREADSAFE}
  finally
    FSynchronizer.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBaseNotifier.Notify(msg: IJclNotificationMessage);
var
  idx: Integer;
begin
  {$IFDEF THREADSAFE}
  FSynchronizer.BeginRead;
  try
  {$ENDIF THREADSAFE}
    for idx := 0 to FListeners.Count - 1 do
      IJclListener(FListeners[idx]).Notification(msg);
  {$IFDEF THREADSAFE}
  finally
    FSynchronizer.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBaseNotifier.Remove(listener: IJclListener);
var
  idx: Integer;
begin
  {$IFDEF THREADSAFE}
  FSynchronizer.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    idx := FListeners.IndexOf(listener);
    if idx >= 0 then
      FListeners.Delete(idx);
  {$IFDEF THREADSAFE}
  finally
    FSynchronizer.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclBaseListener } ===================================================

procedure TJclBaseListener.Notification(msg: IJclNotificationMessage);
begin
  // do nothing; descendants should override this method to process incoming notifications
end;

//=== { TNotifyEventBroadcast } ==============================================

function TJclMethodBroadcast.AddHandler(
  const AHandler: TMethod): Integer;
var
  HandlerLength: Integer;
begin
  HandlerLength := Length(FHandlers);
  if FHandlerCount >= HandlerLength then
  begin
    if HandlerLength > 0 then
      HandlerLength := HandlerLength * 2
    else
      HandlerLength := 4;
    SetLength(FHandlers, HandlerLength);
  end;
  Result := FHandlerCount;
  Inc(FHandlerCount);
  FHandlers[Result] := AHandler;
end;

procedure TJclMethodBroadcast.DeleteHandler(Index: Integer);
var
  I: Integer;
  HandlerLength: Integer;
begin
  for I := Index to FHandlerCount - 2 do
    FHandlers[I] := FHandlers[I + 1];

  HandlerLength := Length(FHandlers);
  Dec(FHandlerCount);
  if (FHandlerCount > 0) and ((2 * FHandlerCount) < HandlerLength) then
  begin
    HandlerLength := HandlerLength div 2;
    SetLength(FHandlers, HandlerLength);
  end;
end;

function TJclMethodBroadcast.GetHandler(Index: Integer): TMethod;
begin
  Result := FHandlers[Index];
end;

procedure TJclMethodBroadcast.RemoveHandler(const AHandler: TMethod);
var
  Index: Integer;
begin
  for Index := FHandlerCount - 1 downto 0 do
    if (TMethod(FHandlers[Index]).Code = TMethod(AHandler).Code) and
       (TMethod(FHandlers[Index]).Data = TMethod(AHandler).Data) then
      DeleteHandler(Index);
end;

//=== { TJclNotifyEventBroadcast } ===========================================

function TJclNotifyEventBroadcast.AddHandler(
  const AHandler: TNotifyEvent): Integer;
begin
  Result := inherited AddHandler(TMethod(AHandler));
end;

function TJclNotifyEventBroadcast.GetHandler(Index: Integer): TNotifyEvent;
begin
  Result := TNotifyEvent(inherited GetHandler(Index));
end;

procedure TJclNotifyEventBroadcast.Notify(Sender: TObject);
var
  Index: Integer;
begin
  for Index := 0 to FHandlerCount - 1 do
    TNotifyEvent(FHandlers[Index])(Sender);
end;

procedure TJclNotifyEventBroadcast.RemoveHandler(const AHandler: TNotifyEvent);
begin
  inherited RemoveHandler(TMethod(AHandler));
end;

//=== { TJclProcedureBroadcast } =============================================

function TJclProcedureEventBroadcast.AddHandler(
  const AHandler: TJclProcedureEvent): Integer;
begin
  Result := inherited AddHandler(TMethod(AHandler));
end;

function TJclProcedureEventBroadcast.GetHandler(Index: Integer): TJclProcedureEvent;
begin
  Result := TJclProcedureEvent(inherited GetHandler(Index));
end;

procedure TJclProcedureEventBroadcast.CallAllProcedures;
var
  Index: Integer;
begin
  for Index := 0 to FHandlerCount - 1 do
    TJclProcedureEvent(FHandlers[Index]);
end;

procedure TJclProcedureEventBroadcast.RemoveHandler(const AHandler: TJclProcedureEvent);
begin
  inherited RemoveHandler(TMethod(AHandler));
end;

//=== { TJclBooleanProcedureBroadcast } =============================================

function TJclBooleanProcedureEventBroadcast.AddHandler(
  const AHandler: TJclBooleanProcedureEvent): Integer;
begin
  Result := inherited AddHandler(TMethod(AHandler));
end;

function TJclBooleanProcedureEventBroadcast.GetHandler(Index: Integer): TJclBooleanProcedureEvent;
begin
  Result := TJclBooleanProcedureEvent(inherited GetHandler(Index));
end;

procedure TJclBooleanProcedureEventBroadcast.CallAllProcedures(Value: Boolean);
var
  Index: Integer;
begin
  for Index := 0 to FHandlerCount - 1 do
    TJclBooleanProcedureEvent(FHandlers[Index])(Value);
end;

procedure TJclBooleanProcedureEventBroadcast.RemoveHandler(const AHandler: TJclBooleanProcedureEvent);
begin
  inherited RemoveHandler(TMethod(AHandler));
end;

//=== { TJclBooleanEventBroadcast } ==========================================

function TJclBooleanEventBroadcast.AddHandler(
  const AHandler: TJclBooleanEvent): Integer;
begin
  Result := inherited AddHandler(TMethod(AHandler));
end;

function TJclBooleanEventBroadcast.GetHandler(Index: Integer): TJclBooleanEvent;
begin
  Result := TJclBooleanEvent(inherited GetHandler(Index));
end;

function TJclBooleanEventBroadcast.LogicalAnd: Boolean;
var
  Index: Integer;
begin
  Result := True;
  for Index := 0 to FHandlerCount - 1 do
  begin
    Result := TJclBooleanEvent(FHandlers[Index]);
    if not Result then
      Break;
  end;
end;

procedure TJclBooleanEventBroadcast.RemoveHandler(
  const AHandler: TJclBooleanEvent);
begin
  inherited RemoveHandler(TMethod(AHandler));
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
