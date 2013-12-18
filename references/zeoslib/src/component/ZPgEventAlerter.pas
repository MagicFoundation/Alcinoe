{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{    Copyright (c) 1999-2003 Zeos Development Group       }
{            Written by Sergey Merkuriev                  }
{                                                         }
{*********************************************************}

{@********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

{*********************************************************}
{                                                         }
{ TZPgEventAlerter, Asynchronous notifying.               }
{   By Ivan Rog - 2010                                    }
{                                                         }
{ Contributors:                                           }
{   Silvio Clecio - http://silvioprog.com.br              }
{                                                         }
{*********************************************************}

unit ZPgEventAlerter;

interface

uses
  SysUtils, Classes, ExtCtrls,
{$IFNDEF UNIX} 
  Windows,
{$ELSE} 
  {$IFNDEF FPC} 
    libc, Math,
  {$ENDIF} 
{$ENDIF}
  ZDbcPostgreSql, ZPlainPostgreSqlDriver, ZConnection, ZAbstractRODataset,
  ZDataset;

type
  TZPgNotifyEvent = procedure(Sender: TObject; Event: string;
    ProcessID: Integer) of object;

  { TZPgEventAlerter }

  TZPgEventAlerter = class (TComponent)
  private
    FActive      : Boolean;
    FEvents      : TStrings;
    FTimer       : TTimer;
    FQueryRefresh: TZReadOnlyQuery;
    FConnection: TZConnection;
    FNotifyFired : TZPgNotifyEvent;
  protected
    procedure SetActive     (Value: Boolean);
    function  GetInterval   : Cardinal;
    procedure SetInterval   (Value: Cardinal);
    procedure SetEvents     (Value: TStrings);
    procedure SetConnection (Value: TZConnection);
    procedure TimerTick     (Sender: TObject);
    procedure CheckEvents;
    procedure OpenNotify;
    procedure CloseNotify;
  public
    constructor Create     (AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property Connection: TZConnection     read FConnection   write SetConnection;
    property Active:     Boolean          read FActive       write SetActive;
    property Events:     TStrings         read FEvents       write SetEvents;
    property Interval:   Cardinal         read GetInterval   write SetInterval    default 250;
    property OnNotify:   TZPgNotifyEvent  read FNotifyFired  write FNotifyFired;
  end;

implementation

{ TZPgEventAlerter }

constructor TZPgEventAlerter.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  FQueryRefresh := TZReadOnlyQuery.Create(nil);
  FEvents := TStringList.Create;
  with TStringList(FEvents) do
    Duplicates := dupIgnore;
  FTimer         := TTimer.Create(Self);
  FTimer.Enabled := False;
  SetInterval(250);
  FTimer.OnTimer := TimerTick;
  FActive        := False;
  if (csDesigning in ComponentState) and Assigned(AOwner) then
   for I := AOwner.ComponentCount - 1 downto 0 do
    if AOwner.Components[I] is TZConnection then
     begin
        FConnection := AOwner.Components[I] as TZConnection;
      Break;
     end;
end;

destructor TZPgEventAlerter.Destroy;
begin
  CloseNotify;
  FEvents.Free;
  FTimer.Free;
  FQueryRefresh.Free;
  inherited Destroy;
end;

procedure TZPgEventAlerter.SetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

function TZPgEventAlerter.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TZPgEventAlerter.SetEvents(Value: TStrings);
var
  I: Integer;
begin
  FEvents.Assign(Value);
  for I := 0 to FEvents.Count -1 do
    FEvents[I] := Trim(FEvents[I]);
end;

procedure TZPgEventAlerter.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      OpenNotify
    else
      CloseNotify;
  end;
end;

procedure TZPgEventAlerter.SetConnection(Value: TZConnection);
begin
  if FConnection <> Value then
  begin
    CloseNotify;
    FConnection := Value;
    FQueryRefresh.Connection := Value;
  end;
end;

procedure TZPgEventAlerter.TimerTick(Sender: TObject);
begin
  if not FActive then
   FTimer.Enabled := False
  else
   CheckEvents;
end;

procedure TZPgEventAlerter.OpenNotify;
var
  I        : Integer;
  Tmp      : array [0..255] of AnsiChar;
  Handle   : PZPostgreSQLConnect;
  ICon     : IZPostgreSQLConnection;
  PlainDRV : IZPostgreSQLPlainDriver;
  Res: PGresult;
begin
  if not Boolean(Pos('postgresql', FConnection.Protocol)) then
    raise EZDatabaseError.Create('Ivalid connection protocol. Need <postgres>, get ' +
      FConnection.Protocol + '.');
  if FActive then
    Exit;
  if not Assigned(FConnection) then
    Exit;
  if ((csLoading in ComponentState) or (csDesigning in ComponentState)) then
    Exit;
  if not FConnection.Connected then
    Exit;
  ICon     := (FConnection.DbcConnection as IZPostgreSQLConnection);
  Handle   := ICon.GetConnectionHandle;
  PlainDRV := ICon.GetPlainDriver;
  if Handle = nil then
    Exit;
  for I := 0 to FEvents.Count-1 do
  begin
    StrPCopy(Tmp, 'listen ' + FEvents.Strings[I]);
    Res := PlainDRV.ExecuteQuery(Handle, Tmp);
    if (PlainDRV.GetResultStatus(Res) <> TZPostgreSQLExecStatusType(
      PGRES_COMMAND_OK)) then
   begin
      PlainDRV.Clear(Res);
    Exit;
   end;
    PlainDRV.Clear(Res);
  end;
 FActive        := True;
  FTimer.Enabled := True;
end;

procedure TZPgEventAlerter.CloseNotify;
var
  I        : Integer;
  tmp      : array [0..255] of AnsiChar;
  Handle   : PZPostgreSQLConnect;
  ICon     : IZPostgreSQLConnection;
  PlainDRV : IZPostgreSQLPlainDriver;
  Res: PGresult;
begin
  if not FActive then
    Exit;
  FActive        := False;
  FTimer.Enabled := False;
  ICon           := (FConnection.DbcConnection as IZPostgreSQLConnection);
  Handle         := ICon.GetConnectionHandle;
  PlainDRV       := ICon.GetPlainDriver;
  if Handle = nil then
    Exit;
  for I := 0 to FEvents.Count-1 do
  begin
    StrPCopy(Tmp, 'unlisten ' + FEvents.Strings[i]);
    Res := PlainDRV.ExecuteQuery(Handle, Tmp);
    if (PlainDRV.GetResultStatus(Res) <> TZPostgreSQLExecStatusType(
      PGRES_COMMAND_OK)) then
   begin
      PlainDRV.Clear(Res);
    Exit;
   end;
    PlainDRV.Clear(Res);
  end;
end;

procedure TZPgEventAlerter.CheckEvents;
var
  Notify: PZPostgreSQLNotify;
  Handle   : PZPostgreSQLConnect;
  ICon     : IZPostgreSQLConnection;
  PlainDRV : IZPostgreSQLPlainDriver;
begin

 ICon      := (FConnection.DbcConnection as IZPostgreSQLConnection);
 Handle    := ICon.GetConnectionHandle;
 if Handle=nil then
 begin
    FTimer.Enabled := False;
    FActive := False;
  Exit;
 end;
 if not FConnection.Connected then
 begin
  CloseNotify;
  Exit;
 end;
 PlainDRV  := ICon.GetPlainDriver;

 if PlainDRV.ConsumeInput(Handle)=1 then
 begin
    while True do
  begin
      Notify := PlainDRV.Notifies(Handle);
      if Notify = nil then
        Break;
      if Assigned(FNotifyFired) then
        FNotifyFired(Self, Notify
{$IFDEF FPC}
          ^
{$ENDIF}
          .relname, Notify
{$IFDEF FPC}
          ^
{$ENDIF}
          .be_pid);
      PlainDRV.FreeNotify(Notify);
  end;
 end;
end;

end.

