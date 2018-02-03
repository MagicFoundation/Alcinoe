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
{ The Original Code is JclWinMidi.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Robert Rossmair                                    }
{ Portions created by Robert Rossmair are Copyright (C) Robert Rossmair. All Rights Reserved.      }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Robert Rossmair                                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ MIDI functions for MS Windows platform                                                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclWinMidi;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils, System.Classes, Winapi.Windows, Winapi.MMSystem,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils, Classes, Windows, MMSystem,
  {$ENDIF ~HAS_UNITSCOPE}
  JclMIDI;

type
  TStereoChannel = (scLeft, scRight);
  
  // MIDI Out
  IJclWinMidiOut = interface(IJclMidiOut)
    ['{F3FCE71C-B924-462C-BA0D-8C2DC118DADB}']
    // property access methods
    function GetChannelVolume(Channel: TStereoChannel): Word;
    procedure SetChannelVolume(Channel: TStereoChannel; const Value: Word);
    function GetVolume: Word;
    procedure SetVolume(const Value: Word);
    // properties
    property ChannelVolume[Channel: TStereoChannel]: Word read GetChannelVolume write SetChannelVolume;
    property Volume: Word read GetVolume write SetVolume;
  end;

type
  TJclWinMidiOut = class(TJclMidiOut, IJclWinMidiOut)
  private
    FHandle: HMIDIOUT;
    FDeviceID: Cardinal;
    FDeviceCaps: MIDIOUTCAPS;
    FVolume: DWORD;
    procedure SetLRVolume(const LeftValue, RightValue: Word);
  protected
    procedure LongMessage(const Data: array of Byte);
    procedure DoSendMessage(const Data: array of Byte); override;
  public
    constructor Create(ADeviceID: Cardinal);
    destructor Destroy; override;
    property DeviceID: Cardinal read FDeviceID;
    function GetName: string; override;
    property Name: string read GetName;
    { IJclWinMidiOut }
    function GetChannelVolume(Channel: TStereoChannel): Word;
    procedure SetChannelVolume(Channel: TStereoChannel; const Value: Word);
    function GetVolume: Word;
    procedure SetVolume(const Value: Word);
    property ChannelVolume[Channel: TStereoChannel]: Word read GetChannelVolume write SetChannelVolume;
    property Volume: Word read GetVolume write SetVolume;
  end;

function MidiOut(DeviceID: Cardinal): IJclWinMidiOut;
procedure GetMidiOutputs(const List: TStrings);
procedure MidiOutCheck(Code: MMResult);

// MIDI In
procedure MidiInCheck(Code: MMResult);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclResources, JclStrings, JclSysUtils;

var
  FMidiOutputs: TStringList = nil;

function MidiOutputs: TStrings;
var
  I: Integer;
  Caps: MIDIOUTCAPS;
begin
  if FMidiOutputs = nil then
  begin
    FMidiOutputs := TStringList.Create;
    for I := 0 to midiOutGetNumDevs - 1 do
      if (midiOutGetDevCaps(I, @Caps, SizeOf(Caps)) = MMSYSERR_NOERROR) then
        FMidiOutputs.Add(Caps.szPName);
  end;
  Result := FMidiOutputs;
end;

procedure GetMidiOutputs(const List: TStrings);
begin
  List.Assign(MidiOutputs);
end;

function GetMidiInErrorMessage(const ErrorCode: MMRESULT): string;
begin
  SetLength(Result, MAXERRORLENGTH-1);
  if midiInGetErrorText(ErrorCode, @Result[1], MAXERRORLENGTH) = MMSYSERR_NOERROR then
    StrResetLength(Result)
  else
    Result := Format(RsMidiInUnknownError, [ErrorCode]);
end;

function GetMidiOutErrorMessage(const ErrorCode: MMRESULT): string;
begin
  SetLength(Result, MAXERRORLENGTH-1);
  if midiOutGetErrorText(ErrorCode, @Result[1], MAXERRORLENGTH) = MMSYSERR_NOERROR then
    StrResetLength(Result)
  else
    Result := Format(RsMidiOutUnknownError, [ErrorCode]);
end;

procedure MidiInCheck(Code: MMResult);
begin
  if Code <> MMSYSERR_NOERROR then
    raise EJclMidiError.Create(GetMidiInErrorMessage(Code));
end;

procedure MidiOutCheck(Code: MMResult);
begin
  if Code <> MMSYSERR_NOERROR then
    raise EJclMidiError.Create(GetMidiOutErrorMessage(Code));
end;

//=== { TJclWinMidiOut } =====================================================

var
  MidiMapperDeviceID: Cardinal = MIDI_MAPPER;

function MidiOut(DeviceID: Cardinal): IJclWinMidiOut;
var
  Device: TJclWinMidiOut;
begin
  if DeviceID = MIDI_MAPPER then
    DeviceID := MidiMapperDeviceID;
  Device := nil;
  if DeviceID <> MIDI_MAPPER then
    Device := TJclWinMidiOut(MidiOutputs.Objects[DeviceID]);
  // make instance a singleton for a given device ID
  if not Assigned(Device) then
  begin
    Device := TJclWinMidiOut.Create(DeviceID);
    if DeviceID = MIDI_MAPPER then
      MidiMapperDeviceID := Device.DeviceID;
    // cannot use DeviceID argument as index here, since it might be MIDI_MAPPER
    MidiOutputs.Objects[Device.DeviceID] := Device;
  end;
  Result := Device;
end;

constructor TJclWinMidiOut.Create(ADeviceID: Cardinal);
begin
  inherited Create;
  FVolume := $FFFFFFFF; // max. volume, in case Get/SetChannelVolume not supported
  MidiOutCheck(midiOutGetDevCaps(ADeviceID, @FDeviceCaps, SizeOf(FDeviceCaps)));
  MidiOutCheck(midiOutOpen(@FHandle, ADeviceID, 0, 0, 0));
  MidiOutCheck(midiOutGetID(FHandle, @FDeviceID));
end;

destructor TJclWinMidiOut.Destroy;
begin
  inherited Destroy;
  midiOutClose(FHandle);
  MidiOutputs.Objects[FDeviceID] := nil;
end;

function TJclWinMidiOut.GetName: string;
begin
  Result := FDeviceCaps.szPName;
end;

procedure TJclWinMidiOut.LongMessage(const Data: array of Byte);
var
  Hdr: MIDIHDR;
begin
  ResetMemory(Hdr, SizeOf(Hdr));
  Hdr.dwBufferLength := High(Data) - Low(Data) + 1;;
  Hdr.dwBytesRecorded := Hdr.dwBufferLength;
  Hdr.lpData := @Data;
  Hdr.dwFlags := 0;
  MidiOutCheck(midiOutPrepareHeader(FHandle, @Hdr, SizeOf(Hdr)));
  MidiOutCheck(midiOutLongMsg(FHandle, @Hdr, SizeOf(Hdr)));
  repeat
  until (Hdr.dwFlags and MHDR_DONE) <> 0;
end;

procedure TJclWinMidiOut.DoSendMessage(const Data: array of Byte);
var
  I: Integer;
  Msg: packed record
  case Integer of
    0:
      (Bytes: array [0..2] of Byte);
    1:
      (DWord: LongWord);
  end;
begin
  if High(Data) < 3 then
  begin
    for I := 0 to High(Data) do
      Msg.Bytes[I] := Data[I];
    MidiOutCheck(midiOutShortMsg(FHandle, Msg.DWord));
  end
  else
    LongMessage(Data);
end;

function TJclWinMidiOut.GetChannelVolume(Channel: TStereoChannel): Word;
begin
  midiOutGetVolume(FHandle, @FVolume);
  Result := FVolume;
end;

procedure TJclWinMidiOut.SetChannelVolume(Channel: TStereoChannel; const Value: Word);
begin
  if Channel = scLeft then
    SetLRVolume(Value, ChannelVolume[scRight])
  else
    SetLRVolume(ChannelVolume[scLeft], Value);
end;

function TJclWinMidiOut.GetVolume: Word;
begin
  Result := GetChannelVolume(scLeft);
end;

procedure TJclWinMidiOut.SetVolume(const Value: Word);
begin
  SetLRVolume(Value, Value);
end;

procedure TJclWinMidiOut.SetLRVolume(const LeftValue, RightValue: Word);
var
  Value: DWORD;
begin
  with LongRec(Value) do
  begin
    Lo := LeftValue;
    Hi := RightValue;
  end;
  if Value <> FVolume then
  begin
    if (MIDICAPS_VOLUME and FDeviceCaps.dwSupport) <> 0 then
      MidiOutCheck(midiOutSetVolume(FHandle, Value));
    FVolume := Value;
  end;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  FreeAndNil(FMidiOutputs);

end.
