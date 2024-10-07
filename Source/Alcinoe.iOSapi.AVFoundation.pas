unit Alcinoe.iOSapi.AVFoundation;

interface

{$I Alcinoe.inc}

uses
  Macapi.ObjectiveC,
  iOSapi.AVFoundation;

{$M+}

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-16853 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALAVPlayerItemClass = interface(AVPlayerItemClass)
    ['{7C1111EF-11D9-4DBC-83D0-0E9041A806E6}']
  end;
  ALAVPlayerItem = interface(AVPlayerItem)
    ['{D9F9BD18-6C79-4A6D-8DF7-0BF322854C71}']
    procedure addOutput(output: AVPlayerItemOutput); cdecl; // https://quality.embarcadero.com/browse/RSP-16853
    procedure removeOutput(output: AVPlayerItemOutput); cdecl; // https://quality.embarcadero.com/browse/RSP-16853
  end;
  TALAVPlayerItem = class(TOCGenericImport<ALAVPlayerItemClass, ALAVPlayerItem>)  end;

implementation

end.
