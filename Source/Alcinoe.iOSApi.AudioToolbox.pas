unit Alcinoe.iOSApi.AudioToolbox;

interface

{$I Alcinoe.inc}

uses
  Macapi.CoreFoundation;

{$M+}

const
  libAudioToolbox = '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';

const
  kSystemSoundID_Vibrate = 4095;

type
  SystemSoundID = UInt32;

procedure AudioServicesPlayAlertSound(inSystemSoundID: SystemSoundID); cdecl; external libAudioToolbox name _PU + 'AudioServicesPlayAlertSound';
procedure AudioServicesPlaySystemSound(inSystemSoundID: SystemSoundID); cdecl; external libAudioToolbox name _PU + 'AudioServicesPlaySystemSound';

implementation

end.
