unit Alcinoe.iOSApi.AudioToolbox;

interface

uses
  Macapi.CoreFoundation;

{$M+}

const
  libAudioToolbox = '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';

const
  kSystemSoundID_Vibrate = 4095;

type
  SystemSoundID = UInt32;

{************************************************************************************************************}
//This function will be deprecated in a future release. Use AudioServicesPlayAlertSoundWithCompletion instead.
//  @function       AudioServicesPlayAlertSound
//  @abstract       Play an Alert Sound
//  @discussion     Play the provided SystemSoundID with AlertSound behavior.
//  @param          inSystemSoundID
//                      A SystemSoundID for the System Sound server to play. On the desktop you
//                      can pass the kSystemSoundID_UserPreferredAlert constant to playback the alert sound
//                      selected by the user in System Preferences. On iOS there is no preferred user alert sound.
//extern void
//AudioServicesPlayAlertSound(SystemSoundID inSystemSoundID) __OSX_AVAILABLE_STARTING(__MAC_10_5,__IPHONE_2_0);
procedure AudioServicesPlayAlertSound(inSystemSoundID: SystemSoundID); cdecl; external libAudioToolbox name _PU + 'AudioServicesPlayAlertSound';

implementation

end.
