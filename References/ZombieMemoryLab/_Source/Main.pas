unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TMainForm = class(TForm)
    StartButton: TButton;
    procedure StartButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Alcinoe.FMX.ErrorReporting,
  Macapi.Helpers,
  Macapi.ObjectiveC,
  iOSapi.AddressBook,
  iOSapi.AddressBookUI,
  iOSapi.AssetsLibrary,
  iOSapi.AudioToolbox,
  iOsapi.AVFAudio,
  iOSapi.AVFoundation,
  iOSapi.CocoaTypes,
  iOsapi.Contacts,
  iOSapi.CoreAudio,
  iOsapi.CoreAudioTypes,
  iOSapi.CoreData,
  iOSapi.CoreGraphics,
  iOSapi.CoreImage,
  iOSapi.CoreLocation,
  iOSapi.CoreMedia,
  iOsapi.CoreMIDI,
  iOSapi.CoreMotion,
  iOSapi.CoreTelephony,
  iOSapi.CoreText,
  iOSapi.CoreVideo,
  iOsapi.DeviceCheck,
  iOSapi.Foundation,
  iOSapi.GLKit,
  iOSapi.Helpers,
  iOSapi.iAd,
  iOsapi.ImageIO,
  iOsapi.Intents,
  iOSapi.LocalAuthentication,
  iOSapi.MapKit,
  iOSapi.MediaPlayer,
  iOSapi.OpenGLES,
  iOSapi.QuartzCore,
  iOSapi.Security,
  iOSapi.StoreKit,
  iOsapi.Symbols,
  iOSapi.UIKit,
  iOsapi.UniformTypeIdentifiers,
  iOSapi.UserNotifications,
  iOSapi.WebKit,
  Alcinoe.StringUtils,
  Alcinoe.common;

{**********************************************}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  TALErrorReporting.Instance;
end;

{****************************************************}
procedure TMainForm.StartButtonClick(Sender: TObject);
begin

  // https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/MemoryMgmt/Articles/mmRules.html
  //
  // Summary of Cocoa MRR rules:
  // - If you create an object with a method whose name starts with “alloc”, “new”,
  //   “copy”, or “mutableCopy”, you own it and must later call `release` (or `autorelease`).
  //
  // - Otherwise, you do NOT own returned objects. If you need to keep them beyond
  //   the current scope, take ownership (e.g., call `retain` or hold a strong reference)
  //   and later balance it with a `release`.

  TThread.CreateAnonymousThread(
  procedure
  var
    LPool: NSAutoreleasePool;
    LBlurFilter1: CIFilter;
    P: Pointer;
  begin

    LPool := TNSAutoreleasePool.Create;
    try

      LBlurFilter1 := TCIFilter.OCClass.filterWithName(StrToNsStr('CIGaussianBlur'));
      P := NSObjectToID(LBlurFilter1);
      ALLog('Step 1');
      LBlurFilter1.release;
      ALLog('Step 2');
      LBlurFilter1 := nil;
      ALLog('Step 3');
    finally
      LPool.release;
    end;

    ALLog('Step 4');

    TThread.CreateAnonymousThread(
    procedure
    var LBlurFilter2: CIFilter;
    begin
      sleep(5000);
      LBlurFilter2 := TCIFilter.Wrap(P);
      ALLog('Step 5', ALInttostrW(LBlurFilter2.retainCount));
    end).Start;

  end).Start;

end;

end.
