unit Alcinoe.iOSapi.PhotosUI;

interface

{$I Alcinoe.inc}

uses
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.Foundation,
  iOSapi.UIKit,
  Alcinoe.iOSApi.Photos;

{$M+}

//const
  //PHLivePhotoBadgeOptionsOverContent = 1 shl 0;
  //PHLivePhotoBadgeOptionsLiveOff = 1 shl 1;
  //PHLivePhotoViewPlaybackStyleUndefined = 0;
  //PHLivePhotoViewPlaybackStyleFull = 1;
  //PHLivePhotoViewPlaybackStyleHint = 2;
  //PHPickerConfigurationAssetRepresentationModeAutomatic = 0;
  //PHPickerConfigurationAssetRepresentationModeCurrent = 1;
  //PHPickerConfigurationAssetRepresentationModeCompatible = 2;
  //PHPickerConfigurationSelectionDefault = 0;
  //PHPickerConfigurationSelectionOrdered = 1;
  //PHPickerConfigurationSelectionContinuous = 2;
  //PHPickerConfigurationSelectionContinuousAndOrdered = 3;
  //PHPickerModeDefault = 0;
  //PHPickerModeCompact = 1;
  //PHPickerCapabilitiesNone = 0;
  //PHPickerCapabilitiesSearch = 1 shl 0;
  //PHPickerCapabilitiesStagingArea = 1 shl 1;
  //PHPickerCapabilitiesCollectionNavigation = 1 shl 2;
  //PHPickerCapabilitiesSelectionActions = 1 shl 3;
  //PHPickerCapabilitiesSensitivityAnalysisIntervention = 1 shl 4;

type

  //PHContentEditingController = interface;
  //PHLivePhotoViewDelegate = interface;
  //PHLivePhotoView = interface;
  PHPickerFilter = interface;
  //PHPickerUpdateConfiguration = interface;
  PHPickerConfiguration = interface;
  PHPickerResult = interface;
  PHPickerViewController = interface;
  PHPickerViewControllerDelegate = interface;
  //PhotosUISupport = interface;

  //TPhotosUICompletionHandler = procedure(param1: PHContentEditingOutput) of object;
  //PHLivePhotoBadgeOptions = NSUInteger;
  //PHLivePhotoViewPlaybackStyle = NSInteger;
  //PHPickerConfigurationAssetRepresentationMode = NSInteger;
  //PHPickerConfigurationSelection = NSInteger;
  //PHPickerMode = NSInteger;
  //PHPickerCapabilities = NSUInteger;
  //PHAssetPlaybackStyle = NSInteger;
  //NSDirectionalRectEdge = NSUInteger;
  //TPhotosUICompletionHandler1 = procedure(param1: NSArray) of object;

  {*********************************************}
  //PHLivePhotoViewClass = interface(UIViewClass)
    //['{F43FF751-4E8C-4A99-942E-228195274105}']
    //{class} function livePhotoBadgeImageWithOptions(badgeOptions: PHLivePhotoBadgeOptions): UIImage; cdecl;
  //end;
  //PHLivePhotoView = interface(UIView)
    //['{29F3DC17-16D4-45E8-83CB-EB27F3DB995B}']
    //procedure setDelegate(delegate: id); cdecl;
    //function delegate: id; cdecl;
    //procedure setLivePhoto(livePhoto: PHLivePhoto); cdecl;
    //function livePhoto: PHLivePhoto; cdecl;
    //procedure setContentsRect(contentsRect: CGRect); cdecl;
    //function contentsRect: CGRect; cdecl;
    //procedure setMuted(muted: BOOL); cdecl;
    //function isMuted: BOOL; cdecl;
    //function playbackGestureRecognizer: UIGestureRecognizer; cdecl;
    //procedure startPlaybackWithStyle(playbackStyle: PHLivePhotoViewPlaybackStyle); cdecl;
    //procedure stopPlayback; cdecl;
  //end;
  //TPHLivePhotoView = class(TOCGenericImport<PHLivePhotoViewClass, PHLivePhotoView>) end;
  //PPHLivePhotoView = Pointer;

  {********************************************}
  PHPickerFilterClass = interface(NSObjectClass)
    ['{32949921-2423-4BEE-9E04-8F8A7DBFCCCA}']
    {class} function imagesFilter: PHPickerFilter; cdecl;
    {class} function videosFilter: PHPickerFilter; cdecl;
    //{class} function livePhotosFilter: PHPickerFilter; cdecl;
    //{class} function depthEffectPhotosFilter: PHPickerFilter; cdecl;
    //{class} function burstsFilter: PHPickerFilter; cdecl;
    //{class} function panoramasFilter: PHPickerFilter; cdecl;
    //{class} function screenshotsFilter: PHPickerFilter; cdecl;
    //{class} function screenRecordingsFilter: PHPickerFilter; cdecl;
    //{class} function cinematicVideosFilter: PHPickerFilter; cdecl;
    //{class} function slomoVideosFilter: PHPickerFilter; cdecl;
    //{class} function timelapseVideosFilter: PHPickerFilter; cdecl;
    //{class} function spatialMediaFilter: PHPickerFilter; cdecl;
    //{class} function playbackStyleFilter(playbackStyle: PHAssetPlaybackStyle): PHPickerFilter; cdecl;
    {class} function anyFilterMatchingSubfilters(subfilters: NSArray): PHPickerFilter; cdecl;
    {class} function allFilterMatchingSubfilters(subfilters: NSArray): PHPickerFilter; cdecl;
    {class} function notFilterOfSubfilter(subfilter: PHPickerFilter): PHPickerFilter; cdecl;
  end;
  PHPickerFilter = interface(NSObject)
    ['{ECB2E60E-2787-471F-9E2D-B03EA982E2E6}']
  end;
  TPHPickerFilter = class(TOCGenericImport<PHPickerFilterClass, PHPickerFilter>) end;
  PPHPickerFilter = Pointer;

  {***********************************************************}
  //PHPickerUpdateConfigurationClass = interface(NSObjectClass)
    //['{CCD5DFA3-6C86-4B2D-BAB4-04DF368B6B22}']
  //end;
  //PHPickerUpdateConfiguration = interface(NSObject)
    //['{81C905C3-E538-444E-9A21-28C81005256C}']
    //procedure setSelectionLimit(selectionLimit: NSInteger); cdecl;
    //function selectionLimit: NSInteger; cdecl;
    //procedure setEdgesWithoutContentMargins(edgesWithoutContentMargins: NSDirectionalRectEdge); cdecl;
    //function edgesWithoutContentMargins: NSDirectionalRectEdge; cdecl;
  //end;
  //TPHPickerUpdateConfiguration = class(TOCGenericImport<PHPickerUpdateConfigurationClass, PHPickerUpdateConfiguration>) end;
  //PPHPickerUpdateConfiguration = Pointer;

  {***************************************************}
  PHPickerConfigurationClass = interface(NSObjectClass)
    ['{E3285E83-85D4-492F-8703-2F0CEC6DE74F}']
  end;
  PHPickerConfiguration = interface(NSObject)
    ['{25A572F9-8C01-4A95-A744-C977B6048879}']
    //procedure setPreferredAssetRepresentationMode(preferredAssetRepresentationMode: PHPickerConfigurationAssetRepresentationMode); cdecl;
    //function preferredAssetRepresentationMode: PHPickerConfigurationAssetRepresentationMode; cdecl;
    //procedure setSelection(selection: PHPickerConfigurationSelection); cdecl;
    //function selection: PHPickerConfigurationSelection; cdecl;
    procedure setSelectionLimit(selectionLimit: NSInteger); cdecl;
    function selectionLimit: NSInteger; cdecl;
    procedure setFilter(filter: PHPickerFilter); cdecl;
    function filter: PHPickerFilter; cdecl;
    //procedure setPreselectedAssetIdentifiers(preselectedAssetIdentifiers: NSArray); cdecl;
    //function preselectedAssetIdentifiers: NSArray; cdecl;
    //procedure setMode(mode: PHPickerMode); cdecl;
    //function mode: PHPickerMode; cdecl;
    //procedure setEdgesWithoutContentMargins(edgesWithoutContentMargins: NSDirectionalRectEdge); cdecl;
    //function edgesWithoutContentMargins: NSDirectionalRectEdge; cdecl;
    //procedure setDisabledCapabilities(disabledCapabilities: PHPickerCapabilities); cdecl;
    //function disabledCapabilities: PHPickerCapabilities; cdecl;
    function initWithPhotoLibrary(photoLibrary: PHPhotoLibrary): PHPickerConfiguration; cdecl;
    function init: PHPickerConfiguration; cdecl;
  end;
  TPHPickerConfiguration = class(TOCGenericImport<PHPickerConfigurationClass, PHPickerConfiguration>) end;
  PPHPickerConfiguration = Pointer;

  {********************************************}
  PHPickerResultClass = interface(NSObjectClass)
    ['{73A7C8D1-6385-4B63-A030-C54E9F08C3F5}']
  end;
  PHPickerResult = interface(NSObject)
    ['{B46E6832-1B29-4A72-B146-87AD6DBBCFC2}']
    function itemProvider: NSItemProvider; cdecl;
    function assetIdentifier: NSString; cdecl;
  end;
  TPHPickerResult = class(TOCGenericImport<PHPickerResultClass, PHPickerResult>) end;
  PPHPickerResult = Pointer;

  {************************************************************}
  PHPickerViewControllerClass = interface(UIViewControllerClass)
    ['{FAA694A3-AFCC-4C71-AA83-F26E45A36720}']
  end;
  PHPickerViewController = interface(UIViewController)
    //['{141707F3-217C-4A54-8749-BFD071C83032}']
    function configuration: PHPickerConfiguration; cdecl;
    procedure setDelegate(delegate: PHPickerViewControllerDelegate); cdecl;
    function delegate: PHPickerViewControllerDelegate; cdecl;
    function initWithConfiguration(configuration: PHPickerConfiguration): PHPickerViewController; cdecl;
    //procedure updatePickerUsingConfiguration(configuration: PHPickerUpdateConfiguration); cdecl;
    //procedure deselectAssetsWithIdentifiers(identifiers: NSArray); cdecl;
    //procedure moveAssetWithIdentifier(identifier: NSString; afterAssetWithIdentifier: NSString); cdecl;
    //procedure scrollToInitialPosition; cdecl;
    //procedure zoomIn; cdecl;
    //procedure zoomOut; cdecl;
  end;
  TPHPickerViewController = class(TOCGenericImport<PHPickerViewControllerClass, PHPickerViewController>) end;
  PPHPickerViewController = Pointer;

  {****************************************}
  //PhotosUISupport = interface(IObjectiveC)
    //['{5220A509-13A8-467D-8B1D-BA2720AB2AF3}']
    //[MethodName('presentLimitedLibraryPickerFromViewController:')]
    //procedure presentLimitedLibraryPickerFromViewController(controller: UIViewController); cdecl;
    //[MethodName('presentLimitedLibraryPickerFromViewController:completionHandler:')]
    //procedure presentLimitedLibraryPickerFromViewControllerCompletionHandler(controller: UIViewController; completionHandler: TPhotosUICompletionHandler1); cdecl;
  //end;

  {***************************************************}
  //PHContentEditingController = interface(IObjectiveC)
    //['{5606540A-E904-4239-B7FB-1AF686CAD292}']
    //function canHandleAdjustmentData(adjustmentData: PHAdjustmentData): BOOL; cdecl;
    //procedure startContentEditingWithInput(contentEditingInput: PHContentEditingInput; placeholderImage: UIImage); cdecl;
    //procedure finishContentEditingWithCompletionHandler(completionHandler: TPhotosUICompletionHandler); cdecl;
    //procedure cancelContentEditing; cdecl;
    //function shouldShowCancelConfirmation: BOOL; cdecl;
  //end;

  {************************************************}
  //PHLivePhotoViewDelegate = interface(IObjectiveC)
    //['{F99664CF-757B-4411-BB0D-4153963DFA2E}']
    //[MethodName('livePhotoView:canBeginPlaybackWithStyle:')]
    //function livePhotoViewCanBeginPlaybackWithStyle(livePhotoView: PHLivePhotoView; canBeginPlaybackWithStyle: PHLivePhotoViewPlaybackStyle): BOOL; cdecl;
    //[MethodName('livePhotoView:willBeginPlaybackWithStyle:')]
    //procedure livePhotoViewWillBeginPlaybackWithStyle(livePhotoView: PHLivePhotoView; willBeginPlaybackWithStyle: PHLivePhotoViewPlaybackStyle); cdecl;
    //[MethodName('livePhotoView:didEndPlaybackWithStyle:')]
    //procedure livePhotoViewDidEndPlaybackWithStyle(livePhotoView: PHLivePhotoView; didEndPlaybackWithStyle: PHLivePhotoViewPlaybackStyle); cdecl;
    //[MethodName('livePhotoView:extraMinimumTouchDurationForTouch:withStyle:')]
    //function livePhotoViewExtraMinimumTouchDurationForTouchWithStyle(livePhotoView: PHLivePhotoView; extraMinimumTouchDurationForTouch: UITouch; withStyle: PHLivePhotoViewPlaybackStyle): NSTimeInterval; cdecl;
  //end;

  {*****************************************************}
  PHPickerViewControllerDelegate = interface(IObjectiveC)
    ['{96EA0797-C853-449F-8366-1074E6A43C5B}']
    procedure picker(picker: PHPickerViewController; didFinishPicking: NSArray); cdecl;
  end;

const
  libPhotosUI = '/System/Library/Frameworks/PhotosUI.framework/PhotosUI';

implementation

{********************************************************}
procedure PhotosUIFakeLoader; cdecl; external libPhotosUI;

end.