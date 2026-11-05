unit Alcinoe.iOSApi.PhotosUI;

interface

{$I Alcinoe.inc}

uses
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.Foundation,
  iOSapi.UIKit,
  Alcinoe.iOSApi.Photos;

{$M+}

type

  {*************************}
  PHPickerFilter = interface;
  PHPickerConfiguration = interface;
  PHPickerResult = interface;
  PHPickerViewController = interface;
  PHPickerViewControllerDelegate = interface;

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
    ['{141707F3-217C-4A54-8749-BFD071C83032}']
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