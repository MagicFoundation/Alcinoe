unit Alcinoe.iOSApi.Photos;

interface

{$I Alcinoe.inc}

uses
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.Foundation,
  iOSapi.AVFoundation,
  iOSapi.UIKit;

{$M+}

Type

  {********************************}
  PHAuthorizationStatus = NSInteger;
  PHImageRequestOptionsDeliveryMode = NSInteger;
  PHImageRequestOptionsVersion = NSInteger;
  PHImageRequestOptionsResizeMode = NSInteger;

const

  {*************************************}
  PHAuthorizationStatusNotDetermined = 0;
  PHAuthorizationStatusRestricted = 1;
  PHAuthorizationStatusDenied = 2;
  PHAuthorizationStatusAuthorized = 3;
  PHImageRequestOptionsDeliveryModeOpportunistic = 0;
  PHImageRequestOptionsDeliveryModeHighQualityFormat = 1;
  PHImageRequestOptionsDeliveryModeFastFormat = 2;
  PHImageRequestOptionsVersionCurrent = 0;
  PHImageRequestOptionsVersionUnadjusted = 1;
  PHImageRequestOptionsVersionOriginal = 2;
  PHImageRequestOptionsResizeModeNone = 0;
  PHImageRequestOptionsResizeModeFast = 1;
  PHImageRequestOptionsResizeModeExact = 2;
  PHAssetMediaTypeUnknown = 0;
  PHAssetMediaTypeImage = 1;
  PHAssetMediaTypeVideo = 2;
  PHAssetMediaTypeAudio = 3;

type

  {*************************}
  PHPhotoLibrary = interface;
  PHObject = interface;
  PHFetchResult = interface;
  PHFetchOptions = interface;
  PHAsset = interface;
  PHImageRequestOptions = interface;
  PHVideoRequestOptions = interface;
  PHImageManager = interface;

  {***************************}
  PHAssetMediaType = NSInteger;
  ObjectType = Pointer;
  TPhotosHandler = procedure(status: PHAuthorizationStatus) of object;
  PHImageRequestID = Int32;
  TPHImageManagerRequestImageDataForAssetResultHandler = procedure(imageData: NSData; dataUTI: NSString; orientation: UIImageOrientation; info: NSDictionary) of object;
  TPHImageManagerRequestAVAssetForVideoResultHandler = procedure(avAsset: AVAsset; audioMix: AVAudioMix; info: NSDictionary) of object;

  {********************************************}
  PHPhotoLibraryClass = interface(NSObjectClass)
    ['{830554B2-EB3A-4650-A375-2BEA4ED95323}']
    { class } function sharedPhotoLibrary: PHPhotoLibrary; cdecl;
    { class } function authorizationStatus: PHAuthorizationStatus; cdecl;
    { class } procedure requestAuthorization(handler: TPhotosHandler); cdecl;
  end;
  PHPhotoLibrary = interface(NSObject)
    ['{48086E18-EE33-4ACD-BDB2-BD32B9523CCD}']
    //procedure performChanges(changeBlock: Pointer { dispatch_block_t }; completionHandler: TPhotosCompletionHandler); cdecl;
    //function performChangesAndWait(changeBlock: Pointer { dispatch_block_t }; error: NSError): Boolean; cdecl;
    //procedure registerChangeObserver(observer: Pointer); cdecl;
    //procedure unregisterChangeObserver(observer: Pointer); cdecl;
  end;
  TPHPhotoLibrary = class(TOCGenericImport<PHPhotoLibraryClass, PHPhotoLibrary>) end;
  PPHPhotoLibrary = Pointer;

  {**************************************}
  PHObjectClass = interface(NSObjectClass)
    ['{E2DD3131-D43F-4C65-8F21-AB5C9CBF7C63}']
  end;
  PHObject = interface(NSObject)
    ['{7381DD09-4F11-4972-BDB8-45C535936AB3}']
    //function localIdentifier: NSString; cdecl;
  end;
  TPHObject = class(TOCGenericImport<PHObjectClass, PHObject>) end;
  PPHObject = Pointer;

  {*******************************************}
  PHFetchResultClass = interface(NSObjectClass)
    ['{81C40F03-EF47-42B7-8385-9113E0CC417C}']
  end;
  PHFetchResult = interface(NSObject)
    ['{4ACE8B3F-F2FC-40EB-B291-04B6EEF85CE2}']
    function count: NSUInteger; cdecl;
    function objectAtIndex(index: NSUInteger): ObjectType; cdecl;
    //function objectAtIndexedSubscript(idx: NSUInteger): ObjectType; cdecl;
    //function containsObject(anObject: ObjectType): Boolean; cdecl;
    //[MethodName('indexOfObject:')]
    //function indexOfObject(anObject: ObjectType): NSUInteger; cdecl;
    //[MethodName('indexOfObject:inRange:')]
    //function indexOfObjectInRange(anObject: ObjectType; inRange: NSRange): NSUInteger; cdecl;
    function firstObject: ObjectType; cdecl;
    //function lastObject: ObjectType; cdecl;
    //function objectsAtIndexes(indexes: NSIndexSet): NSArray; cdecl;
    //procedure enumerateObjectsUsingBlock(block: TPhotosBlock); cdecl;
    //procedure enumerateObjectsWithOptions(opts: NSEnumerationOptions; usingBlock: TPhotosBlock); cdecl;
    //procedure enumerateObjectsAtIndexes(s: NSIndexSet; options: NSEnumerationOptions; usingBlock: TPhotosBlock); cdecl;
    //function countOfAssetsWithMediaType(mediaType: PHAssetMediaType): NSUInteger; cdecl;
  end;
  TPHFetchResult = class(TOCGenericImport<PHFetchResultClass, PHFetchResult>) end;
  PPHFetchResult = Pointer;

  {********************************************}
  PHFetchOptionsClass = interface(NSObjectClass)
    ['{7D2F5AF9-4736-445F-80FC-7821109DDDD4}']
  end;
  PHFetchOptions = interface(NSObject)
    ['{B94B68CA-5BE6-414D-957E-6EA506A15815}']
    //procedure setPredicate(predicate: NSPredicate); cdecl;
    //function predicate: NSPredicate; cdecl;
    //procedure setSortDescriptors(sortDescriptors: NSArray); cdecl;
    //function sortDescriptors: NSArray; cdecl;
    //procedure setIncludeHiddenAssets(includeHiddenAssets: Boolean); cdecl;
    //function includeHiddenAssets: Boolean; cdecl;
    //procedure setIncludeAllBurstAssets(includeAllBurstAssets: Boolean); cdecl;
    //function includeAllBurstAssets: Boolean; cdecl;
    //procedure setIncludeAssetSourceTypes(includeAssetSourceTypes: PHAssetSourceType); cdecl;
    //function includeAssetSourceTypes: PHAssetSourceType; cdecl;
    //procedure setFetchLimit(fetchLimit: NSUInteger); cdecl;
    //function fetchLimit: NSUInteger; cdecl;
    //procedure setWantsIncrementalChangeDetails(wantsIncrementalChangeDetails: Boolean); cdecl;
    //function wantsIncrementalChangeDetails: Boolean; cdecl;
  end;
  TPHFetchOptions = class(TOCGenericImport<PHFetchOptionsClass, PHFetchOptions>) end;
  PPHFetchOptions = Pointer;

  {*************************************}
  PHAssetClass = interface(PHObjectClass)
    ['{3697A99D-0346-4619-B6F5-5C1959EC144D}']
    //{ class } function fetchAssetsInAssetCollection(assetCollection: PHAssetCollection; options: PHFetchOptions): PHFetchResult; cdecl;
    { class } function fetchAssetsWithLocalIdentifiers(identifiers: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function fetchKeyAssetsInAssetCollection(assetCollection: PHAssetCollection; options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function fetchAssetsWithBurstIdentifier(burstIdentifier: NSString; options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function fetchAssetsWithOptions(options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function fetchAssetsWithMediaType(mediaType: PHAssetMediaType; options: PHFetchOptions): PHFetchResult; cdecl;
    { class } function fetchAssetsWithALAssetURLs(assetURLs: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;
  end;
  PHAsset = interface(PHObject)
    ['{904CFD36-D8E8-4F81-BEBC-F8B42021C446}']
    //function playbackStyle: PHAssetPlaybackStyle; cdecl;
    function mediaType: PHAssetMediaType; cdecl;
    //function mediaSubtypes: PHAssetMediaSubtype; cdecl;
    //function pixelWidth: NSUInteger; cdecl;
    //function pixelHeight: NSUInteger; cdecl;
    //function creationDate: NSDate; cdecl;
    //function modificationDate: NSDate; cdecl;
    //function location: CLLocation; cdecl;
    //function duration: NSTimeInterval; cdecl;
    //function isHidden: Boolean; cdecl;
    //function isFavorite: Boolean; cdecl;
    //function burstIdentifier: NSString; cdecl;
    //function burstSelectionTypes: PHAssetBurstSelectionType; cdecl;
    //function representsBurst: Boolean; cdecl;
    //function sourceType: PHAssetSourceType; cdecl;
    //function canPerformEditOperation(editOperation: PHAssetEditOperation): Boolean; cdecl;
    //function requestContentEditingInputWithOptions(options: PHContentEditingInputRequestOptions; completionHandler: TPhotosCompletionHandler1): PHContentEditingInputRequestID; cdecl;
    //procedure cancelContentEditingInputRequest(requestID: PHContentEditingInputRequestID); cdecl;
  end;
  TPHAsset = class(TOCGenericImport<PHAssetClass, PHAsset>) end;
  PPHAsset = Pointer;

  {***************************************************}
  PHImageRequestOptionsClass = interface(NSObjectClass)
    ['{9A0FF6A8-59C4-4357-8FC8-6E6DB94D973A}']
  end;
  PHImageRequestOptions = interface(NSObject)
    ['{5C6EC6D7-C7CD-4451-A56A-9D8568AAE29A}']
    procedure setVersion(version: PHImageRequestOptionsVersion); cdecl;
    function version: PHImageRequestOptionsVersion; cdecl;
    procedure setDeliveryMode(deliveryMode: PHImageRequestOptionsDeliveryMode); cdecl;
    function deliveryMode: PHImageRequestOptionsDeliveryMode; cdecl;
    procedure setResizeMode(resizeMode: PHImageRequestOptionsResizeMode); cdecl;
    function resizeMode: PHImageRequestOptionsResizeMode; cdecl;
    //procedure setNormalizedCropRect(normalizedCropRect: CGRect); cdecl;
    //function normalizedCropRect: CGRect; cdecl;
    procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    function isNetworkAccessAllowed: Boolean; cdecl;
    procedure setSynchronous(synchronous: Boolean); cdecl;
    function isSynchronous: Boolean; cdecl;
    //procedure setProgressHandler(progressHandler: PHAssetImageProgressHandler); cdecl;
    //function progressHandler: PHAssetImageProgressHandler; cdecl;
  end;
  TPHImageRequestOptions = class(TOCGenericImport<PHImageRequestOptionsClass, PHImageRequestOptions>) end;
  PPHImageRequestOptions = Pointer;

  {***************************************************}
  PHVideoRequestOptionsClass = interface(NSObjectClass)
    ['{78236BEB-FA19-4CD8-A5D4-6D31D8D6CA93}']
  end;
  PHVideoRequestOptions = interface(NSObject)
    ['{D07B6FA6-5AC3-4886-903D-A511FBB6EBCC}']
    procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    function isNetworkAccessAllowed: Boolean; cdecl;
    //procedure setVersion(version: PHVideoRequestOptionsVersion); cdecl;
    //function version: PHVideoRequestOptionsVersion; cdecl;
    //procedure setDeliveryMode(deliveryMode: PHVideoRequestOptionsDeliveryMode); cdecl;
    //function deliveryMode: PHVideoRequestOptionsDeliveryMode; cdecl;
    //procedure setProgressHandler(progressHandler: PHAssetVideoProgressHandler); cdecl;
    //function progressHandler: PHAssetVideoProgressHandler; cdecl;
  end;
  TPHVideoRequestOptions = class(TOCGenericImport<PHVideoRequestOptionsClass, PHVideoRequestOptions>) end;

  {********************************************}
  PHImageManagerClass = interface(NSObjectClass)
    ['{A688D438-61FF-4043-91BC-4C95B4B237E1}']
    { class } function defaultManager: PHImageManager; cdecl;
  end;
  PHImageManager = interface(NSObject)
    ['{B0094B28-33E2-4620-9181-FDE2B65C2AD2}']
    //function requestImageForAsset(asset: PHAsset; targetSize: CGSize; contentMode: PHImageContentMode; options: PHImageRequestOptions; resultHandler: TPhotosResultHandler): PHImageRequestID; cdecl;
    function requestImageDataForAsset(asset: PHAsset; options: PHImageRequestOptions; resultHandler: TPHImageManagerRequestImageDataForAssetResultHandler): PHImageRequestID; cdecl;
    //procedure cancelImageRequest(requestID: PHImageRequestID); cdecl;
    //function requestLivePhotoForAsset(asset: PHAsset; targetSize: CGSize; contentMode: PHImageContentMode; options: PHLivePhotoRequestOptions; resultHandler: TPhotosResultHandler2): PHImageRequestID; cdecl;
    //function requestPlayerItemForVideo(asset: PHAsset; options: PHVideoRequestOptions; resultHandler: TPhotosResultHandler3): PHImageRequestID; cdecl;
    //function requestExportSessionForVideo(asset: PHAsset; options: PHVideoRequestOptions; exportPreset: NSString; resultHandler: TPhotosResultHandler4): PHImageRequestID; cdecl;
    function requestAVAssetForVideo(asset: PHAsset; options: PHVideoRequestOptions; resultHandler: TPHImageManagerRequestAVAssetForVideoResultHandler): PHImageRequestID; cdecl;
  end;
  TPHImageManager = class(TOCGenericImport<PHImageManagerClass, PHImageManager>) end;
  PPHImageManager = Pointer;

{*********************************}
function PHImageErrorKey: NSString;

const
  libPhotos = '/System/Library/Frameworks/Photos.framework/Photos';

implementation

{*********************************}
function PHImageErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHImageErrorKey');
end;

end.