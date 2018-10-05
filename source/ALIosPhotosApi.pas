unit ALIosPhotosApi;

interface

uses
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.Foundation,
  iOSapi.UIKit;

{$M+}

Type

  // NS_ENUM
  PHAuthorizationStatus = NSInteger;

const

  // User has not yet made a choice with regards to this application
  PHAuthorizationStatusNotDetermined = 0;

  // This application is not authorized to access photo data.
  // The user cannot change this application’s status, possibly due to active restrictions
  //   such as parental controls being in place.
  PHAuthorizationStatusRestricted = 1;

  // User has explicitly denied this application access to photos data.
  PHAuthorizationStatusDenied = 2;

  // User has authorized this application to access photos data.
  PHAuthorizationStatusAuthorized = 3;

type

    //typedef NS_ENUM(NSInteger, PHImageRequestOptionsDeliveryMode) {
    PHImageRequestOptionsDeliveryMode = NSInteger;

const

  //client may get several image results when the call is asynchronous or will get one result when the call is synchronous
  PHImageRequestOptionsDeliveryModeOpportunistic = 0;

  //client will get one result only and it will be as asked or better than asked (sync requests are automatically processed this way regardless of the specified mode)
  PHImageRequestOptionsDeliveryModeHighQualityFormat = 1;

  //client will get one result only and it may be degraded
  PHImageRequestOptionsDeliveryModeFastFormat = 2;

type

  //typedef NS_ENUM(NSInteger, PHImageRequestOptionsVersion) {
  PHImageRequestOptionsVersion = NSInteger;

const

  // version with edits (aka adjustments) rendered or unadjusted version if there is no edits
  PHImageRequestOptionsVersionCurrent = 0;

  // original version without any adjustments
  PHImageRequestOptionsVersionUnadjusted = 1;

  // original version, in the case of a combined format the highest fidelity format will be returned (e.g. RAW for a RAW+JPG source image)
  PHImageRequestOptionsVersionOriginal = 2;

type

  //typedef NS_ENUM(NSInteger, PHImageRequestOptionsResizeMode) {
  PHImageRequestOptionsResizeMode = NSInteger;

const

  //no resize
  PHImageRequestOptionsResizeModeNone = 0;

  // use targetSize as a hint for optimal decoding when the source image is a compressed format (i.e. subsampling), the delivered image may be larger than targetSize
  PHImageRequestOptionsResizeModeFast = 1;

  // same as above but also guarantees the delivered image is exactly targetSize (must be set when a normalizedCropRect is specified)
  PHImageRequestOptionsResizeModeExact = 2;


//const
  //PHImageContentModeAspectFit = 0;
  //PHImageContentModeAspectFill = 1;
  //PHImageContentModeDefault = PHImageContentModeAspectFit;
  //PHCollectionListTypeMomentList = 1;
  //PHCollectionListTypeFolder = 2;
  //PHCollectionListTypeSmartFolder = 3;
  //PHCollectionListSubtypeMomentListCluster = 1;
  //PHCollectionListSubtypeMomentListYear = 2;
  //PHCollectionListSubtypeRegularFolder = 100;
  //PHCollectionListSubtypeSmartFolderEvents = 200;
  //PHCollectionListSubtypeSmartFolderFaces = 201;
  //PHCollectionListSubtypeAny = 2147483647;
  //PHCollectionEditOperationDeleteContent = 1;
  //PHCollectionEditOperationRemoveContent = 2;
  //PHCollectionEditOperationAddContent = 3;
  //PHCollectionEditOperationCreateContent = 4;
  //PHCollectionEditOperationRearrangeContent = 5;
  //PHCollectionEditOperationDelete = 6;
  //PHCollectionEditOperationRename = 7;
  //PHAssetCollectionTypeAlbum = 1;
  //PHAssetCollectionTypeSmartAlbum = 2;
  //PHAssetCollectionTypeMoment = 3;
  //PHAssetCollectionSubtypeAlbumRegular = 2;
  //PHAssetCollectionSubtypeAlbumSyncedEvent = 3;
  //PHAssetCollectionSubtypeAlbumSyncedFaces = 4;
  //PHAssetCollectionSubtypeAlbumSyncedAlbum = 5;
  //PHAssetCollectionSubtypeAlbumImported = 6;
  //PHAssetCollectionSubtypeAlbumMyPhotoStream = 100;
  //PHAssetCollectionSubtypeAlbumCloudShared = 101;
  //PHAssetCollectionSubtypeSmartAlbumGeneric = 200;
  //PHAssetCollectionSubtypeSmartAlbumPanoramas = 201;
  //PHAssetCollectionSubtypeSmartAlbumVideos = 202;
  //PHAssetCollectionSubtypeSmartAlbumFavorites = 203;
  //PHAssetCollectionSubtypeSmartAlbumTimelapses = 204;
  //PHAssetCollectionSubtypeSmartAlbumAllHidden = 205;
  //PHAssetCollectionSubtypeSmartAlbumRecentlyAdded = 206;
  //PHAssetCollectionSubtypeSmartAlbumBursts = 207;
  //PHAssetCollectionSubtypeSmartAlbumSlomoVideos = 208;
  //PHAssetCollectionSubtypeSmartAlbumUserLibrary = 209;
  //PHAssetCollectionSubtypeSmartAlbumSelfPortraits = 210;
  //PHAssetCollectionSubtypeSmartAlbumScreenshots = 211;
  //PHAssetCollectionSubtypeSmartAlbumDepthEffect = 212;
  //PHAssetCollectionSubtypeSmartAlbumLivePhotos = 213;
  //PHAssetCollectionSubtypeSmartAlbumAnimated = 214;
  //PHAssetCollectionSubtypeSmartAlbumLongExposures = 215;
  //PHAssetCollectionSubtypeAny = 2147483647;
  //PHAssetEditOperationDelete = 1;
  //PHAssetEditOperationContent = 2;
  //PHAssetEditOperationProperties = 3;
  //PHAssetPlaybackStyleUnsupported = 0;
  //PHAssetPlaybackStyleImage = 1;
  //PHAssetPlaybackStyleImageAnimated = 2;
  //PHAssetPlaybackStyleLivePhoto = 3;
  //PHAssetPlaybackStyleVideo = 4;
  //PHAssetPlaybackStyleVideoLooping = 5;
  //PHAssetMediaTypeUnknown = 0;
  //PHAssetMediaTypeImage = 1;
  //PHAssetMediaTypeVideo = 2;
  //PHAssetMediaTypeAudio = 3;
  //PHAssetMediaSubtypeNone = 0;
  //PHAssetMediaSubtypePhotoPanorama = (1 shl 0);
  //PHAssetMediaSubtypePhotoHDR = (1 shl 1);
  //PHAssetMediaSubtypePhotoScreenshot = (1 shl 2);
  //PHAssetMediaSubtypePhotoLive = (1 shl 3);
  //PHAssetMediaSubtypePhotoDepthEffect = (1 shl 4);
  //PHAssetMediaSubtypeVideoStreamed = (1 shl 16);
  //PHAssetMediaSubtypeVideoHighFrameRate = (1 shl 17);
  //PHAssetMediaSubtypeVideoTimelapse = (1 shl 18);
  //PHAssetBurstSelectionTypeNone = 0;
  //PHAssetBurstSelectionTypeAutoPick = (1 shl 0);
  //PHAssetBurstSelectionTypeUserPick = (1 shl 1);
  //PHAssetSourceTypeNone = 0;
  //PHAssetSourceTypeUserLibrary = (1 shl 0);
  //PHAssetSourceTypeCloudShared = (1 shl 1);
  //PHAssetSourceTypeiTunesSynced = (1 shl 2);
  //PHAssetResourceTypePhoto = 1;
  //PHAssetResourceTypeVideo = 2;
  //PHAssetResourceTypeAudio = 3;
  //PHAssetResourceTypeAlternatePhoto = 4;
  //PHAssetResourceTypeFullSizePhoto = 5;
  //PHAssetResourceTypeFullSizeVideo = 6;
  //PHAssetResourceTypeAdjustmentData = 7;
  //PHAssetResourceTypeAdjustmentBasePhoto = 8;
  //PHAssetResourceTypePairedVideo = 9;
  //PHAssetResourceTypeFullSizePairedVideo = 10;
  //PHAssetResourceTypeAdjustmentBasePairedVideo = 11;
  //PHVideoRequestOptionsVersionCurrent = 0;
  //PHVideoRequestOptionsVersionOriginal = 1;
  //PHVideoRequestOptionsDeliveryModeAutomatic = 0;
  //PHVideoRequestOptionsDeliveryModeHighQualityFormat = 1;
  //PHVideoRequestOptionsDeliveryModeMediumQualityFormat = 2;
  //PHVideoRequestOptionsDeliveryModeFastFormat = 3;
  //PHLivePhotoFrameTypePhoto = 0;
  //PHLivePhotoFrameTypeVideo = 1;

type

  // ===== Forward declarations =====

  //PHAdjustmentData = interface;
  //PHPhotoLibrary = interface;
  PHObject = interface;
  //PHObjectPlaceholder = interface;
  PHFetchResult = interface;
  //PHChange = interface;
  //PHPhotoLibraryChangeObserver = interface;
  PHFetchOptions = interface;
  //PHAssetCollection = interface;
  PHAsset = interface;
  //PHContentEditingInput = interface;
  //PHContentEditingOutput = interface;
  //PHAssetResource = interface;
  //PHContentEditingInputRequestOptions = interface;
  //PHAssetChangeRequest = interface;
  //PHAssetCollectionChangeRequest = interface;
  //PHAssetResourceCreationOptions = interface;
  //PHAssetCreationRequest = interface;
  //PHLivePhoto = interface;
  //PHAssetResourceRequestOptions = interface;
  //PHAssetResourceManager = interface;
  //PHObjectChangeDetails = interface;
  //PHFetchResultChangeDetails = interface;
  //PHCollectionList = interface;
  //PHCollection = interface;
  //PHCollectionListChangeRequest = interface;
  PHImageRequestOptions = interface;
  //PHLivePhotoRequestOptions = interface;
  //PHVideoRequestOptions = interface;
  PHImageManager = interface;
  //PHCachingImageManager = interface;
  //PHLivePhotoFrame = interface;
  //PHLivePhotoEditingContext = interface;

  // ===== Framework typedefs =====

  //NSInteger = Integer;
  //PNSInteger = ^NSInteger;

  //PHImageContentMode = NSInteger;
  //PHCollectionListType = NSInteger;
  //PHCollectionListSubtype = NSInteger;
  //PHCollectionEditOperation = NSInteger;
  //PHAssetCollectionType = NSInteger;
  //PHAssetCollectionSubtype = NSInteger;
  //PHAssetEditOperation = NSInteger;
  //PHAssetPlaybackStyle = NSInteger;
  //PHAssetMediaType = NSInteger;
  //NSUInteger = Cardinal;
  //PNSUInteger = ^NSUInteger;

  //PHAssetMediaSubtype = NSUInteger;
  //PHAssetBurstSelectionType = NSUInteger;
  //PHAssetSourceType = NSUInteger;
  //PHAssetResourceType = NSInteger;
  ObjectType = Pointer;
  //PObjectType = ^ObjectType;

  //_NSRange = record
    //location: NSUInteger;
    //length: NSUInteger;
  //end;

  //P_NSRange = ^_NSRange;
  //NSRange = _NSRange;
  //PNSRange = ^NSRange;

  //TPhotosBlock = procedure(param1: ObjectType; param2: NSUInteger; param3: PBoolean) of object;
  //NSEnumerationOptions = NSUInteger;
  TPhotosHandler = procedure(status: PHAuthorizationStatus) of object;
  //TPhotosCompletionHandler = procedure(param1: Boolean; param2: NSError) of object;
  //NSTimeInterval = Double;
  //PNSTimeInterval = ^NSTimeInterval;

  //PHContentEditingInputRequestID = NSUInteger;
  //PPHContentEditingInputRequestID = ^PHContentEditingInputRequestID;
  //TPhotosCanHandleAdjustmentData = function(param1: PHAdjustmentData): Boolean; cdecl;
  //TPhotosProgressHandler = procedure(param1: Double; param2: PBoolean) of object;
  //TPhotosCompletionHandler1 = procedure(param1: PHContentEditingInput; param2: NSDictionary) of object;
  //PHAssetResourceDataRequestID = Int32;
  //PPHAssetResourceDataRequestID = ^PHAssetResourceDataRequestID;
  //PHAssetResourceProgressHandler = procedure(param1: Double) of object;
  //TPhotosDataReceivedHandler = procedure(param1: NSData) of object;
  //TPhotosCompletionHandler2 = procedure(param1: NSError) of object;
  //TPhotosHandler1 = procedure(param1: NSUInteger; param2: NSUInteger) of object;
  //PHAssetImageProgressHandler = procedure(param1: Double; param2: NSError; param3: PBoolean; param4: NSDictionary) of object;
  //CGFloat = Single;
  //PCGFloat = ^CGFloat;

  //CGPoint = record
    //x: CGFloat;
    //y: CGFloat;
  //end;

  //PCGPoint = ^CGPoint;

  //CGSize = record
    //width: CGFloat;
    //height: CGFloat;
  //end;

  //PCGSize = ^CGSize;

  //CGRect = record
    //origin: CGPoint;
    //size: CGSize;
  //end;

  //PCGRect = ^CGRect;

  //PHVideoRequestOptionsVersion = NSInteger;
  //PHVideoRequestOptionsDeliveryMode = NSInteger;
  //PHAssetVideoProgressHandler = procedure(param1: Double; param2: NSError; param3: PBoolean; param4: NSDictionary) of object;

  //typedef int32_t PHImageRequestID PHOTOS_AVAILABLE_IOS_TVOS(8_0, 10_0);
  PHImageRequestID = Int32;

  //PPHImageRequestID = ^PHImageRequestID;
  //TPhotosResultHandler = procedure(param1: UIImage; param2: NSDictionary) of object;
  //UIImageOrientation = NSInteger;

  //(void(^)(NSData *__nullable imageData, NSString *__nullable dataUTI, UIImageOrientation orientation, NSDictionary *__nullable info))
  TPHImageManagerRequestImageDataForAssetResultHandler = procedure(imageData: NSData; dataUTI: NSString; orientation: UIImageOrientation; info: NSDictionary) of object;

  //TPhotosResultHandler2 = procedure(param1: PHLivePhoto; param2: NSDictionary) of object;
  //TPhotosResultHandler3 = procedure(param1: AVPlayerItem; param2: NSDictionary) of object;
  //TPhotosResultHandler4 = procedure(param1: AVAssetExportSession; param2: NSDictionary) of object;
  //TPhotosResultHandler5 = procedure(param1: AVAsset; param2: AVAudioMix; param3: NSDictionary) of object;
  //PHLivePhotoRequestID = Int32;
  //PPHLivePhotoRequestID = ^PHLivePhotoRequestID;
  //PHLivePhotoFrameProcessingBlock = function(param1: Pointer; param2: NSError): CIImage; cdecl;
  //PHLivePhotoEditingOption = NSString;
  //PPHLivePhotoEditingOption = ^PHLivePhotoEditingOption;
  //CMTimeValue = Int64;
  //PCMTimeValue = ^CMTimeValue;
  //CMTimeScale = Int32;
  //PCMTimeScale = ^CMTimeScale;
  //CMTimeFlags = LongWord;
  //CMTimeEpoch = Int64;
  //PCMTimeEpoch = ^CMTimeEpoch;

  //CMTime = record
    //value: CMTimeValue;
    //timescale: CMTimeScale;
    //flags: CMTimeFlags;
    //epoch: CMTimeEpoch;
  //end;

  //PCMTime = ^CMTime;

  //CGImagePropertyOrientation = LongWord;
  //TPhotosCompletionHandler3 = procedure(param1: PHLivePhoto; param2: NSError) of object;
  //PHLivePhotoFrameType = NSInteger;

  // ===== Interface declarations =====

  //PHAdjustmentDataClass = interface(NSObjectClass)
    //['{52C63080-3CB7-4FCA-9205-7F49178348F0}']
  //end;

  //PHAdjustmentData = interface(NSObject)
    //['{1E262C98-F061-42E7-8769-F683E04993FB}']
    //function initWithFormatIdentifier(formatIdentifier: NSString; formatVersion: NSString; data: NSData): Pointer { instancetype }; cdecl;
    //function formatIdentifier: NSString; cdecl;
    //function formatVersion: NSString; cdecl;
    //function data: NSData; cdecl;
  //end;

  //TPHAdjustmentData = class(TOCGenericImport<PHAdjustmentDataClass, PHAdjustmentData>)
  //end;

  //PPHAdjustmentData = Pointer;

  PHPhotoLibraryClass = interface(NSObjectClass)
    ['{830554B2-EB3A-4650-A375-2BEA4ED95323}']

    //+ (PHPhotoLibrary *)sharedPhotoLibrary;
    //{ class } function sharedPhotoLibrary: PHPhotoLibrary; cdecl;

    //+ (PHAuthorizationStatus)authorizationStatus;
    { class } function authorizationStatus: PHAuthorizationStatus; cdecl;

    //+ (void)requestAuthorization:(void(^)(PHAuthorizationStatus status))handler;
    { class } procedure requestAuthorization(handler: TPhotosHandler); cdecl;

  end;
  PHPhotoLibrary = interface(NSObject)
    ['{48086E18-EE33-4ACD-BDB2-BD32B9523CCD}']

    // handlers are invoked on an arbitrary serial queue
    // Nesting change requests will throw an exception
    //- (void)performChanges:(dispatch_block_t)changeBlock completionHandler:(nullable void(^)(BOOL success, NSError *__nullable error))completionHandler;
    //procedure performChanges(changeBlock: Pointer { dispatch_block_t }; completionHandler: TPhotosCompletionHandler); cdecl;

    //- (BOOL)performChangesAndWait:(dispatch_block_t)changeBlock error:(NSError *__autoreleasing *)error;
    //function performChangesAndWait(changeBlock: Pointer { dispatch_block_t }; error: NSError): Boolean; cdecl;

    //- (void)registerChangeObserver:(id<PHPhotoLibraryChangeObserver>)observer;
    //procedure registerChangeObserver(observer: Pointer); cdecl;

    //- (void)unregisterChangeObserver:(id<PHPhotoLibraryChangeObserver>)observer;
    //procedure unregisterChangeObserver(observer: Pointer); cdecl;

  end;
  TPHPhotoLibrary = class(TOCGenericImport<PHPhotoLibraryClass, PHPhotoLibrary>) end;
  PPHPhotoLibrary = Pointer;

  PHObjectClass = interface(NSObjectClass)
    ['{E2DD3131-D43F-4C65-8F21-AB5C9CBF7C63}']
  end;
  PHObject = interface(NSObject)
    ['{7381DD09-4F11-4972-BDB8-45C535936AB3}']

    // Returns an identifier which persistently identifies the object on a given device
    //@property (nonatomic, copy, readonly) NSString *localIdentifier;
    //function localIdentifier: NSString; cdecl;

  end;
  TPHObject = class(TOCGenericImport<PHObjectClass, PHObject>) end;
  PPHObject = Pointer;

  //PHObjectPlaceholderClass = interface(PHObjectClass)
    //['{7D2580D6-3BFC-45B1-A59F-08A078E51A6D}']
  //end;

  //PHObjectPlaceholder = interface(PHObject)
    //['{A6E40892-607F-495A-A7CF-1A36D0E4FD9C}']
  //end;

  //TPHObjectPlaceholder = class(TOCGenericImport<PHObjectPlaceholderClass, PHObjectPlaceholder>)
  //end;

  //PPHObjectPlaceholder = Pointer;

  PHFetchResultClass = interface(NSObjectClass)
    ['{81C40F03-EF47-42B7-8385-9113E0CC417C}']
  end;
  PHFetchResult = interface(NSObject)
    ['{4ACE8B3F-F2FC-40EB-B291-04B6EEF85CE2}']

    //@property (readonly) NSUInteger count;
    //function count: NSUInteger; cdecl;

    //- (ObjectType)objectAtIndex:(NSUInteger)index;
    //function objectAtIndex(index: NSUInteger): ObjectType; cdecl;

    //- (ObjectType)objectAtIndexedSubscript:(NSUInteger)idx;
    //function objectAtIndexedSubscript(idx: NSUInteger): ObjectType; cdecl;

    //- (BOOL)containsObject:(ObjectType)anObject;
    //function containsObject(anObject: ObjectType): Boolean; cdecl;

    //- (NSUInteger)indexOfObject:(ObjectType)anObject;
    //[MethodName('indexOfObject:')]
    //function indexOfObject(anObject: ObjectType): NSUInteger; cdecl;

    //- (NSUInteger)indexOfObject:(ObjectType)anObject inRange:(NSRange)range;
    //[MethodName('indexOfObject:inRange:')]
    //function indexOfObjectInRange(anObject: ObjectType; inRange: NSRange): NSUInteger; cdecl;

    //@property (nonatomic, readonly, nullable) ObjectType firstObject;
    function firstObject: ObjectType; cdecl;

    //@property (nonatomic, readonly, nullable) ObjectType lastObject;
    //function lastObject: ObjectType; cdecl;

    //- (NSArray<ObjectType> *)objectsAtIndexes:(NSIndexSet *)indexes;
    //function objectsAtIndexes(indexes: NSIndexSet): NSArray; cdecl;

    //- (void)enumerateObjectsUsingBlock:(void (^)(ObjectType obj, NSUInteger idx, BOOL *stop))block;
    //procedure enumerateObjectsUsingBlock(block: TPhotosBlock); cdecl;

    //- (void)enumerateObjectsWithOptions:(NSEnumerationOptions)opts usingBlock:(void (^)(ObjectType obj, NSUInteger idx, BOOL *stop))block;
    //procedure enumerateObjectsWithOptions(opts: NSEnumerationOptions; usingBlock: TPhotosBlock); cdecl;

    //- (void)enumerateObjectsAtIndexes:(NSIndexSet *)s options:(NSEnumerationOptions)opts usingBlock:(void (^)(ObjectType obj, NSUInteger idx, BOOL *stop))block;
    //procedure enumerateObjectsAtIndexes(s: NSIndexSet; options: NSEnumerationOptions; usingBlock: TPhotosBlock); cdecl;

    //- (NSUInteger)countOfAssetsWithMediaType:(PHAssetMediaType)mediaType;
    //function countOfAssetsWithMediaType(mediaType: PHAssetMediaType): NSUInteger; cdecl;

  end;
  TPHFetchResult = class(TOCGenericImport<PHFetchResultClass, PHFetchResult>) end;
  PPHFetchResult = Pointer;

  //PHChangeClass = interface(NSObjectClass)
    //['{D04929E2-B30F-45F9-9763-28FFEDD6270E}']
  //end;

  //PHChange = interface(NSObject)
    //['{509D0E68-7DBB-4388-9F4C-0EC9E9D475BA}']
    //function changeDetailsForObject(&object: PHObject): PHObjectChangeDetails; cdecl;
    //function changeDetailsForFetchResult(&object: PHFetchResult): PHFetchResultChangeDetails; cdecl;
  //end;

  //TPHChange = class(TOCGenericImport<PHChangeClass, PHChange>)
  //end;

  //PPHChange = Pointer;

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

  //PHCollectionClass = interface(PHObjectClass)
    //['{A983FE41-FCEB-4AE1-A069-3DE7AD49AD4D}']
    //{ class } function fetchCollectionsInCollectionList(collectionList: PHCollectionList; options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function fetchTopLevelUserCollectionsWithOptions(options: PHFetchOptions): PHFetchResult; cdecl;
  //end;

  //PHCollection = interface(PHObject)
    //['{CE97D434-F624-4C8B-AA09-911AC51D5835}']
    //function canContainAssets: Boolean; cdecl;
    //function canContainCollections: Boolean; cdecl;
    //function localizedTitle: NSString; cdecl;
    //function canPerformEditOperation(anOperation: PHCollectionEditOperation): Boolean; cdecl;
  //end;

  //TPHCollection = class(TOCGenericImport<PHCollectionClass, PHCollection>)
  //end;

  //PPHCollection = Pointer;

  //PHAssetCollectionClass = interface(PHCollectionClass)
    //['{D793F88E-38BB-4DA1-8C1B-B4F8826332FD}']
    //{ class } function fetchAssetCollectionsWithLocalIdentifiers(identifiers: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function fetchAssetCollectionsWithType(&type: PHAssetCollectionType; subtype: PHAssetCollectionSubtype; options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function fetchAssetCollectionsContainingAsset(asset: PHAsset; withType: PHAssetCollectionType; options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function fetchAssetCollectionsWithALAssetGroupURLs(assetGroupURLs: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function fetchMomentsInMomentList(momentList: PHCollectionList; options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function fetchMomentsWithOptions(options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function transientAssetCollectionWithAssets(assets: NSArray; title: NSString): PHAssetCollection; cdecl;
    //{ class } function transientAssetCollectionWithAssetFetchResult(fetchResult: PHFetchResult; title: NSString): PHAssetCollection; cdecl;
  //end;

  //PHAssetCollection = interface(PHCollection)
    //['{4AC73F38-D3E3-4E87-B63B-99D069D98C0A}']
    //function assetCollectionType: PHAssetCollectionType; cdecl;
    //function assetCollectionSubtype: PHAssetCollectionSubtype; cdecl;
    //function estimatedAssetCount: NSUInteger; cdecl;
    //function startDate: NSDate; cdecl;
    //function endDate: NSDate; cdecl;
    //function approximateLocation: CLLocation; cdecl;
    //function localizedLocationNames: NSArray; cdecl;
  //end;

  //TPHAssetCollection = class(TOCGenericImport<PHAssetCollectionClass,
    //PHAssetCollection>)
  //end;

  //PPHAssetCollection = Pointer;

  PHAssetClass = interface(PHObjectClass)
    ['{3697A99D-0346-4619-B6F5-5C1959EC144D}']

    //+ (PHFetchResult<PHAsset *> *)fetchAssetsInAssetCollection:(PHAssetCollection *)assetCollection options:(nullable PHFetchOptions *)options;
    //{ class } function fetchAssetsInAssetCollection(assetCollection: PHAssetCollection; options: PHFetchOptions): PHFetchResult; cdecl;

    //+ (PHFetchResult<PHAsset *> *)fetchAssetsWithLocalIdentifiers:(NSArray<NSString *> *)identifiers options:(nullable PHFetchOptions *)options; // includes hidden assets by default
    //{ class } function fetchAssetsWithLocalIdentifiers(identifiers: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;

    //+ (nullable PHFetchResult<PHAsset *> *)fetchKeyAssetsInAssetCollection:(PHAssetCollection *)assetCollection options:(nullable PHFetchOptions *)options;
    //{ class } function fetchKeyAssetsInAssetCollection(assetCollection: PHAssetCollection; options: PHFetchOptions): PHFetchResult; cdecl;

    //+ (PHFetchResult<PHAsset *> *)fetchAssetsWithBurstIdentifier:(NSString *)burstIdentifier options:(nullable PHFetchOptions *)options;
    //{ class } function fetchAssetsWithBurstIdentifier(burstIdentifier: NSString; options: PHFetchOptions): PHFetchResult; cdecl;

    // Fetches PHAssetSourceTypeUserLibrary assets by default (use includeAssetSourceTypes option to override)
    //+ (PHFetchResult<PHAsset *> *)fetchAssetsWithOptions:(nullable PHFetchOptions *)options;
    //{ class } function fetchAssetsWithOptions(options: PHFetchOptions): PHFetchResult; cdecl;

    //+ (PHFetchResult<PHAsset *> *)fetchAssetsWithMediaType:(PHAssetMediaType)mediaType options:(nullable PHFetchOptions *)options;
    //{ class } function fetchAssetsWithMediaType(mediaType: PHAssetMediaType; options: PHFetchOptions): PHFetchResult; cdecl;

    // assetURLs are URLs retrieved from ALAsset's ALAssetPropertyAssetURL
    //+ (PHFetchResult<PHAsset *> *)fetchAssetsWithALAssetURLs:(NSArray<NSURL *> *)assetURLs options:(nullable PHFetchOptions *)options API_DEPRECATED("Will be removed in a future release", ios(8.0, 11.0), tvos(8.0, 11.0));
    { class } function fetchAssetsWithALAssetURLs(assetURLs: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;

  end;
  PHAsset = interface(PHObject)
    ['{904CFD36-D8E8-4F81-BEBC-F8B42021C446}']

    // Playback style describes how the asset should be presented to the user (regardless of the backing media for that asset).  Use this value to choose the type of view and the appropriate APIs on the PHImageManager to display this asset
    //@property (nonatomic, assign, readonly) PHAssetPlaybackStyle playbackStyle PHOTOS_AVAILABLE_IOS_TVOS(11_0, 11_0);
    //function playbackStyle: PHAssetPlaybackStyle; cdecl;

    //@property (nonatomic, assign, readonly) PHAssetMediaType mediaType;
    //function mediaType: PHAssetMediaType; cdecl;

    //@property (nonatomic, assign, readonly) PHAssetMediaSubtype mediaSubtypes;
    //function mediaSubtypes: PHAssetMediaSubtype; cdecl;

    //@property (nonatomic, assign, readonly) NSUInteger pixelWidth;
    //function pixelWidth: NSUInteger; cdecl;

    //@property (nonatomic, assign, readonly) NSUInteger pixelHeight;
    //function pixelHeight: NSUInteger; cdecl;

    //@property (nonatomic, strong, readonly, nullable) NSDate *creationDate;
    //function creationDate: NSDate; cdecl;

    //@property (nonatomic, strong, readonly, nullable) NSDate *modificationDate;
    //function modificationDate: NSDate; cdecl;

    //@property (nonatomic, strong, readonly, nullable) CLLocation *location;
    //function location: CLLocation; cdecl;

    //@property (nonatomic, assign, readonly) NSTimeInterval duration;
    //function duration: NSTimeInterval; cdecl;

    // a hidden asset will be excluded from moment collections, but may still be included in other smart or regular album collections
    //@property (nonatomic, assign, readonly, getter=isHidden) BOOL hidden;
    //function isHidden: Boolean; cdecl;

    //@property (nonatomic, assign, readonly, getter=isFavorite) BOOL favorite;
    //function isFavorite: Boolean; cdecl;

    //@property (nonatomic, strong, readonly, nullable) NSString *burstIdentifier;
    //function burstIdentifier: NSString; cdecl;

    //@property (nonatomic, assign, readonly) PHAssetBurstSelectionType burstSelectionTypes;
    //function burstSelectionTypes: PHAssetBurstSelectionType; cdecl;

    //@property (nonatomic, assign, readonly) BOOL representsBurst;
    //function representsBurst: Boolean; cdecl;

    //@property (nonatomic, assign, readonly) PHAssetSourceType sourceType PHOTOS_AVAILABLE_IOS_TVOS(9_0, 10_0);
    //function sourceType: PHAssetSourceType; cdecl;

    //- (BOOL)canPerformEditOperation:(PHAssetEditOperation)editOperation;
    //function canPerformEditOperation(editOperation: PHAssetEditOperation): Boolean; cdecl;

    //function requestContentEditingInputWithOptions(options: PHContentEditingInputRequestOptions; completionHandler: TPhotosCompletionHandler1): PHContentEditingInputRequestID; cdecl;
    //procedure cancelContentEditingInputRequest(requestID: PHContentEditingInputRequestID); cdecl;

  end;
  TPHAsset = class(TOCGenericImport<PHAssetClass, PHAsset>) end;
  PPHAsset = Pointer;

  //PHContentEditingInputClass = interface(NSObjectClass)
    //['{3C936542-A12D-43F5-82D6-78EE99F6D217}']
  //end;

  //PHContentEditingInput = interface(NSObject)
    //['{0862E607-BD0D-4858-9D92-6321C39DD171}']
    //function mediaType: PHAssetMediaType; cdecl;
    //function mediaSubtypes: PHAssetMediaSubtype; cdecl;
    //function creationDate: NSDate; cdecl;
    //function location: CLLocation; cdecl;
    //function uniformTypeIdentifier: NSString; cdecl;
    //function playbackStyle: PHAssetPlaybackStyle; cdecl;
    //function adjustmentData: PHAdjustmentData; cdecl;
    //function displaySizeImage: UIImage; cdecl;
    //function fullSizeImageURL: NSURL; cdecl;
    //function fullSizeImageOrientation: Integer; cdecl;
    //function AVAsset: AVAsset; cdecl;
    //function audiovisualAsset: AVAsset; cdecl;
    //function livePhoto: PHLivePhoto; cdecl;
  //end;

  //TPHContentEditingInput = class(TOCGenericImport<PHContentEditingInputClass, PHContentEditingInput>)
  //end;

  //PPHContentEditingInput = Pointer;

  //PHContentEditingOutputClass = interface(NSObjectClass)
    //['{A8A61C9D-287E-4C8A-9452-D1E0C7AD5CA5}']
  //end;

  //PHContentEditingOutput = interface(NSObject)
    //['{EBEA0B26-8A49-4C27-B6E8-BD9C47532347}']
    //function initWithContentEditingInput(contentEditingInput: PHContentEditingInput): Pointer { instancetype }; cdecl;
    //procedure setAdjustmentData(adjustmentData: PHAdjustmentData); cdecl;
    //function adjustmentData: PHAdjustmentData; cdecl;
    //function renderedContentURL: NSURL; cdecl;
    //function initWithPlaceholderForCreatedAsset(placeholderForCreatedAsset: PHObjectPlaceholder): Pointer { instancetype }; cdecl;
  //end;

  //TPHContentEditingOutput = class(TOCGenericImport<PHContentEditingOutputClass, PHContentEditingOutput>)
  //end;

  //PPHContentEditingOutput = Pointer;

  //PHAssetResourceClass = interface(NSObjectClass)
    //['{01C93B4C-889C-4C1B-B8A6-AD41A7CD88D0}']
    //{ class } function assetResourcesForAsset(asset: PHAsset): NSArray; cdecl;
    //{ class } function assetResourcesForLivePhoto(livePhoto: PHLivePhoto): NSArray; cdecl;
  //end;

  //PHAssetResource = interface(NSObject)
    //['{D394E90E-4BCB-4641-9B35-32023A52D193}']
    //function &type: PHAssetResourceType; cdecl;
    //function assetLocalIdentifier: NSString; cdecl;
    //function uniformTypeIdentifier: NSString; cdecl;
    //function originalFilename: NSString; cdecl;
  //end;

  //TPHAssetResource = class(TOCGenericImport<PHAssetResourceClass, PHAssetResource>)
  //end;

  //PPHAssetResource = Pointer;

  //PHContentEditingInputRequestOptionsClass = interface(NSObjectClass)
    //['{75404ADD-3C3C-4BD3-B9C0-2D3154D615B5}']
  //end;

  //PHContentEditingInputRequestOptions = interface(NSObject)
    //['{AC465BF2-7A09-435B-99BD-F5AA0B387F3F}']
    //procedure setCanHandleAdjustmentData(canHandleAdjustmentData: TPhotosCanHandleAdjustmentData); cdecl;
    //function canHandleAdjustmentData: TPhotosCanHandleAdjustmentData; cdecl;
    //procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    //function isNetworkAccessAllowed: Boolean; cdecl;
    //procedure setProgressHandler(progressHandler: TPhotosProgressHandler); cdecl;
    //function progressHandler: TPhotosProgressHandler; cdecl;
  //end;

  //TPHContentEditingInputRequestOptions = class(TOCGenericImport<PHContentEditingInputRequestOptionsClass, PHContentEditingInputRequestOptions>)
  //end;

  //PPHContentEditingInputRequestOptions = Pointer;

  //PHAssetChangeRequestClass = interface(NSObjectClass)
    //['{7A40631E-AD3E-4BE3-8329-733184E08EE3}']
    //{ class } function creationRequestForAssetFromImage(image: UIImage): Pointer { instancetype }; cdecl;
    //{ class } function creationRequestForAssetFromImageAtFileURL(fileURL: NSURL): Pointer { instancetype }; cdecl;
    //{ class } function creationRequestForAssetFromVideoAtFileURL(fileURL: NSURL): Pointer { instancetype }; cdecl;
    //{ class } procedure deleteAssets(assets: Pointer); cdecl;
    //{ class } function changeRequestForAsset(asset: PHAsset): Pointer { instancetype }; cdecl;
  //end;

  //PHAssetChangeRequest = interface(NSObject)
    //['{77EE6917-45FF-4CBF-AB89-2F696879E684}']
    //function placeholderForCreatedAsset: PHObjectPlaceholder; cdecl;
    //procedure setCreationDate(creationDate: NSDate); cdecl;
    //function creationDate: NSDate; cdecl;
    //procedure setLocation(location: CLLocation); cdecl;
    //function location: CLLocation; cdecl;
    //procedure setFavorite(favorite: Boolean); cdecl;
    //function isFavorite: Boolean; cdecl;
    //procedure setHidden(hidden: Boolean); cdecl;
    //function isHidden: Boolean; cdecl;
    //procedure setContentEditingOutput(contentEditingOutput: PHContentEditingOutput); cdecl;
    //function contentEditingOutput: PHContentEditingOutput; cdecl;
    //procedure revertAssetContentToOriginal; cdecl;
  //end;

  //TPHAssetChangeRequest = class(TOCGenericImport<PHAssetChangeRequestClass, PHAssetChangeRequest>)
  //end;

  //PPHAssetChangeRequest = Pointer;

  //PHAssetCollectionChangeRequestClass = interface(NSObjectClass)
    //['{CEC4BF97-FCC8-431D-8CE4-FBFEE8325392}']
    //{ class } function creationRequestForAssetCollectionWithTitle(title: NSString): Pointer { instancetype }; cdecl;
    //{ class } procedure deleteAssetCollections(assetCollections: Pointer); cdecl;
    //[MethodName('changeRequestForAssetCollection:')]
    //{ class } function changeRequestForAssetCollection(assetCollection: PHAssetCollection): Pointer { instancetype }; cdecl;
    //[MethodName('changeRequestForAssetCollection:assets:')]
    //{ class } function changeRequestForAssetCollectionAssets(assetCollection: PHAssetCollection; assets: PHFetchResult): Pointer { instancetype }; cdecl;
  //end;

  //PHAssetCollectionChangeRequest = interface(NSObject)
    //['{2195E6FF-2DDE-4983-8C9A-8E97D5A6E05C}']
    //function placeholderForCreatedAssetCollection: PHObjectPlaceholder; cdecl;
    //procedure setTitle(title: NSString); cdecl;
    //function title: NSString; cdecl;
    //procedure addAssets(assets: Pointer); cdecl;
    //procedure insertAssets(assets: Pointer; atIndexes: NSIndexSet); cdecl;
    //procedure removeAssets(assets: Pointer); cdecl;
    //procedure removeAssetsAtIndexes(indexes: NSIndexSet); cdecl;
    //procedure replaceAssetsAtIndexes(indexes: NSIndexSet; withAssets: Pointer); cdecl;
    //procedure moveAssetsAtIndexes(fromIndexes: NSIndexSet; toIndex: NSUInteger); cdecl;
  //end;

  //TPHAssetCollectionChangeRequest = class(TOCGenericImport<PHAssetCollectionChangeRequestClass, PHAssetCollectionChangeRequest>)
  //end;

  //PPHAssetCollectionChangeRequest = Pointer;

  //PHAssetResourceCreationOptionsClass = interface(NSObjectClass)
    //['{80A8DE5B-9BEC-4D65-AE0C-65D281DF75C0}']
  //end;

  //PHAssetResourceCreationOptions = interface(NSObject)
    //['{C3E48776-66C8-4798-959E-93A709FA2307}']
    //procedure setOriginalFilename(originalFilename: NSString); cdecl;
    //function originalFilename: NSString; cdecl;
    //procedure setUniformTypeIdentifier(uniformTypeIdentifier: NSString); cdecl;
    //function uniformTypeIdentifier: NSString; cdecl;
    //procedure setShouldMoveFile(shouldMoveFile: Boolean); cdecl;
    //function shouldMoveFile: Boolean; cdecl;
  //end;

  //TPHAssetResourceCreationOptions = class(TOCGenericImport<PHAssetResourceCreationOptionsClass, PHAssetResourceCreationOptions>)
  //end;

  //PPHAssetResourceCreationOptions = Pointer;

  //PHAssetCreationRequestClass = interface(PHAssetChangeRequestClass)
    //['{51E2AC6B-5230-401D-A2C6-710EF7F8D6EF}']
    //{ class } function creationRequestForAsset: Pointer { instancetype }; cdecl;
    //{ class } function supportsAssetResourceTypes(types: NSArray): Boolean; cdecl;
  //end;

  //PHAssetCreationRequest = interface(PHAssetChangeRequest)
    //['{E7418E11-0EB9-4ACF-9583-79B6C2F5C890}']
    //[MethodName('addResourceWithType:fileURL:options:')]
    //procedure addResourceWithTypeFileURLOptions(&type: PHAssetResourceType; fileURL: NSURL; options: PHAssetResourceCreationOptions); cdecl;
    //[MethodName('addResourceWithType:data:options:')]
    //procedure addResourceWithTypeDataOptions(&type: PHAssetResourceType; data: NSData; options: PHAssetResourceCreationOptions); cdecl;
  //end;

  //TPHAssetCreationRequest = class(TOCGenericImport<PHAssetCreationRequestClass, PHAssetCreationRequest>)
  //end;

  //PPHAssetCreationRequest = Pointer;

  //PHLivePhotoClass = interface(NSObjectClass)
    //['{B026ACE1-A4D5-4D64-A1C6-C5C862CF7035}']
    //{ class } function requestLivePhotoWithResourceFileURLs(fileURLs: NSArray; placeholderImage: UIImage; targetSize: CGSize; contentMode: PHImageContentMode; resultHandler: TPhotosResultHandler2): PHLivePhotoRequestID; cdecl;
    //{ class } procedure cancelLivePhotoRequestWithRequestID(requestID: PHLivePhotoRequestID); cdecl;
  //end;

  //PHLivePhoto = interface(NSObject)
    //['{9748A1F4-6832-4728-84BA-294CBC5FE154}']
    //function size: CGSize; cdecl;
  //end;

  //TPHLivePhoto = class(TOCGenericImport<PHLivePhotoClass, PHLivePhoto>)
  //end;

  //PPHLivePhoto = Pointer;

  //PHAssetResourceRequestOptionsClass = interface(NSObjectClass)
    //['{68E105E6-083D-4B24-9D72-F0A2B1989625}']
  //end;

  //PHAssetResourceRequestOptions = interface(NSObject)
    //['{A1764F11-BE5F-4145-8BB6-5E344E500047}']
    //procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    //function isNetworkAccessAllowed: Boolean; cdecl;
    //procedure setProgressHandler(progressHandler: PHAssetResourceProgressHandler); cdecl;
    //function progressHandler: PHAssetResourceProgressHandler; cdecl;
  //end;

  //TPHAssetResourceRequestOptions = class(TOCGenericImport<PHAssetResourceRequestOptionsClass, PHAssetResourceRequestOptions>)
  //end;

  //PPHAssetResourceRequestOptions = Pointer;

  //PHAssetResourceManagerClass = interface(NSObjectClass)
    //['{7634E2F6-3D26-4801-9407-9DA44CBC1033}']
    //{ class } function defaultManager: PHAssetResourceManager; cdecl;
  //end;

  //PHAssetResourceManager = interface(NSObject)
    //['{1462F82A-D80C-4DF9-A9A2-C0D03B9CC3CF}']
    //function requestDataForAssetResource(resource: PHAssetResource; options: PHAssetResourceRequestOptions; dataReceivedHandler: TPhotosDataReceivedHandler; completionHandler: TPhotosCompletionHandler2): PHAssetResourceDataRequestID; cdecl;
    //procedure writeDataForAssetResource(resource: PHAssetResource; toFile: NSURL; options: PHAssetResourceRequestOptions; completionHandler: TPhotosCompletionHandler2); cdecl;
    //procedure cancelDataRequest(requestID: PHAssetResourceDataRequestID); cdecl;
  //end;

  //TPHAssetResourceManager = class(TOCGenericImport<PHAssetResourceManagerClass, PHAssetResourceManager>)
  //end;

  //PPHAssetResourceManager = Pointer;

  //PHObjectChangeDetailsClass = interface(NSObjectClass)
    //['{FB0E1E15-8450-4AE2-8611-37F5BE169E79}']
  //end;

  //PHObjectChangeDetails = interface(NSObject)
    //['{321F693B-D084-4A6A-AD9E-FBB2D835036B}']
    //function objectBeforeChanges: ObjectType; cdecl;
    //function objectAfterChanges: ObjectType; cdecl;
    //function assetContentChanged: Boolean; cdecl;
    //function objectWasDeleted: Boolean; cdecl;
  //end;

  //TPHObjectChangeDetails = class(TOCGenericImport<PHObjectChangeDetailsClass, PHObjectChangeDetails>)
  //end;

  //PPHObjectChangeDetails = Pointer;

  //PHFetchResultChangeDetailsClass = interface(NSObjectClass)
    //['{7A5A8AEC-7F44-4CCF-AB0A-7727621726AF}']
    //{ class } function changeDetailsFromFetchResult(fromResult: PHFetchResult; toFetchResult: PHFetchResult; changedObjects: NSArray): Pointer { instancetype }; cdecl;
  //end;

  //PHFetchResultChangeDetails = interface(NSObject)
    //['{17079DA7-895A-494C-9529-1CABABF42706}']
    //function fetchResultBeforeChanges: PHFetchResult; cdecl;
    //function fetchResultAfterChanges: PHFetchResult; cdecl;
    //function hasIncrementalChanges: Boolean; cdecl;
    //function removedIndexes: NSIndexSet; cdecl;
    //function removedObjects: NSArray; cdecl;
    //function insertedIndexes: NSIndexSet; cdecl;
    //function insertedObjects: NSArray; cdecl;
    //function changedIndexes: NSIndexSet; cdecl;
    //function changedObjects: NSArray; cdecl;
    //procedure enumerateMovesWithBlock(handler: TPhotosHandler1); cdecl;
    //function hasMoves: Boolean; cdecl;
  //end;

  //TPHFetchResultChangeDetails = class(TOCGenericImport<PHFetchResultChangeDetailsClass, PHFetchResultChangeDetails>)
  //end;

  //PPHFetchResultChangeDetails = Pointer;

  //PHCollectionListClass = interface(PHCollectionClass)
    //['{2827183F-D245-4EC2-B350-BA2A1A4AD571}']
    //{ class } function fetchCollectionListsContainingCollection(collection: PHCollection; options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function fetchCollectionListsWithLocalIdentifiers(identifiers: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function fetchCollectionListsWithType(collectionListType: PHCollectionListType; subtype: PHCollectionListSubtype; options: PHFetchOptions): PHFetchResult; cdecl;
    //[MethodName('fetchMomentListsWithSubtype:containingMoment:options:')]
    //{ class } function fetchMomentListsWithSubtypeContainingMomentOptions(momentListSubtype: PHCollectionListSubtype; containingMoment: PHAssetCollection; options: PHFetchOptions): PHFetchResult; cdecl;
    //[MethodName('fetchMomentListsWithSubtype:options:')]
    //{ class } function fetchMomentListsWithSubtypeOptions(momentListSubtype: PHCollectionListSubtype; options: PHFetchOptions): PHFetchResult; cdecl;
    //{ class } function transientCollectionListWithCollections(collections: NSArray; title: NSString): PHCollectionList; cdecl;
    //{ class } function transientCollectionListWithCollectionsFetchResult(fetchResult: PHFetchResult; title: NSString): PHCollectionList; cdecl;
  //end;

  //PHCollectionList = interface(PHCollection)
    //['{63208D91-4616-426B-B138-4DEE318CDBB8}']
    //function collectionListType: PHCollectionListType; cdecl;
    //function collectionListSubtype: PHCollectionListSubtype; cdecl;
    //function startDate: NSDate; cdecl;
    //function endDate: NSDate; cdecl;
    //function localizedLocationNames: NSArray; cdecl;
  //end;

  //TPHCollectionList = class(TOCGenericImport<PHCollectionListClass, PHCollectionList>)
  //end;

  //PPHCollectionList = Pointer;

  //PHCollectionListChangeRequestClass = interface(NSObjectClass)
    //['{A18A3D1A-0039-47D4-A97D-11B66E5A3A8E}']
    //{ class } function creationRequestForCollectionListWithTitle(title: NSString): Pointer { instancetype }; cdecl;
    //{ class } procedure deleteCollectionLists(collectionLists: Pointer); cdecl;
    //[MethodName('changeRequestForCollectionList:')]
    //{ class } function changeRequestForCollectionList(collectionList: PHCollectionList): Pointer { instancetype }; cdecl;
    //[MethodName('changeRequestForCollectionList:childCollections:')]
    //{ class } function changeRequestForCollectionListChildCollections(collectionList: PHCollectionList; childCollections: PHFetchResult): Pointer { instancetype }; cdecl;
  //end;

  //PHCollectionListChangeRequest = interface(NSObject)
    //['{02D27868-1261-4673-9D09-92D943F48846}']
    //function placeholderForCreatedCollectionList: PHObjectPlaceholder; cdecl;
    //procedure setTitle(title: NSString); cdecl;
    //function title: NSString; cdecl;
    //procedure addChildCollections(collections: Pointer); cdecl;
    //procedure insertChildCollections(collections: Pointer; atIndexes: NSIndexSet); cdecl;
    //procedure removeChildCollections(collections: Pointer); cdecl;
    //procedure removeChildCollectionsAtIndexes(indexes: NSIndexSet); cdecl;
    //procedure replaceChildCollectionsAtIndexes(indexes: NSIndexSet; withChildCollections: Pointer); cdecl;
    //procedure moveChildCollectionsAtIndexes(indexes: NSIndexSet; toIndex: NSUInteger); cdecl;
  //end;

  //TPHCollectionListChangeRequest = class(TOCGenericImport<PHCollectionListChangeRequestClass, PHCollectionListChangeRequest>)
  //end;

  //PPHCollectionListChangeRequest = Pointer;

  PHImageRequestOptionsClass = interface(NSObjectClass)
    ['{9A0FF6A8-59C4-4357-8FC8-6E6DB94D973A}']
  end;
  PHImageRequestOptions = interface(NSObject)
    ['{5C6EC6D7-C7CD-4451-A56A-9D8568AAE29A}']

    // version
    //@property (nonatomic, assign) PHImageRequestOptionsVersion version;
    procedure setVersion(version: PHImageRequestOptionsVersion); cdecl;
    function version: PHImageRequestOptionsVersion; cdecl;

    // delivery mode. Defaults to PHImageRequestOptionsDeliveryModeOpportunistic
    //@property (nonatomic, assign) PHImageRequestOptionsDeliveryMode deliveryMode;
    procedure setDeliveryMode(deliveryMode: PHImageRequestOptionsDeliveryMode); cdecl;
    function deliveryMode: PHImageRequestOptionsDeliveryMode; cdecl;

    // resize mode. Does not apply when size is PHImageManagerMaximumSize. Defaults to PHImageRequestOptionsResizeModeNone (or no resize)
    //@property (nonatomic, assign) PHImageRequestOptionsResizeMode resizeMode;
    procedure setResizeMode(resizeMode: PHImageRequestOptionsResizeMode); cdecl;
    function resizeMode: PHImageRequestOptionsResizeMode; cdecl;

    // specify crop rectangle in unit coordinates of the original image, such as a face. Defaults to CGRectZero (not applicable)
    //@property (nonatomic, assign) CGRect normalizedCropRect;
    //procedure setNormalizedCropRect(normalizedCropRect: CGRect); cdecl;
    //function normalizedCropRect: CGRect; cdecl;

    // if necessary will download the image from iCloud (client can monitor or cancel using progressHandler). Defaults to NO (see start/stopCachingImagesForAssets)
    //@property (nonatomic, assign, getter=isNetworkAccessAllowed) BOOL networkAccessAllowed;
    procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    function isNetworkAccessAllowed: Boolean; cdecl;

    // return only a single result, blocking until available (or failure). Defaults to NO
    //@property (nonatomic, assign, getter=isSynchronous) BOOL synchronous;
    procedure setSynchronous(synchronous: Boolean); cdecl;
    function isSynchronous: Boolean; cdecl;

    // provide caller a way to be told how much progress has been made prior to delivering the data when it comes from iCloud. Defaults to nil, shall be set by caller
    //@property (nonatomic, copy, nullable) PHAssetImageProgressHandler progressHandler;
    //procedure setProgressHandler(progressHandler: PHAssetImageProgressHandler); cdecl;
    //function progressHandler: PHAssetImageProgressHandler; cdecl;

  end;
  TPHImageRequestOptions = class(TOCGenericImport<PHImageRequestOptionsClass, PHImageRequestOptions>) end;
  PPHImageRequestOptions = Pointer;

  //PHLivePhotoRequestOptionsClass = interface(NSObjectClass)
    //['{F980E179-502A-4932-8601-1280B6A33AAD}']
  //end;

  //PHLivePhotoRequestOptions = interface(NSObject)
    //['{ACF9D07C-486A-433E-BB6E-703D3F7C3203}']
    //procedure setVersion(version: PHImageRequestOptionsVersion); cdecl;
    //function version: PHImageRequestOptionsVersion; cdecl;
    //procedure setDeliveryMode(deliveryMode: PHImageRequestOptionsDeliveryMode); cdecl;
    //function deliveryMode: PHImageRequestOptionsDeliveryMode; cdecl;
    //procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    //function isNetworkAccessAllowed: Boolean; cdecl;
    //procedure setProgressHandler(progressHandler: PHAssetImageProgressHandler); cdecl;
    //function progressHandler: PHAssetImageProgressHandler; cdecl;
  //end;

  //TPHLivePhotoRequestOptions = class(TOCGenericImport<PHLivePhotoRequestOptionsClass, PHLivePhotoRequestOptions>)
  //end;

  //PPHLivePhotoRequestOptions = Pointer;

  //PHVideoRequestOptionsClass = interface(NSObjectClass)
    //['{78236BEB-FA19-4CD8-A5D4-6D31D8D6CA93}']
  //end;

  //PHVideoRequestOptions = interface(NSObject)
    //['{D07B6FA6-5AC3-4886-903D-A511FBB6EBCC}']
    //procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    //function isNetworkAccessAllowed: Boolean; cdecl;
    //procedure setVersion(version: PHVideoRequestOptionsVersion); cdecl;
    //function version: PHVideoRequestOptionsVersion; cdecl;
    //procedure setDeliveryMode(deliveryMode: PHVideoRequestOptionsDeliveryMode); cdecl;
    //function deliveryMode: PHVideoRequestOptionsDeliveryMode; cdecl;
    //procedure setProgressHandler(progressHandler: PHAssetVideoProgressHandler); cdecl;
    //function progressHandler: PHAssetVideoProgressHandler; cdecl;
  //end;

  //TPHVideoRequestOptions = class(TOCGenericImport<PHVideoRequestOptionsClass, PHVideoRequestOptions>)
  //end;

  //PPHVideoRequestOptions = Pointer;

  PHImageManagerClass = interface(NSObjectClass)
    ['{A688D438-61FF-4043-91BC-4C95B4B237E1}']

    //+ (PHImageManager *)defaultManager;
    { class } function defaultManager: PHImageManager; cdecl;

  end;
  PHImageManager = interface(NSObject)
    ['{B0094B28-33E2-4620-9181-FDE2B65C2AD2}']

    // If the asset's aspect ratio does not match that of the given targetSize, contentMode determines how the image will be resized.
    //      PHImageContentModeAspectFit: Fit the asked size by maintaining the aspect ratio, the delivered image may not necessarily be the asked targetSize (see PHImageRequestOptionsDeliveryMode and PHImageRequestOptionsResizeMode)
    //      PHImageContentModeAspectFill: Fill the asked size, some portion of the content may be clipped, the delivered image may not necessarily be the asked targetSize (see PHImageRequestOptionsDeliveryMode && PHImageRequestOptionsResizeMode)
    //      PHImageContentModeDefault: Use PHImageContentModeDefault when size is PHImageManagerMaximumSize (though no scaling/cropping will be done on the result)
    // If -[PHImageRequestOptions isSynchronous] returns NO (or options is nil), resultHandler may be called 1 or more times.
    //     Typically in this case, resultHandler will be called asynchronously on the main thread with the requested results.
    //     However, if deliveryMode = PHImageRequestOptionsDeliveryModeOpportunistic, resultHandler may be called synchronously on the calling thread if any image data is immediately available. If the image data returned in this first pass is of insufficient quality, resultHandler will be called again, asychronously on the main thread at a later time with the "correct" results.
    //     If the request is cancelled, resultHandler may not be called at all.
    // If -[PHImageRequestOptions isSynchronous] returns YES, resultHandler will be called exactly once, synchronously and on the calling thread. Synchronous requests cannot be cancelled.
    // resultHandler for asynchronous requests, always called on main thread
    //- (PHImageRequestID)requestImageForAsset:(PHAsset *)asset targetSize:(CGSize)targetSize contentMode:(PHImageContentMode)contentMode options:(nullable PHImageRequestOptions *)options resultHandler:(void (^)(UIImage *__nullable result, NSDictionary *__nullable info))resultHandler;
    //function requestImageForAsset(asset: PHAsset; targetSize: CGSize; contentMode: PHImageContentMode; options: PHImageRequestOptions; resultHandler: TPhotosResultHandler): PHImageRequestID; cdecl;

    // Request largest represented image as data bytes, resultHandler is called exactly once (deliveryMode is ignored).
    //     If PHImageRequestOptionsVersionCurrent is requested and the asset has adjustments then the largest rendered image data is returned
    //     In all other cases then the original image data is returned
    // resultHandler for asynchronous requests, always called on main thread
    //- (PHImageRequestID)requestImageDataForAsset:(PHAsset *)asset options:(nullable PHImageRequestOptions *)options resultHandler:(void(^)(NSData *__nullable imageData, NSString *__nullable dataUTI, UIImageOrientation orientation, NSDictionary *__nullable info))resultHandler;
    function requestImageDataForAsset(asset: PHAsset; options: PHImageRequestOptions; resultHandler: TPHImageManagerRequestImageDataForAssetResultHandler): PHImageRequestID; cdecl;

    //- (void)cancelImageRequest:(PHImageRequestID)requestID;
    //procedure cancelImageRequest(requestID: PHImageRequestID); cdecl;

    // Requests a live photo representation of the asset. With PHImageRequestOptionsDeliveryModeOpportunistic (or if no options are specified), the resultHandler block may be called more than once (the first call may occur before the method returns). The PHImageResultIsDegradedKey key in the result handler's info parameter indicates when a temporary low-quality live photo is provided.
    //- (PHImageRequestID)requestLivePhotoForAsset:(PHAsset *)asset targetSize:(CGSize)targetSize contentMode:(PHImageContentMode)contentMode options:(nullable PHLivePhotoRequestOptions *)options resultHandler:(void (^)(PHLivePhoto *__nullable livePhoto, NSDictionary *__nullable info))resultHandler PHOTOS_AVAILABLE_IOS_TVOS(9_1, 10_0);
    //function requestLivePhotoForAsset(asset: PHAsset; targetSize: CGSize; contentMode: PHImageContentMode; options: PHLivePhotoRequestOptions; resultHandler: TPhotosResultHandler2): PHImageRequestID; cdecl;

    // Playback only. The result handler is called on an arbitrary queue.
    //- (PHImageRequestID)requestPlayerItemForVideo:(PHAsset *)asset options:(nullable PHVideoRequestOptions *)options resultHandler:(void (^)(AVPlayerItem *__nullable playerItem, NSDictionary *__nullable info))resultHandler;
    //function requestPlayerItemForVideo(asset: PHAsset; options: PHVideoRequestOptions; resultHandler: TPhotosResultHandler3): PHImageRequestID; cdecl;

    // Export. The result handler is called on an arbitrary queue.
    //- (PHImageRequestID)requestExportSessionForVideo:(PHAsset *)asset options:(nullable PHVideoRequestOptions *)options exportPreset:(NSString *)exportPreset resultHandler:(void (^)(AVAssetExportSession *__nullable exportSession, NSDictionary *__nullable info))resultHandler;
    //function requestExportSessionForVideo(asset: PHAsset; options: PHVideoRequestOptions; exportPreset: NSString; resultHandler: TPhotosResultHandler4): PHImageRequestID; cdecl;

    // Everything else. The result handler is called on an arbitrary queue.
    //- (PHImageRequestID)requestAVAssetForVideo:(PHAsset *)asset options:(nullable PHVideoRequestOptions *)options resultHandler:(void (^)(AVAsset *__nullable asset, AVAudioMix *__nullable audioMix, NSDictionary *__nullable info))resultHandler;
    //function requestAVAssetForVideo(asset: PHAsset; options: PHVideoRequestOptions; resultHandler: TPhotosResultHandler5): PHImageRequestID; cdecl;

  end;
  TPHImageManager = class(TOCGenericImport<PHImageManagerClass, PHImageManager>) end;
  PPHImageManager = Pointer;

  //PHCachingImageManagerClass = interface(PHImageManagerClass)
    //['{F6C15D70-802A-4F16-99BC-1C278F177C3C}']
  //end;

  //PHCachingImageManager = interface(PHImageManager)
    //['{410D5173-68CA-48F2-BAF7-3CB110AE0ADA}']
    //procedure setAllowsCachingHighQualityImages(allowsCachingHighQualityImages: Boolean); cdecl;
    //function allowsCachingHighQualityImages: Boolean; cdecl;
    //procedure startCachingImagesForAssets(assets: NSArray; targetSize: CGSize; contentMode: PHImageContentMode; options: PHImageRequestOptions); cdecl;
    //procedure stopCachingImagesForAssets(assets: NSArray; targetSize: CGSize; contentMode: PHImageContentMode; options: PHImageRequestOptions); cdecl;
    //procedure stopCachingImagesForAllAssets; cdecl;
  //end;

  //TPHCachingImageManager = class(TOCGenericImport<PHCachingImageManagerClass, PHCachingImageManager>)
  //end;

  //PPHCachingImageManager = Pointer;

  //PHLivePhotoEditingContextClass = interface(NSObjectClass)
    //['{A36296E2-99A5-4475-8514-27DFBCB2C564}']
  //end;

  //PHLivePhotoEditingContext = interface(NSObject)
    //['{DC6065BC-9472-484B-8D97-3E20DFD881D2}']
    //function initWithLivePhotoEditingInput(livePhotoInput: PHContentEditingInput): Pointer { instancetype }; cdecl;
    //function fullSizeImage: CIImage; cdecl;
    //function duration: CMTime; cdecl;
    //function photoTime: CMTime; cdecl;
    //procedure setFrameProcessor(frameProcessor: PHLivePhotoFrameProcessingBlock); cdecl;
    //function frameProcessor: PHLivePhotoFrameProcessingBlock; cdecl;
    //procedure setAudioVolume(audioVolume: Single); cdecl;
    //function audioVolume: Single; cdecl;
    //function orientation: CGImagePropertyOrientation; cdecl;
    //procedure prepareLivePhotoForPlaybackWithTargetSize(targetSize: CGSize; options: NSDictionary; completionHandler: TPhotosCompletionHandler3); cdecl;
    //procedure saveLivePhotoToOutput(output: PHContentEditingOutput; options: NSDictionary; completionHandler: TPhotosCompletionHandler); cdecl;
    //procedure cancel; cdecl;
  //end;

  //TPHLivePhotoEditingContext = class(TOCGenericImport<PHLivePhotoEditingContextClass, PHLivePhotoEditingContext>)
  //end;

  //PPHLivePhotoEditingContext = Pointer;

  // ===== Protocol declarations =====

  //PHPhotoLibraryChangeObserver = interface(IObjectiveC)
    //['{E67D6ACE-1DA9-4F69-913F-FC7E80EA0C19}']
    //procedure photoLibraryDidChange(changeInstance: PHChange); cdecl;
  //end;

  //PHLivePhotoFrame = interface(IObjectiveC)
    //['{EA23C374-A1B7-411D-9F61-64C3CDA5C773}']
    //function image: CIImage; cdecl;
    //function time: CMTime; cdecl;
    //function &type: PHLivePhotoFrameType; cdecl;
    //function renderScale: CGFloat; cdecl;
  //end;

  // ===== Exported string consts =====

//function PHContentEditingInputResultIsInCloudKey: NSString;
//function PHContentEditingInputCancelledKey: NSString;
//function PHContentEditingInputErrorKey: NSString;
//function PHImageManagerMaximumSize: Pointer;
//function PHImageResultIsInCloudKey: NSString;
//function PHImageResultIsDegradedKey: NSString;
//function PHImageResultRequestIDKey: NSString;
//function PHImageCancelledKey: NSString;
function PHImageErrorKey: NSString;
//function PHLivePhotoInfoErrorKey: NSString;
//function PHLivePhotoInfoIsDegradedKey: NSString;
//function PHLivePhotoInfoCancelledKey: NSString;
//function PHLivePhotoShouldRenderAtPlaybackTime: Pointer;


// ===== External functions =====

const
  libPhotos = '/System/Library/Frameworks/Photos.framework/Photos';

implementation

// {$IF defined(IOS) and NOT defined(CPUARM)} => https://stackoverflow.com/questions/52475704/how-to-link-correctly-apple-ios-library

uses
  Posix.Dlfcn;

var
  PhotosModule: THandle;

// {$ENDIF IOS} => https://stackoverflow.com/questions/52475704/how-to-link-correctly-apple-ios-library

//function PHContentEditingInputResultIsInCloudKey: NSString;
//begin
//  Result := CocoaNSStringConst(libPhotos, 'PHContentEditingInputResultIsInCloudKey');
//end;

//function PHContentEditingInputCancelledKey: NSString;
//begin
//  Result := CocoaNSStringConst(libPhotos, 'PHContentEditingInputCancelledKey');
//end;

//function PHContentEditingInputErrorKey: NSString;
//begin
//  Result := CocoaNSStringConst(libPhotos, 'PHContentEditingInputErrorKey');
//end;

//function PHImageResultIsInCloudKey: NSString;
//begin
//  Result := CocoaNSStringConst(libPhotos, 'PHImageResultIsInCloudKey');
//end;

//function PHImageResultIsDegradedKey: NSString;
//begin
//  Result := CocoaNSStringConst(libPhotos, 'PHImageResultIsDegradedKey');
//end;

//function PHImageResultRequestIDKey: NSString;
//begin
//  Result := CocoaNSStringConst(libPhotos, 'PHImageResultRequestIDKey');
//end;

//function PHImageCancelledKey: NSString;
//begin
//  Result := CocoaNSStringConst(libPhotos, 'PHImageCancelledKey');
//end;

// key (NSError): NSFileManager or iCloud Photo Library errors
//extern NSString *const PHImageErrorKey PHOTOS_AVAILABLE_IOS_TVOS(8_0, 10_0);
function PHImageErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHImageErrorKey');
end;

//function PHLivePhotoInfoErrorKey: NSString;
//begin
//  Result := CocoaNSStringConst(libPhotos, 'PHLivePhotoInfoErrorKey');
//end;

//function PHLivePhotoInfoIsDegradedKey: NSString;
//begin
//  Result := CocoaNSStringConst(libPhotos, 'PHLivePhotoInfoIsDegradedKey');
//end;

//function PHLivePhotoInfoCancelledKey: NSString;
//begin
//  Result := CocoaNSStringConst(libPhotos, 'PHLivePhotoInfoCancelledKey');
//end;

//function PHImageManagerMaximumSize: Pointer;
//begin
//  Result := CocoaPointerConst(libPhotos, 'PHImageManagerMaximumSize');
//end;

//function PHLivePhotoShouldRenderAtPlaybackTime: Pointer;
//begin
//  Result := CocoaPointerConst(libPhotos, 'PHLivePhotoShouldRenderAtPlaybackTime');
//end;

// {$IF defined(IOS) and NOT defined(CPUARM)} => https://stackoverflow.com/questions/52475704/how-to-link-correctly-apple-ios-library

initialization

PhotosModule := dlopen(MarshaledAString(libPhotos), RTLD_LAZY);

finalization

dlclose(PhotosModule);

// {$ENDIF IOS} => https://stackoverflow.com/questions/52475704/how-to-link-correctly-apple-ios-library

end.
