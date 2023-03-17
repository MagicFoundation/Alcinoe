unit Alcinoe.iOSApi.ImageIO;

{$WEAKPACKAGEUNIT}

interface

uses
  Posix.StdDef,
  iOSapi.Foundation,
  iOSapi.CoreGraphics,
  Macapi.CoreFoundation;

//const
//  kCGImageMetadataTypeInvalid = -1;
//  {$EXTERNALSYM kCGImageMetadataTypeInvalid}
//  kCGImageMetadataTypeDefault = 0;
//  {$EXTERNALSYM kCGImageMetadataTypeDefault}
//  kCGImageMetadataTypeString = 1;
//  {$EXTERNALSYM kCGImageMetadataTypeString}
//  kCGImageMetadataTypeArrayUnordered = 2;
//  {$EXTERNALSYM kCGImageMetadataTypeArrayUnordered}
//  kCGImageMetadataTypeArrayOrdered = 3;
//  {$EXTERNALSYM kCGImageMetadataTypeArrayOrdered}
//  kCGImageMetadataTypeAlternateArray = 4;
//  {$EXTERNALSYM kCGImageMetadataTypeAlternateArray}
//  kCGImageMetadataTypeAlternateText = 5;
//  {$EXTERNALSYM kCGImageMetadataTypeAlternateText}
//  kCGImageMetadataTypeStructure = 6;
//  {$EXTERNALSYM kCGImageMetadataTypeStructure}
//  kCGImageMetadataErrorUnknown = 0;
//  {$EXTERNALSYM kCGImageMetadataErrorUnknown}
//  kCGImageMetadataErrorUnsupportedFormat = 1;
//  {$EXTERNALSYM kCGImageMetadataErrorUnsupportedFormat}
//  kCGImageMetadataErrorBadArgument = 2;
//  {$EXTERNALSYM kCGImageMetadataErrorBadArgument}
//  kCGImageMetadataErrorConflictingArguments = 3;
//  {$EXTERNALSYM kCGImageMetadataErrorConflictingArguments}
//  kCGImageMetadataErrorPrefixConflict = 4;
//  {$EXTERNALSYM kCGImageMetadataErrorPrefixConflict}
//  kCGImageStatusUnexpectedEOF = -5;
//  {$EXTERNALSYM kCGImageStatusUnexpectedEOF}
//  kCGImageStatusInvalidData = -4;
//  {$EXTERNALSYM kCGImageStatusInvalidData}
//  kCGImageStatusUnknownType = -3;
//  {$EXTERNALSYM kCGImageStatusUnknownType}
//  kCGImageStatusReadingHeader = -2;
//  {$EXTERNALSYM kCGImageStatusReadingHeader}
//  kCGImageStatusIncomplete = -1;
//  {$EXTERNALSYM kCGImageStatusIncomplete}
//  kCGImageStatusComplete = 0;
//  {$EXTERNALSYM kCGImageStatusComplete}

type
//  // ===== Framework typedefs =====
//  {$M+}
//  CGImageDestinationRef = Pointer;
//  {$EXTERNALSYM CGImageDestinationRef}
  CGImageSourceRef = Pointer;
  {$EXTERNALSYM CGImageSourceRef}
//  CGImageMetadataRef = Pointer;
//  {$EXTERNALSYM CGImageMetadataRef}
//  CGMutableImageMetadataRef = Pointer;
//  {$EXTERNALSYM CGMutableImageMetadataRef}
//  CGImageMetadataTagRef = Pointer;
//  {$EXTERNALSYM CGImageMetadataTagRef}
//  CGImageMetadataType = Integer;
//  {$EXTERNALSYM CGImageMetadataType}
//  CGImageMetadataTagBlock = function(param1: CFStringRef; param2: CGImageMetadataTagRef): Integer; cdecl;
//  {$EXTERNALSYM CGImageMetadataTagBlock}
//  CGImageMetadataErrors = Cardinal;
//  {$EXTERNALSYM CGImageMetadataErrors}
//  CGImageSourceStatus = Integer;
//  {$EXTERNALSYM CGImageSourceStatus}

  // ===== Exported string consts =====

//function kCGImageMetadataNamespaceExif: NSString;
//function kCGImageMetadataNamespaceExifAux: NSString;
//function kCGImageMetadataNamespaceExifEX: NSString;
//function kCGImageMetadataNamespaceDublinCore: NSString;
//function kCGImageMetadataNamespaceIPTCCore: NSString;
//function kCGImageMetadataNamespacePhotoshop: NSString;
//function kCGImageMetadataNamespaceTIFF: NSString;
//function kCGImageMetadataNamespaceXMPBasic: NSString;
//function kCGImageMetadataNamespaceXMPRights: NSString;
//function kCGImageMetadataPrefixExif: NSString;
//function kCGImageMetadataPrefixExifAux: NSString;
//function kCGImageMetadataPrefixExifEX: NSString;
//function kCGImageMetadataPrefixDublinCore: NSString;
//function kCGImageMetadataPrefixIPTCCore: NSString;
//function kCGImageMetadataPrefixPhotoshop: NSString;
//function kCGImageMetadataPrefixTIFF: NSString;
//function kCGImageMetadataPrefixXMPBasic: NSString;
//function kCGImageMetadataPrefixXMPRights: NSString;
//function kCGImageMetadataEnumerateRecursively: NSString;
//function kCFErrorDomainCGImageMetadata: NSString;
//function kCGImageSourceTypeIdentifierHint: NSString;
//function kCGImageSourceShouldCache: NSString;
//function kCGImageSourceShouldCacheImmediately: NSString;
//function kCGImageSourceShouldAllowFloat: NSString;
//function kCGImageSourceCreateThumbnailFromImageIfAbsent: NSString;
//function kCGImageSourceCreateThumbnailFromImageAlways: NSString;
//function kCGImageSourceThumbnailMaxPixelSize: NSString;
//function kCGImageSourceCreateThumbnailWithTransform: NSString;
//function kCGImageDestinationLossyCompressionQuality: NSString;
//function kCGImageDestinationBackgroundColor: NSString;
//function kCGImageDestinationMetadata: NSString;
//function kCGImageDestinationMergeMetadata: NSString;
//function kCGImageMetadataShouldExcludeXMP: NSString;
//function kCGImageDestinationDateTime: NSString;
//function kCGImageDestinationOrientation: NSString;
//function kCGImagePropertyTIFFDictionary: NSString;
//function kCGImagePropertyGIFDictionary: NSString;
//function kCGImagePropertyJFIFDictionary: NSString;
function kCGImagePropertyExifDictionary: NSString;
//function kCGImagePropertyPNGDictionary: NSString;
//function kCGImagePropertyIPTCDictionary: NSString;
//function kCGImagePropertyGPSDictionary: NSString;
//function kCGImagePropertyRawDictionary: NSString;
//function kCGImagePropertyCIFFDictionary: NSString;
//function kCGImagePropertyMakerCanonDictionary: NSString;
//function kCGImagePropertyMakerNikonDictionary: NSString;
//function kCGImagePropertyMakerMinoltaDictionary: NSString;
//function kCGImagePropertyMakerFujiDictionary: NSString;
//function kCGImagePropertyMakerOlympusDictionary: NSString;
//function kCGImagePropertyMakerPentaxDictionary: NSString;
//function kCGImageProperty8BIMDictionary: NSString;
//function kCGImagePropertyDNGDictionary: NSString;
//function kCGImagePropertyExifAuxDictionary: NSString;
//function kCGImagePropertyOpenEXRDictionary: NSString;
//function kCGImagePropertyFileSize: NSString;
//function kCGImagePropertyPixelHeight: NSString;
//function kCGImagePropertyPixelWidth: NSString;
//function kCGImagePropertyDPIHeight: NSString;
//function kCGImagePropertyDPIWidth: NSString;
//function kCGImagePropertyDepth: NSString;
function kCGImagePropertyOrientation: NSString;
//function kCGImagePropertyIsFloat: NSString;
//function kCGImagePropertyIsIndexed: NSString;
//function kCGImagePropertyHasAlpha: NSString;
//function kCGImagePropertyColorModel: NSString;
//function kCGImagePropertyProfileName: NSString;
//function kCGImagePropertyColorModelRGB: NSString;
//function kCGImagePropertyColorModelGray: NSString;
//function kCGImagePropertyColorModelCMYK: NSString;
//function kCGImagePropertyColorModelLab: NSString;
//function kCGImagePropertyTIFFCompression: NSString;
//function kCGImagePropertyTIFFPhotometricInterpretation: NSString;
//function kCGImagePropertyTIFFDocumentName: NSString;
//function kCGImagePropertyTIFFImageDescription: NSString;
//function kCGImagePropertyTIFFMake: NSString;
//function kCGImagePropertyTIFFModel: NSString;
//function kCGImagePropertyTIFFOrientation: NSString;
//function kCGImagePropertyTIFFXResolution: NSString;
//function kCGImagePropertyTIFFYResolution: NSString;
//function kCGImagePropertyTIFFResolutionUnit: NSString;
//function kCGImagePropertyTIFFSoftware: NSString;
//function kCGImagePropertyTIFFTransferFunction: NSString;
//function kCGImagePropertyTIFFDateTime: NSString;
//function kCGImagePropertyTIFFArtist: NSString;
//function kCGImagePropertyTIFFHostComputer: NSString;
//function kCGImagePropertyTIFFCopyright: NSString;
//function kCGImagePropertyTIFFWhitePoint: NSString;
//function kCGImagePropertyTIFFPrimaryChromaticities: NSString;
//function kCGImagePropertyJFIFVersion: NSString;
//function kCGImagePropertyJFIFXDensity: NSString;
//function kCGImagePropertyJFIFYDensity: NSString;
//function kCGImagePropertyJFIFDensityUnit: NSString;
//function kCGImagePropertyJFIFIsProgressive: NSString;
//function kCGImagePropertyExifExposureTime: NSString;
//function kCGImagePropertyExifFNumber: NSString;
//function kCGImagePropertyExifExposureProgram: NSString;
//function kCGImagePropertyExifSpectralSensitivity: NSString;
//function kCGImagePropertyExifISOSpeedRatings: NSString;
//function kCGImagePropertyExifOECF: NSString;
//function kCGImagePropertyExifSensitivityType: NSString;
//function kCGImagePropertyExifStandardOutputSensitivity: NSString;
//function kCGImagePropertyExifRecommendedExposureIndex: NSString;
//function kCGImagePropertyExifISOSpeed: NSString;
//function kCGImagePropertyExifISOSpeedLatitudeyyy: NSString;
//function kCGImagePropertyExifISOSpeedLatitudezzz: NSString;
//function kCGImagePropertyExifVersion: NSString;
//function kCGImagePropertyExifDateTimeOriginal: NSString;
//function kCGImagePropertyExifDateTimeDigitized: NSString;
//function kCGImagePropertyExifComponentsConfiguration: NSString;
//function kCGImagePropertyExifCompressedBitsPerPixel: NSString;
//function kCGImagePropertyExifShutterSpeedValue: NSString;
//function kCGImagePropertyExifApertureValue: NSString;
//function kCGImagePropertyExifBrightnessValue: NSString;
//function kCGImagePropertyExifExposureBiasValue: NSString;
//function kCGImagePropertyExifMaxApertureValue: NSString;
//function kCGImagePropertyExifSubjectDistance: NSString;
//function kCGImagePropertyExifMeteringMode: NSString;
//function kCGImagePropertyExifLightSource: NSString;
//function kCGImagePropertyExifFlash: NSString;
//function kCGImagePropertyExifFocalLength: NSString;
//function kCGImagePropertyExifSubjectArea: NSString;
//function kCGImagePropertyExifMakerNote: NSString;
//function kCGImagePropertyExifUserComment: NSString;
//function kCGImagePropertyExifSubsecTime: NSString;
//function kCGImagePropertyExifSubsecTimeOrginal: NSString;
//function kCGImagePropertyExifSubsecTimeDigitized: NSString;
//function kCGImagePropertyExifFlashPixVersion: NSString;
//function kCGImagePropertyExifColorSpace: NSString;
//function kCGImagePropertyExifPixelXDimension: NSString;
//function kCGImagePropertyExifPixelYDimension: NSString;
//function kCGImagePropertyExifRelatedSoundFile: NSString;
//function kCGImagePropertyExifFlashEnergy: NSString;
//function kCGImagePropertyExifSpatialFrequencyResponse: NSString;
//function kCGImagePropertyExifFocalPlaneXResolution: NSString;
//function kCGImagePropertyExifFocalPlaneYResolution: NSString;
//function kCGImagePropertyExifFocalPlaneResolutionUnit: NSString;
//function kCGImagePropertyExifSubjectLocation: NSString;
//function kCGImagePropertyExifExposureIndex: NSString;
//function kCGImagePropertyExifSensingMethod: NSString;
//function kCGImagePropertyExifFileSource: NSString;
//function kCGImagePropertyExifSceneType: NSString;
//function kCGImagePropertyExifCFAPattern: NSString;
//function kCGImagePropertyExifCustomRendered: NSString;
//function kCGImagePropertyExifExposureMode: NSString;
//function kCGImagePropertyExifWhiteBalance: NSString;
//function kCGImagePropertyExifDigitalZoomRatio: NSString;
//function kCGImagePropertyExifFocalLenIn35mmFilm: NSString;
//function kCGImagePropertyExifSceneCaptureType: NSString;
//function kCGImagePropertyExifGainControl: NSString;
//function kCGImagePropertyExifContrast: NSString;
//function kCGImagePropertyExifSaturation: NSString;
//function kCGImagePropertyExifSharpness: NSString;
//function kCGImagePropertyExifDeviceSettingDescription: NSString;
//function kCGImagePropertyExifSubjectDistRange: NSString;
//function kCGImagePropertyExifImageUniqueID: NSString;
//function kCGImagePropertyExifCameraOwnerName: NSString;
//function kCGImagePropertyExifBodySerialNumber: NSString;
//function kCGImagePropertyExifLensSpecification: NSString;
//function kCGImagePropertyExifLensMake: NSString;
//function kCGImagePropertyExifLensModel: NSString;
//function kCGImagePropertyExifLensSerialNumber: NSString;
//function kCGImagePropertyExifGamma: NSString;
//function kCGImagePropertyExifAuxLensInfo: NSString;
//function kCGImagePropertyExifAuxLensModel: NSString;
//function kCGImagePropertyExifAuxSerialNumber: NSString;
//function kCGImagePropertyExifAuxLensID: NSString;
//function kCGImagePropertyExifAuxLensSerialNumber: NSString;
//function kCGImagePropertyExifAuxImageNumber: NSString;
//function kCGImagePropertyExifAuxFlashCompensation: NSString;
//function kCGImagePropertyExifAuxOwnerName: NSString;
//function kCGImagePropertyExifAuxFirmware: NSString;
//function kCGImagePropertyGIFLoopCount: NSString;
//function kCGImagePropertyGIFDelayTime: NSString;
//function kCGImagePropertyGIFImageColorMap: NSString;
//function kCGImagePropertyGIFHasGlobalColorMap: NSString;
//function kCGImagePropertyGIFUnclampedDelayTime: NSString;
//function kCGImagePropertyPNGGamma: NSString;
//function kCGImagePropertyPNGInterlaceType: NSString;
//function kCGImagePropertyPNGXPixelsPerMeter: NSString;
//function kCGImagePropertyPNGYPixelsPerMeter: NSString;
//function kCGImagePropertyPNGsRGBIntent: NSString;
//function kCGImagePropertyPNGChromaticities: NSString;
//function kCGImagePropertyPNGAuthor: NSString;
//function kCGImagePropertyPNGCopyright: NSString;
//function kCGImagePropertyPNGCreationTime: NSString;
//function kCGImagePropertyPNGDescription: NSString;
//function kCGImagePropertyPNGModificationTime: NSString;
//function kCGImagePropertyPNGSoftware: NSString;
//function kCGImagePropertyPNGTitle: NSString;
//function kCGImagePropertyGPSVersion: NSString;
//function kCGImagePropertyGPSLatitudeRef: NSString;
//function kCGImagePropertyGPSLatitude: NSString;
//function kCGImagePropertyGPSLongitudeRef: NSString;
//function kCGImagePropertyGPSLongitude: NSString;
//function kCGImagePropertyGPSAltitudeRef: NSString;
//function kCGImagePropertyGPSAltitude: NSString;
//function kCGImagePropertyGPSTimeStamp: NSString;
//function kCGImagePropertyGPSSatellites: NSString;
//function kCGImagePropertyGPSStatus: NSString;
//function kCGImagePropertyGPSMeasureMode: NSString;
//function kCGImagePropertyGPSDOP: NSString;
//function kCGImagePropertyGPSSpeedRef: NSString;
//function kCGImagePropertyGPSSpeed: NSString;
//function kCGImagePropertyGPSTrackRef: NSString;
//function kCGImagePropertyGPSTrack: NSString;
//function kCGImagePropertyGPSImgDirectionRef: NSString;
//function kCGImagePropertyGPSImgDirection: NSString;
//function kCGImagePropertyGPSMapDatum: NSString;
//function kCGImagePropertyGPSDestLatitudeRef: NSString;
//function kCGImagePropertyGPSDestLatitude: NSString;
//function kCGImagePropertyGPSDestLongitudeRef: NSString;
//function kCGImagePropertyGPSDestLongitude: NSString;
//function kCGImagePropertyGPSDestBearingRef: NSString;
//function kCGImagePropertyGPSDestBearing: NSString;
//function kCGImagePropertyGPSDestDistanceRef: NSString;
//function kCGImagePropertyGPSDestDistance: NSString;
//function kCGImagePropertyGPSProcessingMethod: NSString;
//function kCGImagePropertyGPSAreaInformation: NSString;
//function kCGImagePropertyGPSDateStamp: NSString;
//function kCGImagePropertyGPSDifferental: NSString;
//function kCGImagePropertyIPTCObjectTypeReference: NSString;
//function kCGImagePropertyIPTCObjectAttributeReference: NSString;
//function kCGImagePropertyIPTCObjectName: NSString;
//function kCGImagePropertyIPTCEditStatus: NSString;
//function kCGImagePropertyIPTCEditorialUpdate: NSString;
//function kCGImagePropertyIPTCUrgency: NSString;
//function kCGImagePropertyIPTCSubjectReference: NSString;
//function kCGImagePropertyIPTCCategory: NSString;
//function kCGImagePropertyIPTCSupplementalCategory: NSString;
//function kCGImagePropertyIPTCFixtureIdentifier: NSString;
//function kCGImagePropertyIPTCKeywords: NSString;
//function kCGImagePropertyIPTCContentLocationCode: NSString;
//function kCGImagePropertyIPTCContentLocationName: NSString;
//function kCGImagePropertyIPTCReleaseDate: NSString;
//function kCGImagePropertyIPTCReleaseTime: NSString;
//function kCGImagePropertyIPTCExpirationDate: NSString;
//function kCGImagePropertyIPTCExpirationTime: NSString;
//function kCGImagePropertyIPTCSpecialInstructions: NSString;
//function kCGImagePropertyIPTCActionAdvised: NSString;
//function kCGImagePropertyIPTCReferenceService: NSString;
//function kCGImagePropertyIPTCReferenceDate: NSString;
//function kCGImagePropertyIPTCReferenceNumber: NSString;
//function kCGImagePropertyIPTCDateCreated: NSString;
//function kCGImagePropertyIPTCTimeCreated: NSString;
//function kCGImagePropertyIPTCDigitalCreationDate: NSString;
//function kCGImagePropertyIPTCDigitalCreationTime: NSString;
//function kCGImagePropertyIPTCOriginatingProgram: NSString;
//function kCGImagePropertyIPTCProgramVersion: NSString;
//function kCGImagePropertyIPTCObjectCycle: NSString;
//function kCGImagePropertyIPTCByline: NSString;
//function kCGImagePropertyIPTCBylineTitle: NSString;
//function kCGImagePropertyIPTCCity: NSString;
//function kCGImagePropertyIPTCSubLocation: NSString;
//function kCGImagePropertyIPTCProvinceState: NSString;
//function kCGImagePropertyIPTCCountryPrimaryLocationCode: NSString;
//function kCGImagePropertyIPTCCountryPrimaryLocationName: NSString;
//function kCGImagePropertyIPTCOriginalTransmissionReference: NSString;
//function kCGImagePropertyIPTCHeadline: NSString;
//function kCGImagePropertyIPTCCredit: NSString;
//function kCGImagePropertyIPTCSource: NSString;
//function kCGImagePropertyIPTCCopyrightNotice: NSString;
//function kCGImagePropertyIPTCContact: NSString;
//function kCGImagePropertyIPTCCaptionAbstract: NSString;
//function kCGImagePropertyIPTCWriterEditor: NSString;
//function kCGImagePropertyIPTCImageType: NSString;
//function kCGImagePropertyIPTCImageOrientation: NSString;
//function kCGImagePropertyIPTCLanguageIdentifier: NSString;
//function kCGImagePropertyIPTCStarRating: NSString;
//function kCGImagePropertyIPTCCreatorContactInfo: NSString;
//function kCGImagePropertyIPTCRightsUsageTerms: NSString;
//function kCGImagePropertyIPTCScene: NSString;
//function kCGImagePropertyIPTCContactInfoCity: NSString;
//function kCGImagePropertyIPTCContactInfoCountry: NSString;
//function kCGImagePropertyIPTCContactInfoAddress: NSString;
//function kCGImagePropertyIPTCContactInfoPostalCode: NSString;
//function kCGImagePropertyIPTCContactInfoStateProvince: NSString;
//function kCGImagePropertyIPTCContactInfoEmails: NSString;
//function kCGImagePropertyIPTCContactInfoPhones: NSString;
//function kCGImagePropertyIPTCContactInfoWebURLs: NSString;
//function kCGImageProperty8BIMLayerNames: NSString;
//function kCGImagePropertyDNGVersion: NSString;
//function kCGImagePropertyDNGBackwardVersion: NSString;
//function kCGImagePropertyDNGUniqueCameraModel: NSString;
//function kCGImagePropertyDNGLocalizedCameraModel: NSString;
//function kCGImagePropertyDNGCameraSerialNumber: NSString;
//function kCGImagePropertyDNGLensInfo: NSString;
//function kCGImagePropertyCIFFDescription: NSString;
//function kCGImagePropertyCIFFFirmware: NSString;
//function kCGImagePropertyCIFFOwnerName: NSString;
//function kCGImagePropertyCIFFImageName: NSString;
//function kCGImagePropertyCIFFImageFileName: NSString;
//function kCGImagePropertyCIFFReleaseMethod: NSString;
//function kCGImagePropertyCIFFReleaseTiming: NSString;
//function kCGImagePropertyCIFFRecordID: NSString;
//function kCGImagePropertyCIFFSelfTimingTime: NSString;
//function kCGImagePropertyCIFFCameraSerialNumber: NSString;
//function kCGImagePropertyCIFFImageSerialNumber: NSString;
//function kCGImagePropertyCIFFContinuousDrive: NSString;
//function kCGImagePropertyCIFFFocusMode: NSString;
//function kCGImagePropertyCIFFMeteringMode: NSString;
//function kCGImagePropertyCIFFShootingMode: NSString;
//function kCGImagePropertyCIFFLensModel: NSString;
//function kCGImagePropertyCIFFLensMaxMM: NSString;
//function kCGImagePropertyCIFFLensMinMM: NSString;
//function kCGImagePropertyCIFFWhiteBalanceIndex: NSString;
//function kCGImagePropertyCIFFFlashExposureComp: NSString;
//function kCGImagePropertyCIFFMeasuredEV: NSString;
//function kCGImagePropertyMakerNikonISOSetting: NSString;
//function kCGImagePropertyMakerNikonColorMode: NSString;
//function kCGImagePropertyMakerNikonQuality: NSString;
//function kCGImagePropertyMakerNikonWhiteBalanceMode: NSString;
//function kCGImagePropertyMakerNikonSharpenMode: NSString;
//function kCGImagePropertyMakerNikonFocusMode: NSString;
//function kCGImagePropertyMakerNikonFlashSetting: NSString;
//function kCGImagePropertyMakerNikonISOSelection: NSString;
//function kCGImagePropertyMakerNikonFlashExposureComp: NSString;
//function kCGImagePropertyMakerNikonImageAdjustment: NSString;
//function kCGImagePropertyMakerNikonLensAdapter: NSString;
//function kCGImagePropertyMakerNikonLensType: NSString;
//function kCGImagePropertyMakerNikonLensInfo: NSString;
//function kCGImagePropertyMakerNikonFocusDistance: NSString;
//function kCGImagePropertyMakerNikonDigitalZoom: NSString;
//function kCGImagePropertyMakerNikonShootingMode: NSString;
//function kCGImagePropertyMakerNikonCameraSerialNumber: NSString;
//function kCGImagePropertyMakerNikonShutterCount: NSString;
//function kCGImagePropertyMakerCanonOwnerName: NSString;
//function kCGImagePropertyMakerCanonCameraSerialNumber: NSString;
//function kCGImagePropertyMakerCanonImageSerialNumber: NSString;
//function kCGImagePropertyMakerCanonFlashExposureComp: NSString;
//function kCGImagePropertyMakerCanonContinuousDrive: NSString;
//function kCGImagePropertyMakerCanonLensModel: NSString;
//function kCGImagePropertyMakerCanonFirmware: NSString;
//function kCGImagePropertyMakerCanonAspectRatioInfo: NSString;
//function kCGImagePropertyOpenEXRAspectRatio: NSString;

// ===== External functions =====

const
  libImageIO = '/System/Library/Frameworks/ImageIO.framework/ImageIO';

//function CGImageMetadataGetTypeID: CFTypeID; cdecl; external libImageIO name _PU + 'CGImageMetadataGetTypeID';
//{$EXTERNALSYM CGImageMetadataGetTypeID}
//function CGImageMetadataCreateMutable: CGMutableImageMetadataRef; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataCreateMutable';
//{$EXTERNALSYM CGImageMetadataCreateMutable}
//function CGImageMetadataCreateMutableCopy(metadata: CGImageMetadataRef): CGMutableImageMetadataRef; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataCreateMutableCopy';
//{$EXTERNALSYM CGImageMetadataCreateMutableCopy}
//function CGImageMetadataTagGetTypeID: CFTypeID; cdecl; external libImageIO name _PU + 'CGImageMetadataTagGetTypeID';
//{$EXTERNALSYM CGImageMetadataTagGetTypeID}
//function CGImageMetadataTagCreate(xmlns: CFStringRef; prefix: CFStringRef; name: CFStringRef;
//  &type: CGImageMetadataType; value: CFTypeRef): CGImageMetadataTagRef; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataTagCreate';
//{$EXTERNALSYM CGImageMetadataTagCreate}
//function CGImageMetadataTagCopyNamespace(tag: CGImageMetadataTagRef): CFStringRef; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataTagCopyNamespace';
//{$EXTERNALSYM CGImageMetadataTagCopyNamespace}
//function CGImageMetadataTagCopyPrefix(tag: CGImageMetadataTagRef): CFStringRef; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataTagCopyPrefix';
//{$EXTERNALSYM CGImageMetadataTagCopyPrefix}
//function CGImageMetadataTagCopyName(tag: CGImageMetadataTagRef): CFStringRef; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataTagCopyName';
//{$EXTERNALSYM CGImageMetadataTagCopyName}
//function CGImageMetadataTagCopyValue(tag: CGImageMetadataTagRef): CFTypeRef; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataTagCopyValue';
//{$EXTERNALSYM CGImageMetadataTagCopyValue}
//function CGImageMetadataTagGetType(tag: CGImageMetadataTagRef): CGImageMetadataType; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataTagGetType';
//{$EXTERNALSYM CGImageMetadataTagGetType}
//function CGImageMetadataTagCopyQualifiers(tag: CGImageMetadataTagRef): CFArrayRef; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataTagCopyQualifiers';
//{$EXTERNALSYM CGImageMetadataTagCopyQualifiers}
//function CGImageMetadataCopyTags(metadata: CGImageMetadataRef): CFArrayRef; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataCopyTags';
//{$EXTERNALSYM CGImageMetadataCopyTags}
//function CGImageMetadataCopyTagWithPath(metadata: CGImageMetadataRef; parent: CGImageMetadataTagRef; path: CFStringRef)
//  : CGImageMetadataTagRef; cdecl; external libImageIO name _PU + 'CGImageMetadataCopyTagWithPath';
//{$EXTERNALSYM CGImageMetadataCopyTagWithPath}
//function CGImageMetadataCopyStringValueWithPath(metadata: CGImageMetadataRef; parent: CGImageMetadataTagRef;
//  path: CFStringRef): CFStringRef; cdecl; external libImageIO name _PU + 'CGImageMetadataCopyStringValueWithPath';
//{$EXTERNALSYM CGImageMetadataCopyStringValueWithPath}
//function CGImageMetadataRegisterNamespaceForPrefix(metadata: CGMutableImageMetadataRef; xmlns: CFStringRef;
//  prefix: CFStringRef; err: CFErrorRef): Integer; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataRegisterNamespaceForPrefix';
//{$EXTERNALSYM CGImageMetadataRegisterNamespaceForPrefix}
//function CGImageMetadataSetTagWithPath(metadata: CGMutableImageMetadataRef; parent: CGImageMetadataTagRef;
//  path: CFStringRef; tag: CGImageMetadataTagRef): Integer; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataSetTagWithPath';
//{$EXTERNALSYM CGImageMetadataSetTagWithPath}
//function CGImageMetadataSetValueWithPath(metadata: CGMutableImageMetadataRef; parent: CGImageMetadataTagRef;
//  path: CFStringRef; value: CFTypeRef): Integer; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataSetValueWithPath';
//{$EXTERNALSYM CGImageMetadataSetValueWithPath}
//function CGImageMetadataRemoveTagWithPath(metadata: CGMutableImageMetadataRef; parent: CGImageMetadataTagRef;
//  path: CFStringRef): Integer; cdecl; external libImageIO name _PU + 'CGImageMetadataRemoveTagWithPath';
//{$EXTERNALSYM CGImageMetadataRemoveTagWithPath}
//procedure CGImageMetadataEnumerateTagsUsingBlock(metadata: CGImageMetadataRef; rootPath: CFStringRef;
//  options: CFDictionaryRef; block: CGImageMetadataTagBlock); cdecl;
//  external libImageIO name _PU + 'CGImageMetadataEnumerateTagsUsingBlock';
//{$EXTERNALSYM CGImageMetadataEnumerateTagsUsingBlock}
//function CGImageMetadataCopyTagMatchingImageProperty(metadata: CGImageMetadataRef; dictionaryName: CFStringRef;
//  propertyName: CFStringRef): CGImageMetadataTagRef; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataCopyTagMatchingImageProperty';
//{$EXTERNALSYM CGImageMetadataCopyTagMatchingImageProperty}
//function CGImageMetadataSetValueMatchingImageProperty(metadata: CGMutableImageMetadataRef; dictionaryName: CFStringRef;
//  propertyName: CFStringRef; value: CFTypeRef): Integer; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataSetValueMatchingImageProperty';
//{$EXTERNALSYM CGImageMetadataSetValueMatchingImageProperty}
//function CGImageMetadataCreateXMPData(metadata: CGImageMetadataRef; options: CFDictionaryRef): CFDataRef; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataCreateXMPData';
//{$EXTERNALSYM CGImageMetadataCreateXMPData}
//function CGImageMetadataCreateFromXMPData(data: CFDataRef): CGImageMetadataRef; cdecl;
//  external libImageIO name _PU + 'CGImageMetadataCreateFromXMPData';
//{$EXTERNALSYM CGImageMetadataCreateFromXMPData}
//function CGImageSourceGetTypeID: CFTypeID; cdecl; external libImageIO name _PU + 'CGImageSourceGetTypeID';
//{$EXTERNALSYM CGImageSourceGetTypeID}
//function CGImageSourceCopyTypeIdentifiers: CFArrayRef; cdecl;
//  external libImageIO name _PU + 'CGImageSourceCopyTypeIdentifiers';
//{$EXTERNALSYM CGImageSourceCopyTypeIdentifiers}
//function CGImageSourceCreateWithDataProvider(provider: CGDataProviderRef; options: CFDictionaryRef): CGImageSourceRef;
//  cdecl; external libImageIO name _PU + 'CGImageSourceCreateWithDataProvider';
//{$EXTERNALSYM CGImageSourceCreateWithDataProvider}
//function CGImageSourceCreateWithData(data: CFDataRef; options: CFDictionaryRef): CGImageSourceRef; cdecl;
//  external libImageIO name _PU + 'CGImageSourceCreateWithData';
//{$EXTERNALSYM CGImageSourceCreateWithData}
function CGImageSourceCreateWithURL(url: CFURLRef; options: CFDictionaryRef): CGImageSourceRef; cdecl;
  external libImageIO name _PU + 'CGImageSourceCreateWithURL';
{$EXTERNALSYM CGImageSourceCreateWithURL}
//function CGImageSourceGetType(isrc: CGImageSourceRef): CFStringRef; cdecl;
//  external libImageIO name _PU + 'CGImageSourceGetType';
//{$EXTERNALSYM CGImageSourceGetType}
//function CGImageSourceGetCount(isrc: CGImageSourceRef): size_t; cdecl;
//  external libImageIO name _PU + 'CGImageSourceGetCount';
//{$EXTERNALSYM CGImageSourceGetCount}
//function CGImageSourceCopyProperties(isrc: CGImageSourceRef; options: CFDictionaryRef): CFDictionaryRef; cdecl;
//  external libImageIO name _PU + 'CGImageSourceCopyProperties';
//{$EXTERNALSYM CGImageSourceCopyProperties}
function CGImageSourceCopyPropertiesAtIndex(isrc: CGImageSourceRef; index: size_t; options: CFDictionaryRef)
  : CFDictionaryRef; cdecl; external libImageIO name _PU + 'CGImageSourceCopyPropertiesAtIndex';
{$EXTERNALSYM CGImageSourceCopyPropertiesAtIndex}
//function CGImageSourceCopyMetadataAtIndex(isrc: CGImageSourceRef; index: size_t; options: CFDictionaryRef)
//  : CGImageMetadataRef; cdecl; external libImageIO name _PU + 'CGImageSourceCopyMetadataAtIndex';
//{$EXTERNALSYM CGImageSourceCopyMetadataAtIndex}
function CGImageSourceCreateImageAtIndex(isrc: CGImageSourceRef; index: size_t; options: CFDictionaryRef): CGImageRef;
  cdecl; external libImageIO name _PU + 'CGImageSourceCreateImageAtIndex';
{$EXTERNALSYM CGImageSourceCreateImageAtIndex}
//procedure CGImageSourceRemoveCacheAtIndex(isrc: CGImageSourceRef; index: size_t); cdecl;
//  external libImageIO name _PU + 'CGImageSourceRemoveCacheAtIndex';
//{$EXTERNALSYM CGImageSourceRemoveCacheAtIndex}
//function CGImageSourceCreateThumbnailAtIndex(isrc: CGImageSourceRef; index: size_t; options: CFDictionaryRef)
//  : CGImageRef; cdecl; external libImageIO name _PU + 'CGImageSourceCreateThumbnailAtIndex';
//{$EXTERNALSYM CGImageSourceCreateThumbnailAtIndex}
//function CGImageSourceCreateIncremental(options: CFDictionaryRef): CGImageSourceRef; cdecl;
//  external libImageIO name _PU + 'CGImageSourceCreateIncremental';
//{$EXTERNALSYM CGImageSourceCreateIncremental}
//procedure CGImageSourceUpdateData(isrc: CGImageSourceRef; data: CFDataRef; final: Integer); cdecl;
//  external libImageIO name _PU + 'CGImageSourceUpdateData';
//{$EXTERNALSYM CGImageSourceUpdateData}
//procedure CGImageSourceUpdateDataProvider(isrc: CGImageSourceRef; provider: CGDataProviderRef; final: Integer); cdecl;
//  external libImageIO name _PU + 'CGImageSourceUpdateDataProvider';
//{$EXTERNALSYM CGImageSourceUpdateDataProvider}
//function CGImageSourceGetStatus(isrc: CGImageSourceRef): CGImageSourceStatus; cdecl;
//  external libImageIO name _PU + 'CGImageSourceGetStatus';
//{$EXTERNALSYM CGImageSourceGetStatus}
//function CGImageSourceGetStatusAtIndex(isrc: CGImageSourceRef; index: size_t): CGImageSourceStatus; cdecl;
//  external libImageIO name _PU + 'CGImageSourceGetStatusAtIndex';
//{$EXTERNALSYM CGImageSourceGetStatusAtIndex}
//function CGImageDestinationGetTypeID: CFTypeID; cdecl; external libImageIO name _PU + 'CGImageDestinationGetTypeID';
//{$EXTERNALSYM CGImageDestinationGetTypeID}
//function CGImageDestinationCopyTypeIdentifiers: CFArrayRef; cdecl;
//  external libImageIO name _PU + 'CGImageDestinationCopyTypeIdentifiers';
//{$EXTERNALSYM CGImageDestinationCopyTypeIdentifiers}
//function CGImageDestinationCreateWithDataConsumer(consumer: CGDataConsumerRef; &type: CFStringRef; count: size_t;
//  options: CFDictionaryRef): CGImageDestinationRef; cdecl;
//  external libImageIO name _PU + 'CGImageDestinationCreateWithDataConsumer';
//{$EXTERNALSYM CGImageDestinationCreateWithDataConsumer}
//function CGImageDestinationCreateWithData(data: CFMutableDataRef; &type: CFStringRef; count: size_t;
//  options: CFDictionaryRef): CGImageDestinationRef; cdecl;
//  external libImageIO name _PU + 'CGImageDestinationCreateWithData';
//{$EXTERNALSYM CGImageDestinationCreateWithData}
//function CGImageDestinationCreateWithURL(url: CFURLRef; &type: CFStringRef; count: size_t; options: CFDictionaryRef)
//  : CGImageDestinationRef; cdecl; external libImageIO name _PU + 'CGImageDestinationCreateWithURL';
//{$EXTERNALSYM CGImageDestinationCreateWithURL}
//procedure CGImageDestinationSetProperties(idst: CGImageDestinationRef; properties: CFDictionaryRef); cdecl;
//  external libImageIO name _PU + 'CGImageDestinationSetProperties';
//{$EXTERNALSYM CGImageDestinationSetProperties}
//procedure CGImageDestinationAddImage(idst: CGImageDestinationRef; image: CGImageRef; properties: CFDictionaryRef);
//  cdecl; external libImageIO name _PU + 'CGImageDestinationAddImage';
//{$EXTERNALSYM CGImageDestinationAddImage}
//procedure CGImageDestinationAddImageFromSource(idst: CGImageDestinationRef; isrc: CGImageSourceRef; index: size_t;
//  properties: CFDictionaryRef); cdecl; external libImageIO name _PU + 'CGImageDestinationAddImageFromSource';
//{$EXTERNALSYM CGImageDestinationAddImageFromSource}
//function CGImageDestinationFinalize(idst: CGImageDestinationRef): Integer; cdecl;
//  external libImageIO name _PU + 'CGImageDestinationFinalize';
//{$EXTERNALSYM CGImageDestinationFinalize}
//procedure CGImageDestinationAddImageAndMetadata(idst: CGImageDestinationRef; image: CGImageRef;
//  metadata: CGImageMetadataRef; options: CFDictionaryRef); cdecl;
//  external libImageIO name _PU + 'CGImageDestinationAddImageAndMetadata';
//{$EXTERNALSYM CGImageDestinationAddImageAndMetadata}
//function CGImageDestinationCopyImageSource(idst: CGImageDestinationRef; isrc: CGImageSourceRef;
//  options: CFDictionaryRef; err: CFErrorRef): Integer; cdecl;
//  external libImageIO name _PU + 'CGImageDestinationCopyImageSource';
//{$EXTERNALSYM CGImageDestinationCopyImageSource}

implementation

//function kCGImageMetadataNamespaceExif: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataNamespaceExif');
//end;

//function kCGImageMetadataNamespaceExifAux: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataNamespaceExifAux');
//end;

//function kCGImageMetadataNamespaceExifEX: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataNamespaceExifEX');
//end;

//function kCGImageMetadataNamespaceDublinCore: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataNamespaceDublinCore');
//end;

//function kCGImageMetadataNamespaceIPTCCore: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataNamespaceIPTCCore');
//end;

//function kCGImageMetadataNamespacePhotoshop: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataNamespacePhotoshop');
//end;

//function kCGImageMetadataNamespaceTIFF: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataNamespaceTIFF');
//end;

//function kCGImageMetadataNamespaceXMPBasic: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataNamespaceXMPBasic');
//end;

//function kCGImageMetadataNamespaceXMPRights: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataNamespaceXMPRights');
//end;

//function kCGImageMetadataPrefixExif: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataPrefixExif');
//end;

//function kCGImageMetadataPrefixExifAux: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataPrefixExifAux');
//end;

//function kCGImageMetadataPrefixExifEX: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataPrefixExifEX');
//end;

//function kCGImageMetadataPrefixDublinCore: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataPrefixDublinCore');
//end;

//function kCGImageMetadataPrefixIPTCCore: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataPrefixIPTCCore');
//end;

//function kCGImageMetadataPrefixPhotoshop: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataPrefixPhotoshop');
//end;

//function kCGImageMetadataPrefixTIFF: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataPrefixTIFF');
//end;

//function kCGImageMetadataPrefixXMPBasic: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataPrefixXMPBasic');
//end;

//function kCGImageMetadataPrefixXMPRights: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataPrefixXMPRights');
//end;

//function kCGImageMetadataEnumerateRecursively: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataEnumerateRecursively');
//end;

//function kCFErrorDomainCGImageMetadata: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCFErrorDomainCGImageMetadata');
//end;

//function kCGImageSourceTypeIdentifierHint: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageSourceTypeIdentifierHint');
//end;

//function kCGImageSourceShouldCache: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageSourceShouldCache');
//end;

//function kCGImageSourceShouldCacheImmediately: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageSourceShouldCacheImmediately');
//end;

//function kCGImageSourceShouldAllowFloat: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageSourceShouldAllowFloat');
//end;

//function kCGImageSourceCreateThumbnailFromImageIfAbsent: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageSourceCreateThumbnailFromImageIfAbsent');
//end;

//function kCGImageSourceCreateThumbnailFromImageAlways: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageSourceCreateThumbnailFromImageAlways');
//end;

//function kCGImageSourceThumbnailMaxPixelSize: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageSourceThumbnailMaxPixelSize');
//end;

//function kCGImageSourceCreateThumbnailWithTransform: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageSourceCreateThumbnailWithTransform');
//end;

//function kCGImageDestinationLossyCompressionQuality: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageDestinationLossyCompressionQuality');
//end;

//function kCGImageDestinationBackgroundColor: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageDestinationBackgroundColor');
//end;

//function kCGImageDestinationMetadata: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageDestinationMetadata');
//end;

//function kCGImageDestinationMergeMetadata: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageDestinationMergeMetadata');
//end;

//function kCGImageMetadataShouldExcludeXMP: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageMetadataShouldExcludeXMP');
//end;

//function kCGImageDestinationDateTime: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageDestinationDateTime');
//end;

//function kCGImageDestinationOrientation: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageDestinationOrientation');
//end;

//function kCGImagePropertyTIFFDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFDictionary');
//end;

//function kCGImagePropertyGIFDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGIFDictionary');
//end;

//function kCGImagePropertyJFIFDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyJFIFDictionary');
//end;

{************************************************}
function kCGImagePropertyExifDictionary: NSString;
begin
  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifDictionary');
end;

//function kCGImagePropertyPNGDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGDictionary');
//end;

//function kCGImagePropertyIPTCDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCDictionary');
//end;

//function kCGImagePropertyGPSDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDictionary');
//end;

//function kCGImagePropertyRawDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyRawDictionary');
//end;

//function kCGImagePropertyCIFFDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFDictionary');
//end;

//function kCGImagePropertyMakerCanonDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerCanonDictionary');
//end;

//function kCGImagePropertyMakerNikonDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonDictionary');
//end;

//function kCGImagePropertyMakerMinoltaDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerMinoltaDictionary');
//end;

//function kCGImagePropertyMakerFujiDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerFujiDictionary');
//end;

//function kCGImagePropertyMakerOlympusDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerOlympusDictionary');
//end;

//function kCGImagePropertyMakerPentaxDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerPentaxDictionary');
//end;

//function kCGImageProperty8BIMDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageProperty8BIMDictionary');
//end;

//function kCGImagePropertyDNGDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyDNGDictionary');
//end;

//function kCGImagePropertyExifAuxDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifAuxDictionary');
//end;

//function kCGImagePropertyOpenEXRDictionary: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyOpenEXRDictionary');
//end;

//function kCGImagePropertyFileSize: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyFileSize');
//end;

//function kCGImagePropertyPixelHeight: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPixelHeight');
//end;

//function kCGImagePropertyPixelWidth: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPixelWidth');
//end;

//function kCGImagePropertyDPIHeight: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyDPIHeight');
//end;

//function kCGImagePropertyDPIWidth: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyDPIWidth');
//end;

//function kCGImagePropertyDepth: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyDepth');
//end;

{*********************************************}
function kCGImagePropertyOrientation: NSString;
begin
  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyOrientation');
end;

//function kCGImagePropertyIsFloat: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIsFloat');
//end;

//function kCGImagePropertyIsIndexed: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIsIndexed');
//end;

//function kCGImagePropertyHasAlpha: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyHasAlpha');
//end;

//function kCGImagePropertyColorModel: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyColorModel');
//end;

//function kCGImagePropertyProfileName: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyProfileName');
//end;

//function kCGImagePropertyColorModelRGB: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyColorModelRGB');
//end;

//function kCGImagePropertyColorModelGray: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyColorModelGray');
//end;

//function kCGImagePropertyColorModelCMYK: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyColorModelCMYK');
//end;

//function kCGImagePropertyColorModelLab: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyColorModelLab');
//end;

//function kCGImagePropertyTIFFCompression: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFCompression');
//end;

//function kCGImagePropertyTIFFPhotometricInterpretation: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFPhotometricInterpretation');
//end;

//function kCGImagePropertyTIFFDocumentName: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFDocumentName');
//end;

//function kCGImagePropertyTIFFImageDescription: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFImageDescription');
//end;

//function kCGImagePropertyTIFFMake: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFMake');
//end;

//function kCGImagePropertyTIFFModel: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFModel');
//end;

//function kCGImagePropertyTIFFOrientation: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFOrientation');
//end;

//function kCGImagePropertyTIFFXResolution: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFXResolution');
//end;

//function kCGImagePropertyTIFFYResolution: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFYResolution');
//end;

//function kCGImagePropertyTIFFResolutionUnit: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFResolutionUnit');
//end;

//function kCGImagePropertyTIFFSoftware: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFSoftware');
//end;

//function kCGImagePropertyTIFFTransferFunction: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFTransferFunction');
//end;

//function kCGImagePropertyTIFFDateTime: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFDateTime');
//end;

//function kCGImagePropertyTIFFArtist: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFArtist');
//end;

//function kCGImagePropertyTIFFHostComputer: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFHostComputer');
//end;

//function kCGImagePropertyTIFFCopyright: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFCopyright');
//end;

//function kCGImagePropertyTIFFWhitePoint: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFWhitePoint');
//end;

//function kCGImagePropertyTIFFPrimaryChromaticities: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyTIFFPrimaryChromaticities');
//end;

//function kCGImagePropertyJFIFVersion: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyJFIFVersion');
//end;

//function kCGImagePropertyJFIFXDensity: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyJFIFXDensity');
//end;

//function kCGImagePropertyJFIFYDensity: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyJFIFYDensity');
//end;

//function kCGImagePropertyJFIFDensityUnit: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyJFIFDensityUnit');
//end;

//function kCGImagePropertyJFIFIsProgressive: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyJFIFIsProgressive');
//end;

//function kCGImagePropertyExifExposureTime: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifExposureTime');
//end;

//function kCGImagePropertyExifFNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifFNumber');
//end;

//function kCGImagePropertyExifExposureProgram: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifExposureProgram');
//end;

//function kCGImagePropertyExifSpectralSensitivity: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSpectralSensitivity');
//end;

//function kCGImagePropertyExifISOSpeedRatings: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifISOSpeedRatings');
//end;

//function kCGImagePropertyExifOECF: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifOECF');
//end;

//function kCGImagePropertyExifSensitivityType: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSensitivityType');
//end;

//function kCGImagePropertyExifStandardOutputSensitivity: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifStandardOutputSensitivity');
//end;

//function kCGImagePropertyExifRecommendedExposureIndex: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifRecommendedExposureIndex');
//end;

//function kCGImagePropertyExifISOSpeed: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifISOSpeed');
//end;

//function kCGImagePropertyExifISOSpeedLatitudeyyy: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifISOSpeedLatitudeyyy');
//end;

//function kCGImagePropertyExifISOSpeedLatitudezzz: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifISOSpeedLatitudezzz');
//end;

//function kCGImagePropertyExifVersion: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifVersion');
//end;

//function kCGImagePropertyExifDateTimeOriginal: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifDateTimeOriginal');
//end;

//function kCGImagePropertyExifDateTimeDigitized: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifDateTimeDigitized');
//end;

//function kCGImagePropertyExifComponentsConfiguration: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifComponentsConfiguration');
//end;

//function kCGImagePropertyExifCompressedBitsPerPixel: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifCompressedBitsPerPixel');
//end;

//function kCGImagePropertyExifShutterSpeedValue: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifShutterSpeedValue');
//end;

//function kCGImagePropertyExifApertureValue: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifApertureValue');
//end;

//function kCGImagePropertyExifBrightnessValue: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifBrightnessValue');
//end;

//function kCGImagePropertyExifExposureBiasValue: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifExposureBiasValue');
//end;

//function kCGImagePropertyExifMaxApertureValue: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifMaxApertureValue');
//end;

//function kCGImagePropertyExifSubjectDistance: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSubjectDistance');
//end;

//function kCGImagePropertyExifMeteringMode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifMeteringMode');
//end;

//function kCGImagePropertyExifLightSource: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifLightSource');
//end;

//function kCGImagePropertyExifFlash: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifFlash');
//end;

//function kCGImagePropertyExifFocalLength: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifFocalLength');
//end;

//function kCGImagePropertyExifSubjectArea: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSubjectArea');
//end;

//function kCGImagePropertyExifMakerNote: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifMakerNote');
//end;

//function kCGImagePropertyExifUserComment: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifUserComment');
//end;

//function kCGImagePropertyExifSubsecTime: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSubsecTime');
//end;

//function kCGImagePropertyExifSubsecTimeOrginal: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSubsecTimeOrginal');
//end;

//function kCGImagePropertyExifSubsecTimeDigitized: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSubsecTimeDigitized');
//end;

//function kCGImagePropertyExifFlashPixVersion: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifFlashPixVersion');
//end;

//function kCGImagePropertyExifColorSpace: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifColorSpace');
//end;

//function kCGImagePropertyExifPixelXDimension: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifPixelXDimension');
//end;

//function kCGImagePropertyExifPixelYDimension: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifPixelYDimension');
//end;

//function kCGImagePropertyExifRelatedSoundFile: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifRelatedSoundFile');
//end;

//function kCGImagePropertyExifFlashEnergy: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifFlashEnergy');
//end;

//function kCGImagePropertyExifSpatialFrequencyResponse: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSpatialFrequencyResponse');
//end;

//function kCGImagePropertyExifFocalPlaneXResolution: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifFocalPlaneXResolution');
//end;

//function kCGImagePropertyExifFocalPlaneYResolution: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifFocalPlaneYResolution');
//end;

//function kCGImagePropertyExifFocalPlaneResolutionUnit: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifFocalPlaneResolutionUnit');
//end;

//function kCGImagePropertyExifSubjectLocation: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSubjectLocation');
//end;

//function kCGImagePropertyExifExposureIndex: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifExposureIndex');
//end;

//function kCGImagePropertyExifSensingMethod: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSensingMethod');
//end;

//function kCGImagePropertyExifFileSource: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifFileSource');
//end;

//function kCGImagePropertyExifSceneType: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSceneType');
//end;

//function kCGImagePropertyExifCFAPattern: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifCFAPattern');
//end;

//function kCGImagePropertyExifCustomRendered: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifCustomRendered');
//end;

//function kCGImagePropertyExifExposureMode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifExposureMode');
//end;

//function kCGImagePropertyExifWhiteBalance: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifWhiteBalance');
//end;

//function kCGImagePropertyExifDigitalZoomRatio: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifDigitalZoomRatio');
//end;

//function kCGImagePropertyExifFocalLenIn35mmFilm: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifFocalLenIn35mmFilm');
//end;

//function kCGImagePropertyExifSceneCaptureType: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSceneCaptureType');
//end;

//function kCGImagePropertyExifGainControl: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifGainControl');
//end;

//function kCGImagePropertyExifContrast: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifContrast');
//end;

//function kCGImagePropertyExifSaturation: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSaturation');
//end;

//function kCGImagePropertyExifSharpness: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSharpness');
//end;

//function kCGImagePropertyExifDeviceSettingDescription: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifDeviceSettingDescription');
//end;

//function kCGImagePropertyExifSubjectDistRange: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifSubjectDistRange');
//end;

//function kCGImagePropertyExifImageUniqueID: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifImageUniqueID');
//end;

//function kCGImagePropertyExifCameraOwnerName: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifCameraOwnerName');
//end;

//function kCGImagePropertyExifBodySerialNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifBodySerialNumber');
//end;

//function kCGImagePropertyExifLensSpecification: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifLensSpecification');
//end;

//function kCGImagePropertyExifLensMake: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifLensMake');
//end;

//function kCGImagePropertyExifLensModel: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifLensModel');
//end;

//function kCGImagePropertyExifLensSerialNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifLensSerialNumber');
//end;

//function kCGImagePropertyExifGamma: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifGamma');
//end;

//function kCGImagePropertyExifAuxLensInfo: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifAuxLensInfo');
//end;

//function kCGImagePropertyExifAuxLensModel: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifAuxLensModel');
//end;

//function kCGImagePropertyExifAuxSerialNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifAuxSerialNumber');
//end;

//function kCGImagePropertyExifAuxLensID: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifAuxLensID');
//end;

//function kCGImagePropertyExifAuxLensSerialNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifAuxLensSerialNumber');
//end;

//function kCGImagePropertyExifAuxImageNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifAuxImageNumber');
//end;

//function kCGImagePropertyExifAuxFlashCompensation: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifAuxFlashCompensation');
//end;

//function kCGImagePropertyExifAuxOwnerName: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifAuxOwnerName');
//end;

//function kCGImagePropertyExifAuxFirmware: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifAuxFirmware');
//end;

//function kCGImagePropertyGIFLoopCount: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGIFLoopCount');
//end;

//function kCGImagePropertyGIFDelayTime: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGIFDelayTime');
//end;

//function kCGImagePropertyGIFImageColorMap: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGIFImageColorMap');
//end;

//function kCGImagePropertyGIFHasGlobalColorMap: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGIFHasGlobalColorMap');
//end;

//function kCGImagePropertyGIFUnclampedDelayTime: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGIFUnclampedDelayTime');
//end;

//function kCGImagePropertyPNGGamma: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGGamma');
//end;

//function kCGImagePropertyPNGInterlaceType: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGInterlaceType');
//end;

//function kCGImagePropertyPNGXPixelsPerMeter: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGXPixelsPerMeter');
//end;

//function kCGImagePropertyPNGYPixelsPerMeter: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGYPixelsPerMeter');
//end;

//function kCGImagePropertyPNGsRGBIntent: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGsRGBIntent');
//end;

//function kCGImagePropertyPNGChromaticities: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGChromaticities');
//end;

//function kCGImagePropertyPNGAuthor: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGAuthor');
//end;

//function kCGImagePropertyPNGCopyright: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGCopyright');
//end;

//function kCGImagePropertyPNGCreationTime: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGCreationTime');
//end;

//function kCGImagePropertyPNGDescription: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGDescription');
//end;

//function kCGImagePropertyPNGModificationTime: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGModificationTime');
//end;

//function kCGImagePropertyPNGSoftware: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGSoftware');
//end;

//function kCGImagePropertyPNGTitle: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyPNGTitle');
//end;

//function kCGImagePropertyGPSVersion: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSVersion');
//end;

//function kCGImagePropertyGPSLatitudeRef: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSLatitudeRef');
//end;

//function kCGImagePropertyGPSLatitude: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSLatitude');
//end;

//function kCGImagePropertyGPSLongitudeRef: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSLongitudeRef');
//end;

//function kCGImagePropertyGPSLongitude: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSLongitude');
//end;

//function kCGImagePropertyGPSAltitudeRef: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSAltitudeRef');
//end;

//function kCGImagePropertyGPSAltitude: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSAltitude');
//end;

//function kCGImagePropertyGPSTimeStamp: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSTimeStamp');
//end;

//function kCGImagePropertyGPSSatellites: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSSatellites');
//end;

//function kCGImagePropertyGPSStatus: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSStatus');
//end;

//function kCGImagePropertyGPSMeasureMode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSMeasureMode');
//end;

//function kCGImagePropertyGPSDOP: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDOP');
//end;

//function kCGImagePropertyGPSSpeedRef: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSSpeedRef');
//end;

//function kCGImagePropertyGPSSpeed: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSSpeed');
//end;

//function kCGImagePropertyGPSTrackRef: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSTrackRef');
//end;

//function kCGImagePropertyGPSTrack: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSTrack');
//end;

//function kCGImagePropertyGPSImgDirectionRef: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSImgDirectionRef');
//end;

//function kCGImagePropertyGPSImgDirection: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSImgDirection');
//end;

//function kCGImagePropertyGPSMapDatum: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSMapDatum');
//end;

//function kCGImagePropertyGPSDestLatitudeRef: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDestLatitudeRef');
//end;

//function kCGImagePropertyGPSDestLatitude: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDestLatitude');
//end;

//function kCGImagePropertyGPSDestLongitudeRef: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDestLongitudeRef');
//end;

//function kCGImagePropertyGPSDestLongitude: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDestLongitude');
//end;

//function kCGImagePropertyGPSDestBearingRef: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDestBearingRef');
//end;

//function kCGImagePropertyGPSDestBearing: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDestBearing');
//end;

//function kCGImagePropertyGPSDestDistanceRef: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDestDistanceRef');
//end;

//function kCGImagePropertyGPSDestDistance: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDestDistance');
//end;

//function kCGImagePropertyGPSProcessingMethod: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSProcessingMethod');
//end;

//function kCGImagePropertyGPSAreaInformation: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSAreaInformation');
//end;

//function kCGImagePropertyGPSDateStamp: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDateStamp');
//end;

//function kCGImagePropertyGPSDifferental: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDifferental');
//end;

//function kCGImagePropertyIPTCObjectTypeReference: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCObjectTypeReference');
//end;

//function kCGImagePropertyIPTCObjectAttributeReference: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCObjectAttributeReference');
//end;

//function kCGImagePropertyIPTCObjectName: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCObjectName');
//end;

//function kCGImagePropertyIPTCEditStatus: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCEditStatus');
//end;

//function kCGImagePropertyIPTCEditorialUpdate: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCEditorialUpdate');
//end;

//function kCGImagePropertyIPTCUrgency: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCUrgency');
//end;

//function kCGImagePropertyIPTCSubjectReference: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCSubjectReference');
//end;

//function kCGImagePropertyIPTCCategory: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCCategory');
//end;

//function kCGImagePropertyIPTCSupplementalCategory: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCSupplementalCategory');
//end;

//function kCGImagePropertyIPTCFixtureIdentifier: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCFixtureIdentifier');
//end;

//function kCGImagePropertyIPTCKeywords: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCKeywords');
//end;

//function kCGImagePropertyIPTCContentLocationCode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCContentLocationCode');
//end;

//function kCGImagePropertyIPTCContentLocationName: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCContentLocationName');
//end;

//function kCGImagePropertyIPTCReleaseDate: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCReleaseDate');
//end;

//function kCGImagePropertyIPTCReleaseTime: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCReleaseTime');
//end;

//function kCGImagePropertyIPTCExpirationDate: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCExpirationDate');
//end;

//function kCGImagePropertyIPTCExpirationTime: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCExpirationTime');
//end;

//function kCGImagePropertyIPTCSpecialInstructions: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCSpecialInstructions');
//end;

//function kCGImagePropertyIPTCActionAdvised: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCActionAdvised');
//end;

//function kCGImagePropertyIPTCReferenceService: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCReferenceService');
//end;

//function kCGImagePropertyIPTCReferenceDate: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCReferenceDate');
//end;

//function kCGImagePropertyIPTCReferenceNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCReferenceNumber');
//end;

//function kCGImagePropertyIPTCDateCreated: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCDateCreated');
//end;

//function kCGImagePropertyIPTCTimeCreated: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCTimeCreated');
//end;

//function kCGImagePropertyIPTCDigitalCreationDate: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCDigitalCreationDate');
//end;

//function kCGImagePropertyIPTCDigitalCreationTime: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCDigitalCreationTime');
//end;

//function kCGImagePropertyIPTCOriginatingProgram: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCOriginatingProgram');
//end;

//function kCGImagePropertyIPTCProgramVersion: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCProgramVersion');
//end;

//function kCGImagePropertyIPTCObjectCycle: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCObjectCycle');
//end;

//function kCGImagePropertyIPTCByline: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCByline');
//end;

//function kCGImagePropertyIPTCBylineTitle: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCBylineTitle');
//end;

//function kCGImagePropertyIPTCCity: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCCity');
//end;

//function kCGImagePropertyIPTCSubLocation: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCSubLocation');
//end;

//function kCGImagePropertyIPTCProvinceState: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCProvinceState');
//end;

//function kCGImagePropertyIPTCCountryPrimaryLocationCode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCCountryPrimaryLocationCode');
//end;

//function kCGImagePropertyIPTCCountryPrimaryLocationName: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCCountryPrimaryLocationName');
//end;

//function kCGImagePropertyIPTCOriginalTransmissionReference: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCOriginalTransmissionReference');
//end;

//function kCGImagePropertyIPTCHeadline: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCHeadline');
//end;

//function kCGImagePropertyIPTCCredit: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCCredit');
//end;

//function kCGImagePropertyIPTCSource: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCSource');
//end;

//function kCGImagePropertyIPTCCopyrightNotice: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCCopyrightNotice');
//end;

//function kCGImagePropertyIPTCContact: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCContact');
//end;

//function kCGImagePropertyIPTCCaptionAbstract: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCCaptionAbstract');
//end;

//function kCGImagePropertyIPTCWriterEditor: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCWriterEditor');
//end;

//function kCGImagePropertyIPTCImageType: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCImageType');
//end;

//function kCGImagePropertyIPTCImageOrientation: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCImageOrientation');
//end;

//function kCGImagePropertyIPTCLanguageIdentifier: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCLanguageIdentifier');
//end;

//function kCGImagePropertyIPTCStarRating: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCStarRating');
//end;

//function kCGImagePropertyIPTCCreatorContactInfo: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCCreatorContactInfo');
//end;

//function kCGImagePropertyIPTCRightsUsageTerms: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCRightsUsageTerms');
//end;

//function kCGImagePropertyIPTCScene: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCScene');
//end;

//function kCGImagePropertyIPTCContactInfoCity: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCContactInfoCity');
//end;

//function kCGImagePropertyIPTCContactInfoCountry: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCContactInfoCountry');
//end;

//function kCGImagePropertyIPTCContactInfoAddress: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCContactInfoAddress');
//end;

//function kCGImagePropertyIPTCContactInfoPostalCode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCContactInfoPostalCode');
//end;

//function kCGImagePropertyIPTCContactInfoStateProvince: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCContactInfoStateProvince');
//end;

//function kCGImagePropertyIPTCContactInfoEmails: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCContactInfoEmails');
//end;

//function kCGImagePropertyIPTCContactInfoPhones: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCContactInfoPhones');
//end;

//function kCGImagePropertyIPTCContactInfoWebURLs: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyIPTCContactInfoWebURLs');
//end;

//function kCGImageProperty8BIMLayerNames: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImageProperty8BIMLayerNames');
//end;

//function kCGImagePropertyDNGVersion: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyDNGVersion');
//end;

//function kCGImagePropertyDNGBackwardVersion: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyDNGBackwardVersion');
//end;

//function kCGImagePropertyDNGUniqueCameraModel: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyDNGUniqueCameraModel');
//end;

//function kCGImagePropertyDNGLocalizedCameraModel: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyDNGLocalizedCameraModel');
//end;

//function kCGImagePropertyDNGCameraSerialNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyDNGCameraSerialNumber');
//end;

//function kCGImagePropertyDNGLensInfo: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyDNGLensInfo');
//end;

//function kCGImagePropertyCIFFDescription: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFDescription');
//end;

//function kCGImagePropertyCIFFFirmware: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFFirmware');
//end;

//function kCGImagePropertyCIFFOwnerName: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFOwnerName');
//end;

//function kCGImagePropertyCIFFImageName: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFImageName');
//end;

//function kCGImagePropertyCIFFImageFileName: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFImageFileName');
//end;

//function kCGImagePropertyCIFFReleaseMethod: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFReleaseMethod');
//end;

//function kCGImagePropertyCIFFReleaseTiming: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFReleaseTiming');
//end;

//function kCGImagePropertyCIFFRecordID: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFRecordID');
//end;

//function kCGImagePropertyCIFFSelfTimingTime: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFSelfTimingTime');
//end;

//function kCGImagePropertyCIFFCameraSerialNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFCameraSerialNumber');
//end;

//function kCGImagePropertyCIFFImageSerialNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFImageSerialNumber');
//end;

//function kCGImagePropertyCIFFContinuousDrive: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFContinuousDrive');
//end;

//function kCGImagePropertyCIFFFocusMode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFFocusMode');
//end;

//function kCGImagePropertyCIFFMeteringMode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFMeteringMode');
//end;

//function kCGImagePropertyCIFFShootingMode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFShootingMode');
//end;

//function kCGImagePropertyCIFFLensModel: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFLensModel');
//end;

//function kCGImagePropertyCIFFLensMaxMM: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFLensMaxMM');
//end;

//function kCGImagePropertyCIFFLensMinMM: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFLensMinMM');
//end;

//function kCGImagePropertyCIFFWhiteBalanceIndex: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFWhiteBalanceIndex');
//end;

//function kCGImagePropertyCIFFFlashExposureComp: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFFlashExposureComp');
//end;

//function kCGImagePropertyCIFFMeasuredEV: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyCIFFMeasuredEV');
//end;

//function kCGImagePropertyMakerNikonISOSetting: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonISOSetting');
//end;

//function kCGImagePropertyMakerNikonColorMode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonColorMode');
//end;

//function kCGImagePropertyMakerNikonQuality: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonQuality');
//end;

//function kCGImagePropertyMakerNikonWhiteBalanceMode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonWhiteBalanceMode');
//end;

//function kCGImagePropertyMakerNikonSharpenMode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonSharpenMode');
//end;

//function kCGImagePropertyMakerNikonFocusMode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonFocusMode');
//end;

//function kCGImagePropertyMakerNikonFlashSetting: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonFlashSetting');
//end;

//function kCGImagePropertyMakerNikonISOSelection: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonISOSelection');
//end;

//function kCGImagePropertyMakerNikonFlashExposureComp: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonFlashExposureComp');
//end;

//function kCGImagePropertyMakerNikonImageAdjustment: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonImageAdjustment');
//end;

//function kCGImagePropertyMakerNikonLensAdapter: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonLensAdapter');
//end;

//function kCGImagePropertyMakerNikonLensType: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonLensType');
//end;

//function kCGImagePropertyMakerNikonLensInfo: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonLensInfo');
//end;

//function kCGImagePropertyMakerNikonFocusDistance: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonFocusDistance');
//end;

//function kCGImagePropertyMakerNikonDigitalZoom: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonDigitalZoom');
//end;

//function kCGImagePropertyMakerNikonShootingMode: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonShootingMode');
//end;

//function kCGImagePropertyMakerNikonCameraSerialNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonCameraSerialNumber');
//end;

//function kCGImagePropertyMakerNikonShutterCount: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerNikonShutterCount');
//end;

//function kCGImagePropertyMakerCanonOwnerName: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerCanonOwnerName');
//end;

//function kCGImagePropertyMakerCanonCameraSerialNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerCanonCameraSerialNumber');
//end;

//function kCGImagePropertyMakerCanonImageSerialNumber: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerCanonImageSerialNumber');
//end;

//function kCGImagePropertyMakerCanonFlashExposureComp: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerCanonFlashExposureComp');
//end;

//function kCGImagePropertyMakerCanonContinuousDrive: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerCanonContinuousDrive');
//end;

//function kCGImagePropertyMakerCanonLensModel: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerCanonLensModel');
//end;

//function kCGImagePropertyMakerCanonFirmware: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerCanonFirmware');
//end;

//function kCGImagePropertyMakerCanonAspectRatioInfo: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyMakerCanonAspectRatioInfo');
//end;

//function kCGImagePropertyOpenEXRAspectRatio: NSString;
//begin
//  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyOpenEXRAspectRatio');
//end;

end.
