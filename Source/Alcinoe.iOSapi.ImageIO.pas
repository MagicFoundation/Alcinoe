unit Alcinoe.iOSapi.ImageIO;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1936 has been resolved. If resolved, remove this unit.'}
  {$MESSAGE WARN 'Check if Macapi.ImageIO.pas was not updated and adjust the IFDEF.'}
{$ENDIF}

uses
  Posix.StdDef,
  iosapi.CocoaTypes,
  iosapi.Foundation,
  Macapi.CoreFoundation;

const
  kCGImageMetadataTypeInvalid = -1;
  {$EXTERNALSYM kCGImageMetadataTypeInvalid}
  kCGImageMetadataTypeDefault = 0;
  {$EXTERNALSYM kCGImageMetadataTypeDefault}
  kCGImageMetadataTypeString = 1;
  {$EXTERNALSYM kCGImageMetadataTypeString}
  kCGImageMetadataTypeArrayUnordered = 2;
  {$EXTERNALSYM kCGImageMetadataTypeArrayUnordered}
  kCGImageMetadataTypeArrayOrdered = 3;
  {$EXTERNALSYM kCGImageMetadataTypeArrayOrdered}
  kCGImageMetadataTypeAlternateArray = 4;
  {$EXTERNALSYM kCGImageMetadataTypeAlternateArray}
  kCGImageMetadataTypeAlternateText = 5;
  {$EXTERNALSYM kCGImageMetadataTypeAlternateText}
  kCGImageMetadataTypeStructure = 6;
  {$EXTERNALSYM kCGImageMetadataTypeStructure}
  kCGImageMetadataErrorUnknown = 0;
  {$EXTERNALSYM kCGImageMetadataErrorUnknown}
  kCGImageMetadataErrorUnsupportedFormat = 1;
  {$EXTERNALSYM kCGImageMetadataErrorUnsupportedFormat}
  kCGImageMetadataErrorBadArgument = 2;
  {$EXTERNALSYM kCGImageMetadataErrorBadArgument}
  kCGImageMetadataErrorConflictingArguments = 3;
  {$EXTERNALSYM kCGImageMetadataErrorConflictingArguments}
  kCGImageMetadataErrorPrefixConflict = 4;
  {$EXTERNALSYM kCGImageMetadataErrorPrefixConflict}
  kCGImageStatusUnexpectedEOF = -5;
  {$EXTERNALSYM kCGImageStatusUnexpectedEOF}
  kCGImageStatusInvalidData = -4;
  {$EXTERNALSYM kCGImageStatusInvalidData}
  kCGImageStatusUnknownType = -3;
  {$EXTERNALSYM kCGImageStatusUnknownType}
  kCGImageStatusReadingHeader = -2;
  {$EXTERNALSYM kCGImageStatusReadingHeader}
  kCGImageStatusIncomplete = -1;
  {$EXTERNALSYM kCGImageStatusIncomplete}
  kCGImageStatusComplete = 0;
  {$EXTERNALSYM kCGImageStatusComplete}

type
  // ===== Framework typedefs =====
  {$M+}
  CGImageDestinationRef = Pointer;
  {$EXTERNALSYM CGImageDestinationRef}
  CGImageSourceRef = Pointer;
  {$EXTERNALSYM CGImageSourceRef}
  CGImageMetadataRef = Pointer;
  {$EXTERNALSYM CGImageMetadataRef}
  CGMutableImageMetadataRef = Pointer;
  {$EXTERNALSYM CGMutableImageMetadataRef}
  CGImageMetadataTagRef = Pointer;
  {$EXTERNALSYM CGImageMetadataTagRef}
  CGImageMetadataType = Integer;
  {$EXTERNALSYM CGImageMetadataType}
  CGImageMetadataTagBlock = function(param1: CFStringRef; param2: CGImageMetadataTagRef): Integer; cdecl;
  {$EXTERNALSYM CGImageMetadataTagBlock}
  CGImageMetadataErrors = Cardinal;
  {$EXTERNALSYM CGImageMetadataErrors}
  CGImageSourceStatus = Integer;
  {$EXTERNALSYM CGImageSourceStatus}

type
  CGImagePropertyOrientation = UInt32;

  // ===== Exported string consts =====

function kCGImageMetadataNamespaceExif: CFStringRef;
function kCGImageMetadataNamespaceExifAux: CFStringRef;
function kCGImageMetadataNamespaceExifEX: CFStringRef;
function kCGImageMetadataNamespaceDublinCore: CFStringRef;
function kCGImageMetadataNamespaceIPTCCore: CFStringRef;
function kCGImageMetadataNamespacePhotoshop: CFStringRef;
function kCGImageMetadataNamespaceTIFF: CFStringRef;
function kCGImageMetadataNamespaceXMPBasic: CFStringRef;
function kCGImageMetadataNamespaceXMPRights: CFStringRef;
function kCGImageMetadataPrefixExif: CFStringRef;
function kCGImageMetadataPrefixExifAux: CFStringRef;
function kCGImageMetadataPrefixExifEX: CFStringRef;
function kCGImageMetadataPrefixDublinCore: CFStringRef;
function kCGImageMetadataPrefixIPTCCore: CFStringRef;
function kCGImageMetadataPrefixPhotoshop: CFStringRef;
function kCGImageMetadataPrefixTIFF: CFStringRef;
function kCGImageMetadataPrefixXMPBasic: CFStringRef;
function kCGImageMetadataPrefixXMPRights: CFStringRef;
function kCGImageMetadataEnumerateRecursively: CFStringRef;
function kCFErrorDomainCGImageMetadata: CFStringRef;
function kCGImageSourceTypeIdentifierHint: CFStringRef;
function kCGImageSourceShouldCache: CFStringRef;
function kCGImageSourceShouldCacheImmediately: CFStringRef;
function kCGImageSourceShouldAllowFloat: CFStringRef;
function kCGImageSourceCreateThumbnailFromImageIfAbsent: CFStringRef;
function kCGImageSourceCreateThumbnailFromImageAlways: CFStringRef;
function kCGImageSourceThumbnailMaxPixelSize: CFStringRef;
function kCGImageSourceCreateThumbnailWithTransform: CFStringRef;
function kCGImageDestinationLossyCompressionQuality: CFStringRef;
function kCGImageDestinationBackgroundColor: CFStringRef;
function kCGImageDestinationMetadata: CFStringRef;
function kCGImageDestinationMergeMetadata: CFStringRef;
function kCGImageMetadataShouldExcludeXMP: CFStringRef;
function kCGImageDestinationDateTime: CFStringRef;
function kCGImageDestinationOrientation: CFStringRef;
function kCGImagePropertyTIFFDictionary: CFStringRef;
function kCGImagePropertyGIFDictionary: CFStringRef;
function kCGImagePropertyJFIFDictionary: CFStringRef;
function kCGImagePropertyExifDictionary: CFStringRef;
function kCGImagePropertyPNGDictionary: CFStringRef;
function kCGImagePropertyIPTCDictionary: CFStringRef;
function kCGImagePropertyGPSDictionary: CFStringRef;
function kCGImagePropertyRawDictionary: CFStringRef;
function kCGImagePropertyCIFFDictionary: CFStringRef;
function kCGImagePropertyMakerCanonDictionary: CFStringRef;
function kCGImagePropertyMakerNikonDictionary: CFStringRef;
function kCGImagePropertyMakerMinoltaDictionary: CFStringRef;
function kCGImagePropertyMakerFujiDictionary: CFStringRef;
function kCGImagePropertyMakerOlympusDictionary: CFStringRef;
function kCGImagePropertyMakerPentaxDictionary: CFStringRef;
function kCGImageProperty8BIMDictionary: CFStringRef;
function kCGImagePropertyDNGDictionary: CFStringRef;
function kCGImagePropertyExifAuxDictionary: CFStringRef;
function kCGImagePropertyOpenEXRDictionary: CFStringRef;
function kCGImagePropertyFileSize: CFStringRef;
function kCGImagePropertyPixelHeight: CFStringRef;
function kCGImagePropertyPixelWidth: CFStringRef;
function kCGImagePropertyDPIHeight: CFStringRef;
function kCGImagePropertyDPIWidth: CFStringRef;
function kCGImagePropertyDepth: CFStringRef;
function kCGImagePropertyOrientation: CFStringRef;
function kCGImagePropertyIsFloat: CFStringRef;
function kCGImagePropertyIsIndexed: CFStringRef;
function kCGImagePropertyHasAlpha: CFStringRef;
function kCGImagePropertyColorModel: CFStringRef;
function kCGImagePropertyProfileName: CFStringRef;
function kCGImagePropertyColorModelRGB: CFStringRef;
function kCGImagePropertyColorModelGray: CFStringRef;
function kCGImagePropertyColorModelCMYK: CFStringRef;
function kCGImagePropertyColorModelLab: CFStringRef;
function kCGImagePropertyTIFFCompression: CFStringRef;
function kCGImagePropertyTIFFPhotometricInterpretation: CFStringRef;
function kCGImagePropertyTIFFDocumentName: CFStringRef;
function kCGImagePropertyTIFFImageDescription: CFStringRef;
function kCGImagePropertyTIFFMake: CFStringRef;
function kCGImagePropertyTIFFModel: CFStringRef;
function kCGImagePropertyTIFFOrientation: CFStringRef;
function kCGImagePropertyTIFFXResolution: CFStringRef;
function kCGImagePropertyTIFFYResolution: CFStringRef;
function kCGImagePropertyTIFFResolutionUnit: CFStringRef;
function kCGImagePropertyTIFFSoftware: CFStringRef;
function kCGImagePropertyTIFFTransferFunction: CFStringRef;
function kCGImagePropertyTIFFDateTime: CFStringRef;
function kCGImagePropertyTIFFArtist: CFStringRef;
function kCGImagePropertyTIFFHostComputer: CFStringRef;
function kCGImagePropertyTIFFCopyright: CFStringRef;
function kCGImagePropertyTIFFWhitePoint: CFStringRef;
function kCGImagePropertyTIFFPrimaryChromaticities: CFStringRef;
function kCGImagePropertyJFIFVersion: CFStringRef;
function kCGImagePropertyJFIFXDensity: CFStringRef;
function kCGImagePropertyJFIFYDensity: CFStringRef;
function kCGImagePropertyJFIFDensityUnit: CFStringRef;
function kCGImagePropertyJFIFIsProgressive: CFStringRef;
function kCGImagePropertyExifExposureTime: CFStringRef;
function kCGImagePropertyExifFNumber: CFStringRef;
function kCGImagePropertyExifExposureProgram: CFStringRef;
function kCGImagePropertyExifSpectralSensitivity: CFStringRef;
function kCGImagePropertyExifISOSpeedRatings: CFStringRef;
function kCGImagePropertyExifOECF: CFStringRef;
function kCGImagePropertyExifSensitivityType: CFStringRef;
function kCGImagePropertyExifStandardOutputSensitivity: CFStringRef;
function kCGImagePropertyExifRecommendedExposureIndex: CFStringRef;
function kCGImagePropertyExifISOSpeed: CFStringRef;
function kCGImagePropertyExifISOSpeedLatitudeyyy: CFStringRef;
function kCGImagePropertyExifISOSpeedLatitudezzz: CFStringRef;
function kCGImagePropertyExifVersion: CFStringRef;
function kCGImagePropertyExifDateTimeOriginal: CFStringRef;
function kCGImagePropertyExifDateTimeDigitized: CFStringRef;
function kCGImagePropertyExifComponentsConfiguration: CFStringRef;
function kCGImagePropertyExifCompressedBitsPerPixel: CFStringRef;
function kCGImagePropertyExifShutterSpeedValue: CFStringRef;
function kCGImagePropertyExifApertureValue: CFStringRef;
function kCGImagePropertyExifBrightnessValue: CFStringRef;
function kCGImagePropertyExifExposureBiasValue: CFStringRef;
function kCGImagePropertyExifMaxApertureValue: CFStringRef;
function kCGImagePropertyExifSubjectDistance: CFStringRef;
function kCGImagePropertyExifMeteringMode: CFStringRef;
function kCGImagePropertyExifLightSource: CFStringRef;
function kCGImagePropertyExifFlash: CFStringRef;
function kCGImagePropertyExifFocalLength: CFStringRef;
function kCGImagePropertyExifSubjectArea: CFStringRef;
function kCGImagePropertyExifMakerNote: CFStringRef;
function kCGImagePropertyExifUserComment: CFStringRef;
function kCGImagePropertyExifSubsecTime: CFStringRef;
function kCGImagePropertyExifSubsecTimeOrginal: CFStringRef;
function kCGImagePropertyExifSubsecTimeDigitized: CFStringRef;
function kCGImagePropertyExifFlashPixVersion: CFStringRef;
function kCGImagePropertyExifColorSpace: CFStringRef;
function kCGImagePropertyExifPixelXDimension: CFStringRef;
function kCGImagePropertyExifPixelYDimension: CFStringRef;
function kCGImagePropertyExifRelatedSoundFile: CFStringRef;
function kCGImagePropertyExifFlashEnergy: CFStringRef;
function kCGImagePropertyExifSpatialFrequencyResponse: CFStringRef;
function kCGImagePropertyExifFocalPlaneXResolution: CFStringRef;
function kCGImagePropertyExifFocalPlaneYResolution: CFStringRef;
function kCGImagePropertyExifFocalPlaneResolutionUnit: CFStringRef;
function kCGImagePropertyExifSubjectLocation: CFStringRef;
function kCGImagePropertyExifExposureIndex: CFStringRef;
function kCGImagePropertyExifSensingMethod: CFStringRef;
function kCGImagePropertyExifFileSource: CFStringRef;
function kCGImagePropertyExifSceneType: CFStringRef;
function kCGImagePropertyExifCFAPattern: CFStringRef;
function kCGImagePropertyExifCustomRendered: CFStringRef;
function kCGImagePropertyExifExposureMode: CFStringRef;
function kCGImagePropertyExifWhiteBalance: CFStringRef;
function kCGImagePropertyExifDigitalZoomRatio: CFStringRef;
function kCGImagePropertyExifFocalLenIn35mmFilm: CFStringRef;
function kCGImagePropertyExifSceneCaptureType: CFStringRef;
function kCGImagePropertyExifGainControl: CFStringRef;
function kCGImagePropertyExifContrast: CFStringRef;
function kCGImagePropertyExifSaturation: CFStringRef;
function kCGImagePropertyExifSharpness: CFStringRef;
function kCGImagePropertyExifDeviceSettingDescription: CFStringRef;
function kCGImagePropertyExifSubjectDistRange: CFStringRef;
function kCGImagePropertyExifImageUniqueID: CFStringRef;
function kCGImagePropertyExifCameraOwnerName: CFStringRef;
function kCGImagePropertyExifBodySerialNumber: CFStringRef;
function kCGImagePropertyExifLensSpecification: CFStringRef;
function kCGImagePropertyExifLensMake: CFStringRef;
function kCGImagePropertyExifLensModel: CFStringRef;
function kCGImagePropertyExifLensSerialNumber: CFStringRef;
function kCGImagePropertyExifGamma: CFStringRef;
function kCGImagePropertyExifAuxLensInfo: CFStringRef;
function kCGImagePropertyExifAuxLensModel: CFStringRef;
function kCGImagePropertyExifAuxSerialNumber: CFStringRef;
function kCGImagePropertyExifAuxLensID: CFStringRef;
function kCGImagePropertyExifAuxLensSerialNumber: CFStringRef;
function kCGImagePropertyExifAuxImageNumber: CFStringRef;
function kCGImagePropertyExifAuxFlashCompensation: CFStringRef;
function kCGImagePropertyExifAuxOwnerName: CFStringRef;
function kCGImagePropertyExifAuxFirmware: CFStringRef;
function kCGImagePropertyGIFLoopCount: CFStringRef;
function kCGImagePropertyGIFDelayTime: CFStringRef;
function kCGImagePropertyGIFImageColorMap: CFStringRef;
function kCGImagePropertyGIFHasGlobalColorMap: CFStringRef;
function kCGImagePropertyGIFUnclampedDelayTime: CFStringRef;
function kCGImagePropertyPNGGamma: CFStringRef;
function kCGImagePropertyPNGInterlaceType: CFStringRef;
function kCGImagePropertyPNGXPixelsPerMeter: CFStringRef;
function kCGImagePropertyPNGYPixelsPerMeter: CFStringRef;
function kCGImagePropertyPNGsRGBIntent: CFStringRef;
function kCGImagePropertyPNGChromaticities: CFStringRef;
function kCGImagePropertyPNGAuthor: CFStringRef;
function kCGImagePropertyPNGCopyright: CFStringRef;
function kCGImagePropertyPNGCreationTime: CFStringRef;
function kCGImagePropertyPNGDescription: CFStringRef;
function kCGImagePropertyPNGModificationTime: CFStringRef;
function kCGImagePropertyPNGSoftware: CFStringRef;
function kCGImagePropertyPNGTitle: CFStringRef;
function kCGImagePropertyGPSVersion: CFStringRef;
function kCGImagePropertyGPSLatitudeRef: CFStringRef;
function kCGImagePropertyGPSLatitude: CFStringRef;
function kCGImagePropertyGPSLongitudeRef: CFStringRef;
function kCGImagePropertyGPSLongitude: CFStringRef;
function kCGImagePropertyGPSAltitudeRef: CFStringRef;
function kCGImagePropertyGPSAltitude: CFStringRef;
function kCGImagePropertyGPSTimeStamp: CFStringRef;
function kCGImagePropertyGPSSatellites: CFStringRef;
function kCGImagePropertyGPSStatus: CFStringRef;
function kCGImagePropertyGPSMeasureMode: CFStringRef;
function kCGImagePropertyGPSDOP: CFStringRef;
function kCGImagePropertyGPSSpeedRef: CFStringRef;
function kCGImagePropertyGPSSpeed: CFStringRef;
function kCGImagePropertyGPSTrackRef: CFStringRef;
function kCGImagePropertyGPSTrack: CFStringRef;
function kCGImagePropertyGPSImgDirectionRef: CFStringRef;
function kCGImagePropertyGPSImgDirection: CFStringRef;
function kCGImagePropertyGPSMapDatum: CFStringRef;
function kCGImagePropertyGPSDestLatitudeRef: CFStringRef;
function kCGImagePropertyGPSDestLatitude: CFStringRef;
function kCGImagePropertyGPSDestLongitudeRef: CFStringRef;
function kCGImagePropertyGPSDestLongitude: CFStringRef;
function kCGImagePropertyGPSDestBearingRef: CFStringRef;
function kCGImagePropertyGPSDestBearing: CFStringRef;
function kCGImagePropertyGPSDestDistanceRef: CFStringRef;
function kCGImagePropertyGPSDestDistance: CFStringRef;
function kCGImagePropertyGPSProcessingMethod: CFStringRef;
function kCGImagePropertyGPSAreaInformation: CFStringRef;
function kCGImagePropertyGPSDateStamp: CFStringRef;
function kCGImagePropertyGPSDifferental: CFStringRef;
function kCGImagePropertyIPTCObjectTypeReference: CFStringRef;
function kCGImagePropertyIPTCObjectAttributeReference: CFStringRef;
function kCGImagePropertyIPTCObjectName: CFStringRef;
function kCGImagePropertyIPTCEditStatus: CFStringRef;
function kCGImagePropertyIPTCEditorialUpdate: CFStringRef;
function kCGImagePropertyIPTCUrgency: CFStringRef;
function kCGImagePropertyIPTCSubjectReference: CFStringRef;
function kCGImagePropertyIPTCCategory: CFStringRef;
function kCGImagePropertyIPTCSupplementalCategory: CFStringRef;
function kCGImagePropertyIPTCFixtureIdentifier: CFStringRef;
function kCGImagePropertyIPTCKeywords: CFStringRef;
function kCGImagePropertyIPTCContentLocationCode: CFStringRef;
function kCGImagePropertyIPTCContentLocationName: CFStringRef;
function kCGImagePropertyIPTCReleaseDate: CFStringRef;
function kCGImagePropertyIPTCReleaseTime: CFStringRef;
function kCGImagePropertyIPTCExpirationDate: CFStringRef;
function kCGImagePropertyIPTCExpirationTime: CFStringRef;
function kCGImagePropertyIPTCSpecialInstructions: CFStringRef;
function kCGImagePropertyIPTCActionAdvised: CFStringRef;
function kCGImagePropertyIPTCReferenceService: CFStringRef;
function kCGImagePropertyIPTCReferenceDate: CFStringRef;
function kCGImagePropertyIPTCReferenceNumber: CFStringRef;
function kCGImagePropertyIPTCDateCreated: CFStringRef;
function kCGImagePropertyIPTCTimeCreated: CFStringRef;
function kCGImagePropertyIPTCDigitalCreationDate: CFStringRef;
function kCGImagePropertyIPTCDigitalCreationTime: CFStringRef;
function kCGImagePropertyIPTCOriginatingProgram: CFStringRef;
function kCGImagePropertyIPTCProgramVersion: CFStringRef;
function kCGImagePropertyIPTCObjectCycle: CFStringRef;
function kCGImagePropertyIPTCByline: CFStringRef;
function kCGImagePropertyIPTCBylineTitle: CFStringRef;
function kCGImagePropertyIPTCCity: CFStringRef;
function kCGImagePropertyIPTCSubLocation: CFStringRef;
function kCGImagePropertyIPTCProvinceState: CFStringRef;
function kCGImagePropertyIPTCCountryPrimaryLocationCode: CFStringRef;
function kCGImagePropertyIPTCCountryPrimaryLocationName: CFStringRef;
function kCGImagePropertyIPTCOriginalTransmissionReference: CFStringRef;
function kCGImagePropertyIPTCHeadline: CFStringRef;
function kCGImagePropertyIPTCCredit: CFStringRef;
function kCGImagePropertyIPTCSource: CFStringRef;
function kCGImagePropertyIPTCCopyrightNotice: CFStringRef;
function kCGImagePropertyIPTCContact: CFStringRef;
function kCGImagePropertyIPTCCaptionAbstract: CFStringRef;
function kCGImagePropertyIPTCWriterEditor: CFStringRef;
function kCGImagePropertyIPTCImageType: CFStringRef;
function kCGImagePropertyIPTCImageOrientation: CFStringRef;
function kCGImagePropertyIPTCLanguageIdentifier: CFStringRef;
function kCGImagePropertyIPTCStarRating: CFStringRef;
function kCGImagePropertyIPTCCreatorContactInfo: CFStringRef;
function kCGImagePropertyIPTCRightsUsageTerms: CFStringRef;
function kCGImagePropertyIPTCScene: CFStringRef;
function kCGImagePropertyIPTCContactInfoCity: CFStringRef;
function kCGImagePropertyIPTCContactInfoCountry: CFStringRef;
function kCGImagePropertyIPTCContactInfoAddress: CFStringRef;
function kCGImagePropertyIPTCContactInfoPostalCode: CFStringRef;
function kCGImagePropertyIPTCContactInfoStateProvince: CFStringRef;
function kCGImagePropertyIPTCContactInfoEmails: CFStringRef;
function kCGImagePropertyIPTCContactInfoPhones: CFStringRef;
function kCGImagePropertyIPTCContactInfoWebURLs: CFStringRef;
function kCGImageProperty8BIMLayerNames: CFStringRef;
function kCGImagePropertyDNGVersion: CFStringRef;
function kCGImagePropertyDNGBackwardVersion: CFStringRef;
function kCGImagePropertyDNGUniqueCameraModel: CFStringRef;
function kCGImagePropertyDNGLocalizedCameraModel: CFStringRef;
function kCGImagePropertyDNGCameraSerialNumber: CFStringRef;
function kCGImagePropertyDNGLensInfo: CFStringRef;
function kCGImagePropertyCIFFDescription: CFStringRef;
function kCGImagePropertyCIFFFirmware: CFStringRef;
function kCGImagePropertyCIFFOwnerName: CFStringRef;
function kCGImagePropertyCIFFImageName: CFStringRef;
function kCGImagePropertyCIFFImageFileName: CFStringRef;
function kCGImagePropertyCIFFReleaseMethod: CFStringRef;
function kCGImagePropertyCIFFReleaseTiming: CFStringRef;
function kCGImagePropertyCIFFRecordID: CFStringRef;
function kCGImagePropertyCIFFSelfTimingTime: CFStringRef;
function kCGImagePropertyCIFFCameraSerialNumber: CFStringRef;
function kCGImagePropertyCIFFImageSerialNumber: CFStringRef;
function kCGImagePropertyCIFFContinuousDrive: CFStringRef;
function kCGImagePropertyCIFFFocusMode: CFStringRef;
function kCGImagePropertyCIFFMeteringMode: CFStringRef;
function kCGImagePropertyCIFFShootingMode: CFStringRef;
function kCGImagePropertyCIFFLensModel: CFStringRef;
function kCGImagePropertyCIFFLensMaxMM: CFStringRef;
function kCGImagePropertyCIFFLensMinMM: CFStringRef;
function kCGImagePropertyCIFFWhiteBalanceIndex: CFStringRef;
function kCGImagePropertyCIFFFlashExposureComp: CFStringRef;
function kCGImagePropertyCIFFMeasuredEV: CFStringRef;
function kCGImagePropertyMakerNikonISOSetting: CFStringRef;
function kCGImagePropertyMakerNikonColorMode: CFStringRef;
function kCGImagePropertyMakerNikonQuality: CFStringRef;
function kCGImagePropertyMakerNikonWhiteBalanceMode: CFStringRef;
function kCGImagePropertyMakerNikonSharpenMode: CFStringRef;
function kCGImagePropertyMakerNikonFocusMode: CFStringRef;
function kCGImagePropertyMakerNikonFlashSetting: CFStringRef;
function kCGImagePropertyMakerNikonISOSelection: CFStringRef;
function kCGImagePropertyMakerNikonFlashExposureComp: CFStringRef;
function kCGImagePropertyMakerNikonImageAdjustment: CFStringRef;
function kCGImagePropertyMakerNikonLensAdapter: CFStringRef;
function kCGImagePropertyMakerNikonLensType: CFStringRef;
function kCGImagePropertyMakerNikonLensInfo: CFStringRef;
function kCGImagePropertyMakerNikonFocusDistance: CFStringRef;
function kCGImagePropertyMakerNikonDigitalZoom: CFStringRef;
function kCGImagePropertyMakerNikonShootingMode: CFStringRef;
function kCGImagePropertyMakerNikonCameraSerialNumber: CFStringRef;
function kCGImagePropertyMakerNikonShutterCount: CFStringRef;
function kCGImagePropertyMakerCanonOwnerName: CFStringRef;
function kCGImagePropertyMakerCanonCameraSerialNumber: CFStringRef;
function kCGImagePropertyMakerCanonImageSerialNumber: CFStringRef;
function kCGImagePropertyMakerCanonFlashExposureComp: CFStringRef;
function kCGImagePropertyMakerCanonContinuousDrive: CFStringRef;
function kCGImagePropertyMakerCanonLensModel: CFStringRef;
function kCGImagePropertyMakerCanonFirmware: CFStringRef;
function kCGImagePropertyMakerCanonAspectRatioInfo: CFStringRef;
function kCGImagePropertyOpenEXRAspectRatio: CFStringRef;

// ===== External functions =====

const
  // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1936
  libImageIO = '/System/Library/Frameworks/ImageIO.framework/ImageIO';

function CGImageMetadataGetTypeID: CFTypeID; cdecl; external libImageIO name _PU + 'CGImageMetadataGetTypeID';
{$EXTERNALSYM CGImageMetadataGetTypeID}
function CGImageMetadataCreateMutable: CGMutableImageMetadataRef; cdecl;
  external libImageIO name _PU + 'CGImageMetadataCreateMutable';
{$EXTERNALSYM CGImageMetadataCreateMutable}
function CGImageMetadataCreateMutableCopy(metadata: CGImageMetadataRef): CGMutableImageMetadataRef; cdecl;
  external libImageIO name _PU + 'CGImageMetadataCreateMutableCopy';
{$EXTERNALSYM CGImageMetadataCreateMutableCopy}
function CGImageMetadataTagGetTypeID: CFTypeID; cdecl; external libImageIO name _PU + 'CGImageMetadataTagGetTypeID';
{$EXTERNALSYM CGImageMetadataTagGetTypeID}
function CGImageMetadataTagCreate(xmlns: CFStringRef; prefix: CFStringRef; name: CFStringRef;
  &type: CGImageMetadataType; value: CFTypeRef): CGImageMetadataTagRef; cdecl;
  external libImageIO name _PU + 'CGImageMetadataTagCreate';
{$EXTERNALSYM CGImageMetadataTagCreate}
function CGImageMetadataTagCopyNamespace(tag: CGImageMetadataTagRef): CFStringRef; cdecl;
  external libImageIO name _PU + 'CGImageMetadataTagCopyNamespace';
{$EXTERNALSYM CGImageMetadataTagCopyNamespace}
function CGImageMetadataTagCopyPrefix(tag: CGImageMetadataTagRef): CFStringRef; cdecl;
  external libImageIO name _PU + 'CGImageMetadataTagCopyPrefix';
{$EXTERNALSYM CGImageMetadataTagCopyPrefix}
function CGImageMetadataTagCopyName(tag: CGImageMetadataTagRef): CFStringRef; cdecl;
  external libImageIO name _PU + 'CGImageMetadataTagCopyName';
{$EXTERNALSYM CGImageMetadataTagCopyName}
function CGImageMetadataTagCopyValue(tag: CGImageMetadataTagRef): CFTypeRef; cdecl;
  external libImageIO name _PU + 'CGImageMetadataTagCopyValue';
{$EXTERNALSYM CGImageMetadataTagCopyValue}
function CGImageMetadataTagGetType(tag: CGImageMetadataTagRef): CGImageMetadataType; cdecl;
  external libImageIO name _PU + 'CGImageMetadataTagGetType';
{$EXTERNALSYM CGImageMetadataTagGetType}
function CGImageMetadataTagCopyQualifiers(tag: CGImageMetadataTagRef): CFArrayRef; cdecl;
  external libImageIO name _PU + 'CGImageMetadataTagCopyQualifiers';
{$EXTERNALSYM CGImageMetadataTagCopyQualifiers}
function CGImageMetadataCopyTags(metadata: CGImageMetadataRef): CFArrayRef; cdecl;
  external libImageIO name _PU + 'CGImageMetadataCopyTags';
{$EXTERNALSYM CGImageMetadataCopyTags}
function CGImageMetadataCopyTagWithPath(metadata: CGImageMetadataRef; parent: CGImageMetadataTagRef; path: CFStringRef)
  : CGImageMetadataTagRef; cdecl; external libImageIO name _PU + 'CGImageMetadataCopyTagWithPath';
{$EXTERNALSYM CGImageMetadataCopyTagWithPath}
function CGImageMetadataCopyStringValueWithPath(metadata: CGImageMetadataRef; parent: CGImageMetadataTagRef;
  path: CFStringRef): CFStringRef; cdecl; external libImageIO name _PU + 'CGImageMetadataCopyStringValueWithPath';
{$EXTERNALSYM CGImageMetadataCopyStringValueWithPath}
function CGImageMetadataRegisterNamespaceForPrefix(metadata: CGMutableImageMetadataRef; xmlns: CFStringRef;
  prefix: CFStringRef; err: CFErrorRef): Integer; cdecl;
  external libImageIO name _PU + 'CGImageMetadataRegisterNamespaceForPrefix';
{$EXTERNALSYM CGImageMetadataRegisterNamespaceForPrefix}
function CGImageMetadataSetTagWithPath(metadata: CGMutableImageMetadataRef; parent: CGImageMetadataTagRef;
  path: CFStringRef; tag: CGImageMetadataTagRef): Integer; cdecl;
  external libImageIO name _PU + 'CGImageMetadataSetTagWithPath';
{$EXTERNALSYM CGImageMetadataSetTagWithPath}
function CGImageMetadataSetValueWithPath(metadata: CGMutableImageMetadataRef; parent: CGImageMetadataTagRef;
  path: CFStringRef; value: CFTypeRef): Integer; cdecl;
  external libImageIO name _PU + 'CGImageMetadataSetValueWithPath';
{$EXTERNALSYM CGImageMetadataSetValueWithPath}
function CGImageMetadataRemoveTagWithPath(metadata: CGMutableImageMetadataRef; parent: CGImageMetadataTagRef;
  path: CFStringRef): Integer; cdecl; external libImageIO name _PU + 'CGImageMetadataRemoveTagWithPath';
{$EXTERNALSYM CGImageMetadataRemoveTagWithPath}
procedure CGImageMetadataEnumerateTagsUsingBlock(metadata: CGImageMetadataRef; rootPath: CFStringRef;
  options: CFDictionaryRef; block: CGImageMetadataTagBlock); cdecl;
  external libImageIO name _PU + 'CGImageMetadataEnumerateTagsUsingBlock';
{$EXTERNALSYM CGImageMetadataEnumerateTagsUsingBlock}
function CGImageMetadataCopyTagMatchingImageProperty(metadata: CGImageMetadataRef; dictionaryName: CFStringRef;
  propertyName: CFStringRef): CGImageMetadataTagRef; cdecl;
  external libImageIO name _PU + 'CGImageMetadataCopyTagMatchingImageProperty';
{$EXTERNALSYM CGImageMetadataCopyTagMatchingImageProperty}
function CGImageMetadataSetValueMatchingImageProperty(metadata: CGMutableImageMetadataRef; dictionaryName: CFStringRef;
  propertyName: CFStringRef; value: CFTypeRef): Integer; cdecl;
  external libImageIO name _PU + 'CGImageMetadataSetValueMatchingImageProperty';
{$EXTERNALSYM CGImageMetadataSetValueMatchingImageProperty}
function CGImageMetadataCreateXMPData(metadata: CGImageMetadataRef; options: CFDictionaryRef): CFDataRef; cdecl;
  external libImageIO name _PU + 'CGImageMetadataCreateXMPData';
{$EXTERNALSYM CGImageMetadataCreateXMPData}
function CGImageMetadataCreateFromXMPData(data: CFDataRef): CGImageMetadataRef; cdecl;
  external libImageIO name _PU + 'CGImageMetadataCreateFromXMPData';
{$EXTERNALSYM CGImageMetadataCreateFromXMPData}
function CGImageSourceGetTypeID: CFTypeID; cdecl; external libImageIO name _PU + 'CGImageSourceGetTypeID';
{$EXTERNALSYM CGImageSourceGetTypeID}
function CGImageSourceCopyTypeIdentifiers: CFArrayRef; cdecl;
  external libImageIO name _PU + 'CGImageSourceCopyTypeIdentifiers';
{$EXTERNALSYM CGImageSourceCopyTypeIdentifiers}
//function CGImageSourceCreateWithDataProvider(provider: CGDataProviderRef; options: CFDictionaryRef): CGImageSourceRef;
//  cdecl; external libImageIO name _PU + 'CGImageSourceCreateWithDataProvider';
//{$EXTERNALSYM CGImageSourceCreateWithDataProvider}
function CGImageSourceCreateWithData(data: CFDataRef; options: CFDictionaryRef): CGImageSourceRef; cdecl;
  external libImageIO name _PU + 'CGImageSourceCreateWithData';
{$EXTERNALSYM CGImageSourceCreateWithData}
function CGImageSourceCreateWithURL(url: CFURLRef; options: CFDictionaryRef): CGImageSourceRef; cdecl;
  external libImageIO name _PU + 'CGImageSourceCreateWithURL';
{$EXTERNALSYM CGImageSourceCreateWithURL}
function CGImageSourceGetType(isrc: CGImageSourceRef): CFStringRef; cdecl;
  external libImageIO name _PU + 'CGImageSourceGetType';
{$EXTERNALSYM CGImageSourceGetType}
function CGImageSourceGetCount(isrc: CGImageSourceRef): size_t; cdecl;
  external libImageIO name _PU + 'CGImageSourceGetCount';
{$EXTERNALSYM CGImageSourceGetCount}
function CGImageSourceCopyProperties(isrc: CGImageSourceRef; options: CFDictionaryRef): CFDictionaryRef; cdecl;
  external libImageIO name _PU + 'CGImageSourceCopyProperties';
{$EXTERNALSYM CGImageSourceCopyProperties}
function CGImageSourceCopyPropertiesAtIndex(isrc: CGImageSourceRef; index: size_t; options: CFDictionaryRef)
  : CFDictionaryRef; cdecl; external libImageIO name _PU + 'CGImageSourceCopyPropertiesAtIndex';
{$EXTERNALSYM CGImageSourceCopyPropertiesAtIndex}
function CGImageSourceCopyMetadataAtIndex(isrc: CGImageSourceRef; index: size_t; options: CFDictionaryRef)
  : CGImageMetadataRef; cdecl; external libImageIO name _PU + 'CGImageSourceCopyMetadataAtIndex';
{$EXTERNALSYM CGImageSourceCopyMetadataAtIndex}
//function CGImageSourceCreateImageAtIndex(isrc: CGImageSourceRef; index: size_t; options: CFDictionaryRef): CGImageRef;
//  cdecl; external libImageIO name _PU + 'CGImageSourceCreateImageAtIndex';
//{$EXTERNALSYM CGImageSourceCreateImageAtIndex}
procedure CGImageSourceRemoveCacheAtIndex(isrc: CGImageSourceRef; index: size_t); cdecl;
  external libImageIO name _PU + 'CGImageSourceRemoveCacheAtIndex';
{$EXTERNALSYM CGImageSourceRemoveCacheAtIndex}
//function CGImageSourceCreateThumbnailAtIndex(isrc: CGImageSourceRef; index: size_t; options: CFDictionaryRef)
//  : CGImageRef; cdecl; external libImageIO name _PU + 'CGImageSourceCreateThumbnailAtIndex';
//{$EXTERNALSYM CGImageSourceCreateThumbnailAtIndex}
function CGImageSourceCreateIncremental(options: CFDictionaryRef): CGImageSourceRef; cdecl;
  external libImageIO name _PU + 'CGImageSourceCreateIncremental';
{$EXTERNALSYM CGImageSourceCreateIncremental}
procedure CGImageSourceUpdateData(isrc: CGImageSourceRef; data: CFDataRef; final: Integer); cdecl;
  external libImageIO name _PU + 'CGImageSourceUpdateData';
{$EXTERNALSYM CGImageSourceUpdateData}
//procedure CGImageSourceUpdateDataProvider(isrc: CGImageSourceRef; provider: CGDataProviderRef; final: Integer); cdecl;
//  external libImageIO name _PU + 'CGImageSourceUpdateDataProvider';
//{$EXTERNALSYM CGImageSourceUpdateDataProvider}
function CGImageSourceGetStatus(isrc: CGImageSourceRef): CGImageSourceStatus; cdecl;
  external libImageIO name _PU + 'CGImageSourceGetStatus';
{$EXTERNALSYM CGImageSourceGetStatus}
function CGImageSourceGetStatusAtIndex(isrc: CGImageSourceRef; index: size_t): CGImageSourceStatus; cdecl;
  external libImageIO name _PU + 'CGImageSourceGetStatusAtIndex';
{$EXTERNALSYM CGImageSourceGetStatusAtIndex}
function CGImageDestinationGetTypeID: CFTypeID; cdecl; external libImageIO name _PU + 'CGImageDestinationGetTypeID';
{$EXTERNALSYM CGImageDestinationGetTypeID}
function CGImageDestinationCopyTypeIdentifiers: CFArrayRef; cdecl;
  external libImageIO name _PU + 'CGImageDestinationCopyTypeIdentifiers';
{$EXTERNALSYM CGImageDestinationCopyTypeIdentifiers}
//function CGImageDestinationCreateWithDataConsumer(consumer: CGDataConsumerRef; &type: CFStringRef; count: size_t;
//  options: CFDictionaryRef): CGImageDestinationRef; cdecl;
//  external libImageIO name _PU + 'CGImageDestinationCreateWithDataConsumer';
//{$EXTERNALSYM CGImageDestinationCreateWithDataConsumer}
function CGImageDestinationCreateWithData(data: CFMutableDataRef; &type: CFStringRef; count: size_t;
  options: CFDictionaryRef): CGImageDestinationRef; cdecl;
  external libImageIO name _PU + 'CGImageDestinationCreateWithData';
{$EXTERNALSYM CGImageDestinationCreateWithData}
function CGImageDestinationCreateWithURL(url: CFURLRef; &type: CFStringRef; count: size_t; options: CFDictionaryRef)
  : CGImageDestinationRef; cdecl; external libImageIO name _PU + 'CGImageDestinationCreateWithURL';
{$EXTERNALSYM CGImageDestinationCreateWithURL}
procedure CGImageDestinationSetProperties(idst: CGImageDestinationRef; properties: CFDictionaryRef); cdecl;
  external libImageIO name _PU + 'CGImageDestinationSetProperties';
{$EXTERNALSYM CGImageDestinationSetProperties}
//procedure CGImageDestinationAddImage(idst: CGImageDestinationRef; image: CGImageRef; properties: CFDictionaryRef);
//  cdecl; external libImageIO name _PU + 'CGImageDestinationAddImage';
//{$EXTERNALSYM CGImageDestinationAddImage}
procedure CGImageDestinationAddImageFromSource(idst: CGImageDestinationRef; isrc: CGImageSourceRef; index: size_t;
  properties: CFDictionaryRef); cdecl; external libImageIO name _PU + 'CGImageDestinationAddImageFromSource';
{$EXTERNALSYM CGImageDestinationAddImageFromSource}
function CGImageDestinationFinalize(idst: CGImageDestinationRef): Integer; cdecl;
  external libImageIO name _PU + 'CGImageDestinationFinalize';
{$EXTERNALSYM CGImageDestinationFinalize}
//procedure CGImageDestinationAddImageAndMetadata(idst: CGImageDestinationRef; image: CGImageRef;
//  metadata: CGImageMetadataRef; options: CFDictionaryRef); cdecl;
//  external libImageIO name _PU + 'CGImageDestinationAddImageAndMetadata';
//{$EXTERNALSYM CGImageDestinationAddImageAndMetadata}
function CGImageDestinationCopyImageSource(idst: CGImageDestinationRef; isrc: CGImageSourceRef;
  options: CFDictionaryRef; err: CFErrorRef): Integer; cdecl;
  external libImageIO name _PU + 'CGImageDestinationCopyImageSource';
{$EXTERNALSYM CGImageDestinationCopyImageSource}

implementation

function kCGImageMetadataNamespaceExif: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataNamespaceExif');
end;

function kCGImageMetadataNamespaceExifAux: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataNamespaceExifAux');
end;

function kCGImageMetadataNamespaceExifEX: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataNamespaceExifEX');
end;

function kCGImageMetadataNamespaceDublinCore: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataNamespaceDublinCore');
end;

function kCGImageMetadataNamespaceIPTCCore: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataNamespaceIPTCCore');
end;

function kCGImageMetadataNamespacePhotoshop: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataNamespacePhotoshop');
end;

function kCGImageMetadataNamespaceTIFF: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataNamespaceTIFF');
end;

function kCGImageMetadataNamespaceXMPBasic: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataNamespaceXMPBasic');
end;

function kCGImageMetadataNamespaceXMPRights: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataNamespaceXMPRights');
end;

function kCGImageMetadataPrefixExif: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataPrefixExif');
end;

function kCGImageMetadataPrefixExifAux: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataPrefixExifAux');
end;

function kCGImageMetadataPrefixExifEX: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataPrefixExifEX');
end;

function kCGImageMetadataPrefixDublinCore: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataPrefixDublinCore');
end;

function kCGImageMetadataPrefixIPTCCore: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataPrefixIPTCCore');
end;

function kCGImageMetadataPrefixPhotoshop: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataPrefixPhotoshop');
end;

function kCGImageMetadataPrefixTIFF: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataPrefixTIFF');
end;

function kCGImageMetadataPrefixXMPBasic: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataPrefixXMPBasic');
end;

function kCGImageMetadataPrefixXMPRights: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataPrefixXMPRights');
end;

function kCGImageMetadataEnumerateRecursively: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataEnumerateRecursively');
end;

function kCFErrorDomainCGImageMetadata: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCFErrorDomainCGImageMetadata');
end;

function kCGImageSourceTypeIdentifierHint: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageSourceTypeIdentifierHint');
end;

function kCGImageSourceShouldCache: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageSourceShouldCache');
end;

function kCGImageSourceShouldCacheImmediately: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageSourceShouldCacheImmediately');
end;

function kCGImageSourceShouldAllowFloat: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageSourceShouldAllowFloat');
end;

function kCGImageSourceCreateThumbnailFromImageIfAbsent: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageSourceCreateThumbnailFromImageIfAbsent');
end;

function kCGImageSourceCreateThumbnailFromImageAlways: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageSourceCreateThumbnailFromImageAlways');
end;

function kCGImageSourceThumbnailMaxPixelSize: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageSourceThumbnailMaxPixelSize');
end;

function kCGImageSourceCreateThumbnailWithTransform: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageSourceCreateThumbnailWithTransform');
end;

function kCGImageDestinationLossyCompressionQuality: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageDestinationLossyCompressionQuality');
end;

function kCGImageDestinationBackgroundColor: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageDestinationBackgroundColor');
end;

function kCGImageDestinationMetadata: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageDestinationMetadata');
end;

function kCGImageDestinationMergeMetadata: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageDestinationMergeMetadata');
end;

function kCGImageMetadataShouldExcludeXMP: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageMetadataShouldExcludeXMP');
end;

function kCGImageDestinationDateTime: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageDestinationDateTime');
end;

function kCGImageDestinationOrientation: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageDestinationOrientation');
end;

function kCGImagePropertyTIFFDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFDictionary');
end;

function kCGImagePropertyGIFDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGIFDictionary');
end;

function kCGImagePropertyJFIFDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyJFIFDictionary');
end;

function kCGImagePropertyExifDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifDictionary');
end;

function kCGImagePropertyPNGDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGDictionary');
end;

function kCGImagePropertyIPTCDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCDictionary');
end;

function kCGImagePropertyGPSDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSDictionary');
end;

function kCGImagePropertyRawDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyRawDictionary');
end;

function kCGImagePropertyCIFFDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFDictionary');
end;

function kCGImagePropertyMakerCanonDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerCanonDictionary');
end;

function kCGImagePropertyMakerNikonDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonDictionary');
end;

function kCGImagePropertyMakerMinoltaDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerMinoltaDictionary');
end;

function kCGImagePropertyMakerFujiDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerFujiDictionary');
end;

function kCGImagePropertyMakerOlympusDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerOlympusDictionary');
end;

function kCGImagePropertyMakerPentaxDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerPentaxDictionary');
end;

function kCGImageProperty8BIMDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageProperty8BIMDictionary');
end;

function kCGImagePropertyDNGDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyDNGDictionary');
end;

function kCGImagePropertyExifAuxDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifAuxDictionary');
end;

function kCGImagePropertyOpenEXRDictionary: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyOpenEXRDictionary');
end;

function kCGImagePropertyFileSize: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyFileSize');
end;

function kCGImagePropertyPixelHeight: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPixelHeight');
end;

function kCGImagePropertyPixelWidth: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPixelWidth');
end;

function kCGImagePropertyDPIHeight: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyDPIHeight');
end;

function kCGImagePropertyDPIWidth: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyDPIWidth');
end;

function kCGImagePropertyDepth: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyDepth');
end;

function kCGImagePropertyOrientation: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyOrientation');
end;

function kCGImagePropertyIsFloat: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIsFloat');
end;

function kCGImagePropertyIsIndexed: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIsIndexed');
end;

function kCGImagePropertyHasAlpha: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyHasAlpha');
end;

function kCGImagePropertyColorModel: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyColorModel');
end;

function kCGImagePropertyProfileName: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyProfileName');
end;

function kCGImagePropertyColorModelRGB: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyColorModelRGB');
end;

function kCGImagePropertyColorModelGray: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyColorModelGray');
end;

function kCGImagePropertyColorModelCMYK: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyColorModelCMYK');
end;

function kCGImagePropertyColorModelLab: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyColorModelLab');
end;

function kCGImagePropertyTIFFCompression: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFCompression');
end;

function kCGImagePropertyTIFFPhotometricInterpretation: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFPhotometricInterpretation');
end;

function kCGImagePropertyTIFFDocumentName: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFDocumentName');
end;

function kCGImagePropertyTIFFImageDescription: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFImageDescription');
end;

function kCGImagePropertyTIFFMake: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFMake');
end;

function kCGImagePropertyTIFFModel: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFModel');
end;

function kCGImagePropertyTIFFOrientation: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFOrientation');
end;

function kCGImagePropertyTIFFXResolution: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFXResolution');
end;

function kCGImagePropertyTIFFYResolution: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFYResolution');
end;

function kCGImagePropertyTIFFResolutionUnit: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFResolutionUnit');
end;

function kCGImagePropertyTIFFSoftware: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFSoftware');
end;

function kCGImagePropertyTIFFTransferFunction: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFTransferFunction');
end;

function kCGImagePropertyTIFFDateTime: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFDateTime');
end;

function kCGImagePropertyTIFFArtist: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFArtist');
end;

function kCGImagePropertyTIFFHostComputer: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFHostComputer');
end;

function kCGImagePropertyTIFFCopyright: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFCopyright');
end;

function kCGImagePropertyTIFFWhitePoint: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFWhitePoint');
end;

function kCGImagePropertyTIFFPrimaryChromaticities: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyTIFFPrimaryChromaticities');
end;

function kCGImagePropertyJFIFVersion: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyJFIFVersion');
end;

function kCGImagePropertyJFIFXDensity: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyJFIFXDensity');
end;

function kCGImagePropertyJFIFYDensity: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyJFIFYDensity');
end;

function kCGImagePropertyJFIFDensityUnit: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyJFIFDensityUnit');
end;

function kCGImagePropertyJFIFIsProgressive: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyJFIFIsProgressive');
end;

function kCGImagePropertyExifExposureTime: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifExposureTime');
end;

function kCGImagePropertyExifFNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifFNumber');
end;

function kCGImagePropertyExifExposureProgram: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifExposureProgram');
end;

function kCGImagePropertyExifSpectralSensitivity: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSpectralSensitivity');
end;

function kCGImagePropertyExifISOSpeedRatings: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifISOSpeedRatings');
end;

function kCGImagePropertyExifOECF: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifOECF');
end;

function kCGImagePropertyExifSensitivityType: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSensitivityType');
end;

function kCGImagePropertyExifStandardOutputSensitivity: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifStandardOutputSensitivity');
end;

function kCGImagePropertyExifRecommendedExposureIndex: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifRecommendedExposureIndex');
end;

function kCGImagePropertyExifISOSpeed: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifISOSpeed');
end;

function kCGImagePropertyExifISOSpeedLatitudeyyy: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifISOSpeedLatitudeyyy');
end;

function kCGImagePropertyExifISOSpeedLatitudezzz: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifISOSpeedLatitudezzz');
end;

function kCGImagePropertyExifVersion: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifVersion');
end;

function kCGImagePropertyExifDateTimeOriginal: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifDateTimeOriginal');
end;

function kCGImagePropertyExifDateTimeDigitized: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifDateTimeDigitized');
end;

function kCGImagePropertyExifComponentsConfiguration: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifComponentsConfiguration');
end;

function kCGImagePropertyExifCompressedBitsPerPixel: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifCompressedBitsPerPixel');
end;

function kCGImagePropertyExifShutterSpeedValue: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifShutterSpeedValue');
end;

function kCGImagePropertyExifApertureValue: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifApertureValue');
end;

function kCGImagePropertyExifBrightnessValue: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifBrightnessValue');
end;

function kCGImagePropertyExifExposureBiasValue: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifExposureBiasValue');
end;

function kCGImagePropertyExifMaxApertureValue: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifMaxApertureValue');
end;

function kCGImagePropertyExifSubjectDistance: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSubjectDistance');
end;

function kCGImagePropertyExifMeteringMode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifMeteringMode');
end;

function kCGImagePropertyExifLightSource: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifLightSource');
end;

function kCGImagePropertyExifFlash: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifFlash');
end;

function kCGImagePropertyExifFocalLength: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifFocalLength');
end;

function kCGImagePropertyExifSubjectArea: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSubjectArea');
end;

function kCGImagePropertyExifMakerNote: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifMakerNote');
end;

function kCGImagePropertyExifUserComment: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifUserComment');
end;

function kCGImagePropertyExifSubsecTime: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSubsecTime');
end;

function kCGImagePropertyExifSubsecTimeOrginal: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSubsecTimeOrginal');
end;

function kCGImagePropertyExifSubsecTimeDigitized: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSubsecTimeDigitized');
end;

function kCGImagePropertyExifFlashPixVersion: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifFlashPixVersion');
end;

function kCGImagePropertyExifColorSpace: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifColorSpace');
end;

function kCGImagePropertyExifPixelXDimension: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifPixelXDimension');
end;

function kCGImagePropertyExifPixelYDimension: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifPixelYDimension');
end;

function kCGImagePropertyExifRelatedSoundFile: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifRelatedSoundFile');
end;

function kCGImagePropertyExifFlashEnergy: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifFlashEnergy');
end;

function kCGImagePropertyExifSpatialFrequencyResponse: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSpatialFrequencyResponse');
end;

function kCGImagePropertyExifFocalPlaneXResolution: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifFocalPlaneXResolution');
end;

function kCGImagePropertyExifFocalPlaneYResolution: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifFocalPlaneYResolution');
end;

function kCGImagePropertyExifFocalPlaneResolutionUnit: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifFocalPlaneResolutionUnit');
end;

function kCGImagePropertyExifSubjectLocation: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSubjectLocation');
end;

function kCGImagePropertyExifExposureIndex: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifExposureIndex');
end;

function kCGImagePropertyExifSensingMethod: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSensingMethod');
end;

function kCGImagePropertyExifFileSource: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifFileSource');
end;

function kCGImagePropertyExifSceneType: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSceneType');
end;

function kCGImagePropertyExifCFAPattern: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifCFAPattern');
end;

function kCGImagePropertyExifCustomRendered: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifCustomRendered');
end;

function kCGImagePropertyExifExposureMode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifExposureMode');
end;

function kCGImagePropertyExifWhiteBalance: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifWhiteBalance');
end;

function kCGImagePropertyExifDigitalZoomRatio: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifDigitalZoomRatio');
end;

function kCGImagePropertyExifFocalLenIn35mmFilm: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifFocalLenIn35mmFilm');
end;

function kCGImagePropertyExifSceneCaptureType: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSceneCaptureType');
end;

function kCGImagePropertyExifGainControl: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifGainControl');
end;

function kCGImagePropertyExifContrast: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifContrast');
end;

function kCGImagePropertyExifSaturation: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSaturation');
end;

function kCGImagePropertyExifSharpness: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSharpness');
end;

function kCGImagePropertyExifDeviceSettingDescription: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifDeviceSettingDescription');
end;

function kCGImagePropertyExifSubjectDistRange: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifSubjectDistRange');
end;

function kCGImagePropertyExifImageUniqueID: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifImageUniqueID');
end;

function kCGImagePropertyExifCameraOwnerName: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifCameraOwnerName');
end;

function kCGImagePropertyExifBodySerialNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifBodySerialNumber');
end;

function kCGImagePropertyExifLensSpecification: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifLensSpecification');
end;

function kCGImagePropertyExifLensMake: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifLensMake');
end;

function kCGImagePropertyExifLensModel: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifLensModel');
end;

function kCGImagePropertyExifLensSerialNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifLensSerialNumber');
end;

function kCGImagePropertyExifGamma: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifGamma');
end;

function kCGImagePropertyExifAuxLensInfo: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifAuxLensInfo');
end;

function kCGImagePropertyExifAuxLensModel: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifAuxLensModel');
end;

function kCGImagePropertyExifAuxSerialNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifAuxSerialNumber');
end;

function kCGImagePropertyExifAuxLensID: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifAuxLensID');
end;

function kCGImagePropertyExifAuxLensSerialNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifAuxLensSerialNumber');
end;

function kCGImagePropertyExifAuxImageNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifAuxImageNumber');
end;

function kCGImagePropertyExifAuxFlashCompensation: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifAuxFlashCompensation');
end;

function kCGImagePropertyExifAuxOwnerName: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifAuxOwnerName');
end;

function kCGImagePropertyExifAuxFirmware: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyExifAuxFirmware');
end;

function kCGImagePropertyGIFLoopCount: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGIFLoopCount');
end;

function kCGImagePropertyGIFDelayTime: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGIFDelayTime');
end;

function kCGImagePropertyGIFImageColorMap: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGIFImageColorMap');
end;

function kCGImagePropertyGIFHasGlobalColorMap: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGIFHasGlobalColorMap');
end;

function kCGImagePropertyGIFUnclampedDelayTime: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGIFUnclampedDelayTime');
end;

function kCGImagePropertyPNGGamma: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGGamma');
end;

function kCGImagePropertyPNGInterlaceType: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGInterlaceType');
end;

function kCGImagePropertyPNGXPixelsPerMeter: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGXPixelsPerMeter');
end;

function kCGImagePropertyPNGYPixelsPerMeter: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGYPixelsPerMeter');
end;

function kCGImagePropertyPNGsRGBIntent: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGsRGBIntent');
end;

function kCGImagePropertyPNGChromaticities: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGChromaticities');
end;

function kCGImagePropertyPNGAuthor: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGAuthor');
end;

function kCGImagePropertyPNGCopyright: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGCopyright');
end;

function kCGImagePropertyPNGCreationTime: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGCreationTime');
end;

function kCGImagePropertyPNGDescription: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGDescription');
end;

function kCGImagePropertyPNGModificationTime: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGModificationTime');
end;

function kCGImagePropertyPNGSoftware: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGSoftware');
end;

function kCGImagePropertyPNGTitle: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyPNGTitle');
end;

function kCGImagePropertyGPSVersion: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSVersion');
end;

function kCGImagePropertyGPSLatitudeRef: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSLatitudeRef');
end;

function kCGImagePropertyGPSLatitude: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSLatitude');
end;

function kCGImagePropertyGPSLongitudeRef: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSLongitudeRef');
end;

function kCGImagePropertyGPSLongitude: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSLongitude');
end;

function kCGImagePropertyGPSAltitudeRef: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSAltitudeRef');
end;

function kCGImagePropertyGPSAltitude: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSAltitude');
end;

function kCGImagePropertyGPSTimeStamp: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSTimeStamp');
end;

function kCGImagePropertyGPSSatellites: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSSatellites');
end;

function kCGImagePropertyGPSStatus: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSStatus');
end;

function kCGImagePropertyGPSMeasureMode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSMeasureMode');
end;

function kCGImagePropertyGPSDOP: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSDOP');
end;

function kCGImagePropertyGPSSpeedRef: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSSpeedRef');
end;

function kCGImagePropertyGPSSpeed: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSSpeed');
end;

function kCGImagePropertyGPSTrackRef: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSTrackRef');
end;

function kCGImagePropertyGPSTrack: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSTrack');
end;

function kCGImagePropertyGPSImgDirectionRef: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSImgDirectionRef');
end;

function kCGImagePropertyGPSImgDirection: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSImgDirection');
end;

function kCGImagePropertyGPSMapDatum: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSMapDatum');
end;

function kCGImagePropertyGPSDestLatitudeRef: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSDestLatitudeRef');
end;

function kCGImagePropertyGPSDestLatitude: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSDestLatitude');
end;

function kCGImagePropertyGPSDestLongitudeRef: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSDestLongitudeRef');
end;

function kCGImagePropertyGPSDestLongitude: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSDestLongitude');
end;

function kCGImagePropertyGPSDestBearingRef: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSDestBearingRef');
end;

function kCGImagePropertyGPSDestBearing: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSDestBearing');
end;

function kCGImagePropertyGPSDestDistanceRef: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSDestDistanceRef');
end;

function kCGImagePropertyGPSDestDistance: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSDestDistance');
end;

function kCGImagePropertyGPSProcessingMethod: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSProcessingMethod');
end;

function kCGImagePropertyGPSAreaInformation: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSAreaInformation');
end;

function kCGImagePropertyGPSDateStamp: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSDateStamp');
end;

function kCGImagePropertyGPSDifferental: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyGPSDifferental');
end;

function kCGImagePropertyIPTCObjectTypeReference: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCObjectTypeReference');
end;

function kCGImagePropertyIPTCObjectAttributeReference: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCObjectAttributeReference');
end;

function kCGImagePropertyIPTCObjectName: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCObjectName');
end;

function kCGImagePropertyIPTCEditStatus: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCEditStatus');
end;

function kCGImagePropertyIPTCEditorialUpdate: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCEditorialUpdate');
end;

function kCGImagePropertyIPTCUrgency: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCUrgency');
end;

function kCGImagePropertyIPTCSubjectReference: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCSubjectReference');
end;

function kCGImagePropertyIPTCCategory: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCCategory');
end;

function kCGImagePropertyIPTCSupplementalCategory: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCSupplementalCategory');
end;

function kCGImagePropertyIPTCFixtureIdentifier: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCFixtureIdentifier');
end;

function kCGImagePropertyIPTCKeywords: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCKeywords');
end;

function kCGImagePropertyIPTCContentLocationCode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCContentLocationCode');
end;

function kCGImagePropertyIPTCContentLocationName: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCContentLocationName');
end;

function kCGImagePropertyIPTCReleaseDate: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCReleaseDate');
end;

function kCGImagePropertyIPTCReleaseTime: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCReleaseTime');
end;

function kCGImagePropertyIPTCExpirationDate: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCExpirationDate');
end;

function kCGImagePropertyIPTCExpirationTime: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCExpirationTime');
end;

function kCGImagePropertyIPTCSpecialInstructions: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCSpecialInstructions');
end;

function kCGImagePropertyIPTCActionAdvised: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCActionAdvised');
end;

function kCGImagePropertyIPTCReferenceService: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCReferenceService');
end;

function kCGImagePropertyIPTCReferenceDate: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCReferenceDate');
end;

function kCGImagePropertyIPTCReferenceNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCReferenceNumber');
end;

function kCGImagePropertyIPTCDateCreated: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCDateCreated');
end;

function kCGImagePropertyIPTCTimeCreated: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCTimeCreated');
end;

function kCGImagePropertyIPTCDigitalCreationDate: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCDigitalCreationDate');
end;

function kCGImagePropertyIPTCDigitalCreationTime: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCDigitalCreationTime');
end;

function kCGImagePropertyIPTCOriginatingProgram: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCOriginatingProgram');
end;

function kCGImagePropertyIPTCProgramVersion: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCProgramVersion');
end;

function kCGImagePropertyIPTCObjectCycle: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCObjectCycle');
end;

function kCGImagePropertyIPTCByline: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCByline');
end;

function kCGImagePropertyIPTCBylineTitle: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCBylineTitle');
end;

function kCGImagePropertyIPTCCity: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCCity');
end;

function kCGImagePropertyIPTCSubLocation: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCSubLocation');
end;

function kCGImagePropertyIPTCProvinceState: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCProvinceState');
end;

function kCGImagePropertyIPTCCountryPrimaryLocationCode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCCountryPrimaryLocationCode');
end;

function kCGImagePropertyIPTCCountryPrimaryLocationName: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCCountryPrimaryLocationName');
end;

function kCGImagePropertyIPTCOriginalTransmissionReference: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCOriginalTransmissionReference');
end;

function kCGImagePropertyIPTCHeadline: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCHeadline');
end;

function kCGImagePropertyIPTCCredit: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCCredit');
end;

function kCGImagePropertyIPTCSource: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCSource');
end;

function kCGImagePropertyIPTCCopyrightNotice: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCCopyrightNotice');
end;

function kCGImagePropertyIPTCContact: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCContact');
end;

function kCGImagePropertyIPTCCaptionAbstract: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCCaptionAbstract');
end;

function kCGImagePropertyIPTCWriterEditor: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCWriterEditor');
end;

function kCGImagePropertyIPTCImageType: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCImageType');
end;

function kCGImagePropertyIPTCImageOrientation: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCImageOrientation');
end;

function kCGImagePropertyIPTCLanguageIdentifier: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCLanguageIdentifier');
end;

function kCGImagePropertyIPTCStarRating: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCStarRating');
end;

function kCGImagePropertyIPTCCreatorContactInfo: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCCreatorContactInfo');
end;

function kCGImagePropertyIPTCRightsUsageTerms: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCRightsUsageTerms');
end;

function kCGImagePropertyIPTCScene: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCScene');
end;

function kCGImagePropertyIPTCContactInfoCity: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCContactInfoCity');
end;

function kCGImagePropertyIPTCContactInfoCountry: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCContactInfoCountry');
end;

function kCGImagePropertyIPTCContactInfoAddress: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCContactInfoAddress');
end;

function kCGImagePropertyIPTCContactInfoPostalCode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCContactInfoPostalCode');
end;

function kCGImagePropertyIPTCContactInfoStateProvince: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCContactInfoStateProvince');
end;

function kCGImagePropertyIPTCContactInfoEmails: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCContactInfoEmails');
end;

function kCGImagePropertyIPTCContactInfoPhones: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCContactInfoPhones');
end;

function kCGImagePropertyIPTCContactInfoWebURLs: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyIPTCContactInfoWebURLs');
end;

function kCGImageProperty8BIMLayerNames: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImageProperty8BIMLayerNames');
end;

function kCGImagePropertyDNGVersion: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyDNGVersion');
end;

function kCGImagePropertyDNGBackwardVersion: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyDNGBackwardVersion');
end;

function kCGImagePropertyDNGUniqueCameraModel: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyDNGUniqueCameraModel');
end;

function kCGImagePropertyDNGLocalizedCameraModel: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyDNGLocalizedCameraModel');
end;

function kCGImagePropertyDNGCameraSerialNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyDNGCameraSerialNumber');
end;

function kCGImagePropertyDNGLensInfo: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyDNGLensInfo');
end;

function kCGImagePropertyCIFFDescription: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFDescription');
end;

function kCGImagePropertyCIFFFirmware: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFFirmware');
end;

function kCGImagePropertyCIFFOwnerName: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFOwnerName');
end;

function kCGImagePropertyCIFFImageName: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFImageName');
end;

function kCGImagePropertyCIFFImageFileName: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFImageFileName');
end;

function kCGImagePropertyCIFFReleaseMethod: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFReleaseMethod');
end;

function kCGImagePropertyCIFFReleaseTiming: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFReleaseTiming');
end;

function kCGImagePropertyCIFFRecordID: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFRecordID');
end;

function kCGImagePropertyCIFFSelfTimingTime: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFSelfTimingTime');
end;

function kCGImagePropertyCIFFCameraSerialNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFCameraSerialNumber');
end;

function kCGImagePropertyCIFFImageSerialNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFImageSerialNumber');
end;

function kCGImagePropertyCIFFContinuousDrive: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFContinuousDrive');
end;

function kCGImagePropertyCIFFFocusMode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFFocusMode');
end;

function kCGImagePropertyCIFFMeteringMode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFMeteringMode');
end;

function kCGImagePropertyCIFFShootingMode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFShootingMode');
end;

function kCGImagePropertyCIFFLensModel: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFLensModel');
end;

function kCGImagePropertyCIFFLensMaxMM: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFLensMaxMM');
end;

function kCGImagePropertyCIFFLensMinMM: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFLensMinMM');
end;

function kCGImagePropertyCIFFWhiteBalanceIndex: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFWhiteBalanceIndex');
end;

function kCGImagePropertyCIFFFlashExposureComp: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFFlashExposureComp');
end;

function kCGImagePropertyCIFFMeasuredEV: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyCIFFMeasuredEV');
end;

function kCGImagePropertyMakerNikonISOSetting: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonISOSetting');
end;

function kCGImagePropertyMakerNikonColorMode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonColorMode');
end;

function kCGImagePropertyMakerNikonQuality: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonQuality');
end;

function kCGImagePropertyMakerNikonWhiteBalanceMode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonWhiteBalanceMode');
end;

function kCGImagePropertyMakerNikonSharpenMode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonSharpenMode');
end;

function kCGImagePropertyMakerNikonFocusMode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonFocusMode');
end;

function kCGImagePropertyMakerNikonFlashSetting: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonFlashSetting');
end;

function kCGImagePropertyMakerNikonISOSelection: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonISOSelection');
end;

function kCGImagePropertyMakerNikonFlashExposureComp: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonFlashExposureComp');
end;

function kCGImagePropertyMakerNikonImageAdjustment: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonImageAdjustment');
end;

function kCGImagePropertyMakerNikonLensAdapter: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonLensAdapter');
end;

function kCGImagePropertyMakerNikonLensType: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonLensType');
end;

function kCGImagePropertyMakerNikonLensInfo: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonLensInfo');
end;

function kCGImagePropertyMakerNikonFocusDistance: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonFocusDistance');
end;

function kCGImagePropertyMakerNikonDigitalZoom: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonDigitalZoom');
end;

function kCGImagePropertyMakerNikonShootingMode: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonShootingMode');
end;

function kCGImagePropertyMakerNikonCameraSerialNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonCameraSerialNumber');
end;

function kCGImagePropertyMakerNikonShutterCount: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerNikonShutterCount');
end;

function kCGImagePropertyMakerCanonOwnerName: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerCanonOwnerName');
end;

function kCGImagePropertyMakerCanonCameraSerialNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerCanonCameraSerialNumber');
end;

function kCGImagePropertyMakerCanonImageSerialNumber: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerCanonImageSerialNumber');
end;

function kCGImagePropertyMakerCanonFlashExposureComp: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerCanonFlashExposureComp');
end;

function kCGImagePropertyMakerCanonContinuousDrive: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerCanonContinuousDrive');
end;

function kCGImagePropertyMakerCanonLensModel: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerCanonLensModel');
end;

function kCGImagePropertyMakerCanonFirmware: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerCanonFirmware');
end;

function kCGImagePropertyMakerCanonAspectRatioInfo: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyMakerCanonAspectRatioInfo');
end;

function kCGImagePropertyOpenEXRAspectRatio: CFStringRef;
begin
  Result := CocoaObjectIdConst(libImageIO, 'kCGImagePropertyOpenEXRAspectRatio');
end;

end.
