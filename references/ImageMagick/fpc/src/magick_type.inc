{
  Copyright 1999-2005 ImageMagick Studio LLC, a non-profit organization
  dedicated to making software imaging solutions freely available.
  
  You may not use this file except in compliance with the License.
  obtain a copy of the License at
  
    http://www.imagemagick.org/script/license.php
  
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

  ImageMagick structure members.

  NOTE: This is a simplifyed version of magick/magick-type.h
}

const MaxTextExtent = 4096;

const
  MagickFalse = 0;
  MagickTrue = 1;

type
  MagickBooleanType = culong;

  PMagickBooleanType = ^MagickBooleanType;

//#include "magick/semaphore.h"

{
  The Quantum Depth was fixed at 16 to make the code simpler

  In the future there is the possibility to go to the c bindings
 and implement the variable QuantumDepth here
}
const
  QuantumDepth = 16;

{$define QuantumLeap}

const
  MagickEpsilon = 1.0E-10;
  MaxColormapSize = 65536;
  MaxMap = 65535;
  QuantumFormat = '%u';
  QuantumRange = 65535;

type
  MagickRealType = double;
  Quantum = Word;

{
  Type declarations.
}
type
  MagickStatusType = Word;
  MagickOffsetType = Int64;
  MagickSizeType = Int64;

const
  UndefinedChannel = 0;
  RedChannel = $0001;
  GrayChannel = $0001;
  CyanChannel = $0001;
  GreenChannel = $0002;
  MagentaChannel = $0002;
  BlueChannel = $0004;
  YellowChannel = $0004;
  AlphaChannel = $0008;
  OpacityChannel = $0008;
  MatteChannel = $0008;  // deprecated
  BlackChannel = $0020;
  IndexChannel = $0020;
  AllChannels = $ff;
  
type
  ChannelType = Byte;

  PChannelType = ^ChannelType;

type
 ClassType = (
  UndefinedClass,
  DirectClass,
  PseudoClass
 );

  PClassType = ^ClassType;

type
 ColorspaceType = (
  UndefinedColorspace,
  RGBColorspace,
  GRAYColorspace,
  TransparentColorspace,
  OHTAColorspace,
  LABColorspace,
  XYZColorspace,
  YCbCrColorspace,
  YCCColorspace,
  YIQColorspace,
  YPbPrColorspace,
  YUVColorspace,
  CMYKColorspace,
  sRGBColorspace,
  HSBColorspace,
  HSLColorspace,
  HWBColorspace,
  Rec601LumaColorspace,
  Rec709LumaColorspace,
  LogColorspace
 );

  PColorspaceType = ^ColorspaceType;

type
 CompositeOperator = (
  UndefinedCompositeOp,
  NoCompositeOp,
  AddCompositeOp,
  AtopCompositeOp,
  BlendCompositeOp,
  BumpmapCompositeOp,
  ClearCompositeOp,
  ColorBurnCompositeOp,
  ColorDodgeCompositeOp,
  ColorizeCompositeOp,
  CopyBlackCompositeOp,
  CopyBlueCompositeOp,
  CopyCompositeOp,
  CopyCyanCompositeOp,
  CopyGreenCompositeOp,
  CopyMagentaCompositeOp,
  CopyOpacityCompositeOp,
  CopyRedCompositeOp,
  CopyYellowCompositeOp,
  DarkenCompositeOp,
  DstAtopCompositeOp,
  DstCompositeOp,
  DstInCompositeOp,
  DstOutCompositeOp,
  DstOverCompositeOp,
  DifferenceCompositeOp,
  DisplaceCompositeOp,
  DissolveCompositeOp,
  ExclusionCompositeOp,
  HardLightCompositeOp,
  HueCompositeOp,
  InCompositeOp,
  LightenCompositeOp,
  LuminizeCompositeOp,
  MinusCompositeOp,
  ModulateCompositeOp,
  MultiplyCompositeOp,
  OutCompositeOp,
  OverCompositeOp,
  OverlayCompositeOp,
  PlusCompositeOp,
  ReplaceCompositeOp,
  SaturateCompositeOp,
  ScreenCompositeOp,
  SoftLightCompositeOp,
  SrcAtopCompositeOp,
  SrcCompositeOp,
  SrcInCompositeOp,
  SrcOutCompositeOp,
  SrcOverCompositeOp,
  SubtractCompositeOp,
  ThresholdCompositeOp,
  XorCompositeOp
 );

  PCompositeOperator = ^CompositeOperator;

type
  // Verified with imagemagick 6.7.7-10
  CompressionType = (
    UndefinedCompression,
    NoCompression,
    BZipCompression,
    DXT1Compression,
    DXT3Compression,
    DXT5Compression,
    FaxCompression,           {* CCITT Group 3 compression *}
    Group4Compression,        {* CCITT Group 4 compression (also used in faxes) *}
    JPEGCompression,
    JPEG2000Compression,      {* ISO/IEC std 15444-1 *}
    LosslessJPEGCompression,
    LZWCompression,
    RLECompression,
    ZipCompression,
    ZipSCompression,
    PizCompression,
    Pxr24Compression,
    B44Compression,
    B44ACompression,
    LZMACompression,          {* Lempel-Ziv-Markov chain algorithm *}
    JBIG1Compression,         {* ISO/IEC std 11544 / ITU-T rec T.82 *}
    JBIG2Compression          {* ISO/IEC std 14492 / ITU-T rec T.88 *}
  );

  PCompressionType = ^CompressionType;
  
const
  UnrecognizedDispose = 0;
  UndefinedDispose = 0;
  NoneDispose = 1;
  BackgroundDispose = 2;
  PreviousDispose = 3;

type
  DisposeType = Byte;

  PDisposeType = ^DisposeType;
  
type
 EndianType = (
  UndefinedEndian,
  LSBEndian,
  MSBEndian
 );

  PEndianType = ^EndianType;

const
  UndefinedException = 0;
  WarningException = 300;
  ResourceLimitWarning = 300;
  TypeWarning = 305;
  OptionWarning = 310;
  DelegateWarning = 315;
  MissingDelegateWarning = 320;
  CorruptImageWarning = 325;
  FileOpenWarning = 330;
  BlobWarning = 335;
  StreamWarning = 340;
  CacheWarning = 345;
  CoderWarning = 350;
  ModuleWarning = 355;
  DrawWarning = 360;
  ImageWarning = 365;
  WandWarning = 370;
  XServerWarning = 380;
  MonitorWarning = 385;
  RegistryWarning = 390;
  ConfigureWarning = 395;
  ErrorException = 400;
  ResourceLimitError = 400;
  TypeError = 405;
  OptionError = 410;
  DelegateError = 415;
  MissingDelegateError = 420;
  CorruptImageError = 425;
  FileOpenError = 430;
  BlobError = 435;
  StreamError = 440;
  CacheError = 445;
  CoderError = 450;
  ModuleError = 455;
  DrawError = 460;
  ImageError = 465;
  WandError = 470;
  XServerError = 480;
  MonitorError = 485;
  RegistryError = 490;
  ConfigureError = 495;
  FatalErrorException = 700;
  ResourceLimitFatalError = 700;
  TypeFatalError = 705;
  OptionFatalError = 710;
  DelegateFatalError = 715;
  MissingDelegateFatalError = 720;
  CorruptImageFatalError = 725;
  FileOpenFatalError = 730;
  BlobFatalError = 735;
  StreamFatalError = 740;
  CacheFatalError = 745;
  CoderFatalError = 750;
  ModuleFatalError = 755;
  DrawFatalError = 760;
  ImageFatalError = 765;
  WandFatalError = 770;
  XServerFatalError = 780;
  MonitorFatalError = 785;
  RegistryFatalError = 790;
  ConfigureFatalError = 795;
  
type
  ExceptionType = Word;
  
  PExceptionType = ^ExceptionType;

type
 FilterTypes = (
  UndefinedFilter,
  PointFilter,
  BoxFilter,
  TriangleFilter,
  HermiteFilter,
  HanningFilter,
  HammingFilter,
  BlackmanFilter,
  GaussianFilter,
  QuadraticFilter,
  CubicFilter,
  CatromFilter,
  MitchellFilter,
  LanczosFilter,
  BesselFilter,
  SincFilter
 );

  PFilterTypes = ^FilterTypes;

const
  UndefinedGravity = 0;
  ForgetGravity = 0;
  NorthWestGravity = 1;
  NorthGravity = 2;
  NorthEastGravity = 3;
  WestGravity = 4;
  CenterGravity = 5;
  EastGravity = 6;
  SouthWestGravity = 7;
  SouthGravity = 8;
  SouthEastGravity = 9;
  StaticGravity = 10;

type
  GravityType = Byte;

  PGravityType = ^GravityType;

type ImageType = (
  UndefinedType,
  BilevelType,
  GrayscaleType,
  GrayscaleMatteType,
  PaletteType,
  PaletteMatteType,
  TrueColorType,
  TrueColorMatteType,
  ColorSeparationType,
  ColorSeparationMatteType,
  OptimizeType
 );

  PImageType = ^ImageType;

type
 InterlaceType = (
  UndefinedInterlace,
  NoInterlace,
  LineInterlace,
  PlaneInterlace,
  PartitionInterlace
 );

  PInterlaceType = ^InterlaceType;

type
 OrientationType = (
  UndefinedOrientation,
  TopLeftOrientation,
  TopRightOrientation,
  BottomRightOrientation,
  BottomLeftOrientation,
  LeftTopOrientation,
  RightTopOrientation,
  RightBottomOrientation,
  LeftBottomOrientation
 );

  POrientationType = ^OrientationType;

type PreviewType = (
  UndefinedPreview,
  RotatePreview,
  ShearPreview,
  RollPreview,
  HuePreview,
  SaturationPreview,
  BrightnessPreview,
  GammaPreview,
  SpiffPreview,
  DullPreview,
  GrayscalePreview,
  QuantizePreview,
  DespecklePreview,
  ReduceNoisePreview,
  AddNoisePreview,
  SharpenPreview,
  BlurPreview,
  ThresholdPreview,
  EdgeDetectPreview,
  SpreadPreview,
  SolarizePreview,
  ShadePreview,
  RaisePreview,
  SegmentPreview,
  SwirlPreview,
  ImplodePreview,
  WavePreview,
  OilPaintPreview,
  CharcoalDrawingPreview,
  JPEGPreview
 );

  PPreviewType = ^PreviewType;

type RenderingIntent = (
  UndefinedIntent,
  SaturationIntent,
  PerceptualIntent,
  AbsoluteIntent,
  RelativeIntent
 );

  PRenderingIntent = ^RenderingIntent;

type ResolutionType = (
  UndefinedResolution,
  PixelsPerInchResolution,
  PixelsPerCentimeterResolution
 );

  PResolutionType = ^ResolutionType;

type TimerState = (
  UndefinedTimerState,
  StoppedTimerState,
  RunningTimerState
 );

  PTimerState = ^TimerState;

type
  AffineMatrix = record
    sx, rx, ry, sy, tx, ty: double;
  end;
  
  PAffineMatrix = ^AffineMatrix;

type
 IndexPacket = Quantum;

type
  PixelPacket = record
{$ifdef WORDS_BIGENDIAN}
   red, green, blue, opacity: Quantum;
{$else}
   blue, green, red, opacity: Quantum;
{$endif}
  end;
  
  PPixelPacket = ^PixelPacket;

type
  ColorPacket = record
   pixel: PixelPacket;
   index: IndexPacket;
   count: culong;
  end;
  
  PColorPacket = ^ColorPacket;

type
  ErrorInfo = record
   mean_error_per_pixel, normalized_mean_error, normalized_maximum_error: double;
  end;

type
  PrimaryInfo = record
   x, y, z: double;
  end;

type 
  ProfileInfo = record
   name: PChar;
   length: cint;
   info: PChar;
   signature: culong;
  end;
  
  PProfileInfo = ^ProfileInfo;

type 
  RectangleInfo = record
   width, height: culong;
   x, y: culong;
  end;

type
  SegmentInfo = record
   x1, y1, x2, y2: double;
  end;

type
  Timer = record
   start, stop, total: double;
  end;
  
  PTimer = ^Timer;

type
  TimerInfo = record
   user, elapsed: Timer;
   state: TimerState;
   signature: culong;
  end;
  
  PTimerInfo = ^TimerInfo;

type
  ChromaticityInfo = record
   red_primary, green_primary, blue_primary, white_point: PrimaryInfo;
 end;

type
  ExceptionInfo = record
   //severity: ExceptionType;
   //error_number: Shortint;
   severity: culong;
   error_number: culong;
   reason, description: PChar;
	 exceptions: Pointer;
	 relinquish: MagickBooleanType;
   semaphore: Pointer; //PSemaphoreInfo;
   signature: culong;
  end;
  
  PExceptionInfo = ^ExceptionInfo;

//typedef struct _Ascii85Info _Ascii85Info_;

//typedef struct _BlobInfo _BlobInfo_;

type
  MagickProgressMonitor = function (const str: PChar; const para: MagickOffsetType;
   const sizetype: MagickSizeType; param: Pointer): PMagickBooleanType;

type
  Image = record
   storage_class: ClassType;
   colorspace: ColorspaceType;
   compression: CompressionType;
   quality: culong;
   orientation: OrientationType;
   taint, matte: MagickBooleanType;
   columns, rows, depth, colors: culong;
   colormap: PPixelPacket;
   background_color, border_color, matte_color: PixelPacket;
   gamma: double;
   chromaticity: ChromaticityInfo;
   rendering_intent: RenderingIntent;
   profiles: Pointer;
   units: ResolutionType;
   montage, directory, geometry: PChar;
   offset: cint;
   x_resolution, y_resolution: double;
   page, extract_info, tile_info: RectangleInfo;  // deprecated
   bias, blur, fuzz: double;
   filter: FilterTypes;
   interlace: InterlaceType;
   endian: EndianType;
   gravity: GravityType;
   compose: CompositeOperator;
   dispose: DisposeType;
   clip_mask: Pointer; // PImage
   scene, delay, ticks_per_second, iterations, total_colors: culong;
   start_loop: cint;
   error: ErrorInfo;
   timer: TimerInfo;
   progress_monitor: MagickProgressMonitor;
   client_data: Pointer;
   cache: Pointer;
   attributes: Pointer;
   ascii85: Pointer; //_Ascii85Info_
   blob: Pointer; // _BlobInfo_
   filename, magick_filename, magick: array[1..MaxTextExtent] of char;
   magick_columns, magick_rows: culong;
   exception: ExceptionInfo;
   debug: MagickBooleanType;
   reference_count: Longint;
   semaphore: Pointer; //PSemaphoreInfo;
   color_profile, iptc_profile: ProfileInfo;
   generic_profile: PProfileInfo;
   generic_profiles: culong;  // this & ProfileInfo is deprecated
   signature: culong;
   previous, list, next: Pointer;
{  struct _Image
    *previous,
    *list,
    *next;}
 end;
 
 PImage = ^Image;

//type
//  size_t = PStreamHandler(procedure (const image: PImage;
//   const param: Pointer; const) );

type
  ImageInfo = record
   compression: CompressionType;
   orientation: OrientationType;
   temporary, adjoin, affirm, antialias: MagickBooleanType;
   size, extract, page, scenes: PChar;
   scene, number_scenes, depth: culong;
   interlace: InterlaceType;
   endian: EndianType;
   units: ResolutionType;
   quality: culong;
   sampling_factor, server_name, font, texture, density: PChar;
   pointsize, fuzz: double;
   background_color, border_color, matte_color: PixelPacket;
   dither, monochrome: MagickBooleanType;
   colors: culong;
   colorspace: ColorspaceType;
   type_: ImageType ;
   preview_type: PreviewType;
   group: Longint;
   ping, verbose: MagickBooleanType;
   view, authenticate: PChar;
   channel: ChannelType;
   attributes: PImage;
   options: Pointer;
   progress_monitor: MagickProgressMonitor;
   client_data, cache: Pointer;
   stream: culong; //StreamHandler;
   file_: Pointer; // *file
   blob: Pointer;
   length: cint;
   magick, unique, zero, filename: array[1..MaxTextExtent] of char;
   debug: MagickBooleanType;

  {
    Deprecated.
  }
   tile: PChar;
   subimage, subrange: culong;
   pen: PixelPacket;
   signature: culong;
 end;

  PImageInfo = ^ImageInfo;


