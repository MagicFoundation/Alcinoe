unit ALImageMagick;

interface

//
// https://www.imagemagick.org/script/magick-wand.php
// https://www.imagemagick.org/MagickWand/
//


{$MINENUMSIZE 4} // https://stackoverflow.com/questions/48953749/why-this-c-to-pascal-conversion-crash

uses winapi.windows;

const
  WandExport = 'CORE_RL_MagickWand_.dll';

type
  ExceptionType = (
    UndefinedException,
    WarningException = 300,
    ResourceLimitWarning = 300,
    TypeWarning = 305,
    OptionWarning = 310,
    DelegateWarning = 315,
    MissingDelegateWarning = 320,
    CorruptImageWarning = 325,
    FileOpenWarning = 330,
    BlobWarning = 335,
    StreamWarning = 340,
    CacheWarning = 345,
    CoderWarning = 350,
    FilterWarning = 352,
    ModuleWarning = 355,
    DrawWarning = 360,
    ImageWarning = 365,
    WandWarning = 370,
    RandomWarning = 375,
    XServerWarning = 380,
    MonitorWarning = 385,
    RegistryWarning = 390,
    ConfigureWarning = 395,
    PolicyWarning = 399,
    ErrorException = 400,
    ResourceLimitError = 400,
    TypeError = 405,
    OptionError = 410,
    DelegateError = 415,
    MissingDelegateError = 420,
    CorruptImageError = 425,
    FileOpenError = 430,
    BlobError = 435,
    StreamError = 440,
    CacheError = 445,
    CoderError = 450,
    FilterError = 452,
    ModuleError = 455,
    DrawError = 460,
    ImageError = 465,
    WandError = 470,
    RandomError = 475,
    XServerError = 480,
    MonitorError = 485,
    RegistryError = 490,
    ConfigureError = 495,
    PolicyError = 499,
    FatalErrorException = 700,
    ResourceLimitFatalError = 700,
    TypeFatalError = 705,
    OptionFatalError = 710,
    DelegateFatalError = 715,
    MissingDelegateFatalError = 720,
    CorruptImageFatalError = 725,
    FileOpenFatalError = 730,
    BlobFatalError = 735,
    StreamFatalError = 740,
    CacheFatalError = 745,
    CoderFatalError = 750,
    FilterFatalError = 752,
    ModuleFatalError = 755,
    DrawFatalError = 760,
    ImageFatalError = 765,
    WandFatalError = 770,
    RandomFatalError = 775,
    XServerFatalError = 780,
    MonitorFatalError = 785,
    RegistryFatalError = 790,
    ConfigureFatalError = 795,
    PolicyFatalError = 799);
  PExceptionType = ^ExceptionType;

type
  FilterType = (
    UndefinedFilter,
    PointFilter,
    BoxFilter,
    TriangleFilter,
    HermiteFilter,
    HannFilter,
    HammingFilter,
    BlackmanFilter,
    GaussianFilter,
    QuadraticFilter,
    CubicFilter,
    CatromFilter,
    MitchellFilter,
    JincFilter,
    SincFilter,
    SincFastFilter,
    KaiserFilter,
    WelchFilter,
    ParzenFilter,
    BohmanFilter,
    BartlettFilter,
    LagrangeFilter,
    LanczosFilter,
    LanczosSharpFilter,
    Lanczos2Filter,
    Lanczos2SharpFilter,
    RobidouxFilter,
    RobidouxSharpFilter,
    CosineFilter,
    SplineFilter,
    LanczosRadiusFilter,
    CubicSplineFilter,
    SentinelFilter); // a count of all the filters, not a real filter

type
  ColorspaceType = (
    UndefinedColorspace,
    CMYColorspace,           // negated linear RGB colorspace
    CMYKColorspace,          // CMY with Black separation
    GRAYColorspace,          // Single Channel greyscale (non-linear) image
    HCLColorspace,
    HCLpColorspace,
    HSBColorspace,
    HSIColorspace,
    HSLColorspace,
    HSVColorspace,           // alias for HSB
    HWBColorspace,
    LabColorspace,
    LCHColorspace,           // alias for LCHuv
    LCHabColorspace,         // Cylindrical (Polar) Lab
    LCHuvColorspace,         // Cylindrical (Polar) Luv
    LogColorspace,
    LMSColorspace,
    LuvColorspace,
    OHTAColorspace,
    Rec601YCbCrColorspace,
    Rec709YCbCrColorspace,
    RGBColorspace,           // Linear RGB colorspace
    scRGBColorspace,         // ??? */
    sRGBColorspace,          // Default: non-linear sRGB colorspace
    TransparentColorspace,
    xyYColorspace,
    XYZColorspace,           // IEEE Color Reference colorspace
    YCbCrColorspace,
    YCCColorspace,
    YDbDrColorspace,
    YIQColorspace,
    YPbPrColorspace,
    YUVColorspace,
    LinearGRAYColorspace);     // Single Channel greyscale (linear) image

type
  StorageType = (
    UndefinedPixel,
    CharPixel,
    DoublePixel,
    FloatPixel,
    LongPixel,
    LongLongPixel,
    QuantumPixel,
    ShortPixel);

type
 PaintMethod = (
  UndefinedMethod,
  PointMethod,
  ReplaceMethod,
  FloodfillMethod,
  FillToBorderMethod,
  ResetMethod);

type
  MagickBooleanType = (MagickFalse = 0,
                       MagickTrue = 1);

type
  PMagickWand = pointer;
  PPixelWand = pointer;
  PDrawingWand = pointer;

type
  TALImageMagickLibrary = class(TObject)
  private
    FlibMagickWand: THandle;
  public

    {$REGION 'https://www.imagemagick.org/api/magick-wand.php'}

    //MagickWandGenesis() initializes the MagickWand environment.
    //The format of the MagickWandGenesis method is:
    //void MagickWandGenesis(void)
    MagickWandGenesis: procedure; cdecl;

    //MagickWandTerminus() terminates the MagickWand environment.
    //The format of the MaickWandTerminus method is:
    //void MagickWandTerminus(void)
    MagickWandTerminus: procedure; cdecl;

    //NewMagickWand() returns a wand required for all other methods in the API. A fatal exception is thrown if there is not enough
    //memory to allocate the wand. Use DestroyMagickWand() to dispose of the wand when it is no longer needed.
    //The format of the NewMagickWand method is:
    //MagickWand *NewMagickWand(void)
    NewMagickWand: function: PMagickWand; cdecl;

    //DestroyMagickWand() deallocates memory associated with an MagickWand.
    //The format of the DestroyMagickWand method is:
    //MagickWand *DestroyMagickWand(MagickWand *wand)
    //A description of each parameter follows:
    //wand: the magick wand.
    DestroyMagickWand: function(wand: PMagickWand): PMagickWand; cdecl;

    //CloneMagickWand() makes an exact copy of the specified wand.
    //The format of the CloneMagickWand method is:
    //MagickWand *CloneMagickWand(const MagickWand *wand)
    //A description of each parameter follows:
    //wand: the magick wand.
    CloneMagickWand: function(const wand: PMagickWand): PMagickWand; cdecl;

    //MagickGetException() returns the severity, reason, and description of any error that occurs when using other methods in this API.
    //The format of the MagickGetException method is:
    //char *MagickGetException(const MagickWand *wand,ExceptionType *severity)
    //A description of each parameter follows:
    //wand: the magick wand.
    //severity: the severity of the error is returned here.
    MagickGetException: function(const wand: PMagickWand; severity: PExceptionType): PansiChar; cdecl;

    //MagickRelinquishMemory() relinquishes memory resources returned by such methods as MagickIdentifyImage(), MagickGetException(), etc.
    //The format of the MagickRelinquishMemory method is:
    //void *MagickRelinquishMemory(void *resource)
    //A description of each parameter follows:
    //resource: Relinquish the memory associated with this resource.
    MagickRelinquishMemory: function (resource: Pointer): Pointer; cdecl;

    //MagickResetIterator() resets the wand iterator.
    //It is typically used either before iterating though images, or before calling specific functions such as MagickAppendImages() to append all images together.
    //Afterward you can use MagickNextImage() to iterate over all the images in a wand container, starting with the first image.
    //Using this before MagickAddImages() or MagickReadImages() will cause new images to be inserted between the first and second image.
    //The format of the MagickResetIterator method is:
    //void MagickResetIterator(MagickWand *wand)
    //A description of each parameter follows:
    //wand: the magick wand.
    MagickResetIterator: procedure(wand: PMagickWand); cdecl;

    {$ENDREGION}

    {$REGION 'https://www.imagemagick.org/api/magick-image.php'}

    //MagickNextImage() sets the next image in the wand as the current image.
    //It is typically used after MagickResetIterator(), after which its first use will set the first image as the current image (unless the wand is empty).
    //It will return MagickFalse when no more images are left to be returned which happens when the wand is empty, or the current image is the last image.
    //When the above condition (end of image list) is reached, the iterator is automaticall set so that you can start using MagickPreviousImage() to
    //again iterate over the images in the reverse direction, starting with the last image (again). You can jump to this condition immeditally using MagickSetLastIterator().
    //The format of the MagickNextImage method is:
    //MagickBooleanType MagickNextImage(MagickWand *wand)
    //A description of each parameter follows:
    //wand: the magick wand.
    MagickNextImage: function(wand: PMagickWand): MagickBooleanType; cdecl;

    //MagickReadImage() reads an image or image sequence. The images are inserted jjust before the current image pointer position.
    //Use MagickSetFirstIterator(), to insert new images before all the current images in the wand, MagickSetLastIterator() to append add
    //to the end, MagickSetIteratorIndex() to place images just after the given index.
    //The format of the MagickReadImage method is:
    //MagickBooleanType MagickReadImage(MagickWand *wand,const char *filename)
    //A description of each parameter follows:
    //wand: the magick wand.
    //filename: the image filename.
    MagickReadImage: function(wand: PMagickWand; const filename: PansiChar): MagickBooleanType; cdecl;

    //MagickWriteImage() writes an image to the specified filename. If the filename parameter is NULL, the image is
    //written to the filename set by MagickReadImage() or MagickSetImageFilename().
    //The format of the MagickWriteImage method is:
    //MagickBooleanType MagickWriteImage(MagickWand *wand, const char *filename)
    //A description of each parameter follows:
    //wand: the magick wand.
    //filename: the image filename.
    MagickWriteImage: function(wand: PMagickWand; const filename: PAnsiChar): MagickBooleanType; cdecl;

    //MagickWriteImages() writes an image or image sequence.
    //The format of the MagickWriteImages method is:
    //MagickBooleanType MagickWriteImages(MagickWand *wand, const char *filename,const MagickBooleanType adjoin)
    //A description of each parameter follows:
    //wand: the magick wand.
    //filename: the image filename.
    //adjoin: join images into a single multi-image file.
    MagickWriteImages: function(wand: PMagickWand; const filename: PAnsiChar; const adjoin: MagickBooleanType): MagickBooleanType; cdecl;

    //MagickExportImagePixels() extracts pixel data from an image and returns it to you. The method returns MagickTrue on success otherwise MagickFalse
    //if an error is encountered. The data is returned as char, short int, int, ssize_t, float, or double in the order specified by map.
    //Suppose you want to extract the first scanline of a 640x480 image as character data in red-green-blue order:
    //MagickExportImagePixels(wand,0,0,640,1,"RGB",CharPixel,pixels);
    //The format of the MagickExportImagePixels method is:
    //MagickBooleanType MagickExportImagePixels(MagickWand *wand, const ssize_t x,const ssize_t y,const size_t columns, const size_t rows,const char *map,const StorageType storage, void *pixels)
    //A description of each parameter follows:
    //wand: the magick wand.
    //x, y, columns, rows: These values define the perimeter of a region of pixels you want to extract.
    //map: This string reflects the expected ordering of the pixel array. It can be any combination or order of R = red, G = green, B = blue, A = alpha (0 is transparent),
    //     O = alpha (0 is opaque), C = cyan, Y = yellow, M = magenta, K = black, I = intensity (for grayscale), P = pad.
    //storage: Define the data type of the pixels. Float and double types are expected to be normalized [0..1] otherwise [0..QuantumRange]. Choose from these types:
    //         CharPixel, DoublePixel, FloatPixel, IntegerPixel, LongPixel, QuantumPixel, or ShortPixel.
    //pixels: This array of values contain the pixel components as defined by map and type. You must preallocate this array where the expected
    //        length varies depending on the values of width, height, map, and type.
    MagickExportImagePixels: function(wand: PMagickWand; const x: ssize_t; const y: ssize_t; const columns: size_t; const rows: size_t; const map: pansichar; const storage: StorageType; pixels: pointer): MagickBooleanType; cdecl;

    //MagickReadImageBlob() reads an image or image sequence from a blob. In all other respects it is like MagickReadImage().
    //The format of the MagickReadImageBlob method is:
    //MagickBooleanType MagickReadImageBlob(MagickWand *wand, const void *blob,const size_t length)
    //A description of each parameter follows:
    //wand: the magick wand.
    //blob: the blob.
    //length: the blob length.
    MagickReadImageBlob: function(wand: PMagickWand; const blob: Pointer; const length: size_t): MagickBooleanType; cdecl;

    //MagickGetImageBlob() implements direct to memory image formats. It returns the image as a blob (a formatted "file" in memory) and its length, starting
    //from the current position in the image sequence. Use MagickSetImageFormat() to set the format to write to the blob (GIF, JPEG, PNG, etc.).
    //Utilize MagickResetIterator() to ensure the write is from the beginning of the image sequence.
    //Use MagickRelinquishMemory() to free the blob when you are done with it.
    //The format of the MagickGetImageBlob method is:
    //unsigned char *MagickGetImageBlob(MagickWand *wand,size_t *length)
    //A description of each parameter follows:
    //wand: the magick wand.
    //length: the length of the blob.
    MagickGetImageBlob: function(wand: PMagickWand; length: pSize_t): PByte; cdecl;

    //MagickSetImageFormat() sets the format of a particular image in a sequence.
    //The format of the MagickSetImageFormat method is:
    //MagickBooleanType MagickSetImageFormat(MagickWand *wand, const char *format)
    //A description of each parameter follows:
    //wand: the magick wand.
    //format: the image format.
    MagickSetImageFormat: function(wand: PMagickWand; const format: Pansichar): MagickBooleanType; cdecl;

    //MagickResizeImage() scales an image to the desired dimensions with one of these filters:
    //    Bessel   Blackman   Box
    //    Catrom   CubicGaussian
    //    Hanning  Hermite    Lanczos
    //    Mitchell PointQuandratic
    //    Sinc     Triangle
    //Most of the filters are FIR (finite impulse response), however, Bessel, Gaussian, and Sinc are IIR (infinite impulse response).
    //Bessel and Sinc are windowed (brought down to zero) with the Blackman filter.
    //The format of the MagickResizeImage method is:
    //MagickBooleanType MagickResizeImage(MagickWand *wand, const size_t columns,const size_t rows,const FilterType filter)
    //A description of each parameter follows:
    //wand: the magick wand.
    //columns: the number of columns in the scaled image.
    //rows: the number of rows in the scaled image.
    //filter: Image filter to use.
    MagickResizeImage: function(wand: PMagickWand; const columns: size_t; const rows: size_t; const filter: FilterType): MagickBooleanType; cdecl;

    //MagickCropImage() extracts a region of the image.
    //The format of the MagickCropImage method is:
    //MagickBooleanType MagickCropImage(MagickWand *wand, const size_t width,const size_t height,const ssize_t x,const ssize_t y)
    //A description of each parameter follows:
    //wand: the magick wand.
    //width: the region width.
    //height: the region height.
    //x: the region x-offset.
    //y: the region y-offset.
    MagickCropImage: function(wand: PMagickWand; const width: size_t; const height: size_t; const x: ssize_t; const y: ssize_t): MagickBooleanType; cdecl;

    //MagickGetImageColorspace() gets the image colorspace.
    //The format of the MagickGetImageColorspace method is:
    //ColorspaceType MagickGetImageColorspace(MagickWand *wand)
    //A description of each parameter follows:
    //wand: the magick wand.
    MagickGetImageColorspace: function(wand: PMagickWand): ColorspaceType; cdecl;

    //MagickSetImageColorspace() sets the image colorspace. But does not modify the image data.
    //The format of the MagickSetImageColorspace method is:
    //MagickBooleanType MagickSetImageColorspace(MagickWand *wand, const ColorspaceType colorspace)
    //A description of each parameter follows:
    //wand: the magick wand.
    //colorspace: the image colorspace: UndefinedColorspace, RGBColorspace, GRAYColorspace, TransparentColorspace, OHTAColorspace, XYZColorspace, YCbCrColorspace,
    //                                  YCCColorspace, YIQColorspace, YPbPrColorspace, YPbPrColorspace, YUVColorspace, CMYKColorspace, sRGBColorspace, HSLColorspace, or HWBColorspace.
    MagickSetImageColorspace: function(wand: PMagickWand; const colorspace: ColorspaceType): MagickBooleanType; cdecl;

    //MagickTransformImageColorspace() transform the image colorspace, setting the images colorspace while transforming the images data to that colorspace.
    //The format of the MagickTransformImageColorspace method is:
    //MagickBooleanType MagickTransformImageColorspace(MagickWand *wand, const ColorspaceType colorspace)
    //A description of each parameter follows:
    //wand: the magick wand.
    //colorspace: the image colorspace: UndefinedColorspace, sRGBColorspace, RGBColorspace, GRAYColorspace, OHTAColorspace, XYZColorspace, YCbCrColorspace,
    //                                  YCCColorspace, YIQColorspace, YPbPrColorspace, YPbPrColorspace, YUVColorspace, CMYKColorspace, HSLColorspace, HWBColorspace.
    MagickTransformImageColorspace: function(wand: PMagickWand;const colorspace: ColorspaceType): MagickBooleanType; cdecl;

    //MagickGetImageWidth() returns the image width.
    //The format of the MagickGetImageWidth method is:
    //size_t MagickGetImageWidth(MagickWand *wand)
    //A description of each parameter follows:
    //wand: the magick wand.
    MagickGetImageWidth: function(wand: PMagickWand): size_t; cdecl;

    //MagickGetImageHeight() returns the image height.
    //The format of the MagickGetImageHeight method is:
    //size_t MagickGetImageHeight(MagickWand *wand)
    //A description of each parameter follows:
    //wand: the magick wand.
    MagickGetImageHeight: function(wand: PMagickWand): size_t; cdecl;

    //MagickGetImageCompressionQuality() gets the image compression quality.
    //The format of the MagickGetImageCompressionQuality method is:
    //size_t MagickGetImageCompressionQuality(MagickWand *wand)
    //A description of each parameter follows:
    //wand: the magick wand.
    MagickGetImageCompressionQuality: function(wand: PMagickWand): size_t; cdecl;

    //MagickSetImageCompressionQuality() sets the image compression quality.
    //The format of the MagickSetImageCompressionQuality method is:
    //MagickBooleanType MagickSetImageCompressionQuality(MagickWand *wand, const size_t quality)
    //A description of each parameter follows:
    //wand: the magick wand.
    //quality: the image compression tlityype.
    MagickSetImageCompressionQuality: function(wand: PMagickWand; const quality: size_t): MagickBooleanType; cdecl;

    //MagickAutoOrientImage() adjusts an image so that its orientation is suitable $ for viewing (i.e. top-left orientation).
    //The format of the MagickAutoOrientImage method is:
    //MagickBooleanType MagickAutoOrientImage(MagickWand *image)
    //A description of each parameter follows:
    //wand: the magick wand.
    MagickAutoOrientImage: function(wand: PMagickWand): MagickBooleanType; cdecl;

    //MagickDrawImage() renders the drawing wand on the current image.
    //The format of the MagickDrawImage method is:
    //MagickBooleanType MagickDrawImage(MagickWand *wand, const DrawingWand *drawing_wand)
    //A description of each parameter follows:
    //wand: the magick wand.
    //drawing_wand: the draw wand.
    MagickDrawImage: function(wand: PMagickWand; const drawing_wand: PDrawingWand): MagickBooleanType; cdecl;

    {$ENDREGION}

    {$REGION 'https://www.imagemagick.org/api/magick-property.php'}

    //MagickGetImageProperty() returns a value associated with the specified property. Use MagickRelinquishMemory() to free the value when you are finished with it.
    //The format of the MagickGetImageProperty method is:
    //char *MagickGetImageProperty(MagickWand *wand,const char *property)
    //A description of each parameter follows:
    //wand: the magick wand.
    //property: the property.
    MagickGetImageProperty: function(wand: PMagickWand; const &property: Pansichar): pansiChar; cdecl;

    //MagickGetImageProperties() returns all the property names that match the specified pattern associated with a wand. Use MagickGetImageProperty() to return the
    //value of a particular property. Use MagickRelinquishMemory() to free the value when you are finished with it.
    //The format of the MagickGetImageProperties method is:
    //char *MagickGetImageProperties(MagickWand *wand, const char *pattern,size_t *number_properties)
    //A description of each parameter follows:
    //wand: the magick wand.
    //pattern: Specifies a pointer to a text string containing a pattern.
    //number_properties: the number properties associated with this wand.
    MagickGetImageProperties: function(wand: PMagickWand; const pattern: PansiChar; number_properties: Psize_t): ppansiChar; cdecl;

    //MagickGetColorspace() gets the wand colorspace type.
    //The format of the MagickGetColorspace method is:
    //ColorspaceType MagickGetColorspace(MagickWand *wand)
    //A description of each parameter follows:
    //wand: the magick wand.
    MagickGetColorspace: function(wand: PMagickWand): ColorspaceType; cdecl;

    //MagickSetColorspace() sets the wand colorspace type.
    //The format of the MagickSetColorspace method is:
    //MagickBooleanType MagickSetColorspace(MagickWand *wand, const ColorspaceType colorspace)
    //A description of each parameter follows:
    //wand: the magick wand.
    //colorspace: the wand colorspace.
    MagickSetColorspace: function(wand: PMagickWand; const colorspace: ColorspaceType): MagickBooleanType; cdecl;

    //MagickGetImageProfile() returns the named image profile.
    //The format of the MagickGetImageProfile method is:
    //unsigned char *MagickGetImageProfile(MagickWand *wand,const char *name, size_t *length)
    //A description of each parameter follows:
    //wand: the magick wand.
    //name: Name of profile to return: ICC, IPTC, or generic profile.
    //length: the length of the profile.
    MagickGetImageProfile: function(wand: PMagickWand; const name: PansiChar; length: PSize_t): PByte; cdecl;

    //MagickGetImageProfiles() returns all the profile names that match the specified pattern associated with a wand.
    //Use MagickGetImageProfile() to return the value of a particular property. Use MagickRelinquishMemory() to free the value when you are finished with it.
    //The format of the MagickGetImageProfiles method is:
    //char *MagickGetImageProfiles(MagickWand *wand,const char *pattern, size_t *number_profiles)
    //A description of each parameter follows:
    //wand: the magick wand.
    //pattern: Specifies a pointer to a text string containing a pattern.
    //number_profiles: the number profiles associated with this wand.
    MagickGetImageProfiles: function(wand: PMagickWand; const pattern: pAnsiChar; const number_profiles: pSize_t): ppAnsiChar; cdecl; // https://stackoverflow.com/questions/48960702/how-to-convert-char-to-delphi

    //MagickSetImageProfile() adds a named profile to the magick wand. If a profile with the same name already exists,
    //it is replaced. This method differs from the MagickProfileImage() method in that it does not apply any CMS color profiles.
    //The format of the MagickSetImageProfile method is:
    //MagickBooleanType MagickSetImageProfile(MagickWand *wand, const char *name,const void *profile,const size_t length)
    //A description of each parameter follows:
    //wand: the magick wand.
    //name: Name of profile to add or remove: ICC, IPTC, or generic profile.
    //profile: the profile.
    //length: the length of the profile.
    MagickSetImageProfile: function(wand: PMagickWand; const name: PansiChar; const profile: Pointer; const length: size_t): MagickBooleanType; cdecl;

    //MagickProfileImage() adds or removes a ICC, IPTC, or generic profile from an image. If the profile is NULL, it is removed
    //from the image otherwise added. Use a name of '*' and a profile of NULL to remove all profiles from the image.
    //The format of the MagickProfileImage method is:
    //MagickBooleanType MagickProfileImage(MagickWand *wand,const char *name, const void *profile,const size_t length)
    //A description of each parameter follows:
    //wand: the magick wand.
    //name: Name of profile to add or remove: ICC, IPTC, or generic profile.
    //profile: the profile.
    //length: the length of the profile.
    MagickProfileImage: function(wand: PMagickWand; const name: PansiChar; const profile: Pointer; const length: size_t): MagickBooleanType; cdecl;

    //MagickRemoveImageProfile() removes the named image profile and returns it.
    //The format of the MagickRemoveImageProfile method is:
    //unsigned char *MagickRemoveImageProfile(MagickWand *wand, const char *name,size_t *length)
    //A description of each parameter follows:
    //wand: the magick wand.
    //name: Name of profile to return: ICC, IPTC, or generic profile.
    //length: the length of the profile.
    MagickRemoveImageProfile: function(wand: PMagickWand; const name: PansiChar; length: pSize_t): PByte; cdecl;

    {$ENDREGION}

    {$REGION 'https://www.imagemagick.org/api/pixel-wand.php'}

    //NewPixelWand() returns a new pixel wand.
    //The format of the NewPixelWand method is:
    //PixelWand *NewPixelWand(void)
    NewPixelWand: function: PPixelWand; cdecl;

    //DestroyPixelWand() deallocates resources associated with a PixelWand.
    //The format of the DestroyPixelWand method is:
    //PixelWand *DestroyPixelWand(PixelWand *wand)
    //A description of each parameter follows:
    //wand: the pixel wand.
    DestroyPixelWand: function(wand: PPixelWand): PPixelWand; cdecl;

    //PixelGetException() returns the severity, reason, and description of any error that occurs when using other methods in this API.
    //The format of the PixelGetException method is:
    //char *PixelGetException(const PixelWand *wand,ExceptionType *severity)
    //A description of each parameter follows:
    //wand: the pixel wand.
    //severity: the severity of the error is returned here.
    PixelGetException: function(const wand: PPixelWand; severity: PExceptionType): PansiChar; cdecl;

    //PixelSetColor() sets the color of the pixel wand with a string (e.g. "blue", "#0000ff", "rgb(0,0,255)", "cmyk(100,100,100,10)", etc.).
    //The format of the PixelSetColor method is:
    //MagickBooleanType PixelSetColor(PixelWand *wand,const char *color)
    //A description of each parameter follows:
    //wand: the pixel wand.
    //color: the pixel wand color.
    PixelSetColor: function(wand: PPixelWand; const color: PansiChar): MagickBooleanType; cdecl;

    //PixelSetAlpha() sets the normalized alpha value of the pixel wand.
    //The format of the PixelSetAlpha method is:
    //void PixelSetAlpha(PixelWand *wand,const double alpha)
    //A description of each parameter follows:
    //wand: the pixel wand.
    //alpha: the level of transparency: 1.0 is fully opaque and 0.0 is fully transparent.
    PixelSetAlpha: procedure(wand: PPixelWand; const alpha: Double); cdecl;

    //PixelSetBlack
    //PixelSetBlack() sets the normalized black color of the pixel wand.
    //The format of the PixelSetBlack method is:
    //void PixelSetBlack(PixelWand *wand,const double black)
    //A description of each parameter follows:
    //wand: the pixel wand.
    //black: the black color.
    PixelSetBlack: procedure(wand: PPixelWand; const black: Double); cdecl;

    //PixelSetBlue() sets the normalized blue color of the pixel wand.
    //The format of the PixelSetBlue method is:
    //void PixelSetBlue(PixelWand *wand,const double blue)
    //A description of each parameter follows:
    //wand: the pixel wand.
    //blue: the blue color.
    PixelSetBlue: procedure(wand: PPixelWand; const blue: Double); cdecl;

    //PixelSetRed() sets the normalized red color of the pixel wand.
    //The format of the PixelSetRed method is:
    //void PixelSetRed(PixelWand *wand,const double red)
    //A description of each parameter follows:
    //wand: the pixel wand.
    //red: the red color.
    PixelSetRed: procedure(wand: PPixelWand; const red: Double); cdecl;

    //PixelSetGreen() sets the normalized green color of the pixel wand.
    //The format of the PixelSetGreen method is:
    //void PixelSetGreen(PixelWand *wand,const double green)
    //A description of each parameter follows:
    //wand: the pixel wand.
    //green: the green color.
    PixelSetGreen: procedure(wand: PPixelWand; const green: Double); cdecl;

    {$ENDREGION}

    {$REGION 'https://www.imagemagick.org/api/drawing-wand.php'}

    //NewDrawingWand() returns a drawing wand required for all other methods in the API.
    //The format of the NewDrawingWand method is:
    //DrawingWand *NewDrawingWand(void)
    NewDrawingWand: function: PDrawingWand; cdecl;

    //DestroyDrawingWand() frees all resources associated with the drawing wand. Once the drawing wand has been freed, it should not be used and further unless it re-allocated.
    //The format of the DestroyDrawingWand method is:
    //DrawingWand *DestroyDrawingWand(DrawingWand *wand)
    //A description of each parameter follows:
    //wand: the drawing wand to destroy.
    DestroyDrawingWand: function(wand: PDrawingWand): PDrawingWand; cdecl;

    //DrawGetException() returns the severity, reason, and description of any error that occurs when using other methods in this API.
    //The format of the DrawGetException method is:
    //char *DrawGetException(const DrawWand *wand, ExceptionType *severity)
    //A description of each parameter follows:
    //wand: the drawing wand.
    //severity: the severity of the error is returned here.
    DrawGetException: function(const wand: PDrawingWand; severity: PExceptionType): PansiChar; cdecl;

    //DrawSetFillColor() sets the fill color to be used for drawing filled objects.
    //The format of the DrawSetFillColor method is:
    //void DrawSetFillColor(DrawingWand *wand,const PixelWand *fill_wand)
    //A description of each parameter follows:
    //wand: the drawing wand.
    //fill_wand: fill wand.
    DrawSetFillColor: procedure(wand: PDrawingWand; const fill_wand: PPixelWand); cdecl;

    //DrawSetFillOpacity() sets the alpha to use when drawing using the fill color or fill texture. Fully opaque is 1.0.
    //The format of the DrawSetFillOpacity method is:
    //void DrawSetFillOpacity(DrawingWand *wand,const double fill_alpha)
    //A description of each parameter follows:
    //wand: the drawing wand.
    //fill_opacity: fill opacity
    DrawSetFillOpacity: procedure(wand: PDrawingWand; const fill_alpha: double); cdecl;

    //DrawSetOpacity() sets the alpha to use when drawing using the fill or stroke color or texture. Fully opaque is 1.0.
    //The format of the DrawSetOpacity method is:
    //void DrawSetOpacity(DrawingWand *wand,const double alpha)
    //A description of each parameter follows:
    //wand: the drawing wand.
    //opacity: fill and stroke opacity. The value 1.0 is opaque.
    DrawSetOpacity: procedure(wand: PDrawingWand; const alpha: double); cdecl;

    //DrawAlpha() paints on the image's alpha channel in order to set effected pixels to transparent. The available paint methods are:
    //    PointMethod: Select the target pixel
    //    ReplaceMethod: Select any pixel that matches the target pixel.
    //    FloodfillMethod: Select the target pixel and matching neighbors.
    //    FillToBorderMethod: Select the target pixel and neighbors not matching
    //border color.
    //    ResetMethod: Select all pixels.
    //The format of the DrawAlpha method is:
    //void DrawAlpha(DrawingWand *wand,const double x,const double y, const PaintMethod paint_method)
    //A description of each parameter follows:
    //wand: the drawing wand.
    //x: x ordinate
    //y: y ordinate
    //paint_method: paint method.
    DrawAlpha: procedure(wand: PDrawingWand; const x, y: double; const paint_method: PaintMethod); cdecl;

    //DrawPoint() draws a point using the current fill color.
    //The format of the DrawPoint method is:
    //void DrawPoint(DrawingWand *wand,const double x,const double y)
    //A description of each parameter follows:
    //wand: the drawing wand.
    //x: target x coordinate
    //y: target y coordinate
    DrawPoint: procedure(wand: PDrawingWand; const x, y: double); cdecl;

    //DrawLine() draws a line on the image using the current stroke color, stroke alpha, and stroke width.
    //The format of the DrawLine method is:
    //void DrawLine(DrawingWand *wand,const double sx,const double sy, const double ex,const double ey)
    //A description of each parameter follows:
    //wand: the drawing wand.
    //sx: starting x ordinate
    //sy: starting y ordinate
    //ex: ending x ordinate
    //ey: ending y ordinate
    DrawLine: procedure(wand: PDrawingWand; const sx, sy, ex, ey: double); cdecl;

    {$ENDREGION}

  public
    constructor Create(aImageMagickHome: String;
                       const aThreadLimit: integer = -1); virtual;
    destructor Destroy; override;
  end;

Var
  ALImageMagickLib: TALImageMagickLibrary;

{*******************************************************************}
procedure alCreateImageMagickLibrary(const aImageMagickHome: String;
                                     const aThreadLimit: integer = -1);
procedure alFreeImageMagickLibrary;
procedure RaiseLastMagickWandError(const wand: PMagickWand);
procedure RaiseLastPixelWandError(const wand: PPixelWand);
procedure RaiseLastDrawingWandError(const wand: PDrawingWand);

implementation

uses system.sysutils,
     alCommon;

{****************************************************************}
constructor TALImageMagickLibrary.Create(aImageMagickHome: String;
                                         const aThreadLimit: integer = -1);
Var LPath: String;
begin

  // http://www.imagemagick.org/script/resources.php
  aImageMagickHome := ExcludeTrailingPathDelimiter(aImageMagickHome);
  LPath := getEnvironmentVariable('PATH');
  if ((pos(aImageMagickHome, LPath) <= 0) and
      (not setEnvironmentVariable(PChar('PATH'), pChar(LPath + ';' + aImageMagickHome)))) or
     (not setEnvironmentVariable(PChar('MAGICK_HOME'), pChar(aImageMagickHome))) or
     (not setEnvironmentVariable(PChar('MAGICK_CONFIGURE_PATH'), pChar(aImageMagickHome))) or
     (not setEnvironmentVariable(PChar('MAGICK_CODER_FILTER_PATH'), pChar(aImageMagickHome + '\modules\filters'))) or
     (not setEnvironmentVariable(PChar('MAGICK_CODER_MODULE_PATH'), pChar(aImageMagickHome + '\modules\coders'))) then raiseLastOsError;

  //https://www.imagemagick.org/discourse-server/viewtopic.php?f=6&t=33662
  //https://stackoverflow.com/questions/49266246/imagemagick-wand-use-only-one-single-cpu
  if (aThreadLimit > -1) and
     (not setEnvironmentVariable(PChar('MAGICK_THREAD_LIMIT'), pChar(Inttostr(aThreadLimit)))) then raiseLastOsError;

  FlibMagickWand := LoadLibrary(pChar(aImageMagickHome + '\CORE_RL_MagickWand_.dll' ));
  if FlibMagickWand = 0 then raiseLastOsError;

  {$REGION 'https://www.imagemagick.org/api/magick-wand.php'}
  MagickWandGenesis := GetProcAddress(FlibMagickWand,'MagickWandGenesis');
  MagickWandTerminus := GetProcAddress(FlibMagickWand,'MagickWandTerminus');
  NewMagickWand := GetProcAddress(FlibMagickWand,'NewMagickWand');
  DestroyMagickWand := GetProcAddress(FlibMagickWand,'DestroyMagickWand');
  CloneMagickWand := GetProcAddress(FlibMagickWand,'CloneMagickWand');
  MagickGetException := GetProcAddress(FlibMagickWand,'MagickGetException');
  MagickRelinquishMemory := GetProcAddress(FlibMagickWand,'MagickRelinquishMemory');
  MagickResetIterator := GetProcAddress(FlibMagickWand,'MagickResetIterator');
  {$ENDREGION}

  {$REGION 'https://www.imagemagick.org/api/magick-image.php'}
  MagickNextImage := GetProcAddress(FlibMagickWand,'MagickNextImage');
  MagickReadImage := GetProcAddress(FlibMagickWand,'MagickReadImage');
  MagickWriteImage := GetProcAddress(FlibMagickWand,'MagickWriteImage');
  MagickWriteImages := GetProcAddress(FlibMagickWand,'MagickWriteImages');
  MagickExportImagePixels := GetProcAddress(FlibMagickWand,'MagickExportImagePixels');
  MagickReadImageBlob := GetProcAddress(FlibMagickWand,'MagickReadImageBlob');
  MagickGetImageBlob := GetProcAddress(FlibMagickWand,'MagickGetImageBlob');
  MagickSetImageFormat := GetProcAddress(FlibMagickWand,'MagickSetImageFormat');
  MagickResizeImage := GetProcAddress(FlibMagickWand,'MagickResizeImage');
  MagickCropImage := GetProcAddress(FlibMagickWand,'MagickCropImage');
  MagickGetImageColorspace := GetProcAddress(FlibMagickWand,'MagickGetImageColorspace');
  MagickSetImageColorspace := GetProcAddress(FlibMagickWand,'MagickSetImageColorspace');
  MagickTransformImageColorspace := GetProcAddress(FlibMagickWand,'MagickTransformImageColorspace');
  MagickGetImageWidth := GetProcAddress(FlibMagickWand,'MagickGetImageWidth');
  MagickGetImageHeight := GetProcAddress(FlibMagickWand,'MagickGetImageHeight');
  MagickGetImageCompressionQuality := GetProcAddress(FlibMagickWand,'MagickGetImageCompressionQuality');
  MagickSetImageCompressionQuality := GetProcAddress(FlibMagickWand,'MagickSetImageCompressionQuality');
  MagickAutoOrientImage := GetProcAddress(FlibMagickWand,'MagickAutoOrientImage');
  MagickDrawImage := GetProcAddress(FlibMagickWand,'MagickDrawImage');
  {$ENDREGION}

  {$REGION 'https://www.imagemagick.org/api/magick-property.php'}
  MagickGetImageProperty := GetProcAddress(FlibMagickWand,'MagickGetImageProperty');
  MagickGetImageProperties := GetProcAddress(FlibMagickWand,'MagickGetImageProperties');
  MagickGetColorspace := GetProcAddress(FlibMagickWand,'MagickGetColorspace');
  MagickSetColorspace := GetProcAddress(FlibMagickWand,'MagickSetColorspace');
  MagickGetImageProfile := GetProcAddress(FlibMagickWand,'MagickGetImageProfile');
  MagickGetImageProfiles := GetProcAddress(FlibMagickWand,'MagickGetImageProfiles');
  MagickSetImageProfile := GetProcAddress(FlibMagickWand,'MagickSetImageProfile');
  MagickProfileImage := GetProcAddress(FlibMagickWand,'MagickProfileImage');
  MagickRemoveImageProfile := GetProcAddress(FlibMagickWand,'MagickRemoveImageProfile');
  {$ENDREGION}

  {$REGION 'https://www.imagemagick.org/api/pixel-wand.php'}
  NewPixelWand := GetProcAddress(FlibMagickWand,'NewPixelWand');
  DestroyPixelWand := GetProcAddress(FlibMagickWand,'DestroyPixelWand');
  PixelGetException := GetProcAddress(FlibMagickWand,'PixelGetException');
  PixelSetColor := GetProcAddress(FlibMagickWand,'PixelSetColor');
  PixelSetAlpha := GetProcAddress(FlibMagickWand,'PixelSetAlpha');
  PixelSetBlack := GetProcAddress(FlibMagickWand,'PixelSetBlack');
  PixelSetBlue := GetProcAddress(FlibMagickWand,'PixelSetBlue');
  PixelSetRed := GetProcAddress(FlibMagickWand,'PixelSetRed');
  PixelSetGreen := GetProcAddress(FlibMagickWand,'PixelSetGreen');
  {$ENDREGION}

  {$REGION 'https://www.imagemagick.org/api/drawing-wand.php'}
  NewDrawingWand := GetProcAddress(FlibMagickWand,'NewDrawingWand');
  DestroyDrawingWand := GetProcAddress(FlibMagickWand,'DestroyDrawingWand');
  DrawGetException := GetProcAddress(FlibMagickWand,'DrawGetException');
  DrawSetFillColor := GetProcAddress(FlibMagickWand,'DrawSetFillColor');
  DrawSetFillOpacity := GetProcAddress(FlibMagickWand,'DrawSetFillOpacity');
  DrawSetOpacity := GetProcAddress(FlibMagickWand,'DrawSetOpacity');
  DrawAlpha := GetProcAddress(FlibMagickWand,'DrawAlpha');
  DrawPoint := GetProcAddress(FlibMagickWand,'DrawPoint');
  DrawLine := GetProcAddress(FlibMagickWand,'DrawLine');
  {$ENDREGION}

  MagickWandGenesis;

end;

{***************************************}
destructor TALImageMagickLibrary.Destroy;
begin
  if FlibMagickWand > 0 then begin
    MagickWandTerminus;
    sleep(250); // << else when the imagemagick was heavy used just before the destroy
                // << I receive an Access Violation in the FreeLibrary(FlibMagickWand)
    if not FreeLibrary(FlibMagickWand) then raiseLastOsError;
  end;
  inherited Destroy;
end;

{**********************************************************}
procedure RaiseLastMagickWandError(const wand: PMagickWand);
var LPAnsiChar: PansiChar;
    LDescription: ansiString;
    LSeverity: ExceptionType;
begin
  LPAnsiChar := ALImageMagickLib.MagickGetException(wand, @LSeverity);
  LDescription := LPAnsiChar;
  ALImageMagickLib.MagickRelinquishMemory(LPAnsiChar);
  raise Exception.create(string(LDescription));
end;

{********************************************************}
procedure RaiseLastPixelWandError(const wand: PPixelWand);
var LPAnsiChar: PansiChar;
    LDescription: ansiString;
    LSeverity: ExceptionType;
begin
  LPAnsiChar := ALImageMagickLib.PixelGetException(wand, @LSeverity);
  LDescription := LPAnsiChar;
  ALImageMagickLib.MagickRelinquishMemory(LPAnsiChar);
  raise Exception.create(string(LDescription));
end;

{************************************************************}
procedure RaiseLastDrawingWandError(const wand: PDrawingWand);
var LPAnsiChar: PansiChar;
    LDescription: ansiString;
    LSeverity: ExceptionType;
begin
  LPAnsiChar := ALImageMagickLib.DrawGetException(wand, @LSeverity);
  LDescription := LPAnsiChar;
  ALImageMagickLib.MagickRelinquishMemory(LPAnsiChar);
  raise Exception.create(string(LDescription));
end;

{******************************************************************}
procedure alCreateImageMagickLibrary(const aImageMagickHome: String;
                                     const aThreadLimit: integer = -1);
begin
  if assigned(ALImageMagickLib) then exit;
  ALImageMagickLib := TALImageMagickLibrary.Create(aImageMagickHome, aThreadLimit);
end;

{*********************************}
procedure alFreeImageMagickLibrary;
begin
  alFreeAndNil(ALImageMagickLib);
end;

initialization
  ALImageMagickLib := nil;

end.
