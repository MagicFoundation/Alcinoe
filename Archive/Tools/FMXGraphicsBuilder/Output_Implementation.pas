{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoToSkSurface(const AImage: sk_image_t; const W, H: single): sk_surface_t;
begin
  var LSrcRect := TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage));
  var LDestRect := LSrcRect.
                     FitInto(
                       TrectF.Create(0, 0, W, H));
  Result := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(AImage, LDestRect.Width, LDestRect.Height);
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoToSkSurface(const AStream: TStream; const W, H: single): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoToSkSurface(LImage, W, H);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoToSkSurface(const AResName: String; const W, H: single): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoToSkSurface(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoToSkSurface(const AFileName: String; const W, H: single): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoToSkSurface(LImage, W, H);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoToSkImage(const AStream: TStream; const W, H: single): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoToSkSurface(LImage, W, H);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoToSkImage(const AResName: String; const W, H: single): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoToSkImage(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoToSkImage(const AFileName: String; const W, H: single): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoToSkSurface(LImage, W, H);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoToJBitmap(const ABitmap: JBitmap; const W, H: single): JBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight);
  var LDestRect := LSrcRect.
                     FitInto(
                       TrectF.Create(0, 0, W, H));
  Result := ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(ABitmap, LDestRect.Width, LDestRect.Height);
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoToJBitmap(const AStream: TStream; const W, H: single): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoToJBitmap(LBitmap, W, H);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoToJBitmap(const AResName: String; const W, H: single): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoToJBitmap(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoToJBitmap(const AFileName: String; const W, H: single): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoToJBitmap(LBitmap, W, H);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoToCGContextRef(const AImage: ALOSImage; const W, H: single): CGContextRef;
begin
  var LSrcRect := TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage));
  var LDestRect := LSrcRect.
                     FitInto(
                       TrectF.Create(0, 0, W, H));
  result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(AImage, LDestRect.Width, LDestRect.Height);
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoToCGContextRef(const AStream: TStream; const W, H: single): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoToCGContextRef(LImage, W, H);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoToCGContextRef(const AResName: String; const W, H: single): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoToCGContextRef(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoToCGContextRef(const AFileName: String; const W, H: single): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoToCGContextRef(LImage, W, H);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoToCGImageRef(const AStream: TStream; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoToCGContextRef(AStream, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoToCGImageRef(const AResName: String; const W, H: single): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoToCGImageRef(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoToCGImageRef(const AFileName: String; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoToCGContextRef(AFileName, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{***********************************************************************************************}
function ALLoadFromBitmapAndFitIntoToBitmap(const ABitmap: TBitmap; const W, H: single): TBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.width, ABitmap.height);
  var LDestRect := LSrcRect.
                     FitInto(
                       TrectF.Create(0, 0, W, H));
  Result := ALLoadFromBitmapAndFitIntoAndCropToBitmap(ABitmap, LDestRect.Width, LDestRect.Height);
end;

{***********************************************************************************************}
function ALLoadFromStreamAndFitIntoToBitmap(const AStream: TStream; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoToBitmap(LBitmap, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*************************************************************************************************}
function ALLoadFromResourceAndFitIntoToBitmap(const AResName: String; const W, H: single): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoToBitmap(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**********************************************************************************************}
function ALLoadFromFileAndFitIntoToBitmap(const AFileName: String; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoToBitmap(LBitmap, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*****************************************************************************************************}
function ALLoadFromStreamAndFitIntoToDrawable(const AStream: TStream; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoToSkImage(AStream, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoToSkSurface(AStream, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoToSkSurface(AStream, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoToJBitmap(AStream, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoToCGContextRef(AStream, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoToBitmap(AStream, W, H);
  {$ENDIF}
end;

{*******************************************************************************************************}
function ALLoadFromResourceAndFitIntoToDrawable(const AResName: String; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoToSkImage(AResName, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoToSkSurface(AResName, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoToSkSurface(AResName, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoToJBitmap(AResName, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoToCGContextRef(AResName, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoToBitmap(AResName, W, H);
  {$ENDIF}
end;

{****************************************************************************************************}
function ALLoadFromFileAndFitIntoToDrawable(const AFileName: String; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoToSkImage(AFileName, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoToSkSurface(AFileName, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoToSkSurface(AFileName, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoToJBitmap(AFileName, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoToCGContextRef(AFileName, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoToBitmap(AFileName, W, H);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropToSkSurface(const AImage: sk_image_t; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
    sk4d_canvas_draw_image_rect(
      LCanvas, // self: sk_canvas_t;
      AImage, // const image: sk_image_t;
      @LSrcRect, // const src: psk_rect_t;
      @LDestRectF,  // const dest: psk_rect_t;
      @LSamplingoptions, // const sampling: psk_samplingoptions_t;
      LPaint, // const paint: sk_paint_t;
      FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropToSkSurface(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(LImage, W, H, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropToSkSurface(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToSkSurface(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropToSkSurface(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(LImage, W, H, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropToSkImage(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(LImage, W, H, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropToSkImage(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToSkImage(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropToSkImage(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(LImage, W, H, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(const ABitmap: JBitmap; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight), TpointF.create(XCropCenter, YCropCenter)).Round;

  var LMatrix := TJMatrix.JavaClass.init;
  LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
  result := TJBitmap.JavaClass.createBitmap(ABitmap{src}, LSrcRect.Left{X}, LSrcRect.top{Y}, LSrcRect.width{Width}, LSrcRect.height{height}, LMatrix{m}, True{filter});
  LMatrix := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropToJBitmap(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropToJBitmap(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToJBitmap(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropToJBitmap(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(const AImage: ALOSImage; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LRatio: single;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage)), TpointF.create(XCropCenter, YCropCenter), LRatio);
  //-----
  Result := ALCreateCGContextRef(LDestRect.Width, LDestRect.Height);
  CGContextDrawImage(
    Result, // c: The graphics context in which to draw the image.
    ALLowerLeftCGRect(
      TpointF.Create(
        0-(LSrcRect.Left*LRatio),
        0-(LSrcRect.top*LRatio)),
      LDestRect.width + (LSrcRect.Left*LRatio) + ((ALOSImageGetWidth(AImage)-LSrcRect.right)*LRatio),
      LDestRect.height + (LSrcRect.top*LRatio)  + ((ALOSImageGetHeight(AImage)-LSrcRect.bottom)*LRatio),
      LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropToCGContextRef(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(LImage, W, H, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropToCGContextRef(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCGContextRef(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropToCGContextRef(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(LImage, W, H, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropToCGImageRef(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropToCGContextRef(AStream, W, H, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropToCGImageRef(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCGImageRef(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropToCGImageRef(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropToCGContextRef(AFileName, W, H, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropToBitmap(const ABitmap: TBitmap; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.Width, ABitmap.height), TpointF.create(XCropCenter, YCropCenter));

  Result := TBitmap.Create(LDestRect.Width,LDestRect.Height);
  try

    if Result.Canvas.BeginScene then
    try
      Result.Canvas.DrawBitmap(
        ABitmap, // const ABitmap: TBitmap;
        LSrcRect, //const SrcRect,
        LDestRect, //const DstRect: TRectF;
        1, //const AOpacity: Single;
        false); // const HighSpeed: Boolean => disable interpolation
    finally
      Result.Canvas.EndScene;
    end;

  except
    AlFreeAndNil(Result);
    raise;
  end;
end;

{************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropToBitmap(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropToBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{**************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropToBitmap(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToBitmap(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{***********************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropToBitmap(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropToBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{******************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropToDrawable(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropToSkImage(AStream, W, H, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropToSkSurface(AStream, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropToSkSurface(AStream, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropToJBitmap(AStream, W, H, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropToCGContextRef(AStream, W, H, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropToBitmap(AStream, W, H, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{********************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropToDrawable(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropToSkImage(AResName, W, H, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropToSkSurface(AResName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropToSkSurface(AResName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropToJBitmap(AResName, W, H, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropToCGContextRef(AResName, W, H, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropToBitmap(AResName, W, H, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{*****************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropToDrawable(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropToSkImage(AFileName, W, H, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropToSkSurface(AFileName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropToSkSurface(AFileName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropToJBitmap(AFileName, W, H, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropToCGContextRef(AFileName, W, H, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropToBitmap(AFileName, W, H, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropToRoundRectSkSurface(const AImage: sk_image_t; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LRRect :=  ALSkCheckHandle(sk4d_rrect_create);
    try
      sk4d_rrect_set_rect3(
        LRRect, // self: sk_rrect_t;
        @LDestRectF, // const rect: psk_rect_t;
        XRadius, // radius_x,
        YRadius); // radius_y: float)

      sk4d_canvas_clip_rrect(
        LCanvas, // self: sk_canvas_t;
        LRRect, // const rrect: sk_rrect_t;
        sk_clipop_t.INTERSECT_SK_CLIPOP, // op: sk_clipop_t;
        true); // anti_alias: _bool);

      var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
      sk4d_canvas_draw_image_rect(
        LCanvas, // self: sk_canvas_t;
        AImage, // const image: sk_image_t;
        @LSrcRect, // const src: psk_rect_t;
        @LDestRectF,  // const dest: psk_rect_t;
        @LSamplingoptions, // const sampling: psk_samplingoptions_t;
        LPaint, // const paint: sk_paint_t;
        FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)

      sk4d_canvas_restore(LCanvas);
    finally
      sk4d_rrect_destroy(LRRect);
    end;
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectSkSurface(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropToRoundRectSkSurface(LImage, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectSkSurface(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToRoundRectSkSurface(LStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropToRoundRectSkSurface(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropToRoundRectSkSurface(LImage, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectSkImage(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropToRoundRectSkSurface(LImage, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectSkImage(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToRoundRectSkImage(LStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropToRoundRectSkImage(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropToRoundRectSkSurface(LImage, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropToRoundRectJBitmap(const ABitmap: JBitmap; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).round;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight), TpointF.create(XCropCenter, YCropCenter)).round;
  var LJDestRect := TJRect.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
  var LJSrcRect := TJRect.JavaClass.init(LSrcRect.left, LSrcRect.top, LSrcRect.right, LSrcRect.bottom);

  Result := TJBitmap.JavaClass.createBitmap(LDestRect.Width, LDestRect.Height, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);

  var LCanvas := TJCanvas.JavaClass.init(result);
  var LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
  LCanvas.drawRoundRect(
    LDestRect.left, // left: Single;
    LDestRect.top, // top: Single;
    LDestRect.right, // right: Single;
    LDestRect.bottom, // bottom: Single
    xRadius {rx},
    yRadius {ry},
    LPaint);

  var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
  LPaint.setXfermode(LPorterDuffXfermode);
  LCanvas.drawBitmap(ABitmap, LJSrcRect, LJDestRect, LPaint);
  LPaint.setXfermode(nil);
  LPorterDuffXfermode := nil;

  LPaint := nil;
  LCanvas := nil;
  LJSrcRect := nil;
  LJDestRect := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectJBitmap(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropToRoundRectJBitmap(LBitmap, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectJBitmap(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToRoundRectJBitmap(LStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropToRoundRectJBitmap(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropToRoundRectJBitmap(LBitmap, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropToRoundRectCGContextRef(const AImage: ALOSImage; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;

var
  LGridHeight: Integer;
  LCurPoint: TpointF;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _moveTo(const x: Single; const y: Single);
  begin
    CGContextMoveToPoint(Result, X, LGridHeight - Y);
    LCurPoint.X := x;
    LCurPoint.Y := Y;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _rQuadTo(const dx1: Single; const dy1: Single; const dx2: Single; const dy2: Single);
  begin
    CGContextAddQuadCurveToPoint(
      Result,
      LCurPoint.X + dx1{cpx},
      LGridHeight - (LCurPoint.Y + dy1){cpy},
      LCurPoint.X + dx2{x},
      LGridHeight - (LCurPoint.Y + dy2){y});
    LCurPoint.X := LCurPoint.X + dx2;
    LCurPoint.Y := LCurPoint.Y + dy2;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _rLineTo(const dx: Single; const dy: Single);
  begin
    CGContextAddLineToPoint(Result, LCurPoint.X + dx{x}, LGridHeight - (LCurPoint.Y + dy{y}));
    LCurPoint.X := LCurPoint.X + dx;
    LCurPoint.Y := LCurPoint.Y + dy;
  end;

begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LRatio: single;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage)), TpointF.create(XCropCenter, YCropCenter), LRatio);
  //-----
  Result := ALCreateCGContextRef(LDestRect.Width, LDestRect.Height);
  CGContextBeginPath(Result);  // Creates a new empty path in a graphics context.

  LGridHeight := LDestRect.Height;
  var LXRadius: single := xRadius;
  var LYRadius: single := yRadius;
  if (LXRadius > LDestRect.width / 2) then LXRadius := LDestRect.width / 2;
  if (LYRadius > LDestRect.height / 2) then LYRadius := LDestRect.height / 2;
  var LWidthMinusCorners: single := (LDestRect.width - (2 * LXRadius));
  var LHeightMinusCorners: single := (LDestRect.height - (2 * LYRadius));

  //----- TopRight
  _moveTo(LDestRect.right, LDestRect.top + LYRadius);
  _rQuadTo(0, -LYRadius, -LXRadius, -LYRadius);
  _rLineTo(-LWidthMinusCorners, 0);

  //----- TopLeft
  _rQuadTo(-LXRadius, 0, -LXRadius, LYRadius);
  _rLineTo(0, LHeightMinusCorners);

  //----- BottomLeft
  _rQuadTo(0, LYRadius, LXRadius, LYRadius);
  _rLineTo(LWidthMinusCorners, 0);

  //----- BottomRight
  _rQuadTo(LXRadius, 0, LXRadius, -LYRadius);
  _rLineTo(0, -LHeightMinusCorners);

  CGContextClosePath(Result); // Closes and terminates the current path’s subpath.
  CGContextClip(Result); // Modifies the current clipping path, using the nonzero winding number rule.
                         // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                         // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                         // save the graphics state before you clip and restore the graphics state after you’ve completed
                         // any clipped drawing.
  CGContextDrawImage(
    Result, // c: The graphics context in which to draw the image.
    ALLowerLeftCGRect(
      TpointF.Create(
        0-(LSrcRect.Left*LRatio),
        0-(LSrcRect.top*LRatio)),
      LDestRect.width + (LSrcRect.Left*LRatio) + ((ALOSImageGetWidth(AImage)-LSrcRect.right)*LRatio),
      LDestRect.height + (LSrcRect.top*LRatio)  + ((ALOSImageGetHeight(AImage)-LSrcRect.bottom)*LRatio),
      LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectCGContextRef(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropToRoundRectCGContextRef(LImage, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectCGContextRef(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToRoundRectCGContextRef(LStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropToRoundRectCGContextRef(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropToRoundRectCGContextRef(LImage, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectCGImageRef(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropToRoundRectCGContextRef(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectCGImageRef(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToRoundRectCGImageRef(LStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropToRoundRectCGImageRef(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropToRoundRectCGContextRef(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{*****************************************************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropToRoundRectBitmap(const ABitmap: TBitmap; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := ALLoadFromBitmapAndFitIntoAndCropToBitmap(aBitmap, W, H, XCropCenter, YCropCenter);
  try

    Result := TBitmap.Create(LBitmap.Width,LBitmap.Height);
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.Fill.Bitmap.Bitmap.Assign(LBitmap);
        Result.Canvas.Fill.bitmap.WrapMode := TWrapMode.TileStretch;
        Result.Canvas.Fill.Kind := TBrushKind.Bitmap;
        Result.Canvas.FillRect(
          TRectF.Create(0,0, Result.Width,Result.Height),
          XRadius,
          YRadius,
          AllCorners,
          1 {AOpacity});
      finally
        Result.Canvas.EndScene;
      end;

    except
      AlFreeAndNil(Result);
      raise;
    end;

  finally
    AlFreeAndNil(LBitmap);
  end;
end;

{*****************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectBitmap(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropToRoundRectBitmap(LBitmap, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*******************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectBitmap(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToRoundRectBitmap(LStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{****************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropToRoundRectBitmap(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropToRoundRectBitmap(LBitmap, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{***********************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectDrawable(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropToRoundRectSkImage(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropToRoundRectSkSurface(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropToRoundRectSkSurface(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropToRoundRectJBitmap(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropToRoundRectCGContextRef(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropToRoundRectBitmap(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{*************************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectDrawable(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropToRoundRectSkImage(AResName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropToRoundRectSkSurface(AResName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropToRoundRectSkSurface(AResName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropToRoundRectJBitmap(AResName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropToRoundRectCGContextRef(AResName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropToRoundRectBitmap(AResName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{**********************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropToRoundRectDrawable(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropToRoundRectSkImage(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropToRoundRectSkSurface(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropToRoundRectSkSurface(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropToRoundRectJBitmap(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropToRoundRectCGContextRef(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropToRoundRectBitmap(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropToCircleSkSurface(const AImage: sk_image_t; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LRRect :=  ALSkCheckHandle(sk4d_rrect_create);
    try
      sk4d_rrect_set_oval(
        LRRect, // self: sk_rrect_t;
        @LDestRectF); // const rect: psk_rect_t;

      sk4d_canvas_clip_rrect(
        LCanvas, // self: sk_canvas_t;
        LRRect, // const rrect: sk_rrect_t;
        sk_clipop_t.INTERSECT_SK_CLIPOP, // op: sk_clipop_t;
        true); // anti_alias: _bool);

      var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
      sk4d_canvas_draw_image_rect(
        LCanvas, // self: sk_canvas_t;
        AImage, // const image: sk_image_t;
        @LSrcRect, // const src: psk_rect_t;
        @LDestRectF,  // const dest: psk_rect_t;
        @LSamplingoptions, // const sampling: psk_samplingoptions_t;
        LPaint, // const paint: sk_paint_t;
        FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)

      sk4d_canvas_restore(LCanvas);
    finally
      sk4d_rrect_destroy(LRRect);
    end;
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropToCircleSkSurface(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropToCircleSkSurface(LImage, W, H, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropToCircleSkSurface(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCircleSkSurface(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropToCircleSkSurface(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropToCircleSkSurface(LImage, W, H, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropToCircleSkImage(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropToCircleSkSurface(LImage, W, H, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropToCircleSkImage(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCircleSkImage(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropToCircleSkImage(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropToCircleSkSurface(LImage, W, H, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropToCircleJBitmap(const ABitmap: JBitmap; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).round;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight), TpointF.create(XCropCenter, YCropCenter)).round;
  var LJDestRect := TJRect.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
  var LJSrcRect := TJRect.JavaClass.init(LSrcRect.left, LSrcRect.top, LSrcRect.right, LSrcRect.bottom);

  Result := TJBitmap.JavaClass.createBitmap(LDestRect.Width, LDestRect.Height, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);

  var LCanvas := TJCanvas.JavaClass.init(result);
  var LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
  LCanvas.drawCircle(LDestRect.Width/2, LDestRect.Height/2, LDestRect.Width/2, LPaint);

  var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
  LPaint.setXfermode(LPorterDuffXfermode);
  LCanvas.drawBitmap(ABitmap, LJSrcRect, LJDestRect, LPaint);
  LPaint.setXfermode(nil);
  LPorterDuffXfermode := nil;

  LPaint := nil;
  LCanvas := nil;
  LJSrcRect := nil;
  LJDestRect := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropToCircleJBitmap(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropToCircleJBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropToCircleJBitmap(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCircleJBitmap(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropToCircleJBitmap(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropToCircleJBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropToCircleCGContextRef(const AImage: ALOSImage; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LRatio: single;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage)), TpointF.create(XCropCenter, YCropCenter), LRatio);
  //-----
  Result := ALCreateCGContextRef(LDestRect.Width, LDestRect.Height);
  CGContextBeginPath(Result);  // Creates a new empty path in a graphics context.
  CGContextAddEllipseInRect(
    Result,
    ALLowerLeftCGRect(
      TPointF.Create(LDestRect.Left, LDestRect.Top),
      LDestRect.Width,
      LDestRect.Height,
      LDestRect.Height)); // Adds an ellipse that fits inside the specified rectangle.
  CGContextClosePath(Result); // Closes and terminates the current path’s subpath.
  CGContextClip(Result); // Modifies the current clipping path, using the nonzero winding number rule.
                         // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                         // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                         // save the graphics state before you clip and restore the graphics state after you’ve completed
                         // any clipped drawing.
  CGContextDrawImage(
    Result, // c: The graphics context in which to draw the image.
    ALLowerLeftCGRect(
      TpointF.Create(
        0-(LSrcRect.Left*LRatio),
        0-(LSrcRect.top*LRatio)),
      LDestRect.width + (LSrcRect.Left*LRatio) + ((ALOSImageGetWidth(AImage)-LSrcRect.right)*LRatio),
      LDestRect.height + (LSrcRect.top*LRatio)  + ((ALOSImageGetHeight(AImage)-LSrcRect.bottom)*LRatio),
      LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropToCircleCGContextRef(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropToCircleCGContextRef(LImage, W, H, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropToCircleCGContextRef(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCircleCGContextRef(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropToCircleCGContextRef(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropToCircleCGContextRef(LImage, W, H, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropToCircleCGImageRef(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropToCircleCGContextRef(AStream, W, H, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropToCircleCGImageRef(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCircleCGImageRef(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropToCircleCGImageRef(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropToCircleCGContextRef(AFileName, W, H, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{******************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropToCircleBitmap(const ABitmap: TBitmap; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := ALLoadFromBitmapAndFitIntoAndCropToBitmap(ABitmap, W, H, XCropCenter, YCropCenter);
  try

    Result := TBitmap.Create(LBitmap.Width,LBitmap.Height);
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.Fill.Bitmap.Bitmap.Assign(LBitmap);
        Result.Canvas.Fill.bitmap.WrapMode := TWrapMode.TileStretch;
        Result.Canvas.Fill.Kind := TBrushKind.Bitmap;
        Result.Canvas.FillEllipse(TRectF.Create(0,0, Result.Width, Result.Height), 1 {AOpacity});
      finally
        Result.Canvas.EndScene;
      end;

    except
      AlFreeAndNil(Result);
      raise;
    end;

  finally
    AlFreeAndNil(LBitmap);
  end;
end;

{******************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropToCircleBitmap(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropToCircleBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{********************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropToCircleBitmap(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCircleBitmap(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropToCircleBitmap(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropToCircleBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropToCircleDrawable(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropToCircleSkImage(AStream, W, H, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropToCircleSkSurface(AStream, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropToCircleSkSurface(AStream, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropToCircleJBitmap(AStream, W, H, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropToCircleCGContextRef(AStream, W, H, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropToCircleBitmap(AStream, W, H, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{**************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropToCircleDrawable(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropToCircleSkImage(AResName, W, H, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropToCircleSkSurface(AResName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropToCircleSkSurface(AResName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropToCircleJBitmap(AResName, W, H, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropToCircleCGContextRef(AResName, W, H, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropToCircleBitmap(AResName, W, H, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{***********************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropToCircleDrawable(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropToCircleSkImage(AFileName, W, H, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropToCircleSkSurface(AFileName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropToCircleSkSurface(AFileName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropToCircleJBitmap(AFileName, W, H, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropToCircleCGContextRef(AFileName, W, H, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropToCircleBitmap(AFileName, W, H, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(const AImage: sk_image_t; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    sk4d_paint_set_color(LPaint, TalphaColorRec.White);
    sk4d_canvas_draw_Rect(LCanvas, @LDestRectF, LPaint);

    var LImageFilter := ALSkCheckHandle(
                          sk4d_imagefilter_make_blur(
                            ALConvertRadiusToSigma(ABlurRadius), //sigma_x,
                            ALConvertRadiusToSigma(ABlurRadius), //sigma_y: float;
                            sk_tilemode_t.CLAMP_SK_TILEMODE, //tile_mode: sk_tilemode_t;
                            0, //input: sk_imagefilter_t;
                            @LDestRectF));//const crop_rect: psk_rect_t
    try
      sk4d_paint_set_Image_filter(LPaint, LImageFilter);
      var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
      sk4d_canvas_draw_image_rect(
        LCanvas, // self: sk_canvas_t;
        AImage, // const image: sk_image_t;
        @LSrcRect, // const src: psk_rect_t;
        @LDestRectF,  // const dest: psk_rect_t;
        @LSamplingoptions, // const sampling: psk_samplingoptions_t;
        LPaint, // const paint: sk_paint_t;
        FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
    finally
      sk4d_refcnt_unref(LImageFilter)
    end;
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToSkSurface(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToSkSurface(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToSkSurface(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToSkSurface(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToSkImage(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToSkImage(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToSkImage(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToSkImage(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(const ABitmap: JBitmap; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).round;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight), TpointF.create(XCropCenter, YCropCenter)).Round;

  var LMatrix := TJMatrix.JavaClass.init;
  LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
  var LBitmap := TJBitmap.JavaClass.createBitmap(ABitmap{src}, LSrcRect.Left{X}, LSrcRect.top{Y}, LSrcRect.width{Width}, LSrcRect.height{height}, LMatrix{m}, True{filter});
  LMatrix := nil;

  Try

    if TOSVersion.Check(12, 0) and
       TJHardwareBuffer.javaclass.isSupported(
         LBitmap.getWidth, // width: Integer;
         LBitmap.getHeight, // height: Integer;
         TJPixelFormat.JavaClass.RGBA_8888, // format: Integer;
         1, // layers: Integer;
         TJHardwareBuffer.javaclass.USAGE_GPU_SAMPLED_IMAGE or
         TJHardwareBuffer.javaclass.USAGE_GPU_COLOR_OUTPUT) then begin //usage: Int64
      Var LImageReader := TJImageReader.JavaClass.newInstance(
                            LBitmap.getWidth, // width: Integer;
                            LBitmap.getHeight,// height: Integer;
                            TJPixelFormat.JavaClass.RGBA_8888, // format: Integer;
                            1, // maxImages: Integer
                            TJHardwareBuffer.javaclass.USAGE_GPU_SAMPLED_IMAGE or
                            TJHardwareBuffer.javaclass.USAGE_GPU_COLOR_OUTPUT); // usage: Int64
      try
        var LRenderNode := TJRenderNode.JavaClass.init(StringToJString('BlurEffect'));
        try
          var LHardwareRenderer := TJHardwareRenderer.JavaClass.init;
          try
            LHardwareRenderer.setSurface(LImageReader.getSurface);
            LHardwareRenderer.setContentRoot(LRenderNode);
            LRenderNode.setPosition(0, 0, LImageReader.GetWidth, LImageReader.GetHeight);
            var LBlurRenderEffect := TJRenderEffect.JavaClass.createBlurEffect(
                                       ABlurRadius,
                                       ABlurRadius,
                                       TJShader_TileMode.JavaClass.MIRROR);
            LRenderNode.setRenderEffect(LBlurRenderEffect);
            var LrenderCanvas := LrenderNode.beginRecording;
            LRenderCanvas.drawBitmap(LBitmap, 0{left}, 0{top}, nil{paint});
            LRenderNode.endRecording;
            LHardwareRenderer.createRenderRequest.setWaitForPresent(true).syncAndDraw;
            var LImage := LImageReader.acquireNextImage;
            if LImage = nil then raise Exception.Create('No Image');
            try
              var LHardwareBuffer := LImage.GetHardwareBuffer;
              if LHardwareBuffer = nil then raise Exception.Create('No HardwareBuffer');
              try
                var LHardwareBitmap := TJBitmap.javaclass.wrapHardwareBuffer(LhardwareBuffer, ALGetGlobalJColorSpace);
                if LHardwareBitmap=nil then raise Exception.Create('Create Bitmap Failed');
                try
                  //This is necessary to convert later the JBitmap in texture via texImage2D
                  Result := LHardwareBitmap.copy(TJBitmap_Config.JavaClass.ARGB_8888, false{isMutable});
                finally
                  if not LHardwareBitmap.equals(Result) then LHardwareBitmap.recycle;
                  LHardwareBitmap := nil;
                end;
              finally
                LhardwareBuffer.close;
                LhardwareBuffer := nil;
              end;
            finally
              LImage.close;
              LImage := nil;
            end;
          finally
            LHardwareRenderer.destroy;
            LHardwareRenderer := nil;
          end;
        finally
          LRenderNode.discardDisplayList;
          LRenderNode := nil;
        end;
      finally
        LImageReader.close;
        LImageReader := nil;
      end;
    end
    else begin
      var LRS := getRenderScript;
      var LInput := TJAllocation.JavaClass.createFromBitmap(LRS, LBitmap);
      var LOutPut := TJAllocation.JavaClass.createTyped(LRS, LInput.getType());
      var LScript :=  TJScriptIntrinsicBlur.javaclass.create(LRS, TJElement.javaclass.U8_4(LRS));
      LScript.setRadius(Min(25, ABlurRadius)); // Set the radius of the Blur. Supported range 0 < radius <= 25
      LScript.setInput(LInput);
      LScript.forEach(LOutPut);
      LOutPut.copyTo(LBitmap);
      Result := LBitmap;
      LScript := nil;
      LInput := nil;
      LOutPut := nil;
      LRS := nil;
    end;

  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToJBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToJBitmap(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToJBitmap(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToJBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropAndBlurToCGContextRef(const AImage: ALOSImage; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  Result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(AImage, W, H, XCropCenter, YCropCenter);
  try
    var LDestRect := Trect.Create(0, 0, CGBitmapContextGetWidth(Result), CGBitmapContextGetHeight(Result));
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     CGBitmapContextGetData(Result), // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     CGBitmapContextGetBytesPerRow(Result) * NSUInteger(LDestRect.Height), // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var Lformat: CIFormat;
      if GlobalUseMetal then Lformat := kCIFormatBGRA8
      else Lformat := kCIFormatRGBA8;
      var LCIImage := TCIImage.Wrap(
                        TCIImage.OCClass.imageWithBitmapData(
                          LData, // d: NSData;
                          CGBitmapContextGetBytesPerRow(Result), // bytesPerRow: size_t;
                          CGSizeMake(LDestRect.Width, LDestRect.Height), // size: CGSize;
                          Lformat, // format: CIFormat;
                          ALGetGlobalCGColorSpace)); // colorSpace: CGColorSpaceRef));

      // Gaussian blur CIFilter naturally creates artifacts at the borders of the
      // output image. It is happening because the gaussian blur filter samples
      // pixels outside the edges of the image. But because there are no pixels,
      // you get this weird artefact. You can use "CIAffineClamp" filter to
      // "extend" your image infinitely in all directions.
      var LClampFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIAffineClamp')));
      LClampFilter.setDefaults;
      LClampFilter.setValueforKey(NSObjectToID(LCIImage), kCIInputImageKey);

      var LBlurFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIGaussianBlur')));
      LBlurFilter.setValueforKey(NSObjectToID(LClampFilter.outputImage), kCIInputImageKey);
      LBlurFilter.setValueforKey(TNSNumber.OCClass.numberWithFloat(aBlurRadius), kCIInputRadiusKey);

      var LCIContext := TCIContext.Wrap(TCIContext.OCClass.contextWithOptions(nil));
      var LCGImageRef := LCIContext.createCGImage(LBlurFilter.outputImage, LCIImage.extent);
      if LCGImageRef = nil then raise Exception.Create('Failed to create CGImageRef from CIContext');
      try

        CGContextDrawImage(
          Result, // c: The graphics context in which to draw the image.
          ALLowerLeftCGRect(
            TpointF.Create(0,0),
              LDestRect.Width,
              LDestRect.Height,
              LDestRect.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
          LCGImageRef); // image The image to draw.

      finally
        CGImageRelease(LCGImageRef);
      end;

      LCIImage := nil; // no need to call LCIImage.release; (i try => exception)
      LCIContext := nil; // no need to call LCIContext.release; (i try => exception)
      LBlurFilter := nil; // no need to call LBlurFilter.release (i try => exception)
      LClampFilter := nil; // no need to call LClampFilter.release (i try => exception)
    finally
      LData.release;
    end;
  Except
    On Exception Do begin
      CGContextRelease(Result);
      Raise;
    end;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCGContextRef(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropAndBlurToCGContextRef(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCGContextRef(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCGContextRef(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCGContextRef(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropAndBlurToCGContextRef(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCGImageRef(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropAndBlurToCGContextRef(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCGImageRef(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCGImageRef(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCGImageRef(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropAndBlurToCGContextRef(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************************************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(const ABitmap: TBitmap; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.Width, ABitmap.height), TpointF.create(XCropCenter, YCropCenter));

  Result := TBitmap.Create(LDestRect.Width,LDestRect.Height);
  try

    if Result.Canvas.BeginScene then
    try
      Result.Canvas.DrawBitmap(
        ABitmap, // const ABitmap: TBitmap;
        LSrcRect, //const SrcRect,
        LDestRect, //const DstRect: TRectF;
        1, //const AOpacity: Single;
        false); // const HighSpeed: Boolean => disable interpolation

      var LBlurEffect := TBlurEffect.Create(nil);
      try
        // Specifies the amount of blur applied to the shadow.
        // Softness is a System.Single value that takes values in the range from 0 through 9.
        // I calculate approximatly that 0.5 = around 12 for blur
        LBlurEffect.softness := ABlurRadius / 24;
        Result.Canvas.Flush;
        LBlurEffect.ProcessEffect(Result.Canvas, Result.Canvas.Bitmap, 1);
      finally
        ALFreeAndNil(LBlurEffect);
      end;

    finally
      Result.Canvas.EndScene;
    end;

  except
    AlFreeAndNil(Result);
    raise;
  end;
end;

{**********************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToBitmap(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToBitmap(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndBlurToBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{****************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToDrawable(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropAndBlurToSkImage(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndBlurToSkSurface(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndBlurToSkSurface(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropAndBlurToJBitmap(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropAndBlurToCGContextRef(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropAndBlurToBitmap(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{******************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToDrawable(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropAndBlurToSkImage(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndBlurToSkSurface(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndBlurToSkSurface(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropAndBlurToJBitmap(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropAndBlurToCGContextRef(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropAndBlurToBitmap(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{***************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndBlurToDrawable(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropAndBlurToSkImage(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndBlurToSkSurface(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndBlurToSkSurface(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropAndBlurToJBitmap(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropAndBlurToCGContextRef(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropAndBlurToBitmap(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropAndBlurToCircleSkSurface(const AImage: sk_image_t; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LRRect :=  ALSkCheckHandle(sk4d_rrect_create);
    try
      sk4d_rrect_set_oval(
        LRRect, // self: sk_rrect_t;
        @LDestRectF); // const rect: psk_rect_t;

      sk4d_canvas_clip_rrect(
        LCanvas, // self: sk_canvas_t;
        LRRect, // const rrect: sk_rrect_t;
        sk_clipop_t.INTERSECT_SK_CLIPOP, // op: sk_clipop_t;
        true); // anti_alias: _bool);

      sk4d_paint_set_color(LPaint, TalphaColorRec.White);
      sk4d_canvas_draw_Rect(LCanvas, @LDestRectF, LPaint);

      var LImageFilter := ALSkCheckHandle(
                            sk4d_imagefilter_make_blur(
                              ALConvertRadiusToSigma(ABlurRadius), //sigma_x,
                              ALConvertRadiusToSigma(ABlurRadius), //sigma_y: float;
                              sk_tilemode_t.CLAMP_SK_TILEMODE, //tile_mode: sk_tilemode_t;
                              0, //input: sk_imagefilter_t;
                              @LDestRectF));//const crop_rect: psk_rect_t
      try
        sk4d_paint_set_Image_filter(LPaint, LImageFilter);
        var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
        sk4d_canvas_draw_image_rect(
          LCanvas, // self: sk_canvas_t;
          AImage, // const image: sk_image_t;
          @LSrcRect, // const src: psk_rect_t;
          @LDestRectF,  // const dest: psk_rect_t;
          @LSamplingoptions, // const sampling: psk_samplingoptions_t;
          LPaint, // const paint: sk_paint_t;
          FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
      finally
        sk4d_refcnt_unref(LImageFilter)
      end;

      sk4d_canvas_restore(LCanvas);
    finally
      sk4d_rrect_destroy(LRRect);
    end;
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkSurface(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropAndBlurToCircleSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleSkSurface(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkSurface(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleSkSurface(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropAndBlurToCircleSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkImage(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndBlurToCircleSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleSkImage(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkImage(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleSkImage(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndBlurToCircleSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropAndBlurToCircleJBitmap(const ABitmap: JBitmap; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LBitmap := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(ABitmap, W, H, aBlurRadius, XCropCenter, YCropCenter);
  try

    var LRect := Trect.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight);
    var LJRect := TJRect.JavaClass.init(LRect.left, LRect.top, LRect.right, LRect.bottom);

    Result := TJBitmap.JavaClass.createBitmap(LRect.Width, LRect.Height, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);

    var LCanvas := TJCanvas.JavaClass.init(result);
    var LPaint := TJPaint.JavaClass.init;
    LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
    LCanvas.drawCircle(LRect.Width/2, LRect.Height/2, LRect.Width/2, LPaint);

    var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
    LPaint.setXfermode(LPorterDuffXfermode);
    LCanvas.drawBitmap(LBitmap, LJRect, LJRect, LPaint);
    LPaint.setXfermode(nil);
    LPorterDuffXfermode := nil;

    LPaint := nil;
    LCanvas := nil;
    LJRect := nil;

  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleJBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToCircleJBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleJBitmap(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleJBitmap(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleJBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToCircleJBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropAndBlurToCircleCGContextRef(const AImage: ALOSImage; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  Result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(AImage, W, H, XCropCenter, YCropCenter);
  try
    var LDestRect := Trect.Create(0, 0, CGBitmapContextGetWidth(Result), CGBitmapContextGetHeight(Result));
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     CGBitmapContextGetData(Result), // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     CGBitmapContextGetBytesPerRow(Result) * NSUInteger(LDestRect.Height), // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var Lformat: CIFormat;
      if GlobalUseMetal then Lformat := kCIFormatBGRA8
      else Lformat := kCIFormatRGBA8;
      var LCIImage := TCIImage.Wrap(
                        TCIImage.OCClass.imageWithBitmapData(
                          LData, // d: NSData;
                          CGBitmapContextGetBytesPerRow(Result), // bytesPerRow: size_t;
                          CGSizeMake(LDestRect.Width, LDestRect.Height), // size: CGSize;
                          Lformat, // format: CIFormat;
                          ALGetGlobalCGColorSpace)); // colorSpace: CGColorSpaceRef));

      // Gaussian blur CIFilter naturally creates artifacts at the borders of the
      // output image. It is happening because the gaussian blur filter samples
      // pixels outside the edges of the image. But because there are no pixels,
      // you get this weird artefact. You can use "CIAffineClamp" filter to
      // "extend" your image infinitely in all directions.
      var LClampFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIAffineClamp')));
      LClampFilter.setDefaults;
      LClampFilter.setValueforKey(NSObjectToID(LCIImage), kCIInputImageKey);

      var LBlurFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIGaussianBlur')));
      LBlurFilter.setValueforKey(NSObjectToID(LClampFilter.outputImage), kCIInputImageKey);
      LBlurFilter.setValueforKey(TNSNumber.OCClass.numberWithFloat(aBlurRadius), kCIInputRadiusKey);

      var LCIContext := TCIContext.Wrap(TCIContext.OCClass.contextWithOptions(nil));
      var LCGImageRef := LCIContext.createCGImage(LBlurFilter.outputImage, LCIImage.extent);
      if LCGImageRef = nil then raise Exception.Create('Failed to create CGImageRef from CIContext');
      try

        CGContextClearRect(
          Result,
          ALLowerLeftCGRect(
            TpointF.Create(0,0),
              LDestRect.Width,
              LDestRect.Height,
              LDestRect.Height)); // Paints a transparent rectangle.

        CGContextBeginPath(Result);  // Creates a new empty path in a graphics context.
        CGContextAddEllipseInRect(
          Result,
          ALLowerLeftCGRect(
            TpointF.Create(0,0),
              LDestRect.Width,
              LDestRect.Height,
              LDestRect.Height)); // Adds an ellipse that fits inside the specified rectangle.
        CGContextClosePath(Result); // Closes and terminates the current path’s subpath.
        CGContextClip(Result); // Modifies the current clipping path, using the nonzero winding number rule.
                               // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                               // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                               // save the graphics state before you clip and restore the graphics state after you’ve completed
                               // any clipped drawing.
        CGContextDrawImage(
          Result, // c: The graphics context in which to draw the image.
          ALLowerLeftCGRect(
            TpointF.Create(0,0),
              LDestRect.Width,
              LDestRect.Height,
              LDestRect.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
          LCGImageRef); // image The image to draw.

      finally
        CGImageRelease(LCGImageRef);
      end;

      LCIImage := nil; // no need to call LCIImage.release; (i try => exception)
      LCIContext := nil; // no need to call LCIContext.release; (i try => exception)
      LBlurFilter := nil; // no need to call LBlurFilter.release (i try => exception)
      LClampFilter := nil; // no need to call LClampFilter.release (i try => exception)
    finally
      LData.release;
    end;
  Except
    On Exception Do begin
      CGContextRelease(Result);
      Raise;
    end;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGContextRef(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropAndBlurToCircleCGContextRef(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleCGContextRef(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGContextRef(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleCGContextRef(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropAndBlurToCircleCGContextRef(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGImageRef(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGContextRef(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleCGImageRef(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGImageRef(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleCGImageRef(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleCGContextRef(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{****************************************************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropAndBlurToCircleBitmap(const ABitmap: TBitmap; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(abitmap, W, H, aBlurRadius, XCropCenter, YCropCenter);
  try

    Result := TBitmap.Create(LBitmap.Width,LBitmap.Height);
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.Fill.Bitmap.Bitmap.Assign(LBitmap);
        Result.Canvas.Fill.bitmap.WrapMode := TWrapMode.TileStretch;
        Result.Canvas.Fill.Kind := TBrushKind.Bitmap;
        Result.Canvas.FillEllipse(TRectF.Create(0,0, Result.Width, Result.Height), 1 {AOpacity});
      finally
        Result.Canvas.EndScene;
      end;

    except
      AlFreeAndNil(Result);
      raise;
    end;

  finally
    AlFreeAndNil(LBitmap);
  end;
end;

{****************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndBlurToCircleBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{******************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleBitmap(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleBitmap(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{***************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndBlurToCircleBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{**********************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleDrawable(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkImage(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkSurface(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkSurface(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleJBitmap(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGContextRef(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleBitmap(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{************************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleDrawable(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleSkImage(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleSkSurface(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleSkSurface(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleJBitmap(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleCGContextRef(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleBitmap(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{*********************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleDrawable(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleSkImage(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleSkSurface(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleSkSurface(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleJBitmap(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleCGContextRef(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleBitmap(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropAndMaskToSkSurface(const AImage: sk_image_t; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, sk4d_image_get_width(AMask), sk4d_image_get_Height(AMask)).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
    sk4d_canvas_draw_image_rect(
      LCanvas, // self: sk_canvas_t;
      AMask, // const image: sk_image_t;
      @LDestRectF, // const src: psk_rect_t;
      @LDestRectF,  // const dest: psk_rect_t;
      @LSamplingoptions, // const sampling: psk_samplingoptions_t;
      LPaint, // const paint: sk_paint_t;
      FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)

    var LBlender := ALSkCheckHandle(
                      sk4d_blender_make_mode(
                        sk_blendmode_t.SRC_IN_SK_BLENDMODE));
    try
      sk4d_paint_set_blender(LPaint, LBlender);
      sk4d_canvas_draw_image_rect(
        LCanvas, // self: sk_canvas_t;
        AImage, // const image: sk_image_t;
        @LSrcRect, // const src: psk_rect_t;
        @LDestRectF,  // const dest: psk_rect_t;
        @LSamplingoptions, // const sampling: psk_samplingoptions_t;
        LPaint, // const paint: sk_paint_t;
        FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
    finally
      sk4d_refcnt_unref(LBlender)
    end;

  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToSkSurface(const AStream: TStream; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropAndMaskToSkSurface(LImage, AMask, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToSkSurface(const AResName: String; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskToSkSurface(LStream, AMask, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndMaskToSkSurface(const AFileName: String; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropAndMaskToSkSurface(LImage, AMask, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToSkImage(const AStream: TStream; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndMaskToSkSurface(LImage, AMask, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToSkImage(const AResName: String; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskToSkImage(LStream, AMask, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndMaskToSkImage(const AFileName: String; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndMaskToSkSurface(LImage, AMask, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropAndMaskToJBitmap(const ABitmap: JBitmap; const AMask: JBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LDestRect := TRect.Create(0, 0, aMask.getWidth, aMask.getHeight);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight), TpointF.create(XCropCenter, YCropCenter)).round;
  var LJDestRect := TJRect.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
  var LJSrcRect := TJRect.JavaClass.init(LSrcRect.left, LSrcRect.top, LSrcRect.right, LSrcRect.bottom);

  Result := TJBitmap.JavaClass.createBitmap(LDestRect.Width, LDestRect.Height, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);

  var LCanvas := TJCanvas.JavaClass.init(result);
  var LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
  LCanvas.drawBitmap(aMask, 0{left}, 0{top}, LPaint);

  var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
  LPaint.setXfermode(LPorterDuffXfermode);
  LCanvas.drawBitmap(ABitmap, LJSrcRect, LJDestRect, LPaint);
  LPaint.setXfermode(nil);
  LPorterDuffXfermode := nil;

  LPaint := nil;
  LCanvas := nil;
  LJSrcRect := nil;
  LJDestRect := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToJBitmap(const AStream: TStream; const AMask: JBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropAndMaskToJBitmap(LBitmap, AMask, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToJBitmap(const AResName: String; const AMask: JBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskToJBitmap(LStream, AMask, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropAndMaskToJBitmap(const AFileName: String; const AMask: JBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropAndMaskToJBitmap(LBitmap, AMask, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropAndMaskToCGContextRef(const AImage: ALOSImage; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LDestRect := Trect.Create(0, 0, CGImageGetWidth(aMask), CGImageGetHeight(aMask));
  var LRatio: single;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage)), TpointF.create(XCropCenter, YCropCenter), LRatio);
  //-----
  Result := ALCreateCGContextRef(LDestRect.Width, LDestRect.Height);
  CGContextClipToMask(
    Result,
    ALLowerLeftCGRect(
      TpointF.Create(0, 0),
      LDestRect.width,
      LDestRect.height,
      LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
  CGContextDrawImage(
    Result, // c: The graphics context in which to draw the image.
    ALLowerLeftCGRect(
      TpointF.Create(
        0-(LSrcRect.Left*LRatio),
        0-(LSrcRect.top*LRatio)),
      LDestRect.width + (LSrcRect.Left*LRatio) + ((ALOSImageGetWidth(AImage)-LSrcRect.right)*LRatio),
      LDestRect.height + (LSrcRect.top*LRatio)  + ((ALOSImageGetHeight(AImage)-LSrcRect.bottom)*LRatio),
      LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToCGContextRef(const AStream: TStream; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropAndMaskToCGContextRef(LImage, AMask, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToCGContextRef(const AResName: String; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskToCGContextRef(LStream, AMask, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndMaskToCGContextRef(const AFileName: String; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropAndMaskToCGContextRef(LImage, AMask, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToCGImageRef(const AStream: TStream; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropAndMaskToCGContextRef(AStream, AMask, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToCGImageRef(const AResName: String; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskToCGImageRef(LStream, AMask, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndMaskToCGImageRef(const AFileName: String; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropAndMaskToCGContextRef(AFileName, AMask, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{*********************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropAndMaskToBitmap(const ABitmap: TBitmap; const AMask: TBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := ALLoadFromBitmapAndFitIntoAndCropToBitmap(aBitmap, aMask.Width, aMask.height, XCropCenter, YCropCenter);
  try

    Result := TBitmap.Create(LBitmap.Width,LBitmap.Height);
    try

      var D, B, M: TBitmapData;
      if Result.Map(TMapAccess.Write, D) then
      try
        if LBitmap.Map(TMapAccess.Read, B) then
        try
          if aMask.Map(TMapAccess.Read, M) then
          try
            for var J := 0 to Result.Height - 1 do
              for var I := 0 to Result.Width - 1 do
              begin
                var C := B.GetPixel(I, J);
                TAlphaColorRec(C).A := TAlphaColorRec(M.GetPixel(I, J)).A;
                if TAlphaColorRec(C).A < 255 then begin  // << don't ask me why we need to do this :(
                  var ratio: single := TAlphaColorRec(C).A / 255;
                  TAlphaColorRec(C).R := round(TAlphaColorRec(C).R * ratio);
                  TAlphaColorRec(C).G := round(TAlphaColorRec(C).G * ratio);
                  TAlphaColorRec(C).B := round(TAlphaColorRec(C).B * ratio);
                end;
                D.SetPixel(I, J, C);
              end;
          finally
            aMask.Unmap(M);
          end;
        finally
          LBitmap.Unmap(B);
        end;
      finally
        Result.Unmap(D);
      end;

    except
      AlFreeAndNil(Result);
      raise;
    end;

  finally
    AlFreeAndNil(LBitmap);
  end;
end;

{*********************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToBitmap(const AStream: TStream; const AMask: TBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndMaskToBitmap(LBitmap, AMask, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{***********************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToBitmap(const AResName: String; const AMask: TBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskToBitmap(LStream, AMask, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{********************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndMaskToBitmap(const AFileName: String; const AMask: TBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndMaskToBitmap(LBitmap, AMask, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{******************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToDrawable(const AStream: TStream; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropAndMaskToSkImage(AStream, AMask, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndMaskToSkSurface(AStream, AMask, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndMaskToSkSurface(AStream, AMask, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropAndMaskToJBitmap(AStream, AMask, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropAndMaskToCGContextRef(AStream, AMask, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropAndMaskToBitmap(AStream, AMask, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{********************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToDrawable(const AResName: String; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropAndMaskToSkImage(AResName, AMask, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndMaskToSkSurface(AResName, AMask, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndMaskToSkSurface(AResName, AMask, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropAndMaskToJBitmap(AResName, AMask, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropAndMaskToCGContextRef(AResName, AMask, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropAndMaskToBitmap(AResName, AMask, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{*****************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndMaskToDrawable(const AFileName: String; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropAndMaskToSkImage(AFileName, AMask, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndMaskToSkSurface(AFileName, AMask, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndMaskToSkSurface(AFileName, AMask, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropAndMaskToJBitmap(AFileName, AMask, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropAndMaskToCGContextRef(AFileName, AMask, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropAndMaskToBitmap(AFileName, AMask, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropAndMaskAndBlurToSkSurface(const AImage: sk_image_t; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, sk4d_image_get_width(AMask), sk4d_image_get_Height(AMask)).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
    sk4d_canvas_draw_image_rect(
      LCanvas, // self: sk_canvas_t;
      AMask, // const image: sk_image_t;
      @LDestRectF, // const src: psk_rect_t;
      @LDestRectF,  // const dest: psk_rect_t;
      @LSamplingoptions, // const sampling: psk_samplingoptions_t;
      LPaint, // const paint: sk_paint_t;
      FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)

    var LBlender := ALSkCheckHandle(
                      sk4d_blender_make_mode(
                        sk_blendmode_t.SRC_IN_SK_BLENDMODE));
    try
      sk4d_paint_set_blender(LPaint, LBlender);

      var LImageFilter := ALSkCheckHandle(
                            sk4d_imagefilter_make_blur(
                              ALConvertRadiusToSigma(ABlurRadius), //sigma_x,
                              ALConvertRadiusToSigma(ABlurRadius), //sigma_y: float;
                              sk_tilemode_t.CLAMP_SK_TILEMODE, //tile_mode: sk_tilemode_t;
                              0, //input: sk_imagefilter_t;
                              @LDestRectF));//const crop_rect: psk_rect_t
      try
        sk4d_paint_set_Image_filter(LPaint, LImageFilter);
        sk4d_canvas_draw_image_rect(
          LCanvas, // self: sk_canvas_t;
          AImage, // const image: sk_image_t;
          @LSrcRect, // const src: psk_rect_t;
          @LDestRectF,  // const dest: psk_rect_t;
          @LSamplingoptions, // const sampling: psk_samplingoptions_t;
          LPaint, // const paint: sk_paint_t;
          FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
      finally
        sk4d_refcnt_unref(LImageFilter)
      end;

    finally
      sk4d_refcnt_unref(LBlender)
    end;

  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkSurface(const AStream: TStream; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropAndMaskAndBlurToSkSurface(LImage, AMask, ABlurRadius, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToSkSurface(const AResName: String; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkSurface(LStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToSkSurface(const AFileName: String; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropAndMaskAndBlurToSkSurface(LImage, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkImage(const AStream: TStream; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndMaskAndBlurToSkSurface(LImage, AMask, ABlurRadius, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToSkImage(const AResName: String; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkImage(LStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToSkImage(const AFileName: String; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndMaskAndBlurToSkSurface(LImage, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropAndMaskAndBlurToJBitmap(const ABitmap: JBitmap; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LBitmap := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(ABitmap, aMask.getWidth, aMask.getHeight, aBlurRadius, XCropCenter, YCropCenter);
  try
    var LRect := TRect.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight);
    var LJRect := TJRect.JavaClass.init(LRect.left, LRect.top, LRect.right, LRect.bottom);

    Result := TJBitmap.JavaClass.createBitmap(LRect.Width, LRect.Height, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);

    var LCanvas := TJCanvas.JavaClass.init(result);
    var LPaint := TJPaint.JavaClass.init;
    LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
    LCanvas.drawBitmap(aMask, 0{left}, 0{top}, LPaint);

    var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
    LPaint.setXfermode(LPorterDuffXfermode);
    LCanvas.drawBitmap(LBitmap, LJRect, LJRect, LPaint);
    LPaint.setXfermode(nil);
    LPorterDuffXfermode := nil;

    LPaint := nil;
    LCanvas := nil;
    LJRect := nil;
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(const AStream: TStream; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropAndMaskAndBlurToJBitmap(LBitmap, AMask, ABlurRadius, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToJBitmap(const AResName: String; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(LStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToJBitmap(const AFileName: String; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropAndMaskAndBlurToJBitmap(LBitmap, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropAndMaskAndBlurToCGContextRef(const AImage: ALOSImage; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  Result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(AImage, CGImageGetWidth(aMask), CGImageGetHeight(aMask), XCropCenter, YCropCenter);
  try
    var LDestRect := Trect.Create(0, 0, CGBitmapContextGetWidth(Result), CGBitmapContextGetHeight(Result));
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     CGBitmapContextGetData(Result), // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     CGBitmapContextGetBytesPerRow(Result) * NSUInteger(LDestRect.Height), // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var Lformat: CIFormat;
      if GlobalUseMetal then Lformat := kCIFormatBGRA8
      else Lformat := kCIFormatRGBA8;
      var LCIImage := TCIImage.Wrap(
                        TCIImage.OCClass.imageWithBitmapData(
                          LData, // d: NSData;
                          CGBitmapContextGetBytesPerRow(Result), // bytesPerRow: size_t;
                          CGSizeMake(LDestRect.Width, LDestRect.Height), // size: CGSize;
                          Lformat, // format: CIFormat;
                          ALGetGlobalCGColorSpace)); // colorSpace: CGColorSpaceRef));

      // Gaussian blur CIFilter naturally creates artifacts at the borders of the
      // output image. It is happening because the gaussian blur filter samples
      // pixels outside the edges of the image. But because there are no pixels,
      // you get this weird artefact. You can use "CIAffineClamp" filter to
      // "extend" your image infinitely in all directions.
      var LClampFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIAffineClamp')));
      LClampFilter.setDefaults;
      LClampFilter.setValueforKey(NSObjectToID(LCIImage), kCIInputImageKey);

      var LBlurFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIGaussianBlur')));
      LBlurFilter.setValueforKey(NSObjectToID(LClampFilter.outputImage), kCIInputImageKey);
      LBlurFilter.setValueforKey(TNSNumber.OCClass.numberWithFloat(aBlurRadius), kCIInputRadiusKey);

      var LCIContext := TCIContext.Wrap(TCIContext.OCClass.contextWithOptions(nil));
      var LCGImageRef := LCIContext.createCGImage(LBlurFilter.outputImage, LCIImage.extent);
      if LCGImageRef = nil then raise Exception.Create('Failed to create CGImageRef from CIContext');
      try

        CGContextClearRect(
          Result,
          ALLowerLeftCGRect(
            TpointF.Create(0,0),
              LDestRect.Width,
              LDestRect.Height,
              LDestRect.Height)); // Paints a transparent rectangle.

        CGContextClipToMask(
          Result,
          ALLowerLeftCGRect(
            TpointF.Create(0, 0),
            LDestRect.width,
            LDestRect.height,
            LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
          aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.

        CGContextDrawImage(
          Result, // c: The graphics context in which to draw the image.
          ALLowerLeftCGRect(
            TpointF.Create(0,0),
              LDestRect.Width,
              LDestRect.Height,
              LDestRect.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
          LCGImageRef); // image The image to draw.

      finally
        CGImageRelease(LCGImageRef);
      end;

      LCIImage := nil; // no need to call LCIImage.release; (i try => exception)
      LCIContext := nil; // no need to call LCIContext.release; (i try => exception)
      LBlurFilter := nil; // no need to call LBlurFilter.release (i try => exception)
      LClampFilter := nil; // no need to call LClampFilter.release (i try => exception)
    finally
      LData.release;
    end;
  Except
    On Exception Do begin
      CGContextRelease(Result);
      Raise;
    end;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGContextRef(const AStream: TStream; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropAndMaskAndBlurToCGContextRef(LImage, AMask, ABlurRadius, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToCGContextRef(const AResName: String; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGContextRef(LStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToCGContextRef(const AFileName: String; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropAndMaskAndBlurToCGContextRef(LImage, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGImageRef(const AStream: TStream; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGContextRef(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToCGImageRef(const AResName: String; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGImageRef(LStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToCGImageRef(const AFileName: String; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToCGContextRef(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{*******************************************************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropAndMaskAndBlurToBitmap(const ABitmap: TBitmap; const AMask: TBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(aBitmap, aMask.Width, aMask.height, ABlurRadius, XCropCenter, YCropCenter);
  try

    Result := TBitmap.Create(LBitmap.Width,LBitmap.Height);
    try

      var D, B, M: TBitmapData;
      if Result.Map(TMapAccess.Write, D) then
      try
        if LBitmap.Map(TMapAccess.Read, B) then
        try
          if aMask.Map(TMapAccess.Read, M) then
          try
            for var J := 0 to Result.Height - 1 do
              for var I := 0 to Result.Width - 1 do
              begin
                var C := B.GetPixel(I, J);
                TAlphaColorRec(C).A := TAlphaColorRec(M.GetPixel(I, J)).A;
                if TAlphaColorRec(C).A < 255 then begin  // << don't ask me why we need to do this :(
                  var ratio: single := TAlphaColorRec(C).A / 255;
                  TAlphaColorRec(C).R := round(TAlphaColorRec(C).R * ratio);
                  TAlphaColorRec(C).G := round(TAlphaColorRec(C).G * ratio);
                  TAlphaColorRec(C).B := round(TAlphaColorRec(C).B * ratio);
                end;
                D.SetPixel(I, J, C);
              end;
          finally
            aMask.Unmap(M);
          end;
        finally
          LBitmap.Unmap(B);
        end;
      finally
        Result.Unmap(D);
      end;

    except
      AlFreeAndNil(Result);
      raise;
    end;

  finally
    AlFreeAndNil(LBitmap);
  end;
end;

{*******************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToBitmap(const AStream: TStream; const AMask: TBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndMaskAndBlurToBitmap(LBitmap, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*********************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToBitmap(const AResName: String; const AMask: TBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToBitmap(LStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{******************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToBitmap(const AFileName: String; const AMask: TBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndMaskAndBlurToBitmap(LBitmap, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{****************************************************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToDrawable(const AStream: TStream; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkImage(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkSurface(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkSurface(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGContextRef(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToBitmap(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{******************************************************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToDrawable(const AResName: String; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToSkImage(AResName, AMask, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToSkSurface(AResName, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToSkSurface(AResName, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToJBitmap(AResName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToCGContextRef(AResName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToBitmap(AResName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{***************************************************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToDrawable(const AFileName: String; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToSkImage(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToSkSurface(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToSkSurface(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToJBitmap(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToCGContextRef(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToBitmap(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndPlaceIntoToSkSurface(const AImage: sk_image_t; const W, H: single): sk_surface_t;
begin
  var LSrcRect := TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage));
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(AImage, LDestRect.Width, LDestRect.Height);
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndPlaceIntoToSkSurface(const AStream: TStream; const W, H: single): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndPlaceIntoToSkSurface(LImage, W, H);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndPlaceIntoToSkSurface(const AResName: String; const W, H: single): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoToSkSurface(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndPlaceIntoToSkSurface(const AFileName: String; const W, H: single): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndPlaceIntoToSkSurface(LImage, W, H);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndPlaceIntoToSkImage(const AStream: TStream; const W, H: single): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndPlaceIntoToSkSurface(LImage, W, H);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndPlaceIntoToSkImage(const AResName: String; const W, H: single): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoToSkImage(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndPlaceIntoToSkImage(const AFileName: String; const W, H: single): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndPlaceIntoToSkSurface(LImage, W, H);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndPlaceIntoToJBitmap(const ABitmap: JBitmap; const W, H: single): JBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.GetWidth, ABitmap.Getheight);
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(ABitmap, LDestRect.Width, LDestRect.Height);
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndPlaceIntoToJBitmap(const AStream: TStream; const W, H: single): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndPlaceIntoToJBitmap(LBitmap, W, H);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndPlaceIntoToJBitmap(const AResName: String; const W, H: single): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoToJBitmap(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndPlaceIntoToJBitmap(const AFileName: String; const W, H: single): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndPlaceIntoToJBitmap(LBitmap, W, H);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndPlaceIntoToCGContextRef(const AImage: ALOSImage; const W, H: single): CGContextRef;
begin
  var LSrcRect := TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage));
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(AImage, LDestRect.Width, LDestRect.Height);
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndPlaceIntoToCGContextRef(const AStream: TStream; const W, H: single): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndPlaceIntoToCGContextRef(LImage, W, H);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndPlaceIntoToCGContextRef(const AResName: String; const W, H: single): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoToCGContextRef(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndPlaceIntoToCGContextRef(const AFileName: String; const W, H: single): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndPlaceIntoToCGContextRef(LImage, W, H);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndPlaceIntoToCGImageRef(const AStream: TStream; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndPlaceIntoToCGContextRef(AStream, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndPlaceIntoToCGImageRef(const AResName: String; const W, H: single): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoToCGImageRef(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndPlaceIntoToCGImageRef(const AFileName: String; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndPlaceIntoToCGContextRef(AFileName, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{*************************************************************************************************}
function ALLoadFromBitmapAndPlaceIntoToBitmap(const ABitmap: TBitmap; const W, H: single): TBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.Width, ABitmap.height);
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromBitmapAndFitIntoAndCropToBitmap(ABitmap, LDestRect.Width, LDestRect.Height);
end;

{*************************************************************************************************}
function ALLoadFromStreamAndPlaceIntoToBitmap(const AStream: TStream; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndPlaceIntoToBitmap(LBitmap, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{***************************************************************************************************}
function ALLoadFromResourceAndPlaceIntoToBitmap(const AResName: String; const W, H: single): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoToBitmap(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{************************************************************************************************}
function ALLoadFromFileAndPlaceIntoToBitmap(const AFileName: String; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndPlaceIntoToBitmap(LBitmap, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*******************************************************************************************************}
function ALLoadFromStreamAndPlaceIntoToDrawable(const AStream: TStream; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndPlaceIntoToSkImage(AStream, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndPlaceIntoToSkSurface(AStream, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndPlaceIntoToSkSurface(AStream, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndPlaceIntoToJBitmap(AStream, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndPlaceIntoToCGContextRef(AStream, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndPlaceIntoToBitmap(AStream, W, H);
  {$ENDIF}
end;

{*********************************************************************************************************}
function ALLoadFromResourceAndPlaceIntoToDrawable(const AResName: String; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndPlaceIntoToSkImage(AResName, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndPlaceIntoToSkSurface(AResName, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndPlaceIntoToSkSurface(AResName, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndPlaceIntoToJBitmap(AResName, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndPlaceIntoToCGContextRef(AResName, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndPlaceIntoToBitmap(AResName, W, H);
  {$ENDIF}
end;

{******************************************************************************************************}
function ALLoadFromFileAndPlaceIntoToDrawable(const AFileName: String; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndPlaceIntoToSkImage(AFileName, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndPlaceIntoToSkSurface(AFileName, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndPlaceIntoToSkSurface(AFileName, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndPlaceIntoToJBitmap(AFileName, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndPlaceIntoToCGContextRef(AFileName, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndPlaceIntoToBitmap(AFileName, W, H);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndPlaceIntoAndBlurToSkSurface(const AImage: sk_image_t; const W, H: single; const ABlurRadius: single): sk_surface_t;
begin
  var LSrcRect := TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage));
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(AImage, LDestRect.Width, LDestRect.Height, ABlurRadius);
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndPlaceIntoAndBlurToSkSurface(const AStream: TStream; const W, H: single; const ABlurRadius: single): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndPlaceIntoAndBlurToSkSurface(LImage, W, H, ABlurRadius);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndPlaceIntoAndBlurToSkSurface(const AResName: String; const W, H: single; const ABlurRadius: single): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoAndBlurToSkSurface(LStream, W, H, ABlurRadius);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndPlaceIntoAndBlurToSkSurface(const AFileName: String; const W, H: single; const ABlurRadius: single): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndPlaceIntoAndBlurToSkSurface(LImage, W, H, ABlurRadius);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndPlaceIntoAndBlurToSkImage(const AStream: TStream; const W, H: single; const ABlurRadius: single): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndPlaceIntoAndBlurToSkSurface(LImage, W, H, ABlurRadius);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndPlaceIntoAndBlurToSkImage(const AResName: String; const W, H: single; const ABlurRadius: single): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoAndBlurToSkImage(LStream, W, H, ABlurRadius);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndPlaceIntoAndBlurToSkImage(const AFileName: String; const W, H: single; const ABlurRadius: single): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndPlaceIntoAndBlurToSkSurface(LImage, W, H, ABlurRadius);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndPlaceIntoAndBlurToJBitmap(const ABitmap: JBitmap; const W, H: single; const ABlurRadius: single): JBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.GetWidth, ABitmap.Getheight);
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(ABitmap, LDestRect.Width, LDestRect.Height, ABlurRadius);
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndPlaceIntoAndBlurToJBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndPlaceIntoAndBlurToJBitmap(LBitmap, W, H, ABlurRadius);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndPlaceIntoAndBlurToJBitmap(const AResName: String; const W, H: single; const ABlurRadius: single): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoAndBlurToJBitmap(LStream, W, H, ABlurRadius);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndPlaceIntoAndBlurToJBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndPlaceIntoAndBlurToJBitmap(LBitmap, W, H, ABlurRadius);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndPlaceIntoAndBlurToCGContextRef(const AImage: ALOSImage; const W, H: single; const ABlurRadius: single): CGContextRef;
begin
  var LSrcRect := TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage));
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromOSImageAndFitIntoAndCropAndBlurToCGContextRef(AImage, LDestRect.Width, LDestRect.Height, ABlurRadius);
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndPlaceIntoAndBlurToCGContextRef(const AStream: TStream; const W, H: single; const ABlurRadius: single): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndPlaceIntoAndBlurToCGContextRef(LImage, W, H, ABlurRadius);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndPlaceIntoAndBlurToCGContextRef(const AResName: String; const W, H: single; const ABlurRadius: single): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoAndBlurToCGContextRef(LStream, W, H, ABlurRadius);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndPlaceIntoAndBlurToCGContextRef(const AFileName: String; const W, H: single; const ABlurRadius: single): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndPlaceIntoAndBlurToCGContextRef(LImage, W, H, ABlurRadius);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndPlaceIntoAndBlurToCGImageRef(const AStream: TStream; const W, H: single; const ABlurRadius: single): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndPlaceIntoAndBlurToCGContextRef(AStream, W, H, ABlurRadius);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndPlaceIntoAndBlurToCGImageRef(const AResName: String; const W, H: single; const ABlurRadius: single): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoAndBlurToCGImageRef(LStream, W, H, ABlurRadius);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndPlaceIntoAndBlurToCGImageRef(const AFileName: String; const W, H: single; const ABlurRadius: single): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndPlaceIntoAndBlurToCGContextRef(AFileName, W, H, ABlurRadius);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{***********************************************************************************************************************************}
function ALLoadFromBitmapAndPlaceIntoAndBlurToBitmap(const ABitmap: TBitmap; const W, H: single; const ABlurRadius: single): TBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.Width, ABitmap.height);
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(ABitmap, LDestRect.Width, LDestRect.Height, ABlurRadius);
end;

{***********************************************************************************************************************************}
function ALLoadFromStreamAndPlaceIntoAndBlurToBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndPlaceIntoAndBlurToBitmap(LBitmap, W, H, ABlurRadius);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*************************************************************************************************************************************}
function ALLoadFromResourceAndPlaceIntoAndBlurToBitmap(const AResName: String; const W, H: single; const ABlurRadius: single): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoAndBlurToBitmap(LStream, W, H, ABlurRadius);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**********************************************************************************************************************************}
function ALLoadFromFileAndPlaceIntoAndBlurToBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndPlaceIntoAndBlurToBitmap(LBitmap, W, H, ABlurRadius);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*****************************************************************************************************************************************}
function ALLoadFromStreamAndPlaceIntoAndBlurToDrawable(const AStream: TStream; const W, H: single; const ABlurRadius: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndPlaceIntoAndBlurToSkImage(AStream, W, H, ABlurRadius);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndPlaceIntoAndBlurToSkSurface(AStream, W, H, ABlurRadius);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndPlaceIntoAndBlurToSkSurface(AStream, W, H, ABlurRadius);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndPlaceIntoAndBlurToJBitmap(AStream, W, H, ABlurRadius);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndPlaceIntoAndBlurToCGContextRef(AStream, W, H, ABlurRadius);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndPlaceIntoAndBlurToBitmap(AStream, W, H, ABlurRadius);
  {$ENDIF}
end;

{*******************************************************************************************************************************************}
function ALLoadFromResourceAndPlaceIntoAndBlurToDrawable(const AResName: String; const W, H: single; const ABlurRadius: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndPlaceIntoAndBlurToSkImage(AResName, W, H, ABlurRadius);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndPlaceIntoAndBlurToSkSurface(AResName, W, H, ABlurRadius);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndPlaceIntoAndBlurToSkSurface(AResName, W, H, ABlurRadius);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndPlaceIntoAndBlurToJBitmap(AResName, W, H, ABlurRadius);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndPlaceIntoAndBlurToCGContextRef(AResName, W, H, ABlurRadius);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndPlaceIntoAndBlurToBitmap(AResName, W, H, ABlurRadius);
  {$ENDIF}
end;

{****************************************************************************************************************************************}
function ALLoadFromFileAndPlaceIntoAndBlurToDrawable(const AFileName: String; const W, H: single; const ABlurRadius: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndPlaceIntoAndBlurToSkImage(AFileName, W, H, ABlurRadius);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndPlaceIntoAndBlurToSkSurface(AFileName, W, H, ABlurRadius);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndPlaceIntoAndBlurToSkSurface(AFileName, W, H, ABlurRadius);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndPlaceIntoAndBlurToJBitmap(AFileName, W, H, ABlurRadius);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndPlaceIntoAndBlurToCGContextRef(AFileName, W, H, ABlurRadius);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndPlaceIntoAndBlurToBitmap(AFileName, W, H, ABlurRadius);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndStretchToSkSurface(const AImage: sk_image_t; const W, H: single): sk_surface_t;
begin
  var LSrcRect := TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage));
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LDestRectF := TRectF.Create(LDestRect);

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
    sk4d_canvas_draw_image_rect(
      LCanvas, // self: sk_canvas_t;
      AImage, // const image: sk_image_t;
      @LSrcRect, // const src: psk_rect_t;
      @LDestRectF,  // const dest: psk_rect_t;
      @LSamplingoptions, // const sampling: psk_samplingoptions_t;
      LPaint, // const paint: sk_paint_t;
      FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndStretchToSkSurface(const AStream: TStream; const W, H: single): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndStretchToSkSurface(LImage, W, H);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndStretchToSkSurface(const AResName: String; const W, H: single): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndStretchToSkSurface(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndStretchToSkSurface(const AFileName: String; const W, H: single): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndStretchToSkSurface(LImage, W, H);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndStretchToSkImage(const AStream: TStream; const W, H: single): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndStretchToSkSurface(LImage, W, H);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndStretchToSkImage(const AResName: String; const W, H: single): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndStretchToSkImage(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndStretchToSkImage(const AFileName: String; const W, H: single): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndStretchToSkSurface(LImage, W, H);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndStretchToJBitmap(const ABitmap: JBitmap; const W, H: single): JBitmap;
begin
  var LSrcRect := TRect.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight);
  var LDestRect := TrectF.Create(0, 0, W, H).Round;

  var LMatrix := TJMatrix.JavaClass.init;
  LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
  result := TJBitmap.JavaClass.createBitmap(ABitmap{src}, LSrcRect.Left{X}, LSrcRect.top{Y}, LSrcRect.width{Width}, LSrcRect.height{height}, LMatrix{m}, True{filter});
  LMatrix := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndStretchToJBitmap(const AStream: TStream; const W, H: single): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndStretchToJBitmap(LBitmap, W, H);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndStretchToJBitmap(const AResName: String; const W, H: single): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndStretchToJBitmap(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndStretchToJBitmap(const AFileName: String; const W, H: single): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndStretchToJBitmap(LBitmap, W, H);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndStretchToCGContextRef(const AImage: ALOSImage; const W, H: single): CGContextRef;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  //-----
  Result := ALCreateCGContextRef(LDestRect.Width, LDestRect.Height);
  CGContextDrawImage(
    Result, // c: The graphics context in which to draw the image.
    ALLowerLeftCGRect(
      TpointF.Create(0,0),
      LDestRect.width,
      LDestRect.Height,
      LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndStretchToCGContextRef(const AStream: TStream; const W, H: single): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndStretchToCGContextRef(LImage, W, H);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndStretchToCGContextRef(const AResName: String; const W, H: single): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndStretchToCGContextRef(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndStretchToCGContextRef(const AFileName: String; const W, H: single): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndStretchToCGContextRef(LImage, W, H);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndStretchToCGImageRef(const AStream: TStream; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndStretchToCGContextRef(AStream, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndStretchToCGImageRef(const AResName: String; const W, H: single): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndStretchToCGImageRef(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndStretchToCGImageRef(const AFileName: String; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndStretchToCGContextRef(AFileName, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{***********************************************************************************************}
function ALLoadFromBitmapAndStretchToBitmap(const ABitmap: TBitmap; const W, H: single): TBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.width, ABitmap.height);
  var LDestRect := TrectF.Create(0, 0, w, h).Round;

  Result := TBitmap.Create(LDestRect.Width,LDestRect.Height);
  try

    if Result.Canvas.BeginScene then
    try
      Result.Canvas.DrawBitmap(
        ABitmap, // const ABitmap: TBitmap;
        LSrcRect, //const SrcRect,
        LDestRect, //const DstRect: TRectF;
        1, //const AOpacity: Single;
        false); // const HighSpeed: Boolean => disable interpolation
    finally
      Result.Canvas.EndScene;
    end;

  except
    AlFreeAndNil(Result);
    raise;
  end;
end;

{***********************************************************************************************}
function ALLoadFromStreamAndStretchToBitmap(const AStream: TStream; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndStretchToBitmap(LBitmap, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*************************************************************************************************}
function ALLoadFromResourceAndStretchToBitmap(const AResName: String; const W, H: single): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndStretchToBitmap(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**********************************************************************************************}
function ALLoadFromFileAndStretchToBitmap(const AFileName: String; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndStretchToBitmap(LBitmap, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*****************************************************************************************************}
function ALLoadFromStreamAndStretchToDrawable(const AStream: TStream; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndStretchToSkImage(AStream, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndStretchToSkSurface(AStream, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndStretchToSkSurface(AStream, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndStretchToJBitmap(AStream, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndStretchToCGContextRef(AStream, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndStretchToBitmap(AStream, W, H);
  {$ENDIF}
end;

{*******************************************************************************************************}
function ALLoadFromResourceAndStretchToDrawable(const AResName: String; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndStretchToSkImage(AResName, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndStretchToSkSurface(AResName, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndStretchToSkSurface(AResName, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndStretchToJBitmap(AResName, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndStretchToCGContextRef(AResName, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndStretchToBitmap(AResName, W, H);
  {$ENDIF}
end;

{****************************************************************************************************}
function ALLoadFromFileAndStretchToDrawable(const AFileName: String; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndStretchToSkImage(AFileName, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndStretchToSkSurface(AFileName, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndStretchToSkSurface(AFileName, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndStretchToJBitmap(AFileName, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndStretchToCGContextRef(AFileName, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndStretchToBitmap(AFileName, W, H);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndWrapToSkSurface(const AImage: sk_image_t; const AWrapMode: TALImageWrapMode; const W, H: single): sk_surface_t;
begin
  case AWrapMode of
    TALImageWrapMode.Fit: Result := ALLoadFromSkImageAndFitIntoToSkSurface(AImage, W, H);
    TALImageWrapMode.Stretch: Result := ALLoadFromSkImageAndStretchToSkSurface(AImage, W, H);
    TALImageWrapMode.Place: Result := ALLoadFromSkImageAndPlaceIntoToSkSurface(AImage, W, H);
    TALImageWrapMode.FitAndCrop: Result := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(AImage, W, H);
    else Raise exception.Create('Error 4CE56031-47CC-4532-ABC2-49C939A186A6')
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndWrapToSkSurface(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndWrapToSkSurface(LImage, AWrapMode, W, H);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndWrapToSkSurface(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndWrapToSkSurface(LStream, AWrapMode, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndWrapToSkSurface(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndWrapToSkSurface(LImage, AWrapMode, W, H);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndWrapToSkImage(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndWrapToSkSurface(LImage, AWrapMode, W, H);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndWrapToSkImage(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndWrapToSkImage(LStream, AWrapMode, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndWrapToSkImage(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndWrapToSkSurface(LImage, AWrapMode, W, H);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndWrapToJBitmap(const ABitmap: JBitmap; const AWrapMode: TALImageWrapMode; const W, H: single): JBitmap;
begin
  case AWrapMode of
    TALImageWrapMode.Fit: Result := ALLoadFromJBitmapAndFitIntoToJBitmap(ABitmap, W, H);
    TALImageWrapMode.Stretch: Result := ALLoadFromJBitmapAndStretchToJBitmap(ABitmap, W, H);
    TALImageWrapMode.Place: Result := ALLoadFromJBitmapAndPlaceIntoToJBitmap(ABitmap, W, H);
    TALImageWrapMode.FitAndCrop: Result := ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(ABitmap, W, H);
    else Raise exception.Create('Error AB4C9111-6649-45D7-8116-70758938CD47')
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndWrapToJBitmap(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndWrapToJBitmap(LBitmap, AWrapMode, W, H);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndWrapToJBitmap(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndWrapToJBitmap(LStream, AWrapMode, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndWrapToJBitmap(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndWrapToJBitmap(LBitmap, AWrapMode, W, H);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndWrapToCGContextRef(const AImage: ALOSImage; const AWrapMode: TALImageWrapMode; const W, H: single): CGContextRef;
begin
  case AWrapMode of
    TALImageWrapMode.Fit: Result := ALLoadFromOSImageAndFitIntoToCGContextRef(AImage, W, H);
    TALImageWrapMode.Stretch: Result := ALLoadFromOSImageAndStretchToCGContextRef(AImage, W, H);
    TALImageWrapMode.Place: Result := ALLoadFromOSImageAndPlaceIntoToCGContextRef(AImage, W, H);
    TALImageWrapMode.FitAndCrop: Result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(AImage, W, H);
    else Raise exception.Create('Error 71B29EF2-207C-479A-B891-42075C545EA8')
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndWrapToCGContextRef(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndWrapToCGContextRef(LImage, AWrapMode, W, H);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndWrapToCGContextRef(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndWrapToCGContextRef(LStream, AWrapMode, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndWrapToCGContextRef(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndWrapToCGContextRef(LImage, AWrapMode, W, H);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndWrapToCGImageRef(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndWrapToCGContextRef(AStream, AWrapMode, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndWrapToCGImageRef(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndWrapToCGImageRef(LStream, AWrapMode, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndWrapToCGImageRef(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndWrapToCGContextRef(AFileName, AWrapMode, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{*******************************************************************************************************************************}
function ALLoadFromBitmapAndWrapToBitmap(const ABitmap: TBitmap; const AWrapMode: TALImageWrapMode; const W, H: single): TBitmap;
begin
  case AWrapMode of
    TALImageWrapMode.Fit: Result := ALLoadFromBitmapAndFitIntoToBitmap(ABitmap, W, H);
    TALImageWrapMode.Stretch: Result := ALLoadFromBitmapAndStretchToBitmap(ABitmap, W, H);
    TALImageWrapMode.Place: Result := ALLoadFromBitmapAndPlaceIntoToBitmap(ABitmap, W, H);
    TALImageWrapMode.FitAndCrop: Result := ALLoadFromBitmapAndFitIntoAndCropToBitmap(ABitmap, W, H);
    else Raise exception.Create('Error 68B96273-FB54-47B5-BD9B-A2DD75BEE07F')
  end;
end;

{*******************************************************************************************************************************}
function ALLoadFromStreamAndWrapToBitmap(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndWrapToBitmap(LBitmap, AWrapMode, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*********************************************************************************************************************************}
function ALLoadFromResourceAndWrapToBitmap(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndWrapToBitmap(LStream, AWrapMode, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{******************************************************************************************************************************}
function ALLoadFromFileAndWrapToBitmap(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndWrapToBitmap(LBitmap, AWrapMode, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*************************************************************************************************************************************}
function ALLoadFromStreamAndWrapToDrawable(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndWrapToSkImage(AStream, AWrapMode, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndWrapToSkSurface(AStream, AWrapMode, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndWrapToSkSurface(AStream, AWrapMode, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndWrapToJBitmap(AStream, AWrapMode, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndWrapToCGContextRef(AStream, AWrapMode, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndWrapToBitmap(AStream, AWrapMode, W, H);
  {$ENDIF}
end;

{***************************************************************************************************************************************}
function ALLoadFromResourceAndWrapToDrawable(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndWrapToSkImage(AResName, AWrapMode, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndWrapToSkSurface(AResName, AWrapMode, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndWrapToSkSurface(AResName, AWrapMode, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndWrapToJBitmap(AResName, AWrapMode, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndWrapToCGContextRef(AResName, AWrapMode, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndWrapToBitmap(AResName, AWrapMode, W, H);
  {$ENDIF}
end;

{************************************************************************************************************************************}
function ALLoadFromFileAndWrapToDrawable(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndWrapToSkImage(AFileName, AWrapMode, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndWrapToSkSurface(AFileName, AWrapMode, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndWrapToSkSurface(AFileName, AWrapMode, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndWrapToJBitmap(AFileName, AWrapMode, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndWrapToCGContextRef(AFileName, AWrapMode, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndWrapToBitmap(AFileName, AWrapMode, W, H);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndNormalizeOrientationToSkSurface(const AImage: sk_image_t; const AExifOrientationInfo: TALExifOrientationInfo): sk_surface_t;
begin
  //No need to care about AExifOrientationInfo with skimage
  //because skimage already loaded the image with the good orientation.
  var LRect := Trect.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage));
  var LRectF := TRectF.Create(LRect);

  Result := ALCreateSkSurface(LRect.Width, LRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
    sk4d_canvas_draw_image_rect(
      LCanvas, // self: sk_canvas_t;
      AImage, // const image: sk_image_t;
      @LRectF, // const src: psk_rect_t;
      @LRectF,  // const dest: psk_rect_t;
      @LSamplingoptions, // const sampling: psk_samplingoptions_t;
      LPaint, // const paint: sk_paint_t;
      FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndNormalizeOrientationToSkSurface(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndNormalizeOrientationToSkSurface(LImage, AExifOrientationInfo);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndNormalizeOrientationToSkSurface(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToSkSurface(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndNormalizeOrientationToSkSurface(const AFileName: String): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndNormalizeOrientationToSkSurface(LImage, AlGetExifOrientationInfo(aFileName));
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndNormalizeOrientationToSkImage(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndNormalizeOrientationToSkSurface(LImage, AExifOrientationInfo);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndNormalizeOrientationToSkImage(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToSkImage(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndNormalizeOrientationToSkImage(const AFileName: String): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndNormalizeOrientationToSkSurface(LImage, AlGetExifOrientationInfo(aFileName));
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndNormalizeOrientationToJBitmap(const ABitmap: JBitmap; const AExifOrientationInfo: TALExifOrientationInfo): JBitmap;
begin
  var LMatrix := TJMatrix.JavaClass.init;
  case aExifOrientationInfo of
    TalExifOrientationInfo.NORMAL:;
    TalExifOrientationInfo.FLIP_HORIZONTAL: LMatrix.setScale(-1, 1);
    TalExifOrientationInfo.ROTATE_180: LMatrix.setRotate(180);
    TalExifOrientationInfo.FLIP_VERTICAL: begin
                                            LMatrix.setRotate(180);
                                            LMatrix.postScale(-1, 1);
                                          end;
    TalExifOrientationInfo.TRANSPOSE: begin
                                        LMatrix.setRotate(90);
                                        LMatrix.postScale(-1, 1);
                                      end;
    TalExifOrientationInfo.ROTATE_90: LMatrix.setRotate(90);
    TalExifOrientationInfo.TRANSVERSE: begin
                                         LMatrix.setRotate(-90);
                                         LMatrix.postScale(-1, 1);
                                       end;
    TalExifOrientationInfo.ROTATE_270: LMatrix.setRotate(-90);
    TalExifOrientationInfo.UNDEFINED:;
    else
      raise exception.Create('Error 49B8D091-6743-426E-8E2F-0802A9B681E7');
  end;
  Result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, 0{X}, 0{Y}, aBitmap.getwidth{Width}, aBitmap.getheight{height}, LMatrix{m}, True{filter});
  LMatrix := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndNormalizeOrientationToJBitmap(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndNormalizeOrientationToJBitmap(LBitmap, AExifOrientationInfo);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndNormalizeOrientationToJBitmap(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToJBitmap(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndNormalizeOrientationToJBitmap(const AFileName: String): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndNormalizeOrientationToJBitmap(LBitmap, AlGetExifOrientationInfo(aFileName));
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndNormalizeOrientationToCGContextRef(const AImage: ALOSImage; const AExifOrientationInfo: TALExifOrientationInfo): CGContextRef;
begin
  var w, h: integer;
  if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270,{UIImageOrientationLeft}
                              TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                              TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                              TalExifOrientationInfo.TRANSVERSE {UIImageOrientationRightMirrored}] then begin
    w := ALOSImageGetHeight(AImage);
    h := ALOSImageGetWidth(AImage);
  end
  else begin
    w := ALOSImageGetWidth(AImage);
    h := ALOSImageGetHeight(AImage);
  end;
  var LMatrix := CGAffineTransformIdentity;
  case aExifOrientationInfo of

    //UIImageOrientationUp: The original pixel data matches the image's intended display orientation.
    TalExifOrientationInfo.NORMAL:;

    //UIImageOrientationUpMirrored: The image has been horizontally flipped from the orientation of its original pixel data
    TalExifOrientationInfo.FLIP_HORIZONTAL: begin
                                              //LMatrix.setScale(-1, 1);
                                              LMatrix := CGAffineTransformTranslate(LMatrix, w, 0);
                                              LMatrix := CGAffineTransformScale(LMatrix, -1, 1);
                                            end;

    //UIImageOrientationDown: The image has been rotated 180° from the orientation of its original pixel data.
    TalExifOrientationInfo.ROTATE_180: begin
                                         //LMatrix.setRotate(180);
                                         LMatrix := CGAffineTransformTranslate(LMatrix, w, h);
                                         LMatrix := CGAffineTransformRotate(LMatrix, degToRad(180));
                                       end;

    //UIImageOrientationDownMirrored: The image has been vertically flipped from the orientation of its original pixel data.
    TalExifOrientationInfo.FLIP_VERTICAL: begin
                                            //LMatrix.setRotate(180);
                                            //LMatrix.postScale(-1, 1);
                                            LMatrix := CGAffineTransformTranslate(LMatrix, w, h);
                                            LMatrix := CGAffineTransformRotate(LMatrix, degToRad(180));
                                            LMatrix := CGAffineTransformTranslate(LMatrix, w, 0);
                                            LMatrix := CGAffineTransformScale(LMatrix, -1, 1);
                                          end;

    //UIImageOrientationLeftMirrored: The image has been rotated 90° clockwise and flipped horizontally from the orientation of its original pixel data.
    TalExifOrientationInfo.TRANSPOSE: begin
                                        //LMatrix.setRotate(90);
                                        //LMatrix.postScale(-1, 1);
                                        LMatrix := CGAffineTransformTranslate(LMatrix, w, 0);
                                        LMatrix := CGAffineTransformRotate(LMatrix, degToRad(90));
                                        LMatrix := CGAffineTransformTranslate(LMatrix, h, 0);
                                        LMatrix := CGAffineTransformScale(LMatrix, -1, 1);
                                      end;

    //UIImageOrientationRight: The image has been rotated 90° clockwise from the orientation of its original pixel data.
    TalExifOrientationInfo.ROTATE_90: begin
                                        //LMatrix.setRotate(90);
                                        LMatrix := CGAffineTransformTranslate(LMatrix, 0, h);
                                        LMatrix := CGAffineTransformRotate(LMatrix, -degToRad(90));
                                      end;

    //UIImageOrientationRightMirrored: The image has been rotated 90° COUNTERclockwise and flipped horizontally from the orientation of its original pixel data.
    TalExifOrientationInfo.TRANSVERSE: begin
                                         //LMatrix.setRotate(-90);
                                         //LMatrix.postScale(-1, 1);
                                         LMatrix := CGAffineTransformTranslate(LMatrix, 0, h);
                                         LMatrix := CGAffineTransformRotate(LMatrix, -degToRad(90));
                                         LMatrix := CGAffineTransformTranslate(LMatrix, h, 0);
                                         LMatrix := CGAffineTransformScale(LMatrix, -1, 1);
                                       end;

    //UIImageOrientationLeft: The image has been rotated 90° COUNTERclockwise from the orientation of its original pixel data.
    TalExifOrientationInfo.ROTATE_270: begin
                                         //LMatrix.setRotate(-90);
                                         LMatrix := CGAffineTransformTranslate(LMatrix, w, 0);
                                         LMatrix := CGAffineTransformRotate(LMatrix, degToRad(90));
                                       end;

    //UNDEFINED
    TalExifOrientationInfo.UNDEFINED:;

    //Error
    else
      raise exception.Create('Error 6205CE05-058D-46C0-A3C8-5491134178D6');

  end;

  Result := ALCreateCGContextRef(W, H);
  CGContextConcatCTM(Result, LMatrix);
  if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270, {UIImageOrientationLeft}
                              TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                              TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                              TalExifOrientationInfo.TRANSVERSE{UIImageOrientationRightMirrored}] then
    CGContextDrawImage(
      Result, // c: The graphics context in which to draw the image.
      CGRectMake(0, 0, h, w), // rect The location and dimensions in user space of the bounding box in which to draw the image.
      ALOSImageGetCgImage(AImage)) // image The image to draw.
  else
    CGContextDrawImage(
      Result, // c: The graphics context in which to draw the image.
      CGRectMake(0, 0, w, h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
      ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndNormalizeOrientationToCGContextRef(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndNormalizeOrientationToCGContextRef(LImage, AExifOrientationInfo);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndNormalizeOrientationToCGContextRef(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToCGContextRef(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndNormalizeOrientationToCGContextRef(const AFileName: String): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndNormalizeOrientationToCGContextRef(LImage, AlGetExifOrientationInfo(aFileName));
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndNormalizeOrientationToCGImageRef(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndNormalizeOrientationToCGContextRef(AStream, AExifOrientationInfo);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndNormalizeOrientationToCGImageRef(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToCGImageRef(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndNormalizeOrientationToCGImageRef(const AFileName: String): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndNormalizeOrientationToCGContextRef(AFileName);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{********************************************************************************************************************************************}
function ALLoadFromStreamAndNormalizeOrientationToBitmap(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): TBitmap;
begin
  Result := Tbitmap.CreateFromStream(aStream);
  case aExifOrientationInfo of
    TalExifOrientationInfo.NORMAL: exit;
    TalExifOrientationInfo.FLIP_HORIZONTAL: Result.FlipHorizontal;
    TalExifOrientationInfo.ROTATE_180: Result.Rotate(180);
    TalExifOrientationInfo.FLIP_VERTICAL: Result.FlipVertical;
    TalExifOrientationInfo.TRANSPOSE: begin
                                        Result.Rotate(90);
                                        Result.FlipHorizontal;
                                      end;
    TalExifOrientationInfo.ROTATE_90: Result.Rotate(90);
    TalExifOrientationInfo.TRANSVERSE: begin
                                         Result.Rotate(-90);
                                         Result.FlipHorizontal;
                                       end;
    TalExifOrientationInfo.ROTATE_270: Result.Rotate(270);
    TalExifOrientationInfo.UNDEFINED: exit;
    else
      raise exception.Create('Error 1C368047-00C4-4F68-8C77-56956FABCF92');
  end;
end;

{**********************************************************************************************************************************************}
function ALLoadFromResourceAndNormalizeOrientationToBitmap(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToBitmap(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;

{***************************************************************************************}
function ALLoadFromFileAndNormalizeOrientationToBitmap(const AFileName: String): TBitmap;
begin
  Result := Tbitmap.CreateFromFile(AFileName);
  case AlGetExifOrientationInfo(aFileName) of
    TalExifOrientationInfo.NORMAL: exit;
    TalExifOrientationInfo.FLIP_HORIZONTAL: Result.FlipHorizontal;
    TalExifOrientationInfo.ROTATE_180: Result.Rotate(180);
    TalExifOrientationInfo.FLIP_VERTICAL: Result.FlipVertical;
    TalExifOrientationInfo.TRANSPOSE: begin
                                        Result.Rotate(90);
                                        Result.FlipHorizontal;
                                      end;
    TalExifOrientationInfo.ROTATE_90: Result.Rotate(90);
    TalExifOrientationInfo.TRANSVERSE: begin
                                         Result.Rotate(-90);
                                         Result.FlipHorizontal;
                                       end;
    TalExifOrientationInfo.ROTATE_270: Result.Rotate(270);
    TalExifOrientationInfo.UNDEFINED: exit;
    else
      raise exception.Create('Error 2F09739F-4CB7-46DC-A665-C64D626DD1D7');
  end;
end;

{**************************************************************************************************************************************************}
function ALLoadFromStreamAndNormalizeOrientationToDrawable(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndNormalizeOrientationToSkImage(AStream, AExifOrientationInfo);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndNormalizeOrientationToSkSurface(AStream, AExifOrientationInfo);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndNormalizeOrientationToSkSurface(AStream, AExifOrientationInfo);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndNormalizeOrientationToJBitmap(AStream, AExifOrientationInfo);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndNormalizeOrientationToCGContextRef(AStream, AExifOrientationInfo);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndNormalizeOrientationToBitmap(AStream, AExifOrientationInfo);
  {$ENDIF}
end;

{****************************************************************************************************************************************************}
function ALLoadFromResourceAndNormalizeOrientationToDrawable(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndNormalizeOrientationToSkImage(AResName, AExifOrientationInfo);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndNormalizeOrientationToSkSurface(AResName, AExifOrientationInfo);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndNormalizeOrientationToSkSurface(AResName, AExifOrientationInfo);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndNormalizeOrientationToJBitmap(AResName, AExifOrientationInfo);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndNormalizeOrientationToCGContextRef(AResName, AExifOrientationInfo);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndNormalizeOrientationToBitmap(AResName, AExifOrientationInfo);
  {$ENDIF}
end;

{*********************************************************************************************}
function ALLoadFromFileAndNormalizeOrientationToDrawable(const AFileName: String): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndNormalizeOrientationToSkImage(AFileName);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndNormalizeOrientationToSkSurface(AFileName);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndNormalizeOrientationToSkSurface(AFileName);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndNormalizeOrientationToJBitmap(AFileName);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndNormalizeOrientationToCGContextRef(AFileName);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndNormalizeOrientationToBitmap(AFileName);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageToSkSurface(const AImage: sk_image_t): sk_surface_t;
begin
  var LRect := Trect.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage));
  var LRectF := TRectF.Create(LRect);

  Result := ALCreateSkSurface(LRect.Width, LRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
    sk4d_canvas_draw_image_rect(
      LCanvas, // self: sk_canvas_t;
      AImage, // const image: sk_image_t;
      @LRectF, // const src: psk_rect_t;
      @LRectF,  // const dest: psk_rect_t;
      @LSamplingoptions, // const sampling: psk_samplingoptions_t;
      LPaint, // const paint: sk_paint_t;
      FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamToSkSurface(const AStream: TStream): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageToSkSurface(LImage);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceToSkSurface(const AResName: String): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamToSkSurface(LStream);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileToSkSurface(const AFileName: String): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageToSkSurface(LImage);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamToSkImage(const AStream: TStream): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    Result := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceToSkImage(const AResName: String): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamToSkImage(LStream);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileToSkImage(const AFileName: String): sk_image_t;
begin
  Result := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamToJBitmap(const AStream: TStream): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    Result := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if Result = nil then raise Exception.create('Failed to decode bitmap from stream');
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceToJBitmap(const AResName: String): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamToJBitmap(LStream);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileToJBitmap(const AFileName: String): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  Result := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if Result = nil then raise Exception.create('Failed to load bitmap from file');
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageToCGContextRef(const AImage: ALOSImage): CGContextRef;
begin
  var LRect := Trect.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage));
  var LRectF := TRectF.Create(LRect);
  //-----
  Result := ALCreateCGContextRef(LRect.Width, LRect.Height);
  CGContextDrawImage(
    Result, // c: The graphics context in which to draw the image.
    CGRectMake(LRectF), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamToCGContextRef(const AStream: TStream): CGContextRef;
begin
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
    AStream.Position := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageToCGContextRef(LImage);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceToCGContextRef(const AResName: String): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamToCGContextRef(LStream);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileToCGContextRef(const AFileName: String): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageToCGContextRef(LImage);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamToCGImageRef(const AStream: TStream): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamToCGContextRef(AStream);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceToCGImageRef(const AResName: String): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamToCGImageRef(LStream);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileToCGImageRef(const AFileName: String): CGImageRef;
begin
  var LContextRef := ALLoadFromFileToCGContextRef(AFileName);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{*****************************************************************}
function ALLoadFromStreamToBitmap(const AStream: TStream): TBitmap;
begin
  Result := Tbitmap.CreateFromStream(aStream);
end;

{*******************************************************************}
function ALLoadFromResourceToBitmap(const AResName: String): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamToBitmap(LStream);
  finally
    ALfreeandNil(LStream);
  end;
end;

{****************************************************************}
function ALLoadFromFileToBitmap(const AFileName: String): TBitmap;
begin
  Result := Tbitmap.CreateFromFile(AFileName);
end;

{***********************************************************************}
function ALLoadFromStreamToDrawable(const AStream: TStream): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamToSkImage(AStream);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamToSkSurface(AStream);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamToSkSurface(AStream);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamToJBitmap(AStream);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamToCGContextRef(AStream);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamToBitmap(AStream);
  {$ENDIF}
end;

{*************************************************************************}
function ALLoadFromResourceToDrawable(const AResName: String): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceToSkImage(AResName);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceToSkSurface(AResName);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceToSkSurface(AResName);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceToJBitmap(AResName);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceToCGContextRef(AResName);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceToBitmap(AResName);
  {$ENDIF}
end;

{**********************************************************************}
function ALLoadFromFileToDrawable(const AFileName: String): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileToSkImage(AFileName);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileToSkSurface(AFileName);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileToSkSurface(AFileName);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileToJBitmap(AFileName);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileToCGContextRef(AFileName);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileToBitmap(AFileName);
  {$ENDIF}
end;

