{*****************}
{$IF defined(ALSkiaCanvas)}
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

{*****************}
{$IF defined(ALSkiaCanvas)}
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

{*****************}
{$IF defined(ALSkiaCanvas)}
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

{*****************}
{$IF defined(ALSkiaCanvas)}
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

{*****************}
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

{*****************}
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

{*****************}
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

{*****************}
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

{*****************}
{$IF defined(IOS)}
function ALLoadFromUIImageAndNormalizeOrientationToCGContextRef(const AImage: UIImage; const AExifOrientationInfo: TALExifOrientationInfo): CGContextRef;
begin
  var w, h: integer;
  if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270,{UIImageOrientationLeft}
                              TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                              TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                              TalExifOrientationInfo.TRANSVERSE {UIImageOrientationRightMirrored}] then begin
    w := CGImageGetHeight(AImage.cgImage);
    h := CGImageGetWidth(AImage.cgImage);
  end
  else begin
    w := CGImageGetWidth(AImage.cgImage);
    h := CGImageGetHeight(AImage.cgImage);
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
  CGContextSetInterpolationQuality(Result, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
  CGContextSetShouldAntialias(Result, True); // Sets anti-aliasing on or off for a graphics context.
  CGContextSetAllowsAntialiasing(Result, True); // Sets whether or not to allow anti-aliasing for a graphics context.
  CGContextConcatCTM(Result, LMatrix);
  if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270, {UIImageOrientationLeft}
                              TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                              TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                              TalExifOrientationInfo.TRANSVERSE{UIImageOrientationRightMirrored}] then
    CGContextDrawImage(
      Result, // c: The graphics context in which to draw the image.
      CGRectMake(0, 0, h, w), // rect The location and dimensions in user space of the bounding box in which to draw the image.
      AImage.CGImage) // image The image to draw.
  else
    CGContextDrawImage(
      Result, // c: The graphics context in which to draw the image.
      CGRectMake(0, 0, w, h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
      AImage.CGImage); // image The image to draw.
end;
{$ENDIF}

{*****************}
{$IF defined(IOS)}
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
      var LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromUIImageAndNormalizeOrientationToCGContextRef(LImage, AExifOrientationInfo);
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

{*****************}
{$IF defined(IOS)}
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

{*****************}
{$IF defined(IOS)}
function ALLoadFromFileAndNormalizeOrientationToCGContextRef(const AFileName: String): CGContextRef;
begin
  var LImage := TUIImage.Wrap(TUIImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename))); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromUIImageAndNormalizeOrientationToCGContextRef(LImage, AlGetExifOrientationInfo(aFileName));
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{*****************}
{$IF defined(IOS)}
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

{*****************}
{$IF defined(IOS)}
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

{*****************}
{$IF defined(IOS)}
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

{*****************}
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

{*****************}
function ALLoadFromResourceAndNormalizeOrientationToBitmap(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToBitmap(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************}
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

{*****************}
function ALLoadFromStreamAndNormalizeOrientationToDrawable(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): TALDrawable;
begin
  {$IF defined(ALSkiaCanvas)}
  Result := ALLoadFromStreamAndNormalizeOrientationToSkImage(AStream, AExifOrientationInfo);
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndNormalizeOrientationToJBitmap(AStream, AExifOrientationInfo);
  try
    result := ALJBitmapToTexture(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(IOS)}
  var LCGContextRef := ALLoadFromStreamAndNormalizeOrientationToCGContextRef(AStream, AExifOrientationInfo);
  try
    result := ALCGContextRefToTexture(LCGContextRef);
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndNormalizeOrientationToBitmap(AStream, AExifOrientationInfo);
  {$ENDIF}
end;

{*****************}
function ALLoadFromResourceAndNormalizeOrientationToDrawable(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): TALDrawable;
begin
  {$IF defined(ALSkiaCanvas)}
  Result := ALLoadFromResourceAndNormalizeOrientationToSkImage(AResName, AExifOrientationInfo);
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndNormalizeOrientationToJBitmap(AResName, AExifOrientationInfo);
  try
    result := ALJBitmapToTexture(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(IOS)}
  var LCGContextRef := ALLoadFromResourceAndNormalizeOrientationToCGContextRef(AResName, AExifOrientationInfo);
  try
    result := ALCGContextRefToTexture(LCGContextRef);
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndNormalizeOrientationToBitmap(AResName, AExifOrientationInfo);
  {$ENDIF}
end;

{*****************}
function ALLoadFromFileAndNormalizeOrientationToDrawable(const AFileName: String): TALDrawable;
begin
  {$IF defined(ALSkiaCanvas)}
  Result := ALLoadFromFileAndNormalizeOrientationToSkImage(AFileName);
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndNormalizeOrientationToJBitmap(AFileName);
  try
    result := ALJBitmapToTexture(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(IOS)}
  var LCGContextRef := ALLoadFromFileAndNormalizeOrientationToCGContextRef(AFileName);
  try
    result := ALCGContextRefToTexture(LCGContextRef);
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndNormalizeOrientationToBitmap(AFileName);
  {$ENDIF}
end;

