program FMXGraphicsBuilder;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  system.AnsiStrings,
  system.Classes,
  System.SysUtils,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.common;

var
  OutputInterfaceLst: TALStringListA;
  OutputImplementationLst: TALStringListA;
  CurrTemplate: AnsiString;
  CurrComment: AnsiString;
  CurrPlatform: AnsiString;
  CurrToName: AnsiString;
  CurrFunctionName: AnsiString;

procedure AddFunctionToOutPut(
            ATemplate: AnsiString;
            ALOADFROM: AnsiString;
            ATOName: AnsiString); overload;
begin
  var LOriginalAlcinoeFMXGraphicsPas: AnsiString := ALGetStringFromFile('..\..\Source\Alcinoe.FMX.Graphics.pas');

  var LTOType: AnsiString;
  if ATOName = 'SkSurface' then LTOType := 'sk_surface_t'
  else if ATOName = 'SkImage' then LTOType := 'sk_image_t'
  else if ATOName = 'JBitmap' then LTOType := 'JBitmap'
  else if ATOName = 'CGContextRef' then LTOType := 'CGContextRef'
  else if ATOName = 'CGImageRef' then LTOType := 'CGImageRef'
  else if ATOName = 'Bitmap' then LTOType := 'TBitmap'
  else if ATOName = 'Drawable' then LTOType := 'TALDrawable'
  else raise Exception.Create('Error E419BE7D-651F-4FA4-A173-0F1C1A5743D6');

  var LMASKTYPE: AnsiString;
  if ATOName = 'SkSurface' then LMASKTYPE := 'sk_image_t'
  else if ATOName = 'SkImage' then LMASKTYPE := 'sk_image_t'
  else if ATOName = 'JBitmap' then LMASKTYPE := 'JBitmap'
  else if ATOName = 'CGContextRef' then LMASKTYPE := 'CGImageRef'
  else if ATOName = 'CGImageRef' then LMASKTYPE := 'CGImageRef'
  else if ATOName = 'Bitmap' then LMASKTYPE := 'TBitmap'
  else if ATOName = 'Drawable' then LMASKTYPE := '{$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}'
  else raise Exception.Create('Error E419BE7D-651F-4FA4-A173-0F1C1A5743D6');

  var LSOURCE: AnsiString;
  if ALOADFROM = 'SkImage' then LSOURCE := 'const AImage: sk_image_t'
  else if ALOADFROM = 'JBitmap' then LSOURCE := 'const ABitmap: JBitmap'
  else if ALOADFROM = 'ALOSImage' then LSOURCE := 'const AImage: ALOSImage'
  else if ALOADFROM = 'Bitmap' then LSOURCE := 'const ABitmap: TBitmap'
  else if ALOADFROM = 'Stream' then LSOURCE := 'const AStream: TStream'
  else if ALOADFROM = 'Resource' then LSOURCE := 'const AResName: String'
  else if ALOADFROM = 'File' then LSOURCE := 'const AFileName: String'
  else raise Exception.Create('Error D90E4E88-BD16-4479-BDAA-E6DD875CC41D');

  var LExifOrientationInfo: AnsiString;
  if ALOADFROM = 'SkImage' then LExifOrientationInfo := 'const AExifOrientationInfo: TALExifOrientationInfo'
  else if ALOADFROM = 'JBitmap' then LExifOrientationInfo := 'const AExifOrientationInfo: TALExifOrientationInfo'
  else if ALOADFROM = 'ALOSImage' then LExifOrientationInfo := 'const AExifOrientationInfo: TALExifOrientationInfo'
  else if ALOADFROM = 'Bitmap' then LExifOrientationInfo := 'const AExifOrientationInfo: TALExifOrientationInfo'
  else if ALOADFROM = 'Stream' then LExifOrientationInfo := 'const AExifOrientationInfo: TALExifOrientationInfo'
  else if ALOADFROM = 'Resource' then LExifOrientationInfo := 'const AExifOrientationInfo: TALExifOrientationInfo'
  else if ALOADFROM = 'File' then LExifOrientationInfo := ''
  else raise Exception.Create('Error D90E4E88-BD16-4479-BDAA-E6DD875CC41D');
  var LIsWithExifOrientationInfo := alposA('<EXIFORIENTATIONINFO>', ATemplate) > 0;

  var LPlatform: AnsiString;
  if ATOName = 'SkSurface' then LPlatform := 'ALSkiaAvailable'
  else if ATOName = 'SkImage' then LPlatform := 'ALSkiaAvailable'
  else if ATOName = 'JBitmap' then LPlatform := 'ANDROID'
  else if ATOName = 'CGContextRef' then LPlatform := 'ALAppleOS'
  else if ATOName = 'CGImageRef' then LPlatform := 'ALAppleOS'
  else if ATOName = 'Bitmap' then LPlatform := ''
  else if ATOName = 'Drawable' then LPlatform := ''
  else raise Exception.Create('Error 93ADC011-FEB5-460E-B1EC-78901A170DE8');

  if CurrTemplate <> Atemplate then begin
    if OutputInterfaceLst.Count > 0 then begin
      OutputInterfaceLst.Add('{$ENDREGION}');
      OutputInterfaceLst.Add('');
    end;
    var LRegionName := ATemplate; // function <LOADFROM>AndFitIntoAndCropAndMaskTo<TOName>(<SOURCE>; const AMask: <TOType>): <TOType>;
    LRegionName := ALStringReplaceA(LRegionName, 'function ', '', [rfReplaceAll, RfIgnoreCase]); // <LOADFROM>AndFitIntoAndCropAndMaskTo<TOName>(<SOURCE>; const AMask: <TOType>): <TOType>;
    LRegionName := ALStringReplaceA(LRegionName, '<LOADFROM>', '', [rfReplaceAll, RfIgnoreCase]); // AndFitIntoAndCropAndMaskTo<TOName>(<SOURCE>; const AMask: <TOType>): <TOType>;
    LRegionName := ALStringReplaceA(LRegionName, 'To<TOName>', '', [rfReplaceAll, RfIgnoreCase]); // AndFitIntoAndCropAndMask(<SOURCE>; const AMask: <TOType>): <TOType>;
    LRegionName := ALStringReplaceA(LRegionName, '<TOName>', '', [rfReplaceAll, RfIgnoreCase]); // AndFitIntoAndCropAndMask(<SOURCE>; const AMask: <TOType>): <TOType>;
    var P := ALPosA('(', LRegionName);
    if P > 0 then delete(LRegionName, P, Maxint); // AndFitIntoAndCropAndMask
    LRegionName := ALStringReplaceA(LRegionName, 'And', ' and ', [rfReplaceAll, RfIgnoreCase]); // And FitInto And Crop And Mask
    LRegionName := ALStringReplaceA(LRegionName, 'To', ' to ', [rfReplaceAll, RfIgnoreCase]); // And FitInto And Crop And Mask
    LRegionName := ALStringReplaceA(LRegionName, 'In to ', 'Into', [rfReplaceAll, RfIgnoreCase]); // And FitInto And Crop And Mask
    LRegionName := 'Load' + LRegionName;
    OutputInterfaceLst.Add('{$REGION '' '+LRegionName+'''}');
    OutputInterfaceLst.Add(CurrComment);
    CurrTemplate := Atemplate;
    CurrToName := ATOName;
  end;

  if CurrPlatform <> LPlatform then begin
    if CurrPlatform <> '' then begin
      OutputInterfaceLst.Add('{$ENDIF}');
      OutputInterfaceLst.Add('');
      CurrToName := AToName;
    end;
    if LPlatform <> '' then OutputInterfaceLst.Add('{$IF defined('+LPlatform+')}');
    CurrPlatform := LPlatform;
  end;

  if CurrToName <> AToName then begin
    OutputInterfaceLst.Add('//--');
    CurrToName := AToName;
  end;

  // function <LOADFROM>AndFitIntoAndCropAndMaskTo<TOName>(<SOURCE>; const AMask: <TOType>): <TOType>;
  ATemplate := ALStringReplaceA(ATemplate, '<LOADFROM>', 'ALLoadFrom' + ALStringReplaceA(ALOADFROM, 'ALOSImage', 'OSImage', [RFIgnoreCase]), [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, '<TOName>', ATOName, [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, '<MASKTYPE>', LMASKTYPE, [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, '<TOType>', LTOType, [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, '<SOURCE>', LSOURCE, [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, '<EXIFORIENTATIONINFO>', LEXIFORIENTATIONINFO, [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, '; ;', ';', [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, '; ;', ';', [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, '; ;', ';', [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, ';;', ';', [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, ';;', ';', [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, ';;', ';', [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, '; )', ')', [rfReplaceAll, RfIgnoreCase]);
  ATemplate := ALStringReplaceA(ATemplate, ';)', ')', [rfReplaceAll, RfIgnoreCase]);

  Var LFunctionName := ATemplate; // function <LOADFROM>AndFitIntoAndCropAndMaskTo<TOName>(<SOURCE>; const AMask: <TOType>): <TOType>;
  var P := ALPosA('(', LFunctionName);
  if P > 0 then delete(LFunctionName, P, Maxint); // ALLoadFromStreamAndFitIntoAndCropAndMaskToBitmap
  if LFunctionName = CurrFunctionName then begin
    ATemplate := ATemplate + ' overload;';
    if ALposA(' overload;', OutputInterfaceLst[OutputInterfaceLst.Count - 1]) <= 0 then OutputInterfaceLst[OutputInterfaceLst.Count - 1] := OutputInterfaceLst[OutputInterfaceLst.Count - 1] + ' overload;';
  end;
  CurrFunctionName := LFunctionName;

  if ATemplate = 'function ALLoadFromBitmapAndNormalizeOrientationToBitmap(const ABitmap: TBitmap; const AExifOrientationInfo: TALExifOrientationInfo): TBitmap;' then
    exit;

  OutputInterfaceLst.Add(ATemplate);

  var Lfunction := ATemplate; // function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(const AStream: TStream; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
  Lfunction := ALStringReplaceA(Lfunction, 'function ', '', [RfIgnoreCase, rfReplaceAll]); // ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(const AStream: TStream; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
  Lfunction := ALStringReplaceA(Lfunction, 'const ', '', [RfIgnoreCase, rfReplaceAll]); // ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(AStream: TStream; AMask: JBitmap; ABlurRadius: single; XCropCenter: single = -50; YCropCenter: single = -50): JBitmap;
  Lfunction := ALStringReplaceA(Lfunction, ')', ';)', [RfIgnoreCase, rfReplaceAll]); // ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(AStream: TStream; AMask: JBitmap; ABlurRadius: single; XCropCenter: single = -50; YCropCenter: single = -50;): JBitmap;
  Lfunction := ALStringReplaceA(Lfunction, ';)}', ')}', [RfIgnoreCase, rfReplaceAll]); // ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(AStream: TStream; AMask: JBitmap; ABlurRadius: single; XCropCenter: single = -50; YCropCenter: single = -50;): JBitmap;
  var P1 := ALposA(':', Lfunction);
  While P1 > 0 do begin
    var P2 := ALposA(';',Lfunction);
    if P2 < 0 then raise Exception.Create('Error 77583EEB-3601-4248-B437-9DF5D7D21109');
    delete(Lfunction, P1, P2-P1+1);
    insert(',', Lfunction, P1);
    P1 := ALPosA(':', Lfunction);
  end; // ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter,),
  Lfunction := ALStringReplaceA(Lfunction, '),', ');', [RfIgnoreCase, rfReplaceAll]); // ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter,);
  Lfunction := ALStringReplaceA(Lfunction, ',)', ')', [RfIgnoreCase, rfReplaceAll]); // ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);

  if (LIsWithExifOrientationInfo) and (ALOADFROM = 'File') and (ATOName <> 'Drawable') then
    Lfunction := ALStringReplaceA(Lfunction, ');', ', AlGetExifOrientationInfo(aFileName));', [RfIgnoreCase]);

  if ALPosA('ALLoadFromJBitmapToJBitmap', Lfunction) > 0 then exit;
  if ALPosA('ALLoadFromBitmapToBitmap', Lfunction) > 0 then exit;

  ATemplate := ALStringReplaceA(ATemplate, ' overload;', '', [RfIgnoreCase]);
  if LPlatform <> '' then begin
    var LDefine := '{$IF defined('+LPlatform+')}';
    var LSep: AnsiString := '{';
    While length(LSep) < length(LDefine) - 1 do LSep := LSep + '*';
    OutputImplementationLst.Add(LSep + '}');
    OutputImplementationLst.Add(LDefine);
  end
  else begin
    var LSep: AnsiString := '{';
    While length(LSep) < length(ATemplate) - 1 do LSep := LSep + '*';
    OutputImplementationLst.Add(LSep + '}');
  end;
  OutputImplementationLst.Add(ATemplate);
  If (ATOName = 'Drawable') then begin
    OutputImplementationLst.Add('begin');
    OutputImplementationLst.Add('  {$IF defined(ALSkiaEngine)}');
    var LfunctionToSKImage := ALStringReplaceA(Lfunction, 'Drawable(', 'SkImage(', [RfIgnoreCase]);
    var LfunctionToSkSurface := ALStringReplaceA(Lfunction, 'Drawable(', 'SkSurface(', [RfIgnoreCase]);
    OutputImplementationLst.Add('    {$IF defined(ALSkiaCanvas)}');
    OutputImplementationLst.Add('    Result := ' + LfunctionToSKImage);
    OutputImplementationLst.Add('    {$ELSEIF defined(ALGPUCanvas)}');
    OutputImplementationLst.Add('    var LSurface := ' + LfunctionToSkSurface);
    OutputImplementationLst.Add('    try');
    OutputImplementationLst.Add('      result := ALCreateTextureFromSkSurface(LSurface);');
    OutputImplementationLst.Add('    finally');
    OutputImplementationLst.Add('      sk4d_refcnt_unref(LSurface);');
    OutputImplementationLst.Add('    end;');
    OutputImplementationLst.Add('    {$ELSE}');
    OutputImplementationLst.Add('    var LSurface := ' + LfunctionToSkSurface);
    OutputImplementationLst.Add('    try');
    OutputImplementationLst.Add('      result := ALCreateBitmapFromSkSurface(LSurface);');
    OutputImplementationLst.Add('    finally');
    OutputImplementationLst.Add('      sk4d_refcnt_unref(LSurface);');
    OutputImplementationLst.Add('    end;');
    OutputImplementationLst.Add('    {$ENDIF}');
    OutputImplementationLst.Add('  {$ELSEIF defined(ANDROID)}');
    var LfunctionToJBitmap := ALStringReplaceA(Lfunction, 'Drawable(', 'JBitmap(', [RfIgnoreCase]);
    OutputImplementationLst.Add('  var LBitmap := ' + LfunctionToJBitmap);
    OutputImplementationLst.Add('  try');
    OutputImplementationLst.Add('    result := ALCreateTextureFromJBitmap(LBitmap);');
    OutputImplementationLst.Add('  finally');
    OutputImplementationLst.Add('    LBitmap.recycle;');
    OutputImplementationLst.Add('    LBitmap := nil;');
    OutputImplementationLst.Add('  end;');
    OutputImplementationLst.Add('  {$ELSEIF defined(ALAppleOS)}');
    var LfunctionToCGContextRef := ALStringReplaceA(Lfunction, 'Drawable(', 'CGContextRef(', [RfIgnoreCase]);
    OutputImplementationLst.Add('  var LCGContextRef := ' + LfunctionToCGContextRef);
    OutputImplementationLst.Add('  try');
    OutputImplementationLst.Add('    {$IF defined(ALGPUCanvas)}');
    OutputImplementationLst.Add('    result := ALCreateTextureFromCGContextRef(LCGContextRef);');
    OutputImplementationLst.Add('    {$ELSE}');
    OutputImplementationLst.Add('    result := ALCreateBitmapFromCGContextRef(LCGContextRef);');
    OutputImplementationLst.Add('    {$ENDIF}');
    OutputImplementationLst.Add('  finally');
    OutputImplementationLst.Add('    CGContextRelease(LCGContextRef);');
    OutputImplementationLst.Add('  end;');
    OutputImplementationLst.Add('  {$ELSE}');
    var LfunctionToBitmap := ALStringReplaceA(Lfunction, 'Drawable(', 'Bitmap(', [RfIgnoreCase]);
    OutputImplementationLst.Add('  Result := ' + LfunctionToBitmap);
    OutputImplementationLst.Add('  {$ENDIF}');
  end
  else If (ALOADFROM = 'Resource') then begin
    OutputImplementationLst.Add('begin');
    var LfunctionFromStream := ALStringReplaceA(Lfunction, 'AResName', 'LStream', [RfIgnoreCase]);
    LfunctionFromStream := ALStringReplaceA(LfunctionFromStream, 'ALLoadFromResource', 'ALLoadFromStream', [RfIgnoreCase]);
    OutputImplementationLst.Add('  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);');
    OutputImplementationLst.Add('  try');
    OutputImplementationLst.Add('    result := '+LfunctionFromStream);
    OutputImplementationLst.Add('  finally');
    OutputImplementationLst.Add('    ALfreeandNil(LStream);');
    OutputImplementationLst.Add('  end;');
  end
  else If (ATOName = 'CGImageRef') then begin
    OutputImplementationLst.Add('begin');
    var LfunctionToCGContextRef := ALStringReplaceA(Lfunction, 'CGImageRef(', 'CGContextRef(', [RfIgnoreCase]);
    if ALposA('ALLoadFromFileAndNormalizeOrientationToCGContextRef(',LfunctionToCGContextRef) > 0 then
      LfunctionToCGContextRef := ALStringReplaceA(LfunctionToCGContextRef, ', AlGetExifOrientationInfo(aFileName)', '', [RfIgnoreCase]);
    OutputImplementationLst.Add('  var LContextRef := '+LfunctionToCGContextRef);
    OutputImplementationLst.Add('  try');
    OutputImplementationLst.Add('    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap');
    OutputImplementationLst.Add('    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually');
    OutputImplementationLst.Add('    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying');
    OutputImplementationLst.Add('    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting');
    OutputImplementationLst.Add('    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,');
    OutputImplementationLst.Add('    // you can avoid the actual physical copy of the data.');
    OutputImplementationLst.Add('    result := CGBitmapContextCreateImage(LContextRef);');
    OutputImplementationLst.Add('    if result = nil then raise Exception.Create(''Failed to create CGImageRef from CGContextRef'');');
    OutputImplementationLst.Add('  finally');
    OutputImplementationLst.Add('    CGContextRelease(LContextRef);');
    OutputImplementationLst.Add('  end;');
  end
  else If (ATOName = 'SkImage') and (ALOADFROM = 'Stream') then begin
    OutputImplementationLst.Add('begin');
    var LfunctionFromSkImageToSkSurface := ALStringReplaceA(Lfunction, 'SkImage(', 'SkSurface(', [RfIgnoreCase]);
    LfunctionFromSkImageToSkSurface := ALStringReplaceA(LfunctionFromSkImageToSkSurface, 'AStream', 'LImage', [RfIgnoreCase]);
    LfunctionFromSkImageToSkSurface := ALStringReplaceA(LfunctionFromSkImageToSkSurface, 'ALLoadFromStream', 'ALLoadFromSkImage', [RfIgnoreCase]);
    OutputImplementationLst.Add('  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));');
    OutputImplementationLst.Add('  try');
    OutputImplementationLst.Add('    var LStreamadapterProcs: sk_streamadapter_procs_t;');
    OutputImplementationLst.Add('    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;');
    OutputImplementationLst.Add('    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;');
    OutputImplementationLst.Add('    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;');
    OutputImplementationLst.Add('    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;');
    OutputImplementationLst.Add('    sk4d_streamadapter_set_procs(@LStreamadapterProcs);');
    if ALposA('ALLoadFromStreamToSkImage', Lfunction) > 0 then begin
      OutputImplementationLst.Add('    Result := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));');
    end
    else begin
      OutputImplementationLst.Add('    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));');
      OutputImplementationLst.Add('    try');
      OutputImplementationLst.Add('      var LSurface := '+LfunctionFromSkImageToSkSurface);
      OutputImplementationLst.Add('      try');
      OutputImplementationLst.Add('        Result := ALCreateSkImageFromSkSurface(LSurface);');
      OutputImplementationLst.Add('      finally');
      OutputImplementationLst.Add('        sk4d_refcnt_unref(LSurface);');
      OutputImplementationLst.Add('      end;');
      OutputImplementationLst.Add('    finally');
      OutputImplementationLst.Add('      sk4d_refcnt_unref(LImage);');
      OutputImplementationLst.Add('    end;');
    end;
    OutputImplementationLst.Add('  finally');
    OutputImplementationLst.Add('    sk4d_streamadapter_destroy(LStream);');
    OutputImplementationLst.Add('  end;');
  end
  else If (ATOName = 'SkSurface') and (ALOADFROM = 'Stream') then begin
    OutputImplementationLst.Add('begin');
    var LfunctionFromSkImageToSkSurface := ALStringReplaceA(Lfunction, 'SkImage(', 'SkSurface(', [RfIgnoreCase]);
    LfunctionFromSkImageToSkSurface := ALStringReplaceA(LfunctionFromSkImageToSkSurface, 'AStream', 'LImage', [RfIgnoreCase]);
    LfunctionFromSkImageToSkSurface := ALStringReplaceA(LfunctionFromSkImageToSkSurface, 'ALLoadFromStream', 'ALLoadFromSkImage', [RfIgnoreCase]);
    OutputImplementationLst.Add('  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));');
    OutputImplementationLst.Add('  try');
    OutputImplementationLst.Add('    var LStreamadapterProcs: sk_streamadapter_procs_t;');
    OutputImplementationLst.Add('    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;');
    OutputImplementationLst.Add('    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;');
    OutputImplementationLst.Add('    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;');
    OutputImplementationLst.Add('    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;');
    OutputImplementationLst.Add('    sk4d_streamadapter_set_procs(@LStreamadapterProcs);');
    OutputImplementationLst.Add('    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));');
    OutputImplementationLst.Add('    try');
    OutputImplementationLst.Add('      Result := '+LfunctionFromSkImageToSkSurface);
    OutputImplementationLst.Add('    finally');
    OutputImplementationLst.Add('      sk4d_refcnt_unref(LImage);');
    OutputImplementationLst.Add('    end;');
    OutputImplementationLst.Add('  finally');
    OutputImplementationLst.Add('    sk4d_streamadapter_destroy(LStream);');
    OutputImplementationLst.Add('  end;');
  end
  else If (ATOName = 'SkImage') and (ALOADFROM = 'File') then begin
    OutputImplementationLst.Add('begin');
    var LfunctionFromSkImageToSkSurface := ALStringReplaceA(Lfunction, 'SkImage(', 'SkSurface(', [RfIgnoreCase]);
    LfunctionFromSkImageToSkSurface := ALStringReplaceA(LfunctionFromSkImageToSkSurface, 'AFileName', 'LImage', [RfIgnoreCase]);
    LfunctionFromSkImageToSkSurface := ALStringReplaceA(LfunctionFromSkImageToSkSurface, 'ALLoadFromFile', 'ALLoadFromSkImage', [RfIgnoreCase]);
    if ALposA('ALLoadFromFileToSkImage', Lfunction) > 0 then begin
      OutputImplementationLst.Add('  Result := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));');
    end
    else begin
      OutputImplementationLst.Add('  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));');
      OutputImplementationLst.Add('  try');
      OutputImplementationLst.Add('    var LSurface := '+LfunctionFromSkImageToSkSurface);
      OutputImplementationLst.Add('    try');
      OutputImplementationLst.Add('      Result := ALCreateSkImageFromSkSurface(LSurface);');
      OutputImplementationLst.Add('    finally');
      OutputImplementationLst.Add('      sk4d_refcnt_unref(LSurface);');
      OutputImplementationLst.Add('    end;');
      OutputImplementationLst.Add('  finally');
      OutputImplementationLst.Add('    sk4d_refcnt_unref(LImage);');
      OutputImplementationLst.Add('  end;');
    end;
  end
  else If (ATOName = 'SkSurface') and (ALOADFROM = 'File') then begin
    OutputImplementationLst.Add('begin');
    var LfunctionFromSkImageToSkSurface := ALStringReplaceA(Lfunction, 'SkImage(', 'SkSurface(', [RfIgnoreCase]);
    LfunctionFromSkImageToSkSurface := ALStringReplaceA(LfunctionFromSkImageToSkSurface, 'AFileName', 'LImage', [RfIgnoreCase]);
    LfunctionFromSkImageToSkSurface := ALStringReplaceA(LfunctionFromSkImageToSkSurface, 'ALLoadFromFile', 'ALLoadFromSkImage', [RfIgnoreCase]);
    OutputImplementationLst.Add('  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));');
    OutputImplementationLst.Add('  try');
    OutputImplementationLst.Add('    Result := '+LfunctionFromSkImageToSkSurface);
    OutputImplementationLst.Add('  finally');
    OutputImplementationLst.Add('    sk4d_refcnt_unref(LImage);');
    OutputImplementationLst.Add('  end;');
  end
  else If (ATOName = 'JBitmap') and (ALOADFROM = 'Stream') then begin
    OutputImplementationLst.Add('begin');
    var LfunctionFromJBitmap := ALStringReplaceA(Lfunction, 'AStream', 'LBitmap', [RfIgnoreCase]);
    LfunctionFromJBitmap := ALStringReplaceA(LfunctionFromJBitmap, 'ALLoadFromStream', 'ALLoadFromJBitmap', [RfIgnoreCase]);
    OutputImplementationLst.Add('  var LLength := AStream.Size-AStream.Position;');
    OutputImplementationLst.Add('  var LArray := TJavaArray<Byte>.Create(LLength);');
    OutputImplementationLst.Add('  try');
    OutputImplementationLst.Add('    AStream.ReadBuffer(LArray.Data^, LLength);');
    OutputImplementationLst.Add('    var LOptions := TJBitmapFactory_Options.Javaclass.Init;');
    OutputImplementationLst.Add('    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;');
    if ALposA('ALLoadFromStreamToJBitmap', Lfunction) > 0 then begin
      OutputImplementationLst.Add('    Result := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);');
      OutputImplementationLst.Add('    if Result = nil then raise Exception.create(''Failed to decode bitmap from stream'');');
    end
    else begin
      OutputImplementationLst.Add('    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);');
      OutputImplementationLst.Add('    if LBitmap = nil then raise Exception.create(''Failed to decode bitmap from stream'');');
      OutputImplementationLst.Add('    try');
      OutputImplementationLst.Add('      Result := '+LfunctionFromJBitmap);
      OutputImplementationLst.Add('    finally');
      OutputImplementationLst.Add('      if not LBitmap.equals(Result) then LBitmap.recycle;');
      OutputImplementationLst.Add('      LBitmap := nil;');
      OutputImplementationLst.Add('    end;');
    end;
    OutputImplementationLst.Add('    LOptions := nil;');
    OutputImplementationLst.Add('  finally');
    OutputImplementationLst.Add('    ALfreeandNil(LArray);');
    OutputImplementationLst.Add('  end;');
  end
  else If (ATOName = 'JBitmap') and (ALOADFROM = 'File') then begin
    OutputImplementationLst.Add('begin');
    var LfunctionFromJBitmap := ALStringReplaceA(Lfunction, 'AFileName', 'LBitmap', [RfIgnoreCase]);
    LfunctionFromJBitmap := ALStringReplaceA(LfunctionFromJBitmap, 'ALLoadFromFile', 'ALLoadFromJBitmap', [RfIgnoreCase]);
    OutputImplementationLst.Add('  var LOptions := TJBitmapFactory_Options.Javaclass.Init;');
    OutputImplementationLst.Add('  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;');
    if ALposA('ALLoadFromFileToJBitmap', Lfunction) > 0 then begin
      OutputImplementationLst.Add('  Result := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);');
      OutputImplementationLst.Add('  if Result = nil then raise Exception.create(''Failed to load bitmap from file'');');
    end
    else begin
      OutputImplementationLst.Add('  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);');
      OutputImplementationLst.Add('  if LBitmap = nil then raise Exception.create(''Failed to load bitmap from file'');');
      OutputImplementationLst.Add('  try');
      OutputImplementationLst.Add('    Result := '+LfunctionFromJBitmap);
      OutputImplementationLst.Add('  finally');
      OutputImplementationLst.Add('    if not LBitmap.equals(Result) then LBitmap.recycle;');
      OutputImplementationLst.Add('    LBitmap := nil;');
      OutputImplementationLst.Add('  end;');
    end;
    OutputImplementationLst.Add('  LOptions := nil;');
  end
  else If (ATOName = 'CGContextRef') and (ALOADFROM = 'Stream') then begin
    OutputImplementationLst.Add('begin');
    var LfunctionFromOSImage := ALStringReplaceA(Lfunction, 'AStream', 'LImage', [RfIgnoreCase]);
    LfunctionFromOSImage := ALStringReplaceA(LfunctionFromOSImage, 'ALLoadFromStream', 'ALLoadFromOSImage', [RfIgnoreCase]);
    OutputImplementationLst.Add('  var LBuffer: Pointer := nil;');
    OutputImplementationLst.Add('  var LLength: Int64 := 0;');
    OutputImplementationLst.Add('  var LMemoryStream: TCustomMemoryStream := nil;');
    OutputImplementationLst.Add('  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin');
    OutputImplementationLst.Add('    LBuffer := TCustomMemoryStream(AStream).Memory;');
    OutputImplementationLst.Add('    LLength := AStream.Size;');
    OutputImplementationLst.Add('    AStream.Position := AStream.Size;');
    OutputImplementationLst.Add('  end');
    OutputImplementationLst.Add('  else LMemoryStream := TMemoryStream.Create;');
    OutputImplementationLst.Add('  try');
    OutputImplementationLst.Add('    if LMemoryStream <> nil then begin');
    OutputImplementationLst.Add('      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);');
    OutputImplementationLst.Add('      LBuffer := LMemoryStream.Memory;');
    OutputImplementationLst.Add('      LLength := LMemoryStream.Size;');
    OutputImplementationLst.Add('    end;');
    OutputImplementationLst.Add('    var LData := TNSData.Wrap(');
    OutputImplementationLst.Add('                   TNSData.alloc.initWithBytesNoCopy(');
    OutputImplementationLst.Add('                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.');
    OutputImplementationLst.Add('                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.');
    OutputImplementationLst.Add('                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.');
    OutputImplementationLst.Add('    try');
    OutputImplementationLst.Add('      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));');
    OutputImplementationLst.Add('      if LImage = nil then raise Exception.create(''Failed to decode image from stream'');');
    OutputImplementationLst.Add('      try');
    OutputImplementationLst.Add('        result := '+LfunctionFromOSImage);
    OutputImplementationLst.Add('      finally');
    OutputImplementationLst.Add('        LImage.release;');
    OutputImplementationLst.Add('      end;');
    OutputImplementationLst.Add('    finally');
    OutputImplementationLst.Add('      LData.release;');
    OutputImplementationLst.Add('    end;');
    OutputImplementationLst.Add('  finally');
    OutputImplementationLst.Add('    ALFreeAndNil(LMemoryStream);');
    OutputImplementationLst.Add('  end;');
  end
  else If (ATOName = 'CGContextRef') and (ALOADFROM = 'File') then begin
    OutputImplementationLst.Add('begin');
    var LfunctionFromOSImage := ALStringReplaceA(Lfunction, 'AFileName', 'LImage', [RfIgnoreCase]);
    LfunctionFromOSImage := ALStringReplaceA(LfunctionFromOSImage, 'ALLoadFromFile', 'ALLoadFromOSImage', [RfIgnoreCase]);
    OutputImplementationLst.Add('  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));');
    OutputImplementationLst.Add('  if LImage = nil then raise Exception.create(''Failed to load image from file'');');
    OutputImplementationLst.Add('  try');
    OutputImplementationLst.Add('    result := '+LfunctionFromOSImage);
    OutputImplementationLst.Add('  finally');
    OutputImplementationLst.Add('    LImage.release;');
    OutputImplementationLst.Add('  end;');
  end
  else If (ATOName = 'Bitmap') and (ALOADFROM = 'Stream') then begin
    OutputImplementationLst.Add('begin');
    var LfunctionFromBitmap := ALStringReplaceA(Lfunction, 'AStream', 'LBitmap', [RfIgnoreCase]);
    LfunctionFromBitmap := ALStringReplaceA(LfunctionFromBitmap, 'ALLoadFromStream', 'ALLoadFromBitmap', [RfIgnoreCase]);
    if ALposA('ALLoadFromBitmapAndNormalizeOrientationToBitmap', LfunctionFromBitmap) > 0 then begin
      OutputImplementationLst.Add('  Result := Tbitmap.CreateFromStream(aStream);');
      OutputImplementationLst.Add('  case aExifOrientationInfo of');
      OutputImplementationLst.Add('    TalExifOrientationInfo.NORMAL: exit;');
      OutputImplementationLst.Add('    TalExifOrientationInfo.FLIP_HORIZONTAL: Result.FlipHorizontal;');
      OutputImplementationLst.Add('    TalExifOrientationInfo.ROTATE_180: Result.Rotate(180);');
      OutputImplementationLst.Add('    TalExifOrientationInfo.FLIP_VERTICAL: Result.FlipVertical;');
      OutputImplementationLst.Add('    TalExifOrientationInfo.TRANSPOSE: begin');
      OutputImplementationLst.Add('                                        Result.Rotate(90);');
      OutputImplementationLst.Add('                                        Result.FlipHorizontal;');
      OutputImplementationLst.Add('                                      end;');
      OutputImplementationLst.Add('    TalExifOrientationInfo.ROTATE_90: Result.Rotate(90);');
      OutputImplementationLst.Add('    TalExifOrientationInfo.TRANSVERSE: begin');
      OutputImplementationLst.Add('                                         Result.Rotate(-90);');
      OutputImplementationLst.Add('                                         Result.FlipHorizontal;');
      OutputImplementationLst.Add('                                       end;');
      OutputImplementationLst.Add('    TalExifOrientationInfo.ROTATE_270: Result.Rotate(270);');
      OutputImplementationLst.Add('    TalExifOrientationInfo.UNDEFINED: exit;');
      OutputImplementationLst.Add('    else');
      OutputImplementationLst.Add('      raise exception.Create(''Error 1C368047-00C4-4F68-8C77-56956FABCF92'');');
      OutputImplementationLst.Add('  end;');
    end
    else begin
      if ALposA('ALLoadFromStreamToBitmap', Lfunction) > 0 then begin
        OutputImplementationLst.Add('  Result := Tbitmap.CreateFromStream(aStream);');
      end
      else begin
        OutputImplementationLst.Add('  var LBitmap := Tbitmap.CreateFromStream(aStream);');
        OutputImplementationLst.Add('  try');
        OutputImplementationLst.Add('    result := '+LfunctionFromBitmap);
        OutputImplementationLst.Add('  finally');
        OutputImplementationLst.Add('    ALFreeAndNil(LBitmap);');
        OutputImplementationLst.Add('  end;');
      end;
    end;
  end
  else If (ATOName = 'Bitmap') and (ALOADFROM = 'File') then begin
    OutputImplementationLst.Add('begin');
    var LfunctionFromBitmap := ALStringReplaceA(Lfunction, 'AFileName', 'LBitmap', [RfIgnoreCase]);
    LfunctionFromBitmap := ALStringReplaceA(LfunctionFromBitmap, 'ALLoadFromFile', 'ALLoadFromBitmap', [RfIgnoreCase]);
    if ALposA('ALLoadFromBitmapAndNormalizeOrientationToBitmap', LfunctionFromBitmap) > 0 then begin
      OutputImplementationLst.Add('  Result := Tbitmap.CreateFromFile(AFileName);');
      OutputImplementationLst.Add('  case AlGetExifOrientationInfo(aFileName) of');
      OutputImplementationLst.Add('    TalExifOrientationInfo.NORMAL: exit;');
      OutputImplementationLst.Add('    TalExifOrientationInfo.FLIP_HORIZONTAL: Result.FlipHorizontal;');
      OutputImplementationLst.Add('    TalExifOrientationInfo.ROTATE_180: Result.Rotate(180);');
      OutputImplementationLst.Add('    TalExifOrientationInfo.FLIP_VERTICAL: Result.FlipVertical;');
      OutputImplementationLst.Add('    TalExifOrientationInfo.TRANSPOSE: begin');
      OutputImplementationLst.Add('                                        Result.Rotate(90);');
      OutputImplementationLst.Add('                                        Result.FlipHorizontal;');
      OutputImplementationLst.Add('                                      end;');
      OutputImplementationLst.Add('    TalExifOrientationInfo.ROTATE_90: Result.Rotate(90);');
      OutputImplementationLst.Add('    TalExifOrientationInfo.TRANSVERSE: begin');
      OutputImplementationLst.Add('                                         Result.Rotate(-90);');
      OutputImplementationLst.Add('                                         Result.FlipHorizontal;');
      OutputImplementationLst.Add('                                       end;');
      OutputImplementationLst.Add('    TalExifOrientationInfo.ROTATE_270: Result.Rotate(270);');
      OutputImplementationLst.Add('    TalExifOrientationInfo.UNDEFINED: exit;');
      OutputImplementationLst.Add('    else');
      OutputImplementationLst.Add('      raise exception.Create(''Error 2F09739F-4CB7-46DC-A665-C64D626DD1D7'');');
      OutputImplementationLst.Add('  end;');
    end
    else begin
      if ALposA('ALLoadFromFileToBitmap', Lfunction) > 0 then begin
        OutputImplementationLst.Add('  Result := Tbitmap.CreateFromFile(AFileName);');
      end
      else begin
        OutputImplementationLst.Add('  var LBitmap := Tbitmap.CreateFromFile(AFileName);');
        OutputImplementationLst.Add('  try');
        OutputImplementationLst.Add('    result := '+LfunctionFromBitmap);
        OutputImplementationLst.Add('  finally');
        OutputImplementationLst.Add('    ALFreeAndNil(LBitmap);');
        OutputImplementationLst.Add('  end;');
      end;
    end;
  end
  else begin
    P1 := ALPosA(ATemplate+#13#10+'begin'#13#10, LOriginalAlcinoeFMXGraphicsPas);
    If P1 <= 0 Then P1 := ALPosA(ATemplate+#13#10#13#10, LOriginalAlcinoeFMXGraphicsPas);
    If P1 > 0 Then begin
      inc(P1, length(ATemplate+#13#10));
      var P2 := AlposA(#13#10'end;', LOriginalAlcinoeFMXGraphicsPas, P1);
      if P2 <= 0 then raise Exception.Create('Error 67DAE094-740D-4CCD-888E-E20E25A424DB');
      OutputImplementationLst.Add(ALcopyStr(LOriginalAlcinoeFMXGraphicsPas, P1, P2-P1));
    end
    else OutputImplementationLst.Add('begin');
  end;
  OutputImplementationLst.Add('end;');
  if LPlatform <> '' then OutputImplementationLst.Add('{$ENDIF}');
  OutputImplementationLst.Add('');
end;

begin
  try

    OutputInterfaceLst := TALStringListA.Create;
    OutputImplementationLst := TALStringListA.Create;
    var LInputLst := TALStringListA.Create;
    try
      LInputLst.LoadFromFile('Input.pas');

      CurrTemplate := '';
      CurrComment := '';
      CurrPlatform := '';
      CurrToName := '';
      CurrFunctionName := '';

      var I: integer := 0;
      While I < LInputLst.Count - 1 do begin
        CurrComment := LInputLst[i];
        inc(I);
        var LTemplate := LInputLst[i];
        inc(I);

        AddFunctionToOutPut(LTemplate, 'SkImage',  'SkSurface');
        AddFunctionToOutPut(LTemplate, 'Stream',   'SkSurface');
        AddFunctionToOutPut(LTemplate, 'Resource', 'SkSurface');
        AddFunctionToOutPut(LTemplate, 'File',     'SkSurface');

        AddFunctionToOutPut(LTemplate, 'Stream',   'SkImage');
        AddFunctionToOutPut(LTemplate, 'Resource', 'SkImage');
        AddFunctionToOutPut(LTemplate, 'File',     'SkImage');

        AddFunctionToOutPut(LTemplate, 'JBitmap',  'JBitmap');
        AddFunctionToOutPut(LTemplate, 'Stream',   'JBitmap');
        AddFunctionToOutPut(LTemplate, 'Resource', 'JBitmap');
        AddFunctionToOutPut(LTemplate, 'File',     'JBitmap');

        AddFunctionToOutPut(LTemplate, 'ALOSImage',  'CGContextRef');
        AddFunctionToOutPut(LTemplate, 'Stream',   'CGContextRef');
        AddFunctionToOutPut(LTemplate, 'Resource', 'CGContextRef');
        AddFunctionToOutPut(LTemplate, 'File',     'CGContextRef');

        AddFunctionToOutPut(LTemplate, 'Stream',   'CGImageRef');
        AddFunctionToOutPut(LTemplate, 'Resource', 'CGImageRef');
        AddFunctionToOutPut(LTemplate, 'File',     'CGImageRef');

        AddFunctionToOutPut(LTemplate, 'Bitmap',   'Bitmap');
        AddFunctionToOutPut(LTemplate, 'Stream',   'Bitmap');
        AddFunctionToOutPut(LTemplate, 'Resource', 'Bitmap');
        AddFunctionToOutPut(LTemplate, 'File',     'Bitmap');

        AddFunctionToOutPut(LTemplate, 'Stream',   'Drawable');
        AddFunctionToOutPut(LTemplate, 'Resource', 'Drawable');
        AddFunctionToOutPut(LTemplate, 'File',     'Drawable');

      end;


      OutputInterfaceLst.Add('{$ENDREGION}');

      OutputInterfaceLst.SaveToFile('Output_InterfaceLst.pas');
      OutputImplementationLst.SaveToFile('Output_Implementation.pas');
    finally
      ALFreeandNil(LInputLst);
      ALFreeandNil(OutputInterfaceLst);
      ALFreeandNil(OutputImplementationLst);
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
