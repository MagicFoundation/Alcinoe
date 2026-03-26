## 03/26/2026

- Renamed `Alcinoe.Mime.ContentTypes` to `Alcinoe.Mime.Types`
- Renamed MIME helper functions:
  - `ALGetDefaultMIMEContentTypeFromExt` to `ALGetDefaultMimeTypeFromExt`
  - `ALGetDefaultFileExtFromMimeContentType` to `ALGetDefaultFileExtFromMimeType`
- Changed `TALStringStreamA` to inherit from `TMemoryStream` instead of `TStream`
- Added `ALStopCurrentService`
- Updated `ALInstallService`:
  - default `APreshutdownTimeout` is now `180000`
  - added `AEnableFailureAutoRestart: Boolean = True`
  - configured automatic restart on service failure
- Added success messages to `ALInstallService` and `ALUninstallService`
- Renamed `TALHttpServerA.MaxBodySize` to `TALHttpServerA.MaxRequestBodySize`
- Simplified `TALHttpServerHttpSys` worker thread management:
  - removed `MinWorkerThreadCount`
  - removed `MaxWorkerThreadCount`
  - `WorkerThreadCount` is now directly configurable
- Added Windows version guards before enabling WinHTTP HTTP/2, HTTP/3, and decompression options
- Optimized parts of `TALJSONNodeA/TALJSONNodeW` JSON/BSON parsing and serialization:
  - reduced internal buffer sizes from `32768` to `16384`
  - used `TBufferedFileStream` for JSON/BSON file saves
  - streamlined direct stream writes in serialization paths

## 03/08/2026

- Changed `TALJSONNodeA.BSON` from `AnsiString` to `TBytes`. If you still need the BSON payload as a byte-based AnsiString, use `TALJSONNodeA.SaveToBSONString`
- Added `poBinaryAsPtrStream` to `TALJSONParseOption`
- Added `LoadFromBSONBytes` and `ParseBSONBytes` to `TALJSONNodeA/TALJSONDocumentA`
- Added `GetBinaryAsBytes`, `GetBinaryAsStream`, `SetBinaryAsBytes`, `SetBinaryAsStream`, `GetOwnsBinaryStream`, and `SetOwnsBinaryStream` to `TALJSONNodeA/TALJSONNodeW`
- Added `GetChildValueBinaryAsBytes`, `GetChildValueBinaryAsStream`, `SetChildValueBinaryAsBytes`, and `SetChildValueBinaryAsStream` to `TALJSONNodeA/TALJSONNodeW`
- Changed `TALJSONNodeA/TALJSONNodeW.ParseBSON` to use `RawBSONBytes: TBytes` instead of string-based BSON buffers
- Changed `TALJSONNodeA/TALJSONNodeW.ObjectID` and related getters/setters to use `TBytes` instead of `AnsiString/String`
- Changed binary handling in `TALJSONNodeA/TALJSONNodeW` from `Binary: AnsiString/String` to `BinaryAsBytes: TBytes` and `BinaryAsStream: TStream`
- Changed `ALJsonEncodeBinaryWithNodeSubTypeHelperA/W` and `ALJsonEncodeObjectIDWithNodeSubTypeHelperA/W` to use `TBytes`
- Changed `ALJSONTryStrToBinaryA/W` and `ALJSONTryStrToObjectIDA/W` to return `TBytes`
- Removed sorted/duplicate/owner support from `TALJSONNodeListA/TALJSONNodeListW`
- Removed `MultiThreadPrepare`, `NextSibling`, `PreviousSibling`, `ParentNode`, and `NodeValue` from `TALJSONNodeA/TALJSONNodeW`
- Removed `SetChildNodes` from `TALJSONNodeA/TALJSONNodeW`, `TALJSONObjectNodeA/TALJSONObjectNodeW`, and `TALJSONArrayNodeA/TALJSONArrayNodeW`
- Removed `ALJSONToXMLA`
- Removed `ALJsonEncodeWithNodeSubTypeHelperA/W`
- Removed `TAlJSONParseDocumentW`
- Added `ALConvertStringToBytes` and `ALRevertBytesToString` to convert an `AnsiString` to `TBytes` without copying the underlying data.

## 03/02/2026

- Added TALCookedUrlW  
- Added ExtractHeaders and ExtractBodyStream to TALHttpRequestA/TALHttpRequestW and TALHttpResponseA/TALHttpResponseW  
- Changed `ALAcquireKeepAliveWinHttpClient/ALReleaseKeepAliveWinHttpClient` to use `AUrl: AnsiString` instead of `AURI: TUri`  
- Changed `ALAcquireKeepAliveNetHttpClient/ALReleaseKeepAliveNetHttpClient` to use `AUrl: String` instead of `AURI: TUri`
- Renamed `TALImageWrapMode.FitAndCrop` to `TALImageWrapMode.Cover`
- Renamed `TALJSONNodeA/TALJSONNodeW.GetChildNodeValueXXX` to `TALJSONNodeA/TALJSONNodeW.GetChildValueXXX`
- Renamed `ALFindJsonNodeByChildNodeValueXXX` to `ALFindJsonNodeByChildValueXXX`

## 02/19/2026

- Added the default indexed property `Chars[index: Integer]: AnsiChar read GetChars write SetChars;` to `TALStringBuilderA`.
- Added the default indexed property `Chars[index: Integer]: Char read GetChars write SetChars;` to `TALStringBuilderW`.
- Added the following overloads:
  - `function ALExtractHeaderParamValue(const AHeaderValue: AnsiString; const AParamName: AnsiString): AnsiString; overload;`
  - `function ALExtractHeaderParamValue(const AHeaderValue: String; const AParamName: String): String; overload;`
- Added `TALMimePartHeadersA` and `TALMultipartDecoderA`.
- Added `function ALJsonEncodeDateTimeA(const AValue: TDateTime): AnsiString;`.
- Updated `LoadFromJSONStream` and `LoadFromBsonStream` to always reset the stream position to 0 before loading.
- Removed `ALExtractHttpCharsetFromContentType` (use `ALExtractHeaderParamValue` instead).
- Added Keychain helpers:
  - `function ALKeychainReadBytes(const AService: String; const AAccount: String; out ABytes: TBytes): Boolean;`
  - `procedure ALKeychainWriteBytes(const AService: String; const AAccount: String; const ABytes: TBytes);`
- Added platform helpers:
  - `function ALGetPlatform: String;`
  - `function ALGetDeviceID: String;`
- Fixed Alcinoe issue #492.
- Added `TALHttpRequestHeadersA.ContentBoundary` and `TALHttpRequestHeadersW.ContentBoundary`.
- Made `TALMultipartDecoderA` **zero-copy**: it no longer duplicates the payload bytes. Each 
  part body is exposed as a stream that directly references the underlying buffer of `APayloadStream`.

## 01/20/2026

- Added **TAlSChannelTlsClient** (native Windows SChannel TLS 1.2 / 1.3 support)
- Added **TALWinSocketClient**
- Added service management helpers:
  - `ALInstallService`
  - `ALUninstallService`
  - `ALStartService`
  - `ALStopService`
  - `ALStartServiceCtrlDispatcher`  
  to fully manage Windows services
- Archived `Alcinoe.Winsock.pas` (functions moved to `Alcinoe.Net`)
- Renamed `Alcinoe.Winapi.HttpApi` to `Alcinoe.Winapi.Http`
- Rebuilt **TALSMTPClient** with native **TLS 1.2 / TLS 1.3** support
- Added **TALMultipartMixedEncoderA** and **TALMultipartAlternativeEncoderA**
- Renamed and rebuilt **TALEMailHeadersA** to **TALMailHeadersA**
- Archived `Alcinoe.Sqlite3.Client` and `Alcinoe.Sqlite3.Wrapper`
- Archived `Alcinoe.POP3.Client`
- Archived `Alcinoe.MemCached.Client`

## 12/24/2025

- Align **Alcinoe** versioning with the Delphi compiler version (e.g. Alcinoe built 
  with Delphi 13.0 now uses version `13.0.x`).
- Added `HttpWorker.Cancel` method.
- Added `ToString` helpers for `TRectF`, `TSizeF`, `TPointF`, `TALRectD`, `TALSizeD`, 
  and `TALPointD`.
- Added unified path helper functions:
  - `ALGetAppDataPathW`
  - `ALGetTempPathW`
  - `ALGetTempFilenameW`
  - `ALGetCachePathW`
- Added `ResourceStream` property to `TALAnimatedImage`.
- Added `PrependItem`, `AppendItem`, and `DeleteItem` methods to `TALDynamicListBox`.
- Removed `AddItem` from `TALDynamicListBox` (use `PrependItem` or `AppendItem` instead).
- `TALEdit` controls are now automatically frozen (using a screenshot for rendering) 
  when partially visible, improving Z-order simulation.
- `TALMemo` and `TALEdit` now use `ALResolveLineHeightMultiplier`, ensuring the same 
  line-height algorithm as `TALText`.
- Updated `TALNotificationService` to use `TMessage`-based notifications:
  - `TNotificationReceivedMessage`
  - `TGetTokenMessage`
  - `TDeleteTokenMessage`
  - `TTokenRefreshMessage`
  - `TNotificationPermissionResultMessage`
  This replaces event-based callbacks (`OnNotificationReceived`, `OnGetToken`, etc.) and 
  better aligns with the singleton design.
- Added an internal `ScrollBox` to `TALBottomSheet` to support content larger than the 
  maximum visible area.
- Added `IsAncestorOf` method to `TControl`.

### 11/25/2025

- Added `TALHttpWorker` component to support background upload/download operations
- Refactoring of HTTP-related types to explicitly separate ANSI (A) 
  and Unicode (W) variants:
  - `TALHttpServerRequest` → `TALHttpServerRequestA`
  - `TALHttpServerResponse` → `TALHttpServerResponseA`
  - `TALHttpServer` → `TALHttpServerA`
  - `TALHTTPCookie` → `TALHTTPCookieA` and `TALHTTPCookieW`
  - `TALHTTPRequestHeaders` → `TALHTTPRequestHeadersA` and `TALHTTPRequestHeadersW`
  - `TALHTTPResponseHeaders` → `TALHTTPResponseHeadersA` and `TALHTTPResponseHeadersW`
  - `TALHTTPRequest` → `TALHTTPRequestA` and `TALHTTPRequestW`
  - `TALHTTPResponse` → `TALHTTPResponseA` and `TALHTTPResponseW`
  - `AlRfc822DayOfWeekNames` → `AlRfc822DayOfWeekNamesA` and `AlRfc822DayOfWeekNamesW`
  - `ALRfc822MonthOfTheYearNames` → `ALRfc822MonthOfTheYearNamesA` and `ALRfc822MonthOfTheYearNamesW`
- `TALUserPreferences` now saves its settings in the file `{PackageName}.user_preferences`

### 11/16/2025

- Renamed TALImage.ApplyExifOrientation to TALImage.ApplyMetadataOrientation
  for consistency with TALVideoPlayer.ApplyMetadataOrientation.

### 11/14/2025

- Added the complete ImageMagick wrapper, including all functions 
  and types from MagickCore and MagickWand. This wrapper was 
  generated automagically using the ImageMagickWrapperGenerator 
  project.

### 11/09/2025

- Renamed `RotateAccordingToMetadataOrientation` → `ApplyMetadataOrientation`
- Renamed `RotateAccordingToExifOrientation` → `ApplyExifOrientation`
- Added `GetSingle`, `GetDouble`, and `GetDateTime` to `TALUserPreferences`
- `TALBrush` now includes:
  - `ResourceStream`
  - `OwnsResourceStream`
  - `ApplyExifOrientation`
- `TALImage`:
  - Now supports `HTTPHeaders` when downloading from a URL
  - Added `OwnsResourceStream`
- Renamed `TalExifOrientationInfo` → `TalExifOrientation`
- Renamed `AlGetExifOrientationInfo` → `AlGetExifOrientation`
- Added `AApplyExifOrientation` parameter to:
  - `ALCreateSkSurfaceFromResource`
  - `ALCreateSkImageFromResource`
  - `ALCreateJBitmapFromResource`
  - `ALCreateCGContextRefFromResource`
  - `ALCreateCGImageRefFromResource`
  - `ALCreateTBitmapFromResource`
  - `ALCreateBitmapFromResource`
  - `ALCreateDrawableFromResource`
- Updated `TALDialog`, `TALLoadingOverlay`, and `TALSheet` to emulate overlays over system bars (via system bar color adjustments)
- Added `ALGetSystemBarsColor` and `ALSetSystemBarsColor`

### 10/24/2025

- **TALMediaPicker**: New cross-platform component to select
  images/videos or capture media from the camera.  
- **TALVideoPlayer**: Added `RotateAccordingToMetadataOrientation` 
  property to automatically adjust playback orientation.  
- **TALImage**: Added `ResourceStream` property to load 
  images directly from a stream.
  

### 10/02/2025

- Archived
  - `Alcinoe.CGI.pas`
  - `Alcinoe.ExprEval.pas`
  - `Alcinoe.FTP.Client.WinINet.pas`
  - `Alcinoe.FTP.Client.pas`
  - `Alcinoe.GSMComm.pas`
  - `Alcinoe.HTTP.Client.WinINet.pas`
  - `Alcinoe.IsapiHTTP.pas`
  - `Alcinoe.LibPhoneNumber.pas`
  - `Alcinoe.MySql.Client.pas`
  - `Alcinoe.MySql.Wrapper.pas`
  - `Alcinoe.NNTP.Client.pas`
  - `Alcinoe.PhpRunner.pas`
  - `Alcinoe.SphinxQL.Client.pas`
  - `Alcinoe.WebSocket.Client.WinHTTP.pas`
  - `Alcinoe.WebSocket.Client.pas`
  - `Alcinoe.ZLibEx.pas`
  - `Alcinoe.ZLibExGZ.pas`
  - `ZLibEx.inc`
  - `ZLibEx.pas`
  - `ZLibExApi.pas`
  - `ZLibExGZ.pas`
  - `Alcinoe.iOSApi.WebRTC.pas`
  - `Alcinoe.FMX.WebRTC.pas`
  - `Alcinoe.AndroidApi.WebRTC.pas`
  - `Alcinoe.QuickSortList.pas`
- Renamed
  - `Alcinoe.Mime.pas` → `Alcinoe.Mime.ContentTypes.pas`  
  - `Alcinoe.MultiPartParser.pas` → `Alcinoe.Mime.Multipart.pas`  
  - `Alcinoe.WinApi.Common.pas` → `Alcinoe.WinApi.Windows.pas`  
  - `ALHTTPEncode` → `ALUrlEncode` and `ALPercentEncode`  
  - `ALHTTPDecode` → `ALUrlDecode` and `ALPercentDecode`  
  - `OnChange` → `OnChanged` in `TALStringsA` and `TALStringsW`
- Added
  - `TALStringBuilderA` (same as `TStringBuilder` but for `AnsiString`)  
  - `IncludeTrailingLineBreakInText` property to `TALStringsA` and `TALStringsW`  
  - `TALHttpSysServer` (an HTTP server built on top of Windows **http.sys**)  
- Removed
  - `ALExtractExpressionA`  
  - `ALExtractHeaderFieldsWithQuoteEscaped` (use `ALExtractHeaderFields` instead)  
  - `StripParamQuotes` from `ALExtractTagParamsA`  
  - `TALNewsArticleHeader`   

### 08/21/2025

- **ALRectFitInto** and **ALRectPlaceInto**  
  `CenterAt` is now expressed in normalized coordinates `[0..1]`.
- **ALDrawMultiLineText**  
  Now decodes all HTML entities, not only `&gt;` and `&lt;`.
- **Mouse Events**  
  `ChildrenMouseDown`, `ChildrenMouseMove`, `ChildrenMouseUp`, `ChildrenMouseEnter`, and `ChildrenMouseLeave`  
  are now part of Alcinoe and no longer require patching the Embarcadero source file.

### 08/21/2025

- Removed `AlInt2BaseN` and `AlBaseN2Int`
- Added `TALJSONNodeA.Clone` and `TALJSONNodeW.Clone`
- Introduced `ALDefaultEstimateLineHeightMultiplier`: When the 
  `LineHeightMultiplier` of a text is `0`, the function assigned to 
  `ALDefaultEstimateLineHeightMultiplier` (e.g. `ALEstimateLineHeightMultiplier`) 
  will be used to determine the multiplier

### 08/15/2025

- Set `TALText.AutoSize` to default to `TALAutoSizeMode.Both`.  
- If `TALText.MaxWidth = 0`, no explicit maximum width is enforced; 
  the control expands to the largest width that still fits fully 
  inside its parent container.  
- If `TALText.MaxHeight = 0`, no explicit maximum height is enforced; 
  the control expands to the largest height that still fits fully 
  inside its parent container.  
- Added `ShowLoadingIndicator` and `HideLoadingIndicator` methods 
  to `TALButton` to morph it into a loading indicator.  
- Renamed resource `alcinoe_loader` to `alcinoe_loading_indicator`.   

### 08/10/2025

- Added **`TALSnackbar`** component for displaying brief, 
  informative messages to users with optional actions.
- Renamed **`TALDownloadContext`** to **`TALWorkerContext`** 
  to better reflect its broader purpose in managing background tasks.
- Added **Secure** option to **`TALUserPreferences`** to 
  enable encrypted storage of sensitive user data.

### 08/03/2025

- Added **`TALLoadingOverlay`** component — a full-screen overlay 
  designed to block user interaction while displaying a Material-inspired 
  loading indicator.

### 07/27/2025

- Added **`TALLeftSheet`**, **`TALRightSheet`**, **`TALTopSheet`**, 
  and **`TALBottomSheet`**.
- Updated **`TALDialog`** to support right-to-left layouts (inherits 
  the `BiDiMode` from the main application form).
- Improved **`TALDialog`** with smooth upward animation when the 
  virtual keyboard is displayed.
- Renamed **`ALBrokenImageResourceName`** from `'broken_image'` to 
  `'alcinoe_broken_image'`.

#### 16/07/2025

- Added `TALWebBrowser`.
- Introduced `FreezeNativeView` and `UnFreezeNativeView` methods 
  to `TALNativeControl`-based controls such as `TALEdit`, `TALMemo`, 
  and `TALWebBrowser`. These methods capture a screenshot of the 
  underlying native view (`FNativeView`), hide the actual native 
  control, and draw the captured bitmap in its place. This allows 
  simulating z-order (e.g., displaying a popup dialog above the 
  control) without altering the native view hierarchy.
- Refactored `Alcinoe.FMX.Edit` and `Alcinoe.FMX.Memo` units.

#### 07/06/2025

- **Renamed** the following `TALInterpolationType` entries to use 
  the `Material3` prefix for improved clarity and alignment with 
  Material Design 3:
  - `MaterialExpressiveFastSpatial` → `Material3ExpressiveFastSpatial`
  - `MaterialExpressiveDefaultSpatial` → `Material3ExpressiveDefaultSpatial`
  - `MaterialExpressiveSlowSpatial` → `Material3ExpressiveSlowSpatial`
  - `MaterialExpressiveFastEffects` → `Material3ExpressiveFastEffects`
  - `MaterialExpressiveDefaultEffects` → `Material3ExpressiveDefaultEffects`
  - `MaterialExpressiveSlowEffects` → `Material3ExpressiveSlowEffects`
  - `MaterialStandardFastSpatial` → `Material3StandardFastSpatial`
  - `MaterialStandardDefaultSpatial` → `Material3StandardDefaultSpatial`
  - `MaterialStandardSlowSpatial` → `Material3StandardSlowSpatial`
  - `MaterialStandardFastEffects` → `Material3StandardFastEffects`
  - `MaterialStandardDefaultEffects` → `Material3StandardDefaultEffects`
  - `MaterialStandardSlowEffects` → `Material3StandardSlowEffects`
  - `MaterialEmphasized` → `Material3Emphasized`
  - `MaterialEmphasizedDecelerate` → `Material3EmphasizedDecelerate`
  - `MaterialEmphasizedAccelerate` → `Material3EmphasizedAccelerate`

#### 07/04/2025

- **Added** `TALToggleButton` control.
- **Added** Material 3 Expressive button styles.
- **Extended** `TALInterpolatedAnimation` with `Bezier` support 
  via `TALInterpolationType` and `TALInterpolationParams`.
- **Improved** text layout to allow character-level breaking when 
  word boundaries are not available.
- **Added** `ClickSound: TALClickSoundMode (Default, Always, Never)` 
  property to all `TALControl` components.
- **Changed** `StateLayer.XRadius` and `YRadius` to default to `NaN`, 
  allowing them to inherit radius values from their parent control.

#### 06/18/2025

- Replaced `AutoSize: Boolean` with `AutoSize: TALAutoSizeMode` to provide more granular control over sizing behavior.
  ```TALAutoSizeMode = (None, Width, Height, All);```

#### 05/10/2025

- Removed `ReadyBeforeResourcesLoaded`.
- Replaced `IsReadyToDisplay` with `IsReadyToDisplay(const AStrict: Boolean = False)`.
- Renamed `TALAnimation.Interpolation` → `TALAnimation.InterpolationType`.
- Renamed `TALAnimation.AnimationType` → `TALAnimation.InterpolationMode`.
- Updated `TALStateTransition.Interpolation` to use `TALAnimation.InterpolationType`.
- Updated `TALStateTransition.AnimationType` to use `TALAnimation.InterpolationMode`.
- Refactored `TALAniIndicator`; added `MotionMode = (Frame, Rotate)`.
- Removed `Trimming` property from `TALBaseTextSettings`.

#### 05/04/2025

- `Fill.ImageNoRadius` is now `True` by default.
- Added `ALVibrateDevice`.
- Reactivated `OnDblClick` event.
- Split the unit `Alcinoe.FMX.DynamicListBox.pas` into several smaller units:
  - `Alcinoe.FMX.Dynamic.Common.pas`
  - `Alcinoe.FMX.Dynamic.Controls.pas`
  - `Alcinoe.FMX.Dynamic.Layouts.pas`
  - `Alcinoe.FMX.Dynamic.ListBox.pas`
  - `Alcinoe.FMX.Dynamic.Objects.pas`
  - `Alcinoe.FMX.Dynamic.PageController.pas`
  - `Alcinoe.FMX.Dynamic.StdCtrls.pas`
  - `Alcinoe.FMX.Dynamic.VideoPlayer.pas`
- Added `IsPreciseGeoLocationAccessGranted` to `TALGeoLocationSensor`.

#### 05/26/2025

- Moved `TALFormatSettingsA` and `TALFormatSettingsW` to the 
  `Alcinoe.Localization` unit for better organization of 
  locale-specific utilities.
- Added `ALIsAlphaString` and `ALIsAlphaNumeric` helper functions 
  for string validation, and renamed `ALIsDecimal` to 
  `ALIsNumeric` for improved clarity.
- Introduced `TALPluralRules`, a new class for handling language-specific 
  pluralization rules based on numeric values, enabling proper 
  selection of plural forms in internationalized applications. 
  This supports use cases such as correctly displaying localized 
  strings for quantities (e.g., "1 item" vs "2 items") based on 
  the user's language.
- Added `TALDialog.GetButtons`, a method that returns the list of 
  buttons currently attached to the dialog.

#### 05/19/2025

- Added the following easing types to TALInterpolationType, matching Material Design 3 motion system:
  - MaterialExpressiveFastSpatial
  - MaterialExpressiveDefaultSpatial
  - MaterialExpressiveSlowSpatial
  - MaterialExpressiveFastEffects
  - MaterialExpressiveDefaultEffects
  - MaterialExpressiveSlowEffects
  - MaterialStandardFastSpatial
  - MaterialStandardDefaultSpatial
  - MaterialStandardSlowSpatial
  - MaterialStandardFastEffects
  - MaterialStandardDefaultEffects
  - MaterialStandardSlowEffects
  - MaterialEmphasized
  - MaterialEmphasizedDecelerate
  - MaterialEmphasizedAccelerate
- Removed Overshoot from TALInterpolatedAnimation.
- Added imageTintColor to TALBrush to allow tinting images with a defined color.
- Added support for image tint in HTML via:
  `<img src="{ResourceName}" color="#FFFFFF or {ColorKey} or inherit">`
- Removed ALConvertFontFamily and replaced it with ALResolveFontFamily.
- Set TextSettings.IsHtml to true by default.
- HasTouchScreen is now disabled in debug mode on devices without a touchscreen.
- Added TALAlignLayout.contents.
- Introduced new TALDialog component.

#### 05/08/2025

- Added `ColorKey` to pair with each color property.
- Introduced `TALStyleManager`.
- In dark mode, resource loading now first checks for `xxx_dark` before loading `xxx`.
- Renamed `TALIniFile` to `TALIniFileA`.
- Added `TALUserPreferences`.

#### 04/24/2025

- Rename **alcinoe-common.jar** to **alcinoe-broadcastreceiver.jar**

#### 04/22/2025

- **TALGeoPositionSensor** has been replaced by **TALGeoLocationSensor**.

#### 04/13/2025

- **TALTabControl** has been replaced by **TALPageController**.
- **TALPageIndicator** has been added.
- **TALBufDrawableCacheEngine** now inherits from **TALRefCountObject**.
- Renamed **TALBounds.OnChange** to **TALBounds.OnChanged**.
- Renamed **TALPosition.OnChange** to **TALPosition.OnChanged**.
- Added the function **ALGetHasTouchScreen**.
- Removed **TALControl.BeginTextUpdate** and **TALControl.EndTextUpdate**.
- Added Metal support to **TALColorAdjustEffect**, removed Highlights and 
  Shadows, and changed the Contrast parameter to accept values from -1 to 1 (instead of -2 to 2).
- Added **TALRefCountBitmap**.
- Removed *AutoShowing* and *Opacity* from **TALScrollEngine**; these properties 
  are now directly managed in **TALScrollBox.TScrollBar**.
- Added **TALScrollEngine.startScroll** and introduced **TALScrollEngine.TouchMode** 
  (with options: Disabled, Enabled, Auto) as well as **TALScrollEngine.Friction**.
- Added the fill property and published ScrollEngine to **TALScrollBox**.
- Removed **HasTouchScreen**, **AutoHide**, and **ShowScrollBars** from **TALScrollEngine**.
- Changed **TALScrollBox.TViewportPositionChangeEvent** from:
  ```
  procedure (Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF) of object;
  ```
  to
  ```
  procedure (Sender: TObject; const OldViewportPosition, NewViewportPosition: TALPointD) of object;
  ```
  
#### 02/02/2025

- Replace **RotationCenter** with **Pivot** ([RSS-2824](https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2824)).
- The **Scale** property of controls now only affects their visual size (zoom in/out) and 
  no longer impacts their alignment within the parent layout ([RSS-2823](https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2823)).
  
#### 20/12/2024

- Added `FillMaskResourceName`, `FillMaskBitmap`, `FillImageNoRadius`, `FillCropCenter`, 
  and `FillBlurRadius` properties to `TALMultiLineTextOptions`.  
- Introduced global variables `ALBrokenImageResourceName`, `ALBrokenImageWidth`, and 
  `ALBrokenImageHeight` to define the image shown when downloading an image in `TALImage` fails.  
- Added the `TALPosition` component, a lightweight version of `TPosition`.  
- Removed `ALGetResourceDirectory`.  
- Introduced the `TALBitmap` type, along with the `ALNullBitmap` constant and the 
  `ALIsBitmapNull` and `ALFreeAndNilBitmap` functions.  
- Renamed the following functions:  
  - `ALCreateBitmapFromSkPixmap`, `ALCreateBitmapFromSkSurface`, `ALCreateBitmapFromSkImage` 
     to `ALCreateTBitmapFromSkPixmap`, `ALCreateTBitmapFromSkSurface`, `ALCreateTBitmapFromSkImage`.  
  - `ALUpdateBitmapFromSkPixmap`, `ALUpdateBitmapFromSkSurface`, `ALUpdateBitmapFromSkImage` 
     to `ALUpdateTBitmapFromSkPixmap`, `ALUpdateTBitmapFromSkSurface`, `ALUpdateTBitmapFromSkImage`.  
  - `ALCreateBitmapFromCGContextRef`, `ALUpdateBitmapFromCGContextRef` to 
    `ALCreateTBitmapFromCGContextRef`, `ALUpdateTBitmapFromCGContextRef`.  
  - `ALCreateTextureFromBitmapSurface`, `ALCreateTextureFromBitmap`, `ALUpdateTextureFromBitmapSurface`, 
    `ALUpdateTextureFromBitmap` to `ALCreateTextureFromTBitmapSurface`, `ALCreateTextureFromTBitmap`, 
    `ALUpdateTextureFromTBitmapSurface`, `ALUpdateTextureFromTBitmap`.  
- Corrected typo in `ALGetImageDimensions`.  
- Added `ALGetExifOrientationInfo` to retrieve ExifOrientationInfo from a stream as 
  well as from a file name.  
- Added `AFillResourceStream` parameter to `ALGetShapeSurfaceRect`.  
- Removed all auto-generated `ALLoadFromxxx` functions from `alcinoe.FMX.Graphics`. 
  Use `ALCreatexxxFromResource` functions instead.  
- Replaced the `ALDrawRectangle` function with the `TALDrawRectangleHelper` record.  
- Removed the `ALDrawCircle` function; use `ALDrawRectangle` with radius instead.  
- Updated `TALImage` to support downloading images from the Internet. Added 
  `MaskResourceName`, `MaskBitmap`, `BackgroundColor`, `LoadingColor`, `BlurRadius`, 
  `Corners`, `CropCenter`, `Shadow`, `Sides`, `Stroke`, `XRadius`, and `YRadius` properties.  
- Added the `TALEllipse` component.  
- Introduced the `AlIsHttpOrHttpsUrl` function.

#### 15/12/2024

- **Default Fill Color Change**  
  The default fill color of `TALBrush` has been updated to **white** (previously `$FFE0E0E0`).  
  This change affects the default fill color of components such as `TALRectangle` and `TALCircle`.  

- **Removal of `TALFont.AutoConvert` Property**  
  The `AutoConvert` property has been removed from `TALFont`.  
  Its behavior is now always **enabled** (equivalent to `AutoConvert = True`).