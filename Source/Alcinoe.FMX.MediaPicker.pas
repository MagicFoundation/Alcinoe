unit Alcinoe.FMX.MediaPicker;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.Messaging,
  System.Generics.Collections,
  {$IF defined(android)}
  Androidapi.JNI.Net,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.UIKit,
  Alcinoe.iOSApi.PhotosUI,
  {$ENDIF}
  System.SysUtils;

type

  {*****************************}
  TALMediaPicker = class(TObject)
  private
    class function CreateInstance: TALMediaPicker;
    class function GetInstance: TALMediaPicker; static;
  protected
    class var FInstance: TALMediaPicker;
  public
    type
      TCreateInstanceFunc = function: TALMediaPicker;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALMediaPicker read GetInstance;
    class function HasInstance: Boolean; inline;
  public
    type
      // ----------
      // TMediaType
      TMediaType = (Unknown, Image, Video);
      TMediaTypes = set of TMediaType;
      // ----------
      // TMediaItem
      TMediaItem = class(TObject)
      private
        FUri: String;
        FMediaType: TMediaType;
      public
        Constructor create(
                      const AUri: String;
                      const AMediaType: TMediaType); virtual;
        property Uri: string read FUri;
        property MediaType: TMediaType read FMediaType;
        function ExtractStream: TStream;
      end;
      // ---------------
      // TOnSuccessEvent
      TOnSuccessEvent = procedure(const AItems: TArray<TMediaItem>) of object;
      // --------------
      // TOnCancelEvent
      TOnCancelEvent = procedure of object;
      // -------------
      // TOnErrorEvent
      TOnErrorEvent = procedure(const AMsg: string) of object;
      // ------------------------------------
      // TShowRequestPermissionRationaleEvent
      TShowRequestPermissionRationaleEvent = Procedure(
                                               const AToRequestCameraPermission: Boolean;
                                               const AToRequestMicPermission: Boolean;
                                               const ACanRequestPermissionProc: TProc; // the procedure to launch when the user response positivelly to the rationale
                                               const ACanNotRequestPermissionProc: TProc) of object; // the procedure to launch when the user response negativelly to the rationale

  private
    {$IF defined(IOS)}
    type
      // --------------------------------------------
      // TItemProviderloadFileRepresentationForwarder
      TItemProviderloadFileRepresentationForwarder = class(TObject)
      private
        FMediaPicker: TALMediaPicker;
        FMediaType: TMediaType;
        FMediaIndex: Integer;
      public
        constructor Create(const AMediaPicker: TALMediaPicker; const AMediaType: TMediaType; const AMediaIndex: Integer);
        procedure CompletionHandler(url: NSURL; error: NSError);
      end;
      // -----------------------------
      // TPickerViewControllerDelegate
      TPickerViewControllerDelegate = class(TOCLocal, PHPickerViewControllerDelegate)
      private
        FMediaPicker: TALMediaPicker;
        FItemProviderloadFileRepresentationForwarders: TObjectList<TItemProviderloadFileRepresentationForwarder>;
      public
        constructor Create(const AMediaPicker: TALMediaPicker);
        destructor Destroy; override;
        procedure picker(picker: PHPickerViewController; didFinishPicking: NSArray); cdecl;
      end;
      // ------------------------------
      // TImagePickerControllerDelegate
      TImagePickerControllerDelegate = class(TOCLocal, UIImagePickerControllerDelegate)
      private
        FMediaPicker: TALMediaPicker;
      public
        constructor Create(const AMediaPicker: TALMediaPicker);
        procedure imagePickerController(picker: UIImagePickerController; didFinishPickingMediaWithInfo: NSDictionary); overload; cdecl;
        procedure imagePickerController(picker: UIImagePickerController; didFinishPickingImage: UIImage; editingInfo: NSDictionary); overload; cdecl;
        procedure imagePickerControllerDidCancel(picker: UIImagePickerController); cdecl;
      end;
    {$ENDIF}
  private
    {$IF defined(android)}
    const REQUEST_CODE_PICK_MEDIA = 21001;
    const REQUEST_CODE_CAPTURE_IMAGE = 21002;
    const REQUEST_CODE_CAPTURE_VIDEO = 21003;
    {$ENDIF}
  private
    FCaptureFromCameraMediaType: TMediaType;
    FOnSuccess: TOnSuccessEvent;
    FOnCancel: TOnCancelEvent;
    FOnError: TOnErrorEvent;
    {$IF defined(IOS)}
    FPickerViewController: PHPickerViewController;
    FPickerViewControllerDelegate: TPickerViewControllerDelegate;
    FImagePickerController: UIImagePickerController;
    FImagePickerControllerDelegate: TImagePickerControllerDelegate;
    FApplicationEventHandlerEnabled: Boolean;
    FLoadFileRepresentationFailed: Boolean;
    FLoadFileRepresentationErrorMessage: String;
    FPendingMediaItemCount: integer;
    FMediaItems: TArray<TMediaItem>;
    procedure ApplicationEventHandler(const Sender: TObject; const M: TMessage);
    {$ENDIF}
    {$IF defined(ANDROID)}
    FCaptureUri: JNET_URI;
    procedure HandleActivityResult(const Sender: TObject; const AMsg: TMessage);
    {$ENDIF}
  protected
    procedure DoCaptureFromCamera; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure PickMedia(
                const AMediaTypes: TMediaTypes;
                const AMaxCount: Integer;
                const AOnSuccess: TOnSuccessEvent;
                const AOnCancel: TOnCancelEvent;
                const AOnError: TOnErrorEvent); virtual;
    procedure CaptureFromCamera(
                const AMediaType: TMediaType;
                const AOnSuccess: TOnSuccessEvent;
                const AOnCancel: TOnCancelEvent;
                const AOnError: TOnErrorEvent;
                const AShowRequestPermissionRationaleEvent: TShowRequestPermissionRationaleEvent); virtual;
  end;

implementation

uses
  {$IF defined(DEBUG)}
  System.Rtti,
  {$ENDIF}
  System.math,
  System.IOUtils,
  {$IF defined(android)}
  AndroidApi.JNI,
  Androidapi.JNI.Support,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Provider,
  AndroidApi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.Helpers,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.Helpers,
  iOSapi.AVFoundation,
  FMX.Helpers.iOS,
  Alcinoe.iOSApi.Photos,
  {$ENDIF}
  {$IF defined(MSWindows)}
  System.UITypes,
  FMX.Dialogs,
  Alcinoe.Mime.ContentTypes,
  {$ENDIF}
  FMX.Platform,
  Alcinoe.Files,
  Alcinoe.StringUtils,
  Alcinoe.FMX.LoadingOverlay,
  ALcinoe.Common;

{*************}
//[MultiThread]
class function TALMediaPicker.CreateInstance: TALMediaPicker;
begin
  result := TALMediaPicker.Create;
end;

{*************}
//[MultiThread]
class function TALMediaPicker.GetInstance: TALMediaPicker;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance);
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALMediaPicker.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{*******************************************}
Constructor TALMediaPicker.TMediaItem.create(
              const AUri: String;
              const AMediaType: TMediaType);
begin
  FUri := AUri;
  FMediaType := AMediaType;
end;

{********************************************************}
function TALMediaPicker.TMediaItem.ExtractStream: TStream;
begin
  {$IF defined(ANDROID)}
  Result := TMemoryStream.Create;
  try
    var LUri := TJnet_Uri.JavaClass.parse(StringToJString(Uri));
    var LInputStream := TAndroidHelper.Context.getContentResolver.openInputStream(LUri);
    if LInputStream = nil then raise Exception.CreateFmt('Cannot open InputStream for URI: %s', [Uri]);
    try
      var LTotalSize: int64 := 0;
      var LProjection := TJavaObjectArray<JString>.Create(1);
      try
        LProjection.Items[0] := TJOpenableColumns.JavaClass.SIZE;
        var LCursor := TAndroidHelper.Context.getContentResolver.query(
                         LUri, // uri: Jnet_Uri
                         LProjection, // projection: TJavaObjectArray<JString>
                         nil, // selection: JString
                         nil, // selectionArgs: TJavaObjectArray<JString>
                         nil); // sortOrder: JString
        if (LCursor <> nil) then begin
          if LCursor.moveToFirst then begin
            var LSizeColumnIndex := LCursor.getColumnIndex(TJOpenableColumns.JavaClass.SIZE);
            if LSizeColumnIndex >= 0 then LTotalSize := LCursor.getLong(LSizeColumnIndex);
          end;
          LCursor.Close;
        end;
      finally
        ALFreeAndNil(LProjection);
      end;
      if LTotalSize <= 0 then LTotalSize := 1048576;
      Result.Size := LTotalSize;
      var LJBuffer := TJavaArray<Byte>.Create(LTotalSize);
      try
        var LRead: Integer;
        repeat
          LRead := LInputStream.read(LJBuffer, 0, LJBuffer.Length);
          if LRead > 0 then begin
            Result.Size := Max(Result.Size, Result.Position + LRead);
            Result.WriteBuffer(PByte(LJBuffer.Data)^, LRead);
          end;
        until LRead <= 0;
      finally
        ALFreeAndNil(LJBuffer);
      end;
      Result.Size:= Result.Position;
      Result.Position := 0;
    finally
      LInputStream.close;
    end;
  Except
    ALFreeAndNil(Result);
    Raise;
  end;
  {$ELSEIF defined(IOS)}
  Result := TFileStream.Create(Uri, fmOpenRead or fmShareDenyWrite);
  {$ELSEIF defined(MSWindows)}
  Result := TFileStream.Create(Uri, fmOpenRead or fmShareDenyWrite);
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

{****************}
{$IF defined(IOS)}
constructor TALMediaPicker.TItemProviderloadFileRepresentationForwarder.Create(const AMediaPicker: TALMediaPicker; const AMediaType: TMediaType; const AMediaIndex: Integer);
begin
  inherited Create;
  FMediaPicker := AMediaPicker;
  FMediaType := AMediaType;
  FMediaIndex := AMediaIndex;
end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALMediaPicker.TItemProviderloadFileRepresentationForwarder.CompletionHandler(url: NSURL; error: NSError);
begin

  {$IF defined(DEBUG)}
  ALLog('TALMediaPicker.TItemProviderloadFileRepresentationForwarder.CompletionHandler');
  {$ENDIF}

  ALMonitorEnter(FMediaPicker{$IF defined(DEBUG)}, 'TALMediaPicker.TItemProviderloadFileRepresentationForwarder.CompletionHandler'{$ENDIF});
  Try

    Try

      if FMediaPicker.FPendingMediaItemCount <= 0 then Raise Exception.Create('Error 7DEE89F0-223F-4D36-A8E1-B216392CEC87');
      Dec(FMediaPicker.FPendingMediaItemCount);

      if (error <> nil) or (url = nil) then begin
        FMediaPicker.FLoadFileRepresentationFailed := True;
        if error <> nil then FMediaPicker.FLoadFileRepresentationErrorMessage := NSStrToStr(error.localizedDescription);
      end
      else begin
        var LSourceFileName := NSStrToStr(URL.path); // /private/var/mobile/Containers/Data/Application/0E36F73C-10B8-4047-A4A1-34EE38567FDD/tmp/.com.apple.Foundation.NSItemProvider.QJqOFE/IMG_0084.mov
        var LDestFileName := ALGetTempFilenameW(ALExtractFileExt(LSourceFileName)); // /private/var/mobile/Containers/Data/Application/0E36F73C-10B8-4047-A4A1-34EE38567FDD/tmp/d85c5ff6-7822-4732-b104-1bb055004c51.mov
        if AlPosW(ALGetTempPathW, LSourceFileName) = 1 then TFile.move(LSourceFileName{SourceFileName}, LDestFileName{DestFileName})
        else TFile.copy(LSourceFileName{SourceFileName}, LDestFileName{DestFileName});
        {$IF defined(DEBUG)}
        if FMediaIndex > high(FMediaPicker.FMediaItems) then raise Exception.Create('Error 698683F0-D90B-4DCD-B935-5C930DFD14AE');
        if FMediaPicker.FMediaItems[FMediaIndex] <> nil then raise Exception.Create('Error 3D2522A6-7156-4174-A21A-D6CBFF731EFF');
        {$ENDIF}
        FMediaPicker.FMediaItems[FMediaIndex] := TMediaItem.Create(LDestFileName, FMediaType);
      end;

      if FMediaPicker.FPendingMediaItemCount = 0 then begin
        TThread.Synchronize(nil,
          procedure
          begin
            TALLoadingOverlay.CloseCurrent;
            if not FMediaPicker.FLoadFileRepresentationFailed then begin
              If Assigned(FMediaPicker.FOnSuccess) then
                FMediaPicker.FOnSuccess(FMediaPicker.FMediaItems);
            end
            else begin
              if Assigned(FMediaPicker.FOnError) then
                FMediaPicker.FOnError(FMediaPicker.FLoadFileRepresentationErrorMessage);
            end;
          end);
        //--
        For var I := low(FMediaPicker.FMediaItems) to high(FMediaPicker.FMediaItems) do
          ALFreeAndNil(FMediaPicker.FMediaItems[I]);
        setlength(FMediaPicker.FMediaItems, 0);
      end;

    except
      on E: Exception do begin
        if FMediaPicker.FPendingMediaItemCount > 0 then begin
          FMediaPicker.FLoadFileRepresentationFailed := True;
          FMediaPicker.FLoadFileRepresentationErrorMessage := E.Message;
        end
        else begin
          TThread.Synchronize(nil,
            procedure
            begin
              TALLoadingOverlay.CloseCurrent;
              if Assigned(FMediaPicker.FOnError) then
                FMediaPicker.FOnError(E.Message);
            end);
        end;
      end;
    end;

  finally
    ALMonitorExit(FMediaPicker{$IF defined(DEBUG)}, 'TALMediaPicker.TItemProviderloadFileRepresentationForwarder.CompletionHandler'{$ENDIF});
  end;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
constructor TALMediaPicker.TPickerViewControllerDelegate.Create(const AMediaPicker: TALMediaPicker);
begin
  inherited Create;
  FMediaPicker := AMediaPicker;
  FItemProviderloadFileRepresentationForwarders := TObjectList<TItemProviderloadFileRepresentationForwarder>.Create(true{AOwnsObjects});
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
destructor TALMediaPicker.TPickerViewControllerDelegate.Destroy;
begin
  ALFreeAndNil(FItemProviderloadFileRepresentationForwarders);
  inherited;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
procedure TALMediaPicker.TPickerViewControllerDelegate.picker(picker: PHPickerViewController; didFinishPicking: NSArray);
begin
  try

    {$IF defined(DEBUG)}
    ALLog('TALMediaPicker.TPickerViewControllerDelegate.picker');
    {$ENDIF}

    // UnblockUserInteraction
    var LWindow := SharedApplication.keyWindow;
    if (LWindow <> nil) and (LWindow.rootViewController <> nil) then
      LWindow.rootViewController.dismissViewControllerAnimated(true{flag}, nil{completion});

    // Empty selection = cancel
    if (didFinishPicking = nil) or (didFinishPicking.count = 0) then begin
      TALLoadingOverlay.CloseCurrent;
      if Assigned(FMediaPicker.FOnCancel) then
        FMediaPicker.FOnCancel;
      Exit;
    end;

    // Init some vars
    FMediaPicker.FLoadFileRepresentationFailed := False;
    FMediaPicker.FLoadFileRepresentationErrorMessage := '';
    FMediaPicker.FPendingMediaItemCount := didFinishPicking.count;
    For var I := low(FMediaPicker.FMediaItems) to high(FMediaPicker.FMediaItems) do
      ALFreeAndNil(FMediaPicker.FMediaItems[i]);
    setlength(FMediaPicker.FMediaItems, didFinishPicking.count);
    For var I := low(FMediaPicker.FMediaItems) to high(FMediaPicker.FMediaItems) do
      FMediaPicker.FMediaItems[i] := nil;
    FItemProviderloadFileRepresentationForwarders.Clear;

    // loadFileRepresentationForTypeIdentifier for each PHPickerResult
    ALMonitorEnter(FMediaPicker{$IF defined(DEBUG)}, 'TALMediaPicker.TPickerViewControllerDelegate.picker'{$ENDIF});
    Try
      TALLoadingOverlay.Builder.Show;
      var LMediaIndex: integer := 0;
      for var I := 0 to didFinishPicking.count - 1 do begin
        var LPHPickerResult := TPHPickerResult.Wrap(didFinishPicking.objectAtIndex(I));
        if LPHPickerResult.itemProvider.hasItemConformingToTypeIdentifier(StrToNSStr('public.image')) then begin
          var LItemProviderloadFileRepresentationForwarder := TItemProviderloadFileRepresentationForwarder.Create(FMediaPicker, TMediaType.Image, LMediaIndex);
          FItemProviderloadFileRepresentationForwarders.Add(LItemProviderloadFileRepresentationForwarder);
          LPHPickerResult.itemProvider.loadFileRepresentationForTypeIdentifier(StrToNSStr('public.image'), LItemProviderloadFileRepresentationForwarder.CompletionHandler);
          inc(LMediaIndex);
        end
        else if LPHPickerResult.itemProvider.hasItemConformingToTypeIdentifier(StrToNSStr('public.movie')) then begin
          var LItemProviderloadFileRepresentationForwarder := TItemProviderloadFileRepresentationForwarder.Create(FMediaPicker, TMediaType.Video, LMediaIndex);
          FItemProviderloadFileRepresentationForwarders.Add(LItemProviderloadFileRepresentationForwarder);
          LPHPickerResult.itemProvider.loadFileRepresentationForTypeIdentifier(StrToNSStr('public.movie'), LItemProviderloadFileRepresentationForwarder.CompletionHandler);
          inc(LMediaIndex);
        end
        else
          Dec(FMediaPicker.FPendingMediaItemCount);
      end;
      setlength(FMediaPicker.FMediaItems, LMediaIndex);
      if FMediaPicker.FPendingMediaItemCount = 0 then
        raise Exception.Create('Error 297888FC-B3A7-460C-81BE-83D5DAFC7903');
    finally
      ALMonitorExit(FMediaPicker{$IF defined(DEBUG)}, 'TALMediaPicker.TPickerViewControllerDelegate.picker'{$ENDIF});
    end;

  except
    on E: Exception do begin
      TALLoadingOverlay.CloseCurrent;
      if Assigned(FMediaPicker.FOnError) then
        FMediaPicker.FOnError(E.Message);
    end;
  end;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
constructor TALMediaPicker.TImagePickerControllerDelegate.Create(const AMediaPicker: TALMediaPicker);
begin
  inherited Create;
  FMediaPicker := AMediaPicker;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
procedure TALMediaPicker.TImagePickerControllerDelegate.imagePickerController(picker: UIImagePickerController; didFinishPickingMediaWithInfo: NSDictionary);
begin
  try
    {$IF defined(DEBUG)}
    //ALLog('TALMediaPicker.TImagePickerControllerDelegate.imagePickerController:picker:didFinishPickingMediaWithInfo');
    {$ENDIF}

    TALLoadingOverlay.CloseCurrent;

    var LWindow := SharedApplication.keyWindow;
    if (LWindow <> nil) and (LWindow.rootViewController <> nil) then
      LWindow.rootViewController.dismissViewControllerAnimated(true{flag}, nil{completion});

    var LMediaType: TMediaType;
    var LMediaTypeStr := NSStrToStr(TNSString.Wrap(didFinishPickingMediaWithInfo.objectForKey(NSStringToID(UIImagePickerControllerMediaType))));
    If LMediaTypeStr = 'public.image' then LMediaType := TMediaType.Image
    else If LMediaTypeStr = 'public.movie' then LMediaType := TMediaType.video
    else Raise Exception.createfmt('Unsupported media type "%s" (expected "public.image" or "public.movie")', [LMediaTypeStr]);

    var LSourceFileName: String;
    if LMediaType = TMediaType.image then begin
      var LImageURL := TNSURL.Wrap(didFinishPickingMediaWithInfo.objectForKey(NSStringToID(UIImagePickerControllerImageURL)));
      if LImageURL <> nil then LSourceFileName := NSStrToStr(LImageURL.path)
      else LSourceFileName := ''
    end
    else if LMediaType = TMediaType.video then begin
      var LMediaURL := TNSURL.Wrap(didFinishPickingMediaWithInfo.objectForKey(NSStringToID(UIImagePickerControllerMediaURL)));
      if LMediaURL <> nil then LSourceFileName := NSStrToStr(LMediaURL.path)
      else LSourceFileName := ''
    end
    else raise Exception.Create('Error F57F98EE-0162-4831-8F88-4C7319211694');

    // /private/var/mobile/Containers/Data/PluginKitPlugin/393DBD99-4FCF-429A-A7F0-F1CE53D4896B/tmp/trim.4807C574-BC43-4952-A3A5-136BDB5C617A.MOV
    // /private/var/mobile/Containers/Data/Application/C515B5F4-BE1B-4698-97B0-5B36A8B74C71/tmp/0F2D80ED-E059-4458-9210-8712DF8B43F5.jpeg
    if LSourceFileName <> '' then begin
      {$IF defined(DEBUG)}
      ALLog('TALMediaPicker.TImagePickerControllerDelegate.imagePickerController:picker:didFinishPickingMediaWithInfo', LSourceFileName);
      {$ENDIF}
      var LDestFileName := ALGetTempFilenameW(ALExtractFileExt(LSourceFileName)); // /private/var/mobile/Containers/Data/Application/0E36F73C-10B8-4047-A4A1-34EE38567FDD/tmp/d85c5ff6-7822-4732-b104-1bb055004c51.mov
      if AlPosW(ALGetTempPathW, LSourceFileName) = 1 then TFile.move(LSourceFileName{SourceFileName}, LDestFileName{DestFileName})
      else TFile.copy(LSourceFileName{SourceFileName}, LDestFileName{DestFileName});
      var LItems: TArray<TMediaItem>;
      SetLength(LItems, 1);
      LItems[0] := TMediaItem.Create(LDestFileName, LMediaType);
      try
        If Assigned(FMediaPicker.FOnSuccess) then
          FMediaPicker.FOnSuccess(LItems);
      finally
        For var I := low(LItems) to high(LItems) do
          ALFreeAndNil(LItems[I]);
      end;
    end
    else if LMediaType = TMediaType.image then begin
      {$IF defined(DEBUG)}
      ALLog('TALMediaPicker.TImagePickerControllerDelegate.imagePickerController:picker:didFinishPickingMediaWithInfo', '<UIImage>');
      {$ENDIF}
      var LImage := TUIImage.Wrap(didFinishPickingMediaWithInfo.objectForKey(NSStringToID(UIImagePickerControllerOriginalImage)));
      if LImage = nil then raise Exception.Create('No image returned by the camera');
      var LData := TNSData.wrap(UIImageJPEGRepresentation(NSObjectTOID(LImage), 0.85));
      if LData = nil then raise Exception.Create('Failed to encode image to JPEG.');
      var LDestFileName := ALGetTempFilenameW('.jpg'); // /private/var/mobile/Containers/Data/Application/0E36F73C-10B8-4047-A4A1-34EE38567FDD/tmp/d85c5ff6-7822-4732-b104-1bb055004c51.jpg
      var LUrl := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(LDestFileName)));
      if not LData.writeToURL(LURL, True{atomically}) then raise Exception.Create('Failed to write image file');
      var LItems: TArray<TMediaItem>;
      SetLength(LItems, 1);
      LItems[0] := TMediaItem.Create(LDestFileName, LMediaType);
      try
        If Assigned(FMediaPicker.FOnSuccess) then
          FMediaPicker.FOnSuccess(LItems);
      finally
        For var I := low(LItems) to high(LItems) do
          ALFreeAndNil(LItems[I]);
      end;
    end
    else
      raise Exception.Create('Resolved URI is empty after processing selection');

  except
    on E: Exception do begin
      if Assigned(FMediaPicker.FOnError) then
        FMediaPicker.FOnError(E.Message);
    end;
  end;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
procedure TALMediaPicker.TImagePickerControllerDelegate.imagePickerController(picker: UIImagePickerController; didFinishPickingImage: UIImage; editingInfo: NSDictionary);
begin
  // iOS 2.0–3.0 Deprecated
  {$IF defined(DEBUG)}
  ALLog(
    'TALMediaPicker.TImagePickerControllerDelegate.imagePickerController:picker:didFinishPickingImage:editingInfo',
    'Unexpected call to deprecated didFinishPickingImage:editingInfo',
    TALLogType.Error);
  {$ENDIF}
  TALLoadingOverlay.CloseCurrent;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
procedure TALMediaPicker.TImagePickerControllerDelegate.imagePickerControllerDidCancel(picker: UIImagePickerController);
begin
  try

    {$IF defined(DEBUG)}
    ALLog('TALMediaPicker.TImagePickerControllerDelegate.imagePickerControllerDidCancel');
    {$ENDIF}

    TALLoadingOverlay.CloseCurrent;

    var LWindow := SharedApplication.keyWindow;
    if (LWindow <> nil) and (LWindow.rootViewController <> nil) then
      LWindow.rootViewController.dismissViewControllerAnimated(true{flag}, nil{completion});

    if Assigned(FMediaPicker.FOnCancel) then
      FMediaPicker.FOnCancel;

  except
    on E: Exception do begin
      if Assigned(FMediaPicker.FOnError) then
        FMediaPicker.FOnError(E.Message);
    end;
  end;
end;
{$ENDIF}

{********************************}
constructor TALMediaPicker.Create;
begin
  inherited Create;
  FCaptureFromCameraMediaType := TMediaType.Unknown;
  FOnSuccess := nil;
  FOnCancel := nil;
  FOnError := nil;
  {$IF defined(IOS)}
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);
  FPickerViewController := nil;
  FPickerViewControllerDelegate := nil;
  FImagePickerController := nil;
  FImagePickerControllerDelegate := nil;
  FApplicationEventHandlerEnabled := False;
  FLoadFileRepresentationFailed := False;
  FLoadFileRepresentationErrorMessage := '';
  FPendingMediaItemCount := 0;
  Setlength(FMediaItems, 0);
  {$ENDIF}
  {$IF defined(ANDROID)}
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, HandleActivityResult);
  FCaptureUri := nil;
  {$ENDIF}
end;

{********************************}
destructor TALMediaPicker.Destroy;
begin
  {$IF defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventHandler);
  if FPickerViewController <> nil then begin
    FPickerViewController.setDelegate(nil);
    FPickerViewController.release;
  end;
  ALFreeAndNil(FPickerViewControllerDelegate);

  if FImagePickerController <> nil then begin
    FImagePickerController.setDelegate(nil);
    FImagePickerController.release;
  end;
  ALFreeAndNil(FImagePickerControllerDelegate);

  For var I := low(FMediaItems) to high(FMediaItems) do
    ALFreeAndNil(FMediaItems[I]);
  {$ENDIF}
  {$IF defined(ANDROID)}
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, HandleActivityResult);
  {$ENDIF}
  inherited;
end;

{*********************************}
procedure TALMediaPicker.PickMedia(
            const AMediaTypes: TMediaTypes;
            const AMaxCount: Integer;
            const AOnSuccess: TOnSuccessEvent;
            const AOnCancel: TOnCancelEvent;
            const AOnError: TOnErrorEvent);
begin
  try

    FOnSuccess := AOnSuccess;
    FOnCancel := AOnCancel;
    FOnError := AOnError;
    TALLoadingOverlay.Builder.SetStealthMode.Show;

    {$REGION 'ANDROID'}
    {$IF defined(ANDROID)}
    if TOSVersion.Check(13, 0) {API level >= 33 (Android TIRAMISU)} then begin
      var LIntent := TJIntent.JavaClass.init(TJMediaStore.JavaClass.ACTION_PICK_IMAGES);
      if (AMediaTypes = [TMediaType.Image]) then LIntent.setType(StringToJString('image/*'))
      else if (AMediaTypes = [TMediaType.Video]) then LIntent.setType(StringToJString('video/*'));
      LIntent.putExtra(TJMediaStore.JavaClass.EXTRA_PICK_IMAGES_MAX, Max(1, AMaxCount));
      TAndroidHelper.Activity.startActivityForResult(LIntent, REQUEST_CODE_PICK_MEDIA);
      exit;
    end;

    var LIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_GET_CONTENT);
    if (AMediaTypes = [TMediaType.Image]) then LIntent.setType(StringToJString('image/*'))
    else if (AMediaTypes = [TMediaType.Video]) then LIntent.setType(StringToJString('video/*'))
    else begin
      LIntent.setType(StringToJString('*/*'));
      var LArray := TJavaObjectArray<JString>.Create(2);
      Try
        LArray.Items[0] := StringToJString('image/*');
        LArray.Items[1] := StringToJString('video/*');
        LIntent.putExtra(TJIntent.JavaClass.EXTRA_MIME_TYPES, LArray);
      finally
        ALFreeAndNil(LArray);
      end;
    end;
    if AMaxCount > 1 then LIntent.putExtra(TJIntent.JavaClass.EXTRA_ALLOW_MULTIPLE, true);
    LIntent.addCategory(TJIntent.JavaClass.CATEGORY_OPENABLE);
    TAndroidHelper.Activity.startActivityForResult(LIntent, REQUEST_CODE_PICK_MEDIA);
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'IOS'}
    {$IF defined(IOS)}
    FApplicationEventHandlerEnabled := False;

    if TOSVersion.Check(14) then begin
      // Note: Using init (instead of initWithPhotoLibrary) yields no assetIdentifier in results.
      var LPickerConfiguration := TPHPickerConfiguration.wrap(TPHPickerConfiguration.OCClass.alloc).initWithPhotoLibrary(TPHPhotoLibrary.OCClass.sharedPhotoLibrary);
      try
        if (AMediaTypes = [TMediaType.Image]) then LPickerConfiguration.setFilter(TPHPickerFilter.OCClass.imagesFilter)
        else if (AMediaTypes = [TMediaType.Video]) then LPickerConfiguration.setFilter(TPHPickerFilter.OCClass.videosFilter)
        else begin
          var LSubfilter := TNSMutableArray.Create;
          try
            LSubfilter.addObject(NSObjectToID(TPHPickerFilter.OCClass.imagesFilter));
            LSubfilter.addObject(NSObjectToID(TPHPickerFilter.OCClass.videosFilter));
            LPickerConfiguration.setFilter(TPHPickerFilter.OCClass.anyFilterMatchingSubfilters(LSubfilter));
          finally
            LSubfilter.release;
          end;
        end;
        LPickerConfiguration.setSelectionLimit(Max(1, AMaxCount));
        if FPickerViewControllerDelegate = nil then
          FPickerViewControllerDelegate := TPickerViewControllerDelegate.Create(Self);
        if FPickerViewController <> nil then
          FPickerViewController.release;
        FPickerViewController := TPHPickerViewController.Wrap(TPHPickerViewController.OCClass.alloc).initWithConfiguration(LPickerConfiguration);
        FPickerViewController.setDelegate(FPickerViewControllerDelegate);
        var LWindow := SharedApplication.keyWindow;
        if (LWindow <> nil) and (LWindow.rootViewController <> nil) then
          LWindow.rootViewController.presentViewController(FPickerViewController{viewControllerToPresent }, True{animated}, nil{completion})
        else
          Raise Exception.Create('Error 5CB8C6A5-789D-4742-9A54-D6C39431EEFF');
      finally
        LPickerConfiguration.release;
      end;
    end
    else begin
      if not TUIImagePickerController.OCClass.isSourceTypeAvailable(UIImagePickerControllerSourceTypePhotoLibrary) then
        raise Exception.Create('Photo library not available on this device');
      if FImagePickerController = nil then begin
        FImagePickerController := TUIImagePickerController.Create;
        if FImagePickerControllerDelegate = nil then
          FImagePickerControllerDelegate := TImagePickerControllerDelegate.Create(Self);
        FImagePickerController.setDelegate(FImagePickerControllerDelegate.GetObjectID);
      end;
      FImagePickerController.setSourceType(UIImagePickerControllerSourceTypePhotoLibrary);
      if (AMediaTypes = [TMediaType.Image]) then FImagePickerController.setMediaTypes(TNSArray.Wrap(TNSArray.OCClass.arrayWithObject(StringToID('public.image'))))
      else if (AMediaTypes = [TMediaType.Video]) then FImagePickerController.setMediaTypes(TNSArray.Wrap(TNSArray.OCClass.arrayWithObject(StringToID('public.movie'))))
      else begin
        var LMediaTypes := TNSMutableArray.Create;
        try
          LMediaTypes.addObject(StringToID('public.image'));
          LMediaTypes.addObject(StringToID('public.movie'));
          FImagePickerController.setMediaTypes(LMediaTypes);
        finally
          LMediaTypes.release;
        end;
      end;
      var LWindow := SharedApplication.keyWindow;
      if (LWindow <> nil) and (LWindow.rootViewController <> nil) then
        LWindow.rootViewController.presentViewController(FImagePickerController{viewControllerToPresent }, True{animated}, nil{completion})
      else
        Raise Exception.Create('Error 5CB8C6A5-789D-4742-9A54-D6C39431EEFF');
    end;
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'MSWindows'}
    {$IF defined(MSWindows)}
    var LOpenDialog := TOpenDialog.Create(nil);
    try
      if (AMediaTypes = [TMediaType.Image]) then LOpenDialog.Filter :=  'Image files (*.jpg;*.jpeg;*.png;*.bmp)|*.jpg;*.jpeg;*.png;*.bmp'
      else if (AMediaTypes = [TMediaType.Video]) then LOpenDialog.Filter :=  'Video files (*.avi;*.mp4;*.mov;*.mkv)|*.avi;*.mp4;*.mov;*.mkv'
      else LOpenDialog.Filter := 'All media (*.jpg;*.jpeg;*.png;*.bmp;*.avi;*.mp4;*.mov;*.mkv)|*.jpg;*.jpeg;*.png;*.bmp;*.avi;*.mp4;*.mov;*.mkv|Image files (*.jpg;*.jpeg;*.png;*.bmp)|*.jpg;*.jpeg;*.png;*.bmp|Video files (*.avi;*.mp4;*.mov;*.mkv)|*.avi;*.mp4;*.mov;*.mkv';
      LOpenDialog.FilterIndex := 1;
      if AMaxCount > 1 then LOpenDialog.Options := LOpenDialog.Options + [TOpenOption.ofAllowMultiSelect];
      if LOpenDialog.Execute then begin
        TALLoadingOverlay.CloseCurrent;
        var LItems: TArray<TMediaItem>;
        SetLength(LItems, LOpenDialog.Files.Count);
        For var I := 0 to LOpenDialog.Files.Count - 1 do begin
          var LMediaType: TMediaType;
          var LMimeContentType := ALGetDefaultMimeContentTypeFromExt(ALExtractFileExt(LOpenDialog.Files[I]));
          If alposW('image/', LMimeContentType) = 1 then LMediaType := TMediaType.Image
          else If alposW('video/', LMimeContentType) = 1 then LMediaType := TMediaType.video
          else LMediaType := TMediaType.Unknown;
          LItems[I] := TMediaItem.Create(
                         LOpenDialog.Files[I],
                         LMediaType);
        end;
        try
          If Assigned(FOnSuccess) then
            FOnSuccess(LItems);
        finally
          For var I := low(LItems) to high(LItems) do
            ALFreeAndNil(LItems[I]);
        end;
      end
      else begin
        TALLoadingOverlay.CloseCurrent;
        If Assigned(FOnCancel) then
          FOnCancel();
      end;
    finally
      ALFreeAndNil(LOpenDialog);
    end;
    {$ENDIF}
    {$ENDREGION}

  except
    on E: Exception do begin
      TALLoadingOverlay.CloseCurrent;
      if Assigned(FOnError) then
        FOnError(E.Message);
    end;
  end;
end;

{*******************************************}
procedure TALMediaPicker.DoCaptureFromCamera;
begin
  try

    {$REGION 'ANDROID'}
    {$IF defined(ANDROID)}
    if FCaptureFromCameraMediaType = TMediaType.Image then begin
      var LFileName := ALGetTempFilenameW('.jpg');  // /data/data/io.magicfoundation.alcinoe.ALFmxMediaPickerDemo/cache/tmp/d85c5ff6-7822-4732-b104-1bb055004c51.jpg
      var LFile := TJFile.JavaClass.init(StringToJString(LFileName));
      FCaptureUri := TAndroidHelper.JFileToJURI(LFile);
      var LIntent := TJIntent.JavaClass.init(TJMediaStore.JavaClass.ACTION_IMAGE_CAPTURE);
      LIntent.putExtra(TJMediaStore.JavaClass.EXTRA_OUTPUT, TJParcelable.Wrap(FCaptureUri));
      LIntent.addFlags(TJIntent.JavaClass.FLAG_GRANT_WRITE_URI_PERMISSION or TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
      TAndroidHelper.Activity.startActivityForResult(LIntent, REQUEST_CODE_CAPTURE_IMAGE);
    end
    else if FCaptureFromCameraMediaType = TMediaType.Video then begin
      var LFileName := ALGetTempFilenameW('.mp4');  // /data/data/io.magicfoundation.alcinoe.ALFmxMediaPickerDemo/cache/tmp/d85c5ff6-7822-4732-b104-1bb055004c51.mp4
      var LFile := TJFile.JavaClass.init(StringToJString(LFileName));
      FCaptureUri := TAndroidHelper.JFileToJURI(LFile);
      var LIntent := TJIntent.JavaClass.init(TJMediaStore.JavaClass.ACTION_VIDEO_CAPTURE);
      LIntent.putExtra(TJMediaStore.JavaClass.EXTRA_OUTPUT, TJParcelable.Wrap(FCaptureUri));
      LIntent.addFlags(TJIntent.JavaClass.FLAG_GRANT_WRITE_URI_PERMISSION or TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
      TAndroidHelper.Activity.startActivityForResult(LIntent, REQUEST_CODE_CAPTURE_VIDEO);
    end
    else
      Raise Exception.Create('Unsupported media type, only Image and Video are supported');
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'IOS'}
    {$IF defined(IOS)}
    if FImagePickerController = nil then begin
      FImagePickerController := TUIImagePickerController.Create;
      if FImagePickerControllerDelegate = nil then
        FImagePickerControllerDelegate := TImagePickerControllerDelegate.Create(Self);
      FImagePickerController.setDelegate(FImagePickerControllerDelegate.GetObjectID);
    end;
    FImagePickerController.setSourceType(UIImagePickerControllerSourceTypeCamera);
    var LMediaTypes: NSArray;
    case FCaptureFromCameraMediaType of
      TMediaType.Image: LMediaTypes := TNSArray.Wrap(TNSArray.OCClass.arrayWithObject(StringToID('public.image')));
      TMediaType.Video: LMediaTypes := TNSArray.Wrap(TNSArray.OCClass.arrayWithObject(StringToID('public.movie')));
      else raise Exception.Create('Unsupported media type, only Image and Video are supported');
    end;
    FImagePickerController.setMediaTypes(LMediaTypes);
    var LWindow := SharedApplication.keyWindow;
    if (LWindow <> nil) and (LWindow.rootViewController <> nil) then
      LWindow.rootViewController.presentViewController(FImagePickerController{viewControllerToPresent }, True{animated}, nil{completion})
    else
      Raise Exception.Create('Error C3CE07EB-C968-4BDA-B79C-75EA7CA9E084')
    {$ENDIF}
    {$ENDREGION}

  except
    on E: Exception do begin
      TALLoadingOverlay.CloseCurrent;
      if Assigned(FOnError) then
        FOnError(E.Message);
    end;
  end;
end;

{*****************************************}
procedure TALMediaPicker.CaptureFromCamera(
            const AMediaType: TMediaType;
            const AOnSuccess: TOnSuccessEvent;
            const AOnCancel: TOnCancelEvent;
            const AOnError: TOnErrorEvent;
            const AShowRequestPermissionRationaleEvent: TShowRequestPermissionRationaleEvent);
begin
  try

    FOnSuccess := AOnSuccess;
    FOnCancel := AOnCancel;
    FOnError := AOnError;
    TALLoadingOverlay.Builder.SetStealthMode.Show;
    FCaptureFromCameraMediaType := AMediaType;

    {$REGION 'ANDROID'}
    {$IF defined(ANDROID)}
    DoCaptureFromCamera;
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'IOS'}
    {$IF defined(IOS)}
    FApplicationEventHandlerEnabled := False;

    if not TUIImagePickerController.OCClass.isSourceTypeAvailable(UIImagePickerControllerSourceTypeCamera) then
      raise Exception.Create('Camera not available on this device');

    var LStatusVideo := TAVCaptureDevice.OCClass.authorizationStatusForMediaType(AVMediaTypeVideo);
    //--
    var LStatusAudio: AVAuthorizationStatus;
    if AMediaType = TMediaType.Video then LStatusAudio := TAVCaptureDevice.OCClass.authorizationStatusForMediaType(AVMediaTypeAudio)
    else LStatusAudio := AVAuthorizationStatusNotDetermined;
    //--
    var LStatus := LStatusVideo;
    if LStatus = AVAuthorizationStatusAuthorized then LStatus := LStatusAudio;
    //--
    case LStatus of
      AVAuthorizationStatusNotDetermined:; // Proceed to launch; dialog will show here
      AVAuthorizationStatusAuthorized:;
      AVAuthorizationStatusDenied,
      AVAuthorizationStatusRestricted: begin
        if assigned(AShowRequestPermissionRationaleEvent) then begin
          AShowRequestPermissionRationaleEvent(
            LStatusVideo in [AVAuthorizationStatusDenied, AVAuthorizationStatusRestricted], // const AToRequestCameraPermission: Boolean;
            LStatusAudio in [AVAuthorizationStatusDenied, AVAuthorizationStatusRestricted], // const AToRequestMicPermission: Boolean;
            procedure
            begin
              FApplicationEventHandlerEnabled := True;
              SharedApplication.openURL(TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(UIApplicationOpenSettingsURLString)), nil{options}, nil{completionHandler});
            end, //const ACanRequestPermissionProc: TProc; // the procedure to launch when the user response positivelly to the rationale
            procedure
            begin
              if LStatusVideo in [AVAuthorizationStatusNotDetermined, AVAuthorizationStatusAuthorized] then
                DoCaptureFromCamera
              else begin
                TALLoadingOverlay.CloseCurrent;
                if Assigned(FOnError) then
                  FOnError('No camera permission granted');
              end;
            end); // const ACanNotRequestPermissionProc: TProc) of object; // the procedure to launch when the user response negativelly to the rationale
          Exit;
        end;
      end;
    end;

    DoCaptureFromCamera;
    {$ENDIF}
    {$ENDREGION}

  except
    on E: Exception do begin
      TALLoadingOverlay.CloseCurrent;
      if Assigned(FOnError) then
        FOnError(E.Message);
    end;
  end;
end;

{****************}
{$IF defined(IOS)}
procedure TALMediaPicker.ApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin
  if not FApplicationEventHandlerEnabled then exit;
  if (M is TApplicationEventMessage) then begin

    var LMsg := TApplicationEventMessage(M);
    if (LMsg.Value.Event <> TApplicationEvent.BecameActive) then exit;

    {$IFDEF DEBUG}
    allog(
      'TALMediaPicker.ApplicationEventHandler',
      'Event: '+TRttiEnumerationType.GetName(LMsg.Value.Event));
    {$ENDIF}

    FApplicationEventHandlerEnabled := False;

    var LStatusVideo := TAVCaptureDevice.OCClass.authorizationStatusForMediaType(AVMediaTypeVideo);
    if LStatusVideo in [AVAuthorizationStatusNotDetermined, AVAuthorizationStatusAuthorized] then DoCaptureFromCamera
    else begin
      TALLoadingOverlay.CloseCurrent;
      if Assigned(FOnError) then
        FOnError('No camera permission granted');
    end;

  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
procedure TALMediaPicker.HandleActivityResult(const Sender: TObject; const AMsg: TMessage);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetMediaTypeFromUri(const AUri: Jnet_Uri): TMediaType;
  begin
    if AUri = nil then Exit(TMediaType.Unknown);
    var LJType := TAndroidHelper.Context.getContentResolver.getType(AUri);
    if LJType <> nil then begin
      var LType := JStringToString(LJType);
      if LType.StartsWith('image/', True{IgnoreCase}) then Result := TMediaType.Image
      else if LType.StartsWith('video/', True{IgnoreCase}) then Result := TMediaType.Video
      else Result := TMediaType.Unknown;
    end
    else Result := TMediaType.Unknown;
  end;

begin
  if not (AMsg is TMessageResultNotification) then Exit;
  try
    var LMessageResult := TMessageResultNotification(AMsg);
    case LMessageResult.RequestCode of

      // --REQUEST_CODE_PICK_MEDIA--
      REQUEST_CODE_PICK_MEDIA: begin
        TALLoadingOverlay.CloseCurrent;
        if (LMessageResult.ResultCode = TJActivity.JavaClass.RESULT_OK) then begin
          var LItems: TArray<TMediaItem>;
          if LMessageResult.Value = nil then SetLength(LItems, 0)
          else begin
            var LClipData := LMessageResult.Value.getClipData;
            if LClipData <> nil then begin
              var LItemCount := LClipData.getItemCount;
              SetLength(LItems, LItemCount);
              for var I := 0 to LItemCount - 1 do begin
                var LUri := LClipData.getItemAt(I).getUri;
                LItems[I] := TMediaItem.Create(
                               JStringToString(LUri.toString),
                               GetMediaTypeFromUri(LUri));
              end;
            end
            else begin
              var LData := LMessageResult.Value.getData;
              if LData <> nil then begin
                SetLength(LItems, 1);
                LItems[0] := TMediaItem.Create(
                               JStringToString(LData.toString),
                               GetMediaTypeFromUri(LData));
              end
              else
                SetLength(LItems, 0);
            end;
          end;
          try
            If Assigned(FOnSuccess) then
              FOnSuccess(LItems);
          finally
            For var I := low(LItems) to high(LItems) do
              ALFreeAndNil(LItems[I]);
          end;
        end
        else if (LMessageResult.ResultCode = TJActivity.JavaClass.RESULT_CANCELED) then begin
          If Assigned(FOnCancel) then
            FOnCancel();
        end
        else begin
          If Assigned(FOnError) then
            FOnError(Format('Unexpected result code (%d)', [LMessageResult.ResultCode]));
        end;
      end;

      // --REQUEST_CODE_CAPTURE_IMAGE--
      // --REQUEST_CODE_CAPTURE_VIDEO--
      REQUEST_CODE_CAPTURE_IMAGE,
      REQUEST_CODE_CAPTURE_VIDEO: begin
        TALLoadingOverlay.CloseCurrent;
        if (LMessageResult.ResultCode = TJActivity.JavaClass.RESULT_OK) then begin
          var LItems: TArray<TMediaItem>;
          SetLength(LItems, 1);
          LItems[0] := TMediaItem.Create(
                         JStringToString(FCaptureUri.toString),
                         GetMediaTypeFromUri(FCaptureUri));
          try
            If Assigned(FOnSuccess) then
              FOnSuccess(LItems);
          finally
            For var I := low(LItems) to high(LItems) do
              ALFreeAndNil(LItems[I]);
          end;
        end
        else if (LMessageResult.ResultCode = TJActivity.JavaClass.RESULT_CANCELED) then begin
          If Assigned(FOnCancel) then
            FOnCancel();
        end
        else begin
          If Assigned(FOnError) then
            FOnError(Format('Unexpected result code (%d)', [LMessageResult.ResultCode]));
        end;
        FCaptureUri := nil;
      end;

    end;
  Except
    on E: Exception do begin
      TALLoadingOverlay.CloseCurrent;
      if Assigned(FOnError) then
        FOnError(E.Message);
    end;
  end;
end;
{$ENDIF}

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.MediaPicker','initialization');
  {$ENDIF}
  TALMediaPicker.FInstance := nil;
  TALMediaPicker.CreateInstanceFunc := @TALMediaPicker.CreateInstance;

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.MediaPicker','finalization');
  {$ENDIF}
  ALFreeAndNil(TALMediaPicker.FInstance);

end.