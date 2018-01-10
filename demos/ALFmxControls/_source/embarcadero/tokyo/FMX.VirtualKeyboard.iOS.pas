{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.VirtualKeyboard.iOS;

interface

{$SCOPEDENUMS ON}

procedure RegisterVirtualKeyboardServices;
procedure UnregisterVirtualKeyboardServices;

implementation

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Generics.Collections, System.UITypes, System.Types, 
  System.Messaging, System.Math, Macapi.ObjectiveC, Macapi.ObjCRuntime, Macapi.Helpers,
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics,
  FMX.Types, FMX.VirtualKeyboard, FMX.Platform, FMX.Forms, FMX.Platform.iOS, FMX.Consts, FMX.Helpers.iOS;

const
  ToolbarHeight = 44;

type
  IKeyboardEvents = interface(NSObject)
  ['{72D3A7FD-DDE3-473D-9750-46C072E7B3B7}']
    { Keyboard notifications }
    procedure KeyboardWillShow(notification: Pointer); cdecl;
    procedure KeyboardWillHide(notification: Pointer); cdecl;
    procedure KeyboardDidHide(notification: Pointer); cdecl;
    procedure KeyboardWillChangeFrame(notification: Pointer); cdecl;
    procedure KeyboardDidChangeFrame(notification: Pointer); cdecl;
    { Actions }
    procedure HideVirtualKeyboard; cdecl;
    procedure CustomButtonAction(sender: Pointer); cdecl;
  end;

  TKeyboardEventHandler = class(TOCLocal)
  strict private type
    TKeyboardState = (Shown, Hidden);
  private
    FKeepFocus: Boolean;
    { Keyborad Notifications }
    procedure SendNotificationAboutKeyboardEvent(const AVKRect: TRect; AKeyboardState :TKeyboardState);
    function GetKeyboardRect(Notification: Pointer): TRect;
    function GetKeyboardFrame(Notification: Pointer): NSRect;
    function InvertFrame(const AFrame: NSRect): NSRect;
  protected
    { TOCLocal }
    function GetObjectiveCClass: PTypeInfo; override;
  public
    { IKeyboardEvents }
    procedure KeyboardWillShow(notification: Pointer); cdecl;
    procedure KeyboardWillHide(notification: Pointer); cdecl;
    procedure KeyboardDidHide(notification: Pointer); cdecl;
    procedure KeyboardWillChangeFrame(notification: Pointer); cdecl;
    procedure KeyboardDidChangeFrame(notification: Pointer); cdecl;
    procedure HideVirtualKeyboard; cdecl;
    procedure CustomButtonAction(sender: Pointer); cdecl;
  end;

  TCocoaVirtualKeyboardService = class(TInterfacedObject, IFMXVirtualKeyboardService, IFMXVirtualKeyboardToolbarService)
  private const
    ToolbarAnimationDuration = 0.25;
  private
    FKeyboardHandler: TKeyboardEventHandler;
    { IFMXVirtualKeyboardToolbarService }
    FToolbarVisible: Boolean;
    FToolBar: UIToolBar;
    FToolBarButtons: NSMutableArray;
    FFlexibleSepararator: UIBarButtonItem;
    FHideButton: UIBarButtonItem;
    FButtons: TList<TVirtualKeyboardToolButton>;
    FUpdatingButtons: Boolean;
    FToolbarEnabled: Boolean;
    FHideButtonVisible: Boolean;
    FStoredActiveForm: TComponent;
    FKeyboardFrame: NSRect;
    FAnimationDuration: NSTimeInterval;
    procedure SetToolbarVisible(const Value: Boolean);
    procedure SetToolbarFrame(const Frame: NSRect; const UseAnimation: Boolean);
    function GetToolbarFrame: NSRect;
    procedure RefreshToolbarButtons;
    procedure RefreshToolbarPosition;
    procedure CreateToolbar;
    procedure ChangeToolbarOrientation;
    procedure SetKeyboardFrame(const Rect: NSRect);
    procedure SetKeyboardAnimationDuration(const AInterval: NSTimeInterval);
    property ToolbarVisible: Boolean read FToolbarVisible write SetToolbarVisible;
  private
    FSubscriptionApplicationEventID: Integer;
    { Messaging }
    procedure ApplicationEventHandler(const Sender: TObject; const M: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXVirtualKeyboardService }
    function ShowVirtualKeyboard(const AControl: TFmxObject): Boolean;
    function HideVirtualKeyboard: Boolean;
    procedure SetTransientState(Value: Boolean);
    function GetVirtualKeyboardState: TVirtualKeyboardStates;
    property VirtualKeyboardState: TVirtualKeyboardStates read GetVirtualKeyboardState;
    { IFMXVirtualKeyboardToolbarService }
    procedure SetToolbarEnabled(const Value: Boolean);
    function IsToolbarEnabled: Boolean;
    procedure SetHideKeyboardButtonVisibility(const Value: Boolean);
    function IsHideKeyboardButtonVisible: Boolean;
    function AddButton(const Title: string; ExecuteEvent: TNotifyEvent): TVirtualKeyboardToolButton;
    procedure DeleteButton(const Index: Integer);
    function ButtonsCount: Integer;
    function GetButtonByIndex(const Index: Integer): TVirtualKeyboardToolButton;
    procedure ClearButtons;
  end;

  TVKToolbarButton_iOS = class(TVirtualKeyboardToolButton)
  protected
    procedure DoChanged; override;
  end;

var
  CocoaKeyboardService: TCocoaVirtualKeyboardService;


procedure RegisterVirtualKeyboardServices;
begin
  CocoaKeyboardService := TCocoaVirtualKeyboardService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXVirtualKeyboardService, CocoaKeyboardService);
  TPlatformServices.Current.AddPlatformService(IFMXVirtualKeyboardToolbarService, CocoaKeyboardService);
end;

procedure UnregisterVirtualKeyboardServices;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXVirtualKeyboardService);
  TPlatformServices.Current.RemovePlatformService(IFMXVirtualKeyboardToolbarService);
  FreeAndNil(CocoaKeyboardService);
end;

{ TVirtualKeyboardEventHandler }

procedure TKeyboardEventHandler.CustomButtonAction(sender: Pointer);
var
  Index: Integer;
begin
  Index := TUIBarButtonItem.Wrap(sender).tag - 1;
  if Index >= 0 then
    TVKToolbarButton_iOS(CocoaKeyboardService.FButtons[Index]).DoExecute;
end;

function TKeyboardEventHandler.GetKeyboardFrame(Notification: Pointer): NSRect;
var
  ScreenService: IFMXScreenService;
  Orientation: TScreenOrientation;
begin
  Result := iOSapi.UIKit.TNSValue.Wrap(TNSNotification.Wrap(Notification).userInfo.valueForKey(StrToNSStr('UIKeyboardFrameEndUserInfoKey'))).CGRectValue;

  if not TOSVersion.Check(8) then
  begin
    Orientation := TScreenOrientation.Portrait;
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
      Orientation := ScreenService.GetScreenOrientation;

    case Orientation of
      TScreenOrientation.InvertedPortrait:
        Result.origin.y := Screen.Size.Height - Result.origin.y - Result.size.height;
      TScreenOrientation.InvertedLandscape:
      begin
        Result := InvertFrame(Result);
        Result.origin.y := Screen.Size.Height - Result.origin.y - Result.size.height;
      end;
      TScreenOrientation.Landscape:
        Result := InvertFrame(Result);
    end;
  end;
end;

function TKeyboardEventHandler.GetKeyboardRect(Notification: Pointer): TRect;
var
  OCRect: NSRect;
begin
  OCRect := GetKeyboardFrame(Notification);
  Result := TRect.Create(Point(Round(OCRect.origin.x), Round(OCRect.origin.y)), Round(OCRect.size.width), Round(OCRect.size.height));
end;

function TKeyboardEventHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IKeyboardEvents);
end;

procedure TKeyboardEventHandler.HideVirtualKeyboard;
begin
  try
    Screen.ActiveForm.Focused := nil;
  except
    Application.HandleException(Screen.ActiveForm);
  end;
end;

function TKeyboardEventHandler.InvertFrame(const AFrame: NSRect): NSRect;
begin
  Result.origin.x := AFrame.origin.y;
  Result.origin.y := AFrame.origin.x;
  Result.size.width := AFrame.size.height;
  Result.size.height := AFrame.size.width;
end;

procedure TKeyboardEventHandler.KeyboardDidChangeFrame(notification: Pointer);
begin
  CocoaKeyboardService.ChangeToolbarOrientation;
end;

procedure TKeyboardEventHandler.KeyboardDidHide(notification: Pointer);
begin
  CocoaKeyboardService.ToolbarVisible := False;
end;

procedure TKeyboardEventHandler.KeyboardWillChangeFrame(notification: Pointer);
var
  KeyboardFrame: NSRect;
begin
  KeyboardFrame := GetKeyboardFrame(Notification);
  CocoaKeyboardService.SetKeyboardFrame(KeyboardFrame);
  if CocoaKeyboardService.IsToolbarEnabled then
    CocoaKeyboardService.RefreshToolbarPosition
end;

procedure TKeyboardEventHandler.KeyboardWillHide(notification: Pointer);
var
  VKRect: TRect;
  Duration: Double;
begin
  if (TVirtualKeyboardState.Visible in CocoaKeyboardService.VirtualKeyboardState) or CocoaKeyboardService.ToolbarVisible then
  begin
    CocoaKeyboardService.ToolbarVisible := False;
    Duration := TNSNumber.Wrap(TNSNotification.Wrap(Notification).userInfo.valueForKey(StrToNSStr('UIKeyboardAnimationDurationUserInfoKey'))).doubleValue;
    CocoaKeyboardService.SetKeyboardAnimationDuration(Duration);
    VKRect := GetKeyboardRect(notification);
    if CocoaKeyboardService.IsToolbarEnabled then
      VKRect.Bottom := VKRect.Bottom + ToolbarHeight;
    if not FKeepFocus then
      HideVirtualKeyboard;
    SendNotificationAboutKeyboardEvent(VKRect, TKeyboardState.Hidden);
  end;
end;

procedure TKeyboardEventHandler.KeyboardWillShow(notification: Pointer);
var
  VKRect: TRect;
  Duration: NSTimeInterval;
begin
  VKRect := GetKeyboardRect(notification);
  Duration := TNSNumber.Wrap(TNSNotification.Wrap(Notification).userInfo.valueForKey(StrToNSStr('UIKeyboardAnimationDurationUserInfoKey'))).doubleValue;
  CocoaKeyboardService.SetKeyboardAnimationDuration(Duration);
  CocoaKeyboardService.SetKeyboardFrame(GetKeyboardFrame(Notification));
  CocoaKeyboardService.CreateToolbar;
  CocoaKeyboardService.ToolbarVisible := True;
  if CocoaKeyboardService.IsToolbarEnabled then
    VKRect.Top := VKRect.Top - ToolbarHeight;
  SendNotificationAboutKeyboardEvent(VKRect, TKeyboardState.Shown);
end;

procedure TKeyboardEventHandler.SendNotificationAboutKeyboardEvent(const AVKRect: TRect;
  AKeyboardState: TKeyboardState);
var
  Message: TVKStateChangeMessage;
begin
  Message := TVKStateChangeMessage.Create(AKeyboardState = TKeyboardState.Shown, AVKRect);
  TMessageManager.DefaultManager.SendMessage(Self, Message, True);
end;

type
  TStoredActiveForm = class (TComponent)
  private
    [weak]FForm: TCommonCustomForm;
    procedure SetForm(const Value: TCommonCustomForm);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Form: TCommonCustomForm read FForm write SetForm;
  end;

{ TStoredActiveForm }

procedure TStoredActiveForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FForm then
      FForm := nil;
  end;
end;

procedure TStoredActiveForm.SetForm(const Value: TCommonCustomForm);
begin
  if FForm <> Value then
  begin
    if FForm <> nil then
    begin
      TComponent(FForm).RemoveFreeNotification(self);
      FForm := nil;
    end;
    FForm := Value;
    if FForm <> nil then
      TComponent(FForm).FreeNotification(Self);
  end;
end;

{ TCocoaVirtualKeyboardToolbarService }

function TCocoaVirtualKeyboardService.AddButton(const Title: string; ExecuteEvent: TNotifyEvent): TVirtualKeyboardToolButton;
begin
  FUpdatingButtons := True;
  Result := TVKToolbarButton_iOS.Create;
  Result.Title := Title;
  Result.OnExecute := ExecuteEvent;
  FUpdatingButtons := False;
  FButtons.Add(Result);
  RefreshToolbarButtons;
end;

function TCocoaVirtualKeyboardService.ButtonsCount: Integer;
begin
  Result := FButtons.Count;
end;

procedure TCocoaVirtualKeyboardService.ChangeToolbarOrientation;
begin
  //Need to change orientation without animation
  if FToolBar <> nil then
    SetToolbarFrame(GetToolbarFrame, False);
end;

procedure TCocoaVirtualKeyboardService.ClearButtons;
begin
  if FButtons.Count > 0 then
  begin
    FUpdatingButtons := True;
    FButtons.Clear;
    FUpdatingButtons := False;
    RefreshToolbarButtons;
  end;
end;

constructor TCocoaVirtualKeyboardService.Create;
begin
  inherited Create;
  FStoredActiveForm := TStoredActiveForm.Create(nil);
  FKeyboardHandler := TKeyboardEventHandler.Create;
  //Subscribing to events
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(
    FKeyboardHandler.GetObjectID, sel_getUid('KeyboardWillShow:'), StringToID('UIKeyboardWillShowNotification'), nil);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(
    FKeyboardHandler.GetObjectID, sel_getUid('KeyboardWillHide:'), StringToID('UIKeyboardWillHideNotification'), nil);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(
    FKeyboardHandler.GetObjectID, sel_getUid('KeyboardDidHide:'), StringToID('UIKeyboardDidHideNotification'), nil);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(
    FKeyboardHandler.GetObjectID, sel_getUid('KeyboardWillChangeFrame:'),
    StringToID('UIKeyboardWillChangeFrameNotification'), nil);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(
    FKeyboardHandler.GetObjectID, sel_getUid('KeyboardDidChangeFrame:'), StringToID('UIKeyboardDidChangeFrameNotification'),
    nil);

  // In this event we detect type of interface idiom (iPad or iPhone), because while application is not initialized,
  // we cannot get correct value for it.
  FSubscriptionApplicationEventID := TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage,
    ApplicationEventHandler);

  //IFMXVirtualKeyboardToolbarService
  FUpdatingButtons := False;
  FToolbarVisible := False;
  FButtons := TList<TVirtualKeyboardToolButton>.Create;
  FToolBarButtons := TNSMutableArray.Create;
  FAnimationDuration := ToolbarAnimationDuration;
  FToolbarEnabled := True;
  FHideButtonVisible := True;
end;

procedure TCocoaVirtualKeyboardService.CreateToolbar;
var
  KeyWindow: UIWindow;
begin
  KeyWindow := SharedApplication.keyWindow;
  if (KeyWindow <> nil) and (KeyWindow.rootViewController <> nil) then
  begin
    if (FToolBar = nil) and FToolbarEnabled then
    begin
      FToolBar := TUIToolbar.Create;
      if TOSVersion.Check(7) then
        FToolBar.setBarStyle(UIBarStyleDefault)
      else
        FToolBar.setBarStyle(UIBarStyleBlackOpaque);
      FToolBar.setAlpha(0.8);
      SetToolbarFrame(GetToolbarFrame, False);
      RefreshToolbarButtons;
      KeyWindow.rootViewController.view.addSubview(FToolbar);
    end
    else
      KeyWindow.rootViewController.view.bringSubviewToFront(FToolbar);
  end;
end;

procedure TCocoaVirtualKeyboardService.DeleteButton(const Index: Integer);
begin
  if (Index >= 0) and (Index < FButtons.Count) then
  begin
    FButtons.Delete(Index);
    RefreshToolbarButtons;
  end;
end;

destructor TCocoaVirtualKeyboardService.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, FSubscriptionApplicationEventID);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).removeObserver(FKeyboardHandler.GetObjectID);
  FreeAndNil(FKeyboardHandler);
  //IFMXVirtualKeyboardToolbarService
  FUpdatingButtons := True;
  FreeAndNil(FButtons);
  if FToolBar <> nil then
  begin
    if FToolBar.items <> nil then
      FToolBar.items.release;
    FToolBar.release;
    FToolBar := nil;
  end;
  if FFlexibleSepararator <> nil then
  begin
    FFlexibleSepararator.release;
    FFlexibleSepararator := nil;
  end;
  if FHideButton <> nil then
  begin
    FHideButton.release;
    FHideButton := nil;
  end;
  FreeAndNil(FStoredActiveForm);
  FToolBarButtons.release;
  inherited;
end;

procedure TCocoaVirtualKeyboardService.ApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin
  if (M is TApplicationEventMessage) and ((M as TApplicationEventMessage).Value.Event = TApplicationEvent.FinishedLaunching) then
  begin
    FToolbarEnabled := not IsPad;
    FHideButtonVisible := FToolbarEnabled;
  end;
end;

function TCocoaVirtualKeyboardService.GetButtonByIndex(const Index: Integer): TVirtualKeyboardToolButton;
begin
  if (Index >= 0) and (Index < FButtons.Count) then
    Result := FButtons[Index]
  else
    Result := nil;
end;

function TCocoaVirtualKeyboardService.GetToolbarFrame: NSRect;
var
  ScreenRect: NSRect;
  InterfaceOrientation: TScreenOrientation;
  ScreenService: IFMXScreenService;
begin
  ScreenRect := MainScreen.bounds;
  Result.origin.x := 0;
  Result.size.height := ToolbarHeight;

  if TOSVersion.Check(8) then
    InterfaceOrientation := TScreenOrientation.Portrait
  else if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
    InterfaceOrientation := ScreenService.GetScreenOrientation
  else
    InterfaceOrientation := TScreenOrientation.Portrait;

  case InterfaceOrientation of
    TScreenOrientation.Portrait,
    TScreenOrientation.InvertedPortrait:
      begin
        if (FToolBar <> nil) and ToolbarVisible and IsToolbarEnabled then
          Result.origin.y := ScreenRect.size.height - FKeyboardFrame.size.height - FToolBar.bounds.size.height
        else
          Result.origin.y := ScreenRect.size.height;
        Result.size.width := ScreenRect.size.width;
      end;
    TScreenOrientation.Landscape,
    TScreenOrientation.InvertedLandscape:
      begin
        if (FToolBar <> nil) and ToolbarVisible and IsToolbarEnabled then
          Result.origin.y := ScreenRect.size.width - FKeyboardFrame.size.height - FToolBar.bounds.size.height
        else
          Result.origin.y := ScreenRect.size.width;
        Result.size.width := ScreenRect.size.height;
      end;
  end;
end;

function TCocoaVirtualKeyboardService.GetVirtualKeyboardState: TVirtualKeyboardStates;
begin
  Result := [TVirtualKeyboardState.AutoShow];
end;

procedure TCocoaVirtualKeyboardService.SetTransientState(Value: Boolean);
begin
end;

function TCocoaVirtualKeyboardService.IsHideKeyboardButtonVisible: Boolean;
begin
  Result := FHideButtonVisible;
end;

function TCocoaVirtualKeyboardService.IsToolbarEnabled: Boolean;
begin
  Result := FToolbarEnabled;
end;

procedure TCocoaVirtualKeyboardService.RefreshToolbarButtons;
var
  I: Integer;
  B: UIBarButtonItem;
  AutoReleasePool: NSAutoReleasePool;
begin
  if not FUpdatingButtons and (FToolBar <> nil) then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      if FToolBar.items <> nil then
      begin
        FToolBar.setItems(nil);
        FFlexibleSepararator := nil;
        FHideButton := nil;
      end;

      FToolBarButtons.removeAllObjects;
    finally
      AutoReleasePool.release;
    end;

    //Custom buttons
    for I := 0 to FButtons.Count - 1 do
    begin
      B := TUIBarButtonItem.Create;
      B.setTitle(StrToNSStr(FButtons[I].Title));
      B.setStyle(UIBarButtonItemStyleBordered);
      B.setTag(I + 1);
      B.setTarget(FKeyboardHandler.GetObjectID);
      B.setAction(sel_getUid('CustomButtonAction:'));
      FToolBarButtons.addObject((B as ILocalObject).GetObjectID);
    end;

    if FHideButtonVisible then
    begin
      //Separator
      if FFlexibleSepararator = nil then
      begin
        FFlexibleSepararator := TUIBarButtonItem.Create;
        FFlexibleSepararator.initWithBarButtonSystemItem(UIBarButtonSystemItemFlexibleSpace, nil, nil);
      end;
      FToolBarButtons.addObject((FFlexibleSepararator as ILocalObject).GetObjectID);

      //Hide button
      if FHideButton = nil then
      begin
        FHideButton := TUIBarButtonItem.Create;
        FHideButton.setTitle(StrToNSStr(SEditorDone));
        FHideButton.setStyle(UIBarButtonItemStyleDone);
        FHideButton.setTarget(FKeyboardHandler.GetObjectID);
        FHideButton.setAction(sel_getUid('HideVirtualKeyboard'));
      end;
      FToolBarButtons.addObject((FHideButton as ILocalObject).GetObjectID);
    end;

    FToolBar.setItems(FToolBarButtons);
  end;
end;

procedure TCocoaVirtualKeyboardService.RefreshToolbarPosition;
begin
  SetToolbarFrame(GetToolbarFrame, False);
end;

procedure TCocoaVirtualKeyboardService.SetHideKeyboardButtonVisibility(const Value: Boolean);
begin
  if FHideButtonVisible <> Value then
  begin
    FHideButtonVisible := Value;
    RefreshToolbarButtons;
  end;
end;

procedure TCocoaVirtualKeyboardService.SetKeyboardAnimationDuration(const AInterval: NSTimeInterval);
begin
  FAnimationDuration := AInterval;
end;

procedure TCocoaVirtualKeyboardService.SetKeyboardFrame(const Rect: NSRect);
begin
  FKeyboardFrame := Rect;
end;

procedure TCocoaVirtualKeyboardService.SetToolbarEnabled(const Value: Boolean);
begin
  if FToolbarEnabled <> Value then
  begin
    if not Value then
      ToolbarVisible := False;
    FToolbarEnabled := Value;
  end;
end;

procedure TCocoaVirtualKeyboardService.SetToolbarFrame(const Frame: NSRect; const UseAnimation: Boolean);
begin
  if FToolBar <> nil then
    if UseAnimation then
    begin
      TUIView.OCClass.beginAnimations(nil, nil);
      try
        TUIView.OCClass.setAnimationDuration(FAnimationDuration);
        TUIView.OCClass.setAnimationBeginsFromCurrentState(True);
        FToolBar.setFrame(Frame);
      finally
        TUIView.OCClass.commitAnimations;
      end;
    end
    else
      FToolBar.setFrame(Frame);
end;

procedure TCocoaVirtualKeyboardService.SetToolbarVisible(const Value: Boolean);
begin
  if FToolbarVisible <> Value then
  begin
    FToolbarVisible := Value;
    if FToolBar <> nil then
    begin
      if FToolbarEnabled then
        SetToolbarFrame(GetToolbarFrame, True);
      FToolBar.setHidden(not (FToolbarEnabled and Value));
    end;
  end;
end;

function TCocoaVirtualKeyboardService.ShowVirtualKeyboard(const AControl: TFmxObject): Boolean;
var
  RootObj: TFmxObject;
  RootView: UIView;
begin
  RootObj := AControl.Root.GetObject;
  if RootObj is TCommonCustomForm then
  begin
    if FStoredActiveForm <> nil then
      TStoredActiveForm(FStoredActiveForm).Form := TCommonCustomForm(RootObj);
    RootView := WindowHandleToPlatform(TCommonCustomForm(RootObj).Handle).View;
    Result := RootView.becomeFirstResponder;
  end
  else
    Result := False;
end;

function TCocoaVirtualKeyboardService.HideVirtualKeyboard: Boolean;
var
  RootObj: TCommonCustomForm;
begin
  RootObj := nil;
  if FStoredActiveForm <> nil then
    RootObj := TStoredActiveForm(FStoredActiveForm).Form;
  if RootObj <> nil then
  begin
    FKeyboardHandler.FKeepFocus := True;
    Result := WindowHandleToPlatform(RootObj.Handle).View.resignFirstResponder;
    FKeyboardHandler.FKeepFocus := False;
  end
  else
    Result := False;
end;

{ TVKToolbarButton }

procedure TVKToolbarButton_iOS.DoChanged;
begin
  inherited;
  CocoaKeyboardService.RefreshToolbarButtons;
end;


initialization
  RegisterVirtualKeyboardServices;
finalization
  UnregisterVirtualKeyboardServices;
end.
