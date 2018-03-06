{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2014-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Presentation.iOS.Style;

interface

{$SCOPEDENUMS ON}

uses
  System.TypInfo, System.Types, System.Classes, System.SysUtils, System.Rtti, System.UITypes, Macapi.ObjectiveC,
  iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.GLKit, FMX.Types,
  FMX.Controls.Presentation, FMX.Controls, FMX.Presentation.Messages, FMX.Controls.Model, FMX.Graphics,
  FMX.Presentation.iOS, FMX.Platform.iOS, FMX.Forms;

type

  TiOSStyledPresentation = class;
  TNativeScene = class;

  /// <summary>Helper class used as root for control's style</summary>
  TNativeStyledControl = class(TStyledControl)
  private
    [Weak] FScene: TNativeScene;
  protected
    function GetDefaultStyleLookupName: string; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure AdjustSize; override;
  end;

  /// <summary>Non TControl class that used as container for style to break control parenting</summary>
  TNativeScene = class(TFmxObject, IControl, IScene, IPaintControl)
  private
    [Weak] FPresentation: TiOSStyledPresentation;
    FStyledControl: TNativeStyledControl;
    FCanvas: TCanvas;
    FUpdateRects: array of TRectF;
    FContextHandle: THandle;
    FDisableUpdating: Integer;
    function GetView: UIView;
    { IControl }
    function GetObject: TFmxObject;
    procedure SetFocus;
    function GetIsFocused: boolean;
    function GetCanFocus: Boolean;
    function GetCanParentFocus: Boolean;
    function GetEnabled: Boolean;
    function GetAbsoluteEnabled: Boolean;
    function GetPopupMenu: TCustomPopupMenu;
    function EnterChildren(AObject: IControl): Boolean;
    function ExitChildren(AObject: IControl): Boolean;
    procedure DoEnter;
    procedure DoExit;
    procedure DoActivate;
    procedure DoDeactivate;
    procedure DoMouseEnter;
    procedure DoMouseLeave;
    function ShowContextMenu(const ScreenPosition: TPointF): Boolean;
    function ScreenToLocal(P: TPointF): TPointF;
    function LocalToScreen(P: TPointF): TPointF;
    function ObjectAtPoint(P: TPointF): IControl;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseMove(Shift: TShiftState; X, Y: Single);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure KeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure Tap(const Point: TPointF);
    procedure DialogKey(var Key: Word; Shift: TShiftState);
    procedure AfterDialogKey(var Key: Word; Shift: TShiftState);
    function FindTarget(P: TPointF; const Data: TDragObject): IControl;
    procedure DragEnter(const Data: TDragObject; const Point: TPointF);
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure DragDrop(const Data: TDragObject; const Point: TPointF);
    procedure DragLeave;
    procedure DragEnd;
    function CheckForAllowFocus: Boolean;
    procedure Repaint;
    function GetDragMode: TDragMode;
    procedure SetDragMode(const ADragMode: TDragMode);
    procedure BeginAutoDrag;
    function GetParent: TFmxObject;
    function GetLocked: Boolean;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    function GetHitTest: Boolean;
    function GetCursor: TCursor;
    function GetInheritedCursor: TCursor;
    function GetDesignInteractive: Boolean;
    function GetAcceptsControls: Boolean;
    procedure SetAcceptsControls(const Value: Boolean);
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetTabStopController: ITabStopController;
    function GetTabStop: Boolean;
    procedure SetTabStop(const TabStop: Boolean);
    function HasHint: Boolean; virtual;
    function GetHintString: string; virtual;
    function GetHintObject: TObject; virtual;
    { IScene }
    function GetStyleBook: TStyleBook;
    procedure SetStyleBook(const Value: TStyleBook);
    procedure AddUpdateRect(R: TRectF);
    function GetUpdateRectsCount: Integer;
    function GetUpdateRect(const Index: Integer): TRectF;
    function GetCanvas: TCanvas;
    function GetSceneScale: Single;
    procedure ChangeScrollingState(const AControl: TControl; const Active: Boolean);
    procedure DisableUpdating;
    procedure EnableUpdating;
    { IPaintControl }
    procedure MergeUpdateRects(const UpdateRects: array of TRectF);
    procedure PaintRects(const UpdateRects: array of TRectF);
    function GetContextHandle: THandle;
    procedure SetContextHandle(const AContextHandle: THandle);
    procedure SetSize(const ASize: TSizeF);
  public
    constructor Create(APresentation: TiOSStyledPresentation); reintroduce;
    destructor Destroy; override;
    /// <summary>Link to platform UIView used as container for scene</summary>
    property View: UIView read GetView;
    /// <summary>Link to root styled control of the scene</summary>
    property StyledControl: TNativeStyledControl read FStyledControl;
  end;

{ TiOSStyledPresentation }

  /// <summary>Objective-C bridge helper for native-styled presentation</summary>
  IiOSSceneControl = interface(GLKView)
  ['{3A907753-FF20-4EB7-A791-E30C62016759}']
    procedure drawRect(R: CGRect); cdecl;
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
  end;

  /// <summary>Basic iOS native-styled presentation, which is UIView.</summary>
  TiOSStyledPresentation = class(TiOSNativeView)
  private
    FNativeScene: TNativeScene;
    function GetView: GLKView;
    function GetStyledControl: TNativeStyledControl;
    procedure PMApplyStyleLookup(var AMessage: TDispatchMessage); message PM_APPLY_STYLE_LOOKUP;
    procedure PMNeedStyleLookup(var AMessage: TDispatchMessage); message PM_NEED_STYLE_LOOKUP;
    procedure PMSetStyleLookup(var AMessage: TDispatchMessageWithValue<string>); message PM_SET_STYLE_LOOKUP;
    procedure PMFindStyleResource(var AMessage: TDispatchMessageWithValue<TFindStyleResourceInfo>); message PM_FIND_STYLE_RESOURCE;
    procedure PMRealign(var AMessage: TDispatchMessage); message PM_REALIGN;
    procedure PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_RECOMMEND_SIZE;
    procedure PMObjectAtPoint(var AMessage: TDispatchMessageWithValue<TObjectAtPointInfo>); message PM_OBJECT_AT_POINT;
    procedure PMStartTriggerAnimation(var AMessage: TDispatchMessageWithValue<TTriggerInfo>); message PM_START_TRIGGER_ANIMATION;
    procedure PMApplyTriggerEffect(var AMessage: TDispatchMessageWithValue<TTriggerInfo>); message PM_APPLY_TRIGGER_EFFECT;
    procedure PMGetResourceLink(var AMessage: TDispatchMessageWithValue<TFmxObject>); message PM_GET_RESOURCE_LINK;
    procedure PMGetAdjustSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_ADJUST_SIZE;
    procedure PMSetAdjustSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_SET_ADJUST_SIZE;
    procedure PMGetAdjustType(var AMessage: TDispatchMessageWithValue<TAdjustType>); message PM_GET_ADJUST_TYPE;
    procedure PMSetAdjustType(var AMessage: TDispatchMessageWithValue<TAdjustType>); message PM_SET_ADJUST_TYPE;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
    procedure InitView; override;
    procedure SetSize(const ASize: TSizeF); override;
    /// <summary>Bridge from presentation's GetDefaultStyleLookupName to StyledControl.GetDefaultStyleLookupName</summary>
    function GetDefaultStyleLookupName: string; virtual;
    /// <summary>Bridge from presentation's GetParentClassStyleLookupName to StyledControl.GetParentClassStyleLookupName</summary>
    function GetParentClassStyleLookupName: string; virtual;
    /// <summary>Bridge from presentation's ApplyStyle to StyledControl.ApplyStyle</summary>
    procedure ApplyStyle; virtual;
    /// <summary>Bridge from presentation's FreeStyle to StyledControl.FreeStyle</summary>
    procedure FreeStyle; virtual;
    /// <summary>Bridge from presentation's DoApplyStyleLookup to StyledControl.DoApplyStyleLookup</summary>
    procedure DoApplyStyleLookup; virtual;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    /// <summary>Overrided Objective-C method</summary>
    procedure drawRect(R: CGRect); cdecl;
    /// <summary>Link to platform UIView used as container for scene</summary>
    property View: GLKView read GetView;
    /// <summary>Link to root styled control of the scene</summary>
    property StyledControl: TNativeStyledControl read GetStyledControl;
  end;

implementation

uses
  System.Math, System.Generics.Collections, FMX.Ani, FMX.Consts, FMX.Presentation.Factory, FMX.Helpers.iOS,
  FMX.Context.GLES.iOS;

type

{ TViewWindowHandle }

  TViewWindowHandle = class(TiOSWindowHandle)
  private
    [Weak] FPresentation: TiOSStyledPresentation;
  protected
    constructor Create(const APresentation: TiOSStyledPresentation); reintroduce;
    function GetView: UIView; override;
    function GetGLView: GLKView; override;
    function GetForm: TCommonCustomForm; override;
    function GetWnd: UIWindow; override;
    function GetScale: single; override;
  end;

  TOpenControl = class(TControl);
  TOpenStyledControl = class(TStyledControl);

{ TNativeStyledControl }

procedure TNativeStyledControl.AdjustSize;
begin
end;

procedure TNativeStyledControl.ApplyStyle;
begin
  inherited;
  FScene.FPresentation.ApplyStyle;
end;

procedure TNativeStyledControl.FreeStyle;
begin
  FScene.FPresentation.FreeStyle;
  inherited;
end;

function TNativeStyledControl.GetDefaultStyleLookupName: string;
begin
  Result := FScene.FPresentation.GetDefaultStyleLookupName;
end;

{ TViewWindowHandle }

constructor TViewWindowHandle.Create(const APresentation: TiOSStyledPresentation);
begin
  inherited Create(nil);
  FPresentation := APresentation;
end;

function TViewWindowHandle.GetForm: TCommonCustomForm;
begin
  Result := nil;
end;

function TViewWindowHandle.GetGLView: GLKView;
begin
  Result := GLKView(FPresentation.Super);
end;

function TViewWindowHandle.GetScale: single;
begin
  Result := MainScreen.scale;
end;

function TViewWindowHandle.GetView: UIView;
begin
  Result := UIView(FPresentation.Super);
end;

function TViewWindowHandle.GetWnd: UIWindow;
begin
  Result := FPresentation.View.window;
end;

constructor TNativeScene.Create(APresentation: TiOSStyledPresentation);
var
  Handle: TiOSWindowHandle;
begin
  inherited Create(nil);
  FPresentation := APresentation;
  Handle := TViewWindowHandle.Create(FPresentation);
  FCanvas := TCanvasManager.CreateFromWindow(Handle, Round(FPresentation.Size.Width),
    Round(APresentation.Size.Height));
  FStyledControl := TNativeStyledControl.Create(nil);
  FStyledControl.DisableDisappear := True;
  FStyledControl.FScene := Self;
  FStyledControl.SetNewScene(Self);
  FStyledControl.Parent := Self;
  FStyledControl.Lock;
end;

destructor TNativeScene.Destroy;
begin
  FCanvas.Free;
  FStyledControl.Free;
  inherited;
end;

{ TNativeScene.IControl }

procedure TNativeScene.AfterDialogKey(var Key: Word; Shift: TShiftState);
begin
  FStyledControl.AfterDialogKey(Key, Shift);
end;

procedure TNativeScene.BeginAutoDrag;
begin
  FStyledControl.BeginAutoDrag;
end;

procedure TNativeScene.BeginUpdate;
begin
  FStyledControl.BeginUpdate;
end;

function TNativeScene.CheckForAllowFocus: Boolean;
begin
  Result := FStyledControl.CheckForAllowFocus;
end;

procedure TNativeScene.DialogKey(var Key: Word; Shift: TShiftState);
begin
  FStyledControl.DialogKey(Key, Shift);
end;

procedure TNativeScene.DoActivate;
begin
  FStyledControl.DoActivate;
end;

procedure TNativeScene.DoDeactivate;
begin
  FStyledControl.DoDeactivate;
end;

procedure TNativeScene.DoEnter;
begin
  FStyledControl.DoEnter;
end;

procedure TNativeScene.DoExit;
begin
  FStyledControl.DoExit;
end;

procedure TNativeScene.DoMouseEnter;
begin
  FStyledControl.DoMouseEnter;
end;

procedure TNativeScene.DoMouseLeave;
begin
  FStyledControl.DoMouseLeave;
end;

procedure TNativeScene.DragDrop(const Data: TDragObject; const Point: TPointF);
begin
  FStyledControl.DragDrop(Data, Point);
end;

procedure TNativeScene.DragEnd;
begin
  FStyledControl.DragEnd;
end;

procedure TNativeScene.DragEnter(const Data: TDragObject; const Point: TPointF);
begin
  FStyledControl.DragEnter(Data, Point);
end;

procedure TNativeScene.DragLeave;
begin
  FStyledControl.DragLeave;
end;

procedure TNativeScene.DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  FStyledControl.DragOver(Data, Point, Operation);
end;

procedure TNativeScene.EnableUpdating;
begin
  Dec(FDisableUpdating);
  if FDisableUpdating < 0 then
    raise EInvalidSceneUpdatingPairCall.Create(SInvalidSceneUpdatingPairCall);
end;

procedure TNativeScene.EndUpdate;
begin
  FStyledControl.EndUpdate;
end;

function TNativeScene.EnterChildren(AObject: IControl): Boolean;
begin
  Result := FStyledControl.EnterChildren(AObject);
end;

function TNativeScene.ExitChildren(AObject: IControl): Boolean;
begin
  Result := FStyledControl.ExitChildren(AObject);
end;

function TNativeScene.FindTarget(P: TPointF; const Data: TDragObject): IControl;
begin
  Result := FStyledControl.FindTarget(P, Data);
end;

function TNativeScene.GetAbsoluteEnabled: Boolean;
begin
  Result := FStyledControl.AbsoluteEnabled;
end;

function TNativeScene.GetAcceptsControls: Boolean;
begin
  Result := FStyledControl.GetAcceptsControls;
end;

function TNativeScene.GetCanFocus: Boolean;
begin
  Result := FStyledControl.GetCanFocus;
end;

function TNativeScene.GetCanParentFocus: Boolean;
begin
  Result := FStyledControl.GetCanParentFocus;
end;

function TNativeScene.GetCursor: TCursor;
begin
  Result := FStyledControl.Cursor;
end;

function TNativeScene.GetDesignInteractive: Boolean;
begin
  Result := FStyledControl.GetDesignInteractive;
end;

function TNativeScene.GetDragMode: TDragMode;
begin
  Result := FStyledControl.DragMode;
end;

function TNativeScene.GetEnabled: Boolean;
begin
  Result := True;
end;

function TNativeScene.GetHitTest: Boolean;
begin
  Result := True;
end;

function TNativeScene.GetInheritedCursor: TCursor;
begin
  Result := FStyledControl.InheritedCursor;
end;

function TNativeScene.GetIsFocused: boolean;
begin
  Result := FStyledControl.IsFocused;
end;

function TNativeScene.GetLocked: Boolean;
begin
  Result := False;
end;

function TNativeScene.GetObject: TFmxObject;
begin
  Result := Self;
end;

function TNativeScene.GetParent: TFmxObject;
begin
  Result := Parent;
end;

function TNativeScene.GetPopupMenu: TCustomPopupMenu;
begin
  Result := FStyledControl.PopupMenu;
end;

function TNativeScene.GetTabStop: Boolean;
begin
  Result := FStyledControl.TabStop;
end;

function TNativeScene.GetTabStopController: ITabStopController;
begin
  Result := FStyledControl.GetTabStopController;
end;

function TNativeScene.GetView: UIView;
begin
  Result := FPresentation.View;
end;

function TNativeScene.GetVisible: Boolean;
begin
  Result := True;
end;

procedure TNativeScene.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  FStyledControl.KeyDown(Key, KeyChar, Shift);
end;

procedure TNativeScene.KeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  FStyledControl.KeyUp(Key, KeyChar, Shift);
end;

function TNativeScene.LocalToScreen(P: TPointF): TPointF;
var
  Point: NSPoint;
begin
  Point := View.window.convertPoint(CGPointMake(P.X, P.Y), View);
  Result := TPointF.Create(Point.x, Point.y);
end;

function TNativeScene.ScreenToLocal(P: TPointF): TPointF;
var
  Point: NSPoint;
begin
  Point := View.convertPoint(CGPointMake(P.X, P.Y), View.window);
  Result := TPointF.Create(Point.x, Point.y);
end;

procedure TNativeScene.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
end;

procedure TNativeScene.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
end;

procedure TNativeScene.MouseMove(Shift: TShiftState; X, Y: Single);
begin
end;

procedure TNativeScene.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
end;

procedure TNativeScene.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
end;

function TNativeScene.ObjectAtPoint(P: TPointF): IControl;
begin
  Result := FStyledControl.ObjectAtPoint(P);
  if (Result <> nil) and (Result.GetObject = FStyledControl) then
    Result := nil;
end;

procedure TNativeScene.Repaint;
begin
  View.setNeedsDisplay;
end;

procedure TNativeScene.SetAcceptsControls(const Value: Boolean);
begin

end;

procedure TNativeScene.DisableUpdating;
begin
  Inc(FDisableUpdating);
end;

procedure TNativeScene.SetDragMode(const ADragMode: TDragMode);
begin

end;

procedure TNativeScene.SetFocus;
begin

end;

procedure TNativeScene.SetTabStop(const TabStop: Boolean);
begin

end;

function TNativeScene.HasHint: Boolean;
begin
  Result := False;
end;

function TNativeScene.GetHintString: string;
begin
  Result := string.Empty;
end;

function TNativeScene.GetHintObject: TObject;
begin
  Result := nil;
end;

procedure TNativeScene.SetVisible(const Value: Boolean);
begin

end;

function TNativeScene.ShowContextMenu(const ScreenPosition: TPointF): Boolean;
begin
  Result := False;
end;

procedure TNativeScene.Tap(const Point: TPointF);
begin

end;

{ TNativeScene.IScene }

procedure TNativeScene.AddUpdateRect(R: TRectF);
begin
  if not (csDestroying in ComponentState) and (FDisableUpdating = 0) then
  begin
    R := TRectF.Create(R.TopLeft.Truncate, R.BottomRight.Ceiling);
    if IntersectRect(R, TRectF.Create(0, 0, FPresentation.Size.Width, FPresentation.Size.Height)) then
      FPresentation.View.setNeedsDisplay;
  end;
end;

procedure TNativeScene.ChangeScrollingState(const AControl: TControl; const Active: Boolean);
begin
end;

function TNativeScene.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TNativeScene.GetSceneScale: Single;
begin
  Result := View.window.screen.scale;
end;

function TNativeScene.GetStyleBook: TStyleBook;
begin
  if (FPresentation.Control <> nil) and (FPresentation.Control.Scene <> nil) then
    Result := FPresentation.Control.Scene.StyleBook
  else
    Result := nil;
end;

function TNativeScene.GetUpdateRect(const Index: Integer): TRectF;
begin
  Result := FUpdateRects[Index];
end;

function TNativeScene.GetUpdateRectsCount: Integer;
begin
  Result := Length(FUpdateRects);
end;

procedure TNativeScene.MergeUpdateRects(const UpdateRects: array of TRectF);
var
  I: Integer;
begin
  if Length(UpdateRects) > 0 then
  begin
    if TCanvasStyle.SupportClipRects in TCanvasManager.DefaultCanvas.GetCanvasStyle then
    begin
      SetLength(FUpdateRects, Length(FUpdateRects) + Length(UpdateRects));
      for I := 0 to Length(UpdateRects) - 1 do
        FUpdateRects[Length(FUpdateRects) - Length(UpdateRects) + I] := UpdateRects[I];
    end
    else
    begin
      SetLength(FUpdateRects, 1);
      FUpdateRects[0] := TRectF.Create(0, 0, FPresentation.Size.Width, FPresentation.Size.Height);
    end;
  end;
end;

{ IPaintControl }

procedure TNativeScene.PaintRects(const UpdateRects: array of TRectF);
var
  I, J: Integer;
  R: TRectF;
  AllowPaint: Boolean;
  Control: TOpenControl;
begin
  MergeUpdateRects(UpdateRects);
  if Length(FUpdateRects) > 0 then
  begin
    if FCanvas.BeginScene(@FUpdateRects, FContextHandle) then
    try
      FCanvas.Clear(0);
      if ChildrenCount > 0 then
        for I := 0 to Children.Count - 1 do
          if (Children[I] is TControl) then
          begin
            Control := TOpenControl(Children[I]);
            if not Control.Visible then
              Continue;
            if Control.Scene = nil then
              Continue;
            if not Control.InPaintTo and Control.UpdateRect.IsEmpty then
              Continue;

            AllowPaint := False;
            if Control.InPaintTo then
              AllowPaint := True;
            if not AllowPaint then
            begin
              R := UnionRect(Control.ChildrenRect, Control.UpdateRect);
              for J := 0 to Length(FUpdateRects) - 1 do
                if IntersectRect(FUpdateRects[J], R) then
                begin
                  AllowPaint := True;
                  Break;
                end;
            end;

            if AllowPaint then
              Control.PaintInternal;
          end;
    finally
      FCanvas.EndScene;
    end;
    SetLength(FUpdateRects, 0);
  end;
end;

function TNativeScene.GetContextHandle: THandle;
begin
  Result := FContextHandle;
end;

procedure TNativeScene.SetContextHandle(const AContextHandle: THandle);
begin
  FContextHandle := AContextHandle;
end;

procedure TNativeScene.SetSize(const ASize: TSizeF);
begin
  FCanvas.SetSize(Trunc(ASize.Width), Trunc(ASize.Height));
  FStyledControl.SetBounds(0, 0, ASize.Width, ASize.Height);
end;

procedure TNativeScene.SetStyleBook(const Value: TStyleBook);
begin
end;

{ TiOSStyledPresentation }

constructor TiOSStyledPresentation.Create;
begin
  inherited;
  View.setOpaque(False);
  FNativeScene := TNativeScene.Create(Self);
  Control.InsertObject(0, FNativeScene);
end;

procedure TiOSStyledPresentation.InitView;
var
  V: Pointer;
begin
  V := GLKView(Super).initWithFrame(ViewFrame, TCustomContextIOS.SharedContext);
  if GetObjectID <> V then
    UpdateObjectID(V);
end;

destructor TiOSStyledPresentation.Destroy;
begin
  FNativeScene.Free;
  inherited;
end;

procedure TiOSStyledPresentation.DoApplyStyleLookup;
begin
  FNativeScene.StyledControl.DoApplyStyleLookup;
end;

procedure TiOSStyledPresentation.ApplyStyle;
begin
  if HasControl then
    TOpenStyledControl(Control).ApplyStyle;
end;

procedure TiOSStyledPresentation.FreeStyle;
begin
  if HasControl then
    TOpenStyledControl(Control).FreeStyle;
end;

function TiOSStyledPresentation.GetDefaultStyleLookupName: string;
begin
  Result := TStyledControl(Control).DefaultStyleLookupName;
end;

function TiOSStyledPresentation.GetParentClassStyleLookupName: string;
begin
  Result := TStyledControl(Control).ParentClassStyleLookupName;
end;

procedure TiOSStyledPresentation.PMApplyStyleLookup(var AMessage: TDispatchMessage);
begin
  inherited;
  DoApplyStyleLookup;
end;

procedure TiOSStyledPresentation.PMFindStyleResource(var AMessage: TDispatchMessageWithValue<TFindStyleResourceInfo>);
begin
  inherited;
  AMessage.Value.Resource := FNativeScene.FStyledControl.FindStyleResource(AMessage.Value.ResourceName, AMessage.Value.Clone);
end;

procedure TiOSStyledPresentation.PMNeedStyleLookup(var AMessage: TDispatchMessage);
begin
  inherited;
  FNativeScene.StyledControl.NeedStyleLookup;
end;

procedure TiOSStyledPresentation.PMObjectAtPoint(var AMessage: TDispatchMessageWithValue<TObjectAtPointInfo>);
begin
  inherited;
  AMessage.Value.Control := FNativeScene.ObjectAtPoint(AMessage.Value.Point);
end;

procedure TiOSStyledPresentation.PMSetStyleLookup(var AMessage: TDispatchMessageWithValue<string>);
begin
  inherited;
  FNativeScene.StyledControl.StyleLookup := AMessage.Value;
end;

procedure TiOSStyledPresentation.PMApplyTriggerEffect(var AMessage: TDispatchMessageWithValue<TTriggerInfo>);
begin
  FNativeScene.StyledControl.ApplyTriggerEffect(AMessage.Value.Instance, AMessage.Value.Trigger);
end;

procedure TiOSStyledPresentation.PMStartTriggerAnimation(var AMessage: TDispatchMessageWithValue<TTriggerInfo>);
begin
  if AMessage.Value.Wait then
    FNativeScene.StyledControl.StartTriggerAnimationWait(AMessage.Value.Instance, AMessage.Value.Trigger)
  else
    FNativeScene.StyledControl.StartTriggerAnimation(AMessage.Value.Instance, AMessage.Value.Trigger);
end;

procedure TiOSStyledPresentation.PMRealign(var AMessage: TDispatchMessage);
begin
  inherited;
  FNativeScene.StyledControl.Realign;
end;

procedure TiOSStyledPresentation.PMSetAdjustSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
  FNativeScene.StyledControl.SetAdjustSizeValue(AMessage.Value);
end;

procedure TiOSStyledPresentation.PMSetAdjustType(var AMessage: TDispatchMessageWithValue<TAdjustType>);
begin
  FNativeScene.StyledControl.SetAdjustType(AMessage.Value);
end;

procedure TiOSStyledPresentation.drawRect(R: CGRect);
var
  PaintControl: IPaintControl;
begin
  if (FNativeScene <> nil) and Supports(FNativeScene, IPaintControl, PaintControl) then
    PaintControl.PaintRects([TRectF.Create(R.origin.x, R.origin.y, R.origin.x + R.size.width, R.origin.y + R.size.height)]);
end;

function TiOSStyledPresentation.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IiOSSceneControl);
end;

function TiOSStyledPresentation.GetStyledControl: TNativeStyledControl;
begin
  Result := FNativeScene.StyledControl;
end;

function TiOSStyledPresentation.GetView: GLKView;
begin
  Result := GLKView(Super);
end;

procedure TiOSStyledPresentation.PMGetAdjustSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
  AMessage.Value := FNativeScene.StyledControl.AdjustSizeValue;
end;

procedure TiOSStyledPresentation.PMGetAdjustType(var AMessage: TDispatchMessageWithValue<TAdjustType>);
begin
  AMessage.Value := FNativeScene.StyledControl.AdjustType;
end;

procedure TiOSStyledPresentation.PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
end;

procedure TiOSStyledPresentation.PMGetResourceLink(var AMessage: TDispatchMessageWithValue<TFmxObject>);
begin
  AMessage.Value := FNativeScene.StyledControl.ResourceLink;
end;

procedure TiOSStyledPresentation.SetSize(const ASize: TSizeF);
begin
  inherited;
  if FNativeScene <> nil then
    FNativeScene.SetSize(Size);
end;

initialization
  TPresentationProxyFactory.Current.RegisterDefault(TControlType.Platform, TiOSPresentationProxy<TiOSStyledPresentation>);
finalization
  TPresentationProxyFactory.Current.UnregisterDefault(TControlType.Platform);
end.
