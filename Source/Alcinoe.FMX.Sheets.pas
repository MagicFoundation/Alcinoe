unit Alcinoe.FMX.Sheets;

interface

uses
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  System.Messaging,
  System.UITypes,
  FMX.Controls,
  FMX.Types,
  Alcinoe.Common,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.NativeControl,
  Alcinoe.FMX.ScrollEngine;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // TALSheet is the root control of the Sheet system.
  // It represents the scrim that overlays the entire form,
  // blocking all touch events on the underlying UI.
  TALSheet = class(TALRectangle, IALScrollableControl)
  public
    type
      // ---------
      // TDockEdge
      TDockEdge = (Top, Bottom, Left, Right);
      // ----------------
      // TOnActionRefProc
      TOnActionRefProc = reference to procedure(Const ASheet: TALSheet; const AAction: Integer; var ACanClose: Boolean);
      // ----------------
      // TOnActionObjProc
      TOnActionObjProc = procedure(Const ASheet: TALSheet; const AAction: Integer; var ACanClose: Boolean) of object;
      // ------------------------
      // TViewportPositionChangeEvent
      TViewportPositionChangeEvent = procedure (Sender: TObject; const OldViewportPosition, NewViewportPosition: TALPointD) of object;
      // --------------
      // TAnimateOption
      TAnimateOption = (AnimateScrim, AnimateContainer);
      // ---------------
      // TAnimateOptions
      TAnimateOptions = set of TAnimateOption;
      // --------
      // TBuilder
      TBuilder = Class(TObject)
      private
        FSheet: TALSheet;
      protected
        function CreateSheet: TALSheet; virtual; abstract;
      public
        constructor Create; virtual;
        destructor Destroy; override;
        function SetOwnerAndParent(const AValue: TALControl): TBuilder;
        function SetContent(const AValue: TALControl): TBuilder;
        function SetContainerMargins(const AValue: TRectF): TBuilder;
        function SetContainerCorners(const AValue: TCorners): TBuilder;
        function SetCloseOnScrimClick(const AValue: boolean): TBuilder;
        function SetOnViewportPositionChange(const AValue: TViewportPositionChangeEvent): TBuilder; overload;
        function SetOnActionCallback(const AValue: TOnActionRefProc): TBuilder; overload;
        function SetOnActionCallback(const AValue: TOnActionObjProc): TBuilder; overload;
        /// <summary>
        ///   Requests the TALSheetManager to display this sheet.
        ///   The Builder instance will be released during this operation.
        /// </summary>
        procedure Show;
        property Sheet: TALSheet read FSheet;
      end;
      // -------------
      // TScrollEngine
      TScrollEngine = class(TALScrollEngine)
      public
        const
          MAXIMUM_FLING_VELOCITY = 8000; // From android ViewConfiguration.MAXIMUM_FLING_VELOCITY
          MAX_SETTLE_DURATION = 600; // From android ViewPager.MAX_SETTLE_DURATION
      private
        FSheet: TALSheet; // 8 bytes;
        FLastViewportPosition: TALPointD; // 16 bytes;
      protected
        procedure DoMouseUp; override;
        procedure DoStart; override;
        procedure DoStop; override;
        procedure DoChanged; override;
      public
        constructor Create(const ASheet: TALSheet); reintroduce;
        property Sheet: TALSheet read FSheet;
      end;
  private
    FDockEdge: TDockEdge;
    FScrollEngine: TALScrollEngine;
    fMouseDownPos: TPointF;
    FHandleMouseEvents: Boolean;
    fScrollCapturedByMe: boolean;
    FFillAlphaAtPeek: Single;
    FContainer: TALRectangle;
    FContent: TALControl;
    FTouchBlocker: TALLayout;
    FDisableScrollRealign: Boolean;
    FShowAnimateOptions: TAnimateOptions;
    FCloseAnimateOptions: TAnimateOptions;
    FOnActionRefProc: TALSheet.TOnActionRefProc;
    FOnActionObjProc: TALSheet.TOnActionObjProc;
    FOnViewportPositionChange: TViewportPositionChangeEvent;
    FOnClosedRefProc: TProc;
    function GetCloseOnScrimClick: boolean;
    procedure SetCloseOnScrimClick(const AValue: Boolean);
    procedure SetContent(const AValue: TALControl);
    procedure ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
    procedure internalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure internalMouseMove(Shift: TShiftState; X, Y: Single);
    procedure internalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure internalMouseLeave;
    { IALScrollableControl }
    function GetScrollEngine: TALScrollEngine;
    procedure SetScrollEngine(const Value: TALScrollEngine);
  protected
    function CreateScrollEngine: TScrollEngine; virtual;
    procedure DoRealign; override;
    procedure ChildrenMouseDown(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseMove(const AObject: TControl; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseUp(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseLeave(const AObject: TControl); override;
    procedure AfterPaint; override;
    property DockEdge: TDockEdge read FDockEdge;
  public
    constructor Create(const AOwner: TComponent; const ADockEdge: TDockEdge); reintroduce; virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    class function Current: TALSheet;
    class procedure CloseCurrent(const AOnClosedCallback: TProc = nil; const AAnimateOptions: TAnimateOptions = [TAnimateOption.AnimateScrim, TAnimateOption.AnimateContainer]);
    function IsTransitioning: Boolean;
    property CloseOnScrimClick: Boolean read GetCloseOnScrimClick write SetCloseOnScrimClick;
    procedure ActionButtonClick(Sender: TObject); virtual;
    property Container: TALRectangle read FContainer;
    property Content: TALControl read FContent write SetContent;
    property ScrollEngine: TALScrollEngine read GetScrollEngine write SetScrollEngine;
    property ShowAnimateOptions: TAnimateOptions read FShowAnimateOptions write FShowAnimateOptions;
    property CloseAnimateOptions: TAnimateOptions read FCloseAnimateOptions write FCloseAnimateOptions;
    property OnActionRefProc: TALSheet.TOnActionRefProc read FOnActionRefProc write FOnActionRefProc;
    property OnActionObjProc: TALSheet.TOnActionObjProc read FOnActionObjProc write FOnActionObjProc;
    property OnViewportPositionChange: TViewportPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
    property OnClosedRefProc: TProc read FOnClosedRefProc write FOnClosedRefProc;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALVerticalSheet = class(TALSheet)
  public
    type
      //---------
      // TBuilder
      TBuilder = Class(TALSheet.TBuilder);
  private
    FDragHandle: TALRectangle;
  public
    constructor Create(const AOwner: TComponent; const ADockEdge: TALSheet.TDockEdge); override;
    property DragHandle: TALRectangle read FDragHandle;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTopSheet = class(TALVerticalSheet)
  public
    type
      //---------
      // TBuilder
      TBuilder = Class(TALVerticalSheet.TBuilder)
      private
        function GetSheet: TALTopSheet;
      protected
        function CreateSheet: TALSheet; override;
      public
        property Sheet: TALTopSheet read GetSheet;
      end;
  public
    /// <summary>
    ///   Creates a builder object. The builder will automatically be
    ///   released when calling the Show method.
    ///   If you do not call Show, you are responsible for releasing the builder manually.
    /// </summary>
    class function Builder: TBuilder;
  public
    constructor Create(const AOwner: TComponent); reintroduce; virtual;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBottomSheet = class(TALVerticalSheet)
  public
    type
      //---------
      // TBuilder
      TBuilder = Class(TALVerticalSheet.TBuilder)
      private
        function GetSheet: TALBottomSheet;
      protected
        function CreateSheet: TALSheet; override;
      public
        property Sheet: TALBottomSheet read GetSheet;
      end;
  public
    /// <summary>
    ///   Creates a builder object. The builder will automatically be
    ///   released when calling the Show method.
    ///   If you do not call Show, you are responsible for releasing the builder manually.
    /// </summary>
    class function Builder: TBuilder;
  private
    FVirtualKeyboardAnimation: TALFloatAnimation;
    procedure VirtualKeyboardChangeHandler(const Sender: TObject; const Msg: System.Messaging.TMessage); virtual;
    procedure VirtualKeyboardAnimationProcess(Sender: TObject);
  public
    constructor Create(const AOwner: TComponent); reintroduce; virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function IsVirtualKeyboardAnimationRunning: Boolean;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // Called "SideSheet" in Material 3.
  TALHorizontalSheet = class(TALSheet)
  public
    type
      //---------
      // TBuilder
      TBuilder = Class(TALSheet.TBuilder)
      public
        function SetHeadlineText(const AValue: String): TBuilder;
        function AddBackButton(const ATag: NativeInt): TBuilder;
        function AddCloseButton(const ATag: NativeInt): TBuilder;
      End;
  private
    FHeaderBar: TALRectangle;
    FBackButton: TALButton;
    FCloseButton: TALButton;
    FHeadline: TALText;
    function GetHeaderBar: TALRectangle;
    function GetBackButton: TALButton;
    function GetCloseButton: TALButton;
    function GetHeadline: TALText;
  public
    constructor Create(const AOwner: TComponent; const ADockEdge: TALSheet.TDockEdge); override;
    function HasHeadline: Boolean;
    function HasBackButton: Boolean;
    function HasCloseButton: Boolean;
    function HasHeaderBar: Boolean;
    procedure AddBackButton(const ATag: NativeInt);
    procedure AddCloseButton(const ATag: NativeInt);
    property HeaderBar: TALRectangle read GetHeaderBar;
    property BackButton: TALButton read GetBackButton;
    property CloseButton: TALButton read GetCloseButton;
    property Headline: TALText read GetHeadline;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALLeftSheet = class(TALHorizontalSheet)
  public
    type
      //---------
      // TBuilder
      TBuilder = Class(TALHorizontalSheet.TBuilder)
      private
        function GetSheet: TALLeftSheet;
      protected
        function CreateSheet: TALSheet; override;
      public
        property Sheet: TALLeftSheet read GetSheet;
      end;
  public
    /// <summary>
    ///   Creates a builder object. The builder will automatically be
    ///   released when calling the Show method.
    ///   If you do not call Show, you are responsible for releasing the builder manually.
    /// </summary>
    class function Builder: TBuilder;
  public
    constructor Create(const AOwner: TComponent); reintroduce; virtual;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALRightSheet = class(TALHorizontalSheet)
  public
    type
      //---------
      // TBuilder
      TBuilder = Class(TALHorizontalSheet.TBuilder)
      private
        function GetSheet: TALRightSheet;
      protected
        function CreateSheet: TALSheet; override;
      public
        property Sheet: TALRightSheet read GetSheet;
      end;
  public
    /// <summary>
    ///   Creates a builder object. The builder will automatically be
    ///   released when calling the Show method.
    ///   If you do not call Show, you are responsible for releasing the builder manually.
    /// </summary>
    class function Builder: TBuilder;
  public
    constructor Create(const AOwner: TComponent); reintroduce; virtual;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALSheetManager = class(TInterfacedObject, IFreeNotification)
  private
    class function CreateInstance: TALSheetManager;
    class function GetInstance: TALSheetManager; static;
  protected
    class var FInstance: TALSheetManager;
  public
    type
      TCreateInstanceFunc = function: TALSheetManager;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALSheetManager read GetInstance;
    class function HasInstance: Boolean; inline;
  private
    FDefaultScrim: TALRectangle;
    FDefaultLeftSheetContainer: TALRectangle;
    FDefaultRightSheetContainer: TALRectangle;
    FDefaultTopSheetContainer: TALRectangle;
    FDefaultBottomSheetContainer: TALRectangle;
    FDefaultTopSheetDragHandle: TALRectangle;
    FDefaultBottomSheetDragHandle: TALRectangle;
    FDefaultLeftSheetHeaderBar: TALRectangle;
    FDefaultRightSheetHeaderBar: TALRectangle;
    FDefaultLeftSheetBackButton: TALButton;
    FDefaultRightSheetBackButton: TALButton;
    FDefaultLeftSheetHeadline: TALText;
    FDefaultRightSheetHeadline: TALText;
    FDefaultLeftSheetCloseButton: TALButton;
    FDefaultRightSheetCloseButton: TALButton;
    FCurrentSheet: TALSheet;
    FScrimAnimation: TALFloatAnimation;
    FContainerAnimation: TALFloatAnimation;
    FQueue: TQueue<TALSheet>;
    FFrozenNativeControls: TArray<TALNativeControl>;
    procedure FreezeNativeViews;
    procedure UnfreezeNativeViews;
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject);
  protected
    procedure ScrimAnimationProcess(Sender: TObject);
    procedure ContainerAnimationProcess(Sender: TObject);
    procedure ScrimAnimationFinish(Sender: TObject);
    procedure ContainerAnimationFinish(Sender: TObject);
    Function HasPendingSheets: Boolean;
    procedure ProcessPendingSheets;
    procedure ShowSheet(const ASheet: TALSheet);
    procedure DoCloseCurrentSheet;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure RequestSheet(const ASheet: TALSheet);
    property DefaultScrim: TALRectangle read FDefaultScrim;
    property DefaultLeftSheetContainer: TALRectangle read FDefaultLeftSheetContainer;
    property DefaultRightSheetContainer: TALRectangle read FDefaultRightSheetContainer;
    property DefaultTopSheetContainer: TALRectangle read FDefaultTopSheetContainer;
    property DefaultBottomSheetContainer: TALRectangle read FDefaultBottomSheetContainer;
    property DefaultTopSheetDragHandle: TALRectangle read FDefaultTopSheetDragHandle;
    property DefaultBottomSheetDragHandle: TALRectangle read FDefaultBottomSheetDragHandle;
    property DefaultLeftSheetHeaderBar: TALRectangle read FDefaultLeftSheetHeaderBar;
    property DefaultRightSheetHeaderBar: TALRectangle read FDefaultRightSheetHeaderBar;
    property DefaultLeftSheetBackButton: TALButton read FDefaultLeftSheetBackButton;
    property DefaultRightSheetBackButton: TALButton read FDefaultRightSheetBackButton;
    property DefaultLeftSheetHeadline: TALText read FDefaultLeftSheetHeadline;
    property DefaultRightSheetHeadline: TALText read FDefaultRightSheetHeadline;
    property DefaultLeftSheetCloseButton: TALButton read FDefaultLeftSheetCloseButton;
    property DefaultRightSheetCloseButton: TALButton read FDefaultRightSheetCloseButton;
    property CurrentSheet: TALSheet read fCurrentSheet write fCurrentSheet;
    function IsShowingSheet: Boolean;
    procedure CloseCurrentSheet;
  end;

var
  ALSheetCloseButtonResourceName: String;
  ALLeftSheetBackButtonResourceName: String;
  ALRightSheetBackButtonResourceName: String;

implementation

uses
  System.Math,
  System.Math.Vectors,
  FMX.Forms,
  FMX.Graphics,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Styles,
  Alcinoe.fmx.Dialogs,
  Alcinoe.fmx.LoadingOverlay;

{***********************************}
constructor TALSheet.TBuilder.Create;
begin
  Inherited create;
  FSheet := CreateSheet;
  FSheet.BeginUpdate;
end;

{***********************************}
destructor TALSheet.TBuilder.Destroy;
begin
  ALFreeAndNil(FSheet);
  inherited;
end;

{*******************************************************************************}
function TALSheet.TBuilder.SetOwnerAndParent(const AValue: TALControl): TBuilder;
begin
  // This will defacto call AValue.Realign and FSheet.EndUpdate
  // in TControl.DoAddObject.SetUpdatingState
  AValue.InsertComponent(FSheet);
  FSheet.Parent := AValue;
  FSheet.BeginUpdate;
  Result := Self;
end;

{************************************************************************}
function TALSheet.TBuilder.SetContent(const AValue: TALControl): TBuilder;
begin
  FSheet.Content := AValue;
  Result := Self;
end;

{*****************************************************************************}
function TALSheet.TBuilder.SetContainerMargins(const AValue: TRectF): TBuilder;
begin
  FSheet.Container.Margins.Rect := AValue;
  Result := Self;
end;

{*******************************************************************************}
function TALSheet.TBuilder.SetContainerCorners(const AValue: TCorners): TBuilder;
begin
  FSheet.Container.Corners := AValue;
  Result := Self;
end;

{*******************************************************************************}
function TALSheet.TBuilder.SetCloseOnScrimClick(const AValue: boolean): TBuilder;
begin
  FSheet.SetCloseOnScrimClick(AValue);
  Result := Self;
end;

{***********************************************************************************************************}
function TALSheet.TBuilder.SetOnViewportPositionChange(const AValue: TViewportPositionChangeEvent): TBuilder;
begin
  FSheet.OnViewportPositionChange := AValue;
  Result := Self;
end;

{***************************************************************************************}
function TALSheet.TBuilder.SetOnActionCallback(const AValue: TOnActionRefProc): TBuilder;
begin
  FSheet.OnActionRefProc := AValue;
  Result := Self;
end;

{***************************************************************************************}
function TALSheet.TBuilder.SetOnActionCallback(const AValue: TOnActionObjProc): TBuilder;
begin
  FSheet.OnActionObjProc := AValue;
  Result := Self;
end;

{*******************************}
procedure TALSheet.TBuilder.Show;
begin
  TALSheetManager.Instance.RequestSheet(FSheet);
  FSheet := nil;
  Free;
end;

{****************************************************************}
constructor TALSheet.TScrollEngine.Create(const ASheet: TALSheet);
begin
  inherited Create;
  FSheet := ASheet;
  FLastViewportPosition := TALPointD.Create(0,0);
  MinEdgeDragResistanceFactor := 0;
  MAxEdgeDragResistanceFactor := 0;
  MinEdgeSpringbackEnabled := False;
  MaxEdgeSpringbackEnabled := False;
End;

{*****************************************}
procedure TALSheet.TScrollEngine.DoMouseUp;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // Extracted from the distanceInfluenceForSnapDuration method in ViewPager.java.
  // We want the duration of the page snap animation to be influenced by the distance that
  // the screen has to travel, however, we don't want this duration to be effected in a
  // purely linear fashion. Instead, we use this method to moderate the effect that the distance
  // of travel has on the overall snap duration.
  function distanceInfluenceForSnapDuration(f: Single): single;
  begin
    f := f - 0.5; // center the values about 0.
    f := f * (0.3 * PI / 2.0);
    Result := sin(f);
  end;

begin

  // Record whether the control was in
  // a pressed (down) state before processing.
  var LWasDown := Down;

   // Call the inherited mouse up handler.
  inherited;

  // If the control was not pressed before
  // this event, exit early.
  if not LWasDown then exit;

  {$IFDEF DEBUG}
  //ALLog(ClassName + '.DoMouseUp', 'ScrollCapturedByMe:'+ALBooltoStrW(FSheet.fScrollCapturedByMe, 'True', 'False'));
  {$ENDIF}

  // Calculate LCmpVelocity
  var LCmpVelocity: TValueRelationship;
  case FSheet.DockEdge of
    TDockEdge.Left, TDockEdge.Right:
      LCmpVelocity := compareValue(UpVelocity.X, 0, TALScrollEngine.DefaultLowVelocityThreshold);
    TDockEdge.Top, TDockEdge.Bottom:
      LCmpVelocity := compareValue(UpVelocity.Y, 0, TALScrollEngine.DefaultLowVelocityThreshold);
    else
      Raise Exception.Create('Error 97194686-A646-40FF-BF80-0D38EAED19D5')
  end;

  // Calculate LTargetPosition
  var LTargetPosition: TALPointD;
  if LCmpVelocity > 0 then begin
    case FSheet.DockEdge of
      TDockEdge.Left:
        // Hide the sheet
        LTargetPosition := TALPointD.Create(
                             0,
                             FSheet.Container.Margins.Top);
      TDockEdge.Right:
        // show the sheet
        LTargetPosition := TALPointD.Create(
                             FSheet.ScrollEngine.MaxScrollLimit.X,
                             FSheet.Container.Margins.Top);
      TDockEdge.Top:
        // Hide the sheet
        LTargetPosition := TALPointD.Create(
                             FSheet.Container.Margins.Left,
                             0);
      TDockEdge.Bottom:
        // show the sheet
        LTargetPosition := TALPointD.Create(
                             FSheet.Container.Margins.Left,
                             FSheet.ScrollEngine.MaxScrollLimit.Y);
      else
        Raise Exception.Create('Error 71322CB0-BD8F-416E-9CE6-55994AEE0CDC')
    end;
  end
  else if LCmpVelocity < 0 then begin
    case FSheet.DockEdge of
      TDockEdge.Left:
        // show the sheet
        LTargetPosition := TALPointD.Create(
                             FSheet.ScrollEngine.MinScrollLimit.X,
                             FSheet.Container.Margins.Top);
      TDockEdge.Right:
        // Hide the sheet
        LTargetPosition := TALPointD.Create(
                             0,
                             FSheet.Container.Margins.Top);
      TDockEdge.Top:
        // show the sheet
        LTargetPosition := TALPointD.Create(
                             FSheet.Container.Margins.Left,
                             FSheet.ScrollEngine.MinScrollLimit.Y);
      TDockEdge.Bottom:
        // Hide the sheet
        LTargetPosition := TALPointD.Create(
                             FSheet.Container.Margins.Left,
                             0);
      else Raise Exception.Create('Error FF8939B5-B737-4DC9-8D44-D28D9C3C8A45')
    end;
  end
  else begin
    case FSheet.DockEdge of
      TDockEdge.Left: begin
        if ViewPortPosition.X > FSheet.ScrollEngine.MinScrollLimit.X / 2 then
          // Hide the sheet
          LTargetPosition := TALPointD.Create(
                               0,
                               FSheet.Container.Margins.Top)
        else
          // show the sheet
          LTargetPosition := TALPointD.Create(
                               FSheet.ScrollEngine.MinScrollLimit.X,
                               FSheet.Container.Margins.Top);
      end;
      TDockEdge.Right: begin
        if ViewPortPosition.X < FSheet.ScrollEngine.MaxScrollLimit.X / 2 then
          // Hide the sheet
          LTargetPosition := TALPointD.Create(
                               0,
                               FSheet.Container.Margins.Top)
        else
          // show the sheet
          LTargetPosition := TALPointD.Create(
                               FSheet.ScrollEngine.MaxScrollLimit.X,
                               FSheet.Container.Margins.Top);
      end;
      TDockEdge.Top: begin
        if ViewPortPosition.Y > FSheet.ScrollEngine.MinScrollLimit.Y / 2 then
          // Hide the sheet
          LTargetPosition := TALPointD.Create(
                               FSheet.Container.Margins.Left,
                               0)
        else
          // show the sheet
          LTargetPosition := TALPointD.Create(
                               FSheet.Container.Margins.Left,
                               FSheet.ScrollEngine.MinScrollLimit.Y);
      end;
      TDockEdge.Bottom: begin
        if ViewPortPosition.Y < FSheet.ScrollEngine.MaxScrollLimit.Y / 2 then
          // Hide the sheet
          LTargetPosition := TALPointD.Create(
                               FSheet.Container.Margins.Left,
                               0)
        else
          // show the sheet
          LTargetPosition := TALPointD.Create(
                               FSheet.Container.Margins.Left,
                               FSheet.ScrollEngine.MaxScrollLimit.Y);
      end;
      else
        Raise Exception.Create('Error A847D0EA-6131-42FB-ABF3-E60B13DE9DCC')
    end;
  end;

  // The following code is extracted from
  // ViewPager.java's smoothScrollTo method.

  if FSheet.DockEdge in [TDockEdge.Left, TDockEdge.Right] then begin

    var sx: Single := ViewPortPosition.X;
    var dx: Single := LTargetPosition.X - sx;
    if SameValue(dx, 0, TEpsilon.Position) then exit;

    var LWidth := FSheet.Container.Width;
    if FSheet.DockEdge = TDockEdge.Left then LWidth := LWidth + FSheet.Container.Margins.Left
    else if FSheet.DockEdge = TDockEdge.Right then LWidth := LWidth + FSheet.Container.Margins.Right
    else raise Exception.Create('Error C0915F33-3BAF-43D0-8206-0F2C564DBC5C');
    var LHalfWidth: Single := LWidth / 2;
    var LDistanceRatio: Single := Min(1.0, Abs(dx) / LWidth);
    var LDistance: Single := LHalfWidth + LHalfWidth * distanceInfluenceForSnapDuration(LDistanceRatio);

    var LDuration: integer;
    var LMaximumVelocity := MAXIMUM_FLING_VELOCITY * ALGetScreenScale;
    var LVelocity: Single := abs(EnsureRange(CurrentVelocity.X, -LMaximumVelocity, LMaximumVelocity));
    if (Lvelocity > 0) then LDuration := {4}3 * round(1000 * abs(LDistance / Lvelocity))
    else begin
      //
      // in smoothScrollTo it's written
      //
      // final float pageWidth = width * mAdapter.getPageWidth(mCurItem);
      //
      // but in PagerAdapter we have
      //
      //   public float getPageWidth(int position) {
      //     return 1.f;
      //   }
      //
      var LPageWidth: Single := LWidth {* mAdapter.getPageWidth(mCurItem)};
      var LPageDelta: Single := abs(dx) / (LPageWidth {+ mPageMargin});
      LDuration := Trunc((LPageDelta + 1) * 100);
      //
      // If the scroll distance equals one full page (including margin), then:
      //   pageDelta would be 1, so duration would be (1+1)x100=200 milliseconds.
      // If the scroll distance is half of that, then:
      //   pageDelta would be 0.5, and duration would be (0.5+1)x100=150 milliseconds.
      //
      // In general, this calculation produces a duration between 100 and 150 ms, which is too fast.
      // To mitigate this, I multiply the computed duration by 2.
      //
      LDuration := LDuration * 2;
    end;
    LDuration := Min(LDuration, MAX_SETTLE_DURATION);

    startScroll(sx, ViewPortPosition.Y, dx, 0, LDuration);
    If SameValue(sx+dx, 0, TEpsilon.Position) then begin
      FSheet.HitTest := False;
      FSheet.FDisableScrollRealign := True;
      try
        FSheet.FTouchBlocker.Parent := FSheet.Container;
        FSheet.FTouchBlocker.Tag := 1;
        FSheet.FTouchBlocker.Visible := True;
        FSheet.FTouchBlocker.HitTest := True;
      finally
        FSheet.FDisableScrollRealign := False;
      end;
    end;

  end
  else if FSheet.DockEdge in [TDockEdge.Top, TDockEdge.Bottom] then begin

    var sy: Single := ViewPortPosition.Y;
    var dy: Single := LTargetPosition.y - sy;
    if SameValue(dy, 0, TEpsilon.Position) then exit;

    var LHeight := FSheet.Container.height;
    if FSheet.DockEdge = TDockEdge.Top then LHeight := LHeight + FSheet.Container.Margins.Top
    else if FSheet.DockEdge = TDockEdge.Bottom then LHeight := LHeight + FSheet.Container.Margins.Bottom
    else raise Exception.Create('Error CFD3862C-F862-435C-A233-D0F5ABEFAD0E');
    var LHalfHeight: Single := LHeight / 2;
    var LDistanceRatio: Single := Min(1.0, Abs(dy) / LHeight);
    var LDistance: Single := LHalfHeight + LHalfHeight * distanceInfluenceForSnapDuration(LDistanceRatio);

    var LDuration: integer;
    var LMaximumVelocity := MAXIMUM_FLING_VELOCITY * ALGetScreenScale;
    var LVelocity: Single := abs(EnsureRange(CurrentVelocity.Y, -LMaximumVelocity, LMaximumVelocity));
    if (Lvelocity > 0) then LDuration := {4}3 * round(1000 * abs(LDistance / Lvelocity))
    else begin
      //
      // in smoothScrollTo it's written
      //
      // final float pageWidth = width * mAdapter.getPageWidth(mCurItem);
      //
      // but in PagerAdapter we have
      //
      //   public float getPageWidth(int position) {
      //     return 1.f;
      //   }
      //
      var LPageHeight: Single := LHeight {* mAdapter.getPageHeight(mCurItem)};
      var LPageDelta: Single := abs(dy) / (LPageHeight {+ mPageMargin});
      LDuration := Trunc((LPageDelta + 1) * 100);
      //
      // If the scroll distance equals one full page (including margin), then:
      //   pageDelta would be 1, so duration would be (1+1)x100=200 milliseconds.
      // If the scroll distance is half of that, then:
      //   pageDelta would be 0.5, and duration would be (0.5+1)x100=150 milliseconds.
      //
      // In general, this calculation produces a duration between 100 and 150 ms, which is too fast.
      // To mitigate this, I multiply the computed duration by 2.
      //
      LDuration := LDuration * 2;
    end;
    LDuration := Min(LDuration, MAX_SETTLE_DURATION);

    startScroll(ViewPortPosition.X, sy, 0, dy, LDuration);
    If SameValue(sy+dy, 0, TEpsilon.Position) then begin
      FSheet.HitTest := False;
      FSheet.FDisableScrollRealign := True;
      try
        FSheet.FTouchBlocker.Parent := FSheet.Container;
        FSheet.FTouchBlocker.Tag := 1;
        FSheet.FTouchBlocker.Visible := True;
        FSheet.FTouchBlocker.HitTest := True;
      finally
        FSheet.FDisableScrollRealign := False;
      end;
    end;

  end
  else
    Raise Exception.Create('Error D699159D-4F50-4B1D-B92F-B9839D59C0B7');

end;

{***************************************}
procedure TALSheet.TScrollEngine.DoStart;
begin
  inherited DoStart;
  if (FSheet.Scene <> nil) and
     (not (csDestroying in FSheet.ComponentState)) then
    FSheet.Scene.ChangeScrollingState(FSheet, True);
end;

{**************************************}
procedure TALSheet.TScrollEngine.DoStop;
begin
  inherited DoStop;
  if (FSheet.Scene <> nil) and
     (not (csDestroying in FSheet.ComponentState)) then
    FSheet.Scene.ChangeScrollingState(nil, False);
end;

{*****************************************}
procedure TALSheet.TScrollEngine.DoChanged;
begin

  //{$IF defined(debug)}
  //ALLog(
  //  ClassName + '.DoChanged',
  //  'ViewportPosition.X: ' + ALfloattostrW(ViewportPosition.X) + ' | ' +
  //  'ViewportPosition.Y: ' + ALfloattostrW(ViewportPosition.Y));
  //{$ENDIF}

  if (not (csDestroying in FSheet.ComponentState)) and
     (assigned(FSheet.FTouchBlocker)) and
     ((not FSheet.FTouchBlocker.Visible) or (FSheet.FTouchBlocker.Tag = 1)) then begin

    var LSaveDisableAlign := FSheet.FDisableAlign;
    FSheet.FDisableAlign := True;
    try
      case FSheet.FDockEdge of
        TALSheet.TDockEdge.Left:
          FSheet.Container.Position.Point := TPointF.Create(-FSheet.Container.Width - ViewportPosition.X, FSheet.Container.Margins.Top);
        TALSheet.TDockEdge.Right:
          FSheet.Container.Position.Point := TPointF.Create(FSheet.Width - ViewportPosition.X, FSheet.Container.Margins.Top);
        TALSheet.TDockEdge.Top:
          FSheet.Container.Position.Point := TPointF.Create(FSheet.Container.Margins.Left, -FSheet.Container.Height - ViewportPosition.Y);
        TALSheet.TDockEdge.Bottom:
          FSheet.Container.Position.Point := TPointF.Create(FSheet.Container.Margins.Left, FSheet.Height - ViewportPosition.Y);
        else
          Raise Exception.Create('Error D30941E5-E277-4995-82A4-A7E5514EFA6F')
      end;
    finally
      FSheet.FDisableAlign := LSaveDisableAlign;
    end;

    var LNewViewportPosition := ViewportPosition;
    if (assigned(FSheet.FOnViewportPositionChange)) and
       (not fLastViewportPosition.EqualsTo(LNewViewportPosition, TEpsilon.Position)) then
      FSheet.FOnViewportPositionChange(FSheet, fLastViewportPosition, LNewViewportPosition);
    fLastViewportPosition := LNewViewportPosition;

    // {*} The ScrollEngine suffers from truncation errors because it operates on real pixels.
    // For example, when we call:
    //   startScroll(sx, sy, dx, dy, LDuration);
    // internally it does:
    //
    //   FOverScroller.startScroll(
    //     Trunc(startX * ALScreenScale), // startX: Integer
    //     Trunc(startY * ALScreenScale), // startY: Integer
    //     Trunc(dx * ALScreenScale),     // dx: Integer
    //     Trunc(dy * ALScreenScale),     // dy: Integer
    //     duration);                     // const duration: Integer = DEFAULT_DURATION
    //
    // The use of Trunc is problematic. Consider the call:
    //   startScroll(0, 600.9091796875, 0, 237.567016601562, 100);
    // With a screen scale of 2.625, it becomes:
    //
    //   FOverScroller.startScroll(
    //     0,    // startX
    //     1577, // startY
    //     0,    // dx
    //     623,  // dy
    //     100); // duration
    //
    // Notice that:
    //   (600.9091796875 + 237.567016601562) * 2.625 = 2201.000015258788
    // while
    //   1577 + 623 = 2200
    // This 1-pixel difference arises from truncation.

    case FSheet.FDockEdge of
      TDockEdge.Left: begin
        var LProgress: Single := abs(ViewPortPosition.X) / Max(1, abs(FSheet.ScrollEngine.MinScrollLimit.X));
        FSheet.Fill.Color := ALSetColorAlpha(FSheet.Fill.Color, FSheet.FFillAlphaAtPeek * LProgress);
        If compareValue(FSheet.Container.Position.X, -FSheet.Container.Width + 1{*}, TEpsilon.Position) <= 0 then
          TALSheetManager.Instance.DoCloseCurrentSheet;
      end;
      TDockEdge.Right: begin
        var LProgress: Single := abs(ViewPortPosition.X) / Max(1, abs(FSheet.ScrollEngine.MaxScrollLimit.X));
        FSheet.Fill.Color := ALSetColorAlpha(FSheet.Fill.Color, FSheet.FFillAlphaAtPeek * LProgress);
        If compareValue(FSheet.Container.Position.X, FSheet.Width - 1{*}, TEpsilon.Position) >= 0 then
          TALSheetManager.Instance.DoCloseCurrentSheet;
      end;
      TDockEdge.Top: begin
        var LProgress: Single := abs(ViewPortPosition.Y) / Max(1, abs(FSheet.ScrollEngine.MinScrollLimit.Y));
        FSheet.Fill.Color := ALSetColorAlpha(FSheet.Fill.Color, FSheet.FFillAlphaAtPeek * LProgress);
        If compareValue(FSheet.Container.Position.Y, -FSheet.Container.Height + 1{*}, TEpsilon.Position) <= 0 then
          TALSheetManager.Instance.DoCloseCurrentSheet;
      end;
      TDockEdge.Bottom: begin
        var LProgress: Single := abs(ViewPortPosition.Y) / Max(1, abs(FSheet.ScrollEngine.MaxScrollLimit.Y));
        FSheet.Fill.Color := ALSetColorAlpha(FSheet.Fill.Color, FSheet.FFillAlphaAtPeek * LProgress);
        If compareValue(FSheet.Container.Position.Y, FSheet.Height - 1{*}, TEpsilon.Position) >= 0 then
          TALSheetManager.Instance.DoCloseCurrentSheet;
      end;
      else
        Raise Exception.Create('Error B8BEA9FD-8B14-4DAC-9285-76D0025D05E0')
    end;

  end;
  inherited DoChanged;

end;

{********************************************************************************}
constructor TALSheet.Create(const AOwner: TComponent; const ADockEdge: TDockEdge);
begin
  inherited Create(AOwner);
  //--
  FTouchBlocker := nil;
  FDisableScrollRealign := False;
  //--
  FDockEdge := ADockEdge;
  FScrollEngine := CreateScrollEngine;
  fMouseDownPos := TpointF.Zero;
  FHandleMouseEvents := False;
  fScrollCapturedByMe := False;
  FFillAlphaAtPeek := 0;
  //--
  FContainer := TALRectangle.Create(Self);
  FContainer.Parent := Self;
  case FDockEdge of
    TDockEdge.Left: FContainer.Assign(TALSheetManager.Instance.DefaultLeftSheetContainer);
    TDockEdge.Right: FContainer.Assign(TALSheetManager.Instance.DefaultRightSheetContainer);
    TDockEdge.Top: FContainer.Assign(TALSheetManager.Instance.DefaultTopSheetContainer);
    TDockEdge.Bottom: FContainer.Assign(TALSheetManager.Instance.DefaultBottomSheetContainer);
    else Raise Exception.Create('Error 1224CCA1-75C7-4E5F-8F4C-48F0DE5BAA05')
  end;
  FContainer.Name := 'ALSheetContainer';
  //--
  FContent := nil;
  //--
  FTouchBlocker := TALLayout.Create(Self);
  FTouchBlocker.Parent := Self;
  FTouchBlocker.Align := TALAlignLayout.Contents;
  FTouchBlocker.HitTest := True;
  //--
  FShowAnimateOptions := [TAnimateOption.AnimateScrim, TAnimateOption.AnimateContainer];
  FCloseAnimateOptions := [TAnimateOption.AnimateScrim, TAnimateOption.AnimateContainer];
  FOnActionRefProc := nil;
  FOnActionObjProc := nil;
  FOnViewportPositionChange := nil;
  FOnClosedRefProc := nil;
  //--
  case FDockEdge of
    TDockEdge.Left, TDockEdge.Right: begin
      ScrollEngine.TouchTracking := [ttHorizontal];
    end;
    TDockEdge.Top, TDockEdge.Bottom: begin
      ScrollEngine.TouchTracking := [ttVertical];
    end;
    else
      Raise Exception.Create('Error B7C720DA-D07C-4036-A24B-5CF13B946F53')
  end;
  //--
  Assign(TALSheetManager.Instance.DefaultScrim);
  Name := 'ALSheetScrim';
  Onclick := ActionButtonClick;
  TMessageManager.DefaultManager.SubscribeToMessage(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
end;

{**************************}
destructor TALSheet.Destroy;
begin
  ALFreeAndNil(FScrollEngine);
  inherited;
end;

{***********************************}
procedure TALSheet.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  // Unsubscribe from TALScrollCapturedMessage to stop receiving messages.
  // This must be done in BeforeDestruction rather than in Destroy,
  // because the control might be freed in the background via ALFreeAndNil(..., delayed),
  // and BeforeDestruction is guaranteed to execute on the main thread.
  TMessageManager.DefaultManager.Unsubscribe(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  FScrollEngine.Stop(True{AAbruptly});
  inherited;
end;

{****************************************}
class function TALSheet.Current: TALSheet;
begin
  if not TALSheetManager.HasInstance then exit(nil);
  Result := TALSheetManager.Instance.CurrentSheet;
end;

{***********************************************************************************************************************************************************************************}
class procedure TALSheet.CloseCurrent(const AOnClosedCallback: TProc = nil; const AAnimateOptions: TAnimateOptions = [TAnimateOption.AnimateScrim, TAnimateOption.AnimateContainer]);
begin
  if not TALSheetManager.HasInstance then exit;
  var LCurrentSheet := TALSheetManager.Instance.CurrentSheet;
  if LCurrentSheet <> nil then begin
    LCurrentSheet.OnClosedRefProc := AOnClosedCallback;
    LCurrentSheet.CloseAnimateOptions := AAnimateOptions;
    TALSheetManager.Instance.CloseCurrentSheet;
  end;
end;

{*****************************************}
function TALSheet.IsTransitioning: Boolean;
begin
  Result := (TALSheetManager.Instance.CurrentSheet = Self) and
            (TALSheetManager.Instance.FContainerAnimation.Running or TALSheetManager.Instance.FScrimAnimation.Running);
end;

{**************************************************}
function TALSheet.CreateScrollEngine: TScrollEngine;
begin
  Result := TScrollEngine.Create(Self);
end;

{*************************************************}
function TALSheet.GetScrollEngine: TALScrollEngine;
begin
  result := FScrollEngine;
end;

{***************************************************************}
procedure TALSheet.SetScrollEngine(const Value: TALScrollEngine);
begin
  FScrollEngine.Assign(Value);
end;

{**********************************************}
function TALSheet.GetCloseOnScrimClick: boolean;
begin
  result := Assigned(Onclick);
end;

{*************************************************************}
procedure TALSheet.SetCloseOnScrimClick(const AValue: Boolean);
begin
  if AValue then Onclick := ActionButtonClick
  else Onclick := nil;
end;

{******************************************************}
procedure TALSheet.SetContent(const AValue: TALControl);
begin
  if FContent <> AValue then begin
    if FContent <> nil then
      raise Exception.Create('The content has already been set and cannot be changed');
    Fcontent := AValue;
    if FContent.Owner <> Container then
      Container.InsertComponent(FContent);
    FContent.parent := Container;
    case FDockEdge of
      TDockEdge.Left: FContent.Align := TALAlignLayout.Right;
      TDockEdge.Right: FContent.Align := TALAlignLayout.Left;
      TDockEdge.Top: FContent.Align := TALAlignLayout.Bottom;
      TDockEdge.Bottom: FContent.Align := TALAlignLayout.Top;
      else Raise Exception.Create('Error DDE223EF-CCB7-41DD-BC36-EE052F440537')
    end;
  end;
end;

{****************************************************}
procedure TALSheet.ActionButtonClick(Sender: TObject);
begin
  var LCanClose: Boolean := True;
  if Assigned(FOnActionRefProc) then FOnActionRefProc(self, TALControl(Sender).Tag, LCanClose)
  else if Assigned(FOnActionObjProc) then FOnActionObjProc(self, TALControl(Sender).Tag, LCanClose)
  else LCanClose := True;
  If LCanClose then begin
    // One of the callbacks (FOnActionRefProc or FOnActionObjProc) may have asked
    // the Sheet manager to present another Sheet, which could already have
    // closed this one. Confirm that this Sheet is still the active one before
    // calling CloseCurrentSheet.
    if (TALSheetManager.HasInstance) and
       (TALSheetManager.Instance.CurrentSheet = self) then
      TALSheetManager.Instance.CloseCurrentSheet;
  end;
end;

{***************************}
procedure TALSheet.DoRealign;
begin
  Inherited;
  if not FDisableScrollRealign then begin
    case FDockEdge of
      TDockEdge.Left: begin
        ScrollEngine.SetScrollLimits(
          TALPointD.Create(Max(-Width, -Container.Width - Container.Margins.Left), 0), // const MinValue: TalPointD;
          TALPointD.Create(0, 0), // const MaxValue: TalPointD;
          True); // const EnforceLimits: Boolean = True
        if not SameValue(Scrollengine.ViewportPosition.X, ScrollEngine.MinScrollLimit.X, TEpsilon.Position) then
          ScrollEngine.SetViewportPosition(
            TALPointD.Create(
              ScrollEngine.MinScrollLimit.X,
              Scrollengine.ViewportPosition.Y))
        else
          TScrollEngine(ScrollEngine).DoChanged;
      end;
      TDockEdge.Right: begin
        ScrollEngine.SetScrollLimits(
          TALPointD.Create(0, 0), // const MinValue: TalPointD;
          TALPointD.Create(Min(Width, Container.Width + Container.Margins.Right), 0), // const MaxValue: TalPointD;
          True); // const EnforceLimits: Boolean = True
        if not SameValue(Scrollengine.ViewportPosition.X, ScrollEngine.MaxScrollLimit.X, TEpsilon.Position) then
          ScrollEngine.SetViewportPosition(
            TALPointD.Create(
              ScrollEngine.MaxScrollLimit.X,
              Scrollengine.ViewportPosition.Y))
        else
          TScrollEngine(ScrollEngine).DoChanged;
      end;
      TDockEdge.Top: begin
        ScrollEngine.SetScrollLimits(
          TALPointD.Create(0, Max(-Height, -Container.Height - Container.Margins.Top)), // const MinValue: TalPointD;
          TALPointD.Create(0, 0), // const MaxValue: TalPointD;
          True); // const EnforceLimits: Boolean = True
        if not SameValue(Scrollengine.ViewportPosition.Y, ScrollEngine.MinScrollLimit.Y, TEpsilon.Position) then
          ScrollEngine.SetViewportPosition(
            TALPointD.Create(
              Scrollengine.ViewportPosition.X,
              ScrollEngine.MinScrollLimit.Y))
        else
          TScrollEngine(ScrollEngine).DoChanged;
      end;
      TDockEdge.Bottom: begin
        ScrollEngine.SetScrollLimits(
          TALPointD.Create(0, 0), // const MinValue: TalPointD;
          TALPointD.Create(0, Min(Height, Container.Height + Container.Margins.Bottom)), // const MaxValue: TalPointD;
          True); // const EnforceLimits: Boolean = True
        if not SameValue(Scrollengine.ViewportPosition.Y, ScrollEngine.MaxScrollLimit.Y, TEpsilon.Position) then
          ScrollEngine.SetViewportPosition(
            TALPointD.Create(
              Scrollengine.ViewportPosition.X,
              ScrollEngine.MaxScrollLimit.Y))
        else
          TScrollEngine(ScrollEngine).DoChanged;
      end;
      else
        Raise Exception.Create('Error 9D640DF3-2F40-49A2-BF4D-F91909EC21DF')
    end;
  end;
end;

{****************************************************************************************}
procedure TALSheet.ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  if (Sender = self) then exit;
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.ScrollCapturedByOtherHandler',
  //  'Captured: ' + ALBoolToStrW(TALScrollCapturedMessage(M).Captured)+ ' | ' +
  //  'ScrollEngine.down: ' + ALBoolToStrW(fScrollEngine.down));
  {$ENDIF}
  if TALScrollCapturedMessage(M).Captured then begin
    {$IFDEF DEBUG}
    if fScrollCapturedByMe then
      raise Exception.Create('Error 9A590A21-139E-44EC-83F5-6813453329E4');
    {$ENDIF}
    if fScrollEngine.down then begin
      fScrollEngine.Down := false;
      FHandleMouseEvents := False;
    end;
  end;
end;

{*******************************************************************************************}
procedure TALSheet.internalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.MouseDown',
  //  'Position:' + ALFormatFloatW('0.##', x) + ',' + ALFormatFloatW('0.##', y));
  {$ENDIF}
  if (Button = TMouseButton.mbLeft) then begin
    FHandleMouseEvents := true;
    fMouseDownPos := TPointF.Create(X,Y);
    {$IF defined(ANDROID) or defined(IOS)}
    if form <> nil then
      ScrollEngine.MouseDown(form.Handle);
    {$ELSE}
    ScrollEngine.MouseDown(X, Y);
    {$ENDIF}
  end;
end;

{*********************************************************************}
procedure TALSheet.internalMouseMove(Shift: TShiftState; X, Y: Single);
begin
  //{$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.internalMouseMove',
  //  'Position:' + ALFormatFloatW('0.##', x) + ',' + ALFormatFloatW('0.##', y));
  //{$ENDIF}
  if FHandleMouseEvents then begin
    if (not fScrollCapturedByMe) and
       (fScrollEngine.TouchEnabled) and
       (((ttHorizontal in fScrollEngine.TouchTracking) and
         (abs(fMouseDownPos.x - x) > abs(fMouseDownPos.y - y)) and
         (abs(fMouseDownPos.x - x) > TALScrollEngine.DefaultTouchSlop) and
         ((ScrollEngine.MinEdgeDragResistanceFactor <> 0) or
          (not SameValue(ScrollEngine.ViewportPosition.X, ScrollEngine.MinScrollLimit.X, TEpsilon.position)) or
          (fMouseDownPos.X - X > 0)) and
         ((ScrollEngine.MaxEdgeDragResistanceFactor <> 0) or
          (not SameValue(ScrollEngine.ViewportPosition.X, ScrollEngine.MaxScrollLimit.X, TEpsilon.position)) or
          (fMouseDownPos.X - X < 0))) or
        ((ttVertical in fScrollEngine.TouchTracking) and
         (abs(fMouseDownPos.y - y) > abs(fMouseDownPos.x - x)) and
         (abs(fMouseDownPos.y - y) > TALScrollEngine.DefaultTouchSlop) and
         ((ScrollEngine.MinEdgeDragResistanceFactor <> 0) or
          (not SameValue(ScrollEngine.ViewportPosition.Y, ScrollEngine.MinScrollLimit.Y, TEpsilon.position)) or
          (fMouseDownPos.Y - Y > 0)) and
         ((ScrollEngine.MaxEdgeDragResistanceFactor <> 0) or
          (not SameValue(ScrollEngine.ViewportPosition.Y, ScrollEngine.MaxScrollLimit.Y, TEpsilon.position)) or
          (fMouseDownPos.Y - Y < 0)))) then begin
      {$IFDEF DEBUG}
      //ALLog(
      //  ClassName + '.internalMouseMove',
      //  'ScrollCapturedByMe');
      {$ENDIF}
      fScrollCapturedByMe := True;
      TMessageManager.DefaultManager.SendMessage(self, TALScrollCapturedMessage.Create(true));
    end;
    {$IF defined(ANDROID) or defined(IOS)}
    if form <> nil then
      ScrollEngine.MouseMove(form.Handle);
    {$ELSE}
    ScrollEngine.MouseMove(X, Y);
    {$ENDIF}
  end;
end;

{*****************************************************************************************}
procedure TALSheet.internalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.internalMouseUp',
  //  'Position:' + ALFormatFloatW('0.##', x) + ',' + ALFormatFloatW('0.##', y));
  {$ENDIF}
  if FHandleMouseEvents and (Button = TMouseButton.mbLeft) then begin
    {$IF defined(ANDROID) or defined(IOS)}
    if form <> nil then
      ScrollEngine.MouseUp(form.Handle);
    {$ELSE}
    ScrollEngine.MouseUp(X, Y);
    {$ENDIF}
    FScrollCapturedByMe := False;
    FHandleMouseEvents := False;
  end;
end;

{************************************}
procedure TALSheet.internalMouseLeave;
begin
  {$IFDEF DEBUG}
  //ALLog(ClassName + '.internalMouseLeave');
  {$ENDIF}
  if FHandleMouseEvents then begin
    ScrollEngine.MouseLeave;
    FScrollCapturedByMe := False;
    FHandleMouseEvents := False;
  end;
end;

{**}
Type
  _TControlProtectedAccess = class(Tcontrol);

{********************************************************************************************************************}
procedure TALSheet.ChildrenMouseDown(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (FTouchBlocker.Visible) and (not FTouchBlocker.HitTest) then begin
    TALSheetManager.Instance.FScrimAnimation.StopAtCurrent;
    TALSheetManager.Instance.FContainerAnimation.StopAtCurrent;
  end;
  if not FTouchBlocker.Visible then begin
    if not aObject.AutoCapture then begin
      {$IF defined(MSWindows)}
      // On Windows, calling doCapture will invoke Winapi.Windows.SetCapture(FormToHWND(AForm));
      // This action deactivates some functionalities in the native control, such as the right-click menu.
      if not Supports(aObject, IALNativeControl) then
      {$ENDIF}
        _TControlProtectedAccess(aObject).capture;
    end;
    var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
    InternalMouseDown(Button, Shift, P.X, P.Y);
  end;
  inherited;
end;

{**********************************************************************************************}
procedure TALSheet.ChildrenMouseMove(const AObject: TControl; Shift: TShiftState; X, Y: Single);
begin
  if not FTouchBlocker.Visible then begin
    var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
    internalMouseMove(Shift, P.X, P.Y);
  end;
  inherited;
end;

{******************************************************************************************************************}
procedure TALSheet.ChildrenMouseUp(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not FTouchBlocker.Visible then begin
    if not aObject.AutoCapture then begin
      {$IF defined(MSWindows)}
      // On Windows, calling doCapture will invoke Winapi.Windows.SetCapture(FormToHWND(AForm));
      // This action deactivates some functionalities in the native control, such as the right-click menu.
      if not Supports(aObject, IALNativeControl) then
      {$ENDIF}
        _TControlProtectedAccess(aObject).releasecapture;
    end;
    var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
    InternalMouseUp(Button, Shift, P.X, P.Y);
  end;
  inherited;
end;

{*************************************************************}
procedure TALSheet.ChildrenMouseLeave(const AObject: TControl);
begin
  if not FTouchBlocker.Visible then
    internalMouseLeave;
  inherited;
end;

{****************************}
procedure TALSheet.AfterPaint;
begin
  Inherited;
  // During the Material3ExpressiveDefaultSpatial animation,
  // the container's "bounds" effect can leave visible gaps (holes) on the sides.
  // This code fills those gaps with the container's fill color to ensure
  // a seamless appearance.
  if (Container.AbsoluteOpacity = 1) and
     (TAlphaColorRec(Container.Fill.Color).A = $FF) then begin
    case DockEdge of
      TALSheet.TDockEdge.Left: begin
        if SameValue(Container.Margins.Left, 0, TEpsilon.Position) then begin
          var LRect := Container.BoundsRect;
          LRect.Offset(0, -LRect.Width);
          LRect.Left := -65535;
          var LLocalRect := LocalRect;
          LLocalRect.Left := LLocalRect.Left - margins.Left;
          LRect.Intersect(LLocalRect);
          if CompareValue(LRect.Width, 0, TEpsilon.Position) >= 0 then begin
            LRect := Canvas.AlignToPixel(LRect);
            Canvas.Fill.Kind := TBrushKind.Solid;
            Canvas.Fill.Color := Container.Fill.Color;
            Canvas.FillRect(LRect, 1{Opacity});
          end;
        end;
      end;
      TALSheet.TDockEdge.Right: begin
        if SameValue(Container.Margins.Right, 0, TEpsilon.Position) then begin
          var LRect := Container.BoundsRect;
          LRect.Offset(0, LRect.Width);
          LRect.Right := 65535;
          var LLocalRect := LocalRect;
          LLocalRect.Right := LLocalRect.Right + margins.Right;
          LRect.Intersect(LLocalRect);
          if CompareValue(LRect.Width, 0, TEpsilon.Position) >= 0 then begin
            LRect := Canvas.AlignToPixel(LRect);
            Canvas.Fill.Kind := TBrushKind.Solid;
            Canvas.Fill.Color := Container.Fill.Color;
            Canvas.FillRect(LRect, 1{Opacity});
          end;
        end;
      end;
      TALSheet.TDockEdge.Top: begin
        if SameValue(Container.Margins.Top, 0, TEpsilon.Position) then begin
          var LRect := Container.BoundsRect;
          LRect.Offset(0, -LRect.Height);
          LRect.Top := -65535;
          var LLocalRect := LocalRect;
          LLocalRect.Top := LLocalRect.Top - margins.Top;
          LRect.Intersect(LLocalRect);
          if CompareValue(LRect.Height, 0, TEpsilon.Position) >= 0 then begin
            LRect := Canvas.AlignToPixel(LRect);
            Canvas.Fill.Kind := TBrushKind.Solid;
            Canvas.Fill.Color := Container.Fill.Color;
            Canvas.FillRect(LRect, 1{Opacity});
          end;
        end;
      end;
      TALSheet.TDockEdge.Bottom: begin
        if SameValue(Container.Margins.Bottom, 0, TEpsilon.Position) then begin
          var LRect := Container.BoundsRect;
          LRect.Offset(0, LRect.Height);
          LRect.Bottom := 65535;
          var LLocalRect := LocalRect;
          LLocalRect.Bottom := LLocalRect.Bottom + margins.Bottom;
          LRect.Intersect(LLocalRect);
          if CompareValue(LRect.Height, 0, TEpsilon.Position) >= 0 then begin
            LRect := Canvas.AlignToPixel(LRect);
            Canvas.Fill.Kind := TBrushKind.Solid;
            Canvas.Fill.Color := Container.Fill.Color;
            Canvas.FillRect(LRect, 1{Opacity});
          end;
        end;
      end;
      else
        raise Exception.Create('Error 1512C27F-9E65-4694-9822-E7BA30A96EC6');
    end;
  end;
end;

{*************************************************************************************************}
constructor TALVerticalSheet.Create(const AOwner: TComponent; const ADockEdge: TALSheet.TDockEdge);
begin
  inherited;
  FDragHandle := TALRectangle.Create(Container);
  FDragHandle.Parent := Container;
  case FDockEdge of
    TDockEdge.Top: FDragHandle.Assign(TALSheetManager.Instance.DefaultTopSheetDragHandle);
    TDockEdge.Bottom: FDragHandle.Assign(TALSheetManager.Instance.DefaultBottomSheetDragHandle);
    else Raise Exception.Create('Error C8222812-AFAB-4483-ACF0-8B35D088F603')
  end;
  FDragHandle.Name := 'ALSheetDragHandle';
end;

{**************************************************}
function TALTopSheet.TBuilder.CreateSheet: TALSheet;
begin
  Result := TALTopSheet.Create(nil);
end;

{**************************************************}
function TALTopSheet.TBuilder.GetSheet: TALTopSheet;
begin
  Result := TALTopSheet(inherited Sheet);
end;

{*******************************************************}
constructor TALTopSheet.Create(const AOwner: TComponent);
begin
  inherited create(AOwner, TALSheet.TDockEdge.Top);
end;

{*******************************************}
class function TALTopSheet.Builder: TBuilder;
begin
  Result := TALTopSheet.TBuilder.Create;
end;

{*****************************************************}
function TALBottomSheet.TBuilder.CreateSheet: TALSheet;
begin
  Result := TALBottomSheet.Create(nil);
end;

{********************************************************}
function TALBottomSheet.TBuilder.GetSheet: TALBottomSheet;
begin
  Result := TALBottomSheet(inherited Sheet);
end;

{**********************************************************}
constructor TALBottomSheet.Create(const AOwner: TComponent);
begin
  inherited create(AOwner, TALSheet.TDockEdge.Bottom);
  FVirtualKeyboardAnimation := nil;
  TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, VirtualKeyboardChangeHandler);
end;

{********************************}
destructor TALBottomSheet.Destroy;
begin
  ALFreeAndNil(FVirtualKeyboardAnimation);
  inherited;
end;

{*****************************************}
procedure TALBottomSheet.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  if FVirtualKeyboardAnimation <> nil then FVirtualKeyboardAnimation.Enabled := False;
  // Unsubscribe from TVKStateChangeMessage to stop receiving messages.
  // This must be done in BeforeDestruction rather than in Destroy,
  // because the control might be freed in the background via ALFreeAndNil(..., delayed),
  // and BeforeDestruction is guaranteed to execute on the main thread.
  TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, VirtualKeyboardChangeHandler);
  inherited;
end;

{*****************************************************************}
function TALBottomSheet.IsVirtualKeyboardAnimationRunning: Boolean;
begin
  Result := (FVirtualKeyboardAnimation <> nil) and (FVirtualKeyboardAnimation.Running);
end;

{**********************************************}
class function TALBottomSheet.Builder: TBuilder;
begin
  Result := TALBottomSheet.TBuilder.Create;
end;

{*****************************************************************************************************************}
procedure TALBottomSheet.VirtualKeyboardChangeHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  if FVirtualKeyboardAnimation = nil then begin
    FVirtualKeyboardAnimation := TALFloatAnimation.Create;
    FVirtualKeyboardAnimation.OnProcess := VirtualKeyboardAnimationProcess;
  end;
  FVirtualKeyboardAnimation.Enabled := False;
  if TVKStateChangeMessage(Msg).KeyboardVisible then begin
    {$IF defined(ANDROID)}
    FVirtualKeyboardAnimation.Duration := 0.300;
    {$ELSE}
    FVirtualKeyboardAnimation.Duration := 0.500;
    {$ENDIF}
    FVirtualKeyboardAnimation.InterpolationType := TALInterpolationType.Material3StandardDefaultEffects;
    FVirtualKeyboardAnimation.StartValue := margins.Bottom;
    FVirtualKeyboardAnimation.StopValue := TVKStateChangeMessage(Msg).KeyboardBounds.Height;
  end
  else begin
    FVirtualKeyboardAnimation.Duration := 0.200;
    FVirtualKeyboardAnimation.InterpolationType := TALInterpolationType.Material3StandardDefaultEffects;
    FVirtualKeyboardAnimation.StartValue := margins.Bottom;
    FVirtualKeyboardAnimation.StopValue := 0;
  end;
  FVirtualKeyboardAnimation.Start;
end;

{************************************************************************}
procedure TALBottomSheet.VirtualKeyboardAnimationProcess(Sender: TObject);
begin
  margins.Bottom := FVirtualKeyboardAnimation.CurrentValue;
end;

{***********************************************************************************}
function TALHorizontalSheet.TBuilder.SetHeadlineText(const AValue: String): TBuilder;
begin
  TALHorizontalSheet(Sheet).Headline.Text := AValue;
  Result := Self;
end;

{**********************************************************************************}
function TALHorizontalSheet.TBuilder.AddBackButton(const ATag: NativeInt): TBuilder;
begin
  TALHorizontalSheet(Sheet).AddBackButton(ATag);
  Result := Self;
end;

{***********************************************************************************}
function TALHorizontalSheet.TBuilder.AddCloseButton(const ATag: NativeInt): TBuilder;
begin
  TALHorizontalSheet(Sheet).AddCloseButton(ATag);
  Result := Self;
end;

{***************************************************************************************************}
constructor TALHorizontalSheet.Create(const AOwner: TComponent; const ADockEdge: TALSheet.TDockEdge);
begin
  inherited;
  FHeaderBar := nil;
  FBackButton := nil;
  FCloseButton := nil;
  FHeadline := nil;
end;

{*****************************************************}
function TALHorizontalSheet.GetHeaderBar: TALRectangle;
begin
  If FHeaderBar = nil then begin
    FHeaderBar := TALRectangle.Create(Container);
    FHeaderBar.Parent := Container;
    case DockEdge of
      TDockEdge.Left: FHeaderBar.Assign(TALSheetManager.Instance.DefaultLeftSheetHeaderBar);
      TDockEdge.Right: FHeaderBar.Assign(TALSheetManager.Instance.DefaultRightSheetHeaderBar);
      else
        Raise Exception.Create('Error 8C2BD5F1-4235-4E40-A43F-38A00CB2F03A')
    end;
    FHeaderBar.Name := 'ALSheetHeaderBar';
  end;
  Result := FHeaderBar;
end;

{***************************************************}
function TALHorizontalSheet.GetBackButton: TALButton;
begin
  If FBackButton = nil then begin
    FBackButton := TALButton.Create(HeaderBar);
    FBackButton.Parent := HeaderBar;
    FBackButton.Name := 'ALSheetBackButton';
    case FDockEdge of
      TDockEdge.Left: FBackButton.Assign(TALSheetManager.Instance.DefaultLeftSheetBackButton);
      TDockEdge.Right: FBackButton.Assign(TALSheetManager.Instance.DefaultRightSheetBackButton);
      else Raise Exception.Create('Error 21FC306A-7C69-463E-A7E1-1A342B3FBBE1')
    end;
    FBackButton.Text := '';
    FBackButton.OnClick := ActionButtonClick;
  end;
  Result := FBackButton;
end;

{****************************************************}
function TALHorizontalSheet.GetCloseButton: TALButton;
begin
  If FCloseButton = nil then begin
    FCloseButton := TALButton.Create(HeaderBar);
    FCloseButton.Parent := HeaderBar;
    FCloseButton.Name := 'ALSheetCloseButton';
    case FDockEdge of
      TDockEdge.Left: FCloseButton.Assign(TALSheetManager.Instance.DefaultLeftSheetCloseButton);
      TDockEdge.Right: FCloseButton.Assign(TALSheetManager.Instance.DefaultRightSheetCloseButton);
      else Raise Exception.Create('Error E3F5417D-E9E9-49C4-B53F-D677F98AF880')
    end;
    FCloseButton.Text := '';
    FCloseButton.OnClick := ActionButtonClick;
  end;
  Result := FCloseButton;
end;

{***********************************************}
function TALHorizontalSheet.GetHeadline: TALText;
begin
  If FHeadline = nil then begin
    FHeadline := TALText.Create(HeaderBar);
    FHeadline.Parent := HeaderBar;
    FHeadline.Name := 'ALSheetHeadline';
    case FDockEdge of
      TDockEdge.Left: FHeadline.Assign(TALSheetManager.Instance.DefaultLeftSheetHeadline);
      TDockEdge.Right: FHeadline.Assign(TALSheetManager.Instance.DefaultRightSheetHeadline);
      else Raise Exception.Create('Error 8F32B838-A12C-44B2-85E9-5DB1A70002B5')
    end;
  end;
  Result := FHeadline;
end;

{***********************************************}
function TALHorizontalSheet.HasHeadline: Boolean;
begin
  result := FHeadline <> nil;
end;

{*************************************************}
function TALHorizontalSheet.HasBackButton: Boolean;
begin
  result := FBackButton <> nil;
end;

{**************************************************}
function TALHorizontalSheet.HasCloseButton: Boolean;
begin
  result := FCloseButton <> nil;
end;

{************************************************}
function TALHorizontalSheet.HasHeaderBar: Boolean;
begin
  result := FHeaderBar <> nil;
end;

{****************************************************************}
procedure TALHorizontalSheet.AddBackButton(const ATag: NativeInt);
begin
  if HasBackButton then raise Exception.Create('A sheet can only contain one back button');
  BackButton.Tag := ATag;
end;

{*****************************************************************}
procedure TALHorizontalSheet.AddCloseButton(const ATag: NativeInt);
begin
  if HasCloseButton then raise Exception.Create('A sheet can only contain one close button');
  CloseButton.Tag := ATag;
end;

{***************************************************}
function TALLeftSheet.TBuilder.CreateSheet: TALSheet;
begin
  Result := TALHorizontalSheet.Create(nil, TALSheet.TDockEdge.Left);
end;

{****************************************************}
function TALLeftSheet.TBuilder.GetSheet: TALLeftSheet;
begin
  Result := TALLeftSheet(inherited Sheet);
end;

{********************************************************}
constructor TALLeftSheet.Create(const AOwner: TComponent);
begin
  inherited create(AOwner, TALSheet.TDockEdge.Left);
end;

{********************************************}
class function TALLeftSheet.Builder: TBuilder;
begin
  Result := TBuilder.Create;
end;

{****************************************************}
function TALRightSheet.TBuilder.CreateSheet: TALSheet;
begin
  Result := TALHorizontalSheet.Create(nil, TALSheet.TDockEdge.Right);
end;

{******************************************************}
function TALRightSheet.TBuilder.GetSheet: TALRightSheet;
begin
  Result := TALRightSheet(inherited Sheet);
end;

{*********************************************************}
constructor TALRightSheet.Create(const AOwner: TComponent);
begin
  inherited create(AOwner, TALSheet.TDockEdge.Right);
end;

{*********************************************}
class function TALRightSheet.Builder: TBuilder;
begin
  Result := TBuilder.Create;
end;

{*********************************}
constructor TALSheetManager.Create;
begin
  inherited;
  FDefaultScrim := TALRectangle.Create(nil);
  FDefaultLeftSheetContainer := TALRectangle.Create(nil);
  FDefaultRightSheetContainer := TALRectangle.Create(nil);
  FDefaultTopSheetContainer := TALRectangle.Create(nil);
  FDefaultBottomSheetContainer := TALRectangle.Create(nil);
  FDefaultTopSheetDragHandle := TALRectangle.Create(nil);
  FDefaultBottomSheetDragHandle := TALRectangle.Create(nil);
  FDefaultLeftSheetHeaderBar := TALRectangle.Create(nil);
  FDefaultRightSheetHeaderBar := TALRectangle.Create(nil);
  FDefaultLeftSheetBackButton := TALButton.Create(nil);
  FDefaultRightSheetBackButton := TALButton.Create(nil);
  FDefaultLeftSheetHeadline := TALText.Create(nil);
  FDefaultRightSheetHeadline := TALText.Create(nil);
  FDefaultLeftSheetCloseButton := TALButton.Create(nil);
  FDefaultRightSheetCloseButton := TALButton.Create(nil);
  FCurrentSheet := Nil;
  FScrimAnimation := TALFloatAnimation.Create;
  FScrimAnimation.OnProcess := ScrimAnimationProcess;
  FScrimAnimation.OnFinish := ScrimAnimationFinish;
  FContainerAnimation := TALFloatAnimation.Create;
  FContainerAnimation.OnProcess := ContainerAnimationProcess;
  FContainerAnimation.OnFinish := ContainerAnimationFinish;
  FQueue := TQueue<TALSheet>.create;
  Setlength(FFrozenNativeControls, 0);
end;

{*********************************}
destructor TALSheetManager.Destroy;
begin
  AlFreeAndNil(FDefaultScrim);
  AlFreeAndNil(FDefaultLeftSheetContainer);
  AlFreeAndNil(FDefaultRightSheetContainer);
  AlFreeAndNil(FDefaultTopSheetContainer);
  AlFreeAndNil(FDefaultBottomSheetContainer);
  AlFreeAndNil(FDefaultTopSheetDragHandle);
  AlFreeAndNil(FDefaultBottomSheetDragHandle);
  AlFreeAndNil(FDefaultLeftSheetHeaderBar);
  AlFreeAndNil(FDefaultRightSheetHeaderBar);
  AlFreeAndNil(FDefaultLeftSheetBackButton);
  AlFreeAndNil(FDefaultRightSheetBackButton);
  AlFreeAndNil(FDefaultLeftSheetHeadline);
  AlFreeAndNil(FDefaultRightSheetHeadline);
  AlFreeAndNil(FDefaultLeftSheetCloseButton);
  AlFreeAndNil(FDefaultRightSheetCloseButton);
  ALFreeAndNil(FScrimAnimation);
  ALFreeAndNil(FContainerAnimation);
  ALFreeAndNil(FQueue);
  inherited;
end;

{******************************************}
procedure TALSheetManager.AfterConstruction;
begin
  inherited;
  TALStyleManager.Instance.ApplySheetManagerStyle('Default', Self);
end;

{*************************************************************}
class function TALSheetManager.CreateInstance: TALSheetManager;
begin
  result := TALSheetManager.Create;
end;

{*************}
//[MultiThread]
class function TALSheetManager.GetInstance: TALSheetManager;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALSheetManager.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{******************************************}
procedure TALSheetManager.FreezeNativeViews;
begin
  For var I := Low(FFrozenNativeControls) to high(FFrozenNativeControls) do
    if FFrozenNativeControls[I] <> nil then
      FFrozenNativeControls[I].RemoveFreeNotify(Self);

  ALFreezeNativeViews(FFrozenNativeControls);

  For var I := Low(FFrozenNativeControls) to high(FFrozenNativeControls) do
    if FFrozenNativeControls[I] <> nil then
      FFrozenNativeControls[I].AddFreeNotify(Self);
end;

{********************************************}
procedure TALSheetManager.UnfreezeNativeViews;
begin
  For var I := Low(FFrozenNativeControls) to high(FFrozenNativeControls) do
    if FFrozenNativeControls[I] <> nil then
      FFrozenNativeControls[I].RemoveFreeNotify(Self);

  ALUnfreezeNativeViews(FFrozenNativeControls)
end;

{***********************************************************}
procedure TALSheetManager.FreeNotification(AObject: TObject);
begin
  For var I := low(FFrozenNativeControls) to high(FFrozenNativeControls) do
    if FFrozenNativeControls[I] = AObject then
      FFrozenNativeControls[I] := nil;

  if FCurrentSheet = AObject then begin
    FScrimAnimation.Enabled := False;
    FContainerAnimation.Enabled := False;
    FCurrentSheet := Nil;
  end;
end;

{***********************************************}
function TALSheetManager.IsShowingSheet: Boolean;
begin
  Result := FCurrentSheet <> nil;
end;

{******************************************}
procedure TALSheetManager.CloseCurrentSheet;
begin
  if FCurrentSheet = nil then exit;

  FCurrentSheet.ScrollEngine.Stop(true{AAbruptly});

  If (not (TALSheet.TAnimateOption.AnimateContainer in FCurrentSheet.CloseAnimateOptions)) then begin
    DoCloseCurrentSheet;
    exit;
  end;

  FCurrentSheet.FDisableScrollRealign := True;
  try
    If not TALSheetManager.Instance.HasPendingSheets then begin
      FCurrentSheet.HitTest := False;
      FCurrentSheet.FTouchBlocker.Parent := FCurrentSheet.Container;
    end;
    FCurrentSheet.FTouchBlocker.Visible := True;
    FCurrentSheet.FTouchBlocker.HitTest := True;
  finally
    FCurrentSheet.FDisableScrollRealign := False;
  end;

  FScrimAnimation.Enabled := False;
  if (not HasPendingSheets) and
     (TALSheet.TAnimateOption.AnimateScrim in FCurrentSheet.CloseAnimateOptions) then begin
    FScrimAnimation.TagFloat := TAlphaColorRec(FCurrentSheet.Fill.Color).A / 255;
    FScrimAnimation.InterpolationType := TALInterpolationType.Linear;
    FScrimAnimation.Duration := 0.200;
    FScrimAnimation.StartValue := 1;
    FScrimAnimation.StopValue := 0;
    FScrimAnimation.Start;
  end;

  FContainerAnimation.Enabled := False;
  FContainerAnimation.Tag := Integer(True);
  FContainerAnimation.InterpolationType := TALInterpolationType.Material3EmphasizedAccelerate;
  FContainerAnimation.Duration := 0.200;
  case FCurrentSheet.DockEdge of
    TALSheet.TDockEdge.Left: begin
      FContainerAnimation.StartValue := FCurrentSheet.Container.Position.X;
      FContainerAnimation.StopValue := -FCurrentSheet.Container.Width;
    end;
    TALSheet.TDockEdge.Right: begin
      FContainerAnimation.StartValue := FCurrentSheet.Container.Position.X;
      FContainerAnimation.StopValue := FCurrentSheet.Width;
    end;
    TALSheet.TDockEdge.Top: begin
      FContainerAnimation.StartValue := FCurrentSheet.Container.Position.Y;
      FContainerAnimation.StopValue := -FCurrentSheet.Container.Height;
    end;
    TALSheet.TDockEdge.Bottom: begin
      FContainerAnimation.StartValue := FCurrentSheet.Container.Position.Y;
      FContainerAnimation.StopValue := FCurrentSheet.Height;
    end;
    else
      raise Exception.Create('Error 5068FF88-E3CF-4016-9C97-FBB54AB7F881');
  end;
  FContainerAnimation.Start;
end;

{********************************************}
procedure TALSheetManager.DoCloseCurrentSheet;
begin
  if FCurrentSheet = nil then exit;
  var LCurrentSheet := FCurrentSheet;
  LCurrentSheet.Visible := False;
  FCurrentSheet := nil;
  FScrimAnimation.Enabled := False;
  FContainerAnimation.Enabled := False;
  if assigned(LCurrentSheet.FOnClosedRefProc) then
    LCurrentSheet.FOnClosedRefProc();
  ProcessPendingSheets;
  if not IsShowingSheet then
    UnfreezeNativeViews
  else
    FScrimAnimation.Stop;
  TThread.ForceQueue(nil,
    procedure
    begin
      ALFreeAndNil(LCurrentSheet);
    end);
end;

{*************}
//[MultiThread]
procedure TALSheetManager.RequestSheet(const ASheet: TALSheet);
begin
  TThread.queue(nil,
    procedure
    begin
      if (TALLoadingOverlayManager.HasInstance) and
         (TALLoadingOverlayManager.Instance.IsShowingLoadingOverlay) then
        TALLoadingOverlay.CloseCurrent;

      if (TALDialogManager.HasInstance) and
         (TALDialogManager.Instance.IsShowingDialog) then
        TALDialog.CloseCurrent;

      if TALSheetManager.HasInstance then begin
        with TALSheetManager.Instance do begin
          FQueue.Enqueue(ASheet);
          if IsShowingSheet then
            CloseCurrentSheet
          else
            ProcessPendingSheets;
        end;
      end
      else
        ALFreeAndNil(ASheet);
    end);
end;

{*************************************************}
Function TALSheetManager.HasPendingSheets: Boolean;
begin
  Result := FQueue.Count > 0;
end;

{*********************************************}
procedure TALSheetManager.ProcessPendingSheets;
begin
  if (not IsShowingSheet) and
     (HasPendingSheets) then begin
    var LSheet := FQueue.Dequeue;
    try
      ShowSheet(LSheet);
    Except
      ALFreeAndNil(LSheet);
      Raise;
    end;
  end;
end;

{**********************************************************}
procedure TALSheetManager.ShowSheet(const ASheet: TALSheet);
begin

  // Attach ASheet
  if ASheet.ParentControl = nil then begin
    Var LForm := Screen.ActiveForm;
    if LForm = nil then LForm := Application.MainForm;
    if LForm = nil then Raise Exception.Create('Error 0B1C5551-F59D-46FA-8E9B-A10AB6A65FDE');
    LForm.Focused := nil;
    FreezeNativeViews;
    ASheet.Align := TALAlignLayout.Contents;
    // This will defacto call LForm.Realign and ASheet.EndUpdate
    // in TCustomForm.DoAddObject.SetUpdatingState
    LForm.InsertComponent(ASheet);
    ASheet.Parent := LForm;
  end
  else begin
    ASheet.ParentControl.ResetFocus;
    FreezeNativeViews;
    ASheet.Align := TALAlignLayout.Contents;
    Asheet.EndUpdate;
  end;

  // Init FCurrentSheet
  FCurrentSheet := ASheet;
  FCurrentSheet.AddFreeNotify(Self);

  // ShowAnimateOptions
  if TALSheet.TAnimateOption.AnimateContainer in FCurrentSheet.ShowAnimateOptions then begin

    // Start the ScrimAnimation
    if (TALSheet.TAnimateOption.AnimateScrim in FCurrentSheet.ShowAnimateOptions) then begin
      FScrimAnimation.Enabled := False;
      FScrimAnimation.TagFloat := TAlphaColorRec(FCurrentSheet.Fill.Color).A / 255;
      FScrimAnimation.InterpolationType := TALInterpolationType.Linear;
      FScrimAnimation.Duration := 0.4;
      FScrimAnimation.StartValue := 0;
      FScrimAnimation.StopValue := 1;
      FScrimAnimation.Start;
    end;

    // Start the ContainerAnimation
    FContainerAnimation.Enabled := False;
    FContainerAnimation.Tag := Integer(False);
    FContainerAnimation.InterpolationType := TALInterpolationType.Material3ExpressiveDefaultSpatial;
    FContainerAnimation.Duration := 0.500;
    case FCurrentSheet.DockEdge of
      TALSheet.TDockEdge.Left: begin
        FContainerAnimation.StartValue := -FCurrentSheet.Container.Width;
        FContainerAnimation.StopValue := -FCurrentSheet.Container.Width - FCurrentSheet.ScrollEngine.MinScrollLimit.X;
        FCurrentSheet.Container.Position.X := FContainerAnimation.StartValue;
      end;
      TALSheet.TDockEdge.Right: begin
        FContainerAnimation.StartValue := FCurrentSheet.Width;
        FContainerAnimation.StopValue := FCurrentSheet.Width - FCurrentSheet.ScrollEngine.MaxScrollLimit.X;
        FCurrentSheet.Container.Position.X := FContainerAnimation.StartValue;
      end;
      TALSheet.TDockEdge.Top: begin
        FContainerAnimation.StartValue := -FCurrentSheet.Container.Height;
        FContainerAnimation.StopValue := -FCurrentSheet.Container.Height - FCurrentSheet.ScrollEngine.MinScrollLimit.Y;
        FCurrentSheet.Container.Position.Y := FContainerAnimation.StartValue;
      end;
      TALSheet.TDockEdge.Bottom: begin
        FContainerAnimation.StartValue := FCurrentSheet.Height;
        FContainerAnimation.StopValue := FCurrentSheet.Height - FCurrentSheet.ScrollEngine.MaxScrollLimit.Y;
        FCurrentSheet.Container.Position.Y := FContainerAnimation.StartValue;
      end;
      else
        raise Exception.Create('Error 7D95B3F6-5787-425B-8C51-4471CFEE4C6B');
    end;
    FContainerAnimation.Start;

  end
  else
    ContainerAnimationFinish(nil);

end;

{***************************************************************}
procedure TALSheetManager.ScrimAnimationProcess(Sender: TObject);
begin
  if FCurrentSheet = nil then exit;
  FCurrentSheet.Fill.Color := ALSetColorAlpha(FCurrentSheet.Fill.Color, FScrimAnimation.CurrentValue * FScrimAnimation.TagFloat);
end;

{**************************************************************}
procedure TALSheetManager.ScrimAnimationFinish(Sender: TObject);
begin
  if FCurrentSheet = nil then exit;
  FCurrentSheet.FFillAlphaAtPeek := FScrimAnimation.TagFloat;
end;

{*******************************************************************}
procedure TALSheetManager.ContainerAnimationProcess(Sender: TObject);
begin
  if FCurrentSheet = nil then exit;
  var LSaveDisableAlign := FCurrentSheet.FDisableAlign;
  FCurrentSheet.FDisableAlign := True;
  try
    case FCurrentSheet.DockEdge of
      TALSheet.TDockEdge.Left,
      TALSheet.TDockEdge.Right:
        FCurrentSheet.Container.Position.X := FContainerAnimation.CurrentValue;
      TALSheet.TDockEdge.Top,
      TALSheet.TDockEdge.Bottom:
        FCurrentSheet.Container.Position.Y := FContainerAnimation.CurrentValue;
      else
        raise Exception.Create('Error F4FF9418-D70E-4CD6-BF13-EB79F7819170');
    end;
  finally
    FCurrentSheet.FDisableAlign := LSaveDisableAlign;
  end;
  if (FContainerAnimation.tag <> Integer(True)) and
     (FContainerAnimation.NormalizedTime > 0.90) then
    FCurrentSheet.FTouchBlocker.HitTest := False;
end;

{******************************************************************}
procedure TALSheetManager.ContainerAnimationFinish(Sender: TObject);
begin
  if FCurrentSheet = nil then exit;
  if FContainerAnimation.tag = Integer(True) then DoCloseCurrentSheet
  else begin
    case FCurrentSheet.FDockEdge of
      TALSheet.TDockEdge.Left: begin
        FCurrentSheet.ScrollEngine.SetViewportPosition(
          TALPointD.Create(-FCurrentSheet.Container.Width - FCurrentSheet.Container.Position.X, 0));
      end;
      TALSheet.TDockEdge.Right: begin
        FCurrentSheet.ScrollEngine.SetViewportPosition(
          TALPointD.Create(FCurrentSheet.Width - FCurrentSheet.Container.Position.X, 0));
      end;
      TALSheet.TDockEdge.Top: begin
        FCurrentSheet.ScrollEngine.SetViewportPosition(
          TALPointD.Create(0, -FCurrentSheet.Container.Height - FCurrentSheet.Container.Position.Y));
      end;
      TALSheet.TDockEdge.Bottom: begin
        FCurrentSheet.ScrollEngine.SetViewportPosition(
          TALPointD.Create(0, FCurrentSheet.Height - FCurrentSheet.Container.Position.Y));
      end;
      else
        Raise Exception.Create('Error DC611DF1-BDCC-4477-AF51-3D3578FBC8F9')
    end;
    FCurrentSheet.FDisableScrollRealign := True;
    try
      FCurrentSheet.FTouchBlocker.Visible := False;
    finally
      FCurrentSheet.FDisableScrollRealign := False;
    end;
  end;
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Sheets','initialization');
  {$ENDIF}
  TALSheetManager.FInstance := nil;
  TALSheetManager.CreateInstanceFunc := @TALSheetManager.CreateInstance;
  ALSheetCloseButtonResourceName := 'alcinoe_cross';
  ALLeftSheetBackButtonResourceName := 'alcinoe_arrow_next';
  ALRightSheetBackButtonResourceName := 'alcinoe_arrow_back';

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Sheets','finalization');
  {$ENDIF}
  ALFreeAndNil(TALSheetManager.FInstance);

end.