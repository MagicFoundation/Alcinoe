unit Alcinoe.FMX.Snackbar;

interface

uses
  System.SysUtils,
  system.Classes,
  System.Generics.Collections,
  System.UITypes,
  FMX.Types,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.NativeControl;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALSnackbar = class(TALLayout)
  public
    const DURATION_LENGTH_SHORT = -1;
    const DURATION_LENGTH_LONG = 0;
    const DURATION_LENGTH_INDEFINITE = -2;
  public
    type
      // ----------------
      // TOnActionRefProc
      TOnActionRefProc = reference to procedure(Const ASnackbar: TALSnackbar; const AAction: Integer; var ACanClose: Boolean);
      // ----------------
      // TOnActionObjProc
      TOnActionObjProc = procedure(Const ASnackbar: TALSnackbar; const AAction: Integer; var ACanClose: Boolean) of object;
      // --------------
      // TAnimateOption
      TAnimateOption = (AnimateContainer);
      // ---------------
      // TAnimateOptions
      TAnimateOptions = set of TAnimateOption;
      // --------
      // TBuilder
      TBuilder = Class(TObject)
      private
        FSnackbar: TALSnackbar;
      public
        constructor Create;
        destructor Destroy; override;
        function SetOwnerAndParent(const AValue: TALControl): TBuilder;
        function SetContainerMarginLeft(const AValue: Single): TBuilder;
        function SetContainerMarginRight(const AValue: Single): TBuilder;
        function SetContainerMarginBottom(const AValue: Single): TBuilder;
        function SetMessageText(const AValue: String): TBuilder;
        function AddActionButton(const ACaption: String; const ATag: NativeInt): TBuilder;
        function AddCloseButton(const ATag: NativeInt): TBuilder;
        function SetOnActionCallback(const AValue: TOnActionRefProc): TBuilder; overload;
        function SetOnActionCallback(const AValue: TOnActionObjProc): TBuilder; overload;
        /// <summary>
        ///   Shows the snackbar and schedules its automatic dismissal,
        ///   mirroring Android behavior: pass
        ///     DURATION_LENGTH_SHORT (-1, ≈1500 ms),
        ///     DURATION_LENGTH_LONG (0, ≈2750 ms),
        ///     DURATION_LENGTH_INDEFINITE (-2, no auto-dismiss), or a
        ///     positive custom duration in milliseconds;
        ///   the timeout countdown begins after the enter animation completes
        ///   and is paused during touch/drag, resuming on release, while
        ///   platforms may extend the effective duration for accessibility; if
        ///   a snackbar is already visible and <c>AForceImmediateShow</c> is
        ///   <c>false</c>, this instance is queued and shown after the current
        ///   one closes, whereas <c>true</c> dismisses the current one
        ///   immediately and replaces it; the Builder instance
        ///   is released during this call.</summary>
        procedure Show(const ADuration: Integer = DURATION_LENGTH_LONG; const AForceImmediateShow: Boolean = True);
        property Snackbar: TALSnackbar read FSnackbar;
      end;
  public
    /// <summary>
    ///   Creates a builder object. The builder will automatically be
    ///   released when calling the Show method.
    ///   If you do not call Show, you are responsible for releasing the builder manually.
    /// </summary>
    class function Builder: TBuilder;
  private
    FMouseDownDT: TDateTime;
    FAutoDismissTimer: TALDisplayTimer;
    FContainer: TALRectangle;
    FMessage: TALText;
    FActionButton: TAlButton;
    FCloseButton: TALButton;
    FShowAnimateOptions: TAnimateOptions;
    FCloseAnimateOptions: TAnimateOptions;
    FOnActionRefProc: TOnActionRefProc;
    FOnActionObjProc: TOnActionObjProc;
    FOnClosedRefProc: TProc;
    procedure AutoDismissTimerProcess(Sender: TObject);
    procedure ContainerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ContainerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    function GetActionButton: TAlButton;
    function GetCloseButton: TALButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    class function Current: TALSnackbar;
    class procedure CloseCurrent(const AOnClosedCallback: TProc = nil; const AAnimateOptions: TAnimateOptions = [TAnimateOption.AnimateContainer]);
    function IsTransitioning: Boolean;
    function HasActionButton: Boolean;
    function HasCloseButton: Boolean;
    procedure AddActionButton(const ACaption: String; const ATag: NativeInt);
    procedure AddCloseButton(const ATag: NativeInt);
    procedure ActionButtonClick(Sender: TObject); virtual;
    property Container: TALRectangle read FContainer;
    property &Message: TALText read FMessage;
    property ActionButton: TALButton read GetActionButton;
    property CloseButton: TALButton read GetcloseButton;
    property ShowAnimateOptions: TAnimateOptions read FShowAnimateOptions write FShowAnimateOptions;
    property CloseAnimateOptions: TAnimateOptions read FCloseAnimateOptions write FCloseAnimateOptions;
    property OnActionRefProc: TOnActionRefProc read FOnActionRefProc write FOnActionRefProc;
    property OnActionObjProc: TOnActionObjProc read FOnActionObjProc write FOnActionObjProc;
    property OnClosedRefProc: TProc read FOnClosedRefProc write FOnClosedRefProc;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALSnackbarManager = class(TInterfacedObject, IFreeNotification)
  private
    class function CreateInstance: TALSnackbarManager;
    class function GetInstance: TALSnackbarManager; static;
  protected
    class var FInstance: TALSnackbarManager;
  public
    type
      TCreateInstanceFunc = function: TALSnackbarManager;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALSnackbarManager read GetInstance;
    class function HasInstance: Boolean; inline;
  public
    const SHORT_DURATION_MS = 1500;
    const LONG_DURATION_MS = 2750;
  private
    FDefaultContainer: TALRectangle;
    FDefaultMessage: TALText;
    FDefaultActionButton: TALButton;
    FDefaultCloseButton: TALButton;
    FCurrentSnackbar: TALSnackbar;
    FContainerAnimation: TALFloatAnimation;
    FQueue: TQueue<TALSnackbar>;
    FFrozenNativeControls: TArray<TALNativeControl>;
    procedure FreezeNativeViews;
    procedure UnfreezeNativeViews;
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject);
  protected
    procedure ContainerAnimationProcess(Sender: TObject);
    procedure ContainerAnimationFinish(Sender: TObject);
    Function HasPendingSnackbars: Boolean;
    procedure ProcessPendingSnackbars;
    procedure ShowSnackbar(const ASnackbar: TALSnackbar);
    procedure DoCloseCurrentSnackbar;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    /// <summary>
    ///   If a Snackbar is already being shown and <c>AForceImmediateShow</c> is <c>false</c>,
    ///   the new Snackbar will be queued and displayed after the current one is closed.
    ///   If <c>AForceImmediateShow</c> is <c>true</c>, the current Snackbar will be closed
    ///   immediately and replaced by the new one.
    ///   Note: Showing a Snackbar on top of another is considered a poor UI practice.
    /// </summary>
    procedure RequestSnackbar(const ASnackbar: TALSnackbar; const AForceImmediateShow: Boolean);
    procedure CustomSnackbarFinished;
    property DefaultContainer: TALRectangle read FDefaultContainer;
    property DefaultMessage: TALText read FDefaultMessage;
    property DefaultActionButton: TALButton read FDefaultActionButton;
    property DefaultCloseButton: TALButton read FDefaultCloseButton;
    property CurrentSnackbar: TALSnackbar read fCurrentSnackbar write fCurrentSnackbar;
    function IsShowingSnackbar: Boolean;
    procedure CloseCurrentSnackbar;
  end;

var
  ALSnackbarCloseButtonResourceName: String;

implementation

uses
  System.Types,
  System.DateUtils,
  FMX.Forms,
  Alcinoe.FMX.Styles,
  Alcinoe.Common;

{**************************************}
constructor TALSnackbar.TBuilder.Create;
begin
  Inherited create;
  FSnackbar := TALSnackbar.Create(nil);
  FSnackbar.BeginUpdate;
end;

{**************************************}
destructor TALSnackbar.TBuilder.Destroy;
begin
  ALFreeAndNil(FSnackbar);
  inherited;
end;

{**********************************************************************************}
function TALSnackbar.TBuilder.SetOwnerAndParent(const AValue: TALControl): TBuilder;
begin
  // This will defacto call AValue.Realign and FSnackbar.EndUpdate
  // in TControl.DoAddObject.SetUpdatingState
  AValue.InsertComponent(FSnackbar);
  FSnackbar.Parent := AValue;
  FSnackbar.BeginUpdate;
  Result := Self;
end;

{***********************************************************************************}
function TALSnackbar.TBuilder.SetContainerMarginLeft(const AValue: Single): TBuilder;
begin
  FSnackbar.Container.Margins.Left := AValue;
  Result := Self;
end;

{************************************************************************************}
function TALSnackbar.TBuilder.SetContainerMarginRight(const AValue: Single): TBuilder;
begin
  FSnackbar.Container.Margins.Right := AValue;
  Result := Self;
end;

{*************************************************************************************}
function TALSnackbar.TBuilder.SetContainerMarginBottom(const AValue: Single): TBuilder;
begin
  FSnackbar.Container.Margins.Bottom := AValue;
  Result := Self;
end;

{***************************************************************************}
function TALSnackbar.TBuilder.SetMessageText(const AValue: String): TBuilder;
begin
  FSnackbar.&Message.Text := AValue;
  Result := Self;
end;

{*****************************************************************************************************}
function TALSnackbar.TBuilder.AddActionButton(const ACaption: String; const ATag: NativeInt): TBuilder;
begin
  FSnackbar.AddActionButton(ACaption, ATag);
  Result := Self;
end;

{****************************************************************************}
function TALSnackbar.TBuilder.AddCloseButton(const ATag: NativeInt): TBuilder;
begin
  FSnackbar.AddCloseButton(ATag);
  Result := Self;
end;

{******************************************************************************************}
function TALSnackbar.TBuilder.SetOnActionCallback(const AValue: TOnActionRefProc): TBuilder;
begin
  FSnackbar.OnActionRefProc := AValue;
  Result := Self;
end;

{******************************************************************************************}
function TALSnackbar.TBuilder.SetOnActionCallback(const AValue: TOnActionObjProc): TBuilder;
begin
  FSnackbar.OnActionObjProc := AValue;
  Result := Self;
end;

{******************************************************************************************************************************}
procedure TALSnackbar.TBuilder.Show(const ADuration: Integer = DURATION_LENGTH_LONG; const AForceImmediateShow: Boolean = True);
begin
  case ADuration of
    DURATION_LENGTH_SHORT: FSnackbar.FAutoDismissTimer.Interval := TALSnackbarManager.SHORT_DURATION_MS / 1000;
    DURATION_LENGTH_LONG: FSnackbar.FAutoDismissTimer.Interval := TALSnackbarManager.LONG_DURATION_MS / 1000;
    DURATION_LENGTH_INDEFINITE: FSnackbar.FAutoDismissTimer.Interval := 0;
    else FSnackbar.FAutoDismissTimer.Interval := ADuration / 1000;
  end;
  FSnackbar.FAutoDismissTimer.Delay := FSnackbar.FAutoDismissTimer.Interval;
  TALSnackbarManager.Instance.RequestSnackbar(FSnackbar, AForceImmediateShow);
  FSnackbar := nil;
  Free;
end;

{*******************************************}
class function TALSnackbar.Builder: TBuilder;
begin
  Result := TBuilder.Create;
end;

{*************************************************}
constructor TALSnackbar.Create(AOwner: TComponent);
begin
  inherited;
  FMouseDownDT := 0;
  FAutoDismissTimer := TALDisplayTimer.Create;
  FAutoDismissTimer.OnProcess := AutoDismissTimerProcess;
  FContainer := TALRectangle.Create(Self);
  FContainer.Parent := Self;
  FContainer.Assign(TALSnackbarManager.Instance.DefaultContainer);
  FContainer.AutoCapture := True;
  FContainer.OnMouseDown := ContainerMouseDown;
  FContainer.OnMouseUp := ContainerMouseUp;
  FContainer.Name := 'ALSnackbarContainer';
  FMessage := TALText.Create(FContainer);
  FMessage.Parent := FContainer;
  FMessage.Assign(TALSnackbarManager.Instance.DefaultMessage);
  FMessage.Name := 'ALSnackbarMessage';
  FActionButton := nil;
  FCloseButton := nil;
  FShowAnimateOptions := [TAnimateOption.AnimateContainer];
  FCloseAnimateOptions := [TAnimateOption.AnimateContainer];
  FOnActionRefProc := nil;
  FOnActionObjProc := nil;
  FOnClosedRefProc := nil;
  Align := TALAlignLayout.Contents;
  Name := 'ALSnackbarScrim';
end;

{*****************************}
destructor TALSnackbar.Destroy;
begin
  ALFreeAndNil(FAutoDismissTimer);
  inherited;
end;

{**************************************}
procedure TALSnackbar.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  FAutoDismissTimer.Enabled := False;
  inherited;
end;

{**********************************************}
class function TALSnackbar.Current: TALSnackbar;
begin
  if not TALSnackbarManager.HasInstance then exit(nil);
  Result := TALSnackbarManager.Instance.CurrentSnackbar;
end;

{*********************************************************************************************************************************************************}
class procedure TALSnackbar.CloseCurrent(const AOnClosedCallback: TProc = nil; const AAnimateOptions: TAnimateOptions = [TAnimateOption.AnimateContainer]);
begin
  if not TALSnackbarManager.HasInstance then exit;
  var LCurrentSnackbar := TALSnackbarManager.Instance.CurrentSnackbar;
  if LCurrentSnackbar <> nil then begin
    LCurrentSnackbar.OnClosedRefProc := AOnClosedCallback;
    LCurrentSnackbar.CloseAnimateOptions := AAnimateOptions;
    TALSnackbarManager.Instance.CloseCurrentSnackbar;
  end;
end;

{********************************************}
function TALSnackbar.IsTransitioning: Boolean;
begin
  Result := (TALSnackbarManager.Instance.CurrentSnackbar = Self) and
            (TALSnackbarManager.Instance.FContainerAnimation.Running);
end;

{*************************************************************}
procedure TALSnackbar.AutoDismissTimerProcess(Sender: TObject);
begin
  CloseCurrent;
end;

{****************************************************************************************************************}
procedure TALSnackbar.ContainerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FMouseDownDT := Now;
  FAutoDismissTimer.Pause;
end;

{**************************************************************************************************************}
procedure TALSnackbar.ContainerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FAutoDismissTimer.Resume;
  If MilliSecondsBetween(FMouseDownDT, Now) <= 200 then CloseCurrent;
end;

{**********************************************}
function TALSnackbar.GetActionButton: TAlButton;
begin
  If FActionButton = nil then begin
    FActionButton := TAlButton.Create(Container);
    FActionButton.Parent := Container;
    FActionButton.Assign(TALSnackbarManager.Instance.DefaultActionButton);
    FActionButton.OnClick := ActionButtonClick;
    FActionButton.Name := 'ALSnackbarActionButton';
  end;
  Result := FActionButton;
end;

{*********************************************}
function TALSnackbar.GetCloseButton: TAlButton;
begin
  If FCloseButton = nil then begin
    FCloseButton := TAlButton.Create(Container);
    FCloseButton.Parent := Container;
    FCloseButton.Assign(TALSnackbarManager.Instance.DefaultCloseButton);
    FCloseButton.OnClick := ActionButtonClick;
    FCloseButton.Name := 'ALSnackbarCloseButton';
  end;
  Result := FCloseButton;
end;

{********************************************}
function TALSnackbar.HasActionButton: Boolean;
begin
  result := FActionButton <> nil;
end;

{*******************************************}
function TALSnackbar.HasCloseButton: Boolean;
begin
  result := FCloseButton <> nil;
end;

{***********************************************************************************}
procedure TALSnackbar.AddActionButton(const ACaption: String; const ATag: NativeInt);
begin
  if HasActionButton then raise Exception.Create('A snackbar can only contain one action button');
  ActionButton.Text := ACaption;
  ActionButton.Tag := ATag;
end;

{**********************************************************}
procedure TALSnackbar.AddCloseButton(const ATag: NativeInt);
begin
  if HasCloseButton then raise Exception.Create('A snackbar can only contain one close button');
  CloseButton.Text := '';
  closeButton.Tag := ATag;
end;

{*******************************************************}
procedure TALSnackbar.ActionButtonClick(Sender: TObject);
begin
  var LCanClose: Boolean := True;
  if Assigned(FOnActionRefProc) then FOnActionRefProc(self, TALControl(Sender).Tag, LCanClose)
  else if Assigned(FOnActionObjProc) then FOnActionObjProc(self, TALControl(Sender).Tag, LCanClose)
  else LCanClose := True;
  If LCanClose then begin
    // One of the callbacks (FOnActionRefProc or FOnActionObjProc) may have asked
    // the Snackbar manager to present another Snackbar, which could already have
    // closed this one. Confirm that this Snackbar is still the active one before
    // calling CloseCurrentSnackbar.
    if (TALSnackbarManager.HasInstance) and
       (TALSnackbarManager.Instance.CurrentSnackbar = self) then
      TALSnackbarManager.Instance.CloseCurrentSnackbar;
  end;
end;

{************************************}
constructor TALSnackbarManager.Create;
begin
  inherited;
  FDefaultContainer := TALRectangle.Create(nil);
  FDefaultMessage := TALText.Create(nil);
  FDefaultActionButton := TALButton.Create(nil);
  FDefaultCloseButton := TALButton.Create(nil);
  FCurrentSnackbar := Nil;
  FContainerAnimation := TALFloatAnimation.Create;
  FContainerAnimation.OnProcess := ContainerAnimationProcess;
  FContainerAnimation.OnFinish := ContainerAnimationFinish;
  FQueue := TQueue<TALSnackbar>.create;
  Setlength(FFrozenNativeControls, 0);
end;

{************************************}
destructor TALSnackbarManager.Destroy;
begin
  AlFreeAndNil(FDefaultContainer);
  AlFreeAndNil(FDefaultMessage);
  AlFreeAndNil(FDefaultActionButton);
  AlFreeAndNil(FDefaultCloseButton);
  ALFreeAndNil(FContainerAnimation);
  ALFreeAndNil(FQueue);
  inherited;
end;

{*********************************************}
procedure TALSnackbarManager.AfterConstruction;
begin
  inherited;
  TALStyleManager.Instance.ApplySnackbarManagerStyle('Default', Self);
end;

{*******************************************************************}
class function TALSnackbarManager.CreateInstance: TALSnackbarManager;
begin
  result := TALSnackbarManager.Create;
end;

{*************}
//[MultiThread]
class function TALSnackbarManager.GetInstance: TALSnackbarManager;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALSnackbarManager.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{*********************************************}
procedure TALSnackbarManager.FreezeNativeViews;
begin
  //For var I := Low(FFrozenNativeControls) to high(FFrozenNativeControls) do
  //  if FFrozenNativeControls[I] <> nil then
  //    FFrozenNativeControls[I].RemoveFreeNotify(Self);
  //
  //ALFreezeNativeViews(FFrozenNativeControls);
  //
  //For var I := Low(FFrozenNativeControls) to high(FFrozenNativeControls) do
  //  if FFrozenNativeControls[I] <> nil then
  //    FFrozenNativeControls[I].AddFreeNotify(Self);
end;

{***********************************************}
procedure TALSnackbarManager.UnfreezeNativeViews;
begin
  //For var I := Low(FFrozenNativeControls) to high(FFrozenNativeControls) do
  //  if FFrozenNativeControls[I] <> nil then
  //    FFrozenNativeControls[I].RemoveFreeNotify(Self);
  //
  //ALUnfreezeNativeViews(FFrozenNativeControls)
end;

{**************************************************************}
procedure TALSnackbarManager.FreeNotification(AObject: TObject);
begin
  For var I := low(FFrozenNativeControls) to high(FFrozenNativeControls) do
    if FFrozenNativeControls[I] = AObject then
      FFrozenNativeControls[I] := nil;

  if FCurrentSnackBar = AObject then begin
    FContainerAnimation.Enabled := False;
    FCurrentSnackBar := Nil;
  end;
end;

{*****************************************************}
function TALSnackbarManager.IsShowingSnackbar: Boolean;
begin
  Result := FCurrentSnackbar <> nil;
end;

{**************************************************}
procedure TALSnackbarManager.CustomSnackbarFinished;
begin
  CloseCurrentSnackbar;
end;

{************************************************}
procedure TALSnackbarManager.CloseCurrentSnackbar;
begin
  if FCurrentSnackbar = nil then exit;

  If (not (TALSnackbar.TAnimateOption.AnimateContainer in FCurrentSnackbar.CloseAnimateOptions)) then begin
    DoCloseCurrentSnackbar;
    exit;
  end;

  FCurrentSnackbar.FAutoDismissTimer.Enabled := False;

  FContainerAnimation.Enabled := False;
  FContainerAnimation.InterpolationType := TALInterpolationType.Material3EmphasizedAccelerate;
  FContainerAnimation.Duration := 0.200;
  FContainerAnimation.StartValue := 1;
  FContainerAnimation.StopValue := 0;
  FContainerAnimation.Start;
end;

{**************************************************}
procedure TALSnackbarManager.DoCloseCurrentSnackbar;
begin
  if FCurrentSnackbar = nil then exit;
  var LCurrentSnackbar := FCurrentSnackbar;
  LCurrentSnackbar.Visible := False;
  LCurrentSnackbar.FAutoDismissTimer.Enabled := False;
  FCurrentSnackbar := nil;
  FContainerAnimation.Enabled := False;
  if assigned(LCurrentSnackbar.FOnClosedRefProc) then
    LCurrentSnackbar.FOnClosedRefProc();
  ProcessPendingSnackbars;
  if not IsShowingSnackbar then
    UnfreezeNativeViews;
  TThread.ForceQueue(nil,
    procedure
    begin
      ALFreeAndNil(LCurrentSnackbar);
    end);
end;

{*************}
//[MultiThread]
procedure TALSnackbarManager.RequestSnackbar(const ASnackbar: TALSnackbar; const AForceImmediateShow: Boolean);
begin
  TThread.queue(nil,
    procedure
    begin
      if TALSnackbarManager.HasInstance then begin
        with TALSnackbarManager.Instance do begin
          FQueue.Enqueue(ASnackbar);
          if AForceImmediateShow then begin
            While FQueue.Peek <> ASnackbar do
              FQueue.Enqueue(FQueue.Dequeue);
            if IsShowingSnackbar then
              CloseCurrentSnackbar
            else
              ProcessPendingSnackbars;
          end
          else
            ProcessPendingSnackbars;
        end;
      end
      else
        ALFreeAndNil(ASnackbar);
    end);
end;

{*******************************************************}
Function TALSnackbarManager.HasPendingSnackbars: Boolean;
begin
  Result := FQueue.Count > 0;
end;

{***************************************************}
procedure TALSnackbarManager.ProcessPendingSnackbars;
begin
  if (not IsShowingSnackbar) and
     (HasPendingSnackbars) then begin
    var LSnackbar := FQueue.Dequeue;
    try
      ShowSnackbar(LSnackbar);
    Except
      ALFreeAndNil(LSnackbar);
      Raise;
    end;
  end;
end;

{**********************************************************************}
procedure TALSnackbarManager.ShowSnackbar(const ASnackbar: TALSnackbar);
begin

  // Init LForm & LClientWidth
  Var LForm: TCommonCustomForm;
  var LClientWidth: Single;
  if ASnackbar.ParentControl = nil then begin
    LForm := Screen.ActiveForm;
    if LForm = nil then LForm := Application.MainForm;
    if LForm = nil then Raise Exception.Create('Error 0B1C5551-F59D-46FA-8E9B-A10AB6A65FDE');
    LClientWidth := LForm.ClientWidth;
  end
  else begin
    LForm := nil;
    LClientWidth := ASnackbar.ParentControl.Width;
  end;

  // Init ASnackbar.Message.MaxWidth
  var LUsedWidth: Single := ASnackbar.container.Padding.Left + ASnackbar.Container.Padding.Right +
                            ASnackbar.container.margins.Left + ASnackbar.Container.margins.Right +
                            ASnackbar.Message.Margins.Left + ASnackbar.Message.Margins.Right;
  if ASnackbar.HasActionButton then begin
    // BeginUpdate was called when the Snackbar was created.
    // Calling EndUpdate now will adjust the size of the ActionButton.
    ASnackBar.ActionButton.EndUpdate;
    ASnackBar.Container.Align := TalAlignLayout.Bottom;
    LUsedWidth := LUsedWidth + ASnackBar.ActionButton.Width + ASnackBar.ActionButton.Margins.Left + ASnackBar.ActionButton.Margins.Right;
  end;
  if ASnackbar.HasCloseButton then begin
    // BeginUpdate was called when the Snackbar was created.
    // Calling EndUpdate now will adjust the size of the CloseButton.
    ASnackBar.CloseButton.EndUpdate;
    ASnackBar.Container.Align := TalAlignLayout.Bottom;
    LUsedWidth := LUsedWidth + ASnackBar.CloseButton.Width + ASnackBar.CloseButton.Margins.Left + ASnackBar.CloseButton.Margins.Right;
  end;
  ASnackbar.Message.MaxWidth := LClientWidth - LUsedWidth;

  // Attach ASnackbar
  if ASnackbar.ParentControl = nil then begin
    LForm.Focused := nil;
    FreezeNativeViews;
    ASnackbar.Align := TALAlignLayout.Contents;
    // This will defacto call LForm.Realign and ASheet.EndUpdate
    // in TCustomForm.DoAddObject.SetUpdatingState
    LForm.InsertComponent(ASnackbar);
    ASnackbar.Parent := LForm;
  end
  else begin
    ASnackbar.ParentControl.ResetFocus;
    FreezeNativeViews;
    ASnackbar.Align := TALAlignLayout.Contents;
    ASnackbar.EndUpdate;
  end;

  // Init FCurrentSnackbar
  FCurrentSnackbar := ASnackbar;
  FCurrentSnackbar.AddFreeNotify(Self);

  // ShowAnimateOptions
  if TALSnackbar.TAnimateOption.AnimateContainer in FCurrentSnackbar.ShowAnimateOptions then begin

    FCurrentSnackbar.Container.Scale.Y := 0;
    FCurrentSnackbar.Container.Pivot.Point := TpointF.Create(0,1);
    FContainerAnimation.Enabled := False;
    FContainerAnimation.InterpolationType := TALInterpolationType.Material3EmphasizedDecelerate;
    FContainerAnimation.Duration := 0.400;
    FContainerAnimation.StartValue := 0;
    FContainerAnimation.StopValue := 1;
    FContainerAnimation.Start;

  end
  else
    ContainerAnimationFinish(nil);

end;

{**********************************************************************}
procedure TALSnackbarManager.ContainerAnimationProcess(Sender: TObject);
begin
  if FCurrentSnackbar = nil then exit;
  FCurrentSnackbar.Container.Scale.Y := FContainerAnimation.CurrentValue;
  if FContainerAnimation.StopValue < 0.5 then
    FCurrentSnackbar.Container.Opacity := FContainerAnimation.CurrentValue;
end;

{*********************************************************************}
procedure TALSnackbarManager.ContainerAnimationFinish(Sender: TObject);
begin
  if FCurrentSnackbar = nil then exit;
  if FContainerAnimation.StopValue < 0.5 then DoCloseCurrentSnackbar
  else begin
    if FCurrentSnackbar.FAutoDismissTimer.Interval > 0 then
      FCurrentSnackbar.FAutoDismissTimer.Start;
  end;
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Snackbar','initialization');
  {$ENDIF}
  TALSnackbarManager.FInstance := nil;
  TALSnackbarManager.CreateInstanceFunc := @TALSnackbarManager.CreateInstance;
  ALSnackbarCloseButtonResourceName := 'alcinoe_cross';

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Snackbar','finalization');
  {$ENDIF}
  ALFreeAndNil(TALSnackbarManager.FInstance);

end.
