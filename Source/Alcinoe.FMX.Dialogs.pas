unit Alcinoe.FMX.Dialogs;

interface

uses
  System.SysUtils,
  system.Classes,
  System.Generics.Collections,
  System.Messaging,
  System.UITypes,
  FMX.Types,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.NativeControl;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // TALDialog is the root control of the dialog system.
  // It represents the scrim that overlays the entire form,
  // blocking all touch events on the underlying UI.
  TALDialog = class(TALRectangle)
  public
    type
      // ----------------
      // TOnActionRefProc
      TOnActionRefProc = reference to procedure(Const ADialog: TALDialog; const AAction: Integer; var ACanClose: Boolean);
      // ----------------
      // TOnActionObjProc
      TOnActionObjProc = procedure(Const ADialog: TALDialog; const AAction: Integer; var ACanClose: Boolean) of object;
      // --------------------
      // TCustomDialogRefProc
      TCustomDialogRefProc = reference to procedure;
      // --------------------
      // TCustomDialogObjProc
      TCustomDialogObjProc = procedure of object;
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
        FDialog: TALDialog;
      public
        constructor Create;
        destructor Destroy; override;
        function SetOwnerAndParent(const AValue: TALControl): TBuilder;
        function SetIconResourceName(const AValue: String): TBuilder;
        function SetIconTintColor(const AValue: TAlphaColor): TBuilder;
        function SetIconSize(const AWidth, AHeight: Single): TBuilder;
        function SetHeadlineText(const AValue: String): TBuilder;
        function SetHeadlineAlign(const AValue: TAlAlignLayout): TBuilder;
        function SetHeadlineTextHorzAlign(const AValue: TALTextHorzAlign): TBuilder;
        function SetMessageText(const AValue: String): TBuilder;
        function AddRadioButton(const ALabel: String; const ATag: NativeInt; const AChecked: Boolean; const AMandatory: Boolean = True): TBuilder;
        function AddCheckBox(const ALabel: String; const ATag: NativeInt; const AChecked: Boolean): TBuilder;
        function AddEdit(const APromptText: String; const ALabelText: String; const ASupportingText: String; const ATag: NativeInt): TBuilder;
        function AddMemo(const APromptText: String; const ALabelText: String; const ASupportingText: String; const ATag: NativeInt): TBuilder;
        function AddButton(const ACaption: String; const ATag: NativeInt; Const AIsFooterButton: Boolean = True): TBuilder;
        function SetButtonFontColor(const ATag: NativeInt; const AFontColor: TalphaColor): TBuilder;
        function SetCloseOnScrimClick(const AValue: boolean): TBuilder;
        function SetCustomContainer(const AValue: TALControl): TBuilder;
        function SetCustomDialogProc(const AValue: TCustomDialogRefProc): TBuilder; overload;
        function SetCustomDialogProc(const AValue: TCustomDialogObjProc): TBuilder; overload;
        function SetOnActionCallback(const AValue: TOnActionRefProc): TBuilder; overload;
        function SetOnActionCallback(const AValue: TOnActionObjProc): TBuilder; overload;
        /// <summary>
        ///   The Builder instance will be released during this operation.
        ///   If a dialog is already being shown and <c>AForceImmediateShow</c> is <c>false</c>,
        ///   the new dialog will be queued and displayed after the current one is closed.
        ///   If <c>AForceImmediateShow</c> is <c>true</c>, the current dialog will be closed
        ///   immediately and replaced by the new one.
        ///   Note: Showing a dialog on top of another is considered a poor UI practice.
        /// </summary>
        procedure Show(const AForceImmediateShow: Boolean = True);
        property Dialog: TALDialog read FDialog;
      end;
  public
    /// <summary>
    ///   Creates a builder object. The builder will automatically be
    ///   released when calling the Show method.
    ///   If you do not call Show, you are responsible for releasing the builder manually.
    /// </summary>
    class function Builder: TBuilder;
  private
    FTouchBlocker: TALLayout;
    FContainer: TALRectangle;
    FIcon: TALImage;
    FHeadline: TALText;
    FContent: TALVertScrollBox;
    FMessage: TALText;
    FButtonBar: TALRectangle;
    FCustomContainer: TALControl;
    FShowAnimateOptions: TAnimateOptions;
    FCloseAnimateOptions: TAnimateOptions;
    FCustomDialogRefProc: TCustomDialogRefProc;
    FCustomDialogObjProc: TCustomDialogObjProc;
    FOnActionRefProc: TOnActionRefProc;
    FOnActionObjProc: TOnActionObjProc;
    FOnClosedRefProc: TProc;
    FVirtualKeyboardAnimation: TALFloatAnimation;
    function GetCloseOnScrimClick: boolean;
    procedure SetCloseOnScrimClick(const AValue: Boolean);
    function GetContainer: TALRectangle;
    function GetIcon: TALImage;
    function GetHeadline: TALText;
    function GetContent: TALVertScrollBox;
    function GetMessage: TALText;
    function GetButtonBar: TALRectangle;
    procedure SetCustomContainer(const AValue: TALControl);
    procedure VirtualKeyboardChangeHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
    procedure VirtualKeyboardAnimationProcess(Sender: TObject);
  protected
    procedure LabelClick(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    class function Current: TALDialog;
    class procedure CloseCurrent(const AOnClosedCallback: TProc = nil; const AAnimateOptions: TAnimateOptions = [TAnimateOption.AnimateScrim, TAnimateOption.AnimateContainer]);
    function IsTransitioning: Boolean;
    property CloseOnScrimClick: Boolean read GetCloseOnScrimClick write SetCloseOnScrimClick;
    function HasContainer: Boolean;
    function HasIcon: Boolean;
    function HasHeadline: Boolean;
    function HasContent: Boolean;
    function HasMessage: Boolean;
    function HasButtonBar: Boolean;
    function HasCustomContainer: Boolean;
    function GetRadioButtons: TArray<TALRadioButton>;
    function GetCheckBoxes: TArray<TALCheckBox>;
    function GetEdits: TArray<TALBaseEdit>;
    function GetButtons: TArray<TALbutton>;
    function GetRadioButton(const ATag: NativeInt): TALRadioButton;
    function GetCheckedRadioButton: TALRadioButton;
    function GetCheckBox(const ATag: NativeInt): TALCheckBox;
    function GetEdit(const ATag: NativeInt): TALBaseEdit;
    function GetButton(const ATag: NativeInt): TALButton;
    procedure AddRadioButton(const ALabel: String; const ATag: NativeInt; const AChecked: Boolean; const AMandatory: Boolean = True);
    procedure AddCheckBox(const ALabel: String; const ATag: NativeInt; const AChecked: Boolean);
    procedure AddEdit(const APromptText: String; const ALabelText: String; const ASupportingText: String; const ATag: NativeInt);
    procedure AddMemo(const APromptText: String; const ALabelText: String; const ASupportingText: String; const ATag: NativeInt);
    procedure AddButton(const ACaption: String; const ATag: NativeInt; Const AIsFooterButton: Boolean = True);
    procedure ActionButtonClick(Sender: TObject); virtual;
    property Container: TALRectangle read GetContainer;
    property Icon: TALImage read GetIcon;
    property Headline: TALText read GetHeadline;
    property Content: TALVertScrollBox read GetContent;
    property &Message: TALText read GetMessage;
    property ButtonBar: TALRectangle read GetButtonBar;
    property CustomContainer: TALControl read FCustomContainer write SetCustomContainer;
    property ShowAnimateOptions: TAnimateOptions read FShowAnimateOptions write FShowAnimateOptions;
    property CloseAnimateOptions: TAnimateOptions read FCloseAnimateOptions write FCloseAnimateOptions;
    property CustomDialogRefProc: TCustomDialogRefProc read FCustomDialogRefProc write FCustomDialogRefProc;
    property CustomDialogObjProc: TCustomDialogObjProc read FCustomDialogObjProc write FCustomDialogObjProc;
    property OnActionRefProc: TOnActionRefProc read FOnActionRefProc write FOnActionRefProc;
    property OnActionObjProc: TOnActionObjProc read FOnActionObjProc write FOnActionObjProc;
    property OnClosedRefProc: TProc read FOnClosedRefProc write FOnClosedRefProc;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDialogManager = class(TInterfacedObject, IFreeNotification)
  private
    class function CreateInstance: TALDialogManager;
    class function GetInstance: TALDialogManager; static;
  protected
    class var FInstance: TALDialogManager;
  public
    type
      TCreateInstanceFunc = function: TALDialogManager;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALDialogManager read GetInstance;
    class function HasInstance: Boolean; inline;
  private
    FDefaultScrim: TALRectangle;
    FDefaultContainer: TALRectangle;
    FDefaultIcon: TALImage;
    FDefaultHeadline: TALText;
    FDefaultContent: TALVertScrollBox;
    FDefaultMessage: TALText;
    FDefaultOptionLayout: TALLayout;
    FDefaultRadioButton: TALRadioButton;
    FDefaultCheckBox: TALCheckbox;
    FDefaultInlineButton: TALButton;
    FDefaultEdit: TALDummyEdit;
    FDefaultMemo: TALDummyMemo;
    FDefaultLabel: TALText;
    FDefaultFooterBar: TALRectangle;
    FDefaultFooterButton: TALButton;
    FCurrentDialog: TALDialog;
    FScrimAnimation: TALFloatAnimation;
    FContainerAnimation: TALFloatAnimation;
    FQueue: TQueue<TALDialog>;
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
    Function HasPendingDialogs: Boolean;
    procedure ProcessPendingDialogs;
    procedure ShowDialog(const ADialog: TALDialog);
    procedure DoCloseCurrentDialog;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function GetMinDialogContainerWidth: Single;
    function GetMaxDialogContainerWidth: Single;
    /// <summary>
    ///   If a dialog is already being shown and <c>AForceImmediateShow</c> is <c>false</c>,
    ///   the new dialog will be queued and displayed after the current one is closed.
    ///   If <c>AForceImmediateShow</c> is <c>true</c>, the current dialog will be closed
    ///   immediately and replaced by the new one.
    ///   Note: Showing a dialog on top of another is considered a poor UI practice.
    /// </summary>
    procedure RequestDialog(const ADialog: TALDialog; const AForceImmediateShow: Boolean);
    procedure CustomDialogFinished;
    property DefaultScrim: TALRectangle read FDefaultScrim;
    property DefaultContainer: TALRectangle read FDefaultContainer;
    property DefaultIcon: TALImage read FDefaultIcon;
    property DefaultHeadline: TALText read FDefaultHeadline;
    property DefaultContent: TALVertScrollBox read FDefaultContent;
    property DefaultMessage: TALText read FDefaultMessage;
    property DefaultOptionLayout: TALLayout read FDefaultOptionLayout;
    property DefaultRadioButton: TALRadioButton read FDefaultRadioButton;
    property DefaultCheckBox: TALCheckbox read FDefaultCheckBox;
    property DefaultInlineButton: TALButton read FDefaultInlineButton;
    property DefaultEdit: TALDummyEdit read FDefaultEdit;
    property DefaultMemo: TALDummyMemo read FDefaultMemo;
    property DefaultLabel: TALText read FDefaultLabel;
    property DefaultFooterBar: TALRectangle read FDefaultFooterBar;
    property DefaultFooterButton: TALButton read FDefaultFooterButton;
    property CurrentDialog: TALDialog read fCurrentDialog write fCurrentDialog;
    function IsShowingDialog: Boolean;
    procedure CloseCurrentDialog;
  end;

implementation

uses
  System.Math,
  System.Types,
  Fmx.Controls,
  FMX.Forms,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Styles,
  Alcinoe.fmx.LoadingOverlay,
  Alcinoe.Common;

{************************************}
constructor TALDialog.TBuilder.Create;
begin
  Inherited create;
  FDialog := TALDialog.Create(nil);
  FDialog.BeginUpdate;
end;

{************************************}
destructor TALDialog.TBuilder.Destroy;
begin
  ALFreeAndNil(FDialog);
  inherited;
end;

{********************************************************************************}
function TALDialog.TBuilder.SetOwnerAndParent(const AValue: TALControl): TBuilder;
begin
  // This will defacto call AValue.Realign and FDialog.EndUpdate
  // in TControl.DoAddObject.SetUpdatingState
  AValue.InsertComponent(FDialog);
  FDialog.Parent := AValue;
  FDialog.BeginUpdate;
  Result := Self;
end;

{******************************************************************************}
function TALDialog.TBuilder.SetIconResourceName(const AValue: String): TBuilder;
begin
  FDialog.Icon.ResourceName := AValue;
  Result := Self;
end;

{********************************************************************************}
function TALDialog.TBuilder.SetIconTintColor(const AValue: TAlphaColor): TBuilder;
begin
  FDialog.Icon.TintColor := AValue;
  Result := Self;
end;

{*******************************************************************************}
function TALDialog.TBuilder.SetIconSize(const AWidth, AHeight: Single): TBuilder;
begin
  FDialog.Icon.Size.Size := TSizeF.Create(AWidth, AHeight);
  Result := Self;
end;

{**************************************************************************}
function TALDialog.TBuilder.SetHeadlineText(const AValue: String): TBuilder;
begin
  FDialog.Headline.Text := AValue;
  Result := Self;
end;

{***********************************************************************************}
function TALDialog.TBuilder.SetHeadlineAlign(const AValue: TAlAlignLayout): TBuilder;
begin
  FDialog.Headline.Align := AValue;
  Result := Self;
end;

{*********************************************************************************************}
function TALDialog.TBuilder.SetHeadlineTextHorzAlign(const AValue: TALTextHorzAlign): TBuilder;
begin
  FDialog.Headline.TextSettings.HorzAlign := AValue;
  Result := Self;
end;

{*************************************************************************}
function TALDialog.TBuilder.SetMessageText(const AValue: String): TBuilder;
begin
  FDialog.&Message.Text := AValue;
  Result := Self;
end;

{***********************************************************************************************************************************************************}
function TALDialog.TBuilder.AddRadioButton(const ALabel: String; const ATag: NativeInt; const AChecked: Boolean; const AMandatory: Boolean = True): TBuilder;
begin
  FDialog.AddRadioButton(ALabel, ATag, AChecked, AMandatory);
  Result := Self;
end;

{**********************************************************************************************************************}
function TALDialog.TBuilder.AddCheckBox(const ALabel: String; const ATag: NativeInt; const AChecked: Boolean): TBuilder;
begin
  FDialog.AddCheckBox(ALabel, ATag, AChecked);
  Result := Self;
end;

{*******************************************************************************************************************************************************}
function TALDialog.TBuilder.AddEdit(const APromptText: String; const ALabelText: String; const ASupportingText: String; const ATag: NativeInt): TBuilder;
begin
  FDialog.AddEdit(APromptText, ALabelText, ASupportingText, ATag);
  Result := Self;
end;

{*******************************************************************************************************************************************************}
function TALDialog.TBuilder.AddMemo(const APromptText: String; const ALabelText: String; const ASupportingText: String; const ATag: NativeInt): TBuilder;
begin
  FDialog.AddMemo(APromptText, ALabelText, ASupportingText, ATag);
  Result := Self;
end;

{************************************************************************************************************************************}
function TALDialog.TBuilder.AddButton(const ACaption: String; const ATag: NativeInt; Const AIsFooterButton: Boolean = True): TBuilder;
begin
  FDialog.AddButton(ACaption, ATag, AIsFooterButton);
  Result := Self;
end;

{*************************************************************************************************************}
function TALDialog.TBuilder.SetButtonFontColor(const ATag: NativeInt; const AFontColor: TalphaColor): TBuilder;
begin
  var Lbutton := FDialog.GetButton(ATag);
  if Lbutton <> nil then Lbutton.TextSettings.Font.Color := AFontColor;
  Result := Self;
end;

{********************************************************************************}
function TALDialog.TBuilder.SetCloseOnScrimClick(const AValue: boolean): TBuilder;
begin
  FDialog.SetCloseOnScrimClick(AValue);
  Result := Self;
end;

{*********************************************************************************}
function TALDialog.TBuilder.SetCustomContainer(const AValue: TALControl): TBuilder;
begin
  FDialog.CustomContainer := AValue;
  Result := Self;
end;

{********************************************************************************************}
function TALDialog.TBuilder.SetCustomDialogProc(const AValue: TCustomDialogRefProc): TBuilder;
begin
  FDialog.CustomDialogRefProc := AValue;
  Result := Self;
end;

{********************************************************************************************}
function TALDialog.TBuilder.SetCustomDialogProc(const AValue: TCustomDialogObjProc): TBuilder;
begin
  FDialog.CustomDialogObjProc := AValue;
  Result := Self;
end;

{****************************************************************************************}
function TALDialog.TBuilder.SetOnActionCallback(const AValue: TOnActionRefProc): TBuilder;
begin
  FDialog.OnActionRefProc := AValue;
  Result := Self;
end;

{****************************************************************************************}
function TALDialog.TBuilder.SetOnActionCallback(const AValue: TOnActionObjProc): TBuilder;
begin
  FDialog.OnActionObjProc := AValue;
  Result := Self;
end;

{***************************************************************************}
procedure TALDialog.TBuilder.Show(const AForceImmediateShow: Boolean = True);
begin
  TALDialogManager.Instance.RequestDialog(FDialog, AForceImmediateShow);
  FDialog := nil;
  Free;
end;

{*****************************************}
class function TALDialog.Builder: TBuilder;
begin
  Result := TBuilder.Create;
end;

{***********************************************}
constructor TALDialog.Create(AOwner: TComponent);
begin
  inherited;
  FTouchBlocker := Nil;
  FContainer := nil;
  FIcon := nil;
  FHeadline := nil;
  FContent := nil;
  FMessage := nil;
  FButtonBar := nil;
  FCustomContainer := nil;
  FShowAnimateOptions := [TAnimateOption.AnimateScrim, TAnimateOption.AnimateContainer];
  FCloseAnimateOptions := [TAnimateOption.AnimateScrim, TAnimateOption.AnimateContainer];
  FCustomDialogRefProc := nil;
  FCustomDialogObjProc := nil;
  FOnActionRefProc := nil;
  FOnActionObjProc := nil;
  FOnClosedRefProc := nil;
  FVirtualKeyboardAnimation := nil;
  TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, VirtualKeyboardChangeHandler);
  Assign(TALDialogManager.Instance.DefaultScrim);
  Name := 'ALDialogScrim';
  Onclick := ActionButtonClick;
end;

{***************************}
destructor TALDialog.Destroy;
begin
  ALFreeAndNil(FVirtualKeyboardAnimation);
  inherited;
end;

{************************************}
procedure TALDialog.BeforeDestruction;
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

{******************************************}
class function TALDialog.Current: TALDialog;
begin
  if not TALDialogManager.HasInstance then exit(nil);
  Result := TALDialogManager.Instance.CurrentDialog;
end;

{************************************************************************************************************************************************************************************}
class procedure TALDialog.CloseCurrent(const AOnClosedCallback: TProc = nil; const AAnimateOptions: TAnimateOptions = [TAnimateOption.AnimateScrim, TAnimateOption.AnimateContainer]);
begin
  if not TALDialogManager.HasInstance then exit;
  var LCurrentDialog := TALDialogManager.Instance.CurrentDialog;
  if LCurrentDialog <> nil then begin
    LCurrentDialog.OnClosedRefProc := AOnClosedCallback;
    LCurrentDialog.CloseAnimateOptions := AAnimateOptions;
    TALDialogManager.Instance.CloseCurrentDialog;
  end;
end;

{******************************************}
function TALDialog.IsTransitioning: Boolean;
begin
  Result := (TALDialogManager.Instance.CurrentDialog = Self) and
            (TALDialogManager.Instance.FContainerAnimation.Running or TALDialogManager.Instance.FScrimAnimation.Running);
end;

{************************************************************************************************************}
procedure TALDialog.VirtualKeyboardChangeHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
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

{*******************************************************************}
procedure TALDialog.VirtualKeyboardAnimationProcess(Sender: TObject);
begin
  margins.Bottom := FVirtualKeyboardAnimation.CurrentValue;
end;

{***********************************************}
function TALDialog.GetCloseOnScrimClick: boolean;
begin
  result := Assigned(Onclick);
end;

{**************************************************************}
procedure TALDialog.SetCloseOnScrimClick(const AValue: Boolean);
begin
  if AValue then Onclick := ActionButtonClick
  else Onclick := nil;
end;

{********************************************}
function TALDialog.GetContainer: TALRectangle;
begin
  If FContainer = nil then begin
    FContainer := TALRectangle.Create(Self);
    FContainer.Parent := Self;
    FContainer.Assign(TALDialogManager.Instance.DefaultContainer);
    FContainer.Name := 'ALDialogContainer';
  end;
  Result := FContainer;
end;

{***********************************}
function TALDialog.GetIcon: TALImage;
begin
  If FIcon = nil then begin
    FIcon := TALImage.Create(Container);
    FIcon.Parent := Container;
    FIcon.Assign(TALDialogManager.Instance.DefaultIcon);
    FIcon.Name := 'ALDialogIcon';
  end;
  Result := FIcon;
end;

{**************************************}
function TALDialog.GetHeadline: TALText;
begin
  If FHeadline = nil then begin
    FHeadline := TALText.Create(Container);
    FHeadline.Parent := Container;
    FHeadline.Assign(TALDialogManager.Instance.DefaultHeadline);
    FHeadline.Name := 'ALDialogHeadline';
  end;
  Result := FHeadline;
end;

{**********************************************}
function TALDialog.GetContent: TALVertScrollBox;
begin
  If FContent = nil then begin
    FContent := TALVertScrollBox.Create(Container);
    FContent.Parent := Container;
    FContent.Assign(TALDialogManager.Instance.DefaultContent);
    FContent.Name := 'ALDialogContent';
  end;
  Result := FContent;
end;

{*************************************}
function TALDialog.GetMessage: TALText;
begin
  If FMessage = nil then begin
    FMessage := TALText.Create(Content);
    FMessage.Parent := Content;
    FMessage.Assign(TALDialogManager.Instance.DefaultMessage);
    FMessage.Name := 'ALDialogMessage';
  end;
  Result := FMessage;
end;

{********************************************}
function TALDialog.GetButtonBar: TALRectangle;
begin
  If FButtonBar = nil then begin
    FButtonBar := TALRectangle.Create(Container);
    FButtonBar.Parent := Container;
    FButtonBar.Assign(TALDialogManager.Instance.DefaultFooterBar);
    FButtonBar.Name := 'ALDialogButtonBar';
  end;
  Result := FButtonBar;
end;

{***************************************************************}
procedure TALDialog.SetCustomContainer(const AValue: TALControl);
begin
  if FCustomContainer <> AValue then begin
    if FCustomContainer <> nil then
      raise Exception.Create('The custom container has already been set and cannot be changed');
    FCustomContainer := AValue;
    if FCustomContainer.Owner <> Self then
      InsertComponent(FCustomContainer);
    FCustomContainer.parent := Self;
  end;
end;

{***************************************}
function TALDialog.HasContainer: Boolean;
begin
  result := FContainer <> nil;
end;

{**********************************}
function TALDialog.HasIcon: Boolean;
begin
  result := FIcon <> nil;
end;

{**************************************}
function TALDialog.HasHeadline: Boolean;
begin
  result := FHeadline <> nil;
end;

{*************************************}
function TALDialog.HasContent: Boolean;
begin
  result := FContent <> nil;
end;

{*************************************}
function TALDialog.HasMessage: Boolean;
begin
  result := FMessage <> nil;
end;

{***************************************}
function TALDialog.HasButtonBar: Boolean;
begin
  result := FButtonBar <> nil;
end;

{*********************************************}
function TALDialog.HasCustomContainer: Boolean;
begin
  result := FCustomContainer <> nil;
end;

{*****************************************************************************************************************************************}
procedure TALDialog.AddRadioButton(const ALabel: String; const ATag: NativeInt; const AChecked: Boolean; const AMandatory: Boolean = True);
begin
  var LLayout := TALLayout.Create(Content);
  LLayout.Parent := Content;
  LLayout.Assign(TALDialogManager.Instance.DefaultOptionLayout);
  var LRadioButton := TALRadioButton.Create(LLayout);
  LRadioButton.Parent := LLayout;
  LRadioButton.Assign(TALDialogManager.Instance.DefaultRadioButton);
  LRadioButton.Tag := ATag;
  LRadioButton.Checked := AChecked;
  LRadioButton.Mandatory := AMandatory;
  LRadioButton.GroupName := 'ALDialogGroup';
  LRadioButton.Name := 'ALDialogRadioButton'+ALIntToStrW(ATag);
  var LLabel := TALText.Create(LLayout);
  LLabel.Parent := LLayout;
  LLabel.Assign(TALDialogManager.Instance.DefaultLabel);
  LLabel.OnClick := LabelClick;
  LLAbel.TagObject := LRadioButton;
  LLAbel.Text := ALabel;
end;

{****************************************************************************************************}
procedure TALDialog.AddCheckBox(const ALabel: String; const ATag: NativeInt; const AChecked: Boolean);
begin
  var LLayout := TALLayout.Create(Content);
  LLayout.Parent := Content;
  LLayout.Assign(TALDialogManager.Instance.DefaultOptionLayout);
  var LCheckBox := TALCheckBox.Create(LLayout);
  LCheckBox.Parent := LLayout;
  LCheckBox.Assign(TALDialogManager.Instance.DefaultCheckBox);
  LCheckBox.Tag := ATag;
  LCheckBox.Checked := AChecked;
  LCheckBox.Name := 'ALDialogCheckBox'+ALIntToStrW(ATag);
  var LLabel := TALText.Create(LLayout);
  LLabel.Parent := LLayout;
  LLabel.Assign(TALDialogManager.Instance.DefaultLabel);
  LLabel.OnClick := LabelClick;
  LLAbel.TagObject := LCheckBox;
  LLAbel.Text := ALabel;
end;

{*************************************************************************************************************************************}
procedure TALDialog.AddEdit(const APromptText: String; const ALabelText: String; const ASupportingText: String; const ATag: NativeInt);
begin
  var LEdit := TALEdit.Create(Container);
  LEdit.Parent := Container;
  LEdit.Assign(TALDialogManager.Instance.DefaultEdit);
  LEdit.PromptText := APromptText;
  LEdit.LabelText := ALabelText;
  LEdit.SupportingText := ASupportingText;
  LEdit.Tag := ATag;
  LEdit.Name := 'ALDialogEdit'+ALIntToStrW(ATag);
end;

{*************************************************************************************************************************************}
procedure TALDialog.AddMemo(const APromptText: String; const ALabelText: String; const ASupportingText: String; const ATag: NativeInt);
begin
  var LMemo := TALMemo.Create(Container);
  LMemo.Parent := Container;
  LMemo.Assign(TALDialogManager.Instance.DefaultMemo);
  LMemo.PromptText := APromptText;
  LMemo.LabelText := ALabelText;
  LMemo.SupportingText := ASupportingText;
  LMemo.Tag := ATag;
  LMemo.Name := 'ALDialogMemo'+ALIntToStrW(ATag);
end;

{******************************************************************************************************************}
procedure TALDialog.AddButton(const ACaption: String; const ATag: NativeInt; Const AIsFooterButton: Boolean = True);
begin
  if AIsFooterButton then begin
    var LButton := TALButton.Create(ButtonBar);
    LButton.Parent := ButtonBar;
    LButton.Assign(TALDialogManager.Instance.DefaultFooterButton);
    LButton.Text := ACaption;
    LButton.Tag := ATag;
    LButton.OnClick := ActionButtonClick;
    LButton.Name := 'ALDialogButton'+ALIntToStrW(ATag);
  end
  else begin
    var LButton := TALButton.Create(Content);
    LButton.Parent := Content;
    LButton.Assign(TALDialogManager.Instance.DefaultInlineButton);
    LButton.Text := ACaption;
    LButton.Tag := ATag;
    LButton.OnClick := ActionButtonClick;
    LButton.Name := 'ALDialogButton'+ALIntToStrW(ATag);
  end;
end;

{*********************************************************}
function TALDialog.GetRadioButtons: TArray<TALRadioButton>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetRadioButtons(const AControl: TControl);
  begin
    for var I := 0 to AControl.ControlsCount - 1 do begin
      if AControl.Controls[i] is TALRadioButton then begin
        setlength(Result, Length(Result) + 1);
        Result[High(Result)] := TALRadioButton(AControl.Controls[i]);
      end
      else _GetRadioButtons(AControl.Controls[i]);
    end;
  end;

begin
  SetLength(Result, 0);
  If HasContent then
    _GetRadioButtons(Content)
  else if HasCustomContainer then
    _GetRadioButtons(CustomContainer);
end;

{****************************************************}
function TALDialog.GetCheckBoxes: TArray<TALCheckBox>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetCheckBoxes(const AControl: TControl);
  begin
    for var I := 0 to AControl.ControlsCount - 1 do begin
      if AControl.Controls[i] is TALCheckBox then begin
        setlength(Result, Length(Result) + 1);
        Result[High(Result)] := TALCheckBox(AControl.Controls[i]);
      end
      else _GetCheckBoxes(AControl.Controls[i]);
    end;
  end;

begin
  SetLength(Result, 0);
  If HasContent then
    _GetCheckBoxes(Content)
  else if HasCustomContainer then
    _GetCheckBoxes(CustomContainer);
end;

{***********************************************}
function TALDialog.GetEdits: TArray<TALBaseEdit>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetEdits(const AControl: TControl);
  begin
    for var I := 0 to AControl.ControlsCount - 1 do begin
      if AControl.Controls[i] is TALBaseEdit then begin
        setlength(Result, Length(Result) + 1);
        Result[High(Result)] := TALBaseEdit(AControl.Controls[i]);
      end
      else _GetEdits(AControl.Controls[i]);
    end;
  end;

begin
  SetLength(Result, 0);
  If HasContainer then
    _GetEdits(Container)
  else if HasCustomContainer then
    _GetEdits(CustomContainer);
end;

{***********************************************}
function TALDialog.Getbuttons: TArray<TALbutton>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _Getbuttons(const AControl: TControl);
  begin
    for var I := 0 to AControl.ControlsCount - 1 do begin
      if AControl.Controls[i] is TALbutton then begin
        setlength(Result, Length(Result) + 1);
        Result[High(Result)] := TALbutton(AControl.Controls[i]);
      end
      else _Getbuttons(AControl.Controls[i]);
    end;
  end;

begin
  SetLength(Result, 0);
  If HasContainer then
    _Getbuttons(Container)
  else if HasCustomContainer then
    _Getbuttons(CustomContainer);
end;

{***********************************************************************}
function TALDialog.GetRadioButton(const ATag: NativeInt): TALRadioButton;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GetRadioButton(const AControl: TControl): TALRadioButton;
  begin
    Result := nil;
    for var I := 0 to AControl.ControlsCount - 1 do begin
      if (AControl.Controls[i].Tag = ATag) and (AControl.Controls[i] is TALRadioButton) then begin
        Result := TALRadioButton(AControl.Controls[i]);
        exit;
      end
      else begin
        Result := _GetRadioButton(AControl.Controls[i]);
        if Result <> nil then exit;
      end;
    end;
  end;

begin
  If HasContent then
    Result := _GetRadioButton(Content)
  else if HasCustomContainer then
    Result := _GetRadioButton(CustomContainer)
  else
    Result := nil;
end;

{*******************************************************}
function TALDialog.GetCheckedRadioButton: TALRadioButton;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GetCheckedRadioButton(const AControl: TControl): TALRadioButton;
  begin
    Result := nil;
    for var I := 0 to AControl.ControlsCount - 1 do begin
      if (AControl.Controls[i] is TALRadioButton) and (TALRadioButton(AControl.Controls[i]).Checked) then begin
        Result := TALRadioButton(AControl.Controls[i]);
        exit;
      end
      else begin
        Result := _GetCheckedRadioButton(AControl.Controls[i]);
        if Result <> nil then exit;
      end;
    end;
  end;

begin
  If HasContent then
    Result := _GetCheckedRadioButton(Content)
  else if HasCustomContainer then
    Result := _GetCheckedRadioButton(CustomContainer)
  else
    Result := nil;
end;

{*****************************************************************}
function TALDialog.GetCheckBox(const ATag: NativeInt): TALCheckBox;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GetCheckBox(const AControl: TControl): TALCheckBox;
  begin
    Result := nil;
    for var I := 0 to AControl.ControlsCount - 1 do begin
      if (AControl.Controls[i].Tag = ATag) and (AControl.Controls[i] is TALCheckBox) then begin
        Result := TALCheckBox(AControl.Controls[i]);
        exit;
      end
      else begin
        Result := _GetCheckBox(AControl.Controls[i]);
        if Result <> nil then exit;
      end;
    end;
  end;

begin
  If HasContent then
    Result := _GetCheckBox(Content)
  else if HasCustomContainer then
    Result := _GetCheckBox(CustomContainer)
  else
    Result := nil;
end;

{*************************************************************}
function TALDialog.GetEdit(const ATag: NativeInt): TALBaseEdit;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GetEdit(const AControl: TControl): TALBaseEdit;
  begin
    Result := nil;
    for var I := 0 to AControl.ControlsCount - 1 do begin
      if (AControl.Controls[i].Tag = ATag) and (AControl.Controls[i] is TALBaseEdit) then begin
        Result := TALBaseEdit(AControl.Controls[i]);
        exit;
      end
      else begin
        Result := _GetEdit(AControl.Controls[i]);
        if Result <> nil then exit;
      end;
    end;
  end;

begin
  If HasContainer then
    Result := _GetEdit(Container)
  else if HasCustomContainer then
    Result := _GetEdit(CustomContainer)
  else
    Result := nil;
end;

{*************************************************************}
function TALDialog.Getbutton(const ATag: NativeInt): TALbutton;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _Getbutton(const AControl: TControl): TALbutton;
  begin
    Result := nil;
    for var I := 0 to AControl.ControlsCount - 1 do begin
      if (AControl.Controls[i].Tag = ATag) and (AControl.Controls[i] is TALbutton) then begin
        Result := TALbutton(AControl.Controls[i]);
        exit;
      end
      else begin
        Result := _Getbutton(AControl.Controls[i]);
        if Result <> nil then exit;
      end;
    end;
  end;

begin
  If HasContainer then
    Result := _Getbutton(Container)
  else if HasCustomContainer then
    Result := _Getbutton(CustomContainer)
  else
    Result := nil;
end;

{*****************************************************}
procedure TALDialog.ActionButtonClick(Sender: TObject);
begin
  var LCanClose: Boolean := True;
  if Assigned(FOnActionRefProc) then FOnActionRefProc(self, TALControl(Sender).Tag, LCanClose)
  else if Assigned(FOnActionObjProc) then FOnActionObjProc(self, TALControl(Sender).Tag, LCanClose)
  else LCanClose := (not assigned(FCustomDialogRefProc)) and (not assigned(FCustomDialogObjProc));
  If LCanClose then begin
    // One of the callbacks (FOnActionRefProc or FOnActionObjProc) may have asked
    // the dialog manager to present another dialog, which could already have
    // closed this one. Confirm that this dialog is still the active one before
    // calling CloseCurrentDialog.
    if (TALDialogManager.HasInstance) and
       (TALDialogManager.Instance.CurrentDialog = self) then
      TALDialogManager.Instance.CloseCurrentDialog;
  end;
end;

{**********************************************}
procedure TALDialog.LabelClick(Sender: TObject);
begin
  if TALText(Sender).TagObject is TALBaseCheckBox then
    TALBaseCheckBox(TALText(Sender).TagObject).Checked := not TALBaseCheckBox(TALText(Sender).TagObject).Checked;
end;

{**********************************}
constructor TALDialogManager.Create;
begin
  inherited;
  FDefaultScrim := TALRectangle.Create(nil);
  FDefaultContainer := TALRectangle.Create(nil);
  FDefaultIcon := TALImage.Create(nil);
  FDefaultHeadline := TALText.Create(nil);
  FDefaultContent := TALVertScrollBox.Create(nil);
  FDefaultMessage := TALText.Create(nil);
  FDefaultOptionLayout := TALLayout.Create(nil);
  FDefaultRadioButton := TALRadioButton.Create(nil);
  FDefaultCheckBox := TALCheckbox.Create(nil);
  FDefaultInlineButton := TALButton.Create(nil);
  FDefaultEdit := TALDummyEdit.Create(nil);
  FDefaultMemo := TALDummyMemo.Create(nil);
  FDefaultLabel := TALText.Create(nil);
  FDefaultFooterBar := TALRectangle.Create(nil);
  FDefaultFooterButton := TALButton.Create(nil);
  FCurrentDialog := Nil;
  FScrimAnimation := TALFloatAnimation.Create;
  FScrimAnimation.OnProcess := ScrimAnimationProcess;
  FScrimAnimation.OnFinish := ScrimAnimationFinish;
  FContainerAnimation := TALFloatAnimation.Create;
  FContainerAnimation.OnProcess := ContainerAnimationProcess;
  FContainerAnimation.OnFinish := ContainerAnimationFinish;
  FQueue := TQueue<TALDialog>.create;
  Setlength(FFrozenNativeControls, 0);
end;

{**********************************}
destructor TALDialogManager.Destroy;
begin
  AlFreeAndNil(FDefaultScrim);
  AlFreeAndNil(FDefaultContainer);
  AlFreeAndNil(FDefaultIcon);
  AlFreeAndNil(FDefaultHeadline);
  AlFreeAndNil(FDefaultContent);
  AlFreeAndNil(FDefaultMessage);
  AlFreeAndNil(FDefaultOptionLayout);
  AlFreeAndNil(FDefaultRadioButton);
  AlFreeAndNil(FDefaultCheckBox);
  AlFreeAndNil(FDefaultInlineButton);
  AlFreeAndNil(FDefaultEdit);
  AlFreeAndNil(FDefaultMemo);
  AlFreeAndNil(FDefaultLabel);
  AlFreeAndNil(FDefaultFooterBar);
  AlFreeAndNil(FDefaultFooterButton);
  ALFreeAndNil(FScrimAnimation);
  ALFreeAndNil(FContainerAnimation);
  ALFreeAndNil(FQueue);
  inherited;
end;

{*******************************************}
procedure TALDialogManager.AfterConstruction;
begin
  inherited;
  TALStyleManager.Instance.ApplyDialogManagerStyle('Default', Self);
end;

{***************************************************************}
class function TALDialogManager.CreateInstance: TALDialogManager;
begin
  result := TALDialogManager.Create;
end;

{*************}
//[MultiThread]
class function TALDialogManager.GetInstance: TALDialogManager;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALDialogManager.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{*******************************************}
procedure TALDialogManager.FreezeNativeViews;
begin
  For var I := Low(FFrozenNativeControls) to high(FFrozenNativeControls) do
    if FFrozenNativeControls[I] <> nil then
      FFrozenNativeControls[I].RemoveFreeNotify(Self);

  ALFreezeNativeViews(FFrozenNativeControls);

  For var I := Low(FFrozenNativeControls) to high(FFrozenNativeControls) do
    if FFrozenNativeControls[I] <> nil then
      FFrozenNativeControls[I].AddFreeNotify(Self);
end;

{*********************************************}
procedure TALDialogManager.UnfreezeNativeViews;
begin
  For var I := Low(FFrozenNativeControls) to high(FFrozenNativeControls) do
    if FFrozenNativeControls[I] <> nil then
      FFrozenNativeControls[I].RemoveFreeNotify(Self);

  ALUnfreezeNativeViews(FFrozenNativeControls)
end;

{************************************************************}
procedure TALDialogManager.FreeNotification(AObject: TObject);
begin
  For var I := low(FFrozenNativeControls) to high(FFrozenNativeControls) do
    if FFrozenNativeControls[I] = AObject then
      FFrozenNativeControls[I] := nil;

  if FCurrentDialog = AObject then begin
    FScrimAnimation.Enabled := False;
    FContainerAnimation.Enabled := False;
    FCurrentDialog := Nil;
  end;
end;

{***********************************************************}
function TALDialogManager.GetMinDialogContainerWidth: Single;
begin
  // https://m3.material.io/components/dialogs/specs
  // Container width: Min 280dp; Max 560dp
  Result := 280;
end;

{***********************************************************}
function TALDialogManager.GetMaxDialogContainerWidth: Single;
begin
  // https://m3.material.io/components/dialogs/specs
  // Container width: Min 280dp; Max 560dp
  Var LForm := Screen.ActiveForm;
  if LForm = nil then LForm := Application.MainForm;
  if LForm = nil then Raise Exception.Create('Error 829A934F-F778-41AD-ACD8-8AD01B182D4A');
  result := min(560, LForm.ClientWidth / 100 * 85);
end;

{*************************************************}
function TALDialogManager.IsShowingDialog: Boolean;
begin
  Result := FCurrentDialog <> nil;
end;

{**********************************************}
procedure TALDialogManager.CustomDialogFinished;
begin
  CloseCurrentDialog;
end;

{********************************************}
procedure TALDialogManager.CloseCurrentDialog;
begin
  if FCurrentDialog = nil then exit;

  if (not assigned(FCurrentDialog.CustomDialogRefProc)) and
     (not assigned(FCurrentDialog.CustomDialogObjProc)) then begin

    If (not (TALDialog.TAnimateOption.AnimateContainer in FCurrentDialog.CloseAnimateOptions)) then begin
      DoCloseCurrentDialog;
      exit;
    end;

    If not TALDialogManager.Instance.HasPendingDialogs then begin
      FCurrentDialog.HitTest := False;
      FCurrentDialog.FTouchBlocker.Parent := FCurrentDialog.Container;
    end;
    FCurrentDialog.FTouchBlocker.Visible := True;
    FCurrentDialog.Container.Align := TALAlignlayout.None;

    FScrimAnimation.Enabled := False;
    if (not HasPendingDialogs) and
       (TALDialog.TAnimateOption.AnimateScrim in FCurrentDialog.CloseAnimateOptions) then begin
      FScrimAnimation.TagFloat := TAlphaColorRec(FCurrentDialog.Fill.Color).A / 255;
      FScrimAnimation.InterpolationType := TALInterpolationType.Linear;
      FScrimAnimation.Duration := 0.200;
      FScrimAnimation.StartValue := 1;
      FScrimAnimation.StopValue := 0;
      FScrimAnimation.Start;
    end;

    FContainerAnimation.Enabled := False;
    FContainerAnimation.TagFloat := FCurrentDialog.Container.Position.y;
    FContainerAnimation.InterpolationType := TALInterpolationType.Material3EmphasizedAccelerate;
    FContainerAnimation.Duration := 0.200;
    FContainerAnimation.StartValue := 1;
    FContainerAnimation.StopValue := 0;
    FContainerAnimation.Start;

  end
  else
    DoCloseCurrentDialog;
end;

{**********************************************}
procedure TALDialogManager.DoCloseCurrentDialog;
begin
  if FCurrentDialog = nil then exit;
  var LCurrentDialog := FCurrentDialog;
  LCurrentDialog.Visible := False;
  FCurrentDialog := nil;
  FScrimAnimation.Enabled := False;
  FContainerAnimation.Enabled := False;
  if assigned(LCurrentDialog.FOnClosedRefProc) then
    LCurrentDialog.FOnClosedRefProc();
  ProcessPendingDialogs;
  if not IsShowingDialog then
    UnfreezeNativeViews
  else begin
    FScrimAnimation.Stop;
    if assigned(FCurrentDialog.CustomDialogRefProc) or
       assigned(FCurrentDialog.CustomDialogObjProc) then
      FCurrentDialog.Fill.Color := LCurrentDialog.Fill.Color;
  end;
  TThread.ForceQueue(nil,
    procedure
    begin
      ALFreeAndNil(LCurrentDialog);
    end);
end;

{*************}
//[MultiThread]
procedure TALDialogManager.RequestDialog(const ADialog: TALDialog; const AForceImmediateShow: Boolean);
begin
  TThread.queue(nil,
    procedure
    begin
      if (TALLoadingOverlayManager.HasInstance) and
         (TALLoadingOverlayManager.Instance.IsShowingLoadingOverlay) then begin
        TALLoadingOverlay.CloseCurrent(
          procedure
          begin
            if TALDialogManager.HasInstance then begin
              ADialog.ShowAnimateOptions := ADialog.ShowAnimateOptions - [TALDialog.TAnimateOption.AnimateScrim];
              TALDialogManager.Instance.RequestDialog(ADialog, AForceImmediateShow);
            end
            else
              ALFreeAndNil(ADialog);
          end,
          TALLoadingOverlayManager.Instance.CurrentLoadingOverlay.CloseAnimateOptions - [TALLoadingOverlay.TAnimateOption.AnimateScrim])
      end
      else if TALDialogManager.HasInstance then begin
        with TALDialogManager.Instance do begin
          FQueue.Enqueue(ADialog);
          if AForceImmediateShow then begin
            While FQueue.Peek <> ADialog do
              FQueue.Enqueue(FQueue.Dequeue);
            if IsShowingDialog then
              CloseCurrentDialog
            else
              ProcessPendingDialogs;
          end
          else
            ProcessPendingDialogs;
        end;
      end
      else
        ALFreeAndNil(ADialog);
    end);
end;

{***************************************************}
Function TALDialogManager.HasPendingDialogs: Boolean;
begin
  Result := FQueue.Count > 0;
end;

{***********************************************}
procedure TALDialogManager.ProcessPendingDialogs;
begin
  if (not IsShowingDialog) and
     (HasPendingDialogs) then begin
    var LDialog := FQueue.Dequeue;
    try
      ShowDialog(LDialog);
    Except
      ALFreeAndNil(LDialog);
      Raise;
    end;
  end;
end;

{**************************************************************}
procedure TALDialogManager.ShowDialog(const ADialog: TALDialog);
begin

  // Init LForm & LClientWidth
  Var LForm: TCommonCustomForm;
  var LClientHeight: Single;
  if ADialog.ParentControl = nil then begin
    LForm := Screen.ActiveForm;
    if LForm = nil then LForm := Application.MainForm;
    if LForm = nil then Raise Exception.Create('Error 0B1C5551-F59D-46FA-8E9B-A10AB6A65FDE');
    LClientHeight := LForm.ClientHeight;
  end
  else begin
    LForm := nil;
    LClientHeight := ADialog.ParentControl.Height;
  end;

  // Update Width/MaxWith of some elements
  if ADialog.HasHeadline then
    ADialog.Headline.MaxWidth := GetMaxDialogContainerWidth
                                 - ADialog.Headline.Margins.Left
                                 - ADialog.Headline.Margins.Right
                                 - ADialog.Container.padding.Left
                                 - ADialog.Container.padding.Right;
  if ADialog.HasMessage then
    ADialog.Message.MaxWidth := GetMaxDialogContainerWidth
                                - ADialog.Message.Margins.Left
                                - ADialog.Message.Margins.Right
                                - ADialog.Content.Margins.Left
                                - ADialog.Content.Margins.Right
                                - ADialog.Container.padding.Left
                                - ADialog.Container.padding.Right;
  var LEdits := ADialog.GetEdits;
  for Var I := low(LEdits) to High(LEdits) do begin
    var LEdit := LEdits[i];
    LEdit.Width := min(
                     320, GetMaxDialogContainerWidth
                     - LEdit.Margins.Left
                     - LEdit.Margins.Right
                     - ADialog.Container.padding.Left
                     - ADialog.Container.padding.Right);
  end;

  // BeginUpdate was called when the dialog was created.
  // Calling EndUpdate now will adjust the size of all text.
  ADialog.EndUpdate;

  // Align all elements properly.
  ADialog.BeginUpdate;

  // CustomDialogRefProc
  if assigned(ADialog.CustomDialogRefProc) then begin
    ADialog.Fill.Color := TALphaColors.Null;
  end

  // CustomDialogObjProc
  else if assigned(ADialog.CustomDialogObjProc) then begin
    ADialog.Fill.Color := TALphaColors.Null;
  end

  // CustomContainer
  else if ADialog.HasCustomContainer then begin
    if ADialog.ParentControl = nil then LForm.Focused := nil
    else ADialog.ParentControl.ResetFocus;
    FreezeNativeViews;
    if ADialog.HasContainer then
      ADialog.Container.Visible := False;
  end

  // Regular dialog
  else begin

    if ADialog.ParentControl = nil then LForm.Focused := nil
    else ADialog.ParentControl.ResetFocus;
    FreezeNativeViews;
    var LCurrY: Single := 0;
    var LContainerWidth: Single := 0;
    var LContentHeight: Single := 0;
    var LContentWidth: Single := 0;
    var LMarginTopAdjustment: Single := 0;

    // Icon
    if ADialog.HasIcon then begin
      if ADialog.Icon.ResourceName = '' then ADialog.Icon.Visible := False
      else begin
        ADialog.Icon.Position.Y := LCurrY + ADialog.Icon.Margins.top;
        LCurrY := ADialog.Icon.Position.Y + ADialog.Icon.Height + ADialog.Icon.Margins.Bottom;
        LContainerWidth := max(
                             LContainerWidth,
                             ADialog.Container.Padding.Right +
                             ADialog.Container.Padding.Left +
                             ADialog.Icon.Margins.Right +
                             ADialog.Icon.Margins.Left +
                             ADialog.Icon.Width);
      end;
    end;

    // Headline
    if ADialog.HasHeadline then begin
      if ADialog.Headline.Text = '' then ADialog.Headline.Visible := False
      else begin
        if (not ADialog.HasIcon) or (not ADialog.Icon.IsVisible) then
          ADialog.Headline.Margins.top := 0;
        ADialog.Headline.Position.Y := LCurrY + ADialog.Headline.Margins.top;
        LCurrY := ADialog.Headline.Position.Y + ADialog.Headline.Height + ADialog.Headline.Margins.Bottom;
        LContainerWidth := max(
                             LContainerWidth,
                             ADialog.Container.Padding.Right +
                             ADialog.Container.Padding.Left +
                             ADialog.Headline.Margins.Right +
                             ADialog.Headline.Margins.Left +
                             ADialog.Headline.Width);
        if ADialog.HasIcon and ADialog.Icon.Visible then begin
          ADialog.Headline.Align := TALAlignLayout.TopCenter;
          ADialog.Headline.TextSettings.HorzAlign := TALTextHorzAlign.Center;
        end;
      end;
    end;

    // Content
    if ADialog.HasContent then begin

      if ((not ADialog.HasIcon) or (not ADialog.Icon.IsVisible)) and
         ((not ADialog.HasHeadline) or (not ADialog.Headline.IsVisible)) then
        ADialog.Content.Margins.top := 0;

      if ADialog.HasMessage then begin
        ADialog.Message.Index := 0;
        if ADialog.Message.Text = '' then ADialog.Message.Visible := False;
      end;

      if ADialog.Content.Content.ControlsCount > 0 then begin
        for var I := 0 to ADialog.Content.Content.ControlsCount - 1 do begin
          var LItem := ADialog.Content.Content.Controls[i];
          if not LItem.Visible then continue;
          LItem.Margins.top := LItem.TagFloat;
          break;
        end;

        for var I := ADialog.Content.Content.ControlsCount - 1 downto 0 do begin
          var LItem := ADialog.Content.Content.Controls[i];
          if not LItem.Visible then continue;
          LItem.Margins.bottom := LItem.TagFloat;
          LMarginTopAdjustment := -LItem.TagFloat;
          break;
        end;
      end;

      For var I := 0 to ADialog.Content.Content.ControlsCount - 1 do begin
        var LItem := ADialog.Content.Content.Controls[i];
        if not LItem.Visible then Continue;
        LItem.Position.Y := LContentHeight + LItem.Margins.top;
        LContentHeight := LItem.Position.Y + LItem.Height + LItem.Margins.bottom;
        LContentWidth := max(
                           LContentWidth,
                           ADialog.content.Padding.Right +
                           ADialog.content.Padding.Left +
                           LItem.margins.Right +
                           LItem.margins.Left +
                           LItem.Width);
      end;

      ADialog.Content.Position.Y := LCurrY + ADialog.Content.Margins.Top;
      LCurrY := ADialog.Content.Position.Y + LContentHeight + ADialog.Content.Margins.Bottom;
      LContainerWidth := Max(
                           LContainerWidth,
                           ADialog.Container.Padding.Right +
                           ADialog.Container.Padding.Left +
                           ADialog.Content.Margins.Right +
                           ADialog.Content.Margins.Left +
                           LContentWidth);

    end;

    // Edits
    For var I := low(LEdits) to high(LEdits) do begin
      var LEdit := LEdits[i];
      LEdit.Margins.top := LEdit.Margins.top + LMarginTopAdjustment;
      LMarginTopAdjustment := 0;
      LEdit.Position.Y := LCurrY + LEdit.Margins.top;
      LCurrY := LEdit.Position.Y + LEdit.Height + LEdit.Margins.Bottom;
      LContainerWidth := max(
                           LContainerWidth,
                           ADialog.Container.Padding.Right +
                           ADialog.Container.Padding.Left +
                           LEdit.Margins.Right +
                           LEdit.Margins.Left +
                           LEdit.Width);
    end;

    // ButtonBar
    if ADialog.HasButtonBar then begin
      ADialog.ButtonBar.Margins.top := ADialog.ButtonBar.Margins.top + LMarginTopAdjustment;
      //LMarginTopAdjustment := 0;
      If ADialog.ButtonBar.Width > GetMaxDialogContainerWidth
                                   - ADialog.ButtonBar.Margins.Right
                                   - ADialog.ButtonBar.Margins.Left
                                   - ADialog.Container.padding.Right
                                   - ADialog.Container.padding.Left then begin
        var LCurrButtonY: Single := 0;
        For var I := 0 to ADialog.ButtonBar.ControlsCount - 1 do begin
          if ADialog.ButtonBar.Controls[i] is TALControl then begin
            var LButton := TALControl(ADialog.ButtonBar.Controls[i]);
            LButton.Position.Y := LCurrButtonY + LButton.Margins.top;
            LCurrButtonY := LButton.Position.Y + LButton.Height + LButton.Margins.bottom;
            LButton.Align := TALAlignLayout.TopRight;
          end;
        end;
        ADialog.ButtonBar.EndUpdate;
        ADialog.ButtonBar.BeginUpdate;
      end
      else begin
        var LCurrButtonX: Single := 0;
        For var I := ADialog.ButtonBar.ControlsCount - 1 downto 0 do begin
          var LButton := ADialog.ButtonBar.Controls[i];
          LButton.Position.X := LCurrButtonX + LButton.Margins.left;
          LCurrButtonX := LButton.Position.X + LButton.Width + LButton.Margins.Right;
        end;
      end;
      ADialog.ButtonBar.Position.Y := LCurrY + ADialog.ButtonBar.Margins.top;
      LCurrY := ADialog.ButtonBar.Position.Y + ADialog.ButtonBar.Height + ADialog.ButtonBar.Margins.Bottom;
    end;

    // Adjust the container Width
    var LDummyControl := TALLayout.Create(ADialog.Container);
    LDummyControl.Parent := ADialog.Container;
    LDummyControl.Width := max(GetMinDialogContainerWidth, LContainerWidth)
                           - ADialog.Container.padding.Right
                           - ADialog.Container.padding.left;
    LDummyControl.Height := 0;
    LDummyControl.Align := TALAlignLayout.TopLeft;

    // Adjust the Content Height
    if ADialog.HasContent then begin
      // Alert dialogs should not exceed 2/3 (66%) of the screen height.
      // This ensures the dialog remains readable and actionable without forcing excessive scrolling.
      Var LMaxContentHeight := (LClientHeight * (2/3))
                               - ADialog.Container.Padding.Top
                               - ADialog.Container.Padding.bottom
                               - LCurrY + LContentHeight;
      ADialog.Content.Height := min(LContentHeight, LMaxContentHeight);
      ADialog.Content.Width := LDummyControl.Width +
                               + ADialog.Container.padding.Left +
                               + ADialog.Container.padding.Right;
    end;

  end;

  // Attach ADialog
  if ADialog.ParentControl = nil then begin
    // This will defacto call LForm.Realign and ADialog.EndUpdate
    // in TCustomForm.DoAddObject.SetUpdatingState
    LForm.InsertComponent(ADialog);
    ADialog.Parent := LForm;
  end
  else begin
    ADialog.EndUpdate;
  end;

  FCurrentDialog := ADialog;
  FCurrentDialog.AddFreeNotify(Self);

  if assigned(FCurrentDialog.CustomDialogRefProc) then FCurrentDialog.CustomDialogRefProc()
  else if assigned(FCurrentDialog.CustomDialogObjProc) then FCurrentDialog.CustomDialogObjProc()
  else begin

    // ShowAnimateOptions
    if TALDialog.TAnimateOption.AnimateContainer in FCurrentDialog.ShowAnimateOptions then begin

      FCurrentDialog.FTouchBlocker := TALLayout.Create(FCurrentDialog);
      FCurrentDialog.FTouchBlocker.Parent := FCurrentDialog;
      FCurrentDialog.FTouchBlocker.Align := TALAlignLayout.Contents;
      FCurrentDialog.FTouchBlocker.HitTest := True;

      FCurrentDialog.Container.Align := TALAlignlayout.None;
      Var LCurrentDialogCenteredPosY: Single := FCurrentDialog.Container.Position.y;
      FCurrentDialog.Container.Position.y := LCurrentDialogCenteredPosY*0.8;
      FCurrentDialog.Container.Scale.Y := 0;
      FCurrentDialog.Container.Pivot.Point := TpointF.Create(0,0);

      if (TALDialog.TAnimateOption.AnimateScrim in FCurrentDialog.ShowAnimateOptions) then begin
        FScrimAnimation.Enabled := False;
        FScrimAnimation.TagFloat := TAlphaColorRec(FCurrentDialog.Fill.Color).A / 255;
        FScrimAnimation.InterpolationType := TALInterpolationType.Linear;
        FScrimAnimation.Duration := 0.4;
        FScrimAnimation.StartValue := 0;
        FScrimAnimation.StopValue := 1;
        FScrimAnimation.Start;
      end;

      FContainerAnimation.Enabled := False;
      FContainerAnimation.TagFloat := LCurrentDialogCenteredPosY;
      FContainerAnimation.InterpolationType := TALInterpolationType.Material3ExpressiveDefaultSpatial;
      FContainerAnimation.Duration := 0.5;
      FContainerAnimation.StartValue := 0;
      FContainerAnimation.StopValue := 1;
      FContainerAnimation.Start;

    end
    else
      ContainerAnimationFinish(nil);

  end;
end;

{****************************************************************}
procedure TALDialogManager.ScrimAnimationProcess(Sender: TObject);
begin
  if FCurrentDialog = nil then exit;
  FCurrentDialog.Fill.Color := ALSetColorAlpha(FCurrentDialog.Fill.Color, FScrimAnimation.CurrentValue * FScrimAnimation.TagFloat);
end;

{********************************************************************}
procedure TALDialogManager.ContainerAnimationProcess(Sender: TObject);
begin
  if FCurrentDialog = nil then exit;
  FCurrentDialog.Container.Scale.Y := FContainerAnimation.CurrentValue;
  if FContainerAnimation.StopValue < 0.5 then
    FCurrentDialog.Container.Position.y := (FContainerAnimation.TagFloat*0.9) + ((FContainerAnimation.TagFloat - (FContainerAnimation.TagFloat*0.9)) * FContainerAnimation.CurrentValue)
  else
    FCurrentDialog.Container.Position.y := (FContainerAnimation.TagFloat*0.8) + ((FContainerAnimation.TagFloat - (FContainerAnimation.TagFloat*0.8)) * FContainerAnimation.CurrentValue);
  if (FContainerAnimation.StopValue > 0.5) and
     (FContainerAnimation.NormalizedTime > 0.90) then
    FCurrentDialog.FTouchBlocker.Visible := False;
end;

{***************************************************************}
procedure TALDialogManager.ScrimAnimationFinish(Sender: TObject);
begin
  // Nothing to do
end;

{*******************************************************************}
procedure TALDialogManager.ContainerAnimationFinish(Sender: TObject);
begin
  if FCurrentDialog = nil then exit;
  if FContainerAnimation.StopValue < 0.5 then DoCloseCurrentDialog
  else begin
    FCurrentDialog.Container.Align := TalAlignLayout.Center;
    FCurrentDialog.FTouchBlocker.Visible := false;
  end;
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Dialogs','initialization');
  {$ENDIF}
  TALDialogManager.FInstance := nil;
  TALDialogManager.CreateInstanceFunc := @TALDialogManager.CreateInstance;

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Dialogs','finalization');
  {$ENDIF}
  ALFreeAndNil(TALDialogManager.FInstance);

end.
