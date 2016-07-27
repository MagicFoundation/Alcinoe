unit ALFmxStdCtrls;

interface

uses System.Classes,
     System.Types,
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     FMX.types3D,
     {$ENDIF}
     System.UITypes,
     System.ImageList,
     FMX.Controls,
     FMX.Graphics,
     FMX.StdCtrls,
     FMX.actnlist,
     FMX.ImgList;

type

  //TTrackBar
  //  TLayout
  //    TStyleObject (ftrack - horizontal)
  //      TStyleObject (fTrackHighlight)
  //        TRectangle
  //    TStyleObject (ftrack - vertical)
  //      TStyleObject (fTrackHighlight)
  //        TRectangle
  //    TThumb
  //      TbuttonStyleObject

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMinThumbTrackBar = class;
  TALMaxThumbTrackBar = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBGTrackBar = Class(TTrackBar)
  private
    [weak] fMinThumbTrackBar: TALMinThumbTrackBar;
    [weak] fMaxThumbTrackBar: TALMaxThumbTrackBar;
  protected
    procedure Resize; override;
    procedure ApplyStyle; override;
    procedure DoRealign; override;
    procedure UpdateHighlight;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMinThumbTrackBar = Class(TTrackBar)
  private
    [weak] fBGTrackBar: TALBGTrackBar;
    [weak] fMaxThumbTrackBar: TALMaxThumbTrackBar;
    procedure OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseLeave(Sender: TObject);
    procedure OnThumbMouseEnter(Sender: TObject);
    procedure onThumbApplyStyleLookup(Sender: TObject);
  protected
    procedure Resize; override;
    procedure ApplyStyle; override;
    procedure DoChanged; override;
    procedure DoTracking; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMaxThumbTrackBar = Class(TTrackBar)
  private
    [weak] fBGTrackBar: TALBGTrackBar;
    [weak] fMinThumbTrackBar: TALMinThumbTrackBar;
    procedure OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseLeave(Sender: TObject);
    procedure OnThumbMouseEnter(Sender: TObject);
    procedure onThumbApplyStyleLookup(Sender: TObject);
  protected
    procedure Resize; override;
    procedure ApplyStyle; override;
    procedure DoChanged; override;
    procedure DoTracking; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALRangeTrackBar = class(TControl)
  private
    fBGTrackBar: TALBGTrackBar;
    fMinThumbTrackBar: TALMinThumbTrackBar;
    fMaxThumbTrackBar: TALMaxThumbTrackBar;
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    function GetMax: Single;
    function GetMin: Single;
    function GetFrequency: Single;
    function GetMaxValue: Single;
    function GetMinValue: Single;
    function GetOrientation: TOrientation;
    function GetSmallChange: Single;
    procedure SetFrequency(const Value: Single);
    procedure SetMaxValue(const Value: Single);
    procedure SetMinValue(const Value: Single);
    procedure SetOrientation(const Value: TOrientation);
    procedure SetSmallChange(const Value: Single);
    procedure SetonApplyStyleLookup(const Value: TNotifyEvent);
    function GetOnApplyStyleLookup: TNotifyEvent;
    function GetStyleLookup: string;
    procedure SetStyleLookup(const Value: string);
  protected
    FOnChange, FOnTracking, fonThumbApplyStyleLookup: TNotifyEvent;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Frequency: Single read GetFrequency write SetFrequency;
    property SmallChange: Single read GetSmallChange write SetSmallChange;
    property Min: Single read GetMin write SetMin;
    property Max: Single read GetMax write SetMax;
    property MinValue: Single read GetMinValue write SetMinValue;
    property MaxValue: Single read GetMaxValue write SetMaxValue;
    property Orientation: TOrientation read GetOrientation write SetOrientation;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTracking: TNotifyEvent read FOnTracking write FOnTracking;
    property onThumbApplyStyleLookup: TNotifyEvent read fonThumbApplyStyleLookup write fonThumbApplyStyleLookup;
    property onApplyStyleLookup: TNotifyEvent read GetOnApplyStyleLookup write SetonApplyStyleLookup;
    property StyleLookup: string read GetStyleLookup write SetStyleLookup;
    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Locked;
    property Height;
    //property HitTest default False;
    property Padding;
    property Opacity;
    property Margins;
    //property PopupMenu;
    property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Scale;
    property Size;
    //property TouchTargetExpansion;
    property Visible;
    property Width;
    property TabOrder;
    property TabStop;
    { Events }
    //property OnPainting;
    //property OnPaint;
    property OnResize;
    { Drag and Drop events }
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    { Mouse events }
    //property OnClick;
    //property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    //property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckBox = class(TControl, IGlyph)
  public const
    DesignBorderColor = $A080D080;
  private
    fdoubleBuffered: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    [weak] fBufImages: TCustomImageList;
    FbufImageIndex: TImageIndex;
    //-----
    FPressing: Boolean;
    FOnChange: TNotifyEvent;
    FIsPressed: Boolean;
    FIsChecked: Boolean;
    FImageCheckedLink: TImageLink;
    FImageUncheckedLink: TImageLink;
    FisImagesChanged: boolean;
    procedure SetdoubleBuffered(const Value: Boolean);
    function GetIsChecked: Boolean;
    procedure SetIsChecked(const Value: Boolean);
    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);
    { IGlyph }
    function GetImageIndex: TImageIndex;
    procedure SetImageIndex(const Value: TImageIndex);
    function GetImageUncheckedIndex: TImageIndex;
    procedure SetImageUncheckedIndex(const Value: TImageIndex);
    function GetImageList: TBaseImageList; inline;
    procedure SetImageList(const Value: TBaseImageList);
    function IGlyph.GetImages = GetImageList;
    procedure IGlyph.SetImages = SetImageList;
    procedure NonBufferedPaint;
  protected
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
    procedure DoEndUpdate; override;
    procedure DoChanged; virtual;
    function ImageCheckedIndexStored: Boolean; virtual;
    function ImageUncheckedIndexStored: Boolean; virtual;
    function ImagesStored: Boolean; virtual;
    function GetDefaultSize: TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
    procedure ImagesChanged;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default false;
    property Action;
    property Align;
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DisableFocusEffect;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled;
    property Locked default False;
    property Height;
    property Hint;
    property HitTest default True;
    property IsChecked: Boolean read GetIsChecked write SetIsChecked default False;
    property ImageCheckedIndex: TImageIndex read GetImageIndex write SetImageIndex stored ImageCheckedIndexStored;
    property ImageUncheckedIndex: TImageIndex read GetImageUncheckedIndex write SetImageUncheckedIndex stored ImageUncheckedIndexStored;
    property Images: TCustomImageList read GetImages write SetImages stored ImagesStored;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property ParentShowHint;
    property ShowHint;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnKeyDown;
    property OnKeyUp;
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

procedure Register;

implementation

uses System.SysUtils,
     System.Math,
     system.Math.Vectors,
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     FMX.Canvas.GPU,
     {$ENDIF}
     fmx.consts,
     fmx.utils,
     FMX.Types,
     AlfmxCommon;

{*********************************}
procedure TALBGTrackBar.ApplyStyle;
begin
  inherited;
  if assigned(FTrackHighlight) then FTrackHighlight.Align := TALignLayout.None;
  if assigned(Thumb) then Thumb.Visible := False;
  TALRangeTrackBar(Parent).MinClipWidth := MinClipWidth;
  TALRangeTrackBar(Parent).MinClipHeight := MinClipHeight;
  UpdateHighlight;
end;

{********************************}
procedure TALBGTrackBar.DoRealign;
begin
  inherited;
  if assigned(Thumb) then Thumb.Visible := False;
end;

{*****************************}
procedure TALBGTrackBar.Resize;
begin
  inherited;
  case Orientation of
    TOrientation.Horizontal: if height <> TALRangeTrackBar(Parent).width then TALRangeTrackBar(Parent).Height := height;
    TOrientation.Vertical: if width <> TALRangeTrackBar(Parent).width then TALRangeTrackBar(Parent).width := width;
  end;
end;

{**************************************}
procedure TALBGTrackBar.UpdateHighlight;
begin
  if FTrackHighlight <> nil then
  begin
    case Orientation of
      TOrientation.Horizontal: begin
        if (fMinThumbTrackBar.Thumb <> nil) and (fMaxThumbTrackBar.Thumb <> nil) then begin
          FTrackHighlight.position.X := fMinThumbTrackBar.GetThumbRect.Left - fMinThumbTrackBar.Thumb.Margins.Left;
          FTrackHighlight.Width := Round((fMaxThumbTrackBar.GetThumbRect.Left + fMaxThumbTrackBar.GetThumbRect.Right) / 2) - FTrackHighlight.position.X;
        end
        else begin
          FTrackHighlight.position.X := 0;
          FTrackHighlight.Width := 0;
        end;
      end;
      TOrientation.Vertical: begin
        if (fMinThumbTrackBar.Thumb <> nil) and (fMaxThumbTrackBar.Thumb <> nil) then begin
          FTrackHighlight.position.y := fMinThumbTrackBar.GetThumbRect.Top - fMinThumbTrackBar.Thumb.Margins.Top;
          FTrackHighlight.Height := Round((fMaxThumbTrackBar.GetThumbRect.Top + fMaxThumbTrackBar.GetThumbRect.Bottom) / 2) - FTrackHighlight.position.y;
        end
        else begin
          FTrackHighlight.position.y := 0;
          FTrackHighlight.Height := 0;
        end;
      end;
    end;
  end;
end;

{**********************************************************************************************************************}
procedure TALMinThumbTrackBar.OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  BringToFront;
  if assigned(TALRangeTrackBar(Parent).OnMouseDown) then
    TALRangeTrackBar(Parent).OnMouseDown(Parent, Button, Shift, X, Y);
end;

{********************************************************************************************************************}
procedure TALMinThumbTrackBar.OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseUp) then
    TALRangeTrackBar(Parent).OnMouseUp(Parent, Button, Shift, X, Y);
end;

{************************************************************************************************}
procedure TALMinThumbTrackBar.OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseMove) then
    TALRangeTrackBar(Parent).OnMouseMove(Parent, Shift, X, Y);
end;

{***************************************************************}
procedure TALMinThumbTrackBar.OnThumbMouseLeave(Sender: TObject);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseLeave) then
    TALRangeTrackBar(Parent).OnMouseLeave(Parent);
end;

{***************************************************************}
procedure TALMinThumbTrackBar.OnThumbMouseEnter(Sender: TObject);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseEnter) then
    TALRangeTrackBar(Parent).OnMouseEnter(Parent);
end;

{*********************************************************************}
procedure TALMinThumbTrackBar.onThumbApplyStyleLookup(Sender: TObject);
begin

  // TThumb
  //   TbuttonStyleObject

  if assigned(TALRangeTrackBar(Parent).onThumbApplyStyleLookup) then
    TALRangeTrackBar(Parent).onThumbApplyStyleLookup(Sender);

  if (value >= fMaxThumbTrackBar.Max) and
     (assigned(Thumb)) then BringToFront
  else if (value <= Min) and
          (assigned(fMaxThumbTrackBar.Thumb)) then fMaxThumbTrackBar.BringToFront;


end;

{***************************************}
procedure TALMinThumbTrackBar.ApplyStyle;
begin
  inherited;
  if assigned(FTrack) then FTrack.visible := False;
  if assigned(Thumb) then begin
    Thumb.OnMouseDown := OnThumbMouseDown;
    Thumb.OnMouseUp := OnThumbMouseUp;
    Thumb.OnMouseMove := OnThumbMouseMove;
    Thumb.OnMouseLeave := OnThumbMouseLeave;
    Thumb.OnMouseEnter := OnThumbMouseEnter;
    Thumb.OnApplyStyleLookup := onThumbApplyStyleLookup;
  end;
  fBGTrackBar.UpdateHighlight;
end;

{***********************************}
procedure TALMinThumbTrackBar.Resize;
begin
  inherited;
  fBGTrackBar.UpdateHighlight;
end;

{***************************************}
procedure TALMinThumbTrackBar.DoTracking;
begin
  if Value > fMaxThumbTrackBar.value then Value := fMaxThumbTrackBar.value
  else begin
    inherited;
    if not (csLoading in ComponentState) and Assigned(TALRangeTrackBar(parent).FOnTracking) then
      TALRangeTrackBar(parent).FOnTracking(parent);
    fBGTrackBar.UpdateHighlight;
  end;
end;

{**************************************}
procedure TALMinThumbTrackBar.DoChanged;
begin
  inherited;
  if not (csLoading in ComponentState) and Assigned(TALRangeTrackBar(parent).FOnChange) then
    TALRangeTrackBar(parent).FOnChange(parent);
  fBGTrackBar.UpdateHighlight;
end;

{**********************************************************************************************************************}
procedure TALMaxThumbTrackBar.OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  BringToFront;
  if assigned(TALRangeTrackBar(Parent).OnMouseDown) then
    TALRangeTrackBar(Parent).OnMouseDown(Parent, Button, Shift, X, Y);
end;

{********************************************************************************************************************}
procedure TALMaxThumbTrackBar.OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseUp) then
    TALRangeTrackBar(Parent).OnMouseUp(Parent, Button, Shift, X, Y);
end;

{************************************************************************************************}
procedure TALMaxThumbTrackBar.OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseMove) then
    TALRangeTrackBar(Parent).OnMouseMove(Parent, Shift, X, Y);
end;

{***************************************************************}
procedure TALMaxThumbTrackBar.OnThumbMouseLeave(Sender: TObject);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseLeave) then
    TALRangeTrackBar(Parent).OnMouseLeave(Parent);
end;

{***************************************************************}
procedure TALMaxThumbTrackBar.OnThumbMouseEnter(Sender: TObject);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseEnter) then
    TALRangeTrackBar(Parent).OnMouseEnter(Parent);
end;

{*********************************************************************}
procedure TALMaxThumbTrackBar.onThumbApplyStyleLookup(Sender: TObject);
begin

  // TThumb
  //   TbuttonStyleObject

  if assigned(TALRangeTrackBar(Parent).onThumbApplyStyleLookup) then
    TALRangeTrackBar(Parent).onThumbApplyStyleLookup(Sender);

  if (value <= fMinThumbTrackBar.min) and
     (assigned(Thumb)) then BringToFront
  else if (value >= max) and
          (assigned(fMinThumbTrackBar.Thumb)) then fMinThumbTrackBar.BringToFront;

end;

{***************************************}
procedure TALMaxThumbTrackBar.ApplyStyle;
begin
  inherited;
  if assigned(FTrack) then FTrack.visible := False;
  if assigned(Thumb) then begin
    Thumb.OnMouseDown := OnThumbMouseDown;
    Thumb.OnMouseUp := OnThumbMouseUp;
    Thumb.OnMouseMove := OnThumbMouseMove;
    Thumb.OnMouseLeave := OnThumbMouseLeave;
    Thumb.OnMouseEnter := OnThumbMouseEnter;
    Thumb.OnApplyStyleLookup := onThumbApplyStyleLookup;
  end;
  fBGTrackBar.UpdateHighlight;
end;

{***********************************}
procedure TALMaxThumbTrackBar.Resize;
begin
  inherited;
  fBGTrackBar.UpdateHighlight;
end;

{***************************************}
procedure TALMaxThumbTrackBar.DoTracking;
begin
  if Value < fMinThumbTrackBar.value then value := fMinThumbTrackBar.value
  else begin
    inherited;
    if not (csLoading in ComponentState) and Assigned(TALRangeTrackBar(parent).FOnTracking) then
      TALRangeTrackBar(parent).FOnTracking(parent);
    fBGTrackBar.UpdateHighlight;
  end;
end;

{**************************************}
procedure TALMaxThumbTrackBar.DoChanged;
begin
  inherited;
  if not (csLoading in ComponentState) and Assigned(TALRangeTrackBar(parent).FOnChange) then
    TALRangeTrackBar(parent).FOnChange(parent);
  fBGTrackBar.UpdateHighlight;
end;

{******************************************************}
constructor TALRangeTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  //-----
  CanParentFocus := True;
  HitTest := False;
  //-----
  FOnChange := nil;
  FOnTracking := nil;
  fonThumbApplyStyleLookup := nil;
  //-----
  fBGTrackBar := TALBGTrackBar.Create(self);
  //-----
  Width := fBGTrackBar.Width;
  height := fBGTrackBar.Height;
  //-----
  fBGTrackBar.Stored := False;
  fBGTrackBar.Parent := Self;
  fBGTrackBar.Align := TalignLayout.Client;
  fBGTrackBar.HitTest := False;
  //-----
  fMaxThumbTrackBar := TALMaxThumbTrackBar.Create(self);
  fMaxThumbTrackBar.Stored := False;
  fMaxThumbTrackBar.Parent := Self;
  fMaxThumbTrackBar.Align := TalignLayout.Client;
  fMaxThumbTrackBar.HitTest := False;
  //-----
  fMinThumbTrackBar := TALMinThumbTrackBar.Create(self);
  fMinThumbTrackBar.Stored := False;
  fMinThumbTrackBar.Parent := Self;
  fMinThumbTrackBar.Align := TalignLayout.Client;
  fMinThumbTrackBar.HitTest := False;
  //-----
  fBGTrackBar.fMinThumbTrackBar := fMinThumbTrackBar;
  fBGTrackBar.fMaxThumbTrackBar := fMaxThumbTrackBar;
  //-----
  fMinThumbTrackBar.fBGTrackBar := fBGTrackBar;
  fMinThumbTrackBar.fMaxThumbTrackBar := fMaxThumbTrackBar;
  //-----
  fMaxThumbTrackBar.fBGTrackBar := fBGTrackBar;
  fMaxThumbTrackBar.fMinThumbTrackBar := fMinThumbTrackBar;
  //-----
  SetMin(fMinThumbTrackBar.Min);
  SetMax(fMaxThumbTrackBar.Max);
end;

{*******************************}
procedure TALRangeTrackBar.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{**********************************}
destructor TALRangeTrackBar.Destroy;
begin
  freeandnil(fMaxThumbTrackBar);
  freeandnil(fMinThumbTrackBar);
  freeandnil(fBGTrackBar);
  inherited;
end;

{********************************}
procedure TALRangeTrackBar.Resize;
begin
  inherited;
  case Orientation of
    TOrientation.Horizontal: if height <> fbgTrackBar.Height then height := fbgTrackBar.Height;
    TOrientation.Vertical: if Width <> fbgTrackBar.Width then Width := fbgTrackBar.Width;
  end;
end;

{***************************************}
function TALRangeTrackBar.GetMax: Single;
begin
  result := fMaxThumbTrackBar.Max;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMax(const Value: Single);
begin
  fBGTrackBar.Max := Value;
  fMinThumbTrackBar.Max := Value;
  fMaxThumbTrackBar.Max := Value;
  //-----
  fMinThumbTrackBar.Value := Min;
  fMaxThumbTrackBar.Value := max;
end;

{***************************************}
function TALRangeTrackBar.GetMin: Single;
begin
  result := fMinThumbTrackBar.Min;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMin(const Value: Single);
begin
  fBGTrackBar.Min := Value;
  fMinThumbTrackBar.Min := Value;
  fMaxThumbTrackBar.Min := Value;
  //-----
  fMinThumbTrackBar.Value := Min;
  fMaxThumbTrackBar.Value := max;
end;

{*********************************************}
function TALRangeTrackBar.GetFrequency: Single;
begin
  result := fBGTrackBar.Frequency;
end;

{***********************************************************}
procedure TALRangeTrackBar.SetFrequency(const Value: Single);
begin
  fBGTrackBar.Frequency := Value;
  fMaxThumbTrackBar.Frequency := Value;
  fMinThumbTrackBar.Frequency := Value;
end;

{********************************************}
function TALRangeTrackBar.GetMaxValue: Single;
begin
  result := fMaxThumbTrackBar.Value;
end;

{**********************************************************}
procedure TALRangeTrackBar.SetMaxValue(const Value: Single);
begin
  fMaxThumbTrackBar.value := Value;
end;

{********************************************}
function TALRangeTrackBar.GetMinValue: Single;
begin
  result := fMinThumbTrackBar.Value;
end;

{**********************************************************}
procedure TALRangeTrackBar.SetMinValue(const Value: Single);
begin
  fMinThumbTrackBar.value := Value;
end;

{************************************************************}
function TALRangeTrackBar.GetOnApplyStyleLookup: TNotifyEvent;
begin
  result := fbgTrackBar.OnApplyStyleLookup;
end;

{**************************************************************************}
procedure TALRangeTrackBar.SetonApplyStyleLookup(const Value: TNotifyEvent);
begin
  fbgTrackBar.OnApplyStyleLookup := Value;
  fMaxThumbTrackBar.OnApplyStyleLookup := Value;
  fMinThumbTrackBar.OnApplyStyleLookup := Value;
end;

{*****************************************************}
function TALRangeTrackBar.GetOrientation: TOrientation;
begin
  result := fBGTrackBar.Orientation;
end;

{*******************************************************************}
procedure TALRangeTrackBar.SetOrientation(const Value: TOrientation);
begin
  fBGTrackBar.Orientation := Value;
  fMaxThumbTrackBar.Orientation := Value;
  fMinThumbTrackBar.Orientation := Value;
end;

{***********************************************}
function TALRangeTrackBar.GetSmallChange: Single;
begin
  result := fBGTrackBar.SmallChange;
end;

{*************************************************************}
procedure TALRangeTrackBar.SetSmallChange(const Value: Single);
begin
  fBGTrackBar.SmallChange := Value;
  fMaxThumbTrackBar.SmallChange := Value;
  fMinThumbTrackBar.SmallChange := Value;
end;

{***********************************************}
function TALRangeTrackBar.GetStyleLookup: string;
begin
  result := fBGTrackBar.StyleLookup;
end;

{*************************************************************}
procedure TALRangeTrackBar.SetStyleLookup(const Value: string);
begin
  fBGTrackBar.StyleLookup := Value;
  fMaxThumbTrackBar.StyleLookup := Value;
  fMinThumbTrackBar.StyleLookup := Value;
end;

{*************************************************}
constructor TALCheckbox.Create(AOwner: TComponent);
begin
  inherited;
  fdoubleBuffered := false;
  fBufBitmap := nil;
  SetAcceptsControls(False);
  CanFocus := True;
  AutoCapture := True;
  FPressing:= false;
  FOnChange := nil;
  FIsPressed := False;
  FIsChecked := False;
  FImageCheckedLink := TGlyphImageLink.Create(Self);
  FImageUncheckedLink := TGlyphImageLink.Create(Self);
  FisImagesChanged := False;
end;

{*****************************}
destructor TALCheckbox.Destroy;
begin
  clearBufBitmap;
  FImageCheckedLink.DisposeOf;
  FImageUncheckedLink.DisposeOf;
  inherited;
end;

{***********************************}
procedure TALCheckbox.clearBufBitmap;
begin
  if fBufBitmap <> nil then begin
    fBufBitmap.Free;
    fBufBitmap := nil;
  end;
end;

{******************************}
procedure TALCheckbox.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  Repaint;
end;

{********************************}
procedure TALCheckbox.DoEndUpdate;
begin
  inherited;
  if FisImagesChanged then
    repaint;
end;

{**********************************}
procedure TALCheckbox.ImagesChanged;
begin
  if ([csLoading, csDestroying, csUpdating] * ComponentState = []) and not IsUpdating then begin
    repaint;
    FisImagesChanged := False;
  end
  else FisImagesChanged := True;
end;

{**************************************************************************************}
procedure TALCheckbox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft then
  begin
    FPressing := True;
    FIsPressed := True;
    StartTriggerAnimation(Self, 'IsPressed');
  end;
end;

{****************************************************************}
procedure TALCheckbox.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (ssLeft in Shift) and (FPressing) then
  begin
    if FIsPressed <> LocalRect.Contains(PointF(X, Y)) then
    begin
      FIsPressed := LocalRect.Contains(PointF(X, Y));
      StartTriggerAnimation(Self, 'IsPressed');
    end;
  end;
end;

{************************************************************************************}
procedure TALCheckbox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if FPressing then
  begin
    inherited;
    FPressing := False;
    FIsPressed := False;

    if LocalRect.Contains(PointF(X, Y)) then
    begin
      IsChecked := not IsChecked;
    end
  end;
end;

{*********************************************************************************************}
procedure TALCheckbox.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  inherited;
  if (KeyChar = ' ') then
  begin
    Click; // Emulate mouse click to perform Action.OnExecute
    IsChecked := not IsChecked;
    KeyChar := #0;
  end;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALCheckbox.MakeBufBitmap: TTexture;
{$ELSE}
function TALCheckbox.MakeBufBitmap: Tbitmap;
{$ENDIF}

var aImageIndex: TimageIndex;
    {$IF defined(ANDROID) or defined(IOS)}
    aSceneScale: Single;
    aBitmap: TBitmap;
    aBitmapSize: TSize;
    aBitmapData: TBitmapData;
    {$ENDIF}

begin

  if IsChecked then aImageIndex := ImageCheckedIndex
  else aImageIndex := ImageUncheckedIndex;

  if ([csLoading, csDestroying, csDesigning] * ComponentState <> []) or
     (not fdoubleBuffered) or
     (Scene = nil) or
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     (Images = nil) or
     (aImageIndex = -1) then begin
    clearBufBitmap;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) and
     (fBufImages = Images) and
     (FbufImageIndex = aImageIndex) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;
  fBufImages := Images;
  FbufImageIndex := aImageIndex;

  {$IF defined(ANDROID) or defined(IOS)}

  //init aSceneScale
  if Scene <> nil then aSceneScale := Scene.GetSceneScale
  else aSceneScale := 1;

  //init aBitmapSize / aBitmap / fBufBitmapRect
  aBitmapSize := TSize.Create(0, 0);
  aBitmap := nil;
  fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, aSceneScale); // to have the pixel aligned width and height
  if (Images <> nil) and
     (fBufBitmapRect.Width >= 1) and
     (fBufBitmapRect.Height >= 1) and
     (aImageIndex <> -1) and
     ([csLoading, csUpdating, csDestroying] * Images.ComponentState = []) then begin
    aBitmapSize := TSize.Create(Round(fBufBitmapRect.Width * aSceneScale), Round(fBufBitmapRect.Height * aSceneScale));
    Images.BestSize(aImageIndex, aBitmapSize);
    aBitmap := Images.Bitmap(aBitmapSize, aImageIndex)
  end;

  if aBitmap <> nil then begin

    //init fBufBitmapRect
    fBufBitmapRect := TRectF.Create(0,
                                    0,
                                    aBitmap.Width / aSceneScale,
                                    aBitmap.Height/ aSceneScale).CenterAt(fBufBitmapRect);

    //convert the aBitmapSurface to texture
    //it's important to make a copy of the aBitmap because it's could be destroyed by the TimageList if
    //their is not anymore enalf of place in it's own caching system
    fBufBitmap := TTexture.Create;
    try
      fBufBitmap.Assign(aBitmap);
      //
      //i don't understand the sheet they do in TTexture.assign! they don't copy the data in the TTexture !
      //
      //procedure TTexture.Assign(Source: TPersistent);
      //begin
      //  ...
      //  if not (TCanvasStyle.NeedGPUSurface in TBitmap(Source).CanvasClass.GetCanvasStyle) then
      //    begin
      //    if TBitmap(Source).Map(TMapAccess.Read, M) then
      //    try
      //      UpdateTexture(M.Data, M.Pitch);
      //    finally
      //      TBitmap(Source).Unmap(M);
      //    end;
      //  end;
      //  ...
      //end;
      //
      {$IF CompilerVersion <> 31}
        {$MESSAGE WARN 'Check if FMX.Types3D.TTexture.assign is still not copying the bitmap data in the TTexture if TCanvasStyle.NeedGPUSurface and adjust the IFDEF'}
      {$ENDIF}
      if (TCanvasStyle.NeedGPUSurface in aBitmap.CanvasClass.GetCanvasStyle) then begin
        if aBitmap.Map(TMapAccess.Read, aBitmapData) then begin
          try
            fBufBitmap.UpdateTexture(aBitmapData.Data, aBitmapData.Pitch);
          finally
            aBitmap.Unmap(aBitmapData);
          end;
        end;
      end;
    except
      fBufBitmap.Free;
      fBufBitmap := nil;
      raise;
    end;

  end;

  {$ENDIF}

  result := fBufBitmap;

end;

{*************************************}
procedure TALCheckbox.NonBufferedPaint;
const
  MinCrossSize = 3;
  MaxCrossSize = 13;
var
  TextRect, ImgRect, BitmapRect: TRectF;
  CrossSize, ScreenScale: Single;
  Bitmap: TBitmap;
  BitmapSize: TSize;
  aImageIndex: TimageIndex;
begin
  if IsChecked then aImageIndex := ImageCheckedIndex
  else aImageIndex := ImageUncheckedIndex;
  if [csLoading, csDestroying] * ComponentState = [] then
  begin
    BitmapSize := TSize.Create(0, 0);
    Bitmap := nil;
    ImgRect := LocalRect;
    if Scene <> nil then
      ScreenScale := Scene.GetSceneScale
    else
      ScreenScale := 1;
    if (Images <> nil) and (ImgRect.Width >= 1) and (ImgRect.Height >= 1) and (aImageIndex <> -1) and
      ([csLoading, csUpdating, csDestroying] * Images.ComponentState = []) then
    begin
      BitmapSize := TSize.Create(Round(ImgRect.Width * ScreenScale), Round(ImgRect.Height * ScreenScale));
      Images.BestSize(aImageIndex, BitmapSize);
      Bitmap := Images.Bitmap(BitmapSize, aImageIndex)
    end;
    if (csDesigning in ComponentState) and not Locked then
      DrawDesignBorder(DesignBorderColor, DesignBorderColor);
    if Bitmap <> nil then
    begin
      BitmapRect := TRectF.Create(0, 0, Bitmap.Width, Bitmap.Height);
      ImgRect := TRectF.Create(CenteredRect(ImgRect.Round, TRectF.Create(0, 0, Bitmap.Width / ScreenScale,
        Bitmap.Height/ ScreenScale).Round));
      Canvas.DrawBitmap(Bitmap, BitmapRect, ImgRect, AbsoluteOpacity, True);
    end;
    if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
    begin
      TextRect := LocalRect;
      TextRect.Inflate(0.5, 0.5);
      Canvas.Stroke.Kind := TBrushKind.Solid;
      Canvas.Stroke.Color := TAlphaColorRec.Darkgray;
      Canvas.Stroke.Dash := TStrokeDash.Solid;
      CrossSize := Trunc(Min(MaxCrossSize, Min(TextRect.Width, TextRect.Height) / 6)) + 1;
      if CrossSize >= MinCrossSize then
      begin
        TextRect.TopLeft.Offset(2, 2);
        TextRect.BottomRight := TextRect.TopLeft;
        TextRect.BottomRight.Offset(CrossSize, CrossSize);
        if Bitmap = nil then
        begin
          if Images = nil then
            Canvas.Stroke.Color := TAlphaColorRec.Red;
          Canvas.DrawLine(TextRect.TopLeft, TextRect.BottomRight, AbsoluteOpacity);
          Canvas.DrawLine(TPointF.Create(TextRect.Right, TextRect.Top), TPointF.Create(TextRect.Left, TextRect.Bottom),
            AbsoluteOpacity);
          TextRect := TRectF.Create(TextRect.Left, TextRect.Bottom, Width, Height);
        end;
        if aImageIndex <> -1 then
        begin
          Canvas.Font.Family := 'Small Font';
          Canvas.Font.Size := 7;
          TextRect.Bottom := TextRect.Top + Canvas.TextHeight(Inttostr(aImageIndex));
          if TextRect.Bottom <= Height then
          begin
            Canvas.Fill.Color := TAlphaColorRec.Darkgray;
            Canvas.FillText(TextRect, Inttostr(aImageIndex), False, AbsoluteOpacity, [], TTextAlign.Leading,
              TTextAlign.Leading);
          end;
        end;
      end;
    end;
  end;
end;

{**************************}
procedure TALCheckbox.Paint;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    NonBufferedPaint;
    exit;
  end;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

end;

{************************************************************}
procedure TALCheckbox.SetdoubleBuffered(const Value: Boolean);
begin
  if Value <> fDoubleBuffered then begin
    fDoubleBuffered := value;
    if not fDoubleBuffered then clearbufBitmap;
  end;
end;

{************************************************}
function TALCheckbox.GetImageList: TBaseImageList;
begin
  Result := GetImages;
end;

{**************************************************************}
procedure TALCheckbox.SetImageList(const Value: TBaseImageList);
begin
  ValidateInheritance(Value, TCustomImageList);
  SetImages(TCustomImageList(Value));
end;

{******************************************}
function TALCheckbox.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(22, 22);
end;

{**********************************************}
function TALCheckbox.GetImageIndex: TImageIndex;
begin
  Result := FImageCheckedLink.ImageIndex;
end;

{************************************************************}
procedure TALCheckbox.SetImageIndex(const Value: TImageIndex);
begin
  if FImageCheckedLink.ImageIndex <> Value then
    FImageCheckedLink.ImageIndex := Value;
end;

{*******************************************************}
function TALCheckbox.GetImageUncheckedIndex: TImageIndex;
begin
  Result := FImageUncheckedLink.ImageIndex;
end;

{*********************************************************************}
procedure TALCheckbox.SetImageUncheckedIndex(const Value: TImageIndex);
begin
  if FImageUncheckedLink.ImageIndex <> Value then
    FImageUncheckedLink.ImageIndex := Value;
end;

{****************************************************}
function TALCheckbox.ImagecheckedIndexStored: Boolean;
begin
  Result := (ImageCheckedIndex <> -1);
end;

{******************************************************}
function TALCheckbox.ImageUncheckedIndexStored: Boolean;
begin
  Result := (ImageUncheckedIndex <> -1);
end;

{***********************************************}
function TALCheckbox.GetImages: TCustomImageList;
begin
  Result := TCustomImageList(FImageCheckedLink.Images);
end;

{*************************************************************}
procedure TALCheckbox.SetImages(const Value: TCustomImageList);
begin
  FImageCheckedLink.Images := Value;
  FImageUncheckedLink.Images := Value;
end;

{*****************************************}
function TALCheckbox.ImagesStored: Boolean;
begin
  Result := Images <> nil;
end;

{*****************************************}
function TALCheckbox.GetIsChecked: Boolean;
begin
  Result := FIsChecked;
end;

{*******************************************************}
procedure TALCheckbox.SetIsChecked(const Value: Boolean);
begin
  if FIsChecked <> Value then
  begin
    FIsChecked := Value;
    StartTriggerAnimation(Self, 'IsChecked');
    DoChanged;
  end;
end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALRangeTrackBar, TALCheckBox]);
end;

end.
