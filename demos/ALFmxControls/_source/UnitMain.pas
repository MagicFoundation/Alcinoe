unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, ALFmxObjects, FMX.Layouts,
  ALFmxLayouts, fmx.types3D, ALFmxCommon, System.ImageList,
  FMX.ImgList, alFmxImgList, ALFmxStdCtrls, FMX.TabControl, ALFmxTabControl,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, ALFmxEdit;

type

  TALRectangleStopWatch = Class(TALRectangle)
  protected
     bufPaintMs: double;
     bufCreatePaintMs: double;
     procedure Paint; override;
  public
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     function MakeBufBitmap: TTexture; override;
    {$ELSE}
     function MakeBufBitmap: Tbitmap; override;
    {$ENDIF}
  End;
  TRectangleStopWatch = Class(TRectangle)
  public
     PaintMs: double;
     procedure Paint; override;
  End;

  TALCircleStopWatch = Class(TALCircle)
  protected
     bufPaintMs: double;
     bufCreatePaintMs: double;
     procedure Paint; override;
  public
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     function MakeBufBitmap: TTexture; override;
    {$ELSE}
     function MakeBufBitmap: Tbitmap; override;
    {$ENDIF}
  End;
  TCircleStopWatch = Class(TCircle)
  public
     PaintMs: double;
     procedure Paint; override;
  End;

  TALTextStopWatch = Class(TalText)
  protected
     bufPaintMs: double;
     bufCreatePaintMs: double;
     procedure Paint; override;
  public
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     function MakeBufBitmap: TTexture; override;
    {$ELSE}
     function MakeBufBitmap: Tbitmap; override;
    {$ENDIF}
  End;
  TTextStopWatch = Class(TText)
  public
     PaintMs: double;
     procedure Paint; override;
  End;

  TALGlyphStopWatch = Class(TALGlyph)
  protected
     bufPaintMs: double;
     bufCreatePaintMs: double;
     procedure Paint; override;
  public
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     function MakeBufBitmap: TTexture; override;
    {$ELSE}
     function MakeBufBitmap: Tbitmap; override;
    {$ENDIF}
  End;
  TGlyphStopWatch = Class(TGlyph)
  public
     PaintMs: double;
     procedure Paint; override;
  End;

  TALCheckBoxStopWatch = Class(TALCheckBox)
  protected
     bufPaintMs: double;
     bufCreatePaintMs: double;
     procedure Paint; override;
  public
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     function MakeBufBitmap: TTexture; override;
    {$ELSE}
     function MakeBufBitmap: Tbitmap; override;
    {$ENDIF}
  End;
  TCheckBoxStopWatch = Class(TCheckBox)
  public
     PaintMs: double;
     procedure PaintChildren; override;
  End;

  TLineStopWatch = Class(TLine)
  public
     PaintMs: double;
     procedure Paint; override;
  End;
  TALLineStopWatch = Class(TALLine)
  protected
     bufPaintMs: double;
     bufCreatePaintMs: double;
  public
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     function MakeBufBitmap: TTexture; override;
    {$ELSE}
     function MakeBufBitmap: Tbitmap; override;
    {$ENDIF}
     procedure Paint; override;
  End;

  TALRangeTrackBarStopWatch = Class(TALRangeTrackBar)
  public
     PaintMs: double;
     procedure PaintChildren; override;
  End;

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button15: TButton;
    ALVertScrollBox1: TALVertScrollBox;
    Button4: TButton;
    Text1: TText;
    Button5: TButton;
    Button3: TButton;
    Text4: TText;
    Button6: TButton;
    Button7: TButton;
    Text2: TText;
    Button8: TButton;
    Button9: TButton;
    Text5: TText;
    Button10: TButton;
    Text6: TText;
    Button11: TButton;
    ImageList1: TImageList;
    Button13: TButton;
    Text10: TText;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Text3: TText;
    Button20: TButton;
    Text7: TText;
    Text9: TText;
    Button21: TButton;
    Button22: TButton;
    ALTabControl1: TALTabControl;
    ALTabItem1: TALTabItem;
    Image1: TImage;
    ALTabItem2: TALTabItem;
    Image2: TImage;
    ALText1: TALText;
    ALText2: TALText;
    ALTabItem3: TALTabItem;
    Image3: TImage;
    ALTabItem4: TALTabItem;
    Image4: TImage;
    ALText3: TALText;
    ALText4: TALText;
    ALText5: TALText;
    ALText6: TALText;
    ALText7: TALText;
    ALText8: TALText;
    ALRectangle9: TALRectangle;
    ALRangeTrackBar1: TALRangeTrackBar;
    ALTrackBar1: TALTrackBar;
    Button14: TButton;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    ALRectangle2: TALRectangle;
    ALEdit2: TALEdit;
    Layout7: TLayout;
    Image7: TImage;
    Layout8: TLayout;
    Image8: TImage;
    Text11: TText;
    Button12: TButton;
    ALText9: TALText;
    Text8: TText;
    procedure Button2Click(Sender: TObject);
    procedure Button255Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure ALTabControl1ViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF);
    procedure ALTabControl1AniTransitionInit(const sender: TObject;
                                             const aVelocity: Double; var aDuration: Single;
                                             var aAnimationType: TAnimationType;
                                             var aInterpolation: TInterpolationType);
    procedure ALTabControl1Resize(Sender: TObject);
    procedure ALVertScrollBox1ScrollBarInit(const sender: TObject; const aScrollBar: TALScrollBoxBar);
    procedure VScrollBarThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure VScrollBarThumbMouseLeave(Sender: TObject);
    procedure VScrollBarThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure VScrollBarThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure ALVertScrollBox1Click(Sender: TObject);
    procedure ALEdit2ChangeTracking(Sender: TObject);
  private
    fALcheckbox2: TALcheckboxStopWatch;
    fcheckbox2: TcheckboxStopWatch;
    fLine: TLineStopWatch;
    fALLine: TALLineStopWatch;
    fText: TTextStopWatch;
    fGlyph: TGlyphStopWatch;
    fALGlyph: TALGlyphStopWatch;
    fALText: TALTextStopWatch;
    fALRectangle1: TALRectangleStopWatch;
    fRectangle1: TRectangleStopWatch;
    fALRectangle2: TALRectangleStopWatch;
    fRectangle2: TRectangleStopWatch;
    fALRectangle3: TALRectangleStopWatch;
    fRectangle3: TRectangleStopWatch;
    fALCircle1: TALCircleStopWatch;
    fCircle1: TCircleStopWatch;
  public
  end;

var
  Form1: TForm1;

implementation

uses system.Diagnostics,
     system.threading,
     system.Math,
     UnitDemo,
     ALFmxInertialMovement,
     ALCommon;

{$R *.fmx}

procedure TForm1.ALEdit2ChangeTracking(Sender: TObject);
begin
  ALLog('ALEdit2ChangeTracking', ALEdit2.Text);
end;

procedure TForm1.ALVertScrollBox1Click(Sender: TObject);
begin
  ALEdit2.ResetFocus;
end;

procedure TForm1.ALTabControl1AniTransitionInit(const sender: TObject;
                                                const aVelocity: Double;
                                                var aDuration: Single;
                                                var aAnimationType: TAnimationType;
                                                var aInterpolation: TInterpolationType);
begin
  // aVelocity = pixels per seconds given by the anicalculations
  // ALTabControl1.Width - abs(ALTabControl1.activeTab.Position.X) = the number of pixel we need to scroll
  // 6 = factor i choose to compensate the deceleration made by the quartic Interpolation
  if comparevalue(aVelocity, 0) <> 0 then aDuration := abs((ALTabControl1.Width - abs(ALTabControl1.activeTab.Position.X)) / aVelocity) * 6
  else aDuration := 0.8;
  if aDuration > 0.8 then aDuration := 0.8
  else if aDuration < 0.1 then aDuration := 0.1;
  aAnimationType := TAnimationType.out;
  aInterpolation := TInterpolationType.circular;
end;

procedure TForm1.ALTabControl1Resize(Sender: TObject);

  procedure _updateLabels(const aTab: TalTabItem);
  var aText1, aText2: TalText;
      aControl1: Tcontrol;
  begin
    aText1 := nil;
    aText2 := nil;
    for aControl1 in aTab.Controls do begin
      if (aControl1 is TalText) and (aControl1.Tag = 1) then aText1 := TalText(aControl1)
      else if (aControl1 is TalText) and (aControl1.Tag = 2) then aText2 := TalText(aControl1);
    end;
    if aText1 <> nil then
      aText1.Position.X := ((aTab.Width - aText1.Width) / 2) + (aTab.Position.X / 5);
    if aText2 <> nil then
      aText2.Position.X := ((aTab.Width - aText2.Width) / 2) + (aTab.Position.X);
  end;

begin
  if ALTabControl1.TabIndex > 0 then _updateLabels(ALTabControl1.tabs[ALTabControl1.TabIndex - 1]);
  _updateLabels(ALTabControl1.tabs[ALTabControl1.TabIndex]);
  if ALTabControl1.TabIndex < ALTabControl1.Tabcount - 1 then _updateLabels(ALTabControl1.tabs[ALTabControl1.TabIndex + 1]);
end;

procedure TForm1.ALTabControl1ViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF);
begin
  ALTabControl1Resize(nil);
end;

procedure TForm1.ALVertScrollBox1ScrollBarInit(const sender: TObject; const aScrollBar: TALScrollBoxBar);
begin
  //special case for windows
  if not ALVertScrollBox1.HasTouchScreen then begin
    if aScrollBar.Orientation = Torientation.Vertical then begin
      aScrollBar.Width := 8;
      aScrollBar.Margins.Right := 3;
      aScrollBar.Thumb.XRadius := 4;
      aScrollBar.Thumb.yRadius := 4;
    end;
  end;
end;

procedure TForm1.VScrollBarThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if not ALVertScrollBox1.HasTouchScreen then begin
    with (sender as TALTrackThumb) do begin
      if tag = 0 then begin // 0 = out and not down
        tag := 1; // 1 = in and not down
        Fill.Color := $64000000;
        (sender as TALTrackThumb).InvalidateRect(localrect);
      end
      else if tag = 10 then  // 10 = out and down
        tag := 11; // 11 = in and down
    end;
  end;
end;

procedure TForm1.VScrollBarThumbMouseLeave(Sender: TObject);
begin
  if not ALVertScrollBox1.HasTouchScreen then begin
    with (sender as TALTrackThumb) do begin
      if tag = 1 then begin // 1 = in and not down
        tag := 0; // 0 = out and not down
        Fill.Color := $47000000;
        (sender as TALTrackThumb).InvalidateRect(localrect);
      end
      else if tag = 11 then // 11 = in and down
        tag := 10; // 10 = out and down
    end;
  end;
end;

procedure TForm1.VScrollBarThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not ALVertScrollBox1.HasTouchScreen then begin
    with (sender as TALTrackThumb) do begin
      if tag = 1 then // 1 = in and not down
        tag := 11 // 11 = in and down
      else
        tag := 10; // 10 = out and down
      Fill.Color := $96000000;
      (sender as TALTrackThumb).InvalidateRect(localrect);
    end;
  end;
end;

procedure TForm1.VScrollBarThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not ALVertScrollBox1.HasTouchScreen then begin
    with (sender as TALTrackThumb) do begin
      if tag = 11 then begin // 11 = in and down
        tag := 1; // 1 = in and not down
        Fill.Color := $64000000;
      end
      else if tag = 10 then begin // 10 = out and down
        tag := 0; // 0 = out and not down
        Fill.Color := $47000000;
      end;
      (sender as TALTrackThumb).InvalidateRect(localrect);
    end;
  end;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  fGlyph.Repaint;
  Text6.Text := 'Paint: ' + FormatFloat('0.#####',fGlyph.PaintMs) + ' ms';
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  fALGlyph.clearBufBitmap;
  fALGlyph.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text6.Text := 'buffer creation: ' + FormatFloat('0.#####',fALGlyph.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALGlyph.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button13Click(Sender: TObject);
begin
  fline.Repaint;
  Text10.Text := 'Paint: ' + FormatFloat('0.#####',fline.PaintMs) + ' ms';
end;

procedure TForm1.Button15Click(Sender: TObject);
begin
  fALText.clearBufBitmap;
  fALText.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text4.Text := 'buffer creation: ' + FormatFloat('0.#####',fALText.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALText.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button16Click(Sender: TObject);
begin
  fALLine.clearBufBitmap;
  fALLine.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text10.Text := 'buffer creation: ' + FormatFloat('0.#####',fALLine.bufCreatePaintMs) + ' ms - ' +
                         'Paint: ' + FormatFloat('0.#####',fALLine.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button17Click(Sender: TObject);
begin
  fALcheckbox2.clearBufBitmap;
  fALcheckbox2.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text3.Text := 'buffer creation: ' + FormatFloat('0.#####',fALcheckbox2.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALcheckbox2.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button18Click(Sender: TObject);
begin
  fcheckbox2.Repaint;
  Text3.Text := 'Paint: ' + FormatFloat('0.#####',fcheckbox2.PaintMs) + ' ms';
end;

procedure TForm1.Button2Click(Sender: TObject);
Var aDemoForm: TDemoForm;
    aRectangle: TALRectangle;
    aChildRectangle: TalRectangle;
    aVertScrollBox: TalVertScrollBox;
    aText: TALText;
    i: integer;
begin
  aDemoForm := TDemoForm.Create(nil);
  aVertScrollBox := TalVertScrollBox.Create(aDemoForm);
  aVertScrollBox.Parent := aDemoForm;
  aVertScrollBox.BeginUpdate;
  aVertScrollBox.Align := TalignLayout.Client;
  for I := 1 to 50 do begin
    aRectangle := TALRectangle.Create(aVertScrollBox);
    aRectangle.Parent := aVertScrollBox;
    aRectangle.doubleBuffered := True;
    aRectangle.Align := TalignLayout.Top;
    aRectangle.Margins.Left := 15;
    aRectangle.Margins.Top := 10;
    aRectangle.Margins.Right := 15;
    aRectangle.Margins.Bottom := 10;
    aRectangle.Position.Y := 0;
    aRectangle.Size.Height := 50;
    aRectangle.XRadius := 12;
    aRectangle.YRadius := 12;
    //-----
    aText := TALText.Create(self);
    aText.Parent := aRectangle;
    aText.doubleBuffered := True;
    aText.Align := TalignLayout.left;
    aText.Text := 'Alcinoe';
    aText.Margins.Left := 15;
    aText.WordWrap := False;
    aText.autosize := True;
    //-----
    aChildRectangle := TALRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.red;
    //-----
    aChildRectangle := TALRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.green;
    //-----
    aChildRectangle := TALRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.blue;
    //-----
    aChildRectangle := TALRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.yellow;
    //-----
    aChildRectangle := TALRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.Orange;
  end;
  ALFmxMakeBufBitmaps(aVertScrollBox);
  aVertScrollBox.endUpdate;
  aDemoForm.Show;
end;

procedure TForm1.Button1Click(Sender: TObject);
Var aDemoForm: TDemoForm;
    aRectangle: TRectangle;
    aChildRectangle: TRectangle;
    aVertScrollBox: TVertScrollBox;
    aText: TText;
    i: integer;
begin
  aDemoForm := TDemoForm.Create(nil);
  aVertScrollBox := TVertScrollBox.Create(aDemoForm);
  aVertScrollBox.Parent := aDemoForm;
  aVertScrollBox.BeginUpdate;
  aVertScrollBox.Align := TalignLayout.Client;
  for I := 1 to 50 do begin
    aRectangle := TRectangle.Create(aVertScrollBox);
    aRectangle.Parent := aVertScrollBox;
    aRectangle.Align := TalignLayout.Top;
    aRectangle.Margins.Left := 15;
    aRectangle.Margins.Top := 10;
    aRectangle.Margins.Right := 15;
    aRectangle.Margins.Bottom := 10;
    aRectangle.Position.Y := 0;
    aRectangle.Size.Height := 50;
    aRectangle.XRadius := 12;
    aRectangle.YRadius := 12;
    //-----
    aText := TText.Create(self);
    aText.Parent := aRectangle;
    aText.Align := TalignLayout.left;
    aText.Text := 'Embarc.';
    aText.Margins.Left := 15;
    aText.WordWrap := False;
    aText.autosize := True;
    //-----
    aChildRectangle := TRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.red;
    //-----
    aChildRectangle := TRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.green;
    //-----
    aChildRectangle := TRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.blue;
    //-----
    aChildRectangle := TRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.yellow;
    //-----
    aChildRectangle := TRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.orange;
  end;
  aVertScrollBox.EndUpdate;
  aDemoForm.Show;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  fText.Repaint;
  Text4.Text := 'Paint: ' + FormatFloat('0.#####',fText.PaintMs) + ' ms';
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  fRectangle1.Repaint;
  Text1.Text := 'Paint: ' + FormatFloat('0.#####',fRectangle1.PaintMs) + ' ms';
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  fRectangle2.Repaint;
  Text2.Text := 'Paint: ' + FormatFloat('0.#####',fRectangle2.PaintMs) + ' ms';
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  fRectangle3.Repaint;
  Text5.Text := 'Paint: ' + FormatFloat('0.#####',fRectangle3.PaintMs) + ' ms';
end;

procedure TForm1.Button21Click(Sender: TObject);
begin
  fCircle1.Repaint;
  Text9.Text := 'Paint: ' + FormatFloat('0.#####',fCircle1.PaintMs) + ' ms';
end;

procedure TForm1.Button22Click(Sender: TObject);
begin
  fALCircle1.clearBufBitmap;
  fALCircle1.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text9.Text := 'buffer creation: ' + FormatFloat('0.#####',fALCircle1.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALCircle1.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button255Click(Sender: TObject);
begin
  fALRectangle1.clearBufBitmap;
  fALRectangle1.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text1.Text := 'buffer creation: ' + FormatFloat('0.#####',fALRectangle1.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALRectangle1.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  fALRectangle2.clearBufBitmap;
  fALRectangle2.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text2.Text := 'buffer creation: ' + FormatFloat('0.#####',fALRectangle2.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALRectangle2.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  fALRectangle3.clearBufBitmap;
  fALRectangle3.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text5.Text := 'buffer creation: ' + FormatFloat('0.#####',fALRectangle3.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALRectangle3.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  beginupdate;

  //-----
  fALRectangle1 := TALRectangleStopWatch.Create(self);
  fALRectangle1.Parent := ALVertScrollBox1;
  fALRectangle1.doubleBuffered := True;
  fALRectangle1.Align := TalignLayout.Top;
  fALRectangle1.Margins.Left := 15;
  fALRectangle1.Margins.Top := 15;
  fALRectangle1.Margins.Right := 15;
  fALRectangle1.Margins.Bottom := 15;
  fALRectangle1.Position.Y := Button5.Position.Y - Button5.Margins.Top;
  fALRectangle1.Size.Height := 50;
  fALRectangle1.XRadius := 12;
  fALRectangle1.YRadius := 12;
  //-----
  fRectangle1 := TRectangleStopWatch.Create(self);
  fRectangle1.Parent := ALVertScrollBox1;
  fRectangle1.Align := TalignLayout.Top;
  fRectangle1.Margins.Left := 15;
  fRectangle1.Margins.Top := 15;
  fRectangle1.Margins.Right := 15;
  fRectangle1.Margins.Bottom := 15;
  fRectangle1.Position.Y := Button5.Position.Y - Button5.Margins.Top;
  fRectangle1.Size.Height := 50;
  fRectangle1.XRadius := 12;
  fRectangle1.YRadius := 12;

  //-----
  fALRectangle2 := TALRectangleStopWatch.Create(self);
  fALRectangle2.Parent := ALVertScrollBox1;
  fALRectangle2.doubleBuffered := True;
  fALRectangle2.Align := TalignLayout.Top;
  fALRectangle2.Margins.Left := 15;
  fALRectangle2.Margins.Top := 15;
  fALRectangle2.Margins.Right := 15;
  fALRectangle2.Margins.Bottom := 15;
  fALRectangle2.Position.Y := button6.Position.Y- Button6.Margins.Top;
  fALRectangle2.Size.Height := 50;
  fALRectangle2.XRadius := 0;
  fALRectangle2.YRadius := 0;
  //-----
  fRectangle2 := TRectangleStopWatch.Create(self);
  fRectangle2.Parent := ALVertScrollBox1;
  fRectangle2.Align := TalignLayout.Top;
  fRectangle2.Margins.Left := 15;
  fRectangle2.Margins.Top := 15;
  fRectangle2.Margins.Right := 15;
  fRectangle2.Margins.Bottom := 15;
  fRectangle2.Position.Y := button6.Position.Y- Button6.Margins.Top;
  fRectangle2.Size.Height := 50;
  fRectangle2.XRadius := 0;
  fRectangle2.YRadius := 0;

  //-----
  fALRectangle3 := TALRectangleStopWatch.Create(self);
  fALRectangle3.Parent := ALVertScrollBox1;
  fALRectangle3.doubleBuffered := True;
  fALRectangle3.Align := TalignLayout.Top;
  fALRectangle3.Margins.Left := 15;
  fALRectangle3.Margins.Top := 15;
  fALRectangle3.Margins.Right := 15;
  fALRectangle3.Margins.Bottom := 15;
  fALRectangle3.Position.Y := button8.Position.Y- Button8.Margins.Top;
  fALRectangle3.Size.Height := 50;
  fALRectangle3.Stroke.Kind := TbrushKind.None;
  fALRectangle3.XRadius := 0;
  fALRectangle3.YRadius := 0;
  //-----
  fRectangle3 := TRectangleStopWatch.Create(self);
  fRectangle3.Parent := ALVertScrollBox1;
  fRectangle3.Align := TalignLayout.Top;
  fRectangle3.Margins.Left := 15;
  fRectangle3.Margins.Top := 15;
  fRectangle3.Margins.Right := 15;
  fRectangle3.Margins.Bottom := 15;
  fRectangle3.Position.Y := button8.Position.Y- Button8.Margins.Top;
  fRectangle3.Size.Height := 50;
  fRectangle3.Stroke.Kind := TbrushKind.None;
  fRectangle3.XRadius := 0;
  fRectangle3.YRadius := 0;

  //-----
  fALCircle1 := TALCircleStopWatch.Create(self);
  fALCircle1.Parent := ALVertScrollBox1;
  fALCircle1.doubleBuffered := True;
  fALCircle1.Align := TalignLayout.Top;
  fALCircle1.Margins.Left := 15;
  fALCircle1.Margins.Top := 15;
  fALCircle1.Margins.Right := 15;
  fALCircle1.Margins.Bottom := 15;
  fALCircle1.Position.Y := button22.Position.Y- Button22.Margins.Top;
  fALCircle1.Size.Height := 50;
  //-----
  fCircle1 := TCircleStopWatch.Create(self);
  fCircle1.Parent := ALVertScrollBox1;
  fCircle1.Align := TalignLayout.Top;
  fCircle1.Margins.Left := 15;
  fCircle1.Margins.Top := 15;
  fCircle1.Margins.Right := 15;
  fCircle1.Margins.Bottom := 15;
  fCircle1.Position.Y := button22.Position.Y- Button22.Margins.Top;
  fCircle1.Size.Height := 50;

  //-----
  fALText := TALTextStopWatch.Create(self);
  fALText.Parent := ALVertScrollBox1;
  fALText.TextSettings.WordWrap := True;
  fALText.TextSettings.HorzAlign := TTextAlign.Center;
  fALText.doubleBuffered := True;
  fALText.Align := TalignLayout.Top;
  fALText.Margins.Top := 8;
  fALText.Position.Y := button15.Position.Y - button15.Margins.Top;
  fALText.Size.Height := 80;
  fALText.Text := 'TALText Random 😂 😉 😐 🙉 🙇 💑 👨‍👨‍👧‍👦 💪 💥 🐇 🌼 🍡 🌋 🗽 🚚 🎁 🎶 📫 azert yuio p qs dfg jhk lm wxvcn bkn ,;/'#167'  123 098 4756 '#168#163' * AZE' +
                  ' RUTY IOP LK QJSH DFU AZZE F WBX CN';
  //-----
  fText := TTextStopWatch.Create(self);
  fText.Parent := ALVertScrollBox1;
  fText.TextSettings.WordWrap := True;
  fText.TextSettings.HorzAlign := TTextAlign.Center;
  fText.Align := TalignLayout.Top;
  fText.Margins.Top := 8;
  fText.Position.Y := button15.Position.Y - button15.Margins.Top;
  fText.Size.Height := 80;
  fText.Text := 'TText Random 😂 😉 😐 🙉 🙇 💑 👨‍👨‍👧‍👦 💪 💥 🐇 🌼 🍡 🌋 🗽 🚚 🎁 🎶 📫 azert yuio p qs dfg jhk lm wxvcn bkn ,;/'#167'  123 098 4756 '#168#163' * AZE' +
                ' RUTY IOP LK QJSH DFU AZZE F WBX CN';

  //-----
  fALGlyph := TALGlyphStopWatch.Create(layout1);
  fALGlyph.Parent := layout1;
  fALGlyph.doubleBuffered := True;
  fALGlyph.Size.Height := 22;
  fALGlyph.Size.width := 22;
  fALGlyph.Position.Point := TpointF.Create(0,0);
  fALGlyph.Images := ImageList1;
  fALGlyph.ImageIndex := 0;
  //-----
  fGlyph := TGlyphStopWatch.Create(layout2);
  fGlyph.Parent := layout2;
  fGlyph.Size.Height := 22;
  fGlyph.Size.width := 22;
  fGlyph.Position.Point := TpointF.Create(0,0);
  fGlyph.Images := ImageList1;
  fGlyph.ImageIndex := 0;

  //-----
  fALcheckbox2 := TALcheckboxStopWatch.Create(layout3);
  fALcheckbox2.Parent := layout3;
  fALcheckbox2.doubleBuffered := True;
  fALcheckbox2.Images := ImageList1;
  fALcheckbox2.ImageCheckedIndex := 1;
  fALcheckbox2.ImageUncheckedIndex := 2;
  fALcheckbox2.Height := 22;
  fALcheckbox2.width := 22;
  fALcheckbox2.Position.Point := TpointF.Create(0,0);
  //-----
  fcheckbox2 := TcheckboxStopWatch.Create(layout4);
  fcheckbox2.Parent := layout4;
  fcheckbox2.Text := '';
  fcheckbox2.Height := 22;
  fcheckbox2.width := 22;
  fcheckbox2.Position.Point := TpointF.Create(0,0);

  //-----
  fALline := TALLineStopWatch.Create(self);
  fALline.Parent := ALVertScrollBox1;
  fALline.doubleBuffered := true;
  fALline.Align := TalignLayout.Top;
  fALline.Margins.Top := 8;
  fALline.Margins.right := 24;
  fALline.Margins.left := 24;
  fALline.Height := 1;
  fALline.Position.Y := button16.Position.Y - button16.Margins.Top;
  fALline.LineType := TLineType.Top;
  //-----
  fline := TLineStopWatch.Create(self);
  fline.Parent := ALVertScrollBox1;
  fline.Align := TalignLayout.Top;
  fline.Margins.Top := 8;
  fline.Margins.right := 24;
  fline.Margins.left := 24;
  fline.Height := 1;
  fline.Position.Y := button16.Position.Y - button16.Margins.Top;
  fline.LineType := TLineType.Top;
  //-----
  ALFmxMakeBufBitmaps(ALVertScrollBox1);
  endupdate;
  ALTabControl1Resize(nil);
end;


procedure TForm1.FormVirtualKeyboardHidden(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  AlVertScrollBox1.margins.Bottom := 0;
  AlVertScrollBox1.AniCalculations.TouchTracking := [ttVertical];
end;

procedure TForm1.FormVirtualKeyboardShown(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  AlVertScrollBox1.margins.Bottom := Bounds.height;
  AlVertScrollBox1.VScrollBar.Value := AlVertScrollBox1.VScrollBar.Max;
  AlVertScrollBox1.AniCalculations.TouchTracking := [];
end;

{ TALTextStopWatch }

{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALTextStopWatch.MakeBufBitmap: TTexture;
{$ELSE}
function TALTextStopWatch.MakeBufBitmap: Tbitmap;
{$ENDIF}
var aStopWatch: TstopWatch;
begin
  if (BufBitmap = nil) then begin
    aStopWatch := TstopWatch.StartNew;
    result := inherited MakeBufBitmap;
    aStopWatch.stop;
    bufCreatePaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  end
  else result := inherited MakeBufBitmap;
end;

procedure TALTextStopWatch.Paint;
var aStopWatch: TstopWatch;
    aRemovebufCreatePaintMs: boolean;
begin
  clearbufBitmap;
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  MakeBufBitmap;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;

{ TTextStopWatch }

procedure TTextStopWatch.Paint;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

{ TRectangleStopWatch }

procedure TRectangleStopWatch.Paint;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

{ TALRectangleStopWatch }

{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALRectangleStopWatch.MakeBufBitmap: TTexture;
{$ELSE}
function TALRectangleStopWatch.MakeBufBitmap: Tbitmap;
{$ENDIF}
var aStopWatch: TstopWatch;
begin
  if (BufBitmap = nil) then begin
    aStopWatch := TstopWatch.StartNew;
    result := inherited MakeBufBitmap;
    aStopWatch.stop;
    bufCreatePaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  end
  else result := inherited MakeBufBitmap;
end;

procedure TALRectangleStopWatch.Paint;
var aStopWatch: TstopWatch;
    aRemovebufCreatePaintMs: boolean;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;

{ TGlyphStopWatch }

procedure TGlyphStopWatch.Paint;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

{ TALGlyphStopWatch }

{$IF DEFINED(IOS) or DEFINED(ANDROID)}
 function TALGlyphStopWatch.MakeBufBitmap: TTexture;
{$ELSE}
 function TALGlyphStopWatch.MakeBufBitmap: Tbitmap;
{$ENDIF}
var aStopWatch: TstopWatch;
begin
  if (BufBitmap = nil) then begin
    aStopWatch := TstopWatch.StartNew;
    result := inherited MakeBufBitmap;
    aStopWatch.stop;
    bufCreatePaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  end
  else result := inherited MakeBufBitmap;
end;

procedure TALGlyphStopWatch.Paint;
var aStopWatch: TstopWatch;
    aRemovebufCreatePaintMs: boolean;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;

{ TCheckBoxStopWatch }

procedure TCheckBoxStopWatch.PaintChildren;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited PaintChildren;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

{ TALCheckBoxStopWatch }

{$IF DEFINED(IOS) or DEFINED(ANDROID)}
 function TALCheckBoxStopWatch.MakeBufBitmap: TTexture;
{$ELSE}
 function TALCheckBoxStopWatch.MakeBufBitmap: Tbitmap;
{$ENDIF}
var aStopWatch: TstopWatch;
begin
  if (BufBitmap = nil) then begin
    aStopWatch := TstopWatch.StartNew;
    result := inherited MakeBufBitmap;
    aStopWatch.stop;
    bufCreatePaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  end
  else result := inherited MakeBufBitmap;
end;

procedure TALCheckBoxStopWatch.Paint;
var aStopWatch: TstopWatch;
    aRemovebufCreatePaintMs: boolean;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;

{ TLineStopWatch }

procedure TLineStopWatch.Paint;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

{ TALLineStopWatch }

{$IF DEFINED(IOS) or DEFINED(ANDROID)}
 function TALLineStopWatch.MakeBufBitmap: TTexture;
{$ELSE}
 function TALLineStopWatch.MakeBufBitmap: Tbitmap;
{$ENDIF}
var aStopWatch: TstopWatch;
begin
  if (BufBitmap = nil) then begin
    aStopWatch := TstopWatch.StartNew;
    result := inherited MakeBufBitmap;
    aStopWatch.stop;
    bufCreatePaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  end
  else result := inherited MakeBufBitmap;
end;

procedure TALLineStopWatch.Paint;
var aStopWatch: TstopWatch;
    aRemovebufCreatePaintMs: boolean;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;

{ TALRangeTrackBarStopWatch }

procedure TALRangeTrackBarStopWatch.PaintChildren;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited PaintChildren;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

{ TALCircleStopWatch }

{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALCircleStopWatch.MakeBufBitmap: TTexture;
{$ELSE}
function TALCircleStopWatch.MakeBufBitmap: Tbitmap;
{$ENDIF}
var aStopWatch: TstopWatch;
begin
  if (BufBitmap = nil) then begin
    aStopWatch := TstopWatch.StartNew;
    result := inherited MakeBufBitmap;
    aStopWatch.stop;
    bufCreatePaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  end
  else result := inherited MakeBufBitmap;
end;

procedure TALCircleStopWatch.Paint;
var aStopWatch: TstopWatch;
    aRemovebufCreatePaintMs: boolean;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;

{ TCircleStopWatch }

procedure TCircleStopWatch.Paint;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

end.
