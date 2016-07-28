unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, ALFmxObjects, FMX.Layouts,
  ALFmxLayouts, fmx.types3D, ALFmxCommon, System.ImageList,
  FMX.ImgList, alFmxImgList, ALFmxStdCtrls;

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
    ALVertScrollBox1: TVertScrollBox;
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
    procedure Button20Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
  private
    fALRangeTrackBar: TALRangeTrackBarStopWatch;
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
     UnitDemo,
     ALCommon;

{$R *.fmx}

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
    aChildRectangle.Margins.Right := 15;
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
    aChildRectangle.Margins.Right := 15;
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
    aChildRectangle.Margins.Right := 15;
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
    aChildRectangle.Margins.Right := 15;
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
    aChildRectangle.Margins.Right := 15;
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
    aChildRectangle.Margins.Right := 15;
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
    aChildRectangle.Margins.Right := 15;
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
    aChildRectangle.Margins.Right := 15;
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
    aChildRectangle.Margins.Right := 15;
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
    aChildRectangle.Margins.Right := 15;
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

procedure TForm1.Button20Click(Sender: TObject);
begin
  fALRangeTrackBar.Repaint;
  Text7.Text := 'Paint: ' + FormatFloat('0.#####',fALRangeTrackBar.PaintMs) + ' ms';
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
  fALText.Text := 'TALText Random 😘 😊 😍 😪 😴 😭 👿 🙀 👼 💇 💋 azert yuio p qs dfg jhk lm wxvcn bkn ,;/'#167'  123 098 4756 '#168#163' * AZE' +
                  ' RUTY IOP LK QJSH DFU AZZE F WBX CN';
  //-----
  fText := TTextStopWatch.Create(self);
  fText.Parent := ALVertScrollBox1;
  fALText.TextSettings.WordWrap := True;
  fALText.TextSettings.HorzAlign := TTextAlign.Center;
  fText.Align := TalignLayout.Top;
  fText.Margins.Top := 8;
  fText.Position.Y := button15.Position.Y - button15.Margins.Top;
  fText.Size.Height := 80;
  fText.Text := 'TText Random 😘 😊 😍 😪 😴 😭 👿 🙀 👼 💇 💋 azert yuio p qs dfg jhk lm wxvcn bkn ,;/'#167'  123 098 4756 '#168#163' * AZE' +
                ' RUTY IOP LK QJSH DFU AZZE F WBX CN';

  //-----
  fALGlyph := TALGlyphStopWatch.Create(self);
  fALGlyph.Parent := ALVertScrollBox1;
  fALGlyph.doubleBuffered := True;
  fALGlyph.Align := TalignLayout.Top;
  fALGlyph.Margins.Top := 8;
  fALGlyph.Position.Y := Button11.Position.Y - Button11.Margins.Top;
  fALGlyph.Size.Height := 22;
  fALGlyph.Images := ImageList1;
  fALGlyph.ImageIndex := 0;
  fALGlyph.Stretch := false;
  //-----
  fGlyph := TGlyphStopWatch.Create(self);
  fGlyph.Parent := ALVertScrollBox1;
  fGlyph.Align := TalignLayout.Top;
  fGlyph.Margins.Top := 8;
  fGlyph.Position.Y := Button11.Position.Y - Button11.Margins.Top;
  fGlyph.Size.Height := 22;
  fGlyph.Images := ImageList1;
  fGlyph.ImageIndex := 0;
  fGlyph.Stretch := false;

  //-----
  fALcheckbox2 := TALcheckboxStopWatch.Create(self);
  fALcheckbox2.Parent := ALVertScrollBox1;
  fALcheckbox2.Align := TalignLayout.Top;
  fALcheckbox2.doubleBuffered := True;
  fALcheckbox2.Images := ImageList1;
  fALcheckbox2.ImageCheckedIndex := 1;
  fALcheckbox2.ImageUncheckedIndex := 2;
  fALcheckbox2.Margins.Top := 8;
  fALcheckbox2.Margins.right := 25;
  fALcheckbox2.Margins.left := 25;
  fALcheckbox2.Position.Y := button17.Position.Y - button17.Margins.Top;
  fALcheckbox2.Height := 22;
  //-----
  fcheckbox2 := TcheckboxStopWatch.Create(self);
  fcheckbox2.Parent := ALVertScrollBox1;
  fcheckbox2.Align := TalignLayout.Top;
  fcheckbox2.Text := '';
  fcheckbox2.Margins.Top := 8;
  fcheckbox2.Margins.right := 25;
  fcheckbox2.Margins.left := 25;
  fcheckbox2.Position.Y := button17.Position.Y - button17.Margins.Top;
  fcheckbox2.Height := 22;

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
  fALRangeTrackBar := TALRangeTrackBarStopWatch.Create(self);
  fALRangeTrackBar.Parent := ALVertScrollBox1;
  fALRangeTrackBar.Align := TalignLayout.Top;
  fALRangeTrackBar.Margins.Top := 8;
  fALRangeTrackBar.Margins.right := 25;
  fALRangeTrackBar.Margins.left := 25;
  fALRangeTrackBar.Position.Y := button20.Position.Y - button20.Margins.Top;
  text7.Position.Y := button20.Position.Y - button20.Margins.height + button20.Margins.Top + + button20.Margins.bottom;

  //-----
  ALFmxMakeBufBitmaps(ALVertScrollBox1);
  endupdate;
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
