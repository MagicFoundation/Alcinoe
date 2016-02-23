unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, ALFmxRectangle, FMX.Layouts,
  ALFmxScrollBox, ALFmxText, fmx.types3D, ALFmxCommon;

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

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button15: TButton;
    ALVertScrollBox1: TALVertScrollBox;
    Button4: TButton;
    Text1: TText;
    Button5: TButton;
    Text3: TText;
    Button3: TButton;
    Text4: TText;
    Button6: TButton;
    Button7: TButton;
    Text2: TText;
    Button8: TButton;
    Button9: TButton;
    Text5: TText;
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
  private
    fText: TTextStopWatch;
    fALText: TALTextStopWatch;
    fALRectangle1: TALRectangleStopWatch;
    fRectangle1: TRectangleStopWatch;
    fALRectangle2: TALRectangleStopWatch;
    fRectangle2: TRectangleStopWatch;
    fALRectangle3: TALRectangleStopWatch;
    fRectangle3: TRectangleStopWatch;
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
          Text3.Text := 'buffer creation: ' + FormatFloat('0.#####',fALText.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALText.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button2Click(Sender: TObject);
Var aDemoForm: TDemoForm;
    aRectangle: TALRectangle;
    aVertScrollBox: TalVertScrollBox;
    aText: TALText;
    i: integer;
begin
  aDemoForm := TDemoForm.Create(nil);
  aDemoForm.BeginUpdate;
  aVertScrollBox := TalVertScrollBox.Create(aDemoForm);
  aVertScrollBox.Parent := aDemoForm;
  aVertScrollBox.Align := TalignLayout.Client;
  for I := 1 to 50 do begin
    aRectangle := TALRectangle.Create(aVertScrollBox);
    aRectangle.Parent := aVertScrollBox;
    aRectangle.doubleBuffered := True;
    aRectangle.Align := TalignLayout.Top;
    aRectangle.Margins.Left := 15;
    aRectangle.Margins.Top := 15;
    aRectangle.Margins.Right := 15;
    aRectangle.Margins.Bottom := 15;
    aRectangle.Position.Y := 0;
    aRectangle.Size.Height := 40;
    aRectangle.XRadius := 12;
    aRectangle.YRadius := 12;
    //-----
    aText := TALText.Create(self);
    aText.Parent := aRectangle;
    aText.doubleBuffered := True;
    aText.Align := TalignLayout.center;
    aText.Text := 'Alcinoe double buffered controls';
    aText.WordWrap := False;
    aText.autosize := True;
  end;
  aVertScrollBox.RecalcSize;
  ALFmxMakeBufBitmaps(aVertScrollBox);
  aDemoForm.endUpdate;
  aDemoForm.Show;
end;

procedure TForm1.Button1Click(Sender: TObject);
Var aDemoForm: TDemoForm;
    aRectangle: TRectangle;
    aVertScrollBox: TVertScrollBox;
    aText: TText;
    i: integer;
begin
  aDemoForm := TDemoForm.Create(nil);
  aDemoForm.BeginUpdate;
  aVertScrollBox := TVertScrollBox.Create(aDemoForm);
  aVertScrollBox.Parent := aDemoForm;
  aVertScrollBox.Align := TalignLayout.Client;
  for I := 1 to 50 do begin
    aRectangle := TRectangle.Create(aVertScrollBox);
    aRectangle.Parent := aVertScrollBox;
    aRectangle.Align := TalignLayout.Top;
    aRectangle.Margins.Left := 15;
    aRectangle.Margins.Top := 15;
    aRectangle.Margins.Right := 15;
    aRectangle.Margins.Bottom := 15;
    aRectangle.Position.Y := 0;
    aRectangle.Size.Height := 40;
    aRectangle.XRadius := 12;
    aRectangle.YRadius := 12;
    //-----
    aText := TText.Create(self);
    aText.Parent := aRectangle;
    aText.Align := TalignLayout.center;
    aText.Text := 'Delphi non buffered controls abc';
    aText.WordWrap := False;
    aText.autosize := True;
  end;
  aDemoForm.endUpdate;
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
  fRectangle1 := TRectangleStopWatch.Create(self);
  fRectangle1.Parent := ALVertScrollBox1;
  fRectangle1.Align := TalignLayout.Top;
  fRectangle1.Margins.Left := 15;
  fRectangle1.Margins.Top := 15;
  fRectangle1.Margins.Right := 15;
  fRectangle1.Margins.Bottom := 15;
  fRectangle1.Position.Y := Button5.Position.Y - 1;
  fRectangle1.Size.Height := 50;
  fRectangle1.XRadius := 12;
  fRectangle1.YRadius := 12;
  //-----
  fALRectangle1 := TALRectangleStopWatch.Create(self);
  fALRectangle1.Parent := ALVertScrollBox1;
  fALRectangle1.doubleBuffered := True;
  fALRectangle1.Align := TalignLayout.Top;
  fALRectangle1.Margins.Left := 15;
  fALRectangle1.Margins.Top := 15;
  fALRectangle1.Margins.Right := 15;
  fALRectangle1.Margins.Bottom := 15;
  fALRectangle1.Position.Y := Button5.Position.Y - 1;
  fALRectangle1.Size.Height := 50;
  fALRectangle1.XRadius := 12;
  fALRectangle1.YRadius := 12;

  //-----
  fRectangle2 := TRectangleStopWatch.Create(self);
  fRectangle2.Parent := ALVertScrollBox1;
  fRectangle2.Align := TalignLayout.Top;
  fRectangle2.Margins.Left := 15;
  fRectangle2.Margins.Top := 15;
  fRectangle2.Margins.Right := 15;
  fRectangle2.Margins.Bottom := 15;
  fRectangle2.Position.Y := button6.Position.Y-1;
  fRectangle2.Size.Height := 50;
  fRectangle2.XRadius := 0;
  fRectangle2.YRadius := 0;
  //-----
  fALRectangle2 := TALRectangleStopWatch.Create(self);
  fALRectangle2.Parent := ALVertScrollBox1;
  fALRectangle2.doubleBuffered := True;
  fALRectangle2.Align := TalignLayout.Top;
  fALRectangle2.Margins.Left := 15;
  fALRectangle2.Margins.Top := 15;
  fALRectangle2.Margins.Right := 15;
  fALRectangle2.Margins.Bottom := 15;
  fALRectangle2.Position.Y := button6.Position.Y - 1;
  fALRectangle2.Size.Height := 50;
  fALRectangle2.XRadius := 0;
  fALRectangle2.YRadius := 0;

  //-----
  fRectangle3 := TRectangleStopWatch.Create(self);
  fRectangle3.Parent := ALVertScrollBox1;
  fRectangle3.Align := TalignLayout.Top;
  fRectangle3.Margins.Left := 15;
  fRectangle3.Margins.Top := 15;
  fRectangle3.Margins.Right := 15;
  fRectangle3.Margins.Bottom := 15;
  fRectangle3.Position.Y := button8.Position.Y-1;
  fRectangle3.Size.Height := 50;
  fRectangle3.Stroke.Kind := TbrushKind.None;
  fRectangle3.XRadius := 0;
  fRectangle3.YRadius := 0;
  //-----
  fALRectangle3 := TALRectangleStopWatch.Create(self);
  fALRectangle3.Parent := ALVertScrollBox1;
  fALRectangle3.doubleBuffered := True;
  fALRectangle3.Align := TalignLayout.Top;
  fALRectangle3.Margins.Left := 15;
  fALRectangle3.Margins.Top := 15;
  fALRectangle3.Margins.Right := 15;
  fALRectangle3.Margins.Bottom := 15;
  fALRectangle3.Position.Y := button8.Position.Y - 1;
  fALRectangle3.Size.Height := 50;
  fALRectangle3.Stroke.Kind := TbrushKind.None;
  fALRectangle3.XRadius := 0;
  fALRectangle3.YRadius := 0;

  //-----
  fALText := TALTextStopWatch.Create(self);
  fALText.Parent := ALVertScrollBox1;
  fALText.doubleBuffered := True;
  fALText.Align := TalignLayout.Top;
  fALText.Margins.Top := 8;
  fALText.Position.Y := button15.Position.Y - button15.Margins.Top;
  fALText.Size.Height := 50;
  fALText.Text := 'azert yuio p qs dfg jhk lm wxvcn bkn ,;/'#167'  123 098 4756 '#168#163' * AZE' +
                  ' RUTY IOP LK QJSH DFU AZZE F WBX CN';
  //-----
  fText := TTextStopWatch.Create(self);
  fText.Parent := ALVertScrollBox1;
  fText.Align := TalignLayout.Top;
  fText.Margins.Top := 8;
  fText.Position.Y := button3.Position.Y - button3.Margins.Top;
  fText.Size.Height := 50;
  fText.Text := 'azert yuio p qs dfg jhk lm wxvcn bkn ,;/'#167'  123 098 4756 '#168#163' * AZE' +
                ' RUTY IOP LK QJSH DFU AZZE F WBX CN';
  endupdate;
end;

{ TRectangleStopWatch }

procedure TRectangleStopWatch.Paint;
var aStopWatch: TstopWatch;
begin
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
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
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;

{ TTextStopWatch }

procedure TTextStopWatch.Paint;
var aStopWatch: TstopWatch;
begin
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
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;
end.
