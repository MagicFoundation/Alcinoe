unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellApi, Printers,
  SynCommons, SynPdf, mORMotReport;

type
  TForm1 = class(TForm)
    edt1: TEdit;
    lbl1: TLabel;
    Label1: TLabel;
    mmo1: TMemo;
    btn1: TButton;
    btn2: TButton;
    Button1: TButton;
    procedure btn2Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$R Vista.res}

procedure TForm1.btn2Click(Sender: TObject);
begin
  Close;
end;

const
  UNICODE: array[0..5] of WideChar =
   (#27161,#28310,#33836,#22283,#30908,#0);
procedure TForm1.btn1Click(Sender: TObject);
var Bmp: TBitmap;
    U: RawUTF8;
    s: string;
    i: integer;
{   R, Marg: TRect;
    iz: TSize;
    M: TMetaFile;}
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width := ClientWidth;
    Bmp.Height := ClientHeight;
    PaintTo(Bmp.Canvas,0,0); // create some bitmap content
    with TGDIPages.Create(self) do
    try
      // the name of the report is taken from main Window's caption
      Caption := self.Caption;
      //Orientation := poLandscape;
      // now we add some content to the report
      BeginDoc;
      {
      M := TMetaFile.Create;
      M.LoadFromFile('emf1.emf');
      Siz := PaperSize;
      Marg := PageMargins;
      R.Left := Marg.Left;
      R.Right := Siz.cx-Marg.Right;
      R.Top := Marg.Top;
      R.Bottom := Siz.cy-Marg.Top;
      DrawMeta(R,M);
      M.Free;
      }

      // header and footer
      Font.Name := 'Georgia';
      //Font.Name := 'Arial Unicode MS';
      Font.Size := 11;
      SaveLayout;
      Font.Style := [fsItalic,fsUnderline];
      TextAlign := taRight;
      AddTextToHeaderAt('http://synopse.info',RightMarginPos);
      Font.Style := [];
      AddLineToFooter(false);
      AddPagesToFooterAt(sPageN,RightMarginPos);
      RestoreSavedLayout;
      AddTextToHeader(ExeVersion.ProgramName);
      AddTextToFooter(DateTimeToStr(Now));
      AddLineToHeader(false);
      Font.Size := 12;
      ExportPDFForceJPEGCompression := 0;

{      // test
      WordWrapLeftCols := true;
      AddColumns([10,22,22,22,22]);
      AddColumnHeaders(['#','Two','Three','4','5'],true,true);
      for i := 1 to 50 do
        DrawTextAcrossCols([IntToStr(i),'Column '+IntToStr(i),
        'This is some big text which must be justified on multiple lines. Text "four" and "five" will be invisible in pdf...',
        'four','five']);
      EndDoc;
      ExportPDF('cells.pdf',True,True);}

      // main content (automaticaly split on next pages)
      NewHalfLine;
      TextAlign := taJustified;
      U := RawUnicodeToUtf8(UNICODE,StrLenW(UNICODE));
      U := 'This is some big '+U+' text which must be justified on multiple lines. ';
      U := U+U+U+U;
      DrawTextU(U);
      NewLine;
      TextAlign := taLeft;
      DrawTitle(edt1.Text,true);
      for i := 1 to 10 do
        DrawText('This is some text '+IntToStr(i));
      NewLine;
      DrawBMP(Bmp,maxInt,50,'Some bitmap in the report');
      AddBookMark('bookmarkname');
      WordWrapLeftCols := true;
      AddColumns([10,20,50]);
      AddColumnHeaders(['#','Two','Three'],true,true);
      for i := 1 to 100 do
        DrawTextAcrossCols([IntToStr(i),'Column '+IntToStr(i),'Some text here. '+s]);
      NewLine;
      DrawBMP(Bmp,maxInt,50,'Some bitmap in the report (twice)');
      DrawTitle('This is your text',false,0,'','bookmarkname');
      DrawText(mmo1.Text);

      EndDoc;
      ForceInternalAntiAliasedFontFallBack := true;
      ForceNoAntiAliased := true;
      //ForceInternalAntiAliased := false;
      ExportPDFAuthor := 'A.Bouchez';
      ExportPDFSubject := 'This is some sample file';
      // set optional PDF export options
      // ExportPDFForceJPEGCompression := 80;
      // ExportPDFEmbeddedTTF := true;
      // ExportPDFUseUniscribe := true;
      // ExportPDFA1 := true;
      //ExportPDF('test.pdf',true,true); close; exit;
      // show a preview form, and allow basic actions via corresponding buttons
      // ForceInternalAntiAliased := true;
      // ForceInternalAntiAliasedFontFallBack := true;
      ShowPreviewForm;
  
    finally
      Free;
    end;
  finally
    Bmp.Free;
  end;
end;


procedure TForm1.FormShow(Sender: TObject);
var FN: TFileName;
    M: TMetaFile;
    i: integer;
begin
  exit;
  //btn1Click(nil); Close; exit;
  //Button1Click(nil); Close; exit;
  with TPdfDocument.Create do
  try
    for i := 0 to 24 do begin
      AddPage;
      M := TMetaFile.Create;
      M.LoadFromFile(IntToStr(i)+'.emf');
      Canvas.RenderMetaFile(M,Canvas.Page.PageHeight/M.Height*1.3);
      M.Free;
    end;
{    AddPage;
    with Canvas do
    begin
      SetFont('Arial',12,[fsBold]);
      TextOut(100,500,'Test');
      MoveTo(100,400);
      LineTo(500,500);
      Stroke;
    end; }
    FN := ChangeFileExt(ExeVersion.ProgramFileName,'.pdf');
    SaveToFile(FN);
    ShellExecute(Handle,nil,pointer(FN),nil,nil,SW_SHOWNORMAL);
  finally
    Free;
  end;
  Close;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i, y: integer;
  TestImage: TBitmap;
  MF: TMetaFile;
  R: TRect;
begin
  with TGDIPages.Create(self) do
  try
    BeginDoc;
    MF := TMetafile.Create;
    MF.LoadFromFile('d:\download\Sample (1).emf');
    DrawGraphic(MF,0,PaperSize.cx-20);
{    for y := 0 to 4 do
    begin

      DrawTitle(edt1.Text, true);
      for i := 1 to 10 do
        DrawText('This is some text ' + IntToStr(i));
      NewLine;

      TestImage := TBitmap.Create;
      try
        TestImage.Width := 500;
        TestImage.Height := 500;

        TestImage.Canvas.Pen.Color := clRed;
        TestImage.Canvas.MoveTo(0, y * 80);
        TestImage.Canvas.LineTo(TestImage.Width, y * 80);

        DrawBMP(TestImage, maxInt, RightMarginPos);
      finally
        TestImage.Free;
      end;

      NewPage;
    end;}

    EndDoc;
    ForceInternalAntiAliased := true;
    ForceInternalAntiAliasedFontFallBack := true;
    ExportPDFGeneratePDF15File := true;
    ExportPDFUseUniscribe := true;
    ShowPreviewForm;
    //ExportPDF('test.pdf', true, true);
  finally
    Free;
  end;
end;
{
var
  xRect: TRect;
const
  Text: WideString = 'RERERERE:';
begin
  with TPdfDocumentGDI.Create do
  try
    PDFA1 := true;
    AddPage;
    UseUniScribe := false; //uniscribe does not change anything about the problem
    with VCLCanvas do begin
      Font.Name := 'Tahoma';
      Font.Size := 8;
      Font.Style := [fsBold];
      Pen.Color := $AAAAAA;

      xRect := Rect(0, 0, TextWidth(Text), TextHeight(Text));
      OffsetRect(xRect, 100, 100);

      Rectangle(xRect);

      Windows.ExtTextOutW(Handle, xRect.Left, xRect.Top, ETO_CLIPPED,
        @xRect, PWideChar(Text), Length(Text), nil);

      Font.Size := 24;

      xRect := Rect(0, 0, TextWidth(Text), TextHeight(Text));
      OffsetRect(xRect, 100, 200);

      Rectangle(xRect);

      Windows.ExtTextOutW(Handle, xRect.Left, xRect.Top, ETO_CLIPPED,
        @xRect, PWideChar(Text), Length(Text), nil);

    end;
    SaveToFile('TestVcl.pdf');
    ShellExecute(Handle,nil,'TestVcl.pdf',nil,nil,SW_SHOWNORMAL);
  finally
    Free;
  end;
end;
}

end.

