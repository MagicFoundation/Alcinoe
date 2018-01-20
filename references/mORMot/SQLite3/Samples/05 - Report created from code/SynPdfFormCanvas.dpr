program SynPdfFormCanvas;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  Math,
  DateUtils,
  SynCommons,
  SynPDF;

const
  PDFFactor: Single = 72.0 / 2.54;

var
  obPDF: TPdfDocument;
  obFormCanvas: TPdfFormWithCanvas;

begin
  obPDF := TPdfDocument.Create(false,0,false);
  obPDF.GeneratePDF15File := true;
  obPDF.DefaultPaperSize := psA4;
  obPDF.DefaultPageLandscape := false;
  obPDF.CompressionMethod := cmFlateDecode;

  obFormCanvas := TPdfFormWithCanvas.Create(obPDF,Trunc(5.0*PDFFactor),Trunc(5.0*PDFFactor));
  obPDF.AddXObject('FORMOBJECT',obFormCanvas);

  obFormCanvas.Canvas.SetTextRenderingMode(trFill);
  obFormCanvas.Canvas.SetFont('Arial',10.0,[]);

  obFormCanvas.Canvas.SetLineWidth(0.01*PDFFactor);

  obFormCanvas.Canvas.Rectangle(0.0*PDFFactor,0.0*PDFFactor,4.9*PDFFactor,4.9*PDFFactor);
  obFormCanvas.Canvas.Stroke;

  obFormCanvas.Canvas.TextOut(1.0*PDFFactor,2.5*PDFFactor,'form text');

  obFormCanvas.CloseCanvas;

  obPDF.AddPage;

  obPDF.Canvas.SetTextRenderingMode(trFill);
  obPDF.Canvas.SetFont('Arial',10.0,[]);

  obPDF.Canvas.SetLineWidth(0.01*PDFFactor);

  obPDF.Canvas.Rectangle(1.0*PDFFactor,1.0*PDFFactor,19.0*PDFFactor,27.9*PDFFactor);
  obPDF.Canvas.Stroke;

  obPDF.Canvas.TextOut(2.0*PDFFactor,27.0*PDFFactor,'XObject form canvas sample');

  obPDF.Canvas.DrawXObject(2.0*PDFFactor,5.0*PDFFactor,1.0,1.0,'FORMOBJECT');
  obPDF.Canvas.DrawXObject(10.0*PDFFactor,10.0*PDFFactor,1.0,0.5,'FORMOBJECT');
  obPDF.Canvas.DrawXObject(8.0*PDFFactor,15.0*PDFFactor,2.0,2.0,'FORMOBJECT');
  obPDF.Canvas.DrawXObject(2.0*PDFFactor,20.0*PDFFactor,0.5,1.0,'FORMOBJECT');

  obPDF.SaveToFile(ChangeFileExt(ExeVersion.ProgramFileName,'.pdf'));

  FreeAndNil(obPDF);
end.
