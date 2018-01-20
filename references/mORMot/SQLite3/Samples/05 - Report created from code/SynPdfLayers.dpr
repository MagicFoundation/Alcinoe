program SynPdfLayers;

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

  obMainLayer1: TPdfOptionalContentGroup;
  obMainLayer2: TPdfOptionalContentGroup;
  obMainLayer3: TPdfOptionalContentGroup;

  obSubLayer1: TPdfOptionalContentGroup;
  obSubLayer2: TPdfOptionalContentGroup;
  obSubLayer3: TPdfOptionalContentGroup;

  obSubSubLayer1: TPdfOptionalContentGroup;
  obSubSubLayer2: TPdfOptionalContentGroup;

  obRadioLayer1: TPdfOptionalContentGroup;
  obRadioLayer2: TPdfOptionalContentGroup;
  obRadioLayer3: TPdfOptionalContentGroup;


begin
  obPDF := TPdfDocument.Create(false,0,false);
  obPDF.UseOptionalContent := true;
  obPDF.DefaultPaperSize := psA4;
  obPDF.DefaultPageLandscape := true;
  obPDF.CompressionMethod := cmFlateDecode;

  obMainLayer1 := obPDF.CreateOptionalContentGroup(nil,'Main Layer 1',true);
  obMainLayer2 := obPDF.CreateOptionalContentGroup(nil,'Main Layer 2',true);
  obMainLayer3 := obPDF.CreateOptionalContentGroup(nil,'Main Layer 3',true);

  obSubLayer1 := obPDF.CreateOptionalContentGroup(obMainLayer1,'Sub Layer 1',true);
  obSubLayer2 := obPDF.CreateOptionalContentGroup(obMainLayer1,'Sub Layer 2',true);
  obSubLayer3 := obPDF.CreateOptionalContentGroup(obMainLayer1,'Sub Layer 3',true);

  obSubSubLayer1 := obPDF.CreateOptionalContentGroup(obSubLayer1,'Sub Sub Layer 1',false);
  obSubSubLayer2 := obPDF.CreateOptionalContentGroup(obSubLayer1,'Sub Sub Layer 2',false);

  obRadioLayer1 := obPDF.CreateOptionalContentGroup(obMainLayer2,'Radio Layer 1',true);
  obRadioLayer2 := obPDF.CreateOptionalContentGroup(obMainLayer2,'Radio Layer 2',false);
  obRadioLayer3 := obPDF.CreateOptionalContentGroup(obMainLayer2,'Radio Layer 3',false);

  // to use the main layers as radios uncomment following line and set only main layer 1 to visible (line 40ff)
  //obPDF.CreateOptionalContentRadioGroup([obMainLayer1,obMainLayer2,obMainLayer3]);

  obPDF.CreateOptionalContentRadioGroup([obRadioLayer1,obRadioLayer2,obRadioLayer3]);

  obPDF.AddPage;

  obPDF.Canvas.SetTextRenderingMode(trFill);
  obPDF.Canvas.SetFont('Arial',10.0,[]);
  obPDF.Canvas.SetLineWidth(0.01*PDFFactor);

  obPDF.Canvas.Rectangle(1.0*PDFFactor,1.0*PDFFactor,27.7*PDFFactor,19.0*PDFFactor);
  obPDF.Canvas.Stroke;

  obPDF.Canvas.TextOut(2.0*PDFFactor,17.0*PDFFactor,'Main Layer 1:');

  obPDF.Canvas.BeginMarkedContent(obMainLayer1);
  begin
    obPDF.Canvas.TextOut(10.0*PDFFactor,17.0*PDFFactor,'Text visible in Main Layer 1');

    obPDF.Canvas.BeginMarkedContent(obSubLayer1);
    begin
      obPDF.Canvas.TextOut(10.0*PDFFactor,15.0*PDFFactor,'Text visible in Sub Layer 1');

      obPDF.Canvas.BeginMarkedContent(obSubSubLayer1);
      obPDF.Canvas.TextOut(15.0*PDFFactor,15.0*PDFFactor,'Text visible in Sub Sub Layer 1');
      obPDF.Canvas.EndMarkedContent;

      obPDF.Canvas.BeginMarkedContent(obSubSubLayer2);
      obPDF.Canvas.TextOut(22.0*PDFFactor,15.0*PDFFactor,'Text visible in Sub Sub Layer 2');
      obPDF.Canvas.EndMarkedContent;
    end;
    obPDF.Canvas.EndMarkedContent;

    obPDF.Canvas.BeginMarkedContent(obSubLayer2);
    begin
      obPDF.Canvas.TextOut(10.0*PDFFactor,14.0*PDFFactor,'Text visible in Sub Layer 2');
    end;
    obPDF.Canvas.EndMarkedContent;

    obPDF.Canvas.BeginMarkedContent(obSubLayer3);
    obPDF.Canvas.TextOut(10.0*PDFFactor,13.0*PDFFactor,'Text visible in Sub Layer 3');
    obPDF.Canvas.EndMarkedContent;
  end;
  obPDF.Canvas.EndMarkedContent;


  obPDF.Canvas.TextOut(2.0*PDFFactor,10.0*PDFFactor,'Main Layer 2:');

  obPDF.Canvas.BeginMarkedContent(obMainLayer2);
  begin
    obPDF.Canvas.TextOut(10.0*PDFFactor,10.0*PDFFactor,'Text visible in Main Layer 2');

    obPDF.Canvas.BeginMarkedContent(obRadioLayer1);
    obPDF.Canvas.TextOut(10.0*PDFFactor,8.0*PDFFactor,'Text visible in Radio Layer 1');
    obPDF.Canvas.EndMarkedContent;

    obPDF.Canvas.BeginMarkedContent(obRadioLayer2);
    obPDF.Canvas.TextOut(10.0*PDFFactor,7.0*PDFFactor,'Text visible in Radio Layer 2');
    obPDF.Canvas.EndMarkedContent;

    obPDF.Canvas.BeginMarkedContent(obRadioLayer3);
    obPDF.Canvas.TextOut(10.0*PDFFactor,6.0*PDFFactor,'Text visible in Radio Layer 3');
    obPDF.Canvas.EndMarkedContent;
  end;
  obPDF.Canvas.EndMarkedContent;

  obPDF.Canvas.TextOut(2.0*PDFFactor,2.0*PDFFactor,'Main Layer 3:');

  obPDF.Canvas.BeginMarkedContent(obMainLayer3);
  obPDF.Canvas.TextOut(10.0*PDFFactor,2.0*PDFFactor,'Text visible in Main Layer 3');
  obPDF.Canvas.EndMarkedContent;

  obPDF.SaveToFile(ChangeFileExt(ExeVersion.ProgramFileName,'.pdf'));

  FreeAndNil(obPDF);
end.
