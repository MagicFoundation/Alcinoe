unit TextConverterMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  JclStreams;

type
  TForm1 = class(TForm)
    ButtonAnsiToAnsi: TButton;
    ButtonAnsiToUTF8: TButton;
    ButtonAnsiToUTF16: TButton;
    ButtonUTF8ToAnsi: TButton;
    ButtonUTF8ToUTF8: TButton;
    ButtonUTF8ToUTF16: TButton;
    ButtonUTF16ToAnsi: TButton;
    ButtonUTF16ToUTF8: TButton;
    ButtonUTF16ToUTF16: TButton;
    OpenDialogTxt: TOpenDialog;
    SaveDialogTxt: TSaveDialog;
    ButtonAutoToAnsi: TButton;
    ButtonAutoToUTF8: TButton;
    ButtonAutoToUTF16: TButton;
    procedure ButtonAnsiToUTF8Click(Sender: TObject);
    procedure ButtonAnsiToUTF16Click(Sender: TObject);
    procedure ButtonUTF8ToAnsiClick(Sender: TObject);
    procedure ButtonUTF8ToUTF16Click(Sender: TObject);
    procedure ButtonUTF16ToAnsiClick(Sender: TObject);
    procedure ButtonUTF16ToUTF8Click(Sender: TObject);
    procedure ButtonAutoToAnsiClick(Sender: TObject);
    procedure ButtonAutoToUTF8Click(Sender: TObject);
    procedure ButtonAutoToUTF16Click(Sender: TObject);
  private
  public
    procedure ConvertFile(ReaderClass, WriterClass: TJclStringStreamClass);
  end;

var
  Form1: TForm1;

implementation

uses
  JclStrings, JclFileUtils;
  
{$R *.dfm}

procedure TForm1.ButtonAnsiToUTF8Click(Sender: TObject);
begin
  ConvertFile(TJclAnsiStream, TJclUTF8Stream);
end;

procedure TForm1.ButtonAutoToAnsiClick(Sender: TObject);
begin
  ConvertFile(TJclAutoStream, TJclAnsiStream);
end;

procedure TForm1.ButtonAutoToUTF16Click(Sender: TObject);
begin
  ConvertFile(TJclAutoStream, TJclUTF16Stream);
end;

procedure TForm1.ButtonAutoToUTF8Click(Sender: TObject);
begin
  ConvertFile(TJclAutoStream, TJclUTF8Stream);
end;

procedure TForm1.ButtonAnsiToUTF16Click(Sender: TObject);
begin
  ConvertFile(TJclAnsiStream, TJclUTF16Stream);
end;

procedure TForm1.ButtonUTF8ToAnsiClick(Sender: TObject);
begin
  ConvertFile(TJclUTF8Stream, TJclAnsiStream);
end;

procedure TForm1.ButtonUTF8ToUTF16Click(Sender: TObject);
begin
  ConvertFile(TJclUTF8Stream, TJclUTF16Stream);
end;

procedure TForm1.ButtonUTF16ToAnsiClick(Sender: TObject);
begin
  ConvertFile(TJclUTF16Stream, TJclAnsiStream);
end;

procedure TForm1.ButtonUTF16ToUTF8Click(Sender: TObject);
begin
  ConvertFile(TJclUTF16Stream, TJclUTF8Stream);
end;

procedure TForm1.ConvertFile(ReaderClass,
  WriterClass: TJclStringStreamClass);
  procedure ConvertFiles(const SourceFileName, DestFileName: TFileName);
  var
    SourceStream, DestStream: TStream;
    Reader, Writer: TJclStringStream;
  begin
    SourceStream := TFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyWrite);
    try
      DestStream := TFileStream.Create(DestFileName, fmCreate);
      try
        Reader := ReaderClass.Create(SourceStream, False);
        try
          Writer := WriterClass.Create(DestStream, False);
          try
            Reader.SkipBOM;
            Writer.WriteBOM;
            JclStreams.WideStringStreamCopy(Reader, Writer);
            Writer.Flush;
          finally
            Writer.Free;
          end;
        finally
          Reader.Free;
        end;
      finally
        DestStream.Free;
      end;
    finally
      SourceStream.Free;
    end;
  end;
var
  SourceFileName, DestFileName, TmpFileName: TFileName;
begin
  if OpenDialogTxt.Execute and SaveDialogTxt.Execute then
  begin
    SourceFileName := OpenDialogTxt.FileName;
    DestFileName := SaveDialogTxt.FileName;
    if StrSame(SourceFileName, DestFileName) then
    begin
      // in place conversion
      TmpFileName := FileGetTempName('');
      ConvertFiles(SourceFileName, TmpFileName);
      FileMove(TmpFileName, DestFileName, True);
    end
    else
      ConvertFiles(SourceFileName, DestFileName);
  end;
end;

end.
