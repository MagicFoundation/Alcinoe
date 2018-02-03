unit VerInfoDemoMain;

interface

{$I jcl.inc}
{$I windowsonly.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    procedure FileListBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  JclFileUtils, JclStrings, JclSysUtils;

{ TForm1 }

procedure TForm1.FileListBox1Change(Sender: TObject);
var
  FileName: TFileName;
  I: Integer;
begin
  FileName := FileListBox1.FileName;
  Memo1.Lines.BeginUpdate;
  try
    Memo1.Lines.Clear;

    if VersionResourceAvailable(FileName) then
      with TJclFileVersionInfo.Create(FileName) do
      try
        for I := 0 to LanguageCount - 1 do
        begin
          LanguageIndex := I;
          Memo1.Lines.Add(Format('[%s] %s', [LanguageIds[I], LanguageNames[I]]));
          Memo1.Lines.Add(StringOfChar('-', 80));
          Memo1.Lines.AddStrings(Items);
          Memo1.Lines.Add(BinFileVersion);
          Memo1.Lines.Add(OSIdentToString(FileOS));
          Memo1.Lines.Add(OSFileTypeToString(FileType, FileSubType));
          Memo1.Lines.Add('');
        end;
        Memo1.Lines.Add('Translations:');
        for I := 0 to TranslationCount - 1 do
          Memo1.Lines.Add(VersionLanguageId(Translations[I]));
        Memo1.Lines.Add(BooleanToStr(TranslationMatchesLanguages));
      finally
        Free;
      end;

  finally
    Memo1.Lines.EndUpdate;
  end;
end;

end.
