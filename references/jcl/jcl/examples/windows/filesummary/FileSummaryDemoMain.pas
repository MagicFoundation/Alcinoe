unit FileSummaryDemoMain;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileCtrl, ActiveX, JclNTFS;

type
  TFormMain = class(TForm)
    DriveComboBox1: TDriveComboBox;
    Memo1: TMemo;
    FileListBox1: TFileListBox;
    DirectoryListBox1: TDirectoryListBox;
    procedure FileListBox1Change(Sender: TObject);
  private
    FFileSummary: TJclFileSummary;
    procedure UpdateFileSummary(const FileName: string);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  JclSysUtils;

procedure TFormMain.FileListBox1Change(Sender: TObject);
begin
  if FileListBox1.FileName <> '' then
    UpdateFileSummary(FileListBox1.FileName);
end;

procedure TFormMain.UpdateFileSummary(const FileName: string);
  function FileTimeToString(const FileTime: TFileTime): string;
  var
    ASystemTime: TSystemTime;
  begin
    if FileTimeToSystemTime(FileTime, ASystemTime) then
      Result := Format('%d/%d/%d %d:%d:%d', [ASystemTime.wYear, ASystemTime.wMonth, ASystemTime.wDay,
        ASystemTime.wHour, ASystemTime.wMinute, ASystemTime.wSecond])
    else
      Result := '';
  end;
var
  AFilePropertySet: TJclFilePropertySet;
  AFileSummaryInformation: TJclFileSummaryInformation;
  ADocumentSummaryInformation: TJclDocSummaryInformation;
  AMediaFileSummaryInformation: TJclMediaFileSummaryInformation;
  AMSISummaryInformation: TJclMSISummaryInformation;
  AVideoSummaryInformation: TJclVideoSummaryInformation;
  AAudioSummaryInformation: TJclAudioSummaryInformation;
begin
  Memo1.Lines.Clear;
  FFileSummary := TJclFileSummary.Create(FileName, fsaRead, fssDenyAll);
  try
    FFileSummary.GetPropertySet(TJclFileSummaryInformation, AFileSummaryInformation);
    if Assigned(AFileSummaryInformation) then
    try
      Memo1.Lines.Add('File summary');

      Memo1.Lines.Add(string('  Title ' + AFileSummaryInformation.Title));
      Memo1.Lines.Add(string('  Subject ' + AFileSummaryInformation.Subject));
      Memo1.Lines.Add(string('  Author ' + AFileSummaryInformation.Author));
      Memo1.Lines.Add(string('  Keywords ' + AFileSummaryInformation.KeyWords));
      Memo1.Lines.Add(string('  Comments ' + AFileSummaryInformation.Comments));
      Memo1.Lines.Add(string('  Template ' + AFileSummaryInformation.Template));
      Memo1.Lines.Add(string('  Last author ' + AFileSummaryInformation.LastAuthor));
      Memo1.Lines.Add(string('  Revision numer ' + AFileSummaryInformation.RevNumber));
      Memo1.Lines.Add(string('  Edit time ' + FileTimeToString(AFileSummaryInformation.EditTime)));
      Memo1.Lines.Add(string('  Last printed time ' + FileTimeToString(AFileSummaryInformation.LastPrintedTime)));
      Memo1.Lines.Add(string('  Creation time ' + FileTimeToString(AFileSummaryInformation.CreationTime)));
      Memo1.Lines.Add(string('  Last save time ' + FileTimeToString(AFileSummaryInformation.LastSaveTime)));
      Memo1.Lines.Add('  Page count ' + IntToStr(AFileSummaryInformation.PageCount));
      Memo1.Lines.Add('  Word count ' + IntToStr(AFileSummaryInformation.WordCount));
      Memo1.Lines.Add('  Char count ' + IntToStr(AFileSummaryInformation.CharCount));
      //AFileSummaryInformation.Thumnail
      Memo1.Lines.Add(string('  App name ' + AFileSummaryInformation.AppName));
      Memo1.Lines.Add('  Security ' + IntToStr(AFileSummaryInformation.Security));
    finally
      AFileSummaryInformation.Free;
    end;

    FFileSummary.GetPropertySet(TJclDocSummaryInformation, ADocumentSummaryInformation);
    if Assigned(ADocumentSummaryInformation) then
    try
      Memo1.Lines.Add('Document summary');
      Memo1.Lines.Add(string('  Category ' + ADocumentSummaryInformation.Category));
      Memo1.Lines.Add(string('  Pres format ' + ADocumentSummaryInformation.PresFormat));
      Memo1.Lines.Add('  Byte count ' + IntToStr(ADocumentSummaryInformation.ByteCount));
      Memo1.Lines.Add('  Line count ' + IntToStr(ADocumentSummaryInformation.LineCount));
      Memo1.Lines.Add('  Par count ' + IntToStr(ADocumentSummaryInformation.ParCount));
      Memo1.Lines.Add('  Slide count ' + IntToStr(ADocumentSummaryInformation.SlideCount));
      Memo1.Lines.Add('  Note count ' + IntToStr(ADocumentSummaryInformation.NoteCount));
      Memo1.Lines.Add('  Hidden count ' + IntToStr(ADocumentSummaryInformation.HiddenCount));
      Memo1.Lines.Add('  MM Clip count ' + IntToStr(ADocumentSummaryInformation.MMClipCount));
      Memo1.Lines.Add('  Scale ' + BooleanToStr(ADocumentSummaryInformation.Scale));
      //ADocumentSummaryInformation.HeadingPair
      //ADocumentSummaryInformation.DocParts
      Memo1.Lines.Add(string('  Manager ' + ADocumentSummaryInformation.Manager));
      Memo1.Lines.Add(string('  Company ' + ADocumentSummaryInformation.Company));
      Memo1.Lines.Add('  Links dirty ' + BooleanToStr(ADocumentSummaryInformation.LinksDirty));
    finally
      ADocumentSummaryInformation.Free;
    end;

    FFileSummary.GetPropertySet(TJclMediaFileSummaryInformation, AMediaFileSummaryInformation);
    if Assigned(AMediaFileSummaryInformation) then
    try
      Memo1.Lines.Add('Media file');
      Memo1.Lines.Add('  Supplier ' + AMediaFileSummaryInformation.Supplier);
      Memo1.Lines.Add('  Source ' + AMediaFileSummaryInformation.Source);
      Memo1.Lines.Add('  Sequence no ' + AMediaFileSummaryInformation.SequenceNo);
      Memo1.Lines.Add('  Project ' + AMediaFileSummaryInformation.Project);
      Memo1.Lines.Add('  Status ' + IntToStr(AMediaFileSummaryInformation.Status));
      Memo1.Lines.Add('  Owner ' + AMediaFileSummaryInformation.Owner);
      Memo1.Lines.Add('  Rating ' + AMediaFileSummaryInformation.Rating);
      Memo1.Lines.Add('  Production ' + FileTimeToString(AMediaFileSummaryInformation.Production));
      Memo1.Lines.Add('  Copyright ' + AMediaFileSummaryInformation.Copyright);
    finally
      AMediaFileSummaryInformation.Free;
    end;

    FFileSummary.GetPropertySet(TJclMSISummaryInformation, AMSISummaryInformation);
    if Assigned(AMSISummaryInformation) then
    try
      Memo1.Lines.Add('MSI summary');
      Memo1.Lines.Add('  Version ' + IntToStr(AMSISummaryInformation.Version));
      Memo1.Lines.Add('  Source ' + IntToStr(AMSISummaryInformation.Source));
      Memo1.Lines.Add('  Restrict ' + IntToStr(AMSISummaryInformation.Restrict));
    finally
      AMSISummaryInformation.Free;
    end;

    FFileSummary.GetPropertySet(TJclShellSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Shell summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclStorageSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Storage summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclImageSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Image summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclDisplacedSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Displaced summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclBriefCaseSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Briefcase summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclMiscSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Misc summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclWebViewSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Webview summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclMusicSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Music summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclDRMSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('DRM summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclVideoSummaryInformation, AVideoSummaryInformation);
    if Assigned(AVideoSummaryInformation) then
    try
      Memo1.Lines.Add('Video summary');
      Memo1.Lines.Add('  Stream name ' + AVideoSummaryInformation.StreamName);
      Memo1.Lines.Add('  Width ' + IntToStr(AVideoSummaryInformation.Width));
      Memo1.Lines.Add('  Height ' + IntToStr(AVideoSummaryInformation.Height));
      Memo1.Lines.Add('  Time length(ms) ' + IntToStr(AVideoSummaryInformation.TimeLength));
      Memo1.Lines.Add('  Frame count ' + IntToStr(AVideoSummaryInformation.FrameCount));
      Memo1.Lines.Add('  Frame rate ' + IntToStr(AVideoSummaryInformation.FrameRate));
      Memo1.Lines.Add('  Data rate ' + IntToStr(AVideoSummaryInformation.DataRate));
      Memo1.Lines.Add('  Sample size ' + IntToStr(AVideoSummaryInformation.SampleSize));
      Memo1.Lines.Add('  Compression ' + AVideoSummaryInformation.Compression);
      Memo1.Lines.Add('  Stream number ' + IntToStr(AVideoSummaryInformation.StreamNumber));
    finally
      AVideoSummaryInformation.Free;
    end;

    FFileSummary.GetPropertySet(TJclAudioSummaryInformation, AAudioSummaryInformation);
    if Assigned(AAudioSummaryInformation) then
    try
      Memo1.Lines.Add('Audio summary');
      Memo1.Lines.Add('  Format ' + AAudioSummaryInformation.Format);
      Memo1.Lines.Add('  Time length ' + IntToStr(AAudioSummaryInformation.TimeLength));
      Memo1.Lines.Add('  Average data rate ' + IntToStr(AAudioSummaryInformation.AverageDataRate));
      Memo1.Lines.Add('  Sample rate ' + IntToStr(AAudioSummaryInformation.SampleRate));
      Memo1.Lines.Add('  Sample size ' + IntToStr(AAudioSummaryInformation.SampleSize));
      Memo1.Lines.Add('  Channel count ' + IntToStr(AAudioSummaryInformation.ChannelCount));
      Memo1.Lines.Add('  Stream number ' + IntToStr(AAudioSummaryInformation.StreamNumber));
      Memo1.Lines.Add('  Stream name ' + AAudioSummaryInformation.StreamName);
      Memo1.Lines.Add('  Compression ' + AAudioSummaryInformation.Compression);
    finally
      AAudioSummaryInformation.Free;
    end;

    FFileSummary.GetPropertySet(TJclControlPanelSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Control panel summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclVolumeSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Volume summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclShareSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Share summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclLinkSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Link summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclQuerySummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Query summary');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclImageInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Image');
    finally
      AFilePropertySet.Free;
    end;

    FFileSummary.GetPropertySet(TJclJpegSummaryInformation, AFilePropertySet);
    if Assigned(AFilePropertySet) then
    try
      Memo1.Lines.Add('Jpeg summary');
    finally
      AFilePropertySet.Free;
    end;
  finally
    FreeAndNil(FFileSummary);
  end;

  if Memo1.Lines.Count = 0 then
    Memo1.Lines.Add('No properties');
end;

end.
