unit PeFuncDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    ExportsListBox: TListBox;
    ImportsListBox: TListBox;
    ImportedLibsListBox: TListBox;
    BitmapResListBox: TListBox;
    OpenBtn: TButton;
    IconsListBox: TListBox;
    FormsListBox: TListBox;
    FileNameLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    CursorsListBox: TListBox;
    Label7: TLabel;
    procedure OpenBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure BeginUpdateListBoxes;
    procedure EndUpdateListBoxes;
    procedure UpdateViews(const FileName: TFileName);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  JclPeImage;

procedure TForm1.BeginUpdateListBoxes;
var
  I: Integer;
  C: TComponent;
begin
  for I := 0 to ComponentCount - 1 do
  begin
    C := Components[I];
    if C is TListBox then
      with TListBox(C) do
      begin
        Items.BeginUpdate;
        Items.Clear;
      end;
  end;
end;

procedure TForm1.EndUpdateListBoxes;
var
  I, Extent: Integer;
  C: TComponent;
begin
  for I := 0 to ComponentCount - 1 do
  begin
    C := Components[I];
    if C is TListBox then
      with TListBox(C) do
      begin
        ItemIndex := -1;
        if Items.Count > 0 then
          Extent := Tag
        else
          Extent := 0;
        SendMessage(Handle, LB_SETHORIZONTALEXTENT, Extent, 0);
        Items.EndUpdate;
      end;
  end;
end;

procedure TForm1.OpenBtnClick(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    FileName := '';
    if Execute then
      if IsValidPeFile(FileName) then
        UpdateViews(FileName)
      else
        ShowMessageFmt('The file "%s" is not valid PE file.', [FileName]);
  end;
end;

procedure TForm1.UpdateViews(const FileName: TFileName);
begin
  BeginUpdateListBoxes;
  Screen.Cursor := crHourGlass;
  try
    FileNameLabel.Caption := FileName;

    // Exported functions
    PeExportedFunctions(FileName, ExportsListBox.Items);
    // Imported functions
    PeImportedFunctions(FileName, ImportsListBox.Items, '', True);
    // Imported libraries (not recursive)
    PeImportedLibraries(FileName, ImportedLibsListBox.Items, False, False);
    // VCL form names
    PeBorFormNames(FileName, FormsListBox.Items);
    // Bitmap, Icon and Cursor names
    PeResourceKindNames(FileName, rtBitmap, BitmapResListBox.Items);
    PeResourceKindNames(FileName, rtIcon, IconsListBox.Items);
    PeResourceKindNames(FileName, rtCursor, CursorsListBox.Items);
  finally
    Screen.Cursor := crDefault;
    EndUpdateListBoxes;
  end;
end;

end.
