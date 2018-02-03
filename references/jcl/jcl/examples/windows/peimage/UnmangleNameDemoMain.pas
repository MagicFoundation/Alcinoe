unit UnmangleNameDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, JclPeImage;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    OpenBtn: TButton;
    OpenDialog1: TOpenDialog;
    PeFileLabel: TLabel;
    PackageLabel: TLabel;
    FilenameLabel: TLabel;
    PackageDescrLabel: TLabel;
    PackageVerLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
  private
    BorImage: TJclPeBorImage;
  public
    procedure UpdateInfo;
    class procedure LabelCaptionParam(Lbl: TLabel; const StringParam: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  ComObj, TypInfo,
  JclSysInfo, JclSysUtils, JclWin32;

// Demonstrates creating custom resource item classes

type
  TJclPeResourceStringItem = class (TJclPeResourceItem)
  public
    function GetItemIDString(const ItemID: Word): string;
  end;

  TJclDemoPeBorImage = class (TJclPeBorImage)
  protected
    function ResourceItemCreate(AEntry: PImageResourceDirectoryEntry;
      AParentItem: TJclPeResourceItem): TJclPeResourceItem; override;
  public
    function ResourceStringValue(const ID: Word): string;
  end;

{ TJclPeResourceStringItem }

function TJclPeResourceStringItem.GetItemIDString(const ItemID: Word): string;
var
  P: PWChar;
  Cnt: Cardinal;
  Len: Word;
begin
  Result := '';
  Assert(IsDirectory);
  P := List[0].RawEntryData;
  Cnt := 0;
  while Cnt < 16 do
  begin
    Len := Word(P^);
    if Len > 0 then
    begin
      Inc(P);
      if Cnt = ItemID then
      begin
        Result := PChar(WideCharLenToString(P, Len));
        Exit;
      end;
      Inc(P, Len);
    end else
      Inc(P);
    Inc(Cnt);
  end;
end;

{ TJclDemoPeBorImage }

function TJclDemoPeBorImage.ResourceItemCreate(AEntry: PImageResourceDirectoryEntry;
  AParentItem: TJclPeResourceItem): TJclPeResourceItem;
begin
  if (AParentItem <> nil) and (AParentItem.Level = 1) and (AParentItem.ResourceType = rtString) then
    Result := TJclPeResourceStringItem.Create(Self, AParentItem, AEntry)
  else
    Result := inherited ResourceItemCreate(AEntry, AParentItem);
end;

function TJclDemoPeBorImage.ResourceStringValue(const ID: Word): string;
var
  Item: TJclPeResourceItem;
  BlockID, ItemID: Word;
begin
  Result := '';
  BlockID := (ID div 16) + 1;
  ItemID := ID mod 16;
  Item := ResourceList.FindResource(rtString, IntToStr(BlockID));
  if Item <> nil then
    Result := (Item as TJclPeResourceStringItem).GetItemIDString(ItemID);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  BorImage := TJclDemoPeBorImage.Create;
  OpenDialog1.InitialDir := GetWindowsSystemFolder;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(BorImage);
end;

procedure TForm1.OpenBtnClick(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    FileName := '';
    if Execute then
    begin
      BorImage.FileName := FileName;
//      BorImage.ExportList.SortList(esOrdinal);
      UpdateInfo;
    end;
  end;
end;

procedure TForm1.UpdateInfo;
const
  YesNoText: array [Boolean] of string = ('[NO]', '[YES]');
begin
  ListView1.Items.Count := BorImage.ExportList.Count;
  ListView1.Invalidate;
  LabelCaptionParam(PeFileLabel, YesNoText[BorImage.IsBorlandImage]);
  LabelCaptionParam(PackageLabel, YesNoText[BorImage.IsPackage]);
  LabelCaptionParam(FilenameLabel, BorImage.FileName);
  if BorImage.IsPackage then
  begin
    LabelCaptionParam(PackageDescrLabel, BorImage.PackageInfo.Description);
    LabelCaptionParam(PackageVerLabel, IntToStr(BorImage.PackageCompilerVersion));
  end
  else
  begin
    LabelCaptionParam(PackageDescrLabel, '');
    LabelCaptionParam(PackageVerLabel, '');
  end;
end;

procedure TForm1.ListView1Data(Sender: TObject; Item: TListItem);
var
  Unmangled, OriginalName, S, SectionName: string;
  Descr: TJclBorUmDescription;
  Res: TJclBorUmResult;
  TI: PTypeInfo;
  TD: PTypeData;
  ResString: PResStringRec;
begin
  with Item do
  begin
    OriginalName := BorImage.ExportList[Index].Name;
    Res := PeBorUnmangleName(OriginalName, Unmangled, Descr);
    if Res = urOk then
    begin
      Caption := Unmangled;
      S := Copy(GetEnumName(TypeInfo(TJclBorUmSymbolKind), Integer(Descr.Kind)), 3, 255);
      if smQualified in Descr.Modifiers then S := S + ' [Q]';
      if smLinkProc in Descr.Modifiers then S := S + ' [L]';
      SubItems.Add(S);
      case Descr.Kind of
        skRTTI:
          begin
            TI := BorImage.ExportList[Index].MappedAddress;
            SubItems.Add(Copy(GetEnumName(TypeInfo(TTypeKind), Integer(TI^.Kind)), 3, 255));
            SubItems.Add(string(TI^.Name));
            TD := GetTypeData(TI);
            case TI^.Kind of
              tkInterface:
                SubItems.Add(GUIDToString(TD^.Guid));
              tkMethod:
                SubItems.Add(GetEnumName(TypeInfo(TMethodKind), Integer(TD^.MethodKind)));
            end;
          end;
        skData:
          begin
            SectionName := BorImage.ExportList[Index].SectionName;
            SubItems.Add(SectionName);
            if (smQualified in Descr.Modifiers) and (SectionName = 'CODE') then
            begin // Exported data in CODE section are resourcestrings
              ResString := BorImage.ExportList[Index].MappedAddress;
              SubItems.Add(Format('ResString ID: %d', [ResString^.Identifier]));
              SubItems.Add(TJclDemoPeBorImage(BorImage).ResourceStringValue(ResString^.Identifier));
            end;
          end;
      end;
    end else
    begin // Not mangled or Microsoft compiler
      PeUnmangleName(OriginalName, Unmangled);
      Caption := Unmangled;
      SubItems.Add(GetEnumName(TypeInfo(TJclBorUmResult), Integer(Res)));
    end;
  end;
end;

class procedure TForm1.LabelCaptionParam(Lbl: TLabel; const StringParam: string);
var
  I: Integer;
begin
  with Lbl do
  begin
    I := Pos(':', Caption);
    if I = 0 then
      Caption := Caption + ': ' + StringParam
    else
      Caption := Copy(Caption, 1, I) + ' ' + StringParam;
  end;
end;

end.
