unit ClrDemoCLRFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ClrDemoAbstractFrame, StdCtrls, JclCLR, CheckLst, ExtCtrls, ComCtrls;

type
  TfrmCLR = class (TfrmAbstract)
    boxFlags: TGroupBox;
    edtEntryPointToken: TEdit;
    edtVer: TEdit;
    lblEntryPointToken: TLabel;
    lblVer: TLabel;
    lstFlags: TCheckListBox;
    lstResources: TListView;
    lstVTableFixups: TListView;
    memResources: TMemo;
    memStrongNameSign: TMemo;
    PC: TPageControl;
    pnlTop: TPanel;
    tsResources: TTabSheet;
    tsStrongNameSign: TTabSheet;
    tsVTableFixup: TTabSheet;
    procedure lstResourcesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    FCLR: TJclClrHeaderEx;
  public
    procedure ShowInfo(const ACLR: TJclCLRHeaderEx); override;
  end;
  
var
  frmCLR: TfrmCLR;

implementation

{$R *.DFM}

uses
  Math, TypInfo, JclMetadata;

{ TfrmCLR }

{
*********************************** TfrmCLR ************************************
}
procedure TfrmCLR.lstResourcesSelectItem(Sender: TObject; Item: TListItem; 
        Selected: Boolean);
begin
  if Selected then
  with TJclCLRResourceRecord(Item.Data) do
    DumpBuf(Memory, Size, memResources, RVA);
end;

procedure TfrmCLR.ShowInfo(const ACLR: TJclCLRHeaderEx);
  
  procedure UpdateFlags;
  var
    AFlag: TJclClrImageFlag;
  begin
    for AFlag:=Low(TJclClrImageFlag) to High(TJclClrImageFlag) do
    begin
      lstFlags.Checked[Integer(AFlag)] := AFlag in ACLR.Flags;
      lstFlags.ItemEnabled[Integer(AFlag)] := False;
    end;
  end;
  
  procedure UpdateEntryPointToken;
  var
    AMethod: TJclCLRTableMethodDefRow;
    ATypeDef: TJclCLRTableTypeDefRow;
  begin
    if Assigned(ACLR.EntryPointToken) then
    begin
      if ACLR.EntryPointToken is TJclCLRTableMethodDefRow then
      begin
        AMethod := ACLR.EntryPointToken as TJclCLRTableMethodDefRow;
        if AMethod.ParentToken is TJclCLRTableTypeDefRow then
        begin
          ATypeDef := AMethod.ParentToken as TJclCLRTableTypeDefRow;
          edtEntryPointToken.Text := ATypeDef.Namespace + '.' + ATypeDef.Name + '.' + AMethod.Name;
        end
        else
          edtEntryPointToken.Text := AMethod.Name;
      end
      else if ACLR.EntryPointToken is TJclCLRTableFileRow then
        edtEntryPointToken.Text := 'External file'
      else
        edtEntryPointToken.Text := '$' + IntToHex(ACLR.Header.EntryPointToken, 8);
    end
    else
      edtEntryPointToken.Text := '(none)';
  end;
  
  procedure UpdateStrongNameSign;
  begin
    if ACLR.HasStrongNameSignature then
      with ACLR.StrongNameSignature, ACLR.Header.StrongNameSignature do
        DumpBuf(Memory, Size, memStrongNameSign,
          VirtualAddress-ACLR.Image.RvaToSection(VirtualAddress).PointerToRawData)
    else
      memStrongNameSign.Clear;
  end;
  
  procedure UpdateResources;
  var
    I: Integer;
  begin
    with lstResources.Items do
    try
      BeginUpdate;
      Clear;
  
      if ACLR.HasResources then
      for I:=0 to ACLR.ResourceCount-1 do
      with Add do
      begin
        Caption := IntToStr(I);
        Data    := ACLR.Resources[I];
        with ACLR.Resources[I] do
        begin
          SubItems.Add('$' + IntToHex(Offset, 8));
          SubItems.Add('$' + IntToHex(RVA, 8));
          SubItems.Add(IntToStr(Size));
        end;
      end
      else
        memResources.Clear;
    finally
      EndUpdate;
    end;
  end;
  
  function FormatVTableKinds(const Kinds: TJclClrVTableKinds): string;
  var
    AKind: TJclClrVTableKind;
  begin
    Result := '';
    for AKind:=Low(TJclClrVTableKind) to High(TJclClrVTableKind) do
      if AKind in Kinds then
        Result := Result + GetEnumName(TypeInfo(TJclClrVTableKind), Integer(AKind)) + ' ';
  end;
  
  procedure UpdateVTableFixups;
  var
    I: Integer;
  begin
    with lstVTableFixups.Items do
    try
      BeginUpdate;
      Clear;
      if ACLR.HasVTableFixup then
      for I:=0 to ACLR.VTableFixupCount-1 do
      with Add do
      begin
        Caption := IntToStr(I);
        Data    := ACLR.VTableFixups[I];
        with ACLR.VTableFixups[I] do
        begin
          SubItems.Add('$' + IntToHex(RVA, 8));
          SubItems.Add(IntToStr(Count));
          SubItems.Add(FormatVTableKinds(Kinds));
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
  
begin
  FCLR := ACLR;
  
  edtVer.Text := ACLR.VersionString;
  UpdateEntryPointToken;
  UpdateFlags;
  UpdateStrongNameSign;
  UpdateResources;
  UpdateVTableFixups;
end;

end.

