unit ClrDemoBlobForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JclCLR, StdCtrls, Buttons, ComCtrls;

type
  TfrmBlobs = class(TForm)
    btnOK: TBitBtn;
    lstBlobs: TListView;
    memDump: TMemo;
    procedure lstBlobsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lstBlobsData(Sender: TObject; Item: TListItem);
  private
    FStream: TJclCLRBlobStream;
    procedure ShowBlobs(const AStream: TJclCLRBlobStream);
  public
    class procedure Execute(const AStream: TJclCLRBlobStream);
  end;

var
  frmBlobs: TfrmBlobs;

implementation

uses ClrDemoAbstractFrame;

{$R *.DFM}

{ TfrmBlobs }

class procedure TfrmBlobs.Execute(const AStream: TJclCLRBlobStream);
begin
  with TfrmBlobs.Create(nil) do
  try
    ShowBlobs(AStream);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmBlobs.ShowBlobs(const AStream: TJclCLRBlobStream);
begin
  FStream := AStream;
  lstBlobs.Items.Count := FStream.BlobCount;
end;

procedure TfrmBlobs.lstBlobsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
  with TJclCLRBlobRecord(Item.Data) do
    TfrmAbstract.DumpBuf(Memory, Size, memDump,
      FStream.Offset + DWORD(Memory) - DWORD(FStream.Data));
end;

procedure TfrmBlobs.lstBlobsData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := IntToStr(Item.Index);
  Item.Data    := FStream.Blobs[Item.Index];

  Item.SubItems.Add('$' +
    IntToHex(FStream.Blobs[Item.Index].Offset, 8));
  Item.SubItems.Add(IntToStr(FStream.Blobs[Item.Index].Size));
end;

end.
