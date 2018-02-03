unit ClrDemoMetaDataFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ClrDemoAbstractFrame, JclCLR, StdCtrls, ComCtrls, ExtCtrls, Menus;

type
  TfrmMetadata = class(TfrmAbstract)
    pnlVer: TPanel;
    lblVer: TLabel;
    edtVer: TEdit;
    edtVerStr: TEdit;
    lblVerStr: TLabel;
    lstStream: TListView;
  private
    { Private declarations }
  public
    procedure ShowInfo(const ACLR: TJclCLRHeaderEx); override;
  end;

var
  frmMetadata: TfrmMetadata;

implementation

{$R *.DFM}

{ TfrmMetadata }

procedure TfrmMetadata.ShowInfo(const ACLR: TJclCLRHeaderEx);
var
  I: Integer;
begin
  with ACLR.Metadata do
  begin
    edtVer.Text    := Version;
    edtVerStr.Text := VersionString;

    with lstStream.Items do
    begin
      Clear;
      for I:=0 to StreamCount-1 do
      with Add do
      begin
        Caption := Streams[I].Name;
        Data    := Streams[I];
        SubItems.Add('$' + IntToHex(Streams[I].Offset, 8));
        SubItems.Add(IntToStr(Streams[I].Size));
      end;
    end;
  end;
end;

end.
