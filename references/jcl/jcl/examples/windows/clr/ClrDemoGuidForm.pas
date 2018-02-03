unit ClrDemoGuidForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, JclCLR;

type
  TfrmGuid = class(TForm)
    btnOK: TBitBtn;
    lstGuids: TListView;
    procedure lstGuidsData(Sender: TObject; Item: TListItem);
  private
    FStream: TJclCLRGuidStream;
    procedure ShowGuids(const AStream: TJclCLRGuidStream);
  public
    class procedure Execute(const AStream: TJclCLRGuidStream);
  end;

implementation

{$R *.DFM}

uses
  ComObj;

{ TfrmGuid }

class procedure TfrmGuid.Execute(const AStream: TJclCLRGuidStream);
begin
  with TfrmGuid.Create(nil) do
  try
    ShowGuids(AStream);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmGuid.ShowGuids(const AStream: TJclCLRGuidStream);
begin
  FStream := AStream;
  lstGuids.Items.Count := FStream.GuidCount;
end;

procedure TfrmGuid.lstGuidsData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := IntToStr(Item.Index);
  Item.SubItems.Add(GUIDToString(FStream.Guids[Item.Index]));
end;

end.
