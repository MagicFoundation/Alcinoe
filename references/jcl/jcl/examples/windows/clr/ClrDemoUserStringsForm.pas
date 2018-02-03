unit ClrDemoUserStringsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ClrDemoStringsForm, StdCtrls, Buttons, ComCtrls, JclCLR;

type
  TfrmUserStrings = class(TForm)
    lstStrings: TListView;
    btnOK: TBitBtn;
    procedure lstStringsData(Sender: TObject; Item: TListItem);
  private
    FStream: TJclCLRUserStringStream;
    procedure ShowStrings(const AStream: TJclCLRUserStringStream);
  public
    class procedure Execute(const AStream: TJclCLRUserStringStream); 
  end;

var
  frmUserStrings: TfrmUserStrings;

implementation

{$R *.DFM}

{ TfrmUserStrings }

class procedure TfrmUserStrings.Execute(const AStream: TJclCLRUserStringStream);
begin
  with TfrmUserStrings.Create(nil) do
  try
    ShowStrings(AStream);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmUserStrings.ShowStrings(const AStream: TJclCLRUserStringStream);
begin
  FStream := AStream;
  lstStrings.Items.Count := FStream.StringCount;
end;

procedure TfrmUserStrings.lstStringsData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := IntToStr(Item.Index);
  Item.SubItems.Add(IntToHex(FStream.Offsets[Item.Index], 8));
  Item.SubItems.Add(FStream.Strings[Item.Index]);
end;

end.
