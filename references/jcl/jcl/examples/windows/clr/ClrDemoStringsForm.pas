unit ClrDemoStringsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, JclCLR;

type
  TfrmStrings = class(TForm)
    lstStrings: TListView;
    btnOK: TBitBtn;
    procedure lstStringsData(Sender: TObject; Item: TListItem);
  private
    FStream: TJclCLRStringsStream;
    procedure ShowStrings(const AStream: TJclCLRStringsStream);
  public
    class procedure Execute(const AStream: TJclCLRStringsStream);
  end;

implementation

{$R *.DFM}

uses
  JclUnicode;

{ TfrmStrings }

class procedure TfrmStrings.Execute(const AStream: TJclCLRStringsStream);
begin
  with TfrmStrings.Create(nil) do
  try
    ShowStrings(AStream);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmStrings.ShowStrings(const AStream: TJclCLRStringsStream);
begin
  FStream := AStream;
  lstStrings.Items.Count := FStream.StringCount;
end;

procedure TfrmStrings.lstStringsData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := IntToStr(Item.Index);
  Item.SubItems.Add(IntToHex(FStream.Offsets[Item.Index], 8));
  Item.SubItems.Add(FStream.Strings[Item.Index]);
end;

end.
