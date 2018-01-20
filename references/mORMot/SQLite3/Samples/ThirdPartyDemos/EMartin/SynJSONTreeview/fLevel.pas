unit fLevel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, CheckLst,
  SynCommons;

type
  TfrmLevel = class(TForm)
    chklstLevel: TCheckListBox;
    pnlBottom: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure FillListBox(const aJSON: Variant);
    function ToJSON: RawUTF8;
  end;

var
  frmLevel: TfrmLevel;

implementation

{$R *.dfm}

{ TfrmLevel }

procedure TfrmLevel.FillListBox(const aJSON: Variant);
var
  I, J, lCount: Integer;
begin
  lCount := TDocVariantData(aJSON).Count;
  chklstLevel.Items.BeginUpdate;
  try
    for I := 0 to lCount-1 do
    begin
      J := chklstLevel.Items.Add(TDocVariantData(aJSON).Values[I]);
      chklstLevel.Checked[J] := True;
    end;
  finally
    chklstLevel.Items.EndUpdate;
  end;
end;

function TfrmLevel.ToJSON: RawUTF8;
var
  lW: TTextWriter;
  I: Integer;
begin
  lW := TTextWriter.CreateOwnedStream;
  try
    lW.Add('[');
    for I := 0 to chklstLevel.Items.Count-1 do
      if chklstLevel.Checked[I] then
      begin
        lW.Add('"');
        lW.AddString(chklstLevel.Items[I]);
        lW.Add('"', ',');
      end;
    lW.CancelLastComma;
    lW.Add(']');
    lW.SetText(Result);
  finally
    lW.Free;
  end;
end;

end.
