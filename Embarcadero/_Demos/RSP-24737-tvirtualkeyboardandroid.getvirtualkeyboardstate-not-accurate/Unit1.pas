unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, fmx.Platform, FMX.Memo.Types,
  FMX.Objects, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Text1: TText;
    Text2: TText;
    procedure FormVirtualKeyboardHidden(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardShown(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
  protected
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormVirtualKeyboardHidden(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  Text2.text := Text2.text + 'Hidden - KeyboardVisible: ' + BoolToStr(KeyboardVisible, true) + ' - Bounds:'+floattoStr(Bounds.height) + #10;
end;

procedure TForm1.FormVirtualKeyboardShown(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  Text2.text := Text2.text + 'Shown - KeyboardVisible: ' + BoolToStr(KeyboardVisible, true) + ' - Bounds:'+floattoStr(Bounds.height) + #10;
end;

end.
