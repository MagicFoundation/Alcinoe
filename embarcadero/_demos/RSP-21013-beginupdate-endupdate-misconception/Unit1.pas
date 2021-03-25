unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Objects, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type

  TmyRectangle = class(Trectangle)
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
  end;

  TForm1 = class(TForm)
    Start: TButton;
    MasterControl: TRectangle;
    Text1: TText;
    Text2: TText;
    Text3: TText;
    procedure StartClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

var LBeginUpdateCount: integer;
    LEndUpdateCount: integer;


procedure TForm1.StartClick(Sender: TObject);
begin

  //create a LChildControl
  var LChildControl := TMyRectangle.create(nil);
  LChildControl.Align := TAlignLayout.Client;
  LChildControl.Fill.Color := TalphaColorRec.Yellow;

  //create a LGrandChildControl as child of LChildControl
  Var LGrandChildControl := TRectangle.create(LChildControl);
  LGrandChildControl.Align := TAlignLayout.Client;
  LGrandChildControl.Fill.Color := TalphaColorRec.red;
  LGrandChildControl.Parent := LChildControl;

  //create a LGrandGrandChildControl as child of LGrandChildControl
  Var LGrandGrandChildControl := TRectangle.create(LGrandChildControl);
  LGrandGrandChildControl.Align := TAlignLayout.Client;
  LGrandGrandChildControl.Fill.Color := TalphaColorRec.blue;
  LGrandGrandChildControl.Parent := LGrandChildControl;

  MasterControl.BeginUpdate;
  try

    LChildControl.Parent :=  MasterControl;

    Showmessage(
      'If everything is ok, all IsUpdating must be equal to true: ' + #13#10 +
      #13#10 +
      'MasterControl.IsUpdating: ' + boolToStr(MasterControl.IsUpdating, true) + #13#10 +
      'ChildControl.IsUpdating: ' + boolToStr(LChildControl.IsUpdating, true) + #13#10 +
      'GrandChildControl.IsUpdating: ' + boolToStr(LGrandChildControl.IsUpdating, true) + #13#10 +
      'GrandGrandChildControl.IsUpdating: ' + boolToStr(LGrandGrandChildControl.IsUpdating, true) + #13#10);

  finally
    MasterControl.EndUpdate;
  end;

  Showmessage('If everything is ok, DoBeginUpdate = DoEndUpdate = 1' + #13#10 +
              #13#10 +
              'Control.DoBeginUpdate was called: '+inttostr(LBeginUpdateCount)+' times' + #13#10 +
              'GrandChildControl.DoEndUpdate was called: '+inttostr(LEndUpdateCount)+' times');

  Showmessage('Try now to rename _FMX.Controls.pas to FMX.Controls.pas and run again');


end;

{ TmyControl }


procedure TmyRectangle.DoBeginUpdate;
begin
  inherited;
  inc(LBeginUpdateCount);
end;

procedure TmyRectangle.DoEndUpdate;
begin
  inherited;
  inc(LEndUpdateCount);
end;

end.
