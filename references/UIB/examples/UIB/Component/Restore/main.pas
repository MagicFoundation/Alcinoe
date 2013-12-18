unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
  SysUtils, Classes, uib;

type
  TMainForm = class(TForm)
    Restore: TUIBRestore;
    Log: TMemo;
    Go: TButton;
    procedure RestoreVerbose(Sender: TObject; Message: String);
    procedure GoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.RestoreVerbose(Sender: TObject; Message: String);
begin
  Log.Lines.Add(Message);
end;

procedure TMainForm.GoClick(Sender: TObject);
begin
  Log.Clear;
  Restore.Run;
end;

end.
