unit ExprEvalExampleMain;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JclExprEval;

type
  TExprEvalForm = class(TForm)
    ExpressionInput: TEdit;
    Memo1: TMemo;
    Label1: TLabel;
    EnterButton: TButton;
    FuncList: TComboBox;
    Label2: TLabel;
    ValueEdit: TEdit;
    Label3: TLabel;
    VarComboBox: TComboBox;
    AssignButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EnterButtonClick(Sender: TObject);
    procedure FuncListClick(Sender: TObject);
    procedure AssignButtonClick(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure VarComboBoxChange(Sender: TObject);
  private
    { Private declarations }
    FEvaluator: TEasyEvaluator;
    FX: Extended;
    FY: Extended;
    FZ: Extended;
  public
    { Public declarations }
  end;

var
  ExprEvalForm: TExprEvalForm;

implementation

{$R *.DFM}

uses
  ExprEvalExampleLogic;

procedure TExprEvalForm.FormCreate(Sender: TObject);
begin
  FEvaluator := TEvaluator.Create;
  FEvaluator.AddVar('x', FX);
  FEvaluator.AddVar('y', FY);
  FEvaluator.AddVar('z', FZ);
  Init(FEvaluator, FuncList.Items);
end;

procedure TExprEvalForm.FormDestroy(Sender: TObject);
begin
  FEvaluator.Free;
end;

procedure TExprEvalForm.AssignButtonClick(Sender: TObject);
var
  Variable: PExtended;
begin
  if VarComboBox.Text = 'x' then
    Variable := @FX
  else
  if VarComboBox.Text = 'y' then
    Variable := @FY
  else
    Variable := @FZ;
  if not TryStrToFloat(ValueEdit.Text, Variable^) then
    ValueEdit.Font.Color := clRed;
end;

procedure TExprEvalForm.EnterButtonClick(Sender: TObject);
begin
  Memo1.Lines.Add(ResultAsText(FEvaluator as TEvaluator, ExpressionInput.Text));
end;

procedure TExprEvalForm.FuncListClick(Sender: TObject);
begin
  ExpressionInput.Text := ExpressionInput.Text + FuncList.Text;
  ActiveControl := ExpressionInput;
  ExpressionInput.SelStart := Length(ExpressionInput.Text);
end;

procedure TExprEvalForm.ValueEditChange(Sender: TObject);
const
  TextColor: array[Boolean] of TColor = (clRed, clWindowText);
var
  Dummy: Extended;
  Valid: Boolean;
begin
  Valid := TryStrToFloat(ValueEdit.Text, Dummy);
  ValueEdit.Font.Color := TextColor[Valid];
  AssignButton.Enabled := Valid;
end;

procedure TExprEvalForm.VarComboBoxChange(Sender: TObject);
begin
  ValueEdit.Text := FloatToStr((FEvaluator as TEvaluator).Evaluate(VarComboBox.Text));
end;

end.
