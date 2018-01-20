unit Unit1;

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  Messages,
  Graphics,
  {$endif}
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  SynCommons,
  mORMot,
  SampleData;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    QuitButton: TButton;
    FindButton: TButton;
    QuestionMemo: TMemo;
    NameEdit: TEdit;
    procedure AddButtonClick(Sender: TObject);
    procedure FindButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Database: TSQLRest;
    Model: TSQLModel;
  end;

var
  Form1: TForm1;

implementation

{$ifdef FPC}
{$R *.lfm}
{$else}
{$R *.dfm}
{$endif}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Model := CreateSampleModel; // from SampleData unit
end;

procedure TForm1.AddButtonClick(Sender: TObject);
var Rec: TSQLSampleRecord;
begin
  Rec := TSQLSampleRecord.Create;
  try
    // we use explicit StringToUTF8() for conversion below
    // a real application should use TLanguageFile.StringToUTF8() in mORMoti18n
    Rec.Name := StringToUTF8(NameEdit.Text);
    Rec.Question := StringToUTF8(QuestionMemo.Text);
    if Database.Add(Rec,true)=0 then
      ShowMessage('Error adding the data') else begin
      NameEdit.Text := '';
      QuestionMemo.Text := '';
      NameEdit.SetFocus;
    end;
  finally
    Rec.Free;
  end;
end;

procedure TForm1.FindButtonClick(Sender: TObject);
var Rec: TSQLSampleRecord;
begin
  Rec := TSQLSampleRecord.Create(Database,'Name=?',[StringToUTF8(NameEdit.Text)]);
  try
    if Rec.ID=0 then
      QuestionMemo.Text := 'Not found' else
      QuestionMemo.Text := UTF8ToString(Rec.Question);
  finally
    Rec.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Database.Free;
  Model.Free;
end;

procedure TForm1.QuitButtonClick(Sender: TObject);
begin
  Close;
end;

end.

