unit EnvironmentExampleMain;

interface

uses
  SysUtils, Classes, Controls, Forms, ComCtrls, 
  JclSysInfo;

type
  TForm1 = class(TForm)
    EnvironmentView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure EnvironmentGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: WideString);
    procedure RefreshBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure GetEnvironment;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  GetEnvironment;
end;

procedure TForm1.EnvironmentGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: WideString);
var
  Key: string;
begin
  with EnvironmentView.Items[ARow] do
  begin
    Key := Caption;
    SetEnvironmentVar(Caption, SubItems[0]);
  end;
end;

procedure TForm1.RefreshBtnClick(Sender: TObject);
begin
  GetEnvironment;
end;

procedure TForm1.GetEnvironment;
var
  I: Integer;
  Key: string;
  S: TStringList;
begin
  S := TStringList.Create;
  try
    GetEnvironmentVars(S);
    for I := 0 to S.Count - 1 do
    begin
      Key := S.Names[I];
      with EnvironmentView.Items.Add do
      begin
        Caption := Key;
        SubItems.Add(S.Values[Key]);
      end;
    end;
  finally
    S.Free;
  end;
end;

end.

