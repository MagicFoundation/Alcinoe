unit AlgorithmsExampleMain;

{$I jcl.inc}

interface

uses
  {$IFDEF LINUX}
  QForms, QStdCtrls, QControls, QComCtrls,
  {$ENDIF}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages, Vcl.Forms, Vcl.ComCtrls, Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls,
  {$ENDIF}
  System.SysUtils, System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows, Messages, Forms, ComCtrls, Graphics, Controls, Dialogs, StdCtrls,
  {$ENDIF}
  SysUtils, Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  JclContainerIntf, JclArrayLists, JclLinkedLists;

type
  TMainForm = class(TForm)
    PageControl1: TPageControl;
    tbsApply: TTabSheet;
    tbsFind: TTabSheet;
    tbsCountObject: TTabSheet;
    tbsCopy: TTabSheet;
    tbsReverse: TTabSheet;
    tbsSort: TTabSheet;
    btnApplyGenerate: TButton;
    btnApply: TButton;
    lbxApply: TListBox;
    btnFindGenerate: TButton;
    btnFind: TButton;
    lbxFind: TListBox;
    edtFind: TEdit;
    btnCountGenerate: TButton;
    btnCount: TButton;
    lbxCount: TListBox;
    edtCount: TEdit;
    btnCopyGenerate: TButton;
    btnCopy: TButton;
    lbxCopySrc: TListBox;
    lbxCopyDes: TListBox;
    btnReverseGenerate: TButton;
    btnReverse: TButton;
    lbxReverse: TListBox;
    btnSortGenerate: TButton;
    btnSort: TButton;
    lbxSort: TListBox;
    lblFound: TLabel;
    lblCount: TLabel;
    edtApply: TEdit;
    procedure btnApplyGenerateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnFindGenerateClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnCountGenerateClick(Sender: TObject);
    procedure btnCountClick(Sender: TObject);
    procedure btnCopyGenerateClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnReverseGenerateClick(Sender: TObject);
    procedure btnReverseClick(Sender: TObject);
    procedure btnSortGenerateClick(Sender: TObject);
    procedure btnSortClick(Sender: TObject);
  private
  public
    List: IJclList;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses JclAlgorithms;

procedure TMainForm.btnApplyGenerateClick(Sender: TObject);
var
  I: Integer;
  It: IJclIterator;
begin
  List.Clear;
  for I := 1 to 10 do
    List.Add(TObject(I));
  lbxApply.Items.Clear;
  It := List.First;
  while It.HasNext do
    lbxApply.Items.Add(IntToStr(Integer(It.Next)));
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  List := TJclArrayList.Create(16, False);
end;

function Add2(AObject: TObject): TObject;
begin
  Result := TObject(Integer(AObject) + 2);
end;

procedure TMainForm.btnApplyClick(Sender: TObject);
var
  It: IJclIterator;
  Value: Integer;
begin
  Value := StrToIntDef(edtApply.Text, 0);
  JclAlgorithms.Apply(List.First, Value, Add2);
  lbxApply.Items.Clear;
  It := List.First;
  while It.HasNext do
    lbxApply.Items.Add(IntToStr(Integer(It.Next)));
end;

procedure TMainForm.btnFindGenerateClick(Sender: TObject);
var
  I: Integer;
  It: IJclIterator;
begin
  List.Clear;
  for I := 1 to 10 do
    List.Add(TObject(I));
  lbxFind.Items.Clear;
  It := List.First;
  while It.HasNext do
    lbxFind.Items.Add(IntToStr(Integer(It.Next)));
end;

procedure TMainForm.btnFindClick(Sender: TObject);
var
  It: IJclIterator;
  Value: Integer;
begin
  Value := StrToIntDef(edtFind.Text, 0);
  It := JclAlgorithms.Find(List.First, List.Size, TObject(Value), SimpleCompare);
  if It = nil then
    lblFound.Caption := 'Not found'
  else
    lblFound.Caption := 'Found';
end;

procedure TMainForm.btnCountGenerateClick(Sender: TObject);
var
  I: Integer;
  It: IJclIterator;
begin
  Randomize;
  List.Clear;
  for I := 1 to 10 do
    List.Add(TObject(Random(10) + 1));
  lbxCount.Items.Clear;
  It := List.First;
  while It.HasNext do
    lbxCount.Items.Add(IntToStr(Integer(It.Next)));
end;

procedure TMainForm.btnCountClick(Sender: TObject);
var
  Count: Integer;
  Value: Integer;
begin
  Value := StrToIntDef(edtCount.Text, 0);
  Count := JclAlgorithms.CountObject(List.First, List.Size, TObject(Value), SimpleCompare);
  lblCount.Caption := IntToStr(Count);
end;

procedure TMainForm.btnCopyGenerateClick(Sender: TObject);
var
  I: Integer;
  It: IJclIterator;
begin
  Randomize;
  List.Clear;
  for I := 1 to 10 do
    List.Add(TObject(Random(10) + 1));
  lbxCopySrc.Items.Clear;
  It := List.First;
  while It.HasNext do
    lbxCopySrc.Items.Add(IntToStr(Integer(It.Next)));
end;

procedure TMainForm.btnCopyClick(Sender: TObject);
var
  AnotherList: IJclList;
  It: IJclIterator;
begin
  AnotherList := TJclArrayList.Create(16, False);
  JclAlgorithms.Generate(AnotherList, 10, TObject(0));
  JclAlgorithms.Copy(List.First, List.Size, AnotherList.First);
  lbxCopyDes.Items.Clear;
  It := AnotherList.First;
  while It.HasNext do
    lbxCopyDes.Items.Add(IntToStr(Integer(It.Next)));
end;

procedure TMainForm.btnReverseGenerateClick(Sender: TObject);
var
  I: Integer;
  It: IJclIterator;
begin
  List.Clear;
  for I := 1 to 10 do
    List.Add(TObject(I));
  lbxReverse.Items.Clear;
  It := List.First;
  while It.HasNext do
    lbxReverse.Items.Add(IntToStr(Integer(It.Next)));
end;

procedure TMainForm.btnReverseClick(Sender: TObject);
var
  It: IJclIterator;
begin
  JclAlgorithms.Reverse(List.First, List.Last);
  lbxReverse.Items.Clear;
  It := List.First;
  while It.HasNext do
    lbxReverse.Items.Add(IntToStr(Integer(It.Next)));
end;

procedure TMainForm.btnSortGenerateClick(Sender: TObject);
var
  I: Integer;
  It: IJclIterator;
begin
  Randomize;
  List.Clear;
  for I := 1 to 10 do
    List.Add(TObject(Random(10) - 5));
  lbxSort.Items.Clear;
  It := List.First;
  while It.HasNext do
    lbxSort.Items.Add(IntToStr(Integer(It.Next)));
end;

procedure TMainForm.btnSortClick(Sender: TObject);
var
  It: IJclIterator;
begin
  JclAlgorithms.Sort(List, 0, 9, IntegerCompare);
  lbxSort.Items.Clear;
  It := List.First;
  while It.HasNext do
    lbxSort.Items.Add(IntToStr(Integer(It.Next)));
end;

end.

