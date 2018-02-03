unit ContainerPerformanceMain;

{$I jcl.inc}

interface

uses
  {$IFDEF LINUX}
  QForms, QStdCtrls, QControls,
  {$ENDIF LINUX}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  {$ENDIF MSWINDOWS}
  System.SysUtils, System.Classes, Vcl.Grids, Vcl.Menus;
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, Grids, Menus;
  {$ENDIF ~HAS_UNITSCOPE}

type
  TMainForm = class(TForm)
    ListPerformanceGrid: TStringGrid;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    Exit1: TMenuItem;
    TestMenu: TMenuItem;
    mnJclArrayList: TMenuItem;
    mnList: TMenuItem;
    mnJclLinkedList: TMenuItem;
    mnJclVector: TMenuItem;
    N1: TMenuItem;
    mnBucketList: TMenuItem;
    mnJclHashMap: TMenuItem;
    mnHashedStringList: TMenuItem;
    mnJclAnsiStrAnsiStrHashMap: TMenuItem;
    N2: TMenuItem;
    mnAllTest: TMenuItem;
    HashPerformanceGrid: TStringGrid;
    mnJclWideStrWideStrHashMap: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure mnAllTestClick(Sender: TObject);
    procedure mnListClick(Sender: TObject);
    procedure mnJclArrayListClick(Sender: TObject);
    procedure mnJclLinkedListClick(Sender: TObject);
    procedure mnJclVectorClick(Sender: TObject);
    procedure mnBucketListClick(Sender: TObject);
    procedure mnJclHashMapClick(Sender: TObject);
    procedure mnHashedStringListClick(Sender: TObject);
    procedure mnJclAnsiStrAnsiStrHashMapClick(Sender: TObject);
    procedure mnJclWideStrWideStrHashMapClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  public
  end;

  TMyObject = class(TInterfacedObject);

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  ContainerPerformanceTests;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ListPerformanceGrid.Cells[1, 0] := 'TList';
  ListPerformanceGrid.Cells[2, 0] := 'TJclArrayList';
  ListPerformanceGrid.Cells[3, 0] := 'TJclLinkedList';
  ListPerformanceGrid.Cells[4, 0] := 'TJclVector';
  ListPerformanceGrid.Cells[0, 1] := 'Add';
  ListPerformanceGrid.Cells[0, 2] := 'Next';
  ListPerformanceGrid.Cells[0, 3] := 'Random';
  ListPerformanceGrid.Cells[0, 4] := 'Insert at 10';
  ListPerformanceGrid.Cells[0, 5] := 'Clear';

  HashPerformanceGrid.Cells[1, 0] := 'TBucketList';
  HashPerformanceGrid.Cells[2, 0] := 'TJclHashMap';
  HashPerformanceGrid.Cells[3, 0] := 'THashedStringList';
  HashPerformanceGrid.Cells[4, 0] := 'TJclAnsiStrAnsiStrHashMap';
  HashPerformanceGrid.Cells[5, 0] := 'TJclWideStrWideStrHashMap';
  HashPerformanceGrid.Cells[0, 1] := 'Add';
  HashPerformanceGrid.Cells[0, 2] := 'Random';
  HashPerformanceGrid.Cells[0, 3] := 'Clear';
end;

procedure TMainForm.mnAllTestClick(Sender: TObject);
begin
  TestList(ListPerformanceGrid.Cols[1]);
  Application.ProcessMessages;
  TestJclArrayList(ListPerformanceGrid.Cols[2]);
  Application.ProcessMessages;
  TestJclLinkedList(ListPerformanceGrid.Cols[3]);
  Application.ProcessMessages;
  TestJclVector(ListPerformanceGrid.Cols[4]);
  Application.ProcessMessages;

  TestBucketList(HashPerformanceGrid.Cols[1]);
  Application.ProcessMessages;
  TestJclHashMap(HashPerformanceGrid.Cols[2]);
  Application.ProcessMessages;
  TestHashedStringList(HashPerformanceGrid.Cols[3]);
  Application.ProcessMessages;
  TestJclAnsiStrAnsiStrHashMap(HashPerformanceGrid.Cols[4]);
  Application.ProcessMessages;
  TestJclWideStrWideStrHashMap(HashPerformanceGrid.Cols[5]);
end;

procedure TMainForm.mnListClick(Sender: TObject);
begin
  TestList(ListPerformanceGrid.Cols[1]);
end;

procedure TMainForm.mnJclArrayListClick(Sender: TObject);
begin
  TestJclArrayList(ListPerformanceGrid.Cols[2]);
end;

procedure TMainForm.mnJclLinkedListClick(Sender: TObject);
begin
  TestJclLinkedList(ListPerformanceGrid.Cols[3]);
end;

procedure TMainForm.mnJclVectorClick(Sender: TObject);
begin
  TestJclVector(ListPerformanceGrid.Cols[4]);
end;

procedure TMainForm.mnBucketListClick(Sender: TObject);
begin
  TestBucketList(HashPerformanceGrid.Cols[1]);
end;

procedure TMainForm.mnJclHashMapClick(Sender: TObject);
begin
  TestJclHashMap(HashPerformanceGrid.Cols[2]);
end;

procedure TMainForm.mnHashedStringListClick(Sender: TObject);
begin
  TestHashedStringList(HashPerformanceGrid.Cols[3]);
end;

procedure TMainForm.mnJclAnsiStrAnsiStrHashMapClick(Sender: TObject);
begin
  TestJclAnsiStrAnsiStrHashMap(HashPerformanceGrid.Cols[4]);
end;

procedure TMainForm.mnJclWideStrWideStrHashMapClick(Sender: TObject);
begin
  TestJclWideStrWideStrHashMap(HashPerformanceGrid.Cols[5]);
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

end.

