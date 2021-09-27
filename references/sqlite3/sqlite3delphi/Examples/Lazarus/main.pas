unit Main;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm_Main }

  TForm_Main = class(TForm)
    Panel_Actions: TPanel;
    Button_DB_Read: TButton;
    Button_DB_Create: TButton;
    Memo_Result: TMemo;
    procedure Button_DB_CreateClick(Sender: TObject);
    procedure Button_DB_ReadClick(Sender: TObject);
  end;

var
  Form_Main: TForm_Main;

implementation

uses
  SQLite3, SQLite3Wrap;

{$R *.lfm}

{ TForm_Main }

procedure TForm_Main.Button_DB_CreateClick(Sender: TObject);
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
  IDs: array[1..6] of Integer;
begin
  // Delete database if it already exists
  DeleteFile('artists.db');

  // Create database and fill it with example data
  Screen.Cursor := crHourGlass;
  DB := TSQLite3Database.Create;
  try
    DB.Open('artists.db');

    // Create table "artists"
    DB.Execute('CREATE TABLE artists (name TEXT, born REAL, died REAL)');

    // Fill the table with artists
    Stmt := DB.Prepare('INSERT INTO artists (name, born, died) VALUES (?, ?, ?)');
    try
      Stmt.BindText  (1, 'Leonardo da Vinci');
      Stmt.BindDouble(2, EncodeDate(1452, 4, 15));
      Stmt.BindDouble(3, EncodeDate(1519, 5, 2));

      Stmt.StepAndReset; // StepAndReset executes a prepared statement
                         // and resets it so we can reuse it again

      IDs[1] := DB.LastInsertRowID; // Save newly added artist's ID to use it
                                    // when filling "paintings" table below

      Stmt.BindText  (1, 'Raphael');
      Stmt.BindDouble(2, EncodeDate(1483, 3, 28));
      Stmt.BindDouble(3, EncodeDate(1520, 4, 6));
      Stmt.StepAndReset;
      IDs[2] := DB.LastInsertRowID;

      Stmt.BindText  (1, 'Arkhip Kuindzhi');
      Stmt.BindDouble(2, EncodeDate(1842, 1, 27));
      Stmt.BindDouble(3, EncodeDate(1898, 7, 24));
      Stmt.StepAndReset;
      IDs[3] := DB.LastInsertRowID;

      Stmt.BindText  (1, 'Nicholas Roerich');
      Stmt.BindDouble(2, EncodeDate(1874, 10, 9));
      Stmt.BindDouble(3, EncodeDate(1947, 12, 13));
      Stmt.StepAndReset;
      IDs[4] := DB.LastInsertRowID;

      Stmt.BindText  (1, 'Ivan Aivazovsky');
      Stmt.BindDouble(2, EncodeDate(1817, 7, 29));
      Stmt.BindDouble(3, EncodeDate(1900, 5, 5));
      Stmt.StepAndReset;
      IDs[5] := DB.LastInsertRowID;

      Stmt.BindText  (1, 'Ivan Shishkin');
      Stmt.BindDouble(2, EncodeDate(1832, 1, 25));
      Stmt.BindDouble(3, EncodeDate(1898, 3, 20));
      Stmt.StepAndReset;
      IDs[6] := DB.LastInsertRowID;
    finally
      Stmt.Free;
    end;

    // Create table "paintings"
    DB.Execute('CREATE TABLE paintings (title TEXT, year INTEGER, artist INTEGER)');

    // Fill the table with paintings info
    Stmt := DB.Prepare('INSERT INTO paintings (title, year, artist) VALUES (?, ?, ?)');
    try
      // Leonardo da Vinci
      Stmt.BindText(1, 'The Virgin and Child with St. Anne');
      Stmt.BindInt (2, 1508);
      Stmt.BindInt (3, IDs[1]);
      Stmt.StepAndReset;

      Stmt.BindText(1, 'Mona Lisa');
      Stmt.BindInt (2, 1519);
      Stmt.BindInt (3, IDs[1]);
      Stmt.StepAndReset;

      // Raphael
      Stmt.BindText(1, 'Sistine Madonna');
      Stmt.BindInt (2, 1514);
      Stmt.BindInt (3, IDs[2]);
      Stmt.StepAndReset;

      Stmt.BindText(1, 'Transfiguration');
      Stmt.BindInt (2, 1520);
      Stmt.BindInt (3, IDs[2]);
      Stmt.StepAndReset;

      // Arkhip Kuindzhi
      Stmt.BindText(1, 'After a rain');
      Stmt.BindInt (2, 1879);
      Stmt.BindInt (3, IDs[3]);
      Stmt.StepAndReset;

      Stmt.BindText(1, 'Elbrus');
      Stmt.BindInt (2, 1895);
      Stmt.BindInt (3, IDs[3]);
      Stmt.StepAndReset;

      // Nicholas Roerich
      Stmt.BindText(1, 'To Kailas. Lahul');
      Stmt.BindInt (2, 1932);
      Stmt.BindInt (3, IDs[4]);
      Stmt.StepAndReset;

      Stmt.BindText(1, 'Krishna');
      Stmt.BindInt (2, 1929);
      Stmt.BindInt (3, IDs[4]);
      Stmt.StepAndReset;

      // Ivan Aivazovsky
      Stmt.BindText(1, 'The Mary Caught in a Storm');
      Stmt.BindInt (2, 1892);
      Stmt.BindInt (3, IDs[5]);
      Stmt.StepAndReset;

      Stmt.BindText(1, 'Brig "Mercury" Attacked by Two Turkish Ships');
      Stmt.BindInt (2, 1892);
      Stmt.BindInt (3, IDs[5]);
      Stmt.StepAndReset;

      // Ivan Shishkin
      Stmt.BindText(1, 'Morning in a Pine Forest');
      Stmt.BindInt (2, 1889);
      Stmt.BindInt (3, IDs[6]);
      Stmt.StepAndReset;

      Stmt.BindText(1, 'Wood Distances');
      Stmt.BindInt (2, 1884);
      Stmt.BindInt (3, IDs[6]);
      Stmt.StepAndReset;
    finally
      Stmt.Free;
    end;

    ShowMessage('Database created.');
  finally
    DB.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm_Main.Button_DB_ReadClick(Sender: TObject);
var
  DB: TSQLite3Database;
  Stmt_Artists,
  Stmt_Paintings: TSQLite3Statement;
begin
  if not FileExists('artists.db') then
  begin
    ShowMessage('The database does not exist. Please create one.');
    Exit;
  end;

  DB := TSQLite3Database.Create;
  try
    DB.Open('artists.db');

    // Show all artists and their paintings
    Stmt_Artists := DB.Prepare('SELECT rowid, name, born, died FROM artists ORDER BY born');
    Stmt_Paintings := DB.Prepare('SELECT title, year FROM paintings WHERE artist = ? ORDER BY year');
    try
      while Stmt_Artists.Step = SQLITE_ROW do
      begin
        Memo_Result.Lines.Add(Stmt_Artists.ColumnText(1));
        Memo_Result.Lines.Add(DateToStr(Stmt_Artists.ColumnDouble(2)) + ' - ' + DateToStr(Stmt_Artists.ColumnDouble(3)));

        Memo_Result.Lines.Add('paintings:');
        Stmt_Paintings.BindInt(1, Stmt_Artists.ColumnInt(0));
        while Stmt_Paintings.Step = SQLITE_ROW do
          Memo_Result.Lines.Add('    ' + Stmt_Paintings.ColumnText(0) + ' (' + Stmt_Paintings.ColumnText(1) + ')');
        Stmt_Paintings.Reset;

        Memo_Result.Lines.Add('');
      end;
    finally
      Stmt_Paintings.Free;
      Stmt_Artists.Free;
    end;
  finally
    DB.Free;
  end;

  // Add separator
  Memo_Result.Lines.Add('------------------------------------------------');
  Memo_Result.Lines.Add('');
end;

end.

