/// answer to http://stackoverflow.com/q/39685770/458259
program ORMFastMultiInsertWithIndex;

{
   insert 1,000,000 rows via the ORM in a SQlite3 in-memory database,
   and search by Key using an index (only 1024 diverse values of Key)
   - note that two similar SELECT are executed, to show two ORM methods

  with index defined before insertion:
    INSERT 1000000 rows in 6.71s
    SELECT 1000000 rows per Key index in 1.15s

  with index created after insertion:
    INSERT 1000000 rows in 2.91s
    CREATE INDEX 1000000 in 1.28s
    SELECT 1000000 rows per Key index in 1.15s

  without the index:
    INSERT 1000000 rows in 2.94s
    SELECT 1000000 rows per Key index in 129.27s

  So for huge data set, an index is worth it, and better done AFTER insertion!

}

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  SynCommons,
  SynSQLite3,
  SynSQLite3Static,
  mORMot,
  mORMotSQLite3;

type
  TSQLEntry = class(TSQLRecord)
  private
    fKey: RawUTF8;
    fValue: RawUTF8;
  published
    property Key: RawUTF8 read fKey write fKey;
    property Value: RawUTF8 read fValue write fValue;
  end;

const
  // used to create some fake data, with some multiple occurences of Key
  COUNT = 1000000; // one million rows insertion !
  UNIQUE_KEY = 1024; // should be a power of two

procedure Process;

var
  db: TSQLRestServerDB;

  procedure DoInsert;
  var i: integer;
      rec: TSQLEntry;
      batch: TSQLRestBatch;
  begin
    rec := TSQLEntry.Create;
    batch := TSQLRestBatch.Create(db,TSQLEntry,10000,[boExtendedJSON]);
    try
      write('batch:');
      for i := 0 to COUNT-1 do begin
        // here we fill with some data
        rec.Key := FormatUTF8('KEY%',[i and pred(UNIQUE_KEY)]);
        rec.Value := FormatUTF8('VALUE%',[i]);
        batch.Add(rec,true);
        if i and $2ffff = $2ffff then begin
          write(' send');
          db.batchSend(batch);
          batch.Reset;
        end;
      end;
      db.BatchSend(batch);
    finally
      batch.Free;
      rec.Free;
    end;
  end;

  procedure DoSelect;
  var i, total: integer;
      rec: TSQLEntry;
      key: RawUTF8;
      values: TRawUTF8DynArray;
  begin
    total := 0;
    rec := TSQLEntry.Create;
    try
      for i := 0 to pred(UNIQUE_KEY) do begin
        key := FormatUTF8('KEY%',[i]);
        // 1. first way: get all values at once
        db.OneFieldValues(TSQLEntry,'Value',FormatUTF8('Key=?',[],[key]),values);
        // here values[] contains all "Value" column associated with this key
        // 2. second way: use ORM
        rec.FillPrepare(db,'Key=?',[],[key],'Value');
        while rec.FillOne do begin
          // here you have rec.Value filled
          assert(rec.Value=values[rec.FillCurrentRow-2]);
          inc(total);
        end;
      end;
      assert(total=COUNT);
    finally
      rec.Free;
    end;
  end;

var timer: TPrecisionTimer;
begin
  if true then
    db := TSQLRestServerDB.CreateWithOwnModel([TSQLEntry],SQLITE_MEMORY_DATABASE_NAME)
  else begin
    deletefile('test.db');
    db := TSQLRestServerDB.CreateWithOwnModel([TSQLEntry],'test.db');
  end;
  try
    db.DB.LockingMode := lmExclusive;
    db.DB.Synchronous := smOff;
    db.CreateMissingTables;
    write('INSERT ', COUNT, ' rows');
    timer.Start;
    DoInsert;
    writeln(' in ',timer.Stop);
    write('CREATE INDEX ', COUNT);
    timer.Start;
    db.CreateSQLIndex(TSQLEntry,'Key',false); // faster done after insertion
    writeln(' in ',timer.Stop);
    write('SELECT ', COUNT, ' rows per Key index');
    timer.Start;
    DoSelect;
    writeln(' in ',timer.Stop);
  finally
    db.Free;
  end;
end;

begin
  Process;
  writeln('Press any key');
  readln;
end.
