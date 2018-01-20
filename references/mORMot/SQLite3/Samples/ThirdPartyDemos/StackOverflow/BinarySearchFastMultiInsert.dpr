/// answer to http://stackoverflow.com/q/39685770/458259
program BinarySearchFastMultiInsert;

{
   one million rows benchmark:
    INSERT 1000000 rows in 215.49ms
    SORT ARRAY 1000000 in 192.64ms
    SELECT 1000000 rows per Key index in 26.15ms

   ten million rows benchmark:
    INSERT 10000000 rows in 2.10s
    SORT ARRAY 10000000 in 3.06s
    SELECT 10000000 rows per Key index in 357.72ms

  compare to SQLite3 in-memory, with index created after insertion:
    INSERT 1000000 rows in 2.91s
    CREATE INDEX 1000000 in 1.28s
    SELECT 1000000 rows per Key index in 1.15s
}

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  SynCommons;

type
  TEntry = record
    Key: RawUTF8;
    Value: RawUTF8;
  end;
  TEntryDynArray = array of TEntry;

const
  // used to create some fake data, with some multiple occurences of Key
  COUNT = 1000000; // million rows insertion !
  UNIQUE_KEY = 1024; // should be a power of two

procedure Process;

var
  entry: TEntryDynArray;
  entrycount: integer;
  entries: TDynArray;

  procedure DoInsert;
  var i: integer;
      rec: TEntry;
  begin
    for i := 0 to COUNT-1 do begin
      // here we fill with some data
      FormatUTF8('KEY%',[i and pred(UNIQUE_KEY)],rec.Key);
      FormatUTF8('VALUE%',[i],rec.Value);
      entries.Add(rec);
    end;
  end;

  procedure DoSelect;
  var i,j, first,last, total: integer;
      key: RawUTF8;
  begin
    total := 0;
    for i := 0 to pred(UNIQUE_KEY) do begin
      FormatUTF8('KEY%',[i],key);
      assert(entries.FindAllSorted(key,first,last));
      for j := first to last do
        assert(entry[j].Key=key);
      inc(total,last-first+1);
    end;
    assert(total=COUNT);
  end;

var timer: TPrecisionTimer;
begin
  entries.InitSpecific(TypeInfo(TEntryDynArray),entry,djRawUTF8,@entrycount);
  write('INSERT ', COUNT, ' rows');
  timer.Start;
  DoInsert;
  writeln(' in ',timer.Stop);
  write('SORT ARRAY ', COUNT);
  timer.Start;
  entries.Sort; // faster done after insertion
  writeln(' in ',timer.Stop);
  write('SELECT ', COUNT, ' rows per Key index');
  timer.Start;
  DoSelect;
  writeln(' in ',timer.Stop);
//  exit; // comment this line to avoid binary and json serialization
  timer.Start;
  FileFromString(entries.SaveTo,'test.db');
  FileFromString(entries.SaveToJson,'test.json');
  writeln('SAVED in ',timer.Stop);
end;

begin
  TTextwriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(TEntryDynArray),'key,value:RawUTF8');
  Process;
  writeln('Press [Enter]');
  readln;
end.
