program ProjectIndexerResearch;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  FastMM4,
  System.SysUtils,
  System.Generics.Collections,
  DelphiAST.ProjectIndexer,
  TestUnit in 'TestUnit.pas';

var
  i      : integer;
  indexer: TProjectIndexer;

begin
  try
    if ParamCount <> 1 then
      Writeln(ParamStr(0) + ' <project.dpr>')
    else begin
      indexer := TProjectIndexer.Create;
      try
//        indexer.SearchPath :=
//          'x:\common\pkg\dspack\src\DirectX9;x:\common\pkg\dspack\src\DSPack;x:\common\DCPCrypt2;x:\common\DCPCrypt2\Ciphers;x:\common\DCPCrypt2\Hashes;x:\common\EZDSL;x:\common\g32;x:\gp\common;' +
//          'x:\gp\common\except;x:\common\src;x:\common\iphlpapi;x:\common\jwa;x:\common\OmniXML;x:\common\OmniXML\extras;x:\ms\common;x:\common\MSSpell;x:\common\pkg\devexpress5\sources;' +
//          'x:\common\pkg\jcl\source\include;x:\common\pkg\jcl\source;x:\common\pkg\jcl\source\common;x:\common\pkg\jcl\source\windows;x:\common\pkg\jcl\source\vcl;x:\common\pkg\jcl\source\prototypes;' +
//          'x:\common\pkg\Abbrevia\source;x:\common\pkg\APRO\run;x:\common\pkg\ics\source;x:\common\pkg\ics\source\include;x:\common\pkg\ics\source\extras;x:\common\pkg\jvcl\archive;' +
//          'x:\common\pkg\svcom\AllVersions\DesignTime;x:\common\pkg\svcom\AllVersions\Runtime;x:\common\pkg\tsilang\units;x:\common\pkg\tsilang\units\Auxilary;x:\common\pkg\vt;x:\common\pkg\vt\common;' +
//          'x:\ms\hl\Delphi;x:\ms\hl\HASP;x:\common\pkg\btree;x:\ms\ettwin;x:\ms\hl\cdg;x:\ms\htdrv;x:\gp\dvb;x:\gp\sttdb3;x:\ms\termcom;x:\ln\Common;x:\ln\Decklink;x:\ln\SubtitleEmbedder;x:\ln\mxf;' +
//          'x:\ln\gxf;x:\ln\FABAudio;x:\gp\arcman;x:\ms\htdrv10;x:\common\fastmm;x:\gp\edl;x:\gp\edl\compile;x:\ln\TS;x:\ln\Renderer;x:\ln\DebugFilter;x:\common\pkg\ChantSpeechKit;x:\common\elevation;' +
//          'x:\ms\install;x:\common\omnithreadlibrary;x:\ln\VirtualStringTree;x:\ln\mpeg;x:\common\pkg\_FAB\Rtf98;x:\common\pkg\kbmMemTable;x:\common\pkg\zip;x:\ln\mp4;x:\common\pkg\sapi\demos\USBView;' +
//          'x:\common\pkg\AAF;x:\common\pkg\taskbarlist;x:\ms\hl\hasp;x:\common\pkg\TsiLang\Units;x:\common\pkg\TsiLang\Units\Auxilary;x:\common\pkg\htmlviewer\source;x:\common\pkg\ppdf;' +
//          'x:\common\pkg\DragDrop\Source;x:\common\pkg\DM\Source;x:\gp\utils;x:\common\pkg\TsiLang\Units;X:\common;x:\common\pkg\jvcl\run;x:\common\pkg\jvcl\common;x:\common\pkg\jvcl\resources;' +
//          'x:\ms\netapi;x:\ln\wm\source;x:\common\ribbon\lib;x:\common\ffmpeg;C:\Program Files (x86)\TestInsight\Source;x:\common\detours\src;x:\common\Spring4D\Source\Base;x:\common\Spring4D\Source\Base\Collections;' +
//          'x:\common\Spring4D\Source\Core\Interception';
//        indexer.Defines := 'DEBUG';
        indexer.SearchPath := 'sub2';
        indexer.Index(ParamStr(1));
        Writeln(indexer.ParsedUnits.Count, ' units');
        for i := 0 to indexer.ParsedUnits.Count - 1 do
          Writeln(indexer.ParsedUnits[i].Name, ' in ', indexer.ParsedUnits[i].Path);
        Writeln;
        Writeln(indexer.IncludeFiles.Count, ' includes');
        for i := 0 to indexer.IncludeFiles.Count - 1 do
          Writeln(indexer.IncludeFiles[i].Name, ' @ ', indexer.IncludeFiles[i].Path);
        Writeln;
        Writeln(indexer.NotFoundUnits.Count, ' not found');
        for i := 0 to indexer.NotFoundUnits.Count - 1 do
          Writeln(indexer.NotFoundUnits[i]);
        Writeln;
        Writeln(indexer.Problems.Count, ' problems');
        for i := 0 to indexer.Problems.Count - 1 do
          Writeln(Ord(indexer.Problems[i].ProblemType), ' ', indexer.Problems[i].FileName, ': ',
            indexer.Problems[i].Description);
        Write('>');
        Readln;
      finally FreeAndNil(indexer); end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
