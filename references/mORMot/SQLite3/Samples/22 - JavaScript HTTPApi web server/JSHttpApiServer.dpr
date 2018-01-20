/// sample program which will serve content on http://localhost:888/root
// - in case of *.md file, execute Showdown template engine in JavaScript
program JSHttpApiServer;

{
  ---------------------------------------------------------------------------
   Download the SpiderMonkey library at https://synopse.info/files/synsm.7z
   and put mozjs-24.dll and libnspr4.dll files with your JSHttpApiServer.exe
  ---------------------------------------------------------------------------
}

{$APPTYPE CONSOLE}

{$ifdef WIN64}
begin
  writeln('SpiderMonkey is not handled in 64 bit mode yet');
  
{$else}

// first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Windows,
  SysUtils,
  SynCommons,
  SynZip,
  SynCrtSock,
  SynSM,
  SynSMAPI;
  //mORMotSM;

{$I Synopse.inc}

type
  TTestServer = class
  protected
    fPath: TFileName;
    fServer: THttpApiServer;
    fSMManager: TSMEngineManager;
    fShowDownLib: SynUnicode;
    fCalls: integer;
    function Process(Ctxt: THttpServerRequest): cardinal;
    /// here we can add some functions / libraries to SM
    procedure DoOnNewEngine(const Engine: TSMEngine);
    /// implement a loadfile() JavaScript function in Delphi
    // - expect 1 string argument as filePath
    function LoadFile(const This: variant; const Args: array of variant): variant;
    /// this function will use JSON encoding, instead of variants
    function LoadFileJSON(const This: TSMObject; const Args: RawUTF8): RawUTF8;
  public
    constructor Create(const Path: TFileName);
    destructor Destroy; override;
  end;


{ TTestServer }

constructor TTestServer.Create(const Path: TFileName);
begin
  fServer := THttpApiServer.Create(false);
  fSMManager := TSMEngineManager.Create;
  fSMManager.OnNewEngine := DoOnNewEngine;
  fShowDownLib := AnyTextFileToSynUnicode(ExeVersion.ProgramFilePath+'showdown.js');
  fServer.AddUrl('root','888',false,'+',true);
  fServer.RegisterCompress(CompressDeflate); // our server will deflate html :)
  fServer.OnRequest := Process;
  fServer.Clone(12);
  fPath := IncludeTrailingPathDelimiter(Path);
end;

destructor TTestServer.Destroy;
begin
  fSMManager.Free;
  fServer.Free;
  inherited;
end;

/// native loadFile() function used to load file from disc
// - same as TTestServer.LoadFile() method, but lower-level (I'm quite confident
// you would appreciate the difference in length and complexity!)
// - expect 1 string argument as filePath
function nsm_loadFile(cx: PJSContext; argc: uintN; vp: Pjsval): JSBool; cdecl;
var in_argv: PjsvalVector;
    filePath: TFileName;
begin
  TSynFPUException.ForDelphiCode;
  try
    if argc<>1 then
      raise Exception.Create('Invalid number of args for loadFile(): required 1 (file path)');
    in_argv := JS_ARGV(cx,vp);
    filePath := JSVAL_TO_STRING(in_argv[0]).ToString(cx);
    JS_SET_RVAL(cx, vp, cx^.NewJSString(AnyTextFileToSynUnicode(filePath)).ToJSVal);
    Result := JS_TRUE;
  except
    on E: Exception do begin // all exceptions MUST be catched on Delphi side
      JS_SET_RVAL(cx, vp, JSVAL_VOID);
      JSError(cx, E);
      Result := JS_FALSE;
    end;
  end;
end;

// see how easy it is, when compared to nsm_loadFile() low-level function!
function TTestServer.LoadFile(const This: variant; const Args: array of variant): variant;
begin
  if length(Args)<>1 then
    raise Exception.Create('Invalid number of args for loadFile(): required 1 (file path)');
  result := AnyTextFileToSynUnicode(Args[0]);
end;

// here using JSON has no advantage (it will be used for interface-based call)
function TTestServer.LoadFileJSON(const This: TSMObject; const Args: RawUTF8): RawUTF8;
var input: TDocVariantData; // easy access to JSON array
begin
  input.InitJSON(Args);
  if input.Count<>1 then
    raise Exception.Create('Invalid number of args for loadFile(): required 1 (file path)');
  result:= JSONEncodeArrayOfConst([AnyTextFileToSynUnicode(input.Values[0])],true);
end;

procedure TTestServer.DoOnNewEngine(const Engine: TSMEngine);
var showDownRunner: SynUnicode;
begin
  // add external JavaScript library to engine (port of the Markdown library)
  Engine.Evaluate(fShowDownLib, 'showdown.js');

  // add the bootstrap function calling loadfile() then showdown's makeHtml()
  showDownRunner := AnyTextFileToSynUnicode(ExeVersion.ProgramFilePath+'showDownRunner.js');
  Engine.Evaluate(showDownRunner, 'showDownRunner.js');

  // add native function to the engine (we show the 3 ways)
  Engine.RegisterMethod(Engine.GlobalObj,'loadFile',LoadFile,1);
  //Engine.RegisterMethod(Engine.GlobalObj,'loadFile',LoadFileJSON,1);
  //Engine.GlobalObject.DefineNativeMethod('loadFile', nsm_loadFile, 1);
end;

{$WARN SYMBOL_PLATFORM OFF}

function TTestServer.Process(Ctxt: THttpServerRequest): cardinal;
var W: TTextWriter;
    FileName, FileExt: TFileName;
    FN, SRName, href: RawUTF8;
    content: variant;
    i: integer;
    SR: TSearchRec;
    engine: TSMEngine;
    timer: TPrecisionTimer;       

  procedure hrefCompute;
  begin
    SRName := StringToUTF8(SR.Name);
    href := FN+StringReplaceChars(SRName,'\','/');
  end;

begin
  // most of this method content is taken from sample "09 - HttpApi web server"
  if not IdemPChar(pointer(Ctxt.URL),'/ROOT') then begin
    result := 404;
    exit;
  end;
  FN := StringReplaceChars(UrlDecode(copy(Ctxt.URL,7,maxInt)),'/','\');
  if PosEx('..',FN)>0 then begin
    result := 404; // circumvent obvious potential security leak
    exit;
  end;
  while (FN<>'') and (FN[1]='\') do
    delete(FN,1,1);
  while (FN<>'') and (FN[length(FN)]='\') do
    delete(FN,length(FN),1);
  FileName := fPath+UTF8ToString(FN);

  // 1. either reply directory listing as html
  if DirectoryExists(FileName) then begin
    W := TTextWriter.CreateOwnedStream;
    try
      W.Add('<html><body style="font-family: Arial">'+
        '<h3>%</h3><p><table>',[FN]);
      FN := StringReplaceChars(FN,'\','/');
      if FN<>'' then
        FN := FN+'/';
      if FindFirst(FileName+'\*.*',faDirectory,SR)=0 then begin
        repeat
          if (SR.Attr and faDirectory<>0) and (SR.Name<>'.') then begin
            hrefCompute;
            if SRName='..' then begin
              i := length(FN);
              while (i>0) and (FN[i]='/') do dec(i);
              while (i>0) and (FN[i]<>'/') do dec(i);
              href := copy(FN,1,i);
            end;
            W.Add('<tr><td><b><a href="/root/%">[%]</a></b></td></tr>',[href,SRName]);
          end;
        until FindNext(SR)<>0;
        FindClose(SR);
      end;
      if FindFirst(FileName+'\*.*',faAnyFile-faDirectory-faHidden,SR)=0 then begin
        repeat
          hrefCompute;
          if SR.Attr and faDirectory=0 then
            W.Add('<tr><td><b><a href="/root/%">%</a></b></td><td>%</td><td>%</td></td></tr>',
              [href,SRName,KB(SR.Size),DateTimeToStr(
              {$ifdef ISDELPHIXE2}SR.TimeStamp{$else}FileDateToDateTime(SR.Time){$endif})]);
        until FindNext(SR)<>0;
        FindClose(SR);
      end;
      W.AddShort(
        '</table></p><gr><p><small>Powered by <a href=http://mormot.net>'+
        XPOWEREDPROGRAM+'</a></small></p></body></html>');
      Ctxt.OutContent := W.Text;
      Ctxt.OutContentType := HTML_CONTENT_TYPE;
      result := 200;
    finally
      W.Free;
    end;
  end else begin

    // 2. or SpiderMonkey is used to render the *.md file as HTML :)
    FileExt := ExtractFileExt(FileName);
    if FileExt='.md' then begin
      timer.Start;

      // 2.1 ensure automatic garbage collection
      engine := fSMManager.ThreadSafeEngine;
      i := InterlockedIncrement(fCalls);
      // in a real application should better be per-thread counter
      if i mod 10=0 then // better to place this counter inside each thread
        engine.GarbageCollect else  // naive trigger of GC
        engine.MaybeGarbageCollect; // perform garbage collection if needed

      // 2.2 execute showDownRunner() JavaScript function (via late-binding!)
      content := engine.Global.showDownRunner(FileName);
      //former line of code is the same as the slightly faster:
      //content := engine.GlobalObject.Run('showDownRunner',[SynUnicode(FileName)]);

      // 2.3 return HTML content
      Ctxt.OutContent := FormatUTF8(
        '% <hr><p><small><a href=/root/%s>%</a> rendered by '+
        '<a href=/root/showdown.js>showdown.js</a> '+
        'on <a href=http://mormot.net>'+XPOWEREDPROGRAM+
        '</a> server in % using SpiderMonkey %</small></p></body></html>',
        [content,FN,ExtractFileName(FileName),Timer.Stop,engine.cx.VersionToString]);
      Ctxt.OutContentType := HTML_CONTENT_TYPE;
    end else begin;

      // 3. or http.sys will send the specified file from kernel mode
      if FileExt='.mds' then
        SetLength(FileName,length(FileName)-1);
      Ctxt.OutContent := StringToUTF8(FileName);
      Ctxt.OutContentType := HTTP_RESP_STATICFILE;
      // THttpApiServer.Execute will return 404 if not found
    end;
    result := 200;
  end;
end;

begin
  with TTestServer.Create(ExeVersion.ProgramFilePath) do
  try
    write('Server is now running on http://localhost:888/root'#13#10#13#10+
      'Press [Enter] to quit');
    readln;
  finally
    Free;
  end;
  
{$endif WIN64}
end.

