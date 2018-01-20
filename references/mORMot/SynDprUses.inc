{$ifndef LVCL}
{$ifdef FPC} // we may be on Kylix or upcoming Delphi for Linux
  {$ifdef Unix} // we may also be on Darwin / OSX  
  cthreads, // if you use threads: always needed for servers
  {$ifdef Darwin}
  iosxwstr, // optional WideString manager for Mac, but won't hurt
  {$else}
  cwstring, // optional WideString manager, but won't hurt
  {$endif}
  {$else}
    {$ifopt D-} // at the moment, debug mode with cmem causes trouble
    //cmem, // default FPC's heap manager is very RAM hungry (one heap per thread)
    {$endif}
  //FastMM4,  // need the latest version, e.g. from lib\kylix sub folder
  {$endif}
{$else}  // enable FastMM4 on older versions of Delphi
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$if CompilerVersion <= 17} // Delphi 2006 (and up) have FastMM4 integrated
      FastMM4,
    {$ifend}
    {$ifdef UNICODE}
    {$WARN DUPLICATE_CTOR_DTOR OFF} // global at .dpr level - see QC 100815
    {$endif}
  {$ELSE}
    FastMM4, // no conditional expressions -> versions older than Delphi 6
  {$ENDIF}
{$endif FPC}
{$endif LVCL}

