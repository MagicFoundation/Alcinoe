{$ifndef LVCL}
  {$ifdef FPC} // we may be on Kylix or upcoming Delphi for Linux
    {$ifdef Unix} // we may also be on Darwin / OSX
      cthreads, // should be the very first unit - before FastMM4 e.g.
    {$endif Unix}
    {$ifdef FPC_X64MM_WIN} // SynFPCx64MM is not fully validated on Windows yet
      {$ifdef MSWINDOWS}
      SynFPCx64MM,
      {$endif MSWINDOWS}
    {$else}
      {$ifdef FPC_X64MM}
        {$ifndef MSWINDOWS}
        SynFPCx64MM,
        {$endif MSWINDOWS}
      {$else}
        {$ifdef FPC_FASTMM4}
          FastMM4Messages in '..\RTL7\FastMM4Messages.pas',
          FastMM4 in '..\RTL7\FastMM4.pas',
        {$else}
          {$ifdef FPC_SYNJEMALLOC}
            SynFPCCMemAligned, // after "apt-get install libjemalloc1"
          {$else}
            {$ifdef FPC_SYNTBB}
              SynFPCCMemAligned, // after "apt-get install libtbb2"
            {$else}
              {$ifdef FPC_SYNCMEM}
                SynFPCCMemAligned, // you may also set FPC_NO_DEFAULT_MEMORYMANAGER
              {$else}
                {$ifdef FPC_CMEM}
                  cmem, // you may also set FPC_NO_DEFAULT_MEMORYMANAGER
                {$endif FPC_CMEM}
              {$endif FPC_SYNCMEM}
            {$endif FPC_SYNTBB}
          {$endif FPC_SYNJEMALLOC}
        {$endif FPC_FASTMM4}
      {$endif FPC_X64MM}
    {$endif FPC_X64MM_WIN}
    {$ifdef Unix} // we may also be on Darwin / OSX
      {$ifdef Darwin}
        // iosxwstr, // optional WideString manager for Mac
      {$else}
        // cwstring, // optional WideString manager
      {$endif Darwin}
    {$else}
      {$ifopt D-} // at the moment, debug mode with cmem causes trouble
        //cmem, // default FPC's heap manager is very RAM hungry (one heap per thread)
      {$endif}
    {$endif Unix}
  {$else}  // enable FastMM4 on older versions of Delphi
    {$IFDEF CONDITIONALEXPRESSIONS}
      {$if CompilerVersion <= 17} // Delphi 2006 (and up) have FastMM4 integrated
      FastMM4Messages,
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

