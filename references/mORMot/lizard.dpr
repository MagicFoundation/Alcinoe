/// compiled under FPC to have working lizard1-32.dll/lizard1-64.dll files for Delphi
library lizard;

uses
  SynLizard;  // LIZARD_STANDALONE should be defined in the Lazarus Projects Options

{$ifndef FPC}
  FPC-only library - not compilable under Delphi
{$endif}

exports
  Lizard_versionNumber, Lizard_compressBound, Lizard_compress,
  Lizard_sizeofState, Lizard_compress_extState,
  Lizard_decompress_safe, Lizard_decompress_safe_partial;

begin
end.
