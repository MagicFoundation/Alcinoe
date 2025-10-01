{*************************************************************************************************
*  ZLibEx.inc                                                                                    *
*  copyright (c) 2006-2013 base2 technologies                                                    *
*                                                                                                *
*  version information for delphi/c++ builder                                                    *
*                                                                                                *
*  revision history                                                                              *
*    2013.05.23  updated for delphi xe3 (2013)                                                   *
*    2012.05.01  updated for delphi xe2 (2012)                                                   *
*    2010.09.18  updated for delphi xe (2011)                                                    *
*    2010.01.27  updated for delphi 2010                                                         *
*    2009.04.11  updated to use CONDITIONALEXPRESSIONS and CompilerVersion                       *
*    2009.01.28  updated for delphi 2009                                                         *
*    2007.10.01  updated for delphi 2007                                                         *
*    2005.11.29  created                                                                         *
*                                                                                                *
*  acknowledgments                                                                               *
*    iztok kacin                                                                                 *
*      2009.04.11  CONDITIONALEXPRESSIONS and CompilerVersion changes                            *
*************************************************************************************************}

{$ifndef CONDITIONALEXPRESSIONS}

  {** delphi ************************************************************************************}

  {$ifdef VER80}                        // delphi 1
    {$define Delphi}

    {$define Version1}
  {$endif}

  {$ifdef VER90}                        // delphi 2
    {$define Delphi}

    {$define Version2}
  {$endif}

  {$ifdef VER100}                       // delphi 3
    {$define Delphi}

    {$define Version3}
  {$endif}

  {$ifdef VER120}                       // delphi 4
    {$define Delphi}

    {$define Version4}
  {$endif}

  {** c++ builder *******************************************************************************}

  {$ifdef VER93}                        // c++ builder 1
    {$define CBuilder}

    {$define Version1}
  {$endif}

  {$ifdef VER110}                       // c++ builder 3
    {$define CBuilder}

    {$define Version3}
  {$endif}

  {$ifdef VER125}                       // c++ builder 4
    {$define CBuilder}

    {$define Version4}
  {$endif}

  {** delphi/c++ builder (common) ***************************************************************}

  {$ifdef VER130}                       // delphi/c++ builder 5
    {$ifdef BCB}
      {$define CBuilder}
    {$ELSE}
      {$define Delphi}
    {$endif}

    {$define Version5}

    {$define Version5Plus}
  {$endif}

{$ELSE}

  {$ifdef BCB}
    {$define CBuilder}
  {$ELSE}
    {$define Delphi}
  {$endif}

  {$define Version5Plus}

  {$if CompilerVersion >= 14.0}         // delphi 6
    {$ifdef VER140}
      {$define Version6}
    {$endif}

    {$define Version6Plus}
  {$ifend}

  {$if CompilerVersion >= 15.0}         // delphi 7
    {$ifdef VER150}
      {$define Version7}
    {$endif}

    {$define Version7Plus}
  {$ifend}

  {$if CompilerVersion >= 16.0}         // delphi 8 (.net)
    {$ifdef VER160}
      {$define Version8}
    {$endif}

    {$define Version8Plus}
  {$ifend}

  {$if CompilerVersion >= 17.0}         // delphi 2005
    {$ifdef VER170}
      {$define Version2005}
    {$endif}

    {$define Version2005Plus}
  {$ifend}

  {$if CompilerVersion >= 18.0}         // bds 2006
    {$ifdef VER180}
      {$define Version2006}
    {$endif}

    {$define Version2006Plus}
  {$ifend}

  {$if CompilerVersion >= 18.5}         // bds 2007
    {$ifdef VER185}
      {$define Version2007}
    {$endif}

    {$define Version2007Plus}
  {$ifend}

  {$if CompilerVersion >= 20.0}         // bds 2009
    {$ifdef VER200}
      {$define Version2009}
    {$endif}

    {$define Version2009Plus}
  {$ifend}

  {$if CompilerVersion >= 21.0}         // bds 2010
    {$ifdef VER210}
      {$define Version2010}
    {$endif}

    {$define Version2010Plus}
  {$ifend}

  {$if CompilerVersion >= 22.0}         // bds xe (2011)
    {$ifdef VER220}
      {$define Version2011}
    {$endif}

    {$define Version2011Plus}
  {$ifend}

  {$if CompilerVersion >= 23.0}         // bds xe2 (2012)
    {$ifdef VER230}
      {$define Version2012}
    {$endif}

    {$define Version2012Plus}
  {$ifend}

  {$if CompilerVersion >= 24.0}         // bds xe3 (2013)
    {$ifdef VER240}
      {$define Version2013}
    {$endif}

    {$define Version2013Plus}
  {$ifend}

{$endif}
