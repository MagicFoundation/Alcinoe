/// automated tests for common units of the Synopse mORMot Framework
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotSelfTests;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2018 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2018
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.16
  - first public release, corresponding to SQLite3 Framework 1.16
  - all mORMot tests are now implemented in this separated unit: this is
    requested by bugs in the Delphi XE2 background compilers: main compiler
    was OK with our code (i.e. it compiles into .exe and run as expected), but
    background IDE compilers (used e.g. for syntax checking) was not able
    to compile the tests within the main .dpr source code

  Version 1.17
  - fixed LVCL and Delphi 5 compilation issues

  Version 1.18
  - renamed SQLite3SelfTests.pas to mORMotSelfTests.pas
  - added TInterfaceStub and TInterfaceMock classes testing
  - added multi-thread stress tests over all handled communication protocols
  - included WebSockets and DDD dedicated testing

}

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64

/// this is the main entry point of the tests
// - this procedure will create a console, then run all available tests
procedure SQLite3ConsoleTests;


implementation

uses
  {$ifdef MSWINDOWS}
  Windows, // for AllocConsole
  {$ifndef DELPHI5OROLDER}
  SynBigTable,
  {$endif}
  {$endif}
  {$ifndef DELPHI5OROLDER}
  mORMot, // for TSQLLog
  {$endif}
  SynLog,
  SynBidirSock, // for WebSocketLog
  SynTests,
  SynSelfTests;

type
  /// Synopse mORMot Framework unitary testing
  // - this class will launch all available SW tests for the freeware Synopse
  // mORMot framework
  // - inherits from TSynTestsLogged in order to create enhanced log information
  // in case of any test case failure
  TTestSynopsemORMotFramework = class(TSynTestsLogged)
  published
    /// test the freeware Synopse library
    // - low level functions and classes, cryptographic or compression routines,
    // PDF generation
    procedure SynopseLibraries;
    {$ifndef DELPHI5OROLDER}
    /// test the freeware Synopse mORMot framework
    // - access to SQLite3 or external engines, ORM features, Client/Server
    procedure _mORMot;
    {$endif DELPHI5OROLDER}
  end;


{ TTestSynopsemORMotFramework }

procedure TTestSynopsemORMotFramework.SynopseLibraries;
begin
  //AddCase(TTestCompression);
  //exit;
  AddCase([TTestLowLevelCommon,
    TTestLowLevelTypes,
{$ifdef MSWINDOWS}
{$ifndef FPC}
{$ifndef DELPHI5OROLDER}
    TTestBigTable,
{$endif}
{$ifndef LVCL}
    TTestSynopsePDF, // PDF uses SynGDIPlus or Jpeg.pas
{$endif}
{$endif}
{$endif}
    TTestCryptographicRoutines,
    TTestECCCryptography,
    TTestCompression,
    TTestProtocols
   ]);
end;

{$ifdef DELPHI5OROLDER}
type // mORMot.pas unit doesn't compile with Delphi 5 yet
  TSQLLog = TSynLog;
{$else}
procedure TTestSynopsemORMotFramework._mORMot;
begin
  //AddCase(TTestFileBased);
  //exit; // (*
  AddCase([TTestFileBased,TTestFileBasedMemoryMap,TTestFileBasedWAL]);
  AddCase(TTestMemoryBased);
  AddCase(TTestBasicClasses);
  // *)
  AddCase(TTestClientServerAccess);
  // (*
  AddCase(TTestServiceOrientedArchitecture);
  AddCase(TTestBidirectionalRemoteConnection);
  AddCase(TTestExternalDatabase);
  AddCase(TTestMultiThreadProcess);
  AddCase([TTestDDDSharedUnits,TTestDDDMultiThread]);
  //exit; // *)
end;
{$endif DELPHI5OROLDER}

procedure SQLite3ConsoleTests;
begin
  {$ifdef MSWINDOWS}
  AllocConsole;
  {$endif}
  {$ifndef DELPHI5OROLDER}
  TSynLogTestLog := TSQLLog; // share the same log file with whole mORMot
  {$endif}
  WebSocketLog := TSQLLog; // enable low-level WebSockets frames logging
  if false then // "if not false then" will create around 1.2 GB of log file
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    //DestinationPath := ExeVersion.ProgramFilePath+'logs'; folder should exist
    PerThreadLog := ptIdentifiedInOnFile;
    //HighResolutionTimestamp := true;
    //RotateFileCount := 5; RotateFileSizeKB := 20*1024; // rotate by 20 MB logs
  end
  else
    TSQLLog.Family.Level := []; // NO log by default (ignore expected ERROR 400)
  // testing is performed by some dedicated classes defined in the above units
  with TTestSynopsemORMotFramework.Create('Synopse mORMot Framework Automated tests') do
  try
    if ParamCount<>0 then begin
      SaveToFile(paramstr(1)); // DestPath on command line -> export to file
      Writeln(Ident,#13#10#13#10' Running tests... please wait');
    end;
    if not Run then
      ExitCode := 1;
    if ParamCount<>0 then
      exit; // direct exit if an external file was generated
  finally
    Free;
  end;
  {$ifndef LINUX}
  WriteLn(#13#10'Done - Press ENTER to Exit');
  ReadLn;
  {$endif}
end;

end.

