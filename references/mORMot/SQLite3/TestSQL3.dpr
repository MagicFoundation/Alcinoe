/// Main unit testing program of the Synopse mORMot framework
// - this program is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
program TestSQL3;

(*
    This file is part of Synopse mORMot database framework.

    Synopse mORMot framework. Copyright (C) 2016 Arnaud Bouchez
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

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2016
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


  Version 1.13
  - code modifications to compile with Delphi 5 compiler (no ORM code compiles
    yet: so only low-level units like SynCommons / SynCrypto / SynPdf are tested)
  - note: in order to be able to use http.sys server under Vista or Seven,
    first compile then execute TestSQL3Register.dpr (need Administrator rights)

  Version 1.15
  - all tests passed with Delphi XE2 (32 Bit)
  - SynSQLite3 logic extracted from SQLite3 unit
  - enhanced tests about external database handling
  - tests renamed to match the new "mORMot" framework name

  Version 1.16
  - all tests are now implemented in a separated SQLite3SelfTests unit -
    this is requested by Delphi XE2 background compiler issues

  Version 1.18
  - renamed SQLite3*.pas units to mORMot*.pas
  - included Windows 64 bit regression tests (and potential FullDebugMode)
  - all tests passed with Delphi XE3 up to 10.1 Berlin for Win32 and Win64 platforms


  this application has EnableMemoryLeakReporting conditional defined in its
  Project/Options -> we can therefore ensure that our mORMot Client/Server
  framework classes have no memory leak
  - Search Path and Debug Path: [\Dev\Lib\LVCL]  (if you plan to use LVCL)
  - Conditional defines: EnableMemoryLeakReporting;USEFTS[;ENHANCEDRTL][;LVCL]
  - if you don't have installed our Enhanced Run Time Library, please delete
    ENHANCEDRTL global conditional
  - if you do not plan to use LVCL, do not refers to these libraries
    (which works only for Delphi 7)
  - first line of uses clause must be  {$I SynDprUses.inc}  to enable FastMM4
    on older versions of Delphi *)

{$ifdef Linux}
  {$ifdef FPC_CROSSCOMPILING}
    {$linklib libc_nonshared.a}
  {$endif}
{$endif}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

uses
  {$ifdef KYLIX3}
  FastMM4,
  ECCProcess in 'Samples/33 - ECC/ECCProcess.pas',
  mORMotSelfTests;
  {$else}
  {$ifdef FullDebugMode}  // defined for the project e.g. under Win64
  FastMM4Messages in '..\RTL7\FastMM4Messages.pas',
  FastMM4 in '..\RTL7\FastMM4.pas',
  {$else}
  {$I SynDprUses.inc}    // will enable FastMM4 prior to Delphi 2006
  {$endif FullDebugMode}
  SysUtils,
  //SynFastWideString,   // no speed benefit for mORMot, but OleDB/Jet works!
  SynLZ in '..\SynLZ.pas',
  SynLZO in '..\SynLZO.pas',
  SynLizard in '..\SynLizard.pas',
  SynCrypto in '..\SynCrypto.pas',
  SynEcc in '..\SynEcc.pas',
  SynCrtSock in '..\SynCrtSock.pas',
  SynBidirSock in '..\SynBiDirSock.pas',
  SynCommons in '..\SynCommons.pas',
  SynLog in '..\SynLog.pas',
  SynTests in '..\SynTests.pas',
{$ifndef DELPHI5OROLDER}
  {$ifndef LVCL}
  SynMongoDB in '..\SynMongoDB.pas',
  {$ifndef NOVARIANTS}
  SynMustache in '..\SynMustache.pas',
  mORMotWrappers in 'mORMotWrappers.pas',
  mORMotMVC in 'mORMotMVC.pas',
  mORMotDDD in 'mORMotDDD.pas',
  dddDomAuthInterfaces in 'DDD\dom\dddDomAuthInterfaces.pas',
  dddDomUserTypes in 'DDD\dom\dddDomUserTypes.pas',
  dddDomUserInterfaces in 'DDD\dom\dddDomUserInterfaces.pas',
  dddDomUserCQRS in 'DDD\dom\dddDomUserCQRS.pas',
  dddInfraAuthRest in 'DDD\infra\dddInfraAuthRest.pas',
  dddInfraSettings in 'DDD\infra\dddInfraSettings.pas',
  dddInfraApps in 'DDD\infra\dddInfraApps.pas',
  dddInfraEmail in 'DDD\infra\dddInfraEmail.pas',
  dddInfraEmailer in 'DDD\infra\dddInfraEmailer.pas',
  dddInfraRepoUser in 'DDD\infra\dddInfraRepoUser.pas',
  {$endif NOVARIANTS}
  {$endif LVCL}
  {$ifdef MSWINDOWS}
  {$ifndef CPU64}
  SynSMAPI in '..\SynSMAPI.pas',
  SynSM in '..\SynSM.pas',
  {$endif CPU64}
  SynTable in '..\SynTable.pas',
  SynBigTable in '..\SynBigTable.pas',
  {$ifndef LVCL}
  SynZipFiles in '..\SynZipFiles.pas',
  {$endif}
  {$endif}
  SynSQLite3 in '..\SynSQLite3.pas',
  SynSQLite3Static in '..\SynSQLite3Static.pas',
  mORMot in 'mORMot.pas',
  mORMotSQLite3 in 'mORMotSQLite3.pas',
  mORMotHttpClient in 'mORMotHttpClient.pas',
  mORMotHttpServer in 'mORMotHttpServer.pas',
  {$ifndef FPC}
  mORMotFastCgiServer in 'mORMotFastCgiServer.pas',
  mORMotService in 'mORMotService.pas',
  //mORMotBigTable,
  {$endif FPC}
  ECCProcess in 'Samples\33 - ECC\ECCProcess.pas',
{$endif DELPHI5OROLDER}
{$ifndef LVCL}
{$ifdef FPC}
{$ifdef WIN64}
  SynZLibSSE in '..\SynZLibSSE.pas',
{$endif}
{$else}
  SynPdf in '..\SynPdf.pas',
  SynGdiPlus in '..\SynGdiPlus.pas',
{$endif FPC}
  SynDB in '..\SynDB.pas',
  SynDBSQLite3 in '..\SynDBSQLite3.pas',
  {$ifdef MSWINDOWS}
  SynDBOracle in '..\SynDBOracle.pas',
  SynOleDB in '..\SynOleDB.pas',
  SynDBODBC in '..\SynDBODBC.pas',
  {$ifdef USEZEOS}
  SynDBZeos in '..\SynDBZeos.pas',
  {$endif}
  {$endif}
{$ifndef DELPHI5OROLDER}
  SynDBRemote in '..\SynDBRemote.pas',
  mORMotDB in 'mORMotDB.pas',
  mORMotMongoDB in 'mORMotMongoDB.pas',
{$endif DELPHI5OROLDER}
{$endif LVCL}
  SynZip in '..\SynZip.pas',
  SynProtoRTSPHTTP in '..\SynProtoRTSPHTTP.pas',
  SynSelfTests in '..\SynSelfTests.pas',
  mORMotSelfTests in 'mORMotSelfTests.pas';
{$endif KYLIX3}

{$ifdef MSWINDOWS}
{$R ..\Vista.res} // includes manifest to identify Windows 10 OS
{$endif}

begin
  {$ifdef ISDELPHI2007ANDUP}
  {$ifdef DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$endif}
  {$endif}
  SQLite3ConsoleTests;
  {$ifdef COMPUTEFPCINTERFACES}
  ChDir(ExeVersion.ProgramFilePath);
  ComputeFPCInterfacesUnit(
    ['..\CrossPlatform\templates','..\..\CrossPlatform\templates'],
     '\..\..\SQlite3\TestSQL3FPCInterfaces.pas');
  {$endif}
end.
