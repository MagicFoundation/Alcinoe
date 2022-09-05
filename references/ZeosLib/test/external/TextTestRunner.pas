{ $Id: TextTestRunner.pas,v 1.28 2004/10/17 10:39:00 neuromancer Exp $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 1.28 $
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2004.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

unit TextTestRunner;

interface
uses
  SysUtils,
  Classes,
  TestFramework;

const
  rcs_id :string = '#(@)$Id: TextTestRunner.pas,v 1.28 2004/10/17 10:39:00 neuromancer Exp $';

type
  TRunnerExitBehavior = (
    rxbContinue,
    rxbPause,
    rxbHaltOnFailures
    );

  TTextTestListener = class(TInterfacedObject, ITestListener, ITestListenerX)
  protected
    startTime: TDateTime;
    endTime: TDateTime;
    runTime: TDateTime;
  public
    // implement the ITestListener interface
    procedure AddSuccess(test: ITest); virtual;
    procedure AddError(error: TTestFailure); virtual;
    procedure AddFailure(failure: TTestFailure); virtual;
    function  ShouldRunTest(test :ITest):boolean; virtual;
    procedure StartSuite(suite: ITest); virtual;
    procedure EndSuite(suite: ITest); virtual;
    procedure StartTest(test: ITest); virtual;
    procedure EndTest(test: ITest); virtual;
    procedure TestingStarts; virtual;
    procedure TestingEnds(testResult: TTestResult); virtual;
    procedure Status(test :ITest; const Msg :string);
    procedure Warning(test :ITest; const Msg :string);
    function  Report(r: TTestResult): string;
    class function RunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult; overload;
    class function RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;

  protected
    function  PrintErrors(r: TTestResult): string; virtual;
    function  PrintFailures(r: TTestResult): string; virtual;
    function  PrintHeader(r: TTestResult): string; virtual;
    function  PrintFailureItems(r :TTestResult): string; virtual;
    function  PrintErrorItems(r :TTestResult): string; virtual;
    function  TruncateString(s: string; len: integer): string; virtual;
  end;

  {: This type defines what the RunTest and RunRegisteredTests methods will do when
     testing has ended.
     @enum rxbContinue Just return the TestResult.
     @enum rxbPause    Pause with a ReadLn before returnng the TestResult.
     @enum rxbHaltOnFailures   Halt the program if errors or failures occurred, setting
                               the program exit code to FailureCount+ErrorCount;
                               behave like rxbContinue if all tests suceeded.
     @seeAlso <See Unit="TextTestRunner" Routine="RunTest">
     @seeAlso <See Unit="TextTestRunner" Routine="RunRegisteredTests">
     }

{: Run the given test suite
}
function RunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult; overload;
function RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult; overload;

implementation

const
  CRLF = #13#10;

{ TTExtTestListener }

procedure TTextTestListener.AddSuccess(test: ITest);
begin
// No display for successes
end;

procedure TTextTestListener.AddError(error: TTestFailure);
begin
  write('E');
end;

procedure TTextTestListener.AddFailure(failure: TTestFailure);
begin
  write('F');
end;

{:
   Prints failures to the standard output
 }
function TTextTestListener.Report(r: TTestResult): string;
begin
  result := PrintHeader(r) +
            PrintErrors(r) +
            PrintFailures(r);
end;

{:
   Prints the errors to the standard output
 }
function TTextTestListener.PrintErrors(r: TTestResult): string;
begin
  result := '';
  if (r.errorCount <> 0) then begin
    if (r.errorCount = 1) then
      result := result + format('There was %d error:', [r.errorCount]) + CRLF
    else
      result := result + format('There were %d errors:', [r.errorCount]) + CRLF;

    result := result + PrintErrorItems(r);
    result := result + CRLF
  end
end;

function TTextTestListener.PrintFailureItems(r :TTestResult): string;
var
  i: Integer;
  failure: TTestFailure;
begin
  result := '';
  for i := 0 to r.FailureCount-1 do begin
    failure := r.Failures[i];
    result := result + format('%3d) %s: %s'#13#10'     at %s'#13#10'      "%s"',
                               [
                               i+1,
                               failure.failedTest.name,
                               failure.thrownExceptionName,
                               failure.LocationInfo,
                               failure.thrownExceptionMessage
                               ]) + CRLF;
  end;
end;

function TTextTestListener.PrintErrorItems(r :TTestResult): string;
var
  i: Integer;
  failure: TTestFailure;
begin
  result := '';
  for i := 0 to r.ErrorCount-1 do begin
    failure := r.Errors[i];
    result := result + format('%3d) %s: %s'#13#10'     at %s'#13#10'      "%s"',
                               [
                               i+1,
                               failure.failedTest.name,
                               failure.thrownExceptionName,
                               failure.LocationInfo,
                               failure.thrownExceptionMessage
                               ]) + CRLF;
  end;
end;

{:
   Prints failures to the standard output
 }
function TTextTestListener.PrintFailures(r: TTestResult): string;
begin
  result := '';
  if (r.failureCount <> 0) then begin
    if (r.failureCount = 1) then
      result := result + format('There was %d failure:', [r.failureCount]) + CRLF
    else
      result := result + format('There were %d failures:', [r.failureCount]) + CRLF;

    result := result + PrintFailureItems(r);
    result := result + CRLF
  end
end;

{:
   Prints the header of the Report
 }
function TTextTestListener.PrintHeader(r: TTestResult): string;
begin
  result := '';
  if r.wasSuccessful then
  begin
    result := result + CRLF;
    result := result + format('OK: %d tests'+CRLF, [r.runCount]);
  end
  else
  begin
    result := result + CRLF;
    result := result + 'FAILURES!!!'+CRLF;
    result := result + 'Test Results:'+CRLF;
    result := result + format('Run:      %8d'+CRLF+'Failures: %8d'+CRLF+'Errors:   %8d'+CRLF,
                      [r.runCount, r.failureCount, r.errorCount]
                      );
  end
end;

procedure TTextTestListener.StartTest(test: ITest);
begin
  write('.');
end;

procedure TTextTestListener.EndTest(test: ITest);
begin

end;

function TTextTestListener.TruncateString(s: string; len: integer): string;
begin
  if Length(s) > len then
    result := copy(s, 1, len) + '...'
  else
    result := s
end;

procedure TTextTestListener.TestingStarts;
begin
  writeln;
  writeln('DUnit / Testing');
  startTime := now;
end;

procedure TTextTestListener.TestingEnds(testResult: TTestResult);
var
  h, m, s, l :Word;
begin
  endTime := now;
  runTime := endTime-startTime;
  writeln;
  DecodeTime(runTime, h,  m, s, l);
  writeln(Format('Time: %d:%2.2d:%2.2d.%d', [h, m, s, l]));
  writeln(Report(testResult));
  writeln;
end;

class function TTextTestListener.RunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
begin
  Result := TestFramework.RunTest(suite, [TTextTestListener.Create]);
  case exitBehavior of
    rxbPause:
      try
        writeln('Press <RETURN> to continue.');
        readln
      except
      end;
    rxbHaltOnFailures:
{$IFNDEF CLR}
      with Result do
      begin
        if not WasSuccessful then
          System.Halt(ErrorCount+FailureCount);
      end
{$ENDIF}
    // else fall through
  end;
end;

class function TTextTestListener.RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
begin
  Result := RunTest(registeredTests, exitBehavior);
end;

function RunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
begin
  Result := TestFramework.RunTest(suite, [TTextTestListener.Create]);
  case exitBehavior of
    rxbPause:
      try
        writeln('Press <RETURN> to continue.');
        readln
      except
      end;
    rxbHaltOnFailures:
{$IFNDEF CLR}
      with Result do
      begin
        if not WasSuccessful then
          System.Halt(ErrorCount+FailureCount);
      end
{$ENDIF}
    // else fall through
  end;
end;

function RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
begin
   Result := RunTest(registeredTests, exitBehavior);
end;


procedure TTextTestListener.Status(test: ITest; const Msg: string);
begin
  writeln(Format('%s: %s', [test.Name, Msg]));
end;

procedure TTextTestListener.Warning(test: ITest; const Msg: string);
begin
  writeln(Format('%s: %s', [test.Name, Msg]));
end;

function TTextTestListener.ShouldRunTest(test: ITest): boolean;
begin
  Result := test.Enabled;
end;

procedure TTextTestListener.EndSuite(suite: ITest);
begin
end;

procedure TTextTestListener.StartSuite(suite: ITest);
begin
end;

end.
