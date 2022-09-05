//---------------------------------------------------------------------------

#include <vcl.h>
#include <TextTestRunner.hpp>
#include <ZPerformanceTestCase.hpp>
#pragma hdrstop

//---------------------------------------------------------------------------

#pragma argsused
int main(int argc, char* argv[])
{
  Texttestrunner::RunRegisteredTests(rxbContinue);
  PerformanceResultProcessor->ProcessResults();
  PerformanceResultProcessor->PrintResults();
  return 0;
}
//---------------------------------------------------------------------------
 
