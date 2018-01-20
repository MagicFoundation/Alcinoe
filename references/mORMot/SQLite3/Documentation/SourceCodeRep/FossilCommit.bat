del *.bak /s> nul

fossil ci -M %1

rem push if chkFossilPush checked in SourceCodeRep tool
if %2==1 (
  fossil push
)

@echo.
@pause