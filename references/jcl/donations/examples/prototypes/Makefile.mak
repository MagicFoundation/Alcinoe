#
# Generates VisualCLX / VCL dependent units from common code base
#
# Uwe Schuster, 2004-01-31
#

jpp		= ..\..\..\jcl\source\prototypes\jpp.exe

release:	VCL VisualCLX

VCL:    	JclDFMTestMain.pas
        $(jpp) -c -dVCL -uVisualCLX -x..\vcl\dfm\ $**

VisualCLX:    	JclDFMTestMain.pas
	$(jpp) -c -dVisualCLX -uVCL -x..\visclx\dfm\ $**
	

