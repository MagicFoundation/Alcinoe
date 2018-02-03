#
# Generates platform dependent units from common code base
#
# $Id$
#

jpp		= ..\..\devtools\jpp.exe
jppContainers	= ..\..\devtools\jppContainers.exe
touch		= $(MAKEDIR)\touch.exe

Options			= -c -dJCL -dSUPPORTS_DEFAULTPARAMS -dSUPPORTS_INT64
# CommonOptions		= $(Options) -f..\common\\
VclOptions		= $(Options) -dVCL -dMSWINDOWS -uUnix -dBitmap32 -f..\vcl\\
WinOptions		= $(Options) -dMSWINDOWS -uUNIX -uHAS_UNIT_LIBC -f..\windows\\
Win32Options		= $(Options) -uHAS_UNIT_LIBC -f..\windows\\
ContainerOptions	= $(Options) -m -ijcl.inc -f..\Common\\
UnixOptions		= $(Options) -uMSWINDOWS -dUNIX -f..\unix\\

release:	VCL Windows ContainersProt Containers

VCL:    	..\vcl\JclGraphics.pas \
		..\vcl\JclGraphUtils.pas

Windows:        ..\windows\JclWin32.pas \
                ..\windows\Hardlinks.pas

ContainersProt:	JclAlgorithms.pas \
		JclArrayLists.pas \
		JclArraySets.pas \
		JclBinaryTrees.pas \
		JclContainerIntf.pas \
		JclHashMaps.pas \
		JclHashSets.pas \
		JclLinkedLists.pas \
		JclQueues.pas \
		JclSortedMaps.pas \
		JclStacks.pas \
                JclTrees.pas \
		JclVectors.pas

Containers:	..\Common\JclAlgorithms.pas \
		..\Common\JclArrayLists.pas \
		..\Common\JclArraySets.pas \
		..\Common\JclBinaryTrees.pas \
		..\Common\JclContainerIntf.pas \
		..\Common\JclHashMaps.pas \
		..\Common\JclHashSets.pas \
		..\Common\JclLinkedLists.pas \
		..\Common\JclQueues.pas \
		..\Common\JclSortedMaps.pas \
		..\Common\JclStacks.pas \
                ..\Common\JclTrees.pas \
		..\Common\JclVectors.pas

..\windows\JclWin32.pas: \
                JclWin32.pas
        $(jpp) -ijcl.inc -iwindowsonly.inc $(WinOptions) $?

JclAlgorithms.pas: \
		containers\JclAlgorithms.int containers\JclAlgorithms.imp
	$(touch) $@

JclArrayLists.pas: \
		containers\JclArrayLists.imp containers\JclArrayLists.int containers\JclContainerCommon.imp containers\JclAlgorithms.int containers\JclAlgorithms.imp
	$(touch) $@

JclArraySets.pas: \
		containers\JclArraySets.imp containers\JclArraySets.int containers\JclContainerCommon.imp
	$(touch) $@

JclBinaryTrees.pas: \
		containers\JclBinaryTrees.imp containers\JclBinaryTrees.int containers\JclContainerCommon.imp
	$(touch) $@

JclContainerIntf.pas: \
		containers\JclContainerIntf.int
	$(touch) $@

JclHashMaps.pas: \
		containers\JclHashMaps.imp containers\JclHashMaps.int containers\JclContainerCommon.imp containers\JclAlgorithms.int containers\JclAlgorithms.imp
	$(touch) $@

JclHashSets.pas: \
		containers\JclHashSets.imp containers\JclHashSets.int containers\JclContainerCommon.imp containers\JclAlgorithms.int containers\JclAlgorithms.imp
	$(touch) $@

JclLinkedLists.pas: \
		containers\JclLinkedLists.imp containers\JclLinkedLists.int containers\JclContainerCommon.imp
	$(touch) $@

JclQueues.pas: \
		containers\JclQueues.imp containers\JclQueues.int containers\JclContainerCommon.imp containers\JclAlgorithms.int containers\JclAlgorithms.imp
	$(touch) $@

JclSortedMaps.pas: \
		containers\JclSortedMaps.imp containers\JclSortedMaps.int containers\JclContainerCommon.imp containers\JclAlgorithms.int containers\JclAlgorithms.imp
	$(touch) $@

JclStacks.pas: \
		containers\JclStacks.imp containers\JclStacks.int containers\JclContainerCommon.imp
	$(touch) $@

JclTrees.pas: \
		containers\JclTrees.imp containers\JclTrees.int containers\JclContainerCommon.imp
	$(touch) $@

JclVectors.pas: \
		containers\JclVectors.imp containers\JclVectors.int containers\JclContainerCommon.imp containers\JclAlgorithms.int containers\JclAlgorithms.imp
	$(touch) $@

{.}.pas{..\common}.pas:
	$(jppContainers) $(ContainerOptions) $<

{.}.pas{..\windows}.pas:
	$(jpp) $(WinOptions) $<

{.}.pas{..\unix}.pas:
	$(jpp) $(UnixOptions) $<

{.}.pas{..\vcl}.pas:
	$(jpp) $(VclOptions) $<

