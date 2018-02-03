#
# Generates container topics
#
# $Id$
#

jppContainers	= ..\..\jcl\devtools\jppContainers.exe
touch		= $(MAKEDIR)\touch.exe

ContainerOptions	= -c -m -i -w -dSUPPORTS_UNICODE_STRING -f..\\

Containers:	..\ContainerCopies.dtx

ContainersProt: ContainerCopies.dtx

ContainerCopies.dtx: \
		JclAlgorithms.inc JclArrayLists.inc JclArraySets.inc JclBinaryTrees.inc JclContainerIntf.inc JclHashMaps.inc JclHashSets.inc JclLinkedLists.inc JclQueues.inc JclSortedMaps.inc JclStacks.inc JclTrees.inc JclVectors.inc
	$(touch) $@

..\ContainerCopies.dtx: ContainerCopies.dtx
	$(jppContainers) $(ContainerOptions) $<

