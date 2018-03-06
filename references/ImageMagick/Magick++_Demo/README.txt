This directory contains the source for a set of Magick++ demo programs
as well as the Visual C++ 6.0 project files required to build them.

To build the demos, open the Workspace file Magick++_Demo.dsw in Visual C++
and select "build all".

The batch script run_demos.bat runs all of the demos in the current directory.
Output from the files use the naming scheme *_out.miff.  These files may be
viewed via the imdisplay program (e.g. "imdisplay demo_out.miff").

The provided demos include:

  demo     - creates a montage illustrating most of the image processing
             operations.
  detrans  - simple utility to remove transparency from a file
  flip     - demonstrates manipulating animations by flipping an animation
             upside down.
  gravity  - creates an animation showing how gravity effects text placement
  piddle   - a vector drawing demo which illustrates use of a drawable list
  shapes   - a simple vector drawing demo
  zoom     - simple utility for evaluating the effect of resize filters on a
             resized image.


