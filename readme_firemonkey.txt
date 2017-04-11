Stéphane Vander Clock 
skype/email: svanderclock@yahoo.fr

you can access the last svn version at:
svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code 
or with a web browser at:
http://alcinoe.svn.sourceforge.net/viewvc/alcinoe/


INSTALL:
--------
 
If you plan to use native control on android (like TALEdit) then you
will need to include lib\jar\alcinoe\alcinoe.jar (first compile it with 
compilejar_xxx.bat) in project manager > target plateform > android > Libraries


WHAT ARE THE ALCINOE FIREMONKEY CONTROLS:
-----------------------------------------

THE FACT: Painting of Firmonkey controls can be sometime slow, or say 
differently, not sufficiently fast for a fluid scrolling. For exemple if 
you simply look the basic Trectangle with round corners, the paint procedure 
can take around 3 ms! So if you have around 20 visible Trectangles on your 
screen, then it's will cost you around 60 ms to repaint the full screen 
(and normally you don't have only trectangle, you also have Tlabel, 
tcheckbox, etc..). After it's just math, take 100ms to repaint the screen, 
so you can only do around 10 frames per seconds (in reality you will have 
much less even) so the scrolling can't be fluid :(

THE SOLUTION: I didn't want to rebuild the firemonkey controls, it's too 
huge job for me, and instead I try to find an intermediate solution. 
This what I find by adding "doublebuffered" property to the firemonkey 
controls. So instead to repaint and repaint (and repaint) the controls 
for every single pixels move of the scrollbox, I first paint the control 
on a "buffer" that I store directly in the GPU memory (through TTexture), 
and when the system ask me to repaint the controls instead of calling 
again the paint algorithm i simply redraw the buffer TTexture.

THE RESULTS: As I say before it's took 3 ms just to paint a simple 
Trectangle with round corners. With my doublebuffered property 
it's take now around 0.1 ms ! so now the scroll look much more 
fluid! It's a success


ABOUT OPENGL DRAW => REPLACED BY NATIVE IOS/ANDROID DRAW
--------------------------------------------------------

Most of the basic shape (like Trectangle, Tcircle, etc.) use openGL to 
draw. it's not very efficient, for example to draw a circle under 
openGL you will in fact draw 50 triangles. This result often in poor 
quality : https://quality.embarcadero.com/browse/RSP-15206
For roundrect it's even worse because you must first calculate the 
path and then later draw it (much more slow than Tcircle)

Other problem is all of these draw depend of Form.quality. if you set 
form.quality to highquality then everythink you will do on the
canvas will be multisample like drawing an image for exemple and that 
could be problematic because the image will be anti-aliased. if you set 
form.quality to highperformance then the draw will be very rough (no 
anti aliasing). 

To resolve this, i build the buffer of my control using NATIVE 
ANDROID/IOS API. In this way we will have a high quality draw at also
a high speed without being dependant of the form.quality


WHY I DON'T LIKE FIREMONKEY STYLE
---------------------------------

I don't like the style in firmonkey, when it's not buggy and not 
licenced, it's very hard to construct (the editor is just a mess) 
and maintain, especially on each new version of delphi, you must 
drop your old style and recreate it from scratch on every update 
of delphi if you want to stay uptodate. The idea was here (template
of controls) but was badly maded. You can use them mainly if you 
don't need to touch them

this why i avoid any components that descend from styledcontrol,
in fact the dfm/fmx are already a good templating system to 
setup all the properties of your controls 


ABOUT TRUE NATIVE CONTROL LIKE TEDIT
------------------------------------

The spirit is to mix firemonkey control with native platform control
when the fonctionality on such control start to be very hard to
implement (like webbrowser, edit, memo, datepicker, etc.). But it's
not to make several distinct form for several platform like 
offer for exemple http://www.turbococoa.com/ (but this option is
also a good alternative in some way, it's up to you to decide)

In delphi (berlin) their is already IOS platform control that was 
quite well implemented but close to none android platform control 
and so i start to build native android/ios controls like TEdit. 
These control work mostly like some windows that are placed on The
top of the form (so off course no z-order with firemonkey control)