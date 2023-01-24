Alcinoe
=======

Alcinoe is a library of visual and non-visual components for
Delphi. The components can be used in commercial as well as 
shareware and freeware and open source projects without cost.
Alcinoe is compatible with <b>Delphi Alexandria 11.2</b>. 
Please "star" (like) this project in GitHub! It's cost 
nothing but help to reference the code

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-24.jpg?raw=true" alt="like" />


Example of app made with Alcinoe
--------------------------------

<a href="https://www.youtube.com/watch?v=IJzEuZTSXDI&vq=hd2160">
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/youtube_kiskis2.jpg?raw=true" alt="KisKis" />
</a>

    
Install Alcinoe
---------------

In Alcinoe 1.0.2 there was a major unit renaming. You can use 
the tool [UnitRenaming](https://github.com/MagicFoundation/Alcinoe/tree/master/Tools/UnitRenaming) 
to automatically rename all old Alcinoe units in your project.

If you plan to use FMX (Firemonkey) framework with Alcinoe 
you will need to patch the original delphi source code. 
To do this you will need to go in [{alcinoe}\Embarcadero\Alexandria\11_2](https://github.com/MagicFoundation/Alcinoe/tree/master/Embarcadero/Alexandria/11_2/)
and run [update.bat](https://github.com/MagicFoundation/Alcinoe/tree/master/Embarcadero/Alexandria/11_2/update.bat) 
to retrieve and patch the original delphi source code. The 
batch file assume that the original source code is located 
in "c:\Program Files (x86)\Embarcadero\Studio\22.0\source\" 
and that you have GIT in your path. Later you will need to 
include in your project search path all subdirectories 
located in [{alcinoe}\Embarcadero\Alexandria\11_2](https://github.com/MagicFoundation/Alcinoe/tree/master/Embarcadero/Alexandria/11_2/).

You will need to run [CompileAll.bat](https://github.com/MagicFoundation/Alcinoe/tree/master/CompileAll.bat)
to download the iOS/Android libraries, to build the Alcinoe Jars, 
to build the BPL, to build all tools located in [{alcinoe}\Tools](https://github.com/MagicFoundation/Alcinoe/tree/master/Tools/)
and finally to build all demos located in [{alcinoe}\Demos](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/).
NOTE: Some demos use [devexpress](https://www.devexpress.com/)

If you don't plan to use any Alcinoe visual components at 
design time, then you don't need to install anything, just 
add [{alcinoe}\Source](https://github.com/MagicFoundation/Alcinoe/tree/master/Source) 
in the search path of your project. 

Else if you want to use visual components at design time then 
you need to install the bpl. Launch Delphi and go in 
component > Install Packages... > and choose the BPL located 
in [{alcinoe}\Libraries\bpl\Alcinoe\Win32\Alexandria\AlcinoeAlexandria.bpl](https://github.com/MagicFoundation/Alcinoe/tree/master/Libraries/bpl/Alcinoe/Win32/Alexandria). 
You also need to add in your search path [{alcinoe}\Source](https://github.com/MagicFoundation/Alcinoe/tree/master/Source) 
and all subdirectories located in [{alcinoe}\Embarcadero\Alexandria\11_2](https://github.com/MagicFoundation/Alcinoe/tree/master/Embarcadero/Alexandria/11_2/).


AndroidMerger: Integrate AAR SDK in FMX Android app
---------------------------------------------------
                                 
<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-26.jpg?raw=true" alt="video player for FireMonkey" width="540" style="width:540px;"/>

An Android library, also called as Android Archive, includes 
everything you need to build an app like source files, 
resource files, manifest etc. This is the reason why AARs are 
different from JARs. AARs can contain resource files as well 
other than compiled byte code. 

Adding such library to Delphi project is long and convoluted 
process consisting of extracting resources from library, 
manually adding them to Delphi deployment files, compiling 
R.Java class, checking dependancies, etc.

With AndroidMerger all of the above can now be done 
automatically in a single command line. In brief 
AndroidMerger will:
 
* Use graddle or internal implementation to list all dependencies.
* Download libraries and dependancies from local or central maven repository.
* Merge the resources of all AARs inside a single directory.
* Merge the AndroidManifest files of all AARs inside AndroidManifest.template.xml.
* Merge google-services.json in the resources of the project.
* Create the R.jar with all resource IDs using aapt or aapt2.
* Update the project file (.dproj) to include all resources.
* Generate the Delphi native bridge file from the Java libraries.

Learn more at [{alcinoe}/Tools/AndroidMerger](https://github.com/MagicFoundation/Alcinoe/tree/master/Tools/AndroidMerger)


DeployMan
---------
                                 
Simplify the deployment of files and folders for iOS and 
Android apps written in Delphi. It is especially useful 
if you need to deploy a lot of files, such as 3rd party SDKs. 
Learn more at [{alcinoe}/Tools/DeployMan](https://github.com/MagicFoundation/Alcinoe/tree/master/Tools/DeployMan)
  

DeployProjNormalizer
--------------------

Create from the dproj a new deployproj file from scratch 
and normalize it (ie: order the node so that you can 
compare different revision with diff compare tools).
Learn more at [{alcinoe}/Tools/DeployProjNormalizer](https://github.com/MagicFoundation/Alcinoe/tree/master/Tools/DeployProjNormalizer)


DProjNormalizer
---------------

Order all nodes in a DProj so that the Dproj stay consistent 
between each commit for easy diff compare. Learn more at [{alcinoe}/Tools/DProjNormalizer](https://github.com/MagicFoundation/Alcinoe/tree/master/Tools/DProjNormalizer)


Opengl video player for FireMonkey
----------------------------------

ALVideoPlayer will render a video to a TEXTURE. This is really 
important because you can fully integrate the video in the Delphi 
form and you can place any controls you want on the top of it 
as it's support Z-ORDER. Official Delphi video player are just 
native video player window on the top of the form and thus not 
supporting Z-ORDER.

Under android Alcinoe use ExoPlayer. ExoPlayer supports features 
like Dynamic adaptive streaming over HTTP (DASH), HLS, 
SmoothStreaming and Common Encryption, which are not supported 
by MediaPlayer. It's designed to be easy to customize and extend. 
Under iOS I use AVPlayer with support also HLS like ExoPlayer does

<p align="left">
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-1.jpg?raw=true" alt="video player for FireMonkey" width="270" style="width:270px;"/>
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-2.jpg?raw=true" alt="video player for FireMonkey" width="270" style="width:270px;"/>
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-3.jpg?raw=true" alt="video player for FireMonkey" width="270" style="width:270px;"/>
</p>

Learn more at [{alcinoe}/Source/Alcinoe.FMX.VideoPlayer.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.FMX.VideoPlayer.pas) and you can also start exploring this feature 
with the demo located at [{alcinoe}/Demos/ALFmxControls](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFmxControls) 
  

WebRTC Delphi wrapper
---------------------

WebRTC (Web Real-Time Communications) is a technology which 
enables Web applications and sites to capture and optionally 
stream audio and/or video media, as well as to exchange arbitrary 
data between browsers and mobile applications without requiring 
an intermediary. The set of standards that comprises WebRTC 
makes it possible to share data and perform teleconferencing 
peer-to-peer, without requiring that the user installs plug-ins 
or any other third-party software.

<p align="center">
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-25.jpg?raw=true" alt="Delphi WebRTC wrapper" />
</p>

TALWebRTC component makes it easy to add video and audio chat 
into your applications, which opens up a whole new world of 
interactivity. Learn more at [{alcinoe}/Demos/ALLiveVideoChat](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALLiveVideoChat)
  

Firemonkey native iOS/Android TEdit and TMemo
---------------------------------------------

The spirit is to mix FireMonkey control with native platform control
when the functionality on such control starts to be very hard to
implement (like webbrowser, edit, memo, datepicker, etc.). But it's
not to make several distinct forms for several platforms like 
offer for example http://www.turbococoa.com/ (but this option is
also a good alternative in some way, it's up to you to decide)

<p align="left">
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-20.png?raw=true" alt="Drawing" width="240" style="width:240px;"/>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-21.png?raw=true" alt="Drawing" width="240" style="width:240px;"/>
</p>

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-22.png?raw=true" alt="Drawing" width="457" style="width:457px;"/>

In Delphi (berlin) their is already IOS platform control that was 
quite well implemented but close to none android platform control 
and so I start to build native android/iOS controls like TEdit/TMemo. 
These control work mostly like some windows that are placed on The
top of the form (so off course no z-order with FireMonkey control). 
Learn more at [{alcinoe}/Demos/ALFmxControls](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFmxControls)


Fast/double-buffered FireMonkey controls with native draw
---------------------------------------------------------

* Rectangle
* Circle
* Text (Can also draw html text on iOS/android/win/macOS)
* Glyph
* etc.

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-23.png?raw=true" alt="TALRectangle" width="600" style="width:600px;" />

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-8.jpg?raw=true" alt="TALRectangle" width="600" style="width:600px;" />

#### The fact ####

Painting of FireMonkey controls can be sometimes slow, or say 
differently, not sufficiently fast for a fluid scrolling. For example if 
you simply look the basic TRectangle with round corners, the paint procedure 
can take around 3ms! So if you have around 20 visible TRectangles on your 
screen, then it's will cost you around 60ms to repaint the full screen 
(and normally you don't have only TRectangle, you also have TLabel, 
TCheckbox, etc..). After it's just math, take 100ms to repaint the screen, 
so you can only do around 10 frames per seconds (in reality you will have 
much less even) so the scrolling can't be fluid :(

#### The solution #### 
 
I didn't want to rebuild the FireMonkey controls, it's a too 
huge job for me, and instead I try to find an intermediate solution. 
This what I find by adding "double-buffered" property to the FireMonkey 
controls. So instead to repaint and repaint (and repaint) the controls 
for every single pixels move of the scrollbox, I first paint the control 
on a "buffer" that I store directly in the GPU memory (through TTexture), 
and when the system asks me to repaint the controls instead of calling 
again the paint algorithm I simply redraw the buffer TTexture.

#### The results ####  

As I say before it's took 3ms just to paint a simple 
TRectangle with round corners. With my double-buffered property 
it's take now around 0.1ms! So now the scroll looks much more 
fluid! 

#### OpenGL draw => Replaced by native iOS/android draw ####

Most of the basic shape (like TRectangle, TCircle, etc.) use openGL to 
draw. it's not very efficient, for example to draw a circle under 
openGL you will in fact draw 50 triangles. This result often in poor 
quality : https://quality.embarcadero.com/browse/RSP-15206
For roundrect it's even worse because you must first calculate the 
path and then later draw it (much more slow than TCircle)

Other problem is all of these draw depend of Form.quality. if you set 
form.quality to highquality then everything you will do on the
canvas will be multisample like drawing an image for example and that 
could be problematic because the image will be anti-aliased. if you set 
form.quality to highperformance then the draw will be very rough (no 
anti aliasing). 

To resolve this, I build the buffer of my control using NATIVE 
ANDROID/IOS API. In this way we will have a high quality draw at also
a high speed without being dependent of the form.quality

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-9.jpg?raw=true" alt="TALCircle" />
  
You can start exploring this feature 
with the demo located at [{alcinoe}/Demos/ALFmxControls](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFmxControls)


Improved FireMonkey controls
----------------------------

* ScrollBox
* TabControl
* RangeTrackBar

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-5.jpg?raw=true" alt="RangeTrackBar" width="600" style="width:600px;"  />

<p align="left">
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-15.jpg?raw=true" alt="video player for FireMonkey" width="250" style="width:250px;"/>
  &nbsp;&nbsp;&nbsp;
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-14.jpg?raw=true" alt="video player for FireMonkey" width="250" style="width:250px;"/>
  &nbsp;&nbsp;&nbsp;
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-16.jpg?raw=true" alt="video player for FireMonkey" width="250" style="width:250px;"/>
</p>

Learn more at [{alcinoe}/Demos/ALFmxControls](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFmxControls)
  

Confetti Falling Animation
--------------------------

ALConfetti is a vanilla Delphi library for creating a configurable, 
high-performance confetti falling animation. Learn more at [{alcinoe}/Demos/ALConfetti](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALConfetti)

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/confetti.gif?raw=true" alt="confetti" width="564" style="width:564px;" />


Firebase cloud messaging
------------------------

Delphi implementation of the latest Firebase cloud messaging (FCM) 
with HTTP V1 protocol that will permit you to send alert notification 
with image in Android and iOS. Learn more at [{alcinoe}\Demos\ALFirebaseMessaging](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFirebaseMessaging)

  
Google OAuth 2.0 Access Token
-----------------------------

Google APIs use the OAuth 2.0 protocol for authentication and 
authorization. You can use the function ALGenerateGoogleOAuth2AccessToken 
to generate an OAuth 2.0 Access Token. Learn more at 
[{alcinoe}/Source/Alcinoe.Cipher.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.Cipher.pas) 
  

Android/iOS VKontakte/Facebook SDK login
----------------------------------------

The VKontakte/Facebook SDK for Android enables people to sign into 
your app with VKontakte/Facebook Login. When people log into your 
app with VKontakte/Facebook they can grant permissions to your app so 
you can retrieve information or perform actions on VKontakte/Facebook 
on their behalf. Learn more at [{alcinoe}\Demos\ALFacebookLogin](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFacebookLogin)


Photo Editor Filters for Android/iOS
------------------------------------

With TALColorAdjustEffect, gorgeous photo filters with one-tap auto 
enhance, make your photos beautiful and expressive in just minutes! 
Learn more at [{alcinoe}\Demos\ALFmxFilterEffects](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFmxFilterEffects)

<p align="left">
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-18.jpg?raw=true" alt="video player for FireMonkey" width="320" style="width:320px;"/>
  &nbsp;&nbsp;&nbsp;
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-19.jpg?raw=true" alt="video player for FireMonkey" width="320" style="width:320px;"/>
</p>


Json Parser
-----------

TALJsonDocument is a Delphi parser/writer for JSON / BSON data
format. it's support DOM and SAX parser (Note a better name could 
be SAJ for Simple API for JSON instead of SAX for Simple API for XML 
but as the concept of SAX is well know I keep this name), support 
BSON format, and use a similar syntax than TALXMLDocument / TXMLDocument.
TALJsonDocument can also export Json / Bson data in TALStringList.

Example :

```
    {
      _id: 1, // comments
      name: { first: "John", last: "Backus" },
      birth: new Date('1999-10-21T21:04:54.234Z'),
      contribs: [ "Fortran", "ALGOL", "Backus-Naur Form", "FP" ],
      awards: [
                { award: "National Medal of Science",
                  year: 1975,
                  by: "National Science Foundation" },
                { award: "Turing Award",
                  year: 1977,
                  by: "ACM" }
              ],
      spouse: "",
      address: {},
      phones: []
    }
```

To access the document nodes :

```
    MyJsonDoc.GetChildNodeValueInt32('_id', 0{default if node not exists});
    MyJsonDoc.GetChildNodeValueText(['name','first'], ''{default if node not exists});
    MyJsonDoc.GetChildNodeValueDateTime('birth', Now{default if node not exists});
```

Learn more at [{alcinoe}/Source/Alcinoe.JSONDoc.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.JSONDoc.pas)
  

ImageMagick wrapper for Delphi
------------------------------

Use ImageMagick® to create, edit, compose, or convert bitmap 
images. It can read and write images in a variety of formats 
(over 200) including PNG, JPEG, GIF, HEIC, TIFF, DPX, EXR, 
WebP, Postscript, PDF, and SVG. Use ImageMagick to resize, 
flip, mirror, rotate, distort, shear and transform images, 
adjust image colors, apply various special effects, or draw 
text, lines, polygons, ellipses and Bézier curves.

Example :

```
    var aWand: PMagickWand;
    begin
    
      //Create the ImageMagick Library
      alCreateImageMagickLibrary({alcinoe} + '\Libraries\dll\imagemagick\win32\imagemagick', min(2, System.CPUCount){aThreadLimit});
      try
      
        //create the wand pointer
        aWand := ALImageMagickLib.NewMagickWand;
        try
      
          //load the image
          if ALImageMagickLib.MagickReadImage(aWand, pansiChar(aInputFilename)) <> MagickTrue then RaiseLastMagickWandError(aWand);
          
          //Set the compression quality
          if ALImageMagickLib.MagickSetImageCompressionQuality(aWand,80) <> MagickTrue then RaiseLastMagickWandError(aWand);
      
          //autorate the image
          if ALImageMagickLib.MagickAutoOrientImage(aWand) <> MagickTrue then RaiseLastMagickWandError(aWand);
      
          //Resize the image using the Lanczos filter
          if ALImageMagickLib.MagickResizeImage(aWand, 640, 480, LanczosFilter) <> MagickTrue then RaiseLastMagickWandError(aWand);
             
          //save the image
          ALImageMagickLib.MagickWriteImage(aWand, pansiChar(aOutputFilename));
      
        finally
          ALImageMagickLib.DestroyMagickWand(aWand);
        end;
    
      finally
        alFreeImageMagickLibrary;
      end;

    end;
```

Learn more at [{alcinoe}/Source/Alcinoe.ImageMagick.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.ImageMagick.pas)

MongoDb client
--------------

A Delphi driver (with connection pool) to access a
mongoDB server. a connection pool is a cache of database
connections maintained so that the connections can be reused
when future requests to the database are required.
In connection pooling, after a connection is created,
it is placed in the pool and it is used over again so that a
new connection does not have to be established. If all the
connections are being used, a new connection is made and is
added to the pool. Connection pooling also cuts down on the
amount of time a user must wait to establish a connection
to the database. Learn more at [{alcinoe}/Source/Alcinoe.MongoDB.Client.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.MongoDB.Client.pas)


WebSocket client
----------------

WebSocket Client for Delphi implemented on the top of WinHTTP.
The WebSocket is a communication protocol, that makes it possible 
to open a two-way interactive communication session between the 
user’s browser and a server. With this, you can send messages to 
a server and receive event-driven responses without having to 
poll the server for a reply. Learn more at [{alcinoe}\Demos\ALWinHTTPWebSocketClient](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALWinHTTPWebSocketClient)


Fast TStringList
----------------

TALStringList Work the same as Delphi TStringList except that it's
allow to search a name=value using a quicksort algorithm when the
list is sorted. Also TALStringList use a locale independent
algorithm (based on the 8-bit ordinal value of each character)
instead of the AnsiCompareText and AnsiCompareStr used by the
Delphi TStringList. at the end the sort in TALStringList is up to
10x more faster than in Delphi TStringList. Also TALStringList is
not an Unicode TStringList but an 100% Ansi StringList. You can 
start exploring this feature with the demo located at 
[{alcinoe}\Demos\ALSortedListBenchmark](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALSortedListBenchmark)


PHP runner
----------

ALPHPRunnerEngine is a simple but useful component for
easily use php (any version) as a scripting language
in Delphi applications. ALPhpRunnerEngine allows to
execute the PHP scripts within the Delphi program without
a WebServer. ALPHPRunnerEngine use the CGI/FastCGI
interface (php-cgi.exe) of PHP to communicate with PHP engine.
Learn more at [{alcinoe}\Demos\ALPhpRunner](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALPhpRunner)
  
Memcached Client
----------------

What is Memcached? Free & open source, high-performance,
distributed memory object caching system, generic in
nature, but intended for use in speeding up dynamic web
applications by alleviating database load. Learn more at 
[{alcinoe}/Source/Alcinoe.MemCached.Client.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.MemCached.Client.pas)


GSM component
-------------

The TAlGSMComm component implements SMS text messaging
through the text-mode interface defined in the GSM
Technical Specification 07.05, version 5.1.0, dated
December 1996.  There are several variations of this spec,
used throughout Nokia, Siemens, Ericsson, etc models.
We have tested the Nokia 6230 in-house, but the Nokia 7190,
8890, 6210 and 9110 models should work as well.  Phones
from other manufacturers will also work, as long as they
implement the text-mode interface. Learn more at 
[{alcinoe}/Source/Alcinoe.GSMComm.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.GSMComm.pas)


SQLite3 Client
--------------

Query Sqlite3 database and get the result In Xml format 
or in Json/Bson format. Learn more at 
[{alcinoe}/Source/Alcinoe.Sqlite3.Client.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.Sqlite3.Client.pas)


And much more
-------------

* CGI runner 
* Http Client (WinInet/WinHTTP)
* MySQL Client 
* NNTP Client
* POP3 Client
* SMTP Client
* Xml Parser 
* etc ...


WIN64
-----

Unfortunately, in win64 we lost all the FastCode heritage. 
(that was mostly based on ASM). That mean that most of 
the functions will be around 2x to 10x more slower. 
you can try to launch /demo/ALStringBenchMark/
in win64 and Win32 to see the difference in speed.


DELPHI D2009+ (UNICODE)
-----------------------

No mistake, Unicode was necessary for a product like Delphi. 
But the way embarcadero decide to implement it’s questionable! 
Indeed they decide to migrate the 8bit string to 16bit string 
instead of choosing to implement Unicode through 8bit string 
(UTF8).  This cause the migration of some Delphi app < d2009 
very hard, especially on application that assume that string 
is 8bit. Here a very good article about why to avoid to use 
UFT-16: [utf8everywhere.org](http://www.utf8everywhere.org/)

So I was facing the need to choose: stay in D2007, move to 
FreePascal (that implement Unicode through UTF8) or migrate 
to Unicode. After studying the Unicode migration I discover 
that it’s will be very hard to do on my own projects. 
Also on lot of my applications the migration to Unicode was 
really unnecessary, as they was already working in UTF8, 
receive their input request in utf8 and output their response 
in UTF8. Here the migration to UTF16 will mean: 
Input (UTF8) => UTF16 => data processing => UTF16 => output(UTF8)
plus off course all the migration job (that include debugging).
In fact, except the input/output to the "visual interface", 
most (if not all) of the input/output of most of the 
application will be done in 8bit string (ex: file storage, 
client/server protocol, HTTP, Smtp, tcp, xml, html, database, 
etc.). So I decide to go in Delphi Xe2 but to stay in 
AnsiString (8bit string). My first through was to replace 
everywhere in my code all the String by AnsiString. 
Unfortunately,  most of the usefull and needed string function 
in D2009+ are not available in 8bit string like inttostr, 
strtoint, strtofloat, Tstrings, TstringStream, etc. Of course 
we can still do ansistring(inttostr(x)) but this is very 
pity (and slow). So I decided to make my component 
like a "framework" to help fully support of 8bit string 
(mostly in UTF8). 

You can also go in /demo/ALStringBenchMark/ to find an 
application to benchmark ansiString vs Unicode String and 
in /demo/ALSortedList/ to see the benchmark of TALStringList 
(ansiString) vs TStringList (UnicodeString). In fact, most 
of the Unicode function (when they are implemented correctly, 
thanks to fastcode) will be close to the same speed as their 
ansi equivalent. But no mistake, Unicode string will still 
use 2x more memory and depending on the application, this may 
be not acceptable.

Under D2009+, ansiString Have now a codepage, and some 
transliteration (OldCodePage => UTF16 => NewCodePage) will 
happen when assigning one ansiString with different codepage 
to another ansistring with another codepage. To avoid this 
it’s important to always set project option to the code page 
you want (eg. 65001 for UTF8) and also to call at the 
beginning of the program SetMultiByteConversionCodePage(CP_UTF8); 
Also it’s very important to avoid using 2 different
string types (eg UTF8string and aniString) even if they have 
the same codepage, because compiler at compile time 
don’t know that codepage is the same and will do a 
transliteration (ex MyAnsiStringUTF8 := MyUTF8String will 
result in UTF8 => UTF16 => UTF8). This is why we use in 
all our code only AnsiString instead of UTF8String (even 
when we assume that string contain only UTF8 char) to 
avoid theses transliteration keep the rule to only use 
AnsiString with SetMultiByteConversionCodePage.