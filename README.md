Alcinoe
=======

Alcinoe is a library of visual and non-visual components for Delphi. The components can 
be used in commercial as well as shareware and freeware and open source projects 
without cost.

Alcinoe is compatible with <b>Delphi Sydney 10.4.2</b>

Please "star" (like) this project in GitHub! It's cost nothing but help
to reference the code

<img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-24.jpg?raw=true" alt="like" />


Example of an app made with Alcinoe: KisKis
-------------------------------------------

* YouTube: https://www.youtube.com/watch?v=IJzEuZTSXDI&vq=hd2160
* iOS: https://itunes.apple.com/us/app/id1420049759
* Android: https://play.google.com/store/apps/details?id=club.kiskis.app&hl=en


Compiled demo
-------------

#### Win32 #### 

* https://svn.code.sf.net/p/alcinoe/code/demos/ALWinHTTPClient/win32/ALWinHTTPClientDemo.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALWinInetHTTPClient/win32/ALWinInetHTTPClientDemo.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALWinHTTPWebSocketClient/win32/ALWinHTTPWebSocketClientDemo.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALSortedListBenchmark/win32/ALSortedListBenchmark.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALCipher/win32/ALCipherDemo.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALJsonDoc/win32/AljsonDocDemo.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALNNTPClient/win32/ALNNTPClientDemo.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALPhpRunner/win32/ALPhpRunnerDemo.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALPOP3Client/win32/POP3ClientDemo.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALSMTPClient/win32/ALSMTPClientDemo.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALSqlite3Client/win32/ALSqlite3clientDemo.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALStressHTTPServer/win32/ALStressHTTPServer.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALStringBenchmark/win32/ALStringBenchmark.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALWebSpider/win32/ALWebSpiderDemo.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALXmlDoc/win32/AlXmlDocDemo.exe
* https://svn.code.sf.net/p/alcinoe/code/demos/ALLiveVideoChat/server/Win32/Release/ALLiveVideoChatServer.exe

#### Android #### 

* https://tinyurl.com/zoxavr9 (ALFmxControl.apk)
* https://tinyurl.com/na5fvy2d (ALFirebaseMessaging.apk)
* https://tinyurl.com/y9gzos9f (ALFacebookLogin.apk)
* https://tinyurl.com/mzf8adyk (ALFmxFilterEffects.apk)
* https://tinyurl.com/ydymo23w (ALLiveVideoChatClient.apk)

#### iOS #### 

Please ask apple how to do


Install Alcinoe
---------------

If you don't plan to use any Alcinoe visual components at design time, then
you don't need to install anything, just add <a href="https://github.com/Zeus64/alcinoe/tree/master/source">{alcinoe_rootdir}\source</a> in the 
search path of your project. 

If you plan to use visual components at design time then you need to
install the bpl. Launch Delphi and go in component > Install Packages... > and choose the 
BPL located in <a href="https://github.com/Zeus64/alcinoe/tree/master/lib/bpl/alcinoe/Win32/sydney">{alcinoe_rootdir}\lib\bpl\alcinoe\Win32\sydney\Alcinoe_sydney.bpl</a> (if you are in delphi Sydney 
else choose the directory that correspond to your Delphi version). 
You still need to add in your search path <a href="https://github.com/Zeus64/alcinoe/tree/master/source">{alcinoe_rootdir}\source</a>  
  
Alcinoe also improuve a little the original delphi source code. In this way you will need
to go in <a href="https://github.com/Zeus64/alcinoe/tree/master/embarcadero/sydney/10_4_2/">{alcinoe_rootdir}\embarcadero\sydney\10_4_2</a>
and run update.bat to retrieve and patch the original delphi source code. The batch file
assume that the original source code is located in "c:\Program Files (x86)\Embarcadero\Studio\21.0\source\" 
and that you have GIT in your path. After the batch copied and patched the Delphi source code you will 
need to include this source code in your project search path.

To finish more and more libraries use now Java 1.8 features (like webRTC, exoplayer, etc.). 
In this way, we must do desugaring (Desugaring allows you to use these features on older 
devices by replacing new bytecodes and language APIs with older ones during the build process). 
With d8.bat (replacement of dx.bat), desugaring is turned on by default. So you can now 
use most of the latest language changes while targeting older devices.
Waiting that embarcadero swap to D8.bat, we must make DX.bat a "proxy" to D8.bat.
For this just replace your original DX.bat (mine is located in c:\SDKs\android\build-tools\30.0.3\d8.bat)
by the one located in <a href="https://github.com/Zeus64/alcinoe/tree/master/tools/D8Proxy/dx.bat">{alcinoe_rootdir}\tools\D8Proxy\dx.bat</a>.
more details here: <a href="https://quality.embarcadero.com/browse/RSP-24155">https://quality.embarcadero.com/browse/RSP-24155</a>

Full opengl video player for FireMonkey
---------------------------------------

ALVideoPlayer will render a video to a TEXTURE. This is really important 
because you can fully integrate the video in the Delphi form and you can 
place any controls you want on the top of it as it's support Z-ORDER. 
Official Delphi video player are just native video player window on the top 
of the form and thus not supporting Z-ORDER.

Under android I use ExoPlayer. ExoPlayer supports features 
like Dynamic adaptive streaming over HTTP (DASH), HLS, SmoothStreaming and 
Common Encryption, which are not supported by MediaPlayer. It's designed 
to be easy to customize and extend. Under iOS I use AVPlayer with support 
also HLS like ExoPlayer does

<p align="left">
  <img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-1.jpg?raw=true" alt="video player for FireMonkey" width="270" style="width:270px;"/>
  <img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-2.jpg?raw=true" alt="video player for FireMonkey" width="270" style="width:270px;"/>
  <img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-3.jpg?raw=true" alt="video player for FireMonkey" width="270" style="width:270px;"/>
</p>


WebRTC Delphi wrapper
---------------------

WebRTC (Web Real-Time Communications) is a technology which enables Web applications and 
sites to capture and optionally stream audio and/or video media, as well as to exchange 
arbitrary data between browsers and mobile applications without requiring an intermediary. 
The set of standards that comprises WebRTC makes it possible to share data and perform 
teleconferencing peer-to-peer, without requiring that the user installs plug-ins or any other 
third-party software.

TALWebRTC component makes it easy to add video and audio chat into 
your applications, which opens up a whole new world of interactivity

<p align="center">
  <img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-25.jpg?raw=true" alt="Delphi WebRTC wrapper" />
</p>


Firemonkey native iOS/Android TEdit and TMemo
---------------------------------------------

The spirit is to mix FireMonkey control with native platform control
when the functionality on such control starts to be very hard to
implement (like webbrowser, edit, memo, datepicker, etc.). But it's
not to make several distinct forms for several platforms like 
offer for example http://www.turbococoa.com/ (but this option is
also a good alternative in some way, it's up to you to decide)

In Delphi (berlin) their is already IOS platform control that was 
quite well implemented but close to none android platform control 
and so I start to build native android/iOS controls like TEdit/TMemo. 
These control work mostly like some windows that are placed on The
top of the form (so off course no z-order with FireMonkey control)


<p align="left">
  <img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-20.png?raw=true" alt="Drawing" width="240" style="width:240px;"/>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-21.png?raw=true" alt="Drawing" width="240" style="width:240px;"/>
</p>

<img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-22.png?raw=true" alt="Drawing" width="457" style="width:457px;"/>



Fast/double-buffered FireMonkey controls with native draw
---------------------------------------------------------

* Rectangle
* Circle
* Text (Can also draw html text on iOS/android/win/macOS)
* Glyph
* etc.

<img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-23.png?raw=true" alt="TALRectangle" width="600" style="width:600px;" />

<img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-8.jpg?raw=true" alt="TALRectangle" width="600" style="width:600px;" />

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

<img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-9.jpg?raw=true" alt="TALCircle" />



Improved FireMonkey controls
----------------------------

* ScrollBox
* TabControl
* RangeTrackBar

<img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-5.jpg?raw=true" alt="RangeTrackBar" width="600" style="width:600px;"  />

<p align="left">
  <img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-15.jpg?raw=true" alt="video player for FireMonkey" width="280" style="width:280px;"/>
  &nbsp;&nbsp;&nbsp;
  <img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-14.jpg?raw=true" alt="video player for FireMonkey" width="280" style="width:280px;"/>
  &nbsp;&nbsp;&nbsp;
  <img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-16.jpg?raw=true" alt="video player for FireMonkey" width="280" style="width:280px;"/>
</p>



Firebase cloud messaging
------------------------

A cross-platform method of using Firebase Cloud Messaging (FCM) to receive push notifications
Using FCM, you can notify a client app that new email or other data is available to sync. 
You can send notification messages to drive user re-engagement and retention. For use cases 
such as instant messaging, a message can transfer a payload of up to 4KB to a client app.


Android/iOS VKontakte/Facebook SDK login
------------------------------

The VKontakte/Facebook SDK for Android enables people to sign into your app with VKontakte/Facebook Login. 
When people log into your app with VKontakte/Facebook they can grant permissions to your app so 
you can retrieve information or perform actions on VKontakte/Facebook on their behalf.


Photo Editor Filters for Android/iOS
------------------------------------

With TALColorAdjustEffect, gorgeous photo filters with one-tap auto 
enhance, make your photos beautiful and expressive in just minutes! 

<p align="left">
  <img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-18.jpg?raw=true" alt="video player for FireMonkey" width="320" style="width:320px;"/>
  &nbsp;&nbsp;&nbsp;
  <img src="https://github.com/Zeus64/alcinoe/blob/master/references/DocImages/img-19.jpg?raw=true" alt="video player for FireMonkey" width="320" style="width:320px;"/>
</p>



Json Parser
-----------

TALJsonDocument is a Delphi parser/writer for JSON / BSON data
format. it's support DOM and SAX parser (Note a better name could 
be SAJ for Simple API for JSON instead of SAX for Simple API for XML 
but as the concept of SAX is well know I keep this name), support 
BSON format, and use a similar syntax than TALXMLDocument / TXMLDocument.
TALJsonDocument can also export Json / Bson data in TALStringList.

When it deals with parsing some (textual) content, two directions
are usually envisaged. In the JSON world, you have usually to
make a choice between:
* A DOM parser, which creates an in-memory tree structure of
  objects mapping the JSON content;
* A SAX parser, which reads the JSON content, then call pre-defined
  events for each JSON content element. 

In fact, DOM parsers use internally a SAX parser to read the JSON
content. Therefore, with the overhead of object creation and
their property initialization, DOM parsers are typically three
to five times slower than SAX (and use much much more memory to
store all the nodes). But, DOM parsers are much more powerful for
handling the data: as soon as it's mapped in native objects,
code can access with no time to any given node, whereas a
SAX-based access will have to read again the whole JSON content.

Most JSON parser available in Delphi uses a DOM-like approach.
For instance, the DBXJSON unit included since Delphi 2010
or the SuperObject library create a class instance mapping
each JSON node. In order to achieve best speed, TALJsonDocument
implements DOM parser and also a SAX parser.

TALJsonDocument can also support comments inside the JSON source
that is an extension to the JSON specifications

TALJsonDocument syntax is very similar 
to TALXMLdocument / TXMLDocument

TALJsonDocument is available in 2 variants: TALJsonDocument that 
is made on the top of ansiString (so UTF-8) and TALJsonDocumentU
that is made on the top of unicode string (so UTF-16)

Example :

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

To access the document nodes :

    MyJsonDoc.loadFromJson(AJsonStr, False);
    MyJsonDoc.ParseOptions := [poAllowComments]; // to allow comments inside the JSON source
    MyJsonDoc.childnodes['_id'].int32;
    MyJsonDoc.childnodes['name'].childnodes['first'].text;
    MyJsonDoc.childnodes['name'].childnodes['last'].text;
    MyJsonDoc.childnodes['birth'].datetime;
    for i := 0 to MyJsonDoc.childnodes['contribs'].ChildNodes.count - 1 do
      MyJsonDoc.childnodes['contribs'].childnodes[i].text;
    for i := 0 to MyJsonDoc.childnodes['awards'].ChildNodes.count - 1 do begin
      MyJsonDoc.childnodes['awards'].childnodes[i].childnodes['award'].text;
      MyJsonDoc.childnodes['awards'].childnodes[i].childnodes['year'].text;
      MyJsonDoc.childnodes['awards'].childnodes[i].childnodes['by'].text;
    end;

Or if you are not sure of the existence of the nodes before to access 
them or you don't want to check it you can also do :

    MyJsonDoc.GetChildNodeValueInt32('_id', 0{default if node not exists});
    MyJsonDoc.GetChildNodeValueText(['name','first'], ''{default if node not exists});
    MyJsonDoc.GetChildNodeValueDateTime('birth', Now{default if node not exists});

To create the document nodes :

    MyJsonDoc.addchild('_id').int32 := 1;
    with MyJsonDoc.addchild('name', ntObject) do begin
      addchild('first').text := 'John';
      addchild('last').text := 'Backus';
    end;
    MyJsonDoc.addchild('birth').dateTime := Now;
    with MyJsonDoc.addchild('contribs', ntArray) do begin
      addchild.text := 'Fortran';
      addchild.text := 'ALGOL';
      addchild.text := 'Backus-Naur Form';
      addchild.text := 'FP';
    end;
    with MyJsonDoc.addchild('awards', ntArray) do begin
      with addchild(ntObject) do begin
        addchild('award').text := 'National Medal of Science';
        addchild('year').int32 := 1975;
        addchild('by').text := 'National Science Foundation';
      end;
      with addchild(ntObject) do begin
        addchild('award').text := 'Turing Award';
        addchild('year').int32 := 1977;
        addchild('by').text := 'ACM';
      end;
    end;
    MyJsonDoc.addchild('spouse');
    MyJsonDoc.addchild('address', ntObject);
    MyJsonDoc.addchild('phones', ntArray);

You can also create/update nodes like this :

    MyJsonDoc.SetChildNodeValueInt32('_id', 0);
    MyJsonDoc.SetChildNodeValueText(['name','first'], 'John');
    MyJsonDoc.SetChildNodeValueDateTime('birth', Now);

To load and save from BSON :

    MyJsonDoc.LoadFromFile(aBSONFileName, False{saxMode}, True{BSON});
    MyJsonDoc.SaveToFile(aBSONFileName, False{saxMode}, True{BSON});

To parse an JSON document in Sax Mode :

    MyJsonDoc.onParseText := procedure (Sender: TObject;
                                        const Path: AnsiString;
                                        const name: AnsiString;
                                        const Args: array of const;
                                        NodeSubType: TALJSONNodeSubType)
      begin
        case NodeSubType of
          nstFloat: Writeln(Path + '=' + ALFloatToStr(Args[0].VExtended^, ALDefaultFormatSettings));
          nstText: Writeln(Path + '=' + ansiString(Args[0].VAnsiString));
          nstObjectID: Writeln(Path + '=' + 'ObjectId("'+ALBinToHex(ansiString(Args[0].VAnsiString))+'")');
          nstBoolean: Writeln(Path + '=' + ALBoolToStr(Args[0].VBoolean,'true','false'));
          nstDateTime: Writeln(Path + '=' + ALFormatDateTime('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettings));
          nstNull: Writeln(Path + '=' + 'null');
          nstRegEx: Writeln(Path + '=' + ansiString(Args[0].VAnsiString));
          nstBinary: Writeln(Path + '=' + 'BinData('+inttostr(Args[1].VInteger)+', "'+ansiString(ALBase64EncodeStringNoCRLF(ansiString(Args[0].VAnsiString)))+'")');
          nstJavascript: Writeln(Path + '=' + ansiString(Args[0].VAnsiString));
          nstInt32: Writeln(Path + '=' + 'NumberInt('+inttostr(Args[0].VInteger)+')');
          nstTimestamp: Writeln(Path + '=' + 'Timestamp('+inttostr(int64(cardinal(Args[0].VInteger)))+', '+inttostr(int64(cardinal(Args[1].VInteger)))+')');
          nstInt64: Writeln(Path + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
        end;
      end;
    MyJsonDoc.LoadFromJSON(AJsonStr, true{saxMode});


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

    var aWand: PMagickWand;
    begin
    
      //Create the ImageMagick Library
      alCreateImageMagickLibrary({alcinoe_rootdir} + '\lib\dll\imagemagick\win32\imagemagick', min(2, System.CPUCount){aThreadLimit});
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


MongoDb client
--------------

Delphi Client for MongoDB database.
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
to the database.

Example :

    aJSONDoc := TALJSONDocument.create;
    aMongoDBClient := TAlMongoDBClient.create;
    try
      aMongoDBClient.Connect('', 0);
      aMongoDBClient.SelectData('test.exemple',
                                '{fieldA:123}', // the query
                                '{fieldA:1, fieldB:1}', // the return fields selector
                                aJSONDoc.node);
      aMongoDBClient.disconnect;
      for i := 0 to aJSONDoc.node.childnodes.count - 1 do
        with aJSONDoc.node.childnodes[i] do
          writeln(aJSONDoc.node.childnodes[i].nodename + '=' + aJSONDoc.node.childnodes[i].text)
    finally
      aMongoDBClient.free;
      aJSONDoc.free;
    end;

Example with connection pool :

    aMongoDBConnectionPoolClient := TAlMongoDBConnectionPoolClient.create(aDBHost, aDBPort);
    try
    
      ::Thread1::
      aMongoDBConnectionPoolClient.SelectData('test.example',
                                              '{fieldA:123}', // the query
                                              '{fieldA:1, fieldB:1}', // the return fields selector
                                              aLocalVarJSONDOC.node);
    
      ::Thread2::
      aMongoDBConnectionPoolClient.SelectData('test.example',
                                              '{fieldA:999}', // the query
                                              '{fieldA:1, fieldB:1}', // the return fields selector
                                              aLocalVarJSONDOC.node);
    
    finally
      aMongoDBClient.free;
    end;

Example tail monitoring :

    aMongoDBTailMonitoringThread := TAlMongoDBTailMonitoringThread.Create(
                                      aDBHost,
                                      aDBPort,
                                      'test.cappedCollectionExemple'
                                      '{}', // the query
                                      '{fieldA:1, fieldB:1}', // the return fields selector
  
                                      Procedure (Sender: TObject; JSONRowData: TALJSONNode)
                                      begin
                                        writeln('New item added in cappedCollectionExemple: ' + JSONRowData.childnodes['fieldA'].text);
                                      end,
  
                                      procedure (Sender: TObject; Error: Exception)
                                      begin
                                        writeln(Error.message);
                                      end);
    ....
    aMongoDBTailMonitoringThread.free;


WebSocket client
----------------

WebSocket Client for Delphi implemented on the top of WinHTTP.
The WebSocket is a communication protocol, that makes it possible 
to open a two-way interactive communication session between the 
user’s browser and a server. With this, you can send messages to 
a server and receive event-driven responses without having to 
poll the server for a reply.


Fast TStringList
----------------

TALStringList Work the same as Delphi TStringList except that it's
allow to search a name=value using a quicksort algorithm when the
list is sorted. Also TALStringList use a locale independent
algorithm (based on the 8-bit ordinal value of each character)
instead of the AnsiCompareText and AnsiCompareStr used by the
Delphi TStringList. at the end the sort in TALStringList is up to
10x more faster than in Delphi TStringList. Also TALStringList is
not an Unicode TStringList but an 100% Ansi StringList

TALNVStringList (NV for NameValue) is same as TALStringList (use
also a quicksort algorithm) except that here optimization is
oriented for name/value list instead of string list.

TALHashedStringList
TALHashedStringList is same as TALStringList except that it's use
an internal hash table instead of a quicksort algorithm. By using
TALHashedStringList instead of TALStringList, you can improve
performance when the list contains a large number of strings
(else if you list don't contain a lot of strings the performance
is lower than TALStringList because of the cost to calculate the
hash)


PHP runner
----------

ALPHPRunnerEngine is a simple but useful component for
easily use php (any version) as a scripting language
in Delphi applications. ALPhpRunnerEngine allows to
execute the PHP scripts within the Delphi program without
a WebServer. ALPHPRunnerEngine use the CGI/FastCGI
interface (php-cgi.exe) of PHP to communicate with PHP engine.


Memcached Client
----------------

Delphi Client for memcached database.

What is Memcached?  Free & open source, high-performance,
distributed memory object caching system, generic in
nature, but intended for use in speeding up dynamic web
applications by alleviating database load.

Memcached is an in-memory key-value store for small chunks
of arbitrary data (strings, objects) from results of
database calls, API calls, or page rendering.

Memcached is simple yet powerful. Its simple design promotes
quick deployment, ease of development, and solves many
problems facing large data caches.


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
implement the text-mode interface.  About 1/4 of the
current phones are capable of being connected to a PC
(through IR or serial cable), about 1/3 of those are
text-mode only, 1/3 are PDU mode only, and the other 1/3
support both text and PDU mode.  Some phones (such as the
Nokia 5190) support SMS, but they use a proprietary protocol,
which TALGSMComm does not support.

To test your phone, connect the phone to your PC through
the serial cable or IR device (consult your phone's documentation
for details on how to connect). Enter "AT"<CR> into a terminal
window to verify the connection is established (you should receive
"OK" from the phone), then enter "AT+CMGF=?"<CR>. The response
should contain a "1", indicating that it supports text-mode.
If both of these tests pass, then your phone meets the basic
requirements.


SQLite3 Client
--------------

Query Sqlite3 database and get the result In Xml format 
or in Json/Bson format. 


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


Where can I get it?
-------------------

#### SVN ####

you can access the last svn version at:
svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code 
or with a web browser at:
http://alcinoe.svn.sourceforge.net/viewvc/alcinoe/

#### GIT ####

You can also find the last version on github at 
https://github.com/Zeus64/alcinoe


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
UFT-16: http://www.utf8everywhere.org/

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