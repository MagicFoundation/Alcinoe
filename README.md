# Alcinoe #

Alcinoe is a library visual and non-visual components for Delphi. The components can 
be used in commercial as well as shareware and freeware and open source projects 
without cost.


## Full opengl video player for firemonkey ##

ALVideoPlayer will render a video on a TEXTURE. this is really important because you can fully integrate the video in
the delphi form and you can place any controls you want on the top of it as it's support Z-ORDER. Official delphi video player
are just native video player window on the top of the form and thus not supporting Z-ORDER.

Under android I use ExoPlayer. ExoPlayer is an open source project that is not part of the Android framework and is distributed
separately from the Android SDK. ExoPlayer’s standard audio and video components are built on Android’s MediaCodec API, which
was released in Android 4.1 (API level 16). Because ExoPlayer is a library, you can easily take advantage of new features as
they become available by updating your app. ExoPlayer supports features like Dynamic adaptive streaming over HTTP (DASH), HLS,
SmoothStreaming and Common Encryption, which are not supported by MediaPlayer. It's designed to be easy to customize and extend.

Under Ios i use AVPlayer with support also HLS like exoplayer do.


## Firemonkey native ios and android TEdit and TMemo ##

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

![](//a.fsdn.com/con/app/proj/alcinoe/screenshots/Untitled-3.png/1)
![](//a.fsdn.com/con/app/proj/alcinoe/screenshots/Untitled-4.png/1)
![](//a.fsdn.com/con/app/proj/alcinoe/screenshots/Untitled-2.png/1)
![](//a.fsdn.com/con/app/proj/alcinoe/screenshots/Untitled-8.png/1)

## Fast/doublebuffered with native draw firemonkey controls ## 

* Rectangle
* Circle
* Text (Can also draw html text on ios/android/win/macos)
* Glyph
* etc.

### the fact ###

Painting of Firmonkey controls can be sometime slow, or say 
differently, not sufficiently fast for a fluid scrolling. For exemple if 
you simply look the basic Trectangle with round corners, the paint procedure 
can take around 3 ms! So if you have around 20 visible Trectangles on your 
screen, then it's will cost you around 60 ms to repaint the full screen 
(and normally you don't have only trectangle, you also have Tlabel, 
tcheckbox, etc..). After it's just math, take 100ms to repaint the screen, 
so you can only do around 10 frames per seconds (in reality you will have 
much less even) so the scrolling can't be fluid :(

### the solution ### 
 
I didn't want to rebuild the firemonkey controls, it's too 
huge job for me, and instead I try to find an intermediate solution. 
This what I find by adding "doublebuffered" property to the firemonkey 
controls. So instead to repaint and repaint (and repaint) the controls 
for every single pixels move of the scrollbox, I first paint the control 
on a "buffer" that I store directly in the GPU memory (through TTexture), 
and when the system ask me to repaint the controls instead of calling 
again the paint algorithm i simply redraw the buffer TTexture.

### the results ###  

As I say before it's took 3 ms just to paint a simple 
Trectangle with round corners. With my doublebuffered property 
it's take now around 0.1 ms ! so now the scroll look much more 
fluid! 

### opengl draw => replaced by native ios/android draw ###

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

### why i don't like firemonkey style ###

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

## Improuved firemonkey controls ## 

* ScrollBox
* TabControl
* RangeTrackBar

## Firebase clound messaging ## 

A cross-platform method of using Firebase Cloud Messaging (FCM) to receive push notifications

## Firebase crash analytics ## 

## Android/ios facebook sdk login ## 

Json Parser
-----------

TALJsonDocument is a Delphi parser/writer for JSON / BSON data
format. it's support DOM and SAX parser, support BSON format,
and use a similar syntax than TALXMLDocument / TXMLDocument.
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

Most JSON parser available in Delphi use a DOM-like approach.
For instance, the DBXJSON unit included since Delphi 2010
or the SuperObject library create a class instance mapping
each JSON node. In order to achieve best speed, TALJsonDocument
implement DOM parser and also a SAX parser.

TALJsonDocument syntax is very similar
to TALXMLdocument / TXMLDocument

exemple :

    {
      _id: 1,
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

Exemple :

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

Exemple with connection pool :

    aMongoDBConnectionPoolClient := TAlMongoDBConnectionPoolClient.create(aDBHost, aDBPort);
    try
    
      ::Thread1::
      aMongoDBConnectionPoolClient.SelectData('test.exemple',
                                              '{fieldA:123}', // the query
                                              '{fieldA:1, fieldB:1}', // the return fields selector
                                              aLocalVarJSONDOC.node);
    
      ::Thread2::
      aMongoDBConnectionPoolClient.SelectData('test.exemple',
                                              '{fieldA:999}', // the query
                                              '{fieldA:1, fieldB:1}', // the return fields selector
                                              aLocalVarJSONDOC.node);
    
    finally
      aMongoDBClient.free;
    end;

Exemple tail monitoring :

    aMongoDBTailMonitoringThread := TAlMongoDBTailMonitoringThread.Create(aDBHost,
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


Fast TStringList
----------------

TALStringList Work the same as Delphi TstringList except that it's
allow to search a name=value using a quicksort algorithm when the
list is sorted. Also TALStringList use a locale independant
algorithme (based on the 8-bit ordinal value of each character)
instead of the AnsiCompareText and AnsiCompareStr used by the
Delphi TstringList. at the end the sort in TALStringList is up to
10x more faster than in Delphi TstringList. Also TALStringList is
not an unicode TstringList but an 100% Ansi StringList

TALNVStringList (NV for NameValue) is same as TALStringList (use
also a quicksort algorithme) except that here optimisation is
oriented for name/value list instead of string list.

TALHashedStringList
TALHashedStringList is same as TALStringList except that it's use
an internal hash table instead of a quicksort algorithm. By using
TALHashedStringList instead of TALStringList, you can improve
performance when the list contains a large number of strings
(else if you list don't contain a lot of strings the performance
is lower than TALStringList because of the cost to calculate the
hash)


## PHP runner ##

ALPHPRunnerEngine is a simple but useful component for
easily use php (any version) as a scripting language
in Delphi applications. ALPhpRunnerEngine allows to
execute the PHP scripts within the Delphi program without
a WebServer. ALPHPRunnerEngine use the CGI/FastCGI
interface (php-cgi.exe) of PHP to communicate with PHP engine.


## Memcached Client ##

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

## GSM compoment ##

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

## And also ## 

* CGI runner 
* Http Client (WinInet/WinHTTP)
* MySQL Client 
* NNTP Client
* POP3 Client
* SMTP Client
* SQLite3 Client 
* Xml Parser 
* and more ...

## Where can I get it? ##

you can access the last svn version at:
svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code 
or with a web browser at:
http://alcinoe.svn.sourceforge.net/viewvc/alcinoe/


## INSTALL ##

If you plan to use native control on android (like TALEdit) then you
will need to include lib\jar\alcinoe\alcinoe.jar in 
project manager > target plateform > android > Libraries

You can see the demo of firemonkey controls here: demos/ALFmxControls/ 
and the compiled apk of the demo: https://tinyurl.com/yda8nr6g

If you encounter some problems to compile the demo, please contact me on 
Skype so that i can help you and update this doc.

## WIN64 ##

Unfortunatly, in win64 we lost all the FastCode heritage. 
(that was mostly based on ASM). That mean that most of 
the functions will be around 2x to 10x more slower. 
you can try to launch /demo/ALStringBenchMark/
in win64 and Win32 to see the difference in speed.
This Unfortunatly make the Win64 support of Delphi
like a "gadget", because most of the app demanding the
win64 support are Server App (or dll) and this king 
of app need all the power. I just Hope that 
embarcadero will improuve the speed of the win64 
functions, but i doubt they want to do them in asm 
because they want to go in multi plateforme, and
they not really want to do dedicated ASM for
every plateforme, they instead want to improuve 
their compiler. but i thing it's an utopia that
their compiler will produce code that can compete
with handly optimized ASM

## ABOUT DELPHI D2009+ (UNICODE) ##

No mistake, Unicode was necessary for a product like Delphi. 
But the way embarcadero decide to implement it’s questionable! 
Indeed they decide to migrate the 8bit string to 16bit string 
instead of choosing to implement Unicode through 8bit string 
(UTF8).  This cause the migration of some Delphi app < d2009 
very hard, especially on application that assume that string 
is 8bit. Here a very good article about why to avoid to use 
UFT-16: http://www.utf8everywhere.org/

The main argument of embarcadero why choosing UTF16 instead 
of UTF8 is because the windows API work in the background in 
UTF16 and better to stay in the same way as the windows API. 
This is still questionable when the strategy becomes to make 
Delphi a multi platform product. 

So I was facing the need to choose: stay in D2007, move to 
freepascal (that implement Unicode through UTF8) or migrate 
to Unicode. After studying the Unicode migration and discover 
that it’s will be very hard to do (even in the Delphi I 
discover some bugs in their own source code due to the 
migration from ansi to Unicode like for exemple 
http://qc.embarcadero.com/wc/qcmain.aspx?d=106424). 
Also on lot of my applications the migration to Unicode was 
really unnecessary, as they was already working in UTF8, 
receive their input request in utf8 and output their response 
in UTF8. Here the migration to UTF16 will mean: 
Input (UTF8) => UTF16 => data processing => UTF16 => output(UTF8)
+ off course all the migration job (that include debugging).
In fact, except the input/output to the "visual interface", 
most (if not all) of the input/output of most of the 
application will be done in 8bit string (ex: file storage, 
client/server protocol, HTTP, Smtp, tcp, xml, html, database, 
etc.). So I decide to go in Delphi Xe2 but to stay in 
ansistring (8bit string). My first through was to replace 
everywhere in my code all the String by AnsiString. 
Unfortunately,  most of the usefull and needed string function 
in D2009+ are not available in 8bit string like inttostr, 
strtoint, strtofloat, Tstrings, TstringStream, etc. that 
simply unbelievable, in their way to go in 16bit string, 
embarcadero remove the 8bit support ! of course we can still 
do ansistring(inttostr(x)) but this is very pity (and slow).
So i decide to make my component like a "framework" to help 
the fully support of 8bit string (mostly in UTF8). 

I build a small application (available in /demo/ALStringToAnsiString/)
to convert all string type and string function to their 
ansistring equivalent. The conversion reflects my 
programming style and quirks so you may need to tweek the 
output or modify the converter so you have the code. Will 
this work for your code? Probably not out of the Non Box but 
you may only be left requiring only few little tweaks.

You can also go in /demo/ALStringBenchMark/ to find an 
application to benchmark ansiString vs Unicode String and 
in /demo/ALSortedList/ to see the benchmark of TALStringList 
(ansiString) vs TStringList (UnicodeString). In fact, most 
of the Unicode function (when they are implemented correctly, 
thanks to fastcode) will be close to the same speed as their 
ansi equivalent. But no mistake, Unicode string will still 
use 2x more memory and dependly of the application, this can 
be not acceptable (exemple application that load huge list of 
string in a TstringList or in an XML object).

Under D2009+, ansiString Have now a codepage, and some 
transliteration (OldCodePage => UTF16 => NewCodePage) will 
happen when assigning one ansiString with different codepage 
to another ansistring with another codepage. To avoid this 
it’s important to always set project option to the code page 
you want (eg. 65001 for UTF8) and also to call at the 
beginning of the program SetMultiByteConversionCodePage(CP_UTF8); 
Also it’s very important to avoid to use 2 differents 
string type (eg UTF8string and aniString) even if they have 
the same codepage, because compiler at compile time 
don’t know that codepage is the same and will do a 
transliteration (ex MyAnsiStringUTF8 := MyUTF8String will 
result in UTF8 => UTF16 => UTF8). This is why we use in 
all our code only AnsiString instead of UTF8String (even 
when we assume that string contain only UTF8 char) to 
avoid theses transliteration keep the rule to only use 
AnsiString with SetMultiByteConversionCodePage and not type 
like UTF8string or other

Also about the compiler warning, when he detect a 
transliteration you will have a warning, but he can not 
always detect the transliteration Ex: MyFunctionNeedPWideChar(Pointer(aPansiChar))
Here unfortunatly you will not get any "warning" nor 
any "error" from the compiler.
