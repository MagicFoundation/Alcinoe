Stéphane Vander Clock 
skype/email: svanderclock@yahoo.fr

you can access the last svn version at:
svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code 
or with a web browser at:
http://alcinoe.svn.sourceforge.net/viewvc/alcinoe/


ABOUT SUPPORT OF DELPHI VERSION
-------------------------------

Actually the source is maintained on last version of 
Delphi (XE4 - seattle). The modifications to make it 
working on lower version of Delphi are little and can 
be easily handled via define condition. But I have no 
time and no older delphi compiler to test it. Anyone 
who want to work on it is welcome, and I will gave 
him a write access to the svn.


ABOUT WIN64
-----------

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


ABOUT DELPHI D2009+ (UNICODE)
-----------------------------

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
