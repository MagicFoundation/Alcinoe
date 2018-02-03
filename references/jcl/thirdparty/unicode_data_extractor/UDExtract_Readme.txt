(c) 1999-2000 Dipl. Ing. Mike Lischke 

UDExtract (which stands for Unicode Data Extractor) is a command line tool to convert the Unicode character database and optionally additional 
files into a resource script which can be compiled by a resource compiler to a *.res file. This resource file can then be used in application to access 
various character properties. The Unicode library on www.lischke-online.de/Unicode.html uses such a resource file for casing, decomposition etc.
Data files (Unicode character database, case folding, special casing etc.) can be downloaded from this site too.

UDExtract comes with full source code.


Usage of UDExtract:
------------------------------
Start UDExtract.exe without parameters or with the /? switch to get a help screen similar to the listing below.

Unicode database conversion tool
© 2000, written by Dipl. Ing. Mike Lischke [public@lischke-online.de]

Usage: UDExtract Source[.txt] Target[.rc] options
  Path and extension are optional. Default extension for all source files
  (including optional files) is ".txt".
  Source must be a Unicode data file (e.g. UnicodeData-3.0.1.txt)
  and Target is a resource script.

  Options might have the following values (not case sensitive):
    /?          shows this screen
    /c=filename specifies an optional file containing special casing
                properties (e.g. SpecialCasing-3.txt)
    /f=filename specifies an optional file containing case fold
                mappings (e.g. CaseFolding-2.txt)
    /v          verbose mode; no warnings, errors etc. are shown, no user input is required

Basically two parameters are needed, the Unicode character database file name where to get the main data from and the resource script file name
where to write the result to. You can specify optional files which contain additional data. The case folding data is important as it is used when 
comparing strings case in-sensitively. If this data is not given then a default path is taken by always using lower case (which is not correct in all 
cases, however). The special casing file contains one-to-many case mapping, that is, mappings from single sized code point (one UCS2 value) to 
several (e.g. the german "ß" es-zed, is upper cased by converting to "SS").

Note: no validation checks are done to verify the correct files are used, so check this manually in advance.

UDExtract returns status codes for use in batch processing. The possible values are:
  0 successful creation, there might have been warnings however
  1 source file not found
  2 invalid option
  3 OS error (e.g. could not create target file)
  4 fatal conversion error


The result of the extraction is a resource script (*.rc file) which can directly be added to a project in Delphi 5 or BCB 5 (which will then automatically 
create the *.res file) or can be compiled using any 32 bit resouce compiler like MSVC++, BRCC32, Resource Workshop etc.

The format of the *.res data is as follows:
1) an own (binary) data type is used named "UNICODEDATA"
2) 5 entries using this data type are inserted, which are named:
  - CATEGORIES
  - CASE
  - DECOMPOSITION
  - COMBINING
  - NUMBERS
  so you can use 
    FindResouce(0, 'CASE', 'UNICODEDATA') 
  or a resource stream, e.g. 
    Stream := TResourceStream.Create(HInstance, 'CATEGORIES', 'UNICODEDATA');
  to load that data.
3) The entries are structure so:
3.1) CATEOGRIES
  - for each used category:
    - a byte containing the ordinal number of the category (see also the list at the end of this readme file)
    - a cardinal (4 byte) containing the number of ranges in this category
    - for each range in the category:
      - a cardinal containing the start value of the range
      - a cardinal containing the end value of the range
  Note: categories are sorted in increasing order, ranges are sorted in increasing order too
3.2) CASE
  - a cardinal containing the number of used case mapping entries
  - for each entry:
    - a cardinal containing a code point
    - a cardinal containing the number of case fold entries
    - a list of cardinals (count is given by the previous value) containg the mapping
    - a cardinal containing the number of lower case entries
    - a list of cardinals containing the actual lower case mapping
    - a cardinal containing the number of title case entries
    - a list of cardinals containing the actual title case mapping
    - a cardinal containing the number of upper case entries
    - a list of cardinals containing the actual upper case mapping
3.3) DECOMPOSITION
  - a cardinal containing the number of used decompositions
  - for each entry:
    - a cardinal containing the code point
    - a cardinal containing the number of decomposition entries for that code point
    - a list of cardinals (number is given by the previous value) containing the actual decomposition values
3.4) COMBINING
  - for each used canonical combining class:
    - a cardinal containing the class
    - a cardinal containing the number of ranges in this class
    - for each range:
      - a cardinal containing the start value of the range
      - a cardinal containing the end value of the range
3.5) NUMBERS
  This resouce contains actually two entries, a list of numbers in nominator/denominator form and a list of code points with an index into the first table (where applicable).
  - a cardinal containing the number of entries in the number table
  - for each entry:
    - a cardinal containing the nominator
    - a cardinal containing the denominator
  - a cardinal containing the number of code-index pairs
  - for each pair:
    - a cardinal containing the code point
    - a cardinal containing the index into the numbers table

Have fun and

Ciao, Mike

public@lischke-online.de
www.lischke-online.de