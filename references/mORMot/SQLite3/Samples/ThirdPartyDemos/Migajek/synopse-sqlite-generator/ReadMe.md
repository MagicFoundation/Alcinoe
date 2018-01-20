# Scope

This simple application speeds up defining SQL Records classes for the Synopse *mORMot* Framework.

Since some versions of Delphi (including mine) does not have class completion, declaring properties, their setters / getters and so on is a huge waste of time.

Thus I've written this simple tool which introduces so-called meta-language for defining SQL Records which is later converted to Delphi code.

It is simply list of field declarations we want to have in our SQL Record.

# Syntax

Each line is a declaration of one field.

It consists of field name and it's type the type might be shortened using aliases (see below).

Additionally, each field can be marked with `;r` and/or `;w` flags which will make the code generator to use respectively getter and/or setter for the specific field.

# Reverse

The tool also makes it possible to parse back from Delphi class declaration to meta-code.

Please note this will only work if the Delphi class follows naming pattern used in `meta2pas` generation.

# List of available aliases

* str <-> `RawUTF8`
* int <-> `integer`

**enjoy!**

Michal *migajek* Gajek
http://migajek.com migajek@gmail.com
