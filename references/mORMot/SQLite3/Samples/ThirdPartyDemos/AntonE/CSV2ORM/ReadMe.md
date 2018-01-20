# Description

This tool takes a CSV file with basic metadata and expand it into a source file(s) for use in Delphi based on pre-defined templates.

Using single and multi-file templates you can make almost any unit or form. Included example includes generating a *mORMot* `TSQLRecord` descendant as well as a CDS-based form for editing a single record/object.

It is not intended as a full-fledged RAD tool but rather to generate the bulk of repetitive code so you can copy to your project and edit from there on.

# Forum Thread

See http://synopse.info/forum/viewtopic.php?id=1911

# Usage

    Create CSV file 

e.g. `SampleObj.csv`:

    Code,RawUTF8,Edit,30
    Desc,RawUTF8,Edit,512
    ItemType,RawUTF8,ComboBox,30
    Cost,Currency,Edit
    LastCostD,TDateTime,DateEdit
    VatCat,Integer,RadioGroup
    Active,Boolean,CheckBox

# Format

    <Property/Field name>, DataType, Control[, Size]

Save with `FileName` of class name, e.g. `SampleObj.csv` would create e.g. classes:

    DataSampleObj.pas (class TSQLSampleObj)
    SampleObjFormU.pas (Class TSampleObjForm)
    SampleObjFormU.dfm


When creating template, keep in mind the following, there are few magic-cookies that get replaced with your text and some tags.

Some magic-cookies:

* MyObj = ClassName, e.g. SampleObj (Determined by filename)
* MyName = property name e.g. BirthDate
* MyType = property type, e.g. `RawUTF8`
* MyField = property CDS Field type `TStringField`
* MyFieldAs = CDS field get/setter str, e.g. `AsString`
* MyControl = If assigned, expanded control type, e.g. `TEdit`
* MyDBControl = If assigned, expanded DB control type, e.g. `TDBEdit`
* MyControlName = If assigned, control name, e.g. `EditBirthDate`
* MyTop = Value that start at 21 and get incremented by 21 on each control, to space controls underneath each other.
* MySize = Size of CDS string field, see 'Tags' below.

# Tags

There are 3 tags:

* `[Fields]...[/Fields]` Cause a loop, passing all properties
* `[Controls] ..[/Controls]` Cause a loop, passing all properties that have a control assigned.
* `[Size]` Only include this line (tag can be anywhere on the line) if the current field have a size>0, used for TStringField size.
* `[!Size]` Only onclude this line if current filed have `Size=0` (i.o.w. not a String field)

E.g.:

    [Fields][Size]Obj.MyName           :=StringToUTF8(CDSMyName   .MyFieldAs);
           [!Size]Obj.MyName           :=             CDSMyName   .MyFieldAs;[/Fields]

This would create a line for each field/property. String fields will get 1st line `[Size]` with `StringToRawUTF8`, other fields will be rendered with 2nd line `[!Size]`, e.g.:

    Obj.Code :=StringToRAWUTF8(CDSCode.AsString);
    Obj.Cost :=                CDSCost.AsInteger;

Templates for generated files can be edited/saved. It is saved in `TClientdataset` binary format, so make backups as upgrades might make it useless.

# Future needed changes

* Make magic-cookies customizable.
* Use syntax-highlighter for code editing.
* Better tags or scripting to make more versatile.
* Support for more data-types and control types. (In meantime, use `Integer`/`RawUTF8` or `TEdit` for unsupported types and edit code afterwards)
* Way to indent code - prettyfy.
* Save templates in separate text files that can be edited in external editor.

# Changelog

* 2014/07/28 - ver 1.1
 - Added Templates and non-DB controls.
 - Added Tags for Field/Control loops and [Size]/[!Size] to make conditional render of string fields. (Fields with a size limit)
