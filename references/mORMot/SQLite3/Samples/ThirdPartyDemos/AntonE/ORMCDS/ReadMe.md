# Description

Unit to convert `TSQLRecord` and it's sub-arrays/records to Nested `TClientdataset`.

Some key features:

* Create `TClientdataset` hierarchy dynamically based on data.
* Also work with static `TClientDataset`+Static fields.
* Handle sub-`TSQLRecord` lists. (See sample)
* Convert Set of ENUM to/from multiple `Boolean` fields for grid checkboxes
* Most importantly: Apply delta-changes back to *mORMot*. i.e. only changed fields.
* (With RTTI adjustments), should work on any platform that support `TClientdataset`, e.g. Intraweb

It is very first version so not tested on insert/delete yet nor many types of data, guaranteed to be buggy & lacking at this stage, but working, with the latest versions of Delphi only (this first version uses the new Rtti.pas unit, and not *mORMot*'s RTTI).

# Forum Thread

See http://synopse.info/forum/viewtopic.php?id=1911

# Supplied Demo project

Choose Static or Dynamic demo, select 'Load' to load data, edit any field or nested data, click 'Apply'.

This Demo would need the JVCL Grid to compile - see http://wiki.delphi-jedi.org/wiki/JVCL_Help:TJvDBUltimGrid

Best is to see code to get more info.

# Disclaimer

My first try with RTTI and new at *mORMot* so I'm sure it could have been done more elegantly with mORMot's RTTI built-support.

Also not optimized for speed but should be pretty fast.