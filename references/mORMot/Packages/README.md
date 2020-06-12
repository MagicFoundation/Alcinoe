
# Synopse mORMot Packages

We are providing two packages for FPC/Lazarus:

- `mormot_base`: Core units needed by *mORMot*
  - Implements ORM, SOA and MVC features
  - ORM via static-linked *SQLite3*
  - ORM over external SQL and *MongoDB*
  - High-level Domain-Driven-Design units  
- `mormot_cross`: Stand-alone package, client-side only, but should be running on all FPC targets

## Lazarus

Initially these Packages were designed to compile into Lazarus.

The `mormot_base` package has just one dependency, disabled by default, which is [ZeosLib](https://sourceforge.net/projects/zeoslib/).

If you want to use *ZeosLib*, you must setup the package before compile it, follwoing instructions below:

1. Open the Package
2. Click on Options
3. In "Compile Options", click on "Custom Options"
4. Click on "Defines" and uncheck `NOSYNDBZEOS` conditional
5. Add `zcomponent` package from *ZeosLib* into it.
6. Save and return to Package
7. Compile

If you have compiled without using this option before, follow the steps above, but using "More > Recompile Clean" option to recompile the package.

If `NOSYNDBZEOS` is defined, `SynDBZeos.pas` unit will be just an "empty unit".


## Delphi

Delphi Packages are not defined since they don't make sense for *mORMot* source code, which doesn't have any visual component.

Just get the *mORMot* sources, then add the corresponding search path to your IDE. See [the corresponding documentation](https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html#TITL_113).