SynTaskDialog for Lazarus and FireMonkey
========================================

Patched version by Ondrej Pokorny (reddwarf), adding Lazarus support to the SynTaskDialog.pas unit.

Including FireMonkey support by Gergely Kovacs, for true cross-platform and cross-compiler use - tested with Delphi XE7.

TaskDialog display would be native on Windows Vista+, emulated on all other platforms - Windows, Linux and OSX tested.


Added Features
--------------

* Emulated display using LCL under Lazarus;
* Emulated display using FMX under Delphi XE7;
* Added external translation function for the emulated dialog (`TaskDialog_Translate`);
* `tdfAllowDialogCancellation` handled in emulated dialog: if not set, Alt+F4 is blocked; if set: Esc is allowed;
* `tdfPositionRelativeToWindow` handled in emulated dialog


Remarks
-------

Maybe To-Do: High DPI-aware emulated dialog + icons.


Contributors
------------

* Arnaud Bouchez (initial unit for Windows)
* Ulrich Gerhardt (fixes and improvements)
* Ondrej Pokorny (LCL/Lazarus port)
* Gergely Kovacs (FireMonkey port)

Thanks to all for sharing your work!


License
-------

Source code licensed under the original MPL/LGPL/GPL tri-license terms of the Synopse components.

Platform-independent icons are from www.iconsdb.com:
    Icon license:
      This icon is provided as CC0 1.0 Universal (CC0 1.0) Public Domain
      Dedication.
      You can copy, modify, use, distribute this icon, even for commercial
      purposes, all without asking permission with no attribution required,
      but always appreciated.
