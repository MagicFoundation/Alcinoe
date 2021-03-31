{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

    Scripting support for mORMot Copyright (C) 2021 Pavel Mashlyakovsky
      pavel.mash at gmail.com

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Initial Developer of the Original Code is
  Pavel Mashlyakovsky.
  Portions created by the Initial Developer are Copyright (C) 2013
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.18
  - initial release. Use SpiderMonkey 1.8.5
  - defines some global conditionals used by SynSMAPI.pas and SynSM.pas

}


{.$define JS_188}
// for future use

{.$define JS_189}
// for future use

{$DEFINE IS_LITTLE_ENDIAN}
//TODO add processor architecture check here or remove define and corresponding code below if FPC only for x32/64?

{$DEFINE JS_THREADSAFE}
// we MUST compile mozjs with JS_THREADSAFE directive

{$DEFINE SM_DEBUG}
// for debuging SynSM

{$DEFINE CONSIDER_TIME_IN_Z}
// let serve all date-time as in GMT0 (UTC) timezone

{.$DEFINE WITHASSERT}
// ensure *TO_JSVAL() macros will check explicitly for the target type

{$ifndef FPC}
{$if CompilerVersion = 24.0}
  // see https://synopse.info/forum/viewtopic.php?pid=12598#p12598
  {$define FIXBUGXE3}
{$ifend}
{$endif}

