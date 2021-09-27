{
  Copyright 1999-2005 ImageMagick Studio LLC, a non-profit organization
  dedicated to making software imaging solutions freely available.
  
  You may not use this file except in compliance with the License.
  obtain a copy of the License at
  
    http://www.imagemagick.org/script/license.php
  
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

  ImageMagick pixel wand API.
}


{
  Declaration from pixel-wand.c
}
type
  PixelWand = record
    id: culong;

    name: array[1..MaxTextExtent] of char;

    exception: ExceptionInfo;

    pixel: MagickPixelPacket;

    count: culong;

    debug: MagickBooleanType;

    signature: culong;
  end;
  
  PPixelWand = ^PixelWand;
  
  PPPixelWand = ^PPixelWand;
  

function PixelGetException(const wand: PPixelWand; severity: PExceptionType): PChar; cdecl; external WandExport;
function PixelGetColorAsString(const wand: PPixelWand): PChar; cdecl; external WandExport;

function PixelGetAlpha(const wand: PPixelWand): Double; cdecl; external WandExport;
function PixelGetBlack(const wand: PPixelWand): Double; cdecl; external WandExport;
function PixelGetBlue(const wand: PPixelWand): Double; cdecl; external WandExport;
function PixelGetCyan(const wand: PPixelWand): Double; cdecl; external WandExport;
function PixelGetGreen(const wand: PPixelWand): Double; cdecl; external WandExport;
function PixelGetMagenta(const wand: PPixelWand): Double; cdecl; external WandExport;
function PixelGetOpacity(const wand: PPixelWand): Double; cdecl; external WandExport;
function PixelGetRed(const wand: PPixelWand): Double; cdecl; external WandExport;
function PixelGetYellow(const wand: PPixelWand): Double; cdecl; external WandExport;

function PixelGetIndex(const wand: PPixelWand): IndexPacket; cdecl; external WandExport;

function IsPixelWand(const wand: PPixelWand): MagickBooleanType; cdecl; external WandExport;
function IsPixelWandSimilar(p: PPixelWand; q: PPixelWand;
 const fuzz: double): MagickBooleanType; cdecl; external WandExport;
function PixelClearException(wand: PPixelWand): MagickBooleanType; cdecl; external WandExport;
function PixelSetColor(wand: PPixelWand; const color: PChar): MagickBooleanType; cdecl; external WandExport;

function DestroyPixelWand(wand: PPixelWand): PPixelWand; cdecl; external WandExport;
function DestroyPixelWands(wand: PPPixelWand;
 const number_wands: culong): PPPixelWand; cdecl; external WandExport;
function NewPixelWand: PPixelWand; cdecl; external WandExport;
function NewPixelWands(const number_wands: culong): PPPixelWand; cdecl; external WandExport;

function PixelGetAlphaQuantum(const wand: PPixelWand): Quantum; cdecl; external  WandExport;
function PixelGetBlackQuantum(const wand: PPixelWand): Quantum; cdecl; external WandExport;
function PixelGetBlueQuantum(const wand: PPixelWand): Quantum; cdecl; external WandExport;
function PixelGetCyanQuantum(const wand: PPixelWand): Quantum; cdecl; external WandExport;
function PixelGetGreenQuantum(const wand: PPixelWand): Quantum; cdecl; external WandExport;
function PixelGetMagentaQuantum(const wand: PPixelWand): Quantum; cdecl; external WandExport;
function PixelGetOpacityQuantum(const wand: PPixelWand): Quantum; cdecl; external WandExport;
function PixelGetRedQuantum(const wand: PPixelWand): Quantum; cdecl; external WandExport;
function PixelGetYellowQuantum(const wand: PPixelWand): Quantum; cdecl; external WandExport;

function PixelGetColorCount(const wand: PPixelWand): culong; cdecl; external WandExport;

procedure ClearPixelWand(wand: PPixelWand); cdecl; external WandExport;
//  PixelGetHSL(const PixelWand *,double *,double *,double *), // Added after 6.2
procedure PixelGetQuantumColor(const wand: PPixelWand; color: PPixelPacket); cdecl; external WandExport;
procedure PixelSetAlpha(wand: PPixelWand; const opacity: Double); cdecl; external WandExport;
procedure PixelSetAlphaQuantum(wand: PPixelWand; const opacity: Quantum); cdecl; external WandExport;
procedure PixelSetBlack(wand: PPixelWand; const opacity: Double); cdecl; external WandExport;
procedure PixelSetBlackQuantum(wand: PPixelWand; const opacity: Quantum); cdecl; external WandExport;
procedure PixelSetBlue(wand: PPixelWand; const opacity: Double); cdecl; external WandExport;
procedure PixelSetBlueQuantum(wand: PPixelWand; const opacity: Quantum); cdecl; external WandExport;
//  PixelSetColorFromWand(PixelWand *,const PixelWand *), // Added after 6.2
procedure PixelSetColorCount(wand: PPixelWand; const count: culong); cdecl; external WandExport;
procedure PixelSetCyan(wand: PPixelWand; const opacity: Double); cdecl; external WandExport;
procedure PixelSetCyanQuantum(wand: PPixelWand; const opacity: Quantum); cdecl; external WandExport;
//  PixelSetFuzz(PixelWand *,const double), // Added after 6.2
procedure PixelSetGreen(wand: PPixelWand; const opacity: Double); cdecl; external WandExport;
procedure PixelSetGreenQuantum(wand: PPixelWand; const opacity: Quantum); cdecl; external WandExport;
//  PixelSetHSL(PixelWand *,const double,const double,const double), // Added after 6.2
procedure PixelSetIndex(wand: PPixelWand; const index: IndexPacket); cdecl; external WandExport;
procedure PixelSetMagenta(wand: PPixelWand; const opacity: Double); cdecl; external WandExport;
procedure PixelSetMagentaQuantum(wand: PPixelWand; const opacity: Quantum); cdecl; external WandExport;
procedure PixelSetMagickColor(wand: PPixelWand; const color: PMagickPixelPacket); cdecl; external WandExport; // Added after 6.2
procedure PixelSetOpacity(wand: PPixelWand; const opacity: Double); cdecl; external WandExport;
procedure PixelSetOpacityQuantum(wand: PPixelWand; const opacity: Quantum); cdecl; external WandExport;
procedure PixelSetQuantumColor(wand: PPixelWand; const color: PPixelPacket); cdecl; external WandExport;
procedure PixelSetRed(wand: PPixelWand; const opacity: Double); cdecl; external WandExport;
procedure PixelSetRedQuantum(wand: PPixelWand; const opacity: Quantum); cdecl; external WandExport;
procedure PixelSetYellow(wand: PPixelWand; const opacity: Double); cdecl; external WandExport;
procedure PixelSetYellowQuantum(wand: PPixelWand; const opacity: Quantum); cdecl; external WandExport;

// Considered a private method in newer versions (after 6.2)
procedure PixelGetMagickColor(const wand: PPixelWand; packet: PMagickPixelPacket); cdecl; external WandExport;

