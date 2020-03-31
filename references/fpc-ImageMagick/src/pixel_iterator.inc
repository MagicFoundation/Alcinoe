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

  Pixel Iterator Methods.
}

type
  PixelIterator = record
    id: culong;

    name: array[0..MaxTextExtent] of char;

    exception: ExceptionInfo;

    view: PViewInfo;

    region: RectangleInfo;

    y: cint;

    pixel_wand: PPPixelWand;

    debug: MagickBooleanType;

    signature: culong;
  end;
  
  PPixelIterator = ^PixelIterator;


function PixelGetIteratorException(const iterator: PPixeliterator;
 severity: PExceptionType): PChar; cdecl; external WandExport;

function IsPixelIterator(const iterator: PPixeliterator): MagickBooleanType; cdecl; external WandExport;
function PixelClearIteratorException(iterator: PPixeliterator): MagickBooleanType; cdecl; external WandExport;
function PixelSetIteratorRow(iterator: PPixeliterator;
 const row: cint): MagickBooleanType; cdecl; external WandExport;
function PixelSyncIterator(iterator: PPixeliterator): MagickBooleanType; cdecl; external WandExport;

function DestroyPixelIterator(iterator: PPixeliterator): PPixelIterator; cdecl; external WandExport;
function NewPixelIterator(wand: PMagickWand): PPixelIterator; cdecl; external WandExport;
function NewPixelRegionIterator(
 wand: PMagickWand; const x, y: cint; const columns, rows: culong;
 const modify: MagickBooleanType): PPixelIterator; cdecl; external WandExport;

function PixelGetNextIteratorRow(iterator: PPixeliterator; var wandCount: culong): PPPixelWand; cdecl; external WandExport;
function PixelGetPreviousIteratorRow(iterator: PPixeliterator; var wandCount: culong): PPPixelWand; cdecl; external WandExport;

procedure ClearPixelIterator(iterator: PPixeliterator); cdecl; external WandExport;
procedure PixelResetIterator(iterator: PPixeliterator); cdecl; external WandExport;
procedure PixelSetFirstIteratorRow(iterator: PPixeliterator); cdecl; external WandExport;
procedure PixelSetLastIteratorRow(iterator: PPixeliterator); cdecl; external WandExport;

{
  Deprecated.
}
{extern WandExport char
  *PixelIteratorGetException(const PixelIterator *,ExceptionType *);

extern WandExport PixelWand
  **PixelGetNextRow(PixelIterator *);}

