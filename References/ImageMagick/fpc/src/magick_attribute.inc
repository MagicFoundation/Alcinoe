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

  Set or Get Magick Wand Attributes.
}

function MagickGetException(wand: PMagickWand;
 severity: PExceptionType): PChar; cdecl; external WandExport;
function MagickGetFilename(const wand: PMagickWand): PChar; cdecl; external WandExport;
function MagickGetFormat(wand: PMagickWand): PChar; cdecl; external WandExport;
function MagickGetHomeURL: PChar; cdecl; external WandExport;
function MagickGetOption(wand: PMagickWand;
 const key: PChar): PChar; cdecl; external WandExport;
{sem documenta
extern WandExport char
  *MagickQueryConfigureOption(const char *),
  **MagickQueryConfigureOptions(const char *,unsigned long *),
  **MagickQueryFonts(const char *,unsigned long *),
  **MagickQueryFormats(const char *,unsigned long *);}

function MagickGetCompression(wand: PMagickWand): CompressionType; cdecl; external WandExport;

function MagickGetCopyright: PChar; cdecl; external WandExport;
function MagickGetPackageName: PChar; cdecl; external WandExport;
function MagickGetQuantumDepth(depth: Pculong): PChar; cdecl; external WandExport;
function MagickGetQuantumRange(range: Pculong): PChar; cdecl; external WandExport;
function MagickGetReleaseDate: PChar; cdecl; external WandExport;
function MagickGetVersion(version: Pculong): PChar; cdecl; external WandExport;

function MagickGetSamplingFactors(wand: PMagickWand;
 number_factors: Pculong): Pdouble; cdecl; external WandExport;
{function (wand: PMagickWand): Pdouble; cdecl; external WandExport;
  *MagickQueryFontMetrics(MagickWand *,const DrawingWand *,const char *),
  *MagickQueryMultilineFontMetrics(MagickWand *,const DrawingWand *,
    const char *);}

function MagickGetInterlaceScheme(wand: PMagickWand): InterlaceType; cdecl; external WandExport;

{function (const wand: PMagickWand): ; cdecl; external WandExport;
extern WandExport MagickBooleanType
  MagickGetPage(MagickWand *,unsigned long *,unsigned long *,long *,long *),
  MagickGetSize(const MagickWand *,unsigned long *,unsigned long *),
  MagickSetBackgroundColor(MagickWand *,const PixelWand *),
  MagickSetCompression(MagickWand *,const CompressionType),
  MagickSetCompressionQuality(MagickWand *,const unsigned long),
  MagickSetFilename(MagickWand *,const char *),
  MagickSetFormat(MagickWand *,const char *),
  MagickSetInterlaceScheme(MagickWand *,const InterlaceType),
  MagickSetOption(MagickWand *,const char *,const char *),
  MagickSetPage(MagickWand *,const unsigned long,const unsigned long,
    const long,const long),
  MagickSetPassphrase(MagickWand *,const char *),
  MagickSetResolution(MagickWand *,const double,const double),
  MagickSetResourceLimit(const ResourceType type,const unsigned long limit),
  MagickSetSamplingFactors(MagickWand *,const unsigned long,const double *),
  MagickSetSize(MagickWand *,const unsigned long,const unsigned long),
  MagickSetType(MagickWand *,const ImageType);

function (const wand: PMagickWand): ; cdecl; external WandExport;
extern WandExport MagickProgressMonitor
  MagickSetProgressMonitor(MagickWand *,const MagickProgressMonitor,void *);

function (const wand: PMagickWand): ; cdecl; external WandExport;
extern WandExport unsigned long
  MagickGetCompressionQuality(MagickWand *),
  MagickGetResource(const ResourceType),
  MagickGetResourceLimit(const ResourceType);}

