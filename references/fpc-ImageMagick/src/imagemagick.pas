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

  ImageMagick Application Programming Interface declarations.
}
{
  Converted from c by: Felipe Monteiro de Carvalho Dez/2005

	Bug-fixed by Ángel Eduardo García Hernández
	Thanks to Marc Geldon and RuBBeR
}
{Version 0.4}
unit ImageMagick;

{$ifdef FPC}
  {$mode objfpc}
	{$PACKRECORDS C}
{$endif}

interface

uses SysUtils, ctypes;

{$z4}

// Fix to compile in older FPC versions
{$ifdef VER2_2}
type
  Pcsize_t = ^size_t;
{$endif}

const
{$ifdef Windows}
  MagickExport = 'CORE_RL_magick_.dll';
  WandExport = 'CORE_RL_wand_.dll';
{$else}
  MagickExport = 'libMagickCore';
  WandExport = 'libMagickWand'; // Previous ImageMagick versions used 'libWand'
{$endif}

{# include "magick/methods.h"
#endif}
{$include magick_type.inc}
{$include type.inc}

{#$include animate.inc}
{#$include annotate.inc}
{#$include attribute.inc}
{#$include blob.inc}
{$include cache.inc}
{$include cache_view.inc}
{#include "magick/coder.h"
#include "magick/client.h"
#include "magick/color.h"
#include "magick/colorspace.h"}
{$include compare.inc}
{#include "magick/composite.h"
#include "magick/compress.h"
#include "magick/configure.h"
#include "magick/conjure.h"}
{$include constitute.inc}
{#include "magick/convert.h"
#include "magick/decorate.h"
#include "magick/delegate.h"
#include "magick/deprecate.h"
#include "magick/display.h"}
{$include draw.inc}
{$include effect.inc}
{#include "magick/enhance.h"
#include "magick/exception.h"}
{$include fx.inc}
{#include "magick/gem.h"
#include "magick/geometry.h"
#include "magick/hashmap.h"
#include "magick/identify.h"
#include "magick/image.h"
#include "magick/import.h"
#include "magick/list.h"
#include "magick/locale_.h"
#include "magick/log.h"
#include "magick/magic.h"
#include "magick/magick.h"
#include "magick/memory_.h"
#include "magick/module.h"
#include "magick/mogrify.h"
#include "magick/monitor.h"
#include "magick/montage.h"
#include "magick/option.h"
#include "magick/paint.h"}
{$include pixel.inc}
{#include "magick/prepress.h"
#include "magick/profile.h"}
{$include quantize.inc}
{#include "magick/quantum.h"
#include "magick/registry.h"
#include "magick/random_.h"
#include "magick/resize.h"
#include "magick/resource_.h"
#include "magick/segment.h"
#include "magick/shear.h"
#include "magick/signature.h"
#include "magick/splay-tree.h"
#include "magick/stream.h"}
{$include statistic.inc}
{#include "magick/string_.h"
#include "magick/timer.h"
#include "magick/token.h"
#include "magick/transform.h"
#include "magick/utility.h"
#include "magick/version.h"
#include "magick/xwindow.h"}

implementation

end.
