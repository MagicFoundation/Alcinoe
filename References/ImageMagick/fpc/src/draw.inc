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

  ImageMagick drawing methods.
}

//#include "magick/type.h"

type
 AlignType = (
  UndefinedAlign,
  LeftAlign,
  CenterAlign,
  RightAlign
 );

type
 ClipPathUnits = (
  UndefinedPathUnits,
  UserSpace,
  UserSpaceOnUse,
  ObjectBoundingBox
 );

type
 DecorationType = (
  UndefinedDecoration,
  NoDecoration,
  UnderlineDecoration,
  OverlineDecoration,
  LineThroughDecoration
 );

type
 FillRule = (
  UndefinedRule,
//#undef EvenOddRule
  EvenOddRule,
  NonZeroRule
 );

type
 GradientType = (
  UndefinedGradient,
  LinearGradient,
  RadialGradient
 );

type
 LineCap = (
  UndefinedCap,
  ButtCap,
  RoundCap,
  SquareCap
 );

type
 LineJoin = (
  UndefinedJoin,
  MiterJoin,
  RoundJoin,
  BevelJoin
 );

type
 PaintMethod = (
  UndefinedMethod,
  PointMethod,
  ReplaceMethod,
  FloodfillMethod,
  FillToBorderMethod,
  ResetMethod
 );

type
 PrimitiveType = (
  UndefinedPrimitive,
  PointPrimitive,
  LinePrimitive,
  RectanglePrimitive,
  RoundRectanglePrimitive,
  ArcPrimitive,
  EllipsePrimitive,
  CirclePrimitive,
  PolylinePrimitive,
  PolygonPrimitive,
  BezierPrimitive,
  ColorPrimitive,
  MattePrimitive,
  TextPrimitive,
  ImagePrimitive,
  PathPrimitive
 );

type
 ReferenceType = (
  UndefinedReference,
  GradientReference
 );

type
 SpreadMethod = (
  UndefinedSpread,
  PadSpread,
  ReflectSpead,
  RepeatSpread
 );

type
  GradientInfo = record
    type_: GradientType;

    color: PixelPacket;

    stop: SegmentInfo;

    length: culong;

    spread: SpreadMethod;

    debug: MagickBooleanType;

    signature: culong;

    previous, next: Pointer;
{  struct _GradientInfo
    *previous,
    *next;}
  end;

type
  ElementReference = record
    id: PChar;

    type_: ReferenceType;

    gradient: GradientInfo;

    signature: culong;

    previous, next: Pointer;
{  struct _ElementReference
    *previous,
    *next;}
  end;

type
  DrawInfo = record
    primitive,
    geometry: PChar;

    viewbox: RectangleInfo;

    affine: AffineMatrix;

    gravity: GravityType;

    fill,
    stroke: PixelPacket;

    stroke_width: double;

    gradient: GradientInfo;

    fill_pattern,
    tile,
    stroke_pattern: PImage;

    stroke_antialias,
    text_antialias: MagickBooleanType;

    fill_rule: FillRule;

    linecap_: LineCap;

    linejoin_: LineJoin;

    miterlimit: culong;

    dash_offset: double;

    decorate: DecorationType;

    compose: CompositeOperator;

    text: PChar;

    face: culong;

    font,
    metrics,
    family: PChar;

    style: StyleType;

    stretch: StretchType;

    weight: culong;
    
    encoding: PChar;

    pointsize: double;

    density: PChar;

    align: AlignType;

    undercolor,
    border_color: PixelPacket;

    server_name: PChar;

    dash_pattern: Pdouble;

    clip_path: PChar;

    bounds: SegmentInfo;

    clip_units: ClipPathUnits;

    opacity: Quantum;

    render: MagickBooleanType;

    element_reference: ElementReference;

    debug: MagickBooleanType;

    signature: culong;
  end;
  
  PDrawInfo = ^DrawInfo;
  
  PPDrawInfo = ^PDrawInfo;

type
  PointInfo = record
    x, y: double;
  end;
  
  PPointInfo = ^PointInfo;

type
  PrimitiveInfo = record
    point: PointInfo;

    coordinates: culong;

    primitive: PrimitiveType;

    method: PaintMethod;

    text: PChar;
  end;

type
  TypeMetric = record
    pixels_per_em: PointInfo;

    ascent,
    descent,
    width,
    height,
    max_advance,
    underline_position,
    underline_thickness: double;

    bounds: SegmentInfo;
  end;

{extern MagickExport DrawInfo
  *CloneDrawInfo(const ImageInfo *,const DrawInfo *),
  *DestroyDrawInfo(DrawInfo *);

extern MagickExport MagickBooleanType
  DrawAffineImage(Image *,const Image *,const AffineMatrix *),
  DrawClipPath(Image *,const DrawInfo *,const char *),
  DrawImage(Image *,const DrawInfo *),
  DrawPatternPath(Image *,const DrawInfo *,const char *,Image **),
  DrawPrimitive(Image *,const DrawInfo *,const PrimitiveInfo *);

extern MagickExport void
  GetAffineMatrix(AffineMatrix *),
  GetDrawInfo(const ImageInfo *,DrawInfo *);}


