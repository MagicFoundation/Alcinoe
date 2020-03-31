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

  ImageMagick drawing wand API.
}

{
  Declaration from drawing-wand.c
}
type
 PathOperation = (
  PathDefaultOperation,
  PathCloseOperation,                        // Z|z (none) */
  PathCurveToOperation,                      // C|c (x1 y1 x2 y2 x y)+ */
  PathCurveToQuadraticBezierOperation,       // Q|q (x1 y1 x y)+ */
  PathCurveToQuadraticBezierSmoothOperation, // T|t (x y)+ */
  PathCurveToSmoothOperation,                // S|s (x2 y2 x y)+ */
  PathEllipticArcOperation,                  // A|a (rx ry x-axis-rotation large-arc-flag sweep-flag x y)+ */
  PathLineToHorizontalOperation,             // H|h x+ */
  PathLineToOperation,                       // L|l (x y)+ */
  PathLineToVerticalOperation,               // V|v y+ */
  PathMoveToOperation                        // M|m (x y)+ */
 );

type
 PathMode = (
  DefaultPathMode,
  AbsolutePathMode,
  RelativePathMode
 );

  DrawingWand = record
    id: culong;

    name: array[0..MaxTextExtent] of Char;

    { Support structures }
  
    image: PImage;

    exception: ExceptionInfo;

    { MVG output string and housekeeping }
  
    mvg: PChar;               // MVG data

    mvg_alloc,                // total allocated memory
//    mvg_length: size_t;       // total MVG length

    mvg_width: culong;      // current line width 

    { Pattern support }
    
    pattern_id: PChar;

    pattern_bounds: RectangleInfo;

//    pattern_offset: size_t;

    { Graphic wand }
    
    index: culong;          // array index

    graphic_context: PPDrawInfo;

    filter_off: MagickBooleanType;    // true if not filtering attributes

    { Pretty-printing depth }
  
    indent_depth: culong;       // number of left-hand pad characters 

    { Path operation support }
  
    path_operation: PathOperation;

    path_mode: PathMode;

    destroy,
    debug: MagickBooleanType;

    signature: culong;
  end;
 
  PDrawingWand = ^DrawingWand;

const x = 9;

function DrawGetTextAlignment(const wand: PDrawingWand): AlignType; cdecl; external WandExport;

function DrawGetClipPath(const wand: PDrawingWand): PChar; cdecl; external WandExport;
function DrawGetException(const wand: PDrawingWand; severity: PExceptionType): PChar; cdecl; external WandExport;
function DrawGetFont(const wand: PDrawingWand): PChar; cdecl; external WandExport;
function DrawGetFontFamily(const wand: PDrawingWand): PChar; cdecl; external WandExport;
function DrawGetTextEncoding(const wand: PDrawingWand): PChar; cdecl; external WandExport;
function DrawGetVectorGraphics(const wand: PDrawingWand): PChar; cdecl; external WandExport;

function DrawGetClipUnits(const wand: PDrawingWand): ClipPathUnits; cdecl; external WandExport;

function DrawGetTextDecoration(const wand: PDrawingWand): DecorationType; cdecl; external WandExport;

function DrawGetFillAlpha(const wand: PDrawingWand): double; cdecl; external WandExport;
function DrawGetFontSize(const wand: PDrawingWand): double; cdecl; external WandExport;
function DrawGetStrokeDashArray(const wand: PDrawingWand; number_elements: culong): PDouble; cdecl; external WandExport;
function DrawGetStrokeDashOffset(const wand: PDrawingWand): double; cdecl; external WandExport;
function DrawGetStrokeAlpha(const wand: PDrawingWand): double; cdecl; external WandExport;
function DrawGetStrokeWidth(const wand: PDrawingWand): double; cdecl; external WandExport;

function PeekDrawingWand(const wand: PDrawingWand): PDrawInfo; cdecl; external WandExport;

function CloneDrawingWand(const wand: PDrawingWand): PDrawingWand; cdecl; external WandExport;
function DestroyDrawingWand(wand: PDrawingWand): PDrawingWand; cdecl; external WandExport;
{ Sem documenta
function DrawAllocateWand(const DrawInfo *,Image *): PDrawingWand; cdecl; external WandExport;
}
function NewDrawingWand: PDrawingWand; cdecl; external WandExport;

function DrawGetClipRule(const wand: PDrawingWand): FillRule; cdecl; external WandExport;
function DrawGetFillRule(const wand: PDrawingWand): FillRule; cdecl; external WandExport;

function DrawGetGravity(const wand: PDrawingWand): GravityType; cdecl; external WandExport;

function DrawGetStrokeLineCap(const wand: PDrawingWand): LineCap; cdecl; external WandExport;

function DrawGetStrokeLineJoin(const wand: PDrawingWand): LineJoin; cdecl; external WandExport;

function DrawClearException(wand: PDrawingWand): MagickBooleanType; cdecl; external WandExport;
function DrawComposite(wand: PDrawingWand; const compose: CompositeOperator;
 const x, y, width, height: double; magick_wand: PMagickWand): MagickBooleanType; cdecl; external WandExport;
function DrawGetStrokeAntialias(const wand: PDrawingWand): MagickBooleanType; cdecl; external WandExport;
function DrawGetTextAntialias(const wand: PDrawingWand): MagickBooleanType; cdecl; external WandExport;
function DrawPopPattern(wand: PDrawingWand): MagickBooleanType; cdecl; external WandExport;
function DrawPushPattern(wand: PDrawingWand; const pattern_id: PChar;
 const x, y, width, height: double): MagickBooleanType; cdecl; external WandExport;
function DrawRender(wand: PDrawingWand): MagickBooleanType; cdecl; external WandExport;
function DrawSetClipPath(wand: PDrawingWand; const clip_path: PChar): MagickBooleanType; cdecl; external WandExport;
function DrawSetFillPatternURL(wand: PDrawingWand; const fill_url: PChar): MagickBooleanType; cdecl; external WandExport;
function DrawSetFont(wand: PDrawingWand; const font_name: PChar): MagickBooleanType; cdecl; external WandExport;
function DrawSetFontFamily(wand: PDrawingWand; const font_family: PChar): MagickBooleanType; cdecl; external WandExport;
function DrawSetStrokeDashArray(wand: PDrawingWand; const number_elements: culong;
 const dash_array: Pdouble): MagickBooleanType; cdecl; external WandExport;
function DrawSetStrokePatternURL(wand: PDrawingWand; const stroke_url: PChar): MagickBooleanType; cdecl; external WandExport;
function DrawSetVectorGraphics(wand: PDrawingWand; const xml: PChar): MagickBooleanType; cdecl; external WandExport;
function IsDrawingWand(const wand: PDrawingWand): MagickBooleanType; cdecl; external WandExport;
function PopDrawingWand(wand: PDrawingWand): MagickBooleanType; cdecl; external WandExport;
function PushDrawingWand(wand: PDrawingWand): MagickBooleanType; cdecl; external WandExport;

function DrawGetFontStretch(const wand: PDrawingWand): StretchType; cdecl; external WandExport;

function DrawGetFontStyle(const wand: PDrawingWand): StyleType; cdecl; external WandExport;

function DrawGetFontWeight(const wand: PDrawingWand): culong; cdecl; external WandExport;
function DrawGetStrokeMiterLimit(const wand: PDrawingWand): culong; cdecl; external WandExport;

procedure ClearDrawingWand(wand: PDrawingWand); cdecl; external WandExport;
procedure DrawAffine(wand: PDrawingWand; const affine: PAffineMatrix); cdecl; external WandExport;
procedure DrawAnnotation(wand: PDrawingWand; const  x, y: double;
 const text: PChar); cdecl; external WandExport;
procedure DrawArc(wand: PDrawingWand; const sx, sy, ex, ey, sd, ed: double); cdecl; external WandExport;
procedure DrawBezier(wand: PDrawingWand; const number_coordinates: culong;
 const coordinates: PPointInfo); cdecl; external WandExport;
procedure DrawCircle(wand: PDrawingWand; const ox, oy, px, py: double); cdecl; external WandExport;
procedure DrawColor(wand: PDrawingWand; const x, y: double;
 const paint_method: PaintMethod); cdecl; external WandExport;
procedure DrawComment(wand: PDrawingWand; const comment: PChar); cdecl; external WandExport;
procedure DrawEllipse(wand: PDrawingWand; const ox, oy, rx, ry, start, end_: double); cdecl; external WandExport;
procedure DrawGetFillColor(const wand: PDrawingWand; fill_color: PPixelWand); cdecl; external WandExport;
procedure DrawGetStrokeColor(const wand: PDrawingWand; stroke_color: PPixelWand); cdecl; external WandExport;
procedure DrawGetTextUnderColor(const wand: PDrawingWand; under_color: PPixelWand); cdecl; external WandExport;
procedure DrawLine(wand: PDrawingWand; const sx, sy, ex, ey: double); cdecl; external WandExport;
procedure DrawMatte(wand: PDrawingWand; const x, y: double;
 const paint_method: PaintMethod); cdecl; external WandExport;
procedure DrawPathClose(wand: PDrawingWand); cdecl; external WandExport;
procedure DrawPathCurveToAbsolute(wand: PDrawingWand;
 const x1, y1, x2, y2, x, y: double); cdecl; external WandExport;
procedure DrawPathCurveToRelative(wand: PDrawingWand;
 const x1, y1, x2, y2, x, y: double); cdecl; external WandExport;
procedure DrawPathCurveToQuadraticBezierAbsolute(wand: PDrawingWand;
 const x1, y1, x, y: double); cdecl; external WandExport;
procedure DrawPathCurveToQuadraticBezierRelative(wand: PDrawingWand;
 const x1, y1, x, y: double); cdecl; external WandExport;
procedure DrawPathCurveToQuadraticBezierSmoothAbsolute(wand: PDrawingWand;
 const x, y: double); cdecl; external WandExport;
procedure DrawPathCurveToQuadraticBezierSmoothRelative(wand: PDrawingWand;
 const x, y: double); cdecl; external WandExport;
procedure DrawPathCurveToSmoothAbsolute(wand: PDrawingWand;
 const x2, y2, x, y: double); cdecl; external WandExport;
procedure DrawPathCurveToSmoothRelative(wand: PDrawingWand;
 const x2, y2, x, y: double); cdecl; external WandExport;
procedure DrawPathEllipticArcAbsolute(wand: PDrawingWand;
    const rx, ry, x_axis_rotation: double;
    const large_arc_flag: MagickBooleanType;
    const sweep_flag: MagickBooleanType; const x, y: double); cdecl; external WandExport;
procedure DrawPathEllipticArcRelative(wand: PDrawingWand;
    const rx, ry, x_axis_rotation: double;
    const large_arc_flag: MagickBooleanType;
    const sweep_flag: MagickBooleanType; const x, y: double); cdecl; external WandExport;
procedure DrawPathFinish(wand: PDrawingWand); cdecl; external WandExport;
procedure DrawPathLineToAbsolute(wand: PDrawingWand; const x, y: double); cdecl; external WandExport;
procedure DrawPathLineToRelative(wand: PDrawingWand; const x, y: double); cdecl; external WandExport;
{procedure DrawPathLineToHorizontalAbsolute(wand: PDrawingWand;
 const mode: PathMode; const x: double); cdecl; external WandExport;
procedure DrawPathLineToHorizontalRelative(wand: PDrawingWand); cdecl; external WandExport;

Contradio na declarao
}
procedure DrawPathLineToVerticalAbsolute(wand: PDrawingWand; const y: double); cdecl; external WandExport;
procedure DrawPathLineToVerticalRelative(wand: PDrawingWand; const y: double); cdecl; external WandExport;
procedure DrawPathMoveToAbsolute(wand: PDrawingWand; const x, y: double); cdecl; external WandExport;
procedure DrawPathMoveToRelative(wand: PDrawingWand; const x, y: double); cdecl; external WandExport;
procedure DrawPathStart(wand: PDrawingWand); cdecl; external WandExport;
procedure DrawPoint(wand: PDrawingWand; const x, y: double); cdecl; external WandExport;
procedure DrawPolygon(wand: PDrawingWand; const number_coordinates: culong; const coordinates: PPointInfo
 ); cdecl; external WandExport;
procedure DrawPolyline(wand: PDrawingWand; const number_coordinates: culong; const coordinates: PPointInfo
 ); cdecl; external WandExport;
procedure DrawPopClipPath(wand: PDrawingWand); cdecl; external WandExport;
procedure DrawPopDefs(wand: PDrawingWand); cdecl; external WandExport;
procedure DrawPushClipPath(wand: PDrawingWand; clip_path_id: PChar); cdecl; external WandExport;
procedure DrawPushDefs(wand: PDrawingWand); cdecl; external WandExport;
procedure DrawRectangle(wand: PDrawingWand; const x1, y1, x2, y2: double); cdecl; external WandExport;
procedure DrawRotate(wand: PDrawingWand; const degrees: double); cdecl; external WandExport;
procedure DrawRoundRectangle(wand: PDrawingWand; const x1, y1, x2, y2, rx, ry: double); cdecl; external WandExport;
procedure DrawScale(wand: PDrawingWand; const x, y: double); cdecl; external WandExport;
procedure DrawSetClipRule(wand: PDrawingWand; const fill_rule: FillRule); cdecl; external WandExport;
procedure DrawSetClipUnits(wand: PDrawingWand; const clip_units: ClipPathUnits); cdecl; external WandExport;
procedure DrawSetFillColor(wand: PDrawingWand; const fill_wand: PPixelWand); cdecl; external WandExport;
procedure DrawSetFillAlpha(wand: PDrawingWand; const fill_opacity: double); cdecl; external WandExport;
procedure DrawSetFillRule(wand: PDrawingWand; const fill_rule: FillRule); cdecl; external WandExport;
procedure DrawSetFontSize(wand: PDrawingWand; const pointsize: double); cdecl; external WandExport;
procedure DrawSetFontStretch(wand: PDrawingWand; const font_stretch: StretchType); cdecl; external WandExport;
procedure DrawSetFontStyle(wand: PDrawingWand; const style: StyleType); cdecl; external WandExport;
procedure DrawSetFontWeight(wand: PDrawingWand; const font_weight: culong); cdecl; external WandExport;
procedure DrawSetGravity(wand: PDrawingWand; const gravity: GravityType); cdecl; external WandExport;
procedure DrawSkewX(wand: PDrawingWand; const degrees: double); cdecl; external WandExport;
procedure DrawSkewY(wand: PDrawingWand; const degrees: double); cdecl; external WandExport;
procedure DrawSetStrokeAntialias(wand: PDrawingWand; const stroke_antialias: MagickBooleanType
 ); cdecl; external WandExport;
procedure DrawSetStrokeColor(wand: PDrawingWand; const stroke_wand: PPixelWand); cdecl; external WandExport;
procedure DrawSetStrokeDashOffset(wand: PDrawingWand; const dash_offset: double); cdecl; external WandExport;
procedure DrawSetStrokeLineCap(wand: PDrawingWand; const linecap_: LineCap); cdecl; external WandExport;
procedure DrawSetStrokeLineJoin(wand: PDrawingWand; const linejoin_: LineJoin); cdecl; external WandExport;
procedure DrawSetStrokeMiterLimit(wand: PDrawingWand; const miterlimit: culong); cdecl; external WandExport;
procedure DrawSetStrokeAlpha(wand: PDrawingWand; const stroke_opacity: double); cdecl; external WandExport;
procedure DrawSetStrokeWidth(wand: PDrawingWand; const troke_width: double); cdecl; external WandExport;
procedure DrawSetTextAlignment(wand: PDrawingWand; const alignment: AlignType); cdecl; external WandExport;
procedure DrawSetTextAntialias(wand: PDrawingWand; const text_antialias: MagickBooleanType
 ); cdecl; external WandExport;
procedure DrawSetTextDecoration(wand: PDrawingWand; const decoration: DecorationType); cdecl; external WandExport;
procedure DrawSetTextEncoding(wand: PDrawingWand; const encoding: PChar); cdecl; external WandExport;
procedure DrawSetTextUnderColor(wand: PDrawingWand; const under_wand: PPixelWand); cdecl; external WandExport;
procedure DrawSetViewbox(wand: PDrawingWand; x1, y1, x2, y2: culong); cdecl; external WandExport;
procedure DrawTranslate(wand: PDrawingWand; const x, y: double); cdecl; external WandExport;

{
  Deprecated.
}
{typedef struct _DrawingWand
  *DrawContext;

extern WandExport double
  DrawGetFillOpacity(const wand: PDrawingWand),
  DrawGetStrokeOpacity(const wand: PDrawingWand);

extern WandExport DrawInfo
  *DrawPeekGraphicWand(const wand: PDrawingWand);

extern WandExport void
  DrawPopGraphicContext(DrawingWand *),
  DrawPushGraphicContext(DrawingWand *),
  DrawSetFillOpacity(DrawingWand *,const double),
  DrawSetStrokeOpacity(DrawingWand *,const double);}
