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

  ImageMagick image f/x methods.
}

type
 MagickEvaluateOperator = (
  UndefinedEvaluateOperator,
  AddEvaluateOperator,
  AndEvaluateOperator,
  DivideEvaluateOperator,
  LeftShiftEvaluateOperator,
  MaxEvaluateOperator,
  MinEvaluateOperator,
  MultiplyEvaluateOperator,
  OrEvaluateOperator,
  RightShiftEvaluateOperator,
  SetEvaluateOperator,
  SubtractEvaluateOperator,
  XorEvaluateOperator
 );

{extern MagickExport Image
  *CharcoalImage(const Image *,const double,const double,ExceptionInfo *),
  *ColorizeImage(const Image *,const char *,const PixelPacket,ExceptionInfo *),
  *ConvolveImage(const Image *,const unsigned long,const double *,
    ExceptionInfo *),
  *ConvolveImageChannel(const Image *,const ChannelType,const unsigned long,
    const double *,ExceptionInfo *),
  *FxImage(const Image *,const char *,ExceptionInfo *),
  *FxImageChannel(const Image *,const ChannelType,const char *,ExceptionInfo *),
  *ImplodeImage(const Image *,const double,ExceptionInfo *),
  *MorphImages(const Image *,const unsigned long,ExceptionInfo *),
  *OilPaintImage(const Image *,const double,ExceptionInfo *),
  *SepiaToneImage(const Image *,const double,ExceptionInfo *),
  *ShadowImage(const Image *,const double,const double,const long,const long,
    ExceptionInfo *),
  *SteganoImage(const Image *,const Image *,ExceptionInfo *),
  *StereoImage(const Image *,const Image *,ExceptionInfo *),
  *SwirlImage(const Image *,double,ExceptionInfo *),
  *TintImage(const Image *,const char *,const PixelPacket,ExceptionInfo *),
  *WaveImage(const Image *,const double,const double,ExceptionInfo *);

extern MagickExport MagickBooleanType
  EvaluateImage(Image *,const MagickEvaluateOperator,const double,
    ExceptionInfo *),
  EvaluateImageChannel(Image *,const ChannelType,const MagickEvaluateOperator,
    const double,ExceptionInfo *),
  SolarizeImage(Image *,const double);}

