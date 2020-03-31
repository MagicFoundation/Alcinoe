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

  ImageMagick image effects methods.
}

type
  NoiseType = ( 
  UndefinedNoise,
  UniformNoise,
  GaussianNoise,
  MultiplicativeGaussianNoise,
  ImpulseNoise,
  LaplacianNoise,
  PoissonNoise
 );

{extern MagickExport Image
  *AdaptiveThresholdImage(const Image *,const unsigned long,const unsigned long,    const long,ExceptionInfo *),
  *AddNoiseImage(const Image *,const NoiseType,ExceptionInfo *),
  *AddNoiseImageChannel(const Image *,const ChannelType,const NoiseType,
    ExceptionInfo *),
  *BlurImage(const Image *,const double,const double,ExceptionInfo *),
  *BlurImageChannel(const Image *,const ChannelType,const double,const double,
    ExceptionInfo *),
  *DespeckleImage(const Image *,ExceptionInfo *),
  *EdgeImage(const Image *,const double,ExceptionInfo *),
  *EmbossImage(const Image *,const double,const double,ExceptionInfo *),
  *GaussianBlurImage(const Image *,const double,const double,ExceptionInfo *),
  *GaussianBlurImageChannel(const Image *,const ChannelType,const double,
    const double,ExceptionInfo *),
  *MedianFilterImage(const Image *,const double,ExceptionInfo *),
  *MotionBlurImage(const Image *,const double,const double,const double,
    ExceptionInfo *),
  *PreviewImage(const Image *,const PreviewType,ExceptionInfo *),
  *RadialBlurImage(const Image *,const double,ExceptionInfo *),
  *RadialBlurImageChannel(const Image *,const ChannelType,const double,
    ExceptionInfo *),
  *ReduceNoiseImage(const Image *,const double,ExceptionInfo *),
  *ShadeImage(const Image *,const MagickBooleanType,const double,const double,
    ExceptionInfo *),
  *SharpenImage(const Image *,const double,const double,ExceptionInfo *),
  *SharpenImageChannel(const Image *,const ChannelType,const double,
    const double,ExceptionInfo *),
  *SpreadImage(const Image *,const double,ExceptionInfo *),
  *UnsharpMaskImage(const Image *,const double,const double,const double,
    const double,ExceptionInfo *),
  *UnsharpMaskImageChannel(const Image *,const ChannelType,const double,
    const double,const double,const double,ExceptionInfo *);

extern MagickExport MagickBooleanType
  BlackThresholdImage(Image *,const char *),
  BilevelImage(Image *,const double),
  BilevelImageChannel(Image *,const ChannelType,const double),
  RandomThresholdImage(Image *,const char *,ExceptionInfo *),
  RandomThresholdImageChannel(Image *,const ChannelType,const char *,
    ExceptionInfo *),
  WhiteThresholdImage(Image *,const char *);}

