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

  ImageMagick image quantization methods.
}

type
  QuantizeInfo = record
    number_colors: culong;

    tree_depth: culong; 

    dither: MagickBooleanType;

    colorspace: ColorspaceType;

    measure_error: MagickBooleanType;

    signature: culong; 
  end;
	PQuantizeInfo = ^QuantizeInfo;

{extern MagickExport MagickBooleanType
  GetImageQuantizeError(Image *),
  MapImage(Image *,const Image *,const MagickBooleanType),
  MapImages(Image *,const Image *,const MagickBooleanType),
  OrderedDitherImage(Image *),
  PosterizeImage(Image *,const unsigned long,const MagickBooleanType),
  QuantizeImage(const QuantizeInfo *,Image *),
  QuantizeImages(const QuantizeInfo *,Image *);

extern MagickExport QuantizeInfo
  *CloneQuantizeInfo(const QuantizeInfo *),
  *DestroyQuantizeInfo(QuantizeInfo *);

extern MagickExport void
  CompressImageColormap(Image *),
  GetQuantizeInfo(QuantizeInfo *);}
