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

  ImageMagick image constitute methods.
}

//#include <magick/constitute.h>

type
  QuantumType = (
  UndefinedQuantum,
  AlphaQuantum,
  BlackQuantum,
  BlueQuantum,
  CMYKAQuantum,
  CMYKQuantum,
  CyanQuantum,
  GrayAlphaQuantum,
  GrayQuantum,
  GreenQuantum,
  IndexAlphaQuantum,
  IndexQuantum,
  MagentaQuantum,
  OpacityQuantum,
  RedQuantum,
  RGBAQuantum,
  RGBOQuantum,
  RGBQuantum,
  YellowQuantum,
  GrayPadQuantum,
  RGBPadQuantum
 );

type
  LongPixelPacket = record
    red,
    green,
    blue,
    opacity,
    index: culong;
  end;

type
  MagickPixelPacket = record
    storage_class: ClassType; // Added after 6.2

    colorspace: ColorspaceType;

    matte: MagickBooleanType;

    fuzz: double;

    depth: culong;

    red,
    green,
    blue,
    opacity,
    index: MagickRealType;
  end;

  PMagickPixelPacket = ^MagickPixelPacket;

{extern MagickExport MagickBooleanType
  ExportImagePixels(const Image *,const long,const long,const unsigned long,
    const unsigned long,const char *,const StorageType,void *,ExceptionInfo *),
  ExportQuantumPixels(Image *,const QuantumType,const size_t,
    const unsigned char *),
  ImportImagePixels(Image *,const long,const long,const unsigned long,
    const unsigned long,const char *,const StorageType,const void *),
  ImportQuantumPixels(Image *,const QuantumType,const size_t,unsigned char *);}

