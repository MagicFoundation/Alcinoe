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

  ImageMagick image methods.
}

type
  ChannelStatistics = record
    depth: culong;

    minima, maxima: Quantum;

    mean, standard_deviation: double;

    scale: culong;
  end;
  
  PChannelStatistics = ^ChannelStatistics;

{extern MagickExport ChannelStatistics
  *GetImageChannelStatistics(const Image *,ExceptionInfo *);

extern MagickExport MagickBooleanType
  GetImageChannelExtrema(const Image *,const ChannelType,unsigned long *,
    unsigned long *,ExceptionInfo *),
  GetImageExtrema(const Image *,unsigned long *,unsigned long *,
    ExceptionInfo *),
  GetImageChannelMean(const Image *,const ChannelType,double *,double *,
    ExceptionInfo *),
  GetImageMean(const Image *,double *,double *,ExceptionInfo *),
  SetImageChannelDepth(Image *,const ChannelType,const unsigned long),
  SetImageDepth(Image *,const unsigned long);

extern MagickExport RectangleInfo
  GetImageBoundingBox(const Image *,ExceptionInfo *exception);

extern MagickExport unsigned long
  GetImageChannelDepth(const Image *,const ChannelType,ExceptionInfo *),
  GetImageDepth(const Image *,ExceptionInfo *),
  GetImageQuantumDepth(const Image *,const MagickBooleanType);}

