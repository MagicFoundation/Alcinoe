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

  ImageMagick image type methods.
}

type
 StretchType = (
  UndefinedStretch,
  NormalStretch,
  UltraCondensedStretch,
  ExtraCondensedStretch,
  CondensedStretch,
  SemiCondensedStretch,
  SemiExpandedStretch,
  ExpandedStretch,
  ExtraExpandedStretch,
  UltraExpandedStretch,
  AnyStretch
 );

type
 StyleType = (
  UndefinedStyle,
  NormalStyle,
  ItalicStyle,
  ObliqueStyle,
  AnyStyle
 );

type
  TypeInfo = record
    face: culong;

    path,
    name,
    description,
    family: PChar;

    style: StyleType;

    stretch: StretchType;

    weight: culong;

    encoding,
    foundry,
    format,
    metrics,
    glyphs: PChar;

    stealth: MagickBooleanType;

    previous, next: Pointer;
{  struct _TypeInfo
    *previous,
    *next;  /* deprecated, use GetTypeInfoList() */}

    signature: culong;
  end;

{function GetTypeList(const char *,unsigned long *,ExceptionInfo *): PPChar; external MagickExport;

extern MagickExport MagickBooleanType
  ListTypeInfo(FILE *,ExceptionInfo *);

extern MagickExport const TypeInfo
  *GetTypeInfo(const char *,ExceptionInfo *),
  *GetTypeInfoByFamily(const char *,const StyleType,const StretchType,
    const unsigned long,ExceptionInfo *),
  **GetTypeInfoList(const char *,unsigned long *,ExceptionInfo *);

MagickExport void
  DestroyTypeList(void);}


