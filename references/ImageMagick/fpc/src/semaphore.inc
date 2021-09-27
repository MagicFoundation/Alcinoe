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

  ImageMagick methods to lock and unlock semaphores.
}
#ifndef _MAGICK_SEMAPHORE_H
#define _MAGICK_SEMAPHORE_H
#endif

typedef struct SemaphoreInfo
  SemaphoreInfo;

extern MagickExport MagickBooleanType
  LockSemaphoreInfo(SemaphoreInfo *),
  UnlockSemaphoreInfo(SemaphoreInfo *);

extern MagickExport SemaphoreInfo
  *AllocateSemaphoreInfo(void),
  *DestroySemaphoreInfo(SemaphoreInfo *);

extern MagickExport void
  AcquireSemaphoreInfo(SemaphoreInfo **),
  DestroySemaphore(void),
  InitializeSemaphore(void),
  RelinquishSemaphoreInfo(SemaphoreInfo *);
