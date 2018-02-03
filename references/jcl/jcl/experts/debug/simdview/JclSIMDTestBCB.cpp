//{**************************************************************************************************}
//{                                                                                                  }
//{ Project JEDI Code Library (JCL)                                                                  }
//{                                                                                                  }
//{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
//{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
//{ License at http://www.mozilla.org/MPL/                                                           }
//{                                                                                                  }
//{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
//{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
//{ and limitations under the License.                                                               }
//{                                                                                                  }
//{ The Original Code is: JvSIMDTest.dpr, released on 2004-10-11.                                    }
//{                                                                                                  }
//{ The Initial Developer of the Original Code is Florent Ouchet                                     }
//{ [ouchet dott florent att laposte dott net]                                                       }
//{ All Rights Free.                                                                                 }
//{                                                                                                  }
//{ You may retrieve the latest version of this file at the Project JEDI's JCL home page,            }
//{ located at https://github.com/project-jedi/jcl                                                   }
//{                                                                                                  }
//{**************************************************************************************************}
//{                                                                                                  }
//{ Last modified: $Date::                                                                         $ }
//{ Revision:      $Rev::                                                                          $ }
//{ Author:        $Author::                                                                       $ }
//{                                                                                                  }
//{**************************************************************************************************}

#pragma hdrstop

#include <iostream>
#include <iomanip>

//---------------------------------------------------------------------------

#if __BORLANDC__ == 1380
#define BCB6
#endif

#if __BORLANDC__ == 1360
#define BCB5
#endif

#ifdef BCB5
#define COMPILER5_UP
#define COMPILER5
#endif

#ifdef BCB6
#define COMPILER6_UP
#define COMPILER5_UP
#define COMPILER6
#endif

#pragma argsused
int main (int argc, char **argv)
{
  using namespace std;
  float Values[4];
  int Index, ErrorCode;
  char Line[256];

  printf("Streaming SIMD Extensions of Intel Pentium and AMD Athlon processors\n");
  printf("By Ouchet Florent <outchy_at_users.sourceforge.net>\n");
  printf("Released 2004,14,3\n");
  printf("All rights free\n\n");

  for (Index=0; Index<4; Index++) {
    do {
      printf("Enter the floating point value %d : ",Index);
      gets(Line);
      ErrorCode = sscanf(Line,"%f",Values+Index);
    } while (ErrorCode!=1);
  }

  printf("\nCheck values :\n");
  for (Index=0; Index<4; Index++)
    printf("Value %d is : %f\n",Index,Values[Index]);

  printf("\nStarting computations : Values*2 ...");
  __asm {
    // breakpoint here
    // hit ctrl+alt+D or go to View/Debug window and open the last item
    // these instructions operate on 4-packed-single-precision floating point values
    // so you should view these registers has single values
    LEA      EAX,  Values
#ifdef COMPILER6_UP
    movups   xmm0, [eax]      // moving Values to xmm0
    addps    xmm0, xmm0       // xmm0 <- xmm0 + xmm0
    movups   [eax], xmm0      // moving xmm0 to Values
#else
    DB       0Fh, 10h, 00h    // movups xmm0, [eax]
    DB       0Fh, 58h, 0C0h   // addps xmm0, xmm0
    DB       0Fh, 11h, 00h    // movups [eax], xmm0
#endif
  };
  printf("Computations ended\nNow values are :\n");
  for (Index=0; Index<4; Index++)
    printf("Value %d is : %f\n",Index,Values[Index]);
  printf("\nProgram terminated\n");
  gets(Line);
  return 0;
}
//---------------------------------------------------------------------------
