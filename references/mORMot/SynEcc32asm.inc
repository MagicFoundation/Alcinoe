(*

  32-bit version of easy-ecc for secp256r1 curve for static linking as x86 asm
  used by SynEcc.pas unit e.g. for Kylix; compiled using gcc -g -O1 -c ecc.c
  with some manual enhancements by Arnaud Bouchez to the generated asm 

    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  Using secp256r1 curve from "simple and secure ECDH and ECDSA library"
  Copyright (c) 2013, Kenneth MacKay - BSD 2-clause license
  https://github.com/esxgx/easy-ecc

  *** BEGIN LICENSE BLOCK *****
  Copyright (c) 2013, Kenneth MacKay
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
   * Redistributions of source code must retain the above copyright notice, this
     list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ***** END LICENSE BLOCK *****

  Version 1.18
  - first public release, corresponding to SQLite3 Framework 1.18

*)


procedure _curve_n;
asm
        db      51H, 25H, 63H, 0FCH, 0C2H, 0CAH, 0B9H, 0F3H
        db      84H, 9EH, 17H, 0A7H, 0ADH, 0FAH, 0E6H, 0BCH
        db      0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH
        db      00H, 00H, 00H, 00H, 0FFH, 0FFH, 0FFH, 0FFH
end;

procedure _curve_G;
asm
        db      96H, 0C2H, 98H, 0D8H, 45H, 39H, 0A1H, 0F4H
        db      0A0H, 33H, 0EBH, 2DH, 81H, 7DH, 03H, 77H
        db      0F2H, 40H, 0A4H, 63H, 0E5H, 0E6H, 0BCH, 0F8H
        db      47H, 42H, 2CH, 0E1H, 0F2H, 0D1H, 17H, 6BH
        db      0F5H, 51H, 0BFH, 37H, 68H, 40H, 0B6H, 0CBH
        db      0CEH, 5EH, 31H, 6BH, 57H, 33H, 0CEH, 2BH
        db      16H, 9EH, 0FH, 7CH, 4AH, 0EBH, 0E7H, 8EH
        db      9BH, 7FH, 1AH, 0FEH, 0E2H, 42H, 0E3H, 4FH
end;

procedure _curve_b;
asm
        db      4BH, 60H, 0D2H, 27H, 3EH, 3CH, 0CEH, 3BH
        db      0F6H, 0B0H, 53H, 0CCH, 0B0H, 06H, 1DH, 65H
        db      0BCH, 86H, 98H, 76H, 55H, 0BDH, 0EBH, 0B3H
        db      0E7H, 93H, 3AH, 0AAH, 0D8H, 35H, 0C6H, 5AH
end;

procedure _curve_p;
asm
        db      0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH
        db      0FFH, 0FFH, 0FFH, 0FFH, 00H, 00H, 00H, 00H
        db      00H, 00H, 00H, 00H, 00H, 00H, 00H, 00H
        db      01H, 00H, 00H, 00H, 0FFH, 0FFH, 0FFH, 0FFH
end;

procedure _vli_clear;
asm
        xor     edx, edx
        mov     dword ptr[eax + 00H], edx
        mov     dword ptr[eax + 04H], edx
        mov     dword ptr[eax + 08H], edx
        mov     dword ptr[eax + 0CH], edx
        mov     dword ptr[eax + 10H], edx
        mov     dword ptr[eax + 14H], edx
        mov     dword ptr[eax + 18H], edx
        mov     dword ptr[eax + 1CH], edx
end;

procedure _vli_isZero;
asm
        mov     edx, dword ptr[eax + 00H]
        or      edx, dword ptr[eax + 04H]
        jnz     @003
        mov     edx, dword ptr[eax + 08H]
        or      edx, dword ptr[eax + 0CH]
        jnz     @003
        mov     edx, dword ptr[eax + 10H]
        or      edx, dword ptr[eax + 14H]
        jnz     @003
        mov     edx, dword ptr[eax + 18H]
        or      edx, dword ptr[eax + 1CH]
        jnz     @003
        mov     eax, 1
        ret
@003:   mov     eax, 0
end;

procedure _vli_testBit;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        mov     ebx, eax
        mov     ebp, edx
        shr     ebp, 6
        mov     ecx, edx
        and     ecx, 3FH
        mov     edi, ecx
        shr     edi, 5
        and     edi, 01H
        mov     esi, edi
        xor     esi, 01H
        shl     esi, cl
        shl     edi, cl
        mov     eax, esi
        and     eax, dword ptr[ebx + ebp * 8]
        mov     edx, edi
        and     edx, dword ptr[ebx + ebp * 8 + 4H]
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _vli_numBits;
asm
        push    esi
        push    ebx
        mov     ecx, dword ptr[eax + 1CH]
        mov     edx, 2
        or      ecx, dword ptr[eax + 18H]
        jz      @006
        mov     edx, 4
        jmp     @007
@005:   sub     edx, 1
        cmp     edx,  - 1
        jz      @011
@006:   mov     ecx, dword ptr[eax + edx * 8 + 4H]
        or      ecx, dword ptr[eax + edx * 8]
        jz      @005
        add     edx, 1
        jz      @012
@007:   mov     ecx, dword ptr[eax + edx * 8 - 8H]
        mov     ebx, dword ptr[eax + edx * 8 - 4H]
        mov     eax, ebx
        or      eax, ecx
        jz      @009
        mov     eax, 0
@008:   shrd    ecx, ebx, 1
        shr     ebx, 1
        add     eax, 1
        mov     esi, ebx
        or      esi, ecx
        jnz     @008
        jmp     @010
@009:   mov     eax, 0
@010:   add     edx, 67108863
        shl     edx, 6
        add     edx, eax
        jmp     @012
@011:   mov     edx, 0
@012:   mov     eax, edx
        pop     ebx
        pop     esi
end;

procedure _vli_set;
asm
        mov     ecx, dword ptr[edx + 00H]
        mov     dword ptr[eax + 00H], ecx
        mov     ecx, dword ptr[edx + 04H]
        mov     dword ptr[eax + 04H], ecx
        mov     ecx, dword ptr[edx + 08H]
        mov     dword ptr[eax + 08H], ecx
        mov     ecx, dword ptr[edx + 0CH]
        mov     dword ptr[eax + 0CH], ecx
        mov     ecx, dword ptr[edx + 10H]
        mov     dword ptr[eax + 10H], ecx
        mov     ecx, dword ptr[edx + 14H]
        mov     dword ptr[eax + 14H], ecx
        mov     ecx, dword ptr[edx + 18H]
        mov     dword ptr[eax + 18H], ecx
        mov     ecx, dword ptr[edx + 1CH]
        mov     dword ptr[eax + 1CH], ecx
end;

procedure _vli_cmp;
asm
        mov     ecx, dword ptr[eax + 1CH]
        cmp     ecx, dword ptr[edx + 1CH]
        ja      @a
        jc      @c
        mov     ecx, dword ptr[eax + 18H]
        cmp     ecx, dword ptr[edx + 18H]
        ja      @a
        jc      @c
        mov     ecx, dword ptr[eax + 14H]
        cmp     ecx, dword ptr[edx + 14H]
        ja      @a
        jc      @c
        mov     ecx, dword ptr[eax + 10H]
        cmp     ecx, dword ptr[edx + 10H]
        ja      @a
        jc      @c
        mov     ecx, dword ptr[eax + 0CH]
        cmp     ecx, dword ptr[edx + 0CH]
        ja      @a
        jc      @c
        mov     ecx, dword ptr[eax + 08H]
        cmp     ecx, dword ptr[edx + 08H]
        ja      @a
        jc      @c
        mov     ecx, dword ptr[eax + 04H]
        cmp     ecx, dword ptr[edx + 04H]
        ja      @a
        jc      @c
        mov     ecx, dword ptr[eax + 00H]
        cmp     ecx, dword ptr[edx + 00H]
        ja      @a
        jc      @c
        mov     eax, 0
        ret
@a:     mov     eax, 1
        ret
@c:     mov     eax, -1
end;

procedure _vli_lshift;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 28
        mov     dword ptr[esp + 8H], eax
        mov     dword ptr[esp + 0CH], edx
        mov     ebx, ecx
        mov     dword ptr[esp + 10H], ecx
        mov     ebp, 0
        mov     eax, 0
        mov     edx, 0
        mov     ecx, 64
        mov     edi, ecx
        sub     edi, ebx
        mov     dword ptr[esp + 14H], edi
        mov     dword ptr[esp], eax
        mov     dword ptr[esp + 4H], edx
@024:   mov     eax, dword ptr[esp + 0CH]
        mov     esi, dword ptr[eax + ebp * 8]
        mov     edi, dword ptr[eax + ebp * 8 + 4H]
        mov     eax, esi
        mov     edx, edi
        movzx   ecx, byte ptr[esp + 10H]
        shld    edx, esi, cl
        shl     eax, cl
        test    cl, 20H
        jz      @025
        mov     edx, eax
        xor     eax, eax
@025:   mov     ecx, eax
        mov     ebx, edx
        or      ecx, dword ptr[esp]
        mov     edx, dword ptr[esp + 8H]
        mov     dword ptr[edx + ebp * 8], ecx
        mov     eax, ebx
        or      eax, dword ptr[esp + 4H]
        mov     dword ptr[edx + ebp * 8 + 4H], eax
        mov     eax, esi
        mov     edx, edi
        movzx   ecx, byte ptr[esp + 14H]
        shrd    eax, edi, cl
        shr     edx, cl
        test    cl, 20H
        jz      @026
        mov     eax, edx
        xor     edx, edx
@026:   mov     dword ptr[esp], eax
        mov     dword ptr[esp + 4H], edx
        add     ebp, 1
        cmp     ebp, 4
        jnz     @024
        add     esp, 28
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _vli_rshift1;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 20
        lea     ebx, [eax + 18H]
        mov     dword ptr[esp + 0CH], ebx
        lea     edx, [eax + 20H]
        cmp     eax, edx
        jnc     @028
        mov     edx, 0
        mov     ebx, 0
        mov     ebp, 0
@027:   mov     esi, dword ptr[eax + edx + 18H]
        mov     edi, dword ptr[eax + edx + 1CH]
        mov     dword ptr[esp], esi
        mov     dword ptr[esp + 4H], edi
        shrd    esi, edi, 1
        shr     edi, 1
        or      ebx, esi
        mov     dword ptr[eax + edx + 18H], ebx
        or      ebp, edi
        mov     dword ptr[eax + edx + 1CH], ebp
        mov     ebp, dword ptr[esp]
        shl     ebp, 31
        mov     ebx, 0
        mov     ecx, dword ptr[esp + 0CH]
        add     ecx, edx
        lea     edx, [edx - 8H]
        cmp     ecx, eax
        ja      @027
@028:   add     esp, 20
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _vli_add;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 28
        mov     dword ptr[esp + 0CH], eax
        mov     dword ptr[esp + 10H], edx
        mov     dword ptr[esp + 14H], ecx
        mov     ebp, 0
        mov     eax, 0
        mov     edx, 0
        mov     dword ptr[esp], eax
        mov     dword ptr[esp + 4H], edx
@029:   mov     eax, dword ptr[esp + 10H]
        mov     ecx, dword ptr[eax + ebp * 8]
        mov     ebx, dword ptr[eax + ebp * 8 + 4H]
        mov     eax, dword ptr[esp + 14H]
        mov     esi, ecx
        mov     edi, ebx
        add     esi, dword ptr[eax + ebp * 8]
        adc     edi, dword ptr[eax + ebp * 8 + 4H]
        add     esi, dword ptr[esp]
        adc     edi, dword ptr[esp + 4H]
        mov     eax, ebx
        xor     eax, edi
        mov     edx, ecx
        xor     edx, esi
        or      eax, edx
        jz      @032
        mov     eax, 1
        mov     edx, 0
        cmp     ebx, edi
        ja      @031
        jc      @030
        cmp     ecx, esi
        ja      @031
@030:   mov     eax, 0
        mov     edx, 0
@031:   mov     dword ptr[esp], eax
        mov     dword ptr[esp + 4H], edx
@032:   mov     eax, dword ptr[esp + 0CH]
        mov     dword ptr[eax + ebp * 8], esi
        mov     dword ptr[eax + ebp * 8 + 4H], edi
        add     ebp, 1
        cmp     ebp, 4
        jnz     @029
        mov     eax, dword ptr[esp]
        mov     edx, dword ptr[esp + 4H]
        add     esp, 28
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _vli_sub;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 28
        mov     dword ptr[esp + 0CH], eax
        mov     dword ptr[esp + 10H], edx
        mov     dword ptr[esp + 14H], ecx
        mov     ebp, 0
        mov     eax, 0
        mov     edx, 0
        mov     dword ptr[esp], eax
        mov     dword ptr[esp + 4H], edx
@033:   mov     eax, dword ptr[esp + 10H]
        mov     ecx, dword ptr[eax + ebp * 8]
        mov     ebx, dword ptr[eax + ebp * 8 + 4H]
        mov     eax, dword ptr[esp + 14H]
        mov     esi, ecx
        mov     edi, ebx
        sub     esi, dword ptr[eax + ebp * 8]
        sbb     edi, dword ptr[eax + ebp * 8 + 4H]
        sub     esi, dword ptr[esp]
        sbb     edi, dword ptr[esp + 4H]
        mov     eax, ebx
        xor     eax, edi
        mov     edx, ecx
        xor     edx, esi
        or      eax, edx
        jz      @036
        mov     eax, 1
        mov     edx, 0
        cmp     ebx, edi
        jc      @035
        ja      @034
        cmp     ecx, esi
        jc      @035
@034:   mov     eax, 0
        mov     edx, 0
@035:   mov     dword ptr[esp], eax
        mov     dword ptr[esp + 4H], edx
@036:   mov     eax, dword ptr[esp + 0CH]
        mov     dword ptr[eax + ebp * 8], esi
        mov     dword ptr[eax + ebp * 8 + 4H], edi
        add     ebp, 1
        cmp     ebp, 4
        jnz     @033
        mov     eax, dword ptr[esp]
        mov     edx, dword ptr[esp + 4H]
        add     esp, 28
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _vli_mult;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 84
        mov     dword ptr[esp + 48H], eax
        mov     dword ptr[esp + 4CH], ecx
        mov     dword ptr[esp + 44H],  - 3
        mov     dword ptr[esp], 0
        mov     dword ptr[esp + 4H], 0
        mov     dword ptr[esp + 38H], 0
        mov     dword ptr[esp + 3CH], 0
        mov     dword ptr[esp + 34H], 0
        mov     dword ptr[esp + 40H], edx
@037:   mov     ebp, 0
        mov     eax, dword ptr[esp + 34H]
        cmp     eax, 3
        jbe     @038
        mov     ebx, dword ptr[esp + 44H]
        cmp     ebx, eax
        ja      @047
        cmp     ebx, 3
        ja      @047
        mov     ebp, ebx
@038:   mov     eax, dword ptr[esp + 34H]
        sub     eax, ebp
        mov     ebx, dword ptr[esp + 4CH]
        lea     eax, [ebx + eax * 8]
        mov     edi, eax
        mov     eax, dword ptr[esp + 38H]
        mov     edx, dword ptr[esp + 3CH]
        mov     dword ptr[esp + 20H], eax
        mov     dword ptr[esp + 24H], edx
        mov     dword ptr[esp + 38H], 0
        mov     dword ptr[esp + 3CH], 0
        jmp     @040
@039:   mov     eax, dword ptr[esp + 8H]
        mov     edx, dword ptr[esp + 0CH]
        mov     dword ptr[esp], eax
        mov     dword ptr[esp + 4H], edx
@040:   mov     eax, dword ptr[esp + 40H]
        mov     ecx, dword ptr[eax + ebp * 8]
        mov     esi, dword ptr[eax + ebp * 8 + 4H]
        mov     eax, ecx
        mul     dword ptr[edi]
        mov     dword ptr[esp + 8H], eax
        mov     dword ptr[esp + 0CH], edx
        mov     eax, ecx
        mul     dword ptr[edi + 4H]
        mov     ecx, eax
        mov     ebx, edx
        mov     eax, esi
        mul     dword ptr[edi + 4H]
        mov     dword ptr[esp + 28H], eax
        mov     dword ptr[esp + 2CH], edx
        mov     eax, esi
        mul     dword ptr[edi]
        add     eax, ecx
        adc     edx, ebx
        mov     dword ptr[esp + 10H], eax
        mov     dword ptr[esp + 14H], edx
        mov     edx, dword ptr[esp + 0CH]
        mov     eax, edx
        xor     edx, edx
        add     eax, dword ptr[esp + 10H]
        adc     edx, dword ptr[esp + 14H]
        mov     dword ptr[esp + 10H], eax
        mov     dword ptr[esp + 14H], edx
        cmp     ebx, edx
        jc      @042
        ja      @041
        cmp     ecx, eax
        jbe     @042
@041:   add     dword ptr[esp + 28H], 0
        adc     dword ptr[esp + 2CH], 1
@042:   mov     ebx, dword ptr[esp + 14H]
        mov     ecx, ebx
        xor     ebx, ebx
        add     ecx, dword ptr[esp + 28H]
        adc     ebx, dword ptr[esp + 2CH]
        mov     edx, dword ptr[esp + 8H]
        mov     eax, dword ptr[esp + 10H]
        mov     dword ptr[esp + 18H], edx
        mov     dword ptr[esp + 1CH], eax
        mov     eax, dword ptr[esp + 18H]
        mov     edx, dword ptr[esp + 1CH]
        add     eax, dword ptr[esp]
        adc     edx, dword ptr[esp + 4H]
        mov     dword ptr[esp + 8H], eax
        mov     dword ptr[esp + 0CH], edx
        mov     eax, 1
        mov     edx, 0
        mov     esi, dword ptr[esp + 4H]
        cmp     dword ptr[esp + 0CH], esi
        jc      @044
        ja      @043
        mov     esi, dword ptr[esp]
        cmp     dword ptr[esp + 8H], esi
        jc      @044
@043:   mov     eax, 0
        mov     edx, 0
@044:   add     eax, ecx
        adc     edx, ebx
        add     dword ptr[esp + 20H], eax
        adc     dword ptr[esp + 24H], edx
        mov     eax, 1
        mov     edx, 0
        cmp     dword ptr[esp + 24H], ebx
        jc      @046
        ja      @045
        cmp     dword ptr[esp + 20H], ecx
        jc      @046
@045:   mov     eax, 0
        mov     edx, 0
@046:   add     dword ptr[esp + 38H], eax
        adc     dword ptr[esp + 3CH], edx
        add     ebp, 1
        sub     edi, 8
        cmp     ebp, dword ptr[esp + 34H]
        ja      @048
        cmp     ebp, 3
        jbe     @039
        mov     esi, dword ptr[esp + 8H]
        mov     edi, dword ptr[esp + 0CH]
        jmp     @049
@047:   mov     esi, dword ptr[esp]
        mov     edi, dword ptr[esp + 4H]
        mov     eax, dword ptr[esp + 38H]
        mov     edx, dword ptr[esp + 3CH]
        mov     dword ptr[esp + 20H], eax
        mov     dword ptr[esp + 24H], edx
        mov     dword ptr[esp + 38H], 0
        mov     dword ptr[esp + 3CH], 0
        jmp     @049
@048:   mov     esi, dword ptr[esp + 8H]
        mov     edi, dword ptr[esp + 0CH]
@049:   mov     ebx, dword ptr[esp + 48H]
        mov     eax, dword ptr[esp + 34H]
        mov     dword ptr[ebx + eax * 8], esi
        mov     dword ptr[ebx + eax * 8 + 4H], edi
        add     eax, 1
        mov     dword ptr[esp + 34H], eax
        mov     ebx, dword ptr[esp + 20H]
        mov     esi, dword ptr[esp + 24H]
        mov     dword ptr[esp], ebx
        mov     dword ptr[esp + 4H], esi
        mov     ebx, dword ptr[esp + 44H]
        lea     ebx, [ebx + 1H]
        mov     dword ptr[esp + 44H], ebx
        cmp     eax, 7
        jne     @037
        mov     ebx, dword ptr[esp + 48H]
        mov     eax, dword ptr[esp + 20H]
        mov     edx, dword ptr[esp + 24H]
        mov     dword ptr[ebx + 38H], eax
        mov     dword ptr[ebx + 3CH], edx
        add     esp, 84
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _vli_modAdd;
asm
        push    esi
        push    ebx
        sub     esp, 4
        mov     ebx, eax
        mov     esi, dword ptr[esp + 10H]
        call    _vli_add
        or      edx, eax
        jnz     @050
        mov     edx, esi
        mov     eax, ebx
        call    _vli_cmp
        test    eax, eax
        js      @051
@050:   mov     ecx, esi
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_sub
@051:   add     esp, 4
        pop     ebx
        pop     esi
end;

procedure _vli_modSub;
asm
        push    ebx
        mov     ebx, eax
        call    _vli_sub
        or      edx, eax
        jz      @052
        mov     ecx, dword ptr[esp + 8H]
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_add
@052:   pop     ebx
end;

procedure _vli_mmod_fast;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 36
        mov     ebx, eax
        mov     edi, edx
        call    _vli_set
        mov     dword ptr[esp], 0
        mov     dword ptr[esp + 4H], 0
        mov     dword ptr[esp + 8H], 0
        mov     eax, dword ptr[edi + 2CH]
        mov     dword ptr[esp + 0CH], eax
        mov     eax, dword ptr[edi + 30H]
        mov     edx, dword ptr[edi + 34H]
        mov     dword ptr[esp + 10H], eax
        mov     dword ptr[esp + 14H], edx
        mov     eax, dword ptr[edi + 38H]
        mov     edx, dword ptr[edi + 3CH]
        mov     dword ptr[esp + 18H], eax
        mov     dword ptr[esp + 1CH], edx
        mov     ecx, 1
        mov     edx, esp
        mov     eax, esp
        call    _vli_lshift
        mov     esi, eax
        mov     ecx, esp
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_add
        add     esi, eax
        mov     eax, dword ptr[edi + 30H]
        mov     ecx, dword ptr[edi + 34H]
        mov     dword ptr[esp + 8H], 0
        mov     dword ptr[esp + 0CH], eax
        mov     edx, dword ptr[edi + 38H]
        mov     eax, dword ptr[edi + 3CH]
        mov     dword ptr[esp + 10H], ecx
        mov     dword ptr[esp + 14H], edx
        mov     dword ptr[esp + 18H], eax
        mov     dword ptr[esp + 1CH], 0
        mov     ecx, 1
        mov     edx, esp
        mov     eax, esp
        call    _vli_lshift
        add     esi, eax
        mov     ecx, esp
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_add
        add     esi, eax
        mov     eax, dword ptr[edi + 20H]
        mov     edx, dword ptr[edi + 24H]
        mov     dword ptr[esp], eax
        mov     dword ptr[esp + 4H], edx
        mov     eax, dword ptr[edi + 28H]
        mov     dword ptr[esp + 8H], eax
        mov     dword ptr[esp + 0CH], 0
        mov     dword ptr[esp + 10H], 0
        mov     dword ptr[esp + 14H], 0
        mov     eax, dword ptr[edi + 38H]
        mov     edx, dword ptr[edi + 3CH]
        mov     dword ptr[esp + 18H], eax
        mov     dword ptr[esp + 1CH], edx
        mov     ecx, esp
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_add
        add     esi, eax
        mov     ecx, dword ptr[edi + 24H]
        mov     edx, dword ptr[edi + 28H]
        mov     eax, dword ptr[edi + 2CH]
        mov     dword ptr[esp], ecx
        mov     dword ptr[esp + 4H], edx
        mov     ecx, dword ptr[edi + 34H]
        mov     dword ptr[esp + 8H], eax
        mov     dword ptr[esp + 0CH], ecx
        mov     eax, dword ptr[edi + 38H]
        mov     edx, dword ptr[edi + 3CH]
        mov     dword ptr[esp + 10H], eax
        mov     dword ptr[esp + 14H], edx
        mov     dword ptr[esp + 18H], ecx
        mov     eax, dword ptr[edi + 20H]
        mov     dword ptr[esp + 1CH], eax
        mov     ecx, esp
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_add
        add     esi, eax
        mov     eax, dword ptr[edi + 28H]
        mov     ecx, dword ptr[edi + 30H]
        mov     edx, dword ptr[edi + 34H]
        mov     ebp, dword ptr[edi + 2CH]
        mov     dword ptr[esp], ebp
        mov     dword ptr[esp + 4H], ecx
        mov     dword ptr[esp + 8H], edx
        mov     dword ptr[esp + 0CH], 0
        mov     dword ptr[esp + 10H], 0
        mov     dword ptr[esp + 14H], 0
        mov     edx, dword ptr[edi + 20H]
        mov     dword ptr[esp + 18H], edx
        mov     dword ptr[esp + 1CH], eax
        mov     ecx, esp
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_sub
        sub     esi, eax
        mov     eax, dword ptr[edi + 30H]
        mov     edx, dword ptr[edi + 34H]
        mov     dword ptr[esp], eax
        mov     dword ptr[esp + 4H], edx
        mov     eax, dword ptr[edi + 38H]
        mov     edx, dword ptr[edi + 3CH]
        mov     dword ptr[esp + 8H], eax
        mov     dword ptr[esp + 0CH], edx
        mov     dword ptr[esp + 10H], 0
        mov     dword ptr[esp + 14H], 0
        mov     edx, dword ptr[edi + 24H]
        mov     eax, dword ptr[edi + 2CH]
        mov     dword ptr[esp + 18H], edx
        mov     dword ptr[esp + 1CH], eax
        mov     ecx, esp
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_sub
        sub     esi, eax
        mov     eax, dword ptr[edi + 30H]
        mov     ecx, dword ptr[edi + 34H]
        mov     edx, dword ptr[edi + 38H]
        mov     dword ptr[esp], ecx
        mov     dword ptr[esp + 4H], edx
        mov     edx, dword ptr[edi + 20H]
        mov     ecx, dword ptr[edi + 24H]
        mov     ebp, dword ptr[edi + 3CH]
        mov     dword ptr[esp + 8H], ebp
        mov     dword ptr[esp + 0CH], edx
        mov     edx, dword ptr[edi + 28H]
        mov     dword ptr[esp + 10H], ecx
        mov     dword ptr[esp + 14H], edx
        mov     dword ptr[esp + 18H], 0
        mov     dword ptr[esp + 1CH], eax
        mov     ecx, esp
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_sub
        sub     esi, eax
        mov     eax, dword ptr[edi + 38H]
        mov     edx, dword ptr[edi + 3CH]
        mov     dword ptr[esp], eax
        mov     dword ptr[esp + 4H], edx
        mov     dword ptr[esp + 8H], 0
        mov     eax, dword ptr[edi + 24H]
        mov     dword ptr[esp + 0CH], eax
        mov     eax, dword ptr[edi + 28H]
        mov     edx, dword ptr[edi + 2CH]
        mov     dword ptr[esp + 10H], eax
        mov     dword ptr[esp + 14H], edx
        mov     dword ptr[esp + 18H], 0
        mov     eax, dword ptr[edi + 34H]
        mov     dword ptr[esp + 1CH], eax
        mov     ecx, esp
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_sub
        sub     esi, eax
        jns     @055
@053:   mov     ecx, offset _curve_p
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_add
        add     eax, esi
        mov     esi, eax
        test    eax, eax
        js      @053
        jmp     @056
@054:   mov     ecx, offset _curve_p
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_sub
        sub     esi, eax
@055:   test    esi, esi
        jnz     @054
        mov     edx, ebx
        mov     eax, offset _curve_p
        call    _vli_cmp
        cmp     eax, 1
        jnz     @054
@056:   add     esp, 36
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _vli_modMult_fast;
asm
        push    ebx
        sub     esp, 64
        mov     ebx, eax
        mov     eax, esp
        call    _vli_mult
        mov     edx, esp
        mov     eax, ebx
        call    _vli_mmod_fast
        add     esp, 64
        pop     ebx
end;

procedure _vli_modInv;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 132
        mov     esi, eax
        mov     edi, edx
        mov     ebx, ecx
        mov     eax, edx
        call    _vli_isZero
        test    eax, eax
        jz      @057
        mov     eax, esi
        call    _vli_clear
        jmp     @069
@057:   mov     edx, edi
        lea     eax, [esp + 60H]
        call    _vli_set
        mov     edx, ebx
        lea     eax, [esp + 40H]
        call    _vli_set
        lea     eax, [esp + 20H]
        call    _vli_clear
        mov     dword ptr[esp + 20H], 1
        mov     dword ptr[esp + 24H], 0
        mov     eax, esp
        call    _vli_clear
        jmp     @064
@058:   test    byte ptr[esp + 60H], 01H
        jnz     @059
        lea     eax, [esp + 60H]
        call    _vli_rshift1
        test    byte ptr[esp + 20H], 01H
        je      @065
        mov     ecx, ebx
        lea     edx, [esp + 20H]
        mov     eax, edx
        call    _vli_add
        mov     edi, eax
        mov     ebp, edx
        lea     eax, [esp + 20H]
        call    _vli_rshift1
        or      ebp, edi
        je      @064
        or      dword ptr[esp + 38H], 00H
        or      dword ptr[esp + 3CH], 80000000H
        jmp     @064
@059:   test    byte ptr[esp + 40H], 01H
        jnz     @060
        lea     eax, [esp + 40H]
        call    _vli_rshift1
        test    byte ptr[esp], 01H
        je      @066
        mov     ecx, ebx
        mov     edx, esp
        mov     eax, esp
        call    _vli_add
        mov     edi, eax
        mov     ebp, edx
        mov     eax, esp
        call    _vli_rshift1
        or      ebp, edi
        je      @064
        or      dword ptr[esp + 18H], 00H
        or      dword ptr[esp + 1CH], 80000000H
        jmp     @064
@060:   test    eax, eax
        jle     @062
        lea     ecx, [esp + 40H]
        lea     edx, [esp + 60H]
        mov     eax, edx
        call    _vli_sub
        lea     eax, [esp + 60H]
        call    _vli_rshift1
        mov     edx, esp
        lea     eax, [esp + 20H]
        call    _vli_cmp
        test    eax, eax
        jns     @061
        mov     ecx, ebx
        lea     edx, [esp + 20H]
        mov     eax, edx
        call    _vli_add
@061:   mov     ecx, esp
        lea     edx, [esp + 20H]
        mov     eax, edx
        call    _vli_sub
        test    byte ptr[esp + 20H], 01H
        je      @067
        mov     ecx, ebx
        lea     edx, [esp + 20H]
        mov     eax, edx
        call    _vli_add
        mov     edi, eax
        mov     ebp, edx
        lea     eax, [esp + 20H]
        call    _vli_rshift1
        or      ebp, edi
        jz      @064
        or      dword ptr[esp + 38H], 00H
        or      dword ptr[esp + 3CH], 80000000H
        jmp     @064
@062:   lea     ecx, [esp + 60H]
        lea     edx, [esp + 40H]
        mov     eax, edx
        call    _vli_sub
        lea     eax, [esp + 40H]
        call    _vli_rshift1
        lea     edx, [esp + 20H]
        mov     eax, esp
        call    _vli_cmp
        test    eax, eax
        jns     @063
        mov     ecx, ebx
        mov     edx, esp
        mov     eax, esp
        call    _vli_add
@063:   lea     ecx, [esp + 20H]
        mov     edx, esp
        mov     eax, esp
        call    _vli_sub
        test    byte ptr[esp], 01H
        jz      @068
        mov     ecx, ebx
        mov     edx, esp
        mov     eax, esp
        call    _vli_add
        mov     edi, eax
        mov     ebp, edx
        mov     eax, esp
        call    _vli_rshift1
        or      ebp, edi
        jz      @064
        or      dword ptr[esp + 18H], 00H
        or      dword ptr[esp + 1CH], 80000000H
@064:   lea     edx, [esp + 40H]
        lea     eax, [esp + 60H]
        call    _vli_cmp
        test    eax, eax
        jne     @058
        lea     edx, [esp + 20H]
        mov     eax, esi
        call    _vli_set
        jmp     @069
@065:   lea     eax, [esp + 20H]
        call    _vli_rshift1
        jmp     @064
@066:   mov     eax, esp
        call    _vli_rshift1
        jmp     @064
@067:   lea     eax, [esp + 20H]
        call    _vli_rshift1
        jmp     @064
@068:   mov     eax, esp
        call    _vli_rshift1
        jmp     @064
@069:   add     esp, 132
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _EccPoint_isZero;
asm
        push    ebx
        mov     ebx, eax
        call    _vli_isZero
        test    eax, eax
        jz      @070
        lea     eax, [ebx + 20H]
        call    _vli_isZero
        test    eax, eax
        setne   al
        movzx   eax, al
@070:   pop     ebx
end;

procedure _ecc_bytesnative;
asm
        push    ebx
        mov     ebx, dword ptr [edx + 18H]
        mov     ecx, dword ptr [edx + 1CH]
        bswap   ebx
        bswap   ecx
        mov     dword ptr [eax + 00H], ecx
        mov     dword ptr [eax + 04H], ebx
        mov     ebx, dword ptr [edx + 10H]
        mov     ecx, dword ptr [edx + 14H]
        bswap   ebx
        bswap   ecx
        mov     dword ptr [eax + 08H], ecx
        mov     dword ptr [eax + 0CH], ebx
        mov     ebx, dword ptr [edx + 08H]
        mov     ecx, dword ptr [edx + 0CH]
        bswap   ebx
        bswap   ecx
        mov     dword ptr [eax + 10H], ecx
        mov     dword ptr [eax + 14H], ebx
        mov     ebx, dword ptr [edx + 00H]
        mov     ecx, dword ptr [edx + 04H]
        bswap   ebx
        bswap   ecx
        mov     dword ptr [eax + 18H], ecx
        mov     dword ptr [eax + 1CH], ebx
        pop     ebx
end;

procedure _vli_modMult;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 140
        mov     ebx, eax
        mov     ebp, edx
        mov     dword ptr[esp + 4H], ecx
        mov     esi, dword ptr[esp + 0A0H]
        mov     eax, esi
        call    _vli_numBits
        mov     edi, eax
        mov     ecx, dword ptr[esp + 4H]
        mov     edx, ebp
        lea     eax, [esp + 48H]
        call    _vli_mult
        lea     eax, [esp + 68H]
        call    _vli_numBits
        lea     ebp, [eax + 100H]
        test    eax, eax
        jnz     @073
        lea     eax, [esp + 48H]
        call    _vli_numBits
        mov     ebp, eax
@073:   cmp     ebp, edi
        jnc     @074
        lea     edx, [esp + 48H]
        mov     eax, ebx
        call    _vli_set
        jmp     @082
@074:   lea     eax, [esp + 8H]
        call    _vli_clear
        lea     eax, [esp + 28H]
        call    _vli_clear
        mov     eax, ebp
        sub     eax, edi
        mov     edi, eax
        shr     edi, 6
        and     eax, 3FH
        mov     ecx, eax
        jz      @075
        lea     eax, [esp + edi * 8 + 8H]
        mov     edx, esi
        call    _vli_lshift
        mov     dword ptr[esp + edi * 8 + 28H], eax
        mov     dword ptr[esp + edi * 8 + 2CH], edx
        jmp     @076
@075:   lea     eax, [esp + edi * 8 + 8H]
        mov     edx, esi
        call    _vli_set
@076:   mov     eax, ebx
        call    _vli_clear
        mov     dword ptr[ebx], 1
        mov     dword ptr[ebx + 4H], 0
        jmp     @081
@077:   lea     edx, [esp + 68H]
        lea     eax, [esp + 28H]
        call    _vli_cmp
        test    eax, eax
        js      @078
        test    eax, eax
        jnz     @080
        lea     edx, [esp + 48H]
        lea     eax, [esp + 8H]
        call    _vli_cmp
        test    eax, eax
        jg      @080
@078:   lea     ecx, [esp + 8H]
        lea     edx, [esp + 48H]
        mov     eax, edx
        call    _vli_sub
        or      edx, eax
        jz      @079
        mov     ecx, ebx
        lea     edx, [esp + 68H]
        mov     eax, edx
        call    _vli_sub
@079:   lea     ecx, [esp + 28H]
        lea     edx, [esp + 68H]
        mov     eax, edx
        call    _vli_sub
@080:   mov     edi, dword ptr[esp + 28H]
        shl     edi, 31
        lea     eax, [esp + 28H]
        call    _vli_rshift1
        lea     eax, [esp + 8H]
        call    _vli_rshift1
        or      dword ptr[esp + 20H], 00H
        or      dword ptr[esp + 24H], edi
        sub     ebp, 1
@081:   cmp     ebp, 256
        ja      @077
        mov     edx, esi
        lea     eax, [esp + 8H]
        call    _vli_cmp
        test    eax, eax
        jns     @077
        lea     edx, [esp + 48H]
        mov     eax, ebx
        call    _vli_set
@082:   add     esp, 140
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _vli_modSquare_fast;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 132
        mov     dword ptr[esp + 3CH], eax
        mov     ebp, edx
        mov     dword ptr[esp + 30H], 0
        mov     dword ptr[esp + 34H], 0
        mov     dword ptr[esp + 28H], 0
        mov     dword ptr[esp + 2CH], 0
        mov     dword ptr[esp + 38H], 0
@083:   mov     ebx, dword ptr[esp + 38H]
        mov     eax, ebx
        mov     edi, 0
        cmp     ebx, 3
        jbe     @084
        lea     edi, [ebx - 3H]
        cmp     edi, ebx
        ja      @094
        mov     eax, 3
        cmp     edi, 3
        ja      @095
@084:   mov     ebx, dword ptr[esp + 28H]
        mov     esi, dword ptr[esp + 2CH]
        mov     dword ptr[esp + 18H], ebx
        mov     dword ptr[esp + 1CH], esi
        mov     ebx, dword ptr[esp + 30H]
        mov     esi, dword ptr[esp + 34H]
        mov     dword ptr[esp + 28H], ebx
        mov     dword ptr[esp + 2CH], esi
        mov     dword ptr[esp + 30H], 0
        mov     dword ptr[esp + 34H], 0
        mov     dword ptr[esp], edi
        mov     dword ptr[esp + 4H], eax
        jmp     @086
@085:   mov     eax, dword ptr[esp + 10H]
        mov     edx, dword ptr[esp + 14H]
        mov     dword ptr[esp + 18H], eax
        mov     dword ptr[esp + 1CH], edx
@086:   mov     edi, dword ptr[esp]
        mov     eax, dword ptr[ebp + edi * 8]
        mov     esi, dword ptr[esp + 4H]
        mul     dword ptr[ebp + esi * 8]
        mov     ecx, eax
        mov     ebx, edx
        mov     eax, dword ptr[ebp + edi * 8]
        mul     dword ptr[ebp + esi * 8 + 4H]
        mov     dword ptr[esp + 10H], eax
        mov     dword ptr[esp + 14H], edx
        mov     eax, dword ptr[ebp + edi * 8 + 4H]
        mul     dword ptr[ebp + esi * 8 + 4H]
        mov     dword ptr[esp + 20H], eax
        mov     dword ptr[esp + 24H], edx
        mov     eax, dword ptr[ebp + edi * 8 + 4H]
        mul     dword ptr[ebp + esi * 8]
        mov     esi, eax
        mov     edi, edx
        add     esi, dword ptr[esp + 10H]
        adc     edi, dword ptr[esp + 14H]
        mov     eax, ebx
        xor     edx, edx
        add     eax, esi
        adc     edx, edi
        mov     esi, dword ptr[esp + 10H]
        mov     edi, dword ptr[esp + 14H]
        cmp     edi, edx
        jc      @088
        ja      @087
        cmp     esi, eax
        jbe     @088
@087:   add     dword ptr[esp + 20H], 0
        adc     dword ptr[esp + 24H], 1
@088:   mov     dword ptr[esp + 8H], ecx
        mov     dword ptr[esp + 0CH], eax
        mov     eax, edx
        xor     edx, edx
        mov     ecx, dword ptr[esp + 20H]
        mov     ebx, dword ptr[esp + 24H]
        add     ecx, eax
        adc     ebx, edx
        mov     edx, dword ptr[esp]
        cmp     dword ptr[esp + 4H], edx
        jbe     @089
        mov     eax, ebx
        xor     edx, edx
        shr     eax, 31
        add     dword ptr[esp + 30H], eax
        adc     dword ptr[esp + 34H], edx
        mov     eax, ecx
        mov     edx, ebx
        shld    edx, ecx, 1
        add     eax, ecx
        mov     dword ptr[esp + 10H], eax
        mov     dword ptr[esp + 14H], edx
        mov     esi, dword ptr[esp + 8H]
        mov     edi, dword ptr[esp + 0CH]
        mov     eax, edi
        shr     eax, 31
        or      eax, dword ptr[esp + 10H]
        mov     ecx, eax
        mov     ebx, dword ptr[esp + 14H]
        mov     eax, esi
        mov     edx, edi
        shld    edx, esi, 1
        add     eax, esi
        mov     dword ptr[esp + 8H], eax
        mov     dword ptr[esp + 0CH], edx
@089:   mov     eax, dword ptr[esp + 8H]
        mov     edx, dword ptr[esp + 0CH]
        mov     esi, dword ptr[esp + 18H]
        mov     edi, dword ptr[esp + 1CH]
        add     eax, esi
        adc     edx, edi
        mov     dword ptr[esp + 10H], eax
        mov     dword ptr[esp + 14H], edx
        mov     eax, 1
        mov     edx, 0
        cmp     edi, dword ptr[esp + 14H]
        ja      @091
        mov     edi, dword ptr[esp + 14H]
        cmp     dword ptr[esp + 1CH], edi
        jc      @090
        mov     edi, dword ptr[esp + 10H]
        cmp     dword ptr[esp + 18H], edi
        ja      @091
@090:   mov     eax, 0
        mov     edx, 0
@091:   add     eax, ecx
        adc     edx, ebx
        add     dword ptr[esp + 28H], eax
        adc     dword ptr[esp + 2CH], edx
        mov     eax, 1
        mov     edx, 0
        mov     esi, dword ptr[esp + 28H]
        mov     edi, dword ptr[esp + 2CH]
        cmp     ebx, edi
        ja      @093
        jc      @092
        cmp     ecx, esi
        ja      @093
@092:   mov     eax, 0
        mov     edx, 0
@093:   add     dword ptr[esp + 30H], eax
        adc     dword ptr[esp + 34H], edx
        add     dword ptr[esp], 1
        mov     eax, dword ptr[esp]
        cmp     dword ptr[esp + 38H], eax
        jc      @096
        sub     dword ptr[esp + 4H], 1
        mov     edx, dword ptr[esp + 4H]
        cmp     eax, edx
        jbe     @085
        jmp     @096
@094:   mov     eax, dword ptr[esp + 28H]
        mov     edx, dword ptr[esp + 2CH]
        mov     dword ptr[esp + 10H], eax
        mov     dword ptr[esp + 14H], edx
        mov     eax, dword ptr[esp + 30H]
        mov     edx, dword ptr[esp + 34H]
        mov     dword ptr[esp + 28H], eax
        mov     dword ptr[esp + 2CH], edx
        mov     dword ptr[esp + 30H], 0
        mov     dword ptr[esp + 34H], 0
        jmp     @096
@095:   mov     eax, dword ptr[esp + 28H]
        mov     edx, dword ptr[esp + 2CH]
        mov     dword ptr[esp + 10H], eax
        mov     dword ptr[esp + 14H], edx
        mov     eax, dword ptr[esp + 30H]
        mov     edx, dword ptr[esp + 34H]
        mov     dword ptr[esp + 28H], eax
        mov     dword ptr[esp + 2CH], edx
        mov     dword ptr[esp + 30H], 0
        mov     dword ptr[esp + 34H], 0
@096:   mov     eax, dword ptr[esp + 38H]
        mov     ebx, dword ptr[esp + 10H]
        mov     esi, dword ptr[esp + 14H]
        mov     dword ptr[esp + eax * 8 + 40H], ebx
        mov     dword ptr[esp + eax * 8 + 44H], esi
        add     eax, 1
        mov     dword ptr[esp + 38H], eax
        cmp     eax, 7
        jne     @083
        mov     eax, dword ptr[esp + 28H]
        mov     edx, dword ptr[esp + 2CH]
        mov     dword ptr[esp + 78H], eax
        mov     dword ptr[esp + 7CH], edx
        lea     edx, [esp + 40H]
        mov     eax, dword ptr[esp + 3CH]
        call    _vli_mmod_fast
        add     esp, 132
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
        ret
end;

procedure _EccPoint_double_jacobian;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 76
        mov     esi, eax
        mov     edi, edx
        mov     ebx, ecx
        mov     eax, ecx
        call    _vli_isZero
        test    eax, eax
        jne     @099
        mov     edx, edi
        lea     eax, [esp + 28H]
        call    _vli_modSquare_fast
        lea     ecx, [esp + 28H]
        mov     edx, esi
        lea     eax, [esp + 8H]
        call    _vli_modMult_fast
        lea     edx, [esp + 28H]
        mov     eax, edx
        call    _vli_modSquare_fast
        mov     ecx, ebx
        mov     edx, edi
        mov     eax, edi
        call    _vli_modMult_fast
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_modSquare_fast
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebx
        mov     edx, esi
        mov     eax, esi
        call    _vli_modAdd
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebx
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_modAdd
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebx
        mov     edx, esi
        mov     eax, ebx
        call    _vli_modSub
        mov     ecx, ebx
        mov     edx, esi
        mov     eax, esi
        call    _vli_modMult_fast
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, esi
        mov     edx, esi
        mov     eax, ebx
        call    _vli_modAdd
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebx
        mov     edx, esi
        mov     eax, esi
        call    _vli_modAdd
        mov     edx, 0
        mov     eax, esi
        call    _vli_testBit
        or      edx, eax
        jz      @097
        mov     ecx, offset _curve_p
        mov     edx, esi
        mov     eax, esi
        call    _vli_add
        mov     ebp, eax
        mov     eax, esi
        call    _vli_rshift1
        shl     ebp, 31
        or      dword ptr[esi + 18H], 00H
        or      dword ptr[esi + 1CH], ebp
        jmp     @098
@097:   mov     eax, esi
        call    _vli_rshift1
@098:   mov     edx, esi
        mov     eax, ebx
        call    _vli_modSquare_fast
        mov     dword ptr[esp], offset _curve_p
        lea     ecx, [esp + 8H]
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_modSub
        mov     dword ptr[esp], offset _curve_p
        lea     ecx, [esp + 8H]
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_modSub
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebx
        lea     edx, [esp + 8H]
        mov     eax, edx
        call    _vli_modSub
        lea     ecx, [esp + 8H]
        mov     edx, esi
        mov     eax, esi
        call    _vli_modMult_fast
        mov     dword ptr[esp], offset _curve_p
        lea     ecx, [esp + 28H]
        mov     edx, esi
        mov     eax, ecx
        call    _vli_modSub
        mov     edx, ebx
        mov     eax, esi
        call    _vli_set
        mov     edx, edi
        mov     eax, ebx
        call    _vli_set
        lea     edx, [esp + 28H]
        mov     eax, edi
        call    _vli_set
@099:   add     esp, 76
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _apply_z;
asm
        push    edi
        push    esi
        push    ebx
        sub     esp, 32
        mov     edi, eax
        mov     ebx, edx
        mov     esi, ecx
        mov     edx, ecx
        mov     eax, esp
        call    _vli_modSquare_fast
        mov     ecx, esp
        mov     edx, edi
        mov     eax, edi
        call    _vli_modMult_fast
        mov     ecx, esi
        mov     edx, esp
        mov     eax, esp
        call    _vli_modMult_fast
        mov     ecx, esp
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_modMult_fast
        add     esp, 32
        pop     ebx
        pop     esi
        pop     edi
end;

procedure _XYcZ_add;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 44
        mov     edi, eax
        mov     ebp, edx
        mov     ebx, ecx
        mov     esi, dword ptr[esp + 40H]
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, eax
        mov     edx, ebx
        lea     eax, [esp + 8H]
        call    _vli_modSub
        lea     edx, [esp + 8H]
        mov     eax, edx
        call    _vli_modSquare_fast
        lea     ecx, [esp + 8H]
        mov     edx, edi
        mov     eax, edi
        call    _vli_modMult_fast
        lea     ecx, [esp + 8H]
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_modMult_fast
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebp
        mov     edx, esi
        mov     eax, esi
        call    _vli_modSub
        mov     edx, esi
        lea     eax, [esp + 8H]
        call    _vli_modSquare_fast
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, edi
        lea     edx, [esp + 8H]
        mov     eax, edx
        call    _vli_modSub
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebx
        lea     edx, [esp + 8H]
        mov     eax, edx
        call    _vli_modSub
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, edi
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_modSub
        mov     ecx, ebx
        mov     edx, ebp
        mov     eax, ebp
        call    _vli_modMult_fast
        mov     dword ptr[esp], offset _curve_p
        lea     ecx, [esp + 8H]
        mov     edx, edi
        mov     eax, ebx
        call    _vli_modSub
        mov     ecx, ebx
        mov     edx, esi
        mov     eax, esi
        call    _vli_modMult_fast
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebp
        mov     edx, esi
        mov     eax, esi
        call    _vli_modSub
        lea     edx, [esp + 8H]
        mov     eax, ebx
        call    _vli_set
        add     esp, 44
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _XYcZ_addC;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 108
        mov     esi, eax
        mov     ebp, edx
        mov     ebx, ecx
        mov     edi, dword ptr[esp + 80H]
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, eax
        mov     edx, ebx
        lea     eax, [esp + 48H]
        call    _vli_modSub
        lea     edx, [esp + 48H]
        mov     eax, edx
        call    _vli_modSquare_fast
        lea     ecx, [esp + 48H]
        mov     edx, esi
        mov     eax, esi
        call    _vli_modMult_fast
        lea     ecx, [esp + 48H]
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_modMult_fast
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebp
        mov     edx, edi
        lea     eax, [esp + 48H]
        call    _vli_modAdd
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebp
        mov     edx, edi
        mov     eax, edi
        call    _vli_modSub
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, esi
        mov     edx, ebx
        lea     eax, [esp + 28H]
        call    _vli_modSub
        lea     ecx, [esp + 28H]
        mov     edx, ebp
        mov     eax, ebp
        call    _vli_modMult_fast
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebx
        mov     edx, esi
        lea     eax, [esp + 28H]
        call    _vli_modAdd
        mov     edx, edi
        mov     eax, ebx
        call    _vli_modSquare_fast
        mov     dword ptr[esp], offset _curve_p
        lea     ecx, [esp + 28H]
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_modSub
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebx
        mov     edx, esi
        lea     eax, [esp + 8H]
        call    _vli_modSub
        lea     ecx, [esp + 8H]
        mov     edx, edi
        mov     eax, edi
        call    _vli_modMult_fast
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebp
        mov     edx, edi
        mov     eax, edi
        call    _vli_modSub
        lea     edx, [esp + 48H]
        lea     eax, [esp + 8H]
        call    _vli_modSquare_fast
        mov     dword ptr[esp], offset _curve_p
        lea     ecx, [esp + 28H]
        lea     edx, [esp + 8H]
        mov     eax, edx
        call    _vli_modSub
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, esi
        lea     edx, [esp + 8H]
        lea     eax, [esp + 28H]
        call    _vli_modSub
        lea     ecx, [esp + 48H]
        lea     edx, [esp + 28H]
        mov     eax, edx
        call    _vli_modMult_fast
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, ebp
        lea     edx, [esp + 28H]
        mov     eax, ebp
        call    _vli_modSub
        lea     edx, [esp + 8H]
        mov     eax, esi
        call    _vli_set
        add     esp, 108
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _EccPoint_mult;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 228
        mov     dword ptr[esp + 18H], eax
        mov     edi, edx
        mov     dword ptr[esp + 1CH], edx
        mov     dword ptr[esp + 10H], ecx
        mov     ebx, dword ptr[esp + 0F8H]
        lea     eax, [esp + 0C0H]
        call    _vli_set
        mov     eax, edi
        add     eax, 32
        mov     dword ptr[esp + 14H], eax
        mov     edx, eax
        lea     eax, [esp + 80H]
        call    _vli_set
        lea     edx, [esp + 0C0H]
        lea     eax, [esp + 0A0H]
        call    _vli_set
        lea     edx, [esp + 80H]
        lea     eax, [esp + 60H]
        call    _vli_set
        lea     eax, [esp + 20H]
        call    _vli_clear
        mov     dword ptr[esp + 20H], 1
        mov     dword ptr[esp + 24H], 0
        test    ebx, ebx
        jz      @100
        mov     edx, ebx
        lea     eax, [esp + 20H]
        call    _vli_set
@100:   lea     ecx, [esp + 20H]
        lea     edx, [esp + 80H]
        lea     eax, [esp + 0C0H]
        call    _apply_z
        lea     ecx, [esp + 20H]
        lea     edx, [esp + 80H]
        lea     eax, [esp + 0C0H]
        call    _EccPoint_double_jacobian
        lea     ecx, [esp + 20H]
        lea     edx, [esp + 60H]
        lea     eax, [esp + 0A0H]
        call    _apply_z
        mov     eax, dword ptr[esp + 10H]
        call    _vli_numBits
        sub     eax, 2
        mov     dword ptr[esp + 0CH], eax
        test    eax, eax
        jle     @102
@101:   mov     edx, dword ptr[esp + 0CH]
        mov     eax, dword ptr[esp + 10H]
        call    _vli_testBit
        or      eax, edx
        sete    al
        movzx   eax, al
        mov     esi, eax
        shl     esi, 5
        lea     ecx, [esp + 60H]
        lea     edi, [ecx + esi]
        lea     ecx, [esp + 0A0H]
        add     esi, ecx
        mov     ebx, 1
        sub     ebx, eax
        shl     ebx, 5
        lea     ecx, [esp + 60H]
        lea     ebp, [ecx + ebx]
        lea     edx, [esp + 0A0H]
        add     ebx, edx
        mov     dword ptr[esp], edi
        mov     ecx, esi
        mov     edx, ebp
        mov     eax, ebx
        call    _XYcZ_addC
        mov     dword ptr[esp], ebp
        mov     ecx, ebx
        mov     edx, edi
        mov     eax, esi
        call    _XYcZ_add
        sub     dword ptr[esp + 0CH], 1
        jnz     @101
@102:   mov     edx, 0
        mov     eax, dword ptr[esp + 10H]
        call    _vli_testBit
        or      eax, edx
        sete    al
        movzx   eax, al
        mov     esi, eax
        shl     esi, 5
        lea     edi, [esp + 60H]
        lea     ebp, [edi + esi]
        lea     edi, [esp + 0A0H]
        add     esi, edi
        mov     ebx, 1
        sub     ebx, eax
        shl     ebx, 5
        lea     eax, [esp + 60H]
        lea     edi, [eax + ebx]
        lea     eax, [esp + 0A0H]
        add     ebx, eax
        mov     dword ptr[esp], ebp
        mov     ecx, esi
        mov     edx, edi
        mov     eax, ebx
        call    _XYcZ_addC
        mov     dword ptr[esp], offset _curve_p
        lea     ecx, [esp + 0A0H]
        lea     edx, [esp + 0C0H]
        lea     eax, [esp + 40H]
        call    _vli_modSub
        mov     ecx, edi
        lea     edx, [esp + 40H]
        mov     eax, edx
        call    _vli_modMult_fast
        mov     ecx, dword ptr[esp + 1CH]
        lea     edx, [esp + 40H]
        mov     eax, edx
        call    _vli_modMult_fast
        mov     ecx, offset _curve_p
        lea     edx, [esp + 40H]
        mov     eax, edx
        call    _vli_modInv
        mov     ecx, dword ptr[esp + 14H]
        lea     edx, [esp + 40H]
        mov     eax, edx
        call    _vli_modMult_fast
        mov     ecx, ebx
        lea     edx, [esp + 40H]
        mov     eax, edx
        call    _vli_modMult_fast
        mov     dword ptr[esp], edi
        mov     ecx, ebx
        mov     edx, ebp
        mov     eax, esi
        call    _XYcZ_add
        lea     ecx, [esp + 40H]
        lea     edx, [esp + 60H]
        lea     eax, [esp + 0A0H]
        call    _apply_z
        lea     edx, [esp + 0A0H]
        mov     edi, dword ptr[esp + 18H]
        mov     eax, edi
        call    _vli_set
        mov     eax, edi
        add     eax, 32
        lea     edx, [esp + 60H]
        call    _vli_set
        add     esp, 228
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _ecc_point_decompress;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 108
        mov     edi, eax
        mov     ebp, edx
        mov     ecx, 0
        mov     edx, 32
        mov     eax, 0
@103:   mov     dword ptr[esp + eax + 48H], ecx
        add     eax, 4
        cmp     eax, edx
        jc      @103
        mov     dword ptr[esp + 48H], 3
        mov     dword ptr[esp + 4CH], 0
        lea     edx, [ebp + 1H]
        mov     eax, edi
        call    _ecc_bytesnative
        lea     esi, [edi + 20H]
        mov     edx, edi
        mov     eax, esi
        call    _vli_modSquare_fast
        mov     dword ptr[esp], offset _curve_p
        lea     ecx, [esp + 48H]
        mov     edx, esi
        mov     eax, esi
        call    _vli_modSub
        mov     ecx, edi
        mov     edx, esi
        mov     eax, esi
        call    _vli_modMult_fast
        mov     dword ptr[esp], offset _curve_p
        mov     ecx, offset _curve_b
        mov     edx, esi
        mov     eax, esi
        call    _vli_modAdd
        mov     ecx, 0
        mov     edx, 32
        mov     eax, 0
@104:   mov     dword ptr[esp + eax + 8H], ecx
        add     eax, 4
        cmp     eax, edx
        jc      @104
        mov     dword ptr[esp + 8H], 1
        mov     dword ptr[esp + 0CH], 0
        mov     ecx, 0
        mov     edx, 32
        mov     eax, 0
@105:   mov     dword ptr[esp + eax + 28H], ecx
        add     eax, 4
        cmp     eax, edx
        jc      @105
        mov     dword ptr[esp + 28H], 1
        mov     dword ptr[esp + 2CH], 0
        lea     ecx, [esp + 8H]
        mov     edx, offset _curve_p
        mov     eax, ecx
        call    _vli_add
        lea     eax, [esp + 8H]
        call    _vli_numBits
        lea     ebx, [eax - 1H]
        cmp     ebx, 1
        jbe     @108
@106:   lea     edx, [esp + 28H]
        mov     eax, edx
        call    _vli_modSquare_fast
        mov     edx, ebx
        lea     eax, [esp + 8H]
        call    _vli_testBit
        or      edx, eax
        jz      @107
        mov     ecx, esi
        lea     edx, [esp + 28H]
        mov     eax, edx
        call    _vli_modMult_fast
@107:   sub     ebx, 1
        cmp     ebx, 1
        jnz     @106
@108:   lea     edx, [esp + 28H]
        mov     eax, esi
        call    _vli_set
        movzx   eax, byte ptr[ebp]
        xor     eax, dword ptr[edi + 20H]
        test    al, 01H
        jz      @109
        mov     ecx, esi
        mov     edx, offset _curve_p
        mov     eax, esi
        call    _vli_sub
@109:   add     esp, 108
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;

procedure _ecc_make_key;
asm
        push    esi
        push    ebx
        sub     esp, 116
        mov     esi, 17
        lea     ebx, [esp + 50H]
@110:   mov     dword ptr[esp], ebx
        call    getRandomNumber
        test    eax, eax
        je      @114
        sub     esi, 1
        je      @113
        mov     eax, ebx
        call    _vli_isZero
        test    eax, eax
        jnz     @112
        mov     edx, ebx
        mov     eax, offset _curve_n
        call    _vli_cmp
        cmp     eax, 1
        jz      @111
        mov     ecx, offset _curve_n
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_sub
@111:   mov     dword ptr[esp], 0
        mov     ecx, ebx
        mov     edx, offset _curve_G
        lea     eax, [esp + 10H]
        call    _EccPoint_mult
@112:   lea     eax, [esp + 10H]
        call    _EccPoint_isZero
        test    eax, eax
        jnz     @110
        lea     edx, [esp + 50H]
        mov     eax, dword ptr[esp + 84H]
        call    _ecc_bytesnative
        mov     eax, dword ptr[esp + 80H]
        add     eax, 1
        lea     edx, [esp + 10H]
        call    _ecc_bytesnative
        movzx   eax, byte ptr[esp + 30H]
        and     eax, 01H
        add     eax, 2
        mov     ecx, dword ptr[esp + 80H]
        mov     byte ptr[ecx], al
        mov     eax, 1
        jmp     @114
@113:   mov     eax, 0
@114:   add     esp, 116
        pop     ebx
        pop     esi
end;

procedure _ecdh_shared_secret;
asm
        sub     esp, 220
        lea     eax, [esp + 50H]
        mov     dword ptr[esp], eax
        call    getRandomNumber
        test    eax, eax
        jz      @115
        mov     edx, dword ptr[esp + 0E0H]
        lea     eax, [esp + 90H]
        call    _ecc_point_decompress
        mov     edx, dword ptr[esp + 0E4H]
        lea     eax, [esp + 70H]
        call    _ecc_bytesnative
        lea     eax, [esp + 50H]
        mov     dword ptr[esp], eax
        lea     ecx, [esp + 70H]
        lea     edx, [esp + 90H]
        lea     eax, [esp + 10H]
        call    _EccPoint_mult
        lea     edx, [esp + 10H]
        mov     eax, dword ptr[esp + 0E8H]
        call    _ecc_bytesnative
        lea     eax, [esp + 10H]
        call    _EccPoint_isZero
        test    eax, eax
        sete    al
        movzx   eax, al
@115:   add     esp, 220
end;

procedure _ecdsa_sign;
asm
        push    esi
        push    ebx
        sub     esp, 180
        mov     esi, 17
        lea     ebx, [esp + 90H]
@116:   mov     dword ptr[esp], ebx
        call    getRandomNumber
        test    eax, eax
        je      @120
        sub     esi, 1
        je      @119
        mov     eax, ebx
        call    _vli_isZero
        test    eax, eax
        jnz     @118
        mov     edx, ebx
        mov     eax, offset _curve_n
        call    _vli_cmp
        cmp     eax, 1
        jz      @117
        mov     ecx, offset _curve_n
        mov     edx, ebx
        mov     eax, ebx
        call    _vli_sub
@117:   mov     dword ptr[esp], 0
        mov     ecx, ebx
        mov     edx, offset _curve_G
        lea     eax, [esp + 10H]
        call    _EccPoint_mult
        lea     edx, [esp + 10H]
        mov     eax, offset _curve_n
        call    _vli_cmp
        cmp     eax, 1
        jz      @118
        mov     ecx, offset _curve_n
        lea     edx, [esp + 10H]
        mov     eax, edx
        call    _vli_sub
@118:   lea     eax, [esp + 10H]
        call    _vli_isZero
        test    eax, eax
        jne     @116
        lea     edx, [esp + 10H]
        mov     eax, dword ptr[esp + 0C8H]
        call    _ecc_bytesnative
        mov     edx, dword ptr[esp + 0C0H]
        lea     eax, [esp + 70H]
        call    _ecc_bytesnative
        mov     dword ptr[esp], offset _curve_n
        lea     ecx, [esp + 70H]
        lea     edx, [esp + 10H]
        lea     eax, [esp + 50H]
        call    _vli_modMult
        mov     edx, dword ptr[esp + 0C4H]
        lea     eax, [esp + 70H]
        call    _ecc_bytesnative
        mov     dword ptr[esp], offset _curve_n
        lea     ecx, [esp + 50H]
        lea     edx, [esp + 70H]
        mov     eax, ecx
        call    _vli_modAdd
        mov     ecx, offset _curve_n
        lea     edx, [esp + 90H]
        mov     eax, edx
        call    _vli_modInv
        mov     dword ptr[esp], offset _curve_n
        lea     ecx, [esp + 90H]
        lea     edx, [esp + 50H]
        mov     eax, edx
        call    _vli_modMult
        mov     eax, dword ptr[esp + 0C8H]
        add     eax, 32
        lea     edx, [esp + 50H]
        call    _ecc_bytesnative
        mov     eax, 1
        jmp     @120
@119:   mov     eax, 0
@120:   add     esp, 180
        pop     ebx
        pop     esi
end;

procedure _ecdsa_verify;
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        sub     esp, 476
        mov     ebx, dword ptr[esp + 1F8H]
        mov     edx, dword ptr[esp + 1F0H]
        lea     eax, [esp + 138H]
        call    _ecc_point_decompress
        mov     edx, ebx
        lea     eax, [esp + 38H]
        call    _ecc_bytesnative
        lea     edx, [ebx + 20H]
        lea     eax, [esp + 18H]
        call    _ecc_bytesnative
        lea     eax, [esp + 38H]
        call    _vli_isZero
        mov     ebx, 0
        test    eax, eax
        jne     @126
        mov     esi, eax
        lea     eax, [esp + 18H]
        call    _vli_isZero
        mov     ebx, eax
        test    eax, eax
        jne     @125
        lea     edx, [esp + 38H]
        mov     eax, offset _curve_n
        call    _vli_cmp
        cmp     eax, 1
        jne     @126
        lea     edx, [esp + 18H]
        mov     eax, offset _curve_n
        call    _vli_cmp
        cmp     eax, 1
        jne     @126
        mov     ecx, offset _curve_n
        lea     edx, [esp + 18H]
        lea     eax, [esp + 178H]
        call    _vli_modInv
        mov     edx, dword ptr[esp + 1F4H]
        lea     eax, [esp + 1B8H]
        call    _ecc_bytesnative
        mov     dword ptr[esp], offset _curve_n
        lea     ecx, [esp + 178H]
        lea     edx, [esp + 1B8H]
        mov     eax, edx
        call    _vli_modMult
        mov     dword ptr[esp], offset _curve_n
        lea     ecx, [esp + 178H]
        lea     edx, [esp + 38H]
        lea     eax, [esp + 198H]
        call    _vli_modMult
        lea     ebp, [esp + 138H]
        lea     esi, [esp + 0F8H]
        mov     edx, ebp
        mov     eax, esi
        call    _vli_set
        lea     edi, [esp + 118H]
        lea     edx, [esp + 158H]
        mov     eax, edi
        call    _vli_set
        mov     edx, offset _curve_G
        lea     eax, [esp + 98H]
        call    _vli_set
        mov     edx, offset _curve_G + 20H
        lea     eax, [esp + 78H]
        call    _vli_set
        mov     dword ptr[esp], offset _curve_p
        lea     ecx, [esp + 98H]
        mov     edx, esi
        lea     eax, [esp + 178H]
        call    _vli_modSub
        mov     dword ptr[esp], edi
        mov     ecx, esi
        lea     edx, [esp + 78H]
        lea     eax, [esp + 98H]
        call    _XYcZ_add
        mov     ecx, offset _curve_p
        lea     edx, [esp + 178H]
        mov     eax, edx
        call    _vli_modInv
        lea     ecx, [esp + 178H]
        mov     edx, edi
        mov     eax, esi
        call    _apply_z
        mov     dword ptr[esp + 8H], 0
        mov     dword ptr[esp + 0CH], offset _curve_G
        mov     dword ptr[esp + 10H], ebp
        mov     dword ptr[esp + 14H], esi
        lea     eax, [esp + 198H]
        call    _vli_numBits
        mov     esi, eax
        lea     eax, [esp + 1B8H]
        call    _vli_numBits
        cmp     esi, eax
        cmovc   esi, eax
        lea     ebp, [esi - 1H]
        mov     edx, ebp
        lea     eax, [esp + 1B8H]
        call    _vli_testBit
        or      eax, edx
        setne   al
        movzx   eax, al
        mov     edi, eax
        mov     edx, ebp
        lea     eax, [esp + 198H]
        call    _vli_testBit
        or      eax, edx
        mov     eax, 2
        cmove   eax, ebx
        or      edi, eax
        mov     edi, dword ptr[esp + edi * 4 + 8H]
        mov     edx, edi
        lea     eax, [esp + 0D8H]
        call    _vli_set
        lea     edx, [edi + 20H]
        lea     eax, [esp + 0B8H]
        call    _vli_set
        lea     eax, [esp + 178H]
        call    _vli_clear
        mov     dword ptr[esp + 178H], 1
        mov     dword ptr[esp + 17CH], 0
        sub     esi, 2
        js      @123
        lea     ebp, [esp + 0B8H]
@121:   lea     ecx, [esp + 178H]
        mov     edx, ebp
        lea     eax, [esp + 0D8H]
        call    _EccPoint_double_jacobian
        mov     edx, esi
        lea     eax, [esp + 1B8H]
        call    _vli_testBit
        or      eax, edx
        setne   al
        movzx   eax, al
        mov     edi, eax
        mov     edx, esi
        lea     eax, [esp + 198H]
        call    _vli_testBit
        or      eax, edx
        mov     eax, 2
        cmove   eax, ebx
        or      edi, eax
        mov     edi, dword ptr[esp + edi * 4 + 8H]
        test    edi, edi
        jz      @122
        mov     edx, edi
        lea     eax, [esp + 98H]
        call    _vli_set
        lea     edx, [edi + 20H]
        lea     eax, [esp + 78H]
        call    _vli_set
        lea     ecx, [esp + 178H]
        lea     edx, [esp + 78H]
        lea     eax, [esp + 98H]
        call    _apply_z
        mov     dword ptr[esp], offset _curve_p
        lea     ecx, [esp + 98H]
        lea     edx, [esp + 0D8H]
        lea     eax, [esp + 58H]
        call    _vli_modSub
        mov     dword ptr[esp], ebp
        lea     ecx, [esp + 0D8H]
        lea     edx, [esp + 78H]
        lea     eax, [esp + 98H]
        call    _XYcZ_add
        lea     ecx, [esp + 58H]
        lea     edx, [esp + 178H]
        mov     eax, edx
        call    _vli_modMult_fast
@122:   sub     esi, 1
        jns     @121
@123:   mov     ecx, offset _curve_p
        lea     edx, [esp + 178H]
        mov     eax, edx
        call    _vli_modInv
        lea     ecx, [esp + 178H]
        lea     edx, [esp + 0B8H]
        lea     eax, [esp + 0D8H]
        call    _apply_z
        lea     edx, [esp + 0D8H]
        mov     eax, offset _curve_n
        call    _vli_cmp
        cmp     eax, 1
        jz      @124
        mov     ecx, offset _curve_n
        lea     edx, [esp + 0D8H]
        mov     eax, edx
        call    _vli_sub
@124:   lea     edx, [esp + 38H]
        lea     eax, [esp + 0D8H]
        call    _vli_cmp
        test    eax, eax
        sete    bl
        movzx   ebx, bl
        jmp     @126
@125:   mov     ebx, esi
@126:   mov     eax, ebx
        add     esp, 476
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;
