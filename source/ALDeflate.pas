{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Inflate and Deflate Functions
Version:      3.52

Description:  Unit to deflate or inflate string using ZlibEx

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALDeflate;

interface

uses Classes,
     ALZlibEx,
     ALZlibExGZ;

{deflate fcn}
Function  ALZInflate4Browser(const s: string): String;
Function  ALZDeflate4Browser(const s: string): String;
function  ALZCompressStr(const s: String; level: TZCompressionLevel = zcDefault): String;
function  ALZDecompressStr(const s: String): String;
procedure ALZCompressStream(inStream, outStream: TStream; level: TZCompressionLevel = zcDefault);
procedure ALZDecompressStream(inStream, outStream: TStream);
function  ALGZCompressStr(const s: String): String;
function  ALGZDecompressStr(const s: String): String;
procedure ALGZCompressStream(inStream, outStream: TStream);
procedure ALGZDecompressStream(inStream, outStream: TStream);


implementation

{****************************************************}
Function  ALZDeflate4Browser(const s: string): String;
var buffer: Pointer;
    size  : Integer;
begin
  ZCompress2(PChar(s),Length(s),buffer,size,zcFastest,-15,9,zsDefault);
  SetLength(result,size);
  Move(buffer^,result[1],size);
  FreeMem(buffer);
end;

{****************************************************}
Function  ALZInflate4Browser(const s: string): String;
var buffer: Pointer;
    size  : Integer;
begin
  ZDecompress2(PChar(s),Length(s),buffer,size,-15);
  SetLength(result,size);
  Move(buffer^,result[1],size);
  FreeMem(buffer);
end;

{**************************************************************************************}
function ALZCompressStr(const s: String; level: TZCompressionLevel = zcDefault): String;
Begin
  Result := ZCompressStr(s,level);
end;

{*************************************************}
function ALZDecompressStr(const s: String): String;
begin
  Result := ZDecompressStr(s);
end;

{***********************************************************************************************}
procedure ALZCompressStream(inStream, outStream: TStream; level: TZCompressionLevel = zcDefault);
begin
  ZCompressStream(inStream, outStream, level);
end;

{**********************************************************}
procedure ALZDecompressStream(inStream, outStream: TStream);
begin
  ZDecompressStream(inStream, outStream);
end;

{************************************************}
function ALGZCompressStr(const s: String): String;
Begin
  Result := GZCompressStr(s);
end;

{**************************************************}
function ALGZDecompressStr(const s: String): String;
begin
  Result := GZDecompressStr(s);
end;

{*********************************************************}
procedure ALGZCompressStream(inStream, outStream: TStream);
begin
  GZCompressStream(inStream, outStream);
end;

{***********************************************************}
procedure ALGZDecompressStream(inStream, outStream: TStream);
begin
  GZDecompressStream(inStream, outStream);
end;

end.
