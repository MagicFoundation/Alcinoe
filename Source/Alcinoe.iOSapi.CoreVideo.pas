unit Alcinoe.iOSapi.CoreVideo;

interface

{$I Alcinoe.inc}

uses
  Posix.StdDef,
  iOSapi.CoreVideo,
  Macapi.CoreFoundation,
  Macapi.Metal;

{$M+}

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1939 has been resolved. If resolved, remove the definitions below.'}
  {$ENDIF}
  CVMetalTextureRef = CVImageBufferRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1939
  PCVMetalTextureRef = ^CVMetalTextureRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1939
  CVMetalTextureCacheRef = Pointer; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1939
  PCVMetalTextureCacheRef = ^CVMetalTextureCacheRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1939

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1939 has been resolved. If resolved, remove the functions below.'}
{$ENDIF}
function CVMetalTextureGetTexture(image: CVMetalTextureRef): pointer; cdecl; external libCoreVideo name _PU + 'CVMetalTextureGetTexture'; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1939
procedure CVMetalTextureCacheFlush(textureCache: CVMetalTextureCacheRef; options: CVOptionFlags); cdecl; external libCoreVideo name _PU + 'CVMetalTextureCacheFlush'; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1939
function CVMetalTextureCacheCreate(allocator: CFAllocatorRef; cacheAttributes: CFDictionaryRef; metalDevice: Pointer; textureAttributes: CFDictionaryRef; cacheOut: PCVMetalTextureCacheRef): CVReturn; cdecl; external libCoreVideo name _PU + 'CVMetalTextureCacheCreate'; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1939
function CVMetalTextureCacheCreateTextureFromImage(allocator: CFAllocatorRef; textureCache: CVMetalTextureCacheRef; sourceImage: CVImageBufferRef; textureAttributes: CFDictionaryRef; pixelFormat: MTLPixelFormat; width: size_t; height: size_t; planeIndex: size_t; textureOut: PCVMetalTextureRef): CVReturn; cdecl; external libCoreVideo name _PU + 'CVMetalTextureCacheCreateTextureFromImage'; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1939

implementation

end.
