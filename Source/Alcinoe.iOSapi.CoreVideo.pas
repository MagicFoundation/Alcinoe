unit Alcinoe.iOSapi.CoreVideo;

interface

{$I Alcinoe.inc}

uses
  iOSapi.CoreVideo,
  Macapi.CoreFoundation;

{$M+}

{*************************************}
{$IFNDEF ALCompilerVersionSupported130}
  {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-4333 has been resolved. If resolved, remove the functions below.'}
{$ENDIF}
function CVOpenGLESTextureCacheCreate(allocator: CFAllocatorRef; cacheAttributes: CFDictionaryRef; eaglContext: Pointer; textureAttributes: CFDictionaryRef; cacheOut: PCVOpenGLESTextureCacheRef): CVReturn; cdecl; external libCoreVideo name _PU + 'CVOpenGLESTextureCacheCreate';

implementation

end.