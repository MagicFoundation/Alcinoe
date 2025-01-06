unit Alcinoe.iOSapi.CoreFoundation;

interface

uses
  Macapi.CoreFoundation;

const
  libCoreFoundation = '/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation';

function CFDataCreate(allocator: CFAllocatorRef; bytes: Pointer; length: CFIndex): CFDataRef; cdecl; external libCoreFoundation name _PU + 'CFDataCreate';
function CFDataCreateWithBytesNoCopy(allocator: CFAllocatorRef; bytes: Pointer; length: CFIndex; bytesDeallocator: CFAllocatorRef): CFDataRef; cdecl; external libCoreFoundation name _PU + 'CFDataCreateWithBytesNoCopy';

implementation

end.
