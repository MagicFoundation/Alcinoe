// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFMXTypes3D.pas' rev: 31.00 (Windows)

#ifndef Alfmxtypes3dHPP
#define Alfmxtypes3dHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxtypes3d
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALTextureAccessPrivate;
class DELPHICLASS TALTexture;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TALTextureAccessPrivate : public System::Classes::TInterfacedPersistent
{
	typedef System::Classes::TInterfacedPersistent inherited;
	
public:
	int FWidth;
	int FHeight;
	Fmx::Types::TPixelFormat FPixelFormat;
	NativeUInt FHandle;
	Fmx::Types3d::TTextureStyles FStyle;
	Fmx::Types3d::TTextureFilter FMagFilter;
	Fmx::Types3d::TTextureFilter FMinFilter;
	float FTextureScale;
	bool FRequireInitializeAfterLost;
	void *FBits;
	int FContextLostId;
	int FContextResetId;
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TALTextureAccessPrivate(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TALTextureAccessPrivate(void) : System::Classes::TInterfacedPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALTexture : public Fmx::Types3d::TTexture
{
	typedef Fmx::Types3d::TTexture inherited;
	
private:
	bool fVolatile;
	
public:
	__fastcall TALTexture(const bool aVolatile);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
public:
	/* TTexture.Destroy */ inline __fastcall virtual ~TALTexture(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Alfmxtypes3d */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXTYPES3D)
using namespace Alfmxtypes3d;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Alfmxtypes3dHPP
