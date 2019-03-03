// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFMXTypes3D.pas' rev: 32.00 (Windows)

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
#include <FMX.Materials.Canvas.hpp>
#include <ALFmxFilterEffects.hpp>
#include <FMX.Materials.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxtypes3d
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALCanvasTextureMaterial;
class DELPHICLASS TALCanvasExternalOESTextureMaterial;
class DELPHICLASS TALCanvasExternalOESColorAdjustEffectTextureMaterial;
class DELPHICLASS TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial;
class DELPHICLASS TALCanvas420YpCbCr8BiPlanarVideoRangeColorAdjustEffectTextureMaterial;
class DELPHICLASS TALCanvas420YpCbCr8PlanarTextureMaterial;
class DELPHICLASS TALCanvas420YpCbCr8PlanarColorAdjustEffectTextureMaterial;
class DELPHICLASS TALTextureAccessPrivate;
class DELPHICLASS TALTexture;
class DELPHICLASS TALBiPlanarTexture;
class DELPHICLASS TALPlanarTexture;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TALCanvasTextureMaterial : public Fmx::Materials::Canvas::TCanvasTextureMaterial
{
	typedef Fmx::Materials::Canvas::TCanvasTextureMaterial inherited;
	
public:
	/* TMaterial.Create */ inline __fastcall virtual TALCanvasTextureMaterial(void) : Fmx::Materials::Canvas::TCanvasTextureMaterial() { }
	/* TMaterial.Destroy */ inline __fastcall virtual ~TALCanvasTextureMaterial(void) { }
	
};


class PASCALIMPLEMENTATION TALCanvasExternalOESTextureMaterial : public TALCanvasTextureMaterial
{
	typedef TALCanvasTextureMaterial inherited;
	
protected:
	virtual void __fastcall DoInitialize(void);
public:
	/* TMaterial.Create */ inline __fastcall virtual TALCanvasExternalOESTextureMaterial(void) : TALCanvasTextureMaterial() { }
	/* TMaterial.Destroy */ inline __fastcall virtual ~TALCanvasExternalOESTextureMaterial(void) { }
	
};


class PASCALIMPLEMENTATION TALCanvasExternalOESColorAdjustEffectTextureMaterial : public TALCanvasExternalOESTextureMaterial
{
	typedef TALCanvasExternalOESTextureMaterial inherited;
	
private:
	Alfmxfiltereffects::TALColorAdjustShaderVariables* fShaderVariables;
	
protected:
	virtual void __fastcall DoApply(Fmx::Types3d::TContext3D* const Context);
	virtual void __fastcall DoInitialize(void);
	
public:
	__fastcall virtual TALCanvasExternalOESColorAdjustEffectTextureMaterial(void);
	__fastcall virtual ~TALCanvasExternalOESColorAdjustEffectTextureMaterial(void);
	__property Alfmxfiltereffects::TALColorAdjustShaderVariables* ShaderVariables = {read=fShaderVariables};
};


class PASCALIMPLEMENTATION TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial : public TALCanvasTextureMaterial
{
	typedef TALCanvasTextureMaterial inherited;
	
private:
	Fmx::Types3d::TTexture* __fastcall getCbCrTexture(void);
	
protected:
	virtual void __fastcall DoApply(Fmx::Types3d::TContext3D* const Context);
	virtual void __fastcall DoInitialize(void);
	
public:
	__property Fmx::Types3d::TTexture* CbCrTexture = {read=getCbCrTexture};
public:
	/* TMaterial.Create */ inline __fastcall virtual TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial(void) : TALCanvasTextureMaterial() { }
	/* TMaterial.Destroy */ inline __fastcall virtual ~TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial(void) { }
	
};


class PASCALIMPLEMENTATION TALCanvas420YpCbCr8BiPlanarVideoRangeColorAdjustEffectTextureMaterial : public TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial
{
	typedef TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial inherited;
	
private:
	Alfmxfiltereffects::TALColorAdjustShaderVariables* fShaderVariables;
	
protected:
	virtual void __fastcall DoApply(Fmx::Types3d::TContext3D* const Context);
	virtual void __fastcall DoInitialize(void);
	
public:
	__fastcall virtual TALCanvas420YpCbCr8BiPlanarVideoRangeColorAdjustEffectTextureMaterial(void);
	__fastcall virtual ~TALCanvas420YpCbCr8BiPlanarVideoRangeColorAdjustEffectTextureMaterial(void);
	__property Alfmxfiltereffects::TALColorAdjustShaderVariables* ShaderVariables = {read=fShaderVariables};
};


class PASCALIMPLEMENTATION TALCanvas420YpCbCr8PlanarTextureMaterial : public TALCanvasTextureMaterial
{
	typedef TALCanvasTextureMaterial inherited;
	
private:
	Fmx::Types3d::TTexture* __fastcall getCbTexture(void);
	Fmx::Types3d::TTexture* __fastcall getCrTexture(void);
	
protected:
	virtual void __fastcall DoApply(Fmx::Types3d::TContext3D* const Context);
	virtual void __fastcall DoInitialize(void);
	
public:
	__property Fmx::Types3d::TTexture* CbTexture = {read=getCbTexture};
	__property Fmx::Types3d::TTexture* CrTexture = {read=getCrTexture};
public:
	/* TMaterial.Create */ inline __fastcall virtual TALCanvas420YpCbCr8PlanarTextureMaterial(void) : TALCanvasTextureMaterial() { }
	/* TMaterial.Destroy */ inline __fastcall virtual ~TALCanvas420YpCbCr8PlanarTextureMaterial(void) { }
	
};


class PASCALIMPLEMENTATION TALCanvas420YpCbCr8PlanarColorAdjustEffectTextureMaterial : public TALCanvas420YpCbCr8PlanarTextureMaterial
{
	typedef TALCanvas420YpCbCr8PlanarTextureMaterial inherited;
	
private:
	Alfmxfiltereffects::TALColorAdjustShaderVariables* fShaderVariables;
	
protected:
	virtual void __fastcall DoApply(Fmx::Types3d::TContext3D* const Context);
	virtual void __fastcall DoInitialize(void);
	
public:
	__fastcall virtual TALCanvas420YpCbCr8PlanarColorAdjustEffectTextureMaterial(void);
	__fastcall virtual ~TALCanvas420YpCbCr8PlanarColorAdjustEffectTextureMaterial(void);
	__property Alfmxfiltereffects::TALColorAdjustShaderVariables* ShaderVariables = {read=fShaderVariables};
};


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
	static TALCanvasExternalOESTextureMaterial* FDefExternalOESMaterial;
	static TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial* FDef420YpCbCr8BiPlanarVideoRangeMaterial;
	static TALCanvas420YpCbCr8PlanarTextureMaterial* FDef420YpCbCr8PlanarMaterial;
	static TALCanvasExternalOESTextureMaterial* __fastcall getDefExternalOESMaterial();
	static TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial* __fastcall getDef420YpCbCr8BiPlanarVideoRangeMaterial();
	static TALCanvas420YpCbCr8PlanarTextureMaterial* __fastcall getDef420YpCbCr8PlanarMaterial();
	TALCanvasTextureMaterial* fMaterial;
	
public:
	__fastcall virtual TALTexture(void);
	__fastcall virtual ~TALTexture(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source)/* overload */;
	__property TALCanvasTextureMaterial* Material = {read=fMaterial, write=fMaterial};
	/* static */ __property TALCanvasExternalOESTextureMaterial* DefExternalOESMaterial = {read=getDefExternalOESMaterial};
	/* static */ __property TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial* Def420YpCbCr8BiPlanarVideoRangeMaterial = {read=getDef420YpCbCr8BiPlanarVideoRangeMaterial};
	/* static */ __property TALCanvas420YpCbCr8PlanarTextureMaterial* Def420YpCbCr8PlanarMaterial = {read=getDef420YpCbCr8PlanarMaterial};
};

#pragma pack(pop)

enum DECLSPEC_DENUM TALTextureFormat : unsigned char { f420YpCbCr8BiPlanarVideoRange, f420YpCbCr8Planar };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALBiPlanarTexture : public TALTexture
{
	typedef TALTexture inherited;
	
private:
	Fmx::Types3d::TTexture* FSecondTexture;
	TALTextureFormat FFormat;
	
public:
	__fastcall virtual TALBiPlanarTexture(void);
	__fastcall virtual ~TALBiPlanarTexture(void);
	__property Fmx::Types3d::TTexture* SecondTexture = {read=FSecondTexture};
	__property TALTextureFormat Format = {read=FFormat, write=FFormat, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALPlanarTexture : public TALTexture
{
	typedef TALTexture inherited;
	
private:
	Fmx::Types3d::TTexture* FSecondTexture;
	Fmx::Types3d::TTexture* FThirdTexture;
	TALTextureFormat FFormat;
	
public:
	__fastcall virtual TALPlanarTexture(void);
	__fastcall virtual ~TALPlanarTexture(void);
	__property Fmx::Types3d::TTexture* SecondTexture = {read=FSecondTexture};
	__property Fmx::Types3d::TTexture* ThirdTexture = {read=FThirdTexture};
	__property TALTextureFormat Format = {read=FFormat, write=FFormat, nodefault};
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
