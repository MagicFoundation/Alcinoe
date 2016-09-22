// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxImgList.pas' rev: 31.00 (Windows)

#ifndef AlfmximglistHPP
#define AlfmximglistHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.ImgList.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmximglist
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALGlyph;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TALGlyph : public Fmx::Imglist::TGlyph
{
	typedef Fmx::Imglist::TGlyph inherited;
	
private:
	float FScreenScale;
	bool fdoubleBuffered;
	Fmx::Graphics::TBitmap* fBufBitmap;
	System::Types::TRectF fBufBitmapRect;
	System::Types::TSizeF fBufSize;
	Fmx::Imglist::TCustomImageList* fBufImages;
	System::Uitypes::TImageIndex FbufImageIndex;
	void __fastcall SetdoubleBuffered(const bool Value);
	
protected:
	virtual void __fastcall Paint(void);
	__property Fmx::Graphics::TBitmap* BufBitmap = {read=fBufBitmap};
	
public:
	__fastcall virtual TALGlyph(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALGlyph(void);
	virtual Fmx::Graphics::TBitmap* __fastcall MakeBufBitmap(void);
	virtual void __fastcall clearBufBitmap(void);
	
__published:
	__property Cursor = {default=0};
	__property RotationAngle = {default=0};
	__property RotationCenter;
	__property Scale;
	__property bool doubleBuffered = {read=fdoubleBuffered, write=SetdoubleBuffered, default=1};
	__property TouchTargetExpansion;
	__property HitTest = {default=0};
	__property AutoHide = {default=0};
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnResize;
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alfmximglist */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXIMGLIST)
using namespace Alfmximglist;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmximglistHPP
