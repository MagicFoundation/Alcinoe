// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxStdCtrls.pas' rev: 31.00 (Windows)

#ifndef AlfmxstdctrlsHPP
#define AlfmxstdctrlsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <System.ImageList.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.ActnList.hpp>
#include <FMX.ImgList.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxstdctrls
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALBGTrackBar;
class DELPHICLASS TALMinThumbTrackBar;
class DELPHICLASS TALMaxThumbTrackBar;
class DELPHICLASS TALRangeTrackBar;
class DELPHICLASS TALCheckBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TALBGTrackBar : public Fmx::Stdctrls::TTrackBar
{
	typedef Fmx::Stdctrls::TTrackBar inherited;
	
private:
	TALMinThumbTrackBar* fMinThumbTrackBar;
	TALMaxThumbTrackBar* fMaxThumbTrackBar;
	
protected:
	virtual void __fastcall Resize(void);
	virtual void __fastcall ApplyStyle(void);
	virtual void __fastcall DoRealign(void);
	HIDESBASE void __fastcall UpdateHighlight(void);
public:
	/* TTrackBar.Create */ inline __fastcall virtual TALBGTrackBar(System::Classes::TComponent* AOwner)/* overload */ : Fmx::Stdctrls::TTrackBar(AOwner) { }
	
public:
	/* TCustomTrack.Destroy */ inline __fastcall virtual ~TALBGTrackBar(void) { }
	
};


class PASCALIMPLEMENTATION TALMinThumbTrackBar : public Fmx::Stdctrls::TTrackBar
{
	typedef Fmx::Stdctrls::TTrackBar inherited;
	
private:
	TALBGTrackBar* fBGTrackBar;
	TALMaxThumbTrackBar* fMaxThumbTrackBar;
	void __fastcall OnThumbMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	void __fastcall OnThumbMouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	void __fastcall OnThumbMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, float X, float Y);
	void __fastcall OnThumbMouseLeave(System::TObject* Sender);
	void __fastcall OnThumbMouseEnter(System::TObject* Sender);
	void __fastcall onThumbApplyStyleLookup(System::TObject* Sender);
	
protected:
	virtual void __fastcall Resize(void);
	virtual void __fastcall ApplyStyle(void);
	virtual void __fastcall DoChanged(void);
	virtual void __fastcall DoTracking(void);
public:
	/* TTrackBar.Create */ inline __fastcall virtual TALMinThumbTrackBar(System::Classes::TComponent* AOwner)/* overload */ : Fmx::Stdctrls::TTrackBar(AOwner) { }
	
public:
	/* TCustomTrack.Destroy */ inline __fastcall virtual ~TALMinThumbTrackBar(void) { }
	
};


class PASCALIMPLEMENTATION TALMaxThumbTrackBar : public Fmx::Stdctrls::TTrackBar
{
	typedef Fmx::Stdctrls::TTrackBar inherited;
	
private:
	TALBGTrackBar* fBGTrackBar;
	TALMinThumbTrackBar* fMinThumbTrackBar;
	void __fastcall OnThumbMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	void __fastcall OnThumbMouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	void __fastcall OnThumbMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, float X, float Y);
	void __fastcall OnThumbMouseLeave(System::TObject* Sender);
	void __fastcall OnThumbMouseEnter(System::TObject* Sender);
	void __fastcall onThumbApplyStyleLookup(System::TObject* Sender);
	
protected:
	virtual void __fastcall Resize(void);
	virtual void __fastcall ApplyStyle(void);
	virtual void __fastcall DoChanged(void);
	virtual void __fastcall DoTracking(void);
public:
	/* TTrackBar.Create */ inline __fastcall virtual TALMaxThumbTrackBar(System::Classes::TComponent* AOwner)/* overload */ : Fmx::Stdctrls::TTrackBar(AOwner) { }
	
public:
	/* TCustomTrack.Destroy */ inline __fastcall virtual ~TALMaxThumbTrackBar(void) { }
	
};


class PASCALIMPLEMENTATION TALRangeTrackBar : public Fmx::Controls::TControl
{
	typedef Fmx::Controls::TControl inherited;
	
private:
	TALBGTrackBar* fBGTrackBar;
	TALMinThumbTrackBar* fMinThumbTrackBar;
	TALMaxThumbTrackBar* fMaxThumbTrackBar;
	void __fastcall SetMax(const float Value);
	void __fastcall SetMin(const float Value);
	float __fastcall GetMax(void);
	float __fastcall GetMin(void);
	float __fastcall GetFrequency(void);
	float __fastcall GetMaxValue(void);
	float __fastcall GetMinValue(void);
	Fmx::Controls::TOrientation __fastcall GetOrientation(void);
	float __fastcall GetSmallChange(void);
	void __fastcall SetFrequency(const float Value);
	void __fastcall SetMaxValue(const float Value);
	void __fastcall SetMinValue(const float Value);
	void __fastcall SetOrientation(const Fmx::Controls::TOrientation Value);
	void __fastcall SetSmallChange(const float Value);
	void __fastcall SetonApplyStyleLookup(const System::Classes::TNotifyEvent Value);
	System::Classes::TNotifyEvent __fastcall GetOnApplyStyleLookup(void);
	System::UnicodeString __fastcall GetStyleLookup(void);
	void __fastcall SetStyleLookup(const System::UnicodeString Value);
	
protected:
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnTracking;
	System::Classes::TNotifyEvent fonThumbApplyStyleLookup;
	virtual void __fastcall Paint(void);
	virtual void __fastcall Resize(void);
	
public:
	__fastcall virtual TALRangeTrackBar(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALRangeTrackBar(void);
	
__published:
	__property float Frequency = {read=GetFrequency, write=SetFrequency};
	__property float SmallChange = {read=GetSmallChange, write=SetSmallChange};
	__property float Min = {read=GetMin, write=SetMin};
	__property float Max = {read=GetMax, write=SetMax};
	__property float MinValue = {read=GetMinValue, write=SetMinValue};
	__property float MaxValue = {read=GetMaxValue, write=SetMaxValue};
	__property Fmx::Controls::TOrientation Orientation = {read=GetOrientation, write=SetOrientation, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnTracking = {read=FOnTracking, write=FOnTracking};
	__property System::Classes::TNotifyEvent onThumbApplyStyleLookup = {read=fonThumbApplyStyleLookup, write=fonThumbApplyStyleLookup};
	__property System::Classes::TNotifyEvent onApplyStyleLookup = {read=GetOnApplyStyleLookup, write=SetonApplyStyleLookup};
	__property System::UnicodeString StyleLookup = {read=GetStyleLookup, write=SetStyleLookup};
	__property Align = {default=0};
	__property Anchors;
	__property ClipChildren = {default=0};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property Enabled;
	__property Locked = {default=0};
	__property Height;
	__property Padding;
	__property Opacity;
	__property Margins;
	__property Position;
	__property Scale;
	__property Size;
	__property Visible;
	__property Width;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property OnResize;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseEnter;
	__property OnMouseLeave;
};


class PASCALIMPLEMENTATION TALCheckBox : public Fmx::Controls::TControl
{
	typedef Fmx::Controls::TControl inherited;
	
public:
	static const unsigned DesignBorderColor = unsigned(0xa080d080);
	
	
private:
	bool fdoubleBuffered;
	Fmx::Graphics::TBitmap* fBufBitmap;
	System::Types::TRectF fBufBitmapRect;
	System::Types::TSizeF fBufSize;
	Fmx::Imglist::TCustomImageList* fBufImages;
	System::Uitypes::TImageIndex FbufImageIndex;
	bool FPressing;
	System::Classes::TNotifyEvent FOnChange;
	bool FIsPressed;
	bool FIsChecked;
	System::Imagelist::TImageLink* FImageCheckedLink;
	System::Imagelist::TImageLink* FImageUncheckedLink;
	bool FisImagesChanged;
	bool FStretch;
	void __fastcall SetdoubleBuffered(const bool Value);
	bool __fastcall GetIsChecked(void);
	void __fastcall SetIsChecked(const bool Value);
	Fmx::Imglist::TCustomImageList* __fastcall GetImages(void);
	void __fastcall SetImages(Fmx::Imglist::TCustomImageList* const Value);
	System::Uitypes::TImageIndex __fastcall GetImageIndex(void);
	void __fastcall SetImageIndex(const System::Uitypes::TImageIndex Value);
	System::Uitypes::TImageIndex __fastcall GetImageUncheckedIndex(void);
	void __fastcall SetImageUncheckedIndex(const System::Uitypes::TImageIndex Value);
	System::Imagelist::TBaseImageList* __fastcall GetImageList(void);
	void __fastcall SetImageList(System::Imagelist::TBaseImageList* const Value);
	void __fastcall NonBufferedPaint(void);
	void __fastcall SetStretch(const bool Value);
	
protected:
	virtual void __fastcall Paint(void);
	__property Fmx::Graphics::TBitmap* BufBitmap = {read=fBufBitmap};
	virtual void __fastcall DoEndUpdate(void);
	virtual void __fastcall DoChanged(void);
	virtual bool __fastcall ImageCheckedIndexStored(void);
	virtual bool __fastcall ImageUncheckedIndexStored(void);
	virtual bool __fastcall ImagesStored(void);
	virtual System::Types::TSizeF __fastcall GetDefaultSize(void);
	
public:
	__fastcall virtual TALCheckBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALCheckBox(void);
	virtual Fmx::Graphics::TBitmap* __fastcall MakeBufBitmap(void);
	virtual void __fastcall clearBufBitmap(void);
	void __fastcall ImagesChanged(void);
	virtual void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall KeyDown(System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	
__published:
	__property bool doubleBuffered = {read=fdoubleBuffered, write=SetdoubleBuffered, default=1};
	__property Action;
	__property Align = {default=0};
	__property Anchors;
	__property CanFocus = {default=1};
	__property CanParentFocus = {default=0};
	__property ClipChildren = {default=0};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property DisableFocusEffect = {default=0};
	__property DragMode = {default=0};
	__property EnableDragHighlight = {default=1};
	__property Enabled;
	__property Locked = {default=0};
	__property Height;
	__property Hint = {default=0};
	__property HitTest = {default=1};
	__property bool IsChecked = {read=GetIsChecked, write=SetIsChecked, default=0};
	__property System::Uitypes::TImageIndex ImageCheckedIndex = {read=GetImageIndex, write=SetImageIndex, stored=ImageCheckedIndexStored, nodefault};
	__property System::Uitypes::TImageIndex ImageUncheckedIndex = {read=GetImageUncheckedIndex, write=SetImageUncheckedIndex, stored=ImageUncheckedIndexStored, nodefault};
	__property Fmx::Imglist::TCustomImageList* Images = {read=GetImages, write=SetImages, stored=ImagesStored};
	__property bool Stretch = {read=FStretch, write=SetStretch, default=1};
	__property Padding;
	__property Opacity;
	__property Margins;
	__property PopupMenu;
	__property Position;
	__property RotationAngle = {default=0};
	__property RotationCenter;
	__property Scale;
	__property Size;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TouchTargetExpansion;
	__property Visible;
	__property Width;
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property OnDragEnter;
	__property OnDragLeave;
	__property OnDragOver;
	__property OnDragDrop;
	__property OnDragEnd;
	__property OnKeyDown;
	__property OnKeyUp;
	__property OnCanFocus;
	__property OnClick;
	__property OnDblClick;
	__property OnEnter;
	__property OnExit;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnPainting;
	__property OnPaint;
	__property OnResize;
private:
	void *__IGlyph;	// Fmx::Actnlist::IGlyph 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {62BDCA4F-820A-4058-B57A-FE8931DB3CCC}
	operator Fmx::Actnlist::_di_IGlyph()
	{
		Fmx::Actnlist::_di_IGlyph intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Actnlist::IGlyph*(void) { return (Fmx::Actnlist::IGlyph*)&__IGlyph; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alfmxstdctrls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXSTDCTRLS)
using namespace Alfmxstdctrls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxstdctrlsHPP
