// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxLayouts.pas' rev: 32.00 (Windows)

#ifndef AlfmxlayoutsHPP
#define AlfmxlayoutsHPP

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
#include <System.Messaging.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.hpp>
#include <ALFmxStdCtrls.hpp>
#include <ALFmxInertialMovement.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxlayouts
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALLayout;
class DELPHICLASS TALScrollBoxContent;
class DELPHICLASS TALScrollBoxAniCalculations;
class DELPHICLASS TALScrollBoxBar;
class DELPHICLASS TALCustomScrollBox;
class DELPHICLASS TALScrollBox;
class DELPHICLASS TALVertScrollBox;
class DELPHICLASS TALHorzScrollBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TALLayout : public Fmx::Layouts::TLayout
{
	typedef Fmx::Layouts::TLayout inherited;
	
protected:
	virtual void __fastcall DoRealign(void);
public:
	/* TLayout.Create */ inline __fastcall virtual TALLayout(System::Classes::TComponent* AOwner) : Fmx::Layouts::TLayout(AOwner) { }
	
public:
	/* TControl.Destroy */ inline __fastcall virtual ~TALLayout(void) { }
	
};


class PASCALIMPLEMENTATION TALScrollBoxContent : public Fmx::Controls::TContent
{
	typedef Fmx::Controls::TContent inherited;
	
private:
	TALCustomScrollBox* FScrollBox;
	
protected:
	virtual void __fastcall ContentChanged(void);
	
public:
	__fastcall virtual TALScrollBoxContent(System::Classes::TComponent* AOwner);
	__property TALCustomScrollBox* ScrollBox = {read=FScrollBox};
public:
	/* TControl.Destroy */ inline __fastcall virtual ~TALScrollBoxContent(void) { }
	
};


class PASCALIMPLEMENTATION TALScrollBoxAniCalculations : public Alfmxinertialmovement::TALAniCalculations
{
	typedef Alfmxinertialmovement::TALAniCalculations inherited;
	
private:
	TALCustomScrollBox* FScrollBox;
	System::Types::TPointF fLastViewportPosition;
	float fScreenScale;
	
protected:
	virtual void __fastcall DoChanged(void);
	virtual void __fastcall DoStart(void);
	virtual void __fastcall DoStop(void);
	
public:
	__fastcall virtual TALScrollBoxAniCalculations(System::Classes::TPersistent* AOwner);
	__property TALCustomScrollBox* ScrollBox = {read=FScrollBox};
public:
	/* TALAniCalculations.Destroy */ inline __fastcall virtual ~TALScrollBoxAniCalculations(void) { }
	
};


class PASCALIMPLEMENTATION TALScrollBoxBar : public Alfmxstdctrls::TALScrollBar
{
	typedef Alfmxstdctrls::TALScrollBar inherited;
	
private:
	TALCustomScrollBox* FScrollBox;
	
protected:
	virtual void __fastcall DoChanged(void);
	virtual void __fastcall Resize(void);
	
public:
	__fastcall virtual TALScrollBoxBar(System::Classes::TComponent* AOwner);
	
__published:
	__property Locked = {stored=false, default=0};
	__property Min = {stored=false};
	__property Max = {stored=false};
	__property Orientation = {stored=false};
	__property Position = {stored=false};
	__property Value = {stored=false};
	__property ViewportSize = {stored=false};
	__property Opacity = {stored=false};
	__property Visible = {stored=false, default=1};
	__property Enabled = {stored=false};
	__property HitTest = {default=0};
public:
	/* TALCustomTrack.Destroy */ inline __fastcall virtual ~TALScrollBoxBar(void) { }
	
};


typedef void __fastcall (__closure *TALScrollBoxPositionChangeEvent)(System::TObject* Sender, const System::Types::TPointF &OldViewportPosition, const System::Types::TPointF &NewViewportPosition);

typedef void __fastcall (__closure *TALScrollBoxBarInit)(System::TObject* const sender, TALScrollBoxBar* const aScrollBar);

class PASCALIMPLEMENTATION TALCustomScrollBox : public Fmx::Controls::TControl
{
	typedef Fmx::Controls::TControl inherited;
	
private:
	float FScreenScale;
	TALScrollBoxAniCalculations* FAniCalculations;
	TALScrollBoxContent* FContent;
	TALScrollBoxBar* FHScrollBar;
	TALScrollBoxBar* FVScrollBar;
	bool FDisableMouseWheel;
	bool fdisableScrollChange;
	bool FHasTouchScreen;
	bool FShowScrollBars;
	bool FAutoHide;
	bool FMouseEvents;
	bool fGestureEvents;
	TALScrollBoxPositionChangeEvent FOnViewportPositionChange;
	TALScrollBoxBarInit fOnScrollBarInit;
	System::Classes::TNotifyEvent fOnAniStart;
	System::Classes::TNotifyEvent fOnAniStop;
	System::Types::TPointF fMouseDownPos;
	int FDeadZoneBeforeAcquireScrolling;
	bool fScrollingAcquiredByMe;
	bool fScrollingAcquiredByOther;
	int fScrollingAcquiredByOtherMessageID;
	float fMaxContentWidth;
	float fMaxContentHeight;
	System::Types::TPointF fAnchoredContentOffset;
	void __fastcall setScrollingAcquiredByMe(const bool Value);
	void __fastcall ScrollingAcquiredByOtherHandler(System::TObject* const Sender, System::Messaging::TMessageBase* const M);
	void __fastcall SetShowScrollBars(const bool Value);
	void __fastcall SetAutoHide(const bool Value);
	void __fastcall setAniCalculations(TALScrollBoxAniCalculations* const Value);
	bool __fastcall isMaxContentHeightStored(void);
	bool __fastcall isMaxContentWidthStored(void);
	
protected:
	virtual void __fastcall Loaded(void);
	virtual void __fastcall DoAddObject(Fmx::Types::TFmxObject* const AObject);
	virtual void __fastcall DoRealign(void);
	virtual void __fastcall CMGesture(Fmx::Types::TGestureEventInfo &EventInfo);
	virtual TALScrollBoxBar* __fastcall CreateScrollBar(const Fmx::Controls::TOrientation aOrientation);
	virtual TALScrollBoxContent* __fastcall CreateContent(void);
	virtual TALScrollBoxAniCalculations* __fastcall CreateAniCalculations(void);
	virtual System::Types::TRectF __fastcall CalcContentBounds(void);
	virtual void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall DoMouseLeave(void);
	virtual void __fastcall MouseWheel(System::Classes::TShiftState Shift, int WheelDelta, bool &Handled);
	__property TALScrollBoxBar* HScrollBar = {read=FHScrollBar};
	__property TALScrollBoxBar* VScrollBar = {read=FVScrollBar};
	__property float MaxContentWidth = {read=fMaxContentWidth, write=fMaxContentWidth, stored=isMaxContentWidthStored};
	__property float MaxContentHeight = {read=fMaxContentHeight, write=fMaxContentHeight, stored=isMaxContentHeightStored};
	
public:
	__fastcall virtual TALCustomScrollBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALCustomScrollBox(void);
	__property TALScrollBoxAniCalculations* AniCalculations = {read=FAniCalculations, write=setAniCalculations};
	virtual void __fastcall Sort(Fmx::Types::_di_TFmxObjectSortCompare Compare);
	void __fastcall ScrollBy(const float Dx, const float Dy);
	virtual Fmx::Types::_di_ITabList __fastcall GetTabList(void);
	__property TALScrollBoxContent* Content = {read=FContent};
	__property bool HasTouchScreen = {read=FHasTouchScreen, nodefault};
	__property bool AutoHide = {read=FAutoHide, write=SetAutoHide, default=1};
	__property bool DisableMouseWheel = {read=FDisableMouseWheel, write=FDisableMouseWheel, default=0};
	__property bool ShowScrollBars = {read=FShowScrollBars, write=SetShowScrollBars, default=1};
	__property TALScrollBoxPositionChangeEvent OnViewportPositionChange = {read=FOnViewportPositionChange, write=FOnViewportPositionChange};
	__property int DeadZoneBeforeAcquireScrolling = {read=FDeadZoneBeforeAcquireScrolling, write=FDeadZoneBeforeAcquireScrolling, default=32};
	__property TALScrollBoxBarInit OnScrollBarInit = {read=fOnScrollBarInit, write=fOnScrollBarInit};
	__property ClipChildren = {default=1};
	__property System::Classes::TNotifyEvent OnAniStart = {read=fOnAniStart, write=fOnAniStart};
	__property System::Classes::TNotifyEvent OnAniStop = {read=fOnAniStop, write=fOnAniStop};
};


class PASCALIMPLEMENTATION TALScrollBox : public TALCustomScrollBox
{
	typedef TALCustomScrollBox inherited;
	
protected:
	virtual void __fastcall Paint(void);
	
public:
	__fastcall virtual TALScrollBox(System::Classes::TComponent* AOwner);
	
__published:
	__property HScrollBar;
	__property VScrollBar;
	__property AniCalculations;
	__property Align = {default=0};
	__property Anchors;
	__property AutoHide = {default=1};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property DisableMouseWheel = {default=0};
	__property DragMode = {default=0};
	__property Enabled;
	__property EnableDragHighlight = {default=1};
	__property Height;
	__property HitTest = {default=1};
	__property Locked = {default=0};
	__property Margins;
	__property Opacity;
	__property Padding;
	__property PopupMenu;
	__property Position;
	__property RotationAngle = {default=0};
	__property RotationCenter;
	__property Scale;
	__property ShowScrollBars = {default=1};
	__property Size;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TouchTargetExpansion;
	__property Visible = {default=1};
	__property Width;
	__property OnPainting;
	__property OnPaint;
	__property OnResize;
	__property OnResized;
	__property OnDragEnter;
	__property OnDragLeave;
	__property OnDragOver;
	__property OnDragDrop;
	__property OnDragEnd;
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnViewportPositionChange;
	__property OnScrollBarInit;
	__property OnAniStart;
	__property OnAniStop;
public:
	/* TALCustomScrollBox.Destroy */ inline __fastcall virtual ~TALScrollBox(void) { }
	
};


class PASCALIMPLEMENTATION TALVertScrollBox : public TALCustomScrollBox
{
	typedef TALCustomScrollBox inherited;
	
protected:
	virtual System::Types::TRectF __fastcall CalcContentBounds(void);
	virtual void __fastcall Paint(void);
	virtual TALScrollBoxAniCalculations* __fastcall CreateAniCalculations(void);
	
public:
	__fastcall virtual TALVertScrollBox(System::Classes::TComponent* AOwner);
	
__published:
	__property MaxContentWidth = {default=0};
	__property VScrollBar;
	__property AniCalculations;
	__property Align = {default=0};
	__property Anchors;
	__property AutoHide = {default=1};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property DisableMouseWheel = {default=0};
	__property DragMode = {default=0};
	__property Enabled;
	__property EnableDragHighlight = {default=1};
	__property Height;
	__property HitTest = {default=1};
	__property Locked = {default=0};
	__property Margins;
	__property Opacity;
	__property Padding;
	__property PopupMenu;
	__property Position;
	__property RotationAngle = {default=0};
	__property RotationCenter;
	__property Scale;
	__property ShowScrollBars = {default=1};
	__property Size;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TouchTargetExpansion;
	__property Visible = {default=1};
	__property Width;
	__property OnPainting;
	__property OnPaint;
	__property OnResize;
	__property OnResized;
	__property OnDragEnter;
	__property OnDragLeave;
	__property OnDragOver;
	__property OnDragDrop;
	__property OnDragEnd;
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnViewportPositionChange;
	__property OnScrollBarInit;
	__property OnAniStart;
	__property OnAniStop;
public:
	/* TALCustomScrollBox.Destroy */ inline __fastcall virtual ~TALVertScrollBox(void) { }
	
};


class PASCALIMPLEMENTATION TALHorzScrollBox : public TALCustomScrollBox
{
	typedef TALCustomScrollBox inherited;
	
protected:
	virtual System::Types::TRectF __fastcall CalcContentBounds(void);
	virtual void __fastcall Paint(void);
	virtual TALScrollBoxAniCalculations* __fastcall CreateAniCalculations(void);
	
public:
	__fastcall virtual TALHorzScrollBox(System::Classes::TComponent* AOwner);
	
__published:
	__property MaxContentHeight = {default=0};
	__property HScrollBar;
	__property AniCalculations;
	__property Align = {default=0};
	__property Anchors;
	__property AutoHide = {default=1};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property DisableMouseWheel = {default=0};
	__property DragMode = {default=0};
	__property Enabled;
	__property EnableDragHighlight = {default=1};
	__property Height;
	__property HitTest = {default=1};
	__property Locked = {default=0};
	__property Margins;
	__property Opacity;
	__property Padding;
	__property PopupMenu;
	__property Position;
	__property RotationAngle = {default=0};
	__property RotationCenter;
	__property Scale;
	__property ShowScrollBars = {default=1};
	__property Size;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TouchTargetExpansion;
	__property Visible = {default=1};
	__property Width;
	__property OnPainting;
	__property OnPaint;
	__property OnResize;
	__property OnResized;
	__property OnDragEnter;
	__property OnDragLeave;
	__property OnDragOver;
	__property OnDragDrop;
	__property OnDragEnd;
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnViewportPositionChange;
	__property OnScrollBarInit;
	__property OnAniStart;
	__property OnAniStop;
public:
	/* TALCustomScrollBox.Destroy */ inline __fastcall virtual ~TALHorzScrollBox(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alfmxlayouts */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXLAYOUTS)
using namespace Alfmxlayouts;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxlayoutsHPP
