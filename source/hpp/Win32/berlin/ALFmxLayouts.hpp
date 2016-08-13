// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxLayouts.pas' rev: 31.00 (Windows)

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
#include <System.SysUtils.hpp>
#include <System.Math.Vectors.hpp>
#include <System.Messaging.hpp>
#include <FMX.Types.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Platform.hpp>
#include <FMX.Controls.hpp>
#include <ALFmxInertialMovement.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxlayouts
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALScrollContent;
class DELPHICLASS TALScrollCalculations;
class DELPHICLASS TALCustomScrollBox;
class DELPHICLASS TALScrollBox;
class DELPHICLASS TALVertScrollBox;
class DELPHICLASS TALHorzScrollBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TALScrollContent : public Fmx::Controls::TContent
{
	typedef Fmx::Controls::TContent inherited;
	
private:
	TALCustomScrollBox* FScrollBox;
	bool FIsContentChanged;
	
protected:
	virtual System::Types::TRectF __fastcall GetClipRect(void);
	virtual System::Types::TRectF __fastcall GetChildrenRect(void);
	virtual Fmx::Types::_di_IControl __fastcall ObjectAtPoint(const System::Types::TPointF &P);
	virtual System::Types::TRectF __fastcall DoGetUpdateRect(void);
	virtual void __fastcall DoAddObject(Fmx::Types::TFmxObject* const AObject);
	virtual void __fastcall DoInsertObject(int Index, Fmx::Types::TFmxObject* const AObject);
	virtual void __fastcall DoRemoveObject(Fmx::Types::TFmxObject* const AObject);
	virtual void __fastcall DoRealign(void);
	virtual void __fastcall ContentChanged(void);
	__property bool IsContentChanged = {read=FIsContentChanged, write=FIsContentChanged, nodefault};
	
public:
	__fastcall virtual TALScrollContent(System::Classes::TComponent* AOwner);
	__property TALCustomScrollBox* ScrollBox = {read=FScrollBox};
	virtual bool __fastcall PointInObjectLocal(float X, float Y);
public:
	/* TControl.Destroy */ inline __fastcall virtual ~TALScrollContent(void) { }
	
};


class PASCALIMPLEMENTATION TALScrollCalculations : public Alfmxinertialmovement::TALAniCalculations
{
	typedef Alfmxinertialmovement::TALAniCalculations inherited;
	
private:
	TALCustomScrollBox* FScrollBox;
	
protected:
	virtual void __fastcall DoChanged(void);
	virtual void __fastcall DoStart(void);
	virtual void __fastcall DoStop(void);
	
public:
	__fastcall virtual TALScrollCalculations(System::Classes::TPersistent* AOwner);
	__property TALCustomScrollBox* ScrollBox = {read=FScrollBox};
public:
	/* TALAniCalculations.Destroy */ inline __fastcall virtual ~TALScrollCalculations(void) { }
	
};


typedef void __fastcall (__closure *TALPositionChangeEvent)(System::TObject* Sender, const System::Types::TPointF &OldViewportPosition, const System::Types::TPointF &NewViewportPosition, const bool ContentSizeChanged);

typedef void __fastcall (__closure *TALOnCalcContentBoundsEvent)(System::TObject* Sender, System::Types::TRectF &ContentBounds);

class PASCALIMPLEMENTATION TALCustomScrollBox : public Fmx::Controls::TStyledControl
{
	typedef Fmx::Controls::TStyledControl inherited;
	
	
private:
	struct DECLSPEC_DRECORD TScrollInfo
	{
	public:
		Fmx::Stdctrls::TScrollBar* Scroll;
		Fmx::Types::TAlignLayout Align;
		System::Types::TRectF Margins;
	};
	
	
	typedef System::DynamicArray<TScrollInfo> _TALCustomScrollBox__1;
	
	typedef System::DynamicArray<TScrollInfo> _TALCustomScrollBox__2;
	
	
private:
	static const System::Int8 SmallChangeFraction = System::Int8(0x5);
	
	static System::Uitypes::TAlphaColor DesignBorderColor;
	Fmx::Platform::_di_IFMXSystemInformationService FSystemInfoSrv;
	bool FDisableMouseWheel;
	TALScrollCalculations* FAniCalculations;
	System::Types::TPointF FLastViewportPosition;
	bool FInInternalAlign;
	Fmx::Controls::TControl* FBackground;
	TALScrollContent* FContent;
	Fmx::Controls::TControl* FContentLayout;
	System::Types::TRectF FContentBounds;
	System::Types::TSizeF FCachedContentSize;
	bool FCachedAutoShowing;
	System::Types::TSizeF FOriginalContentLayoutSize;
	bool FShowScrollBars;
	bool FAutoHide;
	_TALCustomScrollBox__1 FHScrollInfo;
	_TALCustomScrollBox__2 FVScrollInfo;
	System::Types::TRectF FContentMargins;
	bool FVDisablePaint;
	bool FHDisablePaint;
	bool FGDisablePaint;
	Fmx::Controls::TControl* FSizeGripContent;
	Fmx::Controls::TControl* FSizeGripParent;
	Fmx::Controls::TControl* FSizeGrip;
	bool FShowSizeGrip;
	TALPositionChangeEvent FOnViewportPositionChange;
	System::Classes::TNotifyEvent FOnHScrollChange;
	System::Classes::TNotifyEvent FOnVScrollChange;
	TALOnCalcContentBoundsEvent FOnCalcContentBounds;
	bool FMouseEvents;
	bool FContentCalculated;
	System::Types::TPointF fMouseDownPos;
	int FDeadZoneBeforeAcquireScrolling;
	bool fScrollingAcquiredByMe;
	int fScrollingAcquiredByOtherMessageID;
	void __fastcall ScrollingAcquiredByOtherHandler(System::TObject* const Sender, System::Messaging::TMessageBase* const M);
	int __fastcall HScrollIndex(void);
	int __fastcall VScrollIndex(void);
	Fmx::Types::TAlignLayout __fastcall GetHScrollAlign(void);
	Fmx::Types::TAlignLayout __fastcall GetVScrollAlign(void);
	System::Types::TRectF __fastcall GetHScrollMargins(void);
	System::Types::TRectF __fastcall GetVScrollMargins(void);
	float __fastcall GetSceneScale(void);
	void __fastcall SetShowScrollBars(const bool Value);
	void __fastcall SetShowSizeGrip(const bool Value);
	Fmx::Stdctrls::TScrollBar* __fastcall GetVScrollBar(void);
	Fmx::Stdctrls::TScrollBar* __fastcall GetHScrollBar(void);
	void __fastcall UpdateSizeGrip(void);
	void __fastcall UpdateVScrollBar(const float Value, const float ViewportSize);
	void __fastcall UpdateHScrollBar(const float Value, const float ViewportSize);
	void __fastcall InternalAlign(void);
	void __fastcall HScrollChangeProc(System::TObject* Sender);
	void __fastcall VScrollChangeProc(System::TObject* Sender);
	void __fastcall MousePosToAni(float &X, float &Y);
	void __fastcall SetAutoHide(const bool Value);
	void __fastcall SaveDisablePaint(void);
	void __fastcall RestoreDisablePaint(void);
	void __fastcall SetDisablePaint(void);
	System::Types::TPointF __fastcall GetViewportPosition(void);
	void __fastcall SetViewportPosition(const System::Types::TPointF &Value);
	void __fastcall StartScrolling(void);
	void __fastcall StopScrolling(void);
	void __fastcall UpdateOriginalContentLayoutSize(const bool Force);
	void __fastcall ReadPartSize(System::Classes::TReader* Reader, float &Size);
	void __fastcall ReadContentLayoutHeight(System::Classes::TReader* Reader);
	void __fastcall ReadContentLayoutWidth(System::Classes::TReader* Reader);
	void __fastcall WriteContentLayoutHeight(System::Classes::TWriter* Writer);
	void __fastcall WriteContentLayoutWidth(System::Classes::TWriter* Writer);
	
protected:
	virtual void __fastcall AniMouseDown(const bool Touch, const float X, const float Y);
	virtual void __fastcall AniMouseMove(const bool Touch, const float X, const float Y);
	virtual void __fastcall AniMouseUp(const bool Touch, const float X, const float Y);
	Fmx::Platform::TScrollingBehaviours __fastcall GetScrollingBehaviours(void);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall PaddingChanged(void)/* overload */;
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual void __fastcall DoAddObject(Fmx::Types::TFmxObject* const AObject);
	virtual void __fastcall DoRealign(void);
	virtual bool __fastcall IsAddToContent(Fmx::Types::TFmxObject* const AObject);
	virtual void __fastcall ContentAddObject(Fmx::Types::TFmxObject* const AObject);
	virtual void __fastcall ContentInsertObject(int Index, Fmx::Types::TFmxObject* const AObject);
	virtual void __fastcall ContentBeforeRemoveObject(Fmx::Types::TFmxObject* AObject);
	virtual void __fastcall ContentRemoveObject(Fmx::Types::TFmxObject* const AObject);
	virtual void __fastcall HScrollChange(void);
	virtual void __fastcall VScrollChange(void);
	virtual void __fastcall ViewportPositionChange(const System::Types::TPointF &OldViewportPosition, const System::Types::TPointF &NewViewportPosition, const bool ContentSizeChanged);
	virtual void __fastcall CMGesture(Fmx::Types::TGestureEventInfo &EventInfo);
	virtual void __fastcall Painting(void);
	virtual void __fastcall AfterPaint(void);
	virtual void __fastcall ApplyStyle(void);
	virtual void __fastcall FreeStyle(void);
	virtual bool __fastcall IsOpaque(void);
	System::Types::TRectF __fastcall ContentRect(void);
	float __fastcall VScrollBarValue(void);
	float __fastcall HScrollBarValue(void);
	virtual TALScrollContent* __fastcall CreateScrollContent(void);
	virtual TALScrollCalculations* __fastcall CreateAniCalculations(void);
	virtual void __fastcall DoUpdateAniCalculations(TALScrollCalculations* const AAniCalculations);
	void __fastcall UpdateAniCalculations(void);
	virtual System::Types::TRectF __fastcall DoCalcContentBounds(void);
	virtual void __fastcall DoRealignContent(const System::Types::TRectF &R);
	System::Types::TRectF __fastcall GetContentBounds(void);
	virtual void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall DoMouseLeave(void);
	virtual void __fastcall MouseWheel(System::Classes::TShiftState Shift, int WheelDelta, bool &Handled);
	__property Fmx::Controls::TControl* ContentLayout = {read=FContentLayout};
	__property TALScrollContent* Content = {read=FContent};
	__property Fmx::Types::TAlignLayout HScrollAlign = {read=GetHScrollAlign, nodefault};
	__property Fmx::Types::TAlignLayout VScrollAlign = {read=GetVScrollAlign, nodefault};
	__property System::Types::TRectF HScrollMargins = {read=GetHScrollMargins};
	__property System::Types::TRectF VScrollMargins = {read=GetVScrollMargins};
	__property bool InInternalAlign = {read=FInInternalAlign, nodefault};
	__property Fmx::Stdctrls::TScrollBar* HScrollBar = {read=GetHScrollBar};
	__property Fmx::Stdctrls::TScrollBar* VScrollBar = {read=GetVScrollBar};
	
public:
	__fastcall virtual TALCustomScrollBox(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual ~TALCustomScrollBox(void);
	__property TALScrollCalculations* AniCalculations = {read=FAniCalculations};
	__property System::Types::TPointF ViewportPosition = {read=GetViewportPosition, write=SetViewportPosition};
	virtual void __fastcall Sort(Fmx::Types::_di_TFmxObjectSortCompare Compare);
	void __fastcall Center(void);
	void __fastcall ScrollTo _DEPRECATED_ATTRIBUTE1("use ScrollBy(const Dx, Dy: Single)") (const float Dx, const float Dy);
	void __fastcall ScrollBy(const float Dx, const float Dy);
	void __fastcall InViewRect(const System::Types::TRectF &Rect);
	float __fastcall ClientWidth(void);
	float __fastcall ClientHeight(void);
	virtual Fmx::Types::_di_ITabList __fastcall GetTabList(void);
	__property System::Types::TRectF ContentBounds = {read=GetContentBounds};
	void __fastcall InvalidateContentSize(void);
	void __fastcall RealignContent(void);
	__property bool AutoHide = {read=FAutoHide, write=SetAutoHide, default=1};
	__property bool DisableMouseWheel = {read=FDisableMouseWheel, write=FDisableMouseWheel, default=0};
	__property bool ShowScrollBars = {read=FShowScrollBars, write=SetShowScrollBars, default=1};
	__property bool ShowSizeGrip = {read=FShowSizeGrip, write=SetShowSizeGrip, default=0};
	__property TALPositionChangeEvent OnViewportPositionChange = {read=FOnViewportPositionChange, write=FOnViewportPositionChange};
	__property System::Classes::TNotifyEvent OnHScrollChange = {read=FOnHScrollChange, write=FOnHScrollChange};
	__property System::Classes::TNotifyEvent OnVScrollChange = {read=FOnVScrollChange, write=FOnVScrollChange};
	__property TALOnCalcContentBoundsEvent OnCalcContentBounds = {read=FOnCalcContentBounds, write=FOnCalcContentBounds};
	__property int DeadZoneBeforeAcquireScrolling = {read=FDeadZoneBeforeAcquireScrolling, write=FDeadZoneBeforeAcquireScrolling, default=16};
	
__published:
	__property Align = {default=0};
	__property Anchors;
	__property ClipChildren = {default=0};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property DragMode = {default=0};
	__property EnableDragHighlight = {default=1};
	__property Enabled;
	__property Locked = {default=0};
	__property Height;
	__property HelpContext = {default=0};
	__property HelpKeyword = {default=0};
	__property HelpType = {default=1};
	__property HitTest = {default=1};
	__property Padding;
	__property Opacity;
	__property Margins;
	__property PopupMenu;
	__property Position;
	__property RotationAngle = {default=0};
	__property RotationCenter;
	__property Scale;
	__property Size;
	__property StyleLookup = {default=0};
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TouchTargetExpansion;
	__property Visible;
	__property Width;
	__property OnApplyStyleLookup;
	__property OnPainting;
	__property OnPaint;
	__property OnResize;
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
};


class PASCALIMPLEMENTATION TALScrollBox : public TALCustomScrollBox
{
	typedef TALCustomScrollBox inherited;
	
protected:
	virtual void __fastcall Paint(void);
	
public:
	__property Content;
	
__published:
	__property Align = {default=0};
	__property Anchors;
	__property AutoHide = {default=1};
	__property ClipChildren = {default=0};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property DisableMouseWheel = {default=0};
	__property DragMode = {default=0};
	__property Enabled;
	__property EnableDragHighlight = {default=1};
	__property Height;
	__property HelpContext = {default=0};
	__property HelpKeyword = {default=0};
	__property HelpType = {default=1};
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
	__property ShowSizeGrip = {default=0};
	__property Size;
	__property StyleLookup = {default=0};
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TouchTargetExpansion;
	__property Visible;
	__property Width;
	__property OnApplyStyleLookup;
	__property OnPainting;
	__property OnPaint;
	__property OnResize;
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
	__property OnHScrollChange;
	__property OnVScrollChange;
	__property OnCalcContentBounds;
public:
	/* TALCustomScrollBox.Create */ inline __fastcall virtual TALScrollBox(System::Classes::TComponent* AOwner)/* overload */ : TALCustomScrollBox(AOwner) { }
	/* TALCustomScrollBox.Destroy */ inline __fastcall virtual ~TALScrollBox(void) { }
	
};


class PASCALIMPLEMENTATION TALVertScrollBox : public TALCustomScrollBox
{
	typedef TALCustomScrollBox inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetDefaultStyleLookupName(void);
	virtual System::Types::TRectF __fastcall DoCalcContentBounds(void);
	virtual void __fastcall Paint(void);
	virtual void __fastcall DoUpdateAniCalculations(TALScrollCalculations* const AAniCalculations);
	
public:
	__property Content;
	
__published:
	__property Align = {default=0};
	__property Anchors;
	__property AutoHide = {default=1};
	__property ClipChildren = {default=0};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property DisableMouseWheel = {default=0};
	__property DragMode = {default=0};
	__property Enabled;
	__property EnableDragHighlight = {default=1};
	__property Height;
	__property HelpContext = {default=0};
	__property HelpKeyword = {default=0};
	__property HelpType = {default=1};
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
	__property ShowSizeGrip = {default=0};
	__property Size;
	__property StyleLookup = {default=0};
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TouchTargetExpansion;
	__property Visible;
	__property Width;
	__property OnApplyStyleLookup;
	__property OnPainting;
	__property OnPaint;
	__property OnResize;
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
	__property OnVScrollChange;
	__property OnCalcContentBounds;
public:
	/* TALCustomScrollBox.Create */ inline __fastcall virtual TALVertScrollBox(System::Classes::TComponent* AOwner)/* overload */ : TALCustomScrollBox(AOwner) { }
	/* TALCustomScrollBox.Destroy */ inline __fastcall virtual ~TALVertScrollBox(void) { }
	
};


class PASCALIMPLEMENTATION TALHorzScrollBox : public TALCustomScrollBox
{
	typedef TALCustomScrollBox inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetDefaultStyleLookupName(void);
	virtual System::Types::TRectF __fastcall DoCalcContentBounds(void);
	virtual void __fastcall Paint(void);
	virtual void __fastcall DoUpdateAniCalculations(TALScrollCalculations* const AAniCalculations);
	
public:
	__property Content;
	
__published:
	__property Align = {default=0};
	__property Anchors;
	__property AutoHide = {default=1};
	__property ClipChildren = {default=0};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property DisableMouseWheel = {default=0};
	__property DragMode = {default=0};
	__property Enabled;
	__property EnableDragHighlight = {default=1};
	__property Height;
	__property HelpContext = {default=0};
	__property HelpKeyword = {default=0};
	__property HelpType = {default=1};
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
	__property ShowSizeGrip = {default=0};
	__property Size;
	__property StyleLookup = {default=0};
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TouchTargetExpansion;
	__property Visible;
	__property Width;
	__property OnApplyStyleLookup;
	__property OnPainting;
	__property OnPaint;
	__property OnResize;
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
	__property OnHScrollChange;
	__property OnCalcContentBounds;
public:
	/* TALCustomScrollBox.Create */ inline __fastcall virtual TALHorzScrollBox(System::Classes::TComponent* AOwner)/* overload */ : TALCustomScrollBox(AOwner) { }
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
