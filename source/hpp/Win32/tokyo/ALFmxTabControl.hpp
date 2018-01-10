// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxTabControl.pas' rev: 32.00 (Windows)

#ifndef AlfmxtabcontrolHPP
#define AlfmxtabcontrolHPP

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
#include <System.Messaging.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Ani.hpp>
#include <ALFmxLayouts.hpp>
#include <ALFmxInertialMovement.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxtabcontrol
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALTabItem;
class DELPHICLASS TALTabControl;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TALTabItemClass;

enum class DECLSPEC_DENUM TALTabTransition : unsigned char { None, Slide, FadeOut };

typedef void __fastcall (__closure *TALTabPositionChangeEvent)(System::TObject* Sender, const System::Types::TPointF &OldViewportPosition, const System::Types::TPointF &NewViewportPosition);

typedef void __fastcall (__closure *TALTabAniTransitionInit)(System::TObject* const sender, const TALTabTransition ATransition, const double aVelocity, Fmx::Ani::TFloatAnimation* const aAnimation);

class PASCALIMPLEMENTATION TALTabItem : public Fmx::Controls::TControl
{
	typedef Fmx::Controls::TControl inherited;
	
private:
	TALTabControl* FTabControl;
	bool FIsSelected;
	float fViewPortOffset;
	void __fastcall SetIsSelected(const bool Value);
	void __fastcall SetSelectedInternal(const bool Value);
	
protected:
	virtual void __fastcall ParentChanged(void);
	virtual void __fastcall DoRealign(void);
	__property Align = {default=0};
	__property RotationAngle = {default=0};
	__property RotationCenter;
	__property Position;
	__property Margins;
	
public:
	__fastcall virtual TALTabItem(System::Classes::TComponent* AOwner);
	virtual void __fastcall SetBounds(float X, float Y, float AWidth, float AHeight);
	__property TALTabControl* TabControl = {read=FTabControl};
	__property float ViewPortOffset = {read=fViewPortOffset};
	
__published:
	__property ClipChildren = {default=0};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property DragMode = {default=0};
	__property EnableDragHighlight = {default=1};
	__property Enabled = {default=1};
	__property HitTest = {default=0};
	__property bool IsSelected = {read=FIsSelected, write=SetIsSelected, nodefault};
	__property Index = {stored=false};
	__property Padding;
	__property Opacity;
	__property PopupMenu;
	__property Visible = {default=1};
	__property ParentShowHint = {default=1};
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
	__property OnPainting;
	__property OnPaint;
	__property OnResize;
	__property OnResized;
public:
	/* TControl.Destroy */ inline __fastcall virtual ~TALTabItem(void) { }
	
};


class PASCALIMPLEMENTATION TALTabControl : public Fmx::Controls::TControl
{
	typedef Fmx::Controls::TControl inherited;
	
	
public:
	enum class DECLSPEC_DENUM TFindKind : unsigned char { Next, Back, First, Last, Current };
	
	
private:
	int FTabCount;
	bool FMouseEvents;
	bool fGestureEvents;
	bool FchildreenChanging;
	int FTabIndex;
	bool FRealigningTabs;
	System::Classes::TNotifyEvent FOnChange;
	Alfmxinertialmovement::TALAniCalculations* FAniCalculations;
	System::Types::TPointF fLastViewportPosition;
	TALTabPositionChangeEvent FOnViewportPositionChange;
	Fmx::Ani::TFloatAnimation* FAniTransition;
	Alfmxlayouts::TALLayout* FAniTransitionOverlay;
	TALTabAniTransitionInit fOnAniTransitionInit;
	System::Classes::TNotifyEvent fOnAniStart;
	System::Classes::TNotifyEvent fOnAniStop;
	float fMouseDownPos;
	int FDeadZoneBeforeAcquireScrolling;
	bool fScrollingAcquiredByMe;
	bool fScrollingAcquiredByOther;
	int fScrollingAcquiredByOtherMessageID;
	void __fastcall setScrollingAcquiredByMe(const bool Value);
	void __fastcall ScrollingAcquiredByOtherHandler(System::TObject* const Sender, System::Messaging::TMessageBase* const M);
	void __fastcall SetTabIndex(const int Value);
	TALTabItem* __fastcall GetActiveTab(void);
	void __fastcall SetActiveTab(TALTabItem* const Value);
	bool __fastcall getAnimationEnabled(void);
	void __fastcall setAnimationEnabled(const bool Value);
	void __fastcall AniTransitionSlideProcess(System::TObject* Sender);
	void __fastcall AniTransitionSlideFinish(System::TObject* Sender);
	void __fastcall AniTransitionFadeOutFinish(System::TObject* Sender);
	int __fastcall GetItemsCount(void);
	Fmx::Types::TFmxObject* __fastcall GetItem(const int AIndex);
	
protected:
	virtual void __fastcall Paint(void);
	TALTabItem* __fastcall GetTabItem(int AIndex);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall RealignTabs(void);
	virtual void __fastcall ChangeChildren(void);
	virtual void __fastcall DoAddObject(Fmx::Types::TFmxObject* const AObject);
	virtual void __fastcall DoInsertObject(int Index, Fmx::Types::TFmxObject* const AObject);
	virtual void __fastcall DoRemoveObject(Fmx::Types::TFmxObject* const AObject);
	virtual void __fastcall DoDeleteChildren(void);
	virtual void __fastcall DoChange(void);
	virtual void __fastcall CMGesture(Fmx::Types::TGestureEventInfo &EventInfo);
	virtual void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall DoMouseLeave(void);
	virtual void __fastcall Resize(void);
	__property ClipChildren = {default=1};
	__property Padding;
	__property AutoCapture = {default=0};
	
public:
	__fastcall virtual TALTabControl(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALTabControl(void);
	bool __fastcall SetActiveTabWithTransition(TALTabItem* const ATab, const TALTabTransition ATransition, const double AVelocity = 0.000000E+00, const bool ALaunchAniStartEvent = true)/* overload */;
	bool __fastcall SetActiveTabWithTransition(const int ATabIndex, TALTabTransition ATransition)/* overload */;
	bool __fastcall FindVisibleTab(int &Index, const TFindKind FindKind)/* overload */;
	int __fastcall FindVisibleTab(const TFindKind FindKind)/* overload */;
	bool __fastcall Next(TALTabTransition ATransition = (TALTabTransition)(0x1));
	bool __fastcall Previous(TALTabTransition ATransition = (TALTabTransition)(0x1));
	bool __fastcall First(TALTabTransition ATransition = (TALTabTransition)(0x1));
	bool __fastcall Last(TALTabTransition ATransition = (TALTabTransition)(0x1));
	bool __fastcall Delete(const int Index);
	TALTabItem* __fastcall Add(const TALTabItemClass TabClass = 0x0);
	HIDESBASE TALTabItem* __fastcall Insert(const int Index, const TALTabItemClass TabClass = 0x0);
	bool __fastcall HasActiveTab(void);
	__property int TabCount = {read=FTabCount, nodefault};
	__property TALTabItem* Tabs[int AIndex] = {read=GetTabItem};
	__property Alfmxinertialmovement::TALAniCalculations* AniCalculations = {read=FAniCalculations};
	__property Fmx::Ani::TFloatAnimation* AniTransition = {read=FAniTransition};
	__property int DeadZoneBeforeAcquireScrolling = {read=FDeadZoneBeforeAcquireScrolling, write=FDeadZoneBeforeAcquireScrolling, default=16};
	
__published:
	__property Align = {default=0};
	__property Anchors;
	__property TALTabItem* ActiveTab = {read=GetActiveTab, write=SetActiveTab, stored=false};
	__property bool AnimationEnabled = {read=getAnimationEnabled, write=setAnimationEnabled, default=1};
	__property Cursor = {default=0};
	__property DragMode = {default=0};
	__property EnableDragHighlight = {default=1};
	__property Enabled = {default=1};
	__property Locked = {default=0};
	__property Height;
	__property HitTest = {default=1};
	__property Opacity;
	__property Margins;
	__property PopupMenu;
	__property Position;
	__property RotationAngle = {default=0};
	__property RotationCenter;
	__property Scale;
	__property Size;
	__property int TabIndex = {read=FTabIndex, write=SetTabIndex, default=-1};
	__property Visible = {default=1};
	__property Width;
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
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
	__property OnPainting;
	__property OnPaint;
	__property OnResize;
	__property OnResized;
	__property TALTabPositionChangeEvent OnViewportPositionChange = {read=FOnViewportPositionChange, write=FOnViewportPositionChange};
	__property TALTabAniTransitionInit OnAniTransitionInit = {read=fOnAniTransitionInit, write=fOnAniTransitionInit};
	__property System::Classes::TNotifyEvent OnAniStart = {read=fOnAniStart, write=fOnAniStart};
	__property System::Classes::TNotifyEvent OnAniStop = {read=fOnAniStop, write=fOnAniStop};
private:
	void *__IItemsContainer;	// Fmx::Types::IItemsContainer 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {100B2F87-5DCB-4699-B751-B4439588E82A}
	operator Fmx::Types::_di_IItemsContainer()
	{
		Fmx::Types::_di_IItemsContainer intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Types::IItemsContainer*(void) { return (Fmx::Types::IItemsContainer*)&__IItemsContainer; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alfmxtabcontrol */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXTABCONTROL)
using namespace Alfmxtabcontrol;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxtabcontrolHPP
