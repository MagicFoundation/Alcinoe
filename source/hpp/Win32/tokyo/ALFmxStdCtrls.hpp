// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxStdCtrls.pas' rev: 32.00 (Windows)

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
#include <System.Math.hpp>
#include <System.Rtti.hpp>
#include <System.Messaging.hpp>
#include <FMX.Types.hpp>
#include <FMX.StdActns.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.ActnList.hpp>
#include <FMX.ImgList.hpp>
#include <ALFmxInertialMovement.hpp>
#include <ALFmxObjects.hpp>
#include <FMX.Objects.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxstdctrls
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALAniIndicator;
class DELPHICLASS TALTrackThumbGlyph;
class DELPHICLASS TALTrackThumb;
class DELPHICLASS TALTrackBackground;
class DELPHICLASS TALTrackHighlight;
class DELPHICLASS TALCustomTrack;
class DELPHICLASS TALTrackBar;
class DELPHICLASS TALScrollBar;
class DELPHICLASS TALRangeTrackBar;
class DELPHICLASS TALCheckBox;
class DELPHICLASS TALRadioButton;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TALAniIndicator : public Fmx::Controls::TControl
{
	typedef Fmx::Controls::TControl inherited;
	
private:
	Fmx::Types::TTimer* fTimer;
	int finterval;
	int FFrameCount;
	int FRowCount;
	System::UnicodeString fResourceName;
	System::Types::TSmallPoint fFrameIndex;
	float FScreenScale;
	Fmx::Graphics::TBitmap* fBufBitmap;
	System::Types::TRectF fBufBitmapRect;
	System::Types::TSizeF fBufSize;
	void __fastcall setResourceName(const System::UnicodeString Value);
	void __fastcall onTimer(System::TObject* sender);
	bool __fastcall ResourceNameStored(void);
	
protected:
	virtual void __fastcall Paint(void);
	__property Fmx::Graphics::TBitmap* BufBitmap = {read=fBufBitmap};
	virtual bool __fastcall EnabledStored(void);
	virtual void __fastcall SetEnabled(const bool Value);
	virtual System::Types::TSizeF __fastcall GetDefaultSize(void);
	
public:
	__fastcall virtual TALAniIndicator(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALAniIndicator(void);
	virtual Fmx::Graphics::TBitmap* __fastcall MakeBufBitmap(void);
	virtual void __fastcall clearBufBitmap(void);
	
__published:
	__property Align = {default=0};
	__property Anchors;
	__property Cursor = {default=0};
	__property DragMode = {default=0};
	__property EnableDragHighlight = {default=1};
	__property Enabled = {default=0};
	__property Locked = {default=0};
	__property Height;
	__property Hint = {default=0};
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
	__property TouchTargetExpansion;
	__property Visible = {default=1};
	__property Width;
	__property int FrameCount = {read=FFrameCount, write=FFrameCount, default=20};
	__property int RowCount = {read=FRowCount, write=FRowCount, default=4};
	__property int interval = {read=finterval, write=finterval, default=50};
	__property System::UnicodeString ResourceName = {read=fResourceName, write=setResourceName, stored=ResourceNameStored};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property OnDragEnter;
	__property OnDragLeave;
	__property OnDragOver;
	__property OnDragDrop;
	__property OnDragEnd;
	__property OnKeyDown;
	__property OnKeyUp;
	__property OnCanFocus;
	__property OnEnter;
	__property OnExit;
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
};


class PASCALIMPLEMENTATION TALTrackThumbGlyph : public Alfmxobjects::TALImage
{
	typedef Alfmxobjects::TALImage inherited;
	
public:
	__fastcall virtual TALTrackThumbGlyph(System::Classes::TComponent* AOwner);
	
__published:
	__property Align = {default=9};
	__property Locked = {default=1};
	__property HitTest = {default=0};
public:
	/* TALImage.Destroy */ inline __fastcall virtual ~TALTrackThumbGlyph(void) { }
	
};


class PASCALIMPLEMENTATION TALTrackThumb : public Alfmxobjects::TALRectangle
{
	typedef Alfmxobjects::TALRectangle inherited;
	
private:
	Fmx::Stdactns::TValueRange* fValueRange;
	TALCustomTrack* FTrack;
	TALTrackThumbGlyph* FGlyph;
	System::Types::TPointF FDownOffset;
	float fTrackDownOffset;
	bool FPressed;
	int FDeadZoneBeforeAcquireScrolling;
	bool fScrollingAcquiredByMe;
	bool fScrollingAcquiredByOther;
	int fScrollingAcquiredByOtherMessageID;
	void __fastcall setScrollingAcquiredByMe(const bool Value);
	void __fastcall ScrollingAcquiredByOtherHandler(System::TObject* const Sender, System::Messaging::TMessageBase* const M);
	float __fastcall PointToValue(float X, float Y);
	
public:
	__fastcall TALTrackThumb(TALCustomTrack* const ATrack, Fmx::Stdactns::TValueRange* const aValueRange, const bool aWithGlyphObj);
	__fastcall virtual ~TALTrackThumb(void);
	virtual void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall DoMouseLeave(void);
	virtual System::Types::TRectF __fastcall GetDefaultTouchTargetExpansion(void);
	__property bool IsPressed = {read=FPressed, nodefault};
	__property int DeadZoneBeforeAcquireScrolling = {read=FDeadZoneBeforeAcquireScrolling, write=FDeadZoneBeforeAcquireScrolling, default=5};
	
__published:
	__property TouchTargetExpansion;
	__property Locked = {default=1};
	__property Position = {stored=false};
	__property Size = {stored=false};
	__property TALTrackThumbGlyph* Glyph = {read=FGlyph};
	__property Cursor = {default=-21};
};


class PASCALIMPLEMENTATION TALTrackBackground : public Alfmxobjects::TALRectangle
{
	typedef Alfmxobjects::TALRectangle inherited;
	
public:
	__fastcall virtual TALTrackBackground(System::Classes::TComponent* AOwner);
	
__published:
	__property Locked = {default=1};
	__property HitTest = {default=0};
public:
	/* TALRectangle.Destroy */ inline __fastcall virtual ~TALTrackBackground(void) { }
	
};


class PASCALIMPLEMENTATION TALTrackHighlight : public Alfmxobjects::TALRectangle
{
	typedef Alfmxobjects::TALRectangle inherited;
	
public:
	__fastcall virtual TALTrackHighlight(System::Classes::TComponent* AOwner);
	
__published:
	__property Locked = {default=1};
	__property Position = {stored=false};
	__property HitTest = {default=0};
public:
	/* TALRectangle.Destroy */ inline __fastcall virtual ~TALTrackHighlight(void) { }
	
};


class PASCALIMPLEMENTATION TALCustomTrack : public Fmx::Controls::TControl
{
	typedef Fmx::Controls::TControl inherited;
	
private:
	Fmx::Stdactns::TValueRange* FValueRange;
	Fmx::Stdactns::TBaseValueRange* FDefaultValueRange;
	bool __fastcall GetIsTracking(void);
	Fmx::Stdactns::TCustomValueRange* __fastcall GetValueRange(void);
	void __fastcall SetValueRange(Fmx::Stdactns::TCustomValueRange* const AValue);
	void __fastcall SetValueRange_(Fmx::Stdactns::TValueRange* const Value);
	bool __fastcall FrequencyStored(void);
	bool __fastcall MaxStored(void);
	bool __fastcall MinStored(void);
	void __fastcall SetThumbSize(const float Value);
	bool __fastcall ThumbSizeStored(void);
	bool __fastcall ViewportSizeStored(void);
	
protected:
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnTracking;
	bool FIgnoreViewportSize;
	Fmx::Controls::TOrientation FOrientation;
	bool FTracking;
	float FThumbSize;
	float FMinThumbSize;
	TALTrackThumb* FThumb;
	TALTrackBackground* FBackGround;
	TALTrackHighlight* FHighlight;
	virtual void __fastcall SetViewportSize(const float Value);
	virtual float __fastcall GetViewportSize(void);
	virtual float __fastcall GetFrequency(void);
	virtual void __fastcall SetFrequency(const float Value);
	virtual float __fastcall GetMax(void);
	virtual void __fastcall SetMax(const float Value);
	virtual float __fastcall GetMin(void);
	virtual void __fastcall SetMin(const float Value);
	virtual float __fastcall GetValue(void);
	virtual void __fastcall SetValue(float Value);
	virtual bool __fastcall ValueStored(void);
	virtual System::Rtti::TValue __fastcall GetData(void);
	virtual void __fastcall SetData(const System::Rtti::TValue &Value);
	virtual void __fastcall SetOrientation(const Fmx::Controls::TOrientation Value);
	virtual System::Types::TRectF __fastcall GetThumbRect(const float Value, TALTrackThumb* const aThumb)/* overload */;
	virtual void __fastcall KeyDown(System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	virtual System::Types::TRectF __fastcall GetDefaultTouchTargetExpansion(void);
	virtual int __fastcall GetThumbSize(bool &IgnoreViewportSize);
	virtual void __fastcall DoRealign(void);
	__property bool IsTracking = {read=GetIsTracking, nodefault};
	virtual void __fastcall Loaded(void);
	virtual void __fastcall DoChanged(void);
	virtual void __fastcall DoTracking(void);
	virtual Fmx::Stdactns::TValueRange* __fastcall CreateValueRangeTrack(void);
	__property Fmx::Stdactns::TBaseValueRange* DefaultValueRange = {read=FDefaultValueRange};
	__property Fmx::Stdactns::TValueRange* ValueRange = {read=FValueRange, write=SetValueRange_, stored=ValueStored};
	__property float Value = {read=GetValue, write=SetValue, stored=ValueStored};
	__property TALTrackThumb* Thumb = {read=FThumb};
	virtual void __fastcall UpdateHighlight(void);
	
public:
	__fastcall virtual TALCustomTrack(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALCustomTrack(void);
	virtual void __fastcall AfterConstruction(void);
	__property float Min = {read=GetMin, write=SetMin, stored=MinStored};
	__property float Max = {read=GetMax, write=SetMax, stored=MaxStored};
	__property float Frequency = {read=GetFrequency, write=SetFrequency, stored=FrequencyStored};
	__property float ViewportSize = {read=GetViewportSize, write=SetViewportSize, stored=ViewportSizeStored};
	__property Fmx::Controls::TOrientation Orientation = {read=FOrientation, write=SetOrientation, nodefault};
	__property bool Tracking = {read=FTracking, write=FTracking, default=1};
	__property float ThumbSize = {read=FThumbSize, write=SetThumbSize, stored=ThumbSizeStored};
	__property TALTrackBackground* BackGround = {read=FBackGround};
	__property TALTrackHighlight* Highlight = {read=FHighlight};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnTracking = {read=FOnTracking, write=FOnTracking};
private:
	void *__IValueRange;	// Fmx::Stdactns::IValueRange 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {6DFA65EF-A8BF-4D58-9655-664B50C30312}
	operator Fmx::Stdactns::_di_IValueRange()
	{
		Fmx::Stdactns::_di_IValueRange intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Stdactns::IValueRange*(void) { return (Fmx::Stdactns::IValueRange*)&__IValueRange; }
	#endif
	
};


class PASCALIMPLEMENTATION TALTrackBar : public TALCustomTrack
{
	typedef TALCustomTrack inherited;
	
protected:
	virtual System::Types::TSizeF __fastcall GetDefaultSize(void);
	
public:
	__fastcall virtual TALTrackBar(System::Classes::TComponent* AOwner);
	__property ValueRange;
	
__published:
	__property ThumbSize = {default=0};
	__property Thumb;
	__property BackGround;
	__property Highlight;
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
	__property Frequency;
	__property Locked = {default=0};
	__property Height;
	__property Hint = {default=0};
	__property HitTest = {default=1};
	__property Padding;
	__property Min;
	__property Max;
	__property Orientation;
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
	__property Tracking = {default=1};
	__property Value;
	__property Visible = {default=1};
	__property Width;
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property OnChange;
	__property OnTracking;
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
	__property OnResized;
public:
	/* TALCustomTrack.Destroy */ inline __fastcall virtual ~TALTrackBar(void) { }
	
};


class PASCALIMPLEMENTATION TALScrollBar : public TALCustomTrack
{
	typedef TALCustomTrack inherited;
	
protected:
	virtual System::Types::TSizeF __fastcall GetDefaultSize(void);
	
public:
	__fastcall virtual TALScrollBar(System::Classes::TComponent* AOwner);
	__property ValueRange;
	
__published:
	__property Thumb;
	__property BackGround;
	__property Align = {default=0};
	__property Anchors;
	__property CanFocus = {default=0};
	__property CanParentFocus = {default=0};
	__property ClipChildren = {default=0};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property DragMode = {default=0};
	__property EnableDragHighlight = {default=1};
	__property Enabled;
	__property Locked = {default=0};
	__property Height;
	__property HitTest = {default=1};
	__property Padding;
	__property Min;
	__property Max;
	__property Orientation;
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
	__property Value;
	__property Visible = {default=1};
	__property Width;
	__property ViewportSize;
	__property OnChange;
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
	__property OnResized;
public:
	/* TALCustomTrack.Destroy */ inline __fastcall virtual ~TALScrollBar(void) { }
	
};


class PASCALIMPLEMENTATION TALRangeTrackBar : public TALCustomTrack
{
	typedef TALCustomTrack inherited;
	
private:
	Fmx::Stdactns::TValueRange* FMaxValueRange;
	
protected:
	TALTrackThumb* FMaxThumb;
	virtual void __fastcall SetViewportSize(const float Value);
	virtual void __fastcall SetFrequency(const float Value);
	virtual void __fastcall SetMax(const float Value);
	virtual void __fastcall SetMin(const float Value);
	virtual bool __fastcall MaxValueStored(void);
	virtual System::Types::TSizeF __fastcall GetDefaultSize(void);
	virtual void __fastcall SetValue(float Value);
	virtual float __fastcall GetMaxValue(void);
	virtual void __fastcall SetMaxValue(float Value);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall DoRealign(void);
	virtual void __fastcall UpdateHighlight(void);
	
public:
	__fastcall virtual TALRangeTrackBar(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALRangeTrackBar(void);
	
__published:
	__property ThumbSize = {default=0};
	__property TALTrackThumb* MinThumb = {read=FThumb};
	__property TALTrackThumb* MaxThumb = {read=FMaxThumb};
	__property BackGround;
	__property Highlight;
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
	__property Frequency;
	__property Locked = {default=0};
	__property Height;
	__property Hint = {default=0};
	__property HitTest = {default=1};
	__property Padding;
	__property Min;
	__property Max;
	__property float MinValue = {read=GetValue, write=SetValue, stored=ValueStored};
	__property float MaxValue = {read=GetMaxValue, write=SetMaxValue, stored=MaxValueStored};
	__property Orientation;
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
	__property Tracking = {default=1};
	__property Value;
	__property Visible = {default=1};
	__property Width;
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property OnChange;
	__property OnTracking;
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
	__property OnResized;
};


class PASCALIMPLEMENTATION TALCheckBox : public Fmx::Controls::TControl
{
	typedef Fmx::Controls::TControl inherited;
	
private:
	float FScreenScale;
	Fmx::Graphics::TBitmap* fBufBitmap;
	System::Types::TRectF fBufBitmapRect;
	System::Types::TSizeF fBufSize;
	System::UnicodeString FbufResourceName;
	bool FPressing;
	System::Classes::TNotifyEvent FOnChange;
	bool FIsPressed;
	bool FIsChecked;
	System::UnicodeString fImageCheckedResourceName;
	System::UnicodeString fImageUncheckedResourceName;
	Alfmxobjects::TALImageWrapMode FWrapMode;
	void __fastcall setImageCheckedResourceName(const System::UnicodeString Value);
	void __fastcall setImageUncheckedResourceName(const System::UnicodeString Value);
	void __fastcall SetWrapMode(const Alfmxobjects::TALImageWrapMode Value);
	
protected:
	virtual void __fastcall Paint(void);
	__property Fmx::Graphics::TBitmap* BufBitmap = {read=fBufBitmap};
	virtual void __fastcall DoChanged(void);
	virtual System::Types::TSizeF __fastcall GetDefaultSize(void);
	virtual bool __fastcall GetIsChecked(void);
	virtual void __fastcall SetIsChecked(const bool Value);
	virtual bool __fastcall ImageCheckedResourceNameStored(void);
	virtual bool __fastcall ImageUncheckedResourceNameStored(void);
	
public:
	__fastcall virtual TALCheckBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALCheckBox(void);
	virtual Fmx::Graphics::TBitmap* __fastcall MakeBufBitmap(void);
	virtual void __fastcall clearBufBitmap(void);
	virtual void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall KeyDown(System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	
__published:
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
	__property System::UnicodeString ImageCheckedResourceName = {read=fImageCheckedResourceName, write=setImageCheckedResourceName, stored=ImageCheckedResourceNameStored};
	__property System::UnicodeString ImageUncheckedResourceName = {read=fImageUncheckedResourceName, write=setImageUncheckedResourceName, stored=ImageUncheckedResourceNameStored};
	__property Alfmxobjects::TALImageWrapMode WrapMode = {read=FWrapMode, write=SetWrapMode, default=1};
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
	__property Visible = {default=1};
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
	__property OnResized;
};


class PASCALIMPLEMENTATION TALRadioButton : public TALCheckBox
{
	typedef TALCheckBox inherited;
	
private:
	System::UnicodeString FGroupName;
	bool fMandatory;
	System::UnicodeString __fastcall GetGroupName(void);
	void __fastcall SetGroupName(const System::UnicodeString Value);
	bool __fastcall GroupNameStored(void);
	void __fastcall GroupMessageCall(System::TObject* const Sender, System::Messaging::TMessageBase* const M);
	
protected:
	virtual void __fastcall SetIsChecked(const bool Value);
	virtual bool __fastcall ImageCheckedResourceNameStored(void);
	virtual bool __fastcall ImageUncheckedResourceNameStored(void);
	
public:
	__fastcall virtual TALRadioButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALRadioButton(void);
	
__published:
	__property System::UnicodeString GroupName = {read=GetGroupName, write=SetGroupName, stored=GroupNameStored};
	__property bool Mandatory = {read=fMandatory, write=fMandatory, default=0};
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
