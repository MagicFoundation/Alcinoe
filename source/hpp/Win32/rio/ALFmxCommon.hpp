// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxCommon.pas' rev: 33.00 (Windows)

#ifndef AlfmxcommonHPP
#define AlfmxcommonHPP

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
#include <System.Generics.Collections.hpp>
#include <System.Math.Vectors.hpp>
#include <FMX.Types.hpp>
#include <FMX.TextLayout.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.Effects.hpp>
#include <FMX.Controls.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxcommon
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALShadow;
class DELPHICLASS TALBreakTextItem;
class DELPHICLASS TALBreakTextItems;
struct TAlTextElement;
class DELPHICLASS TALDrawMultiLineTextOptions;
class DELPHICLASS TALControlAccessPrivate;
class DELPHICLASS TALTextLayoutAccessPrivate;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TALShadow : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool fenabled;
	float fblur;
	float fOffsetX;
	float fOffsetY;
	System::Uitypes::TAlphaColor fShadowColor;
	System::Classes::TNotifyEvent FOnChanged;
	void __fastcall SetEnabled(const bool Value);
	void __fastcall setblur(const float Value);
	void __fastcall setOffsetX(const float Value);
	void __fastcall setOffsetY(const float Value);
	void __fastcall setShadowColor(const System::Uitypes::TAlphaColor Value);
	bool __fastcall IsblurStored();
	bool __fastcall IsOffsetXStored();
	bool __fastcall IsOffsetYStored();
	
public:
	__fastcall TALShadow();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
	
__published:
	__property bool enabled = {read=fenabled, write=SetEnabled, default=0};
	__property float blur = {read=fblur, write=setblur, stored=IsblurStored};
	__property float OffsetX = {read=fOffsetX, write=setOffsetX, stored=IsOffsetXStored};
	__property float OffsetY = {read=fOffsetY, write=setOffsetY, stored=IsOffsetYStored};
	__property System::Uitypes::TAlphaColor ShadowColor = {read=fShadowColor, write=setShadowColor, default=-1778384896};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TALShadow() { }
	
};


typedef System::Uitypes::TFontName __fastcall (*TALCustomConvertFontFamilyProc)(const System::Uitypes::TFontName AFamily, const System::Uitypes::TFontStyles aFontStyles);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALBreakTextItem : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString Line;
	System::Types::TPointF pos;
	System::Types::TRectF rect;
	System::Uitypes::TAlphaColor fontColor;
	System::Uitypes::TFontStyles fontStyle;
	System::UnicodeString id;
	System::UnicodeString imgSrc;
	bool isEllipsis;
	__fastcall TALBreakTextItem();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALBreakTextItem() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TALBreakTextItems : public System::Generics::Collections::TObjectList__1<TALBreakTextItem*>
{
	typedef System::Generics::Collections::TObjectList__1<TALBreakTextItem*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<ALFmxCommon_TALBreakTextItem>.Create */ inline __fastcall TALBreakTextItems(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TALBreakTextItem*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<ALFmxCommon_TALBreakTextItem>.Create */ inline __fastcall TALBreakTextItems(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TALBreakTextItem*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TALBreakTextItem*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<ALFmxCommon_TALBreakTextItem>.Create */ inline __fastcall TALBreakTextItems(System::Generics::Collections::TEnumerable__1<TALBreakTextItem*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TALBreakTextItem*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<ALFmxCommon_TALBreakTextItem>.Destroy */ inline __fastcall virtual ~TALBreakTextItems() { }
	
public:
	/* {System_Generics_Collections}TList<ALFmxCommon_TALBreakTextItem>.Create */ inline __fastcall TALBreakTextItems()/* overload */ : System::Generics::Collections::TObjectList__1<TALBreakTextItem*>() { }
	/* {System_Generics_Collections}TList<ALFmxCommon_TALBreakTextItem>.Create */ inline __fastcall TALBreakTextItems(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TALBreakTextItem*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TALBreakTextItem*>(AComparer) { }
	/* {System_Generics_Collections}TList<ALFmxCommon_TALBreakTextItem>.Create */ inline __fastcall TALBreakTextItems(System::Generics::Collections::TEnumerable__1<TALBreakTextItem*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TALBreakTextItem*>(Collection) { }
	
};


struct DECLSPEC_DRECORD TAlTextElement
{
public:
	System::UnicodeString Id;
	System::Types::TRectF rect;
};


typedef System::DynamicArray<TAlTextElement> TalTextElements;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALDrawMultiLineTextOptions : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString FontName;
	float FontSize;
	System::Uitypes::TFontStyles FontStyle;
	System::Uitypes::TAlphaColor FontColor;
	System::UnicodeString EllipsisText;
	System::Uitypes::TFontStyles EllipsisFontStyle;
	System::Uitypes::TAlphaColor EllipsisFontColor;
	bool AutoSize;
	bool AutoSizeX;
	bool AutoSizeY;
	bool WordWrap;
	int MaxLines;
	float LineSpacing;
	Fmx::Types::TTextTrimming Trimming;
	System::Types::TPointF FirstLineIndent;
	bool FailIfTextBreaked;
	Fmx::Types::TTextAlign HTextAlign;
	Fmx::Types::TTextAlign VTextAlign;
	Fmx::Graphics::TBrush* Fill;
	Fmx::Graphics::TStrokeBrush* Stroke;
	Fmx::Types::TSides Sides;
	float XRadius;
	float YRadius;
	Fmx::Types::TCorners Corners;
	System::Types::TRectF Padding;
	bool TextIsHtml;
	__fastcall TALDrawMultiLineTextOptions();
	__fastcall virtual ~TALDrawMultiLineTextOptions();
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TALControlAccessPrivate : public Fmx::Types::TFmxObject
{
	typedef Fmx::Types::TFmxObject inherited;
	
	
private:
	enum DECLSPEC_DENUM TDelayedEvent : unsigned char { Resize, Resized };
	
	
private:
	static const System::Int8 InitialControlsCapacity = System::Int8(0xa);
	
	
public:
	static const System::Int8 DefaultTouchTargetExpansion = System::Int8(0x6);
	
	#define TALControlAccessPrivate_DefaultDisabledOpacity  (6.000000E-01)
	
	static const unsigned DesignBorderColor = unsigned(0xa0909090);
	
	
protected:
	static Fmx::Controls::TPaintStage FPaintStage;
	
public:
	Fmx::Types::TMouseEvent FOnMouseUp;
	Fmx::Types::TMouseEvent FOnMouseDown;
	Fmx::Types::TMouseMoveEvent FOnMouseMove;
	Fmx::Types::TMouseWheelEvent FOnMouseWheel;
	System::Classes::TNotifyEvent FOnClick;
	System::Classes::TNotifyEvent FOnDblClick;
	bool FHitTest;
	bool FClipChildren;
	bool FAutoCapture;
	Fmx::Types::TBounds* FPadding;
	Fmx::Types::TBounds* FMargins;
	Fmx::Graphics::TCanvas* FTempCanvas;
	float FRotationAngle;
	Fmx::Types::TPosition* FPosition;
	Fmx::Types::TPosition* FScale;
	Fmx::Types::TPosition* FSkew;
	Fmx::Types::TPosition* FRotationCenter;
	bool FCanFocus;
	Fmx::Types::TCanFocusEvent FOnCanFocus;
	System::Classes::TNotifyEvent FOnEnter;
	System::Classes::TNotifyEvent FOnExit;
	bool FClipParent;
	System::Classes::TNotifyEvent FOnMouseLeave;
	System::Classes::TNotifyEvent FOnMouseEnter;
	Fmx::Controls::TOnPaintEvent FOnPaint;
	Fmx::Controls::TOnPaintEvent FOnPainting;
	System::Uitypes::TCursor FCursor;
	System::Uitypes::TCursor FInheritedCursor;
	System::Uitypes::TDragMode FDragMode;
	bool FEnableDragHighlight;
	Fmx::Types::TDragEnterEvent FOnDragEnter;
	Fmx::Types::TDragDropEvent FOnDragDrop;
	System::Classes::TNotifyEvent FOnDragLeave;
	Fmx::Types::TDragOverEvent FOnDragOver;
	System::Classes::TNotifyEvent FOnDragEnd;
	bool FIsDragOver;
	Fmx::Types::TKeyEvent FOnKeyDown;
	Fmx::Types::TKeyEvent FOnKeyUp;
	Fmx::Types::TTapEvent FOnTap;
	System::UnicodeString FHint;
	System::UnicodeString FActionHint;
	bool FShowHint;
	Fmx::Types::TCustomPopupMenu* FPopupMenu;
	bool FRecalcEnabled;
	bool FEnabled;
	bool FAbsoluteEnabled;
	Fmx::Types::TTabList* FTabList;
	System::Classes::TNotifyEvent FOnResize;
	System::Classes::TNotifyEvent FOnResized;
	bool FDisableEffect;
	bool FAcceptsControls;
	System::Generics::Collections::TList__1<Fmx::Controls::TControl*>* FControls;
	bool FEnableExecuteAction;
	bool FCanParentFocus;
	float FMinClipHeight;
	float FMinClipWidth;
	bool FSmallSizeControl;
	Fmx::Types::TBounds* FTouchTargetExpansion;
	System::Classes::TNotifyEvent FOnDeactivate;
	System::Classes::TNotifyEvent FOnActivate;
	bool FSimpleTransform;
	System::Types::TSize FFixedSize;
	System::Generics::Collections::TList__1<Fmx::Effects::TEffect*>* FEffects;
	float FDisabledOpacity;
	Fmx::Controls::TControl* FParentControl;
	Fmx::Types::_di_IContent FParentContent;
	System::Types::TRectF FUpdateRect;
	bool FTabStop;
	int FDisableDisappear;
	bool FAnchorMove;
	bool FApplyingEffect;
	bool FExitingOrEntering;
	System::Set<TDelayedEvent, TDelayedEvent::Resize, TDelayedEvent::Resized> FDelayedEvents;
	bool FInflated;
	System::Classes::TNotifyEvent FOnApplyStyleLookup;
	Fmx::Types::TAlignLayout FAlign;
	System::Uitypes::TAnchors FAnchors;
	bool FUpdateEffects;
	bool FDisableFocusEffect;
	Fmx::Types::TTouchManager* FTouchManager;
	Fmx::Types::TGestureEvent FOnGesture;
	bool FVisible;
	bool FPressed;
	System::Types::TPointF FPressedPosition;
	bool FDoubleClick;
	bool FParentShowHint;
	Fmx::Controls::TCustomSceneAddRectEvent FCustomSceneAddRect;
	Fmx::Controls::_di_IScene FScene;
	float FLastHeight;
	float FLastWidth;
	Fmx::Types::TControlSize* FSize;
	System::Math::Vectors::TMatrix FLocalMatrix;
	System::Math::Vectors::TMatrix FAbsoluteMatrix;
	System::Math::Vectors::TMatrix FInvAbsoluteMatrix;
	Fmx::Graphics::TBitmap* FEffectBitmap;
	bool FLocked;
	float FOpacity;
	float FAbsoluteOpacity;
	bool FInPaintTo;
	System::Math::Vectors::TMatrix FInPaintToAbsMatrix;
	System::Math::Vectors::TMatrix FInPaintToInvMatrix;
	bool FAbsoluteHasEffect;
	bool FAbsoluteHasDisablePaintEffect;
	bool FAbsoluteHasAfterPaintEffect;
	int FUpdating;
	bool FNeedAlign;
	bool FDisablePaint;
	bool FDisableAlign;
	bool FRecalcOpacity;
	bool FRecalcUpdateRect;
	bool FRecalcAbsolute;
	bool FRecalcHasEffect;
	Fmx::Controls::TControl* FHasClipParent;
	bool FRecalcHasClipParent;
	bool FDesignInteractive;
	bool FDesignSelectionMarks;
	bool FIsMouseOver;
	bool FIsFocused;
	System::Types::TPointF FAnchorRules;
	System::Types::TPointF FAnchorOrigin;
	System::Types::TPointF FOriginalParentSize;
	float FLeft;
	float FTop;
	float FExplicitLeft;
	float FExplicitTop;
	float FExplicitWidth;
	float FExplicitHeight;
public:
	/* TFmxObject.Create */ inline __fastcall virtual TALControlAccessPrivate(System::Classes::TComponent* AOwner) : Fmx::Types::TFmxObject(AOwner) { }
	/* TFmxObject.Destroy */ inline __fastcall virtual ~TALControlAccessPrivate() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TALTextLayoutAccessPrivate : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	static System::Types::TPointF MaxLayoutSize;
	System::Generics::Collections::TList__1<Fmx::Textlayout::TTextAttributedRange*>* FAttributes;
	Fmx::Graphics::TFont* FFont;
	System::Uitypes::TAlphaColor FColor;
	System::UnicodeString FText;
	bool FWordWrap;
	Fmx::Types::TTextAlign FHorizontalAlign;
	Fmx::Types::TTextAlign FVerticalAlign;
	Fmx::Types::TBounds* FPadding;
	bool FNeedUpdate;
	System::Types::TPointF FMaxSize;
	System::Types::TPointF FTopLeft;
	int FUpdating;
	float FOpacity;
	Fmx::Types::TTextTrimming FTrimming;
	bool FRightToLeft;
	Fmx::Graphics::TCanvas* FCanvas;
	int FMessageId;
public:
	/* TObject.Create */ inline __fastcall TALTextLayoutAccessPrivate() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TALTextLayoutAccessPrivate() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TALCustomConvertFontFamilyProc ALCustomConvertFontFamilyProc;
extern DELPHI_PACKAGE System::Uitypes::TFontName __fastcall ALConvertFontFamily(const System::Uitypes::TFontName AFamily, const System::Uitypes::TFontStyles aFontStyles);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALTranslate(const System::UnicodeString AText);
extern DELPHI_PACKAGE void __fastcall ALFmxMakeBufBitmaps(Fmx::Controls::TControl* const aControl);
extern DELPHI_PACKAGE System::Types::TPointF __fastcall ALAlignAbsolutePointToPixelRound(const System::Types::TPointF &Point, const float Scale);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALAlignDimensionToPixelRound(const System::Types::TRectF &Rect, const float Scale)/* overload */;
extern DELPHI_PACKAGE float __fastcall ALAlignDimensionToPixelRound(const float Dimension, const float Scale)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALAlignDimensionToPixelRound(const System::Types::TRectF &Rect)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALAlignDimensionToPixelCeil(const System::Types::TRectF &Rect, const float Scale)/* overload */;
extern DELPHI_PACKAGE float __fastcall ALAlignDimensionToPixelCeil(const float Dimension, const float Scale)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALAlignDimensionToPixelCeil(const System::Types::TRectF &Rect)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALAlignToPixelRound(const System::Types::TRectF &Rect);
extern DELPHI_PACKAGE void __fastcall ALGetTextMetrics(const float aFontSize, const System::Uitypes::TFontStyles aFontStyle, const System::UnicodeString aFontName, float &aAscent, float &aDescent);
extern DELPHI_PACKAGE int __fastcall ALbreakText(const float aFontSize, const System::Uitypes::TFontStyles aFontStyle, const System::UnicodeString aFontName, const System::UnicodeString atext, const float aMaxWidth, float &aMeasuredWidth)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALbreakText(const System::Uitypes::TAlphaColor aFontColor, const float aFontSize, const System::Uitypes::TFontStyles aFontStyle, const System::UnicodeString aFontName, System::Types::TRectF &ARect, const System::UnicodeString AText, const bool aWordWrap, const Fmx::Types::TTextAlign AHTextAlign, const Fmx::Types::TTextAlign AVTextAlign, const Fmx::Types::TTextTrimming aTrimming, TALBreakTextItems* const aBreakTextItems, int &aTotalLines, bool &aAllTextDrawed, const System::Types::TPointF &aFirstLineIndent, const float aLineSpacing = 0.000000E+00f, const System::UnicodeString aEllipsisText = L"\u2026", const System::Uitypes::TFontStyles aEllipsisFontStyle = System::Uitypes::TFontStyles() , const System::Uitypes::TAlphaColor aEllipsisFontColor = (System::Uitypes::TAlphaColor)(0x0),
	const int aMaxlines = 0x0)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALDrawMultiLineText(const System::UnicodeString aText, System::Types::TRectF &aRect, bool &aTextBreaked, bool &aAllTextDrawed, float &aAscent, float &aDescent, System::Types::TPointF &aFirstPos, System::Types::TPointF &aLastPos, TalTextElements &aElements, System::Types::TRectF &aEllipsisRect, TALDrawMultiLineTextOptions* const aOptions)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALDrawMultiLineText(const System::UnicodeString aText, System::Types::TRectF &aRect, bool &aTextBreaked, bool &aAllTextDrawed, TALDrawMultiLineTextOptions* const aOptions)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALDrawMultiLineText(const System::UnicodeString aText, System::Types::TRectF &aRect, bool &aTextBreaked, TALDrawMultiLineTextOptions* const aOptions)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALDrawMultiLineText(const System::UnicodeString aText, System::Types::TRectF &aRect, TALDrawMultiLineTextOptions* const aOptions)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALPaintRectangle(Fmx::Graphics::TCanvas* const aCanvas, const System::Types::TRectF &dstRect, Fmx::Graphics::TBrush* const Fill, Fmx::Graphics::TStrokeBrush* const Stroke, TALShadow* const Shadow = (TALShadow*)(0x0), const Fmx::Types::TSides Sides = (Fmx::Types::TSides() << Fmx::Types::TSide::Top << Fmx::Types::TSide::Left << Fmx::Types::TSide::Bottom << Fmx::Types::TSide::Right ), const Fmx::Types::TCorners Corners = (Fmx::Types::TCorners() << Fmx::Types::TCorner::TopLeft << Fmx::Types::TCorner::TopRight << Fmx::Types::TCorner::BottomLeft << Fmx::Types::TCorner::BottomRight ), const float XRadius = 0.000000E+00f, const float YRadius = 0.000000E+00f);
extern DELPHI_PACKAGE void __fastcall ALPaintCircle(Fmx::Graphics::TCanvas* const aCanvas, const System::Types::TRectF &dstRect, Fmx::Graphics::TBrush* const Fill, Fmx::Graphics::TStrokeBrush* const Stroke, TALShadow* const Shadow = (TALShadow*)(0x0));
extern DELPHI_PACKAGE void __fastcall ALCreateDrawingSurface(Fmx::Graphics::TBitmap* &aBitmap, const bool aClearBitmap, const int w, const int h);
extern DELPHI_PACKAGE void __fastcall ALFreeDrawingSurface(Fmx::Graphics::TBitmap* &aBitmap);
}	/* namespace Alfmxcommon */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXCOMMON)
using namespace Alfmxcommon;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxcommonHPP
