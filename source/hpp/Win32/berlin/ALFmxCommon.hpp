// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxCommon.pas' rev: 31.00 (Windows)

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
struct TALSizeD;
struct TALRectD;
__interface TALResizeImageGetDestSizeFunct;
typedef System::DelphiInterface<TALResizeImageGetDestSizeFunct> _di_TALResizeImageGetDestSizeFunct;
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
	bool __fastcall IsblurStored(void);
	bool __fastcall IsOffsetXStored(void);
	bool __fastcall IsOffsetYStored(void);
	
public:
	__fastcall TALShadow(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
	
__published:
	__property bool enabled = {read=fenabled, write=SetEnabled, default=0};
	__property float blur = {read=fblur, write=setblur, stored=IsblurStored};
	__property float OffsetX = {read=fOffsetX, write=setOffsetX, stored=IsOffsetXStored};
	__property float OffsetY = {read=fOffsetY, write=setOffsetY, stored=IsOffsetYStored};
	__property System::Uitypes::TAlphaColor ShadowColor = {read=fShadowColor, write=setShadowColor, default=-1778384896};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TALShadow(void) { }
	
};


typedef System::StaticArray<double, 2> TALPointDType;

typedef TALPointD *PALPointD;

typedef TALSizeD *PALSizeD;

struct DECLSPEC_DRECORD TALSizeD
{
public:
	double cx;
	double cy;
	__fastcall TALSizeD(const TALSizeD &P)/* overload */;
	__fastcall TALSizeD(const double X, const double Y)/* overload */;
	static bool __fastcall _op_Equality(const TALSizeD &Lhs, const TALSizeD &Rhs);
	static bool __fastcall _op_Inequality(const TALSizeD &Lhs, const TALSizeD &Rhs);
	static TALSizeD __fastcall _op_Addition(const TALSizeD &Lhs, const TALSizeD &Rhs);
	static TALSizeD __fastcall _op_Subtraction(const TALSizeD &Lhs, const TALSizeD &Rhs);
	__fastcall operator TALPointD();
	static TALSizeD __fastcall _op_Implicit(const TALPointD &Point);
	TALSizeD& __fastcall operator=(const TALPointD &Point) { *this = TALSizeD::_op_Implicit(Point); return *this; };
	static TALSizeD __fastcall _op_Implicit(const System::Types::TSize &Size);
	TALSizeD& __fastcall operator=(const System::Types::TSize &Size) { *this = TALSizeD::_op_Implicit(Size); return *this; };
	System::Types::TSize __fastcall Ceiling(void);
	System::Types::TSize __fastcall Truncate(void);
	System::Types::TSize __fastcall Round(void);
	TALSizeD __fastcall Add(const TALSizeD &Point);
	TALSizeD __fastcall Subtract(const TALSizeD &Point);
	double __fastcall Distance(const TALSizeD &P2);
	bool __fastcall IsZero(void);
	TALSizeD __fastcall SwapDimensions(void);
	__property double Width = {read=cx, write=cx};
	__property double Height = {read=cy, write=cy};
	TALSizeD() {}
};


typedef TALRectD *PALRectD;

struct DECLSPEC_DRECORD TALRectD
{
private:
	double __fastcall GetWidth(void);
	void __fastcall SetWidth(const double Value);
	double __fastcall GetHeight(void);
	void __fastcall SetHeight(const double Value);
	TALSizeD __fastcall GetSize(void);
	void __fastcall SetSize(const TALSizeD &Value);
	TALPointD __fastcall GetLocation(void);
	
public:
	__fastcall TALRectD(const TALPointD &Origin)/* overload */;
	__fastcall TALRectD(const TALPointD &Origin, const double Width, const double Height)/* overload */;
	__fastcall TALRectD(const double Left, const double Top, const double Right, const double Bottom)/* overload */;
	__fastcall TALRectD(const TALPointD &P1, const TALPointD &P2, bool Normalize)/* overload */;
	__fastcall TALRectD(const TALRectD &R, bool Normalize)/* overload */;
	__fastcall TALRectD(const System::Types::TRect &R, bool Normalize)/* overload */;
	static bool __fastcall _op_Equality(const TALRectD &Lhs, const TALRectD &Rhs);
	static bool __fastcall _op_Inequality(const TALRectD &Lhs, const TALRectD &Rhs);
	static TALRectD __fastcall _op_Implicit(const System::Types::TRect &Source);
	TALRectD& __fastcall operator=(const System::Types::TRect &Source) { *this = TALRectD::_op_Implicit(Source); return *this; };
	static TALRectD __fastcall _op_Addition(const TALRectD &Lhs, const TALRectD &Rhs);
	static TALRectD __fastcall _op_Multiply(const TALRectD &Lhs, const TALRectD &Rhs);
	static TALRectD __fastcall Empty();
	double __fastcall Fit(const TALRectD &BoundsRect);
	TALRectD __fastcall FitInto(const TALRectD &ADesignatedArea, /* out */ double &Ratio)/* overload */;
	TALRectD __fastcall FitInto(const TALRectD &ADesignatedArea)/* overload */;
	TALRectD __fastcall CenterAt(const TALRectD &ADesignatedArea);
	TALRectD __fastcall PlaceInto(const TALRectD &ADesignatedArea, const System::Types::THorzRectAlign AHorzAlign = (System::Types::THorzRectAlign)(0x0), const System::Types::TVertRectAlign AVertAlign = (System::Types::TVertRectAlign)(0x0));
	TALRectD __fastcall SnapToPixel(const double AScale, const bool APlaceBetweenPixels = true);
	void __fastcall NormalizeRect(void);
	bool __fastcall IsEmpty(void);
	bool __fastcall Contains(const TALPointD &Pt)/* overload */;
	bool __fastcall Contains(const System::Types::TPointF &Pt)/* overload */;
	bool __fastcall Contains(const TALRectD &R)/* overload */;
	bool __fastcall IntersectsWith(const TALRectD &R);
	static TALRectD __fastcall Intersect(const TALRectD &R1, const TALRectD &R2)/* overload */;
	void __fastcall Intersect(const TALRectD &R)/* overload */;
	static TALRectD __fastcall Union(const TALRectD &R1, const TALRectD &R2)/* overload */;
	void __fastcall Union(const TALRectD &R)/* overload */;
	static TALRectD __fastcall Union(const TALPointD *Points, const int Points_High)/* overload */;
	void __fastcall Offset(const double DX, const double DY)/* overload */;
	void __fastcall Offset(const TALPointD &Point)/* overload */;
	void __fastcall SetLocation(const double X, const double Y)/* overload */;
	void __fastcall SetLocation(const TALPointD &Point)/* overload */;
	void __fastcall Inflate(const double DX, const double DY)/* overload */;
	void __fastcall Inflate(const double DL, const double DT, const double DR, const double DB)/* overload */;
	TALPointD __fastcall CenterPoint(void);
	System::Types::TRect __fastcall Ceiling(void);
	System::Types::TRect __fastcall Truncate(void);
	System::Types::TRect __fastcall Round(void);
	bool __fastcall EqualsTo(const TALRectD &R, const double Epsilon = 0.000000E+00);
	__property double Width = {read=GetWidth, write=SetWidth};
	__property double Height = {read=GetHeight, write=SetHeight};
	__property TALSizeD Size = {read=GetSize, write=SetSize};
	__property TALPointD Location = {read=GetLocation, write=SetLocation};
	TALRectD() {}
	
public:
	union
	{
		struct 
		{
			TALPointD TopLeft;
			TALPointD BottomRight;
		};
		struct 
		{
			double Left;
			double Top;
			double Right;
			double Bottom;
		};
		
	};
};


typedef System::Uitypes::TFontName __fastcall (*TALCustomConvertFontFamilyProc)(const System::Uitypes::TFontName AFamily, const System::Uitypes::TFontStyles aFontStyles);

__interface TALResizeImageGetDestSizeFunct  : public System::IInterface 
{
	virtual System::Types::TPointF __fastcall Invoke(const System::Types::TPointF &aOriginalSize) = 0 ;
};

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
	bool isEllipsis;
	__fastcall TALBreakTextItem(void);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALBreakTextItem(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TALBreakTextItems : public System::Generics::Collections::TObjectList__1<TALBreakTextItem*>
{
	typedef System::Generics::Collections::TObjectList__1<TALBreakTextItem*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<ALFmxCommon_TALBreakTextItem>.Create */ inline __fastcall TALBreakTextItems(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TALBreakTextItem*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<ALFmxCommon_TALBreakTextItem>.Create */ inline __fastcall TALBreakTextItems(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TALBreakTextItem*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TALBreakTextItem*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<ALFmxCommon_TALBreakTextItem>.Create */ inline __fastcall TALBreakTextItems(System::Generics::Collections::TEnumerable__1<TALBreakTextItem*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TALBreakTextItem*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<ALFmxCommon_TALBreakTextItem>.Destroy */ inline __fastcall virtual ~TALBreakTextItems(void) { }
	
public:
	/* {System_Generics_Collections}TList<ALFmxCommon_TALBreakTextItem>.Create */ inline __fastcall TALBreakTextItems(void)/* overload */ : System::Generics::Collections::TObjectList__1<TALBreakTextItem*>() { }
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
	__fastcall TALDrawMultiLineTextOptions(void);
	__fastcall virtual ~TALDrawMultiLineTextOptions(void);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TALControlAccessPrivate : public Fmx::Types::TFmxObject
{
	typedef Fmx::Types::TFmxObject inherited;
	
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
	/* TFmxObject.Destroy */ inline __fastcall virtual ~TALControlAccessPrivate(void) { }
	
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
	/* TObject.Create */ inline __fastcall TALTextLayoutAccessPrivate(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TALTextLayoutAccessPrivate(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TALCustomConvertFontFamilyProc ALCustomConvertFontFamilyProc;
extern DELPHI_PACKAGE int __fastcall ALRectWidth(const System::Types::TRect &Rect)/* overload */;
extern DELPHI_PACKAGE float __fastcall ALRectWidth(const System::Types::TRectF &Rect)/* overload */;
extern DELPHI_PACKAGE double __fastcall ALRectWidth(const TALRectD &Rect)/* overload */;
extern DELPHI_PACKAGE int __fastcall ALRectHeight(const System::Types::TRect &Rect)/* overload */;
extern DELPHI_PACKAGE float __fastcall ALRectHeight(const System::Types::TRectF &Rect)/* overload */;
extern DELPHI_PACKAGE double __fastcall ALRectHeight(const TALRectD &Rect)/* overload */;
extern DELPHI_PACKAGE System::Types::TRect __fastcall ALRectCenter(System::Types::TRect &R, const System::Types::TRect &Bounds)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALRectCenter(System::Types::TRectF &R, const System::Types::TRectF &Bounds)/* overload */;
extern DELPHI_PACKAGE TALRectD __fastcall ALRectCenter(TALRectD &R, const TALRectD &Bounds)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALOffsetRect(System::Types::TRect &R, int DX, int DY)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALOffsetRect(System::Types::TRectF &R, float DX, float DY)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALOffsetRect(TALRectD &R, double DX, double DY)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALIntersectRectD(/* out */ TALRectD &Rect, const TALRectD &R1, const TALRectD &R2);
extern DELPHI_PACKAGE bool __fastcall ALUnionRectD(/* out */ TALRectD &Rect, const TALRectD &R1, const TALRectD &R2);
extern DELPHI_PACKAGE System::Uitypes::TFontName __fastcall ALConvertFontFamily(const System::Uitypes::TFontName AFamily, const System::Uitypes::TFontStyles aFontStyles);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALTranslate(const System::UnicodeString AText);
extern DELPHI_PACKAGE void __fastcall ALFmxMakeBufBitmaps(Fmx::Controls::TControl* const aControl);
extern DELPHI_PACKAGE System::Uitypes::TAlphaColor __fastcall ALPrepareColor(const System::Uitypes::TAlphaColor SrcColor, const float Opacity);
extern DELPHI_PACKAGE System::Types::TPointF __fastcall ALAlignAbsolutePointToPixelRound(const System::Types::TPointF &Point, const float Scale);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALAlignDimensionToPixelRound(const System::Types::TRectF &Rect, const float Scale)/* overload */;
extern DELPHI_PACKAGE float __fastcall ALAlignDimensionToPixelRound(const float Dimension, const float Scale)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALAlignDimensionToPixelRound(const System::Types::TRectF &Rect)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALAlignDimensionToPixelCeil(const System::Types::TRectF &Rect, const float Scale)/* overload */;
extern DELPHI_PACKAGE float __fastcall ALAlignDimensionToPixelCeil(const float Dimension, const float Scale)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALAlignDimensionToPixelCeil(const System::Types::TRectF &Rect)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALGetTextMetrics(const float aFontSize, const System::Uitypes::TFontStyles aFontStyle, const System::UnicodeString aFontName, float &aAscent, float &aDescent);
extern DELPHI_PACKAGE int __fastcall ALbreakText(const float aFontSize, const System::Uitypes::TFontStyles aFontStyle, const System::UnicodeString aFontName, const System::UnicodeString atext, const float aMaxWidth, float &aMeasuredWidth)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALbreakText(const System::Uitypes::TAlphaColor aFontColor, const float aFontSize, const System::Uitypes::TFontStyles aFontStyle, const System::UnicodeString aFontName, System::Types::TRectF &ARect, const System::UnicodeString AText, const bool aWordWrap, const Fmx::Types::TTextAlign AHTextAlign, const Fmx::Types::TTextAlign AVTextAlign, const Fmx::Types::TTextTrimming aTrimming, TALBreakTextItems* const aBreakTextItems, int &aTotalLines, const System::Types::TPointF &aFirstLineIndent, const float aLineSpacing = 0.000000E+00f, const System::UnicodeString aEllipsisText = L"\u2026", const System::Uitypes::TFontStyles aEllipsisFontStyle = System::Uitypes::TFontStyles() , const System::Uitypes::TAlphaColor aEllipsisFontColor = (System::Uitypes::TAlphaColor)(0x0), const int aMaxlines = 0x0)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALDrawMultiLineText(const System::UnicodeString aText, System::Types::TRectF &aRect, bool &aTextBreaked, float &aAscent, float &aDescent, System::Types::TPointF &aFirstPos, System::Types::TPointF &aLastPos, TalTextElements &aElements, System::Types::TRectF &aEllipsisRect, TALDrawMultiLineTextOptions* const aOptions)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALDrawMultiLineText(const System::UnicodeString aText, System::Types::TRectF &aRect, bool &aTextBreaked, TALDrawMultiLineTextOptions* const aOptions)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALDrawMultiLineText(const System::UnicodeString aText, System::Types::TRectF &aRect, TALDrawMultiLineTextOptions* const aOptions)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALPaintRectangle(Fmx::Graphics::TCanvas* const aCanvas, const System::Types::TRectF &dstRect, Fmx::Graphics::TBrush* const Fill, Fmx::Graphics::TStrokeBrush* const Stroke, TALShadow* const Shadow = (TALShadow*)(0x0), const Fmx::Types::TSides Sides = (Fmx::Types::TSides() << Fmx::Types::TSide::Top << Fmx::Types::TSide::Left << Fmx::Types::TSide::Bottom << Fmx::Types::TSide::Right ), const Fmx::Types::TCorners Corners = (Fmx::Types::TCorners() << Fmx::Types::TCorner::TopLeft << Fmx::Types::TCorner::TopRight << Fmx::Types::TCorner::BottomLeft << Fmx::Types::TCorner::BottomRight ), const float XRadius = 0.000000E+00f, const float YRadius = 0.000000E+00f);
extern DELPHI_PACKAGE void __fastcall ALPaintCircle(Fmx::Graphics::TCanvas* const aCanvas, const System::Types::TRectF &dstRect, Fmx::Graphics::TBrush* const Fill, Fmx::Graphics::TStrokeBrush* const Stroke, TALShadow* const Shadow = (TALShadow*)(0x0));
extern DELPHI_PACKAGE void __fastcall ALCreateDrawingSurface(Fmx::Graphics::TBitmap* &aBitmap, const bool aClearBitmap, const int w, const int h);
extern DELPHI_PACKAGE void __fastcall ALFreeDrawingSurface(Fmx::Graphics::TBitmap* &aBitmap);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALRectFitInto(const System::Types::TRectF &R, const System::Types::TRectF &Bounds, const System::Types::TPointF &CenterAt, /* out */ float &Ratio)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALRectFitInto(const System::Types::TRectF &R, const System::Types::TRectF &Bounds, const System::Types::TPointF &CenterAt)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropAsCircleImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropAsCircleImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropAsCircleImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropAsCircleImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropAsCircleImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropAsCircleImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropImageV1(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropImageV2(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropImageV3(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndCropImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndCropResourceImageV1(const System::UnicodeString aResName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndCropResourceImageV1(const System::UnicodeString aResName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndCropResourceImageV2(const System::UnicodeString aResName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndCropResourceImageV2(const System::UnicodeString aResName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndCropResourceImageV3(const System::UnicodeString aResName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndCropResourceImageV3(const System::UnicodeString aResName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndCropFileImageV1(const System::UnicodeString aFileName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndCropFileImageV1(const System::UnicodeString aFileName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndCropFileImageV2(const System::UnicodeString aFileName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndCropFileImageV2(const System::UnicodeString aFileName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndCropFileImageV3(const System::UnicodeString aFileName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndCropFileImageV3(const System::UnicodeString aFileName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndFitImageV1(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndFitImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndFitImageV2(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndFitImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndFitImageV3(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALResizeAndFitImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndFitResourceImageV1(const System::UnicodeString aResName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndFitResourceImageV2(const System::UnicodeString aResName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndFitResourceImageV3(const System::UnicodeString aResName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndFitFileImageV1(const System::UnicodeString aFileName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndFitFileImageV2(const System::UnicodeString aFileName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadResizeAndFitFileImageV3(const System::UnicodeString aFileName, const float W, const float H);
}	/* namespace Alfmxcommon */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXCOMMON)
using namespace Alfmxcommon;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxcommonHPP
