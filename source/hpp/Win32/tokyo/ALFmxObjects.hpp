// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxObjects.pas' rev: 32.00 (Windows)

#ifndef AlfmxobjectsHPP
#define AlfmxobjectsHPP

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
#include <System.Rtti.hpp>
#include <FMX.Effects.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>
#include <FMX.TextLayout.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.Objects.hpp>
#include <ALGraphics.hpp>
#include <ALFmxCommon.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxobjects
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALImage;
class DELPHICLASS TALRectangle;
class DELPHICLASS TALCircle;
class DELPHICLASS TALLine;
class DELPHICLASS TALDoubleBufferedTextLayout;
class DELPHICLASS TALText;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TALImageWrapMode : unsigned char { Original, Fit, Stretch, Tile, Center, Place, FitAndCrop };

class PASCALIMPLEMENTATION TALImage : public Fmx::Controls::TControl
{
	typedef Fmx::Controls::TControl inherited;
	
private:
	Algraphics::TalExifOrientationInfo fExifOrientationInfo;
	bool fRotateAccordingToExifOrientation;
	System::UnicodeString fFileName;
	System::UnicodeString fResourceName;
	TALImageWrapMode FWrapMode;
	float FScreenScale;
	Fmx::Graphics::TBitmap* fBufBitmap;
	System::Types::TRectF fBufBitmapRect;
	System::Types::TSizeF fBufSize;
	void __fastcall SetWrapMode(const TALImageWrapMode Value);
	void __fastcall setFileName(const System::UnicodeString Value);
	void __fastcall setResourceName(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall Paint(void);
	__property Fmx::Graphics::TBitmap* BufBitmap = {read=fBufBitmap};
	
public:
	__fastcall virtual TALImage(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALImage(void);
	virtual Fmx::Graphics::TBitmap* __fastcall MakeBufBitmap(void);
	virtual void __fastcall clearBufBitmap(void);
	
__published:
	__property Align = {default=0};
	__property Anchors;
	__property ClipChildren = {default=0};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property DragMode = {default=0};
	__property EnableDragHighlight = {default=1};
	__property Enabled = {default=1};
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
	__property bool RotateAccordingToExifOrientation = {read=fRotateAccordingToExifOrientation, write=fRotateAccordingToExifOrientation, default=0};
	__property Scale;
	__property Size;
	__property TouchTargetExpansion;
	__property Visible = {default=1};
	__property Width;
	__property System::UnicodeString FileName = {read=fFileName, write=setFileName};
	__property System::UnicodeString ResourceName = {read=fResourceName, write=setResourceName};
	__property TALImageWrapMode WrapMode = {read=FWrapMode, write=SetWrapMode, default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
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
};


class PASCALIMPLEMENTATION TALRectangle : public Fmx::Objects::TRectangle
{
	typedef Fmx::Objects::TRectangle inherited;
	
private:
	float FScreenScale;
	bool fdoubleBuffered;
	Fmx::Graphics::TBitmap* fBufBitmap;
	System::Types::TRectF fBufBitmapRect;
	System::Types::TSizeF fBufSize;
	Alfmxcommon::TALShadow* fShadow;
	Fmx::Effects::TShadowEffect* fShadowEffect;
	void __fastcall SetdoubleBuffered(const bool Value);
	void __fastcall SetShadow(Alfmxcommon::TALShadow* const Value);
	
protected:
	virtual void __fastcall FillChanged(System::TObject* Sender);
	virtual void __fastcall StrokeChanged(System::TObject* Sender);
	virtual void __fastcall ShadowChanged(System::TObject* Sender);
	virtual void __fastcall Paint(void);
	__property Fmx::Graphics::TBitmap* BufBitmap = {read=fBufBitmap};
	
public:
	__fastcall virtual TALRectangle(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALRectangle(void);
	virtual Fmx::Graphics::TBitmap* __fastcall MakeBufBitmap(void);
	virtual void __fastcall clearBufBitmap(void);
	
__published:
	__property bool doubleBuffered = {read=fdoubleBuffered, write=SetdoubleBuffered, default=1};
	__property Alfmxcommon::TALShadow* shadow = {read=fShadow, write=SetShadow};
};


class PASCALIMPLEMENTATION TALCircle : public Fmx::Objects::TCircle
{
	typedef Fmx::Objects::TCircle inherited;
	
private:
	float FScreenScale;
	bool fdoubleBuffered;
	Fmx::Graphics::TBitmap* fBufBitmap;
	System::Types::TRectF fBufBitmapRect;
	System::Types::TSizeF fBufSize;
	Alfmxcommon::TALShadow* fShadow;
	Fmx::Effects::TShadowEffect* fShadowEffect;
	void __fastcall SetdoubleBuffered(const bool Value);
	void __fastcall SetShadow(Alfmxcommon::TALShadow* const Value);
	
protected:
	virtual void __fastcall FillChanged(System::TObject* Sender);
	virtual void __fastcall StrokeChanged(System::TObject* Sender);
	virtual void __fastcall ShadowChanged(System::TObject* Sender);
	virtual void __fastcall Paint(void);
	__property Fmx::Graphics::TBitmap* BufBitmap = {read=fBufBitmap};
	
public:
	__fastcall virtual TALCircle(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALCircle(void);
	virtual Fmx::Graphics::TBitmap* __fastcall MakeBufBitmap(void);
	virtual void __fastcall clearBufBitmap(void);
	
__published:
	__property bool doubleBuffered = {read=fdoubleBuffered, write=SetdoubleBuffered, default=1};
	__property Alfmxcommon::TALShadow* shadow = {read=fShadow, write=SetShadow};
};


class PASCALIMPLEMENTATION TALLine : public Fmx::Objects::TLine
{
	typedef Fmx::Objects::TLine inherited;
	
private:
	float FScreenScale;
	bool fdoubleBuffered;
	Fmx::Graphics::TBitmap* fBufBitmap;
	System::Types::TRectF fBufBitmapRect;
	System::Types::TSizeF fBufSize;
	void __fastcall SetdoubleBuffered(const bool Value);
	
protected:
	virtual void __fastcall FillChanged(System::TObject* Sender);
	virtual void __fastcall StrokeChanged(System::TObject* Sender);
	__property Fmx::Graphics::TBitmap* BufBitmap = {read=fBufBitmap};
	
public:
	__fastcall virtual TALLine(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALLine(void);
	virtual void __fastcall Paint(void);
	virtual Fmx::Graphics::TBitmap* __fastcall MakeBufBitmap(void);
	virtual void __fastcall clearBufBitmap(void);
	
__published:
	__property bool doubleBuffered = {read=fdoubleBuffered, write=SetdoubleBuffered, default=1};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TALDoubleBufferedTextLayout : public Fmx::Textlayout::TTextLayout
{
	typedef Fmx::Textlayout::TTextLayout inherited;
	
private:
	float FScreenScale;
	TALText* fTextControl;
	Fmx::Graphics::TBitmap* fBufBitmap;
	System::Types::TRectF fBufBitmapRect;
	Fmx::Types::TTextAlign fBufHorizontalAlign;
	Fmx::Types::TTextAlign fBufVerticalAlign;
	System::Uitypes::TAlphaColor fBuffontColor;
	System::Uitypes::TFontName fBuffontFamily;
	System::Uitypes::TFontStyles fBuffontStyle;
	float fBuffontSize;
	bool fBufWordWrap;
	bool fBufAutosize;
	Fmx::Types::TTextTrimming fBufTrimming;
	System::Types::TSizeF fBufSize;
	System::UnicodeString fBufText;
	bool fBufTextBreaked;
	bool fBufAllTextDrawed;
	
protected:
	virtual void __fastcall DoRenderLayout(void);
	virtual void __fastcall DoDrawLayout(Fmx::Graphics::TCanvas* const ACanvas);
	virtual float __fastcall GetTextHeight(void);
	virtual float __fastcall GetTextWidth(void);
	virtual System::Types::TRectF __fastcall GetTextRect(void);
	virtual int __fastcall DoPositionAtPoint(const System::Types::TPointF &APoint);
	virtual Fmx::Graphics::TRegion __fastcall DoRegionForRange(const Fmx::Textlayout::TTextRange &ARange);
	
public:
	__fastcall TALDoubleBufferedTextLayout(Fmx::Graphics::TCanvas* const ACanvas, TALText* const aTextControl);
	__fastcall virtual ~TALDoubleBufferedTextLayout(void);
	virtual Fmx::Graphics::TBitmap* __fastcall MakeBufBitmap(void);
	virtual void __fastcall clearBufBitmap(void);
	virtual void __fastcall ConvertToPath(Fmx::Graphics::TPathData* const APath);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TALText : public Fmx::Controls::TControl
{
	typedef Fmx::Controls::TControl inherited;
	
private:
	bool fRestoreLayoutUpdateAfterLoaded;
	bool FAutoTranslate;
	bool FAutoConvertFontFamily;
	Fmx::Graphics::TTextSettings* FTextSettings;
	Fmx::Textlayout::TTextLayout* FLayout;
	bool FAutoSize;
	float fMaxWidth;
	float fMaxHeight;
	float FYRadius;
	float FXRadius;
	Fmx::Types::TCorners FCorners;
	Fmx::Types::TSides FSides;
	Fmx::Graphics::TBrush* FFill;
	Fmx::Graphics::TStrokeBrush* FStroke;
	float fLineSpacing;
	bool fTextIsHtml;
	bool fMustCallResized;
	void __fastcall SetFill(Fmx::Graphics::TBrush* const Value);
	void __fastcall SetStroke(Fmx::Graphics::TStrokeBrush* const Value);
	bool __fastcall IsCornersStored(void);
	bool __fastcall IsSidesStored(void);
	Fmx::Graphics::TBitmap* __fastcall GetBufBitmap(void);
	bool __fastcall GetdoubleBuffered(void);
	void __fastcall SetdoubleBuffered(const bool Value);
	void __fastcall SetText(const System::UnicodeString Value);
	void __fastcall SetFont(Fmx::Graphics::TFont* const Value);
	void __fastcall SetHorzTextAlign(const Fmx::Types::TTextAlign Value);
	void __fastcall SetVertTextAlign(const Fmx::Types::TTextAlign Value);
	void __fastcall SetWordWrap(const bool Value);
	void __fastcall SetAutoSize(const bool Value);
	void __fastcall SetColor(const System::Uitypes::TAlphaColor Value);
	void __fastcall SetTrimming(const Fmx::Types::TTextTrimming Value);
	void __fastcall OnFontChanged(System::TObject* Sender);
	Fmx::Graphics::TTextSettings* __fastcall GetTextSettings(void);
	void __fastcall SetTextSettings(Fmx::Graphics::TTextSettings* const Value);
	System::Uitypes::TAlphaColor __fastcall GetColor(void);
	Fmx::Graphics::TFont* __fastcall GetFont(void);
	Fmx::Types::TTextAlign __fastcall GetHorzTextAlign(void);
	Fmx::Types::TTextTrimming __fastcall GetTrimming(void);
	Fmx::Types::TTextAlign __fastcall GetVertTextAlign(void);
	bool __fastcall GetWordWrap(void);
	System::UnicodeString __fastcall GetText(void);
	bool __fastcall IsMaxWidthStored(void);
	bool __fastcall IsMaxHeightStored(void);
	
protected:
	__property Fmx::Graphics::TBitmap* BufBitmap = {read=GetBufBitmap};
	virtual void __fastcall FillChanged(System::TObject* Sender);
	virtual void __fastcall StrokeChanged(System::TObject* Sender);
	virtual void __fastcall SetXRadius(const float Value);
	virtual void __fastcall SetYRadius(const float Value);
	virtual void __fastcall SetCorners(const Fmx::Types::TCorners Value);
	virtual void __fastcall SetSides(const Fmx::Types::TSides Value);
	virtual void __fastcall SetParent(Fmx::Types::TFmxObject* const Value);
	virtual void __fastcall FontChanged(void);
	virtual bool __fastcall SupportsPaintStage(const Fmx::Controls::TPaintStage Stage);
	virtual Fmx::Graphics::TTextSettingsClass __fastcall GetTextSettingsClass(void);
	virtual void __fastcall Paint(void);
	virtual System::Rtti::TValue __fastcall GetData(void);
	virtual void __fastcall SetData(const System::Rtti::TValue &Value);
	virtual void __fastcall DoRealign(void);
	void __fastcall AdjustSize(void);
	virtual void __fastcall Resize(void);
	virtual void __fastcall DoResized(void);
	virtual void __fastcall Loaded(void);
	__property Fmx::Textlayout::TTextLayout* Layout = {read=FLayout};
	
public:
	__fastcall virtual TALText(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALText(void);
	virtual void __fastcall SetNewScene(Fmx::Controls::_di_IScene AScene);
	virtual Fmx::Graphics::TBitmap* __fastcall MakeBufBitmap(void);
	virtual void __fastcall clearBufBitmap(void);
	virtual void __fastcall BeginUpdate(void);
	virtual void __fastcall EndUpdate(void);
	bool __fastcall TextBreaked(void);
	__property Fmx::Graphics::TFont* Font = {read=GetFont, write=SetFont};
	__property System::Uitypes::TAlphaColor Color = {read=GetColor, write=SetColor, nodefault};
	__property Fmx::Types::TTextAlign HorzTextAlign = {read=GetHorzTextAlign, write=SetHorzTextAlign, nodefault};
	__property Fmx::Types::TTextTrimming Trimming = {read=GetTrimming, write=SetTrimming, nodefault};
	__property Fmx::Types::TTextAlign VertTextAlign = {read=GetVertTextAlign, write=SetVertTextAlign, nodefault};
	__property bool WordWrap = {read=GetWordWrap, write=SetWordWrap, nodefault};
	
__published:
	__property Align = {default=0};
	__property Anchors;
	__property bool AutoSize = {read=FAutoSize, write=SetAutoSize, default=0};
	__property ClipChildren = {default=0};
	__property ClipParent = {default=0};
	__property Cursor = {default=0};
	__property DragMode = {default=0};
	__property EnableDragHighlight = {default=1};
	__property Enabled = {default=1};
	__property Locked = {default=0};
	__property Height;
	__property HitTest = {default=0};
	__property Padding;
	__property Opacity;
	__property Margins;
	__property PopupMenu;
	__property Position;
	__property RotationAngle = {default=0};
	__property RotationCenter;
	__property Scale;
	__property Size;
	__property System::UnicodeString Text = {read=GetText, write=SetText};
	__property Fmx::Graphics::TTextSettings* TextSettings = {read=GetTextSettings, write=SetTextSettings};
	__property Visible = {default=1};
	__property Width;
	__property float MaxWidth = {read=fMaxWidth, write=fMaxWidth, stored=IsMaxWidthStored};
	__property float MaxHeight = {read=fMaxHeight, write=fMaxHeight, stored=IsMaxHeightStored};
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
	__property bool doubleBuffered = {read=GetdoubleBuffered, write=SetdoubleBuffered, default=1};
	__property bool AutoTranslate = {read=FAutoTranslate, write=FAutoTranslate, default=1};
	__property bool AutoConvertFontFamily = {read=FAutoConvertFontFamily, write=FAutoConvertFontFamily, default=1};
	__property Fmx::Graphics::TBrush* Fill = {read=FFill, write=SetFill};
	__property Fmx::Graphics::TStrokeBrush* Stroke = {read=FStroke, write=SetStroke};
	__property Fmx::Types::TCorners Corners = {read=FCorners, write=SetCorners, stored=IsCornersStored, nodefault};
	__property Fmx::Types::TSides Sides = {read=FSides, write=SetSides, stored=IsSidesStored, nodefault};
	__property float XRadius = {read=FXRadius, write=SetXRadius};
	__property float YRadius = {read=FYRadius, write=SetYRadius};
	__property float LineSpacing = {read=fLineSpacing, write=fLineSpacing};
	__property bool TextIsHtml = {read=fTextIsHtml, write=fTextIsHtml, default=0};
	__property TouchTargetExpansion;
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall ALLockTexts(Fmx::Controls::TControl* const aParentControl);
extern DELPHI_PACKAGE void __fastcall ALUnLockTexts(Fmx::Controls::TControl* const aParentControl);
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alfmxobjects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXOBJECTS)
using namespace Alfmxobjects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxobjectsHPP
