// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxMemo.pas' rev: 31.00 (Windows)

#ifndef AlfmxmemoHPP
#define AlfmxmemoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <System.UITypes.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Memo.Style.hpp>
#include <FMX.Memo.hpp>
#include <FMX.Types.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.Controls.hpp>
#include <ALFmxEdit.hpp>
#include <ALFmxObjects.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Objects.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxmemo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALStyledMemo;
class DELPHICLASS TALMemo;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TALStyledMemo : public Fmx::Memo::TMemo
{
	typedef Fmx::Memo::TMemo inherited;
	
private:
	Fmx::Types::TReturnKeyType fReturnKeyType;
	Fmx::Memo::Style::TStyledMemo* fStyledMemo;
	Fmx::Types::TBounds* FPadding;
	Fmx::Stdctrls::TScrollBar* FVertScrollBar;
	Alfmxobjects::TALText* fTextPromptControl;
	System::Classes::TNotifyEvent FOnChangeTracking;
	Fmx::Graphics::TTextSettings* FTextSettings;
	void __fastcall OnApplyStyleLookupImpl(System::TObject* sender);
	HIDESBASE void __fastcall SetPadding(Fmx::Types::TBounds* const Value);
	HIDESBASE Fmx::Types::TBounds* __fastcall GetPadding(void);
	System::UnicodeString __fastcall GetTextPrompt(void);
	void __fastcall setTextPrompt(const System::UnicodeString Value);
	System::Uitypes::TAlphaColor __fastcall GetTextPromptColor(void);
	void __fastcall setTextPromptColor(const System::Uitypes::TAlphaColor Value);
	void __fastcall OnChangeTrackingImpl(System::TObject* sender);
	HIDESBASE Fmx::Graphics::TTextSettings* __fastcall GetTextSettings(void);
	HIDESBASE void __fastcall SetTextSettings(Fmx::Graphics::TTextSettings* const Value);
	void __fastcall OnFontChanged(System::TObject* Sender);
	
protected:
	virtual void __fastcall DoEnter(void);
	virtual void __fastcall realignScrollBars(void);
	virtual void __fastcall Resize(void);
	
public:
	__fastcall virtual TALStyledMemo(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual ~TALStyledMemo(void);
	float __fastcall getLineHeight(void);
	int __fastcall getLineCount(void);
	__property System::UnicodeString TextPrompt = {read=GetTextPrompt, write=setTextPrompt};
	__property System::Uitypes::TAlphaColor TextPromptColor = {read=GetTextPromptColor, write=setTextPromptColor, default=0};
	__property Fmx::Types::TBounds* Padding = {read=GetPadding, write=SetPadding};
	__property System::Classes::TNotifyEvent OnChangeTracking = {read=FOnChangeTracking, write=FOnChangeTracking};
	__property Fmx::Graphics::TTextSettings* TextSettings = {read=GetTextSettings, write=SetTextSettings};
	__property Fmx::Types::TReturnKeyType ReturnKeyType = {read=fReturnKeyType, write=fReturnKeyType, default=0};
};


class PASCALIMPLEMENTATION TALMemo : public Alfmxobjects::TALRectangle
{
	typedef Alfmxobjects::TALRectangle inherited;
	
private:
	Fmx::Types::TBounds* fPadding;
	System::UnicodeString fDefStyleAttr;
	bool FAutoTranslate;
	bool FAutoConvertFontFamily;
	System::Classes::TNotifyEvent fOnChangeTracking;
	System::Classes::TNotifyEvent fOnEnter;
	System::Classes::TNotifyEvent fOnExit;
	Fmx::Graphics::TTextSettings* FTextSettings;
	System::Uitypes::TAlphaColor fTintColor;
	Alfmxedit::TALAutoCapitalizationType fAutoCapitalizationType;
	float fLineSpacingMultiplier;
	float fLineSpacingExtra;
	TALStyledMemo* fMemoControl;
	System::UnicodeString __fastcall GetTextPrompt(void);
	void __fastcall setTextPrompt(const System::UnicodeString Value);
	System::Uitypes::TAlphaColor __fastcall GetTextPromptColor(void);
	void __fastcall setTextPromptColor(const System::Uitypes::TAlphaColor Value);
	System::Uitypes::TAlphaColor __fastcall GetTintColor(void);
	void __fastcall setTintColor(const System::Uitypes::TAlphaColor Value);
	float __fastcall GetLineSpacingMultiplier(void);
	void __fastcall SetLineSpacingMultiplier(const float Value);
	bool __fastcall LineSpacingMultiplierStored(void);
	float __fastcall GetLineSpacingExtra(void);
	void __fastcall SetLineSpacingExtra(const float Value);
	bool __fastcall LineSpacingExtraStored(void);
	Fmx::Graphics::TTextSettings* __fastcall GetTextSettings(void);
	void __fastcall SetTextSettings(Fmx::Graphics::TTextSettings* const Value);
	void __fastcall OnFontChanged(System::TObject* Sender);
	System::UnicodeString __fastcall getText(void);
	void __fastcall SetText(const System::UnicodeString Value);
	void __fastcall OnChangeTrackingImpl(System::TObject* Sender);
	void __fastcall OnEnterImpl(System::TObject* Sender);
	void __fastcall OnExitImpl(System::TObject* Sender);
	void __fastcall SetKeyboardType(Fmx::Types::TVirtualKeyboardType Value);
	Fmx::Types::TVirtualKeyboardType __fastcall GetKeyboardType(void);
	void __fastcall setAutoCapitalizationType(const Alfmxedit::TALAutoCapitalizationType Value);
	Alfmxedit::TALAutoCapitalizationType __fastcall GetAutoCapitalizationType(void);
	void __fastcall SetCheckSpelling(const bool Value);
	bool __fastcall GetCheckSpelling(void);
	void __fastcall SetReturnKeyType(const Fmx::Types::TReturnKeyType Value);
	Fmx::Types::TReturnKeyType __fastcall GetReturnKeyType(void);
	void __fastcall SetDefStyleAttr(const System::UnicodeString Value);
	HIDESBASE void __fastcall SetPadding(Fmx::Types::TBounds* const Value);
	HIDESBASE Fmx::Types::TBounds* __fastcall GetPadding(void);
	void __fastcall CreateMemoControl(void);
	HIDESBASE void __fastcall PaddingChangedHandler(System::TObject* Sender);
	bool __fastcall GetContainFocus(void);
	void __fastcall SetMaxLength(const int Value);
	int __fastcall GetMaxLength(void);
	
protected:
	virtual System::Types::TSizeF __fastcall GetDefaultSize(void);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall StrokeChanged(System::TObject* Sender);
	virtual void __fastcall SetSides(const Fmx::Types::TSides Value);
	virtual bool __fastcall GetCanFocus(void);
	
public:
	__fastcall virtual TALMemo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALMemo(void);
	void __fastcall AddNativeView(void);
	void __fastcall RemoveNativeView(void);
	void __fastcall setSelection(const int aStart, const int aStop)/* overload */;
	void __fastcall setSelection(const int aindex)/* overload */;
	float __fastcall getLineHeight(void);
	int __fastcall getLineCount(void);
	__property bool ContainFocus = {read=GetContainFocus, nodefault};
	
__published:
	__property System::UnicodeString DefStyleAttr = {read=fDefStyleAttr, write=SetDefStyleAttr};
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Cursor = {default=-4};
	__property CanFocus = {default=1};
	__property DisableFocusEffect = {default=0};
	__property Fmx::Types::TVirtualKeyboardType KeyboardType = {read=GetKeyboardType, write=SetKeyboardType, default=0};
	__property Alfmxedit::TALAutoCapitalizationType AutoCapitalizationType = {read=GetAutoCapitalizationType, write=setAutoCapitalizationType, default=0};
	__property Fmx::Types::TReturnKeyType ReturnKeyType = {read=GetReturnKeyType, write=SetReturnKeyType, default=0};
	__property int MaxLength = {read=GetMaxLength, write=SetMaxLength, default=0};
	__property System::UnicodeString Text = {read=getText, write=SetText};
	__property Fmx::Graphics::TTextSettings* TextSettings = {read=GetTextSettings, write=SetTextSettings};
	__property Hint = {default=0};
	__property System::UnicodeString TextPrompt = {read=GetTextPrompt, write=setTextPrompt};
	__property System::Uitypes::TAlphaColor TextPromptColor = {read=GetTextPromptColor, write=setTextPromptColor, default=0};
	__property float LineSpacingMultiplier = {read=GetLineSpacingMultiplier, write=SetLineSpacingMultiplier, stored=LineSpacingMultiplierStored};
	__property float LineSpacingExtra = {read=GetLineSpacingExtra, write=SetLineSpacingExtra, stored=LineSpacingExtraStored};
	__property System::Uitypes::TAlphaColor TintColor = {read=GetTintColor, write=setTintColor, default=0};
	__property bool AutoTranslate = {read=FAutoTranslate, write=FAutoTranslate, default=1};
	__property bool AutoConvertFontFamily = {read=FAutoConvertFontFamily, write=FAutoConvertFontFamily, default=1};
	__property TouchTargetExpansion;
	__property bool CheckSpelling = {read=GetCheckSpelling, write=SetCheckSpelling, default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property System::Classes::TNotifyEvent OnChangeTracking = {read=fOnChangeTracking, write=fOnChangeTracking};
	__property System::Classes::TNotifyEvent OnEnter = {read=fOnEnter, write=fOnEnter};
	__property System::Classes::TNotifyEvent OnExit = {read=fOnExit, write=fOnExit};
	__property Fmx::Types::TBounds* Padding = {read=GetPadding, write=SetPadding};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alfmxmemo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXMEMO)
using namespace Alfmxmemo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxmemoHPP
