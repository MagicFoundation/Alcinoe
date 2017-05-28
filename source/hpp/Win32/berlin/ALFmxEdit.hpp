﻿// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxEdit.pas' rev: 31.00 (Windows)

#ifndef AlfmxeditHPP
#define AlfmxeditHPP

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
#include <FMX.Edit.hpp>
#include <FMX.Types.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.Controls.hpp>
#include <ALFmxObjects.hpp>
#include <FMX.Objects.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TALEdit : public Alfmxobjects::TALRectangle
{
	typedef Alfmxobjects::TALRectangle inherited;
	
private:
	System::UnicodeString fDefStyleAttr;
	bool FAutoTranslate;
	bool FAutoConvertFontFamily;
	System::Classes::TNotifyEvent fOnChangeTracking;
	Fmx::Graphics::TTextSettings* FTextSettings;
	System::Uitypes::TAlphaColor fTextPromptColor;
	Fmx::Edit::TEdit* fEditControl;
	System::Uitypes::TAlphaColor fTintColor;
	System::UnicodeString __fastcall GetTextPrompt(void);
	void __fastcall setTextPrompt(const System::UnicodeString Value);
	System::Uitypes::TAlphaColor __fastcall GetTextPromptColor(void);
	void __fastcall setTextPromptColor(const System::Uitypes::TAlphaColor Value);
	System::Uitypes::TAlphaColor __fastcall GetTintColor(void);
	void __fastcall setTintColor(const System::Uitypes::TAlphaColor Value);
	Fmx::Graphics::TTextSettings* __fastcall GetTextSettings(void);
	void __fastcall SetTextSettings(Fmx::Graphics::TTextSettings* const Value);
	void __fastcall OnFontChanged(System::TObject* Sender);
	System::UnicodeString __fastcall getText(void);
	void __fastcall SetText(const System::UnicodeString Value);
	void __fastcall DoChangeTracking(System::TObject* Sender);
	void __fastcall SetKeyboardType(Fmx::Types::TVirtualKeyboardType Value);
	Fmx::Types::TVirtualKeyboardType __fastcall GetKeyboardType(void);
	void __fastcall SetPassword(const bool Value);
	bool __fastcall GetPassword(void);
	void __fastcall SetCheckSpelling(const bool Value);
	bool __fastcall GetCheckSpelling(void);
	void __fastcall SetReturnKeyType(const Fmx::Types::TReturnKeyType Value);
	Fmx::Types::TReturnKeyType __fastcall GetReturnKeyType(void);
	void __fastcall SetDefStyleAttr(const System::UnicodeString Value);
	System::Classes::TNotifyEvent __fastcall GetOnEnter(void);
	void __fastcall SetOnEnter(System::Classes::TNotifyEvent AValue);
	System::Classes::TNotifyEvent __fastcall GetOnExit(void);
	void __fastcall SetOnExit(System::Classes::TNotifyEvent AValue);
	void __fastcall CreateEditControl(void);
	
protected:
	virtual System::Types::TSizeF __fastcall GetDefaultSize(void);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall StrokeChanged(System::TObject* Sender);
	virtual void __fastcall SetSides(const Fmx::Types::TSides Value);
	virtual bool __fastcall GetCanFocus(void);
	
public:
	__fastcall virtual TALEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALEdit(void);
	
__published:
	__property System::UnicodeString DefStyleAttr = {read=fDefStyleAttr, write=SetDefStyleAttr};
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Cursor = {default=-4};
	__property CanFocus = {default=1};
	__property DisableFocusEffect = {default=0};
	__property Fmx::Types::TVirtualKeyboardType KeyboardType = {read=GetKeyboardType, write=SetKeyboardType, default=0};
	__property Fmx::Types::TReturnKeyType ReturnKeyType = {read=GetReturnKeyType, write=SetReturnKeyType, default=0};
	__property bool Password = {read=GetPassword, write=SetPassword, default=0};
	__property System::UnicodeString Text = {read=getText, write=SetText};
	__property Fmx::Graphics::TTextSettings* TextSettings = {read=GetTextSettings, write=SetTextSettings};
	__property Hint = {default=0};
	__property System::UnicodeString TextPrompt = {read=GetTextPrompt, write=setTextPrompt};
	__property System::Uitypes::TAlphaColor TextPromptColor = {read=GetTextPromptColor, write=setTextPromptColor, default=0};
	__property System::Uitypes::TAlphaColor TintColor = {read=GetTintColor, write=setTintColor, default=0};
	__property bool AutoTranslate = {read=FAutoTranslate, write=FAutoTranslate, default=1};
	__property bool AutoConvertFontFamily = {read=FAutoConvertFontFamily, write=FAutoConvertFontFamily, default=1};
	__property TouchTargetExpansion;
	__property bool CheckSpelling = {read=GetCheckSpelling, write=SetCheckSpelling, default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property System::Classes::TNotifyEvent OnChangeTracking = {read=fOnChangeTracking, write=fOnChangeTracking};
	__property OnKeyDown;
	__property OnKeyUp;
	__property System::Classes::TNotifyEvent OnEnter = {read=GetOnEnter, write=SetOnEnter};
	__property System::Classes::TNotifyEvent OnExit = {read=GetOnExit, write=SetOnExit};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alfmxedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXEDIT)
using namespace Alfmxedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxeditHPP
