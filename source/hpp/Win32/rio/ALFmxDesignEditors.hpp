// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxDesignEditors.pas' rev: 33.00 (Windows)

#ifndef AlfmxdesigneditorsHPP
#define AlfmxdesigneditorsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Generics.Collections.hpp>
#include <FMX.Types.hpp>
#include <DesignIntf.hpp>
#include <DesignEditors.hpp>
#include <DesignMenus.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxdesigneditors
{
//-- forward type declarations -----------------------------------------------
struct TALItemClassDesc;
class DELPHICLASS TALItemsEditor;
class DELPHICLASS TALTabControlEditor;
class DELPHICLASS TALTabItemEditor;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TALItemClassDesc
{
public:
	Fmx::Types::TFmxObjectClass ItemClass;
	bool CanContainSimilarItem;
	bool ShowOnlyInMenu;
	__fastcall TALItemClassDesc(const Fmx::Types::TFmxObjectClass AItemClass, const bool ACanContaineSimilarItem, const bool AShowOnlyInMenu);
	TALItemClassDesc() {}
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TALItemsEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
	
private:
	typedef System::DynamicArray<TALItemClassDesc> _TALItemsEditor__1;
	
	
private:
	static System::Generics::Collections::TDictionary__2<System::UnicodeString,int>* FListOfLastItems;
	
protected:
	bool FAllowChild;
	_TALItemsEditor__1 FItemsClasses;
	virtual void __fastcall DoCreateItem(System::TObject* Sender);
	int __fastcall GetIndexOfItemClass();
	void __fastcall SetIndexOfItemClass(const int Value);
	virtual bool __fastcall CanShow();
	__property int IndexOfItemClass = {read=GetIndexOfItemClass, write=SetIndexOfItemClass, nodefault};
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
	virtual void __fastcall PrepareItem(int Index, const Designmenus::_di_IMenuItem AItem);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TALItemsEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALItemsEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALTabControlEditor : public TALItemsEditor
{
	typedef TALItemsEditor inherited;
	
private:
	int FEditorNextTab;
	int FEditorPrevTab;
	int FEditorDeleteTab;
	int FVerbCount;
	int __fastcall GetTabIndex();
	
protected:
	virtual void __fastcall DoCreateItem(System::TObject* Sender);
	
public:
	__fastcall virtual TALTabControlEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner);
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
	virtual void __fastcall PrepareItem(int Index, const Designmenus::_di_IMenuItem AItem);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALTabControlEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALTabItemEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
	virtual void __fastcall PrepareItem(int Index, const Designmenus::_di_IMenuItem AItem);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TALTabItemEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALTabItemEditor() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _SNewItem;
#define Alfmxdesigneditors_SNewItem System::LoadResourceString(&Alfmxdesigneditors::_SNewItem)
extern DELPHI_PACKAGE System::ResourceString _SNewLastItem;
#define Alfmxdesigneditors_SNewLastItem System::LoadResourceString(&Alfmxdesigneditors::_SNewLastItem)
extern DELPHI_PACKAGE System::ResourceString _SItems;
#define Alfmxdesigneditors_SItems System::LoadResourceString(&Alfmxdesigneditors::_SItems)
extern DELPHI_PACKAGE System::ResourceString _SNextTab;
#define Alfmxdesigneditors_SNextTab System::LoadResourceString(&Alfmxdesigneditors::_SNextTab)
extern DELPHI_PACKAGE System::ResourceString _SPrevTab;
#define Alfmxdesigneditors_SPrevTab System::LoadResourceString(&Alfmxdesigneditors::_SPrevTab)
extern DELPHI_PACKAGE System::ResourceString _SUnnamedTab;
#define Alfmxdesigneditors_SUnnamedTab System::LoadResourceString(&Alfmxdesigneditors::_SUnnamedTab)
extern DELPHI_PACKAGE System::ResourceString _SDeleteItem;
#define Alfmxdesigneditors_SDeleteItem System::LoadResourceString(&Alfmxdesigneditors::_SDeleteItem)
extern DELPHI_PACKAGE System::ResourceString _SSetActive;
#define Alfmxdesigneditors_SSetActive System::LoadResourceString(&Alfmxdesigneditors::_SSetActive)
static const System::Int8 EDITOR_CREATE_ITEM = System::Int8(0x0);
static const System::Int8 EDITOR_NEW_ITEM = System::Int8(0x1);
static const System::Int8 EDITOR_SET_ACTIVE = System::Int8(0x0);
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alfmxdesigneditors */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXDESIGNEDITORS)
using namespace Alfmxdesigneditors;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxdesigneditorsHPP
