// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALStringList.pas' rev: 33.00 (Windows)

#ifndef AlstringlistHPP
#define AlstringlistHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Contnrs.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Math.hpp>
#include <ALAVLBinaryTree.hpp>
#include <ALQuickSortList.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alstringlist
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALStringsEnumerator;
class DELPHICLASS TALStrings;
struct TALStringItem;
__interface DELPHIINTERFACE TALStringListSortCompare;
typedef System::DelphiInterface<TALStringListSortCompare> _di_TALStringListSortCompare;
class DELPHICLASS TALStringList;
struct TALNVStringItem;
__interface DELPHIINTERFACE TALNVStringListSortCompare;
typedef System::DelphiInterface<TALNVStringListSortCompare> _di_TALNVStringListSortCompare;
class DELPHICLASS TALNVStringList;
class DELPHICLASS TALAVLStringListBinaryTreeNode;
class DELPHICLASS TALAVLStringList;
class DELPHICLASS TALHashedStringListDictionaryNode;
class DELPHICLASS TALHashedStringList;
class DELPHICLASS TALStringsEnumeratorU;
class DELPHICLASS TALStringsU;
struct TALStringItemU;
__interface DELPHIINTERFACE TALStringListSortCompareU;
typedef System::DelphiInterface<TALStringListSortCompareU> _di_TALStringListSortCompareU;
class DELPHICLASS TALStringListU;
struct TALNVStringItemU;
__interface DELPHIINTERFACE TALNVStringListSortCompareU;
typedef System::DelphiInterface<TALNVStringListSortCompareU> _di_TALNVStringListSortCompareU;
class DELPHICLASS TALNVStringListU;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TALStringsEnumerator : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FIndex;
	TALStrings* FStrings;
	
public:
	__fastcall TALStringsEnumerator(TALStrings* AStrings);
	System::AnsiString __fastcall GetCurrent();
	bool __fastcall MoveNext();
	__property System::AnsiString Current = {read=GetCurrent};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALStringsEnumerator() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALStrings : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	System::AnsiString operator[](int Index) { return this->Strings[Index]; }
	
private:
	char FDelimiter;
	System::AnsiString FLineBreak;
	char FQuoteChar;
	char FNameValueSeparator;
	bool FStrictDelimiter;
	int FUpdateCount;
	bool FProtectedSave;
	System::AnsiString __fastcall GetCommaText();
	System::AnsiString __fastcall GetDelimitedText();
	void __fastcall SetCommaText(const System::AnsiString Value);
	void __fastcall SetDelimitedText(const System::AnsiString Value);
	
protected:
	virtual System::AnsiString __fastcall GetName(int Index);
	virtual System::AnsiString __fastcall GetStrictName(int Index);
	virtual System::AnsiString __fastcall GetValue(const System::AnsiString Name);
	virtual void __fastcall SetValue(const System::AnsiString Name, const System::AnsiString Value);
	virtual System::AnsiString __fastcall GetValueFromIndex(int Index);
	virtual void __fastcall SetValueFromIndex(int Index, const System::AnsiString Value);
	virtual void __fastcall SetPersistentValue(const System::AnsiString Name, const System::AnsiString Value);
	virtual void __fastcall SetPersistentValueFromIndex(int Index, const System::AnsiString Value);
	void __fastcall Error(const System::UnicodeString Msg, int Data)/* overload */;
	void __fastcall Error(System::PResStringRec Msg, int Data)/* overload */;
	System::AnsiString __fastcall ExtractName(const System::AnsiString S);
	virtual System::AnsiString __fastcall Get(int Index) = 0 ;
	virtual int __fastcall GetCapacity();
	virtual int __fastcall GetCount() = 0 ;
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual System::AnsiString __fastcall GetTextStr();
	virtual void __fastcall Put(int Index, const System::AnsiString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetCapacity(int NewCapacity);
	virtual void __fastcall SetTextStr(const System::AnsiString Value);
	virtual void __fastcall SetUpdateState(bool Updating);
	__property int UpdateCount = {read=FUpdateCount, nodefault};
	virtual int __fastcall CompareStrings(const System::AnsiString S1, const System::AnsiString S2);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall virtual TALStrings();
	virtual int __fastcall Add(const System::AnsiString S);
	virtual int __fastcall AddObject(const System::AnsiString S, System::TObject* AObject);
	virtual int __fastcall AddNameValue(const System::AnsiString Name, const System::AnsiString Value);
	virtual int __fastcall AddNameValueObject(const System::AnsiString Name, const System::AnsiString Value, System::TObject* AObject);
	void __fastcall Append(const System::AnsiString S);
	virtual void __fastcall AddStrings(TALStrings* Strings)/* overload */;
	void __fastcall AddStrings(const System::DynamicArray<System::AnsiString> Strings)/* overload */;
	void __fastcall AddStrings(const System::DynamicArray<System::AnsiString> Strings, const System::DynamicArray<System::TObject*> Objects)/* overload */;
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall BeginUpdate();
	virtual void __fastcall Clear() = 0 ;
	virtual void __fastcall Delete(int Index) = 0 ;
	void __fastcall EndUpdate();
	HIDESBASE bool __fastcall Equals(TALStrings* Strings);
	virtual void __fastcall Exchange(int Index1, int Index2);
	TALStringsEnumerator* __fastcall GetEnumerator();
	virtual char * __fastcall GetText();
	virtual int __fastcall IndexOf(const System::AnsiString S);
	virtual int __fastcall IndexOfName(const System::AnsiString Name);
	virtual int __fastcall IndexOfObject(System::TObject* AObject);
	virtual void __fastcall Insert(int Index, const System::AnsiString S) = 0 ;
	virtual void __fastcall InsertObject(int Index, const System::AnsiString S, System::TObject* AObject);
	virtual void __fastcall InsertNameValue(int Index, const System::AnsiString Name, const System::AnsiString Value);
	virtual void __fastcall InsertNameValueObject(int Index, const System::AnsiString Name, const System::AnsiString Value, System::TObject* AObject);
	virtual void __fastcall LoadFromFile(const System::AnsiString FileName);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	virtual void __fastcall Move(int CurIndex, int NewIndex);
	virtual void __fastcall SaveToFile(const System::AnsiString FileName);
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream);
	virtual void __fastcall SetText(char * Text);
	System::DynamicArray<System::AnsiString> __fastcall ToStringArray();
	System::DynamicArray<System::TObject*> __fastcall ToObjectArray();
	__property int Capacity = {read=GetCapacity, write=SetCapacity, nodefault};
	__property System::AnsiString CommaText = {read=GetCommaText, write=SetCommaText};
	__property int Count = {read=GetCount, nodefault};
	__property char Delimiter = {read=FDelimiter, write=FDelimiter, nodefault};
	__property System::AnsiString DelimitedText = {read=GetDelimitedText, write=SetDelimitedText};
	__property System::AnsiString LineBreak = {read=FLineBreak, write=FLineBreak};
	__property System::AnsiString Names[int Index] = {read=GetName};
	__property System::AnsiString StrictNames[int Index] = {read=GetStrictName};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property char QuoteChar = {read=FQuoteChar, write=FQuoteChar, nodefault};
	__property System::AnsiString Values[const System::AnsiString Name] = {read=GetValue, write=SetValue};
	__property System::AnsiString ValueFromIndex[int Index] = {read=GetValueFromIndex, write=SetValueFromIndex};
	__property System::AnsiString PersistentValues[const System::AnsiString Name] = {read=GetValue, write=SetPersistentValue};
	__property System::AnsiString PersistentValueFromIndex[int Index] = {read=GetValueFromIndex, write=SetPersistentValueFromIndex};
	__property char NameValueSeparator = {read=FNameValueSeparator, write=FNameValueSeparator, nodefault};
	__property bool StrictDelimiter = {read=FStrictDelimiter, write=FStrictDelimiter, nodefault};
	__property System::AnsiString Strings[int Index] = {read=Get, write=Put/*, default*/};
	__property System::AnsiString Text = {read=GetTextStr, write=SetTextStr};
	__property bool ProtectedSave = {read=FProtectedSave, write=FProtectedSave, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TALStrings() { }
	
};

#pragma pack(pop)

typedef TALStringItem *PALStringItem;

struct DECLSPEC_DRECORD TALStringItem
{
public:
	System::AnsiString FString;
	System::TObject* FObject;
};


typedef System::DynamicArray<TALStringItem> TALStringItemList;

__interface TALStringListSortCompare  : public System::IInterface 
{
	virtual int __fastcall Invoke(TALStringList* List, int Index1, int Index2) = 0 ;
};

class PASCALIMPLEMENTATION TALStringList : public TALStrings
{
	typedef TALStrings inherited;
	
private:
	TALStringItemList FList;
	int FCount;
	int FCapacity;
	bool FSorted;
	System::Types::TDuplicates FDuplicates;
	bool FCaseSensitive;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnChanging;
	bool FOwnsObject;
	bool FNameValueOptimization;
	void __fastcall ExchangeItems(int Index1, int Index2);
	void __fastcall Grow();
	void __fastcall QuickSort(int L, int R, _di_TALStringListSortCompare SCompare);
	void __fastcall SetSorted(bool Value);
	void __fastcall SetCaseSensitive(const bool Value);
	
protected:
	virtual void __fastcall Changed();
	virtual void __fastcall Changing();
	virtual System::AnsiString __fastcall Get(int Index);
	virtual int __fastcall GetCapacity();
	virtual int __fastcall GetCount();
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual void __fastcall Put(int Index, const System::AnsiString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetCapacity(int NewCapacity);
	virtual void __fastcall SetUpdateState(bool Updating);
	virtual int __fastcall CompareStrings(const System::AnsiString S1, const System::AnsiString S2);
	virtual void __fastcall InsertItem(int Index, const System::AnsiString S, System::TObject* AObject);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall init(bool OwnsObjects);
	
public:
	__fastcall virtual TALStringList()/* overload */;
	__fastcall TALStringList(bool OwnsObjects)/* overload */;
	__fastcall virtual ~TALStringList();
	virtual int __fastcall Add(const System::AnsiString S);
	virtual int __fastcall AddObject(const System::AnsiString S, System::TObject* AObject);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Clear();
	virtual void __fastcall Delete(int Index);
	virtual System::TObject* __fastcall ExtractObject(int Index)/* overload */;
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual bool __fastcall Find(const System::AnsiString S, int &Index);
	bool __fastcall FindName(const System::AnsiString S, int &Index);
	virtual int __fastcall IndexOf(const System::AnsiString S);
	virtual int __fastcall IndexOfName(const System::AnsiString Name);
	virtual void __fastcall Insert(int Index, const System::AnsiString S);
	virtual void __fastcall InsertObject(int Index, const System::AnsiString S, System::TObject* AObject);
	virtual void __fastcall Move(int CurIndex, int NewIndex);
	virtual void __fastcall Sort();
	virtual void __fastcall CustomSort(_di_TALStringListSortCompare Compare);
	__property System::Types::TDuplicates Duplicates = {read=FDuplicates, write=FDuplicates, nodefault};
	__property bool Sorted = {read=FSorted, write=SetSorted, nodefault};
	__property bool CaseSensitive = {read=FCaseSensitive, write=SetCaseSensitive, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnChanging = {read=FOnChanging, write=FOnChanging};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
	__property bool NameValueOptimization = {read=FNameValueOptimization, write=FNameValueOptimization, nodefault};
};


typedef TALNVStringItem *PALNVStringItem;

struct DECLSPEC_DRECORD TALNVStringItem
{
public:
	System::AnsiString FName;
	System::AnsiString FValue;
	bool FNVS;
	System::TObject* FObject;
};


typedef System::DynamicArray<TALNVStringItem> TALNVStringItemList;

__interface TALNVStringListSortCompare  : public System::IInterface 
{
	virtual int __fastcall Invoke(TALNVStringList* List, int Index1, int Index2) = 0 ;
};

class PASCALIMPLEMENTATION TALNVStringList : public TALStrings
{
	typedef TALStrings inherited;
	
private:
	TALNVStringItemList FList;
	int FCount;
	int FCapacity;
	bool FSorted;
	System::Types::TDuplicates FDuplicates;
	bool FCaseSensitive;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnChanging;
	bool FOwnsObject;
	void __fastcall ExchangeItems(int Index1, int Index2);
	void __fastcall Grow();
	void __fastcall QuickSort(int L, int R, _di_TALNVStringListSortCompare SCompare);
	void __fastcall SetSorted(bool Value);
	void __fastcall SetCaseSensitive(const bool Value);
	bool __fastcall ExtractNameValue(const System::AnsiString S, System::AnsiString &Name, System::AnsiString &Value);
	
protected:
	virtual System::AnsiString __fastcall GetName(int Index);
	virtual System::AnsiString __fastcall GetStrictName(int Index);
	virtual System::AnsiString __fastcall GetValue(const System::AnsiString Name);
	virtual void __fastcall SetValue(const System::AnsiString Name, const System::AnsiString Value);
	virtual System::AnsiString __fastcall GetValueFromIndex(int Index);
	virtual void __fastcall SetValueFromIndex(int Index, const System::AnsiString Value);
	virtual void __fastcall SetPersistentValue(const System::AnsiString Name, const System::AnsiString Value);
	virtual void __fastcall SetPersistentValueFromIndex(int Index, const System::AnsiString Value);
	virtual void __fastcall Changed();
	virtual void __fastcall Changing();
	virtual System::AnsiString __fastcall Get(int Index);
	virtual int __fastcall GetCapacity();
	virtual int __fastcall GetCount();
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual System::AnsiString __fastcall GetTextStr();
	virtual void __fastcall Put(int Index, const System::AnsiString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetCapacity(int NewCapacity);
	virtual void __fastcall SetUpdateState(bool Updating);
	virtual int __fastcall CompareStrings(const System::AnsiString S1, const System::AnsiString S2);
	virtual void __fastcall InsertItem(int Index, const System::AnsiString S, System::TObject* AObject)/* overload */;
	virtual void __fastcall InsertItem(int Index, const System::AnsiString Name, bool WithNvS, System::TObject* AObject)/* overload */;
	virtual void __fastcall InsertItem(int Index, const System::AnsiString Name, const System::AnsiString Value, System::TObject* AObject)/* overload */;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall init(bool OwnsObjects);
	
public:
	__fastcall virtual TALNVStringList()/* overload */;
	__fastcall TALNVStringList(bool OwnsObjects)/* overload */;
	__fastcall virtual ~TALNVStringList();
	virtual int __fastcall Add(const System::AnsiString S);
	virtual int __fastcall AddObject(const System::AnsiString S, System::TObject* AObject);
	virtual int __fastcall AddNameValue(const System::AnsiString Name, const System::AnsiString Value);
	virtual int __fastcall AddNameValueObject(const System::AnsiString Name, const System::AnsiString Value, System::TObject* AObject);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Clear();
	virtual void __fastcall Delete(int Index);
	virtual System::TObject* __fastcall ExtractObject(int Index)/* overload */;
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual bool __fastcall Find(const System::AnsiString S, int &Index);
	bool __fastcall FindName(const System::AnsiString Name, int &Index)/* overload */;
	bool __fastcall FindName(const System::AnsiString Name, bool WithNvS, int &Index)/* overload */;
	bool __fastcall FindNameValue(const System::AnsiString Name, const System::AnsiString Value, int &Index);
	virtual int __fastcall IndexOf(const System::AnsiString S);
	virtual int __fastcall IndexOfName(const System::AnsiString Name);
	virtual void __fastcall Insert(int Index, const System::AnsiString S);
	virtual void __fastcall InsertObject(int Index, const System::AnsiString S, System::TObject* AObject);
	virtual void __fastcall InsertNameValue(int Index, const System::AnsiString Name, const System::AnsiString Value);
	virtual void __fastcall InsertNameValueObject(int Index, const System::AnsiString Name, const System::AnsiString Value, System::TObject* AObject);
	virtual void __fastcall Move(int CurIndex, int NewIndex);
	virtual void __fastcall Sort();
	virtual void __fastcall CustomSort(_di_TALNVStringListSortCompare Compare);
	__property System::Types::TDuplicates Duplicates = {read=FDuplicates, write=FDuplicates, nodefault};
	__property bool Sorted = {read=FSorted, write=SetSorted, nodefault};
	__property bool CaseSensitive = {read=FCaseSensitive, write=SetCaseSensitive, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnChanging = {read=FOnChanging, write=FOnChanging};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
};


typedef int __fastcall (*TALAVLStringListSortCompare)(TALAVLStringList* List, int Index1, int Index2);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALAVLStringListBinaryTreeNode : public Alavlbinarytree::TALStringKeyAVLBinaryTreeNode
{
	typedef Alavlbinarytree::TALStringKeyAVLBinaryTreeNode inherited;
	
public:
	System::AnsiString Val;
	System::TObject* Obj;
	int Idx;
	bool Nvs;
public:
	/* TALStringKeyAVLBinaryTreeNode.Create */ inline __fastcall virtual TALAVLStringListBinaryTreeNode() : Alavlbinarytree::TALStringKeyAVLBinaryTreeNode() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALAVLStringListBinaryTreeNode() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TALAVLStringList : public TALStrings
{
	typedef TALStrings inherited;
	
private:
	System::Contnrs::TObjectList* FNodeList;
	Alavlbinarytree::TALStringKeyAVLBinaryTree* FAVLBinTree;
	System::Types::TDuplicates FDuplicates;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnChanging;
	bool FOwnsObject;
	void __fastcall ExchangeItems(int Index1, int Index2);
	void __fastcall QuickSort(int L, int R, TALAVLStringListSortCompare SCompare);
	void __fastcall SetCaseSensitive(const bool Value);
	bool __fastcall GetCaseSensitive();
	bool __fastcall ExtractNameValue(const System::AnsiString S, System::AnsiString &Name, System::AnsiString &Value);
	void __fastcall SetDuplicates(const System::Types::TDuplicates Value);
	
protected:
	virtual System::AnsiString __fastcall GetName(int Index);
	virtual System::AnsiString __fastcall GetStrictName(int Index);
	virtual System::AnsiString __fastcall GetValue(const System::AnsiString Name);
	virtual void __fastcall SetValue(const System::AnsiString Name, const System::AnsiString Value);
	virtual System::AnsiString __fastcall GetValueFromIndex(int Index);
	virtual void __fastcall SetValueFromIndex(int Index, const System::AnsiString Value);
	virtual void __fastcall SetPersistentValue(const System::AnsiString Name, const System::AnsiString Value);
	virtual void __fastcall SetPersistentValueFromIndex(int Index, const System::AnsiString Value);
	virtual void __fastcall Changed();
	virtual void __fastcall Changing();
	virtual System::AnsiString __fastcall Get(int Index);
	virtual int __fastcall GetCount();
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual System::AnsiString __fastcall GetTextStr();
	virtual void __fastcall Put(int Index, const System::AnsiString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetUpdateState(bool Updating);
	virtual void __fastcall InsertItem(int Index, const System::AnsiString Name, const System::AnsiString Value, System::TObject* AObject)/* overload */;
	virtual void __fastcall InsertItem(int Index, const System::AnsiString S, System::TObject* AObject)/* overload */;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall init(bool OwnsObjects);
	
public:
	__fastcall virtual TALAVLStringList()/* overload */;
	__fastcall TALAVLStringList(bool OwnsObjects)/* overload */;
	__fastcall virtual ~TALAVLStringList();
	virtual int __fastcall Add(const System::AnsiString S);
	virtual int __fastcall AddObject(const System::AnsiString S, System::TObject* AObject);
	virtual int __fastcall AddNameValue(const System::AnsiString Name, const System::AnsiString Value);
	virtual int __fastcall AddNameValueObject(const System::AnsiString Name, const System::AnsiString Value, System::TObject* AObject);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Clear();
	virtual void __fastcall Delete(int Index);
	virtual System::TObject* __fastcall ExtractObject(int Index)/* overload */;
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual int __fastcall IndexOf(const System::AnsiString S);
	virtual int __fastcall IndexOfName(const System::AnsiString Name);
	virtual void __fastcall Insert(int Index, const System::AnsiString S);
	virtual void __fastcall InsertObject(int Index, const System::AnsiString S, System::TObject* AObject);
	virtual void __fastcall InsertNameValue(int Index, const System::AnsiString Name, const System::AnsiString Value);
	virtual void __fastcall InsertNameValueObject(int Index, const System::AnsiString Name, const System::AnsiString Value, System::TObject* AObject);
	virtual void __fastcall Move(int CurIndex, int NewIndex);
	virtual void __fastcall CustomSort(TALAVLStringListSortCompare Compare);
	__property System::Types::TDuplicates Duplicates = {read=FDuplicates, write=SetDuplicates, nodefault};
	__property bool CaseSensitive = {read=GetCaseSensitive, write=SetCaseSensitive, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnChanging = {read=FOnChanging, write=FOnChanging};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
};


typedef int __fastcall (*TALHashedStringListSortCompare)(TALHashedStringList* List, int Index1, int Index2);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALHashedStringListDictionaryNode : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::AnsiString ID;
	System::AnsiString Val;
	System::TObject* Obj;
	int Idx;
	bool Nvs;
public:
	/* TObject.Create */ inline __fastcall TALHashedStringListDictionaryNode() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TALHashedStringListDictionaryNode() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TALHashedStringList : public TALStrings
{
	typedef TALStrings inherited;
	
private:
	System::Generics::Collections::TObjectList__1<TALHashedStringListDictionaryNode*>* FNodeList;
	System::Generics::Collections::TObjectDictionary__2<System::AnsiString,TALHashedStringListDictionaryNode*>* FDictionary;
	System::Types::TDuplicates FDuplicates;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnChanging;
	bool FOwnsObject;
	bool FCaseSensitive;
	void __fastcall ExchangeItems(int Index1, int Index2);
	void __fastcall QuickSort(int L, int R, TALHashedStringListSortCompare SCompare);
	void __fastcall SetCaseSensitive(const bool Value);
	bool __fastcall GetCaseSensitive();
	bool __fastcall ExtractNameValue(const System::AnsiString S, System::AnsiString &Name, System::AnsiString &Value);
	void __fastcall SetDuplicates(const System::Types::TDuplicates Value);
	System::Generics::Collections::TObjectDictionary__2<System::AnsiString,TALHashedStringListDictionaryNode*>* __fastcall CreateDictionary(int ACapacity, bool aCaseSensitive);
	
protected:
	virtual System::AnsiString __fastcall GetName(int Index);
	virtual System::AnsiString __fastcall GetStrictName(int Index);
	virtual System::AnsiString __fastcall GetValue(const System::AnsiString Name);
	virtual void __fastcall SetValue(const System::AnsiString Name, const System::AnsiString Value);
	virtual System::AnsiString __fastcall GetValueFromIndex(int Index);
	virtual void __fastcall SetValueFromIndex(int Index, const System::AnsiString Value);
	virtual void __fastcall SetPersistentValue(const System::AnsiString Name, const System::AnsiString Value);
	virtual void __fastcall SetPersistentValueFromIndex(int Index, const System::AnsiString Value);
	virtual void __fastcall Changed();
	virtual void __fastcall Changing();
	virtual System::AnsiString __fastcall Get(int Index);
	virtual int __fastcall GetCount();
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual System::AnsiString __fastcall GetTextStr();
	virtual void __fastcall Put(int Index, const System::AnsiString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetCapacity(int NewCapacity);
	virtual void __fastcall SetUpdateState(bool Updating);
	virtual void __fastcall InsertItem(int Index, const System::AnsiString Name, const System::AnsiString Value, System::TObject* AObject)/* overload */;
	virtual void __fastcall InsertItem(int Index, const System::AnsiString S, System::TObject* AObject)/* overload */;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall init(bool OwnsObjects, int ACapacity);
	
public:
	__fastcall virtual TALHashedStringList()/* overload */;
	__fastcall TALHashedStringList(bool OwnsObjects)/* overload */;
	__fastcall TALHashedStringList(int ACapacity)/* overload */;
	__fastcall TALHashedStringList(bool OwnsObjects, int ACapacity)/* overload */;
	__fastcall virtual ~TALHashedStringList();
	virtual int __fastcall Add(const System::AnsiString S);
	virtual int __fastcall AddObject(const System::AnsiString S, System::TObject* AObject);
	virtual int __fastcall AddNameValue(const System::AnsiString Name, const System::AnsiString Value);
	virtual int __fastcall AddNameValueObject(const System::AnsiString Name, const System::AnsiString Value, System::TObject* AObject);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Clear();
	virtual void __fastcall Delete(int Index);
	virtual System::TObject* __fastcall ExtractObject(int Index)/* overload */;
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual int __fastcall IndexOf(const System::AnsiString S);
	virtual int __fastcall IndexOfName(const System::AnsiString Name);
	virtual void __fastcall Insert(int Index, const System::AnsiString S);
	virtual void __fastcall InsertObject(int Index, const System::AnsiString S, System::TObject* AObject);
	virtual void __fastcall InsertNameValue(int Index, const System::AnsiString Name, const System::AnsiString Value);
	virtual void __fastcall InsertNameValueObject(int Index, const System::AnsiString Name, const System::AnsiString Value, System::TObject* AObject);
	virtual void __fastcall Move(int CurIndex, int NewIndex);
	virtual void __fastcall CustomSort(TALHashedStringListSortCompare Compare);
	__property System::Types::TDuplicates Duplicates = {read=FDuplicates, write=SetDuplicates, nodefault};
	__property bool CaseSensitive = {read=GetCaseSensitive, write=SetCaseSensitive, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnChanging = {read=FOnChanging, write=FOnChanging};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TALStringsEnumeratorU : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FIndex;
	TALStringsU* FStrings;
	
public:
	__fastcall TALStringsEnumeratorU(TALStringsU* AStrings);
	System::UnicodeString __fastcall GetCurrent();
	bool __fastcall MoveNext();
	__property System::UnicodeString Current = {read=GetCurrent};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALStringsEnumeratorU() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALStringsU : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	System::UnicodeString operator[](int Index) { return this->Strings[Index]; }
	
private:
	System::Sysutils::TEncoding* FEncoding;
	System::Sysutils::TEncoding* FDefaultEncoding;
	System::WideChar FDelimiter;
	System::UnicodeString FLineBreak;
	System::WideChar FQuoteChar;
	System::WideChar FNameValueSeparator;
	bool FStrictDelimiter;
	int FUpdateCount;
	bool FWriteBOM;
	bool FProtectedSave;
	System::UnicodeString __fastcall GetCommaText();
	System::UnicodeString __fastcall GetDelimitedText();
	void __fastcall SetCommaText(const System::UnicodeString Value);
	void __fastcall SetDelimitedText(const System::UnicodeString Value);
	void __fastcall SetDefaultEncoding(System::Sysutils::TEncoding* const Value);
	
protected:
	virtual System::UnicodeString __fastcall GetName(int Index);
	virtual System::UnicodeString __fastcall GetStrictName(int Index);
	virtual System::UnicodeString __fastcall GetValue(const System::UnicodeString Name);
	virtual void __fastcall SetValue(const System::UnicodeString Name, const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall GetValueFromIndex(int Index);
	virtual void __fastcall SetValueFromIndex(int Index, const System::UnicodeString Value);
	virtual void __fastcall SetPersistentValue(const System::UnicodeString Name, const System::UnicodeString Value);
	virtual void __fastcall SetPersistentValueFromIndex(int Index, const System::UnicodeString Value);
	void __fastcall Error(const System::UnicodeString Msg, int Data)/* overload */;
	void __fastcall Error(System::PResStringRec Msg, int Data)/* overload */;
	System::UnicodeString __fastcall ExtractName(const System::UnicodeString S);
	virtual System::UnicodeString __fastcall Get(int Index) = 0 ;
	virtual int __fastcall GetCapacity();
	virtual int __fastcall GetCount() = 0 ;
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual System::UnicodeString __fastcall GetTextStr();
	virtual void __fastcall Put(int Index, const System::UnicodeString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetCapacity(int NewCapacity);
	virtual void __fastcall SetEncoding(System::Sysutils::TEncoding* const Value);
	virtual void __fastcall SetTextStr(const System::UnicodeString Value);
	virtual void __fastcall SetUpdateState(bool Updating);
	__property int UpdateCount = {read=FUpdateCount, nodefault};
	virtual int __fastcall CompareStrings(const System::UnicodeString S1, const System::UnicodeString S2);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall virtual TALStringsU();
	__fastcall virtual ~TALStringsU();
	virtual int __fastcall Add(const System::UnicodeString S);
	virtual int __fastcall AddObject(const System::UnicodeString S, System::TObject* AObject);
	virtual int __fastcall AddNameValue(const System::UnicodeString Name, const System::UnicodeString Value);
	virtual int __fastcall AddNameValueObject(const System::UnicodeString Name, const System::UnicodeString Value, System::TObject* AObject);
	void __fastcall Append(const System::UnicodeString S);
	virtual void __fastcall AddStrings(TALStringsU* Strings)/* overload */;
	void __fastcall AddStrings(const System::DynamicArray<System::UnicodeString> Strings)/* overload */;
	void __fastcall AddStrings(const System::DynamicArray<System::UnicodeString> Strings, const System::DynamicArray<System::TObject*> Objects)/* overload */;
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall BeginUpdate();
	virtual void __fastcall Clear() = 0 ;
	virtual void __fastcall Delete(int Index) = 0 ;
	void __fastcall EndUpdate();
	HIDESBASE bool __fastcall Equals(TALStringsU* Strings);
	virtual void __fastcall Exchange(int Index1, int Index2);
	TALStringsEnumeratorU* __fastcall GetEnumerator();
	virtual System::WideChar * __fastcall GetText();
	virtual int __fastcall IndexOf(const System::UnicodeString S);
	virtual int __fastcall IndexOfName(const System::UnicodeString Name);
	virtual int __fastcall IndexOfObject(System::TObject* AObject);
	virtual void __fastcall Insert(int Index, const System::UnicodeString S) = 0 ;
	virtual void __fastcall InsertObject(int Index, const System::UnicodeString S, System::TObject* AObject);
	virtual void __fastcall InsertNameValue(int Index, const System::UnicodeString Name, const System::UnicodeString Value);
	virtual void __fastcall InsertNameValueObject(int Index, const System::UnicodeString Name, const System::UnicodeString Value, System::TObject* AObject);
	virtual void __fastcall LoadFromFile(const System::UnicodeString FileName)/* overload */;
	virtual void __fastcall LoadFromFile(const System::UnicodeString FileName, System::Sysutils::TEncoding* Encoding)/* overload */;
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream)/* overload */;
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream, System::Sysutils::TEncoding* Encoding)/* overload */;
	virtual void __fastcall Move(int CurIndex, int NewIndex);
	virtual void __fastcall SaveToFile(const System::UnicodeString FileName)/* overload */;
	virtual void __fastcall SaveToFile(const System::UnicodeString FileName, System::Sysutils::TEncoding* Encoding)/* overload */;
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream)/* overload */;
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream, System::Sysutils::TEncoding* Encoding)/* overload */;
	virtual void __fastcall SetText(System::WideChar * Text);
	System::DynamicArray<System::UnicodeString> __fastcall ToStringArray();
	System::DynamicArray<System::TObject*> __fastcall ToObjectArray();
	__property int Capacity = {read=GetCapacity, write=SetCapacity, nodefault};
	__property System::UnicodeString CommaText = {read=GetCommaText, write=SetCommaText};
	__property int Count = {read=GetCount, nodefault};
	__property System::Sysutils::TEncoding* DefaultEncoding = {read=FDefaultEncoding, write=SetDefaultEncoding};
	__property System::WideChar Delimiter = {read=FDelimiter, write=FDelimiter, nodefault};
	__property System::UnicodeString DelimitedText = {read=GetDelimitedText, write=SetDelimitedText};
	__property System::Sysutils::TEncoding* Encoding = {read=FEncoding};
	__property System::UnicodeString LineBreak = {read=FLineBreak, write=FLineBreak};
	__property System::UnicodeString Names[int Index] = {read=GetName};
	__property System::UnicodeString StrictNames[int Index] = {read=GetStrictName};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property System::WideChar QuoteChar = {read=FQuoteChar, write=FQuoteChar, nodefault};
	__property System::UnicodeString Values[const System::UnicodeString Name] = {read=GetValue, write=SetValue};
	__property System::UnicodeString ValueFromIndex[int Index] = {read=GetValueFromIndex, write=SetValueFromIndex};
	__property System::UnicodeString PersistentValues[const System::UnicodeString Name] = {read=GetValue, write=SetPersistentValue};
	__property System::UnicodeString PersistentValueFromIndex[int Index] = {read=GetValueFromIndex, write=SetPersistentValueFromIndex};
	__property System::WideChar NameValueSeparator = {read=FNameValueSeparator, write=FNameValueSeparator, nodefault};
	__property bool StrictDelimiter = {read=FStrictDelimiter, write=FStrictDelimiter, nodefault};
	__property System::UnicodeString Strings[int Index] = {read=Get, write=Put/*, default*/};
	__property System::UnicodeString Text = {read=GetTextStr, write=SetTextStr};
	__property bool WriteBOM = {read=FWriteBOM, write=FWriteBOM, nodefault};
	__property bool ProtectedSave = {read=FProtectedSave, write=FProtectedSave, nodefault};
};

#pragma pack(pop)

typedef TALStringItemU *PALStringItemU;

struct DECLSPEC_DRECORD TALStringItemU
{
public:
	System::UnicodeString FString;
	System::TObject* FObject;
};


typedef System::DynamicArray<TALStringItemU> TALStringItemListU;

__interface TALStringListSortCompareU  : public System::IInterface 
{
	virtual int __fastcall Invoke(TALStringListU* List, int Index1, int Index2) = 0 ;
};

class PASCALIMPLEMENTATION TALStringListU : public TALStringsU
{
	typedef TALStringsU inherited;
	
private:
	TALStringItemListU FList;
	int FCount;
	int FCapacity;
	bool FSorted;
	System::Types::TDuplicates FDuplicates;
	bool FCaseSensitive;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnChanging;
	bool FOwnsObject;
	bool FNameValueOptimization;
	void __fastcall ExchangeItems(int Index1, int Index2);
	void __fastcall Grow();
	void __fastcall QuickSort(int L, int R, _di_TALStringListSortCompareU SCompare);
	void __fastcall SetSorted(bool Value);
	void __fastcall SetCaseSensitive(const bool Value);
	
protected:
	virtual void __fastcall Changed();
	virtual void __fastcall Changing();
	virtual System::UnicodeString __fastcall Get(int Index);
	virtual int __fastcall GetCapacity();
	virtual int __fastcall GetCount();
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual void __fastcall Put(int Index, const System::UnicodeString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetCapacity(int NewCapacity);
	virtual void __fastcall SetUpdateState(bool Updating);
	virtual int __fastcall CompareStrings(const System::UnicodeString S1, const System::UnicodeString S2);
	virtual void __fastcall InsertItem(int Index, const System::UnicodeString S, System::TObject* AObject);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall init(bool OwnsObjects);
	
public:
	__fastcall virtual TALStringListU()/* overload */;
	__fastcall TALStringListU(bool OwnsObjects)/* overload */;
	__fastcall virtual ~TALStringListU();
	virtual int __fastcall Add(const System::UnicodeString S);
	virtual int __fastcall AddObject(const System::UnicodeString S, System::TObject* AObject);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Clear();
	virtual void __fastcall Delete(int Index);
	virtual System::TObject* __fastcall ExtractObject(int Index)/* overload */;
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual bool __fastcall Find(const System::UnicodeString S, int &Index);
	bool __fastcall FindName(const System::UnicodeString S, int &Index);
	virtual int __fastcall IndexOf(const System::UnicodeString S);
	virtual int __fastcall IndexOfName(const System::UnicodeString Name);
	virtual void __fastcall Insert(int Index, const System::UnicodeString S);
	virtual void __fastcall InsertObject(int Index, const System::UnicodeString S, System::TObject* AObject);
	virtual void __fastcall Move(int CurIndex, int NewIndex);
	virtual void __fastcall Sort();
	virtual void __fastcall CustomSort(_di_TALStringListSortCompareU Compare);
	__property System::Types::TDuplicates Duplicates = {read=FDuplicates, write=FDuplicates, nodefault};
	__property bool Sorted = {read=FSorted, write=SetSorted, nodefault};
	__property bool CaseSensitive = {read=FCaseSensitive, write=SetCaseSensitive, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnChanging = {read=FOnChanging, write=FOnChanging};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
	__property bool NameValueOptimization = {read=FNameValueOptimization, write=FNameValueOptimization, nodefault};
};


typedef TALNVStringItemU *PALNVStringItemU;

struct DECLSPEC_DRECORD TALNVStringItemU
{
public:
	System::UnicodeString FName;
	System::UnicodeString FValue;
	bool FNVS;
	System::TObject* FObject;
};


typedef System::DynamicArray<TALNVStringItemU> TALNVStringItemListU;

__interface TALNVStringListSortCompareU  : public System::IInterface 
{
	virtual int __fastcall Invoke(TALNVStringListU* List, int Index1, int Index2) = 0 ;
};

class PASCALIMPLEMENTATION TALNVStringListU : public TALStringsU
{
	typedef TALStringsU inherited;
	
private:
	TALNVStringItemListU FList;
	int FCount;
	int FCapacity;
	bool FSorted;
	System::Types::TDuplicates FDuplicates;
	bool FCaseSensitive;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnChanging;
	bool FOwnsObject;
	void __fastcall ExchangeItems(int Index1, int Index2);
	void __fastcall Grow();
	void __fastcall QuickSort(int L, int R, _di_TALNVStringListSortCompareU SCompare);
	void __fastcall SetSorted(bool Value);
	void __fastcall SetCaseSensitive(const bool Value);
	bool __fastcall ExtractNameValue(const System::UnicodeString S, System::UnicodeString &Name, System::UnicodeString &Value);
	
protected:
	virtual System::UnicodeString __fastcall GetName(int Index);
	virtual System::UnicodeString __fastcall GetStrictName(int Index);
	virtual System::UnicodeString __fastcall GetValue(const System::UnicodeString Name);
	virtual void __fastcall SetValue(const System::UnicodeString Name, const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall GetValueFromIndex(int Index);
	virtual void __fastcall SetValueFromIndex(int Index, const System::UnicodeString Value);
	virtual void __fastcall SetPersistentValue(const System::UnicodeString Name, const System::UnicodeString Value);
	virtual void __fastcall SetPersistentValueFromIndex(int Index, const System::UnicodeString Value);
	virtual void __fastcall Changed();
	virtual void __fastcall Changing();
	virtual System::UnicodeString __fastcall Get(int Index);
	virtual int __fastcall GetCapacity();
	virtual int __fastcall GetCount();
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual System::UnicodeString __fastcall GetTextStr();
	virtual void __fastcall Put(int Index, const System::UnicodeString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetCapacity(int NewCapacity);
	virtual void __fastcall SetUpdateState(bool Updating);
	virtual int __fastcall CompareStrings(const System::UnicodeString S1, const System::UnicodeString S2);
	virtual void __fastcall InsertItem(int Index, const System::UnicodeString S, System::TObject* AObject)/* overload */;
	virtual void __fastcall InsertItem(int Index, const System::UnicodeString Name, bool WithNvS, System::TObject* AObject)/* overload */;
	virtual void __fastcall InsertItem(int Index, const System::UnicodeString Name, const System::UnicodeString Value, System::TObject* AObject)/* overload */;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall init(bool OwnsObjects);
	
public:
	__fastcall virtual TALNVStringListU()/* overload */;
	__fastcall TALNVStringListU(bool OwnsObjects)/* overload */;
	__fastcall virtual ~TALNVStringListU();
	virtual int __fastcall Add(const System::UnicodeString S);
	virtual int __fastcall AddObject(const System::UnicodeString S, System::TObject* AObject);
	virtual int __fastcall AddNameValue(const System::UnicodeString Name, const System::UnicodeString Value);
	virtual int __fastcall AddNameValueObject(const System::UnicodeString Name, const System::UnicodeString Value, System::TObject* AObject);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Clear();
	virtual void __fastcall Delete(int Index);
	virtual System::TObject* __fastcall ExtractObject(int Index)/* overload */;
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual bool __fastcall Find(const System::UnicodeString S, int &Index);
	bool __fastcall FindName(const System::UnicodeString Name, int &Index)/* overload */;
	bool __fastcall FindName(const System::UnicodeString Name, bool WithNvS, int &Index)/* overload */;
	bool __fastcall FindNameValue(const System::UnicodeString Name, const System::UnicodeString Value, int &Index);
	virtual int __fastcall IndexOf(const System::UnicodeString S);
	virtual int __fastcall IndexOfName(const System::UnicodeString Name);
	virtual void __fastcall Insert(int Index, const System::UnicodeString S);
	virtual void __fastcall InsertObject(int Index, const System::UnicodeString S, System::TObject* AObject);
	virtual void __fastcall InsertNameValue(int Index, const System::UnicodeString Name, const System::UnicodeString Value);
	virtual void __fastcall InsertNameValueObject(int Index, const System::UnicodeString Name, const System::UnicodeString Value, System::TObject* AObject);
	virtual void __fastcall Move(int CurIndex, int NewIndex);
	virtual void __fastcall Sort();
	virtual void __fastcall CustomSort(_di_TALNVStringListSortCompareU Compare);
	__property System::Types::TDuplicates Duplicates = {read=FDuplicates, write=FDuplicates, nodefault};
	__property bool Sorted = {read=FSorted, write=SetSorted, nodefault};
	__property bool CaseSensitive = {read=FCaseSensitive, write=SetCaseSensitive, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnChanging = {read=FOnChanging, write=FOnChanging};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Alstringlist */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALSTRINGLIST)
using namespace Alstringlist;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlstringlistHPP
