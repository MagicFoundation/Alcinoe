// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALQuickSortList.pas' rev: 32.00 (Windows)

#ifndef AlquicksortlistHPP
#define AlquicksortlistHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alquicksortlist
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALBaseQuickSortList;
struct TALIntegerListItem;
class DELPHICLASS TALIntegerList;
struct TALCardinalListItem;
class DELPHICLASS TALCardinalList;
struct TALInt64ListItem;
class DELPHICLASS TALInt64List;
struct TALNativeIntListItem;
class DELPHICLASS TALNativeIntList;
struct TALDoubleListItem;
class DELPHICLASS TALDoubleList;
//-- type declarations -------------------------------------------------------
typedef int __fastcall (*TALQuickSortListCompare)(System::TObject* List, int Index1, int Index2);

typedef System::DynamicArray<void *> TALQuickSortPointerList;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALBaseQuickSortList : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TALQuickSortPointerList FList;
	int FCount;
	int FCapacity;
	bool FSorted;
	System::Types::TDuplicates FDuplicates;
	void __fastcall SetSorted(bool Value);
	void __fastcall QuickSort(int L, int R, TALQuickSortListCompare SCompare);
	
protected:
	void * __fastcall Get(int Index);
	void __fastcall Grow(void);
	void __fastcall Put(int Index, void * Item);
	virtual void __fastcall Notify(void * Ptr, System::Classes::TListNotification Action);
	void __fastcall SetCapacity(int NewCapacity);
	void __fastcall SetCount(int NewCount);
	virtual int __fastcall CompareItems(const int Index1, const int Index2);
	void __fastcall ExchangeItems(int Index1, int Index2);
	void __fastcall InsertItem(int Index, void * Item);
	void __fastcall Insert(int Index, void * Item);
	__property TALQuickSortPointerList List = {read=FList};
	
public:
	__fastcall TALBaseQuickSortList(void);
	__fastcall virtual ~TALBaseQuickSortList(void);
	virtual void __fastcall Clear(void);
	void __fastcall Delete(int Index);
	__classmethod virtual void __fastcall Error(const System::UnicodeString Msg, NativeInt Data)/* overload */;
	__classmethod void __fastcall Error(System::PResStringRec Msg, NativeInt Data)/* overload */;
	void __fastcall Exchange(int Index1, int Index2);
	TALBaseQuickSortList* __fastcall Expand(void);
	virtual void __fastcall CustomSort(TALQuickSortListCompare Compare);
	virtual void __fastcall Sort(void);
	__property bool Sorted = {read=FSorted, write=SetSorted, nodefault};
	__property int Capacity = {read=FCapacity, write=SetCapacity, nodefault};
	__property int Count = {read=FCount, write=SetCount, nodefault};
	__property System::Types::TDuplicates Duplicates = {read=FDuplicates, write=FDuplicates, nodefault};
};

#pragma pack(pop)

typedef TALIntegerListItem *PALIntegerListItem;

struct DECLSPEC_DRECORD TALIntegerListItem
{
public:
	int FInteger;
	System::TObject* FObject;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TALIntegerList : public TALBaseQuickSortList
{
	typedef TALBaseQuickSortList inherited;
	
public:
	int operator[](int Index) { return this->Items[Index]; }
	
private:
	bool FOwnsObject;
	int __fastcall GetItem(int Index);
	void __fastcall SetItem(int Index, const int Item);
	System::TObject* __fastcall GetObject(int Index);
	void __fastcall PutObject(int Index, System::TObject* AObject);
	
protected:
	virtual void __fastcall Notify(void * Ptr, System::Classes::TListNotification Action);
	HIDESBASE void __fastcall InsertItem(int Index, const int item, System::TObject* AObject);
	virtual int __fastcall CompareItems(const int Index1, const int Index2);
	
public:
	__fastcall TALIntegerList(void)/* overload */;
	__fastcall TALIntegerList(bool OwnsObjects)/* overload */;
	int __fastcall IndexOf(int Item);
	int __fastcall IndexOfObject(System::TObject* AObject);
	int __fastcall Add(const int Item);
	int __fastcall AddObject(const int Item, System::TObject* AObject);
	bool __fastcall TryAdd(const int Item);
	bool __fastcall TryAddObject(const int Item, System::TObject* AObject);
	bool __fastcall Find(int item, int &Index);
	HIDESBASE void __fastcall Insert(int Index, const int item);
	void __fastcall InsertObject(int Index, const int item, System::TObject* AObject);
	__property int Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
	int __fastcall Push(int Item);
	int __fastcall Pop(void);
	int __fastcall Peek(void);
	System::DynamicArray<int> __fastcall ToArray(void);
public:
	/* TALBaseQuickSortList.Destroy */ inline __fastcall virtual ~TALIntegerList(void) { }
	
};

#pragma pack(pop)

typedef TALCardinalListItem *PALCardinalListItem;

struct DECLSPEC_DRECORD TALCardinalListItem
{
public:
	unsigned FCardinal;
	System::TObject* FObject;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TALCardinalList : public TALBaseQuickSortList
{
	typedef TALBaseQuickSortList inherited;
	
public:
	unsigned operator[](int Index) { return this->Items[Index]; }
	
private:
	bool FOwnsObject;
	unsigned __fastcall GetItem(int Index);
	void __fastcall SetItem(int Index, const unsigned Item);
	System::TObject* __fastcall GetObject(int Index);
	void __fastcall PutObject(int Index, System::TObject* AObject);
	
protected:
	virtual void __fastcall Notify(void * Ptr, System::Classes::TListNotification Action);
	HIDESBASE void __fastcall InsertItem(int Index, const unsigned item, System::TObject* AObject);
	virtual int __fastcall CompareItems(const int Index1, const int Index2);
	
public:
	__fastcall TALCardinalList(void)/* overload */;
	__fastcall TALCardinalList(bool OwnsObjects)/* overload */;
	int __fastcall IndexOf(unsigned Item);
	int __fastcall IndexOfObject(System::TObject* AObject);
	int __fastcall Add(const unsigned Item);
	int __fastcall AddObject(const unsigned Item, System::TObject* AObject);
	bool __fastcall TryAdd(const unsigned Item);
	bool __fastcall TryAddObject(const unsigned Item, System::TObject* AObject);
	bool __fastcall Find(unsigned item, int &Index);
	HIDESBASE void __fastcall Insert(int Index, const unsigned item);
	void __fastcall InsertObject(int Index, const unsigned item, System::TObject* AObject);
	__property unsigned Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
	unsigned __fastcall Push(unsigned Item);
	unsigned __fastcall Pop(void);
	unsigned __fastcall Peek(void);
	System::DynamicArray<unsigned> __fastcall ToArray(void);
public:
	/* TALBaseQuickSortList.Destroy */ inline __fastcall virtual ~TALCardinalList(void) { }
	
};

#pragma pack(pop)

typedef TALInt64ListItem *PALInt64ListItem;

struct DECLSPEC_DRECORD TALInt64ListItem
{
public:
	__int64 FInt64;
	System::TObject* FObject;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TALInt64List : public TALBaseQuickSortList
{
	typedef TALBaseQuickSortList inherited;
	
public:
	__int64 operator[](int Index) { return this->Items[Index]; }
	
private:
	bool FOwnsObject;
	__int64 __fastcall GetItem(int Index);
	void __fastcall SetItem(int Index, const __int64 Item);
	System::TObject* __fastcall GetObject(int Index);
	void __fastcall PutObject(int Index, System::TObject* AObject);
	
protected:
	virtual void __fastcall Notify(void * Ptr, System::Classes::TListNotification Action);
	HIDESBASE void __fastcall InsertItem(int Index, const __int64 item, System::TObject* AObject);
	virtual int __fastcall CompareItems(const int Index1, const int Index2);
	
public:
	__fastcall TALInt64List(void)/* overload */;
	__fastcall TALInt64List(bool OwnsObjects)/* overload */;
	int __fastcall IndexOf(__int64 Item);
	int __fastcall IndexOfObject(System::TObject* AObject);
	int __fastcall Add(const __int64 Item);
	int __fastcall AddObject(const __int64 Item, System::TObject* AObject);
	bool __fastcall TryAdd(const __int64 Item);
	bool __fastcall TryAddObject(const __int64 Item, System::TObject* AObject);
	bool __fastcall Find(__int64 item, int &Index);
	HIDESBASE void __fastcall Insert(int Index, const __int64 item);
	void __fastcall InsertObject(int Index, const __int64 item, System::TObject* AObject);
	__property __int64 Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
	__int64 __fastcall Push(__int64 Item);
	__int64 __fastcall Pop(void);
	__int64 __fastcall Peek(void);
	System::DynamicArray<__int64> __fastcall ToArray(void);
public:
	/* TALBaseQuickSortList.Destroy */ inline __fastcall virtual ~TALInt64List(void) { }
	
};

#pragma pack(pop)

typedef TALNativeIntListItem *PALNativeIntListItem;

struct DECLSPEC_DRECORD TALNativeIntListItem
{
public:
	NativeInt FNativeInt;
	System::TObject* FObject;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TALNativeIntList : public TALBaseQuickSortList
{
	typedef TALBaseQuickSortList inherited;
	
public:
	NativeInt operator[](int Index) { return this->Items[Index]; }
	
private:
	bool FOwnsObject;
	NativeInt __fastcall GetItem(int Index);
	void __fastcall SetItem(int Index, const NativeInt Item);
	System::TObject* __fastcall GetObject(int Index);
	void __fastcall PutObject(int Index, System::TObject* AObject);
	
protected:
	virtual void __fastcall Notify(void * Ptr, System::Classes::TListNotification Action);
	HIDESBASE void __fastcall InsertItem(int Index, const NativeInt item, System::TObject* AObject);
	virtual int __fastcall CompareItems(const int Index1, const int Index2);
	
public:
	__fastcall TALNativeIntList(void)/* overload */;
	__fastcall TALNativeIntList(bool OwnsObjects)/* overload */;
	int __fastcall IndexOf(NativeInt Item);
	int __fastcall IndexOfObject(System::TObject* AObject);
	int __fastcall Add(const NativeInt Item);
	int __fastcall AddObject(const NativeInt Item, System::TObject* AObject);
	bool __fastcall TryAdd(const NativeInt Item);
	bool __fastcall TryAddObject(const NativeInt Item, System::TObject* AObject);
	bool __fastcall Find(NativeInt item, int &Index);
	HIDESBASE void __fastcall Insert(int Index, const NativeInt item);
	void __fastcall InsertObject(int Index, const NativeInt item, System::TObject* AObject);
	__property NativeInt Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
	NativeInt __fastcall Push(NativeInt Item);
	NativeInt __fastcall Pop(void);
	NativeInt __fastcall Peek(void);
	System::DynamicArray<NativeInt> __fastcall ToArray(void);
public:
	/* TALBaseQuickSortList.Destroy */ inline __fastcall virtual ~TALNativeIntList(void) { }
	
};

#pragma pack(pop)

typedef TALDoubleListItem *PALDoubleListItem;

struct DECLSPEC_DRECORD TALDoubleListItem
{
public:
	double FDouble;
	System::TObject* FObject;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TALDoubleList : public TALBaseQuickSortList
{
	typedef TALBaseQuickSortList inherited;
	
public:
	double operator[](int Index) { return this->Items[Index]; }
	
private:
	bool FOwnsObject;
	double __fastcall GetItem(int Index);
	void __fastcall SetItem(int Index, const double Item);
	System::TObject* __fastcall GetObject(int Index);
	void __fastcall PutObject(int Index, System::TObject* AObject);
	
protected:
	virtual void __fastcall Notify(void * Ptr, System::Classes::TListNotification Action);
	HIDESBASE void __fastcall InsertItem(int Index, const double item, System::TObject* AObject);
	virtual int __fastcall CompareItems(const int Index1, const int Index2);
	
public:
	__fastcall TALDoubleList(void)/* overload */;
	__fastcall TALDoubleList(bool OwnsObjects)/* overload */;
	int __fastcall IndexOf(double Item);
	int __fastcall IndexOfObject(System::TObject* AObject);
	int __fastcall Add(const double Item);
	int __fastcall AddObject(const double Item, System::TObject* AObject);
	bool __fastcall TryAdd(const double Item);
	bool __fastcall TryAddObject(const double Item, System::TObject* AObject);
	bool __fastcall Find(double item, int &Index);
	HIDESBASE void __fastcall Insert(int Index, const double item);
	void __fastcall InsertObject(int Index, const double item, System::TObject* AObject);
	__property double Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
	double __fastcall Push(double Item);
	double __fastcall Pop(void);
	double __fastcall Peek(void);
	System::DynamicArray<double> __fastcall ToArray(void);
public:
	/* TALBaseQuickSortList.Destroy */ inline __fastcall virtual ~TALDoubleList(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _SALDuplicateItem;
#define Alquicksortlist_SALDuplicateItem System::LoadResourceString(&Alquicksortlist::_SALDuplicateItem)
extern DELPHI_PACKAGE System::ResourceString _SALListCapacityError;
#define Alquicksortlist_SALListCapacityError System::LoadResourceString(&Alquicksortlist::_SALListCapacityError)
extern DELPHI_PACKAGE System::ResourceString _SALListCountError;
#define Alquicksortlist_SALListCountError System::LoadResourceString(&Alquicksortlist::_SALListCountError)
extern DELPHI_PACKAGE System::ResourceString _SALListIndexError;
#define Alquicksortlist_SALListIndexError System::LoadResourceString(&Alquicksortlist::_SALListIndexError)
extern DELPHI_PACKAGE System::ResourceString _SALSortedListError;
#define Alquicksortlist_SALSortedListError System::LoadResourceString(&Alquicksortlist::_SALSortedListError)
}	/* namespace Alquicksortlist */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALQUICKSORTLIST)
using namespace Alquicksortlist;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlquicksortlistHPP
