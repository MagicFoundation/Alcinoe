// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALQuickSortList.pas' rev: 31.00 (Windows)

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
#include <System.RTLConsts.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Generics.Collections.hpp>
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
template<typename TKey, typename TValue> class DELPHICLASS TALDictionary__2;
template<typename TKey, typename TValue> class DELPHICLASS TALObjectDictionary__2;
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
	bool __fastcall Find(int item, int &Index);
	HIDESBASE void __fastcall Insert(int Index, const int item);
	void __fastcall InsertObject(int Index, const int item, System::TObject* AObject);
	__property int Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
	int __fastcall Push(int Item);
	int __fastcall Pop(void);
	int __fastcall Peek(void);
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
	bool __fastcall Find(unsigned item, int &Index);
	HIDESBASE void __fastcall Insert(int Index, const unsigned item);
	void __fastcall InsertObject(int Index, const unsigned item, System::TObject* AObject);
	__property unsigned Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
	unsigned __fastcall Push(unsigned Item);
	unsigned __fastcall Pop(void);
	unsigned __fastcall Peek(void);
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
	bool __fastcall Find(__int64 item, int &Index);
	HIDESBASE void __fastcall Insert(int Index, const __int64 item);
	void __fastcall InsertObject(int Index, const __int64 item, System::TObject* AObject);
	__property __int64 Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
	__int64 __fastcall Push(__int64 Item);
	__int64 __fastcall Pop(void);
	__int64 __fastcall Peek(void);
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
	bool __fastcall Find(NativeInt item, int &Index);
	HIDESBASE void __fastcall Insert(int Index, const NativeInt item);
	void __fastcall InsertObject(int Index, const NativeInt item, System::TObject* AObject);
	__property NativeInt Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
	NativeInt __fastcall Push(NativeInt Item);
	NativeInt __fastcall Pop(void);
	NativeInt __fastcall Peek(void);
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
	bool __fastcall Find(double item, int &Index);
	HIDESBASE void __fastcall Insert(int Index, const double item);
	void __fastcall InsertObject(int Index, const double item, System::TObject* AObject);
	__property double Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property bool OwnsObjects = {read=FOwnsObject, write=FOwnsObject, nodefault};
	double __fastcall Push(double Item);
	double __fastcall Pop(void);
	double __fastcall Peek(void);
public:
	/* TALBaseQuickSortList.Destroy */ inline __fastcall virtual ~TALDoubleList(void) { }
	
};

#pragma pack(pop)

// Template declaration generated by Delphi parameterized types is
// used only for accessing Delphi variables and fields.
// Don't instantiate with new type parameters in user code.
template<typename TKey, typename TValue> class PASCALIMPLEMENTATION TALDictionary__2 : public System::Generics::Collections::TEnumerable__1<System::Generics::Collections::TPair__2<TKey,TValue> >
{
	typedef System::Generics::Collections::TEnumerable__1<System::Generics::Collections::TPair__2<TKey,TValue> > inherited;
	
	
private:
	// Template declaration generated by Delphi parameterized types is
	// used only for accessing Delphi variables and fields.
	// Don't instantiate with new type parameters in user code.
	struct DECLSPEC_DRECORD TItem
	{
	public:
		int HashCode;
		TKey Key;
		TValue Value;
	};
	
	
	typedef System::DynamicArray<TItem> TItemArray;
	
	
public:
	class DELPHICLASS TPairEnumerator;
	#pragma pack(push,4)
	// Template declaration generated by Delphi parameterized types is
	// used only for accessing Delphi variables and fields.
	// Don't instantiate with new type parameters in user code.
	class PASCALIMPLEMENTATION TPairEnumerator : public System::Generics::Collections::TEnumerator__1<System::Generics::Collections::TPair__2<TKey,TValue> >
	{
		typedef System::Generics::Collections::TEnumerator__1<System::Generics::Collections::TPair__2<TKey,TValue> > inherited;
		
	private:
		TALDictionary__2<TKey,TValue>* FDictionary;
		int FIndex;
		System::Generics::Collections::TPair__2<TKey,TValue> __fastcall GetCurrent(void);
		
	protected:
		virtual System::Generics::Collections::TPair__2<TKey,TValue> __fastcall DoGetCurrent(void);
		virtual bool __fastcall DoMoveNext(void);
		
	public:
		__fastcall TPairEnumerator(TALDictionary__2<TKey,TValue>* const ADictionary);
		__property System::Generics::Collections::TPair__2<TKey,TValue> Current = {read=GetCurrent};
		HIDESBASE bool __fastcall MoveNext(void);
	public:
		/* TObject.Destroy */ inline __fastcall virtual ~TPairEnumerator(void) { }
		
	};
	
	#pragma pack(pop)
	
	class DELPHICLASS TKeyEnumerator;
	#pragma pack(push,4)
	// Template declaration generated by Delphi parameterized types is
	// used only for accessing Delphi variables and fields.
	// Don't instantiate with new type parameters in user code.
	class PASCALIMPLEMENTATION TKeyEnumerator : public System::Generics::Collections::TEnumerator__1<TKey>
	{
		typedef System::Generics::Collections::TEnumerator__1<TKey> inherited;
		
	private:
		TALDictionary__2<TKey,TValue>* FDictionary;
		int FIndex;
		TKey __fastcall GetCurrent(void);
		
	protected:
		virtual TKey __fastcall DoGetCurrent(void);
		virtual bool __fastcall DoMoveNext(void);
		
	public:
		__fastcall TKeyEnumerator(TALDictionary__2<TKey,TValue>* const ADictionary);
		__property TKey Current = {read=GetCurrent};
		HIDESBASE bool __fastcall MoveNext(void);
	public:
		/* TObject.Destroy */ inline __fastcall virtual ~TKeyEnumerator(void) { }
		
	};
	
	#pragma pack(pop)
	
	class DELPHICLASS TValueEnumerator;
	#pragma pack(push,4)
	// Template declaration generated by Delphi parameterized types is
	// used only for accessing Delphi variables and fields.
	// Don't instantiate with new type parameters in user code.
	class PASCALIMPLEMENTATION TValueEnumerator : public System::Generics::Collections::TEnumerator__1<TValue>
	{
		typedef System::Generics::Collections::TEnumerator__1<TValue> inherited;
		
	private:
		TALDictionary__2<TKey,TValue>* FDictionary;
		int FIndex;
		TValue __fastcall GetCurrent(void);
		
	protected:
		virtual TValue __fastcall DoGetCurrent(void);
		virtual bool __fastcall DoMoveNext(void);
		
	public:
		__fastcall TValueEnumerator(TALDictionary__2<TKey,TValue>* const ADictionary);
		__property TValue Current = {read=GetCurrent};
		HIDESBASE bool __fastcall MoveNext(void);
	public:
		/* TObject.Destroy */ inline __fastcall virtual ~TValueEnumerator(void) { }
		
	};
	
	#pragma pack(pop)
	
	class DELPHICLASS TValueCollection;
	#pragma pack(push,4)
	// Template declaration generated by Delphi parameterized types is
	// used only for accessing Delphi variables and fields.
	// Don't instantiate with new type parameters in user code.
	class PASCALIMPLEMENTATION TValueCollection : public System::Generics::Collections::TEnumerable__1<TValue>
	{
		typedef System::Generics::Collections::TEnumerable__1<TValue> inherited;
		
	private:
		TALDictionary__2<TKey,TValue>* FDictionary;
		HIDESBASE System::DynamicArray<TValue> __fastcall ToArrayImpl(int Count);
		int __fastcall GetCount(void);
		
	protected:
		virtual System::Generics::Collections::TEnumerator__1<TValue>* __fastcall DoGetEnumerator(void);
		
	public:
		__fastcall TValueCollection(TALDictionary__2<TKey,TValue>* const ADictionary);
		HIDESBASE TALDictionary__2<TKey,TValue>::TValueEnumerator* __fastcall GetEnumerator(void);
		virtual System::DynamicArray<TValue> __fastcall ToArray(void) _FINAL_ATTRIBUTE;
		__property int Count = {read=GetCount, nodefault};
	public:
		/* {System_Generics_Collections}TEnumerable<ALQuickSortList_TALDictionary<TKey,TValue>_TValue>.Destroy */ inline __fastcall virtual ~TValueCollection(void) { }
		
	};
	
	#pragma pack(pop)
	
	class DELPHICLASS TKeyCollection;
	#pragma pack(push,4)
	// Template declaration generated by Delphi parameterized types is
	// used only for accessing Delphi variables and fields.
	// Don't instantiate with new type parameters in user code.
	class PASCALIMPLEMENTATION TKeyCollection : public System::Generics::Collections::TEnumerable__1<TKey>
	{
		typedef System::Generics::Collections::TEnumerable__1<TKey> inherited;
		
	private:
		TALDictionary__2<TKey,TValue>* FDictionary;
		HIDESBASE System::DynamicArray<TKey> __fastcall ToArrayImpl(int Count);
		int __fastcall GetCount(void);
		
	protected:
		virtual System::Generics::Collections::TEnumerator__1<TKey>* __fastcall DoGetEnumerator(void);
		
	public:
		__fastcall TKeyCollection(TALDictionary__2<TKey,TValue>* const ADictionary);
		HIDESBASE TALDictionary__2<TKey,TValue>::TKeyEnumerator* __fastcall GetEnumerator(void);
		virtual System::DynamicArray<TKey> __fastcall ToArray(void) _FINAL_ATTRIBUTE;
		__property int Count = {read=GetCount, nodefault};
	public:
		/* {System_Generics_Collections}TEnumerable<ALQuickSortList_TALDictionary<TKey,TValue>_TKey>.Destroy */ inline __fastcall virtual ~TKeyCollection(void) { }
		
	};
	
	#pragma pack(pop)
	
	
public:
	TValue operator[](const TKey Key) { return this->Items[Key]; }
	
private:
	System::DynamicArray<TItem> FItems;
	int FCount;
	System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<TKey> > FComparer;
	int FGrowThreshold;
	HIDESBASE System::DynamicArray<System::Generics::Collections::TPair__2<TKey,TValue> > __fastcall ToArrayImpl(int Count);
	void __fastcall Rehash(int NewCapPow2);
	void __fastcall Grow(void);
	int __fastcall GetBucketIndex(const TKey Key, int HashCode);
	int __fastcall Hash(const TKey Key);
	TValue __fastcall GetItem(const TKey Key);
	void __fastcall SetItem(const TKey Key, const TValue Value);
	void __fastcall RehashAdd(int HashCode, const TKey Key, const TValue Value);
	void __fastcall DoAdd(int HashCode, int Index, const TKey Key, const TValue Value);
	void __fastcall DoSetValue(int Index, const TValue Value);
	TValue __fastcall DoRemove(const TKey Key, int HashCode, System::Generics::Collections::TCollectionNotification Notification);
	
protected:
	virtual System::Generics::Collections::TEnumerator__1<System::Generics::Collections::TPair__2<TKey,TValue> >* __fastcall DoGetEnumerator(void);
	virtual void __fastcall KeyNotify(const TKey Key, System::Generics::Collections::TCollectionNotification Action);
	virtual void __fastcall ValueNotify(const TValue Value, System::Generics::Collections::TCollectionNotification Action);
	
public:
	__fastcall TALDictionary__2(int ACapacity)/* overload */;
	__fastcall TALDictionary__2(const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<TKey> > AComparer)/* overload */;
	__fastcall TALDictionary__2(int ACapacity, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<TKey> > AComparer)/* overload */;
	__fastcall TALDictionary__2(System::Generics::Collections::TEnumerable__1<System::Generics::Collections::TPair__2<TKey,TValue> >* const Collection)/* overload */;
	__fastcall TALDictionary__2(System::Generics::Collections::TEnumerable__1<System::Generics::Collections::TPair__2<TKey,TValue> >* const Collection, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<TKey> > AComparer)/* overload */;
	__fastcall virtual ~TALDictionary__2(void);
	void __fastcall SetCapacity(int ACapacity);
	bool __fastcall TryAdd(const TKey Key, const TValue Value);
	void __fastcall Add(const TKey Key, const TValue Value);
	void __fastcall Remove(const TKey Key);
	System::Generics::Collections::TPair__2<TKey,TValue> __fastcall ExtractPair(const TKey Key);
	void __fastcall Clear(void);
	void __fastcall TrimExcess(void);
	bool __fastcall TryGetValue(const TKey Key, /* out */ TValue &Value);
	void __fastcall AddOrSetValue(const TKey Key, const TValue Value);
	bool __fastcall ContainsKey(const TKey Key);
	bool __fastcall ContainsValue(const TValue Value);
	virtual System::DynamicArray<System::Generics::Collections::TPair__2<TKey,TValue> > __fastcall ToArray(void) _FINAL_ATTRIBUTE;
	__property TValue Items[const TKey Key] = {read=GetItem, write=SetItem/*, default*/};
	__property int Count = {read=FCount, nodefault};
	
private:
	typedef void __fastcall (__closure *_dt_Alquicksortlist_1)(System::TObject* Sender, TKey Item, System::Generics::Collections::TCollectionNotification Action);
	_dt_Alquicksortlist_1 FOnKeyNotify;
	typedef void __fastcall (__closure *_dt_Alquicksortlist_2)(System::TObject* Sender, TValue Item, System::Generics::Collections::TCollectionNotification Action);
	_dt_Alquicksortlist_2 FOnValueNotify;
	TKeyCollection* FKeyCollection;
	TValueCollection* FValueCollection;
	TKeyCollection* __fastcall GetKeys(void);
	TValueCollection* __fastcall GetValues(void);
	
public:
	HIDESBASE TPairEnumerator* __fastcall GetEnumerator(void);
	__property TKeyCollection* Keys = {read=GetKeys};
	__property TValueCollection* Values = {read=GetValues};
	typedef void __fastcall (__closure *_dt_Alquicksortlist_3)(System::TObject* Sender, TKey Item, System::Generics::Collections::TCollectionNotification Action);
	__property _dt_Alquicksortlist_3 OnKeyNotify = {read=FOnKeyNotify, write=FOnKeyNotify};
	typedef void __fastcall (__closure *_dt_Alquicksortlist_4)(System::TObject* Sender, TValue Item, System::Generics::Collections::TCollectionNotification Action);
	__property _dt_Alquicksortlist_4 OnValueNotify = {read=FOnValueNotify, write=FOnValueNotify};
};


// Template declaration generated by Delphi parameterized types is
// used only for accessing Delphi variables and fields.
// Don't instantiate with new type parameters in user code.
template<typename TKey, typename TValue> class PASCALIMPLEMENTATION TALObjectDictionary__2 : public TALDictionary__2<TKey,TValue>
{
	typedef TALDictionary__2<TKey,TValue> inherited;
	
private:
	System::Generics::Collections::TDictionaryOwnerships FOwnerships;
	
protected:
	virtual void __fastcall KeyNotify(const TKey Key, System::Generics::Collections::TCollectionNotification Action);
	virtual void __fastcall ValueNotify(const TValue Value, System::Generics::Collections::TCollectionNotification Action);
	
public:
	__fastcall TALObjectDictionary__2(System::Generics::Collections::TDictionaryOwnerships Ownerships, int ACapacity)/* overload */;
	__fastcall TALObjectDictionary__2(System::Generics::Collections::TDictionaryOwnerships Ownerships, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<TKey> > AComparer)/* overload */;
	__fastcall TALObjectDictionary__2(System::Generics::Collections::TDictionaryOwnerships Ownerships, int ACapacity, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<TKey> > AComparer)/* overload */;
	__property System::Generics::Collections::TDictionaryOwnerships Ownerships = {read=FOwnerships, write=FOwnerships, nodefault};
public:
	/* {ALQuickSortList}TALDictionary<ALQuickSortList_TALObjectDictionary<TKey,TValue>_TKey,ALQuickSortList_TALObjectDictionary<TKey,TValue>_TValue>.Create */ inline __fastcall TALObjectDictionary__2(int ACapacity)/* overload */ : TALDictionary__2<TKey,TValue>(ACapacity) { }
	/* {ALQuickSortList}TALDictionary<ALQuickSortList_TALObjectDictionary<TKey,TValue>_TKey,ALQuickSortList_TALObjectDictionary<TKey,TValue>_TValue>.Create */ inline __fastcall TALObjectDictionary__2(const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<TKey> > AComparer)/* overload */ : TALDictionary__2<TKey,TValue>(AComparer) { }
	/* {ALQuickSortList}TALDictionary<ALQuickSortList_TALObjectDictionary<TKey,TValue>_TKey,ALQuickSortList_TALObjectDictionary<TKey,TValue>_TValue>.Create */ inline __fastcall TALObjectDictionary__2(int ACapacity, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<TKey> > AComparer)/* overload */ : TALDictionary__2<TKey,TValue>(ACapacity, AComparer) { }
	/* {ALQuickSortList}TALDictionary<ALQuickSortList_TALObjectDictionary<TKey,TValue>_TKey,ALQuickSortList_TALObjectDictionary<TKey,TValue>_TValue>.Create */ inline __fastcall TALObjectDictionary__2(System::Generics::Collections::TEnumerable__1<System::Generics::Collections::TPair__2<TKey,TValue> >* const Collection)/* overload */ : TALDictionary__2<TKey,TValue>(Collection) { }
	/* {ALQuickSortList}TALDictionary<ALQuickSortList_TALObjectDictionary<TKey,TValue>_TKey,ALQuickSortList_TALObjectDictionary<TKey,TValue>_TValue>.Create */ inline __fastcall TALObjectDictionary__2(System::Generics::Collections::TEnumerable__1<System::Generics::Collections::TPair__2<TKey,TValue> >* const Collection, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<TKey> > AComparer)/* overload */ : TALDictionary__2<TKey,TValue>(Collection, AComparer) { }
	/* {ALQuickSortList}TALDictionary<ALQuickSortList_TALObjectDictionary<TKey,TValue>_TKey,ALQuickSortList_TALObjectDictionary<TKey,TValue>_TValue>.Destroy */ inline __fastcall virtual ~TALObjectDictionary__2(void) { }
	
};


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
