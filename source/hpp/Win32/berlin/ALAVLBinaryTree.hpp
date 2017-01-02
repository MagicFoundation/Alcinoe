﻿// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALAVLBinaryTree.pas' rev: 31.00 (Windows)

#ifndef AlavlbinarytreeHPP
#define AlavlbinarytreeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alavlbinarytree
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALBaseAVLBinaryTreeNode;
class DELPHICLASS TALBaseAVLBinaryTree;
class DELPHICLASS TALIntegerKeyAVLBinaryTreeNode;
class DELPHICLASS TALIntegerKeyAVLBinaryTree;
class DELPHICLASS TALCardinalKeyAVLBinaryTreeNode;
class DELPHICLASS TALCardinalKeyAVLBinaryTree;
class DELPHICLASS TALInt64KeyAVLBinaryTreeNode;
class DELPHICLASS TALInt64KeyAVLBinaryTree;
class DELPHICLASS TALStringKeyAVLBinaryTreeNode;
class DELPHICLASS TALStringKeyAVLBinaryTree;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (*TALAVLBinaryTreeIterateFunc)(TALBaseAVLBinaryTree* aTree, TALBaseAVLBinaryTreeNode* aNode, void * aExtData, bool &aContinue);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALBaseAVLBinaryTreeNode : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	System::StaticArray<TALBaseAVLBinaryTreeNode*, 2> ChildNodes;
	System::Int8 Bal;
	virtual void __fastcall SaveToStream(System::Classes::TStream* Astream);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Astream);
	
public:
	__fastcall virtual TALBaseAVLBinaryTreeNode(void);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALBaseAVLBinaryTreeNode(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALBaseAVLBinaryTree : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TALBaseAVLBinaryTreeNode* FHead;
	int FNodeCount;
	
protected:
	virtual void __fastcall FreeNodeObj(TALBaseAVLBinaryTreeNode* aNode);
	virtual int __fastcall CompareNode(void * IdVal, TALBaseAVLBinaryTreeNode* ANode) = 0 /* overload */;
	virtual int __fastcall CompareNode(TALBaseAVLBinaryTreeNode* aNode1, TALBaseAVLBinaryTreeNode* ANode2) = 0 /* overload */;
	virtual TALBaseAVLBinaryTreeNode* __fastcall CreateNode(void) = 0 ;
	virtual void __fastcall InternalIterate(TALAVLBinaryTreeIterateFunc Action, bool Up, void * ExtData);
	virtual bool __fastcall InternalAddNode(TALBaseAVLBinaryTreeNode* aNode);
	virtual TALBaseAVLBinaryTreeNode* __fastcall InternalExtractNode(void * IdVal);
	virtual bool __fastcall InternalDeleteNode(void * IdVal);
	virtual void __fastcall InternalClear(void);
	virtual TALBaseAVLBinaryTreeNode* __fastcall InternalGetHead(void);
	virtual void __fastcall InternalSaveToStream(System::Classes::TStream* Astream);
	virtual void __fastcall InternalLoadFromStream(System::Classes::TStream* Astream);
	virtual TALBaseAVLBinaryTreeNode* __fastcall InternalFindNode(void * idVal);
	virtual TALBaseAVLBinaryTreeNode* __fastcall InternalFirst(void);
	virtual TALBaseAVLBinaryTreeNode* __fastcall InternalLast(void);
	virtual TALBaseAVLBinaryTreeNode* __fastcall InternalNext(TALBaseAVLBinaryTreeNode* aNode);
	virtual TALBaseAVLBinaryTreeNode* __fastcall InternalPrev(TALBaseAVLBinaryTreeNode* aNode);
	virtual int __fastcall InternalGetNodeCount(void);
	
public:
	__fastcall virtual TALBaseAVLBinaryTree(void);
	__fastcall virtual ~TALBaseAVLBinaryTree(void);
	virtual void __fastcall Iterate(TALAVLBinaryTreeIterateFunc Action, bool Up, void * ExtData);
	virtual bool __fastcall AddNode(TALBaseAVLBinaryTreeNode* aNode);
	virtual TALBaseAVLBinaryTreeNode* __fastcall ExtractNode(void * IdVal);
	virtual bool __fastcall DeleteNode(void * IdVal);
	virtual void __fastcall Clear(void);
	virtual TALBaseAVLBinaryTreeNode* __fastcall Head(void);
	virtual void __fastcall SaveToStream(System::Classes::TStream* Astream);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Astream);
	virtual void __fastcall SaveToFile(const System::AnsiString AFilename);
	virtual void __fastcall LoadFromFile(const System::AnsiString AFilename);
	virtual TALBaseAVLBinaryTreeNode* __fastcall FindNode(void * idVal);
	virtual TALBaseAVLBinaryTreeNode* __fastcall First(void);
	virtual TALBaseAVLBinaryTreeNode* __fastcall Last(void);
	virtual TALBaseAVLBinaryTreeNode* __fastcall Next(TALBaseAVLBinaryTreeNode* aNode);
	virtual TALBaseAVLBinaryTreeNode* __fastcall Prev(TALBaseAVLBinaryTreeNode* aNode);
	virtual int __fastcall NodeCount(void);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALIntegerKeyAVLBinaryTreeNode : public TALBaseAVLBinaryTreeNode
{
	typedef TALBaseAVLBinaryTreeNode inherited;
	
protected:
	virtual void __fastcall SaveToStream(System::Classes::TStream* Astream);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Astream);
	
public:
	int ID;
	__fastcall virtual TALIntegerKeyAVLBinaryTreeNode(void);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALIntegerKeyAVLBinaryTreeNode(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALIntegerKeyAVLBinaryTree : public TALBaseAVLBinaryTree
{
	typedef TALBaseAVLBinaryTree inherited;
	
protected:
	virtual int __fastcall CompareNode(void * IdVal, TALBaseAVLBinaryTreeNode* ANode)/* overload */;
	virtual int __fastcall CompareNode(TALBaseAVLBinaryTreeNode* aNode1, TALBaseAVLBinaryTreeNode* ANode2)/* overload */;
	virtual TALBaseAVLBinaryTreeNode* __fastcall CreateNode(void);
	
public:
	HIDESBASE virtual bool __fastcall AddNode(TALIntegerKeyAVLBinaryTreeNode* aNode);
	HIDESBASE virtual TALIntegerKeyAVLBinaryTreeNode* __fastcall ExtractNode(int IdVal);
	HIDESBASE virtual bool __fastcall DeleteNode(int idVal);
	HIDESBASE virtual TALIntegerKeyAVLBinaryTreeNode* __fastcall Head(void);
	HIDESBASE virtual TALIntegerKeyAVLBinaryTreeNode* __fastcall FindNode(int idVal);
	HIDESBASE virtual TALIntegerKeyAVLBinaryTreeNode* __fastcall First(void);
	HIDESBASE virtual TALIntegerKeyAVLBinaryTreeNode* __fastcall Last(void);
	HIDESBASE virtual TALIntegerKeyAVLBinaryTreeNode* __fastcall Next(TALIntegerKeyAVLBinaryTreeNode* aNode);
	HIDESBASE virtual TALIntegerKeyAVLBinaryTreeNode* __fastcall Prev(TALIntegerKeyAVLBinaryTreeNode* aNode);
public:
	/* TALBaseAVLBinaryTree.Create */ inline __fastcall virtual TALIntegerKeyAVLBinaryTree(void) : TALBaseAVLBinaryTree() { }
	/* TALBaseAVLBinaryTree.Destroy */ inline __fastcall virtual ~TALIntegerKeyAVLBinaryTree(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALCardinalKeyAVLBinaryTreeNode : public TALBaseAVLBinaryTreeNode
{
	typedef TALBaseAVLBinaryTreeNode inherited;
	
protected:
	virtual void __fastcall SaveToStream(System::Classes::TStream* Astream);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Astream);
	
public:
	unsigned ID;
	__fastcall virtual TALCardinalKeyAVLBinaryTreeNode(void);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALCardinalKeyAVLBinaryTreeNode(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALCardinalKeyAVLBinaryTree : public TALBaseAVLBinaryTree
{
	typedef TALBaseAVLBinaryTree inherited;
	
protected:
	virtual int __fastcall CompareNode(void * IdVal, TALBaseAVLBinaryTreeNode* ANode)/* overload */;
	virtual int __fastcall CompareNode(TALBaseAVLBinaryTreeNode* aNode1, TALBaseAVLBinaryTreeNode* ANode2)/* overload */;
	virtual TALBaseAVLBinaryTreeNode* __fastcall CreateNode(void);
	
public:
	HIDESBASE virtual bool __fastcall AddNode(TALCardinalKeyAVLBinaryTreeNode* aNode);
	HIDESBASE virtual TALCardinalKeyAVLBinaryTreeNode* __fastcall ExtractNode(unsigned IdVal);
	HIDESBASE virtual bool __fastcall DeleteNode(unsigned idVal);
	HIDESBASE virtual TALCardinalKeyAVLBinaryTreeNode* __fastcall Head(void);
	HIDESBASE virtual TALCardinalKeyAVLBinaryTreeNode* __fastcall FindNode(unsigned idVal);
	HIDESBASE virtual TALCardinalKeyAVLBinaryTreeNode* __fastcall First(void);
	HIDESBASE virtual TALCardinalKeyAVLBinaryTreeNode* __fastcall Last(void);
	HIDESBASE virtual TALCardinalKeyAVLBinaryTreeNode* __fastcall Next(TALCardinalKeyAVLBinaryTreeNode* aNode);
	HIDESBASE virtual TALCardinalKeyAVLBinaryTreeNode* __fastcall Prev(TALCardinalKeyAVLBinaryTreeNode* aNode);
public:
	/* TALBaseAVLBinaryTree.Create */ inline __fastcall virtual TALCardinalKeyAVLBinaryTree(void) : TALBaseAVLBinaryTree() { }
	/* TALBaseAVLBinaryTree.Destroy */ inline __fastcall virtual ~TALCardinalKeyAVLBinaryTree(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TALInt64KeyAVLBinaryTreeNode : public TALBaseAVLBinaryTreeNode
{
	typedef TALBaseAVLBinaryTreeNode inherited;
	
protected:
	virtual void __fastcall SaveToStream(System::Classes::TStream* Astream);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Astream);
	
public:
	__int64 ID;
	__fastcall virtual TALInt64KeyAVLBinaryTreeNode(void);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALInt64KeyAVLBinaryTreeNode(void) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TALInt64KeyAVLBinaryTree : public TALBaseAVLBinaryTree
{
	typedef TALBaseAVLBinaryTree inherited;
	
protected:
	virtual int __fastcall CompareNode(void * IdVal, TALBaseAVLBinaryTreeNode* ANode)/* overload */;
	virtual int __fastcall CompareNode(TALBaseAVLBinaryTreeNode* aNode1, TALBaseAVLBinaryTreeNode* ANode2)/* overload */;
	virtual TALBaseAVLBinaryTreeNode* __fastcall CreateNode(void);
	
public:
	HIDESBASE virtual bool __fastcall AddNode(TALInt64KeyAVLBinaryTreeNode* aNode);
	HIDESBASE virtual TALInt64KeyAVLBinaryTreeNode* __fastcall ExtractNode(__int64 IdVal);
	HIDESBASE virtual bool __fastcall DeleteNode(__int64 idVal);
	HIDESBASE virtual TALInt64KeyAVLBinaryTreeNode* __fastcall Head(void);
	HIDESBASE virtual TALInt64KeyAVLBinaryTreeNode* __fastcall FindNode(__int64 idVal);
	HIDESBASE virtual TALInt64KeyAVLBinaryTreeNode* __fastcall First(void);
	HIDESBASE virtual TALInt64KeyAVLBinaryTreeNode* __fastcall Last(void);
	HIDESBASE virtual TALInt64KeyAVLBinaryTreeNode* __fastcall Next(TALInt64KeyAVLBinaryTreeNode* aNode);
	HIDESBASE virtual TALInt64KeyAVLBinaryTreeNode* __fastcall Prev(TALInt64KeyAVLBinaryTreeNode* aNode);
public:
	/* TALBaseAVLBinaryTree.Create */ inline __fastcall virtual TALInt64KeyAVLBinaryTree(void) : TALBaseAVLBinaryTree() { }
	/* TALBaseAVLBinaryTree.Destroy */ inline __fastcall virtual ~TALInt64KeyAVLBinaryTree(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALStringKeyAVLBinaryTreeNode : public TALBaseAVLBinaryTreeNode
{
	typedef TALBaseAVLBinaryTreeNode inherited;
	
protected:
	virtual void __fastcall SaveToStream(System::Classes::TStream* Astream);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Astream);
	
public:
	System::AnsiString ID;
	__fastcall virtual TALStringKeyAVLBinaryTreeNode(void);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALStringKeyAVLBinaryTreeNode(void) { }
	
};

#pragma pack(pop)

typedef int __fastcall (__closure *TALStringKeyAVLBinaryTreeCompareKeyFunct)(const System::AnsiString aKey1, const System::AnsiString aKey2);

class PASCALIMPLEMENTATION TALStringKeyAVLBinaryTree : public TALBaseAVLBinaryTree
{
	typedef TALBaseAVLBinaryTree inherited;
	
private:
	bool FCaseSensitive;
	TALStringKeyAVLBinaryTreeCompareKeyFunct FcompareKeyFunct;
	void __fastcall SetcaseSensitive(const bool Value);
	
protected:
	int __fastcall CompareKeyCaseSensitive(const System::AnsiString aKey1, const System::AnsiString aKey2);
	int __fastcall CompareKeyCaseInSensitive(const System::AnsiString aKey1, const System::AnsiString aKey2);
	virtual int __fastcall CompareNode(void * IdVal, TALBaseAVLBinaryTreeNode* ANode)/* overload */;
	virtual int __fastcall CompareNode(TALBaseAVLBinaryTreeNode* aNode1, TALBaseAVLBinaryTreeNode* ANode2)/* overload */;
	virtual TALBaseAVLBinaryTreeNode* __fastcall CreateNode(void);
	
public:
	__fastcall virtual TALStringKeyAVLBinaryTree(void);
	HIDESBASE virtual bool __fastcall AddNode(TALStringKeyAVLBinaryTreeNode* aNode);
	HIDESBASE virtual TALStringKeyAVLBinaryTreeNode* __fastcall ExtractNode(const System::AnsiString IdVal);
	HIDESBASE virtual bool __fastcall DeleteNode(const System::AnsiString idVal);
	HIDESBASE virtual TALStringKeyAVLBinaryTreeNode* __fastcall Head(void);
	HIDESBASE virtual TALStringKeyAVLBinaryTreeNode* __fastcall FindNode(const System::AnsiString idVal);
	HIDESBASE virtual TALStringKeyAVLBinaryTreeNode* __fastcall First(void);
	HIDESBASE virtual TALStringKeyAVLBinaryTreeNode* __fastcall Last(void);
	HIDESBASE virtual TALStringKeyAVLBinaryTreeNode* __fastcall Next(TALStringKeyAVLBinaryTreeNode* aNode);
	HIDESBASE virtual TALStringKeyAVLBinaryTreeNode* __fastcall Prev(TALStringKeyAVLBinaryTreeNode* aNode);
	__property bool CaseSensitive = {read=FCaseSensitive, write=SetcaseSensitive, default=1};
public:
	/* TALBaseAVLBinaryTree.Destroy */ inline __fastcall virtual ~TALStringKeyAVLBinaryTree(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Alavlbinarytree */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALAVLBINARYTREE)
using namespace Alavlbinarytree;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlavlbinarytreeHPP
