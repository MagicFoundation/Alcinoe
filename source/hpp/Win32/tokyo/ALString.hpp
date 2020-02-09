// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALString.pas' rev: 32.00 (Windows)

#ifndef AlstringHPP
#define AlstringHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Contnrs.hpp>
#include <ALInit.hpp>
#include <ALStringList.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alstring
{
//-- forward type declarations -----------------------------------------------
struct TALFormatSettings;
class DELPHICLASS EALException;
class DELPHICLASS EALExceptionU;
class DELPHICLASS TALStringStream;
class DELPHICLASS EALMaskException;
class DELPHICLASS TALMask;
class DELPHICLASS TALPerlRegEx;
class DELPHICLASS TALPerlRegExList;
class DELPHICLASS ERegularExpressionError;
class DELPHICLASS TALBasePrecompiledTag;
class DELPHICLASS TALPrecompiledTag;
//-- type declarations -------------------------------------------------------
typedef TALFormatSettings *pALFormatSettings;

struct DECLSPEC_DRECORD TALFormatSettings
{
	
public:
	struct DECLSPEC_DRECORD TEraInfo
	{
	public:
		System::AnsiString EraName;
		int EraOffset;
		System::TDate EraStart;
		System::TDate EraEnd;
	};
	
	
	
private:
	typedef System::StaticArray<System::AnsiString, 12> _TALFormatSettings__1;
	
	typedef System::StaticArray<System::AnsiString, 12> _TALFormatSettings__2;
	
	typedef System::StaticArray<System::AnsiString, 7> _TALFormatSettings__3;
	
	typedef System::StaticArray<System::AnsiString, 7> _TALFormatSettings__4;
	
	typedef System::DynamicArray<TEraInfo> _TALFormatSettings__5;
	
	
public:
	System::AnsiString CurrencyString;
	System::Byte CurrencyFormat;
	System::Byte CurrencyDecimals;
	char DateSeparator;
	char TimeSeparator;
	char ListSeparator;
	System::AnsiString ShortDateFormat;
	System::AnsiString LongDateFormat;
	System::AnsiString TimeAMString;
	System::AnsiString TimePMString;
	System::AnsiString ShortTimeFormat;
	System::AnsiString LongTimeFormat;
	_TALFormatSettings__1 ShortMonthNames;
	_TALFormatSettings__2 LongMonthNames;
	_TALFormatSettings__3 ShortDayNames;
	_TALFormatSettings__4 LongDayNames;
	_TALFormatSettings__5 EraInfo;
	char ThousandSeparator;
	char DecimalSeparator;
	System::Word TwoDigitYearCenturyWindow;
	System::Byte NegCurrFormat;
	static TALFormatSettings __fastcall Create()/* overload */;
	static TALFormatSettings __fastcall Create(unsigned Locale)/* overload */;
	static TALFormatSettings __fastcall Create(const System::AnsiString LocaleName)/* overload */;
	int __fastcall GetEraYearOffset(const System::AnsiString Name);
};


typedef System::Sysutils::TFormatSettings *pALFormatSettingsU;

typedef System::Sysutils::TFormatSettings TALFormatSettingsU;

#pragma pack(push,4)
class PASCALIMPLEMENTATION EALException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	__fastcall EALException(const System::AnsiString Msg);
	__fastcall EALException(const System::AnsiString Msg, const System::TVarRec *Args, const int Args_High);
public:
	/* Exception.CreateRes */ inline __fastcall EALException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EALException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EALException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EALException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EALException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EALException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EALException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EALException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EALException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EALException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EALException(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EALExceptionU : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EALExceptionU(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EALExceptionU(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EALExceptionU(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EALExceptionU(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EALExceptionU(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EALExceptionU(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EALExceptionU(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EALExceptionU(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EALExceptionU(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EALExceptionU(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EALExceptionU(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EALExceptionU(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EALExceptionU(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALStringStream : public System::Classes::TStream
{
	typedef System::Classes::TStream inherited;
	
private:
	System::AnsiString FDataString;
	int FPosition;
	
protected:
	virtual void __fastcall SetSize(int NewSize)/* overload */;
	
public:
	__fastcall TALStringStream(const System::AnsiString AString);
	virtual int __fastcall Read(void *Buffer, int Count)/* overload */;
	System::AnsiString __fastcall ReadString(int Count);
	virtual int __fastcall Seek(int Offset, System::Word Origin)/* overload */;
	virtual int __fastcall Write(const void *Buffer, int Count)/* overload */;
	void __fastcall WriteString(const System::AnsiString AString);
	__property System::AnsiString DataString = {read=FDataString};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALStringStream(void) { }
	
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  SetSize(const __int64 NewSize){ System::Classes::TStream::SetSize(NewSize); }
	
public:
	inline int __fastcall  Read(System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Read(Buffer, Offset, Count); }
	inline int __fastcall  Read(System::DynamicArray<System::Byte> &Buffer, int Count){ return System::Classes::TStream::Read(Buffer, Count); }
	inline __int64 __fastcall  Seek(const __int64 Offset, System::Classes::TSeekOrigin Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	inline __int64 __fastcall  Seek _DEPRECATED_ATTRIBUTE0 (const __int64 Offset, System::Word Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Write(Buffer, Offset, Count); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Count){ return System::Classes::TStream::Write(Buffer, Count); }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EALMaskException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EALMaskException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EALMaskException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EALMaskException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EALMaskException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EALMaskException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EALMaskException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EALMaskException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EALMaskException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EALMaskException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EALMaskException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EALMaskException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EALMaskException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EALMaskException(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALMask : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::Set<char, _DELPHI_SET_CHAR(0), _DELPHI_SET_CHAR(255)> TALMaskSet;
	
	typedef TALMaskSet *PALMaskSet;
	
	enum DECLSPEC_DENUM TALMaskStates : unsigned char { msLiteral, msAny, msSet, msMBCSLiteral };
	
	#pragma pack(push,8)
	struct DECLSPEC_DRECORD TALMaskState
	{
	public:
		bool SkipTo;
		
	public:
		TALMask::TALMaskStates State;
		union
		{
			struct 
			{
				char LeadByte;
				char TrailByte;
			};
			struct 
			{
				bool Negate;
				TALMask::TALMaskSet *CharSet;
			};
			struct 
			{
				char Literal;
			};
			
		};
	};
	#pragma pack(pop)
	
	
	typedef System::DynamicArray<TALMaskState> _TALMask__1;
	
	
private:
	_TALMask__1 FMaskStates;
	
protected:
	int __fastcall InitMaskStates(const System::AnsiString Mask);
	void __fastcall DoneMaskStates(void);
	bool __fastcall MatchesMaskStates(const System::AnsiString Filename);
	
public:
	__fastcall TALMask(const System::AnsiString MaskValue);
	__fastcall virtual ~TALMask(void);
	bool __fastcall Matches(const System::AnsiString Filename);
};

#pragma pack(pop)

enum DECLSPEC_DENUM Alstring__6 : unsigned char { preCaseLess, preMultiLine, preSingleLine, preExtended, preAnchored, preUnGreedy, preNoAutoCapture };

typedef System::Set<Alstring__6, Alstring__6::preCaseLess, Alstring__6::preNoAutoCapture> TALPerlRegExOptions;

enum DECLSPEC_DENUM Alstring__7 : unsigned char { preNotBOL, preNotEOL, preNotEmpty };

typedef System::Set<Alstring__7, Alstring__7::preNotBOL, Alstring__7::preNotEmpty> TALPerlRegExState;

typedef void __fastcall (__closure *TALPerlRegExReplaceEvent)(System::TObject* Sender, System::AnsiString &ReplaceWith);

class PASCALIMPLEMENTATION TALPerlRegEx : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<System::AnsiString> _TALPerlRegEx__1;
	
	
private:
	bool FCompiled;
	bool FStudied;
	TALPerlRegExOptions FOptions;
	TALPerlRegExState FState;
	System::AnsiString FRegEx;
	System::AnsiString FReplacement;
	System::AnsiString FSubject;
	int FStart;
	int FStop;
	System::Classes::TNotifyEvent FOnMatch;
	TALPerlRegExReplaceEvent FOnReplace;
	System::AnsiString __fastcall GetMatchedText(void);
	int __fastcall GetMatchedLength(void);
	int __fastcall GetMatchedOffset(void);
	void __fastcall SetOptions(TALPerlRegExOptions Value);
	void __fastcall SetRegEx(const System::AnsiString Value);
	int __fastcall GetGroupCount(void);
	System::AnsiString __fastcall GetGroups(int Index);
	int __fastcall GetGroupLengths(int Index);
	int __fastcall GetGroupOffsets(int Index);
	void __fastcall SetSubject(const System::AnsiString Value);
	void __fastcall SetStart(const int Value);
	void __fastcall SetStop(const int Value);
	bool __fastcall GetFoundMatch(void);
	System::StaticArray<int, 301> Offsets;
	int OffsetCount;
	int FPCREOptions;
	void *FPattern;
	void *FHints;
	void *FCharTable;
	char *FSubjectPChar;
	bool FHasStoredGroups;
	_TALPerlRegEx__1 FStoredGroups;
	System::AnsiString __fastcall GetSubjectLeft(void);
	System::AnsiString __fastcall GetSubjectRight(void);
	
protected:
	void __fastcall CleanUp(void);
	void __fastcall ClearStoredGroups(void);
	
public:
	__fastcall TALPerlRegEx(void);
	__fastcall virtual ~TALPerlRegEx(void);
	__classmethod System::AnsiString __fastcall EscapeRegExChars(const System::AnsiString S);
	bool __fastcall Compile(const bool RaiseException = true);
	void __fastcall Study(void);
	bool __fastcall Match(void)/* overload */;
	bool __fastcall Match(const System::AnsiString aSubject, Alstringlist::TALStrings* aGroups)/* overload */;
	bool __fastcall MatchAgain(void);
	System::AnsiString __fastcall Replace(void);
	bool __fastcall ReplaceAll(void);
	System::AnsiString __fastcall ComputeReplacement(void);
	void __fastcall StoreGroups(void);
	int __fastcall NamedGroup(const System::AnsiString Name);
	void __fastcall Split(Alstringlist::TALStrings* Strings, int Limit);
	void __fastcall SplitCapture(Alstringlist::TALStrings* Strings, int Limit)/* overload */;
	void __fastcall SplitCapture(Alstringlist::TALStrings* Strings, int Limit, int Offset)/* overload */;
	__property bool Compiled = {read=FCompiled, nodefault};
	__property bool FoundMatch = {read=GetFoundMatch, nodefault};
	__property bool Studied = {read=FStudied, nodefault};
	__property System::AnsiString MatchedText = {read=GetMatchedText};
	__property int MatchedLength = {read=GetMatchedLength, nodefault};
	__property int MatchedOffset = {read=GetMatchedOffset, nodefault};
	__property int Start = {read=FStart, write=SetStart, nodefault};
	__property int Stop = {read=FStop, write=SetStop, nodefault};
	__property TALPerlRegExState State = {read=FState, write=FState, nodefault};
	__property int GroupCount = {read=GetGroupCount, nodefault};
	__property System::AnsiString Groups[int Index] = {read=GetGroups};
	__property int GroupLengths[int Index] = {read=GetGroupLengths};
	__property int GroupOffsets[int Index] = {read=GetGroupOffsets};
	__property System::AnsiString Subject = {read=FSubject, write=SetSubject};
	__property System::AnsiString SubjectLeft = {read=GetSubjectLeft};
	__property System::AnsiString SubjectRight = {read=GetSubjectRight};
	__property TALPerlRegExOptions Options = {read=FOptions, write=SetOptions, nodefault};
	__property System::AnsiString RegEx = {read=FRegEx, write=SetRegEx};
	__property System::AnsiString Replacement = {read=FReplacement, write=FReplacement};
	__property System::Classes::TNotifyEvent OnMatch = {read=FOnMatch, write=FOnMatch};
	__property TALPerlRegExReplaceEvent OnReplace = {read=FOnReplace, write=FOnReplace};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TALPerlRegExList : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TList* FList;
	System::AnsiString FSubject;
	TALPerlRegEx* FMatchedRegEx;
	int FStart;
	int FStop;
	TALPerlRegEx* __fastcall GetRegEx(int Index);
	void __fastcall SetRegEx(int Index, TALPerlRegEx* Value);
	void __fastcall SetSubject(const System::AnsiString Value);
	void __fastcall SetStart(const int Value);
	void __fastcall SetStop(const int Value);
	int __fastcall GetCount(void);
	
protected:
	void __fastcall UpdateRegEx(TALPerlRegEx* ARegEx);
	
public:
	__fastcall TALPerlRegExList(void);
	__fastcall virtual ~TALPerlRegExList(void);
	int __fastcall Add(TALPerlRegEx* ARegEx);
	void __fastcall Clear(void);
	void __fastcall Delete(int Index);
	int __fastcall IndexOf(TALPerlRegEx* ARegEx);
	void __fastcall Insert(int Index, TALPerlRegEx* ARegEx);
	bool __fastcall Match(void);
	bool __fastcall MatchAgain(void);
	__property TALPerlRegEx* RegEx[int Index] = {read=GetRegEx, write=SetRegEx};
	__property int Count = {read=GetCount, nodefault};
	__property System::AnsiString Subject = {read=FSubject, write=SetSubject};
	__property int Start = {read=FStart, write=SetStart, nodefault};
	__property int Stop = {read=FStop, write=SetStop, nodefault};
	__property TALPerlRegEx* MatchedRegEx = {read=FMatchedRegEx};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION ERegularExpressionError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ERegularExpressionError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ERegularExpressionError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ERegularExpressionError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ERegularExpressionError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ERegularExpressionError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ERegularExpressionError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ERegularExpressionError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ERegularExpressionError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ERegularExpressionError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ERegularExpressionError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ERegularExpressionError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ERegularExpressionError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ERegularExpressionError(void) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TALTagParamsClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALBasePrecompiledTag : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::AnsiString fTagString;
	
protected:
	virtual Alstringlist::TALStrings* __fastcall GetTagParams(void) = 0 ;
	
public:
	__property System::AnsiString TagString = {read=fTagString, write=fTagString};
	__property Alstringlist::TALStrings* TagParams = {read=GetTagParams};
public:
	/* TObject.Create */ inline __fastcall TALBasePrecompiledTag(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TALBasePrecompiledTag(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TALPrecompiledTag : public TALBasePrecompiledTag
{
	typedef TALBasePrecompiledTag inherited;
	
private:
	Alstringlist::TALStrings* fTagParams;
	
protected:
	virtual Alstringlist::TALStrings* __fastcall GetTagParams(void);
	
public:
	__fastcall TALPrecompiledTag(void);
	__fastcall virtual ~TALPrecompiledTag(void);
};

#pragma pack(pop)

typedef System::AnsiString __fastcall (*TALHandleTagfunct)(const System::AnsiString TagString, Alstringlist::TALStrings* TagParams, void * ExtData, bool &Handled);

typedef System::AnsiString __fastcall (*TALHandleTagExtendedfunct)(const System::AnsiString TagString, Alstringlist::TALStrings* TagParams, void * ExtData, bool &Handled, const System::AnsiString SourceString, int &TagPosition, int &TagLength);

typedef TALBasePrecompiledTag* __fastcall (*TALHandleTagPrecompileFunct)(const System::AnsiString TagString, Alstringlist::TALStrings* TagParams, void * ExtData, const System::AnsiString SourceString, int &TagPosition, int &TagLength);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _SALInvalidFormat;
#define Alstring_SALInvalidFormat System::LoadResourceString(&Alstring::_SALInvalidFormat)
extern DELPHI_PACKAGE System::ResourceString _SALArgumentMissing;
#define Alstring_SALArgumentMissing System::LoadResourceString(&Alstring::_SALArgumentMissing)
extern DELPHI_PACKAGE TALFormatSettings ALDefaultFormatSettings;
extern DELPHI_PACKAGE System::Sysutils::TFormatSettings ALDefaultFormatSettingsU;
static const System::Int8 cALPerlRegExMAXSUBEXPRESSIONS = System::Int8(0x63);
extern DELPHI_PACKAGE void __fastcall (*ALMove)(const void *Source, void *Dest, NativeInt Count);
extern DELPHI_PACKAGE int __fastcall (*ALPosEx)(const System::AnsiString SubStr, const System::AnsiString S, int Offset/* = 0x1*/);
extern DELPHI_PACKAGE System::AnsiString __fastcall (*AlUpperCase)(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall (*AlLowerCase)(const System::AnsiString S);
extern DELPHI_PACKAGE int __fastcall (*ALCompareStr)(const System::AnsiString S1, const System::AnsiString S2);
extern DELPHI_PACKAGE bool __fastcall (*ALSameStr)(const System::AnsiString S1, const System::AnsiString S2);
extern DELPHI_PACKAGE int __fastcall (*ALCompareText)(const System::AnsiString S1, const System::AnsiString S2);
extern DELPHI_PACKAGE bool __fastcall (*ALSameText)(const System::AnsiString S1, const System::AnsiString S2);
extern DELPHI_PACKAGE bool __fastcall (*ALMatchText)(const System::AnsiString AText, const System::AnsiString *AValues, const int AValues_High);
extern DELPHI_PACKAGE bool __fastcall (*ALMatchStr)(const System::AnsiString AText, const System::AnsiString *AValues, const int AValues_High);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*ALDateToStrU)(const System::TDateTime DateTime, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*ALTimeToStrU)(const System::TDateTime DateTime, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*ALFormatDateTimeU)(const System::UnicodeString Format, System::TDateTime DateTime, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE bool __fastcall (*ALTryStrToDateU)(const System::UnicodeString S, /* out */ System::TDateTime &Value, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::TDateTime __fastcall (*ALStrToDateU)(const System::UnicodeString S, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE bool __fastcall (*ALTryStrToTimeU)(const System::UnicodeString S, /* out */ System::TDateTime &Value, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::TDateTime __fastcall (*ALStrToTimeU)(const System::UnicodeString S, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE bool __fastcall (*ALTryStrToDateTimeU)(const System::UnicodeString S, /* out */ System::TDateTime &Value, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::TDateTime __fastcall (*ALStrToDateTimeU)(const System::UnicodeString S, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE bool __fastcall (*ALTryStrToIntU)(const System::UnicodeString S, /* out */ int &Value);
extern DELPHI_PACKAGE int __fastcall (*ALStrToIntU)(const System::UnicodeString S);
extern DELPHI_PACKAGE int __fastcall (*ALStrToIntDefU)(const System::UnicodeString S, int Default);
extern DELPHI_PACKAGE bool __fastcall (*ALTryStrToInt64U)(const System::UnicodeString S, /* out */ __int64 &Value);
extern DELPHI_PACKAGE __int64 __fastcall (*ALStrToInt64U)(const System::UnicodeString S);
extern DELPHI_PACKAGE __int64 __fastcall (*ALStrToInt64DefU)(const System::UnicodeString S, const __int64 Default);
extern DELPHI_PACKAGE unsigned __int64 __fastcall (*ALStrToUInt64U)(const System::UnicodeString S);
extern DELPHI_PACKAGE unsigned __int64 __fastcall (*ALStrToUInt64DefU)(const System::UnicodeString S, const unsigned __int64 Default);
extern DELPHI_PACKAGE bool __fastcall (*ALTryStrToUInt64U)(const System::UnicodeString S, /* out */ unsigned __int64 &Value);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*ALCurrToStrU)(System::Currency Value, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*ALFormatFloatU)(const System::UnicodeString Format, System::Extended Value, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*ALFormatCurrU)(const System::UnicodeString Format, System::Currency Value, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::Extended __fastcall (*ALStrToFloatU)(const System::UnicodeString S, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::Extended __fastcall (*ALStrToFloatDefU)(const System::UnicodeString S, const System::Extended Default, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::Currency __fastcall (*ALStrToCurrU)(const System::UnicodeString S, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::Currency __fastcall (*ALStrToCurrDefU)(const System::UnicodeString S, const System::Currency Default, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE bool __fastcall (*ALTryStrToCurrU)(const System::UnicodeString S, /* out */ System::Currency &Value, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE int __fastcall (*ALPosU)(const System::UnicodeString SubStr, const System::UnicodeString Str, int Offset/* = 0x1*/);
extern DELPHI_PACKAGE int __fastcall (*ALPosExU)(const System::UnicodeString SubStr, const System::UnicodeString S, int Offset/* = 0x1*/);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*AlUpperCaseU)(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*AlLowerCaseU)(const System::UnicodeString S);
extern DELPHI_PACKAGE System::WideChar __fastcall (*AlUpCaseU)(System::WideChar Ch);
extern DELPHI_PACKAGE int __fastcall (*ALCompareStrU)(const System::UnicodeString S1, const System::UnicodeString S2);
extern DELPHI_PACKAGE bool __fastcall (*ALSameStrU)(const System::UnicodeString S1, const System::UnicodeString S2);
extern DELPHI_PACKAGE int __fastcall (*ALCompareTextU)(const System::UnicodeString S1, const System::UnicodeString S2);
extern DELPHI_PACKAGE bool __fastcall (*ALSameTextU)(const System::UnicodeString S1, const System::UnicodeString S2);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*ALTrimU)(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*ALTrimLeftU)(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*ALTrimRightU)(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*ALDequotedStrU)(const System::UnicodeString S, System::WideChar AQuote);
extern DELPHI_PACKAGE int __fastcall (*ALLastDelimiterU)(const System::UnicodeString Delimiters, const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*ALStringReplaceU)(const System::UnicodeString S, const System::UnicodeString OldPattern, const System::UnicodeString NewPattern, System::Sysutils::TReplaceFlags Flags);
#define cAlUTF8Bom "ï»¿"
#define cAlUTF16LittleEndianBom "ÿþ"
#define cAlUTF16bigEndianBom "þÿ"
#define cAlUTF32LittleEndianBom "ÿþ\x00\x00"
#define cAlUTF32BigEndianBom "\x00\x00þÿ"
extern DELPHI_PACKAGE System::AnsiString __fastcall ALGetFormatSettingsID(const TALFormatSettings &aFormatSettings);
extern DELPHI_PACKAGE void __fastcall ALGetLocaleFormatSettings(unsigned Locale, TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALGUIDToByteString(const GUID &Guid);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALNewGUIDByteString(void);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALGUIDToString(const GUID &Guid, const bool WithoutBracket = false, const bool WithoutHyphen = false);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALNewGUIDString(const bool WithoutBracket = false, const bool WithoutHyphen = false);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall ALNewGUIDBytes(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALGUIDToStringU(const GUID &Guid, const bool WithoutBracket = false, const bool WithoutHyphen = false);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALNewGUIDStringU(const bool WithoutBracket = false, const bool WithoutHyphen = false);
extern DELPHI_PACKAGE bool __fastcall ALMatchesMask(const System::AnsiString Filename, const System::AnsiString Mask);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALIfThen(bool AValue, const System::AnsiString ATrue, System::AnsiString AFalse = System::AnsiString())/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALIfThenU(bool AValue, const System::UnicodeString ATrue, System::UnicodeString AFalse = System::UnicodeString())/* overload */;
extern DELPHI_PACKAGE int __fastcall ALIfThen(bool AValue, const int ATrue, const int AFalse)/* overload */;
extern DELPHI_PACKAGE __int64 __fastcall ALIfThen(bool AValue, const __int64 ATrue, const __int64 AFalse)/* overload */;
extern DELPHI_PACKAGE unsigned __int64 __fastcall ALIfThen(bool AValue, const unsigned __int64 ATrue, const unsigned __int64 AFalse)/* overload */;
extern DELPHI_PACKAGE float __fastcall ALIfThen(bool AValue, const float ATrue, const float AFalse)/* overload */;
extern DELPHI_PACKAGE double __fastcall ALIfThen(bool AValue, const double ATrue, const double AFalse)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall ALIfThen(bool AValue, const System::Extended ATrue, const System::Extended AFalse)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALFormat(const System::AnsiString Format, const System::TVarRec *Args, const int Args_High)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALFormat(const System::AnsiString Format, const System::TVarRec *Args, const int Args_High, System::AnsiString &Result)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALFormat(const System::AnsiString Format, const System::TVarRec *Args, const int Args_High, const TALFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALFormat(const System::AnsiString Format, const System::TVarRec *Args, const int Args_High, const TALFormatSettings &AFormatSettings, System::AnsiString &Result)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALFormatU(const System::UnicodeString Format, const System::TVarRec *Args, const int Args_High)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALFormatU(const System::UnicodeString Format, const System::TVarRec *Args, const int Args_High, System::UnicodeString &Result)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALFormatU(const System::UnicodeString Format, const System::TVarRec *Args, const int Args_High, const System::Sysutils::TFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALFormatU(const System::UnicodeString Format, const System::TVarRec *Args, const int Args_High, const System::Sysutils::TFormatSettings &AFormatSettings, System::UnicodeString &Result)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALTryStrToBool(const System::AnsiString S, /* out */ bool &Value);
extern DELPHI_PACKAGE bool __fastcall ALTryStrToBoolU(const System::UnicodeString S, /* out */ bool &Value);
extern DELPHI_PACKAGE bool __fastcall AlStrToBool(System::AnsiString Value);
extern DELPHI_PACKAGE bool __fastcall AlStrToBoolU(System::UnicodeString Value);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBoolToStr(bool B, const System::AnsiString trueStr = "1", const System::AnsiString falseStr = "0")/* overload */;
extern DELPHI_PACKAGE void __fastcall ALBoolToStr(System::AnsiString &s, bool B, const System::AnsiString trueStr = "1", const System::AnsiString falseStr = "0")/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALBoolToStrU(bool B, const System::UnicodeString trueStr = L"1", const System::UnicodeString falseStr = L"0")/* overload */;
extern DELPHI_PACKAGE void __fastcall ALBoolToStrU(System::UnicodeString &s, bool B, const System::UnicodeString trueStr = L"1", const System::UnicodeString falseStr = L"0")/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALDateToStr(const System::TDateTime DateTime, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALTimeToStr(const System::TDateTime DateTime, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALDateTimeToStr(const System::TDateTime DateTime, const TALFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALDateTimeToStr(const System::TDateTime DateTime, System::AnsiString &s, const TALFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALDateTimeToStrU(const System::TDateTime DateTime, const System::Sysutils::TFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALDateTimeToStrU(const System::TDateTime DateTime, System::UnicodeString &s, const System::Sysutils::TFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALFormatDateTime(const System::AnsiString Format, System::TDateTime DateTime, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE bool __fastcall ALTryStrToDate(const System::AnsiString S, /* out */ System::TDateTime &Value, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::TDateTime __fastcall ALStrToDate(const System::AnsiString S, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE bool __fastcall ALTryStrToTime(const System::AnsiString S, /* out */ System::TDateTime &Value, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::TDateTime __fastcall ALStrToTime(const System::AnsiString S, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE bool __fastcall ALTryStrToDateTime(const System::AnsiString S, /* out */ System::TDateTime &Value, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::TDateTime __fastcall ALStrToDateTime(const System::AnsiString S, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE bool __fastcall ALTryStrToInt(const System::AnsiString S, /* out */ int &Value);
extern DELPHI_PACKAGE int __fastcall ALStrToInt(const System::AnsiString S);
extern DELPHI_PACKAGE int __fastcall ALStrToIntDef(const System::AnsiString S, int Default);
extern DELPHI_PACKAGE bool __fastcall ALTryStrToUInt(const System::AnsiString S, /* out */ unsigned &Value);
extern DELPHI_PACKAGE unsigned __fastcall ALStrToUInt(const System::AnsiString S);
extern DELPHI_PACKAGE unsigned __fastcall ALStrToUIntDef(const System::AnsiString S, unsigned Default);
extern DELPHI_PACKAGE bool __fastcall ALTryStrToInt64(const System::AnsiString S, /* out */ __int64 &Value);
extern DELPHI_PACKAGE __int64 __fastcall ALStrToInt64(const System::AnsiString S);
extern DELPHI_PACKAGE __int64 __fastcall ALStrToInt64Def(const System::AnsiString S, const __int64 Default);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALIntToStr(int Value)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALIntToStr(int Value, System::AnsiString &s)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALIntToStr(__int64 Value)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALIntToStr(__int64 Value, System::AnsiString &s)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALIntToStrU(int Value, System::UnicodeString &s)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALIntToStrU(__int64 Value)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALIntToStrU(__int64 Value, System::UnicodeString &s)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALIntToStrU(int Value)/* overload */;
extern DELPHI_PACKAGE unsigned __int64 __fastcall ALStrToUInt64(const System::AnsiString S);
extern DELPHI_PACKAGE unsigned __int64 __fastcall ALStrToUInt64Def(const System::AnsiString S, const unsigned __int64 Default);
extern DELPHI_PACKAGE bool __fastcall ALTryStrToUInt64(const System::AnsiString S, /* out */ unsigned __int64 &Value);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUIntToStr(unsigned Value)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUIntToStr(unsigned __int64 Value)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALUIntToStrU(unsigned Value)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALUIntToStrU(unsigned __int64 Value)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALIntToHex(int Value, int Digits)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALIntToHex(__int64 Value, int Digits)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALIntToHex(unsigned __int64 Value, int Digits)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALTryBinToHex(const System::AnsiString aBin, /* out */ System::AnsiString &Value)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBinToHex(const System::AnsiString aBin)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALTryBinToHex(const void *aBin, unsigned aBinSize, /* out */ System::AnsiString &Value)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBinToHex(const void *aBin, unsigned aBinSize)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALTryHexToBin(const System::AnsiString aHex, /* out */ System::AnsiString &Value);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALHexToBin(const System::AnsiString aHex);
extern DELPHI_PACKAGE bool __fastcall ALTryBinToHexU(const System::DynamicArray<System::Byte> aBin, /* out */ System::UnicodeString &Value)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALBinToHexU(const System::DynamicArray<System::Byte> aBin)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALTryBinToHexU(const void *aBin, unsigned aBinSize, /* out */ System::UnicodeString &Value)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALBinToHexU(const void *aBin, unsigned aBinSize)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALTryHexToBinU(const System::UnicodeString aHex, /* out */ System::DynamicArray<System::Byte> &Value);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall ALHexToBinU(const System::UnicodeString aHex);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALIntToBit(int value, int digits);
extern DELPHI_PACKAGE int __fastcall AlBitToInt(System::AnsiString Value);
extern DELPHI_PACKAGE System::AnsiString __fastcall AlInt2BaseN(unsigned __int64 NumIn, const char *charset, const int charset_High);
extern DELPHI_PACKAGE unsigned __int64 __fastcall AlBaseN2Int(const System::AnsiString Str, const char *charset, const int charset_High);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBase64EncodeString(const char * P, const int ln)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBase64EncodeString(const System::AnsiString S)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBase64DecodeString(const char * P, const int ln)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBase64DecodeString(const System::AnsiString S)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBase64EncodeStringMIME(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBase64DecodeStringMIME(const System::AnsiString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALBase64EncodeStringU(const System::UnicodeString S, System::Sysutils::TEncoding* const AEncoding = (System::Sysutils::TEncoding*)(0x0));
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALBase64DecodeStringU(const System::UnicodeString S, System::Sysutils::TEncoding* const AEncoding = (System::Sysutils::TEncoding*)(0x0));
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALBase64EncodeBytesU(const System::DynamicArray<System::Byte> Bytes)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALBase64EncodeBytesU(const void * Bytes, const int Size)/* overload */;
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall ALBase64DecodeBytesU(const System::UnicodeString S);
extern DELPHI_PACKAGE bool __fastcall ALIsDecimal(const System::AnsiString S, const bool RejectPlusMinusSign = false);
extern DELPHI_PACKAGE bool __fastcall ALIsInteger(const System::AnsiString S);
extern DELPHI_PACKAGE bool __fastcall ALIsInt64(const System::AnsiString S);
extern DELPHI_PACKAGE bool __fastcall ALIsSmallInt(const System::AnsiString S);
extern DELPHI_PACKAGE bool __fastcall ALIsFloat(const System::AnsiString S, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALFloatToStr(System::Extended Value, const TALFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALFloatToStr(System::Extended Value, System::AnsiString &S, const TALFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALFloatToStrF(System::Extended Value, System::Sysutils::TFloatFormat Format, int Precision, int Digits, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE bool __fastcall ALIsDecimalU(const System::UnicodeString S, const bool RejectPlusMinusSign = false);
extern DELPHI_PACKAGE bool __fastcall ALIsIntegerU(const System::UnicodeString S);
extern DELPHI_PACKAGE bool __fastcall ALIsInt64U(const System::UnicodeString S);
extern DELPHI_PACKAGE bool __fastcall ALIsSmallIntU(const System::UnicodeString S);
extern DELPHI_PACKAGE bool __fastcall ALIsFloatU(const System::UnicodeString S, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALFloatToStrU(System::Extended Value, const System::Sysutils::TFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALFloatToStrU(System::Extended Value, System::UnicodeString &S, const System::Sysutils::TFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALCurrToStr(System::Currency Value, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALFormatFloat(const System::AnsiString Format, System::Extended Value, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALFormatCurr(const System::AnsiString Format, System::Currency Value, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::Extended __fastcall ALStrToFloat(const System::AnsiString S, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::Extended __fastcall ALStrToFloatDef(const System::AnsiString S, const System::Extended Default, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE bool __fastcall ALTryStrToFloat(const System::AnsiString S, /* out */ System::Extended &Value, const TALFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALTryStrToFloat(const System::AnsiString S, /* out */ double &Value, const TALFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALTryStrToFloat(const System::AnsiString S, /* out */ float &Value, const TALFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALTryStrToFloatU(const System::UnicodeString S, /* out */ float &Value, const System::Sysutils::TFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALTryStrToFloatU(const System::UnicodeString S, /* out */ double &Value, const System::Sysutils::TFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALTryStrToFloatU(const System::UnicodeString S, /* out */ System::Extended &Value, const System::Sysutils::TFormatSettings &AFormatSettings)/* overload */;
extern DELPHI_PACKAGE System::Currency __fastcall ALStrToCurr(const System::AnsiString S, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE System::Currency __fastcall ALStrToCurrDef(const System::AnsiString S, const System::Currency Default, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE bool __fastcall ALTryStrToCurr(const System::AnsiString S, /* out */ System::Currency &Value, const TALFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE int __fastcall ALPos(const System::AnsiString SubStr, const System::AnsiString Str, int Offset = 0x1);
extern DELPHI_PACKAGE int __fastcall ALPosExIgnoreCase(const System::AnsiString SubStr, const System::AnsiString S, int Offset = 0x1);
extern DELPHI_PACKAGE int __fastcall ALPosExIgnoreCaseU(const System::UnicodeString SubStr, const System::UnicodeString S, int Offset = 0x1);
extern DELPHI_PACKAGE char __fastcall AlUpCase(const char Ch);
extern DELPHI_PACKAGE char __fastcall AlLoCase(const char Ch);
extern DELPHI_PACKAGE System::WideChar __fastcall AlLoCaseU(System::WideChar Ch);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALTrim(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALTrimLeft(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALTrimRight(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALPadLeft(const System::AnsiString S, const int Width);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALPadRight(const System::AnsiString S, const int Width);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALQuotedStr(const System::AnsiString S, const char Quote = '\x27');
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALQuotedStrU(const System::UnicodeString S, const System::WideChar Quote = (System::WideChar)(0x27));
extern DELPHI_PACKAGE System::AnsiString __fastcall ALExtractQuotedStr(char * &Src, char Quote);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALDequotedStr(const System::AnsiString S, char AQuote);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALExtractQuotedStrU(System::WideChar * &Src, System::WideChar Quote);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALExtractFilePath(const System::AnsiString FileName);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALExtractFileDir(const System::AnsiString FileName);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALExtractFileDrive(const System::AnsiString FileName);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALExtractFileName(const System::AnsiString FileName);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALExtractFileExt(const System::AnsiString FileName);
extern DELPHI_PACKAGE int __fastcall ALLastDelimiter(const System::AnsiString Delimiters, const System::AnsiString S);
extern DELPHI_PACKAGE bool __fastcall ALIsPathDelimiter(const System::AnsiString S, int Index, const System::AnsiString PathDelimiter = "\\");
extern DELPHI_PACKAGE System::AnsiString __fastcall ALIncludeTrailingPathDelimiter(const System::AnsiString S, const System::AnsiString PathDelimiter = "\\");
extern DELPHI_PACKAGE System::AnsiString __fastcall ALExcludeTrailingPathDelimiter(const System::AnsiString S, const System::AnsiString PathDelimiter = "\\");
extern DELPHI_PACKAGE System::AnsiString __fastcall ALIncludeLeadingPathDelimiter(const System::AnsiString S, const System::AnsiString PathDelimiter = "\\");
extern DELPHI_PACKAGE System::AnsiString __fastcall ALExcludeLeadingPathDelimiter(const System::AnsiString S, const System::AnsiString PathDelimiter = "\\");
extern DELPHI_PACKAGE bool __fastcall ALIsPathDelimiterU(const System::UnicodeString S, int Index, const System::UnicodeString PathDelimiter = L"\\");
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALIncludeTrailingPathDelimiterU(const System::UnicodeString S, const System::UnicodeString PathDelimiter = L"\\");
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALExcludeTrailingPathDelimiterU(const System::UnicodeString S, const System::UnicodeString PathDelimiter = L"\\");
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALIncludeLeadingPathDelimiterU(const System::UnicodeString S, const System::UnicodeString PathDelimiter = L"\\");
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALExcludeLeadingPathDelimiterU(const System::UnicodeString S, const System::UnicodeString PathDelimiter = L"\\");
extern DELPHI_PACKAGE System::AnsiString __fastcall ALStringReplace(const System::AnsiString S, const System::AnsiString OldPattern, const System::AnsiString NewPattern, System::Sysutils::TReplaceFlags Flags);
extern DELPHI_PACKAGE void __fastcall ALStrMove(const char * Source, char * &Dest, NativeInt Count);
extern DELPHI_PACKAGE void __fastcall ALStrMoveU(const System::WideChar * Source, System::WideChar * &Dest, NativeInt Count);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALCopyStr(const System::AnsiString aSourceString, int aStart, int aLength)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALCopyStr(const System::AnsiString aSourceString, System::AnsiString &aDestString, int aStart, int aLength)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALCopyStr(const System::AnsiString aSourceString, const System::AnsiString aStartStr, const System::AnsiString aEndStr, const int aOffset = 0x1, const bool aRaiseExceptionIfNotFound = true)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALCopyStrU(const System::UnicodeString aSourceString, int aStart, int aLength)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALCopyStrU(const System::UnicodeString aSourceString, System::UnicodeString &aDestString, int aStart, int aLength)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALCopyStrU(const System::UnicodeString aSourceString, const System::UnicodeString aStartStr, const System::UnicodeString aEndStr, const int aOffset = 0x1, const bool aRaiseExceptionIfNotFound = true)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRandomStr(const int aLength, const char *aCharset, const int aCharset_High)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRandomStr(const int aLength)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALRandomStrU(const int aLength, const System::WideChar *aCharset, const int aCharset_High)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALRandomStrU(const int aLength)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALNEVExtractName(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALNEVExtractValue(const System::AnsiString s);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALFastTagReplacePrecompile(const System::AnsiString SourceString, const System::AnsiString TagStart, const System::AnsiString TagEnd, TALHandleTagPrecompileFunct PrecompileProc, bool StripParamQuotes, void * ExtData, System::Contnrs::TObjectList* TagsContainer, const System::Sysutils::TReplaceFlags flags = System::Sysutils::TReplaceFlags() );
extern DELPHI_PACKAGE System::AnsiString __fastcall ALFastTagReplace(const System::AnsiString SourceString, const System::AnsiString TagStart, const System::AnsiString TagEnd, TALHandleTagfunct ReplaceProc, TALHandleTagExtendedfunct ReplaceExtendedProc, bool StripParamQuotes, System::Sysutils::TReplaceFlags Flags, void * ExtData, TALTagParamsClass TagParamsClass, const bool TagReplaceProcResult = false)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALFastTagReplace(const System::AnsiString SourceString, const System::AnsiString TagStart, const System::AnsiString TagEnd, TALHandleTagfunct ReplaceProc, bool StripParamQuotes, void * ExtData, const System::Sysutils::TReplaceFlags flags = (System::Sysutils::TReplaceFlags() << System::Sysutils::System_Sysutils__85::rfReplaceAll ), const bool TagReplaceProcResult = false)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALFastTagReplace(const System::AnsiString SourceString, const System::AnsiString TagStart, const System::AnsiString TagEnd, TALHandleTagExtendedfunct ReplaceExtendedProc, bool StripParamQuotes, void * ExtData, const System::Sysutils::TReplaceFlags flags = (System::Sysutils::TReplaceFlags() << System::Sysutils::System_Sysutils__85::rfReplaceAll ), const bool TagReplaceProcResult = false)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALFastTagReplace(const System::AnsiString SourceString, const System::AnsiString TagStart, const System::AnsiString TagEnd, const System::AnsiString ReplaceWith, const System::Sysutils::TReplaceFlags Flags = (System::Sysutils::TReplaceFlags() << System::Sysutils::System_Sysutils__85::rfReplaceAll ))/* overload */;
extern DELPHI_PACKAGE bool __fastcall ALExtractTagParams(const System::AnsiString SourceString, const System::AnsiString TagStart, const System::AnsiString TagEnd, bool StripParamQuotes, Alstringlist::TALStrings* TagParams, bool IgnoreCase);
extern DELPHI_PACKAGE void __fastcall ALSplitTextAndTag(const System::AnsiString SourceString, const System::AnsiString TagStart, const System::AnsiString TagEnd, Alstringlist::TALStrings* SplitTextAndTagLst, bool IgnoreCase);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall ALGetBytesFromFile(const System::AnsiString filename, const System::Word ShareMode = (System::Word)(0x20));
extern DELPHI_PACKAGE System::AnsiString __fastcall ALGetStringFromFile(const System::AnsiString filename, const System::Word ShareMode = (System::Word)(0x20));
extern DELPHI_PACKAGE System::AnsiString __fastcall ALGetStringFromFileWithoutUTF8BOM(const System::AnsiString filename, const System::Word ShareMode = (System::Word)(0x20));
extern DELPHI_PACKAGE void __fastcall ALAppendStringToFile(const System::AnsiString Str, const System::AnsiString FileName);
extern DELPHI_PACKAGE void __fastcall ALSaveStringtoFile(const System::AnsiString Str, const System::AnsiString filename);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall ALGetBytesFromStream(System::Classes::TStream* const aStream);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall ALGetBytesFromFileU(const System::UnicodeString filename, const System::Word ShareMode = (System::Word)(0x20));
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALGetStringFromBufferU(const System::DynamicArray<System::Byte> buf, System::Sysutils::TEncoding* const ADefaultEncoding);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALGetStringFromStreamU(System::Classes::TStream* const aStream, System::Sysutils::TEncoding* const ADefaultEncoding);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALGetStringFromFileU(const System::UnicodeString filename, System::Sysutils::TEncoding* const ADefaultEncoding, const System::Word ShareMode = (System::Word)(0x20));
extern DELPHI_PACKAGE void __fastcall ALSaveStringtoFileU(const System::UnicodeString Str, const System::UnicodeString filename, System::Sysutils::TEncoding* AEncoding, const bool WriteBOM = false);
extern DELPHI_PACKAGE System::WideString __fastcall ALWideNormalize(const System::WideString S, const System::WideChar WordSeparator, const System::WideChar *SymbolsToIgnore, const int SymbolsToIgnore_High)/* overload */;
extern DELPHI_PACKAGE System::WideString __fastcall ALWideNormalize(const System::WideString S, const System::WideChar WordSeparator = (System::WideChar)(0x2d))/* overload */;
extern DELPHI_PACKAGE System::WideString __fastcall ALWideRemoveDiacritic(const System::WideString S);
extern DELPHI_PACKAGE System::WideString __fastcall ALWideExpandLigatures(const System::WideString S);
extern DELPHI_PACKAGE System::WideString __fastcall ALWideUpperCaseNoDiacritic(const System::WideString S);
extern DELPHI_PACKAGE System::WideString __fastcall ALWideLowerCaseNoDiacritic(const System::WideString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8RemoveDiacritic(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8ExpandLigatures(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8UpperCaseNoDiacritic(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8LowerCaseNoDiacritic(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8Normalize(const System::AnsiString S, const char WordSeparator, const char *SymbolsToIgnore, const int SymbolsToIgnore_High)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8Normalize(const System::AnsiString S, const char WordSeparator = '\x2d')/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8UpperCase(const System::AnsiString s);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8LowerCase(const System::AnsiString s);
extern DELPHI_PACKAGE bool __fastcall AlUTF8Check(const System::AnsiString S);
extern DELPHI_PACKAGE bool __fastcall AlUTF8DetectBOM(const char * P, const int Size);
extern DELPHI_PACKAGE System::AnsiString __fastcall AlUTF8removeBOM(const System::AnsiString S);
extern DELPHI_PACKAGE int __fastcall ALUTF8CharSize(char Lead);
extern DELPHI_PACKAGE int __fastcall ALUTF8CharCount(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8ByteTrunc(const System::AnsiString s, const int Count);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8CharTrunc(const System::AnsiString s, const int Count);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8UpperFirstChar(const System::AnsiString s);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8TitleCase(const System::AnsiString s);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8SentenceCase(const System::AnsiString s);
extern DELPHI_PACKAGE System::Word __fastcall ALGetCodePageFromCharSetName(System::AnsiString Acharset);
extern DELPHI_PACKAGE System::Word __fastcall ALGetCodePageFromLCID(const int aLCID);
extern DELPHI_PACKAGE System::WideString __fastcall ALStringToWideString(const System::RawByteString S, const System::Word aCodePage);
extern DELPHI_PACKAGE System::AnsiString __fastcall AlWideStringToString(const System::WideString WS, const System::Word aCodePage);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8Encode(const System::RawByteString S, const System::Word aCodePage);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8decode(const System::UTF8String S, const System::Word aCodePage);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8ISO91995CyrillicToLatin(const System::AnsiString aCyrillicText);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALUTF8BGNPCGN1947CyrillicToLatin(const System::AnsiString aCyrillicText);
extern DELPHI_PACKAGE bool __fastcall ALExtractExpression(const System::AnsiString S, const char OpenChar, const char CloseChar, const char *QuoteChars, const int QuoteChars_High, const char EscapeQuoteChar, int &StartPos, int &EndPos);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALHTTPEncode(const System::AnsiString AStr);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALHTTPDecode(const System::AnsiString AStr);
extern DELPHI_PACKAGE void __fastcall ALExtractHeaderFields(const System::Sysutils::TSysCharSet &Separators, const System::Sysutils::TSysCharSet &WhiteSpace, const System::Sysutils::TSysCharSet &Quotes, char * Content, Alstringlist::TALStrings* Strings, bool HttpDecode, bool StripQuotes = false);
extern DELPHI_PACKAGE void __fastcall ALExtractHeaderFieldsWithQuoteEscaped(const System::Sysutils::TSysCharSet &Separators, const System::Sysutils::TSysCharSet &WhiteSpace, const System::Sysutils::TSysCharSet &Quotes, char * Content, Alstringlist::TALStrings* Strings, bool HttpDecode, bool StripQuotes = false);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALHTTPEncodeU(const System::UnicodeString AStr);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALHTTPDecodeU(const System::UnicodeString AStr);
extern DELPHI_PACKAGE void __fastcall ALExtractHeaderFieldsWithQuoteEscapedU(const System::Sysutils::TSysCharSet &Separators, const System::Sysutils::TSysCharSet &WhiteSpace, const System::Sysutils::TSysCharSet &Quotes, System::WideChar * Content, Alstringlist::TALStringsU* Strings, bool HttpDecode, bool StripQuotes = false);
extern DELPHI_PACKAGE void __fastcall ALStringInitialization(void);
extern DELPHI_PACKAGE void __fastcall ALStringFinalization(void);
}	/* namespace Alstring */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALSTRING)
using namespace Alstring;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlstringHPP
