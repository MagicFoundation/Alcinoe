// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALCommon.pas' rev: 34.00 (Windows)

#ifndef AlcommonHPP
#define AlcommonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alcommon
{
//-- forward type declarations -----------------------------------------------
struct TALPointD;
struct TALSizeD;
struct TALRectD;
class DELPHICLASS EALException;
class DELPHICLASS EALExceptionU;
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<double, 2> TALPointDType;

typedef TALPointD *PALPointD;

struct DECLSPEC_DRECORD TALPointD
{
public:
	static TALPointD __fastcall Create(const double AX, const double AY)/* overload */;
	static TALPointD __fastcall Create(const System::Types::TPoint &APoint)/* overload */;
	static TALPointD __fastcall Create(const System::Types::TPointF &APoint)/* overload */;
	static TALPointD __fastcall _op_Addition(const TALPointD &APoint1, const TALPointD &APoint2);
	static TALPointD __fastcall _op_Subtraction(const TALPointD &APoint1, const TALPointD &APoint2);
	static bool __fastcall _op_Equality(const TALPointD &APoint1, const TALPointD &APoint2);
	static bool __fastcall _op_Inequality(const TALPointD &APoint1, const TALPointD &APoint2);
	static TALPointD __fastcall _op_Implicit(const System::Types::TPoint &APoint);
	static TALPointD __fastcall _op_UnaryNegation(const TALPointD &APoint);
	static TALPointD __fastcall _op_Multiply(const TALPointD &APoint1, const TALPointD &APoint2);
	static TALPointD __fastcall _op_Multiply(const TALPointD &APoint, const double AFactor);
	static TALPointD __fastcall _op_Multiply(const double AFactor, const TALPointD &APoint);
	static TALPointD __fastcall _op_Division(const TALPointD &APoint, const double AFactor);
	static bool __fastcall PointInCircle(const TALPointD &Point, const TALPointD &Center, const int Radius);
	static TALPointD __fastcall Zero();
	double __fastcall Distance(const TALPointD &APoint);
	double __fastcall CrossProduct(const TALPointD &APoint);
	double __fastcall DotProduct(const TALPointD &APoint);
	void __fastcall Offset(const TALPointD &APoint)/* overload */;
	void __fastcall Offset(const double ADeltaX, const double ADeltaY)/* overload */;
	void __fastcall Offset(const System::Types::TPoint &APoint)/* overload */;
	void __fastcall SetLocation _DEPRECATED_ATTRIBUTE1("Use \":=\" assignment instead") (const double X, const double Y)/* overload */;
	void __fastcall SetLocation _DEPRECATED_ATTRIBUTE1("Use \":=\" assignment instead") (const TALPointD &P)/* overload */;
	void __fastcall SetLocation _DEPRECATED_ATTRIBUTE1("Use \":=\" assignment instead") (const System::Types::TPoint &P)/* overload */;
	TALPointD __fastcall Subtract _DEPRECATED_ATTRIBUTE1("Use TALPointD.Offset instead") (const TALPointD &Point)/* overload */;
	TALPointD __fastcall Subtract _DEPRECATED_ATTRIBUTE1("Use TALPointD.Offset instead") (const System::Types::TPoint &Point)/* overload */;
	TALPointD __fastcall Add _DEPRECATED_ATTRIBUTE1("Use TALPointD.Offset instead") (const TALPointD &Point)/* overload */;
	TALPointD __fastcall Add _DEPRECATED_ATTRIBUTE1("Use TALPointD.Offset instead") (const System::Types::TPoint &Point)/* overload */;
	TALPointD __fastcall Scale _DEPRECATED_ATTRIBUTE0 (const double AFactor);
	bool __fastcall EqualsTo(const TALPointD &Point, const double Epsilon = 0.000000E+00);
	bool __fastcall IsZero();
	System::Types::TPoint __fastcall Ceiling();
	System::Types::TPoint __fastcall Truncate();
	System::Types::TPoint __fastcall Round();
	TALPointD __fastcall SnapToPixel(const double AScale, const bool APlaceBetweenPixels = true);
	TALPointD __fastcall Normalize();
	double __fastcall Length();
	TALPointD __fastcall Rotate(const double AAngle);
	TALPointD __fastcall Reflect(const TALPointD &APoint);
	TALPointD __fastcall MidPoint(const TALPointD &APoint);
	double __fastcall AngleCosine(const TALPointD &APoint);
	double __fastcall Angle(const TALPointD &APoint);
	double __fastcall Abs();
	
	friend TALPointD operator +(const TALPointD &APoint1, const TALPointD &APoint2) { return TALPointD::_op_Addition(APoint1, APoint2); }
	friend TALPointD operator -(const TALPointD &APoint1, const TALPointD &APoint2) { return TALPointD::_op_Subtraction(APoint1, APoint2); }
	friend bool operator ==(const TALPointD &APoint1, const TALPointD &APoint2) { return TALPointD::_op_Equality(APoint1, APoint2); }
	friend bool operator !=(const TALPointD &APoint1, const TALPointD &APoint2) { return TALPointD::_op_Inequality(APoint1, APoint2); }
	TALPointD& operator =(const System::Types::TPoint &APoint) { *this = TALPointD::_op_Implicit(APoint); return *this; }
	TALPointD operator -() { return TALPointD::_op_UnaryNegation(*this); }
	friend TALPointD operator *(const TALPointD &APoint1, const TALPointD &APoint2) { return TALPointD::_op_Multiply(APoint1, APoint2); }
	friend TALPointD operator *(const TALPointD &APoint, const double AFactor) { return TALPointD::_op_Multiply(APoint, AFactor); }
	friend TALPointD operator *(const double AFactor, const TALPointD &APoint) { return TALPointD::_op_Multiply(AFactor, APoint); }
	friend TALPointD operator /(const TALPointD &APoint, const double AFactor) { return TALPointD::_op_Division(APoint, AFactor); }
	
public:
	union
	{
		struct 
		{
			double X;
			double Y;
		};
		struct 
		{
			TALPointDType V;
		};
		
	};
};


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
	static TALSizeD __fastcall _op_Implicit(const System::Types::TSize &Size);
	System::Types::TSize __fastcall Ceiling();
	System::Types::TSize __fastcall Truncate();
	System::Types::TSize __fastcall Round();
	TALSizeD __fastcall Add(const TALSizeD &Point);
	TALSizeD __fastcall Subtract(const TALSizeD &Point);
	double __fastcall Distance(const TALSizeD &P2);
	bool __fastcall IsZero();
	TALSizeD __fastcall SwapDimensions();
	__property double Width = {read=cx, write=cx};
	__property double Height = {read=cy, write=cy};
	TALSizeD() {}
	
	friend bool operator ==(const TALSizeD &Lhs, const TALSizeD &Rhs) { return TALSizeD::_op_Equality(Lhs, Rhs); }
	friend bool operator !=(const TALSizeD &Lhs, const TALSizeD &Rhs) { return TALSizeD::_op_Inequality(Lhs, Rhs); }
	friend TALSizeD operator +(const TALSizeD &Lhs, const TALSizeD &Rhs) { return TALSizeD::_op_Addition(Lhs, Rhs); }
	friend TALSizeD operator -(const TALSizeD &Lhs, const TALSizeD &Rhs) { return TALSizeD::_op_Subtraction(Lhs, Rhs); }
	TALSizeD& operator =(const TALPointD &Point) { *this = TALSizeD::_op_Implicit(Point); return *this; }
	TALSizeD& operator =(const System::Types::TSize &Size) { *this = TALSizeD::_op_Implicit(Size); return *this; }
};


typedef TALRectD *PALRectD;

struct DECLSPEC_DRECORD TALRectD
{
private:
	double __fastcall GetWidth();
	void __fastcall SetWidth(const double Value);
	double __fastcall GetHeight();
	void __fastcall SetHeight(const double Value);
	TALSizeD __fastcall GetSize();
	void __fastcall SetSize(const TALSizeD &Value);
	TALPointD __fastcall GetLocation();
	
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
	static TALRectD __fastcall _op_Addition(const TALRectD &Lhs, const TALRectD &Rhs);
	static TALRectD __fastcall _op_Multiply(const TALRectD &Lhs, const TALRectD &Rhs);
	static TALRectD __fastcall Empty();
	double __fastcall Fit(const TALRectD &BoundsRect);
	TALRectD __fastcall FitInto(const TALRectD &ADesignatedArea, /* out */ double &Ratio)/* overload */;
	TALRectD __fastcall FitInto(const TALRectD &ADesignatedArea)/* overload */;
	TALRectD __fastcall CenterAt(const TALRectD &ADesignatedArea);
	TALRectD __fastcall PlaceInto(const TALRectD &ADesignatedArea, const System::Types::THorzRectAlign AHorzAlign = (System::Types::THorzRectAlign)(0x0), const System::Types::TVertRectAlign AVertAlign = (System::Types::TVertRectAlign)(0x0));
	TALRectD __fastcall SnapToPixel(const double AScale, const bool APlaceBetweenPixels = true);
	void __fastcall NormalizeRect();
	bool __fastcall IsEmpty();
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
	TALPointD __fastcall CenterPoint();
	System::Types::TRect __fastcall Ceiling();
	System::Types::TRect __fastcall Truncate();
	System::Types::TRect __fastcall Round();
	bool __fastcall EqualsTo(const TALRectD &R, const double Epsilon = 0.000000E+00);
	__property double Width = {read=GetWidth, write=SetWidth};
	__property double Height = {read=GetHeight, write=SetHeight};
	__property TALSizeD Size = {read=GetSize, write=SetSize};
	__property TALPointD Location = {read=GetLocation, write=SetLocation};
	TALRectD() {}
	
	friend bool operator ==(const TALRectD &Lhs, const TALRectD &Rhs) { return TALRectD::_op_Equality(Lhs, Rhs); }
	friend bool operator !=(const TALRectD &Lhs, const TALRectD &Rhs) { return TALRectD::_op_Inequality(Lhs, Rhs); }
	TALRectD& operator =(const System::Types::TRect &Source) { *this = TALRectD::_op_Implicit(Source); return *this; }
	friend TALRectD operator +(const TALRectD &Lhs, const TALRectD &Rhs) { return TALRectD::_op_Addition(Lhs, Rhs); }
	friend TALRectD operator *(const TALRectD &Lhs, const TALRectD &Rhs) { return TALRectD::_op_Multiply(Lhs, Rhs); }
	
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
	/* Exception.Destroy */ inline __fastcall virtual ~EALException() { }
	
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
	/* Exception.Destroy */ inline __fastcall virtual ~EALExceptionU() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TalLogType : unsigned char { VERBOSE, DEBUG, INFO, WARN, ERROR, ASSERT };

typedef void __fastcall (__closure *TALCustomDelayedFreeObjectProc)(System::TObject* &aObject);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int ALCallStackCustomLogsMaxCount;
extern DELPHI_PACKAGE bool ALEnqueueLog;
extern DELPHI_PACKAGE TALCustomDelayedFreeObjectProc ALCustomDelayedFreeObjectProc;
extern "C" unsigned __stdcall EnumDynamicTimeZoneInformation(unsigned dwIndex, Winapi::Windows::PDynamicTimeZoneInformation lpTimeZoneInformation);
extern "C" System::LongBool __stdcall SystemTimeToTzSpecificLocalTimeEx(Winapi::Windows::PDynamicTimeZoneInformation lpTimeZoneInformation, _SYSTEMTIME &lpUniversalTime, _SYSTEMTIME &lpLocalTime);
extern "C" System::LongBool __stdcall TzSpecificLocalTimeToSystemTimeEx(Winapi::Windows::PDynamicTimeZoneInformation lpTimeZoneInformation, _SYSTEMTIME &lpLocalTime, _SYSTEMTIME &lpUniversalTime);
extern DELPHI_PACKAGE void __fastcall (*ALMove)(const void *Source, void *Dest, NativeInt Count);
extern DELPHI_PACKAGE unsigned __int64 ALMAXUInt64;
extern DELPHI_PACKAGE __int64 ALMAXInt64;
extern DELPHI_PACKAGE unsigned ALMAXUInt;
extern DELPHI_PACKAGE int ALMAXInt;
static const System::Int8 ALNullDate = System::Int8(0x0);
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
extern DELPHI_PACKAGE bool __fastcall ALIntersectRect(/* out */ TALRectD &Rect, const TALRectD &R1, const TALRectD &R2);
extern DELPHI_PACKAGE bool __fastcall ALUnionRect(/* out */ TALRectD &Rect, const TALRectD &R1, const TALRectD &R2);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALRectFitInto(const System::Types::TRectF &R, const System::Types::TRectF &Bounds, const System::Types::TPointF &CenterAt, /* out */ float &Ratio)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALRectFitInto(const System::Types::TRectF &R, const System::Types::TRectF &Bounds, const System::Types::TPointF &CenterAt)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALRectFitInto(const System::Types::TRectF &R, const System::Types::TRectF &Bounds, /* out */ float &Ratio)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALRectFitInto(const System::Types::TRectF &R, const System::Types::TRectF &Bounds)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALRectPlaceInto(const System::Types::TRectF &R, const System::Types::TRectF &Bounds, const System::Types::TPointF &CenterAt, /* out */ float &Ratio)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALRectPlaceInto(const System::Types::TRectF &R, const System::Types::TRectF &Bounds, const System::Types::TPointF &CenterAt)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALRectPlaceInto(const System::Types::TRectF &R, const System::Types::TRectF &Bounds, const System::Types::THorzRectAlign AHorzAlign = (System::Types::THorzRectAlign)(0x0), const System::Types::TVertRectAlign AVertAlign = (System::Types::TVertRectAlign)(0x0))/* overload */;
extern DELPHI_PACKAGE void __fastcall ALAddCallStackCustomLogU(const System::UnicodeString aLog);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALGetCallStackCustomLogsU(const bool aPrependTimeStamp = true, const bool aPrependThreadID = true);
extern DELPHI_PACKAGE void __fastcall ALLog(const System::UnicodeString Tag, const System::UnicodeString msg, const TalLogType _type = (TalLogType)(0x2));
extern DELPHI_PACKAGE void __fastcall ALPrintLogQueue(void);
extern DELPHI_PACKAGE int __fastcall AlBoolToInt(bool Value);
extern DELPHI_PACKAGE bool __fastcall AlIntToBool(int Value);
extern DELPHI_PACKAGE int __fastcall ALMediumPos(int LTotal, int LBorder, int LObject);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALIfThen(bool AValue, const System::AnsiString ATrue, System::AnsiString AFalse = System::AnsiString())/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALIfThenU(bool AValue, const System::UnicodeString ATrue, System::UnicodeString AFalse = System::UnicodeString())/* overload */;
extern DELPHI_PACKAGE int __fastcall ALIfThen(bool AValue, const int ATrue, const int AFalse = 0x0)/* overload */;
extern DELPHI_PACKAGE __int64 __fastcall ALIfThen(bool AValue, const __int64 ATrue, const __int64 AFalse = 0LL)/* overload */;
extern DELPHI_PACKAGE unsigned __int64 __fastcall ALIfThen(bool AValue, const unsigned __int64 ATrue, const unsigned __int64 AFalse = 0ULL)/* overload */;
extern DELPHI_PACKAGE float __fastcall ALIfThen(bool AValue, const float ATrue, const float AFalse = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE double __fastcall ALIfThen(bool AValue, const double ATrue, const double AFalse = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall ALIfThen(bool AValue, const System::Extended ATrue, const System::Extended AFalse = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE System::DynamicArray<_TIME_DYNAMIC_ZONE_INFORMATION> __fastcall ALGetDynamicTimeZoneInformations(void);
extern DELPHI_PACKAGE _TIME_DYNAMIC_ZONE_INFORMATION __fastcall ALGetDynamicTimeZoneInformation(const System::UnicodeString aTimeZoneKeyName);
extern DELPHI_PACKAGE System::TDateTime __fastcall AlLocalDateTimeToUTC(const System::TDateTime aLocalDateTime)/* overload */;
extern DELPHI_PACKAGE System::TDateTime __fastcall AlLocalDateTimeToUTC(const System::UnicodeString aTimeZoneKeyName, System::TDateTime aLocalDateTime)/* overload */;
extern DELPHI_PACKAGE System::TDateTime __fastcall AlLocalDateTimeToUTC(const _TIME_DYNAMIC_ZONE_INFORMATION &aTimeZoneInformation, System::TDateTime aLocalDateTime)/* overload */;
extern DELPHI_PACKAGE System::TDateTime __fastcall AlUTCDateTimeToLocal(const System::TDateTime aUTCDateTime)/* overload */;
extern DELPHI_PACKAGE System::TDateTime __fastcall AlUTCDateTimeToLocal(const System::UnicodeString aTimeZoneKeyName, System::TDateTime aUTCDateTime)/* overload */;
extern DELPHI_PACKAGE System::TDateTime __fastcall AlUTCDateTimeToLocal(const _TIME_DYNAMIC_ZONE_INFORMATION &aTimeZoneInformation, System::TDateTime aUTCDateTime)/* overload */;
extern DELPHI_PACKAGE System::TDateTime __fastcall ALUTCNow(void);
extern DELPHI_PACKAGE int __fastcall ALInc(int &x, int Count);
extern DELPHI_PACKAGE System::TDateTime __fastcall ALUnixMsToDateTime(const __int64 aValue);
extern DELPHI_PACKAGE __int64 __fastcall ALDateTimeToUnixMs(const System::TDateTime aValue);
extern DELPHI_PACKAGE void __fastcall ALFreeAndNil(System::TObject* const &Obj, const bool ADelayed = false);
}	/* namespace Alcommon */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALCOMMON)
using namespace Alcommon;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlcommonHPP
