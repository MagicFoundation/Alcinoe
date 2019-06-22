// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALCommon.pas' rev: 33.00 (Windows)

#ifndef AlcommonHPP
#define AlcommonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alcommon
{
//-- forward type declarations -----------------------------------------------
struct TALPointD;
struct TALSizeD;
struct TALRectD;
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
	TALPointD __fastcall operator+(const TALPointD& __rhs) { return TALPointD::_op_Addition(*this, __rhs); };
	static TALPointD __fastcall _op_Subtraction(const TALPointD &APoint1, const TALPointD &APoint2);
	TALPointD __fastcall operator-(const TALPointD& __rhs) { return TALPointD::_op_Subtraction(*this, __rhs); };
	static bool __fastcall _op_Equality(const TALPointD &APoint1, const TALPointD &APoint2);
	bool __fastcall operator==(const TALPointD& __rhs) { return TALPointD::_op_Equality(*this, __rhs); };
	static bool __fastcall _op_Inequality(const TALPointD &APoint1, const TALPointD &APoint2);
	bool __fastcall operator!=(const TALPointD& __rhs) { return TALPointD::_op_Inequality(*this, __rhs); };
	static TALPointD __fastcall _op_Implicit(const System::Types::TPoint &APoint);
	TALPointD& __fastcall operator=(const System::Types::TPoint &APoint) { *this = TALPointD::_op_Implicit(APoint); return *this; };
	static TALPointD __fastcall _op_UnaryNegation(const TALPointD &APoint);
	TALPointD __fastcall operator-() { return TALPointD::_op_UnaryNegation(*this); };
	static TALPointD __fastcall _op_Multiply(const TALPointD &APoint1, const TALPointD &APoint2);
	TALPointD __fastcall operator*(const TALPointD& __rhs) { return TALPointD::_op_Multiply(*this, __rhs); };
	static TALPointD __fastcall _op_Multiply(const TALPointD &APoint, const double AFactor);
	TALPointD __fastcall operator*(const double& __rhs) { return TALPointD::_op_Multiply(*this, __rhs); };
	static TALPointD __fastcall _op_Multiply(const double AFactor, const TALPointD &APoint);
	static TALPointD __fastcall _op_Division(const TALPointD &APoint, const double AFactor);
	TALPointD __fastcall operator/(const double& __rhs) { return TALPointD::_op_Division(*this, __rhs); };
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
	bool __fastcall operator==(const TALSizeD& __rhs) { return TALSizeD::_op_Equality(*this, __rhs); };
	static bool __fastcall _op_Inequality(const TALSizeD &Lhs, const TALSizeD &Rhs);
	bool __fastcall operator!=(const TALSizeD& __rhs) { return TALSizeD::_op_Inequality(*this, __rhs); };
	static TALSizeD __fastcall _op_Addition(const TALSizeD &Lhs, const TALSizeD &Rhs);
	TALSizeD __fastcall operator+(const TALSizeD& __rhs) { return TALSizeD::_op_Addition(*this, __rhs); };
	static TALSizeD __fastcall _op_Subtraction(const TALSizeD &Lhs, const TALSizeD &Rhs);
	TALSizeD __fastcall operator-(const TALSizeD& __rhs) { return TALSizeD::_op_Subtraction(*this, __rhs); };
	__fastcall operator TALPointD();
	static TALSizeD __fastcall _op_Implicit(const TALPointD &Point);
	TALSizeD& __fastcall operator=(const TALPointD &Point) { *this = TALSizeD::_op_Implicit(Point); return *this; };
	static TALSizeD __fastcall _op_Implicit(const System::Types::TSize &Size);
	TALSizeD& __fastcall operator=(const System::Types::TSize &Size) { *this = TALSizeD::_op_Implicit(Size); return *this; };
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
	bool __fastcall operator==(const TALRectD& __rhs) { return TALRectD::_op_Equality(*this, __rhs); };
	static bool __fastcall _op_Inequality(const TALRectD &Lhs, const TALRectD &Rhs);
	bool __fastcall operator!=(const TALRectD& __rhs) { return TALRectD::_op_Inequality(*this, __rhs); };
	static TALRectD __fastcall _op_Implicit(const System::Types::TRect &Source);
	TALRectD& __fastcall operator=(const System::Types::TRect &Source) { *this = TALRectD::_op_Implicit(Source); return *this; };
	static TALRectD __fastcall _op_Addition(const TALRectD &Lhs, const TALRectD &Rhs);
	TALRectD __fastcall operator+(const TALRectD& __rhs) { return TALRectD::_op_Addition(*this, __rhs); };
	static TALRectD __fastcall _op_Multiply(const TALRectD &Lhs, const TALRectD &Rhs);
	TALRectD __fastcall operator*(const TALRectD& __rhs) { return TALRectD::_op_Multiply(*this, __rhs); };
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


enum DECLSPEC_DENUM TalLogType : unsigned char { VERBOSE, DEBUG, INFO, WARN, ERROR, ASSERT };

typedef void __fastcall (__closure *TALCustomDelayedFreeObjectProc)(System::TObject* &aObject);

enum DECLSPEC_DENUM TALIntelCpuFeature : unsigned char { cfFPU, cfVME, cfDE, cfPSE, cfTSC, cfMSR, cfPAE, cfMCE, cfCX8, cfAPIC, cf_d10, cfSEP, cfMTRR, cfPGE, cfMCA, cfCMOV, cfPAT, cfPSE36, cfPSN, cfCLFSH, cf_d20, cfDS, cfACPI, cfMMX, cfFXSR, cfSSE, cfSSE2, cfSS, cfHTT, cfTM, cfIA64, cfPBE, cfSSE3, cfCLMUL, cfDS64, cfMON, cfDSCPL, cfVMX, cfSMX, cfEST, cfTM2, cfSSSE3, cfCID, cfSDBG, cfFMA, cfCX16, cfXTPR, cfPDCM, cf_c16, cfPCID, cfDCA, cfSSE41, cfSSE42, cfX2A, cfMOVBE, cfPOPCNT, cfTSC2, cfAESNI, cfXS, cfOSXS, cfAVX, cfF16C, cfRAND, cfHYP, cfFSGS, cf_b01, cfSGX, cfBMI1, cfHLE, cfAVX2, cf_b06, cfSMEP, cfBMI2, cfERMS, cfINVPCID, cfRTM, cfPQM, cf_b13, cfMPX, cfPQE, cfAVX512F, cfAVX512DQ, cfRDSEED, cfADX, cfSMAP, cfAVX512IFMA, cfPCOMMIT, cfCLFLUSH, cfCLWB, cfIPT, 
	cfAVX512PF, cfAVX512ER, cfAVX512CD, cfSHA, cfAVX512BW, cfAVX512VL, cfPREFW1, cfAVX512VBMI };

typedef System::Set<TALIntelCpuFeature, TALIntelCpuFeature::cfFPU, TALIntelCpuFeature::cfAVX512VBMI> TALIntelCpuFeatures;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TALCustomDelayedFreeObjectProc ALCustomDelayedFreeObjectProc;
extern DELPHI_PACKAGE unsigned __int64 ALMAXUInt64;
extern DELPHI_PACKAGE __int64 ALMAXInt64;
extern DELPHI_PACKAGE unsigned ALMAXUInt;
extern DELPHI_PACKAGE int ALMAXInt;
static const System::Int8 ALNullDate = System::Int8(0x0);
extern DELPHI_PACKAGE TALIntelCpuFeatures ALCpuFeatures;
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
extern DELPHI_PACKAGE void __fastcall ALLog(const System::UnicodeString Tag, const System::UnicodeString msg, const TalLogType _type = (TalLogType)(0x2));
extern DELPHI_PACKAGE int __fastcall AlBoolToInt(bool Value);
extern DELPHI_PACKAGE bool __fastcall AlIntToBool(int Value);
extern DELPHI_PACKAGE int __fastcall ALMediumPos(int LTotal, int LBorder, int LObject);
extern DELPHI_PACKAGE System::TDateTime __fastcall AlLocalDateTimeToUTCDateTime(const System::TDateTime aLocalDateTime);
extern DELPHI_PACKAGE System::TDateTime __fastcall AlUTCDateTimeToLocalDateTime(const System::TDateTime aUTCDateTime);
extern DELPHI_PACKAGE System::TDateTime __fastcall ALUTCNow(void);
extern DELPHI_PACKAGE int __fastcall ALInc(int &x, int Count);
extern DELPHI_PACKAGE System::TDateTime __fastcall ALUnixMsToDateTime(const __int64 aValue);
extern DELPHI_PACKAGE __int64 __fastcall ALDateTimeToUnixMs(const System::TDateTime aValue);
extern DELPHI_PACKAGE void __fastcall ALFreeAndNil(void *Obj, const bool adelayed = false)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALFreeAndNil(void *Obj, const bool adelayed, const bool aRefCountWarn)/* overload */;
}	/* namespace Alcommon */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALCOMMON)
using namespace Alcommon;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlcommonHPP
