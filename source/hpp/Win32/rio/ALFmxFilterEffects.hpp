// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxFilterEffects.pas' rev: 33.00 (Windows)

#ifndef AlfmxfiltereffectsHPP
#define AlfmxfiltereffectsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>
#include <FMX.Filter.hpp>
#include <FMX.Filter.Effects.hpp>
#include <FMX.Effects.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxfiltereffects
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALColorAdjustShaderVariables;
class DELPHICLASS TALFilterBaseFilterAccessPrivate;
class DELPHICLASS TALFilterBaseFilter;
class DELPHICLASS TALColorAdjustEffect;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TALColorAdjustShaderVariables : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	float fContrast;
	float fHighlights;
	float fShadows;
	float fSaturation;
	float fVibrance;
	float fWhites;
	float fBlacks;
	float fTemperature;
	float fTint;
	float fExposure;
	float fGamma;
	
public:
	static float defContrast;
	static float defHighlights;
	static float defShadows;
	static float defSaturation;
	static float defVibrance;
	static float defWhites;
	static float defBlacks;
	static float defTemperature;
	static float defTint;
	static float defExposure;
	static float defGamma;
	__fastcall virtual TALColorAdjustShaderVariables();
	void __fastcall UpdateContext(Fmx::Types3d::TContext3D* const Context);
	__property float Contrast = {read=fContrast, write=fContrast};
	__property float Highlights = {read=fHighlights, write=fHighlights};
	__property float Shadows = {read=fShadows, write=fShadows};
	__property float Saturation = {read=fSaturation, write=fSaturation};
	__property float Vibrance = {read=fVibrance, write=fVibrance};
	__property float Whites = {read=fWhites, write=fWhites};
	__property float Blacks = {read=fBlacks, write=fBlacks};
	__property float Temperature = {read=fTemperature, write=fTemperature};
	__property float Tint = {read=fTint, write=fTint};
	__property float Exposure = {read=fExposure, write=fExposure};
	__property float Gamma = {read=fGamma, write=fGamma};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALColorAdjustShaderVariables() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TALFilterBaseFilterAccessPrivate : public Fmx::Types::TFmxObject
{
	typedef Fmx::Types::TFmxObject inherited;
	
protected:
	Fmx::Filter::TFilter* FFilter;
	Fmx::Filter::Effects::TFilterBaseFilter* FInputFilter;
public:
	/* TFmxObject.Create */ inline __fastcall virtual TALFilterBaseFilterAccessPrivate(System::Classes::TComponent* AOwner) : Fmx::Types::TFmxObject(AOwner) { }
	/* TFmxObject.Destroy */ inline __fastcall virtual ~TALFilterBaseFilterAccessPrivate() { }
	
};


class PASCALIMPLEMENTATION TALFilterBaseFilter : public Fmx::Filter::Effects::TFilterBaseFilter
{
	typedef Fmx::Filter::Effects::TFilterBaseFilter inherited;
	
protected:
	Fmx::Filter::TFilter* __fastcall filter();
public:
	/* TFilterBaseFilter.Create */ inline __fastcall virtual TALFilterBaseFilter(System::Classes::TComponent* AOwner) : Fmx::Filter::Effects::TFilterBaseFilter(AOwner) { }
	/* TFilterBaseFilter.Destroy */ inline __fastcall virtual ~TALFilterBaseFilter() { }
	
};


class PASCALIMPLEMENTATION TALColorAdjustEffect : public Fmx::Filter::Effects::TImageFXEffect
{
	typedef Fmx::Filter::Effects::TImageFXEffect inherited;
	
private:
	float __fastcall GetContrast();
	void __fastcall SetContrast(float AValue);
	float __fastcall GetHighlights();
	void __fastcall SetHighlights(float AValue);
	float __fastcall GetShadows();
	void __fastcall SetShadows(float AValue);
	float __fastcall GetSaturation();
	void __fastcall SetSaturation(float AValue);
	float __fastcall GetVibrance();
	void __fastcall SetVibrance(float AValue);
	float __fastcall GetWhites();
	void __fastcall SetWhites(float AValue);
	float __fastcall GetBlacks();
	void __fastcall SetBlacks(float AValue);
	float __fastcall GetTemperature();
	void __fastcall SetTemperature(float AValue);
	float __fastcall GetTint();
	void __fastcall SetTint(float AValue);
	float __fastcall GetExposure();
	void __fastcall SetExposure(float AValue);
	float __fastcall GetGamma();
	void __fastcall SetGamma(float AValue);
	
public:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
__published:
	__property float Contrast = {read=GetContrast, write=SetContrast};
	__property float Highlights = {read=GetHighlights, write=SetHighlights};
	__property float Shadows = {read=GetShadows, write=SetShadows};
	__property float Saturation = {read=GetSaturation, write=SetSaturation};
	__property float Vibrance = {read=GetVibrance, write=SetVibrance};
	__property float Whites = {read=GetWhites, write=SetWhites};
	__property float Blacks = {read=GetBlacks, write=SetBlacks};
	__property float Temperature = {read=GetTemperature, write=SetTemperature};
	__property float Tint = {read=GetTint, write=SetTint};
	__property float Exposure = {read=GetExposure, write=SetExposure};
	__property float Gamma = {read=GetGamma, write=SetGamma};
public:
	/* TFilterEffect.Create */ inline __fastcall virtual TALColorAdjustEffect(System::Classes::TComponent* AOwner) : Fmx::Filter::Effects::TImageFXEffect(AOwner) { }
	/* TFilterEffect.Destroy */ inline __fastcall virtual ~TALColorAdjustEffect() { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define ALColorAdjustGLSL L"%suniform float _Highlights;uniform float _Shadows;uniform"\
	L" float _Saturation;uniform float _Vibrance;uniform float _"\
	L"Contrast;uniform float _Whites;uniform float _Blacks;unifo"\
	L"rm float _Temperature;uniform float _Tint;uniform float _E"\
	L"xposure;uniform float _Gamma;float BlendOverlayf(float bas"\
	L"e, float blend){return (base < 0.5 ? (2.0 * base * blend) "\
	L": (1.0 - 2.0 * (1.0 - base) * (1.0 - blend)));}vec3 BlendO"\
	L"verlay(vec3 base, vec3 blend){return vec3(BlendOverlayf(ba"\
	L"se.r, blend.r), BlendOverlayf(base.g, blend.g), BlendOverl"\
	L"ayf(base.b, blend.b));}mat3 sRGB2XYZ = mat3(0.4360747,  0."\
	L"3850649,  0.1430804,0.2225045,  0.7168786,  0.0606169,0.01"\
	L"39322,  0.0971045,  0.7141733);mat3 XYZ2sRGB = mat3(3.1338"\
	L"561, -1.6168667, -0.4906146,-0.9787684,  1.9161415,  0.033"\
	L"4540,0.0719453, -0.2289914,  1.4052427);mat3 ROMM2XYZ = ma"\
	L"t3(0.7976749,  0.1351917,  0.0313534,0.2880402,  0.7118741"\
	L",  0.0000857,0.0000000,  0.0000000,  0.8252100);mat3 XYZ2R"\
	L"OMM = mat3(1.3459433, -0.2556075, -0.0511118,-0.5445989,  "\
	L"1.5081673,  0.0205351,0.0000000,  0.0000000,  1.2118128);v"\
	L"ec4 doContrast(vec4 col) {float amount = (_Contrast < 0.0)"\
	L" ? _Contrast/2.0 : _Contrast;vec3 base = col.rgb * sRGB2XY"\
	L"Z * XYZ2ROMM;vec3 overlay = mix(vec3(0.5), base, amount * "\
	L"col.a);vec3 res = BlendOverlay(base, overlay) * ROMM2XYZ *"\
	L" XYZ2sRGB;return vec4(res, col.a);}vec4 doHighlightsAndSha"\
	L"dows(vec4 source) {const vec3 luminanceWeighting = vec3(0."\
	L"3, 0.3, 0.3);float luminance = dot(source.rgb, luminanceWe"\
	L"ighting);float shadow = clamp((pow(luminance, 1.0/(_Shadow"\
	L"s+1.0)) + (-0.76)*pow(luminance, 2.0/(_Shadows+1.0))) - lu"\
	L"minance, 0.0, 1.0);float highlight = clamp((1.0 - (pow(1.0"\
	L"-luminance, 1.0/(2.0-_Highlights)) + (-0.8)*pow(1.0-lumina"\
	L"nce, 2.0/(2.0-_Highlights)))) - luminance, -1.0, 0.0);vec3"\
	L" result = vec3(0.0, 0.0, 0.0) + ((luminance + shadow + hig"\
	L"hlight) - 0.0) * ((source.rgb - vec3(0.0, 0.0, 0.0))/(lumi"\
	L"nance - 0.0));return vec4(result.rgb, source.a);}vec4 doSa"\
	L"turationVibrance(vec4 col) {vec3 color = col.rgb;float lum"\
	L"inance = color.r*0.299 + color.g*0.587 + color.b*0.114;flo"\
	L"at mn = min(min(color.r, color.g), color.b);float mx = max"\
	L"(max(color.r, color.g), color.b);float sat = (1.0-(mx - mn"\
	L")) * (1.0-mx) * luminance * 5.0;vec3 lightness = vec3((mn "\
	L"+ mx)/2.0);color = mix(color, mix(color, lightness, -_Vibr"\
	L"ance), sat);color = mix(color, lightness, (1.0-lightness)*"\
	L"(1.0-_Vibrance)/2.0*abs(_Vibrance));color = mix(color, vec"\
	L"3(luminance), -_Saturation);return vec4(mix(col.rgb, color"\
	L", col.a), col.a);}float wb_Lum(vec3 c){return 0.299*c.r + "\
	L"0.587*c.g + 0.114*c.b;}vec3 wb_ClipColor(vec3 c){float l ="\
	L" wb_Lum(c);float n = min(min(c.r, c.g), c.b);float x = max"\
	L"(max(c.r, c.g), c.b);if (n < 0.0) c = max((c-l)*l / (l-n) "\
	L"+ l, 0.0);if (x > 1.0) c = min((c-l) * (1.0-l) / (x-l) + l"\
	L", 1.0);return c;}vec3 wb_SetLum(vec3 c, float l){c += l - "\
	L"wb_Lum(c);return wb_ClipColor(c);}const float wb = 5.33677"\
	L"8471840789E-03;const float wc = 6.664243592410049E-01;cons"\
	L"t float wd = 3.023761372137289E+00;const float we = -6.994"\
	L"413182098681E+00;const float wf = 3.293987131616894E+00;co"\
	L"nst float wb2 = -1.881032803339283E-01;const float wc2 = 2"\
	L".812945435181010E+00;const float wd2 = -1.495096839176419E"\
	L"+01;const float we2 = 3.349416467551858E+01;const float wf"\
	L"2 = -3.433024909629221E+01;const float wg2 = 1.31430820044"\
	L"2166E+01;const float bb = 8.376727344831676E-01;const floa"\
	L"t bc = -3.418495999327269E+00;const float bd = 8.078054837"\
	L"335609E+00;const float be = -1.209938703324099E+01;const f"\
	L"loat bf = 9.520315785756406E+00;const float bg = -2.919340"\
	L"722745241E+00;const float ba2 = 5.088652898054800E-01;cons"\
	L"t float bb2 = -9.767371127415029E+00;const float bc2 = 4.9"\
	L"10705739925203E+01;const float bd2 = -1.212150899746360E+0"\
	L"2;const float be2 = 1.606205314047741E+02;const float bf2 "\
	L"= -1.085660871669277E+02;const float bg2 = 2.9315822146013"\
	L"88E+01;vec4 doWhitesBlacks(vec4 color) {vec3 base = color."\
	L"rgb;float maxx = max(base.r, max(base.g, base.b));float mi"\
	L"nx = min(base.r, min(base.g, base.b));float lum = (maxx+mi"\
	L"nx)/2.0;float x = lum;float x2 = x*x;float x3 = x2*x;float"\
	L" lum_pos, lum_neg;vec3 res;lum_pos = wb*x + wc*x2+ wd*x3 +"\
	L" we*x2*x2 + wf*x2*x3;lum_pos = min(lum_pos,1.0-lum);lum_ne"\
	L"g = wb2*x + wc2*x2+ wd2*x3 + we2*x2*x2 + wf2*x2*x3 + wg2*x"\
	L"3*x3;lum_neg = max(lum_neg,-lum);res = _Whites>=0.0 ? base"\
	L"*(lum_pos*_Whites+lum)/lum : base * (lum-lum_neg*_Whites)/"\
	L"lum;res = clamp(res, 0.0, 1.0);lum_pos = bb*x + bc*x2+ bd*"\
	L"x3 + be*x2*x2 + bf*x2*x3 + bg*x3*x3;lum_pos = min(lum_pos,"\
	L"1.0-lum);lum_neg = lum<=0.23 ? -lum : ba2 + bb2*x + bc2*x2"\
	L"+ bd2*x3 + be2*x2*x2 + bf2*x2*x3 + bg2*x3*x3;lum_neg = max"\
	L"(lum_neg,-lum);res = _Blacks>=0.0 ? res*(lum_pos*_Blacks+l"\
	L"um)/lum : res * (lum-lum_neg*_Blacks)/lum;res = clamp(res,"\
	L" 0.0, 1.0);return vec4(wb_SetLum(base, wb_Lum(res)), 1.0);"\
	L"}mat3 matRGBtoXYZ = mat3(0.4124564390896922, 0.21267285140"\
	L"562253, 0.0193338955823293,0.357576077643909, 0.7151521552"\
	L"87818, 0.11919202588130297,0.18043748326639894, 0.07217499"\
	L"330655958, 0.9503040785363679);mat3 matXYZtoRGB = mat3(3.2"\
	L"404541621141045, -0.9692660305051868, 0.055643430959114726"\
	L",-1.5371385127977166, 1.8760108454466942, -0.2040259135167"\
	L"538,-0.498531409556016, 0.041556017530349834, 1.0572251882"\
	L"231791);mat3 matAdapt = mat3(0.8951, -0.7502, 0.0389,0.266"\
	L"4, 1.7135, -0.0685,-0.1614, 0.0367, 1.0296);mat3 matAdaptI"\
	L"nv = mat3(0.9869929054667123, 0.43230526972339456, -0.0085"\
	L"28664575177328,-0.14705425642099013, 0.5183602715367776, 0"\
	L".04004282165408487,0.15996265166373125, 0.0492912282128556"\
	L", 0.9684866957875502);vec3 refWhite, refWhiteRGB;vec3 d, s"\
	L";vec3 RGBtoXYZ(vec3 rgb){vec3 xyz, XYZ;xyz = matRGBtoXYZ *"\
	L" rgb;XYZ = matAdapt * xyz;XYZ *= d/s;xyz = matAdaptInv * X"\
	L"YZ;return xyz;}vec3 XYZtoRGB(vec3 xyz){vec3 rgb, RGB;RGB ="\
	L" matAdapt * xyz;rgb *= s/d;xyz = matAdaptInv * RGB;rgb = m"\
	L"atXYZtoRGB * xyz;return rgb;}float t_Lum(vec3 c){return 0."\
	L"299*c.r + 0.587*c.g + 0.114*c.b;}vec3 t_ClipColor(vec3 c){"\
	L"float l = t_Lum(c);float n = min(min(c.r, c.g), c.b);float"\
	L" x = max(max(c.r, c.g), c.b);if (n < 0.0) c = (c-l)*l / (l"\
	L"-n) + l;if (x > 1.0) c = (c-l) * (1.0-l) / (x-l) + l;retur"\
	L"n c;}vec3 t_SetLum(vec3 c, float l){float d = l - t_Lum(c)"\
	L";c.r = c.r + d;c.g = c.g + d;c.b = c.b + d;return t_ClipCo"\
	L"lor(c);}vec3 D50 = vec3(0.96422, 1.0, 0.82521);vec3 D65 = "\
	L"vec3(0.95047, 1.0, 1.08883);vec3 CCT2K = vec3(1.274335, 1."\
	L"0, 0.145233);vec3 CCT4K = vec3(1.009802, 1.0, 0.644496);ve"\
	L"c3 CCT20K = vec3(0.995451, 1.0, 1.886109);vec4 doTemperatu"\
	L"reTint(vec4 col) {vec3 to, from;if (_Temperature < 0.0) {t"\
	L"o = CCT20K;from = D65;} else {to = CCT4K;from = D65;}vec3 "\
	L"base = col.rgb;float lum = t_Lum(base);float temp = abs(_T"\
	L"emperature) * (1.0 - pow(lum, 2.72));refWhiteRGB = from;re"\
	L"fWhite = vec3(mix(from.x, to.x, temp), mix(1.0, 0.9, _Tint"\
	L"), mix(from.z, to.z, temp));refWhite = mix(refWhiteRGB, re"\
	L"fWhite, col.a);d = matAdapt * refWhite;s = matAdapt * refW"\
	L"hiteRGB;vec3 xyz = RGBtoXYZ(base);vec3 rgb = XYZtoRGB(xyz)"\
	L";vec3 res = rgb * (1.0 + (temp + _Tint) / 10.0);return vec"\
	L"4(mix(base, res, col.a), col.a);}float e_Lum(vec3 c){retur"\
	L"n 0.298839*c.r + 0.586811*c.g + 0.11435*c.b;}vec3 e_ClipCo"\
	L"lor(vec3 c){float l = e_Lum(c);float n = min(min(c.r, c.g)"\
	L", c.b);float x = max(max(c.r, c.g), c.b);if (n < 0.0) c = "\
	L"max((c-l)*l / (l-n) + l, 0.0);if (x > 1.0) c = min((c-l) *"\
	L" (1.0-l) / (x-l) + l, 1.0);return c;}vec3 e_SetLum(vec3 c,"\
	L" float l){c += l - e_Lum(c);return e_ClipColor(c);}float S"\
	L"at(vec3 c){float n = min(min(c.r, c.g), c.b);float x = max"\
	L"(max(c.r, c.g), c.b);return x - n;}vec3 SetSat(vec3 c, flo"\
	L"at s){float cmin = min(min(c.r, c.g), c.b);float cmax = ma"\
	L"x(max(c.r, c.g), c.b);vec3 res = vec3(0.0);if (cmax > cmin"\
	L") {if (c.r == cmin && c.b == cmax) {res.r = 0.0;res.g = (("\
	L"c.g-cmin)*s) / (cmax-cmin);res.b = s;}else if (c.r == cmin"\
	L" && c.g == cmax) {res.r = 0.0;res.b = ((c.b-cmin)*s) / (cm"\
	L"ax-cmin);res.g = s;}else if (c.g == cmin && c.b == cmax) {"\
	L"res.g = 0.0;res.r = ((c.r-cmin)*s) / (cmax-cmin);res.b = s"\
	L";}else if (c.g == cmin && c.r == cmax) {res.g = 0.0;res.b "\
	L"= ((c.b-cmin)*s) / (cmax-cmin);res.r = s;}else if (c.b == "\
	L"cmin && c.r == cmax) {res.b = 0.0;res.g = ((c.g-cmin)*s) /"\
	L" (cmax-cmin);res.r = s;}else {res.b = 0.0;res.r = ((c.r-cm"\
	L"in)*s) / (cmax-cmin);res.g = s;}}return res;}float ramp(fl"\
	L"oat t){t *= 2.0;if (t >= 1.0) {t -= 1.0;t = log(0.5) / log"\
	L"(0.5*(1.0-t) + 0.9332*t);}return clamp(t, 0.001, 10.0);}ve"\
	L"c4 doExposureGamma(vec4 col) {vec3 base = col.rgb;vec3 res"\
	L", blend;float amt = mix(0.009, 0.98, _Exposure);if (amt < "\
	L"0.0) {res = mix(vec3(0.0), base, amt + 1.0);blend = mix(ba"\
	L"se, vec3(0.0), amt + 1.0);res = min(res / (1.0 - blend*0.9"\
	L"), 1.0);} else {res = mix(base, vec3(1.0), amt);blend = mi"\
	L"x(vec3(1.0), pow(base, vec3(1.0/0.7)), amt);res = max(1.0 "\
	L"- ((1.0 - res) / blend), 0.0);}res = pow(e_SetLum(SetSat(b"\
	L"ase, Sat(res)), e_Lum(res)), vec3(ramp(1.0 - (_Gamma + 1.0"\
	L") / 2.0)));return vec4(mix(base, res, col.a), col.a);}%svo"\
	L"id main() {%sconst float eps = 0.001;if ((abs(_Temperature"\
	L") > eps) || (abs(_Tint) > eps)) { result = doTemperatureTi"\
	L"nt(result); }if ((abs(_Exposure) > eps) || (abs(_Gamma) > "\
	L"eps)) { result = doExposureGamma(result); }if (abs(_Contra"\
	L"st) > eps) { result = doContrast(result); }if ((abs(_Highl"\
	L"ights - 1.0) > eps) || (abs(_Shadows) > eps)) { result = d"\
	L"oHighlightsAndShadows(result); }if ((abs(_Whites) > eps) |"\
	L"| (abs(_Blacks) > eps)) { result = doWhitesBlacks(result);"\
	L" }if ((abs(_Saturation) > eps) || (abs(_Vibrance) > eps)) "\
	L"{ result = doSaturationVibrance(result); }%sgl_FragColor ="\
	L" result;}"
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alfmxfiltereffects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXFILTEREFFECTS)
using namespace Alfmxfiltereffects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxfiltereffectsHPP
