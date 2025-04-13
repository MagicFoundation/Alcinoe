unit Alcinoe.FMX.FilterEffects;

interface

{$I Alcinoe.inc}

uses
  system.classes,
  FMX.types,
  FMX.types3D,
  FMX.filter,
  FMX.Filter.Effects;

const

  ALColorAdjustGLSL = '''
    %s
    uniform float _Saturation;
    uniform float _Vibrance;
    uniform float _Contrast;
    uniform float _Whites;
    uniform float _Blacks;
    uniform float _Temperature;
    uniform float _Tint;
    uniform float _Exposure;
    uniform float _Gamma;

    /////////////////////////////////////////////////////////////////////////////////////////////
    /// Contrast                                                                              ///
    /// Its not really contrast, I will say its "smart" contrast                              ///
    /// _Contrast: The adjusted contrast (-1.0 <=> 1.0, with 0.0 as the default)              ///
    /// https://gitlab.bestminr.com/bestminr/FrontShaders/blob/master/shaders/contrast.glsl   ///
    /////////////////////////////////////////////////////////////////////////////////////////////

    float BlendOverlayf(float base, float blend){
      return (base < 0.5 ? (2.0 * base * blend) : (1.0 - 2.0 * (1.0 - base) * (1.0 - blend)));
    }

    vec3 BlendOverlay(vec3 base, vec3 blend){
      return vec3(BlendOverlayf(base.r, blend.r), BlendOverlayf(base.g, blend.g), BlendOverlayf(base.b, blend.b));
    }

    mat3 sRGB2XYZ = mat3(
      0.4360747,  0.3850649,  0.1430804,
      0.2225045,  0.7168786,  0.0606169,
      0.0139322,  0.0971045,  0.7141733
    );

    mat3 XYZ2sRGB = mat3(
      3.1338561, -1.6168667, -0.4906146,
      -0.9787684,  1.9161415,  0.0334540,
      0.0719453, -0.2289914,  1.4052427
    );

    mat3 ROMM2XYZ = mat3(
      0.7976749,  0.1351917,  0.0313534,
      0.2880402,  0.7118741,  0.0000857,
      0.0000000,  0.0000000,  0.8252100
    );

    mat3 XYZ2ROMM = mat3(
      1.3459433, -0.2556075, -0.0511118,
      -0.5445989,  1.5081673,  0.0205351,
      0.0000000,  0.0000000,  1.2118128
    );

    vec4 doContrast(vec4 col) {
      float amount = (_Contrast < 0.0) ? _Contrast : _Contrast * 2.0;
      vec3 base = col.rgb * sRGB2XYZ * XYZ2ROMM;
      vec3 overlay = mix(vec3(0.5), base, amount * col.a);
      vec3 res = BlendOverlay(base, overlay) * ROMM2XYZ * XYZ2sRGB;
      return vec4(res, col.a);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////
    /// Highlights                                                                            ///
    /// _Highlights: 0 <=> 1.0, with 0.0 as the default                                       ///
    /// https://gitlab.bestminr.com/bestminr/FrontShaders/blob/master/shaders/highlights.glsl ///
    /////////////////////////////////////////////////////////////////////////////////////////////

    //vec4 doHighlights(vec4 color) {
    //  const float a = 1.357697966704323E-01;
    //  const float b = 1.006045552016985E+00;
    //  const float c = 4.674339906510876E-01;
    //  const float d = 8.029414702292208E-01;
    //  const float e = 1.127806558508491E-01;
    //  float maxx = max(color.r, max(color.g, color.b));
    //  float minx = min(color.r, min(color.g, color.b));
    //  float lum = (maxx+minx)/2.0;
    //  float x1 = abs(_Highlights);
    //  float x2 = lum;
    //  float lum_new =  lum < 0.5 ? lum : lum+ a * sign(_Highlights) * exp(-0.5 * (((x1-b)/c)*((x1-b)/c) + ((x2-d)/e)*((x2-d)/e)));
    //  //return color * lum_new / lum;
    //  return vec4(color * lum_new / lum);
    //}

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    /// saturation_vibrance                                                                            ///
    /// _Saturation: -1.0 <=> 1.0, with 0.0 as the default                                             ///
    /// _Vibrance: -1.0 <=> 1.0, with 0.0 as the default                                               ///
    /// https://gitlab.bestminr.com/bestminr/FrontShaders/blob/master/shaders/saturation_vibrance.glsl ///
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    vec4 doSaturationVibrance(vec4 col) {
      vec3 color = col.rgb;
      float luminance = color.r*0.299 + color.g*0.587 + color.b*0.114;
      float mn = min(min(color.r, color.g), color.b);
      float mx = max(max(color.r, color.g), color.b);
      float sat = (1.0-(mx - mn)) * (1.0-mx) * luminance * 5.0;
      vec3 lightness = vec3((mn + mx)/2.0);
      //vibrance
      color = mix(color, mix(color, lightness, -_Vibrance), sat);
      //negative vibrance
      color = mix(color, lightness, (1.0-lightness)*(1.0-_Vibrance)/2.0*abs(_Vibrance));
      //saturation
      color = mix(color, vec3(luminance), -_Saturation);
      return vec4(mix(col.rgb, color, col.a), col.a);
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// whites_blacks                                                                            ///
    /// _Whites: -100 <=> 100 but I limit to -1 <=> 1, with 0.0 as the default                   ///
    /// _Blacks: -100 <=> 100 but I limit to -1 <=> 1, with 0.0 as the default                   ///
    /// https://gitlab.bestminr.com/bestminr/FrontShaders/blob/master/shaders/whites_blacks.glsl ///
    ////////////////////////////////////////////////////////////////////////////////////////////////

    //wb_Luminance and Saturation functions
    //Adapted from: http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/pdf/pdfs/PDF32000_2008.pdf

    float wb_Lum(vec3 c){
      return 0.299*c.r + 0.587*c.g + 0.114*c.b;
    }

    vec3 wb_ClipColor(vec3 c){
      float l = wb_Lum(c);
      float n = min(min(c.r, c.g), c.b);
      float x = max(max(c.r, c.g), c.b);
      if (n < 0.0) c = max((c-l)*l / (l-n) + l, 0.0);
      if (x > 1.0) c = min((c-l) * (1.0-l) / (x-l) + l, 1.0);
      return c;
    }

    vec3 wb_SetLum(vec3 c, float l){
      c += l - wb_Lum(c);
      return wb_ClipColor(c);
    }

    const float wb = 5.336778471840789E-03;
    const float wc = 6.664243592410049E-01;
    const float wd = 3.023761372137289E+00;
    const float we = -6.994413182098681E+00;
    const float wf = 3.293987131616894E+00;
    const float wb2 = -1.881032803339283E-01;
    const float wc2 = 2.812945435181010E+00;
    const float wd2 = -1.495096839176419E+01;
    const float we2 = 3.349416467551858E+01;
    const float wf2 = -3.433024909629221E+01;
    const float wg2 = 1.314308200442166E+01;

    const float bb = 8.376727344831676E-01;
    const float bc = -3.418495999327269E+00;
    const float bd = 8.078054837335609E+00;
    const float be = -1.209938703324099E+01;
    const float bf = 9.520315785756406E+00;
    const float bg = -2.919340722745241E+00;
    const float ba2 = 5.088652898054800E-01;
    const float bb2 = -9.767371127415029E+00;
    const float bc2 = 4.910705739925203E+01;
    const float bd2 = -1.212150899746360E+02;
    const float be2 = 1.606205314047741E+02;
    const float bf2 = -1.085660871669277E+02;
    const float bg2 = 2.931582214601388E+01;

    vec4 doWhitesBlacks(vec4 color) {
      vec3 base = color.rgb;
      float maxx = max(base.r, max(base.g, base.b));
      float minx = min(base.r, min(base.g, base.b));
      float lum = (maxx+minx)/2.0;
      float x = lum;
      float x2 = x*x;
      float x3 = x2*x;
      float lum_pos, lum_neg;
      vec3 res;
      //whites
      lum_pos = wb*x + wc*x2+ wd*x3 + we*x2*x2 + wf*x2*x3;
      lum_pos = min(lum_pos,1.0-lum);
      lum_neg = wb2*x + wc2*x2+ wd2*x3 + we2*x2*x2 + wf2*x2*x3 + wg2*x3*x3;
      lum_neg = max(lum_neg,-lum);
      res = _Whites>=0.0 ? base*(lum_pos*_Whites+lum)/lum : base * (lum-lum_neg*_Whites)/lum;
      res = clamp(res, 0.0, 1.0);
      //blacks
      lum_pos = bb*x + bc*x2+ bd*x3 + be*x2*x2 + bf*x2*x3 + bg*x3*x3;
      lum_pos = min(lum_pos,1.0-lum);
      lum_neg = lum<=0.23 ? -lum : ba2 + bb2*x + bc2*x2+ bd2*x3 + be2*x2*x2 + bf2*x2*x3 + bg2*x3*x3;
      lum_neg = max(lum_neg,-lum);
      res = _Blacks>=0.0 ? res*(lum_pos*_Blacks+lum)/lum : res * (lum-lum_neg*_Blacks)/lum;
      res = clamp(res, 0.0, 1.0);
      return vec4(wb_SetLum(base, wb_Lum(res)), 1.0);
    }

    //////////////////////////////////////////////////////////////////////////////////////////////
    /// temperature                                                                            ///
    /// _Temperature: -1.0 <=> 1.0, with 0.0 as the default                                    ///
    /// _Tint: -1.0 <=> 1.0, with 0.0 as the default                                           ///
    /// https://gitlab.bestminr.com/bestminr/FrontShaders/blob/master/shaders/temperature.glsl ///
    //////////////////////////////////////////////////////////////////////////////////////////////

    mat3 matRGBtoXYZ = mat3(
      0.4124564390896922, 0.21267285140562253, 0.0193338955823293,
      0.357576077643909, 0.715152155287818, 0.11919202588130297,
      0.18043748326639894, 0.07217499330655958, 0.9503040785363679
    );

    mat3 matXYZtoRGB = mat3(
      3.2404541621141045, -0.9692660305051868, 0.055643430959114726,
      -1.5371385127977166, 1.8760108454466942, -0.2040259135167538,
      -0.498531409556016, 0.041556017530349834, 1.0572251882231791
    );

    mat3 matAdapt = mat3(
      0.8951, -0.7502, 0.0389,
      0.2664, 1.7135, -0.0685,
      -0.1614, 0.0367, 1.0296
    );

    mat3 matAdaptInv = mat3(
      0.9869929054667123, 0.43230526972339456, -0.008528664575177328,
      -0.14705425642099013, 0.5183602715367776, 0.04004282165408487,
      0.15996265166373125, 0.0492912282128556, 0.9684866957875502
    );

    vec3 refWhite, refWhiteRGB;
    vec3 d, s;

    vec3 RGBtoXYZ(vec3 rgb){
      vec3 xyz, XYZ;
      xyz = matRGBtoXYZ * rgb;
      //adaption
      XYZ = matAdapt * xyz;
      XYZ *= d/s;
      xyz = matAdaptInv * XYZ;
      return xyz;
    }

    vec3 XYZtoRGB(vec3 xyz){
      vec3 rgb, RGB;
      //adaption
      RGB = matAdapt * xyz;
      rgb *= s/d;
      xyz = matAdaptInv * RGB;
      rgb = matXYZtoRGB * xyz;
      return rgb;
    }

    float t_Lum(vec3 c){
      return 0.299*c.r + 0.587*c.g + 0.114*c.b;
    }

    vec3 t_ClipColor(vec3 c){
      float l = t_Lum(c);
      float n = min(min(c.r, c.g), c.b);
      float x = max(max(c.r, c.g), c.b);
      if (n < 0.0) c = (c-l)*l / (l-n) + l;
      if (x > 1.0) c = (c-l) * (1.0-l) / (x-l) + l;
      return c;
    }

    vec3 t_SetLum(vec3 c, float l){
      float d = l - t_Lum(c);
      c.r = c.r + d;
      c.g = c.g + d;
      c.b = c.b + d;
      return t_ClipColor(c);
    }

    //illuminants
    //vec3 A = vec3(1.09850, 1.0, 0.35585);
    vec3 D50 = vec3(0.96422, 1.0, 0.82521);
    vec3 D65 = vec3(0.95047, 1.0, 1.08883);
    //vec3 D75 = vec3(0.94972, 1.0, 1.22638);

    //vec3 D50 = vec3(0.981443, 1.0, 0.863177);
    //vec3 D65 = vec3(0.968774, 1.0, 1.121774);

    vec3 CCT2K = vec3(1.274335, 1.0, 0.145233);
    vec3 CCT4K = vec3(1.009802, 1.0, 0.644496);
    vec3 CCT20K = vec3(0.995451, 1.0, 1.886109);

    vec4 doTemperatureTint(vec4 col) {
      vec3 to, from;
      if (_Temperature < 0.0) {
        to = CCT20K;
        from = D65;
      } else {
        to = CCT4K;
        from = D65;
      }
      vec3 base = col.rgb;
      float lum = t_Lum(base);
      //mask by luminance
      float temp = abs(_Temperature) * (1.0 - pow(lum, 2.72));
       //from
      refWhiteRGB = from;
      //to
      refWhite = vec3(mix(from.x, to.x, temp), mix(1.0, 0.9, _Tint), mix(from.z, to.z, temp));
       //mix based on alpha for local adjustments
      refWhite = mix(refWhiteRGB, refWhite, col.a);
      d = matAdapt * refWhite;
      s = matAdapt * refWhiteRGB;
      vec3 xyz = RGBtoXYZ(base);
      vec3 rgb = XYZtoRGB(xyz);
      //brightness compensation
      vec3 res = rgb * (1.0 + (temp + _Tint) / 10.0);
      //preserve luminance
      //vec3 res = t_SetLum(rgb, lum);
      return vec4(mix(base, res, col.a), col.a);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////
    /// Exposure_Gamma                                                                      ///
    /// _Exposure: -1.0 <=> 1.0, with 0.0 as the default                                    ///
    /// _Gamma: -1.0 <=> 1.0, with 0.0 as the default                                       ///
    /// https://gitlab.bestminr.com/bestminr/FrontShaders/blob/master/shaders/exposure.glsl ///
    ///////////////////////////////////////////////////////////////////////////////////////////

    //Luminance and Saturation functions
    //Adapted from: http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/pdf/pdfs/PDF32000_2008.pdf

    float e_Lum(vec3 c){
      return 0.298839*c.r + 0.586811*c.g + 0.11435*c.b;
    }

    vec3 e_ClipColor(vec3 c){
      float l = e_Lum(c);
      float n = min(min(c.r, c.g), c.b);
      float x = max(max(c.r, c.g), c.b);
      if (n < 0.0) c = max((c-l)*l / (l-n) + l, 0.0);
      if (x > 1.0) c = min((c-l) * (1.0-l) / (x-l) + l, 1.0);
      return c;
    }

    vec3 e_SetLum(vec3 c, float l){
      c += l - e_Lum(c);
      return e_ClipColor(c);
    }

    float Sat(vec3 c){
      float n = min(min(c.r, c.g), c.b);
      float x = max(max(c.r, c.g), c.b);
      return x - n;
    }

    vec3 SetSat(vec3 c, float s){
      float cmin = min(min(c.r, c.g), c.b);
      float cmax = max(max(c.r, c.g), c.b);
      vec3 res = vec3(0.0);
      if (cmax > cmin) {
        if (c.r == cmin && c.b == cmax) { // R min G mid B max
          res.r = 0.0;
          res.g = ((c.g-cmin)*s) / (cmax-cmin);
          res.b = s;
        }
        else if (c.r == cmin && c.g == cmax) { // R min B mid G max
          res.r = 0.0;
          res.b = ((c.b-cmin)*s) / (cmax-cmin);
          res.g = s;
        }
        else if (c.g == cmin && c.b == cmax) { // G min R mid B max
          res.g = 0.0;
          res.r = ((c.r-cmin)*s) / (cmax-cmin);
          res.b = s;
        }
        else if (c.g == cmin && c.r == cmax) { // G min B mid R max
          res.g = 0.0;
          res.b = ((c.b-cmin)*s) / (cmax-cmin);
          res.r = s;
        }
        else if (c.b == cmin && c.r == cmax) { // B min G mid R max
          res.b = 0.0;
          res.g = ((c.g-cmin)*s) / (cmax-cmin);
          res.r = s;
        }
        else { // B min R mid G max
          res.b = 0.0;
          res.r = ((c.r-cmin)*s) / (cmax-cmin);
          res.g = s;
        }
      }
      return res;
    }

    //mat3 sRGB2XYZ = mat3(
    //  0.4360747,  0.3850649,  0.1430804,
    //  0.2225045,  0.7168786,  0.0606169,
    //  0.0139322,  0.0971045,  0.7141733
    //);

    //mat3 XYZ2sRGB = mat3(
    //  3.1338561, -1.6168667, -0.4906146,
    //  -0.9787684,  1.9161415,  0.0334540,
    //  0.0719453, -0.2289914,  1.4052427
    //);

    //mat3 ROMM2XYZ = mat3(
    //  0.7976749,  0.1351917,  0.0313534,
    //  0.2880402,  0.7118741,  0.0000857,
    //  0.0000000,  0.0000000,  0.8252100
    //);

    //mat3 XYZ2ROMM = mat3(
    //  1.3459433, -0.2556075, -0.0511118,
    //  -0.5445989,  1.5081673,  0.0205351,
    //  0.0000000,  0.0000000,  1.2118128
    //);

    float ramp(float t){
      t *= 2.0;
      if (t >= 1.0) {
        t -= 1.0;
        t = log(0.5) / log(0.5*(1.0-t) + 0.9332*t);
      }
      return clamp(t, 0.001, 10.0);
    }

    vec4 doExposureGamma(vec4 col) {
      vec3 base = col.rgb;
      vec3 res, blend;
      //base = base * sRGB2XYZ * XYZ2ROMM;
      float amt = mix(0.009, 0.98, _Exposure);
      if (amt < 0.0) {
        res = mix(vec3(0.0), base, amt + 1.0);
        blend = mix(base, vec3(0.0), amt + 1.0);
        res = min(res / (1.0 - blend*0.9), 1.0);
      } else {
        res = mix(base, vec3(1.0), amt);
        blend = mix(vec3(1.0), pow(base, vec3(1.0/0.7)), amt);
        res = max(1.0 - ((1.0 - res) / blend), 0.0);
      }
      res = pow(e_SetLum(SetSat(base, Sat(res)), e_Lum(res)), vec3(ramp(1.0 - (_Gamma + 1.0) / 2.0)));
      //res = res * ROMM2XYZ * XYZ2sRGB;
      return vec4(mix(base, res, col.a), col.a);
    }

    ////////////
    /// Main ///
    ////////////

    void main() {

      %s

      const float eps = 0.001;

      if ((abs(_Temperature) > eps) || (abs(_Tint) > eps)) { result = doTemperatureTint(result); }
      if ((abs(_Exposure) > eps) || (abs(_Gamma) > eps)) { result = doExposureGamma(result); }
      if (abs(_Contrast) > eps) { result = doContrast(result); }
      if ((abs(_Whites) > eps) || (abs(_Blacks) > eps)) { result = doWhitesBlacks(result); }
      if ((abs(_Saturation) > eps) || (abs(_Vibrance) > eps)) { result = doSaturationVibrance(result); }

      %s

      gl_FragColor = result;

    }
    ''';

  ALColorAdjustMSL = '''
    using namespace metal;

    struct ProjectedVertex {
      float4 position [[position]];
      float2 textureCoord;
      float4 color;
      float pointSize [[point_size]];
    };

    /////////////////////////////////////////////////////////////////////////////////////////////
    /// Contrast                                                                              ///
    /// Its not really contrast, I will say its "smart" contrast                              ///
    /// _Contrast: The adjusted contrast (-1.0 <=> 1.0, with 0.0 as the default)              ///
    /// https://gitlab.bestminr.com/bestminr/FrontShaders/blob/master/shaders/contrast.glsl   ///
    /////////////////////////////////////////////////////////////////////////////////////////////

    inline float BlendOverlayf(float base, float blend) {
        return (base < 0.5) ? (2.0 * base * blend) : (1.0 - 2.0 * (1.0 - base) * (1.0 - blend));
    }

    inline float3 BlendOverlay(float3 base, float3 blend) {
        return float3(BlendOverlayf(base.r, blend.r),
                      BlendOverlayf(base.g, blend.g),
                      BlendOverlayf(base.b, blend.b));
    }

    constant float3x3 sRGB2XYZ = float3x3(
        0.4360747,  0.3850649,  0.1430804,
        0.2225045,  0.7168786,  0.0606169,
        0.0139322,  0.0971045,  0.7141733
    );

    constant float3x3 XYZ2sRGB = float3x3(
        3.1338561, -1.6168667, -0.4906146,
       -0.9787684,  1.9161415,  0.0334540,
        0.0719453, -0.2289914,  1.4052427
    );

    constant float3x3 ROMM2XYZ = float3x3(
        0.7976749,  0.1351917,  0.0313534,
        0.2880402,  0.7118741,  0.0000857,
        0.0000000,  0.0000000,  0.8252100
    );

    constant float3x3 XYZ2ROMM = float3x3(
        1.3459433, -0.2556075, -0.0511118,
       -0.5445989,  1.5081673,  0.0205351,
        0.0000000,  0.0000000,  1.2118128
    );

    float4 doContrast(float4 col, float Contrast) {
        float amount = (Contrast < 0.0) ? Contrast : Contrast * 2.0;
        float3 base = (col.rgb * sRGB2XYZ) * XYZ2ROMM;
        float3 overlay = mix(float3(0.5), base, amount * col.a);
        float3 res = (BlendOverlay(base, overlay) * ROMM2XYZ) * XYZ2sRGB;
        return float4(res, col.a);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////
    /// Highlights                                                                            ///
    /// _Highlights: 0 <=> 1.0, with 0.0 as the default                                       ///
    /// https://gitlab.bestminr.com/bestminr/FrontShaders/blob/master/shaders/highlights.glsl ///
    /////////////////////////////////////////////////////////////////////////////////////////////

    //float4 doHighlights(float4 color) {
    //    const float a = 0.1357697966704323;
    //    const float b = 1.006045552016985;
    //    const float c = 0.4674339906510876;
    //    const float d = 0.8029414702292208;
    //    const float e = 0.1127806558508491;
    //    float maxx = max(color.r, max(color.g, color.b));
    //    float minx = min(color.r, min(color.g, color.b));
    //    float lum = (maxx + minx) / 2.0;
    //    float x1 = fabs(u._Highlights); // Note: _Highlights uniform not declared in Uniforms.
    //    float x2 = lum;
    //    float lum_new = (lum < 0.5) ? lum : lum + a * sign(u._Highlights) * exp(-0.5 * (pow((x1-b)/c, 2.0) + pow((x2-d)/e, 2.0)));
    //    return float4(color.rgb * lum_new / lum, color.a);
    //}

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    /// saturation_vibrance                                                                            ///
    /// _Saturation: -1.0 <=> 1.0, with 0.0 as the default                                             ///
    /// _Vibrance: -1.0 <=> 1.0, with 0.0 as the default                                               ///
    /// https://gitlab.bestminr.com/bestminr/FrontShaders/blob/master/shaders/saturation_vibrance.glsl ///
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    float4 doSaturationVibrance(float4 col, float Saturation, float Vibrance) {
        float3 color = col.rgb;
        float luminance = color.r * 0.299 + color.g * 0.587 + color.b * 0.114;
        float mn = min(min(color.r, color.g), color.b);
        float mx = max(max(color.r, color.g), color.b);
        float sat = (1.0 - (mx - mn)) * (1.0 - mx) * luminance * 5.0;
        float3 lightness = float3((mn + mx) / 2.0);
        // vibrance
        color = mix(color, mix(color, lightness, -Vibrance), sat);
        // negative vibrance
        color = mix(color, lightness, (1.0 - lightness) * (1.0 - Vibrance) / 2.0 * fabs(Vibrance));
        // saturation
        color = mix(color, float3(luminance), -Saturation);
        return float4(mix(col.rgb, color, col.a), col.a);
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// whites_blacks                                                                            ///
    /// _Whites: -100 <=> 100 but I limit to -1 <=> 1, with 0.0 as the default                   ///
    /// _Blacks: -100 <=> 100 but I limit to -1 <=> 1, with 0.0 as the default                   ///
    /// https://gitlab.bestminr.com/bestminr/FrontShaders/blob/master/shaders/whites_blacks.glsl ///
    ////////////////////////////////////////////////////////////////////////////////////////////////

    //wb_Luminance and Saturation functions
    //Adapted from: http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/pdf/pdfs/PDF32000_2008.pdf

    float wb_Lum(float3 c) {
        return 0.299 * c.r + 0.587 * c.g + 0.114 * c.b;
    }

    float3 wb_ClipColor(float3 c) {
        float l = wb_Lum(c);
        float n = min(min(c.r, c.g), c.b);
        float x = max(max(c.r, c.g), c.b);
        if (n < 0.0) { c = max((c - l) * l / (l - n) + l, 0.0); }
        if (x > 1.0) { c = min((c - l) * (1.0 - l) / (x - l) + l, 1.0); }
        return c;
    }

    float3 wb_SetLum(float3 c, float l) {
        c += l - wb_Lum(c);
        return wb_ClipColor(c);
    }

    float4 doWhitesBlacks(float4 color, float Whites, float Blacks) {
        float3 base = color.rgb;
        float maxx = max(max(base.r, base.g), base.b);
        float minx = min(min(base.r, base.g), base.b);
        float lum = (maxx + minx) / 2.0;
        float x = lum;
        float x2 = x * x;
        float x3 = x2 * x;
        float lum_pos, lum_neg;
        float3 res;
        // whites constants
        const float wb = 5.336778471840789e-03;
        const float wc = 6.664243592410049e-01;
        const float wd = 3.023761372137289e+00;
        const float we = -6.994413182098681e+00;
        const float wf = 3.293987131616894e+00;
        const float wb2 = -1.881032803339283e-01;
        const float wc2 = 2.812945435181010e+00;
        const float wd2 = -1.495096839176419e+01;
        const float we2 = 3.349416467551858e+01;
        const float wf2 = -3.433024909629221e+01;
        const float wg2 = 1.314308200442166e+01;
        // blacks constants
        const float bb = 8.376727344831676e-01;
        const float bc = -3.418495999327269e+00;
        const float bd = 8.078054837335609e+00;
        const float be = -1.209938703324099e+01;
        const float bf = 9.520315785756406e+00;
        const float bg = -2.919340722745241e+00;
        const float ba2 = 5.088652898054800e-01;
        const float bb2 = -9.767371127415029e+00;
        const float bc2 = 4.910705739925203e+01;
        const float bd2 = -1.212150899746360e+02;
        const float be2 = 1.606205314047741e+02;
        const float bf2 = -1.085660871669277e+02;
        const float bg2 = 2.931582214601388e+01;

        // whites
        lum_pos = wb * x + wc * x2 + wd * x3 + we * (x2 * x2) + wf * (x2 * x3);
        lum_pos = min(lum_pos, 1.0 - lum);
        lum_neg = wb2 * x + wc2 * x2 + wd2 * x3 + we2 * (x2 * x2) + wf2 * (x2 * x3) + wg2 * (x3 * x3);
        lum_neg = max(lum_neg, -lum);
        res = (Whites >= 0.0) ? base * ((lum_pos * Whites + lum) / lum)
                              : base * ((lum - lum_neg * Whites) / lum);
        res = clamp(res, 0.0, 1.0);
        // blacks
        lum_pos = bb * x + bc * x2 + bd * x3 + be * (x2 * x2) + bf * (x2 * x3) + bg * (x3 * x3);
        lum_pos = min(lum_pos, 1.0 - lum);
        lum_neg = (lum <= 0.23) ? -lum
                                : (ba2 + bb2 * x + bc2 * x2 + bd2 * x3 + be2 * (x2 * x2) + bf2 * (x2 * x3) + bg2 * (x3 * x3));
        lum_neg = max(lum_neg, -lum);
        res = (Blacks >= 0.0) ? res * ((lum_pos * Blacks + lum) / lum)
                              : res * ((lum - lum_neg * Blacks) / lum);
        res = clamp(res, 0.0, 1.0);
        return float4(wb_SetLum(base, wb_Lum(res)), 1.0);
    }

    //////////////////////////////////////////////////////////////////////////////////////////////
    /// temperature                                                                            ///
    /// _Temperature: -1.0 <=> 1.0, with 0.0 as the default                                    ///
    /// _Tint: -1.0 <=> 1.0, with 0.0 as the default                                           ///
    /// https://gitlab.bestminr.com/bestminr/FrontShaders/blob/master/shaders/temperature.glsl ///
    //////////////////////////////////////////////////////////////////////////////////////////////

    constant float3x3 matRGBtoXYZ = float3x3(
        0.4124564390896922, 0.21267285140562253, 0.0193338955823293,
        0.357576077643909,   0.715152155287818,  0.11919202588130297,
        0.18043748326639894, 0.07217499330655958, 0.9503040785363679
    );

    constant float3x3 matXYZtoRGB = float3x3(
        3.2404541621141045, -0.9692660305051868,  0.055643430959114726,
       -1.5371385127977166,  1.8760108454466942, -0.2040259135167538,
       -0.498531409556016,    0.041556017530349834, 1.0572251882231791
    );

    constant float3x3 matAdapt = float3x3(
        0.8951, -0.7502, 0.0389,
        0.2664,  1.7135, -0.0685,
       -0.1614,  0.0367, 1.0296
    );

    constant float3x3 matAdaptInv = float3x3(
        0.9869929054667123,  0.43230526972339456, -0.008528664575177328,
       -0.14705425642099013,  0.5183602715367776,   0.04004282165408487,
        0.15996265166373125,  0.0492912282128556,   0.9684866957875502
    );

    inline float t_Lum(float3 c) {
        return 0.299 * c.r + 0.587 * c.g + 0.114 * c.b;
    }

    inline float3 t_ClipColor(float3 c) {
        float l = t_Lum(c);
        float n = min(min(c.r, c.g), c.b);
        float x = max(max(c.r, c.g), c.b);
        if (n < 0.0) { c = (c - l) * l / (l - n) + l; }
        if (x > 1.0) { c = (c - l) * (1.0 - l) / (x - l) + l; }
        return c;
    }

    inline float3 t_SetLum(float3 c, float l) {
        float d = l - t_Lum(c);
        c += d;
        return t_ClipColor(c);
    }

    // Illuminants and CCT values
    constant float3 D65 = float3(0.95047, 1.0, 1.08883);
    constant float3 CCT4K = float3(1.009802, 1.0, 0.644496);
    constant float3 CCT20K = float3(0.995451, 1.0, 1.886109);

    float4 doTemperatureTint(float4 col, float Temperature, float Tint) {
        float3 from = D65;
        float3 to = (Temperature < 0.0) ? CCT20K : CCT4K;
        float3 base = col.rgb;
        float lum = t_Lum(base);
        // mask by luminance
        float temp = fabs(Temperature) * (1.0 - pow(lum, 2.72));
        // mix illuminants based on temperature and tint
        float3 refWhiteRGB = from;
        float3 refWhite = float3(mix(from.x, to.x, temp),
                                 mix(1.0, 0.9, Tint),
                                 mix(from.z, to.z, temp));
        refWhite = mix(refWhiteRGB, refWhite, col.a);
        float3 d = matAdapt * refWhite;
        float3 s = matAdapt * refWhiteRGB;

        // Convert base from RGB to XYZ with adaptation
        float3 xyz = matRGBtoXYZ * base;
        xyz = matAdapt * xyz;
        xyz = xyz * (d / s);
        xyz = matAdaptInv * xyz;
        // Convert back to RGB
        float3 rgb = matXYZtoRGB * xyz;

        // brightness compensation
        float3 res = rgb * (1.0 + (temp + Tint) / 10.0);
        // Optionally preserve luminance:
        // res = t_SetLum(rgb, lum);
        return float4(mix(base, res, col.a), col.a);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////
    /// Exposure_Gamma                                                                      ///
    /// _Exposure: -1.0 <=> 1.0, with 0.0 as the default                                    ///
    /// _Gamma: -1.0 <=> 1.0, with 0.0 as the default                                       ///
    /// https://gitlab.bestminr.com/bestminr/FrontShaders/blob/master/shaders/exposure.glsl ///
    ///////////////////////////////////////////////////////////////////////////////////////////

    //Luminance and Saturation functions
    //Adapted from: http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/pdf/pdfs/PDF32000_2008.pdf

    inline float e_Lum(float3 c) {
        return 0.298839 * c.r + 0.586811 * c.g + 0.11435 * c.b;
    }

    inline float3 e_ClipColor(float3 c) {
        float l = e_Lum(c);
        float n = min(min(c.r, c.g), c.b);
        float x = max(max(c.r, c.g), c.b);
        if (n < 0.0) { c = max((c - l) * l / (l - n) + l, 0.0); }
        if (x > 1.0) { c = min((c - l) * (1.0 - l) / (x - l) + l, 1.0); }
        return c;
    }

    inline float3 e_SetLum(float3 c, float l) {
        c += l - e_Lum(c);
        return e_ClipColor(c);
    }

    inline float Sat(float3 c) {
        float n = min(min(c.r, c.g), c.b);
        float x = max(max(c.r, c.g), c.b);
        return x - n;
    }

    float3 SetSat(float3 c, float s) {
        float cmin = min(min(c.r, c.g), c.b);
        float cmax = max(max(c.r, c.g), c.b);
        float3 res = float3(0.0);
        if (cmax > cmin) {
            if ((c.r == cmin) && (c.b == cmax)) {
                res.r = 0.0;
                res.g = ((c.g - cmin) * s) / (cmax - cmin);
                res.b = s;
            } else if ((c.r == cmin) && (c.g == cmax)) {
                res.r = 0.0;
                res.b = ((c.b - cmin) * s) / (cmax - cmin);
                res.g = s;
            } else if ((c.g == cmin) && (c.b == cmax)) {
                res.g = 0.0;
                res.r = ((c.r - cmin) * s) / (cmax - cmin);
                res.b = s;
            } else if ((c.g == cmin) && (c.r == cmax)) {
                res.g = 0.0;
                res.b = ((c.b - cmin) * s) / (cmax - cmin);
                res.r = s;
            } else if ((c.b == cmin) && (c.r == cmax)) {
                res.b = 0.0;
                res.g = ((c.g - cmin) * s) / (cmax - cmin);
                res.r = s;
            } else {
                res.b = 0.0;
                res.r = ((c.r - cmin) * s) / (cmax - cmin);
                res.g = s;
            }
        }
        return res;
    }

    inline float ramp(float t) {
        t *= 2.0;
        if (t >= 1.0) {
            t -= 1.0;
            t = log(0.5) / log(0.5 * (1.0 - t) + 0.9332 * t);
        }
        return clamp(t, 0.001, 10.0);
    }

    float4 doExposureGamma(float4 col, float Exposure, float Gamma) {
        float3 base = col.rgb;
        float3 res, blend;
        float amt = mix(0.009, 0.98, Exposure);
        if (amt < 0.0) {
            res = mix(float3(0.0), base, amt + 1.0);
            blend = mix(base, float3(0.0), amt + 1.0);
            res = min(res / (1.0 - blend * 0.9), float3(1.0));
        } else {
            res = mix(base, float3(1.0), amt);
            blend = mix(float3(1.0), pow(base, float3(1.0 / 0.7)), amt);
            res = max(1.0 - ((1.0 - res) / blend), float3(0.0));
        }
        res = pow(e_SetLum(SetSat(base, Sat(res)), e_Lum(res)), float3(ramp(1.0 - (Gamma + 1.0) / 2.0)));
        return float4(mix(base, res, col.a), col.a);
    }

    //////////////////////
    /// fragmentShader ///
    //////////////////////

    fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],
                                   constant float &Contrast [[buffer(0)]],
                                   constant float &Saturation [[buffer(1)]],
                                   constant float &Vibrance [[buffer(2)]],
                                   constant float &Whites [[buffer(3)]],
                                   constant float &Blacks [[buffer(4)]],
                                   constant float &Temperature [[buffer(5)]],
                                   constant float &Tint [[buffer(6)]],
                                   constant float &Exposure [[buffer(7)]],
                                   constant float &Gamma [[buffer(8)]],
                                   %s) {
      %s

      float eps = 0.001;
      if ((fabs(Temperature) > eps) || (fabs(Tint) > eps)) { result = doTemperatureTint(result, Temperature, Tint); }
      if ((fabs(Exposure) > eps) || (fabs(Gamma) > eps)) { result = doExposureGamma(result, Exposure, Gamma); }
      if (fabs(Contrast) > eps) { result = doContrast(result, Contrast); }
      if ((fabs(Whites) > eps) || (fabs(Blacks) > eps)) { result = doWhitesBlacks(result, Whites, Blacks); }
      if ((fabs(Saturation) > eps) || (fabs(Vibrance) > eps)) { result = doSaturationVibrance(result, Saturation, Vibrance); }

      %s
      return result;
    }
    ''';

Type

  TALColorAdjustShaderVariables = class(TObject)
  private
    fContrast: Single;
    fSaturation: Single;
    fVibrance: Single;
    fWhites: Single;
    fBlacks: Single;
    fTemperature: Single;
    fTint: Single;
    fExposure: Single;
    fGamma: Single;
  public
    const defContrast: Single    = 0;
    const defSaturation: Single  = 0;
    const defVibrance: Single    = 0;
    const defWhites: Single      = 0;
    const defBlacks: Single      = 0;
    const defTemperature: Single = 0;
    const defTint: Single        = 0;
    const defExposure: Single    = 0;
    const defGamma: Single       = 0;
  public
    constructor Create; virtual;
    procedure UpdateContext(const Context: TContext3D);
  public
    // The contrast multiplier.
    property Contrast: Single read fContrast write fContrast;
    // The Saturation offset.
    property Saturation: Single read fSaturation write fSaturation;
    // The Vibrance offset.
    property Vibrance: Single read fVibrance write fVibrance;
    // The Whites offset.
    property Whites: Single read fWhites write fWhites;
    // The Blacks offset.
    property Blacks: Single read fBlacks write fBlacks;
    // The Temperature offset.
    property Temperature: Single read fTemperature write fTemperature;
    // The Tint offset.
    property Tint: Single read fTint write fTint;
    // The Exposure offset.
    property Exposure: Single read fExposure write fExposure;
    // The Gamma offset.
    property Gamma: Single read fGamma write fGamma;
  end;

  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Filter.Effects.TFilterBaseFilter still has the exact same fields and adjust the IFDEF'}
  {$ENDIF}
  TALFilterBaseFilterAccessPrivate = class(TFmxObject)
  protected
    FFilter: TFilter; // << need to access this private field
    FInputFilter: TFilterBaseFilter;
  end;

  TALFilterBaseFilter = class(TFilterBaseFilter)
  protected
    function filter: TFilter;
  end;

  [ComponentPlatforms($FFFF)]
  TALColorAdjustEffect = class(TImageFXEffect)
  private
    function GetContrast: Single;
    procedure SetContrast(AValue: Single);
    function GetSaturation: Single;
    procedure SetSaturation(AValue: Single);
    function GetVibrance: Single;
    procedure SetVibrance(AValue: Single);
    function GetWhites: Single;
    procedure SetWhites(AValue: Single);
    function GetBlacks: Single;
    procedure SetBlacks(AValue: Single);
    function GetTemperature: Single;
    procedure SetTemperature(AValue: Single);
    function GetTint: Single;
    procedure SetTint(AValue: Single);
    function GetExposure: Single;
    procedure SetExposure(AValue: Single);
    function GetGamma: Single;
    procedure SetGamma(AValue: Single);
  public
    procedure AssignTo(Dest: TPersistent); override;
  published
    // The contrast multiplier.
    property Contrast: Single read GetContrast write SetContrast;
    // The Saturation offset.
    property Saturation: Single read GetSaturation write SetSaturation;
    // The Vibrance offset.
    property Vibrance: Single read GetVibrance write SetVibrance;
    // The Whites offset.
    property Whites: Single read GetWhites write SetWhites;
    // The Blacks offset.
    property Blacks: Single read GetBlacks write SetBlacks;
    // The Temperature offset.
    property Temperature: Single read GetTemperature write SetTemperature;
    // The Tint offset.
    property Tint: Single read GetTint write SetTint;
    // The Exposure offset.
    property Exposure: Single read GetExposure write SetExposure;
    // The Gamma offset.
    property Gamma: Single read GetGamma write SetGamma;
  end;

procedure Register;

implementation

uses
  system.sysutils,
  System.Math.Vectors,
  Alcinoe.StringUtils;

{***********************************************}
constructor TALColorAdjustShaderVariables.Create;
begin
  fContrast := defContrast;
  fSaturation := defSaturation;
  fVibrance := defVibrance;
  fWhites := defWhites;
  fBlacks := defBlacks;
  fTemperature := defTemperature;
  fTint := defTint;
  fExposure := defExposure;
  fGamma := defGamma;
  inherited create;
end;

{*******************************************************************************}
procedure TALColorAdjustShaderVariables.UpdateContext(const Context: TContext3D);
begin
  Context.SetShaderVariable('Contrast',    [Vector3D(Contrast, 0, 0, 0)]);
  Context.SetShaderVariable('Saturation',  [Vector3D(Saturation, 0, 0, 0)]);
  Context.SetShaderVariable('Vibrance',    [Vector3D(Vibrance, 0, 0, 0)]);
  Context.SetShaderVariable('Whites',      [Vector3D(Whites, 0, 0, 0)]);
  Context.SetShaderVariable('Blacks',      [Vector3D(Blacks, 0, 0, 0)]);
  Context.SetShaderVariable('Temperature', [Vector3D(Temperature, 0, 0, 0)]);
  Context.SetShaderVariable('Tint',        [Vector3D(Tint, 0, 0, 0)]);
  Context.SetShaderVariable('Exposure',    [Vector3D(Exposure, 0, 0, 0)]);
  Context.SetShaderVariable('Gamma',       [Vector3D(Gamma, 0, 0, 0)]);
end;

type

  {***********************************}
  TALColorAdjustFilter = class(TFilter)
  public
    constructor Create; override;
    class function FilterAttr: TFilterRec; override;
  end;

{**************************************}
constructor TALColorAdjustFilter.Create;
begin
  inherited;
  FShaders[0] := TShaderManager.RegisterShaderFromData('ColorAdjust.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.SKSL'}
    // Todo- it's the Monochrome Filter right now
    TContextShaderSource.Create(TContextShaderArch.SKSL, [
      $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, $20, $49, $6E, $70, $75, $74, $3B, $68, $61, $6C, $66, $34, $20, $6D, $61, $69, $6E, $28, $66, $6C, $6F, $61, $74, $32, $20, $61,
      $29, $7B, $66, $6C, $6F, $61, $74, $34, $20, $62, $3D, $66, $6C, $6F, $61, $74, $34, $28, $49, $6E, $70, $75, $74, $2E, $65, $76, $61, $6C, $28, $61, $29, $29, $3B, $66, $6C, $6F, $61, $74, $20, $63,
      $3D, $64, $6F, $74, $28, $62, $2E, $78, $79, $7A, $2C, $66, $6C, $6F, $61, $74, $33, $28, $2E, $32, $31, $32, $36, $2C, $2E, $37, $31, $35, $32, $2C, $2E, $30, $37, $32, $32, $29, $29, $3B, $72, $65,
      $74, $75, $72, $6E, $20, $68, $61, $6C, $66, $34, $28, $66, $6C, $6F, $61, $74, $34, $28, $63, $2C, $63, $2C, $63, $2C, $62, $2E, $77, $29, $29, $3B, $7D, $0A, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX9'}
    {$IF defined(MSWindows)}
    // Todo- it's the Monochrome Filter right now
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $1E, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $4F, $00, $00, $00, $00, $02, $FF, $FF, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $48, $00, $00, $00,
      $30, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $38, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00,
      $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F,
      $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $00, $00, $0F, $A0, $D0, $B3, $59, $3E, $59, $17, $37, $3F, $98, $DD, $93, $3D, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80,
      $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $B0, $00, $08, $E4, $A0, $08, $00, $00, $03, $00, $00, $07, $80,
      $00, $00, $E4, $80, $00, $00, $E4, $A0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX11_level_9'}
    {$IF defined(MSWindows)}
    // Todo- it's the Monochrome Filter right now
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $A5, $8F, $0E, $66, $FC, $0B, $67, $65, $5F, $69, $4D, $3C, $70, $E6, $A6, $8F, $01, $00, $00, $00, $FC, $02, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $CC, $00, $00, $00,
      $7C, $01, $00, $00, $F8, $01, $00, $00, $94, $02, $00, $00, $C8, $02, $00, $00, $41, $6F, $6E, $39, $8C, $00, $00, $00, $8C, $00, $00, $00, $00, $02, $FF, $FF, $64, $00, $00, $00, $28, $00, $00, $00,
      $00, $00, $28, $00, $00, $00, $28, $00, $00, $00, $28, $00, $01, $00, $24, $00, $00, $00, $28, $00, $00, $00, $00, $00, $00, $02, $FF, $FF, $51, $00, $00, $05, $00, $00, $0F, $A0, $D0, $B3, $59, $3E,
      $59, $17, $37, $3F, $98, $DD, $93, $3D, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $42, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $E4, $B0, $00, $08, $E4, $A0, $08, $00, $00, $03, $00, $00, $07, $80, $00, $00, $E4, $80, $00, $00, $E4, $A0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80,
      $FF, $FF, $00, $00, $53, $48, $44, $52, $A8, $00, $00, $00, $40, $00, $00, $00, $2A, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00,
      $00, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00, $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00, $68, $00, $00, $02, $01, $00, $00, $00,
      $45, $00, $00, $09, $F2, $00, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $C6, $79, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $10, $00, $00, $0A,
      $12, $00, $10, $00, $00, $00, $00, $00, $86, $03, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $D0, $B3, $59, $3E, $59, $17, $37, $3F, $98, $DD, $93, $3D, $00, $00, $00, $00, $36, $00, $00, $05,
      $F2, $20, $10, $00, $00, $00, $00, $00, $06, $04, $10, $00, $00, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $04, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00,
      $02, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46, $94, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $02, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF, $00, $11, $00, $00, $62, $00, $00, $00, $5C, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00,
      $0C, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70,
      $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $49, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $03, $03, $00, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00,
      $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65,
      $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        ALFormatW(
          ALColorAdjustMSL,
          ['const texture2d<float> Input [[texture(0)]],'+
           'const sampler InputSampler [[sampler(0)]]',
           //--
           'float4 result = Input.sample(InputSampler, in.textureCoord);',
           //--
           ''],
          ALDefaultFormatSettingsW)),
      [TContextShaderVariable.Create('Contrast',    TContextShaderVariableKind.Float, 0, 1),
       TContextShaderVariable.Create('Saturation',  TContextShaderVariableKind.Float, 1, 1),
       TContextShaderVariable.Create('Vibrance',    TContextShaderVariableKind.Float, 2, 1),
       TContextShaderVariable.Create('Whites',      TContextShaderVariableKind.Float, 3, 1),
       TContextShaderVariable.Create('Blacks',      TContextShaderVariableKind.Float, 4, 1),
       TContextShaderVariable.Create('Temperature', TContextShaderVariableKind.Float, 5, 1),
       TContextShaderVariable.Create('Tint',        TContextShaderVariableKind.Float, 6, 1),
       TContextShaderVariable.Create('Exposure',    TContextShaderVariableKind.Float, 7, 1),
       TContextShaderVariable.Create('Gamma',       TContextShaderVariableKind.Float, 8, 1),
       TContextShaderVariable.Create('Input',       TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    TContextShaderSource.Create(
      TContextShaderArch.GLSL,
      TEncoding.UTF8.GetBytes(
        ALFormatW(
          ALColorAdjustGLSL,
          ['varying vec4 TEX0;'+
           'uniform sampler2D _Input;',
           //--
           'vec4 result = texture2D(_Input, TEX0.xy);',
           //--
           ''],
          ALDefaultFormatSettingsW)),
      [TContextShaderVariable.Create('Contrast',    TContextShaderVariableKind.Float, 0, 1),
       TContextShaderVariable.Create('Saturation',  TContextShaderVariableKind.Float, 1, 1),
       TContextShaderVariable.Create('Vibrance',    TContextShaderVariableKind.Float, 2, 1),
       TContextShaderVariable.Create('Whites',      TContextShaderVariableKind.Float, 3, 1),
       TContextShaderVariable.Create('Blacks',      TContextShaderVariableKind.Float, 4, 1),
       TContextShaderVariable.Create('Temperature', TContextShaderVariableKind.Float, 5, 1),
       TContextShaderVariable.Create('Tint',        TContextShaderVariableKind.Float, 6, 1),
       TContextShaderVariable.Create('Exposure',    TContextShaderVariableKind.Float, 7, 1),
       TContextShaderVariable.Create('Gamma',       TContextShaderVariableKind.Float, 8, 1),
       TContextShaderVariable.Create('Input',       TContextShaderVariableKind.Texture, 0, 0)]
    )
    {$ENDREGION}

  ]);
end;

{*********************************************************}
class function TALColorAdjustFilter.FilterAttr: TFilterRec;
begin
  Result := TFilterRec.Create('ALColorAdjust', 'Color adjustments operations.', [
    TFilterValueRec.Create('Contrast',    'The contrast multiplier.', TALColorAdjustShaderVariables.defContrast,    -1,  1),
    TFilterValueRec.Create('Whites',      'The whites offset.',       TALColorAdjustShaderVariables.defWhites,      -1,  1),
    TFilterValueRec.Create('Blacks',      'The blacks offset.',       TALColorAdjustShaderVariables.defBlacks,      -1,  1),
    TFilterValueRec.Create('Exposure',    'The exposure offset.',     TALColorAdjustShaderVariables.defExposure,    -1,  1),
    TFilterValueRec.Create('Gamma',       'The gamma offset.',        TALColorAdjustShaderVariables.defGamma,       -1,  1),
    TFilterValueRec.Create('Saturation',  'The saturation offset.',   TALColorAdjustShaderVariables.defSaturation,  -1,  1),
    TFilterValueRec.Create('Vibrance',    'The vibrance offset.',     TALColorAdjustShaderVariables.defVibrance,    -1,  1),
    TFilterValueRec.Create('Temperature', 'The temperature offset.',  TALColorAdjustShaderVariables.defTemperature, -1,  1),
    TFilterValueRec.Create('Tint',        'The tint offset.',         TALColorAdjustShaderVariables.defTint,        -1,  1)]
  );
end;

{*******************************************}
function TALFilterBaseFilter.filter: TFilter;
begin
  result := TALFilterBaseFilterAccessPrivate(self).FFilter;
end;

{*********************************************************}
procedure TALColorAdjustEffect.AssignTo(Dest: TPersistent);
begin
  if Dest is TALColorAdjustEffect then
    with TALColorAdjustEffect(Dest) do begin
      Contrast := self.Contrast;
      Saturation := self.Saturation;
      Vibrance := self.Vibrance;
      Whites := self.Whites;
      Blacks := self.Blacks;
      Temperature := self.Temperature;
      Tint := self.Tint;
      Exposure := self.Exposure;
      Gamma := self.Gamma;
    end
    else inherited AssignTo(Dest);
end;

{************************************************}
function TALColorAdjustEffect.GetContrast: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Contrast']
  else
    Result := 0.0;
end;

{**************************************************}
function TALColorAdjustEffect.GetSaturation: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Saturation']
  else
    Result := 0.0;
end;

{************************************************}
function TALColorAdjustEffect.GetVibrance: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Vibrance']
  else
    Result := 0.0;
end;

{**********************************************}
function TALColorAdjustEffect.GetWhites: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Whites']
  else
    Result := 0.0;
end;

{**********************************************}
function TALColorAdjustEffect.GetBlacks: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Blacks']
  else
    Result := 0.0;
end;

{***************************************************}
function TALColorAdjustEffect.GetTemperature: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Temperature']
  else
    Result := 0.0;
end;

{********************************************}
function TALColorAdjustEffect.GetTint: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Tint']
  else
    Result := 0.0;
end;

{************************************************}
function TALColorAdjustEffect.GetExposure: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Exposure']
  else
    Result := 0.0;
end;

{*********************************************}
function TALColorAdjustEffect.GetGamma: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Gamma']
  else
    Result := 0.0;
end;

{*********************************************************}
procedure TALColorAdjustEffect.SetContrast(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Contrast'] <> AValue then
  begin
    Filter.ValuesAsFloat['Contrast'] := AValue;
    UpdateParentEffects;
  end;
end;

{***********************************************************}
procedure TALColorAdjustEffect.SetSaturation(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Saturation'] <> AValue then
  begin
    Filter.ValuesAsFloat['Saturation'] := AValue;
    UpdateParentEffects;
  end;
end;

{*********************************************************}
procedure TALColorAdjustEffect.SetVibrance(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Vibrance'] <> AValue then
  begin
    Filter.ValuesAsFloat['Vibrance'] := AValue;
    UpdateParentEffects;
  end;
end;

{*******************************************************}
procedure TALColorAdjustEffect.SetWhites(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Whites'] <> AValue then
  begin
    Filter.ValuesAsFloat['Whites'] := AValue;
    UpdateParentEffects;
  end;
end;

{*******************************************************}
procedure TALColorAdjustEffect.SetBlacks(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Blacks'] <> AValue then
  begin
    Filter.ValuesAsFloat['Blacks'] := AValue;
    UpdateParentEffects;
  end;
end;

{************************************************************}
procedure TALColorAdjustEffect.SetTemperature(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Temperature'] <> AValue then
  begin
    Filter.ValuesAsFloat['Temperature'] := AValue;
    UpdateParentEffects;
  end;
end;

{*****************************************************}
procedure TALColorAdjustEffect.SetTint(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Tint'] <> AValue then
  begin
    Filter.ValuesAsFloat['Tint'] := AValue;
    UpdateParentEffects;
  end;
end;

{*********************************************************}
procedure TALColorAdjustEffect.SetExposure(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Exposure'] <> AValue then
  begin
    Filter.ValuesAsFloat['Exposure'] := AValue;
    UpdateParentEffects;
  end;
end;

{******************************************************}
procedure TALColorAdjustEffect.SetGamma(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Gamma'] <> AValue then
  begin
    Filter.ValuesAsFloat['Gamma'] := AValue;
    UpdateParentEffects;
  end;
end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALColorAdjustEffect]);
  RegisterNoIcon([TALColorAdjustEffect]);
end;

initialization
  RegisterClasses([TALColorAdjustEffect]);
  {$IF defined(ALDPK)}
  // When you compile Alcinoe.dpk inside the IDE and when Alcinoe.dpk is already installed, you could receive the error :
  // Cannot register filter. A filter with the same name "ALColorAdjust" already exists..
  Try
    if TFilterManager.FilterByName('ALColorAdjust') = nil then
  {$ENDIF}
      // 'ColorAdjust' is the CATEGORY! Several filters can have the same category. This why 'ColorAdjust' and not 'ALColorAdjust'
      // the name of the filter is defined in TALColorAdjustFilter.FilterAttr as 'ALColorAdjust'
      TFilterManager.RegisterFilter('ColorAdjust', TALColorAdjustFilter);
  {$IF defined(ALDPK)}
  Except
    // For an unknown reason, under Athens 12.2, when you compile Alcinoe.dpk inside the IDE,
    // you receive an EAccessViolation when calling TFilterManager.FilterByName('ALColorAdjust').
  end;
  {$ENDIF}
end.
