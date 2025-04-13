unit Alcinoe.FMX.Materials.Canvas;

interface

{$I Alcinoe.inc}

uses
  FMX.Types3D,
  FMX.Materials.Canvas,
  Alcinoe.FMX.FilterEffects;

type

  {***********************************************************************}
  TALCanvasColorAdjustTextureMaterial = class(TCanvasTextureMaterial)
  private
    fShaderVariables: TALColorAdjustShaderVariables;
  protected
    procedure DoApply(const Context: TContext3D); override;
    procedure DoInitialize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ShaderVariables: TALColorAdjustShaderVariables read fShaderVariables;
  end;

  {*****************************************************************}
  TALCanvasExternalOESTextureMaterial = class(TCanvasTextureMaterial)
  private
  protected
    procedure DoInitialize; override;
  public
  end;

  {***********************************************************************************************}
  TALCanvasExternalOESColorAdjustTextureMaterial = class(TALCanvasExternalOESTextureMaterial)
  private
    fShaderVariables: TALColorAdjustShaderVariables;
  protected
    procedure DoApply(const Context: TContext3D); override;
    procedure DoInitialize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ShaderVariables: TALColorAdjustShaderVariables read fShaderVariables;
  end;

  {**********************************************************************************}
  TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial = class(TCanvasTextureMaterial)
  private
    function getCbCrTexture: TTexture;
  protected
    procedure DoApply(const Context: TContext3D); override;
    procedure DoInitialize; override;
  public
    property CbCrTexture: TTexture read getCbCrTexture;
  end;

  {*********************************************************************************************************************************}
  TALCanvas420YpCbCr8BiPlanarVideoRangeColorAdjustTextureMaterial = class(TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial)
  private
    fShaderVariables: TALColorAdjustShaderVariables;
  protected
    procedure DoApply(const Context: TContext3D); override;
    procedure DoInitialize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ShaderVariables: TALColorAdjustShaderVariables read fShaderVariables;
  end;

  {**********************************************************************}
  TALCanvas420YpCbCr8PlanarTextureMaterial = class(TCanvasTextureMaterial)
  private
    function getCbTexture: TTexture;
    function getCrTexture: TTexture;
  protected
    procedure DoApply(const Context: TContext3D); override;
    procedure DoInitialize; override;
  public
    property CbTexture: TTexture read getCbTexture;
    property CrTexture: TTexture read getCrTexture;
  end;

  {*********************************************************************************************************}
  TALCanvas420YpCbCr8PlanarColorAdjustTextureMaterial = class(TALCanvas420YpCbCr8PlanarTextureMaterial)
  private
    fShaderVariables: TALColorAdjustShaderVariables;
  protected
    procedure DoApply(const Context: TContext3D); override;
    procedure DoInitialize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ShaderVariables: TALColorAdjustShaderVariables read fShaderVariables;
  end;

{************************************************************************}
function ALGetDefExternalOESMaterial: TALCanvasExternalOESTextureMaterial;
function ALGetDef420YpCbCr8BiPlanarVideoRangeMaterial: TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial;
function ALGetDef420YpCbCr8PlanarMaterial: TALCanvas420YpCbCr8PlanarTextureMaterial;

implementation

uses
  System.Classes,
  system.sysutils,
  Alcinoe.FMX.Types3D,
  Alcinoe.StringUtils,
  Alcinoe.Common;

var
  ALDefExternalOESMaterial: TALCanvasExternalOESTextureMaterial;
  ALDef420YpCbCr8BiPlanarVideoRangeMaterial: TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial;
  ALDef420YpCbCr8PlanarMaterial: TALCanvas420YpCbCr8PlanarTextureMaterial;

{************************************************************************}
function ALGetDefExternalOESMaterial: TALCanvasExternalOESTextureMaterial;
begin
  if ALDefExternalOESMaterial = nil then begin
    var LMaterial := TALCanvasExternalOESTextureMaterial.Create;
    if AtomicCmpExchange(Pointer(ALDefExternalOESMaterial), Pointer(LMaterial), nil) <> nil then AlfreeAndNil(LMaterial)
    {$IFDEF AUTOREFCOUNT}
    else ALDefExternalOESMaterial.__ObjAddRef
    {$ENDIF AUTOREFCOUNT};
  end;
  Result := ALDefExternalOESMaterial;
end;

{**********************************************************************************************************}
function ALGetDef420YpCbCr8BiPlanarVideoRangeMaterial: TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial;
begin
  if ALDef420YpCbCr8BiPlanarVideoRangeMaterial = nil then begin
    var LMaterial := TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial.Create;
    if AtomicCmpExchange(Pointer(ALDef420YpCbCr8BiPlanarVideoRangeMaterial), Pointer(LMaterial), nil) <> nil then AlfreeAndNil(LMaterial)
    {$IFDEF AUTOREFCOUNT}
    else ALDef420YpCbCr8BiPlanarVideoRangeMaterial.__ObjAddRef
    {$ENDIF AUTOREFCOUNT};
  end;
  Result := ALDef420YpCbCr8BiPlanarVideoRangeMaterial;
end;

{**********************************************************************************}
function ALGetDef420YpCbCr8PlanarMaterial: TALCanvas420YpCbCr8PlanarTextureMaterial;
begin
  if ALDef420YpCbCr8PlanarMaterial = nil then begin
    var LMaterial := TALCanvas420YpCbCr8PlanarTextureMaterial.Create;
    if AtomicCmpExchange(Pointer(ALDef420YpCbCr8PlanarMaterial), Pointer(LMaterial), nil) <> nil then AlfreeAndNil(LMaterial)
    {$IFDEF AUTOREFCOUNT}
    else ALDef420YpCbCr8PlanarMaterial.__ObjAddRef
    {$ENDIF AUTOREFCOUNT};
  end;
  Result := ALDef420YpCbCr8PlanarMaterial;
end;

{**********************************************************************}
constructor TALCanvasColorAdjustTextureMaterial.Create;
begin
  inherited create;
  fShaderVariables := TALColorAdjustShaderVariables.create;
end;

{**********************************************************************}
destructor TALCanvasColorAdjustTextureMaterial.Destroy;
begin
  ALFreeAndNil(fShaderVariables);
  inherited destroy;
end;

{************************************************************************************************}
procedure TALCanvasColorAdjustTextureMaterial.DoApply(const Context: TContext3D);
begin
  inherited DoApply(Context);
  fshaderVariables.UpdateContext(Context);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Materials.Canvas.TCanvasTextureMaterial.DoInitialize is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
procedure TALCanvasColorAdjustTextureMaterial.DoInitialize;
begin
  FVertexShader := TShaderManager.RegisterShaderFromData('cnv_texture.fvs', TContextShaderKind.VertexShader, '', [

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct Vertex {'+
          '<#VertexDeclaration#>'+
        '};'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
          'float4 color;'+
          'float pointSize [[point_size]];'+
        '};'+

        'vertex ProjectedVertex vertexShader(constant Vertex *vertexArray [[buffer(0)]],'+
                                            'const unsigned int vertexId [[vertex_id]],'+
                                            'constant float4x4 &MVPMatrix [[buffer(1)]]) {'+
          'Vertex in = vertexArray[vertexId];'+
          'ProjectedVertex out;'+
          'out.position = float4(in.position[0], in.position[1], in.position[2], 1) * MVPMatrix;'+
          'out.textureCoord = in.texcoord0;'+
          'out.color = float4(float(in.color0[2])/255,float(in.color0[1])/255,float(in.color0[0])/255,float(in.color0[3])/255);'+
          'out.pointSize = 1.0f;'+
          'return out;'+
        '}'
      ),
      [TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 1, 4)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    //attribute vec2 a_TexCoord0;
    //attribute vec4 a_Color;
    //attribute vec3 a_Position;
    //varying vec4 TEX0;
    //varying vec4 COLOR0;
    //vec4 _o_pos1;
    //vec4 _o_color1;
    //vec2 _o_texcoord01;
    //vec4 _r0003;
    //vec4 _v0003;
    //uniform vec4 _MVPMatrix[4];
    //void main()
    //{
    //    _v0003 = vec4(a_Position.x, a_Position.y, a_Position.z, 1.0);
    //    _r0003.x = dot(_MVPMatrix[0], _v0003);
    //    _r0003.y = dot(_MVPMatrix[1], _v0003);
    //    _r0003.z = dot(_MVPMatrix[2], _v0003);
    //    _r0003.w = dot(_MVPMatrix[3], _v0003);
    //    _o_pos1 = _r0003;
    //    _o_texcoord01 = a_TexCoord0.xy;
    //    _o_color1 = a_Color;
    //    TEX0.xy = a_TexCoord0.xy;
    //    COLOR0 = a_Color;
    //    gl_Position = _r0003;
    //}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $32, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76,
      $65, $63, $34, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $33, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $3B,
      $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $43, $4F, $4C, $4F, $52,
      $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $63, $6F, $6C, $6F, $72, $31, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $76, $30, $30,
      $30, $33, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $34, $5D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D,
      $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $5F, $76, $30, $30, $30, $33, $20, $3D, $20, $76, $65, $63, $34, $28, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $78,
      $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $79, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $7A, $2C, $20, $31, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $72, $30, $30, $30, $33, $2E, $78, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $30, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $79, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $31, $5D, $2C, $20, $5F, $76, $30, $30, $30,
      $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $7A, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $32, $5D, $2C, $20, $5F,
      $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $77, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $33,
      $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F,
      $63, $6F, $6C, $6F, $72, $31, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $54, $45, $58, $30, $2E, $78, $79, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F,
      $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $43, $4F, $4C, $4F, $52, $30, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F,
      $50, $6F, $73, $69, $74, $69, $6F, $6E, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    )
    {$ENDREGION}

  ]);
  FPixelShader := TShaderManager.RegisterShaderFromData('cnv_texture.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        ALFormatW(
          ALColorAdjustMSL,
          ['const texture2d<float> texture0 [[texture(0)]],'+
           'const sampler texture0Sampler [[sampler(0)]]',
           //--
           'float4 result = texture0.sample(texture0Sampler, in.textureCoord);',
           //--
           'result = result * in.color;'],
          ALDefaultFormatSettingsW)),
      [TContextShaderVariable.Create('Contrast',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Saturation',  TContextShaderVariableKind.Float,   1, 1),
       TContextShaderVariable.Create('Vibrance',    TContextShaderVariableKind.Float,   2, 1),
       TContextShaderVariable.Create('Whites',      TContextShaderVariableKind.Float,   3, 1),
       TContextShaderVariable.Create('Blacks',      TContextShaderVariableKind.Float,   4, 1),
       TContextShaderVariable.Create('Temperature', TContextShaderVariableKind.Float,   5, 1),
       TContextShaderVariable.Create('Tint',        TContextShaderVariableKind.Float,   6, 1),
       TContextShaderVariable.Create('Exposure',    TContextShaderVariableKind.Float,   7, 1),
       TContextShaderVariable.Create('Gamma',       TContextShaderVariableKind.Float,   8, 1),
       TContextShaderVariable.Create('texture0',    TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    //
    //ORIGINAL:
    //
    //varying vec4 COLOR0;
    //varying vec4 TEX0;
    //uniform sampler2D _texture0;
    //
    //void main()
    //{
    //  gl_FragColor = texture2D(_texture0, TEX0.xy) * COLOR0;
    //}
    //
    TContextShaderSource.Create(
      TContextShaderArch.GLSL,
      TEncoding.UTF8.GetBytes(
        ALFormatW(
          ALColorAdjustGLSL,
          ['precision highp float;'+
           'varying vec4 COLOR0;'+
           'varying vec4 TEX0;'+
           'uniform sampler2D _texture0;',
           //--
           'vec4 result = texture2D(_texture0, TEX0.xy);',
           //--
           'result = result * COLOR0;'],
          ALDefaultFormatSettingsW)),
      [TContextShaderVariable.Create('Contrast',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Saturation',  TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Vibrance',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Whites',      TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Blacks',      TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Temperature', TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Tint',        TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Exposure',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Gamma',       TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('texture0',    TContextShaderVariableKind.Texture, 0, 0)]
    )
    {$ENDREGION}

  ]);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Materials.Canvas.TCanvasTextureMaterial.DoInitialize is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
procedure TALCanvasExternalOESTextureMaterial.DoInitialize;
begin
  FVertexShader := TShaderManager.RegisterShaderFromData('cnv_texture.fvs', TContextShaderKind.VertexShader, '', [

    {$REGION 'TContextShaderArch.GLSL'}
    //attribute vec2 a_TexCoord0;
    //attribute vec4 a_Color;
    //attribute vec3 a_Position;
    //varying vec4 TEX0;
    //varying vec4 COLOR0;
    //vec4 _o_pos1;
    //vec4 _o_color1;
    //vec2 _o_texcoord01;
    //vec4 _r0003;
    //vec4 _v0003;
    //uniform vec4 _MVPMatrix[4];
    //void main()
    //{
    //    _v0003 = vec4(a_Position.x, a_Position.y, a_Position.z, 1.0);
    //    _r0003.x = dot(_MVPMatrix[0], _v0003);
    //    _r0003.y = dot(_MVPMatrix[1], _v0003);
    //    _r0003.z = dot(_MVPMatrix[2], _v0003);
    //    _r0003.w = dot(_MVPMatrix[3], _v0003);
    //    _o_pos1 = _r0003;
    //    _o_texcoord01 = a_TexCoord0.xy;
    //    _o_color1 = a_Color;
    //    TEX0.xy = a_TexCoord0.xy;
    //    COLOR0 = a_Color;
    //    gl_Position = _r0003;
    //}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $32, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76,
      $65, $63, $34, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $33, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $3B,
      $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $43, $4F, $4C, $4F, $52,
      $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $63, $6F, $6C, $6F, $72, $31, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $76, $30, $30,
      $30, $33, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $34, $5D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D,
      $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $5F, $76, $30, $30, $30, $33, $20, $3D, $20, $76, $65, $63, $34, $28, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $78,
      $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $79, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $7A, $2C, $20, $31, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $72, $30, $30, $30, $33, $2E, $78, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $30, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $79, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $31, $5D, $2C, $20, $5F, $76, $30, $30, $30,
      $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $7A, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $32, $5D, $2C, $20, $5F,
      $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $77, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $33,
      $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F,
      $63, $6F, $6C, $6F, $72, $31, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $54, $45, $58, $30, $2E, $78, $79, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F,
      $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $43, $4F, $4C, $4F, $52, $30, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F,
      $50, $6F, $73, $69, $74, $69, $6F, $6E, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    )
    {$ENDREGION}

  ]);
  FPixelShader := TShaderManager.RegisterShaderFromData('cnv_texture.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.GLSL'}
    //
    //ORIGINAL:
    //
    //varying vec4 COLOR0;
    //varying vec4 TEX0;
    //uniform sampler2D _texture0;
    //
    //void main()
    //{
    //  gl_FragColor = texture2D(_texture0, TEX0.xy) * COLOR0;
    //}
    //
    TContextShaderSource.Create(
      TContextShaderArch.GLSL,
      TEncoding.UTF8.GetBytes(

        '#extension GL_OES_EGL_image_external : require'+#13#10+
        'precision highp float;'+
        'varying vec4 COLOR0;'+
        'varying vec4 TEX0;'+
        'uniform samplerExternalOES _texture0;'+

        'void main()'+
        '{'+

           'gl_FragColor = texture2D(_texture0, TEX0.xy) * COLOR0;'+

        '}'

      ),
      [TContextShaderVariable.Create('texture0',    TContextShaderVariableKind.Texture, 0, 0)]
    )
    {$ENDREGION}

  ]);
end;

{**********************************************************************}
constructor TALCanvasExternalOESColorAdjustTextureMaterial.Create;
begin
  inherited create;
  fShaderVariables := TALColorAdjustShaderVariables.create;
end;

{**********************************************************************}
destructor TALCanvasExternalOESColorAdjustTextureMaterial.Destroy;
begin
  ALFreeAndNil(fShaderVariables);
  inherited destroy;
end;

{************************************************************************************************}
procedure TALCanvasExternalOESColorAdjustTextureMaterial.DoApply(const Context: TContext3D);
begin
  inherited DoApply(Context);
  fshaderVariables.UpdateContext(Context);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Materials.Canvas.TCanvasTextureMaterial.DoInitialize is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
procedure TALCanvasExternalOESColorAdjustTextureMaterial.DoInitialize;
begin
  FVertexShader := TShaderManager.RegisterShaderFromData('cnv_texture.fvs', TContextShaderKind.VertexShader, '', [

    {$REGION 'TContextShaderArch.GLSL'}
    //attribute vec2 a_TexCoord0;
    //attribute vec4 a_Color;
    //attribute vec3 a_Position;
    //varying vec4 TEX0;
    //varying vec4 COLOR0;
    //vec4 _o_pos1;
    //vec4 _o_color1;
    //vec2 _o_texcoord01;
    //vec4 _r0003;
    //vec4 _v0003;
    //uniform vec4 _MVPMatrix[4];
    //void main()
    //{
    //    _v0003 = vec4(a_Position.x, a_Position.y, a_Position.z, 1.0);
    //    _r0003.x = dot(_MVPMatrix[0], _v0003);
    //    _r0003.y = dot(_MVPMatrix[1], _v0003);
    //    _r0003.z = dot(_MVPMatrix[2], _v0003);
    //    _r0003.w = dot(_MVPMatrix[3], _v0003);
    //    _o_pos1 = _r0003;
    //    _o_texcoord01 = a_TexCoord0.xy;
    //    _o_color1 = a_Color;
    //    TEX0.xy = a_TexCoord0.xy;
    //    COLOR0 = a_Color;
    //    gl_Position = _r0003;
    //}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $32, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76,
      $65, $63, $34, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $33, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $3B,
      $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $43, $4F, $4C, $4F, $52,
      $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $63, $6F, $6C, $6F, $72, $31, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $76, $30, $30,
      $30, $33, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $34, $5D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D,
      $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $5F, $76, $30, $30, $30, $33, $20, $3D, $20, $76, $65, $63, $34, $28, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $78,
      $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $79, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $7A, $2C, $20, $31, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $72, $30, $30, $30, $33, $2E, $78, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $30, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $79, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $31, $5D, $2C, $20, $5F, $76, $30, $30, $30,
      $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $7A, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $32, $5D, $2C, $20, $5F,
      $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $77, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $33,
      $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F,
      $63, $6F, $6C, $6F, $72, $31, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $54, $45, $58, $30, $2E, $78, $79, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F,
      $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $43, $4F, $4C, $4F, $52, $30, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F,
      $50, $6F, $73, $69, $74, $69, $6F, $6E, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    )
    {$ENDREGION}

  ]);
  FPixelShader := TShaderManager.RegisterShaderFromData('cnv_texture.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.GLSL'}
    //
    //ORIGINAL:
    //
    //varying vec4 COLOR0;
    //varying vec4 TEX0;
    //uniform sampler2D _texture0;
    //
    //void main()
    //{
    //  gl_FragColor = texture2D(_texture0, TEX0.xy) * COLOR0;
    //}
    //
    TContextShaderSource.Create(
      TContextShaderArch.GLSL,
      TEncoding.UTF8.GetBytes(
        ALFormatW(
          ALColorAdjustGLSL,
          ['#extension GL_OES_EGL_image_external : require'+#13#10+
           'precision highp float;'+
           'varying vec4 COLOR0;'+
           'varying vec4 TEX0;'+
           'uniform samplerExternalOES _texture0;',
           //--
           'vec4 result = texture2D(_texture0, TEX0.xy);',
           //--
           'result = result * COLOR0;'],
          ALDefaultFormatSettingsW)),
      [TContextShaderVariable.Create('Contrast',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Saturation',  TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Vibrance',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Whites',      TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Blacks',      TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Temperature', TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Tint',        TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Exposure',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Gamma',       TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('texture0',    TContextShaderVariableKind.Texture, 0, 0)]
    )
    {$ENDREGION}

  ]);
end;

{*************************************************************************************}
function TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial.getCbCrTexture: TTexture;
begin
  if (Texture is TALBiPlanarTexture) then result := TALBiPlanarTexture(Texture).SecondTexture
  else result := nil;
end;

{************************************************************************************************}
procedure TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial.DoApply(const Context: TContext3D);
begin
  inherited DoApply(Context);
  Context.SetShaderVariable('texture1', CbCrTexture);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Materials.Canvas.TCanvasTextureMaterial.DoInitialize is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
procedure TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial.DoInitialize;
begin
  FVertexShader := TShaderManager.RegisterShaderFromData('cnv_texture.fvs', TContextShaderKind.VertexShader, '', [

    {$REGION 'TContextShaderArch.GLSL'}
    //attribute vec2 a_TexCoord0;
    //attribute vec4 a_Color;
    //attribute vec3 a_Position;
    //varying vec4 TEX0;
    //varying vec4 COLOR0;
    //vec4 _o_pos1;
    //vec4 _o_color1;
    //vec2 _o_texcoord01;
    //vec4 _r0003;
    //vec4 _v0003;
    //uniform vec4 _MVPMatrix[4];
    //void main()
    //{
    //    _v0003 = vec4(a_Position.x, a_Position.y, a_Position.z, 1.0);
    //    _r0003.x = dot(_MVPMatrix[0], _v0003);
    //    _r0003.y = dot(_MVPMatrix[1], _v0003);
    //    _r0003.z = dot(_MVPMatrix[2], _v0003);
    //    _r0003.w = dot(_MVPMatrix[3], _v0003);
    //    _o_pos1 = _r0003;
    //    _o_texcoord01 = a_TexCoord0.xy;
    //    _o_color1 = a_Color;
    //    TEX0.xy = a_TexCoord0.xy;
    //    COLOR0 = a_Color;
    //    gl_Position = _r0003;
    //}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $32, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76,
      $65, $63, $34, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $33, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $3B,
      $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $43, $4F, $4C, $4F, $52,
      $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $63, $6F, $6C, $6F, $72, $31, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $76, $30, $30,
      $30, $33, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $34, $5D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D,
      $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $5F, $76, $30, $30, $30, $33, $20, $3D, $20, $76, $65, $63, $34, $28, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $78,
      $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $79, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $7A, $2C, $20, $31, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $72, $30, $30, $30, $33, $2E, $78, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $30, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $79, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $31, $5D, $2C, $20, $5F, $76, $30, $30, $30,
      $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $7A, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $32, $5D, $2C, $20, $5F,
      $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $77, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $33,
      $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F,
      $63, $6F, $6C, $6F, $72, $31, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $54, $45, $58, $30, $2E, $78, $79, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F,
      $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $43, $4F, $4C, $4F, $52, $30, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F,
      $50, $6F, $73, $69, $74, $69, $6F, $6E, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    )
    {$ENDREGION}

  ]);
  FPixelShader := TShaderManager.RegisterShaderFromData('cnv_texture.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.GLSL'}
    //
    //ORIGINAL:
    //
    //varying vec4 COLOR0;
    //varying vec4 TEX0;
    //uniform sampler2D _texture0;
    //
    //void main()
    //{
    //  gl_FragColor = texture2D(_texture0, TEX0.xy) * COLOR0;
    //}
    //
    TContextShaderSource.Create(
      TContextShaderArch.GLSL,
      TEncoding.UTF8.GetBytes(

        'varying vec4 COLOR0;'+
        'varying vec4 TEX0;'+
        'uniform sampler2D _texture0;'+
        'uniform sampler2D _texture1;'+

        'void main()'+
        '{'+

           'mediump vec3 yuv;'+
           'lowp vec3 rgb;'+
           'yuv.x = texture2D(_texture0, TEX0.xy).r;'+
           'yuv.yz = texture2D(_texture1, TEX0.xy).rg - vec2(0.5, 0.5);'+

           // BT.601, which is the standard for SDTV is provided as a reference
           // rgb = mat3(
           //   1, 1, 1,
           //   0, -.34413, 1.772,
           //   1.402, -.71414, 0) * yuv;

           // Using BT.709 which is the standard for HDTV
           // https://www.xaymar.com/2017/07/06/how-to-converting-rgb-to-yuv-and-yuv-to-rgb/
           'rgb = mat3('+
             '1.0, 1.0, 1.0, '+
             '0.0, -0.187324, 1.8556, '+
             '1.5748, -0.468124, 0.0) * yuv;'+

           'gl_FragColor = vec4(rgb, 1) * COLOR0;'+

        '}'

      ),
      [TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0),
       TContextShaderVariable.Create('texture1', TContextShaderVariableKind.Texture, 1, 0)]
    )
    {$ENDREGION}

  ]);
end;

{***************************************************************************************}
constructor TALCanvas420YpCbCr8BiPlanarVideoRangeColorAdjustTextureMaterial.Create;
begin
  inherited create;
  fShaderVariables := TALColorAdjustShaderVariables.create;
end;

{***************************************************************************************}
destructor TALCanvas420YpCbCr8BiPlanarVideoRangeColorAdjustTextureMaterial.Destroy;
begin
  ALFreeAndNil(fShaderVariables);
  inherited destroy;
end;

{*****************************************************************************************************************}
procedure TALCanvas420YpCbCr8BiPlanarVideoRangeColorAdjustTextureMaterial.DoApply(const Context: TContext3D);
begin
  inherited DoApply(Context);
  fshaderVariables.UpdateContext(Context);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Materials.Canvas.TCanvasTextureMaterial.DoInitialize is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
procedure TALCanvas420YpCbCr8BiPlanarVideoRangeColorAdjustTextureMaterial.DoInitialize;
begin
  FVertexShader := TShaderManager.RegisterShaderFromData('cnv_texture.fvs', TContextShaderKind.VertexShader, '', [

    {$REGION 'TContextShaderArch.GLSL'}
    //attribute vec2 a_TexCoord0;
    //attribute vec4 a_Color;
    //attribute vec3 a_Position;
    //varying vec4 TEX0;
    //varying vec4 COLOR0;
    //vec4 _o_pos1;
    //vec4 _o_color1;
    //vec2 _o_texcoord01;
    //vec4 _r0003;
    //vec4 _v0003;
    //uniform vec4 _MVPMatrix[4];
    //void main()
    //{
    //    _v0003 = vec4(a_Position.x, a_Position.y, a_Position.z, 1.0);
    //    _r0003.x = dot(_MVPMatrix[0], _v0003);
    //    _r0003.y = dot(_MVPMatrix[1], _v0003);
    //    _r0003.z = dot(_MVPMatrix[2], _v0003);
    //    _r0003.w = dot(_MVPMatrix[3], _v0003);
    //    _o_pos1 = _r0003;
    //    _o_texcoord01 = a_TexCoord0.xy;
    //    _o_color1 = a_Color;
    //    TEX0.xy = a_TexCoord0.xy;
    //    COLOR0 = a_Color;
    //    gl_Position = _r0003;
    //}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $32, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76,
      $65, $63, $34, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $33, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $3B,
      $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $43, $4F, $4C, $4F, $52,
      $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $63, $6F, $6C, $6F, $72, $31, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $76, $30, $30,
      $30, $33, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $34, $5D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D,
      $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $5F, $76, $30, $30, $30, $33, $20, $3D, $20, $76, $65, $63, $34, $28, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $78,
      $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $79, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $7A, $2C, $20, $31, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $72, $30, $30, $30, $33, $2E, $78, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $30, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $79, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $31, $5D, $2C, $20, $5F, $76, $30, $30, $30,
      $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $7A, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $32, $5D, $2C, $20, $5F,
      $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $77, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $33,
      $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F,
      $63, $6F, $6C, $6F, $72, $31, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $54, $45, $58, $30, $2E, $78, $79, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F,
      $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $43, $4F, $4C, $4F, $52, $30, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F,
      $50, $6F, $73, $69, $74, $69, $6F, $6E, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    )
    {$ENDREGION}

  ]);
  FPixelShader := TShaderManager.RegisterShaderFromData('cnv_texture.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.GLSL'}
    //
    //ORIGINAL:
    //
    //varying vec4 COLOR0;
    //varying vec4 TEX0;
    //uniform sampler2D _texture0;
    //
    //void main()
    //{
    //  gl_FragColor = texture2D(_texture0, TEX0.xy) * COLOR0;
    //}
    //
    TContextShaderSource.Create(
      TContextShaderArch.GLSL,
      TEncoding.UTF8.GetBytes(
        ALFormatW(
          ALColorAdjustGLSL,
          ['varying vec4 COLOR0;'+
           'varying vec4 TEX0;'+
           'uniform sampler2D _texture0;'+
           'uniform sampler2D _texture1;',
           //----
           '',
           //----
           'mediump vec3 yuv;'+
           'lowp vec3 rgb;'+
           'yuv.x = texture2D(_texture0, TEX0.xy).r;'+
           'yuv.yz = texture2D(_texture1, TEX0.xy).rg - vec2(0.5, 0.5);'+

           // BT.601, which is the standard for SDTV is provided as a reference
           // rgb = mat3(
           //   1, 1, 1,
           //   0, -.34413, 1.772,
           //   1.402, -.71414, 0) * yuv;

           // Using BT.709 which is the standard for HDTV
           // https://www.xaymar.com/2017/07/06/how-to-converting-rgb-to-yuv-and-yuv-to-rgb/
           'rgb = mat3('+
             '1.0, 1.0, 1.0, '+
             '0.0, -0.187324, 1.8556, '+
             '1.5748, -0.468124, 0.0) * yuv;'+

           'vec4 result = vec4(rgb, 1);',
           //----
           'result = result * COLOR0;'],
          ALDefaultFormatSettingsW)),
      [TContextShaderVariable.Create('Contrast',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Saturation',  TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Vibrance',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Whites',      TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Blacks',      TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Temperature', TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Tint',        TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Exposure',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Gamma',       TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('texture0',    TContextShaderVariableKind.Texture, 0, 0),
       TContextShaderVariable.Create('texture1',    TContextShaderVariableKind.Texture, 1, 0)]
    )
    {$ENDREGION}

  ]);
end;

{***********************************************************************}
function TALCanvas420YpCbCr8PlanarTextureMaterial.getCbTexture: TTexture;
begin
  if (Texture is TALplanarTexture) then result := TALplanarTexture(Texture).SecondTexture
  else result := nil;
end;

{***********************************************************************}
function TALCanvas420YpCbCr8PlanarTextureMaterial.getCrTexture: TTexture;
begin
  if (Texture is TALplanarTexture) then result := TALplanarTexture(Texture).ThirdTexture
  else result := nil;
end;

{************************************************************************************}
procedure TALCanvas420YpCbCr8PlanarTextureMaterial.DoApply(const Context: TContext3D);
begin
  inherited DoApply(Context);;
  Context.SetShaderVariable('texture1', CbTexture);
  Context.SetShaderVariable('texture2', CrTexture);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Materials.Canvas.TCanvasTextureMaterial.DoInitialize is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
procedure TALCanvas420YpCbCr8PlanarTextureMaterial.DoInitialize;
begin
  FVertexShader := TShaderManager.RegisterShaderFromData('cnv_texture.fvs', TContextShaderKind.VertexShader, '', [

    {$REGION 'TContextShaderArch.GLSL'}
    //attribute vec2 a_TexCoord0;
    //attribute vec4 a_Color;
    //attribute vec3 a_Position;
    //varying vec4 TEX0;
    //varying vec4 COLOR0;
    //vec4 _o_pos1;
    //vec4 _o_color1;
    //vec2 _o_texcoord01;
    //vec4 _r0003;
    //vec4 _v0003;
    //uniform vec4 _MVPMatrix[4];
    //void main()
    //{
    //    _v0003 = vec4(a_Position.x, a_Position.y, a_Position.z, 1.0);
    //    _r0003.x = dot(_MVPMatrix[0], _v0003);
    //    _r0003.y = dot(_MVPMatrix[1], _v0003);
    //    _r0003.z = dot(_MVPMatrix[2], _v0003);
    //    _r0003.w = dot(_MVPMatrix[3], _v0003);
    //    _o_pos1 = _r0003;
    //    _o_texcoord01 = a_TexCoord0.xy;
    //    _o_color1 = a_Color;
    //    TEX0.xy = a_TexCoord0.xy;
    //    COLOR0 = a_Color;
    //    gl_Position = _r0003;
    //}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $32, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76,
      $65, $63, $34, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $33, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $3B,
      $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $43, $4F, $4C, $4F, $52,
      $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $63, $6F, $6C, $6F, $72, $31, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $76, $30, $30,
      $30, $33, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $34, $5D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D,
      $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $5F, $76, $30, $30, $30, $33, $20, $3D, $20, $76, $65, $63, $34, $28, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $78,
      $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $79, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $7A, $2C, $20, $31, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $72, $30, $30, $30, $33, $2E, $78, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $30, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $79, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $31, $5D, $2C, $20, $5F, $76, $30, $30, $30,
      $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $7A, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $32, $5D, $2C, $20, $5F,
      $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $77, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $33,
      $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F,
      $63, $6F, $6C, $6F, $72, $31, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $54, $45, $58, $30, $2E, $78, $79, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F,
      $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $43, $4F, $4C, $4F, $52, $30, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F,
      $50, $6F, $73, $69, $74, $69, $6F, $6E, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    )
    {$ENDREGION}

  ]);
  FPixelShader := TShaderManager.RegisterShaderFromData('cnv_texture.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.GLSL'}
    //
    //ORIGINAL:
    //
    //varying vec4 COLOR0;
    //varying vec4 TEX0;
    //uniform sampler2D _texture0;
    //
    //void main()
    //{
    //  gl_FragColor = texture2D(_texture0, TEX0.xy) * COLOR0;
    //}
    //
    TContextShaderSource.Create(
      TContextShaderArch.GLSL,
      TEncoding.UTF8.GetBytes(

        'varying vec4 COLOR0;'+
        'varying vec4 TEX0;'+
        'uniform sampler2D _texture0;'+
        'uniform sampler2D _texture1;'+
        'uniform sampler2D _texture2;'+

        'void main()'+
        '{'+

           'mediump vec3 yuv;'+
           'lowp vec3 rgb;'+
           'yuv.x = texture2D(_texture0, TEX0.xy).r;'+
           'yuv.y = texture2D(_texture1, TEX0.xy).r - 0.5;'+
           'yuv.z = texture2D(_texture2, TEX0.xy).r - 0.5;'+

           // BT.601, which is the standard for SDTV is provided as a reference
           // rgb = mat3(
           //   1, 1, 1,
           //   0, -.34413, 1.772,
           //   1.402, -.71414, 0) * yuv;

           // Using BT.709 which is the standard for HDTV
           // https://www.xaymar.com/2017/07/06/how-to-converting-rgb-to-yuv-and-yuv-to-rgb/
           'rgb = mat3('+
             '1.0, 1.0, 1.0, '+
             '0.0, -0.187324, 1.8556, '+
             '1.5748, -0.468124, 0.0) * yuv;'+

           'gl_FragColor = vec4(rgb, 1) * COLOR0;'+

        '}'

      ),
      [TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0),
       TContextShaderVariable.Create('texture1', TContextShaderVariableKind.Texture, 1, 0),
       TContextShaderVariable.Create('texture2', TContextShaderVariableKind.Texture, 2, 0)]
    )
    {$ENDREGION}

  ]);
end;

{***************************************************************************}
constructor TALCanvas420YpCbCr8PlanarColorAdjustTextureMaterial.Create;
begin
  inherited create;
  fShaderVariables := TALColorAdjustShaderVariables.create;
end;

{***************************************************************************}
destructor TALCanvas420YpCbCr8PlanarColorAdjustTextureMaterial.Destroy;
begin
  ALFreeAndNil(fShaderVariables);
  inherited destroy;
end;

{*****************************************************************************************************}
procedure TALCanvas420YpCbCr8PlanarColorAdjustTextureMaterial.DoApply(const Context: TContext3D);
begin
  inherited DoApply(Context);
  fshaderVariables.UpdateContext(Context);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Materials.Canvas.TCanvasTextureMaterial.DoInitialize is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
procedure TALCanvas420YpCbCr8PlanarColorAdjustTextureMaterial.DoInitialize;
begin
  FVertexShader := TShaderManager.RegisterShaderFromData('cnv_texture.fvs', TContextShaderKind.VertexShader, '', [

    {$REGION 'TContextShaderArch.GLSL'}
    //attribute vec2 a_TexCoord0;
    //attribute vec4 a_Color;
    //attribute vec3 a_Position;
    //varying vec4 TEX0;
    //varying vec4 COLOR0;
    //vec4 _o_pos1;
    //vec4 _o_color1;
    //vec2 _o_texcoord01;
    //vec4 _r0003;
    //vec4 _v0003;
    //uniform vec4 _MVPMatrix[4];
    //void main()
    //{
    //    _v0003 = vec4(a_Position.x, a_Position.y, a_Position.z, 1.0);
    //    _r0003.x = dot(_MVPMatrix[0], _v0003);
    //    _r0003.y = dot(_MVPMatrix[1], _v0003);
    //    _r0003.z = dot(_MVPMatrix[2], _v0003);
    //    _r0003.w = dot(_MVPMatrix[3], _v0003);
    //    _o_pos1 = _r0003;
    //    _o_texcoord01 = a_TexCoord0.xy;
    //    _o_color1 = a_Color;
    //    TEX0.xy = a_TexCoord0.xy;
    //    COLOR0 = a_Color;
    //    gl_Position = _r0003;
    //}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $32, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76,
      $65, $63, $34, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $33, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $3B,
      $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $43, $4F, $4C, $4F, $52,
      $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $63, $6F, $6C, $6F, $72, $31, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $76, $30, $30,
      $30, $33, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $34, $5D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D,
      $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $5F, $76, $30, $30, $30, $33, $20, $3D, $20, $76, $65, $63, $34, $28, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $78,
      $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $79, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $7A, $2C, $20, $31, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $72, $30, $30, $30, $33, $2E, $78, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $30, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $79, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $31, $5D, $2C, $20, $5F, $76, $30, $30, $30,
      $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $7A, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $32, $5D, $2C, $20, $5F,
      $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $77, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $33,
      $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F,
      $63, $6F, $6C, $6F, $72, $31, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $54, $45, $58, $30, $2E, $78, $79, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F,
      $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $43, $4F, $4C, $4F, $52, $30, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F,
      $50, $6F, $73, $69, $74, $69, $6F, $6E, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    )
    {$ENDREGION}

  ]);
  FPixelShader := TShaderManager.RegisterShaderFromData('cnv_texture.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.GLSL'}
    //
    //ORIGINAL:
    //
    //varying vec4 COLOR0;
    //varying vec4 TEX0;
    //uniform sampler2D _texture0;
    //
    //void main()
    //{
    //  gl_FragColor = texture2D(_texture0, TEX0.xy) * COLOR0;
    //}
    //
    TContextShaderSource.Create(
      TContextShaderArch.GLSL,
      TEncoding.UTF8.GetBytes(
        ALFormatW(
          ALColorAdjustGLSL,
          ['varying vec4 COLOR0;'+
           'varying vec4 TEX0;'+
           'uniform sampler2D _texture0;'+
           'uniform sampler2D _texture1;'+
           'uniform sampler2D _texture2;',
           //----
           '',
           //----
           'mediump vec3 yuv;'+
           'lowp vec3 rgb;'+
           'yuv.x = texture2D(_texture0, TEX0.xy).r;'+
           'yuv.y = texture2D(_texture1, TEX0.xy).r - 0.5;'+
           'yuv.z = texture2D(_texture2, TEX0.xy).r - 0.5;'+

           // BT.601, which is the standard for SDTV is provided as a reference
           // rgb = mat3(
           //   1, 1, 1,
           //   0, -.34413, 1.772,
           //   1.402, -.71414, 0) * yuv;

           // Using BT.709 which is the standard for HDTV
           // https://www.xaymar.com/2017/07/06/how-to-converting-rgb-to-yuv-and-yuv-to-rgb/
           'rgb = mat3('+
             '1.0, 1.0, 1.0, '+
             '0.0, -0.187324, 1.8556, '+
             '1.5748, -0.468124, 0.0) * yuv;'+

           'vec4 result = vec4(rgb, 1);',
           //----
           'result = result * COLOR0;'],
          ALDefaultFormatSettingsW)),
      [TContextShaderVariable.Create('Contrast',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Saturation',  TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Vibrance',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Whites',      TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Blacks',      TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Temperature', TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Tint',        TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Exposure',    TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('Gamma',       TContextShaderVariableKind.Float,   0, 1),
       TContextShaderVariable.Create('texture0',    TContextShaderVariableKind.Texture, 0, 0),
       TContextShaderVariable.Create('texture1',    TContextShaderVariableKind.Texture, 1, 0),
       TContextShaderVariable.Create('texture2',    TContextShaderVariableKind.Texture, 2, 0)]
    )
    {$ENDREGION}

  ]);
end;

initialization
  ALDefExternalOESMaterial := nil;
  ALDef420YpCbCr8BiPlanarVideoRangeMaterial := nil;
  ALDef420YpCbCr8PlanarMaterial := nil;

end.
