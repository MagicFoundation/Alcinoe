{**********************************************************}
{                                                          }
{           CodeGear Delphi Runtime Library                }
{                                                          }
{ Delphi-Objective-C Bridge                                }
{ Interfaces for Cocoa framework GLKit                     }
{                                                          }
{ Copyright (c) 2011, Apple Inc. All rights reserved.      }
{                                                          }
{ Translator: Embarcadero Technologies, Inc.               }
{   Copyright(c) 2012-2017 Embarcadero Technologies, Inc.  }
{              All rights reserved                         }
{                                                          }
{**********************************************************}
unit iOSapi.GLKit;

interface

uses
  Posix.StdDef, Macapi.ObjectiveC, iOSapi.CocoaTypes, Macapi.CoreFoundation, iOSapi.Foundation,
  iOSapi.CoreGraphics, iOSapi.OpenGLES, iOSapi.UIKit;

const
  GLKFogModeExp = 0;
  GLKFogModeExp2 = 1;
  GLKFogModeLinear = 2;
  GLKLightingTypePerPixel = 1;
  GLKLightingTypePerVertex = 0;
  GLKTextureEnvModeDecal = 2;
  GLKTextureEnvModeModulate = 1;
  GLKTextureEnvModeReplace = 0;
  GLKTextureInfoAlphaStateNonPremultiplied = 1;
  GLKTextureInfoAlphaStateNone = 0;
  GLKTextureInfoAlphaStatePremultiplied = 2;
  GLKTextureInfoOriginBottomLeft = 2;
  GLKTextureInfoOriginTopLeft = 1;
  GLKTextureInfoOriginUnknown = 0;
  GLKTextureLoaderErrorAlphaPremultiplicationFailure = 16;
  GLKTextureLoaderErrorCompressedTextureUpload = 7;
  GLKTextureLoaderErrorCubeMapInvalidNumFiles = 6;
  GLKTextureLoaderErrorDataPreprocessingFailure = 12;
  GLKTextureLoaderErrorFileOrURLNotFound = 0;
  GLKTextureLoaderErrorInvalidCGImage = 2;
  GLKTextureLoaderErrorInvalidEAGLContext = 17;
  GLKTextureLoaderErrorInvalidNSData = 1;
  GLKTextureLoaderErrorMipmapUnsupported = 13;
  GLKTextureLoaderErrorPVRAtlasUnsupported = 5;
  GLKTextureLoaderErrorReorientationFailure = 15;
  GLKTextureLoaderErrorUncompressedTextureUpload = 8;
  GLKTextureLoaderErrorUnknownFileType = 4;
  GLKTextureLoaderErrorUnknownPathType = 3;
  GLKTextureLoaderErrorUnsupportedBitDepth = 10;
  GLKTextureLoaderErrorUnsupportedCubeMapDimensions = 9;
  GLKTextureLoaderErrorUnsupportedOrientation = 14;
  GLKTextureLoaderErrorUnsupportedPVRFormat = 11;
  GLKTextureTarget2D = 3553;
  GLKTextureTargetCt = 2;
  GLKTextureTargetCubeMap = 34067;
  GLKVertexAttribColor = 2;
  GLKVertexAttribNormal = 1;
  GLKVertexAttribPosition = 0;
  GLKVertexAttribTexCoord0 = 3;
  GLKVertexAttribTexCoord1 = 4;
  GLKViewDrawableColorFormatRGB565 = 1;
  GLKViewDrawableColorFormatRGBA8888 = 0;
  GLKViewDrawableDepthFormat16 = 1;
  GLKViewDrawableDepthFormat24 = 2;
  GLKViewDrawableDepthFormatNone = 0;
  GLKViewDrawableMultisample4X = 1;
  GLKViewDrawableMultisampleNone = 0;
  GLKViewDrawableStencilFormat8 = 1;
  GLKViewDrawableStencilFormatNone = 0;

// ===== External functions =====

const
  libGLKit = '/System/Library/Frameworks/GLKit.framework/GLKit';

// ===== Typedefs and structs =====
type
{$M+}
  GLKMatrix2 = record
    case Integer of
      0: (m00, m01,
          m10, m11: Single);
      1: (m2: array[0..1, 0..1] of Single);
      2: (m: array[0..3] of Single);
  end;
  PGLKMatrix2 = ^GLKMatrix2;

  GLKMatrix3 = record
    case Integer of
      0: (m00, m01, m02,
          m10, m11, m12,
          m20, m21, m22: Single);
      1: (m: array[0..8] of Single);
  end;
  PGLKMatrix3 = ^GLKMatrix3;

  GLKMatrix4 = record
    case Integer of
      0: (m00, m01, m02, m03,
          m10, m11, m12, m13,
          m20, m21, m22, m23,
          m30, m31, m32, m33: Single);
      1: (m: array[0..15] of Single);
  end;
  PGLKMatrix4 = ^GLKMatrix4;

  GLKVector2 = record
    case Integer of
      0: (x, y: Single);
      1: (s, t: Single);
      2: (v: array[0..1] of Single);
  end;
  PGLKVector = ^GLKVector2;

  GLKVector3 = record
    case Integer of
      0: (x, y, z: Single);
      1: (r, g, b: Single);
      2: (s, t, p: Single);
      3: (v: array[0..2] of Single);
  end;
  PGLKVector3 = ^GLKVector3;

  GLKVector4 = record
    case Integer of
      0: (x, y, z, w: Single);
      1: (r, g, b, a: Single);
      2: (s, t, p, q: Single);
      3: (v: array[0..3] of Single);
  end;
  PGLKVector4 = ^GLKVector4;

  GLKQuaternion = record
    case Integer of
      0: (v: GLKVector3;
          s: Single);
      1: (x, y, z, w: Single);
      2: (q: array[0..3] of Single);
  end;
  PGLKQuaternion = ^GLKQuaternion;

  GLKLightingType = NSUInteger;
  GLKMatrixStackRef = Pointer;
  GLKTextureInfoAlphaState = NSUInteger;
  GLKTextureInfoOrigin = NSUInteger;
  GLKTextureTarget = NSUInteger;
  GLKViewDrawableColorFormat = NSUInteger;
  GLKViewDrawableDepthFormat = NSUInteger;
  GLKViewDrawableStencilFormat = NSUInteger;
  GLKViewDrawableMultisample = NSUInteger;
  P_Bool = ^Boolean;

function GLKMathDegreesToRadians(degrees: Single): Single; cdecl; external libGLKit name _PU + 'GLKMathDegreesToRadians';
function GLKMathProject(object_: GLKVector3; model: GLKMatrix4; projection: GLKMatrix4; viewport: PInteger): GLKVector3; cdecl; external libGLKit name _PU + 'GLKMathProject';
function GLKMathRadiansToDegrees(radians: Single): Single; cdecl; external libGLKit name _PU + 'GLKMathRadiansToDegrees';
function GLKMathUnproject(window: GLKVector3; model: GLKMatrix4; projection: GLKMatrix4; viewport: PInteger; success: P_Bool): GLKVector3; cdecl; external libGLKit name _PU + 'GLKMathUnproject';
function GLKMatrix3Add(matrixLeft: GLKMatrix3; matrixRight: GLKMatrix3): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3Add';
function GLKMatrix3GetColumn(matrix: GLKMatrix3; column: Integer): GLKVector3; cdecl; external libGLKit name _PU + 'GLKMatrix3GetColumn';
function GLKMatrix3GetMatrix2(matrix: GLKMatrix3): GLKMatrix2; cdecl; external libGLKit name _PU + 'GLKMatrix3GetMatrix2';
function GLKMatrix3GetRow(matrix: GLKMatrix3; row: Integer): GLKVector3; cdecl; external libGLKit name _PU + 'GLKMatrix3GetRow';
function GLKMatrix3Invert(matrix: GLKMatrix3; isInvertible: P_Bool): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3Invert';
function GLKMatrix3InvertAndTranspose(matrix: GLKMatrix3; isInvertible: P_Bool): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3InvertAndTranspose';
function GLKMatrix3Make(m00: Single; m01: Single; m02: Single; m10: Single; m11: Single; m12: Single; m20: Single; m21: Single; m22: Single): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3Make';
function GLKMatrix3MakeAndTranspose(m00: Single; m01: Single; m02: Single; m10: Single; m11: Single; m12: Single; m20: Single; m21: Single; m22: Single): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3MakeAndTranspose';
function GLKMatrix3MakeRotation(radians: Single; x: Single; y: Single; z: Single): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3MakeRotation';
function GLKMatrix3MakeScale(sx: Single; sy: Single; sz: Single): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3MakeScale';
function GLKMatrix3MakeWithArray(values: PSingle): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3MakeWithArray';
function GLKMatrix3MakeWithArrayAndTranspose(values: PSingle): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3MakeWithArrayAndTranspose';
function GLKMatrix3MakeWithColumns(column0: GLKVector3; column1: GLKVector3; column2: GLKVector3): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3MakeWithColumns';
function GLKMatrix3MakeWithQuaternion(quaternion: GLKQuaternion): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3MakeWithQuaternion';
function GLKMatrix3MakeWithRows(row0: GLKVector3; row1: GLKVector3; row2: GLKVector3): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3MakeWithRows';
function GLKMatrix3MakeXRotation(radians: Single): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3MakeXRotation';
function GLKMatrix3MakeYRotation(radians: Single): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3MakeYRotation';
function GLKMatrix3MakeZRotation(radians: Single): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3MakeZRotation';
function GLKMatrix3Multiply(matrixLeft: GLKMatrix3; matrixRight: GLKMatrix3): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3Multiply';
function GLKMatrix3MultiplyVector3(matrixLeft: GLKMatrix3; vectorRight: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKMatrix3MultiplyVector3';
procedure GLKMatrix3MultiplyVector3Array(matrix: GLKMatrix3; vectors: PGLKVector3; vectorCount: size_t); cdecl; external libGLKit name _PU + 'GLKMatrix3MultiplyVector3Array';
function GLKMatrix3Rotate(matrix: GLKMatrix3; radians: Single; x: Single; y: Single; z: Single): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3Rotate';
function GLKMatrix3RotateWithVector3(matrix: GLKMatrix3; radians: Single; axisVector: GLKVector3): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3RotateWithVector3';
function GLKMatrix3RotateWithVector4(matrix: GLKMatrix3; radians: Single; axisVector: GLKVector4): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3RotateWithVector4';
function GLKMatrix3RotateX(matrix: GLKMatrix3; radians: Single): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3RotateX';
function GLKMatrix3RotateY(matrix: GLKMatrix3; radians: Single): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3RotateY';
function GLKMatrix3RotateZ(matrix: GLKMatrix3; radians: Single): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3RotateZ';
function GLKMatrix3Scale(matrix: GLKMatrix3; sx: Single; sy: Single; sz: Single): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3Scale';
function GLKMatrix3ScaleWithVector3(matrix: GLKMatrix3; scaleVector: GLKVector3): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3ScaleWithVector3';
function GLKMatrix3ScaleWithVector4(matrix: GLKMatrix3; scaleVector: GLKVector4): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3ScaleWithVector4';
function GLKMatrix3SetColumn(matrix: GLKMatrix3; column: Integer; vector: GLKVector3): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3SetColumn';
function GLKMatrix3SetRow(matrix: GLKMatrix3; row: Integer; vector: GLKVector3): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3SetRow';
function GLKMatrix3Subtract(matrixLeft: GLKMatrix3; matrixRight: GLKMatrix3): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3Subtract';
function GLKMatrix3Transpose(matrix: GLKMatrix3): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix3Transpose';
function GLKMatrix4Add(matrixLeft: GLKMatrix4; matrixRight: GLKMatrix4): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4Add';
function GLKMatrix4GetColumn(matrix: GLKMatrix4; column: Integer): GLKVector4; cdecl; external libGLKit name _PU + 'GLKMatrix4GetColumn';
function GLKMatrix4GetMatrix2(matrix: GLKMatrix4): GLKMatrix2; cdecl; external libGLKit name _PU + 'GLKMatrix4GetMatrix2';
function GLKMatrix4GetMatrix3(matrix: GLKMatrix4): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrix4GetMatrix3';
function GLKMatrix4GetRow(matrix: GLKMatrix4; row: Integer): GLKVector4; cdecl; external libGLKit name _PU + 'GLKMatrix4GetRow';
function GLKMatrix4Invert(matrix: GLKMatrix4; isInvertible: P_Bool): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4Invert';
function GLKMatrix4InvertAndTranspose(matrix: GLKMatrix4; isInvertible: P_Bool): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4InvertAndTranspose';
function GLKMatrix4Make(m00: Single; m01: Single; m02: Single; m03: Single; m10: Single; m11: Single; m12: Single; m13: Single; m20: Single; m21: Single; m22: Single; m23: Single; m30: Single; m31: Single; m32: Single; m33: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4Make';
function GLKMatrix4MakeAndTranspose(m00: Single; m01: Single; m02: Single; m03: Single; m10: Single; m11: Single; m12: Single; m13: Single; m20: Single; m21: Single; m22: Single; m23: Single; m30: Single; m31: Single; m32: Single; m33: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeAndTranspose';
function GLKMatrix4MakeFrustum(left: Single; right: Single; bottom: Single; top: Single; nearZ: Single; farZ: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeFrustum';
function GLKMatrix4MakeLookAt(eyeX: Single; eyeY: Single; eyeZ: Single; centerX: Single; centerY: Single; centerZ: Single; upX: Single; upY: Single; upZ: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeLookAt';
function GLKMatrix4MakeOrtho(left: Single; right: Single; bottom: Single; top: Single; nearZ: Single; farZ: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeOrtho';
function GLKMatrix4MakePerspective(fovyRadians: Single; aspect: Single; nearZ: Single; farZ: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakePerspective';
function GLKMatrix4MakeRotation(radians: Single; x: Single; y: Single; z: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeRotation';
function GLKMatrix4MakeScale(sx: Single; sy: Single; sz: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeScale';
function GLKMatrix4MakeTranslation(tx: Single; ty: Single; tz: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeTranslation';
function GLKMatrix4MakeWithArray(values: PSingle): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeWithArray';
function GLKMatrix4MakeWithArrayAndTranspose(values: PSingle): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeWithArrayAndTranspose';
function GLKMatrix4MakeWithColumns(column0: GLKVector4; column1: GLKVector4; column2: GLKVector4; column3: GLKVector4): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeWithColumns';
function GLKMatrix4MakeWithQuaternion(quaternion: GLKQuaternion): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeWithQuaternion';
function GLKMatrix4MakeWithRows(row0: GLKVector4; row1: GLKVector4; row2: GLKVector4; row3: GLKVector4): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeWithRows';
function GLKMatrix4MakeXRotation(radians: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeXRotation';
function GLKMatrix4MakeYRotation(radians: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeYRotation';
function GLKMatrix4MakeZRotation(radians: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4MakeZRotation';
function GLKMatrix4Multiply(matrixLeft: GLKMatrix4; matrixRight: GLKMatrix4): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4Multiply';
function GLKMatrix4MultiplyAndProjectVector3(matrixLeft: GLKMatrix4; vectorRight: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKMatrix4MultiplyAndProjectVector3';
procedure GLKMatrix4MultiplyAndProjectVector3Array(matrix: GLKMatrix4; vectors: PGLKVector3; vectorCount: size_t); cdecl; external libGLKit name _PU + 'GLKMatrix4MultiplyAndProjectVector3Array';
function GLKMatrix4MultiplyVector3(matrixLeft: GLKMatrix4; vectorRight: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKMatrix4MultiplyVector3';
procedure GLKMatrix4MultiplyVector3Array(matrix: GLKMatrix4; vectors: PGLKVector3; vectorCount: size_t); cdecl; external libGLKit name _PU + 'GLKMatrix4MultiplyVector3Array';
procedure GLKMatrix4MultiplyVector3ArrayWithTranslation(matrix: GLKMatrix4; vectors: PGLKVector3; vectorCount: size_t); cdecl; external libGLKit name _PU + 'GLKMatrix4MultiplyVector3ArrayWithTranslation';
function GLKMatrix4MultiplyVector3WithTranslation(matrixLeft: GLKMatrix4; vectorRight: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKMatrix4MultiplyVector3WithTranslation';
function GLKMatrix4MultiplyVector4(matrixLeft: GLKMatrix4; vectorRight: GLKVector4): GLKVector4; cdecl; external libGLKit name _PU + 'GLKMatrix4MultiplyVector4';
procedure GLKMatrix4MultiplyVector4Array(matrix: GLKMatrix4; vectors: PGLKVector4; vectorCount: size_t); cdecl; external libGLKit name _PU + 'GLKMatrix4MultiplyVector4Array';
function GLKMatrix4Rotate(matrix: GLKMatrix4; radians: Single; x: Single; y: Single; z: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4Rotate';
function GLKMatrix4RotateWithVector3(matrix: GLKMatrix4; radians: Single; axisVector: GLKVector3): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4RotateWithVector3';
function GLKMatrix4RotateWithVector4(matrix: GLKMatrix4; radians: Single; axisVector: GLKVector4): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4RotateWithVector4';
function GLKMatrix4RotateX(matrix: GLKMatrix4; radians: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4RotateX';
function GLKMatrix4RotateY(matrix: GLKMatrix4; radians: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4RotateY';
function GLKMatrix4RotateZ(matrix: GLKMatrix4; radians: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4RotateZ';
function GLKMatrix4Scale(matrix: GLKMatrix4; sx: Single; sy: Single; sz: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4Scale';
function GLKMatrix4ScaleWithVector3(matrix: GLKMatrix4; scaleVector: GLKVector3): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4ScaleWithVector3';
function GLKMatrix4ScaleWithVector4(matrix: GLKMatrix4; scaleVector: GLKVector4): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4ScaleWithVector4';
function GLKMatrix4SetColumn(matrix: GLKMatrix4; column: Integer; vector: GLKVector4): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4SetColumn';
function GLKMatrix4SetRow(matrix: GLKMatrix4; row: Integer; vector: GLKVector4): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4SetRow';
function GLKMatrix4Subtract(matrixLeft: GLKMatrix4; matrixRight: GLKMatrix4): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4Subtract';
function GLKMatrix4Translate(matrix: GLKMatrix4; tx: Single; ty: Single; tz: Single): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4Translate';
function GLKMatrix4TranslateWithVector3(matrix: GLKMatrix4; translationVector: GLKVector3): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4TranslateWithVector3';
function GLKMatrix4TranslateWithVector4(matrix: GLKMatrix4; translationVector: GLKVector4): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4TranslateWithVector4';
function GLKMatrix4Transpose(matrix: GLKMatrix4): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrix4Transpose';
function GLKMatrixStackCreate(alloc: CFAllocatorRef): GLKMatrixStackRef; cdecl; external libGLKit name _PU + 'GLKMatrixStackCreate';
function GLKMatrixStackGetMatrix2(stack: GLKMatrixStackRef): GLKMatrix2; cdecl; external libGLKit name _PU + 'GLKMatrixStackGetMatrix2';
function GLKMatrixStackGetMatrix3(stack: GLKMatrixStackRef): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrixStackGetMatrix3';
function GLKMatrixStackGetMatrix3Inverse(stack: GLKMatrixStackRef): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrixStackGetMatrix3Inverse';
function GLKMatrixStackGetMatrix3InverseTranspose(stack: GLKMatrixStackRef): GLKMatrix3; cdecl; external libGLKit name _PU + 'GLKMatrixStackGetMatrix3InverseTranspose';
function GLKMatrixStackGetMatrix4(stack: GLKMatrixStackRef): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrixStackGetMatrix4';
function GLKMatrixStackGetMatrix4Inverse(stack: GLKMatrixStackRef): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrixStackGetMatrix4Inverse';
function GLKMatrixStackGetMatrix4InverseTranspose(stack: GLKMatrixStackRef): GLKMatrix4; cdecl; external libGLKit name _PU + 'GLKMatrixStackGetMatrix4InverseTranspose';
function GLKMatrixStackGetTypeID: CFTypeID; cdecl; external libGLKit name _PU + 'GLKMatrixStackGetTypeID';
procedure GLKMatrixStackLoadMatrix4(stack: GLKMatrixStackRef; matrix: GLKMatrix4); cdecl; external libGLKit name _PU + 'GLKMatrixStackLoadMatrix4';
procedure GLKMatrixStackMultiplyMatrix4(stack: GLKMatrixStackRef; matrix: GLKMatrix4); cdecl; external libGLKit name _PU + 'GLKMatrixStackMultiplyMatrix4';
procedure GLKMatrixStackMultiplyMatrixStack(stackLeft: GLKMatrixStackRef; stackRight: GLKMatrixStackRef); cdecl; external libGLKit name _PU + 'GLKMatrixStackMultiplyMatrixStack';
procedure GLKMatrixStackPop(stack: GLKMatrixStackRef); cdecl; external libGLKit name _PU + 'GLKMatrixStackPop';
procedure GLKMatrixStackPush(stack: GLKMatrixStackRef); cdecl; external libGLKit name _PU + 'GLKMatrixStackPush';
procedure GLKMatrixStackRotate(stack: GLKMatrixStackRef; radians: Single; x: Single; y: Single; z: Single); cdecl; external libGLKit name _PU + 'GLKMatrixStackRotate';
procedure GLKMatrixStackRotateWithVector3(stack: GLKMatrixStackRef; radians: Single; axisVector: GLKVector3); cdecl; external libGLKit name _PU + 'GLKMatrixStackRotateWithVector3';
procedure GLKMatrixStackRotateWithVector4(stack: GLKMatrixStackRef; radians: Single; axisVector: GLKVector4); cdecl; external libGLKit name _PU + 'GLKMatrixStackRotateWithVector4';
procedure GLKMatrixStackRotateX(stack: GLKMatrixStackRef; radians: Single); cdecl; external libGLKit name _PU + 'GLKMatrixStackRotateX';
procedure GLKMatrixStackRotateY(stack: GLKMatrixStackRef; radians: Single); cdecl; external libGLKit name _PU + 'GLKMatrixStackRotateY';
procedure GLKMatrixStackRotateZ(stack: GLKMatrixStackRef; radians: Single); cdecl; external libGLKit name _PU + 'GLKMatrixStackRotateZ';
procedure GLKMatrixStackScale(stack: GLKMatrixStackRef; sx: Single; sy: Single; sz: Single); cdecl; external libGLKit name _PU + 'GLKMatrixStackScale';
procedure GLKMatrixStackScaleWithVector3(stack: GLKMatrixStackRef; scaleVector: GLKVector3); cdecl; external libGLKit name _PU + 'GLKMatrixStackScaleWithVector3';
procedure GLKMatrixStackScaleWithVector4(stack: GLKMatrixStackRef; scaleVector: GLKVector4); cdecl; external libGLKit name _PU + 'GLKMatrixStackScaleWithVector4';
function GLKMatrixStackSize(stack: GLKMatrixStackRef): Integer; cdecl; external libGLKit name _PU + 'GLKMatrixStackSize';
procedure GLKMatrixStackTranslate(stack: GLKMatrixStackRef; tx: Single; ty: Single; tz: Single); cdecl; external libGLKit name _PU + 'GLKMatrixStackTranslate';
procedure GLKMatrixStackTranslateWithVector3(stack: GLKMatrixStackRef; translationVector: GLKVector3); cdecl; external libGLKit name _PU + 'GLKMatrixStackTranslateWithVector3';
procedure GLKMatrixStackTranslateWithVector4(stack: GLKMatrixStackRef; translationVector: GLKVector4); cdecl; external libGLKit name _PU + 'GLKMatrixStackTranslateWithVector4';
function GLKQuaternionAdd(quaternionLeft: GLKQuaternion; quaternionRight: GLKQuaternion): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionAdd';
function GLKQuaternionAngle(quaternion: GLKQuaternion): Single; cdecl; external libGLKit name _PU + 'GLKQuaternionAngle';
function GLKQuaternionAxis(quaternion: GLKQuaternion): GLKVector3; cdecl; external libGLKit name _PU + 'GLKQuaternionAxis';
function GLKQuaternionConjugate(quaternion: GLKQuaternion): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionConjugate';
function GLKQuaternionInvert(quaternion: GLKQuaternion): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionInvert';
function GLKQuaternionLength(quaternion: GLKQuaternion): Single; cdecl; external libGLKit name _PU + 'GLKQuaternionLength';
function GLKQuaternionMake(x: Single; y: Single; z: Single; w: Single): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionMake';
function GLKQuaternionMakeWithAngleAndAxis(radians: Single; x: Single; y: Single; z: Single): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionMakeWithAngleAndAxis';
function GLKQuaternionMakeWithAngleAndVector3Axis(radians: Single; axisVector: GLKVector3): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionMakeWithAngleAndVector3Axis';
function GLKQuaternionMakeWithArray(values: PSingle): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionMakeWithArray';
function GLKQuaternionMakeWithMatrix3(matrix: GLKMatrix3): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionMakeWithMatrix3';
function GLKQuaternionMakeWithMatrix4(matrix: GLKMatrix4): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionMakeWithMatrix4';
function GLKQuaternionMakeWithVector3(vector: GLKVector3; scalar: Single): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionMakeWithVector3';
function GLKQuaternionMultiply(quaternionLeft: GLKQuaternion; quaternionRight: GLKQuaternion): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionMultiply';
function GLKQuaternionNormalize(quaternion: GLKQuaternion): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionNormalize';
function GLKQuaternionRotateVector3(quaternion: GLKQuaternion; vector: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKQuaternionRotateVector3';
procedure GLKQuaternionRotateVector3Array(quaternion: GLKQuaternion; vectors: PGLKVector3; vectorCount: size_t); cdecl; external libGLKit name _PU + 'GLKQuaternionRotateVector3Array';
function GLKQuaternionRotateVector4(quaternion: GLKQuaternion; vector: GLKVector4): GLKVector4; cdecl; external libGLKit name _PU + 'GLKQuaternionRotateVector4';
procedure GLKQuaternionRotateVector4Array(quaternion: GLKQuaternion; vectors: PGLKVector4; vectorCount: size_t); cdecl; external libGLKit name _PU + 'GLKQuaternionRotateVector4Array';
function GLKQuaternionSlerp(quaternionStart: GLKQuaternion; quaternionEnd: GLKQuaternion; t: Single): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionSlerp';
function GLKQuaternionSubtract(quaternionLeft: GLKQuaternion; quaternionRight: GLKQuaternion): GLKQuaternion; cdecl; external libGLKit name _PU + 'GLKQuaternionSubtract';
function GLKVector2Add(vectorLeft: GLKVector2; vectorRight: GLKVector2): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2Add';
function GLKVector2AddScalar(vector: GLKVector2; value: Single): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2AddScalar';
function GLKVector2AllEqualToScalar(vector: GLKVector2; value: Single): Integer; cdecl; external libGLKit name _PU + 'GLKVector2AllEqualToScalar';
function GLKVector2AllEqualToVector2(vectorLeft: GLKVector2; vectorRight: GLKVector2): Integer; cdecl; external libGLKit name _PU + 'GLKVector2AllEqualToVector2';
function GLKVector2AllGreaterThanOrEqualToScalar(vector: GLKVector2; value: Single): Integer; cdecl; external libGLKit name _PU + 'GLKVector2AllGreaterThanOrEqualToScalar';
function GLKVector2AllGreaterThanOrEqualToVector2(vectorLeft: GLKVector2; vectorRight: GLKVector2): Integer; cdecl; external libGLKit name _PU + 'GLKVector2AllGreaterThanOrEqualToVector2';
function GLKVector2AllGreaterThanScalar(vector: GLKVector2; value: Single): Integer; cdecl; external libGLKit name _PU + 'GLKVector2AllGreaterThanScalar';
function GLKVector2AllGreaterThanVector2(vectorLeft: GLKVector2; vectorRight: GLKVector2): Integer; cdecl; external libGLKit name _PU + 'GLKVector2AllGreaterThanVector2';
function GLKVector2Distance(vectorStart: GLKVector2; vectorEnd: GLKVector2): Single; cdecl; external libGLKit name _PU + 'GLKVector2Distance';
function GLKVector2Divide(vectorLeft: GLKVector2; vectorRight: GLKVector2): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2Divide';
function GLKVector2DivideScalar(vector: GLKVector2; value: Single): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2DivideScalar';
function GLKVector2DotProduct(vectorLeft: GLKVector2; vectorRight: GLKVector2): Single; cdecl; external libGLKit name _PU + 'GLKVector2DotProduct';
function GLKVector2Length(vector: GLKVector2): Single; cdecl; external libGLKit name _PU + 'GLKVector2Length';
function GLKVector2Lerp(vectorStart: GLKVector2; vectorEnd: GLKVector2; t: Single): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2Lerp';
function GLKVector2Make(x: Single; y: Single): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2Make';
function GLKVector2MakeWithArray(values: PSingle): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2MakeWithArray';
function GLKVector2Maximum(vectorLeft: GLKVector2; vectorRight: GLKVector2): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2Maximum';
function GLKVector2Minimum(vectorLeft: GLKVector2; vectorRight: GLKVector2): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2Minimum';
function GLKVector2Multiply(vectorLeft: GLKVector2; vectorRight: GLKVector2): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2Multiply';
function GLKVector2MultiplyScalar(vector: GLKVector2; value: Single): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2MultiplyScalar';
function GLKVector2Negate(vector: GLKVector2): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2Negate';
function GLKVector2Normalize(vector: GLKVector2): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2Normalize';
function GLKVector2Project(vectorToProject: GLKVector2; projectionVector: GLKVector2): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2Project';
function GLKVector2Subtract(vectorLeft: GLKVector2; vectorRight: GLKVector2): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2Subtract';
function GLKVector2SubtractScalar(vector: GLKVector2; value: Single): GLKVector2; cdecl; external libGLKit name _PU + 'GLKVector2SubtractScalar';
function GLKVector3Add(vectorLeft: GLKVector3; vectorRight: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3Add';
function GLKVector3AddScalar(vector: GLKVector3; value: Single): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3AddScalar';
function GLKVector3AllEqualToScalar(vector: GLKVector3; value: Single): Integer; cdecl; external libGLKit name _PU + 'GLKVector3AllEqualToScalar';
function GLKVector3AllEqualToVector3(vectorLeft: GLKVector3; vectorRight: GLKVector3): Integer; cdecl; external libGLKit name _PU + 'GLKVector3AllEqualToVector3';
function GLKVector3AllGreaterThanOrEqualToScalar(vector: GLKVector3; value: Single): Integer; cdecl; external libGLKit name _PU + 'GLKVector3AllGreaterThanOrEqualToScalar';
function GLKVector3AllGreaterThanOrEqualToVector3(vectorLeft: GLKVector3; vectorRight: GLKVector3): Integer; cdecl; external libGLKit name _PU + 'GLKVector3AllGreaterThanOrEqualToVector3';
function GLKVector3AllGreaterThanScalar(vector: GLKVector3; value: Single): Integer; cdecl; external libGLKit name _PU + 'GLKVector3AllGreaterThanScalar';
function GLKVector3AllGreaterThanVector3(vectorLeft: GLKVector3; vectorRight: GLKVector3): Integer; cdecl; external libGLKit name _PU + 'GLKVector3AllGreaterThanVector3';
function GLKVector3CrossProduct(vectorLeft: GLKVector3; vectorRight: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3CrossProduct';
function GLKVector3Distance(vectorStart: GLKVector3; vectorEnd: GLKVector3): Single; cdecl; external libGLKit name _PU + 'GLKVector3Distance';
function GLKVector3Divide(vectorLeft: GLKVector3; vectorRight: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3Divide';
function GLKVector3DivideScalar(vector: GLKVector3; value: Single): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3DivideScalar';
function GLKVector3DotProduct(vectorLeft: GLKVector3; vectorRight: GLKVector3): Single; cdecl; external libGLKit name _PU + 'GLKVector3DotProduct';
function GLKVector3Length(vector: GLKVector3): Single; cdecl; external libGLKit name _PU + 'GLKVector3Length';
function GLKVector3Lerp(vectorStart: GLKVector3; vectorEnd: GLKVector3; t: Single): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3Lerp';
function GLKVector3Make(x: Single; y: Single; z: Single): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3Make';
function GLKVector3MakeWithArray(values: PSingle): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3MakeWithArray';
function GLKVector3Maximum(vectorLeft: GLKVector3; vectorRight: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3Maximum';
function GLKVector3Minimum(vectorLeft: GLKVector3; vectorRight: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3Minimum';
function GLKVector3Multiply(vectorLeft: GLKVector3; vectorRight: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3Multiply';
function GLKVector3MultiplyScalar(vector: GLKVector3; value: Single): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3MultiplyScalar';
function GLKVector3Negate(vector: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3Negate';
function GLKVector3Normalize(vector: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3Normalize';
function GLKVector3Project(vectorToProject: GLKVector3; projectionVector: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3Project';
function GLKVector3Subtract(vectorLeft: GLKVector3; vectorRight: GLKVector3): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3Subtract';
function GLKVector3SubtractScalar(vector: GLKVector3; value: Single): GLKVector3; cdecl; external libGLKit name _PU + 'GLKVector3SubtractScalar';
function GLKVector4Add(vectorLeft: GLKVector4; vectorRight: GLKVector4): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4Add';
function GLKVector4AddScalar(vector: GLKVector4; value: Single): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4AddScalar';
function GLKVector4AllEqualToScalar(vector: GLKVector4; value: Single): Integer; cdecl; external libGLKit name _PU + 'GLKVector4AllEqualToScalar';
function GLKVector4AllEqualToVector4(vectorLeft: GLKVector4; vectorRight: GLKVector4): Integer; cdecl; external libGLKit name _PU + 'GLKVector4AllEqualToVector4';
function GLKVector4AllGreaterThanOrEqualToScalar(vector: GLKVector4; value: Single): Integer; cdecl; external libGLKit name _PU + 'GLKVector4AllGreaterThanOrEqualToScalar';
function GLKVector4AllGreaterThanOrEqualToVector4(vectorLeft: GLKVector4; vectorRight: GLKVector4): Integer; cdecl; external libGLKit name _PU + 'GLKVector4AllGreaterThanOrEqualToVector4';
function GLKVector4AllGreaterThanScalar(vector: GLKVector4; value: Single): Integer; cdecl; external libGLKit name _PU + 'GLKVector4AllGreaterThanScalar';
function GLKVector4AllGreaterThanVector4(vectorLeft: GLKVector4; vectorRight: GLKVector4): Integer; cdecl; external libGLKit name _PU + 'GLKVector4AllGreaterThanVector4';
function GLKVector4CrossProduct(vectorLeft: GLKVector4; vectorRight: GLKVector4): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4CrossProduct';
function GLKVector4Distance(vectorStart: GLKVector4; vectorEnd: GLKVector4): Single; cdecl; external libGLKit name _PU + 'GLKVector4Distance';
function GLKVector4Divide(vectorLeft: GLKVector4; vectorRight: GLKVector4): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4Divide';
function GLKVector4DivideScalar(vector: GLKVector4; value: Single): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4DivideScalar';
function GLKVector4DotProduct(vectorLeft: GLKVector4; vectorRight: GLKVector4): Single; cdecl; external libGLKit name _PU + 'GLKVector4DotProduct';
function GLKVector4Length(vector: GLKVector4): Single; cdecl; external libGLKit name _PU + 'GLKVector4Length';
function GLKVector4Lerp(vectorStart: GLKVector4; vectorEnd: GLKVector4; t: Single): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4Lerp';
function GLKVector4Make(x: Single; y: Single; z: Single; w: Single): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4Make';
function GLKVector4MakeWithArray(values: PSingle): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4MakeWithArray';
function GLKVector4MakeWithVector3(vector: GLKVector3; w: Single): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4MakeWithVector3';
function GLKVector4Maximum(vectorLeft: GLKVector4; vectorRight: GLKVector4): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4Maximum';
function GLKVector4Minimum(vectorLeft: GLKVector4; vectorRight: GLKVector4): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4Minimum';
function GLKVector4Multiply(vectorLeft: GLKVector4; vectorRight: GLKVector4): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4Multiply';
function GLKVector4MultiplyScalar(vector: GLKVector4; value: Single): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4MultiplyScalar';
function GLKVector4Negate(vector: GLKVector4): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4Negate';
function GLKVector4Normalize(vector: GLKVector4): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4Normalize';
function GLKVector4Project(vectorToProject: GLKVector4; projectionVector: GLKVector4): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4Project';
function GLKVector4Subtract(vectorLeft: GLKVector4; vectorRight: GLKVector4): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4Subtract';
function GLKVector4SubtractScalar(vector: GLKVector4; value: Single): GLKVector4; cdecl; external libGLKit name _PU + 'GLKVector4SubtractScalar';
function NSStringFromGLKMatrix2(matrix: GLKMatrix2): PNSString; cdecl; external libGLKit name _PU + 'NSStringFromGLKMatrix2';
function NSStringFromGLKMatrix3(matrix: GLKMatrix3): PNSString; cdecl; external libGLKit name _PU + 'NSStringFromGLKMatrix3';
function NSStringFromGLKMatrix4(matrix: GLKMatrix4): PNSString; cdecl; external libGLKit name _PU + 'NSStringFromGLKMatrix4';
function NSStringFromGLKQuaternion(quaternion: GLKQuaternion): PNSString; cdecl; external libGLKit name _PU + 'NSStringFromGLKQuaternion';
function NSStringFromGLKVector2(vector: GLKVector2): PNSString; cdecl; external libGLKit name _PU + 'NSStringFromGLKVector2';
function NSStringFromGLKVector3(vector: GLKVector3): PNSString; cdecl; external libGLKit name _PU + 'NSStringFromGLKVector3';
function NSStringFromGLKVector4(vector: GLKVector4): PNSString; cdecl; external libGLKit name _PU + 'NSStringFromGLKVector4';

type
{$M+}
// ===== Forward declarations =====

  GLKViewControllerDelegate = interface;
  GLKViewDelegate = interface;

  GLKTextureInfo = interface;
  GLKSkyboxEffect = interface;
  GLKViewController = interface;
  GLKView = interface;
  GLKTextureLoader = interface;
  GLKEffectProperty = interface;
  GLKEffectPropertyLight = interface;
  GLKEffectPropertyFog = interface;
  GLKBaseEffect = interface;
  GLKEffectPropertyTransform = interface;
  GLKEffectPropertyTexture = interface;
  GLKEffectPropertyMaterial = interface;
  GLKReflectionMapEffect = interface;

// ===== Protocol declarations =====

  GLKNamedEffect = interface
    ['{0819BD68-3DB4-41B5-8FE4-1D12653231A2}']
    procedure prepareToDraw; cdecl;
  end;
  GLKViewControllerDelegate = interface(IObjectiveC)
    ['{F716096A-E94D-46FB-8E85-A9964533B1F6}']
    procedure glkViewController(controller: GLKViewController; willPause: Boolean); cdecl;
    procedure glkViewControllerUpdate(controller: GLKViewController); cdecl;
  end;
  GLKViewDelegate = interface(IObjectiveC)
    ['{13D41829-A7B1-4DCD-BDE4-FB66DC64655D}']
    procedure glkView(view: GLKView; drawInRect: CGRect); cdecl;
  end;

// ===== Interface declarations =====

  GLKTextureInfoClass = interface(NSObjectClass)
    ['{44F4BC8F-A442-4EBF-A8BC-A19B19BE5301}']
  end;
  GLKTextureInfo = interface(NSObject)
    ['{07470F2B-8B29-41DF-83F4-4DEB6405D568}']
    function alphaState: GLKTextureInfoAlphaState; cdecl;
    function containsMipmaps: Boolean; cdecl;
    function height: GLuint; cdecl;
    function name: GLuint; cdecl;
    function target: GLenum; cdecl;
    function textureOrigin: GLKTextureInfoOrigin; cdecl;
    function width: GLuint; cdecl;
  end;
  TGLKTextureInfo = class(TOCGenericImport<GLKTextureInfoClass, GLKTextureInfo>)  end;

  GLKSkyboxEffectClass = interface(NSObjectClass)
    ['{50B752C5-C8CE-41D3-A7B9-33A3293416B2}']
  end;
  GLKSkyboxEffect = interface(NSObject)
    ['{5FFDD85F-8ED1-4B2F-9F72-561B916515B3}']
    function center: GLKVector3; cdecl;
    procedure draw; cdecl;
    procedure prepareToDraw; cdecl;
    procedure setCenter(center: GLKVector3); cdecl;
    procedure setLabel(label_: NSString); cdecl;
    procedure setXSize(xSize: GLfloat); cdecl;
    procedure setYSize(ySize: GLfloat); cdecl;
    procedure setZSize(zSize: GLfloat); cdecl;
    function textureCubeMap: GLKEffectPropertyTexture; cdecl;
    function transform: GLKEffectPropertyTransform; cdecl;
    function xSize: GLfloat; cdecl;
    function ySize: GLfloat; cdecl;
    function zSize: GLfloat; cdecl;
  end;
  TGLKSkyboxEffect = class(TOCGenericImport<GLKSkyboxEffectClass, GLKSkyboxEffect>)  end;

  GLKViewControllerClass = interface(UIViewControllerClass)
    ['{B91BC5ED-6D33-41BD-ADEA-F26BFEB2FDD2}']
  end;
  GLKViewController = interface(UIViewController)
    ['{823166A6-D4CB-42C5-ADB9-C4E00EB11132}']
    function delegate: Pointer; cdecl;
    function framesDisplayed: NSInteger; cdecl;
    function framesPerSecond: NSInteger; cdecl;
    function isPaused: Boolean; cdecl;
    function pauseOnWillResignActive: Boolean; cdecl;
    function preferredFramesPerSecond: NSInteger; cdecl;
    function resumeOnDidBecomeActive: Boolean; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPauseOnWillResignActive(pauseOnWillResignActive: Boolean); cdecl;
    procedure setPaused(paused: Boolean); cdecl;
    procedure setPreferredFramesPerSecond(preferredFramesPerSecond: NSInteger); cdecl;
    procedure setResumeOnDidBecomeActive(resumeOnDidBecomeActive: Boolean); cdecl;
    function timeSinceFirstResume: NSTimeInterval; cdecl;
    function timeSinceLastDraw: NSTimeInterval; cdecl;
    function timeSinceLastResume: NSTimeInterval; cdecl;
    function timeSinceLastUpdate: NSTimeInterval; cdecl;
  end;
  TGLKViewController = class(TOCGenericImport<GLKViewControllerClass, GLKViewController>)  end;

  GLKViewClass = interface(UIViewClass)
    ['{28B9B440-B979-4CEB-87FC-91EF1B04272F}']
  end;
  GLKView = interface(UIView)
    ['{F81E4E34-668B-429C-A14B-F85BC87B3E7D}']
    procedure bindDrawable; cdecl;
    function context: EAGLContext; cdecl;
    function delegate: Pointer; cdecl;
    procedure deleteDrawable; cdecl;
    procedure display; cdecl;
    function drawableColorFormat: GLKViewDrawableColorFormat; cdecl;
    function drawableDepthFormat: GLKViewDrawableDepthFormat; cdecl;
    function drawableHeight: NSInteger; cdecl;
    function drawableMultisample: GLKViewDrawableMultisample; cdecl;
    function drawableStencilFormat: GLKViewDrawableStencilFormat; cdecl;
    function drawableWidth: NSInteger; cdecl;
    function enableSetNeedsDisplay: Boolean; cdecl;
    function initWithFrame(frame: CGRect; context: EAGLContext): Pointer; cdecl;
    procedure setContext(context: EAGLContext); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDrawableColorFormat(drawableColorFormat: GLKViewDrawableColorFormat); cdecl;
    procedure setDrawableDepthFormat(drawableDepthFormat: GLKViewDrawableDepthFormat); cdecl;
    procedure setDrawableMultisample(drawableMultisample: GLKViewDrawableMultisample); cdecl;
    procedure setDrawableStencilFormat(drawableStencilFormat: GLKViewDrawableStencilFormat); cdecl;
    procedure setEnableSetNeedsDisplay(enableSetNeedsDisplay: Boolean); cdecl;
    function snapshot: UIImage; cdecl;
  end;
  TGLKView = class(TOCGenericImport<GLKViewClass, GLKView>)  end;

  GLKTextureLoaderClass = interface(NSObjectClass)
    ['{1ABC3371-538E-49E3-B446-25474C452F53}']
    {class} function cubeMapWithContentsOfFile(path: NSString; options: NSDictionary; error: NSError): GLKTextureInfo; cdecl; overload;
    {class} function cubeMapWithContentsOfFiles(paths: NSArray; options: NSDictionary; error: NSError): GLKTextureInfo; cdecl; overload;
    {class} function cubeMapWithContentsOfURL(url: NSURL; options: NSDictionary; error: NSError): GLKTextureInfo; cdecl; overload;
    {class} function textureWithCGImage(cgImage: CGImageRef; options: NSDictionary; error: NSError): GLKTextureInfo; cdecl; overload;
    {class} function textureWithContentsOfData(data: NSData; options: NSDictionary; error: NSError): GLKTextureInfo; cdecl; overload;
    {class} function textureWithContentsOfFile(path: NSString; options: NSDictionary; error: NSError): GLKTextureInfo; cdecl; overload;
    {class} function textureWithContentsOfURL(url: NSURL; options: NSDictionary; error: NSError): GLKTextureInfo; cdecl; overload;
  end;
  GLKTextureLoader = interface(NSObject)
    ['{55813C5A-7908-4E42-BF43-713A81BFBE98}']
    function initWithSharegroup(sharegroup: EAGLSharegroup): Pointer; cdecl;
  end;
  TGLKTextureLoader = class(TOCGenericImport<GLKTextureLoaderClass, GLKTextureLoader>)  end;

  GLKEffectPropertyClass = interface(NSObjectClass)
    ['{8A72FF03-5841-4119-AE77-0D1412173280}']
  end;
  GLKEffectProperty = interface(NSObject)
    ['{2EEF7FEB-628C-4E41-9A4F-B6D0956A682C}']
  end;
  TGLEffectProperty = class(TOCGenericImport<GLKEffectPropertyClass, GLKEffectProperty>)  end;

  GLKEffectPropertyLightClass = interface(GLKEffectPropertyClass)
    ['{F44FD652-CE58-48A4-ADED-D00532D2D7CA}']
  end;
  GLKEffectPropertyLight = interface(GLKEffectProperty)
    ['{420CAF92-DD2B-4884-B9BC-692831D43B2D}']
    function ambientColor: GLKVector4; cdecl;
    function constantAttenuation: GLfloat; cdecl;
    function diffuseColor: GLKVector4; cdecl;
    function enabled: GLboolean; cdecl;
    function linearAttenuation: GLfloat; cdecl;
    function position: GLKVector4; cdecl;
    function quadraticAttenuation: GLfloat; cdecl;
    procedure setAmbientColor(ambientColor: GLKVector4); cdecl;
    procedure setConstantAttenuation(constantAttenuation: GLfloat); cdecl;
    procedure setDiffuseColor(diffuseColor: GLKVector4); cdecl;
    procedure setEnabled(enabled: GLboolean); cdecl;
    procedure setLinearAttenuation(linearAttenuation: GLfloat); cdecl;
    procedure setPosition(position: GLKVector4); cdecl;
    procedure setQuadraticAttenuation(quadraticAttenuation: GLfloat); cdecl;
    procedure setSpecularColor(specularColor: GLKVector4); cdecl;
    procedure setSpotCutoff(spotCutoff: GLfloat); cdecl;
    procedure setSpotDirection(spotDirection: GLKVector3); cdecl;
    procedure setSpotExponent(spotExponent: GLfloat); cdecl;
    procedure setTransform(transform: GLKEffectPropertyTransform); cdecl;
    function specularColor: GLKVector4; cdecl;
    function spotCutoff: GLfloat; cdecl;
    function spotDirection: GLKVector3; cdecl;
    function spotExponent: GLfloat; cdecl;
    function transform: GLKEffectPropertyTransform; cdecl;
  end;
  TGLKEffectPropertyLight = class(TOCGenericImport<GLKEffectPropertyLightClass, GLKEffectPropertyLight>)  end;

  GLKEffectPropertyFogClass = interface(GLKEffectPropertyClass)
    ['{220A723B-C579-498E-BF75-A933C7484D17}']
  end;
  GLKEffectPropertyFog = interface(GLKEffectProperty)
    ['{7ADF9206-B02D-48DD-B5C0-01DB33FFE5ED}']
    function color: GLKVector4; cdecl;
    function density: GLfloat; cdecl;
    function enabled: GLboolean; cdecl;
    function mode: GLint; cdecl;
    procedure setColor(color: GLKVector4); cdecl;
    procedure setDensity(density: GLfloat); cdecl;
    procedure setEnabled(enabled: GLboolean); cdecl;
    procedure setEnd(end_: GLfloat); cdecl;
    procedure setMode(mode: GLint); cdecl;
    procedure setStart(start: GLfloat); cdecl;
    function start: GLfloat; cdecl;
  end;
  TGLKEffectPropertyFog = class(TOCGenericImport<GLKEffectPropertyFogClass, GLKEffectPropertyFog>)  end;

  GLKBaseEffectClass = interface(NSObjectClass)
    ['{C97FCB25-126C-4D01-90BF-CE5401ECE7F9}']
  end;
  GLKBaseEffect = interface(NSObject)
    ['{4164AE3C-7A4D-4140-8635-EB42B66EB923}']
    function colorMaterialEnabled: GLboolean; cdecl;
    function constantColor: GLKVector4; cdecl;
    function fog: GLKEffectPropertyFog; cdecl;
    function light0: GLKEffectPropertyLight; cdecl;
    function light1: GLKEffectPropertyLight; cdecl;
    function light2: GLKEffectPropertyLight; cdecl;
    function lightModelAmbientColor: GLKVector4; cdecl;
    function lightModelTwoSided: GLboolean; cdecl;
    function lightingType: GLKLightingType; cdecl;
    function material: GLKEffectPropertyMaterial; cdecl;
    procedure prepareToDraw; cdecl;
    procedure setColorMaterialEnabled(colorMaterialEnabled: GLboolean); cdecl;
    procedure setConstantColor(constantColor: GLKVector4); cdecl;
    procedure setLabel(label_: NSString); cdecl;
    procedure setLightModelAmbientColor(lightModelAmbientColor: GLKVector4); cdecl;
    procedure setLightModelTwoSided(lightModelTwoSided: GLboolean); cdecl;
    procedure setLightingType(lightingType: GLKLightingType); cdecl;
    procedure setTextureOrder(textureOrder: NSArray); cdecl;
    procedure setUseConstantColor(useConstantColor: GLboolean); cdecl;
    function texture2d0: GLKEffectPropertyTexture; cdecl;
    function texture2d1: GLKEffectPropertyTexture; cdecl;
    function textureOrder: NSArray; cdecl;
    function transform: GLKEffectPropertyTransform; cdecl;
    function useConstantColor: GLboolean; cdecl;
  end;
  TGLKBaseEffect = class(TOCGenericImport<GLKBaseEffectClass, GLKBaseEffect>)  end;

  GLKEffectPropertyTransformClass = interface(GLKEffectPropertyClass)
    ['{04465087-0655-4DA3-B871-DA77572BFEF9}']
  end;
  GLKEffectPropertyTransform = interface(GLKEffectProperty)
    ['{ED7132EF-83F1-41B2-8558-41D6B784C7F8}']
    function modelviewMatrix: GLKMatrix4; cdecl;
    function normalMatrix: GLKMatrix3; cdecl;
    function projectionMatrix: GLKMatrix4; cdecl;
    procedure setModelviewMatrix(modelviewMatrix: GLKMatrix4); cdecl;
    procedure setProjectionMatrix(projectionMatrix: GLKMatrix4); cdecl;
  end;
  TGLKEffectPropertyTransform = class(TOCGenericImport<GLKEffectPropertyTransformClass, GLKEffectPropertyTransform>)  end;

  GLKEffectPropertyTextureClass = interface(GLKEffectPropertyClass)
    ['{F284E6B0-98FE-45BE-B496-D0FE3AEC0A6C}']
  end;
  GLKEffectPropertyTexture = interface(GLKEffectProperty)
    ['{9AC6CE6F-FBE8-47EF-897B-25EB72819A19}']
    function enabled: GLboolean; cdecl;
    function envMode: GLint; cdecl;
    function name: GLuint; cdecl;
    procedure setEnabled(enabled: GLboolean); cdecl;
    procedure setEnvMode(envMode: GLint); cdecl;
    procedure setName(name: GLuint); cdecl;
    procedure setTarget(target: GLKTextureTarget); cdecl;
    function target: GLKTextureTarget; cdecl;
  end;
  TGLKEffectPropertyTexture = class(TOCGenericImport<GLKEffectPropertyTextureClass, GLKEffectPropertyTexture>)  end;

  GLKEffectPropertyMaterialClass = interface(GLKEffectPropertyClass)
    ['{95C31DE9-794B-44C1-A01E-1978900BFC57}']
  end;
  GLKEffectPropertyMaterial = interface(GLKEffectProperty)
    ['{0B9C5659-94CF-47BB-A84C-D16745ADE8DA}']
    function ambientColor: GLKVector4; cdecl;
    function diffuseColor: GLKVector4; cdecl;
    function emissiveColor: GLKVector4; cdecl;
    procedure setAmbientColor(ambientColor: GLKVector4); cdecl;
    procedure setDiffuseColor(diffuseColor: GLKVector4); cdecl;
    procedure setEmissiveColor(emissiveColor: GLKVector4); cdecl;
    procedure setShininess(shininess: GLfloat); cdecl;
    procedure setSpecularColor(specularColor: GLKVector4); cdecl;
    function shininess: GLfloat; cdecl;
    function specularColor: GLKVector4; cdecl;
  end;
  TGLKEffectPropertyMaterial = class(TOCGenericImport<GLKEffectPropertyMaterialClass, GLKEffectPropertyMaterial>)  end;

  GLKReflectionMapEffectClass = interface(GLKBaseEffectClass)
    ['{85104CD2-15E8-4D84-BEF8-9D9DEE7BF6EB}']
  end;
  GLKReflectionMapEffect = interface(GLKBaseEffect)
    ['{14D4F2D4-FACF-4F8F-940C-E52A22FF8037}']
    function matrix: GLKMatrix3; cdecl;
    procedure prepareToDraw; cdecl;
    procedure setMatrix(matrix: GLKMatrix3); cdecl;
    function textureCubeMap: GLKEffectPropertyTexture; cdecl;
  end;
  TGLKReflectionMapEffect = class(TOCGenericImport<GLKReflectionMapEffectClass, GLKReflectionMapEffect>)  end;

implementation

{$IF defined(IOS) and NOT defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  GLModule: THandle;

initialization
  GLModule := dlopen(MarshaledAString(libGLKit), RTLD_LAZY);

finalization
  dlclose(GLModule);
{$ENDIF IOS}

end.
