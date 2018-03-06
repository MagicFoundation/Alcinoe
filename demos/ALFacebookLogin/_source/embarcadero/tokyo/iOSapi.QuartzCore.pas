{**********************************************************}
{                                                          }
{           CodeGear Delphi Runtime Library                }
{                                                          }
{ Delphi-Objective-C Bridge                                }
{ Interfaces for Cocoa framework QuartzCore                }
{                                                          }
{ Copyright (c) 2004 Apple Computer, Inc.                  }
{ All rights reserved.                                     }
{                                                          }
{ Translator: Embarcadero Technologies, Inc.               }
{   Copyright(c) 2012-2017 Embarcadero Technologies, Inc.  }
{              All rights reserved                         }
{                                                          }
{**********************************************************}

unit iOSapi.QuartzCore;

interface

uses
  Posix.StdDef, Macapi.ObjCRuntime, Macapi.ObjectiveC, iOSapi.CocoaTypes, Macapi.CoreFoundation,
  iOSapi.Foundation, iOSapi.CoreGraphics;

const
  CA_WARN_DEPRECATED = 1;
  kCALayerBottomEdge = 4;
  kCALayerLeftEdge = 1;
  kCALayerRightEdge = 2;
  kCALayerTopEdge = 8;

// ===== Typedefs and structs =====

type
  CATransform3D = record
    m11, m12, m13, m14: CGFloat;
    m21, m22, m23, m24: CGFloat;
    m31, m32, m33, m34: CGFloat;
    m41, m42, m43, m44: CGFloat;
  end;

// ===== External functions =====

const
  libQuartzCore = '/System/Library/Frameworks/QuartzCore.framework/QuartzCore';

function CACurrentMediaTime: CFTimeInterval; cdecl; external libQuartzCore name _PU + 'CACurrentMediaTime';
function CATransform3DConcat(a: CATransform3D; b: CATransform3D): CATransform3D; cdecl; external libQuartzCore name _PU + 'CATransform3DConcat';
function CATransform3DEqualToTransform(a: CATransform3D; b: CATransform3D): Integer; cdecl; external libQuartzCore name _PU + 'CATransform3DEqualToTransform';
function CATransform3DGetAffineTransform(t: CATransform3D): CGAffineTransform; cdecl; external libQuartzCore name _PU + 'CATransform3DGetAffineTransform';
function CATransform3DInvert(t: CATransform3D): CATransform3D; cdecl; external libQuartzCore name _PU + 'CATransform3DInvert';
function CATransform3DIsAffine(t: CATransform3D): Integer; cdecl; external libQuartzCore name _PU + 'CATransform3DIsAffine';
function CATransform3DIsIdentity(t: CATransform3D): Integer; cdecl; external libQuartzCore name _PU + 'CATransform3DIsIdentity';
function CATransform3DMakeAffineTransform(m: CGAffineTransform): CATransform3D; cdecl; external libQuartzCore name _PU + 'CATransform3DMakeAffineTransform';
function CATransform3DMakeRotation(angle: CGFloat; x: CGFloat; y: CGFloat; z: CGFloat): CATransform3D; cdecl; external libQuartzCore name _PU + 'CATransform3DMakeRotation';
function CATransform3DMakeScale(sx: CGFloat; sy: CGFloat; sz: CGFloat): CATransform3D; cdecl; external libQuartzCore name _PU + 'CATransform3DMakeScale';
function CATransform3DMakeTranslation(tx: CGFloat; ty: CGFloat; tz: CGFloat): CATransform3D; cdecl; external libQuartzCore name _PU + 'CATransform3DMakeTranslation';
function CATransform3DRotate(t: CATransform3D; angle: CGFloat; x: CGFloat; y: CGFloat; z: CGFloat): CATransform3D; cdecl; external libQuartzCore name _PU + 'CATransform3DRotate';
function CATransform3DScale(t: CATransform3D; sx: CGFloat; sy: CGFloat; sz: CGFloat): CATransform3D; cdecl; external libQuartzCore name _PU + 'CATransform3DScale';
function CATransform3DTranslate(t: CATransform3D; tx: CGFloat; ty: CGFloat; tz: CGFloat): CATransform3D; cdecl; external libQuartzCore name _PU + 'CATransform3DTranslate';

type
{$M+}
// ===== Forward declarations =====


// ===== Protocol declarations =====

  CAAction = interface
    ['{15114D7C-C969-4806-B462-2C8FF862C6EB}']
  end;
  CAMediaTiming = interface
    ['{20987BC7-DF3C-43F2-BA93-C24E6860C279}']
    function autoreverses: Boolean; cdecl;
    function beginTime: CFTimeInterval; cdecl;
    function duration: CFTimeInterval; cdecl;
    function fillMode: NSString; cdecl;
    function repeatCount: Single; cdecl;
    function repeatDuration: CFTimeInterval; cdecl;
    procedure setAutoreverses(autoreverses: Boolean); cdecl;
    procedure setBeginTime(beginTime: CFTimeInterval); cdecl;
    procedure setDuration(duration: CFTimeInterval); cdecl;
    procedure setFillMode(fillMode: NSString); cdecl;
    procedure setRepeatCount(repeatCount: Single); cdecl;
    procedure setRepeatDuration(repeatDuration: CFTimeInterval); cdecl;
    procedure setSpeed(speed: Single); cdecl;
    procedure setTimeOffset(timeOffset: CFTimeInterval); cdecl;
    function speed: Single; cdecl;
    function timeOffset: CFTimeInterval; cdecl;
  end;


type
{$M+}
// ===== Forward declarations =====

  CATransaction = interface;
  CAValueFunction = interface;
  CAMediaTimingFunction = interface;
  CALayer = interface;
  CAEmitterCell = interface;
  NSValue = interface;
  CAAnimation = interface;
  CADisplayLink = interface;
  CATextLayer = interface;
  CAShapeLayer = interface;
  CATransition = interface;
  CATiledLayer = interface;
  CAAnimationGroup = interface;
  CAGradientLayer = interface;
  CAEmitterLayer = interface;
  CAScrollLayer = interface;
  CAReplicatorLayer = interface;
  CAPropertyAnimation = interface;
  CABasicAnimation = interface;
  CAKeyframeAnimation = interface;

// ===== Interface declarations =====

  CATransactionClass = interface(NSObjectClass)
    ['{9CAAECB3-D949-4262-92A7-5E7359D9C8BE}']
    {class} function animationDuration: CFTimeInterval; cdecl;
    {class} function animationTimingFunction: CAMediaTimingFunction; cdecl;
    {class} procedure commit; cdecl;
//    {class} function completionBlock: void (^)(void); cdecl;
    {class} function disableActions: Boolean; cdecl;
    {class} procedure flush; cdecl;
    {class} procedure lock; cdecl;
    {class} procedure setAnimationDuration(dur: CFTimeInterval); cdecl;
    {class} procedure setAnimationTimingFunction(function_: CAMediaTimingFunction); cdecl;
    {class} procedure setDisableActions(flag: Boolean); cdecl;
    {class} procedure setValue(anObject: Pointer; forKey: NSString); cdecl;
    {class} procedure unlock; cdecl;
    {class} function valueForKey(key: NSString): Pointer; cdecl;
  end;
  CATransaction = interface(NSObject)
    ['{557D88F0-2192-4636-86DC-5B3246DB742B}']
  end;
  TCATransaction = class(TOCGenericImport<CATransactionClass, CATransaction>)  end;

  CAValueFunctionClass = interface(NSObjectClass)
    ['{BFDA3DBE-5E13-43C4-AC3E-66477865C928}']
    {class} function functionWithName(name: NSString): Pointer; cdecl;
  end;
  CAValueFunction = interface(NSObject)
    ['{708C0B0B-46E7-4DCE-A79C-5880F52D3256}']
    function name: NSString; cdecl;
  end;
  TCAValueFunction = class(TOCGenericImport<CAValueFunctionClass, CAValueFunction>)  end;

  CAMediaTimingFunctionClass = interface(NSObjectClass)
    ['{5A1E7ED1-6689-46CE-9B00-D9CD6045750E}']
    {class} function functionWithName(name: NSString): Pointer; cdecl;
  end;
  CAMediaTimingFunction = interface(NSObject)
    ['{A18855C9-EC88-40E8-888E-03D16D12479F}']
    procedure getControlPointAtIndex(idx: size_t; values: PSingle); cdecl;
  end;
  TCAMediaTimingFunction = class(TOCGenericImport<CAMediaTimingFunctionClass, CAMediaTimingFunction>)  end;

  CALayerClass = interface(NSObjectClass)
    ['{3AA239A4-7FA2-454E-ACA0-FAEEDE874CD9}']
    {class} function defaultActionForKey(event: NSString): Pointer; cdecl;
    {class} function defaultValueForKey(key: NSString): Pointer; cdecl;
    {class} function layer: Pointer; cdecl;
    {class} function needsDisplayForKey(key: NSString): Boolean; cdecl;
  end;
  CALayer = interface(NSObject)
    ['{BDD252B7-74C1-4EBC-947C-55D87479E783}']
    function actionForKey(event: NSString): Pointer; cdecl;
    function actions: NSDictionary; cdecl;
    procedure addAnimation(anim: CAAnimation; forKey: NSString); cdecl;
    procedure addSublayer(layer: CALayer); cdecl;
    function affineTransform: CGAffineTransform; cdecl;
    function anchorPoint: CGPoint; cdecl;
    function anchorPointZ: CGFloat; cdecl;
    function animationForKey(key: NSString): CAAnimation; cdecl;
    function animationKeys: NSArray; cdecl;
    function backgroundColor: CGColorRef; cdecl;
    function backgroundFilters: NSArray; cdecl;
    function borderColor: CGColorRef; cdecl;
    function borderWidth: CGFloat; cdecl;
    function bounds: CGRect; cdecl;
    function compositingFilter: Pointer; cdecl;
    function containsPoint(p: CGPoint): Boolean; cdecl;
    function contents: Pointer; cdecl;
    function contentsAreFlipped: Boolean; cdecl;
    function contentsCenter: CGRect; cdecl;
    function contentsGravity: NSString; cdecl;
    function contentsRect: CGRect; cdecl;
    function contentsScale: CGFloat; cdecl;
    function convertPoint(p: CGPoint; fromLayer: CALayer): CGPoint; cdecl; overload;
//    function convertPoint(p: CGPoint; toLayer: CALayer): CGPoint; cdecl; overload;
    function convertRect(r: CGRect; fromLayer: CALayer): CGRect; cdecl; overload;
//    function convertRect(r: CGRect; toLayer: CALayer): CGRect; cdecl; overload;
    function convertTime(t: CFTimeInterval; fromLayer: CALayer): CFTimeInterval; cdecl; overload;
//    function convertTime(t: CFTimeInterval; toLayer: CALayer): CFTimeInterval; cdecl; overload;
    function cornerRadius: CGFloat; cdecl;
    function delegate: Pointer; cdecl;
    procedure display; cdecl;
    procedure displayIfNeeded; cdecl;
    procedure drawInContext(ctx: CGContextRef); cdecl;
    function edgeAntialiasingMask: Cardinal; cdecl;
    function filters: NSArray; cdecl;
    function frame: CGRect; cdecl;
    function hitTest(p: CGPoint): CALayer; cdecl;
    function init: Pointer; cdecl;
    function initWithLayer(layer: Pointer): Pointer; cdecl;
    procedure insertSublayer(layer: CALayer; above: CALayer); cdecl; overload;
    procedure insertSublayer(layer: CALayer; atIndex: Cardinal); cdecl; overload;
//    procedure insertSublayer(layer: CALayer; below: CALayer); cdecl; overload;
    function isDoubleSided: Boolean; cdecl;
    function isGeometryFlipped: Boolean; cdecl;
    function isHidden: Boolean; cdecl;
    function isOpaque: Boolean; cdecl;
    procedure layoutIfNeeded; cdecl;
    procedure layoutSublayers; cdecl;
    function magnificationFilter: NSString; cdecl;
    function mask: CALayer; cdecl;
    function masksToBounds: Boolean; cdecl;
    function minificationFilter: NSString; cdecl;
    function minificationFilterBias: Single; cdecl;
    function modelLayer: Pointer; cdecl;
    function name: NSString; cdecl;
    function needsDisplay: Boolean; cdecl;
    function needsDisplayOnBoundsChange: Boolean; cdecl;
    function needsLayout: Boolean; cdecl;
    function opacity: Single; cdecl;
    function position: CGPoint; cdecl;
    function preferredFrameSize: CGSize; cdecl;
    function presentationLayer: Pointer; cdecl;
    function rasterizationScale: CGFloat; cdecl;
    procedure removeAllAnimations; cdecl;
    procedure removeAnimationForKey(key: NSString); cdecl;
    procedure removeFromSuperlayer; cdecl;
    procedure renderInContext(ctx: CGContextRef); cdecl;
    procedure scrollPoint(p: CGPoint); cdecl;
    procedure scrollRectToVisible(r: CGRect); cdecl;
    procedure setActions(actions: NSDictionary); cdecl;
    procedure setAffineTransform(m: CGAffineTransform); cdecl;
    procedure setAnchorPoint(anchorPoint: CGPoint); cdecl;
    procedure setAnchorPointZ(anchorPointZ: CGFloat); cdecl;
    procedure setBackgroundColor(backgroundColor: CGColorRef); cdecl;
    procedure setBackgroundFilters(backgroundFilters: NSArray); cdecl;
    procedure setBorderColor(borderColor: CGColorRef); cdecl;
    procedure setBorderWidth(borderWidth: CGFloat); cdecl;
    procedure setBounds(bounds: CGRect); cdecl;
    procedure setCompositingFilter(compositingFilter: Pointer); cdecl;
    procedure setContents(contents: Pointer); cdecl;
    procedure setContentsCenter(contentsCenter: CGRect); cdecl;
    procedure setContentsGravity(contentsGravity: NSString); cdecl;
    procedure setContentsRect(contentsRect: CGRect); cdecl;
    procedure setContentsScale(contentsScale: CGFloat); cdecl;
    procedure setCornerRadius(cornerRadius: CGFloat); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDoubleSided(doubleSided: Boolean); cdecl;
    procedure setEdgeAntialiasingMask(edgeAntialiasingMask: Cardinal); cdecl;
    procedure setFilters(filters: NSArray); cdecl;
    procedure setFrame(frame: CGRect); cdecl;
    procedure setGeometryFlipped(geometryFlipped: Boolean); cdecl;
    procedure setHidden(hidden: Boolean); cdecl;
    procedure setMagnificationFilter(magnificationFilter: NSString); cdecl;
    procedure setMask(mask: CALayer); cdecl;
    procedure setMasksToBounds(masksToBounds: Boolean); cdecl;
    procedure setMinificationFilter(minificationFilter: NSString); cdecl;
    procedure setMinificationFilterBias(minificationFilterBias: Single); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setNeedsDisplay; cdecl;
    procedure setNeedsDisplayInRect(r: CGRect); cdecl;
    procedure setNeedsDisplayOnBoundsChange(needsDisplayOnBoundsChange: Boolean); cdecl;
    procedure setNeedsLayout; cdecl;
    procedure setOpacity(opacity: Single); cdecl;
    procedure setOpaque(opaque: Boolean); cdecl;
    procedure setPosition(position: CGPoint); cdecl;
    procedure setRasterizationScale(rasterizationScale: CGFloat); cdecl;
    procedure setShadowColor(shadowColor: CGColorRef); cdecl;
    procedure setShadowOffset(shadowOffset: CGSize); cdecl;
    procedure setShadowOpacity(shadowOpacity: Single); cdecl;
    procedure setShadowPath(shadowPath: CGPathRef); cdecl;
    procedure setShadowRadius(shadowRadius: CGFloat); cdecl;
    procedure setShouldRasterize(shouldRasterize: Boolean); cdecl;
    procedure setStyle(style: NSDictionary); cdecl;
    procedure setSublayerTransform(sublayerTransform: CATransform3D); cdecl;
    procedure setSublayers(sublayers: NSArray); cdecl;
    procedure setTransform(transform: CATransform3D); cdecl;
    procedure setZPosition(zPosition: CGFloat); cdecl;
    function shadowColor: CGColorRef; cdecl;
    function shadowOffset: CGSize; cdecl;
    function shadowOpacity: Single; cdecl;
    function shadowPath: CGPathRef; cdecl;
    function shadowRadius: CGFloat; cdecl;
    function shouldArchiveValueForKey(key: NSString): Boolean; cdecl;
    function shouldRasterize: Boolean; cdecl;
    function style: NSDictionary; cdecl;
    function sublayerTransform: CATransform3D; cdecl;
    function sublayers: NSArray; cdecl;
    function superlayer: Pointer; cdecl;
    function transform: CATransform3D; cdecl;
    function visibleRect: CGRect; cdecl;
    function zPosition: CGFloat; cdecl;
  end;
  TCALayer = class(TOCGenericImport<CALayerClass, CALayer>)  end;

  CAEAGLLayerClass = interface(CALayerClass)
    ['{336DF8FF-CFB3-4F69-B529-B4D3019C5FC2}']
  end;
  CAEAGLLayer = interface(CALayer)
    ['{15CBE04C-122F-40B2-9961-3B764EAAA690}']
    procedure setDrawableProperties(properties: NSDictionary); cdecl;
    function drawableProperties: NSDictionary; cdecl;
  end;

  CAEmitterCellClass = interface(NSObjectClass)
    ['{956AA94F-4E97-46A6-AC0C-79631D800D24}']
    {class} function defaultValueForKey(key: NSString): Pointer; cdecl;
    {class} function emitterCell: Pointer; cdecl;
  end;
  CAEmitterCell = interface(NSObject)
    ['{D0DC64BD-BF10-4FFA-8BF4-A184E6B38BCD}']
    function alphaRange: Single; cdecl;
    function alphaSpeed: Single; cdecl;
    function birthRate: Single; cdecl;
    function blueRange: Single; cdecl;
    function blueSpeed: Single; cdecl;
    function color: CGColorRef; cdecl;
    function contents: Pointer; cdecl;
    function contentsRect: CGRect; cdecl;
    function emissionLatitude: CGFloat; cdecl;
    function emissionLongitude: CGFloat; cdecl;
    function emissionRange: CGFloat; cdecl;
    function emitterCells: NSArray; cdecl;
    function greenRange: Single; cdecl;
    function greenSpeed: Single; cdecl;
    function isEnabled: Boolean; cdecl;
    function lifetime: Single; cdecl;
    function lifetimeRange: Single; cdecl;
    function magnificationFilter: NSString; cdecl;
    function minificationFilter: NSString; cdecl;
    function minificationFilterBias: Single; cdecl;
    function name: NSString; cdecl;
    function redRange: Single; cdecl;
    function redSpeed: Single; cdecl;
    function scale: CGFloat; cdecl;
    function scaleRange: CGFloat; cdecl;
    function scaleSpeed: CGFloat; cdecl;
    procedure setAlphaRange(alphaRange: Single); cdecl;
    procedure setAlphaSpeed(alphaSpeed: Single); cdecl;
    procedure setBirthRate(birthRate: Single); cdecl;
    procedure setBlueRange(blueRange: Single); cdecl;
    procedure setBlueSpeed(blueSpeed: Single); cdecl;
    procedure setColor(color: CGColorRef); cdecl;
    procedure setContents(contents: Pointer); cdecl;
    procedure setContentsRect(contentsRect: CGRect); cdecl;
    procedure setEmissionLatitude(emissionLatitude: CGFloat); cdecl;
    procedure setEmissionLongitude(emissionLongitude: CGFloat); cdecl;
    procedure setEmissionRange(emissionRange: CGFloat); cdecl;
    procedure setEmitterCells(emitterCells: NSArray); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setGreenRange(greenRange: Single); cdecl;
    procedure setGreenSpeed(greenSpeed: Single); cdecl;
    procedure setLifetime(lifetime: Single); cdecl;
    procedure setLifetimeRange(lifetimeRange: Single); cdecl;
    procedure setMagnificationFilter(magnificationFilter: NSString); cdecl;
    procedure setMinificationFilter(minificationFilter: NSString); cdecl;
    procedure setMinificationFilterBias(minificationFilterBias: Single); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setRedRange(redRange: Single); cdecl;
    procedure setRedSpeed(redSpeed: Single); cdecl;
    procedure setScale(scale: CGFloat); cdecl;
    procedure setScaleRange(scaleRange: CGFloat); cdecl;
    procedure setScaleSpeed(scaleSpeed: CGFloat); cdecl;
    procedure setSpin(spin: CGFloat); cdecl;
    procedure setSpinRange(spinRange: CGFloat); cdecl;
    procedure setStyle(style: NSDictionary); cdecl;
    procedure setVelocity(velocity: CGFloat); cdecl;
    procedure setVelocityRange(velocityRange: CGFloat); cdecl;
    procedure setXAcceleration(xAcceleration: CGFloat); cdecl;
    procedure setYAcceleration(yAcceleration: CGFloat); cdecl;
    procedure setZAcceleration(zAcceleration: CGFloat); cdecl;
    function shouldArchiveValueForKey(key: NSString): Boolean; cdecl;
    function spin: CGFloat; cdecl;
    function spinRange: CGFloat; cdecl;
    function style: NSDictionary; cdecl;
    function velocity: CGFloat; cdecl;
    function velocityRange: CGFloat; cdecl;
    function xAcceleration: CGFloat; cdecl;
    function yAcceleration: CGFloat; cdecl;
    function zAcceleration: CGFloat; cdecl;
  end;
  TCAEmitterCell = class(TOCGenericImport<CAEmitterCellClass, CAEmitterCell>)  end;

  NSValueClass = interface(IObjectiveCClass)
    ['{B3FE5B70-791F-4060-A0C2-6F5697483AAD}']
    {class} function valueWithCATransform3D(t: CATransform3D): Pointer; cdecl;
  end;
  NSValue = interface(IObjectiveCInstance)
    ['{AB368569-2BC0-4074-B3CC-5BAA1064FD05}']
    function CATransform3DValue: CATransform3D; cdecl;
  end;
  TNSValue = class(TOCGenericImport<NSValueClass, NSValue>)  end;

  CAAnimationClass = interface(NSObjectClass)
    ['{A13D480F-54A2-4669-ABD9-6154CC3B83B3}']
    {class} function animation: Pointer; cdecl;
    {class} function defaultValueForKey(key: NSString): Pointer; cdecl;
  end;
  CAAnimation = interface(NSObject)
    ['{8A4FBC87-EF63-4E88-811C-C9CADC8808ED}']
    function delegate: Pointer; cdecl;
    function isRemovedOnCompletion: Boolean; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setRemovedOnCompletion(removedOnCompletion: Boolean); cdecl;
    procedure setTimingFunction(timingFunction: CAMediaTimingFunction); cdecl;
    function shouldArchiveValueForKey(key: NSString): Boolean; cdecl;
    function timingFunction: CAMediaTimingFunction; cdecl;
  end;
  TCAAnimation = class(TOCGenericImport<CAAnimationClass, CAAnimation>)  end;

  NSObjectClass = interface(IObjectiveCClass)
    ['{BD396613-1D5B-4DFB-A474-FA3AB33D1BD8}']
  end;
  NSObject1 = interface(IObjectiveCInstance)
    ['{19EE70F5-969A-49A1-8A86-54B2FC95E3F3}']
    function actionForLayer(layer: CALayer; forKey: NSString): Pointer; cdecl;
    procedure animationDidStart(anim: CAAnimation); cdecl;
    procedure animationDidStop(anim: CAAnimation; finished: Boolean); cdecl;
    function autoreverses: Boolean; cdecl;
    function beginTime: CFTimeInterval; cdecl;
    procedure displayLayer(layer: CALayer); cdecl;
    procedure drawLayer(layer: CALayer; inContext: CGContextRef); cdecl;
    function duration: CFTimeInterval; cdecl;
    function fillMode: NSString; cdecl;
    procedure layoutSublayersOfLayer(layer: CALayer); cdecl;
    function repeatCount: Single; cdecl;
    function repeatDuration: CFTimeInterval; cdecl;
    procedure setAutoreverses(autoreverses: Boolean); cdecl;
    procedure setBeginTime(beginTime: CFTimeInterval); cdecl;
    procedure setDuration(duration: CFTimeInterval); cdecl;
    procedure setFillMode(fillMode: NSString); cdecl;
    procedure setRepeatCount(repeatCount: Single); cdecl;
    procedure setRepeatDuration(repeatDuration: CFTimeInterval); cdecl;
    procedure setSpeed(speed: Single); cdecl;
    procedure setTimeOffset(timeOffset: CFTimeInterval); cdecl;
    function speed: Single; cdecl;
    function timeOffset: CFTimeInterval; cdecl;
  end;
  TNSObject = class(TOCGenericImport<NSObjectClass, NSObject1>)  end;

  CADisplayLinkClass = interface(NSObjectClass)
    ['{138AFE86-6348-44ED-9ED1-31FB10F07EEC}']
    {class} function displayLinkWithTarget(target: Pointer; selector: SEL): Pointer; cdecl;
  end;
  CADisplayLink = interface(NSObject)
    ['{46F85B6A-015A-4D95-8991-DE2997EA2780}']
    procedure addToRunLoop(runloop: NSRunLoop; forMode: NSString); cdecl;
    function duration: CFTimeInterval; cdecl;
    function frameInterval: NSInteger; cdecl;
    procedure invalidate; cdecl;
    function isPaused: Boolean; cdecl;
    procedure removeFromRunLoop(runloop: NSRunLoop; forMode: NSString); cdecl;
    procedure setFrameInterval(frameInterval: NSInteger); cdecl;
    procedure setPaused(paused: Boolean); cdecl;
    function timestamp: CFTimeInterval; cdecl;
  end;
  TCADisplayLink = class(TOCGenericImport<CADisplayLinkClass, CADisplayLink>)  end;

  CATextLayerClass = interface(CALayerClass)
    ['{687313A9-4AFC-4450-9F58-300D4B8A1623}']
  end;
  CATextLayer = interface(CALayer)
    ['{25F35865-FFAE-4279-8569-58C31EA137B5}']
    function alignmentMode: NSString; cdecl;
    function font: CFTypeRef; cdecl;
    function fontSize: CGFloat; cdecl;
    function foregroundColor: CGColorRef; cdecl;
    function isWrapped: Boolean; cdecl;
    procedure setAlignmentMode(alignmentMode: NSString); cdecl;
    procedure setFont(font: CFTypeRef); cdecl;
    procedure setFontSize(fontSize: CGFloat); cdecl;
    procedure setForegroundColor(foregroundColor: CGColorRef); cdecl;
    procedure setString(string_: Pointer); cdecl;
    procedure setTruncationMode(truncationMode: NSString); cdecl;
    procedure setWrapped(wrapped: Boolean); cdecl;
    function truncationMode: NSString; cdecl;
  end;
  TCATextLayer = class(TOCGenericImport<CATextLayerClass, CATextLayer>)  end;

  CAShapeLayerClass = interface(CALayerClass)
    ['{AA8C22CC-95FD-4C06-88E8-97348AEAEAC5}']
  end;
  CAShapeLayer = interface(CALayer)
    ['{B6F59F15-339C-4FAE-AC85-7BC2E423A586}']
    function fillColor: CGColorRef; cdecl;
    function fillRule: NSString; cdecl;
    function lineCap: NSString; cdecl;
    function lineDashPattern: NSArray; cdecl;
    function lineDashPhase: CGFloat; cdecl;
    function lineJoin: NSString; cdecl;
    function lineWidth: CGFloat; cdecl;
    function miterLimit: CGFloat; cdecl;
    function path: CGPathRef; cdecl;
    procedure setFillColor(fillColor: CGColorRef); cdecl;
    procedure setFillRule(fillRule: NSString); cdecl;
    procedure setLineCap(lineCap: NSString); cdecl;
    procedure setLineDashPattern(lineDashPattern: NSArray); cdecl;
    procedure setLineDashPhase(lineDashPhase: CGFloat); cdecl;
    procedure setLineJoin(lineJoin: NSString); cdecl;
    procedure setLineWidth(lineWidth: CGFloat); cdecl;
    procedure setMiterLimit(miterLimit: CGFloat); cdecl;
    procedure setPath(path: CGPathRef); cdecl;
    procedure setStrokeColor(strokeColor: CGColorRef); cdecl;
    procedure setStrokeEnd(strokeEnd: CGFloat); cdecl;
    procedure setStrokeStart(strokeStart: CGFloat); cdecl;
    function strokeColor: CGColorRef; cdecl;
    function strokeEnd: CGFloat; cdecl;
    function strokeStart: CGFloat; cdecl;
  end;
  TCAShapeLayer = class(TOCGenericImport<CAShapeLayerClass, CAShapeLayer>)  end;

  CATransitionClass = interface(CAAnimationClass)
    ['{40A33A78-F024-476B-AAA0-219DF6F00B56}']
  end;
  CATransition = interface(CAAnimation)
    ['{34326C31-0298-47CD-ADE9-0ECEE9660F9F}']
    function endProgress: Single; cdecl;
    function filter: Pointer; cdecl;
    procedure setEndProgress(endProgress: Single); cdecl;
    procedure setFilter(filter: Pointer); cdecl;
    procedure setStartProgress(startProgress: Single); cdecl;
    procedure setSubtype(subtype: NSString); cdecl;
    procedure setType(type_: NSString); cdecl;
    function startProgress: Single; cdecl;
    function subtype: NSString; cdecl;
  end;
  TCATransition = class(TOCGenericImport<CATransitionClass, CATransition>)  end;

  CATiledLayerClass = interface(CALayerClass)
    ['{910978CC-7D8B-456C-A254-D64E20E44A88}']
    {class} function fadeDuration: CFTimeInterval; cdecl;
  end;
  CATiledLayer = interface(CALayer)
    ['{E5AB0E1E-345B-4687-8B19-EE4EF0875C00}']
    function levelsOfDetail: size_t; cdecl;
    function levelsOfDetailBias: size_t; cdecl;
    procedure setLevelsOfDetail(levelsOfDetail: size_t); cdecl;
    procedure setLevelsOfDetailBias(levelsOfDetailBias: size_t); cdecl;
    procedure setTileSize(tileSize: CGSize); cdecl;
    function tileSize: CGSize; cdecl;
  end;
  TCATiledLayer = class(TOCGenericImport<CATiledLayerClass, CATiledLayer>)  end;

  CAAnimationGroupClass = interface(CAAnimationClass)
    ['{78096F7D-445C-4CC4-A95A-2241DFD9F3FC}']
  end;
  CAAnimationGroup = interface(CAAnimation)
    ['{470DD04E-AC6A-48A2-ABAC-153B6E9FD495}']
    function animations: NSArray; cdecl;
    procedure setAnimations(animations: NSArray); cdecl;
  end;
  TCAAnimationGroup = class(TOCGenericImport<CAAnimationGroupClass, CAAnimationGroup>)  end;

  CAGradientLayerClass = interface(CALayerClass)
    ['{A6C3B4B9-1CFD-41BB-8989-F953351C3FFE}']
  end;
  CAGradientLayer = interface(CALayer)
    ['{6C1A836A-054F-4367-8B9D-ABFB8214ACF0}']
    function colors: NSArray; cdecl;
    function endPoint: CGPoint; cdecl;
    function locations: NSArray; cdecl;
    procedure setColors(colors: NSArray); cdecl;
    procedure setEndPoint(endPoint: CGPoint); cdecl;
    procedure setLocations(locations: NSArray); cdecl;
    procedure setStartPoint(startPoint: CGPoint); cdecl;
    procedure setType(type_: NSString); cdecl;
    function startPoint: CGPoint; cdecl;
  end;
  TCAGradientLayer = class(TOCGenericImport<CAGradientLayerClass, CAGradientLayer>)  end;

  CAEmitterLayerClass = interface(CALayerClass)
    ['{C945931C-95FE-4008-AA20-6E3D4B04AA27}']
  end;
  CAEmitterLayer = interface(CALayer)
    ['{04C6F84D-FDB5-4230-A58D-8BCD310977ED}']
    function birthRate: Single; cdecl;
    function emitterCells: NSArray; cdecl;
    function emitterDepth: CGFloat; cdecl;
    function emitterMode: NSString; cdecl;
    function emitterPosition: CGPoint; cdecl;
    function emitterShape: NSString; cdecl;
    function emitterSize: CGSize; cdecl;
    function emitterZPosition: CGFloat; cdecl;
    function lifetime: Single; cdecl;
    function preservesDepth: Boolean; cdecl;
    function renderMode: NSString; cdecl;
    function scale: Single; cdecl;
    function seed: Cardinal; cdecl;
    procedure setBirthRate(birthRate: Single); cdecl;
    procedure setEmitterCells(emitterCells: NSArray); cdecl;
    procedure setEmitterDepth(emitterDepth: CGFloat); cdecl;
    procedure setEmitterMode(emitterMode: NSString); cdecl;
    procedure setEmitterPosition(emitterPosition: CGPoint); cdecl;
    procedure setEmitterShape(emitterShape: NSString); cdecl;
    procedure setEmitterSize(emitterSize: CGSize); cdecl;
    procedure setEmitterZPosition(emitterZPosition: CGFloat); cdecl;
    procedure setLifetime(lifetime: Single); cdecl;
    procedure setPreservesDepth(preservesDepth: Boolean); cdecl;
    procedure setRenderMode(renderMode: NSString); cdecl;
    procedure setScale(scale: Single); cdecl;
    procedure setSeed(seed: Cardinal); cdecl;
    procedure setSpin(spin: Single); cdecl;
    procedure setVelocity(velocity: Single); cdecl;
    function spin: Single; cdecl;
    function velocity: Single; cdecl;
  end;
  TCAEmitterLayer = class(TOCGenericImport<CAEmitterLayerClass, CAEmitterLayer>)  end;

  CAScrollLayerClass = interface(CALayerClass)
    ['{53FEC26B-BB49-4329-A18A-F9D21790820C}']
  end;
  CAScrollLayer = interface(CALayer)
    ['{30D38BE9-8B86-465B-9B3D-AC10FA83EF28}']
    function scrollMode: NSString; cdecl;
    procedure scrollToPoint(p: CGPoint); cdecl;
    procedure scrollToRect(r: CGRect); cdecl;
    procedure setScrollMode(scrollMode: NSString); cdecl;
  end;
  TCAScrollLayer = class(TOCGenericImport<CAScrollLayerClass, CAScrollLayer>)  end;

  CAReplicatorLayerClass = interface(CALayerClass)
    ['{6AF9CCC9-2FC7-47C8-A137-48D219B6EE56}']
  end;
  CAReplicatorLayer = interface(CALayer)
    ['{7056CD88-E37B-42F6-A789-3EF980195483}']
    function instanceAlphaOffset: Single; cdecl;
    function instanceBlueOffset: Single; cdecl;
    function instanceColor: CGColorRef; cdecl;
    function instanceCount: NSInteger; cdecl;
    function instanceDelay: CFTimeInterval; cdecl;
    function instanceGreenOffset: Single; cdecl;
    function instanceRedOffset: Single; cdecl;
    function instanceTransform: CATransform3D; cdecl;
    function preservesDepth: Boolean; cdecl;
    procedure setInstanceAlphaOffset(instanceAlphaOffset: Single); cdecl;
    procedure setInstanceBlueOffset(instanceBlueOffset: Single); cdecl;
    procedure setInstanceColor(instanceColor: CGColorRef); cdecl;
    procedure setInstanceCount(instanceCount: NSInteger); cdecl;
    procedure setInstanceDelay(instanceDelay: CFTimeInterval); cdecl;
    procedure setInstanceGreenOffset(instanceGreenOffset: Single); cdecl;
    procedure setInstanceRedOffset(instanceRedOffset: Single); cdecl;
    procedure setInstanceTransform(instanceTransform: CATransform3D); cdecl;
    procedure setPreservesDepth(preservesDepth: Boolean); cdecl;
  end;
  TCAReplicatorLayer = class(TOCGenericImport<CAReplicatorLayerClass, CAReplicatorLayer>)  end;

  CAPropertyAnimationClass = interface(CAAnimationClass)
    ['{F35A28FD-2177-4445-A0BB-9464EA18E825}']
    {class} function animationWithKeyPath(path: NSString): Pointer; cdecl;
  end;
  CAPropertyAnimation = interface(CAAnimation)
    ['{92ADB3CB-1066-40B5-8D65-75F9321599DB}']
    function isAdditive: Boolean; cdecl;
    function isCumulative: Boolean; cdecl;
    function keyPath: NSString; cdecl;
    procedure setAdditive(additive: Boolean); cdecl;
    procedure setCumulative(cumulative: Boolean); cdecl;
    procedure setKeyPath(keyPath: NSString); cdecl;
    procedure setValueFunction(valueFunction: CAValueFunction); cdecl;
    function valueFunction: CAValueFunction; cdecl;
  end;
  TCAPropertyAnimation = class(TOCGenericImport<CAPropertyAnimationClass, CAPropertyAnimation>)  end;

  CABasicAnimationClass = interface(CAPropertyAnimationClass)
    ['{14F31E2F-3337-4C7C-9095-CC7A6A2EAB97}']
  end;
  CABasicAnimation = interface(CAPropertyAnimation)
    ['{D9ECBF10-0F2C-4A16-B0D4-8A3AAF407C48}']
    function byValue: Pointer; cdecl;
    function fromValue: Pointer; cdecl;
    procedure setByValue(byValue: Pointer); cdecl;
    procedure setFromValue(fromValue: Pointer); cdecl;
    procedure setToValue(toValue: Pointer); cdecl;
    function toValue: Pointer; cdecl;
  end;
  TCABasicAnimation = class(TOCGenericImport<CABasicAnimationClass, CABasicAnimation>)  end;

  CAKeyframeAnimationClass = interface(CAPropertyAnimationClass)
    ['{ECA2D5FD-43C5-4F9F-99AA-007DC451BF36}']
  end;
  CAKeyframeAnimation = interface(CAPropertyAnimation)
    ['{43BC07D7-DAAC-49D0-9D7A-9BD9E4410CEA}']
    function biasValues: NSArray; cdecl;
    function calculationMode: NSString; cdecl;
    function continuityValues: NSArray; cdecl;
    function keyTimes: NSArray; cdecl;
    function path: CGPathRef; cdecl;
    function rotationMode: NSString; cdecl;
    procedure setBiasValues(biasValues: NSArray); cdecl;
    procedure setCalculationMode(calculationMode: NSString); cdecl;
    procedure setContinuityValues(continuityValues: NSArray); cdecl;
    procedure setKeyTimes(keyTimes: NSArray); cdecl;
    procedure setPath(path: CGPathRef); cdecl;
    procedure setRotationMode(rotationMode: NSString); cdecl;
    procedure setTensionValues(tensionValues: NSArray); cdecl;
    procedure setTimingFunctions(timingFunctions: NSArray); cdecl;
    procedure setValues(values: NSArray); cdecl;
    function tensionValues: NSArray; cdecl;
    function timingFunctions: NSArray; cdecl;
    function values: NSArray; cdecl;
  end;
  TCAKeyframeAnimation = class(TOCGenericImport<CAKeyframeAnimationClass, CAKeyframeAnimation>)  end;



implementation

{$IF defined(IOS) and NOT defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  QCModule: THandle;

initialization
  QCModule := dlopen(MarshaledAString(libQuartzCore), RTLD_LAZY);

finalization
  dlclose(QCModule);
{$ENDIF IOS}

end.
