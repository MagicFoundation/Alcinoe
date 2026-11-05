//
// Made from MapLibre Native 6.31.0 (MLN API)
//
unit Alcinoe.iOSApi.MapLibre;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported131}
  {$MESSAGE WARN 'Check whether MapLibre can be compiled as a static library alongside Skia. Verify that the compilation produces no warnings or linker issues. If everything works correctly, switch from dynamic libraries to static libraries.'}
{$ENDIF}

uses
  iOSapi.UIKit,
  iOSapi.CoreGraphics,
  iOSapi.CoreLocation,
  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  Macapi.ObjectiveC;

{$M+}

type

  {*********************************}
  MLNCameraChangeReason = NSUInteger;
  MLNOrnamentVisibility = NSInteger;
  MLNOrnamentPosition = NSUInteger;

const

  {****************************}
  MLNCameraChangeReasonNone = 0;
  MLNCameraChangeReasonProgrammatic = 1 shl 0;
  MLNCameraChangeReasonResetNorth = 1 shl 1;
  MLNCameraChangeReasonGesturePan = 1 shl 2;
  MLNCameraChangeReasonGesturePinch = 1 shl 3;
  MLNCameraChangeReasonGestureRotate = 1 shl 4;
  MLNCameraChangeReasonGestureZoomIn = 1 shl 5;
  MLNCameraChangeReasonGestureZoomOut = 1 shl 6;
  MLNCameraChangeReasonGestureOneFingerZoom = 1 shl 7;
  MLNCameraChangeReasonGestureTilt = 1 shl 8;
  MLNCameraChangeReasonTransitionCancelled = 1 shl 16;
  MLNOrnamentVisibilityAdaptive = 0; // Adapts to the current map state.
  MLNOrnamentVisibilityHidden   = 1; // Always hidden.
  MLNOrnamentVisibilityVisible  = 2; // Always visible.
  MLNOrnamentPositionTopLeft     = 0; // Top left of the map view.
  MLNOrnamentPositionTopRight    = 1; // Top right of the map view.
  MLNOrnamentPositionBottomLeft  = 2; // Bottom left of the map view.
  MLNOrnamentPositionBottomRight = 3; // Bottom right of the map view.

type

  {********************}
  MLNTransition = record
    duration: NSTimeInterval;
    delay: NSTimeInterval;
  end;
  PMLNTransition = ^MLNTransition;

  {**************************}
  MLNCoordinateBounds = record
    sw: CLLocationCoordinate2D;
    ne: CLLocationCoordinate2D;
  end;
  PMLNCoordinateBounds = ^MLNCoordinateBounds;

  {***************************}
  MLNCompassButton = interface;
  MLNFeature = interface;
  MLNStyle = interface;
  MLNStyleLayer = interface;
  MLNForegroundStyleLayer = interface;
  MLNVectorStyleLayer = interface;
  MLNSymbolStyleLayer = interface;
  MLNLineStyleLayer = interface;
  MLNCircleStyleLayer = interface;
  MLNFillStyleLayer = interface;
  MLNFillExtrusionStyleLayer = interface;
  MLNHeatmapStyleLayer = interface;
  MLNRasterStyleLayer = interface;
  MLNShape = interface;
  MLNPointAnnotation = interface;
  MLNPointFeature = interface;
  MLNSource = interface;
  MLNShapeSource = interface;
  MLNTileSource = interface;
  MLNRasterTileSource = interface;
  MLNMapCamera = interface;
  MLNMapView = interface;
  MLNMapViewDelegate = interface;

  {*************************************************}
  MLNCompassButtonClass = interface(UIImageViewClass)
    ['{9D5E2187-EDEB-474F-95B9-FE9722DAF552}']
  end;
  MLNCompassButton = interface(UIImageView)
    ['{13236750-ACF2-4C1C-B514-3CD674A094A3}']
    function compassVisibility: MLNOrnamentVisibility; cdecl;
    procedure setCompassVisibility(compassVisibility: MLNOrnamentVisibility); cdecl;
  end;
  TMLNCompassButton = class(TOCGenericImport<MLNCompassButtonClass, MLNCompassButton>)  end;

  {*********************************}
  MLNFeature = interface(IObjectiveC)
    ['{9258AD10-E8EB-44DA-A400-E7376CFB052D}']
    //procedure setIdentifier(identifier: id); cdecl;
    //function identifier: id; cdecl;
    //procedure setAttributes(attributes: NSDictionary); cdecl;
    //function attributes: NSDictionary; cdecl;
    function attributeForKey(key: NSString): Pointer; cdecl;
    //function geoJSONDictionary: NSDictionary; cdecl;
  end;

  {**************************************}
  MLNStyleClass = interface(NSObjectClass)
    ['{DAAF4A95-D048-487A-99A1-03BC883A40A2}']
    //{class} function predefinedStyles: NSArray; cdecl;
    //{class} function defaultStyle: MLNDefaultStyle; cdecl;
    //{class} function defaultStyleURL: NSURL; cdecl;
    //{class} function predefinedStyle(withStyleName: NSString): MLNDefaultStyle; cdecl;
  end;
  MLNStyle = interface(NSObject)
    ['{9326B09F-600B-4E02-9D08-DB5FEE9580A9}']
    //function name: NSString; cdecl;
    //procedure setStyleJSON(styleJSON: NSString); cdecl;
    //function styleJSON: NSString; cdecl;
    //procedure setSources(sources: NSSet); cdecl;
    //function sources: NSSet; cdecl;
    //procedure setTransition(transition: MLNTransition); cdecl;
    //function transition: MLNTransition; cdecl;
    //procedure setPerformsPlacementTransitions(performsPlacementTransitions: Boolean); cdecl;
    //function performsPlacementTransitions: Boolean; cdecl;
    function sourceWithIdentifier(identifier: NSString): MLNSource; cdecl;
    procedure addSource(source: MLNSource); cdecl;
    procedure removeSource(source: MLNSource); cdecl;
    //function removeSource(source: MLNSource; error: NSError): Boolean; cdecl;
    //procedure setLayers(layers: NSArray); cdecl;
    //function layers: NSArray; cdecl;
    function layerWithIdentifier(identifier: NSString): MLNStyleLayer; cdecl;
    procedure addLayer(layer: MLNStyleLayer); cdecl;
    //procedure insertLayer(layer: MLNStyleLayer; atIndex: NSUInteger); cdecl; overload;
    //procedure insertLayer(layer: MLNStyleLayer; belowLayer: MLNStyleLayer); cdecl; overload;
    //procedure insertLayer(layer: MLNStyleLayer; aboveLayer: MLNStyleLayer); cdecl; overload;
    procedure removeLayer(layer: MLNStyleLayer); cdecl;
    //function imageForName(name: NSString): UIImage; cdecl;
    procedure setImage(image: UIImage; forName: NSString); cdecl;
    procedure removeImageForName(name: NSString); cdecl;
    //procedure setLight(light: MLNLight); cdecl;
    //function light: MLNLight; cdecl;
    //procedure localizeLabelsIntoLocale(locale: NSLocale); cdecl;
  end;
  TMLNStyle = class(TOCGenericImport<MLNStyleClass, MLNStyle>)  end;

  {*******************************************}
  MLNStyleLayerClass = interface(NSObjectClass)
    ['{C2A8C7ED-C078-4CED-AE31-B7DEA616DF91}']
  end;
  MLNStyleLayer = interface(NSObject)
    ['{3EE14BC2-CA78-4FEE-95F3-2FE2BCC12BBC}']
    //function identifier: NSString; cdecl;
    procedure setVisible(visible: Boolean); cdecl;
    function isVisible: Boolean; cdecl;
    //procedure setMaximumZoomLevel(maximumZoomLevel: float); cdecl;
    //function maximumZoomLevel: float; cdecl;
    //procedure setMinimumZoomLevel(minimumZoomLevel: float); cdecl;
    //function minimumZoomLevel: float; cdecl;
  end;
  TMLNStyleLayer = class(TOCGenericImport<MLNStyleLayerClass, MLNStyleLayer>)  end;

  {**********************************************************}
  MLNForegroundStyleLayerClass = interface(MLNStyleLayerClass)
    ['{57E29678-4803-428A-8129-FF5011A94E70}']
  end;
  MLNForegroundStyleLayer = interface(MLNStyleLayer)
    ['{E4841D44-4C72-4DEF-BDB3-E6DFC109FEDA}']
    //function sourceIdentifier: NSString; cdecl;
  end;
  TMLNForegroundStyleLayer = class(TOCGenericImport<MLNForegroundStyleLayerClass, MLNForegroundStyleLayer>)  end;

  {****************************************************************}
  MLNVectorStyleLayerClass = interface(MLNForegroundStyleLayerClass)
    ['{7477CB36-DCBB-4565-89B1-39DCCC493CFD}']
  end;
  MLNVectorStyleLayer = interface(MLNForegroundStyleLayer)
    ['{2BD4294C-C7E3-4D9A-8379-837170AF62F9}']
    //procedure setSourceLayerIdentifier(sourceLayerIdentifier: NSString); cdecl;
    //function sourceLayerIdentifier: NSString; cdecl;
    //procedure setPredicate(predicate: NSPredicate); cdecl;
    //function predicate: NSPredicate; cdecl;
  end;
  TMLNVectorStyleLayer = class(TOCGenericImport<MLNVectorStyleLayerClass, MLNVectorStyleLayer>)  end;

  {************************************************************}
  MLNSymbolStyleLayerClass = interface(MLNVectorStyleLayerClass)
    ['{776CA1E7-F5CB-4439-B026-0D0F13F15B9C}']
  end;
  MLNSymbolStyleLayer = interface(MLNVectorStyleLayer)
    ['{772148A0-5BA6-482B-9473-979079682494}']
    function initWithIdentifier(identifier: NSString; source: MLNSource): MLNSymbolStyleLayer; cdecl;
    procedure setIconAllowsOverlap(iconAllowsOverlap: NSExpression); cdecl;
    function iconAllowsOverlap: NSExpression; cdecl;
    procedure setIconAnchor(iconAnchor: NSExpression); cdecl;
    function iconAnchor: NSExpression; cdecl;
    procedure setIconIgnoresPlacement(iconIgnoresPlacement: NSExpression); cdecl;
    function iconIgnoresPlacement: NSExpression; cdecl;
    procedure setIconImageName(iconImageName: NSExpression); cdecl;
    function iconImageName: NSExpression; cdecl;
    procedure setIconOffset(iconOffset: NSExpression); cdecl;
    function iconOffset: NSExpression; cdecl;
    procedure setIconOptional(iconOptional: NSExpression); cdecl;
    function isIconOptional: NSExpression; cdecl;
    procedure setIconPadding(iconPadding: NSExpression); cdecl;
    function iconPadding: NSExpression; cdecl;
    procedure setIconPitchAlignment(iconPitchAlignment: NSExpression); cdecl;
    function iconPitchAlignment: NSExpression; cdecl;
    procedure setIconRotation(iconRotation: NSExpression); cdecl;
    function iconRotation: NSExpression; cdecl;
    procedure setIconRotationAlignment(iconRotationAlignment: NSExpression); cdecl;
    function iconRotationAlignment: NSExpression; cdecl;
    procedure setIconScale(iconScale: NSExpression); cdecl;
    function iconScale: NSExpression; cdecl;
    procedure setIconTextFit(iconTextFit: NSExpression); cdecl;
    function iconTextFit: NSExpression; cdecl;
    procedure setIconTextFitPadding(iconTextFitPadding: NSExpression); cdecl;
    function iconTextFitPadding: NSExpression; cdecl;
    procedure setKeepsIconUpright(keepsIconUpright: NSExpression); cdecl;
    function keepsIconUpright: NSExpression; cdecl;
    procedure setKeepsTextUpright(keepsTextUpright: NSExpression); cdecl;
    function keepsTextUpright: NSExpression; cdecl;
    procedure setMaximumTextAngle(maximumTextAngle: NSExpression); cdecl;
    function maximumTextAngle: NSExpression; cdecl;
    procedure setMaximumTextWidth(maximumTextWidth: NSExpression); cdecl;
    function maximumTextWidth: NSExpression; cdecl;
    procedure setSymbolAvoidsEdges(symbolAvoidsEdges: NSExpression); cdecl;
    function symbolAvoidsEdges: NSExpression; cdecl;
    procedure setSymbolPlacement(symbolPlacement: NSExpression); cdecl;
    function symbolPlacement: NSExpression; cdecl;
    procedure setSymbolScreenSpace(symbolScreenSpace: NSExpression); cdecl;
    function symbolScreenSpace: NSExpression; cdecl;
    procedure setSymbolSortKey(symbolSortKey: NSExpression); cdecl;
    function symbolSortKey: NSExpression; cdecl;
    procedure setSymbolSpacing(symbolSpacing: NSExpression); cdecl;
    function symbolSpacing: NSExpression; cdecl;
    procedure setSymbolZOrder(symbolZOrder: NSExpression); cdecl;
    function symbolZOrder: NSExpression; cdecl;
    procedure setText(text: NSExpression); cdecl;
    function text: NSExpression; cdecl;
    procedure setTextAllowsOverlap(textAllowsOverlap: NSExpression); cdecl;
    function textAllowsOverlap: NSExpression; cdecl;
    procedure setTextAnchor(textAnchor: NSExpression); cdecl;
    function textAnchor: NSExpression; cdecl;
    procedure setTextFontNames(textFontNames: NSExpression); cdecl;
    function textFontNames: NSExpression; cdecl;
    procedure setTextFontSize(textFontSize: NSExpression); cdecl;
    function textFontSize: NSExpression; cdecl;
    procedure setTextIgnoresPlacement(textIgnoresPlacement: NSExpression); cdecl;
    function textIgnoresPlacement: NSExpression; cdecl;
    procedure setTextJustification(textJustification: NSExpression); cdecl;
    function textJustification: NSExpression; cdecl;
    procedure setTextLetterSpacing(textLetterSpacing: NSExpression); cdecl;
    function textLetterSpacing: NSExpression; cdecl;
    procedure setTextLineHeight(textLineHeight: NSExpression); cdecl;
    function textLineHeight: NSExpression; cdecl;
    procedure setTextOffset(textOffset: NSExpression); cdecl;
    function textOffset: NSExpression; cdecl;
    procedure setTextOptional(textOptional: NSExpression); cdecl;
    function isTextOptional: NSExpression; cdecl;
    procedure setTextPadding(textPadding: NSExpression); cdecl;
    function textPadding: NSExpression; cdecl;
    procedure setTextPitchAlignment(textPitchAlignment: NSExpression); cdecl;
    function textPitchAlignment: NSExpression; cdecl;
    procedure setTextRadialOffset(textRadialOffset: NSExpression); cdecl;
    function textRadialOffset: NSExpression; cdecl;
    procedure setTextRotation(textRotation: NSExpression); cdecl;
    function textRotation: NSExpression; cdecl;
    procedure setTextRotationAlignment(textRotationAlignment: NSExpression); cdecl;
    function textRotationAlignment: NSExpression; cdecl;
    procedure setTextTransform(textTransform: NSExpression); cdecl;
    function textTransform: NSExpression; cdecl;
    procedure setTextVariableAnchor(textVariableAnchor: NSExpression); cdecl;
    function textVariableAnchor: NSExpression; cdecl;
    procedure setTextVariableAnchorOffset(textVariableAnchorOffset: NSExpression); cdecl;
    function textVariableAnchorOffset: NSExpression; cdecl;
    procedure setTextWritingModes(textWritingModes: NSExpression); cdecl;
    function textWritingModes: NSExpression; cdecl;
    procedure setIconColor(iconColor: NSExpression); cdecl;
    function iconColor: NSExpression; cdecl;
    procedure setIconColorTransition(iconColorTransition: MLNTransition); cdecl;
    function iconColorTransition: MLNTransition; cdecl;
    procedure setIconHaloBlur(iconHaloBlur: NSExpression); cdecl;
    function iconHaloBlur: NSExpression; cdecl;
    procedure setIconHaloBlurTransition(iconHaloBlurTransition: MLNTransition); cdecl;
    function iconHaloBlurTransition: MLNTransition; cdecl;
    procedure setIconHaloColor(iconHaloColor: NSExpression); cdecl;
    function iconHaloColor: NSExpression; cdecl;
    procedure setIconHaloColorTransition(iconHaloColorTransition: MLNTransition); cdecl;
    function iconHaloColorTransition: MLNTransition; cdecl;
    procedure setIconHaloWidth(iconHaloWidth: NSExpression); cdecl;
    function iconHaloWidth: NSExpression; cdecl;
    procedure setIconHaloWidthTransition(iconHaloWidthTransition: MLNTransition); cdecl;
    function iconHaloWidthTransition: MLNTransition; cdecl;
    procedure setIconOpacity(iconOpacity: NSExpression); cdecl;
    function iconOpacity: NSExpression; cdecl;
    procedure setIconOpacityTransition(iconOpacityTransition: MLNTransition); cdecl;
    function iconOpacityTransition: MLNTransition; cdecl;
    procedure setIconTranslation(iconTranslation: NSExpression); cdecl;
    function iconTranslation: NSExpression; cdecl;
    procedure setIconTranslationTransition(iconTranslationTransition: MLNTransition); cdecl;
    function iconTranslationTransition: MLNTransition; cdecl;
    procedure setIconTranslationAnchor(iconTranslationAnchor: NSExpression); cdecl;
    function iconTranslationAnchor: NSExpression; cdecl;
    procedure setTextColor(textColor: NSExpression); cdecl;
    function textColor: NSExpression; cdecl;
    procedure setTextColorTransition(textColorTransition: MLNTransition); cdecl;
    function textColorTransition: MLNTransition; cdecl;
    procedure setTextHaloBlur(textHaloBlur: NSExpression); cdecl;
    function textHaloBlur: NSExpression; cdecl;
    procedure setTextHaloBlurTransition(textHaloBlurTransition: MLNTransition); cdecl;
    function textHaloBlurTransition: MLNTransition; cdecl;
    procedure setTextHaloColor(textHaloColor: NSExpression); cdecl;
    function textHaloColor: NSExpression; cdecl;
    procedure setTextHaloColorTransition(textHaloColorTransition: MLNTransition); cdecl;
    function textHaloColorTransition: MLNTransition; cdecl;
    procedure setTextHaloWidth(textHaloWidth: NSExpression); cdecl;
    function textHaloWidth: NSExpression; cdecl;
    procedure setTextHaloWidthTransition(textHaloWidthTransition: MLNTransition); cdecl;
    function textHaloWidthTransition: MLNTransition; cdecl;
    procedure setTextOpacity(textOpacity: NSExpression); cdecl;
    function textOpacity: NSExpression; cdecl;
    procedure setTextOpacityTransition(textOpacityTransition: MLNTransition); cdecl;
    function textOpacityTransition: MLNTransition; cdecl;
    procedure setTextTranslation(textTranslation: NSExpression); cdecl;
    function textTranslation: NSExpression; cdecl;
    procedure setTextTranslationTransition(textTranslationTransition: MLNTransition); cdecl;
    function textTranslationTransition: MLNTransition; cdecl;
    procedure setTextTranslationAnchor(textTranslationAnchor: NSExpression); cdecl;
    function textTranslationAnchor: NSExpression; cdecl;
  end;
  TMLNSymbolStyleLayer = class(TOCGenericImport<MLNSymbolStyleLayerClass, MLNSymbolStyleLayer>) end;

  {**********************************************************}
  MLNLineStyleLayerClass = interface(MLNVectorStyleLayerClass)
    ['{68ABF977-4CF2-4FD4-BDC3-2142B6A4704C}']
  end;
  MLNLineStyleLayer = interface(MLNVectorStyleLayer)
    ['{43B186AF-5250-49ED-B1AC-7D3A705D537B}']
    function initWithIdentifier(identifier: NSString; source: MLNSource): MLNLineStyleLayer; cdecl;
    procedure setLineCap(lineCap: NSExpression); cdecl;
    function lineCap: NSExpression; cdecl;
    procedure setLineJoin(lineJoin: NSExpression); cdecl;
    function lineJoin: NSExpression; cdecl;
    procedure setLineMiterLimit(lineMiterLimit: NSExpression); cdecl;
    function lineMiterLimit: NSExpression; cdecl;
    procedure setLineRoundLimit(lineRoundLimit: NSExpression); cdecl;
    function lineRoundLimit: NSExpression; cdecl;
    procedure setLineSortKey(lineSortKey: NSExpression); cdecl;
    function lineSortKey: NSExpression; cdecl;
    procedure setLineBlur(lineBlur: NSExpression); cdecl;
    function lineBlur: NSExpression; cdecl;
    procedure setLineBlurTransition(lineBlurTransition: MLNTransition); cdecl;
    function lineBlurTransition: MLNTransition; cdecl;
    procedure setLineColor(lineColor: NSExpression); cdecl;
    function lineColor: NSExpression; cdecl;
    procedure setLineColorTransition(lineColorTransition: MLNTransition); cdecl;
    function lineColorTransition: MLNTransition; cdecl;
    procedure setLineDashPattern(lineDashPattern: NSExpression); cdecl;
    function lineDashPattern: NSExpression; cdecl;
    procedure setLineDashPatternTransition(lineDashPatternTransition: MLNTransition); cdecl;
    function lineDashPatternTransition: MLNTransition; cdecl;
    procedure setLineGapWidth(lineGapWidth: NSExpression); cdecl;
    function lineGapWidth: NSExpression; cdecl;
    procedure setLineGapWidthTransition(lineGapWidthTransition: MLNTransition); cdecl;
    function lineGapWidthTransition: MLNTransition; cdecl;
    procedure setLineGradient(lineGradient: NSExpression); cdecl;
    function lineGradient: NSExpression; cdecl;
    procedure setLineOffset(lineOffset: NSExpression); cdecl;
    function lineOffset: NSExpression; cdecl;
    procedure setLineOffsetTransition(lineOffsetTransition: MLNTransition); cdecl;
    function lineOffsetTransition: MLNTransition; cdecl;
    procedure setLineOpacity(lineOpacity: NSExpression); cdecl;
    function lineOpacity: NSExpression; cdecl;
    procedure setLineOpacityTransition(lineOpacityTransition: MLNTransition); cdecl;
    function lineOpacityTransition: MLNTransition; cdecl;
    procedure setLinePattern(linePattern: NSExpression); cdecl;
    function linePattern: NSExpression; cdecl;
    procedure setLinePatternTransition(linePatternTransition: MLNTransition); cdecl;
    function linePatternTransition: MLNTransition; cdecl;
    procedure setLineTranslation(lineTranslation: NSExpression); cdecl;
    function lineTranslation: NSExpression; cdecl;
    procedure setLineTranslationTransition(lineTranslationTransition: MLNTransition); cdecl;
    function lineTranslationTransition: MLNTransition; cdecl;
    procedure setLineTranslationAnchor(lineTranslationAnchor: NSExpression); cdecl;
    function lineTranslationAnchor: NSExpression; cdecl;
    procedure setLineWidth(lineWidth: NSExpression); cdecl;
    function lineWidth: NSExpression; cdecl;
    procedure setLineWidthTransition(lineWidthTransition: MLNTransition); cdecl;
    function lineWidthTransition: MLNTransition; cdecl;
  end;
  TMLNLineStyleLayer = class(TOCGenericImport<MLNLineStyleLayerClass, MLNLineStyleLayer>)  end;

  {************************************************************}
  MLNCircleStyleLayerClass = interface(MLNVectorStyleLayerClass)
    ['{7F7016A6-5BD2-49F0-B73C-521C05D80C17}']
  end;
  MLNCircleStyleLayer = interface(MLNVectorStyleLayer)
    ['{CF89C221-E3E6-4046-B113-AEBE3B7632A3}']
    function initWithIdentifier(identifier: NSString; source: MLNSource): MLNCircleStyleLayer; cdecl;
    procedure setCircleSortKey(circleSortKey: NSExpression); cdecl;
    function circleSortKey: NSExpression; cdecl;
    procedure setCircleBlur(circleBlur: NSExpression); cdecl;
    function circleBlur: NSExpression; cdecl;
    procedure setCircleBlurTransition(circleBlurTransition: MLNTransition); cdecl;
    function circleBlurTransition: MLNTransition; cdecl;
    procedure setCircleColor(circleColor: NSExpression); cdecl;
    function circleColor: NSExpression; cdecl;
    procedure setCircleColorTransition(circleColorTransition: MLNTransition); cdecl;
    function circleColorTransition: MLNTransition; cdecl;
    procedure setCircleOpacity(circleOpacity: NSExpression); cdecl;
    function circleOpacity: NSExpression; cdecl;
    procedure setCircleOpacityTransition(circleOpacityTransition: MLNTransition); cdecl;
    function circleOpacityTransition: MLNTransition; cdecl;
    procedure setCirclePitchAlignment(circlePitchAlignment: NSExpression); cdecl;
    function circlePitchAlignment: NSExpression; cdecl;
    procedure setCircleRadius(circleRadius: NSExpression); cdecl;
    function circleRadius: NSExpression; cdecl;
    procedure setCircleRadiusTransition(circleRadiusTransition: MLNTransition); cdecl;
    function circleRadiusTransition: MLNTransition; cdecl;
    procedure setCircleScaleAlignment(circleScaleAlignment: NSExpression); cdecl;
    function circleScaleAlignment: NSExpression; cdecl;
    procedure setCircleStrokeColor(circleStrokeColor: NSExpression); cdecl;
    function circleStrokeColor: NSExpression; cdecl;
    procedure setCircleStrokeColorTransition(circleStrokeColorTransition: MLNTransition); cdecl;
    function circleStrokeColorTransition: MLNTransition; cdecl;
    procedure setCircleStrokeOpacity(circleStrokeOpacity: NSExpression); cdecl;
    function circleStrokeOpacity: NSExpression; cdecl;
    procedure setCircleStrokeOpacityTransition(circleStrokeOpacityTransition: MLNTransition); cdecl;
    function circleStrokeOpacityTransition: MLNTransition; cdecl;
    procedure setCircleStrokeWidth(circleStrokeWidth: NSExpression); cdecl;
    function circleStrokeWidth: NSExpression; cdecl;
    procedure setCircleStrokeWidthTransition(circleStrokeWidthTransition: MLNTransition); cdecl;
    function circleStrokeWidthTransition: MLNTransition; cdecl;
    procedure setCircleTranslation(circleTranslation: NSExpression); cdecl;
    function circleTranslation: NSExpression; cdecl;
    procedure setCircleTranslationTransition(circleTranslationTransition: MLNTransition); cdecl;
    function circleTranslationTransition: MLNTransition; cdecl;
    procedure setCircleTranslationAnchor(circleTranslationAnchor: NSExpression); cdecl;
    function circleTranslationAnchor: NSExpression; cdecl;
  end;
  TMLNCircleStyleLayer = class(TOCGenericImport<MLNCircleStyleLayerClass, MLNCircleStyleLayer>)  end;

  {**********************************************************}
  MLNFillStyleLayerClass = interface(MLNVectorStyleLayerClass)
    ['{B82A737C-F17A-4109-949E-42F31FC0C2D0}']
  end;
  MLNFillStyleLayer = interface(MLNVectorStyleLayer)
    ['{35C5E65B-2EEF-4A7F-B141-E8964988EBD8}']
    function initWithIdentifier(identifier: NSString; source: MLNSource): MLNFillStyleLayer; cdecl;
    procedure setFillSortKey(fillSortKey: NSExpression); cdecl;
    function fillSortKey: NSExpression; cdecl;
    procedure setFillAntialiased(fillAntialiased: NSExpression); cdecl;
    function isFillAntialiased: NSExpression; cdecl;
    procedure setFillColor(fillColor: NSExpression); cdecl;
    function fillColor: NSExpression; cdecl;
    procedure setFillColorTransition(fillColorTransition: MLNTransition); cdecl;
    function fillColorTransition: MLNTransition; cdecl;
    procedure setFillOpacity(fillOpacity: NSExpression); cdecl;
    function fillOpacity: NSExpression; cdecl;
    procedure setFillOpacityTransition(fillOpacityTransition: MLNTransition); cdecl;
    function fillOpacityTransition: MLNTransition; cdecl;
    procedure setFillOutlineColor(fillOutlineColor: NSExpression); cdecl;
    function fillOutlineColor: NSExpression; cdecl;
    procedure setFillOutlineColorTransition(fillOutlineColorTransition: MLNTransition); cdecl;
    function fillOutlineColorTransition: MLNTransition; cdecl;
    procedure setFillPattern(fillPattern: NSExpression); cdecl;
    function fillPattern: NSExpression; cdecl;
    procedure setFillPatternTransition(fillPatternTransition: MLNTransition); cdecl;
    function fillPatternTransition: MLNTransition; cdecl;
    procedure setFillTranslation(fillTranslation: NSExpression); cdecl;
    function fillTranslation: NSExpression; cdecl;
    procedure setFillTranslationTransition(fillTranslationTransition: MLNTransition); cdecl;
    function fillTranslationTransition: MLNTransition; cdecl;
    procedure setFillTranslationAnchor(fillTranslationAnchor: NSExpression); cdecl;
    function fillTranslationAnchor: NSExpression; cdecl;
  end;
  TMLNFillStyleLayer = class(TOCGenericImport<MLNFillStyleLayerClass, MLNFillStyleLayer>)  end;

  {*******************************************************************}
  MLNFillExtrusionStyleLayerClass = interface(MLNVectorStyleLayerClass)
    ['{A5FADBC9-CBAB-4EBA-9CCC-1DEA55E0FE05}']
  end;
  MLNFillExtrusionStyleLayer = interface(MLNVectorStyleLayer)
    ['{5DB86BB6-3D04-4028-84E0-BD12AA0BA76C}']
    function initWithIdentifier(identifier: NSString; source: MLNSource): MLNFillExtrusionStyleLayer; cdecl;
    procedure setFillExtrusionRoundedCornerDistance(fillExtrusionRoundedCornerDistance: NSExpression); cdecl;
    function fillExtrusionRoundedCornerDistance: NSExpression; cdecl;
    procedure setFillExtrusionBase(fillExtrusionBase: NSExpression); cdecl;
    function fillExtrusionBase: NSExpression; cdecl;
    procedure setFillExtrusionBaseTransition(fillExtrusionBaseTransition: MLNTransition); cdecl;
    function fillExtrusionBaseTransition: MLNTransition; cdecl;
    procedure setFillExtrusionColor(fillExtrusionColor: NSExpression); cdecl;
    function fillExtrusionColor: NSExpression; cdecl;
    procedure setFillExtrusionColorTransition(fillExtrusionColorTransition: MLNTransition); cdecl;
    function fillExtrusionColorTransition: MLNTransition; cdecl;
    procedure setFillExtrusionHasVerticalGradient(fillExtrusionHasVerticalGradient: NSExpression); cdecl;
    function fillExtrusionHasVerticalGradient: NSExpression; cdecl;
    procedure setFillExtrusionHeight(fillExtrusionHeight: NSExpression); cdecl;
    function fillExtrusionHeight: NSExpression; cdecl;
    procedure setFillExtrusionHeightTransition(fillExtrusionHeightTransition: MLNTransition); cdecl;
    function fillExtrusionHeightTransition: MLNTransition; cdecl;
    procedure setFillExtrusionOpacity(fillExtrusionOpacity: NSExpression); cdecl;
    function fillExtrusionOpacity: NSExpression; cdecl;
    procedure setFillExtrusionOpacityTransition(fillExtrusionOpacityTransition: MLNTransition); cdecl;
    function fillExtrusionOpacityTransition: MLNTransition; cdecl;
    procedure setFillExtrusionPattern(fillExtrusionPattern: NSExpression); cdecl;
    function fillExtrusionPattern: NSExpression; cdecl;
    procedure setFillExtrusionPatternTransition(fillExtrusionPatternTransition: MLNTransition); cdecl;
    function fillExtrusionPatternTransition: MLNTransition; cdecl;
    procedure setFillExtrusionTranslation(fillExtrusionTranslation: NSExpression); cdecl;
    function fillExtrusionTranslation: NSExpression; cdecl;
    procedure setFillExtrusionTranslationTransition(fillExtrusionTranslationTransition: MLNTransition); cdecl;
    function fillExtrusionTranslationTransition: MLNTransition; cdecl;
    procedure setFillExtrusionTranslationAnchor(fillExtrusionTranslationAnchor: NSExpression); cdecl;
    function fillExtrusionTranslationAnchor: NSExpression; cdecl;
  end;
  TMLNFillExtrusionStyleLayer = class(TOCGenericImport<MLNFillExtrusionStyleLayerClass, MLNFillExtrusionStyleLayer>)  end;

  {*************************************************************}
  MLNHeatmapStyleLayerClass = interface(MLNVectorStyleLayerClass)
    ['{14547B7F-6B7A-4983-9FEA-3AFF5C83BBD2}']
  end;
  MLNHeatmapStyleLayer = interface(MLNVectorStyleLayer)
    ['{5C8AAAB6-130C-4F05-8472-0A918C017A97}']
    function initWithIdentifier(identifier: NSString; source: MLNSource): MLNHeatmapStyleLayer; cdecl;
    procedure setHeatmapColor(heatmapColor: NSExpression); cdecl;
    function heatmapColor: NSExpression; cdecl;
    procedure setHeatmapIntensity(heatmapIntensity: NSExpression); cdecl;
    function heatmapIntensity: NSExpression; cdecl;
    procedure setHeatmapIntensityTransition(heatmapIntensityTransition: MLNTransition); cdecl;
    function heatmapIntensityTransition: MLNTransition; cdecl;
    procedure setHeatmapOpacity(heatmapOpacity: NSExpression); cdecl;
    function heatmapOpacity: NSExpression; cdecl;
    procedure setHeatmapOpacityTransition(heatmapOpacityTransition: MLNTransition); cdecl;
    function heatmapOpacityTransition: MLNTransition; cdecl;
    procedure setHeatmapRadius(heatmapRadius: NSExpression); cdecl;
    function heatmapRadius: NSExpression; cdecl;
    procedure setHeatmapRadiusTransition(heatmapRadiusTransition: MLNTransition); cdecl;
    function heatmapRadiusTransition: MLNTransition; cdecl;
    procedure setHeatmapWeight(heatmapWeight: NSExpression); cdecl;
    function heatmapWeight: NSExpression; cdecl;
  end;
  TMLNHeatmapStyleLayer = class(TOCGenericImport<MLNHeatmapStyleLayerClass, MLNHeatmapStyleLayer>)  end;

  {****************************************************************}
  MLNRasterStyleLayerClass = interface(MLNForegroundStyleLayerClass)
    ['{71604985-5E61-42F4-A114-8F23CB4151F9}']
  end;
  MLNRasterStyleLayer = interface(MLNForegroundStyleLayer)
    ['{44BAC31C-B28F-44BA-92EB-86E9338BC45E}']
    function initWithIdentifier(identifier: NSString; source: MLNSource): MLNRasterStyleLayer; cdecl;
    procedure setMaximumRasterBrightness(maximumRasterBrightness: NSExpression); cdecl;
    function maximumRasterBrightness: NSExpression; cdecl;
    procedure setMaximumRasterBrightnessTransition(maximumRasterBrightnessTransition: MLNTransition); cdecl;
    function maximumRasterBrightnessTransition: MLNTransition; cdecl;
    procedure setMinimumRasterBrightness(minimumRasterBrightness: NSExpression); cdecl;
    function minimumRasterBrightness: NSExpression; cdecl;
    procedure setMinimumRasterBrightnessTransition(minimumRasterBrightnessTransition: MLNTransition); cdecl;
    function minimumRasterBrightnessTransition: MLNTransition; cdecl;
    procedure setRasterContrast(rasterContrast: NSExpression); cdecl;
    function rasterContrast: NSExpression; cdecl;
    procedure setRasterContrastTransition(rasterContrastTransition: MLNTransition); cdecl;
    function rasterContrastTransition: MLNTransition; cdecl;
    procedure setRasterFadeDuration(rasterFadeDuration: NSExpression); cdecl;
    function rasterFadeDuration: NSExpression; cdecl;
    procedure setRasterHueRotation(rasterHueRotation: NSExpression); cdecl;
    function rasterHueRotation: NSExpression; cdecl;
    procedure setRasterHueRotationTransition(rasterHueRotationTransition: MLNTransition); cdecl;
    function rasterHueRotationTransition: MLNTransition; cdecl;
    procedure setRasterOpacity(rasterOpacity: NSExpression); cdecl;
    function rasterOpacity: NSExpression; cdecl;
    procedure setRasterOpacityTransition(rasterOpacityTransition: MLNTransition); cdecl;
    function rasterOpacityTransition: MLNTransition; cdecl;
    procedure setRasterResamplingMode(rasterResamplingMode: NSExpression); cdecl;
    function rasterResamplingMode: NSExpression; cdecl;
    procedure setRasterSaturation(rasterSaturation: NSExpression); cdecl;
    function rasterSaturation: NSExpression; cdecl;
    procedure setRasterSaturationTransition(rasterSaturationTransition: MLNTransition); cdecl;
    function rasterSaturationTransition: MLNTransition; cdecl;
  end;
  TMLNRasterStyleLayer = class(TOCGenericImport<MLNRasterStyleLayerClass, MLNRasterStyleLayer>)  end;

  {**************************************}
  MLNShapeClass = interface(NSObjectClass)
    ['{73DF31E1-CEF4-4F8F-AB25-947812B4D27F}']
    {class} function shapeWithData(data: NSData; encoding: NSStringEncoding; error: PNSError): MLNShape; cdecl;
  end;
  MLNShape = interface(NSObject)
    ['{51443E38-34A5-4CE3-99A7-E748437980F3}']
    //procedure setTitle(title: NSString); cdecl;
    //function title: NSString; cdecl;
    //procedure setSubtitle(subtitle: NSString); cdecl;
    //function subtitle: NSString; cdecl;
    //function geoJSONDataUsingEncoding(encoding: NSStringEncoding): NSData; cdecl;
  end;
  TMLNShape = class(TOCGenericImport<MLNShapeClass, MLNShape>)  end;

  {************************************************}
  MLNPointAnnotationClass = interface(MLNShapeClass)
    ['{D28495C9-F1F9-4C4A-9B47-F9D6E3133EFD}']
  end;
  MLNPointAnnotation = interface(MLNShape)
    ['{0EAC30D7-B3AF-4144-B5CC-8FE9BF7BEDFD}']
    //procedure setCoordinate(coordinate: CLLocationCoordinate2D); cdecl;
    //function coordinate: CLLocationCoordinate2D; cdecl;
  end;
  TMLNPointAnnotation = class(TOCGenericImport<MLNPointAnnotationClass, MLNPointAnnotation>)  end;

  {*******************************************************}
  MLNPointFeatureClass = interface(MLNPointAnnotationClass)
    ['{E9396CFB-4E8E-4DA6-B6F8-FCB59FD16B9E}']
  end;
  MLNPointFeature = interface(MLNPointAnnotation)
    ['{3FBB1118-F4E9-420F-8A91-814C19493E6C}']
    function attributeForKey(key: NSString): Pointer; cdecl; // From MLNFeature
  end;
  TMLNPointFeature = class(TOCGenericImport<MLNPointFeatureClass, MLNPointFeature>)  end;

  {***************************************}
  MLNSourceClass = interface(NSObjectClass)
    ['{410DB797-CDF4-4639-B821-E312D1BD4D7D}']
  end;
  MLNSource = interface(NSObject)
    ['{D7CE6079-9865-48CC-AE12-0103BA283AD7}']
    function initWithIdentifier(identifier: NSString): MLNSource; cdecl;
    //procedure setIdentifier(identifier: NSString); cdecl;
    //function identifier: NSString; cdecl;
  end;
  TMLNSource = class(TOCGenericImport<MLNSourceClass, MLNSource>) end;

  {*********************************************}
  MLNShapeSourceClass = interface(MLNSourceClass)
    ['{609DE105-5CF8-4933-B7D3-FB503D453C17}']
  end;
  MLNShapeSource = interface(MLNSource)
    ['{2484FAEC-2CFA-42F4-8C02-8FB074BDDD0A}']
    function initWithIdentifier(identifier: NSString; URL: NSURL; options: NSDictionary): MLNShapeSource; cdecl; overload;
    function initWithIdentifier(identifier: NSString; shape: MLNShape; options: NSDictionary): MLNShapeSource; cdecl; overload;
    function initWithIdentifier(identifier: NSString; features: NSArray; options: NSDictionary): MLNShapeSource; cdecl; overload;
    //function initWithIdentifier(identifier: NSString; shapes: NSArray; options: NSDictionary): MLNShapeSource; cdecl; overload;
    procedure setShape(shape: MLNShape); cdecl;
    function shape: MLNShape; cdecl;
    procedure setURL(URL: NSURL); cdecl;
    function URL: NSURL; cdecl;
    //function featuresMatchingPredicate(predicate: NSPredicate): NSArray; cdecl;
    //function leavesOfCluster(cluster: MLNPointFeatureCluster; offset: NSUInteger; limit: NSUInteger): NSArray; cdecl;
    //function childrenOfCluster(cluster: MLNPointFeatureCluster): NSArray; cdecl;
    //function zoomLevelForExpandingCluster(cluster: MLNPointFeatureCluster): double; cdecl;
    //function setFeatureStateForFeatureID(featureID: NSString; state: NSDictionary): BOOL; cdecl;
    //function featureStateForFeatureID(featureID: NSString): NSDictionary; cdecl;
    //function removeFeatureStateForFeatureID(featureID: NSString; stateKey: NSString): BOOL; cdecl; overload;
    //function removeFeatureStateForFeatureID(featureID: NSString): BOOL; cdecl; overload;
    //function resetFeatureStates: BOOL; cdecl;
  end;
  TMLNShapeSource = class(TOCGenericImport<MLNShapeSourceClass, MLNShapeSource>) end;

  {********************************************}
  MLNTileSourceClass = interface(MLNSourceClass)
    ['{A7676C59-8F82-426F-8E39-7DEBF6F2F664}']
  end;
  MLNTileSource = interface(MLNSource)
    ['{7A9AC063-BD96-4A93-8D13-E372D986C735}']
    //function configurationURL: NSURL; cdecl;
    //function attributionInfos: NSArray; cdecl;
    //function attributionHTMLString: NSString; cdecl;
  end;
  TMLNTileSource = class(TOCGenericImport<MLNTileSourceClass, MLNTileSource>)  end;

  {******************************************************}
  MLNRasterTileSourceClass = interface(MLNTileSourceClass)
    ['{8B1F1779-083C-4D68-B30B-F07C0BA3C919}']
  end;
  MLNRasterTileSource = interface(MLNTileSource)
    ['{9B40E4CE-5568-44D4-9FCD-F1F0FD44FF57}']
    function initWithIdentifier(identifier: NSString; configurationURL: NSURL): MLNRasterTileSource; cdecl; overload;
    function initWithIdentifier(identifier: NSString; configurationURL: NSURL; tileSize: CGFloat): MLNRasterTileSource; cdecl; overload;
    function initWithIdentifier(identifier: NSString; tileURLTemplates: NSArray; options: NSDictionary): MLNRasterTileSource; cdecl; overload;
  end;
  TMLNRasterTileSource = class(TOCGenericImport<MLNRasterTileSourceClass, MLNRasterTileSource>)  end;

  {******************************************}
  MLNMapCameraClass = interface(NSObjectClass)
    ['{BD47268F-D6F0-46F0-95E5-A6F76388B1BE}']
    //{class} function camera: MLNMapCamera; cdecl; overload;
    //{class} function camera(centerCoordinate: CLLocationCoordinate2D; fromEyeCoordinate: CLLocationCoordinate2D; eyeAltitude: CLLocationDistance): MLNMapCamera; cdecl; overload;
    //{class} function camera(centerCoordinate: CLLocationCoordinate2D; acrossDistance: CLLocationDistance; pitch: CGFloat; heading: CLLocationDirection): MLNMapCamera; cdecl; overload;
    //{class} function camera(centerCoordinate: CLLocationCoordinate2D; altitude: CLLocationDistance; pitch: CGFloat; heading: CLLocationDirection): MLNMapCamera; cdecl; overload;
    //{class} function camera(centerCoordinate: CLLocationCoordinate2D; fromDistance: CLLocationDistance; pitch: CGFloat; heading: CLLocationDirection): MLNMapCamera; cdecl; overload;
  end;
  MLNMapCamera = interface(NSObject)
    ['{9337DEDD-8F6A-4626-B843-3F16E7E019AC}']
    procedure setCenterCoordinate(centerCoordinate: CLLocationCoordinate2D); cdecl;
    function centerCoordinate: CLLocationCoordinate2D; cdecl;
    procedure setHeading(heading: CLLocationDirection); cdecl;
    function heading: CLLocationDirection; cdecl;
    procedure setRoll(roll: CLLocationDirection); cdecl;
    function roll: CLLocationDirection; cdecl;
    procedure setPitch(pitch: CGFloat); cdecl;
    function pitch: CGFloat; cdecl;
    procedure setAltitude(altitude: CLLocationDistance); cdecl;
    function altitude: CLLocationDistance; cdecl;
    procedure setViewingDistance(viewingDistance: CLLocationDistance); cdecl;
    function viewingDistance: CLLocationDistance; cdecl;
    function isEqualToMapCamera(otherCamera: MLNMapCamera): Boolean; cdecl;
  end;
  TMLNMapCamera = class(TOCGenericImport<MLNMapCameraClass, MLNMapCamera>)  end;

  {**************************************}
  MLNMapViewClass = interface(UIViewClass)
    ['{E0CDBA9B-3811-47C0-9D17-BA2F2B59AC4D}']
  end;
  MLNMapView = interface(UIView)
    ['{C4251D44-9A60-4FFC-9B8C-B88749B5A0A3}']
    function initWithFrame(frame: CGRect): MLNMapView; cdecl; overload;
    function initWithFrame(frame: CGRect; styleURL: NSURL): MLNMapView; cdecl; overload;
    function initWithFrame(frame: CGRect; styleJSON: NSString): MLNMapView; cdecl; overload;
    //function initWithFrame(frame: CGRect; options: MLNMapOptions): MLNMapView; cdecl; overload;
    function delegate: MLNMapViewDelegate; cdecl;
    procedure setDelegate(delegate: MLNMapViewDelegate); cdecl;
    function style: MLNStyle; cdecl;
    function styleURL: NSURL; cdecl;
    procedure setStyleURL(styleURL: NSURL); cdecl;
    function styleJSON: NSString; cdecl;
    procedure setStyleJSON(styleJSON: NSString); cdecl;
    //procedure reloadStyle(sender: Pointer); cdecl;
    //function automaticallyAdjustsContentInset: Boolean; cdecl;
    //procedure setAutomaticallyAdjustsContentInset(automaticallyAdjustsContentInset: Boolean); cdecl;
    //function showsScale: Boolean; cdecl;
    //procedure setShowsScale(showsScale: Boolean); cdecl;
    //function scaleBar: MLNScaleBar; cdecl;
    //function scaleBarShouldShowDarkStyles: Boolean; cdecl;
    //procedure setScaleBarShouldShowDarkStyles(scaleBarShouldShowDarkStyles: Boolean); cdecl;
    //function scaleBarUsesMetricSystem: Boolean; cdecl;
    //procedure setScaleBarUsesMetricSystem(scaleBarUsesMetricSystem: Boolean); cdecl;
    //function scaleBarPosition: MLNOrnamentPosition; cdecl;
    //procedure setScaleBarPosition(scaleBarPosition: MLNOrnamentPosition); cdecl;
    //function scaleBarMargins: CGPoint; cdecl;
    //procedure setScaleBarMargins(scaleBarMargins: CGPoint); cdecl;
    function showsCompassView: Boolean; cdecl;
    procedure setShowsCompassView(showsCompassView: Boolean); cdecl;
    function compassView: MLNCompassButton; cdecl;
    function compassViewPosition: MLNOrnamentPosition; cdecl;
    procedure setCompassViewPosition(compassViewPosition: MLNOrnamentPosition); cdecl;
    function compassViewMargins: CGPoint; cdecl;
    procedure setCompassViewMargins(compassViewMargins: CGPoint); cdecl;
    //function showsLogoView: Boolean; cdecl;
    //procedure setShowsLogoView(showsLogoView: Boolean); cdecl;
    //function logoView: UIImageView; cdecl;
    //function logoViewPosition: MLNOrnamentPosition; cdecl;
    //procedure setLogoViewPosition(logoViewPosition: MLNOrnamentPosition); cdecl;
    //function logoViewMargins: CGPoint; cdecl;
    //procedure setLogoViewMargins(logoViewMargins: CGPoint); cdecl;
    //function showsAttributionButton: Boolean; cdecl;
    //procedure setShowsAttributionButton(showsAttributionButton: Boolean); cdecl;
    //function attributionButton: UIButton; cdecl;
    //function attributionButtonPosition: MLNOrnamentPosition; cdecl;
    //procedure setAttributionButtonPosition(attributionButtonPosition: MLNOrnamentPosition); cdecl;
    //function attributionButtonMargins: CGPoint; cdecl;
    //procedure setAttributionButtonMargins(attributionButtonMargins: CGPoint); cdecl;
    //procedure showAttribution(sender: Pointer); cdecl;
    //function preferredFramesPerSecond: NSInteger; cdecl;
    //procedure setPreferredFramesPerSecond(preferredFramesPerSecond: NSInteger); cdecl;
    //function prefetchesTiles: Boolean; cdecl;
    //procedure setPrefetchesTiles(prefetchesTiles: Boolean); cdecl;
    //function tileCacheEnabled: Boolean; cdecl;
    //procedure setTileCacheEnabled(tileCacheEnabled: Boolean); cdecl;
    //function tileLodMinRadius: Double; cdecl;
    //procedure setTileLodMinRadius(tileLodMinRadius: Double); cdecl;
    //function tileLodScale: Double; cdecl;
    //procedure setTileLodScale(tileLodScale: Double); cdecl;
    //function tileLodPitchThreshold: Double; cdecl;
    //procedure setTileLodPitchThreshold(tileLodPitchThreshold: Double); cdecl;
    //function tileLodZoomShift: Double; cdecl;
    //procedure setTileLodZoomShift(tileLodZoomShift: Double); cdecl;
    //function frustumOffset: UIEdgeInsets; cdecl;
    //procedure setFrustumOffset(frustumOffset: UIEdgeInsets); cdecl;
    //procedure disableLocationManager; cdecl;
    //function locationManager: Pointer; cdecl;
    //procedure setLocationManager(locationManager: Pointer); cdecl;
    //function showsUserLocation: Boolean; cdecl;
    //procedure setShowsUserLocation(showsUserLocation: Boolean); cdecl;
    //function dynamicNavigationCameraAnimationDuration: Boolean; cdecl;
    //procedure setDynamicNavigationCameraAnimationDuration(dynamicNavigationCameraAnimationDuration: Boolean); cdecl;
    //function shouldRequestAuthorizationToUseLocationServices: Boolean; cdecl;
    //procedure setShouldRequestAuthorizationToUseLocationServices(shouldRequestAuthorizationToUseLocationServices: Boolean); cdecl;
    //function isUserLocationVisible: Boolean; cdecl;
    //function userLocation: MLNUserLocation; cdecl;
    //function userTrackingMode: MLNUserTrackingMode; cdecl;
    //procedure setUserTrackingMode(mode: MLNUserTrackingMode; animated: Boolean); cdecl; overload;
    //procedure setUserTrackingMode(mode: MLNUserTrackingMode; animated: Boolean; completion: Pointer); cdecl; overload;
    //procedure setUserTrackingMode(userTrackingMode: MLNUserTrackingMode); cdecl; overload;
    //function userLocationVerticalAlignment: MLNAnnotationVerticalAlignment; cdecl;
    //procedure setUserLocationVerticalAlignment(alignment: MLNAnnotationVerticalAlignment; animated: Boolean); cdecl; overload;
    //procedure setUserLocationVerticalAlignment(userLocationVerticalAlignment: MLNAnnotationVerticalAlignment); cdecl; overload;
    //procedure updateUserLocationAnnotationView; cdecl;
    //procedure updateUserLocationAnnotationViewAnimatedWithDuration(duration: NSTimeInterval); cdecl;
    //function showsUserHeadingIndicator: Boolean; cdecl;
    //procedure setShowsUserHeadingIndicator(showsUserHeadingIndicator: Boolean); cdecl;
    //function displayHeadingCalibration: Boolean; cdecl;
    //procedure setDisplayHeadingCalibration(displayHeadingCalibration: Boolean); cdecl;
    //function targetCoordinate: CLLocationCoordinate2D; cdecl;
    //procedure setTargetCoordinate(targetCoordinate: CLLocationCoordinate2D; animated: Boolean); cdecl; overload;
    //procedure setTargetCoordinate(targetCoordinate: CLLocationCoordinate2D; animated: Boolean; completion: Pointer); cdecl; overload;
    //procedure setTargetCoordinate(targetCoordinate: CLLocationCoordinate2D); cdecl; overload;
    //function isZoomEnabled: Boolean; cdecl;
    //procedure setZoomEnabled(zoomEnabled: Boolean); cdecl;
    //function isQuickZoomReversed: Boolean; cdecl;
    //procedure setQuickZoomReversed(quickZoomReversed: Boolean); cdecl;
    //function isScrollEnabled: Boolean; cdecl;
    //procedure setScrollEnabled(scrollEnabled: Boolean); cdecl;
    //function panScrollingMode: MLNPanScrollingMode; cdecl;
    //procedure setPanScrollingMode(panScrollingMode: MLNPanScrollingMode); cdecl;
    //function isRotateEnabled: Boolean; cdecl;
    //procedure setRotateEnabled(rotateEnabled: Boolean); cdecl;
    //function toleranceForSnappingToNorth: CGFloat; cdecl;
    //procedure setToleranceForSnappingToNorth(toleranceForSnappingToNorth: CGFloat); cdecl;
    //function isPitchEnabled: Boolean; cdecl;
    //procedure setPitchEnabled(pitchEnabled: Boolean); cdecl;
    //function anchorRotateOrZoomGesturesToCenterCoordinate: Boolean; cdecl;
    //procedure setAnchorRotateOrZoomGesturesToCenterCoordinate(anchorRotateOrZoomGesturesToCenterCoordinate: Boolean); cdecl;
    //function isHapticFeedbackEnabled: Boolean; cdecl;
    //procedure setHapticFeedbackEnabled(hapticFeedbackEnabled: Boolean); cdecl;
    //function decelerationRate: CGFloat; cdecl;
    //procedure setDecelerationRate(decelerationRate: CGFloat); cdecl;
    function centerCoordinate: CLLocationCoordinate2D; cdecl;
    procedure setCenterCoordinate(centerCoordinate: CLLocationCoordinate2D); cdecl; overload;
    procedure setCenterCoordinate(centerCoordinate: CLLocationCoordinate2D; animated: Boolean); cdecl; overload;
    procedure setCenterCoordinate(centerCoordinate: CLLocationCoordinate2D; zoomLevel: Double; animated: Boolean); cdecl; overload;
    procedure setCenterCoordinate(centerCoordinate: CLLocationCoordinate2D; zoomLevel: Double; direction: Double; animated: Boolean); cdecl; overload;
    //procedure setCenterCoordinate(centerCoordinate: CLLocationCoordinate2D; zoomLevel: Double; direction: Double; animated: Boolean; completion: Pointer); cdecl; overload;
    function zoomLevel: Double; cdecl;
    procedure setZoomLevel(zoomLevel: Double); cdecl; overload;
    procedure setZoomLevel(zoomLevel: Double; animated: Boolean); cdecl; overload;
    //function minimumZoomLevel: Double; cdecl;
    //procedure setMinimumZoomLevel(minimumZoomLevel: Double); cdecl;
    //function maximumZoomLevel: Double; cdecl;
    //procedure setMaximumZoomLevel(maximumZoomLevel: Double); cdecl;
    //function maximumScreenBounds: MLNCoordinateBounds; cdecl;
    //procedure setMaximumScreenBounds(maximumScreenBounds: MLNCoordinateBounds); cdecl;
    //function direction: Double; cdecl;
    //procedure setDirection(direction: Double; animated: Boolean); cdecl; overload;
    //procedure setDirection(direction: Double); cdecl; overload;
    //function minimumPitch: CGFloat; cdecl;
    //procedure setMinimumPitch(minimumPitch: CGFloat); cdecl;
    //function maximumPitch: CGFloat; cdecl;
    //procedure setMaximumPitch(maximumPitch: CGFloat); cdecl;
    //procedure resetNorth; cdecl;
    //procedure resetPosition; cdecl;
    function visibleCoordinateBounds: MLNCoordinateBounds; cdecl;
    procedure setVisibleCoordinateBounds(visibleCoordinateBounds: MLNCoordinateBounds); cdecl; overload;
    procedure setVisibleCoordinateBounds(visibleCoordinateBounds: MLNCoordinateBounds; animated: Boolean); cdecl; overload;
    //procedure setVisibleCoordinateBounds(visibleCoordinateBounds: MLNCoordinateBounds; insets: UIEdgeInsets; animated: Boolean); cdecl; overload;
    //procedure setVisibleCoordinateBounds(visibleCoordinateBounds: MLNCoordinateBounds; insets: UIEdgeInsets; animated: Boolean; completion: Pointer); cdecl; overload;
    //procedure setVisibleCoordinates(coordinates: Pointer; count: NSUInteger; insets: UIEdgeInsets; animated: Boolean); cdecl; overload;
    //procedure setVisibleCoordinates(coordinates: Pointer; count: NSUInteger; insets: UIEdgeInsets; direction: Double; duration: NSTimeInterval; &function: CAMediaTimingFunction; completion: Pointer); cdecl; overload;
    //procedure showAnnotations(annotations: NSArray; animated: Boolean); cdecl; overload;
    //procedure showAnnotations(annotations: NSArray; insets: UIEdgeInsets; animated: Boolean); cdecl; overload;
    //procedure showAnnotations(annotations: NSArray; insets: UIEdgeInsets; animated: Boolean; completion: Pointer); cdecl; overload;
    function camera: MLNMapCamera; cdecl;
    procedure setCamera(camera: MLNMapCamera); cdecl; overload;
    procedure setCamera(camera: MLNMapCamera; animated: Boolean); cdecl; overload;
    //procedure setCamera(camera: MLNMapCamera; duration: NSTimeInterval; &function: CAMediaTimingFunction); cdecl; overload;
    //procedure setCamera(camera: MLNMapCamera; duration: NSTimeInterval; &function: CAMediaTimingFunction; completion: Pointer); cdecl; overload;
    //procedure setCamera(camera: MLNMapCamera; duration: NSTimeInterval; &function: CAMediaTimingFunction; edgePadding: UIEdgeInsets; completion: Pointer); cdecl; overload;
    //procedure flyToCamera(camera: MLNMapCamera; completion: Pointer); cdecl; overload;
    //procedure flyToCamera(camera: MLNMapCamera; duration: NSTimeInterval; completion: Pointer); cdecl; overload;
    //procedure flyToCamera(camera: MLNMapCamera; duration: NSTimeInterval; peakAltitude: Double; completion: Pointer); cdecl; overload;
    //procedure flyToCamera(camera: MLNMapCamera; insets: UIEdgeInsets; duration: NSTimeInterval; completion: Pointer); cdecl; overload;
    //function cameraThatFitsCoordinateBounds(bounds: MLNCoordinateBounds): MLNMapCamera; cdecl; overload;
    //function cameraThatFitsCoordinateBounds(bounds: MLNCoordinateBounds; insets: UIEdgeInsets): MLNMapCamera; cdecl; overload;
    //function camera(camera: MLNMapCamera; bounds: MLNCoordinateBounds; insets: UIEdgeInsets): MLNMapCamera; cdecl;
    //function camera(camera: MLNMapCamera; shape: MLNShape; insets: UIEdgeInsets): MLNMapCamera; cdecl;
    //function cameraThatFitsShape(shape: MLNShape; direction: Double; insets: UIEdgeInsets): MLNMapCamera; cdecl;
    //function anchorPointForGesture(gesture: UIGestureRecognizer): CGPoint; cdecl;
    //function contentInset: UIEdgeInsets; cdecl;
    //procedure setContentInset(contentInset: UIEdgeInsets; animated: Boolean); cdecl; overload;
    //procedure setContentInset(contentInset: UIEdgeInsets; animated: Boolean; completion: Pointer); cdecl; overload;
    //procedure setContentInset(contentInset: UIEdgeInsets); cdecl; overload;
    function cameraEdgeInsets: UIEdgeInsets; cdecl;
    [MethodName('convertPoint:toCoordinateFromView:')]
    function convertPoint(point: CGPoint; view: UIView): CLLocationCoordinate2D; cdecl;
    //function convertCoordinate(coordinate: CLLocationCoordinate2D; view: UIView): CGPoint; cdecl;
    //function convertRect(rect: CGRect; view: UIView): MLNCoordinateBounds; cdecl;
    //function convertCoordinateBounds(bounds: MLNCoordinateBounds; view: UIView): CGRect; cdecl;
    //function metersPerPointAtLatitude(latitude: Double): Double; cdecl;
    //function mapProjection: MLNMapProjection; cdecl;
    //function annotations: NSArray; cdecl;
    //procedure addAnnotation(annotation: Pointer); cdecl;
    //procedure addAnnotations(annotations: NSArray); cdecl;
    //procedure removeAnnotation(annotation: Pointer); cdecl;
    //procedure removeAnnotations(annotations: NSArray); cdecl;
    //function visibleAnnotations: NSArray; cdecl;
    //function viewForAnnotation(annotation: Pointer): MLNAnnotationView; cdecl;
    //function dequeueReusableAnnotationImageWithIdentifier(identifier: NSString): MLNAnnotationImage; cdecl;
    //function dequeueReusableAnnotationViewWithIdentifier(identifier: NSString): MLNAnnotationView; cdecl;
    //function visibleAnnotationsInRect(rect: CGRect): NSArray; cdecl;
    //function selectedAnnotations: NSArray; cdecl;
    //procedure setSelectedAnnotations(selectedAnnotations: NSArray); cdecl;
    //procedure selectAnnotation(annotation: Pointer; animated: Boolean); cdecl; overload;
    //procedure selectAnnotation(annotation: Pointer; animated: Boolean; completion: Pointer); cdecl; overload;
    //procedure selectAnnotation(annotation: Pointer; moveIntoView: Boolean; animateSelection: Boolean; completion: Pointer); cdecl; overload;
    //procedure deselectAnnotation(annotation: Pointer; animated: Boolean); cdecl;
    //function overlays: NSArray; cdecl;
    //procedure addOverlay(overlay: Pointer); cdecl;
    //procedure addOverlays(overlays: NSArray); cdecl;
    //procedure removeOverlay(overlay: Pointer); cdecl;
    //procedure removeOverlays(overlays: NSArray); cdecl;
    //function visibleFeaturesAtPoint(point: CGPoint): NSArray; cdecl; overload;
    [MethodName('visibleFeaturesAtPoint:inStyleLayersWithIdentifiers:')]
    function visibleFeaturesAtPoint(point: CGPoint; styleLayerIdentifiers: NSSet): NSArray; cdecl; overload;
    //function visibleFeaturesAtPoint(point: CGPoint; styleLayerIdentifiers: NSSet; predicate: NSPredicate): NSArray; cdecl; overload;
    //function visibleFeaturesInRect(rect: CGRect): NSArray; cdecl; overload;
    //function visibleFeaturesInRect(rect: CGRect; styleLayerIdentifiers: NSSet): NSArray; cdecl; overload;
    //function visibleFeaturesInRect(rect: CGRect; styleLayerIdentifiers: NSSet; predicate: NSPredicate): NSArray; cdecl; overload;
    //function debugMask: NSUInteger; cdecl;
    //procedure setDebugMask(debugMask: NSUInteger); cdecl;
    //function isRenderingStatsViewEnabled: Boolean; cdecl;
    //procedure enableRenderingStatsView(value: Boolean); cdecl;
    //function getActionJournalLogFiles: NSArray; cdecl;
    //function getActionJournalLog: NSArray; cdecl;
    //procedure clearActionJournalLog; cdecl;
    //function backendResource: MLNBackendResource; cdecl;
    //procedure triggerRepaint; cdecl;
    //procedure addPluginLayerType(pluginLayerClass: Pointer); cdecl;
  end;
  TMLNMapView = class(TOCGenericImport<MLNMapViewClass, MLNMapView>) end;

  {*****************************************}
  MLNMapViewDelegate = interface(IObjectiveC)
    ['{BA609162-DEB3-48D0-80AC-3F4F5CC4F85E}']
    //[MethodName('mapView:shouldChangeFromCamera:toCamera:')]
    //function mapViewShouldChangeFromCameraToCamera(mapView: MLNMapView; shouldChangeFromCamera: MLNMapCamera; toCamera: MLNMapCamera): BOOL; cdecl;
    //[MethodName('mapView:shouldChangeFromCamera:toCamera:reason:')]
    //function mapViewShouldChangeFromCameraToCameraReason(mapView: MLNMapView; shouldChangeFromCamera: MLNMapCamera; toCamera: MLNMapCamera; reason: MLNCameraChangeReason): BOOL; cdecl;
    //[MethodName('mapView:regionWillChangeAnimated:')]
    //procedure mapViewRegionWillChangeAnimated(mapView: MLNMapView; regionWillChangeAnimated: BOOL); cdecl;
    [MethodName('mapView:regionWillChangeWithReason:animated:')]
    procedure mapViewRegionWillChangeWithReasonAnimated(mapView: MLNMapView; regionWillChangeWithReason: MLNCameraChangeReason; animated: Boolean); cdecl;
    //procedure mapViewRegionIsChanging(mapView: MLNMapView); cdecl;
    [MethodName('mapView:regionIsChangingWithReason:')]
    procedure mapViewRegionIsChangingWithReason(mapView: MLNMapView; regionIsChangingWithReason: MLNCameraChangeReason); cdecl;
    //[MethodName('mapView:regionDidChangeAnimated:')]
    //procedure mapViewRegionDidChangeAnimated(mapView: MLNMapView; regionDidChangeAnimated: BOOL); cdecl;
    [MethodName('mapView:regionDidChangeWithReason:animated:')]
    procedure mapViewRegionDidChangeWithReasonAnimated(mapView: MLNMapView; regionDidChangeWithReason: MLNCameraChangeReason; animated: Boolean); cdecl;
    //procedure mapViewWillStartLoadingMap(mapView: MLNMapView); cdecl;
    //procedure mapViewDidFinishLoadingMap(mapView: MLNMapView); cdecl;
    //procedure mapViewDidFailLoadingMap(mapView: MLNMapView; withError: NSError); cdecl;
    //procedure mapViewWillStartRenderingMap(mapView: MLNMapView); cdecl;
    //procedure mapViewDidFinishRenderingMap(mapView: MLNMapView; fullyRendered: BOOL); cdecl;
    //procedure mapViewWillStartRenderingFrame(mapView: MLNMapView); cdecl;
    //[MethodName('mapViewDidFinishRenderingFrame:fullyRendered:')]
    //procedure mapViewDidFinishRenderingFrameFullyRendered(mapView: MLNMapView; fullyRendered: BOOL); cdecl;
    //[MethodName('mapViewDidFinishRenderingFrame:fullyRendered:frameEncodingTime:frameRenderingTime:')]
    //procedure mapViewDidFinishRenderingFrameFullyRenderedFrameEncodingTimeFrameRenderingTime(mapView: MLNMapView; fullyRendered: BOOL; frameEncodingTime: double; frameRenderingTime: double); cdecl;
    //[MethodName('mapViewDidFinishRenderingFrame:fullyRendered:renderingStats:')]
    //procedure mapViewDidFinishRenderingFrameFullyRenderedRenderingStats(mapView: MLNMapView; fullyRendered: BOOL; renderingStats: MLNRenderingStats); cdecl;
    //procedure mapViewDidBecomeIdle(mapView: MLNMapView); cdecl;
    [MethodName('mapView:didFinishLoadingStyle:')]
    procedure mapViewDidFinishLoadingStyle(mapView: MLNMapView; didFinishLoadingStyle: MLNStyle); cdecl;
    //[MethodName('mapView:sourceDidChange:')]
    //procedure mapViewSourceDidChange(mapView: MLNMapView; sourceDidChange: MLNSource); cdecl;
    //[MethodName('mapView:didFailToLoadImage:')]
    //function mapViewDidFailToLoadImage(mapView: MLNMapView; didFailToLoadImage: NSString): UIImage; cdecl;
    //[MethodName('mapView:shouldRemoveStyleImage:')]
    //function mapViewShouldRemoveStyleImage(mapView: MLNMapView; shouldRemoveStyleImage: NSString): BOOL; cdecl;
    //[MethodName('mapView:shaderWillCompile:backend:defines:')]
    //procedure mapViewShaderWillCompileBackendDefines(mapView: MLNMapView; shaderWillCompile: NSInteger; backend: NSInteger; defines: NSString); cdecl;
    //[MethodName('mapView:shaderDidCompile:backend:defines:')]
    //procedure mapViewShaderDidCompileBackendDefines(mapView: MLNMapView; shaderDidCompile: NSInteger; backend: NSInteger; defines: NSString); cdecl;
    //[MethodName('mapView:shaderDidFailCompile:backend:defines:')]
    //procedure mapViewShaderDidFailCompileBackendDefines(mapView: MLNMapView; shaderDidFailCompile: NSInteger; backend: NSInteger; defines: NSString); cdecl;
    //[MethodName('mapView:glyphsWillLoad:range:')]
    //procedure mapViewGlyphsWillLoadRange(mapView: MLNMapView; glyphsWillLoad: NSArray; range: NSRange); cdecl;
    //[MethodName('mapView:glyphsDidLoad:range:')]
    //procedure mapViewGlyphsDidLoadRange(mapView: MLNMapView; glyphsDidLoad: NSArray; range: NSRange); cdecl;
    //[MethodName('mapView:glyphsDidError:range:')]
    //procedure mapViewGlyphsDidErrorRange(mapView: MLNMapView; glyphsDidError: NSArray; range: NSRange); cdecl;
    //[MethodName('mapView:tileDidTriggerAction:x:y:z:wrap:overscaledZ:sourceID:')]
    //procedure mapViewTileDidTriggerActionXYZWrapOverscaledZSourceID(mapView: MLNMapView; tileDidTriggerAction: MLNTileOperation; x: NSInteger; y: NSInteger; z: NSInteger; wrap: NSInteger; overscaledZ: NSInteger; sourceID: NSString); cdecl;
    //[MethodName('mapView:spriteWillLoad:url:')]
    //procedure mapViewSpriteWillLoadUrl(mapView: MLNMapView; spriteWillLoad: NSString; url: NSString); cdecl;
    //[MethodName('mapView:spriteDidLoad:url:')]
    //procedure mapViewSpriteDidLoadUrl(mapView: MLNMapView; spriteDidLoad: NSString; url: NSString); cdecl;
    //[MethodName('mapView:spriteDidError:url:')]
    //procedure mapViewSpriteDidErrorUrl(mapView: MLNMapView; spriteDidError: NSString; url: NSString); cdecl;
    //procedure mapViewRendererDidError(mapView: MLNMapView); cdecl;
    //procedure mapViewWillStartLocatingUser(mapView: MLNMapView); cdecl;
    //procedure mapViewDidStopLocatingUser(mapView: MLNMapView); cdecl;
    //function mapViewStyleForDefaultUserLocationAnnotationView(mapView: MLNMapView): MLNUserLocationAnnotationViewStyle; cdecl;
    //[MethodName('mapView:didUpdateUserLocation:')]
    //procedure mapViewDidUpdateUserLocation(mapView: MLNMapView; didUpdateUserLocation: MLNUserLocation); cdecl;
    //[MethodName('mapView:didFailToLocateUserWithError:')]
    //procedure mapViewDidFailToLocateUserWithError(mapView: MLNMapView; didFailToLocateUserWithError: NSError); cdecl;
    //[MethodName('mapView:didChangeUserTrackingMode:animated:')]
    //procedure mapViewDidChangeUserTrackingModeAnimated(mapView: MLNMapView; didChangeUserTrackingMode: MLNUserTrackingMode; animated: BOOL); cdecl;
    //function mapViewUserLocationAnchorPoint(mapView: MLNMapView): CGPoint; cdecl;
    //[MethodName('mapView:didChangeLocationManagerAuthorization:')]
    //procedure mapViewDidChangeLocationManagerAuthorization(mapView: MLNMapView; didChangeLocationManagerAuthorization: id); cdecl;
    //[MethodName('mapView:imageForAnnotation:')]
    //function mapViewImageForAnnotation(mapView: MLNMapView; imageForAnnotation: id): MLNAnnotationImage; cdecl;
    //[MethodName('mapView:alphaForShapeAnnotation:')]
    //function mapViewAlphaForShapeAnnotation(mapView: MLNMapView; alphaForShapeAnnotation: MLNShape): CGFloat; cdecl;
    //[MethodName('mapView:strokeColorForShapeAnnotation:')]
    //function mapViewStrokeColorForShapeAnnotation(mapView: MLNMapView; strokeColorForShapeAnnotation: MLNShape): UIColor; cdecl;
    //[MethodName('mapView:fillColorForPolygonAnnotation:')]
    //function mapViewFillColorForPolygonAnnotation(mapView: MLNMapView; fillColorForPolygonAnnotation: MLNPolygon): UIColor; cdecl;
    //[MethodName('mapView:lineWidthForPolylineAnnotation:')]
    //function mapViewLineWidthForPolylineAnnotation(mapView: MLNMapView; lineWidthForPolylineAnnotation: MLNPolyline): CGFloat; cdecl;
    //[MethodName('mapView:viewForAnnotation:')]
    //function mapViewViewForAnnotation(mapView: MLNMapView; viewForAnnotation: id): MLNAnnotationView; cdecl;
    //[MethodName('mapView:didAddAnnotationViews:')]
    //procedure mapViewDidAddAnnotationViews(mapView: MLNMapView; didAddAnnotationViews: NSArray); cdecl;
    //[MethodName('mapView:shapeAnnotationIsEnabled:')]
    //function mapViewShapeAnnotationIsEnabled(mapView: MLNMapView; shapeAnnotationIsEnabled: MLNShape): BOOL; cdecl;
    //[MethodName('mapView:didSelectAnnotation:')]
    //procedure mapViewDidSelectAnnotation(mapView: MLNMapView; didSelectAnnotation: id); cdecl;
    //[MethodName('mapView:didDeselectAnnotation:')]
    //procedure mapViewDidDeselectAnnotation(mapView: MLNMapView; didDeselectAnnotation: id); cdecl;
    //[MethodName('mapView:didSelectAnnotationView:')]
    //procedure mapViewDidSelectAnnotationView(mapView: MLNMapView; didSelectAnnotationView: MLNAnnotationView); cdecl;
    //[MethodName('mapView:didDeselectAnnotationView:')]
    //procedure mapViewDidDeselectAnnotationView(mapView: MLNMapView; didDeselectAnnotationView: MLNAnnotationView); cdecl;
    //[MethodName('mapView:annotationCanShowCallout:')]
    //function mapViewAnnotationCanShowCallout(mapView: MLNMapView; annotationCanShowCallout: id): BOOL; cdecl;
    //[MethodName('mapView:calloutViewForAnnotation:')]
    //function mapViewCalloutViewForAnnotation(mapView: MLNMapView; calloutViewForAnnotation: id): id; cdecl;
    //[MethodName('mapView:leftCalloutAccessoryViewForAnnotation:')]
    //function mapViewLeftCalloutAccessoryViewForAnnotation(mapView: MLNMapView; leftCalloutAccessoryViewForAnnotation: id): UIView; cdecl;
    //[MethodName('mapView:rightCalloutAccessoryViewForAnnotation:')]
    //function mapViewRightCalloutAccessoryViewForAnnotation(mapView: MLNMapView; rightCalloutAccessoryViewForAnnotation: id): UIView; cdecl;
    //[MethodName('mapView:annotation:calloutAccessoryControlTapped:')]
    //procedure mapViewAnnotationCalloutAccessoryControlTapped(mapView: MLNMapView; annotation: id; calloutAccessoryControlTapped: id); cdecl;
    //[MethodName('mapView:tapOnCalloutForAnnotation:')]
    //procedure mapViewTapOnCalloutForAnnotation(mapView: MLNMapView; tapOnCalloutForAnnotation: id); cdecl;
  end;

implementation

{*************************************************************}
procedure MapLibreLoader; cdecl; external framework 'MapLibre';
procedure CoreImageLoader; cdecl; external framework 'CoreImage';
procedure SystemConfigurationLoader; cdecl; external framework 'SystemConfiguration';
procedure SQLite3Loader; cdecl; external '/usr/lib/libsqlite3.dylib';

end.