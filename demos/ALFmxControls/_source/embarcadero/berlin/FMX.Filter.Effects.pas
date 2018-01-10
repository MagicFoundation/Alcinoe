{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

// This unit auto-generated do not edit directly

unit FMX.Filter.Effects;

{$HPPEMIT LINKUNIT}

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.UITypes, FMX.Types, FMX.Effects, FMX.Filter, FMX.Graphics;

type
  TFilterBaseFilter = class(TFmxObject)
  private
    FFilter: TFilter;
    FInputFilter: TFilterBaseFilter;
    function GetOutput: TBitmap;
    procedure SetInput(const AValue: TBitmap);
    procedure SetInputFilter(const AValue: TFilterBaseFilter);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Input: TBitmap write SetInput;
    property Output: TBitmap read GetOutput;
  published
    property InputFilter: TFilterBaseFilter read FInputFilter write SetInputFilter;
  end;

  //A transition effect.
  TFilterPixelateTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
  end;

  //A transition effect.
  TFilterBrightTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
  end;

  //A transition effect.
  TFilterSwirlTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetStrength: Single;
    procedure SetStrength(AValue: Single);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The amount of twist to the spiral.
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The amount of twist to the spiral.
    property Strength: Single read GetStrength write SetStrength;
  end;

  //A transition effect.
  TFilterDissolveTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
    function GetSecond: TBitmap;
    procedure SetSecond(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The seed value that determines dripiness.
    // Second desription
    property Second: TBitmap read GetSecond write SetSecond;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed;
    // Second desription
  end;

  //A transition effect.
  TFilterBloodTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
    function GetSecond: TBitmap;
    procedure SetSecond(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The seed value that determines dripiness.
    // Second desription
    property Second: TBitmap read GetSecond write SetSecond;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed;
    // Second desription
  end;

  //A transition effect.
  TFilterBlurTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
  end;

  //A transition effect.
  TFilterDropTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
    function GetSecond: TBitmap;
    procedure SetSecond(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The seed value that determines dripiness.
    // Second desription
    property Second: TBitmap read GetSecond write SetSecond;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed;
    // Second desription
  end;

  //A transition effect.
  TFilterCrumpleTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
    function GetSecond: TBitmap;
    procedure SetSecond(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The seed value that determines dripiness.
    // Second desription
    property Second: TBitmap read GetSecond write SetSecond;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed;
    // Second desription
  end;

  //A transition effect.
  TFilterWaterTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
    function GetSecond: TBitmap;
    procedure SetSecond(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The seed value that determines dripiness.
    // Second desription
    property Second: TBitmap read GetSecond write SetSecond;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed;
    // Second desription
  end;

  //A transition effect.
  TFilterBandedSwirlTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetStrength: Single;
    procedure SetStrength(AValue: Single);
    function GetFrequency: Single;
    procedure SetFrequency(AValue: Single);
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The amount of twist to the spiral.
    // The frequency of the spiral.
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The amount of twist to the spiral.
    property Strength: Single read GetStrength write SetStrength;
    // The frequency of the spiral.
    property Frequency: Single read GetFrequency write SetFrequency;
    // The center point of the ripples.
  end;

  //A swipe transition effect.
  TFilterSwipeTransition = class(TFilterBaseFilter)
  private
    function GetMousePoint: TPointF;
    procedure SetMousePoint(AValue: TPointF);
    function GetCornerPoint: TPointF;
    procedure SetCornerPoint(AValue: TPointF);
    function GetDeep: Single;
    procedure SetDeep(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetBack: TBitmap;
    procedure SetBack(const AValue: TBitmap);
  public
    // The point of coursor
    property MousePoint: TPointF read GetMousePoint write SetMousePoint;
    // The point of corner
    property CornerPoint: TPointF read GetCornerPoint write SetCornerPoint;
    // The deep of fold
    // Target desription
    property Target: TBitmap read GetTarget write SetTarget;
    // Back page texture
    property Back: TBitmap read GetBack write SetBack;
  published
    // The point of coursor
    // The point of corner
    // The deep of fold
    property Deep: Single read GetDeep write SetDeep;
    // Target desription
    // Back page texture
  end;

  //A transition effect.
  TFilterSlideTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetSlideAmount: TPointF;
    procedure SetSlideAmount(AValue: TPointF);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The center point of the ripples.
    property SlideAmount: TPointF read GetSlideAmount write SetSlideAmount;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The center point of the ripples.
  end;

  //A transition effect.
  TFilterMagnifyTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The center point of the ripples.
  end;

  //A transition effect.
  TFilterLineTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetOrigin: TPointF;
    procedure SetOrigin(AValue: TPointF);
    function GetNormal: TPointF;
    procedure SetNormal(AValue: TPointF);
    function GetOffsetProp: TPointF;
    procedure SetOffsetProp(AValue: TPointF);
    function GetFuzzyAmount: Single;
    procedure SetFuzzyAmount(AValue: Single);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The line origin.
    property Origin: TPointF read GetOrigin write SetOrigin;
    // The line normal.
    property Normal: TPointF read GetNormal write SetNormal;
    // The line offset.
    property OffsetProp: TPointF read GetOffsetProp write SetOffsetProp;
    // The fuzziness factor.
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The line origin.
    // The line normal.
    // The line offset.
    // The fuzziness factor.
    property FuzzyAmount: Single read GetFuzzyAmount write SetFuzzyAmount;
  end;

  //A transition effect.
  TFilterRotateCrumpleTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
    function GetSecond: TBitmap;
    procedure SetSecond(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The seed value that determines dripiness.
    // Second desription
    property Second: TBitmap read GetSecond write SetSecond;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed;
    // Second desription
  end;

  //A transition effect.
  TFilterRippleTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
  end;

  //A transition effect.
  TFilterCircleTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetFuzzyAmount: Single;
    procedure SetFuzzyAmount(AValue: Single);
    function GetCircleSize: Single;
    procedure SetCircleSize(AValue: Single);
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The fuzziness factor.
    // The size of the circle.
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The fuzziness factor.
    property FuzzyAmount: Single read GetFuzzyAmount write SetFuzzyAmount;
    // The size of the circle.
    property CircleSize: Single read GetCircleSize write SetCircleSize;
    // The center point of the ripples.
  end;

  //A transition effect.
  TFilterWiggleTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
    function GetSecond: TBitmap;
    procedure SetSecond(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The seed value that determines dripiness.
    // Second desription
    property Second: TBitmap read GetSecond write SetSecond;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed;
    // Second desription
  end;

  //A transition effect.
  TFilterSaturateTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
  end;

  //A transition effect.
  TFilterWaveTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
  end;

  //A transition effect.
  TFilterFadeTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
  end;

  //A transition effect.
  TFilterBlindTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetNumberOfBlinds: Single;
    procedure SetNumberOfBlinds(AValue: Single);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The number of Blinds strips
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The number of Blinds strips
    property NumberOfBlinds: Single read GetNumberOfBlinds write SetNumberOfBlinds;
  end;

  //A transition effect.
  TFilterShapeTransition = class(TFilterBaseFilter)
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
    function GetSecond: TBitmap;
    procedure SetSecond(const AValue: TBitmap);
  public
    // The amount(%) of the transition from first texture to the second texture.
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
    // The seed value that determines dripiness.
    // Second desription
    property Second: TBitmap read GetSecond write SetSecond;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress;
    // The target bitmap.
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed;
    // Second desription
  end;

  //An pencil stroke effect.
  TFilterPencilStroke = class(TFilterBaseFilter)
  private
    function GetbrushSize: Single;
    procedure SetbrushSize(AValue: Single);
  public
    // The brush size of the sketch effect.
  published
    // The brush size of the sketch effect.
    property brushSize: Single read GetbrushSize write SetbrushSize;
  end;

  //An effect that sharpens the input.
  TFilterSharpen = class(TFilterBaseFilter)
  private
    function GetAmount: Single;
    procedure SetAmount(AValue: Single);
  public
    // The amount of sharpening.
  published
    // The amount of sharpening.
    property Amount: Single read GetAmount write SetAmount;
  end;

  //An effect that embosses the input.
  TFilterEmboss = class(TFilterBaseFilter)
  private
    function GetAmount: Single;
    procedure SetAmount(AValue: Single);
    function GetWidth: Single;
    procedure SetWidth(AValue: Single);
  public
    // The amplitude of the embossing.
    // The separation between samples (as a fraction of input size).
  published
    // The amplitude of the embossing.
    property Amount: Single read GetAmount write SetAmount;
    // The separation between samples (as a fraction of input size).
    property Width: Single read GetWidth write SetWidth;
  end;

  //Sepia effect.
  TFilterSepia = class(TFilterBaseFilter)
  private
    function GetAmount: Single;
    procedure SetAmount(AValue: Single);
  public
    // The amount of sharpening.
  published
    // The amount of sharpening.
    property Amount: Single read GetAmount write SetAmount;
  end;

  //An paper sketch effect.
  TFilterPaperSketch = class(TFilterBaseFilter)
  private
    function GetbrushSize: Single;
    procedure SetbrushSize(AValue: Single);
  public
    // The brush size of the sketch effect.
  published
    // The brush size of the sketch effect.
    property brushSize: Single read GetbrushSize write SetbrushSize;
  end;

  //Pixelate a image.
  TFilterPixelate = class(TFilterBaseFilter)
  private
    function GetBlockCount: Single;
    procedure SetBlockCount(AValue: Single);
  public
    // The number of pixel blocks.
  published
    // The number of pixel blocks.
    property BlockCount: Single read GetBlockCount write SetBlockCount;
  end;

  //An effect that applies cartoon-like shading (posterization).
  TFilterToon = class(TFilterBaseFilter)
  private
    function GetLevels: Single;
    procedure SetLevels(AValue: Single);
  public
    // The number of color levels to use.
  published
    // The number of color levels to use.
    property Levels: Single read GetLevels write SetLevels;
  end;

  //An effect that applies a wave pattern to the input.
  TFilterWave = class(TFilterBaseFilter)
  private
    function GetTime: Single;
    procedure SetTime(AValue: Single);
    function GetWaveSize: Single;
    procedure SetWaveSize(AValue: Single);
  public
    // The moment in time. Animate this value over a long period of time. The speed depends on the size. The larger the size, the larger the increase in time on every frame, thus from 0 to 2048 in a smaller amount of time.
    // The distance between waves. (the higher the value the closer the waves are to their neighbor).
  published
    // The moment in time. Animate this value over a long period of time. The speed depends on the size. The larger the size, the larger the increase in time on every frame, thus from 0 to 2048 in a smaller amount of time.
    property Time: Single read GetTime write SetTime;
    // The distance between waves. (the higher the value the closer the waves are to their neighbor).
    property WaveSize: Single read GetWaveSize write SetWaveSize;
  end;

  //An effect that swirls the input in alternating clockwise and counterclockwise bands.
  TFilterBandedSwirl = class(TFilterBaseFilter)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetBands: Single;
    procedure SetBands(AValue: Single);
    function GetStrength: Single;
    procedure SetStrength(AValue: Single);
    function GetAspectRatio: Single;
    procedure SetAspectRatio(AValue: Single);
  public
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
    // The number of bands in the swirl.
    // The amount of twist to the spiral.
    // he aspect ratio (width / height) of the input.
  published
    // The center point of the ripples.
    // The number of bands in the swirl.
    property Bands: Single read GetBands write SetBands;
    // The amount of twist to the spiral.
    property Strength: Single read GetStrength write SetStrength;
    // he aspect ratio (width / height) of the input.
    property AspectRatio: Single read GetAspectRatio write SetAspectRatio;
  end;

  //An effect that magnifies a circular region.
  TFilterSmoothMagnify = class(TFilterBaseFilter)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetInnerRadius: Single;
    procedure SetInnerRadius(AValue: Single);
    function GetOuterRadius: Single;
    procedure SetOuterRadius(AValue: Single);
    function GetMagnification: Single;
    procedure SetMagnification(AValue: Single);
    function GetAspectRatio: Single;
    procedure SetAspectRatio(AValue: Single);
  public
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
    // The inner radius of the magnified region.
    // The outer radius of the magnified region.
    // The magnification factor.
    // The aspect ratio (width / height) of the input.
  published
    // The center point of the ripples.
    // The inner radius of the magnified region.
    property InnerRadius: Single read GetInnerRadius write SetInnerRadius;
    // The outer radius of the magnified region.
    property OuterRadius: Single read GetOuterRadius write SetOuterRadius;
    // The magnification factor.
    property Magnification: Single read GetMagnification write SetMagnification;
    // The aspect ratio (width / height) of the input.
    property AspectRatio: Single read GetAspectRatio write SetAspectRatio;
  end;

  //Wrap image by two Bezier curves.
  TFilterWrap = class(TFilterBaseFilter)
  private
    function GetLeftStart: Single;
    procedure SetLeftStart(AValue: Single);
    function GetLeftControl1: Single;
    procedure SetLeftControl1(AValue: Single);
    function GetLeftControl2: Single;
    procedure SetLeftControl2(AValue: Single);
    function GetLeftEnd: Single;
    procedure SetLeftEnd(AValue: Single);
    function GetRightStart: Single;
    procedure SetRightStart(AValue: Single);
    function GetRightControl1: Single;
    procedure SetRightControl1(AValue: Single);
    function GetRightControl2: Single;
    procedure SetRightControl2(AValue: Single);
    function GetRightEnd: Single;
    procedure SetRightEnd(AValue: Single);
  public
    // Left wrap curve start point
    // Left wrap curve control point 1
    // Left wrap curve control point 2
    // Left wrap curve end point
    // Right wrap curve start point
    // Right wrap curve control point 1
    // Right wrap curve control point 2
    // Right wrap curve end point
  published
    // Left wrap curve start point
    property LeftStart: Single read GetLeftStart write SetLeftStart;
    // Left wrap curve control point 1
    property LeftControl1: Single read GetLeftControl1 write SetLeftControl1;
    // Left wrap curve control point 2
    property LeftControl2: Single read GetLeftControl2 write SetLeftControl2;
    // Left wrap curve end point
    property LeftEnd: Single read GetLeftEnd write SetLeftEnd;
    // Right wrap curve start point
    property RightStart: Single read GetRightStart write SetRightStart;
    // Right wrap curve control point 1
    property RightControl1: Single read GetRightControl1 write SetRightControl1;
    // Right wrap curve control point 2
    property RightControl2: Single read GetRightControl2 write SetRightControl2;
    // Right wrap curve end point
    property RightEnd: Single read GetRightEnd write SetRightEnd;
  end;

  //An effect that swirls the input in a spiral.
  TFilterSwirl = class(TFilterBaseFilter)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetSpiralStrength: Single;
    procedure SetSpiralStrength(AValue: Single);
    function GetAspectRatio: Single;
    procedure SetAspectRatio(AValue: Single);
  public
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
    // The amount of twist to the spiral.
    // The aspect ratio (width / height) of the input.
  published
    // The center point of the ripples.
    // The amount of twist to the spiral.
    property SpiralStrength: Single read GetSpiralStrength write SetSpiralStrength;
    // The aspect ratio (width / height) of the input.
    property AspectRatio: Single read GetAspectRatio write SetAspectRatio;
  end;

  //An effect that creates bands of bright regions.
  TFilterBands = class(TFilterBaseFilter)
  private
    function GetBandDensity: Single;
    procedure SetBandDensity(AValue: Single);
    function GetBandIntensity: Single;
    procedure SetBandIntensity(AValue: Single);
  public
    // The number of verical bands to add to the output. The higher the value the more bands.
    // Intensity of each band.
  published
    // The number of verical bands to add to the output. The higher the value the more bands.
    property BandDensity: Single read GetBandDensity write SetBandDensity;
    // Intensity of each band.
    property BandIntensity: Single read GetBandIntensity write SetBandIntensity;
  end;

  //An effect that magnifies a circular region.
  TFilterMagnify = class(TFilterBaseFilter)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetRadius: Single;
    procedure SetRadius(AValue: Single);
    function GetMagnification: Single;
    procedure SetMagnification(AValue: Single);
    function GetAspectRatio: Single;
    procedure SetAspectRatio(AValue: Single);
  public
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
    // The radius of the magnified region.
    // The magnification factor.
    // The aspect ratio (width / height) of the input.
  published
    // The center point of the ripples.
    // The radius of the magnified region.
    property Radius: Single read GetRadius write SetRadius;
    // The magnification factor.
    property Magnification: Single read GetMagnification write SetMagnification;
    // The aspect ratio (width / height) of the input.
    property AspectRatio: Single read GetAspectRatio write SetAspectRatio;
  end;

  //An effect that pinches a circular region.
  TFilterPinch = class(TFilterBaseFilter)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetRadius: Single;
    procedure SetRadius(AValue: Single);
    function GetStrength: Single;
    procedure SetStrength(AValue: Single);
    function GetAspectRatio: Single;
    procedure SetAspectRatio(AValue: Single);
  public
    // The center point of the pinched region.
    property Center: TPointF read GetCenter write SetCenter;
    // The radius of the pinched region.
    // The amount of twist to the spiral.
    // The aspect ratio (width / height) of the input.
  published
    // The center point of the pinched region.
    // The radius of the pinched region.
    property Radius: Single read GetRadius write SetRadius;
    // The amount of twist to the spiral.
    property Strength: Single read GetStrength write SetStrength;
    // The aspect ratio (width / height) of the input.
    property AspectRatio: Single read GetAspectRatio write SetAspectRatio;
  end;

  //An effect that superimposes rippling waves upon the input.
  TFilterRipple = class(TFilterBaseFilter)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetAmplitude: Single;
    procedure SetAmplitude(AValue: Single);
    function GetFrequency: Single;
    procedure SetFrequency(AValue: Single);
    function GetPhase: Single;
    procedure SetPhase(AValue: Single);
    function GetAspectRatio: Single;
    procedure SetAspectRatio(AValue: Single);
  public
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
    // The amplitude of the ripples.
    // The frequency of the ripples.
    // The phase of the ripples.
    // The aspect ratio (width / height) of the input.
  published
    // The center point of the ripples.
    // The amplitude of the ripples.
    property Amplitude: Single read GetAmplitude write SetAmplitude;
    // The frequency of the ripples.
    property Frequency: Single read GetFrequency write SetFrequency;
    // The phase of the ripples.
    property Phase: Single read GetPhase write SetPhase;
    // The aspect ratio (width / height) of the input.
    property AspectRatio: Single read GetAspectRatio write SetAspectRatio;
  end;

  //Applies an perspective transform to an image.
  TFilterPerspectiveTransform = class(TFilterBaseFilter)
  private
    function GetTopLeft: TPointF;
    procedure SetTopLeft(AValue: TPointF);
    function GetTopRight: TPointF;
    procedure SetTopRight(AValue: TPointF);
    function GetBottomRight: TPointF;
    procedure SetBottomRight(AValue: TPointF);
    function GetBottomLeft: TPointF;
    procedure SetBottomLeft(AValue: TPointF);
  public
    // Top left point of result transformation.
    property TopLeft: TPointF read GetTopLeft write SetTopLeft;
    // Top right point of result transformation.
    property TopRight: TPointF read GetTopRight write SetTopRight;
    // Bottom right point of result transformation.
    property BottomRight: TPointF read GetBottomRight write SetBottomRight;
    // Bottom left point of result transformation.
    property BottomLeft: TPointF read GetBottomLeft write SetBottomLeft;
  published
    // Top left point of result transformation.
    // Top right point of result transformation.
    // Bottom right point of result transformation.
    // Bottom left point of result transformation.
  end;

  //The size and shape of the cropped image depend on the rectangle you specify.
  TFilterCrop = class(TFilterBaseFilter)
  private
    function GetLeftTop: TPointF;
    procedure SetLeftTop(AValue: TPointF);
    function GetRightBottom: TPointF;
    procedure SetRightBottom(AValue: TPointF);
  public
    // Left-top corner of cropping rect
    property LeftTop: TPointF read GetLeftTop write SetLeftTop;
    // Left-top corner of cropping rect
    property RightBottom: TPointF read GetRightBottom write SetRightBottom;
  published
    // Left-top corner of cropping rect
    // Left-top corner of cropping rect
  end;

  //Applies an affine transform to an image.
  TFilterAffineTransform = class(TFilterBaseFilter)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetRotation: Single;
    procedure SetRotation(AValue: Single);
    function GetScale: Single;
    procedure SetScale(AValue: Single);
  public
    // The center point of the rotation.
    property Center: TPointF read GetCenter write SetCenter;
    // Rotation angle in degrees.
    // Scale value as floating.
  published
    // The center point of the rotation.
    // Rotation angle in degrees.
    property Rotation: Single read GetRotation write SetRotation;
    // Scale value as floating.
    property Scale: Single read GetScale write SetScale;
  end;

  //Pixel shader tiles the image across multiple rows and columns.
  TFilterTiler = class(TFilterBaseFilter)
  private
    function GetVerticalTileCount: Single;
    procedure SetVerticalTileCount(AValue: Single);
    function GetHorizontalTileCount: Single;
    procedure SetHorizontalTileCount(AValue: Single);
    function GetHorizontalOffset: Single;
    procedure SetHorizontalOffset(AValue: Single);
    function GetVerticalOffset: Single;
    procedure SetVerticalOffset(AValue: Single);
  public
    // The number of verical tiles to add to the output. The higher the value the more tiles.
    // The number of horizontal tiles to add to the output. The higher the value the more tiles.
    // Change the horizontal offset of each tile.
    // Change the vertical offset of each tile.
  published
    // The number of verical tiles to add to the output. The higher the value the more tiles.
    property VerticalTileCount: Single read GetVerticalTileCount write SetVerticalTileCount;
    // The number of horizontal tiles to add to the output. The higher the value the more tiles.
    property HorizontalTileCount: Single read GetHorizontalTileCount write SetHorizontalTileCount;
    // Change the horizontal offset of each tile.
    property HorizontalOffset: Single read GetHorizontalOffset write SetHorizontalOffset;
    // Change the vertical offset of each tile.
    property VerticalOffset: Single read GetVerticalOffset write SetVerticalOffset;
  end;

  //An effect that blurs in a single direction.
  TFilterDirectionalBlur = class(TFilterBaseFilter)
  private
    function GetAngle: Single;
    procedure SetAngle(AValue: Single);
    function GetBlurAmount: Single;
    procedure SetBlurAmount(AValue: Single);
  public
    // The direction of the blur (in degrees).
    // The scale of the blur (as a fraction of the input size).
  published
    // The direction of the blur (in degrees).
    property Angle: Single read GetAngle write SetAngle;
    // The scale of the blur (as a fraction of the input size).
    property BlurAmount: Single read GetBlurAmount write SetBlurAmount;
  end;

  //An effect that applies a radial blur to the input.
  TFilterRadialBlur = class(TFilterBaseFilter)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetBlurAmount: Single;
    procedure SetBlurAmount(AValue: Single);
  public
    // The center point of the zomming.
    property Center: TPointF read GetCenter write SetCenter;
    // The scale of the blur (as a fraction of the input size).
  published
    // The center point of the zomming.
    // The scale of the blur (as a fraction of the input size).
    property BlurAmount: Single read GetBlurAmount write SetBlurAmount;
  end;

  //An effect that GaussianBlurs.
  TFilterGaussianBlur = class(TFilterBaseFilter)
  private
    function GetBlurAmount: Single;
    procedure SetBlurAmount(AValue: Single);
  public
    // The GaussianBlur factor.
  published
    // The GaussianBlur factor.
    property BlurAmount: Single read GetBlurAmount write SetBlurAmount;
  end;

  //An effect that blurs.
  TFilterBoxBlur = class(TFilterBaseFilter)
  private
    function GetBlurAmount: Single;
    procedure SetBlurAmount(AValue: Single);
  public
    // The blur factor.
  published
    // The blur factor.
    property BlurAmount: Single read GetBlurAmount write SetBlurAmount;
  end;

  //Converts a grayscale image to a white image that is masked by alpha.
  TFilterMaskToAlpha = class(TFilterBaseFilter)
  private
  public
  published
  end;

  //An effect that makes pixels of a particular color transparent.
  TFilterColorKeyAlpha = class(TFilterBaseFilter)
  private
    function GetColorKey: TAlphaColor;
    procedure SetColorKey(AValue: TAlphaColor);
    function GetTolerance: Single;
    procedure SetTolerance(AValue: Single);
  public
    // The color that becomes transparent.
    // The tolerance in color differences.
  published
    // The color that becomes transparent.
    property ColorKey: TAlphaColor read GetColorKey write SetColorKey;
    // The tolerance in color differences.
    property Tolerance: Single read GetTolerance write SetTolerance;
  end;

  //Remaps colors so they fall within shades of a single color.
  TFilterMonochrome = class(TFilterBaseFilter)
  private
  public
  published
  end;

  //An effect that inverts all colors.
  TFilterInvert = class(TFilterBaseFilter)
  private
  public
  published
  end;

  //An effect that intensifies dark regions.
  TFilterGloom = class(TFilterBaseFilter)
  private
    function GetGloomIntensity: Single;
    procedure SetGloomIntensity(AValue: Single);
    function GetBaseIntensity: Single;
    procedure SetBaseIntensity(AValue: Single);
    function GetGloomSaturation: Single;
    procedure SetGloomSaturation(AValue: Single);
    function GetBaseSaturation: Single;
    procedure SetBaseSaturation(AValue: Single);
  public
    // Intensity of the gloom image.
    // Intensity of the base image.
    // Saturation of the gloom image.
    // Saturation of the base image.
  published
    // Intensity of the gloom image.
    property GloomIntensity: Single read GetGloomIntensity write SetGloomIntensity;
    // Intensity of the base image.
    property BaseIntensity: Single read GetBaseIntensity write SetBaseIntensity;
    // Saturation of the gloom image.
    property GloomSaturation: Single read GetGloomSaturation write SetGloomSaturation;
    // Saturation of the base image.
    property BaseSaturation: Single read GetBaseSaturation write SetBaseSaturation;
  end;

  //Changes the overall hue, or tint, of the source pixels.
  TFilterHueAdjust = class(TFilterBaseFilter)
  private
    function GetHue: Single;
    procedure SetHue(AValue: Single);
  public
    // The hue offset.
  published
    // The hue offset.
    property Hue: Single read GetHue write SetHue;
  end;

  //An effect that intensifies bright regions.
  TFilterBloom = class(TFilterBaseFilter)
  private
    function GetBloomIntensity: Single;
    procedure SetBloomIntensity(AValue: Single);
    function GetBaseIntensity: Single;
    procedure SetBaseIntensity(AValue: Single);
    function GetBloomSaturation: Single;
    procedure SetBloomSaturation(AValue: Single);
    function GetBaseSaturation: Single;
    procedure SetBaseSaturation(AValue: Single);
  public
    // Intensity of the bloom image.
    // Intensity of the base image.
    // Saturation of the bloom image.
    // Saturation of the base image.
  published
    // Intensity of the bloom image.
    property BloomIntensity: Single read GetBloomIntensity write SetBloomIntensity;
    // Intensity of the base image.
    property BaseIntensity: Single read GetBaseIntensity write SetBaseIntensity;
    // Saturation of the bloom image.
    property BloomSaturation: Single read GetBloomSaturation write SetBloomSaturation;
    // Saturation of the base image.
    property BaseSaturation: Single read GetBaseSaturation write SetBaseSaturation;
  end;

  //An effect that controls brightness and contrast.
  TFilterContrast = class(TFilterBaseFilter)
  private
    function GetBrightness: Single;
    procedure SetBrightness(AValue: Single);
    function GetContrast: Single;
    procedure SetContrast(AValue: Single);
  public
    // The brightness offset.
    // The contrast multiplier.
  published
    // The brightness offset.
    property Brightness: Single read GetBrightness write SetBrightness;
    // The contrast multiplier.
    property Contrast: Single read GetContrast write SetContrast;
  end;

  //Generates a solid color.
  TFilterFill = class(TFilterBaseFilter)
  private
    function GetColor: TAlphaColor;
    procedure SetColor(AValue: TAlphaColor);
  public
    // The fill color.
  published
    // The fill color.
    property Color: TAlphaColor read GetColor write SetColor;
  end;

  //Fill all pixels with not empty alpha.
  TFilterFillRGB = class(TFilterBaseFilter)
  private
    function GetColor: TAlphaColor;
    procedure SetColor(AValue: TAlphaColor);
  public
    // The fill color.
  published
    // The fill color.
    property Color: TAlphaColor read GetColor write SetColor;
  end;

  //Normal blending of two images.
  TFilterNormalBlend = class(TFilterBaseFilter)
  private
    function GetTarget: TBitmap;
    procedure SetTarget(const AValue: TBitmap);
  public
    // The target bitmap.
    property Target: TBitmap read GetTarget write SetTarget;
  published
    // The target bitmap.
  end;


  TImageFXEffect = class(TFilterEffect)
  protected
    function CreateFilter: TFilter; override;
  published
    property Trigger;
    property Enabled;
  end;



  //A transition effect.
  TPixelateTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
  end;

  //A transition effect.
  TBrightTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
  end;

  //A transition effect.
  TSwirlTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetStrength: Single;
    procedure SetStrength(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The amount of twist to the spiral.
    property Strength: Single read GetStrength write SetStrength nodefault;
  end;

  //A transition effect.
  TDissolveTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed nodefault;
  end;

  //A transition effect.
  TBloodTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed nodefault;
  end;

  //A transition effect.
  TBlurTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
  end;

  //A transition effect.
  TDropTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed nodefault;
  end;

  //A transition effect.
  TCrumpleTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed nodefault;
  end;

  //A transition effect.
  TWaterTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed nodefault;
  end;

  //A transition effect.
  TBandedSwirlTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetStrength: Single;
    procedure SetStrength(AValue: Single);
    function GetFrequency: Single;
    procedure SetFrequency(AValue: Single);
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The amount of twist to the spiral.
    property Strength: Single read GetStrength write SetStrength nodefault;
    // The frequency of the spiral.
    property Frequency: Single read GetFrequency write SetFrequency nodefault;
  end;

  //A swipe transition effect.
  TSwipeTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    FBack: TBitmap;
    procedure BackChanged(Sender: TObject);
  private
    function GetMousePoint: TPointF;
    procedure SetMousePoint(AValue: TPointF);
    function GetCornerPoint: TPointF;
    procedure SetCornerPoint(AValue: TPointF);
    function GetDeep: Single;
    procedure SetDeep(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    procedure SetBack(const AValue: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The point of coursor
    property MousePoint: TPointF read GetMousePoint write SetMousePoint;
    // The point of corner
    property CornerPoint: TPointF read GetCornerPoint write SetCornerPoint;
  published
    // The deep of fold
    property Deep: Single read GetDeep write SetDeep nodefault;
    // Target desription
    property Target: TBitmap read FTarget write SetTarget;
    // Back page texture
    property Back: TBitmap read FBack write SetBack;
  end;

  //A transition effect.
  TSlideTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetSlideAmount: TPointF;
    procedure SetSlideAmount(AValue: TPointF);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The center point of the ripples.
    property SlideAmount: TPointF read GetSlideAmount write SetSlideAmount;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
  end;

  //A transition effect.
  TMagnifyTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
  end;

  //A transition effect.
  TLineTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetOrigin: TPointF;
    procedure SetOrigin(AValue: TPointF);
    function GetNormal: TPointF;
    procedure SetNormal(AValue: TPointF);
    function GetOffsetProp: TPointF;
    procedure SetOffsetProp(AValue: TPointF);
    function GetFuzzyAmount: Single;
    procedure SetFuzzyAmount(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The line origin.
    property Origin: TPointF read GetOrigin write SetOrigin;
    // The line normal.
    property Normal: TPointF read GetNormal write SetNormal;
    // The line offset.
    property OffsetProp: TPointF read GetOffsetProp write SetOffsetProp;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The fuzziness factor.
    property FuzzyAmount: Single read GetFuzzyAmount write SetFuzzyAmount nodefault;
  end;

  //A transition effect.
  TRotateCrumpleTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed nodefault;
  end;

  //A transition effect.
  TRippleTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
  end;

  //A transition effect.
  TCircleTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetFuzzyAmount: Single;
    procedure SetFuzzyAmount(AValue: Single);
    function GetCircleSize: Single;
    procedure SetCircleSize(AValue: Single);
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The fuzziness factor.
    property FuzzyAmount: Single read GetFuzzyAmount write SetFuzzyAmount nodefault;
    // The size of the circle.
    property CircleSize: Single read GetCircleSize write SetCircleSize nodefault;
  end;

  //A transition effect.
  TWiggleTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed nodefault;
  end;

  //A transition effect.
  TSaturateTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
  end;

  //A transition effect.
  TWaveTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
  end;

  //A transition effect.
  TFadeTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
  end;

  //A transition effect.
  TBlindTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetNumberOfBlinds: Single;
    procedure SetNumberOfBlinds(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The number of Blinds strips
    property NumberOfBlinds: Single read GetNumberOfBlinds write SetNumberOfBlinds nodefault;
  end;

  //A transition effect.
  TShapeTransitionEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    function GetProgress: Single;
    procedure SetProgress(AValue: Single);
    procedure SetTarget(const AValue: TBitmap);
    function GetRandomSeed: Single;
    procedure SetRandomSeed(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount(%) of the transition from first texture to the second texture.
    property Progress: Single read GetProgress write SetProgress nodefault;
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
    // The seed value that determines dripiness.
    property RandomSeed: Single read GetRandomSeed write SetRandomSeed nodefault;
  end;

  //An pencil stroke effect.
  TPencilStrokeEffect = class(TImageFXEffect)
  private
    function GetbrushSize: Single;
    procedure SetbrushSize(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The brush size of the sketch effect.
    property brushSize: Single read GetbrushSize write SetbrushSize nodefault;
  end;

  //An effect that sharpens the input.
  TSharpenEffect = class(TImageFXEffect)
  private
    function GetAmount: Single;
    procedure SetAmount(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount of sharpening.
    property Amount: Single read GetAmount write SetAmount nodefault;
  end;

  //An effect that embosses the input.
  TEmbossEffect = class(TImageFXEffect)
  private
    function GetAmount: Single;
    procedure SetAmount(AValue: Single);
    function GetWidth: Single;
    procedure SetWidth(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amplitude of the embossing.
    property Amount: Single read GetAmount write SetAmount nodefault;
    // The separation between samples (as a fraction of input size).
    property Width: Single read GetWidth write SetWidth nodefault;
  end;

  //Sepia effect.
  TSepiaEffect = class(TImageFXEffect)
  private
    function GetAmount: Single;
    procedure SetAmount(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The amount of sharpening.
    property Amount: Single read GetAmount write SetAmount nodefault;
  end;

  //An paper sketch effect.
  TPaperSketchEffect = class(TImageFXEffect)
  private
    function GetbrushSize: Single;
    procedure SetbrushSize(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The brush size of the sketch effect.
    property brushSize: Single read GetbrushSize write SetbrushSize nodefault;
  end;

  //Pixelate a image.
  TPixelateEffect = class(TImageFXEffect)
  private
    function GetBlockCount: Single;
    procedure SetBlockCount(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The number of pixel blocks.
    property BlockCount: Single read GetBlockCount write SetBlockCount nodefault;
  end;

  //An effect that applies cartoon-like shading (posterization).
  TToonEffect = class(TImageFXEffect)
  private
    function GetLevels: Single;
    procedure SetLevels(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The number of color levels to use.
    property Levels: Single read GetLevels write SetLevels nodefault;
  end;

  //An effect that applies a wave pattern to the input.
  TWaveEffect = class(TImageFXEffect)
  private
    function GetTime: Single;
    procedure SetTime(AValue: Single);
    function GetWaveSize: Single;
    procedure SetWaveSize(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The moment in time. Animate this value over a long period of time. The speed depends on the size. The larger the size, the larger the increase in time on every frame, thus from 0 to 2048 in a smaller amount of time.
    property Time: Single read GetTime write SetTime nodefault;
    // The distance between waves. (the higher the value the closer the waves are to their neighbor).
    property WaveSize: Single read GetWaveSize write SetWaveSize nodefault;
  end;

  //An effect that swirls the input in alternating clockwise and counterclockwise bands.
  TBandedSwirlEffect = class(TImageFXEffect)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetBands: Single;
    procedure SetBands(AValue: Single);
    function GetStrength: Single;
    procedure SetStrength(AValue: Single);
    function GetAspectRatio: Single;
    procedure SetAspectRatio(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The number of bands in the swirl.
    property Bands: Single read GetBands write SetBands nodefault;
    // The amount of twist to the spiral.
    property Strength: Single read GetStrength write SetStrength nodefault;
    // he aspect ratio (width / height) of the input.
    property AspectRatio: Single read GetAspectRatio write SetAspectRatio nodefault;
  end;

  //An effect that magnifies a circular region.
  TSmoothMagnifyEffect = class(TImageFXEffect)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetInnerRadius: Single;
    procedure SetInnerRadius(AValue: Single);
    function GetOuterRadius: Single;
    procedure SetOuterRadius(AValue: Single);
    function GetMagnification: Single;
    procedure SetMagnification(AValue: Single);
    function GetAspectRatio: Single;
    procedure SetAspectRatio(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The inner radius of the magnified region.
    property InnerRadius: Single read GetInnerRadius write SetInnerRadius nodefault;
    // The outer radius of the magnified region.
    property OuterRadius: Single read GetOuterRadius write SetOuterRadius nodefault;
    // The magnification factor.
    property Magnification: Single read GetMagnification write SetMagnification nodefault;
    // The aspect ratio (width / height) of the input.
    property AspectRatio: Single read GetAspectRatio write SetAspectRatio nodefault;
  end;

  //Wrap image by two Bezier curves.
  TWrapEffect = class(TImageFXEffect)
  private
    function GetLeftStart: Single;
    procedure SetLeftStart(AValue: Single);
    function GetLeftControl1: Single;
    procedure SetLeftControl1(AValue: Single);
    function GetLeftControl2: Single;
    procedure SetLeftControl2(AValue: Single);
    function GetLeftEnd: Single;
    procedure SetLeftEnd(AValue: Single);
    function GetRightStart: Single;
    procedure SetRightStart(AValue: Single);
    function GetRightControl1: Single;
    procedure SetRightControl1(AValue: Single);
    function GetRightControl2: Single;
    procedure SetRightControl2(AValue: Single);
    function GetRightEnd: Single;
    procedure SetRightEnd(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Left wrap curve start point
    property LeftStart: Single read GetLeftStart write SetLeftStart nodefault;
    // Left wrap curve control point 1
    property LeftControl1: Single read GetLeftControl1 write SetLeftControl1 nodefault;
    // Left wrap curve control point 2
    property LeftControl2: Single read GetLeftControl2 write SetLeftControl2 nodefault;
    // Left wrap curve end point
    property LeftEnd: Single read GetLeftEnd write SetLeftEnd nodefault;
    // Right wrap curve start point
    property RightStart: Single read GetRightStart write SetRightStart nodefault;
    // Right wrap curve control point 1
    property RightControl1: Single read GetRightControl1 write SetRightControl1 nodefault;
    // Right wrap curve control point 2
    property RightControl2: Single read GetRightControl2 write SetRightControl2 nodefault;
    // Right wrap curve end point
    property RightEnd: Single read GetRightEnd write SetRightEnd nodefault;
  end;

  //An effect that swirls the input in a spiral.
  TSwirlEffect = class(TImageFXEffect)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetSpiralStrength: Single;
    procedure SetSpiralStrength(AValue: Single);
    function GetAspectRatio: Single;
    procedure SetAspectRatio(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The amount of twist to the spiral.
    property SpiralStrength: Single read GetSpiralStrength write SetSpiralStrength nodefault;
    // The aspect ratio (width / height) of the input.
    property AspectRatio: Single read GetAspectRatio write SetAspectRatio nodefault;
  end;

  //An effect that creates bands of bright regions.
  TBandsEffect = class(TImageFXEffect)
  private
    function GetBandDensity: Single;
    procedure SetBandDensity(AValue: Single);
    function GetBandIntensity: Single;
    procedure SetBandIntensity(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The number of verical bands to add to the output. The higher the value the more bands.
    property BandDensity: Single read GetBandDensity write SetBandDensity nodefault;
    // Intensity of each band.
    property BandIntensity: Single read GetBandIntensity write SetBandIntensity nodefault;
  end;

  //An effect that magnifies a circular region.
  TMagnifyEffect = class(TImageFXEffect)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetRadius: Single;
    procedure SetRadius(AValue: Single);
    function GetMagnification: Single;
    procedure SetMagnification(AValue: Single);
    function GetAspectRatio: Single;
    procedure SetAspectRatio(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The radius of the magnified region.
    property Radius: Single read GetRadius write SetRadius nodefault;
    // The magnification factor.
    property Magnification: Single read GetMagnification write SetMagnification nodefault;
    // The aspect ratio (width / height) of the input.
    property AspectRatio: Single read GetAspectRatio write SetAspectRatio nodefault;
  end;

  //An effect that pinches a circular region.
  TPinchEffect = class(TImageFXEffect)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetRadius: Single;
    procedure SetRadius(AValue: Single);
    function GetStrength: Single;
    procedure SetStrength(AValue: Single);
    function GetAspectRatio: Single;
    procedure SetAspectRatio(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The center point of the pinched region.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The radius of the pinched region.
    property Radius: Single read GetRadius write SetRadius nodefault;
    // The amount of twist to the spiral.
    property Strength: Single read GetStrength write SetStrength nodefault;
    // The aspect ratio (width / height) of the input.
    property AspectRatio: Single read GetAspectRatio write SetAspectRatio nodefault;
  end;

  //An effect that superimposes rippling waves upon the input.
  TRippleEffect = class(TImageFXEffect)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetAmplitude: Single;
    procedure SetAmplitude(AValue: Single);
    function GetFrequency: Single;
    procedure SetFrequency(AValue: Single);
    function GetPhase: Single;
    procedure SetPhase(AValue: Single);
    function GetAspectRatio: Single;
    procedure SetAspectRatio(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The center point of the ripples.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The amplitude of the ripples.
    property Amplitude: Single read GetAmplitude write SetAmplitude nodefault;
    // The frequency of the ripples.
    property Frequency: Single read GetFrequency write SetFrequency nodefault;
    // The phase of the ripples.
    property Phase: Single read GetPhase write SetPhase nodefault;
    // The aspect ratio (width / height) of the input.
    property AspectRatio: Single read GetAspectRatio write SetAspectRatio nodefault;
  end;

  //Applies an perspective transform to an image.
  TPerspectiveTransformEffect = class(TImageFXEffect)
  private
    function GetTopLeft: TPointF;
    procedure SetTopLeft(AValue: TPointF);
    function GetTopRight: TPointF;
    procedure SetTopRight(AValue: TPointF);
    function GetBottomRight: TPointF;
    procedure SetBottomRight(AValue: TPointF);
    function GetBottomLeft: TPointF;
    procedure SetBottomLeft(AValue: TPointF);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Top left point of result transformation.
    property TopLeft: TPointF read GetTopLeft write SetTopLeft;
    // Top right point of result transformation.
    property TopRight: TPointF read GetTopRight write SetTopRight;
    // Bottom right point of result transformation.
    property BottomRight: TPointF read GetBottomRight write SetBottomRight;
    // Bottom left point of result transformation.
    property BottomLeft: TPointF read GetBottomLeft write SetBottomLeft;
  published
  end;

  //The size and shape of the cropped image depend on the rectangle you specify.
  TCropEffect = class(TImageFXEffect)
  private
    function GetLeftTop: TPointF;
    procedure SetLeftTop(AValue: TPointF);
    function GetRightBottom: TPointF;
    procedure SetRightBottom(AValue: TPointF);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Left-top corner of cropping rect
    property LeftTop: TPointF read GetLeftTop write SetLeftTop;
    // Left-top corner of cropping rect
    property RightBottom: TPointF read GetRightBottom write SetRightBottom;
  published
  end;

  //Applies an affine transform to an image.
  TAffineTransformEffect = class(TImageFXEffect)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetRotation: Single;
    procedure SetRotation(AValue: Single);
    function GetScale: Single;
    procedure SetScale(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The center point of the rotation.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // Rotation angle in degrees.
    property Rotation: Single read GetRotation write SetRotation nodefault;
    // Scale value as floating.
    property Scale: Single read GetScale write SetScale nodefault;
  end;

  //Pixel shader tiles the image across multiple rows and columns.
  TTilerEffect = class(TImageFXEffect)
  private
    function GetVerticalTileCount: Single;
    procedure SetVerticalTileCount(AValue: Single);
    function GetHorizontalTileCount: Single;
    procedure SetHorizontalTileCount(AValue: Single);
    function GetHorizontalOffset: Single;
    procedure SetHorizontalOffset(AValue: Single);
    function GetVerticalOffset: Single;
    procedure SetVerticalOffset(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The number of verical tiles to add to the output. The higher the value the more tiles.
    property VerticalTileCount: Single read GetVerticalTileCount write SetVerticalTileCount nodefault;
    // The number of horizontal tiles to add to the output. The higher the value the more tiles.
    property HorizontalTileCount: Single read GetHorizontalTileCount write SetHorizontalTileCount nodefault;
    // Change the horizontal offset of each tile.
    property HorizontalOffset: Single read GetHorizontalOffset write SetHorizontalOffset nodefault;
    // Change the vertical offset of each tile.
    property VerticalOffset: Single read GetVerticalOffset write SetVerticalOffset nodefault;
  end;

  //An effect that blurs in a single direction.
  TDirectionalBlurEffect = class(TImageFXEffect)
  private
    function GetAngle: Single;
    procedure SetAngle(AValue: Single);
    function GetBlurAmount: Single;
    procedure SetBlurAmount(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The direction of the blur (in degrees).
    property Angle: Single read GetAngle write SetAngle nodefault;
    // The scale of the blur (as a fraction of the input size).
    property BlurAmount: Single read GetBlurAmount write SetBlurAmount nodefault;
  end;

  //An effect that applies a radial blur to the input.
  TRadialBlurEffect = class(TImageFXEffect)
  private
    function GetCenter: TPointF;
    procedure SetCenter(AValue: TPointF);
    function GetBlurAmount: Single;
    procedure SetBlurAmount(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The center point of the zomming.
    property Center: TPointF read GetCenter write SetCenter;
  published
    // The scale of the blur (as a fraction of the input size).
    property BlurAmount: Single read GetBlurAmount write SetBlurAmount nodefault;
  end;

  //An effect that GaussianBlurs.
  TGaussianBlurEffect = class(TImageFXEffect)
  private
    function GetBlurAmount: Single;
    procedure SetBlurAmount(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The GaussianBlur factor.
    property BlurAmount: Single read GetBlurAmount write SetBlurAmount nodefault;
  end;

  //An effect that blurs.
  TBoxBlurEffect = class(TImageFXEffect)
  private
    function GetBlurAmount: Single;
    procedure SetBlurAmount(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The blur factor.
    property BlurAmount: Single read GetBlurAmount write SetBlurAmount nodefault;
  end;

  //Converts a grayscale image to a white image that is masked by alpha.
  TMaskToAlphaEffect = class(TImageFXEffect)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  //An effect that makes pixels of a particular color transparent.
  TColorKeyAlphaEffect = class(TImageFXEffect)
  private
    function GetColorKey: TAlphaColor;
    procedure SetColorKey(AValue: TAlphaColor);
    function GetTolerance: Single;
    procedure SetTolerance(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The color that becomes transparent.
    property ColorKey: TAlphaColor read GetColorKey write SetColorKey;
    // The tolerance in color differences.
    property Tolerance: Single read GetTolerance write SetTolerance nodefault;
  end;

  //Remaps colors so they fall within shades of a single color.
  TMonochromeEffect = class(TImageFXEffect)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  //An effect that inverts all colors.
  TInvertEffect = class(TImageFXEffect)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  //An effect that intensifies dark regions.
  TGloomEffect = class(TImageFXEffect)
  private
    function GetGloomIntensity: Single;
    procedure SetGloomIntensity(AValue: Single);
    function GetBaseIntensity: Single;
    procedure SetBaseIntensity(AValue: Single);
    function GetGloomSaturation: Single;
    procedure SetGloomSaturation(AValue: Single);
    function GetBaseSaturation: Single;
    procedure SetBaseSaturation(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Intensity of the gloom image.
    property GloomIntensity: Single read GetGloomIntensity write SetGloomIntensity nodefault;
    // Intensity of the base image.
    property BaseIntensity: Single read GetBaseIntensity write SetBaseIntensity nodefault;
    // Saturation of the gloom image.
    property GloomSaturation: Single read GetGloomSaturation write SetGloomSaturation nodefault;
    // Saturation of the base image.
    property BaseSaturation: Single read GetBaseSaturation write SetBaseSaturation nodefault;
  end;

  //Changes the overall hue, or tint, of the source pixels.
  THueAdjustEffect = class(TImageFXEffect)
  private
    function GetHue: Single;
    procedure SetHue(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The hue offset.
    property Hue: Single read GetHue write SetHue nodefault;
  end;

  //An effect that intensifies bright regions.
  TBloomEffect = class(TImageFXEffect)
  private
    function GetBloomIntensity: Single;
    procedure SetBloomIntensity(AValue: Single);
    function GetBaseIntensity: Single;
    procedure SetBaseIntensity(AValue: Single);
    function GetBloomSaturation: Single;
    procedure SetBloomSaturation(AValue: Single);
    function GetBaseSaturation: Single;
    procedure SetBaseSaturation(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Intensity of the bloom image.
    property BloomIntensity: Single read GetBloomIntensity write SetBloomIntensity nodefault;
    // Intensity of the base image.
    property BaseIntensity: Single read GetBaseIntensity write SetBaseIntensity nodefault;
    // Saturation of the bloom image.
    property BloomSaturation: Single read GetBloomSaturation write SetBloomSaturation nodefault;
    // Saturation of the base image.
    property BaseSaturation: Single read GetBaseSaturation write SetBaseSaturation nodefault;
  end;

  //An effect that controls brightness and contrast.
  TContrastEffect = class(TImageFXEffect)
  private
    function GetBrightness: Single;
    procedure SetBrightness(AValue: Single);
    function GetContrast: Single;
    procedure SetContrast(AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The brightness offset.
    property Brightness: Single read GetBrightness write SetBrightness nodefault;
    // The contrast multiplier.
    property Contrast: Single read GetContrast write SetContrast nodefault;
  end;

  //Generates a solid color.
  TFillEffect = class(TImageFXEffect)
  private
    function GetColor: TAlphaColor;
    procedure SetColor(AValue: TAlphaColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The fill color.
    property Color: TAlphaColor read GetColor write SetColor;
  end;

  //Fill all pixels with not empty alpha.
  TFillRGBEffect = class(TImageFXEffect)
  private
    function GetColor: TAlphaColor;
    procedure SetColor(AValue: TAlphaColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The fill color.
    property Color: TAlphaColor read GetColor write SetColor;
  end;

  //Normal blending of two images.
  TNormalBlendEffect = class(TImageFXEffect)
  private
    FTarget: TBitmap;
    procedure TargetChanged(Sender: TObject);
  private
    procedure SetTarget(const AValue: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The target bitmap.
    property Target: TBitmap read FTarget write SetTarget;
  end;

procedure Register;

implementation

uses
  System.UIConsts, System.SysUtils, FMX.Filter.Standard;

constructor TFilterBaseFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilter := TFilterManager.FilterByName(ClassName.Substring(7, 100));
end;

destructor TFilterBaseFilter.Destroy; 
begin
  if FFilter <> nil then
    FFilter.Free;
  inherited Destroy;
end;

function TFilterBaseFilter.GetOutput: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Output']
  else
    Result := nil;
end;

procedure TFilterBaseFilter.SetInput(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Input'] := AValue;
end;

procedure TFilterBaseFilter.SetInputFilter(const AValue: TFilterBaseFilter);
begin
  FInputFilter := AValue;
  if FFilter <> nil then
    if (FInputFilter <> nil) then
      FFilter.InputFilter := FInputFilter.FFilter
    else
      FFilter.InputFilter := nil;
end;

procedure TFilterBaseFilter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited ;
  if (Operation = opRemove) and (AComponent = FInputFilter) then
    InputFilter := nil;
end;

{ TFilterPixelateTransition Implementation }

function TFilterPixelateTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterPixelateTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

procedure TFilterPixelateTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterPixelateTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

{ TFilterBrightTransition Implementation }

function TFilterBrightTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterBrightTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

procedure TFilterBrightTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterBrightTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

{ TFilterSwirlTransition Implementation }

function TFilterSwirlTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterSwirlTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterSwirlTransition.GetStrength: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Strength']
  else
    Result := 0.0;
end;

procedure TFilterSwirlTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterSwirlTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterSwirlTransition.SetStrength(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Strength'] := AValue;
end;

{ TFilterDissolveTransition Implementation }

function TFilterDissolveTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterDissolveTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterDissolveTransition.GetRandomSeed: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;

function TFilterDissolveTransition.GetSecond: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Second']
  else
    Result := nil;
end;

procedure TFilterDissolveTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterDissolveTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterDissolveTransition.SetRandomSeed(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['RandomSeed'] := AValue;
end;

procedure TFilterDissolveTransition.SetSecond(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Second'] := AValue;
end;

{ TFilterBloodTransition Implementation }

function TFilterBloodTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterBloodTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterBloodTransition.GetRandomSeed: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;

function TFilterBloodTransition.GetSecond: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Second']
  else
    Result := nil;
end;

procedure TFilterBloodTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterBloodTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterBloodTransition.SetRandomSeed(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['RandomSeed'] := AValue;
end;

procedure TFilterBloodTransition.SetSecond(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Second'] := AValue;
end;

{ TFilterBlurTransition Implementation }

function TFilterBlurTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterBlurTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

procedure TFilterBlurTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterBlurTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

{ TFilterDropTransition Implementation }

function TFilterDropTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterDropTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterDropTransition.GetRandomSeed: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;

function TFilterDropTransition.GetSecond: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Second']
  else
    Result := nil;
end;

procedure TFilterDropTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterDropTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterDropTransition.SetRandomSeed(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['RandomSeed'] := AValue;
end;

procedure TFilterDropTransition.SetSecond(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Second'] := AValue;
end;

{ TFilterCrumpleTransition Implementation }

function TFilterCrumpleTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterCrumpleTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterCrumpleTransition.GetRandomSeed: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;

function TFilterCrumpleTransition.GetSecond: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Second']
  else
    Result := nil;
end;

procedure TFilterCrumpleTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterCrumpleTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterCrumpleTransition.SetRandomSeed(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['RandomSeed'] := AValue;
end;

procedure TFilterCrumpleTransition.SetSecond(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Second'] := AValue;
end;

{ TFilterWaterTransition Implementation }

function TFilterWaterTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterWaterTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterWaterTransition.GetRandomSeed: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;

function TFilterWaterTransition.GetSecond: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Second']
  else
    Result := nil;
end;

procedure TFilterWaterTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterWaterTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterWaterTransition.SetRandomSeed(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['RandomSeed'] := AValue;
end;

procedure TFilterWaterTransition.SetSecond(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Second'] := AValue;
end;

{ TFilterBandedSwirlTransition Implementation }

function TFilterBandedSwirlTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterBandedSwirlTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterBandedSwirlTransition.GetStrength: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Strength']
  else
    Result := 0.0;
end;

function TFilterBandedSwirlTransition.GetFrequency: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Frequency']
  else
    Result := 0.0;
end;

function TFilterBandedSwirlTransition.GetCenter: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

procedure TFilterBandedSwirlTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterBandedSwirlTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterBandedSwirlTransition.SetStrength(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Strength'] := AValue;
end;

procedure TFilterBandedSwirlTransition.SetFrequency(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Frequency'] := AValue;
end;

procedure TFilterBandedSwirlTransition.SetCenter(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Center'] := AValue;
end;

{ TFilterSwipeTransition Implementation }

function TFilterSwipeTransition.GetMousePoint: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['MousePoint']
  else
    Result := TPointF.Zero;
end;

function TFilterSwipeTransition.GetCornerPoint: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['CornerPoint']
  else
    Result := TPointF.Zero;
end;

function TFilterSwipeTransition.GetDeep: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Deep']
  else
    Result := 0.0;
end;

function TFilterSwipeTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterSwipeTransition.GetBack: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Back']
  else
    Result := nil;
end;

procedure TFilterSwipeTransition.SetMousePoint(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['MousePoint'] := AValue;
end;

procedure TFilterSwipeTransition.SetCornerPoint(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['CornerPoint'] := AValue;
end;

procedure TFilterSwipeTransition.SetDeep(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Deep'] := AValue;
end;

procedure TFilterSwipeTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterSwipeTransition.SetBack(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Back'] := AValue;
end;

{ TFilterSlideTransition Implementation }

function TFilterSlideTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterSlideTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterSlideTransition.GetSlideAmount: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['SlideAmount']
  else
    Result := TPointF.Zero;
end;

procedure TFilterSlideTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterSlideTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterSlideTransition.SetSlideAmount(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['SlideAmount'] := AValue;
end;

{ TFilterMagnifyTransition Implementation }

function TFilterMagnifyTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterMagnifyTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterMagnifyTransition.GetCenter: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

procedure TFilterMagnifyTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterMagnifyTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterMagnifyTransition.SetCenter(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Center'] := AValue;
end;

{ TFilterLineTransition Implementation }

function TFilterLineTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterLineTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterLineTransition.GetOrigin: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Origin']
  else
    Result := TPointF.Zero;
end;

function TFilterLineTransition.GetNormal: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Normal']
  else
    Result := TPointF.Zero;
end;

function TFilterLineTransition.GetOffsetProp: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['OffsetProp']
  else
    Result := TPointF.Zero;
end;

function TFilterLineTransition.GetFuzzyAmount: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['FuzzyAmount']
  else
    Result := 0.0;
end;

procedure TFilterLineTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterLineTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterLineTransition.SetOrigin(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Origin'] := AValue;
end;

procedure TFilterLineTransition.SetNormal(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Normal'] := AValue;
end;

procedure TFilterLineTransition.SetOffsetProp(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['OffsetProp'] := AValue;
end;

procedure TFilterLineTransition.SetFuzzyAmount(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['FuzzyAmount'] := AValue;
end;

{ TFilterRotateCrumpleTransition Implementation }

function TFilterRotateCrumpleTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterRotateCrumpleTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterRotateCrumpleTransition.GetRandomSeed: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;

function TFilterRotateCrumpleTransition.GetSecond: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Second']
  else
    Result := nil;
end;

procedure TFilterRotateCrumpleTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterRotateCrumpleTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterRotateCrumpleTransition.SetRandomSeed(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['RandomSeed'] := AValue;
end;

procedure TFilterRotateCrumpleTransition.SetSecond(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Second'] := AValue;
end;

{ TFilterRippleTransition Implementation }

function TFilterRippleTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterRippleTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

procedure TFilterRippleTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterRippleTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

{ TFilterCircleTransition Implementation }

function TFilterCircleTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterCircleTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterCircleTransition.GetFuzzyAmount: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['FuzzyAmount']
  else
    Result := 0.0;
end;

function TFilterCircleTransition.GetCircleSize: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['CircleSize']
  else
    Result := 0.0;
end;

function TFilterCircleTransition.GetCenter: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

procedure TFilterCircleTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterCircleTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterCircleTransition.SetFuzzyAmount(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['FuzzyAmount'] := AValue;
end;

procedure TFilterCircleTransition.SetCircleSize(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['CircleSize'] := AValue;
end;

procedure TFilterCircleTransition.SetCenter(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Center'] := AValue;
end;

{ TFilterWiggleTransition Implementation }

function TFilterWiggleTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterWiggleTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterWiggleTransition.GetRandomSeed: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;

function TFilterWiggleTransition.GetSecond: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Second']
  else
    Result := nil;
end;

procedure TFilterWiggleTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterWiggleTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterWiggleTransition.SetRandomSeed(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['RandomSeed'] := AValue;
end;

procedure TFilterWiggleTransition.SetSecond(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Second'] := AValue;
end;

{ TFilterSaturateTransition Implementation }

function TFilterSaturateTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterSaturateTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

procedure TFilterSaturateTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterSaturateTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

{ TFilterWaveTransition Implementation }

function TFilterWaveTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterWaveTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

procedure TFilterWaveTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterWaveTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

{ TFilterFadeTransition Implementation }

function TFilterFadeTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterFadeTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

procedure TFilterFadeTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterFadeTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

{ TFilterBlindTransition Implementation }

function TFilterBlindTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterBlindTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterBlindTransition.GetNumberOfBlinds: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['NumberOfBlinds']
  else
    Result := 0.0;
end;

procedure TFilterBlindTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterBlindTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterBlindTransition.SetNumberOfBlinds(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['NumberOfBlinds'] := AValue;
end;

{ TFilterShapeTransition Implementation }

function TFilterShapeTransition.GetProgress: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;

function TFilterShapeTransition.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

function TFilterShapeTransition.GetRandomSeed: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;

function TFilterShapeTransition.GetSecond: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Second']
  else
    Result := nil;
end;

procedure TFilterShapeTransition.SetProgress(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Progress'] := AValue;
end;

procedure TFilterShapeTransition.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;

procedure TFilterShapeTransition.SetRandomSeed(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['RandomSeed'] := AValue;
end;

procedure TFilterShapeTransition.SetSecond(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Second'] := AValue;
end;

{ TFilterPencilStroke Implementation }

function TFilterPencilStroke.GetbrushSize: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['brushSize']
  else
    Result := 0.0;
end;

procedure TFilterPencilStroke.SetbrushSize(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['brushSize'] := AValue;
end;

{ TFilterSharpen Implementation }

function TFilterSharpen.GetAmount: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Amount']
  else
    Result := 0.0;
end;

procedure TFilterSharpen.SetAmount(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Amount'] := AValue;
end;

{ TFilterEmboss Implementation }

function TFilterEmboss.GetAmount: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Amount']
  else
    Result := 0.0;
end;

function TFilterEmboss.GetWidth: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Width']
  else
    Result := 0.0;
end;

procedure TFilterEmboss.SetAmount(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Amount'] := AValue;
end;

procedure TFilterEmboss.SetWidth(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Width'] := AValue;
end;

{ TFilterSepia Implementation }

function TFilterSepia.GetAmount: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Amount']
  else
    Result := 0.0;
end;

procedure TFilterSepia.SetAmount(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Amount'] := AValue;
end;

{ TFilterPaperSketch Implementation }

function TFilterPaperSketch.GetbrushSize: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['brushSize']
  else
    Result := 0.0;
end;

procedure TFilterPaperSketch.SetbrushSize(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['brushSize'] := AValue;
end;

{ TFilterPixelate Implementation }

function TFilterPixelate.GetBlockCount: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BlockCount']
  else
    Result := 0.0;
end;

procedure TFilterPixelate.SetBlockCount(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BlockCount'] := AValue;
end;

{ TFilterToon Implementation }

function TFilterToon.GetLevels: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Levels']
  else
    Result := 0.0;
end;

procedure TFilterToon.SetLevels(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Levels'] := AValue;
end;

{ TFilterWave Implementation }

function TFilterWave.GetTime: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Time']
  else
    Result := 0.0;
end;

function TFilterWave.GetWaveSize: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['WaveSize']
  else
    Result := 0.0;
end;

procedure TFilterWave.SetTime(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Time'] := AValue;
end;

procedure TFilterWave.SetWaveSize(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['WaveSize'] := AValue;
end;

{ TFilterBandedSwirl Implementation }

function TFilterBandedSwirl.GetCenter: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TFilterBandedSwirl.GetBands: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Bands']
  else
    Result := 0.0;
end;

function TFilterBandedSwirl.GetStrength: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Strength']
  else
    Result := 0.0;
end;

function TFilterBandedSwirl.GetAspectRatio: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['AspectRatio']
  else
    Result := 0.0;
end;

procedure TFilterBandedSwirl.SetCenter(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Center'] := AValue;
end;

procedure TFilterBandedSwirl.SetBands(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Bands'] := AValue;
end;

procedure TFilterBandedSwirl.SetStrength(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Strength'] := AValue;
end;

procedure TFilterBandedSwirl.SetAspectRatio(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['AspectRatio'] := AValue;
end;

{ TFilterSmoothMagnify Implementation }

function TFilterSmoothMagnify.GetCenter: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TFilterSmoothMagnify.GetInnerRadius: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['InnerRadius']
  else
    Result := 0.0;
end;

function TFilterSmoothMagnify.GetOuterRadius: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['OuterRadius']
  else
    Result := 0.0;
end;

function TFilterSmoothMagnify.GetMagnification: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Magnification']
  else
    Result := 0.0;
end;

function TFilterSmoothMagnify.GetAspectRatio: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['AspectRatio']
  else
    Result := 0.0;
end;

procedure TFilterSmoothMagnify.SetCenter(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Center'] := AValue;
end;

procedure TFilterSmoothMagnify.SetInnerRadius(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['InnerRadius'] := AValue;
end;

procedure TFilterSmoothMagnify.SetOuterRadius(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['OuterRadius'] := AValue;
end;

procedure TFilterSmoothMagnify.SetMagnification(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Magnification'] := AValue;
end;

procedure TFilterSmoothMagnify.SetAspectRatio(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['AspectRatio'] := AValue;
end;

{ TFilterWrap Implementation }

function TFilterWrap.GetLeftStart: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['LeftStart']
  else
    Result := 0.0;
end;

function TFilterWrap.GetLeftControl1: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['LeftControl1']
  else
    Result := 0.0;
end;

function TFilterWrap.GetLeftControl2: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['LeftControl2']
  else
    Result := 0.0;
end;

function TFilterWrap.GetLeftEnd: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['LeftEnd']
  else
    Result := 0.0;
end;

function TFilterWrap.GetRightStart: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['RightStart']
  else
    Result := 0.0;
end;

function TFilterWrap.GetRightControl1: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['RightControl1']
  else
    Result := 0.0;
end;

function TFilterWrap.GetRightControl2: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['RightControl2']
  else
    Result := 0.0;
end;

function TFilterWrap.GetRightEnd: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['RightEnd']
  else
    Result := 0.0;
end;

procedure TFilterWrap.SetLeftStart(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['LeftStart'] := AValue;
end;

procedure TFilterWrap.SetLeftControl1(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['LeftControl1'] := AValue;
end;

procedure TFilterWrap.SetLeftControl2(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['LeftControl2'] := AValue;
end;

procedure TFilterWrap.SetLeftEnd(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['LeftEnd'] := AValue;
end;

procedure TFilterWrap.SetRightStart(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['RightStart'] := AValue;
end;

procedure TFilterWrap.SetRightControl1(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['RightControl1'] := AValue;
end;

procedure TFilterWrap.SetRightControl2(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['RightControl2'] := AValue;
end;

procedure TFilterWrap.SetRightEnd(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['RightEnd'] := AValue;
end;

{ TFilterSwirl Implementation }

function TFilterSwirl.GetCenter: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TFilterSwirl.GetSpiralStrength: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['SpiralStrength']
  else
    Result := 0.0;
end;

function TFilterSwirl.GetAspectRatio: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['AspectRatio']
  else
    Result := 0.0;
end;

procedure TFilterSwirl.SetCenter(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Center'] := AValue;
end;

procedure TFilterSwirl.SetSpiralStrength(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['SpiralStrength'] := AValue;
end;

procedure TFilterSwirl.SetAspectRatio(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['AspectRatio'] := AValue;
end;

{ TFilterBands Implementation }

function TFilterBands.GetBandDensity: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BandDensity']
  else
    Result := 0.0;
end;

function TFilterBands.GetBandIntensity: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BandIntensity']
  else
    Result := 0.0;
end;

procedure TFilterBands.SetBandDensity(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BandDensity'] := AValue;
end;

procedure TFilterBands.SetBandIntensity(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BandIntensity'] := AValue;
end;

{ TFilterMagnify Implementation }

function TFilterMagnify.GetCenter: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TFilterMagnify.GetRadius: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Radius']
  else
    Result := 0.0;
end;

function TFilterMagnify.GetMagnification: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Magnification']
  else
    Result := 0.0;
end;

function TFilterMagnify.GetAspectRatio: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['AspectRatio']
  else
    Result := 0.0;
end;

procedure TFilterMagnify.SetCenter(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Center'] := AValue;
end;

procedure TFilterMagnify.SetRadius(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Radius'] := AValue;
end;

procedure TFilterMagnify.SetMagnification(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Magnification'] := AValue;
end;

procedure TFilterMagnify.SetAspectRatio(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['AspectRatio'] := AValue;
end;

{ TFilterPinch Implementation }

function TFilterPinch.GetCenter: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TFilterPinch.GetRadius: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Radius']
  else
    Result := 0.0;
end;

function TFilterPinch.GetStrength: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Strength']
  else
    Result := 0.0;
end;

function TFilterPinch.GetAspectRatio: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['AspectRatio']
  else
    Result := 0.0;
end;

procedure TFilterPinch.SetCenter(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Center'] := AValue;
end;

procedure TFilterPinch.SetRadius(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Radius'] := AValue;
end;

procedure TFilterPinch.SetStrength(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Strength'] := AValue;
end;

procedure TFilterPinch.SetAspectRatio(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['AspectRatio'] := AValue;
end;

{ TFilterRipple Implementation }

function TFilterRipple.GetCenter: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TFilterRipple.GetAmplitude: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Amplitude']
  else
    Result := 0.0;
end;

function TFilterRipple.GetFrequency: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Frequency']
  else
    Result := 0.0;
end;

function TFilterRipple.GetPhase: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Phase']
  else
    Result := 0.0;
end;

function TFilterRipple.GetAspectRatio: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['AspectRatio']
  else
    Result := 0.0;
end;

procedure TFilterRipple.SetCenter(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Center'] := AValue;
end;

procedure TFilterRipple.SetAmplitude(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Amplitude'] := AValue;
end;

procedure TFilterRipple.SetFrequency(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Frequency'] := AValue;
end;

procedure TFilterRipple.SetPhase(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Phase'] := AValue;
end;

procedure TFilterRipple.SetAspectRatio(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['AspectRatio'] := AValue;
end;

{ TFilterPerspectiveTransform Implementation }

function TFilterPerspectiveTransform.GetTopLeft: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['TopLeft']
  else
    Result := TPointF.Zero;
end;

function TFilterPerspectiveTransform.GetTopRight: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['TopRight']
  else
    Result := TPointF.Zero;
end;

function TFilterPerspectiveTransform.GetBottomRight: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['BottomRight']
  else
    Result := TPointF.Zero;
end;

function TFilterPerspectiveTransform.GetBottomLeft: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['BottomLeft']
  else
    Result := TPointF.Zero;
end;

procedure TFilterPerspectiveTransform.SetTopLeft(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['TopLeft'] := AValue;
end;

procedure TFilterPerspectiveTransform.SetTopRight(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['TopRight'] := AValue;
end;

procedure TFilterPerspectiveTransform.SetBottomRight(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['BottomRight'] := AValue;
end;

procedure TFilterPerspectiveTransform.SetBottomLeft(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['BottomLeft'] := AValue;
end;

{ TFilterCrop Implementation }

function TFilterCrop.GetLeftTop: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['LeftTop']
  else
    Result := TPointF.Zero;
end;

function TFilterCrop.GetRightBottom: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['RightBottom']
  else
    Result := TPointF.Zero;
end;

procedure TFilterCrop.SetLeftTop(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['LeftTop'] := AValue;
end;

procedure TFilterCrop.SetRightBottom(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['RightBottom'] := AValue;
end;

{ TFilterAffineTransform Implementation }

function TFilterAffineTransform.GetCenter: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TFilterAffineTransform.GetRotation: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Rotation']
  else
    Result := 0.0;
end;

function TFilterAffineTransform.GetScale: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Scale']
  else
    Result := 0.0;
end;

procedure TFilterAffineTransform.SetCenter(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Center'] := AValue;
end;

procedure TFilterAffineTransform.SetRotation(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Rotation'] := AValue;
end;

procedure TFilterAffineTransform.SetScale(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Scale'] := AValue;
end;

{ TFilterTiler Implementation }

function TFilterTiler.GetVerticalTileCount: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['VerticalTileCount']
  else
    Result := 0.0;
end;

function TFilterTiler.GetHorizontalTileCount: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['HorizontalTileCount']
  else
    Result := 0.0;
end;

function TFilterTiler.GetHorizontalOffset: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['HorizontalOffset']
  else
    Result := 0.0;
end;

function TFilterTiler.GetVerticalOffset: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['VerticalOffset']
  else
    Result := 0.0;
end;

procedure TFilterTiler.SetVerticalTileCount(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['VerticalTileCount'] := AValue;
end;

procedure TFilterTiler.SetHorizontalTileCount(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['HorizontalTileCount'] := AValue;
end;

procedure TFilterTiler.SetHorizontalOffset(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['HorizontalOffset'] := AValue;
end;

procedure TFilterTiler.SetVerticalOffset(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['VerticalOffset'] := AValue;
end;

{ TFilterDirectionalBlur Implementation }

function TFilterDirectionalBlur.GetAngle: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Angle']
  else
    Result := 0.0;
end;

function TFilterDirectionalBlur.GetBlurAmount: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BlurAmount']
  else
    Result := 0.0;
end;

procedure TFilterDirectionalBlur.SetAngle(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Angle'] := AValue;
end;

procedure TFilterDirectionalBlur.SetBlurAmount(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BlurAmount'] := AValue;
end;

{ TFilterRadialBlur Implementation }

function TFilterRadialBlur.GetCenter: TPointF;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TFilterRadialBlur.GetBlurAmount: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BlurAmount']
  else
    Result := 0.0;
end;

procedure TFilterRadialBlur.SetCenter(AValue: TPointF);
begin
  if FFilter <> nil then
    FFilter.ValuesAsPoint['Center'] := AValue;
end;

procedure TFilterRadialBlur.SetBlurAmount(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BlurAmount'] := AValue;
end;

{ TFilterGaussianBlur Implementation }

function TFilterGaussianBlur.GetBlurAmount: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BlurAmount']
  else
    Result := 0.0;
end;

procedure TFilterGaussianBlur.SetBlurAmount(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BlurAmount'] := AValue;
end;

{ TFilterBoxBlur Implementation }

function TFilterBoxBlur.GetBlurAmount: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BlurAmount']
  else
    Result := 0.0;
end;

procedure TFilterBoxBlur.SetBlurAmount(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BlurAmount'] := AValue;
end;

{ TFilterMaskToAlpha Implementation }

{ TFilterColorKeyAlpha Implementation }

function TFilterColorKeyAlpha.GetColorKey: TAlphaColor;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsColor['ColorKey']
  else
    Result := claNull;
end;

function TFilterColorKeyAlpha.GetTolerance: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Tolerance']
  else
    Result := 0.0;
end;

procedure TFilterColorKeyAlpha.SetColorKey(AValue: TAlphaColor);
begin
  if FFilter <> nil then
    FFilter.ValuesAsColor['ColorKey'] := AValue;
end;

procedure TFilterColorKeyAlpha.SetTolerance(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Tolerance'] := AValue;
end;

{ TFilterMonochrome Implementation }

{ TFilterInvert Implementation }

{ TFilterGloom Implementation }

function TFilterGloom.GetGloomIntensity: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['GloomIntensity']
  else
    Result := 0.0;
end;

function TFilterGloom.GetBaseIntensity: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BaseIntensity']
  else
    Result := 0.0;
end;

function TFilterGloom.GetGloomSaturation: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['GloomSaturation']
  else
    Result := 0.0;
end;

function TFilterGloom.GetBaseSaturation: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BaseSaturation']
  else
    Result := 0.0;
end;

procedure TFilterGloom.SetGloomIntensity(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['GloomIntensity'] := AValue;
end;

procedure TFilterGloom.SetBaseIntensity(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BaseIntensity'] := AValue;
end;

procedure TFilterGloom.SetGloomSaturation(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['GloomSaturation'] := AValue;
end;

procedure TFilterGloom.SetBaseSaturation(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BaseSaturation'] := AValue;
end;

{ TFilterHueAdjust Implementation }

function TFilterHueAdjust.GetHue: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Hue']
  else
    Result := 0.0;
end;

procedure TFilterHueAdjust.SetHue(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Hue'] := AValue;
end;

{ TFilterBloom Implementation }

function TFilterBloom.GetBloomIntensity: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BloomIntensity']
  else
    Result := 0.0;
end;

function TFilterBloom.GetBaseIntensity: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BaseIntensity']
  else
    Result := 0.0;
end;

function TFilterBloom.GetBloomSaturation: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BloomSaturation']
  else
    Result := 0.0;
end;

function TFilterBloom.GetBaseSaturation: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['BaseSaturation']
  else
    Result := 0.0;
end;

procedure TFilterBloom.SetBloomIntensity(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BloomIntensity'] := AValue;
end;

procedure TFilterBloom.SetBaseIntensity(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BaseIntensity'] := AValue;
end;

procedure TFilterBloom.SetBloomSaturation(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BloomSaturation'] := AValue;
end;

procedure TFilterBloom.SetBaseSaturation(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['BaseSaturation'] := AValue;
end;

{ TFilterContrast Implementation }

function TFilterContrast.GetBrightness: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Brightness']
  else
    Result := 0.0;
end;

function TFilterContrast.GetContrast: Single;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsFloat['Contrast']
  else
    Result := 0.0;
end;

procedure TFilterContrast.SetBrightness(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Brightness'] := AValue;
end;

procedure TFilterContrast.SetContrast(AValue: Single);
begin
  if FFilter <> nil then
    FFilter.ValuesAsFloat['Contrast'] := AValue;
end;

{ TFilterFill Implementation }

function TFilterFill.GetColor: TAlphaColor;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsColor['Color']
  else
    Result := claNull;
end;

procedure TFilterFill.SetColor(AValue: TAlphaColor);
begin
  if FFilter <> nil then
    FFilter.ValuesAsColor['Color'] := AValue;
end;

{ TFilterFillRGB Implementation }

function TFilterFillRGB.GetColor: TAlphaColor;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsColor['Color']
  else
    Result := claNull;
end;

procedure TFilterFillRGB.SetColor(AValue: TAlphaColor);
begin
  if FFilter <> nil then
    FFilter.ValuesAsColor['Color'] := AValue;
end;

{ TFilterNormalBlend Implementation }

function TFilterNormalBlend.GetTarget: TBitmap;
begin
  if FFilter <> nil then
    Result := FFilter.ValuesAsBitmap['Target']
  else
    Result := nil;
end;

procedure TFilterNormalBlend.SetTarget(const AValue: TBitmap);
begin
  if FFilter <> nil then
    FFilter.ValuesAsBitmap['Target'] := AValue;
end;


function TImageFXEffect.CreateFilter: TFilter;
var
  FilterName: string;
begin
  FEffectStyle := [TEffectStyle.DisablePaint];
  FilterName := ClassName.Substring(1, 100);
  Result := TFilterManager.FilterByName(FilterName.Substring(0, FilterName.Length - 6));
end;

{ TPixelateTransitionEffect Implementation }

constructor TPixelateTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TPixelateTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TPixelateTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
procedure TPixelateTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TPixelateTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TPixelateTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

{ TBrightTransitionEffect Implementation }

constructor TBrightTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TBrightTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TBrightTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
procedure TBrightTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBrightTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TBrightTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

{ TSwirlTransitionEffect Implementation }

constructor TSwirlTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TSwirlTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TSwirlTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TSwirlTransitionEffect.GetStrength: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Strength']
  else
    Result := 0.0;
end;
procedure TSwirlTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TSwirlTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TSwirlTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TSwirlTransitionEffect.SetStrength(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Strength'] <> AValue then
  begin
    Filter.ValuesAsFloat['Strength'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TDissolveTransitionEffect Implementation }

constructor TDissolveTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TDissolveTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TDissolveTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TDissolveTransitionEffect.GetRandomSeed: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;
procedure TDissolveTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TDissolveTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TDissolveTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TDissolveTransitionEffect.SetRandomSeed(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['RandomSeed'] <> AValue then
  begin
    Filter.ValuesAsFloat['RandomSeed'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TBloodTransitionEffect Implementation }

constructor TBloodTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TBloodTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TBloodTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TBloodTransitionEffect.GetRandomSeed: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;
procedure TBloodTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBloodTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TBloodTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TBloodTransitionEffect.SetRandomSeed(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['RandomSeed'] <> AValue then
  begin
    Filter.ValuesAsFloat['RandomSeed'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TBlurTransitionEffect Implementation }

constructor TBlurTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TBlurTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TBlurTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
procedure TBlurTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBlurTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TBlurTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

{ TDropTransitionEffect Implementation }

constructor TDropTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TDropTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TDropTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TDropTransitionEffect.GetRandomSeed: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;
procedure TDropTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TDropTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TDropTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TDropTransitionEffect.SetRandomSeed(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['RandomSeed'] <> AValue then
  begin
    Filter.ValuesAsFloat['RandomSeed'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TCrumpleTransitionEffect Implementation }

constructor TCrumpleTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TCrumpleTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TCrumpleTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TCrumpleTransitionEffect.GetRandomSeed: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;
procedure TCrumpleTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TCrumpleTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TCrumpleTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TCrumpleTransitionEffect.SetRandomSeed(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['RandomSeed'] <> AValue then
  begin
    Filter.ValuesAsFloat['RandomSeed'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TWaterTransitionEffect Implementation }

constructor TWaterTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TWaterTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TWaterTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TWaterTransitionEffect.GetRandomSeed: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;
procedure TWaterTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TWaterTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TWaterTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TWaterTransitionEffect.SetRandomSeed(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['RandomSeed'] <> AValue then
  begin
    Filter.ValuesAsFloat['RandomSeed'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TBandedSwirlTransitionEffect Implementation }

constructor TBandedSwirlTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TBandedSwirlTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TBandedSwirlTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TBandedSwirlTransitionEffect.GetStrength: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Strength']
  else
    Result := 0.0;
end;
function TBandedSwirlTransitionEffect.GetFrequency: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Frequency']
  else
    Result := 0.0;
end;
function TBandedSwirlTransitionEffect.GetCenter: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

procedure TBandedSwirlTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBandedSwirlTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TBandedSwirlTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TBandedSwirlTransitionEffect.SetStrength(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Strength'] <> AValue then
  begin
    Filter.ValuesAsFloat['Strength'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBandedSwirlTransitionEffect.SetFrequency(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Frequency'] <> AValue then
  begin
    Filter.ValuesAsFloat['Frequency'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBandedSwirlTransitionEffect.SetCenter(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Center'] <> AValue then
  begin
    Filter.ValuesAsPoint['Center'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TSwipeTransitionEffect Implementation }

constructor TSwipeTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
  FBack := TBitmap.Create(0, 0);
  FBack.OnChange := BackChanged;
end;

destructor TSwipeTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  FreeAndNil(FBack);
  inherited Destroy;
end;

function TSwipeTransitionEffect.GetMousePoint: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['MousePoint']
  else
    Result := TPointF.Zero;
end;

function TSwipeTransitionEffect.GetCornerPoint: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['CornerPoint']
  else
    Result := TPointF.Zero;
end;

function TSwipeTransitionEffect.GetDeep: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Deep']
  else
    Result := 0.0;
end;
procedure TSwipeTransitionEffect.SetMousePoint(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['MousePoint'] <> AValue then
  begin
    Filter.ValuesAsPoint['MousePoint'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TSwipeTransitionEffect.SetCornerPoint(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['CornerPoint'] <> AValue then
  begin
    Filter.ValuesAsPoint['CornerPoint'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TSwipeTransitionEffect.SetDeep(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Deep'] <> AValue then
  begin
    Filter.ValuesAsFloat['Deep'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TSwipeTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TSwipeTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TSwipeTransitionEffect.BackChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Back'] := FBack;
  UpdateParentEffects;
end;

procedure TSwipeTransitionEffect.SetBack(const AValue: TBitmap);
begin
  FBack.Assign(AValue);
end;

{ TSlideTransitionEffect Implementation }

constructor TSlideTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TSlideTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TSlideTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TSlideTransitionEffect.GetSlideAmount: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['SlideAmount']
  else
    Result := TPointF.Zero;
end;

procedure TSlideTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TSlideTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TSlideTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TSlideTransitionEffect.SetSlideAmount(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['SlideAmount'] <> AValue then
  begin
    Filter.ValuesAsPoint['SlideAmount'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TMagnifyTransitionEffect Implementation }

constructor TMagnifyTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TMagnifyTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TMagnifyTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TMagnifyTransitionEffect.GetCenter: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

procedure TMagnifyTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TMagnifyTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TMagnifyTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TMagnifyTransitionEffect.SetCenter(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Center'] <> AValue then
  begin
    Filter.ValuesAsPoint['Center'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TLineTransitionEffect Implementation }

constructor TLineTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TLineTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TLineTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TLineTransitionEffect.GetOrigin: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Origin']
  else
    Result := TPointF.Zero;
end;

function TLineTransitionEffect.GetNormal: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Normal']
  else
    Result := TPointF.Zero;
end;

function TLineTransitionEffect.GetOffsetProp: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['OffsetProp']
  else
    Result := TPointF.Zero;
end;

function TLineTransitionEffect.GetFuzzyAmount: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['FuzzyAmount']
  else
    Result := 0.0;
end;
procedure TLineTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TLineTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TLineTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TLineTransitionEffect.SetOrigin(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Origin'] <> AValue then
  begin
    Filter.ValuesAsPoint['Origin'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TLineTransitionEffect.SetNormal(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Normal'] <> AValue then
  begin
    Filter.ValuesAsPoint['Normal'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TLineTransitionEffect.SetOffsetProp(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['OffsetProp'] <> AValue then
  begin
    Filter.ValuesAsPoint['OffsetProp'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TLineTransitionEffect.SetFuzzyAmount(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['FuzzyAmount'] <> AValue then
  begin
    Filter.ValuesAsFloat['FuzzyAmount'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TRotateCrumpleTransitionEffect Implementation }

constructor TRotateCrumpleTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TRotateCrumpleTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TRotateCrumpleTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TRotateCrumpleTransitionEffect.GetRandomSeed: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;
procedure TRotateCrumpleTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TRotateCrumpleTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TRotateCrumpleTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TRotateCrumpleTransitionEffect.SetRandomSeed(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['RandomSeed'] <> AValue then
  begin
    Filter.ValuesAsFloat['RandomSeed'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TRippleTransitionEffect Implementation }

constructor TRippleTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TRippleTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TRippleTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
procedure TRippleTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TRippleTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TRippleTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

{ TCircleTransitionEffect Implementation }

constructor TCircleTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TCircleTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TCircleTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TCircleTransitionEffect.GetFuzzyAmount: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['FuzzyAmount']
  else
    Result := 0.0;
end;
function TCircleTransitionEffect.GetCircleSize: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['CircleSize']
  else
    Result := 0.0;
end;
function TCircleTransitionEffect.GetCenter: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

procedure TCircleTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TCircleTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TCircleTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TCircleTransitionEffect.SetFuzzyAmount(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['FuzzyAmount'] <> AValue then
  begin
    Filter.ValuesAsFloat['FuzzyAmount'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TCircleTransitionEffect.SetCircleSize(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['CircleSize'] <> AValue then
  begin
    Filter.ValuesAsFloat['CircleSize'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TCircleTransitionEffect.SetCenter(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Center'] <> AValue then
  begin
    Filter.ValuesAsPoint['Center'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TWiggleTransitionEffect Implementation }

constructor TWiggleTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TWiggleTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TWiggleTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TWiggleTransitionEffect.GetRandomSeed: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;
procedure TWiggleTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TWiggleTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TWiggleTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TWiggleTransitionEffect.SetRandomSeed(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['RandomSeed'] <> AValue then
  begin
    Filter.ValuesAsFloat['RandomSeed'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TSaturateTransitionEffect Implementation }

constructor TSaturateTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TSaturateTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TSaturateTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
procedure TSaturateTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TSaturateTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TSaturateTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

{ TWaveTransitionEffect Implementation }

constructor TWaveTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TWaveTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TWaveTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
procedure TWaveTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TWaveTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TWaveTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

{ TFadeTransitionEffect Implementation }

constructor TFadeTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TFadeTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TFadeTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
procedure TFadeTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TFadeTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TFadeTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

{ TBlindTransitionEffect Implementation }

constructor TBlindTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TBlindTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TBlindTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TBlindTransitionEffect.GetNumberOfBlinds: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['NumberOfBlinds']
  else
    Result := 0.0;
end;
procedure TBlindTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBlindTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TBlindTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TBlindTransitionEffect.SetNumberOfBlinds(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['NumberOfBlinds'] <> AValue then
  begin
    Filter.ValuesAsFloat['NumberOfBlinds'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TShapeTransitionEffect Implementation }

constructor TShapeTransitionEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TShapeTransitionEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

function TShapeTransitionEffect.GetProgress: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Progress']
  else
    Result := 0.0;
end;
function TShapeTransitionEffect.GetRandomSeed: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['RandomSeed']
  else
    Result := 0.0;
end;
procedure TShapeTransitionEffect.SetProgress(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Progress'] <> AValue then
  begin
    Filter.ValuesAsFloat['Progress'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TShapeTransitionEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TShapeTransitionEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;

procedure TShapeTransitionEffect.SetRandomSeed(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['RandomSeed'] <> AValue then
  begin
    Filter.ValuesAsFloat['RandomSeed'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TPencilStrokeEffect Implementation }

constructor TPencilStrokeEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TPencilStrokeEffect.Destroy; 
begin
  inherited Destroy;
end;

function TPencilStrokeEffect.GetbrushSize: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['brushSize']
  else
    Result := 0.0;
end;
procedure TPencilStrokeEffect.SetbrushSize(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['brushSize'] <> AValue then
  begin
    Filter.ValuesAsFloat['brushSize'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TSharpenEffect Implementation }

constructor TSharpenEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TSharpenEffect.Destroy; 
begin
  inherited Destroy;
end;

function TSharpenEffect.GetAmount: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Amount']
  else
    Result := 0.0;
end;
procedure TSharpenEffect.SetAmount(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Amount'] <> AValue then
  begin
    Filter.ValuesAsFloat['Amount'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TEmbossEffect Implementation }

constructor TEmbossEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TEmbossEffect.Destroy; 
begin
  inherited Destroy;
end;

function TEmbossEffect.GetAmount: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Amount']
  else
    Result := 0.0;
end;
function TEmbossEffect.GetWidth: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Width']
  else
    Result := 0.0;
end;
procedure TEmbossEffect.SetAmount(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Amount'] <> AValue then
  begin
    Filter.ValuesAsFloat['Amount'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TEmbossEffect.SetWidth(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Width'] <> AValue then
  begin
    Filter.ValuesAsFloat['Width'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TSepiaEffect Implementation }

constructor TSepiaEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TSepiaEffect.Destroy; 
begin
  inherited Destroy;
end;

function TSepiaEffect.GetAmount: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Amount']
  else
    Result := 0.0;
end;
procedure TSepiaEffect.SetAmount(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Amount'] <> AValue then
  begin
    Filter.ValuesAsFloat['Amount'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TPaperSketchEffect Implementation }

constructor TPaperSketchEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TPaperSketchEffect.Destroy; 
begin
  inherited Destroy;
end;

function TPaperSketchEffect.GetbrushSize: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['brushSize']
  else
    Result := 0.0;
end;
procedure TPaperSketchEffect.SetbrushSize(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['brushSize'] <> AValue then
  begin
    Filter.ValuesAsFloat['brushSize'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TPixelateEffect Implementation }

constructor TPixelateEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TPixelateEffect.Destroy; 
begin
  inherited Destroy;
end;

function TPixelateEffect.GetBlockCount: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BlockCount']
  else
    Result := 0.0;
end;
procedure TPixelateEffect.SetBlockCount(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BlockCount'] <> AValue then
  begin
    Filter.ValuesAsFloat['BlockCount'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TToonEffect Implementation }

constructor TToonEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TToonEffect.Destroy; 
begin
  inherited Destroy;
end;

function TToonEffect.GetLevels: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Levels']
  else
    Result := 0.0;
end;
procedure TToonEffect.SetLevels(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Levels'] <> AValue then
  begin
    Filter.ValuesAsFloat['Levels'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TWaveEffect Implementation }

constructor TWaveEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TWaveEffect.Destroy; 
begin
  inherited Destroy;
end;

function TWaveEffect.GetTime: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Time']
  else
    Result := 0.0;
end;
function TWaveEffect.GetWaveSize: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['WaveSize']
  else
    Result := 0.0;
end;
procedure TWaveEffect.SetTime(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Time'] <> AValue then
  begin
    Filter.ValuesAsFloat['Time'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TWaveEffect.SetWaveSize(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['WaveSize'] <> AValue then
  begin
    Filter.ValuesAsFloat['WaveSize'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TBandedSwirlEffect Implementation }

constructor TBandedSwirlEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TBandedSwirlEffect.Destroy; 
begin
  inherited Destroy;
end;

function TBandedSwirlEffect.GetCenter: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TBandedSwirlEffect.GetBands: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Bands']
  else
    Result := 0.0;
end;
function TBandedSwirlEffect.GetStrength: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Strength']
  else
    Result := 0.0;
end;
function TBandedSwirlEffect.GetAspectRatio: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['AspectRatio']
  else
    Result := 0.0;
end;
procedure TBandedSwirlEffect.SetCenter(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Center'] <> AValue then
  begin
    Filter.ValuesAsPoint['Center'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBandedSwirlEffect.SetBands(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Bands'] <> AValue then
  begin
    Filter.ValuesAsFloat['Bands'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBandedSwirlEffect.SetStrength(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Strength'] <> AValue then
  begin
    Filter.ValuesAsFloat['Strength'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBandedSwirlEffect.SetAspectRatio(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['AspectRatio'] <> AValue then
  begin
    Filter.ValuesAsFloat['AspectRatio'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TSmoothMagnifyEffect Implementation }

constructor TSmoothMagnifyEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TSmoothMagnifyEffect.Destroy; 
begin
  inherited Destroy;
end;

function TSmoothMagnifyEffect.GetCenter: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TSmoothMagnifyEffect.GetInnerRadius: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['InnerRadius']
  else
    Result := 0.0;
end;
function TSmoothMagnifyEffect.GetOuterRadius: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['OuterRadius']
  else
    Result := 0.0;
end;
function TSmoothMagnifyEffect.GetMagnification: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Magnification']
  else
    Result := 0.0;
end;
function TSmoothMagnifyEffect.GetAspectRatio: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['AspectRatio']
  else
    Result := 0.0;
end;
procedure TSmoothMagnifyEffect.SetCenter(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Center'] <> AValue then
  begin
    Filter.ValuesAsPoint['Center'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TSmoothMagnifyEffect.SetInnerRadius(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['InnerRadius'] <> AValue then
  begin
    Filter.ValuesAsFloat['InnerRadius'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TSmoothMagnifyEffect.SetOuterRadius(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['OuterRadius'] <> AValue then
  begin
    Filter.ValuesAsFloat['OuterRadius'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TSmoothMagnifyEffect.SetMagnification(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Magnification'] <> AValue then
  begin
    Filter.ValuesAsFloat['Magnification'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TSmoothMagnifyEffect.SetAspectRatio(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['AspectRatio'] <> AValue then
  begin
    Filter.ValuesAsFloat['AspectRatio'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TWrapEffect Implementation }

constructor TWrapEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TWrapEffect.Destroy; 
begin
  inherited Destroy;
end;

function TWrapEffect.GetLeftStart: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['LeftStart']
  else
    Result := 0.0;
end;
function TWrapEffect.GetLeftControl1: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['LeftControl1']
  else
    Result := 0.0;
end;
function TWrapEffect.GetLeftControl2: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['LeftControl2']
  else
    Result := 0.0;
end;
function TWrapEffect.GetLeftEnd: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['LeftEnd']
  else
    Result := 0.0;
end;
function TWrapEffect.GetRightStart: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['RightStart']
  else
    Result := 0.0;
end;
function TWrapEffect.GetRightControl1: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['RightControl1']
  else
    Result := 0.0;
end;
function TWrapEffect.GetRightControl2: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['RightControl2']
  else
    Result := 0.0;
end;
function TWrapEffect.GetRightEnd: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['RightEnd']
  else
    Result := 0.0;
end;
procedure TWrapEffect.SetLeftStart(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['LeftStart'] <> AValue then
  begin
    Filter.ValuesAsFloat['LeftStart'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TWrapEffect.SetLeftControl1(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['LeftControl1'] <> AValue then
  begin
    Filter.ValuesAsFloat['LeftControl1'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TWrapEffect.SetLeftControl2(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['LeftControl2'] <> AValue then
  begin
    Filter.ValuesAsFloat['LeftControl2'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TWrapEffect.SetLeftEnd(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['LeftEnd'] <> AValue then
  begin
    Filter.ValuesAsFloat['LeftEnd'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TWrapEffect.SetRightStart(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['RightStart'] <> AValue then
  begin
    Filter.ValuesAsFloat['RightStart'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TWrapEffect.SetRightControl1(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['RightControl1'] <> AValue then
  begin
    Filter.ValuesAsFloat['RightControl1'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TWrapEffect.SetRightControl2(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['RightControl2'] <> AValue then
  begin
    Filter.ValuesAsFloat['RightControl2'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TWrapEffect.SetRightEnd(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['RightEnd'] <> AValue then
  begin
    Filter.ValuesAsFloat['RightEnd'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TSwirlEffect Implementation }

constructor TSwirlEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TSwirlEffect.Destroy; 
begin
  inherited Destroy;
end;

function TSwirlEffect.GetCenter: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TSwirlEffect.GetSpiralStrength: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['SpiralStrength']
  else
    Result := 0.0;
end;
function TSwirlEffect.GetAspectRatio: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['AspectRatio']
  else
    Result := 0.0;
end;
procedure TSwirlEffect.SetCenter(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Center'] <> AValue then
  begin
    Filter.ValuesAsPoint['Center'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TSwirlEffect.SetSpiralStrength(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['SpiralStrength'] <> AValue then
  begin
    Filter.ValuesAsFloat['SpiralStrength'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TSwirlEffect.SetAspectRatio(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['AspectRatio'] <> AValue then
  begin
    Filter.ValuesAsFloat['AspectRatio'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TBandsEffect Implementation }

constructor TBandsEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TBandsEffect.Destroy; 
begin
  inherited Destroy;
end;

function TBandsEffect.GetBandDensity: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BandDensity']
  else
    Result := 0.0;
end;
function TBandsEffect.GetBandIntensity: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BandIntensity']
  else
    Result := 0.0;
end;
procedure TBandsEffect.SetBandDensity(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BandDensity'] <> AValue then
  begin
    Filter.ValuesAsFloat['BandDensity'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBandsEffect.SetBandIntensity(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BandIntensity'] <> AValue then
  begin
    Filter.ValuesAsFloat['BandIntensity'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TMagnifyEffect Implementation }

constructor TMagnifyEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TMagnifyEffect.Destroy; 
begin
  inherited Destroy;
end;

function TMagnifyEffect.GetCenter: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TMagnifyEffect.GetRadius: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Radius']
  else
    Result := 0.0;
end;
function TMagnifyEffect.GetMagnification: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Magnification']
  else
    Result := 0.0;
end;
function TMagnifyEffect.GetAspectRatio: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['AspectRatio']
  else
    Result := 0.0;
end;
procedure TMagnifyEffect.SetCenter(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Center'] <> AValue then
  begin
    Filter.ValuesAsPoint['Center'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TMagnifyEffect.SetRadius(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Radius'] <> AValue then
  begin
    Filter.ValuesAsFloat['Radius'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TMagnifyEffect.SetMagnification(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Magnification'] <> AValue then
  begin
    Filter.ValuesAsFloat['Magnification'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TMagnifyEffect.SetAspectRatio(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['AspectRatio'] <> AValue then
  begin
    Filter.ValuesAsFloat['AspectRatio'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TPinchEffect Implementation }

constructor TPinchEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TPinchEffect.Destroy; 
begin
  inherited Destroy;
end;

function TPinchEffect.GetCenter: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TPinchEffect.GetRadius: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Radius']
  else
    Result := 0.0;
end;
function TPinchEffect.GetStrength: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Strength']
  else
    Result := 0.0;
end;
function TPinchEffect.GetAspectRatio: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['AspectRatio']
  else
    Result := 0.0;
end;
procedure TPinchEffect.SetCenter(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Center'] <> AValue then
  begin
    Filter.ValuesAsPoint['Center'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TPinchEffect.SetRadius(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Radius'] <> AValue then
  begin
    Filter.ValuesAsFloat['Radius'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TPinchEffect.SetStrength(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Strength'] <> AValue then
  begin
    Filter.ValuesAsFloat['Strength'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TPinchEffect.SetAspectRatio(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['AspectRatio'] <> AValue then
  begin
    Filter.ValuesAsFloat['AspectRatio'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TRippleEffect Implementation }

constructor TRippleEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TRippleEffect.Destroy; 
begin
  inherited Destroy;
end;

function TRippleEffect.GetCenter: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TRippleEffect.GetAmplitude: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Amplitude']
  else
    Result := 0.0;
end;
function TRippleEffect.GetFrequency: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Frequency']
  else
    Result := 0.0;
end;
function TRippleEffect.GetPhase: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Phase']
  else
    Result := 0.0;
end;
function TRippleEffect.GetAspectRatio: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['AspectRatio']
  else
    Result := 0.0;
end;
procedure TRippleEffect.SetCenter(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Center'] <> AValue then
  begin
    Filter.ValuesAsPoint['Center'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TRippleEffect.SetAmplitude(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Amplitude'] <> AValue then
  begin
    Filter.ValuesAsFloat['Amplitude'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TRippleEffect.SetFrequency(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Frequency'] <> AValue then
  begin
    Filter.ValuesAsFloat['Frequency'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TRippleEffect.SetPhase(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Phase'] <> AValue then
  begin
    Filter.ValuesAsFloat['Phase'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TRippleEffect.SetAspectRatio(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['AspectRatio'] <> AValue then
  begin
    Filter.ValuesAsFloat['AspectRatio'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TPerspectiveTransformEffect Implementation }

constructor TPerspectiveTransformEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TPerspectiveTransformEffect.Destroy; 
begin
  inherited Destroy;
end;

function TPerspectiveTransformEffect.GetTopLeft: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['TopLeft']
  else
    Result := TPointF.Zero;
end;

function TPerspectiveTransformEffect.GetTopRight: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['TopRight']
  else
    Result := TPointF.Zero;
end;

function TPerspectiveTransformEffect.GetBottomRight: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['BottomRight']
  else
    Result := TPointF.Zero;
end;

function TPerspectiveTransformEffect.GetBottomLeft: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['BottomLeft']
  else
    Result := TPointF.Zero;
end;

procedure TPerspectiveTransformEffect.SetTopLeft(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['TopLeft'] <> AValue then
  begin
    Filter.ValuesAsPoint['TopLeft'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TPerspectiveTransformEffect.SetTopRight(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['TopRight'] <> AValue then
  begin
    Filter.ValuesAsPoint['TopRight'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TPerspectiveTransformEffect.SetBottomRight(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['BottomRight'] <> AValue then
  begin
    Filter.ValuesAsPoint['BottomRight'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TPerspectiveTransformEffect.SetBottomLeft(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['BottomLeft'] <> AValue then
  begin
    Filter.ValuesAsPoint['BottomLeft'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TCropEffect Implementation }

constructor TCropEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCropEffect.Destroy; 
begin
  inherited Destroy;
end;

function TCropEffect.GetLeftTop: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['LeftTop']
  else
    Result := TPointF.Zero;
end;

function TCropEffect.GetRightBottom: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['RightBottom']
  else
    Result := TPointF.Zero;
end;

procedure TCropEffect.SetLeftTop(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['LeftTop'] <> AValue then
  begin
    Filter.ValuesAsPoint['LeftTop'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TCropEffect.SetRightBottom(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['RightBottom'] <> AValue then
  begin
    Filter.ValuesAsPoint['RightBottom'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TAffineTransformEffect Implementation }

constructor TAffineTransformEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TAffineTransformEffect.Destroy; 
begin
  inherited Destroy;
end;

function TAffineTransformEffect.GetCenter: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TAffineTransformEffect.GetRotation: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Rotation']
  else
    Result := 0.0;
end;
function TAffineTransformEffect.GetScale: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Scale']
  else
    Result := 0.0;
end;
procedure TAffineTransformEffect.SetCenter(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Center'] <> AValue then
  begin
    Filter.ValuesAsPoint['Center'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TAffineTransformEffect.SetRotation(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Rotation'] <> AValue then
  begin
    Filter.ValuesAsFloat['Rotation'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TAffineTransformEffect.SetScale(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Scale'] <> AValue then
  begin
    Filter.ValuesAsFloat['Scale'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TTilerEffect Implementation }

constructor TTilerEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TTilerEffect.Destroy; 
begin
  inherited Destroy;
end;

function TTilerEffect.GetVerticalTileCount: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['VerticalTileCount']
  else
    Result := 0.0;
end;
function TTilerEffect.GetHorizontalTileCount: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['HorizontalTileCount']
  else
    Result := 0.0;
end;
function TTilerEffect.GetHorizontalOffset: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['HorizontalOffset']
  else
    Result := 0.0;
end;
function TTilerEffect.GetVerticalOffset: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['VerticalOffset']
  else
    Result := 0.0;
end;
procedure TTilerEffect.SetVerticalTileCount(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['VerticalTileCount'] <> AValue then
  begin
    Filter.ValuesAsFloat['VerticalTileCount'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TTilerEffect.SetHorizontalTileCount(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['HorizontalTileCount'] <> AValue then
  begin
    Filter.ValuesAsFloat['HorizontalTileCount'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TTilerEffect.SetHorizontalOffset(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['HorizontalOffset'] <> AValue then
  begin
    Filter.ValuesAsFloat['HorizontalOffset'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TTilerEffect.SetVerticalOffset(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['VerticalOffset'] <> AValue then
  begin
    Filter.ValuesAsFloat['VerticalOffset'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TDirectionalBlurEffect Implementation }

constructor TDirectionalBlurEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDirectionalBlurEffect.Destroy; 
begin
  inherited Destroy;
end;

function TDirectionalBlurEffect.GetAngle: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Angle']
  else
    Result := 0.0;
end;
function TDirectionalBlurEffect.GetBlurAmount: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BlurAmount']
  else
    Result := 0.0;
end;
procedure TDirectionalBlurEffect.SetAngle(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Angle'] <> AValue then
  begin
    Filter.ValuesAsFloat['Angle'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TDirectionalBlurEffect.SetBlurAmount(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BlurAmount'] <> AValue then
  begin
    Filter.ValuesAsFloat['BlurAmount'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TRadialBlurEffect Implementation }

constructor TRadialBlurEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TRadialBlurEffect.Destroy; 
begin
  inherited Destroy;
end;

function TRadialBlurEffect.GetCenter: TPointF;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsPoint['Center']
  else
    Result := TPointF.Zero;
end;

function TRadialBlurEffect.GetBlurAmount: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BlurAmount']
  else
    Result := 0.0;
end;
procedure TRadialBlurEffect.SetCenter(AValue: TPointF);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsPoint['Center'] <> AValue then
  begin
    Filter.ValuesAsPoint['Center'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TRadialBlurEffect.SetBlurAmount(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BlurAmount'] <> AValue then
  begin
    Filter.ValuesAsFloat['BlurAmount'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TGaussianBlurEffect Implementation }

constructor TGaussianBlurEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TGaussianBlurEffect.Destroy; 
begin
  inherited Destroy;
end;

function TGaussianBlurEffect.GetBlurAmount: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BlurAmount']
  else
    Result := 0.0;
end;
procedure TGaussianBlurEffect.SetBlurAmount(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BlurAmount'] <> AValue then
  begin
    Filter.ValuesAsFloat['BlurAmount'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TBoxBlurEffect Implementation }

constructor TBoxBlurEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TBoxBlurEffect.Destroy; 
begin
  inherited Destroy;
end;

function TBoxBlurEffect.GetBlurAmount: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BlurAmount']
  else
    Result := 0.0;
end;
procedure TBoxBlurEffect.SetBlurAmount(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BlurAmount'] <> AValue then
  begin
    Filter.ValuesAsFloat['BlurAmount'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TMaskToAlphaEffect Implementation }

constructor TMaskToAlphaEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TMaskToAlphaEffect.Destroy; 
begin
  inherited Destroy;
end;

{ TColorKeyAlphaEffect Implementation }

constructor TColorKeyAlphaEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TColorKeyAlphaEffect.Destroy; 
begin
  inherited Destroy;
end;

function TColorKeyAlphaEffect.GetColorKey: TAlphaColor;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsColor['ColorKey']
  else
    Result := claNull;
end;

function TColorKeyAlphaEffect.GetTolerance: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Tolerance']
  else
    Result := 0.0;
end;
procedure TColorKeyAlphaEffect.SetColorKey(AValue: TAlphaColor);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsColor['ColorKey'] <> AValue then
  begin
    Filter.ValuesAsColor['ColorKey'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TColorKeyAlphaEffect.SetTolerance(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Tolerance'] <> AValue then
  begin
    Filter.ValuesAsFloat['Tolerance'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TMonochromeEffect Implementation }

constructor TMonochromeEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TMonochromeEffect.Destroy; 
begin
  inherited Destroy;
end;

{ TInvertEffect Implementation }

constructor TInvertEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TInvertEffect.Destroy; 
begin
  inherited Destroy;
end;

{ TGloomEffect Implementation }

constructor TGloomEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TGloomEffect.Destroy; 
begin
  inherited Destroy;
end;

function TGloomEffect.GetGloomIntensity: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['GloomIntensity']
  else
    Result := 0.0;
end;
function TGloomEffect.GetBaseIntensity: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BaseIntensity']
  else
    Result := 0.0;
end;
function TGloomEffect.GetGloomSaturation: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['GloomSaturation']
  else
    Result := 0.0;
end;
function TGloomEffect.GetBaseSaturation: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BaseSaturation']
  else
    Result := 0.0;
end;
procedure TGloomEffect.SetGloomIntensity(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['GloomIntensity'] <> AValue then
  begin
    Filter.ValuesAsFloat['GloomIntensity'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TGloomEffect.SetBaseIntensity(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BaseIntensity'] <> AValue then
  begin
    Filter.ValuesAsFloat['BaseIntensity'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TGloomEffect.SetGloomSaturation(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['GloomSaturation'] <> AValue then
  begin
    Filter.ValuesAsFloat['GloomSaturation'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TGloomEffect.SetBaseSaturation(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BaseSaturation'] <> AValue then
  begin
    Filter.ValuesAsFloat['BaseSaturation'] := AValue;
    UpdateParentEffects;
  end;
end;

{ THueAdjustEffect Implementation }

constructor THueAdjustEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor THueAdjustEffect.Destroy; 
begin
  inherited Destroy;
end;

function THueAdjustEffect.GetHue: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Hue']
  else
    Result := 0.0;
end;
procedure THueAdjustEffect.SetHue(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Hue'] <> AValue then
  begin
    Filter.ValuesAsFloat['Hue'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TBloomEffect Implementation }

constructor TBloomEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TBloomEffect.Destroy; 
begin
  inherited Destroy;
end;

function TBloomEffect.GetBloomIntensity: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BloomIntensity']
  else
    Result := 0.0;
end;
function TBloomEffect.GetBaseIntensity: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BaseIntensity']
  else
    Result := 0.0;
end;
function TBloomEffect.GetBloomSaturation: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BloomSaturation']
  else
    Result := 0.0;
end;
function TBloomEffect.GetBaseSaturation: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['BaseSaturation']
  else
    Result := 0.0;
end;
procedure TBloomEffect.SetBloomIntensity(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BloomIntensity'] <> AValue then
  begin
    Filter.ValuesAsFloat['BloomIntensity'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBloomEffect.SetBaseIntensity(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BaseIntensity'] <> AValue then
  begin
    Filter.ValuesAsFloat['BaseIntensity'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBloomEffect.SetBloomSaturation(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BloomSaturation'] <> AValue then
  begin
    Filter.ValuesAsFloat['BloomSaturation'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TBloomEffect.SetBaseSaturation(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['BaseSaturation'] <> AValue then
  begin
    Filter.ValuesAsFloat['BaseSaturation'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TContrastEffect Implementation }

constructor TContrastEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TContrastEffect.Destroy; 
begin
  inherited Destroy;
end;

function TContrastEffect.GetBrightness: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Brightness']
  else
    Result := 0.0;
end;
function TContrastEffect.GetContrast: Single;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsFloat['Contrast']
  else
    Result := 0.0;
end;
procedure TContrastEffect.SetBrightness(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Brightness'] <> AValue then
  begin
    Filter.ValuesAsFloat['Brightness'] := AValue;
    UpdateParentEffects;
  end;
end;

procedure TContrastEffect.SetContrast(AValue: Single);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsFloat['Contrast'] <> AValue then
  begin
    Filter.ValuesAsFloat['Contrast'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TFillEffect Implementation }

constructor TFillEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TFillEffect.Destroy; 
begin
  inherited Destroy;
end;

function TFillEffect.GetColor: TAlphaColor;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsColor['Color']
  else
    Result := claNull;
end;

procedure TFillEffect.SetColor(AValue: TAlphaColor);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsColor['Color'] <> AValue then
  begin
    Filter.ValuesAsColor['Color'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TFillRGBEffect Implementation }

constructor TFillRGBEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TFillRGBEffect.Destroy; 
begin
  inherited Destroy;
end;

function TFillRGBEffect.GetColor: TAlphaColor;
begin
  if Assigned(Filter) then
    Result := Filter.ValuesAsColor['Color']
  else
    Result := claNull;
end;

procedure TFillRGBEffect.SetColor(AValue: TAlphaColor);
begin
  if not Assigned(Filter) then Exit;
  if Filter.ValuesAsColor['Color'] <> AValue then
  begin
    Filter.ValuesAsColor['Color'] := AValue;
    UpdateParentEffects;
  end;
end;

{ TNormalBlendEffect Implementation }

constructor TNormalBlendEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := TBitmap.Create(0, 0);
  FTarget.OnChange := TargetChanged;
end;

destructor TNormalBlendEffect.Destroy; 
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

procedure TNormalBlendEffect.TargetChanged(Sender: TObject);
begin
  if not Assigned(Filter) then Exit;
  Filter.ValuesAsBitmap['Target'] := FTarget;
  UpdateParentEffects;
end;

procedure TNormalBlendEffect.SetTarget(const AValue: TBitmap);
begin
  FTarget.Assign(AValue);
end;


procedure Register;
begin
  RegisterNoIcon([TPixelateTransitionEffect]);
  RegisterNoIcon([TBrightTransitionEffect]);
  RegisterNoIcon([TSwirlTransitionEffect]);
  RegisterNoIcon([TDissolveTransitionEffect]);
  RegisterNoIcon([TBloodTransitionEffect]);
  RegisterNoIcon([TBlurTransitionEffect]);
  RegisterNoIcon([TDropTransitionEffect]);
  RegisterNoIcon([TCrumpleTransitionEffect]);
  RegisterNoIcon([TWaterTransitionEffect]);
  RegisterNoIcon([TBandedSwirlTransitionEffect]);
  RegisterNoIcon([TSwipeTransitionEffect]);
  RegisterNoIcon([TSlideTransitionEffect]);
  RegisterNoIcon([TMagnifyTransitionEffect]);
  RegisterNoIcon([TLineTransitionEffect]);
  RegisterNoIcon([TRotateCrumpleTransitionEffect]);
  RegisterNoIcon([TRippleTransitionEffect]);
  RegisterNoIcon([TCircleTransitionEffect]);
  RegisterNoIcon([TWiggleTransitionEffect]);
  RegisterNoIcon([TSaturateTransitionEffect]);
  RegisterNoIcon([TWaveTransitionEffect]);
  RegisterNoIcon([TFadeTransitionEffect]);
  RegisterNoIcon([TBlindTransitionEffect]);
  RegisterNoIcon([TShapeTransitionEffect]);
  RegisterNoIcon([TPencilStrokeEffect]);
  RegisterNoIcon([TSharpenEffect]);
  RegisterNoIcon([TEmbossEffect]);
  RegisterNoIcon([TSepiaEffect]);
  RegisterNoIcon([TPaperSketchEffect]);
  RegisterNoIcon([TPixelateEffect]);
  RegisterNoIcon([TToonEffect]);
  RegisterNoIcon([TWaveEffect]);
  RegisterNoIcon([TBandedSwirlEffect]);
  RegisterNoIcon([TSmoothMagnifyEffect]);
  RegisterNoIcon([TWrapEffect]);
  RegisterNoIcon([TSwirlEffect]);
  RegisterNoIcon([TBandsEffect]);
  RegisterNoIcon([TMagnifyEffect]);
  RegisterNoIcon([TPinchEffect]);
  RegisterNoIcon([TRippleEffect]);
  RegisterNoIcon([TPerspectiveTransformEffect]);
  RegisterNoIcon([TCropEffect]);
  RegisterNoIcon([TAffineTransformEffect]);
  RegisterNoIcon([TTilerEffect]);
  RegisterNoIcon([TDirectionalBlurEffect]);
  RegisterNoIcon([TRadialBlurEffect]);
  RegisterNoIcon([TGaussianBlurEffect]);
  RegisterNoIcon([TBoxBlurEffect]);
  RegisterNoIcon([TMaskToAlphaEffect]);
  RegisterNoIcon([TColorKeyAlphaEffect]);
  RegisterNoIcon([TMonochromeEffect]);
  RegisterNoIcon([TInvertEffect]);
  RegisterNoIcon([TGloomEffect]);
  RegisterNoIcon([THueAdjustEffect]);
  RegisterNoIcon([TBloomEffect]);
  RegisterNoIcon([TContrastEffect]);
  RegisterNoIcon([TFillEffect]);
  RegisterNoIcon([TFillRGBEffect]);
  RegisterNoIcon([TNormalBlendEffect]);
end;

initialization
  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  RegisterClasses([TPixelateTransitionEffect]);
  RegisterClasses([TBrightTransitionEffect]);
  RegisterClasses([TSwirlTransitionEffect]);
  RegisterClasses([TDissolveTransitionEffect]);
  RegisterClasses([TBloodTransitionEffect]);
  RegisterClasses([TBlurTransitionEffect]);
  RegisterClasses([TDropTransitionEffect]);
  RegisterClasses([TCrumpleTransitionEffect]);
  RegisterClasses([TWaterTransitionEffect]);
  RegisterClasses([TBandedSwirlTransitionEffect]);
  RegisterClasses([TSwipeTransitionEffect]);
  RegisterClasses([TSlideTransitionEffect]);
  RegisterClasses([TMagnifyTransitionEffect]);
  RegisterClasses([TLineTransitionEffect]);
  RegisterClasses([TRotateCrumpleTransitionEffect]);
  RegisterClasses([TRippleTransitionEffect]);
  RegisterClasses([TCircleTransitionEffect]);
  RegisterClasses([TWiggleTransitionEffect]);
  RegisterClasses([TSaturateTransitionEffect]);
  RegisterClasses([TWaveTransitionEffect]);
  RegisterClasses([TFadeTransitionEffect]);
  RegisterClasses([TBlindTransitionEffect]);
  RegisterClasses([TShapeTransitionEffect]);
  RegisterClasses([TPencilStrokeEffect]);
  RegisterClasses([TSharpenEffect]);
  RegisterClasses([TEmbossEffect]);
  RegisterClasses([TSepiaEffect]);
  RegisterClasses([TPaperSketchEffect]);
  RegisterClasses([TPixelateEffect]);
  RegisterClasses([TToonEffect]);
  RegisterClasses([TWaveEffect]);
  RegisterClasses([TBandedSwirlEffect]);
  RegisterClasses([TSmoothMagnifyEffect]);
  RegisterClasses([TWrapEffect]);
  RegisterClasses([TSwirlEffect]);
  RegisterClasses([TBandsEffect]);
  RegisterClasses([TMagnifyEffect]);
  RegisterClasses([TPinchEffect]);
  RegisterClasses([TRippleEffect]);
  RegisterClasses([TPerspectiveTransformEffect]);
  RegisterClasses([TCropEffect]);
  RegisterClasses([TAffineTransformEffect]);
  RegisterClasses([TTilerEffect]);
  RegisterClasses([TDirectionalBlurEffect]);
  RegisterClasses([TRadialBlurEffect]);
  RegisterClasses([TGaussianBlurEffect]);
  RegisterClasses([TBoxBlurEffect]);
  RegisterClasses([TMaskToAlphaEffect]);
  RegisterClasses([TColorKeyAlphaEffect]);
  RegisterClasses([TMonochromeEffect]);
  RegisterClasses([TInvertEffect]);
  RegisterClasses([TGloomEffect]);
  RegisterClasses([THueAdjustEffect]);
  RegisterClasses([TBloomEffect]);
  RegisterClasses([TContrastEffect]);
  RegisterClasses([TFillEffect]);
  RegisterClasses([TFillRGBEffect]);
  RegisterClasses([TNormalBlendEffect]);
end.
