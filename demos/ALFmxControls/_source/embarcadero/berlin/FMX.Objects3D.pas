{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Objects3D;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.UITypes, System.Math.Vectors, FMX.Types, FMX.Objects, FMX.Types3D,
  FMX.Controls3D, FMX.MaterialSources, FMX.Graphics;

type
  TMeshWrapMode = (Original, Fit, Stretch, Resize);

{ TDummy }

  TDummy = class(TControl3D)
  protected
    procedure Render; override;
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property HitTest default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TGrid3D }

  TGrid3D = class(TControl3D)
  private
    FLineColor: TAlphaColor;
    FFrequency: Single;
    FMarks: Single;
    procedure SetLineColor(const Value: TAlphaColor);
    function GetLineColor: TAlphaColor;
    procedure SetFrequency(const Value: Single);
    procedure SetMarks(const Value: Single);
  protected
    procedure SetDepth(const Value: Single); override;
    procedure Render; override;
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Marks: Single read FMarks write SetMarks;
    property Frequency: Single read FFrequency write SetFrequency;
    property LineColor: TAlphaColor read GetLineColor write SetLineColor;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TShape3D }

  TShape3D = class(TControl3D)
  private
    FWrapMode: TMeshWrapMode;
    FMaterialSource: TMaterialSource;
    procedure SetMaterialSource(const Value: TMaterialSource);
    procedure PrepareMaterialSource;
    procedure ReadMaterialDiffuse(Reader: TReader);
    procedure ReadMaterialLighting(Reader: TReader);
    procedure ReadMaterialSpecular(Reader: TReader);
    procedure ReadMaterialAmbient(Reader: TReader);
    procedure ReadMaterialEmissive(Reader: TReader);
    procedure ReadMaterialFillMode(Reader: TReader);
    procedure ReadMaterialModulation(Reader: TReader);
    procedure ReadMaterialShadeMode(Reader: TReader);
    procedure ReadMaterialTexture(Stream: TStream);
    procedure ReadMaterialTextureFiltering(Reader: TReader);
    procedure ReadMaterialShininess(Reader: TReader);
    procedure SetWrapMode(const AMode: TMeshWrapMode);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure FreeNotification(AObject: TObject); override;
    function GetMaterialForSorting: TMaterial; override;
    /// <summary>Callback to override when a wrap mode has changed.</summary>
    procedure WrapModeChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MaterialSource: TMaterialSource read FMaterialSource write SetMaterialSource;
  published
    /// <summary>Specifies how a shape should fit in its bounding box.</summary>
    property WrapMode: TMeshWrapMode read FWrapMode write SetWrapMode default TMeshWrapMode.Stretch;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TStrokeCube }

  TStrokeCube = class(TControl3D)
  private
    FColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
    procedure ReadMaterialDiffuse(Reader: TReader);
    procedure ReadMaterialLighting(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color: TAlphaColor read FColor write SetColor;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TCustomMesh }

  TCustomMesh = class(TShape3D)
  private
    FData: TMeshData;
    procedure SetMeshData(const Value: TMeshData);
  protected
    procedure DoMeshChanged(Sender: TObject);
    procedure Render; override;
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
    /// <summary>Returns a matrix to use in order to fit in the desired mode.</summary>
    function GetMeshMatrix: TMatrix3D;
    property Data: TMeshData read FData write SetMeshData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TCube }

  TCube = class(TCustomMesh)
  private
    FSubdivisionsWidth: Integer;
    FSubdivisionsDepth: Integer;
    FSubdivisionsHeight: Integer;
    procedure SetSubdivisionsDepth(const Value: Integer);
    procedure SetSubdivisionsHeight(const Value: Integer);
    procedure SetSubdivisionsWidth(const Value: Integer);
  protected
    procedure RebuildMesh;
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SubdivisionsDepth: Integer read FSubdivisionsDepth write SetSubdivisionsDepth default 1;
    property SubdivisionsHeight: Integer read FSubdivisionsHeight write SetSubdivisionsHeight default 1;
    property SubdivisionsWidth: Integer read FSubdivisionsWidth write SetSubdivisionsWidth default 1;
    property MaterialSource;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TPlane }

  TPlane = class(TCustomMesh)
  private
    FSubdivisionsWidth: Integer;
    FSubdivisionsHeight: Integer;
    procedure SetSubdivisionsHeight(const Value: Integer);
    procedure SetSubdivisionsWidth(const Value: Integer);
  protected
    procedure SetDepth(const Value: Single); override;
    procedure RebuildMesh;
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SubdivisionsHeight: Integer read FSubdivisionsHeight write SetSubdivisionsHeight default 1;
    property SubdivisionsWidth: Integer read FSubdivisionsWidth write SetSubdivisionsWidth default 1;
    property MaterialSource;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TDisk }

  TDisk = class(TCustomMesh)
  private
    FSubdivisionsAxes: Integer;
    FSubdivisionsCap: Integer;
    procedure SetSubdivisionsAxes(const Value: Integer);
    procedure SetSubdivisionsCap(const Value: Integer);
  protected
    procedure SetHeight(const Value: Single); override;
    procedure RebuildMesh;
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SubdivisionsAxes: Integer read FSubdivisionsAxes write SetSubdivisionsAxes default 16;
    property SubdivisionsCap: Integer read FSubdivisionsCap write SetSubdivisionsCap default 3;
    property MaterialSource;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TMesh }

  TMesh = class(TCustomMesh)
  protected
  published
    property Data;
    property MaterialSource;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TSphere }

  TSphere = class(TCustomMesh)
  private
    FSubdivisionsAxes: Integer;
    FSubdivisionsHeight: Integer;
    procedure SetSubdivisionsAxes(const Value: Integer);
    procedure SetSubdivisionsHeight(const Value: Integer);
  protected
    procedure RebuildMesh;
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SubdivisionsAxes: Integer read FSubdivisionsAxes write SetSubdivisionsAxes default 16;
    property SubdivisionsHeight: Integer read FSubdivisionsHeight write SetSubdivisionsHeight default 12;
    property MaterialSource;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TCylinder }

  TCylinder = class(TCustomMesh)
  private
    FSubdivisionsAxes: Integer;
    FSubdivisionsCap: Integer;
    FSubdivisionsHeight: Integer;
    procedure SetSubdivisionsAxes(const Value: Integer);
    procedure SetSubdivisionsCap(const Value: Integer);
    procedure SetSubdivisionsHeight(const Value: Integer);
  protected
    procedure RebuildMesh;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SubdivisionsAxes: Integer read FSubdivisionsAxes write SetSubdivisionsAxes default 12;
    property SubdivisionsCap: Integer read FSubdivisionsCap write SetSubdivisionsCap default 1;
    property SubdivisionsHeight: Integer read FSubdivisionsHeight write SetSubdivisionsHeight default 1;
    property MaterialSource;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TRoundCube }

  TRoundCube = class(TCustomMesh)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MaterialSource;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TCone }

  TCone = class(TCustomMesh)
  private
    FSubdivisionsAxes: Integer;
    FSubdivisionsCap: Integer;
    FSubdivisionsHeight: Integer;
    FJoinPeakNormals: Boolean;
    procedure SetJoinPeakNormals(const Value: Boolean);
    procedure SetSubdivisionsAxes(const Value: Integer);
    procedure SetSubdivisionsCap(const Value: Integer);
    procedure SetSubdivisionsHeight(const Value: Integer);
  protected
    procedure RebuildMesh;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>If true, the normals of the top of the cone are joined in only one normal.</summary>
    property JoinPeakNormals: Boolean read FJoinPeakNormals write SetJoinPeakNormals default True;
    property SubdivisionsAxes: Integer read FSubdivisionsAxes write SetSubdivisionsAxes default 12;
    property SubdivisionsCap: Integer read FSubdivisionsCap write SetSubdivisionsCap default 1;
    property SubdivisionsHeight: Integer read FSubdivisionsHeight write SetSubdivisionsHeight default 1;
    property MaterialSource;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ Pseudo 3D Objects }

  TExtrudedShapeSide = (Front, Back, Shaft);

  TExtrudedShapeSideHelper = record helper for TExtrudedShapeSide
  const
    esFront = TExtrudedShapeSide.Front deprecated 'Use TExtrudedShapeSide.Front';
    esBack = TExtrudedShapeSide.Back deprecated 'Use TExtrudedShapeSide.Back';
    esShaft = TExtrudedShapeSide.Shaft deprecated 'Use TExtrudedShapeSide.Shaft';
  end;

  TExtrudedShapeSides = set of TExtrudedShapeSide;

{ TExtrudedShape3D }

  TExtrudedShape3D = class(TShape3D)
  private
    FFlatness: Single;
    FSides: TExtrudedShapeSides;
    FMaterialShaftSource: TMaterialSource;
    FMaterialBackSource: TMaterialSource;
    procedure SetFlatness(const Value: Single);
    procedure SetSides(const Value: TExtrudedShapeSides);
    procedure SetMaterialBack(const Value: TMaterialSource);
    procedure SetMaterialShaft(const Value: TMaterialSource);
    procedure PrepareMaterialBackSource;
    procedure ReadMaterialBackDiffuse(Reader: TReader);
    procedure ReadMaterialBackLighting(Reader: TReader);
    procedure ReadMaterialBackSpecular(Reader: TReader);
    procedure ReadMaterialBackAmbient(Reader: TReader);
    procedure ReadMaterialBackEmissive(Reader: TReader);
    procedure ReadMaterialBackFillMode(Reader: TReader);
    procedure ReadMaterialBackModulation(Reader: TReader);
    procedure ReadMaterialBackShadeMode(Reader: TReader);
    procedure ReadMaterialBackTexture(Stream: TStream);
    procedure ReadMaterialBackTextureFiltering(Reader: TReader);
    procedure ReadMaterialBackShininess(Reader: TReader);
    procedure PrepareMaterialShaftSource;
    procedure ReadMaterialShaftDiffuse(Reader: TReader);
    procedure ReadMaterialShaftLighting(Reader: TReader);
    procedure ReadMaterialShaftSpecular(Reader: TReader);
    procedure ReadMaterialShaftAmbient(Reader: TReader);
    procedure ReadMaterialShaftEmissive(Reader: TReader);
    procedure ReadMaterialShaftFillMode(Reader: TReader);
    procedure ReadMaterialShaftModulation(Reader: TReader);
    procedure ReadMaterialShaftShadeMode(Reader: TReader);
    procedure ReadMaterialShaftTexture(Stream: TStream);
    procedure ReadMaterialShaftTextureFiltering(Reader: TReader);
    procedure ReadMaterialShaftShininess(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure FreeNotification(AObject: TObject); override;
    procedure Render; override;
    procedure ShapeMouseMove(Shift: TShiftState; X, Y: Single); virtual;
    procedure ShapeMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure ShapeMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseMove3D(Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
    procedure MouseDown3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
    procedure MouseUp3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Flatness: Single read FFlatness write SetFlatness;
    property Sides: TExtrudedShapeSides read FSides write SetSides;
    property MaterialBackSource: TMaterialSource read FMaterialBackSource write SetMaterialBack;
    property MaterialShaftSource: TMaterialSource read FMaterialShaftSource write SetMaterialShaft;
  end;

{ TRectangle3D }

  TRectangle3D = class(TExtrudedShape3D)
  private
    FYRadius: Single;
    FXRadius: Single;
    FCorners: TCorners;
    FCornerType: TCornerType;
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
    function IsCornersStored: Boolean;
    procedure SetCorners(const Value: TCorners);
    procedure SetCornerType(const Value: TCornerType);
  protected
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property CornerType: TCornerType read FCornerType write SetCornerType;
    property Flatness;
    property Sides;
    property MaterialBackSource;
    property MaterialShaftSource;
    property MaterialSource;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth nodefault;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TEllipse3D }

  TEllipse3D = class(TExtrudedShape3D)
  protected
    procedure Render; override;
  published
    property Flatness;
    property Sides;
    property MaterialBackSource;
    property MaterialShaftSource;
    property MaterialSource;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth nodefault;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TText3D }

  TText3D = class(TExtrudedShape3D)
  private
    FFont: TFont;
    FText: string;
    FWordWrap: Boolean;
    FStretch: Boolean;
    FVertTextAlign: TTextAlign;
    FHorzTextAlign: TTextAlign;
    FPolygon: TPolygon;
    FTextRect: TRectF;
    procedure SetFont(const Value: TFont);
    procedure SetText(const Value: string);
    procedure SetHorzTextAlign(const Value: TTextAlign);
    procedure SetStretch(const Value: Boolean);
    procedure SetVertTextAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure RebuildPolygon;
  protected
    procedure FontChanged(Sender: TObject);
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTextBounds: TRectF;
    function GetPathBounds: TRectF;
    function GetPathLength: Single;
  published
    property Font: TFont read FFont write SetFont;
    property HorzTextAlign: TTextAlign read FHorzTextAlign write SetHorzTextAlign default TTextAlign.Center;
    property VertTextAlign: TTextAlign read FVertTextAlign write SetVertTextAlign default TTextAlign.Center;
    property Text: string read FText write SetText;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
    property Flatness;
    property Sides;
    property MaterialBackSource;
    property MaterialShaftSource;
    property MaterialSource;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth nodefault;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TPath3D }

  TPath3D = class(TExtrudedShape3D)
  private
    FPath: TPathData;
    FWrapMode: TPathWrapMode;
    procedure SetPath(const Value: TPathData);
    procedure SetWrapMode(const Value: TPathWrapMode);
  protected
    procedure PathChanged(Sender: TObject);
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Path: TPathData read FPath write SetPath;
    property WrapMode: TPathWrapMode read FWrapMode write SetWrapMode default TPathWrapMode.Stretch;
    property Flatness;
    property Sides;
    property MaterialBackSource;
    property MaterialShaftSource;
    property MaterialSource;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth nodefault;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TModel3D }

  TMeshDynArray = array of TMesh;

  TMeshCollection = TMeshDynArray;

  TModel3D = class(TDummy)
  private
    FMeshesOwner: TDummy;
    FWrapMode: TMeshWrapMode;
    FMeshCollection: TMeshCollection;
    procedure RelocateMeshes;
    procedure SetWrapMode(const AMode: TMeshWrapMode);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadModel(Stream: TStream);
    procedure WriteModel(Stream: TStream);
    procedure UpdateMeshCollection;
    procedure Resize3D; override;
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
    /// <summary>Returns the bounding box of all meshes inside the model.</summary>
    function GetMeshesLocalBoundingBox: TBoundingBox;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    function LoadFromFile(const AFileName: string): Boolean; virtual;
  published
    property MeshCollection: TMeshCollection read FMeshCollection;
    /// <summary>Specifies how a model should fit in its bounding box.</summary>
    property WrapMode: TMeshWrapMode read FWrapMode write SetWrapMode default TMeshWrapMode.Fit;
  end;

implementation

uses
  System.SysUtils, System.UIConsts, System.Math, System.RTLConsts, System.TypInfo, FMX.Import, FMX.TextLayout;

type
  THackMaterial = class(TMaterial);

{ TDummy }

constructor TDummy.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
end;

destructor TDummy.Destroy;
begin
  inherited;
end;

procedure TDummy.Render;
begin
  if Tag = $FFFE then
    Exit;

  if (csDesigning in ComponentState) and not Locked then
  begin
    Context.DrawCube(NullPoint3D, TPoint3D.Create(Width, Height, Depth), AbsoluteOpacity, $8060A799);
  end;
end;

function TDummy.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
begin
  Result := False;
  if (csDesigning in ComponentState) then
    Result := inherited;
end;

{ TGrid3D }

constructor TGrid3D.Create(AOwner: TComponent);
begin
  inherited;
  FFrequency := 1;
  FMarks := 4;
  FLineColor := $50505050;
  Depth := 0.001;
end;

procedure TGrid3D.Render;
var
  X, Y: Single;
  C: TAlphaColor;
begin
  inherited;
  X := 0;
  Y := 0;
  while X < Width / 2 do
  begin
    if (frac(X) = 0) and (frac(X / Marks) = 0) then
      C := MakeColor(FLineColor and $FFFFFF or $A0000000, AbsoluteOpacity)
    else
      C := MakeColor(FLineColor, AbsoluteOpacity);
    Context.DrawLine(TPoint3D.Create(X, -Height / 2, 0), TPoint3D.Create(X, Height / 2, 0), AbsoluteOpacity, C);
    Context.DrawLine(TPoint3D.Create(-X, -Height / 2, 0), TPoint3D.Create(-X, Height / 2, 0), AbsoluteOpacity, C);
    X := X + FFrequency;
  end;
  while Y < Height / 2 do
  begin
    if (frac(Y) = 0) and (frac(Y / Marks) = 0) then
      C := MakeColor(FLineColor and $FFFFFF or $A0000000, AbsoluteOpacity)
    else
      C := MakeColor(FLineColor, AbsoluteOpacity);
    Context.DrawLine(TPoint3D.Create(-Width / 2, Y, 0), TPoint3D.Create(Width / 2, Y, 0), AbsoluteOpacity, C);
    Context.DrawLine(TPoint3D.Create(-Width / 2, -Y, 0), TPoint3D.Create(Width / 2, -Y, 0), AbsoluteOpacity, C);
    Y := Y + FFrequency;
  end;
end;

function TGrid3D.GetLineColor: TAlphaColor;
begin
  Result := FLineColor;
  //AlphaColorToString(FLineColor)
end;

procedure TGrid3D.SetLineColor(const Value: TAlphaColor);
//var
//  NewColor: TAlphaColor;
begin
//  NewColor := StringToAlphaColor(Value);
  if Value <> FLineColor then
    FLineColor := Value;
end;

procedure TGrid3D.SetFrequency(const Value: Single);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    if FFrequency <= 0 then
      FFrequency := 0.01;
    Repaint;
  end;
end;

function TGrid3D.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
var
  IPoint: TPoint3D;
begin
  if RayCastPlaneIntersect(RayPos, RayDir, TPoint3D.Zero, TPoint3D.Create(0, 0, -1), IPoint) and
    (Abs(IPoint.X) < Width / 2) and (Abs(IPoint.Y) < Height / 2) then
  begin
    Result := True;
    Intersection := TPoint3D(LocalToAbsoluteVector(IPoint));
  end
  else
    Result := False;
end;

procedure TGrid3D.SetDepth(const Value: Single);
begin
  inherited SetDepth(0.001);
end;

procedure TGrid3D.SetMarks(const Value: Single);
begin
  if FMarks <> Value then
  begin
    FMarks := Value;
    Repaint;
  end;
end;

{ TShape3D }

constructor TShape3D.Create(AOwner: TComponent);
begin
  inherited;
  FWrapMode := TMeshWrapMode.Stretch;
end;

destructor TShape3D.Destroy;
begin
  MaterialSource := nil;
  inherited;
end;

procedure TShape3D.FreeNotification(AObject: TObject);
begin
  inherited;
  if AObject = FMaterialSource then
    MaterialSource := nil;
end;

procedure TShape3D.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Only for backward compatibility with XE2
  Filer.DefineProperty('Material.Diffuse', ReadMaterialDiffuse, nil, False);
  Filer.DefineProperty('Material.Ambient', ReadMaterialAmbient, nil, False);
  Filer.DefineProperty('Material.Emissive', ReadMaterialEmissive, nil, False);
  Filer.DefineProperty('Material.Specular', ReadMaterialSpecular, nil, False);
  Filer.DefineProperty('Material.Lighting', ReadMaterialLighting, nil, False);
  Filer.DefineProperty('Material.FillMode', ReadMaterialFillMode, nil, False);
  Filer.DefineProperty('Material.Modulation', ReadMaterialModulation, nil, False);
  Filer.DefineBinaryProperty('Material.Texture.PNG', ReadMaterialTexture, nil, False);
  Filer.DefineProperty('Material.TextureFiltering', ReadMaterialTextureFiltering, nil, False);
  Filer.DefineProperty('Material.ShadeMode', ReadMaterialShadeMode, nil, False);
  Filer.DefineProperty('Material.Shininess', ReadMaterialShininess, nil, False);
end;

procedure TShape3D.PrepareMaterialSource;
var
  Material: TLightMaterialSource;
  N: string;
  I: Integer;
begin
  if (FMaterialSource = nil) then
  begin
    Material := TLightMaterialSource.Create(Owner);
    if Root <> nil then
      Material.Parent := Root.GetObject
    else
      if Owner is TFmxObject then
        Material.Parent := TFmxObject(Owner);

    N := Name + 'MaterialSource';
    I := 1;
    while not IsUniqueGlobalComponentName(N) do
      N := Name + 'MaterialSource' + IntToStr(I);

    Material.Name := N;
    MaterialSource := Material;
  end;
end;

procedure TShape3D.ReadMaterialDiffuse(Reader: TReader);
var
  Color: Integer;
begin
  PrepareMaterialSource;
  if (FMaterialSource <> nil) and (FMaterialSource is TLightMaterialSource) then
  begin
    IdentToAlphaColor(Reader.ReadIdent, Color);
    {$R-}
    TLightMaterialSource(FMaterialSource).Diffuse := TAlphaColor(Color);
    {$R+}
  end;
end;

procedure TShape3D.ReadMaterialAmbient(Reader: TReader);
var
  Color: Integer;
begin
  PrepareMaterialSource;
  if (FMaterialSource <> nil) and (FMaterialSource is TLightMaterialSource) then
  begin
    IdentToAlphaColor(Reader.ReadIdent, Color);
    {$R-}
    TLightMaterialSource(FMaterialSource).Ambient := TAlphaColor(Color);
    {$R+}
  end;
end;

procedure TShape3D.ReadMaterialEmissive(Reader: TReader);
begin
  Reader.ReadIdent;
end;

procedure TShape3D.ReadMaterialSpecular(Reader: TReader);
var
  Color: Integer;
begin
  PrepareMaterialSource;
  if (FMaterialSource <> nil) and (FMaterialSource is TLightMaterialSource) then
  begin
    IdentToAlphaColor(Reader.ReadIdent, Color);
    {$R-}
    TLightMaterialSource(FMaterialSource).Specular := TAlphaColor(Color);
    {$R+}
  end;
end;

procedure TShape3D.ReadMaterialLighting(Reader: TReader);
begin
  PrepareMaterialSource;
  Reader.ReadBoolean; // skip this
end;

procedure TShape3D.ReadMaterialFillMode(Reader: TReader);
begin
  PrepareMaterialSource;
  Reader.ReadIdent; // skip this
end;

procedure TShape3D.ReadMaterialModulation(Reader: TReader);
begin
  PrepareMaterialSource;
  Reader.ReadIdent; // skip this
end;

procedure TShape3D.ReadMaterialTexture(Stream: TStream);
begin
  PrepareMaterialSource;
  if (FMaterialSource <> nil) and (FMaterialSource is TLightMaterialSource) then
    TLightMaterialSource(FMaterialSource).Texture.LoadFromStream(Stream);
end;

procedure TShape3D.ReadMaterialTextureFiltering(Reader: TReader);
begin
  PrepareMaterialSource;
  Reader.ReadIdent; // skip this
end;

procedure TShape3D.ReadMaterialShadeMode(Reader: TReader);
begin
  PrepareMaterialSource;
  Reader.ReadIdent; // skip this
end;

procedure TShape3D.ReadMaterialShininess(Reader: TReader);
begin
  PrepareMaterialSource;
  if (FMaterialSource <> nil) and (FMaterialSource is TLightMaterialSource) then
    TLightMaterialSource(FMaterialSource).Shininess := Reader.ReadInteger;
end;

function TShape3D.GetMaterialForSorting: TMaterial;
begin
  Result := TMaterialSource.ValidMaterial(FMaterialSource);
end;

procedure TShape3D.SetMaterialSource(const Value: TMaterialSource);
begin
  if FMaterialSource <> Value then
  begin
    if FMaterialSource <> nil then
    begin
      FMaterialSource.RemoveChangeNotifier(Self);
      FMaterialSource.RemoveFreeNotify(Self);
    end;
    FMaterialSource := Value;
    if FMaterialSource <> nil then
    begin
      FMaterialSource.AddFreeNotify(Self);
      FMaterialSource.AddChangeNotifier(Self);
    end;
  end;
end;

procedure TShape3D.SetWrapMode(const AMode: TMeshWrapMode);
begin
  if FWrapMode <> AMode then
  begin
    FWrapMode := AMode;
    WrapModeChanged;
    Repaint;
  end;
end;

procedure TShape3D.WrapModeChanged;
begin
end;

{ TStrokeCube }

constructor TStrokeCube.Create(AOwner: TComponent);
begin
  inherited;
  FColor := claWhite;
end;

procedure TStrokeCube.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Only for backward compatibility with XE2
  Filer.DefineProperty('Material.Diffuse', ReadMaterialDiffuse, nil, False);
  Filer.DefineProperty('Material.Lighting', ReadMaterialLighting, nil, False);
end;

procedure TStrokeCube.ReadMaterialDiffuse(Reader: TReader);
var
  LColor: Integer;
begin
  IdentToAlphaColor(Reader.ReadIdent, LColor);
  {$R-}
  Color := TAlphaColor(LColor);
  {$R+}
end;

procedure TStrokeCube.ReadMaterialLighting(Reader: TReader);
begin
  Reader.ReadBoolean; // skip this
end;

procedure TStrokeCube.Render;
begin
  Context.DrawCube(TPoint3D.Zero, TPoint3D.Create(Width, Height, Depth), AbsoluteOpacity, FColor);
end;

procedure TStrokeCube.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Repaint;
  end;
end;

{ TCustomMesh }

constructor TCustomMesh.Create(AOwner: TComponent);
begin
  inherited;
  FData := TMeshData.Create;
  FData.OnChanged := DoMeshChanged;
end;

destructor TCustomMesh.Destroy;
begin
  FData.Free;
  inherited;
end;

function TCustomMesh.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
var
  LMatrix, LInvMatrix: TMatrix3D;
  LLocalRayPos: TPoint3D;
  LLocalRayDir: TPoint3D;
begin
  Result := False;
  // LMatrix is a matrix which converts from local space to mesh space
  LMatrix := GetMeshMatrix;
  LInvMatrix := LMatrix.Inverse;

  LLocalRayPos := TPoint3D(TVector3D(RayPos) * LInvMatrix);
  LLocalRayDir := TPoint3D(TVector3D.Create(RayDir, 0) * LInvMatrix).Normalize;

  if Data.RayCastIntersect(LLocalRayPos, LLocalRayDir, Intersection) then
  begin
    // Convert from child object space to this control space, and then to absolute
    Intersection := TPoint3D(LocalToAbsoluteVector(Intersection * LMatrix));
    Exit(True);
  end;
end;

function TCustomMesh.GetMeshMatrix: TMatrix3D;
var
  DataBoundingBox: TBoundingBox;
  ComponentBoundingBox: TBoundingBox;
  FinalBoundingBox: TBoundingBox;
  LScale: TPoint3D;
  BoundingBoxDisplacement: TPoint3D;
  Translation: TPoint3D;
  HalfSize: TPoint3D;
  ScaleFactor: Single;
begin
  if WrapMode <> TMeshWrapMode.Resize then
  begin
    DataBoundingBox := FData.GetBoundingBox;
    if (DataBoundingBox.Right < DataBoundingBox.Left) or (DataBoundingBox.Bottom < DataBoundingBox.Top) or
      (DataBoundingBox.Far < DataBoundingBox.Near) then
      Exit(TMatrix3D.Identity);
  end
  else
    DataBoundingBox := TBoundingBox.Empty;

  case WrapMode of
    TMeshWrapMode.Original:
    begin
      LScale := TPoint3D.Create(Width, Height, Depth);
      Translation := TPoint3D.Zero;
      BoundingBoxDisplacement := TPoint3D.Zero;
    end;
    TMeshWrapMode.Fit:
    begin
      HalfSize := TPoint3D.Create(0.5 * Width, 0.5 * Height, 0.5 * Depth);
      ComponentBoundingBox := TBoundingBox.Create(-HalfSize, HalfSize);
      FinalBoundingBox := DataBoundingBox.FitInto(ComponentBoundingBox, ScaleFactor);
      ScaleFactor := 1 / ScaleFactor;

      LScale := TPoint3D.Create(ScaleFactor, ScaleFactor, ScaleFactor);
      Translation := FinalBoundingBox.TopLeftNear;
      BoundingBoxDisplacement := -DataBoundingBox.TopLeftNear;
    end;
    TMeshWrapMode.Stretch:
    begin
      LScale := TPoint3D.Create(1, 1, 1);
      if DataBoundingBox.Width > TEpsilon.Vector then
        LScale.X := Width / DataBoundingBox.Width;
      if DataBoundingBox.Height > TEpsilon.Vector then
        LScale.Y := Height / DataBoundingBox.Height;
      if DataBoundingBox.Depth > TEpsilon.Vector then
        LScale.Z := Depth / DataBoundingBox.Depth;
      Translation := TPoint3D.Create(-0.5 * Width, -0.5 * Height, -0.5 * Depth);
      BoundingBoxDisplacement := -DataBoundingBox.TopLeftNear;
    end;
    else
    begin
      LScale := TPoint3D.Create(1, 1, 1);
      Translation := TPoint3D.Zero;
      BoundingBoxDisplacement := TPoint3D.Zero;
    end;
  end;
  Result := TMatrix3D.CreateTranslation(BoundingBoxDisplacement) * TMatrix3D.CreateScaling(LScale) *
    TMatrix3D.CreateTranslation(Translation);
end;

procedure TCustomMesh.Render;
begin
  Context.SetMatrix(GetMeshMatrix * AbsoluteMatrix);
  FData.Render(Context, TMaterialSource.ValidMaterial(FMaterialSource), AbsoluteOpacity);
end;

procedure TCustomMesh.SetMeshData(const Value: TMeshData);
begin
  FData.Assign(Value);
end;

procedure TCustomMesh.DoMeshChanged(Sender: TObject);
begin
  Repaint;
end;

{ TPlane }

constructor TPlane.Create(AOwner: TComponent);
begin
  inherited;
  Depth := 0.001;
  FSubdivisionsWidth := 1;
  FSubdivisionsHeight := 1;
  RebuildMesh;
end;

procedure TPlane.RebuildMesh;
var
  HalfVer, HalfIdx, X, Y: Integer;
begin
  HalfVer := (FSubdivisionsWidth + 1) * (FSubdivisionsHeight + 1);
  HalfIdx := FSubdivisionsWidth * FSubdivisionsHeight * 6;
  FData.VertexBuffer.Length := HalfVer * 2;
  for Y := 0 to FSubdivisionsHeight do
    for X := 0 to FSubdivisionsWidth do
    begin
      FData.VertexBuffer.Vertices[X + (Y * (FSubdivisionsWidth + 1))] := Point3D(-0.5 + (X / FSubdivisionsWidth), -0.5 + (Y / FSubdivisionsHeight), 0);
      FData.VertexBuffer.Normals[X + (Y * (FSubdivisionsWidth + 1))] := Point3D(0, 0, -1);
      FData.VertexBuffer.TexCoord0[X + (Y * (FSubdivisionsWidth + 1))] := PointF(X / FSubdivisionsWidth, Y / FSubdivisionsHeight);
      FData.VertexBuffer.Vertices[HalfVer + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(-0.5 + (X / FSubdivisionsWidth), -0.5 + (Y / FSubdivisionsHeight), 0);
      FData.VertexBuffer.Normals[HalfVer + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(0, 0, 1);
      FData.VertexBuffer.TexCoord0[HalfVer + X + (Y * (FSubdivisionsWidth + 1))] := PointF(X / FSubdivisionsWidth, Y / FSubdivisionsHeight);
    end;
  FData.IndexBuffer.Length := HalfIdx * 2;
  for Y := 0 to FSubdivisionsHeight - 1 do
    for X := 0 to FSubdivisionsWidth - 1 do
    begin
      FData.IndexBuffer[(X + (Y * (FSubdivisionsWidth + 0))) * 6 + 0] := X + (Y * (FSubdivisionsWidth + 1));
      FData.IndexBuffer[(X + (Y * (FSubdivisionsWidth + 0))) * 6 + 1] := X + 1 + (Y * (FSubdivisionsWidth + 1));
      FData.IndexBuffer[(X + (Y * (FSubdivisionsWidth + 0))) * 6 + 2] := X + ((Y + 1) * (FSubdivisionsWidth + 1));
      FData.IndexBuffer[(X + (Y * (FSubdivisionsWidth + 0))) * 6 + 3] := X + ((Y + 1) * (FSubdivisionsWidth + 1));
      FData.IndexBuffer[(X + (Y * (FSubdivisionsWidth + 0))) * 6 + 4] := X + 1 + (Y * (FSubdivisionsWidth + 1));
      FData.IndexBuffer[(X + (Y * (FSubdivisionsWidth + 0))) * 6 + 5] := X + 1 + ((Y + 1) * (FSubdivisionsWidth + 1));

      FData.IndexBuffer[HalfIdx + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 0] := HalfVer + X + (Y * (FSubdivisionsWidth + 1));
      FData.IndexBuffer[HalfIdx + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 2] := HalfVer + X + 1 + (Y * (FSubdivisionsWidth + 1));
      FData.IndexBuffer[HalfIdx + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 1] := HalfVer + X + ((Y + 1) * (FSubdivisionsWidth + 1));
      FData.IndexBuffer[HalfIdx + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 3] := HalfVer + X + ((Y + 1) * (FSubdivisionsWidth + 1));
      FData.IndexBuffer[HalfIdx + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 5] := HalfVer + X + 1 + (Y * (FSubdivisionsWidth + 1));
      FData.IndexBuffer[HalfIdx + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 4] := HalfVer + X + 1 + ((Y + 1) * (FSubdivisionsWidth + 1));
    end;
end;

function TPlane.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
var
  IntersectionPoint: TPoint3D;
  LWidth, LHeight: Single;
begin
  Result := False;
  if RayCastPlaneIntersect(RayPos, RayDir, TPoint3D.Zero, TPoint3D.Create(0, 0, -1), IntersectionPoint) then
  begin
    LWidth := 1;
    LHeight := 1;
    case WrapMode of
      TMeshWrapMode.Fit:
      begin
        LWidth := Min(Width, Height);
        LHeight := LWidth;
      end;
      TMeshWrapMode.Stretch, TMeshWrapMode.Original:
      begin
        LWidth := Width;
        LHeight := Height;
      end;
    end;
    Result := (Abs(IntersectionPoint.X) < LWidth * 0.5) and (Abs(IntersectionPoint.Y) < LHeight * 0.5);
    if Result then
      Intersection := TPoint3D(LocalToAbsoluteVector(IntersectionPoint));
  end;
end;

procedure TPlane.SetDepth(const Value: Single);
begin
  inherited SetDepth(0.001);
end;

procedure TPlane.SetSubdivisionsHeight(const Value: Integer);
begin
  if FSubdivisionsHeight <> Value then
  begin
    FSubdivisionsHeight := Value;
    if FSubdivisionsHeight < 1 then
      FSubdivisionsHeight := 1;
    RebuildMesh;
  end;
end;

procedure TPlane.SetSubdivisionsWidth(const Value: Integer);
begin
  if FSubdivisionsWidth <> Value then
  begin
    FSubdivisionsWidth := Value;
    if FSubdivisionsWidth < 1 then
      FSubdivisionsWidth := 1;
    RebuildMesh;
  end;
end;

{ TDisk }

constructor TDisk.Create(AOwner: TComponent);
begin
  inherited;
  Height := 0.001;
  FSubdivisionsAxes := 16;
  FSubdivisionsCap := 1;
  RebuildMesh;
end;

function TDisk.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
var
  IntersectionPoint: TPoint3D;
  LWidth, LDepth: Single;
begin
  Result := False;
  if RayCastPlaneIntersect(RayPos, RayDir, TPoint3D.Zero, TPoint3D.Create(0, -1, 0), IntersectionPoint) then
  begin
    LWidth := 1;
    LDepth := 1;
    case WrapMode of
      TMeshWrapMode.Fit:
      begin
        LWidth := Min(Width, Depth);
        LDepth := LWidth;
      end;
      TMeshWrapMode.Stretch, TMeshWrapMode.Original:
      begin
        LWidth := Width;
        LDepth := Depth;
      end;
    end;
    Result := (Abs(IntersectionPoint.X) < LWidth * 0.5) and (Abs(IntersectionPoint.Z) < LDepth * 0.5) and
      (Sqr(IntersectionPoint.X / (LWidth * 0.5)) + Sqr(IntersectionPoint.Z / (LDepth * 0.5)) <= 1);
    if Result then
      Intersection := TPoint3D(LocalToAbsoluteVector(IntersectionPoint));
  end;
end;

procedure TDisk.RebuildMesh;
var
  A, H, AA, HH: Integer;
  Phi: Single;
  DPhi: Single;
  PhiSin, PhiCos: Double;
  IdxCount: Integer;
  VerticesWidth: Integer;
  HalfVer, HalfIdx: Integer;
begin
  VerticesWidth := (FSubdivisionsAxes + 1);
  HalfVer := (FSubdivisionsCap) * VerticesWidth + 1;
  FData.VertexBuffer.Length := HalfVer * 2;
  HalfIdx := (FSubdivisionsCap - 1) * FSubdivisionsAxes * 6 + (FSubdivisionsAxes * 3);
  FData.IndexBuffer.Length := HalfIdx * 2;
  DPhi := DegToRad(360) / FSubdivisionsAxes;
  // front face
  IdxCount := 0;
  FData.VertexBuffer.Vertices[HalfVer - 1] := Point3D(0, 0, 0);
  FData.VertexBuffer.TexCoord0[HalfVer - 1] := PointF(0.5, 0.5);
  FData.VertexBuffer.Normals[HalfVer - 1] := Point3D(0, -1, 0);
  for H := 0 to FSubdivisionsCap - 1 do
  begin
    Phi := 0;
    for A := 0 to FSubdivisionsAxes - 1 do
    begin
      SinCos(Phi, PhiSin, PhiCos);
      FData.VertexBuffer.Vertices[A + (H * VerticesWidth)] := Point3D(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap), 0, PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap));
      FData.VertexBuffer.TexCoord0[A + (H * VerticesWidth)] := PointF(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5, 1 - (PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5));
      FData.VertexBuffer.Normals[A + (H * VerticesWidth)] := Point3D(0, -1, 0);
      if A = 0 then
      begin
        FData.VertexBuffer.Vertices[FSubdivisionsAxes + (H * VerticesWidth)] := Point3D(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap), 0, PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap));
        FData.VertexBuffer.TexCoord0[FSubdivisionsAxes + (H * VerticesWidth)] := PointF(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5, 1 - (PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5));
        FData.VertexBuffer.Normals[FSubdivisionsAxes + (H * VerticesWidth)] := Point3D(0, -1, 0);
      end;
      AA := A + 1;
      HH := H - 1;
      if H = 0 then
      begin
        FData.IndexBuffer.Indices[IdxCount + 0] := A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 1] := HalfVer - 1;
        FData.IndexBuffer.Indices[IdxCount + 2] := AA + (H * VerticesWidth);
        IdxCount := IdxCount + 3;
      end
      else
      begin
        FData.IndexBuffer.Indices[IdxCount + 0] := A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 2] := AA + (HH * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 1] := A + (HH * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 3] := A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 5] := AA + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 4] := AA + (HH * VerticesWidth);
        IdxCount := IdxCount + 6;
      end;
      Phi := Phi + DPhi;
    end;
  end;
  // back face
  IdxCount := 0;
  FData.VertexBuffer.Vertices[FData.VertexBuffer.Length - 1] := Point3D(0, 0, 0);
  FData.VertexBuffer.TexCoord0[FData.VertexBuffer.Length - 1] := PointF(0.5, 0.5);
  FData.VertexBuffer.Normals[FData.VertexBuffer.Length - 1] := Point3D(0, 1, 0);
  for H := 0 to FSubdivisionsCap - 1 do
  begin
    Phi := 0;
    for A := 0 to FSubdivisionsAxes - 1 do
    begin
      SinCos(Phi, PhiSin, PhiCos);
      FData.VertexBuffer.Vertices[HalfVer + A + (H * VerticesWidth)] := Point3D(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap), 0, PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap));
      FData.VertexBuffer.TexCoord0[HalfVer + A + (H * VerticesWidth)] := PointF(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5, 1 - (PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5));
      FData.VertexBuffer.Normals[HalfVer + A + (H * VerticesWidth)] := Point3D(0, 1, 0);
      if A = 0 then
      begin
        FData.VertexBuffer.Vertices[HalfVer + FSubdivisionsAxes + (H * VerticesWidth)] := Point3D(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap), 0, PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap));
        FData.VertexBuffer.TexCoord0[HalfVer + FSubdivisionsAxes + (H * VerticesWidth)] := PointF(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5, 1 - (PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5));
        FData.VertexBuffer.Normals[HalfVer + FSubdivisionsAxes + (H * VerticesWidth)] := Point3D(0, 1, 0);
      end;
      AA := A + 1;
      HH := H - 1;
      if H = 0 then
      begin
        FData.IndexBuffer.Indices[HalfIdx + IdxCount + 0] := HalfVer + A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[HalfIdx + IdxCount + 2] := FData.VertexBuffer.Length - 1;
        FData.IndexBuffer.Indices[HalfIdx + IdxCount + 1] := HalfVer + AA + (H * VerticesWidth);
        IdxCount := IdxCount + 3;
      end
      else
      begin
        FData.IndexBuffer.Indices[HalfIdx + IdxCount + 0] := HalfVer + A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[HalfIdx + IdxCount + 1] := HalfVer + AA + (HH * VerticesWidth);
        FData.IndexBuffer.Indices[HalfIdx + IdxCount + 2] := HalfVer + A + (HH * VerticesWidth);
        FData.IndexBuffer.Indices[HalfIdx + IdxCount + 3] := HalfVer + A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[HalfIdx + IdxCount + 4] := HalfVer + AA + (H * VerticesWidth);
        FData.IndexBuffer.Indices[HalfIdx + IdxCount + 5] := HalfVer + AA + (HH * VerticesWidth);
        IdxCount := IdxCount + 6;
      end;
      Phi := Phi + DPhi;
    end;
  end;
end;

procedure TDisk.SetHeight(const Value: Single);
begin
  inherited SetHeight(0.001);
end;

procedure TDisk.SetSubdivisionsAxes(const Value: Integer);
begin
  if FSubdivisionsAxes <> Value then
  begin
    FSubdivisionsAxes := Value;
    if FSubdivisionsAxes < 3 then
      FSubdivisionsAxes := 3;
    RebuildMesh;
  end;
end;

procedure TDisk.SetSubdivisionsCap(const Value: Integer);
begin
  if FSubdivisionsCap <> Value then
  begin
    FSubdivisionsCap := Value;
    if FSubdivisionsCap < 1 then
      FSubdivisionsCap := 1;
    RebuildMesh;
  end;
end;

{ TCube }

constructor TCube.Create(AOwner: TComponent);
begin
  inherited;
  FSubdivisionsDepth := 1;
  FSubdivisionsHeight := 1;
  FSubdivisionsWidth := 1;
  RebuildMesh;
end;

destructor TCube.Destroy;
begin
  inherited;
end;

procedure TCube.RebuildMesh;
var
  X, Y: Integer;
  Face: Integer;
  FaceVertexLength: Integer;
  FaceIndexLength: Integer;
  VertexOffset: Integer;
  IndexOffset: Integer;
begin
  FData.VertexBuffer.Length :=
    (FSubdivisionsWidth + 1) * (FSubdivisionsHeight + 1) * 2 +
    (FSubdivisionsDepth + 1) * (FSubdivisionsWidth + 1) * 2 +
    (FSubdivisionsDepth + 1) * (FSubdivisionsHeight + 1) * 2;
  FData.IndexBuffer.Length :=
    (FSubdivisionsWidth) * (FSubdivisionsHeight) * 6 * 2 +
    (FSubdivisionsDepth) * (FSubdivisionsWidth) * 6 * 2 +
    (FSubdivisionsDepth) * (FSubdivisionsHeight) * 6 * 2;

  VertexOffset := 0;
  IndexOffset := 0;
  FaceVertexLength := (FSubdivisionsWidth + 1) * (FSubdivisionsHeight + 1);
  FaceIndexLength := FSubdivisionsWidth * FSubdivisionsHeight * 6;
  for Face := 0 to 1 do
  begin
    for Y := 0 to FSubdivisionsHeight do
      for X := 0 to FSubdivisionsWidth do
      begin
        if not Odd(Face) then
        begin
          FData.VertexBuffer.Vertices[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(-0.5 + (X / FSubdivisionsWidth), -0.5 + (Y / FSubdivisionsHeight), -0.5);
          FData.VertexBuffer.Normals[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(0, 0, -1);
          FData.VertexBuffer.TexCoord0[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := PointF(X / FSubdivisionsWidth, Y / FSubdivisionsHeight);
        end
        else
        begin
          FData.VertexBuffer.Vertices[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(-0.5 + (X / FSubdivisionsWidth), -0.5 + (Y / FSubdivisionsHeight), 0.5);
          FData.VertexBuffer.Normals[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(0, 0, 1);
          FData.VertexBuffer.TexCoord0[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := PointF(1 - X / FSubdivisionsWidth, Y / FSubdivisionsHeight);
        end;
      end;
    for Y := 0 to FSubdivisionsHeight - 1 do
      for X := 0 to FSubdivisionsWidth - 1 do
      begin
        if Odd(Face) then
        begin
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 0] := VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 2] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 1] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 3] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 5] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 4] := VertexOffset + (Face * FaceVertexLength) + X + 1 + ((Y + 1) * (FSubdivisionsWidth + 1));
        end
        else
        begin
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 0] := VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 1] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 2] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 3] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 4] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 5] := VertexOffset + (Face * FaceVertexLength) + X + 1 + ((Y + 1) * (FSubdivisionsWidth + 1));
        end;
      end;
  end;
  VertexOffset := VertexOffset + FaceVertexLength * 2;
  IndexOffset := IndexOffset + FaceIndexLength * 2;
  FaceVertexLength := (FSubdivisionsDepth + 1) * (FSubdivisionsWidth + 1);
  FaceIndexLength := FSubdivisionsDepth * FSubdivisionsWidth * 6;
  for Face := 0 to 1 do
  begin
    for Y := 0 to FSubdivisionsDepth do
      for X := 0 to FSubdivisionsWidth do
      begin
        if Odd(Face) then
        begin
          FData.VertexBuffer.Vertices[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(-0.5 + (X / FSubdivisionsWidth), -0.5, -0.5 + (Y / FSubdivisionsDepth));
          FData.VertexBuffer.Normals[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(0, -1, 0);
          FData.VertexBuffer.TexCoord0[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := PointF(X / FSubdivisionsWidth, 1 - Y / FSubdivisionsDepth);
        end
        else
        begin
          FData.VertexBuffer.Vertices[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(-0.5 + (X / FSubdivisionsWidth), 0.5, -0.5 + (Y / FSubdivisionsDepth));
          FData.VertexBuffer.Normals[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(0, 1, 0);
          FData.VertexBuffer.TexCoord0[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := PointF(X / FSubdivisionsWidth, Y / FSubdivisionsDepth);
        end;
      end;
    for Y := 0 to FSubdivisionsDepth - 1 do
      for X := 0 to FSubdivisionsWidth - 1 do
      begin
        if Odd(Face) then
        begin
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 0] := VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 2] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 1] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 3] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 5] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 4] := VertexOffset + (Face * FaceVertexLength) + X + 1 + ((Y + 1) * (FSubdivisionsWidth + 1));
        end
        else
        begin
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 0] := VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 1] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 2] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 3] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 4] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 5] := VertexOffset + (Face * FaceVertexLength) + X + 1 + ((Y + 1) * (FSubdivisionsWidth + 1));
        end;
      end;
  end;
  VertexOffset := VertexOffset + FaceVertexLength * 2;
  IndexOffset := IndexOffset + FaceIndexLength * 2;
  FaceVertexLength := (FSubdivisionsDepth + 1) * (FSubdivisionsHeight + 1);
  FaceIndexLength := FSubdivisionsDepth * FSubdivisionsHeight * 6;
  for Face := 0 to 1 do
  begin
    for Y := 0 to FSubdivisionsDepth do
      for X := 0 to FSubdivisionsHeight do
      begin
        if Odd(Face) then
        begin
          FData.VertexBuffer.Vertices[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1))] := Point3D(-0.5, -0.5 + (X / FSubdivisionsHeight), -0.5 + (Y / FSubdivisionsDepth));
          FData.VertexBuffer.Normals[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1))] := Point3D(-1, 0, 0);
          FData.VertexBuffer.TexCoord0[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1))] := PointF(1 - Y / FSubdivisionsDepth, X / FSubdivisionsHeight);
        end
        else
        begin
          FData.VertexBuffer.Vertices[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1))] := Point3D(0.5, -0.5 + (X / FSubdivisionsHeight), -0.5 + (Y / FSubdivisionsDepth));
          FData.VertexBuffer.Normals[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1))] := Point3D(1, 0, 0);
          FData.VertexBuffer.TexCoord0[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1))] := PointF(Y / FSubdivisionsDepth, X / FSubdivisionsHeight);
        end;
      end;
    for Y := 0 to FSubdivisionsDepth - 1 do
      for X := 0 to FSubdivisionsHeight - 1 do
      begin
        if not Odd(Face) then
        begin
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 0] := VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 2] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsHeight + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 1] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsHeight + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 3] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsHeight + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 5] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsHeight + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 4] := VertexOffset + (Face * FaceVertexLength) + X + 1 + ((Y + 1) * (FSubdivisionsHeight + 1));
        end
        else
        begin
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 0] := VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 1] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsHeight + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 2] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsHeight + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 3] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsHeight + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 4] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsHeight + 1));
          FData.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 5] := VertexOffset + (Face * FaceVertexLength) + X + 1 + ((Y + 1) * (FSubdivisionsHeight + 1));
        end;
      end;
  end;
end;

function TCube.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
var
  NearIntersectionPoint, FarIntersectionPoint: TPoint3D;
  Side: Single;
begin
  // Calling inherited will search through the MeshData for intersection. This is
  // wasted effort for such a simple shape.
  Result := False;
  case WrapMode of
    TMeshWrapMode.Original:
    begin
        Result := RayCastCuboidIntersect(RayPos, RayDir, TPoint3D.Zero, Width, Height, Depth, NearIntersectionPoint,
          FarIntersectionPoint) > 0;
        if Result then
        Intersection := TPoint3D(LocalToAbsoluteVector(NearIntersectionPoint));
    end;
    TMeshWrapMode.Resize:
    begin
        Result := RayCastCuboidIntersect(RayPos, RayDir, TPoint3D.Zero, 1, 1, 1, NearIntersectionPoint,
          FarIntersectionPoint) > 0;
        if Result then
        Intersection := TPoint3D(LocalToAbsoluteVector(NearIntersectionPoint));
    end;
    TMeshWrapMode.Fit:
    begin
      Side := Min(Width, Height);
      Side := Min(Depth, Side);
      Result := RayCastCuboidIntersect(RayPos, RayDir, TPoint3D.Zero, Side, Side, Side, NearIntersectionPoint,
        FarIntersectionPoint) > 0;
      if Result then
        Intersection := TPoint3D(LocalToAbsoluteVector(NearIntersectionPoint));
    end;
    TMeshWrapMode.Stretch:
    begin
      Result := RayCastCuboidIntersect(RayPos, RayDir, TPoint3D.Zero, Width, Height, Depth, NearIntersectionPoint, FarIntersectionPoint) > 0;
      if Result then
        Intersection := TPoint3D(LocalToAbsoluteVector(NearIntersectionPoint));
    end;
  end;
end;

procedure TCube.SetSubdivisionsDepth(const Value: Integer);
begin
  if FSubdivisionsDepth <> Value then
  begin
    FSubdivisionsDepth := Value;
    if FSubdivisionsDepth < 1 then
      FSubdivisionsDepth := 1;
    RebuildMesh;
  end;
end;

procedure TCube.SetSubdivisionsHeight(const Value: Integer);
begin
  if FSubdivisionsHeight <> Value then
  begin
    FSubdivisionsHeight := Value;
    if FSubdivisionsHeight < 1 then
      FSubdivisionsHeight := 1;
    RebuildMesh;
  end;
end;

procedure TCube.SetSubdivisionsWidth(const Value: Integer);
begin
  if FSubdivisionsWidth <> Value then
  begin
    FSubdivisionsWidth := Value;
    if FSubdivisionsWidth < 1 then
      FSubdivisionsWidth := 1;
    RebuildMesh;
  end;
end;

{ TSphere }

constructor TSphere.Create(AOwner: TComponent);
begin
  inherited;
  FSubdivisionsAxes := 16;
  FSubdivisionsHeight := 12;
  RebuildMesh;
end;

procedure TSphere.RebuildMesh;
var
  A, H, AA, HH: Integer;
  Theta, Phi: Single;
  DTheta, DPhi: Single;
  ThetaSin, ThetaCos: Double;
  PhiSin, PhiCos: Double;
  IdxCount: Integer;
  VerticesWidth: Integer;
begin
  VerticesWidth := (FSubdivisionsAxes + 1);
  FData.VertexBuffer.Length := (FSubdivisionsHeight + 1) * VerticesWidth - 1;
  FData.IndexBuffer.Length := (FSubdivisionsHeight - 2) * FSubdivisionsAxes * 6 + (FSubdivisionsAxes * 3) + (FSubdivisionsAxes * 3);
  DTheta := DegToRad(180) / FSubdivisionsHeight;
  DPhi := DegToRad(360) / FSubdivisionsAxes;
  IdxCount := 0;
  // fill indices
  Theta := -DegToRad(90);
  for H := 0 to FSubdivisionsHeight - 1 do
  begin
    Phi := 0;
    for A := 0 to FSubdivisionsAxes - 1 do
    begin
      SinCos(Theta, ThetaSin, ThetaCos);
      SinCos(Phi, PhiSin, PhiCos);
      FData.VertexBuffer.Vertices[A + (H * VerticesWidth)] := Point3D(ThetaCos * PhiCos * 0.5, ThetaSin * 0.5, ThetaCos * PhiSin * 0.5);
      FData.VertexBuffer.TexCoord0[A + (H * VerticesWidth)] := PointF(A / FSubdivisionsAxes, H / FSubdivisionsHeight);
      FData.VertexBuffer.Normals[A + (H * VerticesWidth)] := Point3D(ThetaCos * PhiCos, ThetaSin, ThetaCos * PhiSin).Normalize;
      if A = 0 then
      begin
        FData.VertexBuffer.Vertices[FSubdivisionsAxes + (H * VerticesWidth)] := Point3D(ThetaCos * PhiCos * 0.5, ThetaSin * 0.5, ThetaCos * PhiSin * 0.5);
        FData.VertexBuffer.TexCoord0[FSubdivisionsAxes + (H * VerticesWidth)] := PointF(1, H / FSubdivisionsHeight);
        FData.VertexBuffer.Normals[FSubdivisionsAxes + (H * VerticesWidth)] := Point3D(ThetaCos * PhiCos, ThetaSin, ThetaCos * PhiSin).Normalize;
      end;
      AA := A + 1;
      HH := H + 1;
      if H = 0 then
      begin
        FData.VertexBuffer.TexCoord0[A + (H * VerticesWidth)] := PointF((A + 0.5) / FSubdivisionsAxes, 0);
        FData.IndexBuffer.Indices[IdxCount + 0] := A;
        FData.IndexBuffer.Indices[IdxCount + 1] := AA + (HH * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 2] := A + (HH * VerticesWidth);
        IdxCount := IdxCount + 3;
      end
      else
      if H = FSubdivisionsHeight - 1 then
      begin
        FData.VertexBuffer.Vertices[A + (FSubdivisionsHeight * VerticesWidth)] := Point3D(0, 0.5, 0);
        FData.VertexBuffer.TexCoord0[A + (FSubdivisionsHeight * VerticesWidth)] := PointF((A + 0.5) / FSubdivisionsAxes, 1);
        FData.VertexBuffer.Normals[A + (FSubdivisionsHeight * VerticesWidth)] := Point3D(0, 1.0, 0);

        FData.IndexBuffer.Indices[IdxCount + 0] := A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 1] := AA + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 2] := A + (HH * VerticesWidth);
        IdxCount := IdxCount + 3;
      end
      else
      begin
        FData.IndexBuffer.Indices[IdxCount + 0] := A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 1] := AA + (HH * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 2] := A + (HH * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 3] := A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 4] := AA + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 5] := AA + (HH * VerticesWidth);
        IdxCount := IdxCount + 6;
      end;
      Phi := Phi + DPhi;
    end;
    Theta := Theta + DTheta;
  end;
end;

function TSphere.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
var
  NearIntersectionPoint, FarIntersectionPoint: TPoint3D;
  LRadius: Single;
begin
  // Calling inherited will search through the MeshData for intersection. This is
  // wasted effort for such a simple shape.
  Result := False;
  case WrapMode of
    TMeshWrapMode.Original:
    begin
        Result := RayCastEllipsoidIntersect(RayPos, RayDir, TPoint3D.Zero, 0.5 * Width, 0.5 * Height, 0.5 * Depth,
          NearIntersectionPoint, FarIntersectionPoint) > 0;
        if Result then
        Intersection := TPoint3D(LocalToAbsoluteVector(NearIntersectionPoint));
    end;
    TMeshWrapMode.Resize:
    begin
        Result := RayCastEllipsoidIntersect(RayPos, RayDir, TPoint3D.Zero, 0.5, 0.5, 0.5,
          NearIntersectionPoint, FarIntersectionPoint) > 0;
        if Result then
        Intersection := TPoint3D(LocalToAbsoluteVector(NearIntersectionPoint));
    end;
    TMeshWrapMode.Fit:
    begin
      LRadius := Min(0.5 * Width, 0.5 * Height);
      LRadius := Min(0.5 * Depth, LRadius);
      Result := RayCastEllipsoidIntersect(RayPos, RayDir, TPoint3D.Zero, LRadius, LRadius, LRadius,
        NearIntersectionPoint, FarIntersectionPoint) > 0;
      if Result then
        Intersection := TPoint3D(LocalToAbsoluteVector(NearIntersectionPoint));
    end;
    TMeshWrapMode.Stretch:
    begin
      Result := RayCastEllipsoidIntersect(RayPos, RayDir, TPoint3D.Zero, Width / 2, Height / 2, Depth / 2,
        NearIntersectionPoint, FarIntersectionPoint) > 0;
      if Result then
        Intersection := TPoint3D(LocalToAbsoluteVector(NearIntersectionPoint));
    end;
  end;
end;

procedure TSphere.SetSubdivisionsAxes(const Value: Integer);
begin
  if FSubdivisionsAxes <> Value then
  begin
    FSubdivisionsAxes := Value;
    if FSubdivisionsAxes < 3 then
      FSubdivisionsAxes := 3;
    if FSubdivisionsAxes > 50 then
      FSubdivisionsAxes := 50;
    RebuildMesh;
  end;
end;

procedure TSphere.SetSubdivisionsHeight(const Value: Integer);
begin
  if FSubdivisionsHeight <> Value then
  begin
    FSubdivisionsHeight := Value;
    if FSubdivisionsHeight < 2 then
      FSubdivisionsHeight := 2;
    if FSubdivisionsHeight > 50 then
      FSubdivisionsHeight := 50;
    RebuildMesh;
  end;
end;

{ TCylinder }

constructor TCylinder.Create(AOwner: TComponent);
begin
  inherited;
  FSubdivisionsAxes := 12;
  FSubdivisionsCap := 1;
  FSubdivisionsHeight := 1;
  RebuildMesh;
end;

procedure TCylinder.RebuildMesh;
var
  A, H, AA, HH: Integer;
  Phi: Single;
  S, DPhi: Single;
  PhiSin, PhiCos: Double;
  IdxCount: Integer;
  Offset, VerticesWidth: Integer;
begin
  VerticesWidth := (FSubdivisionsAxes + 1);
  FData.VertexBuffer.Length := (FSubdivisionsCap * VerticesWidth + 1) * 2 + ((FSubdivisionsHeight + 1) * VerticesWidth);
  FData.IndexBuffer.Length := ((FSubdivisionsCap - 1) * FSubdivisionsAxes * 6 + (FSubdivisionsAxes * 3)) * 2 +
    FSubdivisionsHeight * FSubdivisionsAxes * 6;
  DPhi := DegToRad(360) / FSubdivisionsAxes;
  IdxCount := 0;
  // bottom and top
  FData.VertexBuffer.Vertices[FData.VertexBuffer.Length - 1] := Point3D(0, 0.5, 0);
  FData.VertexBuffer.TexCoord0[FData.VertexBuffer.Length - 1] := PointF(0.5, 0.5);
  FData.VertexBuffer.Normals[FData.VertexBuffer.Length - 1] := Point3D(0, 1, 0);
  FData.VertexBuffer.Vertices[FData.VertexBuffer.Length - 2] := Point3D(0, -0.5, 0);
  FData.VertexBuffer.TexCoord0[FData.VertexBuffer.Length - 2] := PointF(0.5, 0.5);
  FData.VertexBuffer.Normals[FData.VertexBuffer.Length - 2] := Point3D(0, -1, 0);
  Offset := VerticesWidth * FSubdivisionsCap;
  for H := 0 to FSubdivisionsCap - 1 do
  begin
    Phi := 0;
    for A := 0 to FSubdivisionsAxes - 1 do
    begin
      SinCos(Phi, PhiSin, PhiCos);
      // bottom
      FData.VertexBuffer.Vertices[A + (H * VerticesWidth)] := Point3D(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap), 0.5, (PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap)));
      FData.VertexBuffer.TexCoord0[A + (H * VerticesWidth)] := PointF(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5, 1 - (PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5));
      FData.VertexBuffer.Normals[A + (H * VerticesWidth)] := Point3D(0, 1, 0);
      if A = 0 then
      begin
        FData.VertexBuffer.Vertices[FSubdivisionsAxes + (H * VerticesWidth)] := Point3D(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap), 0.5, (PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap)));
        FData.VertexBuffer.TexCoord0[FSubdivisionsAxes + (H * VerticesWidth)] := PointF(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5, 1 - (PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5));
        FData.VertexBuffer.Normals[FSubdivisionsAxes + (H * VerticesWidth)] := Point3D(0, 1, 0);
      end;
      AA := A + 1;
      HH := H - 1;
      if H = 0 then
      begin
        FData.IndexBuffer.Indices[IdxCount + 0] := A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 2] := FData.VertexBuffer.Length - 1;
        FData.IndexBuffer.Indices[IdxCount + 1] := AA + (H * VerticesWidth);
        IdxCount := IdxCount + 3;
      end
      else
      begin
        FData.IndexBuffer.Indices[IdxCount + 0] := A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 1] := AA + (HH * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 2] := A + (HH * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 3] := A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 4] := AA + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 5] := AA + (HH * VerticesWidth);
        IdxCount := IdxCount + 6;
      end;
      // top
      FData.VertexBuffer.Vertices[Offset + A + (H * VerticesWidth)] := Point3D(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap), -0.5, (PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap)));
      FData.VertexBuffer.TexCoord0[Offset + A + (H * VerticesWidth)] := PointF(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5, 1 - (PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5));
      FData.VertexBuffer.Normals[Offset + A + (H * VerticesWidth)] := Point3D(0, -1, 0);
      if A = 0 then
      begin
        FData.VertexBuffer.Vertices[Offset + FSubdivisionsAxes + (H * VerticesWidth)] := Point3D(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap), -0.5, (PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap)));
        FData.VertexBuffer.TexCoord0[Offset + FSubdivisionsAxes + (H * VerticesWidth)] := PointF(PhiCos * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5, 1 - (PhiSin * 0.5 * ((H + 1) / FSubdivisionsCap) + 0.5));
        FData.VertexBuffer.Normals[Offset + FSubdivisionsAxes + (H * VerticesWidth)] := Point3D(0, -1, 0);
      end;
      if H = 0 then
      begin
        FData.IndexBuffer.Indices[IdxCount + 0] := Offset + A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 1] := FData.VertexBuffer.Length - 2;
        FData.IndexBuffer.Indices[IdxCount + 2] := Offset + AA + (H * VerticesWidth);
        IdxCount := IdxCount + 3;
      end
      else
      begin
        FData.IndexBuffer.Indices[IdxCount + 0] := Offset + A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 2] := Offset + AA + (HH * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 1] := Offset + A + (HH * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 3] := Offset + A + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 5] := Offset + AA + (H * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 4] := Offset + AA + (HH * VerticesWidth);
        IdxCount := IdxCount + 6;
      end;
      Phi := Phi + DPhi;
    end;
  end;
  // sides
  Offset := Offset + VerticesWidth * FSubdivisionsCap;
  for H := 0 to FSubdivisionsHeight - 1 do
  begin
    Phi := 0;
    for A := 0 to FSubdivisionsAxes - 1 do
    begin
      SinCos(Phi, PhiSin, PhiCos);
      S := 1 - (H / FSubdivisionsHeight);
      FData.VertexBuffer.Vertices[Offset + A + (H * VerticesWidth)] := Point3D(PhiCos * 0.5, 0.5 - (1 - S), PhiSin * 0.5);
      FData.VertexBuffer.TexCoord0[Offset + A + (H * VerticesWidth)] := PointF(A / FSubdivisionsAxes, S);
      FData.VertexBuffer.Normals[Offset + A + (H * VerticesWidth)] := Point3D(PhiCos, 0, PhiSin).Normalize;
      if H = 0 then
      begin
        FData.VertexBuffer.Vertices[Offset + A + (FSubdivisionsHeight * VerticesWidth)] := Point3D(PhiCos * 0.5, -0.5, PhiSin * 0.5);
        FData.VertexBuffer.TexCoord0[Offset + A + (FSubdivisionsHeight * VerticesWidth)] := PointF(A / FSubdivisionsAxes, 0);
        FData.VertexBuffer.Normals[Offset + A + (FSubdivisionsHeight * VerticesWidth)] := Point3D(PhiCos, 0, PhiSin).Normalize;
      end;
      if A = 0 then
      begin
        FData.VertexBuffer.Vertices[Offset + FSubdivisionsAxes + (H * VerticesWidth)] := FData.VertexBuffer.Vertices[Offset + (H * VerticesWidth)];
        FData.VertexBuffer.TexCoord0[Offset + FSubdivisionsAxes + (H * VerticesWidth)] := PointF(1, S);
        FData.VertexBuffer.Normals[Offset + FSubdivisionsAxes + (H * VerticesWidth)] := FData.VertexBuffer.Normals[Offset + (H * VerticesWidth)];
        if H = 0 then
        begin
          FData.VertexBuffer.Vertices[Offset + FSubdivisionsAxes + (FSubdivisionsHeight * VerticesWidth)] := Point3D(PhiCos * 0.5, -0.5, PhiSin * 0.5);
          FData.VertexBuffer.TexCoord0[Offset + FSubdivisionsAxes + (FSubdivisionsHeight * VerticesWidth)] := PointF(1, 0);
          FData.VertexBuffer.Normals[Offset + FSubdivisionsAxes + (FSubdivisionsHeight * VerticesWidth)] := Point3D(PhiCos, 0, PhiSin).Normalize;
        end;
      end;
      AA := A + 1;
      HH := H + 1;

      FData.IndexBuffer.Indices[IdxCount + 0] := Offset + A + (H * VerticesWidth);
      FData.IndexBuffer.Indices[IdxCount + 2] := Offset + AA + (HH * VerticesWidth);
      FData.IndexBuffer.Indices[IdxCount + 1] := Offset + A + (HH * VerticesWidth);
      FData.IndexBuffer.Indices[IdxCount + 3] := Offset + A + (H * VerticesWidth);
      FData.IndexBuffer.Indices[IdxCount + 5] := Offset + AA + (H * VerticesWidth);
      FData.IndexBuffer.Indices[IdxCount + 4] := Offset + AA + (HH * VerticesWidth);
      IdxCount := IdxCount + 6;

      Phi := Phi + DPhi;
    end;
  end;
end;

procedure TCylinder.SetSubdivisionsAxes(const Value: Integer);
begin
  if FSubdivisionsAxes <> Value then
  begin
    FSubdivisionsAxes := Value;
    if FSubdivisionsAxes < 3 then
      FSubdivisionsAxes := 3;
    RebuildMesh;
  end;
end;

procedure TCylinder.SetSubdivisionsCap(const Value: Integer);
begin
  if FSubdivisionsCap <> Value then
  begin
    FSubdivisionsCap := Value;
    if FSubdivisionsCap < 1 then
      FSubdivisionsCap := 1;
    RebuildMesh;
  end;
end;

procedure TCylinder.SetSubdivisionsHeight(const Value: Integer);
begin
  if FSubdivisionsHeight <> Value then
  begin
    FSubdivisionsHeight := Value;
    if FSubdivisionsHeight < 1 then
      FSubdivisionsHeight := 1;
    RebuildMesh;
  end;
end;

{ TRoundCube }

const

  RoundCubeVertices: array [0 .. 201] of TMeshVertex = ((X: - 0.42; Y: 0.42; z: 0.5; nx: - 0.0184349; ny: 0.00943183;
    nz: 1.5144; tu: 0.08; tv: 0.08;), (X: 0.42; Y: 0.42; z: 0.5; nx: 0.00943183; ny: 0.0184349; nz: 0.8088; tu: 0.92;
    tv: 0.08;), (X: - 0.42; Y: - 0.42; z: 0.5; nx: - 0.00943183; ny: - 0.0184349; nz: 0.8088; tu: 0.08; tv: 0.92;
    ), (X: 0.42; Y: - 0.42; z: 0.5; nx: 0.0184349; ny: - 0.00943183; nz: 1.5144; tu: 0.92; tv: 0.92;
    ), (X: - 0.42; Y: 0.46; z: 0.489282; nx: - 0.000486155; ny: 0.0444174; nz: 0.0941968; tu: 0.08; tv: 0.04;
    ), (X: - 0.44; Y: 0.454641; z: 0.489282; nx: - 0.00154256; ny: 0.0021282; nz: 0.00378564; tu: 0.06; tv: 0.045359;
    ), (X: - 0.454641; Y: 0.44; z: 0.489282; nx: - 0.00239999; ny: 0.00107179; nz: 0.00378564; tu: 0.045359;
    tv: 0.0599999;), (X: - 0.46; Y: 0.42; z: 0.489282; nx: - 0.0589968; ny: 0.000214357; nz: 0.0841794; tu: 0.04;
    tv: 0.08;), (X: - 0.46; Y: - 0.42; z: 0.489282; nx: - 0.0444174; ny: - 0.000486152; nz: 0.0941968; tu: 0.04;
    tv: 0.92;), (X: - 0.454641; Y: - 0.44; z: 0.489282; nx: - 0.0021282; ny: - 0.00154256; nz: 0.00378564; tu: 0.045359;
    tv: 0.94;), (X: - 0.44; Y: - 0.454641; z: 0.489282; nx: - 0.0010718; ny: - 0.0024; nz: 0.00378564; tu: 0.06;
    tv: 0.954641;), (X: - 0.42; Y: - 0.46; z: 0.489282; nx: - 0.00021436; ny: - 0.0589968; nz: 0.0841795; tu: 0.08;
    tv: 0.96;), (X: 0.42; Y: - 0.46; z: 0.489282; nx: 0.000486156; ny: - 0.0444174; nz: 0.0941969; tu: 0.92; tv: 0.96;
    ), (X: 0.44; Y: - 0.454641; z: 0.489282; nx: 0.00154256; ny: - 0.0021282; nz: 0.00378564; tu: 0.94; tv: 0.954641;
    ), (X: 0.454641; Y: - 0.44; z: 0.489282; nx: 0.0024; ny: - 0.0010718; nz: 0.00378564; tu: 0.954641; tv: 0.94;
    ), (X: 0.46; Y: - 0.42; z: 0.489282; nx: 0.0589968; ny: - 0.000214359; nz: 0.0841795; tu: 0.96; tv: 0.92;
    ), (X: 0.46; Y: 0.42; z: 0.489282; nx: 0.0444174; ny: 0.000486156; nz: 0.0941969; tu: 0.96; tv: 0.08;
    ), (X: 0.454641; Y: 0.44; z: 0.489282; nx: 0.0021282; ny: 0.00154256; nz: 0.00378564; tu: 0.954641; tv: 0.06;
    ), (X: 0.44; Y: 0.454641; z: 0.489282; nx: 0.00107179; ny: 0.0024; nz: 0.00378564; tu: 0.94; tv: 0.045359;
    ), (X: 0.42; Y: 0.46; z: 0.489282; nx: 0.000214357; ny: 0.0589968; nz: 0.0841794; tu: 0.92; tv: 0.04;
    ), (X: - 0.42; Y: 0.489282; z: 0.46; nx: - 0.00107179; ny: 0.0867937; nz: 0.0600113; tu: 0.08; tv: 0.010718;
    ), (X: - 0.454641; Y: 0.48; z: 0.46; nx: - 0.0037282; ny: 0.00591384; nz: 0.00378564; tu: 0.045359; tv: 0.02;
    ), (X: - 0.48; Y: 0.454641; z: 0.46; nx: - 0.00618564; ny: 0.00325743; nz: 0.00378564; tu: 0.02; tv: 0.045359;
    ), (X: - 0.489282; Y: 0.42; z: 0.46; nx: - 0.0947825; ny: 0.000799995; nz: 0.0445744; tu: 0.010718; tv: 0.08;
    ), (X: - 0.489282; Y: - 0.42; z: 0.46; nx: - 0.0867937; ny: - 0.00107179; nz: 0.0600113; tu: 0.010718; tv: 0.92;
    ), (X: - 0.48; Y: - 0.454641; z: 0.46; nx: - 0.00591384; ny: - 0.0037282; nz: 0.00378564; tu: 0.02; tv: 0.954641;
    ), (X: - 0.454641; Y: - 0.48; z: 0.46; nx: - 0.00325744; ny: - 0.00618564; nz: 0.00378564; tu: 0.045359; tv: 0.98;
    ), (X: - 0.42; Y: - 0.489282; z: 0.46; nx: - 0.000800001; ny: - 0.0947825; nz: 0.0445743; tu: 0.08; tv: 0.989282;
    ), (X: 0.42; Y: - 0.489282; z: 0.46; nx: 0.0010718; ny: - 0.0867937; nz: 0.0600112; tu: 0.92; tv: 0.989282;
    ), (X: 0.454641; Y: - 0.48; z: 0.46; nx: 0.0037282; ny: - 0.00591384; nz: 0.00378564; tu: 0.954641; tv: 0.98;
    ), (X: 0.48; Y: - 0.454641; z: 0.46; nx: 0.00618564; ny: - 0.00325743; nz: 0.00378564; tu: 0.98; tv: 0.954641;
    ), (X: 0.489282; Y: - 0.42; z: 0.46; nx: 0.0947825; ny: - 0.000800001; nz: 0.0445743; tu: 0.989282; tv: 0.92;
    ), (X: 0.489282; Y: 0.42; z: 0.46; nx: 0.0867937; ny: 0.0010718; nz: 0.0600112; tu: 0.989282; tv: 0.08;
    ), (X: 0.48; Y: 0.454641; z: 0.46; nx: 0.00591384; ny: 0.0037282; nz: 0.00378564; tu: 0.98; tv: 0.045359;
    ), (X: 0.454641; Y: 0.48; z: 0.46; nx: 0.00325743; ny: 0.00618564; nz: 0.00378564; tu: 0.954641; tv: 0.02;
    ), (X: 0.42; Y: 0.489282; z: 0.46; nx: 0.000799995; ny: 0.0947825; nz: 0.0445744; tu: 0.92; tv: 0.010718;
    ), (X: - 0.42; Y: 0.5; z: 0.42; nx: - 0.00042872; ny: 0.0688001; nz: 0.0184349; tu: 0.08; tv: 0;
    ), (X: - 0.46; Y: 0.489282; z: 0.42; nx: - 0.00197128; ny: 0.00415692; nz: 0.00122871; tu: 0.04; tv: 0.010718;
    ), (X: - 0.489282; Y: 0.46; z: 0.42; nx: - 0.00378564; ny: 0.00261436; nz: 0.00122871; tu: 0.010718; tv: 0.04;
    ), (X: - 0.5; Y: 0.42; z: 0.42; nx: - 0.0365857; ny: 0.0008; nz: 0.00980311; tu: 0; tv: 0.08;
    ), (X: - 0.5; Y: - 0.42; z: 0.42; nx: - 0.0688001; ny: - 0.00042872; nz: 0.0184349; tu: 0; tv: 0.92;
    ), (X: - 0.489282; Y: - 0.46; z: 0.42; nx: - 0.00415693; ny: - 0.00197128; nz: 0.00122872; tu: 0.010718; tv: 0.96;
    ), (X: - 0.46; Y: - 0.489282; z: 0.42; nx: - 0.00261436; ny: - 0.00378564; nz: 0.00122872; tu: 0.04; tv: 0.989282;
    ), (X: - 0.42; Y: - 0.5; z: 0.42; nx: - 0.0008; ny: - 0.0365857; nz: 0.00980306; tu: 0.08; tv: 1;
    ), (X: 0.42; Y: - 0.5; z: 0.42; nx: 0.000428717; ny: - 0.0688001; nz: 0.0184348; tu: 0.92; tv: 1;
    ), (X: 0.46; Y: - 0.489282; z: 0.42; nx: 0.00197128; ny: - 0.00415692; nz: 0.00122872; tu: 0.96; tv: 0.989282;
    ), (X: 0.489282; Y: - 0.46; z: 0.42; nx: 0.00378564; ny: - 0.00261436; nz: 0.00122872; tu: 0.989282; tv: 0.96;
    ), (X: 0.5; Y: - 0.42; z: 0.42; nx: 0.0365857; ny: - 0.0008; nz: 0.00980306; tu: 1; tv: 0.92;
    ), (X: 0.5; Y: 0.42; z: 0.42; nx: 0.0688; ny: 0.000428717; nz: 0.0184348; tu: 1; tv: 0.08;
    ), (X: 0.489282; Y: 0.46; z: 0.42; nx: 0.00415692; ny: 0.00197128; nz: 0.00122872; tu: 0.989282; tv: 0.04;
    ), (X: 0.46; Y: 0.489282; z: 0.42; nx: 0.00261436; ny: 0.00378564; nz: 0.00122872; tu: 0.96; tv: 0.010718;
    ), (X: 0.42; Y: 0.5; z: 0.42; nx: 0.0008; ny: 0.0365857; nz: 0.00980311; tu: 0.92; tv: 0;
    ), (X: - 0.42; Y: 0.5; z: - 0.42; nx: - 0.00900311; ny: 0.0336; nz: 0; tu: 0; tv: 1;
    ), (X: - 0.46; Y: 0.489282; z: - 0.42; nx: - 0.0426031; ny: 0.0917968; nz: 0; tu: 0.00991422; tv: 1;
    ), (X: - 0.489282; Y: 0.46; z: - 0.42; nx: - 0.0827938; ny: 0.0581969; nz: 0; tu: 0.0198284; tv: 1;
    ), (X: - 0.5; Y: 0.42; z: - 0.42; nx: - 0.7728; ny: 0.0180062; nz: 0; tu: 0.0297427; tv: 1;
    ), (X: - 0.5; Y: - 0.42; z: - 0.42; nx: - 1.4448; ny: - 0.00900311; nz: 0; tu: 0.230847; tv: 1;
    ), (X: - 0.489282; Y: - 0.46; z: - 0.42; nx: - 0.0917969; ny: - 0.0426031; nz: 0; tu: 0.240761; tv: 1;
    ), (X: - 0.46; Y: - 0.489282; z: - 0.42; nx: - 0.0581969; ny: - 0.0827938; nz: 0; tu: 0.250676; tv: 1;
    ), (X: - 0.42; Y: - 0.5; z: - 0.42; nx: - 0.0180061; ny: - 0.7728; nz: 0; tu: 0.26059; tv: 1;
    ), (X: 0.42; Y: - 0.5; z: - 0.42; nx: 0.00900306; ny: - 1.4448; nz: 0; tu: 0.461694; tv: 1;
    ), (X: 0.46; Y: - 0.489282; z: - 0.42; nx: 0.042603; ny: - 0.0917969; nz: 0; tu: 0.471609; tv: 1;
    ), (X: 0.489282; Y: - 0.46; z: - 0.42; nx: 0.0827938; ny: - 0.0581969; nz: 0; tu: 0.481523; tv: 1;
    ), (X: 0.5; Y: - 0.42; z: - 0.42; nx: 0.7728; ny: - 0.0180061; nz: 0; tu: 0.491437; tv: 1;
    ), (X: 0.5; Y: 0.42; z: - 0.42; nx: 1.4448; ny: 0.00900306; nz: 0; tu: 0.692542; tv: 1;
    ), (X: 0.489282; Y: 0.46; z: - 0.42; nx: 0.0917968; ny: 0.042603; nz: 0; tu: 0.702456; tv: 1;
    ), (X: 0.46; Y: 0.489282; z: - 0.42; nx: 0.0581969; ny: 0.0827938; nz: 0; tu: 0.71237; tv: 1;
    ), (X: 0.42; Y: 0.5; z: - 0.42; nx: 0.0180062; ny: 0.7728; nz: 0; tu: 0.722284; tv: 1;
    ), (X: - 0.42; Y: 0.489282; z: - 0.46; nx: - 0.000799998; ny: 0.0947825; nz: - 0.0445744; tu: 0.08; tv: 0.989282;
    ), (X: - 0.454641; Y: 0.48; z: - 0.46; nx: - 0.00325744; ny: 0.00618564; nz: - 0.00378564; tu: 0.045359; tv: 0.98;
    ), (X: - 0.48; Y: 0.454641; z: - 0.46; nx: - 0.00591384; ny: 0.0037282; nz: - 0.00378563; tu: 0.02; tv: 0.954641;
    ), (X: - 0.489282; Y: 0.42; z: - 0.46; nx: - 0.0867938; ny: 0.00107179; nz: - 0.0600113; tu: 0.010718; tv: 0.92;
    ), (X: - 0.489282; Y: - 0.42; z: - 0.46; nx: - 0.0947825; ny: - 0.000799996; nz: - 0.0445744; tu: 0.010718;
    tv: 0.08;), (X: - 0.48; Y: - 0.454641; z: - 0.46; nx: - 0.00618564; ny: - 0.00325744; nz: - 0.00378564; tu: 0.02;
    tv: 0.045359;), (X: - 0.454641; Y: - 0.48; z: - 0.46; nx: - 0.0037282; ny: - 0.00591384; nz: - 0.00378564;
    tu: 0.045359; tv: 0.02;), (X: - 0.42; Y: - 0.489282; z: - 0.46; nx: - 0.0010718; ny: - 0.0867938; nz: - 0.0600112;
    tu: 0.08; tv: 0.010718;), (X: 0.42; Y: - 0.489282; z: - 0.46; nx: 0.000800002; ny: - 0.0947825; nz: - 0.0445743;
    tu: 0.92; tv: 0.010718;), (X: 0.454641; Y: - 0.48; z: - 0.46; nx: 0.00325744; ny: - 0.00618564; nz: - 0.00378564;
    tu: 0.954641; tv: 0.02;), (X: 0.48; Y: - 0.454641; z: - 0.46; nx: 0.00591384; ny: - 0.0037282; nz: - 0.00378564;
    tu: 0.98; tv: 0.045359;), (X: 0.489282; Y: - 0.42; z: - 0.46; nx: 0.0867938; ny: - 0.0010718; nz: - 0.0600112;
    tu: 0.989282; tv: 0.08;), (X: 0.489282; Y: 0.42; z: - 0.46; nx: 0.0947825; ny: 0.000800001; nz: - 0.0445743;
    tu: 0.989282; tv: 0.92;), (X: 0.48; Y: 0.454641; z: - 0.46; nx: 0.00618564; ny: 0.00325744; nz: - 0.00378564;
    tu: 0.98; tv: 0.954641;), (X: 0.454641; Y: 0.48; z: - 0.46; nx: 0.0037282; ny: 0.00591384; nz: - 0.00378564;
    tu: 0.954641; tv: 0.98;), (X: 0.42; Y: 0.489282; z: - 0.46; nx: 0.00107179; ny: 0.0867938; nz: - 0.0600113;
    tu: 0.92; tv: 0.989282;), (X: - 0.42; Y: 0.46; z: - 0.489282; nx: - 0.00021436; ny: 0.0589969; nz: - 0.0841794;
    tu: 0.08; tv: 0.96;), (X: - 0.44; Y: 0.454641; z: - 0.489282; nx: - 0.00107179; ny: 0.0024; nz: - 0.00378564;
    tu: 0.06; tv: 0.954641;), (X: - 0.454641; Y: 0.44; z: - 0.489282; nx: - 0.0021282; ny: 0.00154256; nz: - 0.00378564;
    tu: 0.045359; tv: 0.94;), (X: - 0.46; Y: 0.42; z: - 0.489282; nx: - 0.0444174; ny: 0.000486153; nz: - 0.0941968;
    tu: 0.04; tv: 0.92;), (X: - 0.46; Y: - 0.42; z: - 0.489282; nx: - 0.0589969; ny: - 0.000214357; nz: - 0.0841794;
    tu: 0.04; tv: 0.08;), (X: - 0.454641; Y: - 0.44; z: - 0.489282; nx: - 0.0024; ny: - 0.00107179; nz: - 0.00378564;
    tu: 0.045359; tv: 0.06;), (X: - 0.44; Y: - 0.454641; z: - 0.489282; nx: - 0.00154256; ny: - 0.0021282;
    nz: - 0.00378564; tu: 0.06; tv: 0.045359;), (X: - 0.42; Y: - 0.46; z: - 0.489282; nx: - 0.000486157;
    ny: - 0.0444174; nz: - 0.0941969; tu: 0.08; tv: 0.04;), (X: 0.42; Y: - 0.46; z: - 0.489282; nx: 0.00021436;
    ny: - 0.0589969; nz: - 0.0841795; tu: 0.92; tv: 0.04;), (X: 0.44; Y: - 0.454641; z: - 0.489282; nx: 0.0010718;
    ny: - 0.0024; nz: - 0.00378564; tu: 0.94; tv: 0.045359;), (X: 0.454641; Y: - 0.44; z: - 0.489282; nx: 0.0021282;
    ny: - 0.00154256; nz: - 0.00378564; tu: 0.954641; tv: 0.06;), (X: 0.46; Y: - 0.42; z: - 0.489282; nx: 0.0444174;
    ny: - 0.000486157; nz: - 0.0941969; tu: 0.96; tv: 0.08;), (X: 0.46; Y: 0.42; z: - 0.489282; nx: 0.0589969;
    ny: 0.000214359; nz: - 0.0841795; tu: 0.96; tv: 0.92;), (X: 0.454641; Y: 0.44; z: - 0.489282; nx: 0.0024;
    ny: 0.0010718; nz: - 0.00378564; tu: 0.954641; tv: 0.94;), (X: 0.44; Y: 0.454641; z: - 0.489282; nx: 0.00154256;
    ny: 0.0021282; nz: - 0.00378564; tu: 0.94; tv: 0.954641;), (X: 0.42; Y: 0.46; z: - 0.489282; nx: 0.000486153;
    ny: 0.0444174; nz: - 0.0941968; tu: 0.92; tv: 0.96;), (X: 0.42; Y: - 0.42; z: - 0.5; nx: 0.0094318; ny: - 0.0184349;
    nz: - 0.8088; tu: 0.92; tv: 0.08;), (X: - 0.42; Y: - 0.42; z: - 0.5; nx: - 0.0184349; ny: - 0.0094318; nz: - 1.5144;
    tu: 0.08; tv: 0.08;), (X: 0.42; Y: 0.42; z: - 0.5; nx: 0.0184349; ny: 0.0094318; nz: - 1.5144; tu: 0.92; tv: 0.92;
    ), (X: - 0.42; Y: 0.42; z: - 0.5; nx: - 0.0094318; ny: 0.0184349; nz: - 0.8088; tu: 0.08; tv: 0.92;
    ), (X: - 0.42; Y: 0.5; z: 0.42; nx: - 0.00900311; ny: 0.0336; nz: 0; tu: 0; tv: 0;
    ), (X: - 0.42; Y: 0.5; z: 0.42; nx: - 0.00900311; ny: 0.0336; nz: 0; tu: 0; tv: 0;
    ), (X: - 0.46; Y: 0.489282; z: 0.42; nx: - 0.00900311; ny: 0.0336; nz: 0; tu: 0.00991422; tv: 0;
    ), (X: - 0.46; Y: 0.489282; z: 0.42; nx: - 0.0245969; ny: 0.0245969; nz: 0; tu: 0.00991422; tv: 0;
    ), (X: - 0.46; Y: 0.489282; z: 0.42; nx: - 0.0245969; ny: 0.0245969; nz: 0; tu: 0.00991422; tv: 0;
    ), (X: - 0.489282; Y: 0.46; z: 0.42; nx: - 0.0245969; ny: 0.0245969; nz: 0; tu: 0.0198284; tv: 0;
    ), (X: - 0.489282; Y: 0.46; z: 0.42; nx: - 0.0336; ny: 0.00900311; nz: 0; tu: 0.0198284; tv: 0;
    ), (X: - 0.489282; Y: 0.46; z: 0.42; nx: - 0.0336; ny: 0.00900311; nz: 0; tu: 0.0198284; tv: 0;
    ), (X: - 0.5; Y: 0.42; z: 0.42; nx: - 0.0336; ny: 0.00900311; nz: 0; tu: 0.0297427; tv: 0;
    ), (X: - 0.5; Y: 0.42; z: 0.42; nx: - 0.7056; ny: 0; nz: 0; tu: 0.0297427; tv: 0;
    ), (X: - 0.5; Y: 0.42; z: 0.42; nx: - 0.7056; ny: 0; nz: 0; tu: 0.0297427; tv: 0;
    ), (X: - 0.5; Y: - 0.42; z: 0.42; nx: - 0.7056; ny: 0; nz: 0; tu: 0.230847; tv: 0;
    ), (X: - 0.5; Y: - 0.42; z: 0.42; nx: - 0.0336; ny: - 0.00900311; nz: 0; tu: 0.230847; tv: 0;
    ), (X: - 0.5; Y: - 0.42; z: 0.42; nx: - 0.0336; ny: - 0.00900311; nz: 0; tu: 0.230847; tv: 0;
    ), (X: - 0.489282; Y: - 0.46; z: 0.42; nx: - 0.0336; ny: - 0.00900311; nz: 0; tu: 0.240761; tv: 0;
    ), (X: - 0.489282; Y: - 0.46; z: 0.42; nx: - 0.0245969; ny: - 0.0245969; nz: 0; tu: 0.240761; tv: 0;
    ), (X: - 0.489282; Y: - 0.46; z: 0.42; nx: - 0.0245969; ny: - 0.0245969; nz: 0; tu: 0.240761; tv: 0;
    ), (X: - 0.46; Y: - 0.489282; z: 0.42; nx: - 0.0245969; ny: - 0.0245969; nz: 0; tu: 0.250676; tv: 0;
    ), (X: - 0.46; Y: - 0.489282; z: 0.42; nx: - 0.00900306; ny: - 0.0336; nz: 0; tu: 0.250676; tv: 0;
    ), (X: - 0.46; Y: - 0.489282; z: 0.42; nx: - 0.00900306; ny: - 0.0336; nz: 0; tu: 0.250676; tv: 0;
    ), (X: - 0.42; Y: - 0.5; z: 0.42; nx: - 0.00900306; ny: - 0.0336; nz: 0; tu: 0.26059; tv: 0;
    ), (X: - 0.42; Y: - 0.5; z: 0.42; nx: 0; ny: - 0.7056; nz: 0; tu: 0.26059; tv: 0;
    ), (X: - 0.42; Y: - 0.5; z: 0.42; nx: 0; ny: - 0.7056; nz: 0; tu: 0.26059; tv: 0;
    ), (X: 0.42; Y: - 0.5; z: 0.42; nx: 0; ny: - 0.7056; nz: 0; tu: 0.461694; tv: 0;
    ), (X: 0.42; Y: - 0.5; z: 0.42; nx: 0.00900306; ny: - 0.0336; nz: 0; tu: 0.461694; tv: 0;
    ), (X: 0.42; Y: - 0.5; z: 0.42; nx: 0.00900306; ny: - 0.0336; nz: 0; tu: 0.461694; tv: 0;
    ), (X: 0.46; Y: - 0.489282; z: 0.42; nx: 0.00900306; ny: - 0.0336; nz: 0; tu: 0.471609; tv: 0;
    ), (X: 0.46; Y: - 0.489282; z: 0.42; nx: 0.0245969; ny: - 0.0245969; nz: 0; tu: 0.471609; tv: 0;
    ), (X: 0.46; Y: - 0.489282; z: 0.42; nx: 0.0245969; ny: - 0.0245969; nz: 0; tu: 0.471609; tv: 0;
    ), (X: 0.489282; Y: - 0.46; z: 0.42; nx: 0.0245969; ny: - 0.0245969; nz: 0; tu: 0.481523; tv: 0;
    ), (X: 0.489282; Y: - 0.46; z: 0.42; nx: 0.0336; ny: - 0.00900306; nz: 0; tu: 0.481523; tv: 0;
    ), (X: 0.489282; Y: - 0.46; z: 0.42; nx: 0.0336; ny: - 0.00900306; nz: 0; tu: 0.481523; tv: 0;
    ), (X: 0.5; Y: - 0.42; z: 0.42; nx: 0.0336; ny: - 0.00900306; nz: 0; tu: 0.491437; tv: 0;
    ), (X: 0.5; Y: - 0.42; z: 0.42; nx: 0.7056; ny: 0; nz: 0; tu: 0.491437; tv: 0;
    ), (X: 0.5; Y: - 0.42; z: 0.42; nx: 0.7056; ny: 0; nz: 0; tu: 0.491437; tv: 0;
    ), (X: 0.5; Y: 0.42; z: 0.42; nx: 0.7056; ny: 0; nz: 0; tu: 0.692542; tv: 0;
    ), (X: 0.5; Y: 0.42; z: 0.42; nx: 0.0336; ny: 0.00900306; nz: 0; tu: 0.692542; tv: 0;
    ), (X: 0.5; Y: 0.42; z: 0.42; nx: 0.0336; ny: 0.00900306; nz: 0; tu: 0.692542; tv: 0;
    ), (X: 0.489282; Y: 0.46; z: 0.42; nx: 0.0336; ny: 0.00900306; nz: 0; tu: 0.702456; tv: 0;
    ), (X: 0.489282; Y: 0.46; z: 0.42; nx: 0.0245969; ny: 0.0245969; nz: 0; tu: 0.702456; tv: 0;
    ), (X: 0.489282; Y: 0.46; z: 0.42; nx: 0.0245969; ny: 0.0245969; nz: 0; tu: 0.702456; tv: 0;
    ), (X: 0.46; Y: 0.489282; z: 0.42; nx: 0.0245969; ny: 0.0245969; nz: 0; tu: 0.71237; tv: 0;
    ), (X: 0.46; Y: 0.489282; z: 0.42; nx: 0.00900311; ny: 0.0336; nz: 0; tu: 0.71237; tv: 0;
    ), (X: 0.46; Y: 0.489282; z: 0.42; nx: 0.00900311; ny: 0.0336; nz: 0; tu: 0.71237; tv: 0;
    ), (X: 0.42; Y: 0.5; z: 0.42; nx: 0.00900311; ny: 0.0336; nz: 0; tu: 0.722284; tv: 0;
    ), (X: 0.42; Y: 0.5; z: 0.42; nx: 0; ny: 0.7056; nz: 0; tu: 0.722284; tv: 0;
    ), (X: - 0.42; Y: 0.5; z: - 0.42; nx: 0; ny: 0.7056; nz: 0; tu: 1; tv: 1;
    ), (X: 0.42; Y: 0.5; z: 0.42; nx: 0; ny: 0.7056; nz: 0; tu: 0.722284; tv: 0;
    ), (X: - 0.42; Y: 0.5; z: - 0.42; nx: 0; ny: 0.7056; nz: 0; tu: 1; tv: 1;
    ), (X: - 0.42; Y: 0.5; z: 0.42; nx: 0; ny: 0.7056; nz: 0; tu: 1; tv: 0;
    ), (X: - 0.42; Y: 0.5; z: - 0.42; nx: - 0.00037128; ny: 0.00138564; nz: - 0.000371282; tu: 0.08; tv: 1;
    ), (X: - 0.42; Y: 0.5; z: - 0.42; nx: - 0.00042872; ny: 0.0016; nz: - 0.000428716; tu: 0.08; tv: 1;
    ), (X: - 0.46; Y: 0.489282; z: - 0.42; nx: - 0.00042872; ny: 0.0016; nz: - 0.000428716; tu: 0.04; tv: 0.989282;
    ), (X: - 0.46; Y: 0.489282; z: - 0.42; nx: - 0.00101436; ny: 0.00101436; nz: - 0.000371279; tu: 0.04; tv: 0.989282;
    ), (X: - 0.46; Y: 0.489282; z: - 0.42; nx: - 0.00117128; ny: 0.00117128; nz: - 0.000428718; tu: 0.04; tv: 0.989282;
    ), (X: - 0.489282; Y: 0.46; z: - 0.42; nx: - 0.00117128; ny: 0.00117128; nz: - 0.000428718; tu: 0.010718; tv: 0.96;
    ), (X: - 0.489282; Y: 0.46; z: - 0.42; nx: - 0.00138564; ny: 0.00037128; nz: - 0.000371279; tu: 0.010718; tv: 0.96;
    ), (X: - 0.489282; Y: 0.46; z: - 0.42; nx: - 0.0016; ny: 0.000428719; nz: - 0.000428719; tu: 0.010718; tv: 0.96;
    ), (X: - 0.5; Y: 0.42; z: - 0.42; nx: - 0.0016; ny: 0.000428719; nz: - 0.000428719; tu: 0; tv: 0.92;
    ), (X: - 0.5; Y: 0.42; z: - 0.42; nx: - 0.0336; ny: 0; nz: - 0.00900311; tu: 0; tv: 0.92;
    ), (X: - 0.5; Y: 0.42; z: - 0.42; nx: - 0.0336; ny: 0; nz: - 0.00900311; tu: 0; tv: 0.92;
    ), (X: - 0.5; Y: - 0.42; z: - 0.42; nx: - 0.0336; ny: 0; nz: - 0.00900311; tu: 0; tv: 0.08;
    ), (X: - 0.5; Y: - 0.42; z: - 0.42; nx: - 0.00138564; ny: - 0.00037128; nz: - 0.000371282; tu: 0; tv: 0.08;
    ), (X: - 0.5; Y: - 0.42; z: - 0.42; nx: - 0.0016; ny: - 0.00042872; nz: - 0.000428718; tu: 0; tv: 0.08;
    ), (X: - 0.489282; Y: - 0.46; z: - 0.42; nx: - 0.0016; ny: - 0.00042872; nz: - 0.000428718; tu: 0.010718; tv: 0.04;
    ), (X: - 0.489282; Y: - 0.46; z: - 0.42; nx: - 0.00101436; ny: - 0.00101436; nz: - 0.00037128; tu: 0.010718;
    tv: 0.04;), (X: - 0.489282; Y: - 0.46; z: - 0.42; nx: - 0.00117128; ny: - 0.00117128; nz: - 0.000428718;
    tu: 0.010718; tv: 0.04;), (X: - 0.46; Y: - 0.489282; z: - 0.42; nx: - 0.00117128; ny: - 0.00117128;
    nz: - 0.000428718; tu: 0.04; tv: 0.010718;), (X: - 0.46; Y: - 0.489282; z: - 0.42; nx: - 0.000371282;
    ny: - 0.00138564; nz: - 0.000371282; tu: 0.04; tv: 0.010718;), (X: - 0.46; Y: - 0.489282; z: - 0.42;
    nx: - 0.000428717; ny: - 0.0016; nz: - 0.000428717; tu: 0.04; tv: 0.010718;
    ), (X: - 0.42; Y: - 0.5; z: - 0.42; nx: - 0.000428717; ny: - 0.0016; nz: - 0.000428717; tu: 0.08; tv: 0;
    ), (X: - 0.42; Y: - 0.5; z: - 0.42; nx: 0; ny: - 0.0336; nz: - 0.00900306; tu: 0.08; tv: 0;
    ), (X: - 0.42; Y: - 0.5; z: - 0.42; nx: 0; ny: - 0.0336; nz: - 0.00900306; tu: 0.08; tv: 0;
    ), (X: 0.42; Y: - 0.5; z: - 0.42; nx: 0; ny: - 0.0336; nz: - 0.00900306; tu: 0.92; tv: 0;
    ), (X: 0.42; Y: - 0.5; z: - 0.42; nx: 0.000371282; ny: - 0.00138564; nz: - 0.00037128; tu: 0.92; tv: 0;
    ), (X: 0.42; Y: - 0.5; z: - 0.42; nx: 0.000428717; ny: - 0.0016; nz: - 0.000428719; tu: 0.92; tv: 0;
    ), (X: 0.46; Y: - 0.489282; z: - 0.42; nx: 0.000428717; ny: - 0.0016; nz: - 0.000428719; tu: 0.96; tv: 0.010718;
    ), (X: 0.46; Y: - 0.489282; z: - 0.42; nx: 0.00101436; ny: - 0.00101436; nz: - 0.000371281; tu: 0.96; tv: 0.010718;
    ), (X: 0.46; Y: - 0.489282; z: - 0.42; nx: 0.00117128; ny: - 0.00117128; nz: - 0.00042872; tu: 0.96; tv: 0.010718;
    ), (X: 0.489282; Y: - 0.46; z: - 0.42; nx: 0.00117128; ny: - 0.00117128; nz: - 0.00042872; tu: 0.989282; tv: 0.04;
    ), (X: 0.489282; Y: - 0.46; z: - 0.42; nx: 0.00138564; ny: - 0.000371282; nz: - 0.000371282; tu: 0.989282; tv: 0.04;
    ), (X: 0.489282; Y: - 0.46; z: - 0.42; nx: 0.0016; ny: - 0.000428717; nz: - 0.000428717; tu: 0.989282; tv: 0.04;
    ), (X: 0.5; Y: - 0.42; z: - 0.42; nx: 0.0016; ny: - 0.000428717; nz: - 0.000428717; tu: 1; tv: 0.08;
    ), (X: 0.5; Y: - 0.42; z: - 0.42; nx: 0.0336; ny: 0; nz: - 0.00900306; tu: 1; tv: 0.08;
    ), (X: 0.5; Y: - 0.42; z: - 0.42; nx: 0.0336; ny: 0; nz: - 0.00900306; tu: 1; tv: 0.08;
    ), (X: 0.5; Y: 0.42; z: - 0.42; nx: 0.0336; ny: 0; nz: - 0.00900306; tu: 1; tv: 0.92;
    ), (X: 0.5; Y: 0.42; z: - 0.42; nx: 0.00138564; ny: 0.000371282; nz: - 0.00037128; tu: 1; tv: 0.92;
    ), (X: 0.5; Y: 0.42; z: - 0.42; nx: 0.0016; ny: 0.000428717; nz: - 0.000428718; tu: 1; tv: 0.92;
    ), (X: 0.489282; Y: 0.46; z: - 0.42; nx: 0.0016; ny: 0.000428717; nz: - 0.000428718; tu: 0.989282; tv: 0.96;
    ), (X: 0.489282; Y: 0.46; z: - 0.42; nx: 0.00101436; ny: 0.00101436; nz: - 0.00037128; tu: 0.989282; tv: 0.96;
    ), (X: 0.489282; Y: 0.46; z: - 0.42; nx: 0.00117128; ny: 0.00117128; nz: - 0.000428717; tu: 0.989282; tv: 0.96;
    ), (X: 0.46; Y: 0.489282; z: - 0.42; nx: 0.00117128; ny: 0.00117128; nz: - 0.000428717; tu: 0.96; tv: 0.989282;
    ), (X: 0.46; Y: 0.489282; z: - 0.42; nx: 0.00037128; ny: 0.00138564; nz: - 0.00037128; tu: 0.96; tv: 0.989282;
    ), (X: 0.46; Y: 0.489282; z: - 0.42; nx: 0.000428719; ny: 0.0016; nz: - 0.00042872; tu: 0.96; tv: 0.989282;
    ), (X: 0.42; Y: 0.5; z: - 0.42; nx: 0.000428719; ny: 0.0016; nz: - 0.00042872; tu: 0.92; tv: 1;
    ), (X: 0.42; Y: 0.5; z: - 0.42; nx: 0; ny: 0.0336; nz: - 0.00900311; tu: 0.92; tv: 1;
    ), (X: 0.42; Y: 0.5; z: - 0.42; nx: 0; ny: 0.0336; nz: - 0.00900311; tu: 0.92; tv: 1;
    ), (X: - 0.42; Y: 0.5; z: - 0.42; nx: 0; ny: 0.0336; nz: - 0.00900311; tu: 0.08; tv: 1;));

  RoundCubeIndices: array [0 .. 611] of Cardinal = (0, 3, 2, 0, 1, 3, 0, 5, 4, 0, 6, 5, 0, 7, 6, 0, 8, 7, 0, 2, 8, 2, 9, 8,
    2, 10, 9, 2, 11, 10, 2, 12, 11, 2, 3, 12, 3, 13, 12, 3, 14, 13, 3, 15, 14, 3, 16, 15, 3, 1, 16, 1, 17, 16, 1, 18,
    17, 1, 19, 18, 1, 4, 19, 1, 0, 4, 4, 21, 20, 4, 5, 21, 5, 22, 21, 5, 6, 22, 6, 23, 22, 6, 7, 23, 7, 24, 23, 7, 8,
    24, 8, 25, 24, 8, 9, 25, 9, 26, 25, 9, 10, 26, 10, 27, 26, 10, 11, 27, 11, 28, 27, 11, 12, 28, 12, 29, 28, 12, 13,
    29, 13, 30, 29, 13, 14, 30, 14, 31, 30, 14, 15, 31, 15, 32, 31, 15, 16, 32, 16, 33, 32, 16, 17, 33, 17, 34, 33, 17,
    18, 34, 18, 35, 34, 18, 19, 35, 19, 20, 35, 19, 4, 20, 20, 37, 36, 20, 21, 37, 21, 38, 37, 21, 22, 38, 22, 39, 38,
    22, 23, 39, 23, 40, 39, 23, 24, 40, 24, 41, 40, 24, 25, 41, 25, 42, 41, 25, 26, 42, 26, 43, 42, 26, 27, 43, 27, 44,
    43, 27, 28, 44, 28, 45, 44, 28, 29, 45, 29, 46, 45, 29, 30, 46, 30, 47, 46, 30, 31, 47, 31, 48, 47, 31, 32, 48, 32,
    49, 48, 32, 33, 49, 33, 50, 49, 33, 34, 50, 34, 51, 50, 34, 35, 51, 35, 36, 51, 35, 20, 36, 104, 53, 52, 105, 106,
    53, 107, 54, 53, 108, 109, 54, 110, 55, 54, 111, 112, 55, 113, 56, 55, 114, 115, 56, 116, 57, 56, 117, 118, 57, 119,
    58, 57, 120, 121, 58, 122, 59, 58, 123, 124, 59, 125, 60, 59, 126, 127, 60, 128, 61, 60, 129, 130, 61, 131, 62, 61,
    132, 133, 62, 134, 63, 62, 135, 136, 63, 137, 64, 63, 138, 139, 64, 140, 65, 64, 141, 142, 65, 143, 66, 65, 144,
    145, 66, 146, 67, 66, 147, 148, 67, 149, 150, 67, 151, 153, 152, 154, 69, 68, 155, 156, 69, 157, 70, 69, 158, 159,
    70, 160, 71, 70, 161, 162, 71, 163, 72, 71, 164, 165, 72, 166, 73, 72, 167, 168, 73, 169, 74, 73, 170, 171, 74, 172,
    75, 74, 173, 174, 75, 175, 76, 75, 176, 177, 76, 178, 77, 76, 179, 180, 77, 181, 78, 77, 182, 183, 78, 184, 79, 78,
    185, 186, 79, 187, 80, 79, 188, 189, 80, 190, 81, 80, 191, 192, 81, 193, 82, 81, 194, 195, 82, 196, 83, 82, 197,
    198, 83, 199, 68, 83, 200, 201, 68, 68, 85, 84, 68, 69, 85, 69, 86, 85, 69, 70, 86, 70, 87, 86, 70, 71, 87, 71, 88,
    87, 71, 72, 88, 72, 89, 88, 72, 73, 89, 73, 90, 89, 73, 74, 90, 74, 91, 90, 74, 75, 91, 75, 92, 91, 75, 76, 92, 76,
    93, 92, 76, 77, 93, 77, 94, 93, 77, 78, 94, 78, 95, 94, 78, 79, 95, 79, 96, 95, 79, 80, 96, 80, 97, 96, 80, 81, 97,
    81, 98, 97, 81, 82, 98, 82, 99, 98, 82, 83, 99, 83, 84, 99, 83, 68, 84, 84, 85, 103, 85, 86, 103, 86, 87, 103, 87,
    101, 103, 87, 88, 101, 88, 89, 101, 89, 90, 101, 90, 91, 101, 91, 100, 101, 91, 92, 100, 92, 93, 100, 93, 94, 100,
    94, 95, 100, 95, 102, 100, 95, 96, 102, 96, 97, 102, 97, 98, 102, 98, 99, 102, 99, 103, 102, 99, 84, 103, 102, 101,
    100, 102, 103, 101);

constructor TRoundCube.Create(AOwner: TComponent);
begin
  inherited;
  Data.AssignFromMeshVertex(RoundCubeVertices, RoundCubeIndices);
end;

{ TCone }

constructor TCone.Create(AOwner: TComponent);
begin
  inherited;
  FSubdivisionsAxes := 12;
  FSubdivisionsCap := 1;
  FSubdivisionsHeight := 1;
  FJoinPeakNormals := True;
  RebuildMesh;
end;

procedure TCone.RebuildMesh;
var
  AngleIndex, HeightIndex, AuxAngleIndex, AuxHeightIndex: Integer;
  Phi: Single;
  HeightSegment, PhiStep: Single;
  PhiSin, PhiCos: Double;
  IdxCount: Integer;
  Offset, VerticesWidth: Integer;
  Normal, Normal1, Normal2: TPoint3D;
begin
  VerticesWidth := (FSubdivisionsAxes + 1);
  FData.VertexBuffer.Length := (FSubdivisionsCap) * VerticesWidth + ((FSubdivisionsHeight + 1) * VerticesWidth) + 1;
  FData.IndexBuffer.Length := (FSubdivisionsCap - 1) * FSubdivisionsAxes * 6 + (FSubdivisionsAxes * 3) +
    (FSubdivisionsHeight - 1) * FSubdivisionsAxes * 6 + (FSubdivisionsAxes * 3);
  PhiStep := DegToRad(360) / FSubdivisionsAxes;
  IdxCount := 0;
  // bottom center
  FData.VertexBuffer.Vertices[FData.VertexBuffer.Length - 1] := TPoint3D.Create(0, 0.5, 0);
  FData.VertexBuffer.TexCoord0[FData.VertexBuffer.Length - 1] := TPointF.Create(0.5, 0.5);
  FData.VertexBuffer.Normals[FData.VertexBuffer.Length - 1] := TPoint3D.Create(0, 1, 0);
  // bottom
  for HeightIndex := 0 to FSubdivisionsCap - 1 do
  begin
    Phi := 0;
    for AngleIndex := 0 to FSubdivisionsAxes - 1 do
    begin
      SinCos(Phi, PhiSin, PhiCos);
      FData.VertexBuffer.Vertices[AngleIndex + (HeightIndex * VerticesWidth)] :=
        TPoint3D.Create(PhiCos * 0.5 * ((HeightIndex + 1) / FSubdivisionsCap),
                        0.5,
                        PhiSin * 0.5 * ((HeightIndex + 1) / FSubdivisionsCap));

      FData.VertexBuffer.TexCoord0[AngleIndex + (HeightIndex * VerticesWidth)] :=
        TPointF.Create(PhiCos * 0.5 * ((HeightIndex + 1) / FSubdivisionsCap) + 0.5,
                       PhiSin * 0.5 * ((HeightIndex + 1) / FSubdivisionsCap) + 0.5);

      if AngleIndex = 0 then
      begin
        FData.VertexBuffer.Vertices[FSubdivisionsAxes + (HeightIndex * VerticesWidth)] :=
          TPoint3D.Create(PhiCos * 0.5 * ((HeightIndex + 1) / FSubdivisionsCap),
                          0.5,
                          (PhiSin * 0.5 * ((HeightIndex + 1) / FSubdivisionsCap)));

        FData.VertexBuffer.TexCoord0[FSubdivisionsAxes + (HeightIndex * VerticesWidth)] :=
          TPointF.Create(PhiCos * 0.5 * ((HeightIndex + 1) / FSubdivisionsCap) + 0.5,
                         PhiSin * 0.5 * ((HeightIndex + 1) / FSubdivisionsCap) + 0.5);

      end;
      AuxAngleIndex := AngleIndex + 1;
      AuxHeightIndex := HeightIndex - 1;
      if HeightIndex = 0 then
      begin
        FData.IndexBuffer.Indices[IdxCount + 0] := AngleIndex + (HeightIndex * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 2] := FData.VertexBuffer.Length - 1;
        FData.IndexBuffer.Indices[IdxCount + 1] := AuxAngleIndex + (HeightIndex * VerticesWidth);
        IdxCount := IdxCount + 3;
      end
      else
      begin
        FData.IndexBuffer.Indices[IdxCount + 0] := AngleIndex + (HeightIndex * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 1] := AuxAngleIndex + (AuxHeightIndex * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 2] := AngleIndex + (AuxHeightIndex * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 3] := AngleIndex + (HeightIndex * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 4] := AuxAngleIndex + (HeightIndex * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 5] := AuxAngleIndex + (AuxHeightIndex * VerticesWidth);
        IdxCount := IdxCount + 6;
      end;
      Phi := Phi + PhiStep;
    end;
  end;
  // sides
  Phi := 0;
  Offset := VerticesWidth * FSubdivisionsCap;
  for HeightIndex := 0 to FSubdivisionsHeight - 1 do
    for AngleIndex := 0 to FSubdivisionsAxes - 1 do
    begin
      SinCos(Phi, PhiSin, PhiCos);
      HeightSegment := 1 - (HeightIndex / FSubdivisionsHeight);
      FData.VertexBuffer.Vertices[Offset + AngleIndex + (HeightIndex * VerticesWidth)] :=
        TPoint3D.Create(PhiCos * 0.5 * HeightSegment, 0.5 - (1 - HeightSegment), PhiSin * 0.5 * HeightSegment);

      FData.VertexBuffer.TexCoord0[Offset + AngleIndex + (HeightIndex * VerticesWidth)] :=
        TPointF.Create(AngleIndex / FSubdivisionsAxes, HeightSegment);

      if HeightIndex = 0 then
      begin
        FData.VertexBuffer.Vertices[Offset + AngleIndex + (FSubdivisionsHeight * VerticesWidth)] :=
          TPoint3D.Create(0, -0.5, 0);
        FData.VertexBuffer.TexCoord0[Offset + AngleIndex + (FSubdivisionsHeight * VerticesWidth)] :=
          TPointF.Create(AngleIndex / FSubdivisionsAxes, 0);
      end;
      if AngleIndex = 0 then
      begin
        FData.VertexBuffer.Vertices[Offset + FSubdivisionsAxes + (HeightIndex * VerticesWidth)] :=
          FData.VertexBuffer.Vertices[Offset + (HeightIndex * VerticesWidth)];
        FData.VertexBuffer.TexCoord0[Offset + FSubdivisionsAxes + (HeightIndex * VerticesWidth)] :=
          TPointF.Create(1, HeightSegment);

        if HeightIndex = 0 then
        begin
          FData.VertexBuffer.Vertices[Offset + FSubdivisionsAxes + (FSubdivisionsHeight * VerticesWidth)] :=
            Point3D(0, -0.5, 0);
          FData.VertexBuffer.TexCoord0[Offset + FSubdivisionsAxes + (FSubdivisionsHeight * VerticesWidth)] :=
            PointF(1, 0);
        end;
      end;
      AuxAngleIndex := AngleIndex + 1;
      AuxHeightIndex := HeightIndex + 1;
      if HeightIndex = FSubdivisionsHeight - 1 then
      begin
        FData.IndexBuffer.Indices[IdxCount + 0] := Offset + AngleIndex + (HeightIndex * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 1] := Offset + AngleIndex + (FSubdivisionsHeight * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 2] := Offset + AuxAngleIndex + (HeightIndex * VerticesWidth);
        IdxCount := IdxCount + 3;
      end
      else
      begin
        FData.IndexBuffer.Indices[IdxCount + 0] := Offset + AngleIndex + (HeightIndex * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 2] := Offset + AuxAngleIndex + (AuxHeightIndex * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 1] := Offset + AngleIndex + (AuxHeightIndex * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 3] := Offset + AngleIndex + (HeightIndex * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 5] := Offset + AuxAngleIndex + (HeightIndex * VerticesWidth);
        FData.IndexBuffer.Indices[IdxCount + 4] := Offset + AuxAngleIndex + (AuxHeightIndex * VerticesWidth);
        IdxCount := IdxCount + 6;
      end;
      Phi := Phi + PhiStep;
    end;
  // The cone is generated, so generate the vertex normals
  FData.CalcFaceNormals;
  // Correction of the cone seam
  for HeightIndex := 0 to FSubdivisionsHeight - 1 do
  begin
    Normal1 := FData.VertexBuffer.Normals[Offset + 0 + (HeightIndex * VerticesWidth)];
    Normal2 := FData.VertexBuffer.Normals[Offset + FSubdivisionsAxes + (HeightIndex * VerticesWidth)];
    Normal.X := (Normal1.X + Normal2.X) / 2;
    Normal.Y := (Normal1.Y + Normal2.Y) / 2;
    Normal.Z := (Normal1.Z + Normal2.Z) / 2;
    Normal := Normal.Normalize;
    FData.VertexBuffer.Normals[Offset + 0 + (HeightIndex * VerticesWidth)] := Normal;
    FData.VertexBuffer.Normals[Offset + FSubdivisionsAxes + (HeightIndex * VerticesWidth)] := Normal;
  end;
  if FJoinPeakNormals then
  begin
    HeightIndex := FSubdivisionsHeight;
    for AngleIndex := 0 to FSubdivisionsAxes - 1 do
      FData.VertexBuffer.Normals[Offset + AngleIndex + (HeightIndex * VerticesWidth)] := TPoint3D.Create(0, -1, 0);
  end;
end;

procedure TCone.SetJoinPeakNormals(const Value: Boolean);
begin
  if FJoinPeakNormals <> Value then
  begin
    FJoinPeakNormals := Value;
    RebuildMesh;
  end;
end;

procedure TCone.SetSubdivisionsAxes(const Value: Integer);
begin
  if FSubdivisionsAxes <> Value then
  begin
    FSubdivisionsAxes := Value;
    if FSubdivisionsAxes < 3 then
      FSubdivisionsAxes := 3;
    RebuildMesh;
  end;
end;

procedure TCone.SetSubdivisionsCap(const Value: Integer);
begin
  if FSubdivisionsCap <> Value then
  begin
    FSubdivisionsCap := Value;
    if FSubdivisionsCap < 1 then
      FSubdivisionsCap := 1;
    RebuildMesh;
  end;
end;

procedure TCone.SetSubdivisionsHeight(const Value: Integer);
begin
  if FSubdivisionsHeight <> Value then
  begin
    FSubdivisionsHeight := Value;
    if FSubdivisionsHeight < 1 then
      FSubdivisionsHeight := 1;
    RebuildMesh;
  end;
end;

{ TExtrudedShape3D }

constructor TExtrudedShape3D.Create(AOwner: TComponent);
begin
  inherited;
  FFlatness := 1;
  FSides := [TExtrudedShapeSide.Front, TExtrudedShapeSide.Back, TExtrudedShapeSide.Shaft];
  Width := 4;
  Height := 4;
  Depth := 1;
end;

destructor TExtrudedShape3D.Destroy;
begin
  MaterialBackSource := nil;
  MaterialShaftSource := nil;
  inherited;
end;

procedure TExtrudedShape3D.FreeNotification(AObject: TObject);
begin
  inherited;
  if AObject = FMaterialBackSource then
    MaterialBackSource := nil;
  if AObject = FMaterialShaftSource then
    MaterialShaftSource := nil;
end;

procedure TExtrudedShape3D.Render;
var
  M: TMatrix3D;
begin
  inherited;
  M := TMatrix3D.Identity;
  M.m41 := -Width / 2;
  M.m42 := -Height / 2;
  Context.SetMatrix(M * AbsoluteMatrix);
end;

procedure TExtrudedShape3D.MouseMove3D(Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
var
  P3: TPoint3D;
begin
  inherited;
  if RayCastIntersect(TPoint3D(RayPos), TPoint3D(RayDir), P3) then
  begin
    P3 := TPoint3D(AbsoluteToLocalVector(P3));
    if Width > 0 then
      X := (((P3.X + (Width / 2)) / Width) * Width)
    else
      X := P3.X;
    if Depth > 0 then
      Y := (((-P3.Z + (Depth / 2)) / Depth) * Depth)
    else
      Y := -P3.Z;
  end
  else
    Exit;
  ShapeMouseMove(Shift, X, Y);
end;

procedure TExtrudedShape3D.MouseDown3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
var
  P3: TPoint3D;
begin
  inherited;
  if RayCastIntersect(TPoint3D(RayPos), TPoint3D(RayDir), P3) then
  begin
    P3 := TPoint3D(AbsoluteToLocalVector(P3));
    if Width > 0 then
      X := (((P3.X + (Width / 2)) / Width) * Width)
    else
      X := P3.X;
    if Depth > 0 then
      Y := (((-P3.Z + (Depth / 2)) / Depth) * Depth)
    else
      Y := -P3.Z;
  end
  else
    Exit;
  ShapeMouseDown(Button, Shift, X, Y);
end;

procedure TExtrudedShape3D.MouseUp3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
var
  P3: TPoint3D;
begin
  inherited;
  if RayCastIntersect(TPoint3D(RayPos), TPoint3D(RayDir), P3) then
  begin
    P3 := TPoint3D(AbsoluteToLocalVector(P3));
    if Width > 0 then
      X := (((P3.X + (Width / 2)) / Width) * Width)
    else
      X := P3.X;
    if Depth > 0 then
      Y := (((-P3.Z + (Depth / 2)) / Depth) * Depth)
    else
      Y := -P3.Z;
  end;
  ShapeMouseUp(Button, Shift, X, Y);
end;

procedure TExtrudedShape3D.ShapeMouseMove(Shift: TShiftState; X, Y: Single);
begin
end;

procedure TExtrudedShape3D.ShapeMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
end;

procedure TExtrudedShape3D.ShapeMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
end;

procedure TExtrudedShape3D.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Only for backward compatibility with XE2
  Filer.DefineProperty('MaterialBack.Diffuse', ReadMaterialBackDiffuse, nil, False);
  Filer.DefineProperty('MaterialBack.Ambient', ReadMaterialBackAmbient, nil, False);
  Filer.DefineProperty('MaterialBack.Emissive', ReadMaterialBackEmissive, nil, False);
  Filer.DefineProperty('MaterialBack.Specular', ReadMaterialBackSpecular, nil, False);
  Filer.DefineProperty('MaterialBack.Lighting', ReadMaterialBackLighting, nil, False);
  Filer.DefineProperty('MaterialBack.FillMode', ReadMaterialBackFillMode, nil, False);
  Filer.DefineProperty('MaterialBack.Modulation', ReadMaterialBackModulation, nil, False);
  Filer.DefineBinaryProperty('MaterialBack.Texture.PNG', ReadMaterialBackTexture, nil, False);
  Filer.DefineProperty('MaterialBack.TextureFiltering', ReadMaterialBackTextureFiltering, nil, False);
  Filer.DefineProperty('MaterialBack.ShadeMode', ReadMaterialBackShadeMode, nil, False);
  Filer.DefineProperty('MaterialBack.Shininess', ReadMaterialBackShininess, nil, False);

  Filer.DefineProperty('MaterialShaft.Diffuse', ReadMaterialShaftDiffuse, nil, False);
  Filer.DefineProperty('MaterialShaft.Ambient', ReadMaterialShaftAmbient, nil, False);
  Filer.DefineProperty('MaterialShaft.Emissive', ReadMaterialShaftEmissive, nil, False);
  Filer.DefineProperty('MaterialShaft.Specular', ReadMaterialShaftSpecular, nil, False);
  Filer.DefineProperty('MaterialShaft.Lighting', ReadMaterialShaftLighting, nil, False);
  Filer.DefineProperty('MaterialShaft.FillMode', ReadMaterialShaftFillMode, nil, False);
  Filer.DefineProperty('MaterialShaft.Modulation', ReadMaterialShaftModulation, nil, False);
  Filer.DefineBinaryProperty('MaterialShaft.Texture.PNG', ReadMaterialShaftTexture, nil, False);
  Filer.DefineProperty('MaterialShaft.TextureFiltering', ReadMaterialShaftTextureFiltering, nil, False);
  Filer.DefineProperty('MaterialShaft.ShadeMode', ReadMaterialShaftShadeMode, nil, False);
  Filer.DefineProperty('MaterialShaft.Shininess', ReadMaterialShaftShininess, nil, False);
end;

procedure TExtrudedShape3D.PrepareMaterialBackSource;
var
  Material: TLightMaterialSource;
begin
  if (FMaterialBackSource = nil) then
  begin
    Material := TLightMaterialSource.Create(Owner);
    if Root <> nil then
      Material.Parent := Root.GetObject
    else
      if Owner is TFmxObject then
        Material.Parent := TFmxObject(Owner);
    Material.Name := Name + 'MaterialBackSource';
    MaterialBackSource := Material;
  end;
end;

procedure TExtrudedShape3D.ReadMaterialBackDiffuse(Reader: TReader);
var
  Color: Integer;
begin
  PrepareMaterialBackSource;
  if (FMaterialBackSource <> nil) and (FMaterialBackSource is TLightMaterialSource) then
  begin
    IdentToAlphaColor(Reader.ReadIdent, Color);
    {$R-}
    TLightMaterialSource(FMaterialBackSource).Diffuse := TAlphaColor(Color);
    {$R+}
  end;
end;

procedure TExtrudedShape3D.ReadMaterialBackAmbient(Reader: TReader);
var
  Color: Integer;
begin
  PrepareMaterialBackSource;
  if (FMaterialBackSource <> nil) and (FMaterialBackSource is TLightMaterialSource) then
  begin
    IdentToAlphaColor(Reader.ReadIdent, Color);
    {$R-}
    TLightMaterialSource(FMaterialBackSource).Ambient := TAlphaColor(Color);
    {$R+}
  end;
end;

procedure TExtrudedShape3D.ReadMaterialBackEmissive(Reader: TReader);
begin
  Reader.ReadIdent;
end;

procedure TExtrudedShape3D.ReadMaterialBackSpecular(Reader: TReader);
var
  Color: Integer;
begin
  PrepareMaterialBackSource;
  if (FMaterialBackSource <> nil) and (FMaterialBackSource is TLightMaterialSource) then
  begin
    IdentToAlphaColor(Reader.ReadIdent, Color);
    {$R-}
    TLightMaterialSource(FMaterialBackSource).Specular := TAlphaColor(Color);
    {$R+}
  end;
end;

procedure TExtrudedShape3D.ReadMaterialBackLighting(Reader: TReader);
begin
  PrepareMaterialBackSource;
  Reader.ReadBoolean; // skip this
end;

procedure TExtrudedShape3D.ReadMaterialBackFillMode(Reader: TReader);
begin
  PrepareMaterialBackSource;
  Reader.ReadIdent; // skip this
end;

procedure TExtrudedShape3D.ReadMaterialBackModulation(Reader: TReader);
begin
  PrepareMaterialBackSource;
  Reader.ReadIdent; // skip this
end;

procedure TExtrudedShape3D.ReadMaterialBackTexture(Stream: TStream);
begin
  PrepareMaterialBackSource;
  if (FMaterialBackSource <> nil) and (FMaterialBackSource is TLightMaterialSource) then
    TLightMaterialSource(FMaterialBackSource).Texture.LoadFromStream(Stream);
end;

procedure TExtrudedShape3D.ReadMaterialBackTextureFiltering(Reader: TReader);
begin
  PrepareMaterialBackSource;
  Reader.ReadIdent; // skip this
end;

procedure TExtrudedShape3D.ReadMaterialBackShadeMode(Reader: TReader);
begin
  PrepareMaterialBackSource;
  Reader.ReadIdent; // skip this
end;

procedure TExtrudedShape3D.ReadMaterialBackShininess(Reader: TReader);
begin
  PrepareMaterialBackSource;
  if (FMaterialBackSource <> nil) and (FMaterialBackSource is TLightMaterialSource) then
    TLightMaterialSource(FMaterialBackSource).Shininess := Reader.ReadInteger;
end;

procedure TExtrudedShape3D.PrepareMaterialShaftSource;
var
  Material: TLightMaterialSource;
begin
  if (FMaterialShaftSource = nil) then
  begin
    Material := TLightMaterialSource.Create(Owner);
    if Root <> nil then
      Material.Parent := Root.GetObject
    else
      if Owner is TFmxObject then
        Material.Parent := TFmxObject(Owner);
    Material.Name := Name + 'MaterialShaftSource';
    MaterialShaftSource := Material;
  end;
end;

procedure TExtrudedShape3D.ReadMaterialShaftDiffuse(Reader: TReader);
var
  Color: Integer;
begin
  PrepareMaterialShaftSource;
  if (FMaterialShaftSource <> nil) and (FMaterialShaftSource is TLightMaterialSource) then
  begin
    IdentToAlphaColor(Reader.ReadIdent, Color);
    {$R-}
    TLightMaterialSource(FMaterialShaftSource).Diffuse := TAlphaColor(Color);
    {$R+}
  end;
end;

procedure TExtrudedShape3D.ReadMaterialShaftAmbient(Reader: TReader);
var
  Color: Integer;
begin
  PrepareMaterialShaftSource;
  if (FMaterialShaftSource <> nil) and (FMaterialShaftSource is TLightMaterialSource) then
  begin
    IdentToAlphaColor(Reader.ReadIdent, Color);
    {$R-}
    TLightMaterialSource(FMaterialShaftSource).Ambient := TAlphaColor(Color);
    {$R+}
  end;
end;

procedure TExtrudedShape3D.ReadMaterialShaftEmissive(Reader: TReader);
begin
  Reader.ReadIdent;
end;

procedure TExtrudedShape3D.ReadMaterialShaftSpecular(Reader: TReader);
var
  Color: Integer;
begin
  PrepareMaterialShaftSource;
  if (FMaterialShaftSource <> nil) and (FMaterialShaftSource is TLightMaterialSource) then
  begin
    IdentToAlphaColor(Reader.ReadIdent, Color);
    {$R-}
    TLightMaterialSource(FMaterialShaftSource).Specular := TAlphaColor(Color);
    {$R+}
  end;
end;

procedure TExtrudedShape3D.ReadMaterialShaftLighting(Reader: TReader);
begin
  PrepareMaterialShaftSource;
  Reader.ReadBoolean; // skip this
end;

procedure TExtrudedShape3D.ReadMaterialShaftFillMode(Reader: TReader);
begin
  PrepareMaterialShaftSource;
  Reader.ReadIdent; // skip this
end;

procedure TExtrudedShape3D.ReadMaterialShaftModulation(Reader: TReader);
begin
  PrepareMaterialShaftSource;
  Reader.ReadIdent; // skip this
end;

procedure TExtrudedShape3D.ReadMaterialShaftTexture(Stream: TStream);
begin
  PrepareMaterialShaftSource;
  if (FMaterialShaftSource <> nil) and (FMaterialShaftSource is TLightMaterialSource) then
    TLightMaterialSource(FMaterialShaftSource).Texture.LoadFromStream(Stream);
end;

procedure TExtrudedShape3D.ReadMaterialShaftTextureFiltering(Reader: TReader);
begin
  PrepareMaterialShaftSource;
  Reader.ReadIdent; // skip this
end;

procedure TExtrudedShape3D.ReadMaterialShaftShadeMode(Reader: TReader);
begin
  PrepareMaterialShaftSource;
  Reader.ReadIdent; // skip this
end;

procedure TExtrudedShape3D.ReadMaterialShaftShininess(Reader: TReader);
begin
  PrepareMaterialShaftSource;
  if (FMaterialShaftSource <> nil) and (FMaterialShaftSource is TLightMaterialSource) then
    TLightMaterialSource(FMaterialShaftSource).Shininess := Reader.ReadInteger;
end;

procedure TExtrudedShape3D.SetFlatness(const Value: Single);
begin
  if FFlatness <> Value then
  begin
    FFlatness := Value;
    if FFlatness < 0.05 then
      FFlatness := 0.05;
    Repaint;
  end;
end;

procedure TExtrudedShape3D.SetSides(const Value: TExtrudedShapeSides);
begin
  if FSides <> Value then
  begin
    FSides := Value;
    Repaint;
  end;
end;

procedure TExtrudedShape3D.SetMaterialBack(const Value: TMaterialSource);
begin
  if FMaterialBackSource <> Value then
  begin
    if FMaterialBackSource <> nil then
    begin
      FMaterialBackSource.RemoveChangeNotifier(Self);
      FMaterialBackSource.RemoveFreeNotify(Self);
    end;
    FMaterialBackSource := Value;

    if FMaterialBackSource <> nil then
    begin
      FMaterialBackSource.AddFreeNotify(Self);
      FMaterialBackSource.AddChangeNotifier(Self);
    end;
  end;
end;

procedure TExtrudedShape3D.SetMaterialShaft(const Value: TMaterialSource);
begin
  if FMaterialShaftSource <> Value then
  begin
    if FMaterialShaftSource <> nil then
    begin
      FMaterialShaftSource.RemoveChangeNotifier(Self);
      FMaterialShaftSource.RemoveFreeNotify(Self);
    end;
    FMaterialShaftSource := Value;
    if FMaterialShaftSource <> nil then
    begin
      FMaterialShaftSource.AddFreeNotify(Self);
      FMaterialShaftSource.AddChangeNotifier(Self);
    end;
  end;
end;

{ TRectangle3D }

constructor TRectangle3D.Create(AOwner: TComponent);
begin
  inherited;
  FCorners := AllCorners;
end;

destructor TRectangle3D.Destroy;
begin
  inherited;
end;

function TRectangle3D.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

procedure TRectangle3D.Render;
var
  S: TPointF;
  VP: TPolygon;
  r: TRectF;
  Path: TPathData;
begin
  inherited;
  Path := TPathData.Create;
  try
    Path.AddRectangle(RectF(0, 0, Width, Height), XRadius, YRadius, FCorners, FCornerType);
    S := Path.FlattenToPolygon(VP, FFlatness);
    if (S.X > 0) and (S.Y > 0) then
    begin
      r := RectF(0, 0, S.X, S.Y);
      { front }
      if TExtrudedShapeSide.Front in FSides then
      begin
        Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
          RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity, False, True, False);
      end;
      { back }
      if TExtrudedShapeSide.Back in FSides then
      begin
        Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
          RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(FMaterialBackSource), AbsoluteOpacity, True, False, False);
      end;
      { shaft }
      if TExtrudedShapeSide.Shaft in FSides then
      begin
        Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
          RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(FMaterialShaftSource), AbsoluteOpacity, False, False, True);
      end;
    end;
  finally
    Path.Free;
  end;
end;

procedure TRectangle3D.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then
  begin
    FCorners := Value;
    Repaint;
  end;
end;

procedure TRectangle3D.SetCornerType(const Value: TCornerType);
begin
  if FCornerType <> Value then
  begin
    FCornerType := Value;
    Repaint;
  end;
end;

procedure TRectangle3D.SetXRadius(const Value: Single);
begin
  if FXRadius <> Value then
  begin
    FXRadius := Value;
    Repaint;
  end;
end;

procedure TRectangle3D.SetYRadius(const Value: Single);
begin
  if FYRadius <> Value then
  begin
    FYRadius := Value;
    Repaint;
  end;
end;

{ TEllipse3D }

procedure TEllipse3D.Render;
var
  S: TPointF;
  VP: TPolygon;
  r: TRectF;
  Path: TPathData;
begin
  inherited;
  Path := TPathData.Create;
  try
    r := RectF(0, 0, Width, Height);
    Path.AddEllipse(r);

    S := Path.FlattenToPolygon(VP, FFlatness);
    if (S.X > 0) and (S.Y > 0) then
    begin
      r := RectF(0, 0, S.X, S.Y);
      { front }
      if TExtrudedShapeSide.Front in FSides then
      begin
        Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
          RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity, False, True, False);
      end;
      { back }
      if TExtrudedShapeSide.Back in FSides then
      begin
        Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
          RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialBackSource), AbsoluteOpacity, True, False, False);
      end;
      { left }
      if TExtrudedShapeSide.Shaft in FSides then
      begin
        Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
          RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialShaftSource), AbsoluteOpacity, False, False, True);
      end;
    end;
  finally
    Path.Free;
  end;
end;

{ TText3D }

constructor TText3D.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TFont.Create;
  FFont.OnChanged := FontChanged;
  FFlatness := 1;
  Depth := 0.3;
  Width := 3;
  Height := 2;
  ZWrite := True;
  WordWrap := True;
end;

destructor TText3D.Destroy;
begin
  if FFont <> nil then
    FFont.Free;
  inherited;
end;

procedure TText3D.RebuildPolygon;
var
  R: TRectF;
  Path: TPathData;
  B: TPointF;
  Factor: Single;
  Layout: TTextLayout;
begin
  if not Text.IsEmpty then
  begin
    if Projection = TProjection.Camera then
      Factor := 10
    else
      Factor := 1;
    Layout := TTextLayoutManager.DefaultTextLayout.Create(TCanvasManager.MeasureCanvas);
    try
      Layout.BeginUpdate;
      Layout.Font := Font;
      Layout.Font.Size := Font.Size * Factor;
      Layout.WordWrap := WordWrap;
      Layout.Text := Text;
      Layout.HorizontalAlign := HorzTextAlign;
      Layout.VerticalAlign := VertTextAlign;
      Layout.MaxSize := PointF(Width * Factor, Height * Factor);
      Layout.EndUpdate;
      Path := TPathData.Create;
      try
        R := RectF(0, 0, Width * Factor, Height * Factor);
        Layout.ConvertToPath(Path);
        if Stretch then
        begin
          if not Path.IsEmpty then
          begin
            B := Path.FlattenToPolygon(FPolygon, FFlatness / Factor);
            if (B.X > 0) and (B.Y > 0) then
              FTextRect := R;
          end;
        end
        else
        begin
          if not Path.IsEmpty then
          begin
            R := Layout.TextRect;
            if not WordWrap then
            begin
              case HorzTextAlign of
                TTextAlign.Center:
                  R := RectF(0, 0, Width * Factor, RectHeight(r));
                TTextAlign.Leading:
                  R := RectF(0, 0, Width * Factor, RectHeight(r));
                TTextAlign.Trailing:
                  R := RectF(Width * Factor - RectWidth(r), 0, Width * Factor, RectHeight(r));
              end;
              R := UnionRect(r, RectF(0, 0, Width * Factor, Height * Factor));
            end
            else
            begin
              R := UnionRect(r, RectF(0, 0, Width * Factor, Height * Factor));
            end;
            B := Path.FlattenToPolygon(FPolygon, FFlatness / Factor);
            if (B.X > 0) and (B.Y > 0) then
              FTextRect := R;
          end;
        end;
      finally
        Path.Free;
      end;
    finally
      Layout.Free;
    end;
  end;
  Repaint;
end;

procedure TText3D.Render;
var
  R: TRectF;
  Factor: Integer;
begin
  inherited;
  if not Text.IsEmpty then
  begin
    if Projection = TProjection.Camera then
      Factor := 10
    else
      Factor := 1;

    if Length(FPolygon) > 0 then
    begin
      if Stretch then
      begin
        R := FTextRect;
        { front }
        if TExtrudedShapeSide.Front in FSides then
        begin
          Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(R) / Factor, RectHeight(R) / Factor,
            Depth), RectF(0, 0, 0, 0), FPolygon, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity, False, True, False);
        end;
        { back }
        if TExtrudedShapeSide.Back in FSides then
        begin
          Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(R) / Factor, RectHeight(R) / Factor,
            Depth), RectF(0, 0, 0, 0), FPolygon, TMaterialSource.ValidMaterial(MaterialBackSource), AbsoluteOpacity, True, False, False);
        end;
        { left }
        if TExtrudedShapeSide.Shaft in FSides then
        begin
          Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(R) / Factor, RectHeight(R) / Factor,
            Depth), RectF(0, 0, 0, 0), FPolygon, TMaterialSource.ValidMaterial(MaterialShaftSource), AbsoluteOpacity, False, False, True);
        end;
      end
      else
      begin
        R := FTextRect;
        if not WordWrap then
        begin
          case HorzTextAlign of
            TTextAlign.Center:
              r := RectF(0, 0, Width * Factor, RectHeight(r));
            TTextAlign.Leading:
              r := RectF(0, 0, Width * Factor, RectHeight(r));
            TTextAlign.Trailing:
              r := RectF(Width * Factor - RectWidth(r), 0, Width * Factor, RectHeight(r));
          end;
          R := UnionRect(r, RectF(0, 0, Width * Factor, Height * Factor));
        end
        else
          R := UnionRect(r, RectF(0, 0, Width * Factor, Height * Factor));

        { front }
        if TExtrudedShapeSide.Front in FSides then
        begin
          Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(R) / Factor, RectHeight(R) / Factor,
            Depth), R, FPolygon, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity, False, True, False);
        end;
        { back }
        if TExtrudedShapeSide.Back in FSides then
        begin
          Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(R) / Factor, RectHeight(R) / Factor,
            Depth), R, FPolygon, TMaterialSource.ValidMaterial(MaterialBackSource), AbsoluteOpacity, True, False, False);
        end;
        { left }
        if TExtrudedShapeSide.Shaft in FSides then
        begin
          Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(R) / Factor, RectHeight(R) / Factor,
            Depth), R, FPolygon, TMaterialSource.ValidMaterial(MaterialShaftSource), AbsoluteOpacity, False, False, True);
        end;
      end;
    end;
  end;
  if (csDesigning in ComponentState) and not Locked then
    Context.DrawCube(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(Width, Height, Depth), AbsoluteOpacity,
      $8060A799);
end;


function TText3D.GetPathBounds: TRectF;
var
  r: TRectF;
  Bmp: TBitmap;
  Path: TPathData;
begin
  Result := RectF(0, 0, Width, Height);
  if Text <> '' then
  begin
    Bmp := nil;
    Path := nil;
    try
      Bmp := TBitmap.Create(1, 1);
      Bmp.Canvas.Font.Family := Font.Family;
      Bmp.Canvas.Font.Style := Font.Style;
      Bmp.Canvas.Font.Size := Font.Size;
      Path := TPathData.Create;
      if Stretch then
        r := RectF(0, 0, Width * 10, Height * 10)
      else
      begin
        r := RectF(0, 0, Width * 10, Height * 10);
        if Bmp.Canvas.TextToPath(Path, r, Text, WordWrap, HorzTextAlign, VertTextAlign) then
        begin
          Result := Path.GetBounds;
        end;
      end;
    finally
      if Path <> nil then
        Path.Free;
      if Bmp <> nil then
        Bmp.Free;
    end;
    Result := RectF(Result.Left / 10, Result.Top / 10, Result.Right / 10, Result.Bottom / 10);
  end;
end;

function TText3D.GetTextBounds: TRectF;
var
  r: TRectF;
  B: TBitmap;
begin
  Result := RectF(0, 0, 0, 0);
  if Text <> '' then
  begin
    B := TBitmap.Create(1, 1);
    try
      B.Canvas.Font.Family := Font.Family;
      B.Canvas.Font.Style := Font.Style;
      B.Canvas.Font.Size := Font.Size;
      if Stretch then
        r := RectF(0, 0, Width, Height)
      else
      begin
        r := RectF(0, 0, Width * 10, Height * 10);
        B.Canvas.MeasureText(r, Text, WordWrap, [], TTextAlign.Leading, TTextAlign.Leading);
        Result := RectF(0, 0, r.Right / 10, r.Bottom / 10);
      end;
    finally
      B.Free;
    end;
  end;
end;

function TText3D.GetPathLength: Single;
var
  r: TRectF;
  Bmp: TBitmap;
  Path: TPathData;
  Points: TPolygon;
  i: Integer;
  P: TPointF;
begin
  Result := 0;
  if Text <> '' then
  begin
    Bmp := TBitmap.Create(1, 1);
    try
      Bmp.Canvas.Font.Family := Font.Family;
      Bmp.Canvas.Font.Style := Font.Style;
      Bmp.Canvas.Font.Size := Font.Size;
      Path := TPathData.Create;
      try
        if Stretch then
        begin
          r := RectF(0, 0, Width * 10, Height * 10);
          Bmp.Canvas.TextToPath(Path, r, Text, WordWrap, HorzTextAlign, VertTextAlign);
        end
        else
        begin
          r := RectF(0, 0, Width * 10, Height * 10);
          Bmp.Canvas.MeasureText(r, Text, WordWrap, [], TTextAlign.Leading, VertTextAlign);
          Bmp.Canvas.TextToPath(Path, r, Text, WordWrap, HorzTextAlign, VertTextAlign);
        end;
        Path.FlattenToPolygon(Points, FFlatness);
      finally
        Path.Free;
      end;
    finally
      Bmp.Free;
    end;
    for i := 0 to High(Points) do
    begin
      P := Points[i];
      if (P.X >= $FFFF) and (P.Y >= $FFFF) then
        Continue;
      if i > 0 then
      begin
        if Points[i - 1].X >= $FFFF then
        begin
          Result := Result + TPointF.Create(P.X - Points[i - 2].X, P.Y - Points[i - 2].Y).Length;
        end
        else
        begin
          Result := Result + TPointF.Create(P.X - Points[i - 1].X, P.Y - Points[i - 1].Y).Length;
        end;
      end;
    end;
    Result := Result / 10;
  end;
end;

procedure TText3D.FontChanged(Sender: TObject);
begin
  RebuildPolygon;
end;

procedure TText3D.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TText3D.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    RebuildPolygon;
  end;
end;

procedure TText3D.SetHorzTextAlign(const Value: TTextAlign);
begin
  if FHorzTextAlign <> Value then
  begin
    FHorzTextAlign := Value;
    RebuildPolygon;
  end;
end;

procedure TText3D.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    RebuildPolygon;
  end;
end;

procedure TText3D.SetVertTextAlign(const Value: TTextAlign);
begin
  if FVertTextAlign <> Value then
  begin
    FVertTextAlign := Value;
    RebuildPolygon;
  end;
end;

procedure TText3D.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    RebuildPolygon;
  end;
end;

{ TPath3D }

constructor TPath3D.Create(AOwner: TComponent);
begin
  inherited;
  FPath := TPathData.Create;
  FPath.OnChanged := PathChanged;
  FFlatness := 2;
  FWrapMode := TPathWrapMode.Stretch;
end;

destructor TPath3D.Destroy;
begin
  FPath.Free;
  inherited;
end;

procedure TPath3D.Render;
var
  S: TPointF;
  VP: TPolygon;
  r: TRectF;
  i, j: Integer;
begin
  inherited;
  if not FPath.IsEmpty then
  begin
    case FWrapMode of
      TPathWrapMode.Original:
        begin
          S := FPath.FlattenToPolygon(VP, FFlatness);
          if (S.X > 0) and (S.Y > 0) then
          begin
            r := RectF(0, 0, S.X, S.Y);
            { front }
            if TExtrudedShapeSide.Front in FSides then
            begin
              Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
                RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity, False, True, False);
            end;
            { back }
            if TExtrudedShapeSide.Back in FSides then
            begin
              Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
                RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialBackSource), AbsoluteOpacity, True, False, False);
            end;
            { left }
            if TExtrudedShapeSide.Shaft in FSides then
            begin
              Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
                RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialShaftSource), AbsoluteOpacity, False, False, True);
            end;
          end;
        end;
      TPathWrapMode.Fit:
        begin
          S := FPath.FlattenToPolygon(VP, FFlatness);
          if (S.X > 0) and (S.Y > 0) then
          begin
            r := RectF(0, 0, S.X, S.Y);
            r.Fit(RectF(0, 0, Width, Height));
            { front }
            if TExtrudedShapeSide.Front in FSides then
            begin
              Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
                RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity, False, True, False);
            end;
            { back }
            if TExtrudedShapeSide.Back in FSides then
            begin
              Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
                RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialBackSource), AbsoluteOpacity, True, False, False);
            end;
            { left }
            if TExtrudedShapeSide.Shaft in FSides then
            begin
              Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
                RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialShaftSource), AbsoluteOpacity, False, False, True);
            end;
          end;
        end;
      TPathWrapMode.Stretch:
        begin
          S := FPath.FlattenToPolygon(VP, FFlatness);
          if (S.X > 0) and (S.Y > 0) then
          begin
            r := RectF(0, 0, Width, Height);
            { front }
            if TExtrudedShapeSide.Front in FSides then
            begin
              Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
                RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity, False, True, False);
            end;
            { back }
            if TExtrudedShapeSide.Back in FSides then
            begin
              Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
                RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialBackSource), AbsoluteOpacity, True, False, False);
            end;
            { left }
            if TExtrudedShapeSide.Shaft in FSides then
            begin
              Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
                RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialShaftSource), AbsoluteOpacity, False, False, True);
            end;
          end;
        end;
      TPathWrapMode.Tile:
        begin
          S := FPath.FlattenToPolygon(VP, FFlatness);
          if (S.X > 0) and (S.Y > 0) then
          begin
            r := RectF(0, 0, S.X, S.Y);
            for i := 0 to round(Width / RectWidth(r)) do
              for j := 0 to round(Height / RectHeight(r)) do
              begin
                r := RectF(0, 0, S.X, S.Y);
                OffsetRect(r, i * (RectWidth(r)), j * (RectHeight(r)));
                { front }
                if TExtrudedShapeSide.Front in FSides then
                begin
                  Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
                    RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity, False, True, False);
                end;
                { back }
                if TExtrudedShapeSide.Back in FSides then
                begin
                  Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
                    RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialBackSource), AbsoluteOpacity, True, False, False);
                end;
                { left }
                if TExtrudedShapeSide.Shaft in FSides then
                begin
                  Context.FillPolygon(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(RectWidth(r), RectHeight(r), Depth),
                    RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialShaftSource), AbsoluteOpacity, False, False, True);
                end;
              end;
          end;
        end;
    end;
  end;
  if (csDesigning in ComponentState) and not Locked then
    Context.DrawCube(TPoint3D.Create(Width / 2, Height / 2, 0), TPoint3D.Create(Width, Height, Depth), AbsoluteOpacity,
      $8060A799);
end;

procedure TPath3D.PathChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TPath3D.SetPath(const Value: TPathData);
begin
  FPath.Assign(Value);
end;

procedure TPath3D.SetWrapMode(const Value: TPathWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    Repaint;
  end;
end;

{ TModel3D }

procedure TModel3D.Clear;
var
  I: Integer;
begin
  if not (csDestroying in ComponentState) then
    for I := Low(FMeshCollection) to High(FMeshCollection) do
    begin
      RemoveObject(FMeshCollection[I]);
      FMeshCollection[I].Free;
    end;

  FMeshCollection := nil;
end;

constructor TModel3D.Create(AOwner: TComponent);
begin
  inherited;
  FMeshesOwner := TDummy.Create(Self);
  FMeshesOwner.Lock;
  FMeshesOwner.Stored := False;
  FWrapMode := TMeshWrapMode.Fit;
  AddObject(FMeshesOwner);
end;

procedure TModel3D.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Data', ReadModel, WriteModel, True);
end;

destructor TModel3D.Destroy;
begin
  Clear;
  inherited;
end;

function TModel3D.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
var
  I: Integer;
  LMesh: TMesh;
  LMatrix, LInvMatrix: TMatrix3D;
  LRayPos: TPoint3D;
  LRayDir: TPoint3D;
  LLocalRayPos: TPoint3D;
  LLocalRayDir: TPoint3D;
begin
  Result := False;
  LRayPos := TPoint3D(LocalToAbsoluteVector(RayPos));
  LRayDir := TPoint3D(LocalToAbsoluteDirection(RayDir));
  for I := 0 to High(FMeshCollection) do
  begin
    LMesh := FMeshCollection[I];
    // LMatrix is a matrix which converts from local space to mesh space
    LMatrix := LMesh.GetMeshMatrix * LMesh.AbsoluteMatrix;
    LInvMatrix := LMatrix.Inverse;

    LLocalRayPos := TPoint3D(TVector3D(LRayPos) * LInvMatrix);
    LLocalRayDir := TPoint3D(TVector3D.Create(LRayDir, 0) * LInvMatrix).Normalize;

    if LMesh.Data.RayCastIntersect(LLocalRayPos, LLocalRayDir, Intersection) then
    begin
      // Convert from child object space to this control space, and then to absolute
      Intersection := TPoint3D(Intersection * LMatrix);
      Exit(True);
    end;
  end;
end;

function TModel3D.LoadFromFile(const AFileName: string): Boolean;
begin
  Clear;
  Result := TModelImportServices.LoadFromFile(AFileName, FMeshCollection, Self);
  UpdateMeshCollection;
end;

procedure TModel3D.ReadModel(Stream: TStream);

  procedure Read(AStream: TStream);
  var
    I: Integer;
    LLength: Word;
  begin
    Clear;
    AStream.ReadBuffer(LLength, 2);
    SetLength(FMeshCollection, LLength);
    for I := 0 to LLength - 1 do
    begin
      FMeshCollection[I] := TMesh.Create(Self);
      AStream.ReadComponent(FMeshCollection[i]);
    end;
  end;

begin
  Read(Stream);
  UpdateMeshCollection;
end;

procedure TModel3D.Resize3D;
begin
  inherited;
  RelocateMeshes;
end;

procedure TModel3D.SetWrapMode(const AMode: TMeshWrapMode);
begin
  if FWrapMode <> AMode then
  begin
    FWrapMode := AMode;
    UpdateMeshCollection;
    Repaint;
  end;
end;

procedure TModel3D.UpdateMeshCollection;
var
  I: Integer;
  LMesh: TMesh;
begin
  for I := Low(FMeshCollection) to High(FMeshCollection) do
  begin
    LMesh := FMeshCollection[I];
    LMesh.HitTest := False;
    LMesh.Lock;
    LMesh.Stored := False;
    LMesh.WrapMode := TMeshWrapMode.Stretch;
    if LMesh.Parent <> FMeshesOwner then
      LMesh.Parent := FMeshesOwner;
  end;
  RelocateMeshes;
end;

function TModel3D.GetMeshesLocalBoundingBox: TBoundingBox;
var
  MeshBoundingBox: TBoundingBox;
  MeshesBoundingBox: TBoundingBox;
  I: Integer;
  LMesh: TMesh;
begin
  for I := Low(FMeshCollection) to High(FMeshCollection) do
  begin
    LMesh := FMeshCollection[I];
    LMesh.Parent := nil;
    MeshBoundingBox := LMesh.GlobalBounds;
    MeshesBoundingBox := MeshesBoundingBox.Union(MeshBoundingBox);
    LMesh.Parent := FMeshesOwner;
  end;
  Result := MeshesBoundingBox;
end;

procedure TModel3D.RelocateMeshes;
var
  MeshesBoundingBox: TBoundingBox;
  ComponentBoundingBox: TBoundingBox;
  HalfSize: TPoint3D;
  ScaleFactor: Single;
  LScale, Displacement, LSize: TPoint3D;
  I: Integer;
  LMesh: TMesh;
  MeshBoundingBox: TBoundingBox;
begin
  for I := Low(FMeshCollection) to High(FMeshCollection) do
  begin
    LMesh := FMeshCollection[I];
    MeshBoundingBox := LMesh.Data.GetBoundingBox;
    LMesh.Position.Point := MeshBoundingBox.CenterPoint;
    LMesh.Width := MeshBoundingBox.Width;
    LMesh.Height := MeshBoundingBox.Height;
    LMesh.Depth := MeshBoundingBox.Depth;
  end;

  MeshesBoundingBox := GetMeshesLocalBoundingBox;

  LScale := TPoint3D.Create(1, 1, 1);
  Displacement := TPoint3D.Zero;
  FMeshesOwner.Position.Point := TPoint3D.Zero;
  FMeshesOwner.Scale.Point := TPoint3D.Create(1, 1, 1);
  if not MeshesBoundingBox.IsEmpty then
  begin
    case WrapMode of
      TMeshWrapMode.Original:
      begin
        LScale := TPoint3D.Create(Width, Height, Depth);
        Displacement := TPoint3D.Zero;
      end;
      TMeshWrapMode.Fit:
      begin
        HalfSize := TPoint3D.Create(Width * 0.5, Height * 0.5, Depth * 0.5);
        ComponentBoundingBox := TBoundingBox.Create(-HalfSize, HalfSize);
        ScaleFactor := 1 / MeshesBoundingBox.FitIntoScale(ComponentBoundingBox);
        LScale := TPoint3D.Create(ScaleFactor, ScaleFactor, ScaleFactor);
        Displacement := -MeshesBoundingBox.CenterPoint;
      end;
      TMeshWrapMode.Stretch:
      begin
        HalfSize := TPoint3D.Create(Width * 0.5, Height * 0.5, Depth * 0.5);
        ComponentBoundingBox := TBoundingBox.Create(-HalfSize, HalfSize);
        LScale.X := ComponentBoundingBox.Width / MeshesBoundingBox.Width;
        LScale.Y := ComponentBoundingBox.Height / MeshesBoundingBox.Height;
        LScale.Z := ComponentBoundingBox.Depth / MeshesBoundingBox.Depth;
        Displacement := -MeshesBoundingBox.CenterPoint;
      end;
      TMeshWrapMode.Resize:
      begin
        LSize.X := Max(Abs(MeshesBoundingBox.Left) , Abs(MeshesBoundingBox.Right)) * 2.0;
        LSize.Y := Max(Abs(MeshesBoundingBox.Top) , Abs(MeshesBoundingBox.Bottom)) * 2.0;
        LSize.Z := Max(Abs(MeshesBoundingBox.Near) , Abs(MeshesBoundingBox.Far)) * 2.0;
        SetSize(LSize.X, LSize.Y, LSize.Z);
      end;
    end;
    FMeshesOwner.Position.Point := Displacement * LScale;
    FMeshesOwner.Scale.Point := LScale;
  end;
  FMeshesOwner.Position.Point := Displacement * LScale;
  FMeshesOwner.Scale.Point := LScale;
  FMeshesOwner.RotationCenter.Point := TPoint3D.Zero;
  FMeshesOwner.RotationAngle.Point := TPoint3D.Zero;
end;

procedure TModel3D.WriteModel(Stream: TStream);

  procedure Write(AStream: TStream);
  var
    i: Integer;
    LLength: Word;
  begin
    LLength := Length(FMeshCollection);
    AStream.WriteBuffer(LLength, 2);
    for i := 0 to LLength - 1 do
    begin
      AStream.WriteComponent(FMeshCollection[i]);
    end;
  end;

begin
  Write(Stream);
end;

procedure RegisterAliases;
begin
  AddEnumElementAliases(TypeInfo(TExtrudedShapeSide), ['esFront', 'esBack', 'esShaft']);
end;

procedure UnregisterAliases;
begin
  RemoveEnumElementAliases(TypeInfo(TExtrudedShapeSide));
end;

initialization
  RegisterAliases;
  RegisterFmxClasses([TPlane, TDisk, TCube, TMesh, TSphere, TCylinder, TRoundCube, TCone,
    TGrid3D, TStrokeCube, TText3D, TPath3D, TRectangle3D, TEllipse3D,
    TShape3D, TExtrudedShape3D, TModel3D]);
finalization
  UnregisterAliases;
end.
