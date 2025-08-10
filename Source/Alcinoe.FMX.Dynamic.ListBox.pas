unit Alcinoe.FMX.Dynamic.ListBox;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.Types,
  System.UITypes,
  System.Messaging,
  System.Generics.Collections,
  System.Diagnostics,
  FMX.graphics,
  FMX.Controls,
  Alcinoe.JSONDoc,
  Alcinoe.FMX.Ani,
  Alcinoe.Common,
  Alcinoe.FMX.CacheEngines,
  Alcinoe.StringList,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Dynamic.StdCtrls,
  Alcinoe.FMX.Dynamic.Controls,
  Alcinoe.FMX.Dynamic.Objects;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALDynamicListBox = class(TALDynamicControlHost)
  public
    type
      // ---------------------
      // TRefreshTransitionKind
      TRefreshTransitionKind = (None, Scroll, DirectFadeIn, DirectFadeOut, OverlayFadeIn, RevealFadeOut, CrossFade);
      // -----
      // TItem
      TView = Class;
      TItem = class(TALDynamicExtendedControl)
      public
        type
          // --------
          // TContent
          TContent = class(TALDynamicRectangle)
          public
            type
              TFill = class(TALBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateFill: TALBrush; override;
            function CreateStroke: TALStrokeBrush; override;
          public
            constructor Create(const AOwner: TItem); reintroduce; virtual;
          end;
          // ------------
          // TMainContent
          TMainContent = class(TContent)
          public
            constructor Create(const AOwner: TItem); override;
          end;
          // ---------------
          // TLoadingContent
          TLoadingContent = class(TContent)
          public
            type
              TAnimation = class(TALFloatAnimation)
              public
                type
                  TAnimationKind = (None, Pulse, Wave);
              private
                FOwner: TLoadingContent;
                FKind: TAnimationKind;
                FWaveColor: TAlphaColor;
                FWaveAngle: Single;
              protected
                procedure SetKind(const AValue: TAnimationKind); virtual;
                procedure DoProcess; override;
                procedure DoFinish; override;
                function GetDefaultLoop: Boolean; override;
              public
                constructor Create(const AOwner: TLoadingContent); reintroduce; Virtual;
                property Kind: TAnimationKind read FKind write SetKind;
                property WaveColor: TAlphaColor read FWaveColor write FWaveColor;
                property WaveAngle: Single read FWaveAngle write FWaveAngle;
              end;
          private
            FAnimation: TAnimation;
          protected
            procedure Paint; override;
          public
            constructor Create(const AOwner: TItem); override;
            destructor Destroy; override;
            procedure BeforeDestruction; override;
            property Animation: TAnimation read FAnimation;
          end;
          // -------------
          // TErrorContent
          TErrorContent = class(TContent);
      public
        type
          TDownloadDataContext = class;
          TDownloadDataEvent = procedure(const AContext: TDownloadDataContext; out AData: TALJSONNodeW; var AErrorCode: Integer) of object;
          TDownloadDataContext = Class(TALWorkerContext)
          private
            FOnDownloadData: TDownloadDataEvent;
            function GetOwner: TItem;
          public
            CustomParams: TALStringListW;
            constructor Create(const AOwner: TItem); reintroduce; virtual;
            destructor Destroy; override;
            Property Owner: TItem read GetOwner;
          end;
      public
        const
          MainContentType = 1; {core content}
          LoadingContentType = 2; {core content}
          ErrorContentType = 3; {core content}
        type
          TContentBuilderContext = class;
          TCreateContentEvent = function(const AContext: TContentBuilderContext): TContent of object;
          TCreateMainContentEvent = function(const AContext: TContentBuilderContext): TMainContent of object;
          TCreateLoadingContentEvent = function(const AContext: TContentBuilderContext): TLoadingContent of object;
          TCreateErrorContentEvent = function(const AContext: TContentBuilderContext): TErrorContent of object;
          TContentBuilderContext = Class(TALWorkerContext)
          private
            FParentViewOrientation: TOrientation;
            FContentType: Integer;
            FNewContent: TContent;
            // Since we don't use the Self pointer in these events,
            // we can safely cache them locally without worrying
            // that the reference will be destroyed.
            FOnCreateMainContent: TCreateMainContentEvent;
            FOnCreateLoadingContent: TCreateLoadingContentEvent;
            FOnCreateErrorContent: TCreateErrorContentEvent;
            function GetOwner: TItem;
          protected
            function GetContentAlign(const AContentType: Integer): TALAlignLayout; virtual;
            function GetCreateContentMethod(const AContentType: Integer): TMethod; virtual;
          public
            CustomParams: TALStringListW;
            CacheEngine: TALBufDrawableCacheEngine;
            TargetRect: TALRectD;
            constructor Create(const AOwner: TItem; const AContentType: Integer); reintroduce; virtual;
            destructor Destroy; override;
            Property Owner: TItem read GetOwner;
          end;
      private
        FCacheEngine: TALBufDrawableCacheEngine;
        //--
        FData: TALJSONNodeW; // 8 bytes
        FDownloadDataContext: TDownloadDataContext; // 8 bytes
        FDownloadDataErrorCode: Integer; // 4 byte
        FOnDownloadData: TDownloadDataEvent; // 16 bytes
        //--
        FContentBuilderContext: TContentBuilderContext; // 8 bytes
        FMainContent: TContent; // 8 bytes
        FLoadingContent: TContent; // 8 bytes
        FErrorContent: TContent; // 8 bytes
        FOnCreateMainContent: TCreateMainContentEvent; // 16 bytes
        FOnCreateLoadingContent: TCreateLoadingContentEvent; // 16 bytes
        FOnCreateErrorContent: TCreateErrorContentEvent; // 16 bytes
        FOnShowLoadingContent: TNotifyEvent; // 16 bytes
        FOnShowMainContent: TNotifyEvent; // 16 bytes
        FOnShowErrorContent: TNotifyEvent; // 16 bytes
        //--
        function GetCacheEngine: TALBufDrawableCacheEngine;
        Function _GetHost: TALDynamicListBox;
      protected
        function DownloadData(const AForceReload: Boolean = False): boolean; virtual;
        function CreateDownloadDataContext: TDownloadDataContext; virtual;
        class procedure DownloadDataBackgroundProc(var AContext: Tobject); virtual; // [MultiThread]
        class procedure DownloadDataBackgroundProcFetchData(const AContext: TDownloadDataContext; out AData: TALJSONNodeW; var AErrorCode: Integer); virtual; // [MultiThread]
        class procedure DownloadDataBackgroundProcInitData(const AContext: TDownloadDataContext; const AErrorCode: Integer; const AData: TALJSONNodeW); virtual; // [MultiThread]
        class function DownloadDataBackgroundProcCanProcessData(const AContext: TDownloadDataContext): boolean; virtual; // [MultiThread]
        procedure DownloadDataProcessData(const AContext: TDownloadDataContext; const AErrorCode: Integer; var AData: TALJSONNodeW); virtual;
        procedure DownloadDataFinished; virtual;
        function CanDownloadData: Boolean; virtual;
        function IsDownloadDataRunning: Boolean;
        Function HasDataBeenDownloaded: Boolean;
        procedure CancelDownloadData;
        //--
        function CanCreateContent(const AContentType: Integer): Boolean; virtual;
        function CreateContentBuilderContext(const AContentType: Integer): TContentBuilderContext; virtual;
        class function CreateContent(const AContentType: Integer; const AContext: TContentBuilderContext): TContent; virtual;
        class procedure CreateContextContent(const AContext: TContentBuilderContext); virtual;
        procedure DoShiftContent(var ASrcContent: TContent; var ADstContent: TContent); virtual;
        procedure ShiftContent(const AContext: TContentBuilderContext); virtual;
        class procedure DoPreloadContent(var AContext: Tobject); virtual;
        procedure PreloadContent(const AContentType: Integer); virtual;
        procedure TryPreloadContent(const AContentType: Integer); virtual;
        procedure CancelPreloadContent; virtual;
        function IsPreloadingContent: Boolean; virtual; // [MultiThread]
        procedure CreateAndActivateContent(const AContentType: Integer); virtual;
        procedure TryCreateAndActivateContent(const AContentType: Integer); virtual;
        procedure ActivateContent(const AContentType: Integer); virtual;
        procedure FetchContent(const APreload: boolean); overload; virtual;
        procedure FetchContent; overload; virtual;
        //--
        procedure SetHost(Const Value: TALDynamicControlHost); override;
        function GetParentView: TView; virtual;
        procedure VisibleChanged; override;
        procedure SizeChanged; override;
        procedure PaintInternal(const ACanvas: TCanvas); override;
        function GetAbsoluteDisplayedRect: TRectF; override;
        procedure DoRemoveControl(const AControl: TALDynamicControl); override;
        function DoGetDownloadPriority: Int64; override;
      public
        constructor Create(const AOwner: TObject); override;
        destructor Destroy; override;
        procedure BeforeDestruction; override;
        function HasUnconstrainedAutosizeWidth: Boolean; override;
        function HasUnconstrainedAutosizeHeight: Boolean; override;
        function IsReadyToDisplay(const AStrict: Boolean = False): Boolean; override;
        procedure Prepare; virtual;
        procedure Unprepare; virtual;
        Property ParentView: TView read GetParentView;
        property Host: TALDynamicListBox read _GetHost;
        property CacheEngine: TALBufDrawableCacheEngine read GetCacheEngine;
        property Data: TALJSONNodeW read FData;
        property MainContent: TContent read FMainContent;
        property LoadingContent: TContent read FLoadingContent;
        property ErrorContent: TContent read FErrorContent;
        property OnDownloadData: TDownloadDataEvent read FOnDownloadData write FOnDownloadData; // [MultiThread]
        property OnCreateMainContent: TCreateMainContentEvent read FOnCreateMainContent write FOnCreateMainContent; // [MultiThread]
        property OnCreateLoadingContent: TCreateLoadingContentEvent read FOnCreateLoadingContent write FOnCreateLoadingContent; // [MultiThread]
        property OnCreateErrorContent: TCreateErrorContentEvent read FOnCreateErrorContent write FOnCreateErrorContent; // [MultiThread]
        property OnShowLoadingContent: TNotifyEvent read FOnShowLoadingContent write FOnShowLoadingContent;
        property OnShowMainContent: TNotifyEvent read FOnShowMainContent write FOnShowMainContent;
        property OnShowErrorContent: TNotifyEvent read FOnShowErrorContent write FOnShowErrorContent;
      end;
      // -----
      // TView
      TView = class(TItem)
      public
        type
          // ------------
          // TMainContent
          TMainContent = class(TItem.TMainContent)
          private
            Type
              TRealignEvent = procedure(const AContent: TMainContent; const AStartIndex: integer) of object;
          private
            FOnRealign: TRealignEvent; // 8 bytes
            function GetOwner: TView;
          protected
            procedure SetOwner(const Value: TView); reintroduce; virtual;
            procedure DoInsertControl(const AControl: TALDynamicControl; const AIndex: Integer); override;
            procedure DoRemoveControl(const AControl: TALDynamicControl); override;
            procedure DoMoveControl(const AControl: TALDynamicControl; const ANewIndex: Integer); override;
            procedure DoRealign(const AStartIndex: integer); reintroduce; overload; virtual;
            procedure DoRealign; overload; override;
            procedure Realign(const AStartIndex: integer); overload;
            procedure AdjustSize; override;
            procedure DoResized; override;
            function GetFirstVisibleObjectIndex: Integer; override;
            function GetLastVisibleObjectIndex: Integer; override;
            function PaintChildrenOnly: Boolean; override;
          public
            constructor Create(const AOwner: TItem); override;
            property Owner: TView read GetOwner write SetOwner;
            procedure InsertItems(const AItems: TArray<TItem>; const AIndex: Integer); virtual;
            function GetTopBarSize: Single;
            property OnRealign: TRealignEvent read FOnRealign write FOnRealign;
          end;
          // --------
          // TContent
          TContent = class(TItem.TContent)
          private
            function GetOwner: TView;
          protected
            procedure SetOwner(const Value: TView); reintroduce; virtual;
          public
            property Owner: TView read GetOwner write SetOwner;
          end;
          // ---------------
          // TNoItemsContent
          TNoItemsContent = class(TContent);
          // -------------------
          // TSurroundingContent
          TSurroundingContent = class(TContent)
          private
            FShowWithMainContent: boolean;
            FShowWithLoadingContent: boolean;
            FShowWithErrorContent: boolean;
            FShowWithNoItemsContent: boolean;
          public
            constructor Create(const AOwner: TItem); override;
            property ShowWithMainContent: Boolean read FShowWithMainContent write FShowWithMainContent;
            property ShowWithLoadingContent: Boolean read FShowWithLoadingContent write FShowWithLoadingContent;
            property ShowWithErrorContent: Boolean read FShowWithErrorContent write FShowWithErrorContent;
            property ShowWithNoItemsContent: Boolean read FShowWithNoItemsContent write FShowWithNoItemsContent;
          end;
          // ------------------
          // TBackgroundContent
          TBackgroundContent = class(TSurroundingContent);
          // ------------------
          // TForegroundContent
          TForegroundContent = class(TSurroundingContent)
          public
            constructor Create(const AOwner: TItem); override;
          end;
          // -------
          // TTopBar
          TTopBar = class(TSurroundingContent)
          private
            FhidesOnScroll: Boolean;
          protected
            procedure VisibleChanged; override;
            function IsInMotion: boolean; override;
          public
            constructor Create(const AOwner: TItem); override;
            property hidesOnScroll: Boolean read FhidesOnScroll write FhidesOnScroll;
          end;
          // ----------
          // TBottomBar
          TBottomBar = class(TSurroundingContent)
          private
            FhidesOnScroll: Boolean;
          protected
            procedure VisibleChanged; override;
            function IsInMotion: boolean; override;
          public
            constructor Create(const AOwner: TItem); override;
            property hidesOnScroll: Boolean read FhidesOnScroll write FhidesOnScroll;
          end;
          // -----------------------
          // TBasePullToRefreshIndicator
          TBasePullToRefreshIndicator = class(TContent)
          private
            FPullThreshold: Single;
            FPullProgress: Single;
            FCanTriggerRefresh: boolean;
            FIsRefreshing: Boolean;
          protected
            procedure SetPullProgress(const AValue: Single); virtual;
            procedure SetIsRefreshing(const AValue: Boolean); virtual;
          public
            constructor Create(const AOwner: TItem); override;
            property PullThreshold: Single read FPullThreshold write FPullThreshold;
            property PullProgress: Single read FPullProgress write SetPullProgress;
            property CanTriggerRefresh: Boolean read FCanTriggerRefresh write FCanTriggerRefresh;
            property IsRefreshing: Boolean read FIsRefreshing write SetIsRefreshing;
          end;
          // -----------------------
          // TPullToRefreshIndicator
          TPullToRefreshIndicator = class(TBasePullToRefreshIndicator)
          private
            FPullingPhaseAniIndicator: TALDynamicAniIndicator;
            FRefreshingPhaseAniIndicator: TALDynamicAniIndicator;
          protected
            function GetDefaultSize: TSizeF; override;
            procedure SetPullProgress(const Value: Single); override;
            procedure SetIsRefreshing(const AValue: Boolean); override;
          public
            constructor Create(const AOwner: TItem); override;
            property PullingPhaseAniIndicator: TALDynamicAniIndicator read FPullingPhaseAniIndicator;
            property RefreshingPhaseAniIndicator: TALDynamicAniIndicator read FRefreshingPhaseAniIndicator;
          end;
          // ------------------
          // TLoadMoreIndicator
          TLoadMoreIndicator = class(TContent)
          public
            constructor Create(const AOwner: TItem); override;
          end;
          // --------------------
          // TLoadMoreRetryButton
          TLoadMoreRetryButton = class(TContent)
          public
            constructor Create(const AOwner: TItem); override;
          end;
      public
        const
          DefaultPreloadItemCount = 10;
        type
          TItemIdType = (Unknown, Int64, Text);
          TScrollDirection = (FromBeginToEnd, FromEndToBegin);
          TViewportPositionChangeEvent = procedure (Sender: TObject; const OldViewportPosition, NewViewportPosition: TALPointD) of object;
      public
        type
          TDownloadItemsContext = class;
          TCreateItemEvent = function(const AContext: TDownloadItemsContext; var AData: TALJSONNodeW): TItem of object;
          TDownloadItemsEvent = procedure(const AContext: TDownloadItemsContext; out AData: TALJSONNodeW; var APaginationToken: String; var AErrorCode: Integer) of object;
          TDownloadItemsContext = Class(TALWorkerContext)
          private
            FOnDownloadItems: TDownloadItemsEvent;
            FOnCreateItem: TCreateItemEvent;
            FOnCreateItemMainContent: TCreateMainContentEvent;
            FOnCreateItemLoadingContent: TCreateLoadingContentEvent;
            FOnCreateItemErrorContent: TCreateErrorContentEvent;
            FOnDownloadItemData: TDownloadDataEvent;
            function GetOwner: TView;
          public
            ItemIdType: TItemIdType;
            ItemIdNodeName: String;
            MaxItems: integer;
            PaginationToken: String;
            CustomParams: TALStringListW;
            constructor Create(const AOwner: TView); reintroduce; virtual;
            destructor Destroy; override;
            Property Owner: TView read GetOwner;
          end;
      public
        const
          NoItemsContentType = 4; {core content}
          BackgroundContentType = 5; {surrounding content}
          ForegroundContentType = 6; {surrounding content}
          TopBarContentType = 7; {surrounding content}
          BottomBarContentType = 8; {surrounding content}
          PullToRefreshIndicatorContentType = 9; {surrounding content}
          LoadMoreIndicatorContentType = 10; {surrounding content}
          LoadMoreRetryButtonContentType = 11; {surrounding content}
        type
          TCreateMainContentEvent = function(const AContext: TItem.TContentBuilderContext): TMainContent of object;
          TCreateNoItemsContentEvent = function(const AContext: TItem.TContentBuilderContext): TNoItemsContent of object;
          TCreateBackgroundContentEvent = function(const AContext: TItem.TContentBuilderContext): TBackgroundContent of object;
          TCreateForegroundContentEvent = function(const AContext: TItem.TContentBuilderContext): TForegroundContent of object;
          TCreateTopBarEvent = function(const AContext: TItem.TContentBuilderContext): TTopBar of object;
          TCreateBottomBarEvent = function(const AContext: TItem.TContentBuilderContext): TBottomBar of object;
          TCreatePullToRefreshIndicatorEvent = function(const AContext: TItem.TContentBuilderContext): TBasePullToRefreshIndicator of object;
          TCreateLoadMoreIndicatorEvent = function(const AContext: TItem.TContentBuilderContext): TLoadMoreIndicator of object;
          TCreateLoadMoreRetryButtonEvent = function(const AContext: TItem.TContentBuilderContext): TLoadMoreRetryButton of object;
          TContentBuilderContext = Class(TItem.TContentBuilderContext)
          private
            OwnerIsMainView: Boolean;
            Padding: TRectF;
            NewBackgroundContent: TItem.TContent;
            NewForegroundContent: TItem.TContent;
            NewTopBar: TItem.TContent;
            NewBottomBar: TItem.TContent;
            NewPullToRefreshIndicator: TItem.TContent;
            NewLoadMoreIndicator: TItem.TContent;
            NewLoadMoreRetryButton: TItem.TContent;
            OnCreateNoItemsContent: TCreateNoItemsContentEvent;
            OnCreateBackgroundContent: TCreateBackgroundContentEvent;
            OnCreateForegroundContent: TCreateForegroundContentEvent;
            OnCreateTopBar: TCreateTopBarEvent;
            OnCreateBottomBar: TCreateBottomBarEvent;
            OnCreatePullToRefreshIndicator: TCreatePullToRefreshIndicatorEvent;
            OnCreateLoadMoreIndicator: TCreateLoadMoreIndicatorEvent;
            OnCreateLoadMoreRetryButton: TCreateLoadMoreRetryButtonEvent;
          protected
            function GetContentAlign(const AContentType: Integer): TALAlignLayout; override;
            function GetCreateContentMethod(const AContentType: Integer): TMethod; override;
          public
            constructor Create(const AOwner: TView; const AContentType: Integer); reintroduce; virtual;
            destructor Destroy; override;
          end;
      {$IFDEF DEBUG}
      private
        class var DisplayDefaultRefreshRate: single;
      private
        fDebugFpsStarted: Boolean;
        fDebugFpsCount: integer;
        fDebugFpsStopWatch: TstopWatch;
        fDebugFpsRenderTimeStopWatch: TstopWatch;
        fDebugAverageFpsCount: integer;
        fDebugAverageFps: double;
      protected
        procedure LogFPS;
      {$ENDIF}
      private
        FOrientation: TOrientation; // 1 byte
        FItemIdType: TItemIdType; // 1 byte
        FScrollDirection: TScrollDirection; // 1 byte
        fScrollCapturedByMe: boolean; // 1 byte
        FHandleMouseEvents: Boolean; // 1 byte
        FIsSettingViewportPosition: Boolean; // 1 byte
        FRefreshTransitionKind: TRefreshTransitionKind; // 1 byte
        fLastViewportPosition: TALPointD; // 16 bytes
        FMouseDownPos: TpointF; // 8 bytes
        FRefreshTransitionAnimation: TALFloatAnimation; // 8 bytes
        FRefreshingTimer: TALDisplayTimer; // 8 bytes
        FRefreshingView: TView; // 8 bytes
        FNoItemsContent: TItem.TContent; // 8 bytes
        FBackgroundContent: TBackgroundContent; // 8 Bytes
        FForegroundContent: TForegroundContent; // 8 Bytes
        FTopBar: TTopBar; // 8 bytes
        FBottomBar: TBottomBar; // 8 bytes
        FPullToRefreshIndicator: TBasePullToRefreshIndicator; // 8 bytes
        FLoadMoreIndicator: TLoadMoreIndicator; // 8 bytes
        FLoadMoreRetryButton: TLoadMoreRetryButton; // 8 bytes
        FItems: ^TArray<TItem>; // 8 bytes
        FItemIdNodeName: String; // 8 bytes
        FUniqueInt64ItemIds: TDictionary<Int64, boolean>; // 8 bytes | Used to deduplicate items that contain "Int64" IDs. Must be locked using LockItemIds/UnLockItemIds before used
        FUniqueTextItemIds: TDictionary<String, boolean>; // 8 bytes | Used to deduplicate items that contain "String" IDs. Must be locked using LockItemIds/UnLockItemIds before used
        FMaxItems: integer; // 4 bytes
        FPreloadItemCount: integer; // 4 bytes
        FScrollEngine: TALScrollEngine; // 8 bytes
        FFirstVisibleItemIndex: integer; // 4 bytes
        FLastVisibleItemIndex: integer; // 4 bytes
        FFirstPreloadedItemIndex: integer; // 4 bytes
        FLastPreloadedItemIndex: integer; // 4 bytes
        FPaginationToken: String; // 8 bytes
        FTriggerDownloadItemsAtIndex: integer; // 4 byte
        FDownloadItemsErrorCode: Integer; // 4 byte
        FDownloadItemsContext: TDownloadItemsContext; // 8 bytes
        FOnDownloadItems: TDownloadItemsEvent; // 16 bytes
        FOnCreateItem: TCreateItemEvent; // 16 bytes
        FOnCreateItemMainContent: TItem.TCreateMainContentEvent; // 16 bytes
        FOnCreateItemLoadingContent: TItem.TCreateLoadingContentEvent; // 16 bytes
        FOnCreateItemErrorContent: TItem.TCreateErrorContentEvent; // 16 bytes
        FOnDownloadItemData: TItem.TDownloadDataEvent; // 16 bytes
        FOnCreateNoItemsContent: TCreateNoItemsContentEvent; // 16 bytes
        FOnCreateBackgroundContent: TCreateBackgroundContentEvent; // 16 bytes
        FOnCreateForegroundContent: TCreateForegroundContentEvent; // 16 bytes
        FOnCreateTopBar: TCreateTopBarEvent; // 16 bytes
        FOnCreateBottomBar: TCreateBottomBarEvent; // 16 bytes
        FOnCreatePullToRefreshIndicator: TCreatePullToRefreshIndicatorEvent; // 16 bytes
        FOnCreateLoadMoreIndicator: TCreateLoadMoreIndicatorEvent; // 16 bytes
        FOnCreateLoadMoreRetryButton: TCreateLoadMoreRetryButtonEvent; // 16 bytes
        FOnShowNoItemsContent: TNotifyEvent; // 16 bytes
        FOnRealignItems: TView.TMainContent.TRealignEvent; // 16 bytes
        FOnViewportPositionChange: TViewportPositionChangeEvent; // 16 bytes
        function GetItemByIndex(Const AIndex: Integer): TItem;
        function GetItemsCount: integer;
        function GetOnCreateMainContent: TCreateMainContentEvent;
        procedure SetOnCreateMainContent(const AValue: TCreateMainContentEvent);
        procedure SetOrientation(const AValue: TOrientation);
        procedure ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
        procedure InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
        procedure InternalMouseMove(Shift: TShiftState; X, Y: Single);
        procedure InternalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
        procedure InternalMouseLeave;
        procedure RefreshTransitionAnimationProcess(Sender: TObject);
        procedure RefreshTransitionAnimationFinish(Sender: TObject);
        procedure RefreshingTimerProcess(Sender: TObject);
      protected
        procedure LockItemIds; virtual;
        procedure UnLockItemIds; virtual;
        function DownloadItems(const AForceReload: Boolean = False): boolean; virtual;
        function CreateDownloadItemsContext: TDownloadItemsContext; virtual;
        class procedure DownloadItemsBackgroundProc(var AContext: Tobject); virtual; // [MultiThread]
        class procedure DownloadItemsBackgroundProcFetchData(const AContext: TDownloadItemsContext; out AData: TALJSONNodeW; var AErrorCode: Integer); virtual; // [MultiThread]
        class procedure DownloadItemsBackgroundProcCreateItems(const AContext: TDownloadItemsContext; const AErrorCode: Integer; const AData: TALJSONNodeW; out AItems: TArray<TItem>); virtual; // [MultiThread]
        class function DownloadItemsBackgroundProcCanProcessItems(const AContext: TDownloadItemsContext): boolean; virtual; // [MultiThread]
        procedure DownloadItemsProcessItems(const AContext: TDownloadItemsContext; const AErrorCode: Integer; var AItems: TArray<TItem>); virtual;
        procedure DownloadItemsFinished; virtual;
        function CanDownloadItems: Boolean; virtual;
        function IsDownloadItemsRunning: Boolean;
        procedure CancelDownloadItems;
        //--
        function IsMainView: Boolean;
        function CanCreateContent(const AContentType: Integer): Boolean; override;
        function CreateContentBuilderContext(const AContentType: Integer): TItem.TContentBuilderContext; override;
        class procedure CreateContextContent(const AContext: TItem.TContentBuilderContext); override; // [MultiThread]
        function CreateDefaultMainContent(const AContext: TItem.TContentBuilderContext): TItem.TMainContent;
        procedure ShiftContent(const AContext: TItem.TContentBuilderContext); override;
        procedure ActivateContent(const AContentType: Integer); override;
        procedure FetchContent(const APreload: boolean); overload; override;
        //--
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
        procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
        procedure MouseLeave; override;
        procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
        procedure ChildrenMouseDown(const AObject: TALDynamicControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
        procedure ChildrenMouseMove(const AObject: TALDynamicControl; Shift: TShiftState; X, Y: Single); override;
        procedure ChildrenMouseUp(const AObject: TALDynamicControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
        procedure ChildrenMouseLeave(const AObject: TALDynamicControl); override;
        function GetViewportPosition: TALPointD; virtual;
        procedure SetViewportPosition(const AValue: TALPointD); virtual;
        function GetScrollEngine: TALScrollEngine; override;
        procedure ScrollEngineChanged(Sender: TObject); virtual;
        procedure ScrollEngineStart(Sender: TObject); virtual;
        procedure ScrollEngineStop(Sender: TObject); virtual;
        procedure UpdateScrollEngineLimits; virtual;
        function FindFirstVisibleItemIndex: integer; virtual;
        function FindLastVisibleItemIndex: integer; virtual;
        function FindLastActiveItem: TItem; virtual;
        property FirstPreloadedItemIndex: integer read FFirstPreloadedItemIndex;
        property LastPreloadedItemIndex: integer read FLastPreloadedItemIndex;
        property ScrollDirection: TScrollDirection read FScrollDirection;
        procedure PaintInternal(const ACanvas: TCanvas); override;
        procedure DoRemoveControl(const AControl: TALDynamicControl); override;
        procedure SetHost(Const Value: TALDynamicControlHost); override;
      public
        constructor Create(const AOwner: TObject); override;
        destructor Destroy; override;
        procedure BeforeDestruction; override;
        function IsReadyToDisplay(const AStrict: Boolean = False): Boolean; override;
        procedure Prepare; override;
        procedure Unprepare; override;
        function HasMoreItemsToDownload: Boolean; virtual;
        function RetryDownloadItems: boolean; virtual;
        function ScrollToItemIndex(const AIndex: Integer; Const ADuration: integer): Boolean; virtual;
        function ScrollToItem(const AId: String; Const ADuration: integer): Boolean; overload; virtual;
        function ScrollToItem(const AId: Int64; Const ADuration: integer): Boolean; overload; virtual;
        procedure Refresh; virtual;
        property RefreshTransitionKind: TRefreshTransitionKind read FRefreshTransitionKind write FRefreshTransitionKind;
        property PaginationToken: String read FPaginationToken;
        property FirstVisibleItemIndex: integer read FFirstVisibleItemIndex;
        property LastVisibleItemIndex: integer read FLastVisibleItemIndex;
        property Items[const Index: Integer]: TItem read GetItemByIndex;
        property ItemsCount: integer read GetItemsCount;
        property ViewportPosition: TALPointD read GetViewportPosition;
        property ItemIdNodeName: String read FItemIdNodeName write FItemIdNodeName;
        property MaxItems: integer read FMaxItems write FMaxItems;
        property PreloadItemCount: Integer read FPreloadItemCount write FPreloadItemCount;
        property Orientation: TOrientation read FOrientation write SetOrientation;
        property NoItemsContent: TItem.TContent read FNoItemsContent;
        property BackgroundContent: TBackgroundContent read FBackgroundContent;
        property ForegroundContent: TForegroundContent read FForegroundContent;
        property TopBar: TTopBar read FTopBar;
        property BottomBar: TBottomBar read FBottomBar;
        property PullToRefreshIndicator: TBasePullToRefreshIndicator read FPullToRefreshIndicator;
        property LoadMoreIndicator: TLoadMoreIndicator read FLoadMoreIndicator;
        property OnDownloadItems: TDownloadItemsEvent read FOnDownloadItems write FOnDownloadItems; // [MultiThread]
        property OnCreateItem: TCreateItemEvent read FOnCreateItem write FOnCreateItem; // [MultiThread]
        property OnCreateItemMainContent: TItem.TCreateMainContentEvent read FOnCreateItemMainContent write FOnCreateItemMainContent; // [MultiThread]
        property OnCreateItemLoadingContent: TItem.TCreateLoadingContentEvent read FOnCreateItemLoadingContent write FOnCreateItemLoadingContent; // [MultiThread]
        property OnCreateItemErrorContent: TItem.TCreateErrorContentEvent read FOnCreateItemErrorContent write FOnCreateItemErrorContent; // [MultiThread]
        property OnDownloadItemData: TItem.TDownloadDataEvent read FOnDownloadItemData write FOnDownloadItemData; // [MultiThread]
        property OnCreateMainContent: TCreateMainContentEvent read GetOnCreateMainContent write SetOnCreateMainContent; // [MultiThread]
        property OnCreateNoItemsContent: TCreateNoItemsContentEvent read FOnCreateNoItemsContent write FOnCreateNoItemsContent; // [MultiThread]
        property OnCreateBackgroundContent: TCreateBackgroundContentEvent read FOnCreateBackgroundContent write FOnCreateBackgroundContent; // [MultiThread]
        property OnCreateForegroundContent: TCreateForegroundContentEvent read FOnCreateForegroundContent write FOnCreateForegroundContent; // [MultiThread]
        property OnCreateTopBar: TCreateTopBarEvent read FOnCreateTopBar write FOnCreateTopBar; // [MultiThread]
        property OnCreateBottomBar: TCreateBottomBarEvent read FOnCreateBottomBar write FOnCreateBottomBar; // [MultiThread]
        property OnCreatePullToRefreshIndicator: TCreatePullToRefreshIndicatorEvent read FOnCreatePullToRefreshIndicator write FOnCreatePullToRefreshIndicator; // [MultiThread]
        property OnCreateLoadMoreIndicator: TCreateLoadMoreIndicatorEvent read FOnCreateLoadMoreIndicator write FOnCreateLoadMoreIndicator; // [MultiThread]
        property OnCreateLoadMoreRetryButton: TCreateLoadMoreRetryButtonEvent read FOnCreateLoadMoreRetryButton write FOnCreateLoadMoreRetryButton; // [MultiThread]
        property OnShowNoItemsContent: TNotifyEvent read FOnShowNoItemsContent write FOnShowNoItemsContent;
        property OnRealignItems: TView.TMainContent.TRealignEvent read FOnRealignItems write FOnRealignItems;
        property OnViewportPositionChange: TViewportPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
      end;
      // --------------------
      // TCreateMainViewEvent
      TCreateMainViewEvent = function(Const AListBox: TALDynamicListBox): TView of object;
  private
    FMainView: TView;
    FRefreshingView: TView;
    FRefreshTransitionKind: TRefreshTransitionKind;
    FPreloadItemCount: Integer;
    FActiveScrollEnginesCount: Integer;
    FDisableMouseWheel: Boolean;
    FHasBeenPrepared: Boolean;
    FOnDownloadItems: TView.TDownloadItemsEvent; // [MultiThread]
    FOnDownloadItemData: TItem.TDownloadDataEvent; // [MultiThread]
    FOnCreateMainView: TCreateMainViewEvent; // [MultiThread]
    FOnCreateLoadingContent: TItem.TCreateLoadingContentEvent; // [MultiThread]
    FOnCreateErrorContent: TItem.TCreateErrorContentEvent; // [MultiThread]
    FOnCreateNoItemsContent: TView.TCreateNoItemsContentEvent; // [MultiThread]
    FOnCreateBackgroundContent: TView.TCreateBackgroundContentEvent; // [MultiThread]
    FOnCreateForegroundContent: TView.TCreateForegroundContentEvent; // [MultiThread]
    FOnCreateTopBar: TView.TCreateTopBarEvent; // [MultiThread]
    FOnCreateBottomBar: TView.TCreateBottomBarEvent; // [MultiThread]
    FOnCreatePullToRefreshIndicator: TView.TCreatePullToRefreshIndicatorEvent; // [MultiThread]
    FOnCreateLoadMoreIndicator: TView.TCreateLoadMoreIndicatorEvent; // [MultiThread]
    FOnCreateLoadMoreRetryButton: TView.TCreateLoadMoreRetryButtonEvent; // [MultiThread]
    FOnCreateItem: TView.TCreateItemEvent; // [MultiThread]
    FOnCreateItemMainContent: TItem.TCreateMainContentEvent; // [MultiThread]
    FOnCreateItemLoadingContent: TItem.TCreateLoadingContentEvent; // [MultiThread]
    FOnCreateItemErrorContent: TItem.TCreateErrorContentEvent; // [MultiThread]
    FOnShowLoadingContent: TNotifyEvent;
    FOnShowMainContent: TNotifyEvent;
    FOnShowErrorContent: TNotifyEvent;
    FOnShowNoItemsContent: TNotifyEvent;
    FOnRealignItems: TView.TMainContent.TRealignEvent;
    FOnViewportPositionChange: TView.TViewportPositionChangeEvent;
    function GetHasActiveScrollEngines: Boolean;
    procedure SetMainView(const Value: TView);
  protected
    function CreateMainView: TView;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure DoResized; override;
    procedure Paint; override;
    property HasActiveScrollEngines: Boolean read GetHasActiveScrollEngines;
    procedure ShowErrorMessageBanner(const AErrorCode: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetControlAtPos(
               const APos: TALPointD; // APos is local to the control
               out AControlPos: TALPointD; // AControlPos is local to the founded control
               const ACheckHitTest: Boolean = true): TALDynamicControl; overload; override;
    procedure Prepare; virtual;
    function ScrollToItemIndex(const AIndex: Integer; Const ADuration: integer): Boolean;
    function ScrollToItem(const AId: String; Const ADuration: integer): Boolean; overload;
    function ScrollToItem(const AId: Int64; Const ADuration: integer): Boolean; overload;
    property MainView: TView read FMainView write SetMainView;
    property OnCreateMainView: TCreateMainViewEvent read FOnCreateMainView write FOnCreateMainView;
  published
    //property Action;
    property Align;
    property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DisableMouseWheel: Boolean read FDisableMouseWheel write FDisableMouseWheel default False;
    //property ClickSound;
    property ClipChildren default true;
    //property ClipParent;
    //property Cursor;
    //property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property PreloadItemCount: Integer read FPreloadItemCount write FPreloadItemCount default TView.DefaultPreloadItemCount;
    property RefreshTransitionKind: TRefreshTransitionKind read FRefreshTransitionKind write FRefreshTransitionKind default TRefreshTransitionKind.CrossFade;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
    //property TouchTargetExpansion;
    property Visible;
    property Width;
    //--
    property OnCreateLoadingContent: TItem.TCreateLoadingContentEvent read FOnCreateLoadingContent write FOnCreateLoadingContent;
    property OnCreateErrorContent: TItem.TCreateErrorContentEvent read FOnCreateErrorContent write FOnCreateErrorContent; // [MultiThread]
    property OnCreateNoItemsContent: TView.TCreateNoItemsContentEvent read FOnCreateNoItemsContent write FOnCreateNoItemsContent;
    property OnCreateBackgroundContent: TView.TCreateBackgroundContentEvent read FOnCreateBackgroundContent write FOnCreateBackgroundContent;
    property OnCreateForegroundContent: TView.TCreateForegroundContentEvent read FOnCreateForegroundContent write FOnCreateForegroundContent;
    property OnCreateTopBar: TView.TCreateTopBarEvent read FOnCreateTopBar write FOnCreateTopBar;
    property OnCreateBottomBar: TView.TCreateBottomBarEvent read FOnCreateBottomBar write FOnCreateBottomBar;
    property OnCreatePullToRefreshIndicator: TView.TCreatePullToRefreshIndicatorEvent read FOnCreatePullToRefreshIndicator write FOnCreatePullToRefreshIndicator;
    property OnCreateLoadMoreIndicator: TView.TCreateLoadMoreIndicatorEvent read FOnCreateLoadMoreIndicator write FOnCreateLoadMoreIndicator;
    property OnCreateLoadMoreRetryButton: TView.TCreateLoadMoreRetryButtonEvent read FOnCreateLoadMoreRetryButton write FOnCreateLoadMoreRetryButton;
    //--
    property OnCreateItem: TView.TCreateItemEvent read FOnCreateItem write FOnCreateItem; // [MultiThread]
    property OnCreateItemMainContent: TItem.TCreateMainContentEvent read FOnCreateItemMainContent write FOnCreateItemMainContent; // [MultiThread]
    property OnCreateItemLoadingContent: TItem.TCreateLoadingContentEvent read FOnCreateItemLoadingContent write FOnCreateItemLoadingContent;
    property OnCreateItemErrorContent: TItem.TCreateErrorContentEvent read FOnCreateItemErrorContent write FOnCreateItemErrorContent; // [MultiThread]
    property OnDownloadItemData: TItem.TDownloadDataEvent read FOnDownloadItemData write FOnDownloadItemData; // [MultiThread]
    //--
    property OnDownloadItems: TView.TDownloadItemsEvent read FOnDownloadItems write FOnDownloadItems; // [MultiThread]
    property OnRealignItems: TView.TMainContent.TRealignEvent read FOnRealignItems write FOnRealignItems;
    property OnViewportPositionChange: TView.TViewportPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
    //--
    property OnShowLoadingContent: TNotifyEvent read FOnShowLoadingContent write FOnShowLoadingContent;
    property OnShowMainContent: TNotifyEvent read FOnShowMainContent write FOnShowMainContent;
    property OnShowErrorContent: TNotifyEvent read FOnShowErrorContent write FOnShowErrorContent;
    property OnShowNoItemsContent: TNotifyEvent read FOnShowNoItemsContent write FOnShowNoItemsContent;
    //--
    //property OnCanFocus;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    //property OnMouseEnter;
    //property OnMouseLeave;
    //property OnMouseDown;
    //property OnMouseUp;
    //property OnMouseMove;
    //property OnMouseWheel;
    //property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    //property OnPainting;
    //property OnPaint;
    //property OnResize;
    property OnResized;
  end;

Procedure ALDynamicListBoxMakeBufDrawables(const AControl: TALDynamicControl; const AEnsureDoubleBuffered: Boolean = True);

procedure Register;

implementation

uses
  System.Math,
  System.SysUtils,
  System.Math.Vectors,
  {$IF defined(ANDROID)}
  Androidapi.Helpers,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.Helpers,
  Macapi.ObjectiveC,
  iOSApi.Foundation,
  iOSapi.Helpers,
  Alcinoe.iOSapi.UIKit,
  {$ENDIF}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  FMX.types,
  FMX.Utils,
  Alcinoe.FMX.Dialogs,
  Alcinoe.FMX.Graphics,
  Alcinoe.GuardianThread,
  Alcinoe.Localization,
  Alcinoe.StringUtils,
  Alcinoe.HTTP.Client.Net.Pool;

{**}
type
  _TALDynamicControlProtectedAccess = class(TALDynamicControl);
  _TALFloatAnimationProtectedAccess = class(TALFloatAnimation);

{*************************************************************************************************************************}
Procedure ALDynamicListBoxMakeBufDrawables(const AControl: TALDynamicControl; const AEnsureDoubleBuffered: Boolean = True);
begin
  if AEnsureDoubleBuffered then
    AControl.DoubleBuffered := true;
  AControl.MakeBufDrawable;

  for var I := 0 to AControl.ControlsCount - 1 do
    ALDynamicListBoxMakeBufDrawables(AControl.Controls[I], AEnsureDoubleBuffered);
end;

{***************************************************************************}
function TALDynamicListBox.TItem.TContent.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{*****************************************************************************}
function TALDynamicListBox.TItem.TContent.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{***********************************************************************}
constructor TALDynamicListBox.TItem.TContent.Create(const AOwner: TItem);
begin
  inherited create(AOwner);
  HitTest := False;
  Visible := False;
  BeginUpdate;
  IsEphemeral := True;
end;

{*************************************************************}
function TALDynamicListBox.TItem.TContent.CreateFill: TALBrush;
begin
  result := TFill.Create;
end;

{*********************************************************************}
function TALDynamicListBox.TItem.TContent.CreateStroke: TALStrokeBrush;
begin
  result := TStroke.Create;
end;

{***************************************************************************}
constructor TALDynamicListBox.TItem.TMainContent.Create(const AOwner: TItem);
begin
  inherited;
  AutoSize := TALAutoSizeMode.Both;
end;

{***************************************************************************************************}
constructor TALDynamicListBox.TItem.TLoadingContent.TAnimation.Create(const AOwner: TLoadingContent);
Begin
  Inherited create;
  FOwner := AOwner;
  FKind := TAnimationKind.None;
  SetKind(TAnimationKind.Pulse);
  FWaveColor := TalphaColors.White;
  FWaveAngle := 80;
end;

{**********************************************************************************}
function TALDynamicListBox.TItem.TLoadingContent.TAnimation.GetDefaultLoop: Boolean;
begin
  Result := True;
end;

{*************************************************************************************************}
procedure TALDynamicListBox.TItem.TLoadingContent.TAnimation.SetKind(const AValue: TAnimationKind);
begin
  If AValue <> FKind then begin
    Stop;
    FKind := AValue;
    case FKind of
      TAnimationKind.None: begin
        InterpolationType := TALInterpolationType.Linear;
        InterpolationMode := TALInterpolationMode.InOut;
        Delay := 0;
        StartValue := 0;
        StopValue := 1;
        Duration := 1;
        AutoReverse := False;
      end;
      TAnimationKind.Pulse: begin
        // https://mui.com/material-ui/react-skeleton/
        // animation: animation-c7515d 2s ease-in-out 0.5s infinite;
        // 0% {
        //   opacity: 1;
        // }
        // 50% {
        //   opacity: 0.4;
        // }
        // 100% {
        //   opacity: 1;
        // }
        InterpolationType := TALInterpolationType.Quadratic; // similar to ease-in-out
        InterpolationMode := TALInterpolationMode.InOut;
        Delay := 0.5;
        StartValue := 0;
        StopValue := 1;
        Duration := 2;
        AutoReverse := True;
      end;
      TAnimationKind.Wave: begin
        InterpolationType := TALInterpolationType.Quadratic;
        InterpolationMode := TALInterpolationMode.InOut;
        Delay := 0;
        StartValue := -0.5;
        StopValue := 1.5;
        Duration := 2;
        AutoReverse := False;
      end;
    end;
  end;
end;

{*********************************************************************}
procedure TALDynamicListBox.TItem.TLoadingContent.TAnimation.DoProcess;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure UpdateGradient(const AControl: TALDynamicControl);
  begin
    for var I := 0 to AControl.ControlsCount - 1 do begin
      UpdateGradient(AControl.Controls[i]);
      if not (AControl.Controls[i] is TALDynamicShape) then continue;
      var LShape := TALDynamicShape(AControl.Controls[i]);
      if LShape.Width <= 0 then continue;
      LShape.DoubleBuffered := False;
      var LShapeLeft: Single := Fowner.AbsoluteToLocal(LShape.LocalToAbsolute(TpointF.Create(0,0))).X;
      var LGradientCenter: Single := (1/LShape.Width) * ((CurrentValue * Fowner.Width) - LShapeLeft);
      var LGradientRadius: Single := (0.2 * Fowner.Width) / LShape.Width;
      var LGradient := LShape.Fill.Gradient;
      LGradient.Angle := Waveangle;
      LGradient.Style := TGradientStyle.Linear;
      if length(LGradient.Colors) = 0 then begin
        LGradient.Colors := [LShape.Fill.Color, WaveColor, LShape.Fill.Color];
        LShape.Fill.Color := TALphaColors.Null;
      end;
      LGradient.Offsets := [LGradientCenter - LGradientRadius, LGradientCenter, LGradientCenter + LGradientRadius];
    end;
  end;

begin
  inherited;
  if Enabled then begin

    {$IF defined(debug)}
    if (FOwner.Owner = nil) or
       (not (FOwner.Owner is TItem)) then
      Raise Exception.Create('Error 8F54D58D-5E80-49BC-957E-53F7435E2F28');
    {$ENDIF}
    if not Loop then exit;

    Case Kind of

      TAnimationKind.None:;

      TAnimationKind.Pulse: Begin

        // https://mui.com/material-ui/react-skeleton/
        // animation: animation-c7515d 2s ease-in-out 0.5s infinite;
        // 0% {
        //   opacity: 1;
        // }
        // 50% {
        //   opacity: 0.4;
        // }
        // 100% {
        //   opacity: 1;
        // }

        var LOpacity: Single;
        var P: Single := CurrentValue;
        if P <= 0.5 then
          // First half: opacity from 1 → 0.4
          LOpacity := 1 - (P / 0.5) * (1 - 0.4)
        else
          // Second half: opacity from 0.4 → 1
          LOpacity := 0.4 + ((P - 0.5) / 0.5) * (1 - 0.4);

        for var I := 0 to FOwner.ControlsCount - 1 do
          FOwner.Controls[i].Opacity := LOpacity;

      End;

      TAnimationKind.Wave: Begin
        UpdateGradient(FOwner);
      End;

      else
        Raise Exception.Create('Error 3E43ABAE-7A19-41F7-A2A0-332020C068D8')

    end;

    if FOwner.Owner.IsReadyToDisplay(False{AStrict}) then begin
      // We cannot call TItem(FOwner.Owner).ActivateContent(TItem.MainContentType)
      // from here because it will destroy the LoadingContent and the Animation as well.
      // Moreover, destroying an animation during its DoProcess event risks an access violation.
      Loop := False;
      Duration := 0;
      StartValue := 0;
      StopValue := 0;
      AutoReverse := False;
      exit;
    end;

    FOwner.Repaint;

  end;
end;

{********************************************************************}
procedure TALDynamicListBox.TItem.TLoadingContent.TAnimation.DoFinish;
begin
  inherited;
  if Enabled then begin
    {$IF defined(debug)}
    if (FOwner.Owner = nil) or
       (not (FOwner.Owner is TItem)) then
      Raise Exception.Create('Error 89D24C99-AA44-4148-8E7E-1A26E2ECAFC8');
    {$ENDIF}
    FOwner.Visible := False;
    TItem(FOwner.Owner).ActivateContent(TItem.MainContentType);
  end;
end;

{******************************************************************************}
constructor TALDynamicListBox.TItem.TLoadingContent.Create(const AOwner: TItem);
begin
  inherited;
  FAnimation := TAnimation.Create(Self);
end;

{*********************************************************}
destructor TALDynamicListBox.TItem.TLoadingContent.Destroy;
begin
  AlFreeAndNil(FAnimation);
  inherited;
end;

{******************************************************************}
procedure TALDynamicListBox.TItem.TLoadingContent.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  FAnimation.Enabled := False;
  Inherited;
end;

{******************************************************}
procedure TALDynamicListBox.TItem.TLoadingContent.Paint;
begin
  FAnimation.Start;
  inherited;
end;

{***********************************************************************************}
constructor TALDynamicListBox.TItem.TDownloadDataContext.Create(const AOwner: TItem);
begin
  inherited Create(AOwner);
  CustomParams := TALStringListW.Create;
  FOnDownloadData := AOwner.OnDownloadData;
end;

{**************************************************************}
destructor TALDynamicListBox.TItem.TDownloadDataContext.Destroy;
begin
  ALFreeAndNil(CustomParams);
  inherited Destroy;
end;

{********************************************************************}
function TALDynamicListBox.TItem.TDownloadDataContext.GetOwner: TItem;
begin
  Result := TItem(FOwner);
end;

{******************************************************************************************************************}
constructor TALDynamicListBox.TItem.TContentBuilderContext.Create(const AOwner: TItem; const AContentType: Integer);
begin
  inherited Create(AOwner);
  CustomParams := TALStringListW.Create;
  {$IF defined(debug)}
  if AOwner.CacheEngine = nil then
    Raise Exception.Create('Error A377D825-737D-4FB5-8EAF-3A28680DAB6B');
  {$ENDIF}
  CacheEngine := AOwner.CacheEngine.IncreaseRefCount;
  TargetRect := AOwner.LocalRect ;
  if AOwner.ParentView = nil then begin
    {$IF defined(debug)}
    if not (AOwner is TView) then
      Raise Exception.Create('Error 9228ECC4-9E4D-45FE-805F-B234DCF08372');
    {$ENDIF}
    FParentViewOrientation := TView(AOwner).orientation
  end
  else if AOwner is TView then
    FParentViewOrientation := TView(AOwner).orientation
  else
    FParentViewOrientation := AOwner.ParentView.orientation;
  FContentType := AContentType;
  //-
  FNewContent := nil;
  //-
  if AOwner.FMainContent = nil then FOnCreateMainContent := AOwner.OnCreateMainContent
  else FOnCreateMainContent := nil;
  //-
  if AOwner.FLoadingContent = nil then FOnCreateLoadingContent := AOwner.OnCreateLoadingContent
  else FOnCreateLoadingContent := nil;
  //-
  if AOwner.FErrorContent = nil then FOnCreateErrorContent := AOwner.OnCreateErrorContent
  else FOnCreateErrorContent := nil;
end;

{****************************************************************}
destructor TALDynamicListBox.TItem.TContentBuilderContext.Destroy;
begin
  ALFreeAndNil(CustomParams);
  CacheEngine.DecreaseRefCount;
  ALFreeAndNil(FNewContent);
  inherited Destroy;
end;

{**********************************************************************}
function TALDynamicListBox.TItem.TContentBuilderContext.GetOwner: TItem;
begin
  Result := TItem(FOwner);
end;

{**************}
// [MultiThread]
function TALDynamicListBox.TItem.TContentBuilderContext.GetContentAlign(const AContentType: Integer): TALAlignLayout;
begin
  //MainContentType
  //LoadingContentType
  //ErrorContentType
  If (FParentViewOrientation = TOrientation.Horizontal) then Result := TALAlignLayout.Left
  else Result := TALAlignLayout.Top;
end;

{**************}
// [MultiThread]
function TALDynamicListBox.TItem.TContentBuilderContext.GetCreateContentMethod(const AContentType: Integer): TMethod;
begin
  case AContentType of
    MainContentType: Result := TMethod(FOnCreateMainContent);
    LoadingContentType: Result := TMethod(FOnCreateLoadingContent);
    ErrorContentType: Result := TMethod(FOnCreateErrorContent);
    else Raise Exception.Create('Error EFDF75EE-BBDC-48FF-BE8E-397C0729F2E7');
  end;
end;

{****************************************************************}
constructor TALDynamicListBox.TItem.Create(const AOwner: TObject);
begin
  inherited create(AOwner);
  AutoSize := TALAutoSizeMode.Both;
  Align := TALAlignLayout.None;
  //IsEphemeral := False;
  //--
  FData := nil;
  FDownloadDataContext := nil;
  FDownloadDataErrorCode := 0;
  FOnDownloadData := nil;
  //--
  FContentBuilderContext := nil;
  FMainContent := nil;
  FLoadingContent := nil;
  FErrorContent := nil;
  FOnCreateMainContent := nil;
  FOnCreateLoadingContent := nil;
  FOnCreateErrorContent := nil;
  FOnShowLoadingContent := nil;
  FOnShowMainContent := nil;
  FOnShowErrorContent := nil;
end;

{*****************************************}
destructor TALDynamicListBox.TItem.Destroy;
begin
  AlFreeAndNil(FData);
  Inherited Destroy;
end;

{**************************************************}
procedure TALDynamicListBox.TItem.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  CancelDownloadData;
  CancelPreloadContent;
  inherited;
end;

{**********************************************************************}
function TALDynamicListBox.TItem.HasUnconstrainedAutosizeWidth: Boolean;
begin
  Result := GetAutoSize in [TALAutoSizeMode.Both, TALAutoSizeMode.Width];
  if Result then begin
    result := (ParentView = nil) or
              (ParentView.Orientation <> TOrientation.Vertical);
    if (not result) and (Owner <> nil) then
      Result := Owner.HasUnconstrainedAutosizeWidth;
  end;
end;

{***********************************************************************}
function TALDynamicListBox.TItem.HasUnconstrainedAutosizeHeight: Boolean;
begin
  Result := GetAutoSize in [TALAutoSizeMode.Both, TALAutoSizeMode.Height];
  if Result then begin
    result := (ParentView = nil) or
              (ParentView.Orientation <> TOrientation.horizontal);
    if (not result) and (Owner <> nil) then
      Result := Owner.HasUnconstrainedAutosizeHeight;
  end;
end;

{*****************************************************************************************}
function TALDynamicListBox.TItem.IsReadyToDisplay(const AStrict: Boolean = False): Boolean;
begin
  Result := ((FMainContent <> nil) or (FErrorContent <> nil)) and Inherited;
end;

{***********************************************************************************}
procedure TALDynamicListBox.TItem.DoRemoveControl(const AControl: TALDynamicControl);
begin
  if Acontrol = FMainContent then FMainContent := nil
  else if Acontrol = FLoadingContent then FLoadingContent := nil
  else if Acontrol = FErrorContent then FErrorContent := nil;
  inherited;
end;

{************************************************************}
function TALDynamicListBox.TItem.DoGetDownloadPriority: Int64;
begin

  Result := 0;
  if (Index <> -1) then begin
    var LParentView := ParentView;
    if LParentView <> nil then begin
      if (Index >= LParentView.FirstVisibleItemIndex) and
         (Index <= LParentView.LastVisibleItemIndex) then
        Result := 0
      //--
      else if LParentView.ScrollDirection = TView.TScrollDirection.FromBeginToEnd then begin
        if Index > LParentView.LastVisibleItemIndex then
          Result := Index - LParentView.LastVisibleItemIndex // > 0
        else
          Result := Index - LParentView.FirstVisibleItemIndex // < 0
      end
      //--
      else begin
        if Index < LParentView.FirstVisibleItemIndex then
          Result := LParentView.FirstVisibleItemIndex - Index // > 0
        else
          Result := LParentView.LastVisibleItemIndex - Index // < 0
      end;
    end;
  end;

  // This method is called from a background thread, so Owner
  // might be modified concurrently. To ensure thread safety,
  // we assign it to a local variable before using it.
  var LOwner := Owner;
  if LOwner <> nil then begin
    var LOwnerPriority := _TALDynamicControlProtectedAccess(LOwner).DoGetDownloadPriority;
    var LResult := Result;
    Result := Abs(LResult) + abs(LOwnerPriority);
    if (LOwnerPriority < 0) or (LResult < 0) then
      Result := -1 * Result;
  end;

end;

{****************************************}
procedure TALDynamicListBox.TItem.Prepare;
begin
  {$IFDEF DEBUG}
  ALLog(ClassName+'.Prepare', 'Index: ' + ALintToStrW(Index));
  {$ENDIF}
  // FetchContent will invoke DownloadData, and its execution
  // may potentially be deferred until DownloadData is completed.
  FetchContent;
end;

{******************************************}
procedure TALDynamicListBox.TItem.Unprepare;
begin
  {$IFDEF DEBUG}
  ALLog(ClassName+'.Unprepare', 'Index: ' + ALintToStrW(Index));
  {$ENDIF}
  CancelDownloadData;
  CancelPreloadContent;
  for Var I := FControlsCount - 1 downto 0 do
    If Fcontrols[i].IsEphemeral then begin
      var LControl := Fcontrols[i];
      ALFreeAndNil(LControl, true{ADelayed});
    end;
end;

{******************************************************************************************}
function TALDynamicListBox.TItem.DownloadData(const AForceReload: Boolean = False): boolean;
begin

  // Exit if the last download resulted in an error, unless AForceReload is True
  // (e.g., when triggered by a "Reload" button click)
  if (not AForceReload) and
     (FDownloadDataErrorCode <> 0) then exit(False);

  // Exit if no data is needed
  if not CanDownloadData then exit(false);

  // Exit if a thread is already performing the task
  if IsDownloadDataRunning then exit(true);

  // Before starting the background thread
  {$IFDEF DEBUG}
  ALLog(ClassName+'.DownloadData', 'ForceReload: ' + ALBoolToStrW(AForceReload));
  {$ENDIF}
  FDownloadDataErrorCode := 0;

  // Load the data in a separate thread to avoid blocking the calling thread
  FDownloadDataContext := CreateDownloadDataContext;
  Try
    TALNetHttpClientPool.Instance.ExecuteProc(
      DownloadDataBackgroundProc, // const AProc: TALWorkerThreadObjProc;
      FDownloadDataContext, // const AContext: Tobject; // Context will be free by the worker thread
      GetDownloadPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
  except
    ALFreeAndNil(FDownloadDataContext);
    Raise;
  End;

  // Return True
  result := true;

end;

{*******************************************************************************}
function TALDynamicListBox.TItem.CreateDownloadDataContext: TDownloadDataContext;
begin
  Result := TDownloadDataContext.Create(Self);
end;

{****************************************************************************************}
class procedure TALDynamicListBox.TItem.DownloadDataBackgroundProc(var AContext: Tobject);
begin
  var LContext := TDownloadDataContext(AContext);
  if LContext.FOwner = nil then exit;
  try

    var LFreeData := True;
    var LData: TALJSONNodeW := nil;
    Try

      var LDownloadDataErrorCode: Integer := 0;
      DownloadDataBackgroundProcFetchData(LContext, LData, LDownloadDataErrorCode);

      if LContext.FOwner = nil then exit;
      DownloadDataBackgroundProcInitData(LContext, LDownloadDataErrorCode, LData);

      while not DownloadDataBackgroundProcCanProcessData(LContext) do begin
        if LContext.FOwner = nil then exit;
        sleep(250);
      end;

      if LContext.FOwner = nil then exit;
      TThread.queue(nil,
        procedure
        begin
          Try
            if LContext.FOwner <> nil then begin
              var LOwner := LContext.Owner;
              LOwner.DownloadDataProcessData(LContext, LDownloadDataErrorCode, LData);
              LOwner.FDownloadDataContext := nil;
              LOwner.DownloadDataFinished;
            end;
          finally
            ALFreeAndNil(LContext);
            ALfreeAndNil(LData);
          End;
        end);
      AContext := nil; // AContext will be free by TThread.queue
      LFreeData := False; // LData will be free by TThread.queue

    finally
      if LFreeData then
        ALfreeAndNil(LData);
    end;

  Except
    On E: Exception do begin
      ALLog('TALDynamicListBox.TItem.DownloadDataBackgroundProc', E);
      ALMonitorEnter(LContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TItem.DownloadDataBackgroundProc'{$ENDIF});
      try
        if LContext.FOwner <> nil then begin
          LContext.FManagedByWorkerThread := False;
          AContext := nil; // AContext will be free by CancelResourceDownload
        end;
      finally
        ALMonitorExit(LContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TItem.DownloadDataBackgroundProc'{$ENDIF});
      end;
    end;
  end;
end;

{**************}
// [MultiThread]
class procedure TALDynamicListBox.TItem.DownloadDataBackgroundProcFetchData(
                  const AContext: TDownloadDataContext;
                  out AData: TALJSONNodeW;
                  var AErrorCode: Integer);
begin

  // We cannot call ALMonitorEnter(AContext.FLock) here because
  // AContext.FOnDownloadData performs a long HTTP request.
  // In the meantime, CancelDownloadData might be called, and it
  // also needs to acquire the same lock to set AContext.FOwner to nil.
  // Therefore, it's the responsibility of FOnDownloadData to call
  // ALMonitorEnter(AContext.FLock) if it needs to access AContext.FOwner.

  if AContext.FOwner = nil then exit;
  if not assigned(AContext.FOnDownloadData) then
    Raise Exception.Create('Error DF2328CA-BCF7-46D6-B100-AFD222FF8873');
  //var LMethod: TMethod;
  //LMethod.Code := TMethod(AContext.FOnDownloadData).Code;
  // Set Self to nil to prevent accidental access to instance members,
  // as we are in a multithreaded context where most members are not thread-safe.
  // Self can still be accessed via AContext.Owner, but this should be done with caution.
  //LMethod.Data := nil;
  //TDownloadDataEvent(LMethod)(
  //  AContext, // const AContext: TDownloadDataContext;
  //  AData, // Const AData: TALJSONNodeW;
  //  AErrorCode); // var AErrorCode: Integer
  AContext.FOnDownloadData(
    AContext, // const AContext: TDownloadDataContext;
    AData, // Const AData: TALJSONNodeW;
    AErrorCode); // var AErrorCode: Integer

end;

{**************}
// [MultiThread]
class procedure TALDynamicListBox.TItem.DownloadDataBackgroundProcInitData(
                  const AContext: TDownloadDataContext;
                  const AErrorCode: Integer;
                  const AData: TALJSONNodeW);
begin
  if (AErrorCode = 0) and (AData <> nil) then begin
    AData.ChildNodes.SetSorted(true{Value},true{recurse});
    AData.MultiThreadPrepare(true{aOnlyChildList});
  end;
end;

{**************}
// [MultiThread]
class function TALDynamicListBox.TItem.DownloadDataBackgroundProcCanProcessData(const AContext: TDownloadDataContext): boolean; // [MultiThread]
begin
  if TThread.Current.ThreadID = MainThreadID then exit(true);
  ALMonitorEnter(AContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TItem.DownloadDataBackgroundProcCanProcessData'{$ENDIF});
  try
    // Primarily because we want to prevent the list
    // from being updated during the bottom-bound animation.
    var LOwner := AContext.Owner;
    result := (LOwner = nil) or
              (LOwner.Host = nil) or
              (not LOwner.Host.HasActiveScrollEngines);
  finally
    ALMonitorExit(AContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TItem.DownloadDataBackgroundProcCanProcessData'{$ENDIF});
  end;
end;

{********************************************************}
procedure TALDynamicListBox.TItem.DownloadDataProcessData(
            const AContext: TDownloadDataContext;
            const AErrorCode: Integer;
            var AData: TALJSONNodeW);
begin
  FDownloadDataErrorCode := AErrorCode;
  if FDownloadDataErrorCode = 0 then begin
    ALfreeAndNil(FData);
    Fdata := AData;
    AData := nil;
    FOnDownloadData := Nil;
  end
  else ALfreeAndNil(AData);
end;

{*****************************************************}
procedure TALDynamicListBox.TItem.DownloadDataFinished;
begin
  if FDownloadDataErrorCode <> 0 then begin
    if Host <> nil then
      Host.ShowErrorMessageBanner(FDownloadDataErrorCode)
  end
  else
    FetchContent;
end;

{********************************************************}
function TALDynamicListBox.TItem.CanDownloadData: Boolean;
begin
  // Once the data has been downloaded,
  // FOnDownloadData will be set to nil
  Result := assigned(FOnDownloadData);
end;

{**************************************************************}
function TALDynamicListBox.TItem.IsDownloadDataRunning: Boolean;
begin
  result := FDownloadDataContext <> nil;
end;

{**************************************************************}
function TALDynamicListBox.TItem.HasDataBeenDownloaded: Boolean;
begin
  result := (not CanDownloadData) or
            (FDownloadDataErrorCode <> 0);
end;

{***************************************************}
procedure TALDynamicListBox.TItem.CancelDownloadData;
begin
  // The FDownloadDataContext pointer can only be
  // updated in the main thread, so there is no need
  // to lock its access for reading or updating.
  if FDownloadDataContext <> nil then begin
    var LContextToFree: TDownloadDataContext;
    var LLock := FDownloadDataContext.FLock;
    ALMonitorEnter(LLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TItem.CancelDownloadData'{$ENDIF});
    try
      if not FDownloadDataContext.FManagedByWorkerThread then LContextToFree := FDownloadDataContext
      else LContextToFree := nil;
      FDownloadDataContext.FOwner := nil;
      FDownloadDataContext := nil;
    Finally
      ALMonitorExit(LLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TItem.CancelDownloadData'{$ENDIF});
    End;
    ALFreeAndNil(LContextToFree);
  end;
end;

{**************************************************************************************}
function TALDynamicListBox.TItem.CanCreateContent(const AContentType: Integer): Boolean;
begin
  case AContentType of
    MainContentType: result := (FMainContent = nil) and (assigned(FOnCreateMainContent));
    LoadingContentType: result := (FLoadingContent = nil) and (assigned(FOnCreateLoadingContent));
    ErrorContentType: result := (FErrorContent = nil) and (assigned(FOnCreateErrorContent));
    else Raise Exception.Create('Error 5D7FBE4C-AF8D-49FD-B176-67844445A11C')
  end;
end;

{****************************************************************************************************************}
function TALDynamicListBox.TItem.CreateContentBuilderContext(const AContentType: Integer): TContentBuilderContext;
begin
  Result := TItem.TContentBuilderContext.Create(self, AContentType);
end;

{**************}
// [MultiThread]
class function TALDynamicListBox.TItem.CreateContent(const AContentType: Integer; const AContext: TContentBuilderContext): TContent;
begin
  var LMethod := AContext.GetCreateContentMethod(AContentType);
  if LMethod.Code = nil then exit(nil);
  // Set Self to nil to prevent accidental access to instance members,
  // as we are in a multithreaded context where most members are not thread-safe.
  // Self can still be accessed via LContext.Owner, but this should be done with caution.
  //LMethod.Data := nil;
  //--
  ALMonitorEnter(AContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TItem.CreateContent'{$ENDIF});
  Try
    if AContext.FOwner = nil then Exit(nil);
    Result := TCreateContentEvent(LMethod)(AContext);
  finally
    ALMonitorExit(AContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TItem.CreateContent'{$ENDIF});
  End;
  //--
  // We must not allow CreateContent to return nil, because if we do,
  // the framework will attempt to recreate the content on every scroll,
  // since the assigned method is still present.
  if Result = nil then
    Raise Exception.Create('Return value of a CreateContent function cannot be nil');
  if Result.Align = TALAlignLayout.None then
    Result.Align := AContext.GetContentAlign(AContentType);
  Result.EndUpdate;
  ALDynamicListBoxMakeBufDrawables(Result, true{AEnsureDoubleBuffered});
end;

{**************}
// [MultiThread]
class procedure TALDynamicListBox.TItem.CreateContextContent(const AContext: TContentBuilderContext);
begin
  {$IF defined(debug)}
  if AContext.FNewContent <> nil then Raise Exception.Create('Error BA843DF0-3BAE-461B-B1A6-395B6BCCCDD2');
  {$ENDIF}
  AContext.FNewContent := CreateContent(AContext.FContentType, AContext);
end;

{*****************************************************************************************************}
procedure TALDynamicListBox.TItem.DoShiftContent(var ASrcContent: TContent; var ADstContent: TContent);
begin
  If ASrcContent = nil then exit;
  {$IF defined(debug)}
  if ADstContent <> nil then Raise Exception.Create('Error 6DAF294C-74EC-4D46-993B-93809EEEF126');
  {$ENDIF}
  // In case ASrcContent was not created using CreateContextContent or CreateContent
  ASrcContent.EndUpdate;
  ASrcContent.Visible := False;
  AddControl(ASrcContent);
  ADstContent := ASrcContent;
  ASrcContent := nil;
end;

{*************************************************************************************}
procedure TALDynamicListBox.TItem.ShiftContent(const AContext: TContentBuilderContext);
begin
  case AContext.FContentType of
    MainContentType: if AContext.FNewContent <> nil then DoShiftContent(AContext.FNewContent, FMainContent);
    LoadingContentType: if AContext.FNewContent <> nil then DoShiftContent(AContext.FNewContent, FLoadingContent);
    ErrorContentType: if AContext.FNewContent <> nil then DoShiftContent(AContext.FNewContent, FErrorContent);
    else Raise Exception.Create('Error B46A0FA0-2692-4390-B725-F0A787117E8F');
  end;
end;

{**************}
// [MultiThread]
class procedure TALDynamicListBox.TItem.DoPreloadContent(var AContext: Tobject);
begin
  var LContext := TContentBuilderContext(AContext);
  if LContext.FOwner = nil then exit;
  try

    CreateContextContent(LContext);
    if LContext.FOwner = nil then exit;
    TThread.queue(nil,
      procedure
      begin
        Try
          if LContext.FOwner <> nil then begin
            var LOwner := LContext.Owner;
            LOwner.ShiftContent(LContext);
            LOwner.ActivateContent(LContext.FContentType);
            LOwner.FContentBuilderContext := nil;
          end;
        finally
          ALFreeAndNil(LContext, true{ADelayed});
        End;
      end);
    AContext := nil; // AContext will be free by TThread.queue

  Except
    On E: Exception do begin
      ALLog('TALDynamicListBox.TItem.DoPreloadContent', E);
      ALMonitorEnter(LContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TItem.DoPreloadContent'{$ENDIF});
      try
        if LContext.FOwner <> nil then begin
          LContext.FManagedByWorkerThread := False;
          AContext := nil; // AContext will be free by CancelPreloadContent
        end;
      finally
        ALMonitorExit(LContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TItem.DoPreloadContent'{$ENDIF});
      end;
    end;
  end;
end;

{****************************************************************************}
procedure TALDynamicListBox.TItem.PreloadContent(const AContentType: Integer);
begin
  {$IF defined(debug)}
  if FContentBuilderContext <> nil then
    Raise Exception.Create('Error 79557F0A-3566-4E69-BB9D-AF14A904FD4D');
  {$ENDIF}
  FContentBuilderContext := CreateContentBuilderContext(AContentType);
  Try
    TALGraphicThreadPool.Instance.ExecuteProc(
      DoPreloadContent, // const AProc: TALWorkerThreadObjProc;
      FContentBuilderContext, // const AContext: Tobject; // Context will be free by the worker thread
      GetDownloadPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
  except
    ALFreeAndNil(FContentBuilderContext);
    Raise;
  End;
end;

{*******************************************************************************}
procedure TALDynamicListBox.TItem.TryPreloadContent(const AContentType: Integer);
begin
  If (not CanCreateContent(AContentType)) or
     (IsPreloadingContent) then exit;
  PreloadContent(AContentType);
end;

{*****************************************************}
procedure TALDynamicListBox.TItem.CancelPreloadContent;
begin
  // The FContentBuilderContext pointer can only be
  // updated in the main thread, so there is no need
  // to lock its access for reading or updating.
  if FContentBuilderContext <> nil then begin
    var LContextToFree: TContentBuilderContext;
    var LLock := FContentBuilderContext.FLock;
    ALMonitorEnter(LLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TItem.CancelPreloadContent'{$ENDIF});
    try
      if not FContentBuilderContext.FManagedByWorkerThread then LContextToFree := FContentBuilderContext
      else LContextToFree := nil;
      FContentBuilderContext.FOwner := nil;
      FContentBuilderContext := nil;
    Finally
      ALMonitorExit(LLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TItem.CancelPreloadContent'{$ENDIF});
    End;
    ALFreeAndNil(LContextToFree);
  end;
end;

{**************}
// [MultiThread]
function TALDynamicListBox.TItem.IsPreloadingContent: Boolean;
begin
  result := FContentBuilderContext <> nil;
end;

{**************************************************************************************}
procedure TALDynamicListBox.TItem.CreateAndActivateContent(const AContentType: Integer);
begin
  CancelPreloadContent;
  FContentBuilderContext := CreateContentBuilderContext(AContentType);
  Try
    CreateContextContent(FContentBuilderContext);
    ShiftContent(FContentBuilderContext);
    ActivateContent(AContentType);
  finally
    ALFreeAndNil(FContentBuilderContext);
  End;
end;

{*****************************************************************************************}
procedure TALDynamicListBox.TItem.TryCreateAndActivateContent(const AContentType: Integer);
begin
  If CanCreateContent(AContentType) then CreateAndActivateContent(AContentType)
  else ActivateContent(AContentType);
end;

{*****************************************************************************}
procedure TALDynamicListBox.TItem.ActivateContent(const AContentType: Integer);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _HideContent(var AContent: TContent);
  begin
    if AContent.IsEphemeral then ALFreeAndNil(AContent, true{ADelayed})
    else AContent.Visible := False;
  end;

begin
  case AContentType of
    MainContentType: begin
      if (FMainContent = nil) or (FMainContent.Visible) then exit;
      if FLoadingContent <> nil then _HideContent(FLoadingContent);
      if FErrorContent <> nil then _HideContent(FErrorContent);
      FMainContent.Visible := true;
      if assigned(FOnShowMainContent) then FOnShowMainContent(FMainContent);
    end;
    LoadingContentType: begin
      if (FLoadingContent = nil) or (FLoadingContent.Visible) then exit;
      if FMainContent <> nil then _HideContent(FMainContent);
      if FErrorContent <> nil then _HideContent(FErrorContent);
      FLoadingContent.Visible := true;
      if assigned(FOnShowLoadingContent) then FOnShowLoadingContent(FLoadingContent);
    end;
    ErrorContentType: begin
      if (FErrorContent = nil) or (FErrorContent.Visible) then exit;
      if FMainContent <> nil then _HideContent(FMainContent);
      if FLoadingContent <> nil then _HideContent(FLoadingContent);
      FErrorContent.Visible := true;
      if assigned(FOnShowErrorContent) then FOnShowErrorContent(FErrorContent);
    end;
    else
      Raise Exception.Create('Error 2BDD1C9D-E4D9-420C-8F3E-5A2BAEED7F9D')
  end;
end;

{**********************************************************************}
procedure TALDynamicListBox.TItem.FetchContent(const APreload: boolean);
begin

  // If the item is outside the preloaded range, do nothing
  if (ParentView <> nil) and
     ((Index < ParentView.FirstPreloadedItemIndex) or
      (Index > ParentView.LastPreloadedItemIndex)) then exit;

  // If the item is not visible, do nothing
  if not Visible then exit;

  // If data must be downloaded first, fetch loading content
  if DownloadData then begin
    if not APreload then
      TryCreateAndActivateContent(LoadingContentType);
  end

  // fetch error Content
  else if FDownloadDataErrorCode <> 0 then begin
    if APreload then TryPreloadContent(ErrorContentType)
    else TryCreateAndActivateContent(ErrorContentType);
  end

  // fetch main content
  else begin
    if APreload then TryPreloadContent(MainContentType)
    else TryCreateAndActivateContent(MainContentType);
  end;

end;

{*********************************************}
procedure TALDynamicListBox.TItem.FetchContent;
begin
  FetchContent(not IsDisplayed{APreload});
end;

{*************************************************************************}
function TALDynamicListBox.TItem.GetCacheEngine: TALBufDrawableCacheEngine;
begin
  if Host <> nil then Result := Host.CacheEngine
  else if FCacheEngine <> nil then result := FCacheEngine
  else if ParentView <> nil then Result := ParentView.CacheEngine
  else result := nil;
end;

{***********************************************************}
function TALDynamicListBox.TItem._GetHost: TALDynamicListBox;
begin
  Result := TALDynamicListBox(inherited Host);
end;

{****************************************************************************}
procedure TALDynamicListBox.TItem.SetHost(Const Value: TALDynamicControlHost);
begin
  if FHost <> value then begin
    If (Value <> nil) then FCacheEngine := Value.CacheEngine
    else FCacheEngine := nil;
    inherited;
  end;
end;

{****************************************************}
function TALDynamicListBox.TItem.GetParentView: TView;
begin
  // * Items are owned by TMainContent
  // * TMainContent is owned by a view
  if Owner = nil then exit(nil);
  Result := TView(Owner{TMainContent}.owner{TView});
end;

{****************************************************************}
function TALDynamicListBox.TItem.GetAbsoluteDisplayedRect: TRectF;
begin
  if (ParentView <> nil) and
     ((index < ParentView.FirstVisibleItemIndex) or
      (index > ParentView.LastVisibleItemIndex)) then exit(TRectF.Empty);
  Result := Inherited GetAbsoluteDisplayedRect;
end;

{***********************************************}
procedure TALDynamicListBox.TItem.VisibleChanged;
begin
  inherited;
  if (Align = TALAlignLayout.None) and (Owner <> nil) then
    TView.TMainContent(Owner).Realign(Index);
end;

{********************************************}
procedure TALDynamicListBox.TItem.SizeChanged;
begin
  {$IF defined(debug)}
  if (Owner <> nil) and (Align <> TALAlignLayout.None) then
    Raise Exception.Create('Error 2214E98E-D67A-4DA2-A608-E3FE2C976256');
  if (Owner <> nil) and (not (Owner is TView.TMainContent)) then
    Raise Exception.Create('Error 5270D91C-B858-4B85-9475-609AE201786E');
  {$ENDIF}
  if Owner <> nil then
    TView.TMainContent(Owner).Realign(Index);
  inherited;
end;

{**********************************************************************}
procedure TALDynamicListBox.TItem.PaintInternal(const ACanvas: TCanvas);
begin
  FetchContent;
  Inherited;
end;

{***************************************************************************}
constructor TALDynamicListBox.TView.TMainContent.Create(const AOwner: TItem);
begin
  inherited create(AOwner);
  IsEphemeral := False;
  FOnRealign := nil;
end;

{************************************************************}
function TALDynamicListBox.TView.TMainContent.GetOwner: TView;
begin
  Result := TView(inherited Owner);
end;

{**************************************************************************}
procedure TALDynamicListBox.TView.TMainContent.SetOwner(const Value: TView);
begin
  inherited Owner := Value;
end;

{***********************************************************************************************************************}
procedure TALDynamicListBox.TView.TMainContent.DoInsertControl(const AControl: TALDynamicControl; const AIndex: Integer);
begin
  inherited;
  {$IF defined(debug)}
  if AControl.Align <> TALAlignLayout.None then
    Raise Exception.Create('Error E17D1A50-18AE-43D5-AEAF-6E9EA5CC2758');
  {$ENDIF}
  Realign(AControl.Index);
end;

{************************************************************************************************}
procedure TALDynamicListBox.TView.TMainContent.DoRemoveControl(const AControl: TALDynamicControl);
begin
  inherited;
  {$IF defined(debug)}
  if AControl.Align <> TALAlignLayout.None then
    Raise Exception.Create('Error 46D12BC4-BB27-472B-AE2B-1A977646A7AB');
  {$ENDIF}
  Realign(AControl.Index);
end;

{************************************************************************************************************************}
procedure TALDynamicListBox.TView.TMainContent.DoMoveControl(const AControl: TALDynamicControl; const ANewIndex: Integer);
begin
  inherited;
  {$IF defined(debug)}
  if AControl.Align <> TALAlignLayout.None then
    Raise Exception.Create('Error A6D17712-6CDC-474D-A1BD-A0E40A4DA247');
  {$ENDIF}
  Realign(AControl.Index);
end;

{*************************************************************************************************************}
procedure TALDynamicListBox.TView.TMainContent.InsertItems(const AItems: TArray<TItem>; const AIndex: Integer);
begin
  if Length(AItems) = 0 then
    Raise Exception.Create('InsertItems failed: No items provided');
  //--
  var LIndex := Max(0, Min(AIndex, FControlsCount));
  If length(FControls) <= FControlsCount + Length(AItems) - 1 then
    Setlength(FControls, FControlsCount + Length(AItems));
  if LIndex <= FControlsCount - 1 then begin
    ALMove(FControls[LIndex], FControls[LIndex+Length(AItems)], (FControlsCount - 1 - LIndex) * SizeOf(Pointer));
    For var I := LIndex + Length(AItems) to FControlsCount - 1 do
      _TALDynamicControlProtectedAccess(FControls[I]).FIndex := I;
  end;
  //--
  for var i := 0 to Length(AItems) - 1 do begin
    var LItem := AItems[i];
    {$IF defined(debug)}
    if LItem.Owner <> nil then Raise Exception.Create('InsertItems integrity check failed: LItem.Owner is not nil');
    If LItem.FUpdating <> FUpdating then raise Exception.Create('InsertItems integrity check failed: LItem.FUpdating mismatch');
    {$ENDIF}
    FControls[LIndex + i] := LItem;
    LItem.FIndex := LIndex + i;
    LItem.FOwner := Self;
  end;
  //--
  FControlsCount := FControlsCount + Length(AItems);
  //--
  for var i := 0 to Length(AItems) - 1 do
    AItems[i].ParentChanged;
  //--
  Realign(LIndex);
end;

{******************************************************************}
function TALDynamicListBox.TView.TMainContent.GetTopBarSize: Single;
begin
  if (Owner <> nil) and
     (Owner.FTopBar <> nil) and
     (Owner.FTopBar.ShowWithMainContent) then begin
    if Owner.Orientation = Torientation.Horizontal then result := Owner.FTopBar.Width + Owner.FTopBar.margins.Right
    else result := Owner.FTopBar.Height + Owner.FTopBar.margins.bottom;
  end
  else
    Result := 0;
end;

{***********************************************************************************}
procedure TALDynamicListBox.TView.TMainContent.DoRealign(const AStartIndex: integer);
begin

  if (Owner = nil) or
     (IsDestroying) or
     (FDisableAlign) or
     (controlsCount = 0) then exit;

  var LFirstVisibleItemIndex := Owner.FirstVisibleItemIndex;
  var LFirstItemOffset: TALPointD;
  if LFirstVisibleItemIndex >= 0 then LFirstItemOffset := Owner.ViewportPosition - Owner.FItems^[LFirstVisibleItemIndex].Position
  else LFirstItemOffset := TALPointD.Zero;

  FDisableAlign := True;
  try

    // By default, items are top-aligned. You must provide an
    // OnRealign event to change this behavior.
    if assigned(FOnRealign) then FOnRealign(Self, AStartIndex)
    else begin
      if (Owner <> nil) and (Owner.Orientation = TOrientation.Horizontal) then begin
        var LCurrX: double;
        if AStartIndex <= 0 then LCurrX := GetTopBarSize + Padding.Left
        else LCurrX := Controls[AStartIndex - 1].Right + Controls[AStartIndex - 1].margins.Right;

        for var I := AStartIndex to ControlsCount - 1 do begin
          var LControl := Controls[i];
          LControl.SetBounds(
            LCurrX + LControl.margins.left, // X
            Padding.Top + LControl.margins.top, // Y
            LControl.Width, // AWidth
            Height - Padding.Top - LControl.margins.Top - LControl.margins.Bottom - Padding.bottom); // AHeight
          LCurrX := LControl.left + LControl.Width + LControl.margins.right;
        end;
      end
      else begin
        var LCurrY: double;
        if AStartIndex <= 0 then LCurrY := GetTopBarSize + Padding.Top
        else LCurrY := Controls[AStartIndex - 1].bottom + Controls[AStartIndex - 1].margins.bottom;

        for var I := AStartIndex to ControlsCount - 1 do begin
          var LControl := Controls[i];
          LControl.SetBounds(
            Padding.Left + LControl.margins.left, // X
            LCurrY + LControl.margins.top, // Y
            Width - Padding.Left - LControl.margins.left - LControl.margins.right - Padding.Right, // AWidth
            LControl.Height); // AHeight
          LCurrY := LControl.top + LControl.Height + LControl.margins.bottom;
        end;
      end;
    end;

    AdjustSize;

  finally
    FDisableAlign := False;
  end;

  if (not FIsAdjustingSize) then begin
    Owner.UpdateScrollEngineLimits;
    var LViewportPosition: TALPointD;
    if (LFirstVisibleItemIndex >= 0) and (not Owner.ViewportPosition.IsZero) then LViewportPosition := Owner.FItems^[LFirstVisibleItemIndex].Position + LFirstItemOffset
    else LViewportPosition := Owner.ViewportPosition;
    if ((Owner.ScrollEngine.TimerActive)) and (not LViewportPosition.EqualsTo(Owner.ViewportPosition,TEpsilon.Position)) then Owner.ScrollEngine.SetViewportPosition(LViewportPosition, False{EnforceLimits})
    else Owner.SetViewportPosition(LViewportPosition);
  end;

end;

{*******************************************************}
procedure TALDynamicListBox.TView.TMainContent.DoRealign;
begin
  DoRealign(0);
end;

{*********************************************************************************}
procedure TALDynamicListBox.TView.TMainContent.Realign(const AStartIndex: integer);
begin
  if IsDestroying then
    Exit;
  if FDisableAlign then
    Exit;
  if IsUpdating then
    Exit;
  DoRealign(AStartIndex);
end;

{********************************************************}
procedure TALDynamicListBox.TView.TMainContent.AdjustSize;
begin
  if (Owner <> nil) and
     (not IsDestroying) and
     (TNonReentrantHelper.EnterSection(FIsAdjustingSize)) then begin // Non-reantrant
    try
      var LLastActiveItem: TItem := Owner.FindLastActiveItem;
      if LLastActiveItem <> nil then begin
        if Owner.Orientation = TOrientation.Horizontal then begin
          var LWidth: Double := LLastActiveItem.Left + LLastActiveItem.Width + LLastActiveItem.Margins.Right;
          if (Owner.FLoadMoreIndicator <> nil) and (Owner.FPaginationToken <> '') and (Owner.FDownloadItemsErrorCode = 0) then LWidth := LWidth + Owner.FLoadMoreIndicator.width + Owner.FLoadMoreIndicator.Margins.Left + Owner.FLoadMoreIndicator.Margins.right
          else if (Owner.FLoadMoreRetryButton <> nil) and (Owner.FPaginationToken <> '') and (Owner.FDownloadItemsErrorCode <> 0) then LWidth := LWidth + Owner.FLoadMoreRetryButton.width + Owner.FLoadMoreRetryButton.Margins.Left + Owner.FLoadMoreRetryButton.Margins.right
          else LWidth := LWidth + Padding.Right;
          Width := LWidth;
        end
        else begin
          var LHeight: Double := LLastActiveItem.Top + LLastActiveItem.height + LLastActiveItem.Margins.bottom;
          if (Owner.FLoadMoreIndicator <> nil) and (Owner.FPaginationToken <> '') and (Owner.FDownloadItemsErrorCode = 0) then LHeight := LHeight + Owner.FLoadMoreIndicator.Height + Owner.FLoadMoreIndicator.Margins.Top + Owner.FLoadMoreIndicator.Margins.Bottom
          else if (Owner.FLoadMoreRetryButton <> nil) and (Owner.FPaginationToken <> '') and (Owner.FDownloadItemsErrorCode <> 0) then LHeight := LHeight + Owner.FLoadMoreRetryButton.Height + Owner.FLoadMoreRetryButton.Margins.Top + Owner.FLoadMoreRetryButton.Margins.Bottom
          else LHeight := LHeight + Padding.bottom;
          Height := LHeight;
        end;
      end;
    finally
      TNonReentrantHelper.LeaveSection(FIsAdjustingSize)
    end;

    if (not FDisableAlign) then begin
      Owner.UpdateScrollEngineLimits;
      Owner.SetViewportPosition(Owner.ViewportPosition);
    end;
  end;
end;

{*******************************************************}
procedure TALDynamicListBox.TView.TMainContent.DoResized;
begin
  inherited;
  // Call SetViewportPosition to reposition elements like
  // LoadMoreIndicator and LoadMoreRetryButton, which are
  // updated as part of the viewport handling logic.
  //
  // Note: If FDisableAlign = True, the DoRealign procedure will be
  // responsible for calling SetViewportPosition. This is important because
  // SetViewportPosition may call FItems^[i].Prepare immediately if the item
  // is visible on screen, bypassing background preparation and if the prepared
  // item's height differs from the default, DoRealign(i) may be triggered again.
  // However, when DisableRealign or FIsAdjustingSize is active, layout updates
  // are blocked, which can result in incorrect item placement.
  if (Owner <> nil) and
     (not FDisableAlign) and
     (not FIsAdjustingSize) then begin
    Owner.UpdateScrollEngineLimits;
    Owner.SetViewportPosition(Owner.ViewportPosition);
  end;
end;

{********************************************************************************}
function TALDynamicListBox.TView.TMainContent.GetFirstVisibleObjectIndex: Integer;
begin
  if Owner <> nil then Result := Owner.FFirstVisibleItemIndex
  else Result := Inherited;
end;

{*******************************************************************************}
function TALDynamicListBox.TView.TMainContent.GetLastVisibleObjectIndex: Integer;
begin
  if Owner <> nil then Result := Owner.FLastVisibleItemIndex
  else Result := Inherited;
end;

{***********************************************************************}
function TALDynamicListBox.TView.TMainContent.PaintChildrenOnly: boolean;
begin
  Result := True;
end;

{********************************************************}
function TALDynamicListBox.TView.TContent.GetOwner: TView;
begin
  Result := TView(inherited Owner);
end;

{**********************************************************************}
procedure TALDynamicListBox.TView.TContent.SetOwner(const Value: TView);
begin
  inherited Owner := Value;
end;

{**********************************************************************************}
constructor TALDynamicListBox.TView.TSurroundingContent.Create(const AOwner: TItem);
begin
  inherited;
  FShowWithMainContent := True;
  FShowWithLoadingContent := True;
  FShowWithErrorContent := True;
  FShowWithNoItemsContent := True;
end;

{*********************************************************************************}
constructor TALDynamicListBox.TView.TForegroundContent.Create(const AOwner: TItem);
begin
  inherited;
  FShowWithLoadingContent := False;
  FShowWithErrorContent := False;
end;

{**********************************************************************}
constructor TALDynamicListBox.TView.TTopBar.Create(const AOwner: TItem);
begin
  inherited;
  FhidesOnScroll := true;
  FShowWithLoadingContent := False;
  FShowWithErrorContent := False;
end;

{*******************************************************}
procedure TALDynamicListBox.TView.TTopBar.VisibleChanged;
begin
  inherited;
  if visible and (Owner <> nil) then begin
    if Owner.Orientation = TOrientation.Horizontal then left := 0
    else Top := 0;
  end;
end;

{***********************************************************}
function TALDynamicListBox.TView.TTopBar.IsInMotion: boolean;
begin
  result := False;
end;

{*************************************************************************}
constructor TALDynamicListBox.TView.TBottomBar.Create(const AOwner: TItem);
begin
  inherited;
  FhidesOnScroll := true;
  FShowWithLoadingContent := False;
  FShowWithErrorContent := False;
end;

{**********************************************************}
procedure TALDynamicListBox.TView.TBottomBar.VisibleChanged;
begin
  inherited;
  if visible and (Owner <> nil) then begin
    if Owner.Orientation = TOrientation.Horizontal then left := Owner.Width - width
    else Top := Owner.Height - height;
  end;
end;

{**************************************************************}
function TALDynamicListBox.TView.TBottomBar.IsInMotion: boolean;
begin
  result := False;
end;

{******************************************************************************************}
constructor TALDynamicListBox.TView.TBasePullToRefreshIndicator.Create(const AOwner: TItem);
begin
  inherited;
  AutoSize := TALAutoSizeMode.Both;
  FPullThreshold := 24;
  FPullProgress := 0;
  FCanTriggerRefresh := False;
  FIsRefreshing := False;
end;

{**************************************************************************************************}
procedure TALDynamicListBox.TView.TBasePullToRefreshIndicator.SetPullProgress(const AValue: Single);
begin
  FPullProgress := AValue;
end;

{***************************************************************************************************}
procedure TALDynamicListBox.TView.TBasePullToRefreshIndicator.SetIsRefreshing(const AValue: Boolean);
begin
  FIsRefreshing := AValue;
end;

{**************************************************************************************}
constructor TALDynamicListBox.TView.TPullToRefreshIndicator.Create(const AOwner: TItem);
begin
  inherited;
  XRadius := -50;
  YRadius := -50;
  FPullingPhaseAniIndicator := TALDynamicAniIndicator.Create(self);
  FPullingPhaseAniIndicator.Align := TALAlignLayout.Client;
  FPullingPhaseAniIndicator.Animation.Enabled := False;
  FRefreshingPhaseAniIndicator := TALDynamicAniIndicator.Create(self);
  FRefreshingPhaseAniIndicator.Align := TALAlignLayout.Client;
  FRefreshingPhaseAniIndicator.Visible := False;
  FRefreshingPhaseAniIndicator.Animation.InterpolationType := TALInterpolationType.Linear;
  FRefreshingPhaseAniIndicator.Animation.Enabled := True;
end;

{******************************************************************************}
function TALDynamicListBox.TView.TPullToRefreshIndicator.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(40, 40);
end;

{***********************************************************************************************}
procedure TALDynamicListBox.TView.TPullToRefreshIndicator.SetIsRefreshing(const AValue: Boolean);
begin
  inherited;
  FPullingPhaseAniIndicator.Visible := not IsRefreshing;
  FRefreshingPhaseAniIndicator.Visible := IsRefreshing;
end;

{*********************************************************************************************}
procedure TALDynamicListBox.TView.TPullToRefreshIndicator.SetPullProgress(const Value: Single);
begin
  inherited;
  _TALFloatAnimationProtectedAccess(FPullingPhaseAniIndicator.Animation).FCurrentValue := PullProgress / 5;
end;

{*********************************************************************************}
constructor TALDynamicListBox.TView.TLoadMoreIndicator.Create(const AOwner: TItem);
begin
  inherited;
  AutoSize := TALAutoSizeMode.Both;
end;

{***********************************************************************************}
constructor TALDynamicListBox.TView.TLoadMoreRetryButton.Create(const AOwner: TItem);
begin
  inherited;
  AutoSize := TALAutoSizeMode.Both;
end;

{************************************************************************************}
constructor TALDynamicListBox.TView.TDownloadItemsContext.Create(const AOwner: TView);
begin
  inherited Create(AOwner);
  ItemIdType := AOwner.FItemIdType;
  ItemIdNodeName := AOwner.ItemIdNodeName;
  MaxItems := AOwner.MaxItems;
  PaginationToken := AOwner.FPaginationToken;
  CustomParams := TALStringListW.Create;
  FOnDownloadItems := AOwner.OnDownloadItems;
  FOnCreateItem := AOwner.OnCreateItem;
  FOnCreateItemMainContent := AOwner.OnCreateItemMainContent;
  FOnCreateItemLoadingContent := AOwner.OnCreateItemLoadingContent;
  FOnCreateItemErrorContent := AOwner.OnCreateItemErrorContent;
  FOnDownloadItemData := AOwner.OnDownloadItemData;
end;

{***************************************************************}
destructor TALDynamicListBox.TView.TDownloadItemsContext.Destroy;
begin
  ALFreeAndNil(CustomParams);
  inherited Destroy;
end;

{*********************************************************************}
function TALDynamicListBox.TView.TDownloadItemsContext.GetOwner: TView;
begin
  Result := TView(FOwner);
end;

{******************************************************************************************************************}
constructor TALDynamicListBox.TView.TContentBuilderContext.Create(const AOwner: TView; const AContentType: Integer);
begin
  inherited create(AOwner, AContentType);
  //-
  OwnerIsMainView := AOwner.IsMainView;
  if (OwnerIsMainView) and (AContentType = MainContentType) and (AOwner.Host <> nil) then Padding := AOwner.Host.Padding.Rect
  else Padding := TRectF.Empty;
  //-
  if (AContentType = MainContentType) and
     (not assigned(FOnCreateMainContent)) then
    FOnCreateMainContent := AOwner.CreateDefaultMainContent;
  //-
  NewBackgroundContent := nil;
  NewForegroundContent := nil;
  NewTopBar := nil;
  NewBottomBar := nil;
  NewPullToRefreshIndicator := nil;
  NewLoadMoreIndicator := nil;
  NewLoadMoreRetryButton := nil;
  //-
  if AOwner.FNoItemsContent = nil then OnCreateNoItemsContent := AOwner.OnCreateNoItemsContent
  else OnCreateNoItemsContent := nil;
  //-
  if AOwner.FBackgroundContent = nil then OnCreateBackgroundContent := AOwner.OnCreateBackgroundContent
  else OnCreateBackgroundContent := nil;
  //-
  if AOwner.FForegroundContent = nil then OnCreateForegroundContent := AOwner.OnCreateForegroundContent
  else OnCreateForegroundContent := nil;
  //-
  if AOwner.FTopBar = nil then OnCreateTopBar := AOwner.OnCreateTopBar
  else OnCreateTopBar := nil;
  //-
  if AOwner.FBottomBar = nil then OnCreateBottomBar := AOwner.OnCreateBottomBar
  else OnCreateBottomBar := nil;
  //-
  if AOwner.FPullToRefreshIndicator = nil then OnCreatePullToRefreshIndicator := AOwner.OnCreatePullToRefreshIndicator
  else OnCreatePullToRefreshIndicator := nil;
  //-
  if AOwner.FLoadMoreIndicator = nil then OnCreateLoadMoreIndicator := AOwner.OnCreateLoadMoreIndicator
  else OnCreateLoadMoreIndicator := nil;
  //-
  if AOwner.FLoadMoreRetryButton = nil then OnCreateLoadMoreRetryButton := AOwner.OnCreateLoadMoreRetryButton
  else OnCreateLoadMoreRetryButton := nil;
end;

{****************************************************************}
destructor TALDynamicListBox.TView.TContentBuilderContext.Destroy;
begin
  ALFreeAndNil(NewBackgroundContent);
  ALFreeAndNil(NewForegroundContent);
  ALFreeAndNil(NewTopBar);
  ALFreeAndNil(NewBottomBar);
  ALFreeAndNil(NewPullToRefreshIndicator);
  ALFreeAndNil(NewLoadMoreIndicator);
  ALFreeAndNil(NewLoadMoreRetryButton);
  inherited;
end;

{**************}
// [MultiThread]
function TALDynamicListBox.TView.TContentBuilderContext.GetContentAlign(const AContentType: Integer): TALAlignLayout;
begin
  case AContentType of
    BackgroundContentType,
    ForegroundContentType,
    PullToRefreshIndicatorContentType: begin
      Result := TALAlignLayout.None;
    end;
    TopBarContentType,
    BottomBarContentType,
    LoadMoreIndicatorContentType,
    LoadMoreRetryButtonContentType: begin
      If (FParentViewOrientation = TOrientation.Horizontal) then Result := TALAlignLayout.Vertical
      else Result := TALAlignLayout.horizontal;
    end;
    //--
    MainContentType: begin
      If (FParentViewOrientation = TOrientation.Horizontal) then Result := TALAlignLayout.Vertical
      else Result := TALAlignLayout.horizontal;
    end
    //--
    else begin
      //LoadingContentType
      //ErrorContentType
      //NoItemsContentType
      if OwnerIsMainView then Result := TALAlignLayout.Client
      else Result := inherited;
    end;
  end;
end;

{**************}
// [MultiThread]
function TALDynamicListBox.TView.TContentBuilderContext.GetCreateContentMethod(const AContentType: Integer): TMethod;
begin
  case AContentType of
    NoItemsContentType: Result := TMethod(OnCreateNoItemsContent);
    BackgroundContentType: Result := TMethod(OnCreateBackgroundContent);
    ForegroundContentType: Result := TMethod(OnCreateForegroundContent);
    TopBarContentType: Result := TMethod(OnCreateTopBar);
    BottomBarContentType: Result := TMethod(OnCreateBottomBar);
    PullToRefreshIndicatorContentType: Result := TMethod(OnCreatePullToRefreshIndicator);
    LoadMoreIndicatorContentType: Result := TMethod(OnCreateLoadMoreIndicator);
    LoadMoreRetryButtonContentType: Result := TMethod(OnCreateLoadMoreRetryButton);
    else Result := inherited;
  end;
end;

{****************************************************************}
constructor TALDynamicListBox.TView.Create(const AOwner: TObject);
begin
  inherited create(AOwner);
  AutoCapture := True;
  AutoSize := TALAutoSizeMode.None;
  {$IFDEF DEBUG}
  fDebugFpsStarted := False;
  fDebugFpsCount := 0;
  //fDebugFpsStopWatch
  //fDebugFpsRenderTimeStopWatch
  fDebugAverageFpsCount := 0;
  fDebugAverageFps := 0;
  {$ENDIF}
  FOrientation := TOrientation.Vertical;
  FItemIdType := TItemIdType.Unknown;
  FScrollDirection := TScrollDirection.FromBeginToEnd;
  fScrollCapturedByMe := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  FHandleMouseEvents := False;
  FIsSettingViewportPosition := False;
  FRefreshTransitionKind := TRefreshTransitionKind.CrossFade;
  fLastViewportPosition := TALPointD.Create(0,0);
  FMouseDownPos := TPointF.Zero;
  FRefreshTransitionAnimation := nil;
  FRefreshingTimer := nil;
  FRefreshingView := nil;
  FNoItemsContent := nil;
  FBackgroundContent := nil;
  FForegroundContent := nil;
  FTopBar := nil;
  FBottomBar := Nil;
  FPullToRefreshIndicator := Nil;
  FLoadMoreIndicator := Nil;
  FLoadMoreRetryButton := Nil;
  FItems := nil;
  FItemIdNodeName := 'id';
  FUniqueInt64ItemIds := TDictionary<Int64, boolean>.create;
  FUniqueTextItemIds := TDictionary<String, boolean>.create;
  FMaxItems := MaxInt;
  FPreloadItemCount := DefaultPreloadItemCount;
  //--
  FScrollEngine := TALScrollEngine.Create;
  FScrollEngine.TouchTracking := [TTVertical];
  FScrollEngine.OnChanged := ScrollEngineChanged;
  FScrollEngine.OnStart := ScrollEngineStart;
  FScrollEngine.OnStop := ScrollEngineStop;
  //--
  FFirstVisibleItemIndex := 0; // = Low(FItems^) when FItems is empty
  FLastVisibleItemIndex := -1; // = High(FItems^) when FItems is empty
  FFirstPreloadedItemIndex := 0; // = Low(FItems^) when FItems is empty
  FLastPreloadedItemIndex := -1; // = High(FItems^) when FItems is empty
  FPaginationToken := #0;
  FTriggerDownloadItemsAtIndex := -maxint;
  FDownloadItemsErrorCode := 0;
  FDownloadItemsContext := nil;
  FOnDownloadItems := nil;
  FOnCreateItem := nil;
  FOnCreateItemMainContent := nil;
  FOnCreateItemLoadingContent := nil;
  FOnCreateItemErrorContent := nil;
  FOnDownloadItemData := nil;
  FOnCreateNoItemsContent := nil;
  FOnCreateBackgroundContent := nil;
  FOnCreateForegroundContent := nil;
  FOnCreateTopBar := nil;
  FOnCreateBottomBar := nil;
  FOnCreatePullToRefreshIndicator := nil;
  FOnCreateLoadMoreIndicator := nil;
  FOnCreateLoadMoreRetryButton := nil;
  FOnShowNoItemsContent := nil;
  FOnRealignItems := nil;
  FOnViewportPositionChange := nil;
end;

{*****************************************}
destructor TALDynamicListBox.TView.Destroy;
begin
  ALFreeAndNil(FRefreshTransitionAnimation);
  ALFreeAndNil(FRefreshingTimer);
  ALFreeAndNil(FRefreshingView);
  ALFreeAndNil(FUniqueInt64ItemIds);
  ALFreeAndNil(FUniqueTextItemIds);
  ALFreeAndNil(FScrollEngine);
  inherited;
end;

{**************************************************}
procedure TALDynamicListBox.TView.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  if FRefreshTransitionAnimation <> nil then FRefreshTransitionAnimation.Enabled := False;
  if FRefreshingTimer <> nil then FRefreshingTimer.Enabled := False;
  CancelDownloadItems;
  TMessageManager.DefaultManager.Unsubscribe(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  if FscrollEngine.TimerActive then begin
    FscrollEngine.Stop(true{AAbruptly});
    ScrollEngineStop(nil);
  end;
  inherited;
end;

{*****************************************************************************************}
function TALDynamicListBox.TView.IsReadyToDisplay(const AStrict: Boolean = False): Boolean;
begin
  Result := ((FMainContent <> nil) or (FErrorContent <> nil));
  If not result then exit;
  For var I := FirstVisibleItemIndex to LastVisibleItemIndex do begin
    if FItems^[i].FMainContent = nil then Exit(false)
    else begin
      Result := FItems^[i].IsReadyToDisplay(AStrict);
      if not result then exit;
    end;
  end;
end;

{***********************************************************************************}
procedure TALDynamicListBox.TView.DoRemoveControl(const AControl: TALDynamicControl);
begin
  if Acontrol = FNoItemsContent then FNoItemsContent := nil
  else if Acontrol = FBackgroundContent then FBackgroundContent := nil
  else if Acontrol = FForegroundContent then FForegroundContent := nil
  else if Acontrol = FTopBar then FTopBar := nil
  else if Acontrol = FBottomBar then FBottomBar := nil
  else if Acontrol = FPullToRefreshIndicator then FPullToRefreshIndicator := nil
  else if Acontrol = FLoadMoreIndicator then FLoadMoreIndicator := nil
  else if Acontrol = FLoadMoreRetryButton then FLoadMoreRetryButton := nil;
  inherited;
end;

{****************************************************************************}
procedure TALDynamicListBox.TView.SetHost(Const Value: TALDynamicControlHost);
begin
  if FHost <> value then begin
    if (FscrollEngine <> nil) and (FscrollEngine.TimerActive) then begin
      FscrollEngine.Stop(true{AAbruptly});
      ScrollEngineStop(nil);
    end;
    inherited;
  end;
end;

{****************************************}
procedure TALDynamicListBox.TView.Prepare;
begin
  inherited;
  // SetViewportPosition will prepare all items
  // and also invoke DownloadItems.
  SetViewportPosition(ViewportPosition);
  // Prepare all Items
  if FItems <> nil then
    for var I := Max(Low(FItems^){0 if empty}, FFirstPreloadedItemIndex) to Min(High(FItems^){-1 if empty}, FLastPreloadedItemIndex) do
      FItems^[i].Prepare;
end;

{******************************************}
procedure TALDynamicListBox.TView.Unprepare;
begin
  CancelDownloadItems;
  inherited;
  if Fitems <> nil then
    for Var I := FFirstPreloadedItemIndex to FLastPreloadedItemIndex do
      FItems^[i].Unprepare;
end;

{****************************************************************************}
function TALDynamicListBox.TView.GetItemByIndex(Const AIndex: Integer): TItem;
begin
  if Fitems <> nil then
    result := Fitems^[AIndex]
  else
    raise Exception.Create('Index is out of bounds');
end;

{******************************************************}
function TALDynamicListBox.TView.GetItemsCount: integer;
begin
  if FMainContent <> nil then
    Result := FMainContent.ControlsCount
  else
    Result := 0;
end;

{*******************************************************************************}
function TALDynamicListBox.TView.GetOnCreateMainContent: TCreateMainContentEvent;
begin
  Result := TCreateMainContentEvent(inherited OnCreateMainContent);
end;

{**********************************************************************************************}
procedure TALDynamicListBox.TView.SetOnCreateMainContent(const AValue: TCreateMainContentEvent);
begin
  inherited OnCreateMainContent := TItem.TCreateMainContentEvent(AValue);
end;

{***************************************************************************}
procedure TALDynamicListBox.TView.SetOrientation(const AValue: TOrientation);
begin
  if FOrientation <> AValue then begin
    FOrientation := AValue;
    if FOrientation = TOrientation.Horizontal then FScrollEngine.TouchTracking := [tthorizontal]
    else FScrollEngine.TouchTracking := [ttVertical];
  end;
end;

{*******************************************************************************************************}
procedure TALDynamicListBox.TView.ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  if (Sender = self) then exit;
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.ScrollCapturedByOtherHandler',
  //  'Captured: ' + ALBoolToStrW(TALScrollCapturedMessage(M).Captured)+ ' | ' +
  //  'ScrollEngine.down: ' + ALBoolToStrW(FScrollEngine.down));
  {$ENDIF}
  if TALScrollCapturedMessage(M).Captured then begin
    {$IFDEF DEBUG}
    if fScrollCapturedByMe then
      raise Exception.Create('Error 8EA8C349-8441-4D2F-BC5A-872772A40513');
    {$ENDIF}
    if FScrollEngine.down then begin
      FScrollEngine.Down := false;
      FHandleMouseEvents := False;
    end;
  end;
end;

{**********************************************************************************************************}
procedure TALDynamicListBox.TView.InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.MouseDown',
  //  'Position: ' + ALFormatFloatW('0.##', x) + ',' + ALFormatFloatW('0.##', y));
  {$ENDIF}
  if (Button = TMouseButton.mbLeft) then begin
    FHandleMouseEvents := true;
    if (FPullToRefreshIndicator <> nil) then begin
      FPullToRefreshIndicator.CanTriggerRefresh := IsMainView and
                                                   ScrollEngine.IsVelocityLow and
                                                   ScrollEngine.ViewportPosition.IsZero;
    end;
    fMouseDownPos := TpointF.Create(X,Y);
    {$IF (not defined(ALUIAutomationEnabled)) and (defined(ANDROID) or defined(IOS))}
    if form <> nil then
      ScrollEngine.MouseDown(form.Handle);
    {$ELSE}
    ScrollEngine.MouseDown(X, Y);
    {$ENDIF}
  end;
end;

{************************************************************************************}
procedure TALDynamicListBox.TView.InternalMouseMove(Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.InternalMouseMove',
  //  'Position: ' + ALFormatFloatW('0.##', x) + ',' + ALFormatFloatW('0.##', y));
  {$ENDIF}
  if FHandleMouseEvents then begin
    if (not fScrollCapturedByMe) and
       (FScrollEngine.TouchEnabled) and
       (((ttHorizontal in FScrollEngine.TouchTracking) and
         (abs(fMouseDownPos.x - x) > abs(fMouseDownPos.y - y)) and
         (abs(fMouseDownPos.x - x) > TALScrollEngine.DefaultTouchSlop)) or
        ((ttVertical in FScrollEngine.TouchTracking) and
         (abs(fMouseDownPos.y - y) > abs(fMouseDownPos.x - x)) and
         (abs(fMouseDownPos.y - y) > TALScrollEngine.DefaultTouchSlop))) then begin
      {$IFDEF DEBUG}
      //ALLog(
      //  ClassName + '.InternalMouseMove',
      //  'ScrollCapturedByMe');
      {$ENDIF}
      fScrollCapturedByMe := True;
      TMessageManager.DefaultManager.SendMessage(self, TALScrollCapturedMessage.Create(True));
    end;
    {$IF (not defined(ALUIAutomationEnabled)) and (defined(ANDROID) or defined(IOS))}
    if form <> nil then
      ScrollEngine.MouseMove(form.Handle);
    {$ELSE}
    ScrollEngine.MouseMove(X, Y);
    {$ENDIF}
  end;
end;

{********************************************************************************************************}
procedure TALDynamicListBox.TView.InternalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.InternalMouseUp',
  //  'Position: ' + ALFormatFloatW('0.##', x) + ',' + ALFormatFloatW('0.##', y));
  {$ENDIF}
  if FHandleMouseEvents and (Button = TMouseButton.mbLeft) then begin
    {$IF (not defined(ALUIAutomationEnabled)) and (defined(ANDROID) or defined(IOS))}
    if form <> nil then
      ScrollEngine.MouseUp(form.Handle);
    {$ELSE}
    ScrollEngine.MouseUp(X, Y);
    {$ENDIF}
    FScrollCapturedByMe := False;
    FHandleMouseEvents := False;
    if (FPullToRefreshIndicator <> nil) then begin
      if (FPullToRefreshIndicator.CanTriggerRefresh) and
         (FPullToRefreshIndicator.PullProgress > 1) then begin
        FPullToRefreshIndicator.IsRefreshing := True;
        Refresh;
      end;
    end;
  end;
end;

{***************************************************}
procedure TALDynamicListBox.TView.InternalMouseLeave;
begin
  {$IFDEF DEBUG}
  //ALLog(ClassName + '.InternalMouseLeave');
  {$ENDIF}
  if FHandleMouseEvents then begin
    ScrollEngine.MouseLeave;
    FScrollCapturedByMe := False;
    FHandleMouseEvents := False;
  end;
end;

{**************************************************************************************************}
procedure TALDynamicListBox.TView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  InternalMouseDown(Button, Shift, X, Y);
end;

{****************************************************************************}
procedure TALDynamicListBox.TView.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  InternalMouseMove(Shift, X, Y);
  inherited;
end;

{************************************************************************************************}
procedure TALDynamicListBox.TView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  InternalMouseUp(Button, Shift, X, Y);
end;

{*******************************************}
procedure TALDynamicListBox.TView.MouseLeave;
begin
  inherited;
  InternalMouseLeave;
end;

{**********************************************************************************************************}
procedure TALDynamicListBox.TView.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  if (not Handled) and (FMainContent <> nil) then begin
    if ssHorizontal in Shift then begin
      if FMainContent.Width > Width then begin
        var Offset: Single := (Width / 5) * -1 * (WheelDelta / 120);
        ScrollEngine.MouseWheel(Offset, 0);
        Handled := True;
      end;
    end
    else if FMainContent.Height > Height then begin
      var Offset: Single := (Height / 5) * -1 * (WheelDelta / 120);
      ScrollEngine.MouseWheel(0, Offset);
      Handled := True;
    end
    else if FMainContent.Width > Width then begin
      var Offset: Single := (Width / 5) * -1 * (WheelDelta / 120);
      ScrollEngine.MouseWheel(Offset, 0);
      Handled := True;
    end;
  end;
end;

{********************************************************************************************************************************************}
procedure TALDynamicListBox.TView.ChildrenMouseDown(const AObject: TALDynamicControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not aObject.AutoCapture then _TALDynamicControlProtectedAccess(AObject).capture;
  var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  InternalMouseDown(Button, Shift, P.X, P.Y);
  inherited;
end;

{**********************************************************************************************************************}
procedure TALDynamicListBox.TView.ChildrenMouseMove(const AObject: TALDynamicControl; Shift: TShiftState; X, Y: Single);
begin
  var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  InternalMouseMove(Shift, P.X, P.Y);
  inherited;
end;

{******************************************************************************************************************************************}
procedure TALDynamicListBox.TView.ChildrenMouseUp(const AObject: TALDynamicControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not aObject.AutoCapture then _TALDynamicControlProtectedAccess(aObject).releasecapture;
  var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  InternalMouseUp(Button, Shift, P.X, P.Y);
  inherited;
end;

{*************************************************************************************}
procedure TALDynamicListBox.TView.ChildrenMouseLeave(const AObject: TALDynamicControl);
begin
  InternalMouseLeave;
  inherited;
end;

{****************************************************************}
function TALDynamicListBox.TView.GetScrollEngine: TALScrollEngine;
begin
  Result := FScrollEngine;
end;

{*********************************************************************}
procedure TALDynamicListBox.TView.ScrollEngineChanged(Sender: TObject);
begin
  {$IFDEF DEBUG}
  //ALLog(Classname+'.ScrollEngineChanged');
  {$ENDIF}
  SetViewportPosition(ScrollEngine.ViewportPosition);
end;

{*******************************************************************}
procedure TALDynamicListBox.TView.ScrollEngineStart(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ALLog(Classname+'.ScrollEngineStart');
  {$ENDIF}
  If Host <> nil then
    inc(Host.FActiveScrollEnginesCount);
end;

{******************************************************************}
procedure TALDynamicListBox.TView.ScrollEngineStop(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ALLog(Classname+'.ScrollEngineStop');
  {$ENDIF}
  If Host <> nil then begin
    dec(Host.FActiveScrollEnginesCount);
    {$IFDEF DEBUG}
    if Host.FActiveScrollEnginesCount < 0 then
      Raise Exception.Create('Error E277A8CF-210A-4C0F-8427-C0FF34C470FE');
    {$ENDIF}
  end;

  if (FRefreshTransitionKind = TRefreshTransitionKind.Scroll) and
     (FRefreshingTimer <> nil) and
     (not FRefreshingTimer.Enabled) and
     (FRefreshingView <> nil) then begin
    Host.HitTest := True;
    var LRefreshingView := FRefreshingView;
    FRefreshingView := nil;
    Host.MainView := LRefreshingView;
  end;
end;

{*********************************************************}
procedure TALDynamicListBox.TView.UpdateScrollEngineLimits;
begin
  ScrollEngine.MinScrollLimit := TALPointD.Create(0,0);
  if FMainContent = nil then ScrollEngine.MaxScrollLimit := TALPointD.Create(0,0)
  else if Orientation = TOrientation.Horizontal then ScrollEngine.MaxScrollLimit := TALPointD.Create(max(0,FMainContent.Width - Width), 0)
  else ScrollEngine.MaxScrollLimit := TALPointD.Create(0, max(0,FMainContent.Height - height));
end;

{******************************************************************}
function TALDynamicListBox.TView.FindFirstVisibleItemIndex: integer;
begin

  //
  // -4 -     -
  // -3 -     -
  // -2 -     -
  // -1 -     -                     <= this pixel is NOT drawn
  //  0 +++++++ (FViewportPosition) <= this pixel is drawn
  //  1 +++++++                     <= this pixel is drawn
  //  2 +++++++                     <= this pixel is drawn
  //  3 -     - (Owner.Height)      <= this pixel is NOT drawn
  //  4 -     -
  //

  //----- 0 if FItems is empty
  Result := Max(
              Low(FItems^){0 if empty},
              Min(
                High(FItems^){-1 if empty},
                FFirstVisibleItemIndex));

  //
  //                         x   x   x
  //                           x   x
  // FirstVisibleItemIndex > x   x   x
  //                           x   x
  //                         x   x   x < LastVisibleItemIndex
  //                           x   x
  //                         x   x   x
  //
  If Orientation = TOrientation.horizontal then begin
    var LPos: Double := ViewportPosition.X;
    var LTMPIdx := Result;
    while (LTMPIdx >= Low(FItems^)) and
          (LTMPIdx <= High(FItems^)) do begin
      if FItems^[LTMPIdx].Visible then begin
        if compareValue(FItems^[LTMPIdx].right, LPos, TEpsilon.Position) >= 0 then Result := LTMPIdx
        else break;
      end;
      dec(LTMPIdx);
    end;
    //--
    while (Result < High(FItems^)) and
          ((not FItems^[Result].Visible) or
           (compareValue(FItems^[Result].right, LPos, TEpsilon.Position) < 0)) do
      inc(Result);
  end
  else begin
    var LPos: Double := ViewportPosition.Y;
    var LTMPIdx := Result;
    while (LTMPIdx >= Low(FItems^)) and
          (LTMPIdx <= High(FItems^)) do begin
      if FItems^[LTMPIdx].Visible then begin
        if compareValue(FItems^[LTMPIdx].bottom, LPos, TEpsilon.Position) >= 0 then Result := LTMPIdx
        else break;
      end;
      dec(LTMPIdx);
    end;
    //--
    while (Result < High(FItems^)) and
          ((not FItems^[Result].Visible) or
           (compareValue(FItems^[Result].bottom, LPos, TEpsilon.Position) < 0)) do
      inc(Result);
  end;

  // Consistency check only in debug
  {$IF defined(DEBUG)}
  if (Result < Low(FItems^)) then
    raise Exception.Create('Error FBAEF066-CFC7-4B63-96BA-15A1F0C8D827');
  {$ENDIF}

end;

{*****************************************************************}
function TALDynamicListBox.TView.FindLastVisibleItemIndex: integer;
begin

  //
  // -4 -     -
  // -3 -     -
  // -2 -     -
  // -1 -     -                     <= this pixel is NOT drawn
  //  0 +++++++ (FViewportPosition) <= this pixel is drawn
  //  1 +++++++                     <= this pixel is drawn
  //  2 +++++++                     <= this pixel is drawn
  //  3 -     - (Owner.Height)      <= this pixel is NOT drawn
  //  4 -     -
  //

  //----- -1 if FItems is empty
  Result := Min(
              High(FItems^){-1 if empty},
              Max(
                Low(FItems^){0 if empty},
                FLastVisibleItemIndex));

  //
  //                       x   x   x
  //                         x   x
  // FirstVisibleItemIdx > x   x   x
  //                         x   x
  //                       x   x   x < LastVisibleItemIdx
  //                         x   x     ------------------
  //                       x   x   x
  //
  If Orientation = TOrientation.horizontal then begin
    var LPos: Double := ViewportPosition.X + Width;
    var LTMPIdx := Result;
    while (LTMPIdx >= Low(FItems^)) and
          (LTMPIdx <= High(FItems^)) do begin
      if FItems^[LTMPIdx].Visible then begin
        if compareValue(FItems^[LTMPIdx].left, LPos, TEpsilon.Position) < 0 then Result := LTMPIdx
        else break;
      end;
      inc(LTMPIdx);
    end;
    //--
    while (Result > Low(FItems^)) and
          ((not FItems^[Result].Visible) or
           (compareValue(FItems^[Result].left, LPos, TEpsilon.Position) >= 0)) do
      dec(Result);
  end
  else begin
    var LPos: Double := ViewportPosition.Y + Height;
    var LTMPIdx := Result;
    while (LTMPIdx >= Low(FItems^)) and
          (LTMPIdx <= High(FItems^)) do begin
      if FItems^[LTMPIdx].Visible then begin
        if compareValue(FItems^[LTMPIdx].top,  LPos, TEpsilon.Position) < 0 then Result := LTMPIdx
        else break;
      end;
      inc(LTMPIdx);
    end;
    //--
    while (Result > Low(FItems^)) and
          ((not FItems^[Result].Visible) or
           (compareValue(FItems^[Result].top,  LPos, TEpsilon.Position) >= 0)) do
      dec(Result);
  end;

  // Consistency check only in debug
  {$IF defined(DEBUG)}
  if (Result > High(FItems^)) then
    raise Exception.Create('Error 399D434C-C527-476A-B49D-33CFDAB30D86');
  {$ENDIF}

end;

{*********************************************************}
function TALDynamicListBox.TView.FindLastActiveItem: TItem;
begin
  Result := nil;
  if (Fitems <> nil) then
    For var I := high(Fitems^) downto low(Fitems^) do
      if Fitems^[i].Visible then begin
        Result := Fitems^[i];
        break;
      end;
end;

{**********************************************************************}
procedure TALDynamicListBox.TView.PaintInternal(const ACanvas: TCanvas);
begin
  {$IFDEF DEBUG}
  LogFPS;
  {$ENDIF}
  inherited;
end;

{************}
{$IFDEF DEBUG}
procedure TALDynamicListBox.TView.LogFPS;
begin

  if not ScrollEngine.TouchEnabled then exit;

  fDebugFpsRenderTimeStopWatch.stop;
  if (fDebugFpsStarted) and
     (not ScrollEngine.down) and
     (abs(ScrollEngine.CurrentVelocity.y) > 100) and
     (fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds > 1200{1000*1.2} / DisplayDefaultRefreshRate) then begin
    ALLog(
      classname + '.fps',
      'Drop frame detected | '  +
      ALFormatFloatW('0.00', fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds) + ' | '+
      'Velocity: ' + ALFormatFloatW('0', ScrollEngine.CurrentVelocity.y),
      TalLogType.warn);
  end;

  if (abs(ScrollEngine.CurrentVelocity.y) > 100) and (not ScrollEngine.down) then begin
    If not fDebugFpsStarted then begin
      fDebugFpsStarted := true;
      fDebugFpsCount := 0;
      fDebugFpsStopWatch := TstopWatch.StartNew;
    end
    else begin
      inc(fDebugFpsCount);
    end;
  end
  else if fDebugFpsStarted then begin
    fDebugFpsStopWatch.stop;
    inc(fDebugFpsCount);
    if fDebugFpsStopWatch.Elapsed.totalMilliseconds > 0 then begin
      fDebugAverageFps := ((fDebugAverageFps * fDebugAverageFpsCount) + ((fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000)) / (fDebugAverageFpsCount + 1);
      inc(fDebugAverageFpsCount);
      ALLog(
        classname + '.fps',
        ALFormatFloatW('0.##', (fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000) + ' fps' + ' | ' +
        'Average: ' + ALFormatFloatW('0.##', fDebugAverageFps) + ' fps' + ' | ' +
        'System default: ' + ALFormatFloatW('0.##', DisplayDefaultRefreshRate) + ' fps',
        TalLogType.verbose);
    end;
    fDebugFpsStarted := False;
  end;
  fDebugFpsRenderTimeStopWatch := TstopWatch.StartNew;

end;
{$ENDIF}

{**************************************************************}
function TALDynamicListBox.TView.GetViewportPosition: TALPointD;
begin
  If FMainContent = nil then Result := TALPointD.create(0,0)
  else Result := TALPointD.create(-FMainContent.left, -FMainContent.top);
end;

{*****************************************************************************}
procedure TALDynamicListBox.TView.SetViewportPosition(const AValue: TALPointD);
begin
  {$IF defined(debug)}
  if TThread.Current.ThreadID <> MainThreadID then raise exception.Create('Error 0EF7A7EC-1662-48EE-8C75-41BCDB1D96D7');
  {$ENDIF}
  if (TNonReentrantHelper.EnterSection(FIsSettingViewportPosition)) then begin

    Try

      var LPrevDisableAlign := FDisableAlign;
      FDisableAlign := True;
      Try

        {$REGION 'Update MainContent position'}
        if FItems = nil then exit;
        var LOldViewportPosition := TALPointD.Create(-FMainContent.Left, -FMainContent.Top);
        FMainContent.SetPosition(-AValue);
        {$ENDREGION}

        {$REGION 'Update FScrollDirection'}
        If Orientation = TOrientation.Horizontal then begin
          var LCompareValue := CompareValue(LOldViewportPosition.X, AValue.X, TEpsilon.Position);
          if LCompareValue < 0 then FScrollDirection := TScrollDirection.FromBeginToEnd
          else if LCompareValue > 0 then FScrollDirection := TScrollDirection.FromEndToBegin;
        end
        else begin
          var LCompareValue := CompareValue(LOldViewportPosition.Y, AValue.Y, TEpsilon.Position);
          if LCompareValue < 0 then FScrollDirection := TScrollDirection.FromBeginToEnd
          else if LCompareValue > 0 then FScrollDirection := TScrollDirection.FromEndToBegin;
        end;
        {$ENDREGION}

        {$REGION 'calculate FFirstVisibleItemIndex/FLastVisibleItemIndex'}
        FFirstVisibleItemIndex := FindFirstVisibleItemIndex;
        FLastVisibleItemIndex := FindLastVisibleItemIndex;
        {$ENDREGION}

        {$REGION 'calculate FFirstPreloadedItemIndex/FLastPreloadedItemIndex'}
        var LOldFirstPreloadedItemIndex := FFirstPreloadedItemIndex;
        var LOldLastPreloadedItemIndex := FLastPreloadedItemIndex;
        FFirstPreloadedItemIndex := Max(Low(FItems^){0 if empty}, FFirstVisibleItemIndex - PreloadItemCount);
        FLastPreloadedItemIndex := Min(High(FItems^){-1 if empty}, FFirstVisibleItemIndex + PreloadItemCount);

        // Consistency check only in debug
        {$IF defined(DEBUG)}
        if (LOldFirstPreloadedItemIndex < Low(FItems^)) or
           (LOldLastPreloadedItemIndex > High(FItems^)) or
           (FFirstPreloadedItemIndex < Low(FItems^)) or
           (FLastPreloadedItemIndex > High(FItems^)) then
          raise Exception.Create('Error FD682464-DBB8-4D50-B31F-F8AEFCEBBA15');
        {$ENDIF}
        {$ENDREGION}

        {$REGION 'offset preload'}

        //
        // (A)   LOldLastPreloadedItemIndex
        // (A)   | LOldFirstPreloadedItemIndex
        //       | |
        // (B)   | |               | LOldFirstPreloadedItemIndex
        // (B)   | |               |         | LOldLastPreloadedItemIndex
        //       | |               |---------|
        //      -1 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
        // (1)                   7 |---------| 14
        // (2)                        10 |-----------| 18
        // (3)                     8 |-----| 13
        // (4)                 6 |---| 10
        // (5)           3 |-------| 9
        // (6)                            12 |---| 16
        // (7)  -1 |-| 2
        // (8)                                    16 |-----| 21
        // (9)  -1 | 1
        // (10)                                         19 | 21
        //

        // Delete drawables that get out of the scope on the left
        // ----->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        // FItems is empty: for I := 0 to -1
        // (A1) for i := 0 to 7      (B1) for i := 8 to 7
        // (A2) for i := 0 to 10     (B2) for i := 8 to 10
        // (A3) for i := 0 to 8      (B3) for i := 8 to 8
        // (A4) for i := 0 to 6      (B4) for i := 8 to 6
        // (A5) for i := 0 to 3      (B5) for i := 8 to 3
        // (A6) for i := 0 to 12     (B6) for i := 8 to 12
        // (A7) for i := 0 to -1     (B7) for i := 8 to -1
        // (A8) for i := 0 to 16     (B8) for i := 8 to 16
        // (A9) for i := 0 to -1     (B9) for i := 8 to -1
        // (A10) for i := 0 to 19    (B10) for i := 8 to 19
        for var I := Max(Low(FItems^){0 if empty}, LOldFirstPreloadedItemIndex) to Min(High(FItems^){-1 if empty}, FFirstPreloadedItemIndex - 1) do
          FItems^[i].Unprepare;

        // Delete drawables that get out of the scope on the right
        // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-----
        // FItems is empty: for I := -1 downto 0
        // (A1) for i := -1 downto 14     (B1) for i := 13 downto 14
        // (A2) for i := -1 downto 18     (B2) for i := 13 downto 18
        // (A3) for i := -1 downto 13     (B3) for i := 13 downto 13
        // (A4) for i := -1 downto 10     (B4) for i := 13 downto 10
        // (A5) for i := -1 downto 9      (B5) for i := 13 downto 9
        // (A6) for i := -1 downto 16     (B6) for i := 13 downto 16
        // (A7) for i := -1 downto 2      (B7) for i := 13 downto 2
        // (A8) for i := -1 downto 21     (B8) for i := 13 downto 21
        // (A9) for i := -1 downto 1      (B9) for i := 13 downto 1
        // (A10) for i := -1 downto 21    (B10) for i := 13 downto 21
        for var I := Min(High(FItems^){-1 if empty}, LOldLastPreloadedItemIndex) downto Max(Low(FItems^){0 if empty}, FLastPreloadedItemIndex + 1) do
          FItems^[i].Unprepare;

        // Add drawabled that enter in the scope on the right
        // >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>+++++
        // (A1) for i := 8 to -1      (B1) for i := 8 to 7
        // (A2) for i := 11 to -1     (B2) for i := 11 to 7
        // (A3) for i := 9 to -1      (B3) for i := 9 to 7
        // (A4) for i := 7 to -1      (B4) for i := 7 to 7
        // (A5) for i := 4 to -1      (B5) for i := 4 to 7
        // (A6) for i := 13 to -1     (B6) for i := 13 to 7
        // (A7) for i := 0 to -1      (B7) for i := 0 to 1
        // (A8) for i := 17 to -1     (B8) for i := 17 to 7
        // (A9) for i := 0 to -1      (B9) for i := 0 to 0
        // (A10) for i := 20 to -1    (B10) for i := 20 to 7
        for var I := Max(Low(FItems^){0 if empty}, FFirstPreloadedItemIndex) to MinIntValue([High(FItems^){-1 if empty}, FLastPreloadedItemIndex, LoldFirstPreloadedItemIndex - 1]) do
          FItems^[i].prepare;

        // Add drawables that enter in the scope on the left
        // +++++<<<<<<<<>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        // (A1) for i := 13 downto 8      (B1) for i := 13 downto 14
        // (A2) for i := 17 downto 11     (B2) for i := 17 downto 14
        // (A3) for i := 12 downto 9      (B3) for i := 12 downto 14
        // (A4) for i := 9 downto 7       (B4) for i := 9 downto 14
        // (A5) for i := 8 downto 4       (B5) for i := 8 downto 14
        // (A6) for i := 15 downto 13     (B6) for i := 15 downto 14
        // (A7) for i := 1 downto 0       (B7) for i := 1 downto 14
        // (A8) for i := 20 downto 17     (B8) for i := 20 downto 17
        // (A9) for i := 0 downto 0       (B9) for i := 0 downto 14
        // (A10) for i := 20 downto 20    (B10) for i := 20 downto 20
        for var I := Min(High(FItems^){-1 if empty}, FLastPreloadedItemIndex) downto MaxIntValue([Low(FItems^){0 if empty}, FFirstPreloadedItemIndex, LoldLastPreloadedItemIndex + 1]) do
          FItems^[i].prepare;

        {$ENDREGION}

        {$REGION 'Handle TopBar with HidesOnScroll'}
        if (FTopBar <> nil) and (FTopBar.HidesOnScroll) and (FMainContent.Visible) then begin

          // {*} When reaching the scroll boundary, aValue.x/y is truncated from FScrollEngine.MaxScrollLimit.x.
          // Example from TALScrollEngine.DoMouseUp:
          //   LMaxY := Trunc(FMaxScrollLimit.y * ALScreenScale);
          // This is by design and reflects how the scroll engine works

          If Orientation = TOrientation.horizontal then begin

            // In the overscroll-at-left zone
            if CompareValue(aValue.X, 0, TEpsilon.Position) <= 0 then begin
              FTopBar.Visible := true;
              FTopBar.Left := 0;
            end

            // Scrolling up (from right to left) — hide the top bar
            else if aValue.X - LOldViewportPosition.X > 0 then begin
              // Not in the overscroll-at-left zone
              if (aValue.X > FScrollEngine.MinScrollLimit.X + 1{*}) then begin
                var LLeft: Double := max(-FTopBar.Width, FTopBar.Left - (aValue.X - LOldViewportPosition.X));
                if LLeft < -FTopBar.Width + TEpsilon.Position then begin
                  FTopBar.Visible := False;
                  FTopBar.Left := -FTopBar.Width;
                end
                else begin
                  FTopBar.Visible := True;
                  FTopBar.Left := LLeft;
                end;
              end
              // In the overscroll-at-left zone
              // force the top bar to be visible
              else begin
                FTopBar.Visible := true;
                FTopBar.Left := 0;
              end;
            end

            // Scrolling down (from left to right) — show the top bar
            else begin
              // Not in the overscroll-at-right zone
              if (aValue.X < FScrollEngine.MaxScrollLimit.X - 1{*}) then begin
                var LLeft: Double := min(0, FTopBar.Left + (LOldViewportPosition.X - aValue.X));
                if LLeft < -FTopBar.Width + TEpsilon.Position then begin
                  FTopBar.Visible := False;
                  FTopBar.Left := -FTopBar.Width;
                end
                else begin
                  FTopBar.Visible := True;
                  FTopBar.Left := LLeft;
                end;
              end
              // In the overscroll-at-right zone
              // force the top bar to be hidden
              else begin
                FTopBar.Visible := false;
                FTopBar.Left := -FTopBar.Width;
              end;
            end;

          end
          else begin

            // In the overscroll-at-top zone
            if CompareValue(aValue.Y, 0, TEpsilon.Position) <= 0 then begin
              FTopBar.Visible := True;
              FTopBar.top := 0;
            end

            // Scrolling up (from bottom to top) — hide the top bar
            else if aValue.Y - LOldViewportPosition.Y > 0 then begin
              // Not in the overscroll-at-top zone
              if (aValue.Y > FScrollEngine.MinScrollLimit.Y + 1{*}) then begin
                var LTop: Double := max(-FTopBar.Height, FTopBar.top - (aValue.Y - LOldViewportPosition.Y));
                if LTop < -FTopBar.Height + TEpsilon.Position then begin
                  FTopBar.Visible := False;
                  FTopBar.top := -FTopBar.Height;
                end
                else begin
                  FTopBar.Visible := True;
                  FTopBar.top := LTop;
                end;
              end
              // In the overscroll-at-top zone
              // force the top bar to be visible
              else begin
                FTopBar.Visible := true;
                FTopBar.top := 0;
              end;
            end

            // Scrolling down (from top to bottom) — show the top bar
            else begin
              // Not in the overscroll-at-bottom zone
              if (aValue.Y < FScrollEngine.MaxScrollLimit.Y - 1{*}) then begin
                var LTop: Double := min(0, FTopBar.top + (LOldViewportPosition.Y - aValue.Y));
                if LTop < -FTopBar.Height + TEpsilon.Position then begin
                  FTopBar.Visible := False;
                  FTopBar.top := -FTopBar.Height;
                end
                else begin
                  FTopBar.Visible := True;
                  FTopBar.top := LTop;
                end;
              end
              // In the overscroll-at-bottom zone
              // force the top bar to be hidden
              else begin
                FTopBar.Visible := false;
                FTopBar.top := -FTopBar.Height;
              end;
            end;

          end;

        end;
        {$ENDREGION}

        {$REGION 'Handle BottomBar with HidesOnScroll'}
        if (FBottomBar <> nil) and (FBottomBar.HidesOnScroll) and (FMainContent.Visible) then begin

          // (*) When reaching the scroll boundary, aValue.x/y is truncated from FScrollEngine.MaxScrollLimit.x.
          // Example from TALScrollEngine.DoMouseUp:
          //   LMaxY := Trunc(FMaxScrollLimit.y * ALScreenScale);
          // This is by design and reflects how the scroll engine works

          If Orientation = TOrientation.horizontal then begin

            // In the overscroll-at-left zone
            if CompareValue(aValue.X, 0, TEpsilon.Position) <= 0 then begin
              FBottomBar.Visible := true;
              FBottomBar.Left := Width - FBottomBar.Width;
            end

            // Scrolling up (from right to left) — hide the bottom bar
            else if aValue.X - LOldViewportPosition.X > 0 then begin
              // Not in the overscroll-at-left zone
              if (aValue.X > FScrollEngine.MinScrollLimit.X + 1{*}) then begin
                var LLeft: Double := min(Width, FBottomBar.Left + (aValue.X - LOldViewportPosition.X));
                if LLeft > Width - TEpsilon.Position then begin
                  FBottomBar.Visible := False;
                  FBottomBar.Left := Width;
                end
                else begin
                  FBottomBar.Visible := True;
                  FBottomBar.Left := LLeft;
                end;
              end
              // In the overscroll-at-left zone
              // force the bottom bar to be visible
              else begin
                FBottomBar.Visible := true;
                FBottomBar.Left := Width - FBottomBar.Width;
              end;
            end

            // Scrolling down (from left to right) — show the bottom bar
            else begin
              // Not in the overscroll-at-right zone
              if (aValue.X < FScrollEngine.MaxScrollLimit.X - 1{*}) then begin
                var LLeft: Double := max(Width - FBottomBar.Width, FBottomBar.Left - (LOldViewportPosition.X - aValue.X));
                if LLeft > Width - TEpsilon.Position then begin
                  FBottomBar.Visible := False;
                  FBottomBar.Left := Width;
                end
                else begin
                  FBottomBar.Visible := True;
                  FBottomBar.Left := LLeft;
                end;
              end
              // In the overscroll-at-right zone
              // force the bottom bar to be hidden
              else begin
                FBottomBar.Visible := false;
                FBottomBar.Left := Width;
              end;
            end;

          end
          else begin

            // In the overscroll-at-top zone
            if CompareValue(aValue.Y, 0, TEpsilon.Position) <= 0 then begin
              FBottomBar.Visible := true;
              FBottomBar.top := Height - FBottomBar.Height;
            end

            // Scrolling up (from bottom to top) — hide the bottom bar
            else if aValue.Y - LOldViewportPosition.Y > 0 then begin
              // Not in the overscroll-at-top zone
              if (aValue.Y > FScrollEngine.MinScrollLimit.Y + 1{*}) then begin
                var LTop: Double := min(Height, FBottomBar.top + (aValue.Y - LOldViewportPosition.Y));
                if LTop > Height - TEpsilon.Position then begin
                  FBottomBar.Visible := False;
                  FBottomBar.top := Height;
                end
                else begin
                  FBottomBar.Visible := True;
                  FBottomBar.top := LTop;
                end;
              end
              // In the overscroll-at-top zone
              // force the bottom bar to be visible
              else begin
                FBottomBar.Visible := true;
                FBottomBar.top := Height - FBottomBar.Height;
              end;
            end

            // Scrolling down (from top to bottom) — show the bottom bar
            else begin
              // Not in the overscroll-at-bottom zone
              if (aValue.Y < FScrollEngine.MaxScrollLimit.Y - 1{*}) then begin
                var LTop: Double := max(Height - FBottomBar.Height, FBottomBar.top - (LOldViewportPosition.Y - aValue.Y));
                if LTop > Height - TEpsilon.Position then begin
                  FBottomBar.Visible := False;
                  FBottomBar.top := Height;
                end
                else begin
                  FBottomBar.Visible := True;
                  FBottomBar.top := LTop;
                end;
              end
              // In the overscroll-at-bottom zone
              // force the bottom bar to be hidden
              else begin
                FBottomBar.Visible := false;
                FBottomBar.top := Height;
              end;
            end;

          end;

        end;
        {$ENDREGION}

        {$REGION 'Handle FPullToRefreshIndicator'}
        if (FPullToRefreshIndicator <> nil) then begin

          // (*) When reaching the scroll boundary, aValue.x/y is truncated from FScrollEngine.MaxScrollLimit.x.
          // Example from TALScrollEngine.DoMouseUp:
          //   LMaxY := Trunc(FMaxScrollLimit.y * ALScreenScale);
          // This is by design and reflects how the scroll engine works

          If Orientation = TOrientation.horizontal then begin

            // Calculate LEdgeOffset
            var LEdgeOffset: Single;
            If (FTopBar <> nil) and (FTopBar.Visible) then LEdgeOffset := FTopBar.Left + FTopBar.Width
            else LEdgeOffset := 0;

            // IsRefreshing
            if FPullToRefreshIndicator.IsRefreshing then begin
              FPullToRefreshIndicator.Visible := True;
              FPullToRefreshIndicator.SetPosition(max(LEdgeOffset + FPullToRefreshIndicator.PullThreshold, min(FPullToRefreshIndicator.Left, LEdgeOffset - (aValue.Y*1.5) - FPullToRefreshIndicator.Width)), (Height - FPullToRefreshIndicator.Height) / 2);
            end

            // not IsRefreshing
            else begin

              // In the overscroll-at-left zone
              if (FPullToRefreshIndicator.CanTriggerRefresh) and (aValue.Y < FScrollEngine.MinScrollLimit.Y - 1{*}) then begin
                FPullToRefreshIndicator.Visible := True;
                FPullToRefreshIndicator.SetPosition({LEdgeOffset}0 - (aValue.Y*1.5) - FPullToRefreshIndicator.Width, (Height - FPullToRefreshIndicator.Height) / 2);
                FPullToRefreshIndicator.SetPullProgress((FPullToRefreshIndicator.Left - LEdgeOffset) / FPullToRefreshIndicator.PullThreshold);
              end
              // Not in the overscroll-at-left zone
              // force the PullToRefreshIndicator to be hidden
              else
                FPullToRefreshIndicator.Visible := false;

            end;

          end
          else begin

            // Calculate LEdgeOffset
            var LEdgeOffset: Single;
            If (FTopBar <> nil) and (FTopBar.Visible) then LEdgeOffset := FTopBar.top + FTopBar.Height
            else LEdgeOffset := 0;

            // IsRefreshing
            if FPullToRefreshIndicator.IsRefreshing then begin
              FPullToRefreshIndicator.Visible := True;
              FPullToRefreshIndicator.SetPosition((Width - FPullToRefreshIndicator.Width) / 2,  max(LEdgeOffset + FPullToRefreshIndicator.PullThreshold, min(FPullToRefreshIndicator.top, LEdgeOffset - (aValue.Y*1.5) - FPullToRefreshIndicator.Height)));
            end

            // not IsRefreshing
            else begin

              // In the overscroll-at-top zone
              if (FPullToRefreshIndicator.CanTriggerRefresh) and (aValue.Y < FScrollEngine.MinScrollLimit.Y - 1{*}) then begin
                FPullToRefreshIndicator.Visible := True;
                FPullToRefreshIndicator.SetPosition((Width - FPullToRefreshIndicator.Width) / 2,  {LEdgeOffset}0 - (aValue.Y*1.5) - FPullToRefreshIndicator.Height);
                FPullToRefreshIndicator.SetPullProgress((FPullToRefreshIndicator.Top - LEdgeOffset) / FPullToRefreshIndicator.PullThreshold);
              end
              // Not in the overscroll-at-top zone
              // force the PullToRefreshIndicator to be hidden
              else
                FPullToRefreshIndicator.Visible := false;

            end;

          end;

        end;
        {$ENDREGION}

        {$REGION 'Handle FLoadMoreIndicator'}
        if (FLoadMoreIndicator <> nil) then begin
          If (FMainContent.Visible) and (FPaginationToken <> '') and (FDownloadItemsErrorCode = 0) then begin
            If Orientation = TOrientation.horizontal then begin
              var LLeft: Double := FMainContent.Width - AValue.X - FLoadMoreIndicator.Width - FLoadMoreIndicator.Margins.Right;
              if LLeft > Width then FLoadMoreIndicator.Visible := False
              else begin
                FLoadMoreIndicator.Visible := True;
                FLoadMoreIndicator.Left := LLeft;
              end;
            end
            else begin
              var LTop: Double := FMainContent.Height - AValue.Y - FLoadMoreIndicator.Height - FLoadMoreIndicator.Margins.Bottom;
              if LTop > Height then FLoadMoreIndicator.Visible := False
              else begin
                FLoadMoreIndicator.Visible := True;
                FLoadMoreIndicator.top := LTop;
              end;
            end;
          end
          else FLoadMoreIndicator.Visible := False;
        end;
        {$ENDREGION}

        {$REGION 'Handle FLoadMoreRetryButton'}
        if (FLoadMoreRetryButton <> nil) then begin
          If (FMainContent.Visible) and (FPaginationToken <> '') and (FDownloadItemsErrorCode <> 0) then begin
            If Orientation = TOrientation.horizontal then begin
              var LLeft: Double := FMainContent.Width - AValue.X - FLoadMoreRetryButton.Width - FLoadMoreRetryButton.Margins.Right;
              if LLeft > Width then FLoadMoreRetryButton.Visible := False
              else begin
                FLoadMoreRetryButton.Visible := True;
                FLoadMoreRetryButton.Left := LLeft;
              end;
            end
            else begin
              var LTop: Double := FMainContent.Height - AValue.Y - FLoadMoreRetryButton.Height - FLoadMoreRetryButton.Margins.Bottom;
              if LTop > Height then FLoadMoreRetryButton.Visible := False
              else begin
                FLoadMoreRetryButton.Visible := True;
                FLoadMoreRetryButton.top := LTop;
              end;
            end;
          end
          else FLoadMoreRetryButton.Visible := False;
        end;
        {$ENDREGION}

        {$REGION 'DownloadItems'}
        DownloadItems;
        {$ENDREGION}

        {$REGION 'repaint'}
        Repaint;
        {$ENDREGION}

      Finally
        FDisableAlign := LPrevDisableAlign;
      End;

    finally
      TNonReentrantHelper.LeaveSection(FIsSettingViewportPosition);
    End;

    {$REGION 'OnViewportPositionChange'}
    if (assigned(FOnViewportPositionChange)) and
       (not fLastViewportPosition.EqualsTo(AValue, TEpsilon.Position)) then
      FOnViewportPositionChange(Self, fLastViewportPosition, AValue);
    fLastViewportPosition := AValue;
    {$ENDREGION}

  end;
end;

{********************************************}
procedure TALDynamicListBox.TView.LockItemIds;
begin
  ALMonitorEnter(FUniqueInt64ItemIds{$IF defined(DEBUG)}, 'TALDynamicListBox.TView.LockItemIds'{$ENDIF});
end;

{**********************************************}
procedure TALDynamicListBox.TView.UnLockItemIds;
begin
  ALMonitorExit(FUniqueInt64ItemIds{$IF defined(DEBUG)}, 'TALDynamicListBox.TView.LockItemIds'{$ENDIF});
end;

{*******************************************************************************************}
function TALDynamicListBox.TView.DownloadItems(const AForceReload: Boolean = False): boolean;
begin

  // Exit if the last download resulted in an error, unless AForceReload is True
  // (e.g., when triggered by a "Reload" button click)
  if (not AForceReload) and
     (FDownloadItemsErrorCode <> 0) then exit(False);

  // Exit if no data is needed
  if not CanDownloadItems then exit(false);

  // Exit if a thread is already performing the task
  if IsDownloadItemsRunning then exit(true);

  // Before starting the background thread
  {$IFDEF DEBUG}
  ALLog(ClassName+'.DownloadItems', 'ForceReload: ' + ALBoolToStrW(AForceReload));
  {$ENDIF}
  FDownloadItemsErrorCode := 0;

  // Load the data in a separate thread to avoid blocking the calling thread
  FDownloadItemsContext := CreateDownloadItemsContext;
  Try
    TALNetHttpClientPool.Instance.ExecuteProc(
      DownloadItemsBackgroundProc, // const AProc: TALWorkerThreadObjProc;
      FDownloadItemsContext, // const AContext: Tobject; // Context will be free by the worker thread
      GetDownloadPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
  except
    ALFreeAndNil(FDownloadItemsContext);
    Raise;
  End;

  // Return True
  result := true;

end;

{***************************************************************}
function TALDynamicListBox.TView.HasMoreItemsToDownload: Boolean;
begin
  Result := FPaginationtoken <> '';
end;

{***********************************************************}
function TALDynamicListBox.TView.RetryDownloadItems: boolean;
begin
  result := DownloadItems(true{AForceReload});
  if result then SetViewportPosition(ViewportPosition);
end;

{***********************************************************************************************************}
function TALDynamicListBox.TView.ScrollToItemIndex(const AIndex: Integer; Const ADuration: integer): Boolean;
begin
  if not FItems^[AIndex].Visible then Exit(False);

  var LStopX: single := FItems^[AIndex].left;
  var LStopY: single := FItems^[AIndex].Top;
  If Orientation = TOrientation.horizontal then LStopX := LStopX - FMainContent.Padding.Left
  else LStopY := LStopY - FMainContent.Padding.Top;

  If FTopBar.Visible then begin
    if not FTopBar.hidesOnScroll then begin
      If Orientation = TOrientation.horizontal then LStopX := LStopX - FTopBar.Height
      else LStopY := LStopY - FTopBar.Height;
    end
    else begin
      If Orientation = TOrientation.horizontal then begin
        // Scrolling up (from right to left) — hide the top bar
        if LStopX - scrollengine.ViewportPosition.X > 0 then begin
          FTopBar.Visible := False;
          FTopBar.Left := -FTopBar.Width;
        end
        // Scrolling down (from left to right) — show the top bar
        else begin
          FTopBar.Visible := True;
          FTopBar.Left := 0;
          LStopX := LStopX - FTopBar.Width;
        end;
      end
      else begin
        // Scrolling up (from bottom to top) — hide the top bar
        if LStopY - scrollengine.ViewportPosition.Y > 0 then begin
          FTopBar.Visible := False;
          FTopBar.Top := -FTopBar.Height;
        end
        // Scrolling down (from top to bottom) — show the top bar
        else begin
          FTopBar.Visible := True;
          FTopBar.Top := 0;
          LStopY := LStopY - FTopBar.Height;
        end;
      end;
    end;
  end;

  scrollengine.startScroll(
    scrollengine.ViewportPosition.x,// startX: Double;
    scrollengine.ViewportPosition.y, // startY: Double;
    min(LStopX, scrollengine.MaxScrollLimit.x) - scrollengine.ViewportPosition.x, // dx: Double;
    min(LStopY, scrollengine.MaxScrollLimit.Y) - scrollengine.ViewportPosition.y, // dy: Double;
    ADuration); // const duration: integer = TALOverScroller.DEFAULT_DURATION)

  Result := true;

end;

{**************************************************************************************************}
function TALDynamicListBox.TView.ScrollToItem(const AId: String; Const ADuration: integer): Boolean;
begin
  for var I := low(FItems^) to High(FItems^) do
    if (FItems^[i].Visible) and (FItems^[i].Data.GetChildNodeValueText(FItemIdNodeName, '') = AId) then begin
      result := ScrollToItemIndex(i, ADuration);
      Exit;
    end;
  Result := False;
end;

{*************************************************************************************************}
function TALDynamicListBox.TView.ScrollToItem(const AId: Int64; Const ADuration: integer): Boolean;
begin
  for var I := low(FItems^) to High(FItems^) do
    if (FItems^[i].Visible) and (FItems^[i].Data.GetChildNodeValueInt64(FItemIdNodeName, 0) = AId) then begin
      result := ScrollToItemIndex(i, ADuration);
      Exit;
    end;
  Result := False;
end;

{****************************************}
procedure TALDynamicListBox.TView.Refresh;
begin
  if not IsMainView then
    Raise Exception.Create('Only the main view can be refreshed.');
  if FRefreshingView <> nil then exit;
  FRefreshingView := Host.CreateMainView;
  FRefreshingView.Prepare;
  if FRefreshingTimer = nil then begin
    FRefreshingTimer := TALDisplayTimer.Create;
    FRefreshingTimer.Interval := 0.2;
    FRefreshingTimer.OnProcess := RefreshingTimerProcess;
    FRefreshingTimer.Enabled := True;
  end;
end;

{***********************************************************************************}
procedure TALDynamicListBox.TView.RefreshTransitionAnimationProcess(Sender: TObject);
begin
  case FRefreshTransitionKind of
    TRefreshTransitionKind.DirectFadeIn: FRefreshingView.Opacity := FRefreshTransitionAnimation.CurrentValue;
    TRefreshTransitionKind.DirectFadeOut: Opacity := FRefreshTransitionAnimation.CurrentValue;
    TRefreshTransitionKind.OverlayFadeIn: FRefreshingView.Opacity := FRefreshTransitionAnimation.CurrentValue;
    TRefreshTransitionKind.RevealFadeOut: Opacity := FRefreshTransitionAnimation.CurrentValue;
    TRefreshTransitionKind.CrossFade: begin
      Opacity := 1-FRefreshTransitionAnimation.CurrentValue;
      FRefreshingView.Opacity := FRefreshTransitionAnimation.CurrentValue;
    end;
    else raise Exception.Create('Error AF651414-E6EE-4B42-90D1-3509657FCB22');
  end;
  Repaint;
end;

{**********************************************************************************}
procedure TALDynamicListBox.TView.RefreshTransitionAnimationFinish(Sender: TObject);
begin
  Host.HitTest := True;
  var LRefreshingView := FRefreshingView;
  FRefreshingView := nil;
  LRefreshingView.Opacity := 1;
  Host.MainView := LRefreshingView;
end;

{************************************************************************}
procedure TALDynamicListBox.TView.RefreshingTimerProcess(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createRefreshTransitionAnimation;
  begin
    {$If defined(DEBUG)}
    if FRefreshTransitionAnimation <> nil then
      Raise Exception.Create('Error 79000123-2EA0-41A4-A93A-B181BF7FCF83');
    {$ENDIf}
    FRefreshTransitionAnimation := TALFloatAnimation.Create;
    FRefreshTransitionAnimation.OnProcess := RefreshTransitionAnimationProcess;
    FRefreshTransitionAnimation.OnFinish := RefreshTransitionAnimationFinish;
    FRefreshTransitionAnimation.InterpolationType := TALInterpolationType.Material3ExpressiveDefaultEffects;
    FRefreshTransitionAnimation.duration := 0.2;
  end;

begin

  {$If defined(DEBUG)}
  if FRefreshingView = nil then
    Raise Exception.Create('Error EE277FA4-FC66-410F-B853-0A6AA2F04F3A');
  {$ENDIf}

  if (not Host.Pressed) and
     (not FScrollEngine.Down) and
     (Host.FActiveScrollEnginesCount = 0) and
     (not TALDialogManager.Instance.IsShowingDialog) and
     (FRefreshingView.IsReadyToDisplay(True{AStrict})) then begin

    FRefreshingTimer.Enabled := False;
    var LRefreshTransitionKind := FRefreshTransitionKind;
    if (LRefreshTransitionKind = TRefreshTransitionKind.Scroll) and
       (Host.MainView.scrollengine.ViewportPosition.EqualsTo(TALPointD.Zero, TEpsilon.Position)) then
      LRefreshTransitionKind := TRefreshTransitionKind.None;

    Host.HitTest := False;

    case LRefreshTransitionKind of
      TRefreshTransitionKind.None: begin
        Host.HitTest := True;
        var LRefreshingView := FRefreshingView;
        FRefreshingView := nil;
        Host.MainView := LRefreshingView;
      end;
      //--
      TRefreshTransitionKind.Scroll: begin
        var LDelta: double;
        If (Orientation = TOrientation.Horizontal) then LDelta := abs(Host.MainView.scrollengine.ViewportPosition.x)
        else LDelta := abs(Host.MainView.scrollengine.ViewportPosition.y);

        var LDuration: integer := min(500, round(LDelta / 5{pixels per ms}));
        {$If defined(DEBUG)}
        ALLog(ClassName+'.RefreshingTimerProcess', 'Duration: ' + ALintToStrW(LDuration));
        {$ENDIf}

        Host.MainView.scrollengine.startScroll(
          Host.MainView.scrollengine.ViewportPosition.x,// startX: Double;
          Host.MainView.scrollengine.ViewportPosition.y, // startY: Double;
          -Host.MainView.scrollengine.ViewportPosition.x, // dx: Double;
          -Host.MainView.scrollengine.ViewportPosition.y, // dy: Double;
          LDuration); // const duration: integer = TALOverScroller.DEFAULT_DURATION)
      end;
      //--
      TRefreshTransitionKind.DirectFadeIn: begin
        Host.FRefreshingView := FRefreshingView;
        Opacity := 0;
        _createRefreshTransitionAnimation;
        FRefreshTransitionAnimation.StartValue := 0;
        FRefreshTransitionAnimation.StopValue := 1;
        FRefreshTransitionAnimation.Start;
      end;
      //--
      TRefreshTransitionKind.DirectFadeOut: begin
        _createRefreshTransitionAnimation;
        FRefreshTransitionAnimation.StartValue := 1;
        FRefreshTransitionAnimation.StopValue := 0;
        FRefreshTransitionAnimation.Start;
      end;
      //--
      TRefreshTransitionKind.OverlayFadeIn: begin
        Host.FRefreshingView := FRefreshingView;
        _createRefreshTransitionAnimation;
        FRefreshTransitionAnimation.StartValue := 0;
        FRefreshTransitionAnimation.StopValue := 1;
        FRefreshTransitionAnimation.Start;
      end;
      //--
      TRefreshTransitionKind.RevealFadeOut: begin
        Host.FRefreshingView := FRefreshingView;
        _createRefreshTransitionAnimation;
        FRefreshTransitionAnimation.StartValue := 1;
        FRefreshTransitionAnimation.StopValue := 0;
        FRefreshTransitionAnimation.Start;
      end;
      //--
      TRefreshTransitionKind.CrossFade: begin
        Host.FRefreshingView := FRefreshingView;
        _createRefreshTransitionAnimation;
        FRefreshTransitionAnimation.StartValue := 0;
        FRefreshTransitionAnimation.StopValue := 1;
        FRefreshTransitionAnimation.InterpolationType := TALInterpolationType.Material3ExpressiveSlowEffects;
        FRefreshTransitionAnimation.duration := 0.3;
        FRefreshTransitionAnimation.Start;
      end;
      //--
      else raise Exception.Create('Error AF651414-E6EE-4B42-90D1-3509657FCB22');
    end;

  end;

end;

{*********************************************************************************}
function TALDynamicListBox.TView.CreateDownloadItemsContext: TDownloadItemsContext;
begin
  Result := TDownloadItemsContext.Create(Self);
end;

{*****************************************************************************************}
class procedure TALDynamicListBox.TView.DownloadItemsBackgroundProc(var AContext: Tobject);
begin
  var LContext := TDownloadItemsContext(AContext);
  if LContext.FOwner = nil then exit;
  try

    var LData: TALJSONNodeW := nil;
    var LFreeItems := True;
    var LItems: TArray<TItem> := nil;
    Try

      var LDownloadItemsErrorCode: Integer := 0;
      DownloadItemsBackgroundProcFetchData(LContext, LData, LDownloadItemsErrorCode);

      if LContext.FOwner = nil then exit;
      DownloadItemsBackgroundProcCreateItems(LContext, LDownloadItemsErrorCode, LData, LItems);

      while not DownloadItemsBackgroundProcCanProcessItems(LContext) do begin
        if LContext.FOwner = nil then exit;
        sleep(250);
      end;

      if LContext.FOwner = nil then exit;
      TThread.queue(nil,
        procedure
        begin
          Try
            if LContext.FOwner <> nil then begin
              var LOwner := LContext.owner;
              LOwner.DownloadItemsProcessItems(LContext, LDownloadItemsErrorCode, LItems);
              LOwner.FDownloadItemsContext := nil;
              LOwner.DownloadItemsFinished;
            end;
          finally
            ALFreeAndNil(LContext);
            For var I := Low(LItems) to High(LItems) do
              ALFreeAndNil(LItems[i]);
          End;
        end);
      AContext := nil; // AContext will be free by TThread.queue
      LFreeItems := False; // LItems will be free by TThread.queue

    finally
      ALfreeAndNil(LData);
      if LFreeItems then
        For var I := Low(LItems) to High(LItems) do
          ALFreeAndNil(LItems[i]);
    end;

  Except
    On E: Exception do begin
      ALLog('TALDynamicListBox.TView.DownloadItemsBackgroundProc', E);
      ALMonitorEnter(LContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TView.DownloadItemsBackgroundProc'{$ENDIF});
      try
        if LContext.FOwner <> nil then begin
          LContext.FManagedByWorkerThread := False;
          AContext := nil; // AContext will be free by CancelDownloadItems
        end;
      finally
        ALMonitorExit(LContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TView.DownloadItemsBackgroundProc'{$ENDIF});
      end;
    end;
  end;
end;

{**************}
// [MultiThread]
class procedure TALDynamicListBox.TView.DownloadItemsBackgroundProcFetchData(
                  const AContext: TDownloadItemsContext;
                  out AData: TALJSONNodeW;
                  var AErrorCode: Integer);
begin

  // We cannot call ALMonitorEnter(AContext.FLock) here because
  // AContext.FOnDownloadItems performs a long HTTP request.
  // In the meantime, CancelDownloadItems might be called, and it
  // also needs to acquire the same lock to set AContext.FOwner to nil.
  // Therefore, it's the responsibility of FOnDownloadItems to call
  // ALMonitorEnter(AContext.FLock) if it needs to access AContext.FOwner.

  if AContext.FOwner = nil then exit;
  if not assigned(AContext.FOnDownloadItems) then
    Raise Exception.Create('Error DF2328CA-BCF7-46D6-B100-AFD222FF8873');
  //var LMethod: TMethod;
  //LMethod.Code := TMethod(AContext.FOnDownloadItems).Code;
  // Set Self to nil to prevent accidental access to instance members,
  // as we are in a multithreaded context where most members are not thread-safe.
  // Self can still be accessed via AContext.Owner, but this should be done with caution.
  //LMethod.Data := nil;
  if AContext.PaginationToken = #0 then
    AContext.PaginationToken := '';
  //TDownloadItemsEvent(LMethod)(
  //  AContext, // const AContext: TDownloadItemsContext;
  //  AData, // Const AData: TALJSONNodeW;
  //  AContext.PaginationToken, // var APaginationToken: String;
  //  AErrorCode); // var AErrorCode: Integer
  AContext.FOnDownloadItems(
    AContext, // const AContext: TDownloadItemsContext;
    AData, // Const AData: TALJSONNodeW;
    AContext.PaginationToken, // var APaginationToken: String;
    AErrorCode); // var AErrorCode: Integer

end;

{**************}
// [MultiThread]
class procedure TALDynamicListBox.TView.DownloadItemsBackgroundProcCreateItems(
                  const AContext: TDownloadItemsContext;
                  const AErrorCode: Integer;
                  const AData: TALJSONNodeW;
                  out AItems: TArray<TItem>);
begin
  ALMonitorEnter(AContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TView.DownloadItemsBackgroundProcCreateItems'{$ENDIF});
  try

    // Exit
    if (AContext.FOwner = nil) or
       (AErrorCode <> 0) or
       (AData = nil) or
       (AData.ChildNodes.Count = 0) then begin
      AItems := nil;
      Exit;
    end;

    // Calculate ItemIdType
    If AContext.ItemIdType = TItemIdType.Unknown then begin
      var LIdNode := AData.ChildNodes[0].getChildNode(AContext.ItemIdNodeName);
      if LIdNode.NodeSubType in [nstInt32, nstInt64] then AContext.ItemIdType := TItemIdType.Int64
      else AContext.ItemIdType := TItemIdType.Text;
    end;

    // Init LOwner
    var LOwner := AContext.Owner;

    // Deduplicates items
    LOwner.LockItemIds;
    try
      var Lcount: integer;
      if Acontext.ItemIdType = TItemIdType.Int64 then LCount := LOwner.FUniqueInt64ItemIds.Count
      else LCount := LOwner.FUniqueTextItemIds.Count;
      for var i := AData.ChildNodes.Count - 1 downto 0 do begin
        if Lcount >= Acontext.MaxItems then break;
        var LItemNode := AData.ChildNodes[i];
        if AContext.ItemIdType = TItemIdType.Int64 then begin
          var LItemId := LItemNode.getChildNodeValueInt64(AContext.ItemIdNodeName, 0);
          if (LItemID <> 0) and (not LOwner.FUniqueInt64ItemIds.TryAdd(LItemID, true)) then begin
            AData.ChildNodes.Delete(i);
            continue;
          end;
        end
        else begin
          var LItemId := LItemNode.getChildNodeValueText(AContext.ItemIdNodeName, '');
          if (LItemID <> '') and (not LOwner.FUniqueTextItemIds.TryAdd(LItemID, true)) then begin
            AData.ChildNodes.Delete(i);
            continue;
          end;
        end;
        inc(LCount);
      end;
    finally
      LOwner.UnLockItemIds
    end;

    // Update AItems
    Setlength(AItems, AData.ChildNodes.Count);
    For var I := AData.ChildNodes.Count - 1 downto 0 do begin
      var LData := AData.ChildNodes.Extract(I);
      LData.ChildNodes.SetSorted(true{Value},true{recurse});
      LData.MultiThreadPrepare(true{aOnlyChildList});
      var LItem: TItem;
      If assigned(AContext.FOnCreateItem) then begin
        //var LMethod: TMethod;
        //LMethod.Code := TMethod(AContext.FOnCreateItem).Code;
        // Set Self to nil to prevent accidental access to instance members,
        // as we are in a multithreaded context where most members are not thread-safe.
        // Self can still be accessed via AContext.Owner, but this should be done with caution.
        //LMethod.Data := nil;
        //LItem := TCreateItemEvent(LMethod)(
        //           AContext, // const AContext: TDownloadItemsContext;
        //           LData); // Const AData: TALJSONNodeW;
        LItem := AContext.FOnCreateItem(
                   AContext, // const AContext: TDownloadItemsContext;
                   LData); // Const AData: TALJSONNodeW;
      end
      else begin
        LItem := TItem.Create(nil);
        LItem.OnCreateMainContent := AContext.FOnCreateItemMainContent;
        LItem.OnCreateLoadingContent := AContext.FOnCreateItemLoadingContent;
        LItem.OnCreateErrorContent := AContext.FOnCreateItemErrorContent;
        LItem.OnDownloadData := AContext.FOnDownloadItemData;
      end;
      if LItem.FData = nil then LItem.FData := LData
      else ALFreeAndNil(LData);
      AItems[i] := LItem;
    end;

  finally
    ALMonitorExit(AContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TView.DownloadItemsBackgroundProcCreateItems'{$ENDIF});
  end;
end;

{**************}
// [MultiThread]
class function TALDynamicListBox.TView.DownloadItemsBackgroundProcCanProcessItems(const AContext: TDownloadItemsContext): boolean; // [MultiThread]
begin
  if TThread.Current.ThreadID = MainThreadID then exit(true);
  ALMonitorEnter(AContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TView.DownloadItemsBackgroundProcCanProcessItems'{$ENDIF});
  try
    // Primarily because we want to prevent the list
    // from being updated during the bottom-bound animation.
    var LOwner := TView(AContext.FOwner);
    result := (LOwner = nil) or
              (LOwner.Host = nil) or
              ((not LOwner.IsPreloadingContent) and
               (not LOwner.Host.HasActiveScrollEngines));
  finally
    ALMonitorExit(AContext.FLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TView.DownloadItemsBackgroundProcCanProcessItems'{$ENDIF});
  end;
end;

{**********************************************************}
procedure TALDynamicListBox.TView.DownloadItemsProcessItems(
            const AContext: TDownloadItemsContext;
            const AErrorCode: Integer;
            var AItems: TArray<TItem>);
begin
  FDownloadItemsErrorCode := AErrorCode;
  if FDownloadItemsErrorCode = 0 then begin

    {$IFDEF DEBUG}
    var LStopWatch := TstopWatch.startNew;
    try
    {$ENDIF}

      // Create the MainContent
      if FMainContent = nil then begin
        CreateAndActivateContent(MainContentType);
        {$IFDEF DEBUG}
        if FMainContent = nil then
          Raise Exception.Create('Error 6482DF15-1503-4D36-84E5-FBF560650F28');
        {$ENDIF}
        FItems := @FMainContent.FControls;
      end;

      // Update FPaginationToken
      FPaginationToken := AContext.PaginationToken;

      // Add the items
      if (length(AItems) > 0) then begin
        TMainContent(FMainContent).InsertItems(AItems, Maxint);
        FTriggerDownloadItemsAtIndex := High(FItems^) - (length(AItems) div 3);
        AItems := nil;
      end
      // Adjust the size of FMainContent to reclaim space previously
      // occupied by the LoadMoreIndicator or LoadMoreRetryButton,
      // and potentially reveal hidden items that were waiting for
      // additional records to be properly aligned.
      else
        TMainContent(FMainContent).Realign(High(FItems^)+1);

    {$IFDEF DEBUG}
    finally
      LStopWatch.Stop;
      ALLog(ClassName+'.DownloadItemsProcessItems', 'timeTaken: ' + ALFloatToStrW(LStopWatch.Elapsed.TotalMilliseconds));
    end;
    {$ENDIF}

  end;
end;

{******************************************************}
procedure TALDynamicListBox.TView.DownloadItemsFinished;
begin
  if FDownloadItemsErrorCode <> 0 then begin
    // Adjust size to accommodate the retry button
    // and to show the retry button (SetViewportPosition is
    // called inside AdjustSize);
    if FMainContent <> nil then FMainContent.AdjustSize;
    if Host <> nil then
      Host.ShowErrorMessageBanner(FDownloadItemsErrorCode);
  end
  else
    SetViewportPosition(ViewportPosition);
end;

{*********************************************************}
function TALDynamicListBox.TView.CanDownloadItems: Boolean;
begin
  Result := (FLastVisibleItemIndex >= FTriggerDownloadItemsAtIndex) and
            (FPaginationToken <> '');
end;

{***************************************************************}
function TALDynamicListBox.TView.IsDownloadItemsRunning: Boolean;
begin
  result := FDownloadItemsContext <> nil;
end;

{****************************************************}
procedure TALDynamicListBox.TView.CancelDownloadItems;
begin
  // The FDownloadItemsContext pointer can only be
  // updated in the main thread, so there is no need
  // to lock its access for reading or updating.
  if FDownloadItemsContext <> nil then begin
    var LContextToFree: TDownloadItemsContext;
    var LLock := FDownloadItemsContext.FLock;
    ALMonitorEnter(LLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TView.CancelDownloadItems'{$ENDIF});
    try
      if not FDownloadItemsContext.FManagedByWorkerThread then LContextToFree := FDownloadItemsContext
      else LContextToFree := nil;
      FDownloadItemsContext.FOwner := nil;
      FDownloadItemsContext := nil;
    Finally
      ALMonitorExit(LLock{$IF defined(DEBUG)}, 'TALDynamicListBox.TView.CancelDownloadItems'{$ENDIF});
    End;
    ALFreeAndNil(LContextToFree);
  end;
end;

{***************************************************}
function TALDynamicListBox.TView.IsMainView: Boolean;
begin
  Result := (Host <> nil) and
            (Host.MainView = Self)
end;

{**************************************************************************************}
function TALDynamicListBox.TView.CanCreateContent(const AContentType: Integer): Boolean;
begin
  case AContentType of
    0{surrounding content}: Result := False;
    MainContentType: result := (FMainContent = nil) and (assigned(FOnCreateMainContent));
    LoadingContentType: result := (FLoadingContent = nil) and (assigned(FOnCreateLoadingContent));
    ErrorContentType: result := (FErrorContent = nil) and (assigned(FOnCreateErrorContent));
    NoItemsContentType: result := (FNoItemsContent = nil) and (assigned(FOnCreateNoItemsContent));
    else Raise Exception.Create('Error A9AEA850-A65C-45C5-90CF-827C3790A7C0')
  end;
  Result := Result or
            ((FBackgroundContent = nil) and assigned(FOnCreateBackgroundContent)) or
            ((FForegroundContent = nil) and assigned(FOnCreateForegroundContent)) or
            ((FTopBar = nil) and assigned(FOnCreateTopBar)) or
            ((FBottomBar = nil) and assigned(FOnCreateBottomBar)) or
            ((FPullToRefreshIndicator = nil) and assigned(FOnCreatePullToRefreshIndicator)) or
            ((FLoadMoreIndicator = nil) and assigned(FOnCreateLoadMoreIndicator)) or
            ((FLoadMoreRetryButton = nil) and assigned(FOnCreateLoadMoreRetryButton));
end;

{**********************************************************************************************************************}
function TALDynamicListBox.TView.CreateContentBuilderContext(const AContentType: Integer): TItem.TContentBuilderContext;
begin
  Result := TView.TContentBuilderContext.Create(Self, AContentType);
end;

{**************}
// [MultiThread]
class procedure TALDynamicListBox.TView.CreateContextContent(const AContext: TItem.TContentBuilderContext);
begin
  if AContext.FContentType <> 0{surrounding content} then
    Inherited CreateContextContent(AContext);
  var LContext := TContentBuilderContext(AContext);
  {$IF defined(debug)}
  if (LContext.NewBackgroundContent <> nil) or
     (LContext.NewForegroundContent <> nil) or
     (LContext.NewTopBar <> nil) or
     (LContext.NewBottomBar <> nil) or
     (LContext.NewPullToRefreshIndicator <> nil) or
     (LContext.NewLoadMoreIndicator <> nil) or
     (LContext.NewLoadMoreRetryButton <> nil) then
    Raise Exception.Create('Error 0350C5EF-C261-498F-A2C1-C6EB0FDE6744');
  {$ENDIF}
  If assigned(LContext.OnCreateBackgroundContent) then LContext.NewBackgroundContent := CreateContent(BackgroundContentType, AContext);
  If assigned(LContext.OnCreateForegroundContent) then LContext.NewForegroundContent := CreateContent(ForegroundContentType, AContext);
  If assigned(LContext.OnCreateTopBar) then LContext.NewTopBar := CreateContent(TopBarContentType, AContext);
  If assigned(LContext.OnCreateBottomBar) then LContext.NewBottomBar := CreateContent(BottomBarContentType, AContext);
  If assigned(LContext.OnCreatePullToRefreshIndicator) then LContext.NewPullToRefreshIndicator := CreateContent(PullToRefreshIndicatorContentType, AContext);
  If assigned(LContext.OnCreateLoadMoreIndicator) then LContext.NewLoadMoreIndicator := CreateContent(LoadMoreIndicatorContentType, AContext);
  If assigned(LContext.OnCreateLoadMoreRetryButton) then LContext.NewLoadMoreRetryButton := CreateContent(LoadMoreRetryButtonContentType, AContext);
end;

{**************************************************************************************************************************}
function TALDynamicListBox.TView.CreateDefaultMainContent(const AContext: TItem.TContentBuilderContext): TItem.TMainContent;
begin
  Result := TMainContent.Create(nil);
  Result.SetBoundsRect(AContext.TargetRect);
  Result.Padding.Rect := TALDynamicListBox.TView.TContentBuilderContext(AContext).Padding;
end;

{*******************************************************************************************}
procedure TALDynamicListBox.TView.ShiftContent(const AContext: TItem.TContentBuilderContext);
begin
  var LContext := TContentBuilderContext(AContext);
  if LContext.NewBackgroundContent <> nil then DoShiftContent(LContext.NewBackgroundContent, TItem.TContent(FBackgroundContent));
  if LContext.NewForegroundContent <> nil then DoShiftContent(LContext.NewForegroundContent, TItem.TContent(FForegroundContent));
  if LContext.NewTopBar <> nil then DoShiftContent(LContext.NewTopBar, TItem.TContent(FTopBar));
  if LContext.NewBottomBar <> nil then DoShiftContent(LContext.NewBottomBar, TItem.TContent(FBottomBar));
  if LContext.NewPullToRefreshIndicator <> nil then DoShiftContent(LContext.NewPullToRefreshIndicator, TItem.TContent(FPullToRefreshIndicator));
  if LContext.NewLoadMoreIndicator <> nil then DoShiftContent(LContext.NewLoadMoreIndicator, TItem.TContent(FLoadMoreIndicator));
  if LContext.NewLoadMoreRetryButton <> nil then DoShiftContent(LContext.NewLoadMoreRetryButton, TItem.TContent(FLoadMoreRetryButton));
  case AContext.FContentType of
    0{surrounding content}:;
    NoItemsContentType: if LContext.FNewContent <> nil then DoShiftContent(AContext.FNewContent, FNoItemsContent);
    MainContentType: begin
      Inherited;
      if (FMainContent <> nil) and
         (not assigned(TMainContent(FMainContent).OnRealign)) then
        TMainContent(FMainContent).OnRealign := OnRealignItems;
    end
    else inherited;
  end;
end;

{*****************************************************************************}
procedure TALDynamicListBox.TView.ActivateContent(const AContentType: Integer);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _HideContent(var AContent: TItem.TContent);
  begin
    if AContent.IsEphemeral then ALFreeAndNil(AContent, true{ADelayed})
    else AContent.Visible := False;
  end;

begin
  case AContentType of
    0{surrounding content}:;
    //--
    MainContentType: begin
      if (FMainContent = nil) or (FMainContent.Visible) then exit;
      // If LoadingContent is present, it will be responsible
      // for calling ActivateContent once CoreContent is ready.
      if (FLoadingContent <> nil) and (FLoadingContent.Visible) then exit;
      if FLoadingContent <> nil then _HideContent(FLoadingContent);
      if FErrorContent <> nil then _HideContent(FErrorContent);
      if FNoItemsContent <> nil then _HideContent(FNoItemsContent);
      FMainContent.Visible := true;
      if FBackgroundContent <> nil then FBackgroundContent.Visible := FBackgroundContent.ShowWithMainContent;
      if FForegroundContent <> nil then FForegroundContent.Visible := FForegroundContent.ShowWithMainContent;
      if FTopBar <> nil then FTopBar.Visible := FTopBar.ShowWithMainContent;
      if FBottomBar <> nil then FBottomBar.Visible := FBottomBar.ShowWithMainContent;
      if FPullToRefreshIndicator <> nil then FPullToRefreshIndicator.Visible := True;
      if FLoadMoreIndicator <> nil then FLoadMoreIndicator.Visible := (FPaginationToken <> '') and (FDownloadItemsErrorCode = 0);
      if FLoadMoreRetryButton <> nil then FLoadMoreRetryButton.Visible := (FPaginationToken <> '') and (FDownloadItemsErrorCode <> 0);
      SetViewportPosition(ViewportPosition);
      if assigned(FOnShowMainContent) then FOnShowMainContent(FMainContent);
    end;
    //--
    LoadingContentType: begin
      if (FLoadingContent = nil) or (FLoadingContent.Visible) then exit;
      if FMainContent <> nil then _HideContent(FMainContent);
      if FErrorContent <> nil then _HideContent(FErrorContent);
      if FNoItemsContent <> nil then _HideContent(FNoItemsContent);
      FLoadingContent.Visible := true;
      if FBackgroundContent <> nil then FBackgroundContent.Visible := FBackgroundContent.ShowWithLoadingContent;
      if FForegroundContent <> nil then FForegroundContent.Visible := FForegroundContent.ShowWithLoadingContent;
      if FTopBar <> nil then FTopBar.Visible := FTopBar.ShowWithLoadingContent;
      if FBottomBar <> nil then FBottomBar.Visible := FBottomBar.ShowWithLoadingContent;
      if FPullToRefreshIndicator <> nil then FPullToRefreshIndicator.Visible := False;
      if FLoadMoreIndicator <> nil then FLoadMoreIndicator.Visible := False;
      if FLoadMoreRetryButton <> nil then FLoadMoreRetryButton.Visible := False;
      if assigned(FOnShowLoadingContent) then FOnShowLoadingContent(FLoadingContent);
    end;
    //--
    ErrorContentType: begin
      if (FErrorContent = nil) or (FErrorContent.Visible) then exit;
      if FMainContent <> nil then _HideContent(FMainContent);
      if FLoadingContent <> nil then _HideContent(FLoadingContent);
      if FNoItemsContent <> nil then _HideContent(FNoItemsContent);
      FErrorContent.Visible := true;
      if FBackgroundContent <> nil then FBackgroundContent.Visible := FBackgroundContent.ShowWithErrorContent;
      if FForegroundContent <> nil then FForegroundContent.Visible := FForegroundContent.ShowWithErrorContent;
      if FTopBar <> nil then FTopBar.Visible := FTopBar.ShowWithErrorContent;
      if FBottomBar <> nil then FBottomBar.Visible := FBottomBar.ShowWithErrorContent;
      if FPullToRefreshIndicator <> nil then FPullToRefreshIndicator.Visible := False;
      if FLoadMoreIndicator <> nil then FLoadMoreIndicator.Visible := False;
      if FLoadMoreRetryButton <> nil then FLoadMoreRetryButton.Visible := False;
      if assigned(FOnShowErrorContent) then FOnShowErrorContent(FErrorContent);
    end;
    //--
    NoItemsContentType: begin
      if (FNoItemsContent = nil) or (FNoItemsContent.Visible) then exit;
      if FMainContent <> nil then _HideContent(FMainContent);
      if FLoadingContent <> nil then _HideContent(FLoadingContent);
      if FErrorContent <> nil then _HideContent(FErrorContent);
      FNoItemsContent.Visible := true;
      if FBackgroundContent <> nil then FBackgroundContent.Visible := FBackgroundContent.ShowWithNoItemsContent;
      if FForegroundContent <> nil then FForegroundContent.Visible := FForegroundContent.ShowWithNoItemsContent;
      if FTopBar <> nil then FTopBar.Visible := FTopBar.ShowWithNoItemsContent;
      if FBottomBar <> nil then FBottomBar.Visible := FBottomBar.ShowWithNoItemsContent;
      if FPullToRefreshIndicator <> nil then FPullToRefreshIndicator.Visible := False;
      if FLoadMoreIndicator <> nil then FLoadMoreIndicator.Visible := False;
      if FLoadMoreRetryButton <> nil then FLoadMoreRetryButton.Visible := False;
      if assigned(FOnShowNoItemsContent) then FOnShowNoItemsContent(FNoItemsContent);
    end;
    //--
    else
      Raise Exception.Create('Error 6224D91A-9694-48B5-88E5-6299E830EAE8')
  end;
  //BackgroundContent
  //MainContent
  //LoadingContent
  //ErrorContent
  //NoItemsContent
  //LoadMoreIndicator
  //LoadMoreRetryButton
  //ForegroundContent
  //TopBar
  //BottomBar
  //PullToRefreshIndicator
  var LIndex := -1;
  if (FBackgroundContent <> nil) then begin inc(LIndex); if FBackgroundContent.Index <> LIndex then MoveControl(FBackgroundContent,LIndex); end;
  if (FMainContent <> nil) then begin inc(LIndex); if FMainContent.Index <> LIndex then MoveControl(FMainContent,LIndex); end;
  if (FLoadingContent <> nil) then begin inc(LIndex); if FLoadingContent.Index <> LIndex then MoveControl(FLoadingContent,LIndex); end;
  if (FErrorContent <> nil) then begin inc(LIndex); if FErrorContent.Index <> LIndex then MoveControl(FErrorContent,LIndex); end;
  if (FNoItemsContent <> nil) then begin inc(LIndex); if FNoItemsContent.Index <> LIndex then MoveControl(FNoItemsContent,LIndex); end;
  if (FLoadMoreIndicator <> nil) then begin inc(LIndex); if FLoadMoreIndicator.Index <> LIndex then MoveControl(FLoadMoreIndicator,LIndex); end;
  if (FLoadMoreRetryButton <> nil) then begin inc(LIndex); if FLoadMoreRetryButton.Index <> LIndex then MoveControl(FLoadMoreRetryButton,LIndex); end;
  if (FForegroundContent <> nil) then begin inc(LIndex); if FForegroundContent.Index <> LIndex then MoveControl(FForegroundContent,LIndex); end;
  if (FPullToRefreshIndicator <> nil) then begin inc(LIndex); if FPullToRefreshIndicator.Index <> LIndex then MoveControl(FPullToRefreshIndicator,LIndex); end;
  if (FTopBar <> nil) then begin inc(LIndex); if FTopBar.Index <> LIndex then MoveControl(FTopBar,LIndex); end;
  if (FBottomBar <> nil) then begin inc(LIndex); if FBottomBar.Index <> LIndex then MoveControl(FBottomBar,LIndex); end;
end;

{**********************************************************************}
procedure TALDynamicListBox.TView.FetchContent(const APreload: boolean);
begin

  // If the item is outside the preloaded range, do nothing
  if (ParentView <> nil) and
     ((Index < ParentView.FirstPreloadedItemIndex) or
      (Index > ParentView.LastPreloadedItemIndex)) then exit;

  // If the item is not visible, do nothing
  if not Visible then exit;

  // If data must be downloaded first, fetch loading content
  if DownloadData then begin
    if APreload then TryPreloadContent(0{preload surrounding content})
    else TryCreateAndActivateContent(LoadingContentType);
  end

  // If the items have never been downloaded, download them
  // first and fetch the loading content
  else if (FMainContent = nil) and (DownloadItems) then begin
    if APreload then TryPreloadContent(0{preload surrounding content})
    else TryCreateAndActivateContent(LoadingContentType);
  end

  // If the items were never successfully downloaded and an
  // error occurred during the last download, fetch the error content
  else if (FMainContent = nil) and ((FDownloadDataErrorCode <> 0) or (FDownloadItemsErrorCode <> 0)) then begin
    if APreload then TryPreloadContent(ErrorContentType)
    else TryCreateAndActivateContent(ErrorContentType);
  end

  // fetch main content
  else begin
    if APreload then TryPreloadContent(0{preload surrounding content})
    else TryCreateAndActivateContent(0{create surrounding content});
  end;

end;

{*******************************************************}
constructor TALDynamicListBox.Create(aOwner: TComponent);
begin
  inherited create(AOwner);
  FRefreshTransitionKind := TRefreshTransitionKind.CrossFade;
  FPreloadItemCount := TView.DefaultPreloadItemCount;
  FActiveScrollEnginesCount := 0;
  FDisableMouseWheel := False;
  FHasBeenPrepared := False;
  FOnDownloadItems := nil;
  FOnDownloadItemData := nil;
  FOnCreateMainView := nil;
  FOnCreateLoadingContent := nil;
  FOnCreateErrorContent := nil;
  FOnCreateNoItemsContent := nil;
  FOnCreateBackgroundContent := nil;
  FOnCreateForegroundContent := nil;
  FOnCreateTopBar := nil;
  FOnCreateBottomBar := nil;
  FOnCreatePullToRefreshIndicator := nil;
  FOnCreateLoadMoreIndicator := nil;
  FOnCreateLoadMoreRetryButton := nil;
  FOnCreateItem := nil;
  FOnCreateItemMainContent := nil;
  FOnCreateItemLoadingContent := nil;
  FOnCreateItemErrorContent := nil;
  FOnShowLoadingContent := nil;
  FOnShowMainContent := nil;
  FOnShowErrorContent := nil;
  FOnShowNoItemsContent := nil;
  FOnRealignItems := nil;
  FOnViewportPositionChange := nil;
  FMainView := nil;
  FRefreshingView := nil;
end;

{***********************************}
destructor TALDynamicListBox.Destroy;
begin
  ALFreeAndNil(FMainView);
  inherited Destroy;
end;

{***********************************************}
function TALDynamicListBox.CreateMainView: TView;
begin
  If assigned(OnCreateMainView) then Result := OnCreateMainView(Self)
  else begin
    Result := TView.Create(nil);
    Result.RefreshTransitionKind := RefreshTransitionKind;
    Result.PreloadItemCount := PreloadItemCount;
    //OnDownloadData
    //OnCreateMainContent
    Result.OnCreateLoadingContent := OnCreateLoadingContent;
    Result.OnCreateErrorContent := OnCreateErrorContent;
    Result.OnDownloadItems := OnDownloadItems;
    Result.OnCreateItem := OnCreateItem;
    Result.OnCreateItemMainContent := OnCreateItemMainContent;
    Result.OnCreateItemLoadingContent := OnCreateItemLoadingContent;
    Result.OnCreateItemErrorContent := OnCreateItemErrorContent;
    Result.OnDownloadItemData := OnDownloadItemData;
    Result.OnCreateNoItemsContent := OnCreateNoItemsContent;
    Result.OnCreateBackgroundContent := OnCreateBackgroundContent;
    Result.OnCreateForegroundContent := OnCreateForegroundContent;
    Result.OnCreateTopBar := OnCreateTopBar;
    Result.OnCreateBottomBar := OnCreateBottomBar;
    Result.OnCreatePullToRefreshIndicator := OnCreatePullToRefreshIndicator;
    Result.OnCreateLoadMoreIndicator := OnCreateLoadMoreIndicator;
    Result.OnCreateLoadMoreRetryButton := OnCreateLoadMoreRetryButton;
    Result.OnShowLoadingContent := OnShowLoadingContent;
    Result.OnShowMainContent := OnShowMainContent;
    Result.OnShowErrorContent := OnShowErrorContent;
    Result.OnShowNoItemsContent := OnShowNoItemsContent;
    Result.OnRealignItems := OnRealignItems;
    Result.OnViewportPositionChange := OnViewportPositionChange;
  end;
  {$IF defined(DEBUG)}
  if (Result.FCacheEngine <> nil) and (Result.FCacheEngine <> CacheEngine) then
    raise Exception.Create('Error A1847154-423C-4B4E-A8B3-695BB8E8D992');
  {$ENDIF}
  Result.FCacheEngine := CacheEngine;
  Result.Align := TALAlignLayout.Client;
  Result.SetBounds(0,0,Width,Height);
end;

{**********************************************************}
procedure TALDynamicListBox.SetMainView(const Value: TView);
begin
  If Value <> FMainView then begin
    FRefreshingView := nil;
    if FMainView <> nil then begin
      FMainView.SetHost(nil);
      FMainView.ParentChanged;
      ALFreeAndNil(FMainView,true{ADelayed});
    end;
    FMainView := Value;
    FMainView.setHost(Self);
    FMainView.ParentChanged;
    Repaint;
  end;
end;

{**********************************}
procedure TALDynamicListBox.Prepare;
begin
  if not FHasBeenPrepared then begin
    {$IF defined(DEBUG)}
    If (MainView <> nil) then
      Raise Exception.Create('Error 5C7E8E31-687F-48D1-9902-3F4BB9DF002E');
    {$ENDIF}
    MainView := CreateMainView;
    MainView.Prepare;
    FHasBeenPrepared := True;
  end;
end;

{*****************************************************************************************************}
function TALDynamicListBox.ScrollToItemIndex(const AIndex: Integer; Const ADuration: integer): Boolean;
begin
  Result := MainView.ScrollToItemIndex(AIndex, ADuration);
end;

{********************************************************************************************}
function TALDynamicListBox.ScrollToItem(const AId: String; Const ADuration: integer): Boolean;
begin
  Result := MainView.ScrollToItem(AId, ADuration);
end;

{*******************************************************************************************}
function TALDynamicListBox.ScrollToItem(const AId: Int64; Const ADuration: integer): Boolean;
begin
  Result := MainView.ScrollToItem(AId, ADuration);
end;

{*****************************************}
function TALDynamicListBox.GetControlAtPos(
           const APos: TALPointD; // APos is local to the control
           out AControlPos: TALPointD; // AControlPos is local to the founded control
           const ACheckHitTest: Boolean = true): TALDynamicControl;
begin
  if MainView = nil then begin
    AControlPos := TALPointD.Zero;
    exit(nil);
  end;
  result := MainView.GetControlAtPos(MainView.AbsoluteToLocal(LocalToAbsolute(APos.ReducePrecision)), AControlPos, ACheckHitTest);
end;

{****************************************************************************************************}
procedure TALDynamicListBox.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if (FMainView <> nil) and (not FDisableMouseWheel) then
    FMainView.MouseWheel(Shift, WheelDelta, Handled);
  inherited;
end;

{************************************************************}
function TALDynamicListBox.GetHasActiveScrollEngines: Boolean;
begin
  Result := FActiveScrollEnginesCount > 0;
end;

{*************************************************************************}
procedure TALDynamicListBox.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  if MainView <> nil then
    MainView.AncestorVisibleChanged(Visible);
end;

{************************************}
procedure TALDynamicListBox.DoResized;
begin
  inherited;
  if MainView <> nil then
    MainView.SetBounds(0,0,Width,Height);
  CacheEngine.CLearEntries;
end;

{********************************}
procedure TALDynamicListBox.Paint;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _PaintRefreshingView;
  begin
    var LSavedMatrix := Canvas.Matrix;
    try
      var LMatrix := LSavedMatrix * TMatrix.CreateTranslation(FRefreshingView.left,FRefreshingView.Top);
      Canvas.SetMatrix(LMatrix);
      FRefreshingView.PaintInternal(Canvas);
    finally
      Canvas.SetMatrix(LSavedMatrix);
    end;
  end;

begin
  if (TALGuardianThread.HasInstance) and (HasActiveScrollEngines) then
    TALGuardianThread.Instance.CanExecute := False;
  //--
  inherited;
  //--
  Prepare;
  //--
  if (FRefreshingView <> nil) and (FRefreshTransitionKind = TRefreshTransitionKind.RevealFadeOut) then
    _PaintRefreshingView;
  //--
  if MainView = nil then
    Raise Exception.Create('Error EE6AB6E3-5988-4325-9EA4-92AC8B0D7C28');
  var LSavedMatrix := Canvas.Matrix;
  try
    var LMatrix := LSavedMatrix * TMatrix.CreateTranslation(MainView.left,MainView.Top);
    Canvas.SetMatrix(LMatrix);
    MainView.PaintInternal(Canvas);
  finally
    Canvas.SetMatrix(LSavedMatrix);
  end;
  //--
  if (FRefreshingView <> nil) and (FRefreshTransitionKind <> TRefreshTransitionKind.RevealFadeOut) then
    _PaintRefreshingView;
  //--
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{****************************************************************************}
procedure TALDynamicListBox.ShowErrorMessageBanner(const AErrorCode: Integer);
begin

end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALDynamicListBox]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALDynamicListBox, 'Size');
  UnlistPublishedProperty(TALDynamicListBox, 'StyleName');
  UnlistPublishedProperty(TALDynamicListBox, 'OnTap');
  {$ENDIF}
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Dynamic.ListBox','initialization');
  {$ENDIF}
  RegisterFmxClasses([TALDynamicListBox]);
  {$IFDEF DEBUG}
  {$If defined(Android)}
  //https://developer.android.com/media/optimize/performance/frame-rate
  TALDynamicListBox.TView.DisplayDefaultRefreshRate := TAndroidHelper.Display.getRefreshRate;
  {$ELSEIf defined(IOS)}
  TALDynamicListBox.TView.DisplayDefaultRefreshRate := TALUIScreen.Wrap(NSObjectToID(TiOSHelper.MainScreen)).maximumFramesPerSecond;
  {$ELSE}
  TALDynamicListBox.TView.DisplayDefaultRefreshRate := 60
  {$ENDIF}
  {$ENDIF}

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Dynamic.ListBox','finalization');
  {$ENDIF}

end.
