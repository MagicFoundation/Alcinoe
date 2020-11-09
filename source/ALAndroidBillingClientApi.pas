unit ALAndroidBillingClientApi;

interface

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App;

type

  {*************************}
  JBillingClient = interface;
  JBillingClient_Builder = interface;
  JPurchasesUpdatedListener = interface;
  JBillingClientStateListener = interface;
  JBillingClient_BillingResponseCode = interface;
  JBillingClient_SkuType = interface;
  JBillingClient_FeatureType = interface;
  JBillingFlowParams = interface;
  JBillingFlowParams_Builder = interface;
  JSkuDetails = interface;
  JSkuDetailsParams = interface;
  JSkuDetailsParams_Builder = interface;
  JSkuDetailsResponseListener = interface;
  JBillingResult = interface;
  JBillingResult_Builder = interface;
  JPurchase = interface;
  JPurchase_PurchaseState = interface;
  JAcknowledgePurchaseParams = interface;
  JAcknowledgePurchaseParams_Builder = interface;
  JAcknowledgePurchaseResponseListener = interface;
  JPurchase_PurchasesResult = interface;
  JAccountIdentifiers = interface;

  {*******************************************}
  JBillingClientClass = interface(JObjectClass)
    ['{712170FB-20E3-4A50-86DF-5B012781427B}']
    {class} function init: JBillingClient; cdecl;
    {class} function newBuilder(context: JContext): JBillingClient_Builder; cdecl;
  end;
  [JavaSignature('com/android/billingclient/api/BillingClient')]
  JBillingClient = interface(JObject)
    ['{6730B6D8-9EC6-4754-BD81-EC63724F9A05}']
    procedure acknowledgePurchase(params: JAcknowledgePurchaseParams; listener: JAcknowledgePurchaseResponseListener); cdecl;
    //procedure consumeAsync(consumeParams: JConsumeParams; listener: JConsumeResponseListener); cdecl;
    procedure endConnection; cdecl;
    function isFeatureSupported(feature: JString): JBillingResult; cdecl;
    function isReady: Boolean; cdecl;
    function launchBillingFlow(activity: JActivity; params: JBillingFlowParams): JBillingResult; cdecl;
    //procedure launchPriceChangeConfirmationFlow(activity: JActivity; params: JPriceChangeFlowParams; listener: JPriceChangeConfirmationListener); cdecl;
    //procedure queryPurchaseHistoryAsync(skuType: JString; listener: JPurchaseHistoryResponseListener); cdecl;
    function queryPurchases(skuType: JString): JPurchase_PurchasesResult; cdecl;
    procedure querySkuDetailsAsync(params: JSkuDetailsParams; listener: JSkuDetailsResponseListener); cdecl;
    procedure startConnection(listener: JBillingClientStateListener); cdecl;
  end;
  TJBillingClient = class(TJavaGenericImport<JBillingClientClass, JBillingClient>) end;

  {***************************************************}
  JBillingClient_BuilderClass = interface(JObjectClass)
    ['{ECC0E3EA-850C-4099-8DC0-73FB403263A2}']
  end;
  [JavaSignature('com/android/billingclient/api/BillingClient$Builder')]
  JBillingClient_Builder = interface(JObject)
    ['{51383F43-2FFF-41B4-A6B8-59FFF94DBA40}']
    function build: JBillingClient; cdecl;
    function enablePendingPurchases: JBillingClient_Builder; cdecl;
    function setListener(listener: JPurchasesUpdatedListener): JBillingClient_Builder; cdecl;
  end;
  TJBillingClient_Builder = class(TJavaGenericImport<JBillingClient_BuilderClass, JBillingClient_Builder>) end;

  {****************************************************}
  JPurchasesUpdatedListenerClass = interface(IJavaClass)
    ['{C6526317-548F-4929-AF83-5789A4F2D687}']
  end;
  [JavaSignature('com/android/billingclient/api/PurchasesUpdatedListener')]
  JPurchasesUpdatedListener = interface(IJavaInstance)
    ['{DD72D6E1-F387-4C52-939C-9A6F759FF193}']
    procedure onPurchasesUpdated(billingResult: JBillingResult; purchases: JList); cdecl;
  end;
  TJPurchasesUpdatedListener = class(TJavaGenericImport<JPurchasesUpdatedListenerClass, JPurchasesUpdatedListener>) end;

  {******************************************************}
  JBillingClientStateListenerClass = interface(IJavaClass)
    ['{4694464C-600E-4652-BC51-CA7B08BB57C4}']
  end;
  [JavaSignature('com/android/billingclient/api/BillingClientStateListener')]
  JBillingClientStateListener = interface(IJavaInstance)
    ['{7BDF919C-4D52-4AF6-99D4-A2598CAFDAEE}']
    procedure onBillingServiceDisconnected; cdecl;
    procedure onBillingSetupFinished(billingResult: JBillingResult); cdecl;
  end;
  TJBillingClientStateListener = class(TJavaGenericImport<JBillingClientStateListenerClass, JBillingClientStateListener>) end;

  {*******************************************************************}
  JBillingClient_BillingResponseCodeClass = interface(JAnnotationClass)
    ['{AF6E5AA9-2437-48BD-BCAC-CA0544B4336F}']
    {class} function _GetBILLING_UNAVAILABLE: Integer; cdecl;
    {class} function _GetDEVELOPER_ERROR: Integer; cdecl;
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetFEATURE_NOT_SUPPORTED: Integer; cdecl;
    {class} function _GetITEM_ALREADY_OWNED: Integer; cdecl;
    {class} function _GetITEM_NOT_OWNED: Integer; cdecl;
    {class} function _GetITEM_UNAVAILABLE: Integer; cdecl;
    {class} function _GetOK: Integer; cdecl;
    {class} function _GetSERVICE_DISCONNECTED: Integer; cdecl;
    {class} function _GetSERVICE_TIMEOUT: Integer; cdecl;
    {class} function _GetSERVICE_UNAVAILABLE: Integer; cdecl;
    {class} function _GetUSER_CANCELED: Integer; cdecl;
    {class} property BILLING_UNAVAILABLE: Integer read _GetBILLING_UNAVAILABLE; // 3
    {class} property DEVELOPER_ERROR: Integer read _GetDEVELOPER_ERROR; // 5
    {class} property ERROR: Integer read _GetERROR; // 6
    {class} property FEATURE_NOT_SUPPORTED: Integer read _GetFEATURE_NOT_SUPPORTED; // -2
    {class} property ITEM_ALREADY_OWNED: Integer read _GetITEM_ALREADY_OWNED; // 7
    {class} property ITEM_NOT_OWNED: Integer read _GetITEM_NOT_OWNED; // 8
    {class} property ITEM_UNAVAILABLE: Integer read _GetITEM_UNAVAILABLE; // 4
    {class} property OK: Integer read _GetOK; // 0
    {class} property SERVICE_DISCONNECTED: Integer read _GetSERVICE_DISCONNECTED; // -1
    {class} property SERVICE_TIMEOUT: Integer read _GetSERVICE_TIMEOUT; // -3
    {class} property SERVICE_UNAVAILABLE: Integer read _GetSERVICE_UNAVAILABLE; // 2
    {class} property USER_CANCELED: Integer read _GetUSER_CANCELED; // 1
  end;
  [JavaSignature('com/android/billingclient/api/BillingClient$BillingResponseCode')]
  JBillingClient_BillingResponseCode = interface(JAnnotation)
    ['{B267B336-25F2-4699-936B-E83121891349}']
  end;
  TJBillingClient_BillingResponseCode = class(TJavaGenericImport<JBillingClient_BillingResponseCodeClass, JBillingClient_BillingResponseCode>) end;

  {*******************************************************}
  JBillingClient_SkuTypeClass = interface(JAnnotationClass)
    ['{2DA268AB-449C-4FA2-B6F6-04E89CE038DE}']
    {class} function _GetINAPP: JString; cdecl;
    {class} function _GetSUBS: JString; cdecl;
    {class} property INAPP: JString read _GetINAPP;
    {class} property SUBS: JString read _GetSUBS;
  end;
  [JavaSignature('com/android/billingclient/api/BillingClient$SkuType')]
  JBillingClient_SkuType = interface(JAnnotation)
    ['{2A9EAA15-EBA9-477D-BE1B-0786B1B5418D}']
  end;
  TJBillingClient_SkuType = class(TJavaGenericImport<JBillingClient_SkuTypeClass, JBillingClient_SkuType>) end;

  {***********************************************************}
  JBillingClient_FeatureTypeClass = interface(JAnnotationClass)
    ['{89501AAC-D095-4906-B8F7-6C8825B146FC}']
    {class} function _GetIN_APP_ITEMS_ON_VR: JString; cdecl;
    {class} function _GetPRICE_CHANGE_CONFIRMATION: JString; cdecl;
    {class} function _GetSUBSCRIPTIONS: JString; cdecl;
    {class} function _GetSUBSCRIPTIONS_ON_VR: JString; cdecl;
    {class} function _GetSUBSCRIPTIONS_UPDATE: JString; cdecl;
    {class} property IN_APP_ITEMS_ON_VR: JString read _GetIN_APP_ITEMS_ON_VR;
    {class} property PRICE_CHANGE_CONFIRMATION: JString read _GetPRICE_CHANGE_CONFIRMATION;
    {class} property SUBSCRIPTIONS: JString read _GetSUBSCRIPTIONS;
    {class} property SUBSCRIPTIONS_ON_VR: JString read _GetSUBSCRIPTIONS_ON_VR;
    {class} property SUBSCRIPTIONS_UPDATE: JString read _GetSUBSCRIPTIONS_UPDATE;
  end;
  [JavaSignature('com/android/billingclient/api/BillingClient$FeatureType')]
  JBillingClient_FeatureType = interface(JAnnotation)
    ['{05459874-73A3-4153-BAB0-851DA54D6B57}']
  end;
  TJBillingClient_FeatureType = class(TJavaGenericImport<JBillingClient_FeatureTypeClass, JBillingClient_FeatureType>) end;

  {***********************************************}
  JBillingFlowParamsClass = interface(JObjectClass)
    ['{8936C06D-F591-40E3-9063-A8CEAD30CBD8}']
    {class} function _GetEXTRA_PARAM_KEY_ACCOUNT_ID: JString; cdecl;
    {class} function _GetEXTRA_PARAM_KEY_OLD_SKUS: JString; cdecl;
    {class} function _GetEXTRA_PARAM_KEY_OLD_SKU_PURCHASE_TOKEN: JString; cdecl;
    {class} function _GetEXTRA_PARAM_KEY_REPLACE_SKUS_PRORATION_MODE: JString; cdecl;
    {class} function _GetEXTRA_PARAM_KEY_VR: JString; cdecl;
    {class} function newBuilder: JBillingFlowParams_Builder; cdecl;
    {class} property EXTRA_PARAM_KEY_ACCOUNT_ID: JString read _GetEXTRA_PARAM_KEY_ACCOUNT_ID;
    {class} property EXTRA_PARAM_KEY_OLD_SKUS: JString read _GetEXTRA_PARAM_KEY_OLD_SKUS;
    {class} property EXTRA_PARAM_KEY_OLD_SKU_PURCHASE_TOKEN: JString read _GetEXTRA_PARAM_KEY_OLD_SKU_PURCHASE_TOKEN;
    {class} property EXTRA_PARAM_KEY_REPLACE_SKUS_PRORATION_MODE: JString read _GetEXTRA_PARAM_KEY_REPLACE_SKUS_PRORATION_MODE;
    {class} property EXTRA_PARAM_KEY_VR: JString read _GetEXTRA_PARAM_KEY_VR;
  end;
  [JavaSignature('com/android/billingclient/api/BillingFlowParams')]
  JBillingFlowParams = interface(JObject)
    ['{BFB32B82-E7B4-4A94-98EE-06CD7DA28F87}']
    function getOldSku: JString; cdecl;
    function getOldSkuPurchaseToken: JString; cdecl;
    function getReplaceSkusProrationMode: Integer; cdecl;
    function getSku: JString; cdecl;
    function getSkuDetails: JSkuDetails; cdecl;
    function getSkuType: JString; cdecl;
    function getVrPurchaseFlow: Boolean; cdecl;
  end;
  TJBillingFlowParams = class(TJavaGenericImport<JBillingFlowParamsClass, JBillingFlowParams>) end;

  {*******************************************************}
  JBillingFlowParams_BuilderClass = interface(JObjectClass)
    ['{51A887BE-1DB1-4718-91D3-D58E208B52B3}']
  end;
  [JavaSignature('com/android/billingclient/api/BillingFlowParams$Builder')]
  JBillingFlowParams_Builder = interface(JObject)
    ['{C8EC7F4E-17D2-4A82-BFE7-219D542C022C}']
    function build: JBillingFlowParams; cdecl;
    function setObfuscatedAccountId(obfuscatedAccountid: JString): JBillingFlowParams_Builder; cdecl;
    function setObfuscatedProfileId(obfuscatedProfileId: JString): JBillingFlowParams_Builder; cdecl;
    function setOldSku(oldSku: JString; purchaseToken: JString): JBillingFlowParams_Builder; cdecl;
    function setReplaceSkusProrationMode(replaceSkusProrationMode: Integer): JBillingFlowParams_Builder; cdecl;
    function setSkuDetails(skuDetails: JSkuDetails): JBillingFlowParams_Builder; cdecl;
    function setVrPurchaseFlow(isVrPurchaseFlow: Boolean): JBillingFlowParams_Builder; cdecl;
  end;
  TJBillingFlowParams_Builder = class(TJavaGenericImport<JBillingFlowParams_BuilderClass, JBillingFlowParams_Builder>) end;

  {****************************************}
  JSkuDetailsClass = interface(JObjectClass)
    ['{567B43AC-22A1-4C4E-B851-7DC78A6BF15F}']
    {class} function init(jsonSkuDetails: JString): JSkuDetails; cdecl;
  end;
  [JavaSignature('com/android/billingclient/api/SkuDetails')]
  JSkuDetails = interface(JObject)
    ['{A1773629-CCD1-4022-AE84-BFC8CC4B0F67}']
    function equals(o: JObject): Boolean; cdecl;
    function getDescription: JString; cdecl;
    function getFreeTrialPeriod: JString; cdecl;
    function getIconUrl: JString; cdecl;
    function getIntroductoryPrice: JString; cdecl;
    function getIntroductoryPriceAmountMicros: Int64; cdecl;
    function getIntroductoryPriceCycles: Integer; cdecl;
    function getIntroductoryPricePeriod: JString; cdecl;
    function getOriginalJson: JString; cdecl;
    function getOriginalPrice: JString; cdecl;
    function getOriginalPriceAmountMicros: Int64; cdecl;
    function getPrice: JString; cdecl;
    function getPriceAmountMicros: Int64; cdecl;
    function getPriceCurrencyCode: JString; cdecl;
    function getSku: JString; cdecl;
    function getSubscriptionPeriod: JString; cdecl;
    function getTitle: JString; cdecl;
    function getType: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJSkuDetails = class(TJavaGenericImport<JSkuDetailsClass, JSkuDetails>) end;

  {**********************************************}
  JSkuDetailsParamsClass = interface(JObjectClass)
    ['{9C5A4D15-F9B5-48C6-9E08-135862DE929C}']
    {class} function init: JSkuDetailsParams; cdecl;
    {class} function newBuilder: JSkuDetailsParams_Builder; cdecl;
  end;
  [JavaSignature('com/android/billingclient/api/SkuDetailsParams')]
  JSkuDetailsParams = interface(JObject)
    ['{B3C2D965-96AB-41E3-B166-E91238563E7A}']
    function getSkuType: JString; cdecl;
    function getSkusList: JList; cdecl;
  end;
  TJSkuDetailsParams = class(TJavaGenericImport<JSkuDetailsParamsClass, JSkuDetailsParams>) end;

  {******************************************************}
  JSkuDetailsParams_BuilderClass = interface(JObjectClass)
    ['{C6B7FB49-C2D8-40EF-B573-ED5063B63AA6}']
  end;
  [JavaSignature('com/android/billingclient/api/SkuDetailsParams$Builder')]
  JSkuDetailsParams_Builder = interface(JObject)
    ['{9C922B57-007B-4922-B3C2-C6EDAD62DDB6}']
    function build: JSkuDetailsParams; cdecl;
    function setSkusList(skusList: JList): JSkuDetailsParams_Builder; cdecl;
    function setType(&type: JString): JSkuDetailsParams_Builder; cdecl;
  end;
  TJSkuDetailsParams_Builder = class(TJavaGenericImport<JSkuDetailsParams_BuilderClass, JSkuDetailsParams_Builder>) end;

  {******************************************************}
  JSkuDetailsResponseListenerClass = interface(IJavaClass)
    ['{F98B8535-A64D-4702-B416-6329FC99CB4A}']
  end;
  [JavaSignature('com/android/billingclient/api/SkuDetailsResponseListener')]
  JSkuDetailsResponseListener = interface(IJavaInstance)
    ['{0ABAF8ED-9EE8-468F-9C55-6EAE63C6A93F}']
    procedure onSkuDetailsResponse(billingResult: JBillingResult; skuDetailsList: JList); cdecl;
  end;
  TJSkuDetailsResponseListener = class(TJavaGenericImport<JSkuDetailsResponseListenerClass, JSkuDetailsResponseListener>) end;

  {*******************************************}
  JBillingResultClass = interface(JObjectClass)
    ['{E393EA90-204C-489D-A39D-18C16559B14A}']
    {class} function init: JBillingResult; cdecl;
    {class} function newBuilder: JBillingResult_Builder; cdecl;
  end;
  [JavaSignature('com/android/billingclient/api/BillingResult')]
  JBillingResult = interface(JObject)
    ['{27FB411C-A7F9-4E54-A4D2-E6F786C9F9E0}']
    function getDebugMessage: JString; cdecl;
    function getResponseCode: Integer; cdecl;
  end;
  TJBillingResult = class(TJavaGenericImport<JBillingResultClass, JBillingResult>) end;

  {***************************************************}
  JBillingResult_BuilderClass = interface(JObjectClass)
    ['{6600D57E-1121-4B45-8D67-2CCF82C1A3B5}']
  end;
  [JavaSignature('com/android/billingclient/api/BillingResult$Builder')]
  JBillingResult_Builder = interface(JObject)
    ['{E5D3A63D-DC2C-408C-A84B-1F6BBEA72DC4}']
    function build: JBillingResult; cdecl;
    function setDebugMessage(debugMessage: JString): JBillingResult_Builder; cdecl;
    function setResponseCode(responseCode: Integer): JBillingResult_Builder; cdecl;
  end;
  TJBillingResult_Builder = class(TJavaGenericImport<JBillingResult_BuilderClass, JBillingResult_Builder>) end;

  {**************************************}
  JPurchaseClass = interface(JObjectClass)
    ['{D3E3AE57-5506-462B-906F-155A7868DF6E}']
    function init(jsonPurchaseInfo: JString; signature: JString): JPurchase; cdecl;
  end;
  [JavaSignature('com/android/billingclient/api/Purchase')]
  JPurchase = interface(JObject)
    ['{111B4602-BE74-4C59-B63A-FB6AE4ED1114}']
    function equals(o: JObject): Boolean; cdecl;
    function getAccountIdentifiers: JAccountIdentifiers; cdecl;
    function getDeveloperPayload: JString; cdecl;
    function getOrderId: JString; cdecl;
    function getOriginalJson: JString; cdecl;
    function getPackageName: JString; cdecl;
    function getPurchaseState: Integer; cdecl;
    function getPurchaseTime: Int64; cdecl;
    function getPurchaseToken: JString; cdecl;
    function getSignature: JString; cdecl;
    function getSku: JString; cdecl;
    function hashCode: Integer; cdecl;
    function isAcknowledged: Boolean; cdecl;
    function isAutoRenewing: Boolean; cdecl;
    function toString: JString; cdecl;
  end;
  TJPurchase = class(TJavaGenericImport<JPurchaseClass, JPurchase>) end;

  {********************************************************}
  JPurchase_PurchaseStateClass = interface(JAnnotationClass)
    ['{2DBAF561-5EEA-4026-B052-A04936349B21}']
    {class} function _GetPENDING: Integer; cdecl;
    {class} function _GetPURCHASED: Integer; cdecl;
    {class} function _GetUNSPECIFIED_STATE: Integer; cdecl;
    {class} property PENDING: Integer read _GetPENDING;
    {class} property PURCHASED: Integer read _GetPURCHASED;
    {class} property UNSPECIFIED_STATE: Integer read _GetUNSPECIFIED_STATE;
  end;
  [JavaSignature('com/android/billingclient/api/Purchase$PurchaseState')]
  JPurchase_PurchaseState = interface(JAnnotation)
    ['{8449E34F-559A-428D-AD7C-5DF0B988FDF8}']
  end;
  TJPurchase_PurchaseState = class(TJavaGenericImport<JPurchase_PurchaseStateClass, JPurchase_PurchaseState>) end;

  {************************************************}
  JAccountIdentifiersClass = interface(JObjectClass)
    ['{E667E593-17A5-4C97-B478-ECC9BEB176F2}']
  end;
  [JavaSignature('com/android/billingclient/api/AccountIdentifiers')]
  JAccountIdentifiers = interface(JObject)
    ['{3196ED73-3FFE-4CE0-8D97-CD11C7C10241}']
    function getObfuscatedAccountId: JString; cdecl;
    function getObfuscatedProfileId: JString; cdecl;
  end;
  TJAccountIdentifiers = class(TJavaGenericImport<JAccountIdentifiersClass, JAccountIdentifiers>) end;

  {*******************************************************}
  JAcknowledgePurchaseParamsClass = interface(JObjectClass)
    ['{2185AD81-96BC-471A-BAE0-A33996F685A8}']
    {class} function newBuilder: JAcknowledgePurchaseParams_Builder; cdecl;
  end;
  [JavaSignature('com/android/billingclient/api/AcknowledgePurchaseParams')]
  JAcknowledgePurchaseParams = interface(JObject)
    ['{AA475126-D182-431D-A041-232001EC1081}']
    function getPurchaseToken: JString; cdecl;
  end;
  TJAcknowledgePurchaseParams = class(TJavaGenericImport<JAcknowledgePurchaseParamsClass, JAcknowledgePurchaseParams>) end;

  {***************************************************************}
  JAcknowledgePurchaseParams_BuilderClass = interface(JObjectClass)
    ['{9F7597AA-0479-48CF-8472-989A15FB103A}']
  end;
  [JavaSignature('com/android/billingclient/api/AcknowledgePurchaseParams$Builder')]
  JAcknowledgePurchaseParams_Builder = interface(JObject)
    ['{70B65A59-FF42-4FB9-A092-6710BCB71A02}']
    function build: JAcknowledgePurchaseParams; cdecl;
    function setPurchaseToken(purchaseToken: JString): JAcknowledgePurchaseParams_Builder; cdecl;
  end;
  TJAcknowledgePurchaseParams_Builder = class(TJavaGenericImport<JAcknowledgePurchaseParams_BuilderClass, JAcknowledgePurchaseParams_Builder>) end;

  {***************************************************************}
  JAcknowledgePurchaseResponseListenerClass = interface(IJavaClass)
    ['{1C7DA2B0-C721-4953-91F2-F4620E2647A2}']
  end;
  [JavaSignature('com/android/billingclient/api/AcknowledgePurchaseResponseListener')]
  JAcknowledgePurchaseResponseListener = interface(IJavaInstance)
    ['{53864E86-098B-4B74-9E38-E6FF4557D660}']
    procedure onAcknowledgePurchaseResponse(billingResult: JBillingResult); cdecl;
  end;
  TJAcknowledgePurchaseResponseListener = class(TJavaGenericImport<JAcknowledgePurchaseResponseListenerClass, JAcknowledgePurchaseResponseListener>) end;

  {******************************************************}
  JPurchase_PurchasesResultClass = interface(JObjectClass)
    ['{951390BE-A395-42E2-B5E3-26A77B439391}']
    {class} function init(mBillingResult: JBillingResult; purchasesList: JList): JPurchase_PurchasesResult; cdecl;
  end;
  [JavaSignature('com/android/billingclient/api/Purchase$PurchasesResult')]
  JPurchase_PurchasesResult = interface(JObject)
    ['{0A18D9D8-1A76-4F32-BD60-13D2883E20F2}']
    function getBillingResult: JBillingResult; cdecl;
    function getPurchasesList: JList; cdecl;
    function getResponseCode: Integer; cdecl;
  end;
  TJPurchase_PurchasesResult = class(TJavaGenericImport<JPurchase_PurchasesResultClass, JPurchase_PurchasesResult>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JBillingClient', TypeInfo(ALAndroidBillingClientApi.JBillingClient));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JBillingClient_Builder', TypeInfo(ALAndroidBillingClientApi.JBillingClient_Builder));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JPurchasesUpdatedListener', TypeInfo(ALAndroidBillingClientApi.JPurchasesUpdatedListener));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JBillingClientStateListener', TypeInfo(ALAndroidBillingClientApi.JBillingClientStateListener));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JBillingClient_BillingResponseCode', TypeInfo(ALAndroidBillingClientApi.JBillingClient_BillingResponseCode));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JBillingClient_SkuType', TypeInfo(ALAndroidBillingClientApi.JBillingClient_SkuType));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JBillingClient_FeatureType', TypeInfo(ALAndroidBillingClientApi.JBillingClient_FeatureType));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JBillingFlowParams', TypeInfo(ALAndroidBillingClientApi.JBillingFlowParams));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JBillingFlowParams_Builder', TypeInfo(ALAndroidBillingClientApi.JBillingFlowParams_Builder));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JSkuDetails', TypeInfo(ALAndroidBillingClientApi.JSkuDetails));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JSkuDetailsParams', TypeInfo(ALAndroidBillingClientApi.JSkuDetailsParams));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JSkuDetailsParams_Builder', TypeInfo(ALAndroidBillingClientApi.JSkuDetailsParams_Builder));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JSkuDetailsResponseListener', TypeInfo(ALAndroidBillingClientApi.JSkuDetailsResponseListener));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JBillingResult', TypeInfo(ALAndroidBillingClientApi.JBillingResult));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JBillingResult_Builder', TypeInfo(ALAndroidBillingClientApi.JBillingResult_Builder));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JPurchase', TypeInfo(ALAndroidBillingClientApi.JPurchase));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JPurchase_PurchaseState', TypeInfo(ALAndroidBillingClientApi.JPurchase_PurchaseState));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JAcknowledgePurchaseParams', TypeInfo(ALAndroidBillingClientApi.JAcknowledgePurchaseParams));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JAcknowledgePurchaseParams_Builder', TypeInfo(ALAndroidBillingClientApi.JAcknowledgePurchaseParams_Builder));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JAcknowledgePurchaseResponseListener', TypeInfo(ALAndroidBillingClientApi.JAcknowledgePurchaseResponseListener));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JPurchase_PurchasesResult', TypeInfo(ALAndroidBillingClientApi.JPurchase_PurchasesResult));
  TRegTypes.RegisterType('ALAndroidBillingClientApi.JAccountIdentifiers', TypeInfo(ALAndroidBillingClientApi.JAccountIdentifiers));
end;

initialization
  RegisterTypes;

end.
