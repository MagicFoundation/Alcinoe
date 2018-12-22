{**********************************************************}
{                                                          }
{             CodeGear Delphi Runtime Library              }
{                                                          }
{   Copyright(c) 2012-2017 Embarcadero Technologies, Inc.   }
{              All rights reserved                         }
{                                                          }
{**********************************************************}

//
// Delphi-Objective-C Bridge
// Interfaces for Cocoa framework Security
//

unit iOSapi.Security;

interface

uses
  iOSapi.CocoaTypes, iOSapi.Foundation, Macapi.CoreFoundation, Macapi.Dispatch;

const
  SSL_NULL_WITH_NULL_NULL = 0;
  SSL_RSA_WITH_NULL_MD5 = 1;
  SSL_RSA_WITH_NULL_SHA = 2;
  SSL_RSA_EXPORT_WITH_RC4_40_MD5 = 3;
  SSL_RSA_WITH_RC4_128_MD5 = 4;
  SSL_RSA_WITH_RC4_128_SHA = 5;
  SSL_RSA_EXPORT_WITH_RC2_CBC_40_MD5 = 6;
  SSL_RSA_WITH_IDEA_CBC_SHA = 7;
  SSL_RSA_EXPORT_WITH_DES40_CBC_SHA = 8;
  SSL_RSA_WITH_DES_CBC_SHA = 9;
  SSL_RSA_WITH_3DES_EDE_CBC_SHA = 10;
  SSL_DH_DSS_EXPORT_WITH_DES40_CBC_SHA = 11;
  SSL_DH_DSS_WITH_DES_CBC_SHA = 12;
  SSL_DH_DSS_WITH_3DES_EDE_CBC_SHA = 13;
  SSL_DH_RSA_EXPORT_WITH_DES40_CBC_SHA = 14;
  SSL_DH_RSA_WITH_DES_CBC_SHA = 15;
  SSL_DH_RSA_WITH_3DES_EDE_CBC_SHA = 16;
  SSL_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA = 17;
  SSL_DHE_DSS_WITH_DES_CBC_SHA = 18;
  SSL_DHE_DSS_WITH_3DES_EDE_CBC_SHA = 19;
  SSL_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA = 20;
  SSL_DHE_RSA_WITH_DES_CBC_SHA = 21;
  SSL_DHE_RSA_WITH_3DES_EDE_CBC_SHA = 22;
  SSL_DH_anon_EXPORT_WITH_RC4_40_MD5 = 23;
  SSL_DH_anon_WITH_RC4_128_MD5 = 24;
  SSL_DH_anon_EXPORT_WITH_DES40_CBC_SHA = 25;
  SSL_DH_anon_WITH_DES_CBC_SHA = 26;
  SSL_DH_anon_WITH_3DES_EDE_CBC_SHA = 27;
  SSL_FORTEZZA_DMS_WITH_NULL_SHA = 28;
  SSL_FORTEZZA_DMS_WITH_FORTEZZA_CBC_SHA = 29;
  TLS_RSA_WITH_AES_128_CBC_SHA = 47;
  TLS_DH_DSS_WITH_AES_128_CBC_SHA = 48;
  TLS_DH_RSA_WITH_AES_128_CBC_SHA = 49;
  TLS_DHE_DSS_WITH_AES_128_CBC_SHA = 50;
  TLS_DHE_RSA_WITH_AES_128_CBC_SHA = 51;
  TLS_DH_anon_WITH_AES_128_CBC_SHA = 52;
  TLS_RSA_WITH_AES_256_CBC_SHA = 53;
  TLS_DH_DSS_WITH_AES_256_CBC_SHA = 54;
  TLS_DH_RSA_WITH_AES_256_CBC_SHA = 55;
  TLS_DHE_DSS_WITH_AES_256_CBC_SHA = 56;
  TLS_DHE_RSA_WITH_AES_256_CBC_SHA = 57;
  TLS_DH_anon_WITH_AES_256_CBC_SHA = 58;
  TLS_ECDH_ECDSA_WITH_NULL_SHA = 49153;
  TLS_ECDH_ECDSA_WITH_RC4_128_SHA = 49154;
  TLS_ECDH_ECDSA_WITH_3DES_EDE_CBC_SHA = 49155;
  TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA = 49156;
  TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA = 49157;
  TLS_ECDHE_ECDSA_WITH_NULL_SHA = 49158;
  TLS_ECDHE_ECDSA_WITH_RC4_128_SHA = 49159;
  TLS_ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA = 49160;
  TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA = 49161;
  TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA = 49162;
  TLS_ECDH_RSA_WITH_NULL_SHA = 49163;
  TLS_ECDH_RSA_WITH_RC4_128_SHA = 49164;
  TLS_ECDH_RSA_WITH_3DES_EDE_CBC_SHA = 49165;
  TLS_ECDH_RSA_WITH_AES_128_CBC_SHA = 49166;
  TLS_ECDH_RSA_WITH_AES_256_CBC_SHA = 49167;
  TLS_ECDHE_RSA_WITH_NULL_SHA = 49168;
  TLS_ECDHE_RSA_WITH_RC4_128_SHA = 49169;
  TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA = 49170;
  TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA = 49171;
  TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA = 49172;
  TLS_ECDH_anon_WITH_NULL_SHA = 49173;
  TLS_ECDH_anon_WITH_RC4_128_SHA = 49174;
  TLS_ECDH_anon_WITH_3DES_EDE_CBC_SHA = 49175;
  TLS_ECDH_anon_WITH_AES_128_CBC_SHA = 49176;
  TLS_ECDH_anon_WITH_AES_256_CBC_SHA = 49177;
  TLS_NULL_WITH_NULL_NULL = 0;
  TLS_RSA_WITH_NULL_MD5 = 1;
  TLS_RSA_WITH_NULL_SHA = 2;
  TLS_RSA_WITH_RC4_128_MD5 = 4;
  TLS_RSA_WITH_RC4_128_SHA = 5;
  TLS_RSA_WITH_3DES_EDE_CBC_SHA = 10;
  TLS_RSA_WITH_NULL_SHA256 = 59;
  TLS_RSA_WITH_AES_128_CBC_SHA256 = 60;
  TLS_RSA_WITH_AES_256_CBC_SHA256 = 61;
  TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA = 13;
  TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA = 16;
  TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA = 19;
  TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA = 22;
  TLS_DH_DSS_WITH_AES_128_CBC_SHA256 = 62;
  TLS_DH_RSA_WITH_AES_128_CBC_SHA256 = 63;
  TLS_DHE_DSS_WITH_AES_128_CBC_SHA256 = 64;
  TLS_DHE_RSA_WITH_AES_128_CBC_SHA256 = 103;
  TLS_DH_DSS_WITH_AES_256_CBC_SHA256 = 104;
  TLS_DH_RSA_WITH_AES_256_CBC_SHA256 = 105;
  TLS_DHE_DSS_WITH_AES_256_CBC_SHA256 = 106;
  TLS_DHE_RSA_WITH_AES_256_CBC_SHA256 = 107;
  TLS_DH_anon_WITH_RC4_128_MD5 = 24;
  TLS_DH_anon_WITH_3DES_EDE_CBC_SHA = 27;
  TLS_DH_anon_WITH_AES_128_CBC_SHA256 = 108;
  TLS_DH_anon_WITH_AES_256_CBC_SHA256 = 109;
  TLS_PSK_WITH_RC4_128_SHA = 138;
  TLS_PSK_WITH_3DES_EDE_CBC_SHA = 139;
  TLS_PSK_WITH_AES_128_CBC_SHA = 140;
  TLS_PSK_WITH_AES_256_CBC_SHA = 141;
  TLS_DHE_PSK_WITH_RC4_128_SHA = 142;
  TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA = 143;
  TLS_DHE_PSK_WITH_AES_128_CBC_SHA = 144;
  TLS_DHE_PSK_WITH_AES_256_CBC_SHA = 145;
  TLS_RSA_PSK_WITH_RC4_128_SHA = 146;
  TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA = 147;
  TLS_RSA_PSK_WITH_AES_128_CBC_SHA = 148;
  TLS_RSA_PSK_WITH_AES_256_CBC_SHA = 149;
  TLS_PSK_WITH_NULL_SHA = 44;
  TLS_DHE_PSK_WITH_NULL_SHA = 45;
  TLS_RSA_PSK_WITH_NULL_SHA = 46;
  TLS_RSA_WITH_AES_128_GCM_SHA256 = 156;
  TLS_RSA_WITH_AES_256_GCM_SHA384 = 157;
  TLS_DHE_RSA_WITH_AES_128_GCM_SHA256 = 158;
  TLS_DHE_RSA_WITH_AES_256_GCM_SHA384 = 159;
  TLS_DH_RSA_WITH_AES_128_GCM_SHA256 = 160;
  TLS_DH_RSA_WITH_AES_256_GCM_SHA384 = 161;
  TLS_DHE_DSS_WITH_AES_128_GCM_SHA256 = 162;
  TLS_DHE_DSS_WITH_AES_256_GCM_SHA384 = 163;
  TLS_DH_DSS_WITH_AES_128_GCM_SHA256 = 164;
  TLS_DH_DSS_WITH_AES_256_GCM_SHA384 = 165;
  TLS_DH_anon_WITH_AES_128_GCM_SHA256 = 166;
  TLS_DH_anon_WITH_AES_256_GCM_SHA384 = 167;
  TLS_PSK_WITH_AES_128_GCM_SHA256 = 168;
  TLS_PSK_WITH_AES_256_GCM_SHA384 = 169;
  TLS_DHE_PSK_WITH_AES_128_GCM_SHA256 = 170;
  TLS_DHE_PSK_WITH_AES_256_GCM_SHA384 = 171;
  TLS_RSA_PSK_WITH_AES_128_GCM_SHA256 = 172;
  TLS_RSA_PSK_WITH_AES_256_GCM_SHA384 = 173;
  TLS_PSK_WITH_AES_128_CBC_SHA256 = 174;
  TLS_PSK_WITH_AES_256_CBC_SHA384 = 175;
  TLS_PSK_WITH_NULL_SHA256 = 176;
  TLS_PSK_WITH_NULL_SHA384 = 177;
  TLS_DHE_PSK_WITH_AES_128_CBC_SHA256 = 178;
  TLS_DHE_PSK_WITH_AES_256_CBC_SHA384 = 179;
  TLS_DHE_PSK_WITH_NULL_SHA256 = 180;
  TLS_DHE_PSK_WITH_NULL_SHA384 = 181;
  TLS_RSA_PSK_WITH_AES_128_CBC_SHA256 = 182;
  TLS_RSA_PSK_WITH_AES_256_CBC_SHA384 = 183;
  TLS_RSA_PSK_WITH_NULL_SHA256 = 184;
  TLS_RSA_PSK_WITH_NULL_SHA384 = 185;
  TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256 = 49187;
  TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384 = 49188;
  TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA256 = 49189;
  TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA384 = 49190;
  TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256 = 49191;
  TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384 = 49192;
  TLS_ECDH_RSA_WITH_AES_128_CBC_SHA256 = 49193;
  TLS_ECDH_RSA_WITH_AES_256_CBC_SHA384 = 49194;
  TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256 = 49195;
  TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384 = 49196;
  TLS_ECDH_ECDSA_WITH_AES_128_GCM_SHA256 = 49197;
  TLS_ECDH_ECDSA_WITH_AES_256_GCM_SHA384 = 49198;
  TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256 = 49199;
  TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384 = 49200;
  TLS_ECDH_RSA_WITH_AES_128_GCM_SHA256 = 49201;
  TLS_ECDH_RSA_WITH_AES_256_GCM_SHA384 = 49202;
  TLS_EMPTY_RENEGOTIATION_INFO_SCSV = 255;
  SSL_RSA_WITH_RC2_CBC_MD5 = 65408;
  SSL_RSA_WITH_IDEA_CBC_MD5 = 65409;
  SSL_RSA_WITH_DES_CBC_MD5 = 65410;
  SSL_RSA_WITH_3DES_EDE_CBC_MD5 = 65411;
  SSL_NO_SUCH_CIPHERSUITE = 65535;
  errSecSuccess = 0;
  errSecUnimplemented = -4;
  errSecIO = -36;
  errSecOpWr = -49;
  errSecParam = -50;
  errSecAllocate = -108;
  errSecUserCanceled = -128;
  errSecBadReq = -909;
  errSecInternalComponent = -2070;
  errSecNotAvailable = -25291;
  errSecDuplicateItem = -25299;
  errSecItemNotFound = -25300;
  errSecInteractionNotAllowed = -25308;
  errSecDecode = -26275;
  errSecAuthFailed = -25293;
  kSecPaddingNone = 0;
  kSecPaddingPKCS1 = 1;
  kSecPaddingOAEP = 2;
  kSecPaddingPKCS1MD2 = 32768;
  kSecPaddingPKCS1MD5 = 32769;
  kSecPaddingPKCS1SHA1 = 32770;
  kSecPaddingPKCS1SHA224 = 32771;
  kSecPaddingPKCS1SHA256 = 32772;
  kSecPaddingPKCS1SHA384 = 32773;
  kSecPaddingPKCS1SHA512 = 32774;
  kSecRevocationOCSPMethod = (1 shl 0);
  kSecRevocationCRLMethod = (1 shl 1);
  kSecRevocationPreferCRL = (1 shl 2);
  kSecRevocationRequirePositiveResponse = (1 shl 3);
  kSecRevocationNetworkAccessDisabled = (1 shl 4);
  kSecRevocationUseAnyAvailableMethod = (kSecRevocationOCSPMethod or kSecRevocationCRLMethod);
  kSecTrustResultInvalid = 0;
  kSecTrustResultProceed = 1;
  kSecTrustResultConfirm = 2;
  kSecTrustResultDeny = 3;
  kSecTrustResultUnspecified = 4;
  kSecTrustResultRecoverableTrustFailure = 5;
  kSecTrustResultFatalTrustFailure = 6;
  kSecTrustResultOtherError = 7;
  kSSLProtocolUnknown = 0;
  kSSLProtocol3 = 2;
  kTLSProtocol1 = 4;
  kTLSProtocol11 = 7;
  kTLSProtocol12 = 8;
  kDTLSProtocol1 = 9;
  kSSLProtocol2 = 1;
  kSSLProtocol3Only = 3;
  kTLSProtocol1Only = 5;
  kSSLProtocolAll = 6;
  kSSLSessionOptionBreakOnServerAuth = 0;
  kSSLSessionOptionBreakOnCertRequested = 1;
  kSSLSessionOptionBreakOnClientAuth = 2;
  kSSLSessionOptionFalseStart = 3;
  kSSLSessionOptionSendOneByteRecord = 4;
  kSSLIdle = 0;
  kSSLHandshake = 1;
  kSSLConnected = 2;
  kSSLClosed = 3;
  kSSLAborted = 4;
  kSSLClientCertNone = 0;
  kSSLClientCertRequested = 1;
  kSSLClientCertSent = 2;
  kSSLClientCertRejected = 3;
  errSSLProtocol = -9800;
  errSSLNegotiation = -9801;
  errSSLFatalAlert = -9802;
  errSSLWouldBlock = -9803;
  errSSLSessionNotFound = -9804;
  errSSLClosedGraceful = -9805;
  errSSLClosedAbort = -9806;
  errSSLXCertChainInvalid = -9807;
  errSSLBadCert = -9808;
  errSSLCrypto = -9809;
  errSSLInternal = -9810;
  errSSLModuleAttach = -9811;
  errSSLUnknownRootCert = -9812;
  errSSLNoRootCert = -9813;
  errSSLCertExpired = -9814;
  errSSLCertNotYetValid = -9815;
  errSSLClosedNoNotify = -9816;
  errSSLBufferOverflow = -9817;
  errSSLBadCipherSuite = -9818;
  errSSLPeerUnexpectedMsg = -9819;
  errSSLPeerBadRecordMac = -9820;
  errSSLPeerDecryptionFail = -9821;
  errSSLPeerRecordOverflow = -9822;
  errSSLPeerDecompressFail = -9823;
  errSSLPeerHandshakeFail = -9824;
  errSSLPeerBadCert = -9825;
  errSSLPeerUnsupportedCert = -9826;
  errSSLPeerCertRevoked = -9827;
  errSSLPeerCertExpired = -9828;
  errSSLPeerCertUnknown = -9829;
  errSSLIllegalParam = -9830;
  errSSLPeerUnknownCA = -9831;
  errSSLPeerAccessDenied = -9832;
  errSSLPeerDecodeError = -9833;
  errSSLPeerDecryptError = -9834;
  errSSLPeerExportRestriction = -9835;
  errSSLPeerProtocolVersion = -9836;
  errSSLPeerInsufficientSecurity = -9837;
  errSSLPeerInternalError = -9838;
  errSSLPeerUserCancelled = -9839;
  errSSLPeerNoRenegotiation = -9840;
  errSSLPeerAuthCompleted = -9841;
  errSSLClientCertRequested = -9842;
  errSSLHostNameMismatch = -9843;
  errSSLConnectionRefused = -9844;
  errSSLDecryptionFail = -9845;
  errSSLBadRecordMac = -9846;
  errSSLRecordOverflow = -9847;
  errSSLBadConfiguration = -9848;
  errSSLUnexpectedRecord = -9849;
  kSSLServerSide = 0;
  kSSLClientSide = 1;
  kSSLStreamType = 0;
  kSSLDatagramType = 1;
  kNeverAuthenticate = 0;
  kAlwaysAuthenticate = 1;
  kTryAuthenticate = 2;

type
  // ===== Framework typedefs =====
{$M+}
  SSLCipherSuite = Word;
  SecCertificateRef = Pointer;
  SecIdentityRef = Pointer;
  SecKeyRef = Pointer;
  SecPolicyRef = Pointer;
  SecPadding = LongWord;
  SecRandomRef = Pointer;
  SecTrustResultType = LongWord;
  SecTrustRef = Pointer;
  SecTrustCallback = procedure(param1: SecTrustRef; param2: SecTrustResultType) of object;
  SSLContextRef = Pointer;
  SSLConnectionRef = Pointer;
  SSLProtocol = LongWord;
  SSLSessionOption = LongWord;
  SSLSessionState = LongWord;
  SSLClientCertificateState = LongWord;
  SSLReadFunc = function(param1: SSLConnectionRef; param2: Pointer; param3: LongWord): OSStatus; cdecl;
  SSLWriteFunc = function(param1: SSLConnectionRef; param2: Pointer; param3: LongWord): OSStatus; cdecl;
  SSLProtocolSide = LongWord;
  SSLConnectionType = LongWord;
  SSLAuthenticate = LongWord;
  // ===== External functions =====

const
  libSecurity = '/System/Library/Frameworks/Security.framework/Security';

function SecCertificateGetTypeID: CFTypeID; cdecl; external libSecurity name _PU + 'SecCertificateGetTypeID';
function SecCertificateCreateWithData(allocator: CFAllocatorRef; data: CFDataRef): SecCertificateRef; cdecl;
  external libSecurity name _PU + 'SecCertificateCreateWithData';
function SecCertificateCopyData(certificate: SecCertificateRef): CFDataRef; cdecl;
  external libSecurity name _PU + 'SecCertificateCopyData';
function SecCertificateCopySubjectSummary(certificate: SecCertificateRef): CFStringRef; cdecl;
  external libSecurity name _PU + 'SecCertificateCopySubjectSummary';
function SecIdentityGetTypeID: CFTypeID; cdecl; external libSecurity name _PU + 'SecIdentityGetTypeID';
function SecIdentityCopyCertificate(identityRef: SecIdentityRef; certificateRef: SecCertificateRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecIdentityCopyCertificate';
function SecIdentityCopyPrivateKey(identityRef: SecIdentityRef; privateKeyRef: SecKeyRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecIdentityCopyPrivateKey';
function SecPKCS12Import(pkcs12_data: CFDataRef; options: CFDictionaryRef; items: CFArrayRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecPKCS12Import';
function SecItemCopyMatching(query: CFDictionaryRef; result: CFTypeRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecItemCopyMatching';
function SecItemAdd(attributes: CFDictionaryRef; result: CFTypeRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecItemAdd';
function SecItemUpdate(query: CFDictionaryRef; attributesToUpdate: CFDictionaryRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecItemUpdate';
function SecItemDelete(query: CFDictionaryRef): OSStatus; cdecl; external libSecurity name _PU + 'SecItemDelete';
function SecKeyGetTypeID: CFTypeID; cdecl; external libSecurity name _PU + 'SecKeyGetTypeID';
function SecKeyGeneratePair(parameters: CFDictionaryRef; publicKey: SecKeyRef; privateKey: SecKeyRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecKeyGeneratePair';
function SecKeyRawSign(key: SecKeyRef; padding: SecPadding; dataToSign: Byte; dataToSignLen: LongWord; sig: Byte;
  sigLen: LongWord): OSStatus; cdecl; external libSecurity name _PU + 'SecKeyRawSign';
function SecKeyRawVerify(key: SecKeyRef; padding: SecPadding; signedData: Byte; signedDataLen: LongWord; sig: Byte;
  sigLen: LongWord): OSStatus; cdecl; external libSecurity name _PU + 'SecKeyRawVerify';
function SecKeyEncrypt(key: SecKeyRef; padding: SecPadding; plainText: Byte; plainTextLen: LongWord; cipherText: Byte;
  cipherTextLen: LongWord): OSStatus; cdecl; external libSecurity name _PU + 'SecKeyEncrypt';
function SecKeyDecrypt(key: SecKeyRef; padding: SecPadding; cipherText: Byte; cipherTextLen: LongWord; plainText: Byte;
  plainTextLen: LongWord): OSStatus; cdecl; external libSecurity name _PU + 'SecKeyDecrypt';
function SecKeyGetBlockSize(key: SecKeyRef): LongWord; cdecl; external libSecurity name _PU + 'SecKeyGetBlockSize';
function SecPolicyGetTypeID: CFTypeID; cdecl; external libSecurity name _PU + 'SecPolicyGetTypeID';
function SecPolicyCopyProperties(policyRef: SecPolicyRef): CFDictionaryRef; cdecl;
  external libSecurity name _PU + 'SecPolicyCopyProperties';
function SecPolicyCreateBasicX509: SecPolicyRef; cdecl; external libSecurity name _PU + 'SecPolicyCreateBasicX509';
function SecPolicyCreateSSL(server: Boolean; hostname: CFStringRef): SecPolicyRef; cdecl;
  external libSecurity name _PU + 'SecPolicyCreateSSL';
function SecPolicyCreateRevocation(revocationFlags: CFOptionFlags): SecPolicyRef; cdecl;
  external libSecurity name _PU + 'SecPolicyCreateRevocation';
function SecPolicyCreateWithProperties(policyIdentifier: CFTypeRef; properties: CFDictionaryRef): SecPolicyRef; cdecl;
  external libSecurity name _PU + 'SecPolicyCreateWithProperties';
function SecRandomCopyBytes(rnd: SecRandomRef; count: LongWord; bytes: Byte): Integer; cdecl;
  external libSecurity name _PU + 'SecRandomCopyBytes';
function SecTrustGetTypeID: CFTypeID; cdecl; external libSecurity name _PU + 'SecTrustGetTypeID';
function SecTrustCreateWithCertificates(certificates: CFTypeRef; policies: CFTypeRef; trust: SecTrustRef): OSStatus;
  cdecl; external libSecurity name _PU + 'SecTrustCreateWithCertificates';
function SecTrustSetPolicies(trust: SecTrustRef; policies: CFTypeRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecTrustSetPolicies';
function SecTrustCopyPolicies(trust: SecTrustRef; policies: CFArrayRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecTrustCopyPolicies';
function SecTrustSetNetworkFetchAllowed(trust: SecTrustRef; allowFetch: Boolean): OSStatus; cdecl;
  external libSecurity name _PU + 'SecTrustSetNetworkFetchAllowed';
function SecTrustGetNetworkFetchAllowed(trust: SecTrustRef; allowFetch: Boolean): OSStatus; cdecl;
  external libSecurity name _PU + 'SecTrustGetNetworkFetchAllowed';
function SecTrustSetAnchorCertificates(trust: SecTrustRef; anchorCertificates: CFArrayRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecTrustSetAnchorCertificates';
function SecTrustSetAnchorCertificatesOnly(trust: SecTrustRef; anchorCertificatesOnly: Boolean): OSStatus; cdecl;
  external libSecurity name _PU + 'SecTrustSetAnchorCertificatesOnly';
function SecTrustCopyCustomAnchorCertificates(trust: SecTrustRef; anchors: CFArrayRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecTrustCopyCustomAnchorCertificates';
function SecTrustSetVerifyDate(trust: SecTrustRef; verifyDate: CFDateRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecTrustSetVerifyDate';
function SecTrustGetVerifyTime(trust: SecTrustRef): CFAbsoluteTime; cdecl;
  external libSecurity name _PU + 'SecTrustGetVerifyTime';
function SecTrustEvaluate(trust: SecTrustRef; result: SecTrustResultType): OSStatus; cdecl;
  external libSecurity name _PU + 'SecTrustEvaluate';
function SecTrustEvaluateAsync(trust: SecTrustRef; queue: dispatch_queue_t; result: SecTrustCallback): OSStatus; cdecl;
  external libSecurity name _PU + 'SecTrustEvaluateAsync';
function SecTrustGetTrustResult(trust: SecTrustRef; result: SecTrustResultType): OSStatus; cdecl;
  external libSecurity name _PU + 'SecTrustGetTrustResult';
function SecTrustCopyPublicKey(trust: SecTrustRef): SecKeyRef; cdecl;
  external libSecurity name _PU + 'SecTrustCopyPublicKey';
function SecTrustGetCertificateCount(trust: SecTrustRef): CFIndex; cdecl;
  external libSecurity name _PU + 'SecTrustGetCertificateCount';
function SecTrustGetCertificateAtIndex(trust: SecTrustRef; ix: CFIndex): SecCertificateRef; cdecl;
  external libSecurity name _PU + 'SecTrustGetCertificateAtIndex';
function SecTrustCopyExceptions(trust: SecTrustRef): CFDataRef; cdecl;
  external libSecurity name _PU + 'SecTrustCopyExceptions';
function SecTrustSetExceptions(trust: SecTrustRef; exceptions: CFDataRef): Integer; cdecl;
  external libSecurity name _PU + 'SecTrustSetExceptions';
function SecTrustCopyProperties(trust: SecTrustRef): CFArrayRef; cdecl;
  external libSecurity name _PU + 'SecTrustCopyProperties';
function SecTrustCopyResult(trust: SecTrustRef): CFDictionaryRef; cdecl;
  external libSecurity name _PU + 'SecTrustCopyResult';
function SecTrustSetOCSPResponse(trust: SecTrustRef; responseData: CFTypeRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SecTrustSetOCSPResponse';
function SSLContextGetTypeID: CFTypeID; cdecl; external libSecurity name _PU + 'SSLContextGetTypeID';
function SSLCreateContext(alloc: CFAllocatorRef; protocolSide: SSLProtocolSide; connectionType: SSLConnectionType)
  : SSLContextRef; cdecl; external libSecurity name _PU + 'SSLCreateContext';
function SSLGetSessionState(context: SSLContextRef; state: SSLSessionState): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetSessionState';
function SSLSetSessionOption(context: SSLContextRef; option: SSLSessionOption; value: Boolean): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetSessionOption';
function SSLGetSessionOption(context: SSLContextRef; option: SSLSessionOption; value: Boolean): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetSessionOption';
function SSLSetIOFuncs(context: SSLContextRef; readFunc: SSLReadFunc; writeFunc: SSLWriteFunc): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetIOFuncs';
function SSLSetProtocolVersionMin(context: SSLContextRef; minVersion: SSLProtocol): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetProtocolVersionMin';
function SSLGetProtocolVersionMin(context: SSLContextRef; minVersion: SSLProtocol): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetProtocolVersionMin';
function SSLSetProtocolVersionMax(context: SSLContextRef; maxVersion: SSLProtocol): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetProtocolVersionMax';
function SSLGetProtocolVersionMax(context: SSLContextRef; maxVersion: SSLProtocol): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetProtocolVersionMax';
function SSLSetCertificate(context: SSLContextRef; certRefs: CFArrayRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetCertificate';
function SSLSetConnection(context: SSLContextRef; connection: SSLConnectionRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetConnection';
function SSLGetConnection(context: SSLContextRef; connection: SSLConnectionRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetConnection';
function SSLSetPeerDomainName(context: SSLContextRef; peerName: MarshaledAString; peerNameLen: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetPeerDomainName';
function SSLGetPeerDomainNameLength(context: SSLContextRef; peerNameLen: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetPeerDomainNameLength';
function SSLGetPeerDomainName(context: SSLContextRef; peerName: MarshaledAString; peerNameLen: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetPeerDomainName';
function SSLSetDatagramHelloCookie(dtlsContext: SSLContextRef; cookie: Pointer; cookieLen: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetDatagramHelloCookie';
function SSLSetMaxDatagramRecordSize(dtlsContext: SSLContextRef; maxSize: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetMaxDatagramRecordSize';
function SSLGetMaxDatagramRecordSize(dtlsContext: SSLContextRef; maxSize: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetMaxDatagramRecordSize';
function SSLGetNegotiatedProtocolVersion(context: SSLContextRef; protocol: SSLProtocol): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetNegotiatedProtocolVersion';
function SSLGetNumberSupportedCiphers(context: SSLContextRef; numCiphers: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetNumberSupportedCiphers';
function SSLGetSupportedCiphers(context: SSLContextRef; ciphers: SSLCipherSuite; numCiphers: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetSupportedCiphers';
function SSLSetEnabledCiphers(context: SSLContextRef; ciphers: SSLCipherSuite; numCiphers: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetEnabledCiphers';
function SSLGetNumberEnabledCiphers(context: SSLContextRef; numCiphers: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetNumberEnabledCiphers';
function SSLGetEnabledCiphers(context: SSLContextRef; ciphers: SSLCipherSuite; numCiphers: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetEnabledCiphers';
function SSLCopyPeerTrust(context: SSLContextRef; trust: SecTrustRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLCopyPeerTrust';
function SSLSetPeerID(context: SSLContextRef; peerID: Pointer; peerIDLen: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetPeerID';
function SSLGetPeerID(context: SSLContextRef; peerID: Pointer; peerIDLen: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetPeerID';
function SSLGetNegotiatedCipher(context: SSLContextRef; cipherSuite: SSLCipherSuite): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetNegotiatedCipher';
function SSLSetEncryptionCertificate(context: SSLContextRef; certRefs: CFArrayRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetEncryptionCertificate';
function SSLSetClientSideAuthenticate(context: SSLContextRef; auth: SSLAuthenticate): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLSetClientSideAuthenticate';
function SSLAddDistinguishedName(context: SSLContextRef; derDN: Pointer; derDNLen: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLAddDistinguishedName';
function SSLCopyDistinguishedNames(context: SSLContextRef; names: CFArrayRef): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLCopyDistinguishedNames';
function SSLGetClientCertificateState(context: SSLContextRef; clientState: SSLClientCertificateState): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetClientCertificateState';
function SSLHandshake(context: SSLContextRef): OSStatus; cdecl; external libSecurity name _PU + 'SSLHandshake';
function SSLWrite(context: SSLContextRef; data: Pointer; dataLength: LongWord; processed: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLWrite';
function SSLRead(context: SSLContextRef; data: Pointer; dataLength: LongWord; processed: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLRead';
function SSLGetBufferedReadSize(context: SSLContextRef; bufSize: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetBufferedReadSize';
function SSLGetDatagramWriteSize(dtlsContext: SSLContextRef; bufSize: LongWord): OSStatus; cdecl;
  external libSecurity name _PU + 'SSLGetDatagramWriteSize';
function SSLClose(context: SSLContextRef): OSStatus; cdecl; external libSecurity name _PU + 'SSLClose';

  // ===== Exported string consts =====

function kSecImportExportPassphrase: NSString;
function kSecImportItemLabel: NSString;
function kSecImportItemKeyID: NSString;
function kSecImportItemTrust: NSString;
function kSecImportItemCertChain: NSString;
function kSecImportItemIdentity: NSString;
function kSecClass: NSString;
function kSecClassInternetPassword: NSString;
function kSecClassGenericPassword: NSString;
function kSecClassCertificate: NSString;
function kSecClassKey: NSString;
function kSecClassIdentity: NSString;
function kSecMatchPolicy: NSString;
function kSecMatchItemList: NSString;
function kSecMatchSearchList: NSString;
function kSecMatchIssuers: NSString;
function kSecMatchEmailAddressIfPresent: NSString;
function kSecMatchSubjectContains: NSString;
function kSecMatchCaseInsensitive: NSString;
function kSecMatchTrustedOnly: NSString;
function kSecMatchValidOnDate: NSString;
function kSecMatchLimit: NSString;
function kSecMatchLimitOne: NSString;
function kSecMatchLimitAll: NSString;
function kSecReturnData: NSString;
function kSecReturnAttributes: NSString;
function kSecReturnRef: NSString;
function kSecReturnPersistentRef: NSString;
function kSecAttrAccessible: NSString;
function kSecAttrAccessControl: NSString;
function kSecAttrAccessGroup: NSString;
function kSecAttrSynchronizable: NSString;
function kSecAttrSynchronizableAny: NSString;
function kSecAttrCreationDate: NSString;
function kSecAttrModificationDate: NSString;
function kSecAttrDescription: NSString;
function kSecAttrComment: NSString;
function kSecAttrCreator: NSString;
function kSecAttrType: NSString;
function kSecAttrLabel: NSString;
function kSecAttrIsInvisible: NSString;
function kSecAttrIsNegative: NSString;
function kSecAttrAccount: NSString;
function kSecAttrService: NSString;
function kSecAttrGeneric: NSString;
function kSecAttrSecurityDomain: NSString;
function kSecAttrServer: NSString;
function kSecAttrProtocol: NSString;
function kSecAttrAuthenticationType: NSString;
function kSecAttrPort: NSString;
function kSecAttrPath: NSString;
function kSecAttrSubject: NSString;
function kSecAttrIssuer: NSString;
function kSecAttrSerialNumber: NSString;
function kSecAttrSubjectKeyID: NSString;
function kSecAttrPublicKeyHash: NSString;
function kSecAttrCertificateType: NSString;
function kSecAttrCertificateEncoding: NSString;
function kSecAttrKeyClass: NSString;
function kSecAttrApplicationLabel: NSString;
function kSecAttrIsPermanent: NSString;
function kSecAttrApplicationTag: NSString;
function kSecAttrKeyType: NSString;
function kSecAttrKeySizeInBits: NSString;
function kSecAttrEffectiveKeySize: NSString;
function kSecAttrCanEncrypt: NSString;
function kSecAttrCanDecrypt: NSString;
function kSecAttrCanDerive: NSString;
function kSecAttrCanSign: NSString;
function kSecAttrCanVerify: NSString;
function kSecAttrCanWrap: NSString;
function kSecAttrCanUnwrap: NSString;
function kSecAttrAccessibleWhenUnlocked: NSString;
function kSecAttrAccessibleAfterFirstUnlock: NSString;
function kSecAttrAccessibleAlways: NSString;
function kSecAttrAccessibleWhenPasscodeSetThisDeviceOnly: NSString;
function kSecAttrAccessibleWhenUnlockedThisDeviceOnly: NSString;
function kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly: NSString;
function kSecAttrAccessibleAlwaysThisDeviceOnly: NSString;
function kSecAttrProtocolFTP: NSString;
function kSecAttrProtocolFTPAccount: NSString;
function kSecAttrProtocolHTTP: NSString;
function kSecAttrProtocolIRC: NSString;
function kSecAttrProtocolNNTP: NSString;
function kSecAttrProtocolPOP3: NSString;
function kSecAttrProtocolSMTP: NSString;
function kSecAttrProtocolSOCKS: NSString;
function kSecAttrProtocolIMAP: NSString;
function kSecAttrProtocolLDAP: NSString;
function kSecAttrProtocolAppleTalk: NSString;
function kSecAttrProtocolAFP: NSString;
function kSecAttrProtocolTelnet: NSString;
function kSecAttrProtocolSSH: NSString;
function kSecAttrProtocolFTPS: NSString;
function kSecAttrProtocolHTTPS: NSString;
function kSecAttrProtocolHTTPProxy: NSString;
function kSecAttrProtocolHTTPSProxy: NSString;
function kSecAttrProtocolFTPProxy: NSString;
function kSecAttrProtocolSMB: NSString;
function kSecAttrProtocolRTSP: NSString;
function kSecAttrProtocolRTSPProxy: NSString;
function kSecAttrProtocolDAAP: NSString;
function kSecAttrProtocolEPPC: NSString;
function kSecAttrProtocolIPP: NSString;
function kSecAttrProtocolNNTPS: NSString;
function kSecAttrProtocolLDAPS: NSString;
function kSecAttrProtocolTelnetS: NSString;
function kSecAttrProtocolIMAPS: NSString;
function kSecAttrProtocolIRCS: NSString;
function kSecAttrProtocolPOP3S: NSString;
function kSecAttrAuthenticationTypeNTLM: NSString;
function kSecAttrAuthenticationTypeMSN: NSString;
function kSecAttrAuthenticationTypeDPA: NSString;
function kSecAttrAuthenticationTypeRPA: NSString;
function kSecAttrAuthenticationTypeHTTPBasic: NSString;
function kSecAttrAuthenticationTypeHTTPDigest: NSString;
function kSecAttrAuthenticationTypeHTMLForm: NSString;
function kSecAttrAuthenticationTypeDefault: NSString;
function kSecAttrKeyClassPublic: NSString;
function kSecAttrKeyClassPrivate: NSString;
function kSecAttrKeyClassSymmetric: NSString;
function kSecAttrKeyTypeRSA: NSString;
function kSecAttrKeyTypeEC: NSString;
function kSecValueRef: NSString; // https://quality.embarcadero.com/browse/RSP-20279
function kSecValueData: NSString; // https://quality.embarcadero.com/browse/RSP-20279


implementation


{$IF defined(IOS) and NOT defined(CPUARM)}

uses
  Posix.Dlfcn;

var
  SecurityModule: THandle;

{$ENDIF IOS}

function kSecImportExportPassphrase: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecImportExportPassphrase');
end;

function kSecImportItemLabel: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecImportItemLabel');
end;

function kSecImportItemKeyID: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecImportItemKeyID');
end;

function kSecImportItemTrust: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecImportItemTrust');
end;

function kSecImportItemCertChain: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecImportItemCertChain');
end;

function kSecImportItemIdentity: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecImportItemIdentity');
end;

function kSecClass: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecClass');
end;

function kSecClassInternetPassword: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecClassInternetPassword');
end;

function kSecClassGenericPassword: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecClassGenericPassword');
end;

function kSecClassCertificate: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecClassCertificate');
end;

function kSecClassKey: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecClassKey');
end;

function kSecClassIdentity: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecClassIdentity');
end;

function kSecMatchPolicy: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecMatchPolicy');
end;

function kSecMatchItemList: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecMatchItemList');
end;

function kSecMatchSearchList: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecMatchSearchList');
end;

function kSecMatchIssuers: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecMatchIssuers');
end;

function kSecMatchEmailAddressIfPresent: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecMatchEmailAddressIfPresent');
end;

function kSecMatchSubjectContains: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecMatchSubjectContains');
end;

function kSecMatchCaseInsensitive: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecMatchCaseInsensitive');
end;

function kSecMatchTrustedOnly: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecMatchTrustedOnly');
end;

function kSecMatchValidOnDate: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecMatchValidOnDate');
end;

function kSecMatchLimit: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecMatchLimit');
end;

function kSecMatchLimitOne: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecMatchLimitOne');
end;

function kSecMatchLimitAll: NSString;
begin
  Result := CocoaNSStringConst(libSecurity, 'kSecMatchLimitAll');
end;

function kSecReturnData: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecReturnData');
end;

function kSecReturnAttributes: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecReturnAttributes');
end;

function kSecReturnRef: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecReturnRef');
end;

function kSecReturnPersistentRef: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecReturnPersistentRef');
end;

function kSecAttrAccessible: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAccessible');
end;

function kSecAttrAccessControl: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAccessControl');
end;

function kSecAttrAccessGroup: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAccessGroup');
end;

function kSecAttrSynchronizable: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrSynchronizable');
end;

function kSecAttrSynchronizableAny: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrSynchronizableAny');
end;

function kSecAttrCreationDate: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrCreationDate');
end;

function kSecAttrModificationDate: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrModificationDate');
end;

function kSecAttrDescription: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrDescription');
end;

function kSecAttrComment: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrComment');
end;

function kSecAttrCreator: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrCreator');
end;

function kSecAttrType: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrType');
end;

function kSecAttrLabel: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrLabel');
end;

function kSecAttrIsInvisible: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrIsInvisible');
end;

function kSecAttrIsNegative: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrIsNegative');
end;

function kSecAttrAccount: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAccount');
end;

function kSecAttrService: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrService');
end;

function kSecAttrGeneric: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrGeneric');
end;

function kSecAttrSecurityDomain: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrSecurityDomain');
end;

function kSecAttrServer: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrServer');
end;

function kSecAttrProtocol: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocol');
end;

function kSecAttrAuthenticationType: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAuthenticationType');
end;

function kSecAttrPort: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrPort');
end;

function kSecAttrPath: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrPath');
end;

function kSecAttrSubject: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrSubject');
end;

function kSecAttrIssuer: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrIssuer');
end;

function kSecAttrSerialNumber: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrSerialNumber');
end;

function kSecAttrSubjectKeyID: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrSubjectKeyID');
end;

function kSecAttrPublicKeyHash: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrPublicKeyHash');
end;

function kSecAttrCertificateType: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrCertificateType');
end;

function kSecAttrCertificateEncoding: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrCertificateEncoding');
end;

function kSecAttrKeyClass: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrKeyClass');
end;

function kSecAttrApplicationLabel: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrApplicationLabel');
end;

function kSecAttrIsPermanent: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrIsPermanent');
end;

function kSecAttrApplicationTag: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrApplicationTag');
end;

function kSecAttrKeyType: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrKeyType');
end;

function kSecAttrKeySizeInBits: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrKeySizeInBits');
end;

function kSecAttrEffectiveKeySize: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrEffectiveKeySize');
end;

function kSecAttrCanEncrypt: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrCanEncrypt');
end;

function kSecAttrCanDecrypt: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrCanDecrypt');
end;

function kSecAttrCanDerive: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrCanDerive');
end;

function kSecAttrCanSign: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrCanSign');
end;

function kSecAttrCanVerify: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrCanVerify');
end;

function kSecAttrCanWrap: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrCanWrap');
end;

function kSecAttrCanUnwrap: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrCanUnwrap');
end;

function kSecAttrAccessibleWhenUnlocked: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAccessibleWhenUnlocked');
end;

function kSecAttrAccessibleAfterFirstUnlock: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAccessibleAfterFirstUnlock');
end;

function kSecAttrAccessibleAlways: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAccessibleAlways');
end;

function kSecAttrAccessibleWhenPasscodeSetThisDeviceOnly: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAccessibleWhenPasscodeSetThisDeviceOnly');
end;

function kSecAttrAccessibleWhenUnlockedThisDeviceOnly: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAccessibleWhenUnlockedThisDeviceOnly');
end;

function kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly');
end;

function kSecAttrAccessibleAlwaysThisDeviceOnly: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAccessibleAlwaysThisDeviceOnly');
end;

function kSecAttrProtocolFTP: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolFTP');
end;

function kSecAttrProtocolFTPAccount: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolFTPAccount');
end;

function kSecAttrProtocolHTTP: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolHTTP');
end;

function kSecAttrProtocolIRC: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolIRC');
end;

function kSecAttrProtocolNNTP: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolNNTP');
end;

function kSecAttrProtocolPOP3: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolPOP3');
end;

function kSecAttrProtocolSMTP: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolSMTP');
end;

function kSecAttrProtocolSOCKS: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolSOCKS');
end;

function kSecAttrProtocolIMAP: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolIMAP');
end;

function kSecAttrProtocolLDAP: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolLDAP');
end;

function kSecAttrProtocolAppleTalk: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolAppleTalk');
end;

function kSecAttrProtocolAFP: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolAFP');
end;

function kSecAttrProtocolTelnet: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolTelnet');
end;

function kSecAttrProtocolSSH: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolSSH');
end;

function kSecAttrProtocolFTPS: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolFTPS');
end;

function kSecAttrProtocolHTTPS: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolHTTPS');
end;

function kSecAttrProtocolHTTPProxy: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolHTTPProxy');
end;

function kSecAttrProtocolHTTPSProxy: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolHTTPSProxy');
end;

function kSecAttrProtocolFTPProxy: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolFTPProxy');
end;

function kSecAttrProtocolSMB: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolSMB');
end;

function kSecAttrProtocolRTSP: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolRTSP');
end;

function kSecAttrProtocolRTSPProxy: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolRTSPProxy');
end;

function kSecAttrProtocolDAAP: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolDAAP');
end;

function kSecAttrProtocolEPPC: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolEPPC');
end;

function kSecAttrProtocolIPP: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolIPP');
end;

function kSecAttrProtocolNNTPS: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolNNTPS');
end;

function kSecAttrProtocolLDAPS: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolLDAPS');
end;

function kSecAttrProtocolTelnetS: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolTelnetS');
end;

function kSecAttrProtocolIMAPS: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolIMAPS');
end;

function kSecAttrProtocolIRCS: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolIRCS');
end;

function kSecAttrProtocolPOP3S: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrProtocolPOP3S');
end;

function kSecAttrAuthenticationTypeNTLM: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAuthenticationTypeNTLM');
end;

function kSecAttrAuthenticationTypeMSN: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAuthenticationTypeMSN');
end;

function kSecAttrAuthenticationTypeDPA: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAuthenticationTypeDPA');
end;

function kSecAttrAuthenticationTypeRPA: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAuthenticationTypeRPA');
end;

function kSecAttrAuthenticationTypeHTTPBasic: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAuthenticationTypeHTTPBasic');
end;

function kSecAttrAuthenticationTypeHTTPDigest: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAuthenticationTypeHTTPDigest');
end;

function kSecAttrAuthenticationTypeHTMLForm: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAuthenticationTypeHTMLForm');
end;

function kSecAttrAuthenticationTypeDefault: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrAuthenticationTypeDefault');
end;

function kSecAttrKeyClassPublic: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrKeyClassPublic');
end;

function kSecAttrKeyClassPrivate: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrKeyClassPrivate');
end;

function kSecAttrKeyClassSymmetric: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrKeyClassSymmetric');
end;

function kSecAttrKeyTypeRSA: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrKeyTypeRSA');
end;

function kSecAttrKeyTypeEC: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecAttrKeyTypeEC');
end;

// https://quality.embarcadero.com/browse/RSP-20279
function kSecValueRef: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecValueRef');
end;

// https://quality.embarcadero.com/browse/RSP-20279
function kSecValueData: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecValueData');
end;

{$IF defined(IOS) and NOT defined(CPUARM)}

initialization

SecurityModule := dlopen(MarshaledAString(libSecurity), RTLD_LAZY);

finalization

dlclose(SecurityModule);
{$ENDIF IOS}

end.
