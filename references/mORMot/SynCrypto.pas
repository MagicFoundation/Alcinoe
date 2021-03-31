/// fast cryptographic routines (hashing and cypher)
// - implements AES,XOR,ADLER32,MD5,RC4,SHA1,SHA256,SHA384,SHA512,SHA3 and JWT
// - optimized for speed (tuned assembler and SSE3/SSE4/AES-NI/PADLOCK support)
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCrypto;

(*
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Alfred Glaenzer (alf)
  - Eric Grange for SHA-3 MMX asm optimization
  - EvaF
  - Intel's sha256_sse4.asm under under a three-clause Open Software license
  - Johan Bontes
  - souchaud
  - Project Nayuki (MIT License) for SHA-512 optimized x86 asm
  - Wolfgang Ehrhardt under zlib license for SHA-3 and AES "pure pascal" code
  - Maxim Masiutin for the MD5 asm

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****


      Synopse Cryptographic routines
      ==============================

    - fastest ever 100% Delphi (and asm ;) code
    - AES Crypto(128,192,256 bits key) with optimized asm version
      and multi-threaded code for multi-core CPU for blocks > 512 KB
    - XOR Crypto (32 bits key) - very fast with variable or fixed key
    - RC4 Crypto - weak, but simple and standard (used e.g. by SynPdf)
    - ADLER32 - 32 bits fast Hash with optimized asm version
    - MD5 - standard fast 128 bits Hash
    - SHA-1 - 160 bits Secure Hash
    - SHA-256 - 256 bits Secure Hash with optimized asm version
    - SHA-512 - 512 bits Secure Hash with optimized asm version (with SHA-384)
    - SHA-3 - 224/256/384/512/Shake algorithms based on Keccak permutation
    - hardware AES-NI and SHA-SSE4 support for latest CPU
    - VIA PADLOCK optional support - native .o code on linux or .dll (Win32)
     (tested on a Dedibox C7 (rev1) linux server - need validation for Win32)
    - Microsoft AES Cryptographic Provider optional support via CryptoAPI
*)

interface

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

{.$define USEPADLOCK}

{.$define AESPASCAL} // for debug

{$ifdef Linux}
  {$undef USETHREADSFORBIGAESBLOCKS} // uses low-level WinAPI threading
  {$ifdef KYLIX3}
    {.$define USEPADLOCK} // dedibox Linux tested only
  {$endif}
{$else}
  {$ifndef DELPHI5OROLDER}
    // on Windows: enable Microsoft AES Cryptographic Provider (XP SP3 and up)
    {$define USE_PROV_RSA_AES}
  {$endif}
  // on Windows: will use Threads for very big blocks (>512KB) if multi-CPU
  {$define USETHREADSFORBIGAESBLOCKS}
{$endif}

{$ifdef USEPADLOCK}
{$ifdef MSWINDOWS}
  {$define USEPADLOCKDLL}   // Win32: we can use LibPadlock.dll
{$else}
  {.$define PADLOCKDEBUG}   // display message before using padlock
  {.$define USEPADLOCKDLL}  // Linux: use fast .o linked code
{$endif}
{$endif}

uses
{$ifdef MSWINDOWS}
  Windows,
{$else}
  {$ifdef KYLIX3}
  LibC,
  SynKylix,
  {$endif}
  {$ifdef FPC}
  BaseUnix,
  SynFPCLinux,
  {$endif FPC}
  {$endif MSWINDOWS}
  SysUtils,
{$ifndef LVCL}
  {$ifndef DELPHI5OROLDER}
  RTLConsts,
  {$endif}
{$endif LVCL}
  Classes,
  SynLZ, // already included in SynCommons, and used by CompressShaAes()
  SynCommons,
  SynTable; // for TSynUniqueIdentifierGenerator

{$ifdef ABSOLUTEPASCAL}
  {$define AES_PASCAL}
  {$define SHA3_PASCAL}
{$else}
{$ifdef DELPHI5OROLDER}
  {$define AES_PASCAL} // Delphi 5 internal asm is buggy :(
  {$define SHA3_PASCAL}
  {$define SHA512_X86} // external sha512-x86.obj
{$else}
  {$ifdef CPUINTEL} // AES-NI supported for x86 and x64 under Windows
    {$ifdef CPU64}
      {$ifdef HASAESNI}
        {$define USEAESNI}
        {$define USEAESNI64}
      {$else}
        {$define AES_PASCAL} // Delphi XE2/XE3 do not have the AES-NI opcodes :(
      {$endif}
      {$define AESPASCAL_OR_CPU64}
      {$ifndef BSD}
        {$define CRC32C_X64} // external crc32_iscsi_01 for win64/lin64
        {$define SHA512_X64} // external sha512_sse4 for win64/lin64
      {$endif}
    {$else}
      {$ifdef MSWINDOWS}
        {$define SHA512_X86} // external sha512-x86.obj/.o
      {$endif}
      {$ifdef ABSOLUTEPASCAL}
        {$define AES_PASCAL} // x86 AES asm below is not PIC-safe
      {$else}
        {$define CPUX86_NOTPIC}
      {$endif ABSOLUTEPASCAL}
      {$ifdef FPC}
        {$ifdef DARWIN}
          {$define AES_PASCAL} // as reported by alf
        {$endif DARWIN}
        {$ifdef LINUX}
          {$ifndef AES_PASCAL}
            {$define SHA512_X86} // external linux32/sha512-x86.o
          {$endif AES_PASCAL}
        {$endif}
      {$endif FPC}
      {$ifndef AES_PASCAL}
        {$define USEAESNI} // some functions are not PIC-safe
        {$define USEAESNI32}
      {$endif AES_PASCAL}
    {$endif}
  {$else}
    {$define AES_PASCAL}
    {$define SHA3_PASCAL}
  {$endif CPUINTEL}
{$endif}
{$endif}

{$ifdef AES_PASCAL}
  {$define AESPASCAL_OR_CPU64}
{$endif}

{.$define AES_ROLLED}
// if defined, use rolled version, which is slightly slower (at least on my CPU)

{$ifndef AESPASCAL_OR_CPU64}
  {$define AES_ROLLED} // asm requires rolled decryption keys
{$endif}
{$ifdef CPUX64}
  {$define AES_ROLLED} // asm requires rolled decryption keys
{$endif}

{$ifdef USEPADLOCK}
var
  /// if dll/so and VIA padlock compatible CPU are present
  padlock_available: boolean = false;
{$endif}

const
  /// hide all AES Context complex code
  AESContextSize = 276+sizeof(pointer){$ifdef USEPADLOCK}*2{$endif}
    {$ifdef USEAESNI32}+sizeof(pointer){$endif};
  /// hide all SHA-1/SHA-2 complex code by storing the context as buffer
  SHAContextSize = 108;
  /// hide all SHA-3 complex code by storing the Keccak Sponge as buffer
  SHA3ContextSize = 412;
  /// power of two for a standard AES block size during cypher/uncypher
  // - to be used as 1 shl AESBlockShift or 1 shr AESBlockShift for fast div/mod
  AESBlockShift = 4;
  /// bit mask for fast modulo of AES block size
  AESBlockMod = 15;
  /// maximum AES key size (in bytes)
  AESKeySize = 256 div 8;

type
  /// class of Exceptions raised by this unit
  ESynCrypto = class(ESynException);

  /// 128 bits memory block for AES data cypher/uncypher
  TAESBlock = THash128;

  /// points to a 128 bits memory block, as used for AES data cypher/uncypher
  PAESBlock = ^TAESBlock;

  /// 256 bits memory block for maximum AES key storage
  TAESKey = THash256;

  /// stores an array of THash128 to check for their unicity
  // - used e.g. to implement TAESAbstract.IVHistoryDepth property, but may be
  // also used to efficiently store a list of 128-bit IPv6 addresses
  {$ifdef USERECORDWITHMETHODS}THash128History = record
    {$else}THash128History = object{$endif}
  private
    Previous: array of THash128Rec;
    Index: integer;
  public
    /// how many THash128 values can be stored
    Depth: integer;
    /// how many THash128 values are currently stored
    Count: integer;
    /// initialize the storage for a given history depth
    // - if Count reaches Depth, then older items will be removed
    procedure Init(size, maxsize: integer);
    /// O(n) fast search of a hash value in the stored entries
    // - returns true if the hash was found, or false if it did not appear
    function Exists(const hash: THash128): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// add a hash value to the stored entries, checking for duplicates
    // - returns true if the hash was added, or false if it did already appear
    function Add(const hash: THash128): boolean;
  end;

  PAES = ^TAES;
  /// handle AES cypher/uncypher
  // - this is the default Electronic codebook (ECB) mode
  // - this class will use AES-NI hardware instructions, if available
  {$ifdef USEPADLOCK}
  // - this class will use VIA PadLock instructions, if available
  {$endif}
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance (warning: not for Padlock)
  {$ifdef USERECORDWITHMETHODS}TAES = record
    {$else}TAES = object{$endif}
  private
    Context: packed array[1..AESContextSize] of byte;
    {$ifdef USEPADLOCK}
    function DoPadlockInit(const Key; KeySize: cardinal): boolean;
    {$endif}
  public
    /// Initialize AES contexts for cypher
    // - first method to call before using this object for encryption
    // - KeySize is in bits, i.e. 128,192,256
    function EncryptInit(const Key; KeySize: cardinal): boolean;
    /// encrypt an AES data block into another data block
    procedure Encrypt(const BI: TAESBlock; var BO: TAESBlock); overload;
    /// encrypt an AES data block
    procedure Encrypt(var B: TAESBlock); overload;

    /// Initialize AES contexts for uncypher
    // - first method to call before using this object for decryption
    // - KeySize is in bits, i.e. 128,192,256
    function DecryptInit(const Key; KeySize: cardinal): boolean;
    /// Initialize AES contexts for uncypher, from another TAES.EncryptInit
    function DecryptInitFrom(const Encryption{$ifndef DELPHI5OROLDER}: TAES{$endif};
      const Key; KeySize: cardinal): boolean;
    /// decrypt an AES data block
    procedure Decrypt(var B: TAESBlock); overload;
    /// decrypt an AES data block into another data block
    procedure Decrypt(const BI: TAESBlock; var BO: TAESBlock); overload;

    /// Finalize AES contexts for both cypher and uncypher
    // - would fill the TAES instance with zeros, for safety
    // - is only mandatoy when padlock is used
    procedure Done;

    /// generic initialization method for AES contexts
    // - call either EncryptInit() either DecryptInit() method
    function DoInit(const Key; KeySize: cardinal; doEncrypt: boolean): boolean;
    /// perform the AES cypher or uncypher to continuous memory blocks
    // - call either Encrypt() either Decrypt() method
    procedure DoBlocks(pIn, pOut: PAESBlock; out oIn, oOut: PAESBLock; Count: integer; doEncrypt: boolean); overload;
    /// perform the AES cypher or uncypher to continuous memory blocks
    // - call either Encrypt() either Decrypt() method
    procedure DoBlocks(pIn, pOut: PAESBlock; Count: integer; doEncrypt: boolean); overload;
    {$ifdef USETHREADSFORBIGAESBLOCKS}
    /// perform the AES cypher or uncypher to continuous memory blocks
    // - this special method will use Threads for bigs blocks (>512KB) if multi-CPU
    // - call either Encrypt() either Decrypt() method
    procedure DoBlocksThread(var bIn, bOut: PAESBlock; Count: integer; doEncrypt: boolean);
    {$endif}
    /// performs AES-OFB encryption and decryption on whole blocks
    // - may be called instead of TAESOFB when only a raw TAES is available
    // - this method is thread-safe (except if padlock is used)
    procedure DoBlocksOFB(const iv: TAESBlock; src, dst: pointer; blockcount: PtrUInt);
    /// TRUE if the context was initialized via EncryptInit/DecryptInit
    function Initialized: boolean; {$ifdef FPC}inline;{$endif}
    /// return TRUE if the AES-NI instruction sets are available on this CPU
    function UsesAESNI: boolean; {$ifdef HASINLINE}inline;{$endif}
    /// returns the key size in bits (128/192/256)
    function KeyBits: integer; {$ifdef FPC}inline;{$endif}
  end;

type
  /// low-level AES-GCM processing
  // - implements standard AEAD (authenticated-encryption with associated-data)
  // algorithm, as defined by NIST and
  TAESGCMEngine = object
  private
    /// standard AES encryption context
    // - will use AES-NI if available
    actx: TAES;
    /// ghash value of the Authentication Data
    aad_ghv: TAESBlock;
    /// ghash value of the Ciphertext
    txt_ghv: TAESBlock;
    /// ghash H current value
    ghash_h: TAESBlock;
    /// number of Authentication Data bytes processed
    aad_cnt: TQWordRec;
    /// number of bytes of the Ciphertext
    atx_cnt: TQWordRec;
    /// initial 32-bit ctr val - to be reused in Final()
    y0_val: integer;
    /// current 0..15 position in encryption block
    blen: byte;
    /// the state of this context
    flags: set of (flagInitialized, flagFinalComputed, flagFlushed);
    /// lookup table for fast Galois Finite Field multiplication
    // - is defined as last field of the object for better code generation
    gf_t4k: array[byte] of TAESBlock;
    /// build the gf_t4k[] internal table - assuming set to zero by caller
    procedure Make4K_Table;
    /// compute a * ghash_h in Galois Finite Field 2^128
    procedure gf_mul_h(var a: TAESBlock); {$ifdef FPC} inline; {$endif}
    /// low-level AES-CTR encryption
    procedure internal_crypt(ptp, ctp: PByte; ILen: PtrUInt);
    /// low-level GCM authentication
    procedure internal_auth(ctp: PByte; ILen: PtrUInt;
      var ghv: TAESBlock; var gcnt: TQWordRec);
  public
    /// initialize the AES-GCM structure for the supplied Key
    function Init(const Key; KeyBits: PtrInt): boolean;
    /// start AES-GCM encryption with a given Initialization Vector
    // - IV_len is in bytes use 12 for exact IV setting, otherwise the
    // supplied buffer will be hashed using gf_mul_h()
    function Reset(pIV: pointer; IV_len: PtrInt): boolean;
    /// encrypt a buffer with AES-GCM, updating the associated authentication data
    function Encrypt(ptp, ctp: Pointer; ILen: PtrInt): boolean;
    /// decrypt a buffer with AES-GCM, updating the associated authentication data
    // - also validate the GMAC with the supplied ptag/tlen if ptag<>nil,
    // and skip the AES-CTR phase if the authentication doesn't match
    function Decrypt(ctp, ptp: Pointer; ILen: PtrInt;
      ptag: pointer=nil; tlen: PtrInt=0): boolean;
    /// append some data to be authenticated, but not encrypted
    function Add_AAD(pAAD: pointer; aLen: PtrInt): boolean;
    /// finalize the AES-GCM encryption, returning the authentication tag
    // - will also flush the AES context to avoid forensic issues, unless
    // andDone is forced to false
    function Final(out tag: TAESBlock; andDone: boolean=true): boolean;
    /// flush the AES context to avoid forensic issues
    // - do nothing if Final() has been already called
    procedure Done;
    /// single call AES-GCM encryption and authentication process
    function FullEncryptAndAuthenticate(const Key; KeyBits: PtrInt;
      pIV: pointer; IV_len: PtrInt; pAAD: pointer; aLen: PtrInt;
      ptp, ctp: Pointer; pLen: PtrInt; out tag: TAESBlock): boolean;
    /// single call AES-GCM decryption and verification process
    function FullDecryptAndVerify(const Key; KeyBits: PtrInt;
      pIV: pointer; IV_len: PtrInt; pAAD: pointer; aLen: PtrInt;
      ctp, ptp: Pointer; pLen: PtrInt; ptag: pointer; tLen: PtrInt): boolean;
  end;

  /// class-reference type (metaclass) of an AES cypher/uncypher
  TAESAbstractClass = class of TAESAbstract;

  /// used internally by TAESAbstract to detect replay attacks
  // - when EncryptPKCS7/DecryptPKCS7 are used with IVAtBeginning=true, and
  // IVReplayAttackCheck property contains repCheckedIfAvailable or repMandatory
  // - EncryptPKCS7 will encrypt this record (using the global shared
  // AESIVCTR_KEY over AES-128) to create a random IV, as a secure
  // cryptographic pseudorandom number generator (CSPRNG), nonce and ctr
  // ensuring 96 bits of entropy
  // - DecryptPKCS7 will decode and ensure that the IV has an increasing CTR
  // - memory size matches an TAESBlock on purpose, for direct encryption
  TAESIVCTR = packed record
    /// 8 bytes of random value
    nonce: QWord;
    /// contains the crc32c hash of the block cipher mode (e.g. 'AESCFB')
    // - when magic won't match (i.e. in case of mORMot revision < 3063), the
    // check won't be applied in DecryptPKCS7: this security feature is
    // backward compatible if IVReplayAttackCheck is repCheckedIfAvailable,
    // but will fail for repMandatory
    magic: cardinal;
    /// an increasing counter, used to detect replay attacks
    // - is set to a 32-bit random value at initialization
    // - is increased by one for every EncryptPKCS7, so can be checked against
    // replay attack in DecryptPKCS7, and implement a safe CSPRNG for stored IV
    ctr: cardinal;
  end;

  /// how TAESAbstract.DecryptPKCS7 should detect replay attack
  // - repNoCheck and repCheckedIfAvailable will be compatible with older
  // versions of the protocol, but repMandatory will reject any encryption
  // without the TAESIVCTR algorithm
  TAESIVReplayAttackCheck = (repNoCheck, repCheckedIfAvailable, repMandatory);

  /// handle AES cypher/uncypher with chaining
  // - use any of the inherited implementation, corresponding to the chaining
  // mode required - TAESECB, TAESCBC, TAESCFB, TAESOFB and TAESCTR classes to
  // handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7-like padding)
  TAESAbstract = class(TSynPersistent)
  protected
    fKeySize: cardinal;
    fKeySizeBytes: cardinal;
    fKey: TAESKey;
    fIV: TAESBlock;
    fIVCTR: TAESIVCTR;
    fIVCTRState: (ctrUnknown, ctrUsed, ctrNotused);
    fIVHistoryDec: THash128History;
    fIVReplayAttackCheck: TAESIVReplayAttackCheck;
    procedure SetIVHistory(aDepth: integer);
    procedure SetIVCTR;
    function DecryptPKCS7Len(var InputLen,ivsize: integer; Input: pointer;
      IVAtBeginning, RaiseESynCryptoOnError: boolean): boolean;
  public
    /// Initialize AES context for cypher
    // - first method to call before using this class
    // - KeySize is in bits, i.e. 128,192,256
    constructor Create(const aKey; aKeySize: cardinal); reintroduce; overload; virtual;
    /// Initialize AES context for AES-128 cypher
    // - first method to call before using this class
    // - just a wrapper around Create(aKey,128);
    constructor Create(const aKey: THash128); reintroduce; overload;
    /// Initialize AES context for AES-256 cypher
    // - first method to call before using this class
    // - just a wrapper around Create(aKey,256);
    constructor Create(const aKey: THash256); reintroduce; overload;
    /// Initialize AES context for cypher, from some TAESPRNG random bytes
    // - may be used to hide some sensitive information from memory, like
    // CryptDataForCurrentUser but with a temporary key
    constructor CreateTemp(aKeySize: cardinal);
    /// Initialize AES context for cypher, from SHA-256 hash
    // - here the Key is supplied as a string, and will be hashed using SHA-256
    // via the SHA256Weak proprietary algorithm - to be used only for backward
    // compatibility of existing code
    // - consider using more secure (and more standard) CreateFromPBKDF2 instead
    constructor CreateFromSha256(const aKey: RawUTF8);
    /// Initialize AES context for cypher, from PBKDF2_HMAC_SHA256 derivation
    // - here the Key is supplied as a string, and will be hashed using
    // PBKDF2_HMAC_SHA256 with the specified salt and rounds
    constructor CreateFromPBKDF2(const aKey: RawUTF8; const aSalt: RawByteString;
      aRounds: Integer);
    /// compute a class instance similar to this one
    // - could be used to have a thread-safe re-use of a given encryption key
    function Clone: TAESAbstract; virtual;
    /// compute a class instance similar to this one, for performing the
    // reverse encryption/decryption process
    // - this default implementation calls Clone, but CFB/OFB/CTR chaining modes
    // using only AES encryption (i.e. inheriting from TAESAbstractEncryptOnly)
    // will return self to avoid creating two instances
    // - warning: to be used only with IVAtBeginning=false
    function CloneEncryptDecrypt: TAESAbstract; virtual;
    /// release the used instance memory and resources
    // - also fill the secret fKey buffer with zeros, for safety
    destructor Destroy; override;

    /// perform the AES cypher in the corresponding mode
    // - when used in block chaining mode, you should have set the IV property
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); virtual; abstract;
    /// perform the AES un-cypher in the corresponding mode
    // - when used in block chaining mode, you should have set the IV property
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); virtual; abstract;

    /// encrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 padding is described in RFC 5652 - it will add up to 16 bytes to
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer - this IV may
    // contain an internal encrypted CTR, to detect any replay attack attempt,
    // if IVReplayAttackCheck is set to repCheckedIfAvailable or repMandatory
    function EncryptPKCS7(const Input: RawByteString; IVAtBeginning: boolean=false): RawByteString; overload;
    /// decrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 padding is described in RFC 5652 - it will trim up to 16 bytes from
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    // - if IVAtBeginning is TRUE, the Initialization Vector will be taken
    // from the beginning of the input binary buffer - if IVReplayAttackCheck is
    // set, this IV will be validated to contain an increasing encrypted CTR,
    // and raise an ESynCrypto when a replay attack attempt is detected
    // - if RaiseESynCryptoOnError=false, returns '' on any decryption error
    function DecryptPKCS7(const Input: RawByteString; IVAtBeginning: boolean=false;
      RaiseESynCryptoOnError: boolean=true): RawByteString; overload;
    /// encrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 padding is described in RFC 5652 - it will add up to 16 bytes to
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer - this IV may
    // contain an internal encrypted CTR, to detect any replay attack attempt,
    // if IVReplayAttackCheck is set to repCheckedIfAvailable or repMandatory
    function EncryptPKCS7(const Input: TBytes; IVAtBeginning: boolean=false): TBytes; overload;
    /// decrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 padding is described in RFC 5652 - it will trim up to 16 bytes from
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    // - if IVAtBeginning is TRUE, the Initialization Vector will be taken
    // from the beginning of the input binary buffer - if IVReplayAttackCheck is
    // set, this IV will be validated to contain an increasing encrypted CTR,
    // and raise an ESynCrypto when a replay attack attempt is detected
    // - if RaiseESynCryptoOnError=false, returns [] on any decryption error
    function DecryptPKCS7(const Input: TBytes; IVAtBeginning: boolean=false;
      RaiseESynCryptoOnError: boolean=true): TBytes; overload;

    /// compute how many bytes would be needed in the output buffer, when
    // encrypte using a PKCS7 padding pattern
    // - could be used to pre-compute the OutputLength for EncryptPKCS7Buffer()
    // - PKCS7 padding is described in RFC 5652 - it will add up to 16 bytes to
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    function EncryptPKCS7Length(InputLen: cardinal; IVAtBeginning: boolean): cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    /// encrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 padding is described in RFC 5652 - it will add up to 16 bytes to
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    // - use EncryptPKCS7Length() function to compute the actual needed length
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer - this IV will in
    // fact contain an internal encrypted CTR, to detect any replay attack attempt
    // - returns TRUE on success, FALSE if OutputLen is not correct - you should
    // use EncryptPKCS7Length() to compute the exact needed number of bytes
    function EncryptPKCS7Buffer(Input,Output: Pointer; InputLen,OutputLen: cardinal;
      IVAtBeginning: boolean): boolean;
    /// decrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 padding is described in RFC 5652 - it will trim up to 16 bytes from
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    // - if IVAtBeginning is TRUE, the Initialization Vector will be taken
    // from the beginning of the input binary buffer  - this IV will in fact
    // contain an internal encrypted CTR, to detect any replay attack attempt
    // - if RaiseESynCryptoOnError=false, returns '' on any decryption error
    function DecryptPKCS7Buffer(Input: Pointer; InputLen: integer;
      IVAtBeginning: boolean; RaiseESynCryptoOnError: boolean=true): RawByteString;

    /// initialize AEAD (authenticated-encryption with associated-data) nonce
    // - i.e. setup 256-bit MAC computation during next Encrypt/Decrypt call
    // - may be used e.g. for AES-GCM or our custom AES-CTR modes
    // - default implementation, for a non AEAD protocol, returns false
    function MACSetNonce(const aKey: THash256; aAssociated: pointer=nil;
      aAssociatedLen: integer=0): boolean; virtual;
    /// returns AEAD (authenticated-encryption with associated-data) MAC
    /// - i.e. optional 256-bit MAC computation during last Encrypt/Decrypt call
    // - may be used e.g. for AES-GCM or our custom AES-CTR modes
    // - default implementation, for a non AEAD protocol, returns false
    function MACGetLast(out aCRC: THash256): boolean; virtual;
    /// validate if the computed AEAD MAC matches the expected supplied value
    // - is just a wrapper around MACGetLast() and IsEqual() functions
    function MACEquals(const aCRC: THash256): boolean; virtual;
    /// validate if an encrypted buffer matches the stored AEAD MAC
    // - expects the 256-bit MAC, as returned by MACGetLast, to be stored after
    // the encrypted data
    // - default implementation, for a non AEAD protocol, returns false
    function MACCheckError(aEncrypted: pointer; Count: cardinal): boolean; virtual;
    /// perform one step PKCS7 encryption/decryption and authentication from
    // a given 256-bit key
    // - returns '' on any (MAC) issue during decryption (Encrypt=false) or if
    // this class does not support AEAD MAC
    // - as used e.g. by CryptDataForCurrentUser()
    // - do not use this abstract class method, but inherited TAESCFBCRC/TAESOFBCRC
    // - will store a header with its own CRC, so detection of most invalid
    // formats (e.g. from fuzzing input) will occur before any AES/MAC process
    class function MACEncrypt(const Data: RawByteString; const Key: THash256;
      Encrypt: boolean): RawByteString; overload;
    /// perform one step PKCS7 encryption/decryption and authentication from
    // a given 128-bit key
    // - returns '' on any (MAC) issue during decryption (Encrypt=false) or if
    // this class does not support AEAD MAC
    // - do not use this abstract class method, but inherited TAESCFBCRC/TAESOFBCRC
    // - will store a header with its own CRC, so detection of most invalid
    // formats (e.g. from fuzzing input) will occur before any AES/MAC process
    class function MACEncrypt(const Data: RawByteString; const Key: THash128;
      Encrypt: boolean): RawByteString; overload;
    /// perform one step PKCS7 encryption/decryption and authentication with
    // the curent AES instance
    // - returns '' on any (MAC) issue during decryption (Encrypt=false) or if
    // this class does not support AEAD MAC
    // - as used e.g. by CryptDataForCurrentUser()
    // - do not use this abstract class method, but inherited TAESCFBCRC/TAESOFBCRC
    // - will store a header with its own CRC, so detection of most invalid
    // formats (e.g. from fuzzing input) will occur before any AES/MAC process
    function MACAndCrypt(const Data: RawByteString; Encrypt: boolean): RawByteString;

    /// simple wrapper able to cypher/decypher any in-memory content
    // - here data variables could be text or binary
    // - use StringToUTF8() to define the Key parameter from a VCL string
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer
    // - will use SHA256Weak() and PKCS7 padding with the current class mode
    class function SimpleEncrypt(const Input,Key: RawByteString; Encrypt: boolean;
      IVAtBeginning: boolean=false; RaiseESynCryptoOnError: boolean=true): RawByteString; overload;
    /// simple wrapper able to cypher/decypher any in-memory content
    // - here data variables could be text or binary
    // - you could use e.g. THMAC_SHA256 to safely compute the Key/KeySize value
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer
    // - will use SHA256Weak() and PKCS7 padding with the current class mode
    class function SimpleEncrypt(const Input: RawByteString; const Key;
      KeySize: integer; Encrypt: boolean; IVAtBeginning: boolean=false;
      RaiseESynCryptoOnError: boolean=true): RawByteString; overload;
    /// simple wrapper able to cypher/decypher any file content
    // - just a wrapper around SimpleEncrypt() and StringFromFile/FileFromString
    // - use StringToUTF8() to define the Key parameter from a VCL string
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer
    // - will use SHA256Weak() and PKCS7 padding with the current class mode
    class function SimpleEncryptFile(const InputFile, OutputFile: TFileName;
      const Key: RawByteString; Encrypt: boolean; IVAtBeginning: boolean=false;
      RaiseESynCryptoOnError: boolean=true): boolean; overload;
    /// simple wrapper able to cypher/decypher any file content
    // - just a wrapper around SimpleEncrypt() and StringFromFile/FileFromString
    // - you could use e.g. THMAC_SHA256 to safely compute the Key/KeySize value
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer
    // - will use SHA256Weak() and PKCS7 padding with the current class mode
    class function SimpleEncryptFile(const InputFile, Outputfile: TFileName; const Key;
      KeySize: integer; Encrypt: boolean; IVAtBeginning: boolean=false;
      RaiseESynCryptoOnError: boolean=true): boolean; overload;
    //// returns e.g. 'aes128cfb' or '' if nil
    function AlgoName: TShort16;

    /// associated Key Size, in bits (i.e. 128,192,256)
    property KeySize: cardinal read fKeySize;
    /// associated Initialization Vector
    // - all modes (except ECB) do expect an IV to be supplied for chaining,
    // before any encryption or decryption is performed
    // - you could also use PKCS7 encoding with IVAtBeginning=true option
    property IV: TAESBlock read fIV write fIV;
    /// let IV detect replay attack for EncryptPKCS7 and DecryptPKCS7
    // - if IVAtBeginning=true and this property is set, EncryptPKCS7 will
    // store a random IV from an internal CTR, and DecryptPKCS7 will check this
    // incoming IV CTR consistency, and raise an ESynCrypto exception on failure
    // - leave it to its default repNoCheck if the very same TAESAbstract
    // instance is expected to be used with several sources, by which the IV CTR
    // will be unsynchronized
    // - security warning: by design, this is NOT cautious with CBC chaining:
    // you should use it only with CFB, OFB or CTR mode, since the IV sequence
    // will be predictable if you know the fixed AES private key of this unit,
    // but the IV sequence features uniqueness as it is generated by a good PRNG -
    // see http://crypto.stackexchange.com/q/3515
    property IVReplayAttackCheck: TAESIVReplayAttackCheck
      read fIVReplayAttackCheck write fIVReplayAttackCheck;
    /// maintains an history of previous IV, to avoid re-play attacks
    // - only useful when EncryptPKCS7/DecryptPKCS7 are used with
    // IVAtBeginning=true, and IVReplayAttackCheck is left to repNoCheck
    property IVHistoryDepth: integer read fIVHistoryDec.Depth write SetIVHistory;
  end;

  /// handle AES cypher/uncypher with chaining with out own optimized code
  // - use any of the inherited implementation, corresponding to the chaining
  // mode required - TAESECB, TAESCBC, TAESCFB, TAESOFB and TAESCTR classes to
  // handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7-like padding)
  // - this class will use AES-NI hardware instructions, if available
  // - those classes are re-entrant, i.e. that you can call the Encrypt*
  // or Decrypt* methods on the same instance several times
  TAESAbstractSyn = class(TAESAbstract)
  protected
    fIn, fOut: PAESBlock;
    fCV: TAESBlock;
    AES: TAES;
    fAESInit: (initNone, initEncrypt, initDecrypt);
    procedure EncryptInit;
    procedure DecryptInit;
    procedure TrailerBytes(count: cardinal);
  public
    /// creates a new instance with the very same values
    // - by design, our classes will use TAES stateless context, so this method
    // will just copy the current fields to a new instance, by-passing
    // the key creation step
    function Clone: TAESAbstract; override;
    /// release the used instance memory and resources
    // - also fill the TAES instance with zeros, for safety
    destructor Destroy; override;
    /// perform the AES cypher in the corresponding mode, over Count bytes
    // - this abstract method will set CV from fIV property, and fIn/fOut
    // from BufIn/BufOut
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the corresponding mode
    // - this abstract method will set CV from fIV property, and fIn/fOut
    // from BufIn/BufOut
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// read-only access to the internal CV block, which may be have just been
    // used by Encrypt/Decrypt methods
    property CV: TAESBlock read fCV;
  end;

  /// handle AES cypher/uncypher without chaining (ECB)
  // - this mode is known to be less secure than the others
  // - IV property should be set to a fixed value to encode the trailing bytes
  // of the buffer by a simple XOR - but you should better use the PKC7 pattern
  // - this class will use AES-NI hardware instructions, if available, e.g.
  // ! ECB128: 19.70ms in x86 optimized code, 6.97ms with AES-NI
  TAESECB = class(TAESAbstractSyn)
  public
    /// perform the AES cypher in the ECB mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the ECB mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with Cipher-block chaining (CBC)
  // - this class will use AES-NI hardware instructions, if available, e.g.
  // ! CBC192: 24.91ms in x86 optimized code, 9.75ms with AES-NI
  // - expect IV to be set before process, or IVAtBeginning=true
  TAESCBC = class(TAESAbstractSyn)
  public
    /// perform the AES cypher in the CBC mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CBC mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// abstract parent class for chaining modes using only AES encryption
  TAESAbstractEncryptOnly = class(TAESAbstractSyn)
  public
    /// Initialize AES context for cypher
    // - will pre-generate the encryption key (aKeySize in bits, i.e. 128,192,256)
    constructor Create(const aKey; aKeySize: cardinal); override;
    /// compute a class instance similar to this one, for performing the
    // reverse encryption/decryption process
    // - will return self to avoid creating two instances
    // - warning: to be used only with IVAtBeginning=false
    function CloneEncryptDecrypt: TAESAbstract; override;
  end;

  /// handle AES cypher/uncypher with Cipher feedback (CFB)
  // - this class will use AES-NI hardware instructions, if available, e.g.
  // ! CFB128: 22.25ms in x86 optimized code, 9.29ms with AES-NI
  // - expect IV to be set before process, or IVAtBeginning=true
  TAESCFB = class(TAESAbstractEncryptOnly)
  public
    /// perform the AES cypher in the CFB mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CFB mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with Output feedback (OFB)
  // - this class will use AES-NI hardware instructions, if available, e.g.
  // ! OFB256: 27.69ms in x86 optimized code, 9.94ms with AES-NI
  // - expect IV to be set before process, or IVAtBeginning=true
  // - TAESOFB 128/256 have an optimized asm version under x86_64 + AES_NI
  TAESOFB = class(TAESAbstractEncryptOnly)
  public
    /// perform the AES cypher in the OFB mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the OFB mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with 64-bit Counter mode (CTR)
  // - the CTR will use a counter in bytes 7..0 by default - which is safe
  // but not standard - call ComposeIV() to change e.g. to NIST behavior
  // - this class will use AES-NI hardware instructions, e.g.
  // ! CTR256: 28.13ms in x86 optimized code, 10.63ms with AES-NI
  // - expect IV to be set before process, or IVAtBeginning=true
  TAESCTR = class(TAESAbstractEncryptOnly)
  protected
    fCTROffset, fCTROffsetMin: PtrInt;
  public
    /// Initialize AES context for cypher
    // - will pre-generate the encryption key (aKeySize in bits, i.e. 128,192,256)
    constructor Create(const aKey; aKeySize: cardinal); override;
    /// defines how the IV is set and updated in CTR mode
    // - default (if you don't call this method) uses a Counter in bytes 7..0
    // - you can specify startup Nonce and Counter, and the Counter position
    // - NonceLen + CounterLen should be 16 - otherwise it fails and returns false
    function ComposeIV(Nonce, Counter: PAESBlock; NonceLen, CounterLen: integer;
      LSBCounter: boolean): boolean; overload;
    /// defines how the IV is set and updated in CTR mode
    // - you can specify startup Nonce and Counter, and the Counter position
    // - Nonce + Counter lengths should add to 16 - otherwise returns false
    function ComposeIV(const Nonce, Counter: TByteDynArray; LSBCounter: boolean): boolean; overload;
    /// perform the AES cypher in the CTR mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CTR mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// internal 256-bit structure used for TAESAbstractAEAD MAC storage
  TAESMAC256 = record
    /// the AES-encrypted MAC of the plain content
    // - plain text digital signature, to perform message authentication
    // and integrity
    plain: THash128;
    /// the plain MAC of the encrypted content
    // - encrypted text digital signature, to check for errors,
    // with no compromission of the plain content
    encrypted: THash128;
  end;

  /// AEAD (authenticated-encryption with associated-data) abstract class
  // - perform AES encryption and on-the-fly MAC computation, i.e. computes
  // a proprietary 256-bit MAC during AES cyphering, as 128-bit CRC of the
  // encrypted data and 128-bit CRC of the plain data, seeded from a Key
  // - the 128-bit CRC of the plain text is then encrypted using the current AES
  // engine, so returned 256-bit MAC value has cryptographic level, and ensure
  // data integrity, authenticity, and check against transmission errors
  TAESAbstractAEAD = class(TAESAbstractEncryptOnly)
  protected
    fMAC, fMACKey: TAESMAC256;
  public
    /// release the used instance memory and resources
    // - also fill the internal internal MAC hashes with zeros, for safety
    destructor Destroy; override;
    /// initialize 256-bit MAC computation for next Encrypt/Decrypt call
    // - initialize the internal fMACKey property, and returns true
    // - only the plain text crc is seeded from aKey - encrypted message crc
    // will use -1 as fixed seed, to avoid aKey compromission
    // - should be set with a new MAC key value before each message, to avoid
    // replay attacks (as called from TECDHEProtocol.SetKey)
    function MACSetNonce(const aKey: THash256; aAssociated: pointer=nil;
      aAssociatedLen: integer=0): boolean; override;
    /// returns 256-bit MAC computed during last Encrypt/Decrypt call
    // - encrypt the internal fMAC property value using the current AES cypher
    // on the plain content and returns true; only the plain content CRC-128 is
    // AES encrypted, to avoid reverse attacks against the known encrypted data
    function MACGetLast(out aCRC: THash256): boolean; override;
    /// validate if an encrypted buffer matches the stored MAC
    // - expects the 256-bit MAC, as returned by MACGetLast, to be stored after
    // the encrypted data
    // - returns true if the 128-bit CRC of the encrypted text matches the
    // supplied buffer, ignoring the 128-bit CRC of the plain data
    // - since it is easy to forge such 128-bit CRC, it will only indicate
    // that no transmission error occured, but won't be an integrity or
    // authentication proof (which will need full Decrypt + MACGetLast)
    // - may use any MACSetNonce() aAssociated value
    function MACCheckError(aEncrypted: pointer; Count: cardinal): boolean; override;
  end;

  /// AEAD combination of AES with Cipher feedback (CFB) and 256-bit MAC
  // - this class will use AES-NI and CRC32C hardware instructions, if available
  // - expect IV to be set before process, or IVAtBeginning=true
  TAESCFBCRC = class(TAESAbstractAEAD)
  public
    /// perform the AES cypher in the CFB mode, and compute a 256-bit MAC
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CFB mode, and compute 256-bit MAC
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// AEAD combination of AES with Output feedback (OFB) and 256-bit MAC
  // - this class will use AES-NI and CRC32C hardware instructions, if available
  // - expect IV to be set before process, or IVAtBeginning=true
  TAESOFBCRC = class(TAESAbstractAEAD)
  public
    /// perform the AES cypher in the OFB mode, and compute a 256-bit MAC
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the OFB mode, and compute a 256-bit MAC
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES-GCM cypher/uncypher with built-in authentication
  // - implements AEAD (authenticated-encryption with associated-data) methods
  // like MACEncrypt/MACCheckError
  // - this class will use AES-NI hardware instructions, if available
  TAESGCM = class(TAESAbstract)
  protected
    fAES: TAESGCMEngine;
    fContext: (ctxNone,ctxEncrypt,ctxDecrypt); // used to call AES.Reset()
  public
    /// Initialize the AES-GCM context for cypher
    // - first method to call before using this class
    // - KeySize is in bits, i.e. 128,192,256
    constructor Create(const aKey; aKeySize: cardinal); override;
    /// creates a new instance with the very same values
    // - by design, our classes will use TAESGCMEngine stateless context, so
    // this method will just copy the current fields to a new instance,
    // by-passing the key creation step
    function Clone: TAESAbstract; override;
    /// release the used instance memory and resources
    // - also fill the internal TAES instance with zeros, for safety
    destructor Destroy; override;
    /// perform the AES-GCM cypher and authentication
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher and authentication
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// prepare the AES-GCM process before Encrypt/Decrypt is called
    // - aKey is not used: AES-GCM has its own nonce setting algorithm, and
    // the IV will be set from random value by EncryptPKCS7()
    // - will just include any supplied associated data to the GMAC tag
    function MACSetNonce(const aKey: THash256; aAssociated: pointer=nil;
      aAssociatedLen: integer=0): boolean; override;
    /// returns AEAD (authenticated-encryption with associated-data) MAC
    /// - only the lower 128-bit (THash256.Lo) of aCRC is filled with the GMAC
    function MACGetLast(out aCRC: THash256): boolean; override;
    /// validate if an encrypted buffer matches the stored AEAD MAC
    // - since AES-GCM is a one pass process, always assume the content is fine
    // and returns true - we don't know the IV at this time
    function MACCheckError(aEncrypted: pointer; Count: cardinal): boolean; override;
  end;

{$ifdef USE_PROV_RSA_AES}
type
  /// handle AES cypher/uncypher using Windows CryptoAPI and the
  // official Microsoft AES Cryptographic Provider (PROV_RSA_AES)
  // - see @http://msdn.microsoft.com/en-us/library/windows/desktop/aa386979
  // - timing of our optimized asm versions, for small (<=8KB) block processing
  // (similar to standard web pages or most typical JSON/XML content),
  // benchmarked on a Core i7 notebook and compiled as Win32 platform:
  // ! AES128 - ECB:79.33ms CBC:83.37ms CFB:80.75ms OFB:78.98ms CTR:80.45ms
  // ! AES192 - ECB:91.16ms CBC:96.06ms CFB:96.45ms OFB:92.12ms CTR:93.38ms
  // ! AES256 - ECB:103.22ms CBC:119.14ms CFB:111.59ms OFB:107.00ms CTR:110.13ms
  // - timing of the same process, using CryptoAPI official PROV_RSA_AES provider:
  // ! AES128 - ECB_API:102.88ms CBC_API:124.91ms
  // ! AES192 - ECB_API:115.75ms CBC_API:129.95ms
  // ! AES256 - ECB_API:139.50ms CBC_API:154.02ms
  // - but the CryptoAPI does not supports AES-NI, whereas our classes handle it,
  // with a huge speed benefit
  // - under Win64, the official CryptoAPI is faster than our PUREPASCAL version,
  // and the Win32 version of CryptoAPI itself, but slower than our AES-NI code
  // ! AES128 - ECB:107.95ms CBC:112.65ms CFB:109.62ms OFB:107.23ms CTR:109.42ms
  // ! AES192 - ECB:130.30ms CBC:133.04ms CFB:128.78ms OFB:127.25ms CTR:130.22ms
  // ! AES256 - ECB:145.33ms CBC:147.01ms CFB:148.36ms OFB:145.96ms CTR:149.67ms
  // ! AES128 - ECB_API:89.64ms CBC_API:100.84ms
  // ! AES192 - ECB_API:99.05ms CBC_API:105.85ms
  // ! AES256 - ECB_API:107.11ms CBC_API:118.04ms
  // - in practice, you could forget about using the CryptoAPI, unless you are
  // required to do so, for legal/corporate reasons
  TAESAbstract_API = class(TAESAbstract)
  protected
    fKeyHeader: packed record
      bType: byte;
      bVersion: byte;
      reserved: word;
      aiKeyAlg: cardinal;
      dwKeyLength: cardinal;
    end;
    fKeyHeaderKey: TAESKey; // should be just after fKeyHeader record
    fKeyCryptoAPI: pointer;
    fInternalMode: cardinal;
    procedure InternalSetMode; virtual; abstract;
    procedure EncryptDecrypt(BufIn, BufOut: pointer; Count: cardinal; DoEncrypt: boolean);
  public
    /// Initialize AES context for cypher
    // - first method to call before using this class
    // - KeySize is in bits, i.e. 128,192,256
    constructor Create(const aKey; aKeySize: cardinal); override;
    /// release the AES execution context
    destructor Destroy; override;
    /// perform the AES cypher in the ECB mode
    // - if Count is not a multiple of a 16 bytes block, the IV will be used
    // to XOR the trailing bytes - so it won't be compatible with our
    // TAESAbstractSyn classes: you should better use PKC7 padding instead
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the ECB mode
    // - if Count is not a multiple of a 16 bytes block, the IV will be used
    // to XOR the trailing bytes - so it won't be compatible with our
    // TAESAbstractSyn classes: you should better use PKC7 padding instead
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher without chaining (ECB) using Windows CryptoAPI
  TAESECB_API = class(TAESAbstract_API)
  protected
    /// will set fInternalMode := CRYPT_MODE_ECB
    procedure InternalSetMode; override;
  end;

  /// handle AES cypher/uncypher Cipher-block chaining (CBC) using Windows CryptoAPI
  TAESCBC_API = class(TAESAbstract_API)
  protected
    /// will set fInternalMode := CRYPT_MODE_CBC
    procedure InternalSetMode; override;
  end;

  /// handle AES cypher/uncypher Cipher feedback (CFB) using Windows CryptoAPI
  // - NOT TO BE USED: the current PROV_RSA_AES provider does not return
  // expected values for CFB
  TAESCFB_API = class(TAESAbstract_API)
  protected
    /// will set fInternalMode := CRYPT_MODE_CFB
    procedure InternalSetMode; override;
  end;

  /// handle AES cypher/uncypher Output feedback (OFB) using Windows CryptoAPI
  // - NOT TO BE USED: the current PROV_RSA_AES provider does not implement
  // this mode, and returns a NTE_BAD_ALGID error
  TAESOFB_API = class(TAESAbstract_API)
  protected
    /// will set fInternalMode := CRYPT_MODE_OFB
    procedure InternalSetMode; override;
  end;

{$endif USE_PROV_RSA_AES}

var
  /// 128-bit random AES-128 entropy key for TAESAbstract.IVReplayAttackCheck
  // - as used internally by AESIVCtrEncryptDecrypt() function
  // - you may customize this secret for your own project, but be aware that
  // it will affect all TAESAbstract instances, so should match on all ends
  AESIVCTR_KEY: TBlock128 = (
    $ce5d5e3e, $26506c65, $568e0092, $12cce480);

/// global shared function which may encrypt or decrypt any 128-bit block
// using AES-128 and the global AESIVCTR_KEY
procedure AESIVCtrEncryptDecrypt(const BI; var BO; DoEncrypt: boolean);

type
  /// thread-safe class containing a TAES encryption/decryption engine
  TAESLocked = class(TSynPersistentLock)
  protected
    fAES: TAES;
  public
    /// finalize all used memory and resources
    destructor Destroy; override;
  end;

  /// cryptographic pseudorandom number generator (CSPRNG) based on AES-256
  // - use as a shared instance via TAESPRNG.Fill() overloaded class methods
  // - this class is able to generate some random output by encrypting successive
  // values of a counter with AES-256 and a secret key
  // - this internal secret key is generated from PBKDF2 derivation of OS-supplied
  // entropy using HMAC over SHA-512
  // - by design, such a PRNG is as good as the cypher used - for reference, see
  // https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator
  // - it would use fast hardware AES-NI or Padlock opcodes, if available
  TAESPRNG = class(TAESLocked)
  protected
    fCTR: THash128Rec; // we use a litle-endian CTR
    fBytesSinceSeed: integer;
    fSeedAfterBytes: integer;
    fAESKeySize: integer;
    fSeedPBKDF2Rounds: cardinal;
    fTotalBytes: QWord;
    procedure IncrementCTR; {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize the internal secret key, using Operating System entropy
    // - entropy is gathered from the OS, using GetEntropy() method
    // - you can specify how many PBKDF2_HMAC_SHA512 rounds are applied to the
    // OS-gathered entropy - the higher, the better, but also the slower
    // - internal private key would be re-seeded after ReseedAfterBytes
    // bytes (1MB by default) are generated, using GetEntropy()
    // - by default, AES-256 will be used, unless AESKeySize is set to 128,
    // which may be slightly faster (especially if AES-NI is not available)
    constructor Create(PBKDF2Rounds: integer = 16;
      ReseedAfterBytes: integer = 1024*1024; AESKeySize: integer = 256); reintroduce; virtual;
    /// fill a TAESBlock with some pseudorandom data
    // - could be used e.g. to compute an AES Initialization Vector (IV)
    // - this method is thread-safe
    procedure FillRandom(out Block: TAESBlock); overload; virtual;
    /// fill a 256-bit buffer with some pseudorandom data
    // - this method is thread-safe
    procedure FillRandom(out Buffer: THash256); overload;
    /// fill a binary buffer with some pseudorandom data
    // - this method is thread-safe
    procedure FillRandom(Buffer: pointer; Len: integer); overload; virtual;
    /// returns a binary buffer filled with some pseudorandom data
    // - this method is thread-safe
    function FillRandom(Len: integer): RawByteString; overload;
    /// returns a binary buffer filled with some pseudorandom data
    // - this method is thread-safe
    function FillRandomBytes(Len: integer): TBytes;
    /// returns an hexa-encoded binary buffer filled with some pseudorandom data
    // - this method is thread-safe
    function FillRandomHex(Len: integer): RawUTF8;
    /// returns a 32-bit unsigned random number
    function Random32: cardinal; overload;
    /// returns a 32-bit unsigned random number, with a maximum value
    function Random32(max: cardinal): cardinal; overload;
    /// returns a 64-bit unsigned random number
    function Random64: QWord;
    /// returns a floating-point random number in range [0..1]
    function RandomExt: TSynExtended;
    /// returns a 64-bit floating-point random number in range [0..1]
    function RandomDouble: double;
    /// computes a random ASCII password
    // - will contain uppercase/lower letters, digits and $.:()?%!-+*/@#
    // excluding ;,= to allow direct use in CSV content
    function RandomPassword(Len: integer): RawUTF8;
    /// would force the internal generator to re-seed its private key
    // - avoid potential attacks on backward or forward security
    // - would be called by FillRandom() methods, according to SeedAfterBytes
    // - this method is thread-safe
    procedure Seed; virtual;
    /// retrieve some entropy bytes from the Operating System
    // - entropy comes from CryptGenRandom API on Windows, and /dev/urandom or
    // /dev/random on Linux/POSIX
    // - this system-supplied entropy is then XORed with the output of a SHA-3
    // cryptographic SHAKE-256 generator in XOF mode, of several entropy sources
    // (timestamp, thread and system information, SynCommons.Random32 function)
    // unless SystemOnly is TRUE
    // - depending on the system, entropy may not be true randomness: if you need
    // some truly random values, use TAESPRNG.Main.FillRandom() or TAESPRNG.Fill()
    // methods, NOT this class function (which will be much slower, BTW)
    class function GetEntropy(Len: integer; SystemOnly: boolean=false): RawByteString; virtual;
    /// returns a shared instance of a TAESPRNG instance
    // - if you need to generate some random content, just call the
    // TAESPRNG.Main.FillRandom() overloaded methods, or directly TAESPRNG.Fill()
    class function Main: TAESPRNG;
      {$ifdef HASINLINE}inline;{$endif}
    /// just a wrapper around TAESPRNG.Main.FillRandom() function
    // - this method is thread-safe, but you may use your own TAESPRNG instance
    // if you need some custom entropy level
    class procedure Fill(Buffer: pointer; Len: integer); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// just a wrapper around TAESPRNG.Main.FillRandom() function
    // - this method is thread-safe, but you may use your own TAESPRNG instance
    // if you need some custom entropy level
    class procedure Fill(out Block: TAESBlock); overload;
    /// just a wrapper around TAESPRNG.Main.FillRandom() function
    // - this method is thread-safe, but you may use your own TAESPRNG instance
    // if you need some custom entropy level
    class procedure Fill(out Block: THash256); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// just a wrapper around TAESPRNG.Main.FillRandom() function
    // - this method is thread-safe, but you may use your own TAESPRNG instance
    // if you need some custom entropy level
    class function Fill(Len: integer): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// just a wrapper around TAESPRNG.Main.FillRandomBytes() function
    // - this method is thread-safe, but you may use your own TAESPRNG instance
    // if you need some custom entropy level
    class function Bytes(Len: integer): TBytes;
      {$ifdef HASINLINE}inline;{$endif}
    /// create an anti-forensic representation of a key for safe storage
    // - a binary buffer will be split into StripesCount items, ready to be
    // saved on disk; returned length is BufferBytes*(StripesCount+1) bytes
    // - AFSplit supports secure data destruction crucial for secure on-disk
    // key management. The key idea is to bloat information and therefore
    // improve the chance of destroying a single bit of it. The information
    // is bloated in such a way, that a single missing bit causes the original
    // information become unrecoverable.
    // - this implementation uses SHA-256 as diffusion element, and the current
    // TAESPRNG instance to gather randomness
    // - for reference, see TKS1 as used for LUKS and defined in
    // @https://gitlab.com/cryptsetup/cryptsetup/wikis/TKS1-draft.pdf
    function AFSplit(const Buffer; BufferBytes, StripesCount: integer): RawByteString; overload;
    /// create an anti-forensic representation of a key for safe storage
    // - a binary buffer will be split into StripesCount items, ready to be
    // saved on disk; returned length is BufferBytes*(StripesCount+1) bytes
    // - jsut a wrapper around the other overloaded AFSplit() funtion
    function AFSplit(const Buffer: RawByteString; StripesCount: integer): RawByteString; overload;
    /// retrieve a key from its anti-forensic representation
    // - is the reverse function of AFSplit() method
    // - returns TRUE if the input buffer matches BufferBytes value
    class function AFUnsplit(const Split: RawByteString;
      out Buffer; BufferBytes: integer): boolean; overload;
    /// retrieve a key from its anti-forensic representation
    // - is the reverse function of AFSplit() method
    // - returns the un-splitted binary content
    // - returns '' if StripesCount is incorrect
    class function AFUnsplit(const Split: RawByteString;
      StripesCount: integer): RawByteString; overload;
    /// after how many generated bytes Seed method would be called
    // - default is 1 MB
    property SeedAfterBytes: integer read fSeedAfterBytes;
    /// how many PBKDF2_HMAC_SHA512 count is applied by Seed to the entropy
    // - default is 16 rounds, which is more than enough for entropy gathering,
    // since GetEntropy output comes from a SHAKE-256 generator in XOF mode
    property SeedPBKDF2Rounds: cardinal read fSeedPBKDF2Rounds;
    /// how many bits (128 or 256 - which is the default) are used for the AES
    property AESKeySize: integer read fAESKeySize;
    /// how many bytes this generator did compute
    property TotalBytes: QWord read fTotalBytes;
  end;

  /// TAESPRNG-compatible class using Operating System pseudorandom source
  // - may be used instead of TAESPRNG if a "standard" generator is required -
  // you could override MainAESPRNG global variable
  // - will call /dev/urandom under POSIX, and CryptGenRandom API on Windows
  // - warning: may block on some BSD flavors, depending on /dev/urandom
  // - from the cryptographic point of view, our TAESPRNG class doesn't suffer
  // from the "black-box" approach of Windows, give consistent randomness
  // over all supported cross-platform, and is indubitably faster
  TAESPRNGSystem = class(TAESPRNG)
  public
    /// initialize the Operating System PRNG
    constructor Create; reintroduce; virtual;
    /// fill a TAESBlock with some pseudorandom data
    // - this method is thread-safe
    procedure FillRandom(out Block: TAESBlock); override;
    /// fill a binary buffer with some pseudorandom data
    // - this method is thread-safe
    procedure FillRandom(Buffer: pointer; Len: integer); override;
    /// called to force the internal generator to re-seed its private key
    // - won't do anything for the Operating System pseudorandom source
    procedure Seed; override;
  end;

var
  /// the shared TAESPRNG instance returned by TAESPRNG.Main class function
  // - you may override this to a customized instance, e.g. if you expect
  // a specific random generator to be used, like TAESPRNGSystem
  // - all TAESPRNG.Fill() class functions will use this instance
  MainAESPRNG: TAESPRNG;

{$ifdef HASINLINE}
/// defined globally to initialize MainAESPRNG for inlining TAESPRNG.Main
procedure SetMainAESPRNG;
{$endif}

/// low-level function returning some random binary using standard API
// - will call /dev/urandom or /dev/random under POSIX, and CryptGenRandom API
// on Windows, and fallback to SynCommons.FillRandom if the system API failed
// or for padding if more than 32 bytes is retrieved from /dev/urandom
// - you should not have to call this procedure, but faster and safer TAESPRNG
procedure FillSystemRandom(Buffer: PByteArray; Len: integer; AllowBlocking: boolean);

/// low-level function able to derivate a 0..1 floating-point from 128-bit of data
// - used e.g. by TAESPRNG.RandomExt
function Hash128ToExt({$ifdef FPC}constref{$else}const{$endif} r: THash128): TSynExtended;
  {$ifdef FPC}inline;{$endif}

/// low-level function able to derivate a 0..1 64-bit floating-point from 128-bit of data
// - used e.g. by TAESPRNG.RandomDouble
function Hash128ToDouble({$ifdef FPC}constref{$else}const{$endif} r: THash128): double;
  {$ifdef FPC}inline;{$endif}

/// low-level function able to derivate a 0..1 32-bit floating-point from 128-bit of data
function Hash128ToSingle({$ifdef FPC}constref{$else}const{$endif} r: THash128): double;
  {$ifdef FPC}inline;{$endif}

type
  PSHA1Digest = ^TSHA1Digest;
  /// 160 bits memory block for SHA-1 hash digest storage
  TSHA1Digest = THash160;

  PSHA1 = ^TSHA1;
  /// handle SHA-1 hashing
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance, e.g. for THMAC_SHA1
  // - see TSynHasher if you expect to support more than one algorithm at runtime
  {$ifdef USERECORDWITHMETHODS}TSHA1 = record
    {$else}TSHA1 = object{$endif}
  private
    Context: packed array[1..SHAContextSize] of byte;
  public
    /// initialize SHA-1 context for hashing
    procedure Init;
    /// update the SHA-1 context with some data
    procedure Update(Buffer: pointer; Len: integer); overload;
    /// update the SHA-1 context with some data
    procedure Update(const Buffer: RawByteString); overload;
    /// finalize and compute the resulting SHA-1 hash Digest of all data
    // affected to Update() method
    // - will also call Init to reset all internal temporary context, for safety
    procedure Final(out Digest: TSHA1Digest; NoInit: boolean=false); overload;
    /// finalize and compute the resulting SHA-1 hash Digest of all data
    // affected to Update() method
    // - will also call Init to reset all internal temporary context, for safety
    function Final(NoInit: boolean=false): TSHA1Digest; overload; {$ifdef HASINLINE}inline;{$endif}
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    // - only Full() is Padlock-implemented - use this rather than Update()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TSHA1Digest);
  end;

  PSHA256Digest = ^TSHA256Digest;
  /// 256 bits (32 bytes) memory block for SHA-256 hash digest storage
  TSHA256Digest = THash256;

  PSHA256 = ^TSHA256;
  /// handle SHA-256 hashing
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance, e.g. for THMAC_SHA256
  // - see TSynHasher if you expect to support more than one algorithm at runtime
  {$ifdef USERECORDWITHMETHODS}TSHA256 = record
    {$else}TSHA256 = object{$endif}
  private
    Context: packed array[1..SHAContextSize] of byte;
  public
    /// initialize SHA-256 context for hashing
    procedure Init;
    /// update the SHA-256 context with some data
    procedure Update(Buffer: pointer; Len: integer); overload;
    /// update the SHA-256 context with some data
    procedure Update(const Buffer: RawByteString); overload;
    /// finalize and compute the resulting SHA-256 hash Digest of all data
    // affected to Update() method
    procedure Final(out Digest: TSHA256Digest; NoInit: boolean=false); overload;
    /// finalize and compute the resulting SHA-256 hash Digest of all data
    // affected to Update() method
    function Final(NoInit: boolean=false): TSHA256Digest; overload; {$ifdef HASINLINE}inline;{$endif}
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    // - only Full() is Padlock-implemented - use this rather than Update()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TSHA256Digest);
  end;

  TSHA512Hash = record a, b, c, d, e, f, g, h: QWord; end;

  PSHA384Digest = ^TSHA384Digest;
  /// 384 bits (64 bytes) memory block for SHA-384 hash digest storage
  TSHA384Digest = THash384;

  /// handle SHA-384 hashing
  // - it is in fact a TSHA512 truncated hash, with other initial hash values
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance, e.g. for THMAC_SHA384
  // - see TSynHasher if you expect to support more than one algorithm at runtime
  {$ifdef USERECORDWITHMETHODS}TSHA384 = record
    {$else}TSHA384 = object{$endif}
  private
    Hash: TSHA512Hash;
    MLen: QWord;
    Data: array[0..127] of byte;
    Index: integer;
  public
    /// initialize SHA-384 context for hashing
    procedure Init;
    /// update the SHA-384 context with some data
    procedure Update(Buffer: pointer; Len: integer); overload;
    /// update the SHA-384 context with some data
    procedure Update(const Buffer: RawByteString); overload;
    /// finalize and compute the resulting SHA-384 hash Digest of all data
    // affected to Update() method
    // - will also call Init to reset all internal temporary context, for safety
    procedure Final(out Digest: TSHA384Digest; NoInit: boolean=false); overload;
    /// finalize and compute the resulting SHA-384 hash Digest of all data
    // affected to Update() method
    function Final(NoInit: boolean=false): TSHA384Digest; overload; {$ifdef HASINLINE}inline;{$endif}
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TSHA384Digest);
  end;

  /// points to SHA-384 hashing instance
  PSHA384 = ^TSHA384;

  PSHA512Digest = ^TSHA512Digest;
  /// 512 bits (64 bytes) memory block for SHA-512 hash digest storage
  TSHA512Digest = THash512;

  /// handle SHA-512 hashing
  // - by design, this algorithm is expected to be much faster on 64-bit CPU,
  // since all internal process involves QWord - but we included a SSE3 asm
  // optimized version on 32-bit CPU under Windows and Linux, which is almost
  // as fast as on plain x64, and even faster than SHA-256 and SHA-3
  // - under x86/Delphi, plain pascal is 40MB/s, SSE3 asm 180MB/s
  // - on x64, pascal Delphi is 150MB/s, and FPC is 190MB/s (thanks to native
  // RorQWord intrinsic compiler function) - we also included a SSE4 asm version
  // which outperforms other cryptographic hashes to more than 380MB/s
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance, e.g. for THMAC_SHA512
  // - see TSynHasher if you expect to support more than one algorithm at runtime
  {$ifdef USERECORDWITHMETHODS}TSHA512 = record
    {$else}TSHA512 = object{$endif}
  private
    Hash: TSHA512Hash;
    MLen: QWord;
    Data: array[0..127] of byte;
    Index: integer;
  public
    /// initialize SHA-512 context for hashing
    procedure Init;
    /// update the SHA-512 context with some data
    procedure Update(Buffer: pointer; Len: integer); overload;
    /// update the SHA-512 context with some data
    procedure Update(const Buffer: RawByteString); overload;
    /// finalize and compute the resulting SHA-512 hash Digest of all data
    // affected to Update() method
    // - will also call Init to reset all internal temporary context, for safety
    procedure Final(out Digest: TSHA512Digest; NoInit: boolean=false); overload;
    /// finalize and compute the resulting SHA-512 hash Digest of all data
    // affected to Update() method
    function Final(NoInit: boolean=false): TSHA512Digest; overload; {$ifdef HASINLINE}inline;{$endif}
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TSHA512Digest);
  end;

  /// points to SHA-512 hashing instance
  PSHA512 = ^TSHA512;

  /// SHA-3 instances, as defined by NIST Standard for Keccak sponge construction
  TSHA3Algo = (SHA3_224, SHA3_256, SHA3_384, SHA3_512, SHAKE_128, SHAKE_256);

  PSHA3 = ^TSHA3;
  /// handle SHA-3 (Keccak) hashing
  // - Keccak was the winner of the NIST hashing competition for a new hashing
  // algorithm to provide an alternative to SHA-256. It became SHA-3 and was
  // named by NIST a FIPS 180-4, then FIPS 202 hashing standard in 2015
  // - by design, SHA-3 doesn't need to be encapsulated into a HMAC algorithm,
  // since it already includes proper padding, so keys could be concatenated
  // - this implementation is based on Wolfgang Ehrhardt's and Eric Grange's,
  // with our own manually optimized x64 assembly
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance, e.g. after InitCypher
  // - see TSynHasher if you expect to support more than one algorithm at runtime
  {$ifdef USERECORDWITHMETHODS}TSHA3 = record
    {$else}TSHA3 = object{$endif}
  private
    Context: packed array[1..SHA3ContextSize] of byte;
  public
    /// initialize SHA-3 context for hashing
    // - in practice, you may use SHA3_256 or SHA3_512 to return THash256
    // or THash512 digests
    procedure Init(Algo: TSHA3Algo);
    /// update the SHA-3 context with some data
    procedure Update(Buffer: pointer; Len: integer); overload;
    /// update the SHA-3 context with some data
    procedure Update(const Buffer: RawByteString); overload;
    /// finalize and compute the resulting SHA-3 hash 256-bit Digest
    procedure Final(out Digest: THash256; NoInit: boolean=false); overload;
    /// finalize and compute the resulting SHA-3 hash 512-bit Digest
    procedure Final(out Digest: THash512; NoInit: boolean=false); overload;
    /// finalize and compute the resulting SHA-3 hash 256-bit Digest
    function Final256(NoInit: boolean=false): THash256;
    /// finalize and compute the resulting SHA-3 hash 512-bit Digest
    function Final512(NoInit: boolean=false): THash512;
    /// finalize and compute the resulting SHA-3 hash Digest
    // - Digest destination buffer must contain enough bytes
    // - default DigestBits=0 will write the default number of bits to Digest
    // output memory buffer, according to the current TSHA3Algo
    // - you can call this method several times, to use this SHA-3 hasher as
    // "Extendable-Output Function" (XOF), e.g. for stream encryption (ensure
    // NoInit is set to true, to enable recall)
    procedure Final(Digest: pointer; DigestBits: integer=0; NoInit: boolean=false); overload;
    /// compute a SHA-3 hash 256-bit Digest from a buffer, in one call
    // - call Init, then Update(), then Final() using SHA3_256 into a THash256
    procedure Full(Buffer: pointer; Len: integer; out Digest: THash256); overload;
    /// compute a SHA-3 hash 512-bit Digest from a buffer, in one call
    // - call Init, then Update(), then Final() using SHA3_512 into a THash512
    procedure Full(Buffer: pointer; Len: integer; out Digest: THash512); overload;
    /// compute a SHA-3 hash Digest from a buffer, in one call
    // - call Init, then Update(), then Final() using the supplied algorithm
    // - default DigestBits=0 will write the default number of bits to Digest
    // output memory buffer, according to the specified TSHA3Algo
    procedure Full(Algo: TSHA3Algo; Buffer: pointer; Len: integer;
      Digest: pointer; DigestBits: integer=0); overload;
    /// compute a SHA-3 hash hexadecimal Digest from a buffer, in one call
    // - call Init, then Update(), then Final() using the supplied algorithm
    // - default DigestBits=0 will write the default number of bits to Digest
    // output memory buffer, according to the specified TSHA3Algo
    function FullStr(Algo: TSHA3Algo; Buffer: pointer; Len: integer;
      DigestBits: integer=0): RawUTF8;
    /// uses SHA-3 in "Extendable-Output Function" (XOF) to cypher some content
    // - there is no MAC stored in the resulting binary
    // - Source and Dest will have the very same DataLen size in bytes,
    // and Dest will be Source XORed with the XOF output, so encryption and
    // decryption are just obtained by the same symmetric call
    // - in this implementation, Source and Dest should point to two diverse buffers
    // - for safety, the Key should be a secret value, pre-pended with a random
    // salt/IV or a resource-specific identifier (e.g. a record ID or a S/N),
    // to avoid reverse composition of the cypher from known content - note that
    // concatenating keys with SHA-3 is as safe as computing a HMAC for SHA-2
    procedure Cypher(Key, Source, Dest: pointer; KeyLen, DataLen: integer;
      Algo: TSHA3Algo = SHAKE_256); overload;
    /// uses SHA-3 in "Extendable-Output Function" (XOF) to cypher some content
    // - this overloaded function works with RawByteString content
    // - resulting string will have the very same size than the Source
    // - XOF is implemented as a symmetrical algorithm: use this Cypher()
    // method for both encryption and decryption of any buffer
    function Cypher(const Key, Source: RawByteString; Algo: TSHA3Algo = SHAKE_256): RawByteString; overload;
    /// uses SHA-3 in "Extendable-Output Function" (XOF) to cypher some content
    // - prepare the instance to further Cypher() calls
    // - you may reuse the very same TSHA3 instance by copying it to a local
    // variable before calling this method (this copy is thread-safe)
    // - works with RawByteString content
    procedure InitCypher(Key: pointer; KeyLen: integer; Algo: TSHA3Algo = SHAKE_256); overload;
    /// uses SHA-3 in "Extendable-Output Function" (XOF) to cypher some content
    // - prepare the instance to further Cypher() calls
    // - you may reuse the very same TSHA3 instance by copying it to a local
    // variable before calling this method (this copy is thread-safe)
    // - works with RawByteString content
    procedure InitCypher(const Key: RawByteString; Algo: TSHA3Algo = SHAKE_256); overload;
    /// uses SHA-3 in "Extendable-Output Function" (XOF) to cypher some content
    // - this overloaded function expects the instance to have been prepared
    // by previous InitCypher call
    // - resulting Dest buffer will have the very same size than the Source
    // - XOF is implemented as a symmetrical algorithm: use this Cypher()
    // method for both encryption and decryption of any buffer
    // - you can call this method several times, to work with a stream buffer;
    // but for safety, you should eventually call Done
    procedure Cypher(Source, Dest: pointer; DataLen: integer); overload;
    /// uses SHA-3 in "Extendable-Output Function" (XOF) to cypher some content
    // - this overloaded function expects the instance to have been prepared
    // by previous InitCypher call
    // - resulting string will have the very same size than the Source
    // - XOF is implemented as a symmetrical algorithm: use this Cypher()
    // method for both encryption and decryption of any buffer
    // - you can call this method several times, to work with a stream buffer;
    // but for safety, you should eventually call Done
    function Cypher(const Source: RawByteString): RawByteString; overload;
    /// returns the algorithm specified at Init()
    function Algorithm: TSHA3Algo;
    /// fill all used memory context with zeros, for safety
    // - is necessary only when NoInit is set to true (e.g. after InitCypher)
    procedure Done;
  end;

  TMD5In = array[0..15] of cardinal;
  PMD5In = ^TMD5In;
  /// 128 bits memory block for MD5 hash digest storage
  TMD5Digest = THash128;
  PMD5Digest = ^TMD5Digest;
  PMD5 = ^TMD5;
  TMD5Buf = TBlock128;

  /// handle MD5 hashing
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance
  // - see TSynHasher if you expect to support more than one algorithm at runtime
  // - even if MD5 is now seldom used, it is still faster than SHA alternatives,
  // when you need a 128-bit cryptographic hash, but can afford some collisions
  // - this implementation has optimized x86 and x64 assembly, for processing
  // around 500MB/s, and a pure-pascal fallback code on other platforms
  {$ifdef USERECORDWITHMETHODS}TMD5 = record
    {$else}TMD5 = object{$endif}
  private
    in_: TMD5In;
    bytes: array[0..1] of cardinal;
  public
    buf: TMD5Buf;
    /// initialize MD5 context for hashing
    procedure Init;
    /// update the MD5 context with some data
    procedure Update(const buffer; Len: cardinal); overload;
    /// update the MD5 context with some data
    procedure Update(const Buffer: RawByteString); overload;
    /// finalize the MD5 hash process
    // - the resulting hash digest would be stored in buf public variable
    procedure Finalize;
    /// finalize and compute the resulting MD5 hash Digest of all data
    // affected to Update() method
    procedure Final(out result: TMD5Digest); overload;
    /// finalize and compute the resulting MD5 hash Digest of all data
    // affected to Update() method
    function Final: TMD5Digest; overload;
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TMD5Digest);
  end;

  /// handle RC4 encryption/decryption
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance
  // - you can also restore and backup any previous state of the RC4 encryption
  // by copying the whole TRC4 variable into another (stack-allocated) variable
  {$ifdef USERECORDWITHMETHODS}TRC4 = record
    {$else}TRC4 = object{$endif}
  private
    {$ifdef CPUINTEL}
    state: array[byte] of PtrInt; // PtrInt=270MB/s  byte=240MB/s on x86
    {$else}
    state: array[byte] of byte; // on ARM, keep the CPU cache usage low
    {$endif}
    currI, currJ: PtrInt;
  public
    /// initialize the RC4 encryption/decryption
    // - KeyLen is in bytes, and should be within 1..255 range
    procedure Init(const aKey; aKeyLen: integer);
    /// initialize RC4-drop[3072] encryption/decryption after SHA-3 hashing
    // - will use SHAKE-128 generator in XOF mode to generate a 256 bytes key,
    // then drop the first 3072 bytes from the RC4 stream
    // - this initializer is much safer than plain Init, so should be considered
    // for any use on RC4 for new projects - even if AES-NI is 2 times faster,
    // and safer SHAKE-128 operates in XOF mode at a similar speed range
    procedure InitSHA3(const aKey; aKeyLen: integer);
    /// drop the next Count bytes from the RC4 cypher state
    // - may be used in Stream mode, or to initialize in RC4-drop[n] mode
    procedure Drop(Count: cardinal);
    /// perform the RC4 cypher encryption/decryption on a buffer
    // - each call to this method shall be preceeded with an Init() call
    // - RC4 is a symmetrical algorithm: use this Encrypt() method
    // for both encryption and decryption of any buffer
    procedure Encrypt(const BufIn; var BufOut; Count: cardinal);
      {$ifdef HASINLINE}inline;{$endif}
    /// perform the RC4 cypher encryption/decryption on a buffer
    // - each call to this method shall be preceeded with an Init() call
    // - RC4 is a symmetrical algorithm: use this EncryptBuffer() method
    // for both encryption and decryption of any buffer
    procedure EncryptBuffer(BufIn, BufOut: PByte; Count: cardinal);
  end;

{$A-} { packed memory structure }
  /// internal header for storing our AES data with salt and CRC
  // - memory size matches an TAESBlock on purpose, for direct encryption
  {$ifdef USERECORDWITHMETHODS}TAESFullHeader = record
    {$else}TAESFullHeader = object{$endif}
  public
    /// Len before compression (if any)
    OriginalLen,
    /// Len before AES encoding
    SourceLen,
    /// Random Salt for better encryption
    SomeSalt,
    /// CRC from header
    HeaderCheck: cardinal;
    /// computes the Key checksum, using Adler32 algorithm
    function Calc(const Key; KeySize: cardinal): cardinal;
  end;
{$A+}

  PAESFull = ^TAESFull;
  /// AES and XOR encryption object for easy direct memory or stream access
  // - calls internaly TAES objet methods, and handle memory and streams for best speed
  // - a TAESFullHeader is encrypted at the begining, allowing fast Key validation,
  // but the resulting stream is not compatible with raw TAES object
  {$ifdef USERECORDWITHMETHODS}TAESFull = record
    {$else}TAESFull = object{$endif}
  public
    /// header, stored at the beginning of struct -> 16-byte aligned
    Head: TAESFullHeader;
    /// this memory stream is used in case of EncodeDecode(outStream=bOut=nil)
    // method call
    outStreamCreated: TMemoryStream;
    /// main method of AES or XOR cypher/uncypher
    // - return out size, -1 if error on decoding (Key not correct)
    // - valid KeySize: 0=nothing, 32=xor, 128,192,256=AES
    // - if outStream is TMemoryStream -> auto-reserve space (no Realloc:)
    // - for normal usage, you just have to Assign one In and one Out
    // - if outStream AND bOut are both nil, an outStream is created via
    // THeapMemoryStream.Create
    // - if Padlock is used, 16-byte alignment is forced (via tmp buffer if necessary)
    // - if Encrypt -> OriginalLen can be used to store unCompressed Len
    function EncodeDecode(const Key; KeySize, inLen: cardinal; Encrypt: boolean;
      inStream, outStream: TStream; bIn, bOut: pointer; OriginalLen: cardinal=0): integer;
  end;

  /// AES encryption stream
  // - encrypt the Data on the fly, in a compatible way with AES() - last bytes
  // are coded with XOR (not compatible with TAESFull format)
  // - not optimized for small blocks -> ok if used AFTER TBZCompressor/TZipCompressor
  // - warning: Write() will crypt Buffer memory in place -> use AFTER T*Compressor
  TAESWriteStream = class(TStream)
  public
    Adler, // CRC from uncrypted compressed data - for Key check
    DestSize: cardinal;
  private
    Dest: TStream;
    Buf: TAESBlock; // very small buffer for remainging 0..15 bytes
    BufCount: integer; // number of pending bytes (0..15) in Buf
    AES: TAES;
    NoCrypt: boolean; // if KeySize=0
  public
    /// initialize the AES encryption stream for an output stream (e.g.
    // a TMemoryStream or a TFileStream)
    constructor Create(outStream: TStream; const Key; KeySize: cardinal);
    /// finalize the AES encryption stream
    // - internaly call the Finish method
    destructor Destroy; override;
    /// read some data is not allowed -> this method will raise an exception on call
    function Read(var Buffer; Count: Longint): Longint; override;
    /// append some data to the outStream, after encryption
    function Write(const Buffer; Count: Longint): Longint; override;
    /// read some data is not allowed -> this method will raise an exception on call
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    /// write pending data
    // - should always be called before closeing the outStream (some data may
    // still be in the internal buffers)
    procedure Finish;
  end;

/// direct MD5 hash calculation of some data
function MD5Buf(const Buffer; Len: Cardinal): TMD5Digest;

/// direct MD5 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function MD5(const s: RawByteString): RawUTF8;

/// direct SHA-1 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function SHA1(const s: RawByteString): RawUTF8;

/// direct SHA-384 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function SHA384(const s: RawByteString): RawUTF8;

/// direct SHA-512 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function SHA512(const s: RawByteString): RawUTF8;

type
  /// compute the HMAC message authentication code using SHA-1 as hash function
  // - you may use HMAC_SHA1() overloaded functions for one-step process
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance via Compute(), e.g. for fast PBKDF2
  {$ifdef USERECORDWITHMETHODS}THMAC_SHA1 = record
    {$else}THMAC_SHA1 = object{$endif}
  private
    sha: TSHA1;
    step7data: THash512Rec;
  public
    /// prepare the HMAC authentication with the supplied key
    // - content of this record is stateless, so you can prepare a HMAC for a
    // key using Init, then copy this THMAC_SHA1 instance to a local variable,
    // and use this local thread-safe copy for actual HMAC computing
    procedure Init(key: pointer; keylen: integer);
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(msg: pointer; msglen: integer);
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: TSHA1Digest; NoInit: boolean=false); overload;
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: RawUTF8; NoInit: boolean=false); overload;
    /// computes the HMAC of the supplied message according to the key
    // - expects a previous call on Init() to setup the shared key
    // - similar to a single Update(msg,msglen) followed by Done, but re-usable
    // - this method is thread-safe on any shared THMAC_SHA1 instance
    procedure Compute(msg: pointer; msglen: integer; out result: TSHA1Digest);
  end;
  /// points to a HMAC message authentication context using SHA-1
  PHMAC_SHA1 = ^THMAC_SHA1;

/// compute the HMAC message authentication code using SHA-1 as hash function
procedure HMAC_SHA1(const key,msg: RawByteString; out result: TSHA1Digest); overload;

/// compute the HMAC message authentication code using SHA-1 as hash function
procedure HMAC_SHA1(const key: TSHA1Digest; const msg: RawByteString;
  out result: TSHA1Digest); overload;

/// compute the HMAC message authentication code using SHA-1 as hash function
procedure HMAC_SHA1(key,msg: pointer; keylen,msglen: integer;
  out result: TSHA1Digest); overload;

/// compute the PBKDF2 derivation of a password using HMAC over SHA-1
// - this function expect the resulting key length to match SHA-1 digest size
procedure PBKDF2_HMAC_SHA1(const password,salt: RawByteString; count: Integer;
  out result: TSHA1Digest);

type
  /// compute the HMAC message authentication code using SHA-384 as hash function
  // - you may use HMAC_SHA384() overloaded functions for one-step process
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance via Compute(), e.g. for fast PBKDF2
  {$ifdef USERECORDWITHMETHODS}THMAC_SHA384 = record
    {$else}THMAC_SHA384 = object{$endif}
  private
    sha: TSHA384;
    step7data: array[0..31] of cardinal;
  public
    /// prepare the HMAC authentication with the supplied key
    // - content of this record is stateless, so you can prepare a HMAC for a
    // key using Init, then copy this THMAC_SHA384 instance to a local variable,
    // and use this local thread-safe copy for actual HMAC computing
    procedure Init(key: pointer; keylen: integer);
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(msg: pointer; msglen: integer);
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: TSHA384Digest; NoInit: boolean=false); overload;
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: RawUTF8; NoInit: boolean=false); overload;
    /// computes the HMAC of the supplied message according to the key
    // - expects a previous call on Init() to setup the shared key
    // - similar to a single Update(msg,msglen) followed by Done, but re-usable
    // - this method is thread-safe on any shared THMAC_SHA384 instance
    procedure Compute(msg: pointer; msglen: integer; out result: TSHA384Digest);
  end;
  /// points to a HMAC message authentication context using SHA-384
  PHMAC_SHA384 = ^THMAC_SHA384;

/// compute the HMAC message authentication code using SHA-384 as hash function
procedure HMAC_SHA384(const key,msg: RawByteString; out result: TSHA384Digest); overload;

/// compute the HMAC message authentication code using SHA-384 as hash function
procedure HMAC_SHA384(const key: TSHA384Digest; const msg: RawByteString;
  out result: TSHA384Digest); overload;

/// compute the HMAC message authentication code using SHA-384 as hash function
procedure HMAC_SHA384(key,msg: pointer; keylen,msglen: integer;
  out result: TSHA384Digest); overload;

/// compute the PBKDF2 derivation of a password using HMAC over SHA-384
// - this function expect the resulting key length to match SHA-384 digest size
procedure PBKDF2_HMAC_SHA384(const password,salt: RawByteString; count: Integer;
  out result: TSHA384Digest);

type
  /// compute the HMAC message authentication code using SHA-512 as hash function
  // - you may use HMAC_SHA512() overloaded functions for one-step process
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance via Compute(), e.g. for fast PBKDF2
  {$ifdef USERECORDWITHMETHODS}THMAC_SHA512 = record
    {$else}THMAC_SHA512 = object{$endif}
  private
    sha: TSHA512;
    step7data: array[0..31] of cardinal;
  public
    /// prepare the HMAC authentication with the supplied key
    // - content of this record is stateless, so you can prepare a HMAC for a
    // key using Init, then copy this THMAC_SHA512 instance to a local variable,
    // and use this local thread-safe copy for actual HMAC computing
    procedure Init(key: pointer; keylen: integer);
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(msg: pointer; msglen: integer);
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: TSHA512Digest; NoInit: boolean=false); overload;
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: RawUTF8; NoInit: boolean=false); overload;
    /// computes the HMAC of the supplied message according to the key
    // - expects a previous call on Init() to setup the shared key
    // - similar to a single Update(msg,msglen) followed by Done, but re-usable
    // - this method is thread-safe on any shared THMAC_SHA512 instance
    procedure Compute(msg: pointer; msglen: integer; out result: TSHA512Digest);
  end;
  /// points to a HMAC message authentication context using SHA-512
  PHMAC_SHA512 = ^THMAC_SHA512;

/// compute the HMAC message authentication code using SHA-512 as hash function
procedure HMAC_SHA512(const key,msg: RawByteString; out result: TSHA512Digest); overload;

/// compute the HMAC message authentication code using SHA-512 as hash function
procedure HMAC_SHA512(const key: TSHA512Digest; const msg: RawByteString;
  out result: TSHA512Digest); overload;

/// compute the HMAC message authentication code using SHA-512 as hash function
procedure HMAC_SHA512(key,msg: pointer; keylen,msglen: integer;
  out result: TSHA512Digest); overload;

/// compute the PBKDF2 derivation of a password using HMAC over SHA-512
// - this function expect the resulting key length to match SHA-512 digest size
procedure PBKDF2_HMAC_SHA512(const password,salt: RawByteString; count: Integer;
  out result: TSHA512Digest);


/// direct SHA-256 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function SHA256(const s: RawByteString): RawUTF8; overload;

/// direct SHA-256 hash calculation of some binary data
// - result is returned in hexadecimal format
function SHA256(Data: pointer; Len: integer): RawUTF8; overload;

/// direct SHA-256 hash calculation of some binary data
// - result is returned in TSHA256Digest binary format
// - since the result would be stored temporarly in the stack, it may be
// safer to use an explicit TSHA256Digest variable, which would be filled
// with zeros by a ... finally FillZero(
function SHA256Digest(Data: pointer; Len: integer): TSHA256Digest; overload;

/// direct SHA-256 hash calculation of some binary data
// - result is returned in TSHA256Digest binary format
// - since the result would be stored temporarly in the stack, it may be
// safer to use an explicit TSHA256Digest variable, which would be filled
// with zeros by a ... finally FillZero(
function SHA256Digest(const Data: RawByteString): TSHA256Digest; overload;

/// direct SHA-256 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
// - this procedure has a weak password protection: small incoming data
// is append to some salt, in order to have at least a 256 bytes long hash:
// such a feature improve security for small passwords, e.g.
// - note that this algorithm is proprietary, and less secure (and standard)
// than the PBKDF2 algorithm, so is there only for backward compatibility of
// existing code: use PBKDF2_HMAC_SHA256 or similar functions for password
// derivation
procedure SHA256Weak(const s: RawByteString; out Digest: TSHA256Digest);

type
  /// compute the HMAC message authentication code using SHA-256 as hash function
  // - you may use HMAC_SHA256() overloaded functions for one-step process
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance via Compute(), e.g. for fast PBKDF2
  {$ifdef USERECORDWITHMETHODS}THMAC_SHA256 = record
    {$else}THMAC_SHA256 = object{$endif}
  private
    sha: TSha256;
    step7data: THash512Rec;
  public
    /// prepare the HMAC authentication with the supplied key
    // - content of this record is stateless, so you can prepare a HMAC for a
    // key using Init, then copy this THMAC_SHA256 instance to a local variable,
    // and use this local thread-safe copy for actual HMAC computing
    procedure Init(key: pointer; keylen: integer);
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(msg: pointer; msglen: integer); overload;
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(const msg: THash128); overload;
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(const msg: THash256); overload;
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(const msg: RawByteString); overload;
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: TSHA256Digest; NoInit: boolean=false); overload;
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: RawUTF8; NoInit: boolean=false); overload;
    /// computes the HMAC of the supplied message according to the key
    // - expects a previous call on Init() to setup the shared key
    // - similar to a single Update(msg,msglen) followed by Done, but re-usable
    // - this method is thread-safe on any shared THMAC_SHA256 instance
    procedure Compute(msg: pointer; msglen: integer; out result: TSHA256Digest);
  end;
  /// points to a HMAC message authentication context using SHA-256
  PHMAC_SHA256 = ^THMAC_SHA256;

/// compute the HMAC message authentication code using SHA-256 as hash function
procedure HMAC_SHA256(const key,msg: RawByteString; out result: TSHA256Digest); overload;

/// compute the HMAC message authentication code using SHA-256 as hash function
procedure HMAC_SHA256(const key: TSHA256Digest; const msg: RawByteString;
  out result: TSHA256Digest); overload;

/// compute the HMAC message authentication code using SHA-256 as hash function
procedure HMAC_SHA256(key,msg: pointer; keylen,msglen: integer; out result: TSHA256Digest); overload;

/// compute the PBKDF2 derivation of a password using HMAC over SHA-256
// - this function expect the resulting key length to match SHA-256 digest size
procedure PBKDF2_HMAC_SHA256(const password,salt: RawByteString; count: Integer;
  out result: TSHA256Digest; const saltdefault: RawByteString=''); overload;

/// compute the PBKDF2 derivation of a password using HMAC over SHA-256, into
// several 256-bit items, so can be used to return any size of output key
// - this function expect the result array to have the expected output length
// - allows resulting key length to be more than one SHA-256 digest size, e.g.
// to be used for both Encryption and MAC
procedure PBKDF2_HMAC_SHA256(const password,salt: RawByteString; count: Integer;
  var result: THash256DynArray; const saltdefault: RawByteString=''); overload;

/// low-level anti-forensic diffusion of a memory buffer using SHA-256
// - as used by TAESPRNG.AFSplit and TAESPRNG.AFUnSplit
procedure AFDiffusion(buf,rnd: pointer; size: cardinal);


/// direct SHA-3 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
// - default DigestBits=0 will write the default number of bits to Digest
// output memory buffer, according to the specified TSHA3Algo
function SHA3(Algo: TSHA3Algo; const s: RawByteString;
  DigestBits: integer=0): RawUTF8; overload;

/// direct SHA-3 hash calculation of some binary buffer
// - result is returned in hexadecimal format
// - default DigestBits=0 will write the default number of bits to Digest
// output memory buffer, according to the specified TSHA3Algo
function SHA3(Algo: TSHA3Algo; Buffer: pointer; Len: integer;
  DigestBits: integer=0): RawUTF8; overload;

/// safe key derivation using iterated SHA-3 hashing
// - you can use SHA3_224, SHA3_256, SHA3_384, SHA3_512 algorithm to fill
// the result buffer with the default sized derivated key of 224,256,384 or 512
// bits (leaving resultbytes = 0)
// - or you may select SHAKE_128 or SHAKE_256, and specify any custom key size
// in resultbytes (used e.g. by PBKDF2_SHA3_Crypt)
procedure PBKDF2_SHA3(algo: TSHA3Algo; const password,salt: RawByteString;
  count: Integer; result: PByte; resultbytes: integer=0);

/// encryption/decryption of any data using iterated SHA-3 hashing key derivation
// - specified algo is expected to be SHAKE_128 or SHAKE_256
// - expected the supplied data buffer to be small - for bigger content, consider
// using TSHA.Cypher after 256-bit PBKDF2_SHA3 key derivation
procedure PBKDF2_SHA3_Crypt(algo: TSHA3Algo; const password,salt: RawByteString;
  count: Integer; var data: RawByteString);


type
  /// the HMAC/SHA-3 algorithms known by TSynSigner
  TSignAlgo = (
    saSha1, saSha256, saSha384, saSha512,
    saSha3224, saSha3256, saSha3384, saSha3512, saSha3S128, saSha3S256);

  /// JSON-serialization ready object as used by TSynSigner.PBKDF2 overloaded methods
  // - default value for unspecified parameters will be SHAKE_128 with
  // rounds=1000 and a fixed salt
  TSynSignerParams = packed record
    algo: TSignAlgo;
    secret,salt: RawUTF8;
    rounds: integer;
  end;

  /// a generic wrapper object to handle digital HMAC-SHA-2/SHA-3 signatures
  // - used e.g. to implement TJWTSynSignerAbstract
  {$ifdef USERECORDWITHMETHODS}TSynSigner = record
    {$else}TSynSigner = object{$endif}
  private
    ctxt: packed array[1..SHA3ContextSize] of byte; // enough space for all
    fSignatureSize: integer;
    fAlgo: TSignAlgo;
  public
    /// initialize the digital HMAC/SHA-3 signing context with some secret text
    procedure Init(aAlgo: TSignAlgo; const aSecret: RawUTF8); overload;
    /// initialize the digital HMAC/SHA-3 signing context with some secret binary
    procedure Init(aAlgo: TSignAlgo; aSecret: pointer; aSecretLen: integer); overload;
    /// initialize the digital HMAC/SHA-3 signing context with PBKDF2 safe
    // iterative key derivation of a secret salted text
    procedure Init(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
      aSecretPBKDF2Rounds: integer; aPBKDF2Secret: PHash512Rec=nil); overload;
    /// process some message content supplied as memory buffer
    procedure Update(aBuffer: pointer; aLen: integer); overload;
    /// process some message content supplied as string
    procedure Update(const aBuffer: RawByteString); overload; {$ifdef HASINLINE}inline;{$endif}
    /// returns the computed digital signature as lowercase hexadecimal text
    function Final: RawUTF8; overload;
    /// returns the raw computed digital signature
    // - SignatureSize bytes will be written: use Signature.Lo/h0/b3/b accessors
    procedure Final(out aSignature: THash512Rec; aNoInit: boolean=false); overload;
    /// one-step digital signature of a buffer as lowercase hexadecimal string
    function Full(aAlgo: TSignAlgo; const aSecret: RawUTF8;
      aBuffer: Pointer; aLen: integer): RawUTF8; overload;
    /// one-step digital signature of a buffer with PBKDF2 derivation
    function Full(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
      aSecretPBKDF2Rounds: integer; aBuffer: Pointer; aLen: integer): RawUTF8; overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    procedure PBKDF2(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
      aSecretPBKDF2Rounds: integer; out aDerivatedKey: THash512Rec); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    procedure PBKDF2(const aParams: TSynSignerParams; out aDerivatedKey: THash512Rec); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    // - accept as input a TSynSignerParams serialized as JSON object
    procedure PBKDF2(aParamsJSON: PUTF8Char; aParamsJSONLen: integer;
      out aDerivatedKey: THash512Rec; const aDefaultSalt: RawUTF8='I6sWioAidNnhXO9BK';
      aDefaultAlgo: TSignAlgo=saSha3S128); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    // - accept as input a TSynSignerParams serialized as JSON object
    procedure PBKDF2(const aParamsJSON: RawUTF8; out aDerivatedKey: THash512Rec;
      const aDefaultSalt: RawUTF8='I6sWioAidNnhXO9BK'; aDefaultAlgo: TSignAlgo=saSha3S128); overload;
    /// prepare a TAES object with the key derivated via a PBKDF2() call
    // - aDerivatedKey is defined as "var", since it will be zeroed after use
    procedure AssignTo(var aDerivatedKey: THash512Rec; out aAES: TAES; aEncrypt: boolean);
    /// fill the intenral context with zeros, for security
    procedure Done;
    /// the algorithm used for digitial signature
    property Algo: TSignAlgo read fAlgo;
    /// the size, in bytes, of the digital signature of this algorithm
    // - potential values are 20, 28, 32, 48 and 64
    property SignatureSize: integer read fSignatureSize;
  end;
  /// reference to TSynSigner wrapper object
  PSynSigner = ^TSynSigner;

  /// hash algorithms available for HashFile/HashFull functions and TSynHasher object
  THashAlgo = (hfMD5, hfSHA1, hfSHA256, hfSHA384, hfSHA512, hfSHA3_256, hfSHA3_512);
  /// set of algorithms available for HashFile/HashFull functions and TSynHasher object
  THashAlgos = set of THashAlgo;

  /// convenient multi-algorithm hashing wrapper
  // - as used e.g. by HashFile/HashFull functions
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance
  {$ifdef USERECORDWITHMETHODS}TSynHasher = record
    {$else}TSynHasher = object{$endif}
  private
    fAlgo: THashAlgo;
    ctxt: array[1..SHA3ContextSize] of byte; // enough space for all algorithms
  public
    /// initialize the internal hashing structure for a specific algorithm
    // - returns false on unknown/unsupported algorithm
    function Init(aAlgo: THashAlgo): boolean;
    /// hash the supplied memory buffer
    procedure Update(aBuffer: Pointer; aLen: integer); overload;
    /// hash the supplied string content
    procedure Update(const aBuffer: RawByteString); overload; {$ifdef HASINLINE}inline;{$endif}
    /// returns the resulting hash as lowercase hexadecimal string
    function Final: RawUTF8;
    /// one-step hash computation of a buffer as lowercase hexadecimal string
    function Full(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUTF8;
    /// the hash algorithm used by this instance
    property Algo: THashAlgo read fAlgo;
  end;

/// compute the hexadecimal hash of any (big) file
// - using a temporary buffer of 1MB for the sequential reading
function HashFile(const aFileName: TFileName; aAlgo: THashAlgo): RawUTF8; overload;

/// compute the hexadecimal hashe(s) of one file, as external .md5/.sha256/.. files
// - reading the file once in memory, then apply all algorithms on it and
// generate the text hash files in the very same folder
procedure HashFile(const aFileName: TFileName; aAlgos: THashAlgos); overload;

/// one-step hash computation of a buffer as lowercase hexadecimal string
function HashFull(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUTF8;

/// compute the HMAC message authentication code using crc256c as hash function
// - HMAC over a non cryptographic hash function like crc256c is known to be
// safe as MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
// - performs two crc32c hashes, so SSE 4.2 gives more than 2.2 GB/s on a Core i7
procedure HMAC_CRC256C(key,msg: pointer; keylen,msglen: integer; out result: THash256); overload;

/// compute the HMAC message authentication code using crc256c as hash function
// - HMAC over a non cryptographic hash function like crc256c is known to be
// safe as MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
// - performs two crc32c hashes, so SSE 4.2 gives more than 2.2 GB/s on a Core i7
procedure HMAC_CRC256C(const key: THash256; const msg: RawByteString; out result: THash256); overload;

/// compute the HMAC message authentication code using crc256c as hash function
// - HMAC over a non cryptographic hash function like crc256c is known to be
// safe as MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
// - performs two crc32c hashes, so SSE 4.2 gives more than 2.2 GB/s on a Core i7
procedure HMAC_CRC256C(const key,msg: RawByteString; out result: THash256); overload;

type
  /// compute the HMAC message authentication code using crc32c as hash function
  // - HMAC over a non cryptographic hash function like crc32c is known to be a
  // safe enough MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
  // - SSE 4.2 will let MAC be computed at 4 GB/s on a Core i7
  // - you may use HMAC_CRC32C() overloaded functions for one-step process
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance via Compute()
  {$ifdef USERECORDWITHMETHODS}THMAC_CRC32C = record
    {$else}THMAC_CRC32C = object{$endif}
  private
    seed: cardinal;
    step7data: THash512Rec;
  public
    /// prepare the HMAC authentication with the supplied key
    // - consider using Compute to re-use a prepared HMAC instance
    procedure Init(key: pointer; keylen: integer); overload;
    /// prepare the HMAC authentication with the supplied key
    // - consider using Compute to re-use a prepared HMAC instance
    procedure Init(const key: RawByteString); overload;
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(msg: pointer; msglen: integer); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(const msg: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// computes the HMAC of all supplied message according to the key
    function Done(NoInit: boolean=false): cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    /// computes the HMAC of the supplied message according to the key
    // - expects a previous call on Init() to setup the shared key
    // - similar to a single Update(msg,msglen) followed by Done, but re-usable
    // - this method is thread-safe
    function Compute(msg: pointer; msglen: integer): cardinal;
  end;

  /// points to HMAC message authentication code using crc32c as hash function
  PHMAC_CRC32C= ^THMAC_CRC32C;

/// compute the HMAC message authentication code using crc32c as hash function
// - HMAC over a non cryptographic hash function like crc32c is known to be a
// safe enough MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
// - SSE 4.2 will let MAC be computed at 4 GB/s on a Core i7
function HMAC_CRC32C(key,msg: pointer; keylen,msglen: integer): cardinal; overload;

/// compute the HMAC message authentication code using crc32c as hash function
// - HMAC over a non cryptographic hash function like crc32c is known to be a
// safe enough MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
// - SSE 4.2 will let MAC be computed at 4 GB/s on a Core i7
function HMAC_CRC32C(const key: THash256; const msg: RawByteString): cardinal; overload;

/// compute the HMAC message authentication code using crc32c as hash function
// - HMAC over a non cryptographic hash function like crc32c is known to be a
// safe enough MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
// - SSE 4.2 will let MAC be computed at 4 GB/s on a Core i7
function HMAC_CRC32C(const key,msg: RawByteString): cardinal; overload;


/// direct Encrypt/Decrypt of data using the TAES class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
procedure AES(const Key; KeySize: cardinal; buffer: pointer; Len: Integer; Encrypt: boolean); overload;

/// direct Encrypt/Decrypt of data using the TAES class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
procedure AES(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: Integer; Encrypt: boolean); overload;

/// direct Encrypt/Decrypt of data using the TAES class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
function  AES(const Key; KeySize: cardinal; const s: RawByteString; Encrypt: boolean): RawByteString; overload;

/// direct Encrypt/Decrypt of data using the TAES class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
function  AES(const Key; KeySize: cardinal; buffer: pointer; Len: cardinal; Stream: TStream; Encrypt: boolean): boolean; overload;

/// AES and XOR encryption using the TAESFull format
// - outStream will be larger/smaller than Len (full AES encrypted)
// - returns true if OK
function AESFull(const Key; KeySize: cardinal; bIn: pointer; Len: Integer;
  outStream: TStream; Encrypt: boolean; OriginalLen: Cardinal=0): boolean; overload;

/// AES and XOR encryption using the TAESFull format
// - bOut must be at least bIn+32/Encrypt bIn-16/Decrypt
// - returns outLength, -1 if error
function AESFull(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: Integer;
  Encrypt: boolean; OriginalLen: Cardinal=0): integer; overload;

/// AES and XOR decryption check using the TAESFull format
// - return true if begining of buff contains true AESFull encrypted data with this Key
// - if not KeySize in [128,192,256] -> use fast and efficient Xor Cypher
function AESFullKeyOK(const Key; KeySize: cardinal; buff: pointer): boolean;

/// AES encryption using the TAES format with a supplied SHA-256 password
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
procedure AESSHA256(Buffer: pointer; Len: integer; const Password: RawByteString; Encrypt: boolean); overload;

/// AES encryption using the TAES format with a supplied SHA-256 password
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
procedure AESSHA256(bIn, bOut: pointer; Len: integer; const Password: RawByteString; Encrypt: boolean); overload;

/// AES encryption using the TAES format with a supplied SHA-256 password
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
function AESSHA256(const s, Password: RawByteString; Encrypt: boolean): RawByteString; overload;

/// AES encryption using the TAESFull format with a supplied SHA-256 password
// - outStream will be larger/smaller than Len: this is a full AES version with
// a triming TAESFullHeader at the beginning
procedure AESSHA256Full(bIn: pointer; Len: Integer; outStream: TStream; const Password: RawByteString; Encrypt: boolean); overload;

var
  /// salt for CryptDataForCurrentUser function
  // - is filled with some random bytes by default, but you may override
  // it for a set of custom processes calling CryptDataForCurrentUser
  CryptProtectDataEntropy: THash256 = (
    $19,$8E,$BA,$52,$FA,$D6,$56,$99,$7B,$73,$1B,$D0,$8B,$3A,$95,$AB,
    $94,$63,$C2,$C0,$78,$05,$9C,$8B,$85,$B7,$A1,$E3,$ED,$93,$27,$18);

{$ifdef MSWINDOWS}
/// protect some data for the current user, using Windows DPAPI
// - the application can specify a secret salt text, which should reflect the
// current execution context, to ensure nobody could decrypt the data without
// knowing this application-specific AppSecret value
// - will use CryptProtectData DPAPI function call under Windows
// - see https://msdn.microsoft.com/en-us/library/ms995355
// - this function is Windows-only, could be slow, and you don't know which
// algorithm is really used on your system, so using CryptDataForCurrentUser()
// may be a better (and cross-platform) alternative
// - also note that DPAPI has been closely reverse engineered - see e.g.
// https://www.passcape.com/index.php?section=docsys&cmd=details&id=28
function CryptDataForCurrentUserDPAPI(const Data,AppSecret: RawByteString; Encrypt: boolean): RawByteString;
{$endif}

/// protect some data via AES-256-CFB and a secret known by the current user only
// - the application can specify a secret salt text, which should reflect the
// current execution context, to ensure nobody could decrypt the data without
// knowing this application-specific AppSecret value
// - here data is cyphered using a random secret key, stored in a file located in
// ! GetSystemPath(spUserData)+sep+PBKDF2_HMAC_SHA256(CryptProtectDataEntropy,User)
// with sep='_' under Windows, and sep='.syn-' under Linux/Posix
// - under Windows, it will encode the secret file via CryptProtectData DPAPI,
// so has the same security level than plain CryptDataForCurrentUserDPAPI()
// - under Linux/POSIX, access to the $HOME user's .xxxxxxxxxxx secret file with
// chmod 400 is considered to be a safe enough approach
// - this function is up to 100 times faster than CryptDataForCurrentUserDPAPI,
// generates smaller results, and is consistent on all Operating Systems
// - you can use this function over a specified variable, to cypher it in place,
// with try ... finally block to protect memory access of the plain data:
// !  constructor TMyClass.Create;
// !  ...
// !    fSecret := CryptDataForCurrentUser('Some Secret Value','appsalt',true);
// !  ...
// !  procedure TMyClass.DoSomething;
// !  var plain: RawByteString;
// !  begin
// !    plain := CryptDataForCurrentUser(fSecret,'appsalt',false);
// !    try
// !      // here plain = 'Some Secret Value'
// !    finally
// !      FillZero(plain); // safely erase uncyphered content from heap
// !    end;
// !  end;
function CryptDataForCurrentUser(const Data,AppSecret: RawByteString; Encrypt: boolean): RawByteString;


const
  SHA1DIGESTSTRLEN = sizeof(TSHA1Digest)*2;
  SHA256DIGESTSTRLEN = sizeof(TSHA256Digest)*2;
  MD5DIGESTSTRLEN = sizeof(TMD5Digest)*2;

type
  /// 32-characters ASCII string, e.g. as returned by AESBlockToShortString()
  Short32 = string[32];

/// compute the hexadecial representation of an AES 16-byte block
// - returns a stack-allocated short string
function AESBlockToShortString(const block: TAESBlock): short32; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the hexadecial representation of an AES 16-byte block
// - fill a stack-allocated short string
procedure AESBlockToShortString(const block: TAESBlock; out result: short32); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the hexadecial representation of an AES 16-byte block
function AESBlockToString(const block: TAESBlock): RawUTF8;

/// compute the hexadecimal representation of a SHA-1 digest
function SHA1DigestToString(const D: TSHA1Digest): RawUTF8;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the SHA-1 digest from its hexadecimal representation
// - returns true on success (i.e. Source has the expected size and characters)
// - just a wrapper around SynCommons.HexToBin()
function SHA1StringToDigest(const Source: RawUTF8; out Dest: TSHA1Digest): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the hexadecimal representation of a SHA-256 digest
function SHA256DigestToString(const D: TSHA256Digest): RawUTF8;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the SHA-256 digest from its hexadecimal representation
// - returns true on success (i.e. Source has the expected size and characters)
// - just a wrapper around SynCommons.HexToBin()
function SHA256StringToDigest(const Source: RawUTF8; out Dest: TSHA256Digest): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the hexadecimal representation of a SHA-384 digest
function SHA384DigestToString(const D: TSHA384Digest): RawUTF8;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the hexadecimal representation of a SHA-512 digest
function SHA512DigestToString(const D: TSHA512Digest): RawUTF8;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the hexadecimal representation of a MD5 digest
function MD5DigestToString(const D: TMD5Digest): RawUTF8;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the MD5 digest from its hexadecimal representation
// - returns true on success (i.e. Source has the expected size and characters)
// - just a wrapper around SynCommons.HexToBin()
function MD5StringToDigest(const Source: RawUTF8; out Dest: TMD5Digest): boolean;

/// apply the XOR operation to the supplied binary buffers of 16 bytes
procedure XorBlock16(A,B: {$ifdef CPU64}PInt64Array{$else}PCardinalArray{$endif});
  {$ifdef HASINLINE}inline;{$endif} overload;

/// apply the XOR operation to the supplied binary buffers of 16 bytes
procedure XorBlock16(A,B,C: {$ifdef CPU64}PInt64Array{$else}PCardinalArray{$endif});
 {$ifdef HASINLINE}inline;{$endif} overload;

/// compute the HTDigest for a user and a realm, according to a supplied password
// - apache-compatible: 'agent007:download area:8364d0044ef57b3defcfa141e8f77b65'
function htdigest(const user, realm, pass: RawByteString): RawUTF8;

/// self test of Adler32 routines
function Adler32SelfTest: boolean;

/// self test of MD5 routines
function MD5SelfTest: boolean;

/// self test of SHA-1 routines
function SHA1SelfTest: boolean;

/// self test of SHA-256 routines
function SHA256SelfTest: boolean;

/// self test of AES routines
function AESSelfTest(onlytables: Boolean): boolean;

/// self test of RC4 routines
function RC4SelfTest: boolean;

/// entry point of the raw MD5 transform function - may be used for low-level use
procedure RawMd5Compress(var Hash; Data: pointer);

/// entry point of the raw SHA-1 transform function - may be used for low-level use
procedure RawSha1Compress(var Hash; Data: pointer);

/// entry point of the raw SHA-256 transform function - may be used for low-level use
procedure RawSha256Compress(var Hash; Data: pointer);

/// entry point of the raw SHA-512 transform function - may be used for low-level use
procedure RawSha512Compress(var Hash; Data: pointer);

// little endian fast conversion
// - 160 bits = 5 integers
// - use fast bswap asm in x86/x64 mode
procedure bswap160(s,d: PIntegerArray);

// little endian fast conversion
// - 256 bits = 8 integers
// - use fast bswap asm in x86/x64 mode
procedure bswap256(s,d: PIntegerArray);

/// simple Adler32 implementation
// - a bit slower than Adler32Asm() version below, but shorter code size
function Adler32Pas(Adler: cardinal; p: pointer; Count: Integer): cardinal;

/// fast Adler32 implementation
// - 16-bytes-chunck unrolled asm version
function Adler32Asm(Adler: cardinal; p: pointer; Count: Integer): cardinal;
  {$ifdef PUREPASCAL}{$ifdef HASINLINE}inline;{$endif}{$endif}

// - very fast XOR according to Cod - not Compression or Stream compatible
// - used in AESFull() for KeySize=32
procedure XorBlock(p: PIntegerArray; Count, Cod: integer);

/// fast and simple XOR Cypher using Index (=Position in Dest Stream)
// - Compression not compatible with this function: should be applied after
// compress (e.g. as outStream for TAESWriteStream)
// - Stream compatible (with updated Index)
// - used in AES() and TAESWriteStream
procedure XorOffset(P: PByteArray; Index,Count: integer);

/// fast XOR Cypher changing by Count value
// - Compression compatible, since the XOR value is always the same, the
// compression rate will not change a lot
procedure XorConst(p: PIntegerArray; Count: integer);

var
  /// the encryption key used by CompressShaAes() global function
  // - the key is global to the whole process
  // - use CompressShaAesSetKey() procedure to set this Key from text
  CompressShaAesKey: TSHA256Digest;

  /// the AES-256 encoding class used by CompressShaAes() global function
  // - use any of the implementation classes, corresponding to the chaining
  // mode required - TAESECB, TAESCBC, TAESCFB, TAESOFB and TAESCTR classes to
  // handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7-like padding)
  // - set to the secure and efficient CFB mode by default
  CompressShaAesClass: TAESAbstractClass = TAESCFB;

/// set an text-based encryption key for CompressShaAes() global function
// - will compute the key via SHA256Weak() and set CompressShaAesKey
// - the key is global to the whole process
procedure CompressShaAesSetKey(const Key: RawByteString; AesClass: TAESAbstractClass=nil);

/// encrypt data content using the AES-256/CFB algorithm, after SynLZ compression
// - as expected by THttpSocket.RegisterCompress()
// - will return 'synshaaes' as ACCEPT-ENCODING: header parameter
// - will use global CompressShaAesKey / CompressShaAesClass variables to be set
// according to the expected algorithm and Key e.g. via a call to CompressShaAesSetKey()
// - if you want to change the chaining mode, you can customize the global
// CompressShaAesClass variable to the expected TAES* class name
// - will store a hash of both cyphered and clear stream: if the
// data is corrupted during transmission, will instantly return ''
function CompressShaAes(var DataRawByteString; Compress: boolean): AnsiString;

type
  /// possible return codes by IProtocol classes
  TProtocolResult = (sprSuccess,
    sprBadRequest, sprUnsupported, sprUnexpectedAlgorithm,
    sprInvalidCertificate, sprInvalidSignature,
    sprInvalidEphemeralKey, sprInvalidPublicKey, sprInvalidPrivateKey,
    sprInvalidMAC);

  /// perform safe communication after unilateral or mutual authentication
  // - see e.g. TProtocolNone or SynEcc's TECDHEProtocolClient and
  // TECDHEProtocolServer implementation classes
  IProtocol = interface
    ['{91E3CA39-3AE2-44F4-9B8C-673AC37C1D1D}']
    /// initialize the communication by exchanging some client/server information
    // - expects the handshaking messages to be supplied as UTF-8 text, may be as
    // base64-encoded binary - see e.g. TWebSocketProtocolBinary.ProcessHandshake
    // - should return sprUnsupported if the implemented protocol does not
    // expect any handshaking mechanism
    // - returns sprSuccess and set something into OutData, depending on the
    // current step of the handshake
    // - returns an error code otherwise
    function ProcessHandshake(const MsgIn: RawUTF8; out MsgOut: RawUTF8): TProtocolResult;
    /// encrypt a message on one side, ready to be transmitted to the other side
    // - this method should be thread-safe in the implementation class
    procedure Encrypt(const aPlain: RawByteString; out aEncrypted: RawByteString);
    /// decrypt a message on one side, as transmitted from the other side
    // - should return sprSuccess if the
    // - should return sprInvalidMAC in case of wrong aEncrypted input (e.g.
    // packet corruption, MiM or Replay attacks attempts)
    // - this method should be thread-safe in the implementation class
    function Decrypt(const aEncrypted: RawByteString; out aPlain: RawByteString): TProtocolResult;
    /// will create another instance of this communication protocol
    function Clone: IProtocol;
  end;
  /// stores a list of IProtocol instances
  IProtocolDynArray = array of IProtocol;

  /// implements a fake no-encryption protocol
  // - may be used for debugging purposes, or when encryption is not needed
  TProtocolNone = class(TInterfacedObject, IProtocol)
  public
    /// initialize the communication by exchanging some client/server information
    // - this method will return sprUnsupported
    function ProcessHandshake(const MsgIn: RawUTF8; out MsgOut: RawUTF8): TProtocolResult;
    /// encrypt a message on one side, ready to be transmitted to the other side
    // - this method will return the plain text with no actual encryption
    procedure Encrypt(const aPlain: RawByteString; out aEncrypted: RawByteString);
    /// decrypt a message on one side, as transmitted from the other side
    // - this method will return the encrypted text with no actual decryption
    function Decrypt(const aEncrypted: RawByteString; out aPlain: RawByteString): TProtocolResult;
    /// will create another instance of this communication protocol
    function Clone: IProtocol;
  end;

  /// implements a secure protocol using AES encryption
  // - as used e.g. by 'synopsebinary' WebSockets protocol
  // - this class will maintain two TAESAbstract instances, one for encryption
  // and another one for decryption, with PKCS7 padding and no MAC validation
  TProtocolAES = class(TInterfacedObjectLocked, IProtocol)
  protected
    fAES: array[boolean] of TAESAbstract; // [false]=decrypt [true]=encrypt
  public
    /// initialize this encryption protocol with the given AES settings
    constructor Create(aClass: TAESAbstractClass; const aKey; aKeySize: cardinal;
      aIVReplayAttackCheck: TAESIVReplayAttackCheck=repCheckedIfAvailable); reintroduce; virtual;
    /// will create another instance of this communication protocol
    constructor CreateFrom(aAnother: TProtocolAES); reintroduce; virtual;
    /// finalize the encryption
    destructor Destroy; override;
    /// initialize the communication by exchanging some client/server information
    // - this method will return sprUnsupported, since no key negociation is involved
    function ProcessHandshake(const MsgIn: RawUTF8; out MsgOut: RawUTF8): TProtocolResult;
    /// encrypt a message on one side, ready to be transmitted to the other side
    // - this method uses AES encryption and PKCS7 padding
    procedure Encrypt(const aPlain: RawByteString; out aEncrypted: RawByteString);
    /// decrypt a message on one side, as transmitted from the other side
    // - this method uses AES decryption and PKCS7 padding
    function Decrypt(const aEncrypted: RawByteString; out aPlain: RawByteString): TProtocolResult;
    /// will create another instance of this communication protocol
    function Clone: IProtocol;
  end;

  /// class-reference type (metaclass) of an AES secure protocol
  TProtocolAESClass = class of TProtocolAES;

{$ifndef NOVARIANTS}
type
  /// JWT Registered Claims, as defined in RFC 7519
  // - known registered claims have a specific name and behavior, and will be
  // handled automatically by TJWTAbstract
  // - corresponding field names are iss,sub,aud,exp,nbf,iat,jti - as defined
  // in JWT_CLAIMS_TEXT constant
  // - jrcIssuer identifies the server which originated the token, e.g.
  // "iss":"https://example.auth0.com/" when the token comes from Auth0 servers
  // - jrcSubject is the application-specific extent which is protected by this
  // JWT, e.g. an User or Resource ID, e.g. "sub":"auth0|57fe9f1bad961aa242870e"
  // - jrcAudience claims that the token is valid only for one or several
  // resource servers (may be a JSON string or a JSON array of strings), e.g.
  // "aud":["https://myshineyfileserver.sometld"] - TJWTAbstract will check
  // that the supplied "aud" field does match an expected list of identifiers
  // - jrcExpirationTime contains the Unix timestamp in seconds after which
  // the token must not be granted access, e.g. "exp":1477474667
  // - jrcNotBefore contains the Unix timestamp in seconds before which the
  // token must not be granted access, e.g. "nbf":147745438
  // - jrcIssuedAt contains the Unix timestamp in seconds when the token was
  // generated, e.g. "iat":1477438667
  // - jrcJwtID provides a unique identifier for the JWT, to prevent any replay;
  // TJWTAbstract.Compute will set an obfuscated TSynUniqueIdentifierGenerator
  // hexadecimal value
  TJWTClaim = (
    jrcIssuer, jrcSubject, jrcAudience, jrcExpirationTime, jrcNotBefore,
    jrcIssuedAt, jrcJwtID);
  /// set of JWT Registered Claims, as in TJWTAbstract.Claims
  TJWTClaims = set of TJWTClaim;

  /// Exception raised when running JSON Web Tokens
  EJWTException = class(ESynException);

  /// TJWTContent.result codes after TJWTAbstract.Verify method call
  TJWTResult = (jwtValid,
    jwtNoToken, jwtWrongFormat, jwtInvalidAlgorithm, jwtInvalidPayload,
    jwtUnexpectedClaim, jwtMissingClaim, jwtUnknownAudience,
    jwtExpired, jwtNotBeforeFailed, jwtInvalidIssuedAt, jwtInvalidID,
    jwtInvalidSignature);
  //// set of TJWTContent.result codes
  TJWTResults = set of TJWTResult;

  /// JWT decoded content, as processed by TJWTAbstract
  // - optionally cached in memory
  TJWTContent = record
    /// store latest Verify() result
    result: TJWTResult;
    /// set of known/registered claims, as stored in the JWT payload
    claims: TJWTClaims;
    /// match TJWTAbstract.Audience[] indexes for reg[jrcAudience]
    audience: set of 0..15;
    /// known/registered claims UTF-8 values, as stored in the JWT payload
    // - e.g. reg[jrcSubject]='1234567890' and reg[jrcIssuer]='' for
    // $ {"sub": "1234567890","name": "John Doe","admin": true}
    reg: array[TJWTClaim] of RawUTF8;
    /// custom/unregistered claim values, as stored in the JWT payload
    // - registered claims will be available from reg[], not in this field
    // - e.g. data.U['name']='John Doe' and data.B['admin']=true for
    // $ {"sub": "1234567890","name": "John Doe","admin": true}
    // but data.U['sub'] if not defined, and reg[jrcSubject]='1234567890'
    data: TDocVariantData;
  end;
  /// pointer to a JWT decoded content, as processed by TJWTAbstract
  PJWTContent = ^TJWTContent;
  /// used to store a list of JWT decoded content
  // - as used e.g. by TJWTAbstract cache
  TJWTContentDynArray = array of TJWTContent;

  /// available options for TJWTAbstract process
  TJWTOption = (joHeaderParse, joAllowUnexpectedClaims, joAllowUnexpectedAudience,
    joNoJwtIDGenerate, joNoJwtIDCheck, joDoubleInData);
  /// store options for TJWTAbstract process
  TJWTOptions = set of TJWTOption;

  /// abstract parent class for implementing JSON Web Tokens
  // - to represent claims securely between two parties, as defined in industry
  // standard @http://tools.ietf.org/html/rfc7519
  // - you should never use this abstract class directly, but e.g. TJWTHS256,
  // TJWTHS384, TJWTHS512 or TJWTES256 (as defined in SynEcc.pas) inherited classes
  // - for security reasons, one inherited class is implementing a single
  // algorithm, as is very likely to be the case on production: you pickup one
  // "alg", then you stick to it; if your server needs more than one algorithm
  // for compatibility reasons, use a separate key and URI - this design will
  // reduce attack surface, and fully avoid weaknesses as described in
  // @https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries
  // and @http://tools.ietf.org/html/rfc7518#section-8.5
  TJWTAbstract = class(TSynPersistent)
  protected
    fAlgorithm: RawUTF8;
    fHeader: RawUTF8;
    fHeaderB64: RawUTF8;
    fClaims: TJWTClaims;
    fOptions: TJWTOptions;
    fAudience: TRawUTF8DynArray;
    fExpirationSeconds: integer;
    fIDGen: TSynUniqueIdentifierGenerator;
    fCacheTimeoutSeconds: integer;
    fCacheResults: TJWTResults;
    fCache: TSynDictionary;
    procedure SetCacheTimeoutSeconds(value: integer); virtual;
    function PayloadToJSON(const DataNameValue: array of const;
      const Issuer, Subject, Audience: RawUTF8; NotBefore: TDateTime;
      ExpirationMinutes: cardinal): RawUTF8; virtual;
    procedure Parse(const Token: RawUTF8; var JWT: TJWTContent;
      out headpayload: RawUTF8; out signature: RawByteString; excluded: TJWTClaims); virtual;
    function CheckAgainstActualTimestamp(var JWT: TJWTContent): boolean;
    // abstract methods which should be overriden by inherited classes
    function ComputeSignature(const headpayload: RawUTF8): RawUTF8; virtual; abstract;
    procedure CheckSignature(const headpayload: RawUTF8; const signature: RawByteString;
      var JWT: TJWTContent); virtual; abstract;
  public
    /// initialize the JWT processing instance
    // - the supplied set of claims are expected to be defined in the JWT payload
    // - aAudience are the allowed values for the jrcAudience claim
    // - aExpirationMinutes is the deprecation time for the jrcExpirationTime claim
    // - aIDIdentifier and aIDObfuscationKey are passed to a
    // TSynUniqueIdentifierGenerator instance used for jrcJwtID claim
    constructor Create(const aAlgorithm: RawUTF8; aClaims: TJWTClaims;
      const aAudience: array of RawUTF8; aExpirationMinutes: integer;
      aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUTF8); reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// compute a new JWT for a given payload
    // - here the data payload is supplied as Name,Value pairs - by convention,
    // some registered Names (see TJWTClaim) should not be used here, and private
    // claims names are expected to be short (typically 3 chars), or an URI
    // - depending on the instance Claims, you should also specify associated
    // Issuer, Subject, Audience and NotBefore values; expected 'exp', 'nbf',
    // 'iat', 'jti' claims will also be generated and included, if needed
    // - you can override the aExpirationMinutes value as defined in Create()
    // - Audience is usually a single text, serialized as a JSON string, but
    // if the value supplied starts with '[', it is expected to be an array
    // of text values, already serialized as a JSON array of strings
    // - this method is thread-safe
    function Compute(const DataNameValue: array of const; const Issuer: RawUTF8='';
      const Subject: RawUTF8=''; const Audience: RawUTF8=''; NotBefore: TDateTime=0;
      ExpirationMinutes: integer=0; Signature: PRawUTF8=nil): RawUTF8;
    /// compute a HTTP Authorization header containing a JWT for a given payload
    // - just a wrapper around Compute(), returned the HTTP header value:
    // $ Authorization: <HttpAuthorizationHeader>
    // following the expected pattern:
    // $ Authorization: Bearer <Token>
    // - this method is thread-safe
    function ComputeAuthorizationHeader(const DataNameValue: array of const;
      const Issuer: RawUTF8=''; const Subject: RawUTF8=''; const Audience: RawUTF8='';
      NotBefore: TDateTime=0; ExpirationMinutes: integer=0): RawUTF8;
    /// check a JWT value, and its signature
    // - will validate all expected Claims (minus ExcludedClaims optional
    // parameter), and the associated signature
    // - verification state is returned in JWT.result (jwtValid for a valid JWT),
    // together with all parsed payload information
    // - supplied JWT is transmitted e.g. in HTTP header:
    // $ Authorization: Bearer <Token>
    // - this method is thread-safe
    procedure Verify(const Token: RawUTF8; out JWT: TJWTContent;
      ExcludedClaims: TJWTClaims=[]); overload;
    /// check a JWT value, and its signature
    // - will validate all expected Claims, and the associated signature
    // - verification state is returned as function result
    // - supplied JWT is transmitted e.g. in HTTP header:
    // $ Authorization: Bearer <Token>
    // - this method is thread-safe
    function Verify(const Token: RawUTF8): TJWTResult; overload;
    /// check a HTTP Authorization header value as JWT, and its signature
    // - will validate all expected Claims, and the associated signature
    // - verification state is returned in JWT.result (jwtValid for a valid JWT),
    // together with all parsed payload information
    // - expect supplied HttpAuthorizationHeader as transmitted in HTTP header:
    // $ Authorization: <HttpAuthorizationHeader>
    // - this method is thread-safe
    function VerifyAuthorizationHeader(const HttpAuthorizationHeader: RawUTF8;
      out JWT: TJWTContent): boolean; overload;
    /// in-place decoding and quick check of the JWT paylod
    // - it won't check the signature, but the header's algorithm against the
    // class name (use TJWTAbstract class to allow any algorithm)
    // - it will decode the JWT payload and check for its expiration, and some
    // mandatory fied values - you can optionally retrieve the Expiration time,
    // the ending Signature, and/or the Payload decoded as TDocVariant
    // - NotBeforeDelta allows to define some time frame for the "nbf" field
    // - may be used on client side to quickly validate a JWT received from
    // server, without knowing the exact algorithm or secret keys
    class function VerifyPayload(const Token, ExpectedSubject, ExpectedIssuer,
      ExpectedAudience: RawUTF8; Expiration: PUnixTime=nil; Signature: PRawUTF8=nil;
      Payload: PVariant=nil; IgnoreTime: boolean=false; NotBeforeDelta: TUnixTime=15): TJWTResult;
  published
    /// the name of the algorithm used by this instance (e.g. 'HS256')
    property Algorithm: RawUTF8 read fAlgorithm;
    /// allow to tune the Verify and Compute method process
    property Options: TJWTOptions read fOptions write fOptions;
    /// the JWT Registered Claims, as implemented by this instance
    // - Verify() method will ensure all claims are defined in the payload,
    // then fill TJWTContent.reg[] with all corresponding values
    property Claims: TJWTClaims read fClaims;
    /// the period, in seconds, for the "exp" claim
    property ExpirationSeconds: integer read fExpirationSeconds;
    /// the audience string values associated with this instance
    // - will be checked by Verify() method, and set in TJWTContent.audience
    property Audience: TRawUTF8DynArray read fAudience;
    /// delay of optional in-memory cache of Verify() TJWTContent
    // - equals 0 by default, i.e. cache is disabled
    // - may be useful if the signature process is very resource consumming
    // (e.g. for TJWTES256 or even HMAC-SHA-256) - see also CacheResults
    // - each time this property is assigned, internal cache content is flushed
    property CacheTimeoutSeconds: integer read fCacheTimeoutSeconds
      write SetCacheTimeoutSeconds;
    /// which TJWTContent.result should be stored in in-memory cache
    // - default is [jwtValid] but you may also include jwtInvalidSignature
    // if signature checking uses a lot of resources
    // - only used if CacheTimeoutSeconds>0
    property CacheResults: TJWTResults read fCacheResults write fCacheResults;
  end;

  /// class-reference type (metaclass) of a JWT algorithm process
  TJWTAbstractClass = class of TJWTAbstract;

  /// implements JSON Web Tokens using 'none' algorithm
  // - as defined in @http://tools.ietf.org/html/rfc7518 paragraph 3.6
  // - you should never use this weak algorithm in production, unless your
  // communication is already secured by other means, and use JWT as cookies
  TJWTNone = class(TJWTAbstract)
  protected
    function ComputeSignature(const headpayload: RawUTF8): RawUTF8; override;
    procedure CheckSignature(const headpayload: RawUTF8; const signature: RawByteString;
      var JWT: TJWTContent); override;
  public
    /// initialize the JWT processing using the 'none' algorithm
    // - the supplied set of claims are expected to be defined in the JWT payload
    // - aAudience are the allowed values for the jrcAudience claim
    // - aExpirationMinutes is the deprecation time for the jrcExpirationTime claim
    // - aIDIdentifier and aIDObfuscationKey are passed to a
    // TSynUniqueIdentifierGenerator instance used for jrcJwtID claim
    constructor Create(aClaims: TJWTClaims; const aAudience: array of RawUTF8;
      aExpirationMinutes: integer=0; aIDIdentifier: TSynUniqueIdentifierProcess=0;
      aIDObfuscationKey: RawUTF8=''); reintroduce;
  end;

  /// abstract parent of JSON Web Tokens using HMAC-SHA2 or SHA-3 algorithms
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - digital signature will be processed by an internal TSynSigner instance
  // - never use this abstract class, but any inherited class, or
  // JWT_CLASS[].Create to instantiate a JWT process from a given algorithm
  TJWTSynSignerAbstract = class(TJWTAbstract)
  protected
    fSignPrepared: TSynSigner;
    function GetAlgo: TSignAlgo; virtual; abstract;
    function ComputeSignature(const headpayload: RawUTF8): RawUTF8; override;
    procedure CheckSignature(const headpayload: RawUTF8; const signature: RawByteString;
      var JWT: TJWTContent); override;
  public
    /// initialize the JWT processing using SHA3 algorithm
    // - the supplied set of claims are expected to be defined in the JWT payload
    // - the supplied secret text will be used to compute the digital signature,
    // directly if aSecretPBKDF2Rounds=0, or via PBKDF2 iterative key derivation
    // if some number of rounds were specified
    // - aAudience are the allowed values for the jrcAudience claim
    // - aExpirationMinutes is the deprecation time for the jrcExpirationTime claim
    // - aIDIdentifier and aIDObfuscationKey are passed to a
    // TSynUniqueIdentifierGenerator instance used for jrcJwtID claim
    // - optionally return the PBKDF2 derivated key for aSecretPBKDF2Rounds>0
    constructor Create(const aSecret: RawUTF8; aSecretPBKDF2Rounds: integer;
      aClaims: TJWTClaims; const aAudience: array of RawUTF8; aExpirationMinutes: integer=0;
      aIDIdentifier: TSynUniqueIdentifierProcess=0; aIDObfuscationKey: RawUTF8='';
      aPBKDF2Secret: PHash512Rec=nil); reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// the digital signature size, in byte
    property SignatureSize: integer read fSignPrepared.fSignatureSize;
    /// the TSynSigner raw algorithm used for digital signature
    property SignatureAlgo: TSignAlgo read fSignPrepared.fAlgo;
    /// low-level read access to the internal signature structure
    property SignPrepared: TSynSigner read fSignPrepared;
  end;
  /// meta-class for TJWTSynSignerAbstract creations
  TJWTSynSignerAbstractClass = class of TJWTSynSignerAbstract;

  /// implements JSON Web Tokens using 'HS256' (HMAC SHA-256) algorithm
  // - as defined in @http://tools.ietf.org/html/rfc7518 paragraph 3.2
  // - our HMAC SHA-256 implementation used is thread safe, and very fast
  // (x86: 3us, x64: 2.5us) so cache is not needed
  // - resulting signature size will be of 256 bits
  TJWTHS256 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// implements JSON Web Tokens using 'HS384' (HMAC SHA-384) algorithm
  // - as defined in @http://tools.ietf.org/html/rfc7518 paragraph 3.2
  // - our HMAC SHA-384 implementation used is thread safe, and very fast
  // even on x86 (if the CPU supports SSE3 opcodes)
  // - resulting signature size will be of 384 bits
  TJWTHS384 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// implements JSON Web Tokens using 'HS512' (HMAC SHA-512) algorithm
  // - as defined in @http://tools.ietf.org/html/rfc7518 paragraph 3.2
  // - our HMAC SHA-512 implementation used is thread safe, and very fast
  // even on x86 (if the CPU supports SSE3 opcodes)
  // - resulting signature size will be of 512 bits
  TJWTHS512 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-224 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 224 bits
  TJWTS3224 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-256 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 256 bits
  TJWTS3256 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-384 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 384 bits
  TJWTS3384 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-512 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 512 bits
  TJWTS3512 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-SHAKE128 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 256 bits
  TJWTS3S128 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-SHAKE256 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 512 bits
  TJWTS3S256 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

const
  /// the text field names of the registerd claims, as defined by RFC 7519
  // - see TJWTClaim enumeration and TJWTClaims set
  // - RFC standard expects those to be case-sensitive
  JWT_CLAIMS_TEXT: array[TJWTClaim] of RawUTF8 = (
    'iss','sub','aud','exp','nbf','iat','jti');

  /// how TJWTSynSignerAbstract algorithms are identified in the JWT
  // - SHA-1 will fallback to HS256 (since there will never be SHA-1 support)
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  JWT_TEXT: array[TSignAlgo] of RawUTF8 = (
    'HS256','HS256','HS384','HS512','S3224','S3256','S3384','S3512','S3S128','S3S256');

  /// able to instantiate any of the TJWTSynSignerAbstract instance expected
  // - SHA-1 will fallback to TJWTHS256 (since SHA-1 will never be supported)
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // - typical use is the following:
  // ! result := JWT_CLASS[algo].Create(master, round, claims, [], expirationMinutes);
  JWT_CLASS: array[TSignAlgo] of TJWTSynSignerAbstractClass = (
    TJWTHS256, TJWTHS256, TJWTHS384, TJWTHS512,
    TJWTS3224, TJWTS3256, TJWTS3384, TJWTS3512, TJWTS3S128, TJWTS3S256);


function ToText(res: TJWTResult): PShortString; overload;
function ToCaption(res: TJWTResult): string; overload;
function ToText(claim: TJWTClaim): PShortString; overload;
function ToText(claims: TJWTClaims): ShortString; overload;

{$endif NOVARIANTS}

function ToText(chk: TAESIVReplayAttackCheck): PShortString; overload;
function ToText(res: TProtocolResult): PShortString; overload;
function ToText(algo: TSignAlgo): PShortString; overload;
function ToText(algo: THashAlgo): PShortString; overload;
function ToText(algo: TSHA3Algo): PShortString; overload;


implementation

{$ifndef NOVARIANTS}
uses
  Variants;
{$endif}

function ToText(res: TProtocolResult): PShortString;
begin
  result := GetEnumName(TypeInfo(TProtocolResult),ord(res));
end;

function ToText(chk: TAESIVReplayAttackCheck): PShortString;
begin
  result := GetEnumName(TypeInfo(TAESIVReplayAttackCheck),ord(chk));
end;

function ToText(algo: TSignAlgo): PShortString;
begin
  result := GetEnumName(TypeInfo(TSignAlgo),ord(algo));
end;

function ToText(algo: THashAlgo): PShortString;
begin
  result := GetEnumName(TypeInfo(THashAlgo),ord(algo));
end;

function ToText(algo: TSHA3Algo): PShortString;
begin
  result := GetEnumName(TypeInfo(TSHA3Algo),ord(algo));
end;


{$ifdef CPU64}
procedure XorBlock16(A,B: PInt64Array);
begin
  A[0] := A[0] xor B[0];
  A[1] := A[1] xor B[1];
end;

procedure XorBlock16(A,B,C: PInt64Array);
begin
  B[0] := A[0] xor C[0];
  B[1] := A[1] xor C[1];
end;
{$else}
procedure XorBlock16(A,B: PCardinalArray);
begin
  A[0] := A[0] xor B[0];
  A[1] := A[1] xor B[1];
  A[2] := A[2] xor B[2];
  A[3] := A[3] xor B[3];
end;

procedure XorBlock16(A,B,C: PCardinalArray);
begin
  B[0] := A[0] xor C[0];
  B[1] := A[1] xor C[1];
  B[2] := A[2] xor C[2];
  B[3] := A[3] xor C[3];
end;
{$endif}

procedure AESBlockToShortString(const block: TAESBlock; out result: short32);
begin
  result[0] := #32;
  SynCommons.BinToHex(@block,@result[1],16);
end;

function AESBlockToShortString(const block: TAESBlock): short32;
begin
  AESBlockToShortString(block,result);
end;

function AESBlockToString(const block: TAESBlock): RawUTF8;
begin
  FastSetString(result,nil,32);
  SynCommons.BinToHex(@block,pointer(result),16);
end;

function MD5DigestToString(const D: TMD5Digest): RawUTF8;
begin
  BinToHexLower(@D,sizeof(D),result);
end;

function MD5StringToDigest(const Source: RawUTF8; out Dest: TMD5Digest): boolean;
begin
  result := SynCommons.HexToBin(pointer(Source), @Dest, sizeof(Dest));
end;

function SHA1DigestToString(const D: TSHA1Digest): RawUTF8;
begin
  BinToHexLower(@D,sizeof(D),result);
end;

function SHA1StringToDigest(const Source: RawUTF8; out Dest: TSHA1Digest): boolean;
begin
  result := SynCommons.HexToBin(pointer(Source), @Dest, sizeof(Dest));
end;

function SHA256DigestToString(const D: TSHA256Digest): RawUTF8;
begin
  BinToHexLower(@D,sizeof(D),result);
end;

function SHA256StringToDigest(const Source: RawUTF8; out Dest: TSHA256Digest): boolean;
begin
  result := SynCommons.HexToBin(pointer(Source), @Dest, sizeof(Dest));
end;

function SHA512DigestToString(const D: TSHA512Digest): RawUTF8;
begin
  BinToHexLower(@D, sizeof(D), result);
end;

function SHA384DigestToString(const D: TSHA384Digest): RawUTF8;
begin
  BinToHexLower(@D, sizeof(D), result);
end;

{$ifdef USEPADLOCK}

const
  AES_SUCCEEDED = 0;
  KEY_128BITS = 0;
  KEY_192BITS = 1;
  KEY_256BITS = 2;
  ACE_AES_ECB = 0;
  ACE_AES_CBC = 1;

{$ifdef USEPADLOCKDLL}
type
  tpadlock_phe_available = function: boolean; cdecl;
  tpadlock_phe_sha = function(
    buffer: pointer; nbytes: integer; var Digest): integer; cdecl;

  tpadlock_ace_available = function: boolean; cdecl;
  tpadlock_aes_begin = function: pointer; cdecl;
  tpadlock_aes_setkey = function(
    ctx: pointer; const key; key_len: integer): integer; cdecl;
  tpadlock_aes_setmodeiv = function(
    ctx: pointer; mode: integer; var iv): integer; cdecl;
  tpadlock_aes_encrypt = function(
    ctx, bIn, bOut: pointer; nbytes: integer): integer; cdecl;
  tpadlock_aes_decrypt = function(
    ctx, bIn, bOut: pointer; nbytes: integer): integer; cdecl;
  tpadlock_aes_close = function(
    ctx: pointer): integer; cdecl;

var
  padlock_phe_available: tpadlock_phe_available = nil;
  padlock_phe_sha1: tpadlock_phe_sha = nil;
  padlock_phe_sha256: tpadlock_phe_sha = nil;

  padlock_ace_available: tpadlock_ace_available = nil;
  padlock_aes_begin: tpadlock_aes_begin = nil;
  padlock_aes_setkey: tpadlock_aes_setkey = nil;
  padlock_aes_setmodeiv: tpadlock_aes_setmodeiv = nil;
  padlock_aes_encrypt: tpadlock_aes_encrypt = nil;
  padlock_aes_decrypt: tpadlock_aes_decrypt = nil;
  padlock_aes_close: tpadlock_aes_close = nil;

{$ifdef MSWINDOWS}
  PadLockLibHandle: THandle = 0;
{$else} // Linux:
  PadLockLibHandle: HMODULE = 0;
{$endif}


procedure PadlockInit;
begin
{$ifdef MSWINDOWS}
  PadLockLibHandle := LoadLibrary('LibPadlock');
{$else} // Linux:
  PadLockLibHandle := LoadLibrary('libvia_padlock.so');
  if PadLockLibHandle=0 then
    PadLockLibHandle := LoadLibrary('libvia_padlock.so.1.0.0');
{$endif}
  if PadLockLibHandle=0 then
    exit;
  padlock_phe_available := GetProcAddress(PadLockLibHandle,'padlock_phe_available');
  padlock_phe_sha1 := GetProcAddress(PadLockLibHandle,'padlock_phe_sha1');
  padlock_phe_sha256 := GetProcAddress(PadLockLibHandle,'padlock_phe_sha256');
  padlock_ace_available := GetProcAddress(PadLockLibHandle,'padlock_ace_available');
  padlock_aes_begin := GetProcAddress(PadLockLibHandle,'padlock_aes_begin');
  padlock_aes_setkey := GetProcAddress(PadLockLibHandle,'padlock_aes_setkey');
  padlock_aes_setmodeiv := GetProcAddress(PadLockLibHandle,'padlock_aes_setmodeiv');
  padlock_aes_encrypt := GetProcAddress(PadLockLibHandle,'padlock_aes_encrypt');
  padlock_aes_decrypt := GetProcAddress(PadLockLibHandle,'padlock_aes_decrypt');
  padlock_aes_close := GetProcAddress(PadLockLibHandle,'padlock_aes_close');
  if @padlock_phe_available=nil then exit;
  if @padlock_phe_sha1=nil then exit;
  if @padlock_phe_sha256=nil then exit;
  if @padlock_ace_available=nil then exit;
  if @padlock_aes_begin=nil then exit;
  if @padlock_aes_setkey=nil then exit;
  if @padlock_aes_setmodeiv=nil then exit;
  if @padlock_aes_encrypt=nil then exit;
  if @padlock_aes_decrypt=nil then exit;
  if @padlock_aes_close=nil then exit;
  if padlock_phe_available and padlock_ace_available then
    padlock_available := true;
end;
{$else} // not USEPADLOCKDLL:

{$ifdef MSWINDOWS}
{$L padlock.obj}
{$L padlock_sha.obj}
{$L padlock_aes.obj}
{$else}
{$L padlock.o}
{$L padlock_sha.o}
{$L padlock_aes.o}
{$endif}

function memcpy(dest, src: Pointer; count: integer): Pointer; cdecl;
begin
  MoveFast(src^, dest^, count);
  Result := dest;
end;

function memset(dest: Pointer; val: Integer; count: integer): Pointer; cdecl;
begin
  FillcharFast(dest^, count, val);
  Result := dest;
end;

function malloc(size: integer): Pointer; cdecl;
begin
  GetMem(Result, size);
end;

procedure free(pBlock: Pointer); cdecl;
begin
  FreeMem(pBlock);
end;

function printf(format:PAnsiChar; args:array of const): PAnsiChar; cdecl;
begin
  result := format;
  // called on error -> do nothing
end;

{ this .o files have been generated from the sdk sources with
    gcc-2.95 -c -O2 padlock*.c -I../include
}
function padlock_phe_available: boolean; cdecl; external;
function padlock_phe_sha1(buf: pointer; nbytes: integer; var Digest): integer; cdecl; external;
function padlock_phe_sha256(buf: pointer; nbytes: integer; var Digest): integer; cdecl; external;

function padlock_ace_available: boolean; cdecl; external;
function padlock_aes_begin: pointer; cdecl; external;
function padlock_aes_setkey(ctx: pointer; const key; key_len: integer): integer; cdecl; external;
function padlock_aes_setmodeiv(ctx: pointer; mode: integer; var iv): integer; cdecl; external;
function padlock_aes_encrypt(ctx, bIn, bOut: pointer; nbytes: integer): integer; cdecl; external;
function padlock_aes_decrypt(ctx, bIn, bOut: pointer; nbytes: integer): integer; cdecl; external;
function padlock_aes_close(ctx: pointer): integer; cdecl; external;

procedure PadlockInit;
begin
  if padlock_phe_available and padlock_ace_available then
    padlock_available := true;
{$ifdef PADLOCKDEBUG}if padlock_available then writeln('PADLOCK available'); {$endif}
end;
{$endif USEPADLOCKDLL}
{$endif USEPADLOCK}

procedure XorMemoryPtrInt(dest, source: PPtrIntArray; count: integer);
  {$ifdef HASINLINE}inline;{$endif}
{$ifdef FPC}
begin
  while count>0 do begin
    dec(count);
    PPtrInt(dest)^ := PPtrInt(dest)^ xor PPtrInt(source)^;
    inc(PPtrInt(dest));
    inc(PPtrInt(source));
  end;
end;
{$else}
var i: integer;
begin
  for i := 0 to count-1 do
    dest^[i] := dest^[i] xor source^[i];
end;
{$endif}

const
  AESMaxRounds = 14;

type
  TKeyArray = packed array[0..AESMaxRounds] of TAESBlock;

  /// low-level content of TAES.Context (AESContextSize bytes)
  // - is defined privately in the implementation section
  // - don't change the structure below: it is fixed in the asm code
  // -> use PUREPASCAL if you really have to change it
  TAESContext = packed record
    RK: TKeyArray;   // Key (encr. or decr.)
    IV: TAESBlock;   // IV or CTR
    buf: TAESBlock;  // Work buffer
    {$ifdef USEPADLOCK}
    ViaCtx: pointer; // padlock_*() context
    {$endif}
    DoBlock: procedure(const ctxt, source, dest); // main AES function
    {$ifdef USEAESNI32}AesNi32: pointer;{$endif}
    Initialized: boolean;
    Rounds: byte;    // Number of rounds
    KeyBits: word;   // Number of bits in key (128/192/256)
  end;


// helper types for better code generation
type
  TWA4  = TBlock128;     // AES block as array of cardinal
  TAWk  = packed array[0..4*(AESMaxRounds+1)-1] of cardinal; // Key as array of cardinal
  PWA4  = ^TWA4;
  PAWk  = ^TAWk;

const
  RCon: array[0..9] of cardinal = ($01,$02,$04,$08,$10,$20,$40,$80,$1b,$36);

// AES computed tables - don't change the order below!
var
  Td0, Td1, Td2, Td3, Te0, Te1, Te2, Te3: array[byte] of cardinal;
  SBox, InvSBox: array[byte] of byte;
  Xor32Byte: TByteArray absolute Td0;  // 2^13=$2000=8192 bytes of XOR tables ;)

procedure ComputeAesStaticTables;
var i, x,y: byte;
    pow,log: array[byte] of byte;
    c: cardinal;
begin // 835 bytes of code to compute 4.5 KB of tables
  x := 1;
  for i := 0 to 255 do begin
    pow[i] := x;
    log[x] := i;
    if x and $80<>0 then
      x := x xor (x shl 1) xor $1B else
      x := x xor (x shl 1);
  end;
  SBox[0] := $63;
  InvSBox[$63] := 0;
  for i := 1 to 255 do begin
    x := pow[255-log[i]]; y := (x shl 1)+(x shr 7);
    x := x xor y; y := (y shl 1)+(y shr 7);
    x := x xor y; y := (y shl 1)+(y shr 7);
    x := x xor y; y := (y shl 1)+(y shr 7);
    x := x xor y xor $63;
    SBox[i] := x;
    InvSBox[x] := i;
  end;
  for i := 0 to 255 do begin
    x := SBox[i];
    y := x shl 1;
    if x and $80<>0 then
      y := y xor $1B;
    Te0[i] := y+x shl 8+x shl 16+(y xor x)shl 24;
    Te1[i] := Te0[i] shl 8+Te0[i] shr 24;
    Te2[i] := Te1[i] shl 8+Te1[i] shr 24;
    Te3[i] := Te2[i] shl 8+Te2[i] shr 24;
    x := InvSBox[i];
    if x=0 then
      continue;
    c := log[x]; // Td0[c] = Si[c].[0e,09,0d,0b] -> e.g. log[$0e]=223 below
    Td0[i] := pow[(c+223)mod 255]+pow[(c+199)mod 255]shl 8+
        pow[(c+238)mod 255]shl 16+pow[(c+104)mod 255]shl 24;
    Td1[i] := Td0[i] shl 8+Td0[i] shr 24;
    Td2[i] := Td1[i] shl 8+Td1[i] shr 24;
    Td3[i] := Td2[i] shl 8+Td2[i] shr 24;
  end;
end;

type
  TSHAHash  = packed record
    A,B,C,D,E,F,G,H: cardinal; // will use A..E with TSHA1, A..H with TSHA256
  end;

  TSHAContext = packed record
    // Working hash (TSHA256.Init expect this field to be the first)
    Hash: TSHAHash;
    // 64bit msg length
    MLen: QWord;
    // Block buffer
    Buffer: array[0..63] of byte;
    // Index in buffer
    Index : integer;
  end;

{$ifdef CPUINTEL}

{$ifdef CPU32}

procedure bswap256(s,d: PIntegerArray); {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    ebx
        mov     ecx, eax // ecx=s, edx=d
        mov     eax, [ecx]
        mov     ebx, [ecx + 4]
        bswap   eax
        bswap   ebx
        mov     [edx], eax
        mov     [edx + 4], ebx
        mov     eax, [ecx + 8]
        mov     ebx, [ecx + 12]
        bswap   eax
        bswap   ebx
        mov     [edx + 8], eax
        mov     [edx + 12], ebx
        mov     eax, [ecx + 16]
        mov     ebx, [ecx + 20]
        bswap   eax
        bswap   ebx
        mov     [edx + 16], eax
        mov     [edx + 20], ebx
        mov     eax, [ecx + 24]
        mov     ebx, [ecx + 28]
        bswap   eax
        bswap   ebx
        mov     [edx + 24], eax
        mov     [edx + 28], ebx
        pop     ebx
end;

procedure bswap160(s,d: PIntegerArray); {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    ebx
        mov     ecx, eax // ecx=s, edx=d
        mov     eax, [ecx]
        mov     ebx, [ecx + 4]
        bswap   eax
        bswap   ebx
        mov     [edx], eax
        mov     [edx + 4], ebx
        mov     eax, [ecx + 8]
        mov     ebx, [ecx + 12]
        bswap   eax
        bswap   ebx
        mov     [edx + 8], eax
        mov     [edx + 12], ebx
        mov     eax, [ecx + 16]
        bswap   eax
        mov     [edx + 16], eax
        pop     ebx
end;

function gf2_multiply(x,y,m: PtrUInt): PtrUInt; {$ifdef FPC}nostackframe; assembler;{$endif}
asm // eax=x edx=y ecx=m
        push    esi
        push    edi
        push    ebx
        push    ebp
        mov     ebp, 32
        mov     ebx, eax
        and     eax, 1
        cmovne  eax, edx
@s:     mov     esi, eax
        mov     edi, ecx
        shr     esi, 1
        xor     edi, esi
        test    al, 1
        mov     eax, esi
        cmovne  eax, edi
        shr     ebx, 1
        mov     esi, eax
        xor     esi, edx
        test    bl, 1
        cmovne  eax, esi
        dec     ebp
        jne     @s
        pop     ebp
        pop     ebx
        pop     edi
        pop     esi
end;

{$endif CPU32}

{$ifdef CPU64}

procedure bswap256(s,d: PIntegerArray); {$ifdef FPC} nostackframe; assembler;
asm {$else} asm .noframe {$endif}
        mov     eax, dword ptr[s]
        mov     r8d, dword ptr[s + 4]
        mov     r9d, dword ptr[s + 8]
        mov     r10d, dword ptr[s + 12]
        bswap   eax
        bswap   r8d
        bswap   r9d
        bswap   r10d
        mov     dword ptr[d], eax
        mov     dword ptr[d + 4], r8d
        mov     dword ptr[d + 8], r9d
        mov     dword ptr[d + 12], r10d
        mov     eax, dword ptr[s + 16]
        mov     r8d, dword ptr[s + 20]
        mov     r9d, dword ptr[s + 24]
        mov     r10d, dword ptr[s + 28]
        bswap   eax
        bswap   r8d
        bswap   r9d
        bswap   r10d
        mov     dword ptr[d + 16], eax
        mov     dword ptr[d + 20], r8d
        mov     dword ptr[d + 24], r9d
        mov     dword ptr[d + 28], r10d
end;

procedure bswap160(s,d: PIntegerArray); {$ifdef FPC} nostackframe; assembler;
asm {$else} asm .noframe {$endif}
        mov     eax, dword ptr[s]
        mov     r8d, dword ptr[s + 4]
        mov     r9d, dword ptr[s + 8]
        mov     r10d, dword ptr[s + 12]
        bswap   eax
        bswap   r8d
        bswap   r9d
        bswap   r10d
        mov     dword ptr[d], eax
        mov     dword ptr[d + 4], r8d
        mov     dword ptr[d + 8], r9d
        mov     dword ptr[d + 12], r10d
        mov     eax, dword ptr[s + 16]
        bswap   eax
        mov     dword ptr[d + 16], eax
end;

// see http://nicst.de/crc.pdf

function gf2_multiply(x,y,m,bits: PtrUInt): PtrUInt; {$ifdef FPC} nostackframe; assembler;
asm {$else} asm .noframe {$endif}
        mov     rax, x
        and     rax, 1
        cmovne  rax, y
@s:     mov     r10, rax
        mov     r11, m
        shr     r10, 1
        xor     r11, r10
        test    al, 1
        mov     rax, r10
        cmovne  rax, r11
        shr     x, 1
        mov     r10, rax
        xor     r10, y
        {$ifdef win64}
        test    cl, 1
        {$else}
        test    dil, 1
        {$endif}
        cmovne  rax, r10
        dec     bits
        jne     @s
end;

{$endif CPU64}

{$else not CPUINTEL}

procedure bswap256(s,d: PIntegerArray);
begin
  {$ifdef FPC} // use fast platform-specific function
  d[0] := SwapEndian(s[0]);
  d[1] := SwapEndian(s[1]);
  d[2] := SwapEndian(s[2]);
  d[3] := SwapEndian(s[3]);
  d[4] := SwapEndian(s[4]);
  d[5] := SwapEndian(s[5]);
  d[6] := SwapEndian(s[6]);
  d[7] := SwapEndian(s[7]);
  {$else}
  d[0] := bswap32(s[0]);
  d[1] := bswap32(s[1]);
  d[2] := bswap32(s[2]);
  d[3] := bswap32(s[3]);
  d[4] := bswap32(s[4]);
  d[5] := bswap32(s[5]);
  d[6] := bswap32(s[6]);
  d[7] := bswap32(s[7]);
  {$endif FPC}
end;

procedure bswap160(s,d: PIntegerArray);
begin
  {$ifdef FPC} // use fast platform-specific function
  d[0] := SwapEndian(s[0]);
  d[1] := SwapEndian(s[1]);
  d[2] := SwapEndian(s[2]);
  d[3] := SwapEndian(s[3]);
  d[4] := SwapEndian(s[4]);
  {$else}
  d[0] := bswap32(s[0]);
  d[1] := bswap32(s[1]);
  d[2] := bswap32(s[2]);
  d[3] := bswap32(s[3]);
  d[4] := bswap32(s[4]);
  {$endif FPC}
end;

{$endif CPUINTEL}

function SHA256SelfTest: boolean;
function SingleTest(const s: RawByteString; const TDig: TSHA256Digest): boolean;
var SHA: TSHA256;
  Digest: TSHA256Digest;
  i: integer;
begin
  // 1. Hash complete RawByteString
  SHA.Full(pointer(s),length(s),Digest);
  result := IsEqual(Digest,TDig);
  if not result then exit;
  // 2. one update call for all chars
  SHA.Init;
  for i := 1 to length(s) do
    SHA.Update(@s[i],1);
  SHA.Final(Digest);
  result := IsEqual(Digest,TDig);
  // 3. test consistency with Padlock engine down results
{$ifdef USEPADLOCK}
  if not result or not padlock_available then exit;
  padlock_available := false;  // force PadLock engine down
  SHA.Full(pointer(s),length(s),Digest);
  result := IsEqual(Digest,TDig);
{$ifdef PADLOCKDEBUG} write('=padlock '); {$endif}
  padlock_available := true;
{$endif}
end;
var Digest: TSHA256Digest;
const
  D1: TSHA256Digest = ($ba,$78,$16,$bf,$8f,$01,$cf,$ea,$41,$41,$40,$de,$5d,$ae,$22,$23,
     $b0,$03,$61,$a3,$96,$17,$7a,$9c,$b4,$10,$ff,$61,$f2,$00,$15,$ad);
  D2: TSHA256Digest = ($24,$8d,$6a,$61,$d2,$06,$38,$b8,$e5,$c0,$26,$93,$0c,$3e,$60,$39,
     $a3,$3c,$e4,$59,$64,$ff,$21,$67,$f6,$ec,$ed,$d4,$19,$db,$06,$c1);
  D3: TSHA256Digest =
    ($94,$E4,$A9,$D9,$05,$31,$23,$1D,$BE,$D8,$7E,$D2,$E4,$F3,$5E,$4A,
     $0B,$F4,$B3,$BC,$CE,$EB,$17,$16,$D5,$77,$B1,$E0,$8B,$A9,$BA,$A3);
begin
//  result := true; exit;
  result := SingleTest('abc', D1) and
     SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', D2);
  if not result then exit;
  SHA256Weak('lagrangehommage',Digest); // test with len=256>64
  result := IsEqual(Digest,D3);
  {$ifdef CPU64}
  {$ifdef CPUINTEL}
  if cfSSE41 in CpuFeatures then begin
    Exclude(CpuFeatures,cfSSE41);
    result := result and SingleTest('abc', D1) and
       SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', D2);
    Include(CpuFeatures,cfSSE41);
  end;
  {$endif}
  {$endif}
end;

function MD5(const s: RawByteString): RawUTF8;
var MD5: TMD5;
    D: TMD5Digest;
begin
  MD5.Full(pointer(s),Length(s),D);
  result := MD5DigestToString(D);
  FillZero(D);
end;

function SHA1(const s: RawByteString): RawUTF8;
var SHA: TSHA1;
    Digest: TSHA1Digest;
begin
  SHA.Full(pointer(s),length(s),Digest);
  result := SHA1DigestToString(Digest);
  FillZero(Digest);
end;

function SHA384(const s: RawByteString): RawUTF8;
var SHA: TSHA384;
    Digest: TSHA384Digest;
begin
  SHA.Full(pointer(s),length(s),Digest);
  result := SHA384DigestToString(Digest);
  FillZero(Digest);
end;

function SHA512(const s: RawByteString): RawUTF8;
var SHA: TSHA512;
    Digest: TSHA512Digest;
begin
  SHA.Full(pointer(s),length(s),Digest);
  result := SHA512DigestToString(Digest);
  FillZero(Digest);
end;


{ THMAC_SHA1 }

procedure THMAC_SHA1.Init(key: pointer; keylen: integer);
var i: integer;
    k0,k0xorIpad: THash512Rec;
begin
  FillZero(k0.b);
  if keylen>sizeof(k0) then
    sha.Full(key,keylen,k0.b160) else
    MoveFast(key^,k0,keylen);
  for i := 0 to 15 do
    k0xorIpad.c[i] := k0.c[i] xor $36363636;
  for i := 0 to 15 do
    step7data.c[i] := k0.c[i] xor $5c5c5c5c;
  sha.Init;
  sha.Update(@k0xorIpad,sizeof(k0xorIpad));
  FillZero(k0.b);
  FillZero(k0xorIpad.b);
end;

procedure THMAC_SHA1.Update(msg: pointer; msglen: integer);
begin
  sha.Update(msg,msglen);
end;

procedure THMAC_SHA1.Done(out result: TSHA1Digest; NoInit: boolean);
begin
  sha.Final(result);
  sha.Update(@step7data,sizeof(step7data));
  sha.Update(@result,sizeof(result));
  sha.Final(result,NoInit);
  if not NoInit then
    FillZero(step7data.b);
end;

procedure THMAC_SHA1.Done(out result: RawUTF8; NoInit: boolean);
var res: TSHA1Digest;
begin
  Done(res,NoInit);
  result := SHA1DigestToString(res);
  if not NoInit then
    FillZero(res);
end;

procedure THMAC_SHA1.Compute(msg: pointer; msglen: integer; out result: TSHA1Digest);
var temp: THMAC_SHA1;
begin
  temp := self; // thread-safe copy
  temp.Update(msg,msglen);
  temp.Done(result);
end;

procedure HMAC_SHA1(key,msg: pointer; keylen,msglen: integer; out result: TSHA1Digest);
var mac: THMAC_SHA1;
begin
  mac.Init(key,keylen);
  mac.Update(msg,msglen);
  mac.Done(result);
end;

procedure HMAC_SHA1(const key,msg: RawByteString; out result: TSHA1Digest);
begin
  HMAC_SHA1(pointer(key),pointer(msg),length(key),length(msg),result);
end;

procedure HMAC_SHA1(const key: TSHA1Digest; const msg: RawByteString; out result: TSHA1Digest);
begin
  HMAC_SHA1(@key,pointer(msg),sizeof(key),length(msg),result);
end;

procedure PBKDF2_HMAC_SHA1(const password,salt: RawByteString; count: Integer;
  out result: TSHA1Digest);
var i: integer;
    tmp: TSHA1Digest;
    mac: THMAC_SHA1;
    first: THMAC_SHA1;
begin
  HMAC_SHA1(password,salt+#0#0#0#1,result);
  if count<2 then
    exit;
  tmp := result;
  first.Init(pointer(password),length(password));
  for i := 2 to count do begin
    mac := first; // re-use the very same SHA context for best performance
    mac.sha.Update(@tmp,sizeof(tmp));
    mac.Done(tmp,true);
    XorMemory(@result,@tmp,sizeof(result));
  end;
  FillcharFast(mac,sizeof(mac),0);
  FillcharFast(first,sizeof(first),0);
  FillZero(tmp);
end;


{ THMAC_SHA256 }

procedure THMAC_SHA256.Init(key: pointer; keylen: integer);
var i: integer;
    k0,k0xorIpad: THash512Rec;
begin
  FillZero(k0.b);
  if keylen>sizeof(k0) then
    sha.Full(key,keylen,k0.Lo) else
    MoveFast(key^,k0,keylen);
  for i := 0 to 15 do
    k0xorIpad.c[i] := k0.c[i] xor $36363636;
  for i := 0 to 15 do
    step7data.c[i] := k0.c[i] xor $5c5c5c5c;
  sha.Init;
  sha.Update(@k0xorIpad,sizeof(k0xorIpad));
  FillZero(k0.b);
  FillZero(k0xorIpad.b);
end;

procedure THMAC_SHA256.Update(msg: pointer; msglen: integer);
begin
  sha.Update(msg,msglen);
end;

procedure THMAC_SHA256.Update(const msg: THash128);
begin
  sha.Update(@msg,sizeof(msg));
end;

procedure THMAC_SHA256.Update(const msg: THash256);
begin
  sha.Update(@msg,sizeof(msg));
end;

procedure THMAC_SHA256.Update(const msg: RawByteString);
begin
  sha.Update(pointer(msg),length(msg));
end;

procedure THMAC_SHA256.Done(out result: TSHA256Digest; NoInit: boolean);
begin
  sha.Final(result);
  sha.Update(@step7data,sizeof(step7data));
  sha.Update(@result,sizeof(result));
  sha.Final(result,NoInit);
  if not NoInit then
    FillZero(step7data.b);
end;

procedure THMAC_SHA256.Done(out result: RawUTF8; NoInit: boolean);
var res: THash256;
begin
  Done(res,NoInit);
  result := SHA256DigestToString(res);
  if not NoInit then
    FillZero(res);
end;

procedure THMAC_SHA256.Compute(msg: pointer; msglen: integer; out result: TSHA256Digest);
var temp: THMAC_SHA256;
begin
  temp := self; // thread-safe copy
  temp.Update(msg,msglen);
  temp.Done(result);
end;

procedure HMAC_SHA256(key,msg: pointer; keylen,msglen: integer; out result: TSHA256Digest);
var mac: THMAC_SHA256;
begin
  mac.Init(key,keylen);
  mac.Update(msg,msglen);
  mac.Done(result);
end;

procedure HMAC_SHA256(const key,msg: RawByteString; out result: TSHA256Digest);
begin
  HMAC_SHA256(pointer(key),pointer(msg),length(key),length(msg),result);
end;

procedure HMAC_SHA256(const key: TSHA256Digest; const msg: RawByteString; out result: TSHA256Digest);
begin
  HMAC_SHA256(@key,pointer(msg),sizeof(key),length(msg),result);
end;

procedure PBKDF2_HMAC_SHA256(const password,salt: RawByteString; count: Integer;
  out result: TSHA256Digest; const saltdefault: RawByteString);
var i: integer;
    tmp: TSHA256Digest;
    mac: THMAC_SHA256;
    first: THMAC_SHA256;
begin
  if salt='' then
    HMAC_SHA256(password,saltdefault+#0#0#0#1,result) else
    HMAC_SHA256(password,salt+#0#0#0#1,result);
  if count<2 then
    exit;
  tmp := result;
  first.Init(pointer(password),length(password));
  for i := 2 to count do begin
    mac := first; // re-use the very same SHA context for best performance
    mac.sha.Update(@tmp,sizeof(tmp));
    mac.Done(tmp,true);
    XorMemoryPtrInt(@result,@tmp,sizeof(result) shr {$ifdef CPU32}2{$else}3{$endif});
  end;
  FillcharFast(first,sizeof(first),0);
  FillcharFast(mac,sizeof(mac),0);
  FillZero(tmp);
end;

procedure PBKDF2_HMAC_SHA256(const password,salt: RawByteString; count: Integer;
  var result: THash256DynArray; const saltdefault: RawByteString);
var n,i: integer;
    iter: RawByteString;
    tmp: TSHA256Digest;
    mac: THMAC_SHA256;
    first: THMAC_SHA256;
begin
  first.Init(pointer(password),length(password));
  SetLength(iter,sizeof(integer));
  for n := 0 to high(result) do begin
    PInteger(iter)^ := bswap32(n+1); // U1 = PRF(Password, Salt || INT_32_BE(i))
    if salt='' then
      HMAC_SHA256(password,saltdefault+iter,result[n]) else
      HMAC_SHA256(password,salt+iter,result[n]);
    tmp := result[n];
    for i := 2 to count do begin
      mac := first; // re-use the very same SHA context for best performance
      mac.sha.Update(@tmp,sizeof(tmp));
      mac.Done(tmp,true);
      XorMemoryPtrInt(@result[n],@tmp,sizeof(result[n]) shr {$ifdef CPU32}2{$else}3{$endif});
    end;
  end;
  FillZero(tmp);
  FillcharFast(mac,sizeof(mac),0);
  FillcharFast(first,sizeof(first),0);
end;

function SHA256(const s: RawByteString): RawUTF8;
var SHA: TSHA256;
    Digest: TSHA256Digest;
begin
  SHA.Full(pointer(s),length(s),Digest);
  result := SHA256DigestToString(Digest);
  FillZero(Digest);
end;

function SHA256(Data: pointer; Len: integer): RawUTF8;
var SHA: TSHA256;
    Digest: TSHA256Digest;
begin
  SHA.Full(Data,Len,Digest);
  result := SHA256DigestToString(Digest);
  FillZero(Digest);
end;

function SHA256Digest(Data: pointer; Len: integer): TSHA256Digest;
var SHA: TSHA256;
begin
  SHA.Full(Data,Len,result);
end;

function SHA256Digest(const Data: RawByteString): TSHA256Digest;
var SHA: TSHA256;
begin
  SHA.Full(pointer(Data),Length(Data),result);
end;


{ THMAC_SHA384 }

procedure THMAC_SHA384.Init(key: pointer; keylen: integer);
var i: integer;
    k0,k0xorIpad: array[0..31] of cardinal;
begin
  FillCharFast(k0,sizeof(k0),0);
  if keylen>sizeof(k0) then
    sha.Full(key,keylen,PSHA384Digest(@k0)^) else
    MoveFast(key^,k0,keylen);
  for i := 0 to 31 do
    k0xorIpad[i] := k0[i] xor $36363636;
  for i := 0 to 31 do
    step7data[i] := k0[i] xor $5c5c5c5c;
  sha.Init;
  sha.Update(@k0xorIpad,sizeof(k0xorIpad));
  FillCharFast(k0,sizeof(k0),0);
  FillCharFast(k0xorIpad,sizeof(k0xorIpad),0);
end;

procedure THMAC_SHA384.Update(msg: pointer; msglen: integer);
begin
  sha.Update(msg,msglen);
end;

procedure THMAC_SHA384.Done(out result: TSHA384Digest; NoInit: boolean);
begin
  sha.Final(result);
  sha.Update(@step7data,sizeof(step7data));
  sha.Update(@result,sizeof(result));
  sha.Final(result,NoInit);
  if not NoInit then
    FillCharFast(step7data,sizeof(step7data),0);
end;

procedure THMAC_SHA384.Done(out result: RawUTF8; NoInit: boolean);
var res: THash384;
begin
  Done(res,NoInit);
  result := SHA384DigestToString(res);
  if not NoInit then
    FillZero(res);
end;

procedure THMAC_SHA384.Compute(msg: pointer; msglen: integer; out result: TSHA384Digest);
var temp: THMAC_SHA384;
begin
  temp := self; // thread-safe copy
  temp.Update(msg,msglen);
  temp.Done(result);
end;

procedure HMAC_SHA384(key,msg: pointer; keylen,msglen: integer; out result: TSHA384Digest);
var mac: THMAC_SHA384;
begin
  mac.Init(key,keylen);
  mac.Update(msg,msglen);
  mac.Done(result);
end;

procedure HMAC_SHA384(const key,msg: RawByteString; out result: TSHA384Digest);
begin
  HMAC_SHA384(pointer(key),pointer(msg),length(key),length(msg),result);
end;

procedure HMAC_SHA384(const key: TSHA384Digest; const msg: RawByteString; out result: TSHA384Digest);
begin
  HMAC_SHA384(@key,pointer(msg),sizeof(key),length(msg),result);
end;

procedure PBKDF2_HMAC_SHA384(const password,salt: RawByteString; count: Integer;
  out result: TSHA384Digest);
var i: integer;
    tmp: TSHA384Digest;
    mac: THMAC_SHA384;
    first: THMAC_SHA384;
begin
  HMAC_SHA384(password,salt+#0#0#0#1,result);
  if count<2 then
    exit;
  tmp := result;
  first.Init(pointer(password),length(password));
  for i := 2 to count do begin
    mac := first; // re-use the very same SHA context for best performance
    mac.sha.Update(@tmp,sizeof(tmp));
    mac.Done(tmp,true);
    XorMemoryPtrInt(@result,@tmp,sizeof(result) shr {$ifdef CPU32}2{$else}3{$endif});
  end;
  FillcharFast(mac,sizeof(mac),0);
  FillcharFast(first,sizeof(first),0);
  FillZero(tmp);
end;


{ THMAC_SHA512 }

procedure THMAC_SHA512.Init(key: pointer; keylen: integer);
var i: integer;
    k0,k0xorIpad: array[0..31] of cardinal;
begin
  FillCharFast(k0,sizeof(k0),0);
  if keylen>sizeof(k0) then
    sha.Full(key,keylen,PSHA512Digest(@k0)^) else
    MoveFast(key^,k0,keylen);
  for i := 0 to 31 do
    k0xorIpad[i] := k0[i] xor $36363636;
  for i := 0 to 31 do
    step7data[i] := k0[i] xor $5c5c5c5c;
  sha.Init;
  sha.Update(@k0xorIpad,sizeof(k0xorIpad));
  FillCharFast(k0,sizeof(k0),0);
  FillCharFast(k0xorIpad,sizeof(k0xorIpad),0);
end;

procedure THMAC_SHA512.Update(msg: pointer; msglen: integer);
begin
  sha.Update(msg,msglen);
end;

procedure THMAC_SHA512.Done(out result: TSHA512Digest; NoInit: boolean);
begin
  sha.Final(result);
  sha.Update(@step7data,sizeof(step7data));
  sha.Update(@result,sizeof(result));
  sha.Final(result,NoInit);
  if not NoInit then
    FillCharFast(step7data,sizeof(step7data),0);
end;

procedure THMAC_SHA512.Done(out result: RawUTF8; NoInit: boolean);
var res: THash512;
begin
  Done(res,NoInit);
  result := SHA512DigestToString(res);
  if not NoInit then
    FillZero(res);
end;

procedure THMAC_SHA512.Compute(msg: pointer; msglen: integer; out result: TSHA512Digest);
var temp: THMAC_SHA512;
begin
  temp := self; // thread-safe copy
  temp.Update(msg,msglen);
  temp.Done(result);
end;

procedure HMAC_SHA512(key,msg: pointer; keylen,msglen: integer; out result: TSHA512Digest);
var mac: THMAC_SHA512;
begin
  mac.Init(key,keylen);
  mac.Update(msg,msglen);
  mac.Done(result);
end;

procedure HMAC_SHA512(const key,msg: RawByteString; out result: TSHA512Digest);
begin
  HMAC_SHA512(pointer(key),pointer(msg),length(key),length(msg),result);
end;

procedure HMAC_SHA512(const key: TSHA512Digest; const msg: RawByteString; out result: TSHA512Digest);
begin
  HMAC_SHA512(@key,pointer(msg),sizeof(key),length(msg),result);
end;

procedure PBKDF2_HMAC_SHA512(const password,salt: RawByteString; count: Integer;
  out result: TSHA512Digest);
var i: integer;
    tmp: TSHA512Digest;
    mac: THMAC_SHA512;
    first: THMAC_SHA512;
begin
  HMAC_SHA512(password,salt+#0#0#0#1,result);
  if count<2 then
    exit;
  tmp := result;
  first.Init(pointer(password),length(password));
  for i := 2 to count do begin
    mac := first; // re-use the very same SHA context for best performance
    mac.sha.Update(@tmp,sizeof(tmp));
    mac.Done(tmp,true);
    XorMemoryPtrInt(@result,@tmp,sizeof(result) shr {$ifdef CPU32}2{$else}3{$endif});
  end;
  FillcharFast(mac,sizeof(mac),0);
  FillcharFast(first,sizeof(first),0);
  FillZero(tmp);
end;


{ HMAC_CRC256C }

procedure crc256cmix(h1,h2: cardinal; h: PCardinalArray);
begin // see https://goo.gl/Pls5wi
  h^[0] := h1; inc(h1,h2);
  h^[1] := h1; inc(h1,h2);
  h^[2] := h1; inc(h1,h2);
  h^[3] := h1; inc(h1,h2);
  h^[4] := h1; inc(h1,h2);
  h^[5] := h1; inc(h1,h2);
  h^[6] := h1; inc(h1,h2);
  h^[7] := h1;
end;

procedure HMAC_CRC256C(key,msg: pointer; keylen,msglen: integer; out result: THash256);
var i: integer;
    h1,h2: cardinal;
    k0,k0xorIpad,step7data: THash512Rec;
begin
  FillCharFast(k0,sizeof(k0),0);
  if keylen>sizeof(k0) then
    crc256c(key,keylen,k0.Lo) else
    MoveFast(key^,k0,keylen);
  for i := 0 to 15 do
    k0xorIpad.c[i] := k0.c[i] xor $36363636;
  for i := 0 to 15 do
    step7data.c[i] := k0.c[i] xor $5c5c5c5c;
  h1 := crc32c(crc32c(0,@k0xorIpad,sizeof(k0xorIpad)),msg,msglen);
  h2 := crc32c(crc32c(h1,@k0xorIpad,sizeof(k0xorIpad)),msg,msglen);
  crc256cmix(h1,h2,@result);
  h1 := crc32c(crc32c(0,@step7data,sizeof(step7data)),@result,sizeof(result));
  h2 := crc32c(crc32c(h1,@step7data,sizeof(step7data)),@result,sizeof(result));
  crc256cmix(h1,h2,@result);
  FillCharFast(k0,sizeof(k0),0);
  FillCharFast(k0xorIpad,sizeof(k0),0);
  FillCharFast(step7data,sizeof(k0),0);
end;

procedure HMAC_CRC256C(const key: THash256; const msg: RawByteString; out result: THash256);
begin
  HMAC_CRC256C(@key,pointer(msg),SizeOf(key),length(msg),result);
end;

procedure HMAC_CRC256C(const key,msg: RawByteString; out result: THash256);
begin
  HMAC_CRC256C(pointer(key),pointer(msg),length(key),length(msg),result);
end;


{ THMAC_CRC32C }

procedure THMAC_CRC32C.Init(const key: RawByteString);
begin
  Init(pointer(key),length(key));
end;

procedure THMAC_CRC32C.Init(key: pointer; keylen: integer);
var i: integer;
    k0,k0xorIpad: THash512Rec;
begin
  FillCharFast(k0,sizeof(k0),0);
  if keylen>sizeof(k0) then
    crc256c(key,keylen,k0.Lo) else
    MoveFast(key^,k0,keylen);
  for i := 0 to 15 do
    k0xorIpad.c[i] := k0.c[i] xor $36363636;
  for i := 0 to 15 do
    step7data.c[i] := k0.c[i] xor $5c5c5c5c;
  seed := crc32c(0,@k0xorIpad,sizeof(k0xorIpad));
  FillCharFast(k0,sizeof(k0),0);
  FillCharFast(k0xorIpad,sizeof(k0xorIpad),0);
end;

procedure THMAC_CRC32C.Update(msg: pointer; msglen: integer);
begin
  seed := crc32c(seed,msg,msglen);
end;

procedure THMAC_CRC32C.Update(const msg: RawByteString);
begin
  seed := crc32c(seed,pointer(msg),length(msg));
end;

function THMAC_CRC32C.Done(NoInit: boolean): cardinal;
begin
  result := crc32c(seed,@step7data,sizeof(step7data));
  if not NoInit then
    FillcharFast(self,sizeof(self),0);
end;

function THMAC_CRC32C.Compute(msg: pointer; msglen: integer): cardinal;
begin
  result := crc32c(crc32c(seed,msg,msglen),@step7data,sizeof(step7data));
end;

function HMAC_CRC32C(key,msg: pointer; keylen,msglen: integer): cardinal;
var mac: THMAC_CRC32C;
begin
  mac.Init(key,keylen);
  mac.Update(msg,msglen);
  result := mac.Done;
end;

function HMAC_CRC32C(const key: THash256; const msg: RawByteString): cardinal;
begin
  result := HMAC_CRC32C(@key,pointer(msg),SizeOf(key),length(msg));
end;

function HMAC_CRC32C(const key,msg: RawByteString): cardinal;
begin
  result := HMAC_CRC32C(pointer(key),pointer(msg),length(key),length(msg));
end;


function SHA1SelfTest: boolean;
function SingleTest(const s: RawByteString; TDig: TSHA1Digest): boolean;
var SHA: TSHA1;
    Digest: TSHA1Digest;
    i: integer;
begin
  // 1. Hash complete RawByteString
  SHA.Full(pointer(s),length(s),Digest);
  result := IsEqual(Digest,TDig);
  if not result then exit;
  // 2. one update call for all chars
  for i := 1 to length(s) do
    SHA.Update(@s[i],1);
  SHA.Final(Digest);
  result := IsEqual(Digest,TDig);
  // 3. test consistency with Padlock engine down results
{$ifdef USEPADLOCK}
  if not result or not padlock_available then exit;
  padlock_available := false;  // force PadLock engine down
  SHA.Full(pointer(s),length(s),Digest);
  result := IsEqual(Digest,TDig);
{$ifdef PADLOCKDEBUG} write('=padlock '); {$endif}
  padlock_available := true;
{$endif}
end;
const
  Test1Out: TSHA1Digest=
    ($A9,$99,$3E,$36,$47,$06,$81,$6A,$BA,$3E,$25,$71,$78,$50,$C2,$6C,$9C,$D0,$D8,$9D);
  Test2Out: TSHA1Digest=
    ($84,$98,$3E,$44,$1C,$3B,$D2,$6E,$BA,$AE,$4A,$A1,$F9,$51,$29,$E5,$E5,$46,$70,$F1);
var
  s: RawByteString;
  SHA: TSHA1;
  Digest: TSHA1Digest;
begin
  result := SingleTest('abc',Test1Out) and
    SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq',Test2Out);
  if not result then exit;
  s := 'Wikipedia, l''encyclopedie libre et gratuite';
  SHA.Full(pointer(s),length(s),Digest);
  result := SHA1DigestToString(Digest)='c18cc65028bbdc147288a2d136313287782b9c73';
  if not result then exit;
  HMAC_SHA1('','',Digest);
  result := SHA1DigestToString(Digest)='fbdb1d1b18aa6c08324b7d64b71fb76370690e1d';
  if not result then exit;
  HMAC_SHA1('key','The quick brown fox jumps over the lazy dog',Digest);
  result := SHA1DigestToString(Digest)='de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9';
  if not result then exit;
  // from https://www.ietf.org/rfc/rfc6070.txt
  PBKDF2_HMAC_SHA1('password','salt',1,Digest);
  s := SHA1DigestToString(Digest);
  result := s='0c60c80f961f0e71f3a9b524af6012062fe037a6';
  if not result then exit;
  PBKDF2_HMAC_SHA1('password','salt',2,Digest);
  s := SHA1DigestToString(Digest);
  result := s='ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957';
  if not result then exit;
  PBKDF2_HMAC_SHA1('password','salt',4096,Digest);
  s := SHA1DigestToString(Digest);
  result := s='4b007901b765489abead49d926f721d065a429c1';
end;


{ TAES }

function AESSelfTest(onlytables: Boolean): boolean;
var A: TAES;
    st: RawByteString;
    Key: TSHA256Digest;
    s,b,p: TAESBlock;
    i,k,ks: integer;
begin
  // ensure that we have $2000 bytes of contiguous XOR tables ;)
  result := (PtrUInt(@TD0)+$400=PtrUInt(@TD1))and(PtrUInt(@TD0)+$800=PtrUInt(@TD2))
    and(PtrUInt(@TD0)+$C00=PtrUInt(@TD3))and(PtrUInt(@TD0)+$1000=PtrUInt(@TE0))
    and(PtrUInt(@TD0)+$1400=PtrUInt(@TE1))and(PtrUInt(@TD0)+$1800=PtrUInt(@TE2))
    and(PtrUInt(@TD0)+$1C00=PtrUInt(@TE3))and
    (SBox[255]=$16)and(InvSBox[0]=$52)and
    (Te0[0]=$a56363c6)and(Te0[255]=$3a16162c)and
    (Te1[0]=$6363c6a5)and(Te1[255]=$16162c3a)and
    (Te3[0]=$c6a56363)and(Te3[255]=$2c3a1616)and
    (Td0[0]=$50a7f451)and(Td0[99]=0)and(Td0[255]=$4257b8d0)and
    (Td3[0]=$5150a7f4)and(Td3[255]=$d04257b8);
  if onlytables or not result then
    exit;
  // test
  result := false;
  Randomize;
  st := '1234essai';
  PInteger(@st[1])^ := Random(MaxInt);
  for k := 0 to 2 do begin
    ks := 128+k*64; // test keysize of 128,192 and 256 bits
//    write('Test AES ',ks);
    for i := 1 to 100 do begin
      SHA256Weak(st,Key);
      moveFast(Key,s,16);
      A.EncryptInit(Key,ks);
      A.Encrypt(s,b);
      A.Done;
      A.DecryptInit(Key,ks);
      A.Decrypt(b,p);
      A.Done;
      if not IsEqual(p,s) then begin
        writeln('AESSelfTest compareError with keysize=',ks);
        exit;
      end;
      st := st+AnsiChar(Random(255));
    end;
  end;
  result := true;
end;

procedure TAES.Encrypt(var B: TAESBlock);
begin
  TAESContext(Context).DoBlock(Context,B,B);
end;

{$ifdef USEAESNI}
{$ifdef CPU32}
procedure AesNiEncryptXmm7_128; {$ifdef FPC} nostackframe; assembler; {$endif}
asm // input: eax=TAESContext, xmm7=data; output: eax=TAESContext, xmm7=data
        movups  xmm0, [eax + 16 * 0]
        movups  xmm1, [eax + 16 * 1]
        movups  xmm2, [eax + 16 * 2]
        movups  xmm3, [eax + 16 * 3]
        movups  xmm4, [eax + 16 * 4]
        movups  xmm5, [eax + 16 * 5]
        movups  xmm6, [eax + 16 * 6]
        pxor    xmm7, xmm0
  {$ifdef HASAESNI}
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
  {$else}
        db      $66, $0F, $38, $DC, $F9
        db      $66, $0F, $38, $DC, $FA
        db      $66, $0F, $38, $DC, $FB
        db      $66, $0F, $38, $DC, $FC
        db      $66, $0F, $38, $DC, $FD
        db      $66, $0F, $38, $DC, $FE
  {$endif}
        movups  xmm0, [eax + 16 * 7]
        movups  xmm1, [eax + 16 * 8]
        movups  xmm2, [eax + 16 * 9]
        movups  xmm3, [eax + 16 * 10]
  {$ifdef HASAESNI}
        aesenc  xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenclast xmm7, xmm3
  {$else}
        db      $66, $0F, $38, $DC, $F8
        db      $66, $0F, $38, $DC, $F9
        db      $66, $0F, $38, $DC, $FA
        db      $66, $0F, $38, $DD, $FB
  {$endif}
end;
procedure aesniencrypt128(const ctxt, source, dest);
  {$ifdef FPC} nostackframe; assembler; {$endif}
asm // eax=ctxt edx=source ecx=dest
        movups  xmm7, [edx]
        call    AesNiEncryptXmm7_128
        movups  [ecx], xmm7
        pxor    xmm7, xmm7 // for safety
end;
procedure AesNiEncryptXmm7_192;
  {$ifdef FPC} nostackframe; assembler; {$endif}
asm // input: eax=TAESContext, xmm7=data; output: eax=TAESContext, xmm7=data
        movups  xmm0, [eax + 16 * 0]
        movups  xmm1, [eax + 16 * 1]
        movups  xmm2, [eax + 16 * 2]
        movups  xmm3, [eax + 16 * 3]
        movups  xmm4, [eax + 16 * 4]
        movups  xmm5, [eax + 16 * 5]
        movups  xmm6, [eax + 16 * 6]
        pxor    xmm7, xmm0
  {$ifdef HASAESNI}
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
  {$else}
        db      $66, $0F, $38, $DC, $F9
        db      $66, $0F, $38, $DC, $FA
        db      $66, $0F, $38, $DC, $FB
        db      $66, $0F, $38, $DC, $FC
        db      $66, $0F, $38, $DC, $FD
        db      $66, $0F, $38, $DC, $FE
  {$endif}
        movups  xmm0, [eax + 16 * 7]
        movups  xmm1, [eax + 16 * 8]
        movups  xmm2, [eax + 16 * 9]
        movups  xmm3, [eax + 16 * 10]
        movups  xmm4, [eax + 16 * 11]
        movups  xmm5, [eax + 16 * 12]
  {$ifdef HASAESNI}
        aesenc  xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenclast xmm7, xmm5
  {$else}
        db      $66, $0F, $38, $DC, $F8
        db      $66, $0F, $38, $DC, $F9
        db      $66, $0F, $38, $DC, $FA
        db      $66, $0F, $38, $DC, $FB
        db      $66, $0F, $38, $DC, $FC
        db      $66, $0F, $38, $DD, $FD
  {$endif}
end;
procedure aesniencrypt192(const ctxt, source, dest);
  {$ifdef FPC} nostackframe; assembler; {$endif}
asm // eax=ctxt edx=source ecx=dest
        movups  xmm7, [edx]
        call    AesNiEncryptXmm7_192
        movups  [ecx], xmm7
        pxor    xmm7, xmm7 // for safety
end;
procedure AesNiEncryptXmm7_256;
  {$ifdef FPC} nostackframe; assembler; {$endif}
asm // input: eax=TAESContext, xmm7=data; output: eax=TAESContext, xmm7=data
        movups  xmm0, [eax + 16 * 0]
        movups  xmm1, [eax + 16 * 1]
        movups  xmm2, [eax + 16 * 2]
        movups  xmm3, [eax + 16 * 3]
        movups  xmm4, [eax + 16 * 4]
        movups  xmm5, [eax + 16 * 5]
        movups  xmm6, [eax + 16 * 6]
        pxor    xmm7, xmm0
  {$ifdef HASAESNI}
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
  {$else}
        db      $66, $0F, $38, $DC, $F9
        db      $66, $0F, $38, $DC, $FA
        db      $66, $0F, $38, $DC, $FB
        db      $66, $0F, $38, $DC, $FC
        db      $66, $0F, $38, $DC, $FD
        db      $66, $0F, $38, $DC, $FE
  {$endif}
        movups  xmm0, [eax + 16 * 7]
        movups  xmm1, [eax + 16 * 8]
        movups  xmm2, [eax + 16 * 9]
        movups  xmm3, [eax + 16 * 10]
        movups  xmm4, [eax + 16 * 11]
        movups  xmm5, [eax + 16 * 12]
        movups  xmm6, [eax + 16 * 13]
  {$ifdef HASAESNI}
        aesenc  xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
  {$else}
        db      $66, $0F, $38, $DC, $F8
        db      $66, $0F, $38, $DC, $F9
        db      $66, $0F, $38, $DC, $FA
        db      $66, $0F, $38, $DC, $FB
        db      $66, $0F, $38, $DC, $FC
        db      $66, $0F, $38, $DC, $FD
        db      $66, $0F, $38, $DC, $FE
  {$endif}
        movups  xmm1, [eax + 16 * 14]
  {$ifdef HASAESNI}
        aesenclast xmm7, xmm1
  {$else}
        db      $66, $0F, $38, $DD, $F9
  {$endif}
end;
procedure aesniencrypt256(const ctxt, source, dest);
  {$ifdef FPC} nostackframe; assembler; {$endif}
asm // eax=ctxt edx=source ecx=dest
        movups  xmm7, [edx]
        call    AesNiEncryptXmm7_256
        movups  [ecx], xmm7
        pxor    xmm7, xmm7 // for safety
end;
{$endif CPU32}
{$ifdef CPU64}
procedure aesniencrypt128(const ctxt, source, dest); {$ifdef FPC}nostackframe; assembler;
asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr[source]
        movups  xmm0, dqword ptr[ctxt + 16 * 0]
        movups  xmm1, dqword ptr[ctxt + 16 * 1]
        movups  xmm2, dqword ptr[ctxt + 16 * 2]
        movups  xmm3, dqword ptr[ctxt + 16 * 3]
        movups  xmm4, dqword ptr[ctxt + 16 * 4]
        movups  xmm5, dqword ptr[ctxt + 16 * 5]
        movups  xmm6, dqword ptr[ctxt + 16 * 6]
        movups  xmm8, dqword ptr[ctxt + 16 * 7]
        movups  xmm9, dqword ptr[ctxt + 16 * 8]
        movups  xmm10, dqword ptr[ctxt + 16 * 9]
        movups  xmm11, dqword ptr[ctxt + 16 * 10]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenclast xmm7, xmm11
        movups  dqword ptr[dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;
procedure aesniencrypt192(const ctxt, source, dest); {$ifdef FPC}nostackframe; assembler;
asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr[source]
        movups  xmm0, dqword ptr[ctxt + 16 * 0]
        movups  xmm1, dqword ptr[ctxt + 16 * 1]
        movups  xmm2, dqword ptr[ctxt + 16 * 2]
        movups  xmm3, dqword ptr[ctxt + 16 * 3]
        movups  xmm4, dqword ptr[ctxt + 16 * 4]
        movups  xmm5, dqword ptr[ctxt + 16 * 5]
        movups  xmm6, dqword ptr[ctxt + 16 * 6]
        movups  xmm8, dqword ptr[ctxt + 16 * 7]
        movups  xmm9, dqword ptr[ctxt + 16 * 8]
        movups  xmm10, dqword ptr[ctxt + 16 * 9]
        movups  xmm11, dqword ptr[ctxt + 16 * 10]
        movups  xmm12, dqword ptr[ctxt + 16 * 11]
        movups  xmm13, dqword ptr[ctxt + 16 * 12]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenc  xmm7, xmm11
        aesenc  xmm7, xmm12
        aesenclast xmm7, xmm13
        movups  dqword ptr[dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;
procedure aesniencrypt256(const ctxt, source, dest); {$ifdef FPC}nostackframe; assembler;
asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr[source]
        movups  xmm0, dqword ptr[ctxt + 16 * 0]
        movups  xmm1, dqword ptr[ctxt + 16 * 1]
        movups  xmm2, dqword ptr[ctxt + 16 * 2]
        movups  xmm3, dqword ptr[ctxt + 16 * 3]
        movups  xmm4, dqword ptr[ctxt + 16 * 4]
        movups  xmm5, dqword ptr[ctxt + 16 * 5]
        movups  xmm6, dqword ptr[ctxt + 16 * 6]
        movups  xmm8, dqword ptr[ctxt + 16 * 7]
        movups  xmm9, dqword ptr[ctxt + 16 * 8]
        movups  xmm10, dqword ptr[ctxt + 16 * 9]
        movups  xmm11, dqword ptr[ctxt + 16 * 10]
        movups  xmm12, dqword ptr[ctxt + 16 * 11]
        movups  xmm13, dqword ptr[ctxt + 16 * 12]
        movups  xmm14, dqword ptr[ctxt + 16 * 13]
        movups  xmm15, dqword ptr[ctxt + 16 * 14]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenc  xmm7, xmm11
        aesenc  xmm7, xmm12
        aesenc  xmm7, xmm13
        aesenc  xmm7, xmm14
        aesenclast xmm7, xmm15
        movups  dqword ptr[dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;
procedure aesnidecrypt128(const ctxt, source, dest); {$ifdef FPC}nostackframe; assembler;
asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr[source]
        movups  xmm0, dqword ptr[ctxt + 16 * 10]
        movups  xmm1, dqword ptr[ctxt + 16 * 9]
        movups  xmm2, dqword ptr[ctxt + 16 * 8]
        movups  xmm3, dqword ptr[ctxt + 16 * 7]
        movups  xmm4, dqword ptr[ctxt + 16 * 6]
        movups  xmm5, dqword ptr[ctxt + 16 * 5]
        movups  xmm6, dqword ptr[ctxt + 16 * 4]
        movups  xmm8, dqword ptr[ctxt + 16 * 3]
        movups  xmm9, dqword ptr[ctxt + 16 * 2]
        movups  xmm10, dqword ptr[ctxt + 16 * 1]
        movups  xmm11, dqword ptr[ctxt + 16 * 0]
        pxor    xmm7, xmm0
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
        aesdec  xmm7, xmm8
        aesdec  xmm7, xmm9
        aesdec  xmm7, xmm10
        aesdeclast xmm7, xmm11
        movups  dqword ptr[dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;
procedure aesnidecrypt192(const ctxt, source, dest); {$ifdef FPC}nostackframe; assembler;
asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr[source]
        movups  xmm0, dqword ptr[ctxt + 16 * 12]
        movups  xmm1, dqword ptr[ctxt + 16 * 11]
        movups  xmm2, dqword ptr[ctxt + 16 * 10]
        movups  xmm3, dqword ptr[ctxt + 16 * 9]
        movups  xmm4, dqword ptr[ctxt + 16 * 8]
        movups  xmm5, dqword ptr[ctxt + 16 * 7]
        movups  xmm6, dqword ptr[ctxt + 16 * 6]
        movups  xmm8, dqword ptr[ctxt + 16 * 5]
        movups  xmm9, dqword ptr[ctxt + 16 * 4]
        movups  xmm10, dqword ptr[ctxt + 16 * 3]
        movups  xmm11, dqword ptr[ctxt + 16 * 2]
        movups  xmm12, dqword ptr[ctxt + 16 * 1]
        movups  xmm13, dqword ptr[ctxt + 16 * 0]
        pxor    xmm7, xmm0
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
        aesdec  xmm7, xmm8
        aesdec  xmm7, xmm9
        aesdec  xmm7, xmm10
        aesdec  xmm7, xmm11
        aesdec  xmm7, xmm12
        aesdeclast xmm7, xmm13
        movups  dqword ptr[dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;
procedure aesnidecrypt256(const ctxt, source, dest); {$ifdef FPC}nostackframe; assembler;
asm {$else} asm .noframe {$endif}
        movups  xmm7, dqword ptr[source]
        movups  xmm0, dqword ptr[ctxt + 16 * 14]
        movups  xmm1, dqword ptr[ctxt + 16 * 13]
        movups  xmm2, dqword ptr[ctxt + 16 * 12]
        movups  xmm3, dqword ptr[ctxt + 16 * 11]
        movups  xmm4, dqword ptr[ctxt + 16 * 10]
        movups  xmm5, dqword ptr[ctxt + 16 * 9]
        movups  xmm6, dqword ptr[ctxt + 16 * 8]
        movups  xmm8, dqword ptr[ctxt + 16 * 7]
        movups  xmm9, dqword ptr[ctxt + 16 * 6]
        movups  xmm10, dqword ptr[ctxt + 16 * 5]
        movups  xmm11, dqword ptr[ctxt + 16 * 4]
        movups  xmm12, dqword ptr[ctxt + 16 * 3]
        movups  xmm13, dqword ptr[ctxt + 16 * 2]
        movups  xmm14, dqword ptr[ctxt + 16 * 1]
        movups  xmm15, dqword ptr[ctxt + 16 * 0]
        pxor    xmm7, xmm0
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
        aesdec  xmm7, xmm8
        aesdec  xmm7, xmm9
        aesdec  xmm7, xmm10
        aesdec  xmm7, xmm11
        aesdec  xmm7, xmm12
        aesdec  xmm7, xmm13
        aesdec  xmm7, xmm14
        aesdeclast xmm7, xmm15
        movups  dqword ptr[dest], xmm7
        pxor    xmm7, xmm7 // for safety
end;
{$endif CPU64}
{$endif USEAESNI}

procedure aesencryptpas(const ctxt: TAESContext; bi, bo: PWA4);
{ AES_PASCAL version (c) Wolfgang Ehrhardt under zlib license:
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:
 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.
 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.
 3. This notice may not be removed or altered from any source distribution.
 -> code has been refactored and tuned especially for FPC x86_64 target }
var
  t: PCardinalArray;    // faster on a PIC system
{$ifdef AES_ROLLED}
  s0,s1,s2,s3: PtrUInt; // TAESBlock s# as separate variables
  t0,t1,t2: cardinal;   // TAESBlock t# as separate variables
  pk: PWA4;
  i: integer;
begin
  pk := @ctxt.RK;
  s0 := bi[0] xor pk[0];
  s1 := bi[1] xor pk[1];
  s2 := bi[2] xor pk[2];
  s3 := bi[3] xor pk[3];
  inc(pk);
  t := @Te0;
  for i := 1 to ctxt.rounds-1 do begin
    t0 := t[s0 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s3 shr 24];
    t1 := t[s1 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s0 shr 24];
    t2 := t[s2 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s1 shr 24];
    s3 := t[s3 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[3];;
    s0 := t0 xor pk[0];
    s1 := t1 xor pk[1];
    s2 := t2 xor pk[2];
    inc(pk);
  end;
  bo[0] := ((SBox[s0        and $ff])        xor
            (SBox[s1 shr  8 and $ff]) shl  8 xor
            (SBox[s2 shr 16 and $ff]) shl 16 xor
            (SBox[s3 shr 24])         shl 24    ) xor pk[0];
  bo[1] := ((SBox[s1        and $ff])        xor
            (SBox[s2 shr  8 and $ff]) shl  8 xor
            (SBox[s3 shr 16 and $ff]) shl 16 xor
            (SBox[s0 shr 24])         shl 24    ) xor pk[1];
  bo[2] := ((SBox[s2        and $ff])        xor
            (SBox[s3 shr  8 and $ff]) shl  8 xor
            (SBox[s0 shr 16 and $ff]) shl 16 xor
            (SBox[s1 shr 24])         shl 24    ) xor pk[2];
  bo[3] := ((SBox[s3        and $ff])        xor
            (SBox[s0 shr  8 and $ff]) shl  8 xor
            (SBox[s1 shr 16 and $ff]) shl 16 xor
            (SBox[s2 shr 24])         shl 24    ) xor pk[3];
{$else}
  s0,s1,s2,s3,t0,t1,t2,t3: cardinal; // TAESBlock s#/t# as separate variables
  pK: PAWk;
begin
  pK := @ctxt.RK;
  // Initialize with input block
  s0 := bi[0] xor pk[0];
  s1 := bi[1] xor pk[1];
  s2 := bi[2] xor pk[2];
  s3 := bi[3] xor pk[3];
  t := @Te0;
  // Round 1
  t0 := t[s0 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[4];
  t1 := t[s1 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[5];
  t2 := t[s2 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[6];
  t3 := t[s3 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[7];
  // Round 2
  s0 := t[t0 and $ff] xor t[$100+t1 shr 8 and $ff] xor t[$200+t2 shr 16 and $ff] xor t[$300+t3 shr 24] xor pk[8];
  s1 := t[t1 and $ff] xor t[$100+t2 shr 8 and $ff] xor t[$200+t3 shr 16 and $ff] xor t[$300+t0 shr 24] xor pk[9];
  s2 := t[t2 and $ff] xor t[$100+t3 shr 8 and $ff] xor t[$200+t0 shr 16 and $ff] xor t[$300+t1 shr 24] xor pk[10];
  s3 := t[t3 and $ff] xor t[$100+t0 shr 8 and $ff] xor t[$200+t1 shr 16 and $ff] xor t[$300+t2 shr 24] xor pk[11];
  // Round 3
  t0 := t[s0 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[12];
  t1 := t[s1 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[13];
  t2 := t[s2 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[14];
  t3 := t[s3 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[15];
  // Round 4
  s0 := t[t0 and $ff] xor t[$100+t1 shr 8 and $ff] xor t[$200+t2 shr 16 and $ff] xor t[$300+t3 shr 24] xor pk[16];
  s1 := t[t1 and $ff] xor t[$100+t2 shr 8 and $ff] xor t[$200+t3 shr 16 and $ff] xor t[$300+t0 shr 24] xor pk[17];
  s2 := t[t2 and $ff] xor t[$100+t3 shr 8 and $ff] xor t[$200+t0 shr 16 and $ff] xor t[$300+t1 shr 24] xor pk[18];
  s3 := t[t3 and $ff] xor t[$100+t0 shr 8 and $ff] xor t[$200+t1 shr 16 and $ff] xor t[$300+t2 shr 24] xor pk[19];
  // Round 5
  t0 := t[s0 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[20];
  t1 := t[s1 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[21];
  t2 := t[s2 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[22];
  t3 := t[s3 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[23];
  // Round 6
  s0 := t[t0 and $ff] xor t[$100+t1 shr 8 and $ff] xor t[$200+t2 shr 16 and $ff] xor t[$300+t3 shr 24] xor pk[24];
  s1 := t[t1 and $ff] xor t[$100+t2 shr 8 and $ff] xor t[$200+t3 shr 16 and $ff] xor t[$300+t0 shr 24] xor pk[25];
  s2 := t[t2 and $ff] xor t[$100+t3 shr 8 and $ff] xor t[$200+t0 shr 16 and $ff] xor t[$300+t1 shr 24] xor pk[26];
  s3 := t[t3 and $ff] xor t[$100+t0 shr 8 and $ff] xor t[$200+t1 shr 16 and $ff] xor t[$300+t2 shr 24] xor pk[27];
  // Round 7
  t0 := t[s0 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[28];
  t1 := t[s1 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[29];
  t2 := t[s2 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[30];
  t3 := t[s3 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[31];
  // Round 8
  s0 := t[t0 and $ff] xor t[$100+t1 shr 8 and $ff] xor t[$200+t2 shr 16 and $ff] xor t[$300+t3 shr 24] xor pk[32];
  s1 := t[t1 and $ff] xor t[$100+t2 shr 8 and $ff] xor t[$200+t3 shr 16 and $ff] xor t[$300+t0 shr 24] xor pk[33];
  s2 := t[t2 and $ff] xor t[$100+t3 shr 8 and $ff] xor t[$200+t0 shr 16 and $ff] xor t[$300+t1 shr 24] xor pk[34];
  s3 := t[t3 and $ff] xor t[$100+t0 shr 8 and $ff] xor t[$200+t1 shr 16 and $ff] xor t[$300+t2 shr 24] xor pk[35];
  // Round 9
  t0 := t[s0 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[36];
  t1 := t[s1 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[37];
  t2 := t[s2 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[38];
  t3 := t[s3 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[39];
  if ctxt.rounds>10 then begin
    // Round 10
    s0 := t[t0 and $ff] xor t[$100+t1 shr 8 and $ff] xor t[$200+t2 shr 16 and $ff] xor t[$300+t3 shr 24] xor pk[40];
    s1 := t[t1 and $ff] xor t[$100+t2 shr 8 and $ff] xor t[$200+t3 shr 16 and $ff] xor t[$300+t0 shr 24] xor pk[41];
    s2 := t[t2 and $ff] xor t[$100+t3 shr 8 and $ff] xor t[$200+t0 shr 16 and $ff] xor t[$300+t1 shr 24] xor pk[42];
    s3 := t[t3 and $ff] xor t[$100+t0 shr 8 and $ff] xor t[$200+t1 shr 16 and $ff] xor t[$300+t2 shr 24] xor pk[43];
    // Round 11
    t0 := t[s0 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[44];
    t1 := t[s1 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[45];
    t2 := t[s2 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[46];
    t3 := t[s3 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[47];
    if ctxt.rounds>12 then begin
      // Round 12
      s0 := t[t0 and $ff] xor t[$100+t1 shr 8 and $ff] xor t[$200+t2 shr 16 and $ff] xor t[$300+t3 shr 24] xor pk[48];
      s1 := t[t1 and $ff] xor t[$100+t2 shr 8 and $ff] xor t[$200+t3 shr 16 and $ff] xor t[$300+t0 shr 24] xor pk[49];
      s2 := t[t2 and $ff] xor t[$100+t3 shr 8 and $ff] xor t[$200+t0 shr 16 and $ff] xor t[$300+t1 shr 24] xor pk[50];
      s3 := t[t3 and $ff] xor t[$100+t0 shr 8 and $ff] xor t[$200+t1 shr 16 and $ff] xor t[$300+t2 shr 24] xor pk[51];
      // Round 13
      t0 := t[s0 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[52];
      t1 := t[s1 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[53];
      t2 := t[s2 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[54];
      t3 := t[s3 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[55];
    end;
  end;
  inc(PByte(pK), ctxt.rounds shl 4);
  bo[0] := ((SBox[t0        and $ff])        xor
            (SBox[t1 shr  8 and $ff]) shl  8 xor
            (SBox[t2 shr 16 and $ff]) shl 16 xor
            (SBox[t3 shr 24])         shl 24    ) xor pk[0];
  bo[1] := ((SBox[t1        and $ff])        xor
            (SBox[t2 shr  8 and $ff]) shl  8 xor
            (SBox[t3 shr 16 and $ff]) shl 16 xor
            (SBox[t0 shr 24])         shl 24    ) xor pk[1];
  bo[2] := ((SBox[t2        and $ff])        xor
            (SBox[t3 shr  8 and $ff]) shl  8 xor
            (SBox[t0 shr 16 and $ff]) shl 16 xor
            (SBox[t1 shr 24])         shl 24    ) xor pk[2];
  bo[3] := ((SBox[t3        and $ff])        xor
            (SBox[t0 shr  8 and $ff]) shl  8 xor
            (SBox[t1 shr 16 and $ff]) shl 16 xor
            (SBox[t2 shr 24])         shl 24    ) xor pk[3];
{$endif}
end;

{$ifdef USEPADLOCK}
procedure aesencryptpadlock(const ctxt: TAESContext; bi, bo: PWA4);
begin
  padlock_aes_encrypt(ctxt.ViaCtx,bi,bo,16);
end;
{$endif}

{$ifdef CPUX64}
procedure aesencryptx64(const ctxt: TAESContext; bi, bo: PWA4);
{$ifdef FPC}nostackframe; assembler; asm{$else}
asm // input: rcx/rdi=TAESContext, rdx/rsi=source, r8/rdx=dest
        .noframe
{$endif} // rolled optimized encryption asm version by A. Bouchez
        push    r15
        push    r14
        push    r13
        push    r12
        push    rbx
        push    rbp
        {$ifdef win64}
        push    rdi
        push    rsi
        mov     r15, r8
        mov     r12, rcx
        {$else}
        mov     r15, rdx
        mov     rdx, rsi
        mov     r12, rdi
        {$endif win64}
        movzx   r13, byte ptr [r12].TAESContext.Rounds
        mov     eax, dword ptr [rdx]
        mov     ebx, dword ptr [rdx+4H]
        mov     ecx, dword ptr [rdx+8H]
        mov     edx, dword ptr [rdx+0CH]
        xor     eax, dword ptr [r12]
        xor     ebx, dword ptr [r12+4H]
        xor     ecx, dword ptr [r12+8H]
        xor     edx, dword ptr [r12+0CH]
        sub     r13, 1
        add     r12, 16
        lea     r14, [rip+Te0]
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@round: mov     esi, eax
        mov     edi, edx
        movzx   r8d, al
        movzx   r9d, cl
        movzx   r10d, bl
        mov     r8d, dword ptr [r14+r8*4]
        mov     r9d, dword ptr [r14+r9*4]
        mov     r10d, dword ptr [r14+r10*4]
        shr     esi, 16
        shr     edi, 16
        movzx   ebp, bh
        xor     r8d, dword ptr [r14+rbp*4+400H]
        movzx   ebp, dh
        xor     r9d, dword ptr [r14+rbp*4+400H]
        movzx   ebp, ch
        xor     r10d, dword ptr [r14+rbp*4+400H]
        shr     ebx, 16
        shr     ecx, 16
        movzx   ebp, dl
        mov     edx, dword ptr [r14+rbp*4]
        movzx   ebp, cl
        xor     r8d, dword ptr [r14+rbp*4+800H]
        movzx   ebp, sil
        xor     r9d, dword ptr [r14+rbp*4+800H]
        movzx   r11, dil
        movzx   eax, ah
        shr     edi, 8
        movzx   ebp, bh
        shr     esi, 8
        xor     r10d, dword ptr [r14+r11*4+800H]
        xor     edx, dword ptr [r14+rax*4+400H]
        xor     r8d, dword ptr [r14+rdi*4+0C00H]
        xor     r9d, dword ptr [r14+rbp*4+0C00H]
        xor     r10d, dword ptr [r14+rsi*4+0C00H]
        movzx   ebp, bl
        xor     edx, dword ptr [r14+rbp*4+800H]
        mov     rbx, r10
        mov     rax, r8
        movzx   ebp, ch
        xor     edx, dword ptr [r14+rbp*4+0C00H]
        mov     rcx, r9
        xor     eax, dword ptr [r12]
        xor     ebx, dword ptr [r12+4H]
        xor     ecx, dword ptr [r12+8H]
        xor     edx, dword ptr [r12+0CH]
        add     r12, 16
        sub     r13, 1
        jnz     @round
        lea     r9, [rip+SBox]
        movzx   r8, al
        movzx   r14, byte ptr [r9+r8]
        movzx   edi, bh
        movzx   r8, byte ptr [r9+rdi]
        shl     r8d, 8
        xor     r14d, r8d
        mov     r11, rcx
        shr     r11, 16
        and     r11, 0FFH
        movzx   r8, byte ptr [r9+r11]
        shl     r8d, 16
        xor     r14d, r8d
        mov     r11, rdx
        shr     r11, 24
        movzx   r8, byte ptr [r9+r11]
        shl     r8d, 24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12]
        mov     dword ptr [r15], r14d
        movzx   r8, bl
        movzx   r14, byte ptr [r9+r8]
        movzx   edi, ch
        movzx   r8, byte ptr [r9+rdi]
        shl     r8d, 8
        xor     r14d, r8d
        mov     r11, rdx
        shr     r11, 16
        and     r11, 0FFH
        movzx   r8, byte ptr [r9+r11]
        shl     r8d, 16
        xor     r14d, r8d
        mov     r11, rax
        shr     r11, 24
        movzx   r8, byte ptr [r9+r11]
        shl     r8d, 24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12+4H]
        mov     dword ptr [r15+4H], r14d
        movzx   r8, cl
        movzx   r14, byte ptr [r9+r8]
        movzx   edi, dh
        movzx   r8, byte ptr [r9+rdi]
        shl     r8d, 8
        xor     r14d, r8d
        mov     r11, rax
        shr     r11, 16
        and     r11, 0FFH
        movzx   r8, byte ptr [r9+r11]
        shl     r8d, 16
        xor     r14d, r8d
        mov     r11, rbx
        shr     r11, 24
        movzx   r8, byte ptr [r9+r11]
        shl     r8d, 24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12+8H]
        mov     dword ptr [r15+8H], r14d
        and     rdx, 0FFH
        movzx   r14, byte ptr [r9+rdx]
        movzx   eax, ah
        movzx   r8, byte ptr [r9+rax]
        shl     r8d, 8
        xor     r14d, r8d
        shr     rbx, 16
        and     rbx, 0FFH
        movzx   r8, byte ptr [r9+rbx]
        shl     r8d, 16
        xor     r14d, r8d
        shr     rcx, 24
        movzx   r8, byte ptr [r9+rcx]
        shl     r8d, 24
        xor     r14d, r8d
        xor     r14d, dword ptr [r12+0CH]
        mov     dword ptr [r15+0CH], r14d
        {$ifdef win64}
        pop     rsi
        pop     rdi
        {$endif win64}
        pop     rbp
        pop     rbx
        pop     r12
        pop     r13
        pop     r14
        pop     r15
end;
{$endif CPUX64}

{$ifdef CPUX86_NOTPIC}
procedure aesencrypt386(const ctxt: TAESContext; bi, bo: PWA4);
  {$ifdef FPC} nostackframe; assembler; {$endif}
asm // rolled optimized encryption asm version by A. Bouchez
        push    ebx
        push    esi
        push    edi
        push    ebp
        add     esp,  - 24
        mov     [esp + 4], ecx
        mov     ecx, eax // ecx=pk
        movzx   eax, byte ptr[eax].taescontext.rounds
        dec     eax
        mov     [esp + 20], eax
        mov     ebx, [edx]
        xor     ebx, [ecx]
        mov     esi, [edx + 4]
        xor     esi, [ecx + 4]
        mov     eax, [edx + 8]
        xor     eax, [ecx + 8]
        mov     edx, [edx + 12]
        xor     edx, [ecx + 12]
        lea     ecx, [ecx + 16]
@1:     // pk=ecx s0=ebx s1=esi s2=eax s3=edx
        movzx   edi, bl
        mov     edi, dword ptr[4 * edi + te0]
        movzx   ebp, si
        shr     ebp, $08
        xor     edi, dword ptr[4 * ebp + te1]
        mov     ebp, eax
        shr     ebp, $10
        and     ebp, $ff
        xor     edi, dword ptr[4 * ebp + te2]
        mov     ebp, edx
        shr     ebp, $18
        xor     edi, dword ptr[4 * ebp + te3]
        mov     [esp + 8], edi
        mov     edi, esi
        and     edi, 255
        mov     edi, dword ptr[4 * edi + te0]
        movzx   ebp, ax
        shr     ebp, $08
        xor     edi, dword ptr[4 * ebp + te1]
        mov     ebp, edx
        shr     ebp, $10
        and     ebp, 255
        xor     edi, dword ptr[4 * ebp + te2]
        mov     ebp, ebx
        shr     ebp, $18
        xor     edi, dword ptr[4 * ebp + te3]
        mov     [esp + 12], edi
        movzx   edi, al
        mov     edi, dword ptr[4 * edi + te0]
        movzx   ebp, dh
        xor     edi, dword ptr[4 * ebp + te1]
        mov     ebp, ebx
        shr     ebp, $10
        and     ebp, 255
        xor     edi, dword ptr[4 * ebp + te2]
        mov     ebp, esi
        shr     ebp, $18
        xor     edi, dword ptr[4 * ebp + te3]
        mov     [esp + 16], edi
        and     edx, 255
        mov     edx, dword ptr[4 * edx + te0]
        shr     ebx, $08
        and     ebx, 255
        xor     edx, dword ptr[4 * ebx + te1]
        shr     esi, $10
        and     esi, 255
        xor     edx, dword ptr[4 * esi + te2]
        shr     eax, $18
        xor     edx, dword ptr[4 * eax + te3]
        mov     ebx, [ecx]
        xor     ebx, [esp + 8]
        mov     esi, [ecx + 4]
        xor     esi, [esp + 12]
        mov     eax, [ecx + 8]
        xor     eax, [esp + 16]
        xor     edx, [ecx + 12]
        lea     ecx, [ecx + 16]
        dec     byte ptr[esp + 20]
        jne     @1
        mov     ebp, ecx // ebp=pk
        movzx   ecx, bl
        mov     edi, esi
        movzx   ecx, byte ptr[ecx + SBox]
        shr     edi, $08
        and     edi, 255
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $08
        xor     ecx, edi
        mov     edi, eax
        shr     edi, $10
        and     edi, 255
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $10
        xor     ecx, edi
        mov     edi, edx
        shr     edi, $18
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $18
        xor     ecx, edi
        xor     ecx, [ebp]
        mov     edi, [esp + 4]
        mov     [edi], ecx
        mov     ecx, esi
        and     ecx, 255
        movzx   ecx, byte ptr[ecx + SBox]
        movzx   edi, ah
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $08
        xor     ecx, edi
        mov     edi, edx
        shr     edi, $10
        and     edi, 255
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $10
        xor     ecx, edi
        mov     edi, ebx
        shr     edi, $18
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $18
        xor     ecx, edi
        xor     ecx, [ebp + 4]
        mov     edi, [esp + 4]
        mov     [edi + 4], ecx
        mov     ecx, eax
        and     ecx, 255
        movzx   ecx, byte ptr[ecx + SBox]
        movzx   edi, dh
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $08
        xor     ecx, edi
        mov     edi, ebx
        shr     edi, $10
        and     edi, 255
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $10
        xor     ecx, edi
        mov     edi, esi
        shr     edi, $18
        movzx   edi, byte ptr[edi + SBox]
        shl     edi, $18
        xor     ecx, edi
        xor     ecx, [ebp + 8]
        mov     edi, [esp + 4]
        mov     [edi + 8], ecx
        and     edx, 255
        movzx   edx, byte ptr[edx + SBox]
        shr     ebx, $08
        and     ebx, 255
        xor     ecx, ecx
        mov     cl, byte ptr[ebx + SBox]
        shl     ecx, $08
        xor     edx, ecx
        shr     esi, $10
        and     esi, 255
        xor     ecx, ecx
        mov     cl, byte ptr[esi + SBox]
        shl     ecx, $10
        xor     edx, ecx
        shr     eax, $18
        movzx   eax, byte ptr[eax + SBox]
        shl     eax, $18
        xor     edx, eax
        xor     edx, [ebp + 12]
        mov     eax, [esp + 4]
        mov     [eax + 12], edx
        add     esp, 24
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;
{$endif CPUX86_NOTPIC}

procedure TAES.Encrypt(const BI: TAESBlock; var BO: TAESBlock);
begin
  TAESContext(Context).DoBlock(Context,BI,BO);
end;

{$ifdef USEAESNI} // should be put outside the main method for FPC :(
procedure ShiftAesNi(KeySize: cardinal; pk: pointer);
{$ifdef CPU32} {$ifdef FPC} nostackframe; assembler; {$endif}
asm // eax=KeySize edx=pk
        movups  xmm1, [edx]
        movups  xmm5, dqword ptr[@mask]
        cmp     al, 128
        je      @128
        cmp     al, 192
        je      @e // 192 bits is very complicated -> skip by now (use 128+256)
@256:   movups  xmm3, [edx + 16]
        add     edx, 32
        db      $66, $0F, $3A, $DF, $D3, $01 // aeskeygenassist xmm2,xmm3,1
        call    @exp256
        db      $66, $0F, $3A, $DF, $D3, $02 // aeskeygenassist xmm2,xmm3,2
        call    @exp256
        db      $66, $0F, $3A, $DF, $D3, $04 // aeskeygenassist xmm2,xmm3,4
        call    @exp256
        db      $66, $0F, $3A, $DF, $D3, $08 // aeskeygenassist xmm2,xmm3,8
        call    @exp256
        db      $66, $0F, $3A, $DF, $D3, $10 // aeskeygenassist xmm2,xmm3,$10
        call    @exp256
        db      $66, $0F, $3A, $DF, $D3, $20 // aeskeygenassist xmm2,xmm3,$20
        call    @exp256
        db      $66, $0F, $3A, $DF, $D3, $40 // aeskeygenassist xmm2,xmm3,$40
        pshufd  xmm2, xmm2, $FF
        movups  xmm4, xmm1
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm1, xmm4
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm1, xmm4
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm1, xmm4
        pxor    xmm1, xmm2
        movups  [edx], xmm1
        jmp     @e
@mask:  dd      $ffffffff
        dd      $03020100
        dd      $07060504
        dd      $0b0a0908
@exp256:pshufd  xmm2, xmm2, $ff
        movups  xmm4, xmm1
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm1, xmm4
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm1, xmm4
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm1, xmm4
        pxor    xmm1, xmm2
        movups  [edx], xmm1
        add     edx, $10
        db      $66, $0F, $3A, $DF, $E1, $00 // aeskeygenassist xmm4,xmm1,0
        pshufd  xmm2, xmm4, $AA
        movups  xmm4, xmm3
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm3, xmm4
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm3, xmm4
        db      $66, $0F, $38, $00, $E5 // pshufb xmm4,xmm5
        pxor    xmm3, xmm4
        pxor    xmm3, xmm2
        movups  [edx], xmm3
        add     edx, $10
        ret
@exp128:pshufd  xmm2, xmm2, $FF
        movups  xmm3, xmm1
        db      $66, $0F, $38, $00, $DD // pshufb xmm3,xmm5
        pxor    xmm1, xmm3
        db      $66, $0F, $38, $00, $DD // pshufb xmm3,xmm5
        pxor    xmm1, xmm3
        db      $66, $0F, $38, $00, $DD // pshufb xmm3,xmm5
        pxor    xmm1, xmm3
        pxor    xmm1, xmm2
        movups  [edx], xmm1
        add     edx, $10
        ret
@128:   add     edx, 16
        db      $66, $0F, $3A, $DF, $D1, $01 // aeskeygenassist xmm2,xmm1,1
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $02 // aeskeygenassist xmm2,xmm1,2
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $04 // aeskeygenassist xmm2,xmm1,4
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $08 // aeskeygenassist xmm2,xmm1,8
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $10 // aeskeygenassist xmm2,xmm1,$10
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $20 // aeskeygenassist xmm2,xmm1,$20
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $40 // aeskeygenassist xmm2,xmm1,$40
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $80 // aeskeygenassist xmm2,xmm1,$80
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $1b // aeskeygenassist xmm2,xmm1,$1b
        call    @exp128
        db      $66, $0F, $3A, $DF, $D1, $36 // aeskeygenassist xmm2,xmm1,$36
        call    @exp128
@e:     db      $f3 // rep ret
end;
{$endif CPU32}
{$ifdef CPU64}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     eax, keysize
        movups  xmm1, dqword ptr[pk]
        movaps  xmm5, dqword ptr[rip + @mask]
        cmp     al, 128
        je      @128
        cmp     al, 192
        je      @e // 192 bits is very complicated -> skip by now (128+256)
@256:   movups  xmm3, dqword ptr[pk + 16]
        add     pk, 32
        aeskeygenassist xmm2, xmm3, 1
        call    @exp256
        aeskeygenassist xmm2, xmm3, 2
        call    @exp256
        aeskeygenassist xmm2, xmm3, 4
        call    @exp256
        aeskeygenassist xmm2, xmm3, 8
        call    @exp256
        aeskeygenassist xmm2, xmm3, $10
        call    @exp256
        aeskeygenassist xmm2, xmm3, $20
        call    @exp256
        aeskeygenassist xmm2, xmm3, $40
        pshufd  xmm2, xmm2, $FF
        movups  xmm4, xmm1
        pshufb  xmm4, xmm5
        pxor    xmm1, xmm4
        pshufb  xmm4, xmm5
        pxor    xmm1, xmm4
        pshufb  xmm4, xmm5
        pxor    xmm1, xmm4
        pxor    xmm1, xmm2
        movups  dqword ptr[pk], xmm1
        jmp     @e
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@mask:  dd      $ffffffff
        dd      $03020100
        dd      $07060504
        dd      $0b0a0908
@exp256:pshufd  xmm2, xmm2, $ff
        movups  xmm4, xmm1
        pshufb  xmm4, xmm5
        pxor    xmm1, xmm4
        pshufb  xmm4, xmm5
        pxor    xmm1, xmm4
        pshufb  xmm4, xmm5
        pxor    xmm1, xmm4
        pxor    xmm1, xmm2
        movups  dqword ptr[pk], xmm1
        add     pk, $10
        aeskeygenassist xmm4, xmm1, 0
        pshufd  xmm2, xmm4, $AA
        movups  xmm4, xmm3
        pshufb  xmm4, xmm5
        pxor    xmm3, xmm4
        pshufb  xmm4, xmm5
        pxor    xmm3, xmm4
        pshufb  xmm4, xmm5
        pxor    xmm3, xmm4
        pxor    xmm3, xmm2
        movups  dqword ptr[pk], xmm3
        add     pk, $10
@e:     ret
@exp128:pshufd  xmm2, xmm2, $FF
        movups  xmm3, xmm1
        pshufb  xmm3, xmm5
        pxor    xmm1, xmm3
        pshufb  xmm3, xmm5
        pxor    xmm1, xmm3
        pshufb  xmm3, xmm5
        pxor    xmm1, xmm3
        pxor    xmm1, xmm2
        movups  dqword ptr[pk], xmm1
        add     pk, $10
        ret
@128:   add     pk, 16
        aeskeygenassist xmm2, xmm1, 1
        call    @exp128
        aeskeygenassist xmm2, xmm1, 2
        call    @exp128
        aeskeygenassist xmm2, xmm1, 4
        call    @exp128
        aeskeygenassist xmm2, xmm1, 8
        call    @exp128
        aeskeygenassist xmm2, xmm1, $10
        call    @exp128
        aeskeygenassist xmm2, xmm1, $20
        call    @exp128
        aeskeygenassist xmm2, xmm1, $40
        call    @exp128
        aeskeygenassist xmm2, xmm1, $80
        call    @exp128
        aeskeygenassist xmm2, xmm1, $1b
        call    @exp128
        aeskeygenassist xmm2, xmm1, $36
        call    @exp128
end;
{$endif CPU64}
{$endif USEAESNI}

function TAES.EncryptInit(const Key; KeySize: cardinal): boolean;
  procedure Shift(KeySize: cardinal; pk: PAWK);
  var i: integer;
      temp: cardinal;
  begin
    // 32 bit use shift and mask
    case KeySize of
    128:
      for i := 0 to 9 do begin
        temp := pK^[3];
        // SubWord(RotWord(temp)) if "word" count mod 4 = 0
        pK^[4] := ((SBox[(temp shr  8) and $ff])       ) xor
                  ((SBox[(temp shr 16) and $ff]) shl  8) xor
                  ((SBox[(temp shr 24)        ]) shl 16) xor
                  ((SBox[(temp       ) and $ff]) shl 24) xor
                  pK^[0] xor RCon[i];
        pK^[5] := pK^[1] xor pK^[4];
        pK^[6] := pK^[2] xor pK^[5];
        pK^[7] := pK^[3] xor pK^[6];
        inc(PByte(pK),4*4);
      end;
    192:
      for i := 0 to 7 do begin
        temp := pK^[5];
        // SubWord(RotWord(temp)) if "word" count mod 6 = 0
        pK^[ 6] := ((SBox[(temp shr  8) and $ff])       ) xor
                   ((SBox[(temp shr 16) and $ff]) shl  8) xor
                   ((SBox[(temp shr 24)        ]) shl 16) xor
                   ((SBox[(temp       ) and $ff]) shl 24) xor
                   pK^[0] xor RCon[i];
        pK^[ 7] := pK^[1] xor pK^[6];
        pK^[ 8] := pK^[2] xor pK^[7];
        pK^[ 9] := pK^[3] xor pK^[8];
        if i=7 then exit;
        pK^[10] := pK^[4] xor pK^[ 9];
        pK^[11] := pK^[5] xor pK^[10];
        inc(PByte(pK),6*4);
      end;
    else // 256:
      for i := 0 to 6 do begin
        temp := pK^[7];
        // SubWord(RotWord(temp)) if "word" count mod 8 = 0
        pK^[ 8] := ((SBox[(temp shr  8) and $ff])       ) xor
                   ((SBox[(temp shr 16) and $ff]) shl  8) xor
                   ((SBox[(temp shr 24)        ]) shl 16) xor
                   ((SBox[(temp       ) and $ff]) shl 24) xor
                   pK^[0] xor RCon[i];
        pK^[ 9] := pK^[1] xor pK^[ 8];
        pK^[10] := pK^[2] xor pK^[ 9];
        pK^[11] := pK^[3] xor pK^[10];
        if i=6 then exit;
        temp := pK^[11];
        // SubWord(temp) if "word" count mod 8 = 4
        pK^[12] := ((SBox[(temp       ) and $ff])       ) xor
                   ((SBox[(temp shr  8) and $ff]) shl  8) xor
                   ((SBox[(temp shr 16) and $ff]) shl 16) xor
                   ((SBox[(temp shr 24)        ]) shl 24) xor
                   pK^[4];
        pK^[13] := pK^[5] xor pK^[12];
        pK^[14] := pK^[6] xor pK^[13];
        pK^[15] := pK^[7] xor pK^[14];
        inc(PByte(pK),8*4);
      end;
    end;
  end;
var Nk: integer;
    ctx: TAESContext absolute Context;
begin
  result := true;
  ctx.Initialized := true;
  {$ifdef USEPADLOCK}
  if DoPadlockInit(Key,KeySize) then begin
    ctx.DoBlock := @aesencryptpadlock;
    exit; // Init OK
  end;
  {$endif}
  if (KeySize<>128) and (KeySize<>192) and (KeySize<>256) then begin
    result := false;
    ctx.Initialized := false;
    exit;
  end;
  Nk := KeySize div 32;
  MoveFast(Key, ctx.RK, 4*Nk);
  // aes128ofb: aesencryptpas=140MB/s aesencryptx64=200MB/s aesniencrypt=500MB/s
  {$ifdef CPUX64}
  ctx.DoBlock := @aesencryptx64;
  {$else}
  {$ifdef CPUX86_NOTPIC}
  ctx.DoBlock := @aesencrypt386;
  {$else}
  ctx.DoBlock := @aesencryptpas;
  {$endif}
  {$endif}
  {$ifdef CPUINTEL}
  {$ifdef USEAESNI}
  if cfAESNI in CpuFeatures then begin
     case KeySize of
     128: ctx.DoBlock := @aesniencrypt128;
     192: ctx.DoBlock := @aesniencrypt192;
     256: ctx.DoBlock := @aesniencrypt256;
     end;
     {$ifdef USEAESNI32}
     case KeySize of
     128: ctx.AesNi32 := @AesNiEncryptXmm7_128;
     192: ctx.AesNi32 := @AesNiEncryptXmm7_192;
     256: ctx.AesNi32 := @AesNiEncryptXmm7_256;
     end;
     {$endif}
  end;
  {$endif}
  {$endif}
  ctx.Rounds  := 6+Nk;
  ctx.KeyBits := KeySize;
  // Calculate encryption round keys
  {$ifdef USEAESNI} // 192 is more complex and seldom used -> skip to pascal
  if (KeySize<>192) and (cfAESNI in CpuFeatures) then
    ShiftAesNi(KeySize,@ctx.RK) else
  {$endif}
    Shift(KeySize,pointer(@ctx.RK));
end;

{$ifdef USEAESNI} // should be put outside the main method for FPC :(
{$ifdef CPU32}
procedure MakeDecrKeyAesNi(Rounds: integer; RK: Pointer);
  {$ifdef FPC} nostackframe; assembler; {$endif}
asm // eax=Rounds edx=RK
        sub     eax, 9
        movups  xmm0, [edx + $10]
        movups  xmm1, [edx + $20]
        movups  xmm2, [edx + $30]
        movups  xmm3, [edx + $40]
        movups  xmm4, [edx + $50]
        movups  xmm5, [edx + $60]
        movups  xmm6, [edx + $70]
        movups  xmm7, [edx + $80]
  {$ifdef HASAESNI}
        aesimc  xmm0, xmm0
        aesimc  xmm1, xmm1
        aesimc  xmm2, xmm2
        aesimc  xmm3, xmm3
        aesimc  xmm4, xmm4
        aesimc  xmm5, xmm5
        aesimc  xmm6, xmm6
        aesimc  xmm7, xmm7
  {$else}
        db      $66, $0F, $38, $DB, $C0
        db      $66, $0F, $38, $DB, $C9
        db      $66, $0F, $38, $DB, $D2
        db      $66, $0F, $38, $DB, $DB
        db      $66, $0F, $38, $DB, $E4
        db      $66, $0F, $38, $DB, $ED
        db      $66, $0F, $38, $DB, $F6
        db      $66, $0F, $38, $DB, $FF
  {$endif}
        movups  [edx + $10], xmm0
        movups  [edx + $20], xmm1
        movups  [edx + $30], xmm2
        movups  [edx + $40], xmm3
        movups  [edx + $50], xmm4
        movups  [edx + $60], xmm5
        movups  [edx + $70], xmm6
        movups  [edx + $80], xmm7
        lea     edx, [edx + $90]
@loop:  movups  xmm0, [edx]
        db      $66, $0F, $38, $DB, $C0 // aesimc xmm0,xmm0
        movups  [edx], xmm0
        dec     eax
        lea     edx, [edx + 16]
        jnz     @loop
end;
{$endif CPU32}
{$ifdef CPU64}
procedure MakeDecrKeyAesNi(Rounds: integer; RK: Pointer);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     eax, Rounds
        sub     eax, 9
        movups  xmm0, dqword ptr[RK + $10]
        movups  xmm1, dqword ptr[RK + $20]
        movups  xmm2, dqword ptr[RK + $30]
        movups  xmm3, dqword ptr[RK + $40]
        movups  xmm4, dqword ptr[RK + $50]
        movups  xmm5, dqword ptr[RK + $60]
        movups  xmm6, dqword ptr[RK + $70]
        movups  xmm7, dqword ptr[RK + $80]
        aesimc  xmm0, xmm0
        aesimc  xmm1, xmm1
        aesimc  xmm2, xmm2
        aesimc  xmm3, xmm3
        aesimc  xmm4, xmm4
        aesimc  xmm5, xmm5
        aesimc  xmm6, xmm6
        aesimc  xmm7, xmm7
        movups  dqword ptr[RK + $10], xmm0
        movups  dqword ptr[RK + $20], xmm1
        movups  dqword ptr[RK + $30], xmm2
        movups  dqword ptr[RK + $40], xmm3
        movups  dqword ptr[RK + $50], xmm4
        movups  dqword ptr[RK + $60], xmm5
        movups  dqword ptr[RK + $70], xmm6
        movups  dqword ptr[RK + $80], xmm7
        lea     RK, [RK + $90]
@loop:  movups  xmm0, dqword ptr[RK]
        aesimc  xmm0, xmm0
        movups  dqword ptr[RK], xmm0
        dec     eax
        lea     RK, [RK + 16]
        jnz     @loop
end;
{$endif CPU64}
{$endif USEAESNI}

{$ifdef USEPADLOCK}
procedure aesdecryptpadlock(const ctxt: TAESContext; bi, bo: PWA4);
begin
  padlock_aes_decrypt(ctxt.ViaCtx,bi,bo,16);
end;
{$endif}

procedure aesdecryptpas(const ctxt: TAESContext; bi, bo: PWA4);
var
  {$ifdef AES_ROLLED}
  s0,s1,s2,s3: PtrUInt;  // TAESBlock s# as separate variables
  t0,t1,t2: cardinal;    // TAESBlock t# as separate variables
  i: integer;
  pk: PWA4;
  {$else}
  s0,s1,s2,s3,t0,t1,t2,t3: cardinal; // TAESBlock s#/t# as separate variables
  pk: PAWk;  // pointer to loop rount key
  {$endif}
  t: PCardinalArray;    // faster on a PIC system
begin
  t := @Td0;
{$ifdef AES_ROLLED}
  // Wolfgang Ehrhardt rolled version - faster on modern CPU than unrolled one below
  // Setup key pointer
  pk := PWA4(@ctxt.RK[ctxt.Rounds]);
  // Initialize with input block
  s0 := bi[0] xor pk[0];
  s1 := bi[1] xor pk[1];
  s2 := bi[2] xor pk[2];
  s3 := bi[3] xor pk[3];
  dec(pk);
  for I := 1 to ctxt.Rounds-1 do begin
      t0 := t[s0 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s1 shr 24];
      t1 := t[s1 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s2 shr 24];
      t2 := t[s2 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s3 shr 24];
      s3 := t[s3 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[3];
      s0 := t0 xor pk[0];
      s1 := t1 xor pk[1];
      s2 := t2 xor pk[2];
      dec(pk);
    end;
  bo[0] := ((InvSBox[s0        and $ff])        xor
            (InvSBox[s3 shr  8 and $ff]) shl  8 xor
            (InvSBox[s2 shr 16 and $ff]) shl 16 xor
            (InvSBox[s1 shr 24])         shl 24    ) xor pk[0];
  bo[1] := ((InvSBox[s1        and $ff])        xor
            (InvSBox[s0 shr  8 and $ff]) shl  8 xor
            (InvSBox[s3 shr 16 and $ff]) shl 16 xor
            (InvSBox[s2 shr 24])         shl 24    ) xor pk[1];
  bo[2] := ((InvSBox[s2        and $ff])        xor
            (InvSBox[s1 shr  8 and $ff]) shl  8 xor
            (InvSBox[s0 shr 16 and $ff]) shl 16 xor
            (InvSBox[s3 shr 24])         shl 24    ) xor pk[2];
  bo[3] := ((InvSBox[s3        and $ff])        xor
            (InvSBox[s2 shr  8 and $ff]) shl  8 xor
            (InvSBox[s1 shr 16 and $ff]) shl 16 xor
            (InvSBox[s0 shr 24])         shl 24    ) xor pk[3];
{$else} // unrolled version (WE6) from Wolfgang Ehrhardt - slower
  // Setup key pointer
  pk := PAWk(@ctxt.RK);
  // Initialize with input block
  s0 := bi[0] xor pk[0];
  s1 := bi[1] xor pk[1];
  s2 := bi[2] xor pk[2];
  s3 := bi[3] xor pk[3];
  // Round 1
  t0 := t[s0 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[4];
  t1 := t[s1 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[5];
  t2 := t[s2 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[6];
  t3 := t[s3 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[7];
  // Round 2
  s0 := t[t0 and $ff] xor t[$100+t3 shr 8 and $ff] xor t[$200+t2 shr 16 and $ff] xor t[$300+t1 shr 24] xor pk[8];
  s1 := t[t1 and $ff] xor t[$100+t0 shr 8 and $ff] xor t[$200+t3 shr 16 and $ff] xor t[$300+t2 shr 24] xor pk[9];
  s2 := t[t2 and $ff] xor t[$100+t1 shr 8 and $ff] xor t[$200+t0 shr 16 and $ff] xor t[$300+t3 shr 24] xor pk[10];
  s3 := t[t3 and $ff] xor t[$100+t2 shr 8 and $ff] xor t[$200+t1 shr 16 and $ff] xor t[$300+t0 shr 24] xor pk[11];
  // Round 3
  t0 := t[s0 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[12];
  t1 := t[s1 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[13];
  t2 := t[s2 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[14];
  t3 := t[s3 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[15];
  // Round 4
  s0 := t[t0 and $ff] xor t[$100+t3 shr 8 and $ff] xor t[$200+t2 shr 16 and $ff] xor t[$300+t1 shr 24] xor pk[16];
  s1 := t[t1 and $ff] xor t[$100+t0 shr 8 and $ff] xor t[$200+t3 shr 16 and $ff] xor t[$300+t2 shr 24] xor pk[17];
  s2 := t[t2 and $ff] xor t[$100+t1 shr 8 and $ff] xor t[$200+t0 shr 16 and $ff] xor t[$300+t3 shr 24] xor pk[18];
  s3 := t[t3 and $ff] xor t[$100+t2 shr 8 and $ff] xor t[$200+t1 shr 16 and $ff] xor t[$300+t0 shr 24] xor pk[19];
  // Round 5
  t0 := t[s0 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[20];
  t1 := t[s1 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[21];
  t2 := t[s2 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[22];
  t3 := t[s3 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[23];
  // Round 6
  s0 := t[t0 and $ff] xor t[$100+t3 shr 8 and $ff] xor t[$200+t2 shr 16 and $ff] xor t[$300+t1 shr 24] xor pk[24];
  s1 := t[t1 and $ff] xor t[$100+t0 shr 8 and $ff] xor t[$200+t3 shr 16 and $ff] xor t[$300+t2 shr 24] xor pk[25];
  s2 := t[t2 and $ff] xor t[$100+t1 shr 8 and $ff] xor t[$200+t0 shr 16 and $ff] xor t[$300+t3 shr 24] xor pk[26];
  s3 := t[t3 and $ff] xor t[$100+t2 shr 8 and $ff] xor t[$200+t1 shr 16 and $ff] xor t[$300+t0 shr 24] xor pk[27];
  // Round 7
  t0 := t[s0 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[28];
  t1 := t[s1 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[29];
  t2 := t[s2 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[30];
  t3 := t[s3 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[31];
  // Round 8
  s0 := t[t0 and $ff] xor t[$100+t3 shr 8 and $ff] xor t[$200+t2 shr 16 and $ff] xor t[$300+t1 shr 24] xor pk[32];
  s1 := t[t1 and $ff] xor t[$100+t0 shr 8 and $ff] xor t[$200+t3 shr 16 and $ff] xor t[$300+t2 shr 24] xor pk[33];
  s2 := t[t2 and $ff] xor t[$100+t1 shr 8 and $ff] xor t[$200+t0 shr 16 and $ff] xor t[$300+t3 shr 24] xor pk[34];
  s3 := t[t3 and $ff] xor t[$100+t2 shr 8 and $ff] xor t[$200+t1 shr 16 and $ff] xor t[$300+t0 shr 24] xor pk[35];
  // Round 9
  t0 := t[s0 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[36];
  t1 := t[s1 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[37];
  t2 := t[s2 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[38];
  t3 := t[s3 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[39];
  if ctxt.rounds>10 then begin
    // Round 10
    s0 := t[t0 and $ff] xor t[$100+t3 shr 8 and $ff] xor t[$200+t2 shr 16 and $ff] xor t[$300+t1 shr 24] xor pk[40];
    s1 := t[t1 and $ff] xor t[$100+t0 shr 8 and $ff] xor t[$200+t3 shr 16 and $ff] xor t[$300+t2 shr 24] xor pk[41];
    s2 := t[t2 and $ff] xor t[$100+t1 shr 8 and $ff] xor t[$200+t0 shr 16 and $ff] xor t[$300+t3 shr 24] xor pk[42];
    s3 := t[t3 and $ff] xor t[$100+t2 shr 8 and $ff] xor t[$200+t1 shr 16 and $ff] xor t[$300+t0 shr 24] xor pk[43];
    // Round 11
    t0 := t[s0 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[44];
    t1 := t[s1 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[45];
    t2 := t[s2 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[46];
    t3 := t[s3 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[47];
    if ctxt.rounds>12 then begin
      // Round 12
      s0 := t[t0 and $ff] xor t[$100+t3 shr 8 and $ff] xor t[$200+t2 shr 16 and $ff] xor t[$300+t1 shr 24] xor pk[48];
      s1 := t[t1 and $ff] xor t[$100+t0 shr 8 and $ff] xor t[$200+t3 shr 16 and $ff] xor t[$300+t2 shr 24] xor pk[49];
      s2 := t[t2 and $ff] xor t[$100+t1 shr 8 and $ff] xor t[$200+t0 shr 16 and $ff] xor t[$300+t3 shr 24] xor pk[50];
      s3 := t[t3 and $ff] xor t[$100+t2 shr 8 and $ff] xor t[$200+t1 shr 16 and $ff] xor t[$300+t0 shr 24] xor pk[51];
      // Round 13
      t0 := t[s0 and $ff] xor t[$100+s3 shr 8 and $ff] xor t[$200+s2 shr 16 and $ff] xor t[$300+s1 shr 24] xor pk[52];
      t1 := t[s1 and $ff] xor t[$100+s0 shr 8 and $ff] xor t[$200+s3 shr 16 and $ff] xor t[$300+s2 shr 24] xor pk[53];
      t2 := t[s2 and $ff] xor t[$100+s1 shr 8 and $ff] xor t[$200+s0 shr 16 and $ff] xor t[$300+s3 shr 24] xor pk[54];
      t3 := t[s3 and $ff] xor t[$100+s2 shr 8 and $ff] xor t[$200+s1 shr 16 and $ff] xor t[$300+s0 shr 24] xor pk[55];
    end;
  end;
  inc(PByte(pk), (ctxt.rounds shl 4));
  // Uses InvSBox and shl, needs type cast cardinal() for
  // 16 bit compilers: here InvSBox is byte, Td4 is cardinal
  bo[0] := ((InvSBox[t0 and $ff]) xor
    (InvSBox[t3 shr  8 and $ff]) shl  8 xor
    (InvSBox[t2 shr 16 and $ff]) shl 16 xor
    (InvSBox[t1 shr 24]) shl 24) xor pk[0];
  bo[1] := ((InvSBox[t1 and $ff]) xor
    (InvSBox[t0 shr  8 and $ff]) shl  8 xor
    (InvSBox[t3 shr 16 and $ff]) shl 16 xor
    (InvSBox[t2 shr 24]) shl 24) xor pk[1];
  bo[2] := ((InvSBox[t2 and $ff]) xor
    (InvSBox[t1 shr  8 and $ff]) shl  8 xor
    (InvSBox[t0 shr 16 and $ff]) shl 16 xor
    (InvSBox[t3 shr 24]) shl 24) xor pk[2];
  bo[3] := ((InvSBox[t3 and $ff]) xor
    (InvSBox[t2 shr  8 and $ff]) shl  8 xor
    (InvSBox[t1 shr 16 and $ff]) shl 16 xor
    (InvSBox[t0 shr 24]) shl 24) xor pk[3];
{$endif AES_ROLLED}
end;

{$ifdef CPUX86}
{$ifdef USEAESNI}
procedure aesnidecrypt128(const ctxt, source, dest);
  {$ifdef FPC} nostackframe; assembler; {$endif}
asm
        movups  xmm7, [edx]
        movups  xmm0, [eax + 16 * 10]
        movups  xmm1, [eax + 16 * 9]
        movups  xmm2, [eax + 16 * 8]
        movups  xmm3, [eax + 16 * 7]
        movups  xmm4, [eax + 16 * 6]
        movups  xmm5, [eax + 16 * 5]
        movups  xmm6, [eax + 16 * 4]
        pxor    xmm7, xmm0
  {$ifdef HASAESNI}
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
  {$else}
        db      $66, $0F, $38, $DE, $F9
        db      $66, $0F, $38, $DE, $FA
        db      $66, $0F, $38, $DE, $FB
        db      $66, $0F, $38, $DE, $FC
  {$endif}
        movups  xmm0, [eax + 16 * 3]
        movups  xmm1, [eax + 16 * 2]
        movups  xmm2, [eax + 16 * 1]
        movups  xmm3, [eax + 16 * 0]
  {$ifdef HASAESNI}
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
        aesdec  xmm7, xmm0
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdeclast xmm7, xmm3
  {$else}
        db      $66, $0F, $38, $DE, $FD
        db      $66, $0F, $38, $DE, $FE
        db      $66, $0F, $38, $DE, $F8
        db      $66, $0F, $38, $DE, $F9
        db      $66, $0F, $38, $DE, $FA
        db      $66, $0F, $38, $DF, $FB
  {$endif}
        movups  [ecx], xmm7
        pxor    xmm7, xmm7
end;

procedure aesnidecrypt192(const ctxt, source, dest);
  {$ifdef FPC} nostackframe; assembler; {$endif}
asm
        movups  xmm7, [edx]
        movups  xmm0, [eax + 16 * 12]
        movups  xmm1, [eax + 16 * 11]
        movups  xmm2, [eax + 16 * 10]
        movups  xmm3, [eax + 16 * 9]
        movups  xmm4, [eax + 16 * 8]
        movups  xmm5, [eax + 16 * 7]
        movups  xmm6, [eax + 16 * 6]
        pxor    xmm7, xmm0
  {$ifdef HASAESNI}
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
  {$else}
        db      $66, $0F, $38, $DE, $F9
        db      $66, $0F, $38, $DE, $FA
        db      $66, $0F, $38, $DE, $FB
        db      $66, $0F, $38, $DE, $FC
        db      $66, $0F, $38, $DE, $FD
        db      $66, $0F, $38, $DE, $FE
  {$endif}
        movups  xmm0, [eax + 16 * 5]
        movups  xmm1, [eax + 16 * 4]
        movups  xmm2, [eax + 16 * 3]
        movups  xmm3, [eax + 16 * 2]
        movups  xmm4, [eax + 16 * 1]
        movups  xmm5, [eax + 16 * 0]
  {$ifdef HASAESNI}
        aesdec  xmm7, xmm0
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdeclast xmm7, xmm5
  {$else}
        db      $66, $0F, $38, $DE, $F8
        db      $66, $0F, $38, $DE, $F9
        db      $66, $0F, $38, $DE, $FA
        db      $66, $0F, $38, $DE, $FB
        db      $66, $0F, $38, $DE, $FC
        db      $66, $0F, $38, $DF, $FD
  {$endif}
        movups  [ecx], xmm7
        pxor    xmm7, xmm7
end;

procedure aesnidecrypt256(const ctxt, source, dest);
  {$ifdef FPC} nostackframe; assembler; {$endif}
asm
        movups  xmm7, [edx]
        movups  xmm0, [eax + 16 * 14]
        movups  xmm1, [eax + 16 * 13]
        movups  xmm2, [eax + 16 * 12]
        movups  xmm3, [eax + 16 * 11]
        movups  xmm4, [eax + 16 * 10]
        movups  xmm5, [eax + 16 * 9]
        movups  xmm6, [eax + 16 * 8]
        pxor    xmm7, xmm0
  {$ifdef HASAESNI}
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
  {$else}
        db      $66, $0F, $38, $DE, $F9
        db      $66, $0F, $38, $DE, $FA
        db      $66, $0F, $38, $DE, $FB
        db      $66, $0F, $38, $DE, $FC
        db      $66, $0F, $38, $DE, $FD
        db      $66, $0F, $38, $DE, $FE
  {$endif}
        movups  xmm0, [eax + 16 * 7]
        movups  xmm1, [eax + 16 * 6]
        movups  xmm2, [eax + 16 * 5]
        movups  xmm3, [eax + 16 * 4]
        movups  xmm4, [eax + 16 * 3]
        movups  xmm5, [eax + 16 * 2]
        movups  xmm6, [eax + 16 * 1]
  {$ifdef HASAESNI}
        aesdec  xmm7, xmm0
        aesdec  xmm7, xmm1
        aesdec  xmm7, xmm2
        aesdec  xmm7, xmm3
        aesdec  xmm7, xmm4
        aesdec  xmm7, xmm5
        aesdec  xmm7, xmm6
  {$else}
        db      $66, $0F, $38, $DE, $F8
        db      $66, $0F, $38, $DE, $F9
        db      $66, $0F, $38, $DE, $FA
        db      $66, $0F, $38, $DE, $FB
        db      $66, $0F, $38, $DE, $FC
        db      $66, $0F, $38, $DE, $FD
        db      $66, $0F, $38, $DE, $FE
  {$endif}
        movups  xmm0, [eax + 16 * 0]
  {$ifdef HASAESNI}
        aesdeclast xmm7, xmm0
  {$else}
        db      $66, $0F, $38, $DF, $F8
  {$endif}
        movups  [ecx], xmm7
        pxor    xmm7, xmm7
end;
{$endif}

{$ifdef CPUX86_NOTPIC}
procedure aesdecrypt386(const ctxt: TAESContext; bi, bo: PWA4);
  {$ifdef FPC} nostackframe; assembler; {$endif}
asm
        push    ebx
        push    esi
        push    edi
        push    ebp
        add     esp,  - 20
        mov     [esp], ecx
        movzx   ecx, byte ptr[eax].taescontext.rounds
        lea     esi, [4 * ecx]
        lea     ecx, [ecx - 1]
        lea     eax, [eax + 4 * esi] // eax=@ctx.rk[ctx.rounds]=pk
        mov     [esp + 16], ecx      // [esp+16]=ctx.round
        mov     ebx, [edx]
        xor     ebx, [eax]
        mov     esi, [edx + 4]
        xor     esi, [eax + 4]
        mov     ecx, [edx + 8]
        xor     ecx, [eax + 8]
        mov     edx, [edx + 12]
        xor     edx, [eax + 12]
        lea     eax, [eax - 16]
@1:     // pk=eax s0=ebx s1=esi s2=ecx s3=edx
        movzx   edi, bl
        mov     edi, dword ptr[4 * edi + td0]
        movzx   ebp, dh
        xor     edi, dword ptr[4 * ebp + td1]
        mov     ebp, ecx
        shr     ebp, $10
        and     ebp, 255
        xor     edi, dword ptr[4 * ebp + td2]
        mov     ebp, esi
        shr     ebp, $18
        xor     edi, dword ptr[4 * ebp + td3]
        mov     [esp + 4], edi
        mov     edi, esi
        and     edi, 255
        mov     edi, dword ptr[4 * edi + td0]
        movzx   ebp, bh
        xor     edi, dword ptr[4 * ebp + td1]
        mov     ebp, edx
        shr     ebp, $10
        and     ebp, 255
        xor     edi, dword ptr[4 * ebp + td2]
        mov     ebp, ecx
        shr     ebp, $18
        xor     edi, dword ptr[4 * ebp + td3]
        mov     [esp + 8], edi
        movzx   edi, cl
        mov     edi, dword ptr[4 * edi + td0]
        movzx   ebp, si
        shr     ebp, $08
        xor     edi, dword ptr[4 * ebp + td1]
        mov     ebp, ebx
        shr     ebp, $10
        and     ebp, 255
        xor     edi, dword ptr[4 * ebp + td2]
        mov     ebp, edx
        shr     ebp, $18
        xor     edi, dword ptr[4 * ebp + td3]
        mov     [esp + 12], edi
        and     edx, 255
        mov     edx, dword ptr[4 * edx + td0]
        movzx   ecx, ch
        xor     edx, dword ptr[4 * ecx + td1]
        shr     esi, $10
        and     esi, 255
        xor     edx, dword ptr[4 * esi + td2]
        shr     ebx, $18
        xor     edx, dword ptr[4 * ebx + td3]
        xor     edx, [eax + 12]
        mov     ebx, [eax]
        xor     ebx, [esp + 4]
        mov     esi, [eax + 4]
        xor     esi, [esp + 8]
        mov     ecx, [eax + 8]
        xor     ecx, [esp + 12]
        lea     eax, [eax - 16]
        dec     byte ptr[esp + 16]
        jnz     @1
        mov     ebp, eax
        movzx   eax, bl
        movzx   eax, byte ptr[eax + invsbox]
        movzx   edi, dh
        movzx   edi, byte ptr[edi + invsbox]
        shl     edi, $08
        xor     eax, edi
        mov     edi, ecx
        shr     edi, $10
        and     edi, 255
        movzx   edi, byte ptr[edi + invsbox]
        shl     edi, $10
        xor     eax, edi
        mov     edi, esi
        shr     edi, $18
        movzx   edi, byte ptr[edi + invsbox]
        shl     edi, $18
        xor     eax, edi
        xor     eax, [ebp]
        mov     edi, [esp]
        mov     [edi], eax
        mov     eax, esi
        and     eax, 255
        movzx   eax, byte ptr[eax + invsbox]
        movzx   edi, bh
        movzx   edi, byte ptr[edi + invsbox]
        shl     edi, $08
        xor     eax, edi
        mov     edi, edx
        shr     edi, $10
        and     edi, 255
        movzx   edi, byte ptr[edi + invsbox]
        shl     edi, $10
        xor     eax, edi
        mov     edi, ecx
        shr     edi, $18
        movzx   edi, byte ptr[edi + invsbox]
        shl     edi, $18
        xor     eax, edi
        xor     eax, [ebp + 4]
        mov     edi, [esp]
        mov     [edi + 4], eax
        movzx   eax, cl
        movzx   eax, byte ptr[eax + invsbox]
        movzx   edi, si
        shr     edi, $08
        movzx   edi, byte ptr[edi + invsbox]
        shl     edi, $08
        xor     eax, edi
        mov     edi, ebx
        shr     edi, $10
        and     edi, 255
        movzx   edi, byte ptr[edi + invsbox]
        shl     edi, $10
        xor     eax, edi
        mov     edi, edx
        shr     edi, $18
        movzx   edi, byte ptr[edi + invsbox]
        shl     edi, $18
        xor     eax, edi
        xor     eax, [ebp + 8]
        mov     edi, [esp]
        mov     [edi + 8], eax
        and     edx, 255
        movzx   eax, byte ptr[edx + invsbox]
        shr     ecx, $08
        and     ecx, 255
        movzx   edx, byte ptr[ecx + invsbox]
        shl     edx, $08
        xor     eax, edx
        shr     esi, $10
        and     esi, 255
        movzx   edx, byte ptr[esi + invsbox]
        shl     edx, $10
        xor     eax, edx
        shr     ebx, $18
        movzx   edx, byte ptr[ebx + invsbox]
        shl     edx, $18
        xor     eax, edx
        xor     eax, [ebp + 12]
        mov     [edi + 12], eax
        add     esp, 20
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;
{$endif CPUX86_NOTPIC}
{$endif CPUX86}

procedure MakeDecrKey(rounds: integer; k: PAWk);
// Calculate decryption key from encryption key
var x: cardinal;
    {$ifndef AES_ROLLED}
    i,j: integer;
    {$endif}
    t: PCardinalArray;  // faster on a PIC system
    s: PByteArray;
begin
  {$ifndef AES_ROLLED} // inversion is needed only for fully unrolled version
  i := 0;
  j := 4*rounds;
  while i<j do begin
    x := k[i];    k[i] := k[j];      k[j] := x;
    x := k[i+1];  k[i+1] := k[j+1];  k[j+1] := x;
    x := k[i+2];  k[i+2] := k[j+2];  k[j+2] := x;
    x := k[i+3];  k[i+3] := k[j+3];  k[j+3] := x;
    inc(i,4);
    dec(j,4);
  end;
  {$endif}
  t := @Td0;
  s := @SBox;
  repeat
    inc(PByte(k),16);
    dec(rounds);
    x := k[0];
    k[0] := t[$300+s[x shr 24]] xor t[$200+s[x shr 16 and $ff]] xor
            t[$100+s[x shr 8 and $ff]] xor t[s[x and $ff]];
    x := k[1];
    k[1] := t[$300+s[x shr 24]] xor t[$200+s[x shr 16 and $ff]] xor
            t[$100+s[x shr 8 and $ff]] xor t[s[x and $ff]];
    x := k[2];
    k[2] := t[$300+s[x shr 24]] xor t[$200+s[x shr 16 and $ff]] xor
            t[$100+s[x shr 8 and $ff]] xor t[s[x and $ff]];
    x := k[3];
    k[3] := t[$300+s[x shr 24]] xor t[$200+s[x shr 16 and $ff]] xor
            t[$100+s[x shr 8 and $ff]] xor t[s[x and $ff]];
  until rounds=1;
end;

function TAES.DecryptInitFrom(const Encryption{$ifndef DELPHI5OROLDER}: TAES{$endif};
  const Key; KeySize: cardinal): boolean;
var ctx: TAESContext absolute Context;
begin
  {$ifdef USEPADLOCK}
  if DoPadlockInit(Key,KeySize) then begin
    result := true;
    ctx.Initialized := true;
    ctx.DoBlock := @aesdecryptpadlock;
    exit; // Init OK
  end;
  {$endif}
  ctx.Initialized := false;
  if not {$ifdef DELPHI5OROLDER}TAES{$endif}(Encryption).Initialized then
    // e.g. called from DecryptInit()
    EncryptInit(Key, KeySize) else // contains Initialized := true
    self := {$ifdef DELPHI5OROLDER}TAES{$endif}(Encryption);
  result := ctx.Initialized;
  if not result then
    exit;
  {$ifdef CPUX86_NOTPIC}
  ctx.DoBlock := @aesdecrypt386;
  {$else}
  ctx.DoBlock := @aesdecryptpas;
  {$endif}
  {$ifdef USEAESNI}
  if cfAESNI in CpuFeatures then begin
    MakeDecrKeyAesNi(ctx.Rounds,@ctx.RK);
    case KeySize of
    128: ctx.DoBlock := @aesnidecrypt128;
    192: ctx.DoBlock := @aesnidecrypt192;
    256: ctx.DoBlock := @aesnidecrypt256;
    end;
  end else
  {$endif}
    MakeDecrKey(ctx.Rounds,@ctx.RK);
end;

function TAES.DecryptInit(const Key; KeySize: cardinal): boolean;
begin
  result := DecryptInitFrom(self, Key, KeySize);
end;

procedure TAES.Decrypt(var B: TAESBlock);
begin
  TAESContext(Context).DoBlock(Context,B,B);
end;

procedure TAES.Decrypt(const BI: TAESBlock; var BO: TAESBlock);
begin
  TAESContext(Context).DoBlock(Context,BI,BO);
end;

procedure TAES.DoBlocks(pIn, pOut: PAESBlock; out oIn, oOut: PAESBLock;
  Count: integer; doEncrypt: boolean);
var i: integer;
    ctx: TAESContext absolute Context;
begin
{$ifdef USEPADLOCK}
//  assert(PtrUInt(pIn) and $F=0); // must be 16 bytes aligned
  if ctx.ViaCtx<>nil then begin
    if Count<>0 then begin
      Count := Count shl AESBlockShift;
      if doEncrypt then
        padlock_aes_encrypt(ctx.ViaCtx,pIn,pOut,Count) else
        padlock_aes_decrypt(ctx.ViaCtx,pIn,pOut,Count);
    end;
    oIn := pointer(PtrUInt(pIn)+PtrUInt(Count));
    oOut := pointer(PtrUInt(pOut)+PtrUInt(Count));
    exit;
  end;
{$endif}
  for i := 1 to Count do begin
    ctx.DoBlock(ctx,pIn^,pOut^);
    inc(pIn);
    inc(pOut);
  end;
  oIn := pIn;
  oOut := pOut;
end;

function TAES.DoInit(const Key; KeySize: cardinal; doEncrypt: boolean): boolean;
begin
  if doEncrypt then
    result := EncryptInit(Key, KeySize) else
    result := DecryptInit(Key,KeySize);
end;

procedure TAES.DoBlocks(pIn, pOut: PAESBlock; Count: integer; doEncrypt: boolean);
begin
  DoBlocks(pIn,pOut,pIn,pOut,Count,doEncrypt);
end;

procedure TAES.DoBlocksOFB(const iv: TAESBlock; src, dst: pointer; blockcount: PtrUInt);
var cv: TAESBlock;
begin
  cv := iv;
  while blockcount > 0 do begin
    dec(blockcount);
    TAESContext(Context).DoBlock(Context,cv,cv);
    XorBlock16(src,dst,pointer(@cv));
    inc(PByte(src),SizeOf(TAESBlock));
    inc(PByte(dst),SizeOf(TAESBlock));
  end;
end;

function TAES.Initialized: boolean;
begin
  result := TAESContext(Context).Initialized;
end;

function TAES.UsesAESNI: boolean;
begin
  {$ifdef CPUINTEL}
  result := cfAESNI in CpuFeatures;
  {$else}
  result := false;
  {$endif}
end;

function TAES.KeyBits: integer;
begin
  result := TAESContext(Context).KeyBits;
end;

procedure TAES.Done;
var ctx: TAESContext absolute Context;
begin
  {$ifdef USEPADLOCK}
  if Initialized and padlock_available and (ctx.ViaCtx<>nil) then
    padlock_aes_close(ctx.ViaCtx);
  {$endif USEPADLOCK}
  FillcharFast(ctx,sizeof(ctx),0); // always erase key in memory after use
end;

{$ifdef USETHREADSFORBIGAESBLOCKS}
type
  TThreadParams = record
    bIn, bOut: pAESBlock;
    BlockCount,BlockIndex: integer;
    Encrypt: boolean;
    ID: DWORD;
    AES: TAES;
  end;

{ we use direct Windows threads, since we don't need any exception handling
  nor memory usage inside the Thread handler
   -> avoid classes.TThread and system.BeginThread() use
   -> application is still "officialy" mono-threaded (i.e. IsMultiThread=false),
     for faster System.pas and FastMM4 (no locking)
   -> code is even shorter then original one using TThread }
function ThreadWrapper(var P: TThreadParams): Integer; stdcall;
begin
  with P do
    AES.DoBlocks(bIn,bOut,bIn,bOut,BlockCount,Encrypt);
  ExitThread(0);
  result := 0; // make the compiler happy, but won't never be called
end;

procedure TAES.DoBlocksThread(var bIn, bOut: PAESBlock; Count: integer; doEncrypt: boolean);
var Thread: array[0..3] of TThreadParams; // faster than dynamic array
    Handle: array[0..3] of THandle; // high(Thread) is not compiled by XE2
    nThread, i, nOne: integer;
    pIn, pOut: PAESBlock;
begin
  if Count=0 then exit;
  if {$ifdef USEPADLOCK} padlock_available or {$endif}
     {$ifdef USEAESNI} (cfAESNI in CpuFeatures) or {$endif}
    (SystemInfo.dwNumberOfProcessors<=1) or // (DebugHook<>0) or
    (Count<((512*1024) shr AESBlockShift)) then begin // not needed below 512 KB
    DoBlocks(bIn,bOut,bIn,bOut,Count,doEncrypt);
    exit;
  end;
  nThread := SystemInfo.dwNumberOfProcessors;
  if nThread>length(Thread) then // a quad-core is enough ;)
    nThread := length(Thread);
  nOne := Count div nThread;
  pIn := bIn;
  pOut := bOut;
  for i := 0 to nThread-1 do
  with Thread[i] do begin // create threads parameters
    bIn := pIn;
    bOut := pOut;
    BlockCount := nOne;
    BlockIndex := i+1;
    Encrypt := doEncrypt;
    AES := self; // local copy of the AES context for every thread
    Handle[i] := CreateThread(nil,0,@ThreadWrapper,@Thread[i],0,ID);
    inc(pIn,nOne);
    inc(pOut,nOne);
    dec(Count,nOne);
  end;
  if Count>0 then
    DoBlocks(pIn,pOut,pIn,pOut,Count,doEncrypt); // remaining blocks
  {$ifopt C+}
  inc(Count,nOne*nThread);
  assert(PtrUInt(pIn)-PtrUInt(bIn)=cardinal(Count)shl AESBlockShift);
  assert(PtrUInt(pOut)-PtrUInt(bOut)=cardinal(Count)shl AESBlockShift);
  {$endif}
  bIn := pIn;
  bOut := pOut;
  WaitForMultipleObjects(nThread,@Handle[0],True,INFINITE);
  for i := 0 to nThread-1 do
    CloseHandle(Handle[i]);
end;
{$endif USETHREADSFORBIGAESBLOCKS}


{ AES-GCM Support }

const
  // lookup table as used by mul_x/gf_mul/gf_mul_h
  gft_le: array[byte] of word = (
     $0000, $c201, $8403, $4602, $0807, $ca06, $8c04, $4e05,
     $100e, $d20f, $940d, $560c, $1809, $da08, $9c0a, $5e0b,
     $201c, $e21d, $a41f, $661e, $281b, $ea1a, $ac18, $6e19,
     $3012, $f213, $b411, $7610, $3815, $fa14, $bc16, $7e17,
     $4038, $8239, $c43b, $063a, $483f, $8a3e, $cc3c, $0e3d,
     $5036, $9237, $d435, $1634, $5831, $9a30, $dc32, $1e33,
     $6024, $a225, $e427, $2626, $6823, $aa22, $ec20, $2e21,
     $702a, $b22b, $f429, $3628, $782d, $ba2c, $fc2e, $3e2f,
     $8070, $4271, $0473, $c672, $8877, $4a76, $0c74, $ce75,
     $907e, $527f, $147d, $d67c, $9879, $5a78, $1c7a, $de7b,
     $a06c, $626d, $246f, $e66e, $a86b, $6a6a, $2c68, $ee69,
     $b062, $7263, $3461, $f660, $b865, $7a64, $3c66, $fe67,
     $c048, $0249, $444b, $864a, $c84f, $0a4e, $4c4c, $8e4d,
     $d046, $1247, $5445, $9644, $d841, $1a40, $5c42, $9e43,
     $e054, $2255, $6457, $a656, $e853, $2a52, $6c50, $ae51,
     $f05a, $325b, $7459, $b658, $f85d, $3a5c, $7c5e, $be5f,
     $00e1, $c2e0, $84e2, $46e3, $08e6, $cae7, $8ce5, $4ee4,
     $10ef, $d2ee, $94ec, $56ed, $18e8, $dae9, $9ceb, $5eea,
     $20fd, $e2fc, $a4fe, $66ff, $28fa, $eafb, $acf9, $6ef8,
     $30f3, $f2f2, $b4f0, $76f1, $38f4, $faf5, $bcf7, $7ef6,
     $40d9, $82d8, $c4da, $06db, $48de, $8adf, $ccdd, $0edc,
     $50d7, $92d6, $d4d4, $16d5, $58d0, $9ad1, $dcd3, $1ed2,
     $60c5, $a2c4, $e4c6, $26c7, $68c2, $aac3, $ecc1, $2ec0,
     $70cb, $b2ca, $f4c8, $36c9, $78cc, $bacd, $fccf, $3ece,
     $8091, $4290, $0492, $c693, $8896, $4a97, $0c95, $ce94,
     $909f, $529e, $149c, $d69d, $9898, $5a99, $1c9b, $de9a,
     $a08d, $628c, $248e, $e68f, $a88a, $6a8b, $2c89, $ee88,
     $b083, $7282, $3480, $f681, $b884, $7a85, $3c87, $fe86,
     $c0a9, $02a8, $44aa, $86ab, $c8ae, $0aaf, $4cad, $8eac,
     $d0a7, $12a6, $54a4, $96a5, $d8a0, $1aa1, $5ca3, $9ea2,
     $e0b5, $22b4, $64b6, $a6b7, $e8b2, $2ab3, $6cb1, $aeb0,
     $f0bb, $32ba, $74b8, $b6b9, $f8bc, $3abd, $7cbf, $bebe);

procedure mul_x(var a: TAESBlock; const b: TAESBlock);
// {$ifdef HASINLINE}inline;{$endif} // inlining has no benefit here
var t: cardinal;
    y: TWA4 absolute b;
const
  MASK_80 = cardinal($80808080);
  MASK_7F = cardinal($7f7f7f7f);
begin
  t := gft_le[(y[3] shr 17) and MASK_80];
  TWA4(a)[3] :=  ((y[3] shr 1) and MASK_7F) or (((y[3] shl 15) or (y[2] shr 17)) and MASK_80);
  TWA4(a)[2] :=  ((y[2] shr 1) and MASK_7F) or (((y[2] shl 15) or (y[1] shr 17)) and MASK_80);
  TWA4(a)[1] :=  ((y[1] shr 1) and MASK_7F) or (((y[1] shl 15) or (y[0] shr 17)) and MASK_80);
  TWA4(a)[0] := (((y[0] shr 1) and MASK_7F) or ( (y[0] shl 15) and MASK_80)) xor t;
end;

procedure gf_mul(var a: TAESBlock; const b: TAESBlock);
var p: array[0..7] of TAESBlock;
    x: TWA4;
    t: cardinal;
    i: PtrInt;
    j: integer;
    c: byte;
begin
  p[0] := b;
  for i := 1 to 7 do
    mul_x(p[i], p[i-1]);
  FillZero(TAESBlock(x));
  for i:=0 to 15 do begin
    c := a[15-i];
    if i>0 then begin
      // inlined mul_x8()
      t := gft_le[x[3] shr 24];
      x[3] := ((x[3] shl 8) or  (x[2] shr 24));
      x[2] := ((x[2] shl 8) or  (x[1] shr 24));
      x[1] := ((x[1] shl 8) or  (x[0] shr 24));
      x[0] := ((x[0] shl 8) xor t);
    end;
    for j:=0 to 7 do begin
      if c and ($80 shr j) <> 0 then begin
        x[3] := x[3] xor TWA4(p[j])[3];
        x[2] := x[2] xor TWA4(p[j])[2];
        x[1] := x[1] xor TWA4(p[j])[1];
        x[0] := x[0] xor TWA4(p[j])[0];
      end;
    end;
  end;
  a := TAESBlock(x);
end;


{ TAESGCMEngine }

procedure TAESGCMEngine.Make4K_Table;
var j, k: PtrInt;
begin
  gf_t4k[128] := ghash_h;
  j := 64;
  while j>0 do begin
    mul_x(gf_t4k[j],gf_t4k[j+j]);
    j := j shr 1;
  end;
  j := 2;
  while j<256 do begin
    for k := 1 to j-1 do
      XorBlock16(@gf_t4k[k],@gf_t4k[j+k],@gf_t4k[j]);
    inc(j,j);
  end;
end;

procedure TAESGCMEngine.gf_mul_h(var a: TAESBlock);
var
  x: TWA4;
  i: PtrUInt;
  t: cardinal;
  p: PWA4;
  {$ifdef CPUX86NOTPIC}
  tab: TWordArray absolute gft_le;
  {$else}
  tab: PWordArray;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  tab := @gft_le;
  {$endif CPUX86NOTPIC}
  x := TWA4(gf_t4k[a[15]]);
  for i := 14 downto 0 do begin
    p := @gf_t4k[a[i]];
    t := tab[x[3] shr 24];
    // efficient mul_x8 and xor using pre-computed table entries
    x[3] := ((x[3] shl 8) or  (x[2] shr 24)) xor p^[3];
    x[2] := ((x[2] shl 8) or  (x[1] shr 24)) xor p^[2];
    x[1] := ((x[1] shl 8) or  (x[0] shr 24)) xor p^[1];
    x[0] := ((x[0] shl 8) xor t) xor p^[0];
  end;
  a := TAESBlock(x);
end;

procedure GCM_IncCtr(var x: TAESBlock); {$ifdef HASINLINE} inline; {$endif}
begin
  // in AES-GCM, CTR covers only 32 LSB Big-Endian bits, i.e. x[15]..x[12]
  inc(x[15]);
  if x[15]<>0 then
    exit;
  inc(x[14]);
  if x[14]<>0 then
    exit;
  inc(x[13]);
  if x[13]=0 then
    inc(x[12]);
end;

procedure TAESGCMEngine.internal_crypt(ptp, ctp: PByte; ILen: PtrUInt);
var b_pos: PtrUInt;
begin
  b_pos := blen;
  inc(blen,ILen);
  blen := blen and AESBlockMod;
  if b_pos=0 then
    b_pos := SizeOf(TAESBlock) else
    while (ILen>0) and (b_pos<SizeOf(TAESBlock)) do begin
      ctp^ := ptp^ xor TAESContext(actx).buf[b_pos];
      inc(b_pos);
      inc(ptp);
      inc(ctp);
      dec(ILen);
    end;
  while ILen>=SizeOf(TAESBlock) do begin
    GCM_IncCtr(TAESContext(actx).IV);
    actx.Encrypt(TAESContext(actx).IV,TAESContext(actx).buf); // maybe AES-NI
    XorBlock16(pointer(ptp),pointer(ctp),@TAESContext(actx).buf);
    inc(PAESBlock(ptp));
    inc(PAESBlock(ctp));
    dec(ILen,SizeOf(TAESBlock));
  end;
  while ILen>0 do begin
    if b_pos=SizeOf(TAESBlock) then begin
      GCM_IncCtr(TAESContext(actx).IV);
      actx.Encrypt(TAESContext(actx).IV,TAESContext(actx).buf);
      b_pos := 0;
    end;
    ctp^ := TAESContext(actx).buf[b_pos] xor ptp^;
    inc(b_pos);
    inc(ptp);
    inc(ctp);
    dec(ILen);
  end;
end;

procedure TAESGCMEngine.internal_auth(ctp: PByte; ILen: PtrUInt;
  var ghv: TAESBlock; var gcnt: TQWordRec);
var b_pos: PtrUInt;
begin
  b_pos := gcnt.L and AESBlockMod;
  inc(gcnt.V,ILen);
  if (b_pos=0) and (gcnt.V<>0) then
    gf_mul_h(ghv);
  while (ILen>0) and (b_pos<SizeOf(TAESBlock)) do begin
    ghv[b_pos] := ghv[b_pos] xor ctp^;
    inc(b_pos);
    inc(ctp);
    dec(ILen);
  end;
  while ILen>=SizeOf(TAESBlock) do begin
    gf_mul_h(ghv);
    XorBlock16(@ghv,pointer(ctp));
    inc(PAESBlock(ctp));
    dec(ILen,SizeOf(TAESBlock));
  end;
  while ILen>0 do begin
    if b_pos=SizeOf(TAESBlock) then begin
      gf_mul_h(ghv);
      b_pos := 0;
    end;
    ghv[b_pos] := ghv[b_pos] xor ctp^;
    inc(b_pos);
    inc(ctp);
    dec(ILen);
  end;
end;

function TAESGCMEngine.Init(const Key; KeyBits: PtrInt): boolean;
begin
  FillcharFast(self,SizeOf(self),0);
  result := actx.EncryptInit(Key,KeyBits);
  if not result then
    exit;
  actx.Encrypt(ghash_h, ghash_h);
  Make4K_Table;
end;

const
  CTR_POS  = 12;

function TAESGCMEngine.Reset(pIV: pointer; IV_len: PtrInt): boolean;
var i, n_pos: PtrInt;
begin
  if (pIV=nil) or (IV_len=0) then begin
    result := false;
    exit;
  end;
  if IV_len=CTR_POS then begin
    // Initialization Vector size matches perfect size of 12 bytes
    MoveFast(pIV^,TAESContext(actx).IV,CTR_POS);
    TWA4(TAESContext(actx).IV)[3] := $01000000;
  end else begin
    // Initialization Vector is otherwise computed from GHASH(IV,H)
    n_pos := IV_len;
    FillZero(TAESContext(actx).IV);
    while n_pos>=SizeOf(TAESBlock) do begin
      XorBlock16(@TAESContext(actx).IV,pIV);
      inc(PAesBlock(pIV));
      dec(n_pos,SizeOf(TAESBlock));
      gf_mul_h(TAESContext(actx).IV);
    end;
    if n_pos>0 then begin
      for i := 0 to n_pos-1 do
        TAESContext(actx).IV[i] := TAESContext(actx).IV[i] xor PAESBlock(pIV)^[i];
      gf_mul_h(TAESContext(actx).IV);
    end;
    n_pos := IV_len shl 3;
    i := 15;
    while n_pos>0 do begin
      TAESContext(actx).IV[i] := TAESContext(actx).IV[i] xor byte(n_pos);
      n_pos := n_pos shr 8;
      dec(i);
    end;
    gf_mul_h(TAESContext(actx).IV);
  end;
  // reset internal state and counters
  y0_val := TWA4(TAESContext(actx).IV)[3];
  FillZero(aad_ghv);
  FillZero(txt_ghv);
  aad_cnt.V := 0;
  atx_cnt.V := 0;
  flags := [];
  result := true;
end;

function TAESGCMEngine.Encrypt(ptp, ctp: Pointer; ILen: PtrInt): boolean;
begin
  if ILen>0 then begin
    if (ptp=nil) or (ctp=nil) or (flagFinalComputed in flags) then begin
      result := false;
      exit;
    end;
    if (ILen and AESBlockMod=0) and (blen=0) then begin
      inc(atx_cnt.V,ILen);
      ILen := ILen shr AESBlockShift;
      repeat // loop optimized e.g. for PKCS7 padding
        GCM_IncCtr(TAESContext(actx).IV);
        actx.Encrypt(TAESContext(actx).IV,TAESContext(actx).buf); // maybe AES-NI
        XorBlock16(ptp,ctp,@TAESContext(actx).buf);
        gf_mul_h(txt_ghv);
        XorBlock16(@txt_ghv,ctp);
        inc(PAESBlock(ptp));
        inc(PAESBlock(ctp));
        dec(ILen);
      until ILen=0;
    end else begin // generic process in dual steps
      internal_crypt(ptp,ctp,iLen);
      internal_auth(ctp,ILen,txt_ghv,atx_cnt);
    end;
  end;
  result := true;
end;

function TAESGCMEngine.Decrypt(ctp, ptp: Pointer; ILen: PtrInt;
  ptag: pointer; tlen: PtrInt): boolean;
var tag: TAESBlock;
begin
  result := false;
  if ILen>0 then begin
    if (ptp=nil) or (ctp=nil) or (flagFinalComputed in flags) then
      exit;
    if (ILen and AESBlockMod=0) and (blen=0) then begin
      inc(atx_cnt.V,ILen);
      ILen := ILen shr AESBlockShift;
      repeat // loop optimized e.g. for PKCS7 padding
        gf_mul_h(txt_ghv);
        XorBlock16(@txt_ghv,ctp);
        GCM_IncCtr(TAESContext(actx).IV);
        actx.Encrypt(TAESContext(actx).IV,TAESContext(actx).buf); // maybe AES-NI
        XorBlock16(ctp,ptp,@TAESContext(actx).buf);
        inc(PAESBlock(ptp));
        inc(PAESBlock(ctp));
        dec(ILen);
      until ILen=0;
      if (ptag<>nil) and (tlen>0) then begin
        Final(tag,{anddone=}false);
        if not IsEqual(tag,ptag^,tlen) then
          exit; // check authentication after single pass encryption + auth
      end;
    end else begin // generic process in dual steps
      internal_auth(ctp,ILen,txt_ghv,atx_cnt);
      if (ptag<>nil) and (tlen>0) then begin
        Final(tag,{anddone=}false);
        if not IsEqual(tag,ptag^,tlen) then
          exit; // check authentication before encryption
      end;
      internal_crypt(ctp,ptp,iLen);
    end;
  end;
  result := true;
end;

function TAESGCMEngine.Add_AAD(pAAD: pointer; aLen: PtrInt): boolean;
begin
  if aLen>0 then begin
    if (pAAD=nil) or (flagFinalComputed in flags) then begin
      result := false;
      exit;
    end;
    internal_auth(pAAD,aLen,aad_ghv,aad_cnt);
  end;
  result := true;
end;

function TAESGCMEngine.Final(out tag: TAESBlock; andDone: boolean): boolean;
var
  tbuf: TAESBlock;
  ln: cardinal;
begin
  if not (flagFinalComputed in flags) then begin
    include(flags,flagFinalComputed);
    // compute GHASH(H, AAD, ctp)
    gf_mul_h(aad_ghv);
    gf_mul_h(txt_ghv);
    // compute len(AAD) || len(ctp) with each len as 64-bit big-endian
    ln := (atx_cnt.V+AESBlockMod) shr AESBlockShift;
    if (aad_cnt.V>0) and (ln<>0) then begin
      tbuf := ghash_h;
      while ln<>0 do begin
        if odd(ln) then
          gf_mul(aad_ghv,tbuf);
        ln := ln shr 1;
        if ln<>0 then
          gf_mul(tbuf,tbuf);
      end;
    end;
    TWA4(tbuf)[0] := bswap32((aad_cnt.L shr 29) or (aad_cnt.H shl 3));
    TWA4(tbuf)[1] := bswap32((aad_cnt.L shl  3));
    TWA4(tbuf)[2] := bswap32((atx_cnt.L shr 29) or (atx_cnt.H shl 3));
    TWA4(tbuf)[3] := bswap32((atx_cnt.L shl  3));
    XorBlock16(@tbuf,@txt_ghv);
    XorBlock16(@aad_ghv,@tbuf);
    gf_mul_h(aad_ghv);
    // compute E(K,Y0)
    tbuf := TAESContext(actx).IV;
    TWA4(tbuf)[3] := y0_val;
    actx.Encrypt(tbuf);
    // GMAC = GHASH(H, AAD, ctp) xor E(K,Y0)
    XorBlock16(@aad_ghv,@tag,@tbuf);
    if andDone then
      Done;
    result := true;
  end else begin
    Done;
    result := false;
  end;
end;

procedure TAESGCMEngine.Done;
begin
  if flagFlushed in flags then
    exit;
  actx.Done;
  include(flags,flagFlushed);
end;

function TAESGCMEngine.FullEncryptAndAuthenticate(const Key; KeyBits: PtrInt;
  pIV: pointer; IV_len: PtrInt; pAAD: pointer; aLen: PtrInt; ptp, ctp: Pointer;
  pLen: PtrInt; out tag: TAESBlock): boolean;
begin
  result := Init(Key,KeyBits) and Reset(pIV,IV_len) and Add_AAD(pAAD,aLen) and
            Encrypt(ptp,ctp,pLen) and Final(tag);
  Done;
end;

function TAESGCMEngine.FullDecryptAndVerify(const Key; KeyBits: PtrInt;
  pIV: pointer; IV_len: PtrInt; pAAD: pointer; aLen: PtrInt; ctp, ptp: Pointer;
  pLen: PtrInt; ptag: pointer; tLen: PtrInt): boolean;
begin
  result := Init(Key,KeyBits) and Reset(pIV,IV_len) and Add_AAD(pAAD,aLen) and
            Decrypt(ctp,ptp,pLen,ptag,tlen);
  Done;
end;



{ TSHA256 }

// under Win32, with a Core i7 CPU: pure pascal: 152ms - x86: 112ms
// under Win64, with a Core i7 CPU: pure pascal: 202ms - SSE4: 78ms

procedure Sha256ExpandMessageBlocks(W, Buf: PIntegerArray);
// Calculate "expanded message blocks"
{$ifdef AES_PASCAL}
var i: integer;
begin
  // bswap256() instead of "for i := 0 to 15 do W[i]:= bswap32(Buf[i]);"
  bswap256(@Buf[0],@W[0]);
  bswap256(@Buf[8],@W[8]);
  for i := 16 to 63 do
    {$ifdef FPC} // uses faster built-in right rotate intrinsic
    W[i] := (RorDWord(W[i-2],17)xor RorDWord(W[i-2],19)xor(W[i-2]shr 10))+W[i-7]+
      (RorDWord(W[i-15],7)xor RorDWord(W[i-15],18)xor(W[i-15]shr 3))+W[i-16];
    {$else}
    W[i] := (((W[i-2]shr 17)or(W[i-2]shl 15))xor((W[i-2]shr 19)or(W[i-2]shl 13))
      xor (W[i-2]shr 10))+W[i-7]+(((W[i-15]shr 7)or(W[i-15]shl 25))
      xor ((W[i-15]shr 18)or(W[i-15]shl 14))xor(W[i-15]shr 3))+W[i-16];
    {$endif}
end;
{$else}
{$ifdef CPUX86} {$ifdef FPC} nostackframe; assembler; {$endif}
asm // W=eax Buf=edx
        push    esi
        push    edi
        push    ebx
        mov     esi, eax
        // part 1: W[i]:= RB(TW32Buf(Buf)[i])
        mov     eax, [edx]
        mov     ebx, [edx + 4]
        bswap   eax
        bswap   ebx
        mov     [esi], eax
        mov     [esi + 4], ebx
        mov     eax, [edx + 8]
        mov     ebx, [edx + 12]
        bswap   eax
        bswap   ebx
        mov     [esi + 8], eax
        mov     [esi + 12], ebx
        mov     eax, [edx + 16]
        mov     ebx, [edx + 20]
        bswap   eax
        bswap   ebx
        mov     [esi + 16], eax
        mov     [esi + 20], ebx
        mov     eax, [edx + 24]
        mov     ebx, [edx + 28]
        bswap   eax
        bswap   ebx
        mov     [esi + 24], eax
        mov     [esi + 28], ebx
        mov     eax, [edx + 32]
        mov     ebx, [edx + 36]
        bswap   eax
        bswap   ebx
        mov     [esi + 32], eax
        mov     [esi + 36], ebx
        mov     eax, [edx + 40]
        mov     ebx, [edx + 44]
        bswap   eax
        bswap   ebx
        mov     [esi + 40], eax
        mov     [esi + 44], ebx
        mov     eax, [edx + 48]
        mov     ebx, [edx + 52]
        bswap   eax
        bswap   ebx
        mov     [esi + 48], eax
        mov     [esi + 52], ebx
        mov     eax, [edx + 56]
        mov     ebx, [edx + 60]
        bswap   eax
        bswap   ebx
        mov     [esi + 56], eax
        mov     [esi + 60], ebx
        lea     esi, [esi + 64]
        // part2: w[i]:= lrot_1(w[i-3] xor w[i-8] xor w[i-14] xor w[i-16])
        mov     ecx, 48
@@2:    mov     eax, [esi - 2 * 4]    // w[i-2]
        mov     edi, [esi - 7 * 4]    // w[i-7]
        mov     edx, eax
        mov     ebx, eax              // sig1: rr17 xor rr19 xor srx,10
        ror     eax, 17
        ror     edx, 19
        shr     ebx, 10
        xor     eax, edx
        xor     eax, ebx
        add     edi, eax
        mov     eax, [esi - 15 * 4]   // w[i-15]
        mov     ebx, eax              // sig0: rr7 xor rr18 xor sr3
        mov     edx, eax
        ror     eax, 7
        ror     edx, 18
        shr     ebx, 3
        xor     eax, edx
        xor     eax, ebx
        add     eax, edi
        add     eax, [esi - 16 * 4]   // w[i-16]
        mov     [esi], eax
        add     esi, 4
        dec     ecx
        jnz     @@2
        pop     ebx
        pop     edi
        pop     esi
end;
{$endif CPUX86}
{$ifdef CPUX64}
{$ifdef FPC}nostackframe; assembler; asm{$else}
asm // W=rcx Buf=rdx
  .noframe
{$endif}
     {$ifndef win64}
        mov     rdx, rsi
        mov     rcx, rdi
     {$endif win64}
        mov     rax, rcx
        push    rsi
        push    rdi
        push    rbx
        mov     rsi, rax
        // part 1: W[i]:= RB(TW32Buf(Buf)[i])
        mov     eax, [rdx]
        mov     ebx, [rdx + 4]
        bswap   eax
        bswap   ebx
        mov     [rsi], eax
        mov     [rsi + 4], ebx
        mov     eax, [rdx + 8]
        mov     ebx, [rdx + 12]
        bswap   eax
        bswap   ebx
        mov     [rsi + 8], eax
        mov     [rsi + 12], ebx
        mov     eax, [rdx + 16]
        mov     ebx, [rdx + 20]
        bswap   eax
        bswap   ebx
        mov     [rsi + 16], eax
        mov     [rsi + 20], ebx
        mov     eax, [rdx + 24]
        mov     ebx, [rdx + 28]
        bswap   eax
        bswap   ebx
        mov     [rsi + 24], eax
        mov     [rsi + 28], ebx
        mov     eax, [rdx + 32]
        mov     ebx, [rdx + 36]
        bswap   eax
        bswap   ebx
        mov     [rsi + 32], eax
        mov     [rsi + 36], ebx
        mov     eax, [rdx + 40]
        mov     ebx, [rdx + 44]
        bswap   eax
        bswap   ebx
        mov     [rsi + 40], eax
        mov     [rsi + 44], ebx
        mov     eax, [rdx + 48]
        mov     ebx, [rdx + 52]
        bswap   eax
        bswap   ebx
        mov     [rsi + 48], eax
        mov     [rsi + 52], ebx
        mov     eax, [rdx + 56]
        mov     ebx, [rdx + 60]
        bswap   eax
        bswap   ebx
        mov     [rsi + 56], eax
        mov     [rsi + 60], ebx
        lea     rsi, [rsi + 64]
        // part2: W[i]:= LRot_1(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16])
        mov     ecx, 48
@@2:    mov     eax, [rsi - 2 * 4]    // W[i-2]
        mov     edi, [rsi - 7 * 4]    // W[i-7]
        mov     edx, eax
        mov     ebx, eax          // Sig1: RR17 xor RR19 xor SRx,10
        ror     eax, 17
        ror     edx, 19
        shr     ebx, 10
        xor     eax, edx
        xor     eax, ebx
        add     edi, eax
        mov     eax, [rsi - 15 * 4]   // W[i-15]
        mov     ebx, eax          // Sig0: RR7 xor RR18 xor SR3
        mov     edx, eax
        ror     eax, 7
        ror     edx, 18
        shr     ebx, 3
        xor     eax, edx
        xor     eax, ebx
        add     eax, edi
        add     eax, [rsi - 16 * 4]   // W[i-16]
        mov     [rsi], eax
        add     rsi, 4
        dec     ecx
        jnz     @@2
        pop     rbx
        pop     rdi
        pop     rsi
end;
{$endif CPUX64}
{$endif AES_PASCAL}

const
  K256: array[0..63] of cardinal = (
   $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1,
   $923f82a4, $ab1c5ed5, $d807aa98, $12835b01, $243185be, $550c7dc3,
   $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174, $e49b69c1, $efbe4786,
   $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
   $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147,
   $06ca6351, $14292967, $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13,
   $650a7354, $766a0abb, $81c2c92e, $92722c85, $a2bfe8a1, $a81a664b,
   $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
   $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a,
   $5b9cca4f, $682e6ff3, $748f82ee, $78a5636f, $84c87814, $8cc70208,
   $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);

{$ifdef CPUX64}
// optimized unrolled version from Intel's sha256_sse4.asm
//  Original code is released as Copyright (c) 2012, Intel Corporation
var
  K256AlignedStore: RawByteString;
  K256Aligned: pointer; // movaps + paddd do expect 16 bytes alignment
const
  STACK_SIZE = 32{$ifndef LINUX}+7*16{$endif};

procedure sha256_sse4(var input_data; var digest; num_blks: PtrUInt);
{$ifdef FPC}nostackframe; assembler; asm{$else}
asm // rcx=input_data rdx=digest r8=num_blks (Linux: rdi,rsi,rdx)
        .noframe
{$endif FPC}
        push    rbx
        {$ifdef LINUX}
        mov     r8, rdx
        mov     rcx, rdi
        mov     rdx, rsi
        {$else}
        push    rsi   // Win64 expects those registers to be preserved
        push    rdi
        {$endif}
        push    rbp
        push    r13
        push    r14
        push    r15
        sub     rsp, STACK_SIZE
        {$ifndef LINUX}
        movaps  [rsp + 20H], xmm6    // manual .PUSHNV for FPC compatibility
        movaps  [rsp + 30H], xmm7
        movaps  [rsp + 40H], xmm8
        movaps  [rsp + 50H], xmm9
        movaps  [rsp + 60H], xmm10
        movaps  [rsp + 70H], xmm11
        movaps  [rsp + 80H], xmm12
        {$endif}
        shl     r8, 6
        je      @done
        add     r8, rcx
        mov     [rsp], r8
        mov     eax, [rdx]
        mov     ebx, [rdx + 4H]
        mov     edi, [rdx + 8H]
        mov     esi, [rdx + 0CH]
        mov     r8d, [rdx + 10H]
        mov     r9d, [rdx + 14H]
        mov     r10d, [rdx + 18H]
        mov     r11d, [rdx + 1CH]
        movaps  xmm12, [rip + @flip]
        movaps  xmm10, [rip + @00BA]
        movaps  xmm11, [rip + @DC00]
@loop0: mov     rbp, [rip + K256Aligned]
        movups  xmm4, [rcx]
        pshufb  xmm4, xmm12
        movups  xmm5, [rcx + 10h]
        pshufb  xmm5, xmm12
        movups  xmm6, [rcx + 20h]
        pshufb  xmm6, xmm12
        movups  xmm7, [rcx + 30h]
        pshufb  xmm7, xmm12
        mov     [rsp + 8h], rcx
        mov     rcx, 3
@loop1: movaps  xmm9, [rbp]
        paddd   xmm9, xmm4
        movaps  [rsp + 10h], xmm9
        movaps  xmm0, xmm7
        mov     r13d, r8d
        ror     r13d, 14
        mov     r14d, eax
        palignr xmm0, xmm6, 04h
        ror     r14d, 9
        xor     r13d, r8d
        mov     r15d, r9d
        ror     r13d, 5
        movaps  xmm1, xmm5
        xor     r14d, eax
        xor     r15d, r10d
        paddd   xmm0, xmm4
        xor     r13d, r8d
        and     r15d, r8d
        ror     r14d, 11
        palignr xmm1, xmm4, 04h
        xor     r14d, eax
        ror     r13d, 6
        xor     r15d, r10d
        movaps  xmm2, xmm1
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 10h]
        movaps  xmm3, xmm1
        mov     r13d, eax
        add     r11d, r15d
        mov     r15d, eax
        pslld   xmm1, 25
        or      r13d, edi
        add     esi, r11d
        and     r15d, edi
        psrld   xmm2, 7
        and     r13d, ebx
        add     r11d, r14d
        por     xmm1, xmm2
        or      r13d, r15d
        add     r11d, r13d
        movaps  xmm2, xmm3
        mov     r13d, esi
        mov     r14d, r11d
        movaps  xmm8, xmm3
        ror     r13d, 14
        xor     r13d, esi
        mov     r15d, r8d
        ror     r14d, 9
        pslld   xmm3, 14
        xor     r14d, r11d
        ror     r13d, 5
        xor     r15d, r9d
        psrld   xmm2, 18
        ror     r14d, 11
        xor     r13d, esi
        and     r15d, esi
        ror     r13d, 6
        pxor    xmm1, xmm3
        xor     r14d, r11d
        xor     r15d, r9d
        psrld   xmm8, 3
        add     r15d, r13d
        add     r15d, [rsp + 14h]
        ror     r14d, 2
        pxor    xmm1, xmm2
        mov     r13d, r11d
        add     r10d, r15d
        mov     r15d, r11d
        pxor    xmm1, xmm8
        or      r13d, ebx
        add     edi, r10d
        and     r15d, ebx
        pshufd  xmm2, xmm7, 0fah
        and     r13d, eax
        add     r10d, r14d
        paddd   xmm0, xmm1
        or      r13d, r15d
        add     r10d, r13d
        movaps  xmm3, xmm2
        mov     r13d, edi
        mov     r14d, r10d
        ror     r13d, 14
        movaps  xmm8, xmm2
        xor     r13d, edi
        ror     r14d, 9
        mov     r15d, esi
        xor     r14d, r10d
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r15d, r8d
        psrlq   xmm3, 19
        xor     r13d, edi
        and     r15d, edi
        psrld   xmm8, 10
        ror     r14d, 11
        xor     r14d, r10d
        xor     r15d, r8d
        ror     r13d, 6
        pxor    xmm2, xmm3
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 18h]
        pxor    xmm8, xmm2
        mov     r13d, r10d
        add     r9d, r15d
        mov     r15d, r10d
        pshufb  xmm8, xmm10
        or      r13d, eax
        add     ebx, r9d
        and     r15d, eax
        paddd   xmm0, xmm8
        and     r13d, r11d
        add     r9d, r14d
        pshufd  xmm2, xmm0, 50h
        or      r13d, r15d
        add     r9d, r13d
        movaps  xmm3, xmm2
        mov     r13d, ebx
        ror     r13d, 14
        mov     r14d, r9d
        movaps  xmm4, xmm2
        ror     r14d, 9
        xor     r13d, ebx
        mov     r15d, edi
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r14d, r9d
        xor     r15d, esi
        psrlq   xmm3, 19
        xor     r13d, ebx
        and     r15d, ebx
        ror     r14d, 11
        psrld   xmm4, 10
        xor     r14d, r9d
        ror     r13d, 6
        xor     r15d, esi
        pxor    xmm2, xmm3
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 1ch]
        pxor    xmm4, xmm2
        mov     r13d, r9d
        add     r8d, r15d
        mov     r15d, r9d
        pshufb  xmm4, xmm11
        or      r13d, r11d
        add     eax, r8d
        and     r15d, r11d
        paddd   xmm4, xmm0
        and     r13d, r10d
        add     r8d, r14d
        or      r13d, r15d
        add     r8d, r13d
        movaps  xmm9, [rbp + 10h]
        paddd   xmm9, xmm5
        movaps  [rsp + 10h], xmm9
        movaps  xmm0, xmm4
        mov     r13d, eax
        ror     r13d, 14
        mov     r14d, r8d
        palignr xmm0, xmm7, 04h
        ror     r14d, 9
        xor     r13d, eax
        mov     r15d, ebx
        ror     r13d, 5
        movaps  xmm1, xmm6
        xor     r14d, r8d
        xor     r15d, edi
        paddd   xmm0, xmm5
        xor     r13d, eax
        and     r15d, eax
        ror     r14d, 11
        palignr xmm1, xmm5, 04h
        xor     r14d, r8d
        ror     r13d, 6
        xor     r15d, edi
        movaps  xmm2, xmm1
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 10h]
        movaps  xmm3, xmm1
        mov     r13d, r8d
        add     esi, r15d
        mov     r15d, r8d
        pslld   xmm1, 25
        or      r13d, r10d
        add     r11d, esi
        and     r15d, r10d
        psrld   xmm2, 7
        and     r13d, r9d
        add     esi, r14d
        por     xmm1, xmm2
        or      r13d, r15d
        add     esi, r13d
        movaps  xmm2, xmm3
        mov     r13d, r11d
        mov     r14d, esi
        movaps  xmm8, xmm3
        ror     r13d, 14
        xor     r13d, r11d
        mov     r15d, eax
        ror     r14d, 9
        pslld   xmm3, 14
        xor     r14d, esi
        ror     r13d, 5
        xor     r15d, ebx
        psrld   xmm2, 18
        ror     r14d, 11
        xor     r13d, r11d
        and     r15d, r11d
        ror     r13d, 6
        pxor    xmm1, xmm3
        xor     r14d, esi
        xor     r15d, ebx
        psrld   xmm8, 3
        add     r15d, r13d
        add     r15d, [rsp + 14h]
        ror     r14d, 2
        pxor    xmm1, xmm2
        mov     r13d, esi
        add     edi, r15d
        mov     r15d, esi
        pxor    xmm1, xmm8
        or      r13d, r9d
        add     r10d, edi
        and     r15d, r9d
        pshufd  xmm2, xmm4, 0fah
        and     r13d, r8d
        add     edi, r14d
        paddd   xmm0, xmm1
        or      r13d, r15d
        add     edi, r13d
        movaps  xmm3, xmm2
        mov     r13d, r10d
        mov     r14d, edi
        ror     r13d, 14
        movaps  xmm8, xmm2
        xor     r13d, r10d
        ror     r14d, 9
        mov     r15d, r11d
        xor     r14d, edi
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r15d, eax
        psrlq   xmm3, 19
        xor     r13d, r10d
        and     r15d, r10d
        psrld   xmm8, 10
        ror     r14d, 11
        xor     r14d, edi
        xor     r15d, eax
        ror     r13d, 6
        pxor    xmm2, xmm3
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 18h]
        pxor    xmm8, xmm2
        mov     r13d, edi
        add     ebx, r15d
        mov     r15d, edi
        pshufb  xmm8, xmm10
        or      r13d, r8d
        add     r9d, ebx
        and     r15d, r8d
        paddd   xmm0, xmm8
        and     r13d, esi
        add     ebx, r14d
        pshufd  xmm2, xmm0, 50h
        or      r13d, r15d
        add     ebx, r13d
        movaps  xmm3, xmm2
        mov     r13d, r9d
        ror     r13d, 14
        mov     r14d, ebx
        movaps  xmm5, xmm2
        ror     r14d, 9
        xor     r13d, r9d
        mov     r15d, r10d
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r14d, ebx
        xor     r15d, r11d
        psrlq   xmm3, 19
        xor     r13d, r9d
        and     r15d, r9d
        ror     r14d, 11
        psrld   xmm5, 10
        xor     r14d, ebx
        ror     r13d, 6
        xor     r15d, r11d
        pxor    xmm2, xmm3
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 1ch]
        pxor    xmm5, xmm2
        mov     r13d, ebx
        add     eax, r15d
        mov     r15d, ebx
        pshufb  xmm5, xmm11
        or      r13d, esi
        add     r8d, eax
        and     r15d, esi
        paddd   xmm5, xmm0
        and     r13d, edi
        add     eax, r14d
        or      r13d, r15d
        add     eax, r13d
        movaps  xmm9, [rbp + 20h]
        paddd   xmm9, xmm6
        movaps  [rsp + 10h], xmm9
        movaps  xmm0, xmm5
        mov     r13d, r8d
        ror     r13d, 14
        mov     r14d, eax
        palignr xmm0, xmm4, 04h
        ror     r14d, 9
        xor     r13d, r8d
        mov     r15d, r9d
        ror     r13d, 5
        movaps  xmm1, xmm7
        xor     r14d, eax
        xor     r15d, r10d
        paddd   xmm0, xmm6
        xor     r13d, r8d
        and     r15d, r8d
        ror     r14d, 11
        palignr xmm1, xmm6, 04h
        xor     r14d, eax
        ror     r13d, 6
        xor     r15d, r10d
        movaps  xmm2, xmm1
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 10h]
        movaps  xmm3, xmm1
        mov     r13d, eax
        add     r11d, r15d
        mov     r15d, eax
        pslld   xmm1, 25
        or      r13d, edi
        add     esi, r11d
        and     r15d, edi
        psrld   xmm2, 7
        and     r13d, ebx
        add     r11d, r14d
        por     xmm1, xmm2
        or      r13d, r15d
        add     r11d, r13d
        movaps  xmm2, xmm3
        mov     r13d, esi
        mov     r14d, r11d
        movaps  xmm8, xmm3
        ror     r13d, 14
        xor     r13d, esi
        mov     r15d, r8d
        ror     r14d, 9
        pslld   xmm3, 14
        xor     r14d, r11d
        ror     r13d, 5
        xor     r15d, r9d
        psrld   xmm2, 18
        ror     r14d, 11
        xor     r13d, esi
        and     r15d, esi
        ror     r13d, 6
        pxor    xmm1, xmm3
        xor     r14d, r11d
        xor     r15d, r9d
        psrld   xmm8, 3
        add     r15d, r13d
        add     r15d, [rsp + 14h]
        ror     r14d, 2
        pxor    xmm1, xmm2
        mov     r13d, r11d
        add     r10d, r15d
        mov     r15d, r11d
        pxor    xmm1, xmm8
        or      r13d, ebx
        add     edi, r10d
        and     r15d, ebx
        pshufd  xmm2, xmm5, 0fah
        and     r13d, eax
        add     r10d, r14d
        paddd   xmm0, xmm1
        or      r13d, r15d
        add     r10d, r13d
        movaps  xmm3, xmm2
        mov     r13d, edi
        mov     r14d, r10d
        ror     r13d, 14
        movaps  xmm8, xmm2
        xor     r13d, edi
        ror     r14d, 9
        mov     r15d, esi
        xor     r14d, r10d
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r15d, r8d
        psrlq   xmm3, 19
        xor     r13d, edi
        and     r15d, edi
        psrld   xmm8, 10
        ror     r14d, 11
        xor     r14d, r10d
        xor     r15d, r8d
        ror     r13d, 6
        pxor    xmm2, xmm3
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 18h]
        pxor    xmm8, xmm2
        mov     r13d, r10d
        add     r9d, r15d
        mov     r15d, r10d
        pshufb  xmm8, xmm10
        or      r13d, eax
        add     ebx, r9d
        and     r15d, eax
        paddd   xmm0, xmm8
        and     r13d, r11d
        add     r9d, r14d
        pshufd  xmm2, xmm0, 50h
        or      r13d, r15d
        add     r9d, r13d
        movaps  xmm3, xmm2
        mov     r13d, ebx
        ror     r13d, 14
        mov     r14d, r9d
        movaps  xmm6, xmm2
        ror     r14d, 9
        xor     r13d, ebx
        mov     r15d, edi
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r14d, r9d
        xor     r15d, esi
        psrlq   xmm3, 19
        xor     r13d, ebx
        and     r15d, ebx
        ror     r14d, 11
        psrld   xmm6, 10
        xor     r14d, r9d
        ror     r13d, 6
        xor     r15d, esi
        pxor    xmm2, xmm3
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 1ch]
        pxor    xmm6, xmm2
        mov     r13d, r9d
        add     r8d, r15d
        mov     r15d, r9d
        pshufb  xmm6, xmm11
        or      r13d, r11d
        add     eax, r8d
        and     r15d, r11d
        paddd   xmm6, xmm0
        and     r13d, r10d
        add     r8d, r14d
        or      r13d, r15d
        add     r8d, r13d
        movaps  xmm9, [rbp + 30h]
        paddd   xmm9, xmm7
        movaps  [rsp + 10h], xmm9
        add     rbp, 64
        movaps  xmm0, xmm6
        mov     r13d, eax
        ror     r13d, 14
        mov     r14d, r8d
        palignr xmm0, xmm5, 04h
        ror     r14d, 9
        xor     r13d, eax
        mov     r15d, ebx
        ror     r13d, 5
        movaps  xmm1, xmm4
        xor     r14d, r8d
        xor     r15d, edi
        paddd   xmm0, xmm7
        xor     r13d, eax
        and     r15d, eax
        ror     r14d, 11
        palignr xmm1, xmm7, 04h
        xor     r14d, r8d
        ror     r13d, 6
        xor     r15d, edi
        movaps  xmm2, xmm1
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 10h]
        movaps  xmm3, xmm1
        mov     r13d, r8d
        add     esi, r15d
        mov     r15d, r8d
        pslld   xmm1, 25
        or      r13d, r10d
        add     r11d, esi
        and     r15d, r10d
        psrld   xmm2, 7
        and     r13d, r9d
        add     esi, r14d
        por     xmm1, xmm2
        or      r13d, r15d
        add     esi, r13d
        movaps  xmm2, xmm3
        mov     r13d, r11d
        mov     r14d, esi
        movaps  xmm8, xmm3
        ror     r13d, 14
        xor     r13d, r11d
        mov     r15d, eax
        ror     r14d, 9
        pslld   xmm3, 14
        xor     r14d, esi
        ror     r13d, 5
        xor     r15d, ebx
        psrld   xmm2, 18
        ror     r14d, 11
        xor     r13d, r11d
        and     r15d, r11d
        ror     r13d, 6
        pxor    xmm1, xmm3
        xor     r14d, esi
        xor     r15d, ebx
        psrld   xmm8, 3
        add     r15d, r13d
        add     r15d, [rsp + 14h]
        ror     r14d, 2
        pxor    xmm1, xmm2
        mov     r13d, esi
        add     edi, r15d
        mov     r15d, esi
        pxor    xmm1, xmm8
        or      r13d, r9d
        add     r10d, edi
        and     r15d, r9d
        pshufd  xmm2, xmm6, 0fah
        and     r13d, r8d
        add     edi, r14d
        paddd   xmm0, xmm1
        or      r13d, r15d
        add     edi, r13d
        movaps  xmm3, xmm2
        mov     r13d, r10d
        mov     r14d, edi
        ror     r13d, 14
        movaps  xmm8, xmm2
        xor     r13d, r10d
        ror     r14d, 9
        mov     r15d, r11d
        xor     r14d, edi
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r15d, eax
        psrlq   xmm3, 19
        xor     r13d, r10d
        and     r15d, r10d
        psrld   xmm8, 10
        ror     r14d, 11
        xor     r14d, edi
        xor     r15d, eax
        ror     r13d, 6
        pxor    xmm2, xmm3
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 18h]
        pxor    xmm8, xmm2
        mov     r13d, edi
        add     ebx, r15d
        mov     r15d, edi
        pshufb  xmm8, xmm10
        or      r13d, r8d
        add     r9d, ebx
        and     r15d, r8d
        paddd   xmm0, xmm8
        and     r13d, esi
        add     ebx, r14d
        pshufd  xmm2, xmm0, 50h
        or      r13d, r15d
        add     ebx, r13d
        movaps  xmm3, xmm2
        mov     r13d, r9d
        ror     r13d, 14
        mov     r14d, ebx
        movaps  xmm7, xmm2
        ror     r14d, 9
        xor     r13d, r9d
        mov     r15d, r10d
        ror     r13d, 5
        psrlq   xmm2, 17
        xor     r14d, ebx
        xor     r15d, r11d
        psrlq   xmm3, 19
        xor     r13d, r9d
        and     r15d, r9d
        ror     r14d, 11
        psrld   xmm7, 10
        xor     r14d, ebx
        ror     r13d, 6
        xor     r15d, r11d
        pxor    xmm2, xmm3
        ror     r14d, 2
        add     r15d, r13d
        add     r15d, [rsp + 1ch]
        pxor    xmm7, xmm2
        mov     r13d, ebx
        add     eax, r15d
        mov     r15d, ebx
        pshufb  xmm7, xmm11
        or      r13d, esi
        add     r8d, eax
        and     r15d, esi
        paddd   xmm7, xmm0
        and     r13d, edi
        add     eax, r14d
        or      r13d, r15d
        add     eax, r13d
        sub     rcx, 1
        jne     @loop1
        mov     rcx, 2
@loop2: paddd   xmm4, [rbp]
        movaps  [rsp + 10h], xmm4
        mov     r13d, r8d
        ror     r13d, 14
        mov     r14d, eax
        xor     r13d, r8d
        ror     r14d, 9
        mov     r15d, r9d
        xor     r14d, eax
        ror     r13d, 5
        xor     r15d, r10d
        xor     r13d, r8d
        ror     r14d, 11
        and     r15d, r8d
        xor     r14d, eax
        ror     r13d, 6
        xor     r15d, r10d
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 10h]
        mov     r13d, eax
        add     r11d, r15d
        mov     r15d, eax
        or      r13d, edi
        add     esi, r11d
        and     r15d, edi
        and     r13d, ebx
        add     r11d, r14d
        or      r13d, r15d
        add     r11d, r13d
        mov     r13d, esi
        ror     r13d, 14
        mov     r14d, r11d
        xor     r13d, esi
        ror     r14d, 9
        mov     r15d, r8d
        xor     r14d, r11d
        ror     r13d, 5
        xor     r15d, r9d
        xor     r13d, esi
        ror     r14d, 11
        and     r15d, esi
        xor     r14d, r11d
        ror     r13d, 6
        xor     r15d, r9d
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 14h]
        mov     r13d, r11d
        add     r10d, r15d
        mov     r15d, r11d
        or      r13d, ebx
        add     edi, r10d
        and     r15d, ebx
        and     r13d, eax
        add     r10d, r14d
        or      r13d, r15d
        add     r10d, r13d
        mov     r13d, edi
        ror     r13d, 14
        mov     r14d, r10d
        xor     r13d, edi
        ror     r14d, 9
        mov     r15d, esi
        xor     r14d, r10d
        ror     r13d, 5
        xor     r15d, r8d
        xor     r13d, edi
        ror     r14d, 11
        and     r15d, edi
        xor     r14d, r10d
        ror     r13d, 6
        xor     r15d, r8d
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 18h]
        mov     r13d, r10d
        add     r9d, r15d
        mov     r15d, r10d
        or      r13d, eax
        add     ebx, r9d
        and     r15d, eax
        and     r13d, r11d
        add     r9d, r14d
        or      r13d, r15d
        add     r9d, r13d
        mov     r13d, ebx
        ror     r13d, 14
        mov     r14d, r9d
        xor     r13d, ebx
        ror     r14d, 9
        mov     r15d, edi
        xor     r14d, r9d
        ror     r13d, 5
        xor     r15d, esi
        xor     r13d, ebx
        ror     r14d, 11
        and     r15d, ebx
        xor     r14d, r9d
        ror     r13d, 6
        xor     r15d, esi
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 1ch]
        mov     r13d, r9d
        add     r8d, r15d
        mov     r15d, r9d
        or      r13d, r11d
        add     eax, r8d
        and     r15d, r11d
        and     r13d, r10d
        add     r8d, r14d
        or      r13d, r15d
        add     r8d, r13d
        paddd   xmm5, [rbp + 10h]
        movaps  [rsp + 10h], xmm5
        add     rbp, 32
        mov     r13d, eax
        ror     r13d, 14
        mov     r14d, r8d
        xor     r13d, eax
        ror     r14d, 9
        mov     r15d, ebx
        xor     r14d, r8d
        ror     r13d, 5
        xor     r15d, edi
        xor     r13d, eax
        ror     r14d, 11
        and     r15d, eax
        xor     r14d, r8d
        ror     r13d, 6
        xor     r15d, edi
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 10h]
        mov     r13d, r8d
        add     esi, r15d
        mov     r15d, r8d
        or      r13d, r10d
        add     r11d, esi
        and     r15d, r10d
        and     r13d, r9d
        add     esi, r14d
        or      r13d, r15d
        add     esi, r13d
        mov     r13d, r11d
        ror     r13d, 14
        mov     r14d, esi
        xor     r13d, r11d
        ror     r14d, 9
        mov     r15d, eax
        xor     r14d, esi
        ror     r13d, 5
        xor     r15d, ebx
        xor     r13d, r11d
        ror     r14d, 11
        and     r15d, r11d
        xor     r14d, esi
        ror     r13d, 6
        xor     r15d, ebx
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 14h]
        mov     r13d, esi
        add     edi, r15d
        mov     r15d, esi
        or      r13d, r9d
        add     r10d, edi
        and     r15d, r9d
        and     r13d, r8d
        add     edi, r14d
        or      r13d, r15d
        add     edi, r13d
        mov     r13d, r10d
        ror     r13d, 14
        mov     r14d, edi
        xor     r13d, r10d
        ror     r14d, 9
        mov     r15d, r11d
        xor     r14d, edi
        ror     r13d, 5
        xor     r15d, eax
        xor     r13d, r10d
        ror     r14d, 11
        and     r15d, r10d
        xor     r14d, edi
        ror     r13d, 6
        xor     r15d, eax
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 18h]
        mov     r13d, edi
        add     ebx, r15d
        mov     r15d, edi
        or      r13d, r8d
        add     r9d, ebx
        and     r15d, r8d
        and     r13d, esi
        add     ebx, r14d
        or      r13d, r15d
        add     ebx, r13d
        mov     r13d, r9d
        ror     r13d, 14
        mov     r14d, ebx
        xor     r13d, r9d
        ror     r14d, 9
        mov     r15d, r10d
        xor     r14d, ebx
        ror     r13d, 5
        xor     r15d, r11d
        xor     r13d, r9d
        ror     r14d, 11
        and     r15d, r9d
        xor     r14d, ebx
        ror     r13d, 6
        xor     r15d, r11d
        add     r15d, r13d
        ror     r14d, 2
        add     r15d, [rsp + 1ch]
        mov     r13d, ebx
        add     eax, r15d
        mov     r15d, ebx
        or      r13d, esi
        add     r8d, eax
        and     r15d, esi
        and     r13d, edi
        add     eax, r14d
        or      r13d, r15d
        add     eax, r13d
        movaps  xmm4, xmm6
        movaps  xmm5, xmm7
        dec     rcx
        jne     @loop2
        add     eax, [rdx]
        mov     [rdx], eax
        add     ebx, [rdx + 4H]
        add     edi, [rdx + 8H]
        add     esi, [rdx + 0CH]
        add     r8d, [rdx + 10H]
        add     r9d, [rdx + 14H]
        add     r10d, [rdx + 18H]
        add     r11d, [rdx + 1CH]
        mov     [rdx + 4H], ebx
        mov     [rdx + 8H], edi
        mov     [rdx + 0CH], esi
        mov     [rdx + 10H], r8d
        mov     [rdx + 14H], r9d
        mov     [rdx + 18H], r10d
        mov     [rdx + 1CH], r11d
        mov     rcx, [rsp + 8H]
        add     rcx, 64
        cmp     rcx, [rsp]
        jne     @loop0
@done: {$ifndef LINUX}
        movaps  xmm6, [rsp + 20H]
        movaps  xmm7, [rsp + 30H]
        movaps  xmm8, [rsp + 40H]
        movaps  xmm9, [rsp + 50H]
        movaps  xmm10, [rsp + 60H]
        movaps  xmm11, [rsp + 70H]
        movaps  xmm12, [rsp + 80H]
        {$endif}
        add     rsp, STACK_SIZE
        pop     r15
        pop     r14
        pop     r13
        pop     rbp
        {$ifndef LINUX}
        pop     rdi
        pop     rsi
        {$endif}
        pop     rbx
        ret
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@flip:  dq      $0405060700010203
        dq      $0C0D0E0F08090A0B
@00BA:  dq      $0B0A090803020100
        dq      $FFFFFFFFFFFFFFFF
@DC00:  dq      $FFFFFFFFFFFFFFFF
        dq      $0B0A090803020100
end;
{$endif CPUX64}

procedure Sha256CompressPas(var Hash: TSHAHash; Data: pointer);
// Actual hashing function
var H: TSHAHash;
    W: array[0..63] of cardinal;
    {$ifdef PUREPASCAL}
    i: integer;
    t1, t2: cardinal;
    {$endif}
begin
  // calculate "expanded message blocks"
  Sha256ExpandMessageBlocks(@W,Data);
  // assign old working hash to local variables A..H
  H.A := Hash.A;
  H.B := Hash.B;
  H.C := Hash.C;
  H.D := Hash.D;
  H.E := Hash.E;
  H.F := Hash.F;
  H.G := Hash.G;
  H.H := Hash.H;
{$ifdef PUREPASCAL}
  // SHA-256 compression function
  for i := 0 to high(W) do begin
    {$ifdef FPC} // uses built-in right rotate intrinsic
    t1 := H.H+(RorDWord(H.E,6) xor RorDWord(H.E,11) xor RorDWord(H.E,25))+
      ((H.E and H.F)xor(not H.E and H.G))+K256[i]+W[i];
    t2 := (RorDWord(H.A,2) xor RorDWord(H.A,13) xor RorDWord(H.A,22))+
      ((H.A and H.B)xor(H.A and H.C)xor(H.B and H.C));
    {$else}
    t1 := H.H+(((H.E shr 6)or(H.E shl 26))xor((H.E shr 11)or(H.E shl 21))xor
      ((H.E shr 25)or(H.E shl 7)))+((H.E and H.F)xor(not H.E and H.G))+K256[i]+W[i];
    t2 := (((H.A shr 2)or(H.A shl 30))xor((H.A shr 13)or(H.A shl 19))xor
      ((H.A shr 22)xor(H.A shl 10)))+((H.A and H.B)xor(H.A and H.C)xor(H.B and H.C));
    {$endif}
    H.H := H.G; H.G := H.F; H.F := H.E; H.E := H.D+t1;
    H.D := H.C; H.C := H.B; H.B := H.A; H.A := t1+t2;
  end;
{$else}
  // SHA-256 compression function - optimized by A.B. for pipelined CPU
  asm
    push ebx
    push esi
    push edi
    xor  edi,edi // edi=i
    // rolled version faster than the unrolled one (good pipelining work :)
@s: mov  eax,[H].TSHAHash.E
    mov  ecx,eax
    mov  edx,eax
    mov  ebx,eax // ebx=E
    ror  eax,6
    ror  edx,11
    ror  ecx,25
    xor  eax,edx
    mov  edx,[H].TSHAHash.G
    xor  eax,ecx
    mov  ecx,[H].TSHAHash.H
    add  ecx,eax // T1=ecx
    mov  eax,[H].TSHAHash.F
    mov  [H].TSHAHash.H,edx
    mov  [H].TSHAHash.G,eax
    xor  eax,edx
    mov  [H].TSHAHash.F,ebx
    and  eax,ebx
    xor  eax,edx
    add  eax,dword ptr [K256+edi*4]
    add  eax,ecx
    mov  ecx,[H].TSHAHash.D
    add  eax,dword ptr [W+edi*4]
    mov  ebx,[H].TSHAHash.A
    //  eax= T1 := H + Sum1(E) +(((F xor G) and E) xor G)+K256[i]+W[i];
    add  ecx,eax
    mov  esi,eax  // esi = T1
    mov  [H].TSHAHash.E,ecx // E := D + T1;
    mov  eax,ebx // Sum0(A)
    mov  edx,ebx
    ror  eax,2
    mov  ecx,ebx
    ror  edx,13
    ror  ecx,22
    xor  eax,edx
    xor  eax,ecx // eax = Sum0(A)
    mov  ecx,[H].TSHAHash.B
    add  esi,eax
    mov  eax,ebx // ebx=A
    mov  edx,ebx // eax=edx=A
    or   eax,ecx
    and  eax,[H].TSHAHash.C   // eax = (A or B)and C
    and  edx,ecx
    or   eax,edx // eax = ((A or B)and C) or (A and B)
    inc  edi
    add  esi,eax  // esi= T1+T2
    mov  [H].TSHAHash.A,esi // all these instructions are pipelined -> roll OK
    mov  eax,[H].TSHAHash.C // eax=C ecx=B ebx=A
    mov  [H].TSHAHash.B,ebx
    mov  [H].TSHAHash.C,ecx
    mov  [H].TSHAHash.D,eax
    cmp  edi,64
    jnz  @s
    pop  edi
    pop  esi
    pop  ebx
  end;
{$endif PUREPASCAL}
  // calculate new working hash
  inc(Hash.A,H.A);
  inc(Hash.B,H.B);
  inc(Hash.C,H.C);
  inc(Hash.D,H.D);
  inc(Hash.E,H.E);
  inc(Hash.F,H.F);
  inc(Hash.G,H.G);
  inc(Hash.H,H.H);
end;

procedure RawSha256Compress(var Hash; Data: pointer);
begin
  {$ifdef CPUX64}
  if K256AlignedStore<>'' then // use optimized Intel's sha256_sse4.asm
    sha256_sse4(Data^,Hash,1) else
  {$endif CPUX64}
    Sha256CompressPas(TSHAHash(Hash),Data);
end;

procedure TSHA256.Final(out Digest: TSHA256Digest; NoInit: boolean);
// finalize SHA-256 calculation, clear context
var Data: TSHAContext absolute Context;
begin
  // append bit '1' after Buffer
  Data.Buffer[Data.Index] := $80;
  FillcharFast(Data.Buffer[Data.Index+1],63-Data.Index,0);
  // compress if more than 448 bits (no space for 64 bit length storage)
  if Data.Index>=56 then begin
    RawSha256Compress(Data.Hash,@Data.Buffer);
    FillcharFast(Data.Buffer,56,0);
  end;
  // write 64 bit Buffer length into the last bits of the last block
  // (in big endian format) and do a final compress
  PInteger(@Data.Buffer[56])^ := bswap32(TQWordRec(Data.MLen).H);
  PInteger(@Data.Buffer[60])^ := bswap32(TQWordRec(Data.MLen).L);
  RawSha256Compress(Data.Hash,@Data.Buffer);
  // Hash -> Digest to little endian format
  bswap256(@Data.Hash,@Digest);
  // clear Data and internally stored Digest
  if not NoInit then
    Init;
end;

function TSHA256.Final(NoInit: boolean): TSHA256Digest;
begin
  Final(result,NoInit);
end;

procedure TSHA256.Full(Buffer: pointer; Len: integer; out Digest: TSHA256Digest);
begin
{$ifdef USEPADLOCK}
  // Padlock need all data once -> Full() is OK, not successive Update()
  if padlock_available then begin
    Init; // for later Update use
    {$ifdef PADLOCKDEBUG}write('padlock_phe_sha256 ');{$endif}
    if padlock_phe_sha256(buffer,Len,Digest)=0 then
      exit else
    {$ifdef PADLOCKDEBUG}write(':ERROR ');{$endif}
  end;
{$endif}
  Init;
  Update(Buffer,Len);
  Final(Digest);
end;

procedure TSHA256.Init;
var Data: TSHAContext absolute Context;
begin
  Data.Hash.A := $6a09e667;
  Data.Hash.B := $bb67ae85;
  Data.Hash.C := $3c6ef372;
  Data.Hash.D := $a54ff53a;
  Data.Hash.E := $510e527f;
  Data.Hash.F := $9b05688c;
  Data.Hash.G := $1f83d9ab;
  Data.Hash.H := $5be0cd19;
  FillcharFast(Data.MLen,sizeof(Data)-sizeof(Data.Hash),0);
end;

procedure TSHA256.Update(Buffer: pointer; Len: integer);
var Data: TSHAContext absolute Context;
    aLen: integer;
begin
  if Buffer=nil then exit; // avoid GPF
  inc(Data.MLen,QWord(cardinal(Len)) shl 3);
  {$ifdef CPUX64}
  if (K256AlignedStore<>'') and (Data.Index=0) and (Len>=64) then begin
    // use optimized Intel's sha256_sse4.asm for whole blocks
    sha256_sse4(Buffer^,Data.Hash,Len shr 6);
    inc(PByte(Buffer),Len);
    Len := Len and 63;
    dec(PByte(Buffer),Len);
  end;
  {$endif CPUX64}
  while Len>0 do begin
    aLen := 64-Data.Index;
    if aLen<=Len then begin
      if Data.Index<>0 then begin
        MoveFast(Buffer^,Data.Buffer[Data.Index],aLen);
        RawSha256Compress(Data.Hash,@Data.Buffer);
        Data.Index := 0;
      end else
        RawSha256Compress(Data.Hash,Buffer); // avoid temporary copy
      dec(Len,aLen);
      inc(PByte(Buffer),aLen);
    end else begin
      MoveFast(Buffer^,Data.Buffer[Data.Index],Len);
      inc(Data.Index,Len);
      break;
    end;
  end;
end;

procedure TSHA256.Update(const Buffer: RawByteString);
begin
  Update(pointer(Buffer),length(Buffer));
end;

procedure SHA256Weak(const s: RawByteString; out Digest: TSHA256Digest);
var L: integer;
    SHA: TSHA256;
    p: PAnsiChar;
    tmp: array[0..255] of byte;
begin
  L := length(s);
  p := pointer(s);
  if L<sizeof(tmp) then begin
    FillcharFast(tmp,sizeof(tmp),L); // add some salt to unweak password
    if L>0 then
      MoveFast(p^,tmp,L);
    SHA.Full(@tmp,sizeof(tmp),Digest);
  end else
    SHA.Full(p,L,Digest);
end;


{ common SHA384/SHA512 hashing kernel }

const
  SHA512K: array[0..79] of QWord = (
    QWord($428a2f98d728ae22),QWord($7137449123ef65cd),QWord($b5c0fbcfec4d3b2f),QWord($e9b5dba58189dbbc),
    QWord($3956c25bf348b538),QWord($59f111f1b605d019),QWord($923f82a4af194f9b),QWord($ab1c5ed5da6d8118),
    QWord($d807aa98a3030242),QWord($12835b0145706fbe),QWord($243185be4ee4b28c),QWord($550c7dc3d5ffb4e2),
    QWord($72be5d74f27b896f),QWord($80deb1fe3b1696b1),QWord($9bdc06a725c71235),QWord($c19bf174cf692694),
    QWord($e49b69c19ef14ad2),QWord($efbe4786384f25e3),QWord($0fc19dc68b8cd5b5),QWord($240ca1cc77ac9c65),
    QWord($2de92c6f592b0275),QWord($4a7484aa6ea6e483),QWord($5cb0a9dcbd41fbd4),QWord($76f988da831153b5),
    QWord($983e5152ee66dfab),QWord($a831c66d2db43210),QWord($b00327c898fb213f),QWord($bf597fc7beef0ee4),
    QWord($c6e00bf33da88fc2),QWord($d5a79147930aa725),QWord($06ca6351e003826f),QWord($142929670a0e6e70),
    QWord($27b70a8546d22ffc),QWord($2e1b21385c26c926),QWord($4d2c6dfc5ac42aed),QWord($53380d139d95b3df),
    QWord($650a73548baf63de),QWord($766a0abb3c77b2a8),QWord($81c2c92e47edaee6),QWord($92722c851482353b),
    QWord($a2bfe8a14cf10364),QWord($a81a664bbc423001),QWord($c24b8b70d0f89791),QWord($c76c51a30654be30),
    QWord($d192e819d6ef5218),QWord($d69906245565a910),QWord($f40e35855771202a),QWord($106aa07032bbd1b8),
    QWord($19a4c116b8d2d0c8),QWord($1e376c085141ab53),QWord($2748774cdf8eeb99),QWord($34b0bcb5e19b48a8),
    QWord($391c0cb3c5c95a63),QWord($4ed8aa4ae3418acb),QWord($5b9cca4f7763e373),QWord($682e6ff3d6b2b8a3),
    QWord($748f82ee5defb2fc),QWord($78a5636f43172f60),QWord($84c87814a1f0ab72),QWord($8cc702081a6439ec),
    QWord($90befffa23631e28),QWord($a4506cebde82bde9),QWord($bef9a3f7b2c67915),QWord($c67178f2e372532b),
    QWord($ca273eceea26619c),QWord($d186b8c721c0c207),QWord($eada7dd6cde0eb1e),QWord($f57d4f7fee6ed178),
    QWord($06f067aa72176fba),QWord($0a637dc5a2c898a6),QWord($113f9804bef90dae),QWord($1b710b35131c471b),
    QWord($28db77f523047d84),QWord($32caab7b40c72493),QWord($3c9ebe0a15c9bebc),QWord($431d67c49c100d4c),
    QWord($4cc5d4becb3e42b6),QWord($597f299cfc657e2a),QWord($5fcb6fab3ad6faec),QWord($6c44198c4a475817));

procedure sha512_compresspas(var Hash: TSHA512Hash; Data: PQWordArray);
var a,b,c,d,e,f,g,h, temp1,temp2: QWord; // to use registers on CPU64
    w: array[0..79] of QWord;
    i: integer;
begin
  bswap64array(Data,@w,16);
  for i := 16 to 79 do
    {$ifdef FPC} // uses faster built-in right rotate intrinsic
    w[i] := (RorQWord(w[i-2],19) xor RorQWord(w[i-2],61) xor (w[i-2] shr 6)) +
      w[i-7] + (RorQWord(w[i-15],1) xor RorQWord(w[i-15],8) xor (w[i-15] shr 7)) + w[i-16];
    {$else}
    w[i] := (((w[i-2] shr 19) or (w[i-2] shl 45)) xor ((w[i-2] shr 61) or (w[i-2] shl 3)) xor
      (w[i-2] shr 6)) + w[i-7] + (((w[i-15] shr 1) or (w[i-15] shl 63)) xor
      ((w[i-15] shr 8) or (w[i-15] shl 56)) xor (w[i-15] shr 7)) + w[i-16];
    {$endif}
  a := Hash.a;
  b := Hash.b;
  c := Hash.c;
  d := Hash.d;
  e := Hash.e;
  f := Hash.f;
  g := Hash.g;
  h := Hash.h;
  for i := 0 to 79 do begin
    {$ifdef FPC}
    temp1 := h + (RorQWord(e,14) xor RorQWord(e,18) xor RorQWord(e,41)) +
      ((e and f) xor (not e and g)) + SHA512K[i] + w[i];
    temp2 := (RorQWord(a,28) xor RorQWord(a,34) xor RorQWord(a,39)) +
      ((a and b) xor (a and c) xor (b and c));
    {$else}
    temp1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor
      ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + SHA512K[i] + w[i];
    temp2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor
      ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
    {$endif}
    h := g;
    g := f;
    f := e;
    e := d + temp1;
    d := c;
    c := b;
    b := a;
    a := temp1 + temp2;
  end;
  inc(Hash.a,a);
  inc(Hash.b,b);
  inc(Hash.c,c);
  inc(Hash.d,d);
  inc(Hash.e,e);
  inc(Hash.f,f);
  inc(Hash.g,g);
  inc(Hash.h,h);
end;

{$ifdef SHA512_X86} // optimized asm using SSE3 instructions for x86 32-bit
{$ifdef FPC}
  {$ifdef MSWINDOWS}
    {$L static\i386-win32\sha512-x86.o}
  {$else}
    {$L static/i386-linux/sha512-x86.o}
  {$endif}
{$else}
  {$L sha512-x86.obj}
{$endif}
{
  SHA-512 hash in x86 assembly
  Copyright (c) 2014 Project Nayuki. (MIT License)
  https://www.nayuki.io/page/fast-sha2-hashes-in-x86-assembly

  Permission is hereby granted, free of charge, to any person obtaining a copy of
  this software and associated documentation files (the "Software"), to deal in
  the Software without restriction, including without limitation the rights to
  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
  the Software, and to permit persons to whom the Software is furnished to do so,
  subject to the following conditions:
  - The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
  - The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall the
  authors or copyright holders be liable for any claim, damages or other liability,
  whether in an action of contract, tort or otherwise, arising from, out of or
  in connection with the Software or the use or other dealings in the Software.
}
procedure sha512_compress(state: PQWord; block: PByteArray); cdecl; external;
{$endif SHA512_X86}


{$ifdef SHA512_X64} // optimized asm using SSE4 instructions for x64 64-bit
{$ifdef FPC}
  {$ifdef MSWINDOWS}
    {$L sha512-x64sse4.obj}
  {$else}
    {$L static/x86_64-linux/sha512-x64sse4.o}
  {$endif}
{$else}
  {$L sha512-x64sse4.obj}
{$endif}

procedure sha512_sse4(data, hash: pointer; blocks: Int64); {$ifdef FPC}cdecl;{$endif} external;
{$endif SHA512_X64}

procedure RawSha512Compress(var Hash; Data: pointer);
begin
  {$ifdef SHA512_X86}
  if cfSSSE3 in CpuFeatures then
    sha512_compress(@Hash,Data) else
  {$endif}
  {$ifdef SHA512_X64}
  if cfSSE41 in CpuFeatures then
    sha512_sse4(Data,@Hash,1) else
  {$endif}
    sha512_compresspas(TSHA512Hash(Hash), Data);
end;


{ TSHA384 }

procedure TSHA384.Final(out Digest: TSHA384Digest; NoInit: boolean);
begin
  Data[Index] := $80;
  FillcharFast(Data[Index+1],127-Index,0);
  if Index>=112 then begin
    RawSha512Compress(Hash,@Data);
    FillcharFast(Data,112,0);
  end;
  PQWord(@Data[112])^ := bswap64(MLen shr 61);
  PQWord(@Data[120])^ := bswap64(MLen shl 3);
  RawSha512Compress(Hash,@Data);
  bswap64array(@Hash,@Digest,6);
  if not NoInit then
    Init;
end;

function TSHA384.Final(NoInit: boolean): TSHA384Digest;
begin
  Final(result,NoInit);
end;

procedure TSHA384.Full(Buffer: pointer; Len: integer; out Digest: TSHA384Digest);
begin
  Init;
  Update(Buffer,Len); // final bytes
  Final(Digest);
end;

procedure TSHA384.Init;
begin
  Hash.a := QWord($cbbb9d5dc1059ed8);
  Hash.b := QWord($629a292a367cd507);
  Hash.c := QWord($9159015a3070dd17);
  Hash.d := QWord($152fecd8f70e5939);
  Hash.e := QWord($67332667ffc00b31);
  Hash.f := QWord($8eb44a8768581511);
  Hash.g := QWord($db0c2e0d64f98fa7);
  Hash.h := QWord($47b5481dbefa4fa4);
  MLen := 0;
  Index := 0;
  FillcharFast(Data,sizeof(Data),0);
end;

procedure TSHA384.Update(Buffer: pointer; Len: integer);
var aLen: integer;
begin
  if (Buffer=nil) or (Len<=0) then exit; // avoid GPF
  inc(MLen,Len);
  repeat
    aLen := sizeof(Data)-Index;
    if aLen<=Len then begin
      if Index<>0 then begin
        MoveFast(Buffer^,Data[Index],aLen);
        RawSha512Compress(Hash,@Data);
        Index := 0;
      end else // avoid temporary copy
        RawSha512Compress(Hash,Buffer);
      dec(Len,aLen);
      inc(PByte(Buffer),aLen);
    end else begin
      MoveFast(Buffer^,Data[Index],Len);
      inc(Index,Len);
      break;
    end;
  until Len<=0;
end;

procedure TSHA384.Update(const Buffer: RawByteString);
begin
  Update(pointer(Buffer),length(Buffer));
end;


{ TSHA512 }

procedure TSHA512.Final(out Digest: TSHA512Digest; NoInit: boolean);
begin
  Data[Index] := $80;
  FillcharFast(Data[Index+1],127-Index,0);
  if Index>=112 then begin
    RawSha512Compress(Hash,@Data);
    FillcharFast(Data,112,0);
  end;
  PQWord(@Data[112])^ := bswap64(MLen shr 61);
  PQWord(@Data[120])^ := bswap64(MLen shl 3);
  RawSha512Compress(Hash,@Data);
  bswap64array(@Hash,@Digest,8);
  if not NoInit then
    Init;
end;

function TSHA512.Final(NoInit: boolean): TSHA512Digest;
begin
  Final(result,NoInit);
end;

procedure TSHA512.Full(Buffer: pointer; Len: integer; out Digest: TSHA512Digest);
begin
  Init;
  Update(Buffer,Len); // final bytes
  Final(Digest);
end;

procedure TSHA512.Init;
begin
  Hash.a := $6a09e667f3bcc908;
  Hash.b := QWord($bb67ae8584caa73b);
  Hash.c := $3c6ef372fe94f82b;
  Hash.d := QWord($a54ff53a5f1d36f1);
  Hash.e := $510e527fade682d1;
  Hash.f := QWord($9b05688c2b3e6c1f);
  Hash.g := $1f83d9abfb41bd6b;
  Hash.h := $5be0cd19137e2179;
  MLen := 0;
  Index := 0;
  FillcharFast(Data,sizeof(Data),0);
end;

procedure TSHA512.Update(Buffer: pointer; Len: integer);
var aLen: integer;
begin
  if (Buffer=nil) or (Len<=0) then exit; // avoid GPF
  inc(MLen,Len);
  repeat
    aLen := sizeof(Data)-Index;
    if aLen<=Len then begin
      if Index<>0 then begin
        MoveFast(Buffer^,Data[Index],aLen);
        RawSha512Compress(Hash,@Data);
        Index := 0;
      end else // avoid temporary copy
        RawSha512Compress(Hash,Buffer);
      dec(Len,aLen);
      inc(PByte(Buffer),aLen);
    end else begin
      MoveFast(Buffer^,Data[Index],Len);
      inc(Index,Len);
      break;
    end;
  until Len<=0;
end;

procedure TSHA512.Update(const Buffer: RawByteString);
begin
  Update(pointer(Buffer),length(Buffer));
end;


{ TSHA3 }

{ SHA-3 / Keccak original code (c) Wolfgang Ehrhardt under zlib license:
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:
 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.
 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.
 3. This notice may not be removed or altered from any source distribution. }

const
  cKeccakPermutationSize = 1600;
  cKeccakMaximumRate = 1536;
  cKeccakPermutationSizeInBytes = cKeccakPermutationSize div 8;
  cKeccakMaximumRateInBytes = cKeccakMaximumRate div 8;
  cKeccakNumberOfRounds = 24;
  cRoundConstants: array[0..cKeccakNumberOfRounds-1] of QWord = (
    QWord($0000000000000001), QWord($0000000000008082), QWord($800000000000808A),
    QWord($8000000080008000), QWord($000000000000808B), QWord($0000000080000001),
    QWord($8000000080008081), QWord($8000000000008009), QWord($000000000000008A),
    QWord($0000000000000088), QWord($0000000080008009), QWord($000000008000000A),
    QWord($000000008000808B), QWord($800000000000008B), QWord($8000000000008089),
    QWord($8000000000008003), QWord($8000000000008002), QWord($8000000000000080),
    QWord($000000000000800A), QWord($800000008000000A), QWord($8000000080008081),
    QWord($8000000000008080), QWord($0000000080000001), QWord($8000000080008008));

type
  {$ifdef USERECORDWITHMETHODS}TSHA3Context = record
    {$else}TSHA3Context = object{$endif}
  public
    State: packed array[0..cKeccakPermutationSizeInBytes-1] of byte;
    DataQueue: packed array[0..cKeccakMaximumRateInBytes-1] of byte;
    Algo: TSHA3Algo;
    Squeezing: boolean;
    Rate: integer;
    Capacity: integer;
    BitsInQueue: integer;
    BitsAvailableForSqueezing: integer;
    procedure Init(aAlgo: TSHA3Algo);
    procedure InitSponge(aRate, aCapacity: integer);
    procedure AbsorbQueue;
    procedure Absorb(data: PByteArray; databitlen: integer);
    procedure AbsorbFinal(data: PByteArray; databitlen: integer);
    procedure PadAndSwitchToSqueezingPhase;
    procedure Squeeze(output: PByteArray; outputLength: integer);
    procedure FinalBit_LSB(bits: byte; bitlen: integer;
      hashval: Pointer; numbits: integer);
  end;
  PSHA3Context = ^TSHA3Context;

{$ifdef SHA3_PASCAL}

{$ifdef FPC} // RotL/RolQword is an intrinsic function under FPC :)

function RotL(const x: QWord; c: integer): QWord; inline;
begin
  result := RolQword(x, c);
end;

function RotL1(const x: QWord): QWord; inline;
begin
  result := RolQword(x);
end;

{$else}

function RotL(const x: QWord; c: integer): QWord; {$ifdef HASINLINE}inline;{$endif}
begin
  result := (x shl c) or (x shr (64 - c));
end;

function RotL1(var x: QWord): QWord; {$ifdef HASINLINE}inline;{$endif}
begin
  result := (x shl 1) or (x shr (64 - 1));
end;

{$endif FPC}

procedure KeccakPermutation(A: PQWordArray);
var
  B: array[0..24] of QWord;
  C0, C1, C2, C3, C4, D0, D1, D2, D3, D4: QWord;
  i: integer;
begin
  for i := 0 to 23 do begin
    C0 := A[00] xor A[05] xor A[10] xor A[15] xor A[20];
    C1 := A[01] xor A[06] xor A[11] xor A[16] xor A[21];
    C2 := A[02] xor A[07] xor A[12] xor A[17] xor A[22];
    C3 := A[03] xor A[08] xor A[13] xor A[18] xor A[23];
    C4 := A[04] xor A[09] xor A[14] xor A[19] xor A[24];
    D0 := RotL1(C0) xor C3;
    D1 := RotL1(C1) xor C4;
    D2 := RotL1(C2) xor C0;
    D3 := RotL1(C3) xor C1;
    D4 := RotL1(C4) xor C2;
    B[00] := A[00] xor D1;
    B[01] := RotL(A[06] xor D2, 44);
    B[02] := RotL(A[12] xor D3, 43);
    B[03] := RotL(A[18] xor D4, 21);
    B[04] := RotL(A[24] xor D0, 14);
    B[05] := RotL(A[03] xor D4, 28);
    B[06] := RotL(A[09] xor D0, 20);
    B[07] := RotL(A[10] xor D1, 3);
    B[08] := RotL(A[16] xor D2, 45);
    B[09] := RotL(A[22] xor D3, 61);
    B[10] := RotL(A[01] xor D2, 1);
    B[11] := RotL(A[07] xor D3, 6);
    B[12] := RotL(A[13] xor D4, 25);
    B[13] := RotL(A[19] xor D0, 8);
    B[14] := RotL(A[20] xor D1, 18);
    B[15] := RotL(A[04] xor D0, 27);
    B[16] := RotL(A[05] xor D1, 36);
    B[17] := RotL(A[11] xor D2, 10);
    B[18] := RotL(A[17] xor D3, 15);
    B[19] := RotL(A[23] xor D4, 56);
    B[20] := RotL(A[02] xor D3, 62);
    B[21] := RotL(A[08] xor D4, 55);
    B[22] := RotL(A[14] xor D0, 39);
    B[23] := RotL(A[15] xor D1, 41);
    B[24] := RotL(A[21] xor D2, 2);
    A[00] := B[00] xor ((not B[01]) and B[02]);
    A[01] := B[01] xor ((not B[02]) and B[03]);
    A[02] := B[02] xor ((not B[03]) and B[04]);
    A[03] := B[03] xor ((not B[04]) and B[00]);
    A[04] := B[04] xor ((not B[00]) and B[01]);
    A[05] := B[05] xor ((not B[06]) and B[07]);
    A[06] := B[06] xor ((not B[07]) and B[08]);
    A[07] := B[07] xor ((not B[08]) and B[09]);
    A[08] := B[08] xor ((not B[09]) and B[05]);
    A[09] := B[09] xor ((not B[05]) and B[06]);
    A[10] := B[10] xor ((not B[11]) and B[12]);
    A[11] := B[11] xor ((not B[12]) and B[13]);
    A[12] := B[12] xor ((not B[13]) and B[14]);
    A[13] := B[13] xor ((not B[14]) and B[10]);
    A[14] := B[14] xor ((not B[10]) and B[11]);
    A[15] := B[15] xor ((not B[16]) and B[17]);
    A[16] := B[16] xor ((not B[17]) and B[18]);
    A[17] := B[17] xor ((not B[18]) and B[19]);
    A[18] := B[18] xor ((not B[19]) and B[15]);
    A[19] := B[19] xor ((not B[15]) and B[16]);
    A[20] := B[20] xor ((not B[21]) and B[22]);
    A[21] := B[21] xor ((not B[22]) and B[23]);
    A[22] := B[22] xor ((not B[23]) and B[24]);
    A[23] := B[23] xor ((not B[24]) and B[20]);
    A[24] := B[24] xor ((not B[20]) and B[21]);
    A[00] := A[00] xor cRoundConstants[i];
  end;
end;

{$else SHA3_PASCAL}

{ - MMX 32-bit assembler version based on optimized SHA-3 kernel by Eric Grange
   https://www.delphitools.info/2016/04/19/new-sha-3-permutation-kernel
  - new x64 assembler version by Synopse }

procedure KeccakPermutationKernel(B, A, C: Pointer);
{$ifdef CPU32} // Eric Grange's MMX version (PIC-safe)
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        add     edx, 128
        add     eax, 128
        movq    mm1, [edx - 120]
        movq    mm4, [edx - 96]
        movq    mm3, [edx - 104]
        pxor    mm1, [edx - 80]
        movq    mm5, [edx + 16]
        pxor    mm1, [edx]
        movq    mm2, [edx - 112]
        pxor    mm1, [edx + 40]
        pxor    mm1, [edx - 40]
        movq    mm0, [edx - 128]
        movq    mm6, mm1
        pxor    mm4, [edx - 56]
        movq    [ecx + 8], mm1
        psrlq   mm6, 63
        pxor    mm4, [edx + 24]
        pxor    mm4, [edx + 64]
        pxor    mm4, [edx - 16]
        psllq   mm1, 1
        pxor    mm2, [edx + 48]
        por     mm1, mm6
        movq    mm6, [edx - 88]
        pxor    mm1, mm4
        pxor    mm2, [edx - 32]
        pxor    mm2, [edx - 72]
        pxor    mm6, mm1
        movq    mm7, mm6
        psrlq   mm7, 28
        psllq   mm6, 36
        por     mm6, mm7
        pxor    mm2, [edx + 8]
        movq    [eax], mm6
        movq    mm6, [edx + 32]
        movq    mm7, mm4
        psrlq   mm7, 63
        psllq   mm4, 1
        pxor    mm0, mm6
        por     mm4, mm7
        pxor    mm4, mm2
        pxor    mm5, mm4
        movq    mm7, mm5
        pxor    mm0, [edx - 8]
        psllq   mm5, 21
        psrlq   mm7, 43
        pxor    mm6, mm1
        por     mm5, mm7
        movq    [eax - 104], mm5
        movq    mm5, [edx - 48]
        pxor    mm0, mm5
        movq    mm7, mm6
        psrlq   mm7, 46
        psllq   mm6, 18
        por     mm6, mm7
        movq    [eax - 16], mm6
        movq    mm6, [edx + 56]
        pxor    mm5, mm1
        movq    mm7, mm5
        pxor    mm3, mm6
        psllq   mm5, 3
        psrlq   mm7, 61
        pxor    mm3, [edx + 16]
        pxor    mm3, [edx - 24]
        por     mm5, mm7
        pxor    mm6, mm4
        pxor    mm0, [edx - 88]
        movq    mm7, mm6
        psrlq   mm7, 8
        movq    [eax - 72], mm5
        movq    mm5, mm2
        psllq   mm2, 1
        psllq   mm6, 56
        psrlq   mm5, 63
        por     mm6, mm7
        por     mm2, mm5
        pxor    mm2, mm0
        movq    [eax + 24], mm6
        movq    mm5, [edx - 120]
        movq    mm6, mm0
        psllq   mm0, 1
        pxor    mm5, mm2
        pxor    mm3, [edx - 64]
        psrlq   mm6, 63
        por     mm0, mm6
        movq    mm6, [edx - 64]
        movq    mm7, mm5
        psllq   mm5, 1
        psrlq   mm7, 63
        pxor    mm6, mm4
        por     mm5, mm7
        pxor    mm0, mm3
        movq    mm7, mm6
        movq    [eax - 48], mm5
        movq    mm5, [edx]
        psllq   mm6, 55
        psrlq   mm7, 9
        por     mm6, mm7
        movq    [eax + 40], mm6
        movq    mm6, [edx - 40]
        pxor    mm5, mm2
        movq    mm7, mm5
        psllq   mm5, 45
        psrlq   mm7, 19
        pxor    mm6, mm2
        por     mm5, mm7
        movq    [eax - 64], mm5
        movq    mm5, [edx + 40]
        movq    mm7, mm6
        pxor    mm5, mm2
        psllq   mm6, 10
        psrlq   mm7, 54
        por     mm6, mm7
        movq    [eax + 8], mm6
        movq    mm6, [edx - 96]
        movq    mm7, mm3
        psrlq   mm7, 63
        psllq   mm3, 1
        por     mm3, mm7
        movq    mm7, mm5
        psllq   mm5, 2
        psrlq   mm7, 62
        por     mm5, mm7
        movq    [eax + 64], mm5
        movq    mm5, [edx + 24]
        pxor    mm6, mm0
        movq    mm7, mm6
        psrlq   mm7, 37
        psllq   mm6, 27
        por     mm6, mm7
        movq    [eax - 8], mm6
        pxor    mm5, mm0
        movq    mm6, [edx - 16]
        movq    mm7, mm5
        psllq   mm5, 8
        pxor    mm3, [ecx + 8]
        psrlq   mm7, 56
        pxor    mm6, mm0
        por     mm5, mm7
        movq    [eax - 24], mm5
        movq    mm7, mm6
        psllq   mm6, 39
        movq    mm5, [edx - 112]
        psrlq   mm7, 25
        por     mm6, mm7
        movq    [eax + 48], mm6
        movq    mm6, [edx - 24]
        pxor    mm5, mm3
        movq    mm7, mm5
        psrlq   mm7, 2
        psllq   mm5, 62
        por     mm5, mm7
        movq    [eax + 32], mm5
        movq    mm5, [edx - 104]
        pxor    mm6, mm4
        movq    mm7, mm6
        psrlq   mm7, 39
        psllq   mm6, 25
        por     mm6, mm7
        pxor    mm5, mm4
        movq    [eax - 32], mm6
        movq    mm6, [edx - 128]
        pxor    mm6, mm1
        movq    mm4, mm6
        movq    [eax - 128], mm6
        movq    mm4, mm6
        movq    mm6, [edx - 8]
        movq    mm7, mm5
        psrlq   mm7, 36
        psllq   mm5, 28
        pxor    mm6, mm1
        por     mm5, mm7
        movq    mm7, mm6
        psrlq   mm7, 23
        movq    mm1, mm5
        movq    [eax - 88], mm5
        movq    mm5, [edx - 56]
        pxor    mm5, mm0
        psllq   mm6, 41
        por     mm6, mm7
        movq    [eax + 56], mm6
        movq    mm6, [edx + 48]
        pxor    mm6, mm3
        movq    mm7, mm5
        psrlq   mm7, 44
        psllq   mm5, 20
        por     mm5, mm7
        movq    [eax - 80], mm5
        pandn   mm1, mm5
        movq    mm5, [edx - 32]
        movq    mm7, mm6
        psrlq   mm7, 3
        psllq   mm6, 61
        por     mm6, mm7
        pxor    mm1, mm6
        movq    [eax - 56], mm6
        movq    mm6, [edx + 8]
        movq    [edx - 56], mm1
        movq    mm1, [eax - 112]
        pxor    mm5, mm3
        movq    mm7, mm5
        psllq   mm5, 43
        psrlq   mm7, 21
        pxor    mm6, mm3
        por     mm5, mm7
        movq    mm1, mm5
        movq    mm5, [edx - 80]
        pxor    mm5, mm2
        movq    mm2, [eax - 104]
        movq    mm7, mm6
        psrlq   mm7, 49
        psllq   mm6, 15
        por     mm6, mm7
        movq    [eax + 16], mm6
        movq    mm6, [edx + 64]
        movq    [eax - 96], mm6
        movq    mm7, mm5
        psrlq   mm7, 20
        psllq   mm5, 44
        pxor    mm6, mm0
        por     mm5, mm7
        movq    mm7, mm6
        psrlq   mm7, 50
        psllq   mm6, 14
        por     mm6, mm7
        pandn   mm2, mm6
        movq    mm0, mm5
        pandn   mm0, mm1
        pxor    mm2, mm1
        pandn   mm1, [eax - 104]
        movq    [edx - 112], mm2
        pandn   mm4, mm5
        pxor    mm1, mm5
        movq    [eax - 120], mm5
        movq    mm2, [eax - 40]
        movq    [edx - 120], mm1
        movq    mm5, [edx - 72]
        movq    mm1, [eax - 64]
        pxor    mm4, mm6
        movq    [edx - 96], mm4
        pxor    mm5, mm3
        movq    mm4, [eax - 88]
        movq    mm7, mm5
        movq    mm3, mm6
        pxor    mm0, [eax - 128]
        movq    [edx - 128], mm0
        movq    mm6, [eax - 72]
        psllq   mm5, 6
        psrlq   mm7, 58
        movq    mm0, [eax - 56]
        por     mm5, mm7
        movq    mm2, mm5
        movq    mm5, [eax - 80]
        movq    mm7, mm1
        pandn   mm7, mm0
        pxor    mm7, mm6
        movq    [edx - 72], mm7
        movq    mm7, [eax - 72]
        pandn   mm6, mm1
        pxor    mm6, mm5
        pandn   mm0, mm4
        pandn   mm5, mm7
        movq    mm7, [eax]
        pxor    mm5, mm4
        movq    mm4, [eax - 24]
        movq    [edx - 80], mm6
        movq    mm6, [eax - 48]
        movq    [edx - 88], mm5
        movq    mm5, mm1
        movq    mm1, [eax - 16]
        pxor    mm0, mm5
        movq    mm5, mm1
        pandn   mm3, [eax - 128]
        pxor    mm3, [eax - 104]
        movq    [edx - 64], mm0
        movq    mm0, [eax + 8]
        movq    [edx - 104], mm3
        movq    mm3, [eax - 32]
        pandn   mm6, mm2
        pxor    mm6, mm5
        movq    [edx - 16], mm6
        movq    mm6, [eax + 56]
        pandn   mm3, mm4
        pxor    mm3, mm2
        movq    [edx - 40], mm3
        movq    mm3, [eax - 32]
        pandn   mm5, [eax - 48]
        pxor    mm5, mm4
        movq    [edx - 24], mm5
        pandn   mm7, mm0
        movq    mm5, [eax + 16]
        pandn   mm4, mm1
        pxor    mm4, mm3
        movq    [edx - 32], mm4
        movq    mm4, [eax + 40]
        movq    mm1, mm5
        movq    mm5, [eax + 48]
        pandn   mm5, mm6
        pxor    mm5, mm4
        pandn   mm2, mm3
        movq    mm3, [eax - 8]
        movq    [edx + 40], mm5
        movq    mm5, [eax + 24]
        pxor    mm7, mm3
        movq    [edx - 8], mm7
        movq    mm7, [eax + 64]
        pxor    mm2, [eax - 48]
        movq    [edx - 48], mm2
        movq    mm2, mm5
        pandn   mm2, mm3
        pxor    mm2, mm1
        movq    [edx + 16], mm2
        pandn   mm3, [eax]
        movq    mm2, mm5
        movq    mm5, [eax + 48]
        pandn   mm6, mm7
        pxor    mm6, mm5
        movq    [edx + 48], mm6
        pandn   mm1, mm2
        movq    mm6, [eax + 32]
        pxor    mm1, mm0
        pxor    mm3, mm2
        movq    [edx + 24], mm3
        pandn   mm0, [eax + 16]
        pxor    mm0, [eax]
        movq    mm3, mm4
        movq    [edx + 8], mm1
        movq    [edx], mm0
        movq    mm0, mm6
        movq    mm1, [eax + 56]
        pandn   mm4, mm5
        pxor    mm4, mm0
        pandn   mm0, mm3
        pxor    mm0, mm7
        movq    [edx + 32], mm4
        pandn   mm7, mm6
        pxor    mm7, mm1
        movq    [edx + 56], mm7
        movq    [edx + 64], mm0
{$else}
{$ifdef FPC}nostackframe; assembler; asm{$else}
// Synopse's x64 asm, optimized for both in+out-order pipelined CPUs
asm // input: rcx=B, rdx=A, r8=C (Linux: rdi,rsi,rdx)
        .noframe
{$endif}{$ifndef win64}
        mov     r8, rdx
        mov     rdx, rsi
        mov     rcx, rdi
        {$endif win64}
        push    rbx
        push    r12
        push    r13
        push    r14
        add     rdx, 128
        add     rcx, 128
        // theta
        mov     r10, [rdx - 128]
        mov     r11, [rdx - 120]
        mov     r12, [rdx - 112]
        mov     r13, [rdx - 104]
        mov     r14, [rdx - 96]
        xor     r10, [rdx - 88]
        xor     r11, [rdx - 80]
        xor     r12, [rdx - 72]
        xor     r13, [rdx - 64]
        xor     r14, [rdx - 56]
        xor     r10, [rdx - 48]
        xor     r11, [rdx - 40]
        xor     r12, [rdx - 32]
        xor     r13, [rdx - 24]
        xor     r14, [rdx - 16]
        xor     r10, [rdx - 8]
        xor     r11, [rdx]
        xor     r12, [rdx + 8]
        xor     r13, [rdx + 16]
        xor     r14, [rdx + 24]
        xor     r10, [rdx + 32]
        xor     r11, [rdx + 40]
        xor     r12, [rdx + 48]
        xor     r13, [rdx + 56]
        xor     r14, [rdx + 64]
        mov     [r8], r10
        mov     [r8 + 8], r11
        mov     [r8 + 16], r12
        mov     [r8 + 24], r13
        mov     [r8 + 32], r14
        rol     r10, 1
        rol     r11, 1
        rol     r12, 1
        rol     r13, 1
        rol     r14, 1
        xor     r10, [r8 + 24]
        xor     r11, [r8 + 32]
        xor     r12, [r8]
        xor     r13, [r8 + 8]
        xor     r14, [r8 + 16]
        // rho pi
        mov     rax, [rdx - 128]
        mov     r8, [rdx - 80]
        mov     r9, [rdx - 32]
        mov     rbx, [rdx + 16]
        xor     rax, r11
        xor     r8, r12
        xor     r9, r13
        xor     rbx, r14
        rol     r8, 44
        rol     r9, 43
        rol     rbx, 21
        mov     [rcx - 128], rax
        mov     [rcx - 120], r8
        mov     [rcx - 112], r9
        mov     [rcx - 104], rbx
        mov     rax, [rdx + 64]
        mov     r8, [rdx - 104]
        mov     r9, [rdx - 56]
        mov     rbx, [rdx - 48]
        xor     rax, r10
        xor     r8, r14
        xor     r9, r10
        xor     rbx, r11
        rol     rax, 14
        rol     r8, 28
        rol     r9, 20
        rol     rbx, 3
        mov     [rcx - 96], rax
        mov     [rcx - 88], r8
        mov     [rcx - 80], r9
        mov     [rcx - 72], rbx
        mov     rax, [rdx]
        mov     r8, [rdx + 48]
        mov     r9, [rdx - 120]
        mov     rbx, [rdx - 72]
        xor     rax, r12
        xor     r8, r13
        xor     r9, r12
        xor     rbx, r13
        rol     rax, 45
        rol     r8, 61
        rol     r9, 1
        rol     rbx, 6
        mov     [rcx - 64], rax
        mov     [rcx - 56], r8
        mov     [rcx - 48], r9
        mov     [rcx - 40], rbx
        mov     rax, [rdx - 24]
        mov     r8, [rdx + 24]
        mov     r9, [rdx + 32]
        mov     rbx, [rdx - 96]
        xor     rax, r14
        xor     r8, r10
        xor     r9, r11
        xor     rbx, r10
        rol     rax, 25
        rol     r8, 8
        rol     r9, 18
        rol     rbx, 27
        mov     [rcx - 32], rax
        mov     [rcx - 24], r8
        mov     [rcx - 16], r9
        mov     [rcx - 8], rbx
        mov     rax, [rdx - 88]
        mov     r8, [rdx - 40]
        mov     r9, [rdx + 8]
        mov     rbx, [rdx + 56]
        xor     rax, r11
        xor     r8, r12
        xor     r9, r13
        xor     rbx, r14
        rol     rax, 36
        rol     r8, 10
        rol     r9, 15
        rol     rbx, 56
        mov     [rcx], rax
        mov     [rcx + 8], r8
        mov     [rcx + 16], r9
        mov     [rcx + 24], rbx
        mov     rax, [rdx - 112]
        mov     r8, [rdx - 64]
        mov     r9, [rdx - 16]
        mov     rbx, [rdx - 8]
        xor     rax, r13
        xor     r8, r14
        xor     r9, r10
        mov     r10, [rdx + 40]
        xor     rbx, r11
        rol     rax, 62
        rol     r8, 55
        xor     r10, r12
        rol     r9, 39
        rol     rbx, 41
        mov     [rcx + 32], rax
        mov     [rcx + 40], r8
        rol     r10, 2
        mov     [rcx + 48], r9
        mov     [rcx + 56], rbx
        mov     [rcx + 64], r10
        // chi
        mov     rax, [rcx - 120]
        mov     r8, [rcx - 112]
        mov     r9, [rcx - 104]
        mov     r10, [rcx - 96]
        mov     r11, [rcx - 128]
        mov     r12, [rcx - 80]
        mov     r13, [rcx - 72]
        mov     r14, [rcx - 64]
        mov     rbx, [rcx - 56]
        not     rax
        not     r8
        not     r9
        not     r10
        not     r11
        not     r12
        not     r13
        not     r14
        not     rbx
        and     rax, [rcx - 112]
        and     r8, [rcx - 104]
        and     r9, [rcx - 96]
        and     r10, [rcx - 128]
        and     r11, [rcx - 120]
        and     r12, [rcx - 72]
        and     r13, [rcx - 64]
        and     r14, [rcx - 56]
        and     rbx, [rcx - 88]
        xor     rax, [rcx - 128]
        xor     r8, [rcx - 120]
        xor     r9, [rcx - 112]
        xor     r10, [rcx - 104]
        xor     r11, [rcx - 96]
        xor     r12, [rcx - 88]
        xor     r13, [rcx - 80]
        xor     r14, [rcx - 72]
        xor     rbx, [rcx - 64]
        mov     [rdx - 128], rax
        mov     [rdx - 120], r8
        mov     [rdx - 112], r9
        mov     [rdx - 104], r10
        mov     [rdx - 96], r11
        mov     [rdx - 88], r12
        mov     [rdx - 80], r13
        mov     [rdx - 72], r14
        mov     [rdx - 64], rbx
        mov     rax, [rcx - 88]
        mov     rbx, [rcx - 40]
        mov     r8, [rcx - 32]
        mov     r9, [rcx - 24]
        mov     r10, [rcx - 16]
        mov     r11, [rcx - 48]
        mov     r12, [rcx]
        mov     r13, [rcx + 8]
        mov     r14, [rcx + 16]
        not     rax
        not     rbx
        not     r8
        not     r9
        not     r10
        not     r11
        not     r12
        not     r13
        not     r14
        and     rax, [rcx - 80]
        and     rbx, [rcx - 32]
        and     r8, [rcx - 24]
        and     r9, [rcx - 16]
        and     r10, [rcx - 48]
        and     r11, [rcx - 40]
        and     r12, [rcx + 8]
        and     r13, [rcx + 16]
        and     r14, [rcx + 24]
        xor     rax, [rcx - 56]
        xor     rbx, [rcx - 48]
        xor     r8, [rcx - 40]
        xor     r9, [rcx - 32]
        xor     r10, [rcx - 24]
        xor     r11, [rcx - 16]
        xor     r12, [rcx - 8]
        xor     r13, [rcx]
        xor     r14, [rcx + 8]
        mov     [rdx - 56], rax
        mov     [rdx - 48], rbx
        mov     [rdx - 40], r8
        mov     [rdx - 32], r9
        mov     [rdx - 24], r10
        mov     [rdx - 16], r11
        mov     [rdx - 8], r12
        mov     [rdx], r13
        mov     [rdx + 8], r14
        mov     rax, [rcx + 24]
        mov     rbx, [rcx - 8]
        mov     r8, [rcx + 40]
        mov     r9, [rcx + 48]
        mov     r10, [rcx + 56]
        mov     r11, [rcx + 64]
        mov     r12, [rcx + 32]
        not     rax
        not     rbx
        not     r8
        not     r9
        not     r10
        not     r11
        not     r12
        and     rax, [rcx - 8]
        and     rbx, [rcx]
        and     r8, [rcx + 48]
        and     r9, [rcx + 56]
        and     r10, [rcx + 64]
        and     r11, [rcx + 32]
        and     r12, [rcx + 40]
        xor     rax, [rcx + 16]
        xor     rbx, [rcx + 24]
        xor     r8, [rcx + 32]
        xor     r9, [rcx + 40]
        xor     r10, [rcx + 48]
        xor     r11, [rcx + 56]
        xor     r12, [rcx + 64]
        mov     [rdx + 16], rax
        mov     [rdx + 24], rbx
        mov     [rdx + 32], r8
        mov     [rdx + 40], r9
        mov     [rdx + 48], r10
        mov     [rdx + 56], r11
        mov     [rdx + 64], r12
        pop     r14
        pop     r13
        pop     r12
        pop     rbx
{$endif}
end;

procedure KeccakPermutation(A: PQWordArray);
var
  B: array[0..24] of QWord;
  C: array[0..4] of QWord;
  i: integer;
begin
  for i := 0 to 23 do begin
    KeccakPermutationKernel(@B, A, @C);
    A[00] := A[00] xor cRoundConstants[i];
  end;
 {$ifdef CPU32}
  asm
    emms // reset MMX after use
  end;
  {$endif}
end;

{$endif SHA3_PASCAL}

procedure TSHA3Context.Init(aAlgo: TSHA3Algo);
begin
  case aAlgo of
    SHA3_224:
      InitSponge(1152, 448);
    SHA3_256:
      InitSponge(1088, 512);
    SHA3_384:
      InitSponge(832, 768);
    SHA3_512:
      InitSponge(576, 1024);
    SHAKE_128:
      InitSponge(1344, 256);
    SHAKE_256:
      InitSponge(1088, 512);
  else
    raise ESynCrypto.CreateUTF8('Unexpected TSHA3Context.Init(%)', [ord(aAlgo)]);
  end;
  Algo := aAlgo;
end;

procedure TSHA3Context.InitSponge(aRate, aCapacity: integer);
begin
  if (aRate + aCapacity <> 1600) or (aRate <= 0) or (aRate >= 1600) or
     ((aRate and 63) <> 0) then
    raise ESynCrypto.CreateUTF8('Unexpected TSHA3Context.Init(%,%)', [aRate, aCapacity]);
  FillCharFast(self, sizeof(self), 0);
  Rate := aRate;
  Capacity := aCapacity;
end;

procedure TSHA3Context.AbsorbQueue;
begin
  XorMemoryPtrInt(@State, @DataQueue, Rate shr {$ifdef CPU32}5{$else}6{$endif});
  KeccakPermutation(@State);
end;

procedure TSHA3Context.Absorb(data: PByteArray; databitlen: integer);
var
  i, j, wholeBlocks, partialBlock: integer;
  partialByte: integer;
  curData: pointer;
begin
  if BitsInQueue and 7 <> 0 then
    raise ESynCrypto.Create('TSHA3Context.Absorb: only last may contain partial');
  if Squeezing then
    raise ESynCrypto.Create('TSHA3Context.Absorb: too late for additional input');
  i := 0;
  while i < databitlen do begin
    if (BitsInQueue = 0) and (databitlen >= Rate) and (i <= (databitlen - Rate)) then begin
      wholeBlocks := (databitlen - i) div Rate;
      curData := @data^[i shr 3];
      for j := 1 to wholeBlocks do begin
        XorMemoryPtrInt(@State, curData, Rate shr {$ifdef CPU32}5{$else}6{$endif});
        KeccakPermutation(@State);
        inc(PByte(curData), Rate shr 3);
      end;
      inc(i, wholeBlocks * Rate);
    end
    else begin
      partialBlock := databitlen - i;
      if partialBlock + BitsInQueue > Rate then
        partialBlock := Rate - BitsInQueue;
      partialByte := partialBlock and 7;
      dec(partialBlock, partialByte);
      MoveFast(data^[i shr 3], DataQueue[BitsInQueue shr 3], partialBlock shr 3);
      inc(BitsInQueue, partialBlock);
      inc(i, partialBlock);
      if BitsInQueue = Rate then begin
        AbsorbQueue;
        BitsInQueue := 0;
      end;
      if partialByte > 0 then begin
        DataQueue[BitsInQueue shr 3] := data^[i shr 3] and ((1 shl partialByte) - 1);
        inc(BitsInQueue, partialByte);
        inc(i, partialByte);
      end;
    end;
  end;
end;

procedure TSHA3Context.AbsorbFinal(data: PByteArray; databitlen: integer);
var
  lastByte: byte;
begin
  if databitlen and 7 = 0 then
    Absorb(data, databitlen)
  else begin
    Absorb(data, databitlen - (databitlen and 7));
    // Align the last partial byte to the least significant bits
    lastByte := data^[databitlen shr 3] shr (8 - (databitlen and 7));
    Absorb(@lastByte, databitlen and 7);
  end;
end;

procedure TSHA3Context.PadAndSwitchToSqueezingPhase;
var
  i: integer;
begin // note: the bits are numbered from 0=LSB to 7=MSB
  if BitsInQueue + 1 = Rate then begin
    i := BitsInQueue shr 3;
    DataQueue[i] := DataQueue[i] or (1 shl (BitsInQueue and 7));
    AbsorbQueue;
    FillCharFast(DataQueue, Rate shr 3, 0);
  end
  else begin
    i := BitsInQueue shr 3;
    FillCharFast(DataQueue[(BitsInQueue + 7) shr 3], Rate shr 3 - (BitsInQueue + 7) shr 3, 0);
    DataQueue[i] := DataQueue[i] or (1 shl (BitsInQueue and 7));
  end;
  i := (Rate - 1) shr 3;
  DataQueue[i] := DataQueue[i] or (1 shl ((Rate - 1) and 7));
  AbsorbQueue;
  MoveFast(State, DataQueue, Rate shr 3);
  BitsAvailableForSqueezing := Rate;
  Squeezing := true;
end;

procedure TSHA3Context.Squeeze(output: PByteArray; outputLength: integer);
var
  i: integer;
  partialBlock: integer;
begin
  if not Squeezing then
    PadAndSwitchToSqueezingPhase;
  if outputLength and 7 <> 0 then
    raise ESynCrypto.CreateUTF8('TSHA3Context.Squeeze(%?)', [outputLength]);
  i := 0;
  while i < outputLength do begin
    if BitsAvailableForSqueezing = 0 then begin
      KeccakPermutation(@State);
      MoveFast(State, DataQueue, Rate shr 3);
      BitsAvailableForSqueezing := Rate;
    end;
    partialBlock := BitsAvailableForSqueezing;
    if partialBlock > outputLength - i then
      partialBlock := outputLength - i;
    MoveFast(DataQueue[(Rate - BitsAvailableForSqueezing) shr 3],
      output^[i shr 3], partialBlock shr 3);
    dec(BitsAvailableForSqueezing, partialBlock);
    inc(i, partialBlock);
  end;
end;

procedure TSHA3Context.FinalBit_LSB(bits: byte; bitlen: integer;
  hashval: Pointer; numbits: integer);
var
  ll: integer;
  lw: word;
begin
  bitlen := bitlen and 7;
  if bitlen = 0 then
    lw := 0
  else
    lw := bits and Pred(word(1) shl bitlen);
  // 'append' (in LSB language) the domain separation bits
  if Algo >= SHAKE_128 then begin
    // SHAKE: append four bits 1111
    lw := lw or (word($F) shl bitlen);
    ll := bitlen + 4;
  end
  else begin
    // SHA-3: append two bits 01
    lw := lw or (word($2) shl bitlen);
    ll := bitlen + 2;
  end;
  // update state with final bits
  if ll < 9 then begin // 0..8 bits, one call to update
    lw := lw shl (8 - ll);
    AbsorbFinal(@lw, ll);
    // squeeze the digits from the sponge
    Squeeze(hashval, numbits);
  end
  else begin
    // more than 8 bits, first a regular update with low byte
    AbsorbFinal(@lw, 8);
    // finally update remaining last bits
    dec(ll, 8);
    lw := lw shr ll;
    AbsorbFinal(@lw, ll);
    Squeeze(hashval, numbits);
  end;
end;

procedure TSHA3.Init(Algo: TSHA3Algo);
begin
  PSHA3Context(@Context)^.Init(Algo);
end;

function TSHA3.Algorithm: TSHA3Algo;
begin
  result := PSHA3Context(@Context)^.Algo;
end;

procedure TSHA3.Update(const Buffer: RawByteString);
begin
  if Buffer <> '' then
    PSHA3Context(@Context)^.Absorb(pointer(Buffer), Length(Buffer) shl 3);
end;

procedure TSHA3.Update(Buffer: pointer; Len: integer);
begin
  if Len > 0 then
    PSHA3Context(@Context)^.Absorb(Buffer, Len shl 3);
end;

procedure TSHA3.Final(out Digest: THash256; NoInit: boolean);
begin
  Final(@Digest, 256, NoInit);
end;

procedure TSHA3.Final(out Digest: THash512; NoInit: boolean);
begin
  Final(@Digest, 512, NoInit);
end;

const
  SHA3_DEF_LEN: array[TSHA3Algo] of integer = (224, 256, 384, 512, 256, 512);

procedure TSHA3.Final(Digest: pointer; DigestBits: integer; NoInit: boolean);
begin
  if DigestBits = 0 then
    DigestBits := SHA3_DEF_LEN[TSHA3Context(Context).Algo];
  if TSHA3Context(Context).Squeezing then // used as Extendable-Output Function
    PSHA3Context(@Context)^.Squeeze(Digest, DigestBits)
  else
    PSHA3Context(@Context)^.FinalBit_LSB(0, 0, Digest, DigestBits);
  if not NoInit then
    FillCharFast(Context, sizeof(Context), 0);
end;

function TSHA3.Final256(NoInit: boolean): THash256;
begin
  Final(result,NoInit);
end;

function TSHA3.Final512(NoInit: boolean): THash512;
begin
  Final(result,NoInit);
end;

procedure TSHA3.Full(Buffer: pointer; Len: integer; out Digest: THash256);
begin
  Full(SHA3_256, Buffer, Len, @Digest, 256);
end;

procedure TSHA3.Full(Buffer: pointer; Len: integer; out Digest: THash512);
begin
  Full(SHA3_512, Buffer, Len, @Digest, 512);
end;

procedure TSHA3.Full(Algo: TSHA3Algo; Buffer: pointer; Len: integer;
  Digest: pointer; DigestBits: integer);
begin
  Init(Algo);
  Update(Buffer, Len);
  Final(Digest, DigestBits);
end;

function TSHA3.FullStr(Algo: TSHA3Algo; Buffer: pointer; Len: integer;
  DigestBits: integer): RawUTF8;
var
  tmp: RawByteString;
begin
  if DigestBits = 0 then
    DigestBits := SHA3_DEF_LEN[Algo];
  SetLength(tmp, DigestBits shr 3);
  Full(Algo, Buffer, Len, pointer(tmp), DigestBits);
  result := SynCommons.BinToHex(tmp);
  FillZero(tmp);
end;

procedure TSHA3.Cypher(Key, Source, Dest: pointer; KeyLen, DataLen: integer;
  Algo: TSHA3Algo);
begin
  if DataLen <= 0 then
    exit;
  if Source = Dest then
    raise ESynCrypto.Create('Unexpected TSHA3.Cypher(Source=Dest)');
  Full(Algo, Key, KeyLen, Dest, DataLen shl 3);
  XorMemory(Dest, Source, DataLen); // just as simple as that!
end;

function TSHA3.Cypher(const Key, Source: RawByteString; Algo: TSHA3Algo): RawByteString;
var
  len: integer;
begin
  len := length(Source);
  SetString(result, nil, len);
  Cypher(pointer(Key), pointer(Source), pointer(result), length(Key), len);
end;

procedure TSHA3.InitCypher(Key: pointer; KeyLen: integer; Algo: TSHA3Algo);
begin
  Init(Algo);
  Update(Key, KeyLen);
  PSHA3Context(@Context)^.FinalBit_LSB(0, 0, nil, 0);
end;

procedure TSHA3.InitCypher(const Key: RawByteString; Algo: TSHA3Algo);
begin
  InitCypher(pointer(Key), length(Key), Algo);
end;

procedure TSHA3.Cypher(Source, Dest: pointer; DataLen: integer);
begin
  Final(Dest, DataLen shl 3, true); // in XOF mode
  XorMemory(Dest, Source, DataLen);
end;

function TSHA3.Cypher(const Source: RawByteString): RawByteString;
var
  len: integer;
begin
  len := length(Source);
  SetString(result, nil, len);
  Cypher(pointer(Source), pointer(result), len);
end;

procedure TSHA3.Done;
begin
  FillCharFast(self, sizeof(self), 0);
end;

function SHA3(Algo: TSHA3Algo; const s: RawByteString;
  DigestBits: integer): RawUTF8;
begin
  result := SHA3(algo, pointer(s), length(s), DigestBits);
end;

function SHA3(Algo: TSHA3Algo; Buffer: pointer; Len, DigestBits: integer): RawUTF8;
var
  instance: TSHA3;
begin
  result := instance.FullStr(algo, Buffer, Len, DigestBits);
end;

procedure PBKDF2_SHA3(algo: TSHA3Algo; const password,salt: RawByteString;
  count: Integer; result: PByte; resultbytes: Integer);
var i: integer;
    tmp: RawByteString;
    mac: TSHA3;
    first: TSHA3;
begin
  if resultbytes<=0 then
    resultbytes := SHA3_DEF_LEN[algo] shr 3;
  SetLength(tmp,resultbytes);
  first.Init(algo);
  first.Update(password);
  mac := first;
  mac.Update(salt);
  mac.Final(pointer(tmp),resultbytes shl 3,true);
  MoveFast(pointer(tmp)^,result^,resultbytes);
  for i := 2 to count do begin
    mac := first;
    mac.Update(pointer(tmp),resultbytes);
    mac.Final(pointer(tmp),resultbytes shl 3,true);
    XorMemory(pointer(result),pointer(tmp),resultbytes);
  end;
  FillcharFast(mac,sizeof(mac),0);
  FillcharFast(first,sizeof(first),0);
  FillZero(tmp);
end;

procedure PBKDF2_SHA3_Crypt(algo: TSHA3Algo; const password,salt: RawByteString;
  count: Integer; var data: RawByteString);
var key: RawByteString;
    len: integer;
begin
  len := length(data);
  SetLength(key,len);
  PBKDF2_SHA3(algo,password,salt,count,pointer(key),len);
  XorMemory(pointer(data),pointer(key),len);
  FillZero(key);
end;


{ TSynHasher }

function TSynHasher.Init(aAlgo: THashAlgo): boolean;
begin
  fAlgo := aAlgo;
  result := true;
  case aAlgo of
  hfMD5:      PMD5(@ctxt)^.Init;
  hfSHA1:     PSHA1(@ctxt)^.Init;
  hfSHA256:   PSHA256(@ctxt)^.Init;
  hfSHA384:   PSHA384(@ctxt)^.Init;
  hfSHA512:   PSHA512(@ctxt)^.Init;
  hfSHA3_256: PSHA3(@ctxt)^.Init(SHA3_256);
  hfSHA3_512: PSHA3(@ctxt)^.Init(SHA3_512);
  else result := false;
  end;
end;

procedure TSynHasher.Update(aBuffer: Pointer; aLen: integer);
begin
  case fAlgo of
  hfMD5:      PMD5(@ctxt)^.Update(aBuffer^,aLen);
  hfSHA1:     PSHA1(@ctxt)^.Update(aBuffer,aLen);
  hfSHA256:   PSHA256(@ctxt)^.Update(aBuffer,aLen);
  hfSHA384:   PSHA384(@ctxt)^.Update(aBuffer,aLen);
  hfSHA512:   PSHA512(@ctxt)^.Update(aBuffer,aLen);
  hfSHA3_256: PSHA3(@ctxt)^.Update(aBuffer,aLen);
  hfSHA3_512: PSHA3(@ctxt)^.Update(aBuffer,aLen);
  end;
end;

procedure TSynHasher.Update(const aBuffer: RawByteString);
begin
  Update(pointer(aBuffer),length(aBuffer));
end;

function TSynHasher.Final: RawUTF8;
begin
  case fAlgo of
  hfMD5:      result := MD5DigestToString(PMD5(@ctxt)^.Final);
  hfSHA1:     result := SHA1DigestToString(PSHA1(@ctxt)^.Final);
  hfSHA256:   result := SHA256DigestToString(PSHA256(@ctxt)^.Final);
  hfSHA384:   result := SHA384DigestToString(PSHA384(@ctxt)^.Final);
  hfSHA512:   result := SHA512DigestToString(PSHA512(@ctxt)^.Final);
  hfSHA3_256: result := SHA256DigestToString(PSHA3(@ctxt)^.Final256);
  hfSHA3_512: result := SHA512DigestToString(PSHA3(@ctxt)^.Final512);
  end;
end;

function TSynHasher.Full(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUTF8;
begin
  Init(aAlgo);
  Update(aBuffer,aLen);
  result := Final;
end;

function HashFull(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUTF8;
var hasher: TSynHasher;
begin
  result := hasher.Full(aAlgo,aBuffer,aLen);
end;

function HashFile(const aFileName: TFileName; aAlgo: THashAlgo): RawUTF8;
var
  hasher: TSynHasher;
  temp: RawByteString;
  F: THandle;
  size: TQWordRec;
  read: cardinal;
begin
  result := '';
  if (aFileName='') or not hasher.Init(aAlgo) then
    exit;
  F := FileOpenSequentialRead(aFileName);
  if PtrInt(F)>=0 then
    try
      size.L := GetFileSize(F,@size.H);
      SetLength(temp,1 shl 20);
      while size.V>0 do begin
        read := FileRead(F,pointer(temp)^,1 shl 20);
        if read<=0 then
          exit;
        hasher.Update(pointer(temp),read);
        dec(size.V,read);
      end;
      result := hasher.Final;
    finally
      FileClose(F);
    end;
end;

procedure HashFile(const aFileName: TFileName; aAlgos: THashAlgos);
var data, hash: RawUTF8;
    efn, fn: string;
    a: THashAlgo;
begin
  if aAlgos=[] then
    exit;
  efn := ExtractFileName(aFileName);
  data := StringFromFile(aFileName);
  if data<>'' then
    for a := low(a) to high(a) do
      if a in aAlgos then begin
        FormatUTF8('% *%',[HashFull(a,pointer(data),length(data)),efn],hash);
        FormatString('%.%',[efn,LowerCase(TrimLeftLowerCaseShort(ToText(a)))],fn);
        FileFromString(hash,fn);
      end;
end;


{ TSynSigner }

procedure TSynSigner.Init(aAlgo: TSignAlgo; aSecret: pointer; aSecretLen: integer);
const
  SIGN_SIZE: array[TSignAlgo] of byte = (
    20, 32, 48, 64, 28, 32, 48, 64, 32, 64);
  SHA3_ALGO: array[saSha3224..saSha3S256] of TSHA3Algo = (
    SHA3_224, SHA3_256, SHA3_384, SHA3_512, SHAKE_128, SHAKE_256);
begin
  fAlgo := aAlgo;
  fSignatureSize := SIGN_SIZE[fAlgo];
  case fAlgo of
  saSha1:   PHMAC_SHA1(@ctxt)^.Init(aSecret,aSecretLen);
  saSha256: PHMAC_SHA256(@ctxt)^.Init(aSecret,aSecretLen);
  saSha384: PHMAC_SHA384(@ctxt)^.Init(aSecret,aSecretLen);
  saSha512: PHMAC_SHA512(@ctxt)^.Init(aSecret,aSecretLen);
  saSha3224..saSha3S256: begin
    PSHA3(@ctxt)^.Init(SHA3_ALGO[fAlgo]);
    PSHA3(@ctxt)^.Update(aSecret,aSecretLen); // HMAC pattern included in SHA-3
  end;
  end;
end;

procedure TSynSigner.Init(aAlgo: TSignAlgo; const aSecret: RawUTF8);
begin
  Init(aAlgo,pointer(aSecret),length(aSecret));
end;

procedure TSynSigner.Init(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
  aSecretPBKDF2Rounds: integer; aPBKDF2Secret: PHash512Rec);
var temp: THash512Rec;
begin
  if aSecretPBKDF2Rounds>1 then begin
    PBKDF2(aAlgo,aSecret,aSalt,aSecretPBKDF2Rounds,temp);
    Init(aAlgo,@temp,fSignatureSize);
    if aPBKDF2Secret<>nil then
      aPBKDF2Secret^ := temp;
    FillZero(temp.b);
  end else
    Init(aAlgo,aSecret);
end;

procedure TSynSigner.Update(const aBuffer: RawByteString);
begin
  Update(pointer(aBuffer),length(aBuffer));
end;

procedure TSynSigner.Update(aBuffer: pointer; aLen: integer);
begin
  case fAlgo of
  saSha1:   PHMAC_SHA1(@ctxt)^.Update(aBuffer,aLen);
  saSha256: PHMAC_SHA256(@ctxt)^.Update(aBuffer,aLen);
  saSha384: PHMAC_SHA384(@ctxt)^.Update(aBuffer,aLen);
  saSha512: PHMAC_SHA512(@ctxt)^.Update(aBuffer,aLen);
  saSha3224..saSha3S256: PSHA3(@ctxt)^.Update(aBuffer,aLen);
  end;
end;

procedure TSynSigner.Final(out aSignature: THash512Rec; aNoInit: boolean);
begin
  case fAlgo of
  saSha1:   PHMAC_SHA1(@ctxt)^.Done(aSignature.b160,aNoInit);
  saSha256: PHMAC_SHA256(@ctxt)^.Done(aSignature.Lo,aNoInit);
  saSha384: PHMAC_SHA384(@ctxt)^.Done(aSignature.b384,aNoInit);
  saSha512: PHMAC_SHA512(@ctxt)^.Done(aSignature.b,aNoInit);
  saSha3224..saSha3S256: PSHA3(@ctxt)^.Final(@aSignature,fSignatureSize shl 3,aNoInit);
  end;
end;

function TSynSigner.Final: RawUTF8;
var sig: THash512Rec;
begin
  Final(sig);
  result := BinToHexLower(@sig,fSignatureSize);
end;

function TSynSigner.Full(aAlgo: TSignAlgo; const aSecret: RawUTF8;
  aBuffer: Pointer; aLen: integer): RawUTF8;
begin
  Init(aAlgo,aSecret);
  Update(aBuffer,aLen);
  result := Final;
end;

function TSynSigner.Full(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
  aSecretPBKDF2Rounds: integer; aBuffer: Pointer; aLen: integer): RawUTF8;
begin
  Init(aAlgo,aSecret,aSalt,aSecretPBKDF2Rounds);
  Update(aBuffer,aLen);
  result := Final;
end;

procedure TSynSigner.PBKDF2(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
  aSecretPBKDF2Rounds: integer; out aDerivatedKey: THash512Rec);
var iter: TSynSigner;
    temp: THash512Rec;
    i: integer;
begin
  Init(aAlgo,aSecret);
  iter := self;
  iter.Update(aSalt);
  if fAlgo<saSha3224 then
    iter.Update(#0#0#0#1); // padding and XoF mode already part of SHA-3 process
  iter.Final(aDerivatedKey,true);
  if aSecretPBKDF2Rounds<2 then
    exit;
  temp := aDerivatedKey;
  for i := 2 to aSecretPBKDF2Rounds do begin
    iter := self;
    iter.Update(@temp,fSignatureSize);
    iter.Final(temp,true);
    XorMemory(@aDerivatedKey,@temp,fSignatureSize);
  end;
  FillZero(temp.b);
  FillCharFast(iter.ctxt,SizeOf(iter.ctxt),0);
  FillCharFast(ctxt,SizeOf(ctxt),0);
end;

procedure TSynSigner.PBKDF2(const aParams: TSynSignerParams;
  out aDerivatedKey: THash512Rec);
begin
  PBKDF2(aParams.algo,aParams.secret,aParams.salt,aParams.rounds,aDerivatedKey);
end;

procedure TSynSigner.PBKDF2(aParamsJSON: PUTF8Char; aParamsJSONLen: integer;
  out aDerivatedKey: THash512Rec; const aDefaultSalt: RawUTF8; aDefaultAlgo: TSignAlgo);
var tmp: TSynTempBuffer;
    k: TSynSignerParams;
  procedure SetDefault;
  begin
    k.algo := aDefaultAlgo;
    k.secret := '';
    k.salt := aDefaultSalt;
    k.rounds := 1000;
  end;
begin
  SetDefault;
  if (aParamsJSON=nil) or (aParamsJSONLen<=0) then
    k.secret := aDefaultSalt else
    if aParamsJSON[1]<>'{' then
      FastSetString(k.secret,aParamsJSON,aParamsJSONLen) else begin
    tmp.Init(aParamsJSON,aParamsJSONLen);
    try
      if (RecordLoadJSON(k,tmp.buf,TypeInfo(TSynSignerParams))=nil) or
         (k.secret='') or (k.salt='') then begin
        SetDefault;
        FastSetString(k.secret,aParamsJSON,aParamsJSONLen);
      end;
    finally
      FillCharFast(tmp.buf^,tmp.len,0);
      tmp.Done;
    end;
  end;
  PBKDF2(k.algo,k.secret,k.salt,k.rounds,aDerivatedKey);
  FillZero(k.secret);
end;

procedure TSynSigner.PBKDF2(const aParamsJSON: RawUTF8; out aDerivatedKey: THash512Rec;
    const aDefaultSalt: RawUTF8; aDefaultAlgo: TSignAlgo);
begin
  PBKDF2(pointer(aParamsJSON),length(aParamsJSON),aDerivatedKey,aDefaultSalt,aDefaultAlgo);
end;

procedure TSynSigner.AssignTo(var aDerivatedKey: THash512Rec; out aAES: TAES; aEncrypt: boolean);
var ks: integer;
begin
  case Algo of
  saSha3S128: ks := 128; // truncate to Keccak sponge precision
  saSha3S256: ks := 256;
  else
    case SignatureSize of
    20: begin
      ks := 128;
      aDerivatedKey.i0 := aDerivatedKey.i0 xor aDerivatedKey.i4;
    end;
    28: ks := 192;
    32: ks := 256;
    48: begin
      ks := 256;
      aDerivatedKey.d0 := aDerivatedKey.d0 xor aDerivatedKey.d4;
      aDerivatedKey.d1 := aDerivatedKey.d1 xor aDerivatedKey.d5;
    end;
    64: begin
      ks := 256;
      aDerivatedKey.d0 := aDerivatedKey.d0 xor aDerivatedKey.d4;
      aDerivatedKey.d1 := aDerivatedKey.d1 xor aDerivatedKey.d5;
      aDerivatedKey.d2 := aDerivatedKey.d0 xor aDerivatedKey.d6;
      aDerivatedKey.d3 := aDerivatedKey.d1 xor aDerivatedKey.d7;
    end;
    else exit;
    end;
  end;
  aAES.DoInit(aDerivatedKey,ks,aEncrypt);
  FillZero(aDerivatedKey.b);
end;

procedure TSynSigner.Done;
begin
  FillCharFast(self, SizeOf(self), 0);
end;

procedure AES(const Key; KeySize: cardinal; buffer: pointer; Len: Integer; Encrypt: boolean);
begin
  AES(Key,KeySize,buffer,buffer,Len,Encrypt);
end;

procedure AES(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: Integer; Encrypt: boolean);
var n: integer;
    pIn, pOut: PAESBlock;
    Crypt: TAES;
begin
  if (bIn=nil) or (bOut=nil) then exit;
  // 1. Init
  n := Len shr AESBlockShift;
  if n<0 then exit else
  if n>0 then
    if (KeySize>4) and not Crypt.DoInit(Key,KeySize,Encrypt) then
      KeySize := 4; // if error in KeySize, use default fast XorOffset()
  if KeySize=0 then begin // KeySize=0 -> no encryption -> direct copy
    MoveFast(bIn^, bOut^, Len);
    exit;
  end;
  if n<1 then begin // too small for AES -> XorOffset() remaining 0..15 bytes
    MoveFast(bIn^, bOut^, Len);
    XorOffset(bOut,0,Len);
    exit;
  end;
  // 2. All full blocks, with AES
{$ifdef USETHREADSFORBIGAESBLOCKS}
  pIn := bIn;
  pOut := bOut;
  Crypt.DoBlocksThread(pIn,pOut,n,Encrypt);
{$else}
  Crypt.DoBlocks(bIn,bOut,pIn,pOut,n,Encrypt);
{$endif}
  // 3. Last block, just XORed from Key
//  assert(KeySize div 8>=AESBlockSize);
  n := cardinal(Len) and AESBlockMod;
  MoveFast(pIn^,pOut^,n); // pIn=pOut is tested in MoveFast()
  XorOffset(pointer(pOut),Len-n,n);
  Crypt.Done;
end;

const TmpSize = 65536;
  // Tmp buffer for AESFull -> Xor Crypt is TmpSize-dependent / use XorBlock()
      TmpSizeBlock = TmpSize shr AESBlockShift;
type
  TTmp = array[0..TmpSizeBlock-1] of TAESBlock;

function AES(const Key; KeySize: cardinal; const s: RawByteString; Encrypt: boolean): RawByteString;
begin
  SetString(result,nil,length(s));
  if s<>'' then
    AES(Key,KeySize,pointer(s),pointer(result),length(s),Encrypt);
end;

function AES(const Key; KeySize: cardinal; buffer: pointer; Len: cardinal; Stream: TStream; Encrypt: boolean): boolean; overload;
var buf: pointer;
    last, b, n, i: cardinal;
    Crypt: TAES;
begin
  result := false;
  if buffer=nil then exit;
  if (KeySize>4) and not Crypt.DoInit(Key,KeySize,Encrypt) then
    KeySize := 4; // if error in KeySize, use default fast XorOffset()
  if KeySize=0 then begin // no Crypt -> direct write to dest Stream
    Stream.WriteBuffer(buffer^,Len);
    result := true;
    exit;
  end;
  getmem(buf,TmpSize);
  try
    Last := Len and AESBlockMod;
    n := Len-Last;
    i := 0;
    while n>0 do begin // crypt/uncrypt all AESBlocks
      if n>TmpSize then
        b := TmpSize else
        b := n;
      assert(b and AESBlockMod=0);
      if KeySize=4 then begin
        MoveFast(buffer^,buf^,b);
        XorOffset(pointer(buf),i,b);
        inc(i,b);
      end else
        Crypt.DoBlocks(buffer,buf,b shr AESBlockShift,Encrypt);
      Stream.WriteBuffer(buf^,b);
      inc(PByte(buffer),b);
      dec(n,b);
    end;
    assert((KeySize>4)or(i=Len-Last));
    if last>0 then begin // crypt/uncrypt (Xor) last 0..15 bytes
      MoveFast(buffer^,buf^,Last);
      XorOffset(pointer(buf),Len-Last,Last);
      Stream.WriteBuffer(buf^,Last);
    end;
    result := true;
  finally
    freemem(buf);
  end;
end;

function KeyFrom(const Key; KeySize: cardinal): cardinal;
begin
  case KeySize div 8 of
  0:   result := 0;
  1:   result := PByte(@Key)^;
  2,3: result := PWord(@Key)^;
  else result := PInteger(@Key)^;
  end;
end;

function TAESFullHeader.Calc(const Key; KeySize: cardinal): cardinal;
begin
  result := Adler32Asm(KeySize,@Key,KeySize shr 3) xor Te0[OriginalLen and $FF]
    xor Te1[SourceLen and $FF] xor Td0[SomeSalt and $7FF];
end;

function TAESFull.EncodeDecode(const Key; KeySize, inLen: cardinal; Encrypt: boolean;
  inStream, outStream: TStream; bIn, bOut: pointer; OriginalLen: Cardinal=0): integer;
var Tmp: ^TTmp;
    pIn, pOut: PAESBlock;
    Crypt: TAES;
    nBlock,
    XorCod: cardinal;
procedure Read(Tmp: pointer; ByteCount: cardinal);
begin
  if pIn=nil then
    InStream.Read(Tmp^,ByteCount) else begin
    MoveFast(pIn^,Tmp^,ByteCount);
    inc(PByte(pIn),ByteCount);
  end;
end;
procedure Write(Tmp: pointer; ByteCount: cardinal);
begin
  if pOut=nil then
    OutStream.WriteBuffer(Tmp^,ByteCount) else begin
    MoveFast(Tmp^,pOut^,ByteCount);
    inc(PByte(pOut),ByteCount);
  end;
end;
procedure SetOutLen(Len: cardinal);
var P: cardinal;
begin
  result := Len; // global EncodeDecode() result
  if OutStream<>nil then begin
    if OutStream.InheritsFrom(TMemoryStream) then
      with TMemoryStream(OutStream) do begin
        P := Seek(0,soFromCurrent);
        Size := P+Len; // auto-reserve space (no Realloc:)
        Seek(P+Len,soBeginning);
        bOut := PAnsiChar(Memory)+P;
        pOut := bOut;
        OutStream := nil; //  OutStream is slower and use no thread
      end;
  end else
  if bOut=nil then begin
    outStreamCreated := THeapMemoryStream.Create; // faster than TMemoryStream
    outStreamCreated.Size := Len; // auto-reserve space (no Realloc:)
    bOut := outStreamCreated.Memory;
    pOut := bOut; // OutStream is slower and use no thread
  end;
  if KeySize=0 then exit; // no Tmp to be allocated on direct copy
{$ifdef USEPADLOCK} // PADLOCK prefers 16-bytes alignment
  if (KeySize=32) or (InStream<>nil) or (OutStream<>nil) or
     (PtrUInt(bIn) and $f<>0) or (PtrUInt(bOut) and $f<>0) then begin
    New(Tmp);
//    assert(PtrUInt(Tmp) and $F=0);
  end;
{$else}
  if (KeySize=32) or (InStream<>nil) or (OutStream<>nil) then
    New(Tmp);
{$endif}
end;
procedure DoBlock(BlockCount: integer);
begin
  if BlockCount=0 then
    exit;
  Read(Tmp,BlockCount shl AESBlockShift);
  Crypt.DoBlocks(PAESBLock(Tmp),PAESBLock(Tmp),BlockCount,Encrypt);
  Write(Tmp,BlockCount shl AESBlockShift);
end;
var n, LastLen: cardinal;
    i: integer;
    Last: TAESBlock;
begin
  result := 0; // makes FixInsight happy
  Tmp := nil;
  outStreamCreated := nil;
  Head.SourceLen := InLen;
  nBlock := Head.SourceLen shr AESBlockShift;
  if Encrypt and (OriginalLen<>0) then
    Head.OriginalLen := OriginalLen else
    Head.OriginalLen := InLen;
  KeySize := KeySize div 8;
  if not (KeySize in [0,4,16,24,32]) then
    KeySize := 0 else  // valid KeySize: 0=nothing, 32=xor, 128,192,256=AES
    KeySize := KeySize*8;
  XorCod := inLen;
  if (inStream<>nil) and inStream.InheritsFrom(TMemoryStream) then begin
    bIn := TMemoryStream(inStream).Memory;
    inStream := nil;
   end;
  pIn := bIn;
  pOut := bOut;
  if (KeySize>=128) and not Crypt.DoInit(Key,KeySize,Encrypt) then
    KeySize := 32;
  if KeySize=32 then
     XorCod := KeyFrom(Key,KeySize) xor XorCod else
  if (KeySize=0) and (InStream=nil) then begin
    SetOutLen(inLen);
    Write(bIn,inLen);  // no encryption -> direct write
    exit;
  end;
  try
    // 0. KeySize = 0:direct copy 32:XorBlock
    if KeySize<128 then begin
      SetOutLen(inLen);
      assert(Tmp<>nil);
      LastLen := inLen;
      while LastLen<>0 do begin
        if LastLen>TmpSize then
          n := TmpSize else
          n := LastLen;
        Read(Tmp,n);
        if KeySize>0 then
          XorBlock(pointer(Tmp),n,XorCod);
        Write(Tmp,n);
        dec(LastLen,n);
      end;
    end else begin // now we do AES encryption:
      // 1. Header process
      if Encrypt then begin
        // encrypt data
        if (pIn=pOut) and (pIn<>nil) then begin
          assert(false); // Head in pOut^ will overflow data in pIn^
          result := 0;
          exit;
        end;
        LastLen := inLen and AESBlockMod;
        if LastLen=0 then
          SetOutLen(inLen+sizeof(TAESBlock)) else
          SetOutLen((nBlock+2)shl AESBlockShift);
        Head.SomeSalt := random(MaxInt);
        Head.HeaderCheck := Head.Calc(Key,KeySize);
        Crypt.Encrypt(TAESBlock(Head));
        Write(@Head,sizeof(Head));
      end else begin
        // uncrypt data
        dec(nBlock); // Header is already done
        Read(@Head,sizeof(Head));
        Crypt.Decrypt(TAESBlock(Head));
        with Head do begin
          if HeaderCheck<>Head.Calc(Key,KeySize) then begin
            result := -1;
            exit; // wrong key
          end;
          SetOutLen(SourceLen);
          LastLen := SourceLen and AESBlockMod;
        end;
        if LastLen<>0 then
          dec(nBlock); // the very last block is for the very last bytes
      end;
      // 2. All full blocks, with AES
      if Tmp=nil then begin
      {$ifdef USETHREADSFORBIGAESBLOCKS} // Tmp is 64KB -> helpless Threads
        Crypt.DoBlocksThread(pIn,pOut,nBlock,Encrypt);
      {$else}
        Crypt.DoBlocks(pIn,pOut,pIn,pOut,nBlock,Encrypt);
      {$endif}
      end else begin
        for i := 1 to nBlock div TmpSizeBlock do
          DoBlock(TmpSizeBlock);
        DoBlock(nBlock mod TmpSizeBlock);
      end;
      // 3. Last block
      if LastLen<>0 then
      if Encrypt then begin
        FillcharFast(Last,sizeof(TAESBlock),0);
        Read(@Last,LastLen);
        Crypt.Encrypt(Last);
        Write(@Last,sizeof(TAESBlock));
      end else begin
        Read(@Last,sizeof(TAESBlock));
        Crypt.Decrypt(Last);
        Write(@Last,LastLen);
      end;
      Crypt.Done;
    end;
  finally
    if Tmp<>nil then
      Freemem(Tmp);
  end;
end;


function AESFullKeyOK(const Key; KeySize: cardinal; buff: pointer): boolean;
// true if begining of buff contains true AESFull encrypted data with this Key
var Crypt: TAES;
    Head: TAESFullHeader;
begin
  if KeySize<128 then
    result := true else
  if not Crypt.DecryptInit(Key,KeySize) then
    result := false else begin
    Crypt.Decrypt(PAESBlock(buff)^,TAESBlock(Head));
    result := Head.Calc(Key,KeySize)=Head.HeaderCheck;
    Crypt.Done;
  end;
end;

function AESFull(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: integer;
  Encrypt: boolean; OriginalLen: Cardinal=0): integer; overload;
// bOut must be at least bIn+32/Encrypt bIn-16/Decrypt -> returns outLength, <0 if error
var A: TAESFull;
begin
  result := A.EncodeDecode(Key,KeySize,Len,Encrypt,nil,nil,bIn,bOut,OriginalLen);
end;

function AESFull(const Key; KeySize: cardinal; bIn: pointer; Len: Integer;
   outStream: TStream; Encrypt: boolean; OriginalLen: Cardinal=0): boolean; // true is Key OK
// outStream will be larger/smaller than Len: this is a full AES version
// if not KeySize in [128,192,256] -> use very fast and Simple Xor Cypher
var A: TAESFull;
begin
  result := A.EncodeDecode(Key,KeySize,
    Len,Encrypt,nil,outStream,bIn,nil,OriginalLen)>=0;
end;

procedure AESSHA256(bIn, bOut: pointer; Len: integer; const Password: RawByteString; Encrypt: boolean);
var Digest: TSHA256Digest;
begin
  SHA256Weak(Password,Digest);
  AES(Digest,sizeof(Digest)*8,bIn,bOut,Len,Encrypt);
  FillZero(Digest);
end;

function AESSHA256(const s, Password: RawByteString; Encrypt: boolean): RawByteString;
begin
  SetString(result,nil,length(s));
  AESSHA256(pointer(s),pointer(result),length(s),Password,Encrypt);
end;

procedure AESSHA256(Buffer: pointer; Len: integer; const Password: RawByteString; Encrypt: boolean);
// Encrypt/Decrypt Buffer with AES and SHA-256 password
begin
  AESSHA256(Buffer,Buffer,Len,Password,Encrypt);
end;

procedure AESSHA256Full(bIn: pointer; Len: Integer; outStream: TStream; const Password: RawByteString; Encrypt: boolean);
// outStream will be larger/smaller than Len: this is a full AES version
var Digest: TSHA256Digest;
begin
  SHA256Weak(Password,Digest);
  AESFull(Digest,sizeof(Digest)*8,bIn,Len,outStream,Encrypt);
end;


function Adler32Pas(Adler: cardinal; p: pointer; Count: Integer): cardinal;
// simple Adler32 implementation (twice slower than Asm, but shorter code size)
var s1, s2: cardinal;
    i, n: integer;
begin
  s1 := LongRec(Adler).Lo;
  s2 := LongRec(Adler).Hi;
  while Count>0 do begin
    if Count<5552 then
      n := Count else
      n := 5552;
    for i := 1 to n do begin
      inc(s1,PByte(p)^);
      inc(PByte(p));
      inc(s2,s1);
    end;
    s1 := s1 mod 65521;
    s2 := s2 mod 65521;
    dec(Count,n);
  end;
  result := (s1 and $ffff)+(s2 and $ffff) shl 16;
end;

function Adler32Asm(Adler: cardinal; p: pointer; Count: Integer): cardinal;
{$ifdef PUREPASCAL}
begin
  result := Adler32Pas(Adler,p,Count);
end;
{$else} {$ifdef FPC} nostackframe; assembler; {$endif}
asm
        push    ebx
        push    esi
        push    edi
        mov     edi, eax
        shr     edi, 16
        movzx   ebx, ax
        push    ebp
        mov     esi, edx
        test    esi, esi
        mov     ebp, ecx
        jne     @31
        mov     eax, 1
        jmp     @32
@31:    test    ebp, ebp
        jbe     @34
@33:    cmp     ebp, 5552
        jae     @35
        mov     eax, ebp
        jmp     @36
@35:    mov     eax, 5552
@36:    sub     ebp, eax
        cmp     eax, 16
        jl      @38
        xor     edx, edx
        xor     ecx, ecx
@39:    sub     eax, 16
        mov     dl, [esi]
        mov     cl, [esi + 1]
        add     ebx, edx
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 2]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 3]
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 4]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 5]
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 6]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 7]
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 8]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 9]
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 10]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 11]
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 12]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 13]
        add     edi, ebx
        add     ebx, ecx
        mov     dl, [esi + 14]
        add     edi, ebx
        add     ebx, edx
        mov     cl, [esi + 15]
        add     edi, ebx
        add     ebx, ecx
        add     esi, 16
        lea     edi, [edi + ebx]
        cmp     eax, 16
        jge     @39
@38:    test    eax, eax
        je      @42
@43:    movzx   edx, byte ptr[esi]
        add     ebx, edx
        dec     eax
        lea     esi, [esi + 1]
        lea     edi, [edi + ebx]
        jg      @43
@42:    mov     ecx, 65521
        mov     eax, ebx
        xor     edx, edx
        div     ecx
        mov     ebx, edx
        mov     ecx, 65521
        mov     eax, edi
        xor     edx, edx
        div     ecx
        test    ebp, ebp
        mov     edi, edx
        ja      @33
@34:    mov     eax, edi
        shl     eax, 16
        or      eax, ebx
@32:    pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;
{$endif}

function Adler32SelfTest: boolean;
begin
  result :=
  {$ifndef PUREPASCAL}
    (Adler32Asm(1,@Te0,sizeof(Te0))=$BCBEFE10) and
    (Adler32Asm(7,@Te1,sizeof(Te1)-3)=$DA91FDBE) and
  {$endif}
    (Adler32Pas(1,@Te0,sizeof(Te0))=$BCBEFE10) and
    (Adler32Pas(7,@Te1,sizeof(Te1)-3)=$DA91FDBE);
end;


{ TAESWriteStream }

constructor TAESWriteStream.Create(outStream: TStream; const Key; KeySize: cardinal);
begin
  inherited Create;
  if KeySize=0 then
    NoCrypt := true else
    AES.EncryptInit(Key,KeySize);
  Dest := outStream;
end;

destructor TAESWriteStream.Destroy;
begin
  Finish;
  AES.Done;
  inherited;
end;

procedure TAESWriteStream.Finish;
begin
  if BufCount=0 then exit;
  assert((BufCount<sizeof(TAESBlock)) and AES.Initialized and not NoCrypt);
  XorOffset(@Buf,DestSize,BufCount);
  Dest.WriteBuffer(Buf,BufCount);
  BufCount := 0;
end;

function TAESWriteStream.Read(var Buffer; Count: Integer): Longint;
begin
  raise ESynCrypto.CreateUTF8('Unexpected %.Read',[self]);
end;

function TAESWriteStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  raise ESynCrypto.CreateUTF8('Unexpected %.Seek',[self]);
end;

function TAESWriteStream.Write(const Buffer; Count: Integer): Longint;
// most of the time, a 64KB-buffered compressor have BufCount=0
// will crypt 'const Buffer' memory in place -> use AFTER T*Compressor
var B: TByteArray absolute Buffer;
    Len: integer;
begin
  result := Count;
  Adler := Adler32Asm(Adler,@Buffer,Count);
  if not NoCrypt then // KeySize=0 -> save as-is
  if not AES.Initialized then // if error in KeySize -> default fast XorOffset()
    XorOffset(@B,DestSize,Count) else begin
    if BufCount>0 then begin
      Len := sizeof(TAESBlock)-BufCount;
      if Len>Count then
        Len := Count;
      MoveFast(Buffer,Buf[BufCount],Len);
      inc(BufCount,Len);
      if BufCount<sizeof(TAESBlock) then
        exit;
      AES.Encrypt(Buf);
      Dest.WriteBuffer(Buf,sizeof(TAESBlock));
      inc(DestSize,sizeof(TAESBlock));
      Dec(Count,Len);
      AES.DoBlocks(@B[Len],@B[Len],cardinal(Count) shr AESBlockShift,true);
    end else
      AES.DoBlocks(@B,@B,cardinal(Count) shr AESBlockShift,true);
    BufCount := cardinal(Count) and AESBlockMod;
    if BufCount<>0 then begin
      dec(Count,BufCount);
      MoveFast(B[Count],Buf[0],BufCount);
    end;
  end;
  Dest.WriteBuffer(Buffer,Count);
  inc(DestSize,Count);
end;

procedure XorBlock(p: PIntegerArray; Count, Cod: integer);
// very fast Xor() according to Cod - not Compression or Stream compatible
var i: integer;
begin
  for i := 1 to Count shr 4 do begin // proceed through 16 bytes blocs
    Cod := (Cod shl 11) xor integer(Td0[cod shr 21]); // shr 21 -> 8*[byte] of cardinal
    p^[0] := p^[0] xor Cod;
    p^[1] := p^[1] xor Cod;
    p^[2] := p^[2] xor Cod;
    p^[3] := p^[3] xor Cod;
    inc(PByte(p),16);
  end;
  Cod := (Cod shl 11) xor integer(Td0[cod shr 21]);
  for i := 1 to (Count and AESBlockMod)shr 2 do begin // last 4 bytes blocs
    p^[0] := p^[0] xor Cod;
    inc(PByte(p),4);
  end;
  for i := 1 to Count and 3 do begin
    PByte(p)^ := PByte(p)^ xor byte(Cod);
    inc(PByte(p));
  end;
end;

procedure XorOffset(P: PByteArray; Index,Count: integer);
// XorOffset: fast and simple Cypher using Index (=Position in Dest Stream):
// Compression not OK -> apply after compress (e.g. TBZCompressor.withXor=true)
var Len: integer;
begin
  if Count>0 then
  repeat
    Index := Index and $1FFF;
    Len := $2000-Index;
    if Len>Count then
      Len := Count;
    XorMemory(P,@Xor32Byte[Index],Len);
    inc(P,Len);
    inc(Index,Len);
    Dec(Count,Len);
  until Count=0;
end;


procedure XorConst(P: PIntegerArray; Count: integer);
// XorConst: fast Cypher changing by Count value
// (compression OK):
var i: integer;
    Code: integer;
begin // 1 to 3 bytes may stay unencrypted: not relevant
  Code := integer(Td0[Count and $3FF]);
  for i := 1 to (Count shr 4) do begin
     P^[0] := P^[0] xor Code;
     P^[1] := P^[1] xor Code;
     P^[2] := P^[2] xor Code;
     P^[3] := P^[3] xor Code;
     inc(PByte(P),16);
  end;
  for i := 0 to ((Count and AESBlockMod)shr 2)-1 do // last 4 bytes blocs
    P^[i] := P^[i] xor Code;
end;


{ TMD5 }

procedure MD5Transform(var buf: TMD5Buf; const in_: TMD5In);
// see https://synopse.info/forum/viewtopic.php?id=4369 for asm numbers
{$ifdef CPUX64}
{
 MD5_Transform-x64
 MD5 transform routine optimized for x64 processors
 Copyright 2018 Ritlabs, SRL
 The 64-bit version is written by Maxim Masiutin <max@ritlabs.com>

 The main advantage of this 64-bit version is that it loads 64 bytes of hashed
 message into 8 64-bit registers (RBP, R8, R9, R10, R11, R12, R13, R14) at the
 beginning, to avoid excessive memory load operations througout the routine.

 MD5_Transform-x64 is released under a dual license, and you may choose to use
 it under either the Mozilla Public License 2.0 (MPL 2.1, available from
 https://www.mozilla.org/en-US/MPL/2.0/) or the GNU Lesser General Public
 License Version 3, dated 29 June 2007 (LGPL 3, available from
 https://www.gnu.org/licenses/lgpl.html).

 MD5_Transform-x64 is based on Peter Sawatzki's code.
 Taken from https://github.com/maximmasiutin/MD5_Transform-x64
}
{$ifdef FPC}nostackframe; assembler; asm{$else}
asm // W=rcx Buf=rdx
        .noframe
{$endif}
        {$ifndef win64}
        mov     rdx, rsi
        mov     rcx, rdi
        {$endif win64}
        push    rbx
        push    rsi
        push    rdi
        push    rbp
        push    r12
        push    r13
        push    r14
        mov     r14, rdx
        mov     rsi, rcx
        push    rsi
        mov     eax, dword ptr [rsi]
        mov     ebx, dword ptr [rsi+4H]
        mov     ecx, dword ptr [rsi+8H]
        mov     edx, dword ptr [rsi+0CH]
        mov     rbp, qword ptr [r14]
        add     eax, -680876936
        add     eax, ebp
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        ror     rbp, 32
        add     edx, -389564586
        add     edx, ebp
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        mov     r8, qword ptr [r14+8H]
        add     ecx, 606105819
        add     ecx, r8d
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        ror     r8, 32
        add     ebx, -1044525330
        add     ebx, r8d
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        mov     r9, qword ptr [r14+10H]
        add     eax, -176418897
        add     eax, r9d
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        ror     r9, 32
        add     edx, 1200080426
        add     edx, r9d
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        mov     r10, qword ptr [r14+18H]
        add     ecx, -1473231341
        add     ecx, r10d
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        ror     r10, 32
        add     ebx, -45705983
        add     ebx, r10d
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        mov     r11, qword ptr [r14+20H]
        add     eax, 1770035416
        add     eax, r11d
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        ror     r11, 32
        add     edx, -1958414417
        add     edx, r11d
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        mov     r12, qword ptr [r14+28H]
        add     ecx, -42063
        add     ecx, r12d
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        ror     r12, 32
        add     ebx, -1990404162
        add     ebx, r12d
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        mov     r13, qword ptr [r14+30H]
        add     eax, 1804603682
        add     eax, r13d
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        ror     r13, 32
        add     edx, -40341101
        add     edx, r13d
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        mov     r14, qword ptr [r14+38H]
        add     ecx, -1502002290
        add     ecx, r14d
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        ror     r14, 32
        add     ebx, 1236535329
        add     ebx, r14d
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        add     eax, -165796510
        add     eax, ebp
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        ror     r10, 32
        add     edx, -1069501632
        add     edx, r10d
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, 643717713
        add     ecx, r12d
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        ror     rbp, 32
        add     ebx, -373897302
        add     ebx, ebp
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, -701558691
        add     eax, r9d
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        ror     r12, 32
        add     edx, 38016083
        add     edx, r12d
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, -660478335
        add     ecx, r14d
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        ror     r9, 32
        add     ebx, -405537848
        add     ebx, r9d
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, 568446438
        add     eax, r11d
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        ror     r14, 32
        add     edx, -1019803690
        add     edx, r14d
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, -187363961
        add     ecx, r8d
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        ror     r11, 32
        add     ebx, 1163531501
        add     ebx, r11d
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, -1444681467
        add     eax, r13d
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        ror     r8, 32
        add     edx, -51403784
        add     edx, r8d
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        ror     r10, 32
        add     ecx, 1735328473
        add     ecx, r10d
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        ror     r13, 32
        add     ebx, -1926607734
        add     ebx, r13d
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        ror     r9, 32
        add     eax, -378558
        add     eax, r9d
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        add     edx, -2022574463
        add     edx, r11d
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        ror     r12, 32
        add     ecx, 1839030562
        add     ecx, r12d
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        add     ebx, -35309556
        add     ebx, r14d
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        ror     rbp, 32
        add     eax, -1530992060
        add     eax, ebp
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        ror     r9, 32
        add     edx, 1272893353
        add     edx, r9d
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        add     ecx, -155497632
        add     ecx, r10d
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        ror     r12, 32
        add     ebx, -1094730640
        add     ebx, r12d
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        ror     r13, 32
        add     eax, 681279174
        add     eax, r13d
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        ror     rbp, 32
        add     edx, -358537222
        add     edx, ebp
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        ror     r8, 32
        add     ecx, -722521979
        add     ecx, r8d
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        ror     r10, 32
        add     ebx, 76029189
        add     ebx, r10d
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        ror     r11, 32
        add     eax, -640364487
        add     eax, r11d
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        ror     r13, 32
        add     edx, -421815835
        add     edx, r13d
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        ror     r14, 32
        add     ecx, 530742520
        add     ecx, r14d
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        ror     r8, 32
        add     ebx, -995338651
        add     ebx, r8d
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        add     eax, -198630844
        add     eax, ebp
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        ror     r10, 32
        add     edx, 1126891415
        add     edx, r10d
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        ror     r14, 32
        add     ecx, -1416354905
        add     ecx, r14d
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        ror     r9, 32
        add     ebx, -57434055
        add     ebx, r9d
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        add     eax, 1700485571
        add     eax, r13d
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        ror     r8, 32
        add     edx, -1894986606
        add     edx, r8d
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        add     ecx, -1051523
        add     ecx, r12d
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        ror     rbp, 32
        add     ebx, -2054922799
        add     ebx, ebp
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        ror     r11, 32
        add     eax, 1873313359
        add     eax, r11d
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        ror     r14, 32
        add     edx, -30611744
        add     edx, r14d
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        ror     r10, 32
        add     ecx, -1560198380
        add     ecx, r10d
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        ror     r13, 32
        add     ebx, 1309151649
        add     ebx, r13d
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        ror     r9, 32
        add     eax, -145523070
        add     eax, r9d
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        ror     r12, 32
        add     edx, -1120210379
        add     edx, r12d
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        ror     r8, 32
        add     ecx, 718787259
        add     ecx, r8d
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        ror     r11, 32
        add     ebx, -343485551
        add     ebx, r11d
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        pop     rsi
        add     dword ptr [rsi], eax
        add     dword ptr [rsi+4H], ebx
        add     dword ptr [rsi+8H], ecx
        add     dword ptr [rsi+0CH], edx
        pop     r14
        pop     r13
        pop     r12
        pop     rbp
        pop     rdi
        pop     rsi
        pop     rbx
end;
{$else}
{$ifdef PUREPASCAL}
var a,b,c,d: cardinal; // unrolled -> compiler will only use cpu registers :)
// the code below is very fast, and can be compared proudly against C or ASM
begin
  a := buf[0];
  b := buf[1];
  c := buf[2];
  d := buf[3];
  {$ifdef FPC} // uses faster built-in right rotate intrinsic
  inc(a,in_[0]+$d76aa478+(d xor(b and(c xor d)))); a := RolDWord(a,7)+b;
  inc(d,in_[1]+$e8c7b756+(c xor(a and(b xor c)))); d := RolDWord(d,12)+a;
  inc(c,in_[2]+$242070db+(b xor(d and(a xor b)))); c := RolDWord(c,17)+d;
  inc(b,in_[3]+$c1bdceee+(a xor(c and(d xor a)))); b := RolDWord(b,22)+c;
  inc(a,in_[4]+$f57c0faf+(d xor(b and(c xor d)))); a := RolDWord(a,7)+b;
  inc(d,in_[5]+$4787c62a+(c xor(a and(b xor c)))); d := RolDWord(d,12)+a;
  inc(c,in_[6]+$a8304613+(b xor(d and(a xor b)))); c := RolDWord(c,17)+d;
  inc(b,in_[7]+$fd469501+(a xor(c and(d xor a)))); b := RolDWord(b,22)+c;
  inc(a,in_[8]+$698098d8+(d xor(b and(c xor d)))); a := RolDWord(a,7)+b;
  inc(d,in_[9]+$8b44f7af+(c xor(a and(b xor c)))); d := RolDWord(d,12)+a;
  inc(c,in_[10]+$ffff5bb1+(b xor(d and(a xor b)))); c := RolDWord(c,17)+d;
  inc(b,in_[11]+$895cd7be+(a xor(c and(d xor a)))); b := RolDWord(b,22)+c;
  inc(a,in_[12]+$6b901122+(d xor(b and(c xor d)))); a := RolDWord(a,7)+b;
  inc(d,in_[13]+$fd987193+(c xor(a and(b xor c)))); d := RolDWord(d,12)+a;
  inc(c,in_[14]+$a679438e+(b xor(d and(a xor b)))); c := RolDWord(c,17)+d;
  inc(b,in_[15]+$49b40821+(a xor(c and(d xor a)))); b := RolDWord(b,22)+c;
  inc(a,in_[1]+$f61e2562+(c xor(d and(b xor c))));  a := RolDWord(a,5)+b;
  inc(d,in_[6]+$c040b340+(b xor(c and(a xor b))));  d := RolDWord(d,9)+a;
  inc(c,in_[11]+$265e5a51+(a xor(b and(d xor a)))); c := RolDWord(c,14)+d;
  inc(b,in_[0]+$e9b6c7aa+(d xor(a and(c xor d))));  b := RolDWord(b,20)+c;
  inc(a,in_[5]+$d62f105d+(c xor(d and(b xor c))));  a := RolDWord(a,5)+b;
  inc(d,in_[10]+$02441453+(b xor(c and(a xor b)))); d := RolDWord(d,9)+a;
  inc(c,in_[15]+$d8a1e681+(a xor(b and(d xor a)))); c := RolDWord(c,14)+d;
  inc(b,in_[4]+$e7d3fbc8+(d xor(a and(c xor d))));  b := RolDWord(b,20)+c;
  inc(a,in_[9]+$21e1cde6+(c xor(d and(b xor c))));  a := RolDWord(a,5)+b;
  inc(d,in_[14]+$c33707d6+(b xor(c and(a xor b)))); d := RolDWord(d,9)+a;
  inc(c,in_[3]+$f4d50d87+(a xor(b and(d xor a))));  c := RolDWord(c,14)+d;
  inc(b,in_[8]+$455a14ed+(d xor(a and(c xor d))));  b := RolDWord(b,20)+c;
  inc(a,in_[13]+$a9e3e905+(c xor(d and(b xor c)))); a := RolDWord(a,5)+b;
  inc(d,in_[2]+$fcefa3f8+(b xor(c and(a xor b))));  d := RolDWord(d,9)+a;
  inc(c,in_[7]+$676f02d9+(a xor(b and(d xor a))));  c := RolDWord(c,14)+d;
  inc(b,in_[12]+$8d2a4c8a+(d xor(a and(c xor d)))); b := RolDWord(b,20)+c;
  inc(a,in_[5]+$fffa3942+(b xor c xor d));  a := RolDWord(a,4)+b;
  inc(d,in_[8]+$8771f681+(a xor b xor c));  d := RolDWord(d,11)+a;
  inc(c,in_[11]+$6d9d6122+(d xor a xor b)); c := RolDWord(c,16)+d;
  inc(b,in_[14]+$fde5380c+(c xor d xor a)); b := RolDWord(b,23)+c;
  inc(a,in_[1]+$a4beea44+(b xor c xor d));  a := RolDWord(a,4)+b;
  inc(d,in_[4]+$4bdecfa9+(a xor b xor c));  d := RolDWord(d,11)+a;
  inc(c,in_[7]+$f6bb4b60+(d xor a xor b));  c := RolDWord(c,16)+d;
  inc(b,in_[10]+$bebfbc70+(c xor d xor a)); b := RolDWord(b,23)+c;
  inc(a,in_[13]+$289b7ec6+(b xor c xor d)); a := RolDWord(a,4)+b;
  inc(d,in_[0]+$eaa127fa+(a xor b xor c));  d := RolDWord(d,11)+a;
  inc(c,in_[3]+$d4ef3085+(d xor a xor b));  c := RolDWord(c,16)+d;
  inc(b,in_[6]+$04881d05+(c xor d xor a));  b := RolDWord(b,23)+c;
  inc(a,in_[9]+$d9d4d039+(b xor c xor d));  a := RolDWord(a,4)+b;
  inc(d,in_[12]+$e6db99e5+(a xor b xor c)); d := RolDWord(d,11)+a;
  inc(c,in_[15]+$1fa27cf8+(d xor a xor b)); c := RolDWord(c,16)+d;
  inc(b,in_[2]+$c4ac5665+(c xor d xor a));   b := RolDWord(b,23)+c;
  inc(a,in_[0]+$f4292244+(c xor(b or(not d))));  a := RolDWord(a,6)+b;
  inc(d,in_[7]+$432aff97+(b xor(a or(not c))));  d := RolDWord(d,10)+a;
  inc(c,in_[14]+$ab9423a7+(a xor(d or(not b)))); c := RolDWord(c,15)+d;
  inc(b,in_[5]+$fc93a039+(d xor(c or(not a))));  b := RolDWord(b,21)+c;
  inc(a,in_[12]+$655b59c3+(c xor(b or(not d)))); a := RolDWord(a,6)+b;
  inc(d,in_[3]+$8f0ccc92+(b xor(a or(not c))));  d := RolDWord(d,10)+a;
  inc(c,in_[10]+$ffeff47d+(a xor(d or(not b)))); c := RolDWord(c,15)+d;
  inc(b,in_[1]+$85845dd1+(d xor(c or(not a))));  b := RolDWord(b,21)+c;
  inc(a,in_[8]+$6fa87e4f+(c xor(b or(not d))));  a := RolDWord(a,6)+b;
  inc(d,in_[15]+$fe2ce6e0+(b xor(a or(not c)))); d := RolDWord(d,10)+a;
  inc(c,in_[6]+$a3014314+(a xor(d or(not b))));  c := RolDWord(c,15)+d;
  inc(b,in_[13]+$4e0811a1+(d xor(c or(not a)))); b := RolDWord(b,21)+c;
  inc(a,in_[4]+$f7537e82+(c xor(b or(not d))));  a := RolDWord(a,6)+b;
  inc(d,in_[11]+$bd3af235+(b xor(a or(not c)))); d := RolDWord(d,10)+a;
  inc(c,in_[2]+$2ad7d2bb+(a xor(d or(not b))));  c := RolDWord(c,15)+d;
  inc(b,in_[9]+$eb86d391+(d xor(c or(not a))));  b := RolDWord(b,21)+c;
  {$else}
  inc(a,in_[0]+$d76aa478+(d xor(b and(c xor d)))); a := ((a shl 7)or(a shr(32-7)))+b;
  inc(d,in_[1]+$e8c7b756+(c xor(a and(b xor c)))); d := ((d shl 12)or(d shr(32-12)))+a;
  inc(c,in_[2]+$242070db+(b xor(d and(a xor b)))); c := ((c shl 17)or(c shr(32-17)))+d;
  inc(b,in_[3]+$c1bdceee+(a xor(c and(d xor a)))); b := ((b shl 22)or(b shr(32-22)))+c;
  inc(a,in_[4]+$f57c0faf+(d xor(b and(c xor d)))); a := ((a shl 7)or(a shr(32-7)))+b;
  inc(d,in_[5]+$4787c62a+(c xor(a and(b xor c)))); d := ((d shl 12)or(d shr(32-12)))+a;
  inc(c,in_[6]+$a8304613+(b xor(d and(a xor b)))); c := ((c shl 17)or(c shr(32-17)))+d;
  inc(b,in_[7]+$fd469501+(a xor(c and(d xor a)))); b := ((b shl 22)or(b shr(32-22)))+c;
  inc(a,in_[8]+$698098d8+(d xor(b and(c xor d)))); a := ((a shl 7)or(a shr(32-7)))+b;
  inc(d,in_[9]+$8b44f7af+(c xor(a and(b xor c)))); d := ((d shl 12)or(d shr(32-12)))+a;
  inc(c,in_[10]+$ffff5bb1+(b xor(d and(a xor b)))); c := ((c shl 17)or(c shr(32-17)))+d;
  inc(b,in_[11]+$895cd7be+(a xor(c and(d xor a)))); b := ((b shl 22)or(b shr(32-22)))+c;
  inc(a,in_[12]+$6b901122+(d xor(b and(c xor d)))); a := ((a shl 7)or(a shr(32-7)))+b;
  inc(d,in_[13]+$fd987193+(c xor(a and(b xor c)))); d := ((d shl 12)or(d shr(32-12)))+a;
  inc(c,in_[14]+$a679438e+(b xor(d and(a xor b)))); c := ((c shl 17)or(c shr(32-17)))+d;
  inc(b,in_[15]+$49b40821+(a xor(c and(d xor a)))); b := ((b shl 22)or(b shr(32-22)))+c;
  inc(a,in_[1]+$f61e2562+(c xor(d and(b xor c))));  a := ((a shl 5)or(a shr(32-5)))+b;
  inc(d,in_[6]+$c040b340+(b xor(c and(a xor b))));  d := ((d shl 9)or(d shr(32-9)))+a;
  inc(c,in_[11]+$265e5a51+(a xor(b and(d xor a)))); c := ((c shl 14)or(c shr(32-14)))+d;
  inc(b,in_[0]+$e9b6c7aa+(d xor(a and(c xor d))));  b := ((b shl 20)or(b shr(32-20)))+c;
  inc(a,in_[5]+$d62f105d+(c xor(d and(b xor c))));  a := ((a shl 5)or(a shr(32-5)))+b;
  inc(d,in_[10]+$02441453+(b xor(c and(a xor b)))); d := ((d shl 9)or(d shr(32-9)))+a;
  inc(c,in_[15]+$d8a1e681+(a xor(b and(d xor a)))); c := ((c shl 14)or(c shr(32-14)))+d;
  inc(b,in_[4]+$e7d3fbc8+(d xor(a and(c xor d))));  b := ((b shl 20)or(b shr(32-20)))+c;
  inc(a,in_[9]+$21e1cde6+(c xor(d and(b xor c))));  a := ((a shl 5)or(a shr(32-5)))+b;
  inc(d,in_[14]+$c33707d6+(b xor(c and(a xor b)))); d := ((d shl 9)or(d shr(32-9)))+a;
  inc(c,in_[3]+$f4d50d87+(a xor(b and(d xor a))));  c := ((c shl 14)or(c shr(32-14)))+d;
  inc(b,in_[8]+$455a14ed+(d xor(a and(c xor d))));  b := ((b shl 20)or(b shr(32-20)))+c;
  inc(a,in_[13]+$a9e3e905+(c xor(d and(b xor c)))); a := ((a shl 5)or(a shr(32-5)))+b;
  inc(d,in_[2]+$fcefa3f8+(b xor(c and(a xor b))));  d := ((d shl 9)or(d shr(32-9)))+a;
  inc(c,in_[7]+$676f02d9+(a xor(b and(d xor a))));  c := ((c shl 14)or(c shr(32-14)))+d;
  inc(b,in_[12]+$8d2a4c8a+(d xor(a and(c xor d)))); b := ((b shl 20)or(b shr(32-20)))+c;
  inc(a,in_[5]+$fffa3942+(b xor c xor d));  a := ((a shl 4)or(a shr(32-4)))+b;
  inc(d,in_[8]+$8771f681+(a xor b xor c));  d := ((d shl 11)or(d shr(32-11)))+a;
  inc(c,in_[11]+$6d9d6122+(d xor a xor b)); c := ((c shl 16)or(c shr(32-16)))+d;
  inc(b,in_[14]+$fde5380c+(c xor d xor a)); b := ((b shl 23)or(b shr(32-23)))+c;
  inc(a,in_[1]+$a4beea44+(b xor c xor d));  a := ((a shl 4)or(a shr(32-4)))+b;
  inc(d,in_[4]+$4bdecfa9+(a xor b xor c));  d := ((d shl 11)or(d shr(32-11)))+a;
  inc(c,in_[7]+$f6bb4b60+(d xor a xor b));  c := ((c shl 16)or(c shr(32-16)))+d;
  inc(b,in_[10]+$bebfbc70+(c xor d xor a)); b := ((b shl 23)or(b shr(32-23)))+c;
  inc(a,in_[13]+$289b7ec6+(b xor c xor d)); a := ((a shl 4)or(a shr(32-4)))+b;
  inc(d,in_[0]+$eaa127fa+(a xor b xor c));  d := ((d shl 11)or(d shr(32-11)))+a;
  inc(c,in_[3]+$d4ef3085+(d xor a xor b));  c := ((c shl 16)or(c shr(32-16)))+d;
  inc(b,in_[6]+$04881d05+(c xor d xor a));  b := ((b shl 23)or(b shr(32-23)))+c;
  inc(a,in_[9]+$d9d4d039+(b xor c xor d));  a := ((a shl 4)or(a shr(32-4)))+b;
  inc(d,in_[12]+$e6db99e5+(a xor b xor c)); d := ((d shl 11)or(d shr(32-11)))+a;
  inc(c,in_[15]+$1fa27cf8+(d xor a xor b)); c := ((c shl 16)or(c shr(32-16)))+d;
  inc(b,in_[2]+$c4ac5665+(c xor d xor a));   b := ((b shl 23)or(b shr(32-23)))+c;
  inc(a,in_[0]+$f4292244+(c xor(b or(not d))));  a := ((a shl 6)or(a shr(32-6)))+b;
  inc(d,in_[7]+$432aff97+(b xor(a or(not c))));  d := ((d shl 10)or(d shr(32-10)))+a;
  inc(c,in_[14]+$ab9423a7+(a xor(d or(not b)))); c := ((c shl 15)or(c shr(32-15)))+d;
  inc(b,in_[5]+$fc93a039+(d xor(c or(not a))));  b := ((b shl 21)or(b shr(32-21)))+c;
  inc(a,in_[12]+$655b59c3+(c xor(b or(not d)))); a := ((a shl 6)or(a shr(32-6)))+b;
  inc(d,in_[3]+$8f0ccc92+(b xor(a or(not c))));  d := ((d shl 10)or(d shr(32-10)))+a;
  inc(c,in_[10]+$ffeff47d+(a xor(d or(not b)))); c := ((c shl 15)or(c shr(32-15)))+d;
  inc(b,in_[1]+$85845dd1+(d xor(c or(not a))));  b := ((b shl 21)or(b shr(32-21)))+c;
  inc(a,in_[8]+$6fa87e4f+(c xor(b or(not d))));  a := ((a shl 6)or(a shr(32-6)))+b;
  inc(d,in_[15]+$fe2ce6e0+(b xor(a or(not c)))); d := ((d shl 10)or(d shr(32-10)))+a;
  inc(c,in_[6]+$a3014314+(a xor(d or(not b))));  c := ((c shl 15)or(c shr(32-15)))+d;
  inc(b,in_[13]+$4e0811a1+(d xor(c or(not a)))); b := ((b shl 21)or(b shr(32-21)))+c;
  inc(a,in_[4]+$f7537e82+(c xor(b or(not d))));  a := ((a shl 6)or(a shr(32-6)))+b;
  inc(d,in_[11]+$bd3af235+(b xor(a or(not c)))); d := ((d shl 10)or(d shr(32-10)))+a;
  inc(c,in_[2]+$2ad7d2bb+(a xor(d or(not b))));  c := ((c shl 15)or(c shr(32-15)))+d;
  inc(b,in_[9]+$eb86d391+(d xor(c or(not a))));  b := ((b shl 21)or(b shr(32-21)))+c;
  {$endif}
  inc(buf[0],a);
  inc(buf[1],b);
  inc(buf[2],c);
  inc(buf[3],d);
end;
{$else PUREPASCAL}
{
 MD5_386.Asm   -  386 optimized helper routine for calculating
                  MD Message-Digest values
 written 2/2/94 by Peter Sawatzki
 Buchenhof 3, D58091 Hagen, Germany Fed Rep
 Peter@Sawatzki.de http://www.sawatzki.de

 original C Source was found in Dr. Dobbs Journal Sep 91
 MD5 algorithm from RSA Data Security, Inc.
 Taken from https://github.com/maximmasiutin/MD5_Transform-x64
}   {$ifdef FPC} nostackframe; assembler; {$endif}
asm // eax=buf:TMD5Buf edx=in_:TMD5In
        push    ebx
        push    esi
        push    edi
        push    ebp
        mov     ebp, edx
        push    eax
        mov     edx, dword ptr [eax+0CH]
        mov     ecx, dword ptr [eax+8H]
        mov     ebx, dword ptr [eax+4H]
        mov     eax, dword ptr [eax]
        add     eax, dword ptr [ebp]
        add     eax, -680876936
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        add     edx, dword ptr [ebp+4H]
        add     edx, -389564586
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        add     ecx, dword ptr [ebp+8H]
        add     ecx, 606105819
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        add     ebx, dword ptr [ebp+0CH]
        add     ebx, -1044525330
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        add     eax, dword ptr [ebp+10H]
        add     eax, -176418897
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        add     edx, dword ptr [ebp+14H]
        add     edx, 1200080426
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        add     ecx, dword ptr [ebp+18H]
        add     ecx, -1473231341
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        add     ebx, dword ptr [ebp+1CH]
        add     ebx, -45705983
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        add     eax, dword ptr [ebp+20H]
        add     eax, 1770035416
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        add     edx, dword ptr [ebp+24H]
        add     edx, -1958414417
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        add     ecx, dword ptr [ebp+28H]
        add     ecx, -42063
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        add     ebx, dword ptr [ebp+2CH]
        add     ebx, -1990404162
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        add     eax, dword ptr [ebp+30H]
        add     eax, 1804603682
        mov     esi, ebx
        not     esi
        and     esi, edx
        mov     edi, ecx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 7
        add     eax, ebx
        add     edx, dword ptr [ebp+34H]
        add     edx, -40341101
        mov     esi, eax
        not     esi
        and     esi, ecx
        mov     edi, ebx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 12
        add     edx, eax
        add     ecx, dword ptr [ebp+38H]
        add     ecx, -1502002290
        mov     esi, edx
        not     esi
        and     esi, ebx
        mov     edi, eax
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 17
        add     ecx, edx
        add     ebx, dword ptr [ebp+3CH]
        add     ebx, 1236535329
        mov     esi, ecx
        not     esi
        and     esi, eax
        mov     edi, edx
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 22
        add     ebx, ecx
        add     eax, dword ptr [ebp+4H]
        add     eax, -165796510
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        add     edx, dword ptr [ebp+18H]
        add     edx, -1069501632
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, dword ptr [ebp+2CH]
        add     ecx, 643717713
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        add     ebx, dword ptr [ebp]
        add     ebx, -373897302
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, dword ptr [ebp+14H]
        add     eax, -701558691
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        add     edx, dword ptr [ebp+28H]
        add     edx, 38016083
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, dword ptr [ebp+3CH]
        add     ecx, -660478335
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        add     ebx, dword ptr [ebp+10H]
        add     ebx, -405537848
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, dword ptr [ebp+24H]
        add     eax, 568446438
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        add     edx, dword ptr [ebp+38H]
        add     edx, -1019803690
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, dword ptr [ebp+0CH]
        add     ecx, -187363961
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        add     ebx, dword ptr [ebp+20H]
        add     ebx, 1163531501
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, dword ptr [ebp+34H]
        add     eax, -1444681467
        mov     esi, edx
        not     esi
        and     esi, ecx
        mov     edi, edx
        and     edi, ebx
        or      esi, edi
        add     eax, esi
        rol     eax, 5
        add     eax, ebx
        add     edx, dword ptr [ebp+8H]
        add     edx, -51403784
        mov     esi, ecx
        not     esi
        and     esi, ebx
        mov     edi, ecx
        and     edi, eax
        or      esi, edi
        add     edx, esi
        rol     edx, 9
        add     edx, eax
        add     ecx, dword ptr [ebp+1CH]
        add     ecx, 1735328473
        mov     esi, ebx
        not     esi
        and     esi, eax
        mov     edi, ebx
        and     edi, edx
        or      esi, edi
        add     ecx, esi
        rol     ecx, 14
        add     ecx, edx
        add     ebx, dword ptr [ebp+30H]
        add     ebx, -1926607734
        mov     esi, eax
        not     esi
        and     esi, edx
        mov     edi, eax
        and     edi, ecx
        or      esi, edi
        add     ebx, esi
        rol     ebx, 20
        add     ebx, ecx
        add     eax, dword ptr [ebp+14H]
        add     eax, -378558
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        add     edx, dword ptr [ebp+20H]
        add     edx, -2022574463
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        add     ecx, dword ptr [ebp+2CH]
        add     ecx, 1839030562
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        add     ebx, dword ptr [ebp+38H]
        add     ebx, -35309556
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        add     eax, dword ptr [ebp+4H]
        add     eax, -1530992060
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        add     edx, dword ptr [ebp+10H]
        add     edx, 1272893353
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        add     ecx, dword ptr [ebp+1CH]
        add     ecx, -155497632
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        add     ebx, dword ptr [ebp+28H]
        add     ebx, -1094730640
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        add     eax, dword ptr [ebp+34H]
        add     eax, 681279174
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        add     edx, dword ptr [ebp]
        add     edx, -358537222
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        add     ecx, dword ptr [ebp+0CH]
        add     ecx, -722521979
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        add     ebx, dword ptr [ebp+18H]
        add     ebx, 76029189
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        add     eax, dword ptr [ebp+24H]
        add     eax, -640364487
        mov     esi, edx
        xor     esi, ecx
        xor     esi, ebx
        add     eax, esi
        rol     eax, 4
        add     eax, ebx
        add     edx, dword ptr [ebp+30H]
        add     edx, -421815835
        mov     esi, ecx
        xor     esi, ebx
        xor     esi, eax
        add     edx, esi
        rol     edx, 11
        add     edx, eax
        add     ecx, dword ptr [ebp+3CH]
        add     ecx, 530742520
        mov     esi, ebx
        xor     esi, eax
        xor     esi, edx
        add     ecx, esi
        rol     ecx, 16
        add     ecx, edx
        add     ebx, dword ptr [ebp+8H]
        add     ebx, -995338651
        mov     esi, eax
        xor     esi, edx
        xor     esi, ecx
        add     ebx, esi
        rol     ebx, 23
        add     ebx, ecx
        add     eax, dword ptr [ebp]
        add     eax, -198630844
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        add     edx, dword ptr [ebp+1CH]
        add     edx, 1126891415
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        add     ecx, dword ptr [ebp+38H]
        add     ecx, -1416354905
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        add     ebx, dword ptr [ebp+14H]
        add     ebx, -57434055
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        add     eax, dword ptr [ebp+30H]
        add     eax, 1700485571
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        add     edx, dword ptr [ebp+0CH]
        add     edx, -1894986606
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        add     ecx, dword ptr [ebp+28H]
        add     ecx, -1051523
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        add     ebx, dword ptr [ebp+4H]
        add     ebx, -2054922799
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        add     eax, dword ptr [ebp+20H]
        add     eax, 1873313359
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        add     edx, dword ptr [ebp+3CH]
        add     edx, -30611744
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        add     ecx, dword ptr [ebp+18H]
        add     ecx, -1560198380
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        add     ebx, dword ptr [ebp+34H]
        add     ebx, 1309151649
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        add     eax, dword ptr [ebp+10H]
        add     eax, -145523070
        mov     esi, edx
        not     esi
        or      esi, ebx
        xor     esi, ecx
        add     eax, esi
        rol     eax, 6
        add     eax, ebx
        add     edx, dword ptr [ebp+2CH]
        add     edx, -1120210379
        mov     esi, ecx
        not     esi
        or      esi, eax
        xor     esi, ebx
        add     edx, esi
        rol     edx, 10
        add     edx, eax
        add     ecx, dword ptr [ebp+8H]
        add     ecx, 718787259
        mov     esi, ebx
        not     esi
        or      esi, edx
        xor     esi, eax
        add     ecx, esi
        rol     ecx, 15
        add     ecx, edx
        add     ebx, dword ptr [ebp+24H]
        add     ebx, -343485551
        mov     esi, eax
        not     esi
        or      esi, ecx
        xor     esi, edx
        add     ebx, esi
        rol     ebx, 21
        add     ebx, ecx
        pop     esi
        add     dword ptr [esi], eax
        add     dword ptr [esi+4H], ebx
        add     dword ptr [esi+8H], ecx
        add     dword ptr [esi+0CH], edx
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;
{$endif PUREPASCAL}
{$endif CPUX64}

procedure RawMd5Compress(var Hash; Data: pointer);
begin
  MD5Transform(TMD5Buf(Hash), PMD5In(Data)^);
end;

function TMD5.Final: TMD5Digest;
begin
  Finalize;
  result := TMD5Digest(buf);
end;

procedure TMD5.Final(out result: TMD5Digest);
begin
  Finalize;
  result := TMD5Digest(buf);
end;

procedure TMD5.Finalize;
var count: Integer;
    p: ^Byte;
begin
  count := bytes[0] and $3f;  // number of pending bytes in
  p := @in_;
  Inc(p,count);
  // Set the first char of padding to 0x80.  There is always room
  p^ := $80;
  Inc(p);
  // Bytes of padding needed to make 56 bytes (-8..55)
  count := 55-count;
  if count<0 then begin  //  Padding forces an extra block
    FillcharFast(p^,count+8,0);
    MD5Transform(buf,in_);
    p := @in_;
    count := 56;
  end;
  FillcharFast(p^,count,0);
  // Append length in bits and transform
  in_[14] := bytes[0] shl 3;
  in_[15] := (bytes[1] shl 3) or (bytes[0] shr 29);
  MD5Transform(buf,in_);
end;

procedure TMD5.Full(Buffer: pointer; Len: integer; out Digest: TMD5Digest);
begin
  buf[0] := $67452301;
  buf[1] := $efcdab89;
  buf[2] := $98badcfe;
  buf[3] := $10325476;
  bytes[0] := Len;
  while Len>=SizeOf(TMD5In) do begin
    MD5Transform(buf,PMD5In(Buffer)^);
    inc(PMD5In(Buffer));
    dec(Len,SizeOf(TMD5In));
  end;
  MoveFast(Buffer^,in_,Len);
  Buffer := PAnsiChar(@in_)+Len;
  PByte(Buffer)^ := $80;
  inc(PByte(Buffer));
  Len := 55-Len;
  if Len>=0 then
    FillcharFast(Buffer^,Len,0) else begin
    FillcharFast(Buffer^,Len+8,0);
    MD5Transform(buf,in_);
    FillcharFast(in_,56,0);
  end;
  Len := bytes[0];
  in_[14] := Len shl 3;
  in_[15] := Len shr 29;
  MD5Transform(buf,in_);
  Digest := TMD5Digest(buf);
end;

procedure TMD5.Init;
begin
  buf[0] := $67452301;
  buf[1] := $efcdab89;
  buf[2] := $98badcfe;
  buf[3] := $10325476;
  bytes[0] := 0;
  bytes[1] := 0;
end;

procedure TMD5.Update(const buffer; len: Cardinal);
var p: ^TMD5In;
    t: cardinal;
    i: integer;
begin
  p := @buffer;
  // Update byte count
  t := bytes[0];
  Inc(bytes[0],len);
  if bytes[0]<t then
    Inc(bytes[1]);     // 64 bit carry from low to high
  t := 64-(t and 63);  // space available in in_ (at least 1)
  if t>len then begin
    MoveFast(p^,Pointer(PtrUInt(@in_)+64-t)^,len);
    exit;
  end;
  // First chunk is an odd size
  MoveFast(p^,Pointer(PtrUInt(@in_)+64-t)^,t);
  MD5Transform(buf,in_);
  inc(PByte(p),t);
  dec(len,t);
  // Process data in 64-byte chunks
  for i := 1 to len shr 6 do begin
    MD5Transform(buf,p^);
    inc(p);
  end;
  // Handle any remaining bytes of data.
  MoveFast(p^,in_,len and 63);
end;

procedure TMD5.Update(const Buffer: RawByteString);
begin
  Update(pointer(Buffer)^,length(Buffer));
end;

function MD5Buf(const Buffer; Len: Cardinal): TMD5Digest;
var MD5: TMD5;
begin
  MD5.Full(@Buffer,Len,result);
end;

function htdigest(const user, realm, pass: RawByteString): RawUTF8;
// apache-compatible: agent007:download area:8364d0044ef57b3defcfa141e8f77b65
//    hash=`echo -n "$user:$realm:$pass" | md5sum | cut -b -32`
//    echo "$user:$realm:$hash"
var tmp: RawByteString;
begin
  tmp := user+':'+realm+':';
  result := tmp+MD5(tmp+pass);
end;

function MD5SelfTest: boolean;
begin
  result := (htdigest('agent007','download area','secret')=
    'agent007:download area:8364d0044ef57b3defcfa141e8f77b65') and
    (MD5('')='d41d8cd98f00b204e9800998ecf8427e') and
    (MD5('a')='0cc175b9c0f1b6a831c399e269772661') and
    (MD5('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789')=
     'd174ab98d277d9f5a5611c2c9f419d9f');
end;


{ TSHA1 }

// TSHAContext = Hash,MLen,Buffer,Index
procedure sha1Compress(var Hash: TSHAHash; Data: PByteArray);
var A, B, C, D, E, X: cardinal;
    W: array[0..79] of cardinal;
    i: integer;
begin
  // init W[] + A..E
  bswap256(@Data[0],@W[0]);
  bswap256(@Data[32],@W[8]);
  for i := 16 to 79 do begin
    X  := W[i-3] xor W[i-8] xor W[i-14] xor W[i-16];
    W[i] := (X shl 1) or (X shr 31);
  end;
  A := Hash.A;
  B := Hash.B;
  C := Hash.C;
  D := Hash.D;
  E := Hash.E;
  // unrolled loop -> all is computed in cpu registers
  // note: FPC detects "(A shl 5) or (A shr 27)" pattern into "RolDWord(A,5)" :)
  Inc(E,((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[ 0]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[ 1]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[ 2]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[ 3]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[ 4]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[ 5]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[ 6]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[ 7]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[ 8]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[ 9]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[10]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[11]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[12]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[13]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[14]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[15]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[16]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[17]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[18]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[19]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[20]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[21]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[22]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[23]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[24]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[25]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[26]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[27]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[28]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[29]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[30]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[31]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[32]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[33]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[34]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[35]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[36]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[37]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[38]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[39]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[40]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[41]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[42]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[43]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[44]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[45]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[46]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[47]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[48]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[49]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[50]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[51]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[52]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[53]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[54]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[55]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[56]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[57]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[58]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[59]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[60]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[61]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[62]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[63]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[64]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[65]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[66]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[67]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[68]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[69]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[70]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[71]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[72]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[73]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[74]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[75]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[76]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[77]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[78]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[79]); C:= (C shl 30) or (C shr 2);
  // Calculate new working hash
  inc(Hash.A,A);
  inc(Hash.B,B);
  inc(Hash.C,C);
  inc(Hash.D,D);
  inc(Hash.E,E);
end;

procedure RawSha1Compress(var Hash; Data: pointer);
begin
  sha1Compress(TSHAHash(Hash), Data);
end;

procedure TSHA1.Final(out Digest: TSHA1Digest; NoInit: boolean);
var Data: TSHAContext absolute Context;
begin
  // 1. append bit '1' after Buffer
  Data.Buffer[Data.Index] := $80;
  FillcharFast(Data.Buffer[Data.Index+1],63-Data.Index,0);
  // 2. Compress if more than 448 bits, (no room for 64 bit length
  if Data.Index>=56 then begin
    sha1Compress(Data.Hash,@Data.Buffer);
    FillcharFast(Data.Buffer,56,0);
  end;
  // Write 64 bit Buffer length into the last bits of the last block
  // (in big endian format) and do a final compress
  PCardinal(@Data.Buffer[56])^ := bswap32(TQWordRec(Data.MLen).H);
  PCardinal(@Data.Buffer[60])^ := bswap32(TQWordRec(Data.MLen).L);
  sha1Compress(Data.Hash,@Data.Buffer);
  // Hash -> Digest to little endian format
  bswap160(@Data.Hash,@Digest);
  // Clear Data
  if not NoInit then
    Init;
end;

function TSHA1.Final(NoInit: boolean): TSHA1Digest;
begin
  Final(result,NoInit);
end;

procedure TSHA1.Full(Buffer: pointer; Len: integer; out Digest: TSHA1Digest);
begin
{$ifdef USEPADLOCK}
  // Padlock need all data once -> Full() is OK, not successive Update()
  if padlock_available then begin
    Init; // for later Update use
    {$ifdef PADLOCKDEBUG}write('padlock_phe_sha1 ');{$endif}
    if padlock_phe_sha1(buffer,Len,Digest)=0 then
      exit else
    {$ifdef PADLOCKDEBUG}write(':ERROR ');{$endif}
  end;
{$endif}
  Init;
  Update(Buffer,Len);
  Final(Digest);
end;

procedure TSHA1.Init;
var Data: TSHAContext absolute Context;
begin
  Data.Hash.A := $67452301;
  Data.Hash.B := $EFCDAB89;
  Data.Hash.C := $98BADCFE;
  Data.Hash.D := $10325476;
  Data.Hash.E := $C3D2E1F0;
  FillcharFast(Data.MLen,sizeof(Data)-sizeof(Data.Hash),0);
end;

procedure TSHA1.Update(Buffer: pointer; Len: integer);
var Data: TSHAContext absolute Context;
    aLen: integer;
begin
  if Buffer=nil then exit; // avoid GPF
  inc(Data.MLen,QWord(Cardinal(Len)) shl 3);
  while Len>0 do begin
    aLen := sizeof(Data.Buffer)-Data.Index;
    if aLen<=Len then begin
      if Data.Index<>0 then begin
        MoveFast(buffer^,Data.Buffer[Data.Index],aLen);
        sha1Compress(Data.Hash,@Data.Buffer);
        Data.Index := 0;
      end else
        sha1Compress(Data.Hash,Buffer); // avoid temporary copy
      dec(Len,aLen);
      inc(PByte(buffer),aLen);
    end else begin
      MoveFast(buffer^,Data.Buffer[Data.Index],Len);
      inc(Data.Index,Len);
      break;
    end;
  end;
end;

procedure TSHA1.Update(const Buffer: RawByteString);
begin
  Update(pointer(Buffer),length(Buffer));
end;


{ TAESAbstract }

var
  aesivctr: array[boolean] of TAESLocked;

procedure AESIVCtrEncryptDecrypt(const BI; var BO; DoEncrypt: boolean);
begin
  if aesivctr[DoEncrypt]=nil then begin
    GarbageCollectorFreeAndNil(aesivctr[DoEncrypt],TAESLocked.Create);
    with aesivctr[DoEncrypt].fAES do
      if DoEncrypt then
        EncryptInit(AESIVCTR_KEY,128) else
        DecryptInit(AESIVCTR_KEY,128);
  end;
  with aesivctr[DoEncrypt] do begin
    fSafe^.Lock;
    TAESContext(fAES.Context).DoBlock(fAES.Context,BI,BO);
    fSafe^.UnLock;
  end;
end;

constructor TAESAbstract.Create(const aKey; aKeySize: cardinal);
begin
   if (aKeySize<>128) and (aKeySize<>192) and (aKeySize<>256) then
    raise ESynCrypto.CreateUTF8('%.Create(aKeySize=%): 128/192/256 required',[self,aKeySize]);
  fKeySize := aKeySize;
  fKeySizeBytes := fKeySize shr 3;
  MoveFast(aKey,fKey,fKeySizeBytes);
end;

procedure TAESAbstract.SetIVCTR;
var tmp: PShortString; // temp variable to circumvent FPC bug
begin
  repeat
    TAESPRNG.Main.FillRandom(TAESBLock(fIVCTR)); // set nonce + ctr
  until fIVCTR.nonce<>0;
  tmp := ClassNameShort(self);
  fIVCtr.magic := crc32c($aba5aba5,@tmp^[2],6); // TAESECB_API -> 'AESECB'
end;

constructor TAESAbstract.Create(const aKey: THash128);
begin
  Create(aKey,128);
end;

constructor TAESAbstract.Create(const aKey: THash256);
begin
  Create(aKey,256);
end;

constructor TAESAbstract.CreateTemp(aKeySize: cardinal);
var tmp: THash256;
begin
  TAESPRNG.Main.FillRandom(tmp);
  Create(tmp,aKeySize);
  FillZero(tmp);
end;

constructor TAESAbstract.CreateFromSha256(const aKey: RawUTF8);
var Digest: TSHA256Digest;
begin
  SHA256Weak(aKey,Digest);
  Create(Digest,256);
  FillZero(Digest);
end;

constructor TAESAbstract.CreateFromPBKDF2(const aKey: RawUTF8; const aSalt: RawByteString;
  aRounds: Integer);
var Digest: TSHA256Digest;
begin
  PBKDF2_HMAC_SHA256(aKey,aSalt,aRounds,Digest,ToText(ClassType));
  Create(Digest,256);
  FillZero(Digest);
end;

destructor TAESAbstract.Destroy;
begin
  inherited Destroy;
  FillZero(fKey);
end;

function TAESAbstract.AlgoName: TShort16;
const TXT: array[2..4] of array[0..7] of AnsiChar = (#9'aes128',#9'aes192',#9'aes256');
var s: PShortString;
begin
  if (self=nil) or (KeySize=0) then
    result[0] := #0 else begin
    PInt64(@result)^ := PInt64(@TXT[KeySize shr 6])^;
    s := ClassNameShort(self);
    if s^[0]<#7 then
      result[0] := #6 else begin
      result[7] := NormToLower[s^[5]]; // TAESCBC -> 'aes128cbc'
      result[8] := NormToLower[s^[6]];
      result[9] := NormToLower[s^[7]];
    end;
  end;
end;

procedure TAESAbstract.SetIVHistory(aDepth: integer);
begin
  fIVHistoryDec.Init(aDepth,aDepth);
end;

function TAESAbstract.EncryptPKCS7(const Input: RawByteString;
  IVAtBeginning: boolean): RawByteString;
begin
  SetString(result,nil,EncryptPKCS7Length(length(Input),IVAtBeginning));
  EncryptPKCS7Buffer(Pointer(Input),pointer(result),
    length(Input),length(result),IVAtBeginning);
end;

function TAESAbstract.EncryptPKCS7(const Input: TBytes;
  IVAtBeginning: boolean): TBytes;
begin
  result := nil;
  SetLength(result,EncryptPKCS7Length(length(Input),IVAtBeginning));
  EncryptPKCS7Buffer(Pointer(Input),pointer(result),
    length(Input),length(result),IVAtBeginning);
end;

function TAESAbstract.EncryptPKCS7Length(InputLen: cardinal;
  IVAtBeginning: boolean): cardinal;
begin
  result := InputLen+sizeof(TAESBlock)-(InputLen and AESBlockMod);
  if IVAtBeginning then
    inc(Result,sizeof(TAESBlock));
end;

function TAESAbstract.EncryptPKCS7Buffer(Input,Output: Pointer; InputLen,OutputLen: cardinal;
  IVAtBeginning: boolean): boolean;
var padding, ivsize: cardinal;
begin
  padding := sizeof(TAESBlock)-(InputLen and AESBlockMod);
  if IVAtBeginning then
    ivsize := sizeof(TAESBlock) else
    ivsize := 0;
  if OutputLen<>ivsize+InputLen+padding then begin
    result := false;
    exit;
  end;
  if IVAtBeginning then begin
    if fIVReplayAttackCheck<>repNoCheck then begin
      if fIVCTR.nonce=0 then
        SetIVCTR;
      AESIVCtrEncryptDecrypt(fIVCTR,fIV,true); // PRNG from fixed secret
      inc(fIVCTR.ctr); // replay attack protection
    end else
      TAESPRNG.Main.FillRandom(fIV); // PRNG from real entropy
    PAESBlock(Output)^ := fIV;
  end;
  MoveFast(Input^,PByteArray(Output)^[ivsize],InputLen);
  FillcharFast(PByteArray(Output)^[ivsize+InputLen],padding,padding);
  Inc(PByte(Output),ivsize);
  Encrypt(Output,Output,InputLen+padding);
  result := true;
end;

function TAESAbstract.DecryptPKCS7Len(var InputLen,ivsize: Integer;
  Input: pointer; IVAtBeginning, RaiseESynCryptoOnError: boolean): boolean;
var ctr: TAESIVCTR;
begin
  result := true;
  if (InputLen<sizeof(TAESBlock)) or (InputLen and AESBlockMod<>0) then
    if RaiseESynCryptoOnError then
      raise ESynCrypto.CreateUTF8('%.DecryptPKCS7: Invalid InputLen=%',[self,InputLen]) else
      result := false;
  if result and IVAtBeginning then begin
    if (fIVReplayAttackCheck<>repNoCheck) and (fIVCTRState<>ctrNotUsed) then begin
      if fIVCTR.nonce=0 then
        SetIVCTR;
      AESIVCtrEncryptDecrypt(Input^,ctr,false);
      if fIVCTRState=ctrUnknown then
        if ctr.magic=fIVCTR.magic then begin
          fIVCTR := ctr;
          fIVCTRState := ctrUsed;
          inc(fIVCTR.ctr);
        end else
        if fIVReplayAttackCheck=repMandatory then
          if RaiseESynCryptoOnError then
            raise ESynCrypto.CreateUTF8('%.DecryptPKCS7: IVCTR is not handled '+
             'on encryption',[self]) else
            result := false else begin
          fIVCTRState := ctrNotused;
          if fIVHistoryDec.Depth=0 then
            SetIVHistory(64); // naive but efficient fallback
        end else
        if IsEqual(TAESBlock(ctr),TAESBlock(fIVCTR)) then
          inc(fIVCTR.ctr) else
          if RaiseESynCryptoOnError then
            raise ESynCrypto.CreateUTF8('%.DecryptPKCS7: wrong IVCTR %/% %/% -> '+
             'potential replay attack',[self,ctr.magic,fIVCTR.magic,ctr.ctr,fIVCTR.ctr]) else
            result := false;
    end;
    fIV := PAESBlock(Input)^;
    if result and (fIVHistoryDec.Depth>0) and not fIVHistoryDec.Add(fIV) then
      if RaiseESynCryptoOnError then
        raise ESynCrypto.CreateUTF8('%.DecryptPKCS7: duplicated IV=% -> '+
          'potential replay attack',[self,AESBlockToShortString(fIV)]) else
       result := false;
    dec(InputLen,sizeof(TAESBlock));
    ivsize := sizeof(TAESBlock);
  end else
    ivsize := 0;
end;

function TAESAbstract.DecryptPKCS7Buffer(Input: Pointer; InputLen: integer;
  IVAtBeginning, RaiseESynCryptoOnError: boolean): RawByteString;
var ivsize,padding: integer;
    tmp: array[0..1023] of AnsiChar;
    P: PAnsiChar;
begin
  result := '';
  if not DecryptPKCS7Len(InputLen,ivsize,Input,IVAtBeginning,RaiseESynCryptoOnError) then
    exit;
  if InputLen<sizeof(tmp) then
    P := @tmp else begin
    SetString(result,nil,InputLen);
    P := pointer(result);
  end;
  Decrypt(@PByteArray(Input)^[ivsize],P,InputLen);
  padding := ord(P[InputLen-1]); // result[1..len]
  if padding>sizeof(TAESBlock) then
    if RaiseESynCryptoOnError then
      raise ESynCrypto.CreateUTF8('%.DecryptPKCS7: Invalid Input',[self]) else
      result := '' else
    if P=@tmp then
      SetString(result,P,InputLen-padding) else
      SetLength(result,InputLen-padding); // fast in-place resize
end;

function TAESAbstract.DecryptPKCS7(const Input: RawByteString;
  IVAtBeginning, RaiseESynCryptoOnError: boolean): RawByteString;
begin
  result := DecryptPKCS7Buffer(pointer(Input),length(Input),IVAtBeginning,RaiseESynCryptoOnError);
end;

function TAESAbstract.DecryptPKCS7(const Input: TBytes;
  IVAtBeginning, RaiseESynCryptoOnError: boolean): TBytes;
var len,ivsize,padding: integer;
begin
  result := nil;
  len := length(Input);
  if not DecryptPKCS7Len(len,ivsize,pointer(Input),IVAtBeginning,RaiseESynCryptoOnError) then
    exit;
  SetLength(result,len);
  Decrypt(@PByteArray(Input)^[ivsize],pointer(result),len);
  padding := result[len-1]; // result[0..len-1]
  if padding>sizeof(TAESBlock) then
    if RaiseESynCryptoOnError then
      raise ESynCrypto.CreateUTF8('%.DecryptPKCS7: Invalid Input',[self]) else
      result := nil else
    SetLength(result,len-padding); // fast in-place resize
end;

function TAESAbstract.MACSetNonce(const aKey: THash256; aAssociated: pointer;
  aAssociatedLen: integer): boolean;
begin
  result := false;
end;

function TAESAbstract.MACGetLast(out aCRC: THash256): boolean;
begin
  result := false;
end;

function TAESAbstract.MACEquals(const aCRC: THash256): boolean;
var mac: THash256;
begin
  result := MACGetLast(mac) and IsEqual(mac,aCRC);
end;

function TAESAbstract.MACCheckError(aEncrypted: pointer; Count: cardinal): boolean;
begin
  result := false;
end;

function TAESAbstract.MACAndCrypt(const Data: RawByteString; Encrypt: boolean): RawByteString;
type
  TCryptData = packed record
    nonce,mac: THash256;
    crc: cardinal; // crc32c(nonce+mac) to avoid naive fuzzing
    data: RawByteString;
  end;
  PCryptData = ^TCryptData;
const
  VERSION = 1;
  CRCSIZ = sizeof(THash256)*2;
  SIZ = CRCSIZ+sizeof(cardinal);
var rec: TCryptData;
    len: integer;
    pcd: PCryptData absolute Data;
    P: PAnsiChar;
begin
  result := ''; // e.g. MACSetNonce not supported
  try
    if Encrypt then begin
      TAESPRNG.Main.FillRandom(rec.nonce);
      if not MACSetNonce(rec.nonce) then
        exit;
      rec.Data := EncryptPKCS7(Data,{IVAtBeginning=}true);
      if not MACGetLast(rec.mac) then
        exit;
      rec.crc := crc32c(VERSION,@rec.nonce,CRCSIZ);
      result := RecordSave(rec,TypeInfo(TCryptData));
    end else begin
      if (length(Data)<=SIZ) or (pcd^.crc<>crc32c(VERSION,pointer(pcd),CRCSIZ)) then
        exit;
      P := @pcd^.data; // inlined RecordLoad() for safety
      len := FromVarUInt32(PByte(P));
      if length(Data)-len<>P-pointer(Data) then
        exit; // avoid buffer overflow
      if MACSetNonce(pcd^.nonce) then
        result := DecryptPKCS7Buffer(P,len,true,false);
      if result<>'' then
        if not MACEquals(pcd^.mac) then begin
          FillZero(result);
          result := '';
        end;
    end;
  finally
    FillZero(rec.data);
  end;
end;

class function TAESAbstract.MACEncrypt(const Data: RawByteString; const Key: THash256;
  Encrypt: boolean): RawByteString;
var aes: TAESAbstract;
begin
  aes := Create(Key);
  try
    result := aes.MACAndCrypt(Data,Encrypt);
  finally
    aes.Free;
  end;
end;

class function TAESAbstract.MACEncrypt(const Data: RawByteString; const Key: THash128;
  Encrypt: boolean): RawByteString;
var aes: TAESAbstract;
begin
  aes := Create(Key);
  try
    result := aes.MACAndCrypt(Data,Encrypt);
  finally
    aes.Free;
  end;
end;

class function TAESAbstract.SimpleEncrypt(const Input,Key: RawByteString;
  Encrypt, IVAtBeginning, RaiseESynCryptoOnError: boolean): RawByteString;
var instance: TAESAbstract;
begin
  instance := CreateFromSha256(Key);
  try
    if Encrypt then
      result := instance.EncryptPKCS7(Input,IVAtBeginning) else
      result := instance.DecryptPKCS7(Input,IVAtBeginning,RaiseESynCryptoOnError);
  finally
    instance.Free;
  end;
end;

class function TAESAbstract.SimpleEncrypt(const Input: RawByteString; const Key;
  KeySize: integer; Encrypt, IVAtBeginning, RaiseESynCryptoOnError: boolean): RawByteString;
var instance: TAESAbstract;
begin
  instance := Create(Key,KeySize);
  try
    if Encrypt then
      result := instance.EncryptPKCS7(Input,IVAtBeginning) else
      result := instance.DecryptPKCS7(Input,IVAtBeginning,RaiseESynCryptoOnError);
  finally
    instance.Free;
  end;
end;

class function TAESAbstract.SimpleEncryptFile(const InputFile, OutputFile: TFileName;
  const Key: RawByteString; Encrypt, IVAtBeginning,RaiseESynCryptoOnError: boolean): boolean;
var src,dst: RawByteString;
begin
  result := false;
  src := StringFromFile(InputFile);
  if src<>'' then begin
    dst := SimpleEncrypt(src,Key,Encrypt,IVAtBeginning,RaiseESynCryptoOnError);
    if dst<>'' then
      result := FileFromString(dst,OutputFile);
  end;
end;

class function TAESAbstract.SimpleEncryptFile(const InputFile, Outputfile: TFileName;
  const Key; KeySize: integer; Encrypt, IVAtBeginning, RaiseESynCryptoOnError: boolean): boolean;
var src,dst: RawByteString;
begin
  result := false;
  src := StringFromFile(InputFile);
  if src<>'' then begin
    dst := SimpleEncrypt(src,Key,KeySize,Encrypt,IVAtBeginning,RaiseESynCryptoOnError);
    if dst<>'' then
      result := FileFromString(dst,OutputFile);
  end;
end;

function TAESAbstract.Clone: TAESAbstract;
begin
  result := TAESAbstractClass(ClassType).Create(fKey,fKeySize);
  result.IVHistoryDepth := IVHistoryDepth;
  result.IVReplayAttackCheck := IVReplayAttackCheck;
end;

function TAESAbstract.CloneEncryptDecrypt: TAESAbstract;
begin
  result := Clone;
end;


{ TAESAbstractSyn }

destructor TAESAbstractSyn.Destroy;
begin
  inherited Destroy;
  AES.Done;      // mandatory for Padlock - also fill buffer with 0 for safety
  FillZero(fCV); // may contain sensitive data on some modes
  FillZero(fIV);
end;

function TAESAbstractSyn.Clone: TAESAbstract;
begin
  if (fIVHistoryDec.Count<>0) {$ifdef USEPADLOCK} or
     TAESContext(AES).initialized and (TAESContext(AES).ViaCtx<>nil){$endif} then
    result := inherited Clone else begin
    result := NewInstance as TAESAbstractSyn;
    MoveFast(pointer(self)^,pointer(result)^,InstanceSize);
  end;
end;

procedure TAESAbstractSyn.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  fIn := BufIn;
  fOut := BufOut;
  fCV := fIV;
end;

procedure TAESAbstractSyn.DecryptInit;
begin
  if AES.DecryptInit(fKey,fKeySize) then
    fAESInit := initDecrypt else
    raise ESynCrypto.CreateUTF8('%.DecryptInit',[self]);
end;

procedure TAESAbstractSyn.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  fIn := BufIn;
  fOut := BufOut;
  fCV := fIV;
end;

procedure TAESAbstractSyn.EncryptInit;
begin
  if AES.EncryptInit(fKey,fKeySize) then
    fAESInit := initEncrypt else
    raise ESynCrypto.CreateUTF8('%.EncryptInit',[self]);
end;

procedure TAESAbstractSyn.TrailerBytes(count: cardinal);
begin
  if fAESInit<>initEncrypt then
    EncryptInit;
  TAESContext(AES.Context).DoBlock(AES.Context,fCV,fCV);
  XorMemory(pointer(fOut),pointer(fIn),@fCV,count);
end;


{ TAESECB }

procedure TAESECB.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  inherited; // CV := IV + set fIn,fOut
  if fAESInit<>initDecrypt then
    DecryptInit;
  for i := 1 to Count shr 4 do begin
    TAESContext(AES.Context).DoBlock(AES.Context,fIn^,fOut^);
    inc(fIn);
    inc(fOut);
  end;
  Count := Count and AESBlockMod;
  if Count<>0 then
    TrailerBytes(Count);
end;

procedure TAESECB.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  inherited; // CV := IV + set fIn,fOut
  if fAESInit<>initEncrypt then
    EncryptInit;
  for i := 1 to Count shr 4 do begin
    TAESContext(AES.Context).DoBlock(AES.Context,fIn^,fOut^);
    inc(fIn);
    inc(fOut);
  end;
  Count := Count and AESBlockMod;
  if Count<>0 then
    TrailerBytes(Count);
end;


{ TAESCBC }

procedure TAESCBC.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
    tmp: TAESBlock;
begin
  inherited; // CV := IV + set fIn,fOut
  if Count>=sizeof(TAESBlock) then begin
    if fAESInit<>initDecrypt then
      DecryptInit;
    for i := 1 to Count shr 4 do begin
      tmp := fIn^;
      TAESContext(AES.Context).DoBlock(AES.Context,fIn^,fOut^);
      XorBlock16(pointer(fOut),pointer(@fCV));
      fCV := tmp;
      inc(fIn);
      inc(fOut);
    end;
  end;
  Count := Count and AESBlockMod;
  if Count<>0 then
    TrailerBytes(Count);
end;

procedure TAESCBC.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  inherited; // CV := IV + set fIn,fOut
  if fAESInit<>initEncrypt then
    EncryptInit;
  for i := 1 to Count shr 4 do begin
    XorBlock16(pointer(fIn),pointer(fOut),pointer(@fCV));
    TAESContext(AES.Context).DoBlock(AES.Context,fOut^,fOut^);
    fCV := fOut^;
    inc(fIn);
    inc(fOut);
  end;
  Count := Count and AESBlockMod;
  if Count<>0 then
    TrailerBytes(Count);
end;

{ TAESAbstractEncryptOnly }

constructor TAESAbstractEncryptOnly.Create(const aKey; aKeySize: cardinal);
begin
  inherited Create(aKey,aKeySize);
  EncryptInit; // as expected by overriden Encrypt/Decrypt methods below
end;

function TAESAbstractEncryptOnly.CloneEncryptDecrypt: TAESAbstract;
begin
  result := self;
end;


{ TAESCFB }

{$ifdef USEAESNI32}
procedure AesNiTrailer; // = TAESAbstractSyn.EncryptTrailer from AES-NI asm
  {$ifdef FPC} nostackframe; assembler; {$endif}
asm // eax=TAESContext ecx=len xmm7=CV esi=BufIn edi=BufOut
    call   dword ptr [eax].TAESContext.AesNi32 // = AES.Encrypt(fCV,fCV)
    lea    edx, [eax].TAESContext.buf // used as temporary buffer
    movups [edx], xmm7
    cld
@s: lodsb
    xor    al, [edx] // = XorMemory(pointer(fOut),pointer(fIn),@fCV,len);
    inc    edx
    stosb
    dec    ecx
    jnz    @s
end;
{$endif}

procedure TAESCFB.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
    tmp: TAESBlock;
begin
  {$ifdef USEAESNI32}
  if Assigned(TAESContext(AES.Context).AesNi32) then
  asm
        push    esi
        push    edi
        mov     eax, self
        mov     ecx, count
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[eax].TAESCFB.fIV
        lea     eax, [eax].TAESCFB.AES
        push    ecx
        shr     ecx, 4
        jz      @z
@s:     call    dword ptr[eax].TAESContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        movaps  xmm1, xmm0
        pxor    xmm0, xmm7
        movaps  xmm7, xmm1              // fCV := fIn
        movups  dqword ptr[edi], xmm0  // fOut := fIn xor fCV
        dec     ecx
        lea     esi, [esi + 16]
        lea     edi, [edi + 16]
        jnz     @s
@z:     pop     ecx
        and     ecx, 15
        jz      @0
        call    AesNiTrailer
@0:     pop     edi
        pop     esi
        pxor    xmm7, xmm7 // for safety
  end else
  {$endif} begin
    inherited; // CV := IV + set fIn,fOut
    for i := 1 to Count shr 4 do begin
      tmp := fIn^;
      TAESContext(AES.Context).DoBlock(AES.Context,fCV,fCV);
      XorBlock16(pointer(fIn),pointer(fOut),pointer(@fCV));
      fCV := tmp;
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AESBlockMod;
    if Count<>0 then
      TrailerBytes(Count);
  end;
end;

procedure TAESCFB.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  {$ifdef USEAESNI32}
  if Assigned(TAESContext(AES.Context).AesNi32) then
  asm
        push    esi
        push    edi
        mov     eax, self
        mov     ecx, count
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[eax].TAESCFB.fIV
        lea     eax, [eax].TAESCFB.AES
        push    ecx
        shr     ecx, 4
        jz      @z
@s:     call    dword ptr[eax].TAESContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        pxor    xmm7, xmm0
        movups  dqword ptr[edi], xmm7  // fOut := fIn xor fCV
        dec     ecx
        lea     esi, [esi + 16]
        lea     edi, [edi + 16]
        jnz     @s
@z:     pop     ecx
        and     ecx, 15
        jz      @0
        call    AesNiTrailer
@0:     pop     edi
        pop     esi
        pxor    xmm7, xmm7 // for safety
  end else
  {$endif} begin
    inherited; // CV := IV + set fIn,fOut
    for i := 1 to Count shr 4 do begin
      TAESContext(AES.Context).DoBlock(AES.Context,fCV,fCV);
      XorBlock16(pointer(fIn),pointer(fOut),pointer(@fCV));
      fCV := fOut^;
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AESBlockMod;
    if Count<>0 then
      TrailerBytes(Count);
  end;
end;


{ TAESAbstractAEAD }

destructor TAESAbstractAEAD.Destroy;
begin
  inherited Destroy;
  FillCharFast(fMacKey,sizeof(fMacKey),0);
  FillCharFast(fMac,sizeof(fMac),0);
end;

function TAESAbstractAEAD.MACSetNonce(const aKey: THash256; aAssociated: pointer;
  aAssociatedLen: integer): boolean;
var rec: THash256Rec absolute aKey;
begin
  // safe seed for plain text crc, before AES encryption
  // from TECDHEProtocol.SetKey, aKey is a public nonce to avoid replay attacks
  fMACKey.plain := rec.Lo;
  XorBlock16(@fMACKey.plain,@rec.Hi);
  // neutral seed for encrypted crc, to check for errors, with no compromission
  if (aAssociated<>nil) and (aAssociatedLen>0) then
    crc128c(aAssociated,aAssociatedLen,fMACKey.encrypted) else
    FillcharFast(fMACKey.encrypted,sizeof(THash128),255);
  result := true;
end;

function TAESAbstractAEAD.MACGetLast(out aCRC: THash256): boolean;
var rec: THash256Rec absolute aCRC;
begin
  // encrypt the plain text crc, to perform message authentication and integrity
  AES.Encrypt(fMAC.plain,rec.Lo);
  // store the encrypted text crc, to check for errors, with no compromission
  rec.Hi := fMAC.encrypted;
  result := true;
end;

function TAESAbstractAEAD.MACCheckError(aEncrypted: pointer; Count: cardinal): boolean;
var crc: THash128;
begin
  result := false;
  if (Count<32) or (Count and AESBlockMod<>0) then
    exit;
  crc := fMACKey.encrypted;
  crcblocks(@crc,aEncrypted,Count shr 4-2);
  result := IsEqual(crc,PHash128(@PByteArray(aEncrypted)[Count-sizeof(crc)])^);
end;


{ TAESCFBCRC }

procedure TAESCFBCRC.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
    tmp: TAESBlock;
begin
  if Count=0 then
    exit;
  fMAC := fMACKey; // reuse the same key until next MACSetNonce()
  {$ifdef USEAESNI32}
  if Assigned(TAESContext(AES.Context).AesNi32) and (Count and AESBlockMod=0) then
  asm
        push    ebx
        push    esi
        push    edi
        mov     ebx, self
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[ebx].TAESCFBCRC.fIV
@s:     lea     eax, [ebx].TAESCFBCRC.fMAC.encrypted
        mov     edx, esi
        call    crcblock // using SSE4.2 or fast tables
        lea     eax, [ebx].TAESCFBCRC.AES
        call    dword ptr[eax].TAESContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        movaps  xmm1, xmm0
        pxor    xmm0, xmm7
        movaps  xmm7, xmm1              // fCV := fIn
        movups  dqword ptr[edi], xmm0  // fOut := fIn xor fCV
        lea     eax, [ebx].TAESCFBCRC.fMAC.plain
        mov     edx, edi
        call    crcblock
        sub     dword ptr[count], 16
        lea     esi, [esi + 16]
        lea     edi, [edi + 16]
        ja      @s
@z:     pop     edi
        pop     esi
        pop     ebx
        pxor    xmm7, xmm7 // for safety
  end else
  {$endif} begin
    inherited; // CV := IV + set fIn,fOut
    for i := 1 to Count shr 4 do begin
      tmp := fIn^;
      crcblock(@fMAC.encrypted,pointer(fIn)); // fIn may be = fOut
      TAESContext(AES.Context).DoBlock(AES.Context,fCV,fCV);
      XorBlock16(pointer(fIn),pointer(fOut),pointer(@fCV));
      fCV := tmp;
      crcblock(@fMAC.plain,pointer(fOut));
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AESBlockMod;
    if Count<>0 then begin
      TrailerBytes(Count);
      with fMAC do // includes trailing bytes to the plain crc
        PCardinal(@plain)^ := crc32c(PCardinal(@plain)^,pointer(fOut),Count);
    end;
  end;
end;

procedure TAESCFBCRC.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  if Count=0 then
    exit;
  fMAC := fMACKey; // reuse the same key until next MACSetNonce()
  {$ifdef USEAESNI32}
  if Assigned(TAESContext(AES.Context).AesNi32) and (Count and AESBlockMod=0) then
  asm
        push    ebx
        push    esi
        push    edi
        mov     ebx, self
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[ebx].TAESCFBCRC.fIV
@s:     lea     eax, [ebx].TAESCFBCRC.fMAC.plain
        mov     edx, esi
        call    crcblock
        lea     eax, [ebx].TAESCFBCRC.AES
        call    dword ptr[eax].TAESContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        pxor    xmm7, xmm0
        movups  dqword ptr[edi], xmm7  // fOut := fIn xor fCV  +  fCV := fOut^
        lea     eax, [ebx].TAESCFBCRC.fMAC.encrypted
        mov     edx, edi
        call    crcblock
        sub     dword ptr[count], 16
        lea     esi, [esi + 16]
        lea     edi, [edi + 16]
        ja      @s
        pop     edi
        pop     esi
        pop     ebx
        pxor    xmm7, xmm7 // for safety
  end else
  {$endif} begin
    inherited; // CV := IV + set fIn,fOut
    for i := 1 to Count shr 4 do begin
      TAESContext(AES.Context).DoBlock(AES.Context,fCV,fCV);
      crcblock(@fMAC.plain,pointer(fIn)); // fOut may be = fIn
      XorBlock16(pointer(fIn),pointer(fOut),pointer(@fCV));
      fCV := fOut^;
      crcblock(@fMAC.encrypted,pointer(fOut));
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AESBlockMod;
    if Count<>0 then begin
      with fMAC do // includes trailing bytes to the plain crc
        PCardinal(@plain)^ := crc32c(PCardinal(@plain)^,pointer(fIn),Count);
      TrailerBytes(Count);
    end;
  end;
end;


{ TAESOFBCRC }

procedure TAESOFBCRC.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  if Count=0 then
    exit;
  fMAC := fMACKey; // reuse the same key until next MACSetNonce()
  {$ifdef USEAESNI32}
  if Assigned(TAESContext(AES.Context).AesNi32) and (Count and AESBlockMod=0) then
  asm
        push    ebx
        push    esi
        push    edi
        mov     ebx, self
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[ebx].TAESOFBCRC.fIV
@s:     lea     eax, [ebx].TAESOFBCRC.fMAC.encrypted
        mov     edx, esi
        call    crcblock
        lea     eax, [ebx].TAESOFBCRC.AES
        call    dword ptr[eax].TAESContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        pxor    xmm0, xmm7
        movups  dqword ptr[edi], xmm0  // fOut := fIn xor fCV
        lea     eax, [ebx].TAESOFBCRC.fMAC.plain
        mov     edx, edi
        call    crcblock
        sub     dword ptr[count], 16
        lea     esi, [esi + 16]
        lea     edi, [edi + 16]
        ja      @s
        pop     edi
        pop     esi
        pop     ebx
        pxor    xmm7, xmm7 // for safety
  end else
  {$endif} begin
    inherited Encrypt(BufIn,BufOut,Count); // CV := IV + set fIn,fOut
    for i := 1 to Count shr 4 do begin
      TAESContext(AES.Context).DoBlock(AES.Context,fCV,fCV);
      crcblock(@fMAC.encrypted,pointer(fIn)); // fOut may be = fIn
      XorBlock16(pointer(fIn),pointer(fOut),pointer(@fCV));
      crcblock(@fMAC.plain,pointer(fOut));
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AESBlockMod;
    if Count<>0 then begin
      TrailerBytes(Count);
      with fMAC do // includes trailing bytes to the plain crc
        PCardinal(@plain)^ := crc32c(PCardinal(@plain)^,pointer(fOut),Count);
    end;
  end;
end;

procedure TAESOFBCRC.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  if Count=0 then
    exit;
  fMAC := fMACKey; // reuse the same key until next MACSetNonce()
  {$ifdef USEAESNI32}
  if Assigned(TAESContext(AES.Context).AesNi32) and (Count and AESBlockMod=0) then
  asm
        push    ebx
        push    esi
        push    edi
        mov     ebx, self
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[ebx].TAESOFBCRC.fIV
@s:     lea     eax, [ebx].TAESOFBCRC.fMAC.plain
        mov     edx, esi
        call    crcblock
        lea     eax, [ebx].TAESOFBCRC.AES
        call    dword ptr[eax].TAESContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        pxor    xmm0, xmm7
        movups  dqword ptr[edi], xmm0  // fOut := fIn xor fCV
        lea     eax, [ebx].TAESOFBCRC.fMAC.encrypted
        mov     edx, edi
        call    crcblock
        sub     dword ptr[count], 16
        lea     esi, [esi + 16]
        lea     edi, [edi + 16]
        ja      @s
        pop     edi
        pop     esi
        pop     ebx
        pxor    xmm7, xmm7 // for safety
  end else
  {$endif} begin
    inherited Encrypt(BufIn,BufOut,Count); // CV := IV + set fIn,fOut
    for i := 1 to Count shr 4 do begin
      TAESContext(AES.Context).DoBlock(AES.Context,fCV,fCV);
      crcblock(@fMAC.plain,pointer(fIn)); // fOut may be = fIn
      XorBlock16(pointer(fIn),pointer(fOut),pointer(@fCV));
      crcblock(@fMAC.encrypted,pointer(fOut));
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AESBlockMod;
    if Count<>0 then begin
      with fMAC do // includes trailing bytes to the plain crc
        PCardinal(@plain)^ := crc32c(PCardinal(@plain)^,pointer(fIn),Count);
      TrailerBytes(Count);
    end;
  end;
end;


{ TAESOFB }

{$ifdef USEAESNI64}
procedure AesNiEncryptOFB_128(self: TAESOFB; source, dest: pointer; blockcount: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        test    blockcount, blockcount
        jz      @z
        movups  xmm7, dqword ptr[self].TAESOFB.fIV  // xmm7 = fCV
        lea     self, [self].TAESOFB.AES
        movups  xmm0, dqword ptr[self + 16 * 0]
        movups  xmm1, dqword ptr[self + 16 * 1]
        movups  xmm2, dqword ptr[self + 16 * 2]
        movups  xmm3, dqword ptr[self + 16 * 3]
        movups  xmm4, dqword ptr[self + 16 * 4]
        movups  xmm5, dqword ptr[self + 16 * 5]
        movups  xmm6, dqword ptr[self + 16 * 6]
        movups  xmm8, dqword ptr[self + 16 * 7]
        movups  xmm9, dqword ptr[self + 16 * 8]
        movups  xmm10, dqword ptr[self + 16 * 9]
        movups  xmm11, dqword ptr[self + 16 * 10]
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr[source]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenclast xmm7, xmm11
        pxor    xmm15, xmm7
        movups  dqword ptr[dest], xmm15  // fOut := fIn xor fCV
        add     source, 16
        add     dest, 16
        dec     blockcount
        jnz     @s
@z:
end;

procedure AesNiEncryptOFB_256(self: TAESOFB; source, dest: pointer; blockcount: PtrUInt);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        test    blockcount, blockcount
        jz      @z
        movups  xmm7, dqword ptr[self].TAESOFB.fIV  // xmm7 = fCV
        lea     self, [self].TAESOFB.AES
        movups  xmm0, dqword ptr[self + 16 * 0]
        movups  xmm1, dqword ptr[self + 16 * 1]
        movups  xmm2, dqword ptr[self + 16 * 2]
        movups  xmm3, dqword ptr[self + 16 * 3]
        movups  xmm4, dqword ptr[self + 16 * 4]
        movups  xmm5, dqword ptr[self + 16 * 5]
        movups  xmm6, dqword ptr[self + 16 * 6]
        movups  xmm8, dqword ptr[self + 16 * 7]
        movups  xmm9, dqword ptr[self + 16 * 8]
        movups  xmm10, dqword ptr[self + 16 * 9]
        movups  xmm11, dqword ptr[self + 16 * 10]
        movups  xmm12, dqword ptr[self + 16 * 11]
        movups  xmm13, dqword ptr[self + 16 * 12]
        movups  xmm14, dqword ptr[self + 16 * 13]
        add     self, 16 * 14
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     movups  xmm15, dqword ptr[self]
        pxor    xmm7, xmm0
        aesenc  xmm7, xmm1
        aesenc  xmm7, xmm2
        aesenc  xmm7, xmm3
        aesenc  xmm7, xmm4
        aesenc  xmm7, xmm5
        aesenc  xmm7, xmm6
        aesenc  xmm7, xmm8
        aesenc  xmm7, xmm9
        aesenc  xmm7, xmm10
        aesenc  xmm7, xmm11
        aesenc  xmm7, xmm12
        aesenc  xmm7, xmm13
        aesenc  xmm7, xmm14
        aesenclast xmm7, xmm15
        movups  xmm15, dqword ptr[source]
        pxor    xmm15, xmm7
        movups  dqword ptr[dest], xmm15  // fOut := fIn xor fCV
        add     source, 16
        add     dest, 16
        dec     blockcount
        jnz     @s
@z:
end;
{$endif USEAESNI64}

procedure TAESOFB.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  Encrypt(BufIn, BufOut, Count); // by definition
end;

procedure TAESOFB.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  {$ifdef USEAESNI64}
  if (Count and AESBlockMod=0) and (cfAESNI in CpuFeatures) then
    with TAESContext(AES.Context) do
    case KeyBits of
    128: begin
      AesNiEncryptOFB_128(self,BufIn,BufOut,Count shr 4);
      exit;
    end;
    256: begin
      AesNiEncryptOFB_256(self,BufIn,BufOut,Count shr 4);
      exit;
    end;
    end;
  {$endif USEAESNI64}
  {$ifdef USEAESNI32}
  if Assigned(TAESContext(AES.Context).AesNi32) then
  asm
        push    esi
        push    edi
        mov     eax, self
        mov     ecx, count
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[eax].TAESOFB.fIV  // xmm7 = fCV
        lea     eax, [eax].TAESOFB.AES
        push    ecx
        shr     ecx, 4
        jz      @z
@s:     call    dword ptr[eax].TAESContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        pxor    xmm0, xmm7
        movups  dqword ptr[edi], xmm0  // fOut := fIn xor fCV
        dec     ecx
        lea     esi, [esi + 16]
        lea     edi, [edi + 16]
        jnz     @s
@z:     pop     ecx
        and     ecx, 15
        jz      @0
        call    AesNiTrailer
@0:     pop     edi
        pop     esi
        pxor    xmm7, xmm7 // for safety
  end else
  {$endif} begin
    inherited; // CV := IV + set fIn,fOut
    for i := 1 to Count shr 4 do begin
      TAESContext(AES.Context).DoBlock(AES.Context,fCV,fCV);
      XorBlock16(pointer(fIn),pointer(fOut),pointer(@fCV));
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AESBlockMod;
    if Count<>0 then
      TrailerBytes(Count);
  end;
end;


{ TAESCTR }

constructor TAESCTR.Create(const aKey; aKeySize: cardinal);
begin
  inherited Create(aKey, aKeySize);
  fCTROffset := 7; // counter is in the lower 64 bits, nonce in the upper 64 bits
end;

function TAESCTR.ComposeIV(Nonce, Counter: PAESBlock; NonceLen, CounterLen: integer;
  LSBCounter: boolean): boolean;
begin
  result := (NonceLen + CounterLen = 16) and (CounterLen > 0);
  if result then
    if LSBCounter then begin
      MoveFast(Nonce[0], fIV[0], NonceLen);
      MoveFast(Counter[0], fIV[NonceLen], CounterLen);
      fCTROffset := 15;
      fCTROffsetMin := 16-CounterLen;
    end else begin
      MoveFast(Counter[0], fIV[0], CounterLen);
      MoveFast(Nonce[0], fIV[CounterLen], NonceLen);
      fCTROffset := CounterLen-1;
      fCTROffsetMin := 0;
    end;
end;

function TAESCTR.ComposeIV(const Nonce, Counter: TByteDynArray;
  LSBCounter: boolean): boolean;
begin
  result := ComposeIV(pointer(Nonce), pointer(Counter),
    length(Nonce), length(Counter), LSBCounter);
end;

procedure TAESCTR.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
    offs: PtrInt;
    tmp: TAESBlock;
begin
  inherited; // CV := IV + set fIn,fOut
  for i := 1 to Count shr 4 do begin
    TAESContext(AES.Context).DoBlock(AES.Context,fCV,tmp);
    offs := fCTROffset;
    inc(fCV[offs]);
    if fCV[offs]=0 then // manual big-endian increment
      repeat
        dec(offs);
        inc(fCV[offs]);
        if (fCV[offs]<>0) or (offs=fCTROffsetMin) then
          break;
      until false;
    XorBlock16(pointer(fIn),pointer(fOut),pointer(@tmp));
    inc(fIn);
    inc(fOut);
  end;
  Count := Count and AESBlockMod;
  if Count<>0 then begin
    TAESContext(AES.Context).DoBlock(AES.Context,fCV,tmp);
    XorMemory(pointer(fOut),pointer(fIn),@tmp,Count);
  end;
end;

procedure TAESCTR.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  Encrypt(BufIn, BufOut, Count); // by definition
end;


{ TAESGCM }

constructor TAESGCM.Create(const aKey; aKeySize: cardinal);
begin
  inherited Create(aKey,aKeySize); // set fKey/fKeySize
  if not fAES.Init(aKey,aKeySize) then
    raise ESynCrypto.CreateUTF8('%.Create(keysize=%) failed',[self,aKeySize]);
end;

function TAESGCM.Clone: TAESAbstract;
begin
  result := NewInstance as TAESGCM;
  result.fKey := fKey;
  result.fKeySize := fKeySize;
  result.fKeySizeBytes := fKeySizeBytes;
  TAESGCM(result).fAES := fAES; // reuse the very same TAESGCMEngine memory
end;

destructor TAESGCM.Destroy;
begin
  inherited Destroy;
  fAES.Done;
  FillZero(fIV);
end;

procedure TAESGCM.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  if fContext<>ctxEncrypt then
    if fContext=ctxNone then begin
      fAES.Reset(@fIV,CTR_POS); // caller should have set the IV
      fContext := ctxEncrypt;
    end else
      raise ESynCrypto.CreateUTF8('%.Encrypt after Decrypt',[self]);
  if not fAES.Encrypt(BufIn,BufOut,Count) then
    raise ESynCrypto.CreateUTF8('%.Encrypt called after GCM final state',[self]);
end;

procedure TAESGCM.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  if fContext<>ctxDecrypt then
    if fContext=ctxNone then begin
      fAES.Reset(@fIV,CTR_POS);
      fContext := ctxDecrypt;
    end else
      raise ESynCrypto.CreateUTF8('%.Decrypt after Encrypt',[self]);
  if not fAES.Decrypt(BufIn,BufOut,Count) then
    raise ESynCrypto.CreateUTF8('%.Decrypt called after GCM final state',[self]);
end;

function TAESGCM.MACSetNonce(const aKey: THash256; aAssociated: pointer;
  aAssociatedLen: integer): boolean;
begin
  if fContext<>ctxNone then begin
    result := false; // should be called before Encrypt/Decrypt
    exit;
  end;
  // aKey is ignored since not used during GMAC computation
  if (aAssociated<>nil) and (aAssociatedLen>0) then
    fAES.Add_AAD(aAssociated,aAssociatedLen);
  result := true;
end;

function TAESGCM.MACGetLast(out aCRC: THash256): boolean;
begin
  if fContext=ctxNone then begin
    result := false; // should be called after Encrypt/Decrypt
    exit;
  end;
  fAES.Final(THash256Rec(aCRC).Lo,{forreuse:anddone=}false);
  FillZero(THash256Rec(aCRC).Hi); // upper 128-bit are not used
  fContext := ctxNone; // allow reuse of this fAES instance
  result := true;
end;

function TAESGCM.MACCheckError(aEncrypted: pointer; Count: cardinal): boolean;
begin
  result := true; // AES-GCM requires the IV to be set -> will be checked later
end;


{$ifdef MSWINDOWS}

type
  HCRYPTPROV = pointer;
  HCRYPTKEY = pointer;
  HCRYPTHASH = pointer;

  {$ifdef USERECORDWITHMETHODS}TCryptLibrary = record
    {$else}TCryptLibrary = object{$endif}
  public
    AcquireContextA: function(var phProv: HCRYPTPROV; pszContainer: PAnsiChar;
      pszProvider: PAnsiChar; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
    ReleaseContext: function(hProv: HCRYPTPROV; dwFlags: PtrUInt): BOOL; stdcall;
    ImportKey: function(hProv: HCRYPTPROV; pbData: pointer; dwDataLen: DWORD;
      hPubKey: HCRYPTKEY; dwFlags: DWORD; var phKey: HCRYPTKEY): BOOL; stdcall;
    SetKeyParam: function(hKey: HCRYPTKEY; dwParam: DWORD; pbData: pointer;
      dwFlags: DWORD): BOOL; stdcall;
    DestroyKey: function(hKey: HCRYPTKEY): BOOL; stdcall;
    Encrypt: function(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL;
      dwFlags: DWORD; pbData: pointer; var pdwDataLen: DWORD; dwBufLen: DWORD): BOOL; stdcall;
    Decrypt: function(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL;
      dwFlags: DWORD; pbData: pointer; var pdwDataLen: DWORD): BOOL; stdcall;
    GenRandom: function(hProv: HCRYPTPROV; dwLen: DWORD; pbBuffer: Pointer): BOOL; stdcall;
    Tested: boolean;
    Handle: THandle;
    function Available: boolean;
  end;

const
  HCRYPTPROV_NOTTESTED = HCRYPTPROV(-1);
  PROV_RSA_FULL = 1;
  CRYPT_VERIFYCONTEXT  = DWORD($F0000000);

var
  CryptoAPI: TCryptLibrary;

function TCryptLibrary.Available: boolean;
  procedure Acquire;
  const NAMES: array[0..7] of PChar = (
    'CryptAcquireContextA','CryptReleaseContext',
    'CryptImportKey','CryptSetKeyParam','CryptDestroyKey',
    'CryptEncrypt','CryptDecrypt','CryptGenRandom');
  var P: PPointer;
      i: integer;
  begin
    Tested := true;
    Handle := GetModuleHandle('advapi32.dll');
    if Handle<>0 then begin
      P := @@AcquireContextA;
      for i := 0 to high(NAMES) do begin
        P^ := GetProcAddress(Handle,NAMES[i]);
        if P^=nil then begin
          PPointer(@@AcquireContextA)^ := nil;
          break;
        end;
        inc(P);
      end;
    end;
  end;
begin
  if not Tested then
    Acquire;
  result := Assigned(AcquireContextA);
end;

{$ifdef USE_PROV_RSA_AES}
var
  CryptoAPIAESProvider: HCRYPTPROV = HCRYPTPROV_NOTTESTED;

const
  PROV_RSA_AES = 24;
  CRYPT_NEWKEYSET = 8;
  PLAINTEXTKEYBLOB = 8;
  CUR_BLOB_VERSION = 2;
  KP_IV = 1;
  KP_MODE = 4;

  CALG_AES_128  = $660E;
  CALG_AES_192  = $660F;
  CALG_AES_256  = $6610;

  CRYPT_MODE_CBC = 1;
  CRYPT_MODE_ECB = 2;
  CRYPT_MODE_OFB = 3;
  CRYPT_MODE_CFB = 4;
  CRYPT_MODE_CTS = 5;

procedure EnsureCryptoAPIAESProviderAvailable;
begin
  if CryptoAPIAESProvider=nil then
    raise ESynCrypto.Create('PROV_RSA_AES provider not installed') else
  if CryptoAPIAESProvider=HCRYPTPROV_NOTTESTED then begin
    CryptoAPIAESProvider := nil;
    if CryptoAPI.Available then begin
      if not CryptoAPI.AcquireContextA(CryptoAPIAESProvider,nil,nil,PROV_RSA_AES,0) then
        if (HRESULT(GetLastError)<>NTE_BAD_KEYSET) or not CryptoAPI.AcquireContextA(
           CryptoAPIAESProvider,nil,nil,PROV_RSA_AES,CRYPT_NEWKEYSET) then
          raise ESynCrypto.CreateLastOSError('in AcquireContext',[]);
    end;
  end;
end;


{ TAESAbstract_API }

constructor TAESAbstract_API.Create(const aKey; aKeySize: cardinal);
begin
  EnsureCryptoAPIAESProviderAvailable;
  inherited Create(aKey,aKeySize); // check and set fKeySize[Bytes]
  InternalSetMode;
  fKeyHeader.bType := PLAINTEXTKEYBLOB;
  fKeyHeader.bVersion := CUR_BLOB_VERSION;
  case fKeySize of
  128: fKeyHeader.aiKeyAlg := CALG_AES_128;
  192: fKeyHeader.aiKeyAlg := CALG_AES_192;
  256: fKeyHeader.aiKeyAlg := CALG_AES_256;
  end;
  fKeyHeader.dwKeyLength := fKeySizeBytes;
  fKeyHeaderKey := fKey;
end;

destructor TAESAbstract_API.Destroy;
begin
  if fKeyCryptoAPI<>nil then
    CryptoAPI.DestroyKey(fKeyCryptoAPI);
  FillCharFast(fKeyHeaderKey,sizeof(fKeyHeaderKey),0);
  inherited;
end;

procedure TAESAbstract_API.EncryptDecrypt(BufIn, BufOut: pointer; Count: cardinal;
  DoEncrypt: boolean);
var n: Cardinal;
begin
  if Count=0 then
    exit; // nothing to do
  if fKeyCryptoAPI<>nil then begin
    CryptoAPI.DestroyKey(fKeyCryptoAPI);
    fKeyCryptoAPI := nil;
  end;
  if not CryptoAPI.ImportKey(CryptoAPIAESProvider,
     @fKeyHeader,sizeof(fKeyHeader)+fKeySizeBytes,nil,0,fKeyCryptoAPI) then
    raise ESynCrypto.CreateLastOSError('in CryptImportKey for %',[self]);
  if not CryptoAPI.SetKeyParam(fKeyCryptoAPI,KP_IV,@fIV,0) then
    raise ESynCrypto.CreateLastOSError('in CryptSetKeyParam(KP_IV) for %',[self]);
  if not CryptoAPI.SetKeyParam(fKeyCryptoAPI,KP_MODE,@fInternalMode,0) then
    raise ESynCrypto.CreateLastOSError('in CryptSetKeyParam(KP_MODE,%) for %',[fInternalMode,self]);
  if BufOut<>BufIn then
    MoveFast(BufIn^,BufOut^,Count);
  n := Count and not AESBlockMod;
  if DoEncrypt then begin
    if not CryptoAPI.Encrypt(fKeyCryptoAPI,nil,false,0,BufOut,n,Count) then
      raise ESynCrypto.CreateLastOSError('in Encrypt() for %',[self]);
  end else
    if not CryptoAPI.Decrypt(fKeyCryptoAPI,nil,false,0,BufOut,n) then
      raise ESynCrypto.CreateLastOSError('in Decrypt() for %',[self]);
  dec(Count,n);
  if Count>0 then // remaining bytes will be XORed with the supplied IV
    XorMemory(@PByteArray(BufOut)[n],@PByteArray(BufIn)[n],@fIV,Count);
end;

procedure TAESAbstract_API.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  EncryptDecrypt(BufIn,BufOut,Count,true);
end;

procedure TAESAbstract_API.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  EncryptDecrypt(BufIn,BufOut,Count,false);
end;

{ TAESECB_API }

procedure TAESECB_API.InternalSetMode;
begin
  fInternalMode := CRYPT_MODE_ECB;
end;

{ TAESCBC_API }

procedure TAESCBC_API.InternalSetMode;
begin
  fInternalMode := CRYPT_MODE_CBC;
end;

{ TAESCFB_API }

procedure TAESCFB_API.InternalSetMode;
begin
  raise ESynCrypto.CreateUTF8('%: CRYPT_MODE_CFB does not work',[self]);
  fInternalMode := CRYPT_MODE_CFB;
end;

{ TAESOFB_API }

procedure TAESOFB_API.InternalSetMode;
begin
  raise ESynCrypto.CreateUTF8('%: CRYPT_MODE_OFB not implemented by PROV_RSA_AES',[self]);
  fInternalMode := CRYPT_MODE_OFB;
end;

{$endif USE_PROV_RSA_AES}

{$endif MSWINDOWS}


{ TAESLocked }

destructor TAESLocked.Destroy;
begin
  inherited Destroy;
  fAES.Done; // mandatory for Padlock - also fill AES buffer with 0 for safety
end;


{ TAESPRNG }

constructor TAESPRNG.Create(PBKDF2Rounds, ReseedAfterBytes, AESKeySize: integer);
begin
  inherited Create;
  if PBKDF2Rounds<2 then
    PBKDF2Rounds := 2;
  fSeedPBKDF2Rounds := PBKDF2Rounds;
  fSeedAfterBytes := ReseedAfterBytes;
  fAESKeySize := AESKeySize;
  Seed;
end;

procedure FillSystemRandom(Buffer: PByteArray; Len: integer; AllowBlocking: boolean);
var fromos: boolean;
    i: integer;
    {$ifdef LINUX}
    dev: integer;
    {$endif}
    {$ifdef MSWINDOWS}
    prov: HCRYPTPROV;
    {$endif}
    tmp: array[byte] of byte;
begin
  fromos := false;
  {$ifdef LINUX}
  dev := FileOpen('/dev/urandom',fmOpenRead);
  if (dev<=0) and AllowBlocking then
    dev := FileOpen('/dev/random',fmOpenRead);
  if dev>0 then
    try
      i := Len;
      if i>32 then
        i := 32; // up to 256 bits - see "man urandom" Usage paragraph
      fromos := (FileRead(dev,Buffer[0],i)=i) and (Len<=32); // will XOR up to Len
    finally
      FileClose(dev);
    end;
  {$endif LINUX}
  {$ifdef MSWINDOWS}
  if CryptoAPI.Available then
    if CryptoAPI.AcquireContextA(prov,nil,nil,PROV_RSA_FULL,CRYPT_VERIFYCONTEXT) then begin
      fromos := CryptoAPI.GenRandom(prov,len,Buffer);
      CryptoAPI.ReleaseContext(prov,0);
    end;
  {$endif MSWINDOWS}
  if fromos then
    exit;
  i := Len;
  repeat // call Random32() (=RdRand32 or Lecuyer) as fallback/padding
    SynCommons.FillRandom(@tmp,SizeOf(tmp) shr 2);
    if i<=SizeOf(tmp) then begin
      XorMemory(@Buffer^[Len-i],@tmp,i);
      break;
    end;
    XorMemoryPtrInt(@Buffer^[Len-i],@tmp,SizeOf(tmp) shr {$ifdef CPU32}2{$else}3{$endif});
    dec(i,SizeOf(tmp));
  until false;
end;

{$ifdef DELPHI5OROLDER} // not defined in SysUtils.pas
function CreateGuid(out guid: TGUID): HResult; stdcall;
  external 'ole32.dll' name 'CoCreateGuid';
{$endif}

class function TAESPRNG.GetEntropy(Len: integer; SystemOnly: boolean): RawByteString;
var ext: TSynExtended;
    data: THash512Rec;
    fromos, version: RawByteString;
    sha3: TSHA3;
  procedure sha3update;
  var g: TGUID;
  begin
    SynCommons.FillRandom(@data.Hi,8); // QueryPerformanceCounter+8*Random32
    sha3.Update(@data,sizeof(data));
    CreateGUID(g); // not random, but genuine (at least on Windows)
    sha3.Update(@g,sizeof(g));
  end;
begin
  QueryPerformanceCounter(data.d0); // data.d1 = some bytes on stack
  try
    // retrieve some initial entropy from OS
    SetLength(fromos,Len);
    FillSystemRandom(pointer(fromos),len,{allowblocking=}SystemOnly);
    if SystemOnly then begin
      result := fromos;
      fromos := '';
      exit;
    end;
    // xor some explicit entropy - it won't hurt
    sha3.Init(SHAKE_256); // used in XOF mode for variable-length output
    sha3update;
    data.h1 := ExeVersion.Hash.b;
    version := RecordSave(ExeVersion,TypeInfo(TExeVersion));
    sha3.Update(version); // exe and host/user info
    ext := NowUTC;
    sha3.Update(@ext,sizeof(ext));
    sha3update;
    ext := Random; // why not?
    sha3.Update(@ext,sizeof(ext));
    data.i0 := integer(HInstance); // override data.d0d1/h0
    data.i1 := integer(GetCurrentThreadId);
    data.i2 := integer(MainThreadID);
    data.i3 := integer(UnixMSTimeUTCFast);
    SleepHiRes(0); // force non deterministic time shift
    sha3update;
    sha3.Update(OSVersionText);
    sha3.Update(@SystemInfo,sizeof(SystemInfo));
    result := sha3.Cypher(fromos); // = XOR entropy using SHA-3 in XOF mode
  finally
    sha3.Done;
    FillZero(fromos);
  end;
end;

procedure TAESPRNG.Seed;
var key: THash512Rec;
    entropy: RawByteString;
begin
  try
    entropy := GetEntropy(128); // 128 bytes is the HMAC_SHA512 key block size
    PBKDF2_HMAC_SHA512(entropy,ExeVersion.User,fSeedPBKDF2Rounds,key.b);
    fSafe^.Lock;
    try
      fAES.EncryptInit(key.Lo,fAESKeySize);
      crcblocks(@fCTR,@key.Hi,2);
      fBytesSinceSeed := 0;
    finally
      fSafe^.UnLock;
    end;
  finally
    FillZero(key.b); // avoid the key appear in clear on stack
    FillZero(entropy);
  end;
end;

procedure TAESPRNG.IncrementCTR;
begin
  {$ifdef CPU64}
  inc(fCTR.Lo);
  if fCTR.Lo=0 then
    inc(fCTR.Hi);
  {$else}
  inc(fCTR.i0);
  if fCTR.i0=0 then begin
    inc(fCTR.i1);
    if fCTR.i1=0 then begin
      inc(fCTR.i2);
      if fCTR.i2=0 then
        inc(fCTR.i3);
    end;
  end;
  {$endif}
end;

procedure TAESPRNG.FillRandom(out Block: TAESBlock);
begin
  if fBytesSinceSeed>fSeedAfterBytes then
    Seed;
  fSafe^.Lock;
  TAESContext(fAES.Context).DoBlock(fAES.Context,fCTR.b,Block);
  IncrementCTR;
  inc(fBytesSinceSeed,SizeOf(Block));
  inc(fTotalBytes,SizeOf(Block));
  fSafe^.UnLock;
end;

procedure TAESPRNG.FillRandom(out Buffer: THash256);
begin
  FillRandom(@Buffer,sizeof(Buffer));
end;

procedure TAESPRNG.FillRandom(Buffer: pointer; Len: integer);
var buf: ^TAESBlock absolute Buffer;
    rnd: TAESBLock;
    i: integer;
begin
  if Len<=0 then
    exit;
  if fBytesSinceSeed>fSeedAfterBytes then
    Seed;
  fSafe^.Lock;
  for i := 1 to Len shr 4 do begin
    TAESContext(fAES.Context).DoBlock(fAES.Context,fCTR.b,buf^);
    IncrementCTR;
    inc(buf);
  end;
  inc(fBytesSinceSeed,Len);
  inc(fTotalBytes,Len);
  Len := Len and AESBlockMod;
  if Len>0 then begin
    TAESContext(fAES.Context).DoBlock(fAES.Context,fCTR.b,rnd);
    IncrementCTR;
    MoveFast(rnd,buf^,Len);
  end;
  fSafe^.UnLock;
end;

function TAESPRNG.FillRandom(Len: integer): RawByteString;
begin
  SetString(result,nil,Len);
  FillRandom(pointer(result),Len);
end;

function TAESPRNG.FillRandomBytes(Len: integer): TBytes;
begin
  if Len<>length(result) then
    result := nil;
  SetLength(result,Len);
  FillRandom(pointer(result),Len);
end;

function TAESPRNG.FillRandomHex(Len: integer): RawUTF8;
var bin: pointer;
begin
  FastSetString(result,nil,Len*2);
  if Len=0 then
    exit;
  bin := @PByteArray(result)[Len]; // temporary store random bytes at the end
  FillRandom(bin,Len);
  SynCommons.BinToHex(bin,pointer(result),Len);
end;

function TAESPRNG.Random32: cardinal;
var block: THash128Rec;
begin
  FillRandom(block.b);
  result := block.c0; // no need to XOR with c1, c2, c3 with a permutation algo
end;

function TAESPRNG.Random32(max: cardinal): cardinal;
var block: THash128Rec;
begin
  FillRandom(block.b);
  result := (Qword(block.c0)*max) shr 32; // no need to XOR with c1, c2, c3
end;

function TAESPRNG.Random64: QWord;
var block: THash128Rec;
begin
  FillRandom(block.b);
  result := block.L; // no need to XOR with H
end;

function Hash128ToExt({$ifdef FPC}constref{$else}const{$endif} r: THash128): TSynExtended;
const
  COEFF64: TSynExtended = (1.0/$80000000)/$100000000;  // 2^-63
begin
  result := (THash128Rec(r).Lo and $7fffffffffffffff)*COEFF64;
end;

function Hash128ToDouble({$ifdef FPC}constref{$else}const{$endif} r: THash128): double;
const
  COEFF64: double = (1.0/$80000000)/$100000000;  // 2^-63
begin
  result := (THash128Rec(r).Lo and $7fffffffffffffff)*COEFF64;
end;

function Hash128ToSingle({$ifdef FPC}constref{$else}const{$endif} r: THash128): double;
const
  COEFF64: single = (1.0/$80000000)/$100000000;  // 2^-63
begin
  result := (THash128Rec(r).Lo and $7fffffffffffffff)*COEFF64;
end;

function TAESPRNG.RandomExt: TSynExtended;
var block: THash128;
begin
  FillRandom(block);
  result := Hash128ToExt(block);
end;

function TAESPRNG.RandomDouble: double;
var block: THash128;
begin
  FillRandom(block);
  result := Hash128ToDouble(block);
end;

function TAESPRNG.RandomPassword(Len: integer): RawUTF8;
const CHARS: array[0..127] of AnsiChar =
  'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'+
  ':bcd.fgh(jklmn)pqrst?vwxyz+BCD%FGH!JKLMN/PQRST@VWX#Z$.:()?%!-+*/@#';
var i: integer;
    haspunct: boolean;
    P: PAnsiChar;
begin
  repeat
    result := FillRandom(Len);
    haspunct := false;
    P := pointer(result);
    for i := 1 to Len do begin
      P^ := CHARS[ord(P^) mod sizeof(CHARS)];
      if not haspunct and
         not (ord(P^) in [ord('A')..ord('Z'),ord('a')..ord('z'),ord('0')..ord('9')]) then
        haspunct := true;
      inc(P);
    end;
  until (Len<=4) or (haspunct and (LowerCase(result)<>result));
end;

procedure SetMainAESPRNG;
begin
  GlobalLock;
  if MainAESPRNG=nil then
    GarbageCollectorFreeAndNil(MainAESPRNG, TAESPRNG.Create);
  GlobalUnLock;
end;

class function TAESPRNG.Main: TAESPRNG;
begin
  if MainAESPRNG=nil then
    SetMainAESPRNG;
  result := MainAESPRNG;
end;

procedure AFDiffusion(buf,rnd: pointer; size: cardinal);
var sha: TSHA256;
    dig: TSHA256Digest;
    last, iv: cardinal;
    i: integer;
begin
  XorMemory(buf,rnd,size);
  sha.Init;
  last := size div SizeOf(dig);
  for i := 0 to last-1 do begin
    iv := bswap32(i); // host byte order independent hash IV (as in TKS1/LUKS)
    sha.Update(@iv,SizeOf(iv));
    sha.Update(buf,SizeOf(dig));
    sha.Final(PSHA256Digest(buf)^);
    inc(PByte(buf),SizeOf(dig));
  end;
  dec(size,last*SizeOf(dig));
  if size=0 then
    exit;
  iv := bswap32(last);
  sha.Update(@iv,SizeOf(iv));
  sha.Update(buf,size);
  sha.Final(dig);
  MoveSmall(@dig,buf,size);
end;

function TAESPRNG.AFSplit(const Buffer; BufferBytes, StripesCount: integer): RawByteString;
var dst: pointer;
    tmp: TByteDynArray;
    i: integer;
begin
  result := '';
  if self<>nil then
    SetLength(result,BufferBytes*(StripesCount+1));
  if result='' then
    exit;
  dst := pointer(result);
  SetLength(tmp,BufferBytes);
  for i := 1 to StripesCount do begin
    FillRandom(dst,BufferBytes);
    AFDiffusion(pointer(tmp),dst,BufferBytes);
    inc(PByte(dst),BufferBytes);
  end;
  XorMemory(dst,@Buffer,pointer(tmp),BufferBytes);
end;

function TAESPRNG.AFSplit(const Buffer: RawByteString; StripesCount: integer): RawByteString;
begin
  result := AFSplit(pointer(Buffer)^,length(Buffer),StripesCount);
end;

class function TAESPRNG.AFUnsplit(const Split: RawByteString;
  out Buffer; BufferBytes: integer): boolean;
var len: cardinal;
    i: integer;
    src: pointer;
    tmp: TByteDynArray;
begin
  len := length(Split);
  result := (len<>0) and (len mod cardinal(BufferBytes)=0);
  if not result then
    exit;
  src := pointer(Split);
  SetLength(tmp,BufferBytes);
  for i := 2 to len div cardinal(BufferBytes) do begin
    AFDiffusion(pointer(tmp),src,BufferBytes);
    inc(PByte(src),BufferBytes);
  end;
  XorMemory(@Buffer,src,pointer(tmp),BufferBytes);
end;

class function TAESPRNG.AFUnsplit(const Split: RawByteString;
  StripesCount: integer): RawByteString;
var len: cardinal;
begin
  result := '';
  len := length(Split);
  if (len=0) or (len mod cardinal(StripesCount+1)<>0) then
    exit;
  len := len div cardinal(StripesCount+1);
  SetLength(result,len);
  if not AFUnsplit(Split,pointer(result)^,len) then
    result := '';
end;

class procedure TAESPRNG.Fill(Buffer: pointer; Len: integer);
begin
  Main.FillRandom(Buffer,Len);
end;

class procedure TAESPRNG.Fill(out Block: TAESBlock);
begin
  Main.FillRandom(Block);
end;

class procedure TAESPRNG.Fill(out Block: THash256);
begin
  Main.FillRandom(Block);
end;

class function TAESPRNG.Fill(Len: integer): RawByteString;
begin
  result := Main.FillRandom(Len);
end;

class function TAESPRNG.Bytes(Len: integer): TBytes;
begin
  result := Main.FillRandomBytes(Len);
end;


{ TAESPRNGSystem }

constructor TAESPRNGSystem.Create;
begin
  inherited Create(0,0);
end;

procedure TAESPRNGSystem.FillRandom(out Block: TAESBlock);
begin
  FillRandom(@Block,sizeof(Block));
end;

procedure TAESPRNGSystem.FillRandom(Buffer: pointer; Len: integer);
begin
  FillSystemRandom(Buffer,Len,false);
end;

procedure TAESPRNGSystem.Seed;
begin // do nothing
end;


{ TRC4 }

procedure TRC4.Init(const aKey; aKeyLen: integer);
var i,k: integer;
    j,tmp: PtrInt;
begin
  if aKeyLen<=0 then
    raise ESynCrypto.CreateUTF8('TRC4.Init(invalid aKeyLen=%)',[aKeyLen]);
  dec(aKeyLen);
  for i := 0 to high(state) do
    state[i] := i;
  j := 0;
  k := 0;
  for i := 0 to high(state) do begin
    j := (j+state[i]+TByteArray(aKey)[k]) and $ff;
    tmp := state[i];
    state[i] := state[j];
    state[j] := tmp;
    if k>=aKeyLen then // avoid slow mod operation within loop
      k := 0 else
      inc(k);
  end;
  currI := 0;
  currJ := 0;
end;

procedure TRC4.InitSHA3(const aKey; aKeyLen: integer);
var sha: TSHA3;
    dig: array[byte] of byte; // max RC4 state size is 256 bytes
begin
  sha.Full(SHAKE_128,@aKey,aKeyLen,@dig,SizeOf(dig)shl 3); // XOF mode
  Init(dig,SizeOf(dig));
  FillCharFast(dig,SizeOf(dig),0);
  Drop(3072);
end;

procedure TRC4.EncryptBuffer(BufIn, BufOut: PByte; Count: cardinal);
var i,j,ki,kj: PtrInt;
    by4: array[0..3] of byte;
begin
  i := currI;
  j := currJ;
  while Count>3 do begin
    dec(Count,4);
    i := (i+1) and $ff;
    ki := State[i];
    j := (j+ki) and $ff;
    kj := (ki+State[j]) and $ff;
    State[i] := State[j];
    i := (i+1) and $ff;
    State[j] := ki;
    ki := State[i];
    by4[0] := State[kj];
    j := (j+ki) and $ff;
    kj := (ki+State[j]) and $ff;
    State[i] := State[j];
    i := (i+1) and $ff;
    State[j] := ki;
    by4[1] := State[kj];
    ki := State[i];
    j := (j+ki) and $ff;
    kj := (ki+State[j]) and $ff;
    State[i] := State[j];
    i := (i+1) and $ff;
    State[j] := ki;
    by4[2] := State[kj];
    ki := State[i];
    j := (j+ki) and $ff;
    kj := (ki+State[j]) and $ff;
    State[i] := State[j];
    State[j] := ki;
    by4[3] := State[kj];
    PCardinal(BufOut)^ := PCardinal(BufIn)^ xor cardinal(by4);
    inc(BufIn,4);
    inc(BufOut,4);
  end;
  while Count>0 do begin
    dec(Count);
    i := (i+1) and $ff;
    ki := State[i];
    j := (j+ki) and $ff;
    kj := (ki+State[j]) and $ff;
    State[i] := State[j];
    State[j] := ki;
    BufOut^ := BufIn^ xor State[kj];
    inc(BufIn);
    inc(BufOut);
  end;
  currI := i;
  currJ := j;
end;

procedure TRC4.Encrypt(const BufIn; var BufOut; Count: cardinal);
begin
  EncryptBuffer(@BufIn,@BufOut,Count);
end;

procedure TRC4.Drop(Count: cardinal);
var i,j,ki: PtrInt;
begin
  i := currI;
  j := currJ;
  while Count>0 do begin
    dec(Count);
    i := (i+1) and $ff;
    ki := state[i];
    j := (j+ki) and $ff;
    state[i] := state[j];
    state[j] := ki;
  end;
  currI := i;
  currJ := j;
end;

function RC4SelfTest: boolean;
const
  Key: array[0..4] of byte = ($61,$8A,$63,$D2,$FB);
  InDat: array[0..4] of byte = ($DC,$EE,$4C,$F9,$2C);
  OutDat: array[0..4] of byte = ($F1,$38,$29,$C9,$DE);
  Test1: array[0..7] of byte = ($01,$23,$45,$67,$89,$ab,$cd,$ef);
  Res1: array[0..7] of byte = ($75,$b7,$87,$80,$99,$e0,$c5,$96);
  Key2: array[0..3] of byte = ($ef,$01,$23,$45);
  Test2: array[0..9] of byte = (0,0,0,0,0,0,0,0,0,0);
  Res2: array[0..9] of byte = ($d6,$a1,$41,$a7,$ec,$3c,$38,$df,$bd,$61);
var RC4: TRC4;
    Dat: array[0..9] of byte;
    Backup: TRC4;
begin
  RC4.Init(Test1,8);
  RC4.Encrypt(Test1,Dat,8);
  result := CompareMem(@Dat,@Res1,sizeof(Res1));
  RC4.Init(Key2,4);
  RC4.Encrypt(Test2,Dat,10);
  result := result and CompareMem(@Dat,@Res2,sizeof(Res2));
  RC4.Init(Key,sizeof(Key));
  RC4.Encrypt(InDat,Dat,sizeof(InDat));
  result := result and CompareMem(@Dat,@OutDat,sizeof(OutDat));
  RC4.Init(Key,sizeof(Key));
  Backup := RC4;
  RC4.Encrypt(InDat,Dat,sizeof(InDat));
  result := result and CompareMem(@Dat,@OutDat,sizeof(OutDat));
  RC4 := Backup;
  RC4.Encrypt(InDat,Dat,sizeof(InDat));
  result := result and CompareMem(@Dat,@OutDat,sizeof(OutDat));
  RC4 := Backup;
  RC4.Encrypt(OutDat,Dat,sizeof(InDat));
  result := result and CompareMem(@Dat,@InDat,sizeof(OutDat));
end;


procedure CompressShaAesSetKey(const Key: RawByteString; AesClass: TAESAbstractClass);
begin
  if Key='' then
    FillZero(CompressShaAesKey) else
    SHA256Weak(Key,CompressShaAesKey);
end;

function CompressShaAes(var DataRawByteString; Compress: boolean): AnsiString;
var Data: RawByteString absolute DataRawByteString;
begin
  if (Data<>'') and (CompressShaAesClass<>nil) then
  try
    with CompressShaAesClass.Create(CompressShaAesKey,256) do
    try
      if Compress then begin
        CompressSynLZ(Data,true);
        Data := EncryptPKCS7(Data,{IVAtBeginning=}true);
      end else begin
        Data := DecryptPKCS7(Data,{IVAtBeginning=}true);
        if CompressSynLZ(Data,false)='' then begin
          result := '';
          exit; // invalid content
        end;
      end;
    finally
      Free;
    end;
  except
    on Exception do begin // e.g. ESynCrypto in DecryptPKCS7(Data)
      result := '';
      exit; // invalid content
    end;
  end;
  result := 'synshaaes'; // mark success
end;


{ THash128History }

procedure THash128History.Init(size, maxsize: integer);
begin
  Depth := maxsize;
  SetLength(Previous,size);
  Count := 0;
  Index := 0;
end;

function THash128History.Exists(const hash: THash128): boolean;
begin
  if Count = 0 then
    result := false else
    result := Hash128Index(pointer(Previous),Count,@hash)>=0;
end;

function THash128History.Add(const hash: THash128): boolean;
var n: integer;
begin
  result := Hash128Index(pointer(Previous),Count,@hash)<0;
  if not result then
    exit;
  Previous[Index].b := hash;
  inc(Index);
  if Index>=length(Previous) then
    if Index=Depth then
      Index := 0 else begin
      n := NextGrow(Index);
      if n>=Depth then
        n := Depth;
      SetLength(Previous,n);
    end;
  if Count<Depth then
    inc(Count);
end;

{$ifdef MSWINDOWS}
type
  {$ifdef FPC}
  {$PACKRECORDS C} // mandatory under Win64
  {$endif}
  DATA_BLOB = record
    cbData: DWORD;
    pbData: PAnsiChar;
  end;
  PDATA_BLOB = ^DATA_BLOB;
  {$ifdef FPC}
  {$PACKRECORDS DEFAULT}
  {$endif}
const
  CRYPTPROTECT_UI_FORBIDDEN = $1;
  CRYPTDLL = 'Crypt32.dll';

function CryptProtectData(const DataIn: DATA_BLOB; szDataDescr: PWideChar;
  OptionalEntropy: PDATA_BLOB; Reserved, PromptStruct: Pointer; dwFlags: DWORD;
  var DataOut: DATA_BLOB): BOOL; stdcall; external CRYPTDLL name 'CryptProtectData';
function CryptUnprotectData(const DataIn: DATA_BLOB; szDataDescr: PWideChar;
  OptionalEntropy: PDATA_BLOB; Reserved, PromptStruct: Pointer; dwFlags: DWORD;
  var DataOut: DATA_BLOB): Bool; stdcall; external CRYPTDLL name 'CryptUnprotectData';

function CryptDataForCurrentUserDPAPI(const Data,AppSecret: RawByteString; Encrypt: boolean): RawByteString;
var src,dst,ent: DATA_BLOB;
    e: PDATA_BLOB;
    ok: boolean;
begin
  src.pbData := pointer(Data);
  src.cbData := length(Data);
  if AppSecret<>'' then begin
    ent.pbData := pointer(AppSecret);
    ent.cbData := length(AppSecret);
    e := @ent;
  end else
    e := nil;
  if Encrypt then
    ok := CryptProtectData(src,nil,e,nil,nil,CRYPTPROTECT_UI_FORBIDDEN,dst) else
    ok := CryptUnprotectData(src,nil,e,nil,nil,CRYPTPROTECT_UI_FORBIDDEN,dst);
  if ok then begin
    SetString(result,dst.pbData,dst.cbData);
    LocalFree(HLOCAL(dst.pbData));
  end else
    result := '';
end;
{$endif MSWINDOWS}

var
  __h: THash256;
  __hmac: THMAC_SHA256; // initialized from CryptProtectDataEntropy salt

procedure read__h__hmac;
var keyfile: TFileName;
    instance: THash256;
    key,key2,appsec: RawByteString;
begin
  __hmac.Init(@CryptProtectDataEntropy,32);
  SetString(appsec,PAnsiChar(@CryptProtectDataEntropy),32);
  PBKDF2_HMAC_SHA256(appsec,ExeVersion.User,100,instance);
  FillZero(appsec);
  appsec := BinToBase64URI(@instance,15); // local file has 21 chars length
  FormatString({$ifdef MSWINDOWS}'%_%'{$else}'%.syn-%'{$endif},
    [GetSystemPath(spUserData),appsec], string(keyfile)); // .* files are hidden under Linux
  SetString(appsec,PAnsiChar(@instance[15]),17); // use remaining bytes as key
  try
    key := StringFromFile(keyfile);
    if key<>'' then begin
      try
        key2 := TAESCFB.SimpleEncrypt(key,appsec,false,true);
      except
        key2 := ''; // handle decryption error
      end;
      FillZero(key);
      {$ifdef MSWINDOWS}
      key := CryptDataForCurrentUserDPAPI(key2,appsec,false);
      {$else}
      key := key2;
      {$endif}
      if TAESPRNG.AFUnsplit(key,__h,sizeof(__h)) then
        exit; // successfully extracted secret key in __h
    end;
    if FileExists(keyfile) then // allow rewrite of invalid local file
      {$ifdef MSWINDOWS}
      SetFileAttributes(pointer(keyfile),FILE_ATTRIBUTE_NORMAL);
      {$else}
      {$ifdef FPC}fpchmod{$else}chmod{$endif}(pointer(keyfile),S_IRUSR or S_IWUSR);
      {$endif}
    TAESPRNG.Main.FillRandom(__h);
    key := TAESPRNG.Main.AFSplit(__h,sizeof(__h),126);
    {$ifdef MSWINDOWS} // 4KB local file, DPAPI-cyphered but with no DPAPI BLOB layout
    key2 := CryptDataForCurrentUserDPAPI(key,appsec,true);
    FillZero(key);
    {$else} // 4KB local chmod 400 hidden file in $HOME folder under Linux/POSIX
    key2 := key;
    {$endif}
    key := TAESCFB.SimpleEncrypt(key2,appsec,true,true);
    if not FileFromString(key,keyfile) then
      ESynCrypto.CreateUTF8('Unable to write %',[keyfile]);
    {$ifdef MSWINDOWS}
    SetFileAttributes(pointer(keyfile),FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_READONLY);
    {$else}
    {$ifdef FPC}fpchmod{$else}chmod{$endif}(pointer(keyfile),S_IRUSR);
    {$endif}
  finally
    FillZero(key);
    FillZero(key2);
    FillZero(appsec);
    FillZero(instance);
  end;
end;

function CryptDataForCurrentUser(const Data,AppSecret: RawByteString; Encrypt: boolean): RawByteString;
var hmac: THMAC_SHA256;
    secret: THash256;
begin
  result := '';
  if Data='' then
    exit;
  if IsZero(__h) then
    read__h__hmac;
  try
    hmac := __hmac; // thread-safe reuse of CryptProtectDataEntropy salt
    hmac.Update(AppSecret);
    hmac.Update(__h);
    hmac.Done(secret);
    result := TAESCFBCRC.MACEncrypt(Data,secret,Encrypt);
  finally
    FillZero(secret);
  end;
end;


{ TProtocolNone }

function TProtocolNone.ProcessHandshake(
  const MsgIn: RawUTF8; out MsgOut: RawUTF8): TProtocolResult;
begin
  result := sprUnsupported;
end;

function TProtocolNone.Decrypt(const aEncrypted: RawByteString;
  out aPlain: RawByteString): TProtocolResult;
begin
  aPlain := aEncrypted;
  result := sprSuccess;
end;

procedure TProtocolNone.Encrypt(const aPlain: RawByteString;
  out aEncrypted: RawByteString);
begin
  aEncrypted := aPlain;
end;

function TProtocolNone.Clone: IProtocol;
begin
  result := TProtocolNone.Create;
end;


{ TProtocolAES }

constructor TProtocolAES.Create(aClass: TAESAbstractClass; const aKey;
  aKeySize: cardinal; aIVReplayAttackCheck: TAESIVReplayAttackCheck);
begin
  inherited Create;
  fAES[false] := aClass.Create(aKey,aKeySize);
  fAES[false].IVReplayAttackCheck := aIVReplayAttackCheck;
  fAES[true] := fAES[false].Clone;
end;

constructor TProtocolAES.CreateFrom(aAnother: TProtocolAES);
begin
  inherited Create;
  fAES[false] := aAnother.fAES[false].Clone;
  fAES[true] := fAES[false].Clone;
end;

destructor TProtocolAES.Destroy;
begin
  fAES[false].Free;
  fAES[true].Free;
  inherited Destroy;
end;

function TProtocolAES.ProcessHandshake(
  const MsgIn: RawUTF8; out MsgOut: RawUTF8): TProtocolResult;
begin
  result := sprUnsupported;
end;

function TProtocolAES.Decrypt(const aEncrypted: RawByteString;
  out aPlain: RawByteString): TProtocolResult;
begin
  fSafe.Lock;
  try
    try
      aPlain := fAES[false].DecryptPKCS7(aEncrypted,{iv=}true,{raise=}false);
      if aPlain='' then
        result := sprBadRequest else
        result := sprSuccess;
    except
      result := sprInvalidMAC;
    end;
  finally
    fSafe.UnLock;
  end;
end;

procedure TProtocolAES.Encrypt(const aPlain: RawByteString;
  out aEncrypted: RawByteString);
begin
  fSafe.Lock;
  try
    aEncrypted := fAES[true].EncryptPKCS7(aPlain,{IVAtBeginning=}true);
  finally
    fSafe.UnLock;
  end;
end;

function TProtocolAES.Clone: IProtocol;
begin
 result := TProtocolAESClass(ClassType).CreateFrom(self);
end;


{$ifndef NOVARIANTS}

{ TJWTAbstract }

constructor TJWTAbstract.Create(const aAlgorithm: RawUTF8;
  aClaims: TJWTClaims; const aAudience: array of RawUTF8; aExpirationMinutes: integer;
  aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUTF8);
begin
  if aAlgorithm='' then
    raise EJWTException.CreateUTF8('%.Create(algo?)',[self]);
  inherited Create;
  if high(aAudience)>=0 then begin
    fAudience := TRawUTF8DynArrayFrom(aAudience);
    include(aClaims,jrcAudience);
  end;
  if aExpirationMinutes>0 then begin
    include(aClaims,jrcExpirationTime);
    fExpirationSeconds := aExpirationMinutes*60;
  end else
    exclude(aClaims,jrcExpirationTime);
  fAlgorithm := aAlgorithm;
  fClaims := aClaims;
  if jrcJwtID in aClaims then
    fIDGen := TSynUniqueIdentifierGenerator.Create(aIDIdentifier,aIDObfuscationKey);
  if fHeader='' then
    FormatUTF8('{"alg":"%","typ":"JWT"}',[aAlgorithm],fHeader);
  fHeaderB64 := BinToBase64URI(fHeader)+'.';
  fCacheResults := [jwtValid];
end;

destructor TJWTAbstract.Destroy;
begin
  fIDGen.Free;
  fCache.Free;
  inherited;
end;

const
  JWT_MAXSIZE = 4096; // coherent with HTTP headers limitations

function TJWTAbstract.Compute(const DataNameValue: array of const;
  const Issuer, Subject, Audience: RawUTF8; NotBefore: TDateTime;
  ExpirationMinutes: integer; Signature: PRawUTF8): RawUTF8;
var payload, headpayload, signat: RawUTF8;
begin
  result := '';
  if self=nil then
    exit;
  payload := PayloadToJSON(DataNameValue,Issuer,Subject,Audience,NotBefore,ExpirationMinutes);
  headpayload := fHeaderB64+BinToBase64URI(payload);
  signat := ComputeSignature(headpayload);
  result := headpayload+'.'+signat;
  if length(result)>JWT_MAXSIZE then
    raise EJWTException.CreateUTF8('%.Compute oversize: len=%',[self,length(result)]);
  if Signature<>nil then
    Signature^ := signat;
end;

function TJWTAbstract.ComputeAuthorizationHeader(const DataNameValue: array of const;
  const Issuer, Subject, Audience: RawUTF8; NotBefore: TDateTime;
  ExpirationMinutes: integer): RawUTF8;
begin
  if self=nil then
    result := '' else
    result := 'Bearer '+
      Compute(DataNameValue,Issuer,Subject,Audience,NotBefore,ExpirationMinutes);
end;

function TJWTAbstract.PayloadToJSON(const DataNameValue: array of const;
  const Issuer, Subject, Audience: RawUTF8; NotBefore: TDateTime;
  ExpirationMinutes: cardinal): RawUTF8;
  procedure RaiseMissing(c: TJWTClaim);
  begin
    raise EJWTException.CreateUTF8('%.PayloadJSON: missing %', [self, ToText(c)^]);
  end;
var payload: TDocVariantData;
begin
  result := '';
  payload.InitObject(DataNameValue,JSON_OPTIONS_FAST);
  if jrcIssuer in fClaims then
    if Issuer='' then
      RaiseMissing(jrcIssuer) else
      payload.AddValueFromText(JWT_CLAIMS_TEXT[jrcIssuer],Issuer,true);
  if jrcSubject in fClaims then
    if Subject='' then
      RaiseMissing(jrcSubject) else
      payload.AddValueFromText(JWT_CLAIMS_TEXT[jrcSubject],Subject,true);
  if jrcAudience in fClaims then
    if Audience='' then
      RaiseMissing(jrcAudience) else
      if Audience[1]='[' then
        payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcAudience],_JsonFast(Audience)) else
        payload.AddValueFromText(JWT_CLAIMS_TEXT[jrcAudience],Audience,true);
  if jrcNotBefore in fClaims then
    if NotBefore<=0 then
      payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcNotBefore],UnixTimeUTC) else
      payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcNotBefore],DateTimeToUnixTime(NotBefore));
  if jrcIssuedAt in fClaims then
    payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcIssuedAt],UnixTimeUTC);
  if jrcExpirationTime in fClaims then begin
    if ExpirationMinutes=0 then
      ExpirationMinutes := fExpirationSeconds else
      ExpirationMinutes := ExpirationMinutes*60;
    payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcExpirationTime],UnixTimeUTC+ExpirationMinutes);
  end;
  if jrcJwtID in fClaims then
    if joNoJwtIDGenerate in fOptions then begin
      if payload.GetValueIndex(JWT_CLAIMS_TEXT[jrcJwtID])<0 then
        exit; // not generated, but should be supplied
    end else
      payload.AddValueFromText(JWT_CLAIMS_TEXT[jrcJwtID],fIDGen.ToObfuscated(fIDGen.ComputeNew));
  result := payload.ToJSON;
end;

procedure TJWTAbstract.SetCacheTimeoutSeconds(value: integer);
begin
  fCacheTimeoutSeconds := value;
  FreeAndNil(fCache);
  if (value>0) and (fCacheResults<>[]) then
    fCache := TSynDictionary.Create(TypeInfo(TRawUTF8DynArray),
      TypeInfo(TJWTContentDynArray),false,value);
end;

procedure TJWTAbstract.Verify(const Token: RawUTF8; out JWT: TJWTContent;
  ExcludedClaims: TJWTClaims);
var headpayload: RawUTF8;
    signature: RawByteString;
    fromcache: boolean;
begin
  JWT.result := jwtNoToken;
  if (self=nil) or (fCache=nil) then
    fromcache := false else begin
    fromcache := fCache.FindAndCopy(Token,JWT);
    fCache.DeleteDeprecated;
  end;
  if not fromcache then
    Parse(Token,JWT,headpayload,signature,ExcludedClaims);
  if JWT.result in [jwtValid,jwtNotBeforeFailed] then
    if CheckAgainstActualTimestamp(JWT) and not fromcache then
      CheckSignature(headpayload,signature,JWT); // depending on the algorithm used
  if not fromcache and (self<>nil) and (fCache<>nil) and (JWT.result in fCacheResults) then
    fCache.Add(Token,JWT);
end;

function TJWTAbstract.Verify(const Token: RawUTF8): TJWTResult;
var jwt: TJWTContent;
begin
  Verify(Token,jwt);
  result := jwt.result;
end;

function TJWTAbstract.CheckAgainstActualTimestamp(var JWT: TJWTContent): boolean;
var nowunix, unix: cardinal;
begin
  if [jrcExpirationTime,jrcNotBefore,jrcIssuedAt]*JWT.claims<>[] then begin
    result := false;
    nowunix := UnixTimeUTC; // validate against actual timestamp
    if jrcExpirationTime in JWT.claims then
      if not ToCardinal(JWT.reg[jrcExpirationTime],unix) or (nowunix>unix) then begin
        JWT.result := jwtExpired;
        exit;
      end;
    if jrcNotBefore in JWT.claims then
      if not ToCardinal(JWT.reg[jrcNotBefore],unix) or (nowunix<unix) then begin
        JWT.result := jwtNotBeforeFailed;
        exit;
      end;
    if jrcIssuedAt in JWT.claims then // allow 1 minute time lap between nodes
      if not ToCardinal(JWT.reg[jrcIssuedAt],unix) or (unix>nowunix+60) then begin
        JWT.result := jwtInvalidIssuedAt;
        exit;
      end;
  end;
  result := true;
  JWT.result := jwtValid;
end;

procedure TJWTAbstract.Parse(const Token: RawUTF8; var JWT: TJWTContent;
  out headpayload: RawUTF8; out signature: RawByteString; excluded: TJWTClaims);
var payloadend,j,toklen,c,cap,headerlen,len,a: integer;
    P: PUTF8Char;
    N,V: PUTF8Char;
    wasString: boolean;
    EndOfObject: AnsiChar;
    claim: TJWTClaim;
    requiredclaims: TJWTClaims;
    id: TSynUniqueIdentifierBits;
    value: variant;
    payload: RawUTF8;
    head: array[0..1] of TValuePUTF8Char;
    aud: TDocVariantData;
    tok: PAnsiChar absolute Token;
begin
  // 0. initialize parsing
  Finalize(JWT.reg);
  JWT.data.InitFast(0,dvObject); // custom claims
  byte(JWT.claims) := 0;
  word(JWT.audience) := 0;
  toklen := length(Token);
  if (toklen=0) or (self=nil) then begin
    JWT.result := jwtNoToken;
    exit;
  end;
  // 1. validate the header (including algorithm "alg" verification)
  JWT.result := jwtInvalidAlgorithm;
  if joHeaderParse in fOptions then begin // slower parsing
    headerlen := PosExChar('.',Token);
    if (headerlen=0) or (headerlen>512) then
      exit;
    Base64URIToBin(tok,headerlen-1,signature);
    JSONDecode(pointer(signature),['alg','typ'],@head);
    if not head[0].Idem(fAlgorithm) or
       ((head[1].Value<>nil) and not head[1].Idem('JWT')) then
      exit;
  end else begin // fast direct compare of fHeaderB64 (including "alg")
    headerlen := length(fHeaderB64);
    if (toklen<=headerlen) or not CompareMem(pointer(fHeaderB64),tok,headerlen) then
      exit;
  end;
  // 2. extract the payload
  JWT.result := jwtWrongFormat;
  if toklen>JWT_MAXSIZE Then
    exit;
  payloadend := PosEx('.',Token,headerlen+1);
  if (payloadend=0) or (payloadend-headerlen>2700) then
    exit;
  Base64URIToBin(tok+payloadend,toklen-payloadend,signature);
  if (signature='') and (payloadend<>toklen) then
    exit;
  JWT.result := jwtInvalidPayload;
  Base64URIToBin(tok+headerlen,payloadend-headerlen-1,RawByteString(payload));
  if payload='' then
    exit;
  // 3. decode the payload into JWT.reg[]/JWT.claims (known) and JWT.data (custom)
  P := GotoNextNotSpace(pointer(payload));
  if P^<>'{' then
    exit;
  P := GotoNextNotSpace(P+1);
  cap := JSONObjectPropCount(P);
  if cap<0 then
    exit;
  requiredclaims := fClaims - excluded;
  if cap>0 then
  repeat
    N := GetJSONPropName(P);
    if N=nil then
      exit;
    V := GetJSONFieldOrObjectOrArray(P,@wasstring,@EndOfObject,true);
    if V=nil then
      exit;
    len := StrLen(N);
    if len=3 then begin
      c := PInteger(N)^;
      for claim := low(claim) to high(claim) do
        if PInteger(JWT_CLAIMS_TEXT[claim])^=c then begin
          if V^=#0 then
            exit;
          include(JWT.claims,claim);
          if not(claim in fClaims) and not(joAllowUnexpectedClaims in fOptions) then begin
            JWT.result := jwtUnexpectedClaim;
            exit;
          end;
          FastSetString(JWT.reg[claim],V,StrLen(V));
          if claim in requiredclaims then
          case claim of
          jrcJwtID:
            if not(joNoJwtIDCheck in fOptions) then
              if not fIDGen.FromObfuscated(JWT.reg[jrcJwtID],id.Value) or
                 (id.CreateTimeUnix<UNIXTIME_MINIMAL) then begin
                JWT.result := jwtInvalidID;
                exit;
              end;
          jrcAudience:
            if JWT.reg[jrcAudience][1]='[' then begin
              aud.InitJSON(JWT.reg[jrcAudience],JSON_OPTIONS_FAST);
              if aud.Count=0 then
                exit;
              for j := 0 to aud.Count-1 do begin
                a := FindRawUTF8(fAudience,VariantToUTF8(aud.Values[j]));
                if a<0 then begin
                  JWT.result := jwtUnknownAudience;
                  if not (joAllowUnexpectedAudience in fOptions) then
                    exit;
                end else
                  include(JWT.audience,a);
              end;
              aud.Clear;
            end else begin
              a := FindRawUTF8(fAudience,JWT.reg[jrcAudience]);
              if a<0 then begin
                JWT.result := jwtUnknownAudience;
                if not (joAllowUnexpectedAudience in fOptions) then
                  exit;
              end else
                include(JWT.audience,a);
            end;
          end;
          len := 0; // don't add to JWT.data
          dec(cap);
          break;
        end;
      if len=0 then
        continue;
    end;
    GetVariantFromJSON(V,wasString,value,@JSON_OPTIONS[true],joDoubleInData in fOptions);
    if JWT.data.Count=0 then
      JWT.data.Capacity := cap;
    JWT.data.AddValue(N,len,value)
  until EndOfObject='}';
  if JWT.data.Count>0 then
    JWT.data.Capacity := JWT.data.Count;
  if requiredclaims-JWT.claims<>[] then
    JWT.result := jwtMissingClaim else begin
    FastSetString(headpayload,tok,payloadend-1);
    JWT.result := jwtValid;
  end;
end;

function TJWTAbstract.VerifyAuthorizationHeader(const HttpAuthorizationHeader: RawUTF8;
  out JWT: TJWTContent): boolean;
begin
  if (cardinal(length(HttpAuthorizationHeader)-10)>4096) or
     not IdemPChar(pointer(HttpAuthorizationHeader), 'BEARER ') then
    JWT.result := jwtWrongFormat else
    Verify(copy(HttpAuthorizationHeader,8,maxInt),JWT);
  result := JWT.result=jwtValid;
end;

class function TJWTAbstract.VerifyPayload(const Token, ExpectedSubject, ExpectedIssuer,
  ExpectedAudience: RawUTF8; Expiration: PUnixTime; Signature: PRawUTF8; Payload: PVariant;
  IgnoreTime: boolean; NotBeforeDelta: TUnixTime): TJWTResult;
var P,B: PUTF8Char;
    V: array[0..4] of TValuePUTF8Char;
    now, time: PtrUInt;
    text: RawUTF8;
begin
  result := jwtInvalidAlgorithm;
  B := pointer(Token);
  P := PosChar(B,'.');
  if P=nil then
    exit;
  if self<>TJWTAbstract then begin
    text := Base64URIToBin(PAnsiChar(B),P-B);
    if not IdemPropNameU(copy(ToText(self),5,10),JSONDecode(text,'alg')) then
      exit;
  end;
  B := P+1;
  P := PosChar(B,'.');
  result := jwtInvalidSignature;
  if P=nil then
    exit;
  result := jwtInvalidPayload;
  text := Base64URIToBin(PAnsiChar(B),P-B);
  if text='' then
    exit;
  if Payload<>nil then
    _Json(text,Payload^,JSON_OPTIONS_FAST);
  JSONDecode(pointer(text),['iss','aud','exp','nbf','sub'],@V,true);
  result := jwtUnexpectedClaim;
  if ((ExpectedSubject<>'') and not V[4].Idem(ExpectedSubject)) or
     ((ExpectedIssuer<>'') and not V[0].Idem(ExpectedIssuer)) then
    exit;
  result := jwtUnknownAudience;
  if (ExpectedAudience<>'') and not V[1].Idem(ExpectedAudience) then
    exit;
  if Expiration<>nil then
    Expiration^ := 0;
  if (V[2].Value<>nil) or (V[3].Value<>nil) then begin
    now := UnixTimeUTC;
    if V[2].Value<>nil then begin
      time := V[2].ToCardinal;
      result := jwtExpired;
      if not IgnoreTime and (now>time) then
        exit;
      if Expiration<>nil then
        Expiration^ := time;
    end;
    if not IgnoreTime and (V[3].Value<>nil) then begin
      time := V[3].ToCardinal;
      result := jwtNotBeforeFailed;
      if (time=0) or (now+PtrUInt(NotBeforeDelta)<time) then
        exit;
    end;
  end;
  inc(P);
  if Signature<>nil then
    FastSetString(Signature^,P,StrLen(P));
  result := jwtValid;
end;


{ TJWTNone }

constructor TJWTNone.Create(aClaims: TJWTClaims;
  const aAudience: array of RawUTF8; aExpirationMinutes: integer;
  aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUTF8);
begin
  fHeader := '{"alg":"none"}'; // "typ":"JWT" is optional, so we save a few bytes
  inherited Create('none',aClaims,aAudience,aExpirationMinutes,
    aIDIdentifier,aIDObfuscationKey);
end;

procedure TJWTNone.CheckSignature(const headpayload: RawUTF8; const signature: RawByteString;
  var JWT: TJWTContent);
begin
  if signature='' then // JWA defined empty string for "none" JWS
    JWT.result := jwtValid else
    JWT.result := jwtInvalidSignature;
end;

function TJWTNone.ComputeSignature(const headpayload: RawUTF8): RawUTF8;
begin
  result := '';
end;


{ TJWTSynSignerAbstract }

constructor TJWTSynSignerAbstract.Create(const aSecret: RawUTF8;
  aSecretPBKDF2Rounds: integer; aClaims: TJWTClaims; const aAudience: array of RawUTF8;
  aExpirationMinutes: integer; aIDIdentifier: TSynUniqueIdentifierProcess;
  aIDObfuscationKey: RawUTF8; aPBKDF2Secret: PHash512Rec);
var algo: TSignAlgo;
begin
  algo := GetAlgo;
  inherited Create(JWT_TEXT[algo],aClaims,aAudience,
    aExpirationMinutes,aIDIdentifier,aIDObfuscationKey);
  if (aSecret<>'') and (aSecretPBKDF2Rounds>0) then
    fSignPrepared.Init(algo,aSecret,fHeaderB64,aSecretPBKDF2Rounds,aPBKDF2Secret) else
    fSignPrepared.Init(algo,aSecret);
end;

procedure TJWTSynSignerAbstract.CheckSignature(const headpayload: RawUTF8;
  const signature: RawByteString; var JWT: TJWTContent);
var signer: TSynSigner;
    temp: THash512Rec;
begin
  JWT.result := jwtInvalidSignature;
  if length(signature)<>SignatureSize then
    exit;
  signer := fSignPrepared; // thread-safe re-use of prepared TSynSigner
  signer.Update(pointer(headpayload),length(headpayload));
  signer.Final(temp);
{  writeln('payload=',headpayload);
  writeln('sign=',bintohex(@temp,SignatureSize));
  writeln('expected=',bintohex(pointer(signature),SignatureSize)); }
  if CompareMem(@temp,pointer(signature),SignatureSize) then
    JWT.result := jwtValid;
end;

function TJWTSynSignerAbstract.ComputeSignature(const headpayload: RawUTF8): RawUTF8;
var signer: TSynSigner;
    temp: THash512Rec;
begin
  signer := fSignPrepared;
  signer.Update(pointer(headpayload),length(headpayload));
  signer.Final(temp);
  result := BinToBase64URI(@temp,SignatureSize);
end;

destructor TJWTSynSignerAbstract.Destroy;
begin
  FillCharFast(fSignPrepared,SizeOf(fSignPrepared),0);
  inherited Destroy;
end;

{ TJWTHS256 }

function TJWTHS256.GetAlgo: TSignAlgo;
begin
  result := saSha256;
end;

{ TJWTHS384 }

function TJWTHS384.GetAlgo: TSignAlgo;
begin
  result := saSha384;
end;

{ TJWTHS512 }

function TJWTHS512.GetAlgo: TSignAlgo;
begin
  result := saSha512;
end;

{ TJWTS3224 }

function TJWTS3224.GetAlgo: TSignAlgo;
begin
  result := saSha3224;
end;

{ TJWTS3256 }

function TJWTS3256.GetAlgo: TSignAlgo;
begin
  result := saSha3256;
end;

{ TJWTS3384 }

function TJWTS3384.GetAlgo: TSignAlgo;
begin
  result := saSha3384;
end;

{ TJWTS3512 }

function TJWTS3512.GetAlgo: TSignAlgo;
begin
  result := saSha3512;
end;

{ TJWTS3S128 }

function TJWTS3S128.GetAlgo: TSignAlgo;
begin
  result := saSha3S128;
end;

{ TJWTS3S256 }

function TJWTS3S256.GetAlgo: TSignAlgo;
begin
  result := saSha3S256;
end;

var
  _TJWTResult: array[TJWTResult] of PShortString;
  _TJWTClaim: array[TJWTClaim] of PShortString;

function ToText(res: TJWTResult): PShortString;
begin
  result := _TJWTResult[res];
end;

function ToCaption(res: TJWTResult): string;
begin
  GetCaptionFromTrimmed(_TJWTResult[res],result);
end;

function ToText(claim: TJWTClaim): PShortString;
begin
  result := _TJWTClaim[claim];
end;

function ToText(claims: TJWTClaims): ShortString;
begin
  GetSetNameShort(TypeInfo(TJWTClaims),claims,result);
end;

{$endif NOVARIANTS}

{$ifdef CRC32C_X64}
{ ISCSI CRC 32 Implementation with crc32 and pclmulqdq Instruction
  Copyright(c) 2011-2015 Intel Corporation All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in
   the documentation and/or other materials provided with the
   distribution.
 * Neither the name of Intel Corporation nor the names of its
   contributors may be used to endorse or promote products derived
   from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICESLOSS OF USE,
 DATA, OR PROFITSOR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. }

{$ifdef FPC}
  {$ifdef MSWINDOWS}
    {$L crc32c64.obj}
  {$else}
    {$L static/x86_64-linux/crc32c64.o}
  {$endif}
{$else}
  {$L crc32c64.obj}
{$endif}
// defined in SynCrypto.pas, not in SynCommons.pas, to avoid .o/.obj dependencies

function crc32_iscsi_01(buf: PAnsiChar; len: PtrUInt; crc: cardinal): cardinal; {$ifdef FPC}cdecl;{$endif} external;

function crc32c_sse42_aesni(crc: PtrUInt; buf: PAnsiChar; len: PtrUInt): cardinal;
{$ifdef FPC}nostackframe; assembler; asm{$else}asm .noframe {$endif}
        mov     rax, crc
        mov     rcx, len
        not     eax
        test    buf, buf
        jz      @z
        cmp     len, 64
        jb      @sml
        // our  call: rcx/rdi=crc rdx/rsi=buf r8/rdx=len
        // iscsi_01:  rcx/rdi=buf rdx/rsi=len r8/rdx=crc
        mov     crc, buf
        mov     buf, len
        mov     len, rax
        call    crc32_iscsi_01
@z:     not     eax
        ret
@sml:   shr     len, 3
        jz      @2
{$ifdef FPC} align 16
@s:     crc32   rax, qword [buf] // hash 8 bytes per loop
{$else} @s:     db $F2,$48,$0F,$38,$F1,$02 // circumvent Delphi inline asm compiler bug
{$endif}add     buf, 8
        dec     len
        jnz     @s
@2:     test    cl, 4
        jz      @3
        crc32   eax, dword ptr[buf]
        add     buf, 4
@3:     test    cl, 2
        jz      @1
        crc32   eax, word ptr[buf]
        add     buf, 2
@1:     test    cl, 1
        jz      @0
        crc32   eax, byte ptr[buf]
@0:     not     eax
end;

{$endif CRC32C_X64}

initialization
  ComputeAesStaticTables;
{$ifdef USEPADLOCK}
  PadlockInit;
{$endif USEPADLOCK}
{$ifdef CPUX64}
  {$ifdef CRC32C_X64} // use SSE4.2+pclmulqdq instructions
    if (cfSSE42 in CpuFeatures) and (cfAesNi in CpuFeatures) then
      crc32c := @crc32c_sse42_aesni;
  {$endif CRC32C_X64}
  if cfSSE41 in CpuFeatures then begin // optimized Intel's sha256_sse4.asm
    if K256AlignedStore='' then
      GetMemAligned(K256AlignedStore,@K256,SizeOf(K256),K256Aligned);
    if PtrUInt(K256Aligned) and 15<>0 then
      K256AlignedStore := ''; // if not properly aligned -> fallback to pascal
  end;
{$endif CPUX64}
  TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TSignAlgo));
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TSynSignerParams),
    'algo:TSignAlgo secret,salt:RawUTF8 rounds:integer');
  {$ifndef NOVARIANTS}
  GetEnumNames(TypeInfo(TJWTResult),@_TJWTResult);
  GetEnumNames(TypeInfo(TJWTClaim),@_TJWTClaim);
  {$endif NOVARIANTS}
  assert(sizeof(TMD5Buf)=sizeof(TMD5Digest));
  assert(sizeof(TAESContext)=AESContextSize);
  assert(AESContextSize<=300); // see synsqlite3.c KEYLENGTH
  assert(sizeof(TSHAContext)=SHAContextSize);
  assert(sizeof(TSHA3Context)=SHA3ContextSize);
  assert(1 shl AESBlockShift=sizeof(TAESBlock));
  assert(sizeof(TAESFullHeader)=sizeof(TAESBlock));
  assert(sizeof(TAESIVCTR)=sizeof(TAESBlock));
  assert(sizeof(TSHA256)=sizeof(TSHA1));
  assert(sizeof(TSHA512)>sizeof(TSHA256));
  assert(sizeof(TSHA3)>sizeof(TSHA512));
  assert(sizeof(TSHA3)>sizeof(THMAC_SHA512));

finalization
{$ifdef USEPADLOCKDLL}
  if PadLockLibHandle<>0 then
    FreeLibrary(PadLockLibHandle); // same on Win+Linux, thanks to SysUtils
{$endif USEPADLOCKDLL}
  FillZero(__h);
{$ifdef MSWINDOWS}
  if CryptoAPI.Handle<>0 then begin
    {$ifdef USE_PROV_RSA_AES}
    if (CryptoAPIAESProvider<>nil) and (CryptoAPIAESProvider<>HCRYPTPROV_NOTTESTED) then
      CryptoAPI.ReleaseContext(CryptoAPIAESProvider,0);
    {$endif USE_PROV_RSA_AES}
    FreeLibrary(CryptoAPI.Handle);
  end;
{$endif MSWINDOWS}
end.
