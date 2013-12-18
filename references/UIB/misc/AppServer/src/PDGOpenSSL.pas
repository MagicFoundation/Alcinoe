{$DEFINE AES_LONG}

unit PDGOpenSSL;

{$define HEADER_STACK_H}
{$define HEADER_OPENSSLCONF_H}
{$define HEADER_AES_H}
{$define HEADER_E_OS2_H}
{$define HEADER_SAFESTACK_H}
{$define HEADER_OPENSSLV_H}
{$define HEADER_OSSL_TYP_H}
{$define HEADER_EBCDIC_H}
{$define HEADER_SYMHACKS_H}
{$define HEADER_CRYPTO_H}
{$define HEADER_BIO_H}
{$define HEADER_BN_H}
{$define HEADER_ASN1_H}
{$define HEADER_ASN1T_H}
{$define HEADER_ASN1_MAC_H}
{$define HEADER_BLOWFISH_H}
{$define HEADER_BUFFER_H}
{$define HEADER_CAST_H}
{$define HEADER_COMP_H}
{$define HEADER_LHASH_H}
{$define HEADER_CONF_H}
{$define HEADER_CONF_API_H}
{$define HEADER_UI_H}
{$define HEADER_UI_COMPAT_H}
{$define HEADER_DES_OLD_H}
{$define HEADER_DES_H}
{$define HEADER_DH_H}
{$define HEADER_DSA_H}
{$define HEADER_DSO_H}
{$define HEADER_PQ_COMPAT_H}
{$define HEADER_PQUEUE_H}
{$define HEADER_DTLS1_H}
{$define HEADER_EC_H}
{$define HEADER_ECDH_H}
{$define HEADER_ECDSA_H}
{$define HEADER_RSA_H}
{$define HEADER_RAND_H}
{$define HEADER_EVP_H}
{$define HEADER_SHA_H}
{$define HEADER_PKCS7_H}
{$define HEADER_X509_VFY_H}
{$define HEADER_X509_H}
{$define HEADER_X509V3_H}
{$define HEADER_STORE_H}
{$define HEADER_ERR_H}
{$define HEADER_ENGINE_H}
{$define HEADER_HMAC_H}
{$define HEADER_IDEA_H}
{$define HEADER_KRB5_ASN_H}
{$define HEADER_KSSL_H}
{$define HEADER_MD2_H}
{$define HEADER_MD4_H}
{$define HEADER_MD5_H}
{$define HEADER_OBJ_MAC_H}
{$define HEADER_OBJECTS_H}
{$define HEADER_OCSP_H}
{$define HEADER_PEM2_H}
{$define HEADER_PEM_H}
{$define HEADER_PKCS12_H}
{$define HEADER_RC2_H}
{$define HEADER_RC4_H}
{$define HEADER_RIPEMD_H}
{$define HEADER_SSL2_H}
{$define HEADER_TLS1_H}
{$define HEADER_SSL23_H}
{$define HEADER_SSL3_H}
{$define HEADER_SSL_H}
{$define HEADER_TMDIFF_H}
{$define HEADER_TXT_DB_H}

interface
uses Classes;

const
  LIBEAY = 'libeay32.dll';

(*******************************************************************************
 * opensslconf
 ******************************************************************************)

(* OpenSSL was configured with the following options: *)

{$define OPENSSL_SYSNAME_WIN64A}
{$ifndef OPENSSL_DOING_MAKEDEPEND}
  {$define OPENSSL_NO_CAMELLIA}
  {$define OPENSSL_NO_CAPIENG}
  {$define OPENSSL_NO_CMS}
  {$define OPENSSL_NO_GMP}
  {$define OPENSSL_NO_JPAKE}
  {$define OPENSSL_NO_KRB5}
  {$define OPENSSL_NO_MDC2}
  {$define OPENSSL_NO_RC5}
  {$define OPENSSL_NO_RFC3779}
  {$define OPENSSL_NO_SEED}
{$endif} (* OPENSSL_DOING_MAKEDEPEND *)

{$define OPENSSL_THREADS}

(* The OPENSSL_NO_* macros are also defined as NO_* if the application
   asks for it.  This is a transient feature that is provided for those
   who haven't had the time to do the appropriate changes in their
   applications.  *)

{$ifdef OPENSSL_ALGORITHM_DEFINES}
  {$if defined(OPENSSL_NO_CAMELLIA) and not defined(NO_CAMELLIA)}
    {$define NO_CAMELLIA}
  {$ifend}
  {$if defined(OPENSSL_NO_CAPIENG) and not defined(NO_CAPIENG)}
    {$define NO_CAPIENG}
  {$ifend}
  {$if defined(OPENSSL_NO_CMS) and not defined(NO_CMS)}
    {$define NO_CMS}
  {$ifend}
  {$if defined(OPENSSL_NO_GMP) and not defined(NO_GMP)}
    {$define NO_GMP}
  {$ifend}
  {$if defined(OPENSSL_NO_JPAKE) and not defined(NO_JPAKE)}
    {$define NO_JPAKE}
  {$ifend}
  {$if defined(OPENSSL_NO_KRB5) and not defined(NO_KRB5)}
    {$define NO_KRB5}
  {$ifend}
  {$if defined(OPENSSL_NO_MDC2) and not defined(NO_MDC2)}
    {$define NO_MDC2}
  {$ifend}
  {$if defined(OPENSSL_NO_RC5) and not defined(NO_RC5)}
    {$define NO_RC5}
  {$ifend}
  {$if defined(OPENSSL_NO_RFC3779) and not defined(NO_RFC3779)}
    {$define NO_RFC3779}
  {$ifend}
  {$if defined(OPENSSL_NO_SEED) and not defined(NO_SEED)}
    {$define NO_SEED}
  {$ifend}
{$endif}

(* crypto/opensslconf.h.in *)

{$ifdef OPENSSL_DOING_MAKEDEPEND}

(* Include any symbols here that have to be explicitly set to enable a feature
 * that should be visible to makedepend.
 *
 * [Our "make depend" doesn't actually look at this, we use actual build settings
 * instead; we want to make it easy to remove subdirectories with disabled algorithms.]
 *)

  {$define OPENSSL_FIPS}
{$endif}

(* Generate 80386 code? *)
{$undef I386_ONLY}

//#if !(defined(VMS) || defined(__VMS)) (* VMS uses logical names instead *)
//#if defined(HEADER_CRYPTLIB_H) && !defined(OPENSSLDIR)
//#define ENGINESDIR "/usr/local/ssl/lib/engines"
//#define OPENSSLDIR "/usr/local/ssl"
//#endif
//#endif

//#undef OPENSSL_UNISTD
//#define OPENSSL_UNISTD <unistd.h>

{$define OPENSSL_EXPORT_VAR_AS_FUNCTION}

{$if defined(HEADER_IDEA_H) and not defined(IDEA_INT)}
type
  IDEA_INT = Cardinal;
{$ifend}

{$if defined(HEADER_MD2_H) and not defined(MD2_INT)}
type
  MD2_INT = Cardinal;
{$ifend}

{$if defined(HEADER_RC2_H) and not defined(RC2_INT)}
(* I need to put in a mod for the alpha - eay *)
type
  RC2_INT = Cardinal;
{$ifend}

{$if defined(HEADER_RC4_H)}
{$if not defined(RC4_INT)}
(* using int types make the structure larger but make the code faster
 * on most boxes I have tested - up to %20 faster. *)
(*
 * I don't know what does "most" mean, but declaring "int" is a must on:
 * - Intel P6 because partial register stalls are very expensive;
 * - elder Alpha because it lacks byte load/store instructions;
 *)
type
  RC4_INT = Cardinal;
{$ifend}
{$if not defined(RC4_CHUNK)}
(*
 * This enables code handling data aligned at natural CPU word
 * boundary. See crypto/rc4/rc4_enc.c for further details.
 *)
type
  RC4_CHUNK = UInt64;
{$ifend}
{$ifend}

{$if (defined(HEADER_NEW_DES_H) or defined(HEADER_DES_H)) and not defined(DES_LONG)}
(* If this is set to 'unsigned int' on a DEC Alpha, this gives about a
 * %20 speed up (longs are 8 bytes, int's are 4). *)
{$ifndef DES_LONG}
type
  DES_LONG = Cardinal;
{$endif}
{$ifend}

{$if defined(HEADER_BN_H) and not defined(CONFIG_HEADER_BN_H)}
{$define CONFIG_HEADER_BN_H}
{$undef BN_LLONG}

(* Should we define BN_DIV2W here? *)

(* Only one for the following should be defined *)
(* The prime number generation stuff may not work when
 * EIGHT_BIT but I don't care since I've only used this mode
 * for debuging the bignum libraries *)
{$undef SIXTY_FOUR_BIT_LONG}
{$define SIXTY_FOUR_BIT}
{$undef THIRTY_TWO_BIT}
{$undef SIXTEEN_BIT}
{$undef EIGHT_BIT}
{$ifend}

{$if defined(HEADER_RC4_LOCL_H) and not defined(CONFIG_HEADER_RC4_LOCL_H)}
{$define CONFIG_HEADER_RC4_LOCL_H}
(* if this is defined data[i] is used instead of *data, this is a %20
 * speedup on x86 *)
{$undef RC4_INDEX}
{$ifend}

{$if defined(HEADER_BF_LOCL_H) and not defined(CONFIG_HEADER_BF_LOCL_H)}
{$define CONFIG_HEADER_BF_LOCL_H}
{$undef BF_PTR}
{$ifend} (* HEADER_BF_LOCL_H *)

{$if defined(HEADER_DES_LOCL_H) and not defined(CONFIG_HEADER_DES_LOCL_H)}
{$define CONFIG_HEADER_DES_LOCL_H}
{$ifndef DES_DEFAULT_OPTIONS}
(* the following is tweaked from a config script, that is why it is a
 * protected undef/define *)
{$ifndef DES_PTR}
{$undef DES_PTR}
{$endif}

(* This helps C compiler generate the correct code for multiple functional
 * units.  It reduces register dependancies at the expense of 2 more
 * registers *)
{$ifndef DES_RISC1}
{$undef DES_RISC1}
{$endif}

{$ifndef DES_RISC2}
{$undef DES_RISC2}
{$endif}

{$if defined(DES_RISC1) and defined(DES_RISC2)}
YOU SHOULD NOT HAVE BOTH DES_RISC1 AND DES_RISC2 DEFINED!!!!!
{$ifend}

(* Unroll the inner loop, this sometimes helps, sometimes hinders.
 * Very mucy CPU dependant *)
{$ifndef DES_UNROLL}
{$undef DES_UNROLL}
{$endif}

(* These default values were supplied by
 * Peter Gutman <pgut001@cs.auckland.ac.nz>
 * They are only used if nothing else has been defined *)
{$if not defined(DES_PTR) and not defined(DES_RISC1) and not defined(DES_RISC2) and not defined(DES_UNROLL)}
(* Special defines which change the way the code is built depending on the
   CPU and OS.  For SGI machines you can use _MIPS_SZLONG (32 or 64) to find
   even newer MIPS CPU's, but at the moment one size fits all for
   optimization options.  Older Sparc's work better with only UNROLL, but
   there's no way to tell at compile time what it is you're running on *)

{$if defined( sun )}  (* Newer Sparc's *)
  {$define DES_PTR}
  {$define DES_RISC1}
  {$define DES_UNROLL}
{$elif defined( __ultrix )} (* Older MIPS *)
  {$define DES_PTR}
  {$define DES_RISC2}
  {$define DES_UNROLL}
{$elif defined( __osf1__ )} (* Alpha *)
  {$define DES_PTR}
  {$define DES_RISC2}
{$elif defined ( _AIX )}  (* RS6000 *)
  (* Unknown *)
{$elif defined( __hpux )}  (* HP-PA *)
  (* Unknown *)
{$elif defined( __aux )}  (* 68K *)
  (* Unknown *)
{$elif defined( __dgux )}  (* 88K (but P6 in latest boxes) *)
  {$define DES_UNROLL}
{$elif defined( __sgi )}  (* Newer MIPS *)
  {$define DES_PTR}
  {$define DES_RISC2}
  {$define DES_UNROLL}
{$elif defined(i386) || defined(__i386__) (* x86 boxes, should be gcc *)
  {$define DES_PTR}
  {$define DES_RISC1}
  {$define DES_UNROLL}
{$ifend} (* Systems-specific speed defines *)
{$ifend}

{$endif} (* DES_DEFAULT_OPTIONS *)
{$ifend} (* HEADER_DES_LOCL_H *)


(*******************************************************************************
 * stack
 ******************************************************************************)

{$ifdef HEADER_STACK_H}

type
  fn_stack_cmp = function(const p1, p2: PPAnsiChar): Integer; cdecl;
  fn_sk_pop_free = procedure(p: Pointer); cdecl;

  PSTACK = ^STACK;
  stack_st = record
   num: integer;
   data: PPAnsiChar;
   sorted: Integer;
   num_alloc: integer;
   comp: fn_stack_cmp;
  end;
  STACK = stack_st;

//#define M_sk_num(sk)  ((sk) ? (sk)->num:-1)
//#define M_sk_value(sk,n) ((sk) ? (sk)->data[n] : NULL)

function sk_num(const PSTACK ): Integer; cdecl; external LIBEAY;
function sk_value(const p: PSTACK; i: Integer): PAnsiChar; cdecl; external LIBEAY;
function sk_set(p: PSTACK; i: integer; c: PAnsiChar): PAnsiChar; cdecl; external LIBEAY;
function sk_new(cmp: fn_stack_cmp): PSTACK; cdecl; external LIBEAY;
function sk_new_null(): PSTACK; cdecl; external LIBEAY;
procedure sk_free(p: PSTACK); cdecl; external LIBEAY;
procedure sk_pop_free(st: PSTACK; func: fn_sk_pop_free); cdecl; external LIBEAY;
function sk_insert(sk: PSTACK; data: PAnsiChar; where: Integer): Integer; cdecl; external LIBEAY;
function sk_delete(st: PSTACK; loc: Integer): PAnsiChar; cdecl; external LIBEAY;
function sk_delete_ptr(st: PSTACK; p: PAnsiChar): PAnsiChar; cdecl; external LIBEAY;
function sk_find(st: PSTACK; data: PAnsiChar): Integer; cdecl; external LIBEAY;
function sk_find_ex(st: PSTACK; data: PAnsiChar): Integer; cdecl; external LIBEAY;
function sk_push(st: PSTACK; data: PAnsiChar): Integer; cdecl; external LIBEAY;
function sk_unshift(st: PSTACK; data: PAnsiChar): Integer; cdecl; external LIBEAY;
function sk_shift(st: PSTACK): PAnsiChar; cdecl; external LIBEAY;
function sk_pop(st: PSTACK): PAnsiChar; cdecl; external LIBEAY;
procedure sk_zero(st: PSTACK); cdecl; external LIBEAY;
// TODO:
//int ( *sk_set_cmp_func (sk: PSTACK; c: fn_stack_cmp))
//   (const char * const *, const char * const *);
function sk_dup(st: PSTACK): PSTACK; cdecl; external LIBEAY;
procedure sk_sort(st: PSTACK); cdecl; external LIBEAY;
function sk_is_sorted(const st: PSTACK): Integer; cdecl; external LIBEAY;

{$endif}

(*******************************************************************************
 * aes
 ******************************************************************************)

{$ifdef HEADER_AES_H}

{$ifdef OPENSSL_NO_AES}
AES is disabled.
{$endif}

const
  _AES_ENCRYPT = 1;
  _AES_DECRYPT = 0;

(* Because array size can't be a const in C, the following two are macros.
   Both sizes are in bytes. *)
  AES_MAXNR = 14;
  AES_BLOCK_SIZE = 16;

{$ifdef OPENSSL_FIPS}
type
  FIPS_AES_SIZE_T = Integer;
{$endif}

(* This should be a hidden type, but EVP requires that the size be known *)
type
  PAES_KEY = ^AES_KEY;
  aes_key_st = record
{$ifdef AES_LONG}
    rd_key: array[0..(4 *(AES_MAXNR + 1)) - 1] of LongWord;
{$else}
    rd_key: array[0..(4 *(AES_MAXNR + 1)) - 1] of Cardinal;
{$endif}
    rounds: Integer;
  end;
  AES_KEY = aes_key_st;

function AES_options(): PAnsiChar; cdecl; external LIBEAY;

function AES_set_encrypt_key(const userKey: PAnsiChar; const bits: Integer; key: PAES_KEY): Integer; cdecl; external LIBEAY;
function AES_set_decrypt_key(const userKey: PAnsiChar; const bits: Integer; key: PAES_KEY): Integer; cdecl; external LIBEAY;

procedure AES_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const key: PAES_KEY); cdecl; external LIBEAY;
procedure AES_decrypt(const in_: PAnsiChar; out_: PAnsiChar; const key: PAES_KEY); cdecl; external LIBEAY;

procedure AES_ecb_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const key: PAES_KEY; const enc: Integer); cdecl; external LIBEAY;
procedure AES_cbc_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec: PAnsiChar; const enc: Integer); cdecl; external LIBEAY;
procedure AES_cfb128_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec: PAnsiChar; num: PInteger; const enc: Integer); cdecl; external LIBEAY;
procedure AES_cfb1_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec: PAnsiChar; num: PInteger; const enc: Integer); cdecl; external LIBEAY;
procedure AES_cfb8_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec: PAnsiChar; num: PInteger; const enc: Integer); cdecl; external LIBEAY;
procedure AES_cfbr_encrypt_block(const in_: PAnsiChar;out_: PAnsiChar; const nbits: Integer;const key: PAES_KEY; ivec: PAnsiChar;const enc: Integer); cdecl; external LIBEAY;
procedure AES_ofb128_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec: PAnsiChar; num: PInteger); cdecl; external LIBEAY;

type
  TAesBlock = array[0..AES_BLOCK_SIZE-1] of AnsiChar;
procedure AES_ctr128_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec, ecount_buf: TAesBlock; num: PCardinal); cdecl; external LIBEAY;

(* For IGE, see also http://www.links.org/files/openssl-ige.pdf *)
(* NB: the IV is _two_ blocks long *)
procedure AES_ige_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec: PAnsiChar; const enc: Integer); cdecl; external LIBEAY;
(* NB: the IV is _four_ blocks long *)
procedure AES_bi_ige_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; const key2: PAES_KEY; const ivec: PAnsiChar; const enc: Integer); cdecl; external LIBEAY;
function AES_wrap_key(key: PAES_KEY; const iv: PAnsiChar; out_: PAnsiChar; const in_: PAnsiChar; inlen: Cardinal): Integer; cdecl; external LIBEAY;
function AES_unwrap_key(key: PAES_KEY; const iv: PAnsiChar; out_: PAnsiChar; const in_: PAnsiChar; inlen: Cardinal): Integer; cdecl; external LIBEAY;

{$endif} (* !HEADER_AES_H *)

(*******************************************************************************
 * e_os2
 ******************************************************************************)

(*******************************************************************************
 * safestack
 ******************************************************************************)

(*******************************************************************************
 * opensslv
 ******************************************************************************)

(*******************************************************************************
 * ossl_typ
 ******************************************************************************)

(*******************************************************************************
 * ebcdic
 ******************************************************************************)

(*******************************************************************************
 * symhacks
 ******************************************************************************)

(*******************************************************************************
 * crypto
 ******************************************************************************)

(*******************************************************************************
 * bio
 ******************************************************************************)

(*******************************************************************************
 * bn
 ******************************************************************************)

(*******************************************************************************
 * asn1
 ******************************************************************************)

(*******************************************************************************
 * asn1t
 ******************************************************************************)

(*******************************************************************************
 * asn1_mac
 ******************************************************************************)

(*******************************************************************************
 * blowfish
 ******************************************************************************)

(*******************************************************************************
 * buffer
 ******************************************************************************)

(*******************************************************************************
 * cast
 ******************************************************************************)

(*******************************************************************************
 * comp
 ******************************************************************************)

(*******************************************************************************
 * lhash
 ******************************************************************************)

(*******************************************************************************
 * conf
 ******************************************************************************)

(*******************************************************************************
 * conf_api
 ******************************************************************************)

(*******************************************************************************
 * ui
 ******************************************************************************)

(*******************************************************************************
 * ui_compat
 ******************************************************************************)

(*******************************************************************************
 * des_old
 ******************************************************************************)

(*******************************************************************************
 * des
 ******************************************************************************)

(*******************************************************************************
 * dh
 ******************************************************************************)

(*******************************************************************************
 * dsa
 ******************************************************************************)

(*******************************************************************************
 * dso
 ******************************************************************************)

(*******************************************************************************
 * pq_compat
 ******************************************************************************)

(*******************************************************************************
 * pqueue
 ******************************************************************************)

(*******************************************************************************
 * dtls1
 ******************************************************************************)

(*******************************************************************************
 * ec
 ******************************************************************************)

(*******************************************************************************
 * ecdh
 ******************************************************************************)

(*******************************************************************************
 * ecdsa
 ******************************************************************************)

(*******************************************************************************
 * rsa
 ******************************************************************************)

(*******************************************************************************
 * rand
 ******************************************************************************)

(*******************************************************************************
 * evp
 ******************************************************************************)

(*******************************************************************************
 * sha
 ******************************************************************************)

(*******************************************************************************
 * pkcs7
 ******************************************************************************)

(*******************************************************************************
 * x509_vfy
 ******************************************************************************)

(*******************************************************************************
 * x509
 ******************************************************************************)

(*******************************************************************************
 * x509v3
 ******************************************************************************)

(*******************************************************************************
 * store
 ******************************************************************************)

(*******************************************************************************
 * err
 ******************************************************************************)

(*******************************************************************************
 * engine
 ******************************************************************************)

(*******************************************************************************
 * hmac
 ******************************************************************************)

(*******************************************************************************
 * idea
 ******************************************************************************)

(*******************************************************************************
 * krb5_asn
 ******************************************************************************)

(*******************************************************************************
 * kssl
 ******************************************************************************)

(*******************************************************************************
 * md2
 ******************************************************************************)

(*******************************************************************************
 * md4
 ******************************************************************************)

(*******************************************************************************
 * md5
 ******************************************************************************)

(*******************************************************************************
 * obj_mac
 ******************************************************************************)

(*******************************************************************************
 * objects
 ******************************************************************************)

(*******************************************************************************
 * ocsp
 ******************************************************************************)

(*******************************************************************************
 * pem2
 ******************************************************************************)

(*******************************************************************************
 * pem
 ******************************************************************************)

(*******************************************************************************
 * pkcs12
 ******************************************************************************)

(*******************************************************************************
 * rc2
 ******************************************************************************)

(*******************************************************************************
 * rc4
 ******************************************************************************)

(*******************************************************************************
 * ripemd
 ******************************************************************************)

(*******************************************************************************
 * ssl2
 ******************************************************************************)

(*******************************************************************************
 * tls1
 ******************************************************************************)

(*******************************************************************************
 * ssl23
 ******************************************************************************)

(*******************************************************************************
 * ssl3
 ******************************************************************************)

(*******************************************************************************
 * ssl
 ******************************************************************************)

(*******************************************************************************
 * tmdiff
 ******************************************************************************)

(*******************************************************************************
 * txt_db
 ******************************************************************************)


procedure AesEncryptStream(InStream, OutStream: TStream; const pass: PAnsiChar; bits: Integer);
procedure AesDecryptStream(InStream, OutStream: TStream; const pass: PAnsiChar; bits: Integer);

implementation

procedure AesEncryptStream(InStream, OutStream: TStream; const pass: PAnsiChar; bits: Integer);
var
  inbuffer, outbuffer: TAesBlock;
  key: AES_KEY;
  len: Integer;
begin
  instream.Seek(0, soFromBeginning);
  AES_set_encrypt_key(pass, bits, @key);
  inbuffer[0] := AnsiChar(sizeof(TAesBlock) - ((InStream.Size+1)  mod sizeof(TAesBlock)));
  len := InStream.Read(inbuffer[1], sizeof(TAesBlock)-1) + 1;
  while len > 0 do
  begin
    AES_encrypt(@inbuffer, @outbuffer, @key);
    OutStream.Write(outbuffer, SizeOf(TAesBlock));
    len := InStream.Read(inbuffer, sizeof(TAesBlock));
  end;
end;

procedure AesDecryptStream(InStream, OutStream: TStream; const pass: PAnsiChar; bits: Integer);
var
  inbuffer, outbuffer: TAesBlock;
  key: AES_KEY;
  m: Byte;
  i, c: Integer;
begin
  if InStream.Size mod SizeOf(TAesBlock) <> 0 then Exit;

  InStream.Seek(0, soFromBeginning);

  c := InStream.Size div sizeof(TAesBlock);
  AES_set_decrypt_key(pass, bits, @key);

  InStream.Read(inbuffer, sizeof(TAesBlock));
  AES_decrypt(@inbuffer, @outbuffer, @key);
  m := Byte(outbuffer[0]);
  if c <> 1 then
    OutStream.Write(outbuffer[1], sizeof(TAesBlock)-1) else
    begin
      OutStream.Write(outbuffer[1], sizeof(TAesBlock)-1-m);
      Exit;
    end;

  for i := 2 to c-1 do
  begin
    InStream.Read(inbuffer, sizeof(TAesBlock));
    AES_decrypt(@inbuffer, @outbuffer, @key);
    OutStream.Write(outbuffer, sizeof(TAesBlock));
  end;

  InStream.Read(inbuffer, sizeof(TAesBlock));
  AES_decrypt(@inbuffer, @outbuffer, @key);
  OutStream.Write(outbuffer, sizeof(TAesBlock)-m);
end;


end.




