unit Bcrypt;

(*
	Sample Usage
	============

		//Hash a password using default cost (e.g. 11 ==> 2^11 ==> 2,048 rounds)
		hash := TBCrypt.HashPassword('p@ssword1');

		//Hash a password using custom cost factor
		hash := TBCrypt.HashPassword('p@ssword1', 14); //14 ==> 2^14 ==> 16,384 rounds

		//Check a password
		var
			passwordRehashNeeded: Boolean;

		TBCrypt.CheckPassword(szPassword, existingHash, {out}passwordRehashNeeded);

	Remarks
	=======

	Bcrypt is an algorithm designed for hashing passwords, and only passwords.

		i.e. It's not a generic, high-speed, generic hashing algorithm.
			  It's not a password-based key derivation function
			  It's computationally and memory expensive
			  It's limited to passwords of 71 bytes.

	http://static.usenix.org/events/usenix99/provos/provos.pdf

	It uses the Blowfish encryption algorithm, but with an "expensive key setup" modification,
	contained in the function EksBlowfishSetup.



	Version 1.13     20180729
			- UIntPtr isn't declared in Delphi 2010 (21.0). Maybe it first appeared in Delphi XE (22.0)?
	Version 1.12     20180419
			- Made compatible with Delphi 5
			- Published all self tests, but put slow ones behind the -SlowUnitTests command line parameter
	Version 1.11     20180120
			- Bugfix: The raw version of CheckPassword forgot to time the hash operation, and set PasswordRehashNeeded out parameter approriately
	Version 1.10     20161212
			- Bugfix: Don't zero out password byte array when it is empty - it's a range check error and unneeded
	Version 1.09     20161122
			- Added: In accordance with the recommendations of NIST SP 800-63B, we now apply KC normalization to the password.
						Choice was between NFKC and NFKD. SASLprep (rfc4013), like StringPrep (rfc3454) both specified NFKC.
	Version 1.08     20161029
			- We now burn the intermediate UTF8 form of the password.
			- BCrypt key has a maximum size of 72 bytes. We burn off any excess bytes after converting the string to utf8
			- Changed the handling of passwords longer than the 72 characters to match other implemntations.
			  No longer do we forcibly add a terminating null, but instead simply chop the utf8 array at 72 bytes.
	Version 1.07     20160423
			- Changed: Fixed up compiler defines so that we work on Delphi 5, Delphi 7, and Delphi XE5 (the only versions of Delphi i use)
			- Added the fast SelfTests into their own DUnit test; the ones that are too slow are still kept in the public section
			- Added granular abilty to disable BCrypt unit tests
	Version 1.06     20151026
			- Added: CheckPassword functions now take "PasswordRehashNeeded" out parameter.
			  The value will contain True if the password needs to be rehashed.
			  For for example: if the BCrypt cost needs to be increased as the hash was calculated too quickly,
			  or if the BCrypt standard has been updated (as OpenBSD updated their canonical output to 2b)
			- Updated: We now use OS CryptGenRandom for salt generation.
			  If it fails, we fall back to a type 4 ("random") UUID.
			  Salt doesn't have to be cryptographically strong, just different.
			- Fix: We now recognize version "2" and well as anything version "2"+[letter].
			  OpenBSD canonical version now outputs "2c".
			  We will continue to output "2b" for a while, at least hopefully until everyone's updated.
			  Previous version would fail on anything besides "2a".
			  In the case of a federated login, we can't have older software that only knows how to handle "2a" suddenly
			  start failing when it encounters "2b".
			- Updated: Removed dependancy on Blowfish.pas. Copied relavent ECB function, state variable, and digits of PI

	Version 1.05     20150321
			- Performance improvement: Was so worried about using faster verison of Move in BlowfishEncryptECB and BlowfishDecryptECB,
			  that i didn't stop to notice that i shouldn't even be using Move; but instead a 32-bit assignment
			- Fix: Fixed bug in EksBlowfishSetup. If you used a cost factor 31 (2,147,483,648), then the Integer loop
			  control variable would overflow and the expensive key setup wouldn't run at all (zero iterations)
			- The original whitepaper mentions the maximum key length of 56 bytes.
			  This was a misunderstanding based on the Blowfish maximum recommended key size of 448 bits.
			  The algorithm can, and does, support up to 72 bytes (e.g. 71 ASCII characters + null terminator).
			  Note: Variant 2b of bcrypt also *caps* the password length at 72 (to avoid integer wraparound on passwords longer than 2 billion characters O.o)

	Version 1.04     20150312
			- Performance improvement: ExpandKey: Hoisted loop variable, use xor to calculate SaltHalfIndex to avoid speculative execution jump, unrolled loop to two 32-bit XORs (16% faster)
			- Performance improvement (D5,D7): Now use pure pascal version of FastCode Move() (50% faster)

	Version 1.03     20150319
			- Fix: Defined away Modernizer (so people who are not me can use it)
			- Added: If no cost factor is specified when hashing a password,
			  the cost factor is now a sliding factor, based on Moore's Law and when BCrypt was designed

	Version 1.02     20141215
			- Added support for XE2 string/UnicodeString/AnsiString
			- Update: Updated code to work in 64-bit environment

	Version 1.01     20130612
			- New: Added HashPassword overload that lets you specify your desired cost

	Version 1.0      20120504
			- Initial release by Ian Boyd, Public Domain


	bcrypt was designed for OpenBSD, where hashes in the password file have a certain format.

	The convention used in BSD when generating password hash strings is to format it as:
			$version$salt$hash

	MD5 hash uses version "1":
			"$"+"1"+"$"+salt+hash

	bcrypt uses version "2a", but also encodes the cost

			"$"+"2a"+"$"+rounds+"$"+salt+hash

	e.g.
			$2a$10$Ro0CUfOqk6cXEKf3dyaM7OhSCvnwM9s4wIX9JeLapehKK5YdLxKcm
			$==$==$======================-------------------------------

	The benfit of this scheme is:
			- the number of rounds
			- the salt used

	This means that stored hashes are backwards and forwards compatible with changing the number of rounds


	BCrypt variants
	===============

	$2$
		The original specification used the prefix $2$.
		This was in contrast to the other algorithm prefixes:
			$1$ - MD5
			$5$ - SHA-256
			$6$ - SHA-512

	$2a$
		The original specification did not define how to handle non-ASCII character, or how to handle a null terminator.
		The specification was revised to specify that when hashing strings:
				- the string must be UTF-8 encoded
				- the null terminator must be included

	$2x$, $2y$ June 2011
		A bug was discovered in crypt_blowfish, a PHP implementation of BCrypt.
		It was mis-handling characters with the 8th bit set.
		They suggested that system administrators update their existing password database, replacing $2a$ with $2x$,
		to indicate that those hashes are bad (and need to use the old broken algorithm).
		They also suggested the idea of having crypt_blowfish emit $2y$ for hashes generated by the fixed algorithm.
		Nobody else, including canonical OpenBSD, adopted the idea of 2x/2y. This version marker was was limited to crypt_blowfish.
		http://seclists.org/oss-sec/2011/q2/632

	$2b$ February 2014
		A bug was discovered in the OpenBSD implemenation of bcrypt.
		They were storing the length of their strings in an unsigned char.
		If a password was longer than 255 characters, it would overflow and wrap at 255.
		BCrypt was created for OpenBSD. When they have a bug in *their* library, they decided its ok to bump the version.
		This means that everyone else needs to follow suit if you want to remain current to "their" specification.
		http://undeadly.org/cgi?action=article&sid=20140224132743
		http://marc.info/?l=openbsd-misc&m=139320023202696
*)

interface

(*
	The problem is how to make "TBytes", "UIntPtr", and "UnicodeString" work in Delphi 5, Delphi 7, Delphi 2010, and XE2+

	| Item            | Delphi 5        | Delphi 7              | Delphi 2009            | Delphi 2010            | Delphi XE2             |
	|-----------------|-----------------|-----------------------|------------------------|------------------------|------------------------|
	| Product version | 5               | 7                     | 12                     | 14                     | 16                     |
	| Version         | VER130          | VER150                | VER200                 | VER210                 | VER230                 |
	| CompilerVersion | n/a             | 15.0                  | 20.0                   | 21.0                   | 23.0                   |
	| TBytes          | = array of Byte | = Types.TByteDynArray | = Types.TByteDynArray? | = Types.TByteDynArray? | = SysUtils.TBytes      |
	| UnicodeString   | = WideString    | = WideString          | = System.UnicodeString | = System.UnicodeString | = System.UnicodeString | Added in Delphi 2009
	| UIntPtr         | = Cardinal      | = Cardinal            | = Cardinal             | = Cardinal             | = System.UIntPtr       |

	And it wasn't until Delphi 6 (CompilerVersion >= 14.0) that conditional expressions (CONDITIONALEXPRESSIONS) were added.
*)
{$IFDEF CONDITIONALEXPRESSIONS}
	{$IF CompilerVersion >= 22}
		{$DEFINE COMPILER_15_UP} //Delphi XE
	{$IFEND}
	{$IF CompilerVersion >= 15}
		{$DEFINE COMPILER_7_UP} //Delphi 7
	{$IFEND}
{$ENDIF}

uses
	SysUtils, Windows, Math,
	{$IFDEF COMPILER_7_UP}Types,{$ENDIF} //Types.pas didn't appear until ~Delphi 7.
	ComObj;

type
{$IFNDEF UNICODE}
	UnicodeString = WideString; //System.UnicodeString wasn't added until Delphi 2009
{$ENDIF}

{$IFDEF VER150} //Delphi 7
	TBytes = Types.TByteDynArray; //TByteDynArray wasn't added until around Delphi 7. Sometime later it moved to SysUtils.
{$ENDIF}

{$IFDEF VER130} //Delphi 5
	TBytes = array of Byte; //for old-fashioned Delphi 5, we have to do it ourselves
{$ENDIF}

{$IFNDEF COMPILER_15_UP}
	//Someone said that Delphi 2010 (Delphi 14) didn't have UIntPtr.
	//So maybe it was Delphi XE (Delphi 15)
	UIntPtr = Cardinal; //an unsigned, pointer sized, integer
{$ENDIF}

	TBlowfishData= record
		InitBlock: array[0..7] of Byte;    { initial IV }
		LastBlock: array[0..7] of Byte;    { current IV }
		SBoxM: array[0..3, 0..255] of DWORD;
		PBoxM: array[0..17] of DWORD;
	end;

	TBCrypt = class(TObject)
	private
		class function TryParseHashString(const hashString: string;
				out version: string; out Cost: Integer; out Salt: TBytes): Boolean;
	protected
		class function EksBlowfishSetup(const Cost: Integer; salt, key: array of Byte): TBlowfishData;
		class procedure ExpandKey(var state: TBlowfishData; salt, key: array of Byte);
		class function CryptCore(const Cost: Integer; Key: array of Byte; salt: array of Byte): TBytes;

		class function FormatPasswordHashForBsd(const Version: string; const cost: Integer; const salt: array of Byte; const hash: array of Byte): string;

		class function BsdBase64Encode(const data: array of Byte; BytesToEncode: Integer): string;
		class function BsdBase64Decode(const s: string): TBytes;

		class function PasswordStringPrep(const Source: UnicodeString): TBytes;

		class function SelfTestA: Boolean; //known test vectors
		class function SelfTestB: Boolean; //BSD's base64 encoder/decoder
		class function SelfTestC: Boolean; //unicode strings in UTF8
		class function SelfTestD: Boolean; //different length passwords
		class function SelfTestE: Boolean; //salt rng
		class function SelfTestF: Boolean; //correctbatteryhorsestapler
		class function SelfTestG: Boolean; //check that we support up to 72 characters
		class function SelfTestH: Boolean; //check that we don't limit our passwords to 256 characters (as OpenBSD did)
		class function SelfTestI: Boolean; //check that we use unicode compatible composition (NFKC) on passwords
		class function SelfTestJ: Boolean; //check that composed and decomposed strings both validate to the same
		class function SelfTestK: Boolean; //SASLprep rules for passwords

		class function GenRandomBytes(len: Integer; const data: Pointer): HRESULT;

		class function GetModernCost(SampleCost: Integer; SampleHashDurationMS: Real): Integer;
		class function GetModernCost_Benchmark: Integer;

		class function TimingSafeSameString(const Safe, User: string): Boolean;
		class function PasswordRehashNeededCore(const Version: string; const Cost: Integer; SampleCost: Integer; SampleHashDurationMS: Real): Boolean;
	public
		//Hashes a password into the OpenBSD password-file format (non-standard base-64 encoding). Also validate that BSD style string
		class function HashPassword(const password: UnicodeString): string; overload;
		class function HashPassword(const password: UnicodeString; cost: Integer): string; overload;
		class function CheckPassword(const password: UnicodeString; const expectedHashString: string; out PasswordRehashNeeded: Boolean): Boolean; overload;

		//If you want to handle the cost, salt, and encoding yourself, you can do that.
		class function HashPassword(const password: UnicodeString; const salt: array of Byte; const cost: Integer): TBytes; overload;
		class function CheckPassword(const password: UnicodeString; const salt, hash: array of Byte; const Cost: Integer; out PasswordRehashNeeded: Boolean): Boolean; overload;
		class function GenerateSalt: TBytes;

		//Evaluate the cost (or version) of a hash string, and figure out if it needs to be rehashed
		class function PasswordRehashNeeded(const HashString: string): Boolean;

		class function SelfTest: Boolean;
	end;

	EBCryptException = class(Exception);

implementation

{$IFDEF Strict}
	{$DEFINE Sqm}
{$ENDIF}

{$IFDEF NoSqm}
	{$UNDEF Sqm}
{$ENDIF}

{$IFDEF UnitTests}
	{$DEFINE BCryptUnitTests}
{$ENDIF}

{$IFDEF NoBCryptUnitTests}
	{$UNDEF BCryptUnitTests}
{$ENDIF}


uses
{$IFDEF Sqm}SqmApi,{$ENDIF}
{$IFDEF BCryptUnitTests}TestFramework,{$ENDIF}
	ActiveX;

const
	BCRYPT_COST = 11; //cost determintes the number of rounds. 11 = 2^11 rounds (2,048)
	{
		| Cost | Iterations        | E6300        | E5-2620     | i5-2500    | i7-2700K    |
		|                          |      2006-Q3 |     2012-Q1 |    2011-Q1 |     2011-Q4 |
		|                          |     1.86 GHz |       2 GHz |    3.3 GHz |     3.5 GHz |
		|------|-------------------|--------------|-------------|------------|-------------|
		|    8 |    256 iterations |     61.65 ms |     48.8 ms |    21.7 ms |     20.8 ms |  <-- minimum allowed by BCrypt
		|    9 |    512 iterations |    126.09 ms |     77.7 ms |    43.3 ms |     41.5 ms |
		|   10 |  1,024 iterations |    249.10 ms |    128.8 ms |    85.5 ms |     83.2 ms |
		|   11 |  2,048 iterations |    449.23 ms |    250.1 ms |   173.3 ms |    166.8 ms |  <-- current default (BCRYPT_COST=11)
		|   12 |  4,096 iterations |  1,007.05 ms |    498.8 ms |   345.6 ms |    333.4 ms |
		|   13 |  8,192 iterations |  1,995.48 ms |    999.1 ms |   694.3 ms |    667.9 ms |
		|   14 | 16,384 iterations |  4,006.78 ms |  1,997.6 ms | 1,390.5 ms |  1,336.5 ms |
		|   15 | 32,768 iterations |  8,027.05 ms |  3,999.9 ms | 2,781.4 ms |  2,670.5 ms |
		|   16 | 65,536 iterations | 15,982.14 ms |  8,008.2 ms | 5,564.9 ms |  5,342.8 ms |


		At the time of deployment in 1976, crypt could hash fewer than 4 passwords per second. (250 ms per password)
		In 1977, on a VAX-11/780, crypt (MD5) could be evaluated about 3.6 times per second.   (277 ms per password)
		If 277 ms per hash was our target, it would mean a range of 180 ms..360 ms.

		At the time of publication of BCrypt (1999) the default costs were:
			- Normal User: 6
			- the Superuser: 8

			"Of course, whatever cost people choose should be reevaluated from time to time."

		We want to target between 250-500ms per hash.
	}

	BCRYPT_SALT_LEN = 16; //bcrypt uses 128-bit (16-byte) salt (This isn't an adjustable parameter, just a name for a constant)
	BCRYPT_MaxKeyLen = 72; //72 bytes ==> 71 ansi charcters + null terminator

	BsdBase64EncodeTable: array[0..63] of Char =
			{ 0:} './'+
			{ 2:} 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'+
			{28:} 'abcdefghijklmnopqrstuvwxyz'+
			{54:} '0123456789';

			//the traditional base64 encode table:
			//'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
			//'abcdefghijklmnopqrstuvwxyz' +
			//'0123456789+/';

	BsdBase64DecodeTable: array[#0..#127] of Integer = (
			{  0:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ________________
			{ 16:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ________________
			{ 32:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  1,  // ______________./
			{ 48:} 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, -1, -1, -1, -1, -1, -1,  // 0123456789______
			{ 64:} -1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16,  // _ABCDEFGHIJKLMNO
			{ 80:} 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, -1, -1, -1, -1, -1,  // PQRSTUVWXYZ_____
			{ 96:} -1, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,  // _abcdefghijklmno
			{113:} 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, -1, -1, -1, -1, -1); // pqrstuvwxyz_____

	TestVectors: array[1..20, 1..3] of string = (
			('',                                   '$2a$06$DCq7YPn5Rq63x1Lad4cll.',    '$2a$06$DCq7YPn5Rq63x1Lad4cll.TV4S6ytwfsfvkgY8jIucDrjc8deX1s.'),
			('',                                   '$2a$08$HqWuK6/Ng6sg9gQzbLrgb.',    '$2a$08$HqWuK6/Ng6sg9gQzbLrgb.Tl.ZHfXLhvt/SgVyWhQqgqcZ7ZuUtye'),
			('',                                   '$2a$10$k1wbIrmNyFAPwPVPSVa/ze',    '$2a$10$k1wbIrmNyFAPwPVPSVa/zecw2BCEnBwVS2GbrmgzxFUOqW9dk4TCW'),
			('',                                   '$2a$12$k42ZFHFWqBp3vWli.nIn8u',    '$2a$12$k42ZFHFWqBp3vWli.nIn8uYyIkbvYRvodzbfbK18SSsY.CsIQPlxO'),
			('a',                                  '$2a$06$m0CrhHm10qJ3lXRY.5zDGO',    '$2a$06$m0CrhHm10qJ3lXRY.5zDGO3rS2KdeeWLuGmsfGlMfOxih58VYVfxe'),
			('a',                                  '$2a$08$cfcvVd2aQ8CMvoMpP2EBfe',    '$2a$08$cfcvVd2aQ8CMvoMpP2EBfeodLEkkFJ9umNEfPD18.hUF62qqlC/V.'),
			('a',                                  '$2a$10$k87L/MF28Q673VKh8/cPi.',    '$2a$10$k87L/MF28Q673VKh8/cPi.SUl7MU/rWuSiIDDFayrKk/1tBsSQu4u'),
			('a',                                  '$2a$12$8NJH3LsPrANStV6XtBakCe',    '$2a$12$8NJH3LsPrANStV6XtBakCez0cKHXVxmvxIlcz785vxAIZrihHZpeS'),
			('abc',                                '$2a$06$If6bvum7DFjUnE9p2uDeDu',    '$2a$06$If6bvum7DFjUnE9p2uDeDu0YHzrHM6tf.iqN8.yx.jNN1ILEf7h0i'),
			('abc',                                '$2a$08$Ro0CUfOqk6cXEKf3dyaM7O',    '$2a$08$Ro0CUfOqk6cXEKf3dyaM7OhSCvnwM9s4wIX9JeLapehKK5YdLxKcm'),
			('abc',                                '$2a$10$WvvTPHKwdBJ3uk0Z37EMR.',    '$2a$10$WvvTPHKwdBJ3uk0Z37EMR.hLA2W6N9AEBhEgrAOljy2Ae5MtaSIUi'),
			('abc',                                '$2a$12$EXRkfkdmXn2gzds2SSitu.',    '$2a$12$EXRkfkdmXn2gzds2SSitu.MW9.gAVqa9eLS1//RYtYCmB1eLHg.9q'),
			('abcdefghijklmnopqrstuvwxyz',         '$2a$06$.rCVZVOThsIa97pEDOxvGu',    '$2a$06$.rCVZVOThsIa97pEDOxvGuRRgzG64bvtJ0938xuqzv18d3ZpQhstC'),
			('abcdefghijklmnopqrstuvwxyz',         '$2a$08$aTsUwsyowQuzRrDqFflhge',    '$2a$08$aTsUwsyowQuzRrDqFflhgekJ8d9/7Z3GV3UcgvzQW3J5zMyrTvlz.'),
			('abcdefghijklmnopqrstuvwxyz',         '$2a$10$fVH8e28OQRj9tqiDXs1e1u',    '$2a$10$fVH8e28OQRj9tqiDXs1e1uxpsjN0c7II7YPKXua2NAKYvM6iQk7dq'),
			('abcdefghijklmnopqrstuvwxyz',         '$2a$12$D4G5f18o7aMMfwasBL7Gpu',    '$2a$12$D4G5f18o7aMMfwasBL7GpuQWuP3pkrZrOAnqP.bmezbMng.QwJ/pG'),
			('~!@#$%^&*()      ~!@#$%^&*()PNBFRD', '$2a$06$fPIsBO8qRqkjj273rfaOI.',    '$2a$06$fPIsBO8qRqkjj273rfaOI.HtSV9jLDpTbZn782DC6/t7qT67P6FfO'),
			('~!@#$%^&*()      ~!@#$%^&*()PNBFRD', '$2a$08$Eq2r4G/76Wv39MzSX262hu',    '$2a$08$Eq2r4G/76Wv39MzSX262huzPz612MZiYHVUJe/OcOql2jo4.9UxTW'),
			('~!@#$%^&*()      ~!@#$%^&*()PNBFRD', '$2a$10$LgfYWkbzEvQ4JakH7rOvHe',    '$2a$10$LgfYWkbzEvQ4JakH7rOvHe0y8pHKF9OaFgwUZ2q7W2FFZmZzJYlfS'),
			('~!@#$%^&*()      ~!@#$%^&*()PNBFRD', '$2a$12$WApznUOJfkEGSmYRfnkrPO',    '$2a$12$WApznUOJfkEGSmYRfnkrPOr466oFDCaj4b6HY3EXGvfxm43seyhgC')
	);

	SInvalidHashString = 'Invalid base64 hash string';
	SBcryptCostRangeError = 'BCrypt cost factor must be between 4..31 (%d)';
	SKeyRangeError = 'Key must be between 1 and 72 bytes long (%d)';
	SSaltLengthError = 'Salt must be 16 bytes';
	SInvalidLength = 'Invalid length';

{$IFDEF BCryptUnitTests}
type
	TBCryptTests = class(TTestCase)
	public
		procedure SpeedTests;
		function GetCompilerOptions: string;
	public
		//These are just too darn slow (as they should be) for continuous testing
		procedure SelfTest;

	published
		procedure SelfTestA_KnownTestVectors; //known test vectors
		procedure SelfTestB_Base64EncoderDecoder; //BSD's base64 encoder/decoder
		procedure SelfTestC_UnicodeStrings; //unicode strings in UTF8
		procedure SelfTestD_VariableLengthPasswords; //different length passwords
		procedure SelfTestE_SaltRNG; //salt rng
		procedure SelfTestF_CorrectBattery; //correctbatteryhorsestapler
		procedure SelfTestG_PasswordLength; //check that we support up to 72 characters
		procedure SelfTestH_OpenBSDLengthBug; //check that we don't limit our passwords to 256 characters (as OpenBSD did)
		procedure SelfTestI_UnicodeCompatibleComposition; //check that we apply KC normalization (NIST SP 800-63B)
		procedure SelfTestJ_NormalizedPasswordsMatch; //
		procedure SelfTestK_SASLprep; //

		procedure Test_ParseHashString; //How well we handle past, present, and future versioning strings

		procedure Benchmark;
		procedure Test_ManualSystem;
	end;
{$ENDIF}

const
	advapi32 = 'advapi32.dll';

function CryptAcquireContextW(out phProv: THandle; pszContainer: PWideChar; pszProvider: PWideChar; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall; external advapi32;
function CryptReleaseContext(hProv: THandle; dwFlags: DWORD): BOOL; stdcall; external advapi32;
function CryptGenRandom(hProv: THandle; dwLen: DWORD; pbBuffer: Pointer): BOOL; stdcall; external advapi32;

procedure BurnString(var s: UnicodeString);
begin
	if Length(s) > 0 then
	begin
		{$IFDEF UNICODE}
		{
			In Delphi 5 (and really anything before XE2), UnicodeString is an alias for WideString.
			WideString does not have, or does it need, an RTL UniqueString function.
		}
		UniqueString({var}s); //We can't FillChar the string if it's shared, or its in the constant data page
		{$ENDIF}
		FillChar(s[1], Length(s), 0);
		s := '';
	end;
end;

{ TBCrypt }

class function TBCrypt.HashPassword(const password: UnicodeString): string;
var
	cost: Integer;
begin
	{
		Generate a hash for the specified password using the default cost.

		Sample Usage:
			hash := TBCrypt.HashPassword('correct horse battery stample');


	Rather than using a fixed default cost, use a self-adjusting cost.
	We give ourselves two methods:

		- Moore's Law sliding constant
		- Benchmark

	The problem with using Moore's Law is that it's falling behind for single-core performance.
	Since 2004, single-core performance is only going up 21% per year, rather than the 26% of Moore's Law.

			26%/year ==> doubles every 18 months
			21%/year ==> doubles every 44 months

	So i could use a more practical variation of Moore's Law. Knowing that it is now doubling every 44 months,
	and that i want the target speed to be between 500-750ms, i could use the new value.

	The alternative is to run a quick benchmark. It only takes 1.8ms to do a cost=4 hash. Use it benchmark the computer.

	The 3rd alternative would be to run the hash as normal, and time it. If it takes less than 500ms to calculate, then
	do it again with a cost of BCRYPT_COST+1.
	}

	cost := TBCrypt.GetModernCost_Benchmark;

	if cost < BCRYPT_COST then
		cost := BCRYPT_COST;

	Result := TBCrypt.HashPassword(password, cost);
end;

class function TBCrypt.HashPassword(const password: UnicodeString; cost: Integer): string;
var
	salt: TBytes;
	hash: TBytes;
begin
	{
		Generate a hash for the supplied password using the specified cost.

		Sample usage:

			hash := TBCrypt.HashPassword('Correct battery Horse staple', 13); //Cost factor 13
	}

	salt := GenerateSalt();

	hash := TBCrypt.HashPassword(password, salt, cost);

	//20151010  I don't want to emit 2b just yet. The previous bcrypt would fail on anything besides 2a.
	//This version handles any single letter suffix. But if we have cross system authentication, and an older system
	//tries to validate a 2b password it will fail.
	//Wait a year or so until everyone has the new bcrypt
	Result := FormatPasswordHashForBsd('2a', cost, salt, hash);
end;

class function TBCrypt.GenerateSalt: TBytes;
var
	type4Uuid: TGUID;
	salt: TBytes;
begin
	//Salt is a 128-bit (16 byte) random value
	SetLength(salt, BCRYPT_SALT_LEN);

	//20150309  Use real random data. Fallback to random guid if it fails
	if Failed(Self.GenRandomBytes(BCRYPT_SALT_LEN, {out}@salt[0])) then
	begin
		//Type 4 UUID (RFC 4122) is a handy source of (almost) 128-bits of random data (actually 120 bits)
		//But the security doesn't come from the salt being secret, it comes from the salt being different each time
		OleCheck(CoCreateGUID(Type4Uuid));
		Move(type4Uuid.D1, salt[0], BCRYPT_SALT_LEN); //16 bytes
	end;

	Result := salt;
end;

class function TBCrypt.HashPassword(const password: UnicodeString; const salt: array of Byte; const cost: Integer): TBytes;
var
	key: TBytes;
begin
	{ The canonical BSD algorithm expects a null-terminated UTF8 key.
		If the key is longer than 72 bytes, they truncate the array of bytes to 72.

		Yes, this does mean that can can lose the null terminator, and we can chop a multi-byte utf8 code point into an invalid character.
	}

	//Pseudo-standard dictates that unicode strings are converted to UTF8 (rather than UTF16, UTF32, UTF16LE, ISO-8859-1, Windows-1252, etc)
	key := TBCrypt.PasswordStringPrep(password);
	try
		//Truncate if its longer than 72 bytes (BCRYPT_MaxKeyLen), and burn the excess
		if Length(key) > BCRYPT_MaxKeyLen then
		begin
			ZeroMemory(@key[BCRYPT_MaxKeyLen], Length(key)-BCRYPT_MaxKeyLen);
			SetLength(key,BCRYPT_MaxKeyLen);
		end;

		Result := TBCrypt.CryptCore(cost, key, salt);
	finally
		if Length(key) > 0 then
		begin
			ZeroMemory(@key[0], Length(key));
			SetLength(key, 0);
		end;
	end;
end;

{$OVERFLOWCHECKS OFF}
procedure BlowfishEncryptECB(const Data: TBlowfishData; InData, OutData: Pointer);
var
	xL, xR: LongWord;
begin
	xL := PLongWord(InData)^;
	xR := PLongWord(UIntPtr(InData)+4)^;

	xL := (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
	xR := (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
	xL := xL xor Data.PBoxM[0];
	xR := xR xor (((Data.SBoxM[0, (xL shr 24) and $FF] + Data.SBoxM[1, (xL shr 16) and $FF]) xor Data.SBoxM[2, (xL shr 8) and $FF]) + Data.SBoxM[3, xL and $FF]) xor Data.PBoxM[1];
	xL := xL xor (((Data.SBoxM[0, (xR shr 24) and $FF] + Data.SBoxM[1, (xR shr 16) and $FF]) xor Data.SBoxM[2, (xR shr 8) and $FF]) + Data.SBoxM[3, xR and $FF]) xor Data.PBoxM[2];
	xR := xR xor (((Data.SBoxM[0, (xL shr 24) and $FF] + Data.SBoxM[1, (xL shr 16) and $FF]) xor Data.SBoxM[2, (xL shr 8) and $FF]) + Data.SBoxM[3, xL and $FF]) xor Data.PBoxM[3];
	xL := xL xor (((Data.SBoxM[0, (xR shr 24) and $FF] + Data.SBoxM[1, (xR shr 16) and $FF]) xor Data.SBoxM[2, (xR shr 8) and $FF]) + Data.SBoxM[3, xR and $FF]) xor Data.PBoxM[4];
	xR := xR xor (((Data.SBoxM[0, (xL shr 24) and $FF] + Data.SBoxM[1, (xL shr 16) and $FF]) xor Data.SBoxM[2, (xL shr 8) and $FF]) + Data.SBoxM[3, xL and $FF]) xor Data.PBoxM[5];
	xL := xL xor (((Data.SBoxM[0, (xR shr 24) and $FF] + Data.SBoxM[1, (xR shr 16) and $FF]) xor Data.SBoxM[2, (xR shr 8) and $FF]) + Data.SBoxM[3, xR and $FF]) xor Data.PBoxM[6];
	xR := xR xor (((Data.SBoxM[0, (xL shr 24) and $FF] + Data.SBoxM[1, (xL shr 16) and $FF]) xor Data.SBoxM[2, (xL shr 8) and $FF]) + Data.SBoxM[3, xL and $FF]) xor Data.PBoxM[7];
	xL := xL xor (((Data.SBoxM[0, (xR shr 24) and $FF] + Data.SBoxM[1, (xR shr 16) and $FF]) xor Data.SBoxM[2, (xR shr 8) and $FF]) + Data.SBoxM[3, xR and $FF]) xor Data.PBoxM[8];
	xR := xR xor (((Data.SBoxM[0, (xL shr 24) and $FF] + Data.SBoxM[1, (xL shr 16) and $FF]) xor Data.SBoxM[2, (xL shr 8) and $FF]) + Data.SBoxM[3, xL and $FF]) xor Data.PBoxM[9];
	xL := xL xor (((Data.SBoxM[0, (xR shr 24) and $FF] + Data.SBoxM[1, (xR shr 16) and $FF]) xor Data.SBoxM[2, (xR shr 8) and $FF]) + Data.SBoxM[3, xR and $FF]) xor Data.PBoxM[10];
	xR := xR xor (((Data.SBoxM[0, (xL shr 24) and $FF] + Data.SBoxM[1, (xL shr 16) and $FF]) xor Data.SBoxM[2, (xL shr 8) and $FF]) + Data.SBoxM[3, xL and $FF]) xor Data.PBoxM[11];
	xL := xL xor (((Data.SBoxM[0, (xR shr 24) and $FF] + Data.SBoxM[1, (xR shr 16) and $FF]) xor Data.SBoxM[2, (xR shr 8) and $FF]) + Data.SBoxM[3, xR and $FF]) xor Data.PBoxM[12];
	xR := xR xor (((Data.SBoxM[0, (xL shr 24) and $FF] + Data.SBoxM[1, (xL shr 16) and $FF]) xor Data.SBoxM[2, (xL shr 8) and $FF]) + Data.SBoxM[3, xL and $FF]) xor Data.PBoxM[13];
	xL := xL xor (((Data.SBoxM[0, (xR shr 24) and $FF] + Data.SBoxM[1, (xR shr 16) and $FF]) xor Data.SBoxM[2, (xR shr 8) and $FF]) + Data.SBoxM[3, xR and $FF]) xor Data.PBoxM[14];
	xR := xR xor (((Data.SBoxM[0, (xL shr 24) and $FF] + Data.SBoxM[1, (xL shr 16) and $FF]) xor Data.SBoxM[2, (xL shr 8) and $FF]) + Data.SBoxM[3, xL and $FF]) xor Data.PBoxM[15];
	xL := xL xor (((Data.SBoxM[0, (xR shr 24) and $FF] + Data.SBoxM[1, (xR shr 16) and $FF]) xor Data.SBoxM[2, (xR shr 8) and $FF]) + Data.SBoxM[3, xR and $FF]) xor Data.PBoxM[16];
	xR := xR xor Data.PBoxM[17];
	xL := (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
	xR := (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);

	//Got rid of the moves
	PLongWord(OutData)^ := xR;
	PLongWord(UIntPtr(OutData)+4)^ := xL;
end;
{$OVERFLOWCHECKS ON}


class function TBCrypt.CryptCore(const Cost: Integer; key, salt: array of Byte): TBytes;
var
	state: TBlowfishData;
	i: Integer;
	plainText: array[0..23] of Byte;
	cipherText: array[0..23] of Byte;
{$IFDEF Sqm}t1: Int64;{$ENDIF}

const
	magicText: AnsiString = 'OrpheanBeholderScryDoubt'; //the 24-byte data we will be encrypting 64 times
begin
{$IFDEF Sqm}
	t1 := Sqm.GetTimestamp;
{$ENDIF}
	try
		state := TBCrypt.EksBlowfishSetup(cost, salt, key);

		//Conceptually we are encrypting "OrpheanBeholderScryDoubt" 64 times
		Move(magicText[1], plainText[0], 24);

		for i := 1 to 64 do
		begin
			//The painful thing is that the plaintext is 24 bytes long; this is three 8-byte blocks.
			//Which means we have to do the EBC encryption on 3 separate sections.
			BlowfishEncryptECB(state, Pointer(@plainText[ 0]), Pointer(@cipherText[ 0]));
			BlowfishEncryptECB(state, Pointer(@plainText[ 8]), Pointer(@cipherText[ 8]));
			BlowfishEncryptECB(state, Pointer(@plainText[16]), Pointer(@cipherText[16]));

			Move(cipherText[0], plainText[0], 24);
		end;

		//Copy final cipherText to Result
		SetLength(Result, 24);
		Move(cipherText[0], Result[0], 24);
	finally
		//Burn cipher state
		FillChar(state, SizeOf(state), 0);
		FillChar(plainText[0], SizeOf(plainText), 0);
		FillChar(cipherText[0], 24, 0);
	end;


{$IFDEF Sqm}
	Sqm.TimerStop('BCrypt/CryptCore', t1);
	Sqm.TimerStop('BCrypt/CryptCore/Cost'+IntToStr(Cost), t1);
{$ENDIF}
end;


class function TBCrypt.EksBlowfishSetup(const Cost: Integer; salt, key: array of Byte): TBlowfishData;
var
	rounds: Cardinal; //rounds = 2^cost
	i: Cardinal;
	Len: Integer;
const
	zero: array[0..15] of Byte = (0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0);

	//SBLOCKS ARE THE HEX DIGITS OF PI.
	//The amount of hex digits can be increased if you want to experiment with more rounds and longer key lengths
	PBox: array[0..17] of DWORD = (
				$243f6a88, $85a308d3, $13198a2e, $03707344, $a4093822, $299f31d0,
				$082efa98, $ec4e6c89, $452821e6, $38d01377, $be5466cf, $34e90c6c,
				$c0ac29b7, $c97c50dd, $3f84d5b5, $b5470917, $9216d5d9, $8979fb1b);

	SBox: array[0..3, 0..255] of DWORD = (
			//SBox[0]
			(
						  $d1310ba6, $98dfb5ac, $2ffd72db, $d01adfb7, $b8e1afed, $6a267e96,
						  $ba7c9045, $f12c7f99, $24a19947, $b3916cf7, $0801f2e2, $858efc16,
						  $636920d8, $71574e69, $a458fea3, $f4933d7e, $0d95748f, $728eb658,
						  $718bcd58, $82154aee, $7b54a41d, $c25a59b5, $9c30d539, $2af26013,
						  $c5d1b023, $286085f0, $ca417918, $b8db38ef, $8e79dcb0, $603a180e,
						  $6c9e0e8b, $b01e8a3e, $d71577c1, $bd314b27, $78af2fda, $55605c60,
						  $e65525f3, $aa55ab94, $57489862, $63e81440, $55ca396a, $2aab10b6,
						  $b4cc5c34, $1141e8ce, $a15486af, $7c72e993, $b3ee1411, $636fbc2a,
						  $2ba9c55d, $741831f6, $ce5c3e16, $9b87931e, $afd6ba33, $6c24cf5c,
						  $7a325381, $28958677, $3b8f4898, $6b4bb9af, $c4bfe81b, $66282193,
						  $61d809cc, $fb21a991, $487cac60, $5dec8032, $ef845d5d, $e98575b1,
						  $dc262302, $eb651b88, $23893e81, $d396acc5, $0f6d6ff3, $83f44239,
						  $2e0b4482, $a4842004, $69c8f04a, $9e1f9b5e, $21c66842, $f6e96c9a,
						  $670c9c61, $abd388f0, $6a51a0d2, $d8542f68, $960fa728, $ab5133a3,
						  $6eef0b6c, $137a3be4, $ba3bf050, $7efb2a98, $a1f1651d, $39af0176,
						  $66ca593e, $82430e88, $8cee8619, $456f9fb4, $7d84a5c3, $3b8b5ebe,
						  $e06f75d8, $85c12073, $401a449f, $56c16aa6, $4ed3aa62, $363f7706,
						  $1bfedf72, $429b023d, $37d0d724, $d00a1248, $db0fead3, $49f1c09b,
						  $075372c9, $80991b7b, $25d479d8, $f6e8def7, $e3fe501a, $b6794c3b,
						  $976ce0bd, $04c006ba, $c1a94fb6, $409f60c4, $5e5c9ec2, $196a2463,
						  $68fb6faf, $3e6c53b5, $1339b2eb, $3b52ec6f, $6dfc511f, $9b30952c,
						  $cc814544, $af5ebd09, $bee3d004, $de334afd, $660f2807, $192e4bb3,
						  $c0cba857, $45c8740f, $d20b5f39, $b9d3fbdb, $5579c0bd, $1a60320a,
						  $d6a100c6, $402c7279, $679f25fe, $fb1fa3cc, $8ea5e9f8, $db3222f8,
						  $3c7516df, $fd616b15, $2f501ec8, $ad0552ab, $323db5fa, $fd238760,
						  $53317b48, $3e00df82, $9e5c57bb, $ca6f8ca0, $1a87562e, $df1769db,
						  $d542a8f6, $287effc3, $ac6732c6, $8c4f5573, $695b27b0, $bbca58c8,
						  $e1ffa35d, $b8f011a0, $10fa3d98, $fd2183b8, $4afcb56c, $2dd1d35b,
						  $9a53e479, $b6f84565, $d28e49bc, $4bfb9790, $e1ddf2da, $a4cb7e33,
						  $62fb1341, $cee4c6e8, $ef20cada, $36774c01, $d07e9efe, $2bf11fb4,
						  $95dbda4d, $ae909198, $eaad8e71, $6b93d5a0, $d08ed1d0, $afc725e0,
						  $8e3c5b2f, $8e7594b7, $8ff6e2fb, $f2122b64, $8888b812, $900df01c,
						  $4fad5ea0, $688fc31c, $d1cff191, $b3a8c1ad, $2f2f2218, $be0e1777,
						  $ea752dfe, $8b021fa1, $e5a0cc0f, $b56f74e8, $18acf3d6, $ce89e299,
						  $b4a84fe0, $fd13e0b7, $7cc43b81, $d2ada8d9, $165fa266, $80957705,
						  $93cc7314, $211a1477, $e6ad2065, $77b5fa86, $c75442f5, $fb9d35cf,
						  $ebcdaf0c, $7b3e89a0, $d6411bd3, $ae1e7e49, $00250e2d, $2071b35e,
						  $226800bb, $57b8e0af, $2464369b, $f009b91e, $5563911d, $59dfa6aa,
						  $78c14389, $d95a537f, $207d5ba2, $02e5b9c5, $83260376, $6295cfa9,
						  $11c81968, $4e734a41, $b3472dca, $7b14a94a, $1b510052, $9a532915,
						  $d60f573f, $bc9bc6e4, $2b60a476, $81e67400, $08ba6fb5, $571be91f,
						  $f296ec6b, $2a0dd915, $b6636521, $e7b9f9b6, $ff34052e, $c5855664,
						  $53b02d5d, $a99f8fa1, $08ba4799, $6e85076a
			),
			//SBox[1]
			(
						 $4b7a70e9, $b5b32944, $db75092e, $c4192623, $ad6ea6b0, $49a7df7d,
						  $9cee60b8, $8fedb266, $ecaa8c71, $699a17ff, $5664526c, $c2b19ee1,
						  $193602a5, $75094c29, $a0591340, $e4183a3e, $3f54989a, $5b429d65,
						  $6b8fe4d6, $99f73fd6, $a1d29c07, $efe830f5, $4d2d38e6, $f0255dc1,
						  $4cdd2086, $8470eb26, $6382e9c6, $021ecc5e, $09686b3f, $3ebaefc9,
						  $3c971814, $6b6a70a1, $687f3584, $52a0e286, $b79c5305, $aa500737,
						  $3e07841c, $7fdeae5c, $8e7d44ec, $5716f2b8, $b03ada37, $f0500c0d,
						  $f01c1f04, $0200b3ff, $ae0cf51a, $3cb574b2, $25837a58, $dc0921bd,
						  $d19113f9, $7ca92ff6, $94324773, $22f54701, $3ae5e581, $37c2dadc,
						  $c8b57634, $9af3dda7, $a9446146, $0fd0030e, $ecc8c73e, $a4751e41,
						  $e238cd99, $3bea0e2f, $3280bba1, $183eb331, $4e548b38, $4f6db908,
						  $6f420d03, $f60a04bf, $2cb81290, $24977c79, $5679b072, $bcaf89af,
						  $de9a771f, $d9930810, $b38bae12, $dccf3f2e, $5512721f, $2e6b7124,
						  $501adde6, $9f84cd87, $7a584718, $7408da17, $bc9f9abc, $e94b7d8c,
						  $ec7aec3a, $db851dfa, $63094366, $c464c3d2, $ef1c1847, $3215d908,
						  $dd433b37, $24c2ba16, $12a14d43, $2a65c451, $50940002, $133ae4dd,
						  $71dff89e, $10314e55, $81ac77d6, $5f11199b, $043556f1, $d7a3c76b,
						  $3c11183b, $5924a509, $f28fe6ed, $97f1fbfa, $9ebabf2c, $1e153c6e,
						  $86e34570, $eae96fb1, $860e5e0a, $5a3e2ab3, $771fe71c, $4e3d06fa,
						  $2965dcb9, $99e71d0f, $803e89d6, $5266c825, $2e4cc978, $9c10b36a,
						  $c6150eba, $94e2ea78, $a5fc3c53, $1e0a2df4, $f2f74ea7, $361d2b3d,
						  $1939260f, $19c27960, $5223a708, $f71312b6, $ebadfe6e, $eac31f66,
						  $e3bc4595, $a67bc883, $b17f37d1, $018cff28, $c332ddef, $be6c5aa5,
						  $65582185, $68ab9802, $eecea50f, $db2f953b, $2aef7dad, $5b6e2f84,
						  $1521b628, $29076170, $ecdd4775, $619f1510, $13cca830, $eb61bd96,
						  $0334fe1e, $aa0363cf, $b5735c90, $4c70a239, $d59e9e0b, $cbaade14,
						  $eecc86bc, $60622ca7, $9cab5cab, $b2f3846e, $648b1eaf, $19bdf0ca,
						  $a02369b9, $655abb50, $40685a32, $3c2ab4b3, $319ee9d5, $c021b8f7,
						  $9b540b19, $875fa099, $95f7997e, $623d7da8, $f837889a, $97e32d77,
						  $11ed935f, $16681281, $0e358829, $c7e61fd6, $96dedfa1, $7858ba99,
						  $57f584a5, $1b227263, $9b83c3ff, $1ac24696, $cdb30aeb, $532e3054,
						  $8fd948e4, $6dbc3128, $58ebf2ef, $34c6ffea, $fe28ed61, $ee7c3c73,
						  $5d4a14d9, $e864b7e3, $42105d14, $203e13e0, $45eee2b6, $a3aaabea,
						  $db6c4f15, $facb4fd0, $c742f442, $ef6abbb5, $654f3b1d, $41cd2105,
						  $d81e799e, $86854dc7, $e44b476a, $3d816250, $cf62a1f2, $5b8d2646,
						  $fc8883a0, $c1c7b6a3, $7f1524c3, $69cb7492, $47848a0b, $5692b285,
						  $095bbf00, $ad19489d, $1462b174, $23820e00, $58428d2a, $0c55f5ea,
						  $1dadf43e, $233f7061, $3372f092, $8d937e41, $d65fecf1, $6c223bdb,
						  $7cde3759, $cbee7460, $4085f2a7, $ce77326e, $a6078084, $19f8509e,
						  $e8efd855, $61d99735, $a969a7aa, $c50c06c2, $5a04abfc, $800bcadc,
						  $9e447a2e, $c3453484, $fdd56705, $0e1e9ec9, $db73dbd3, $105588cd,
						  $675fda79, $e3674340, $c5c43465, $713e38d8, $3d28f89e, $f16dff20,
						  $153e21e7, $8fb03d4a, $e6e39f2b, $db83adf7
				),
				//SBox[2]
				(
					 $e93d5a68, $948140f7, $f64c261c, $94692934, $411520f7, $7602d4f7,
						  $bcf46b2e, $d4a20068, $d4082471, $3320f46a, $43b7d4b7, $500061af,
						  $1e39f62e, $97244546, $14214f74, $bf8b8840, $4d95fc1d, $96b591af,
						  $70f4ddd3, $66a02f45, $bfbc09ec, $03bd9785, $7fac6dd0, $31cb8504,
						  $96eb27b3, $55fd3941, $da2547e6, $abca0a9a, $28507825, $530429f4,
						  $0a2c86da, $e9b66dfb, $68dc1462, $d7486900, $680ec0a4, $27a18dee,
						  $4f3ffea2, $e887ad8c, $b58ce006, $7af4d6b6, $aace1e7c, $d3375fec,
						  $ce78a399, $406b2a42, $20fe9e35, $d9f385b9, $ee39d7ab, $3b124e8b,
						  $1dc9faf7, $4b6d1856, $26a36631, $eae397b2, $3a6efa74, $dd5b4332,
						  $6841e7f7, $ca7820fb, $fb0af54e, $d8feb397, $454056ac, $ba489527,
						  $55533a3a, $20838d87, $fe6ba9b7, $d096954b, $55a867bc, $a1159a58,
						  $cca92963, $99e1db33, $a62a4a56, $3f3125f9, $5ef47e1c, $9029317c,
						  $fdf8e802, $04272f70, $80bb155c, $05282ce3, $95c11548, $e4c66d22,
						  $48c1133f, $c70f86dc, $07f9c9ee, $41041f0f, $404779a4, $5d886e17,
						  $325f51eb, $d59bc0d1, $f2bcc18f, $41113564, $257b7834, $602a9c60,
						  $dff8e8a3, $1f636c1b, $0e12b4c2, $02e1329e, $af664fd1, $cad18115,
						  $6b2395e0, $333e92e1, $3b240b62, $eebeb922, $85b2a20e, $e6ba0d99,
						  $de720c8c, $2da2f728, $d0127845, $95b794fd, $647d0862, $e7ccf5f0,
						  $5449a36f, $877d48fa, $c39dfd27, $f33e8d1e, $0a476341, $992eff74,
						  $3a6f6eab, $f4f8fd37, $a812dc60, $a1ebddf8, $991be14c, $db6e6b0d,
						  $c67b5510, $6d672c37, $2765d43b, $dcd0e804, $f1290dc7, $cc00ffa3,
						  $b5390f92, $690fed0b, $667b9ffb, $cedb7d9c, $a091cf0b, $d9155ea3,
						  $bb132f88, $515bad24, $7b9479bf, $763bd6eb, $37392eb3, $cc115979,
						  $8026e297, $f42e312d, $6842ada7, $c66a2b3b, $12754ccc, $782ef11c,
						  $6a124237, $b79251e7, $06a1bbe6, $4bfb6350, $1a6b1018, $11caedfa,
						  $3d25bdd8, $e2e1c3c9, $44421659, $0a121386, $d90cec6e, $d5abea2a,
						  $64af674e, $da86a85f, $bebfe988, $64e4c3fe, $9dbc8057, $f0f7c086,
						  $60787bf8, $6003604d, $d1fd8346, $f6381fb0, $7745ae04, $d736fccc,
						  $83426b33, $f01eab71, $b0804187, $3c005e5f, $77a057be, $bde8ae24,
						  $55464299, $bf582e61, $4e58f48f, $f2ddfda2, $f474ef38, $8789bdc2,
						  $5366f9c3, $c8b38e74, $b475f255, $46fcd9b9, $7aeb2661, $8b1ddf84,
						  $846a0e79, $915f95e2, $466e598e, $20b45770, $8cd55591, $c902de4c,
						  $b90bace1, $bb8205d0, $11a86248, $7574a99e, $b77f19b6, $e0a9dc09,
						  $662d09a1, $c4324633, $e85a1f02, $09f0be8c, $4a99a025, $1d6efe10,
						  $1ab93d1d, $0ba5a4df, $a186f20f, $2868f169, $dcb7da83, $573906fe,
						  $a1e2ce9b, $4fcd7f52, $50115e01, $a70683fa, $a002b5c4, $0de6d027,
						  $9af88c27, $773f8641, $c3604c06, $61a806b5, $f0177a28, $c0f586e0,
						  $006058aa, $30dc7d62, $11e69ed7, $2338ea63, $53c2dd94, $c2c21634,
						  $bbcbee56, $90bcb6de, $ebfc7da1, $ce591d76, $6f05e409, $4b7c0188,
						  $39720a3d, $7c927c24, $86e3725f, $724d9db9, $1ac15bb4, $d39eb8fc,
						  $ed545578, $08fca5b5, $d83d7cd3, $4dad0fc4, $1e50ef5e, $b161e6f8,
						  $a28514d9, $6c51133c, $6fd5c7e7, $56e14ec4, $362abfce, $ddc6c837,
						  $d79a3234, $92638212, $670efa8e, $406000e0
				),
				//SBox[3]
				(
						  $3a39ce37, $d3faf5cf, $abc27737, $5ac52d1b, $5cb0679e, $4fa33742,
						  $d3822740, $99bc9bbe, $d5118e9d, $bf0f7315, $d62d1c7e, $c700c47b,
						  $b78c1b6b, $21a19045, $b26eb1be, $6a366eb4, $5748ab2f, $bc946e79,
						  $c6a376d2, $6549c2c8, $530ff8ee, $468dde7d, $d5730a1d, $4cd04dc6,
						  $2939bbdb, $a9ba4650, $ac9526e8, $be5ee304, $a1fad5f0, $6a2d519a,
                    $63ef8ce2, $9a86ee22, $c089c2b8, $43242ef6, $a51e03aa, $9cf2d0a4,
                    $83c061ba, $9be96a4d, $8fe51550, $ba645bd6, $2826a2f9, $a73a3ae1,
						  $4ba99586, $ef5562e9, $c72fefd3, $f752f7da, $3f046f69, $77fa0a59,
						  $80e4a915, $87b08601, $9b09e6ad, $3b3ee593, $e990fd5a, $9e34d797,
                    $2cf0b7d9, $022b8b51, $96d5ac3a, $017da67d, $d1cf3ed6, $7c7d2d28,
                    $1f9f25cf, $adf2b89b, $5ad6b472, $5a88f54c, $e029ac71, $e019a5e6,
                    $47b0acfd, $ed93fa9b, $e8d3c48d, $283b57cc, $f8d56629, $79132e28,
                    $785f0191, $ed756055, $f7960e44, $e3d35e8c, $15056dd4, $88f46dba,
                    $03a16125, $0564f0bd, $c3eb9e15, $3c9057a2, $97271aec, $a93a072a,
                    $1b3f6d9b, $1e6321f5, $f59c66fb, $26dcf319, $7533d928, $b155fdf5,
                    $03563482, $8aba3cbb, $28517711, $c20ad9f8, $abcc5167, $ccad925f,
						  $4de81751, $3830dc8e, $379d5862, $9320f991, $ea7a90c2, $fb3e7bce,
                    $5121ce64, $774fbe32, $a8b6e37e, $c3293d46, $48de5369, $6413e680,
                    $a2ae0810, $dd6db224, $69852dfd, $09072166, $b39a460a, $6445c0dd,
                    $586cdecf, $1c20c8ae, $5bbef7dd, $1b588d40, $ccd2017f, $6bb4e3bb,
                    $dda26a7e, $3a59ff45, $3e350a44, $bcb4cdd5, $72eacea8, $fa6484bb,
                    $8d6612ae, $bf3c6f47, $d29be463, $542f5d9e, $aec2771b, $f64e6370,
                    $740e0d8d, $e75b1357, $f8721671, $af537d5d, $4040cb08, $4eb4e2cc,
                    $34d2466a, $0115af84, $e1b00428, $95983a1d, $06b89fb4, $ce6ea048,
                    $6f3f3b82, $3520ab82, $011a1d4b, $277227f8, $611560b1, $e7933fdc,
						  $bb3a792b, $344525bd, $a08839e1, $51ce794b, $2f32c9b7, $a01fbac9,
						  $e01cc87e, $bcc7d1f6, $cf0111c3, $a1e8aac7, $1a908749, $d44fbd9a,
						  $d0dadecb, $d50ada38, $0339c32a, $c6913667, $8df9317c, $e0b12b4f,
						  $f79e59b7, $43f5bb3a, $f2d519ff, $27d9459c, $bf97222c, $15e6fc2a,
						  $0f91fc71, $9b941525, $fae59361, $ceb69ceb, $c2a86459, $12baa8d1,
						  $b6c1075e, $e3056a0c, $10d25065, $cb03a442, $e0ec6e0e, $1698db3b,
						  $4c98a0be, $3278e964, $9f1f9532, $e0d392df, $d3a0342b, $8971f21e,
						  $1b0a7441, $4ba3348c, $c5be7120, $c37632d8, $df359f8d, $9b992f2e,
						  $e60b6f47, $0fe3f11d, $e54cda54, $1edad891, $ce6279cf, $cd3e7e6f,
						  $1618b166, $fd2c1d05, $848fd2c5, $f6fb2299, $f523f357, $a6327623,
						  $93a83531, $56cccd02, $acf08162, $5a75ebb5, $6e163697, $88d273cc,
						  $de966292, $81b949d0, $4c50901b, $71c65614, $e6c6c7bd, $327a140a,
						  $45e1d006, $c3f27b9a, $c9aa53fd, $62a80f00, $bb25bfe2, $35bdd2f6,
						  $71126905, $b2040222, $b6cbcf7c, $cd769c2b, $53113ec0, $1640e3d3,
						  $38abbd60, $2547adf0, $ba38209c, $f746ce76, $77afa1c5, $20756060,
						  $85cbfe4e, $8ae88dd8, $7aaaf9b0, $4cf9aa7e, $1948c25c, $02fb8a8c,
						  $01c36ae4, $d6ebe1f9, $90d4f869, $a65cdea0, $3f09252d, $c208e69f,
						  $b74e6132, $ce77e25b, $578fdfe3, $3ac372e6
				)
			);
begin
	//Expensive key setup
	if (cost < 4) or (cost > 31) then
		raise EBCryptException.CreateFmt(SBcryptCostRangeError, [cost]); //'BCrypt cost factor must be between 4..31 (%d)'

	Len := Length(key);
	if (Len > BCRYPT_MaxKeyLen) then //maximum of 72 bytes
		raise EBCryptException.CreateFmt(SKeyRangeError, [Len]); //'Key must be between 1 and 72 bytes long (%d)'

	if Length(salt) <> BCRYPT_SALT_LEN then
		raise EBCryptException.Create(SSaltLengthError); //'Salt must be 16 bytes'

	//Copy S and P boxes into local state
	Move(SBox, Result.SBoxM, Sizeof(SBox));
	Move(PBox, Result.PBoxM, Sizeof(PBox));

	Self.ExpandKey({var} Result, salt, key);

	//rounds = 2^cost
	rounds := 1 shl cost;

	for i := 1 to rounds do
	begin
		Self.ExpandKey({var} Result, zero, key);
		Self.ExpandKey({var} Result, zero, salt);
	end;

	//Result := what it is
end;

class procedure TBCrypt.ExpandKey(var State: TBlowfishData; salt, key: array of Byte);
var
	i, j, k: Integer;
	A: DWord;
	KeyB: PByteArray;
	Block: array[0..7] of Byte;
	len: Integer;
	//saltHalf: Integer;
	saltHalfIndex: Integer;
begin
	//TODO: burn all stack variables

	//ExpandKey phase of the Expensive key setup
	len := Length(key);
	if (len > BCRYPT_MaxKeyLen) then
		raise EBCryptException.CreateFmt(SKeyRangeError, [len]); //'Key must be between 1 and 72 bytes long (%d)'

	{
		XOR all the subkeys in the P-array with the encryption key
		The first 32 bits of the key are XORed with P1, the next 32 bits with P2, and so on.
		The key is viewed as being cyclic; when the process reaches the end of the key,
		it starts reusing bits from the beginning to XOR with subkeys.
	}
	if len > 0 then
	begin
		KeyB := PByteArray(@key[0]);
		k := 0;
		for i := 0 to 17 do
		begin
			A :=      KeyB[(k+3) mod len];
			A := A + (KeyB[(k+2) mod len] shl 8);
			A := A + (KeyB[(k+1) mod len] shl 16);
			A := A + (KeyB[k]             shl 24);
			State.PBoxM[i] := State.PBoxM[i] xor A;
			k := (k+4) mod len;
		end;
	end;

	//Blowfsh-encrypt the first 64 bits of its salt argument using the current state of the key schedule.
	BlowfishEncryptECB(State, @salt[0], @Block);

	//The resulting ciphertext replaces subkeys P1 and P2.
	State.PBoxM[0] := Block[3] + (Block[2] shl 8) + (Block[1] shl 16) + (Block[0] shl 24);
	State.PBoxM[1] := Block[7] + (Block[6] shl 8) + (Block[5] shl 16) + (Block[4] shl 24);

{$RANGECHECKS OFF}
	saltHalfIndex := 8;
	for i := 1 to 8 do
	begin
		//That same ciphertext is also XORed with the second 64-bits of salt

		//Delphi compiler is not worth its salt; it doesn't do hoisting ("Any compiler worth its salt will hoist" - Eric Brumer C++ compiler team)
		//Salt is 0..15 (0..7 is first block, 8..15 is second block)
		PLongWord(@block[0])^ := PLongWord(@block[0])^ xor PLongWord(@salt[saltHalfIndex  ])^;
		PLongWord(@block[4])^ := PLongWord(@block[4])^ xor PLongWord(@salt[saltHalfIndex+4])^;

		//saltHalf := saltHalf xor 1;
		saltHalfIndex := saltHalfIndex xor 8;

		//and the result encrypted with the new state of the key schedule
		BlowfishEncryptECB(State, @Block, @Block);

		// The output of the second encryption replaces subkeys P3 and P4. (P[2] and P[3])
		State.PBoxM[i*2] :=   Block[3] + (Block[2] shl 8) + (Block[1] shl 16) + (Block[0] shl 24);
		State.PBoxM[i*2+1] := Block[7] + (Block[6] shl 8) + (Block[5] shl 16) + (Block[4] shl 24);
	end;

	//When ExpandKey finishes replacing entries in the P-Array, it continues on replacing S-box entries two at a time.
	for j := 0 to 3 do
	begin
		for i := 0 to 127 do
		begin
			//That same ciphertext is also XORed with the second 64-bits of salt
			//Delphi compiler is not worth its salt; it doesn't do hoisting ("Any compiler worth its salt will hoist" - Eric Brumer C++ compiler team)
			//Salt is 0..15 (0..7 is first block, 8..15 is second block)
			PLongWord(@block[0])^ := PLongWord(@block[0])^ xor PLongWord(@salt[saltHalfIndex  ])^;
			PLongWord(@block[4])^ := PLongWord(@block[4])^ xor PLongWord(@salt[saltHalfIndex+4])^;

			//saltHalf := saltHalf xor 1;
			saltHalfIndex := saltHalfIndex xor 8;

			//and the result encrypted with the new state of the key schedule
			BlowfishEncryptECB(State, @Block, @Block);

			// The output of the second encryption replaces subkeys S1 and P2. (S[0] and S[1])
			State.SBoxM[j, i*2  ] := Block[3] + (Block[2] shl 8) + (Block[1] shl 16) + (Block[0] shl 24);
			State.SBoxM[j, i*2+1] := Block[7] + (Block[6] shl 8) + (Block[5] shl 16) + (Block[4] shl 24);
		end;
	end;
{$RANGECHECKS ON}
end;

class function TBCrypt.CheckPassword(const password: UnicodeString; const salt, hash: array of Byte; const Cost: Integer; out PasswordRehashNeeded: Boolean): Boolean;
var
	candidateHash: TBytes;
	len: Integer;
	freq, t1, t2: Int64;
begin
	Result := False;
	PasswordRehashNeeded := False;

	if not QueryPerformanceFrequency({var}freq) then
		freq := -1; //avoid a division by zero

	//Measure how long it takes to run the hash. If it's too quick, it's time to increase the cost
	if not QueryPerformanceCounter(t1) then t1 := 0;

	candidateHash := TBCrypt.HashPassword(password, salt, cost);

	if not QueryPerformanceCounter(t2) then t2 := 0;

	len := Length(hash);
	if Length(candidateHash) <> len then
		Exit;

	Result := CompareMem(@candidateHash[0], @hash[0], len);

	//Based on how long it took to hash the password, see if a rehash is needed to increase the cost
	PasswordRehashNeeded := TBcrypt.PasswordRehashNeededCore(version, cost, cost, (t2-t1)/freq*1000);
end;

{$IFNDEF UNICODE}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

class function TBCrypt.TryParseHashString(const hashString: string;
		out version: string; out Cost: Integer; out Salt: TBytes): Boolean;
var
	s: string;
	n: Integer; //current index

	function GetNextToken(var index: Integer): string;
	var
		maxLen: Integer;
		startIndex, copyLen: Integer;
	begin
		Result := '';
		maxLen := Length(hashString);
		if (index > maxLen) then
			Exit;

		//Make sure we start on a "$" token divider
		if hashString[index] <> '$' then
			Exit;

		//Move past the "$" into the token value
		Inc(index);

		startIndex := index;
		copyLen := 0;

		while (index <= maxLen) and (hashString[index] <> '$') do
		begin
			Inc(index);
			Inc(copyLen);
		end;
		if copyLen > 0 then
			Result := Copy(hashString, startIndex, copyLen);
	end;
begin
	Result := False;

	{
		Pick apart our specially formatted hash string

			$2a$nn$[22 character salt, b64 encoded][32 character hash, b64 encoded]

		We also need to accept version 2, the original version

			$2$nn$[22 character salt, b64 encoded][32 character hash, b64 encoded]
	}
	if Length(hashString) < 27 then //"$2$4$" + 22 = 27
		Exit;

	//Get version token
	n := 1;
	Version := GetNextToken({var}n);
	if Version = '' then
		Exit;
	if Version[1] <> '2' then
		Exit;
	if Length(Version) > 1 then
	begin
		if Length(Version) > 2 then //we only comprehend "2c", where c is a character
			Exit;
		if not CharInSet(Version[2], ['A'..'Z', 'a'..'z']) then
			Exit;
	end;

	//Get cost token
	s := GetNextToken({var}n);
	Cost := StrToIntDef(s, -1);
	if Cost <= 0 then
		Exit;

	//Get remaining 22 character salt
//	s := Copy(GetNextToken({var}n), 1, 22);
	s := GetNextToken({var}n);
	s := Copy(s, 1, 22);
	if Length(s) < 22 then
		Exit;
	try
		Salt := BsdBase64Decode(s); //salt is always 16 bytes (16 bytes * 4/3 --> 22 base64 characters)
	except
		on EBCryptException do //E.g. invalid base64 string
			Exit;
	end;

	Result := True;
end;

class function TBCrypt.CheckPassword(const password: UnicodeString; const expectedHashString: string; out PasswordRehashNeeded: Boolean): Boolean;
var
	version: string;
	cost: Integer;
	salt: TBytes;
	hash: TBytes;
	actualHashString: string;
	freq, t1, t2: Int64;
begin
	PasswordRehashNeeded := False;

	if not QueryPerformanceFrequency({var}freq) then
		freq := -1; //avoid a division by zero

	if not TryParseHashString(expectedHashString, {out}version, {out}cost, {out}salt) then
		raise Exception.Create(SInvalidHashString);

	//Measure how long it takes to run the hash. If it's too quick, it's time to increase the cost
	if not QueryPerformanceCounter(t1) then t1 := 0;

	hash := TBCrypt.HashPassword(password, salt, cost);

	if not QueryPerformanceCounter(t2) then t2 := 0;

	actualHashString := FormatPasswordHashForBsd(version, cost, salt, hash);

	//Result := (actualHashString = expectedHashString);
	Result := TBcrypt.TimingSafeSameString(actualHashString, expectedHashString);

	//Based on how long it took to hash the password, see if a rehash is needed to increase the cost
	PasswordRehashNeeded := TBcrypt.PasswordRehashNeededCore(version, cost, cost, (t2-t1)/freq*1000);
end;

class function TBCrypt.BsdBase64Encode(const data: array of Byte; BytesToEncode: Integer): string;

	function EncodePacket(b1, b2, b3: Byte; Len: Integer): string;
	begin
		Result := '';

		Result := Result + BsdBase64EncodeTable[b1 shr 2];
		Result := Result + BsdBase64EncodeTable[((b1 and $03) shl 4) or (b2 shr 4)];
		if Len < 2 then Exit;

		Result := Result + BsdBase64EncodeTable[((b2 and $0f) shl 2) or (b3 shr 6)];
		if Len < 3 then Exit;

		Result := Result + BsdBase64EncodeTable[b3 and $3f];
	end;

var
	i: Integer;
	len: Integer;
	b1, b2: Integer;
begin
	Result := '';

	len := BytesToEncode;
	if len = 0 then
		Exit;

	if len > Length(data) then
		raise EBCryptException.Create(SInvalidLength);

	//encode whole 3-byte chunks  TV4S 6ytw fsfv kgY8 jIuc Drjc 8deX 1s.
	i := Low(data);
	while len >= 3 do
	begin
		Result := Result+EncodePacket(data[i], data[i+1], data[i+2], 3);
		Inc(i, 3);
		Dec(len, 3);
	end;

	if len = 0 then
		Exit;

	//encode partial final chunk
	Assert(len < 3);
	if len >= 1 then
		b1 := data[i]
	else
		b1 := 0;
	if len >= 2 then
		b2 := data[i+1]
	else
		b2 := 0;
	Result := Result+EncodePacket(b1, b2, 0, len);
end;

class function TBCrypt.SelfTest: Boolean;
begin
	Result := True;

	Result := Result and SelfTestA;  //known test vectors
	Result := Result and SelfTestB;  //the base64 encoder/decoder
	Result := Result and SelfTestC;  //unicode strings
	Result := Result and SelfTestD;  //different length passwords
	Result := Result and SelfTestE;  //salt RNG
	Result := Result and SelfTestF;  //example of correct horse battery staple
	Result := Result and SelfTestG;  //72 byte key length (71characters + 1 null = 72)
	Result := Result and SelfTestH;  //check out handling of strings longer than 255 characters
	Result := Result and SelfTestI;  //check that we use unicode compatible composition (NFKC) on passwords
	Result := Result and SelfTestJ;  //check that composed and decomposed strings both validate to the same
	Result := Result and SelfTestK;  //SASLprep rules for passwords

end;

class function TBCrypt.FormatPasswordHashForBsd(const Version: string; const cost: Integer; const salt, hash: array of Byte): string;
var
	saltString: string;
	hashString: string;
begin
	saltString := BsdBase64Encode(salt, Length(salt));
	hashString := BsdBase64Encode(hash, Length(hash)-1); //Yes, everything except the last byte.
		//OpenBSD, in the pseudo-base64 implementation, doesn't include the last byte of the hash
		//Nobody knows why, but that's what all existing tests do - so it's what i do

	Result := Format('$%s$%.2d$%s%s', [Version, cost, saltString, hashString]);
end;

class function TBCrypt.BsdBase64Decode(const s: string): TBytes;

	function Char64(character: Char): Integer;
	begin
		if (Ord(character) > Length(BsdBase64DecodeTable)) then
		begin
			Result := -1;
			Exit;
		end;

		Result := BsdBase64DecodeTable[character];
	end;

	procedure Append(value: Byte);
	var
		i: Integer;
	begin
		i := Length(Result);
		SetLength(Result, i+1);
		Result[i] := value;
	end;

var
	i: Integer;
	len: Integer;
	c1, c2, c3, c4: Integer;
begin
	SetLength(Result, 0);

	len := Length(s);
	i := 1;
	while i <= len do
	begin
		// We'll need to have at least 2 character to form one byte.
		// Anything less is invalid
		if (i+1) > len then
		begin
			raise EBCryptException.Create(SInvalidHashString); //'Invalid base64 hash string'
//			Break;
		end;

		c1 := Char64(s[i]);
		Inc(i);
		c2 := Char64(s[i]);
		Inc(i);

		if (c1 = -1) or (c2 = -1) then
		begin
			raise EBCryptException.Create(SInvalidHashString); //'Invalid base64 hash string'
//			Break;
		end;

		//Now we have at least one byte in c1|c2
		// c1 = ..111111
		// c2 = ..112222
		Append( ((c1 and $3f) shl 2) or (c2 shr 4) );

		//If there's a 3rd character, then we can use c2|c3 to form the second byte
		if (i > len) then
			Break;
		c3 := Char64(s[i]);
		Inc(i);

		if (c3 = -1) then
		begin
			raise EBCryptException.Create(SInvalidHashString); //'Invalid base64 hash string'
//			Break;
		end;

		//Now we have the next byte in c2|c3
		// c2 = ..112222
		// c3 = ..222233
		Append( ((c2 and $0f) shl 4) or (c3 shr 2) );

		//If there's a 4th caracter, then we can use c3|c4 to form the third byte
		if i > len then
			Break;
		c4 := Char64(s[i]);
		Inc(i);

		if (c4 = -1) then
		begin
			raise EBCryptException.Create(SInvalidHashString); //'Invalid base64 hash string'
//			Break;
		end;

		//Now we have the next byte in c3|c4
		// c3 = ..222233
		// c4 = ..333333
		Append( ((c3 and $03) shl 6) or c4 );
	end;
end;


//Requires Vista or newer. Similar functionality existed in Windows 2000 under FoldString
//function NormalizeString(NormForm: Cardinal; SrcString: PWideChar; SrcLength: Cardinal; DstString: PWideChar; DstLength: Cardinal): Integer; stdcall; external 'Normaliz.dll';

class function TBCrypt.PasswordStringPrep(const Source: UnicodeString): TBytes;
var
	normalizedLength: Integer;
	normalized: UnicodeString;
	strLen: Integer;
	dw: DWORD;
const
	CodePage = CP_UTF8;
	SLenError = '[PasswordStringPrep] Could not get length of destination string. Error %d (%s)';
	SConvertStringError = '[PasswordStringPrep] Could not convert utf16 to utf8 string. Error %d (%s)';
begin
	if Length(Source) = 0 then
	begin
		SetLength(Result, 0);
		Exit;
	end;

	{
		20161122
		NIST Special Publication 800-63-3B (Digital Authentication Guideline - Authentication and Lifecycle Management)
		https://pages.nist.gov/800-63-3/sp800-63b.html

		Reminds us to normalize the string (using either NFKC or NFKD).
			- K (Compatibility normalization): decomposes ligatures into base compatibilty characters
			- C (Composition):                 adds character+diacritic into single code point (if possible)
			- D (Decomposition):               separates an accented character into the letter and the diacritic

		SASLprep (rfc4013), like StringPrep (rfc3454) both specified NFKC:

			Before: Noe¨l
			After:  Noël

		Spaces
		======

		RFC7613 - Preparation, Enforcement, and Comparison of Internationalized Strings Representing Usernames and Passwords,
		like RFC4013 that it obsoletes, reminds us to normalize all the differnet unicode space characters into the
		standard single U+0020 SPACE:

			2.  Additional Mapping Rule: Any instances of non-ASCII space MUST be mapped to ASCII space (U+0020);
				 a non-ASCII space is any Unicode code point having a Unicode general category of "Zs"
				 (with the  exception of U+0020).

					U+0020	SPACE
					U+00A0	NO-BREAK SPACE
					U+1680	OGHAM SPACE MARK
					U+2000	EN QUAD
					U+2001	EM QUAD
					U+2002	EN SPACE
					U+2003	EM SPACE
					U+2004	THREE-PER-EM SPACE
					U+2005	FOUR-PER-EM SPACE
					U+2006	SIX-PER-EM SPACE
					U+2007	FIGURE SPACE
					U+2008	PUNCTUATION SPACE
					U+2009	THIN SPACE
					U+200A	HAIR SPACE
					U+202F	NARROW NO-BREAK SPACE
					U+205F	MEDIUM MATHEMATICAL SPACE
					U+3000	IDEOGRAPHIC SPACE

		This is handled by NFC.
	}

	//We use concrete variable for length, because i've seen it return asking for 64 bytes for a 6 byte string
//	normalizedLength := NormalizeString(5, PWideChar(Source), Length(Source), nil, 0);
	normalizedLength := FoldStringW(MAP_FOLDCZONE, PWideChar(Source), Length(Source), nil, 0);
	if normalizedLength = 0 then
	begin
		dw := GetLastError;
		raise EConvertError.CreateFmt(SLenError, [dw, SysErrorMessage(dw)]);
	end;

	// Allocate memory for destination string
	SetLength(normalized, normalizedLength);

	// Now do it for real
	try
//		normalizedLength := NormalizeString(5, PWideChar(Source), Length(Source), PWideChar(normalized), Length(normalized));
		normalizedLength := FoldStringW(MAP_FOLDCZONE, PWideChar(Source), Length(Source), PWideChar(normalized), Length(normalized));
		if normalizedLength = 0 then
		begin
			dw := GetLastError;
			raise EConvertError.CreateFmt(SLenError, [dw, SysErrorMessage(dw)]);
		end;

		{
			Now perform the conversion of UTF-16 to UTF-8
		}
		// Determine real size of destination string, in bytes
		strLen := WideCharToMultiByte(CodePage, 0,
				PWideChar(normalized), normalizedLength, //Source
				nil, 0, //Destination
				nil, nil);
		if strLen = 0 then
		begin
			dw := GetLastError;
			raise EConvertError.CreateFmt(SLenError, [dw, SysErrorMessage(dw)]);
		end;

		// Allocate memory for destination string
		SetLength(Result, strLen+1); //+1 for the null terminator

		// Convert source UTF-16 string (WideString) to the destination using the code-page
		strLen := WideCharToMultiByte(CodePage, 0,
				PWideChar(normalized), normalizedLength, //Source
				PAnsiChar(Result), strLen, //Destination
				nil, nil);
		if strLen = 0 then
		begin
			dw := GetLastError;
			raise EConvertError.CreateFmt(SConvertStringError, [dw, SysErrorMessage(dw)]);
		end;

		//Set the null terminator
		Result[strLen] := 0;
	finally
		//Burn the intermediate normalized form
		BurnString(normalized);
	end;
end;


class function TBCrypt.SelfTestB: Boolean;
var
	i: Integer;
	salt: string;
	encoded: string;
	data: TBytes;
	recoded: string;
const
	SSelfTestFailed = 'BSDBase64 encoder self-test failed';
begin
	for i := Low(TestVectors) to High(TestVectors) do
	begin
		salt := TestVectors[i,2];

		encoded := Copy(salt, 8, 22); //salt is always 22 characters

		data := TBCrypt.BsdBase64Decode(encoded);

		recoded := TBCrypt.BsdBase64Encode(data, Length(data));
		if (recoded <> encoded) then
			raise Exception.Create(SSelfTestFailed);
	end;

	Result := True;
end;

class function TBCrypt.SelfTestA: Boolean;
var
	i: Integer;

	procedure t(const password: UnicodeString; const HashSalt: string; const ExpectedHashString: string);
	var
		version: string;
		cost: Integer;
		salt: TBytes;
		hash: TBytes;
		actualHashString: string;
	begin
		//Extract "$2a$06$If6bvum7DFjUnE9p2uDeDu" rounds and base64 salt from the HashSalt
		if not TBCrypt.TryParseHashString(HashSalt, {out}version, {out}cost, {out}salt) then
			raise Exception.Create('bcrypt self-test failed: invalid versionsalt "'+HashSalt+'"');

		hash := TBCrypt.HashPassword(password, salt, cost);
		actualHashString := TBCrypt.FormatPasswordHashForBsd(version, cost, salt, hash);

		if actualHashString <> ExpectedHashString then
			raise Exception.CreateFmt('bcrypt self-test failed. Password: "%s". Actual hash "%s". Expected hash: "%s"', [password, actualHashString, ExpectedHashString]);
	end;

begin
	for i := Low(TestVectors) to High(TestVectors) do
	begin
		t(TestVectors[i,1], TestVectors[i,2], TestVectors[i,3] );
	end;

	Result := True;
end;

class function TBCrypt.SelfTestC: Boolean;
var
	s: UnicodeString;
	hash: string;
	rehashNeeded: Boolean;
const
	n: UnicodeString=''; //n=nothing.
			//Work around bug in Delphi compiler when building widestrings
			//http://stackoverflow.com/a/7031942/12597
begin
	{
		We test that the it doesn't choke on international characters
		This was a bug in a version of bcrypt somewhere, that we do not intend to duplicate
	}
	s := n+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0; //U+03C0: Greek Small Letter Pi
	hash := TBCrypt.HashPassword(s);
	if not TBCrypt.CheckPassword(s, hash, {out}rehashNeeded) then
		raise Exception.Create('Failed to validate unicode string "'+s+'"');


	s := n+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0; //U+03C0: Greek Small Letter Pi
	hash := TBCrypt.HashPassword(s);
	if not TBCrypt.CheckPassword(s, hash, {out}rehashNeeded) then
		raise Exception.Create('Failed to validate unicode string "'+s+'"');

	Result := True;
end;

class function TBCrypt.GenRandomBytes(len: Integer; const data: Pointer): HRESULT;
var
	hProv: THandle;
const
	PROV_RSA_FULL = 1;
	CRYPT_VERIFYCONTEXT = DWORD($F0000000);
	CRYPT_SILENT         = $00000040;
begin
	if not CryptAcquireContextW(hPRov, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT or CRYPT_SILENT) then
	begin
		Result := HResultFromWin32(GetLastError);
		Exit;
	end;
	try
		if not CryptGenRandom(hProv, len, data) then
		begin
			Result := HResultFromWin32(GetLastError);
			Exit;
		end;
	finally
		CryptReleaseContext(hProv, 0);
	end;

	Result := S_OK;
end;

class function TBCrypt.GetModernCost(SampleCost: Integer; SampleHashDurationMS: Real): Integer;
begin
	{
		Given a Cost factor, and how long it took to compute the hash, figure out the cost needed to get a hashing duration
		between 250ms - 500ms. Maybe 200-400. It probably needs some discussion.

		This function is used not only as part of the benchmark to decide on a cost for new passwords, but we
		also use it to measure how long it took to check an existing password, and decide if it was too quick
		and the password hash needs to be upgraded.

		What speed to use?
		==================

		At the time of deployment in 1976, crypt could hash fewer than 4 passwords per second. (250 ms per password)
		In 1977, on a VAX-11/780, crypt (MD5) could be evaluated about 3.6 times per second.   (277 ms per password)

		At the time of publication of BCrypt (1999) the default costs were:
			- Normal User: 6
			- the Superuser: 8

			"Of course, whatever cost people choose should be reevaluated from time to time."

		Speed tests from the paper from 1999 for Cost 5
			- OpenBSD 2.3, P5 133 MHz:     156.2 ms  (6.4 crypts/sec)
			- x86 assembler, P5 133 MHz:     4.4 ms  (22.5 crypts/sec)
			- Alpha 21164A 600 MHz:          1.6 ms  (62.5 crypts/sec)

		That means that these speeds would for Cost 6 be:
			- OpenBSD 2.3, P5 133 MHz:     312.5 ms
			- x86 assembler, P5 133 MHz:     8.9 ms
			- Alpha 21164A 600 MHz:          3.2 ms

		For cost 8 (superuser) these would be:
			- OpenBSD 2.3, P5 133 MHz:   1,250.0 ms
			- x86 assembler, P5 133 MHz:    35.6 ms
			- Alpha 21164A 600 MHz:         12.8 ms


		For regular users we want to target between 250-500ms per hash (i.e. no more than 500 ms, no less than 250 ms)
		We would like to target 300ms as the calculation time, but not exceed 500ms.

		- if our calculation time was less than 250ms, then we can double it
		- if our calculation time was more than 500ms, then we can half it

		Speed of pins
		=============

		For a 5-digit pin (59,049 combinations) the time to exhaust all combinations is:
				200 ms --> 3.2 hours
				250 ms --> 4.1 hours
				400 ms --> 6.6 hours
				500 ms --> 8.2 hours
	}
	Result := BCRYPT_COST; //never recommend lower than the fixed BCRYPT_COST

	if (SampleCost <=0 ) or (SampleHashDurationMS <= 0) then
		Exit;

	//while the duration is too low, double it
	while SampleHashDurationMS < 250 do
	begin
		Inc(SampleCost);
		SampleHashDurationMS := SampleHashDurationMS*2;
	end;

	//if the duration is too high, halve it (as long as we are still above the minumum cost)
	while (SampleHashDurationMS > 500) and (SampleCost > BCRYPT_COST) do
	begin
		Dec(SampleCost);
		SampleHashDurationMS := SampleHashDurationMS / 2;
	end;

	Result := SampleCost;
end;

class function TBCrypt.GetModernCost_Benchmark: Integer;
var
	t1, t2, freq: Int64;
const
	testCost = 5; //4;
begin
	{
		Run a quick benchmark on the current machine to see how fast this PC is.
		A cost of 4 is the lowest we allow, so we will hash with that.

		The problem with the Moore's Law approach, is that Moore's Law failed in 2004:
				http://preshing.com/20120208/a-look-back-at-single-threaded-cpu-performance/
				http://preshing.com/images/float-point-perf.png
	}
	Result := BCRYPT_COST; //don't ever go less than the default cost

	if not QueryPerformanceFrequency({var}freq) then Exit;
	if (freq = 0) then Exit;
	if not QueryPerformanceCounter(t1) then Exit;
	if (t1 = 0) then Exit;

	TBCrypt.HashPassword('Benchmark', testCost);

	if not QueryPerformanceCounter(t2) then Exit;
	if t2=0 then Exit;

	Result := TBCrypt.GetModernCost(testCost, (t2-t1)/freq * 1000);
end;

class function TBCrypt.SelfTestD: Boolean;
var
	i: Integer;
	password: string;
	hash: string;
begin
	for i := 0 to 56 do
	begin
		password := Copy('The quick brown fox jumped over the lazy dog then sat on a log', 1, i);
		hash := TBCrypt.HashPassword(password, 4);
		if (hash = '') then
			raise Exception.Create('hash creation failed');
	end;

	Result := True;
end;

class function TBCrypt.SelfTestE: Boolean;
var
	salt: TBytes;
begin
	{
		Ensure that the salt generator creates salt, and it is of the correct length.
	}
	salt := TBCrypt.GenerateSalt;
	if Length(salt) <> BCRYPT_SALT_LEN then
		raise Exception.Create('BCrypt selftest failed; invalid salt length');

	Result := True;
end;

class function TBCrypt.SelfTestF: Boolean;
var
	rehashNeeded: Boolean;
begin
	{
		Validate a known password hash
	}
	//OutputDebugString('SAMPLING ON');
	Result := TBCrypt.CheckPassword('correctbatteryhorsestapler', '$2a$12$mACnM5lzNigHMaf7O1py1O3vlf6.BA8k8x3IoJ.Tq3IB/2e7g61Km', {out}rehashNeeded);
	//OutputDebugString('SAMPLING OFF');
end;

class function TBCrypt.SelfTestG: Boolean;
var
	s55, s56, s57: TBytes;
	s70, s71, s72, s73, s74: TBytes;
	sCopy: TBytes;
	salt: TBytes;

	function BytesEqual(const data1, data2: array of Byte): Boolean;
	begin
		Result := True;

		if Length(data1) <> Length(data2) then
		begin
			Result := False;
			Exit;
		end;

		if Length(data1) = 0 then
			Exit;

		Result := CompareMem(@data1[0], @data2[0], Length(data1))
   end;
const
	testPassword = 'The quick brown fox jumped over the lazy dog then sat on a log. The sixth sick';
	//                                                                   56^               ^72
begin
	Result := True;

	salt := TBCrypt.GenerateSalt;

	s55 := TBCrypt.HashPassword(Copy(testPassword, 1, 55), salt, 4);
	s56 := TBCrypt.HashPassword(Copy(testPassword, 1, 56), salt, 4);
	s57 := TBCrypt.HashPassword(Copy(testPassword, 1, 57), salt, 4);

	//First make sure that we can generate the same hash twice with the same password and salt
	sCopy := TBCrypt.HashPassword(Copy(testPassword, 1, 55), salt, 4);
	if not BytesEqual(s55, sCopy) then
		Result := False;

	//The old limit was 56 byte (55 characters + 1 null terminator). Make sure that we can at least handle 57
	if BytesEqual(s55, s56) then
		Result := False;
	if BytesEqual(s56, s57) then
		Result := False;

	//Finally, do the same test around the 72 character limit. If you specify more than 71 characters, it is cut off
	//20161025: Change to match what OpenBSD does: they cut off the byte array - null terminator and all - after 72 bytes
	s70 := TBCrypt.HashPassword(Copy(testPassword, 1, 70), salt, 4);
	s71 := TBCrypt.HashPassword(Copy(testPassword, 1, 71), salt, 4);
	s72 := TBCrypt.HashPassword(Copy(testPassword, 1, 72), salt, 4);
	s73 := TBCrypt.HashPassword(Copy(testPassword, 1, 73), salt, 4);
	s74 := TBCrypt.HashPassword(Copy(testPassword, 1, 74), salt, 4);

	if BytesEqual(s70, s71) then //we should have still had room
		Result := False;

	if BytesEqual(s71, s72) then //the 72 character version has no room for the null terminator anymore, so it's also going to be different
		Result := False;

	if not BytesEqual(s72, s73) then //we should have stopped at 72 characters
		Result := False;
	if not BytesEqual(s72, s74) then //definitely don't overflow into 74
		Result := False;
end;

class function TBCrypt.SelfTestH: Boolean;
var
	szPassword: string;
	rehashNeeded: Boolean;
begin
{
	A bug was discovered in the OpenBSD implemenation of bcrypt in February of 2014

		http://undeadly.org/cgi?action=article&sid=20140224132743
		http://marc.info/?l=openbsd-misc&m=139320023202696

	They were storing the length of their strings in an unsigned char (i.e. 0..255)
	If a password was longer than 255 characters, it would overflow and wrap at 255.

	They fixed their bug, and decided that everyone should use a new version string (2b).

	Delphi doesn't have this problem, because Delphi does strings correctly (length prefixed, null terminated, reference counted)
}
	szPassword := StringOfChar('a', 260);

	Result := TBCrypt.CheckPassword(szPassword, '$2a$04$QqpSfI8JYX8HSxNwW5yx8Ohp12sNboonE6e5jfnGZ0fD4ZZwQkOOK', {out}rehashNeeded);
end;

class function TBCrypt.SelfTestI: Boolean;
var
	password: UnicodeString;
	utf8: TBytes;
const
	n: UnicodeString=''; //n=nothing.
			//Work around bug in Delphi compiler when building widestrings
			//http://stackoverflow.com/a/7031942/12597
begin
	{
		Before: A + ¨ + fi + n
				A:  U+0041
				¨:  U+0308 Combining Diaeresis
				fi: U+FB01 Latin Small Ligature Fi
				n:  U+006E

		Normalized:  Ä + f + i + n
				Ä:  U+00C4  Latin Capital Letter A with Diaeresis
				f:  U+0066
				i:  U+0069
				n:  U+006E

		Final UTF-8:
				Ä:  0xC3 0x84
				f:  0x66
				i:  0x69
				n:  0x6E
				\0: 0x00
	}
	password := n + WideChar('A') + WideChar(#$0308) + WideChar(#$FB01) + WideChar('n');

	utf8 := TBCrypt.PasswordStringPrep(password);

	{
		0xC3 0x84 0x66 0x69 0x6E 0x00
	}
	Result := (Length(utf8) = 6);
	Result := Result and (utf8[0] = $c3);
	Result := Result and (utf8[1] = $84);
	Result := Result and (utf8[2] = $66);
	Result := Result and (utf8[3] = $69);
	Result := Result and (utf8[4] = $6e);
	Result := Result and (utf8[5] = $00); //we do include the null terminator
end;

class function TBCrypt.SelfTestJ: Boolean;
var
	password1: UnicodeString;
	password2: UnicodeString;
	hash: string;
	passwordRehashNeeded: Boolean;
const
	n: UnicodeString=''; //n=nothing.
			//Work around bug in Delphi compiler when building widestrings
			//http://stackoverflow.com/a/7031942/12597
begin
	{
		There are four Unicode normalization schemes:

			NFC	Composition
			NFD	Decomposition
			NFKC	Compatible Composition   <--- the one we use
			NFKD	Compatible Decomposition

		NIST Special Publication 800-63-3B (Digital Authentication Guideline - Authentication and Lifecycle Management)
			says that passwords should have unicode normalization KC or KD applied.

		RFC7613 (SASLprep) specifies the use of NFKC
			https://tools.ietf.org/html/rfc7613
			 Preparation, Enforcement, and Comparison of Internationalized Strings Representing Usernames and Passwords

		Original
				A:  U+0041
				¨:  U+0308 Combining Diaeresis
				fi: U+FB01 Latin Small Ligature Fi
				n:  U+006E

		Normalized:  Ä + f + i + n
				Ä:  U+00C4  Latin Capital Letter A with Diaeresis
				f:  U+0066
				i:  U+0069
				n:  U+006E
	}
	password1 := n + 'A' + #$0308 + #$FB01 + 'n';
	password2 := n + #$00C4 + 'f' + 'i' + 'n';

	hash := TBCrypt.HashPassword(password1, 4);

	Result := TBCrypt.CheckPassword(password2, hash, {out}passwordRehashNeeded);
end;

class function TBCrypt.SelfTestK: Boolean;
var
	pass: UnicodeString;

	function CheckUtf8(const s: UnicodeString; Expected: array of Byte): Boolean;
	var
		data: TBytes;
	begin
		Result := False;

		data := TBCrypt.PasswordStringPrep(s);

		if Length(data) <> Length(Expected) then
			Exit;

		if not CompareMem(@data[0], @Expected[0], Length(data)) then
			Exit;

		Result := True;
	end;

begin
	{
		1. Width-Mapping Rule: Fullwidth and halfwidth characters MUST NOT be mapped to their decomposition mappings
			(see Unicode Standard Annex #11 [UAX11](https://tools.ietf.org/html/rfc7613#ref-UAX11)).
	}

	Result := True;

	{
		Fullwidth "Test"

			U+FF34  FULLWIDTH LATIN CAPITAL LETTER T   UTF8 0xEF 0xBC 0xB4
			U+FF45  FULLWIDTH LATIN SMALL LETTER   e   UTF8 0xEF 0xBD 0x85
			U+FF53  FULLWIDTH LATIN SMALL LETTER   s   UTF8 0xEF 0xBD 0x93
			U+FF54  FULLWIDTH LATIN SMALL LETTER   t   UTF8 0xEF 0xBD 0x94
	}
	//pass := #$ff34 + #$ff45 + #$ff53 + #$ff54;
	//if not CheckUtf8(pass, [$ef, $bc, $b4, $ef, $bd, $85, $bd, $93, $ef, $bd, $94, 0]) then Result := False;


	{
		Halfwidth
			U+FFC3  HALFWIDTH HANGUL LETTER AE         UTF8 0xEF 0xBF 0x83
	}
	//pass := #$ffc3;
	//if not CheckUtf8(pass, [$ef, $bf, $83, 0]) then Result := False;


	{
		2.  Additional Mapping Rule: Any instances of non-ASCII space MUST be mapped to ASCII space (U+0020);
			 a non-ASCII space is any Unicode code point having a Unicode general category of "Zs"
			 (with the  exception of U+0020).

			U+0020	SPACE
			U+00A0	NO-BREAK SPACE
			U+1680	OGHAM SPACE MARK
			U+2000	EN QUAD
			U+2001	EM QUAD
			U+2002	EN SPACE
			U+2003	EM SPACE
			U+2004	THREE-PER-EM SPACE
			U+2005	FOUR-PER-EM SPACE
			U+2006	SIX-PER-EM SPACE
			U+2007	FIGURE SPACE
			U+2008	PUNCTUATION SPACE
			U+2009	THIN SPACE
			U+200A	HAIR SPACE
			U+202F	NARROW NO-BREAK SPACE
			U+205F	MEDIUM MATHEMATICAL SPACE
			U+3000	IDEOGRAPHIC SPACE
	}
	pass := #$0020;
	if not CheckUtf8(pass, [$20, 0]) then Result := False;
	pass := #$00A0;
	if not CheckUtf8(pass, [$20, 0]) then Result := False;
	pass := #$2000;
	if not CheckUtf8(pass, [$20, 0]) then Result := False;
end;

class function TBCrypt.PasswordRehashNeeded(const HashString: string): Boolean;
var
	idealCost: Integer;
	version: string;
	cost: Integer;
	salt: TBytes;
begin
	{
		As computing power increases, we might from time to time need to increase the cost factor.
		Run a microbenchmark to get the desired cost factor, and compare it to the cost factor stored in the Hash.

		If the desired cost is higher, then we need to rehash.
		The time to do this is after we have validated the user's credentials. This way we can silently rehash their
		known good password.
	}
	if not TBCrypt.TryParseHashString(hashString, {out}version, {out}cost, {out}salt) then
	begin
		//raise EBCryptException.Create('PasswordRehashNeeded: Could not parse Hash string');
		Result := True; //if they expected it to be a valid BCrypt hash, and it's not, then they definitely need to rehash something
		Exit;
	end;

	idealCost := TBCrypt.GetModernCost_Benchmark;

	if (cost < idealCost) then
	begin
		Result := True;
		Exit;
	end;

	Result := False;
end;

class function TBCrypt.PasswordRehashNeededCore(const Version: string; const Cost: Integer;
		SampleCost: Integer; SampleHashDurationMS: Real): Boolean;
var
	idealCost: Integer;
begin
	{
		This core routine is used by two paths:
			- checking a real password, where we are passed the actual cost
	}
	//If the cost is lower than our hard-coded minimum, then they need to rehash
	if Cost < BCRYPT_COST then
	begin
		Result := True;
		Exit;
	end;

	if AnsiSameText(version, '2') //original spec, that didn't define UTF-8 or what to do with a null terminator
			or AnsiSameText(version, '2x') //hashes generated by a buggy version of crypt_blowfish; didn't handle unicode correctly
			or AnsiSameText(version, '2y') //fixed version of crypt_blowfish
//			or AnsiSameText(version, '2a') //buggy version of OpenBSD that stored password length in a Byte
			then
	begin
		//It should be version 2a. (eventually will be moved to 2b)
		Result := True;
		Exit;
	end;

	//Use the supplied hashing time as a benchmark
	if (SampleCost > 0) and (SampleHashDurationMS > 0) then
	begin
		idealCost := GetModernCost(SampleCost, SampleHashDurationMS);
		if (idealCost > Cost) then
		begin
			Result := True;
			Exit;
		end;
	end;

	Result := False; //no rehash needed
end;

class function TBCrypt.TimingSafeSameString(const Safe, User: string): Boolean;
var
	i: Integer;
	safeLen, userLen: Integer;
	nDiff: Integer;
begin
	{
		A timing safe equals comparison

		To prevent leaking length information, it is important that user input is always used as the second parameter.

			safe The internal (safe) value to be checked
			user The user submitted (unsafe) value

		Returns True if the two strings are identical.
	}

	safeLen := Length(Safe);
	userLen := Length(User);

	// Set the result to the difference between the lengths
	nDiff := safeLen - userLen;

	// Note that we ALWAYS iterate over the user-supplied length
	// This is to prevent leaking length information
	for i := 0 to userLen-1 do
	begin
		// Using mod here is a trick to prevent notices.
		// It's safe, since if the lengths are different nDiff is already non-zero
		nDiff := nDiff or (
				Ord(Safe[(i mod safeLen) + 1])
				xor
				Ord(User[i+1])
		);
	end;

	 // They are only identical strings if nDiff is exactly zero
	Result := (nDiff = 0);
end;

{$IFDEF BCryptUnitTests}

{ TBCryptTests }

procedure TBCryptTests.Benchmark;
var
	cost: Integer;
	t1, t2, freq: Int64;
	durationMS: Double;
begin
	if not FindCmdLineSwitch('SlowUnitTests', ['/', '-'], True) then
	begin
		Status('Very slow test. Specify -SlowUnitTests to include');
		Exit;
	end;

	if not QueryPerformanceFrequency(freq) then
		freq := -1; //to avoid division by zero

	Status('Cost factor	Duration (ms)');

	cost := 4; //the minimum supported bcrypt cost

	while (cost <= 31) do
	begin
		QueryPerformanceCounter({out}t1);
		TBCrypt.HashPassword('benchmark', cost);
		QueryPerformanceCounter({out}t2);

		durationMS := (t2-t1)/freq * 1000;

		Status(Format('%d	%.4f', [cost, durationMS]));

		Inc(cost);

		if durationMS > 15000 then
			Break;
	end;

	Status(Self.GetCompilerOptions);
end;

function TBCryptTests.GetCompilerOptions: string;

	procedure Add(s: string);
	begin
		if Result <> '' then
			Result := Result+#13#10;

		Result := Result + s;
	end;

begin
	Result := '';

(*
	Other than for certain debugging situations, you should never have a need to turn optimizations off.
*)
{$IFOPT O+} //OPTIMIZATION
	Add('Optimization: ON');
{$ELSE}
	Add('Optimization: OFF');
{$ENDIF}

(*
	Enabling overflow checking slows down your program and makes it somewhat larger, so use {$Q+} only for debugging.
*)
{$IFOPT Q+} //OVERFLOWCHECKS
	Add('Overflow Checking: ON');
{$ELSE}
	Add('Overflow Checking: OFF');
{$ENDIF}

(*
	Enabling range checking slows down your program and makes it somewhat larger.
*)
{$IFOPT R+} //RANGECHECKS
	Add('Range Checking: ON');
{$ELSE}
	Add('Range Checking: OFF');
{$ENDIF}

{$IFOPT W+} //STACKFRAMES
	Add('Stack frames: ON');
{$ELSE}
	Add('Stack frames: OFF');
{$ENDIF}

{$IFOPT I+} //IOCHECKS
	Add('I/O Checking: ON');
{$ELSE}
	Add('I/O Checking: OFF');
{$ENDIF}
end;

procedure TBCryptTests.SelfTest;
begin
	CheckTrue(TBCrypt.SelfTest);
end;

procedure TBCryptTests.SelfTestA_KnownTestVectors;
begin
	if not FindCmdLineSwitch('SlowUnitTests', ['/', '-'], True) then
	begin
		Status('Very slow test. Specify -SlowUnitTests to include');
		Exit;
	end;

	CheckTrue(TBCrypt.SelfTestA);
end;

procedure TBCryptTests.SelfTestB_Base64EncoderDecoder;
begin
	CheckTrue(TBCrypt.SelfTestB);
end;

procedure TBCryptTests.SelfTestC_UnicodeStrings;
begin
	if not FindCmdLineSwitch('SlowUnitTests', ['/', '-'], True) then
	begin
		Status('Very slow test. Specify -SlowUnitTests to include');
		Exit;
	end;

	CheckTrue(TBCrypt.SelfTestC);
end;

procedure TBCryptTests.SelfTestD_VariableLengthPasswords;
begin
	CheckTrue(TBCrypt.SelfTestD);
end;

procedure TBCryptTests.SelfTestE_SaltRNG;
begin
	CheckTrue(TBCrypt.SelfTestE);
end;

procedure TBCryptTests.SelfTestF_CorrectBattery;
var
	t1, t2, freq: Int64;
begin
	if not QueryPerformanceFrequency(freq) then
		freq := -1; //to avoid division by zero

	QueryPerformanceCounter(t1);
	CheckTrue(TBcrypt.SelfTestF);
	QueryPerformanceCounter(t2);

	Status(Format('%.4f ms', [(t2-t1)/freq*1000]));

	Status(GetCompilerOptions);
end;

procedure TBCryptTests.SelfTestG_PasswordLength;
begin
	CheckTrue(TBCrypt.SelfTestG);
end;

procedure TBCryptTests.SelfTestH_OpenBSDLengthBug;
begin
	CheckTrue(TBCrypt.SelfTestH);
end;

procedure TBCryptTests.SelfTestI_UnicodeCompatibleComposition;
begin
	CheckTrue(TBCrypt.SelfTestI);
end;

procedure TBCryptTests.SelfTestJ_NormalizedPasswordsMatch;
begin
	CheckTrue(TBCrypt.SelfTestJ);
end;

procedure TBCryptTests.SelfTestK_SASLprep;
begin
	CheckTrue(TBCrypt.SelfTestK);
end;

procedure TBCryptTests.SpeedTests;
var
	freq: Int64;

	procedure TimeIt(Cost: Integer);
	var
		t1, t2: Int64;
		timems: Real;
		bestTime: Real;
		n: Integer;
	begin
		bestTime := 0;

		n := 5;
		while n > 0 do
		begin
			QueryPerformanceCounter(t1);
			TBCrypt.HashPassword('corrent horse battery staple', Cost);
			QueryPerformanceCounter(t2);

			Dec(n);

			timems := (t2-t1)/freq*1000; //milliseconds
			if (bestTime = 0) or (timems < bestTime) then
			begin
				bestTime := timems;
				n := 5; //we found a new min. Wait until we get five min in a row
			end;
		end;

		Status(Format('BCrypt, cost=%d: %.2f ms', [cost, bestTime]));
	end;
begin
	if not QueryPerformanceFrequency(freq) then
		freq := -1; //negative one, so that a division results in negative ticks, rather than infinite or exception

	TimeIt(8);
	TimeIt(9);
	TimeIt(10);
	TimeIt(11);
	TimeIt(12);
	TimeIt(13);
	TimeIt(14);
	TimeIt(15);
//OutputDebugString('SAMPLING ON');
	TimeIt(16);
//OutputDebugString('SAMPLING OFF');
//	TimeIt(17);
end;

procedure TBCryptTests.Test_ManualSystem;
var
	salt: TBytes;
	hash: TBytes;
	password: string;
	validPassword: Boolean;
	passwordRehashNeeded: Boolean;
begin
	{
		Normally bcrypt hashes to an OpenBSD password-file compatible string (i.e. $2b$...)
		But if you want to handle the salt, and hash bytes directly, and come up with your own serialization format
		you can do that too
	}
	password := 'correct horse battery staple';
	salt := TBCrypt.GenerateSalt;

	hash := TBCrypt.HashPassword(password, salt, 4); //4 is the lowest cost we'll accept

	validPassword := TBCrypt.CheckPassword(password, salt, hash, 4, {out}passwordRehashNeeded);
	CheckTrue(validPassword);
	CheckTrue(passwordRehashNeeded, 'Expected passwordRehashNeede to be true, given that we used a cost of 4');
end;

procedure TBCryptTests.Test_ParseHashString;

	procedure t(HashString: string; ExpectedResult: Boolean; ExpectedVersion: string; ExpectedCost: Integer;
			ExpectedSaltBase64: string; TestSituation: string);
	var
		actualVersion: string;
		actualCost: Integer;
		actualSalt: TBytes;
		actualSaltBase64: string;
		actualResult: Boolean;
	begin
//		expectedSalt := TBCrypt.BsdBase64Decode(ExpectedSaltBase64);
		actualResult := TBCrypt.TryParseHashString(HashString, {out}actualVersion, {out}actualCost, {out}actualSalt);

		CheckEquals(expectedResult, actualResult, HashString+'   '+TestSituation);
		if actualResult then
		begin
			CheckEquals(ExpectedVersion, actualVersion, 'Version for hash: '+HashString+'   '+TestSituation);
			CheckEquals(ExpectedCost, actualCost, 'Cost for hash: '+HashString+'   '+TestSituation);

			actualSaltBase64 := TBCrypt.BsdBase64Encode(actualSalt, Length(actualSalt));
			CheckEquals(expectedSaltBase64, actualSaltBase64, 'Salt for hash: '+HashString+'   '+TestSituation);
		end;
	end;
begin
	t('$2c$06$DCq7YPn5Rq63x1Lad4cll.', True, '2c', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Future version suffix - 2c');
	t('$2b$06$DCq7YPn5Rq63x1Lad4cll.', True, '2b', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Current version suffix - 2b');
	t('$2a$06$DCq7YPn5Rq63x1Lad4cll.', True, '2a', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Most common version suffix - 2a');
	t('$2y$06$DCq7YPn5Rq63x1Lad4cll.', True, '2y', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Crypt_blowfish fixed hash version - 2y');
	t('$2x$06$DCq7YPn5Rq63x1Lad4cll.', True, '2x', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Crypt_blowfish buggy hash version - 2x');
	t('$2$06$DCq7YPn5Rq63x1Lad4cll.',  True, '2',  6, 'DCq7YPn5Rq63x1Lad4cll.', 'Original bcrypt version - 2');

	t('$2c$6$DCq7YPn5Rq63x1Lad4cll.', True, '2c', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Single digit cost factor "6"');
	t('$2b$06$DCq7YPn5Rq63x1Lad4cll.', True, '2b', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Two digit cost factor "06"');
	t('$2a$006$DCq7YPn5Rq63x1Lad4cll.', True, '2a', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Triple digit cost factor "006"');

	t('$2c$0$DCq7YPn5Rq63x1Lad4cll.', False, '2c', 0, 'DCq7YPn5Rq63x1Lad4cll.', 'Zero cost factor is not valid');
	t('$2c$$DCq7YPn5Rq63x1Lad4cll.', False, '2c', 0, 'DCq7YPn5Rq63x1Lad4cll.', 'Empty cost factor is not valid');

	t('$3$6$DCq7YPn5Rq63x1Lad4cll.', False, '3', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Unknown version number');
	t('$20$6$DCq7YPn5Rq63x1Lad4cll.', False, '20', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Version suffix can only be a letter');
	t('$2_$6$DCq7YPn5Rq63x1Lad4cll.', False, '2_', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Version suffix can only be a letter');
	t('$2ca$6$DCq7YPn5Rq63x1Lad4cll.', False, '2ca', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Version string can be at most 2 characters ("2" + "letter")');

	t('$2C$6$DCq7YPn5Rq63x1Lad4cll.', True, '2C', 6, 'DCq7YPn5Rq63x1Lad4cll.', 'Version suffix may also be UPPERCASE');

	t('$2a', False, '2a', 0, '', 'Missing everything after version');
	t('$2a$', False, '2a', 0, '', 'Missing everything after version');
	t('$2a$6', False, '2a', 6, '', 'Missing everything after cost factor');
	t('$2a$6$', False, '2a', 6, '', 'Missing everything after cost factor');
	t('$2a$6$DCq7YPn5Rq63x1Lad4cll', False, '2a', 6, 'DCq7YPn5Rq63x1Lad4cll', 'Salt is too short');
end;

{$ENDIF}

initialization
{$IFDEF BCryptUnitTests}
	RegisterTest('Library', TBCryptTests.Suite);
{$ENDIF}

{
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org/>
}
end.
