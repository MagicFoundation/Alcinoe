unit ECCProcess;

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

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
   - Nicolas Marchand (MC)

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

*)

interface

{$I Synopse.inc}

uses
  SysUtils,
  Classes,
  SynCommons,
  SynTable,
  SynEcc,
  SynCrypto;

/// end-user command to create a new private/public key file
// - as used in the ECC.dpr command-line sample project
function ECCCommandNew(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const Issuer: RawUTF8; StartDate: TDateTime; ExpirationDays: integer;
  const SavePassword: RawUTF8; SavePassordRounds, SplitFiles: integer): TFileName;

/// end-user command to create a renew a .private file password
// - as used in the ECC.dpr command-line sample project
function ECCCommandRekey(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const SavePassword: RawUTF8; SavePassordRounds: integer): TFileName;

/// end-user command to sign a file using a private key file
// - as used in the ECC.dpr command-line sample project
function ECCCommandSignFile(const FileToSign, AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const MetaNameValuePairs: array of const): TFileName;

/// end-user command to verify a file signature
// - as used in the ECC.dpr command-line sample project
function ECCCommandVerifyFile(const FileToVerify, AuthPubKey: TFileName;
  const AuthBase64: RawUTF8): TECCValidity;

/// end-user command to create a .inc pascal source file from a private key file
// - ready to be included within the executable binary as private secret
// - as used in the ECC.dpr command-line sample project
function ECCCommandSourceFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const ConstName, Comment, PassWord: RawUTF8): TFileName;

/// end-user command to create a .json base-64 text array from a set of public key files
// - ready to be included e.g. as settings of any server
// - ECCCommandChainCertificates(['*']) will create a 'chain.ca' of all
// public key files in the current folder
// - as used in the ECC.dpr command-line sample project
function ECCCommandChainCertificates(const CertFiles: array of RawUTF8): TFileName;

/// end-user command to display the json information from a .private file
// - as used in the ECC.dpr command-line sample project
function ECCCommandInfoPrivFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer): RawUTF8;

/// end-user command to encrypt a file with the .synecc format
// - as used in the ECC.dpr command-line sample project
procedure ECCCommandCryptFile(const FileToCrypt, DestFile, AuthPubKey: TFileName;
  const AuthBase64, AuthSerial, Password: RawUTF8; PasswordRounds: integer;
  Algo: TECIESAlgo=ecaUnknown);

/// end-user command to decrypt a .synecc file
// - as used in the ECC.dpr command-line sample project
// - if AuthPrivKey is not set, it will search for the stored TECCCertificate.Serial
function ECCCommandDecryptFile(const FileToDecrypt, DestFile, AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const DecryptPassword: RawUTF8; DecryptPasswordRounds: integer;
  Signature: PECCSignatureCertifiedContent; MetaData: PRawJSON): TECCDecrypt;

/// end-user command to verify a .synecc file signature, after decryption
// - as used in the ECC.dpr command-line sample project
// - the supplied signature can be retrieved from ECCCommandDecryptFile()
function ECCCommandVerifyDecryptedFile(const FileToVerify: TFileName;
  const Signature: TECCSignatureCertifiedContent): TECCValidity;

/// end-user command to initialize local cheat.private/.public files
// - as used in the ECC.dpr command-line sample project
// - those files will let new/rekey commands create a .cheat file, encrypted
// using the master cheat.public key, so that the password of the generated
// .private key file could be retrieved using the cheat.private key and its
// password/round, via ECCCommandCheat (ECC cheat)
// - it may be convenient to remember a single password instead of several,
// and security will be enhanced by the fact that cheat.private stays hidden
function ECCCommandCheatInit(const Issuer, CheatPassword: RawUTF8;
  CheatRounds: integer): TFileName;

/// end-user command to display a .private password as stored in its .cheat file
// - as used in the ECC.dpr command-line sample project
// - using the master password/rounds of the local cheat.private key, as
// generated by ECCCommandCheatInit (ECC cheatinit)
function ECCCommandCheat(const PrivateFile: TFileName; const CheatPassword: RawUTF8;
  CheatRounds: integer; out authpass: RawUTF8; out authround: integer): RawUTF8;

/// end-user command to encrypt a file with the symetric .synaead format
// - will use symetric encryption via AES-256-CFB/PKCS7 over PBKDF2_HMAC_SHA256
// - as used in the ECC.dpr command-line sample project
procedure AEADCommandCryptFile(const FileToCrypt, DestFile: TFileName;
  const Password, PasswordSalt: RawUTF8; PasswordRounds: integer);

/// end-user command to decrypt a symetric .synaead file
// - will use symetric encryption via AES-256-CFB/PKCS7 over PBKDF2_HMAC_SHA256
// - as used in the ECC.dpr command-line sample project
procedure AEADCommandDecryptFile(const FileToDecrypt, DestFile: TFileName;
  const Password, PasswordSalt: RawUTF8; PasswordRounds: integer);

type
  /// the actions implemented by ECCCommand()
  // - as used in the ECC.dpr command-line sample project
  // - retrieved from the command line as first parameter
  TECCCommand = (
    ecHelp, ecNew, ecRekey, ecSign, ecVerify, ecSource, ecInfoPriv,
    ecChain, ecChainAll,
    ecCrypt, ecDecrypt, ecInfoCrypt,
    ecAeadCrypt, ecAeadDecrypt,
    ecCheatInit, ecCheat);
  /// the result code returned by ECCCommand()
  // - as used in the ECC.dpr command-line sample project
  TECCCommandError = (
    eccSuccess, eccUnknownCommand, eccValidationError, eccError, eccException);


/// execute the encryption process corresponding to the command line options
// - as used in the ECC.dpr sample project
// - returns the ExitCode expected value (0=eccSuccess)
function ECCCommand(cmd: TECCCommand; const sw: ICommandLine): TECCCommandError;

const
  CHEAT_FILEEXT = '.cheat';
  CHEAT_FILEMASTER = 'cheat';
  CHEAT_ROUNDS = 100000;
  CHEAT_SPLIT = 100;

  AEAD_FILEEXT = '.synaead';
  DEFAULT_AEADROUNDS = 60000;


implementation

procedure CreateCheatFile(secret: TECCCertificateSecret;
  const SavePassword: RawUTF8; SavePasswordRounds: integer);
var json,bin: RawByteString;
    master: TECCCertificate;
    fn: TFileName;
begin
  master := TECCCertificate.Create;
  try
    if master.FromFile(CHEAT_FILEMASTER) then
      try
        if SavePasswordRounds=DEFAULT_ECCROUNDS then
          json := SavePassword else
          json := JSONEncode(['pass',SavePassword,'rounds',SavePasswordRounds]);
        bin := TAESPRNG.Main.AFSplit(pointer(json)^,length(json),CHEAT_SPLIT);
        fn := UTF8ToString(secret.Serial)+CHEAT_FILEEXT;
        FileFromString(master.Encrypt(bin),fn);
      finally
        FillZero(json);
        FillZero(bin);
      end;
  finally
    master.Free;
  end;
end;

function ECCCommandNew(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const Issuer: RawUTF8; StartDate: TDateTime; ExpirationDays: integer;
  const SavePassword: RawUTF8; SavePassordRounds, SplitFiles: integer): TFileName;
var auth,new: TECCCertificateSecret;
begin
  if AuthPrivKey='' then
    auth := nil else
    auth := TECCCertificateSecret.CreateFromSecureFile(AuthPrivKey,AuthPassword,AuthPasswordRounds);
  try
    // generate pair
    new := TECCCertificateSecret.CreateNew(auth,Issuer,ExpirationDays,StartDate);
    try
      // save private key as .private password-protected binary file
      new.SaveToSecureFiles(SavePassword,'.',SplitFiles,64,SavePassordRounds);
      CreateCheatFile(new,SavePassword,SavePassordRounds);
      // save public key as .public JSON file
      result := ChangeFileExt(new.SaveToSecureFileName,ECCCERTIFICATEPUBLIC_FILEEXT);
      new.ToFile(result);
    finally
      new.Free;
    end;
  finally
    auth.Free;
  end;
end;

function ECCCommandRekey(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const SavePassword: RawUTF8; SavePassordRounds: integer): TFileName;
var auth: TECCCertificateSecret;
begin
  auth := TECCCertificateSecret.CreateFromSecureFile(AuthPrivKey,AuthPassword,AuthPasswordRounds);
  try
    auth.SaveToSecureFile(SavePassword,'.',64,SavePassordRounds);
    CreateCheatFile(auth,SavePassword,SavePassordRounds);
    result := auth.SaveToSecureFileName;
  finally
    auth.Free;
  end;
end;

function ECCCommandSignFile(const FileToSign, AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const MetaNameValuePairs: array of const): TFileName;
var auth: TECCCertificateSecret;
begin
  auth := TECCCertificateSecret.CreateFromSecureFile(AuthPrivKey,AuthPassword,AuthPasswordRounds);
  try
    result := auth.SignFile(FileToSign,MetaNameValuePairs);
  finally
    auth.Free;
  end;
end;

function ECCCommandSourceFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const ConstName, Comment, PassWord: RawUTF8): TFileName;
var auth: TECCCertificateSecret;
begin
  auth := TECCCertificateSecret.CreateFromSecureFile(AuthPrivKey,AuthPassword,AuthPasswordRounds);
  try
    result := AuthPrivKey+'.inc';
    FileFromString(auth.SaveToSource(ConstName,Comment,Password),result);
  finally
    auth.Free;
  end;
end;

function ECCCommandVerifyFile(const FileToVerify, AuthPubKey: TFileName;
  const AuthBase64: RawUTF8): TECCValidity;
var content: RawByteString;
    auth: TECCCertificate;
    cert: TECCSignatureCertified;
begin
  content := StringFromFile(FileToVerify);
  if content='' then
    raise EECCException.CreateUTF8('File not found: %',[FileToVerify]);
  cert := TECCSignatureCertified.CreateFromFile(FileToVerify);
  try
    if not cert.Check then begin
      result := ecvInvalidSignature;
      exit;
    end;
    auth := TECCCertificate.Create;
    try
      if auth.FromAuth(AuthPubKey,AuthBase64,cert.AuthoritySerial) then
        result := cert.Verify(auth,pointer(content),length(content)) else
        result := ecvUnknownAuthority;
    finally
      auth.Free;
    end;
  finally
    cert.Free;
  end;
end;

function ECCCommandVerifyDecryptedFile(const FileToVerify: TFileName;
  const Signature: TECCSignatureCertifiedContent): TECCValidity;
var content: RawByteString;
    auth: TECCCertificate;
    cert: TECCSignatureCertified;
begin
  content := StringFromFile(FileToVerify);
  if content='' then
    raise EECCException.CreateUTF8('File not found: %',[FileToVerify]);
  cert := TECCSignatureCertified.CreateFrom(Signature);
  try
    auth := TECCCertificate.Create;
    try
      result := ecvUnknownAuthority;
      if auth.FromAuth('','',cert.AuthoritySerial) then
        result := cert.Verify(auth,pointer(content),length(content));
    finally
      auth.Free;
    end;
  finally
    cert.Free;
  end;
end;

procedure ECCCommandCryptFile(const FileToCrypt, DestFile, AuthPubKey: TFileName;
  const AuthBase64, AuthSerial, Password: RawUTF8; PasswordRounds: integer;
  Algo: TECIESAlgo);
var content: RawByteString;
    auth: TECCCertificate;
begin
  content := StringFromFile(FileToCrypt);
  if content='' then
    raise EECCException.CreateUTF8('File not found: %',[FileToCrypt]);
  auth := TECCCertificate.Create;
  try
    if not auth.FromAuth(AuthPubKey,AuthBase64,AuthSerial) then
      raise EECCException.Create('No public key');
    if not auth.EncryptFile(FileToCrypt,DestFile,Password,PasswordRounds,Algo,true) then
      raise EECCException.CreateUTF8('EncryptFile failed for %',[FileToCrypt]);
  finally
    auth.Free;
    FillZero(content);
  end;
end;

function ECCCommandDecryptFile(const FileToDecrypt, DestFile, AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const DecryptPassword: RawUTF8; DecryptPasswordRounds: integer;
  Signature: PECCSignatureCertifiedContent; MetaData: PRawJSON): TECCDecrypt;
var auth: TECCCertificateSecret;
    head: TECIESHeader;
    priv: TFileName;
begin
  auth := TECCCertificateSecret.Create;
  try
    result := ecdNoPrivateKey;
    if FileExists(AuthPrivKey) then
      priv := AuthPrivKey else begin
      if not ECIESHeaderFile(FileToDecrypt,head) then
        exit;
      priv := UTF8ToString(ECCText(head.recid));
      if not ECCKeyFileFind(priv,true) then
        exit; // not found local .private from header 
    end;
    if not auth.LoadFromSecureFile(priv,AuthPassword,AuthPasswordRounds) then
      exit;
    result := auth.DecryptFile(FileToDecrypt,DestFile,
      DecryptPassword,DecryptPasswordRounds,Signature,MetaData);
  finally
    auth.Free;
  end;
end;

function ECCCommandChainCertificates(const CertFiles: array of RawUTF8): TFileName;
var n,i: integer;
    files: TFileNameDynArray;
begin
  result := '';
  n := length(CertFiles);
  if n=0 then
    exit;
  if (n=1) and (CertFiles[0]='*') then begin
    files := FindFilesDynArrayToFileNames(
      FindFiles('.','*'+ECCCERTIFICATEPUBLIC_FILEEXT));
    result := 'chain'+ECCCERTIFICATES_FILEEXT;
  end else begin
    SetLength(files,n);
    for i := 0 to n-1 do begin
      files[i] := UTF8ToString(CertFiles[i]);
      if not ECCKeyFileFind(files[i],false) then
        exit;
     end;
    result := format('chain%d'+ECCCERTIFICATES_FILEEXT,[GetTickCount64]);
  end;
  with TECCCertificateChain.CreateFromFiles(files) do
  try
    if ValidateItems<>nil then begin
      result := '';
      raise EECCException.Create('Some of the certificates are invalid');
    end;
    SaveToFile(result);
  finally
    Free;
  end;
end;

function ECCCommandInfoPrivFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer): RawUTF8;
var auth: TECCCertificateSecret;
begin
  auth := TECCCertificateSecret.CreateFromSecureFile(AuthPrivKey,AuthPassword,AuthPasswordRounds);
  try
    result := JSONReformat(VariantSaveJSON(auth.ToVariant))
  finally
    auth.Free;
  end;
end;

function ECCCommandCheatInit(const Issuer, CheatPassword: RawUTF8;
  CheatRounds: integer): TFileName;
var new: TECCCertificateSecret;
    priv: RawByteString;
begin
  if FileExists(CHEAT_FILEMASTER+ECCCERTIFICATEPUBLIC_FILEEXT) or
     FileExists(CHEAT_FILEMASTER+ECCCERTIFICATESECRET_FILEEXT) then
    raise EECCException.Create(CHEAT_FILEMASTER+' file already exist');
  // generate pair
  new := TECCCertificateSecret.CreateNew(nil,Issuer);
  try
    // save private key as cheat.private password-protected binary file
    priv := new.SaveToSecureBinary(CheatPassword,128,CheatRounds);
    FileFromString(priv,CHEAT_FILEMASTER+ECCCERTIFICATESECRET_FILEEXT);
    // save public key as mastercheat.public JSON file
    result := CHEAT_FILEMASTER+ECCCERTIFICATEPUBLIC_FILEEXT;
    JSONReformatToFile(VariantSaveJSON(new.ToVariant),result);
  finally
    new.Free;
    FillZero(priv);
  end;
end;

function ECCCommandCheat(const PrivateFile: TFileName; const CheatPassword: RawUTF8;
  CheatRounds: integer; out authpass: RawUTF8; out authround: integer): RawUTF8;
var bin,split,json: RawByteString;
    master: TECCCertificateSecret;
    doc: TDocVariantData;
    fn: TFileName;
    res: TECCDecrypt;
begin
  fn := ChangeFileExt(PrivateFile,CHEAT_FILEEXT);
  bin := StringFromFile(fn);
  if bin='' then
    raise EECCException.CreateUTF8('Unknown file %',[fn]);
  master := TECCCertificateSecret.CreateFromSecureFile(
    CHEAT_FILEMASTER,CheatPassword,CheatRounds);
  try
    res := master.Decrypt(bin,split);
    if res<>ecdDecrypted then
      raise EECCException.CreateUTF8('% on %',[ToText(res)^,fn]);
    json := TAESPRNG.AFUnsplit(split,CHEAT_SPLIT);
    if json='' then
      raise EECCException.CreateUTF8('Incorrect file %',[fn]);
    if not doc.InitJSON(json) then
      doc.InitObject(['pass',json,'rounds',DEFAULT_ECCROUNDS],JSON_OPTIONS_FAST);
    authpass := doc.U['pass'];
    authround := doc.I['rounds'];
    result := doc.ToJSON('','',jsonHumanReadable);
  finally
    master.Free;
    FillZero(json);
  end;
end;

procedure AEADProcess(Encrypt: boolean; var Source: RawByteString;
  const DestFileName: TFileName; const Password, PasswordSalt: RawUTF8; PasswordRounds: integer);
var
  aeskey: THash256;
  dst: RawByteString;
begin
  try
    PBKDF2_HMAC_SHA256(Password,PasswordSalt,PasswordRounds,aeskey,'salt');
    try
      dst := TAESCFBCRC.MACEncrypt(Source,aeskey,Encrypt);
      try
        if dst='' then
          raise EECCException.CreateUTF8('MACEncrypt failed for %',[DestFileName]);
        if not FileFromString(dst,DestFileName) then
          raise EECCException.CreateUTF8('FileFromString failed for %',[DestFileName]);
      finally
        FillZero(dst);
      end;
    finally
      FillZero(aeskey);
    end;
  finally
    FillZero(Source);
  end;
end;

procedure AEADCommandCryptFile(const FileToCrypt, DestFile: TFileName;
  const Password, PasswordSalt: RawUTF8; PasswordRounds: integer);
var plain: RawByteString;
    dest: TFileName;
begin
  plain := StringFromFile(FileToCrypt);
  if plain='' then
    raise EECCException.CreateUTF8('File not found: %',[FileToCrypt]);
  if DestFile='' then
    dest := FileToCrypt+AEAD_FILEEXT else
    dest := DestFile;
  AEADProcess({encrypt=}true,plain,dest,Password,PasswordSalt,PasswordRounds);
end;


procedure AEADCommandDecryptFile(const FileToDecrypt, DestFile: TFileName;
  const Password, PasswordSalt: RawUTF8; PasswordRounds: integer);
var encrypted: RawByteString;
    dest: TFileName;
begin
  encrypted := StringFromFile(FileToDecrypt);
  if encrypted='' then
    raise EECCException.CreateUTF8('File not found: %',[FileToDeCrypt]);
  if DestFile='' then
    dest := GetFileNameWithoutExt(FileToDecrypt) else
    dest := DestFile;
  AEADProcess({encrypt=}false,encrypted,dest,Password,PasswordSalt,PasswordRounds);
end;



function ECCCommand(cmd: TECCCommand; const sw: ICommandLine): TECCCommandError;

  procedure WriteVerif(verif: TECCValidity; const filename: TFileName; const sw: ICommandLine);
  var res: string;
  begin
    res := SysUtils.LowerCase(GetCaptionFromEnum(TypeInfo(TECCValidity),ord(verif)));
    if verif in ECC_VALIDSIGN then
      sw.Text(' % file verified as %.',[filename,res],ccLightGreen) else begin
      sw.Text(' % file verification failure: % (%).',[filename,res,ord(verif)],ccLightRed);
      result := eccValidationError;
    end;
  end;
  procedure WritePassword(const privfile: TFileName;
    const pass: RawUTF8; rounds: integer);
  var a: TECDHEAuth;
      privkey: RawUTF8;
  begin
    if privfile='' then
      exit;
    sw.Text('Corresponding TSynPersistentWithPassword.ComputePassword:',[]);
    sw.Text(' encryption %',[TSynPersistentWithPassword.ComputePassword(pass)],ccLightBlue);
    privkey := StringToUTF8(copy(GetFileNameWithoutExt(privfile),1,8));
    for a := low(a) to high(a) do
      sw.Text(' % %',[ToText(a)^,TECDHEProtocol.FromKeyCompute(
        privkey,pass,rounds,'',a)],ccLightBlue);
    sw.Text('',[]);
  end;

var issuer, authpass, savepass, constname, comment, json: RawUTF8;
    meta: RawJSON;
    start: TDateTime;
    authrounds, days, saverounds, splitfiles: integer;
    msg: string;
    origfile,auth,newfile,jsonfile: TFileName;
    algo: TECIESAlgo;
    decrypt: TECCDecrypt;
    decryptsign: TECCSignatureCertifiedContent;
begin
  result := eccSuccess;
  if sw=nil then
    raise EECCException.Create('ECCCommand(nil)');
  try
  try
    case cmd of
    ecNew: begin
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .private file name of the signing authority.'#13#10+
          'Will create a self-signed certificate if left void.');
      until (auth='') or ECCKeyFileFind(auth,true) or sw.NoPrompt;
      if auth<>'' then begin
        sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
        authpass := sw.AsUTF8('AuthPass','',
          'Enter the PassPhrase of this .private file.');
        authrounds := sw.AsInt('AuthRounds',DEFAULT_ECCROUNDS,
          'Enter the PassPhrase iteration rounds of this .private file.');
      end else
        authrounds := 0;
      issuer := sw.AsUTF8('Issuer',ExeVersion.User,
        'Enter Issuer identifier text.'#13#10'Will be truncated to 15-20 ascii-7 chars.');
      start := sw.AsDate('Start',NowUTC,
        'Enter the YYYY-MM-DD start date of its validity.'#13#10+
        '0 will create a never-expiring certificate.');
      if start<=0 then
        days := 0 else
        days := sw.AsInt('Days',365,'Enter the number of days of its validity.');
      repeat
        savepass := sw.AsUTF8('NewPass',TAESPRNG.Main.RandomPassword(12),
          'Enter a private PassPhrase for the new key (at least 8 chars long).'#13#10+
          'Save this in a safe place: if you forget it, the key will be useless!');
      until (length(savepass)>=8) or sw.NoPrompt;
      repeat
        saverounds := sw.AsInt('NewRounds',DEFAULT_ECCROUNDS,
          'Enter the PassPhrase iteration rounds for the new key (at least 1000).'#13#10+
          'The higher, the safer, but will demand more computation time.');
      until (saverounds>=1000) or sw.NoPrompt;
      splitfiles := 1;
      {repeat
        splitfiles := sw.AsInt('SplitFiles',1,
          'Into how many files the private key should be parceled out.');
      until (splitfiles>0) or sw.NoPrompt;}
      newfile := EccCommandNew(
        auth,authpass,authrounds,issuer,start,days,savepass,saverounds,splitfiles);
      WritePassword(newfile,savepass,saverounds);
      if newfile<>'' then begin
        newfile := newfile+'/.private';
        if FileExists(ChangeFileExt(newfile, CHEAT_FILEEXT)) then
          newfile := newfile+('/'+CHEAT_FILEEXT);
      end;
    end;
    ecRekey: begin
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .private certificate file name.');
      until ECCKeyFileFind(auth,true) or sw.NoPrompt;
      sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
      authpass := sw.AsUTF8('AuthPass','',
        'Enter the PassPhrase of this .private file.');
      authrounds := sw.AsInt('AuthRounds',DEFAULT_ECCROUNDS,
        'Enter the PassPhrase iteration rounds of this .private file.');
      repeat
        savepass := sw.AsUTF8('NewPass',TAESPRNG.Main.RandomPassword(12),
          'Enter a NEW private PassPhrase for the key (at least 8 chars long).'#13#10+
          'Save this in a safe place: if you forget it, the key will be useless!');
      until (length(savepass)>=8) or sw.NoPrompt;
      repeat
        saverounds := sw.AsInt('NewRounds',DEFAULT_ECCROUNDS,
          'Enter the NEW PassPhrase iteration rounds for the key (at least 1000).'#13#10+
          'The higher, the safer, but will demand more computation time.');
      until (saverounds>=1000) or sw.NoPrompt;
      newfile := EccCommandRekey(auth,authpass,authrounds,savepass,saverounds);
      WritePassword(newfile,savepass,saverounds);
      if FileExists(ChangeFileExt(newfile,CHEAT_FILEEXT)) then
        newfile := newfile+('/'+CHEAT_FILEEXT);
    end;
    ecSign: begin
      repeat
        origfile := sw.AsString('File','','Enter the name of the file to be signed.');
      until FileExists(origfile) or sw.NoPrompt;
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .private file name of the signing authority.');
      until ECCKeyFileFind(auth,true) or sw.NoPrompt;
      sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
      authpass := sw.AsUTF8('Pass','',
        'Enter the PassPhrase of this .private file.');
      authrounds := sw.AsInt('Rounds',DEFAULT_ECCROUNDS,
        'Enter the PassPhrase iteration rounds of this .private file.');
      newfile := EccCommandSignFile(origfile,auth,authpass,authrounds,[]);
    end;
    ecVerify: begin
      repeat
        origfile := sw.AsString('File','','Enter the name of the file to be verified.');
      until sw.NoPrompt or
        (FileExists(origfile) and FileExists(origfile+ECCCERTIFICATESIGN_FILEEXT));
      WriteVerif(ECCCommandVerifyFile(origfile,sw.AsString('auth','',''),''),origfile,sw);
    end;
    ecSource: begin
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .private certificate file name.');
      until ECCKeyFileFind(auth,true) or sw.NoPrompt;
      sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
      authpass := sw.AsUTF8('Pass','',
        'Enter the PassPhrase of this .private file.');
      authrounds := sw.AsInt('Rounds',DEFAULT_ECCROUNDS,
        'Enter the PassPhrase iteration rounds of this .private file.');
      constname := sw.AsUTF8('Const','',
        'Enter the variable name to define the const in source.');
      comment := sw.AsUTF8('Comment','',
        'Enter some optional comment to identify this private key.');
      newfile := EccCommandSourceFile(auth,authpass,authrounds,constname,comment,
        TAESPRNG.Main.RandomPassword(24));
    end;
    ecInfoPriv: begin
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .private certificate file name.');
      until ECCKeyFileFind(auth,true) or sw.NoPrompt;
      if not sw.NoPrompt then
        sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
      authpass := sw.AsUTF8('Pass','',
        'Enter the PassPhrase of this .private file.');
      authrounds := sw.AsInt('Rounds',DEFAULT_ECCROUNDS,
        'Enter the PassPhrase iteration rounds of this .private file.');
      json := ECCCommandInfoPrivFile(auth,authpass,authrounds);
      sw.Text('%',[json]);
      jsonfile := sw.AsString('Json','','');
      if jsonfile <> '' then
        FileFromString(json,jsonfile);
      if not sw.NoPrompt then
        WritePassword(auth,authpass,authrounds);
    end;
    ecChain:
      newfile := ECCCommandChainCertificates(sw.AsArray);
    ecChainAll:
      newfile := ECCCommandChainCertificates(['*']);
    ecCrypt: begin
      repeat
        origfile := sw.AsString('File','','Enter the name of the file to be encrypted.');
      until FileExists(origfile) or sw.NoPrompt;
      repeat
        newfile := SysUtils.Trim(sw.AsString('Out',origfile+ENCRYPTED_FILEEXT,
          'Enter the name of the encrypted file'));
      until (newfile <> '') or sw.NoPrompt;
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .public file name of the encryption authority.');
      until ECCKeyFileFind(auth,false) or sw.NoPrompt;
      sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
      authpass := sw.AsUTF8('SaltPass','salt','Enter the optional PassPhrase to be used for encryption.');
      authrounds := sw.AsInt('SaltRounds',DEFAULT_ECCROUNDS, 'Enter the PassPhrase iteration rounds.');
      algo := TECIESAlgo(sw.AsEnum('Algo', '0', TypeInfo(TECIESAlgo), ''));
      ECCCommandCryptFile(origfile,newfile,auth,'','',authpass,authrounds,algo);
    end;
    ecInfoCrypt: begin
      repeat
        origfile := sw.AsString('File','','Enter the name of the encrypted file.');
      until FileExists(origfile) or sw.NoPrompt;
      newfile := sw.AsString('RawFile','','');
      json := JSONReformat(ECIESHeaderText(origfile,newfile));
      sw.Text('%',[json]);
      jsonfile := sw.AsString('Json','','');
      if jsonfile <> '' then
        FileFromString(json,jsonfile);
    end;
    ecDecrypt: begin
      repeat
        origfile := sw.AsString('File','','Enter the name of the file to be decrypted.');
      until FileExists(origfile) or sw.NoPrompt;
      repeat
        newfile := SysUtils.Trim(sw.AsString('Out',GetFileNameWithoutExt(origfile)+'.2',
          'Enter the name of the decrypted file'));
      until (newfile <> '') or sw.NoPrompt;
      authpass := sw.AsUTF8('AuthPass','',
        'Enter the PassPhrase of the associated .private file.');
      authrounds := sw.AsInt('AuthRounds',DEFAULT_ECCROUNDS,
        'Enter the PassPhrase iteration rounds of this .private file.');
      savepass := sw.AsUTF8('SaltPass','salt','Enter the optional PassPhrase to be used for decryption.');
      saverounds := sw.AsInt('SaltRounds',DEFAULT_ECCROUNDS, 'Enter the PassPhrase iteration rounds.');
      decrypt := ECCCommandDecryptFile(origfile,newfile,
        sw.AsString('Auth','',''),authpass,authrounds,savepass,saverounds,@decryptsign,@meta);
      msg := SysUtils.LowerCase(GetCaptionFromEnum(TypeInfo(TECCDecrypt),ord(decrypt)));
      if decrypt in ECC_VALIDDECRYPT then begin
        if decrypt=ecdDecryptedWithSignature then
          WriteVerif(ECCCommandVerifyDecryptedFile(newfile,decryptsign),newfile,sw);
        if meta<>'' then
          sw.Text(' % file meta = %',[origfile,meta],ccGreen);
        sw.Text(' % file %.',[origfile,msg],ccLightGreen);
      end else begin
        sw.Text(' % file decryption failure: % (%).',[origfile,msg,ord(decrypt)],ccLightRed);
        result := eccError;
      end;
    end;
    ecAeadCrypt: begin
      repeat
        origfile := sw.AsString('File','','Enter the name of the file to be encrypted.');
      until FileExists(origfile) or sw.NoPrompt;
      repeat
        newfile := SysUtils.Trim(sw.AsString('Out',origfile+AEAD_FILEEXT,
          'Enter the name of the encrypted file'));
      until (newfile <> '') or sw.NoPrompt;
      authpass := sw.AsUTF8('Pass','', 'Enter the PassPhrase to be used for encryption.');
      savepass := sw.AsUTF8('Salt','salt','Enter the optional PassPhrase to be used for encryption.');
      authrounds := sw.AsInt('Rounds',DEFAULT_AEADROUNDS, 'Enter the PassPhrase iteration rounds.');
      AEADCommandCryptFile(origfile,newfile,authpass,savepass, authrounds);
    end;
    ecAeadDecrypt: begin
      repeat
        origfile := sw.AsString('File','','Enter the name of the file to be decrypted.');
      until FileExists(origfile) or sw.NoPrompt;
      repeat
        newfile := SysUtils.Trim(sw.AsString('Out',GetFileNameWithoutExt(origfile)+'.2',
          'Enter the name of the decrypted file'));
      until (newfile <> '') or sw.NoPrompt;
      authpass := sw.AsUTF8('Pass','','Enter the PassPhrase to be used for decryption.');
      savepass := sw.AsUTF8('Salt','salt','Enter the optional PassPhrase to be used for decryption.');
      authrounds := sw.AsInt('Rounds',DEFAULT_AEADROUNDS,'Enter the PassPhrase iteration rounds.');
      AEADCommandDecryptFile(origfile,newfile,authpass,savepass,authrounds);
      sw.Text(' % file decrypted.',[origfile],ccLightGreen);
    end;
    ecCheatInit: begin
      issuer := sw.AsUTF8('Issuer',ExeVersion.User,
        'Enter Issuer identifier text of the master cheat keys.'#13#10+
        'Will be truncated to 15-20 ascii-7 chars.');
      repeat
        savepass := sw.AsUTF8('NewPass',TAESPRNG.Main.RandomPassword(12),
          'Enter a private PassPhrase for the master cheat.private key (at least 8 chars).'#13#10+
          'Save this in a safe place: if you forget it, the key will be useless!');
      until (length(savepass)>=8) or sw.NoPrompt;
      repeat
        saverounds := sw.AsInt('NewRounds',CHEAT_ROUNDS,
          'Enter iteration rounds for the mastercheat.private key (at least 100000).');
      until (saverounds>=CHEAT_ROUNDS) or sw.NoPrompt;
      newfile := ECCCommandCheatInit(issuer,savepass,saverounds);
      if newfile<>'' then
        newfile := newfile+'/.private';
    end;
    ecCheat: begin
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .private certificate file name.');
      until ECCKeyFileFind(auth,true) or sw.NoPrompt;
      if not sw.NoPrompt then
        sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
      savepass := sw.AsUTF8('AuthPass','',
        'Enter the PassPhrase of the master cheat.private file.');
      saverounds := sw.AsInt('AuthRounds',CHEAT_ROUNDS,
        'Enter the PassPhrase iteration rounds of the cheat.private file.');
      sw.Text('%',[ECCCommandCheat(auth,savepass,saverounds,authpass,authrounds)]);
      if not sw.NoPrompt then
        WritePassword(auth,authpass,authrounds);
    end;
    else
      result := eccUnknownCommand;
    end;
  except
    on E: Exception do begin
      if not sw.NoPrompt then
        ConsoleShowFatalException(E,false);
      newfile := '';
      result := eccException;
    end;
  end;
  finally
    if not sw.NoPrompt then begin
      FillcharFast(pointer(authpass)^,length(authpass),0);
      FillcharFast(pointer(savepass)^,length(savepass),0);
    end;
  end;
  if (newfile<>'') and (result=eccSuccess) and not(cmd in [ecInfoCrypt]) then
    sw.Text(' % file created.',[newfile],ccWhite);
  sw.TextColor(ccLightGray);
end;

end.
