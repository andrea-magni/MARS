{******************************************************************************}
{                                                                              }
{  Delphi JOSE Library                                                         }
{  Copyright (c) 2015-2017 Paolo Rossi                                         }
{  https://github.com/paolo-rossi/delphi-jose-jwt                              }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

/// <summary>
///   JSON Web Algorithms (JWA) RFC implementation (partial) <br />
/// </summary>
/// <seealso href="https://tools.ietf.org/html/rfc7518">
///   JWA RFC Document
/// </seealso>
unit JOSE.Core.JWA.Signing;

interface

uses
  System.SysUtils,
  JOSE.Types.Bytes,
  JOSE.Hashing.HMAC,
  JOSE.Signing.RSA,
  JOSE.Core.Base,
  JOSE.Core.Parts,
  JOSE.Core.JWA,
  JOSE.Core.JWK;

type
  IJOSESigningAlgorithm = interface(IJOSEAlgorithm)
  ['{F999E708-40F5-40E3-81F9-C4D20EB2FA79}']
    function VerifySignature(const AKey, AInput, ASignature: TJOSEBytes): Boolean;
    function Sign(const AKey, AInput: TJOSEBytes): TJOSEBytes;
    procedure ValidateSigningKey(const AKey: TJOSEBytes);
    procedure ValidateVerificationKey(const AKey: TJOSEBytes);
  end;

  TBaseSignatureAlgorithm = class(TJOSEAlgorithm, IJOSESigningAlgorithm)
  public
    function VerifySignature(const AKey, AInput, ASignature: TJOSEBytes): Boolean;
    function Sign(const AKey, AInput: TJOSEBytes): TJOSEBytes;
    procedure ValidateSigningKey(const AKey: TJOSEBytes);
    procedure ValidateVerificationKey(const AKey: TJOSEBytes);
  end;

  TUnsecureNoneAlgorithm = class(TJOSEAlgorithm, IJOSESigningAlgorithm)
  private
    const CANNOT_HAVE_KEY = 'Unsecured JWS (%s=%s) must not use a key';
    procedure ValidateKey(const AKey: TJOSEBytes);
  public
    constructor Create;
    function VerifySignature(const AKey, AInput, ASignature: TJOSEBytes): Boolean;
    function Sign(const AKey, AInput: TJOSEBytes): TJOSEBytes;
    procedure ValidateSigningKey(const AKey: TJOSEBytes);
    procedure ValidateVerificationKey(const AKey: TJOSEBytes);
  end;

  THmacUsingShaAlgorithm = class(TJOSEAlgorithm, IJOSESigningAlgorithm)
  private
    FKeyMinLength: Integer;
  protected
    FHMACAlgorithm: THMACAlgorithm;
    constructor Create(const AAlgorithmId: TJOSEAlgorithmId; AKeyMinLength: Integer);
    procedure ValidateKey(const AKey: TJOSEBytes);
  public
    class function HmacSha256: IJOSESigningAlgorithm;
    class function HmacSha384: IJOSESigningAlgorithm;
    class function HmacSha512: IJOSESigningAlgorithm;

    function VerifySignature(const AKey, AInput, ASignature: TJOSEBytes): Boolean;
    function Sign(const AKey, AInput: TJOSEBytes): TJOSEBytes;
    procedure ValidateSigningKey(const AKey: TJOSEBytes);
    procedure ValidateVerificationKey(const AKey: TJOSEBytes);
  end;

  TRSAUsingSHAAlgorithm = class(TJOSEAlgorithm, IJOSESigningAlgorithm)
  private
    FKeyMinLength: Integer;
  protected
    FRSAAlgorithm: TRSAAlgorithm;
    constructor Create(const AAlgorithmId: TJOSEAlgorithmId; AKeyMinLength: Integer);
  public
    class function RSA256: IJOSESigningAlgorithm;
    class function RSA384: IJOSESigningAlgorithm;
    class function RSA512: IJOSESigningAlgorithm;

    function VerifySignature(const AKey, AInput, ASignature: TJOSEBytes): Boolean;
    function Sign(const AKey, AInput: TJOSEBytes): TJOSEBytes;
    procedure ValidateSigningKey(const AKey: TJOSEBytes);
    procedure ValidateVerificationKey(const AKey: TJOSEBytes);
  end;

implementation

uses
  System.Types,
  System.StrUtils,
  JOSE.Encoding.Base64;

constructor THmacUsingShaAlgorithm.Create(const AAlgorithmId: TJOSEAlgorithmId; AKeyMinLength: Integer);
begin
  FAlgorithmIdentifier := AAlgorithmId;

  case AAlgorithmId of
    TJOSEAlgorithmId.HS256: FHMACAlgorithm := THMACAlgorithm.SHA256;
    TJOSEAlgorithmId.HS384: FHMACAlgorithm := THMACAlgorithm.SHA384;
    TJOSEAlgorithmId.HS512: FHMACAlgorithm := THMACAlgorithm.SHA512;
  end;
  FKeyCategory := TJOSEKeyCategory.Symmetric;
  FKeyType := 'oct';
  FKeyMinLength := AKeyMinLength;
end;

class function THmacUsingShaAlgorithm.HmacSha256: IJOSESigningAlgorithm;
begin
  Result := THmacUsingShaAlgorithm.Create(TJOSEAlgorithmId.HS256, 256);
end;

class function THmacUsingShaAlgorithm.HmacSha384: IJOSESigningAlgorithm;
begin
  Result := THmacUsingShaAlgorithm.Create(TJOSEAlgorithmId.HS384, 384);
end;

class function THmacUsingShaAlgorithm.HmacSha512: IJOSESigningAlgorithm;
begin
  Result := THmacUsingShaAlgorithm.Create(TJOSEAlgorithmId.HS512, 512);
end;

function THmacUsingShaAlgorithm.Sign(const AKey, AInput: TJOSEBytes): TJOSEBytes;
var
  LSign: TJOSEBytes;
begin
  LSign := THMAC.Sign(AInput, AKey, FHMACAlgorithm);
  Result := TBase64.URLEncode(LSign.AsBytes);
end;

procedure THmacUsingShaAlgorithm.ValidateKey(const AKey: TJOSEBytes);
begin
  if AKey.IsEmpty then
    raise EJOSEException.Create('Key is null');

  if AKey.Size * 8 < FKeyMinLength then
    raise EJOSEException.CreateFmt('Key is too short (%dbit), expected (%dbit)',
      [AKey.Size * 8, FKeyMinLength]);
end;

procedure THmacUsingShaAlgorithm.ValidateSigningKey(const AKey: TJOSEBytes);
begin
  ValidateKey(AKey);
end;

procedure THmacUsingShaAlgorithm.ValidateVerificationKey(const AKey: TJOSEBytes);
begin
  ValidateKey(AKey);
end;

function THmacUsingShaAlgorithm.VerifySignature(const AKey, AInput, ASignature: TJOSEBytes): Boolean;
var
  LComputedSignature: TJOSEBytes;
begin
  LComputedSignature := THMAC.Sign(AInput, AKey, FHMACAlgorithm);
  LComputedSignature := TBase64.URLEncode(LComputedSignature.AsBytes);

  Result := LComputedSignature = ASignature;
end;

{ TBaseSignatureAlgorithm }

function TBaseSignatureAlgorithm.Sign(const AKey, AInput: TJOSEBytes): TJOSEBytes;
begin
  Result := '';
end;

procedure TBaseSignatureAlgorithm.ValidateSigningKey(const AKey: TJOSEBytes);
begin
  raise EJOSEException.Create('Not implemented');
end;

procedure TBaseSignatureAlgorithm.ValidateVerificationKey(const AKey: TJOSEBytes);
begin
  raise EJOSEException.Create('Not implemented');
end;

function TBaseSignatureAlgorithm.VerifySignature(const AKey, AInput, ASignature: TJOSEBytes): Boolean;
begin
  Result := False;
end;

{ TUnsecureNoneAlgorithm }

constructor TUnsecureNoneAlgorithm.Create;
begin
  FAlgorithmIdentifier := TJOSEAlgorithmId.None;
  FKeyCategory := TJOSEKeyCategory.None;
end;

function TUnsecureNoneAlgorithm.Sign(const AKey, AInput: TJOSEBytes): TJOSEBytes;
begin
  ValidateKey(AKey);
  Result := TJOSEBytes.Empty;
end;

procedure TUnsecureNoneAlgorithm.ValidateKey(const AKey: TJOSEBytes);
begin
  if not AKey.IsEmpty then
    raise EJOSEException.Create(Format(CANNOT_HAVE_KEY,
      [THeaderNames.ALGORITHM, TJOSEAlgorithmId.None.AsString]));
end;

procedure TUnsecureNoneAlgorithm.ValidateSigningKey(const AKey: TJOSEBytes);
begin
  ValidateKey(AKey);
end;

procedure TUnsecureNoneAlgorithm.ValidateVerificationKey(const AKey: TJOSEBytes);
begin
  ValidateKey(AKey);
end;

function TUnsecureNoneAlgorithm.VerifySignature(const AKey, AInput, ASignature: TJOSEBytes): Boolean;
begin
  ValidateKey(AKey);
  Result := ASignature.IsEmpty;
end;

{ TRSAAlgorithm }

constructor TRSAUsingSHAAlgorithm.Create(const AAlgorithmId: TJOSEAlgorithmId; AKeyMinLength: Integer);
begin
  FAlgorithmIdentifier := AAlgorithmId;

  case AAlgorithmId of
    TJOSEAlgorithmId.RS256: FRSAAlgorithm := TRSAAlgorithm.RS256;
    TJOSEAlgorithmId.RS384: FRSAAlgorithm := TRSAAlgorithm.RS384;
    TJOSEAlgorithmId.RS512: FRSAAlgorithm := TRSAAlgorithm.RS512;
  end;
  FKeyCategory := TJOSEKeyCategory.Asymmetric;
  FKeyType := 'pem';
  FKeyMinLength := AKeyMinLength;
end;

class function TRSAUsingSHAAlgorithm.RSA256: IJOSESigningAlgorithm;
begin
  Result := TRSAUsingSHAAlgorithm.Create(TJOSEAlgorithmId.RS256, 256);
end;

class function TRSAUsingSHAAlgorithm.RSA384: IJOSESigningAlgorithm;
begin
  Result := TRSAUsingSHAAlgorithm.Create(TJOSEAlgorithmId.RS384, 384);
end;

class function TRSAUsingSHAAlgorithm.RSA512: IJOSESigningAlgorithm;
begin
  Result := TRSAUsingSHAAlgorithm.Create(TJOSEAlgorithmId.RS512, 512);
end;

function TRSAUsingSHAAlgorithm.Sign(const AKey, AInput: TJOSEBytes): TJOSEBytes;
var
  LSign: TJOSEBytes;
begin
  LSign := TRSA.Sign(AInput, AKey, FRSAAlgorithm);
  Result := TBase64.URLEncode(LSign.AsBytes);
end;

procedure TRSAUsingSHAAlgorithm.ValidateSigningKey(const AKey: TJOSEBytes);
begin
  if AKey.IsEmpty then
    raise EJOSEException.Create('Key is null');

  if not TRSA.VerifyPrivateKey(AKey) then
    raise EJOSEException.Create('Key is not RSA key in PEM format');
end;

procedure TRSAUsingSHAAlgorithm.ValidateVerificationKey(const AKey: TJOSEBytes);
begin
  if AKey.IsEmpty then
    raise EJOSEException.Create('Key is null');

  if not TRSA.VerifyPublicKey(AKey) then
    raise EJOSEException.Create('Key is not RSA key in PEM format');
end;

function TRSAUsingSHAAlgorithm.VerifySignature(const AKey, AInput, ASignature: TJOSEBytes): Boolean;
var
  LDecodedSignature: TJOSEBytes;
begin
  ValidateVerificationKey(AKey);
  LDecodedSignature := TBase64.URLDecode(ASignature);
  Result := TRSA.Verify(AInput, LDecodedSignature, AKey, FRSAAlgorithm);
end;

end.
