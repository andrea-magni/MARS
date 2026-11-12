(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.JOSEJWT.TaurusTLS;

// Asymmetric JWT algorithms (JWT.Algorithm = RS256, PS256 or ES256) with the JOSE backend:
// add this unit to the uses of the ignition, after MARS.JOSEJWT.Token.
// It replaces the default crypto providers of delphi-jose-jwt (Indy and OpenSSL 1.0) with the
// TaurusTLS ones (OpenSSL 3). It needs:
// - the TaurusTLS library (https://github.com/JPeterMugaas/TaurusTLS), not shipped with MARS:
//   its Source folder in the library path or in the search path of the project;
// - OpenSSL 3 at run time: libcrypto-3.dll and libssl-3.dll (Win32), libcrypto-3-x64.dll and
//   libssl-3-x64.dll (Win64), both of them, where the executable finds them.
// HS256 does not need this unit. The mORMot backend needs neither this unit nor OpenSSL.

interface

implementation

uses
  JOSE.Providers, JOSE.Providers.TaurusTLS
, MARS.JOSEJWT.Token
;

initialization
  TJOSEProviders.UnregisterProvider; // the default ones: never OpenSSL 1.0
  TJOSETaurusTLSProviders.Register;
  TMARSJOSEJWTToken.AsymmetricProviderRegistered := True;

end.
