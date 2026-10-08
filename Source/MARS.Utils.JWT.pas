unit MARS.Utils.JWT;

interface

    const JWT_USERNAME = 'UserName';
    const JWT_ROLES = 'Roles';

    const JWT_ISSUER_PARAM = 'JWT.Issuer';
    const JWT_ISSUER_PARAM_DEFAULT = 'MARS-Curiosity';
    const JWT_SECRET_PARAM = 'JWT.Secret';
    // public, therefore never used unless JWT.AllowDefaultSecret=true (see TMARSToken.SecretFromParameters)
    const JWT_SECRET_PARAM_DEFAULT = '{788A2FD0-8E93-4C11-B5AF-51867CF26EE7}';
    const JWT_ALLOWDEFAULTSECRET_PARAM = 'JWT.AllowDefaultSecret';
    // key rotation: JWT.KeyId names JWT.Secret (written as "kid" in the header of new tokens),
    // JWT.PreviousSecret.<kid> keeps a retired key valid for verification only,
    // JWT.PreviousSecret (no kid) does the same for tokens issued without a "kid"
    const JWT_KEYID_PARAM = 'JWT.KeyId';
    const JWT_PREVIOUSSECRET_PARAM = 'JWT.PreviousSecret';
    const JWT_KEYID_HEADER = 'kid';
    const JWT_COOKIEENABLED_PARAM = 'JWT.CookieEnabled';
    const JWT_COOKIEENABLED_PARAM_DEFAULT = true;
    const JWT_COOKIENAME_PARAM = 'JWT.CookieName';
    const JWT_COOKIENAME_PARAM_DEFAULT = 'access_token';
    const JWT_COOKIEDOMAIN_PARAM = 'JWT.CookieDomain';
    const JWT_COOKIEPATH_PARAM = 'JWT.CookiePath';
    const JWT_DURATION_PARAM = 'JWT.Duration';
    const JWT_DURATION_IN_SECONDS_PARAM = 'JWT.Duration.InSeconds';
    const JWT_DURATION_IN_MINUTES_PARAM = 'JWT.Duration.InMinutes';
    const JWT_DURATION_PARAM_DEFAULT = 1; // 1 day
    const JWT_COOKIESECURE_PARAM = 'JWT.CookieSecure';
    const JWT_COOKIESECURE_PARAM_DEFAULT = false;
    const JWT_COOKIESAMESITE_PARAM = 'JWT.CookieSameSite';
    const JWT_COOKIESAMESITE_PARAM_DEFAULT = 'Lax';

    const JWT_AUDIENCE_CLAIM   = 'aud';
    const JWT_EXPIRATION_CLAIM = 'exp';
    const JWT_ISSUED_AT_CLAIM  = 'iat';
    const JWT_ISSUER_CLAIM     = 'iss';
    const JWT_JWT_ID_CLAIM     = 'jti';
    const JWT_NOT_BEFORE_CLAIM = 'nbf';
    const JWT_SUBJECT_CLAIM    = 'sub';

    const JWT_DURATION_CLAIM = 'duration';


implementation

end.
