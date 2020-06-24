{******************************************************************************}
{                                                                              }
{       Delphi cross platform socket library                                   }
{                                                                              }
{       Copyright (c) 2017 WiNDDRiVER(soulawing@gmail.com)                     }
{                                                                              }
{       Homepage: https://github.com/winddriver/Delphi-Cross-Socket            }
{                                                                              }
{******************************************************************************}
unit Net.CrossHttpMiddleware;

interface

uses
  System.SysUtils, Net.CrossHttpServer;

type
  /// <summary>
  ///   HTTP��֤��ȡ�û�����
  /// </summary>
  TAuthGetPasswordProc = reference to procedure(ARequest: ICrossHttpRequest; const AUserName: string; var ACorrectPassword: string);

  /// <summary>
  ///   �м��
  /// </summary>
  /// <remarks>
  ///   ���� TCrossHttpServer.Use()
  /// </remarks>
  TNetCrossMiddleware = class
  public
    /// <summary>
    ///   HTTP������֤
    /// </summary>
    /// <param name="AAuthFunc">
    ///   �û������֤����
    /// </param>
    /// <remarks>
    ///   <see cref="https://zh.wikipedia.org/wiki/HTTP%E5%9F%BA%E6%9C%AC%E8%AE%A4%E8%AF%81">
    ///   ά���ٿ�: HTTP������֤</see>
    /// </remarks>
    class function AuthenticateBasic(AAuthGetPasswordProc: TAuthGetPasswordProc; const ARealm: string = ''): TCrossHttpRouterProc2; static;

    /// <summary>
    ///   HTTPժҪ��֤
    /// </summary>
    /// <param name="AAuthFunc">
    ///   �û������֤����
    /// </param>
    /// <remarks>
    ///   <see cref="https://zh.wikipedia.org/wiki/HTTP%E6%91%98%E8%A6%81%E8%AE%A4%E8%AF%81">
    ///   ά���ٿ�: HTTPժҪ��֤</see>
    /// </remarks>
    class function AuthenticateDigest(AAuthGetPasswordProc: TAuthGetPasswordProc; const ARealm: string = ''): TCrossHttpRouterProc2; static;

    /// <summary>
    ///   ����Դ��Դ����
    /// </summary>
    /// <remarks>
    ///   <see href="https://zh.wikipedia.org/wiki/%E8%B7%A8%E4%BE%86%E6%BA%90%E8%B3%87%E6%BA%90%E5%85%B1%E4%BA%AB">
    ///   ά���ٿ�: ����Դ��Դ����</see>
    /// </remarks>
    class function CORS: TCrossHttpRouterProc2; static;

    /// <summary>
    ///   HTTP�ϸ��䰲ȫ
    /// </summary>
    /// <remarks>
    ///   <see href="https://zh.wikipedia.org/wiki/HTTP%E4%B8%A5%E6%A0%BC%E4%BC%A0%E8%BE%93%E5%AE%89%E5%85%A8">
    ///   ά���ٿ�: HTTP�ϸ��䰲ȫ</see>
    /// </remarks>
    class function HSTS: TCrossHttpRouterProc2; static;
  end;

implementation

uses
  System.Hash, System.NetEncoding, Utils.Utils, Net.CrossHttpParams;

{ TNetCrossMiddleware }

class function TNetCrossMiddleware.AuthenticateBasic(AAuthGetPasswordProc: TAuthGetPasswordProc;
  const ARealm: string): TCrossHttpRouterProc2;
begin
  Result :=
    procedure(ARequest: ICrossHttpRequest; AResponse: ICrossHttpResponse; var AHandled: Boolean)
    var
      LAuthStr: string;
      LStrArr: TArray<string>;
      LCorrectPassword: string;
    begin
      // Authorization: Basic cm9vdDpyb290
      // base64���ֽ�����ʽΪ "�û���:����"
      LAuthStr := ARequest.Header['Authorization'];
      if (LAuthStr <> '') then
      begin
        if (LAuthStr.StartsWith('Basic')) then
          LAuthStr := LAuthStr.Substring(6)
        else
          LAuthStr := '';
      end;

      LCorrectPassword := #0;
      if (LAuthStr <> '') then
      begin
        LAuthStr := TNetEncoding.Base64.Decode(LAuthStr);
        LStrArr := LAuthStr.Split([':']);

        // ��ȡ�û�����Ӧ����ȷ����
        if Assigned(AAuthGetPasswordProc) and (Length(LStrArr) > 0) then
          AAuthGetPasswordProc(ARequest, LStrArr[0], LCorrectPassword);
      end;

      // ƥ������
      if (LAuthStr = '') or (Length(LStrArr) < 2) or (LStrArr[1] <> LCorrectPassword) then
      begin
        AHandled := True;
        AResponse.Header['WWW-authenticate'] := Format('Basic Realm="%s"', [ARealm]);
        AResponse.SendStatus(401);
        Exit;
      end;

      AHandled := False;
    end;
end;

class function TNetCrossMiddleware.AuthenticateDigest(
  AAuthGetPasswordProc: TAuthGetPasswordProc; const ARealm: string): TCrossHttpRouterProc2;
begin
  Result :=
    procedure(ARequest: ICrossHttpRequest; AResponse: ICrossHttpResponse; var AHandled: Boolean)
    var
      LUserName, LCorrectPassword: string;
      LNonce, LUserResponse, LCorrectResponse: string;
      LAuthStr: string;
      A1, A2, HA1, HA2: string;
      LAuthParams: TDelimitParams;
    begin
      // Authorization: Digest username="admin", realm="test realm", nonce="2468217498b46028705d401192459edd", uri="/login?key=value1", response="1d663058353e8f5831328728c29a6a1a", qop=auth, nc=00000006, cnonce="5d63a594e16feba2"
      LAuthStr := ARequest.Header['Authorization'];
      if (LAuthStr <> '') then
      begin
        if (LAuthStr.StartsWith('Digest')) then
          LAuthStr := LAuthStr.Substring(7)
        else
          LAuthStr := '';
      end;

      LCorrectPassword := #0;
      if (LAuthStr <> '') then
      begin
        LAuthParams := TDelimitParams.Create;
        try
          LAuthParams.Delimiter := ',';
          LAuthParams.Decode(LAuthStr);

          LUserName := LAuthParams['username'].Replace('"', '');
          // ��ȡ�û�����Ӧ����ȷ����
          if Assigned(AAuthGetPasswordProc) then
            AAuthGetPasswordProc(ARequest, LUserName, LCorrectPassword);

          {$region '����ժҪ'}
          A1 := Format('%s:%s:%s', [LUserName, ARealm, LCorrectPassword]);
          A2 := Format('%s:%s', [ARequest.Method, LAuthParams['uri'].Replace('"', '')]);

          HA1 := TUtils.BytesToHex(THashMD5.GetHashBytes(A1));
          HA2 := TUtils.BytesToHex(THashMD5.GetHashBytes(A2));

          LCorrectResponse := HA1 +
            ':' + LAuthParams['nonce'].Replace('"', '') +
            ':' + LAuthParams['nc'].Replace('"', '') +
            ':' + LAuthParams['cnonce'].Replace('"', '') +
            ':auth' +
            ':' + HA2;
          LCorrectResponse := TUtils.BytesToHex(THashMD5.GetHashBytes(LCorrectResponse));
          {$endregion}

          // �ͻ����Ѽ���õ�ժҪ
          LUserResponse := LAuthParams['response'].Replace('"', '');
        finally
          FreeAndNil(LAuthParams);
        end;
      end;

      // �ȶԿͻ��������˵�ժҪ�Ƿ�ƥ��
      if (LAuthStr = '') or (LUserResponse <> LCorrectResponse) then
      begin
        AHandled := True;
        LNonce := TUtils.BytesToHex(THashMD5.GetHashBytes(DateTimeToStr(Now)));
        AResponse.Header['WWW-authenticate'] := Format(
          'Digest realm="%s", qop=auth, nonce="%s"',
          [ARealm, LNonce]);
        AResponse.SendStatus(401);
        Exit;
      end;

      AHandled := False;
    end;
end;

class function TNetCrossMiddleware.CORS: TCrossHttpRouterProc2;
begin
  Result :=
    procedure(ARequest: ICrossHttpRequest; AResponse: ICrossHttpResponse; var AHandled: Boolean)
    begin
      AHandled := False;
      AResponse.Header['Access-Control-Allow-Origin'] := '*';
      AResponse.Header['Access-Control-Allow-Methods'] := '*';
      AResponse.Header['Access-Control-Allow-Headers'] := '*';
    end;
end;

class function TNetCrossMiddleware.HSTS: TCrossHttpRouterProc2;
begin
  Result :=
    procedure(ARequest: ICrossHttpRequest; AResponse: ICrossHttpResponse; var AHandled: Boolean)
    begin
      AHandled := False;
      AResponse.Header['Strict-Transport-Security'] := 'max-age=31536000; includeSubDomains';
    end;
end;

end.
