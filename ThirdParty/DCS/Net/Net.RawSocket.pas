unit Net.RawSocket;

interface

uses
  System.SysUtils, Net.SocketAPI,
  {$IFDEF POSIX}
  Posix.Base, Posix.SysSocket, Posix.NetinetIn
  {$ELSE}
  Winapi.Windows, Net.Winsock2, Net.Wship6
  {$ENDIF};

type
  /// <summary>
  ///   �򵥵��׽��ֲ�����
  /// </summary>
  TRawSocket = class
  private
    FSocket: THandle;
    FSockAddr: TRawSockAddrIn;
    FPeerAddr: string;
    FPeerPort: Word;
  public
    /// <summary>
    ///   �ر� Socket
    /// </summary>
    procedure Close;

    /// <summary>
    ///   ���ӵ�����, ֧�� IPv6
    /// </summary>
    function Connect(const AHost: string; APort: Word): Integer;

    /// <summary>
    ///   �� Socket ��ָ����ַ�Ͷ˿�, ֧�� IPv6
    /// </summary>
    function Bind(const Addr: string; APort: Word): Integer;

    /// <summary>
    ///   ��������
    /// </summary>
    function Listen(backlog: Integer = SOMAXCONN): Integer;

    /// <summary>
    ///   ����һ����������, ������ Socket
    /// </summary>
    function Accept(Addr: PSockAddr; AddrLen: PInteger): THandle;

    /// <summary>
    ///   ��������
    /// </summary>
    function Recv(var Buf; len: Integer; flags: Integer = 0): Integer;

    /// <summary>
    ///   ��������
    /// </summary>
    function Send(const Buf; len: Integer; flags: Integer = 0): Integer;

    /// <summary>
    ///    �������ݴ�ָ����ַ�˿�(����UDP)
    /// </summary>
    function RecvFrom(const Addr: PSockAddr; var AddrLen: Integer; var Buf;
      len: Integer; flags: Integer = 0): Integer;

    /// <summary>
    ///    �������ݵ�ָ����ַ�˿�(����UDP)
    /// </summary>
    function SendTo(const Addr: PSockAddr; AddrLen: Integer; const Buf;
      len: Integer; flags: Integer = 0): Integer;

    /// <summary>
    ///    �ж��׽����Ƿ���Ч
    /// </summary>
    function IsValid: Boolean;

    /// <summary>
    ///    �׽��־��
    /// </summary>
    property Socket: THandle read FSocket;

    /// <summary>
    ///    �׽��ֵ�ַ��Ϣ
    ///    ����� Connect ��Զ�˵��׽���, ��õ�ַ�������Զ�˵�ַ��Ϣ
    ///    ����� Bind �����ص��׽���, ��õ�ַ������Ǳ��ص�ַ��Ϣ
    /// </summary>
    property SockAddr: TRawSockAddrIn read FSockAddr;
    property PeerAddr: string read FPeerAddr;
    property PeerPort: Word read FPeerPort;
  end;

implementation

{ TRawSocket }

procedure TRawSocket.Close;
begin
  if (FSocket = INVALID_HANDLE_VALUE) then Exit;

  TSocketAPI.CloseSocket(FSocket);
  FSocket := INVALID_HANDLE_VALUE;
end;

function TRawSocket.Connect(const AHost: string; APort: Word): Integer;
var
  LHints: TRawAddrInfo;
  LAddrInfo: PRawAddrInfo;
begin
  FillChar(LHints, SizeOf(TRawAddrInfo), 0);
  LHints.ai_family := AF_UNSPEC;
  LHints.ai_socktype := SOCK_STREAM;
  LHints.ai_protocol := IPPROTO_TCP;
  LAddrInfo := TSocketAPI.GetAddrInfo(AHost, APort.ToString, LHints);
  if (LAddrInfo = nil) then Exit(-1);

  try
    FSocket := TSocketAPI.NewSocket(LAddrInfo.ai_family, LAddrInfo.ai_socktype,
      LAddrInfo.ai_protocol);
    if (FSocket = INVALID_HANDLE_VALUE) then Exit(-1);

    FSockAddr.AddrLen := LAddrInfo.ai_addrlen;
    Move(LAddrInfo.ai_addr^, FSockAddr.Addr, LAddrInfo.ai_addrlen);
    TSocketAPI.ExtractAddrInfo(@FSockAddr.Addr, FSockAddr.AddrLen, FPeerAddr, FPeerPort);

    TSocketAPI.SetKeepAlive(FSocket, 5, 3, 5);
    Result := TSocketAPI.Connect(FSocket, @FSockAddr.Addr, FSockAddr.AddrLen);
  finally
    TSocketAPI.FreeAddrInfo(LAddrInfo);
  end;
end;

function TRawSocket.Bind(const Addr: string; APort: Word): Integer;
var
  LHints: TRawAddrInfo;
  LAddrInfo: PRawAddrInfo;
begin
  FillChar(LHints, SizeOf(TRawAddrInfo), 0);
  LHints.ai_family := AF_UNSPEC;
  LHints.ai_socktype := SOCK_STREAM;
  LHints.ai_protocol := IPPROTO_TCP;
  LAddrInfo := TSocketAPI.GetAddrInfo(Addr, APort.ToString, LHints);
  if (LAddrInfo = nil) then Exit(-1);

  FSockAddr.AddrLen := LAddrInfo.ai_addrlen;
  Move(LAddrInfo.ai_addr^, FSockAddr.Addr, LAddrInfo.ai_addrlen);
  TSocketAPI.FreeAddrInfo(LAddrInfo);

  TSocketAPI.ExtractAddrInfo(@FSockAddr.Addr, FSockAddr.AddrLen, FPeerAddr, FPeerPort);

  Result := TSocketAPI.Bind(FSocket, @FSockAddr.Addr, FSockAddr.AddrLen);
end;

function TRawSocket.Listen(backlog: Integer): Integer;
begin
  Result := TSocketAPI.Listen(FSocket, backlog);
end;

function TRawSocket.Accept(Addr: PSockAddr; AddrLen: PInteger): THandle;
begin
  Result := TSocketAPI.Accept(FSocket, Addr, AddrLen);
end;

function TRawSocket.Recv(var Buf; len, flags: Integer): Integer;
begin
  Result := TSocketAPI.Recv(FSocket, Buf, len, flags);
end;

function TRawSocket.Send(const Buf; len, flags: Integer): Integer;
begin
  Result := TSocketAPI.Send(FSocket, Buf, len, flags);
end;

function TRawSocket.RecvFrom(const Addr: PSockAddr; var AddrLen: Integer;
  var Buf; len, flags: Integer): Integer;
begin
  Result := TSocketAPI.RecvFrom(FSocket, Addr, AddrLen, Buf, len, flags);
end;

function TRawSocket.SendTo(const Addr: PSockAddr; AddrLen: Integer; const Buf;
  len: Integer; flags: Integer): Integer;
begin
  Result := TSocketAPI.SendTo(FSocket, Addr, AddrLen, Buf, len, flags);
end;

function TRawSocket.IsValid: Boolean;
begin
  Result := TSocketAPI.IsValidSocket(FSocket);
end;

end.
