unit Net.CrossSslSocket.Base;

interface

uses
  Net.CrossSocket.Base;

type
  /// <summary>
  ///   SSL Socket
  /// </summary>
  /// <remarks>
  ///   ��ȷ��ʹ�ò���:
  ///   <list type="number">
  ///     <item>
  ///       SetCertificateificate �� SetCertificateificateFile
  ///     </item>
  ///     <item>
  ///       SetPrivateKey �� SetPrivateKeyFile, �ͻ��˲���Ҫ��һ��
  ///     </item>
  ///     <item>
  ///       Connect / Listen
  ///     </item>
  ///   </list>
  /// </remarks>
  ICrossSslSocket = interface(ICrossSocket)
  ['{A4765486-A0F1-4EFD-BC39-FA16AED21A6A}']
    /// <summary>
    ///   ���ڴ����֤��
    /// </summary>
    /// <param name="ACertBuf">
    ///   ֤�黺����
    /// </param>
    /// <param name="ACertBufSize">
    ///   ֤�黺������С
    /// </param>
    procedure SetCertificate(ACertBuf: Pointer; ACertBufSize: Integer); overload;

    /// <summary>
    ///   ���ַ�������֤��
    /// </summary>
    /// <param name="ACertStr">
    ///   ֤���ַ���
    /// </param>
    procedure SetCertificate(const ACertStr: string); overload;

    /// <summary>
    ///   ���ļ�����֤��
    /// </summary>
    /// <param name="ACertFile">
    ///   ֤���ļ�
    /// </param>
    procedure SetCertificateFile(const ACertFile: string);

    /// <summary>
    ///   ���ڴ����˽Կ
    /// </summary>
    /// <param name="APKeyBuf">
    ///   ˽Կ������
    /// </param>
    /// <param name="APKeyBufSize">
    ///   ˽Կ��������С
    /// </param>
    procedure SetPrivateKey(APKeyBuf: Pointer; APKeyBufSize: Integer); overload;

    /// <summary>
    ///   ���ַ�������˽Կ
    /// </summary>
    /// <param name="APKeyStr">
    ///   ˽Կ�ַ���
    /// </param>
    procedure SetPrivateKey(const APKeyStr: string); overload;

    /// <summary>
    ///   ���ļ�����˽Կ
    /// </summary>
    /// <param name="APKeyFile">
    ///   ˽Կ�ļ�
    /// </param>
    procedure SetPrivateKeyFile(const APKeyFile: string);
  end;

implementation

end.
