unit MARS.Core.RequestAndResponse.Interfaces;

interface

uses
  Classes, SysUtils;

type
  IMARSRequest = interface
    function GetRawContent: TBytes;
    function GetContent: string;
    function GetQueryParamIndex(const AName: string): Integer;
    function GetQueryParamName(const AIndex: integer): string;
    function GetQueryParamValue(const AIndex: Integer): string; overload;
    function GetQueryParamValue(const AName: string): string; overload;
    function GetQueryParamCount: Integer;
    function GetFormParamIndex(const AName: string): Integer;
    function GetFormParamName(const AIndex: Integer): string;
    function GetFormParamValue(const AIndex: Integer): string; overload;
    function GetFormParamValue(const AName: string): string; overload;
    function GetFormFileParamIndex(const AName: string): Integer;
    function GetFormFileParam(const AIndex: Integer; out AFieldName, AFileName: string;
      out ABytes: TBytes; out AContentType: string): Boolean;
    function GetFormParamCount: Integer;
    function GetHeaderParamCount: Integer;
    function GetHeaderParamIndex(const AName: string): Integer;
    function GetHeaderParamValue(const AHeaderName: string): string; overload;
    function GetHeaderParamValue(const AIndex: Integer): string; overload;
    function GetCookieParamIndex(const AName: string): Integer;
    function GetCookieParamValue(const AIndex: Integer): string; overload;
    function GetCookieParamValue(const AName: string): string; overload;
    function GetCookieParamCount: Integer;
    function GetFilesCount: Integer;
    function GetFormParams: string;
    function GetAccept: string;
    function GetAuthorization: string;
    function GetMethod: string;
    function GetQueryString: string;
    function GetHostName: string;
    function GetPort: Integer;
    function GetRawPath: string;
    function GetDate: TDateTime;

    function AsObject: TObject;
    procedure CheckWorkaroundForISAPI;

    property RawContent: TBytes read GetRawContent;
    property Content: string read GetContent;
    property Accept: string read GetAccept;
    property Authorization: string read GetAuthorization;
    property Method: string read GetMethod;
    property QueryString: string read GetQueryString;
    property HostName: string read GetHostName;
    property Port: Integer read GetPort;
    property RawPath: string read GetRawPath;
  end;

  IMARSResponse = interface
    function GetContentStream: TStream;
    procedure SetContentStream(const AContentStream: TStream);
    function GetContentType: string;
    procedure SetContentType(const AContentType: string);
    function GetContentEncoding: string;
    procedure SetContentEncoding(const AContentEncoding: string);
    function GetStatusCode: Integer;
    procedure SetStatusCode(const AStatusCode: Integer);
    function GetContent: string;
    procedure SetContent(const AContent: string);
    procedure SetHeader(const AName, AValue: string);
    procedure SetCookie(const AName, AValue, ADomain, APath: string; const AExpiration: TDateTime; const ASecure: Boolean);

    property Content: string read GetContent write SetContent;
    property ContentStream: TStream read GetContentStream write SetContentStream;
    property ContentType: string read GetContentType write SetContentType;
    property ContentEncoding: string read GetContentEncoding write SetContentEncoding;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
  end;


implementation

end.
