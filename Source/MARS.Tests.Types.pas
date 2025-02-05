unit MARS.Tests.Types;

interface

uses
  Classes, SysUtils, System.Generics.Collections, System.Rtti
, MARS.Core.Registry.Utils
;

type
  TRequestData = record
  private
    function GetMethodName: string;
    function GetMethodQualifiedName: string;
  public
    HostName: string;
    Port: Integer;
    Path: string;
    HttpMethod: string;
    QueryString: string;
    Body: string;
    Accept: string;
    Token: string;

    ExpectedResponse: string;

    ResourceName: string;
    Info: TMARSConstructorInfo;
    Method: TRttiMethod;

    const DEFAULT_HOSTNAME = 'localhost';
    const DEFAULT_PORT = 8080;
    const DEFAULT_HTTPMETHOD = 'GET';
    const DEFAULT_ACCEPT = 'application/json';

    constructor Create(
      const AHostName: string;
      const APort: Integer;
      const APath: string;
      const AHttpMethod: string = DEFAULT_HTTPMETHOD;
      const AQueryString: string = '';
      const ABody: string = '';
      const AAccept: string = DEFAULT_ACCEPT;
      const AToken: string = '');

    constructor CreateFromStringArray(const AStringArray: TArray<string>);
    constructor CreateFromString(const AString: string; const ASeparator: string = ',');

    procedure SetContext(const AResourceName: string; const AInfo: TMARSConstructorInfo;
      const AMethod: TRttiMethod);

    property MethodName: string read GetMethodName;
    property MethodQualifiedName: string read GetMethodQualifiedName;
  end;


implementation

{ TRequestData }

constructor TRequestData.Create(
  const AHostName: string; const APort: Integer;
  const APath, AHttpMethod, AQueryString, ABody, AAccept, AToken: string);
begin
  SetContext('', nil, nil);
  HostName := AHostName;
  Port := APort;
  Path := APath;
  HttpMethod := AHttpMethod;
  QueryString := AQueryString;
  Body := ABody;
  Accept := AAccept;
  Token := AToken;
end;

constructor TRequestData.CreateFromString(
  const AString: string; const ASeparator: string);
begin
  CreateFromStringArray(AString.Split([ASeparator]));
end;

constructor TRequestData.CreateFromStringArray(
  const AStringArray: TArray<string>);

    function GetValue(const AIndex: Integer; const ADefault: string = ''): string;
    begin
      Result := ADefault;
      var LCount := Length(AStringArray);
      if LCount > AIndex then
        Result := AStringArray[AIndex];
    end;

begin
  SetContext('', nil, nil);
  HostName    := GetValue(0, DEFAULT_HOSTNAME);
  Port        := GetValue(1, DEFAULT_PORT.ToString).ToInteger;
  Path        := GetValue(2);
  HttpMethod  := GetValue(3, DEFAULT_HTTPMETHOD);
  QueryString := GetValue(4);
  Body        := GetValue(5);
  Accept      := GetValue(6, DEFAULT_ACCEPT);
  Token       := GetValue(7);
end;

function TRequestData.GetMethodName: string;
begin
  Result := '';
  if Assigned(Method) then
    Result := Method.Name;
end;

function TRequestData.GetMethodQualifiedName: string;
begin
  Result := '';
  if Assigned(Method) and Assigned(Method.Parent) then
    Result := Method.Parent.Name + '.' + Method.Name;
end;

procedure TRequestData.SetContext(const AResourceName: string;
  const AInfo: TMARSConstructorInfo; const AMethod: TRttiMethod);
begin
  ResourceName := AResourceName;
  Info := AInfo;
  Method := AMethod;
end;

end.
