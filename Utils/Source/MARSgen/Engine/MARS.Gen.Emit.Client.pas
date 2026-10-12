(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.Emit.Client;

// Writes Client.<Api> (TGenClientMapper): generated, written again on every import. A class with a
// method per operation, on top of the MARS client (TMARSCustomClient: a TMARSNetClient of its own
// by default); bodies and results through the JSON of MARS (the types of Model.<Api>).

interface

uses
  System.SysUtils, System.Classes,
  MARS.Gen.Model, MARS.Gen.Types, MARS.Gen.Client, MARS.Gen.Output;

type
  TGenClientEmitter = class
  private
    FClient: TGenClientMapper;
    FLines: TStringList;
    procedure Add(const ALine: string = '');
    procedure AddComments(const AComments: TArray<string>; const AIndent: string);
    function Signature(const ACall: TGenClientCall; const AClassName: string; const AWithDefaults: Boolean): string;
    function PathExpression(const ACall: TGenClientCall): string;
    procedure AddBody(const ACall: TGenClientCall);
  public
    constructor Create(const AClient: TGenClientMapper);
    destructor Destroy; override;
    function ClientUnit: string;

    // Client.<Api> through AOutput (the types are written by the caller); returns its name
    class function WriteTo(const AApi: TGenApi; const ATypes: TGenTypeMapper; const AOutput: TGenOutput): string; static;
  end;

implementation

uses
  System.StrUtils,
  MARS.Gen.Naming;

const
  INDENT = '  ';
  WRAP_WIDTH = 100;

function Quoted(const AText: string): string;
begin
  Result := '''' + AText.Replace('''', '''''') + '''';
end;

{ TGenClientEmitter }

constructor TGenClientEmitter.Create(const AClient: TGenClientMapper);
begin
  inherited Create;
  FClient := AClient;
  FLines := TStringList.Create;
end;

destructor TGenClientEmitter.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TGenClientEmitter.Add(const ALine: string);
begin
  FLines.Add(ALine);
end;

procedure TGenClientEmitter.AddComments(const AComments: TArray<string>; const AIndent: string);
begin
  for var LLine in AComments do
    if LLine.Trim = '' then
      Add(AIndent + '//')
    else
      for var LWrapped in TGenNaming.WrapWords(LLine, WRAP_WIDTH) do
        Add(AIndent + '// ' + LWrapped);
end;

function TGenClientEmitter.Signature(const ACall: TGenClientCall; const AClassName: string;
  const AWithDefaults: Boolean): string;
var
  LParams: TArray<string>;
  LParam: string;
begin
  LParams := [];
  for var LItem in ACall.Params do
  begin
    LParam := 'const ' + LItem.Name + ': ' + LItem.TypeName;
    if AWithDefaults and not LItem.Required then
      LParam := LParam + ' = ' + LItem.DefaultValue;
    LParams := LParams + [LParam];
  end;

  if ACall.ResultType = '' then
    Result := 'procedure '
  else
    Result := 'function ';
  if AClassName <> '' then
    Result := Result + AClassName + '.';
  Result := Result + ACall.CallName;
  if Length(LParams) > 0 then
    Result := Result + '(' + string.Join('; ', LParams) + ')';
  if ACall.ResultType <> '' then
    Result := Result + ': ' + ACall.ResultType;
  Result := Result + ';';
end;

function TGenClientEmitter.PathExpression(const ACall: TGenClientCall): string;
var
  LPath, LName: string;
  LParts: TArray<string>;
  LStart, LEnd: Integer;
  LParam: TGenClientParam;
begin
  // '/pets/' + Encode(Text(APetId)) + '/photos'
  LParts := [];
  LPath := ACall.Operation.Path;
  while LPath <> '' do
  begin
    LStart := LPath.IndexOf('{');
    LEnd := LPath.IndexOf('}', LStart + 1);
    if (LStart < 0) or (LEnd < 0) then
    begin
      LParts := LParts + [Quoted(LPath)];
      Break;
    end;
    if LStart > 0 then
      LParts := LParts + [Quoted(LPath.Substring(0, LStart))];
    LName := LPath.Substring(LStart + 1, LEnd - LStart - 1);
    LParam := ACall.ParamOf(LName);
    if Assigned(LParam) and (LParam.Kind = TGenClientParamKind.Path) then
      LParts := LParts + ['Encode(Text(' + LParam.Name + '))']
    else
      // a template without its parameter: as written
      LParts := LParts + [Quoted('{' + LName + '}')];
    LPath := LPath.Substring(LEnd + 1);
  end;
  if Length(LParts) = 0 then
    LParts := [''''''];
  Result := string.Join(' + ', LParts);
end;

procedure TGenClientEmitter.AddBody(const ACall: TGenClientCall);
const
  LISTS: array[TGenClientParamKind] of string = ('', 'LQuery', 'LHeaders', 'LCookies', '');
var
  LCondition, LValue, LBody, LCall: string;
  LHasCookies: Boolean;
begin
  LHasCookies := False;
  for var LParam in ACall.Params do
    if LParam.Kind = TGenClientParamKind.Cookie then
      LHasCookies := True;
  for var LApiKey in ACall.ApiKeys do
    if LApiKey.Location = TGenParameterLocation.Cookie then
      LHasCookies := True;

  Add('var');
  Add(INDENT + 'LQuery, LHeaders' + IfThen(LHasCookies, ', LCookies') + ': TArray<string>;');
  Add('begin');
  Add(INDENT + 'LQuery := [];');
  Add(INDENT + 'LHeaders := [];');
  if LHasCookies then
    Add(INDENT + 'LCookies := [];');

  for var LParam in ACall.Params do
  begin
    if LParam.Kind in [TGenClientParamKind.Path, TGenClientParamKind.Body] then
      Continue;

    if LParam.IsArray then
    begin
      // query: name=a&name=b (explode) or name=a,b; header: a,b
      if LParam.Explode then
      begin
        Add(INDENT + 'for var LItem in ' + LParam.Name + ' do');
        Add(INDENT + INDENT + 'LQuery := LQuery + [' + Quoted(LParam.ExternalName + '=') + ' + Encode(Text(LItem))];');
        Continue;
      end;
      LCondition := 'Length(' + LParam.Name + ') > 0';
      if LParam.Kind = TGenClientParamKind.Query then
        LValue := 'Join(' + LParam.Name + ', True)'
      else
        LValue := 'Join(' + LParam.Name + ', False)';
    end
    else
    begin
      LCondition := LParam.Name + ' <> ' + LParam.DefaultValue;
      if LParam.Kind in [TGenClientParamKind.Query, TGenClientParamKind.Cookie] then
        LValue := 'Encode(Text(' + LParam.Name + '))'
      else
        LValue := 'Text(' + LParam.Name + ')';
    end;

    LCall := LISTS[LParam.Kind] + ' := ' + LISTS[LParam.Kind] + ' + [' + Quoted(LParam.ExternalName + '=') + ' + ' + LValue + '];';
    if LParam.Required and not LParam.IsArray then
      Add(INDENT + LCall)
    else
    begin
      Add(INDENT + 'if ' + LCondition + ' then');
      Add(INDENT + INDENT + LCall);
    end;
  end;

  for var LApiKey in ACall.ApiKeys do
  begin
    case LApiKey.Location of
      TGenParameterLocation.Query:
        LCall := 'LQuery := LQuery + [' + Quoted(LApiKey.KeyName + '=') + ' + Encode(F' + LApiKey.PropertyName.TrimLeft(['&']) + ')];';
      TGenParameterLocation.Cookie:
        LCall := 'LCookies := LCookies + [' + Quoted(LApiKey.KeyName + '=') + ' + Encode(F' + LApiKey.PropertyName.TrimLeft(['&']) + ')];';
    else
      LCall := 'LHeaders := LHeaders + [' + Quoted(LApiKey.KeyName + '=') + ' + F' + LApiKey.PropertyName.TrimLeft(['&']) + '];';
    end;
    Add(INDENT + 'if F' + LApiKey.PropertyName.TrimLeft(['&']) + ' <> '''' then');
    Add(INDENT + INDENT + LCall);
  end;

  if LHasCookies then
  begin
    Add(INDENT + 'if Length(LCookies) > 0 then');
    Add(INDENT + INDENT + 'LHeaders := LHeaders + [''Cookie='' + string.Join(''; '', LCookies)];');
  end;

  // the body
  LBody := 'nil';
  if ACall.BodyParam <> nil then
    case ACall.BodyKind of
      TGenClientContentKind.JSON:
        LBody := 'ToJSON<' + ACall.BodyParam.TypeName + '>(' + ACall.BodyParam.Name + ')';
      TGenClientContentKind.AnyJSON:
        LBody := 'JSONBytes(' + ACall.BodyParam.Name + ')';
      TGenClientContentKind.Binary:
        LBody := ACall.BodyParam.Name;
      TGenClientContentKind.Text:
        LBody := 'TEncoding.UTF8.GetBytes(' + ACall.BodyParam.Name + ')';
    end;

  LCall := 'Execute(' + Quoted(ACall.HttpMethod) + ', ' + PathExpression(ACall) + ', LQuery, LHeaders, ' + LBody
    + ', ' + Quoted(ACall.ContentType) + ', ' + Quoted(ACall.Accept) + ')';
  case ACall.ResultKind of
    TGenClientContentKind.None:
      Add(INDENT + LCall + ';');
    TGenClientContentKind.JSON:
      Add(INDENT + 'Result := FromJSON<' + ACall.ResultType + '>(' + LCall + ');');
    TGenClientContentKind.AnyJSON:
      Add(INDENT + 'Result := TJSONValue.ParseJSONValue(' + LCall + ', 0);');
    TGenClientContentKind.Binary:
      Add(INDENT + 'Result := ' + LCall + ';');
    TGenClientContentKind.Text:
      Add(INDENT + 'Result := TEncoding.UTF8.GetString(' + LCall + ');');
  end;
  Add('end;');
end;

function TGenClientEmitter.ClientUnit: string;
var
  LApi: TGenApi;
  LSource, LClass, LField: string;
begin
  LApi := FClient.Api;
  LClass := FClient.ClientClass;
  LSource := '"' + LApi.Title + '"';
  if LApi.Version <> '' then
    LSource := LSource + ' ' + LApi.Version;
  if LApi.OpenAPIVersion <> '' then
    LSource := LSource + ' (OpenAPI ' + LApi.OpenAPIVersion + ')';

  FLines.Clear;
  Add('(*');
  Add('  Generated by MARSgen from ' + LSource + '.');
  Add('  Do not edit: this unit is written again when the document is imported again.');
  Add('*)');
  Add('unit ' + FClient.UnitName + ';');
  Add;
  Add('// A typed client of the API, on top of the MARS client:');
  Add('//');
  Add('//   LClient := ' + LClass + '.Create; // or Create(''http://...'')');
  Add('//   try');
  if FClient.UsesToken then
    Add('//     LClient.Token := ...; // the token of the API');
  Add('//     ... LClient.<operation>(...) ...');
  Add('//   finally');
  Add('//     LClient.Free;');
  Add('//   end;');
  Add('//');
  Add('// Errors: EMARSClientHttpException (MARS.Client.Utils), with the status code and the content of');
  Add('// the response. Objects and TJSONValue results belong to the caller. Not thread-safe: a client');
  Add('// per thread.');
  Add;
  Add('interface');
  Add;
  Add('uses');
  Add(INDENT + 'System.SysUtils, System.Classes, System.JSON'
    + IfThen(FClient.Types.Options.Classes, ', System.Generics.Collections'));
  // MARS.Core.JSON: TJSONRawString in the signatures
  Add(', MARS.Core.JSON, MARS.Client.Client');
  Add(', ' + FClient.Types.UnitName);
  Add(';');
  Add;
  Add('type');
  Add(INDENT + LClass + ' = class');
  Add(INDENT + 'public const');
  Add(INDENT + INDENT + 'DefaultBaseURL = ' + Quoted(FClient.DefaultBaseURL) + ';');
  Add(INDENT + 'private');
  Add(INDENT + INDENT + 'FClient: TMARSCustomClient;');
  Add(INDENT + INDENT + 'FOwnsClient: Boolean;');
  Add(INDENT + INDENT + 'FBaseURL: string;');
  Add(INDENT + INDENT + 'FToken: string;');
  for var LApiKey in FClient.ApiKeys do
    Add(INDENT + INDENT + 'F' + LApiKey.PropertyName.TrimLeft(['&']) + ': string;');
  Add(INDENT + 'protected');
  Add(INDENT + INDENT + '// a request: APath encoded, AQuery of encoded name=value pairs, AHeaders of name=value pairs');
  Add(INDENT + INDENT + 'function Execute(const AMethod, APath: string; const AQuery, AHeaders: TArray<string>;');
  Add(INDENT + INDENT + INDENT + 'const ABody: TBytes; const AContentType, AAccept: string): TBytes; virtual;');
  Add(INDENT + INDENT + 'class function Encode(const AValue: string): string; static;');
  Add(INDENT + INDENT + 'class function Text(const AValue: string): string; overload; static;');
  Add(INDENT + INDENT + 'class function Text(const AValue: Integer): string; overload; static;');
  Add(INDENT + INDENT + 'class function Text(const AValue: Int64): string; overload; static;');
  Add(INDENT + INDENT + 'class function Text(const AValue: Double): string; overload; static;');
  Add(INDENT + INDENT + 'class function Text(const AValue: Currency): string; overload; static;');
  Add(INDENT + INDENT + 'class function Text(const AValue: Boolean): string; overload; static;');
  Add(INDENT + INDENT + 'class function Join<T>(const AValues: TArray<T>; const AEncode: Boolean): string; static;');
  Add(INDENT + INDENT + 'class function ToJSON<T>(const AValue: T): TBytes; static;');
  Add(INDENT + INDENT + 'class function JSONBytes(const AValue: TJSONValue): TBytes; static;');
  Add(INDENT + INDENT + 'class function FromJSON<T>(const AContent: TBytes): T; static;');
  Add(INDENT + 'public');
  Add(INDENT + INDENT + '// AClient: nil for a TMARSNetClient of this client (the token as a Bearer header); otherwise');
  Add(INDENT + INDENT + '// yours (not freed), for timeouts, proxy, logging, another HTTP library');
  Add(INDENT + INDENT + 'constructor Create(const ABaseURL: string = DefaultBaseURL; const AClient: TMARSCustomClient = nil);');
  Add(INDENT + INDENT + 'destructor Destroy; override;');
  for var LCall in FClient.Calls do
  begin
    Add;
    AddComments(LCall.Comments, INDENT + INDENT);
    for var LParam in LCall.Params do
      if Length(LParam.Comments) > 0 then
        AddComments([LParam.Name + ': ' + string.Join('; ', LParam.Comments)], INDENT + INDENT);
    Add(INDENT + INDENT + Signature(LCall, '', True));
  end;
  Add;
  Add(INDENT + INDENT + 'property BaseURL: string read FBaseURL write FBaseURL;');
  Add(INDENT + INDENT + '// sent as the MARS client does (a Bearer header with the client of this one)');
  Add(INDENT + INDENT + 'property Token: string read FToken write FToken;');
  for var LApiKey in FClient.ApiKeys do
  begin
    LField := 'F' + LApiKey.PropertyName.TrimLeft(['&']);
    Add(INDENT + INDENT + '// API key ' + LApiKey.Scheme + ': ' + ParameterLocationToString(LApiKey.Location) + ' '
      + LApiKey.KeyName + ', sent when not empty');
    Add(INDENT + INDENT + 'property ' + LApiKey.PropertyName + ': string read ' + LField + ' write ' + LField + ';');
  end;
  Add(INDENT + INDENT + 'property Client: TMARSCustomClient read FClient;');
  Add(INDENT + 'end;');
  Add;
  Add('implementation');
  Add;
  Add('uses');
  Add(INDENT + 'System.Rtti, System.NetEncoding');
  Add(', MARS.Client.Client.Net');
  Add(';');
  Add;
  Add('{ ' + LClass + ' }');
  Add;
  Add('constructor ' + LClass + '.Create(const ABaseURL: string; const AClient: TMARSCustomClient);');
  Add('begin');
  Add(INDENT + 'inherited Create;');
  Add(INDENT + 'FBaseURL := ABaseURL;');
  Add(INDENT + 'FClient := AClient;');
  Add(INDENT + 'if not Assigned(FClient) then');
  Add(INDENT + 'begin');
  Add(INDENT + INDENT + 'FClient := TMARSNetClient.Create(nil);');
  Add(INDENT + INDENT + 'FClient.AuthEndorsement := TMARSAuthEndorsement.AuthorizationBearer;');
  Add(INDENT + INDENT + 'FOwnsClient := True;');
  Add(INDENT + 'end;');
  Add('end;');
  Add;
  Add('destructor ' + LClass + '.Destroy;');
  Add('begin');
  Add(INDENT + 'if FOwnsClient then');
  Add(INDENT + INDENT + 'FClient.Free;');
  Add(INDENT + 'inherited;');
  Add('end;');
  Add;
  Add('function ' + LClass + '.Execute(const AMethod, APath: string; const AQuery, AHeaders: TArray<string>;');
  Add(INDENT + 'const ABody: TBytes; const AContentType, AAccept: string): TBytes;');
  Add('var');
  Add(INDENT + 'LURL: string;');
  Add(INDENT + 'LHeaders: TStringList;');
  Add(INDENT + 'LContent, LResponse: TBytesStream;');
  Add('begin');
  Add(INDENT + 'LURL := FBaseURL.TrimRight([''/'']) + APath;');
  Add(INDENT + 'if Length(AQuery) > 0 then');
  Add(INDENT + INDENT + 'LURL := LURL + ''?'' + string.Join(''&'', AQuery);');
  Add(INDENT + 'LHeaders := TStringList.Create;');
  Add(INDENT + 'try');
  Add(INDENT + INDENT + 'LHeaders.AddStrings(AHeaders);');
  Add(INDENT + INDENT + 'FClient.ApplyCustomHeaders(LHeaders);');
  Add(INDENT + INDENT + 'LContent := TBytesStream.Create(ABody);');
  Add(INDENT + INDENT + 'LResponse := TBytesStream.Create;');
  Add(INDENT + INDENT + 'try');
  Add(INDENT + INDENT + INDENT + 'if AMethod = ''GET'' then');
  Add(INDENT + INDENT + INDENT + INDENT + 'FClient.Get(LURL, LResponse, FToken, AAccept, AContentType)');
  Add(INDENT + INDENT + INDENT + 'else if AMethod = ''POST'' then');
  Add(INDENT + INDENT + INDENT + INDENT + 'FClient.Post(LURL, LContent, LResponse, FToken, AAccept, AContentType)');
  Add(INDENT + INDENT + INDENT + 'else if AMethod = ''PUT'' then');
  Add(INDENT + INDENT + INDENT + INDENT + 'FClient.Put(LURL, LContent, LResponse, FToken, AAccept, AContentType)');
  Add(INDENT + INDENT + INDENT + 'else if AMethod = ''DELETE'' then');
  Add(INDENT + INDENT + INDENT + INDENT + 'FClient.Delete(LURL, LContent, LResponse, FToken, AAccept, AContentType)');
  Add(INDENT + INDENT + INDENT + 'else if AMethod = ''PATCH'' then');
  Add(INDENT + INDENT + INDENT + INDENT + 'FClient.Patch(LURL, LContent, LResponse, FToken, AAccept, AContentType)');
  Add(INDENT + INDENT + INDENT + 'else');
  Add(INDENT + INDENT + INDENT + INDENT + 'FClient.Query(LURL, LContent, LResponse, FToken, AAccept, AContentType);');
  Add(INDENT + INDENT + INDENT + 'Result := Copy(LResponse.Bytes, 0, LResponse.Size);');
  Add(INDENT + INDENT + 'finally');
  Add(INDENT + INDENT + INDENT + 'LResponse.Free;');
  Add(INDENT + INDENT + INDENT + 'LContent.Free;');
  Add(INDENT + INDENT + INDENT + '// the headers of this request only: an empty value removes them');
  Add(INDENT + INDENT + INDENT + 'for var LIndex := 0 to LHeaders.Count - 1 do');
  Add(INDENT + INDENT + INDENT + INDENT + 'LHeaders[LIndex] := LHeaders.Names[LIndex] + ''='';');
  Add(INDENT + INDENT + INDENT + 'FClient.ApplyCustomHeaders(LHeaders);');
  Add(INDENT + INDENT + 'end;');
  Add(INDENT + 'finally');
  Add(INDENT + INDENT + 'LHeaders.Free;');
  Add(INDENT + 'end;');
  Add('end;');
  Add;
  Add('class function ' + LClass + '.Encode(const AValue: string): string;');
  Add('const');
  Add(INDENT + 'HEX = ''0123456789ABCDEF'';');
  Add('begin');
  Add(INDENT + '// percent-encoding of UTF-8, all but the unreserved characters (RFC 3986)');
  Add(INDENT + 'Result := '''';');
  Add(INDENT + 'for var LByte in TEncoding.UTF8.GetBytes(AValue) do');
  Add(INDENT + INDENT + 'if CharInSet(Char(LByte), [''A''..''Z'', ''a''..''z'', ''0''..''9'', ''-'', ''.'', ''_'', ''~'']) then');
  Add(INDENT + INDENT + INDENT + 'Result := Result + Char(LByte)');
  Add(INDENT + INDENT + 'else');
  Add(INDENT + INDENT + INDENT + 'Result := Result + ''%'' + HEX[LByte shr 4 + 1] + HEX[LByte and $F + 1];');
  Add('end;');
  Add;
  Add('class function ' + LClass + '.Text(const AValue: string): string;');
  Add('begin');
  Add(INDENT + 'Result := AValue;');
  Add('end;');
  Add;
  Add('class function ' + LClass + '.Text(const AValue: Integer): string;');
  Add('begin');
  Add(INDENT + 'Result := AValue.ToString;');
  Add('end;');
  Add;
  Add('class function ' + LClass + '.Text(const AValue: Int64): string;');
  Add('begin');
  Add(INDENT + 'Result := AValue.ToString;');
  Add('end;');
  Add;
  Add('class function ' + LClass + '.Text(const AValue: Double): string;');
  Add('begin');
  Add(INDENT + 'Result := FloatToStr(AValue, TFormatSettings.Invariant);');
  Add('end;');
  Add;
  Add('class function ' + LClass + '.Text(const AValue: Currency): string;');
  Add('begin');
  Add(INDENT + 'Result := CurrToStr(AValue, TFormatSettings.Invariant);');
  Add('end;');
  Add;
  Add('class function ' + LClass + '.Text(const AValue: Boolean): string;');
  Add('begin');
  Add(INDENT + 'if AValue then');
  Add(INDENT + INDENT + 'Result := ''true''');
  Add(INDENT + 'else');
  Add(INDENT + INDENT + 'Result := ''false'';');
  Add('end;');
  Add;
  Add('class function ' + LClass + '.Join<T>(const AValues: TArray<T>; const AEncode: Boolean): string;');
  Add('var');
  Add(INDENT + 'LText: string;');
  Add('begin');
  Add(INDENT + '// the values separated by commas (form or simple style, not exploded)');
  Add(INDENT + 'Result := '''';');
  Add(INDENT + 'for var LValue in AValues do');
  Add(INDENT + 'begin');
  Add(INDENT + INDENT + 'LText := TValue.From<T>(LValue).ToString;');
  Add(INDENT + INDENT + 'if TypeInfo(T) = TypeInfo(Boolean) then');
  Add(INDENT + INDENT + INDENT + 'LText := LowerCase(LText)');
  Add(INDENT + INDENT + 'else if (TypeInfo(T) = TypeInfo(Double)) or (TypeInfo(T) = TypeInfo(Currency)) then');
  Add(INDENT + INDENT + INDENT + 'LText := FloatToStr(TValue.From<T>(LValue).AsExtended, TFormatSettings.Invariant);');
  Add(INDENT + INDENT + 'if AEncode then');
  Add(INDENT + INDENT + INDENT + 'LText := Encode(LText);');
  Add(INDENT + INDENT + 'if Result <> '''' then');
  Add(INDENT + INDENT + INDENT + 'Result := Result + '','';');
  Add(INDENT + INDENT + 'Result := Result + LText;');
  Add(INDENT + 'end;');
  Add('end;');
  Add;
  Add('class function ' + LClass + '.ToJSON<T>(const AValue: T): TBytes;');
  Add('var');
  Add(INDENT + 'LJSON: TJSONValue;');
  Add('begin');
  Add(INDENT + 'LJSON := TJSONObject.TValueToJSONValue(TValue.From<T>(AValue));');
  Add(INDENT + 'try');
  Add(INDENT + INDENT + 'Result := JSONBytes(LJSON);');
  Add(INDENT + 'finally');
  Add(INDENT + INDENT + 'LJSON.Free;');
  Add(INDENT + 'end;');
  Add('end;');
  Add;
  Add('class function ' + LClass + '.JSONBytes(const AValue: TJSONValue): TBytes;');
  Add('begin');
  Add(INDENT + 'if Assigned(AValue) then');
  Add(INDENT + INDENT + 'Result := TEncoding.UTF8.GetBytes(AValue.ToJSON)');
  Add(INDENT + 'else');
  Add(INDENT + INDENT + 'Result := TEncoding.UTF8.GetBytes(''null'');');
  Add('end;');
  Add;
  Add('class function ' + LClass + '.FromJSON<T>(const AContent: TBytes): T;');
  Add('var');
  Add(INDENT + 'LJSON: TJSONValue;');
  Add(INDENT + 'LValue: TValue;');
  Add(INDENT + 'LContext: TRttiContext;');
  Add('begin');
  Add(INDENT + 'Result := Default(T);');
  Add(INDENT + 'if Length(AContent) = 0 then');
  Add(INDENT + INDENT + 'Exit;');
  Add(INDENT + 'LJSON := TJSONValue.ParseJSONValue(AContent, 0);');
  Add(INDENT + 'if not Assigned(LJSON) then');
  Add(INDENT + INDENT + 'raise EJSONException.Create(''The response is not JSON'');');
  Add(INDENT + 'try');
  Add(INDENT + INDENT + 'LValue := TValue.Empty;');
  Add(INDENT + INDENT + 'TJSONObject.TJSONValueToTValue(LJSON, LContext.GetType(TypeInfo(T)), LValue);');
  Add(INDENT + INDENT + 'if not LValue.IsEmpty then');
  Add(INDENT + INDENT + INDENT + 'Result := LValue.AsType<T>;');
  Add(INDENT + 'finally');
  Add(INDENT + INDENT + 'LJSON.Free;');
  Add(INDENT + 'end;');
  Add('end;');

  for var LCall in FClient.Calls do
  begin
    Add;
    Add(Signature(LCall, LClass, False));
    AddBody(LCall);
  end;

  Add;
  Add('end.');
  Result := FLines.Text;
end;

class function TGenClientEmitter.WriteTo(const AApi: TGenApi; const ATypes: TGenTypeMapper;
  const AOutput: TGenOutput): string;
var
  LClient: TGenClientMapper;
  LEmitter: TGenClientEmitter;
begin
  LClient := TGenClientMapper.Create(AApi, ATypes);
  try
    LClient.Execute;
    LEmitter := TGenClientEmitter.Create(LClient);
    try
      AOutput.WriteGenerated(LClient.UnitName + '.pas', LEmitter.ClientUnit);
      Result := LClient.UnitName;
    finally
      LEmitter.Free;
    end;
  finally
    LClient.Free;
  end;
end;

end.
