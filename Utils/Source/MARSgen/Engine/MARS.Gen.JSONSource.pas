(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.JSONSource;

// Loads a document (OpenAPI, JSON sample) from a file, a URL or a string, as JSON or YAML, into
// a System.JSON tree: the readers of MARSgen work on JSON only.
//
// YAML is converted with the YAML 1.2 core schema: plain scalars that look like null, booleans
// and numbers become JSON null, true/false and numbers (with their text: 1.0 stays 1.0); quoted
// scalars and !!str ones stay strings. Aliases are resolved, merge keys (<<) applied.

interface

uses
  System.SysUtils, System.Classes, System.JSON;

type
  EGenSourceError = class(Exception);

  TGenSourceFormat = (gsfAuto, gsfJSON, gsfYAML);

  TGenJSONSource = class
  public
    // the caller owns the result
    class function FromText(const AText: string; const AFormat: TGenSourceFormat = gsfAuto;
      const ASourceName: string = ''): TJSONValue; static;
    class function FromFile(const AFileName: string; const AFormat: TGenSourceFormat = gsfAuto): TJSONValue; static;
    class function FromURL(const AURL: string; const AFormat: TGenSourceFormat = gsfAuto): TJSONValue; static;
    // a file name or an http(s) URL
    class function Load(const AFileNameOrURL: string; const AFormat: TGenSourceFormat = gsfAuto): TJSONValue; static;

    // JSON when the first significant character is { or [, YAML otherwise
    class function DetectFormat(const AText: string): TGenSourceFormat; static;
    class function YAMLToJSON(const AText: string): TJSONValue; static;
  end;

implementation

uses
  System.IOUtils, System.RegularExpressions, System.Net.HttpClient, System.Net.URLClient,
  System.NetConsts, Neslib.Utf8, Neslib.Yaml;

const
  // YAML 1.2 core schema
  RE_NULL = '^(null|Null|NULL|~)?$';
  RE_BOOL_TRUE = '^(true|True|TRUE)$';
  RE_BOOL_FALSE = '^(false|False|FALSE)$';
  RE_INT = '^[-+]?[0-9]+$';
  RE_INT_OCT = '^0o[0-7]+$';
  RE_INT_HEX = '^0x[0-9a-fA-F]+$';
  RE_FLOAT = '^[-+]?(\.[0-9]+|[0-9]+(\.[0-9]*)?)([eE][-+]?[0-9]+)?$';

function TryOctalToInt64(const ADigits: string; out AValue: Int64): Boolean;
begin
  AValue := 0;
  for var LChar in ADigits do
  begin
    if AValue > High(Int64) div 8 then
      Exit(False);
    AValue := AValue * 8 + (Ord(LChar) - Ord('0'));
  end;
  Result := ADigits <> '';
end;

function ScalarToJSON(const ANode: TYamlNode): TJSONValue;
var
  LText: string;
  LInt: Int64;
begin
  LText := ANode.ToString;

  // a key without a value (key:) is null, whatever tag the parser gives it
  if (LText = '') and (ANode.ScalarStyle in [TYamlScalarStyle.Any, TYamlScalarStyle.Plain]) then
    Exit(TJSONNull.Create);

  // quoted, block or explicitly tagged as a string: a string
  if not (ANode.ScalarStyle in [TYamlScalarStyle.Any, TYamlScalarStyle.Plain])
    or ANode.Tag.EndsWith('str')
  then
    Exit(TJSONString.Create(LText));

  if TRegEx.IsMatch(LText, RE_NULL) then
    Result := TJSONNull.Create
  else if TRegEx.IsMatch(LText, RE_BOOL_TRUE) then
    Result := TJSONBool.Create(True)
  else if TRegEx.IsMatch(LText, RE_BOOL_FALSE) then
    Result := TJSONBool.Create(False)
  else if TRegEx.IsMatch(LText, RE_INT) then
    // the text as it is (no Int64 range limit, leading + removed)
    Result := TJSONNumber.Create(LText.TrimLeft(['+']))
  else if TRegEx.IsMatch(LText, RE_INT_OCT) and TryOctalToInt64(LText.Substring(2), LInt) then
    Result := TJSONNumber.Create(LInt)
  else if TRegEx.IsMatch(LText, RE_INT_HEX) and TryStrToInt64('$' + LText.Substring(2), LInt) then
    Result := TJSONNumber.Create(LInt)
  else if TRegEx.IsMatch(LText, RE_FLOAT) then
  begin
    // JSON wants a digit around the dot: .5 -> 0.5, 1. -> 1.0
    LText := LText.TrimLeft(['+']);
    if LText.StartsWith('.') then
      LText := '0' + LText
    else if LText.StartsWith('-.') then
      LText := '-0' + LText.Substring(1);
    LText := TRegEx.Replace(LText, '\.([eE]|$)', '.0$1');
    Result := TJSONNumber.Create(LText);
  end
  else
    // .inf, .nan and everything else
    Result := TJSONString.Create(LText);
end;

function NodeToJSON(const ANode: TYamlNode; const ADepth: Integer): TJSONValue; forward;

procedure MergeInto(const AObject: TJSONObject; const ASource: TYamlNode; const ADepth: Integer);
var
  LSource: TYamlNode;
  LElement: PYamlElement;
begin
  // <<: *anchor or <<: [*a, *b]: keys not already present are added
  LSource := ASource;
  if LSource.NodeType = TYamlNodeType.Alias then
    LSource := LSource.Target;

  if LSource.NodeType = TYamlNodeType.Sequence then
  begin
    for var LIndex := 0 to LSource.Count - 1 do
      MergeInto(AObject, LSource.Nodes[LIndex], ADepth);
    Exit;
  end;

  if LSource.NodeType <> TYamlNodeType.Mapping then
    raise EGenSourceError.Create('YAML: a merge key (<<) needs a mapping');

  for var LIndex := 0 to LSource.Count - 1 do
  begin
    LElement := LSource.Elements[LIndex];
    if AObject.GetValue(LElement.Key.ToString) = nil then
      AObject.AddPair(LElement.Key.ToString, NodeToJSON(LElement.Value, ADepth + 1));
  end;
end;

function NodeToJSON(const ANode: TYamlNode; const ADepth: Integer): TJSONValue;
var
  LElement: PYamlElement;
  LKey: string;
  LObject: TJSONObject;
  LArray: TJSONArray;
  LMerges: TArray<TYamlNode>;
begin
  // aliases of aliases in a cycle: a document that cannot be expanded into a tree
  if ADepth > 512 then
    raise EGenSourceError.Create('YAML: nesting too deep (recursive aliases?)');

  case ANode.NodeType of
    TYamlNodeType.Null:
      Result := TJSONNull.Create;

    TYamlNodeType.Scalar:
      Result := ScalarToJSON(ANode);

    TYamlNodeType.Alias:
      Result := NodeToJSON(ANode.Target, ADepth + 1);

    TYamlNodeType.Sequence:
    begin
      LArray := TJSONArray.Create;
      try
        for var LIndex := 0 to ANode.Count - 1 do
          LArray.AddElement(NodeToJSON(ANode.Nodes[LIndex], ADepth + 1));
      except
        LArray.Free;
        raise;
      end;
      Result := LArray;
    end;

    TYamlNodeType.Mapping:
    begin
      LObject := TJSONObject.Create;
      try
        LMerges := [];
        for var LIndex := 0 to ANode.Count - 1 do
        begin
          LElement := ANode.Elements[LIndex];
          LKey := LElement.Key.ToString;
          if (LKey = '<<') and (LElement.Key.NodeType = TYamlNodeType.Scalar)
            and (LElement.Key.ScalarStyle in [TYamlScalarStyle.Any, TYamlScalarStyle.Plain])
          then
            LMerges := LMerges + [LElement.Value]
          else
          begin
            // a repeated key: the last one wins, as in most parsers
            LObject.RemovePair(LKey).Free;
            LObject.AddPair(LKey, NodeToJSON(LElement.Value, ADepth + 1));
          end;
        end;
        for var LMerge in LMerges do
          MergeInto(LObject, LMerge, ADepth);
      except
        LObject.Free;
        raise;
      end;
      Result := LObject;
    end;
  else
    raise EGenSourceError.Create('YAML: unsupported node');
  end;
end;

{ TGenJSONSource }

class function TGenJSONSource.DetectFormat(const AText: string): TGenSourceFormat;
begin
  for var LChar in AText do
  begin
    if CharInSet(LChar, [' ', #9, #10, #13]) or (LChar = #$FEFF) then
      Continue;
    if CharInSet(LChar, ['{', '[']) then
      Exit(gsfJSON);
    Break;
  end;
  Result := gsfYAML;
end;

class function TGenJSONSource.YAMLToJSON(const AText: string): TJSONValue;
var
  LDocument: IYamlDocument;
begin
  try
    LDocument := TYamlDocument.Parse(AText);
  except
    on E: Exception do
      raise EGenSourceError.Create('YAML: ' + E.Message);
  end;
  Result := NodeToJSON(LDocument.Root, 0);
end;

class function TGenJSONSource.FromText(const AText: string; const AFormat: TGenSourceFormat;
  const ASourceName: string): TJSONValue;
var
  LFormat: TGenSourceFormat;
  LPrefix: string;
begin
  LPrefix := '';
  if ASourceName <> '' then
    LPrefix := ASourceName + ': ';

  LFormat := AFormat;
  if LFormat = gsfAuto then
    LFormat := DetectFormat(AText);

  if LFormat = gsfJSON then
  begin
    try
      Result := TJSONValue.ParseJSONValue(AText, False, True);
    except
      on E: Exception do
        raise EGenSourceError.Create(LPrefix + 'JSON: ' + E.Message);
    end;
    if not Assigned(Result) then
      raise EGenSourceError.Create(LPrefix + 'not valid JSON');
  end
  else
  begin
    try
      Result := YAMLToJSON(AText);
    except
      on E: EGenSourceError do
        raise EGenSourceError.Create(LPrefix + E.Message);
    end;
  end;
end;

class function TGenJSONSource.FromFile(const AFileName: string; const AFormat: TGenSourceFormat): TJSONValue;
var
  LFormat: TGenSourceFormat;
  LExtension: string;
begin
  if not TFile.Exists(AFileName) then
    raise EGenSourceError.Create('File not found: ' + AFileName);

  LFormat := AFormat;
  if LFormat = gsfAuto then
  begin
    LExtension := TPath.GetExtension(AFileName).ToLower;
    if LExtension = '.json' then
      LFormat := gsfJSON
    else if (LExtension = '.yaml') or (LExtension = '.yml') then
      LFormat := gsfYAML;
  end;

  // UTF-8 unless a BOM says otherwise
  Result := FromText(TFile.ReadAllText(AFileName, TEncoding.UTF8), LFormat, TPath.GetFileName(AFileName));
end;

class function TGenJSONSource.FromURL(const AURL: string; const AFormat: TGenSourceFormat): TJSONValue;
var
  LClient: THTTPClient;
  LResponse: IHTTPResponse;
  LFormat: TGenSourceFormat;
  LContentType: string;
begin
  LClient := THTTPClient.Create;
  try
    LClient.Accept := 'application/json, application/yaml, application/x-yaml, text/yaml, */*';
    try
      LResponse := LClient.Get(AURL);
    except
      on E: Exception do
        raise EGenSourceError.Create(AURL + ': ' + E.Message);
    end;
    if LResponse.StatusCode <> 200 then
      raise EGenSourceError.CreateFmt('%s: HTTP %d %s', [AURL, LResponse.StatusCode, LResponse.StatusText]);

    LFormat := AFormat;
    if LFormat = gsfAuto then
    begin
      LContentType := LResponse.MimeType.ToLower;
      if LContentType.Contains('json') then
        LFormat := gsfJSON
      else if LContentType.Contains('yaml') then
        LFormat := gsfYAML;
    end;

    Result := FromText(LResponse.ContentAsString(TEncoding.UTF8), LFormat, AURL);
  finally
    LClient.Free;
  end;
end;

class function TGenJSONSource.Load(const AFileNameOrURL: string; const AFormat: TGenSourceFormat): TJSONValue;
begin
  if AFileNameOrURL.StartsWith('http://', True) or AFileNameOrURL.StartsWith('https://', True) then
    Result := FromURL(AFileNameOrURL, AFormat)
  else
    Result := FromFile(AFileNameOrURL, AFormat);
end;

end.
