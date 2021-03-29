(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.MessageBodyWriters;

interface

uses
  Classes, SysUtils, Rtti

  , MARS.Core.Attributes
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyWriter
  , MARS.Core.Activation.Interfaces
  ;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TObjectWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TArrayOfObjectWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TJSONValueWriter = class(TInterfacedObject, IMessageBodyWriter)
  protected
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);

    class procedure WriteJSONValue(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation); inline;
  end;

  [Produces(TMediaType.APPLICATION_XML)]
  TXMLWriter = class(TInterfacedObject, IMessageBodyWriter)
  protected
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);

    class procedure WriteXML(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation); inline;
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TRecordWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TArrayOfRecordWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_OCTET_STREAM)
  , Produces(TMediaType.WILDCARD)
  , Produces('data:image/png;base64')]
  TStreamValueWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TStandardMethodWriter = class(TInterfacedObject, IMessageBodyWriter)
  private
    procedure ForEachParameter(const AActivation: IMARSActivation;
      const ADoSomething: TProc<TRttiParameter, TValue>;
      const AFilterFunc: TFunc<TRttiParameter, Boolean> = nil);
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.WILDCARD)]
  TPrimitiveTypesWriter = class(TInterfacedObject, IMessageBodyWriter)
  private
  protected
    function GetProducesValue(const AActivation: IMARSActivation;
      var AContentType: string): Boolean; virtual;
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

implementation

uses
  System.TypInfo, Xml.XMLIntf, System.JSON
, MARS.Core.JSON, MARS.Core.Utils, MARS.Rtti.Utils
;

{ TObjectWriter }

procedure TObjectWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LJSON: TJSONValue;
begin
  LJSON := TJSONObject.ObjectToJSON(AValue.AsObject);
  try
    TJSONValueWriter.WriteJSONValue(LJSON, AMediaType, AOutputStream, AActivation);
  finally
    LJSON.Free;
  end;
end;

{ TArrayOfObjectWriter }

procedure TArrayOfObjectWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LJSONArray: TJSONArray;
  LIndex: Integer;
  LElement: TValue;
begin
  if not AValue.IsArray then
    Exit;

  LJSONArray := TJSONArray.Create;
  try
    for LIndex := 0 to AValue.GetArrayLength -1 do
    begin
      LElement := AValue.GetArrayElement(LIndex);

      LJSONArray.Add(TJSONObject.ObjectToJSON(LElement.AsObject));
    end;

    TJSONValueWriter.WriteJSONValue(LJSONArray, AMediaType, AOutputStream, AActivation);
  finally
    LJSONArray.Free;
  end;
end;

{ TJSONValueWriter }

class procedure TJSONValueWriter.WriteJSONValue(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
begin
  TMARSMessageBodyWriter.WriteWith<TJSONValueWriter>(AValue, AMediaType, AOutputStream, AActivation);
end;

procedure TJSONValueWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LJSONValue: TJSONValue;
  LJSONString: string;
  LCallbackName: string;
  LCallbackKey: string;
  LJSONPEnabled: Boolean;
  LJSONPProc: TProc<JSONPAttribute>;
  LContentType: string;
  LEncoding: TEncoding;
  LContentBytes: TBytes;
begin
  if not TMARSMessageBodyWriter.GetDesiredEncoding(AActivation, LEncoding) then
    LEncoding := TEncoding.UTF8; // UTF8 by default

  LJSONString := '';
  if AValue.IsType<string> then
    LJSONString := AValue.AsType<string>
  else if AValue.IsType<TJSONValue> then
  begin
    LJSONValue := AValue.AsObject as TJSONValue;
    LJSONString := LJSONValue.ToJSON;
  end;
  if LJSONString = '' then
    Exit;

  // JSONP
  LJSONPProc :=
    procedure (AAttr: JSONPAttribute)
    begin
      LJSONPEnabled := AAttr.Enabled;
      LCallbackKey := AAttr.CallbackKey;
      LContentType := AAttr.ContentType;
    end;

  LJSONPEnabled := False;
  if Assigned(AActivation) then
  begin
    if not AActivation.Method.HasAttribute<JSONPAttribute>(LJSONPProc) then
      AActivation.Resource.HasAttribute<JSONPAttribute>(LJSONPProc);
    if LJSONPEnabled then
    begin
      LCallbackName := AActivation.URL.QueryTokenByName(LCallbackKey, True, False);
      if LCallbackName = '' then
        LCallbackName := 'callback';

      if LJSONPEnabled then
      begin
        LJSONString := LCallbackName + '(' + LJSONString + ');';
        AActivation.Response.ContentType := LContentType;
      end;
    end;
  end;

  LContentBytes := LEncoding.GetBytes(LJSONString);
  AOutputStream.Write(LContentBytes, Length(LContentBytes));
end;

{ TStreamValueWriter }

procedure TStreamValueWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LStream: TStream;
begin
  if (not AValue.IsEmpty) and AValue.IsInstanceOf(TStream) then
  begin
    LStream := AValue.AsObject as TStream;
    if not Assigned(LStream) then
      Exit;

    if (not AMediaType.IsWildcard) and AMediaType.Matches('data:image/png;base64') then
      StringToStream(AOutputStream, StreamToBase64(LStream), TEncoding.ASCII)
    else
      AOutputStream.CopyFrom(LStream, LStream.Size);
  end;
end;

{ TRecordWriter }

procedure TRecordWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LJSONObj: TJSONObject;
  LJSONWriter: TJSONValueWriter;
begin
  if not AValue.IsEmpty then
  begin
    LJSONObj := TJSONObject.RecordToJSON(AValue);
    try
      LJSONWriter := TJSONValueWriter.Create;
      try
        LJSONWriter.WriteTo(LJSONObj, AMediaType, AOutputStream, AActivation);
      finally
        LJSONWriter.Free;
      end;
    finally
      LJSONObj.Free;
    end;
  end;
end;

{ TArrayOfRecordWriter }

procedure TArrayOfRecordWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LJSONArray: TJSONArray;
  LIndex: Integer;
  LElement: TValue;
begin
  if not AValue.IsArray then
    Exit;

  LJSONArray := TJSONArray.Create;
  try
    for LIndex := 0 to AValue.GetArrayLength -1 do
    begin
      LElement := AValue.GetArrayElement(LIndex);

      LJSONArray.Add(TJSONObject.RecordToJSON(LElement));
    end;

    TJSONValueWriter.WriteJSONValue(LJSONArray, AMediaType, AOutputStream, AActivation);
  finally
    LJSONArray.Free;
  end;
end;


{ TStandardMethodWriter }

procedure TStandardMethodWriter.ForEachParameter(const AActivation: IMARSActivation;
  const ADoSomething: TProc<TRttiParameter, TValue>;
  const AFilterFunc: TFunc<TRttiParameter, Boolean>);
var
  LParameter: TRttiParameter;
  LParameters: TArray<TRttiParameter>;
  LIndex: Integer;
begin
  LParameters := AActivation.Method.GetParameters;

  for LIndex := 0 to High(LParameters) do
  begin
    LParameter := LParameters[LIndex];

    if (not Assigned(AFilterFunc)) or AFilterFunc(LParameter) then
    begin
      if Assigned(ADoSomething) then
        ADoSomething(LParameter, AActivation.MethodArguments[LIndex]);
    end;
  end;
end;

procedure TStandardMethodWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LResult: TJSONObject;
  LOutputParams: TJSONArray;
begin
  LResult := TJSONObject.Create;
  try
    LResult.WriteTValue('result', AValue);

    LOutputParams := nil;
    ForEachParameter(AActivation
      , procedure (AParameter: TRttiParameter; AParameterValue: TValue)
        var
          LOutputParamJSON: TJSONObject;
        begin
          LOutputParamJSON := TJSONObject.Create;
          try
            LOutputParamJSON.WriteTValue('name', AParameter.Name);
            LOutputParamJSON.WriteTValue('value', AParameterValue);

            if not Assigned(LOutputParams) then
              LOutputParams := TJSONArray.Create;
            LOutputParams.Add(LOutputParamJSON);
          except
            LOutputParamJSON.Free;
            raise;
          end;
        end
      , function (AParameter: TRttiParameter): Boolean
        begin
          Result := ([pfOut, pfVar] * AParameter.Flags) <> []; // is a var or out argument
        end
    );
    if Assigned(LOutputParams) then
      LResult.AddPair('outputParams', LOutputParams);

    TJSONValueWriter.WriteJSONValue(LResult, AMediaType, AOutputStream, AActivation);
  finally
    LResult.Free;
  end;
end;

{ TPrimitiveTypesWriter }

function TPrimitiveTypesWriter.GetProducesValue(const AActivation: IMARSActivation;
  var AContentType: string): Boolean;
var
  LProduces: string;
  LFound: Boolean;
begin
  LFound := False;
  LProduces := '';

  AActivation.Method.HasAttribute<ProducesAttribute>(
    procedure(AAttr: ProducesAttribute)
    begin
      LProduces := AAttr.Value;
      LFound := True;
    end
  );
  if not LFound then
    AActivation.Resource.HasAttribute<ProducesAttribute>(
      procedure(AAttr: ProducesAttribute)
      begin
        LProduces := AAttr.Value;
        LFound := True;
      end
    );

  Result := LFound;
  if Result then
    AContentType := LProduces;
end;

procedure TPrimitiveTypesWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LEncoding: TEncoding;
  LContentBytes: TBytes;
  LContent: string;
  LContentType: string;
  LEncodingName: string;
begin
  if not TMARSMessageBodyWriter.GetDesiredEncoding(AActivation, LEncoding) then
    LEncoding := TEncoding.UTF8; // UTF8 by default
  LEncodingName := GetEncodingName(LEncoding);

  LContentType := AActivation.Response.ContentType;
  if GetProducesValue(AActivation, LContentType) then
  begin
    if not LContentType.ToLower.Contains('charset=') then
      LContentType := LContentType + '; charset=' + LEncodingName;
    AActivation.Response.ContentType := LContentType;
  end
  else
    AActivation.Response.ContentType := 'text/plain; charset=' + LEncodingName; // default: text/plain

  AActivation.Response.ContentEncoding := LEncodingName;

  LContent := TValueToString(AValue);

  LContentBytes := LEncoding.GetBytes(LContent);
  AOutputStream.Write(LContentBytes, Length(LContentBytes));
end;

{ TXMLWriter }

procedure TXMLWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LEncoding: TEncoding;
  LContentBytes: TBytes;
  LXMLDocument: IXMLDocument;
  LXMLString: string;
  LXMLNode: IXMLNode;
begin
  if not TMARSMessageBodyWriter.GetDesiredEncoding(AActivation, LEncoding) then
    LEncoding := TEncoding.UTF8; // UTF8 by default

  LXMLString := '';
  if AValue.IsType<string> then
    LXMLString := AValue.AsType<string>
  else if AValue.IsType<IXMLDocument> then
  begin
    LXMLDocument := AValue.AsType<IXMLDocument>;
    LXMLDocument.SaveToXML(LXMLString);
  end
  else if AValue.IsType<IXMLNode> then
  begin
    LXMLNode := AValue.AsType<IXMLNode>;
    LXMLString := LXMLNode.XML;
  end;

  if LXMLString = '' then
    Exit;

  LContentBytes := LEncoding.GetBytes(LXMLString);
  AOutputStream.Write(LContentBytes, Length(LContentBytes));
end;

class procedure TXMLWriter.WriteXML(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
begin
  TMARSMessageBodyWriter.WriteWith<TXMLWriter>(AValue, AMediaType, AOutputStream, AActivation);
end;

procedure RegisterWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TJSONValue>(TJSONValueWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TJSONValueWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := (AType.Handle = TypeInfo(TJSONRawString)) and (AMediaType = TMediaType.APPLICATION_JSON);
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter<IXMLDocument>(TXMLWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<IXMLNode>(TXMLWriter);

  TMARSMessageBodyRegistry.Instance.RegisterWriter<TStream>(TStreamValueWriter);

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TObjectWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := AType.IsObjectOfType<TObject>;
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_VERY_LOW;
      end
  );


  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TArrayOfObjectWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := AType.IsDynamicArrayOf<TObject>;
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_VERY_LOW;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TRecordWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := AType.IsRecord;
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TArrayOfRecordWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := AType.IsDynamicArrayOfRecord;
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TStandardMethodWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := (AMediaType = TMediaType.APPLICATION_JSON) or (AMediaType = TMediaType.WILDCARD);
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_ZERO;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TPrimitiveTypesWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := (AType.TypeKind in [tkInteger, tkInt64, tkChar, tkEnumeration, tkFloat,
          tkString, tkSet, tkWChar, tkLString, tkWString,
          tkVariant, tkArray, tkRecord, tkInt64, tkDynArray, tkUString]);
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_ZERO;
      end
  );
end;

initialization
  RegisterWriters;

end.
