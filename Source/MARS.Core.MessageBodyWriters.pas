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
  TJSONValueWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
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
  , Produces(TMediaType.WILDCARD)]
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


implementation

uses
    System.TypInfo
  , MARS.Core.JSON
  , MARS.Core.Utils
  , MARS.Rtti.Utils
  ;

{ TObjectWriter }

procedure TObjectWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LStreamWriter: TStreamWriter;
  LObj: TJSONObject;
begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    LObj := ObjectToJSON(AValue.AsObject);
    try
//      LObj.AddPair('Writer', ClassName);
      LStreamWriter.Write(LObj.ToJSON);
    finally
      LObj.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TJSONValueWriter }

procedure TJSONValueWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LStreamWriter: TStreamWriter;
  LJSONValue: TJSONValue;
  LJSONString: string;
  LCallbackName: string;
  LCallbackKey: string;
begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    LJSONValue := AValue.AsObject as TJSONValue;
    if not Assigned(LJSONValue) then
      Exit;

    LJSONString := LJSONValue.ToJSON;

    if AActivation.Application.Parameters.ByName('JSONP.Enabled', False).AsBoolean then
    begin
      LCallbackKey := AActivation.Application.Parameters.ByName('JSONP.CallbackKey', 'callback').AsString;
      LCallbackName := AActivation.URL.QueryTokenByName(LCallbackKey, True, False);
      if LCallbackName = '' then
        LCallbackName := 'callback';
      LJSONString := LCallbackName + '(' + LJSONString + ');';
      AActivation.Response.ContentType := 'text/javascript';
    end;

    LStreamWriter.Write(LJSONString);
  finally
    LStreamWriter.Free;
  end;
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
    if Assigned(LStream) then
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
  LJSONWriter: TJSONValueWriter;
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

      LJSONArray.AddElement(TJSONObject.RecordToJSON(LElement));
    end;

    LJSONWriter := TJSONValueWriter.Create;
    try
      LJSONWriter.WriteTo(LJSONArray, AMediaType, AOutputStream, AActivation);
    finally
      LJSONWriter.Free;
    end;
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
  LJSONWriter: TJSONValueWriter;
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

    LJSONWriter := TJSONValueWriter.Create;
    try
      LJSONWriter.WriteTo(LResult, AMediaType, AOutputStream, AActivation);
    finally
      LJSONWriter.Free;
    end;
  finally
    LResult.Free;
  end;
end;


procedure RegisterWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TJSONValue>(TJSONValueWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TStream>(TStreamValueWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TObject>(TObjectWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
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

  TMARSMessageBodyRegistry.Instance.RegisterWriter(TStandardMethodWriter
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := (AMediaType = TMediaType.APPLICATION_JSON) or (AMediaType = TMediaType.WILDCARD);
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
