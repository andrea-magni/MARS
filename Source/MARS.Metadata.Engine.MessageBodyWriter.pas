(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Metadata.Engine.MessageBodyWriter;

interface

uses
  Classes, SysUtils, Rtti

  , MARS.Core.Attributes
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyWriter
  , MARS.Core.Engine
  , MARS.Core.Application
  , MARS.Core.JSON
  , MARS.Core.Registry
;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TMARSEngineJSONWriter = class(TInterfacedObject, IMessageBodyWriter)
  private
    FEngine: TMARSEngine;
  protected
    function GetEngineJSON: TJSONObject; virtual;
    function GetApplicationsJSON: TJSONArray; virtual;
    function GetApplicationJSON(const AApplication: TMARSApplication): TJSONObject; virtual;
    function GetResourcesJSON(const AApplication: TMARSApplication): TJSONArray; virtual;
    function GetResourceJSON(const APath: string; const AConstructorInfo: TMARSConstructorInfo): TJSONObject; virtual;
    function GetMethodsJSON(const AResourceType: TRttiType): TJSONArray; virtual;
    function GetMethodJSON(const AMethod: TRttiMethod; const AHttpMethodAttribute: HttpMethodAttribute): TJSONObject; virtual;

    property Engine: TMARSEngine read FEngine;
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
  end;

implementation

uses
    MARS.Core.Utils
  , MARS.Rtti.Utils
  ;

{ TMARSEngineJSONWriter }

function TMARSEngineJSONWriter.GetApplicationJSON(
  const AApplication: TMARSApplication): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('Name', AApplication.Name);
    Result.AddPair('BasePath', AApplication.BasePath);

    Result.AddPair('Resources', GetResourcesJSON(AApplication));
  except
    Result.Free;
    raise;
  end;
end;

function TMARSEngineJSONWriter.GetApplicationsJSON: TJSONArray;
var
  LResult: TJSONArray;
begin
  LResult := TJSONArray.Create;
  try
    Engine.EnumerateApplications(
      procedure (APath: string; AApplication: TMARSApplication)
      begin
        LResult.Add(GetApplicationJSON(AApplication));
      end
    );
    Result := LResult;
  except
    LResult.Free;
    raise;
  end;
end;

function TMARSEngineJSONWriter.GetEngineJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('Name', Engine.Name);
    Result.AddPair('BasePath', Engine.BasePath);

    Result.AddPair('Applications', GetApplicationsJSON);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSEngineJSONWriter.GetMethodJSON(const AMethod: TRttiMethod;
  const AHttpMethodAttribute: HttpMethodAttribute): TJSONObject;
var
  LResult: TJSONObject;
begin
  LResult := TJSONObject.Create;
  try
    LResult.AddPair('HttpMethodName', AHttpMethodAttribute.HttpMethodName);
    LResult.AddPair('Name', AMethod.Name);
    AMethod.ForEachAttribute<MARSAttribute>(
      procedure (AAttribute: MARSAttribute)
      begin
        LResult.AddPair(AAttribute.ClassName, 'YES');
        //AM TODO
      end
    );
    Result := LResult;
  except
    LResult.Free;
    raise;
  end;
end;

function TMARSEngineJSONWriter.GetMethodsJSON(const AResourceType: TRttiType): TJSONArray;
var
  LResult: TJSONArray;
begin
  LResult := TJSONArray.Create;
  try
    AResourceType.ForEachMethodWithAttribute<HttpMethodAttribute>(
      function (AMethod: TRttiMethod; AHttpMethodAttribute: HttpMethodAttribute): Boolean
      begin
        Result := True;

        LResult.Add(GetMethodJSON(AMethod, AHttpMethodAttribute));
      end
    );
    Result := LResult;
  except
    LResult.Free;
    raise;
  end;
end;

function TMARSEngineJSONWriter.GetResourceJSON(const APath: string; const AConstructorInfo: TMARSConstructorInfo): TJSONObject;
var
  LRttiContext: TRttiContext;
  LResourceType: TRttiType;
begin
  LResourceType := LRttiContext.GetType(AConstructorInfo.TypeTClass);

  Result := TJSONObject.Create;
  try
    Result.AddPair('Path', APath);
    Result.AddPair('Type', LResourceType.Name);
    Result.AddPair('Methods', GetMethodsJSON(LResourceType));

    Result := Result;
  except
    Result.Free;
    raise;
  end;
end;

function TMARSEngineJSONWriter.GetResourcesJSON(
  const AApplication: TMARSApplication): TJSONArray;
var
  LResult: TJSONArray;
begin
  LResult := TJSONArray.Create;
  try
    AApplication.EnumerateResources(
      procedure (APath: string; AConstructorInfo: TMARSConstructorInfo)
      begin
        LResult.Add(GetResourceJSON(APath, AConstructorInfo));
      end
    );
    Result := LResult;
  except
    LResult.Free;
    raise;
  end;
end;

procedure TMARSEngineJSONWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LEngineJSON: TJSONObject;
begin
  FEngine := AValue.AsType<TMARSEngine>;
  Assert(Assigned(FEngine));

  LEngineJSON := GetEngineJSON;
  try
    LStreamWriter := TStreamWriter.Create(AOutputStream);
    try
      LStreamWriter.Write(LEngineJSON.ToJSON);
    finally
      LStreamWriter.Free;
    end;
  finally
    LEngineJSON.Free;
  end;
end;

initialization
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TMARSEngine>(TMARSEngineJSONWriter);

end.
