(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Activation.Interfaces;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, Rtti, Diagnostics
, MARS.Core.URL, MARS.Core.Token
, MARS.Core.Engine.Interfaces
, MARS.Core.Application.Interfaces
, MARS.Core.MediaType, MARS.Core.Injection.Types, MARS.Core.RequestAndResponse.Interfaces
, MARS.Core.JSON
;

type
  {$M+}
  IMARSActivation = interface
    procedure AddToContext(AValue: TValue);
    function HasToken: Boolean;

    procedure Invoke;

    function GetId: string;
    function GetApplication: IMARSApplication;
    function GetEngine: IMARSEngine;
    function GetInvocationTime: TStopwatch;
    function GetSetupTime: TStopwatch;
    function GetTeardownTime: TStopwatch;
    function GetSerializationTime: TStopwatch;
    function GetMethod: TRttiMethod;
    function GetMethodReturnType: TRttiType;
    function GetMethodArguments: TArray<TValue>;
    function GetMethodAttributes: TArray<TCustomAttribute>;
    function GetMethodResult: TValue;
    function GetRequest: IMARSRequest;
    function GetResource: TRttiType;
    function GetResourceAttributes: TArray<TCustomAttribute>;
    function GetResourceInstance: TObject;
    function GetResourcePath: string;
    function GetResponse: IMARSResponse;
    function GetURL: TMARSURL;
    function GetURLPrototype: TMARSURL;
    function GetToken: TMARSToken;
    function GetEndpointName: string;

    property Id: string read GetId;
    property Application: IMARSApplication read GetApplication;
    property Engine: IMARSEngine read GetEngine;
    property InvocationTime: TStopwatch read GetInvocationTime;
    property SetupTime: TStopwatch read GetSetupTime;
    property TeardownTime: TStopwatch read GetTeardownTime;
    property SerializationTime: TStopwatch read GetSerializationTime;
    property Method: TRttiMethod read GetMethod;
    property MethodReturnType: TRttiType read GetMethodReturnType;
    property MethodArguments: TArray<TValue> read GetMethodArguments;
    property MethodAttributes: TArray<TCustomAttribute> read GetMethodAttributes;
    property MethodResult: TValue read GetMethodResult;
    property Request: IMARSRequest read GetRequest;
    property Resource: TRttiType read GetResource;
    property ResourceAttributes: TArray<TCustomAttribute> read GetResourceAttributes;
    property ResourceInstance: TObject read GetResourceInstance;
    property ResourcePath: string read GetResourcePath;
    property Response: IMARSResponse read GetResponse;
    property URL: TMARSURL read GetURL;
    property URLPrototype: TMARSURL read GetURLPrototype;
    property Token: TMARSToken read GetToken;
    // Resource.Method for resources, the route name (i.e. "GET people/{id:int}") for routes
    property EndpointName: string read GetEndpointName;
  end;

  // JSON serialization options for AActivation (readers and writers): the global default
  // (DefaultMARSJSONSerializationOptions), then the JSON.* application parameters, then the
  // attributes of the resource and of the method. Without an activation: the global default.
  function JSONSerializationOptionsFor(const AActivation: IMARSActivation): TMARSJSONSerializationOptions;

implementation

function JSONSerializationOptionsFor(const AActivation: IMARSActivation): TMARSJSONSerializationOptions;
begin
  Result := DefaultMARSJSONSerializationOptions;
  if not Assigned(AActivation) then
    Exit;
  {$IFNDEF MARS_JSON_LEGACY}
  if Assigned(AActivation.Application) then
    Result := Result.AdjustWith(AActivation.Application.Parameters);
  {$ENDIF}
  Result := Result
    .AdjustWith(AActivation.ResourceAttributes)
    .AdjustWith(AActivation.MethodAttributes);
end;

end.
