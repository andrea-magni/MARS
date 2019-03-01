(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Activation.Interfaces;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, Rtti, Diagnostics
, MARS.Core.URL, MARS.Core.Application, MARS.Core.Engine, MARS.Core.Token
, MARS.Core.MediaType, MARS.Core.Injection.Types, MARS.Core.RequestAndResponse.Interfaces
;

type
  IMARSActivation = interface
    procedure AddToContext(AValue: TValue);
    function HasToken: Boolean;

    procedure Invoke;

    function GetApplication: TMARSApplication;
    function GetEngine: TMARSEngine;
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
    function GetResponse: IMARSResponse;
    function GetURL: TMARSURL;
    function GetURLPrototype: TMARSURL;
    function GetToken: TMARSToken;

    property Application: TMARSApplication read GetApplication;
    property Engine: TMARSEngine read GetEngine;
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
    property Response: IMARSResponse read GetResponse;
    property URL: TMARSURL read GetURL;
    property URLPrototype: TMARSURL read GetURLPrototype;
    property Token: TMARSToken read GetToken;
  end;


implementation

end.
