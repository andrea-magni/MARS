(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Activation.Interfaces;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, Rtti, Diagnostics
  , HTTPApp

  , MARS.Core.URL
  , MARS.Core.Application
  , MARS.Core.Engine
  , MARS.Core.Token
  , MARS.Core.Registry
  , MARS.Core.MediaType
  , MARS.Core.Injection.Types
  ;

type

  IMARSActivation = interface
    procedure AddToContext(AValue: TValue);
    function HasToken: Boolean;

    procedure Invoke;

    function GetApplication: TMARSApplication;
    function GetEngine: TMARSEngine;
    function GetInvocationTime: TStopwatch;
    function GetMethod: TRttiMethod;
    function GetMethodArguments: TArray<TValue>;
    function GetMethodResult: TValue;
    function GetRequest: TWebRequest;
    function GetResource: TRttiType;
    function GetResourceInstance: TObject;
    function GetResponse: TWebResponse;
    function GetURL: TMARSURL;
    function GetURLPrototype: TMARSURL;
    function GetToken: TMARSToken;

    property Application: TMARSApplication read GetApplication;
    property Engine: TMARSEngine read GetEngine;
    property InvocationTime: TStopwatch read GetInvocationTime;
    property Method: TRttiMethod read GetMethod;
    property MethodArguments: TArray<TValue> read GetMethodArguments;
    property MethodResult: TValue read GetMethodResult;
    property Request: TWebRequest read GetRequest;
    property Resource: TRttiType read GetResource;
    property ResourceInstance: TObject read GetResourceInstance;
    property Response: TWebResponse read GetResponse;
    property URL: TMARSURL read GetURL;
    property URLPrototype: TMARSURL read GetURLPrototype;
    property Token: TMARSToken read GetToken;
  end;


implementation

end.
