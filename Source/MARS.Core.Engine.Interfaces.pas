(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)

unit MARS.Core.Engine.Interfaces;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Rtti, Generics.Collections
, MARS.Core.Classes, MARS.Core.URL, MARS.Core.Exceptions, MARS.Utils.Parameters
, MARS.Core.RequestAndResponse.Interfaces, MARS.Core.Application.Interfaces
, MARS.Core.Registry.Utils
;

type
  IMARSEngine = interface; // fwd

  TBeforeHandleRequestProc = reference to function(const AEngine: IMARSEngine;
    const AURL: TMARSURL; const ARequest: IMARSRequest; const AResponse: IMARSResponse;
    var Handled: Boolean): Boolean;
  TAfterHandleRequestProc = reference to procedure(const AEngine: IMARSEngine;
    const AURL: TMARSURL; const ARequest: IMARSRequest; const AResponse: IMARSResponse;
    var Handled: Boolean);
  TGetApplicationProc = reference to procedure (const AEngine: IMARSEngine;
    const AURL: TMARSURL; const ARequest: IMARSRequest; const AResponse: IMARSResponse;
    var AApplication: IMARSApplication);


  IMARSEngine = interface ['{222D2BB8-589A-456C-A5D3-C6BEAE4022B9}']
    function HandleRequest(ARequest: IMARSRequest; AResponse: IMARSResponse): Boolean;

    function AddApplication(const AName, ABasePath: string;
      const AResources: array of string; const AParametersSliceName: string = '';
      const ADefaultResourcePath: string = ''): IMARSApplication;
    function ApplicationByName(const AName: string): IMARSApplication;
    function ApplicationByBasePath(const ABasePath: string): IMARSApplication;

    procedure EnumerateApplications(const ADoSomething: TProc<string, IMARSApplication>);
    function IsCORSEnabled: Boolean;

    function GetApplications: TMARSApplicationDictionary;
    function GetParameters: TMARSParameters;
    function GetBasePath: string;
    procedure SetBasePath(const AValue: string);
    function GetName: string;
    function GetPort: Integer;
    procedure SetPort(const AValue: Integer);
    function GetPortSSL: Integer;
    procedure SetPortSSL(const AValue: Integer);
    function GetThreadPoolSize: Integer;
    procedure SetThreadPoolSize(const AValue: Integer);
    function GetBeforeHandleRequest: TBeforeHandleRequestProc;
    procedure SetBeforeHandleRequest(const AValue: TBeforeHandleRequestProc);
    function GetAfterHandleRequest: TAfterHandleRequestProc;
    procedure SetAfterHandleRequest(const AValue: TAfterHandleRequestProc);
    function GetOnGetApplication: TGetApplicationProc;
    procedure SetOnGetApplication(const AValue: TGetApplicationProc);

    property Applications: TMARSApplicationDictionary read GetApplications;
    property Parameters: TMARSParameters read GetParameters;

    property BasePath: string read GetBasePath write SetBasePath;
    property Name: string read GetName;
    property Port: Integer read GetPort write SetPort;
    property PortSSL: Integer read GetPortSSL write SetPortSSL;
    property ThreadPoolSize: Integer read GetThreadPoolSize write SetThreadPoolSize;

    property BeforeHandleRequest: TBeforeHandleRequestProc read GetBeforeHandleRequest write SetBeforeHandleRequest;
    property AfterHandleRequest: TAfterHandleRequestProc read GetAfterHandleRequest write SetAfterHandleRequest;
    property OnGetApplication: TGetApplicationProc read GetOnGetApplication write SetOnGetApplication;

  end;

implementation

end.
