(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Core.Response
  , MARS.Core.URL

  , MARS.Core.Token.Resource
  ;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;
  end;

  [Path('item'), Produces(TMediaType.APPLICATION_JSON)]
  TItemResource = class
    type
      TItem = record
        id: string;
        name: string;
        constructor Create(AId: string; AName: string);
      end;
  private
    [Context] URL: TMARSURL;
  public
    [GET]
    function GetItems: TArray<TItem>;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
    MARS.Core.Registry
{$IFDEF MSWINDOWS}
  , CodeSiteLogging
{$ENDIF}
;

{ THelloWorldResource }

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

{ TItemResource }

function TItemResource.GetItems: TArray<TItem>;
begin
{$IFDEF MSWINDOWS}
  CodeSite.SendMsg(URL.ToString);
{$ENDIF}
  Result := [
    TItem.Create('1', 'Andrea')
  , TItem.Create('2', 'Guido')
  ];
end;

{ TItemResource.TItem }

constructor TItemResource.TItem.Create(AId, AName: string);
begin
  id := AId;
  name := AName;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TItemResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
