(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit ServerTests;

interface

uses
  TestFramework
  , Generics.Collections
  , MARS.http.Server.Indy
  , MARS.Core.Engine
  , MARS.Core.Application
  ;

type
  TMARSServerTestCase = class(TTestCase)
  private
  protected
    FEngine: TMARSEngine;
    FServer: TMARShttpServerIndy;
    procedure SetUp; override;
    procedure TearDown; override;

    procedure InitApplication(const AApplication: TMARSApplication); virtual;
  end;

implementation

uses
  SysUtils;

{ TMARSServerTestCase }

procedure TMARSServerTestCase.InitApplication(const AApplication: TMARSApplication);
begin

end;

procedure TMARSServerTestCase.SetUp;
begin
  inherited;
  FEngine := TMARSEngine.Create;

  // Engine configuration
  FEngine.Port := 8080;
  FEngine.Name := 'MARS Test';
  FEngine.BasePath := '/rest';
  FEngine.ThreadPoolSize := 5;

  // Application configuration
  InitApplication(FEngine.AddApplication('Default', 'default', []));

  // Create http server
  FServer := TMARShttpServerIndy.Create(FEngine);

  if not FServer.Active then
    FServer.Active := True;
end;

procedure TMARSServerTestCase.TearDown;
begin
  inherited;
  FServer.Free;
  FServer := nil;

  FEngine.Free;
  FEngine := nil;
end;

end.
