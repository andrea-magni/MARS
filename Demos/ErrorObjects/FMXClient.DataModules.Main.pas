(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes
, MARS.Client.Application
, MARS.Client.Client, MARS.Client.Client.Net, MARS.Client.CustomResource
, MARS.Client.Resource
, MARS.Core.JSON
;

type
  TMainDataModule = class(TDataModule)
    MARSApplication: TMARSClientApplication;
    MARSClient: TMARSNetClient;
    ErrorWithResponseResource: TMARSClientResource;
  private
  public
    procedure TryErrorWithResponse(const AOnError: TProc<TJSONObject>); overload;
    procedure TryErrorWithResponse<T>(const AOnError: TProc<T>); overload;
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  MARS.Client.Utils
, MARS.Core.Utils
;

{ TMainDataModule }

procedure TMainDataModule.TryErrorWithResponse(const AOnError: TProc<TJSONObject>);
begin
  ErrorWithResponseResource.GET(
    procedure
    begin
      // before execute here
    end
  , procedure (AResponse: TStream)
    begin
      // OK, got a response
    end
  , procedure (AException: Exception)
    begin
      // ERROR

      if (AException is EMARSClientHttpException) then
      begin
        var LException := EMARSClientHttpException(AException);

        if Assigned(AOnError) then
        begin
          var LJSON := LException.ContentAsJSON as TJSONObject;
          try
            AOnError(LJSON);
          finally
            LJSON.Free;
          end;
        end;

      end;

    end
  );
end;

procedure TMainDataModule.TryErrorWithResponse<T>(const AOnError: TProc<T>);
begin
  ErrorWithResponseResource.GET(
    procedure
    begin
      // before execute here
    end
  , procedure (AResponse: TStream)
    begin
      // OK, got a response
    end
  , procedure (AException: Exception)
    begin
      // ERROR

      if (AException is EMARSClientHttpException) then
      begin
        var LException := EMARSClientHttpException(AException);

        if Assigned(AOnError) then
            AOnError(LException.ContentAs<T>);
      end;

    end
  );
end;

end.
