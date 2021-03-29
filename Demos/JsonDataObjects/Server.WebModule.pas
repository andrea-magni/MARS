(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.WebModule;

{$I MARS.inc}

interface

uses
{$ifdef DelphiXE3_UP}
  System.SysUtils, System.Classes, Web.HTTPApp;
{$else}
  SysUtils, Classes, HTTPApp;
{$endif}


type
  TServerWebModule = class(TWebModule)
    procedure ServerWebModuleDefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TServerWebModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  MARS.http.Server.Indy
, Server.Ignition;

procedure TServerWebModule.ServerWebModuleDefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  inherited;

  if not TServerEngine.Default.HandleRequest(TMARSWebRequest.Create(Request), TMARSWebResponse.Create(Response)) then
  begin
    Response.ContentType := 'application/json';
    Response.Content :=
      '{"success": false, "details": '
      + '{'
        + '"error": "Request not found",'
        + '"pathinfo": "' + Request.PathInfo + '"'
      + '}'
    + '}';
  end
  else
    Handled := True;
end;

end.
