(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.URL, MARS.Core.Activation.Interfaces
//, MARS.Core.Token
, MARS.Core.Token.Resource
, MARS.OpenAPI.v3, MARS.Metadata.Attributes
, MARS.Core.Utils
;

type
  TMyRecord = record
    Field1: string;
    Field2: string;
    Person: string;
    Age: Integer;
    function ToString: string;
  end;


  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context] MARSActivation: IMARSActivation;
    procedure LogToUI(const AContent: string);
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    (*
      You can receive any form definition, no contraints on the fields.
      Request example (HTTP):

POST /rest/default/helloworld/array HTTP/1.1
Host: localhost:8080
Content-Type: application/x-www-form-urlencoded
Content-Length: 54

Field1=Test1&Field2=Test2&Person=Andrea%20Magni&Age=39

    *)
    [POST, Path('array'), Consumes(TMediaType.APPLICATION_FORM_URLENCODED_TYPE)]
    function CollectDataAsParamArray([BodyParam] AForm: TArray<TFormParam>): string;

    (*
      Form definition will be mapped to a Delphi data type (TMyRecord).
      Fields have to match record's fields names.

      Request example (HTTP):

POST /rest/default/helloworld/array HTTP/1.1
Host: localhost:8080
Content-Type: application/x-www-form-urlencoded
Content-Length: 54

Field1=Test1&Field2=Test2&Person=Andrea%20Magni&Age=39

    *)
    [POST, Path('record'), Consumes(TMediaType.APPLICATION_FORM_URLENCODED_TYPE), Produces(TMediaType.APPLICATION_JSON)]
    function CollectDataAsRecord([BodyParam] ARecord: TMyRecord): TMyRecord;


    [ GET
    , Path('/openapi')
    , Produces(TMediaType.APPLICATION_JSON)
    , Produces(TMediaType.APPLICATION_YAML)
    // prevents the method itself being part of the OpenAPI specification
    , MetaVisible(False)]
    function GetOpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
  MARS.Core.Registry
, System.Messaging
, Utils.MessageTypes;

{ THelloWorldResource }

function THelloWorldResource.CollectDataAsParamArray(
  AForm: TArray<TFormParam>): string;
var
  LFormText: string;
begin
  LFormText := '';
  for var LParam in AForm do
  begin
    if LFormText <> '' then
      LFormText := LFormText + sLineBreak;
    LFormText := LFormText + LParam.ToString;
  end;

  Result := LFormText;

  LogToUI(LFormText);
end;

function THelloWorldResource.CollectDataAsRecord(ARecord: TMyRecord): TMyRecord;
begin
  Result := ARecord;

  LogToUI(ARecord.ToString);
end;

function THelloWorldResource.GetOpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;
begin
  Result := AOpenAPI;
end;

procedure THelloWorldResource.LogToUI(const AContent: string);
var
  LContext: string;
begin
  // Beware: MARSActivation won't be available later (i.e. in the Queue anonymous method here below)
  LContext := MARSActivation.Method.Name;

  // Beware: System.Messaging is not thread-safe so we need to get back into main thread before sending message
  TThread.Queue(
    nil
  , procedure
    begin
      TMessageManager.DefaultManager.SendMessage(
        nil
      , TLogMessage.Create(AContent, LContext)
      );
    end
  );
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

{ TMyRecord }

function TMyRecord.ToString: string;
begin
  Result := Format('Field1: %s, Field2: %s, Person: %s, Age: %d', [Field1, Field2, Person, Age]);
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
