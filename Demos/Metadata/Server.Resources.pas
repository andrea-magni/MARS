(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  Classes, SysUtils

, MARS.Core.Registry, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON
, MARS.Core.MessageBodyWriters, MARS.Core.MessageBodyReaders
;

type
  [Path('/items'), Produces(TMediaType.APPLICATION_JSON)]
  TItemsResource = class
  private
  protected
  public
    [GET]
    function GetList: TJSONArray;

    [GET, Path('/{id}')]
    function GetItem([PathParam] id: Integer; [QueryParam] filter: string): TJSONObject;

    [POST]
    procedure AddItem([BodyParam] item: TJSONObject);
  end;

  [Path('/customers'), Produces(TMediaType.APPLICATION_JSON)]
  TCustomersResource = class
  private
  protected
  public
    [GET]
    function GetList: TJSONArray;

    [GET, Path('/{id}')]
    function GetCustomer([PathParam] id: Integer): TJSONObject;
  end;


implementation


{ TItemsResource }

procedure TItemsResource.AddItem(item: TJSONObject);
begin
  // store item
end;

function TItemsResource.GetItem(id: Integer; filter: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.WriteIntegerValue('id', id);
  Result.WriteStringValue('dx', 'This is item n. ' + id.ToString);
  Result.WriteDateTimeValue('time', Now);
end;

function TItemsResource.GetList: TJSONArray;
begin
  Result := TJSONArray.Create;
  Result.Add(1);
  Result.Add(2);
  Result.Add(3);
end;

{ TCustomersResource }

function TCustomersResource.GetCustomer(id: Integer): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.WriteIntegerValue('id', id);
  Result.WriteStringValue('dx', 'This is customer n. ' + id.ToString);
  Result.WriteDateTimeValue('time', Now);
end;

function TCustomersResource.GetList: TJSONArray;
begin
  Result := TJSONArray.Create;
  Result.Add(1);
  Result.Add(2);
  Result.Add(3);
  Result.Add(4);
  Result.Add(5);
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TItemsResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TCustomersResource>;

end.
