(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Routes;

// Route-based endpoints (MARS.Core.Routes): two modules, added to the application
// in Server.Ignition with AddRoutes('Server.Routes.*'). See docs/server/routes.md

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Generics.Collections, System.Generics.Defaults, System.Diagnostics
, MARS.Core.Routes, MARS.Core.MediaType, MARS.Core.Exceptions
;

type
  TPerson = record
    Id: Integer;
    Name: string;
    City: string;
  end;

  // in-memory storage shared by all the requests (thread safe)
  TPeopleStore = class
  private
    class var FLock: TCriticalSection;
    class var FPeople: TDictionary<Integer, TPerson>;
    class var FNextId: Integer;
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;
    class function All(const ANameFilter: string): TArray<TPerson>;
    class function Find(const AId: Integer): TPerson;
    class function Add(const APerson: TPerson): TPerson;
    class function Update(const AId: Integer; const APerson: TPerson): TPerson;
    class procedure Remove(const AId: Integer);
  end;

implementation

{ TPeopleStore }

class constructor TPeopleStore.ClassCreate;
var
  LPerson: TPerson;
begin
  FLock := TCriticalSection.Create;
  FPeople := TDictionary<Integer, TPerson>.Create;
  FNextId := 1;

  LPerson.Name := 'Andrea';
  LPerson.City := 'Piacenza';
  Add(LPerson);
  LPerson.Name := 'Ada';
  LPerson.City := 'London';
  Add(LPerson);
end;

class destructor TPeopleStore.ClassDestroy;
begin
  FPeople.Free;
  FLock.Free;
end;

class function TPeopleStore.All(const ANameFilter: string): TArray<TPerson>;
var
  LPerson: TPerson;
begin
  Result := [];
  FLock.Enter;
  try
    for LPerson in FPeople.Values do
      if (ANameFilter = '') or LPerson.Name.ToLower.Contains(ANameFilter.ToLower) then
        Result := Result + [LPerson];
  finally
    FLock.Leave;
  end;
  TArray.Sort<TPerson>(Result, TComparer<TPerson>.Construct(
    function (const ALeft, ARight: TPerson): Integer
    begin
      Result := ALeft.Id - ARight.Id;
    end));
end;

class function TPeopleStore.Find(const AId: Integer): TPerson;
begin
  FLock.Enter;
  try
    if not FPeople.TryGetValue(AId, Result) then
      raise EMARSHttpException.CreateFmt('Person %d not found', [AId], 404);
  finally
    FLock.Leave;
  end;
end;

class function TPeopleStore.Add(const APerson: TPerson): TPerson;
begin
  FLock.Enter;
  try
    Result := APerson;
    Result.Id := FNextId;
    Inc(FNextId);
    FPeople.Add(Result.Id, Result);
  finally
    FLock.Leave;
  end;
end;

class function TPeopleStore.Update(const AId: Integer; const APerson: TPerson): TPerson;
begin
  FLock.Enter;
  try
    if not FPeople.ContainsKey(AId) then
      raise EMARSHttpException.CreateFmt('Person %d not found', [AId], 404);
    Result := APerson;
    Result.Id := AId;
    FPeople[AId] := Result;
  finally
    FLock.Leave;
  end;
end;

class procedure TPeopleStore.Remove(const AId: Integer);
begin
  FLock.Enter;
  try
    if not FPeople.ContainsKey(AId) then
      raise EMARSHttpException.CreateFmt('Person %d not found', [AId], 404);
    FPeople.Remove(AId);
  finally
    FLock.Leave;
  end;
end;

initialization

  // GET  /rest/default/ping
  // GET  /rest/default/whoami   (token needed)
  MARSRoutes('Server.Routes.Misc', '',
    procedure (const R: TMARSRouter)
    begin
      R.Get<string>('ping',
        function (const C: TMARSRouteContext): string
        begin
          Result := 'pong';
        end
      ).Produces(TMediaType.TEXT_PLAIN)
       .Summary('Liveness check');

      R.Get<string>('whoami',
        function (const C: TMARSRouteContext): string
        begin
          Result := C.Token.UserName;
        end
      ).Produces(TMediaType.TEXT_PLAIN)
       .RolesAllowed('standard')
       .Summary('Name of the authenticated user');
    end
  );

  // /rest/default/people
  MARSRoutes('Server.Routes.People', 'people',
    procedure (const R: TMARSRouter)
    begin
      R.Produces(TMediaType.APPLICATION_JSON)
       .Summary('People (route-based endpoints)');

      // middleware of the module: time spent by every route of the group
      R.Use(
        procedure (const C: TMARSRouteContext; const ANext: TProc)
        var
          LWatch: TStopwatch;
        begin
          LWatch := TStopwatch.StartNew;
          ANext();
          C.Response.SetHeader('Server-Timing', 'app;dur=' + LWatch.ElapsedMilliseconds.ToString);
        end
      );

      // GET /people?name=an
      R.Get<TArray<TPerson>>('',
        function (const C: TMARSRouteContext): TArray<TPerson>
        begin
          Result := TPeopleStore.All(C.Query<string>('name', ''));
        end
      ).Summary('List people')
       .QueryParam<string>('name', 'part of the name (optional)');

      // GET /people/1 ({id:int}: /people/abc does not match)
      R.Get<TPerson>('{id:int}',
        function (const C: TMARSRouteContext): TPerson
        begin
          Result := TPeopleStore.Find(C.Path<Integer>('id'));
        end
      ).Summary('Get a person');

      // POST /people {"Name":"Grace","City":"New York"} -> 201 + Location
      R.Post<TPerson, TPerson>('',
        function (const C: TMARSRouteContext; const APerson: TPerson): TPerson
        begin
          Result := TPeopleStore.Add(APerson);
          C.Created(C.URL.URL.TrimRight(['/']) + '/' + Result.Id.ToString);
        end
      ).RolesAllowed('admin')
       .Summary('Add a person');

      // PUT /people/1 {"Name":"Andrea","City":"Milano"}
      R.Put<TPerson, TPerson>('{id:int}',
        function (const C: TMARSRouteContext; const APerson: TPerson): TPerson
        begin
          Result := TPeopleStore.Update(C.Path<Integer>('id'), APerson);
        end
      ).RolesAllowed('admin')
       .Summary('Update a person');

      // DELETE /people/1 -> 204
      R.Delete('{id:int}',
        procedure (const C: TMARSRouteContext)
        begin
          TPeopleStore.Remove(C.Path<Integer>('id'));
          C.NoContent;
        end
      ).RolesAllowed('admin')
       .Summary('Remove a person');

      // nested group: GET /people/1/greeting
      R.Group('{id:int}/greeting',
        procedure (const G: TMARSRouter)
        begin
          G.Get<string>('',
            function (const C: TMARSRouteContext): string
            begin
              Result := Format('Hello, %s from %s!', [TPeopleStore.Find(C.Path<Integer>('id')).Name
                , TPeopleStore.Find(C.Path<Integer>('id')).City]);
            end
          ).Produces(TMediaType.TEXT_PLAIN)
           .Summary('Greet a person');
        end
      );
    end
  );

end.
