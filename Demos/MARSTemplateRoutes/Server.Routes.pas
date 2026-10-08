(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Routes;

// The endpoints of this project are routes (MARS.Core.Routes): modules of routes
// registered here with MARSRoutes and added to the application in Server.Ignition
// with AddRoutes('Server.Routes.*'). Login (token) and OpenAPI/Swagger come from the
// resources in Server.Resources.Token and Server.Resources.OpenAPI.
// Docs: https://andrea-magni.github.io/MARS/server/routes

interface

implementation

uses
  System.SysUtils
, MARS.Core.Routes, MARS.Core.MediaType
;

initialization
  MARSRoutes('Server.Routes.HelloWorld', 'helloworld',
    procedure (const R: TMARSRouter)
    begin
      R.Produces(TMediaType.TEXT_PLAIN)
       .Summary('Hello World');

      // GET /rest/default/helloworld
      R.Get<string>('',
        function (const C: TMARSRouteContext): string
        begin
          Result := 'Hello World!';
        end
      ).Summary('Say hello');

      // GET /rest/default/helloworld/me (a token is needed: POST /rest/default/token)
      R.Get<string>('me',
        function (const C: TMARSRouteContext): string
        begin
          Result := 'Hello ' + C.Token.UserName + '!';
        end
      ).RolesAllowed('standard')
       .Summary('Say hello to the authenticated user');

      // GET /rest/default/helloworld/Andrea
      R.Get<string>('{name}',
        function (const C: TMARSRouteContext): string
        begin
          Result := 'Hello ' + C.Path<string>('name') + '!';
        end
      ).Summary('Say hello to someone');

(*
      // more examples: a record as JSON, a typed body, query parameters, a middleware

      R.Get<TArray<TCustomer>>('customers',
        function (const C: TMARSRouteContext): TArray<TCustomer>
        begin
          Result := TCustomers.Search(C.Query<string>('name', ''));
        end
      ).Produces(TMediaType.APPLICATION_JSON)
       .QueryParam<string>('name', 'part of the name (optional)');

      R.Post<TCustomer, TCustomer>('customers',
        function (const C: TMARSRouteContext; const ACustomer: TCustomer): TCustomer
        begin
          Result := TCustomers.Add(ACustomer);
          C.Created('customers/' + Result.Id.ToString);
        end
      ).Produces(TMediaType.APPLICATION_JSON)
       .RolesAllowed('admin');

      R.Use(
        procedure (const C: TMARSRouteContext; const ANext: TProc)
        begin
          // before the handler
          ANext();
          // after the handler: C.Response, C.Activation.MethodResult
        end
      );
*)
    end
  );

end.
