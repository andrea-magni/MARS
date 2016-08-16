(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit Server.Resources;

interface

uses
  SysUtils, Classes

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Core.Token
  , MARS.Core.Token.Resource
  , MARS.WebServer.Resources

  , Model
  ;

type
  [Path('/item')]
  TItemResource = class
  private
  protected
    [Context] Token: TMARSToken;
  public
    [GET, Path('/{id}'), RolesAllowed('user')]
    function Retrieve([PathParam] id: Integer): TToDoItem;

    [GET, RolesAllowed('user')]
    function RetrieveAll(): TArray<TToDoItem>;

    [POST, RolesAllowed('user')]
    function Add([FormParam('text')] AText: string): TToDoItem;

    [PUT, Path('/{id}'), RolesAllowed('user')]
    function Update([PathParam('id')] AId: Integer; [FormParam('text')] AText: string): TToDoItem;

    [DELETE, Path('/{id}'), RolesAllowed('user')]
    procedure Delete([PathParam('id')] AId: Integer);
  end;

  [Path('/token')]
  TTokenResource = class(TMARSTokenResource)
  private
  protected
    function Authenticate(const AUserName: string; const APassword: string): Boolean; override;
  public
  end;

  [Path('/webapp'), RootFolder('Z:\MARS\Demos\ToDoList\www\todo_angular', True) ]
  TWebAppResource = class(TFileSystemResource)
  end;

implementation

uses
  DB
  , MARS.Core.Registry
  , MARS.Core.Exceptions

  {$if CompilerVersion > 24} //XE3
  , FireDAC.Comp.Client
  , Model.Persistence.FDAC
  {$else}
  , SQLExpr
  , Model.Persistence.DBX
  {$ifend}
  ;

{ TItemResource }

function TItemResource.Add(AText: string): TToDoItem;
var
  LAccessor: TDBAccessor;
begin
  if AText = '' then
    raise EMARSHttpException.Create('Text cannot be empty');

  LAccessor := TDBAccessor.Create;
  try
    Result := TToDoItem.Create;
    try
      Result.Owner := Token.UserName;
      Result.Text := AText;
      LAccessor.New(Result);
    except
      Result.Free;
      Result := nil;
      raise;
    end;
  finally
    LAccessor.Free;
  end;
end;

procedure TItemResource.Delete(AId: Integer);
var
  LAccessor: TDBAccessor;
begin
  LAccessor := TDBAccessor.Create;
  try
    LAccessor.Delete(AId);
  finally
    LAccessor.Free;
  end;
end;

function TItemResource.Retrieve(id: Integer): TToDoItem;
var
  LAccessor: TDBAccessor;
begin
  LAccessor := TDBAccessor.Create;
  try
    Result := LAccessor.Retrieve(id);
    if not Assigned(Result) then
      raise EMARSHttpException.Create(
        Format('Item not found: %d', [id]), 404);
  finally
    LAccessor.Free;
  end;
end;

function TItemResource.RetrieveAll: TArray<TToDoItem>;
var
  LAccessor: TDBAccessor;
  LResult: TArray<TToDoItem>;
begin
  SetLength(LResult, 0);

  LAccessor := TDBAccessor.Create;
  try
    LAccessor.Select(
        'OWNER = :OWNER'
      , procedure(AQuery: {$if CompilerVersion > 24}TFDQuery{$else}TSQLQuery{$ifend})
        begin
          AQuery.ParamByName('OWNER').AsString := Token.UserName;
        end
      , procedure(AQuery: {$if CompilerVersion > 24}TFDQuery{$else}TSQLQuery{$ifend})
        begin
          SetLength(LResult, Length(LResult) + 1);
          LResult[Length(LResult) -1] := TToDoItem.CreateFromRecord(AQuery);
        end
    );
  finally
    LAccessor.Free;
  end;

  Result := LResult;
end;


function TItemResource.Update(AId: Integer; AText: string): TToDoItem;
var
  LAccessor: TDBAccessor;
begin
  LAccessor := TDBAccessor.Create;
  try
    Result := LAccessor.Retrieve(AId);
    try
      Result.Text := AText;
      LAccessor.Update(Result);
    except
      Result.Free;
      Result := nil;
      raise;
    end;
  finally
    LAccessor.Free;
  end;
end;

{ TTokenResource }

function TTokenResource.Authenticate(const AUserName,
  APassword: string): Boolean;
var
  LAccessor: TDBAccessor;
  LRoles: TArray<string>;
  LUserName: string;
begin
  LAccessor := TDBAccessor.Create;
  try
    LUserName := AUserName;
    Result := LAccessor.Authenticate(LUserName, APassword, LRoles);
    Token.SetUserNameAndRoles(LUserName, LRoles);
  finally
    LAccessor.Free;
  end;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TItemResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TWebAppResource>(
    function: TObject
    begin
      Result := TWebAppResource.Create;
    end
  );

end.
