(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Model.Persistence.FDAC;

interface

uses
   Classes, SysUtils
   , DB
   , FireDAC.Comp.Client, FireDAC.Stan.Param

   , MARS.Core.Classes

   , Model
   ;

type
  TDBAccessor = class
  private
    FConnection: TFDConnection;
  protected
    function GetConnection: TFDConnection;
    function GenerateKey: Integer;
    procedure WithQuery(const ADoSomething: TProc<TFDQuery>);
    property Connection: TFDConnection read GetConnection;

    const CONNECTION_NAME = 'MARS_TODO_LIST';
  public
    function New(const AItem: TToDoItem): Integer;
    function Retrieve(const AID: Integer): TToDoItem;
    procedure Update(const AValue: TToDoItem);
    procedure Delete(const AID: Integer);

    procedure Select(const AWhereCond: string;
      const ABeforeExecute: TProc<TFDQuery>; const AOnRecord: TProc<TFDQuery>);

    function Authenticate(var AUserName: string; const APassword: string;
      out ARoles: TArray<string>): Boolean;

    destructor Destroy; override;
  end;

implementation

{ TToDoItemPersistor }

function TDBAccessor.New(const AItem: TToDoItem): Integer;
begin
  AItem.ID := GenerateKey;
  WithQuery(
    procedure (AQuery: TFDQuery)
    begin
      AQuery.SQL.Text := 'INSERT INTO ITEMS (ID, OWNER, TEXT, CREATION_DATE) VALUES (:ID, :OWNER, :TEXT, :CREATION_DATE)';
      AQuery.ParamByName('ID').AsInteger := AItem.ID;
      AQuery.ParamByName('OWNER').AsString := AItem.Owner;
      AQuery.ParamByName('TEXT').AsString := AItem.Text;
      AQuery.ParamByName('CREATION_DATE').AsDateTime := Now;

      AQuery.ExecSQL;
    end
  );

  Result := AItem.ID;
end;

function TDBAccessor.Authenticate(var AUserName: string; const APassword: string;
  out ARoles: TArray<string>): Boolean;
var
  LFound: Boolean;
  LUserName: string;
  LRoles: TArray<string>;
begin
  LFound := False;
  LUserName := AUserName;
  SetLength(LRoles, 0);
  WithQuery(
    procedure (AQuery: TFDQuery)
    begin
      AQuery.SQL.Text := 'select * from ACCOUNT where UPPER(USERNAME) = UPPER(:USERNAME) and PWD = :PWD';
      AQuery.ParamByName('USERNAME').AsString := LUserName;
      AQuery.ParamByName('PWD').AsString := APassword;
      AQuery.Open;

      if not AQuery.IsEmpty then
      begin
        LFound := True;
        LUserName := AQuery.FieldByName('USERNAME').AsString;
        if AQuery.FieldByName('IS_ADMIN').AsInteger > 0 then
          LRoles := TArray<string>.Create('user', 'admin')
        else
          LRoles := TArray<string>.Create('user');
      end;

    end
  );

  Result := LFound;
  AUserName := LUserName;
  ARoles := LRoles;
end;

procedure TDBAccessor.Delete(const AID: Integer);
begin
  WithQuery(
    procedure(AQuery: TFDQuery)
    begin
      AQuery.SQL.Text := 'DELETE FROM ITEMS WHERE ID = :ID';
      AQuery.ParamByName('ID').AsInteger := AID;
      AQuery.ExecSQL();
    end
  );
end;

destructor TDBAccessor.Destroy;
begin
  FreeAndNil(FConnection);
  inherited;
end;

function TDBAccessor.GenerateKey: Integer;
var
  LNewID: Integer;
begin
  WithQuery(
    procedure(AQuery: TFDQuery)
    begin
      AQuery.SQL.Text := 'select gen_ID(GEN_ITEMS, 1) NEWID from RDB$DATABASE';
      AQuery.Open;
      LNewID := AQuery.Fields[0].AsInteger;
    end
  );

  Result := LNewID;
end;

function TDBAccessor.GetConnection: TFDConnection;
begin
  if not Assigned(FConnection) then
  begin
    FConnection := TFDConnection.Create(nil);
    FConnection.ConnectionDefName := CONNECTION_NAME;
    FConnection.Connected := True;
  end;
  Result := FConnection;
end;

function TDBAccessor.Retrieve(const AID: Integer): TToDoItem;
var
  LItem: TToDoItem;
begin
  LItem := nil;

  WithQuery(
    procedure (AQuery: TFDQuery)
    begin
      AQuery.SQL.Text := 'select * from ITEMS where ID = :ID';
      AQuery.ParamByName('ID').AsInteger := AID;
      AQuery.Open;

      if not AQuery.IsEmpty then
        LItem := TToDoItem.CreateFromRecord(AQuery);
    end
  );

  Result := LItem;
end;

procedure TDBAccessor.Select(const AWhereCond: string;
  const ABeforeExecute: TProc<TFDQuery>; const AOnRecord: TProc<TFDQuery>);
begin
  WithQuery(
    procedure (AQuery: TFDQuery)
    begin
      AQuery.SQL.Text := 'select * from ITEMS';
      if AWhereCond <> '' then
        AQuery.SQL.Text := AQuery.SQL.Text + ' where ' + AWhereCond;

      if Assigned(ABeforeExecute) then
        ABeforeExecute(AQuery);
      AQuery.Open;

      AQuery.First;
      while not AQuery.Eof do
      try
        if Assigned(AOnRecord) then
          AOnRecord(AQuery);
      finally
        AQuery.Next;
      end;
    end
  );
end;

procedure TDBAccessor.Update(const AValue: TToDoItem);
begin
  WithQuery(
    procedure (AQuery: TFDQuery)
    begin
      AQuery.SQL.Text := 'UPDATE ITEMS SET TEXT = :TEXT, OWNER = :OWNER where ID = :ID';
      AQuery.ParamByName('ID').AsInteger := AVAlue.ID;
      AQuery.ParamByName('TEXT').AsString := AVAlue.Text;
      AQuery.ParamByName('OWNER').AsString := AVAlue.Owner;
      AQuery.ExecSQL;
    end
  );
end;


procedure TDBAccessor.WithQuery(const ADoSomething: TProc<TFDQuery>);
var
  FQuery: TFDQuery;
begin
  FQuery := TFDQuery.Create(nil);
  try
    FQuery.Connection := Connection;
    ADoSomething(FQuery);
  finally
    FQuery.Free;
  end;
end;

end.
