(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Model.Persistence.DBX;

interface

uses
   Classes, SysUtils
   , DBXFirebird, DB, SqlExpr, FMTBcd

   , MARS.Core.Classes

   , Model

   ;

type
  TDBAccessor = class
  private
    FConnection: TSQLConnection;
  protected
    function GetConnection: TSQLConnection;
    function GenerateKey: Integer;
    procedure WithQuery(const ADoSomething: TProc<TSQLQuery>);
    property Connection: TSQLConnection read GetConnection;
  public
    function New(const AItem: TToDoItem): Integer;
    function Retrieve(const AID: Integer): TToDoItem;
    procedure Update(const AValue: TToDoItem);
    procedure Delete(const AID: Integer);

    procedure Select(const AWhereCond: string;
      const ABeforeExecute: TProc<TSQLQuery>; const AOnRecord: TProc<TSQLQuery>);

    function Authenticate(var AUserName: string; const APassword: string;
      var ARoles: string): Boolean;
  end;

implementation

{ TToDoItemPersistor }

function TDBAccessor.New(const AItem: TToDoItem): Integer;
begin
  AItem.ID := GenerateKey;
  WithQuery(
    procedure (AQuery: TSQLQuery)
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
  var ARoles: string): Boolean;
var
  LFound: Boolean;
  LRoles, LUserName: string;
begin
  LFound := False;
  LUserName := AUserName;
  LRoles := '';
  WithQuery(
    procedure (AQuery: TSQLQuery)
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
          LRoles := 'admin'
        else
          LRoles := 'user';
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
    procedure(AQuery: TSQLQuery)
    begin
      AQuery.SQL.Text := 'DELETE FROM ITEMS WHERE ID = :ID';
      AQuery.ParamByName('ID').AsInteger := AID;
      AQuery.ExecSQL();
    end
  );
end;

function TDBAccessor.GenerateKey: Integer;
var
  LNewID: Integer;
begin
  WithQuery(
    procedure(AQuery: TSQLQuery)
    begin
      AQuery.SQL.Text := 'select gen_ID(GEN_ITEMS, 1) NEWID from RDB$DATABASE';
      AQuery.Open;
      LNewID := AQuery.Fields[0].AsInteger;
    end
  );

  Result := LNewID;
end;

function TDBAccessor.GetConnection: TSQLConnection;
begin
  if not Assigned(FConnection) then
  begin
    FConnection := TSQLConnection.Create(nil);

    FConnection.DriverName := 'Firebird';
    FConnection.GetDriverFunc := 'getSQLDriverINTERBASE';
    FConnection.LibraryName := 'dbxfb.dll';
    FConnection.LoginPrompt := False;
    FConnection.Params.Clear;

    FConnection.Params.Add('DriverUnit=DBXFirebird');
    FConnection.Params.Add('DriverPackageLoader=TDBXDynalinkDriverLoader,DbxCommonDriver150.bpl');
    FConnection.Params.Add('DriverAssemblyLoader=Borland.Data.TDBXDynalinkDriverLoader,Borla' +
      'nd.Data.DbxCommonDriver,Version=15.0.0.0,Culture=neutral,PublicK' +
      'eyToken=91d62ebb5b0d1b1b');
    FConnection.Params.Add('MetaDataPackageLoader=TDBXFirebirdMetaDataCommandFactory,DbxFirebirdDriver150.bpl');
    FConnection.Params.Add('MetaDataAssemblyLoader=Borland.Data.TDBXFirebirdMetaDataCommandF' +
      'actory,Borland.Data.DbxFirebirdDriver,Version=15.0.0.0,Culture=n' +
      'eutral,PublicKeyToken=91d62ebb5b0d1b1b');
    FConnection.Params.Add('GetDriverFunc=getSQLDriverINTERBASE');
    FConnection.Params.Add('LibraryName=dbxfb.dll');
    FConnection.Params.Add('VendorLib=fbclient.DLL');
    FConnection.Params.Add('BlobSize=-1');
    FConnection.Params.Add('CommitRetain=False');
    FConnection.Params.Add('Database=localhost:C:\Sviluppo\MARS_TODOLIST.FDB');
    FConnection.Params.Add('ErrorResourceFile=');
    FConnection.Params.Add('LocaleCode=0000');
    FConnection.Params.Add('Password=masterkey');
    FConnection.Params.Add('RoleName=RoleName');
    FConnection.Params.Add('ServerCharSet=');
    FConnection.Params.Add('SQLDialect=3');
    FConnection.Params.Add('IsolationLevel=ReadCommitted');
    FConnection.Params.Add('User_Name=sysdba');
    FConnection.Params.Add('WaitOnLocks=True');
    FConnection.Params.Add('Trim Char=False');

    FConnection.VendorLib := 'fbclient.DLL';

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
    procedure (AQuery: TSQLQuery)
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
  const ABeforeExecute: TProc<TSQLQuery>; const AOnRecord: TProc<TSQLQuery>);
begin
  WithQuery(
    procedure (AQuery: TSQLQuery)
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
    procedure (AQuery: TSQLQuery)
    begin
      AQuery.SQL.Text := 'UPDATE ITEMS SET TEXT = :TEXT, OWNER = :OWNER where ID = :ID';
      AQuery.ParamByName('ID').AsInteger := AVAlue.ID;
      AQuery.ParamByName('TEXT').AsString := AVAlue.Text;
      AQuery.ParamByName('OWNER').AsString := AVAlue.Owner;
      AQuery.ExecSQL;
    end
  );
end;


procedure TDBAccessor.WithQuery(const ADoSomething: TProc<TSQLQuery>);
var
  FQuery: TSQLQuery;
begin
  FQuery := TSQLQuery.Create(nil);
  try
    FQuery.SQLConnection := Connection;
    ADoSomething(FQuery);
  finally
    FQuery.Free;
  end;
end;

end.
