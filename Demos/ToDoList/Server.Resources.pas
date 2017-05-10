(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes, Rtti

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Core.Response

  , MARS.Core.Token
  , MARS.Core.Token.Resource

  , Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Phys.FB
  , MARS.Data.FireDAC
  , Model, Model.Utilities
;

type

  [
      Path('item')
    , Produces(TMediaType.APPLICATION_JSON)
    , Consumes(TMediaType.APPLICATION_JSON)
  ]
  TItemResource = class
  private
  protected
    [Context] FD: TMARSFireDAC;
  public
    [GET]
    function RetrieveItems: TArray<TToDoItem>;

    [GET, Path('/{id}')]
    function RetrieveItem([PathParam] id: Integer): TTodoItem;

    [PUT]
    function Update([BodyParam] AItem: TToDoItem): TToDoItem;

    [POST]
    function Store([BodyParam] AItem: TToDoItem): TToDoItem;

    [DELETE, Path('/{id}')]
    procedure Delete([PathParam] id: Integer);
  end;

  [
     Path('account')
   , Produces(TMediaType.APPLICATION_JSON)
   , Consumes(TMediaType.APPLICATION_JSON)
  ]
  TAccountResource = class
  private
  protected
    [Context] FD: TMARSFireDAC;
  public
    [GET, Path('validate')]
    procedure Validate;

    [GET, Path('available/{username}')]
    function IsAvailable([PathParam] const username: string): Boolean;

    [GET, Path('password-reset')]
    procedure PasswordReset;

    [GET]
    function RetrieveItem(): TAccount;

    [POST]
    function Store([BodyParam] ANewAccount: TAccount): TAccount;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  protected
    [Context] FD: TMARSFireDAC;
    function Authenticate(const AUserName: string; const APassword: string): Boolean; override;
  end;


implementation

uses
    StrUtils
  , MARS.Core.Registry, MARS.Rtti.Utils
  , MARS.Core.Exceptions
;

{ TItemResource }

procedure TItemResource.Delete(id: Integer);
var
  LCommand: TFDCommand;
begin
  LCommand := FD.CreateCommand('delete from ITEMS where ID = :PathParam_id and OWNER_ID = :Token_Claim_ACCOUNT_ID');
  LCommand.Execute;
  if LCommand.RowsAffected < 1 then
    raise EMARSHttpException.CreateFmt('Item %d not found or access denied', [id], 404);
end;

function TItemResource.RetrieveItem([PathParam] id: Integer): TTodoItem;
var
  LQuery: TFDQuery;
begin
  LQuery := FD.CreateQuery('select * from ITEMS where ID = :PathParam_id and OWNER_ID = :Token_Claim_ACCOUNT_ID');
  LQuery.Open;
  if LQuery.IsEmpty then
    raise EMARSHttpException.CreateFmt('Item %d not found or access denied', [id], 404);

  TRecord<TToDoItem>.FromDataSet(Result, LQuery);
end;

function TItemResource.RetrieveItems: TArray<TToDoItem>;
begin
  Result := TRecord<TTodoItem>.DataSetToArray(
    FD.CreateQuery('select * from ITEMS where OWNER_ID = :Token_Claim_ACCOUNT_ID')
  );
end;

function TItemResource.Store(AItem: TToDoItem): TToDoItem;
var
  LQuery: TFDQuery;
begin
  LQuery := FD.CreateQuery('select * from ITEMS where 1=0');
  LQuery.UpdateOptions.AutoIncFields := 'ID';
  LQuery.Open;

  AItem.Creation_Date := Now;
  AItem.Last_Update := AItem.Creation_Date;

  TRecord<TToDoItem>.ToDataSet(AItem, LQuery, True);
  TRecord<TToDoItem>.FromDataSet(Result, LQuery);
end;

function TItemResource.Update(AItem: TToDoItem): TToDoItem;
var
  LQuery: TFDQuery;
begin
  LQuery := FD.CreateQuery('select * from ITEMS where ID=:PathParam_id and OWNER_ID = :Token_Claim_ACCOUNT_ID');
  LQuery.Open;
  if LQuery.IsEmpty then
    raise EMARSHttpException.CreateFmt('Item %d not found or access denied', [AItem.Id], 404);

  TRecord<TToDoItem>.FromDataSet(Result, LQuery);

  Result.Text := AItem.Text;
  Result.Done_Date := AItem.Done_Date;
  Result.Due_Date := AItem.Due_Date;
  Result.Last_Update := Now;

  TRecord<TToDoItem>.ToDataSet(Result, LQuery);
  TRecord<TToDoItem>.FromDataSet(Result, LQuery);
end;

{ TAccountResource }

function TAccountResource.Store([BodyParam] ANewAccount: TAccount): TAccount;
var
  LResult: TAccount;
begin
  FD.InTransaction(
    procedure (ATransaction: TFDTransaction)
    var
      LQuery: TFDQuery;
    begin
      LQuery := FD.CreateQuery('select * from ACCOUNT where 1=0', ATransaction);
      LQuery.UpdateOptions.AutoIncFields := 'ID';
      LQuery.Open;

      ANewAccount.Creation_Date := Now;
      ANewAccount.Active := False;
      ANewAccount.Last_Update := ANewAccount.Creation_Date;
      ANewAccount.Pwd_Hash := TModelUtilities.GetPasswordHash(ANewAccount.Pwd_Hash);
      ANewAccount.Reset_Code := TModelUtilities.GenerateUniqueCode;

      TRecord<TAccount>.ToDataSet(ANewAccount, LQuery, True);
      TRecord<TAccount>.FromDataSet(LResult, LQuery);
    end
  );

  Result := LResult;
end;

function TAccountResource.IsAvailable(const username: string): Boolean;
begin
  Result := FD.Query('select ID from ACCOUNT where USERNAME = :PathParam_username').IsEmpty;
end;

procedure TAccountResource.PasswordReset;
begin

end;

function TAccountResource.RetrieveItem(): TAccount;
var
  LAccount: TAccount;
begin
  FD.Query('select * from ACCOUNT where ID = :Token_Claim_ACCOUNT_ID', nil, nil,
    procedure (AQuery: TFDQuery)
    begin
      if AQuery.IsEmpty then
        raise EMARSHttpException.Create('Account not found or access denied', 404);
      TRecord<TAccount>.FromDataSet(LAccount, AQuery);
    end
  );
  Result := LAccount;
end;

procedure TAccountResource.Validate;
begin

end;

{ TTokenResource }

function TTokenResource.Authenticate(const AUserName,
  APassword: string): Boolean;
var
  LFound: Boolean;
begin
  FD.Query(
     'select ID, USERNAME, FIRST_NAME, LAST_NAME, ROLES from ACCOUNT '
   + 'where '
   + ' (COALESCE(ACTIVE, 0) <> 0) '
   + ' and (USERNAME = :pUSERNAME) and (PWD_HASH = :pPWD_HASH)'
  , nil
  , procedure (AQuery: TFDQuery)
    begin
      AQuery.ParamByName('pUSERNAME').AsString := AUserName;
      AQuery.ParamByName('pPWD_HASH').AsString := TModelUtilities.GetPasswordHash(APassword);
    end
  , procedure (AQuery: TFDQuery)
    begin
      LFound := AQuery.RecordCount = 1;
      if LFound then
      begin
        Token.SetUserNameAndRoles(AQuery.FieldByName('USERNAME').AsString, AQuery.FieldByName('ROLES').AsString.Split([',', ';']) );
        Token.Claims.Values['ACCOUNT_ID'] := AQuery.FieldByName('ID').AsInteger;
        Token.Claims.Values['FIRST_NAME'] := AQuery.FieldByName('FIRST_NAME').AsString;
        Token.Claims.Values['LAST_NAME'] := AQuery.FieldByName('LAST_NAME').AsString;
      end;
    end
  );

  Result := LFound;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TItemResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TAccountResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
