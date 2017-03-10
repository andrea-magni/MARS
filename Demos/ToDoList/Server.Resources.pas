(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Core.Response

  , MARS.Core.Token
  , MARS.Core.Token.Resource
//  , MARS.Core.Invocation


  , Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Phys.FB
//  , MARS.Data.MessageBodyWriters
  , MARS.Data.FireDAC
  , Model, Model.Utilities
  ;

type
  [Path('item'), Produces(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_JSON)]
  TItemResource = class
  private
  protected
    [Context] FD: TMARSFireDAC;
  public
    [GET]
    function RetrieveItems: TToDoItems;
    [GET, Path('/{id}')]
    function RetrieveItem([PathParam] id: Integer): TTodoItem;
    [PUT]
    function Update([BodyParam] AItem: TToDoItem): TToDoItem;
    [POST]
    function Store([BodyParam] AItem: TToDoItem): TToDoItem;
    [DELETE, Path('/{id}')]
    procedure Delete([PathParam] id: Integer);
  end;

  [Path('account')]
  TAccountResource = class
  private
  protected
    [Context] FD: TMARSFireDAC;
  public
    [GET, Path('validate')]
    procedure Validate;
    [GET, Path('password-reset')]
    procedure PasswordReset;
    [POST]
    function Store([BodyParam] ANewAccount: TAccount): TAccount;
    [DELETE, Path('/{id}')]
    procedure Delete([PathParam] id: Integer);
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  protected
    [Context] FD: TMARSFireDAC;
    function Authenticate(const AUserName: string;
      const APassword: string): Boolean; override;

  end;

implementation

uses
    MARS.Core.Registry, MARS.Rtti.Utils
  , MARS.Core.Exceptions
//    , Web.ReqMulti, Web.ReqFiles
  ;

{ TItemResource }

procedure TItemResource.Delete(id: Integer);
var
  LCommand: TFDCommand;
begin
  LCommand := FD.CreateCommand('delete from ITEMS where ID = :AID and OWNER_ID = :Token_Claim_ACCOUNT_ID');
  LCommand.ParamByName('AID').AsInteger := id;
  LCommand.Execute;
  if LCommand.RowsAffected < 1 then
    raise EMARSHttpException.CreateFmt('Item %d not found or access denied', [id], 404);
end;

function TItemResource.RetrieveItem([PathParam] id: Integer): TTodoItem;
var
  LQuery: TFDQuery;
begin
  LQuery := FD.CreateQuery('select * from ITEMS where ID = :AID and OWNER_ID = :Token_Claim_ACCOUNT_ID');
  LQuery.ParamByName('AID').AsInteger := id;
  LQuery.Open;

  TRecord<TToDoItem>.FromDataSet(Result, LQuery);
  if Result.Id <= 0 then
    raise EMARSHttpException.CreateFmt('Item %d not found or access denied', [id], 404);
end;

function TItemResource.RetrieveItems: TToDoItems;
begin
  Result := FD.CreateQuery('select * from ITEMS where OWNER_ID = :Token_Claim_ACCOUNT_ID').ToToDoItems;
end;

//function THelloWorldResource.Test2: string;
//var
//  LFile: TWebRequestFile;
//  LFS: TFileStream;
//  LIndex: Integer;
//begin
//
//  Result := AR.Request.Files.Count.ToString;
//  for LIndex := 0 to AR.Request.Files.Count-1 do
//  begin
//    LFile := AR.Request.Files.Items[LIndex] as TWebRequestFile;
//
//    LFS := TFileStream.Create('C:\temp\test_upload\' + LFile.FileName, fmCreate or fmOpenWrite or fmShareDenyWrite);
//    try
//      LFS.CopyFrom(LFile.Stream, 0);
//    finally
//      LFS.Free;
//    end;
//  end;
//end;

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
  LQuery := FD.CreateQuery('select * from ITEMS where ID=:AID and OWNER_ID = :Token_Claim_ACCOUNT_ID');
  LQuery.ParamByName('AID').AsInteger := AItem.Id;
  LQuery.Open;
  if LQuery.RecordCount <> 1 then
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
      ANewAccount.Last_Update := ANewAccount.Creation_Date;
      ANewAccount.Pwd_Hash := TModelUtilities.GetPasswordHash(ANewAccount.Pwd_Hash);
      TRecord<TAccount>.ToDataSet(ANewAccount, LQuery, True);
      TRecord<TAccount>.FromDataSet(LResult, LQuery);
    end
  );

  Result := LResult;
end;

procedure TAccountResource.Delete;
begin

end;

procedure TAccountResource.PasswordReset;
begin

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
   + ' (COALESCE(ACTIVATED, 0) <> 0) '
   + ' and (USERNAME = :pUSERNAME) '
   + ' and (PWD_HASH = :pPWD_HASH)'
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
