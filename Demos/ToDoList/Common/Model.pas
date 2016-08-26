(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Model;

interface

uses
    Classes, SysUtils
  , DB
  , MARS.Rtti.Utils
  , MARS.Core.JSON
  ;

type
  TToDoItem = class
  private
    FID: Integer;
    FText: string;
    FOwner: string;
  protected
  public
    function ToJson: TJSONObject;
    function ToXML: string;
    procedure CopyFromDataSet(ADataSet: TDataSet);
    constructor CreateFromRecord(const ADataSet: TDataSet);

    property ID: Integer read FID write FID;
    property Owner: string read FOwner write FOwner;
    property Text: string read FText write FText;
  end;

implementation

{ TToDoItem }

procedure TToDoItem.CopyFromDataSet(ADataSet: TDataSet);
begin
  ID := ADataSet.FieldByName('ID').AsInteger;
  Text := ADataSet.FieldByName('TEXT').AsString;
  Owner := ADataSet.FieldByName('OWNER').AsString;
end;

constructor TToDoItem.CreateFromRecord(const ADataSet: TDataSet);
begin
  inherited Create;
  CopyFromDataSet(ADataSet);
end;

function TToDoItem.ToJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('ID', TJSONNumber.Create(FID));
  Result.AddPair('Owner', Owner);
  Result.AddPair('Text', Text);
end;

function TToDoItem.ToXML: string;
begin
  Result := Format('<ITEM><ID>%d</ID><OWNER>%s</OWNER><TEXT>%s</TEXT></ITEM>', [ID, Owner, Text]);
end;

end.
