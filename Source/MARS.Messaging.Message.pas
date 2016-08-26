(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Messaging.Message;

{$I MARS.inc}

interface

uses
  Classes, SysUtils
  , Rtti
  , MARS.Core.JSON
  ;

type
  TMARSMessage = class
  private
    FCreationDateTime: TDateTime;
  public
    constructor Create(); virtual;
    procedure Assign(ASource: TMARSMessage); virtual;
    function ToJSON: TJSONObject; virtual;

    property CreationDateTime: TDateTime read FCreationDateTime;
  end;

  TMARSCustomMessage = class(TMARSMessage)
  private
  public
    class function Clone<T: TMARSMessage, constructor>(ASource: T): T;
  end;

  TMARSStringMessage = class(TMARSCustomMessage)
  private
    FValue: string;
  public
    constructor Create(const AValue: string); reintroduce;
    procedure Assign(ASource: TMARSMessage); override;
    function ToJSON: TJSONObject; override;

    property Value: string read FValue write FValue;
  end;

  TMARSJSONObjectMessage = class(TMARSCustomMessage)
  private
    FValue: TJSONObject;
    procedure SetValue(const AValue: TJSONObject);
  public
    constructor Create(AValue: TJSONObject); reintroduce;
    destructor Destroy; override;

    procedure Assign(ASource: TMARSMessage); override;
    function ToJSON: TJSONObject; override;

    property Value: TJSONObject read FValue write SetValue;
  end;


implementation

uses
  DateUtils
  , MARS.Core.Utils
  ;

{ TMARSMessage }

procedure TMARSMessage.Assign(ASource: TMARSMessage);
begin
  FCreationDateTime := ASource.FCreationDateTime;
end;

constructor TMARSMessage.Create();
begin
  inherited Create;
  FCreationDateTime := Now;
end;

function TMARSMessage.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('MessageType', ClassName);
  Result.AddPair('CreationDateTime', DateToISO8601(CreationDateTime));
end;

{ TMARSCustomMessage }

class function TMARSCustomMessage.Clone<T>(ASource: T): T;
begin
  Result := T.Create;
  Result.Assign(ASource);
end;


{ TMARSStringMessage }

procedure TMARSStringMessage.Assign(ASource: TMARSMessage);
begin
  inherited;
  if ASource is TMARSStringMessage then
    Self.FValue := TMARSStringMessage(ASource).FValue;
end;

constructor TMARSStringMessage.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

function TMARSStringMessage.ToJSON: TJSONObject;
begin
  Result := inherited ToJSON;
  Result.AddPair('Value', Value);
end;

{ TMARSJSONObjectMessage }

procedure TMARSJSONObjectMessage.Assign(ASource: TMARSMessage);
begin
  inherited;
  if ASource is TMARSJSONObjectMessage then
  begin
    Value := TMARSJSONObjectMessage(ASource).Value;
  end;
end;

constructor TMARSJSONObjectMessage.Create(AValue: TJSONObject);
begin
  inherited Create;
  Value := AValue;
end;

destructor TMARSJSONObjectMessage.Destroy;
begin
  FValue.Free;
  inherited;
end;

procedure TMARSJSONObjectMessage.SetValue(const AValue: TJSONObject);
begin
  if FValue <> AValue then
    FValue := AValue.Clone as TJSONObject;
end;

function TMARSJSONObjectMessage.ToJSON: TJSONObject;
begin
  Result := inherited ToJSON;
  Result.AddPair('Value', Value);
end;

end.
