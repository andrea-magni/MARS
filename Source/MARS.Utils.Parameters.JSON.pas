unit MARS.Utils.Parameters.JSON;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
  , MARS.Core.JSON
  , MARS.Utils.Parameters;

type
  TMARSParametersJSONReaderWriter=class
  public
    type
      TCustomLoadFunc = reference to function(const AParameters: TMARSParameters; const ASource: TJSONObject; const ASliceName: string): Boolean;
      TCustomSaveFunc = reference to function(const AParameters: TMARSParameters; const ADestination: TJSONObject): Boolean;
  private
    class var FCustomLoadFunc: TCustomLoadFunc;
    class var FCustomSaveFunc: TCustomSaveFunc;
  protected
  public
    class procedure Load(const AParameters: TMARSParameters; const ASource: TJSONObject; const ASliceName: string = '');
    class function Save(const AParameters: TMARSParameters): TJSONObject; overload;
    class procedure Save(const AParameters: TMARSParameters; ADestination: TJSONObject); overload;

    class property CustomLoadFunc: TCustomLoadFunc read FCustomLoadFunc write FCustomLoadFunc;
    class property CustomSaveFunc: TCustomSaveFunc read FCustomSaveFunc write FCustomSaveFunc;
  end;

  TMARSParametersJSONReaderWriterHelper=class helper for TMARSParameters
  public
    procedure LoadFromJSON(const AJSONString: string); overload;
    procedure LoadFromJSON(const ASource: TJSONObject); overload;
    function SaveToJSON: TJSONObject; overload;
    procedure SaveToJSON(ADestination: TJSONObject); overload;
  end;

implementation

uses
  StrUtils
  , Rtti, TypInfo
  , Generics.Collections
  , MARS.Core.Utils
  {$ifdef DelphiXE7_UP}, System.JSON {$endif}
  ;

{ TMARSParametersJSONReaderWriter }

class procedure TMARSParametersJSONReaderWriter.Load(
  const AParameters: TMARSParameters; const ASource: TJSONObject; const ASliceName: string);
begin
  if not Assigned(ASource) then
    Exit;

  // custom behavior?
  var LDone := False;
  if Assigned(CustomLoadFunc) then
    LDone := CustomLoadFunc(AParameters, ASource, ASliceName);

  if LDone then
    Exit;

  // default behavior
  for var LPair in ASource do
  begin
    var LName := AParameters.CombineSliceAndParamName(ASliceName, LPair.JsonString.Value);
    var LValue := ASource.ReadValue(LName, TValue.Empty, DefaultMARSJSONSerializationOptions);
    AParameters.Values[LName] := LValue;
  end;
end;

class function TMARSParametersJSONReaderWriter.Save(
  const AParameters: TMARSParameters): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Save(AParameters, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class procedure TMARSParametersJSONReaderWriter.Save(
  const AParameters: TMARSParameters; ADestination: TJSONObject);
var
  LPair: TPair<string, TValue>;
  LSlice: string;
  LParamName: string;
begin
  Assert(Assigned(AParameters));
  Assert(Assigned(ADestination));

  // custom behavior?
  var LDone := False;
  if Assigned(CustomSaveFunc) then
    LDone := CustomSaveFunc(AParameters, ADestination);

  if LDone then
    Exit;

  // default behavior
  for LPair in AParameters do
  begin
    TMARSParameters.GetSliceAndParamName(LPair.Key, LSlice, LParamName);

    ADestination.WriteTValue(LPair.Key, LPair.Value)
  end;
end;

{ TMARSParametersJSONReaderWriterHelper }

procedure TMARSParametersJSONReaderWriterHelper.LoadFromJSON(
  const ASource: TJSONObject);
begin
  TMARSParametersJSONReaderWriter.Load(Self, ASource);
end;

function TMARSParametersJSONReaderWriterHelper.SaveToJSON: TJSONObject;
begin
  Result := TMARSParametersJSONReaderWriter.Save(Self);
end;

procedure TMARSParametersJSONReaderWriterHelper.LoadFromJSON(
  const AJSONString: string);
begin
  var LJSONObj := TJSONObject.ParseJSONValue(AJSONString) as TJSONObject;
  try
    LoadFromJSON(LJSONObj);
  finally
    FreeAndNil(LJSONObj);
  end;
end;

procedure TMARSParametersJSONReaderWriterHelper.SaveToJSON(
  ADestination: TJSONObject);
begin
  TMARSParametersJSONReaderWriter.Save(Self, ADestination);
end;

end.
