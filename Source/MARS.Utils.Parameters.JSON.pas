unit MARS.Utils.Parameters.JSON;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
  , MARS.Core.JSON
  , MARS.Utils.Parameters;

type
  TMARSParametersJSONReaderWriter=class
  private
  protected
  public
    class procedure Load(const AParameters: TMARSParameters; const ASource: TJSONObject; const ASliceName: string = '');
    class function Save(const AParameters: TMARSParameters): TJSONObject; overload;
    class procedure Save(const AParameters: TMARSParameters; ADestination: TJSONObject); overload;
  end;

  TMARSParametersJSONReaderWriterHelper=class helper for TMARSParameters
  public
    procedure LoadFromJSON(const ASource: TJSONObject);
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
var
  LPair: TJSONPair;
  LValue: TValue;
  LName: string;
{$ifndef DelphiXE6_UP}
  LIndex: Integer;
{$endif}
begin
  if Assigned(ASource) then
  begin
{$ifdef DelphiXE6_UP}
    for LPair in ASource do
    begin
{$else}
    for LIndex := 0 to ASource.Size - 1 do
    begin
      LPair := ASource.Get(LIndex);
{$endif}

      LName := AParameters.CombineSliceAndParamName(ASliceName, LPair.JsonString.Value);

      LValue := ASource.ReadValue(LName, TValue.Empty, DefaultMARSJSONSerializationOptions);

      AParameters.Values[LName] := LValue;
    end;
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

procedure TMARSParametersJSONReaderWriterHelper.SaveToJSON(
  ADestination: TJSONObject);
begin
  TMARSParametersJSONReaderWriter.Save(Self, ADestination);
end;

end.
