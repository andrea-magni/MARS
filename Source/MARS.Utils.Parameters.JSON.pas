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

      if LPair.JsonValue is TJSONNumber then
        LValue := GuessTValueFromString(TJSONNumber(LPair.JsonValue).Value)
      else if LPair.JsonValue is TJSONTrue then
        LValue := True
      else if LPair.JsonValue is TJSONFalse then
        LValue := False
      else if LPair.JsonValue is TJSONObject then
      begin
        Load(AParameters, TJSONObject(LPair.JsonValue), LName);
        Continue;
      end
      else if LPair.JsonValue is TJSONArray then
        LValue := '{array}'
      else if LPair.JsonValue is TJSONString then
        LValue := TJSONString(LPair.JsonValue).Value;

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

    case LPair.Value.Kind of
      tkInteger: ADestination.WriteIntegerValue(LPair.Key, LPair.Value.AsInteger);
        tkInt64: ADestination.WriteInt64Value(LPair.Key, LPair.Value.AsInt64);
        tkFloat: ADestination.WriteDoubleValue(LPair.Key, LPair.Value.AsExtended);

      tkChar,
      tkString,
      tkWChar,
      tkLString,
      tkWString,
      tkUString: ADestination.WriteStringValue(LPair.Key, LPair.Value.AsString);

      tkEnumeration: begin
        if LPair.Value.IsType<Boolean> then
          ADestination.WriteBoolValue(LPair.Key, LPair.Value.AsBoolean)
        else
          ADestination.WriteStringValue(LPair.Key, GetEnumName(LPair.Value.TypeInfo, LPair.Value.AsOrdinal));
      end;

//      tkUnknown: ;
//      tkSet: ;

//      tkClass: ;
//      tkMethod: ;
//      tkVariant: ;
//      tkArray: ;
//      tkRecord: ;
//      tkInterface: ;
//      tkDynArray: ;
//      tkClassRef: ;
//      tkPointer: ;
//      tkProcedure: ;
    end;
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
