unit MARS.Utils.Parameters.JSON;

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
    class function Save(const AParameters: TMARSParameters): TJSONObject;
  end;

  TMARSParametersJSONReaderWriterHelper=class helper for TMARSParameters
  public
    procedure LoadFromJSON(const ASource: TJSONObject);
    function SaveToJSON: TJSONObject;
  end;

implementation

uses
  StrUtils
  , Rtti, TypInfo
  , Generics.Collections
  , MARS.Core.Utils
  ;

{ TMARSParametersJSONReaderWriter }

class procedure TMARSParametersJSONReaderWriter.Load(
  const AParameters: TMARSParameters; const ASource: TJSONObject; const ASliceName: string);
var
  LPair: TJSONPair;
  LValue: TValue;
  LName: string;
begin
  if Assigned(ASource) then
  begin
    for LPair in ASource do
    begin
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
var
  LPair: TPair<string, TValue>;
  LSlice: string;
  LParamName: string;
  LValue: TJSONValue;
begin
  Result := TJSONObject.Create;

  for LPair in AParameters do
  begin
    TMARSParameters.GetSliceAndParamName(LPair.Key, LSlice, LParamName);

    LValue := nil;
    case LPair.Value.Kind of
//      tkUnknown: ;
      tkInteger: LValue := TJSONNumber.Create(LPair.Value.AsInteger);
        tkInt64: LValue := TJSONNumber.Create(LPair.Value.AsInt64);
        tkFloat: LValue := TJSONNumber.Create(LPair.Value.AsExtended);

      tkChar,
      tkString,
      tkWChar,
      tkLString,
      tkWString,
      tkUString: LValue := TJSONString.Create(LPair.Value.AsString);

      tkEnumeration: begin
        if LPair.Value.IsType<Boolean> then
        begin
          if LPair.Value.AsBoolean then
            LValue := TJSONTrue.Create
          else
            LValue := TJSONFalse.Create;
        end
        else
        begin
          LValue := TJSONString.Create( GetEnumName(LPair.Value.TypeInfo, LPair.Value.AsOrdinal) );
        end;
      end;
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

    if Assigned(LValue) then
    begin

      Result.AddPair(LPair.Key, LValue);
    end;
//    else
//      raise Exception.Create('Unsupported value');

//    Result.AddElement(TJSONObject.Create(TJSONPair.Create(LName, AParameters[LName].AsString)));
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

end.
