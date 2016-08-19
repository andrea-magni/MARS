(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Metadata.JSON;

interface

uses
    MARS.Metadata
  , MARS.Core.JSON
;

type
  TMARSMetadataJSON=class helper for TMARSMetadata
  public
    function ToJSON: TJSONObject; virtual;
  end;

implementation

uses
    System.Rtti, System.TypInfo, Generics.Collections
  , MARS.Rtti.Utils
;

{ TMARSMetadataJSON }

function TMARSMetadataJSON.ToJSON: TJSONObject;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LField: TRttiField;
  LFields: TArray<TRttiField>;
  LProperties: TArray<TRttiProperty>;
  LProperty: TRttiProperty;
  LItem: TMARSMetadata;
  LList: TMARSMetadataList;
  LJSONArray: TJSONArray;
begin
  Result := TJSONObject.Create;
  LType := LContext.GetType(Self.ClassType);

  // fields
  LFields := LType.GetFields;
  for LField in LFields do
  begin
    if LField.Visibility >= TMemberVisibility.mvPublic then
    begin
      if LField.FieldType.IsObjectOfType<TMARSMetadata> then
        Result.AddPair(LField.Name, LField.GetValue(Self).AsType<TMARSMetadata>.ToJSON)
      else if LField.FieldType is TRttiInstanceType then
      begin
        LList := LField.GetValue(Self).AsType<TMARSMetadataList>;
        if Assigned(LList) then
        begin
          LJSONArray := TJSONArray.Create;
          try
            for LItem in LList do
              LJSONArray.Add(LItem.ToJSON);

            Result.AddPair(LField.Name, LJSONArray);
          except
            LJSONArray.Free;
            raise;
          end;
        end;
      end
      else
        TValueToJSONObject(Result, LField.Name, LField.GetValue(Self));
    end;
  end;

  // properties
  LProperties := LType.GetProperties;
  for LProperty in LProperties do
  begin
    if LProperty.Visibility >= TMemberVisibility.mvPublic then
    begin
      if LProperty.PropertyType.IsObjectOfType<TMARSMetadata> then
        Result.AddPair(LProperty.Name, LProperty.GetValue(Self).AsType<TMARSMetadata>.ToJSON)
      else if LProperty.PropertyType is TRttiInstanceType then
      begin
        LList := LProperty.GetValue(Self).AsType<TMARSMetadataList>;
        if Assigned(LList) then
        begin
          LJSONArray := TJSONArray.Create;
          try
            for LItem in LList do
              LJSONArray.Add(LItem.ToJSON);

            Result.AddPair(LProperty.Name, LJSONArray);
          except
            LJSONArray.Free;
            raise;
          end;
        end;
      end
      else
        TValueToJSONObject(Result, LProperty.Name, LProperty.GetValue(Self));
    end;
  end;
end;

end.
