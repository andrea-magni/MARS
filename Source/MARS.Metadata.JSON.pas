(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Metadata.JSON;

interface

uses
    System.Rtti
  , MARS.Metadata
  , MARS.Core.JSON
;

type
  TMARSMetadataJSON=class helper for TMARSMetadata
  protected
    procedure ReadItem(const AItem: TJSONObject; const AMetadataClass: TMARSMetadataClass);
    procedure ReadField(const AField: TRttiField; const AJSONObject: TJSONObject); virtual;
    procedure ReadFieldList(const AField: TRttiField; const AJSONArray: TJSONArray); virtual;
    procedure ReadProperty(const AProperty: TRttiProperty; const AJSONObject: TJSONObject); virtual;
    procedure ReadPropertyList(const AProperty: TRttiProperty; const AJSONArray: TJSONArray); virtual;
  public
    function ToJSON: TJSONObject; virtual;
    procedure FromJSON(const AJSONObject: TJSONObject); virtual;
  end;


implementation

uses
    System.TypInfo, Generics.Collections, System.JSON
  , MARS.Rtti.Utils
;

{ TMARSMetadataJSON }

procedure TMARSMetadataJSON.ReadField(const AField: TRttiField;
  const AJSONObject: TJSONObject);
begin

end;

procedure TMARSMetadataJSON.ReadFieldList(const AField: TRttiField;
  const AJSONArray: TJSONArray);
begin

end;

procedure TMARSMetadataJSON.ReadItem(const AItem: TJSONObject; const AMetadataClass: TMARSMetadataClass);
var
  LMetadata: TMARSMetadata;
begin
  LMetadata := AMetadataClass.Create(Self);
  try
    LMetadata.FromJSON(AItem);
  except
    LMetadata.Free;
    raise;
  end;
end;

procedure TMARSMetadataJSON.ReadProperty(const AProperty: TRttiProperty;
  const AJSONObject: TJSONObject);
begin

end;

procedure TMARSMetadataJSON.ReadPropertyList(const AProperty: TRttiProperty;
  const AJSONArray: TJSONArray);
var
  LItem: TJSONValue;
begin
  inherited;
  //AM TODO: ugly (make more general)!
  for LItem in AJSONArray do
  begin
    if AProperty.Name = 'Applications' then
      ReadItem(LItem as TJSONObject, TMARSApplicationMetadata)
    else if AProperty.Name = 'Resources' then
      ReadItem(LItem as TJSONObject, TMARSResourceMetadata)
    else if AProperty.Name = 'Methods' then
      ReadItem(LItem as TJSONObject, TMARSMethodMetadata)
    else if AProperty.Name = 'Parameters' then
      ReadItem(LItem as TJSONObject, TMARSRequestParamMetadata);
  end;
end;

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
        Result.WriteTValue(LField.Name, LField.GetValue(Self));
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
        Result.WriteTValue(LProperty.Name, LProperty.GetValue(Self));
    end;
  end;
end;

procedure TMARSMetadataJSON.FromJSON(const AJSONObject: TJSONObject);
var
  LContext: TRttiContext;
  LType: TRttiType;
  LField: TRttiField;
  LFields: TArray<TRttiField>;
  LProperties: TArray<TRttiProperty>;
  LProperty: TRttiProperty;
  LFieldObject: TJSONObject;
  LFieldArray: TJSONArray;
begin
  LType := LContext.GetType(Self.ClassType);

  // fields
  LFields := LType.GetFields;
  for LField in LFields do
  begin
    if LField.Visibility >= TMemberVisibility.mvPublic then
    begin
      if LField.FieldType.IsObjectOfType<TMARSMetadata> then
      begin
        if AJSONObject.TryGetValue<TJSONObject>(LField.Name, LFieldObject) then
          ReadField(LField, LFieldObject);
      end
      else if LField.FieldType is TRttiInstanceType then
      begin
        if AJSONObject.TryGetValue<TJSONArray>(LField.Name, LFieldArray) then
          ReadFieldList(LField, LFieldArray);
      end
      else
        LField.SetValue(Self, AJSONObject.ReadValue(LField.Name, TValue.Empty, LField.FieldType));
    end;
  end;

  // properties
  LProperties := LType.GetProperties;
  for LProperty in LProperties do
  begin
    if (LProperty.Visibility >= TMemberVisibility.mvPublic) then
    begin
      if LProperty.PropertyType.IsObjectOfType<TMARSMetadata> then
      begin
        if AJSONObject.TryGetValue<TJSONObject>(LProperty.Name, LFieldObject) then
          ReadProperty(LProperty, LFieldObject);
      end
      else if LProperty.PropertyType is TRttiInstanceType then
      begin
        if AJSONObject.TryGetValue<TJSONArray>(LProperty.Name, LFieldArray) then
          ReadPropertyList(LProperty, LFieldArray);
      end
      else if (LProperty.IsWritable) then
        LProperty.SetValue(Self, AJSONObject.ReadValue(LProperty.Name, TValue.Empty, LProperty.PropertyType));
    end;
  end;
end;

end.
