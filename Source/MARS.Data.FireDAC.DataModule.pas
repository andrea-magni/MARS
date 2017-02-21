(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.FireDAC.DataModule;

interface

uses
  System.SysUtils, System.Classes
  , Data.FireDACJSONReflect

  , MARS.Core.JSON
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.URL

  , MARS.Data.FireDAC
  , MARS.Data.MessageBodyWriters
  , MARS.Data.FireDAC.ReadersAndWriters

  , FireDAC.Comp.Client
  ;

type
  RESTIncludeDefault = class(MARSAttribute)
  private
    FDefaultValue: Boolean;
  public
    constructor Create(ADefaultValue: Boolean);
    property DefaultValue: Boolean read FDefaultValue write FDefaultValue;
  end;

  RESTExposeAttribute = class(MARSAttribute);
  RESTInclude = class(RESTExposeAttribute);
  RESTExclude = class(RESTExposeAttribute);

  [Produces(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_JSON)]
  TMARSFDDataModuleResource = class(TDataModule)
  private
    function GetResourceName: string;
  protected
    [Context] URL: TMARSURL;

    procedure BeforeApplyUpdates(ADeltas: TFDJSONDeltas; ADelta: TFDMemTable;
      ADataSet: TFDCustomQuery); virtual;

    procedure ApplyUpdates(ADeltas: TFDJSONDeltas;
      AOnApplyUpdates: TProc<string, Integer, IFDJSONDeltasApplyUpdates> = nil); virtual;
  public
    [GET]
    function Retrieve: TArray<TFDCustomQuery>; virtual;

    [POST]
    function Update([BodyParam] const AJSONDeltas: TJSONObject): TJSONArray; virtual;

  published
    property ResourceName: string read GetResourceName;
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
    Generics.Collections
  , MARS.Rtti.Utils
  , MARS.Core.Exceptions
  , Rtti, TypInfo
  ;

{ TDataModule1 }

procedure TMARSFDDataModuleResource.ApplyUpdates(ADeltas: TFDJSONDeltas;
  AOnApplyUpdates: TProc<string, Integer, IFDJSONDeltasApplyUpdates>);
var
  LApplyUpdates: IFDJSONDeltasApplyUpdates;
  LIndex: Integer;
  LDelta: TPair<string, TFDMemTable>;
  LDataSet: TFDCustomQuery;
  LApplyResult: Integer;
begin
  LApplyUpdates := TFDJSONDeltasApplyUpdates.Create(ADeltas);
  try
    for LIndex := 0 to TFDJSONDeltasReader.GetListCount(ADeltas) - 1 do
    begin
      LDelta := TFDJSONDeltasReader.GetListItem(ADeltas, LIndex);
      LDataSet := Self.FindComponent(LDelta.Key) as TFDCustomQuery;

      BeforeApplyUpdates(ADeltas, LDelta.Value, LDataSet);
      LApplyResult := LApplyUpdates.ApplyUpdates(LDelta.Key, LDataSet.Command);
      if Assigned(AOnApplyUpdates) then
        AOnApplyUpdates(LDataSet.Name, LApplyResult, LApplyUpdates);
    end;
  finally
    LApplyUpdates := nil; // it's an interface
  end;
end;

procedure TMARSFDDataModuleResource.BeforeApplyUpdates(ADeltas: TFDJSONDeltas;
  ADelta: TFDMemTable; ADataSet: TFDCustomQuery);
begin

end;

function TMARSFDDataModuleResource.GetResourceName: string;
var
  LResult: string;
begin
  LResult := '';
  TRttiHelper.IfHasAttribute<PathAttribute>(Self,
    procedure (AAttrib: PathAttribute)
    begin
      LResult := AAttrib.Value;
    end
  );
  Result := LResult;
end;

function TMARSFDDataModuleResource.Retrieve: TArray<TFDCustomQuery>;
var
  LIncludeDefault: Boolean;
  LDataSets: TArray<TFDCustomQuery>;
begin
  // determine default behavior
  LIncludeDefault := True;
  TRttiHelper.IfHasAttribute<RESTIncludeDefault>(Self,
    procedure(AAttrib: RESTIncludeDefault)
    begin
      LIncludeDefault := AAttrib.DefaultValue;
    end
  );

  LDataSets := [];
  TRttiHelper.ForEachField(Self
    , function(AField: TRttiField): Boolean
      begin
        if (AField.Visibility >= TMemberVisibility.mvPublic)
          and (AField.FieldType.IsObjectOfType(TFDCustomQuery)) then
        begin
          if (LIncludeDefault or AField.HasAttribute<RESTInclude>)
             and (not AField.HasAttribute<RESTExclude>)
          then
            LDataSets := LDataSets + [AField.GetValue(Self).AsObject as TFDCustomQuery]
        end;

        Result := True;
      end
  );

  Result := LDataSets;
end;

function TMARSFDDataModuleResource.Update([BodyParam] const AJSONDeltas: TJSONObject): TJSONArray;
var
  LDeltas: TFDJSONDeltas;
  LResult: TJSONArray;
begin
  Result := nil;

  LDeltas := TFDJSONDeltas.Create;
  try
    // build FireDAC delta objects
    if not TFDJSONInterceptor.JSONObjectToDataSets(AJSONDeltas, LDeltas) then
      raise EMARSException.Create('Error de-serializing deltas');

    // apply updates
    LResult := TJSONArray.Create;
    try
      ApplyUpdates(LDeltas,
        procedure(ADatasetName: string; AApplyResult: Integer; AApplyUpdates: IFDJSONDeltasApplyUpdates)
        var
          LResultObj: TJSONObject;
        begin
          LResultObj := TJSONObject.Create;
          try
            LResultObj.AddPair('dataset', ADatasetName);
            LResultObj.AddPair('result', TJSONNumber.Create(AApplyResult));
            LResultObj.AddPair('errors', TJSONNumber.Create(AApplyUpdates.Errors.Count));
            LResultObj.AddPair('errorText', AApplyUpdates.Errors.Strings.Text);
            LResult.AddElement(LResultObj);
          except
            LResultObj.Free;
            raise;
          end;
        end
      );

      Result := LResult;
    except
      LResult.Free;
      raise;
    end;
  finally
    LDeltas.Free;
  end;
end;

{ RESTIncludeDefault }

constructor RESTIncludeDefault.Create(ADefaultValue: Boolean);
begin
  inherited Create;
  FDefaultValue := ADefaultValue;
end;

end.
