(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.FireDAC.DataModule;

interface

uses
  System.SysUtils, System.Classes

  , MARS.Core.JSON
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.URL

  , MARS.Data.FireDAC, MARS.Data.FireDAC.Utils
  , MARS.Data.MessageBodyWriters
  , MARS.Data.FireDAC.ReadersAndWriters

  , FireDAC.Comp.Client, FireDAC.Comp.DataSet
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

 [
   Produces(TMediaType.APPLICATION_JSON)
 , Produces(TMediaType.APPLICATION_JSON_FireDAC)
 , Consumes(TMediaType.APPLICATION_JSON)
 , Consumes(TMediaType.APPLICATION_JSON_FireDAC)
 ]
  TMARSFDDataModuleResource = class(TDataModule)
  private
  protected
    [Context] FD: TMARSFireDAC;
    [Context] URL: TMARSURL;

    procedure BeforeApplyUpdates(ADeltas: TArray<TFDMemTable>; ADelta: TFDMemTable;
      ADataSet: TFDDataSet); virtual;
  public
    [GET]
    function Retrieve: TArray<TFDDataSet>; virtual;

    [POST]
    function Update([BodyParam] const ADeltas: TArray<TFDMemTable>): TArray<TMARSFDApplyUpdatesRes>; virtual;
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

procedure TMARSFDDataModuleResource.BeforeApplyUpdates(ADeltas: TArray<TFDMemTable>;
  ADelta: TFDMemTable; ADataSet: TFDDataSet);
begin

end;

function TMARSFDDataModuleResource.Retrieve: TArray<TFDDataSet>;
var
  LIncludeDefault: Boolean;
  LDataSet: TFDDataSet;
  LDataSets: TArray<TFDDataSet>;
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
          and (AField.FieldType.IsObjectOfType(TFDDataSet)) then
        begin
          if (LIncludeDefault or AField.HasAttribute<RESTInclude>)
             and (not AField.HasAttribute<RESTExclude>)
          then
            LDataSets := LDataSets + [AField.GetValue(Self).AsObject as TFDDataSet]
        end;

        Result := True;
      end
  );

  for LDataSet in LDataSets do
  begin
    if LDataSet is TFDAdaptedDataSet then
    begin
      FD.InjectMacroValues(TFDAdaptedDataSet(LDataSet).Command);
      FD.InjectParamValues(TFDAdaptedDataSet(LDataSet).Command);
    end;
  end;

  Result := LDataSets;
end;

function TMARSFDDataModuleResource.Update([BodyParam] const ADeltas: TArray<TFDMemTable>): TArray<TMARSFDApplyUpdatesRes>;
begin
  Result := FD.ApplyUpdates(
    Retrieve, ADeltas
  , procedure (ADataset: TFDDataSet; ADelta: TFDMemTable)
    begin
      BeforeApplyUpdates(ADeltas, ADelta, ADataSet);
    end
  );
end;

{ RESTIncludeDefault }

constructor RESTIncludeDefault.Create(ADefaultValue: Boolean);
begin
  inherited Create;
  FDefaultValue := ADefaultValue;
end;

end.
