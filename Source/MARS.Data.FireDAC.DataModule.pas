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
  protected
    [Context] FD: TMARSFireDAC;
    [Context] URL: TMARSURL;

    procedure BeforeApplyUpdates(ADeltas: TFDJSONDeltas; ADelta: TFDMemTable;
      ADataSet: TFDCustomQuery); virtual;
  public
    [GET]
    function Retrieve: TArray<TFDCustomQuery>; virtual;

    [POST]
    function Update([BodyParam] const AJSONDeltas: TFDJSONDeltas): TJSONArray; virtual;
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

procedure TMARSFDDataModuleResource.BeforeApplyUpdates(ADeltas: TFDJSONDeltas;
  ADelta: TFDMemTable; ADataSet: TFDCustomQuery);
begin

end;

function TMARSFDDataModuleResource.Retrieve: TArray<TFDCustomQuery>;
var
  LIncludeDefault: Boolean;
  LDataSet: TFDCustomQuery;
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

  for LDataSet in LDataSets do
  begin
    FD.InjectMacroValues(LDataSet.Command);
    FD.InjectParamValues(LDataSet.Command);
  end;

  Result := LDataSets;
end;

function TMARSFDDataModuleResource.Update([BodyParam] const AJSONDeltas: TFDJSONDeltas): TJSONArray;
var
  LResult: TJSONArray;
begin
  // apply updates
  LResult := TJSONArray.Create;
  try
    FD.ApplyUpdates(
      Retrieve
    , AJSONDeltas
    , procedure(ADataset: TFDCustomQuery; AApplyResult: Integer; AApplyUpdates: IFDJSONDeltasApplyUpdates)
      var
        LResultObj: TJSONObject;
      begin
        LResultObj := TJSONObject.Create;
        try
          LResultObj.AddPair('dataset', ADataset.Name);
          LResultObj.AddPair('result', TJSONNumber.Create(AApplyResult));
          LResultObj.AddPair('errors', TJSONNumber.Create(AApplyUpdates.Errors.Count));
          LResultObj.AddPair('errorText', AApplyUpdates.Errors.Strings.Text);
          LResult.AddElement(LResultObj);
        except
          LResultObj.Free;
          raise;
        end;
      end
    , procedure (ADataset: TFDCustomQuery; ADelta: TFDMemTable)
      begin
        BeforeApplyUpdates(AJSONDeltas, ADelta, ADataSet);
      end
    );

    Result := LResult;
  except
    LResult.Free;
    raise;
  end;
end;

{ RESTIncludeDefault }

constructor RESTIncludeDefault.Create(ADefaultValue: Boolean);
begin
  inherited Create;
  FDefaultValue := ADefaultValue;
end;

end.
