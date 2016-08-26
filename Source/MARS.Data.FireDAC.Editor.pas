(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.FireDAC.Editor;

interface

uses
  System.Classes, System.SysUtils
  ,  DesignEditors
  , MARS.Client.CustomResource.Editor
  , MARS.Client.FireDAC;

type
  TMARSFDResourceEditor = class(TMARSClientCustomResourceEditor)
  private
    function CurrentObj: TMARSFDResource;
  protected
    procedure SetDesignTimePosition(AComponent: TComponent; AIndex: Integer = 0);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  Windows
  , VCL.Dialogs
  , DesignIntf

  , FireDAC.Comp.Client
;

procedure Register;
begin
  RegisterComponentEditor(TMARSFDResource, TMARSFDResourceEditor);
end;

{ TMARSFDResourceEditor }

function TMARSFDResourceEditor.CurrentObj: TMARSFDResource;
begin
  Result := Component as TMARSFDResource;
end;

procedure TMARSFDResourceEditor.ExecuteVerb(Index: Integer);
var
  LIndex: Integer;
  LMemTable: TFDMemTable;
  LOwner: TComponent;
  LCreated: Integer;
begin
  inherited;
  LIndex := GetVerbCount - 1;
  if Index = LIndex then
  begin
    LCreated := 0;
    LOwner := CurrentObj.Owner;
    CurrentObj.GET;

    CurrentObj.ResourceDataSets.ForEach(
      procedure(AItem: TMARSFDResourceDatasetsItem)
      begin
        if (not Assigned(AItem.DataSet)) and (AItem.DataSetName <> '') then
        begin
          LMemTable := TFDMemTable.Create(LOwner);
          try
            LMemTable.Name := Designer.UniqueName(AItem.DataSetName);
            AItem.DataSet := LMemTable;

            SetDesignTimePosition(LMemTable, LCreated);
            Inc(LCreated);
          except
            LMemTable.Free;
            raise;
          end;
        end;
      end
    );

    CurrentObj.GET();
  end;

  Designer.Modified;
end;

function TMARSFDResourceEditor.GetVerb(Index: Integer): string;
var
  LIndex: Integer;
begin
  Result := inherited GetVerb(Index);

  LIndex := GetVerbCount - 1;
  if Index = LIndex then
  begin
    Result := 'Create datasets';
  end;
end;

function TMARSFDResourceEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

procedure TMARSFDResourceEditor.SetDesignTimePosition(AComponent: TComponent; AIndex: Integer);
var
  LRec: LongRec;
begin
  LRec := LongRec(CurrentObj.DesignInfo);

  LRec.Hi := LRec.Hi + 48; // top
  LRec.Lo := LRec.Lo + (AIndex * 48); // left
  AComponent.DesignInfo := Integer(LRec);
  Designer.Modified;
end;

end.


