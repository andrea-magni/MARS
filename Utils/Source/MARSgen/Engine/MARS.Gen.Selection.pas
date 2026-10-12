(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.Selection;

// A part of a document: the operations of some tags, some operations, or (types only) some
// schemas. Applied to the model before the generation: the other operations are removed, and the
// named schemas are the ones the selection uses (transitively), in the order of the document.

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  MARS.Gen.Model;

type
  TGenSelection = record
    // the operations with one of these tags
    Tags: TArray<string>;
    // these operations: operationId, or METHOD /path
    Operations: TArray<string>;
    // types only: these schemas (and the ones they use), no operation
    Schemas: TArray<string>;
    function IsEmpty: Boolean;
    function SelectsOperations: Boolean;
    class function Empty: TGenSelection; static;
    // a, b,c -> [a, b, c]
    class function Split(const AText: string): TArray<string>; static;
  end;

  TGenSelector = class
  private
    class function IndexOf(const AOperations: TArray<TGenOperation>; const AOperation: TGenOperation): Integer; static;
  public
    // the key of an operation in a selection: its operationId, else METHOD /path
    class function OperationKey(const AOperation: TGenOperation): string; static;
    // the named schemas used by AOperations, and by ASchemas, transitively
    class function UsedSchemas(const AApi: TGenApi; const AOperations: TArray<TGenOperation>;
      const ASchemas: TArray<TGenSchema>): TList<TGenSchema>; static;
    // the operation is in the selection (by its key, METHOD /path or one of its tags)
    class function IsSelected(const AOperation: TGenOperation; const ASelection: TGenSelection): Boolean; static;
    // the selection of some operations (or, types only, schemas): empty when they are all of them,
    // the tags when they are exactly the operations of whole tags, else the operations
    class function FromChecked(const AApi: TGenApi; const AOperations: TArray<TGenOperation>;
      const ASchemas: TArray<string>): TGenSelection; static;
    // keeps the selection in AApi (diagnostics for names that match nothing); nothing when empty
    class procedure Apply(const AApi: TGenApi; const ASelection: TGenSelection); static;
  end;

implementation

uses
  System.StrUtils;

{ TGenSelection }

class function TGenSelection.Empty: TGenSelection;
begin
  Result.Tags := [];
  Result.Operations := [];
  Result.Schemas := [];
end;

function TGenSelection.IsEmpty: Boolean;
begin
  Result := (Length(Tags) = 0) and (Length(Operations) = 0) and (Length(Schemas) = 0);
end;

function TGenSelection.SelectsOperations: Boolean;
begin
  Result := (Length(Tags) > 0) or (Length(Operations) > 0);
end;

class function TGenSelection.Split(const AText: string): TArray<string>;
begin
  Result := [];
  for var LItem in AText.Split([',']) do
    if LItem.Trim <> '' then
      Result := Result + [LItem.Trim];
end;

{ TGenSelector }

class function TGenSelector.OperationKey(const AOperation: TGenOperation): string;
begin
  if AOperation.OperationId <> '' then
    Result := AOperation.OperationId
  else
    Result := UpperCase(AOperation.Method) + ' ' + AOperation.Path;
end;

class function TGenSelector.UsedSchemas(const AApi: TGenApi; const AOperations: TArray<TGenOperation>;
  const ASchemas: TArray<TGenSchema>): TList<TGenSchema>;
var
  LVisited: TList<TGenSchema>;

  procedure Visit(const ASchema: TGenSchema);
  begin
    if not Assigned(ASchema) or LVisited.Contains(ASchema) then
      Exit;
    LVisited.Add(ASchema);
    Visit(ASchema.Items);
    Visit(ASchema.AdditionalProperties);
    for var LProperty in ASchema.Properties do
      Visit(LProperty.Schema);
    for var LItem in ASchema.AllOf do
      Visit(LItem);
    for var LItem in ASchema.OneOf do
      Visit(LItem);
    for var LItem in ASchema.AnyOf do
      Visit(LItem);
    if Assigned(ASchema.Discriminator) then
      for var LItem in ASchema.Discriminator.Mapping.Values do
        Visit(LItem);
  end;

begin
  LVisited := TList<TGenSchema>.Create;
  try
    for var LOperation in AOperations do
    begin
      for var LParameter in LOperation.Parameters do
        Visit(LParameter.Schema);
      if Assigned(LOperation.RequestBody) then
        for var LContent in LOperation.RequestBody.Contents do
          Visit(LContent.Schema);
      for var LResponse in LOperation.Responses do
      begin
        for var LContent in LResponse.Contents do
          Visit(LContent.Schema);
        for var LHeader in LResponse.Headers do
          Visit(LHeader.Schema);
      end;
    end;
    for var LSchema in ASchemas do
      Visit(LSchema);

    // the named ones, in the order of the document
    Result := TList<TGenSchema>.Create;
    for var LSchema in AApi.Schemas do
      if LVisited.Contains(LSchema) then
        Result.Add(LSchema);
  finally
    LVisited.Free;
  end;
end;

class function TGenSelector.IsSelected(const AOperation: TGenOperation;
  const ASelection: TGenSelection): Boolean;
begin
  Result := MatchText(OperationKey(AOperation), ASelection.Operations)
    or MatchText(UpperCase(AOperation.Method) + ' ' + AOperation.Path, ASelection.Operations);
  if not Result then
    for var LTag in AOperation.Tags do
      if MatchText(LTag, ASelection.Tags) then
        Exit(True);
end;

class function TGenSelector.FromChecked(const AApi: TGenApi; const AOperations: TArray<TGenOperation>;
  const ASchemas: TArray<string>): TGenSelection;
var
  LTags: TArray<string>;
  LWhole: Boolean;
  LCovered: Integer;
begin
  Result := TGenSelection.Empty;
  if Length(ASchemas) > 0 then
  begin
    Result.Schemas := ASchemas;
    Exit;
  end;
  if Length(AOperations) = AApi.Operations.Count then
    Exit;

  // the tags all of whose operations are checked
  LTags := [];
  for var LOperation in AApi.Operations do
    for var LTag in LOperation.Tags do
      if IndexText(LTag, LTags) < 0 then
      begin
        LWhole := True;
        for var LOther in AApi.Operations do
          if MatchText(LTag, LOther.Tags) and (IndexOf(AOperations, LOther) < 0) then
            LWhole := False;
        if LWhole then
          LTags := LTags + [LTag];
      end;

  // exactly the operations of those tags: the tags
  Result.Tags := LTags;
  LCovered := 0;
  for var LOperation in AApi.Operations do
    if IsSelected(LOperation, Result) then
      Inc(LCovered);
  if (Length(LTags) > 0) and (LCovered = Length(AOperations)) then
    Exit;

  Result.Tags := [];
  for var LOperation in AOperations do
    Result.Operations := Result.Operations + [OperationKey(LOperation)];
end;

class function TGenSelector.IndexOf(const AOperations: TArray<TGenOperation>; const AOperation: TGenOperation): Integer;
begin
  for var LIndex := 0 to High(AOperations) do
    if AOperations[LIndex] = AOperation then
      Exit(LIndex);
  Result := -1;
end;

class procedure TGenSelector.Apply(const AApi: TGenApi; const ASelection: TGenSelection);
var
  LKept: TList<TGenOperation>;
  LRoots: TArray<TGenSchema>;
  LUsed: TList<TGenSchema>;
  LSchema: TGenSchema;
  LMatched: Boolean;
begin
  if ASelection.IsEmpty then
    Exit;

  // names that match nothing: a typo, or a document that changed
  for var LTag in ASelection.Tags do
  begin
    LMatched := False;
    for var LOperation in AApi.Operations do
      if MatchText(LTag, LOperation.Tags) then
        LMatched := True;
    if not LMatched then
      AApi.AddDiagnostic(TGenSeverity.Warning, '', 'selection: tag ' + LTag + ': no operation');
  end;
  for var LName in ASelection.Operations do
  begin
    LMatched := False;
    for var LOperation in AApi.Operations do
      if SameText(LName, OperationKey(LOperation)) or SameText(LName, UpperCase(LOperation.Method) + ' ' + LOperation.Path) then
        LMatched := True;
    if not LMatched then
      AApi.AddDiagnostic(TGenSeverity.Warning, '', 'selection: operation ' + LName + ' not in the document');
  end;
  LRoots := [];
  for var LName in ASelection.Schemas do
  begin
    LSchema := AApi.FindSchema(LName);
    if Assigned(LSchema) then
      LRoots := LRoots + [LSchema]
    else
      AApi.AddDiagnostic(TGenSeverity.Warning, '', 'selection: schema ' + LName + ' not in the document');
  end;

  // the operations: the selected ones (none for a selection of schemas)
  LKept := TList<TGenOperation>.Create;
  try
    if ASelection.SelectsOperations then
      for var LOperation in AApi.Operations do
        if IsSelected(LOperation, ASelection) then
          LKept.Add(LOperation);

    if ASelection.SelectsOperations and (LKept.Count = 0) then
      AApi.AddDiagnostic(TGenSeverity.Error, '', 'selection: no operation of the document selected')
    else if not ASelection.SelectsOperations and (Length(LRoots) = 0) then
      AApi.AddDiagnostic(TGenSeverity.Error, '', 'selection: no schema of the document selected');

    for var LIndex := AApi.Operations.Count - 1 downto 0 do
      if not LKept.Contains(AApi.Operations[LIndex]) then
        AApi.Operations.Delete(LIndex);

    // the named schemas used by the selection
    LUsed := UsedSchemas(AApi, LKept.ToArray, LRoots);
    try
      AApi.Schemas.Clear;
      AApi.Schemas.AddRange(LUsed);
    finally
      LUsed.Free;
    end;
  finally
    LKept.Free;
  end;
end;

end.
