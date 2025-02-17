unit MARS.Report.FastReport;

interface

{$I MARS.inc}

uses
  System.Classes, System.SysUtils, Generics.Collections, Rtti, Data.DB

// *** BEWARE ***
// if your Delphi edition/license does not include FastReport,
// remove the MARS_FASTREPORT definition in the MARS.inc file!
, frxClass, frxExportBaseDialog, frxExportPDF, frxDBSet

, MARS.Core.Application.Interfaces
, MARS.Core.Attributes
, MARS.Core.Classes
, MARS.Core.Declarations
, MARS.Core.Exceptions
, MARS.Core.JSON
, MARS.Core.MediaType
, MARS.Core.Registry
, MARS.Core.Token
, MARS.Core.URL
, MARS.Utils.Parameters
, MARS.Core.Activation.Interfaces
;

type
  EMARSFastReportException = class(EMARSApplicationException);

  MARSFastReportAttribute = class(TCustomAttribute);

  ReportAttribute = class(MARSFastReportAttribute)
  private
    FReportDefName: string;
    FExpandMacros: Boolean;
  public
    constructor Create(AReportDefName: string; const AExpandMacros: Boolean = False);
    property ReportDefName: string read FReportDefName;
    property ExpandMacros: Boolean read FExpandMacros;
  end;

  TfrxReportClass = class of TfrxReport;
  TAfterCreateReportProc = reference to procedure(const AReport: TfrxReport; const AActivation: IMARSActivation);

  TReportDef = record
    Name: string;
    FileName: string;

    // DoublePass
    // ConvertNulls
    // IgnoreDevByZero
    // IgnoreExprError
    // MaxMemSize
    // PrintIfEmpty
    // Author
    // Compressed
    // Name
    // Password
  end;

  TReportDefs = TArray<TReportDef>;

  TReportDefDictionary = TDictionary<string, TReportDef>;
  TReportOnGetValue = reference to procedure (const AReport: TfrxReport; const VarName: string; var Value: Variant);

  TMARSFastReport = class
  private
    FReportDef: TReportDef;
    FReportDefName: string;
    FActivation: IMARSActivation;
    FReport: TfrxReport;
    FPDFExport: TfrxPDFExport;
    FReportOnGetValue: TReportOnGetValue;

    // CLASS FUNCTIONS AND DATA
    class var FAfterCreateReport: TAfterCreateReportProc;
    class var FCustomReportClass: TfrxReportClass;
    class var FReportDefDictionary: TReportDefDictionary;
  protected
    procedure SetReportDefName(const Value: string);
    function GetReport: TfrxReport;
    function GetPDFExport: TfrxPDFExport;
    function GetReportDef: TReportDef;

    procedure DoReportBeforePrint(Sender: TfrxReportComponent);
    procedure DoReportAfterPrint(Sender: TfrxReportComponent);

    procedure DoReportBeginDoc(Sender: TObject);
    procedure DoReportEndDoc(Sender: TObject);

    procedure DoReportGetValue(const VarName: string; var Value: Variant);

    procedure DoReportPrintPage(Page: TfrxReportPage; CopyNo: Integer);
    procedure DoReportPrintReport(Sender: TObject);
  public
    const MACRO_DELIMITER = '_';

    constructor Create(const AReportDefName: string; const AActivation: IMARSActivation = nil); virtual;
    destructor Destroy; override;

    function AddDataSet(const ADataSet: TDataSet; const ADataSetName: string = ''): TfrxDBDataset;
    procedure SetDataSetForBand(const ADataSet: TfrxDBDataset; const ABandName: string);

    procedure SetMasterDataSet(const ADataSet: TDataSet;
      const ADataSetName: string = ''; const AMasterDataName: string = 'MasterData1');
    function ExportToPDF(): TBytes;

    property Report: TfrxReport read GetReport;
    property ReportDefName: string read FReportDefName write SetReportDefName;
    property ReportDef: TReportDef read GetReportDef;

    property ReportOnGetValue: TReportOnGetValue read FReportOnGetValue write FReportOnGetValue;


    property PDFExport: TfrxPDFExport read GetPDFExport;
    property Activation: IMARSActivation read FActivation;

    // CLASS FUNCTIONS AND DATA
    class constructor ClassCreate;
    class destructor ClassDestroy;

    class function GetContextValue(const AName: string; const AActivation: IMARSActivation;
      const ADesiredType: TFieldType = ftUnknown): TValue; virtual;

    class function GetReportClass: TfrxReportClass;

    class function CreateReportByDefName(const AReportDefName: string;
      const AActivation: IMARSActivation = nil): TfrxReport;

    class function LoadReportDefs(const AParameters: TMARSParameters;
      const ASliceName: string = ''): TArray<string>;

    class function GetReportDefByName(const AReportDefName: string): TReportDef;

    class property AfterCreateReport: TAfterCreateReportProc read FAfterCreateReport write FAfterCreateReport;
    class property CustomReportClass: TfrxReportClass read FCustomReportClass write FCustomReportClass;
    class property ReportDefDictionary: TReportDefDictionary read FReportDefDictionary;
  end;

implementation

uses
  StrUtils, Variants, IOUtils
, MARS.Core.Utils
, MARS.Core.Exceptions
, MARS.Data.Utils
, MARS.Rtti.Utils
, MARS.Report.FastReport.InjectionService
//  , MARS.Report.FastReport.ReadersAndWriters
;


{ TMARSFastReport }

procedure TMARSFastReport.SetDataSetForBand(const ADataSet: TfrxDBDataset;
  const ABandName: string);
var
  LDataBand: TfrxDataBand;
begin
  if not Assigned(ADataSet) then
    raise EMARSFastReportException.CreateFmt('[SetDataSetForBand] DataSet not assigned. Band: %s', [ABandName]);

  LDataBand := Report.FindObject(ABandName) as TfrxDataBand;
  if not Assigned(LDataBand) then
    raise EMARSFastReportException.CreateFmt('[SetDataSetForBand] Band [%s] not found. Report def: %s', [ABandName, ReportDefName]);

  LDataBand.DataSet := ADataSet;
end;

procedure TMARSFastReport.SetMasterDataSet(const ADataSet: TDataSet;
  const ADataSetName: string; const AMasterDataName: string);
var
  LFrxDataSet: TfrxDBDataSet;
begin
  LFrxDataSet := AddDataSet(ADataSet, ADataSetName);
  try
    SetDataSetForBand(LFrxDataSet, AMasterDataName);
  except
    LFrxDataSet.Free;
    raise;
  end;
end;

function TMARSFastReport.AddDataSet(const ADataSet: TDataSet;
  const ADataSetName: string): TfrxDBDataset;
begin
  Result := TfrxDBDataSet.Create(nil);
  try
    Result.DataSet := ADataSet;

    if ADataSetName <> '' then
    begin
      Result.Name := ADataSetName;
      Result.UserName := ADataSetName;
    end
    else
    begin
      Result.Name := ADataSet.Name;
      Result.UserName := ADataSet.Name;
    end;

    Report.DataSets.Add(Result);

    Activation.AddToContext(Result);
    Activation.AddToContext(ADataSet);
  except
    Result.Free;
    raise;
  end;
end;

class constructor TMARSFastReport.ClassCreate;
begin
  FReportDefDictionary := TReportDefDictionary.Create;;
end;

class destructor TMARSFastReport.ClassDestroy;
begin
  FreeAndNil(FReportDefDictionary);
end;

constructor TMARSFastReport.Create(const AReportDefName: string; const AActivation: IMARSActivation);
begin
  inherited Create;
  ReportDefName := AReportDefName;
  FActivation := AActivation;
end;

class function TMARSFastReport.CreateReportByDefName(
  const AReportDefName: string; const AActivation: IMARSActivation): TfrxReport;
var
  LReportDef: TReportDef;
begin
  LReportDef := GetReportDefByName(AReportDefName);

  Result := GetReportClass().Create(nil);
  try
//    Result.EngineOptions.SilentMode := True;
//    Result.EngineOptions.NewSilentMode := TfrxSilentMode.simSilent;

    if AReportDefName <> '' then
      Result.Name := AReportDefName;

    if LReportDef.FileName <> '' then
      Result.LoadFromFile(LReportDef.FileName);

    Result.DataSets.Clear;

    if Assigned(FAfterCreateReport) then
      FAfterCreateReport(Result, AActivation);
  except
    Result.Free;
    raise;
  end;
end;

destructor TMARSFastReport.Destroy;
begin
  FreeAndNil(FPDFExport);
  FreeAndNil(FReport);
  inherited;
end;

procedure TMARSFastReport.DoReportAfterPrint(Sender: TfrxReportComponent);
begin

end;

procedure TMARSFastReport.DoReportBeforePrint(Sender: TfrxReportComponent);
begin

end;

procedure TMARSFastReport.DoReportBeginDoc(Sender: TObject);
begin

end;

procedure TMARSFastReport.DoReportEndDoc(Sender: TObject);
begin

end;

procedure TMARSFastReport.DoReportGetValue(const VarName: string;
  var Value: Variant);
var
  LContextValue: TValue;
begin
  LContextValue := GetContextValue(VarName, Activation);
  Value := LContextValue.AsVariant;

  if Assigned(FReportOnGetValue) then
    FReportOnGetValue(Report, VarName, Value);
end;

procedure TMARSFastReport.DoReportPrintPage(Page: TfrxReportPage;
  CopyNo: Integer);
begin

end;

procedure TMARSFastReport.DoReportPrintReport(Sender: TObject);
begin

end;

function TMARSFastReport.ExportToPDF: TBytes;
var
  LFileName: string;
begin
  Report.PrepareReport();

  LFileName := TPath.GetTempFileName;
  try
    PDFExport.FileName := LFileName;
    FReport.Export(PDFExport);

    Result := TFile.ReadAllBytes(LFileName);
  finally
    TFile.Delete(LFileName);
  end;
end;

class function TMARSFastReport.GetContextValue(const AName: string;
  const AActivation: IMARSActivation; const ADesiredType: TFieldType): TValue;
var
  LFirstToken, LSecondToken: string;
  LHasThirdToken: Boolean;
  LSecondTokenAndAll, LThirdTokenAndAll: string;
  LNameTokens: TArray<string>;
  LFirstDelim, LSecondDelim: Integer;

  LIndex: Integer;
//  LCustomProvider: TContextValueProviderProc;
begin
  Result := TValue.Empty;
  LNameTokens := AName.Split([MACRO_DELIMITER]);
  if Length(LNameTokens) < 2 then
    Exit;

  LFirstToken := LNameTokens[0];
  LSecondToken := LNameTokens[1];
  LFirstDelim := AName.IndexOf(MACRO_DELIMITER);
  LSecondTokenAndAll := AName.Substring(LFirstDelim + 1);
  LHasThirdToken := Length(LNameTokens) > 2;
  if LHasThirdToken then
  begin
    LSecondDelim := AName.IndexOf(MACRO_DELIMITER, LFirstDelim + Length(MACRO_DELIMITER));
    LThirdTokenAndAll := AName.Substring(LSecondDelim + 1);
  end;

  if SameText(LFirstToken, 'Token') then
  begin
    Result := ReadPropertyValue(AActivation.Token, LSecondToken);

    if SameText(LSecondToken, 'HasRole') and LHasThirdToken then
      Result := AActivation.Token.HasRole(LThirdTokenAndAll)
    else if SameText(LSecondToken, 'Claim') and LHasThirdToken then
      Result := AActivation.Token.Claims.ByNameText(LThirdTokenAndAll);
  end
  else if SameText(LFirstToken, 'PathParam') then
  begin
    LIndex := AActivation.URLPrototype.GetPathParamIndex(LSecondTokenAndAll);
    if (LIndex > -1) and (LIndex < Length(AActivation.URL.PathTokens)) then
      Result := AActivation.URL.PathTokens[LIndex] { TODO -oAndrea : Try to convert according to ADesiredType }
    else
      raise EMARSFastReportException.CreateFmt('PathParam not found: %s', [LSecondTokenAndAll]);
  end
  else if SameText(LFirstToken, 'QueryParam') then
    Result := AActivation.URL.QueryTokenByName(LSecondTokenAndAll)
  else if SameText(LFirstToken, 'FormParam') then
    Result := AActivation.Request.GetFormParamValue(LSecondTokenAndAll)
  else if SameText(LFirstToken, 'Request') then
    Result := ReadPropertyValue(AActivation.Request.AsObject, LSecondTokenAndAll)
//  else if SameText(LFirstToken, 'Response') then
//    Result := ReadPropertyValue(AActivation.Response, LSecondToken)
  else if SameText(LFirstToken, 'URL') then
    Result := ReadPropertyValue(AActivation.URL, LSecondTokenAndAll)
  else if SameText(LFirstToken, 'URLPrototype') then
    Result := ReadPropertyValue(AActivation.URLPrototype, LSecondTokenAndAll)
//  else // last chance, custom injection
//    for LCustomProvider in FContextValueProviders do
//      LCustomProvider(AActivation, AName, ADesiredType, Result);
end;

function TMARSFastReport.GetPDFExport: TfrxPDFExport;
begin
  if not Assigned(FPDFExport) then
  begin
    FPDFExport := TfrxPDFExport.Create(nil);

    FPDFExport.ShowDialog := False;
//    FPDFExport.UseFileCache = True;
    FPDFExport.ShowProgress := False;
    FPDFExport.OverwritePrompt := False;
//    FPDFExport.DataOnly = False;
//    FPDFExport.EmbedFontsIfProtected = False;
//    FPDFExport.InteractiveFormsFontSubset = 'A-Z,a-z,0-9,#43-#47 ';
    FPDFExport.OpenAfterExport := False;
    FPDFExport.PrintOptimized := False;            // PARAM
//    FPDFExport.Outline = False
//    FPDFExport.Background = False
//    FPDFExport.HTMLTags = True
//    FPDFExport.Quality = 95
    FPDFExport.Author := 'MARS FastReport';        // PARAM
    FPDFExport.Subject := 'MARS FastReport PDF';   // PARAM
    FPDFExport.Creator := 'MARS FastReport';       // PARAM
    FPDFExport.ProtectionFlags := [ePrint, eModify, eCopy, eAnnot]; // PARAM
    FPDFExport.HideToolbar := False;               // PARAM
    FPDFExport.HideMenubar := False;               // PARAM
    FPDFExport.HideWindowUI := False;              // PARAM
    FPDFExport.FitWindow := False;                 // PARAM
    FPDFExport.CenterWindow := False;              // PARAM
//    FPDFExport.PrintScaling = False
//    FPDFExport.PdfA = False
//    FPDFExport.PDFStandard = psNone
//    FPDFExport.PDFVersion = pv17
  end;
  Result := FPDFExport;
end;

function TMARSFastReport.GetReport: TfrxReport;
begin
  if not Assigned(FReport) then
  begin
    FReport := CreateReportByDefName(ReportDefName, FActivation);

    FReport.OnBeforePrint := DoReportBeforePrint;
    FReport.OnAfterPrint := DoReportAfterPrint;
    FReport.OnBeginDoc := DoReportBeginDoc;
    FReport.OnEndDoc := DoReportEndDoc;
    FReport.OnGetValue := DoReportGetValue;
    FReport.OnPrintPage := DoReportPrintPage;
    FReport.OnPrintReport := DoReportPrintReport;
  end;
  Result := FReport;
end;

class function TMARSFastReport.GetReportClass: TfrxReportClass;
begin
  Result := TfrxReport;
  if Assigned(FCustomReportClass) then
    Result := FCustomReportClass;
end;

function TMARSFastReport.GetReportDef: TReportDef;
begin
  if (FReportDef.Name <> FReportDefName) then
    FReportDef := GetReportDefByName(FReportDefName);
  Result := FReportDef;
end;

class function TMARSFastReport.LoadReportDefs(
  const AParameters: TMARSParameters; const ASliceName: string): TArray<string>;
var
  LData, LReportParams: TMARSParameters;
  LReportDefNames: TArray<string>;
  LReportDefName: string;
  LReportDef: TReportDef;
begin
  Result := [];
  LData := TMARSParameters.Create('');
  try
    LData.CopyFrom(AParameters, ASliceName);
    LReportDefNames := LData.SliceNames;

    for LReportDefName in LReportDefNames do
    begin
      LReportParams := TMARSParameters.Create(LReportDefName);
      try
        LReportParams.CopyFrom(LData, LReportDefName);

        LReportDef.Name := LReportDefName;
        LReportDef.FileName := LReportParams.ByNameText('FileName', '')
          .AsString
          .Replace('{bin}', ExtractFilePath(ParamStr(0)), [rfReplaceAll, rfIgnoreCase]);

        ReportDefDictionary.Add(LReportDefName, LReportDef);

//        LParams := GetAsTStrings(LReportParams);
//        try
//          if FDManager.ConnectionDefs.FindConnectionDef(LReportDefName) = nil then
//          begin
//            FDManager.AddConnectionDef(LReportDefName, LParams.Values['DriverID'], LParams);
//            Result := Result + [LReportDefName];
//          end;
//        finally
//          LParams.Free;
//        end;


      finally
        LReportParams.Free;
      end;
    end;
  finally
    LData.Free;
  end;
end;

class function TMARSFastReport.GetReportDefByName(
  const AReportDefName: string): TReportDef;
begin
  if not FReportDefDictionary.TryGetValue(AReportDefName, Result) then
    raise EMARSFastReportException.CreateFmt('Report definition not found: [%s]', [AReportDefName]);
end;

procedure TMARSFastReport.SetReportDefName(const Value: string);
begin
  if FReportDefName <> Value then
  begin
    FreeAndNil(FReport);
    FReportDefName := Value;
  end;

end;

{ ReportAttribute }

constructor ReportAttribute.Create(AReportDefName: string;
  const AExpandMacros: Boolean);
begin
  inherited Create;
  FReportDefName := AReportDefName;
  FExpandMacros := AExpandMacros;
end;

end.
