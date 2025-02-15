(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Report.FastReport.InjectionService;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti, Types
  , MARS.Core.Injection
  , MARS.Core.Injection.Interfaces
  , MARS.Core.Injection.Types
  , MARS.Core.Activation.Interfaces
;

type
  TMARSFastReportInjectionService = class(TInterfacedObject, IMARSInjectionService)
  protected
  public
    procedure GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation; out AValue: TInjectionValue);

    const FastReport_ReportDefName_PARAM = 'FastReport.ReportDefName';
    const FastReport_ReportExpandMacros_PARAM = 'FastReport.ReportExpandMacros';

    const FastReport_ReportDefName_PARAM_DEFAULT = 'MAIN_REPORT';
    const FastReport_ReportExpandMacros_PARAM_DEFAULT = False;

   class function GetReportDefName(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): string;
  end;

implementation

uses
  MARS.Rtti.Utils
, MARS.Core.Token, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application
, Data.DB
, frxClass
//, FireDAC.Comp.Client
, MARS.Report.FastReport
;


{ TMARSFastReportInjectionService }

class function TMARSFastReportInjectionService.GetReportDefName(
  const ADestination: TRttiObject;
  const AActivation: IMARSActivation): string;
var
  LReportDefName: string;
  LExpandMacros: Boolean;
begin
  LReportDefName := '';
  LExpandMacros := False;

  // field, property or method param annotation
  ADestination.HasAttribute<ReportAttribute>(
    procedure (AAttrib: ReportAttribute)
    begin
      LReportDefName := AAttrib.ReportDefName;
      LExpandMacros := AAttrib.ExpandMacros;
    end
  );

  // second chance: method annotation
  if (LReportDefName = '') then
    AActivation.Method.HasAttribute<ReportAttribute>(
      procedure (AAttrib: ReportAttribute)
      begin
        LReportDefName := AAttrib.ReportDefName;
        LExpandMacros := AAttrib.ExpandMacros;
      end
    );

  // third chance: resource annotation
  if (LReportDefName = '') then
    AActivation.Resource.HasAttribute<ReportAttribute>(
      procedure (AAttrib: ReportAttribute)
      begin
        LReportDefName := AAttrib.ReportDefName;
        LExpandMacros := AAttrib.ExpandMacros;
      end
    );

  // last chance: application parameters
  if (LReportDefName = '') then
  begin
    LReportDefName := AActivation.Application.Parameters.ByName(
      FastReport_ReportDefName_PARAM, FastReport_ReportDefName_PARAM_DEFAULT
    ).AsString;
    LExpandMacros := AActivation.Application.Parameters.ByName(
      FastReport_ReportExpandMacros_PARAM, FastReport_ReportExpandMacros_PARAM_DEFAULT
    ).AsBoolean;
  end;

  if LExpandMacros then
    LReportDefName := TMARSFastReport.GetContextValue(LReportDefName, AActivation, ftString).AsString;

  Result := LReportDefName;
end;

procedure TMARSFastReportInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
begin
  if ADestination.GetRttiType.IsObjectOfType(TfrxReport) then
    AValue := TInjectionValue.Create(
      TMARSFastReport.CreateReportByDefName(GetReportDefName(ADestination, AActivation), AActivation)
    )
  else if ADestination.GetRttiType.IsObjectOfType(TMARSFastReport) then
    AValue := TInjectionValue.Create(
      TMARSFastReport.Create(GetReportDefName(ADestination, AActivation), AActivation)
    );
end;

procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TMARSFastReportInjectionService.Create;
    end
  , function (const ADestination: TRttiObject): Boolean
    var
      LType: TRttiType;
    begin
      Result := ((ADestination is TRttiParameter) or (ADestination is TRttiField) or (ADestination is TRttiProperty));
      if Result then
      begin
        LType := ADestination.GetRttiType;
        Result := LType.IsObjectOfType(TfrxReport)
          or LType.IsObjectOfType(TMARSFastReport);
      end;
    end
  );
end;

initialization
  RegisterServices;

end.
