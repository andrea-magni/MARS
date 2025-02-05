unit MARS.Tests.TestCaseProvider;

interface

uses
  Classes, SysUtils, Generics.Collections, System.Rtti
, DUnitX.Types, DUnitX.InternalDataProvider, DUnitX.TestDataProvider
, DUnitX.TestFramework
, MARS.Core.Engine, MARS.Core.Application, MARS.Core.Registry.Utils
, MARS.Tests.Types
;

type
  TProviderAppFunc = TFunc<TMARSApplication>;

  TMARSTestCaseData = record
    Data: TRequestData;

    constructor Create(const AResourceName: string;
      const AInfo: TMARSConstructorInfo;
      const AMethod: TRttiMethod;
      const AResourcePath, AMethodPath, AHttpMethod: string);
  end;

  TMARSTestCaseProvider = class(TTestDataProvider)
  private
     FList : TList<TMARSTestCaseData>;
  protected
     function GetEngine: TMARSEngine; virtual; abstract;
     function GetApplication: TMARSApplication; virtual; abstract;
     procedure InitTestCaseData; virtual;
     property List: TList<TMARSTestCaseData> read FList;
     property Engine: TMARSEngine read GetEngine;
     property Application: TMARSApplication read GetApplication;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCaseCount(const methodName: string): Integer; override;
    function GetCaseName(const methodName: string; const caseNumber: Integer): string; override;
    function GetCaseParams(const methodName: string; const caseNumber: Integer): TValueArray; override;
  end;

  procedure RegisterMARSTestCaseProvider(const AProviderName: string; const AProviderClass: TTestDataProviderClass);

implementation

uses
  MARS.Core.Attributes, MARS.Core.Utils, MARS.Core.URL
, MARS.Rtti.Utils
;

procedure RegisterMARSTestCaseProvider(const AProviderName: string; const AProviderClass: TTestDataProviderClass);
begin
  TestDataProviderManager.RegisterProvider(AProviderName, AProviderClass);
end;

{ TMARSTestCaseProvider }

constructor TMARSTestCaseProvider.Create;
begin
  inherited;
  FList := TList<TMARSTestCaseData>.create;
  InitTestCaseData;
end;

destructor TMARSTestCaseProvider.Destroy;
begin
  FList.Free;
  inherited;
end;

function TMARSTestCaseProvider.GetCaseCount(const methodName: string): Integer;
begin
  Result := FList.Count;
end;

function TMARSTestCaseProvider.GetCaseName(const methodName: string;
  const caseNumber: Integer): string;
begin
  var LTestCase := FList[caseNumber];
  Result := LTestCase.Data.MethodQualifiedName + ' ' + LTestCase.Data.HttpMethod + ' ' + LTestCase.Data.Path;
end;

function TMARSTestCaseProvider.GetCaseParams(const methodName: string;
  const caseNumber: Integer): TValueArray;
begin
  var LTestCase := FList[caseNumber];
  Result := [
    LTestCase.Data.ResourceName
  , LTestCase.Data.Info
  , LTestCase.Data.Method
  , TValue.From<TRequestData>(LTestCase.Data)
  ];
end;

procedure TMARSTestCaseProvider.InitTestCaseData;
begin
  var LBasePath := Engine.BasePath + Application.BasePath + '/';

  FList.Clear;
  Application.EnumerateResources(
    procedure (AResourceName: string; AInfo: TMARSConstructorInfo)
    begin
      var LResourcePath := AInfo.Path;
      for var LMethod in AInfo.Methods do
      begin
        var LMethodPath := '';
        var LHttpMethod := '';

        for var LAttribute in LMethod.GetAttributes do
        begin
          if LAttribute is PathAttribute then
            LMethodPath := PathAttribute(LAttribute).Value;

          if LAttribute is HttpMethodAttribute then
            LHttpMethod := HttpMethodAttribute(LAttribute).HttpMethodName;
        end;

        // if RolesAllowed --> Token valido

        if (LResourcePath <> '') and (LHttpMethod <> '') then
          FList.Add(TMARSTestCaseData.Create(AResourceName, AInfo, LMethod, LBasePath + LResourcePath, LMethodPath, LHttpMethod));
      end;
    end
  );
end;

{ TMARSTestCaseData }

constructor TMARSTestCaseData.Create(const AResourceName: string;
  const AInfo: TMARSConstructorInfo;
  const AMethod: TRttiMethod;
  const AResourcePath, AMethodPath, AHttpMethod: string);
begin
  Data.SetContext(AResourceName, AInfo, AMethod);
  Data.Path := TMARSURL.URL_PATH_SEPARATOR + SmartConcat([AResourcePath, AMethodPath], TMARSURL.URL_PATH_SEPARATOR);
  Data.HttpMethod := AHttpMethod;
end;

end.
