unit Tests.MARSParameters;

interface

uses
  Classes, SysUtils, Rtti, Types, TypInfo
, DUnitX.TestFramework
, MARS.Utils.Parameters
, MARS.Utils.Parameters.IniFile
, MARS.Utils.Parameters.JSON
;

type
  [TestFixture('MARSParameters')]
  TMARSParametersFixture = class
  private
  protected
  public
    [ Test

    , TestCase('String'
      , '''
        {
          "Value": "string"
        }
        |Value|string
        ''', '|')

    , TestCase('Integer'
      , '''
        {
          "Value": 123
        }
        |Value|Integer
        ''', '|')

//    , TestCase('Int64'
//      , '''
//        {
//          "Value": 12345678901234567890
//        }
//        |Value|Integer
//        ''', '|')

    , TestCase('Double'
      , '''
        {
          "Value": 123.45
        }
        |Value|Double
        ''', '|')

    , TestCase('Boolean'
      , '''
        {
          "Value": true
        }
        |Value|Boolean
        ''', '|')
    ]
    procedure LoadFromJSON(AJSONString: string; AMemberName: string; AMemberType: string);

    [ Test

    , TestCase('String'
      , '''
        [General]
        Value=string
        |General.Value|string
        ''', '|')

    , TestCase('Integer'
      , '''
        [General]
        Value=123
        |General.Value|Integer
        ''', '|')

//    , TestCase('Int64'
//      , '''
//        [General]
//        Value=12345678901234567890
//        |General.Value|Int64
//        ''', '|')

    , TestCase('Double'
      , '''
        [General]
        Value=123.45
        |General.Value|Double
        ''', '|')

    , TestCase('Boolean'
      , '''
        [General]
        Value=true
        |General.Value|Boolean
        ''', '|')

    ]
    procedure LoadFromINIFile(AIniFileContent: string; AMemberName: string; AMemberType: string);
  end;

implementation


uses
  IniFiles, IOUtils
, MARS.Core.Utils
, MARS.Core.JSON
;

{ TMARSParametersFixture }

procedure TMARSParametersFixture.LoadFromINIFile(AIniFileContent, AMemberName,
  AMemberType: string);
begin
  const LTempIniFileName = TPath.GetTempFileName;

  TFile.WriteAllText(LTempIniFileName, AIniFileContent, TEncoding.UTF8);
  try
    var LParameters := TMARSParameters.Create('Test');
    try
//      LParameters.LoadFromIniFile(LTempIniFileName);
      TMARSParametersIniFileReaderWriter.Load(LParameters, LTempIniFileName);

      var LParameterValue := LParameters.ByName(AMemberName);
      Assert.IsFalse(LParameterValue.IsEmpty, 'Parameter value is empty');

      var LContext := TRttiContext.Create;
      var LRttiType := LContext.GetType(LParameterValue.TypeInfo);
      var LRttiTypeName := LRttiType.Name;

      Assert.AreEqual(AMemberType, LRttiTypeName, 'Type differs');

    finally
      LParameters.Free;
    end;
  finally
    TFIle.Delete(LTempIniFileName);
  end;
end;

procedure TMARSParametersFixture.LoadFromJSON(AJSONString: string; AMemberName: string; AMemberType: string);
begin
  var LJSONObject := TJSONObject.ParseJSONValue(AJSONString) as TJSONObject;
  try
    var LParameters := TMARSParameters.Create('Test');
    try
//      LParameters.LoadFromJSON(LJSONObject);
      TMARSParametersJSONReaderWriter.Load(LParameters, LJSONObject);

      var LParameterValue := LParameters.ByName(AMemberName);
      Assert.IsFalse(LParameterValue.IsEmpty, 'Parameter value is empty');

      var LContext := TRttiContext.Create;
      var LRttiType := LContext.GetType(LParameterValue.TypeInfo);
      var LRttiTypeName := LRttiType.Name;

      Assert.AreEqual(AMemberType, LRttiTypeName, 'Type differs');

    finally
      LParameters.Free;
    end;
  finally
    LJSONObject.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSParametersFixture);


end.
