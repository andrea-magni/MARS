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

    , TestCase('StringNumeric'
      , '''
        {
          "Value": "123"
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

    , TestCase('StringNumeric'
      , '''
        [General]
        Value=123
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

    [Test]
    procedure Test1;

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

procedure TMARSParametersFixture.Test1;
begin
  var LContext := TRttiContext.Create;

  var LParameters := TMARSParameters.Create('Test');
  try

    LParameters.Values['AString'] := 'theString';
    var LValue := LParameters['AString'];
    var LRttiType := LContext.GetType(LValue.TypeInfo);
    var LRttiTypeName := LRttiType.Name;
    Assert.AreEqual('string', LRttiTypeName, 'Type differs');

    LParameters.Values['AInteger'] := Integer(123);
    LValue := LParameters['AInteger'];
    LRttiType := LContext.GetType(LValue.TypeInfo);
    LRttiTypeName := LRttiType.Name;
    Assert.AreEqual('Integer', LRttiTypeName, 'Type differs');

    LParameters.Values['ADouble'] := Double(123.45);
    LValue := LParameters['ADouble'];
    LRttiType := LContext.GetType(LValue.TypeInfo);
    LRttiTypeName := LRttiType.Name;
    Assert.AreEqual('Double', LRttiTypeName, 'Type differs');

    LParameters.Values['ABoolean'] := True;
    LValue := LParameters['ABoolean'];
    LRttiType := LContext.GetType(LValue.TypeInfo);
    LRttiTypeName := LRttiType.Name;
    Assert.AreEqual('Boolean', LRttiTypeName, 'Type differs');


  finally
    LParameters.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSParametersFixture);


end.
