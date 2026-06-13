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
        Value="123"
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
    procedure TestTypes;

    [Test]
    procedure CustomLoadFromJSON;
    [Test]
    procedure CustomSaveToJSON;
  end;

implementation


uses
  IniFiles, IOUtils
, MARS.Core.Utils
, System.JSON, MARS.Core.JSON
;

{ TMARSParametersFixture }

procedure TMARSParametersFixture.CustomLoadFromJSON;
begin
 const JSONString =
 '''
   {
        "CO_dataInizio": "20240101",
        "iss": "Abracadabra",
        "CO_dataFine": "22001231",
        "idAzienda": "142",
        "BackOffice": "false",
        "CO_tipoSoggetto": "operatore",
        "duration": 10.0,
        "RE_dataFine": "22001231",
        "idCliente": "232",
        "RE_tipoSoggetto": "operatore",
        "RE_idServizio": 117,
        "exp": 1782056814,
        "iat": 1781192814,
        "Roles": "user",
        "CO_idServizio": 391,
        "RE_dataInizio": "20240101",
        "idUtente": 142,
        "UserName": "aaaaaaaaaa72410BBD7EAE05C72F4bbbbbbbbbb"
    }
 ''';

 TMARSParametersJSONReaderWriter.CustomLoadFunc :=
   function (const AParameters: TMARSParameters; const ASource: TJSONObject; const ASliceName: string): Boolean
   begin
     Result := True; // inhibits default behavior

     for var LPair in ASource do
     begin
        var LName := AParameters.CombineSliceAndParamName(ASliceName, LPair.JsonString.Value);
        var LValue: TValue := TValue.Empty;

        if LName.StartsWith('id') then
          LValue := StrToIntDef(ASource.ReadStringValue(LName, '0'), 0)
        else
          LValue := ASource.ReadValue(LName, TValue.Empty, DefaultMARSJSONSerializationOptions);
        AParameters.Values[LName] := LValue;
     end;
   end;
  try

    var LParameters := TMARSParameters.Create('Test');
    try
      LParameters.LoadFromJSON(JSONString);

      var LInteger1 := LParameters.Values[ 'idCliente' ].AsInteger;
      var LInteger2 := LParameters.Values[ 'idAzienda' ].AsInteger;

      Assert.AreEqual(232, LInteger1, 'idCliente differs');
      Assert.AreEqual(142, LInteger2, 'idAzienda differs');
    finally
      FreeAndNil(LParameters);
    end;
  finally
    TMARSParametersJSONReaderWriter.CustomLoadFunc := nil;
  end;
end;

procedure TMARSParametersFixture.CustomSaveToJSON;
begin
  TMARSParametersJSONReaderWriter.CustomSaveFunc :=
    function (const AParameters: TMARSParameters; const ADestination: TJSONObject): Boolean
    begin
      Result := True; // inhibits default behavior

      for var LPair in AParameters do
      begin
        var LValue := LPair.Value;
        if LValue.IsType<string> then
          LValue := LValue.AsString.ToUpper;

        ADestination.WriteTValue(LPair.Key, LValue)
      end;
    end;
  try

    var LParameters := TMARSParameters.Create('Test');
    try
     LParameters.Values[ 'idCliente' ] := 232;
     LParameters.Values[ 'idAzienda' ] := 142;
     LParameters.Values[ 'myString' ] := 'Andrea';

      var LJSON := LParameters.SaveToJSON;
      try
        Assert.AreEqual(232, LJSON.ReadIntegerValue('idCliente'), 'idCliente differs');
        Assert.AreEqual(142, LJSON.ReadIntegerValue('idAzienda'), 'idAzienda differs');
        Assert.AreEqual('ANDREA', LJSON.ReadStringValue('myString'), False, 'myString differs');
      finally
        FreeAndNil(LJSON);
      end;

    finally
      FreeAndNil(LParameters);
    end;
  finally
    TMARSParametersJSONReaderWriter.CustomSaveFunc := nil;
  end;
end;

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

      Assert.AreEqual(AMemberType, LRttiTypeName, 'Type differs ' + AMemberName + ' ' + AMemberType);

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

procedure TMARSParametersFixture.TestTypes;
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
