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

  // [Include] section and case insensitive ini parameters
  [TestFixture('MARSParameters Ini')]
  TMARSParametersIniFixture = class
  private
    FFolder: string;
    function WriteIni(const ARelativeFileName, AContent: string): string;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure IncludedFileIsOverridden;
    [Test] procedure NestedIncludes;
    [Test] procedure SeveralIncludesInOrder;
    [Test] procedure CircularIncludeRaises;
    [Test] procedure MissingIncludeRaises;
    [Test] procedure IniIsCaseInsensitive;
    [Test] procedure JSONStaysCaseSensitive;
    [Test] procedure ApplicationCopyKeepsCaseInsensitivity;
  end;

implementation


uses
  IniFiles, IOUtils, StrUtils
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
//      TMARSParametersIniFileReaderWriter.Load(LParameters, LTempIniFileName);
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

{ TMARSParametersIniFixture }

procedure TMARSParametersIniFixture.Setup;
begin
  FFolder := TPath.Combine(TPath.GetTempPath, 'MARSParametersIni-' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(TPath.Combine(FFolder, 'Project'));
end;

procedure TMARSParametersIniFixture.TearDown;
begin
  if TDirectory.Exists(FFolder) then
    TDirectory.Delete(FFolder, True);
end;

function TMARSParametersIniFixture.WriteIni(const ARelativeFileName, AContent: string): string;
begin
  Result := TPath.Combine(FFolder, ARelativeFileName);
  TFile.WriteAllText(Result, AContent, TEncoding.UTF8);
end;

procedure TMARSParametersIniFixture.IncludedFileIsOverridden;
begin
  WriteIni('BaseConfiguration.ini', '''
    [DefaultEngine]
    Port=8080
    ThreadPoolSize=50
    DefaultApp.JWT.Duration=1
    [DefaultApp]
    JWT.Secret=base-secret
    ''');
  var LProjectIni := WriteIni('Project\Project.ini', '''
    [Include]
    Base=..\BaseConfiguration.ini
    [DefaultEngine]
    Port=9090
    DefaultApp.JWT.Secret=project-secret
    ''');

  var LParameters := TMARSParameters.Create('DefaultEngine');
  try
    TMARSParametersIniFileReaderWriter.Load(LParameters, LProjectIni);

    Assert.AreEqual(9090, LParameters.ByName('Port').AsInteger, 'project wins');
    Assert.AreEqual(50, LParameters.ByName('ThreadPoolSize').AsInteger, 'from the base');
    Assert.AreEqual(1, LParameters.ByName('DefaultApp.JWT.Duration').AsInteger, 'from the base');
    Assert.AreEqual('project-secret', LParameters.ByName('DefaultApp.JWT.Secret').AsString, 'project wins');
    Assert.IsFalse(LParameters.ContainsParam('Include.Base'), '[Include] is not a parameter');
    Assert.AreEqual(4, LParameters.Count);
  finally
    LParameters.Free;
  end;
end;

procedure TMARSParametersIniFixture.NestedIncludes;
begin
  WriteIni('Company.ini', '''
    [DefaultEngine]
    A=company
    B=company
    C=company
    ''');
  WriteIni('BaseConfiguration.ini', '''
    [Include]
    Company=Company.ini
    [DefaultEngine]
    B=base
    C=base
    ''');
  var LProjectIni := WriteIni('Project\Project.ini', '''
    [Include]
    Base=..\BaseConfiguration.ini
    [DefaultEngine]
    C=project
    ''');

  var LParameters := TMARSParameters.Create('DefaultEngine');
  try
    TMARSParametersIniFileReaderWriter.Load(LParameters, LProjectIni);
    Assert.AreEqual('company', LParameters.ByName('A').AsString);
    Assert.AreEqual('base', LParameters.ByName('B').AsString);
    Assert.AreEqual('project', LParameters.ByName('C').AsString);
  finally
    LParameters.Free;
  end;
end;

procedure TMARSParametersIniFixture.SeveralIncludesInOrder;
begin
  WriteIni('First.ini', '''
    [DefaultEngine]
    A=first
    B=first
    ''');
  WriteIni('Second.ini', '''
    [DefaultEngine]
    B=second
    ''');
  var LProjectIni := WriteIni('Project.ini', '''
    [Include]
    One=First.ini
    Two=Second.ini
    ''');

  var LParameters := TMARSParameters.Create('DefaultEngine');
  try
    TMARSParametersIniFileReaderWriter.Load(LParameters, LProjectIni);
    Assert.AreEqual('first', LParameters.ByName('A').AsString);
    Assert.AreEqual('second', LParameters.ByName('B').AsString, 'later include wins');
  finally
    LParameters.Free;
  end;
end;

procedure TMARSParametersIniFixture.CircularIncludeRaises;
begin
  WriteIni('A.ini', '''
    [Include]
    B=B.ini
    ''');
  WriteIni('B.ini', '''
    [Include]
    A=A.ini
    ''');

  var LParameters := TMARSParameters.Create('DefaultEngine');
  try
    var LCall: TTestLocalMethod :=
      procedure
      begin
        TMARSParametersIniFileReaderWriter.Load(LParameters, TPath.Combine(FFolder, 'A.ini'));
      end;
    Assert.WillRaiseWithMessageRegex(LCall, EMARSParametersIniFileException, 'Circular.*A\.ini -> .*B\.ini -> .*A\.ini');
  finally
    LParameters.Free;
  end;
end;

procedure TMARSParametersIniFixture.MissingIncludeRaises;
begin
  var LProjectIni := WriteIni('Project.ini', '''
    [Include]
    Base=NotThere.ini
    ''');

  var LParameters := TMARSParameters.Create('DefaultEngine');
  try
    var LCall: TTestLocalMethod :=
      procedure
      begin
        TMARSParametersIniFileReaderWriter.Load(LParameters, LProjectIni);
      end;
    Assert.WillRaiseWithMessageRegex(LCall, EMARSParametersIniFileException, 'NotThere\.ini');
  finally
    LParameters.Free;
  end;
end;

procedure TMARSParametersIniFixture.IniIsCaseInsensitive;
begin
  WriteIni('BaseConfiguration.ini', '''
    [DefaultEngine]
    Feature.X=base
    [DefaultApp]
    JWT.Secret=base-secret
    ''');
  var LProjectIni := WriteIni('Project.ini', '''
    [include]
    base=BaseConfiguration.ini
    [defaultengine]
    feature.x=project
    [defaultapp]
    jwt.secret=project-secret
    ''');

  var LParameters := TMARSParameters.Create('DefaultEngine');
  try
    TMARSParametersIniFileReaderWriter.Load(LParameters, LProjectIni);

    Assert.AreEqual(2, LParameters.Count, 'no duplicates');
    Assert.AreEqual('project', LParameters.ByName('Feature.X').AsString);
    Assert.AreEqual('project', LParameters.ByName('FEATURE.X').AsString);
    Assert.AreEqual('project-secret', LParameters.ByName('DefaultApp.JWT.Secret').AsString);
    Assert.IsTrue(LParameters.ContainsParam('defaultapp.jwt.secret'));
    Assert.IsTrue(MatchStr('Feature.X', LParameters.ParamNames), 'first spelling kept');

    // set in code, in another case: same parameter
    LParameters.Values['feature.X'] := 'code';
    Assert.AreEqual(2, LParameters.Count);
    Assert.AreEqual('code', LParameters.ByName('Feature.X').AsString);
  finally
    LParameters.Free;
  end;
end;

procedure TMARSParametersIniFixture.JSONStaysCaseSensitive;
begin
  var LParameters := TMARSParameters.Create('');
  try
    LParameters.LoadFromJSON('{"Value": "upper", "value": "lower"}');

    Assert.AreEqual(2, LParameters.Count);
    Assert.AreEqual('upper', LParameters.ByName('Value').AsString);
    Assert.AreEqual('lower', LParameters.ByName('value').AsString);
    Assert.IsTrue(LParameters.ByName('VALUE').IsEmpty);
  finally
    LParameters.Free;
  end;
end;

procedure TMARSParametersIniFixture.ApplicationCopyKeepsCaseInsensitivity;
begin
  var LIni := WriteIni('Project.ini', '''
    [DefaultApp]
    jwt.secret=s3cr3t
    ''');

  var LEngine := TMARSParameters.Create('DefaultEngine');
  var LApp := TMARSParameters.Create('DefaultApp');
  try
    TMARSParametersIniFileReaderWriter.Load(LEngine, LIni);
    LApp.CopyFrom(LEngine, 'DefaultApp'); // what AddApplication does

    Assert.AreEqual('s3cr3t', LApp.ByName('JWT.Secret').AsString);
  finally
    LApp.Free;
    LEngine.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSParametersFixture);
  TDUnitX.RegisterTestFixture(TMARSParametersIniFixture);


end.
