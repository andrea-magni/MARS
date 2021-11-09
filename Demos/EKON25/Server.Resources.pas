(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes, System.Rtti
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.URL
//, MARS.Core.Token
, MARS.Core.Token.Resource
, MARS.OpenAPI.v3, MARS.Metadata.Attributes, MARS.Core.Activation.Interfaces
, MARS.YAML.ReadersAndWriters, Neslib.Yaml, Neslib.Utf8
;

type
  TNationality = (IT, DE, Other);

  TSpeaker = record
  private
    function GetAgeInYears: Integer;
  public
    Name: string;
    Surname: string;
    DateOfBirth: TDate;
    Nationality: TNationality;
    AgeInYears: Integer;
    procedure SetDateOfBirth(const ADate: TDate);
  end;

  TSpeech = record
    Speaker: TSpeaker;
    Title: string;
    ScheduledAt: TDateTime;
    DurationInMinutes: Integer;

    function ToJSONFilter(const AMember: TRttiMember; const AObj: TJSONObject): Boolean;
    function ToYAMLFilter(const AMember: TRttiMember; const AYAML: TYamlNode): Boolean;

    constructor Create(const ASpeaker: TSpeaker; const ATitle: string;
      const AScheduledAt: TDateTime; const ADuration: Integer = 60);
  end;

  TConference = class
  private
    FSpeeches: TArray<TSpeech>;
    FEndDate: TDate;
    FBeginDate: TDate;
    function GetDurationInDays: Integer;
    function GetSpeechCount: Integer;
    procedure SetBeginDate(const Value: TDate);
    procedure SetEndDate(const Value: TDate);
  public
    // Fields
    Name: string;

    // Methods
    procedure AddSpeech(const ASpeech: TSpeech);

    function ToJSONFilter(const AMember: TRttiMember; const AObj: TJSONObject): Boolean;

    // Properties
    [JSONName('theDate'), YAMLName('theDate')]
    property BeginDate: TDate read FBeginDate write SetBeginDate;
    [JSONName(''), YAMLName('')]
    property EndDate: TDate read FEndDate write SetEndDate;
    property SpeechCount: Integer read GetSpeechCount;
    property DurationInDays: Integer read GetDurationInDays;
    property Speeches: TArray<TSpeech> read FSpeeches;
  end;


  [Path('helloworld')]
  TEKONConferenceResource = class
  protected
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [ GET
    , Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)
    , Path('/speaker')]
    function GetSpeaker: TSpeaker;

    [ GET
    , Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)
    , Path('/speech')]
    function GetSpeech: TSpeech;

    [ GET
    , Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)
    , Path('/conference')]
    function GetConference: TConference;

    [ POST
    , Consumes(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_YAML)
    , Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)
    , Path('/speech')]
    function PostSpeech([BodyParam] ASpeech: TSpeech): TSpeech;

    [ GET
    , Path('/openapi')
    , Produces(TMediaType.APPLICATION_JSON)
    , Produces(TMediaType.APPLICATION_YAML)
    // prevents the method itself being part of the OpenAPI specification
    , MetaVisible(False)]
    function GetOpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;
  end;

  [Path('token'), MetaDescription('Authentication and authorization')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
  DateUtils, TimeSpan
, MARS.Core.Registry
, CodeSiteLogging
;

{ TEKONConferenceResource }

function TEKONConferenceResource.GetConference: TConference;
begin
  Result := TConference.Create;
  Result.Name := 'EKON25';
  Result.BeginDate := EncodeDateTime(2021, 11, 08, 09, 00, 00, 000);
  Result.EndDate := EncodeDateTime(2021, 11, 10, 17, 00, 00, 000);
  Result.AddSpeech(GetSpeech);
  Result.AddSpeech(
    TSpeech.Create(GetSpeaker
                   , 'Mobile development with FMXER'
                   , EncodeDateTime(2021, 11, 10, 13, 30, 0, 000)
                   , 210
  ));
end;

function TEKONConferenceResource.GetOpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;
begin
  Result := AOpenAPI;
end;

function TEKONConferenceResource.GetSpeech: TSpeech;
begin
  Result.Speaker := GetSpeaker;
  Result.Title := 'OpenAPI 3.0 in MARS REST library';
  Result.ScheduledAt := EncodeDateTime(2021, 11, 09, 13, 45, 00, 000);
  Result.DurationInMinutes := 60;
end;

function TEKONConferenceResource.PostSpeech(ASpeech: TSpeech): TSpeech;
begin
  Result := ASpeech;
end;

function TEKONConferenceResource.GetSpeaker: TSpeaker;
begin
  Result.Name := 'Andrea';
  Result.Surname := 'Magni';
  Result.Nationality := TNationality.IT;
  Result.SetDateOfBirth(EncodeDate(1982, 5, 24));
end;

function TEKONConferenceResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

{ TSpeaker }

function TSpeaker.GetAgeInYears: Integer;
begin
  Result := YearsBetween(DateOfBirth, Now);
end;

procedure TSpeaker.SetDateOfBirth(const ADate: TDate);
begin
  DateOfBirth := ADate;
  AgeInYears := GetAgeInYears;
end;

{ TConference }

procedure TConference.AddSpeech(const ASpeech: TSpeech);
begin
  FSpeeches := FSpeeches + [ASpeech];
end;

function TConference.GetDurationInDays: Integer;
begin
  Result := DaysBetween(BeginDate, EndDate);
end;

function TConference.GetSpeechCount: Integer;
begin
  Result := Length(FSpeeches);
end;

procedure TConference.SetBeginDate(const Value: TDate);
begin
  FBeginDate := Value;
end;

procedure TConference.SetEndDate(const Value: TDate);
begin
  FEndDate := Value;
end;

function TConference.ToJSONFilter(const AMember: TRttiMember;
  const AObj: TJSONObject): Boolean;
begin
  Result := True;

  // skip fields
  if AMember is TRttiField then Result := False;

  // custom values
  if (AMember.Name = 'BeginDate') then
    AObj.WriteBoolValue('Live', ((Now > BeginDate) and (Now < EndDate)));
end;

{ TSpeech }

constructor TSpeech.Create(const ASpeaker: TSpeaker; const ATitle: string;
  const AScheduledAt: TDateTime; const ADuration: Integer);
begin
  Speaker := ASpeaker;
  Title := ATitle;
  ScheduledAt := AScheduledAt;
  DurationInMinutes := ADuration;
end;

function TSpeech.ToJSONFilter(const AMember: TRttiMember; const AObj: TJSONObject): Boolean;
begin
  Result := True;

  if (AMember.Name = 'ScheduledAt') then
  begin
    if (ScheduledAt > Now) then
    begin
      var LTimeSpan := TTimeSpan.FromDays(ScheduledAt - Now);
      AObj.WriteIntegerValue('countdownDays', LTimeSpan.Days);
      AObj.WriteIntegerValue('countdownHours', LTimeSpan.Hours);
      AObj.WriteIntegerValue('countdownMinutes', LTimeSpan.Minutes);
      AObj.WriteIntegerValue('countdownSeconds', LTimeSpan.Seconds);
    end
    else
    begin
      if MinutesBetween(ScheduledAt, Now) <= DurationInMinutes then
      begin
        var LTimeSpan := TTimeSpan.FromDays(Now - ScheduledAt);
        AObj.WriteIntegerValue('elapsedDays', LTimeSpan.Days);
        AObj.WriteIntegerValue('elapsedHours', LTimeSpan.Hours);
        AObj.WriteIntegerValue('elapsedMinutes', LTimeSpan.Minutes);
        AObj.WriteIntegerValue('elapsedSeconds', LTimeSpan.Seconds);
      end;
    end;
  end;
end;

function TSpeech.ToYAMLFilter(const AMember: TRttiMember;
  const AYAML: TYamlNode): Boolean;
begin
  Result := True;

  if (AMember.Name = 'ScheduledAt') then
  begin
    if (ScheduledAt > Now) then
    begin
      var LTimeSpan := TTimeSpan.FromDays(ScheduledAt - Now);
      var LNode := AYAML.AddOrSetMapping('elapsed');
      LNode.AddOrSetValue('days', LTimeSpan.Days);
      LNode.AddOrSetValue('hours', LTimeSpan.Hours);
      LNode.AddOrSetValue('minutes', LTimeSpan.Minutes);
      LNode.AddOrSetValue('seconds', LTimeSpan.Seconds);
    end
    else
    begin
      if MinutesBetween(ScheduledAt, Now) <= DurationInMinutes then
      begin
        var LTimeSpan := TTimeSpan.FromDays(Now - ScheduledAt);
        var LNode := AYAML.AddOrSetMapping('elapsed');
        LNode.AddOrSetValue('days', LTimeSpan.Days);
        LNode.AddOrSetValue('hours', LTimeSpan.Hours);
        LNode.AddOrSetValue('minutes', LTimeSpan.Minutes);
        LNode.AddOrSetValue('seconds', LTimeSpan.Seconds);
      end;
    end;
  end;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResources([TEKONConferenceResource, TTokenResource]);
end.
