(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.MessageBodyReader;

{$I MARS.inc}

interface

uses
    Classes
  , SysUtils
  , Rtti
  , Generics.Defaults
  , Generics.Collections
  , MARS.Core.MediaType
  , MARS.Core.Declarations
  , MARS.Core.Classes
  , MARS.Core.Activation.Interfaces
  ;

type
  IMessageBodyReader = interface
  ['{C22068E1-3085-482D-9EAB-4829C7AE87C0}']

    function ReadFrom(
    {$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
      const ADestination: TRttiObject; const AMediaType: TMediaType;
      const AActivation: IMARSActivation
    ): TValue;
  end;

  TIsReadableFunction = reference to function(AType: TRttiType;
    const AAttributes: TAttributeArray; AMediaType: string): Boolean;
  TGetAffinityFunction = reference to function(AType: TRttiType;
    const AAttributes: TAttributeArray; AMediaType: string): Integer;

  TReaderEntryInfo = record
    _RttiType: TRttiType;
    RttiName: string;
    CreateInstance: TFunc<IMessageBodyReader>;
    IsReadable: TIsReadableFunction;
    GetAffinity: TGetAffinityFunction;
  end;

  TMARSMessageBodyReaderRegistry = class
  private
  private
    FRegistry: TList<TReaderEntryInfo>;
    FRttiContext: TRttiContext;
    class var _Instance: TMARSMessageBodyReaderRegistry;
    class function GetInstance: TMARSMessageBodyReaderRegistry; static;
  protected
    function GetConsumesMediaTypes(const AObject: TRttiObject): TMediaTypeList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterReader(
      const ACreateInstance: TFunc<IMessageBodyReader>;
      const AIsReadable: TIsReadableFunction;
      const AGetAffinity: TGetAffinityFunction;
      AReaderRttiType: TRttiType); overload;

    procedure RegisterReader(
      const AReaderClass: TClass;
      const AIsReadable: TIsReadableFunction;
      const AGetAffinity: TGetAffinityFunction); overload;

    procedure RegisterReader(const AReaderClass: TClass; const ASubjectClass: TClass;
      const AGetAffinity: TGetAffinityFunction); overload;

    procedure RegisterReader<T>(const AReaderClass: TClass); overload;

    procedure FindReader(const ADestination: TRttiObject;
      out AReader: IMessageBodyReader; out AMediaType: TMediaType);

    procedure Enumerate(const AProc: TProc<TReaderEntryInfo>);

    class property Instance: TMARSMessageBodyReaderRegistry read GetInstance;
    class function GetDefaultClassAffinityFunc<T>: TGetAffinityFunction;
    class destructor ClassDestructor;

    const AFFINITY_HIGH = 100;
    const AFFINITY_MEDIUM = 50;
    const AFFINITY_LOW = 10;
    const AFFINITY_VERY_LOW = 1;
    const AFFINITY_ZERO = 0;
  end;

implementation

uses
    MARS.Core.Utils
  , MARS.Rtti.Utils
  , MARS.Core.Exceptions
  , MARS.Core.Attributes
  ;

{ TMARSMessageBodyReaderRegistry }

class destructor TMARSMessageBodyReaderRegistry.ClassDestructor;
begin
  if Assigned(_Instance) then
    FreeAndNil(_Instance);
end;

constructor TMARSMessageBodyReaderRegistry.Create;
begin
  inherited Create;

  FRegistry := TList<TReaderEntryInfo>.Create;
  FRttiContext := TRttiContext.Create;
end;

destructor TMARSMessageBodyReaderRegistry.Destroy;
begin
  FRegistry.Free;
  inherited;
end;

procedure TMARSMessageBodyReaderRegistry.Enumerate(const AProc: TProc<TReaderEntryInfo>);
var
  LEntry: TReaderEntryInfo;
begin
  for LEntry in FRegistry do
    AProc(LEntry);
end;

procedure TMARSMessageBodyReaderRegistry.FindReader(const ADestination: TRttiObject;
  out AReader: IMessageBodyReader; out AMediaType: TMediaType);
var
  LMethod: TRttiMethod;
  LReaderEntry: TReaderEntryInfo;
  LFound: Boolean;
  LCandidateAffinity: Integer;
  LCandidate: TReaderEntryInfo;
  LReaderRttiType: TRttiType;

  LReaderMediaTypes: TMediaTypeList;
  LMethodConsumesMediaTypes: TMediaTypeList;
  LAllowedMediaTypes: TArray<string>;
  LMediaTypes: TArray<string>;
  LMediaType: string;
  LCandidateMediaType: string;
//  LCandidateQualityFactor: Double;
begin
  Assert(Assigned(ADestination));
  LMethod := ADestination.Parent as TRttiMethod;
  Assert(Assigned(LMethod));

  AMediaType := nil;
  AReader := nil;
  LFound := False;
  LCandidateAffinity := -1;
  LCandidateMediaType := '';
//  LCandidateQualityFactor := -1;

  // consider method's Consumes
  LMethodConsumesMediaTypes := GetConsumesMediaTypes(LMethod);
  try
    if LMethodConsumesMediaTypes.Count > 0 then
      LAllowedMediaTypes := LMethodConsumesMediaTypes.ToArrayOfString
    else
{$ifdef DelphiXE7_UP}
      LAllowedMediaTypes := [];
{$else}
      SetLength(LAllowedMediaTypes, 0);
{$endif}

    if (Length(LAllowedMediaTypes) = 0)
      or ((Length(LAllowedMediaTypes) = 1) and (LAllowedMediaTypes[0] = TMediaType.WILDCARD))
    then // defaults
    begin
      if LMethodConsumesMediaTypes.Count > 0 then
        LAllowedMediaTypes := LMethodConsumesMediaTypes.ToArrayOfString
      else
      begin
        SetLength(LAllowedMediaTypes, 2);
        LAllowedMediaTypes[0] := TMediaType.APPLICATION_JSON;
        LAllowedMediaTypes[1] := TMediaType.WILDCARD;
      end;
    end;

      // collect compatible Readers
      for LReaderEntry in FRegistry do
      begin
        LReaderRttiType := FRttiContext.FindType(LReaderEntry.RttiName);
        LReaderMediaTypes := GetConsumesMediaTypes(LReaderRttiType);
        try
          if LReaderMediaTypes.Contains(TMediaType.WILDCARD) then
            LMediaTypes := LAllowedMediaTypes
          else
            LMediaTypes := TMediaTypeList.Intersect(LAllowedMediaTypes, LReaderMediaTypes);
          for LMediaType in LMediaTypes do
            if LReaderEntry.IsReadable(ADestination.GetRttiType, LMethod.GetAttributes, LMediaType) then
            begin
              if not LFound
                 or (
                   (LCandidateAffinity < LReaderEntry.GetAffinity(ADestination.GetRttiType, LMethod.GetAttributes, LMediaType))
//                   or (LCandidateQualityFactor < LAcceptMediaTypes.GetQualityFactor(LMediaType))
                 )
              then
              begin
                LCandidate := LReaderEntry;
                LCandidateAffinity := LCandidate.GetAffinity(ADestination.GetRttiType, LMethod.GetAttributes, LMediaType);
                LCandidateMediaType := LMediaType;
//                LCandidateQualityFactor := 1;
                LFound := True;
              end;
            end;
        finally
          LReaderMediaTypes.Free;
        end;
      end;

      if LFound then
      begin
        AReader := LCandidate.CreateInstance();
        AMediaType := TMediaType.Create(LCandidateMediaType);
      end;
  finally
    LMethodConsumesMediaTypes.Free;
  end;
end;

class function TMARSMessageBodyReaderRegistry.GetDefaultClassAffinityFunc<T>: TGetAffinityFunction;
begin
  Result :=
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    var
      LType: TRttiType;
    begin
      Result := 0;
      if not Assigned(AType) then
        Exit;

      LType := TRttiContext.Create.GetType(TypeInfo(T));
      if (AType = LType) then
        Result := 100
      else if AType.IsObjectOfType<T>(False) then
        Result := 95
      else if AType.IsObjectOfType<T> then
        Result := 90;
    end
end;

class function TMARSMessageBodyReaderRegistry.GetInstance: TMARSMessageBodyReaderRegistry;
begin
  if not Assigned(_Instance) then
    _Instance := TMARSMessageBodyReaderRegistry.Create;
  Result := _Instance;
end;

function TMARSMessageBodyReaderRegistry.GetConsumesMediaTypes(
  const AObject: TRttiObject): TMediaTypeList;
var
  LList: TMediaTypeList;
begin
  LList := TMediaTypeList.Create;

  AObject.ForEachAttribute<ConsumesAttribute>(
    procedure (AConsumes: ConsumesAttribute)
    begin
      LList.Add( TMediaType.Create(AConsumes.Value) );
    end
  );

  // if AObject is a method, fall back to its class
  if (LList.Count = 0) and (AObject is TRttiMethod) then
  begin
     (TRttiMethod(AObject).Parent).ForEachAttribute<ConsumesAttribute>(
        procedure (AConsumes: ConsumesAttribute)
        begin
          LList.Add( TMediaType.Create(AConsumes.Value) );
        end
     );
  end;


  Result := LList;
end;

procedure TMARSMessageBodyReaderRegistry.RegisterReader(const AReaderClass: TClass;
  const AIsReadable: TIsReadableFunction; const AGetAffinity: TGetAffinityFunction);
begin
  RegisterReader(
    function : IMessageBodyReader
    var LInstance: TObject;
    begin
      LInstance := AReaderClass.Create;
      if not Supports(LInstance, IMessageBodyReader, Result) then
        raise EMARSException.Create('Interface IMessageBodyReader not implemented');
    end
    , AIsReadable
    , AGetAffinity
    , TRttiContext.Create.GetType(AReaderClass)
  );
end;

procedure TMARSMessageBodyReaderRegistry.RegisterReader(const AReaderClass,
  ASubjectClass: TClass; const AGetAffinity: TGetAffinityFunction);
begin
  RegisterReader(
    AReaderClass,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsObjectOfType(ASubjectClass);
    end,
    AGetAffinity
  );
end;

procedure TMARSMessageBodyReaderRegistry.RegisterReader<T>(const AReaderClass: TClass);
begin
  RegisterReader(
    AReaderClass
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      var
        LType: TRttiType;
      begin
        LType := TRttiContext.Create.GetType(TypeInfo(T));
        Result := False;
        if Assigned(AType) then
        begin
          Result := (AType = LType);
          if not Result then
            Result := AType.IsObjectOfType<T>();
        end;
      end
    , Self.GetDefaultClassAffinityFunc<T>()
  );
end;

procedure TMARSMessageBodyReaderRegistry.RegisterReader(
  const ACreateInstance: TFunc<IMessageBodyReader>;
  const AIsReadable: TIsReadableFunction;
  const AGetAffinity: TGetAffinityFunction;
  AReaderRttiType: TRttiType);
var
  LEntryInfo: TReaderEntryInfo;
begin
  LEntryInfo.CreateInstance := ACreateInstance;
  LEntryInfo.IsReadable := AIsReadable;
  LEntryInfo._RttiType := AReaderRttiType;
  LEntryInfo.RttiName := AReaderRttiType.QualifiedName;
  LEntryInfo.GetAffinity := AGetAffinity;

  FRegistry.Add(LEntryInfo)
end;

end.
