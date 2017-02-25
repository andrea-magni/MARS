(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.MessageBodyWriter;

interface

uses
    Classes, SysUtils, Rtti, Generics.Defaults, Generics.Collections
  , MARS.Core.MediaType
  , MARS.Core.Declarations
  , MARS.Core.Classes
  ;

type
  IMessageBodyWriter = interface
  ['{C22068E1-3085-482D-9EAB-4829C7AE87C0}']
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
  end;

  TIsWritableFunction = reference to function(AType: TRttiType;
    const AAttributes: TAttributeArray; AMediaType: string): Boolean;
  TGetAffinityFunction = reference to function(AType: TRttiType;
    const AAttributes: TAttributeArray; AMediaType: string): Integer;

  TEntryInfo = record
    _RttiType: TRttiType;
    RttiName: string;
    CreateInstance: TFunc<IMessageBodyWriter>;
    IsWritable: TIsWritableFunction;
    GetAffinity: TGetAffinityFunction;
  end;

  TMARSMessageBodyRegistry = class
  private
  private
    FRegistry: TList<TEntryInfo>;
    FRttiContext: TRttiContext;
    class var _Instance: TMARSMessageBodyRegistry;
    class function GetInstance: TMARSMessageBodyRegistry; static;
  protected
    function GetProducesMediaTypes(const AObject: TRttiObject): TMediaTypeList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterWriter(
      const ACreateInstance: TFunc<IMessageBodyWriter>;
      const AIsWritable: TIsWritableFunction;
      const AGetAffinity: TGetAffinityFunction;
      AWriterRttiType: TRttiType); overload;

    procedure RegisterWriter(
      const AWriterClass: TClass;
      const AIsWritable: TIsWritableFunction;
      const AGetAffinity: TGetAffinityFunction); overload;

    procedure RegisterWriter(const AWriterClass: TClass; const ASubjectClass: TClass;
      const AGetAffinity: TGetAffinityFunction); overload;

    procedure RegisterWriter<T: class>(const AWriterClass: TClass); overload;

    procedure FindWriter(const AMethod: TRttiMethod; const AAccept: string;
      out AWriter: IMessageBodyWriter; out AMediaType: TMediaType);

    procedure Enumerate(const AProc: TProc<TEntryInfo>);

    class property Instance: TMARSMessageBodyRegistry read GetInstance;
    class function GetDefaultClassAffinityFunc<T: class>: TGetAffinityFunction;
    class destructor ClassDestructor;

    const AFFINITY_HIGH = 30;
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

{ TMARSMessageBodyRegistry }

class destructor TMARSMessageBodyRegistry.ClassDestructor;
begin
  if Assigned(_Instance) then
    FreeAndNil(_Instance);
end;

constructor TMARSMessageBodyRegistry.Create;
begin
  inherited Create;

  FRegistry := TList<TEntryInfo>.Create;
  FRttiContext := TRttiContext.Create;
end;

destructor TMARSMessageBodyRegistry.Destroy;
begin
  FRegistry.Free;
  inherited;
end;

procedure TMARSMessageBodyRegistry.Enumerate(const AProc: TProc<TEntryInfo>);
var
  LEntry: TEntryInfo;
begin
  for LEntry in FRegistry do
    AProc(LEntry);
end;

procedure TMARSMessageBodyRegistry.FindWriter(const AMethod: TRttiMethod;
  const AAccept: string;
  out AWriter: IMessageBodyWriter; out AMediaType: TMediaType);
var
  LWriterEntry: TEntryInfo;
  LFound: Boolean;
  LCandidateAffinity: Integer;
  LCandidate: TEntryInfo;
  LWriterRttiType: TRttiType;
  LAcceptParser: TAcceptParser;

  LAcceptMediaTypes, LWriterMediaTypes: TMediaTypeList;
  LMethodProducesMediaTypes: TMediaTypeList;
  LAllowedMediaTypes: TArray<string>;
  LMediaTypes: TArray<string>;
  LMediaType: string;
  LCandidateMediaType: string;
  LCandidateQualityFactor: Double;

begin
  AMediaType := nil;
  AWriter := nil;
  LFound := False;
  LCandidateAffinity := -1;
  LCandidateMediaType := '';
  LCandidateQualityFactor := -1;
  if not Assigned(AMethod.ReturnType) then
    Exit; // no serialization (it's a procedure!)


  // consider client's Accept
  LAcceptParser := TAcceptParser.Create(AAccept);
  try
    LAcceptMediaTypes := LAcceptParser.MediaTypeList;

    // consider method's Produces
    LMethodProducesMediaTypes := GetProducesMediaTypes(AMethod);
    try
      if LMethodProducesMediaTypes.Count > 0 then
        LAllowedMediaTypes := TMediaTypeList.Intersect(LAcceptMediaTypes, LMethodProducesMediaTypes)
      else
        LAllowedMediaTypes := LAcceptMediaTypes.ToArrayOfString;

        if (Length(LAllowedMediaTypes) = 0)
          or ((Length(LAllowedMediaTypes) = 1) and (LAllowedMediaTypes[0] = TMediaType.WILDCARD))
        then // defaults
        begin
          if LMethodProducesMediaTypes.Count > 0 then
            LAllowedMediaTypes := LMethodProducesMediaTypes.ToArrayOfString
          else
          begin
            SetLength(LAllowedMediaTypes, 2);
            LAllowedMediaTypes[0] := TMediaType.APPLICATION_JSON;
            LAllowedMediaTypes[1] := TMediaType.WILDCARD;
          end;
        end;

        // collect compatible writers
        for LWriterEntry in FRegistry do
        begin
          LWriterRttiType := FRttiContext.FindType(LWriterEntry.RttiName);
          LWriterMediaTypes := GetProducesMediaTypes(LWriterRttiType);
          try
            if LWriterMediaTypes.Contains(TMediaType.WILDCARD) then
              LMediaTypes := LAllowedMediaTypes
            else
              LMediaTypes := TMediaTypeList.Intersect(LAllowedMediaTypes, LWriterMediaTypes);
            for LMediaType in LMediaTypes do
              if LWriterEntry.IsWritable(AMethod.ReturnType, AMethod.GetAttributes, LMediaType) then
              begin
                if not LFound
                   or (
                     (LCandidateAffinity < LWriterEntry.GetAffinity(AMethod.ReturnType, AMethod.GetAttributes, LMediaType))
                     or (LCandidateQualityFactor < LAcceptMediaTypes.GetQualityFactor(LMediaType))
                   )
                then
                begin
                  LCandidate := LWriterEntry;
                  LCandidateAffinity := LCandidate.GetAffinity(AMethod.ReturnType, AMethod.GetAttributes, LMediaType);
                  LCandidateMediaType := LMediaType;
                  LCandidateQualityFactor := LAcceptMediaTypes.GetQualityFactor(LMediaType);
                  LFound := True;
                end;
              end;
          finally
            LWriterMediaTypes.Free;
          end;
        end;

        if LFound then
        begin
          AWriter := LCandidate.CreateInstance();
          AMediaType := TMediaType.Create(LCandidateMediaType);
        end;
    finally
      LMethodProducesMediaTypes.Free;
    end;
  finally
    LAcceptParser.Free;
  end;
end;

class function TMARSMessageBodyRegistry.GetDefaultClassAffinityFunc<T>: TGetAffinityFunction;
begin
  Result :=
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      if Assigned(AType) and AType.IsObjectOfType<T>(False) then
        Result := 100
      else if Assigned(AType) and AType.IsObjectOfType<T> then
        Result := 99
      else
        Result := 0;
    end
end;

class function TMARSMessageBodyRegistry.GetInstance: TMARSMessageBodyRegistry;
begin
  if not Assigned(_Instance) then
    _Instance := TMARSMessageBodyRegistry.Create;
  Result := _Instance;
end;

function TMARSMessageBodyRegistry.GetProducesMediaTypes(
  const AObject: TRttiObject): TMediaTypeList;
var
  LList: TMediaTypeList;
begin
  LList := TMediaTypeList.Create;

  AObject.ForEachAttribute<ProducesAttribute>(
    procedure (AProduces: ProducesAttribute)
    begin
      LList.Add( TMediaType.Create(AProduces.Value) );
    end
  );

  // if AObject is a method, fall back to its class
  if (LList.Count = 0) and (AObject is TRttiMethod) then
  begin
     (TRttiMethod(AObject).Parent).ForEachAttribute<ProducesAttribute>(
        procedure (AProduces: ProducesAttribute)
        begin
          LList.Add( TMediaType.Create(AProduces.Value) );
        end
     );
  end;


  Result := LList;
end;

procedure TMARSMessageBodyRegistry.RegisterWriter(const AWriterClass: TClass;
  const AIsWritable: TIsWritableFunction; const AGetAffinity: TGetAffinityFunction);
begin
  RegisterWriter(
    function : IMessageBodyWriter
    var LInstance: TObject;
    begin
      LInstance := AWriterClass.Create;
      if not Supports(LInstance, IMessageBodyWriter, Result) then
        raise EMARSException.Create('Interface IMessageBodyWriter not implemented');
    end
    , AIsWritable
    , AGetAffinity
    , TRttiContext.Create.GetType(AWriterClass)
  );
end;

procedure TMARSMessageBodyRegistry.RegisterWriter(const AWriterClass,
  ASubjectClass: TClass; const AGetAffinity: TGetAffinityFunction);
begin
  RegisterWriter(
    AWriterClass,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsObjectOfType(ASubjectClass);
    end,
    AGetAffinity
  );
end;

procedure TMARSMessageBodyRegistry.RegisterWriter<T>(const AWriterClass: TClass);
begin
  RegisterWriter(
    AWriterClass
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and AType.IsObjectOfType<T>;
      end
    , Self.GetDefaultClassAffinityFunc<T>()
  );
end;

procedure TMARSMessageBodyRegistry.RegisterWriter(
  const ACreateInstance: TFunc<IMessageBodyWriter>;
  const AIsWritable: TIsWritableFunction;
  const AGetAffinity: TGetAffinityFunction;
  AWriterRttiType: TRttiType);
var
  LEntryInfo: TEntryInfo;
begin
  LEntryInfo.CreateInstance := ACreateInstance;
  LEntryInfo.IsWritable := AIsWritable;
  LEntryInfo._RttiType := AWriterRttiType;
  LEntryInfo.RttiName := AWriterRttiType.QualifiedName;
  LEntryInfo.GetAffinity := AGetAffinity;

  FRegistry.Add(LEntryInfo)
end;

end.
