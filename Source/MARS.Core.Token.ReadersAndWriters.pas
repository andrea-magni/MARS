(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Token.ReadersAndWriters;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti

  , MARS.Core.Attributes
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyWriter
  , MARS.Core.MessageBodyReader
  , MARS.Core.Activation.Interfaces
  ;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TMARSTokenWriterJSON = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

//  [Consumes(TMediaType.APPLICATION_JSON)]
//  TMARSTokenReaderJSON = class(TInterfacedObject, IMessageBodyReader)
//  public
//    function ReadFrom(const AInputData: TBytes;
//      const AAttributes: TAttributeArray;
//      AMediaType: TMediaType; ARequestHeaders: TStrings): TValue; virtual;
//  end;


implementation

uses
    Generics.Collections
  , MARS.Core.Utils
  , MARS.Rtti.Utils
  , MARS.Core.Token
  , MARS.Core.JSON
  , MARS.Core.MessageBodyWriters
  , MARS.Utils.Parameters.JSON
  ;


{ TMARSTokenWriterJSON }

procedure TMARSTokenWriterJSON.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LToken: TMARSToken;
  LJSONObj: TJSONObject;
begin
  LToken := AValue.AsObject as TMARSToken;
  if Assigned(LToken) then
  begin
    LJSONObj := TJSONObject.Create;
    try
      LJSONObj.WriteStringValue('Token', LToken.Token);
      LJSONObj.WriteBoolValue('IsVerified', LToken.IsVerified);
      LJSONObj.WriteStringValue('UserName', LToken.UserName);
      LJSONObj.WriteStringValue('Roles', StringArrayToString(LToken.Roles));
      if LToken.Expiration > 0 then
        LJSONObj.WriteDateTimeValue('Expiration', LToken.Expiration);
      if LToken.IssuedAt > 0 then
        LJSONObj.WriteDateTimeValue('IssuedAt', LToken.IssuedAt);
      if LToken.Claims.Count > 0  then
        LJSONObj.AddPair('Claims', LToken.Claims.SaveToJSON);

      TJSONValueWriter.WriteJSONValue(LJSONObj, AMediaType, AOutputStream, AActivation);
    finally
      LJSONObj.Free;
    end;
  end;
end;

procedure RegisterReadersAndWriters;
begin
//  TMARSMessageBodyReaderRegistry.Instance.RegisterReader<TMARSToken>(TMARSTokenReaderJSON);

  TMARSMessageBodyRegistry.Instance.RegisterWriter<TMARSToken>(TMARSTokenWriterJSON);
end;


initialization
  RegisterReadersAndWriters;

end.
