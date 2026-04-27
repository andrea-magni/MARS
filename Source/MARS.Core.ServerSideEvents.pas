(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit MARS.Core.ServerSideEvents;

interface

uses
  Classes, SysUtils, System.JSON
, Web.HttpApp
, MARS.Core.JSON
;

type
  TMARSWorkerProc = TProc<TWebResponseStream>;

  TMARSServerSideEvent = record
    KeepAliveTimeout: Integer;
    Worker: TProc<TWebResponseStream>;

    constructor Create(const AWorker: TMARSWorkerProc;
      const AKeepAliveTimeout: Integer = 15000  // default
    );
  end;


  TMARSWebResponseStream = class helper for TWebResponseStream

    // JSON values for Data
    procedure WriteData(const AValue: TJSONValue); overload;

    // Record->JSON values for Data
    procedure WriteData<R: record>(const ARecord: R;
      const AFilterProc: TToJSONFilterProc = nil;
      const AFormatJSON: Boolean = False;
      const AFormatIndentation: Integer = 4); overload;
    procedure WriteData<R: record>(const ARecord: R;
      const AOptions: TMARSJSONSerializationOptions;
      const AFilterProc: TToJSONFilterProc = nil;
      const AFormatJSON: Boolean = False;
      const AFormatIndentation: Integer = 4); overload;

    // ID and Data
    procedure Write(const AId: string; const AData: string); overload;
    procedure Write(const AId: string; const AData: TJSONValue); overload;
    procedure Write<R: record>(const AId: string;
      const AData: R;
      const AFilterProc: TToJSONFilterProc = nil;
      const AFormatJSON: Boolean = False;
      const AFormatIndentation: Integer = 4); overload;
    procedure Write<R: record>(const AId: string;
      const AData: R;
      const AOptions: TMARSJSONSerializationOptions;
      const AFilterProc: TToJSONFilterProc = nil;
      const AFormatJSON: Boolean = False;
      const AFormatIndentation: Integer = 4); overload;

  end;


implementation

uses
  MARS.Core.ServerSideEvents.MessageBodyWriters;

{ TMARSServerSideEvent }

constructor TMARSServerSideEvent.Create(const AWorker: TMARSWorkerProc;
  const AKeepAliveTimeout: Integer);
begin
  Worker := AWorker;
  KeepAliveTimeout := AKeepAliveTimeout;
end;

{ TMARSWebResponseStream }

procedure TMARSWebResponseStream.WriteData<R>(const ARecord: R;
  const AFilterProc: TToJSONFilterProc;
  const AFormatJSON: Boolean;
  const AFormatIndentation: Integer);
begin
  WriteData<R>(ARecord, DefaultMARSJSONSerializationOptions, AFilterProc, AFormatJSON, AFormatIndentation);
end;

procedure TMARSWebResponseStream.Write(const AId, AData: string);
begin
  WriteID(AId);
  WriteData(AData);
end;

procedure TMARSWebResponseStream.Write(const AId: string;
  const AData: TJSONValue);
begin
  WriteID(AId);
  WriteData(AData);
end;

procedure TMARSWebResponseStream.Write<R>(const AId: string; const AData: R;
  const AOptions: TMARSJSONSerializationOptions;
  const AFilterProc: TToJSONFilterProc; const AFormatJSON: Boolean;
  const AFormatIndentation: Integer);
begin
  WriteID(AId);
  WriteData<R>(AData, AOptions, AFilterProc, AFormatJSON, AFormatIndentation);
end;

procedure TMARSWebResponseStream.Write<R>(const AId: string; const AData: R;
  const AFilterProc: TToJSONFilterProc; const AFormatJSON: Boolean;
  const AFormatIndentation: Integer);
begin
  Write<R>(AId, AData, DefaultMARSJSONSerializationOptions, AFilterProc, AFormatJSON, AFormatIndentation);
end;

procedure TMARSWebResponseStream.WriteData<R>(const ARecord: R;
  const AOptions: TMARSJSONSerializationOptions;
  const AFilterProc: TToJSONFilterProc;
  const AFormatJSON: Boolean;
  const AFormatIndentation: Integer);
var
  LJSONString: string;
begin
  LJSONString := TJSONObject.RecordToJSONString<R>(ARecord, AOptions, AFilterProc, AFormatJSON, AFormatIndentation);
  WriteData(LJSONString);
end;

procedure TMARSWebResponseStream.WriteData(const AValue: TJSONValue);
begin
  WriteData(AValue.ToJSON);
end;

end.
