(*
  Copyright 2026, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.MCP.Attributes;

{$I MARS.inc}

interface

uses
  Classes, SysUtils
, MARS.Core.Attributes
;

type
  // Declares MCP server identity (initialize response). Apply to a TMCPResource descendant.
  MCPServerInfoAttribute = class(MARSAttribute)
  private
    FServerName: string;
    FVersion: string;
    FInstructions: string;
  public
    constructor Create(const AServerName: string; const AVersion: string = '1.0.0';
      const AInstructions: string = '');

    property ServerName: string read FServerName;
    property Version: string read FVersion;
    property Instructions: string read FInstructions;
  end;

  // Marks a public method of a TMCPResource descendant as an MCP tool.
  MCPToolAttribute = class(MARSAttribute)
  private
    FToolName: string;
    FDescription: string;
  public
    constructor Create(const ADescription: string); overload;
    constructor Create(const AToolName, ADescription: string); overload;

    property ToolName: string read FToolName;
    property Description: string read FDescription;
  end;

  // Marks a TMCPResource descendant as OAuth-protected: unauthenticated requests
  // are answered with 401 and a WWW-Authenticate header carrying the protected
  // resource metadata URL (MCP authorization discovery). See MARS.MCP.OAuth.
  MCPOAuthAttribute = class(MARSAttribute);

  // Documents (and optionally renames) a tool parameter in the generated JSON Schema.
  MCPParamAttribute = class(MARSAttribute)
  private
    FParamName: string;
    FDescription: string;
  public
    constructor Create(const ADescription: string); overload;
    constructor Create(const AParamName, ADescription: string); overload;

    property ParamName: string read FParamName;
    property Description: string read FDescription;
  end;

implementation

{ MCPServerInfoAttribute }

constructor MCPServerInfoAttribute.Create(const AServerName, AVersion,
  AInstructions: string);
begin
  inherited Create;
  FServerName := AServerName;
  FVersion := AVersion;
  FInstructions := AInstructions;
end;

{ MCPToolAttribute }

constructor MCPToolAttribute.Create(const ADescription: string);
begin
  Create('', ADescription);
end;

constructor MCPToolAttribute.Create(const AToolName, ADescription: string);
begin
  inherited Create;
  FToolName := AToolName;
  FDescription := ADescription;
end;

{ MCPParamAttribute }

constructor MCPParamAttribute.Create(const ADescription: string);
begin
  Create('', ADescription);
end;

constructor MCPParamAttribute.Create(const AParamName, ADescription: string);
begin
  inherited Create;
  FParamName := AParamName;
  FDescription := ADescription;
end;

end.
