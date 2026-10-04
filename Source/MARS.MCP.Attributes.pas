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

const
  // MCP Apps (extension io.modelcontextprotocol/ui): MIME type of an HTML view
  MCP_APP_MIME_TYPE = 'text/html;profile=mcp-app';
  MCP_APP_URI_SCHEME = 'ui://';

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

  // Exposes a method of a TMCPResource descendant as an MCP resource (readable
  // content identified by a URI). URIs containing {param} placeholders are
  // listed as resource templates and the placeholders bind to method parameters.
  MCPResourceAttribute = class(MARSAttribute)
  private
    FURI: string;
    FResourceName: string;
    FDescription: string;
    FMimeType: string;
  public
    constructor Create(const AURI, ADescription: string); overload;
    constructor Create(const AURI, AResourceName, ADescription: string); overload;
    constructor Create(const AURI, AResourceName, ADescription, AMimeType: string); overload;

    property URI: string read FURI;
    property ResourceName: string read FResourceName;
    property Description: string read FDescription;
    property MimeType: string read FMimeType;
  end;

  // Exposes a method of a TMCPResource descendant as an MCP prompt (reusable
  // prompt template). Method parameters become the prompt arguments.
  MCPPromptAttribute = class(MARSAttribute)
  private
    FPromptName: string;
    FDescription: string;
  public
    constructor Create(const ADescription: string); overload;
    constructor Create(const APromptName, ADescription: string); overload;

    property PromptName: string read FPromptName;
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

  // Makes a tool parameter optional: it is left out of the "required" list, the
  // JSON Schema advertises the value as "default", and a missing (or null)
  // argument is bound to it. ADefaultJSON is a JSON literal converted like any
  // argument: 'true', '7', '"value_date"', '""', '[]'.
  MCPDefaultAttribute = class(MARSAttribute)
  private
    FDefaultJSON: string;
  public
    constructor Create(const ADefaultJSON: string);

    property DefaultJSON: string read FDefaultJSON;
  end;

  // Adds metadata to the "_meta" of a tool or resource (tools/list, resources/list,
  // resources/templates/list and resources/read contents). AMetaJSON is a JSON object
  // literal, e.g. '{"ui":{"permissions":{"clipboardWrite":{}}}}'. Several attributes
  // are merged; the MCPToolUI / MCPApp* attributes below are applied on top.
  MCPMetaAttribute = class(MARSAttribute)
  private
    FMetaJSON: string;
  public
    constructor Create(const AMetaJSON: string);

    property MetaJSON: string read FMetaJSON;
  end;

  // MCP Apps: links a tool to the UI resource (an MCPAppResource) that renders its
  // results: _meta.ui.resourceUri. AVisibility, comma separated, sets _meta.ui.visibility:
  // 'model,app' (the default when empty), 'app' (callable by the view only, hidden from
  // the model) or 'model'.
  MCPToolUIAttribute = class(MARSAttribute)
  private
    FResourceURI: string;
    FVisibility: string;
  public
    constructor Create(const AResourceURI: string; const AVisibility: string = '');

    property ResourceURI: string read FResourceURI;
    property Visibility: string read FVisibility;
  end;

  // MCP Apps: a UI resource, an MCPResource with a ui:// URI whose method returns the
  // HTML document of the view (MIME type text/html;profile=mcp-app).
  MCPAppResourceAttribute = class(MCPResourceAttribute)
  public
    constructor Create(const AURI, ADescription: string); overload;
    constructor Create(const AURI, AResourceName, ADescription: string); overload;
  end;

  // MCP Apps: origins the view of a UI resource may reach (_meta.ui.csp), each a comma
  // separated list. Without it the host blocks every external origin.
  //   AConnectDomains   fetch/XHR/WebSocket (connect-src)
  //   AResourceDomains  scripts, styles, images, fonts, media (script-src, style-src, ...)
  //   AFrameDomains     nested iframes (frame-src)
  //   ABaseUriDomains   base URIs of the document (base-uri)
  MCPAppCSPAttribute = class(MARSAttribute)
  private
    FConnectDomains: string;
    FResourceDomains: string;
    FFrameDomains: string;
    FBaseUriDomains: string;
  public
    constructor Create(const AConnectDomains: string; const AResourceDomains: string = '';
      const AFrameDomains: string = ''; const ABaseUriDomains: string = '');

    property ConnectDomains: string read FConnectDomains;
    property ResourceDomains: string read FResourceDomains;
    property FrameDomains: string read FFrameDomains;
    property BaseUriDomains: string read FBaseUriDomains;
  end;

  // MCP Apps: whether the host should draw a border and background around the view of a
  // UI resource (_meta.ui.prefersBorder). Without it the host decides.
  MCPAppBorderAttribute = class(MARSAttribute)
  private
    FPrefersBorder: Boolean;
  public
    constructor Create(const APrefersBorder: Boolean);

    property PrefersBorder: Boolean read FPrefersBorder;
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

{ MCPResourceAttribute }

constructor MCPResourceAttribute.Create(const AURI, ADescription: string);
begin
  Create(AURI, '', ADescription, '');
end;

constructor MCPResourceAttribute.Create(const AURI, AResourceName, ADescription: string);
begin
  Create(AURI, AResourceName, ADescription, '');
end;

constructor MCPResourceAttribute.Create(const AURI, AResourceName, ADescription, AMimeType: string);
begin
  inherited Create;
  FURI := AURI;
  FResourceName := AResourceName;
  FDescription := ADescription;
  FMimeType := AMimeType;
end;

{ MCPPromptAttribute }

constructor MCPPromptAttribute.Create(const ADescription: string);
begin
  Create('', ADescription);
end;

constructor MCPPromptAttribute.Create(const APromptName, ADescription: string);
begin
  inherited Create;
  FPromptName := APromptName;
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

{ MCPDefaultAttribute }

constructor MCPDefaultAttribute.Create(const ADefaultJSON: string);
begin
  inherited Create;
  FDefaultJSON := ADefaultJSON;
end;

{ MCPMetaAttribute }

constructor MCPMetaAttribute.Create(const AMetaJSON: string);
begin
  inherited Create;
  FMetaJSON := AMetaJSON;
end;

{ MCPToolUIAttribute }

constructor MCPToolUIAttribute.Create(const AResourceURI, AVisibility: string);
begin
  inherited Create;
  FResourceURI := AResourceURI;
  FVisibility := AVisibility;
end;

{ MCPAppResourceAttribute }

constructor MCPAppResourceAttribute.Create(const AURI, ADescription: string);
begin
  Create(AURI, '', ADescription);
end;

constructor MCPAppResourceAttribute.Create(const AURI, AResourceName, ADescription: string);
begin
  inherited Create(AURI, AResourceName, ADescription, MCP_APP_MIME_TYPE);
end;

{ MCPAppCSPAttribute }

constructor MCPAppCSPAttribute.Create(const AConnectDomains, AResourceDomains,
  AFrameDomains, ABaseUriDomains: string);
begin
  inherited Create;
  FConnectDomains := AConnectDomains;
  FResourceDomains := AResourceDomains;
  FFrameDomains := AFrameDomains;
  FBaseUriDomains := ABaseUriDomains;
end;

{ MCPAppBorderAttribute }

constructor MCPAppBorderAttribute.Create(const APrefersBorder: Boolean);
begin
  inherited Create;
  FPrefersBorder := APrefersBorder;
end;

end.
