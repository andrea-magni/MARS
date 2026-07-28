(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.DB;

interface

uses
  SysUtils, Classes
, FireDAC.Comp.Client
, MARS.Core.Attributes, MARS.Core.MediaType
, MARS.Data.FireDAC
, MARS.MCP.Data, MARS.MCP.Attributes
;

type
  TEmployeeOperationResult = record
    rowsAffected: Integer;
    message: string;
  end;

  // whole endpoint requires a verified token (Authorization: Bearer <JWT>):
  // [MCPOAuth] answers unauthenticated requests with 401 + WWW-Authenticate so
  // OAuth-capable MCP clients auto-discover the authorization server, while
  // statically issued tokens (POST /rest/default/token, password 'mars')
  // keep working the same way
  [Path('mcpdb'), MCPOAuth
  , MCPServerInfo('MARS Demo MCP DB Server', '1.0.0'
    , 'MCP server exposing a demo EMPLOYEES database (SQLite via FireDAC). '
    + 'Use the tools to browse employees; salary changes require the admin role.')
  ]
  TDemoDBMCPResource = class(TMCPDataResource)
  protected
    [Context] FD: TMARSFireDAC;
  public
    [MCPTool('list_employees', 'Lists all employees with id, name, role, salary and hire date')]
    function ListEmployees: TFDQuery;

    [MCPTool('find_employees', 'Finds employees whose name contains the given text (case-insensitive)')]
    function FindEmployees(
      [MCPParam('nameContains', 'Text to search for in employee names')] const AText: string): TFDQuery;

    [MCPTool('employee_by_id', 'Returns a single employee by its numeric id')]
    function EmployeeById(
      [MCPParam('id', 'Employee id')] const AId: Integer): TFDQuery;

    [RolesAllowed('admin')]
    [MCPTool('raise_salary', 'Raises the salary of an employee by the given percentage (admin only)')]
    function RaiseSalary(
      [MCPParam('id', 'Employee id')] const AId: Integer;
      [MCPParam('percent', 'Percentage increase, e.g. 10 for +10%')] const APercent: Double): TEmployeeOperationResult;
  end;

implementation

uses
  FireDAC.Stan.Param
, MARS.Core.Registry
;

{ TDemoDBMCPResource }

function TDemoDBMCPResource.ListEmployees: TFDQuery;
begin
  Result := FD.Query('select ID, NAME, ROLE, SALARY, HIRED from EMPLOYEES order by ID');
end;

function TDemoDBMCPResource.FindEmployees(const AText: string): TFDQuery;
begin
  Result := FD.Query(
    'select ID, NAME, ROLE, SALARY, HIRED from EMPLOYEES'
    + ' where upper(NAME) like upper(:TXT) order by ID'
  , nil, True
  , procedure (AQuery: TFDQuery)
    begin
      AQuery.ParamByName('TXT').AsString := '%' + AText + '%';
    end
  );
end;

function TDemoDBMCPResource.EmployeeById(const AId: Integer): TFDQuery;
begin
  Result := FD.Query(
    'select ID, NAME, ROLE, SALARY, HIRED from EMPLOYEES where ID = :ID'
  , nil, True
  , procedure (AQuery: TFDQuery)
    begin
      AQuery.ParamByName('ID').AsInteger := AId;
    end
  );
end;

function TDemoDBMCPResource.RaiseSalary(const AId: Integer;
  const APercent: Double): TEmployeeOperationResult;
begin
  Result.rowsAffected := FD.ExecuteSQL(
    'update EMPLOYEES set SALARY = round(SALARY * (1 + :PCT / 100.0), 2) where ID = :ID'
  , nil
  , procedure (ACommand: TFDCommand)
    begin
      ACommand.Params.ParamByName('PCT').AsFloat := APercent;
      ACommand.Params.ParamByName('ID').AsInteger := AId;
    end
  );

  if Result.rowsAffected = 1 then
    Result.message := Format('Salary of employee %d raised by %.1f%%', [AId, APercent], TFormatSettings.Invariant)
  else
    Result.message := Format('No employee found with id %d', [AId]);
end;

initialization
  MARSRegister([TDemoDBMCPResource]);

end.
