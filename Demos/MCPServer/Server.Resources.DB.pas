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

    // MCP resources: readable content the client can attach to the conversation
    [MCPResource('db://schema', 'schema', 'DDL of the demo database: read this to learn tables and columns', 'text/plain')]
    function DbSchema: string;

    [MCPResource('employees://{id}', 'employee', 'A single employee record by numeric id')]
    function EmployeeResource(
      [MCPParam('id', 'Employee id')] const AId: Integer): TFDQuery;

    // MCP prompt: a reusable, guided workflow the user can pick from the client UI
    [MCPPrompt('salary_review', 'Guided salary review for an employee')]
    function SalaryReviewPrompt(
      [MCPParam('employeeName', 'Name (or part of the name) of the employee to review')] const AName: string): string;
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

function TDemoDBMCPResource.DbSchema: string;
var
  LQuery: TFDQuery;
begin
  Result := '';
  LQuery := FD.Query('select sql from sqlite_master where type = ''table'' order by name');
  while not LQuery.Eof do
  begin
    Result := Result + LQuery.Fields[0].AsString + ';' + sLineBreak;
    LQuery.Next;
  end;
end;

function TDemoDBMCPResource.EmployeeResource(const AId: Integer): TFDQuery;
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

function TDemoDBMCPResource.SalaryReviewPrompt(const AName: string): string;
begin
  Result :=
    'Perform a salary review for the employee matching "' + AName + '":' + sLineBreak
    + '1. Use the find_employees tool to locate the employee and note id, role and current salary.' + sLineBreak
    + '2. Use list_employees to compare the salary with colleagues in similar roles.' + sLineBreak
    + '3. Propose a fair adjustment (as a percentage) with a short motivation.' + sLineBreak
    + '4. Only if I explicitly confirm, apply it with the raise_salary tool.';
end;

initialization
  MARSRegister([TDemoDBMCPResource]);

end.
