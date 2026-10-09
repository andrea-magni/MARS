(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Database;

interface

uses
  MARS.Utils.Parameters
;

type
  /// <summary> Creates the CUSTOMERS table of the demo, with a few rows, when it does not
  /// exist. The MySQL/MariaDB database itself must exist (see UniDAC.MAIN_DB.Database in
  /// Server.ini) </summary>
  TDemoDatabase = class
  public
    /// <summary> Before the connection definitions are loaded </summary>
    class procedure Prepare(const AParameters: TMARSParameters);
    class procedure Setup(const AConnectionDefName: string);
  end;

implementation

uses
  System.SysUtils
, Uni, MySQLUniProvider // the provider of the database (Provider Name in Server.ini)
, MARS.Data.UniDAC
;

class procedure TDemoDatabase.Prepare(const AParameters: TMARSParameters);
begin
  // nothing to prepare for MySQL/MariaDB (the Firebird demos expand {bin} in the file name of
  // the database)
end;

class procedure TDemoDatabase.Setup(const AConnectionDefName: string);
var
  LUniDAC: TMARSUniDAC;
  LCount: Integer;
begin
  // no activation here: TMARSUniDAC works without one, as long as the statements do not use
  // PathParam_*, QueryParam_*, Token_* and the like
  LUniDAC := TMARSUniDAC.Create(AConnectionDefName);
  try
    LUniDAC.ExecuteSQL(
        'create table if not exists customers ('
      + '  id integer not null auto_increment primary key,'
      + '  name varchar(100) not null,'
      + '  city varchar(50),'
      + '  credit decimal(12,2) not null default 0'
      + ')'
    );

    LCount := 0;
    LUniDAC.Query('select count(*) from customers', nil, nil,
      procedure (AQuery: TUniQuery)
      begin
        LCount := AQuery.Fields[0].AsInteger;
      end
    );

    if LCount = 0 then
      LUniDAC.ExecuteSQL(
          'insert into customers (name, city, credit) values'
        + ' (''Ada Lovelace'', ''London'', 1500),'
        + ' (''Alan Turing'', ''London'', 800),'
        + ' (''Grace Hopper'', ''New York'', 2300),'
        + ' (''Niklaus Wirth'', ''Zurich'', 1200),'
        + ' (''Anders Hejlsberg'', ''Copenhagen'', 3100),'
        + ' (''Edsger Dijkstra'', ''Amsterdam'', 950)'
      );
  finally
    LUniDAC.Free;
  end;
end;

end.
