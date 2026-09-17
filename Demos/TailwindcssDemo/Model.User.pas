(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Model.User;

interface

uses
  Classes, SysUtils
, MARS.Core.Attributes, MARS.Core.JSON
;

type
  TUser = record
    [JSONName('id')] Id: Integer;
    [JSONName('userName')] UserName: string;
    [JSONName('realName')] RealName: string;
    [JSONName('lang')] Lang: string;
    [JSONName('active')] Is_Active: Boolean;
    [JSONName('')] password_Hash: string; // this will never reach the JSON representation
  end;


implementation

end.
