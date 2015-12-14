(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
program FireDACBasicClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  Forms.Main in 'Forms.Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
