(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

program TailwindcssDemoServerApplication;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Ignition in 'Server.Ignition.pas',
  ServerConst in 'ServerConst.pas',
  Server.Resources.OpenAPI in 'Server.Resources.OpenAPI.pas',
  Server.Resources.Token in 'Server.Resources.Token.pas',
  Server.Resources.OTP in 'Server.Resources.OTP.pas',
  Server.Resources.User in 'Server.Resources.User.pas',
  Server.Resources.Web.Auth in 'Server.Resources.Web.Auth.pas',
  Server.Resources.Web.Users in 'Server.Resources.Web.Users.pas',
  Server.Resources.Web.Static in 'Server.Resources.Web.Static.pas',
  Server.Security.UserPolicy in 'Server.Security.UserPolicy.pas',
  Server.Services.WebRender in 'Server.Services.WebRender.pas',
  Server.Web.Models in 'Server.Web.Models.pas',
  Model.User in 'Model.User.pas',
  UserRepository in 'UserRepository.pas',
  Utils.OTP in 'Utils.OTP.pas',
  Utils.QRCode in 'Utils.QRCode.pas',
  DelphiZXIngQRCode in 'ThirdParty\DelphiZXIngQRCode.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
