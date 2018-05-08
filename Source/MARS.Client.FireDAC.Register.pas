unit MARS.Client.FireDAC.Register;

interface

procedure Register;

implementation

uses
  Classes, DesignIntf,
  MARS.Client.FireDAC, MARS.Data.FireDAC.Editor;

procedure Register;
begin
  RegisterComponents('MARS-Curiosity Client', [TMARSFDResource]);
  RegisterComponentEditor(TMARSFDResource, TMARSFDResourceEditor);
end;

end.
