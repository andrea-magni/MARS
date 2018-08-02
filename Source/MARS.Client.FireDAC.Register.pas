unit MARS.Client.FireDAC.Register;

interface

procedure Register;

implementation

uses
  Classes, DesignIntf
, MARS.Client.FireDAC, MARS.Data.FireDAC.Editor;

procedure Register;
begin
  RegisterComponents('MARS-Curiosity Client', [TMARSFDResource, TMARSFDDataSetResource]);
  RegisterComponentEditor(TMARSFDResource, TMARSFDResourceEditor);
//  RegisterComponentEditor(TMARSFDDataSetResource, TMARSFDResourceEditor);
end;

end.
