unit MARS.Client.Register;

interface

procedure Register;

implementation

uses
  Classes, DesignIntf,
  MARS.Client.Client.Indy, MARS.Client.Application,
  MARS.Client.Resource, MARS.Client.Resource.FormData, MARS.Client.Resource.JSON,
  MARS.Client.Resource.Stream, MARS.Client.Token, MARS.Client.Client.Net,
  MARS.Client.CustomResource, MARS.Client.CustomResource.Editor;

procedure Register;
begin
  RegisterComponents('MARS-Curiosity Client', [TMARSClient,
                                               TMARSClientApplication,
                                               TMARSClientResource,
                                               TMARSClientResourceFormData,
                                               TMARSClientResourceJSON,
                                               TMARSClientResourceStream,
                                               TMARSClientToken,
                                               TMARSNetClient]);
  RegisterComponentEditor(TMARSClientCustomResource, TMARSClientCustomResourceEditor);
end;

end.
