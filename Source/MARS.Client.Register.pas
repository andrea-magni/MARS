unit MARS.Client.Register;

interface

procedure Register;

implementation

uses
  Classes, DesignIntf,
  MARS.Client.Client.Indy, MARS.Client.Application,
  MARS.Client.Resource, MARS.Client.Resource.FormData, MARS.Client.Resource.JSON,
  MARS.Client.Resource.Stream, MARS.Client.Token, MARS.Client.Client.Net,
  MARS.Client.CustomResource, MARS.Client.CustomResource.Editor,
  MARS.Client.Resource.FormUrlEncoded;

procedure Register;
begin
  RegisterComponents('MARS-Curiosity Client', [TMARSClient,
                                               TMARSClientApplication,
                                               TMARSClientResource,
                                               TMARSClientResourceFormData,
                                               TMARSClientResourceFormUrlEncoded,
                                               TMARSClientResourceJSON,
                                               TMARSClientResourceStream,
                                               TMARSClientToken,
                                               TMARSNetClient]);
  RegisterComponentEditor(TMARSClientCustomResource, TMARSClientCustomResourceEditor);
end;

end.
