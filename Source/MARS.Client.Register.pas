unit MARS.Client.Register;

interface

procedure Register;

implementation

uses
  Classes, DesignIntf,
  MARS.Client.Client.Indy, MARS.Client.Application, MARS.Client.Messaging.Resource,
  MARS.Client.Resource, MARS.Client.Resource.FormData, MARS.Client.Resource.JSON,
  MARS.Client.Resource.Stream, MARS.Client.SubResource, MARS.Client.SubResource.JSON,
  MARS.Client.SubResource.Stream, MARS.Client.Token, MARS.Client.Client.Net,
  MARS.Client.CustomResource, MARS.Client.CustomResource.Editor;

procedure Register;
begin
  RegisterComponents('MARS-Curiosity Client', [TMARSClient,
                                               TMARSClientApplication,
                                               TMARSClientMessagingResource,
                                               TMARSClientResource,
                                               TMARSClientResourceFormData,
                                               TMARSClientResourceJSON,
                                               TMARSClientResourceStream,
                                               TMARSClientSubResource,
                                               TMARSClientSubResourceJSON,
                                               TMARSClientSubResourceStream,
                                               TMARSClientToken,
                                               TMARSNetClient]);
  RegisterComponentEditor(TMARSClientCustomResource, TMARSClientCustomResourceEditor);
end;

end.
