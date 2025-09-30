unit MARS.Client.Register;

interface

{$I MARS.inc}
{$R ..\MARSSplash.res}

procedure Register;

implementation

uses
  System.Classes, System.SysUtils, DesignIntf, ToolsAPI
, BrandingAPI, VCL.Graphics, Vcl.Imaging.pngimage, System.Types, Winapi.Windows
, MARS.Client.Client.Indy, MARS.Client.Client.Net
, MARS.Client.Application
, MARS.Client.CustomResource, MARS.Client.CustomResource.Editor
, MARS.Client.Resource, MARS.Client.Resource.FormData, MARS.Client.Resource.JSON
, MARS.Client.Resource.Stream, MARS.Client.Resource.FormUrlEncoded
, MARS.Client.Token
;

const
  MARSVersion = '1.6.1';
  Component_Docs_URL = 'https://github.com/andrea-magni/MARS/Wiki/';
  {$IFDEF Delphi11Alexandria_UP}
  ABOUT_RES_NAME = 'MARSSPLASH48PNG';
  SPLASH_RES_NAME = 'MARSSPLASH48PNG';
  {$ELSE}
  ABOUT_RES_NAME = 'MARSSPLASH24BMP';
  SPLASH_RES_NAME = 'MARSSPLASH24BMP';
  {$ENDIF}
  RsAboutTitle = 'MARS-Curiosity REST Library';
  RsAboutDescription = 'MARS-Curiosity REST Library - Client library Components - https://github.com/andrea-magni/MARS' + sLineBreak +
    'Delphi components implementing REST client library for MARS Application as well as standard REST applications.';
  RsAboutLicense = 'Apache 2.0 (Free/Opensource)';
var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer;


{$IFDEF Delphi11Alexandria_UP}
function CreateBitmapFromPngRes(const AResName: string): Vcl.Graphics.TBitmap;
var
  LPngImage: TPngImage;
  LResStream: TResourceStream;
begin
  LPngImage := nil;
  try
    Result := Vcl.Graphics.TBitmap.Create;
    LPngImage := TPngImage.Create;
    LResStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
    try
      LPngImage.LoadFromStream(LResStream);
      Result.Assign(LPngImage);
    finally
      LResStream.Free;
    end;
  finally
    LPngImage.Free;
  end;
end;

procedure RegisterAboutBox;
var
  LBitmap: Vcl.Graphics.TBitmap;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  LBitmap := CreateBitmapFromPngRes(ABOUT_RES_NAME);
  try
    AboutBoxIndex := AboutBoxServices.AddPluginInfo(
      RsAboutTitle+' '+MARSVersion,
      RsAboutDescription, LBitmap.Handle, False, RsAboutLicense);
  finally
    LBitmap.Free;
  end;
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterWithSplashScreen;
var
  LBitmap: Vcl.Graphics.TBitmap;
begin
  LBitmap := CreateBitmapFromPngRes(SPLASH_RES_NAME);
  try
    SplashScreenServices.AddPluginBitmap(
      RsAboutTitle + ' ' + MARSVersion,
      LBitmap.Handle, False, RsAboutLicense, '');
  finally
    LBitmap.Free;
  end;
end;
{$ELSE}
procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), ABOUT_RES_NAME);
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(RsAboutTitle+' '+MARSVersion,
    RsAboutDescription, ProductImage, False, RsAboutLicense);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterWithSplashScreen;
var
  ProductImage: HBITMAP;
begin
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), SPLASH_RES_NAME);
  SplashScreenServices.AddPluginBitmap(RsAboutTitle, ProductImage,
    False, RsAboutLicense);
end;
{$ENDIF}


procedure Register;
begin
  RegisterWithSplashScreen;
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

initialization
  RegisterAboutBox;

finalization
  UnRegisterAboutBox;


end.
