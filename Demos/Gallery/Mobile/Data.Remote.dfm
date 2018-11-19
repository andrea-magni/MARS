object RemoteData: TRemoteData
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 296
  Width = 441
  object MARSClient: TMARSClient
    MARSEngineURL = 'http://localhost:8080/'
    ConnectTimeout = 0
    ReadTimeout = -1
    ProxyConfig.Enabled = False
    ProxyConfig.Port = 0
    ProtocolVersion = pv1_1
    HttpClient.AllowCookies = True
    HttpClient.ProxyParams.BasicAuthentication = False
    HttpClient.ProxyParams.ProxyPort = 0
    HttpClient.Request.ContentLength = -1
    HttpClient.Request.ContentRangeEnd = -1
    HttpClient.Request.ContentRangeStart = -1
    HttpClient.Request.ContentRangeInstanceLength = -1
    HttpClient.Request.ContentType = 'application/json'
    HttpClient.Request.Accept = 'application/json'
    HttpClient.Request.BasicAuthentication = False
    HttpClient.Request.Host = 'localhost:8080'
    HttpClient.Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    HttpClient.Request.Ranges.Units = 'bytes'
    HttpClient.Request.Ranges = <>
    HttpClient.HTTPOptions = [hoForceEncodeParams]
    Left = 64
    Top = 16
  end
  object CategoriesResource: TMARSClientResourceJSON
    Application = GalleryApplication
    SpecificAccept = 'application/json'
    SpecificContentType = 'application/json'
    Resource = 'category'
    Left = 176
    Top = 88
  end
  object GalleryApplication: TMARSClientApplication
    DefaultMediaType = 'application/json'
    DefaultContentType = 'application/json'
    AppName = 'gallery'
    Client = MARSClient
    OnError = GalleryApplicationError
    Left = 176
    Top = 16
  end
  object CategoryItemsSubResource: TMARSClientResourceJSON
    Application = GalleryApplication
    SpecificAccept = 'application/json'
    SpecificContentType = 'application/json'
    Resource = 'category/main'
    Left = 176
    Top = 144
  end
  object ItemSubResource: TMARSClientResourceStream
    Application = GalleryApplication
    SpecificAccept = '*/*'
    SpecificContentType = 'application/octet-stream'
    Resource = 'category/main'
    PathParamsValues.Strings = (
      'city'
      'IMG_0035.jpg')
    Left = 176
    Top = 200
  end
  object Token: TMARSClientToken
    Application = GalleryApplication
    SpecificAccept = 'application/json'
    SpecificContentType = 'application/json'
    Resource = 'token'
    Left = 280
    Top = 16
  end
end
