object MainDataModule: TMainDataModule
  OldCreateOrder = False
  Height = 394
  Width = 605
  object Client: TMARSClient
    MARSEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 0
    ReadTimeout = -1
    AuthCookieName = 'access_token'
    ProxyConfig.Enabled = False
    ProxyConfig.Port = 0
    ProtocolVersion = pv1_1
    HttpClient.ProxyParams.BasicAuthentication = False
    HttpClient.ProxyParams.ProxyPort = 0
    HttpClient.Request.ContentLength = -1
    HttpClient.Request.ContentRangeEnd = -1
    HttpClient.Request.ContentRangeStart = -1
    HttpClient.Request.ContentRangeInstanceLength = -1
    HttpClient.Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    HttpClient.Request.BasicAuthentication = False
    HttpClient.Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    HttpClient.Request.Ranges.Units = 'bytes'
    HttpClient.Request.Ranges = <>
    HttpClient.HTTPOptions = [hoForceEncodeParams]
    Left = 280
    Top = 16
  end
  object DefaultApplication: TMARSClientApplication
    DefaultMediaType = 'application/json'
    DefaultContentType = 'application/json'
    Client = Client
    Left = 280
    Top = 72
  end
  object MetadataResource: TMARSClientResourceJSON
    Application = DefaultApplication
    SpecificAccept = 'application/json'
    SpecificContentType = 'application/json'
    Resource = 'metadata'
    Left = 280
    Top = 136
  end
end
