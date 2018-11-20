object MainDataModule: TMainDataModule
  OldCreateOrder = False
  Height = 411
  Width = 518
  object MARSApplication: TMARSClientApplication
    DefaultMediaType = 'application/json'
    DefaultContentType = 'application/json'
    Client = MARSClient
    Left = 88
    Top = 80
  end
  object MARSClient: TMARSNetClient
    MARSEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 60000
    ReadTimeout = 60000
    ProxyConfig.Enabled = False
    ProxyConfig.Port = 0
    HttpClient.Asynchronous = False
    HttpClient.ConnectionTimeout = 60000
    HttpClient.ResponseTimeout = 60000
    HttpClient.AllowCookies = True
    HttpClient.HandleRedirects = True
    HttpClient.Accept = '*/*'
    HttpClient.ContentType = 'multipart/form-data'
    HttpClient.UserAgent = 'Embarcadero URI Client/1.0'
    Left = 88
    Top = 24
  end
  object HelloWorldRes1: TMARSClientResourceFormData
    Application = MARSApplication
    SpecificAccept = '*/*'
    SpecificContentType = 'multipart/form-data'
    Resource = 'helloworld/1'
    Left = 88
    Top = 152
  end
end
