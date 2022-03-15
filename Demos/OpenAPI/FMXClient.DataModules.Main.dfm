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
    HttpClient.Asynchronous = False
    HttpClient.ConnectionTimeout = 60000
    HttpClient.ResponseTimeout = 60000
    HttpClient.AllowCookies = True
    HttpClient.HandleRedirects = True
    HttpClient.UserAgent = 'Embarcadero URI Client/1.0'
    Left = 88
    Top = 24
  end
end
