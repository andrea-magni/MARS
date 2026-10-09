object MainDataModule: TMainDataModule
  OnCreate = DataModuleCreate
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
    AuthCookieName = 'access_token'
    ProxyConfig.Enabled = False
    ProxyConfig.Port = 0
    HttpClient.UserAgent = 'Embarcadero URI Client/1.0'
    OnLog = MARSClientLog
    Left = 88
    Top = 24
  end
  object CustomersTable: TVirtualTable
    Left = 232
    Top = 24
  end
  object CitiesTable: TVirtualTable
    Left = 232
    Top = 80
  end
end
