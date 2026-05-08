object MainDataModule: TMainDataModule
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 411
  Width = 518
  object MARSApplication: TMARSClientApplication
    DefaultMediaType = 'application/json'
    DefaultContentType = 'application/json'
    Client = MARSHttpClient1
    Left = 88
    Top = 80
  end
  object MARSHttpClient1: TMARSHttpClient
    MARSEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 60000
    ReadTimeout = 60000
    AuthCookieName = 'access_token'
    ProxyConfig.Enabled = False
    ProxyConfig.Port = 0
    Left = 88
    Top = 24
  end
end
