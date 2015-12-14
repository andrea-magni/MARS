object MainDataModule: TMainDataModule
  OldCreateOrder = False
  Height = 411
  Width = 518
  object MARSClient: TMARSClient
    MARSEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 0
    ReadTimeout = -1
    Left = 88
    Top = 24
  end
  object MARSApplication: TMARSClientApplication
    DefaultMediaType = 'application/json'
    AppName = 'default'
    Client = MARSClient
    Left = 88
    Top = 80
  end
end
