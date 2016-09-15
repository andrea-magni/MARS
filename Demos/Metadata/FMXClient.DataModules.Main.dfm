object MainDataModule: TMainDataModule
  OldCreateOrder = False
  Height = 394
  Width = 605
  object Client: TMARSClient
    MARSEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 0
    ReadTimeout = -1
    Left = 280
    Top = 16
  end
  object DefaultApplication: TMARSClientApplication
    DefaultMediaType = 'application/json'
    AppName = 'default'
    Client = Client
    Left = 280
    Top = 72
  end
  object MetadataResource: TMARSClientResourceJSON
    Application = DefaultApplication
    Resource = 'metadata'
    Left = 280
    Top = 136
  end
end
