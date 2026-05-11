object MainDataModule: TMainDataModule
  OnCreate = DataModuleCreate
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
    BaseURL = 'http://localhost:8080/rest'
    MARSEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 60000
    ReadTimeout = 60000
    AuthCookieName = 'access_token'
    ProxyConfig.Enabled = False
    ProxyConfig.Port = 0
    Left = 88
    Top = 24
  end
  object MARSClientResourceSSE1: TMARSClientResourceSSE
    Application = MARSApplication
    SpecificAccept = '*/*'
    Resource = 'helloworld'
    Active = False
    LastEventID = '22'
    OnOpen = MARSClientResourceSSE1Open
    OnReconnect = MARSClientResourceSSE1Reconnect
    OnClose = MARSClientResourceSSE1Close
    OnMessage = MARSClientResourceSSE1Message
    OnComment = MARSClientResourceSSE1Comment
    OnError = MARSClientResourceSSE1Error
    Left = 96
    Top = 208
  end
  object MARSClientToken1: TMARSClientToken
    Application = MARSApplication
    SpecificAccept = 'application/json'
    SpecificContentType = 'application/json'
    Resource = 'token'
    UserName = 'admin'
    Password = '23'
    Left = 216
    Top = 152
  end
end
