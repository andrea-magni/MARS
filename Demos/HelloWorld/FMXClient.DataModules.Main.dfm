object MainDataModule: TMainDataModule
  OldCreateOrder = False
  Height = 394
  Width = 605
  object MARSClient1: TMARSClient
    MARSEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 0
    ReadTimeout = -1
    Left = 280
    Top = 16
  end
  object MARSClientApplication1: TMARSClientApplication
    DefaultMediaType = 'application/json'
    AppName = 'default'
    Client = MARSClient1
    Left = 280
    Top = 72
  end
  object HelloWorldResource: TMARSClientResource
    Application = MARSClientApplication1
    Resource = 'helloworld'
    Left = 280
    Top = 128
  end
  object EchoStringResource: TMARSClientSubResource
    Application = MARSClientApplication1
    Resource = 'echostring'
    ParentResource = HelloWorldResource
    Left = 224
    Top = 208
  end
  object ReverseStringResource: TMARSClientSubResource
    Application = MARSClientApplication1
    Resource = 'reversestring'
    ParentResource = HelloWorldResource
    Left = 328
    Top = 192
  end
  object PostExampleResource: TMARSClientSubResourceJSON
    Application = MARSClientApplication1
    Resource = 'postexample'
    ParentResource = HelloWorldResource
    Left = 72
    Top = 288
  end
end
