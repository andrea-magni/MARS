object MainDataModule: TMainDataModule
  OldCreateOrder = False
  Height = 394
  Width = 605
  object MARSClient1: TMARSClient
    MARSEngineURL = 'http://localhost:8080/rest'
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
    HttpClient.Request.Accept = '*/*'
    HttpClient.Request.BasicAuthentication = False
    HttpClient.Request.Host = 'localhost:8080'
    HttpClient.Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    HttpClient.Request.Ranges.Units = 'bytes'
    HttpClient.Request.Ranges = <>
    HttpClient.HTTPOptions = [hoForceEncodeParams]
    Left = 280
    Top = 16
  end
  object MARSClientApplication1: TMARSClientApplication
    DefaultMediaType = 'application/json'
    DefaultContentType = 'application/json'
    Client = MARSClient1
    Left = 280
    Top = 72
  end
  object HelloWorldResource: TMARSClientResource
    Application = MARSClientApplication1
    SpecificAccept = '*/*'
    Resource = 'helloworld'
    Left = 280
    Top = 128
  end
  object EchoStringResource: TMARSClientResource
    Application = MARSClientApplication1
    SpecificAccept = '*/*'
    Resource = 'helloworld/echostring'
    Left = 136
    Top = 192
  end
  object ReverseStringResource: TMARSClientResource
    Application = MARSClientApplication1
    SpecificAccept = '*/*'
    Resource = 'helloworld/reversestring'
    Left = 280
    Top = 192
  end
  object SumSubResource: TMARSClientResource
    Application = MARSClientApplication1
    SpecificAccept = '*/*'
    Resource = 'helloworld/sum'
    Left = 424
    Top = 192
  end
end
