object MainDataModule: TMainDataModule
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
  object CustomersDataResource: TMARSFDResource
    Application = MARSApplication
    Resource = 'customersdata'
    ResourceDataSets = <
      item
        DataSetName = 'Customers'
        DataSet = CustomersTable
        SendDelta = True
        Synchronize = True
      end
      item
        DataSetName = 'Cities'
        DataSet = CitiesTable
        SendDelta = False
        Synchronize = True
      end>
    Left = 88
    Top = 152
  end
  object CustomersTable: TFDMemTable
    Left = 232
    Top = 24
  end
  object CitiesTable: TFDMemTable
    Left = 232
    Top = 80
  end
end
