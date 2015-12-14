object TodoDM: TTodoDM
  OldCreateOrder = False
  Height = 266
  Width = 341
  object ToDoClient: TMARSClient
    MARSEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 0
    ReadTimeout = -1
    Left = 40
    Top = 16
  end
  object ToDoApplication: TMARSClientApplication
    DefaultMediaType = 'application/json'
    AppName = 'todo'
    Client = ToDoClient
    Left = 40
    Top = 72
  end
  object Token: TMARSClientToken
    Application = ToDoApplication
    Resource = 'token'
    Password = 'pwd'
    Left = 184
    Top = 16
  end
  object ItemResource: TMARSClientResourceJSON
    Application = ToDoApplication
    Resource = 'item'
    PathParamsValues.Strings = (
      '30')
    Left = 40
    Top = 136
  end
  object AllItemsSubResource: TMARSClientSubResourceJSON
    Application = ToDoApplication
    Resource = 'all'
    ParentResource = ItemResource
    Left = 40
    Top = 192
  end
end
