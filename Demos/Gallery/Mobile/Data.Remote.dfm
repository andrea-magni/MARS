object RemoteData: TRemoteData
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 296
  Width = 441
  object MARSClient: TMARSClient
    MARSEngineURL = 'http://localhost:8080/'
    ConnectTimeout = 0
    ReadTimeout = -1
    ProtocolVersion = pv1_1
    Left = 64
    Top = 16
  end
  object CategoriesResource: TMARSClientResourceJSON
    Application = GalleryApplication
    Resource = 'category'
    Left = 176
    Top = 88
  end
  object GalleryApplication: TMARSClientApplication
    DefaultMediaType = 'application/json'
    AppName = 'gallery'
    Client = MARSClient
    OnError = GalleryApplicationError
    Left = 176
    Top = 16
  end
  object CategoryItemsSubResource: TMARSClientSubResourceJSON
    Application = GalleryApplication
    Resource = 'main'
    ParentResource = CategoriesResource
    Left = 176
    Top = 144
  end
  object ItemSubResource: TMARSClientSubResourceStream
    Application = GalleryApplication
    Resource = 'main'
    PathParamsValues.Strings = (
      'city'
      'IMG_0035.jpg')
    ParentResource = CategoriesResource
    Left = 176
    Top = 200
  end
  object Token: TMARSClientToken
    Application = GalleryApplication
    Resource = 'token'
    Left = 280
    Top = 16
  end
end
