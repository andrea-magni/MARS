object ServerService: TServerService
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  DisplayName = 'WebStencilsDemo Service'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
