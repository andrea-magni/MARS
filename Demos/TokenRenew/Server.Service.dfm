object ServerService: TServerService
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  DisplayName = 'TokenRenew Service'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
