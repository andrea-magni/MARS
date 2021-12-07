object ServerService: TServerService
  OldCreateOrder = False
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  DisplayName = 'AuthDemo Service'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
