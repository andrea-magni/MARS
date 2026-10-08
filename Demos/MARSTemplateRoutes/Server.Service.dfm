object ServerService: TServerService
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  DisplayName = 'MARSTemplate Service'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
