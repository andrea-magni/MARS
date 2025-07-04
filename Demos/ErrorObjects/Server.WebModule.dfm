object ServerWebModule: TServerWebModule
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = ServerWebModuleDefaultHandlerAction
    end>
  Height = 230
  Width = 415
end
