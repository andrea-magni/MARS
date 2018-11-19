unit Data.Remote;

interface

uses
  System.SysUtils, System.Classes, System.JSON, MARS.Client.CustomResource,
  MARS.Client.Resource, MARS.Client.Resource.JSON, MARS.Client.Client,
  MARS.Client.Application, MARS.Utils.Parameters, MARS.Client.Token, MARS.Client.Client.Indy,
  MARS.Client.Resource.Stream
;

type
  TJSONArrayProc = TProc<TJSONArray>;
  TStreamProc = TProc<TStream>;

  TRemoteData = class(TDataModule)
    MARSClient: TMARSClient;
    CategoriesResource: TMARSClientResourceJSON;
    GalleryApplication: TMARSClientApplication;
    CategoryItemsSubResource: TMARSClientResourceJSON;
    ItemSubResource: TMARSClientResourceStream;
    Token: TMARSClientToken;
    procedure GalleryApplicationError(AResource: TObject; AException: Exception;
      AVerb: TMARSHttpVerb; const AAfterExecute: TProc<System.Classes.TStream>;
      var AHandled: Boolean);
    procedure DataModuleCreate(Sender: TObject);
  private
  public
    procedure GetCategories(const AOnSuccess: TJSONArrayProc);
    procedure GetItems(const ACategory: string; const AOnSuccess: TJSONArrayProc);
    procedure GetItem(const ACategory, AItem: string; const AOnSuccess: TStreamProc);
  end;

var
  RemoteData: TRemoteData;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses Data.Main;

{$R *.dfm}

{ TRemoteData }

procedure TRemoteData.DataModuleCreate(Sender: TObject);
begin
  {$IFDEF ANDROID} // tethering
  MARSClient.MARSEngineURL := 'http://192.168.43.152:8080/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  MARSClient.MARSEngineURL := 'http://localhost:8080/';
  {$ENDIF}
end;

procedure TRemoteData.GalleryApplicationError(AResource: TObject;
  AException: Exception; AVerb: TMARSHttpVerb;
  const AAfterExecute: TProc<System.Classes.TStream>; var AHandled: Boolean);
var
  LMessage: string;
begin
  LMessage := AException.Message;
  AHandled := True;

  TThread.Queue(nil,
    procedure
    begin
      MainData.ShowError(LMessage);
    end
  );
end;

procedure TRemoteData.GetCategories(const AOnSuccess: TJSONArrayProc);
begin
  CategoriesResource.GETAsync(
    procedure (AResource: TMARSClientCustomResource)
    var
      LResponse: TJSONValue;
    begin
      LResponse := (AResource as TMARSClientResourceJSON).Response;
      if Assigned(AOnSuccess) and (LResponse is TJSONArray) then
        AOnSuccess(TJSONArray(LResponse));
    end
  );
end;

procedure TRemoteData.GetItem(const ACategory, AItem: string;
  const AOnSuccess: TStreamProc);
begin
  ItemSubResource.Resource := 'category';
  ItemSubResource.PathParamsValues.Clear;
  ItemSubResource.PathParamsValues.Add(ACategory);
  ItemSubResource.PathParamsValues.Add(AItem);
  ItemSubResource.GETAsync(
    procedure (AResource: TMARSClientCustomResource)
    var
      LResponse: TStream;
    begin
      LResponse := (AResource as TMARSClientResourceStream).Response;
      if Assigned(AOnSuccess) then
        AOnSuccess(LResponse);
    end
  );
end;

procedure TRemoteData.GetItems(const ACategory: string; const AOnSuccess: TJSONArrayProc);
begin
  CategoryItemsSubResource.Resource := 'category/' + ACategory;
  CategoryItemsSubResource.GETAsync(
    procedure (AResource: TMARSClientCustomResource)
    var
      LResponse: TJSONValue;
    begin
      LResponse := (AResource as TMARSClientResourceJSON).Response;
      if Assigned(AOnSuccess) and (LResponse is TJSONArray) then
        AOnSuccess(TJSONArray(LResponse));
    end
  );
end;

end.
