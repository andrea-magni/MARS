unit Custom.Types;

interface

uses
  Classes, SysUtils;

type
  TVAT = record
    ID: string;
    CountryCode: string;
    constructor Create(const AID: string; const ACountryCode: string = 'IT');
  end;

  TAddress = class
  private
    FState: string;
    FLangID: Integer;
    FCity: string;
  public
    ZipCode: string;
    constructor Create(const ACity: string; const AState: string; const ALangID: Integer = 0);

    property City: string read FCity write FCity;
    property State: string read FState write FState;
    property LangID: Integer read FLangID write FLangID;
  end;


  TCustomer = class
  private
    FName: string;
    FVAT: TVAT;
    FAddresses: TArray<TAddress>;
  public
    procedure AddAddress(const AAddress: TAddress);
    destructor Destroy; override;

    property Name: string read FName write FName;
    property VAT: TVAT read FVAT write FVAT;
    property Addresses: TArray<TAddress> read FAddresses write FAddresses;
  end;

  TInvoice = class
  private
    FVAT: Currency;
    FDate: TDate;
    FCompany: string;
    FTotal: Currency;
    FNumber: string;
    FAmount: Currency;
    FCustomer: TCustomer;
    procedure SetNumber(const Value: string);
  protected

  public
    constructor Create(); virtual;
    destructor Destroy; override;


    property Number: string read FNumber write SetNumber;
    property Date: TDate read FDate write FDate;
    property Company: string read FCompany write FCompany;
    property Amount: Currency read FAmount write FAmount;
    property VAT: Currency read FVAT write FVAT;
    property Total: Currency read FTotal write FTotal;

    property Customer: TCustomer read FCustomer write FCustomer;
  end;

implementation

uses
  CodeSiteLogging;

{ TInvoice }

constructor TInvoice.Create;
begin
  inherited Create;
  FCustomer := TCustomer.Create;
end;

destructor TInvoice.Destroy;
begin
  FCustomer.Free;
  inherited;
end;

procedure TInvoice.SetNumber(const Value: string);
begin
  FNumber := Value;
end;

{ TCustomer }

procedure TCustomer.AddAddress(const AAddress: TAddress);
begin
  FAddresses := FAddresses + [AAddress];
end;

destructor TCustomer.Destroy;
begin
  for var LAddress in FAddresses do
    LAddress.Free;
  FAddresses := [];

  inherited;
end;

{ TAddress }

constructor TAddress.Create(const ACity, AState: string;
  const ALangID: Integer);
begin
  inherited Create;
  City := ACity;
  State := AState;
  LangID := ALangID;
end;

{ TVAT }

constructor TVAT.Create(const AID, ACountryCode: string);
begin
  ID := AID;
  CountryCode := ACountryCode;
end;

end.
