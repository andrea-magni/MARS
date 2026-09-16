unit Tests.Objects.Types;

interface

type
  TObjectWithProperties = class
  private
    FProp2: string;
    FProp3: string;
    FProp1: string;
  protected
  public
    property Prop1: string read FProp1 write FProp1;
    property Prop2: string read FProp2 write FProp2;
    property Prop3: string read FProp3 write FProp3;
  end;

  TObjectWithNames = class
  private
    FSerie: Integer;
    FKPrelieviT: Integer;
    FNumero: Integer;
    FAnno: Integer;
    FDataDocumento: string;
    FIdReparto: string;
    FIdCausale: string;
  protected
  public
    property KPrelieviT: Integer read FKPrelieviT write FKPrelieviT;
    property Anno: Integer read FAnno write FAnno;
    property Serie: Integer read FSerie write FSerie;
    property Numero: Integer read FNumero write FNumero;
    property DataDocumento: string read FDataDocumento write FDataDocumento;
    property IdCausale: string read FIdCausale write FIdCausale;
    property IdReparto: string read FIdReparto write FIdReparto;
  end;

  // plain classes with public fields, used as record members
  TTag = class
  public
    Name: string;
  end;

  TCode = class
  public
    Code: Integer;
    Tag: string;
  end;

  // constructor defaults and an owned sub-object: JSON keys left out must not touch them
  TOwnerWithDefaults = class
  private
    FDetail: TTag;
  public
    Name: string;
    Enabled: Boolean;
    Retries: Integer;
    constructor Create;
    destructor Destroy; override;
    property Detail: TTag read FDetail write FDetail;
  end;

  TPerson = class
  private
    FName: string;
    FSurname: string;
  protected
  public
    constructor Create(const AName, ASurname: string);

    property Name: string read FName write FName;
    property Surname: string read FSurname write FSurname;

  end;


implementation


{ TOwnerWithDefaults }

constructor TOwnerWithDefaults.Create;
begin
  inherited Create;
  FDetail := TTag.Create;
  FDetail.Name := 'from constructor';
  Enabled := True;
  Retries := 3;
end;

destructor TOwnerWithDefaults.Destroy;
begin
  FDetail.Free;
  inherited;
end;

{ TPerson }

constructor TPerson.Create(const AName, ASurname: string);
begin
  inherited Create;
  FName := AName;
  FSurname := ASurname;
end;

end.
