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


{ TPerson }

constructor TPerson.Create(const AName, ASurname: string);
begin
  inherited Create;
  FName := AName;
  FSurname := ASurname;
end;

end.
