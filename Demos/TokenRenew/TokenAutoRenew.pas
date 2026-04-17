unit TokenAutoRenew;

interface

uses
  Classes, SysUtils
, MARS.Core.Attributes, MARS.Core.Token, MARS.Core.Activation.Interfaces
;


type
  TRenewalStrategy = (DurationPercentage, RemainingSeconds);

  TokenAutoRenewAttribute = class(MARSAttribute)
  private
    FRenewalStrategy: TRenewalStrategy;
    FRenewalStrategyValue: Integer;
  public
    constructor Create(
      const ARenewalStrategyValue: Integer = 50;
      const ARenewalStrategy: TRenewalStrategy = DurationPercentage
    );

    function RenewalThresholdSeconds(const AActivation: IMARSActivation): Double; virtual;
    function ExpiresInSeconds(const AActivation: IMARSActivation): Double; virtual;
    function RenewRequired(const AActivation: IMARSActivation): Boolean; virtual;
    procedure Renew(const AActivation: IMARSActivation); virtual;

    property RenewalStrategy: TRenewalStrategy read FRenewalStrategy;
  end;


implementation

uses
  TimeSpan, System.Rtti
, MARS.Utils.JWT
;

{ TokenAutoRenewAttribute }

constructor TokenAutoRenewAttribute.Create(
  const ARenewalStrategyValue: Integer = 50;
  const ARenewalStrategy: TRenewalStrategy = DurationPercentage
);
begin
  inherited Create;
  FRenewalStrategy := ARenewalStrategy;
  FRenewalStrategyValue := ARenewalStrategyValue;
end;

function TokenAutoRenewAttribute.ExpiresInSeconds(
  const AActivation: IMARSActivation): Double;
var
  LToken: TMARSToken;
  LExpiration: TDateTime;
begin
  LToken := AActivation.Token;
  LExpiration := LToken.Expiration;
  Result := TTimeSpan.Subtract(LExpiration, Now).TotalSeconds;
end;

procedure TokenAutoRenewAttribute.Renew(const AActivation: IMARSActivation);
var
  LSecret: string;
begin
  LSecret := AActivation.Application.Parameters.ByNameText(JWT_SECRET_PARAM).AsString;
  AActivation.Token.Build(LSecret);
end;

function TokenAutoRenewAttribute.RenewalThresholdSeconds(
  const AActivation: IMARSActivation): Double;
var
  LToken: TMARSToken;
  LDurationSeconds: Int64;
begin
  LToken := AActivation.Token;
  LDurationSeconds :=  LToken.DurationSecs;

  case FRenewalStrategy of
     DurationPercentage: Result := (LDurationSeconds * (FRenewalStrategyValue / 100));
       RemainingSeconds: Result := FRenewalStrategyValue;
    else
      raise Exception.Create('Renewal strategy unknown: ' + TRttiEnumerationType.GetName<TRenewalStrategy>(FRenewalStrategy));
  end;

end;

function TokenAutoRenewAttribute.RenewRequired(
  const AActivation: IMARSActivation): Boolean;
begin

  Result := ExpiresInSeconds(AActivation) < RenewalThresholdSeconds(AActivation);
end;

end.
