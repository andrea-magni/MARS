(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Utils.FloatText;

// Floats as JSON numbers (#278): the shortest decimal that reads back to the same value, and the
// value nearest to a decimal (round half to even), as JavaScript, Python, Java... do.
// TJSONNumber.Create writes 15 significant digits (not enough for a Double), FloatToStr and
// StrToFloat of the RTL are not correctly rounded (and write garbage below 1E-307 on Win32):
// they are used only as estimates, checked with exact integer arithmetic (the free-format
// algorithm of Steele & White, Burger & Dybvig to write; the fast path of Clinger and a
// comparison with the midpoints between Doubles to read).

interface

uses
  System.SysUtils;

// the shortest text that reads back to AValue: '0.1', '1.2345678901234567', '3.0', '1E-20'
// (plain from 1E-4 up to 1E15, as TJSONNumber writes them). EConvertError for NaN and infinities.
function FloatToJSONText(const AValue: Double): string;
// the same with the precision of a Single: '0.1' for 0.1 (not 0.100000001490116)
function SingleToJSONText(const AValue: Single): string;
// AValue := the Double nearest to the JSON number AText (round half to even); False if AText is
// not a JSON number (no spaces, '.' as separator) or beyond the largest Double
function TryJSONTextToFloat(const AText: string; out AValue: Double): Boolean;

implementation

uses
  System.Math;

const
  // the significant digits read: more do not change the result (the last one is kept as sticky)
  FLOAT_TEXT_MAX_DIGITS = 800;
  // 10^(800 + 330) times 2^1080 at most
  FLOAT_TEXT_BIG_WORDS = 160;

type
  // a natural number, 32 bits per word, the least significant first
  TFloatTextBig = record
    Len: Integer;
    Words: array[0..FLOAT_TEXT_BIG_WORDS - 1] of Cardinal;
  end;

procedure BigSet(var A: TFloatTextBig; const AValue: UInt64);
begin
  A.Words[0] := Cardinal(AValue);
  A.Words[1] := Cardinal(AValue shr 32);
  if A.Words[1] <> 0 then
    A.Len := 2
  else if A.Words[0] <> 0 then
    A.Len := 1
  else
    A.Len := 0;
end;

procedure BigAssign(var A: TFloatTextBig; const B: TFloatTextBig);
begin
  A.Len := B.Len;
  if B.Len > 0 then
    Move(B.Words[0], A.Words[0], B.Len * SizeOf(Cardinal));
end;

procedure BigMulAdd(var A: TFloatTextBig; const AMul: Cardinal; const AAdd: Cardinal = 0);
var
  I: Integer;
  LCarry: UInt64;
begin
  LCarry := AAdd;
  for I := 0 to A.Len - 1 do
  begin
    LCarry := UInt64(A.Words[I]) * AMul + LCarry;
    A.Words[I] := Cardinal(LCarry);
    LCarry := LCarry shr 32;
  end;
  if LCarry <> 0 then
  begin
    A.Words[A.Len] := Cardinal(LCarry);
    Inc(A.Len);
  end;
end;

procedure BigMulPow10(var A: TFloatTextBig; ACount: Integer);
begin
  while ACount >= 9 do
  begin
    BigMulAdd(A, 1000000000);
    Dec(ACount, 9);
  end;
  while ACount > 0 do
  begin
    BigMulAdd(A, 10);
    Dec(ACount);
  end;
end;

procedure BigShl(var A: TFloatTextBig; const ABits: Integer);
var
  LWords, LBits, I: Integer;
begin
  if (A.Len = 0) or (ABits = 0) then
    Exit;
  LWords := ABits div 32;
  LBits := ABits mod 32;
  if LBits = 0 then
  begin
    for I := A.Len - 1 downto 0 do
      A.Words[I + LWords] := A.Words[I];
  end
  else
  begin
    A.Words[A.Len + LWords] := 0;
    for I := A.Len - 1 downto 0 do
    begin
      A.Words[I + LWords + 1] := A.Words[I + LWords + 1] or (A.Words[I] shr (32 - LBits));
      A.Words[I + LWords] := A.Words[I] shl LBits;
    end;
  end;
  for I := 0 to LWords - 1 do
    A.Words[I] := 0;
  Inc(A.Len, LWords + 1);
  if (LBits = 0) or (A.Words[A.Len - 1] = 0) then
    Dec(A.Len);
end;

function BigCompare(const A, B: TFloatTextBig): Integer;
var
  I: Integer;
begin
  if A.Len <> B.Len then
    Exit(A.Len - B.Len);
  for I := A.Len - 1 downto 0 do
    if A.Words[I] <> B.Words[I] then
    begin
      if A.Words[I] > B.Words[I] then
        Exit(1)
      else
        Exit(-1);
    end;
  Result := 0;
end;

// AResult := A + B
procedure BigAdd(var AResult: TFloatTextBig; const A, B: TFloatTextBig);
var
  I, LLen: Integer;
  LCarry: UInt64;
begin
  LLen := A.Len;
  if B.Len > LLen then
    LLen := B.Len;
  LCarry := 0;
  for I := 0 to LLen - 1 do
  begin
    if I < A.Len then
      LCarry := LCarry + A.Words[I];
    if I < B.Len then
      LCarry := LCarry + B.Words[I];
    AResult.Words[I] := Cardinal(LCarry);
    LCarry := LCarry shr 32;
  end;
  AResult.Len := LLen;
  if LCarry <> 0 then
  begin
    AResult.Words[LLen] := Cardinal(LCarry);
    AResult.Len := LLen + 1;
  end;
end;

// A := A - B, A >= B
procedure BigSub(var A: TFloatTextBig; const B: TFloatTextBig);
var
  I: Integer;
  LBorrow: Int64;
begin
  LBorrow := 0;
  for I := 0 to A.Len - 1 do
  begin
    LBorrow := Int64(A.Words[I]) - LBorrow;
    if I < B.Len then
      LBorrow := LBorrow - B.Words[I];
    if LBorrow < 0 then
    begin
      A.Words[I] := Cardinal(LBorrow + (Int64(1) shl 32));
      LBorrow := 1;
    end
    else
    begin
      A.Words[I] := Cardinal(LBorrow);
      LBorrow := 0;
    end;
  end;
  while (A.Len > 0) and (A.Words[A.Len - 1] = 0) do
    Dec(A.Len);
end;

function BigBitLength(const A: TFloatTextBig): Integer;
var
  LTop: Cardinal;
begin
  if A.Len = 0 then
    Exit(0);
  Result := (A.Len - 1) * 32;
  LTop := A.Words[A.Len - 1];
  while LTop <> 0 do
  begin
    Inc(Result);
    LTop := LTop shr 1;
  end;
end;

// the significant digits of a JSON number, without leading and trailing zeros, and the
// exponent of the last one
function ScanJSONNumber(const AText: string; out ANegative: Boolean; out ADigits: string;
  out AExponent: Integer): Boolean;
var
  LIndex, LLength, LExpValue: Integer;
  LExpNegative, LAnyDigit, LDropped: Boolean;
begin
  Result := False;
  LLength := Length(AText);
  LIndex := 1;
  ANegative := (LIndex <= LLength) and (AText[LIndex] = '-');
  if ANegative then
    Inc(LIndex);
  ADigits := '';
  AExponent := 0;
  LAnyDigit := False;
  LDropped := False;
  while (LIndex <= LLength) and CharInSet(AText[LIndex], ['0'..'9']) do
  begin
    LAnyDigit := True;
    if (ADigits <> '') or (AText[LIndex] <> '0') then
    begin
      if Length(ADigits) < FLOAT_TEXT_MAX_DIGITS then
        ADigits := ADigits + AText[LIndex]
      else
      begin
        Inc(AExponent);
        LDropped := LDropped or (AText[LIndex] <> '0');
      end;
    end;
    Inc(LIndex);
  end;
  if not LAnyDigit then
    Exit;
  if (LIndex <= LLength) and (AText[LIndex] = '.') then
  begin
    Inc(LIndex);
    LAnyDigit := False;
    while (LIndex <= LLength) and CharInSet(AText[LIndex], ['0'..'9']) do
    begin
      LAnyDigit := True;
      if (ADigits <> '') or (AText[LIndex] <> '0') then
      begin
        if Length(ADigits) < FLOAT_TEXT_MAX_DIGITS then
        begin
          ADigits := ADigits + AText[LIndex];
          Dec(AExponent);
        end
        else
          LDropped := LDropped or (AText[LIndex] <> '0');
      end
      else
        Dec(AExponent);
      Inc(LIndex);
    end;
    if not LAnyDigit then
      Exit;
  end;
  if (LIndex <= LLength) and CharInSet(AText[LIndex], ['e', 'E']) then
  begin
    Inc(LIndex);
    LExpNegative := (LIndex <= LLength) and (AText[LIndex] = '-');
    if (LIndex <= LLength) and CharInSet(AText[LIndex], ['+', '-']) then
      Inc(LIndex);
    if (LIndex > LLength) or not CharInSet(AText[LIndex], ['0'..'9']) then
      Exit;
    LExpValue := 0;
    while (LIndex <= LLength) and CharInSet(AText[LIndex], ['0'..'9']) do
    begin
      if LExpValue < 100000 then
        LExpValue := LExpValue * 10 + Ord(AText[LIndex]) - Ord('0');
      Inc(LIndex);
    end;
    if LExpNegative then
      AExponent := AExponent - LExpValue
    else
      AExponent := AExponent + LExpValue;
  end;
  if LIndex <= LLength then
    Exit;
  // the digits dropped, not all 0: the value is above the others (a sticky digit)
  if LDropped then
  begin
    ADigits := ADigits + '1';
    Dec(AExponent);
  end;
  while (ADigits <> '') and (ADigits[Length(ADigits)] = '0') do
  begin
    SetLength(ADigits, Length(ADigits) - 1);
    Inc(AExponent);
  end;
  Result := True;
end;

// the exact value when the digits and the power of 10 are exact Doubles (Clinger): up to 15
// digits, 10^-22..10^22
{$WARN SYMBOL_PLATFORM OFF} // SetPrecisionMode (Win32 only)
function ExactSmallDecimal(const ADigits: string; const AExponent: Integer; out AValue: Double): Boolean;
var
  LMantissa: UInt64;
  LPower: Double;
  I: Integer;
  {$IFDEF CPUX86}
  LPrecision: TFPUPrecisionMode;
  {$ENDIF}
begin
  Result := (Length(ADigits) <= 15) and (AExponent >= -22) and (AExponent <= 22);
  if not Result then
    Exit;
  LMantissa := 0;
  for I := 1 to Length(ADigits) do
    LMantissa := LMantissa * 10 + UInt64(Ord(ADigits[I]) - Ord('0'));
  {$IFDEF CPUX86}
  // x87: one rounding to 53 bits, not to 64 and then to 53
  LPrecision := SetPrecisionMode(pmDouble);
  try
  {$ENDIF}
    LPower := 1;
    for I := 1 to Abs(AExponent) do
      LPower := LPower * 10;
    // through Int64: UInt64 in a float expression gives 0 with dcc32
    if AExponent >= 0 then
      AValue := Int64(LMantissa) * LPower
    else
      AValue := Int64(LMantissa) / LPower;
  {$IFDEF CPUX86}
  finally
    SetPrecisionMode(LPrecision);
  end;
  {$ENDIF}
end;
{$WARN SYMBOL_PLATFORM DEFAULT}

// the sign of ADigits * 10^AExponent - AMultiplier * 2^APower2
function CompareDecimalBinary(const ADigits: TFloatTextBig; const AExponent: Integer;
  const AMultiplier: UInt64; const APower2: Integer): Integer;
var
  A, B: TFloatTextBig;
begin
  BigAssign(A, ADigits);
  BigSet(B, AMultiplier);
  if AExponent >= 0 then
    BigMulPow10(A, AExponent)
  else
    BigMulPow10(B, -AExponent);
  if APower2 >= 0 then
    BigShl(B, APower2)
  else
    BigShl(A, -APower2);
  Result := BigCompare(A, B);
end;

// a positive finite Double as AMantissa * 2^AExponent
procedure DecomposeDouble(const AValue: Double; out AMantissa: UInt64; out AExponent: Integer);
var
  LBits: UInt64;
begin
  LBits := PUInt64(@AValue)^ and not (UInt64(1) shl 63);
  AMantissa := LBits and ((UInt64(1) shl 52) - 1);
  AExponent := LBits shr 52;
  if AExponent = 0 then
    AExponent := -1074
  else
  begin
    AMantissa := AMantissa or (UInt64(1) shl 52);
    AExponent := AExponent - 1075;
  end;
end;

// AValue (a positive estimate) := the Double nearest to ADigits * 10^AExponent; False when the
// estimate is too far (more than a few steps) or not a positive finite value
function CorrectEstimate(const ADigits: TFloatTextBig; const AExponent: Integer;
  var AValue: Double): Boolean;
var
  LStep, LExponent2, LCompare: Integer;
  LMantissa, LBits: UInt64;
begin
  for LStep := 1 to 4 do
  begin
    if not (AValue > 0) or AValue.IsInfinity then
      Exit(False);
    DecomposeDouble(AValue, LMantissa, LExponent2);
    LBits := PUInt64(@AValue)^;
    // above the midpoint with the next Double (or on it, and the mantissa odd)
    LCompare := CompareDecimalBinary(ADigits, AExponent, 2 * LMantissa + 1, LExponent2 - 1);
    if (LCompare > 0) or ((LCompare = 0) and Odd(LMantissa)) then
    begin
      PUInt64(@AValue)^ := LBits + 1;
      Continue;
    end;
    // below the midpoint with the previous one (half the distance at a power of 2)
    if (LMantissa = (UInt64(1) shl 52)) and (LExponent2 > -1074) then
      LCompare := CompareDecimalBinary(ADigits, AExponent, 4 * LMantissa - 1, LExponent2 - 2)
    else
      LCompare := CompareDecimalBinary(ADigits, AExponent, 2 * LMantissa - 1, LExponent2 - 1);
    if (LCompare < 0) or ((LCompare = 0) and Odd(LMantissa)) then
    begin
      PUInt64(@AValue)^ := LBits - 1;
      Continue;
    end;
    Exit(True);
  end;
  Result := False;
end;

// AValue := the Double nearest to ADigits * 10^AExponent, by division (any value); False
// beyond the largest Double
function DivideDecimal(const ADigits: TFloatTextBig; const AExponent: Integer; out AValue: Double): Boolean;
var
  N, M, LRest, LDivisor, LPart: TFloatTextBig;
  LExponent2, LCompare: Integer;
  LQuotient, LBits: UInt64;

  function Divide(const AExponent2: Integer): UInt64;
  var
    LIndex: Integer;
  begin
    // N / (M * 2^AExponent2): the quotient (< 2^56), the remainder in LRest, the divisor in LDivisor
    BigAssign(LRest, N);
    BigAssign(LDivisor, M);
    if AExponent2 >= 0 then
      BigShl(LDivisor, AExponent2)
    else
      BigShl(LRest, -AExponent2);
    Result := 0;
    for LIndex := 55 downto 0 do
    begin
      BigAssign(LPart, LDivisor);
      BigShl(LPart, LIndex);
      if BigCompare(LRest, LPart) >= 0 then
      begin
        BigSub(LRest, LPart);
        Result := Result or (UInt64(1) shl LIndex);
      end;
    end;
  end;

begin
  Result := False;
  AValue := 0;
  BigAssign(N, ADigits);
  BigSet(M, 1);
  if AExponent >= 0 then
    BigMulPow10(N, AExponent)
  else
    BigMulPow10(M, -AExponent);

  // a quotient of 53 bits: N / M = LQuotient * 2^LExponent2
  LExponent2 := BigBitLength(N) - BigBitLength(M) - 53;
  if LExponent2 < -1074 then
    LExponent2 := -1074;
  LQuotient := Divide(LExponent2);
  while LQuotient >= (UInt64(1) shl 53) do
  begin
    Inc(LExponent2);
    LQuotient := Divide(LExponent2);
  end;
  while (LQuotient < (UInt64(1) shl 52)) and (LExponent2 > -1074) do
  begin
    Dec(LExponent2);
    LQuotient := Divide(LExponent2);
  end;

  // round half to even
  BigShl(LRest, 1);
  LCompare := BigCompare(LRest, LDivisor);
  if (LCompare > 0) or ((LCompare = 0) and Odd(LQuotient)) then
    Inc(LQuotient);
  if LQuotient = (UInt64(1) shl 53) then
  begin
    LQuotient := UInt64(1) shl 52;
    Inc(LExponent2);
  end;
  if LExponent2 > 971 then // infinity
    Exit;

  if LQuotient >= (UInt64(1) shl 52) then
    LBits := (UInt64(LExponent2 + 1075) shl 52) or (LQuotient and ((UInt64(1) shl 52) - 1))
  else
    LBits := LQuotient; // subnormal (LExponent2 = -1074)
  PUInt64(@AValue)^ := LBits;
  Result := True;
end;

// AValue := the Double nearest to the JSON number AText (round half to even); False if AText is
// not a JSON number or beyond the largest Double
function TryJSONTextToFloat(const AText: string; out AValue: Double): Boolean;
var
  LNegative: Boolean;
  LDigits: string;
  LExponent: Integer;
  LBig: TFloatTextBig;
  LChar: Char;
begin
  Result := False;
  AValue := 0;
  if not ScanJSONNumber(AText, LNegative, LDigits, LExponent) then
    Exit;

  if LDigits = '' then // zero
    AValue := 0
  else if Length(LDigits) + LExponent > 310 then // above the largest Double
    Exit
  else if Length(LDigits) + LExponent < -330 then // below half the smallest one
    AValue := 0
  else if not ExactSmallDecimal(LDigits, LExponent, AValue) then
  begin
    LBig.Len := 0;
    for LChar in LDigits do
      BigMulAdd(LBig, 10, Ord(LChar) - Ord('0'));
    // the estimate of the RTL, corrected; the division when it is not close enough
    if not TryStrToFloat(AText, AValue, TFormatSettings.Invariant) then
      AValue := 0;
    AValue := Abs(AValue);
    if not CorrectEstimate(LBig, LExponent, AValue) then
      if not DivideDecimal(LBig, LExponent, AValue) then
        Exit;
  end;
  if LNegative then
    AValue := -AValue;
  Result := True;
end;

// the digits of AMantissa * 2^AExponent (> 0), ADigits and the position of the decimal point
// (0.ADigits * 10^ADecimalPoint): the shortest that reads back to the same value with the
// precision AMantissaBits (52: Double, 23: Single), whose smallest exponent is AMinExponent
procedure ShortestDigits(const AMantissa: UInt64; const AExponent, AMinExponent,
  AMantissaBits: Integer; out ADigits: string; out ADecimalPoint: Integer);
var
  R, S, MPlus, MMinus, LSum: TFloatTextBig;
  LEven, LLow, LHigh: Boolean;
  LDigit, LK, LCompare, LCount: Integer;
  LBuffer: array[0..31] of Char;
begin
  LEven := not Odd(AMantissa);
  if AExponent >= 0 then
  begin
    BigSet(R, AMantissa);
    BigSet(MPlus, 1);
    if AMantissa <> (UInt64(1) shl AMantissaBits) then
    begin
      BigShl(R, AExponent + 1);
      BigSet(S, 2);
      BigShl(MPlus, AExponent);
      BigAssign(MMinus, MPlus);
    end
    else
    begin
      BigShl(R, AExponent + 2);
      BigSet(S, 4);
      BigShl(MPlus, AExponent + 1);
      BigSet(MMinus, 1);
      BigShl(MMinus, AExponent);
    end;
  end
  else
  begin
    BigSet(S, 1);
    BigSet(MMinus, 1);
    if (AExponent = AMinExponent) or (AMantissa <> (UInt64(1) shl AMantissaBits)) then
    begin
      BigSet(R, AMantissa * 2);
      BigShl(S, 1 - AExponent);
      BigSet(MPlus, 1);
    end
    else
    begin
      BigSet(R, AMantissa * 4);
      BigShl(S, 2 - AExponent);
      BigSet(MPlus, 2);
    end;
  end;

  // an estimate of the position of the decimal point, then its fixup
  LK := Ceil(Log10(Int64(AMantissa)) + AExponent * Log10(2) - 1E-10);
  if LK >= 0 then
    BigMulPow10(S, LK)
  else
  begin
    BigMulPow10(R, -LK);
    BigMulPow10(MPlus, -LK);
    BigMulPow10(MMinus, -LK);
  end;
  while True do
  begin
    BigAdd(LSum, R, MPlus);
    LCompare := BigCompare(LSum, S);
    if (LCompare > 0) or (LEven and (LCompare = 0)) then
    begin
      BigMulAdd(S, 10);
      Inc(LK);
    end
    else
      Break;
  end;
  while True do
  begin
    BigAdd(LSum, R, MPlus);
    BigMulAdd(LSum, 10);
    LCompare := BigCompare(LSum, S);
    if (LCompare < 0) or (not LEven and (LCompare = 0)) then
    begin
      BigMulAdd(R, 10);
      BigMulAdd(MPlus, 10);
      BigMulAdd(MMinus, 10);
      Dec(LK);
    end
    else
      Break;
  end;
  ADecimalPoint := LK;

  LCount := 0;
  repeat
    BigMulAdd(R, 10);
    BigMulAdd(MPlus, 10);
    BigMulAdd(MMinus, 10);
    LDigit := 0;
    while BigCompare(R, S) >= 0 do
    begin
      BigSub(R, S);
      Inc(LDigit);
    end;
    LCompare := BigCompare(R, MMinus);
    LLow := (LCompare < 0) or (LEven and (LCompare = 0));
    BigAdd(LSum, R, MPlus);
    LCompare := BigCompare(LSum, S);
    LHigh := (LCompare > 0) or (LEven and (LCompare = 0));
    if LLow and LHigh then
    begin
      // both read back to the value: the nearest
      BigAdd(LSum, R, R);
      if BigCompare(LSum, S) >= 0 then
        Inc(LDigit);
    end
    else if LHigh then
      Inc(LDigit);
    LBuffer[LCount] := Chr(Ord('0') + LDigit);
    Inc(LCount);
  until LLow or LHigh or (LCount > High(LBuffer));
  SetString(ADigits, PChar(@LBuffer[0]), LCount);
end;

// the digits as a JSON number, as the RTL writes them: plain from 1E-4 up to 1E15 (with ".0"
// when integral), "1.5E-20" otherwise
function DigitsToJSONText(const ANegative: Boolean; const ADigits: string;
  const ADecimalPoint: Integer): string;
var
  LCount: Integer;
begin
  LCount := Length(ADigits);
  if (ADecimalPoint < -3) or (ADecimalPoint > 15) then
  begin
    Result := ADigits[1];
    if LCount > 1 then
      Result := Result + '.' + Copy(ADigits, 2, MaxInt);
    Result := Result + 'E' + IntToStr(ADecimalPoint - 1);
  end
  else if ADecimalPoint <= 0 then
    Result := '0.' + StringOfChar('0', -ADecimalPoint) + ADigits
  else if ADecimalPoint >= LCount then
    Result := ADigits + StringOfChar('0', ADecimalPoint - LCount) + '.0'
  else
    Result := Copy(ADigits, 1, ADecimalPoint) + '.' + Copy(ADigits, ADecimalPoint + 1, MaxInt);
  if ANegative then
    Result := '-' + Result;
end;

function FloatToJSONText(const AValue: Double): string;
var
  LBits, LMantissa: UInt64;
  LExponent: Integer;
  LNegative: Boolean;
  LDigits: string;
  LBack: Double;
begin
  if AValue.IsNan or AValue.IsInfinity then
    raise EConvertError.Create('NaN and infinities are not JSON numbers');
  LBits := PUInt64(@AValue)^;
  if (LBits shl 1) = 0 then
  begin
    if (LBits shr 63) <> 0 then
      Exit('-0.0');
    Exit('0.0');
  end;

  // up to 15 digits: those of the RTL, when they are exact (they are the shortest then)
  if ScanJSONNumber(FloatToStrF(Abs(AValue), ffGeneral, 15, 0, TFormatSettings.Invariant),
       LNegative, LDigits, LExponent)
    and ExactSmallDecimal(LDigits, LExponent, LBack) and (LBack = Abs(AValue)) then
    Exit(DigitsToJSONText((LBits shr 63) <> 0, LDigits, LExponent + Length(LDigits)));

  DecomposeDouble(AValue, LMantissa, LExponent);
  ShortestDigits(LMantissa, LExponent, -1074, 52, LDigits, LExponent);
  Result := DigitsToJSONText((LBits shr 63) <> 0, LDigits, LExponent);
end;

function SingleToJSONText(const AValue: Single): string;
var
  LBits: Cardinal;
  LFraction: UInt64;
  LExponent: Integer;
  LDigits: string;
  LDecimalPoint: Integer;
begin
  if AValue.IsNan or AValue.IsInfinity then
    raise EConvertError.Create('NaN and infinities are not JSON numbers');
  LBits := PCardinal(@AValue)^;
  LFraction := LBits and ((1 shl 23) - 1);
  LExponent := (LBits shr 23) and $FF;
  if (LExponent = 0) and (LFraction = 0) then
  begin
    if (LBits shr 31) <> 0 then
      Exit('-0.0');
    Exit('0.0');
  end;
  if LExponent = 0 then
    ShortestDigits(LFraction, -149, -149, 23, LDigits, LDecimalPoint)
  else
    ShortestDigits(LFraction or (1 shl 23), LExponent - 150, -149, 23, LDigits, LDecimalPoint);
  Result := DigitsToJSONText((LBits shr 31) <> 0, LDigits, LDecimalPoint);
end;

end.
