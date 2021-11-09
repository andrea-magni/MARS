unit Neslib.Hash;
{< Hashing functionality }

{$INCLUDE 'Neslib.inc'}

interface

{ Computes a 32-bit MurmurHash2 value, as described here:
    https://sites.google.com/site/murmurhash/
  This hash algorithm is fast and very resistant to hash collisions, making it
  ideal for hash tables.

  Parameters:
    AData: pointer to the data to hash.
    ALen: size of the data.

  Returns:
    The hash code. The hash code is guaranteed never to be negative.

  NOTE: There is a newer version called MurmurHash3, which is potentially even
  faster. However, it uses some C language features (to achieve this
  performance) that are not available in Delphi. }

function MurmurHash2(const AData; ALen: Integer): Integer;

implementation

{$OVERFLOWCHECKS OFF}

function MurmurHash2(const AData; ALen: Integer): Integer;
{ https://sites.google.com/site/murmurhash/MurmurHash2.cpp?attredirects=0 }
const
  M = $5bd1e995;
  R = 24;
var
  H, K: Cardinal;
  Data: PByte;
label
  label1, label2, label3, finish;
begin
  H := ALen;
  Data := @AData;

  while (ALen >= 4) do
  begin
    K := PCardinal(Data)^;

    K := K * M;
    K := K xor (K shr R);
    K := K * M;

    H := H * M;
    H := H xor K;

    Inc(Data, 4);
    Dec(ALen, 4);
  end;

  case ALen of
    3: goto label3;
    2: goto label2;
    1: goto label1;
  else
    goto finish;
  end;

label3:
  H := H xor (Data[2] shl 16);

label2:
  H := H xor (Data[1] shl 8);

label1:
  H := H xor Data[0];
  H := H * M;

finish:
  H := H xor (H shr 13);
  H := H * M;
  Result := (H xor (H shr 15)) and $7FFFFFFF;
end;

end.
