unit Neslib.Utf8;
{< Various utilities for working with UTF-8 data.
  Includes fast UTF-8 <=> UTF-16 conversion routines.

  These routines are optimized for speed and don't perform any error checking.
  So, for example, converting invalid UTF-8 to Unicode will result in an invalid
  string.

  As a result, UnicodeToUtf8 is at least 2 times (Windows), but up to 10 times
  (iOS) faster than Delphi's TEncoding.UTF8.GetBytes.

  Also, Utf8ToUnicode is at least 2 times (Windows), but up to 35 times (iOS)
  faster than Delphi's TEncoding.UTF8.GetString. }

{$INCLUDE 'Neslib.inc'}

interface

uses
  System.SysUtils;
  
{ Fast Unicode to UTF-8 conversion.

  Parameters:
    ASource: Unicode string.

  Returns:
    Converted UTF8String. }
function Utf16ToUtf8(const ASource: String): UTF8String; overload; inline;

{ Fast Unicode to UTF-8 conversion using a provided buffer.

  Parameters:
    ASource: Unicode string.
    ASourceLength: the number of characters in ASource to use.
    ABuffer: pointer to the buffer to store the UTF-8 data into. The buffer must
      be at least large enough to store ((ASourceLength + 1) * 3) bytes.

  Returns:
    The number of UTF-8 bytes stored in the buffer. }
function Utf16ToUtf8(const ASource: String; const ASourceLength: Integer;
  const ABuffer: Pointer): Integer; overload; inline;

{ Fast Unicode to UTF-8 conversion using a provided buffer.

  Parameters:
    ASource: pointer to UTF-16 encoded data.
    ASourceLength: the number of characters (NOT bytes) in ASource to use.
    ABuffer: pointer to the buffer to store the UTF-8 data into. The buffer must
      be at least large enough to store ((ASourceLength + 1) * 3) bytes.

  Returns:
    The number of UTF-8 bytes stored in the buffer. }
function Utf16ToUtf8(const ASource: Pointer; const ASourceLength: Integer;
  const ABuffer: Pointer): Integer; overload;

{ Fast UTF-8 to Unicode conversion.

  Parameters:
    ASource: UTF-8 encoded string.

  Returns:
    The converted Unicode string. }
function Utf8ToUtf16(const ASource: UTF8String): String; overload;

{ Fast UTF-8 to Unicode conversion.

  Parameters:
    ASource: null-terminated UTF-8 encoded character array.

  Returns:
    The converted Unicode string. }
function Utf8ToUtf16(const ASource: PUTF8Char): String; overload; inline;

{ Fast UTF-8 to Unicode conversion.

  Parameters:
    ASource: pointer to the UTF-8 data.
    ASourceLength: the number of bytes in ASource to use.

  Returns:
    The Unicode string. }
function Utf8ToUtf16(const ASource: Pointer; const ASourceLength: Integer): String; overload;

{ UTF-8 versions of some routines in System.SysUtils. }

function UpperCase(const AStr: UTF8String): UTF8String; overload;
function LowerCase(const AStr: UTF8String): UTF8String; overload;

{ Integer <=> String }

function IntToUtf8Str(const AValue: Integer): UTF8String; overload;
function IntToUtf8Str(const AValue: Int64): UTF8String; overload;

function UIntToUtf8Str(const AValue: Cardinal): UTF8String; overload;
function UIntToUtf8Str(const AValue: UInt64): UTF8String; overload;

function IntToUtf8Hex(const AValue: Int8): UTF8String; overload; inline;
function IntToUtf8Hex(const AValue: UInt8): UTF8String; overload; inline;
function IntToUtf8Hex(const AValue: Int16): UTF8String; overload; inline;
function IntToUtf8Hex(const AValue: UInt16): UTF8String; overload; inline;
function IntToUtf8Hex(const AValue: Int32): UTF8String; overload; inline;
function IntToUtf8Hex(const AValue: UInt32): UTF8String; overload; inline;
function IntToUtf8Hex(const AValue: Int64): UTF8String; overload; inline;
function IntToUtf8Hex(const AValue: UInt64): UTF8String; overload; inline;

function IntToUtf8Hex(const AValue: Integer; const ADigits: Integer): UTF8String; overload;
function IntToUtf8Hex(const AValue: Int64; const ADigits: Integer): UTF8String; overload;
function IntToUtf8Hex(const AValue: UInt64; const ADigits: Integer): UTF8String; overload;

function StrToInt(const AStr: UTF8String): Integer; overload;
function StrToIntDef(const AStr: UTF8String; const ADefault: Integer): Integer; overload;
function TryStrToInt(const AStr: UTF8String; out AValue: Integer): Boolean; overload;

function StrToUInt(const AStr: UTF8String): Cardinal; overload;
function StrToUIntDef(const AStr: UTF8String; const ADefault: Cardinal): Cardinal; overload;
function TryStrToUInt(const AStr: UTF8String; out AValue: Cardinal): Boolean; overload;

function StrToInt64(const AStr: UTF8String): Int64; overload;
function StrToInt64Def(const AStr: UTF8String; const ADefault: Int64): Int64; overload;
function TryStrToInt64(const AStr: UTF8String; out AValue: Int64): Boolean; overload;

function StrToUInt64(const AStr: UTF8String): UInt64; overload;
function StrToUInt64Def(const AStr: UTF8String; const ADefault: UInt64): UInt64; overload;
function TryStrToUInt64(const AStr: UTF8String; out AValue: UInt64): Boolean; overload;

{ Float <=> String }

function FloatToUtf8Str(const AValue: Double): UTF8String; overload;
function FloatToUtf8Str(const AValue: Double;
  const AFormatSettings: TFormatSettings): UTF8String; overload;

function TextToFloat(const AStr: UTF8String; var AValue: Double): Boolean; overload;
function TextToFloat(const AStr: UTF8String; var AValue: Double;
  const AFormatSettings: TFormatSettings): Boolean; overload;

function StrToFloat(const AStr: UTF8String): Double; overload;
function StrToFloat(const AStr: UTF8String;
  const AFormatSettings: TFormatSettings): Double; overload;

function StrToFloatDef(const AStr: UTF8String;
  const ADefault: Double): Double; overload; inline;
function StrToFloatDef(const AStr: UTF8String; const ADefault: Double;
  const AFormatSettings: TFormatSettings): Double; overload; inline;

function TryStrToFloat(const AStr: UTF8String; out AValue: Double): Boolean; overload; inline;
function TryStrToFloat(const AStr: UTF8String; out AValue: Double;
  const AFormatSettings: TFormatSettings): Boolean; overload; inline;

function TryStrToFloat(const AStr: UTF8String; out AValue: Single): Boolean; overload; inline;
function TryStrToFloat(const AStr: UTF8String; out AValue: Single;
  const AFormatSettings: TFormatSettings): Boolean; overload; 

implementation

uses
  System.SysConst;
  
{ Quick Unicode and UTF-8/UTF-16 recap.

  Unicode codepoint range: U+0..U+10FFFF, except U+D800..U+DFFF (these are used
  for surrogates).
  Codepoints U+0..U+FFFF are stored in a single WideChar.
  Codepoints U+1000..U+10FFFF are stores as two WideChar's (surrogate pair)

  UTF-8 Encodes each Unicode codepoint in 1-4 bytes. The high bits of the first
  byte tell how many bytes are used:

  Range              Scalar codepoint value      1st byte  2nd byte  3rd byte  4th byte
  -------------------------------------------------------------------------------------
  U+0..U+7F          00000000 0xxxxxxx           0xxxxxxx
  U+80..U+7FF        00000yyy yyxxxxxx           110yyyyy  10xxxxxx
  U+800..U+FFFF      zzzzyyyy yyxxxxxx           1110zzzz  10yyyyyy  10xxxxxx
  U+10000..U+10FFFF  000uuuuu zzzzyyyy yyxxxxxx  11110uuu  10uuzzzz  10yyyyyy  10xxxxxx }

{$POINTERMATH ON}

{$IF SizeOf(Extended) >= 10}
  {$DEFINE EXTENDEDHAS10BYTES}
{$ENDIF}

function Utf16ToUtf8(const ASource: String): UTF8String;
var
  SrcLength: Integer;
begin
  SrcLength := Length(ASource);
  if (SrcLength = 0) then
    Exit('');

  SetLength(Result, (SrcLength + 1) * 3);
  SetLength(Result, Utf16ToUtf8(ASource, SrcLength, @Result[Low(UTF8String)]));
end;

function Utf16ToUtf8(const ASource: String; const ASourceLength: Integer;
  const ABuffer: Pointer): Integer;
begin
  Result := Utf16ToUtf8(Pointer(ASource), ASourceLength, ABuffer);
end;

function Utf16ToUtf8(const ASource: Pointer; const ASourceLength: Integer;
  const ABuffer: Pointer): Integer;
var
  SrcLength: Integer;
  S: PWord;
  D, DStart: PByte;
  Codepoint: UInt32;
begin
  SrcLength := ASourceLength;
  S := ASource;
  D := ABuffer;
  DStart := D;

  { Try to convert 2 wide characters at a time if possible. This speeds up the
    process if those 2 characters are both ASCII characters (U+0..U+7F). }
  while (SrcLength >= 2) do
  begin
    if ((PCardinal(S)^ and $FF80FF80) = 0) then
    begin
      { Common case: 2 ASCII characters in a row.
        00000000 0yyyyyyy 00000000 0xxxxxxx => 0yyyyyyy 0xxxxxxx }
      D[0] := S[0]; // 00000000 0yyyyyyy => 0yyyyyyy
      D[1] := S[1]; // 00000000 0xxxxxxx => 0xxxxxxx
      Inc(S, 2);
      Inc(D, 2);
      Dec(SrcLength, 2);
    end
    else
    begin
      Codepoint := S^;
      Inc(S);
      Dec(SrcLength);

      if (Codepoint < $80) then
      begin
        { ASCI character (U+0..U+7F).
          00000000 0xxxxxxx => 0xxxxxxx }
        D^ := Codepoint;
        Inc(D);
      end
      else if (Codepoint < $800) then
      begin
        { 2-byte sequence (U+80..U+7FF)
          00000yyy yyxxxxxx => 110yyyyy 10xxxxxx }
        D^ := (Codepoint shr 6) or $C0;   // 00000yyy yyxxxxxx => 110yyyyy
        Inc(D);
        D^ := (Codepoint and $3F) or $80; // 00000yyy yyxxxxxx => 10xxxxxx
        Inc(D);
      end
      else if (Codepoint >= $D800) and (Codepoint <= $DBFF) then
      begin
        { The codepoint is part of a UTF-16 surrogate pair:
            S[0]: 110110yy yyyyyyyy ($D800-$DBFF, high-surrogate)
            S[1]: 110111xx xxxxxxxx ($DC00-$DFFF, low-surrogate)

          Where the UCS4 codepoint value is:
            0000yyyy yyyyyyxx xxxxxxxx + $00010000 (U+10000..U+10FFFF)

          This can be calculated using:
            (((S[0] and $03FF) shl 10) or (S[1] and $03FF)) + $00010000

          However it can be calculated faster using:
            (S[0] shl 10) + S[1] - $035FDC00

          because:
            * S[0] shl 10: also shifts the leading 110110 to the left, making
              the result $D800 shl 10 = $03600000 too large
            * S[1] is                   $0000DC00 too large
            * So we need to subract     $0360DC00 (sum of the above)
            * But we need to add        $00010000
            * So in total, we subtract  $035FDC00 (difference of the above) }

        Codepoint := (Codepoint shl 10) + S^ - $035FDC00;
        Inc(S);
        Dec(SrcLength);

        { The resulting codepoint is encoded as a 4-byte UTF-8 sequence:

          000uuuuu zzzzyyyy yyxxxxxx => 11110uuu 10uuzzzz 10yyyyyy 10xxxxxx }

        Assert(Codepoint > $FFFF);
        D^ := (Codepoint shr 18) or $F0;           // 000uuuuu zzzzyyyy yyxxxxxx => 11110uuu
        Inc(D);
        D^ := ((Codepoint shr 12) and $3F) or $80; // 000uuuuu zzzzyyyy yyxxxxxx => 10uuzzzz
        Inc(D);
        D^ := ((Codepoint shr 6) and $3F) or $80;  // 000uuuuu zzzzyyyy yyxxxxxx => 10yyyyyy
        Inc(D);
        D^ := (Codepoint and $3F) or $80;          // 000uuuuu zzzzyyyy yyxxxxxx => 10xxxxxx
        Inc(D);
      end
      else
      begin
        { 3-byte sequence (U+800..U+FFFF, excluding U+D800..U+DFFF).
          zzzzyyyy yyxxxxxx => 1110zzzz 10yyyyyy 10xxxxxx }
        D^ := (Codepoint shr 12) or $E0;           // zzzzyyyy yyxxxxxx => 1110zzzz
        Inc(D);
        D^ := ((Codepoint shr 6) and $3F) or $80;  // zzzzyyyy yyxxxxxx => 10yyyyyy
        Inc(D);
        D^ := (Codepoint and $3F) or $80;          // zzzzyyyy yyxxxxxx => 10xxxxxx
        Inc(D);
      end;
    end;
  end;

  { We may have 1 wide character left to encode.
    Use the same process as above. }
  if (SrcLength <> 0) then
  begin
    Codepoint := S^;
    Inc(S);

    if (Codepoint < $80) then
    begin
      D^ := Codepoint;
      Inc(D);
    end
    else if (Codepoint < $800) then
    begin
      D^ := (Codepoint shr 6) or $C0;
      Inc(D);
      D^ := (Codepoint and $3F) or $80;
      Inc(D);
    end
    else if (Codepoint >= $D800) and (Codepoint <= $DBFF) then
    begin
      Codepoint := (Codepoint shl 10) + S^ - $35FDC00;

      Assert(Codepoint > $FFFF);
      D^ := (Codepoint shr 18) or $F0;
      Inc(D);
      D^ := ((Codepoint shr 12) and $3F) or $80;
      Inc(D);
      D^ := ((Codepoint shr 6) and $3F) or $80;
      Inc(D);
      D^ := (Codepoint and $3F) or $80;
      Inc(D);
    end
    else
    begin
      D^ := (Codepoint shr 12) or $E0;
      Inc(D);
      D^ := ((Codepoint shr 6) and $3F) or $80;
      Inc(D);
      D^ := (Codepoint and $3F) or $80;
      Inc(D);
    end;
  end;

  Result := D - DStart;
end;

function Utf8ToUtf16(const ASource: UTF8String): String;
begin
  Result := Utf8ToUtf16(@ASource[Low(UTF8String)], Length(ASource));
end;

function Utf8ToUtf16(const ASource: PUTF8Char): String;
var
  S: PUTF8Char;
begin
  if (ASource = nil) then
    Exit('');

  S := ASource;

  { Unroll loop }
  while (S^ <> #0) do
  begin
    Inc(S);
    if (S^ = #0) then
      Break;

    Inc(S);
    if (S^ = #0) then
      Break;

    Inc(S);
    if (S^ = #0) then
      Break;

    Inc(S);
  end;

  Result := Utf8ToUtf16(ASource, S - ASource);
end;

function Utf8ToUtf16(const ASource: Pointer; const ASourceLength: Integer): String;
var
  SrcLength: Integer;
  S: PByte;
  D, DStart: PWord;
  Codepoint: UInt32;
begin
  SrcLength := ASourceLength;
  if (SrcLength = 0) then
    Exit('');

  SetLength(Result, SrcLength + 1);
  S := ASource;
  D := Pointer(Result);
  DStart := D;

  { Try to convert 4 bytes at a time. This speeds up the process if those 4
    bytes are all ASCII characters (U+0..U+7F) }
  while (SrcLength >= 4) do
  begin
    if ((PCardinal(S)^ and $80808080) = 0) then
    begin
      { Common case: 4 ASCII characters in a row.
        0zzzzzzz 0yyyyyyy 0xxxxxxx 0wwwwwww => 00000000 0zzzzzzz 00000000 0yyyyyyy 00000000 0xxxxxxx 00000000 0wwwwwww }
      D[0] := S[0]; // 0zzzzzzz => 00000000 0zzzzzzz
      D[1] := S[1]; // 0yyyyyyy => 00000000 0yyyyyyy
      D[2] := S[2]; // 0xxxxxxx => 00000000 0xxxxxxx
      D[3] := S[3]; // 0wwwwwww => 00000000 0wwwwwww
      Inc(S, 4);
      Inc(D, 4);
      Dec(SrcLength, 4);
    end
    else
    begin
      Codepoint := S^;
      Inc(S);
      if (Codepoint < $80) then
      begin
        { ASCI character (U+0..U+7F).
          0xxxxxxx => 00000000 0xxxxxxx }
        D^ := Codepoint;
        Dec(SrcLength);
      end
      else
      if ((Codepoint shr 5) = $06) then
      begin
        { 2-byte sequence (U+80..U+7FF)
          110yyyyy 10xxxxxx => 00000yyy yyxxxxxx }
        D^ := ((Codepoint shl 6) and $7FF) // 110yyyyy => 00000yyy yy000000
            + (S^ and $3F);                // 10xxxxxx => 00000000 00xxxxxx
        Inc(S);
        Dec(SrcLength, 2);
      end
      else
      begin
        if ((Codepoint shr 4) = $0E) then
        begin
          { 3-byte sequence (U+800..U+FFFF, excluding U+D800..U+DFFF).
            1110zzzz 10yyyyyy 10xxxxxx => zzzzyyyy yyxxxxxx }
          Codepoint :=
             ((Codepoint shl 12) and $FFFF) // 1110zzzz => zzzz0000 00000000
           + ((S^ shl 6) and $FFF);         // 10yyyyyy => 0000yyyy yy000000
          Inc(S);
          Inc(Codepoint, S^ and $3F);       // 10xxxxxx => 00000000 00xxxxxx
          Inc(S);
          Dec(SrcLength, 3);
          Assert(CodePoint <= $FFFF);
          D^ := Codepoint;
        end
        else
        begin
          Assert((Codepoint shr 3) = $1E);
          { 4-byte sequence (U+10000..U+10FFFF).
            11110uuu 10uuzzzz 10yyyyyy 10xxxxxx => 000uuuuu zzzzyyyy yyxxxxxx }
          Codepoint :=
             ((Codepoint shl 18) and $1FFFFF) // 11110uuu => 000uuu00 00000000 00000000
           + ((S^ shl 12) and $3FFFF);        // 10uuzzzz => 000000uu zzzz0000 00000000
          Inc(S);
          Inc(Codepoint, (S^ shl 6) and $FFF);// 10yyyyyy => 00000000 0000yyyy yy000000
          Inc(S);
          Inc(Codepoint, S^ and $3F);         // 10xxxxxx => 00000000 00000000 00xxxxxx
          Inc(S);
          Dec(SrcLength, 4);

          { The value $00010000 must be subtracted from this codepoint, and the
            result must be converted to a UTF-16 surrogate pair:
              D[0]: 110110yy yyyyyyyy ($D800-$DBFF, high-surrogate)
              D[1]: 110111xx xxxxxxxx ($DC00-$DFFF, low-surrogate) }

          Assert(Codepoint > $FFFF);
          Dec(Codepoint, $00010000);
          D^ := $D800 + (Codepoint shr 10);
          Inc(D);
          D^ := $DC00 + (Codepoint and $3FF);
        end;
      end;
      Inc(D);
    end;
  end;

  { We may 1-3 bytes character left to encode.
    Use the same process as above. }
  while (SrcLength > 0) do
  begin
    Codepoint := S^;
    Inc(S);
    if (Codepoint < $80) then
    begin
      D^ := Codepoint;
      Dec(SrcLength);
    end
    else
    if ((Codepoint shr 5) = $06) then
    begin
      D^ := ((Codepoint shl 6) and $7FF) + (S^ and $3F);
      Inc(S);
      Dec(SrcLength, 2);
    end
    else
    begin
      if ((Codepoint shr 4) = $0E) then
      begin
        Codepoint := ((Codepoint shl 12) and $FFFF) + ((S^ shl 6) and $FFF);
        Inc(S);
        Inc(Codepoint, S^ and $3F);
        Inc(S);
        Dec(SrcLength, 3);
        Assert(CodePoint <= $FFFF);
        D^ := Codepoint;
      end
      else
      begin
        Assert((Codepoint shr 3) = $1E);
        Codepoint := ((Codepoint shl 18) and $1FFFFF) + ((S^ shl 12) and $3FFFF);
        Inc(S);
        Inc(Codepoint, (S^ shl 6) and $FFF);
        Inc(S);
        Inc(Codepoint, S^ and $3F);
        Inc(S);
        Dec(SrcLength, 4);

        Assert(CodePoint > $FFFF);
        D^ := $D7C0 + (Codepoint shr 10);
        Inc(D);
        D^ := $DC00 + (Codepoint and $3FF);
      end;
    end;
    Inc(D);
  end;
  SetLength(Result, D - DStart);
end;

function UpperCase(const AStr: UTF8String): UTF8String;
var
  I, Len: Integer;
  DstP, SrcP: PUTF8Char;
  Ch: UTF8Char;
begin
  Len := Length(AStr);
  SetLength(Result, Len);
  if (Len > 0) then
  begin
    DstP := PUTF8Char(Pointer(Result));
    SrcP := PUTF8Char(Pointer(AStr));
    for I := 0 to Len - 1 do
    begin
      Ch := SrcP^;
      case Ch of
        'a'..'z':
          Ch := UTF8Char(Byte(Ch) xor $20);
      end;
      DstP^ := Ch;
      Inc(DstP);
      Inc(SrcP);
    end;
  end;
end;

function LowerCase(const AStr: UTF8String): UTF8String;
var
  I, Len: Integer;
  DstP, SrcP: PUTF8Char;
  Ch: UTF8Char;
begin
  Len := Length(AStr);
  SetLength(Result, Len);
  if (Len > 0) then
  begin
    DstP := PUTF8Char(Pointer(Result));
    SrcP := PUTF8Char(Pointer(AStr));
    for I := 0 to Len - 1 do
    begin
      Ch := SrcP^;
      case Ch of
        'A'..'Z':
          Ch := UTF8Char(Byte(Ch) or $20);
      end;
      DstP^ := Ch;
      Inc(DstP);
      Inc(SrcP);
    end;
  end;
end;

const
  TwoDigitLookup : packed array [0..99, 0..1] of UTF8Char =
    ('00','01','02','03','04','05','06','07','08','09',
     '10','11','12','13','14','15','16','17','18','19',
     '20','21','22','23','24','25','26','27','28','29',
     '30','31','32','33','34','35','36','37','38','39',
     '40','41','42','43','44','45','46','47','48','49',
     '50','51','52','53','54','55','56','57','58','59',
     '60','61','62','63','64','65','66','67','68','69',
     '70','71','72','73','74','75','76','77','78','79',
     '80','81','82','83','84','85','86','87','88','89',
     '90','91','92','93','94','95','96','97','98','99');

type
  _TwoChar = record
    case Integer of
      0: (Ch: array [0..1] of UTF8Char);
      1: (U16: UInt16);
  end;

const
  TwoHexLookup : packed array [0..255] of _TwoChar =
  (
    (Ch:'00'),(Ch:'01'),(Ch:'02'),(Ch:'03'),(Ch:'04'),(Ch:'05'),(Ch:'06'),(Ch:'07'),(Ch:'08'),(Ch:'09'),(Ch:'0A'),(Ch:'0B'),(Ch:'0C'),(Ch:'0D'),(Ch:'0E'),(Ch:'0F'),
    (Ch:'10'),(Ch:'11'),(Ch:'12'),(Ch:'13'),(Ch:'14'),(Ch:'15'),(Ch:'16'),(Ch:'17'),(Ch:'18'),(Ch:'19'),(Ch:'1A'),(Ch:'1B'),(Ch:'1C'),(Ch:'1D'),(Ch:'1E'),(Ch:'1F'),
    (Ch:'20'),(Ch:'21'),(Ch:'22'),(Ch:'23'),(Ch:'24'),(Ch:'25'),(Ch:'26'),(Ch:'27'),(Ch:'28'),(Ch:'29'),(Ch:'2A'),(Ch:'2B'),(Ch:'2C'),(Ch:'2D'),(Ch:'2E'),(Ch:'2F'),
    (Ch:'30'),(Ch:'31'),(Ch:'32'),(Ch:'33'),(Ch:'34'),(Ch:'35'),(Ch:'36'),(Ch:'37'),(Ch:'38'),(Ch:'39'),(Ch:'3A'),(Ch:'3B'),(Ch:'3C'),(Ch:'3D'),(Ch:'3E'),(Ch:'3F'),
    (Ch:'40'),(Ch:'41'),(Ch:'42'),(Ch:'43'),(Ch:'44'),(Ch:'45'),(Ch:'46'),(Ch:'47'),(Ch:'48'),(Ch:'49'),(Ch:'4A'),(Ch:'4B'),(Ch:'4C'),(Ch:'4D'),(Ch:'4E'),(Ch:'4F'),
    (Ch:'50'),(Ch:'51'),(Ch:'52'),(Ch:'53'),(Ch:'54'),(Ch:'55'),(Ch:'56'),(Ch:'57'),(Ch:'58'),(Ch:'59'),(Ch:'5A'),(Ch:'5B'),(Ch:'5C'),(Ch:'5D'),(Ch:'5E'),(Ch:'5F'),
    (Ch:'60'),(Ch:'61'),(Ch:'62'),(Ch:'63'),(Ch:'64'),(Ch:'65'),(Ch:'66'),(Ch:'67'),(Ch:'68'),(Ch:'69'),(Ch:'6A'),(Ch:'6B'),(Ch:'6C'),(Ch:'6D'),(Ch:'6E'),(Ch:'6F'),
    (Ch:'70'),(Ch:'71'),(Ch:'72'),(Ch:'73'),(Ch:'74'),(Ch:'75'),(Ch:'76'),(Ch:'77'),(Ch:'78'),(Ch:'79'),(Ch:'7A'),(Ch:'7B'),(Ch:'7C'),(Ch:'7D'),(Ch:'7E'),(Ch:'7F'),
    (Ch:'80'),(Ch:'81'),(Ch:'82'),(Ch:'83'),(Ch:'84'),(Ch:'85'),(Ch:'86'),(Ch:'87'),(Ch:'88'),(Ch:'89'),(Ch:'8A'),(Ch:'8B'),(Ch:'8C'),(Ch:'8D'),(Ch:'8E'),(Ch:'8F'),
    (Ch:'90'),(Ch:'91'),(Ch:'92'),(Ch:'93'),(Ch:'94'),(Ch:'95'),(Ch:'96'),(Ch:'97'),(Ch:'98'),(Ch:'99'),(Ch:'9A'),(Ch:'9B'),(Ch:'9C'),(Ch:'9D'),(Ch:'9E'),(Ch:'9F'),
    (Ch:'A0'),(Ch:'A1'),(Ch:'A2'),(Ch:'A3'),(Ch:'A4'),(Ch:'A5'),(Ch:'A6'),(Ch:'A7'),(Ch:'A8'),(Ch:'A9'),(Ch:'AA'),(Ch:'AB'),(Ch:'AC'),(Ch:'AD'),(Ch:'AE'),(Ch:'AF'),
    (Ch:'B0'),(Ch:'B1'),(Ch:'B2'),(Ch:'B3'),(Ch:'B4'),(Ch:'B5'),(Ch:'B6'),(Ch:'B7'),(Ch:'B8'),(Ch:'B9'),(Ch:'BA'),(Ch:'BB'),(Ch:'BC'),(Ch:'BD'),(Ch:'BE'),(Ch:'BF'),
    (Ch:'C0'),(Ch:'C1'),(Ch:'C2'),(Ch:'C3'),(Ch:'C4'),(Ch:'C5'),(Ch:'C6'),(Ch:'C7'),(Ch:'C8'),(Ch:'C9'),(Ch:'CA'),(Ch:'CB'),(Ch:'CC'),(Ch:'CD'),(Ch:'CE'),(Ch:'CF'),
    (Ch:'D0'),(Ch:'D1'),(Ch:'D2'),(Ch:'D3'),(Ch:'D4'),(Ch:'D5'),(Ch:'D6'),(Ch:'D7'),(Ch:'D8'),(Ch:'D9'),(Ch:'DA'),(Ch:'DB'),(Ch:'DC'),(Ch:'DD'),(Ch:'DE'),(Ch:'DF'),
    (Ch:'E0'),(Ch:'E1'),(Ch:'E2'),(Ch:'E3'),(Ch:'E4'),(Ch:'E5'),(Ch:'E6'),(Ch:'E7'),(Ch:'E8'),(Ch:'E9'),(Ch:'EA'),(Ch:'EB'),(Ch:'EC'),(Ch:'ED'),(Ch:'EE'),(Ch:'EF'),
    (Ch:'F0'),(Ch:'F1'),(Ch:'F2'),(Ch:'F3'),(Ch:'F4'),(Ch:'F5'),(Ch:'F6'),(Ch:'F7'),(Ch:'F8'),(Ch:'F9'),(Ch:'FA'),(Ch:'FB'),(Ch:'FC'),(Ch:'FD'),(Ch:'FE'),(Ch:'FF')
  );

const
  OneHexLookup: packed array [0..$F] of UTF8Char = '0123456789ABCDEF';

function _IntToStr32(const AValue: Cardinal; const ANegative: Boolean): UTF8String;
var
  I, J, K: Cardinal;
  Digits, NewLen: Integer;
  P: PUTF8Char;
begin
  I := AValue;
  if (I >= 10000) then
    if (I >= 1000000) then
      if (I >= 100000000) then
        Digits := 9 + Ord(I >= 1000000000)
      else
        Digits := 7 + Ord(I >= 10000000)
    else
      Digits := 5 + Ord(I >= 100000)
  else
    if (I >= 100) then
      Digits := 3 + Ord(I >= 1000)
    else
      Digits := 1 + Ord(I >= 10);

  NewLen  := Digits + Ord(ANegative);
  SetLength(Result, NewLen);

  P := PUTF8Char(Result);
  P^ := '-';
  Inc(P, Ord(ANegative));

  if (Digits > 2) then
  begin
    repeat
      J  := I div 100;
      K  := J * 100;
      K  := I - K;
      I  := J;
      Dec(Digits, 2);
      PWord(P + Digits)^ := Word(TwoDigitLookup[K]);
    until (Digits <= 2);
  end;

  if (Digits = 2) then
    PWord(P + Digits - 2)^ := Word(TwoDigitLookup[I])
  else
    P^ := UTF8Char(I or Ord('0'));
end;

function _IntToStr64(const AValue: UInt64; const ANegative: Boolean): UTF8String;
var
  I64, J64, K64: UInt64;
  I32, J32, K32, L32 : Cardinal;
  Digits: Byte;
  P: PUTF8Char;
  NewLen: Integer;
begin
  if (ANegative and (AValue <= High(Integer))) or
     (not ANegative and (AValue <= High(Cardinal)))
  then
    Exit(_IntToStr32(AValue, ANegative));

  I64 := AValue;
  if (I64 >= 100000000000000) then
    if (I64 >= 10000000000000000) then
      if (I64 >= 1000000000000000000) then
        if (I64 >= 10000000000000000000) then
          Digits := 20
        else
          Digits := 19
      else
        Digits := 17 + Ord(I64 >= 100000000000000000)
    else
      Digits := 15 + Ord(I64 >= 1000000000000000)
  else
    if (I64 >= 1000000000000) then
      Digits := 13 + Ord(I64 >= 10000000000000)
    else
      if (I64 >= 10000000000) then
        Digits := 11 + Ord(I64 >= 100000000000)
      else
        Digits := 10;

  NewLen  := Digits + Ord(ANegative);
  SetLength(Result, NewLen);

  P := PUTF8Char(Result);
  P^ := '-';
  Inc(P, Ord(ANegative));

  if (Digits = 20) then
  begin
    P^ := '1';
    Inc(P);
    Dec(I64, 10000000000000000000);
    Dec(Digits);
  end;

  if (Digits > 17) then
  begin
    if (Digits = 19) then
    begin
      P^ := '0';
      while (I64 >= 1000000000000000000) do
      begin
        Dec(I64, 1000000000000000000);
        Inc(P^);
      end;
      Inc(P);
    end;

    P^ := '0';
    while (I64 >= 100000000000000000) do
    begin
      Dec(I64, 100000000000000000);
      Inc(P^);
    end;

    Inc(P);
    Digits := 17;
  end;

  J64 := I64 div 100000000;
  K64 := I64 - (J64 * 100000000);
  I32 := K64;
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  PWord(P + Digits - 2)^ := Word(TwoDigitLookup[K32]);

  I32 := J32 div 100;
  L32 := I32 * 100;
  L32 := J32 - L32;
  PWord(P + Digits - 4)^ := Word(TwoDigitLookup[L32]);

  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  PWord(P + Digits - 6)^ := Word(TwoDigitLookup[K32]);
  PWord(P + Digits - 8)^ := Word(TwoDigitLookup[J32]);

  Dec(Digits, 8);
  I32 := J64;

  if (Digits > 2) then
  begin
    repeat
      J32 := I32 div 100;
      K32 := J32 * 100;
      K32 := I32 - K32;
      I32 := J32;
      Dec(Digits, 2);
      PWord(P + Digits)^ := Word(TwoDigitLookup[K32]);
    until Digits <= 2;
  end;

  if (Digits = 2) then
    PWord(P + Digits - 2)^ := Word(TwoDigitLookup[I32])
  else
    P^ := UTF8Char(I32 or Ord('0'));
end;

function IntToUtf8Str(const AValue: Integer): UTF8String;
begin
  if (AValue < 0) then
    Result := _IntToStr32(-AValue, True)
  else
    Result := _IntToStr32(AValue, False);
end;

function IntToUtf8Str(const AValue: Int64): UTF8String;
begin
  if (AValue < 0) then
    Result := _IntToStr64(-AValue, True)
  else
    Result := _IntToStr64(AValue, False);
end;

function UIntToUtf8Str(const AValue: Cardinal): UTF8String;
begin
  Result := _IntToStr32(AValue, False);
end;

function UIntToUtf8Str(const AValue: UInt64): UTF8String;
begin
  Result := _IntToStr64(AValue, False);
end;

function _UInt32ToHexString(AValue: UInt32; ADigits, ANewLen: Integer;
  const AFillCh: UTF8Char): UTF8String;
var
  P: Integer;
  Ptr: PWord;
begin
  if (ADigits > ANewLen) then
  begin
    SetLength(Result, ADigits);
    FillChar(Result[Low(UTF8String)], ADigits - ANewLen, AFillCh);
    P := Low(UTF8String) + ADigits - ANewLen;
  end
  else
  begin
    SetLength(Result, ANewLen);
    P := Low(UTF8String);
  end;

  Ptr := @Result[P + ANewLen];
  while (ANewLen >= 2) do
  begin
    Dec(ANewLen, 2);
    Dec(Ptr);
    Ptr^ := TwoHexLookup[AValue and $FF].U16;
    AValue := AValue shr 8;
  end;

  if (ANewLen = 1) then
    Result[P] := OneHexLookup[AValue and $F];
end;

function _UInt64ToHexString(AValue: UInt64; ADigits, ANewLen: Integer;
  const AFillCh: UTF8Char): UTF8String;
var
  P: Integer;
  Ptr: PWord;
begin
  if (ADigits > ANewLen) then
  begin
    SetLength(Result, ADigits);
    FillChar(Result[Low(UTF8String)], ADigits - ANewLen, AFillCh);
    P := Low(UTF8String) + ADigits - ANewLen;
  end
  else
  begin
    SetLength(Result, ANewLen);
    P := Low(UTF8String);
  end;

  Ptr := @Result[P + ANewLen];
  while (ANewLen >= 2) do
  begin
    Dec(ANewLen, 2);
    Dec(Ptr);
    Ptr^ := TwoHexLookup[AValue and $FF].U16;
    AValue := AValue shr 8;
  end;

  if (ANewLen = 1) then
    Result[P] := OneHexLookup[AValue and $F];
end;

function _UIntToHex32(const AValue: UInt32; const ADigits: Integer): UTF8String;
var
  I: UInt32;
  NewLen: Integer;
begin
  NewLen := 1;
  I := AValue shr 4;
  while (I > 0) do
  begin
    Inc(NewLen);
    I := I shr 4;
  end;
  Result := _UInt32ToHexString(AValue, ADigits, NewLen, '0');
end;

function _UIntToHex64(const AValue: UInt64; const ADigits: Integer): UTF8String;
var
  I: UInt64;
  NewLen: Integer;
begin
  NewLen := 1;
  I := AValue shr 4;
  while (I > 0) do
  begin
    Inc(NewLen);
    I := I shr 4;
  end;
  Result := _UInt64ToHexString(AValue, ADigits, NewLen, '0');
end;

function IntToUtf8Hex(const AValue: Int8): UTF8String;
begin
  Result := IntToUtf8Hex(UInt8(AValue), SizeOf(Int8) * 2);
end;

function IntToUtf8Hex(const AValue: UInt8): UTF8String;
begin
  Result := IntToUtf8Hex(AValue, SizeOf(UInt8) * 2);
end;

function IntToUtf8Hex(const AValue: Int16): UTF8String;
begin
  Result := IntToUtf8Hex(UInt16(AValue), SizeOf(Int16) * 2);
end;

function IntToUtf8Hex(const AValue: UInt16): UTF8String;
begin
  Result := IntToUtf8Hex(AValue, SizeOf(UInt16) * 2);
end;

function IntToUtf8Hex(const AValue: Int32): UTF8String;
begin
  Result := IntToUtf8Hex(UInt32(AValue), SizeOf(Int32) * 2);
end;

function IntToUtf8Hex(const AValue: UInt32): UTF8String;
begin
  Result := IntToUtf8Hex(AValue, SizeOf(UInt32) * 2);
end;

function IntToUtf8Hex(const AValue: Int64): UTF8String;
begin
  Result := IntToUtf8Hex(UInt64(AValue), SizeOf(Int64) * 2);
end;

function IntToUtf8Hex(const AValue: UInt64): UTF8String;
begin
  Result := IntToUtf8Hex(AValue, SizeOf(UInt64) * 2);
end;

function IntToUtf8Hex(const AValue: Integer; const ADigits: Integer): UTF8String;
begin
  Result := _UIntToHex32(Cardinal(AValue), ADigits);
end;

function IntToUtf8Hex(const AValue: Int64; const ADigits: Integer): UTF8String;
begin
  Result := _UIntToHex64(AValue, ADigits);
end;

function IntToUtf8Hex(const AValue: UInt64; const ADigits: Integer): UTF8String;
begin
  Result := _UIntToHex64(AValue, ADigits);
end;

procedure ConvertErrorFmt(const AResString: PResStringRec;
  const AArgs: array of const);
begin
  raise EConvertError.CreateResFmt(AResString, AArgs);
end;

function TryStrToInt(const AStr: UTF8String; out AValue: Integer): Boolean;
const
  FirstIndex = Low(UTF8String);
var
  I, Dig, Val: Integer;
  Sign, Empty: Boolean;
begin
  if (AStr = '') then
  begin
    AValue := 0;
    Exit(False);
  end;

  I := FirstIndex;
  Val := 0;
  Sign := False;
  Empty := True;

  while (AStr[I] = ' ') do
    Inc(I);

  if (AStr[I] = '-') then
  begin
    Sign := True;
    Inc(I);
  end else if (AStr[I] = '+') then
    Inc(I);

  if ((AStr[I] = '0') and (I < High(AStr)) and ((AStr[I + 1] = 'X') or (AStr[I + 1] = 'x'))) or
      (AStr[I] = '$') or
      (AStr[I] = 'X') or
      (AStr[I] = 'x') then
  begin
    // Hex
    if (AStr[I] = '0') then
      Inc(I);

    Inc(I);
    while True do
    begin
      case AStr[I] of
       '0'..'9': Dig := Ord(AStr[I]) - Ord('0');
       'A'..'F': Dig := Ord(AStr[I]) - Ord('A') + 10;
       'a'..'f': Dig := Ord(AStr[I]) - Ord('a') + 10;
      else
        Break;
      end;

      if (Val < 0) or (Val > (High(Integer) shr 3)) then
        Break;

      Val := (Val shl 4) + Dig;
      Inc(I);
      Empty := False;
    end;

    if Sign then
      Val := -Val;
  end
  else
  begin
    // Decimal
    while True do
    begin
      case AStr[I] of
        '0'..'9': Dig := Ord(AStr[I]) - Ord('0');
      else
        Break;
      end;

      if (Val < 0) or (Val > (High(Integer) div 10)) then
        Break;

      Val := (Val * 10) + Dig;
      Inc(I);
      Empty := False;
    end;

    if Sign then
      Val := -Val;

    if (Val <> 0) and (Sign <> (Val < 0)) then
      Dec(I);
  end;

  if ((AStr[I] <> #0) or Empty) then
  begin
    AValue := 0;
    Exit(False);
  end;

  AValue := Val;
  Result := True;
end;

function TryStrToInt64(const AStr: UTF8String; out AValue: Int64): Boolean;
const
  FirstIndex = Low(UTF8String);
var
  Val: Int64;
  I, Dig: Integer;
  Sign, Empty: Boolean;
begin
  if (AStr = '') then
  begin
    AValue := 0;
    Exit(False);
  end;

  I := FirstIndex;
  Val := 0;
  Sign := False;
  Empty := True;

  while (AStr[I] = ' ') do
    Inc(I);

  if (AStr[I] = '-') then
  begin
    Sign := True;
    Inc(I);
  end else if (AStr[I] = '+') then
    Inc(I);

  if ((AStr[I] = '0') and (I < High(AStr)) and ((AStr[I + 1] = 'X') or (AStr[I + 1] = 'x'))) or
      (AStr[I] = '$') or
      (AStr[I] = 'X') or
      (AStr[I] = 'x') then
  begin
    // Hex
    if (AStr[I] = '0') then
      Inc(I);

    Inc(I);
    while True do
    begin
      case AStr[I] of
       '0'..'9': Dig := Ord(AStr[I]) - Ord('0');
       'A'..'F': Dig := Ord(AStr[I]) - Ord('A') + 10;
       'a'..'f': Dig := Ord(AStr[I]) - Ord('a') + 10;
      else
        Break;
      end;

      if (Val < 0) or (Val > (High(Int64) shr 3)) then
        Break;

      Val := (Val shl 4) + Dig;
      Inc(I);
      Empty := False;
    end;

    if Sign then
      Val := -Val;
  end
  else
  begin
    // Decimal
    while True do
    begin
      case AStr[I] of
        '0'..'9': Dig := Ord(AStr[I]) - Ord('0');
      else
        Break;
      end;

      if (Val < 0) or (Val > (High(Int64) div 10)) then
        Break;

      Val := (Val * 10) + Dig;
      Inc(I);
      Empty := False;
    end;

    if Sign then
      Val := -Val;

    if (Val <> 0) and (Sign <> (Val < 0)) then
      Dec(I);
  end;

  if ((AStr[I] <> #0) or Empty) then
  begin
    AValue := 0;
    Exit(False);
  end;

  AValue := Val;
  Result := True;
end;

function TryStrToUInt64(const AStr: UTF8String; out AValue: UInt64): Boolean;
const
  FirstIndex = Low(UTF8String);
var
  Val: UInt64;
  I, Dig: Integer;
  Sign, Empty: Boolean;
begin
  if (AStr = '') then
  begin
    AValue := 0;
    Exit(False);
  end;

  I := FirstIndex;
  Val := 0;
  Sign := False;
  Empty := True;

  while (AStr[I] = ' ') do
    Inc(I);

  if (AStr[I] = '-') then
  begin
    Sign := True;
    Inc(I);
  end else if (AStr[I] = '+') then
    Inc(I);

  if ((AStr[I] = '0') and (I < High(AStr)) and ((AStr[I + 1] = 'X') or (AStr[I + 1] = 'x'))) or
      (AStr[I] = '$') or
      (AStr[I] = 'X') or
      (AStr[I] = 'x') then
  begin
    // Hex
    if (AStr[I] = '0') then
      Inc(I);

    Inc(I);
    while True do
    begin
      case AStr[I] of
       '0'..'9': Dig := Ord(AStr[I]) - Ord('0');
       'A'..'F': Dig := Ord(AStr[I]) - Ord('A') + 10;
       'a'..'f': Dig := Ord(AStr[I]) - Ord('a') + 10;
      else
        Break;
      end;

      if (Val > (High(UInt64) shr 4)) then
        Break;

      if (Sign) and (Dig <> 0) then
        Break;

      Val := (Val shl 4) + Cardinal(Dig);
      Inc(I);
      Empty := False;
    end;
  end
  else
  begin
    // Decimal
    while True do
    begin
      case AStr[I] of
        '0'..'9': Dig := Ord(AStr[I]) - Ord('0');
      else
        Break;
      end;

      if (Val >= 1844674407370955161) then
      begin
        if (Val > 1844674407370955161) or (High(UInt64) - (Val * 10) < Dig) then
          Break;
      end;

      if (Sign) and (Dig <> 0) then
        Break;

      Val := (Val * 10) + Cardinal(Dig);
      Inc(I);
      Empty := False;
    end;
  end;

  if ((AStr[I] <> #0) or Empty) then
  begin
    AValue := 0;
    Exit(False);
  end;

  AValue := Val;
  Result := True;
end;

function StrToInt(const AStr: UTF8String): Integer;
begin
  if (not TryStrToInt(AStr, Result)) then
    ConvertErrorFmt(@SInvalidInteger, [AStr]);
end;

function StrToIntDef(const AStr: UTF8String; const ADefault: Integer): Integer;
begin
  if (not TryStrToInt(AStr, Result)) then
    Result := ADefault;
end;

function StrToUInt(const AStr: UTF8String): Cardinal;
var
  I: Int64;
begin
  if (not TryStrToInt64(AStr, I)) or (I < Cardinal.MinValue) or (I > Cardinal.MaxValue) then
    ConvertErrorFmt(@SInvalidInteger, [AStr]);

  Result := I;
end;

function StrToUIntDef(const AStr: UTF8String; const ADefault: Cardinal): Cardinal;
var
  I: Int64;
begin
  if (not TryStrToInt64(AStr, I)) or (I < Cardinal.MinValue) or (I > Cardinal.MaxValue) then
    Result := ADefault
  else
    Result := I;
end;

function TryStrToUInt(const AStr: UTF8String; out AValue: Cardinal): Boolean;
var
  I: Int64;
begin
  Result := TryStrToInt64(AStr, I) and (I >= Cardinal.MinValue) and (I <= Cardinal.MaxValue);
  AValue := I;
end;

function StrToInt64(const AStr: UTF8String): Int64;
begin
  if (not TryStrToInt64(AStr, Result)) then
    ConvertErrorFmt(@SInvalidInteger, [AStr]);
end;

function StrToInt64Def(const AStr: UTF8String; const ADefault: Int64): Int64;
begin
  if (not TryStrToInt64(AStr, Result)) then
    Result := ADefault;
end;

function StrToUInt64(const AStr: UTF8String): UInt64;
begin
  if (not TryStrToUInt64(AStr, Result)) then
    ConvertErrorFmt(@SInvalidInteger, [AStr]);
end;

function StrToUInt64Def(const AStr: UTF8String; const ADefault: UInt64): UInt64;
begin
  if (not TryStrToUInt64(AStr, Result)) then
    Result := ADefault;
end;

function InternalFloatToText(ABuffer: PUtf8Char; const AValue: Extended;
  AFormat: TFloatFormat; APrecision, ADigits: Integer;
  const AFormatSettings: TFormatSettings): Integer;
const
  CMinExtPrecision = 2;
  {$IFDEF EXTENDEDHAS10BYTES}
  CMaxExtPrecision = 18;
  {$ELSE}
  CMaxExtPrecision = 17;
  {$ENDIF}
  CGenExpDigits = 9999;

  CExpChar = 'E';
  CMinusSign = '-';
  CPlusSign = '+';
  CZero = '0';
  CSpecial: array [0..1] of UTF8String = ('INF', 'NAN');
  CCurrencyFormats: array [0..3] of UTF8String = ('$*@@@', '*$@@@', '$ *@@', '* $@@');
  CNegCurrencyFormats: array[0..15] of UTF8String = (
    '($*)@', '-$*@@', '$-*@@', '$*-@@', '(*$)@', '-*$@@',
    '*-$@@', '*$-@@', '-* $@', '-$ *@', '* $-@',
    '$ *-@', '$ -*@', '*- $@', '($ *)', '(* $)');
var
  FloatRec: TFloatRec;

  LDigits: Integer;
  LExponent: Cardinal;
  LUseENotation: Boolean;

  LFloatRecDigit: Integer;
  LNextThousand: Integer;

  procedure AppendChar(const AChar: UTF8Char);
  begin
    ABuffer^ := AChar;
    Inc(ABuffer);
    Inc(Result);
  end;

  procedure AppendString(const AStr: UTF8String);
  var
    L: Integer;
  begin
    L := Length(AStr);

    if (L > 0) then
    begin
      Move(AStr[Low(UTF8String)], ABuffer^, L);
      Inc(ABuffer, L);
      Inc(Result, L);
    end;
  end;

  function GetDigit: Byte;
  begin
    Result := FloatRec.Digits[LFloatRecDigit];

    if (Result = 0) then
      Result := Ord('0')
    else
      Inc(LFloatRecDigit);
  end;

  procedure FormatNumber;
  var
    K: Integer;
  begin
    if (ADigits > CMaxExtPrecision) then
      LDigits := CMaxExtPrecision
    else
      LDigits := ADigits;

    K := FloatRec.Exponent;
    if (K > 0) then
    begin
      { Find the position of the next thousand separator }
      LNextThousand := 0;
      if (AFormat <> ffFixed) then
        LNextThousand := ((K - 1) mod 3) + 1;

      repeat
        { Append the next digit }
        AppendChar(UTF8Char(GetDigit));

        { Update loop counters }
        Dec(K);
        Dec(LNextThousand);

        { Try to append the thousands separator and reset the counter }
        if (LNextThousand = 0) and (K > 0) then
        begin
          LNextThousand := 3;

          if (AFormatSettings.ThousandSeparator <> #0) then
            AppendChar(UTF8Char(AFormatSettings.ThousandSeparator));
        end;
      until (K = 0);

    end
    else
      AppendChar(CZero);

    { If there are ADigits left to fill }
    if (LDigits <> 0) then
    begin
      { Put in the decimal separator if it was specified }
      if (AFormatSettings.DecimalSeparator <> #0) then
        AppendChar(UTF8Char(AFormatSettings.DecimalSeparator));

      { If there is negative exponent }
      if (K < 0) then
      begin
        { Fill with zeroes until the exponent or ADigits are exhausted}
        repeat
          AppendChar(CZero);

          Inc(K);
          Dec(LDigits);
        until (K = 0) or (LDigits = 0);
      end;

      if (LDigits > 0) then
      begin
        { Exponent was filled, there are still ADigits left to fill }
        repeat
          AppendChar(UTF8Char(GetDigit));
          Dec(LDigits);
        until (LDigits <= 0);
      end;
    end;
  end;

  procedure FormatExponent;
  var
    LMinCnt, LExponent: Integer;
    LExpString: UTF8String;
    LDigitCnt: Integer;
  begin
    { Adjust digit count }
    if (ADigits > 4) then
      LMinCnt := 0
    else
      LMinCnt := ADigits;

    { Get exponent }
    LExponent := FloatRec.Exponent - 1;

    { Place the E character into position }
    AppendChar(CExpChar);

    if (FloatRec.Digits[0] <> 0) then
    begin
      if (LExponent < 0) then
      begin
        LExponent := -LExponent;
        AppendChar(CMinusSign);
      end
      else
      begin
        if (AFormat <> ffGeneral) then
          AppendChar(CPlusSign);
      end;
    end
    else
    begin
      if (AFormat <> ffGeneral) then
        AppendChar(CPlusSign);
      LExponent := 0;
    end;

    LExpString := IntToUtf8Str(LExponent);
    LDigitCnt := Length(LExpString);

    while (LDigitCnt < LMinCnt) do
    begin
      AppendChar(CZero);
      Inc(LDigitCnt);
    end;

    AppendString(LExpString);
  end;

begin
  LFloatRecDigit := 0;
  Result := 0;

  { Check min and max precisions for an Extended }
  if (APrecision < CMinExtPrecision) then
    APrecision := CMinExtPrecision
  else if (APrecision > CMaxExtPrecision) then
    APrecision := CMaxExtPrecision;

  { Check the number of ADigits to use }
  if (AFormat in [ffGeneral, ffExponent]) then
    LDigits := CGenExpDigits
  else
    LDigits := ADigits;

  { Decode the float }
  FloatToDecimal(FloatRec, AValue, fvExtended, APrecision, LDigits);
  {$IFDEF EXTENDEDHAS10BYTES}
  LExponent := UInt16(FloatRec.Exponent) - $7FFF;
  {$ELSE}
  LExponent := UInt16(FloatRec.Exponent) - $7FF;
  {$ENDIF}

  { Check for INF or NAN}
  if (LExponent < 2) then
  begin
    { Append the sign to output buffer }
    if FloatRec.Negative then
      AppendChar(CMinusSign);

    AppendString(CSpecial[LExponent]);
    Exit;
  end;

  if (not (AFormat in [ffGeneral..ffCurrency])) or
    ((FloatRec.Exponent > APrecision) and (AFormat <> ffExponent))
  then
    AFormat := ffGeneral;

  case AFormat of
    ffGeneral:
      begin
        { Append the sign to output buffer }
        if FloatRec.Negative then
          AppendChar(CMinusSign);

        LUseENotation := False;

        { Obtain digit count and whether to use the E notation }
        LDigits := FloatRec.Exponent;
        if (LDigits > APrecision) or (LDigits < -3) then
        begin
          LDigits := 1;
          LUseENotation := True;
        end;

        if (LDigits > 0) then
        begin
          { Append the ADigits that precede decimal separator }
          while (LDigits > 0) do
          begin
            AppendChar(UTF8Char(GetDigit));
            Dec(LDigits);
          end;

          { Append the decimal separator and the following digit }
          if (FloatRec.Digits[LFloatRecDigit] <> 0) then
          begin
            AppendChar(UTF8Char(AFormatSettings.DecimalSeparator));

            { Append the ADigits that come after the decimal separator }
            while (FloatRec.Digits[LFloatRecDigit] <> 0) do
              AppendChar(UTF8Char(GetDigit));
          end;

          if LUseENotation then
            FormatExponent;
        end
        else
        begin
          AppendChar(CZero);

          if (FloatRec.Digits[0] <> 0) then
          begin
            AppendChar(UTF8Char(AFormatSettings.DecimalSeparator));
            LDigits := -LDigits;

            { Append zeroes to fulfill the exponent }
            while (LDigits > 0) do
            begin
              AppendChar(CZero);
              Dec(LDigits);
            end;

            { Attach all the other ADigits now }
            while (FloatRec.Digits[LFloatRecDigit] <> 0) do
              AppendChar(UTF8Char(GetDigit));
          end;
        end;
      end;

    ffExponent:
      begin
        { Append the sign to output buffer }
        if FloatRec.Negative then
          AppendChar(CMinusSign);

        { Append the first digit and the decimal separator }
        AppendChar(UTF8Char(GetDigit));
        AppendChar(UTF8Char(AFormatSettings.DecimalSeparator));

        { Append ADigits based on the APrecision requirements }
        Dec(APrecision);
        repeat
          AppendChar(UTF8Char(GetDigit));
          Dec(APrecision);
        until (APrecision = 0);

        FormatExponent;
      end;

    ffNumber, ffFixed:
      begin
        { Append the sign to output buffer }
        if FloatRec.Negative then
          AppendChar(CMinusSign);

        FormatNumber;
      end;
  end;
end;

function FloatToUtf8Str(const AValue: Double): UTF8String;
var
  Buffer: array [0..63] of UTF8Char;
begin
  SetString(Result, Buffer, InternalFloatToText(Buffer, AValue, ffGeneral, 15, 0, FormatSettings));
end;

function FloatToUtf8Str(const AValue: Double;
  const AFormatSettings: TFormatSettings): UTF8String;
var
  Buffer: array [0..63] of UTF8Char;
begin
  SetString(Result, Buffer, InternalFloatToText(Buffer, AValue, ffGeneral, 15, 0, AFormatSettings));
end;

const
  mIE = $0001;
  mOE = $0008;

{$IFNDEF EXTERNALLINKER}
{$IF Defined(CPUX86)}
const
  CWNear = $133F;

function TestAndClearFPUExceptions(AExceptionMask: Word): Boolean;
asm
      PUSH    ECX
      MOV     CX, AX
      FSTSW   AX
      TEST    AX, CX
      JNE     @@bad
      XOR     EAX, EAX
      INC     EAX
      JMP     @@exit
@@bad:
      XOR     EAX, EAX
@@exit:
      POP     ECX
      FCLEX
      RET
end;
{$ELSEIF Defined(CPUX64)}
const
  MXCSRNear = $1F80;

function TestAndClearSSEExceptions(AExceptionMask: UInt32): Boolean;
var
  MXCSR: UInt32;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  MXCSR := GetMXCSR;
  Result := ((MXCSR and $003F) and AExceptionMask) = 0;
  ResetMXCSR;
  {$WARN SYMBOL_PLATFORM ON}
end;
{$ENDIF}
{$ENDIF}

function InternalTextToDouble(ABuffer: PUTF8Char; out AValue: Double;
  const AFormatSettings: TFormatSettings): Boolean;
const
  {$IFDEF EXTENDEDHAS10BYTES}
  CMaxExponent = 4999;
  {$ELSE}
  CMaxExponent = 1024;
  {$ENDIF}
  CExponent = 'E';
  CPlus = '+';
  CMinus = '-';

var
  {$IFNDEF EXTERNALLINKER}
  {$IF Defined(CPUX86)}
  LSavedCtrlWord: Word;
  {$ELSEIF Defined(CPUX64)}
  LSavedMXCSR: UInt32;
  {$ENDIF}
  {$ENDIF}
  LPower: Integer;
  LSign: SmallInt;
  LResult: Double;
  LCurrChar: UTF8Char;

  procedure NextChar;
  begin
    LCurrChar := ABuffer^;
    Inc(ABuffer);
  end;

  procedure SkipWhitespace; 
  begin
    { Skip white spaces }
    while (LCurrChar = ' ') do
      NextChar;
  end;

  function ReadSign: SmallInt;
  begin
    Result := 1;
    if (LCurrChar = CPlus) then
      NextChar()
    else if (LCurrChar = CMinus) then
    begin
      NextChar();
      Result := -1;
    end;
  end;

  function ReadNumber(var AOut: Double): Integer;
  begin
    Result := 0;
    while (LCurrChar >= '0') and (LCurrChar <= '9') do
    begin
      AOut := AOut * 10;
      AOut := AOut + Ord(LCurrChar) - Ord('0');

      NextChar();
      Inc(Result);
    end;
  end;

  function ReadExponent: SmallInt;
  var
    LSign: SmallInt;
  begin
    LSign := ReadSign();
    Result := 0;
    while (LCurrChar >= '0') and (LCurrChar <= '9') do
    begin
      Result := Result * 10;
      Result := Result + Ord(LCurrChar) - Ord('0');
      NextChar();
    end;

    if (Result > CMaxExponent) then
      Result := CMaxExponent;

    Result := Result * LSign;
  end;

begin
  { Prepare }
  Result := False;
  NextChar;

  {$IFNDEF EXTERNALLINKER}
  {$IF Defined(CPUX86)}
  { Prepare the FPU }
  LSavedCtrlWord := Get8087CW;
  TestAndClearFPUExceptions(0);
  Set8087CW(CWNear);
  {$ELSEIF Defined(CPUX64)}
  { Prepare the FPU }
  {$WARN SYMBOL_PLATFORM OFF}
  LSavedMXCSR := GetMXCSR;
  TestAndClearSSEExceptions(0);
  SetMXCSR(MXCSRNear);
  {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}
  {$ENDIF}

  { Skip white spaces }
  SkipWhitespace;

  { Exit if nothing to do }
  if (LCurrChar <> #0) then
  begin
    { Detect the sign of the number }
    LSign := ReadSign;
    if (LCurrChar <> #0) then
    begin
      { De result }
      LResult := 0;

      { Read the integer and fractionary parts }
      ReadNumber(LResult);
      if (LCurrChar = UTF8Char(AFormatSettings.DecimalSeparator)) then
      begin
        NextChar;
        LPower := -ReadNumber(LResult);
      end 
      else
        LPower := 0;

      { Read the exponent and adjust the power }
      if (UTF8Char(Ord(LCurrChar) and $DF) = CExponent) then
      begin
        NextChar;
        Inc(LPower, ReadExponent);
      end;

      { Skip white spaces }
      SkipWhitespace;

      { Continue only if the buffer is depleted }
      if (LCurrChar = #0) then
      begin
        { Calculate the final number }
        {$IFDEF NEXTGEN}
        try
          LResult := Power10(LResult, LPower) * LSign;
          AValue := LResult;
          Result := True;
        except
          Result := False;
        end;
        {$ELSE}
        LResult := Power10(LResult, LPower) * LSign;
        AValue := LResult;
        {$ENDIF}

        {$IFNDEF EXTERNALLINKER}
        {$IF Defined(CPUX86)}
        { Final check that everything went OK }
        Result := TestAndClearFPUExceptions(mIE + mOE);
        {$ELSEIF Defined(CPUX64)}
        { Final check that everything went OK }
        Result := TestAndClearSSEExceptions(mIE + mOE);
        {$ENDIF}
        {$ENDIF}
      end;
    end;
  end;

  { Clear Math Exceptions }
  {$IFNDEF EXTERNALLINKER}
  {$IF Defined(CPUX86)}
  Set8087CW(LSavedCtrlWord);
  {$ELSEIF Defined(CPUX64)}
  {$WARN SYMBOL_PLATFORM OFF}
  SetMXCSR(LSavedMXCSR);
  {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF CPUX64}
  {$ENDIF}
end;

function TextToFloat(const AStr: UTF8String; var AValue: Double): Boolean; 
begin
  Result := InternalTextToDouble(PUTF8Char(AStr), AValue, FormatSettings);
end;

function TextToFloat(const AStr: UTF8String; var AValue: Double;
  const AFormatSettings: TFormatSettings): Boolean; overload;
begin
  Result := InternalTextToDouble(PUTF8Char(AStr), AValue, AFormatSettings);
end;

function StrToFloat(const AStr: UTF8String): Double; 
begin
  if (not TextToFloat(AStr, Result, FormatSettings)) then
    ConvertErrorFmt(@SInvalidFloat, [AStr]);
end;

function StrToFloat(const AStr: UTF8String;
  const AFormatSettings: TFormatSettings): Double; 
begin
  if (not TextToFloat(AStr, Result, AFormatSettings)) then
    ConvertErrorFmt(@SInvalidFloat, [AStr]);
end;

function StrToFloatDef(const AStr: UTF8String;
  const ADefault: Double): Double; 
begin
  if (not TextToFloat(AStr, Result, FormatSettings)) then
    Result := ADefault;
end;

function StrToFloatDef(const AStr: UTF8String; const ADefault: Double;
  const AFormatSettings: TFormatSettings): Double; 
begin
  if (not TextToFloat(AStr, Result, AFormatSettings)) then
    Result := ADefault;
end;

function TryStrToFloat(const AStr: UTF8String; out AValue: Double): Boolean; 
begin
  Result := TextToFloat(AStr, AValue, FormatSettings);
end;

function TryStrToFloat(const AStr: UTF8String; out AValue: Double;
  const AFormatSettings: TFormatSettings): Boolean; 
begin
  Result := TextToFloat(AStr, AValue, AFormatSettings);
end;

function TryStrToFloat(const AStr: UTF8String; out AValue: Single): Boolean; 
begin
  Result := TryStrToFloat(AStr, AValue, FormatSettings);
end;

function TryStrToFloat(const AStr: UTF8String; out AValue: Single;
  const AFormatSettings: TFormatSettings): Boolean; overload;
var
  Val: Double;
begin
  Result := TextToFloat(AStr, Val, AFormatSettings);
  if Result then
  begin
    if (Val < Single.MinValue) or (Val > Single.MaxValue) then
      Result := False;
  end;
  
  if Result then
    AValue := Val;
end;

end.
