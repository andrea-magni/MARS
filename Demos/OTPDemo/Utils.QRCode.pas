unit Utils.QRCode;

interface

uses
  Classes, SysUtils
{$IFDEF MSWINDOWS}
, Windows, VCL.Graphics, Vcl.Imaging.pngimage, UITypes, NetEncoding
{$ENDIF}
, DelphiZXIngQRCode;

type TImageFormat = (ifSVG {$IFDEF MSWINDOWS}, ifPNG, ifBMP {$ENDIF} );

function GenerateQRCode(
  const AFormat: TImageFormat;
  const AText: string;
  const AFGColor: string = '#000000';
  const ABGColor: string = '#ffffff';
  const ADesiredSize: Integer = 200;
  const AEncoding: TQRCodeEncoding = qrAuto; const AQuietZone: Integer = 4
): TMemoryStream;

function GenerateQRCode_SVG(
  const AText: string;
  const AFGColor: string = '#000000';
  const ABGColor: string = '#ffffff';
  const ADesiredSize: Integer = 200;
  const AEncoding: TQRCodeEncoding = qrAuto; const AQuietZone: Integer = 4
): string;


{$IFDEF MSWINDOWS}
function StringToTColor(const AColorString: string): TColor;
function TColorToString(const AColor: TColor): string;

function GenerateQRCode_PNGBase64(
  const AText: string;
  const AFGColor: string = '#000000';
  const ABGColor: string = '#ffffff';
  const ADesiredSize: Integer = 200;
  const AEncoding: TQRCodeEncoding = qrAuto; const AQuietZone: Integer = 4
): string;
{$ENDIF}

implementation

function StreamToString(const AStream: TStream; const AEncoding: TEncoding = nil): string;
begin
  Result := '';
  if not Assigned(AStream) then
    Exit;
  var LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.UTF8;

  AStream.Position := 0;
  var LBytes: TBytes := [];
  SetLength(LBytes, AStream.Size);
  AStream.Read(LBytes, AStream.Size);
  Result := LEncoding.GetString(LBytes);
end;


function GenerateQRCode_SVG(
  const AText: string;
  const AFGColor: string = '#000000';
  const ABGColor: string = '#ffffff';
  const ADesiredSize: Integer = 200;
  const AEncoding: TQRCodeEncoding = qrAuto; const AQuietZone: Integer = 4
): string;
begin
  var LQRCodeStream := GenerateQRCode(ifSVG, AText);
  try
    Result := StreamToString(LQRCodeStream, TEncoding.UTF8);
  finally
    LQRCodeStream.Free;
  end;
end;


{$IFDEF MSWINDOWS}
function StreamToBase64(const AStream: TStream): string;
var
  LBase64Stream: TStringStream;
begin
  Assert(Assigned(AStream));

  LBase64Stream := TStringStream.Create;
  try
    AStream.Position := 0;
    TNetEncoding.Base64.Encode(AStream, LBase64Stream);
    Result := LBase64Stream.DataString;
  finally
    LBase64Stream.Free;
  end;
end;

function GenerateQRCode_PNGBase64(
  const AText: string;
  const AFGColor: string = '#000000';
  const ABGColor: string = '#ffffff';
  const ADesiredSize: Integer = 200;
  const AEncoding: TQRCodeEncoding = qrAuto; const AQuietZone: Integer = 4
): string;
begin
  var LQRCodeStream := GenerateQRCode(ifPNG, AText);
  try
    Result := StreamToBase64(LQRCodeStream);
  finally
    LQRCodeStream.Free;
  end;
end;


function StringToTColor(const AColorString: string): TColor;
begin
  var LColorRec: TColorRec := Default(TColorRec);

  var LColorString := AColorString.Substring(1);
  if LColorString.Length <> 6 then
    LColorString := '000000'; // black

  LColorRec.R := StrToIntDef('0x' + LColorString.Substring(0,2), 0);
  LColorRec.G := StrToIntDef('0x' + LColorString.Substring(2,2), 0);
  LColorRec.B := StrToIntDef('0x' + LColorString.Substring(4,2), 0);
  Result := LColorRec.Color;
end;

function TColorToString(const AColor: TColor): string;
begin
  var LColorRec: TColorRec;
  LColorRec.Color := AColor;
  Result := '#'
    + IntToHex(LColorRec.R, 2)
    + IntToHex(LColorRec.G, 2)
    + IntToHex(LColorRec.B, 2)
end;

function RenderToBMP(const ADesiredWidth, ADesiredHeight: Integer; const AQRCodeBitmap: TBitmap): TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.Width := ADesiredWidth;
    Result.Height := ADesiredHeight;
    Result.Canvas.Brush.Color := clWhite;
    Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));
    if ((AQRCodeBitmap.Width > 0) and (AQRCodeBitmap.Height > 0)) then
    begin
      var Scale: Double;
      if (Result.Width < Result.Height) then
        Scale := Result.Width / AQRCodeBitmap.Width
      else
        Scale := Result.Height / AQRCodeBitmap.Height;
      Result.Canvas.StretchDraw(Rect(0, 0, Trunc(Scale * AQRCodeBitmap.Width), Trunc(Scale * AQRCodeBitmap.Height)), AQRCodeBitmap);
    end;

  except
    Result.Free;
    raise;
  end;
end;

procedure ExportToBMP(const ADesiredWidth, ADesiredHeight: Integer; const AQRCodeBitmap: TBitmap; const AStream: TStream);
begin
  var LBMPOutput := RenderToBMP(ADesiredWidth, ADesiredHeight, AQRCodeBitmap);
  try
    LBMPOutput.SaveToStream(AStream);
  finally
    LBMPOutput.Free;
  end;
end;

procedure ExportToPNG(const ADesiredWidth, ADesiredHeight: Integer; const AQRCodeBitmap: TBitmap; const AStream: TStream);
begin
  var LTempStream := TMemoryStream.Create;
  try
    var LBMP := RenderToBMP(ADesiredWidth, ADesiredHeight, AQRCodeBitmap);
    try
      var LPNG := TPngImage.Create;
      try
        LPNG.Assign(LBMP);
        LPNG.SaveToStream(AStream);
      finally
        LPNG.Free;
      end;
    finally
      LBMP.Free;
    end;
  finally
    LTempStream.Free;
  end;
end;

{$ENDIF}

function ExportToSVG(
  const AQRCode: TDelphiZXingQRCode;
  const ACellSize: Integer = 10;
  const AFGColor: string = 'black';
  const ABGColor: string = 'white';
  const ATransparentBackground: Boolean = False
): string;
begin
  if (AQRCode.Rows = 0) or (AQRCode.Columns = 0) then
    Exit('');

  const LWidth  = AQRCode.Columns * ACellSize;
  const LHeight = AQRCode.Rows * ACellSize;

  var LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendLine(
      Format(
        '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">'
      , [LWidth, LHeight, LWidth, LHeight]
      )
    );

    // Disegna sfondo solo se NON trasparente
    if not ATransparentBackground then
      LBuilder.AppendLine(
        Format(
          '<rect width="100%%" height="100%%" fill="%s"/>'
        , [ABGColor]
        )
      );

    for var LRow := 0 to AQRCode.Rows - 1 do
      for var LCol := 0 to AQRCode.Columns - 1 do
        if AQRCode.IsBlack[LRow, LCol] then
          LBuilder.AppendFormat(
            '<rect x="%d" y="%d" width="%d" height="%d" fill="%s"/>'
          , [
              LCol * ACellSize, LRow * ACellSize,
              ACellSize, ACellSize,
              AFGColor
            ]
          );

    LBuilder.AppendLine('</svg>');

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function GenerateQRCode(
  const AFormat: TImageFormat;
  const AText: string;
  const AFGColor, ABGColor: string;
  const ADesiredSize: Integer;
  const AEncoding: TQRCodeEncoding; const AQuietZone: Integer
): TMemoryStream;
var
  LQRCode: TDelphiZXingQRCode;
  LRow, LColumn: Integer;
begin
  LQRCode := TDelphiZXingQRCode.Create;
  try
    LQRCode.Data := AText;
    LQRCode.Encoding := AEncoding;
    LQRCode.QuietZone := AQuietZone;

    if AFormat = ifSVG then
    begin
      Result := TMemoryStream.Create;
      try
        const LCellSize = Trunc(ADesiredSize / LQRCode.Columns);
        var LSVG := ExportToSVG(LQRCode, LCellSize, AFGColor, ABGColor);
        var LBytes := TEncoding.UTF8.GetBytes(LSVG);
        Result.Write(LBytes, Length(LBytes));
      except
        Result.Free;
        raise;
      end;
    end

    {$IFDEF MSWINDOWS}
    else
    begin
      var LQRCodeBitmap := TBitmap.Create;
      try
        LQRCodeBitmap.SetSize(LQRCode.Rows, LQRCode.Columns);
        LQRCodeBitmap.PixelFormat := pf32bit;
        var LFGColor := StringToTColor(AFGColor);
        var LBGColor := StringToTColor(ABGColor);
        for LRow := 0 to LQRCode.Rows - 1 do
          for LColumn := 0 to LQRCode.Columns - 1 do
            LQRCodeBitmap.Canvas.Pixels[LColumn, LRow] :=
              if LQRCode.IsBlack[LRow, LColumn] then LFGColor else LBGColor;

        Result := TMemoryStream.Create;
        try
          case AFormat of
            ifBMP: ExportToBMP(ADesiredSize, ADesiredSize, LQRCodeBitmap, Result);
            ifPNG: ExportToPNG(ADesiredSize, ADesiredSize, LQRCodeBitmap, Result);
            else
              raise Exception.Create('Format not supported');
          end;
        except
          Result.Free;
          raise;
        end;

      finally
        LQRCodeBitmap.Free;
      end;
    end;
    {$ENDIF}
  finally
    LQRCode.Free;
  end;

end;

end.
