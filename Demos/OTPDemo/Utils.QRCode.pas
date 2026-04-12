unit Utils.QRCode;

interface

uses
  Classes, SysUtils
, Windows, VCL.Graphics, Vcl.Imaging.pngimage
, DelphiZXIngQRCode;

type TImageFormat = (ifBMP, ifPNG);

function GenerateQRCode(
  const AFormat: TImageFormat;
  const AText: string;
  const AColor: TColor = clBlack;
  const ADesiredWidth: Integer = 200; const ADesiredHeight: Integer = 200;
  const AEncoding: TQRCodeEncoding = qrAuto; const AQuietZone: Integer = 4
): TMemoryStream;


implementation

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

function GenerateQRCode(
  const AFormat: TImageFormat;
  const AText: string;
  const AColor: TColor;
  const ADesiredWidth: Integer; const ADesiredHeight: Integer;
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

    var LQRCodeBitmap := TBitmap.Create;
    try
      LQRCodeBitmap.SetSize(LQRCode.Rows, LQRCode.Columns);
      LQRCodeBitmap.PixelFormat := pf32bit;
      for LRow := 0 to LQRCode.Rows - 1 do
      begin
        for LColumn := 0 to LQRCode.Columns - 1 do
        begin
          if (LQRCode.IsBlack[LRow, LColumn]) then
          begin
            LQRCodeBitmap.Canvas.Pixels[LColumn, LRow] := AColor;
          end else
          begin
            LQRCodeBitmap.Canvas.Pixels[LColumn, LRow] := clWhite;
          end;
        end;
      end;

      Result := TMemoryStream.Create;
      try
        case AFormat of
          ifBMP: ExportToBMP(ADesiredWidth, ADesiredHeight, LQRCodeBitmap, Result);
          ifPNG: ExportToPNG(ADesiredWidth, ADesiredHeight, LQRCodeBitmap, Result);
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

  finally
    LQRCode.Free;
  end;

end;

(*
begin
  try
    var LText: string := '';
    if not FindCmdLineSwitch('text', LText) then
      raise Exception.Create('specify -text argument');

    var LFileName: string := '';
    if not FindCmdLineSwitch('filename', LFileName) then
      LFileName := 'qrcode.png';

    var LColor: TColor := clBlack;
    var LColorStr := '';
    if not FindCmdLineSwitch('color', LColorStr) then
      LColorStr := '#000000';

    if not LColorStr.StartsWith('#') then
      raise Exception.Create('Color should be in format #RRGGBB');

    LColorStr := LColorStr.Substring(1); // strip #
    var LColor_R := StrToInt('$' + LColorStr.Substring(0, 2));
    var LColor_G := StrToInt('$' + LColorStr.Substring(2, 2));
    var LColor_B := StrToInt('$' + LColorStr.Substring(4, 2));
    LColor := RGB(LColor_R, LColor_G, LColor_B);
    LColorStr := '#' + Format('%s%s%s', [IntToHex(LColor_R, 2), IntToHex(LColor_G, 2), IntToHex(LColor_B, 2)]);

    WriteLn('Text: ' + LText);
    WriteLn('FileName: ' + LFileName);
    WriteLn('Color: ' + LColorStr);
    var LStream := GenerateQRCode(ifPNG, LText, LColor);
    try
      WriteLn('QRCode generated');
      LStream.SaveToFile(LFileName);
      WriteLn('QRCode saved to ' + LFileName);
    finally
      LStream.Free;
    end;

  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
*)

end.
