unit Neslib.SysUtils;
{< Sytem utilities }

{$INCLUDE 'Neslib.inc'}

interface

uses
  System.SysUtils;

var
  { A TFormatSettings record configured for US number settings.
    It uses a period (.) as a decimal separator and comma (,) as thousands
    separator.
    Can be used to convert strings to floating-point values in cases where the
    strings are always formatted to use periods as decimal separators
    (regardless of locale). }
  USFormatSettings: TFormatSettings;

implementation

initialization
  USFormatSettings := TFormatSettings.Create('en-US');
  USFormatSettings.DecimalSeparator := '.';
  USFormatSettings.ThousandSeparator := ',';

end.
