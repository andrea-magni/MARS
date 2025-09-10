{$define MARS_FIREDAC} // To enable MARS FireDAC support
{$define MARS_FASTREPORT} // To enable MARS FastReport support
{.$define MARS_UNIDAC} // To enable MARS Devart UniDAC support
{.$define MARS_JSON_LEGACY} // To revert to Rest.JSON serializer/deserializer for JSON and TObject descendants

{$define MARS_ZLIB}
{$define MARS_ZIP}

{$if compilerversion = 22}
  {$define DelphiXE}
{$ENDIF}

{$if compilerversion = 23}
  {$define DelphiXE2}
{$ENDIF}

{$if compilerversion = 24}
  {$define DelphiXE3}
{$ENDIF}

{$if compilerversion = 25}
  {$define DelphiXE4}
{$ENDIF}

{$if compilerversion = 26}
  {$define DelphiXE5}
{$ENDIF}

{$if compilerversion = 27}
  {$define DelphiXE6}
{$ENDIF}

{$if compilerversion = 28}
  {$define DelphiXE7}
{$ENDIF}

{$if compilerversion = 29}
  {$define DelphiXE8}
{$ENDIF}

{$if compilerversion = 30}
  {$define Delphi10Seattle}
{$ENDIF}

{$if compilerversion = 31}
  {$define Delphi10Berlin}
{$ENDIF}

{$if compilerversion = 32}
  {$define Delphi10Tokyo}
{$ENDIF}

{$if compilerversion = 33}
  {$define Delphi10Rio}
{$ENDIF}

{$if compilerversion = 34}
  {$define Delphi10Sydney}
{$ENDIF}

{$if compilerversion = 35}
  {$define Delphi11Alexandria}
{$ENDIF}

{$if compilerversion = 36}
  {$define Delphi12Athens}
{$ENDIF}


{$if compilerversion >= 22}
  {$define DelphiXE_UP}
{$ENDIF}

{$if compilerversion >= 23}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
{$ENDIF}

{$if compilerversion >= 24}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
{$ENDIF}

{$if compilerversion >= 25}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
{$ENDIF}

{$if compilerversion >= 26}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
  {$define DelphiXE5_UP}
{$ENDIF}

{$if compilerversion >= 27}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
  {$define DelphiXE5_UP}
  {$define DelphiXE6_UP}
{$ENDIF}

{$if compilerversion >= 28}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
  {$define DelphiXE5_UP}
  {$define DelphiXE6_UP}
  {$define DelphiXE7_UP}
{$ENDIF}

{$if compilerversion >= 29}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
  {$define DelphiXE5_UP}
  {$define DelphiXE6_UP}
  {$define DelphiXE7_UP}
  {$define DelphiXE8_UP}
{$ENDIF}

{$if compilerversion >= 30}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
  {$define DelphiXE5_UP}
  {$define DelphiXE6_UP}
  {$define DelphiXE7_UP}
  {$define DelphiXE8_UP}
  {$define Delphi10Seattle_UP}
{$ENDIF}

{$if compilerversion >= 31}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
  {$define DelphiXE5_UP}
  {$define DelphiXE6_UP}
  {$define DelphiXE7_UP}
  {$define DelphiXE8_UP}
  {$define Delphi10Seattle_UP}
  {$define Delphi10Berlin_UP}
{$ENDIF}

{$if compilerversion >= 32}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
  {$define DelphiXE5_UP}
  {$define DelphiXE6_UP}
  {$define DelphiXE7_UP}
  {$define DelphiXE8_UP}
  {$define Delphi10Seattle_UP}
  {$define Delphi10Berlin_UP}
  {$define Delphi10Tokyo_UP}  
{$ENDIF}

{$if compilerversion >= 33}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
  {$define DelphiXE5_UP}
  {$define DelphiXE6_UP}
  {$define DelphiXE7_UP}
  {$define DelphiXE8_UP}
  {$define Delphi10Seattle_UP}
  {$define Delphi10Berlin_UP}
  {$define Delphi10Tokyo_UP}
  {$define Delphi10Rio_UP}
{$ENDIF}

{$if compilerversion >= 34}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
  {$define DelphiXE5_UP}
  {$define DelphiXE6_UP}
  {$define DelphiXE7_UP}
  {$define DelphiXE8_UP}
  {$define Delphi10Seattle_UP}
  {$define Delphi10Berlin_UP}
  {$define Delphi10Tokyo_UP}
  {$define Delphi10Rio_UP}
  {$define Delphi10Sydney_UP}
{$ENDIF}

{$if compilerversion >= 35}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
  {$define DelphiXE5_UP}
  {$define DelphiXE6_UP}
  {$define DelphiXE7_UP}
  {$define DelphiXE8_UP}
  {$define Delphi10Seattle_UP}
  {$define Delphi10Berlin_UP}
  {$define Delphi10Tokyo_UP}
  {$define Delphi10Rio_UP}
  {$define Delphi10Sydney_UP}
  {$define Delphi11Alexandria_UP}
{$ENDIF}

{$if compilerversion >= 36}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
  {$define DelphiXE5_UP}
  {$define DelphiXE6_UP}
  {$define DelphiXE7_UP}
  {$define DelphiXE8_UP}
  {$define Delphi10Seattle_UP}
  {$define Delphi10Berlin_UP}
  {$define Delphi10Tokyo_UP}
  {$define Delphi10Rio_UP}
  {$define Delphi10Sydney_UP}
  {$define Delphi11Alexandria_UP}
  {$define Delphi12Athens_UP}
{$ENDIF}

{$if compilerversion >= 37}
  {$define DelphiXE_UP}
  {$define DelphiXE2_UP}
  {$define DelphiXE3_UP}
  {$define DelphiXE4_UP}
  {$define DelphiXE5_UP}
  {$define DelphiXE6_UP}
  {$define DelphiXE7_UP}
  {$define DelphiXE8_UP}
  {$define Delphi10Seattle_UP}
  {$define Delphi10Berlin_UP}
  {$define Delphi10Tokyo_UP}
  {$define Delphi10Rio_UP}
  {$define Delphi10Sydney_UP}
  {$define Delphi11Alexandria_UP}
  {$define Delphi12Athens_UP}
  {$define Delphi13Florence_UP}
{$ENDIF}
