{******************************************************************************}
{                                                                              }
{       Delphi cross platform socket library                                   }
{                                                                              }
{       Copyright (c) 2017 WiNDDRiVER(soulawing@gmail.com)                     }
{                                                                              }
{       Homepage: https://github.com/winddriver/Delphi-Cross-Socket            }
{                                                                              }
{******************************************************************************}
unit Net.CrossHttpRouter;

interface

uses
  Net.CrossHttpServer;

type
  /// <summary>
  ///   ·��
  /// </summary>
  /// <remarks>
  ///   ���� TCrossHttpServer.Route(), Get(), Post() ��
  /// </remarks>
  TNetCrossRouter = class
  public
    /// <summary>
    ///   ��̬�ļ�·��
    /// </summary>
    /// <param name="ALocalDir">
    ///   ����Ŀ¼
    /// </param>
    class function &Static(const ALocalDir, AFileParamName: string): TCrossHttpRouterProc2; static;

    /// <summary>
    ///   �ļ��б�·��
    /// </summary>
    /// <param name="APath">
    ///   ����·��, �ò�����Ϊ����Ŀ¼�б�ҳ���ж�λ��·��
    /// </param>
    /// <param name="ALocalDir">
    ///   ����Ŀ¼
    /// </param>
    class function Dir(const APath, ALocalDir, ADirParamName: string): TCrossHttpRouterProc2; static;

    /// <summary>
    ///   ����Ĭ����ҳ�ļ��ľ�̬�ļ�·��
    /// </summary>
    /// <param name="ALocalDir">
    ///   ����Ĭ����ҳ�ļ��ı���Ŀ¼
    /// </param>
    /// <param name="ADefIndexFiles">
    ///   Ĭ�ϵ���ҳ�ļ�,��˳��ѡ��,���ҵ��ĸ���ʹ���ĸ�
    /// </param>
    class function Index(const ALocalDir, AFileParamName: string; const ADefIndexFiles: TArray<string>): TCrossHttpRouterProc2; static;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils, System.NetEncoding;

{ TNetCrossRouter }

class function TNetCrossRouter.Index(const ALocalDir, AFileParamName: string;
  const ADefIndexFiles: TArray<string>): TCrossHttpRouterProc2;
var
  LDefIndexFiles: TArray<string>;
begin
  if (ADefIndexFiles <> nil) then
    LDefIndexFiles := ADefIndexFiles
  else
    LDefIndexFiles := [
      'index.html',
      'main.html',
      'index.js',
      'main.js',
      'index.htm',
      'main.htm'
    ];

  Result :=
    procedure(ARequest: ICrossHttpRequest; AResponse: ICrossHttpResponse; var AHandled: Boolean)
    var
      LPath, LFile, LDefMainFile: string;
    begin
      LPath := ALocalDir;
      LFile := ARequest.Params[AFileParamName];

      if (LFile = '') then
      begin
        for LDefMainFile in LDefIndexFiles do
        begin
          LFile := TPath.Combine(LPath, LDefMainFile);
          if TFile.Exists(LFile) then
          begin
            AResponse.SendFile(LFile);
            AHandled := True;
            Exit;
          end;
        end;
      end else
      begin
        LFile := TPath.Combine(LPath, LFile);
        if TFile.Exists(LFile) then
        begin
          AResponse.SendFile(LFile);
          AHandled := True;
          Exit;
        end;
      end;

      AHandled := False;
    end;
end;

class function TNetCrossRouter.Static(
  const ALocalDir, AFileParamName: string): TCrossHttpRouterProc2;
begin
  Result :=
    procedure(ARequest: ICrossHttpRequest; AResponse: ICrossHttpResponse; var AHandled: Boolean)
    var
      LFile: string;
    begin
      AHandled := True;
      LFile := TPath.Combine(ALocalDir, ARequest.Params[AFileParamName]);
      if (LFile = '') then
      begin
        AHandled := False;
        Exit;
      end;
      LFile := TPath.GetFullPath(LFile);
      AResponse.SendFile(LFile);
    end;
end;

{$region 'Dir��������'}
type
  THttpFileEntry = class
    Name: string;
    Size: Int64;
    Time: TDateTime;
    Directory: Boolean;
    ReadOnly: Boolean;
    SysFile: Boolean;
    Hidden: Boolean;
  end;

function BuildDirList(const ARealPath, ARequestPath, AHome: string): string;
  function SmartSizeToStr(ABytes: Int64): string;
  const
    KBYTES = Int64(1024);
    MBYTES = KBYTES * 1024;
    GBYTES = MBYTES * 1024;
    TBYTES = GBYTES * 1024;
    PBYTES = TBYTES * 1024;
  begin
    if (ABytes < KBYTES) then
      Result := Format('%dB', [ABytes])
    else if (ABytes < MBYTES) then
      Result := Format('%.2fK ', [ABytes / KBYTES])
    else if (ABytes < GBYTES) then
      Result := Format('%.2fM ', [ABytes / MBYTES])
    else if (ABytes < TBYTES) then
      Result := Format('%.2fG ', [ABytes / GBYTES])
    else if (ABytes < PBYTES) then
      Result := Format('%.2fT ', [ABytes / TBYTES])
    else
      Result := Format('%.2fP ', [ABytes / PBYTES]);
  end;

  function FormatDirEntry(const APath: string; F: THttpFileEntry): string;
  var
    Attr, Link, NameString, SizeString: string;
  begin
    if (F.Name = '.') or (F.Name = '..') then
    begin
      Result := '';
      Exit;
    end;

    // drwsh
    Attr := '-rw--';
    if F.Directory then
    begin
      Attr[1] := 'd';
      SizeString := '';
      NameString := '<font color="#0074d9">' + F.Name + '</font>';
    end
    else
    begin
      SizeString := SmartSizeToStr(F.Size);
      NameString := F.Name;
    end;

    if F.ReadOnly then
      Attr[3] := '-';

    if F.SysFile then
      Attr[4] := 's';

    if F.Hidden then
      Attr[5] := 'h';

    if (APath[Length(APath)] = '/') then
      Link := TNetEncoding.URL.Encode(F.Name)
    else
      Link := APath + '/' + TNetEncoding.URL.Encode(F.Name);

    Result :=
      '<TD WIDTH="55%" NOWRAP><A HREF="' + Link + '">' + NameString + '</A></TD>' +
      '<TD WIDTH="5%" ALIGN="LEFT" NOWRAP>' + Attr + '</TD>' +
      '<TD WIDTH="%15" ALIGN="right" NOWRAP>' + SizeString + '</TD>' +
      '<TD WIDTH="5%" NOWRAP></TD>' +
      '<TD WIDTH="20%" NOWRAP>' + FormatDateTime('YYYY-MM-DD HH:NN:SS', F.Time) + '</TD>';
  end;

  function PathToURL(const APath, AHome: string): string;
    function _NormalizePath(const APathStr: string): string;
    begin
      Result := APathStr.Replace('\/', '/').Replace('\\', '/').Replace('//', '/');
      if (Result = '') then
        Result := '/'
      else
      begin
        if (Result.Chars[0] <> '/') then
          Result := '/' + Result;
        if (Result.Chars[Result.Length - 1] <> '/') then
          Result := Result + '/';
      end;
    end;
  var
    LPath, LHome, LSubPath: string;
    LPathArr, LHomeArr: TArray<string>;
    I: Integer;
  begin
    LPath := _NormalizePath(APath);
    LHome := _NormalizePath(AHome);

    LPathArr := LPath.Split(['/', '\'], TStringSplitOptions.ExcludeEmpty);
    LHomeArr := LHome.Split(['/', '\'], TStringSplitOptions.ExcludeEmpty);
    if Length(LHomeArr) > Length(LPathArr) then Exit('');

    I := 0;
    while True do
    begin
      if (I >= Length(LPathArr)) or (I >= Length(LHomeArr))
        or not SameText(LPathArr[I], LHomeArr[I]) then Break;
      Inc(I);
    end;

    Result := Format('<A HREF="%s"><b><font color="#ff4136">Home</font></b></A> / ',
      [LHome]);
    LSubPath := LHome;

    while True do
    begin
      if (I >= Length(LPathArr)) then Break;

      LSubPath := LSubPath + LPathArr[I] + '/';
      Result := Result + Format('<A HREF="%s"><b><font color="#0074d9">%s</font></b></A> / ',
        [LSubPath, LPathArr[I]]);
      Inc(I);
    end;
  end;
var
  Status: Integer;
  F: TSearchRec;
  DirList: TStringList;
  FileList: TStringList;
  Data: THttpFileEntry;
  i: Integer;
  Total: Cardinal;
  TotalBytes: Int64;
  HTML: string;
begin
  DirList := TStringList.Create;
  FileList := TStringList.Create;
  Status := FindFirst(TPath.Combine(ARealPath, '*.*'), faAnyFile, F);
  while Status = 0 do
  begin
    if (F.Name <> '.') and (F.Name <> '..') then
    begin
      Data := THttpFileEntry.Create;
      Data.Name := F.Name;
      Data.Size := F.Size;
      Data.Time := F.TimeStamp;
      Data.Directory := ((F.Attr and faDirectory) <> 0);
      Data.ReadOnly := ((F.Attr and faReadOnly) <> 0);
      Data.SysFile := ((F.Attr and faSysFile) <> 0);
      Data.Hidden := ((F.Attr and faHidden) <> 0);

      if ((F.Attr and faDirectory) <> 0) then
        DirList.AddObject(Data.Name, Data)
      else
        FileList.AddObject(Data.Name, Data);
    end;

    Status := FindNext(F);
  end;
  FindClose(F);
  DirList.Sort;
  FileList.Sort;

  HTML :=
    '<HTML>' +
    '<HEAD>' +
    '' +
    '<STYLE TYPE="text/css">' +
    '.dirline {font-family: "Microsoft Yahei",simsun,arial; color: #111111; font-style: normal;}' +
    '.hline {height:0;overflow:hiddne;border-top:1px solid #C3C3C3}' +
    '.vline {width:0;overflow:hiddne;border-left:1px solid #C3C3C3}' +
    'a:link {text-decoration: none; color: #111111;}' +
    'a:visited {text-decoration: none; color: #111111;} ' +
    'a:hover {text-decoration: underline; color: #0000FF;}' +
    'a:active {text-decoration: none; color: #111111;}' +
    '</STYLE>' +
    '<TITLE>�ļ��б�</TITLE>' +
    '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">' +
    '</HEAD>' +
    '<BODY>' +
    '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER">' +
    '<TR><TD>' + PathToURL(ARequestPath, AHome) + '<BR><BR></TD></TR></TABLE>';

  TotalBytes := 0;
  Total := DirList.Count + FileList.Count;
  if Total <= 0 then
    HTML := HTML + '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER"><TR><TD><BR>��Ŀ¼</TD></TR></TABLE>'
  else
  begin
    HTML := HTML +
      // ����
      '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER">' +
      '<TR>' +
      '<TD WIDTH="55%" NOWRAP>�ļ���</TD>' +
      '<TD WIDTH="5%" ALIGN="LEFT" NOWRAP>����</TD>' +
      '<TD WIDTH="%15" ALIGN="right" NOWRAP>��С</TD>' +
      '<TD WIDTH="5%" NOWRAP></TD>' +
      '<TD WIDTH="20%" NOWRAP>�޸�ʱ��</TD>' +
      '</TR>' +
      '</TABLE>' +

      // һ����ɫ����
      '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER">' +
      '<TR><TD HEIGHT="3"><div class="hline"></div></TD></TR>' +
      '</TABLE>' +

      // �ļ��б���
      '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER">';

    for i := 0 to DirList.Count - 1 do
    begin
      Data := THttpFileEntry(DirList.Objects[i]);
      HTML := HTML + '<TR>' + FormatDirEntry(ARequestPath, Data) + '</TR>';
      FreeAndNil(Data);
    end;

    for i := 0 to FileList.Count - 1 do
    begin
      Data := THttpFileEntry(FileList.Objects[i]);
      HTML := HTML + '<TR>' + FormatDirEntry(ARequestPath, Data) + '</TR>';
      TotalBytes := TotalBytes + Data.Size;
      FreeAndNil(Data);
    end;

    HTML := HTML + '</TABLE>' +
      // һ����ɫ����
      '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER">' +
      '<TR><TD HEIGHT="3"><div class="hline"></div></TD></TR>' +
      '</TABLE>' +

      // ҳ��ͳ����Ϣ
      '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER">' +
      '<TR>' +
      '<TD WIDTH="55%" NOWRAP>' + Format('Ŀ¼: %d, �ļ�: %d', [DirList.Count, FileList.Count]) + '</TD>' +
      '<TD WIDTH="5%" NOWRAP></TD>' +
      '<TD WIDTH="%15" ALIGN="right" NOWRAP>' + SmartSizeToStr(TotalBytes) + '</TD>' +
      '<TD WIDTH="5%" NOWRAP></TD>' +
      '<TD WIDTH="20%" NOWRAP></TD>' +
      '</TR>' +
      '</TABLE>';
  end;

  FreeAndNil(DirList);
  FreeAndNil(FileList);

  HTML := HTML + '</BODY></HTML>';
  Result := HTML;
end;
{$endregion}

class function TNetCrossRouter.Dir(
  const APath, ALocalDir, ADirParamName: string): TCrossHttpRouterProc2;
begin
  Result :=
    procedure(ARequest: ICrossHttpRequest; AResponse: ICrossHttpResponse; var AHandled: Boolean)
    var
      LFile: string;
    begin
      AHandled := True;

      LFile := TPath.Combine(ALocalDir, ARequest.Params[ADirParamName]);
      if (LFile = '') then
      begin
        AHandled := False;
        Exit;
      end;

      LFile := TPath.GetFullPath(LFile);
      if (TDirectory.Exists(LFile)) then
        AResponse.Send(BuildDirList(LFile, ARequest.Path, APath))
      else if TFile.Exists(LFile) then
        AResponse.SendFile(LFile)
      else
        AHandled := False;
    end;
end;

end.
