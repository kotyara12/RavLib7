unit rHttpUtils;

interface

uses
  Classes;

type
  TSSLVer = (sslNone, sslSSL2, sslSSL23, sslSSL3, sslTLS1, sslTLS11, sslTLS12);
  TProxyProtocol = (ppHttp, ppHttps, ppSocks4, ppSocks5);
  TProxyProtocols = set of TProxyProtocol;
  TProxyLevel = (plTransparent, plAnonymous, plDistorting, plElite);
  TProxyLevels = set of TProxyLevel;

  PProxyData = ^TProxyData;
  TProxyData = record
    sServer: string;
    iPort: Word;
    sUsername: string;
    sPassword: string;
  end;

  TFtpServer = record
    sHost: string;
    iPort: Word;
    sLogin: string;
    sPassword: string;
    bPassiveMode: Boolean;
  end;

function  rHttp_ExtractFilePath(const sUrl: string): string;
function  rHttp_ExtractFileName(const sUrl: string): string;
function  rHttp_ExcludePathDelimiter(const sUrl: string): string;
function  rHttp_ConcatParams(const sBase, sPart: string): string;
function  rHttp_IsSSL(const sUrl: string): Boolean;

function  rHttp_Ping(const sServer: string; const iTimeout: Integer = 3000; const iTryCount: Integer = 3): Boolean;

procedure rHttp_DownloadFileEx(const pProxy: PProxyData; const sUrl, sFilename: string; const aSSL: TSSLVer);
procedure rHttp_DownloadFile(const sUrl, sFilename: string; const bUseSSL: Boolean);
function  rHttp_GetEx(const pProxy: PProxyData; const sUrl: string; const aSSL: TSSLVer; const bRedirect: Boolean = True): string;
function  rHttp_Get(const sUrl: string; const bUseSSL: Boolean; const bRedirect: Boolean = True): string; overload;
function  rHttp_Get(const sUrl: string; const bRedirect: Boolean = True): string; overload;
function  rHttp_PostEx(const pProxy: PProxyData; const sUrl: string; const slValues: TStrings; const aSSL: TSSLVer; const bRedirect: Boolean = True): string;
function  rHttp_Post(const sUrl: string; const slValues: TStrings; const bUseSSL: Boolean; const bRedirect: Boolean = True): string; overload;
function  rHttp_Post(const sUrl: string; const slValues: TStrings; const bRedirect: Boolean = True): string; overload;
function  rHttp_Post(const sUrl, sValues: string; const bUseSSL: Boolean; const bRedirect: Boolean = True): string; overload;
function  rHttp_Post(const sUrl, sValues: string; const bRedirect: Boolean = True): string; overload;

function  rFtp_CreateParams(const aHost: string; const aPort: Word; const aLogin, aPassword: string; const aPassiveMode: Boolean): TFtpServer;
procedure rFtp_TransferFile(const aFtp: TFtpServer; const bUpload: Boolean; const sFileName, sFtpDir, sLocDir: string);

function  rProxy_StrToProxy(const sProxy: string): TProxyData;
function  rProxy_ProxyToStr(const aProxy: TProxyData): string;
function  rProxy_ProxyDbNet(var aList: TStringList;
  const aProtocols: TProxyProtocols; const aLevels: TProxyLevels; const aCountry: string;
  const aPort, aUpTime, aRTime, aMinCnt: Integer): Integer;

const
  sRqAcceptDef                  = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8';
  sRqUserAgentDef               = 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.80 Safari/537.36';
  sRqAcceptEncodingZip          = 'gzip, deflate, sdch';

implementation

uses
  Windows, SysUtils, StrUtils, rxStrUtils, SynaCode, rParse, rFileProcs, rDialogs,
  IdTCPConnection, IdTCPClient, IdIcmpClient, IdHTTP, IdCompressorZLib, IdSSLOpenSSL,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdCookieManager,
  IdExplicitTLSClientServerBase, IdFTPCommon, IdFTP;

{ == Utilites ================================================================== }

function rHttp_ExtractFilePath(const sUrl: string): string;
begin
  Result := Copy(sUrl, 1, LastDelimiter('/', sUrl));
end;

function rHttp_ExtractFileName(const sUrl: string): string;
begin
  Result := Copy(sUrl, LastDelimiter('/', sUrl) + 1, MaxInt);
end;

function rHttp_IncludePathDelimiter(const sUrl: string): string;
begin
  Result := Trim(sUrl);

  if (Result <> EmptyStr) and (Result[Length(Result)] <> '/') then
    Result := Result + '/';
end;

function rHttp_ExcludePathDelimiter(const sUrl: string): string;
begin
  Result := Trim(sUrl);

  if (Result <> EmptyStr) and (Result[Length(Result)] = '/') then
    Delete(Result, Length(Result), 1);
end;

function rHttp_ConcatParams(const sBase, sPart: string): string;
begin
  if sBase = EmptyStr
  then Result := sPart
  else begin
    if sPart = EmptyStr
    then Result := sBase
    else Result := sBase + '&' + sPart;
  end;
end;

function rHttp_IsSSL(const sUrl: string): Boolean;
begin
  Result := AnsiStartsText('https://', sUrl);
end;

function rHttp_SSLVersion(const aSSL: TSSLVer): TIdSSLVersion;
begin
  case aSSL of
    sslSSL2:  Result := sslvSSLv2;
    sslSSL23: Result := sslvSSLv23;
    sslSSL3:  Result := sslvSSLv3;
    sslTLS1:  Result := sslvTLSv1;
    sslTLS11: Result := sslvTLSv1_1;
    sslTLS12: Result := sslvTLSv1_2;
    else Result := sslvTLSv1;
  end;
end;

{ == Ping ====================================================================== }

function rHttp_Ping(const sServer: string; const iTimeout: Integer = 3000; const iTryCount: Integer = 3): Boolean;
var
  IdPING: TIdIcmpClient;
  iTry: Integer;
begin
  IdPING := TIdIcmpClient.Create;
  try
    IdPING.Host := sServer;
    IdPING.PacketSize := 1024;
    IdPING.ReceiveTimeout  := iTimeout;

    iTry := 1;
    repeat
      IdPING.Ping;

      if IdPING.ReplyStatus.ReplyStatusType in [rsError, rsTimeOut, rsErrorUnreachable, rsErrorTTLExceeded,
        rsErrorPacketTooBig, rsErrorParameter, rsErrorDatagramConversion, rsErrorSecurityFailure] then
      begin
        Result := False;
        Break;
      end
      else begin
        Result := True;
        Inc(iTry);
      end;
    until iTry > iTryCount;
  finally
    IdPING.Free;
  end;
end;

{ == HTTP ====================================================================== }

procedure rHttp_DownloadFileEx(const pProxy: PProxyData; const sUrl, sFilename: string; const aSSL: TSSLVer);
var
  IdHTTP: TIdHTTP;
  IdCZip: TIdCompressorZLib;
  IdCSLL: TIdSSLIOHandlerSocketOpenSSL;
  Buf: TFileStream;
begin
  IdHTTP := TIdHTTP.Create;
  IdCZip := TIdCompressorZLib.Create;
  IdHTTP.Compressor := IdCZip;
  if aSSL <> sslNone
  then IdCSLL := TIdSSLIOHandlerSocketOpenSSL.Create
  else IdCSLL := nil;
  try
    if Assigned(pProxy) and (pProxy^.sServer <> EmptyStr) and (pProxy^.iPort > 0) then
    begin
      IdHTTP.ProxyParams.ProxyServer := pProxy^.sServer;
      IdHTTP.ProxyParams.ProxyPort := pProxy^.iPort;
      IdHTTP.ProxyParams.ProxyUsername := pProxy^.sUsername;
      IdHTTP.ProxyParams.ProxyPassword := pProxy^.sPassword;
    end;
    IdHTTP.HandleRedirects := True;
    IdHTTP.Request.Accept := sRqAcceptDef;
    IdHTTP.Request.AcceptEncoding := sRqAcceptEncodingZip;
    IdHTTP.Request.UserAgent := sRqUserAgentDef;
    if Assigned(IdCSLL) then
    begin
      IdCSLL.SSLOptions.Method := rHttp_SSLVersion(aSSL);
      IdCSLL.SSLOptions.VerifyMode := [];
      IdHTTP.IOHandler := IdCSLL;
    end;

    if FileExists(sFilename)
    then Buf := TFileStream.Create(sFilename, fmOpenReadWrite or fmShareExclusive)
    else Buf := TFileStream.Create(sFilename, fmCreate or fmShareExclusive);
    try
      IdHTTP.Get(sUrl, Buf);
    finally
      Buf.Free;
    end;
  finally
    if Assigned(IdCSLL) then
      FreeAndNil(IdCSLL);
    IdHTTP.Compressor := nil;
    FreeAndNil(IdCZip);
    FreeAndNil(IdHTTP);
  end;
end;

procedure rHttp_DownloadFile(const sUrl, sFilename: string; const bUseSSL: Boolean);
begin
  if bUseSSL
  then rHttp_DownloadFileEx(nil, sUrl, sFilename, sslTLS1)
  else rHttp_DownloadFileEx(nil, sUrl, sFilename, sslNone);
end;

function rHttp_GetEx(const pProxy: PProxyData; const sUrl: string; const aSSL: TSSLVer; const bRedirect: Boolean = True): string;
var
  IdHTTP: TIdHTTP;
  IdCZip: TIdCompressorZLib;
  IdCSLL: TIdSSLIOHandlerSocketOpenSSL;
  Stream: TStringStream;
begin
  IdHTTP := TIdHTTP.Create;
  IdCZip := TIdCompressorZLib.Create;
  IdHTTP.Compressor := IdCZip;
  if aSSL <> sslNone
  then IdCSLL := TIdSSLIOHandlerSocketOpenSSL.Create
  else IdCSLL := nil;
  Stream := TStringStream.Create('');
  try
    if Assigned(pProxy) and (pProxy^.sServer <> EmptyStr) and (pProxy^.iPort > 0) then
    begin
      IdHTTP.ProxyParams.ProxyServer := pProxy^.sServer;
      IdHTTP.ProxyParams.ProxyPort := pProxy^.iPort;
      IdHTTP.ProxyParams.ProxyUsername := pProxy^.sUsername;
      IdHTTP.ProxyParams.ProxyPassword := pProxy^.sPassword;
    end;
    IdHTTP.HandleRedirects := bRedirect;
    IdHTTP.Request.Accept := sRqAcceptDef;
    IdHTTP.Request.AcceptEncoding := sRqAcceptEncodingZip;
    IdHTTP.Request.UserAgent := sRqUserAgentDef;
    if Assigned(IdCSLL) then
    begin
      IdCSLL.SSLOptions.Method := rHttp_SSLVersion(aSSL);
      IdCSLL.SSLOptions.VerifyMode := [];
      IdHTTP.IOHandler := IdCSLL;
    end;

    try
      IdHTTP.Get(sUrl, Stream);
    finally
      IdHTTP.Disconnect;
    end;

    Stream.Position := 0;
    Result := Stream.DataString;
  finally
    FreeAndNil(Stream);
    if Assigned(IdCSLL) then
      FreeAndNil(IdCSLL);
    IdHTTP.Compressor := nil;
    FreeAndNil(IdCZip);
    FreeAndNil(IdHTTP);
  end;
end;

function rHttp_Get(const sUrl: string; const bUseSSL: Boolean; const bRedirect: Boolean = True): string;
begin
  if bUseSSL
  then Result := rHttp_GetEx(nil, sUrl, sslTLS1, bRedirect)
  else Result := rHttp_GetEx(nil, sUrl, sslNone, bRedirect);
end;

function rHttp_Get(const sUrl: string; const bRedirect: Boolean = True): string;
begin
  Result := rHttp_Get(sUrl, rHttp_IsSSL(sUrl), bRedirect);
end;

function rHttp_PostEx(const pProxy: PProxyData; const sUrl: string; const slValues: TStrings; const aSSL: TSSLVer; const bRedirect: Boolean = True): string;
var
  IdHTTP: TIdHTTP;
  IdCZip: TIdCompressorZLib;
  IdCSLL: TIdSSLIOHandlerSocketOpenSSL;
  Stream: TStringStream;
begin
  IdHTTP := TIdHTTP.Create;
  IdCZip := TIdCompressorZLib.Create;
  IdHTTP.Compressor := IdCZip;
  if aSSL <> sslNone
  then IdCSLL := TIdSSLIOHandlerSocketOpenSSL.Create
  else IdCSLL := nil;
  Stream := TStringStream.Create('');
  try
    if Assigned(pProxy) and (pProxy^.sServer <> EmptyStr) and (pProxy^.iPort > 0) then
    begin
      IdHTTP.ProxyParams.ProxyServer := pProxy^.sServer;
      IdHTTP.ProxyParams.ProxyPort := pProxy^.iPort;
      IdHTTP.ProxyParams.ProxyUsername := pProxy^.sUsername;
      IdHTTP.ProxyParams.ProxyPassword := pProxy^.sPassword;
    end;
    IdHTTP.HandleRedirects := bRedirect;
    IdHTTP.Request.Accept := sRqAcceptDef;
    IdHTTP.Request.AcceptEncoding := sRqAcceptEncodingZip;
    IdHTTP.Request.UserAgent := sRqUserAgentDef;
    if Assigned(idCSLL) then
    begin
      IdCSLL.SSLOptions.Method := rHttp_SSLVersion(aSSL);
      IdCSLL.SSLOptions.VerifyMode := [];
      IdHTTP.IOHandler := IdCSLL;
    end;

    try
      IdHTTP.Post(sUrl, slValues, Stream);
    finally
      IdHTTP.Disconnect;
    end;

    Stream.Position := 0;
    Result := Stream.DataString;
  finally
    FreeAndNil(Stream);
    if Assigned(IdCSLL) then
      FreeAndNil(IdCSLL);
    IdHTTP.Compressor := nil;
    FreeAndNil(IdCZip);
    FreeAndNil(IdHTTP);
  end;
end;

function rHttp_Post(const sUrl: string; const slValues: TStrings; const bUseSSL: Boolean; const bRedirect: Boolean = True): string;
begin
  if bUseSSL
  then Result := rHttp_PostEx(nil, sUrl, slValues, sslTLS1, bRedirect)
  else Result := rHttp_PostEx(nil, sUrl, slValues, sslNone, bRedirect);
end;

function rHttp_Post(const sUrl: string; const slValues: TStrings; const bRedirect: Boolean = True): string;
begin
  Result := rHttp_Post(sUrl, slValues, rHttp_IsSSL(sUrl), bRedirect);
end;

function rHttp_Post(const sUrl, sValues: string; const bUseSSL: Boolean; const bRedirect: Boolean = True): string;
var
  slValues: TStringList;
begin
  slValues := TStringList.Create;
  try
    slValues.Text := sValues;

    Result := rHttp_Post(sUrl, slValues, bUseSSL, bRedirect);
  finally
    FreeAndNil(slValues);
  end;
end;

function rHttp_Post(const sUrl, sValues: string; const bRedirect: Boolean = True): string; overload;
begin
  Result := rHttp_Post(sUrl, sValues, rHttp_IsSSL(sUrl), bRedirect);
end;

{ == FTP ======================================================================= }

function rFtp_CreateParams(const aHost: string; const aPort: Word; const aLogin, aPassword: string; const aPassiveMode: Boolean): TFtpServer;
begin
  Result.sHost := aHost;
  Result.iPort := aPort;
  Result.sLogin := aLogin;
  Result.sPassword := aPassword;
  Result.bPassiveMode := aPassiveMode;
end;

procedure rFtp_TransferFile(const aFtp: TFtpServer; const bUpload: Boolean; const sFileName, sFtpDir, sLocDir: string);
var
  IdFTP: TIdFTP;
  IdZIP: TIdCompressorZLib;
  i, iCount: Integer;
  sDir: string;
begin
  IdFTP := TIdFTP.Create;
  IdZIP := TIdCompressorZLib.Create;
  IdFTP.Compressor := IdZIP;
  try
    IdFTP.Host := aFtp.sHost;
    IdFTP.Port := aFtp.iPort;
    IdFTP.Username := aFtp.sLogin;
    IdFTP.Password := aFtp.sPassword;
    IdFTP.Passive := aFtp.bPassiveMode;
    IdFTP.TransferType := ftBinary;
    IdFTP.UseTLS := utNoTLSSupport;

    IdFTP.Connect;
    try
      iCount := WordCount(sFtpDir, ['/']);
      for i := 1 to iCount do
      begin
        sDir := Trim(ExtractWord(i, sFtpDir, ['/']));
        if sDir <> EmptyStr then
        begin
          if bUpload then
          begin
            try
              IdFTP.ChangeDir(sDir);
            except
              IdFTP.MakeDir(sDir);
              IdFTP.ChangeDir(sDir);
            end;
          end
          else IdFTP.ChangeDir(sDir);
        end;
      end;

      if bUpload then
      begin
        IdFTP.Put(IncludeTrailingPathDelimiter(sLocDir) + sFileName, sFileName, False, -1);
        try
          IdFTP.SetModTime(sFileName,
            FileTimeToDateTimeLocal(wFileGetLastWriteTime(
              IncludeTrailingPathDelimiter(sLocDir) + sFileName)));
        except
          // ... FTP server not supported MDTM
        end;
      end
      else IdFTP.Get(sFileName, IncludeTrailingPathDelimiter(sLocDir) + sFileName, True, False);
    finally
      IdFTP.Disconnect;
    end;
  finally
    IdFTP.Compressor := nil;
    FreeAndNil(IdZIP);
    FreeAndNil(IdFTP);
  end;
end;

{ == Proxy ===================================================================== }

function rProxy_StrToProxy(const sProxy: string): TProxyData;
var
  sSrvPort: string;
begin
  FillChar(Result, SizeOf(Result), 0);
  if sProxy <> EmptyStr then
  begin
    sSrvPort := Trim(ExtractWord(1, sProxy, ['|']));
    Result.sServer := Trim(ExtractWord(1, sSrvPort, [':']));
    Result.iPort := StrToInt(Trim(ExtractWord(2, sSrvPort, [':'])));
    Result.sUsername := Trim(ExtractWord(2, sProxy, ['|']));
    Result.sPassword := Trim(ExtractWord(3, sProxy, ['|']));
  end;
end;

function rProxy_ProxyToStr(const aProxy: TProxyData): string;
begin
  Result := EmptyStr;
  if aProxy.sServer <> EmptyStr then
    Result := aProxy.sServer + ':' + IntToStr(aProxy.iPort);
  if (Result <> EmptyStr) and (aProxy.sUsername <> EmptyStr) then
    Result := Result + '|' + aProxy.sUsername + '|' + aProxy.sPassword;
end;

function  rProxy_ProxyDbNet(var aList: TStringList;
  const aProtocols: TProxyProtocols; const aLevels: TProxyLevels; const aCountry: string;
  const aPort, aUpTime, aRTime, aMinCnt: Integer): Integer;
const
  iPageSize   = 15;
var
  iPage, iPageCnt: Integer;

  function LoadPage(const iPage: Integer): Integer;
  const
    urlList     = 'http://proxydb.net/';
    urlProtocol = 'protocol=%s';
    urlLevels   = 'anonlvl=%d';
    urlCountry  = 'country=%s';
    urlUpTime   = 'min_uptime=%d';
    urlRTime    = 'max_response_time=%d';
    urlOffset   = 'offset=%d';
  var
    urlParams, urlGet: string;
    i, iCount, iNumk, iPort: Integer;
    sResp, sTable, sRow, sScript: string;
    v1, v2, v2p, v2r: string;

    function HexToStr(const sHex: string): string;
    var
      iBuf, iRes: Integer;
    begin
      Result := EmptyStr;

      Val('$' + sHex, iBuf, iRes);
      if iRes = 0 then
        Result := Chr(iBuf);
    end;

  begin
    Result := 0;

    // ��������� ���������
    urlParams := EmptyStr;

    if ppHttp in aProtocols then
      urlParams := rHttp_ConcatParams(urlParams, Format(urlProtocol, ['http']));
    if ppHttps in aProtocols then
      urlParams := rHttp_ConcatParams(urlParams, Format(urlProtocol, ['https']));
    if ppSocks4 in aProtocols then
      urlParams := rHttp_ConcatParams(urlParams, Format(urlProtocol, ['socks4']));
    if ppSocks5 in aProtocols then
      urlParams := rHttp_ConcatParams(urlParams, Format(urlProtocol, ['socks5']));

    if plTransparent in aLevels then
      urlParams := rHttp_ConcatParams(urlParams, Format(urlLevels, [1]));
    if plAnonymous in aLevels then
      urlParams := rHttp_ConcatParams(urlParams, Format(urlLevels, [2]));
    if plDistorting in aLevels then
      urlParams := rHttp_ConcatParams(urlParams, Format(urlLevels, [3]));
    if plElite in aLevels then
      urlParams := rHttp_ConcatParams(urlParams, Format(urlLevels, [4]));

    if aCountry <> EmptyStr then
    begin
      iCount := WordCount(aCountry, [';', ',']);
      for i := 1 to iCount do
        urlParams := rHttp_ConcatParams(urlParams, Format(urlCountry, [Trim(ExtractWord(i, aCountry, [';', ',']))]));
    end;

    if aUpTime > 0 then
      urlParams := rHttp_ConcatParams(urlParams, Format(urlUpTime, [aUpTime]));

    if aRTime > 0 then
      urlParams := rHttp_ConcatParams(urlParams, Format(urlRTime, [aRTime div 1000]));

    if iPage > 1 then
      urlParams := rHttp_ConcatParams(urlParams, Format(urlOffset, [(iPage - 1) * iPageSize]));

    // ��������� ������
    if urlParams = EmptyStr
    then urlGet := urlList
    else urlGet := urlList + '?' + urlParams;

    // ��������� ��������
    sResp := rHTTP_GetEx(nil, urlGet, sslNone, True);
    if sResp <> EmptyStr then
    begin
      // �������� ������� � �������
      sTable := Parse_GetValue(sResp, '<tbody>', '</tbody>');
      if sTable <> EmptyStr then
      begin
        iNumk := StrToIntDef(Parse_GetBlockValue(sResp, '<div style="display:none" data-', '</div>', '="', '"'), 0);
        repeat
          sRow := Parse_CutFirstValue(sTable, '<tr>');
          if sRow <> EmptyStr then
          begin
            (* 2018-06-18 ******************************************************
            <script>
              var  s =
               '2.86.831'.split('').reverse().join('');
              var yy =/* */ atob('\x4e\x43\x34\x78\x4e\x44\x55\x3d'.replace(/\\x([0-9A-Fa-f]{2})/g,function(){return String.fromCharCode(parseInt(arguments[1], 16))}));
              var /**/ pp =  (3115 - ([]+[]))/**/ +  (+document.querySelector('[data-nnumb]').getAttribute('data-nnumb'))-[]+[];
              document.write('<a href="/' + s + yy + '/' + pp + '#http">' + s + yy + String.fromCharCode(58) + pp + '</a>');
            </script>
            * 2018-06-18 ******************************************************)

            sScript := Parse_GetValue(sRow, '<script>', '</script>');

            v1 := ReverseString(Parse_GetBlockValue(sScript, 'var', 'split', '''', ''''));
            v2 := Parse_GetValue(sScript, 'atob(''', '''');
            v2r := EmptyStr;
            repeat
              v2p := Parse_CutFirstValue(v2, '\x');
              if v2p <> EmptyStr then
                v2r := v2r + HexToStr(v2p);
            until v2p = EmptyStr;
            iPort := StrToInt(Parse_GetBlockValue(sScript, 'pp =', ';', '(', ' ')) + iNumk;

            if (aPort = 0) or (iPort = aPort) then
              aList.Add(Format('%s:%d', [v1 + DecodeBase64(AnsiString(v2r)), iPort]));

            Inc(Result);
          end;
        until sRow = EmptyStr;
      end;
    end;
  end;

begin
  aList.Clear;

  iPage := 0;
  repeat
    Inc(iPage);
    iPageCnt := LoadPage(iPage);
  until (aList.Count >= aMinCnt) or (iPageCnt < iPageSize);

  // InfoBox(aList.Text);
  Result := aList.Count;
end;

end.

