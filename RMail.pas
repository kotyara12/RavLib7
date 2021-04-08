unit RMail;

interface

uses
  Classes;

procedure SendEMail(const SmtpServer: string;
  const Port: Integer; const UserName, Password: string;
  const From, Dest, Subject, Body: string; BodyList: TStrings);

implementation

uses
  SysUtils, Windows, WinSock, RBase64, RxStrUtils, RDialogs;

resourcestring
  SErrWSAStartUp             = '������ ������������� WSA!';
  SErrGetHostByName          = 'SMTP c����� "%s" �� ������!';
  SErrGetIpAddress           = '������ ��������� IP-������ SMTP-�������!';
  SErrInitSocket             = '������ ������������� ������!';
  SErrConnectSocket          = '������ ����������� ������!';
  SErrConnctToSmtpServer     = '������ ����������� � SMTP ������� "%s", ���� %d!';
  SErrSmtpBadLogin           = '������ ����������� ������������ "%s" �� SMTP-�������!';
  SErrSmtpBadPassword        = '������ ������� ��� "%s" �������!';
  SErrSmtpNotReady           = 'SMTP-������ �� ������ ������� ���������� � �������� ���������!';
  SErrSendSmtp               = '������ �������� E-Mail ��������� �� SMTP-������ (������� ����� �� ������� "%s")!';
  SErrSendFrom               = '������������ ����� ����������� "%s"!';
  SErrSendTo                 = '������������ ����� ���������� "%s"!';
  // SErrCloseSession           = '������ �������� E-Mail ��������� �� SMTP-������!';

const
  AddrDiv        = [';'];
  CRLF           = #13#10;
  Boundary       = 'RavBoundary';
  CmdEhlo        = 'EHLO ';
  CmdLogin       = 'AUTH LOGIN ';
  CmdMailFrom    = 'MAIL FROM:';
  CmdMailDest    = 'RCPT TO:';
  CmdBeginData   = 'DATA';
  CmdEndData     = CRLF + '.' + CRLF;
  CmdQuit        = 'QUIT';
  HeaderFrom     = 'From: %s' + CRLF;
  HeaderDest     = 'To: %s' + CRLF;
  HeaderSubj     = 'Subject: =?Windows-1251?B?%s?=' + CRLF;
  HeaderMime     = 'MIME-Version: 1.0' + CRLF;
  BodyType       = 'Content-Type: text/plain; charset=Windows-1251' + CRLF;
  BodyEncode     = 'Content-transfer-encoding: base64' + CRLF + CRLF;

  RcvReady       = '220';
  RcvOk          = '250';
  RcvLoginOk     = '334';
  RcvLoginNoPwd  = '403';
  RcvPwdOk       = '235';
  RcvMsgReady    = '354';

procedure SendEMail(const SmtpServer: string;
  const Port: Integer; const UserName, Password: string;
  const From, Dest, Subject, Body: string; BodyList: TStrings);
var
  WSAData: TWSAData;
  WsaErr: integer;
  SAddr: TSockAddrIn;
  Sock: TSocket;
  SmtpHost: PHostEnt;
  SmtpIP: PChar;
  RcvCode, AddrStr: string;
  i, ToCnt: Integer;

  function ExtractAddr(const S: string): string;
  var
    P: Integer;
  begin
    Result := S;
    P := Pos('<', Result);
    if P > 0 then Delete(Result, 1, P - 1);
    P := Pos('>', Result);
    if P > 0 then Delete(Result, P + 1, Length(Result) - P);
  end;

  function RemoveCRLF(const Str: string): string;
  begin
    Result := ReplaceStr(ReplaceStr(Str, #13, ' '), #10, ' ');
  end;

  procedure SendData(Str: string);
  var
    i: integer;
  begin
    for i := 1 to Length(Str) do
     if send(Sock, Str[i], 1, 0) = SOCKET_ERROR then Exit;
  end;

  function ReciveCode: string;
  var
    buff: array[0..1024] of Char;
    Res: integer;
  begin
    Result := '';
    ZeroMemory(@buff, SizeOf(buff));
    Res := recv(Sock, buff, SizeOf(buff), 0);
    if Res <> SOCKET_ERROR then
      Result := Copy(buff, 1, 3);
  end;

  function SendCmd(const Cmd: string; const ExpectedRcvCode: string): Boolean;
  begin
    SendData(Cmd);
    Result := ReciveCode = ExpectedRcvCode;
  end;

begin
  // �������������� WSA
  WsaErr := WSAStartUp($101, WSAData);
  if WsaErr <> 0 then
    raise Exception.Create(SErrWSAStartUp);
  try
    // ���������� ������
    SmtpHost := GetHostByName(PChar(SmtpServer));
    if SmtpHost = nil then
      raise Exception.CreateFmt(SErrGetHostByName, [SmtpServer]);
    // ��������� IP-�����
    SmtpIP := inet_ntoa(PInAddr(SmtpHost.h_addr_list^)^);
    if SmtpIP = nil then
      raise Exception.Create(SErrGetIpAddress);
    ZeroMemory(@SAddr, SizeOf(SAddr));
    with SAddr do
    begin
      sin_family := AF_INET;
      sin_port := htons(Port);
      sin_addr.S_addr := Inet_Addr(SmtpIP);
    end;
    // �������������� �����
    Sock := socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
    if Sock = INVALID_SOCKET then
      raise Exception.Create(SErrInitSocket);
    try
      // ������������ � ������
      if Connect(Sock, SAddr, SizeOf(SAddr)) > 0 then
        raise Exception.Create(SErrConnectSocket);
      // ����������� � �������
      if ReciveCode <> RcvReady then
        raise Exception.CreateFmt(SErrConnctToSmtpServer, [SmtpServer, Port]);
      if not SendCmd(CmdEhlo + SmtpServer + CRLF, RcvOk) then
        raise Exception.CreateFmt(SErrConnctToSmtpServer, [SmtpServer, Port]);
      // ��������� �� �������
      SendData(CmdLogin + EncodeBase64(UserName) + CRLF);
      RcvCode := ReciveCode;
      if RcvCode <> RcvLoginOk then
      begin
        if RcvCode <> RcvLoginNoPwd then
          raise Exception.CreateFmt(SErrSmtpBadLogin, [UserName]);
      end
      else begin
        if not SendCmd(EncodeBase64(Password) + CRLF, RcvPwdOk) then
          raise Exception.CreateFmt(SErrSmtpBadPassword, [UserName]);
      end;
      // �������� ������
      if not SendCmd(CmdMailFrom + ExtractAddr(RemoveCRLF(From)) + CRLF, RcvOk) then
        raise Exception.CreateFmt(SErrSendFrom, [From]);
      ToCnt := 0;
      for i := 1 to WordCount(Dest, AddrDiv) do
      begin
        AddrStr := RemoveCRLF(Trim(ExtractWord(i, Dest, AddrDiv)));
        if SendCmd(CmdMailDest + ExtractAddr(AddrStr) + CRLF, RcvOk) then Inc(ToCnt);
      end;
      if ToCnt = 0 then
        raise Exception.CreateFmt(SErrSendTo, [Dest]);
      // �������� ������� ������ ���������
      if not SendCmd(CmdBeginData + CRLF, RcvMsgReady) then
        raise Exception.Create(SErrSmtpNotReady);
      // �������� ��������� ���������
      SendData(Format(HeaderFrom, [RemoveCRLF(From)]));
      SendData(Format(HeaderDest, [RemoveCRLF(Dest)]));
      SendData(Format(HeaderSubj, [EncodeBase64(Subject)]));
      SendData(HeaderMime);
      SendData(BodyType);
      SendData(BodyEncode);
      // �������� ����� ���������
      if Assigned(BodyList) then
        for i := 0 to BodyList.Count - 1 do
          SendData(EncodeBase64(BodyList[i] + CRLF) + CRLF)
      else
        SendData(EncodeBase64(Body) + CRLF);
      // ��������� ����������
      SendData(CmdEndData);
      RcvCode := ReciveCode;
      if not (RcvCode = RcvOk) then
        raise Exception.CreateFmt(SErrSendSmtp, [RcvCode]);
      SendData(CmdQuit + CRLF);
    finally
      // ��������� �����
      CloseSocket(Sock);
    end;
  finally
    // ��������� WSA
    WSACleanup;
  end;
end;

end.
