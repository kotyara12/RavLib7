unit rTgIntf;

interface

uses
  rHttpUtils;

type
  tTgUser = record
    idUser: Int64;
    sJson: string;
    bIsBot: Boolean;
    sUsername: string;
    sFirstName: string;
    sLastName: string;
    sFullName: string;
    sLngCode: string;
  end;

  tTgChat = record
    idChat: Int64;
    sJson: string;
    sTitle: string;
    sChatType: string;
  end;

  tTgMessage = record
    idMsg: Int64;
    sJson: string;
    tgChat: tTgChat;
    tgFrom: tTgUser;
    iDate: Integer;
    sText: string;
  end;

  tTgUpdateType = (tgmUnknown, tgmMessage, tgmEditedMessage, tgmChannelPost, tgmCallbackQuery);

  tTgUpdate = record
    idUpd: Int64;
    sJson: string;
    tgType: tTgUpdateType;
    tgMsg: tTgMessage;
  end;

  tTgUpdates = array of tTgUpdate;

function  tg_UpdatesParse(const jsonRes: string; var bOk: Boolean): tTgUpdates;
function  tg_UpdatesGet(const aProxy: PProxyData; const sBotToken: string; const idOffset: Int64; var bOk: Boolean): tTgUpdates;
function  tg_UpdatesWait(const aProxy: PProxyData; const sBotToken: string; const idOffset: Int64; const iTimeout: Integer; var bOk: Boolean): tTgUpdates;
function  tg_SendMessage(const aProxy: PProxyData; const sBotToken: string; const idChat: Int64;
  const sMessage, sMarkup: string; const idReply: Int64;
  const bNotNotify: Boolean = False; const bShowLinks: Boolean = False): tTgMessage;
procedure tg_DeleteMessage(const aProxy: PProxyData; const sBotToken: string; const idChat: Int64;
  const idMessage: Int64);

implementation

uses
  SysUtils, Classes, uLkJSON,
  IdTCPConnection, IdTCPClient, IdIcmpClient, IdHTTP, IdCompressorZLib, IdMultipartFormData,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdCookieManager,
  rDialogs;

function tg_FromDecode(const jsonUser: TlkJSONobject): tTgUser;
begin
  FillChar(Result, SizeOf(Result), 0);

  if Assigned(jsonUser) then
  begin
    with Result do
    begin
      idUser := jsonUser.getInt64FromName('id');
      sJson := TlkJSON.GenerateText(jsonUser);
      bIsBot := jsonUser.getBooleanFromName('is_bot');
      sUsername := jsonUser.getStringFromName('username');
      sFirstName := jsonUser.getStringFromName('first_name');
      sLastName := jsonUser.getStringFromName('last_name');
      sLngCode := jsonUser.getStringFromName('language_code');

      if sUsername = EmptyStr then
        sUsername := Format('%d', [idUser]);

      if sFirstName <> EmptyStr then
        sFullName := sFirstName;
      if sLastName <> EmptyStr then
      begin
        if sFullName = EmptyStr
        then sFullName := sLastName
        else sFullName := sFullName + #32 + sLastName;
      end;
      if sFullName = EmptyStr then
        sFullName := sUsername;
    end;
  end;
end;

function tg_FromChatDecode(const jsonChat: TlkJSONobject): tTgUser;
begin
  FillChar(Result, SizeOf(Result), 0);

  if Assigned(jsonChat) then
  begin
    with Result do
    begin
      idUser := jsonChat.getInt64FromName('id');
      sJson := TlkJSON.GenerateText(jsonChat);
      bIsBot := False;
      sUsername := jsonChat.getStringFromName('username');
      if sUsername = EmptyStr then
        sUsername := jsonChat.getStringFromName('title');
      sFirstName := jsonChat.getStringFromName('first_name');
      sLastName := jsonChat.getStringFromName('last_name');
      sLngCode := jsonChat.getStringFromName('language_code');

      if sUsername = EmptyStr then
        sUsername := Format('%d', [idUser]);

      if sFirstName <> EmptyStr then
        sFullName := sFirstName;
      if sLastName <> EmptyStr then
      begin
        if sFullName = EmptyStr
        then sFullName := sLastName
        else sFullName := sFullName + #32 + sLastName;
      end;
      if sFullName = EmptyStr then
        sFullName := sUsername;
    end;
  end;
end;


function tg_ChatDecode(const jsonChat: TlkJSONobject): tTgChat;
begin
  FillChar(Result, SizeOf(Result), 0);

  if Assigned(jsonChat) then
  begin
    with Result do
    begin
      idChat := jsonChat.getInt64FromName('id');
      sJson := TlkJSON.GenerateText(jsonChat);
      if jsonChat.getStringFromName('title') = EmptyStr
      then sTitle := jsonChat.getStringFromName('username')
      else sTitle := jsonChat.getStringFromName('title');
      sChatType := jsonChat.getStringFromName('type');
    end;
  end;
end;

function tg_MessageDecode(const jsonMsg: TlkJSONobject): tTgMessage;
begin
  FillChar(Result, SizeOf(Result), 0);

  if Assigned(jsonMsg) then
  begin
    with Result do
    begin
      if jsonMsg.IndexOfName('message_id') > -1
      then idMsg := jsonMsg.getInt64FromName('message_id')
      else idMsg := jsonMsg.getInt64FromName('id');
      sJson := TlkJSON.GenerateText(jsonMsg);
      iDate := jsonMsg.getIntFromName('date');
      sText := jsonMsg.getStringFromName('text');
      tgChat := tg_ChatDecode(jsonMsg.Field['chat'] as TlkJSONobject);
      if jsonMsg.IndexOfName('from') > -1
      then tgFrom := tg_FromDecode(jsonMsg.Field['from'] as TlkJSONobject)
      else tgFrom := tg_FromChatDecode(jsonMsg.Field['chat'] as TlkJSONobject);
    end;
  end;
end;

function tg_CallbackDecode(const jsonMsg: TlkJSONobject): tTgMessage;
begin
  FillChar(Result, SizeOf(Result), 0);

  if Assigned(jsonMsg) then
  begin
    with Result do
    begin
      idMsg := TlkJSONobject(jsonMsg.Field['message']).getInt64FromName('message_id');
      sJson := TlkJSON.GenerateText(jsonMsg);
      sText := jsonMsg.getStringFromName('data');
      tgChat := tg_ChatDecode(TlkJSONobject(TlkJSONobject(jsonMsg.Field['message']).Field['chat']));
      tgFrom := tg_FromDecode(TlkJSONobject(jsonMsg.Field['from']));
    end;
  end;
end;

function tg_UpdateDecode(const jsonUpd: TlkJSONobject): tTgUpdate;
begin
  FillChar(Result, SizeOf(Result), 0);

  if Assigned(jsonUpd) then
  begin
    with Result do
    begin
      idUpd := jsonUpd.getInt64FromName('update_id');
      sJson := TlkJSON.GenerateText(jsonUpd);

      tgType := tgmUnknown;
      if jsonUpd.IndexOfName('message') > -1 then
      begin
        tgType := tgmMessage;
        tgMsg := tg_MessageDecode(jsonUpd.Field['message'] as TlkJSONobject);
      end;

      if jsonUpd.IndexOfName('edited_message') > -1 then
      begin
        tgType := tgmEditedMessage;
        tgMsg := tg_MessageDecode(jsonUpd.Field['edited_message'] as TlkJSONobject);
      end;

      if jsonUpd.IndexOfName('channel_post') > -1 then
      begin
        tgType := tgmChannelPost;
        tgMsg := tg_MessageDecode(jsonUpd.Field['channel_post'] as TlkJSONobject);
      end;

      if jsonUpd.IndexOfName('callback_query') > -1 then
      begin
        tgType := tgmCallbackQuery;
        tgMsg := tg_CallbackDecode(jsonUpd.Field['callback_query'] as TlkJSONobject);
      end;
    end;
  end;
end;

function tg_UpdatesDecode(const jsonRes: TlkJSONobject; var bOk: Boolean): tTgUpdates;
var
  jsonUpds: TlkJSONlist;
  i, iCount: Integer;
begin
  SetLength(Result, 0);

  if Assigned(jsonRes) and jsonRes.getBooleanFromName('ok') then
  begin
    bOk := True;
    jsonUpds := jsonRes.Field['result'] as TlkJSONlist;

    iCount := jsonUpds.Count;
    if iCount > 0 then
    begin
      SetLength(Result, iCount);

      for i := 0 to iCount - 1 do
        Result[i] := tg_UpdateDecode(jsonUpds.Child[i] as TlkJSONobject);
    end;
  end
  else bOk := False;
end;

function tg_UpdatesParse(const jsonRes: string; var bOk: Boolean): tTgUpdates;
begin
  Result := tg_UpdatesDecode(TlkJSON.ParseText(jsonRes) as TlkJSONobject, bOk);
end;

function tg_API(const aProxy: PProxyData; const sBotToken, sApiMethod, sParams: string): string;
var
  IdHTTP: TIdHTTP;
  IdCZip: TIdCompressorZLib;
  IdCSLL: TIdSSLIOHandlerSocketOpenSSL;
  fRequest, fResponse: TStringStream;
begin
  IdHTTP := TIdHTTP.Create;
  IdCZip := TIdCompressorZLib.Create;
  IdHTTP.Compressor := IdCZip;
  IdCSLL := TIdSSLIOHandlerSocketOpenSSL.Create;
  fRequest := TStringStream.Create(sParams);
  fResponse := TStringStream.Create('');
  try
    IdCSLL.SSLOptions.Method := sslvTLSv1_2;
    IdCSLL.SSLOptions.VerifyMode := [];
    IdHTTP.HandleRedirects := True;
    IdHTTP.Request.Accept := 'application/json';
    IdHTTP.Request.ContentType := 'application/json';
    IdHTTP.Request.AcceptEncoding := 'gzip, deflate, sdch';
    IdHTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.80 Safari/537.36';
    IdHTTP.IOHandler := IdCSLL;

    if Assigned(aProxy) then
    begin
      with aProxy^ do
      begin
        IdHTTP.ProxyParams.ProxyServer := sServer;
        IdHTTP.ProxyParams.ProxyPort := iPort;
        IdHTTP.ProxyParams.ProxyUsername := sUsername;
        IdHTTP.ProxyParams.ProxyPassword := sPassword;
      end;
    end;

    try
      IdHTTP.Post(Format('https://api.telegram.org/bot%0:s/%1:s', [sBotToken, sApiMethod]),
        fRequest, fResponse);
    finally
      IdHTTP.Disconnect;
    end;

    fResponse.Position := 0;
    Result := fResponse.DataString;
  finally
    FreeAndNil(fRequest);
    FreeAndNil(fResponse);
    if Assigned(IdCSLL) then
      FreeAndNil(IdCSLL);
    IdHTTP.Compressor := nil;
    FreeAndNil(IdCZip);
    FreeAndNil(IdHTTP);
  end;
end;

function tg_UpdatesGet(const aProxy: PProxyData; const sBotToken: string; const idOffset: Int64; var bOk: Boolean): tTgUpdates;
var
  jsonMsg: TlkJSONobject;
begin
  jsonMsg := TlkJSONobject.Create;
  jsonMsg.Add('offset', idOffset);

  Result := tg_UpdatesParse(tg_API(aProxy, sBotToken, 'getUpdates', TlkJSON.GenerateText(jsonMsg)), bOk);
end;

function tg_UpdatesWait(const aProxy: PProxyData; const sBotToken: string; const idOffset: Int64; const iTimeout: Integer; var bOk: Boolean): tTgUpdates;
var
  jsonMsg: TlkJSONobject;
begin
  jsonMsg := TlkJSONobject.Create;
  jsonMsg.Add('offset', idOffset);
  jsonMsg.Add('timeout', iTimeout);

  Result := tg_UpdatesParse(tg_API(aProxy, sBotToken, 'getUpdates', TlkJSON.GenerateText(jsonMsg)), bOk);
end;

function tg_SendMessage(const aProxy: PProxyData; const sBotToken: string; const idChat: Int64;
  const sMessage, sMarkup: string; const idReply: Int64;
  const bNotNotify: Boolean = False; const bShowLinks: Boolean = False): tTgMessage;
var
  jsonMsg, jsonRes: TlkJSONobject;
begin
  jsonMsg := TlkJSONobject.Create;
  try
    jsonMsg.Add('chat_id', idChat);
    jsonMsg.Add('text', sMessage);
    jsonMsg.Add('parse_mode', 'HTML');
    if idReply <> 0 then
      jsonMsg.Add('reply_to_message_id', idReply);
    if bNotNotify then
      jsonMsg.Add('disable_notification', True);
    if not bShowLinks then
      jsonMsg.Add('disable_web_page_preview', True);
    if sMarkup <> EmptyStr then
      jsonMsg.Add('reply_markup', sMarkup);

    jsonRes := TlkJSON.ParseText(tg_API(aProxy, sBotToken, 'sendMessage', TlkJSON.GenerateText(jsonMsg))) as TlkJSONobject;
    if Assigned(jsonRes) and jsonRes.getBooleanFromName('ok')
    then Result := tg_MessageDecode(jsonRes.Field['result'] as TlkJSONobject)
    else FillChar(Result, SizeOf(Result), 0);
  finally
    jsonMsg.Free;
  end;
end;

procedure tg_DeleteMessage(const aProxy: PProxyData; const sBotToken: string; const idChat: Int64;
  const idMessage: Int64);
var
  jsonMsg: TlkJSONobject;
begin
  jsonMsg := TlkJSONobject.Create;
  try
    jsonMsg.Add('chat_id', idChat);
    jsonMsg.Add('message_id', idMessage);
    tg_API(aProxy, sBotToken, 'deleteMessage', TlkJSON.GenerateText(jsonMsg));
  finally
    jsonMsg.Free;
  end;
end;

end.
