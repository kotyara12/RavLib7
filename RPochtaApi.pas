unit rPochtaAPI;

interface

uses
  SysUtils, rHttpUtils;

type
  EPochtaAPIException = class(Exception)
  private
    fErrorCode: Integer;
    fErrorMessage: string;
    fResponseText: string;
  public
    constructor Create(const iErrorCode: Integer; const aErrorMessage, aResponseText: string); overload; virtual;

    property ErrorCode: Integer read fErrorCode;
    property ErrorMessage: string read fErrorMessage;
    property ResponseText: string read fResponseText;
  end;

  TPochatAPI_Tariff = record
    iTotalRateWoVat: Integer;
    iTotalRateVat: Integer;
    iTotalRate: Integer;
  end;

  TPochatAPI_Order = record
    sId: string;
    sBarcode: string;
    iTotalRateWoVat: Integer;
    iTotalRateVat: Integer;
    iTotalRate: Integer;
  end;

function rPochtaAPI_CallAPI(const aProxy: PProxyData; const apiURL, apiMethod: string;
  const sToken, sUser: string; const jsonParams: string): string;

function rPochtaAPI_Tariff(const aProxy: PProxyData;
  const sToken, sUser: string;
  const jsonParams: string): TPochatAPI_Tariff;

function rPochtaAPI_AddressClean(const aProxy: PProxyData;
  const sToken, sUser: string;
  const sIndex, sCity, sAddress: string): string;
function rPochtaAPI_AddressPart(const jsonAddress: string; const sPartName: string): string;
function rPochtaAPI_AddressFormat(const jsonAddress: string; const bUseIndex: Boolean): string;

function rPochtaAPI_FioClean(const aProxy: PProxyData;
  const sToken, sUser: string;
  const sFio: string): string;
function rPochtaAPI_FioFormat(const jsonFio: string): string;

function rPochtaAPI_OrderJson(const aProxy: PProxyData;
  const sToken, sUser: string;
  const sOrderId: string): string;
function rPochtaAPI_OrderData(const aProxy: PProxyData;
  const sToken, sUser: string;
  const sOrderId: string): TPochatAPI_Order;
function rPochtaAPI_OrderCreate(const aProxy: PProxyData;
  const sToken, sUser: string;
  const jsonParams: string;
  const bRecipientIsFio: Boolean;
  var sIndex, sCity, sAddress, sRecipient: string): string;
procedure rPochtaAPI_OrderDelete(const aProxy: PProxyData;
  const sToken, sUser: string;
  const sOrderId: string);


implementation

uses
  Classes, StrUtils, uLkJSON,
  IdTCPConnection, IdTCPClient, IdIcmpClient, IdHTTP, IdCompressorZLib, IdMultipartFormData,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdCookieManager,
  IdException, IdReplyRFC, IdGlobal,
  rDialogs;

const
  caAddressType            = 'address-type';
  caArea                   = 'area';
  caBuilding               = 'building';
  caCorpus                 = 'corpus';
  caHotel                  = 'hotel';
  caHouse                  = 'house';
  caId                     = 'id';
  caIndex                  = 'index';
  caLetter                 = 'letter';
  caLocation               = 'location';
  caNumAddressType         = 'num-address-type';
  caOriginalAddress        = 'original-address';
  caOriginalFio            = 'original-fio';
  caPlace                  = 'place';
  caQualityCode            = 'quality-code';
  caRegion                 = 'region';
  caRoom                   = 'room';
  caSlash                  = 'slash';
  caStreet                 = 'street';
  caValidationCode         = 'validation-code';
  caSurName                = 'surname';
  caName                   = 'name';
  caMiddleName             = 'middle-name';
  caGivenName              = 'given-name';
  caRecipientName          = 'recipient-name';
  caErrors                 = 'errors';
  caErrorCodes             = 'error-codes';
  caCode                   = 'code';
  caDescription            = 'description';
  caTo                     = '-to';
  caResultIds              = 'result-ids';
  caBarcode                = 'barcode';
  caTotalRate              = 'total-rate';
  caTotalRateWoVat         = 'total-rate-wo-vat';
  caTotalRateVat           = 'total-vat';

  qcGOOD                   = 'GOOD';       // Пригоден для почтовой рассылки
  qcON_DEMAND	             = 'ON_DEMAND';  // До востребования
  qcPOSTAL_BOX             = 'POSTAL_BOX'; //	Абонентский ящик
  qcUNDEF_05               = 'UNDEF_05';   //	Не определена квартира/офис

  qcUNDEF_01               = 'UNDEF_01';   //	Не определен регион
  qcUNDEF_02               = 'UNDEF_02';   //	Не определен город или населенный пункт
  qcUNDEF_03               = 'UNDEF_03';   //	Не определена улица
  qcUNDEF_04               = 'UNDEF_04';   //	Не определен номер дома
  qcUNDEF_06               = 'UNDEF_06';   //	Не определен
  qcUNDEF_07               = 'UNDEF_07';   //	Иностранный адрес

resourcestring
  rsQualityCode_UNDEF_01   = 'Неверный адрес: не определен регион';
  rsQualityCode_UNDEF_02   = 'Неверный адрес: не определен город или населенный пункт';
  rsQualityCode_UNDEF_03   = 'Неверный адрес: не определена улица';
  rsQualityCode_UNDEF_04   = 'Неверный адрес: не определен номер дома';
  rsQualityCode_UNDEF_05   = 'Предупреждение: не определена квартира/офис';
  rsQualityCode_UNDEF_06   = 'Неверный адрес';
  rsQualityCode_BAD_FIO    = 'Неверное ФИО';
  rsQualityCode_FIO_NAME   = 'Не удалось определить имя получателя';
  rsQualityCode_FIO_SNAME  = 'Не удалось определить фамилию получателя';
  rsQualityCode_FIO_MNAME  = 'Не удалось определить отчество получателя';

{ == EPochtaAPIException ======================================================= }

constructor EPochtaAPIException.Create(const iErrorCode: Integer; const aErrorMessage, aResponseText: string);
begin
  if aResponseText = EmptyStr
  then inherited Create(aErrorMessage)
  else inherited Create(aErrorMessage + #13#13 + aResponseText);

  fErrorCode := iErrorCode;
  fErrorMessage := aErrorMessage;
  fResponseText := aResponseText;
end;

{ == CallAPI =================================================================== }

function rPochtaAPI_CallAPI(const aProxy: PProxyData; const apiURL, apiMethod: string; const sToken, sUser: string; const jsonParams: string): string;
const
  urlPochtaAPIBase = 'https://otpravka-api.pochta.ru';
var
  IdHTTP: TIdHTTP;
  IdCZip: TIdCompressorZLib;
  IdCSLL: TIdSSLIOHandlerSocketOpenSSL;
  fRequest: TStringStream;
  fDefEnc: IdTextEncodingType;
begin
  Result := EmptyStr;

  IdHTTP := TIdHTTP.Create;
  IdCZip := TIdCompressorZLib.Create;
  IdHTTP.Compressor := IdCZip;
  IdCSLL := TIdSSLIOHandlerSocketOpenSSL.Create;
  fRequest := TStringStream.Create(jsonParams);
  try
    IdCSLL.SSLOptions.VerifyMode := [];
    IdHTTP.HandleRedirects := True;
    IdHTTP.Request.CustomHeaders.AddValue('Authorization', 'AccessToken ' + sToken);
    IdHTTP.Request.CustomHeaders.AddValue('X-User-Authorization', 'Basic ' + sUser);
    IdHTTP.Request.ContentType := 'application/json';
    IdHTTP.Request.CharSet := 'UTF-8';
    IdHTTP.Request.Accept := 'application/json';
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
      fDefEnc := GIdDefaultTextEncoding;
      try
        GIdDefaultTextEncoding := encUTF8;
        try
          if SameText(apiMethod, 'get') then
            Result := IdHTTP.Get(urlPochtaAPIBase + apiURL);
          if SameText(apiMethod, 'put') then
            Result := IdHTTP.Put(urlPochtaAPIBase + apiURL, fRequest);
          if SameText(apiMethod, 'post') then
            Result := IdHTTP.Post(urlPochtaAPIBase + apiURL, fRequest);
          if SameText(apiMethod, 'delete') then
            IdHTTP.Delete(urlPochtaAPIBase + apiURL, fRequest);
        except
          on E: EIdHTTPProtocolException do
            raise EPochtaAPIException.Create(E.ErrorCode, E.Message, Utf8Decode(E.ErrorMessage));
          on E: EIdReplyRFCError do
            raise EPochtaAPIException.Create(E.ErrorCode, E.Message, EmptyStr);
          on E: Exception do
            raise EPochtaAPIException.Create(-1, E.Message, EmptyStr);
        end;
      finally
        GIdDefaultTextEncoding := fDefEnc;
      end;
    finally
      IdHTTP.Disconnect;
    end;
  finally
    FreeAndNil(fRequest);
    if Assigned(IdCSLL) then
      FreeAndNil(IdCSLL);
    IdHTTP.Compressor := nil;
    FreeAndNil(IdCZip);
    FreeAndNil(IdHTTP);
  end;
end;

function rPochtaAPI_Tariff(const aProxy: PProxyData; const sToken, sUser: string; const jsonParams: string): TPochatAPI_Tariff;
var
  jsResponse: TlkJSONbase;
begin
  FillChar(Result, SizeOf(Result), 0);

  jsResponse := TlkJSON.ParseText(
    rPochtaAPI_CallAPI(aProxy, '/1.0/tariff', 'post', sToken, sUser, Utf8Encode(jsonParams)));

  if Assigned(jsResponse) then
  begin
    try
      Result.iTotalRateWoVat := TlkJSONobject(jsResponse).getIntFromName(caTotalRate);
      Result.iTotalRateVat := TlkJSONobject(jsResponse).getIntFromName(caTotalRateVat);
      Result.iTotalRate := Result.iTotalRateWoVat + Result.iTotalRateVat;
    finally
      jsResponse.Free;
    end;
  end;
end;


function rPochtaAPI_AddressClean(const aProxy: PProxyData; const sToken, sUser: string;
  const sIndex, sCity, sAddress: string): string;
var
  jsRequest: TlkJSONlist;
  jsReqAddr: TlkJSONObject;
  jsResponse, jsAddr: TlkJSONbase;

  function FindGoodAddress(const sQualityCode: string): TlkJSONobject;
  var
    jsItem: TlkJSONbase;
    i, iCount: Integer;
  begin
    Result := nil;

    iCount := TlkJSONlist(jsResponse).Count - 1;
    for i := 0 to iCount do
    begin
      jsItem := TlkJSONlist(jsResponse).Child[i];
      if Assigned(jsItem) and (jsItem is TlkJSONobject)
        and SameText(TlkJSONobject(jsItem).getStringFromName(caQualityCode), sQualityCode) then
      begin
        Result := TlkJSONobject(jsItem);
        Break;
      end;
    end;
  end;

begin
  Result := sAddress;

  // Составляем JSON запрос
  jsRequest := TlkJSONList.Create;
  try
    // Для гарантии отправляем запрос в трех разных вариантах, потом выберем лучший
    jsReqAddr := TlkJSONObject.Create;
    jsReqAddr.Add(caId, 'addr');
    jsReqAddr.Add(caOriginalAddress, sAddress);
    jsRequest.Add(jsReqAddr);

    if (sIndex <> EmptyStr) and not AnsiContainsText(sAddress, sIndex) then
    begin
      jsReqAddr := TlkJSONObject.Create;
      jsReqAddr.Add(caId, 'addr_index');
      jsReqAddr.Add(caOriginalAddress, sIndex + ', ' + sAddress);
      jsRequest.Add(jsReqAddr);
    end;

    if (sCity <> EmptyStr) and not AnsiContainsText(sAddress, sCity) then
    begin
      jsReqAddr := TlkJSONObject.Create;
      if (sIndex = EmptyStr) or AnsiContainsText(sAddress, sIndex) then
      begin
        jsReqAddr.Add(caId, 'addr_city');
        jsReqAddr.Add(caOriginalAddress, sCity + ', ' + sAddress);
      end
      else begin
        jsReqAddr.Add(caId, 'addr_index_city');
        jsReqAddr.Add(caOriginalAddress, sIndex + ', ' + sCity + ', ' + sAddress);
      end;
      jsRequest.Add(jsReqAddr);
    end;

    // Отправляем запрос к API
    jsResponse := TlkJSON.ParseText(
      rPochtaAPI_CallAPI(aProxy, '/1.0/clean/address', 'post', sToken, sUser,
      TlkJSON.GenerateText(jsRequest)));

    if Assigned(jsResponse) then
    begin
      try
        // Находим самый качественный вариант адреса
        jsAddr := FindGoodAddress(qcGOOD);
        // Абонентский ящик
        if not Assigned(jsAddr) then
          jsAddr := FindGoodAddress(qcPOSTAL_BOX);
        // До востребования
        if not Assigned(jsAddr) then
          jsAddr := FindGoodAddress(qcON_DEMAND);
        // Не определена квартира/офис (для юрлиц), пригоден для отправки
        if not Assigned(jsAddr) then
          jsAddr := FindGoodAddress(qcUNDEF_05);
        // Иностранный адрес
        if not Assigned(jsAddr) then
          jsAddr := FindGoodAddress(qcUNDEF_07);

        // Удалось распознать адрес
        if Assigned(jsAddr) then
          // JSON возвращается в UTF8!
          Result := TlkJSON.GenerateText(jsAddr)
        else begin
          // Не удалось распознать адрес
          jsAddr := FindGoodAddress(qcUNDEF_06);
          if Assigned(jsAddr) then
            raise EPochtaAPIException.Create(6, rsQualityCode_UNDEF_06, Utf8Decode(TlkJSON.GenerateText(jsAddr)));
          jsAddr := FindGoodAddress(qcUNDEF_04);
          if Assigned(jsAddr) then
            raise EPochtaAPIException.Create(4, rsQualityCode_UNDEF_04, Utf8Decode(TlkJSON.GenerateText(jsAddr)));
          jsAddr := FindGoodAddress(qcUNDEF_03);
          if Assigned(jsAddr) then
            raise EPochtaAPIException.Create(3, rsQualityCode_UNDEF_03, Utf8Decode(TlkJSON.GenerateText(jsAddr)));
          jsAddr := FindGoodAddress(qcUNDEF_02);
          if Assigned(jsAddr) then
            raise EPochtaAPIException.Create(2, rsQualityCode_UNDEF_02, Utf8Decode(TlkJSON.GenerateText(jsAddr)));
          jsAddr := FindGoodAddress(qcUNDEF_01);
          if Assigned(jsAddr) then
            raise EPochtaAPIException.Create(1, rsQualityCode_UNDEF_01, Utf8Decode(TlkJSON.GenerateText(jsAddr)));

          raise EPochtaAPIException.Create(-1, rsQualityCode_UNDEF_06, Utf8Decode(TlkJSON.GenerateText(jsResponse)));
        end;
      finally
        jsResponse.Free;
      end;
    end;
  finally
    jsRequest.Free;
  end;
end;

function rPochtaAPI_AddressPart(const jsonAddress: string; const sPartName: string): string;
var
  jsAddress: TlkJSONbase;
begin
  Result := EmptyStr;

  // JSON принимается в UTF8!
  jsAddress := TlkJSON.ParseText(jsonAddress);
  if Assigned(jsAddress) then
  begin
    try
      Result := TlkJSONobject(jsAddress).getStringFromName(sPartName);
    finally
      jsAddress.Free;
    end;
  end;
end;

function rPochtaAPI_AddressFormat(const jsonAddress: string; const bUseIndex: Boolean): string;
var
  jsAddress: TlkJSONbase;

  procedure AddAddressPart(const jsAddress: TlkJSONobject; const sPartName, sDivider: string);
  var
    sValue: string;
  begin
    sValue := jsAddress.getStringFromName(sPartName);
    if sValue <> EmptyStr then
    begin
      if Result = EmptyStr
      then Result := sValue
      else Result := Result + sDivider + sValue;
    end;
  end;

begin
  Result := EmptyStr;

  // JSON принимается в UTF8!
  jsAddress := TlkJSON.ParseText(jsonAddress);
  if Assigned(jsAddress) then
  begin
    try
      if bUseIndex then
        Result := TlkJSONobject(jsAddress).getStringFromName(caIndex);
      AddAddressPart(TlkJSONobject(jsAddress), caRegion, ', ');
      AddAddressPart(TlkJSONobject(jsAddress), caArea, ', ');
      if not SameText(TlkJSONobject(jsAddress).getStringFromName(caRegion),
                      TlkJSONobject(jsAddress).getStringFromName(caPlace)) then
        AddAddressPart(TlkJSONobject(jsAddress), caPlace, ', ');
      AddAddressPart(TlkJSONobject(jsAddress), caLocation, ', ');
      AddAddressPart(TlkJSONobject(jsAddress), caStreet, ', ');
      AddAddressPart(TlkJSONobject(jsAddress), caHouse, ', д ');
      AddAddressPart(TlkJSONobject(jsAddress), caSlash, ' / ');
      AddAddressPart(TlkJSONobject(jsAddress), caBuilding, ', стр ');
      AddAddressPart(TlkJSONobject(jsAddress), caCorpus, ', корп ');
      AddAddressPart(TlkJSONobject(jsAddress), caLetter, ' ');
      AddAddressPart(TlkJSONobject(jsAddress), caHotel, ', гост ');
      AddAddressPart(TlkJSONobject(jsAddress), caRoom, ', кв ');
    finally
      jsAddress.Free;
    end;
  end;
end;

function rPochtaAPI_FioClean(const aProxy: PProxyData; const sToken, sUser: string; const sFio: string): string;
var
  jsRequest: TlkJSONlist;
  jsReqFio: TlkJSONObject;
  jsResponse, jsFio: TlkJSONbase;
begin
  // Составляем JSON запрос
  jsRequest := TlkJSONList.Create;
  try
    jsReqFio := TlkJSONObject.Create;
    jsReqFio.Add(caId, 'fio');
    jsReqFio.Add(caOriginalFio, sFio);
    jsRequest.Add(jsReqFio);

    // Отправляем запрос к API
    jsResponse := TlkJSON.ParseText(
      rPochtaAPI_CallAPI(aProxy, '/1.0/clean/physical', 'post', sToken, sUser,
      TlkJSON.GenerateText(jsRequest)));

    if Assigned(jsResponse) then
    begin
      try
        if TlkJSONlist(jsResponse).Count = 1 then
        begin
          jsFio := TlkJSONlist(jsResponse).Child[0];

          if Assigned(jsFio) and (jsFio is TlkJSONObject) then
          begin
            if TlkJSONObject(jsFio).getStringFromName(caSurName) = EmptyStr then
              raise EPochtaAPIException.Create(-1, rsQualityCode_FIO_SNAME, Utf8Decode(TlkJSON.GenerateText(jsFio)));
            if TlkJSONObject(jsFio).getStringFromName(caName) = EmptyStr then
              raise EPochtaAPIException.Create(-1, rsQualityCode_FIO_NAME, Utf8Decode(TlkJSON.GenerateText(jsFio)));
            if TlkJSONObject(jsFio).getStringFromName(caMiddleName) = EmptyStr then
              raise EPochtaAPIException.Create(-1, rsQualityCode_FIO_MNAME, Utf8Decode(TlkJSON.GenerateText(jsFio)));

            Result := TlkJSON.GenerateText(jsFio);
          end
          else raise EPochtaAPIException.Create(-1, rsQualityCode_BAD_FIO, Utf8Decode(TlkJSON.GenerateText(jsFio)));

          Result := TlkJSON.GenerateText(jsFio);
        end
        else raise EPochtaAPIException.Create(-1, rsQualityCode_BAD_FIO, Utf8Decode(TlkJSON.GenerateText(jsResponse)));
      finally
        jsResponse.Free;
      end;
    end;
  finally
    jsRequest.Free;
  end;
end;

function rPochtaAPI_FioFormat(const jsonFio: string): string;
var
  jsFio: TlkJSONbase;
begin
  Result := EmptyStr;
  // JSON принимается в UTF8!
  jsFio := TlkJSON.ParseText(jsonFio);
  if Assigned(jsFio)then
  begin
    try
      with TlkJSONObject(jsFio) do
      begin
        Result := getStringFromName(caSurName) +
            #32 + getStringFromName(caName) +
            #32 + getStringFromName(caMiddleName);
      end;
    finally
      jsFio.Free;
    end;
  end;
end;

function rPochtaAPI_OrderJson(const aProxy: PProxyData; const sToken, sUser: string; const sOrderId: string): string;
begin
  Result := rPochtaAPI_CallAPI(aProxy, Format('/1.0/backlog/%s', [sOrderId]), 'get', sToken, sUser, EmptyStr);
end;

function rPochtaAPI_OrderData(const aProxy: PProxyData; const sToken, sUser: string; const sOrderId: string): TPochatAPI_Order;
var
  jsResponse: TlkJSONbase;
begin
  FillChar(Result, SizeOf(Result), 0);

  jsResponse := TlkJSON.ParseText(rPochtaAPI_OrderJson(aProxy, sToken, sUser, sOrderId));
  if Assigned(jsResponse) then
  begin
    try
      with TlkJSONObject(jsResponse) do
      begin
        Result.sId := IntToStr(getInt64FromName(caId));
        Result.sBarcode := getStringFromName(caBarcode);
        Result.iTotalRateWoVat := getIntFromName(caTotalRateWoVat);
        Result.iTotalRateVat := getIntFromName(caTotalRateVat);
        Result.iTotalRate := Result.iTotalRateWoVat + Result.iTotalRateVat;
      end;
    finally
      jsResponse.Free;
    end;
  end;
end;

function rPochtaAPI_OrderCreate(const aProxy: PProxyData; const sToken, sUser: string; const jsonParams: string;
  const bRecipientIsFio: Boolean; var sIndex, sCity, sAddress, sRecipient: string): string;
var
  jsonAddrTo, jsonFioTo: string;
  jsAddrTo, jsFioTo, jsReqData: TlkJSONObject;
  jsRequest: TlkJSONlist;
  jsResponse, jsArray, jsErrCodes: TlkJSONbase;
  sError, sErrors: string;
  i: Integer;

  procedure AddParameter(const jsFrom: TlkJSONObject; const sNameFrom, sNameTo: string);
  var
    sValue: string;
  begin
    sValue := jsFrom.getStringFromName(sNameFrom);
    if sValue <> EmptyStr then
      jsReqData.Add(sNameTo, sValue);
  end;

begin
  Result := EmptyStr;

  // Нормализуем адрес
  jsonAddrTo := rPochtaAPI_AddressClean(aProxy, sToken, sUser, sIndex, sCity, sAddress);
  jsAddrTo := TlkJSON.ParseText(jsonAddrTo) as TlkJSONObject;
  // Возвращаем в те же переменные нормализованные данные
  sAddress := rPochtaAPI_AddressFormat(jsonAddrTo, sIndex = EmptyStr);
  sIndex := TlkJSONobject(jsAddrTo).getStringFromName(caIndex);
  sCity := TlkJSONobject(jsAddrTo).getStringFromName(caPlace);
  try
    // Нормализуем ФИО, если это физлицо
    jsFioTo := nil;
    if bRecipientIsFio then
    begin
      jsonFioTo := rPochtaAPI_FioClean(aProxy, sToken, sUser, sRecipient);
      jsFioTo := TlkJSON.ParseText(jsonFioTo) as TlkJSONObject;
      // Возвращаем в ту же переменную нормализованное ФИО
      sRecipient := rPochtaAPI_FioFormat(jsonFioTo);
    end;
    try
      // Создаем JSON параметров заказа (внимание! его не нужно удалять самому!)
      jsReqData := TlkJSON.ParseText(jsonParams) as TlkJSONObject;

      // Добавляем адрес получателя частями
      AddParameter(jsAddrTo, caAddressType, caAddressType + caTo);
      AddParameter(jsAddrTo, caIndex, caIndex + caTo);
      AddParameter(jsAddrTo, caRegion, caRegion + caTo);
      AddParameter(jsAddrTo, caArea, caArea + caTo);
      AddParameter(jsAddrTo, caPlace, caPlace + caTo);
      AddParameter(jsAddrTo, caLocation, caLocation + caTo);
      AddParameter(jsAddrTo, caStreet, caStreet + caTo);
      AddParameter(jsAddrTo, caHouse, caHouse + caTo);
      AddParameter(jsAddrTo, caSlash, caSlash + caTo);
      AddParameter(jsAddrTo, caBuilding, caBuilding + caTo);
      AddParameter(jsAddrTo, caCorpus, caCorpus + caTo);
      AddParameter(jsAddrTo, caLetter, caLetter + caTo);
      AddParameter(jsAddrTo, caHotel, caHotel + caTo);
      AddParameter(jsAddrTo, caRoom, caRoom + caTo);

      // Добавляем наименование получателя одной строкой (ФИО, наименование организации)
      jsReqData.Add(caRecipientName, sRecipient);
      // Добавляем ФИО получателя частями
      if Assigned(jsFioTo) then
      begin
        AddParameter(jsFioTo, caSurname, caSurname);
        AddParameter(jsFioTo, caName, caGivenName);
        AddParameter(jsFioTo, caMiddleName, caMiddleName);
      end;

      // Добавляем созранные параметры в массив
      jsRequest := TlkJSONlist.Create;
      try
        jsRequest.Add(jsReqData);

        // Отправляем запрос к API
        jsResponse := TlkJSON.ParseText(
          rPochtaAPI_CallAPI(aProxy, '/1.0/user/backlog', 'put', sToken, sUser,
          TlkJSON.GenerateText(jsRequest)));

        if Assigned(jsResponse) then
        begin
          try
            // Проверяем ответ на наличие ошибок
            jsArray := TlkJSONObject(jsResponse).Field[caErrors];
            if Assigned(jsArray) and (TlkJSONlist(jsArray).Count > 0) then
            begin
              // Берем только самую первую ошибку, так как у нас только один заказ
              jsErrCodes := TlkJSONObject(TlkJSONlist(jsArray).Child[0]).Field[caErrorCodes] as TlkJSONlist;
              for i := 0 to jsErrCodes.Count - 1 do
              begin
                sError := TlkJSONObject(jsErrCodes.Child[i]).getStringFromName(caCode)
                 + ': ' + TlkJSONObject(jsErrCodes.Child[i]).getStringFromName(caDescription);
                if i = 0 then sErrors := sError else sErrors := sErrors + #13#13 + sError;
              end;

              raise EPochtaAPIException.Create(-1, sErrors, Utf8Decode(TlkJSON.GenerateText(jsErrCodes)));
            end;

            // Ошибок нет, возращаем результат
            jsArray := TlkJSONObject(jsResponse).Field[caResultIds];
            if Assigned(jsArray) and (TlkJSONlist(jsArray).Count > 0) then
              Result := TlkJSON.GenerateText(TlkJSONlist(jsArray).Child[0]);
          finally
            jsResponse.Free;
          end;
        end;
      finally
        jsRequest.Free;
      end;
    finally
      jsFioTo.Free;
    end;
  finally
    jsAddrTo.Free;
  end;
end;

procedure rPochtaAPI_OrderDelete(const aProxy: PProxyData; const sToken, sUser: string; const sOrderId: string);
begin
  rPochtaAPI_CallAPI(aProxy, '/1.0/backlog', 'delete', sToken, sUser,
    Utf8Encode(Format('[%s]', [sOrderId])));
end;

end.
