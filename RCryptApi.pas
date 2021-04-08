unit RCryptApi;

interface

uses
  Types, SysUtils, WCrypt2;

type
  CryptApiException = class (Exception);

function StrToHexStr(const InString: string): string;
function HexStrToStr(const InString: string): string;

function EncryptPwd_Text(const sInText, sPassword: string; const dwProvType, dwAlgHash, dwAlgKey: Cardinal): string;
function DecryptPwd_Text(const sInText, sPassword: string; const dwProvType, dwAlgHash, dwAlgKey: Cardinal): string;

function EncryptPwd_Text_RsaShaRc2(const sInText, sPassword: string): string;
function DecryptPwd_Text_RsaShaRc2(const sInText, sPassword: string): string;

function EncryptPwd_Text_RsaShaRc2Hex(const sInText, sPassword: string): string;
function DecryptPwd_Text_RsaShaRc2Hex(const sInText, sPassword: string): string;

function EncryptPwd_Text_RsaShaRc2B64(const sInText, sPassword: string): string;
function DecryptPwd_Text_RsaShaRc2B64(const sInText, sPassword: string): string;

function EncryptPwd_Text_RsaShaRc4(const sInText, sPassword: string): string;
function DecryptPwd_Text_RsaShaRc4(const sInText, sPassword: string): string;

function EncryptPwd_Text_RsaShaRc4Hex(const sInText, sPassword: string): string;
function DecryptPwd_Text_RsaShaRc4Hex(const sInText, sPassword: string): string;

function EncryptPwd_Text_RsaShaRc4B64(const sInText, sPassword: string): string;
function DecryptPwd_Text_RsaShaRc4B64(const sInText, sPassword: string): string;


implementation

uses
  Classes, RSysUtils, SynaCode, RDialogs;

resourcestring
  SErrCryptAcquireContext = 'Ошибка инициализации криптопровайдера: %s';
  SErrCryptCreateHash     = 'Ошибка создания хэш-объекта: %s';
  SErrCryptHashData       = 'Ошибка хэширования: %s';
  SErrCryptDeriveKey      = 'Ошибка генерации ключа: %s';
  SErrCryptGetSize        = 'Ошибка вычисления размера буфера: %s';
  SErrCryptEncrypt        = 'Ошибка шифрования: %s';
  SErrCryptDecrypt        = 'Ошибка дешифрования: %s';

function StrToHexStr(const InString: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(InString) do
    Result := Result + IntToHex(Ord(InString[i]), 2);
end;

function HexStrToStr(const InString: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(InString) do
    if Odd(i) then
      Result := Result + Chr(StrToIntDef('$' + InString[i] + InString[i + 1], 32));
end;

function EncryptPwd_Text(const sInText, sPassword: string; const dwProvType, dwAlgHash, dwAlgKey: Cardinal): string;
var
  hProv: HCRYPTPROV;
  hHash: HCRYPTHASH;
  hSessionKey: HCRYPTKEY;
  pBuffer: PByte;
  dwBufLen: DWord;
  dwDataLen: DWord;
begin
  Result := EmptyStr;
  // Подключение к "встроенному" криптопровайдеру ( pzsProvider = nil )
  if not CryptAcquireContext(@hProv, nil, nil, dwProvType, CRYPT_VERIFYCONTEXT) then
    raise CryptApiException.CreateFmt(SErrCryptAcquireContext, [GetSystemError(True)]);
  try
    // Создаем хэш-объект
    if not CryptCreateHash(hProv, dwAlgHash, 0, 0, @hHash) then
      raise CryptApiException.CreateFmt(SErrCryptCreateHash, [GetSystemError(True)]);
    try
      // Хешируем пароль
      if not CryptHashData(hHash, @sPassword[1], Length(sPassword), 0) then
        raise CryptApiException.CreateFmt(SErrCryptHashData, [GetSystemError(True)]);
      // Создаем сессионный ключ на основании пароля
      if not CryptDeriveKey(hProv, dwAlgKey, hHash, 0, @hSessionKey) then
        raise CryptApiException.CreateFmt(SErrCryptDeriveKey, [GetSystemError(True)]);
      try
        // Вычисляем размер буфера для зашифрованного сообщения
        dwBufLen := Length(sInText);
        if not CryptEncrypt(hSessionKey, 0, True, 0, nil, @dwBufLen, 0) then
          raise CryptApiException.CreateFmt(SErrCryptGetSize, [GetSystemError(True)]);
        // Выделяем память под буфер
        GetMem(pBuffer, dwBufLen);
        try
          // Записываем в буфер исходный текст
          Move(sInText[1], pBuffer^, dwBufLen);
          // Шифруем сообщение
          dwDataLen := Length(sInText);
          if not CryptEncrypt(hSessionKey, 0, True, 0, pBuffer, @dwDataLen, dwBufLen) then
            raise CryptApiException.CreateFmt(SErrCryptEncrypt, [GetSystemError(True)]);
          // Возвращаем расшифрованное сообщение
          SetLength(Result, dwDataLen div SizeOf(Char));
          Move(pBuffer^, Result[1], dwDataLen);
        finally
          // Удаляем буфер
          FreeMem(pBuffer);
        end;
      finally
        // Удаляем сессионный ключ
        CryptDestroyKey(hSessionKey);
      end;
    finally
      // Удаляем хэш-объект
      CryptDestroyHash(hHash);
    end;
  finally
    // Удаляем подключение к криптопровайдеру
    CryptReleaseContext(hProv, 0);
  end;
end;

function EncryptPwd_Text_RsaShaRc2(const sInText, sPassword: string): string;
begin
  Result := EncryptPwd_Text(sInText, sPassword, PROV_RSA_FULL, CALG_SHA, CALG_RC2);
end;

function EncryptPwd_Text_RsaShaRc4(const sInText, sPassword: string): string;
begin
  Result := EncryptPwd_Text(sInText, sPassword, PROV_RSA_FULL, CALG_SHA, CALG_RC4);
end;

function EncryptPwd_Text_RsaShaRc2Hex(const sInText, sPassword: string): string;
begin
  Result := StrToHexStr(EncryptPwd_Text(sInText, sPassword, PROV_RSA_FULL, CALG_SHA, CALG_RC2));
end;

function EncryptPwd_Text_RsaShaRc4Hex(const sInText, sPassword: string): string;
begin
  Result := StrToHexStr(EncryptPwd_Text(sInText, sPassword, PROV_RSA_FULL, CALG_SHA, CALG_RC4));
end;

function EncryptPwd_Text_RsaShaRc2B64(const sInText, sPassword: string): string;
begin
  Result := EncodeBase64(EncryptPwd_Text(sInText, sPassword, PROV_RSA_FULL, CALG_SHA, CALG_RC2));
end;

function EncryptPwd_Text_RsaShaRc4B64(const sInText, sPassword: string): string;
begin
  Result := EncodeBase64(EncryptPwd_Text(sInText, sPassword, PROV_RSA_FULL, CALG_SHA, CALG_RC4));
end;

function DecryptPwd_Text(const sInText, sPassword: string; const dwProvType, dwAlgHash, dwAlgKey: Cardinal): string;
var
  hProv: HCRYPTPROV;
  hHash: HCRYPTHASH;
  hSessionKey: HCRYPTKEY;
  pBuffer: PByte;
  dwBufLen: DWord;
begin
  Result := EmptyStr;
  if sInText <> '' then
  begin
    // Подключение к "встроенному" криптопровайдеру ( pzsProvider = nil )
    if not CryptAcquireContext(@hProv, nil, nil, dwProvType, CRYPT_VERIFYCONTEXT) then
      raise CryptApiException.CreateFmt(SErrCryptAcquireContext, [GetSystemError(True)]);
    try
      // Создаем хэш-объект
      if not CryptCreateHash(hProv, dwAlgHash, 0, 0, @hHash) then
        raise CryptApiException.CreateFmt(SErrCryptCreateHash, [GetSystemError(True)]);
      try
        // Хешируем пароль
        if not CryptHashData(hHash, @sPassword[1], Length(sPassword), 0) then
          raise CryptApiException.CreateFmt(SErrCryptHashData, [GetSystemError(True)]);
        // Создаем сессионный ключ на основании пароля
        if not CryptDeriveKey(hProv, dwAlgKey, hHash, 0, @hSessionKey) then
          raise CryptApiException.CreateFmt(SErrCryptDeriveKey, [GetSystemError(True)]);
        try
          // Выделяем память под буфер
          dwBufLen := Length(sInText) * SizeOf(Char);
          GetMem(pBuffer, dwBufLen);
          try
            // Записываем в буфер исходный текст
            Move(sInText[1], pBuffer^, dwBufLen);
            // Расшифровываем сообщение
            if not CryptDecrypt(hSessionKey, 0, True, 0, pBuffer, @dwBufLen) then
              raise CryptApiException.CreateFmt(SErrCryptDecrypt, [GetSystemError(True)]);
            // Возвращаем расшифрованное сообщение
            SetLength(Result, dwBufLen div SizeOf(Char));
            Move(pBuffer^, Result[1], dwBufLen);
          finally
            // Удаляем буфер
            FreeMem(pBuffer);
          end;
        finally
          // Удаляем сессионный ключ
          CryptDestroyKey(hSessionKey);
        end;
      finally
        // Удаляем хэш-объект
        CryptDestroyHash(hHash);
      end;
    finally
      // Удаляем подключение к криптопровайдеру
      CryptReleaseContext(hProv, 0);
    end;
  end;
end;

function DecryptPwd_Text_RsaShaRc2(const sInText, sPassword: string): string;
begin
  Result := DecryptPwd_Text(sInText, sPassword, PROV_RSA_FULL, CALG_SHA, CALG_RC2);
end;

function DecryptPwd_Text_RsaShaRc4(const sInText, sPassword: string): string;
begin
  Result := DecryptPwd_Text(sInText, sPassword, PROV_RSA_FULL, CALG_SHA, CALG_RC4);
end;

function DecryptPwd_Text_RsaShaRc2Hex(const sInText, sPassword: string): string;
begin
  Result := DecryptPwd_Text(HexStrToStr(sInText), sPassword, PROV_RSA_FULL, CALG_SHA, CALG_RC2);
end;

function DecryptPwd_Text_RsaShaRc4Hex(const sInText, sPassword: string): string;
begin
  Result := DecryptPwd_Text(HexStrToStr(sInText), sPassword, PROV_RSA_FULL, CALG_SHA, CALG_RC4);
end;

function DecryptPwd_Text_RsaShaRc2B64(const sInText, sPassword: string): string;
begin
  Result := DecryptPwd_Text(DecodeBase64(sInText), sPassword, PROV_RSA_FULL, CALG_SHA, CALG_RC2);
end;

function DecryptPwd_Text_RsaShaRc4B64(const sInText, sPassword: string): string;
begin
  Result := DecryptPwd_Text(DecodeBase64(sInText), sPassword, PROV_RSA_FULL, CALG_SHA, CALG_RC4);
end;

end.
