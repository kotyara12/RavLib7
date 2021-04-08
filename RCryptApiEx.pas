unit RCryptApiEx;

interface

function EncryptPwd_Text_RsaShaRc2B64_Ex(const sInText, sPassword: string): string;
function DecryptPwd_Text_RsaShaRc2B64_Ex(const sInText, sPassword: string): string;

function EncryptPwd_Text_RsaShaRc4B64_Ex(const sInText, sPassword: string): string;
function DecryptPwd_Text_RsaShaRc4B64_Ex(const sInText, sPassword: string): string;

implementation

uses
  SysUtils, RCryptApi, RExHandlers;

function EncryptPwd_Text_RsaShaRc2B64_Ex(const sInText, sPassword: string): string;
begin
  try
    Result := EncryptPwd_Text_RsaShaRc2B64(sInText, sPassword);
  except
    on E: Exception do
    begin
      Result := EmptyStr;
      HandleExcept(E, nil);
    end;
  end;
end;

function DecryptPwd_Text_RsaShaRc2B64_Ex(const sInText, sPassword: string): string;
begin
  try
    Result := DecryptPwd_Text_RsaShaRc2B64(sInText, sPassword);
  except
    on E: Exception do
    begin
      Result := EmptyStr;
      HandleExcept(E, nil);
    end;
  end;
end;


function EncryptPwd_Text_RsaShaRc4B64_Ex(const sInText, sPassword: string): string;
begin
  try
    Result := EncryptPwd_Text_RsaShaRc4B64(sInText, sPassword);
  except
    on E: Exception do
    begin
      Result := EmptyStr;
      HandleExcept(E, nil);
    end;
  end;
end;

function DecryptPwd_Text_RsaShaRc4B64_Ex(const sInText, sPassword: string): string;
begin
  try
    Result := DecryptPwd_Text_RsaShaRc4B64(sInText, sPassword);
  except
    on E: Exception do
    begin
      Result := EmptyStr;
      HandleExcept(E, nil);
    end;
  end;
end;

end.
