unit RServices;

interface

uses
  SysUtils, Windows;

procedure CreateServiceDescription(const ServiceName, Description: string);
procedure DeleteServiceDescription(const ServiceName: string);

implementation

uses
  Registry, SvcMgr;

const
  ServicePath          = 'SYSTEM\CurrentControlSet\Services\';
  ServiceDecriptionKey = 'Description';

resourcestring
  SErrorOpenKey  = 'Ошибка доступа к ключу реестра: ''%s''';
  SErrorWriteKey = 'Ошибка записи в реестр: ''%s''';

procedure CreateServiceDescription(const ServiceName, Description: string);
var
  EventLog: TEventLogger;
begin
  EventLog := TEventLogger.Create(ServiceName);
  try
    try
      with TRegistry.Create(KEY_ALL_ACCESS) do
      begin
        try
          RootKey := HKEY_LOCAL_MACHINE;
          if OpenKey(ServicePath + ServiceName, True) then
          begin
            try
              WriteString(ServiceDecriptionKey, Description);
            finally
              CloseKey;
            end;
          end
          else EventLog.LogMessage(Format(SErrorOpenKey,
            [ServicePath + ServiceName]), EVENTLOG_ERROR_TYPE, 0, 0);
        finally
          Free;
        end;
      end;
    except
      on E: Exception do
        EventLog.LogMessage(Format(SErrorWriteKey, [E.Message]), EVENTLOG_ERROR_TYPE, 0, 0);
    end;
  finally
    EventLog.Free;
  end;
end;

procedure DeleteServiceDescription(const ServiceName: string);
var
  EventLog: TEventLogger;
begin
  EventLog := TEventLogger.Create(ServiceName);
  try
    try
      with TRegistry.Create(KEY_ALL_ACCESS) do
      begin
        try
          RootKey := HKEY_LOCAL_MACHINE;
          if OpenKey(ServicePath + ServiceName, False) then
          begin
            try
              if ValueExists(ServiceDecriptionKey)
              then DeleteValue(ServiceDecriptionKey);
            finally
              CloseKey;
            end;
          end
          else EventLog.LogMessage(Format(SErrorOpenKey,
            [ServicePath + ServiceName]), EVENTLOG_ERROR_TYPE, 0, 0);
        finally
          Free;
        end;
      end;
    except
      on E: Exception do
        EventLog.LogMessage(Format(SErrorWriteKey, [E.Message]), EVENTLOG_ERROR_TYPE, 0, 0);
    end;
  finally
    EventLog.Free;
  end;
end;

end.
