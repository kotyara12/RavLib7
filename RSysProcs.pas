unit RSysProcs;

interface

uses
  RMsgTypes;

function RestartWindowsTitle(const Flags: Longword): string;
function RestartWindowsEx(const Flags: Longword;
  const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
function RestartWindows(const Flags: Longword; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;

{ function AdjustPrivilegeEx(const PrivilegeName: string; const Enable: Boolean;
  const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
function AdjustPrivilege(const PrivilegeName: string; const Enable: Boolean;
  const ErrOptions: TRErrorsOptions; const ShowProc: TRShowInfoNotifyEvent): TROperation; }

function ServiceExists(const ServiceName: string): Boolean;
function ServiceStarted(const ServiceName: string): Boolean;
function ServiceStopped(const ServiceName: string): Boolean;

function StartServiceEx(const ServiceName: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function StartService(const ServiceName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
function StopServiceEx(const ServiceName: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function StopService(const ServiceName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;

implementation

uses
  Windows, SysUtils, WinSvc, RSysUtils;

resourcestring
  SRestartWindows_LOGOFF      = 'Завершение сеанса пользователя';
  SRestartWindows_REBOOT      = 'Перезагрузка Windows';
  SRestartWindows_SHUTDOWN    = 'Завершение работы компьютера';
  SRestartWindows_POWEROFF    = ' (выключить питание компьютера)';
  SRestartWindows_FORCE       = ' (с принудительным завершением программ)';
  SRestartWindows_FORCEIFHUNG = ' (с принудительным завершением "зависших" программ)';

  SPrivilegeOn                = 'Установка дополнительных привилегий: "%s"';
  SPrivilegeOff               = 'Удаление дополнительных привилегий: "%s"';

  SStartService               = 'Запуск сервиса "%s"';
  SStopService                = 'Останов сервиса "%s"';
  SErrorTimeOut               = 'Превышено время ожидания!';
  SServiceAlreadyStarted      = 'Указанная служба уже запущена!';
  SServiceAlreadyStopped      = 'Указанная служба уже остановлена!';

function RestartWindowsTitle(const Flags: Longword): string;
begin
  Result := SRestartWindows_LOGOFF;
  if (EWX_REBOOT and Flags) > 0 then
    Result := SRestartWindows_REBOOT;
  if (EWX_REBOOT and Flags) > 0 then
    Result := SRestartWindows_SHUTDOWN;
  if (EWX_POWEROFF and Flags) > 0 then
    Result := Result + SRestartWindows_POWEROFF;
  if (EWX_FORCE and Flags) > 0 then
    Result := Result + SRestartWindows_FORCE;
  if (EWX_FORCEIFHUNG and Flags) > 0 then
    Result := Result + SRestartWindows_FORCEIFHUNG;
end;

function RestartWindowsEx(const Flags: Longword;
  const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
begin
  InitTransaction(RestartWindowsTitle(Flags), Result, ShowProc);
  try
    try
      if ExitWindows(Flags)
      then Result.Result := GetSystemError(S_OK)
      else begin
        Result.State := msError;
        Result.Result := GetSystemError(GetLastError);
      end;
    except
      on E: Exception do
      begin
        Result.State := msError;
        Result.Result := E.Message;
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function RestartWindows(const Flags: Longword; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
begin
  SetTransactionToOperation(RestartWindowsEx(Flags, ShowProc),
    Result, ErrOptions, ShowProc);
end;

function ServiceExists(const ServiceName: string): Boolean;
var
  SC_Manager: SC_Handle;
  SC_Service: SC_Handle;
begin
  Result := False;
  SC_Manager := WinSvc.OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SC_Manager <> 0 then
  begin
    try
      SC_Service := WinSvc.OpenService(SC_Manager, PAnsiChar(ServiceName),
        SERVICE_QUERY_STATUS);
      if SC_Service <> 0 then
      begin
        Result := True;
        CloseServiceHandle(SC_Service);
      end;
    finally
      CloseServiceHandle(SC_Manager);
    end;
  end;
end;

function ServiceStarted(const ServiceName: string): Boolean;
var
  SC_Manager: SC_Handle;
  SC_Service: SC_Handle;
  ServiceStatus: TServiceStatus;
begin
  Result := False;
  SC_Manager := WinSvc.OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SC_Manager <> 0 then
  begin
    try
      SC_Service := WinSvc.OpenService(SC_Manager, PAnsiChar(ServiceName),
        SERVICE_QUERY_STATUS);
      if SC_Service <> 0 then
      begin
        try
          QueryServiceStatus(SC_Service, ServiceStatus);
          Result := ServiceStatus.dwCurrentState = SERVICE_RUNNING;
        finally
          CloseServiceHandle(SC_Service);
        end;
      end;
    finally
      CloseServiceHandle(SC_Manager);
    end;
  end;
end;

function ServiceStopped(const ServiceName: string): Boolean;
var
  SC_Manager: SC_Handle;
  SC_Service: SC_Handle;
  ServiceStatus: TServiceStatus;
begin
  Result := False;
  SC_Manager := WinSvc.OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SC_Manager <> 0 then
  begin
    try
      SC_Service := WinSvc.OpenService(SC_Manager, PAnsiChar(ServiceName),
        SERVICE_QUERY_STATUS);
      if SC_Service <> 0 then
      begin
        try
          QueryServiceStatus(SC_Service, ServiceStatus);
          Result := ServiceStatus.dwCurrentState = SERVICE_STOPPED;
        finally
          CloseServiceHandle(SC_Service);
        end;
      end;
    finally
      CloseServiceHandle(SC_Manager);
    end;
  end;
end;

function StartServiceEx(const ServiceName: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  SC_Manager: SC_Handle;
  SC_Service: SC_Handle;
  ServiceStatus: TServiceStatus;
  TmpCheckPoint : DWord;
  Args: PAnsiChar;
begin
  InitTransaction(Format(SStartService, [ServiceName]), Result, ShowProc);
  try
    try
      SC_Manager := WinSvc.OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
      if SC_Manager <> 0 then
      begin
        try
          SC_Service := WinSvc.OpenService(SC_Manager, PAnsiChar(ServiceName),
            SERVICE_START or SERVICE_QUERY_STATUS);
          if SC_Service <> 0 then
          begin
            try
              if QueryServiceStatus(SC_Service, ServiceStatus) then
              begin
                if ServiceStatus.dwCurrentState = SERVICE_RUNNING then
                begin
                  Result.State := msIgnored;
                  Result.Result := SServiceAlreadyStarted;
                end
                else begin
                  if WinSvc.StartService(SC_Service, 0, Args) then
                  begin
                    if QueryServiceStatus(SC_Service, ServiceStatus) then
                    begin
                      while (ServiceStatus.dwCurrentState <> SERVICE_RUNNING) do
                      begin
                        if IsTerminatedTran(BreakProc, Result) then
                          Break;
                        TmpCheckPoint := ServiceStatus.dwCheckPoint;
                        Sleep(ServiceStatus.dwWaitHint);
                        if not QueryServiceStatus(SC_Service, ServiceStatus) then
                        begin
                          Result.State := msError;
                          Result.Result := GetSystemError(GetLastError);
                          Break;
                        end;
                        if ServiceStatus.dwCheckPoint < TmpCheckPoint then
                        begin
                          Result.State := msError;
                          Result.Result := SErrorTimeOut;
                          Break;
                        end;
                      end;
                      if QueryServiceStatus(SC_Service, ServiceStatus)
                      and (ServiceStatus.dwCurrentState = SERVICE_RUNNING) then
                      begin
                        Result.State := msOk;
                        Result.Result := GetSystemError(S_OK);
                      end;
                    end
                    else begin
                      Result.State := msError;
                      Result.Result := GetSystemError(GetLastError);
                    end;
                  end
                  else begin
                    Result.State := msError;
                    Result.Result := GetSystemError(GetLastError);
                  end;
                end;
              end
              else begin
                Result.State := msError;
                Result.Result := GetSystemError(GetLastError);
              end;
            finally
              CloseServiceHandle(SC_Service);
            end;
          end
          else begin
            Result.State := msError;
            Result.Result := GetSystemError(GetLastError);
          end;
        finally
          CloseServiceHandle(SC_Manager);
        end;
      end
      else begin
        Result.State := msError;
        Result.Result := GetSystemError(GetLastError);
      end;
    except
      on E: Exception do
      begin
        Result.State := msError;
        Result.Result := E.Message;
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function StartService(const ServiceName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
begin
  SetTransactionToOperation(StartServiceEx(ServiceName, ShowProc, BreakProc),
    Result, ErrOptions, ShowProc);
end;

function StopServiceEx(const ServiceName: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  SC_Manager: SC_Handle;
  SC_Service: SC_Handle;
  ServiceStatus: TServiceStatus;
  TmpCheckPoint : DWord;
begin
  InitTransaction(Format(SStopService, [ServiceName]), Result, ShowProc);
  try
    try
      SC_Manager := WinSvc.OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
      if SC_Manager <> 0 then
      begin
        try
          SC_Service := WinSvc.OpenService(SC_Manager, PAnsiChar(ServiceName),
            SERVICE_START or SERVICE_QUERY_STATUS);
          if SC_Service <> 0 then
          begin
            try
              if QueryServiceStatus(SC_Service, ServiceStatus) then
              begin
                if ServiceStatus.dwCurrentState = SERVICE_STOPPED then
                begin
                  Result.State := msIgnored;
                  Result.Result := SServiceAlreadyStopped;
                end
                else begin
                  if WinSvc.ControlService(SC_Service, SERVICE_CONTROL_STOP, ServiceStatus) then
                  begin
                    if QueryServiceStatus(SC_Service, ServiceStatus) then
                    begin
                      while ServiceStatus.dwCurrentState <> SERVICE_STOPPED do
                      begin
                        if IsTerminatedTran(BreakProc, Result) then
                          Break;
                        TmpCheckPoint := ServiceStatus.dwCheckPoint;
                        Sleep(ServiceStatus.dwWaitHint);
                        if not QueryServiceStatus(SC_Service, ServiceStatus) then
                        begin
                          Result.State := msError;
                          Result.Result := GetSystemError(GetLastError);
                          Break;
                        end;
                        if ServiceStatus.dwCheckPoint < TmpCheckPoint then
                        begin
                          Result.State := msError;
                          Result.Result := SErrorTimeOut;
                          Break;
                        end;
                      end;
                      if QueryServiceStatus(SC_Service, ServiceStatus)
                      and (ServiceStatus.dwCurrentState = SERVICE_STOPPED) then
                      begin
                        Result.State := msOk;
                        Result.Result := GetSystemError(S_OK);
                      end;
                    end
                    else begin
                      Result.State := msError;
                      Result.Result := GetSystemError(GetLastError);
                    end;
                  end
                  else begin
                    Result.State := msError;
                    Result.Result := GetSystemError(GetLastError);
                  end;
                end;
              end
              else begin
                Result.State := msError;
                Result.Result := GetSystemError(GetLastError);
              end;
            finally
              CloseServiceHandle(SC_Service);
            end;
          end
          else begin
            Result.State := msError;
            Result.Result := GetSystemError(GetLastError);
          end;
        finally
          CloseServiceHandle(SC_Manager);
        end;
      end
      else begin
        Result.State := msError;
        Result.Result := GetSystemError(GetLastError);
      end;
    except
      on E: Exception do
      begin
        Result.State := msError;
        Result.Result := E.Message;
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function StopService(const ServiceName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
begin
  SetTransactionToOperation(StopServiceEx(ServiceName, ShowProc, BreakProc),
    Result, ErrOptions, ShowProc);
end;

end.
