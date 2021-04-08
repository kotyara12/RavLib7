unit RSrvControl;

interface

uses
  SysUtils, Windows, WinSvc;

type
  TServiceManager = class
  private
    ServiceControlManager: SC_Handle;
    ServiceHandle: SC_Handle;
  protected
    function DoStartService(NumberOfArgument: DWORD; ServiceArgVectors: PChar): Boolean;
  public
    function  Connect(MachineName: PChar = nil; DatabaseName: PChar = nil;
      Access: DWORD = SC_MANAGER_ALL_ACCESS): Boolean;
    procedure Disconnect;
    function  OpenServiceConnection(ServiceName: PChar): Boolean;
    procedure CloseServiceConnection;
    function  CreateService(const ServiceName, DisplayName, ExeName: string;
      const DesiredAccess, ServiceType, StartType, ErrorControl: Cardinal;
      const Dependencies, ServiceStartName, Password: PAnsiChar): Boolean;
    function  DeleteService: Boolean;
    function  StartService: Boolean; overload;
    function  StartService(NumberOfArgument: DWORD; ServiceArgVectors: PChar): Boolean; overload;
    function  StopService: Boolean;
    procedure PauseService;
    procedure ContinueService;
    procedure ShutdownService;
    function  GetStatus: DWORD;
    function  ServiceRunning: Boolean;
    function  ServiceStopped: Boolean;
  end;

implementation

{ TServiceManager }

function TServiceManager.Connect(MachineName, DatabaseName: PChar; Access: DWORD): Boolean;
begin
  ServiceControlManager := OpenSCManager(MachineName, DatabaseName, Access);
  Result := (ServiceControlManager <> 0);
end;

procedure TServiceManager.Disconnect;
begin
  CloseServiceConnection;
  if ServiceControlManager <> 0 then
    CloseServiceHandle(ServiceControlManager);
  ServiceControlManager := 0;
end;

function TServiceManager.OpenServiceConnection(ServiceName: PChar): Boolean;
begin
  ServiceHandle := OpenService(ServiceControlManager, ServiceName, SERVICE_ALL_ACCESS);
  Result := (ServiceHandle <> 0);
end;

procedure TServiceManager.CloseServiceConnection;
begin
  if ServiceHandle <> 0 then
    CloseServiceHandle(ServiceHandle);
  ServiceHandle := 0;
end;

function TServiceManager.GetStatus: DWORD;
var
  ServiceStatus: TServiceStatus;
begin
  QueryServiceStatus(ServiceHandle, ServiceStatus);
  Result := ServiceStatus.dwCurrentState;
end;

function TServiceManager.ServiceRunning: Boolean;
begin
  Result := GetStatus = SERVICE_RUNNING;
end;

function TServiceManager.ServiceStopped: Boolean;
begin
  Result := GetStatus = SERVICE_STOPPED;
end;

function TServiceManager.DoStartService(NumberOfArgument: DWORD; ServiceArgVectors: PChar): Boolean;
begin
  Result := Boolean(WinSvc.StartService(ServiceHandle, NumberOfArgument, ServiceArgVectors));
end;

function TServiceManager.CreateService(const ServiceName, DisplayName, ExeName: string;
  const DesiredAccess, ServiceType, StartType, ErrorControl: Cardinal;
  const Dependencies, ServiceStartName, Password: PAnsiChar): Boolean;
begin
  Result := Boolean(WinSvc.CreateService(ServiceControlManager,
    PAnsiChar(ServiceName), PAnsiChar(DisplayName),
    DesiredAccess, ServiceType, StartType, ErrorControl, PAnsiChar(ExeName),
    nil, nil, Dependencies, ServiceStartName, Password));
end;

function TServiceManager.DeleteService: Boolean;
begin
  Result := Boolean(WinSvc.DeleteService(ServiceHandle));
end;

function TServiceManager.StartService(NumberOfArgument: DWORD; ServiceArgVectors: PChar): Boolean;
begin
  Result := DoStartService(NumberOfArgument, ServiceArgVectors);
end;

function TServiceManager.StartService: Boolean;
begin
  Result := DoStartService(0, '');
end;

function TServiceManager.StopService: Boolean;
var
  ServiceStatus: TServiceStatus;
begin
  Result := ControlService(ServiceHandle, SERVICE_CONTROL_STOP, ServiceStatus);
end;

procedure TServiceManager.PauseService;
var
  ServiceStatus: TServiceStatus;
begin
  ControlService(ServiceHandle, SERVICE_CONTROL_PAUSE, ServiceStatus);
end;

procedure TServiceManager.ContinueService;
var
  ServiceStatus: TServiceStatus;
begin
  ControlService(ServiceHandle, SERVICE_CONTROL_CONTINUE, ServiceStatus);
end;

procedure TServiceManager.ShutdownService;
var
  ServiceStatus: TServiceStatus;
begin
  ControlService(ServiceHandle, SERVICE_CONTROL_SHUTDOWN, ServiceStatus);
end;

end.