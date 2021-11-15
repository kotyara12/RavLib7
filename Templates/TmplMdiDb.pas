unit TmplMdiDb;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplMdiStyled, AppEvnts, Menus, StdActns, ActnList, ImgList,
  ComCtrls, ToolWin, RMessages, DM_TmplBase;

type
  TMdiMainDbTemplate = class(TMdiMainStyledTemplate)
    SetDbConnectParams: TAction;
    divFile1: TMenuItem;
    itemSetDbConnectParams: TMenuItem;
    menuReferences: TMenuItem;
    CreateResqueCopyDb: TAction;
    itemCreateResqueCopyDb: TMenuItem;
    RestoreResqueCopyDb: TAction;
    itemRestoreResqueCopyDb: TMenuItem;
    procedure SetDbConnectParamsUpdate(Sender: TObject);
    procedure SetDbConnectParamsExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CreateResqueCopyDbUpdate(Sender: TObject);
    procedure CreateResqueCopyDbExecute(Sender: TObject);
    procedure RestoreResqueCopyDbUpdate(Sender: TObject);
    procedure RestoreResqueCopyDbExecute(Sender: TObject);
  protected
    function  InitApplication: Boolean; override;
    procedure DoneApplication; override;
    function  InitAppModules: Boolean; virtual;
    procedure DoneAppModules; virtual;
  public
    DbModule: TBaseDataTemplate;
    procedure DoDatabaseConnectOpen;
    procedure DoDatabaseConnectClose;
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RDialogs, RMsgRu;

{ == Инициализация приложения ================================================== }
function TMdiMainDbTemplate.InitApplication: Boolean;
begin
  Result := inherited InitApplication;
  if Result then
    DoDatabaseConnectOpen;
end;

{ == Завершение работы приложения ============================================== }
procedure TMdiMainDbTemplate.DoneApplication;
begin
  try
    DoDatabaseConnectClose;
  finally
    inherited;
  end;
end;

{ == Подключение к базе данных ================================================= }
procedure TMdiMainDbTemplate.DoDatabaseConnectOpen;
begin
  StartWait;
  ShowInStatusBar(SMsgConnDatabase);
  try
    if Assigned(DbModule) then
    begin
      if DbModule.ConnectToDatabase then
      begin
        if not InitAppModules then
        begin
          DbModule.CloseConnection;
          Application.Terminate;
        end;
      end;
    end
    else ErrorBox(EDataModuleNotCreated);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TMdiMainDbTemplate.DoDatabaseConnectClose;
begin
  StartWait;
  ShowInStatusBar(SMsgCloseDatabase);
  try
    if Assigned(DbModule) then
    begin
      try
        DoneAppModules;
      finally
        DbModule.CloseConnection;
      end;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Динамически наследуемые функции подключения к базе данных ================= }
function TMdiMainDbTemplate.InitAppModules: Boolean;
begin
  Result := True;
end;

procedure TMdiMainDbTemplate.DoneAppModules;
begin
  CloseAllWindows;
end;

{ == Установка параметров соединения с базой данных ============================ }
procedure TMdiMainDbTemplate.SetDbConnectParamsUpdate(Sender: TObject);
begin
  SetDbConnectParams.Enabled := IsNotWait and Assigned(DbModule);
end;

procedure TMdiMainDbTemplate.SetDbConnectParamsExecute(Sender: TObject);
begin
  if Assigned(DbModule) then
  begin
    if DbModule.ChangeConnectProperties then
    begin
      if DbModule.acDb.Connected then
      begin
        if QueryBoxStdYN(SQueryReconnect) = ID_YES then
        begin
          DoDatabaseConnectClose;
          DoDatabaseConnectOpen;
        end;
      end
      else DoDatabaseConnectOpen;
    end;
  end
end;

{ == Создание резевной копии БД ================================================ }
procedure TMdiMainDbTemplate.CreateResqueCopyDbUpdate(Sender: TObject);
begin
  CreateResqueCopyDb.Enabled := IsNotWait and Assigned(DbModule) and DbModule.ResqueEnabled;
end;

procedure TMdiMainDbTemplate.CreateResqueCopyDbExecute(Sender: TObject);
begin
  if Assigned(DbModule) and (QueryBoxStdYN(SQueryCloseConnect) = ID_YES) then
  begin
    DoDatabaseConnectClose;
    DbModule.CreateResqueCopy;
    DoDatabaseConnectOpen;
  end;
end;

procedure TMdiMainDbTemplate.RestoreResqueCopyDbUpdate(Sender: TObject);
begin
  RestoreResqueCopyDb.Enabled := IsNotWait and Assigned(DbModule) and DbModule.ResqueEnabled;
end;

procedure TMdiMainDbTemplate.RestoreResqueCopyDbExecute(Sender: TObject);
begin
  if Assigned(DbModule) and (QueryBoxStdYN(SQueryCloseConnect) = ID_YES) then
  begin
    DoDatabaseConnectClose;
    DbModule.RestoreResqueCopy;
    DoDatabaseConnectOpen;
  end;
end;

{ == Запрос о завершении работы ================================================ }
procedure TMdiMainDbTemplate.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not Assigned(DbModule) or not DbModule.AppInitialized;
  if not CanClose then inherited;
end;

end.
