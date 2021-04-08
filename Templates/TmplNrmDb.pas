unit TmplNrmDb;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplMdiStyled, AppEvnts, Menus, StdActns, ActnList, ImgList,
  ComCtrls, ToolWin, RMessages, DM_TmplBase, TmplNrmStyled;

type
  TNrmMainDbTemplate = class(TNrmMainStyledTemplate)
    SetDbConnectParams: TAction;
    divFile1: TMenuItem;
    itemSetDbConnectParams: TMenuItem;
    menuReferences: TMenuItem;
    CreateResqueCopyDb: TAction;
    RestoreResqueCopyDb: TAction;
    itemCreateResqueCopyDb: TMenuItem;
    itemRestoreResqueCopyDb: TMenuItem;
    procedure SetDbConnectParamsUpdate(Sender: TObject);
    procedure SetDbConnectParamsExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CreateResqueCopyDbUpdate(Sender: TObject);
    procedure CreateResqueCopyDbExecute(Sender: TObject);
    procedure RestoreResqueCopyDbUpdate(Sender: TObject);
    procedure RestoreResqueCopyDbExecute(Sender: TObject);
  protected
    procedure InitApplication; override;
    procedure DoneApplication; override;
    function  InitAppModules: Boolean; dynamic;
    procedure DoneAppModules; dynamic;
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
procedure TNrmMainDbTemplate.InitApplication;
begin
  try
    inherited;
  finally
    DoDatabaseConnectOpen;
  end;
end;

{ == Завершение работы приложения ============================================== }
procedure TNrmMainDbTemplate.DoneApplication;
begin
  try
    inherited;
  finally
    DoDatabaseConnectClose;
  end;
end;

{ == Подключение к базе данных ================================================= }
procedure TNrmMainDbTemplate.DoDatabaseConnectOpen;
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

procedure TNrmMainDbTemplate.DoDatabaseConnectClose;
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
function TNrmMainDbTemplate.InitAppModules: Boolean;
begin
  Result := True;
end;

procedure TNrmMainDbTemplate.DoneAppModules;
begin
end;

{ == Установка параметров соединения с базой данных ============================ }
procedure TNrmMainDbTemplate.SetDbConnectParamsUpdate(Sender: TObject);
begin
  SetDbConnectParams.Enabled := IsNotWait and Assigned(DbModule);
end;

procedure TNrmMainDbTemplate.SetDbConnectParamsExecute(Sender: TObject);
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
procedure TNrmMainDbTemplate.CreateResqueCopyDbUpdate(Sender: TObject);
begin
  CreateResqueCopyDb.Enabled := IsNotWait and Assigned(DbModule) and DbModule.ResqueEnabled;
end;

procedure TNrmMainDbTemplate.CreateResqueCopyDbExecute(Sender: TObject);
begin
  if Assigned(DbModule) and (QueryBoxStdYN(SQueryCloseConnect) = ID_YES) then
  begin
    DoDatabaseConnectClose;
    DbModule.CreateResqueCopy;
    DoDatabaseConnectOpen;
  end;
end;

procedure TNrmMainDbTemplate.RestoreResqueCopyDbUpdate(Sender: TObject);
begin
  RestoreResqueCopyDb.Enabled := IsNotWait and Assigned(DbModule) and DbModule.ResqueEnabled;
end;

procedure TNrmMainDbTemplate.RestoreResqueCopyDbExecute(Sender: TObject);
begin
  if Assigned(DbModule) and (QueryBoxStdYN(SQueryCloseConnect) = ID_YES) then
  begin
    DoDatabaseConnectClose;
    DbModule.RestoreResqueCopy;
    DoDatabaseConnectOpen;
  end;
end;

{ == Запрос о завершении работы ================================================ }
procedure TNrmMainDbTemplate.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not Assigned(DbModule) or not DbModule.AppInitialized;
  if not CanClose then inherited;
end;

end.
