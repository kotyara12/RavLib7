unit AdminForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplMdiRss, AppEvnts, Menus, StdActns, ActnList, ImgList,
  ComCtrls, ToolWin;

type
  TFormAdmin = class(TMdiMainRssTemplate)
    RefSrLevelsToolButton: TToolButton;
    itemRefSrLevels: TMenuItem;
    RefSrOpersToolButton: TToolButton;
    itemRefSrOpers: TMenuItem;
    SysLogToolButton: TToolButton;
    SeparatorRef: TToolButton;
    menuSysLog: TMenuItem;
    menuUsers: TMenuItem;
    itemSysLog: TMenuItem;
    itemOpGroups: TMenuItem;
    OpGroupsToolButton: TToolButton;
    SeparatorUser: TToolButton;
    UsersToolButton: TToolButton;
    itemUsers: TMenuItem;
    divUsers1: TMenuItem;
    itemSysSettings: TMenuItem;
    divReferences: TMenuItem;
    SeparatorSettings: TToolButton;
    SysSettingsToolButton: TToolButton;
    itemSysLogSave: TMenuItem;
    itemSysLogClear: TMenuItem;
    divSysLog: TMenuItem;
    SysLogClearToolButton: TToolButton;
    RefSrLevels: TAction;
    RefSrOpers: TAction;
    SysLogView: TAction;
    OpGroups: TAction;
    Users: TAction;
    SysSettings: TAction;
    SysLogSave: TAction;
    SysLogClear: TAction;
    procedure RefSrLevelsUpdate(Sender: TObject);
    procedure RefSrLevelsExecute(Sender: TObject);
    procedure RefSrOpersUpdate(Sender: TObject);
    procedure RefSrOpersExecute(Sender: TObject);
    procedure SysLogViewUpdate(Sender: TObject);
    procedure SysLogViewExecute(Sender: TObject);
    procedure SysLogSaveUpdate(Sender: TObject);
    procedure SysLogSaveExecute(Sender: TObject);
    procedure SysLogClearUpdate(Sender: TObject);
    procedure SysLogClearExecute(Sender: TObject);
    procedure OpGroupsUpdate(Sender: TObject);
    procedure OpGroupsExecute(Sender: TObject);
    procedure UsersUpdate(Sender: TObject);
    procedure UsersExecute(Sender: TObject);
    procedure SysSettingsUpdate(Sender: TObject);
    procedure SysSettingsExecute(Sender: TObject);
  protected
    function InitAppModules: Boolean; override;
  public
  end;

var
  FormAdmin: TFormAdmin;

implementation

uses
  RVclUtils, RMdiIntf, BaseDbUnit, AdminUnit, OprList,
  SrLevelsForm, SrOpersForm, SysLogForm, OpGroupsForm, UsersForm, SettingsForm;

{$R *.dfm}

{ == Иницилизация модулей приложения =========================================== }
function TFormAdmin.InitAppModules: Boolean;
begin
  Result := AdminData.InitAdminData;
end;

{ == Настройка справочника уровней безопасности операций ======================= }
procedure TFormAdmin.RefSrLevelsUpdate(Sender: TObject);
begin
  RefSrLevels.Enabled := IsNotWait and orViewSrLevels and AdminData.SR_LEVELS.Active;
end;

procedure TFormAdmin.RefSrLevelsExecute(Sender: TObject);
begin
  ShowMdiForm(TFormSrLevels, tagViewSrLevels, UniqueMdiWins);
end;

{ == Просмотр справочника операций ============================================= }
procedure TFormAdmin.RefSrOpersUpdate(Sender: TObject);
begin
  RefSrOpers.Enabled := IsNotWait and orViewOperation and AdminData.SR_OPERATIONS.Active;
end;

procedure TFormAdmin.RefSrOpersExecute(Sender: TObject);
begin
  ShowMdiForm(TFormSrOpers, tagViewSrOperations, UniqueMdiWins);
end;

{ == Журнал работы системы ===================================================== }
procedure TFormAdmin.SysLogViewUpdate(Sender: TObject);
begin
  SysLogView.Enabled := IsNotWait and orViewSysLog and BaseData.acDb.Connected;
end;

procedure TFormAdmin.SysLogViewExecute(Sender: TObject);
begin
  ShowMdiForm(TFormSysLog, tagViewSysLog, UniqueMdiWins);
end;

{ == Выгрузить данные системного журнала работы в файл ========================= }
procedure TFormAdmin.SysLogSaveUpdate(Sender: TObject);
begin
  SysLogSave.Enabled := IsNotWait and orViewSysLog and BaseData.acDb.Connected;
end;

procedure TFormAdmin.SysLogSaveExecute(Sender: TObject);
begin
  AdminData.SysLogSave;
end;

{ == Удаление всех записей за предыдущие дни из системного журнала работы ====== }
procedure TFormAdmin.SysLogClearUpdate(Sender: TObject);
begin
  SysLogClear.Enabled := IsNotWait and orClearSysLog and BaseData.acDb.Connected;
end;

procedure TFormAdmin.SysLogClearExecute(Sender: TObject);
begin
  AdminData.SysLogClear;
end;

{ == Управление группами доступа =============================================== }
procedure TFormAdmin.OpGroupsUpdate(Sender: TObject);
begin
  OpGroups.Enabled := IsNotWait and orViewOpGroups and BaseData.acDb.Connected;
end;

procedure TFormAdmin.OpGroupsExecute(Sender: TObject);
begin
  ShowMdiForm(TFormOpGroups, tagViewOpGroups, UniqueMdiWins);
end;

{ == Управление пользователями системы ========================================= }
procedure TFormAdmin.UsersUpdate(Sender: TObject);
begin
  Users.Enabled := IsNotWait and orViewUsers and BaseData.acDb.Connected;
end;

procedure TFormAdmin.UsersExecute(Sender: TObject);
begin
  ShowMdiForm(TFormUsers, tagViewUsers, UniqueMdiWins);
end;

{ == Настройка параметров системы ============================================== }
procedure TFormAdmin.SysSettingsUpdate(Sender: TObject);
begin
  SysSettings.Enabled := IsNotWait and orViewSysSettings and BaseData.acDb.Connected;
end;

procedure TFormAdmin.SysSettingsExecute(Sender: TObject);
begin
  ShowMdiForm(TFormSettings, tagViewSysSettings, UniqueMdiWins);
end;

end.
