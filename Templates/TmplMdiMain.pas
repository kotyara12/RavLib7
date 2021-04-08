unit TmplMdiMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ActnCtrls, ToolWin, ImgList, ActnList, StdActns, Menus,
  ExtCtrls, AppEvnts, TmplBase, TmplStorage;

type
  TMdiMainTemplate = class(TStorageTemplate)
    StatusBar: TStatusBar;
    ImageList: TImageList;
    ActionList: TActionList;
    FileExit: TFileExit;
    WindowClose: TWindowClose;
    WindowCascade: TWindowCascade;
    WindowTileHorizontal: TWindowTileHorizontal;
    WindowTileVertical: TWindowTileVertical;
    WindowMinimizeAll: TWindowMinimizeAll;
    WindowArrange: TWindowArrange;
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    itemFileExit: TMenuItem;
    CoolBar: TCoolBar;
    BaseToolBar: TToolBar;
    FileExitToolButton: TToolButton;
    menuWindow: TMenuItem;
    itemWindowCascade: TMenuItem;
    itemWindowTileHorizontal: TMenuItem;
    itemWindowTileVertical: TMenuItem;
    itemWindowMinimizeAll: TMenuItem;
    itemWindowClose: TMenuItem;
    itemWindowArrange: TMenuItem;
    menuHelp: TMenuItem;
    HelpKeyword: TAction;
    About: TAction;
    itemHelpKeyword: TMenuItem;
    divHelp1: TMenuItem;
    itemAbout: TMenuItem;
    ApplicationEvents: TApplicationEvents;
    AppToolBar: TToolBar;
    BarPopupMenu: TPopupMenu;
    divHelp2: TMenuItem;
    AutoUpdatesEnabled: TAction;
    AutoUpdatesRun: TAction;
    itemAutoUpdatesEnabled: TMenuItem;
    itemAutoUpdatesRun: TMenuItem;
    procedure HelpKeywordUpdate(Sender: TObject);
    procedure HelpKeywordExecute(Sender: TObject);
    procedure AboutUpdate(Sender: TObject);
    procedure AboutExecute(Sender: TObject);
    procedure ApplicationEventsHint(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BarPopupMenuPopup(Sender: TObject);
    procedure AutoUpdatesEnabledUpdate(Sender: TObject);
    procedure AutoUpdatesEnabledExecute(Sender: TObject);
    procedure AutoUpdatesRunUpdate(Sender: TObject);
    procedure AutoUpdatesRunExecute(Sender: TObject);
  private
    FStartTimer: TTimer;
    FCloseAppQuery: Boolean;
    FSelfUpdatesEnabled: Boolean;
    FSelfUpdatesOnStart: Boolean;
    procedure StartInitApplication(Sender: TObject);
    procedure InitCoolPopupMenu;
    procedure OnBarPropupClick(Sender: TObject);
  protected
    procedure InitForm; override;
    procedure DoneForm; override;
    function  InitApplication: Boolean; virtual;
    procedure InitExceptionsHandler; virtual;
    procedure DoneApplication; dynamic;
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    procedure LoadFormControls; override;
    procedure SaveFormControls; override;
    function  UniqueMdiWins: Boolean; virtual;
    procedure CloseAllWindows;
  end;

implementation

{$R *.dfm}

{$IFNDEF DEMO}
{$IFDEF AU}
  {$DEFINE AU_ANY}
{$ENDIF}
{$IFDEF AU2}
  {$DEFINE AU_ANY}
{$ENDIF}
{$IFDEF AUCFG}
  {$DEFINE AU_ANY}
{$ENDIF}
{$ENDIF}

uses
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  {$IFDEF AU} RSelfUpdate, PrjVariables, {$ENDIF}
  {$IFDEF AU2} RSelfUpdate2, PrjVariables, {$ENDIF}
  {$IFDEF AUCFG} RSelfUpdate2, RIniFiles, PrjVariables, {$ENDIF}
  RVclUtils, RFrmStorage, RDialogs, RMsgRu, RExHandlers, RSysUtils,
  RExHandlersSb, RExHandlersExDlg,
  AboutForm;

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TMdiMainTemplate.SetStyle;
begin
  // inherited;
  Ctl3D := ApplicationStyle.DataForm.Ctl3D;
  FontDataToFont(ApplicationStyle.DataForm.FormFont, Font);
  CoolBar.Visible := ApplicationStyle.MainForm.TbVisible;
  StatusBar.Visible := ApplicationStyle.MainForm.SbVisible;
end;
{$ENDIF}

function TMdiMainTemplate.UniqueMdiWins: Boolean;
begin
  Result := False;
end;

{ == Инициализация формы ======================================================= }
procedure TMdiMainTemplate.InitForm;
begin
  FCloseAppQuery := False;
  {$IFDEF AU_ANY}
  FSelfUpdatesEnabled := True;
  FSelfUpdatesOnStart := False;

  AutoUpdatesEnabled.Visible := True;
  AutoUpdatesRun.Visible := True;
  divHelp2.Visible := True;
  {$ELSE}
  FSelfUpdatesEnabled := False;
  FSelfUpdatesOnStart := False;
  {$ENDIF}
  inherited;
  if not Assigned(FStartTimer) then
  begin
    FStartTimer := TTimer.Create(Self);
    FStartTimer.Interval := 250;
    FStartTimer.OnTimer := StartInitApplication;
    FStartTimer.Enabled := True;
  end;
end;

{ == Сохранение и восстановление размеров формы ================================ }
const
  iniCloseAppQuery      = 'CloseAppQuery';
  iniSelfUpdatesOnStart = 'SelfUpdateOnRun';

procedure TMdiMainTemplate.LoadFormControls;
begin
  inherited;
  LoadCoolBar(Self, CoolBar);
  FCloseAppQuery := LoadFormBoolean(Self, iniCloseAppQuery, FCloseAppQuery);
  {$IFDEF AU_ANY}
  FSelfUpdatesOnStart := LoadFormBoolean(Self, iniSelfUpdatesOnStart, True);
  {$ENDIF}
end;

procedure TMdiMainTemplate.SaveFormControls;
begin
  inherited;
  SaveCoolBar(Self, CoolBar);
  SaveFormBoolean(Self, iniCloseAppQuery, FCloseAppQuery);
  {$IFDEF AU_ANY}
  SaveFormBoolean(Self, iniSelfUpdatesOnStart, FSelfUpdatesOnStart);
  {$ENDIF}
end;

{ == Инициализация меню панели ================================================= }
procedure TMdiMainTemplate.InitCoolPopupMenu;
const
  ItemName = 'BarPopupItem_%d';
var
  i: Integer;
  CaptionItem: string;
begin
  while BarPopupMenu.Items.Count > 0 do
    BarPopupMenu.Items[0].Free;
  for i := 0 to CoolBar.Bands.Count - 1 do
  begin
    if CoolBar.Bands[i].Control is TToolBar
    then CaptionItem := TToolBar(CoolBar.Bands[i].Control).Caption
    else CaptionItem := CoolBar.Bands[i].Control.Name;
    BarPopupMenu.Items.Add(NewItem(CaptionItem, 0, CoolBar.Bands[i].Visible,
      True, OnBarPropupClick, 0, Format(ItemName, [i])));
    BarPopupMenu.Items[i].Tag := i;
  end;
end;

procedure TMdiMainTemplate.BarPopupMenuPopup(Sender: TObject);
begin
  InitCoolPopupMenu;
end;

procedure TMdiMainTemplate.OnBarPropupClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    CoolBar.Bands[TMenuItem(Sender).Tag].Visible := not CoolBar.Bands[TMenuItem(Sender).Tag].Visible;
end;

{ == Инициализация приложения ================================================== }
procedure TMdiMainTemplate.StartInitApplication(Sender: TObject);
begin
  if Assigned(FStartTimer) then FStartTimer.Free;
  StartWait;
  ShowInStatusBar(SMsgInitApplication);
  try
    InitExceptionsHandler;
    Application.ProcessMessages;
    InitApplication;
    Application.ProcessMessages;
  finally
    ShowInStatusBar(EmptyStr);
    ExitWait;
  end;
end;

function TMdiMainTemplate.InitApplication: Boolean;
{$IFDEF AUCFG}
var
  sSelfUpdateFile: string;
{$ENDIF}
begin
  Result := True;
  // InitCoolPopupMenu;
  {$IFDEF AU}
  if FSelfUpdatesOnStart then
    rSU_SelfUpdate(urlSelfUpdate, True, 0);
  {$ENDIF}
  {$IFDEF AU2}
  if FSelfUpdatesOnStart then
  begin
    if rsu2_SelfUpdate(sSelfUpdateFile, True, 0) = suInstalled then
      rsu2_SelfRestart;
  end;
  {$ENDIF}
  {$IFDEF AUCFG}
  if FSelfUpdatesOnStart then
  begin
    sSelfUpdateFile := ReadIniString(ExtractFilePath(ParamStr(0)) + SGlobalCfgFile, 'SELF_UPDATE', 'CfgFile', EmptyStr);
    if sSelfUpdateFile <> EmptyStr then
    begin
      if rsu2_SelfUpdate(sSelfUpdateFile, True, 0) = suInstalled then
        rsu2_SelfRestart;
    end;
  end;
  {$ENDIF}
end;

{ == Инициализация обработчика ошибок приложения =============================== }
procedure TMdiMainTemplate.InitExceptionsHandler;
begin
  AppExceptionsHandler := TExceptHandler.Create;
  Application.OnException := AppExceptionsHandler.AppExceptHandler;
  AppExceptionsHandler.ChannelCreate(TStatusExceptChannel, 1, True);
  AppExceptionsHandler.ChannelCreate(TExDlgExceptChannel, 200, True);
end;

{ == Завершение работы приложения ============================================== }
procedure TMdiMainTemplate.DoneForm;
begin
  ShowInStatusBar(SMsgDoneApplication);
  try
    DoneApplication;
  finally
    try
      inherited;
    finally
      Application.OnException := nil;
    end;
  end;
end;

procedure TMdiMainTemplate.CloseAllWindows;
begin
  while MDIChildCount > 0 do
  begin
    MdiChildren[MDIChildCount - 1].Close;
    Application.ProcessMessages;
  end;
end;

procedure TMdiMainTemplate.DoneApplication;
begin
  CloseAllWindows;
end;

{ == Отображение подсказки в строке статуса ==================================== }
procedure TMdiMainTemplate.ApplicationEventsHint(Sender: TObject);
begin
  if IsNotWait then ShowInStatusBar(Application.Hint);
end;

{ == Автоматическое обновление ================================================= }
procedure TMdiMainTemplate.AutoUpdatesEnabledUpdate(Sender: TObject);
begin
  AutoUpdatesEnabled.Checked := FSelfUpdatesOnStart;
  AutoUpdatesEnabled.Enabled := IsNotWait;
end;

procedure TMdiMainTemplate.AutoUpdatesEnabledExecute(Sender: TObject);
begin
  {$IFDEF AU_ANY}
  SaveFormBoolean(Self, iniSelfUpdatesOnStart, FSelfUpdatesOnStart);
  FSelfUpdatesOnStart := not FSelfUpdatesOnStart;
  {$ENDIF}
end;

procedure TMdiMainTemplate.AutoUpdatesRunUpdate(Sender: TObject);
begin
  AutoUpdatesRun.Enabled := IsNotWait;
end;

procedure TMdiMainTemplate.AutoUpdatesRunExecute(Sender: TObject);
{$IFDEF AUCFG}
var
  sSelfUpdateFile: string;
{$ENDIF}
begin
  {$IFDEF AU}
  rSU_SelfUpdate(urlSelfUpdate, True, 0);
  {$ENDIF}
  {$IFDEF AU2}
  if rsu2_SelfUpdate(sSelfUpdateFile, True, 0) = suInstalled then
    rsu2_SelfRestart;
  {$ENDIF}
  {$IFDEF AUCFG}
  sSelfUpdateFile := ReadIniString(ExtractFilePath(ParamStr(0)) + SGlobalCfgFile, 'SELF_UPDATE', 'CfgFile', EmptyStr);
  if sSelfUpdateFile <> sSelfUpdateFile then
    rsu2_SelfUpdate(sSelfUpdateFile, True, 0);
  {$ENDIF}
end;

{ == Вызов справочной информации =============================================== }
procedure TMdiMainTemplate.HelpKeywordUpdate(Sender: TObject);
begin
  HelpKeyword.Enabled := IsNotWait and FileExists(Application.CurrentHelpFile);
end;

procedure TMdiMainTemplate.HelpKeywordExecute(Sender: TObject);
begin
  OpenHelp(Application.CurrentHelpFile, EmptyStr);
end;

{ == Информация о программе ==================================================== }
procedure TMdiMainTemplate.AboutUpdate(Sender: TObject);
begin
  About.Enabled := IsNotWait;
end;

procedure TMdiMainTemplate.AboutExecute(Sender: TObject);
begin
  ShowAbout;
end;

{ == Запрос о завершении работы ================================================ }
procedure TMdiMainTemplate.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := IsNotWait and (not FCloseAppQuery or CloseAppQuery);
end;

end.
