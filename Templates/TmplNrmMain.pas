unit TmplNrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ActnCtrls, ToolWin, ImgList, ActnList, StdActns, Menus,
  ExtCtrls, AppEvnts, TmplBase, TmplStorage;

type
  TNrmMainTemplate = class(TStorageTemplate)
    StatusBar: TStatusBar;
    ImageList: TImageList;
    ActionList: TActionList;
    FileExit: TFileExit;
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    itemFileExit: TMenuItem;
    CoolBar: TCoolBar;
    BaseToolBar: TToolBar;
    FileExitToolButton: TToolButton;
    menuHelp: TMenuItem;
    HelpKeyword: TAction;
    About: TAction;
    itemHelpKeyword: TMenuItem;
    divHelp1: TMenuItem;
    itemAbout: TMenuItem;
    ApplicationEvents: TApplicationEvents;
    HelpToolButton: TToolButton;
    AboutToolButton: TToolButton;
    SeparatorHelp: TToolButton;
    AppToolBar: TToolBar;
    BarPopupMenu: TPopupMenu;
    procedure HelpKeywordUpdate(Sender: TObject);
    procedure HelpKeywordExecute(Sender: TObject);
    procedure AboutUpdate(Sender: TObject);
    procedure AboutExecute(Sender: TObject);
    procedure ApplicationEventsHint(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BarPopupMenuPopup(Sender: TObject);
  private
    FStartTimer: TTimer;
    procedure StartInitApplication(Sender: TObject);
    procedure InitCoolPopupMenu;
    procedure OnBarPropupClick(Sender: TObject);
  protected
    FCloseAppQuery: Boolean;
    procedure InitForm; override;
    procedure DoneForm; override;
    procedure InitApplication; virtual;
    procedure InitExceptionsHandler; virtual;
    procedure DoneApplication; dynamic;
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    procedure LoadFormControls; override;
    procedure SaveFormControls; override;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  RVclUtils, RFrmStorage, RDialogs, RMsgRu, RExHandlers,
  RExHandlersSb, RExHandlersExDlg, AboutForm;

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TNrmMainTemplate.SetStyle;
begin
  // inherited;
  Ctl3D := ApplicationStyle.DataForm.Ctl3D;
  FontDataToFont(ApplicationStyle.DataForm.FormFont, Font);
  CoolBar.Visible := ApplicationStyle.MainForm.TbVisible;
  StatusBar.Visible := ApplicationStyle.MainForm.SbVisible;
end;
{$ENDIF}

{ == Инициализация формы ======================================================= }
procedure TNrmMainTemplate.InitForm;
begin
  FCloseAppQuery := False;
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
  CCloseAppQuery = 'CloseAppQuery';

procedure TNrmMainTemplate.LoadFormControls;
begin
  inherited;
  LoadCoolBar(Self, CoolBar);
  FCloseAppQuery := LoadFormBoolean(Self, CCloseAppQuery, FCloseAppQuery);
end;

procedure TNrmMainTemplate.SaveFormControls;
begin
  inherited;
  SaveCoolBar(Self, CoolBar);
  SaveFormBoolean(Self, CCloseAppQuery, FCloseAppQuery);
end;

{ == Инициализация меню панели ================================================= }
procedure TNrmMainTemplate.InitCoolPopupMenu;
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

procedure TNrmMainTemplate.BarPopupMenuPopup(Sender: TObject);
begin
  InitCoolPopupMenu;
end;

procedure TNrmMainTemplate.OnBarPropupClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    CoolBar.Bands[TMenuItem(Sender).Tag].Visible := not CoolBar.Bands[TMenuItem(Sender).Tag].Visible;
end;

{ == Инициализация приложения ================================================== }
procedure TNrmMainTemplate.StartInitApplication(Sender: TObject);
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

procedure TNrmMainTemplate.InitApplication;
begin
  // InitCoolPopupMenu;
end;

{ == Инициализация обработчика ошибок приложения =============================== }
procedure TNrmMainTemplate.InitExceptionsHandler;
begin
  AppExceptionsHandler := TExceptHandler.Create;
  Application.OnException := AppExceptionsHandler.AppExceptHandler;
  AppExceptionsHandler.ChannelCreate(TStatusExceptChannel, 1, True);
  AppExceptionsHandler.ChannelCreate(TExDlgExceptChannel, 200, True);
end;

{ == Завершение работы приложения ============================================== }
procedure TNrmMainTemplate.DoneForm;
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

procedure TNrmMainTemplate.DoneApplication;
begin
end;

{ == Отображение подсказки в строке статуса ==================================== }
procedure TNrmMainTemplate.ApplicationEventsHint(Sender: TObject);
begin
  if IsNotWait then ShowInStatusBar(Application.Hint);
end;

{ == Вызов справочной информации =============================================== }
procedure TNrmMainTemplate.HelpKeywordUpdate(Sender: TObject);
begin
  HelpKeyword.Enabled := IsNotWait and FileExists(Application.CurrentHelpFile);
end;

procedure TNrmMainTemplate.HelpKeywordExecute(Sender: TObject);
begin
  Application.HelpCommand(HELP_FINDER, 0);
end;

{ == Информация о программе ==================================================== }
procedure TNrmMainTemplate.AboutUpdate(Sender: TObject);
begin
  About.Enabled := IsNotWait;
end;

procedure TNrmMainTemplate.AboutExecute(Sender: TObject);
begin
  ShowAbout;
end;

{ == Запрос о завершении работы ================================================ }
procedure TNrmMainTemplate.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := IsNotWait and (not FCloseAppQuery or CloseAppQuery);
end;

end.
