unit TmplData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplStorage, ToolWin, ComCtrls, Menus, ActnList, ImgList;

type
  TDataTemplate = class(TStorageTemplate)
    ActionList: TActionList;
    PopupMenu: TPopupMenu;
    MainMenu: TMainMenu;
    CoolBar: TCoolBar;
    ToolBar: TToolBar;
    StatusBar: TStatusBar;
    CloseSelect: TAction;
    CloseCancel: TAction;
    itemCloseSelectP: TMenuItem;
    itemCloseCancelP: TMenuItem;
    menuHelp: TMenuItem;
    AboutBox: TAction;
    ShowHelp: TAction;
    divPopupRefresh: TMenuItem;
    itemHelpContext: TMenuItem;
    divHelp1: TMenuItem;
    itemAboutBox: TMenuItem;
    menuEdit: TMenuItem;
    menuData: TMenuItem;
    menuReports: TMenuItem;
    menuOperations: TMenuItem;
    Refresh: TAction;
    itemRefresh: TMenuItem;
    itemRefreshP: TMenuItem;
    divPopupEnd: TMenuItem;
    divPopupSubmenus: TMenuItem;
    menuDataP: TMenuItem;
    menuOperationsP: TMenuItem;
    menuReportsP: TMenuItem;
    Find: TAction;
    itemFind: TMenuItem;
    itemFindP: TMenuItem;
    DataPopupMenu: TPopupMenu;
    OperationsPopupMenu: TPopupMenu;
    ReportsPopupMenu: TPopupMenu;
    procedure CloseSelectUpdate(Sender: TObject);
    procedure CloseSelectExecute(Sender: TObject);
    procedure CloseCancelUpdate(Sender: TObject);
    procedure CloseCancelExecute(Sender: TObject);
    procedure ShowHelpUpdate(Sender: TObject);
    procedure ShowHelpExecute(Sender: TObject);
    procedure AboutBoxUpdate(Sender: TObject);
    procedure AboutBoxExecute(Sender: TObject);
    procedure RefreshUpdate(Sender: TObject);
    procedure RefreshExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    procedure InitForm; override;
    procedure DoneForm; override;
    procedure FreeForm; override;
    procedure StartForm; override;
    {$IFDEF ATTACH}
    procedure LoadAttachments; virtual;
    {$ENDIF}
    function  BeforeLoadData: Boolean; virtual;
    function  LoadDataForm: Boolean; virtual;
    procedure LoadDataTry; virtual;
    procedure LoadDataFinally; virtual;
    function  AfterLoadData: Boolean; virtual;
    procedure InitDataComponents; virtual;
    procedure DoneDataComponents; virtual;
    procedure CloseDataSets; virtual;
    {$IFDEF RSS}
    function  CheckTag(const Tag: Integer): Integer;
    function  GetViewTag: Integer; virtual;
    procedure LogOnOpen; virtual;
    procedure LogOnClose; virtual;
    {$ENDIF}
    function  SelectVisible: Boolean; virtual;
    function  SelectEnabled: Boolean; virtual;
    procedure SetSelectionBefore; virtual;
    procedure SetSelection(const SelectedID: Integer); virtual;
    procedure SetSelectionAfter; virtual;
    function  GetSelection: Integer; virtual;
    procedure SetSelections(const GroupId, SelectedID: Integer); virtual;
    function  GetSelections(out GroupId, SelectedID: Integer): Boolean; virtual;
  public
    FormData: Pointer;
    SelectMode: Boolean;
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    procedure LoadFormControls; override;
    procedure SaveFormControls; override;
    function  LoadData: Boolean;
    procedure ShowItemsCount; virtual;
    procedure SetSelectedValue(const SelectedID: Integer);
    function  GetSelectedValue: Integer;
    procedure SetSelectedValues(const GroupId, SelectedID: Integer);
    function  GetSelectedValues(out GroupId, SelectedID: Integer): Boolean;
  end;

  TDataFormClass = class of TDataTemplate;

implementation

uses
  RVclUtils, RMsgRu, RFrmStorage, RDialogs, RExHandlers, RSysUtils,
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  {$IFDEF RSS} RDbLog, RRssConst, {$ENDIF}
  BaseDbUnit, AboutForm;

{$R *.dfm}

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TDataTemplate.SetStyle;
begin
  inherited;
  CoolBar.Visible := ApplicationStyle.DataForm.TbVisible;
  ToolBar.ShowCaptions := ApplicationStyle.DataForm.TbCaptions;
  ToolBar.ButtonHeight := 1;
  ToolBar.ButtonWidth := 1;
  StatusBar.Visible := ApplicationStyle.DataForm.SbVisible;
end;
{$ENDIF}

{ == Инициализация формы ======================================================= }
procedure TDataTemplate.FormCreate(Sender: TObject);
begin
  FormData := nil;
  inherited;
end;

procedure TDataTemplate.InitForm;
begin
  ShowInStatusBar(SMsgInitDataForm);
  try
    SelectMode := True;
    try
      try
        inherited;
      finally
        InitDataComponents;
      end;
    except
      on E: Exception do
        HandleExcept(E, Self, SErrInitForm);
    end;
  finally
    ShowInStatusBar(EmptyStr);
  end;
end;

{ == Первый запуск формы ======================================================= }
procedure TDataTemplate.StartForm;
begin
  try
    inherited;
  finally
    {$IFDEF RSS}
    LogOnOpen;
    {$ENDIF}
  end;
end;

procedure TDataTemplate.FormShow(Sender: TObject);
begin
  try
    inherited;
  finally
    CloseSelectUpdate(Sender);
  end;
end;

{ == Закрытие формы ============================================================ }
procedure TDataTemplate.DoneForm;
begin
  ShowInStatusBar(SMsgSaveDataForm);
  try
    try
      try
        {$IFDEF RSS}
        LogOnClose;
        {$ENDIF}
      finally
        inherited;
      end;
    except
      on E: Exception do
        HandleExcept(E, Self, SErrDoneForm);
    end;
  finally
    ShowInStatusBar(EmptyStr);
  end;
end;

procedure TDataTemplate.FreeForm;
begin
  ShowInStatusBar(SMsgSaveDataForm);
  try
    try
      FormData := nil;
      try
        try
          CloseDataSets;
        finally
          DoneDataComponents;
        end;
      except
        on E: Exception do
          HandleExcept(E, Self, SErrDoneForm);
      end;
    finally
      inherited;
    end;
  finally
    ShowInStatusBar(EmptyStr);
  end;
end;

{ == Инициализация Data-компонентов ============================================ }
procedure TDataTemplate.InitDataComponents;
begin
end;

{ == Деактивация Data-компонентов ============================================== }
procedure TDataTemplate.DoneDataComponents;
begin
end;

{ == Закрываем незакрытые данные =============================================== }
procedure TDataTemplate.CloseDataSets;
begin
end;

{ == Сохранение и восстановление размеров формы ================================ }
procedure TDataTemplate.LoadFormControls;
begin
  inherited;
  LoadCoolBar(Self, CoolBar);
end;

procedure TDataTemplate.SaveFormControls;
begin
  inherited;
  SaveCoolBar(Self, CoolBar);
end;

{ == Отображение количества записей ============================================ }
procedure TDataTemplate.ShowItemsCount;
begin
end;

{ == Загрузка данных =========================================================== }
{$IFDEF ATTACH}
procedure TDataTemplate.LoadAttachments;
begin
end;
{$ENDIF}

function TDataTemplate.BeforeLoadData: Boolean;
begin
  Result := True;
end;

procedure TDataTemplate.LoadDataTry;
begin
end;

procedure TDataTemplate.LoadDataFinally;
begin
end;

function TDataTemplate.LoadDataForm: Boolean;
begin
  Result := True;
end;

function TDataTemplate.AfterLoadData: Boolean;
begin
  Result := True;
end;

function TDataTemplate.LoadData: Boolean;
begin
  StartWait;
  ShowInStatusBar(SMsgLoadDataWait);
  Result := False;
  try
    FormShow(nil);
    try
      Result := BeforeLoadData;
      if Result then
      begin
        LoadDataTry;
        try
          {$IFDEF ATTACH}
          LoadAttachments;
          {$ENDIF}
          Result := LoadDataForm;
        finally
          LoadDataFinally;
        end;
        if Result then AfterLoadData;
      end;
    except
      on E: Exception do
      begin
        Result := False;
        HandleExcept(E, Self, sErrLoadData);
      end;
    end;
  finally
    ShowItemsCount;
    if Result then ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Выбрать запись и закрыть окно ============================================= }
function TDataTemplate.SelectVisible: Boolean;
begin
  Result := SelectMode and (fsModal in FormState);
end;

function TDataTemplate.SelectEnabled: Boolean;
begin
  Result := False;
end;

procedure TDataTemplate.CloseSelectUpdate(Sender: TObject);
begin
  CloseSelect.Visible := SelectVisible;
  CloseSelect.Enabled := IsNotWait and SelectVisible and SelectEnabled;
end;

procedure TDataTemplate.CloseSelectExecute(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

{ == Ввод и выборка выбранного значения из формы =============================== }
procedure TDataTemplate.SetSelectionBefore;
begin
end;

procedure TDataTemplate.SetSelection(const SelectedID: Integer);
begin
end;

procedure TDataTemplate.SetSelectionAfter;
begin
end;

procedure TDataTemplate.SetSelectedValue(const SelectedID: Integer);
begin
  StartWait;
  ShowInStatusBar(SMsgGetSelection);
  try
    SetSelection(SelectedID);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

function TDataTemplate.GetSelection: Integer;
begin
  Result := intDisable;
end;

function TDataTemplate.GetSelectedValue: Integer;
begin
  StartWait;
  ShowInStatusBar(SMsgGetSelection);
  try
    Result := GetSelection;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TDataTemplate.SetSelections(const GroupId, SelectedID: Integer);
begin
  SetSelectedValue(SelectedID);
end;

procedure TDataTemplate.SetSelectedValues(const GroupId, SelectedID: Integer);
begin
  StartWait;
  ShowInStatusBar(SMsgSetSelection);
  try
    SetSelections(GroupId, SelectedID);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

function TDataTemplate.GetSelections(out GroupId, SelectedID: Integer): Boolean;
begin
  GroupId := intDisable;
  SelectedID := GetSelectedValue;
  Result := GetSelectedValue > intDisable;
end;

function TDataTemplate.GetSelectedValues(out GroupId, SelectedID: Integer): Boolean;
begin
  StartWait;
  ShowInStatusBar(SMsgGetSelection);
  try
    Result := GetSelections(GroupId, SelectedID);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Закрыть окно ============================================================== }
procedure TDataTemplate.CloseCancelUpdate(Sender: TObject);
begin
  CloseCancel.Enabled := IsNotWait;
end;

procedure TDataTemplate.CloseCancelExecute(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

{ == Вызов справочной информации =============================================== }
procedure TDataTemplate.ShowHelpUpdate(Sender: TObject);
begin
  ShowHelp.Enabled := IsNotWait
    and (((HelpFile <> EmptyStr) and FileExists(HelpFile))
      or FileExists(Application.HelpFile));
end;

procedure TDataTemplate.ShowHelpExecute(Sender: TObject);
begin
  if FileExists(HelpFile)
  then OpenHelp(HelpFile, HelpKeyword)
  else OpenHelp(Application.HelpFile, HelpKeyword);
end;

{ == Информация о программе ==================================================== }
procedure TDataTemplate.AboutBoxUpdate(Sender: TObject);
begin
  AboutBox.Visible := (FormStyle = fsMDIChild) or MainMenu.AutoMerge;
  divHelp1.Visible := AboutBox.Visible;
  AboutBox.Enabled := IsNotWait;
end;

procedure TDataTemplate.AboutBoxExecute(Sender: TObject);
begin
  ShowAbout;
end;

{$IFDEF RSS}
{ == Проверка тегов протокола ================================================== }
function TDataTemplate.CheckTag(const Tag: Integer): Integer;
begin
  if Tag > 0
  then Result := Tag
  else begin
    ErrorBox(Format(EErrBadOperationTag, [Tag]));
    Result := tagError;
  end;
end;

{ == Генерация тегов протокола ================================================= }
function TDataTemplate.GetViewTag: Integer;
begin
  Result := CheckTag(Tag);
end;

{ == Протоколирование событий ================================================== }
procedure TDataTemplate.LogOnOpen;
begin
  AddToDbLog(GetViewTag, Format(SLogOpenWindow, [Caption]));
end;

procedure TDataTemplate.LogOnClose;
begin
  AddToDbLog(GetViewTag, Format(SLogCloseWindow, [Caption]));
end;
{$ENDIF}

{ == Обновить данные =========================================================== }
procedure TDataTemplate.RefreshUpdate(Sender: TObject);
begin
  Refresh.Enabled := IsNotWait;
end;

procedure TDataTemplate.RefreshExecute(Sender: TObject);
begin
  LoadData;
end;

end.
