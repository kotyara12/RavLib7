unit TmplChild;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplStorage, Menus, ActnList, ComCtrls, ToolWin, ImgList;

type
  TChildTemplate = class(TStorageTemplate)
    ActionList: TActionList;
    CloseSelect: TAction;
    CloseCancel: TAction;
    MainMenu: TMainMenu;
    CoolBar: TCoolBar;
    ToolBar: TToolBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseSelectUpdate(Sender: TObject);
    procedure CloseSelectExecute(Sender: TObject);
    procedure CloseCancelUpdate(Sender: TObject);
    procedure CloseCancelExecute(Sender: TObject);
  protected
    procedure InitForm; override;
    procedure DoneForm; override;
    procedure FreeForm; override;
    function  BeforeLoadData: Boolean; virtual;
    function  LoadDataForm: Boolean; virtual;
    function  AfterLoadData: Boolean; virtual;
    procedure InitDataComponents; virtual;
    procedure DoneDataComponents; virtual;
    procedure CloseDataSets; virtual;
    function  SelectVisible: Boolean; virtual;
    function  SelectEnabled: Boolean; virtual;
    procedure SetSelection(const SelectedID: Integer); virtual;
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


implementation

{$R *.dfm}

uses
  RVclUtils, RMsgRu, RFrmStorage, RDialogs, RExHandlers,
  {$IFDEF STYLES} RAppStyles, RFonts; {$ENDIF}

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TChildTemplate.SetStyle;
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
procedure TChildTemplate.FormCreate(Sender: TObject);
begin
  FormData := nil;
  inherited;
end;

procedure TChildTemplate.InitForm;
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

{ == Отображение формы ========================================================= }
procedure TChildTemplate.FormShow(Sender: TObject);
begin
  try
    inherited;
  finally
    CloseSelectUpdate(Sender);
  end;
end;

{ == Закрытие формы ============================================================ }
procedure TChildTemplate.DoneForm;
begin
  ShowInStatusBar(SMsgSaveDataForm);
  try
    try
      try
        DoneDataComponents;
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

procedure TChildTemplate.FreeForm;
begin
  try
    try
      CloseDataSets;
    except
      on E: Exception do
        HandleExcept(E, Self, SErrDoneForm);
    end;
  finally
    inherited;
  end;
end;

{ == Инициализация Data-компонентов ============================================ }
procedure TChildTemplate.InitDataComponents;
begin
end;

{ == Деактивация Data-компонентов ============================================== }
procedure TChildTemplate.DoneDataComponents;
begin
end;

{ == Закрываем незакрытые данные =============================================== }
procedure TChildTemplate.CloseDataSets;
begin
end;

{ == Сохранение и восстановление размеров формы ================================ }
procedure TChildTemplate.LoadFormControls;
begin
  inherited;
  LoadCoolBar(Self, CoolBar);
end;

procedure TChildTemplate.SaveFormControls;
begin
  inherited;
  SaveCoolBar(Self, CoolBar);
end;

{ == Отображение количества записей ============================================ }
procedure TChildTemplate.ShowItemsCount;
begin
end;

{ == Загрузка данных =========================================================== }
function TChildTemplate.BeforeLoadData: Boolean;
begin
  Result := True;
end;

procedure TChildTemplate.LoadDataTry;
begin
end;

procedure TChildTemplate.LoadDataFinally;
begin
end;

function TChildTemplate.LoadDataForm: Boolean;
begin
  Result := True;
end;

function TChildTemplate.AfterLoadData: Boolean;
begin
  Result := True;
end;

function TChildTemplate.LoadData: Boolean;
begin
  StartWait;
  ShowInStatusBar(SMsgLoadDataWait);
  Result := False;
  try
    FormShow(nil);
    Result := BeforeLoadData;
    Result := Result and LoadDataForm;
    Result := Result and AfterLoadData;
  finally
    ShowItemsCount;
    if Result then ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Выбрать запись и закрыть окно ============================================= }
function TChildTemplate.SelectVisible: Boolean;
begin
  Result := SelectMode and (fsModal in FormState);
end;

function TChildTemplate.SelectEnabled: Boolean;
begin
  Result := False;
end;

procedure TChildTemplate.CloseSelectUpdate(Sender: TObject);
begin
  CloseSelect.Visible := SelectVisible;
  CloseSelect.Enabled := IsNotWait and SelectVisible and SelectEnabled;
end;

procedure TChildTemplate.CloseSelectExecute(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

{ == Ввод и выборка выбранного значения из формы =============================== }
procedure TChildTemplate.SetSelection(const SelectedID: Integer);
begin
end;

procedure TChildTemplate.SetSelectedValue(const SelectedID: Integer);
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

function TChildTemplate.GetSelection: Integer;
begin
  Result := intDisable;
end;

function TChildTemplate.GetSelectedValue: Integer;
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

procedure TChildTemplate.SetSelections(const GroupId, SelectedID: Integer);
begin
  SetSelectedValue(SelectedID);
end;

procedure TChildTemplate.SetSelectedValues(const GroupId, SelectedID: Integer);
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

function TChildTemplate.GetSelections(out GroupId, SelectedID: Integer): Boolean;
begin
  GroupId := intDisable;
  SelectedID := GetSelectedValue;
  Result := GetSelectedValue > intDisable;
end;

function TChildTemplate.GetSelectedValues(out GroupId, SelectedID: Integer): Boolean;
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
procedure TChildTemplate.CloseCancelUpdate(Sender: TObject);
begin
  CloseCancel.Enabled := IsNotWait;
end;

procedure TChildTemplate.CloseCancelExecute(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

end.
