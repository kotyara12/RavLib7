unit TmplListSimple;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplStorage, ComCtrls, ToolWin, RavListView, Menus, ImgList,
  ActnList, StdCtrls, Buttons, ExtCtrls;

type
  TSimpleListTemplate = class(TStorageTemplate)
    ListView: TRSortListView;
    CoolBar: TCoolBar;
    ToolBar: TToolBar;
    StatusBar: TStatusBar;
    ActionList: TActionList;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    PopupMenu: TPopupMenu;
    menuEdit: TMenuItem;
    menuData: TMenuItem;
    menuPrint: TMenuItem;
    DataPopupMenu: TPopupMenu;
    PrintPopupMenu: TPopupMenu;
    Find: TAction;
    itemFind: TMenuItem;
    itemFindP: TMenuItem;
    divData1: TMenuItem;
    menuSortList: TMenuItem;
    menuSortListD: TMenuItem;
    divPopupData: TMenuItem;
    menuDataP: TMenuItem;
    menuSortListP: TMenuItem;
    ListSortAsc: TAction;
    ListSortDesc: TAction;
    divSortList: TMenuItem;
    itemListSortAsc: TMenuItem;
    itemListSortDesc: TMenuItem;
    divSortListD: TMenuItem;
    itemListSortAscD: TMenuItem;
    itemListSortDescD: TMenuItem;
    divSortListP: TMenuItem;
    itemListSortAscP: TMenuItem;
    itemListSortDescP: TMenuItem;
    ListGrid: TAction;
    divPopupExt: TMenuItem;
    itemListGridP: TMenuItem;
    MultiSelectOnOff: TAction;
    itemMultiSelectOnOff: TMenuItem;
    itemMultiSelectOnOffP: TMenuItem;
    CloseCancel: TAction;
    divPopupClose: TMenuItem;
    itemCloseCancelP: TMenuItem;
    Refresh: TAction;
    divEdit1: TMenuItem;
    itemRefresh: TMenuItem;
    itemRefreshP: TMenuItem;
    divPopupRefresh: TMenuItem;
    ShowHelp: TAction;
    menuHelp: TMenuItem;
    itemShowHelp: TMenuItem;
    ListExportToExcel: TAction;
    ListExportToFile: TAction;
    ListCreateDynamicReport: TAction;
    itemListExportToExcel: TMenuItem;
    itemListExportToFile: TMenuItem;
    itemListCreateDynamicReport: TMenuItem;
    itemListCreateDynamicReportR: TMenuItem;
    itemListExportToExcelR: TMenuItem;
    itemListExportToFileR: TMenuItem;
    menuPrintP: TMenuItem;
    itemListCreateDynamicReportP: TMenuItem;
    itemListExportToExcelP: TMenuItem;
    itemListExportToFileP: TMenuItem;
    FindPanel: TPanel;
    edFastFind: TEdit;
    btnFastFind: TBitBtn;
    FindFast: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FindUpdate(Sender: TObject);
    procedure FindExecute(Sender: TObject);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListSortAscUpdate(Sender: TObject);
    procedure ListSortAscExecute(Sender: TObject);
    procedure ListSortDescUpdate(Sender: TObject);
    procedure ListSortDescExecute(Sender: TObject);
    procedure ListGridUpdate(Sender: TObject);
    procedure ListGridExecute(Sender: TObject);
    procedure MultiSelectOnOffUpdate(Sender: TObject);
    procedure MultiSelectOnOffExecute(Sender: TObject);
    procedure CloseCancelUpdate(Sender: TObject);
    procedure CloseCancelExecute(Sender: TObject);
    procedure RefreshUpdate(Sender: TObject);
    procedure RefreshExecute(Sender: TObject);
    procedure ShowHelpUpdate(Sender: TObject);
    procedure ShowHelpExecute(Sender: TObject);
    procedure ListExportToExcelUpdate(Sender: TObject);
    procedure ListExportToExcelExecute(Sender: TObject);
    procedure ListExportToFileUpdate(Sender: TObject);
    procedure ListExportToFileExecute(Sender: TObject);
    procedure ListCreateDynamicReportUpdate(Sender: TObject);
    procedure ListCreateDynamicReportExecute(Sender: TObject);
    procedure FindPanelResize(Sender: TObject);
    procedure FindFastUpdate(Sender: TObject);
    procedure FindFastExecute(Sender: TObject);
    procedure edFastFindKeyPress(Sender: TObject; var Key: Char);
  private
    fLastIndex: Integer;
    fShowWaitOnLoad: Boolean;
    procedure CreateSortMenus;
    procedure UpdateSortMenus;
    procedure ChangeSortColumn(Sender: TObject);
  protected
    procedure InitForm; override;
    procedure StartForm; override;
    procedure DoneForm; override;
    procedure FreeForm; override;
    procedure InitFormVariables; override;
    function  GetDetailExtName: string; virtual;
    function  GetDetailName: string; virtual;
    function  GetExcelPageOrientation: Integer; virtual;
    function  GetReportComment: string; virtual;
    {$IFDEF RSS}
    procedure LogOnOpen; dynamic;
    procedure LogOnClose; dynamic;
    procedure LogOnListViewExportExcel; virtual;
    procedure LogOnListViewExportFile(const FileName: string; const RecordCount: Integer); virtual;
    {$ENDIF}
    function  BeforeLoadData: Boolean; virtual;
    function  LoadDataForm: Boolean; virtual;
    procedure LoadDataTry; virtual;
    procedure LoadDataFinally; virtual;
    function  AfterLoadData: Boolean; virtual;
    procedure CloseDataSets; dynamic;
  public
    FormData: Pointer;
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    function  LoadData: Boolean;
    procedure LoadFormControls; override;
    procedure SaveFormControls; override;
    procedure ShowItemsCount; virtual;
    property  ShowWaitOnLoad: Boolean read fShowWaitOnLoad write fShowWaitOnLoad;
  end;

  TSimpleListClass = class of TSimpleListTemplate;

procedure ShowSimpleListForm(Form: TSimpleListClass; const Data: Pointer;
  const FormCaption: string; const ShowWaitOnLoad: Boolean);

implementation

{$R *.dfm}

uses
  RVclUtils, RExHandlers, RFrmStorage, RWait, RMsgRu,
  RDialogs, RFind, RListView, RFileExport,
  {$IFDEF RSS} RDbLog, RRssConst, {$ENDIF}
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  RxVerInf, RDbPrint, RExpExcel;

procedure ShowSimpleListForm(Form: TSimpleListClass; const Data: Pointer;
  const FormCaption: string; const ShowWaitOnLoad: Boolean);
begin
  with Form.Create(Application) do
  begin
    try
      FormData := Data;
      if FormCaption <> EmptyStr then Caption := FormCaption;
      fShowWaitOnLoad := ShowWaitOnLoad;
      if LoadData then
        ShowModal;
    finally
      Free;
    end;
  end;
end;

{ TSimpleListTemplate }

{ == Инициализация формы ======================================================= }

procedure TSimpleListTemplate.FormCreate(Sender: TObject);
begin
  fLastIndex := intDisable;
  FormData := nil;
  inherited;
end;

procedure TSimpleListTemplate.InitForm;
begin
  ShowInStatusBar(SMsgInitDataForm);
  try
    try
      inherited;
    except
      on E: Exception do
        HandleExcept(E, Self, SErrInitForm);
    end;
  finally
    ShowInStatusBar(EmptyStr);
  end;
end;

procedure TSimpleListTemplate.StartForm;
begin
  try
    try
      inherited;
    finally
      CreateSortMenus;
      UpdateSortMenus;
    end;
  finally
    {$IFDEF RSS}
    LogOnOpen;
    {$ENDIF}
  end;
end;

procedure TSimpleListTemplate.DoneForm;
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

procedure TSimpleListTemplate.CloseDataSets;
begin
end;

procedure TSimpleListTemplate.FreeForm;
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
    FormData := nil;
  end;
end;

procedure TSimpleListTemplate.InitFormVariables;
begin
  inherited;

  FindPanel.Height := edFastFind.Height + 10;
end;

procedure TSimpleListTemplate.FindPanelResize(Sender: TObject);
begin
  edFastFind.Width := FindPanel.ClientWidth - btnFastFind.Width - 10;

  btnFastFind.Left := FindPanel.ClientWidth - btnFastFind.Width - 4;
  btnFastFind.Height := edFastFind.Height;
end;

{ == Визуальные параметры формы ================================================ }

{$IFDEF STYLES}
procedure TSimpleListTemplate.SetStyle;
begin
  inherited SetStyle;
  ListView.Color := ApplicationStyle.DataForm.DataColor;
  FontDataToFont(ApplicationStyle.DataForm.DataFont, ListView.Font);
  FindPanel.Visible := ApplicationStyle.DataForm.FastFindPanel;
end;
{$ENDIF}

procedure TSimpleListTemplate.LoadFormControls;
begin
  inherited;
  LoadListColumns(Self, ListView);
end;

procedure TSimpleListTemplate.SaveFormControls;
begin
  inherited;
  SaveListColumns(Self, ListView);
end;

{ == Работа с журналом аудита ================================================== }

function TSimpleListTemplate.GetDetailName: string;
begin
  Result := ListView.Name;
end;

function TSimpleListTemplate.GetDetailExtName: string;
begin
  Result := Caption;
end;

{$IFDEF RSS}
procedure TSimpleListTemplate.LogOnOpen;
begin
  if Tag > 0 then
    AddToDbLog(Tag, Format(SLogOpenWindow, [Caption]));
end;

procedure TSimpleListTemplate.LogOnClose;
begin
  if Tag > 0 then
    AddToDbLog(Tag, Format(SLogCloseWindow, [Caption]));
end;

procedure TSimpleListTemplate.LogOnListViewExportExcel;
begin
  if Tag > 0 then
    AddToDbLog(Tag, Format(SLogExportExcel, [GetDetailExtName, GetDetailName]));
end;

procedure TSimpleListTemplate.LogOnListViewExportFile(const FileName: string; const RecordCount: Integer);
begin
  if Tag > 0 then
    AddToDbLog(Tag, Format(SLogExportFile, [GetDetailExtName, GetDetailName, FileName, RecordCount]));
end;
{$ENDIF}

{ == Загрузка данных =========================================================== }

function TSimpleListTemplate.BeforeLoadData: Boolean;
begin
  Result := True;
end;

procedure TSimpleListTemplate.LoadDataTry;
begin
  ListView.Items.BeginUpdate;
  if Assigned(ListView.Selected)
  then fLastIndex := ListView.Selected.Index
  else fLastIndex := intDisable;
end;

function TSimpleListTemplate.LoadDataForm: Boolean;
begin
  Result := True;
end;

procedure TSimpleListTemplate.LoadDataFinally;
begin
  if (fLastIndex > intDisable) and (fLastIndex < ListView.Items.Count)
  then ListView.Selected := ListView.Items[fLastIndex];
  ListView.Items.EndUpdate;
  ScrollToSelectedItem(ListView);
end;

function TSimpleListTemplate.AfterLoadData: Boolean;
begin
  Result := True;
end;

function TSimpleListTemplate.LoadData: Boolean;
begin
  StartWait;
  ShowInStatusBar(SMsgLoadDataWait);
  if fShowWaitOnLoad then ShowWaitMsg(SMsgLoadDataWait);
  Result := False;
  try
    FormShow(nil);
    Result := BeforeLoadData;
    if Result then
    begin
      LoadDataTry;
      try
        Result := LoadDataForm;
      finally
        LoadDataFinally;
      end;
      if Result then AfterLoadData;
    end;
  finally
    if fShowWaitOnLoad then CloseWaitMsg;
    if Result then ShowInStatusBar(EmptyStr);
    ShowItemsCount;
    StopWait;
  end;
end;

procedure TSimpleListTemplate.ShowItemsCount;
begin
  StatusBar.SimpleText := Format(SItemsCount, [ListView.Items.Count]);
end;

{ == Обновить данные =========================================================== }
procedure TSimpleListTemplate.RefreshUpdate(Sender: TObject);
begin
  Refresh.Enabled := IsNotWait;
end;

procedure TSimpleListTemplate.RefreshExecute(Sender: TObject);
begin
  LoadData;
end;

{ == Закрыть окно ============================================================== }

procedure TSimpleListTemplate.CloseCancelUpdate(Sender: TObject);
begin
  CloseCancel.Enabled := IsNotWait;
end;

procedure TSimpleListTemplate.CloseCancelExecute(Sender: TObject);
begin
  Close;
end;

{ == Справка =================================================================== }

procedure TSimpleListTemplate.ShowHelpUpdate(Sender: TObject);
begin
  ShowHelp.Enabled := IsNotWait
    and (((HelpFile <> EmptyStr) and FileExists(HelpFile))
      or FileExists(Application.HelpFile))
    and (((HelpType = htKeyword) and (HelpKeyword <> EmptyStr))
      or ((HelpType = htContext) and (HelpContext <> 0)));
end;

procedure TSimpleListTemplate.ShowHelpExecute(Sender: TObject);
begin
  if HelpType = htKeyword
  then Application.HelpKeyword(HelpKeyword)
  else Application.HelpContext(HelpContext);
end;

{ == Поиск ===================================================================== }
procedure TSimpleListTemplate.FindUpdate(Sender: TObject);
begin
  Find.Enabled := IsNotWait and (ListView.Items.Count > 0);
end;

procedure TSimpleListTemplate.FindExecute(Sender: TObject);
begin
  FindInList(ListView);
end;

{ == Быстрый поиск ============================================================= }
procedure TSimpleListTemplate.FindFastUpdate(Sender: TObject);
begin
  FindFast.Enabled := IsNotWait and (ListView.Items.Count > 0) and (edFastFind.Text <> '');
end;

procedure TSimpleListTemplate.FindFastExecute(Sender: TObject);
begin
  ListView.Selected := LV_FindTextPart(ListView, edFastFind.Text);

  if Assigned(ListView.Selected) then
  begin
    ScrollToSelectedItem(ListView);
    ListView.SetFocus;
  end
  else ErrorBox(Format(SErrStrNotFound, [edFastFind.Text]));
end;

procedure TSimpleListTemplate.edFastFindKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    if (ListView.Items.Count > 0) and (edFastFind.Text <> '') then
      FindFastExecute(Sender);
  end;

  if Key = #27 then
  begin
    Key := #0;
    edFastFind.Clear;
  end;
end;

{ == Сортировка ================================================================ }

procedure TSimpleListTemplate.CreateSortMenus;

  procedure CreateSortMenu(Menu: TMenuItem);
  var
    i: Integer;
    SortItem: TMenuItem;
  begin
    for i := ListView.Columns.Count - 1 downto 0 do
    begin
      SortItem := TMenuItem.Create(Menu);
      SortItem.Name := Menu.Name + IntToStr(i);
      SortItem.OnClick := ChangeSortColumn;
      SortItem.Caption := Format(SSortItemName, [ListView.Columns[i].Caption]);
      SortItem.Hint := Format(SSortItemHint, [ListView.Columns[i].Caption]);
      SortItem.ShortCut := ShortCut(Word(IntToStr(i)[1]), [ssCtrl]);
      SortItem.Tag := i;
      Menu.Insert(0, SortItem);
    end;
    SortItem := TMenuItem.Create(Menu);
    SortItem.Name := Menu.Name + IntToStr(ListView.Columns.Count);
    SortItem.OnClick := ChangeSortColumn;
    SortItem.Caption := SSortDisableName;
    SortItem.Hint := SSortDisableHint;
    SortItem.Tag := intDisable;
    Menu.Insert(0, SortItem);
  end;

begin
  CreateSortMenu(menuSortList);
  CreateSortMenu(menuSortListP);
  CreateSortMenu(menuSortListD);
end;

procedure TSimpleListTemplate.UpdateSortMenus;

  procedure UpdateSortMenu(Menu: TMenuItem);
  var
    i: Integer;
  begin
    for i := 0 to Menu.Count - 1 do
      if Pos(Menu.Name, Menu[i].Name) = 1 then
        Menu[i].Checked := Menu[i].Tag = ListView.SortColumn;
  end;

begin
  UpdateSortMenu(menuSortList);
  UpdateSortMenu(menuSortListP);
  UpdateSortMenu(menuSortListD);
end;

procedure TSimpleListTemplate.ChangeSortColumn(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    StartWait;
    ShowInStatusBar(SMsgSortData);
    try
      ListView.SortColumn := TMenuItem(Sender).Tag;
      UpdateSortMenus;
    finally
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  end;
end;

procedure TSimpleListTemplate.ListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  UpdateSortMenus;
end;

procedure TSimpleListTemplate.ListSortAscUpdate(Sender: TObject);
begin
  ListSortAsc.Checked := ListView.SortDirection = sdAscending;
  ListSortAsc.Enabled := IsNotWait;
end;

procedure TSimpleListTemplate.ListSortAscExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    ListView.SortDirection := sdAscending;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TSimpleListTemplate.ListSortDescUpdate(Sender: TObject);
begin
  ListSortDesc.Checked := ListView.SortDirection = sdDescending;
  ListSortDesc.Enabled := IsNotWait;
end;

procedure TSimpleListTemplate.ListSortDescExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    ListView.SortDirection := sdDescending;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Отображение сетки ========================================================= }

procedure TSimpleListTemplate.ListGridUpdate(Sender: TObject);
begin
  ListGrid.Enabled := IsNotWait;
  ListGrid.Checked := ListView.GridLines;
end;

procedure TSimpleListTemplate.ListGridExecute(Sender: TObject);
begin
  StartWait;
  try
    ListView.GridLines := not ListView.GridLines;
  finally
    StopWait;
  end;
end;

{ == Мультиобработка =========================================================== }

procedure TSimpleListTemplate.MultiSelectOnOffUpdate(Sender: TObject);
begin
  MultiSelectOnOff.Enabled := IsNotWait;
  MultiSelectOnOff.Checked := ListView.MultiSelect;
end;

procedure TSimpleListTemplate.MultiSelectOnOffExecute(Sender: TObject);
begin
  StartWait;
  try
    ListView.MultiSelect := not ListView.MultiSelect;
  finally
    StopWait;
  end;
end;

{ == Экспорт данных в Microsoft Excel ========================================== }

procedure TSimpleListTemplate.ListExportToExcelUpdate(Sender: TObject);
begin
  ListExportToExcel.Enabled := IsNotWait;
end;

function TSimpleListTemplate.GetReportComment: string;
begin
  Result := EmptyStr;
end;

function TSimpleListTemplate.GetExcelPageOrientation: Integer;
begin
  Result := 0;
end;

procedure TSimpleListTemplate.ListExportToExcelExecute(Sender: TObject);
begin
  if ExportListViewToExcel(ListView, GetDetailExtName, GetDetailName,
      EmptyStr, GetReportComment, GetExcelPageOrientation) then
  begin
    {$IFDEF RSS}
    LogOnListViewExportExcel;
    {$ENDIF}
  end;
end;

{ == Экспорт в файл ============================================================ }

procedure TSimpleListTemplate.ListExportToFileUpdate(Sender: TObject);
begin
  ListExportToFile.Enabled := IsNotWait;
end;

procedure TSimpleListTemplate.ListExportToFileExecute(Sender: TObject);
var
  ExpFileName: string;
  RecordCount: Integer;
begin
  RecordCount := ExportListViewToCsv(ListView, ExpFileName);
  if RecordCount > 0 then
  begin
    {$IFDEF RSS}
    LogOnListViewExportFile(ExpFileName, RecordCount);
    {$ENDIF}
  end;
end;

{ == Генерация текстового отчета для выделенной записи ========================= }

procedure TSimpleListTemplate.ListCreateDynamicReportUpdate(Sender: TObject);
begin
  ListCreateDynamicReport.Enabled := IsNotWait and Assigned(ListView.Selected);
end;

procedure TSimpleListTemplate.ListCreateDynamicReportExecute(Sender: TObject);
begin
  PrintCurrentItem(ListView.Selected);
end;

end.
