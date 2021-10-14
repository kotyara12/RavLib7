unit TmplTreeList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplTreeDetail, ComCtrls, RavListView, Menus, ActnList,
  RavTreeView, Buttons, ExtCtrls, ToolWin, StdCtrls;

type
  TTreeListTemplate = class(TTreeDetailTemplate)
    ListView: TRSortListView;
    ListSortAsc: TAction;
    ListSortDesc: TAction;
    menuSortList: TMenuItem;
    itemListSortAsc: TMenuItem;
    itemListSortDesc: TMenuItem;
    menuSortListD: TMenuItem;
    itemListSortDescD: TMenuItem;
    itemListSortAscD: TMenuItem;
    divSortListD: TMenuItem;
    NewToolButton: TToolButton;
    PropertiesToolButton: TToolButton;
    DeleteToolButton: TToolButton;
    SeparatorEdit: TToolButton;
    FindToolButton: TToolButton;
    SeparatorFind: TToolButton;
    DataToolButton: TToolButton;
    OperToolButton: TToolButton;
    ReportsToolButton: TToolButton;
    ToolsSeparator: TToolButton;
    RefreshToolButton: TToolButton;
    ToolButton13: TToolButton;
    CloseSelectToolButton: TToolButton;
    CloseCancelToolButton: TToolButton;
    ListGrid: TAction;
    ListExportToExcel: TAction;
    ListCreateDynamicReport: TAction;
    itemListExportToExcel: TMenuItem;
    itemListExportToExcelR: TMenuItem;
    itemListExportToExcelP: TMenuItem;
    itemListCreateDynamicReport: TMenuItem;
    itemListCreateDynamicReportR: TMenuItem;
    itemListCreateDynamicReportP: TMenuItem;
    ListExportToFile: TAction;
    itemListExportToFile: TMenuItem;
    itemListExportToFileP: TMenuItem;
    itemListExportToFileR: TMenuItem;
    divDataList: TMenuItem;
    divDataTreeD: TMenuItem;
    MultiSelectOnOff: TAction;
    itemMultiSelectOnOff: TMenuItem;
    divEditMulti: TMenuItem;
    itemMultiSelectOnOffP: TMenuItem;
    FindPanel: TPanel;
    edFastFind: TEdit;
    btnFastFind: TBitBtn;
    FindFast: TAction;
    SelectAll: TAction;
    itemSelectAll: TMenuItem;
    itemSelectAllP: TMenuItem;
    CheckAll: TAction;
    CheckNone: TAction;
    CheckInverse: TAction;
    itemCheckAll: TMenuItem;
    itemCheckNone: TMenuItem;
    itemCheckInverse: TMenuItem;
    divDataDCheck: TMenuItem;
    itemCheckAllD: TMenuItem;
    itemCheckNoneD: TMenuItem;
    itemCheckInverseD: TMenuItem;
    divListMultiedit: TMenuItem;
    divSortList: TMenuItem;
    menuSortListP: TMenuItem;
    itemListSortDescP: TMenuItem;
    itemListSortAscP: TMenuItem;
    divSortMenuP: TMenuItem;
    itemCheckAllP: TMenuItem;
    itemCheckNoneP: TMenuItem;
    itemCheckInverseP: TMenuItem;
    divPopupList: TMenuItem;
    procedure TreePanelResize(Sender: TObject);
    procedure ListSortAscUpdate(Sender: TObject);
    procedure ListSortAscExecute(Sender: TObject);
    procedure ListSortDescUpdate(Sender: TObject);
    procedure ListSortDescExecute(Sender: TObject);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListGridUpdate(Sender: TObject);
    procedure ListGridExecute(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListExportToExcelUpdate(Sender: TObject);
    procedure ListExportToExcelExecute(Sender: TObject);
    procedure ListCreateDynamicReportUpdate(Sender: TObject);
    procedure ListCreateDynamicReportExecute(Sender: TObject);
    procedure ListExportToFileUpdate(Sender: TObject);
    procedure ListExportToFileExecute(Sender: TObject);
    procedure MultiSelectOnOffUpdate(Sender: TObject);
    procedure MultiSelectOnOffExecute(Sender: TObject);
    procedure FindUpdate(Sender: TObject);
    procedure FindExecute(Sender: TObject);
    procedure FindPanelResize(Sender: TObject);
    procedure FindFastUpdate(Sender: TObject);
    procedure FindFastExecute(Sender: TObject);
    procedure edFastFindKeyPress(Sender: TObject; var Key: Char);
    procedure edFastFindEnter(Sender: TObject);
    procedure edFastFindExit(Sender: TObject);
    procedure SelectAllUpdate(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure CheckAllUpdate(Sender: TObject);
    procedure CheckAllExecute(Sender: TObject);
    procedure CheckNoneUpdate(Sender: TObject);
    procedure CheckNoneExecute(Sender: TObject);
    procedure CheckInverseUpdate(Sender: TObject);
    procedure CheckInverseExecute(Sender: TObject);
  private
    LastIndex: Integer;
    procedure CreateSortMenus;
    procedure UpdateSortMenus;
    procedure ChangeSortColumn(Sender: TObject);
  protected
    procedure StartForm; override;
    procedure InitFormVariables; override;
    // Сохранение и восстановление позиции в списке
    procedure ListPositionSave; virtual;
    procedure ListPositionRestore; virtual;
    // Загрузка данных
    procedure LoadDataNodeTry; override;
    procedure LoadDataNodeFinally; override;
    // Генерация имен и тегов набора данных
    {$IFDEF RSS}
    function  GetDetailEditTag: Integer; virtual;
    {$ENDIF}
    function  GetDetailName: string; virtual;
    function  GetDetailExtName: string; virtual;
    function  GetDetailOwnerId: Integer; virtual;
    // Выделение и возврат значения
    function  SelectEnabled: Boolean; override;
    // Экспорт данных
    function  GetReportComment: string; virtual;
    function  GetExcelPageOrientation: Integer; virtual;
    {$IFDEF RSS}
    procedure LogOnListViewExportExcel; virtual;
    procedure LogOnListViewExportFile(const FileName: string; const RecordCount: Integer); virtual;
    {$ENDIF}
    procedure SetSelection(const SelectedID: Integer); override;
    function  GetSelection: Integer; override;
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    procedure ShowItemsCount; override;
    procedure LoadFormControls; override;
    procedure SaveFormControls; override;
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RExHandlers, RFrmStorage, RMsgRu, RProgress, RFind,
  RListView, RFileExport, RDialogs,
  {$IFDEF RSS} RDbLog, RRssConst, {$ENDIF}
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  RxVerInf, RDbPrint, RExpExcel;

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TTreeListTemplate.SetStyle;
begin
  inherited;
  ListView.Color := ApplicationStyle.DataForm.DataColor;
  FontDataToFont(ApplicationStyle.DataForm.DataFont, ListView.Font);
  FindPanel.Visible := ApplicationStyle.DataForm.FastFindPanel;
end;
{$ENDIF}

{ == Сохранение и восстановление размеров формы ================================ }
procedure TTreeListTemplate.LoadFormControls;
begin
  inherited;
  LoadListColumns(Self, ListView);
end;

procedure TTreeListTemplate.SaveFormControls;
begin
  inherited;
  SaveListColumns(Self, ListView);
end;

{ == Отображение формы ========================================================= }
procedure TTreeListTemplate.StartForm;
begin
  try
    inherited;
  finally
    CreateSortMenus;
    UpdateSortMenus;
  end;
end;

procedure TTreeListTemplate.InitFormVariables;
begin
  inherited;

  FindPanel.Height := edFastFind.Height + 10;
end;

procedure TTreeListTemplate.FindPanelResize(Sender: TObject);
begin
  edFastFind.Width := FindPanel.ClientWidth - btnFastFind.Width - 6;

  btnFastFind.Left := FindPanel.ClientWidth - btnFastFind.Width - 3;
  btnFastFind.Height := edFastFind.Height;
end;

{ == Сохранение и восстановление позиции в списке ============================== }
procedure TTreeListTemplate.ListPositionSave;
begin
  if Assigned(ListView.Selected)
  then LastIndex := ListView.Selected.Index
  else LastIndex := intDisable;
end;

procedure TTreeListTemplate.ListPositionRestore;
begin
  if (LastIndex > intDisable) and (LastIndex < ListView.Items.Count)
  then ListView.Selected := ListView.Items[LastIndex];
end;

{ == Загрузка данных =========================================================== }
procedure TTreeListTemplate.LoadDataNodeTry;
begin
  inherited;
  ListView.Items.BeginUpdate;
  ListPositionSave;
end;

procedure TTreeListTemplate.LoadDataNodeFinally;
begin
  ListPositionRestore;
  ListView.Items.EndUpdate;
  ScrollToSelectedItem(ListView);
  inherited;
end;

{ == Выбрать запись и закрыть окно ============================================= }
function TTreeListTemplate.SelectEnabled: Boolean;
begin
  Result := not ListView.MultiSelect and Assigned(ListView.Selected)
    and (GetItemId(ListView.Selected) > intDisable);
end;

{ == Ввод и выборка выбранного значения из формы =============================== }
procedure TTreeListTemplate.SetSelection(const SelectedID: Integer);
begin
  ListView.Selected := LV_FindId(ListView, SelectedID);
end;

function TTreeListTemplate.GetSelection: Integer;
begin
  if Assigned(ListView.Selected)
  then Result := GetItemId(ListView.Selected)
  else Result := intDisable;
end;

function TTreeListTemplate.GetDetailOwnerId: Integer;
begin
  if Assigned(ListView.Selected)
  then Result := GetItemId(ListView.Selected)
  else Result := intDisable;
end;

{ == Отображение числа записей ================================================= }
procedure TTreeListTemplate.TreePanelResize(Sender: TObject);
begin
  StatusBar.Panels[0].Width := TreePanel.Width;
end;

procedure TTreeListTemplate.ShowItemsCount;
begin
  StatusBar.Panels[0].Text := Format(SItemsCount, [TreeView.Items.Count]);
  StatusBar.Panels[1].Text := Format(SItemsCount, [ListView.Items.Count]);
end;

{ == Поиск ===================================================================== }
procedure TTreeListTemplate.FindUpdate(Sender: TObject);
begin
  Find.Enabled := IsNotWait and (ListView.Items.Count > 0);
end;

procedure TTreeListTemplate.FindExecute(Sender: TObject);
begin
  FindInList(ListView);
end;

{ == Быстрый поиск ============================================================= }
procedure TTreeListTemplate.FindFastUpdate(Sender: TObject);
begin
  FindFast.Enabled := IsNotWait and (ListView.Items.Count > 0) and (edFastFind.Text <> '');
end;

procedure TTreeListTemplate.FindFastExecute(Sender: TObject);
begin
  ListView.Selected := LV_FindTextPart(ListView, edFastFind.Text);

  if Assigned(ListView.Selected) then
  begin
    ScrollToSelectedItem(ListView);
    ListView.SetFocus;
  end
  else ErrorBox(Format(SErrStrNotFound, [edFastFind.Text]));
end;

procedure TTreeListTemplate.edFastFindKeyPress(Sender: TObject; var Key: Char);
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

procedure TTreeListTemplate.edFastFindEnter(Sender: TObject);
begin
  CloseSelect.ShortCut := 0;
end;

procedure TTreeListTemplate.edFastFindExit(Sender: TObject);
begin
  CloseSelect.ShortCut := 13;
end;

{ == Изменение столбца сортировки ============================================== }
procedure TTreeListTemplate.CreateSortMenus;

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

procedure TTreeListTemplate.UpdateSortMenus;

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

procedure TTreeListTemplate.ChangeSortColumn(Sender: TObject);
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

procedure TTreeListTemplate.ListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  UpdateSortMenus;
end;

{ == Упорядочить по возрастанию ================================================ }
procedure TTreeListTemplate.ListSortAscUpdate(Sender: TObject);
begin
  ListSortAsc.Enabled := IsNotWait;
  ListSortAsc.Checked := ListView.SortDirection = sdAscending;
end;

procedure TTreeListTemplate.ListSortAscExecute(Sender: TObject);
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

{ == Упорядочить по убыванию =================================================== }
procedure TTreeListTemplate.ListSortDescUpdate(Sender: TObject);
begin
  ListSortDesc.Enabled := IsNotWait;
  ListSortDesc.Checked := ListView.SortDirection = sdDescending;
end;

procedure TTreeListTemplate.ListSortDescExecute(Sender: TObject);
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

{ == Отображать линии сетки ==================================================== }
procedure TTreeListTemplate.ListGridUpdate(Sender: TObject);
begin
  ListGrid.Enabled := IsNotWait;
  ListGrid.Checked := ListView.GridLines;
end;

procedure TTreeListTemplate.ListGridExecute(Sender: TObject);
begin
  StartWait;
  try
    ListView.GridLines := not ListView.GridLines;
  finally
    StopWait;
  end;
end;

{ == Редактирование списка ===================================================== }
procedure TTreeListTemplate.ListViewDblClick(Sender: TObject);
begin
  if Properties.Enabled then PropertiesExecute(Sender);
end;

{ == Генерация кода операции редактирования ==================================== }
{$IFDEF RSS}
function TTreeListTemplate.GetDetailEditTag: Integer;
begin
  Result := CheckTag(ListView.Tag);
end;
{$ENDIF}

{ == Генерация системного имени набора данных ================================== }
function TTreeListTemplate.GetDetailName: string;
begin
  Result := ListView.Name;
end;

function TTreeListTemplate.GetDetailExtName: string;
begin
  Result := Caption;
end;

{ == Генерация текстового отчета для выделенной записи ========================= }
procedure TTreeListTemplate.ListCreateDynamicReportUpdate(Sender: TObject);
begin
  ListCreateDynamicReport.Enabled := IsNotWait and Assigned(ListView.Selected);
end;

procedure TTreeListTemplate.ListCreateDynamicReportExecute(Sender: TObject);
begin
  PrintCurrentItem(ListView.Selected);
end;

{$IFDEF RSS}
procedure TTreeListTemplate.LogOnListViewExportExcel;
begin
  AddToDbLog(GetViewTag, Format(SLogExportExcel, [GetDetailExtName, GetDetailName]));
end;

procedure TTreeListTemplate.LogOnListViewExportFile(const FileName: string; const RecordCount: Integer);
begin
  AddToDbLog(GetViewTag, Format(SLogExportFile, [GetDetailExtName, GetDetailName, FileName, RecordCount]));
end;
{$ENDIF}

{ == Экспорт данных в Microsoft Excel ========================================== }
procedure TTreeListTemplate.ListExportToExcelUpdate(Sender: TObject);
begin
  ListExportToExcel.Enabled := IsNotWait;
end;

function TTreeListTemplate.GetReportComment: string;
begin
  if TreePanel.Visible and Assigned(TreeView.Selected)
  then Result := Format('%s: %s', [TreeHeaderPanel.Caption, TreeView.Selected.Text])
  else Result := EmptyStr;
end;

function TTreeListTemplate.GetExcelPageOrientation: Integer;
begin
  Result := 0;
end;

procedure TTreeListTemplate.ListExportToExcelExecute(Sender: TObject);
var
  VerInfo: TVersionInfo;
begin
  VerInfo := AppVerInfo;
  try
    if ExportListViewToExcel(ListView, GetDetailExtName, GetDetailName,
        EmptyStr, // Format(SCopyrightsStr, [VerInfo.ProductName, VerInfo.InternalName, VerInfo.LegalTrademarks]),
        GetReportComment, GetExcelPageOrientation) then
    begin
      {$IFDEF RSS}
      LogOnListViewExportExcel;
      {$ENDIF}
    end;
  finally
    VerInfo.Free;
  end;
end;

{ == Экспорт в файл ============================================================ }
procedure TTreeListTemplate.ListExportToFileUpdate(Sender: TObject);
begin
  ListExportToFile.Enabled := IsNotWait;
end;

procedure TTreeListTemplate.ListExportToFileExecute(Sender: TObject);
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

{ == Мультиобработка =========================================================== }
procedure TTreeListTemplate.MultiSelectOnOffUpdate(Sender: TObject);
begin
  MultiSelectOnOff.Enabled := IsNotWait;
  MultiSelectOnOff.Checked := ListView.MultiSelect;
end;

procedure TTreeListTemplate.MultiSelectOnOffExecute(Sender: TObject);
begin
  StartWait;
  try
    ListView.MultiSelect := not ListView.MultiSelect;
  finally
    StopWait;
  end;
end;

procedure TTreeListTemplate.SelectAllUpdate(Sender: TObject);
begin
  SelectAll.Enabled := IsNotWait and ListView.MultiSelect
    and (ListView.Items.Count > 0);
end;

procedure TTreeListTemplate.SelectAllExecute(Sender: TObject);
var
  i, iCount: Integer;
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    ListView.Items.BeginUpdate;
    try
      iCount := ListView.Items.Count - 1;
      for i := 0 to iCount do
        ListView.Items[i].Selected := True;
    finally
      ListView.Items.EndUpdate;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeListTemplate.CheckAllUpdate(Sender: TObject);
begin
  CheckAll.Enabled := IsNotWait and ListView.Checkboxes
    and (ListView.Items.Count > 0);
end;

procedure TTreeListTemplate.CheckAllExecute(Sender: TObject);
var
  i, iCount: Integer;
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    ListView.Items.BeginUpdate;
    try
      iCount := ListView.Items.Count - 1;
      for i := 0 to iCount do
        ListView.Items[i].Checked := True;
    finally
      ListView.Items.EndUpdate;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeListTemplate.CheckNoneUpdate(Sender: TObject);
begin
  CheckNone.Enabled := IsNotWait and ListView.Checkboxes
    and (ListView.Items.Count > 0);
end;

procedure TTreeListTemplate.CheckNoneExecute(Sender: TObject);
var
  i, iCount: Integer;
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    ListView.Items.BeginUpdate;
    try
      iCount := ListView.Items.Count - 1;
      for i := 0 to iCount do
        ListView.Items[i].Checked := False;
    finally
      ListView.Items.EndUpdate;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeListTemplate.CheckInverseUpdate(Sender: TObject);
begin
  CheckInverse.Enabled := IsNotWait and ListView.Checkboxes
    and (ListView.Items.Count > 0);
end;

procedure TTreeListTemplate.CheckInverseExecute(Sender: TObject);
var
  i, iCount: Integer;
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    ListView.Items.BeginUpdate;
    try
      iCount := ListView.Items.Count - 1;
      for i := 0 to iCount do
        ListView.Items[i].Checked := not ListView.Items[i].Checked;
    finally
      ListView.Items.EndUpdate;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

end.
