unit TmplList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplData, Menus, ActnList, ComCtrls, ToolWin, RavListView,
  StdCtrls, Buttons, ExtCtrls;

type
  TListTemplate = class(TDataTemplate)
    ListView: TRSortListView;
    ListExportToExcel: TAction;
    ListExportToFile: TAction;
    ListCreateDynamicReport: TAction;
    menuSortList: TMenuItem;
    itemListSortDesc: TMenuItem;
    itemListSortAsc: TMenuItem;
    divSortList: TMenuItem;
    divData1: TMenuItem;
    itemListExportToExcel: TMenuItem;
    itemListExportToFile: TMenuItem;
    itemListCreateDynamicReport: TMenuItem;
    itemListCreateDynamicReportP: TMenuItem;
    itemListExportToExcelP: TMenuItem;
    itemListExportToFileP: TMenuItem;
    itemListCreateDynamicReportO: TMenuItem;
    itemListExportToExcelO: TMenuItem;
    itemListExportToFileO: TMenuItem;
    ListSortAsc: TAction;
    ListSortDesc: TAction;
    menuSortListD: TMenuItem;
    itemListSortDescD: TMenuItem;
    itemListSortAscD: TMenuItem;
    divSortListD: TMenuItem;
    ListGrid: TAction;
    itemListGridP: TMenuItem;
    divPopupLine: TMenuItem;
    MultiSelectOnOff: TAction;
    itemMultiSelectOnOff: TMenuItem;
    divEditMulti: TMenuItem;
    itemMultiSelectOnOffP: TMenuItem;
    itemListCreateDynamicReportR: TMenuItem;
    ListExportToExcelR: TMenuItem;
    itemListExportToFileR: TMenuItem;
    SelectAll: TAction;
    CheckAll: TAction;
    CheckNone: TAction;
    CheckInverse: TAction;
    itemSelectAll: TMenuItem;
    itemSelectAllP: TMenuItem;
    divData2: TMenuItem;
    itemCheckAll: TMenuItem;
    itemCheckInverse: TMenuItem;
    itemCheckNone: TMenuItem;
    divDataD1: TMenuItem;
    itemCheckAllD: TMenuItem;
    itemCheckInverseD: TMenuItem;
    itemCheckNoneD: TMenuItem;
    FindPanel: TPanel;
    edFastFind: TEdit;
    btnFastFind: TBitBtn;
    FindFast: TAction;
    menuSortListP: TMenuItem;
    itemListSortDescP: TMenuItem;
    itemListSortAscP: TMenuItem;
    divListSortP: TMenuItem;
    FindFastClear: TAction;
    btnFastFindClear: TBitBtn;
    procedure ListExportToExcelUpdate(Sender: TObject);
    procedure ListExportToExcelExecute(Sender: TObject);
    procedure FindExecute(Sender: TObject);
    procedure FindUpdate(Sender: TObject);
    procedure ListSortAscUpdate(Sender: TObject);
    procedure ListSortAscExecute(Sender: TObject);
    procedure ListSortDescUpdate(Sender: TObject);
    procedure ListSortDescExecute(Sender: TObject);
    procedure ListGridUpdate(Sender: TObject);
    procedure ListGridExecute(Sender: TObject);
    procedure ListExportToFileUpdate(Sender: TObject);
    procedure ListExportToFileExecute(Sender: TObject);
    procedure ListCreateDynamicReportUpdate(Sender: TObject);
    procedure ListCreateDynamicReportExecute(Sender: TObject);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure MultiSelectOnOffUpdate(Sender: TObject);
    procedure MultiSelectOnOffExecute(Sender: TObject);
    procedure SelectAllUpdate(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure CheckAllUpdate(Sender: TObject);
    procedure CheckAllExecute(Sender: TObject);
    procedure CheckNoneUpdate(Sender: TObject);
    procedure CheckNoneExecute(Sender: TObject);
    procedure CheckInverseUpdate(Sender: TObject);
    procedure CheckInverseExecute(Sender: TObject);
    procedure FindPanelResize(Sender: TObject);
    procedure FindFastUpdate(Sender: TObject);
    procedure FindFastExecute(Sender: TObject);
    procedure edFastFindKeyPress(Sender: TObject; var Key: Char);
    procedure edFastFindEnter(Sender: TObject);
    procedure edFastFindExit(Sender: TObject);
    procedure FindFastClearUpdate(Sender: TObject);
    procedure FindFastClearExecute(Sender: TObject);
  private
    procedure CreateSortMenus;
    procedure UpdateSortMenus;
    procedure ChangeSortColumn(Sender: TObject);
  protected
    LastIndex: Integer;
    procedure StartForm; override;
    procedure InitFormVariables; override;
    // Загрузка данных
    procedure FixPosition; virtual;
    procedure RestorePosition; virtual;
    procedure LoadDataTry; override;
    procedure LoadDataFinally; override;
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
  RVclUtils, RExHandlers, RFrmStorage, RMsgRu, RDialogs, RFind, RListView, RCsvExport,
  {$IFDEF RSS} RDbLog, RRssConst, {$ENDIF}
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  RxVerInf, RDbPrint, RExpExcel;

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TListTemplate.SetStyle;
begin
  inherited;
  ListView.Color := ApplicationStyle.DataForm.DataColor;
  FontDataToFont(ApplicationStyle.DataForm.DataFont, ListView.Font);
  FindPanel.Visible := ApplicationStyle.DataForm.FastFindPanel;
end;
{$ENDIF}

{ == Сохранение и восстановление размеров формы ================================ }
procedure TListTemplate.LoadFormControls;
begin
  inherited;
  LoadListColumns(Self, ListView);
end;

procedure TListTemplate.SaveFormControls;
begin
  inherited;
  SaveListColumns(Self, ListView);
end;

{ == Отображение формы ========================================================= }
procedure TListTemplate.StartForm;
begin
  try
    inherited;
  finally
    CreateSortMenus;
    UpdateSortMenus;
  end;
end;

procedure TListTemplate.InitFormVariables;
begin
  inherited;

  FindPanel.Height := edFastFind.Height + 10;
end;

procedure TListTemplate.FindPanelResize(Sender: TObject);
begin
  edFastFind.Width := FindPanel.ClientWidth - btnFastFind.Width - edFastFind.Height - 12;
  btnFastFind.Left := FindPanel.ClientWidth - btnFastFind.Width - edFastFind.Height - 5;
  btnFastFind.Height := edFastFind.Height;
  btnFastFindClear.Left := btnFastFind.Left + btnFastFind.Width + 2;
  btnFastFindClear.Width := edFastFind.Height;
  btnFastFindClear.Height := edFastFind.Height;
  btnFastFindClear.Caption := '';
end;

{ == Загрузка данных =========================================================== }
procedure TListTemplate.FixPosition;
begin
  if Assigned(ListView.Selected)
  then LastIndex := ListView.Selected.Index
  else LastIndex := intDisable;
end;

procedure TListTemplate.RestorePosition;
begin
  if (LastIndex > intDisable) and (LastIndex < ListView.Items.Count)
  then ListView.Selected := ListView.Items[LastIndex];
end;

procedure TListTemplate.LoadDataTry;
begin
  inherited;
  ListView.Items.BeginUpdate;
  FixPosition;
end;

procedure TListTemplate.LoadDataFinally;
begin
  RestorePosition;
  ListView.Items.EndUpdate;
  ScrollToSelectedItem(ListView);
  inherited;
end;

{ == Выбрать запись и закрыть окно ============================================= }
function TListTemplate.SelectEnabled: Boolean;
begin
  Result := not ListView.MultiSelect and Assigned(ListView.Selected)
    and (GetItemId(ListView.Selected) > intDisable);
end;

{ == Ввод и выборка выбранного значения из формы =============================== }
procedure TListTemplate.SetSelection(const SelectedID: Integer);
begin
  ListView.Selected := LV_FindId(ListView, SelectedID);
end;

function TListTemplate.GetSelection: Integer;
begin
  if Assigned(ListView.Selected)
  then Result := GetItemId(ListView.Selected)
  else Result := intDisable;
end;

function TListTemplate.GetDetailOwnerId: Integer;
begin
  Result := intDisable;
end;

{ == Отображение числа записей ================================================= }
procedure TListTemplate.ShowItemsCount;
begin
  StatusBar.SimpleText := Format(SItemsCount, [ListView.Items.Count]);
end;

{ == Поиск ===================================================================== }
procedure TListTemplate.FindUpdate(Sender: TObject);
begin
  Find.Enabled := IsNotWait and (ListView.Items.Count > 0);
end;

procedure TListTemplate.FindExecute(Sender: TObject);
begin
  FindInList(ListView);
end;

{ == Быстрый поиск ============================================================= }
procedure TListTemplate.FindFastUpdate(Sender: TObject);
begin
  FindFast.Enabled := IsNotWait and (ListView.Items.Count > 0) and (edFastFind.Text <> '');
end;

procedure TListTemplate.FindFastExecute(Sender: TObject);
begin
  ListView.Selected := LV_FindTextPart(ListView, edFastFind.Text);

  if Assigned(ListView.Selected) then
  begin
    ScrollToSelectedItem(ListView);
    ListView.SetFocus;
  end
  else ErrorBox(Format(SErrStrNotFound, [edFastFind.Text]));
end;

procedure TListTemplate.FindFastClearUpdate(Sender: TObject);
begin
  FindFastClear.Enabled := IsNotWait and (ListView.Items.Count > 0) and (edFastFind.Text <> '');
end;

procedure TListTemplate.FindFastClearExecute(Sender: TObject);
begin
  edFastFind.Clear;
  ListView.SetFocus;
end;

procedure TListTemplate.edFastFindKeyPress(Sender: TObject; var Key: Char);
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

procedure TListTemplate.edFastFindEnter(Sender: TObject);
begin
  CloseSelect.ShortCut := 0;
end;

procedure TListTemplate.edFastFindExit(Sender: TObject);
begin
  CloseSelect.ShortCut := 13;
end;

{ == Изменение столбца сортировки ============================================== }
procedure TListTemplate.CreateSortMenus;

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

procedure TListTemplate.UpdateSortMenus;

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

procedure TListTemplate.ChangeSortColumn(Sender: TObject);
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

procedure TListTemplate.ListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  UpdateSortMenus;
end;

{ == Упорядочить по возрастанию ================================================ }
procedure TListTemplate.ListSortAscUpdate(Sender: TObject);
begin
  ListSortAsc.Checked := ListView.SortDirection = sdAscending;
  ListSortAsc.Enabled := IsNotWait;
end;

procedure TListTemplate.ListSortAscExecute(Sender: TObject);
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
procedure TListTemplate.ListSortDescUpdate(Sender: TObject);
begin
  ListSortDesc.Checked := ListView.SortDirection = sdDescending;
  ListSortDesc.Enabled := IsNotWait;
end;

procedure TListTemplate.ListSortDescExecute(Sender: TObject);
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
procedure TListTemplate.ListGridUpdate(Sender: TObject);
begin
  ListGrid.Enabled := IsNotWait;
  ListGrid.Checked := ListView.GridLines;
end;

procedure TListTemplate.ListGridExecute(Sender: TObject);
begin
  StartWait;
  try
    ListView.GridLines := not ListView.GridLines;
  finally
    StopWait;
  end;
end;

{$IFDEF RSS}
{ == Генерация кода операции редактирования ==================================== }
function TListTemplate.GetDetailEditTag: Integer;
begin
  Result := CheckTag(ListView.Tag);
end;
{$ENDIF}

{ == Генерация системного имени набора данных ================================== }
function TListTemplate.GetDetailName: string;
begin
  Result := ListView.Name;
end;

function TListTemplate.GetDetailExtName: string;
begin
  Result := Caption;
end;

{$IFDEF RSS}
procedure TListTemplate.LogOnListViewExportExcel;
begin
  AddToDbLog(GetViewTag, Format(SLogExportExcel, [GetDetailExtName, GetDetailName]));
end;

procedure TListTemplate.LogOnListViewExportFile(const FileName: string; const RecordCount: Integer);
begin
  AddToDbLog(GetViewTag, Format(SLogExportFile, [GetDetailExtName, GetDetailName, FileName, RecordCount]));
end;
{$ENDIF}

{ == Экспорт данных в Microsoft Excel ========================================== }
procedure TListTemplate.ListExportToExcelUpdate(Sender: TObject);
begin
  ListExportToExcel.Enabled := IsNotWait;
end;

function TListTemplate.GetReportComment: string;
begin
  Result := EmptyStr;
end;

function TListTemplate.GetExcelPageOrientation: Integer;
begin
  Result := 0;
end;

procedure TListTemplate.ListExportToExcelExecute(Sender: TObject);
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
procedure TListTemplate.ListExportToFileUpdate(Sender: TObject);
begin
  ListExportToFile.Enabled := IsNotWait;
end;

procedure TListTemplate.ListExportToFileExecute(Sender: TObject);
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
procedure TListTemplate.ListCreateDynamicReportUpdate(Sender: TObject);
begin
  ListCreateDynamicReport.Enabled := IsNotWait and Assigned(ListView.Selected);
end;

procedure TListTemplate.ListCreateDynamicReportExecute(Sender: TObject);
begin
  PrintCurrentItem(ListView.Selected);
end;

{ == Мультиобработка =========================================================== }
procedure TListTemplate.MultiSelectOnOffUpdate(Sender: TObject);
begin
  MultiSelectOnOff.Enabled := IsNotWait;
  MultiSelectOnOff.Checked := ListView.MultiSelect;
end;

procedure TListTemplate.MultiSelectOnOffExecute(Sender: TObject);
begin
  StartWait;
  try
    ListView.MultiSelect := not ListView.MultiSelect;
  finally
    StopWait;
  end;
end;

procedure TListTemplate.SelectAllUpdate(Sender: TObject);
begin
  SelectAll.Enabled := IsNotWait and ListView.MultiSelect
    and (ListView.Items.Count > 0);
end;

procedure TListTemplate.SelectAllExecute(Sender: TObject);
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

procedure TListTemplate.CheckAllUpdate(Sender: TObject);
begin
  CheckAll.Enabled := IsNotWait and ListView.Checkboxes
    and (ListView.Items.Count > 0);
end;

procedure TListTemplate.CheckAllExecute(Sender: TObject);
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

procedure TListTemplate.CheckNoneUpdate(Sender: TObject);
begin
  CheckNone.Enabled := IsNotWait and ListView.Checkboxes
    and (ListView.Items.Count > 0);
end;

procedure TListTemplate.CheckNoneExecute(Sender: TObject);
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

procedure TListTemplate.CheckInverseUpdate(Sender: TObject);
begin
  CheckInverse.Enabled := IsNotWait and ListView.Checkboxes
    and (ListView.Items.Count > 0);
end;

procedure TListTemplate.CheckInverseExecute(Sender: TObject);
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
