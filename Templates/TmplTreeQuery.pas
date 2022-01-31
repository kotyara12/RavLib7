unit TmplTreeQuery;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplTreeDetail, Menus, ActnList, ImgList, ComCtrls, RavTreeView,
  Buttons, ExtCtrls, ToolWin, Grids, DBGrids, RDbColorGrid, RDbPanel, AdoDb, DB, DBCtrls,
  RDbStatus, RDbCustom, RDbGridTuner, RDbOrder, RDbFilter, RDbFind, RDbData,
  DBActns, RDbEditor, RDbCustomSearch, RDbSearch, RDbUpdater, StdCtrls,
  Tabs;

type
  TTreeQueryTemplate = class(TTreeDetailTemplate)
    InfoPanel: TRDbInfoPanel;
    DbGrid: TRDbStyledGrid;
    RDbFind: TRDbSearch;
    RDbFilter: TRDbFilter;
    RDbOrder: TRDbOrder;
    RDbGridTuner: TRDbGridTuner;
    RDbFilterStatus: TRDbFilterStatus;
    DataSetFirst: TDataSetFirst;
    DataSetPrior: TDataSetPrior;
    DataSetNext: TDataSetNext;
    DataSetLast: TDataSetLast;
    itemDataSetFirst: TMenuItem;
    itemDataSetPrior: TMenuItem;
    itemDataSetNext: TMenuItem;
    itemDataSetLast: TMenuItem;
    divDataFilter: TMenuItem;
    FindRecordToolButton: TToolButton;
    SeparatorFind: TToolButton;
    CloseSelectToolButton: TToolButton;
    CloseCancelToolButton: TToolButton;
    NewToolButton: TToolButton;
    PropertiesToolButton: TToolButton;
    DeleteItemToolButton: TToolButton;
    SeparatorEdit: TToolButton;
    RefreshToolButton: TToolButton;
    SeparatorRefresh: TToolButton;
    DbGridSetup: TAction;
    DbGridDefault: TAction;
    FilterUser: TAction;
    FilterDefault: TAction;
    FilterNone: TAction;
    SortUser: TAction;
    SortDefault: TAction;
    DataToolButton: TToolButton;
    itemFilterUser: TMenuItem;
    itemFilterDefault: TMenuItem;
    itemFilterNone: TMenuItem;
    divDataSort: TMenuItem;
    itemSortUser: TMenuItem;
    itemSortDefault: TMenuItem;
    divDataGrid: TMenuItem;
    itemDbGridSetup: TMenuItem;
    itemDbGridDefault: TMenuItem;
    ReportsToolButton: TToolButton;
    itemDataSetExportToExcelP: TMenuItem;
    SeparatorEnd: TToolButton;
    OpersToolButton: TToolButton;
    itemFilterUserD: TMenuItem;
    itemFilterDefaultD: TMenuItem;
    itemFilterNoneD: TMenuItem;
    divDataDFilter: TMenuItem;
    itemSortUserD: TMenuItem;
    itemSortDefaultD: TMenuItem;
    divDataDSort: TMenuItem;
    itemDbGridSetupD: TMenuItem;
    itemDbGridDefaultD: TMenuItem;
    DataSetExportToExcel: TAction;
    itemDataSetExportToExcelR: TMenuItem;
    itemDataSetExportToExcel: TMenuItem;
    DataSetCreateDynamicReport: TAction;
    itemDataSetCreateDynamicReport: TMenuItem;
    itemDataSetCreateDynamicReportP: TMenuItem;
    itemDataSetCreateDynamicReportR: TMenuItem;
    DataSetExportToFileCsv: TAction;
    itemDataSetExportToFile: TMenuItem;
    itemDataSetExportToFileR: TMenuItem;
    itemDataSetExportToFileP: TMenuItem;
    divDataTree: TMenuItem;
    divDataDTree: TMenuItem;
    RDbEditor: TRDbExportEditor;
    divEditImport: TMenuItem;
    ImportDS: TAction;
    itemImportDS: TMenuItem;
    MultiSelectOnOff: TAction;
    divEditMulti: TMenuItem;
    itemMultiSelectOnOff: TMenuItem;
    itemMultiSelectOnOffP: TMenuItem;
    DataSetReportList: TAction;
    divRepExp: TMenuItem;
    itemDataSetReportList: TMenuItem;
    itemDataSetReportListR: TMenuItem;
    divRepExpR: TMenuItem;
    divRepExpP: TMenuItem;
    itemDataSetReportListP: TMenuItem;
    Attachments: TAction;
    divAttach: TMenuItem;
    itemAttachments: TMenuItem;
    itemAttachmentsP: TMenuItem;
    TitleGridPopupMenu: TPopupMenu;
    itemDbGridSetupT: TMenuItem;
    itemDbGridDefaultT: TMenuItem;
    ColumnLeft: TAction;
    ColumnCenter: TAction;
    ColumnRight: TAction;
    itemColumnLeft: TMenuItem;
    itemColumnCenter: TMenuItem;
    itemColumnRight: TMenuItem;
    menuAlgn: TMenuItem;
    divMenuTitle1: TMenuItem;
    itemSortUserT: TMenuItem;
    SetCurrOrderAsc: TAction;
    itemSetCurrOrderAsc: TMenuItem;
    SetCurrOrderDesc: TAction;
    itemSetCurrOrderDesc: TMenuItem;
    FindColumn: TAction;
    itemFindColumn: TMenuItem;
    divMenuTitle2: TMenuItem;
    DataSetStatistic: TAction;
    divRepStat: TMenuItem;
    itemStatistic: TMenuItem;
    itemStatisticR: TMenuItem;
    divRepStatR: TMenuItem;
    ColumnStatistic: TAction;
    divMenuTitle3: TMenuItem;
    itemColumnStatisticT: TMenuItem;
    SelectAll: TAction;
    itemSelectAllP: TMenuItem;
    itemSelectAll: TMenuItem;
    RDbLocate: TRDbFind;
    DbLocate: TAction;
    itemDbLocate: TMenuItem;
    itemDbLocateP: TMenuItem;
    itemDbLocateT: TMenuItem;
    RDbUpdater: TRDbUpdater;
    SelectDefaultValues: TAction;
    itemSelectDefaultValues: TMenuItem;
    itemSelectDefaultValuesP: TMenuItem;
    FindPanel: TPanel;
    edFastFind: TEdit;
    btnFastFind: TBitBtn;
    FindFast: TAction;
    InfoPanelPopupMenu: TPopupMenu;
    itemCopyInfoPanel: TMenuItem;
    FilterSelected: TAction;
    itemFilterSelected: TMenuItem;
    itemFilterSelectedD: TMenuItem;
    TabViews: TTabSet;
    ViewsPopupMenu: TPopupMenu;
    itemDbGridSetupV: TMenuItem;
    DateSetGrouping: TAction;
    itemDateSetGrouping: TMenuItem;
    itemDateSetGroupingR: TMenuItem;
    itemDateSetGroupingT: TMenuItem;
    divPopupMultiedit: TMenuItem;
    menuNavigationP: TMenuItem;
    itemDataSetLastP: TMenuItem;
    itemDataSetNextP: TMenuItem;
    itemDataSetPriorP: TMenuItem;
    itemDataSetFirstP: TMenuItem;
    menuViewsDataP: TMenuItem;
    itemDbGridDefaultP: TMenuItem;
    itemDbGridSetupP: TMenuItem;
    menuGroupsP: TMenuItem;
    itemDataSetStatisticP: TMenuItem;
    itemDateSetGroupingP: TMenuItem;
    menuFilterDataP: TMenuItem;
    itemFilterNoneP: TMenuItem;
    itemFilterSelectedP: TMenuItem;
    itemFilterDefaultP: TMenuItem;
    itemFilterUserP: TMenuItem;
    menuSortDataP: TMenuItem;
    itemSortDefaultP: TMenuItem;
    itemSortUserP: TMenuItem;
    FindFastClear: TAction;
    btnFastFindClear: TBitBtn;
    procedure DbGridSetupExecute(Sender: TObject);
    procedure DbGridSetupUpdate(Sender: TObject);
    procedure DbGridDefaultUpdate(Sender: TObject);
    procedure DbGridDefaultExecute(Sender: TObject);
    procedure FilterUserUpdate(Sender: TObject);
    procedure FilterUserExecute(Sender: TObject);
    procedure FilterDefaultUpdate(Sender: TObject);
    procedure FilterDefaultExecute(Sender: TObject);
    procedure FilterNoneUpdate(Sender: TObject);
    procedure FilterNoneExecute(Sender: TObject);
    procedure SortUserUpdate(Sender: TObject);
    procedure SortUserExecute(Sender: TObject);
    procedure SortDefaultUpdate(Sender: TObject);
    procedure SortDefaultExecute(Sender: TObject);
    procedure DataSetExportToExcelUpdate(Sender: TObject);
    procedure DataSetExportToExcelExecute(Sender: TObject);
    procedure DataSetCreateDynamicReportUpdate(Sender: TObject);
    procedure DataSetCreateDynamicReportExecute(Sender: TObject);
    procedure NewRecordUpdate(Sender: TObject);
    procedure NewRecordExecute(Sender: TObject);
    procedure DbGridDblClick(Sender: TObject);
    procedure DataSetExportToFileCsvUpdate(Sender: TObject);
    procedure DataSetExportToFileCsvExecute(Sender: TObject);
    procedure GetNewKey(Sender: TObject; var Value: Integer);
    procedure FreeNewKey(Sender: TObject; var Value: Integer);
    procedure SaveToLog(Sender: TObject; const EditTag: Integer;
      const Text: String);
    procedure DetailGetExcelCopyright(Sender: TObject; var Value: string);
    procedure DetailBeforeShowEditor(Sender: TObject; Editor: TForm;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure DetailAfterPostLogged(Sender: TObject; Editor: TForm;
      OldData, NewData: TRecordData; const Mode: TEditMode;
      const EditTag: Integer; var Complete: Boolean);
    procedure DetailGetExcelComment(Sender: TObject; var Value: String);
    procedure ImportDSUpdate(Sender: TObject);
    procedure ImportDSExecute(Sender: TObject);
    procedure MultiSelectOnOffUpdate(Sender: TObject);
    procedure MultiSelectOnOffExecute(Sender: TObject);
    procedure DetailGetExcelCaption(Sender: TObject; var Value: String);
    procedure FindUpdate(Sender: TObject);
    procedure FindExecute(Sender: TObject);
    procedure DataSetReportListUpdate(Sender: TObject);
    procedure DataSetReportListExecute(Sender: TObject);
    procedure DetailBeforeDelete(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure AttachmentsUpdate(Sender: TObject);
    procedure AttachmentsExecute(Sender: TObject);
    procedure ColumnLeftUpdate(Sender: TObject);
    procedure ColumnLeftExecute(Sender: TObject);
    procedure ColumnCenterUpdate(Sender: TObject);
    procedure ColumnCenterExecute(Sender: TObject);
    procedure ColumnRightUpdate(Sender: TObject);
    procedure ColumnRightExecute(Sender: TObject);
    procedure SetCurrOrderAscUpdate(Sender: TObject);
    procedure SetCurrOrderAscExecute(Sender: TObject);
    procedure SetCurrOrderDescUpdate(Sender: TObject);
    procedure SetCurrOrderDescExecute(Sender: TObject);
    procedure DbGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DetailAfterProcessRecord(Sender: TObject;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure FindColumnUpdate(Sender: TObject);
    procedure FindColumnExecute(Sender: TObject);
    procedure DetailAfterProcessRecords(Sender: TObject);
    procedure DataSetStatisticUpdate(Sender: TObject);
    procedure DataSetStatisticExecute(Sender: TObject);
    procedure ColumnStatisticUpdate(Sender: TObject);
    procedure ColumnStatisticExecute(Sender: TObject);
    procedure SelectAllUpdate(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure DbLocateUpdate(Sender: TObject);
    procedure DbLocateExecute(Sender: TObject);
    procedure RDbEditorCreateSetDefault(Sender: TObject;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure SelectDefaultValuesUpdate(Sender: TObject);
    procedure SelectDefaultValuesExecute(Sender: TObject);
    procedure FindPanelResize(Sender: TObject);
    procedure FindFastUpdate(Sender: TObject);
    procedure FindFastExecute(Sender: TObject);
    procedure edFastFindKeyPress(Sender: TObject; var Key: Char);
    procedure edFastFindEnter(Sender: TObject);
    procedure edFastFindExit(Sender: TObject);
    procedure InfoPanelPopupMenuPopup(Sender: TObject);
    procedure itemCopyInfoPanelClick(Sender: TObject);
    procedure FilterSelectedUpdate(Sender: TObject);
    procedure FilterSelectedExecute(Sender: TObject);
    procedure RDbGridTunerViewChange(Sender: TObject);
    procedure TabViewsChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure DateSetGroupingUpdate(Sender: TObject);
    procedure DateSetGroupingExecute(Sender: TObject);
    procedure DbGridColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    procedure FindFastClearUpdate(Sender: TObject);
    procedure FindFastClearExecute(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure DataToolButtonClick(Sender: TObject);
    procedure OpersToolButtonClick(Sender: TObject);
    procedure ReportsToolButtonClick(Sender: TObject);
  private
    procedure ColumnFlagsReset;
    procedure ColumnFlagsUpdate;
  protected
    fCurrColumn: TColumn;
    fLastState: Boolean;
    {$IFDEF ATTACH}
    fDsAttachsCnt: TAdoQuery;
    {$ENDIF}
    procedure InitFormVariables; override;
    procedure InitDataComponents; override;
    procedure DoneDataComponents; override;
    {$IFDEF ATTACH}
    procedure CreateAttachmentsField; virtual;
    procedure LoadAttachments; override;
    {$ENDIF}
    procedure LoadDataNodeTry; override;
    procedure LoadDataNodeFinally; override;
    function  LoadDataNodeEnd: Boolean; override;
    procedure CloseDataSets; override;
    // Выделение и возврат значения
    function  SelectVisible: Boolean; override;
    function  SelectEnabled: Boolean; override;
    // Редактирование данных
    function  DetailInsertEnabled: Boolean; override;
    function  DetailCopyEnabled: Boolean; override;
    function  DetailOpenEnabled: Boolean; override;
    function  DetailMoveEnabled: Boolean; override;
    function  DetailEditEnabled: Boolean; override;
    function  DetailDeleteEnabled: Boolean; override;
    procedure DetailCopy; override;
    procedure DetailEdit; override;
    function  DetailMove(TargetNode: TTreeNode): Boolean; override;
    procedure DetailDelete; override;
    // Проверка перед удалением группы
    function  TreeNodeDetailIsEmpty(Node: TTreeNode): Boolean; override;
    // Установка и считывание значений
    procedure SetSelection(const SelectedID: Integer); override;
    function  GetSelection: Integer; override;
    procedure SetSelections(const GroupId, SelectedID: Integer); override;
    function  GetSelections(out GroupId, SelectedID: Integer): Boolean; override;
    // Параметры фильтра данных
    function  ReportsVar_FilterTree: string; virtual;
    function  ReportsVar_FilterUser: string; virtual;
    function  ReportsVar_Variables: string; virtual;
    function  ReportsVar_DefaultPath: string; virtual;
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RDialogs, RMsgRu, RExHandlers, RDbState, RDbUtils, RDbConst, RDbText,
  RDbImport, RProgress, RDbOpenDS, ClipBrd, RClipBrd, RxStrUtils, StrUtils,
  {$IFDEF RSS} RDbLog, RRssConst, {$ENDIF}
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  {$IFDEF ATTACH} RDbAttachs, {$ENDIF}
  {$IFDEF FR4} ReportsForm, {$ENDIF}
  RDbPrint, BaseDbUnit, TmplDbDialog;

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TTreeQueryTemplate.SetStyle;
begin
  inherited;
  DbGrid.Color := ApplicationStyle.DataForm.DataColor;
  TabViews.BackgroundColor := ApplicationStyle.DataForm.FormColor;
  FontDataToFont(ApplicationStyle.DataForm.DataFont, DbGrid.Font);
  FontDataToFont(ApplicationStyle.DataForm.DataFont, TabViews.Font);
  // FontDataToFont(ApplicationStyle.DataForm.DataFont, DbGrid.TitleFont);
  InfoPanel.BeginUpdate;
  try
    FindPanel.Visible := ApplicationStyle.DataForm.FastFindPanel;
    InfoPanel.ControlStyle := InfoPanel.ControlStyle - [csParentBackground] + [csOpaque];
    InfoPanel.Visible := ApplicationStyle.DataForm.InfoPanelVisible;
    InfoPanel.Color := ApplicationStyle.DataForm.InfoPanelColor;
    InfoPanel.ColorText := ApplicationStyle.DataForm.InfoTextColor;
    FontDataToFont(ApplicationStyle.DataForm.InfoLabelFont, InfoPanel.FontLabels);
    FontDataToFont(ApplicationStyle.DataForm.InfoTextFont, InfoPanel.FontTexts);
  finally
    InfoPanel.EndUpdate;
  end;
end;
{$ENDIF}

{ == Инициализация Db-компонентов ============================================== }
procedure TTreeQueryTemplate.InitFormVariables;
begin
  {$IFDEF ATTACH}
  fDsAttachsCnt := nil;
  {$ENDIF}

  inherited InitFormVariables;

  FindPanel.Height := edFastFind.Height + 10;
end;

procedure TTreeQueryTemplate.FindPanelResize(Sender: TObject);
begin
  edFastFind.Width := FindPanel.ClientWidth - btnFastFind.Width - edFastFind.Height - 8;
  btnFastFind.Left := FindPanel.ClientWidth - btnFastFind.Width - edFastFind.Height - 5;
  btnFastFind.Height := edFastFind.Height;
  btnFastFindClear.Left := btnFastFind.Left + btnFastFind.Width + 2;
  btnFastFindClear.Width := edFastFind.Height;
  btnFastFindClear.Height := edFastFind.Height;
  btnFastFindClear.Caption := '';
end;

procedure TTreeQueryTemplate.InitDataComponents;
begin
  try
    inherited;
  finally
    try
      {$IFDEF FR4}
      DataSetReportList.Visible := True;
      divRepExp.Visible := True;
      divRepExpP.Visible := True;
      divRepExpR.Visible := True;
      {$ELSE}
      DataSetReportList.Visible := False;
      divRepExp.Visible := False;
      divRepExpP.Visible := False;
      divRepExpR.Visible := False;
      {$ENDIF}
      {$IFDEF ATTACH}
      Attachments.Visible := RDbEditor.KeyFieldIsPresent and (DbGrid.AttachField <> '');
      divAttach.Visible := Attachments.Visible;
      {$ELSE}
      Attachments.Visible := False;
      divAttach.Visible := False;
      {$ENDIF}
      RDbFilter.DateFormatWhere := BaseData.DbParameters.DateFormat;
      RDbFilter.CaseStringsEnabled := BaseData.DbParameters.CaseEnabled;
    finally
      try
        ShowInStatusBar(Format(SMsgLoadDataFormEx, [SPrmFind]));
        RDbFind.Open;
        if Assigned(RDbLocate.DataSet) then
          RDbLocate.Open;
      finally
        ShowInStatusBar(Format(SMsgLoadDataFormEx, [SPrmGridTuner]));
        {$IFDEF ATTACH}
        CreateAttachmentsField;
        {$ENDIF}
        RDbGridTuner.Open;
      end;
    end;
  end;
end;

{ == Деактивация Db-компонентов ================================================ }
procedure TTreeQueryTemplate.DoneDataComponents;
begin
  if RDbEditor.DataSet.Filtered then
  begin
    RDbEditor.DataSet.Filtered := False;
    RDbEditor.DataSet.Filter := '';
  end;
  try
    inherited;
  finally
    try
      try
        try
          ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmFind]));
          if RDbFind.Active then RDbFind.Close;
          if RDbLocate.Active then RDbLocate.Close;
        finally
          ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmGridTuner]));
          if RDbGridTuner.Active then RDbGridTuner.Close;
        end;
      finally
        ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmFilter]));
        if RDbFilter.Active then RDbFilter.Close;
      end;
    finally
      ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmOrder]));
      if RDbOrder.Active then RDbOrder.Close;
    end;
  end;
end;

{$IFDEF ATTACH}
{ == Прикрепленные файлы ======================================================= }
procedure TTreeQueryTemplate.CreateAttachmentsField;
begin
  if Attachments.Visible and not RDbEditor.DataSetIsOpened then
    rAttachs_CreateAttachsFld(RDbEditor);
end;

procedure TTreeQueryTemplate.LoadAttachments;
begin
  try
    if Assigned(fDsAttachsCnt)
    then SafeRequery(fDsAttachsCnt)
    else fDsAttachsCnt := rAttachs_OpenAttachsLookup(BaseData.acDb, RDbEditor);
  except
    on E: Exception do
    begin
      HandleExcept(E, fDsAttachsCnt, sErrOpenAttachsLookup);
      FreeAndNil(fDsAttachsCnt);
    end;
  end;
end;
{$ENDIF}

{ == Метки сортировки на заголовке таблицы ===================================== }

procedure TTreeQueryTemplate.ColumnFlagsReset;
begin
  DbGrid.ColumnFlagsClear;
end;

procedure TTreeQueryTemplate.ColumnFlagsUpdate;
begin
  if RDbOrder.Active then
    DbGrid.FieldFlagSort(RDbOrder.GetOrderString);
end;

{ == Загрузка данных =========================================================== }
procedure TTreeQueryTemplate.LoadDataNodeTry;
begin
  inherited LoadDataNodeTry;
  ColumnFlagsReset;
end;

procedure TTreeQueryTemplate.LoadDataNodeFinally;
begin
  inherited LoadDataNodeFinally;
  ColumnFlagsUpdate;
end;

function TTreeQueryTemplate.LoadDataNodeEnd: Boolean;
begin
  Result := inherited LoadDataNodeEnd;
  RDbEditor.ClearSelection;
end;

{ == Закрываем открытые наборы данных ========================================== }
procedure TTreeQueryTemplate.CloseDataSets;
begin
  inherited;

  if RDbEditor.DataSetIsOpened then
    RDbEditor.DataSet.Close;

  {$IFDEF ATTACH}
  if Assigned(fDsAttachsCnt) then
    FreeAndNil(fDsAttachsCnt);
  {$ENDIF}
end;

{ == Выбрать запись и закрыть окно ============================================= }
function TTreeQueryTemplate.SelectVisible: Boolean;
begin
  Result := inherited SelectVisible and RDbEditor.KeyFieldIsPresent;
end;

function TTreeQueryTemplate.SelectEnabled: Boolean;
begin
  Result := IsNotWait and RDbEditor.KeyFieldIsPresent and (RDbEditor.SelCount = 1);
end;

{ == Ввод и выборка выбранного значения из формы =============================== }
procedure TTreeQueryTemplate.SetSelection(const SelectedID: Integer);
begin
  RDbEditor.LocateKey(SelectedID);
end;

function TTreeQueryTemplate.GetSelection: Integer;
begin
  Result := RDbEditor.GetKeyValue;
end;

procedure TTreeQueryTemplate.SetSelections(const GroupId, SelectedID: Integer);
begin
  if TreePanel.Visible then TreeView.GotoNode([ntGroup, ntItem], GroupId, gtSelectTopNode);
  SetSelection(SelectedID);
end;

function TTreeQueryTemplate.GetSelections(out GroupId, SelectedID: Integer): Boolean;
begin
  Result := RDbEditor.KeyFieldIsPresent and RDbEditor.DataSetIsNotEmply;
  if Result then
  begin
    GroupId := RDbEditor.GetOwnerValue;
    SelectedID := RDbEditor.GetKeyValue;
  end
  else begin
    GroupId := TreeView.GetNodeId(TreeView.Selected);
    SelectedID := intDisable;
  end;
end;

{ == Поиск данных по таблице =================================================== }
procedure TTreeQueryTemplate.FindUpdate(Sender: TObject);
begin
  Find.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply and RDbFind.Active;
end;

procedure TTreeQueryTemplate.FindExecute(Sender: TObject);
begin
  RDbFind.ShowDialog;
end;

procedure TTreeQueryTemplate.FindColumnUpdate(Sender: TObject);
begin
  FindColumn.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply
    and RDbFind.Active and Assigned(fCurrColumn);
end;

procedure TTreeQueryTemplate.FindColumnExecute(Sender: TObject);
begin
  if Assigned(fCurrColumn) then
    RDbFind.ShowDialogEx(fCurrColumn.FieldName, EmptyStr, False);
end;

{ == Перейти к указанной записи ================================================ }
procedure TTreeQueryTemplate.DbLocateUpdate(Sender: TObject);
begin
  DbLocate.Visible := RDbLocate.Active;
  DbLocate.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TTreeQueryTemplate.DbLocateExecute(Sender: TObject);
begin
  RDbLocate.ShowDialog;
end;

{ == Быстрый поиск ============================================================= }
procedure TTreeQueryTemplate.FindFastUpdate(Sender: TObject);
begin
  FindFast.Enabled := IsNotWait and RDbEditor.DataSetIsOpened;
end;

procedure TTreeQueryTemplate.FindFastExecute(Sender: TObject);
begin
  if edFastFind.Text = EmptyStr
  then RDbEditor.DataSet.Filtered := False
  else begin
    try
      DbFilterAllFields(RDbEditor.DataSet, edFastFind.Text,
        {$IFDEF STYLES} ApplicationStyle.DataForm.FastFindLeadAsterisk {$ELSE} True {$ENDIF}, True);
    except
      try
        DbFilterAllFields(RDbEditor.DataSet, edFastFind.Text,
          {$IFDEF STYLES} ApplicationStyle.DataForm.FastFindLeadAsterisk {$ELSE} True {$ENDIF}, False);
      except
        on E: Exception do
        begin
          RDbEditor.DataSet.Filtered := False;
          RDbEditor.DataSet.Filter := '';
          ErrorBox(Format(SErrFindError, [edFastFind.Text]));
        end;
      end;
    end;
  end;
end;

procedure TTreeQueryTemplate.FindFastClearUpdate(Sender: TObject);
begin
  FindFastClear.Enabled := IsNotWait and RDbEditor.DataSetIsOpened and RDbEditor.DataSet.Filtered;
end;

procedure TTreeQueryTemplate.FindFastClearExecute(Sender: TObject);
begin
  edFastFind.Text := '';
  RDbEditor.DataSet.Filtered := False;
  RDbEditor.DataSet.Filter := '';
  DbGrid.SetFocus;
end;

procedure TTreeQueryTemplate.edFastFindKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    if RDbEditor.DataSetIsOpened then
    begin
      FindFastExecute(Sender);
      if {$IFDEF STYLES} ApplicationStyle.DataForm.FastFindGotoData and {$ENDIF}
        (RDbEditor.DataSet.RecordCount > 0) then
          DbGrid.SetFocus;
    end;
  end;

  if Key = #27 then
  begin
    Key := #0;
    edFastFind.Clear;
    RDbEditor.DataSet.Filtered := False;
    RDbEditor.DataSet.Filter := '';
    {$IFDEF STYLES}
    if ApplicationStyle.DataForm.FastFindGotoData then
      DbGrid.SetFocus;
    {$ELSE}
    DbGrid.SetFocus;
    {$ENDIF}
  end;
end;

procedure TTreeQueryTemplate.edFastFindEnter(Sender: TObject);
begin
  CloseSelect.ShortCut := 0;
  DataSetFirst.ShortCut := 0;
  DataSetLast.ShortCut := 0;
end;

procedure TTreeQueryTemplate.edFastFindExit(Sender: TObject);
begin
  CloseSelect.ShortCut := 13;
  DataSetFirst.ShortCut := 36;
  DataSetLast.ShortCut := 35;
end;

{ == Настройка отображения столбцов в таблице ================================== }
procedure TTreeQueryTemplate.DbGridSetupUpdate(Sender: TObject);
begin
  DbGridSetup.Enabled := IsNotWait and Assigned(RDbEditor.DataSet)
    and RDbGridTuner.Active;
end;

procedure TTreeQueryTemplate.DbGridSetupExecute(Sender: TObject);
begin
  if RDbGridTuner.ShowDialog then
  begin
    fCurrColumn := nil;
    ColumnFlagsReset;
    ColumnFlagsUpdate;
  end;
end;

procedure TTreeQueryTemplate.RDbGridTunerViewChange(Sender: TObject);
var
  i, iCount: Integer;
begin
  iCount := RDbGridTuner.Views.Count;

  TabViews.Tabs.BeginUpdate;
  try
    TabViews.Tabs.Clear;
    for i := 0 to iCount - 1 do
      TabViews.Tabs.Add(RDbGridTuner.Views[i]);

    TabViews.TabIndex := RDbGridTuner.ViewActive;
  finally
    TabViews.Tabs.EndUpdate;
  end;

  TabViews.Visible := iCount > 1;
end;

procedure TTreeQueryTemplate.TabViewsChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
begin
  RDbGridTuner.ViewActive := NewTab;
  ColumnFlagsReset;
  ColumnFlagsUpdate;
end;

procedure TTreeQueryTemplate.DbGridColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
begin
  ColumnFlagsReset;
  ColumnFlagsUpdate;
end;

{ == Установить столбцы "по умолчанию" (все видимые) =========================== }
procedure TTreeQueryTemplate.DbGridDefaultUpdate(Sender: TObject);
begin
  DbGridDefault.Enabled := IsNotWait and Assigned(RDbEditor.DataSet)
    and RDbGridTuner.Active;
end;

procedure TTreeQueryTemplate.DbGridDefaultExecute(Sender: TObject);
begin
  if QueryBoxStdNY(SQueryResetColumns) = ID_YES then
  begin
    RDbGridTuner.ResetColumns;
    fCurrColumn := nil;
    ColumnFlagsReset;
    ColumnFlagsUpdate;
  end;
end;

{ == Установка произвольного фильтра данных ==================================== }
procedure TTreeQueryTemplate.FilterUserUpdate(Sender: TObject);
begin
  FilterUser.Enabled := IsNotWait and RDbFilter.Active;
end;

procedure TTreeQueryTemplate.FilterUserExecute(Sender: TObject);
begin
  if RDbFilter.ShowDialog then
    LoadData;
end;

{ == Установка фильтра данных "по умолчанию" =================================== }
procedure TTreeQueryTemplate.FilterDefaultUpdate(Sender: TObject);
begin
  FilterDefault.Enabled := IsNotWait and RDbFilter.Active;
end;

procedure TTreeQueryTemplate.FilterDefaultExecute(Sender: TObject);
begin
  RDbFilter.ResetFilter;
  LoadData;
end;

{ == Фильтр "по выделению" ===================================================== }
procedure TTreeQueryTemplate.FilterSelectedUpdate(Sender: TObject);
begin
  FilterSelected.Enabled := IsNotWait and RDbFilter.Active
    and RDbEditor.KeyFieldIsPresent
    and RDbEditor.RecordIsSelected;
end;

procedure TTreeQueryTemplate.FilterSelectedExecute(Sender: TObject);
begin
  RDbFilter.KeysWhere := Format(sqlInList, [RDbEditor.KeyFieldName, RDbEditor.GetSelIds]);
  RDbFilter.KeysText := STextFilterSelected;
  LoadData;
end;

{ == Отключить фильтр данных =================================================== }
procedure TTreeQueryTemplate.FilterNoneUpdate(Sender: TObject);
begin
  FilterNone.Enabled := IsNotWait and RDbFilter.Active;
end;

procedure TTreeQueryTemplate.FilterNoneExecute(Sender: TObject);
begin
  RDbFilter.ClearFilter;
  LoadData;
end;

{ == Установка произвольной сортировки данных ================================== }
procedure TTreeQueryTemplate.SortUserUpdate(Sender: TObject);
begin
  SortUser.Enabled := IsNotWait and RDbOrder.Active;
end;

procedure TTreeQueryTemplate.SortUserExecute(Sender: TObject);
begin
  if RDbOrder.ShowDialog then LoadData;
end;

{ == Установка сортировки данных "по умолчанию" ================================ }
procedure TTreeQueryTemplate.SortDefaultUpdate(Sender: TObject);
begin
  SortDefault.Enabled := IsNotWait and RDbOrder.Active;
end;

procedure TTreeQueryTemplate.SortDefaultExecute(Sender: TObject);
begin
  RDbOrder.ResetOrder;
  LoadData;
end;

{ == Обработка меню заголовка таблицы ========================================== }
procedure TTreeQueryTemplate.DbGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  GridPos: TGridCoord;
  GridCol: TColumn;
  MousePos: TPoint;
begin
  inherited;
  MousePos.X := X; MousePos.Y := Y;
  MousePos := DbGrid.ClientToScreen(MousePos);
  GridPos := DbGrid.MouseCoord(X, Y);
  if (GridPos.X > 0) and (GridPos.X <= DbGrid.Columns.Count)
  then GridCol := DbGrid.Columns[GridPos.X - 1]
  else GridCol := nil;
  if Button = mbRight then
  begin
    if GridPos.Y = 0 then
    begin
      fCurrColumn := GridCol;
      TitleGridPopupMenu.Popup(MousePos.X, MousePos.Y);
    end
    else begin
      fCurrColumn := nil;
      if Assigned(GridCol) and Assigned(GridCol.PopupMenu)
      then GridCol.PopupMenu.Popup(MousePos.X, MousePos.Y)
      else PopupMenu.Popup(MousePos.X, MousePos.Y);
    end;
  end
  else fCurrColumn := GridCol;
end;

procedure TTreeQueryTemplate.ColumnLeftUpdate(Sender: TObject);
begin
  ColumnLeft.Enabled := IsNotWait and Assigned(fCurrColumn);
  ColumnLeft.Checked := Assigned(fCurrColumn) and (fCurrColumn.Alignment = taLeftJustify);
end;

procedure TTreeQueryTemplate.ColumnLeftExecute(Sender: TObject);
begin
  StartWait;
  try
    if Assigned(fCurrColumn) then
    begin
      fCurrColumn.Alignment := taLeftJustify;
      fCurrColumn.Title.Alignment := taLeftJustify;
    end;
  finally
    StopWait;
  end;
end;

procedure TTreeQueryTemplate.ColumnCenterUpdate(Sender: TObject);
begin
  ColumnCenter.Enabled := IsNotWait and Assigned(fCurrColumn);
  ColumnCenter.Checked := Assigned(fCurrColumn) and (fCurrColumn.Alignment = taCenter);
end;

procedure TTreeQueryTemplate.ColumnCenterExecute(Sender: TObject);
begin
  StartWait;
  try
    if Assigned(fCurrColumn) then
    begin
      fCurrColumn.Alignment := taCenter;
      fCurrColumn.Title.Alignment := taCenter;
    end;
  finally
    StopWait;
  end;
end;

procedure TTreeQueryTemplate.ColumnRightUpdate(Sender: TObject);
begin
  ColumnRight.Enabled := IsNotWait and Assigned(fCurrColumn);
  ColumnRight.Checked := Assigned(fCurrColumn) and (fCurrColumn.Alignment = taRightJustify);
end;

procedure TTreeQueryTemplate.ColumnRightExecute(Sender: TObject);
begin
  StartWait;
  try
    if Assigned(fCurrColumn) then
    begin
      fCurrColumn.Alignment := taRightJustify;
      fCurrColumn.Title.Alignment := taRightJustify;
    end;
  finally
    StopWait;
  end;
end;

procedure TTreeQueryTemplate.SetCurrOrderAscUpdate(Sender: TObject);
begin
  if Assigned(fCurrColumn) and Assigned(fCurrColumn.Field) 
    and (fCurrColumn.Field.FieldKind = fkData) and RDbOrder.Active then
  begin
    SetCurrOrderAsc.Visible := True;
    SetCurrOrderAsc.Enabled := IsNotWait;
    SetCurrOrderAsc.Checked := RDbOrder.GetFieldDirection(fCurrColumn.Field.FieldName) = odAsc;
  end
  else SetCurrOrderAsc.Visible := False;
end;

procedure TTreeQueryTemplate.SetCurrOrderAscExecute(Sender: TObject);
begin
  if Assigned(fCurrColumn) then
    RDbOrder.SetFieldDirection(fCurrColumn.Field.FieldName, odAsc);
  LoadData;
end;

procedure TTreeQueryTemplate.SetCurrOrderDescUpdate(Sender: TObject);
begin
  if Assigned(fCurrColumn) and Assigned(fCurrColumn.Field)
    and (fCurrColumn.Field.FieldKind = fkData) and RDbOrder.Active then
  begin
    SetCurrOrderDesc.Visible := True;
    SetCurrOrderDesc.Enabled := IsNotWait;
    SetCurrOrderDesc.Checked := RDbOrder.GetFieldDirection(fCurrColumn.Field.FieldName) = odDesc;
  end
  else SetCurrOrderDesc.Visible := False;
end;

procedure TTreeQueryTemplate.SetCurrOrderDescExecute(Sender: TObject);
begin
  if Assigned(fCurrColumn) then
    RDbOrder.SetFieldDirection(fCurrColumn.Field.FieldName, odDesc);
  LoadData;
end;

{ == Редактирование данных ===================================================== }
function TTreeQueryTemplate.DetailInsertEnabled: Boolean;
begin
  Result := RDbEditor.RecordCanInserted;
end;

function TTreeQueryTemplate.DetailCopyEnabled: Boolean;
begin
  Result := RDbEditor.RecordCanCopyed;
end;

function TTreeQueryTemplate.DetailOpenEnabled: Boolean;
begin
  Result := RDbEditor.RecordCanOpened;
end;

function TTreeQueryTemplate.DetailMoveEnabled: Boolean;
begin
  Result := RDbEditor.RecordCanMoved;
end;

function TTreeQueryTemplate.DetailEditEnabled: Boolean;
begin
  Result := RDbEditor.RecordCanEdited;
end;

function TTreeQueryTemplate.DetailDeleteEnabled: Boolean;
begin
  Result := RDbEditor.RecordCanDeleted;
end;

{ == Обработка событий редактора данных (общие) ================================ }
procedure TTreeQueryTemplate.GetNewKey(Sender: TObject; var Value: Integer);
begin
  Value := BaseData.GetNewId(TRDbCustomEditor(Sender).GetObjectName(etView),
    TRDbCustomEditor(Sender).KeyFieldName);
end;

procedure TTreeQueryTemplate.FreeNewKey(Sender: TObject; var Value: Integer);
begin
  BaseData.FreeId(TRDbCustomEditor(Sender).GetObjectName(etView), Value);
end;

procedure TTreeQueryTemplate.SaveToLog(Sender: TObject;
  const EditTag: Integer; const Text: String);
begin
  {$IFDEF RSS}
  AddToDbLog(EditTag, Text);
  {$ENDIF}
end;

procedure TTreeQueryTemplate.DetailAfterProcessRecord(Sender: TObject;
  const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean);
begin
  ShowItemsCount;
end;

procedure TTreeQueryTemplate.DetailAfterProcessRecords(Sender: TObject);
begin
  if TreePanel.Visible then DetailFindOwnerNode;
  ShowItemsCount;
end;

{ == Обработка событий редактора данных (таблица) ============================== }
procedure TTreeQueryTemplate.RDbEditorCreateSetDefault(Sender: TObject;
  const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean);
begin
  inherited;
  RDbUpdater.UpdateDataSetDefault(nil);
end;

procedure TTreeQueryTemplate.DetailBeforeShowEditor(Sender: TObject;
  Editor: TForm; const Mode: TEditMode; const EditTag: Integer;
  var Complete: Boolean);
begin
  if Assigned(Editor) and (Editor is TDbDialogTemplate) then
  begin
    TDbDialogTemplate(Editor).Caption := GetEditorCaption(
      RDbEditor.GetObjectDesc(etView), RDbEditor.DataSet);
    if TDbDialogTemplate(Editor).DataSource.DataSet <> RDbEditor.DataSet then
      TDbDialogTemplate(Editor).DataSource.DataSet := RDbEditor.DataSet;
  end;
end;

procedure TTreeQueryTemplate.DetailAfterPostLogged(Sender: TObject;
  Editor: TForm; OldData, NewData: TRecordData; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  if TreePanel.Visible and not RDbEditor.MultiMode
  and (Mode in [etInsert, etImport, etEdit, etMove, etModify]) then
    DetailFindOwnerNode;
end;

{ == Создать новую запись ====================================================== }
procedure TTreeQueryTemplate.NewRecordUpdate(Sender: TObject);
begin
  NewRecord.Enabled := IsNotWait and DetailInsertEnabled;
end;

procedure TTreeQueryTemplate.NewRecordExecute(Sender: TObject);
begin
  if TreePanel.Visible
  then fLastState := RDbEditor.InsertRecord(TreeView.GetNodeId(TreeView.Selected))
  else fLastState := RDbEditor.InsertRecord;
end;

{ == Копировать запись ========================================================= }
procedure TTreeQueryTemplate.DetailCopy;
begin
  if TreePanel.Visible
  then fLastState := RDbEditor.CopyRecord(TreeView.GetNodeId(TreeView.Selected))
  else fLastState := RDbEditor.CopyRecord;
end;

{ == Свойства выделенной записи ================================================ }
procedure TTreeQueryTemplate.DetailEdit;
begin
  if (RDbEditor.GetSelCount > 1) and (RDbUpdater.Items.Count > 0)
  then fLastState := RDbUpdater.ShowDialog(nil)
  else fLastState := RDbEditor.EditRecord;
end;

procedure TTreeQueryTemplate.PopupMenuPopup(Sender: TObject);
begin
  itemPropRecP.Default := not SelectVisible;
  itemCloseSelectP.Default := SelectVisible;
end;

procedure TTreeQueryTemplate.DbGridDblClick(Sender: TObject);
begin
  {$IFDEF ATTACH}
  if Assigned(fCurrColumn)
  and (fCurrColumn.Field = RDbEditor.DataSet.FindField(fnATTACHS_CNT)) then
  begin
    AttachmentsUpdate(Sender);
    if Attachments.Enabled then
      AttachmentsExecute(Sender);
  end
  else begin
    if SelectVisible then
      CloseSelectExecute(nil)
    else begin
      if Properties.Enabled then
        PropertiesExecute(Sender);
    end;
  end;
  {$ELSE}
  if SelectVisible then
    CloseSelectExecute(nil)
  else begin
    if Properties.Enabled then
      PropertiesExecute(Sender);
  end;
  {$ENDIF}
end;

{ == Перемещение записи в другую группу ======================================== }
function TTreeQueryTemplate.DetailMove(TargetNode: TTreeNode): Boolean;
begin
  Result := RDbEditor.MoveRecords(TreeView.GetNodeId(TargetNode));
end;

{ == Удалить выделенную запись ================================================= }
function TTreeQueryTemplate.TreeNodeDetailIsEmpty(Node: TTreeNode): Boolean;
begin
  Result := RDbEditor.DataSetIsEmply;
end;

procedure TTreeQueryTemplate.DetailDelete;
begin
  fLastState := RDbEditor.DeleteRecord;
end;

{ == Экспорт данных в Microsoft Excel ========================================== }
procedure TTreeQueryTemplate.DataSetExportToExcelUpdate(Sender: TObject);
begin
  DataSetExportToExcel.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TTreeQueryTemplate.DetailGetExcelCopyright(Sender: TObject; var Value: String);
begin
  // Value := SCopyrightsStr;
end;

procedure TTreeQueryTemplate.DetailGetExcelCaption(Sender: TObject; var Value: String);
begin
  Value := Caption;
end;

procedure TTreeQueryTemplate.DetailGetExcelComment(Sender: TObject; var Value: String);
begin
  if TreePanel.Visible and Assigned(TreeView.Selected) then
  begin
    if RDbFilter.Active
    then Value := Format('%s: %s; ', [TreeHeaderPanel.Caption,
      TreeView.Selected.Text]) + RDbFilter.GetTextString
    else Value := Format('%s: %s', [TreeHeaderPanel.Caption,
      TreeView.Selected.Text]);
  end
  else begin
    if RDbFilter.Active
    then Value := RDbFilter.GetTextString
    else Value := EmptyStr;
  end;
end;

procedure TTreeQueryTemplate.DataSetExportToExcelExecute(Sender: TObject);
begin
  RDbEditor.ExportToExcel;
end;

{ == Экспорт в файл ============================================================ }
procedure TTreeQueryTemplate.DataSetExportToFileCsvUpdate(Sender: TObject);
begin
  DataSetExportToFileCsv.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TTreeQueryTemplate.DataSetExportToFileCsvExecute(Sender: TObject);
begin
  RDbEditor.ExportToCsvFile;
end;

{ == Генерация текстового отчета для выделенной записи ========================= }
procedure TTreeQueryTemplate.DataSetCreateDynamicReportUpdate(Sender: TObject);
begin
  DataSetCreateDynamicReport.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TTreeQueryTemplate.DataSetCreateDynamicReportExecute(Sender: TObject);
begin
  PrintCurrentRecord(RDbEditor.DataSet);
end;

{ == Импорт... ================================================================= }
procedure TTreeQueryTemplate.ImportDSUpdate(Sender: TObject);
begin
  ImportDS.Enabled := IsNotWait and RDbEditor.RecordCanImported;
end;

procedure TTreeQueryTemplate.ImportDSExecute(Sender: TObject);
begin
  if TreePanel.Visible
  then ImportExternalData(RDbEditor, TreeView.GetNodeId(TreeView.Selected))
  else ImportExternalData(RDbEditor);
end;

{ == Мультиобработка =========================================================== }
procedure TTreeQueryTemplate.MultiSelectOnOffUpdate(Sender: TObject);
begin
  MultiSelectOnOff.Enabled := IsNotWait and (RDbEditor.DbGrid <> nil);
  MultiSelectOnOff.Checked := RDbEditor.MultiSelect;
end;

procedure TTreeQueryTemplate.MultiSelectOnOffExecute(Sender: TObject);
begin
  StartWait;
  try
    RDbEditor.MultiSelect := not RDbEditor.MultiSelect;
  finally
    StopWait;
  end;
end;

procedure TTreeQueryTemplate.SelectDefaultValuesUpdate(Sender: TObject);
begin
  SelectDefaultValues.Enabled := IsNotWait and (RDbUpdater.Items.Count > 0);
end;

procedure TTreeQueryTemplate.SelectDefaultValuesExecute(Sender: TObject);
begin
  RDbUpdater.ShowDialogDefault;
end;


{ == Редактирование отчетов ==================================================== }
procedure TTreeQueryTemplate.DataSetReportListUpdate(Sender: TObject);
begin
  DataSetReportList.Enabled := IsNotWait and DataSetReportList.Visible
    and RDbEditor.DataSetIsOpened
   {$IFDEF RSS}
    and (Tag > 0);
   {$ENDIF}
end;

function TTreeQueryTemplate.ReportsVar_FilterTree: string;
begin
  if TreePanel.Visible
  then Result := Format('%s: %s; ', [TreeHeaderPanel.Caption, TreeView.Selected.Text])
  else Result := EmptyStr;
end;

function TTreeQueryTemplate.ReportsVar_FilterUser: string;
begin
  if RDbFilter.Active
  then Result := RDbFilter.GetTextString
  else Result := EmptyStr;
end;

function TTreeQueryTemplate.ReportsVar_Variables: string;
begin
  Result := EmptyStr;
end;

function TTreeQueryTemplate.ReportsVar_DefaultPath: string;
begin
  Result := EmptyStr;
end;

procedure TTreeQueryTemplate.DataSetReportListExecute(Sender: TObject);
{$IFDEF FR4}
var
  DsList: TDataSets;
{$ENDIF}
begin
{$IFDEF FR4}
  SetLength(DsList, 0);
  try
    CreateListDataSet(Self, DsList);
    OpenReportsList(BaseData.acDb, Self, DsList,
      ReportsVar_FilterTree, ReportsVar_FilterUser, ReportsVar_Variables,
      ReportsVar_DefaultPath,
      BaseData.orEditReports);
  finally
    SetLength(DsList, 0);
  end;
{$ENDIF}
end;

{ == Вложения ================================================================== }
procedure TTreeQueryTemplate.DetailBeforeDelete(Sender: TObject;
  OldData, NewData: TRecordData; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  inherited;
{$IFDEF ATTACH}
  if Attachments.Visible then
    Complete := Complete and rAttachs_DeleteAttachments(BaseData.acDb, RDbEditor);
{$ENDIF}
end;

procedure TTreeQueryTemplate.AttachmentsUpdate(Sender: TObject);
begin
  Attachments.Enabled := IsNotWait and Attachments.Visible
    and RDbEditor.RecordCanOpened;
end;

procedure TTreeQueryTemplate.AttachmentsExecute(Sender: TObject);
begin
{$IFDEF ATTACH}
  if rAttachs_EditAttachments(BaseData.acDb, RDbEditor, Tag, RDbEditor.RecordCanModified) then
  begin
    SafeRequery(fDsAttachsCnt);
    RDbEditor.DataSet.Refresh;
  end;
{$ENDIF}
end;

{ == Статистика ================================================================ }
procedure TTreeQueryTemplate.DataSetStatisticUpdate(Sender: TObject);
begin
  DataSetStatistic.Visible := RDbEditor.StatisticFields <> EmptyStr;
  DataSetStatistic.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
  // divRepStat.Visible := DataSetStatistic.Visible;
  // divRepStatP.Visible := DataSetStatistic.Visible;
  // divRepStatR.Visible := DataSetStatistic.Visible;
end;

procedure TTreeQueryTemplate.DataSetStatisticExecute(Sender: TObject);
begin
  RDbEditor.ShowStatistic(RDbEditor.StatisticFields, False);
end;

procedure TTreeQueryTemplate.ColumnStatisticUpdate(Sender: TObject);
begin
  ColumnStatistic.Enabled := IsNotWait and Assigned(fCurrColumn) and RDbEditor.DataSetIsNotEmply;
end;

procedure TTreeQueryTemplate.ColumnStatisticExecute(Sender: TObject);
begin
  if Assigned(fCurrColumn) then
    RDbEditor.ShowStatistic(fCurrColumn.FieldName, True);
end;

procedure TTreeQueryTemplate.DateSetGroupingUpdate(Sender: TObject);
begin
  DateSetGrouping.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TTreeQueryTemplate.DateSetGroupingExecute(Sender: TObject);
begin
  RDbEditor.ShowGrouping;
end;

{ == Выделить все ============================================================== }
procedure TTreeQueryTemplate.SelectAllUpdate(Sender: TObject);
begin
  SelectAll.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply and RDbEditor.MultiSelect;
end;

procedure TTreeQueryTemplate.SelectAllExecute(Sender: TObject);
var
  KeyId: Integer;
  KeyBk: TBookmark;
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    DbGrid.SelectedRows.Clear;
    with DbGrid.DataSource.DataSet do
    begin
      DisableControls;
      PositionDS_Store(RDbEditor.DataSet, RDbEditor.KeyFieldName, KeyId, KeyBk);
      try
        First;
        while not EOF do
        begin
          DbGrid.SelectedRows.CurrentRowSelected := True;
          Next;
        end;
      finally
        PositionDS_Restore(RDbEditor.DataSet, RDbEditor.KeyFieldName, KeyId, KeyBk);
        EnableControls;
      end;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeQueryTemplate.InfoPanelPopupMenuPopup(Sender: TObject);
begin
  itemCopyInfoPanel.Enabled := Assigned(InfoPanelPopupMenu.PopupComponent) and
      (((InfoPanelPopupMenu.PopupComponent is TDbText) and (TDbText(InfoPanelPopupMenu.PopupComponent).Caption <> ''))
    or ((InfoPanelPopupMenu.PopupComponent is TDbMemo) and (TDbMemo(InfoPanelPopupMenu.PopupComponent).Text <> ''))
    or ((InfoPanelPopupMenu.PopupComponent is TRDbText) and (TRDbText(InfoPanelPopupMenu.PopupComponent).FieldText <> ''))
    or ((InfoPanelPopupMenu.PopupComponent is TStaticText) and (TStaticText(InfoPanelPopupMenu.PopupComponent).Caption <> ''))
    or ((InfoPanelPopupMenu.PopupComponent is TImage) and (Assigned(TImage(InfoPanelPopupMenu.PopupComponent).Picture.Graphic))));
end;

procedure TTreeQueryTemplate.itemCopyInfoPanelClick(Sender: TObject);
begin
  if Assigned(InfoPanelPopupMenu.PopupComponent) then
  begin
    if InfoPanelPopupMenu.PopupComponent is TDbText then
      PutStringIntoClipBoard(TDbText(InfoPanelPopupMenu.PopupComponent).Caption);
    if InfoPanelPopupMenu.PopupComponent is TDbMemo then
      PutStringIntoClipBoard(TDbMemo(InfoPanelPopupMenu.PopupComponent).Text);
    if InfoPanelPopupMenu.PopupComponent is TRDbText then
      PutStringIntoClipBoard(TRDbText(InfoPanelPopupMenu.PopupComponent).FieldText);
    if InfoPanelPopupMenu.PopupComponent is TStaticText then
      PutStringIntoClipBoard(TStaticText(InfoPanelPopupMenu.PopupComponent).Caption);
    if InfoPanelPopupMenu.PopupComponent is TImage then
      Clipboard.Assign(TImage(InfoPanelPopupMenu.PopupComponent).Picture);
  end;
end;

procedure TTreeQueryTemplate.DataToolButtonClick(Sender: TObject);
var
  fPosition: TPoint;
begin
  fPosition := ToolBar.ClientToScreen(Point(DataToolButton.Left, DataToolButton.Top + DataToolButton.Height));
  DataPopupMenu.Popup(fPosition.X, fPosition.Y);
end;

procedure TTreeQueryTemplate.OpersToolButtonClick(Sender: TObject);
var
  fPosition: TPoint;
begin
  fPosition := ToolBar.ClientToScreen(Point(OpersToolButton.Left, OpersToolButton.Top + OpersToolButton.Height));
  OperationsPopupMenu.Popup(fPosition.X, fPosition.Y);
end;

procedure TTreeQueryTemplate.ReportsToolButtonClick(Sender: TObject);
var
  fPosition: TPoint;
begin
  fPosition := ToolBar.ClientToScreen(Point(ReportsToolButton.Left, ReportsToolButton.Top + ReportsToolButton.Height));
  ReportsPopupMenu.Popup(fPosition.X, fPosition.Y);
end;

end.
