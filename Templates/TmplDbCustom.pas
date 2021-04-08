unit TmplDbCustom;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplData, Menus, ActnList, ImgList, ComCtrls, ToolWin, Grids,
  DBGrids, RDbColorGrid, ExtCtrls, DB, DBCtrls, ADODb, RDbData, RDbPanel, RDbStatus, RDbCustom,
  RDbGridTuner, RDbSearch, DBActns, RDbEditor, RDbCustomSearch, RDbFind,
  RDbUpdater, StdCtrls, Buttons, Tabs;

type
  TDbCustomTemplate = class(TDataTemplate)
    DataPanel: TPanel;
    RDbFind: TRDbSearch;
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
    divDataFind: TMenuItem;
    DataSetInsert: TAction;
    DataSetEdit: TAction;
    DataSetDelete: TAction;
    DbGridSetup: TAction;
    DbGridDefault: TAction;
    itemDataSetInsert: TMenuItem;
    itemDataSetEdit: TMenuItem;
    itemDataSetDelete: TMenuItem;
    divDataGrid: TMenuItem;
    itemDbGridSetup: TMenuItem;
    itemDbGridDefault: TMenuItem;
    ExportToExcel: TAction;
    itemExportToExcel: TMenuItem;
    DbGrid: TRDbStyledGrid;
    InfoPanel: TRDbInfoPanel;
    itemExportToExcelP: TMenuItem;
    itemDbGridDefaultP: TMenuItem;
    itemDbGridSetupP: TMenuItem;
    divEditRefresh: TMenuItem;
    itemDbGridSetupD: TMenuItem;
    itemDbGridDefaultD: TMenuItem;
    itemExportToExcelR: TMenuItem;
    CreateDynamicReport: TAction;
    itemCreateDynamicReport: TMenuItem;
    itemCreateDynamicReportR: TMenuItem;
    itemCreateDynamicReportP: TMenuItem;
    ExportToFileCsv: TAction;
    itemExportToFileR: TMenuItem;
    itemExportToFile: TMenuItem;
    itemExportToFileP: TMenuItem;
    RDbEditor: TRDbExportEditor;
    ImportDS: TAction;
    itemImportDS: TMenuItem;
    divEditImport: TMenuItem;
    MultiSelectOnOff: TAction;
    itemMultiSelectOnOff: TMenuItem;
    divEditMulti: TMenuItem;
    DataSetReportList: TAction;
    itemDataSetReportList: TMenuItem;
    divRepExp: TMenuItem;
    itemDataSetReportListP: TMenuItem;
    divRepExpP: TMenuItem;
    itemDataSetReportListR: TMenuItem;
    divRepExpR: TMenuItem;
    Attachments: TAction;
    divAttach: TMenuItem;
    itemAttachments: TMenuItem;
    itemDataSetFirstP: TMenuItem;
    itemDataSetPriorP: TMenuItem;
    itemDataSetNextP: TMenuItem;
    itemDataSetLastP: TMenuItem;
    divPopupEdit: TMenuItem;
    itemDataSetInsertP: TMenuItem;
    itemDataSetEditP: TMenuItem;
    itemDataSetDeleteP: TMenuItem;
    divPopupMulti: TMenuItem;
    itemMultiSelectOnOffP: TMenuItem;
    divAttachP: TMenuItem;
    itemAttachmentsP: TMenuItem;
    ColumnLeft: TAction;
    ColumnCenter: TAction;
    ColumnRight: TAction;
    TitleGridPopupMenu: TPopupMenu;
    menuAlgn: TMenuItem;
    itemColumnLeft: TMenuItem;
    itemColumnCenter: TMenuItem;
    itemColumnRight: TMenuItem;
    itemDbGridSetupT: TMenuItem;
    itemDbGridDefaultT: TMenuItem;
    DataSetCopy: TAction;
    itemDataSetCopy: TMenuItem;
    itemitemDataSetCopyP: TMenuItem;
    FindColumn: TAction;
    itemFindColumn: TMenuItem;
    divGridTitleFind: TMenuItem;
    DataSetStatistic: TAction;
    ColumnStatistic: TAction;
    divGridTitleStat: TMenuItem;
    itemColumnStatisticT: TMenuItem;
    divRepStat: TMenuItem;
    divRepStatP: TMenuItem;
    divRepStatR: TMenuItem;
    itemDataSetStatisticR: TMenuItem;
    itemDataSetStatisticP: TMenuItem;
    itemDataSetStatistic: TMenuItem;
    SelectAll: TAction;
    itemSelectAll: TMenuItem;
    itemSelectAllP: TMenuItem;
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
    TabViews: TTabSet;
    ViewsPopupMenu: TPopupMenu;
    itemDbGridSetupV: TMenuItem;
    DateSetGrouping: TAction;
    itemDateSetGrouping: TMenuItem;
    itemDateSetGroupingP: TMenuItem;
    itemDateSetGroupingR: TMenuItem;
    itemDateSetGroupingT: TMenuItem;
    procedure DataSetInsertUpdate(Sender: TObject);
    procedure DataSetInsertExecute(Sender: TObject);  virtual;
    procedure DataSetEditUpdate(Sender: TObject);
    procedure DataSetEditExecute(Sender: TObject); virtual;
    procedure DataSetDeleteUpdate(Sender: TObject);
    procedure DataSetDeleteExecute(Sender: TObject); virtual;
    procedure DbGridSetupUpdate(Sender: TObject);
    procedure DbGridSetupExecute(Sender: TObject);
    procedure DbGridDefaultUpdate(Sender: TObject);
    procedure DbGridDefaultExecute(Sender: TObject);
    procedure ExportToExcelUpdate(Sender: TObject);
    procedure ExportToExcelExecute(Sender: TObject);
    procedure DbGridDblClick(Sender: TObject);
    procedure RefreshUpdate(Sender: TObject);
    procedure RefreshExecute(Sender: TObject);
    procedure FindUpdate(Sender: TObject);
    procedure FindExecute(Sender: TObject);
    procedure CreateDynamicReportUpdate(Sender: TObject);
    procedure CreateDynamicReportExecute(Sender: TObject);
    procedure ExportToFileCsvUpdate(Sender: TObject);
    procedure ExportToFileCsvExecute(Sender: TObject);
    procedure RDbEditorSaveToLog(Sender: TObject; const EditTag: Integer; const Text: string);
    procedure RDbEditorGetNewKey(Sender: TObject; var Value: Integer);
    procedure RDbEditorFreeNewKey(Sender: TObject; var Value: Integer);
    procedure RDbEditorBeforeShowEditor(Sender: TObject; Editor: TForm;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure RDbEditorGetExcelCopyright(Sender: TObject; var Value: string);
    procedure ImportDSUpdate(Sender: TObject);
    procedure ImportDSExecute(Sender: TObject);
    procedure MultiSelectOnOffUpdate(Sender: TObject);
    procedure MultiSelectOnOffExecute(Sender: TObject);
    procedure RDbEditorGetExcelCaption(Sender: TObject; var Value: String);
    procedure DataSetReportListUpdate(Sender: TObject);
    procedure DataSetReportListExecute(Sender: TObject);
    procedure RDbEditorBeforeDelete(Sender: TObject; OldData,
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
    procedure DbGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DataSetCopyUpdate(Sender: TObject);
    procedure DataSetCopyExecute(Sender: TObject);
    procedure FindColumnUpdate(Sender: TObject);
    procedure FindColumnExecute(Sender: TObject);
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
    procedure RDbGridTunerViewChange(Sender: TObject);
    procedure TabViewsChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure DateSetGroupingUpdate(Sender: TObject);
    procedure DateSetGroupingExecute(Sender: TObject);
    procedure DbGridColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
  private
  protected
    fCurrColumn: TColumn;
    fLastState: Boolean;
    {$IFDEF ATTACH}
    fDsAttachsCnt: TAdoQuery;
    {$ENDIF}
    function  SelectVisible: Boolean; override;
    function  SelectEnabled: Boolean; override;
    procedure InitFormVariables; override;
    procedure InitDataComponents; override;
    procedure DoneDataComponents; override;
    {$IFDEF ATTACH}
    procedure CreateAttachmentsField; virtual;
    procedure LoadAttachments; override;
    {$ENDIF}
    procedure ColumnFlagsReset; virtual;
    procedure ColumnFlagsUpdate; virtual;
    procedure LoadDataTry; override;
    procedure LoadDataFinally; override;
    procedure CloseDataSets; override;
    procedure SetSelectionBefore; override;
    procedure SetSelection(const SelectedID: Integer); override;
    procedure SetSelectionAfter; override;
    function  GetSelection: Integer; override;
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

uses
  RVclUtils, RDialogs, RMsgRu, RDbState, RDbConst, RDbUtils, RDbPrint, RDbImport,
  RDbOpenDS, RDbText, RExHandlers, RClipBrd, ClipBrd, RxStrUtils, StrUtils,  
  {$IFDEF RSS} RDbLog, RRssConst, {$ENDIF}
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  {$IFDEF FR4} ReportsForm, {$ENDIF}
  {$IFDEF ATTACH} RDbAttachs, {$ENDIF}
  BaseDbUnit, TmplDbDialog;

{$R *.dfm}

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TDbCustomTemplate.SetStyle;
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
procedure TDbCustomTemplate.InitFormVariables;
begin
  {$IFDEF ATTACH}
  fDsAttachsCnt := nil;
  {$ENDIF}

  inherited InitFormVariables;

  FindPanel.Height := edFastFind.Height + 10;
end;

procedure TDbCustomTemplate.FindPanelResize(Sender: TObject);
begin
  edFastFind.Width := FindPanel.ClientWidth - btnFastFind.Width - 10;

  btnFastFind.Left := FindPanel.ClientWidth - btnFastFind.Width - 4;
  btnFastFind.Height := edFastFind.Height;
end;

procedure TDbCustomTemplate.InitDataComponents;
begin
  try
    inherited;
  finally
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
    divAttachP.Visible := Attachments.Visible;
    {$ELSE}
    Attachments.Visible := False;
    divAttach.Visible := False;
    divAttachP.Visible := False;
    {$ENDIF}
    try
      ShowInStatusBar(Format(SMsgLoadDataFormEx, [SPrmFind]));
      if Assigned(RDbFind.DataSet) then
        RDbFind.Open;
      if Assigned(RDbLocate.DataSet) then
        RDbLocate.Open;
    finally
      ShowInStatusBar(Format(SMsgLoadDataFormEx, [SPrmGridTuner]));
      {$IFDEF ATTACH}
      CreateAttachmentsField;
      {$ENDIF}
      if Assigned(RDbGridTuner.DbGrid) then
        RDbGridTuner.Open;
    end;
  end;
end;

{ == Деактивация Db-компонентов ================================================ }
procedure TDbCustomTemplate.DoneDataComponents;
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
      ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmFind]));
      if RDbFind.Active then RDbFind.Close;
      if RDbLocate.Active then RDbLocate.Close;
    finally
      ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmGridTuner]));
      if RDbGridTuner.Active then RDbGridTuner.Close;
    end;
  end;
end;

{$IFDEF ATTACH}
{ == Прикрепленные файлы ======================================================= }
procedure TDbCustomTemplate.CreateAttachmentsField;
begin
  if Attachments.Visible and not RDbEditor.DataSetIsOpened then
    rAttachs_CreateAttachsFld(RDbEditor);
end;

procedure TDbCustomTemplate.LoadAttachments;
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
procedure TDbCustomTemplate.ColumnFlagsReset;
begin
  DbGrid.ColumnFlagsClear;
end;

procedure TDbCustomTemplate.ColumnFlagsUpdate;
begin
  if Assigned(RDbEditor.DataSet) and (RDbEditor.DataSet is TAdoTable) then
  begin
    if (TAdoTable(RDbEditor.DataSet).IndexFieldNames = '')
    then DbGrid.FieldFlagSet(RDbEditor.KeyFieldName, ffSortAsc)
    else DbGrid.FieldFlagSort(TAdoTable(RDbEditor.DataSet).IndexFieldNames);
  end
  else DbGrid.FieldFlagSet(RDbEditor.KeyFieldName, ffSortAsc);
end;

{ == Загрузка данных =========================================================== }
procedure TDbCustomTemplate.LoadDataTry;
begin
  inherited LoadDataTry;
  ColumnFlagsReset;
end;

procedure TDbCustomTemplate.LoadDataFinally;
begin
  inherited LoadDataFinally;
  ColumnFlagsUpdate;
end;

{ == Закрываем открытые наборы данных ========================================== }
procedure TDbCustomTemplate.CloseDataSets;
begin
  inherited;

  {$IFDEF ATTACH}
  if Assigned(fDsAttachsCnt) then
    fDsAttachsCnt.Free;
  {$ENDIF}
end;

{ == Выбрать запись и закрыть окно ============================================= }
function TDbCustomTemplate.SelectVisible: Boolean;
begin
  Result := inherited SelectVisible and RDbEditor.KeyFieldIsPresent;
end;

function TDbCustomTemplate.SelectEnabled: Boolean;
begin
  Result := IsNotWait and RDbEditor.KeyFieldIsPresent and (RDbEditor.SelCount = 1);
end;

{ == Ввод и выборка выбранного значения из формы =============================== }
procedure TDbCustomTemplate.SetSelectionBefore;
begin
  RDbEditor.DataSet.DisableControls;
end;

procedure TDbCustomTemplate.SetSelection(const SelectedID: Integer);
begin
  RDbEditor.LocateKey(SelectedID);
end;

procedure TDbCustomTemplate.SetSelectionAfter;
begin
  RDbEditor.DataSet.EnableControls;
end;

function TDbCustomTemplate.GetSelection: Integer;
begin
  Result := RDbEditor.GetKeyValue;
end;

{ == Протоколирование событий ================================================== }
procedure TDbCustomTemplate.RDbEditorSaveToLog(Sender: TObject;
  const EditTag: Integer; const Text: String);
begin
  {$IFDEF RSS}
  AddToDbLog(EditTag, Text);
  {$ENDIF}
end;

{ == Обработка событий редатирования =========================================== }
procedure TDbCustomTemplate.RDbEditorGetNewKey(Sender: TObject; var Value: Integer);
begin
  with TRDbEditor(Sender) do
    Value := BaseData.GetNewId(GetObjectName(etView), KeyFieldName);
end;

procedure TDbCustomTemplate.RDbEditorFreeNewKey(Sender: TObject; var Value: Integer);
begin
  with TRDbEditor(Sender) do
    BaseData.FreeId(GetObjectName(etView), Value);
end;

procedure TDbCustomTemplate.RDbEditorCreateSetDefault(Sender: TObject;
  const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean);
begin
  inherited;
  RDbUpdater.UpdateDataSetDefault(nil);
end;

procedure TDbCustomTemplate.RDbEditorBeforeShowEditor(Sender: TObject;
  Editor: TForm; const Mode: TEditMode; const EditTag: Integer;
  var Complete: Boolean);
begin
  if Editor is TDbDialogTemplate then
  begin
    TDbDialogTemplate(Editor).Caption := GetEditorCaption(
      RDbEditor.GetObjectDesc(etView), RDbEditor.DataSet);
    if TDbDialogTemplate(Editor).DataSource.DataSet <> RDbEditor.DataSet then
      TDbDialogTemplate(Editor).DataSource.DataSet := RDbEditor.DataSet;
  end;
end;

procedure TDbCustomTemplate.RDbEditorGetExcelCopyright(Sender: TObject; var Value: String);
begin
  // Value := SCopyrightsStr;
end;

procedure TDbCustomTemplate.RDbEditorGetExcelCaption(Sender: TObject; var Value: String);
begin
  Value := Caption;
end;

{ == Создать новую запись ====================================================== }
procedure TDbCustomTemplate.DataSetInsertUpdate(Sender: TObject);
begin
  DataSetInsert.Enabled := IsNotWait and RDbEditor.RecordCanInserted;
end;

procedure TDbCustomTemplate.DataSetInsertExecute(Sender: TObject);
begin
  fLastState := RDbEditor.InsertRecord;
end;

{ == Копировать запись ========================================================= }
procedure TDbCustomTemplate.DataSetCopyUpdate(Sender: TObject);
begin
  DataSetCopy.Enabled := IsNotWait and RDbEditor.RecordCanCopyed;
end;

procedure TDbCustomTemplate.DataSetCopyExecute(Sender: TObject);
begin
  fLastState := RDbEditor.CopyRecord;
end;

{ == Свойства выделенной записи ================================================ }
procedure TDbCustomTemplate.DataSetEditUpdate(Sender: TObject);
begin
  DataSetEdit.Enabled := IsNotWait and RDbEditor.RecordCanOpened;
end;

procedure TDbCustomTemplate.DataSetEditExecute(Sender: TObject);
begin
  if (RDbEditor.GetSelCount > 1) and (RDbUpdater.Items.Count > 0)
  then fLastState := RDbUpdater.ShowDialog(nil)
  else fLastState := RDbEditor.EditRecord;
end;

procedure TDbCustomTemplate.DbGridDblClick(Sender: TObject);
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
    if IsNotWait and RDbEditor.RecordCanOpened then
      DataSetEditExecute(Sender);
  end;
  {$ELSE}
  if IsNotWait and RDbEditor.RecordCanOpened then
    DataSetEditExecute(Sender);
  {$ENDIF}
end;

{ == Удалить выделенную запись ================================================= }
procedure TDbCustomTemplate.DataSetDeleteUpdate(Sender: TObject);
begin
  DataSetDelete.Enabled := IsNotWait and RDbEditor.RecordCanDeleted;
end;

procedure TDbCustomTemplate.DataSetDeleteExecute(Sender: TObject);
begin
  fLastState := RDbEditor.DeleteRecord(True);
end;

{ == Обновить данные =========================================================== }
procedure TDbCustomTemplate.RefreshUpdate(Sender: TObject);
begin
  Refresh.Enabled := IsNotWait and Assigned(RDbEditor.DataSet);
end;

procedure TDbCustomTemplate.RefreshExecute(Sender: TObject);
begin
  BaseData.OpenDataSetMsg(RDbEditor.DataSet);
  RDbEditor.RefreshSelection;
end;

{ == Поиск записи ============================================================== }
procedure TDbCustomTemplate.FindUpdate(Sender: TObject);
begin
  Find.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply and RDbFind.Active;
end;

procedure TDbCustomTemplate.FindExecute(Sender: TObject);
begin
  RDbFind.ShowDialog;
end;

procedure TDbCustomTemplate.FindColumnUpdate(Sender: TObject);
begin
  FindColumn.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply
    and RDbFind.Active and Assigned(fCurrColumn);
end;

procedure TDbCustomTemplate.FindColumnExecute(Sender: TObject);
begin
  RDbFind.ShowDialogEx(fCurrColumn.FieldName, EmptyStr, False);
end;

{ == Перейти к указанной записи ================================================ }
procedure TDbCustomTemplate.DbLocateUpdate(Sender: TObject);
begin
  DbLocate.Visible := RDbLocate.Active;
  DbLocate.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TDbCustomTemplate.DbLocateExecute(Sender: TObject);
begin
  RDbLocate.ShowDialog;
end;

{ == Быстрый поиск ============================================================= }
procedure TDbCustomTemplate.FindFastUpdate(Sender: TObject);
begin
  FindFast.Enabled := IsNotWait and RDbEditor.DataSetIsOpened;
end;

procedure TDbCustomTemplate.FindFastExecute(Sender: TObject);
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

procedure TDbCustomTemplate.edFastFindKeyPress(Sender: TObject; var Key: Char);
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

procedure TDbCustomTemplate.edFastFindEnter(Sender: TObject);
begin
  CloseSelect.ShortCut := 0;
  DataSetFirst.ShortCut := 0;
  DataSetLast.ShortCut := 0;
end;

procedure TDbCustomTemplate.edFastFindExit(Sender: TObject);
begin
  CloseSelect.ShortCut := 13;
  DataSetFirst.ShortCut := 36;
  DataSetLast.ShortCut := 35;
end;

{ == Настройка отображения столбцов в таблице ================================== }
procedure TDbCustomTemplate.DbGridSetupUpdate(Sender: TObject);
begin
  DbGridSetup.Enabled := IsNotWait and Assigned(RDbEditor.DataSet)
    and RDbGridTuner.Active;
end;

procedure TDbCustomTemplate.DbGridSetupExecute(Sender: TObject);
begin
  if RDbGridTuner.ShowDialog then
  begin
    fCurrColumn := nil;
    ColumnFlagsReset;
    ColumnFlagsUpdate;
  end;
end;

procedure TDbCustomTemplate.RDbGridTunerViewChange(Sender: TObject);
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

procedure TDbCustomTemplate.TabViewsChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
begin
  RDbGridTuner.ViewActive := NewTab;
  ColumnFlagsReset;
  ColumnFlagsUpdate;
end;

procedure TDbCustomTemplate.DbGridColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
begin
  ColumnFlagsReset;
  ColumnFlagsUpdate;
end;

{ == Установить столбцы "по умолчанию" (все видимые) =========================== }
procedure TDbCustomTemplate.DbGridDefaultUpdate(Sender: TObject);
begin
  DbGridDefault.Enabled := IsNotWait and Assigned(RDbEditor.DataSet)
    and RDbGridTuner.Active;
end;

procedure TDbCustomTemplate.DbGridDefaultExecute(Sender: TObject);
begin
  if QueryBoxStdNY(SQueryResetColumns) = ID_YES then
  begin
    RDbGridTuner.ResetColumns;
    fCurrColumn := nil;
    ColumnFlagsReset;
    ColumnFlagsUpdate;
  end;
end;

{ == Экспорт данных в Microsoft Excel ========================================== }
procedure TDbCustomTemplate.ExportToExcelUpdate(Sender: TObject);
begin
  ExportToExcel.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TDbCustomTemplate.ExportToExcelExecute(Sender: TObject);
begin
  RDbEditor.ExportToExcel;
end;

{ == Экспорт в файл ============================================================ }
procedure TDbCustomTemplate.ExportToFileCsvUpdate(Sender: TObject);
begin
  ExportToFileCsv.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TDbCustomTemplate.ExportToFileCsvExecute(Sender: TObject);
begin
  RDbEditor.ExportToCsvFile;
end;

{ == Генерация текстового отчета для выделенной записи ========================= }
procedure TDbCustomTemplate.CreateDynamicReportUpdate(Sender: TObject);
begin
  CreateDynamicReport.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TDbCustomTemplate.CreateDynamicReportExecute(Sender: TObject);
begin
  PrintCurrentRecord(RDbEditor.DataSet);
end;

{ == Мастер импорта... ========================================================= }
procedure TDbCustomTemplate.ImportDSUpdate(Sender: TObject);
begin
  ImportDS.Enabled := IsNotWait and RDbEditor.RecordCanImported;
end;

procedure TDbCustomTemplate.ImportDSExecute(Sender: TObject);
begin
  ImportExternalData(RDbEditor);
end;

{ == Мультиобработка =========================================================== }
procedure TDbCustomTemplate.MultiSelectOnOffUpdate(Sender: TObject);
begin
  MultiSelectOnOff.Enabled := IsNotWait and (RDbEditor.DbGrid <> nil);
  MultiSelectOnOff.Checked := RDbEditor.MultiSelect;
end;

procedure TDbCustomTemplate.MultiSelectOnOffExecute(Sender: TObject);
begin
  StartWait;
  try
    RDbEditor.MultiSelect := not RDbEditor.MultiSelect;
  finally
    StopWait;
  end;
end;

procedure TDbCustomTemplate.SelectDefaultValuesUpdate(Sender: TObject);
begin
  SelectDefaultValues.Enabled := IsNotWait and (RDbUpdater.Items.Count > 0);
end;

procedure TDbCustomTemplate.SelectDefaultValuesExecute(Sender: TObject);
begin
  RDbUpdater.ShowDialogDefault;
end;

{ == Настраиваемые отчеты ====================================================== }
procedure TDbCustomTemplate.DataSetReportListUpdate(Sender: TObject);
begin
  DataSetReportList.Enabled := IsNotWait and DataSetReportList.Visible
    and RDbEditor.DataSetIsOpened
   {$IFDEF RSS}
    and (Tag > 0);
   {$ENDIF}
end;

function TDbCustomTemplate.ReportsVar_FilterTree: string;
begin
  Result := EmptyStr;
end;

function TDbCustomTemplate.ReportsVar_FilterUser: string;
begin
  Result := EmptyStr;
end;

function TDbCustomTemplate.ReportsVar_Variables: string;
begin
  Result := EmptyStr;
end;

function TDbCustomTemplate.ReportsVar_DefaultPath: string;
begin
  Result := EmptyStr;
end;

procedure TDbCustomTemplate.DataSetReportListExecute(Sender: TObject);
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
procedure TDbCustomTemplate.RDbEditorBeforeDelete(Sender: TObject; OldData,
  NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
  var Complete: Boolean);
begin
  inherited;
{$IFDEF ATTACH}
  if Attachments.Visible then
    Complete := Complete and rAttachs_DeleteAttachments(BaseData.acDb, RDbEditor);
{$ENDIF}
end;

procedure TDbCustomTemplate.AttachmentsUpdate(Sender: TObject);
begin
  Attachments.Enabled := IsNotWait and Attachments.Visible
    and RDbEditor.RecordCanOpened;
end;

procedure TDbCustomTemplate.AttachmentsExecute(Sender: TObject);
begin
{$IFDEF ATTACH}
  if rAttachs_EditAttachments(BaseData.acDb, RDbEditor, Tag, RDbEditor.RecordCanModified) then
  begin
    SafeRequery(fDsAttachsCnt);
    RDbEditor.DataSet.Refresh;
  end;
{$ENDIF}
end;

{ == Обработка меню заголовка таблицы ========================================== }

procedure TDbCustomTemplate.DbGridMouseUp(Sender: TObject;
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

procedure TDbCustomTemplate.ColumnLeftUpdate(Sender: TObject);
begin
  ColumnLeft.Enabled := IsNotWait and Assigned(fCurrColumn);
  ColumnLeft.Checked := Assigned(fCurrColumn) and (fCurrColumn.Alignment = taLeftJustify);
end;

procedure TDbCustomTemplate.ColumnLeftExecute(Sender: TObject);
begin
  StartWait;
  try
    fCurrColumn.Alignment := taLeftJustify;
    fCurrColumn.Title.Alignment := taLeftJustify;
  finally
    StopWait;
  end;
end;

procedure TDbCustomTemplate.ColumnCenterUpdate(Sender: TObject);
begin
  ColumnCenter.Enabled := IsNotWait and Assigned(fCurrColumn);
  ColumnCenter.Checked := Assigned(fCurrColumn) and (fCurrColumn.Alignment = taCenter);
end;

procedure TDbCustomTemplate.ColumnCenterExecute(Sender: TObject);
begin
  StartWait;
  try
    fCurrColumn.Alignment := taCenter;
    fCurrColumn.Title.Alignment := taCenter;
  finally
    StopWait;
  end;
end;

procedure TDbCustomTemplate.ColumnRightUpdate(Sender: TObject);
begin
  ColumnRight.Enabled := IsNotWait and Assigned(fCurrColumn);
  ColumnRight.Checked := Assigned(fCurrColumn) and (fCurrColumn.Alignment = taRightJustify);
end;

procedure TDbCustomTemplate.ColumnRightExecute(Sender: TObject);
begin
  StartWait;
  try
    fCurrColumn.Alignment := taRightJustify;
    fCurrColumn.Title.Alignment := taRightJustify;
  finally
    StopWait;
  end;
end;

{ == Статистика ================================================================ }
procedure TDbCustomTemplate.DataSetStatisticUpdate(Sender: TObject);
begin
  DataSetStatistic.Visible := RDbEditor.StatisticFields <> EmptyStr;
  DataSetStatistic.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
  // divRepStat.Visible := DataSetStatistic.Visible;
  // divRepStatP.Visible := DataSetStatistic.Visible;
  // divRepStatR.Visible := DataSetStatistic.Visible;
end;

procedure TDbCustomTemplate.DataSetStatisticExecute(Sender: TObject);
begin
  RDbEditor.ShowStatistic(RDbEditor.StatisticFields, False);
end;

procedure TDbCustomTemplate.ColumnStatisticUpdate(Sender: TObject);
begin
  ColumnStatistic.Enabled := IsNotWait and Assigned(fCurrColumn) and RDbEditor.DataSetIsNotEmply;
end;

procedure TDbCustomTemplate.ColumnStatisticExecute(Sender: TObject);
begin
  RDbEditor.ShowStatistic(fCurrColumn.FieldName, True);
end;

procedure TDbCustomTemplate.DateSetGroupingUpdate(Sender: TObject);
begin
  DateSetGrouping.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TDbCustomTemplate.DateSetGroupingExecute(Sender: TObject);
begin
  RDbEditor.ShowGrouping;
end;

{ == Выделить все ============================================================== }
procedure TDbCustomTemplate.SelectAllUpdate(Sender: TObject);
begin
  SelectAll.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply and RDbEditor.MultiSelect;
end;

procedure TDbCustomTemplate.SelectAllExecute(Sender: TObject);
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

procedure TDbCustomTemplate.InfoPanelPopupMenuPopup(Sender: TObject);
begin
  itemCopyInfoPanel.Enabled := Assigned(InfoPanelPopupMenu.PopupComponent) and
      (((InfoPanelPopupMenu.PopupComponent is TDbText) and (TDbText(InfoPanelPopupMenu.PopupComponent).Caption <> ''))
    or ((InfoPanelPopupMenu.PopupComponent is TDbMemo) and (TDbMemo(InfoPanelPopupMenu.PopupComponent).Text <> ''))
    or ((InfoPanelPopupMenu.PopupComponent is TRDbText) and (TRDbText(InfoPanelPopupMenu.PopupComponent).FieldText <> ''))
    or ((InfoPanelPopupMenu.PopupComponent is TStaticText) and (TStaticText(InfoPanelPopupMenu.PopupComponent).Caption <> ''))
    or ((InfoPanelPopupMenu.PopupComponent is TImage) and (Assigned(TImage(InfoPanelPopupMenu.PopupComponent).Picture.Graphic))));
end;

procedure TDbCustomTemplate.itemCopyInfoPanelClick(Sender: TObject);
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

end.
