unit DbFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Tabs, Grids, DBGrids, RDbColorGrid, StdCtrls, Buttons, ExtCtrls,
  ActnList, ComCtrls, RDbStatus, RDbUpdater, RDbCustom, RDbGridTuner,
  RDbOrder, RDbFilter, RDbSearch, RDbCustomSearch, RDbFind, DB, AdoDB,
  RDbData, RDbEditor, Menus, DBActns, ToolWin, RDbPanel;

type
  TOpenDataSetProc = procedure (Sender: TDataSet; var Complete: Boolean) of object;

  TFrameDb = class(TFrame)
    DbGrid: TRDbStyledGrid;
    DbActions: TActionList;
    FindPanel: TPanel;
    edFastFilter: TEdit;
    btnFastFilter: TBitBtn;
    DbTabViews: TTabSet;
    actDbAttachments: TAction;
    RDbEditor: TRDbExportEditor;
    RDbLocate: TRDbFind;
    RDbSearch: TRDbSearch;
    RDbGridTuner: TRDbGridTuner;
    RDbUpdater: TRDbUpdater;
    RDbFilterStatus: TRDbFilterStatus;
    GridPopupMenu: TPopupMenu;
    actDbFirst: TDataSetFirst;
    actDbPrior: TDataSetPrior;
    actDbNext: TDataSetNext;
    actDbLast: TDataSetLast;
    actDbInsert: TAction;
    actDbClone: TAction;
    actDbEdit: TAction;
    actDbDelete: TAction;
    actDbImport: TAction;
    actDbRefresh: TAction;
    actDbMultiSelectOnOff: TAction;
    actDbSetDefaultValues: TAction;
    actDbFind: TAction;
    actDbFindColumn: TAction;
    actDbLocate: TAction;
    actDbFastFilter: TAction;
    actDbGridTune: TAction;
    actDbGridDefault: TAction;
    actDbColumnLeft: TAction;
    actDbColumnCenter: TAction;
    actDbColumnRight: TAction;
    actDbSelectAll: TAction;
    actDbExportToExcel: TAction;
    actDbExportToFileCsv: TAction;
    actDbCreateDynamicReport: TAction;
    actDbDataSetStatistic: TAction;
    actDbColumnStatistic: TAction;
    actDbDateSetGrouping: TAction;
    TitlePopupMenu: TPopupMenu;
    itemDbFindColumnT: TMenuItem;
    divMenuTitleFind: TMenuItem;
    menuDbColumnAlignT: TMenuItem;
    itemDbColumnRightT: TMenuItem;
    itemDbColumnCenterT: TMenuItem;
    itemDbColumnLeftT: TMenuItem;
    itemDbGridTuneT: TMenuItem;
    divMenuTitleFilter: TMenuItem;
    itemDbColumnStatistic: TMenuItem;
    itemDbDateSetGrouping: TMenuItem;
    itemDbFirstP: TMenuItem;
    itemDbPriorP: TMenuItem;
    itemDbNextP: TMenuItem;
    itemDbLastP: TMenuItem;
    itemDbInsertP: TMenuItem;
    itemDbCloneP: TMenuItem;
    itemDbEditP: TMenuItem;
    itemDbDeleteP: TMenuItem;
    divGridEditP: TMenuItem;
    itemDbFindP: TMenuItem;
    itemDbLocateP: TMenuItem;
    divGridNavP: TMenuItem;
    itemDbAttachmentsP: TMenuItem;
    divGridAttachmentsP: TMenuItem;
    menuGridSetupP: TMenuItem;
    itemDbGridDefaultP: TMenuItem;
    itemDbGridTuneP: TMenuItem;
    menuOperationsP: TMenuItem;
    itemDbCreateDynamicReportP: TMenuItem;
    itemactDbExportToFileCsvP: TMenuItem;
    itemDbExportToExcelP: TMenuItem;
    divGridDataP: TMenuItem;
    itemDbMultiSelectOnOffP: TMenuItem;
    divGridMultiP: TMenuItem;
    itemDbRefreshP: TMenuItem;
    menuDbNavigationP: TMenuItem;
    menuGroupP: TMenuItem;
    itemDbDateSetGroupingP: TMenuItem;
    itemactDbDataSetStatisticP: TMenuItem;
    itemDbSelectAllP: TMenuItem;
    RDbFilter: TRDbFilter;
    RDbOrder: TRDbOrder;
    actDbFilterCustom: TAction;
    actDbFilterDefault: TAction;
    actDbFilterSelected: TAction;
    actDbFilterNone: TAction;
    actDbSortCustom: TAction;
    actDbSortDefault: TAction;
    actDbSortColumnAsc: TAction;
    actDbSortColumnDesc: TAction;
    menuDataFilterP: TMenuItem;
    itemDbFilterCustomP: TMenuItem;
    itemDbFilterDefaultP: TMenuItem;
    itemDbFilterSelectedP: TMenuItem;
    itemDbFilterNoneP: TMenuItem;
    menuDataSortP: TMenuItem;
    itemDbSortCustomP: TMenuItem;
    itemDbSortDefaultP: TMenuItem;
    divMenuTitleSort: TMenuItem;
    itemDbSortColumnAscT: TMenuItem;
    itemDbSortColumnDescT: TMenuItem;
    itemDbFilterNoneT: TMenuItem;
    itemDbFilterSelectedT: TMenuItem;
    itemDbFilterDefaultT: TMenuItem;
    itemDbFilterCustomT: TMenuItem;
    itemDbSortCustomT: TMenuItem;
    divMenuTitleView: TMenuItem;
    menuReportsP: TMenuItem;
    itemDbSetDefaultValuesP: TMenuItem;
    ViewsPopupMenu: TPopupMenu;
    itemDbGridTuneV: TMenuItem;
    procedure FindPanelResize(Sender: TObject);
    procedure DbSaveToLog(Sender: TObject; const EditTag: Integer;
      const Text: String);
    procedure DbGetNewKey(Sender: TObject; var Value: Integer);
    procedure DbFreeNewKey(Sender: TObject; var Value: Integer);
    procedure DbCreateSetDefault(Sender: TObject;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure DbBeforeShowEditor(Sender: TObject; Editor: TForm;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure actDbInsertUpdate(Sender: TObject);
    procedure actDbInsertExecute(Sender: TObject);
    procedure actDbCloneUpdate(Sender: TObject);
    procedure actDbCloneExecute(Sender: TObject);
    procedure actDbEditUpdate(Sender: TObject);
    procedure actDbEditExecute(Sender: TObject);
    procedure DbGridDblClick(Sender: TObject);
    procedure actDbDeleteUpdate(Sender: TObject);
    procedure actDbDeleteExecute(Sender: TObject);
    procedure actDbRefreshUpdate(Sender: TObject);
    procedure actDbRefreshExecute(Sender: TObject);
    procedure actDbFindUpdate(Sender: TObject);
    procedure actDbFindExecute(Sender: TObject);
    procedure actDbFindColumnUpdate(Sender: TObject);
    procedure actDbFindColumnExecute(Sender: TObject);
    procedure actDbLocateUpdate(Sender: TObject);
    procedure actDbLocateExecute(Sender: TObject);
    procedure actDbFastFilterUpdate(Sender: TObject);
    procedure actDbFastFilterExecute(Sender: TObject);
    procedure edFastFilterEnter(Sender: TObject);
    procedure edFastFilterExit(Sender: TObject);
    procedure edFastFilterKeyPress(Sender: TObject; var Key: Char);
    procedure actDbGridTuneUpdate(Sender: TObject);
    procedure actDbGridTuneExecute(Sender: TObject);
    procedure DbTabViewsChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure RDbGridTunerViewChange(Sender: TObject);
    procedure DbGridColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    procedure actDbGridDefaultUpdate(Sender: TObject);
    procedure actDbGridDefaultExecute(Sender: TObject);
    procedure actDbExportToExcelUpdate(Sender: TObject);
    procedure actDbExportToExcelExecute(Sender: TObject);
    procedure actDbExportToFileCsvUpdate(Sender: TObject);
    procedure actDbExportToFileCsvExecute(Sender: TObject);
    procedure actDbCreateDynamicReportUpdate(Sender: TObject);
    procedure actDbCreateDynamicReportExecute(Sender: TObject);
    procedure actDbImportUpdate(Sender: TObject);
    procedure actDbImportExecute(Sender: TObject);
    procedure actDbMultiSelectOnOffUpdate(Sender: TObject);
    procedure actDbMultiSelectOnOffExecute(Sender: TObject);
    procedure actDbSetDefaultValuesUpdate(Sender: TObject);
    procedure actDbSetDefaultValuesExecute(Sender: TObject);
    procedure DbBeforeDelete(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure actDbAttachmentsUpdate(Sender: TObject);
    procedure actDbAttachmentsExecute(Sender: TObject);
    procedure DbGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actDbColumnLeftUpdate(Sender: TObject);
    procedure actDbColumnLeftExecute(Sender: TObject);
    procedure actDbColumnCenterUpdate(Sender: TObject);
    procedure actDbColumnCenterExecute(Sender: TObject);
    procedure actDbColumnRightUpdate(Sender: TObject);
    procedure actDbColumnRightExecute(Sender: TObject);
    procedure actDbDataSetStatisticUpdate(Sender: TObject);
    procedure actDbDataSetStatisticExecute(Sender: TObject);
    procedure actDbColumnStatisticUpdate(Sender: TObject);
    procedure actDbColumnStatisticExecute(Sender: TObject);
    procedure actDbDateSetGroupingUpdate(Sender: TObject);
    procedure actDbDateSetGroupingExecute(Sender: TObject);
    procedure actDbSelectAllUpdate(Sender: TObject);
    procedure actDbSelectAllExecute(Sender: TObject);
    procedure actDbFilterCustomUpdate(Sender: TObject);
    procedure actDbFilterCustomExecute(Sender: TObject);
    procedure actDbFilterDefaultUpdate(Sender: TObject);
    procedure actDbFilterDefaultExecute(Sender: TObject);
    procedure actDbFilterSelectedUpdate(Sender: TObject);
    procedure actDbFilterSelectedExecute(Sender: TObject);
    procedure actDbFilterNoneUpdate(Sender: TObject);
    procedure actDbFilterNoneExecute(Sender: TObject);
    procedure actDbSortCustomUpdate(Sender: TObject);
    procedure actDbSortCustomExecute(Sender: TObject);
    procedure actDbSortDefaultUpdate(Sender: TObject);
    procedure actDbSortDefaultExecute(Sender: TObject);
    procedure actDbSortColumnAscUpdate(Sender: TObject);
    procedure actDbSortColumnAscExecute(Sender: TObject);
    procedure actDbSortColumnDescUpdate(Sender: TObject);
    procedure actDbSortColumnDescExecute(Sender: TObject);
  private
    fCurrColumn: TColumn;
    fLastState: Boolean;
    {$IFDEF ATTACH}
    fDsAttachsCnt: TAdoQuery;
    {$ENDIF}
    fOnOpenDS: TOpenDataSetProc;
  protected
    procedure ColumnFlagsReset;
    procedure ColumnFlagsUpdate;
    {$IFDEF ATTACH}
    procedure CreateAttachmentsField;
    procedure LoadAttachments;
    {$ENDIF}
    procedure OpenDataSetBefore;
    function  OpenDataSetExec: Boolean;
    procedure OpenDataSetAfter;
  public
    procedure Init;
    procedure Done;
    procedure InitDataComponents;
    procedure DoneDataComponents;
    function  OpenDataSet: Boolean;
    procedure CloseDataSet;
    {$IFDEF STYLES}
    procedure SetStyle;
    {$ENDIF}
    property LastState: Boolean read fLastState;
    property OnCustomOpenDataSet: TOpenDataSetProc read fOnOpenDS write fOnOpenDS;
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RDialogs, RMsgRu, RDbState, RDbConst, RDbUtils, RDbPrint, RDbImport,
  RDbOpenDS, RDbText, RExHandlers, RClipBrd, ClipBrd, RxStrUtils, StrUtils,  
  {$IFDEF RSS} RDbLog, RRssConst, {$ENDIF}
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  {$IFDEF ATTACH} RDbAttachs, {$ENDIF}
  BaseDbUnit, TmplDbDialog;

{ TFrameDb }

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TFrameDb.SetStyle;
begin
  inherited;
  FindPanel.Visible := (FindPanel.Height > 0) and ApplicationStyle.DataForm.FastFindPanel;
  DbGrid.Color := ApplicationStyle.DataForm.DataColor;
  DbTabViews.BackgroundColor := ApplicationStyle.DataForm.FormColor;
  FontDataToFont(ApplicationStyle.DataForm.DataFont, DbGrid.Font);
  FontDataToFont(ApplicationStyle.DataForm.DataFont, DbTabViews.Font);
end;
{$ENDIF}

{ == Инициализация фрейма ===================================================== }
procedure TFrameDb.Init;
begin
  {$IFDEF ATTACH}
  fDsAttachsCnt := nil;
  {$ENDIF}

  if FindPanel.Height > 0 then
  begin
    FindPanel.Height := edFastFilter.Height + 10;
    FindPanelResize(nil);
  end;
end;

procedure TFrameDb.Done;
begin
  if RDbEditor.DataSet.Filtered then
  begin
    RDbEditor.DataSet.Filtered := False;
    RDbEditor.DataSet.Filter := EmptyStr;
  end;

  {$IFDEF ATTACH}
  FreeAndNil(fDsAttachsCnt);
  {$ENDIF}
end;

{ == Изменение размера ========================================================= }
procedure TFrameDb.FindPanelResize(Sender: TObject);
begin
  edFastFilter.Width := FindPanel.ClientWidth - btnFastFilter.Width - 6;

  btnFastFilter.Left := FindPanel.ClientWidth - btnFastFilter.Width - 2;
  btnFastFilter.Height := edFastFilter.Height;
end;

{ == Инициализация Db-компонентов ============================================== }
procedure TFrameDb.InitDataComponents;
begin
  {$IFDEF ATTACH}
  actDbAttachments.Visible := RDbEditor.KeyFieldIsPresent and (DbGrid.AttachField <> '');
  CreateAttachmentsField;
  {$ELSE}
  actDbAttachments.Visible := False;
  {$ENDIF}
  divGridAttachmentsP.Visible := actDbAttachments.Visible;
  try
    if Assigned(RDbSearch.DataSet) then
    begin
      ShowInStatusBar(Format(SMsgLoadDataFormEx, [SPrmFind]));
      RDbSearch.Open;
    end;
    if Assigned(RDbLocate.DataSet) then
    begin
      ShowInStatusBar(Format(SMsgLoadDataFormEx, [SPrmFind]));
      RDbLocate.Open;
    end;
    if Assigned(RDbFilter.DataSet) then
    begin
      ShowInStatusBar(Format(SMsgLoadDataFormEx, [SPrmFilter]));
      RDbFilter.DateFormatWhere := BaseData.DbParameters.DateFormat;
      RDbFilter.CaseStringsEnabled := BaseData.DbParameters.CaseEnabled;
      RDbFilter.Open;
    end;
    if Assigned(RDbOrder.DataSet) then
    begin
      ShowInStatusBar(Format(SMsgLoadDataFormEx, [SPrmOrder]));
      RDbOrder.Open;
    end;
    if Assigned(RDbGridTuner.DbGrid) then
    begin
      ShowInStatusBar(Format(SMsgLoadDataFormEx, [SPrmGridTuner]));
      RDbGridTuner.Open;
    end;

    menuDataFilterP.Visible := RDbFilter.Active;
    divMenuTitleFilter.Visible := RDbFilter.Active;
    menuDataSortP.Visible := RDbOrder.Active;
    divMenuTitleSort.Visible := RDbOrder.Active;
    menuGridSetupP.Visible := RDbGridTuner.Active;
    divMenuTitleView.Visible := RDbGridTuner.Active;
  except
    on E: Exception do
      HandleExcept(E, Self, SErrLoadFormPlacement);
  end;
end;

{ == Деактивация Db-компонентов ================================================ }
procedure TFrameDb.DoneDataComponents;
begin
  try
    if RDbSearch.Active then
    begin
      ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmFind]));
      RDbSearch.Close;
    end;
    if RDbLocate.Active then
    begin
      ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmFind]));
      RDbLocate.Close;
    end;
    if RDbFilter.Active then
    begin
      ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmFilter]));
      RDbFilter.Close;
    end;
    if RDbOrder.Active then
    begin
      ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmOrder]));
      RDbOrder.Close;
    end;
    if RDbGridTuner.Active then
    begin
      ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmGridTuner]));
      RDbGridTuner.Close;
    end;
  except
    on E: Exception do
      HandleExcept(E, Self, SErrSaveFormPlacement);
  end;
end;

{$IFDEF ATTACH}
{ == Прикрепленные файлы ======================================================= }
procedure TFrameDb.CreateAttachmentsField;
begin
  if actDbAttachments.Visible and not RDbEditor.DataSetIsOpened then
    rAttachs_CreateAttachsFld(RDbEditor);
end;

procedure TFrameDb.LoadAttachments;
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
procedure TFrameDb.ColumnFlagsReset;
begin
  DbGrid.ColumnFlagsClear;
end;

procedure TFrameDb.ColumnFlagsUpdate;
begin
  if Assigned(RDbEditor.DataSet) then
  begin
    if RDbOrder.Active
    then DbGrid.FieldFlagSort(RDbOrder.GetOrderString)
    else begin
      if RDbEditor.DataSet is TAdoTable then
      begin
        if (TAdoTable(RDbEditor.DataSet).IndexFieldNames = EmptyStr)
        then DbGrid.FieldFlagSet(RDbEditor.KeyFieldName, ffSortAsc)
        else DbGrid.FieldFlagSort(TAdoTable(RDbEditor.DataSet).IndexFieldNames);
      end
      else DbGrid.FieldFlagSet(RDbEditor.KeyFieldName, ffSortAsc);
    end;
  end
  else DbGrid.FieldFlagSet(RDbEditor.KeyFieldName, ffSortAsc);
end;

{ == Открытие набора данных =================================================== }

procedure TFrameDb.OpenDataSetBefore;
begin
  ColumnFlagsReset;
  {$IFDEF ATTACH}
  LoadAttachments;
  {$ENDIF}
end;

function TFrameDb.OpenDataSetExec: Boolean;
begin
  if Assigned(fOnOpenDS)
  then fOnOpenDS(RDbEditor.DataSet, Result)
  else Result := BaseData.OpenDataSet(RDbEditor.DataSet);
end;

procedure TFrameDb.OpenDataSetAfter;
begin
  if RDbFilter.Active
  then RDbEditor.ClearSelection
  else RDbEditor.RefreshSelection;
  ColumnFlagsUpdate;
end;

function TFrameDb.OpenDataSet: Boolean;
begin
  if Assigned(RDbEditor.DataSet) then
  begin
    try
      StartWait;
      ShowInStatusBar(SMsgLoadDataWait);
      try
        OpenDataSetBefore;
        try
          Result := OpenDataSetExec;
        finally
          OpenDataSetAfter;
        end;
      finally
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;
    except
      on E: Exception do
      begin
        Result := False;
        HandleExcept(E, Self, sErrLoadData);
      end;
    end;
  end
  else Result := False;
end;

procedure TFrameDb.CloseDataSet;
begin
  {$IFDEF ATTACH}
  if Assigned(fDsAttachsCnt) then
    FreeAndNil(fDsAttachsCnt);
  {$ENDIF}
end;

{ == Обработка событий ======================================================== }
procedure TFrameDb.DbSaveToLog(Sender: TObject; const EditTag: Integer; const Text: String);
begin
  {$IFDEF RSS}
  AddToDbLog(EditTag, Text);
  {$ENDIF}
end;

procedure TFrameDb.DbGetNewKey(Sender: TObject; var Value: Integer);
begin
  with TRDbEditor(Sender) do
    Value := BaseData.GetNewId(GetObjectName(etView), KeyFieldName);
end;

procedure TFrameDb.DbFreeNewKey(Sender: TObject; var Value: Integer);
begin
  with TRDbEditor(Sender) do
    BaseData.FreeId(GetObjectName(etView), Value);
end;

procedure TFrameDb.DbCreateSetDefault(Sender: TObject; const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean);
begin
   RDbUpdater.UpdateDataSetDefault(nil);
end;

procedure TFrameDb.DbBeforeShowEditor(Sender: TObject;
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

{ == Создать новую запись ====================================================== }
procedure TFrameDb.actDbInsertUpdate(Sender: TObject);
begin
  actDbInsert.Enabled := IsNotWait and RDbEditor.RecordCanInserted(True);
end;

procedure TFrameDb.actDbInsertExecute(Sender: TObject);
begin
  fLastState := RDbEditor.InsertRecord;
end;

{ == Копировать запись ========================================================= }
procedure TFrameDb.actDbCloneUpdate(Sender: TObject);
begin
  actDbClone.Enabled := IsNotWait and RDbEditor.RecordCanCopyed(True);
end;

procedure TFrameDb.actDbCloneExecute(Sender: TObject);
begin
  fLastState := RDbEditor.CopyRecord;
end;

procedure TFrameDb.actDbEditUpdate(Sender: TObject);
begin
  actDbEdit.Enabled := IsNotWait and RDbEditor.RecordCanOpened(True);
end;

procedure TFrameDb.actDbEditExecute(Sender: TObject);
begin
  if (RDbEditor.GetSelCount > 1) and (RDbUpdater.Items.Count > 0)
  then fLastState := RDbUpdater.ShowDialog(nil)
  else fLastState := RDbEditor.EditRecord;
end;

procedure TFrameDb.DbGridDblClick(Sender: TObject);
begin
  {$IFDEF ATTACH}
  if Assigned(fCurrColumn)
  and (fCurrColumn.Field = RDbEditor.DataSet.FindField(DbGrid.AttachField)) then
  begin
    actDbAttachmentsUpdate(Sender);
    if actDbAttachments.Enabled then
      actDbAttachmentsExecute(Sender);
  end
  else begin
    if IsNotWait and RDbEditor.RecordCanOpened then
      actDbEditExecute(Sender);
  end;
  {$ELSE}
  if IsNotWait and RDbEditor.RecordCanOpened then
    actDbEditExecute(Sender);
  {$ENDIF}
end;

{ == Удалить выделенную запись ================================================= }
procedure TFrameDb.actDbDeleteUpdate(Sender: TObject);
begin
  actDbDelete.Enabled := IsNotWait and RDbEditor.RecordCanDeleted;
end;

procedure TFrameDb.actDbDeleteExecute(Sender: TObject);
begin
  fLastState := RDbEditor.DeleteRecord(True);
end;

{ == Мастер импорта... ========================================================= }
procedure TFrameDb.actDbImportUpdate(Sender: TObject);
begin
  actDbImport.Enabled := IsNotWait and RDbEditor.RecordCanImported;
end;

procedure TFrameDb.actDbImportExecute(Sender: TObject);
begin
  ImportExternalData(RDbEditor);
end;

{ == Мультиобработка =========================================================== }
procedure TFrameDb.actDbMultiSelectOnOffUpdate(Sender: TObject);
begin
  actDbMultiSelectOnOff.Enabled := IsNotWait and (RDbEditor.DbGrid <> nil);
  actDbMultiSelectOnOff.Checked := RDbEditor.MultiSelect;
end;

procedure TFrameDb.actDbMultiSelectOnOffExecute(Sender: TObject);
begin
  StartWait;
  try
    RDbEditor.MultiSelect := not RDbEditor.MultiSelect;
  finally
    StopWait;
  end;
end;

procedure TFrameDb.actDbSetDefaultValuesUpdate(Sender: TObject);
begin
  actDbSetDefaultValues.Enabled := IsNotWait and (RDbUpdater.Items.Count > 0);
end;

procedure TFrameDb.actDbSetDefaultValuesExecute(Sender: TObject);
begin
  RDbUpdater.ShowDialogDefault;
end;

procedure TFrameDb.actDbSelectAllUpdate(Sender: TObject);
begin
  actDbSelectAll.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply and RDbEditor.MultiSelect;
end;

procedure TFrameDb.actDbSelectAllExecute(Sender: TObject);
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

{ == Обновить данные =========================================================== }
procedure TFrameDb.actDbRefreshUpdate(Sender: TObject);
begin
  actDbRefresh.Enabled := IsNotWait and Assigned(RDbEditor.DataSet);
end;

procedure TFrameDb.actDbRefreshExecute(Sender: TObject);
begin
  OpenDataSet;
end;

{ == Вложения ================================================================== }
procedure TFrameDb.DbBeforeDelete(Sender: TObject; OldData,
  NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
  var Complete: Boolean);
begin
{$IFDEF ATTACH}
  if actDbAttachments.Visible then
    Complete := Complete and rAttachs_DeleteAttachments(BaseData.acDb, RDbEditor);
{$ENDIF}
end;

procedure TFrameDb.actDbAttachmentsUpdate(Sender: TObject);
begin
  actDbAttachments.Enabled := IsNotWait and actDbAttachments.Visible and RDbEditor.RecordCanOpened;
end;

procedure TFrameDb.actDbAttachmentsExecute(Sender: TObject);
begin
{$IFDEF ATTACH}
  if rAttachs_EditAttachments(BaseData.acDb, RDbEditor, Tag, RDbEditor.RecordCanModified) then
  begin
    SafeRequery(fDsAttachsCnt);
    RDbEditor.DataSet.Refresh;
  end;
{$ENDIF}
end;

{ == Поиск записи ============================================================== }
procedure TFrameDb.actDbFindUpdate(Sender: TObject);
begin
  actDbFind.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply and RDbSearch.Active;
end;

procedure TFrameDb.actDbFindExecute(Sender: TObject);
begin
  RDbSearch.ShowDialog;
end;

procedure TFrameDb.actDbFindColumnUpdate(Sender: TObject);
begin
  actDbFindColumn.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply
    and RDbSearch.Active and Assigned(fCurrColumn);
end;

procedure TFrameDb.actDbFindColumnExecute(Sender: TObject);
begin
  if Assigned(fCurrColumn) then
    RDbSearch.ShowDialogEx(fCurrColumn.FieldName, EmptyStr, False);
end;

{ == Перейти к указанной записи ================================================ }
procedure TFrameDb.actDbLocateUpdate(Sender: TObject);
begin
  actDbLocate.Visible := RDbLocate.Active;
  actDbLocate.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TFrameDb.actDbLocateExecute(Sender: TObject);
begin
  RDbLocate.ShowDialog;
end;

{ == Быстрый поиск ============================================================= }
procedure TFrameDb.actDbFastFilterUpdate(Sender: TObject);
begin
  actDbFastFilter.Enabled := IsNotWait and RDbEditor.DataSetIsOpened;
end;

procedure TFrameDb.actDbFastFilterExecute(Sender: TObject);
begin
  if edFastFilter.Text = EmptyStr
  then RDbEditor.DataSet.Filtered := False
  else begin
    try
      DbFilterAllFields(RDbEditor.DataSet, edFastFilter.Text,
        {$IFDEF STYLES} ApplicationStyle.DataForm.FastFindLeadAsterisk {$ELSE} True {$ENDIF}, True);
    except
      try
        DbFilterAllFields(RDbEditor.DataSet, edFastFilter.Text,
          {$IFDEF STYLES} ApplicationStyle.DataForm.FastFindLeadAsterisk {$ELSE} True {$ENDIF}, False);
      except
        on E: Exception do
        begin
          RDbEditor.DataSet.Filtered := False;
          RDbEditor.DataSet.Filter := '';
          ErrorBox(Format(SErrFindError, [edFastFilter.Text]));
        end;
      end;
    end;
  end;
end;

procedure TFrameDb.edFastFilterKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    if RDbEditor.DataSetIsOpened then
    begin
      actDbFastFilterExecute(Sender);
      if {$IFDEF STYLES} ApplicationStyle.DataForm.FastFindGotoData and {$ENDIF}
        (RDbEditor.DataSet.RecordCount > 0) then
          DbGrid.SetFocus;
    end;
  end;

  if Key = #27 then
  begin
    Key := #0;
    edFastFilter.Clear;
    RDbEditor.DataSet.Filtered := False;
    RDbEditor.DataSet.Filter := EmptyStr;
    {$IFDEF STYLES}
    if ApplicationStyle.DataForm.FastFindGotoData then
      DbGrid.SetFocus;
    {$ELSE}
    DbGrid.SetFocus;
    {$ENDIF}
  end;
end;

procedure TFrameDb.edFastFilterEnter(Sender: TObject);
begin
  actDbFirst.ShortCut := 0;
  actDbLast.ShortCut := 0;
end;

procedure TFrameDb.edFastFilterExit(Sender: TObject);
begin
  actDbFirst.ShortCut := 36;
  actDbLast.ShortCut := 35;
end;

{ == Фильтр данных... ========================================================== }
procedure TFrameDb.actDbFilterCustomUpdate(Sender: TObject);
begin
  actDbFilterCustom.Visible := Assigned(RDbFilter.DataSet);
  actDbFilterCustom.Enabled := IsNotWait and RDbFilter.Active;
end;

procedure TFrameDb.actDbFilterCustomExecute(Sender: TObject);
begin
  if RDbFilter.ShowDialog then OpenDataSet;
end;

{ == Фильтр по умолчанию ======================================================= }
procedure TFrameDb.actDbFilterDefaultUpdate(Sender: TObject);
begin
  actDbFilterDefault.Visible := Assigned(RDbFilter.DataSet);
  actDbFilterDefault.Enabled := IsNotWait and RDbFilter.Active;
end;

procedure TFrameDb.actDbFilterDefaultExecute(Sender: TObject);
begin
  RDbFilter.ResetFilter;
  OpenDataSet;
end;

{ == Фильтр по выделению ======================================================= }
procedure TFrameDb.actDbFilterSelectedUpdate(Sender: TObject);
begin
  actDbFilterSelected.Visible := Assigned(RDbFilter.DataSet);
  actDbFilterSelected.Enabled := IsNotWait and RDbFilter.Active
    and RDbEditor.KeyFieldIsPresent and RDbEditor.RecordIsSelected;
end;

procedure TFrameDb.actDbFilterSelectedExecute(Sender: TObject);
begin
  RDbFilter.KeysWhere := Format(sqlInList, [RDbEditor.KeyFieldName, RDbEditor.GetSelIds]);
  RDbFilter.KeysText := STextFilterSelected;
  OpenDataSet;
end;

{ == Показать всё ============================================================= }
procedure TFrameDb.actDbFilterNoneUpdate(Sender: TObject);
begin
  actDbFilterNone.Visible := Assigned(RDbFilter.DataSet);
  actDbFilterNone.Enabled := IsNotWait and RDbFilter.Active;
end;

procedure TFrameDb.actDbFilterNoneExecute(Sender: TObject);
begin
  RDbFilter.ClearFilter;
  OpenDataSet;
end;

{ == Сортировка... ============================================================= }
procedure TFrameDb.actDbSortCustomUpdate(Sender: TObject);
begin
  actDbSortCustom.Visible := Assigned(RDbOrder.DataSet);
  actDbSortCustom.Enabled := IsNotWait and RDbOrder.Active;
end;

procedure TFrameDb.actDbSortCustomExecute(Sender: TObject);
begin
  if RDbOrder.ShowDialog then OpenDataSet;
end;

{ == Сортировка по умолчанию =================================================== }
procedure TFrameDb.actDbSortDefaultUpdate(Sender: TObject);
begin
  actDbSortDefault.Visible := Assigned(RDbOrder.DataSet);
  actDbSortDefault.Enabled := IsNotWait and RDbOrder.Active;
end;

procedure TFrameDb.actDbSortDefaultExecute(Sender: TObject);
begin
  RDbOrder.ResetOrder;
  OpenDataSet;
end;

{ == Обработка меню заголовка таблицы ========================================== }
procedure TFrameDb.actDbSortColumnAscUpdate(Sender: TObject);
begin
  if Assigned(fCurrColumn) and Assigned(fCurrColumn.Field)
    and (fCurrColumn.Field.FieldKind = fkData) and RDbOrder.Active then
  begin
    actDbSortColumnAsc.Visible := True;
    actDbSortColumnAsc.Enabled := IsNotWait;
    actDbSortColumnAsc.Checked := RDbOrder.GetFieldDirection(fCurrColumn.Field.FieldName) = odAsc;
  end
  else actDbSortColumnAsc.Visible := False;
end;

procedure TFrameDb.actDbSortColumnAscExecute(Sender: TObject);
begin
  RDbOrder.SetFieldDirection(fCurrColumn.Field.FieldName, odAsc);
  OpenDataSet;
end;

procedure TFrameDb.actDbSortColumnDescUpdate(Sender: TObject);
begin
  if Assigned(fCurrColumn) and Assigned(fCurrColumn.Field)
    and (fCurrColumn.Field.FieldKind = fkData) and RDbOrder.Active then
  begin
    actDbSortColumnDesc.Visible := True;
    actDbSortColumnDesc.Enabled := IsNotWait;
    actDbSortColumnDesc.Checked := RDbOrder.GetFieldDirection(fCurrColumn.Field.FieldName) = odDesc;
  end
  else actDbSortColumnDesc.Visible := False;
end;

procedure TFrameDb.actDbSortColumnDescExecute(Sender: TObject);
begin
  RDbOrder.SetFieldDirection(fCurrColumn.Field.FieldName, odDesc);
  OpenDataSet;
end;

{ == Настройка отображения столбцов в таблице ================================== }
procedure TFrameDb.actDbGridTuneUpdate(Sender: TObject);
begin
  actDbGridTune.Enabled := IsNotWait and Assigned(RDbEditor.DataSet) and RDbGridTuner.Active;
end;

procedure TFrameDb.actDbGridTuneExecute(Sender: TObject);
begin
  if RDbGridTuner.ShowDialog then
  begin
    ColumnFlagsReset;
    fCurrColumn := nil;
    ColumnFlagsUpdate;
  end;
end;

procedure TFrameDb.RDbGridTunerViewChange(Sender: TObject);
var
  i, iCount: Integer;
begin
  iCount := RDbGridTuner.Views.Count;

  DbTabViews.Tabs.BeginUpdate;
  try
    DbTabViews.Tabs.Clear;
    for i := 0 to iCount - 1 do
      DbTabViews.Tabs.Add(RDbGridTuner.Views[i]);

    DbTabViews.TabIndex := RDbGridTuner.ViewActive;
  finally
    DbTabViews.Tabs.EndUpdate;
  end;

  DbTabViews.Visible := iCount > 1;
end;

procedure TFrameDb.DbTabViewsChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
begin
  ColumnFlagsReset;
  RDbGridTuner.ViewActive := NewTab;
  ColumnFlagsUpdate;
end;

procedure TFrameDb.DbGridColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
begin
  ColumnFlagsReset;
  ColumnFlagsUpdate;
end;

{ == Установить столбцы "по умолчанию" (все видимые) =========================== }
procedure TFrameDb.actDbGridDefaultUpdate(Sender: TObject);
begin
  actDbGridDefault.Enabled := IsNotWait and Assigned(RDbEditor.DataSet) and RDbGridTuner.Active;
end;

procedure TFrameDb.actDbGridDefaultExecute(Sender: TObject);
begin
  if QueryBoxStdNY(SQueryResetColumns) = ID_YES then
  begin
    ColumnFlagsReset;
    RDbGridTuner.ResetColumns;
    fCurrColumn := nil;
    ColumnFlagsUpdate;
  end;
end;

{ == Обработка меню заголовка таблицы ========================================== }
procedure TFrameDb.DbGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  GridPos: TGridCoord;
  GridCol: TColumn;
  MousePos: TPoint;
begin
  MousePos.X := X;
  MousePos.Y := Y;
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
      TitlePopupMenu.Popup(MousePos.X, MousePos.Y);
    end
    else begin
      fCurrColumn := nil;
      if Assigned(GridCol) and Assigned(GridCol.PopupMenu)
      then GridCol.PopupMenu.Popup(MousePos.X, MousePos.Y)
      else GridPopupMenu.Popup(MousePos.X, MousePos.Y);
    end;
  end
  else fCurrColumn := GridCol;
end;

procedure TFrameDb.actDbColumnLeftUpdate(Sender: TObject);
begin
  actDbColumnLeft.Enabled := IsNotWait and Assigned(fCurrColumn);
  actDbColumnLeft.Checked := Assigned(fCurrColumn) and (fCurrColumn.Alignment = taLeftJustify);
end;

procedure TFrameDb.actDbColumnLeftExecute(Sender: TObject);
begin
  StartWait;
  try
    fCurrColumn.Alignment := taLeftJustify;
    fCurrColumn.Title.Alignment := taLeftJustify;
  finally
    StopWait;
  end;
end;

procedure TFrameDb.actDbColumnCenterUpdate(Sender: TObject);
begin
  actDbColumnCenter.Enabled := IsNotWait and Assigned(fCurrColumn);
  actDbColumnCenter.Checked := Assigned(fCurrColumn) and (fCurrColumn.Alignment = taCenter);
end;

procedure TFrameDb.actDbColumnCenterExecute(Sender: TObject);
begin
  StartWait;
  try
    fCurrColumn.Alignment := taCenter;
    fCurrColumn.Title.Alignment := taCenter;
  finally
    StopWait;
  end;
end;

procedure TFrameDb.actDbColumnRightUpdate(Sender: TObject);
begin
  actDbColumnRight.Enabled := IsNotWait and Assigned(fCurrColumn);
  actDbColumnRight.Checked := Assigned(fCurrColumn) and (fCurrColumn.Alignment = taRightJustify);
end;

procedure TFrameDb.actDbColumnRightExecute(Sender: TObject);
begin
  StartWait;
  try
    fCurrColumn.Alignment := taRightJustify;
    fCurrColumn.Title.Alignment := taRightJustify;
  finally
    StopWait;
  end;
end;

{ == Экспорт данных в Microsoft Excel ========================================== }
procedure TFrameDb.actDbExportToExcelUpdate(Sender: TObject);
begin
  actDbExportToExcel.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TFrameDb.actDbExportToExcelExecute(Sender: TObject);
begin
  RDbEditor.ExportToExcel;
end;

{ == Экспорт в файл ============================================================ }
procedure TFrameDb.actDbExportToFileCsvUpdate(Sender: TObject);
begin
  actDbExportToFileCsv.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TFrameDb.actDbExportToFileCsvExecute(Sender: TObject);
begin
  RDbEditor.ExportToCsvFile;
end;

{ == Генерация текстового отчета для выделенной записи ========================= }
procedure TFrameDb.actDbCreateDynamicReportUpdate(Sender: TObject);
begin
  actDbCreateDynamicReport.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TFrameDb.actDbCreateDynamicReportExecute(Sender: TObject);
begin
  PrintCurrentRecord(RDbEditor.DataSet);
end;

{ == Статистика ================================================================ }
procedure TFrameDb.actDbDataSetStatisticUpdate(Sender: TObject);
begin
  actDbDataSetStatistic.Visible := RDbEditor.StatisticFields <> EmptyStr;
  actDbDataSetStatistic.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TFrameDb.actDbDataSetStatisticExecute(Sender: TObject);
begin
  RDbEditor.ShowStatistic(RDbEditor.StatisticFields, False);
end;

procedure TFrameDb.actDbColumnStatisticUpdate(Sender: TObject);
begin
  actDbColumnStatistic.Enabled := IsNotWait and Assigned(fCurrColumn) and RDbEditor.DataSetIsNotEmply;
end;

procedure TFrameDb.actDbColumnStatisticExecute(Sender: TObject);
begin
  RDbEditor.ShowStatistic(fCurrColumn.FieldName, True);
end;

procedure TFrameDb.actDbDateSetGroupingUpdate(Sender: TObject);
begin
  actDbDateSetGrouping.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TFrameDb.actDbDateSetGroupingExecute(Sender: TObject);
begin
  RDbEditor.ShowGrouping;
end;

end.
