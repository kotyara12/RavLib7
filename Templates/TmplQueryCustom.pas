unit TmplQueryCustom;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDb, RDbStatus, RDbCustom, RDbGridTuner, RDbFind, DB, Menus,
  ActnList, ImgList, Grids, DBGrids, RDbColorGrid, RDbPanel, ExtCtrls,
  ComCtrls, ToolWin, ADODB, RDbFilter, RDbOrder, DBActns, RDbEditor,
  RDbCustomSearch, RDbSearch, TmplDbCustom, RDbUpdater, StdCtrls, Buttons, Tabs;

type
  TQueryCustomTemplate = class(TDbCustomTemplate)
    RDbFilter: TRDbFilter;
    RDbOrder: TRDbOrder;
    FilterUser: TAction;
    FilterDefault: TAction;
    FilterNone: TAction;
    SortUser: TAction;
    SortDefault: TAction;
    itemFilterUser: TMenuItem;
    itemFilterDefault: TMenuItem;
    itemFilterNone: TMenuItem;
    divDataSort: TMenuItem;
    itemSortUser: TMenuItem;
    itemSortDefault: TMenuItem;
    divDataFilter: TMenuItem;
    itemFilterUserP: TMenuItem;
    itemFilterDefaultP: TMenuItem;
    itemFilterNoneP: TMenuItem;
    divPopupSort: TMenuItem;
    divPopupGrid: TMenuItem;
    itemSortUserP: TMenuItem;
    itemSortDefaultP: TMenuItem;
    itemFilterUserD: TMenuItem;
    itemFilterDefaultD: TMenuItem;
    itemFilterNoneD: TMenuItem;
    itemSortUserD: TMenuItem;
    itemSortDefaultD: TMenuItem;
    divDataPFilter: TMenuItem;
    divDataPSort: TMenuItem;
    SetCurrOrderAsc: TAction;
    SetCurrOrderDesc: TAction;
    itemSetCurrOrderAsc: TMenuItem;
    itemSetCurrOrderDesc: TMenuItem;
    itemSortUserT: TMenuItem;
    divGridTitle: TMenuItem;
    FilterSelected: TAction;
    itemFilterSelected: TMenuItem;
    itemFilterSelectedP: TMenuItem;
    itemFilterSelectedD: TMenuItem;
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
    procedure RDbEditorGetExcelComment(Sender: TObject; var Value: String);
    procedure SetCurrOrderDescUpdate(Sender: TObject);
    procedure SetCurrOrderDescExecute(Sender: TObject);
    procedure SetCurrOrderAscUpdate(Sender: TObject);
    procedure SetCurrOrderAscExecute(Sender: TObject);
    procedure FilterSelectedUpdate(Sender: TObject);
    procedure FilterSelectedExecute(Sender: TObject);
    procedure RefreshExecute(Sender: TObject);
  private
  protected
    procedure InitDataComponents; override;
    procedure DoneDataComponents; override;
    procedure ColumnFlagsUpdate; override;
    function  AfterLoadData: Boolean; override;
    procedure CloseDataSets; override;
    function  ReportsVar_FilterUser: string; override;
  public
  end;

implementation

{$R *.dfm}

uses
  RDbConst, RDialogs, RMsgRu, RVclUtils, RxStrUtils, StrUtils,
  BaseDbUnit;

{ == Инициализация Db-компонентов ============================================== }
procedure TQueryCustomTemplate.InitDataComponents;
begin
  try
    inherited;
  finally
    RDbFilter.DateFormatWhere := BaseData.DbParameters.DateFormat;
    RDbFilter.CaseStringsEnabled := BaseData.DbParameters.CaseEnabled;
  end;
end;

{ == Деактивация Db-компонентов ================================================ }
procedure TQueryCustomTemplate.DoneDataComponents;
begin
  try
    inherited;
  finally
    try
      ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmFilter]));
      if RDbFilter.Active then RDbFilter.Close;
    finally
      ShowInStatusBar(Format(SMsgSaveDataFormEx, [SPrmOrder]));
      if RDbOrder.Active then RDbOrder.Close;
    end;
  end;
end;

{ == Метки сортировки на заголовке таблицы ===================================== }
procedure TQueryCustomTemplate.ColumnFlagsUpdate;
begin
  if RDbOrder.Active then
    DbGrid.FieldFlagSort(RDbOrder.GetOrderString);
end;


{ == Загрузка данных =========================================================== }
function TQueryCustomTemplate.AfterLoadData: Boolean;
begin
  Result := inherited AfterLoadData;
  RDbEditor.ClearSelection;
end;

procedure TQueryCustomTemplate.RefreshExecute(Sender: TObject);
begin
  LoadData;
end;

{ == Закрытие наборов данных =================================================== }
procedure TQueryCustomTemplate.CloseDataSets;
begin
  try
    inherited;
  finally
    if RDbEditor.DataSetIsOpened then
      RDbEditor.DataSet.Close;
  end;
end;

{ == Экспорт данных в Microsoft Excel ========================================== }
procedure TQueryCustomTemplate.RDbEditorGetExcelComment(Sender: TObject; var Value: String);
begin
  if RDbFilter.Active then Value := RDbFilter.GetTextString;
end;

{ == Установка произвольного фильтра данных ==================================== }
procedure TQueryCustomTemplate.FilterUserUpdate(Sender: TObject);
begin
  FilterUser.Enabled := IsNotWait and RDbFilter.Active;
end;

procedure TQueryCustomTemplate.FilterUserExecute(Sender: TObject);
begin
  if RDbFilter.ShowDialog then LoadData;
end;

{ == Установка фильтра данных "по умолчанию" =================================== }
procedure TQueryCustomTemplate.FilterDefaultUpdate(Sender: TObject);
begin
  FilterDefault.Enabled := IsNotWait and RDbFilter.Active;
end;

procedure TQueryCustomTemplate.FilterDefaultExecute(Sender: TObject);
begin
  RDbFilter.ResetFilter;
  LoadData;
end;

{ == Установка фильтра данных "по выделению" =================================== }
procedure TQueryCustomTemplate.FilterSelectedUpdate(Sender: TObject);
begin
  FilterSelected.Enabled := IsNotWait and RDbFilter.Active
    and RDbEditor.KeyFieldIsPresent
    and RDbEditor.RecordIsSelected;
end;

procedure TQueryCustomTemplate.FilterSelectedExecute(Sender: TObject);
begin
  RDbFilter.KeysWhere := Format(sqlInList, [RDbEditor.KeyFieldName, RDbEditor.GetSelIds]);
  RDbFilter.KeysText := STextFilterSelected;
  LoadData;
end;

{ == Отключить фильтр данных =================================================== }
procedure TQueryCustomTemplate.FilterNoneUpdate(Sender: TObject);
begin
  FilterNone.Enabled := IsNotWait and RDbFilter.Active;
end;

procedure TQueryCustomTemplate.FilterNoneExecute(Sender: TObject);
begin
  RDbFilter.ClearFilter;
  LoadData;
end;

{ == Установка произвольной сортировки данных ================================== }
procedure TQueryCustomTemplate.SortUserUpdate(Sender: TObject);
begin
  SortUser.Enabled := IsNotWait and RDbOrder.Active;
end;

procedure TQueryCustomTemplate.SortUserExecute(Sender: TObject);
begin
  if RDbOrder.ShowDialog then LoadData;
end;

{ == Установка сортировки данных "по умолчанию" ================================ }
procedure TQueryCustomTemplate.SortDefaultUpdate(Sender: TObject);
begin
  SortDefault.Enabled := IsNotWait and RDbOrder.Active;
end;

procedure TQueryCustomTemplate.SortDefaultExecute(Sender: TObject);
begin
  RDbOrder.ResetOrder;
  LoadData;
end;

{ == Обработка меню заголовка таблицы ========================================== }

procedure TQueryCustomTemplate.SetCurrOrderAscUpdate(Sender: TObject);
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

procedure TQueryCustomTemplate.SetCurrOrderAscExecute(Sender: TObject);
begin
  RDbOrder.SetFieldDirection(fCurrColumn.Field.FieldName, odAsc);
  LoadData;
end;

procedure TQueryCustomTemplate.SetCurrOrderDescUpdate(Sender: TObject);
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

procedure TQueryCustomTemplate.SetCurrOrderDescExecute(Sender: TObject);
begin
  RDbOrder.SetFieldDirection(fCurrColumn.Field.FieldName, odDesc);
  LoadData;
end;

function TQueryCustomTemplate.ReportsVar_FilterUser: string;
begin
  Result := inherited ReportsVar_FilterUser;
  if RDbFilter.Active then
    Result := RDbFilter.GetTextString;
end;

end.
