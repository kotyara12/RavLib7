unit TmplDbSimple;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplStorage, ComCtrls, ToolWin, Grids, DBGrids, RDbColorGrid,
  RDbStatus, RDbCustom, RDbGridTuner, RDbOrder, RDbFilter, RDbCustomSearch,
  RDbSearch, DB, RDbEditor, Menus, ImgList, ActnList, ExtCtrls, RDbPanel, RAdoUtils,
  StdCtrls, Buttons;

type
  TSimpleDbTemplate = class(TStorageTemplate)
    StatusBar: TStatusBar;
    CoolBar: TCoolBar;
    ToolBar: TToolBar;
    DbGrid: TRDbStyledGrid;
    RDbEditor: TRDbExportEditor;
    RDbSearch: TRDbSearch;
    RDbFilter: TRDbFilter;
    RDbOrder: TRDbOrder;
    RDbGridTuner: TRDbGridTuner;
    RDbFilterStatus: TRDbFilterStatus;
    ActionList: TActionList;
    ImageList: TImageList;
    PopupMenu: TPopupMenu;
    MainMenu: TMainMenu;
    DataSetFirst: TAction;
    DataSetPrior: TAction;
    DataSetNext: TAction;
    DataSetLast: TAction;
    Find: TAction;
    FilterUser: TAction;
    FilterDefault: TAction;
    FilterNone: TAction;
    SortUser: TAction;
    SortDefault: TAction;
    DbGridSetup: TAction;
    CreateDynamicReport: TAction;
    CloseCancel: TAction;
    ExportToExcel: TAction;
    DataSetFirstToolButton: TToolButton;
    DataSetPriorToolButton: TToolButton;
    DataSetNextToolButton: TToolButton;
    DataSetLastToolButton: TToolButton;
    Refresh: TAction;
    DbGridDefault: TAction;
    Separator1: TToolButton;
    Separator2: TToolButton;
    Separator3: TToolButton;
    FindToolButton: TToolButton;
    FilterToolButton: TToolButton;
    SoftToolButton: TToolButton;
    TableToolButton: TToolButton;
    RefreshToolButton: TToolButton;
    Separator4: TToolButton;
    CloseCancelToolButton: TToolButton;
    PrintToolButton: TToolButton;
    FilterPopupMenu: TPopupMenu;
    SortPopupMenu: TPopupMenu;
    TablePopupMenu: TPopupMenu;
    PrintPopupMenu: TPopupMenu;
    itemDataSetFirstP: TMenuItem;
    itemDataSetPriorP: TMenuItem;
    itemDataSetNextP: TMenuItem;
    itemDataSetLastP: TMenuItem;
    divP1: TMenuItem;
    itemFindP: TMenuItem;
    divP2: TMenuItem;
    itemRefreshP: TMenuItem;
    divP4: TMenuItem;
    itemCloseCancelP: TMenuItem;
    divP3: TMenuItem;
    itemFilterDefaultP: TMenuItem;
    itemFilterUserP: TMenuItem;
    itemFilterNoneP: TMenuItem;
    menuFilterP: TMenuItem;
    menuSoftP: TMenuItem;
    itemSortUserP: TMenuItem;
    itemSortDefaultP: TMenuItem;
    menuTableP: TMenuItem;
    itemDbGridDefaultP: TMenuItem;
    itemDbGridSetupP: TMenuItem;
    menuPrintP: TMenuItem;
    itemExportToExcelP: TMenuItem;
    itemCreateDynamicReportP: TMenuItem;
    itemFilterDefaultF: TMenuItem;
    itemFilterUserF: TMenuItem;
    itemFilterNoneF: TMenuItem;
    itemSortUserS: TMenuItem;
    itemSortDefaultS: TMenuItem;
    itemDbGridSetupG: TMenuItem;
    itemDbGridDefaultG: TMenuItem;
    itemExportToExcelE: TMenuItem;
    itemCreateDynamicReportE: TMenuItem;
    menuData: TMenuItem;
    itemDbGridDefault: TMenuItem;
    itemDbGridSetup: TMenuItem;
    itemSortDefault: TMenuItem;
    itemSortUser: TMenuItem;
    divD2: TMenuItem;
    itemFilterNone: TMenuItem;
    itemFilterDefault: TMenuItem;
    itemFilterUser: TMenuItem;
    itemFind: TMenuItem;
    divD1: TMenuItem;
    itemDataSetLast: TMenuItem;
    itemDataSetNext: TMenuItem;
    itemDataSetPrior: TMenuItem;
    itemDataSetFirst: TMenuItem;
    itemRefresh: TMenuItem;
    menuPrint: TMenuItem;
    itemExportToExcel: TMenuItem;
    itemCreateDynamicReport: TMenuItem;
    menuFilter: TMenuItem;
    menuSort: TMenuItem;
    menuTable: TMenuItem;
    InfoPanel: TRDbInfoPanel;
    ShowHelp: TAction;
    menuHelp: TMenuItem;
    itemShowHelp: TMenuItem;
    ExportToFile: TAction;
    itemExportToFile: TMenuItem;
    itemExportToFileR: TMenuItem;
    itemExportToFileP: TMenuItem;
    TitleGridPopupMenu: TPopupMenu;
    itemFindT: TMenuItem;
    divTitle1: TMenuItem;
    itemSortUserT: TMenuItem;
    divTitle2: TMenuItem;
    itemDbGridSetupT: TMenuItem;
    DbGridDefaultT: TMenuItem;
    FindColumn: TAction;
    ColumnLeft: TAction;
    ColumnCenter: TAction;
    ColumnRight: TAction;
    SetCurrOrderAsc: TAction;
    SetCurrOrderDesc: TAction;
    itemSetCurrOrderAsc: TMenuItem;
    itemSetCurrOrderDesc: TMenuItem;
    menuAlingnment: TMenuItem;
    itemColumnLeft: TMenuItem;
    itemColumnCenter: TMenuItem;
    itemColumnRight: TMenuItem;
    FindPanel: TPanel;
    edFastFind: TEdit;
    btnFastFind: TBitBtn;
    FindFast: TAction;
    InfoPanelPopupMenu: TPopupMenu;
    itemCopyInfoPanel: TMenuItem;
    procedure RDbEditorSaveToLog(Sender: TObject; const EditTag: Integer;
      const Text: String);
    procedure FormCreate(Sender: TObject);
    procedure DataSetFirstUpdate(Sender: TObject);
    procedure DataSetFirstExecute(Sender: TObject);
    procedure DataSetPriorUpdate(Sender: TObject);
    procedure DataSetPriorExecute(Sender: TObject);
    procedure DataSetNextUpdate(Sender: TObject);
    procedure DataSetNextExecute(Sender: TObject);
    procedure DataSetLastUpdate(Sender: TObject);
    procedure DataSetLastExecute(Sender: TObject);
    procedure FindUpdate(Sender: TObject);
    procedure FindExecute(Sender: TObject);
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
    procedure DbGridSetupUpdate(Sender: TObject);
    procedure DbGridSetupExecute(Sender: TObject);
    procedure DbGridDefaultUpdate(Sender: TObject);
    procedure DbGridDefaultExecute(Sender: TObject);
    procedure CreateDynamicReportUpdate(Sender: TObject);
    procedure CreateDynamicReportExecute(Sender: TObject);
    procedure ExportToExcelUpdate(Sender: TObject);
    procedure ExportToExcelExecute(Sender: TObject);
    procedure RDbEditorGetExcelComment(Sender: TObject; var Value: String);
    procedure RDbEditorGetExcelCaption(Sender: TObject; var Value: String);
    procedure RDbEditorGetExcelCopyright(Sender: TObject;
      var Value: String);
    procedure RefreshUpdate(Sender: TObject);
    procedure RefreshExecute(Sender: TObject);
    procedure CloseCancelUpdate(Sender: TObject);
    procedure CloseCancelExecute(Sender: TObject);
    procedure ShowHelpUpdate(Sender: TObject);
    procedure ShowHelpExecute(Sender: TObject);
    procedure ExportToFileUpdate(Sender: TObject);
    procedure ExportToFileExecute(Sender: TObject);
    procedure DbGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FindColumnUpdate(Sender: TObject);
    procedure FindColumnExecute(Sender: TObject);
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
    procedure RDbEditorGetEditTag(Sender: TObject; const Mode: TEditMode;
      var EditTag: Integer);
    procedure RDbEditorGetExportTag(Sender: TObject;
      const Mode: TExportMode; var ExportTag: Integer);
    procedure FindPanelResize(Sender: TObject);
    procedure FindFastUpdate(Sender: TObject);
    procedure FindFastExecute(Sender: TObject);
    procedure edFastFindKeyPress(Sender: TObject; var Key: Char);
    procedure edFastFindEnter(Sender: TObject);
    procedure edFastFindExit(Sender: TObject);
    procedure InfoPanelPopupMenuPopup(Sender: TObject);
    procedure itemCopyInfoPanelClick(Sender: TObject);
  private
    fShowWaitOnLoad: Boolean;
    fCurrColumn: TColumn;
  protected
    procedure InitForm; override;
    procedure StartForm; override;
    procedure DoneForm; override;
    procedure FreeForm; override;
    procedure InitFormVariables; override;
    procedure InitDataComponents; dynamic;
    procedure DoneDataComponents; dynamic;
    {$IFDEF RSS}
    procedure LogOnOpen; dynamic;
    procedure LogOnClose; dynamic;
    {$ENDIF}
    function  BeforeLoadData: Boolean; virtual;
    function  LoadDataForm: Boolean; virtual;
    procedure LoadDataTry; virtual;
    procedure LoadDataFinally; virtual;
    function  AfterLoadData: Boolean; virtual;
    procedure CloseDataSets; dynamic;
  public
    Database: Pointer;
    DbParameters: RAdoDbParameters;
    FormData: Pointer;
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    function  LoadData: Boolean;
  end;

  TSimpleDbClass = class of TSimpleDbTemplate;

procedure ShowSimpleDbForm(Form: TSimpleDbClass; const Db, Data: Pointer;
  const Dp: RAdoDbParameters; const FormCaption: string; const ViewTag: Integer;
  const ShowWaitOnLoad: Boolean);

implementation

{$R *.dfm}

uses
  RVclUtils, RMsgRu, RExHandlers, RDialogs, RWait, RDbUtils, DbCtrls, RDbText, RClipBrd, ClipBrd,
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  {$IFDEF RSS} RDbLog, RRssConst, {$ENDIF}
  RDbPrint;

procedure ShowSimpleDbForm(Form: TSimpleDbClass; const Db, Data: Pointer;
  const Dp: RAdoDbParameters; const FormCaption: string; const ViewTag: Integer;
  const ShowWaitOnLoad: Boolean);
begin
  with Form.Create(Application) do
  begin
    try
      Database := Db;
      DbParameters := Dp;
      FormData := Data;
      if ViewTag <> 0 then Tag := ViewTag;
      if FormCaption <> EmptyStr then Caption := FormCaption;
      if Dp.DateFormat <> EmptyStr then RDbFilter.DateFormatWhere := Dp.DateFormat;
      RDbFilter.CaseStringsEnabled := Dp.CaseEnabled;
      fShowWaitOnLoad := ShowWaitOnLoad;
      if LoadData then
        ShowModal;
    finally
      Free;
    end;
  end;
end;

{ == TSimpleDbTemplate ========================================================= }

procedure TSimpleDbTemplate.FormCreate(Sender: TObject);
begin
  Database := nil;
  FormData := nil;
  fCurrColumn := nil;
  inherited;
end;

procedure TSimpleDbTemplate.InitForm;
begin
  ShowInStatusBar(SMsgInitDataForm);
  try
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

procedure TSimpleDbTemplate.StartForm;
begin
  try
    inherited;
  finally
    {$IFDEF RSS}
    LogOnOpen;
    {$ENDIF}
  end;
end;

procedure TSimpleDbTemplate.DoneForm;
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

procedure TSimpleDbTemplate.FreeForm;
begin
  try
    try
      try
        DoneDataComponents;
      finally
        CloseDataSets;
      end;
    except
      on E: Exception do
        HandleExcept(E, Self, SErrDoneForm);
    end;
  finally
    inherited;
    FormData := nil;
    Database := nil;
  end;
end;

procedure TSimpleDbTemplate.InitFormVariables;
begin
  inherited;

  FindPanel.Height := edFastFind.Height + 10;
end;

procedure TSimpleDbTemplate.FindPanelResize(Sender: TObject);
begin
  edFastFind.Width := FindPanel.ClientWidth - btnFastFind.Width - 10;

  btnFastFind.Left := FindPanel.ClientWidth - btnFastFind.Width - 4;
  btnFastFind.Height := edFastFind.Height;
end;

procedure TSimpleDbTemplate.InitDataComponents;
begin
  try
    RDbSearch.Open;
  finally
    RDbGridTuner.Open;
  end;
end;

procedure TSimpleDbTemplate.DoneDataComponents;
begin
  if RDbEditor.DataSet.Filtered then
  begin
    RDbEditor.DataSet.Filtered := False;
    RDbEditor.DataSet.Filter := '';
  end;
  try
    if RDbSearch.Active then RDbSearch.Close;
  finally
    try
      if RDbGridTuner.Active then RDbGridTuner.Close;
    finally
      try
        if RDbFilter.Active then RDbFilter.Close;
      finally
        if RDbOrder.Active then RDbOrder.Close;
      end;
    end;
  end;
end;

{$IFDEF STYLES}
procedure TSimpleDbTemplate.SetStyle;
begin
  inherited;
  CoolBar.Visible := ApplicationStyle.DataForm.TbVisible;
  ToolBar.ShowCaptions := ApplicationStyle.DataForm.TbCaptions;
  ToolBar.ButtonHeight := 1;
  ToolBar.ButtonWidth := 1;
  StatusBar.Visible := ApplicationStyle.DataForm.SbVisible;
  FindPanel.Visible := ApplicationStyle.DataForm.FastFindPanel;
  DbGrid.Color := ApplicationStyle.DataForm.DataColor;
  FontDataToFont(ApplicationStyle.DataForm.DataFont, DbGrid.Font);
  InfoPanel.BeginUpdate;
  try
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

{$IFDEF RSS}
procedure TSimpleDbTemplate.LogOnOpen;
begin
  if Tag > 0 then
    AddToDbLog(Tag, Format(SLogOpenWindow, [Caption]));
end;

procedure TSimpleDbTemplate.LogOnClose;
begin
  if Tag > 0 then
    AddToDbLog(Tag, Format(SLogCloseWindow, [Caption]));
end;
{$ENDIF}

procedure TSimpleDbTemplate.RDbEditorGetEditTag(Sender: TObject;
  const Mode: TEditMode; var EditTag: Integer);
begin
  if Mode = etView then
  begin
    EditTag := Tag;
    if EditTag = 0 then
      EditTag := RDbEditor.Tag;
    if (EditTag = 0) and Assigned(RDbEditor.DataSet) then
      EditTag := RDbEditor.DataSet.Tag;
  end
  else begin
    if Assigned(RDbEditor.DataSet) then
      EditTag := RDbEditor.DataSet.Tag;
    if EditTag = 0 then
      EditTag := Tag;
    if EditTag = 0 then
      EditTag := RDbEditor.Tag;
  end;
end;

procedure TSimpleDbTemplate.RDbEditorGetExportTag(Sender: TObject;
  const Mode: TExportMode; var ExportTag: Integer);
begin
  ExportTag := Tag;
  if (ExportTag = 0) then
    ExportTag := RDbEditor.Tag;
  if (ExportTag = 0) and Assigned(RDbEditor.DataSet) then
    ExportTag := RDbEditor.DataSet.Tag;
end;

procedure TSimpleDbTemplate.RDbEditorSaveToLog(Sender: TObject;
  const EditTag: Integer; const Text: string);
begin
  {$IFDEF RSS}
  AddToDbLog(EditTag, Text);
  {$ENDIF}
end;

function TSimpleDbTemplate.BeforeLoadData: Boolean;
begin
  Result := True;
end;

procedure TSimpleDbTemplate.LoadDataTry;
begin
end;

function TSimpleDbTemplate.LoadDataForm: Boolean;
begin
  Result := True;
end;

procedure TSimpleDbTemplate.LoadDataFinally;
begin
end;

function TSimpleDbTemplate.AfterLoadData: Boolean;
begin
  Result := True;
end;

function TSimpleDbTemplate.LoadData: Boolean;
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
    StopWait;
  end;
end;

procedure TSimpleDbTemplate.CloseDataSets;
begin
  if RDbEditor.DataSetIsOpened then RDbEditor.DataSet.Close;
end;

procedure TSimpleDbTemplate.DbGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  GridPos: TGridCoord;
  MousePos: TPoint;
begin
  inherited;
  MousePos.X := X; MousePos.Y := Y;
  MousePos := DbGrid.ClientToScreen(MousePos);
  GridPos := DbGrid.MouseCoord(X, Y);
  if Button = mbRight then
  begin
    if (GridPos.Y = 0) and (GridPos.X > 0) and (GridPos.X <= DbGrid.Columns.Count) then
    begin
      fCurrColumn := DbGrid.Columns[GridPos.X - 1];
      TitleGridPopupMenu.Popup(MousePos.X, MousePos.Y);
    end
    else begin
      fCurrColumn := nil;
      PopupMenu.Popup(MousePos.X, MousePos.Y);
    end;
  end
  else fCurrColumn := nil;
end;

procedure TSimpleDbTemplate.ColumnLeftUpdate(Sender: TObject);
begin
  ColumnLeft.Enabled := IsNotWait and Assigned(fCurrColumn);
  ColumnLeft.Checked := Assigned(fCurrColumn) and (fCurrColumn.Alignment = taLeftJustify);
end;

procedure TSimpleDbTemplate.ColumnLeftExecute(Sender: TObject);
begin
  StartWait;
  try
    fCurrColumn.Alignment := taLeftJustify;
    fCurrColumn.Title.Alignment := taLeftJustify;
  finally
    StopWait;
  end;
end;

procedure TSimpleDbTemplate.ColumnCenterUpdate(Sender: TObject);
begin
  ColumnCenter.Enabled := IsNotWait and Assigned(fCurrColumn);
  ColumnCenter.Checked := Assigned(fCurrColumn) and (fCurrColumn.Alignment = taCenter);
end;

procedure TSimpleDbTemplate.ColumnCenterExecute(Sender: TObject);
begin
  StartWait;
  try
    fCurrColumn.Alignment := taCenter;
    fCurrColumn.Title.Alignment := taCenter;
  finally
    StopWait;
  end;
end;

procedure TSimpleDbTemplate.ColumnRightUpdate(Sender: TObject);
begin
  ColumnRight.Enabled := IsNotWait and Assigned(fCurrColumn);
  ColumnRight.Checked := Assigned(fCurrColumn) and (fCurrColumn.Alignment = taRightJustify);
end;

procedure TSimpleDbTemplate.ColumnRightExecute(Sender: TObject);
begin
  StartWait;
  try
    fCurrColumn.Alignment := taRightJustify;
    fCurrColumn.Title.Alignment := taRightJustify;
  finally
    StopWait;
  end;
end;

procedure TSimpleDbTemplate.DataSetFirstUpdate(Sender: TObject);
begin
  DataSetFirst.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply
    and not RDbEditor.DataSet.Bof;
end;

procedure TSimpleDbTemplate.DataSetFirstExecute(Sender: TObject);
begin
  StartWait;
  try
    RDbEditor.DataSet.First;
  finally
    StopWait;
  end;
end;

procedure TSimpleDbTemplate.DataSetPriorUpdate(Sender: TObject);
begin
  DataSetPrior.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply
    and not RDbEditor.DataSet.Bof;
end;

procedure TSimpleDbTemplate.DataSetPriorExecute(Sender: TObject);
begin
  StartWait;
  try
    RDbEditor.DataSet.Prior;
  finally
    StopWait;
  end;
end;

procedure TSimpleDbTemplate.DataSetNextUpdate(Sender: TObject);
begin
  DataSetNext.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply
    and not RDbEditor.DataSet.Eof;
end;

procedure TSimpleDbTemplate.DataSetNextExecute(Sender: TObject);
begin
  StartWait;
  try
    RDbEditor.DataSet.Next;
  finally
    StopWait;
  end;
end;

procedure TSimpleDbTemplate.DataSetLastUpdate(Sender: TObject);
begin
  DataSetLast.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply
    and not RDbEditor.DataSet.Eof;
end;

procedure TSimpleDbTemplate.DataSetLastExecute(Sender: TObject);
begin
  StartWait;
  try
    RDbEditor.DataSet.Last;
  finally
    StopWait;
  end;
end;

procedure TSimpleDbTemplate.FindUpdate(Sender: TObject);
begin
  Find.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply and RDbSearch.Active;
end;

procedure TSimpleDbTemplate.FindExecute(Sender: TObject);
begin
  RDbSearch.ShowDialog;
end;

procedure TSimpleDbTemplate.FindColumnUpdate(Sender: TObject);
begin
  FindColumn.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply
    and RDbSearch.Active and Assigned(fCurrColumn);
end;

procedure TSimpleDbTemplate.FindColumnExecute(Sender: TObject);
begin
  RDbSearch.ShowDialogEx(fCurrColumn.FieldName, EmptyStr, False);
end;

procedure TSimpleDbTemplate.FindFastUpdate(Sender: TObject);
begin
  FindFast.Enabled := IsNotWait and RDbEditor.DataSetIsOpened;
end;

procedure TSimpleDbTemplate.FindFastExecute(Sender: TObject);
begin
  if edFastFind.Text = EmptyStr
  then RDbEditor.DataSet.Filtered := False
  else begin
    try
      DbFilterAllFields(RDbEditor.DataSet, edFastFind.Text,
        {$IFDEF STYLES} ApplicationStyle.DataForm.FastFindLeadAsterisk {$ELSE} True {$ENDIF});
    except
      on E: Exception do
      begin
        RDbEditor.DataSet.Filtered := False;
        RDbEditor.DataSet.Filter := '';
        HandleExcept(E, RDbEditor.DataSet, Format(SErrFindError, [edFastFind.Text]));
      end;
    end;
  end;
end;

procedure TSimpleDbTemplate.edFastFindKeyPress(Sender: TObject; var Key: Char);
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

procedure TSimpleDbTemplate.edFastFindEnter(Sender: TObject);
begin
  DataSetFirst.ShortCut := 0;
  DataSetLast.ShortCut := 0;
end;

procedure TSimpleDbTemplate.edFastFindExit(Sender: TObject);
begin
  DataSetFirst.ShortCut := 36;
  DataSetLast.ShortCut := 35;
end;

procedure TSimpleDbTemplate.FilterUserUpdate(Sender: TObject);
begin
  FilterUser.Enabled := IsNotWait and RDbFilter.Active;
end;

procedure TSimpleDbTemplate.FilterUserExecute(Sender: TObject);
begin
  if RDbFilter.ShowDialog then LoadData;
end;

procedure TSimpleDbTemplate.FilterDefaultUpdate(Sender: TObject);
begin
  FilterDefault.Enabled := IsNotWait and RDbFilter.Active;
end;

procedure TSimpleDbTemplate.FilterDefaultExecute(Sender: TObject);
begin
  RDbFilter.ResetFilter;
  LoadData;
end;

procedure TSimpleDbTemplate.FilterNoneUpdate(Sender: TObject);
begin
  FilterNone.Enabled := IsNotWait and RDbFilter.Active;
end;

procedure TSimpleDbTemplate.FilterNoneExecute(Sender: TObject);
begin
  RDbFilter.ClearFilter;
  LoadData;
end;

procedure TSimpleDbTemplate.SortUserUpdate(Sender: TObject);
begin
  SortUser.Enabled := IsNotWait and RDbOrder.Active;
end;

procedure TSimpleDbTemplate.SortUserExecute(Sender: TObject);
begin
  if RDbOrder.ShowDialog then LoadData;
end;

procedure TSimpleDbTemplate.SortDefaultUpdate(Sender: TObject);
begin
  SortDefault.Enabled := IsNotWait and RDbOrder.Active;
end;

procedure TSimpleDbTemplate.SortDefaultExecute(Sender: TObject);
begin
  RDbOrder.ResetOrder;
  LoadData;
end;

procedure TSimpleDbTemplate.SetCurrOrderAscUpdate(Sender: TObject);
begin
  if Assigned(fCurrColumn) and Assigned(fCurrColumn.Field)
    and (fCurrColumn.Field.FieldKind = fkData) and RDbOrder.Active then
  begin
    SetCurrOrderAsc.Visible := SortUser.Visible;
    SetCurrOrderAsc.Enabled := IsNotWait;
    SetCurrOrderAsc.Checked := RDbOrder.GetFieldDirection(fCurrColumn.Field.FieldName) = odAsc;
  end
  else SetCurrOrderAsc.Visible := False;
  divTitle1.Visible := SortUser.Visible;
end;

procedure TSimpleDbTemplate.SetCurrOrderAscExecute(Sender: TObject);
begin
  RDbOrder.SetFieldDirection(fCurrColumn.Field.FieldName, odAsc);
  LoadData;
end;

procedure TSimpleDbTemplate.SetCurrOrderDescUpdate(Sender: TObject);
begin
  if Assigned(fCurrColumn) and Assigned(fCurrColumn.Field)
    and (fCurrColumn.Field.FieldKind = fkData) and RDbOrder.Active then
  begin
    SetCurrOrderDesc.Visible := SortUser.Visible;
    SetCurrOrderDesc.Enabled := IsNotWait;
    SetCurrOrderDesc.Checked := RDbOrder.GetFieldDirection(fCurrColumn.Field.FieldName) = odDesc;
  end
  else SetCurrOrderDesc.Visible := False;
  divTitle1.Visible := SortUser.Visible;
end;

procedure TSimpleDbTemplate.SetCurrOrderDescExecute(Sender: TObject);
begin
  RDbOrder.SetFieldDirection(fCurrColumn.Field.FieldName, odDesc);
  LoadData;
end;

procedure TSimpleDbTemplate.DbGridSetupUpdate(Sender: TObject);
begin
  DbGridSetup.Enabled := IsNotWait and Assigned(RDbEditor.DataSet)
    and RDbGridTuner.Active;
end;

procedure TSimpleDbTemplate.DbGridSetupExecute(Sender: TObject);
begin
  RDbGridTuner.ShowDialog;
end;

procedure TSimpleDbTemplate.DbGridDefaultUpdate(Sender: TObject);
begin
  DbGridDefault.Enabled := IsNotWait and Assigned(RDbEditor.DataSet)
    and RDbGridTuner.Active;
end;

procedure TSimpleDbTemplate.DbGridDefaultExecute(Sender: TObject);
begin
  if QueryBoxStdNY(SQueryResetColumns) = ID_YES then
    RDbGridTuner.ResetColumns;
end;

procedure TSimpleDbTemplate.CreateDynamicReportUpdate(Sender: TObject);
begin
  CreateDynamicReport.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TSimpleDbTemplate.CreateDynamicReportExecute(Sender: TObject);
begin
  PrintCurrentRecord(RDbEditor.DataSet);
end;

procedure TSimpleDbTemplate.ExportToExcelUpdate(Sender: TObject);
begin
  ExportToExcel.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TSimpleDbTemplate.ExportToExcelExecute(Sender: TObject);
begin
  RDbEditor.ExportToExcel;
end;

procedure TSimpleDbTemplate.RDbEditorGetExcelCopyright(Sender: TObject; var Value: String);
begin
  Value := SCopyrightsStr;
end;

procedure TSimpleDbTemplate.RDbEditorGetExcelCaption(Sender: TObject; var Value: String);
begin
  Value := Caption;
end;

procedure TSimpleDbTemplate.RDbEditorGetExcelComment(Sender: TObject; var Value: String);
begin
  if RDbFilter.Active then Value := RDbFilter.GetTextString;
end;

procedure TSimpleDbTemplate.ExportToFileUpdate(Sender: TObject);
begin
  ExportToFile.Enabled := IsNotWait and RDbEditor.DataSetIsNotEmply;
end;

procedure TSimpleDbTemplate.ExportToFileExecute(Sender: TObject);
begin
  RDbEditor.ExportToCsvFile;
end;

procedure TSimpleDbTemplate.RefreshUpdate(Sender: TObject);
begin
  Refresh.Enabled := IsNotWait;
end;

procedure TSimpleDbTemplate.RefreshExecute(Sender: TObject);
begin
  LoadData;
end;

procedure TSimpleDbTemplate.CloseCancelUpdate(Sender: TObject);
begin
  CloseCancel.Enabled := IsNotWait;
end;

procedure TSimpleDbTemplate.CloseCancelExecute(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

procedure TSimpleDbTemplate.ShowHelpUpdate(Sender: TObject);
begin
  ShowHelp.Enabled := IsNotWait
    and (((HelpFile <> EmptyStr) and FileExists(HelpFile))
      or FileExists(Application.HelpFile))
    and (((HelpType = htKeyword) and (HelpKeyword <> EmptyStr))
      or ((HelpType = htContext) and (HelpContext <> 0)));
end;

procedure TSimpleDbTemplate.ShowHelpExecute(Sender: TObject);
begin
  if HelpType = htKeyword
  then Application.HelpKeyword(HelpKeyword)
  else Application.HelpContext(HelpContext);
end;

procedure TSimpleDbTemplate.InfoPanelPopupMenuPopup(Sender: TObject);
begin
  itemCopyInfoPanel.Enabled := Assigned(InfoPanelPopupMenu.PopupComponent) and
      (((InfoPanelPopupMenu.PopupComponent is TDbText) and (TDbText(InfoPanelPopupMenu.PopupComponent).Caption <> ''))
    or ((InfoPanelPopupMenu.PopupComponent is TDbMemo) and (TDbMemo(InfoPanelPopupMenu.PopupComponent).Text <> ''))
    or ((InfoPanelPopupMenu.PopupComponent is TRDbText) and (TRDbText(InfoPanelPopupMenu.PopupComponent).FieldText <> ''))
    or ((InfoPanelPopupMenu.PopupComponent is TStaticText) and (TStaticText(InfoPanelPopupMenu.PopupComponent).Caption <> ''))
    or ((InfoPanelPopupMenu.PopupComponent is TImage) and (Assigned(TImage(InfoPanelPopupMenu.PopupComponent).Picture.Graphic))));
end;

procedure TSimpleDbTemplate.itemCopyInfoPanelClick(Sender: TObject);
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
