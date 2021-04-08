unit RExpExcel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls, Spin, Db, DbGrids,
  Menus, ImgList, ActnList;

{ == Экспорт таблицы в Excel =================================================== }
function ExportDataSetToExcel(ADS: TDataSet; Grid: TDbGrid;
  const ATitle, ASheetName, ACopyright, AComment: string;
  const Orientation: Integer; const UseIniFile: Boolean = True): Boolean;

{ == Экспорт данных из TListView в Excel ======================================= }
function ExportListViewToExcel(LV: TListView;
  const ATitle, ASheetName, ACopyright, AComment: string;
  const Orientation: Integer; const UseIniFile: Boolean = True): Boolean;

{ == TFormExpExcel ============================================================= }

type
  TFormExpExcel = class(TDialogTemplate)
    PageControl: TPageControl;
    PageTabSheet: TTabSheet;
    FieldsTabSheet: TTabSheet;
    gbPage: TGroupBox;
    lblOrientation: TLabel;
    lblFontSize: TLabel;
    lblPageName: TLabel;
    edOrientation: TComboBox;
    edFontSize: TSpinEdit;
    edPageName: TEdit;
    ListViewLabel: TLabel;
    ListView: TListView;
    SaveCheckBox: TCheckBox;
    FileBtn: TBitBtn;
    ActionList: TActionList;
    ImageList: TImageList;
    FilePopupMenu: TPopupMenu;
    ListPopupMenu: TPopupMenu;
    OpenFile: TAction;
    SaveFile: TAction;
    itemOpenFile: TMenuItem;
    itemSaveFile: TMenuItem;
    btnMoveUp: TBitBtn;
    btnMoveDown: TBitBtn;
    btnRename: TBitBtn;
    MoveUp: TAction;
    MoveDown: TAction;
    Rename: TAction;
    itemMoveUp: TMenuItem;
    itemMoveDown: TMenuItem;
    divN1: TMenuItem;
    itemRename: TMenuItem;
    gbSendMode: TGroupBox;
    edSendMode: TComboBox;
    edRecNum: TStaticText;
    lblSendMode: TLabel;
    gbReportTitle: TGroupBox;
    lblTitle: TLabel;
    edTitle: TEdit;
    lblNotes: TLabel;
    edNotes: TEdit;
    cbDateCreate: TCheckBox;
    cbReportTitle: TCheckBox;
    cbColumnsTitle: TCheckBox;
    procedure FileBtnClick(Sender: TObject);
    procedure OpenFileUpdate(Sender: TObject);
    procedure OpenFileExecute(Sender: TObject);
    procedure SaveFileUpdate(Sender: TObject);
    procedure SaveFileExecute(Sender: TObject);
    procedure MoveUpUpdate(Sender: TObject);
    procedure MoveUpExecute(Sender: TObject);
    procedure MoveDownUpdate(Sender: TObject);
    procedure MoveDownExecute(Sender: TObject);
    procedure RenameUpdate(Sender: TObject);
    procedure RenameExecute(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewClick(Sender: TObject);
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edSendModeChange(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure cbReportTitleClick(Sender: TObject);
  private
    ItemChecked: TListItem;
  protected
    procedure StartForm; override;
    procedure FreeForm; override;
  public
    FormName: string;
    DataName: string;
    TotalRows: Integer;
    SelectedRows: Integer;
    procedure LoadList_Fields(DS: TDataSet; DG: TDbGrid);
    procedure LoadList_Columns(LV: TListView);
    procedure LoadExportParameters(const FileName: string; const LoadState, LoadTitle: Boolean);
    procedure SaveExportParameters(const FileName: string; const SaveState: Boolean);
  end;

implementation

{$R *.dfm}

uses
  IniFiles, RxStrUtils, ExcelConst, RExpExcelParam, RDbConst, RFrmStorage, RDbUtils,
  RVclUtils, RMsExcel, RSysUtils, RMsgRu, RListView, RDialogs, RProgress, RExHandlers;

resourcestring
  rsErrLoadExportParameters = 'Ошибка загрузки параметров экспорта из файла "%s"!';
  rsErrSaveExportParameters = 'Ошибка сохранения параметров экспорта в файле "%s"!';
  rsErrLoadFieldParameters  = 'Ошибка загрузки параметров экспортируемого поля ID="%d"!';

  rsDlgFilter               = 'Файлы параметров экспорта (*.exp)|*.exp|Все файлы (*.*)|*.*';
  rsDlgDefaultExt           = 'exp';

const
  iniSectionName            = 'EXPORT_EXCEL_%s.%s';
  iniTitleEnabled           = 'TitleEnabled';
  iniTitle                  = 'Title';
  iniNotes                  = 'Notes';
  iniColumnsTitle           = 'ColumnsTitle';
  iniPageName               = 'SheetName';
  iniTimeCreate             = 'ShowCreateTime';
  iniOrientation            = 'PageOrientation';
  iniBaseFontSize           = 'BaseFontSize';
  iniState                  = 'RestoreParameters';
  iniItem                   = 'Item_%d';
  iniValue                  = '%d;%s;%s;%s;%d;%s';
  chDelims                  = [';'];

  PrepareSteps              = 5;
  DefaultFontSize           = 8;
  DefaultColumnWidth        = 10;
  DefaultNumberFormat       = '';
  DefaultTextFormat         = '@';

  siAlignment               = 0;
  siWidth                   = 1;
  siFieldName               = 2;

{ == Экспорт таблицы в Excel =================================================== }
function ExportDataSetToExcel(ADS: TDataSet; Grid: TDbGrid;
  const ATitle, ASheetName, ACopyright, AComment: string;
  const Orientation: Integer; const UseIniFile: Boolean = True): Boolean;
var
  Sheet, Row: Variant;
  i, iCount, X, Y, BaseY, SizeX, SizeY: Integer;
  Fld: TField;
  Bmk: TBookmark;

  procedure PrepareRecordData(ListView: TListView);
  var
    i, iCount: Integer;
  begin
    X := 0;
    iCount := ListView.Items.Count - 1;
    for i := 0 to iCount do
    begin
      if ListView.Items[i].Checked then
      begin
        Inc(X);
        Fld := ADS.FindField(ListView.Items[i].Subitems[siFieldName]);
        if Assigned(Fld) then
        begin
          // 2015-12-06: исправлен вывод числовых данных (было: "Row[Y, X] := GetFieldString(Fld, #0);")
          if IsNumericField(Fld)
          then Row[Y, X] := Fld.Value
          else Row[Y, X] := GetFieldString(Fld, #0);
        end
        else Row[Y, X] := SFieldNotFound;
      end;
    end;
    Inc(Y);
  end;

begin
  Result := False;
  with TFormExpExcel.Create(Application.MainForm) do
  begin
    try
      // Подготовка формы
      StartWait;
      ShowInStatusBar(SMsgPrepareOperation);
      try
        DataName := ADS.Name;
        FormName := ADS.Owner.Name;
        // Заполняем поля параметров экспорта
        edTitle.Text := ATitle;
        edNotes.Text := AComment;
        edOrientation.ItemIndex := Orientation;
        edFontSize.Value := DefaultFontSize;
        edPageName.Text := ASheetName;
        SaveCheckBox.Enabled := UseIniFile;
        SaveCheckBox.Checked := UseIniFile;
        FileBtn.Visible := UseIniFile;
        // Мультиобработка: 2007-09-13
        TotalRows := ADS.RecordCount;
        if Assigned(Grid) and (dgMultiSelect in Grid.Options)
          and (Grid.SelectedRows.Count > 1) then
        begin
          SelectedRows := Grid.SelectedRows.Count;
          edSendMode.ItemIndex := 1;
        end
        else begin
          SelectedRows := 1;
          edSendMode.ItemIndex := 0;
        end;
        edSendModeChange(nil);
        // Загружаем список полей
        LoadList_Fields(ADS, Grid);
        // Восстанавливаем параметры экспорта из INI-файла
        if UseIniFile then
          LoadExportParameters(GetModuleIniFile, True, False);
        // Выталкиваем наверх отмеченные поля
        MoveUpCheckedItems(ListView);
        // Выделяем верхний элемент
        if ListView.Items.Count > 0 then ListView.Selected := ListView.Items[0];
      finally
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;
      // Показываем форму
      if ShowModal = mrOk then
      begin
        // Экспорт данных
        StartWait;
        ShowInStatusBar(SMsgExportDataWait);
        try
          // Сохраняем параметры экспорта в INI-файле
          if UseIniFile then
            SaveExportParameters(GetModuleIniFile, True);
          // Считаем число записей
          SizeY := TotalRows;
          if edSendMode.ItemIndex = 1 then SizeY := SelectedRows;
          // Начинаем экспорт
          ShowInStatusBar(SMsgExportDataWait);
          ShowProgress(SMsgOpenExcel, SizeY + ListView.Items.Count * 2 + PrepareSteps);
          try
            // Подключение к Excel-ю
            if ConnectToMsExcel then
            begin
              try
                try
                  // Создаем книгу
                  UpdateProgressMessage(SMsgOpenWorkbook);
                  UpdateProgressStep(1);
                  if SetDefaultWorkbook then
                  begin
                    // Открываем книгу и создаем лист
                    UpdateProgressMessage(SMsgOpenWorksheet);
                    UpdateProgressStep(1);
                    Sheet := OpenSheetCW(1, edPageName.Text);
                    // Форматируем страницу
                    if not VarIsEmpty(Sheet) then
                    begin
                      UpdateProgressMessage(SMsgFormatPage);
                      // Устанавливаем вертикальные смещения
                      BaseY := 1;
                      if cbReportTitle.Checked then
                      begin
                        if ACopyright <> EmptyStr then Inc(BaseY, 2);
                        Inc(BaseY, 1);
                        if Trim(edNotes.Text) <> EmptyStr then Inc(BaseY, 1);
                        if cbDateCreate.Checked then Inc(BaseY, 1);
                        Inc(BaseY, 1);
                      end;
                      // Установка ориентации страницы
                      if edOrientation.ItemIndex > 0
                      then SetPageOrientation(Sheet, edOrientation.ItemIndex);
                      UpdateProgressStep(1);
                      // Форматируем столбцы
                      X := 0; SizeX := 0;
                      iCount := ListView.Items.Count - 1;
                      for i := 0 to iCount do
                      begin
                        if ListView.Items[i].Checked then
                        begin
                          Inc(X); Inc(SizeX);
                          Fld := ADS.FindField(ListView.Items[i].Subitems[siFieldName]);
                          if Assigned(Fld) and IsNumericField(Fld)
                          then SetColumnParams(Sheet.Columns[X],
                            edFontSize.Value * RStrToFloatDef(ListView.Items[i].SubItems[siWidth], DefaultColumnWidth) / DefaultFontSize,
                            AlignmentToExcelH(TKindId(ListView.Items[i].Data)^.Kind),
                            xlVAlignCenter, edFontSize.Value, DefaultNumberFormat)
                          else SetColumnParams(Sheet.Columns[X],
                            edFontSize.Value * RStrToFloatDef(ListView.Items[i].SubItems[siWidth], DefaultColumnWidth) / DefaultFontSize,
                            AlignmentToExcelH(TKindId(ListView.Items[i].Data)^.Kind),
                            xlVAlignCenter, edFontSize.Value, DefaultTextFormat);
                        end;
                        UpdateProgressStep(1);
                      end;
                      // Выводим заголовок страницы
                      if cbReportTitle.Checked then
                      begin
                        i := 1;
                        if (ACopyright <> EmptyStr) then
                        begin
                          CellText(Sheet.Cells[i, 1], ACopyright,
                            edFontSize.Value - 2, [fsItalic], intDisable, xlHAlignLeft, xlVAlignCenter, False, EmptyStr);
                          Inc(i, 2);
                        end;
                        CellText(Sheet.Cells[i, 1], edTitle.Text,
                          edFontSize.Value + 4, [fsBold], intDisable, xlHAlignLeft, xlVAlignCenter, False, EmptyStr);
                        Inc(i, 1);
                        if Trim(edNotes.Text) <> EmptyStr then
                        begin
                          CellText(Sheet.Cells[i, 1], edNotes.Text,
                            edFontSize.Value, [fsBold], intDisable, xlHAlignLeft, xlVAlignCenter, False, EmptyStr);
                          Inc(i, 1);
                        end;
                        if cbDateCreate.Checked then
                        begin
                          CellText(Sheet.Cells[i, 1],
                            Format(SMsgReportDate, [DateTimeToStr(Now)]),
                            edFontSize.Value, [fsItalic], intDisable, xlHAlignLeft, xlVAlignCenter, False, EmptyStr);
                        end;
                      end;
                      UpdateProgressStep(1);
                      // Выводим заголовки столбцов
                      if cbColumnsTitle.Checked then
                      begin
                        Row := VarArrayCreate([1, 1, 1, SizeX], varVariant);
                        // Генерируем массив данных
                        X := 0;
                        iCount := ListView.Items.Count - 1;
                        for i := 0 to iCount do
                        begin
                          if ListView.Items[i].Checked then
                          begin
                            Inc(X);
                            Row[1, X] := ListView.Items[i].Caption;
                          end;
                          UpdateProgressStep(1);
                        end;
                        // Передаем массив в Excel
                        RangeText(Sheet, BaseY, BaseY, 1, SizeX, Row,
                          edFontSize.Value, [], 3, xlHAlignCenter, xlVAlignCenter, True);
                        Inc(BaseY);
                      end;

                      // Экспорт данных
                      UpdateProgressMessage(SMsgPrepareData);
                      ADS.DisableControls;
                      try
                        Bmk := ADS.GetBookmark;
                        try
                          // Выделяем память под диапазон
                          Row := VarArrayCreate([BaseY, BaseY + SizeY - 1, 1, SizeX], varVariant);
                          // Генерируем массив данных
                          Y := BaseY;
                          case edSendMode.ItemIndex of
                            0: begin
                                 // Все записи: 2007-09-13
                                 ADS.First;
                                 while not ADS.Eof do
                                 begin
                                   PrepareRecordData(ListView);
                                   ADS.Next;
                                   UpdateProgressStep(1);
                                 end;
                               end;
                            1: begin
                                 // Выделенные записи: 2007-09-13
                                 if SizeY > 1 then
                                 begin
                                   // По списку записей
                                   for i := 0 to SizeY - 1 do
                                   begin
                                     try
                                       ADS.GotoBookmark(Pointer(Grid.SelectedRows[i]));
                                     except
                                       UpdateProgressStep(1);
                                       Continue;
                                     end;
                                     PrepareRecordData(ListView);
                                     UpdateProgressStep(1);
                                   end;
                                 end
                                 else begin
                                   // Текущая запись
                                   PrepareRecordData(ListView);
                                   UpdateProgressStep(1);
                                 end;
                               end;
                          end;
                          // Передаем массив в Excel
                          UpdateProgressMessage(SMsgTransferData);
                          RangeTextDefault(Sheet, BaseY, BaseY + SizeY - 1, 1, SizeX, Row, 2);
                          UpdateProgressStep(1);
                        finally
                          try
                            ADS.GotoBookmark(Bmk);
                          except
                          end;
                          ADS.FreeBookmark(Bmk);
                        end;
                      finally
                        ADS.EnableControls;
                      end;
                    end;
                  end;
                  // Возвращаем флаг успешного завершения
                  Result := True;
                finally
                  // Показываем скрытую копию Excel и отключаемся
                  ShowMsExcelAndDiconnect;
                end;
              except
                on E: Exception do
                  HandleExcept(E, nil, SErrExportExcel);
              end;
            end;
          finally
            CloseProgress;
          end;
        finally
          ShowInStatusBar(SMsgExportComplete);
          StopWait;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

{ == Экспорт данных из TListView в Excel ======================================= }
function ExportListViewToExcel(LV: TListView; const ATitle, ASheetName,
  ACopyright, AComment: string; const Orientation: Integer;
  const UseIniFile: Boolean = True): Boolean;
var
  Sheet, Row: Variant;
  i, iCount, j, jCount, X, Y, BaseY, SizeX, SizeY: Integer;
begin
  Result := False;
  with TFormExpExcel.Create(Application.MainForm) do
  begin
    try
      // Подготовка формы
      StartWait;
      ShowInStatusBar(SMsgPrepareOperation);
      try
        DataName := LV.Name;
        FormName := LV.Owner.Name;
        // Заполняем поля параметров экспорта
        edTitle.Text := ATitle;
        edNotes.Text := AComment;
        edOrientation.ItemIndex := Orientation;
        edFontSize.Value := DefaultFontSize;
        edPageName.Text := ASheetName;
        SaveCheckBox.Enabled := UseIniFile;
        SaveCheckBox.Checked := UseIniFile;
        FileBtn.Visible := UseIniFile;
        // Мультиобработка: 2007-09-13
        TotalRows := LV.Items.Count;
        if LV.MultiSelect and (LV.SelCount > 1) then
        begin
          SelectedRows := LV.SelCount;
          edSendMode.ItemIndex := 1;
        end
        else begin
          SelectedRows := 1;
          edSendMode.ItemIndex := 0;
        end;
        edSendModeChange(nil);
        // Загружаем список полей
        LoadList_Columns(LV);
        // Восстанавливаем параметры экспорта из INI-файла
        if UseIniFile then
          LoadExportParameters(GetModuleIniFile, True, False);
        // Выталкиваем наверх отмеченные поля
        MoveUpCheckedItems(ListView);
        // Выделяем верхний элемент
        if ListView.Items.Count > 0 then ListView.Selected := ListView.Items[0];
      finally
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;
      // Показываем форму
      if ShowModal = mrOk then
      begin
        // Экспорт данных
        StartWait;
        ShowInStatusBar(SMsgExportDataWait);
        try
          // Сохраняем параметры экспорта в INI-файле
          if UseIniFile then
            SaveExportParameters(GetModuleIniFile, True);
          // Считаем число записей
          SizeY := TotalRows;
          if edSendMode.ItemIndex = 1 then SizeY := SelectedRows;
          // Начинаем экспорт
          ShowInStatusBar(SMsgExportDataWait);
          ShowProgress(SMsgOpenExcel, SizeY + ListView.Items.Count * 2 + PrepareSteps);
          try
            // Подключение к Excel-ю
            if ConnectToMsExcel then
            begin
              try
                try
                  // Создаем книгу
                  UpdateProgressMessage(SMsgOpenWorkbook);
                  UpdateProgressStep(1);
                  if SetDefaultWorkbook then
                  begin
                    // Открываем книгу и создаем лист
                    UpdateProgressMessage(SMsgOpenWorksheet);
                    UpdateProgressStep(1);
                    Sheet := OpenSheetCW(1, edPageName.Text);
                    // Форматируем страницу
                    if not VarIsEmpty(Sheet) then
                    begin
                      UpdateProgressMessage(SMsgFormatPage);
                      // Устанавливаем вертикальные смещения
                      BaseY := 1;
                      if cbReportTitle.Checked then
                      begin
                        if ACopyright <> EmptyStr then Inc(BaseY, 2);
                        Inc(BaseY, 1);
                        if Trim(edNotes.Text) <> EmptyStr then Inc(BaseY, 1);
                        if cbDateCreate.Checked then Inc(BaseY, 1);
                        Inc(BaseY, 1);
                      end;
                      // Установка ориентации страницы
                      if edOrientation.ItemIndex > 0
                      then SetPageOrientation(Sheet, edOrientation.ItemIndex);
                      UpdateProgressStep(1);
                      // Форматируем столбцы
                      X := 0; SizeX := 0;
                      iCount := ListView.Items.Count - 1;
                      for i := 0 to iCount do
                      begin
                        if ListView.Items[i].Checked then
                        begin
                          Inc(X); Inc(SizeX);
                          SetColumnParams(Sheet.Columns[X],
                            RStrToFloatDef(ListView.Items[i].SubItems[siWidth], DefaultColumnWidth),
                            AlignmentToExcelH(TKindId(ListView.Items[i].Data)^.Kind),
                            xlVAlignCenter, edFontSize.Value, DefaultNumberFormat);
                        end;
                        UpdateProgressStep(1);
                      end;
                      // Выводим заголовок страницы
                      if cbReportTitle.Checked then
                      begin
                        i := 1;
                        if (ACopyright <> EmptyStr) then
                        begin
                          CellText(Sheet.Cells[i, 1], ACopyright,
                            edFontSize.Value - 2, [fsItalic], intDisable, xlHAlignLeft, xlVAlignCenter, False, EmptyStr);
                          Inc(i, 2);
                        end;
                        CellText(Sheet.Cells[i, 1], edTitle.Text,
                          edFontSize.Value + 4, [fsBold], intDisable, xlHAlignLeft, xlVAlignCenter, False, EmptyStr);
                        Inc(i, 1);
                        if Trim(edNotes.Text) <> EmptyStr then
                        begin
                          CellText(Sheet.Cells[i, 1], edNotes.Text,
                            edFontSize.Value, [fsBold], intDisable, xlHAlignLeft, xlVAlignCenter, False, EmptyStr);
                          Inc(i, 1);
                        end;
                        if cbDateCreate.Checked then
                        begin
                          CellText(Sheet.Cells[i, 1],
                            Format(SMsgReportDate, [DateTimeToStr(Now)]),
                            edFontSize.Value, [fsItalic], intDisable, xlHAlignLeft, xlVAlignCenter, False, EmptyStr);
                        end;
                      end;
                      UpdateProgressStep(1);
                      // Выводим заголовки столбцов
                      if cbColumnsTitle.Checked then
                      begin
                        Row := VarArrayCreate([1, 1, 1, SizeX], varVariant);
                        // Генерируем массив данных
                        X := 0;
                        iCount := ListView.Items.Count - 1;
                        for i := 0 to iCount do
                        begin
                          if ListView.Items[i].Checked then
                          begin
                            Inc(X);
                            Row[1, X] := ListView.Items[i].Caption;
                          end;
                          UpdateProgressStep(1);
                        end;
                        // Передаем массив в Excel
                        RangeText(Sheet, BaseY, BaseY, 1, SizeX, Row,
                          edFontSize.Value, [], 3, xlHAlignCenter, xlVAlignCenter, True);
                        Inc(BaseY);
                      end;
                      // Экспорт данных
                      UpdateProgressMessage(SMsgPrepareData);
                      // Выделяем память под диапазон
                      Row := VarArrayCreate([BaseY, BaseY + SizeY - 1, 1, SizeX], varVariant);
                      // Генерируем массив данных
                      Y := BaseY;
                      jCount := LV.Items.Count - 1;
                      for j := 0 to jCount do
                      begin
                        if (edSendMode.ItemIndex = 0)
                        or (LV.MultiSelect and LV.Items[j].Selected)
                        or (not LV.MultiSelect and (LV.Items[j] = LV.Selected)) then
                        begin
                          X := 0;
                          iCount := ListView.Items.Count - 1;
                          for i := 0 to iCount do
                          begin
                            if ListView.Items[i].Checked then
                            begin
                              Inc(X);
                              if TKindId(ListView.Items[i].Data)^.Id = 0
                              then Row[Y, X] := LV.Items[j].Caption
                              else Row[Y, X] := LV.Items[j].SubItems[TKindId(ListView.Items[i].Data)^.Id - 1];
                            end;
                          end;
                          Inc(Y);
                          UpdateProgressStep(1);
                        end;
                      end;
                      // Передаем массив в Excel
                      UpdateProgressMessage(SMsgTransferData);
                      RangeTextDefault(Sheet, BaseY, BaseY + SizeY - 1, 1, SizeX, Row, 2);
                      UpdateProgressStep(1);
                    end;
                  end;
                  // Возвращаем флаг успешного завершения
                  Result := True;
                finally
                  // Показываем скрытую копию Excel и отключаемся
                  ShowMsExcelAndDiconnect;
                end;
              except
                on E: Exception do
                  HandleExcept(E, nil, SErrExportExcel);
              end;
            end;
          finally
            CloseProgress;
          end;
        finally
          ShowInStatusBar(SMsgExportComplete);
          StopWait;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

{ == Процедуры диалогового окна ================================================ }

procedure TFormExpExcel.StartForm;
begin
  inherited;

  LoadListColumns(Self, ListView, True);

  cbReportTitleClick(nil);
  PageControlChange(nil);
end;

procedure TFormExpExcel.FreeForm;
begin
  SaveListColumns(Self, ListView, True);
end;

procedure TFormExpExcel.PageControlChange(Sender: TObject);
begin
  if Visible then
  begin
    case PageControl.ActivePageIndex of
      0: cbReportTitle.SetFocus;
      1: ListView.SetFocus;
    end;
  end;
end;

procedure TFormExpExcel.FileBtnClick(Sender: TObject);
var
  CursorPos: TPoint;
begin
  CursorPos.X := FileBtn.Left;
  CursorPos.Y := FileBtn.Top + FileBtn.Height;
  CursorPos := ButtonsMovedPanel.ClientToScreen(CursorPos);
  FilePopupMenu.Popup(CursorPos.X, CursorPos.Y);
end;

procedure TFormExpExcel.cbReportTitleClick(Sender: TObject);
begin
  // gbReportTitle.Enabled := cbReportTitle.Checked;
  if cbReportTitle.Checked then
  begin
    edTitle.Color := clWindow;
    edTitle.ReadOnly := False;
    edNotes.Color := clWindow;
    edNotes.ReadOnly := False;
  end
  else begin
    edTitle.Color := Color;
    edTitle.ReadOnly := True;
    edNotes.Color := Color;
    edNotes.ReadOnly := True;
  end;
  cbDateCreate.Enabled := cbReportTitle.Checked;
  gbReportTitle.TabStop := cbReportTitle.Checked;
end;

// Загрузка списка полей набора данных -----------------------------------------

procedure TFormExpExcel.LoadList_Fields(DS: TDataSet; DG: TDbGrid);
var
  i, iCount: Integer;
  Id: TKindId;
begin
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
    // Загружаем список полей TDbGrid
    if Assigned(DG) then
    begin
      iCount := DG.Columns.Count - 1;
      for i := 0 to iCount do
        with ListView.Items.Add do
        begin
          Caption := DG.Columns[i].Title.Caption;
          Checked := True;
          Subitems.Add(SAlignments[DG.Columns[i].Alignment]);
          Subitems.Add(FloatToStrF(DG.Columns[i].Width / WidthColumnToExcel, ffFixed, 10, 2));
          Subitems.Add(DG.Columns[i].Field.FieldName);
          New(Id);
          Id^.Id := DG.Columns[i].Field.Index;
          Id^.Kind := Integer(DG.Columns[i].Alignment);
          Data := Id;
        end;
    end;
    // Загружаем оставшиеся поля
    iCount := DS.FieldCount - 1;
    for i := 0 to iCount do
      if LV_FindKindId(ListView, DS.Fields[i].Index) = nil then
        with ListView.Items.Add do
        begin
          Caption := DS.Fields[i].DisplayName;
          Checked := DS.Fields[i].Visible and not Assigned(DG);
          Subitems.Add(SAlignments[DS.Fields[i].Alignment]);
          Subitems.Add(FloatToStrF(DS.Fields[i].DisplayWidth / WidthFieldToExcel, ffFixed, 10, 2));
          Subitems.Add(DS.Fields[i].FieldName);
          New(Id);
          Id^.Id := DS.Fields[i].Index;
          Id^.Kind := Integer(DS.Fields[i].Alignment);
          Data := Id;
        end;
  finally
    ListView.Items.EndUpdate;
  end;
end;

// Загрузка списка столбцов TListView ------------------------------------------

procedure TFormExpExcel.LoadList_Columns(LV: TListView);
var
  i, iCount: Integer;
  Id: TKindId;
begin
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
    iCount := LV.Columns.Count - 1;
    for i := 0 to iCount do
      with ListView.Items.Add do
      begin
        Caption := LV.Columns[i].Caption;
        Checked := True;
        Subitems.Add(SAlignments[LV.Columns[i].Alignment]);
        Subitems.Add(FloatToStrF(LV.Columns[i].Width / WidthColumnToExcel, ffFixed, 10, 2));
        Subitems.Add(LV.Columns[i].Caption);
        New(Id);
        Id^.Id := i;
        Id^.Kind := Integer(LV.Columns[i].Alignment);
        Data := Id;
      end;
  finally
    ListView.Items.EndUpdate;
  end;
end;

// Открыть файл ----------------------------------------------------------------

procedure TFormExpExcel.LoadExportParameters(const FileName: string; const LoadState, LoadTitle: Boolean);
var
  Ini: TMemIniFile;
  Item: TListItem;
  SectionName, ItemStr, FieldName: string;
  i, iCount, FieldId: Integer;
begin
  StartWait;
  ShowInStatusBar(SMsgLoadDataWait);
  try
    try
      SectionName := Format(iniSectionName, [AnsiUpperCase(FormName), AnsiUpperCase(DataName)]);
      Ini := TMemIniFile.Create(FileName);
      try
        if Ini.SectionExists(SectionName) then
        begin
          if LoadState
          then SaveCheckBox.Checked := Ini.ReadBool(SectionName, iniState, SaveCheckBox.Checked);
          if not LoadState or SaveCheckBox.Checked then
          begin
            cbReportTitle.Checked := Ini.ReadBool(SectionName, iniTitleEnabled, cbReportTitle.Checked);
            // Считываем параметры листа
            if LoadTitle or (Trim(edTitle.Text) = EmptyStr)
            then edTitle.Text := Ini.ReadString(SectionName, iniTitle, edTitle.Text);
            if Trim(edNotes.Text) = EmptyStr
            then edNotes.Text := Ini.ReadString(SectionName, iniNotes, edNotes.Text);
            cbDateCreate.Checked := Ini.ReadBool(SectionName, iniTimeCreate, cbDateCreate.Checked);
            edPageName.Text := Ini.ReadString(SectionName, iniPageName, edPageName.Text);
            edOrientation.ItemIndex := Ini.ReadInteger(SectionName, iniOrientation, edOrientation.ItemIndex);
            edFontSize.Value := Ini.ReadInteger(SectionName, iniBaseFontSize, edFontSize.Value);
            cbColumnsTitle.Checked := Ini.ReadBool(SectionName, iniColumnsTitle, cbColumnsTitle.Checked);
            // Считываем столбцы
            iCount := ListView.Items.Count - 1;
            for i := 0 to iCount do
            begin
              ItemStr := Ini.ReadString(SectionName, Format(iniItem, [i]), EmptyStr);
              try
                if ItemStr <> EmptyStr then
                begin
                  if WordCount(ItemStr, chDelims) = 6 then
                  begin
                    FieldId := StrToIntDef(Trim(ExtractWord(1, ItemStr, chDelims)), intDisable);
                    FieldName := Trim(ExtractWord(2, ItemStr, chDelims));
                    Item := LV_FindKindId(ListView, FieldId);
                    if Assigned(Item) and SameText(Item.SubItems[siFieldName], FieldName) then
                    begin
                      if Item.Index <> i then Item := MoveItemTo(ListView, Item, i);
                      Item.Caption := ExtractWord(3, ItemStr, chDelims);
                      Item.Checked := StrToBoolDef(Trim(ExtractWord(4, ItemStr, chDelims)), Item.Checked);
                      TKindId(Item.Data)^.Kind := StrToIntDef(Trim(ExtractWord(5, ItemStr, chDelims)), TKindId(Item.Data)^.Kind);
                      Item.SubItems[siAlignment] := SAlignments[TAlignment(TKindId(Item.Data)^.Kind)];
                      Item.SubItems[siWidth] := Trim(ExtractWord(6, ItemStr, chDelims));
                    end
                    else begin
                      Ini.DeleteKey(SectionName, Format(iniItem, [i]));
                      // raise Exception.CreateFmt(rsErrFieldNotFound, [FieldName, FieldId, SectionName]);
                    end
                  end
                  else begin
                    Ini.DeleteKey(SectionName, Format(iniItem, [i]));
                    // raise Exception.CreateFmt(rsErrStringNotCorrected, [ItemStr]);
                  end;
                end;
              except
                on E: Exception do
                  HandleExcept(E, nil, Format(rsErrLoadFieldParameters, [i]));
              end;
            end;
          end;
        end;
        // else if not LoadState then raise Exception.CreateFmt(rsErrParamsNotFound, [DataName, FileName]);
        cbReportTitleClick(nil);
      finally
        Ini.Free;
      end;
    except
      on E: Exception do
        HandleExcept(E, nil, Format(rsErrLoadExportParameters, [FileName]));
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TFormExpExcel.OpenFileUpdate(Sender: TObject);
begin
  OpenFile.Enabled := IsNotWait and SaveCheckBox.Enabled;
end;

procedure TFormExpExcel.OpenFileExecute(Sender: TObject);
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(Self);
  try
    Dialog.Filter := rsDlgFilter;
    Dialog.DefaultExt := rsDlgDefaultExt;
    Dialog.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
    Dialog.InitialDir := ExtractFilePath(GetApplicationFileName);
    if Dialog.Execute then LoadExportParameters(Dialog.FileName, False, True);
  finally
    Dialog.Free;
  end;
end;

// Сохранить в файле -----------------------------------------------------------

procedure TFormExpExcel.SaveExportParameters(const FileName: string; const SaveState: Boolean);
var
  Ini: TMemIniFile;
  SectionName: string;
  i, iCount: Integer;
begin
  StartWait;
  ShowInStatusBar(SMsgSaveDataWait);
  try
    try
      SectionName := Format(iniSectionName, [AnsiUpperCase(FormName), AnsiUpperCase(DataName)]);
      Ini := TMemIniFile.Create(FileName);
      try
        // Удаляем секцию
        Ini.EraseSection(SectionName);
        // Сохраняем параметры листа
        if SaveState then Ini.WriteBool(SectionName, iniState, SaveCheckBox.Checked);
        Ini.WriteBool(SectionName, iniTitleEnabled, cbReportTitle.Checked);
        Ini.WriteString(SectionName, iniTitle, edTitle.Text);
        Ini.WriteString(SectionName, iniNotes, edNotes.Text);
        Ini.WriteBool(SectionName, iniTimeCreate, cbDateCreate.Checked);
        Ini.WriteString(SectionName, iniPageName, edPageName.Text);
        Ini.WriteInteger(SectionName, iniOrientation, edOrientation.ItemIndex);
        Ini.WriteInteger(SectionName, iniBaseFontSize, edFontSize.Value);
        Ini.WriteBool(SectionName, iniColumnsTitle, cbColumnsTitle.Checked);
        // Сохраняем поля
        iCount := ListView.Items.Count - 1;
        for i := 0 to iCount do
          Ini.WriteString(SectionName,
            Format(iniItem, [i]),
            Format(iniValue,
              [TKindId(ListView.Items[i].Data)^.Id,
               Trim(ListView.Items[i].SubItems[siFieldName]),
               ListView.Items[i].Caption,
               BoolToStr(ListView.Items[i].Checked),
               TKindId(ListView.Items[i].Data)^.Kind,
               ListView.Items[i].SubItems[siWidth]]));
      finally
        Ini.UpdateFile;
        Ini.Free;
      end;
    except
      on E: Exception do
        HandleExcept(E, nil, Format(rsErrSaveExportParameters, [FileName]));
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TFormExpExcel.SaveFileUpdate(Sender: TObject);
begin
  SaveFile.Enabled := IsNotWait and SaveCheckBox.Enabled;
end;

procedure TFormExpExcel.SaveFileExecute(Sender: TObject);
var
  Dialog: TSaveDialog;
begin
  Dialog := TSaveDialog.Create(Self);
  try
    Dialog.Filter := rsDlgFilter;
    Dialog.DefaultExt := rsDlgDefaultExt;
    Dialog.Options := [ofHideReadOnly, ofPathMustExist, ofCreatePrompt, ofShareAware, ofEnableSizing];
    Dialog.InitialDir := ExtractFilePath(Application.ExeName);
    if Dialog.Execute then SaveExportParameters(Dialog.FileName, False);
  finally
    Dialog.Free;
  end;
end;

// Вверх -----------------------------------------------------------------------

procedure TFormExpExcel.MoveUpUpdate(Sender: TObject);
begin
  MoveUp.Enabled := IsNotWait and Assigned(ListView.Selected)
    and (ListView.Selected.Index > 0);
end;

procedure TFormExpExcel.MoveUpExecute(Sender: TObject);
begin
  StartWait;
  try
    MoveSelectedItemUp(ListView);
    MoveUpCheckedItems(ListView);
    ListView.SetFocus;
  finally
    StopWait;
  end;
end;

// Вниз ------------------------------------------------------------------------

procedure TFormExpExcel.MoveDownUpdate(Sender: TObject);
begin
  MoveDown.Enabled := IsNotWait and Assigned(ListView.Selected)
    and (ListView.Selected.Index < ListView.Items.Count - 1);
end;

procedure TFormExpExcel.MoveDownExecute(Sender: TObject);
begin
  StartWait;
  try
    MoveSelectedItemDown(ListView);
    MoveUpCheckedItems(ListView);
    ListView.SetFocus;
  finally
    StopWait;
  end;
end;

procedure TFormExpExcel.ListViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
  HitTest: THitTests;
begin
  Item := ListView.GetItemAt(x, y);
  HitTest := ListView.GetHitTestInfoAt(x, y);
  if Assigned(Item) and (htOnStateIcon in HitTest)
  then ItemChecked := Item
  else ItemChecked := nil;
end;

procedure TFormExpExcel.ListViewClick(Sender: TObject);
begin
  if Assigned(ItemChecked) then
  begin
    StartWait;
    try
      ListView.Selected := ItemChecked;
      MoveUpCheckedItems(ListView);
      ScrollToSelectedItem(ListView);
      ItemChecked := nil;
    finally
      StopWait;
    end;
  end;
end;

procedure TFormExpExcel.ListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 32 then
  begin
    StartWait;
    try
      MoveUpCheckedItems(ListView);
      ScrollToSelectedItem(ListView);
    finally
      StopWait;
    end;
  end;
end;

// Свойства --------------------------------------------------------------------

procedure TFormExpExcel.RenameUpdate(Sender: TObject);
begin
  Rename.Enabled := IsNotWait and Assigned(ListView.Selected);
end;

procedure TFormExpExcel.RenameExecute(Sender: TObject);
begin
  with TFormExpExcelParam.Create(Self) do
  begin
    try
      StartWait;
      try
        AlignComboBox.Items.BeginUpdate;
        try
          AlignComboBox.Items.Clear;
          AlignComboBox.Items.Add(SAlignmentLeft);
          AlignComboBox.Items.Add(SAlignmentRight);
          AlignComboBox.Items.Add(SAlignmentCenter);
        finally
          AlignComboBox.Items.EndUpdate;
        end;
        CaptionEdit.Text := ListView.Selected.Caption;
        AlignComboBox.ItemIndex := TKindId(ListView.Selected.Data)^.Kind;
        WidthEdit.Value := RStrToFloatDef(ListView.Selected.SubItems[siWidth], DefaultColumnWidth);
      finally
        StopWait;
      end;
      if ShowModal = mrOk then
      begin
        StartWait;
        try
          ListView.Selected.Caption := CaptionEdit.Text;
          TKindId(ListView.Selected.Data)^.Kind := AlignComboBox.ItemIndex;
          ListView.Selected.SubItems[siAlignment] := AlignComboBox.Text;
          ListView.Selected.SubItems[siWidth] := FloatToStrF(WidthEdit.Value, ffFixed, 10, 2)
        finally
          StopWait;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TFormExpExcel.ListViewDblClick(Sender: TObject);
begin
  if Rename.Enabled then RenameExecute(Sender);
end;

procedure TFormExpExcel.edSendModeChange(Sender: TObject);
begin
  case edSendMode.ItemIndex of
    0: edRecNum.Caption := IntToStr(TotalRows);
    1: edRecNum.Caption := IntToStr(SelectedRows);
  end;
end;

end.
