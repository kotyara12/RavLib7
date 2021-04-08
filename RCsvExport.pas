unit RCsvExport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls, Db, DbGrids,
  Mask, ToolEdit, Menus, ImgList, ActnList;

type
  TFormCsvExport = class(TDialogTemplate)
    PageControl: TPageControl;
    tsFile: TTabSheet;
    tsColumns: TTabSheet;
    SaveCheckBox: TCheckBox;
    btnMoveDown: TBitBtn;
    btnMoveUp: TBitBtn;
    lblColumns: TLabel;
    lvColumns: TListView;
    lblFilename: TLabel;
    edFilename: TFilenameEdit;
    FileBtn: TBitBtn;
    ActionList: TActionList;
    OpenFile: TAction;
    SaveFile: TAction;
    MoveUp: TAction;
    MoveDown: TAction;
    ImageList: TImageList;
    FilePopupMenu: TPopupMenu;
    itemOpenFile: TMenuItem;
    itemSaveFile: TMenuItem;
    ListPopupMenu: TPopupMenu;
    itemMoveUp: TMenuItem;
    itemMoveDown: TMenuItem;
    gbSendMode: TGroupBox;
    SendModeComboBoxLabel: TLabel;
    SendModeComboBox: TComboBox;
    RecNum: TStaticText;
    rgDivider: TRadioGroup;
    cbExportHeaders: TCheckBox;
    cbQuotes: TCheckBox;
    lblCodePage: TLabel;
    edCodePage: TComboBox;
    procedure PageControlChange(Sender: TObject);
    procedure UpdateButtons(Sender: TObject);
    procedure FileBtnClick(Sender: TObject);
    procedure SendModeComboBoxChange(Sender: TObject);
    procedure OpenFileUpdate(Sender: TObject);
    procedure OpenFileExecute(Sender: TObject);
    procedure SaveFileUpdate(Sender: TObject);
    procedure SaveFileExecute(Sender: TObject);
    procedure MoveUpUpdate(Sender: TObject);
    procedure MoveUpExecute(Sender: TObject);
    procedure MoveDownUpdate(Sender: TObject);
    procedure MoveDownExecute(Sender: TObject);
    procedure lvColumnsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lvColumnsClick(Sender: TObject);
    procedure lvColumnsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    sFormName: string;
    sDataName: string;
    fRowsTotal: Integer;
    fRowsSelected: Integer;
    ItemChecked: TListItem;
  protected
    procedure StartForm; override;
    procedure FreeForm; override;
  public
    procedure LoadList_Fields(ADS: TDataSet; Grid: TDbGrid);
    procedure LoadList_Columns(LV: TListView);
    procedure LoadExportParameters(const FileName: string; const LoadState: Boolean);
    procedure SaveExportParameters(const FileName: string; const SaveState: Boolean);
  end;

function ExportDataSetToCsv(ADS: TDataSet; Grid: TDbGrid; var FileName: string;
  const UseIniFile: Boolean = True): Integer;
function ExportListViewToCsv(LV: TListView; var FileName: string;
  const UseIniFile: Boolean = True): Integer;

implementation

uses
  IniFiles, RxStrUtils, StrUtils, RStrUtils, RDbUtils,
  RVclUtils, RDialogs, RProgress, RMsgRu, RListView, RSysUtils, RExHandlers,
  RFrmStorage, RDbConst;

{$R *.dfm}

resourcestring
  SErrLoadExportParameters  = 'Ошибка загрузки параметров экспорта из файла "%s"!';
  SErrSaveExportParameters  = 'Ошибка сохранения параметров экспорта в файле "%s"!';
  SErrLoadFieldParameters   = 'Ошибка загрузки параметров экспортируемого поля ID="%d"!';

  SDlgFilter                = 'Файлы параметров экспорта (*.exc)|*.exc|Все файлы (*.*)|*.*';
  SDlgDefaultExt            = 'exc';

const
  iniSectionName            = 'EXPORT_CSV_%s.%s';
  iniDivider                = 'Divider';
  iniCodePage               = 'CodePage';
  iniHeaders                = 'Headers';
  iniQuoted                 = 'Quoted';
  iniState                  = 'RestoreParameters';
  iniItem                   = 'Item_%d';
  iniValue                  = '%d;%s;%s;%s';
  chDelims                  = [';'];

  siFieldName               = 0;

  chQuote                   = '"';

function ExportDataSetToCsv(ADS: TDataSet; Grid: TDbGrid; var FileName: string;
  const UseIniFile: Boolean = True): Integer;
var
  fOut: TextFile;
  sDiv: string;
  iRow, iRows: Integer;
  Bmk: TBookmark;

  function GenerateLineHead(lvColumns: TListView; const sDiv: string; const bQuoued: Boolean): string;
  var
    i, iCount: Integer;
    Fld: TField;
  begin
    Result := EmptyStr;

    iCount := lvColumns.Items.Count - 1;
    for i := 0 to iCount do
    begin
      if lvColumns.Items[i].Checked then
      begin
        Fld := ADS.FindField(lvColumns.Items[i].Subitems[siFieldName]);
        if Assigned(Fld) then
        begin
          if bQuoued
          then Result := AddDelimStrEx(Result, AnsiQuotedStr(Fld.FieldName, chQuote), sDiv)
          else Result := AddDelimStrEx(Result, Fld.FieldName, sDiv);
        end;
      end;
    end;
  end;

  function GenerateLineData(lvColumns: TListView; const sDiv: string; const bQuoued: Boolean): string;
  var
    i, iCount: Integer;
    Fld: TField;
  begin
    Result := EmptyStr;

    iCount := lvColumns.Items.Count - 1;
    for i := 0 to iCount do
    begin
      if lvColumns.Items[i].Checked then
      begin
        Fld := ADS.FindField(lvColumns.Items[i].Subitems[siFieldName]);
        if Assigned(Fld) then
        begin
          if bQuoued
          then Result := AddDelimStrEx(Result, GetFieldString(Fld, chQuote), sDiv)
          else Result := AddDelimStrEx(Result, GetFieldString(Fld, #0), sDiv);
        end;
      end;
    end;
  end;

begin
  Result := -1;

  with TFormCsvExport.Create(Application.MainForm) do
  begin
    try
      StartWait;
      ShowInStatusBar(SMsgPrepareOperation);
      try
        sDataName := ADS.Name;
        sFormName := ADS.Owner.Name;
        edFilename.FileName := FileName;
        // Мультиобработка
        fRowsTotal := ADS.RecordCount;
        if Assigned(Grid) and (dgMultiSelect in Grid.Options)
          and (Grid.SelectedRows.Count > 1) then
        begin
          fRowsSelected := Grid.SelectedRows.Count;
          SendModeComboBox.ItemIndex := 1;
        end
        else begin
          fRowsSelected := 1;
          SendModeComboBox.ItemIndex := 0;
        end;
        SendModeComboBoxChange(nil);
        // Загружаем список полей
        LoadList_Fields(ADS, Grid);
        // Восстанавливаем параметры экспорта из INI-файла
        if UseIniFile then
          LoadExportParameters(GetModuleIniFile, True);
        // Выталкиваем наверх отмеченные поля
        MoveUpCheckedItems(lvColumns);
        // Выделяем верхний элемент
        if lvColumns.Items.Count > 0 then
          lvColumns.Selected := lvColumns.Items[0];
      finally
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;

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
          iRows := fRowsTotal;
          if SendModeComboBox.ItemIndex = 1 then
            iRows := fRowsSelected;
          // Другие параметры
          case rgDivider.ItemIndex of
            1: sDiv := ',';
            2: sDiv := #9;
            else sDiv := ';';
          end;
          // Начинаем экспорт
          ShowInStatusBar(SMsgExportDataWait);
          if cbExportHeaders.Checked
          then ShowProgress(SMsgExportDataWait, iRows + 1)
          else ShowProgress(SMsgExportDataWait, iRows);
          try
            try
              AssignFile(fOut, edFilename.FileName);
              Rewrite(fOut);
              try
                if cbExportHeaders.Checked then
                begin
                  case edCodePage.ItemIndex of
                    1: WriteLn(fOut, Utf8Encode(GenerateLineHead(lvColumns, sDiv, cbQuotes.Checked)));
                    else WriteLn(fOut, GenerateLineHead(lvColumns, sDiv, cbQuotes.Checked));
                  end;
                  UpdateProgressStep(1);
                end;

                ADS.DisableControls;
                try
                  Bmk := ADS.GetBookmark;
                  try
                    Result := 0;
                    case SendModeComboBox.ItemIndex of
                      0: begin
                           ADS.First;
                           while not ADS.Eof do
                           begin
                             case edCodePage.ItemIndex of
                               1: WriteLn(fOut, Utf8Encode(GenerateLineData(lvColumns, sDiv, cbQuotes.Checked)));
                               else WriteLn(fOut, GenerateLineData(lvColumns, sDiv, cbQuotes.Checked));
                             end;
                             Inc(Result);
                             ADS.Next;
                             UpdateProgressStep(1);
                           end;
                         end;
                      1: begin
                           if iRows > 1 then
                           begin
                             // По списку записей
                             for iRow := 0 to iRows - 1 do
                             begin
                               try
                                 ADS.GotoBookmark(Pointer(Grid.SelectedRows[iRow]));
                               except
                                 UpdateProgressStep(1);
                                 Continue;
                               end;
                               WriteLn(fOut, GenerateLineData(lvColumns, sDiv, cbQuotes.Checked));
                               Inc(Result);
                               UpdateProgressStep(1);
                             end;
                           end
                           else begin
                             // Текущая запись
                             WriteLn(fOut, GenerateLineData(lvColumns, sDiv, cbQuotes.Checked));
                             Inc(Result);
                             UpdateProgressStep(1);
                           end;
                         end;
                      end;
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
              finally
                CloseFile(fOut);
              end;
            except
              on E: Exception do
                HandleExcept(E, nil, Format(SErrExportFile, [edFilename.FileName]));
            end;
          finally
            CloseProgress;
          end;
        finally
          ShowInStatusBar(SMsgOperationComplete);
          StopWait;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function ExportListViewToCsv(LV: TListView; var FileName: string;
  const UseIniFile: Boolean = True): Integer;
var
  fOut: TextFile;
  sDiv: string;
  iRow, iRows: Integer;

  function GenerateLineHead(lvColumns: TListView; const sDiv: string; const bQuoued: Boolean): string;
  var
    i, iCount: Integer;
  begin
    Result := EmptyStr;

    iCount := lvColumns.Items.Count - 1;
    for i := 0 to iCount do
    begin
      if lvColumns.Items[i].Checked then
      begin
        if bQuoued
        then Result := AddDelimStrEx(Result, AnsiQuotedStr(lvColumns.Items[i].Caption, chQuote), sDiv)
        else Result := AddDelimStrEx(Result, lvColumns.Items[i].Caption, sDiv);
      end;
    end;
  end;

  function GenerateLineData(lvColumns: TListView; const iRow: Integer; const sDiv: string; const bQuoued: Boolean): string;
  var
    i, iCount, iCol: Integer;
    sValue: string;
  begin
    Result := EmptyStr;

    iCount := lvColumns.Items.Count - 1;
    for i := 0 to iCount do
    begin
      if lvColumns.Items[i].Checked then
      begin
        iCol := TId(lvColumns.Items[i].Data)^;
        if iCol = 0
        then sValue := LV.Items[iRow].Caption
        else sValue := LV.Items[iRow].Subitems[iCol - 1];
        if bQuoued
        then Result := AddDelimStrEx(Result, AnsiQuotedStr(sValue, chQuote), sDiv)
        else Result := AddDelimStrEx(Result, sValue, sDiv);
      end;
    end;
  end;

begin
  Result := -1;

  with TFormCsvExport.Create(Application.MainForm) do
  begin
    try
      StartWait;
      ShowInStatusBar(SMsgPrepareOperation);
      try
        sDataName := LV.Name;
        sFormName := LV.Owner.Name;
        edFilename.FileName := FileName;
        // Мультиобработка
        fRowsTotal := LV.Items.Count;
        if LV.MultiSelect and (LV.SelCount > 1) then
        begin
          fRowsSelected := LV.SelCount;
          SendModeComboBox.ItemIndex := 1;
        end
        else begin
          fRowsSelected := 1;
          SendModeComboBox.ItemIndex := 0;
        end;
        SendModeComboBoxChange(nil);
        // Загружаем список полей
        LoadList_Columns(LV);
        // Восстанавливаем параметры экспорта из INI-файла
        if UseIniFile then
          LoadExportParameters(GetModuleIniFile, True);
        // Выталкиваем наверх отмеченные поля
        MoveUpCheckedItems(lvColumns);
        // Выделяем верхний элемент
        if lvColumns.Items.Count > 0 then
          lvColumns.Selected := lvColumns.Items[0];
      finally
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;

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
          iRows := fRowsTotal;
          // Другие параметры
          case rgDivider.ItemIndex of
            1: sDiv := ',';
            2: sDiv := #9;
            else sDiv := ';';
          end;
          // Начинаем экспорт
          ShowInStatusBar(SMsgExportDataWait);
          if cbExportHeaders.Checked
          then ShowProgress(SMsgExportDataWait, iRows + 1)
          else ShowProgress(SMsgExportDataWait, iRows);
          try
            try
              AssignFile(fOut, edFilename.FileName);
              Rewrite(fOut);
              try
                if cbExportHeaders.Checked then
                begin
                  case edCodePage.ItemIndex of
                    1: WriteLn(fOut, Utf8Encode(GenerateLineHead(lvColumns, sDiv, cbQuotes.Checked)));
                    else WriteLn(fOut, GenerateLineHead(lvColumns, sDiv, cbQuotes.Checked));
                  end;
                  UpdateProgressStep(1);
                end;

                Result := 0;
                for iRow := 0 to iRows - 1 do
                begin
                  if (SendModeComboBox.ItemIndex = 0)
                  or (LV.MultiSelect and LV.Items[iRow].Selected)
                  or (not LV.MultiSelect and (LV.Items[iRow] = LV.Selected)) then
                  begin
                    case edCodePage.ItemIndex of
                      1: WriteLn(fOut, Utf8Encode(GenerateLineData(lvColumns, iRow, sDiv, cbQuotes.Checked)));
                      else WriteLn(fOut, GenerateLineData(lvColumns, iRow, sDiv, cbQuotes.Checked));
                    end;
                    Inc(Result);
                  end;
                  UpdateProgressStep(1);
                end;
              finally
                CloseFile(fOut);
              end;
            except
              on E: Exception do
                HandleExcept(E, nil, Format(SErrExportFile, [edFilename.FileName]));
            end;
          finally
            CloseProgress;
          end;
        finally
          ShowInStatusBar(SMsgOperationComplete);
          StopWait;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

{ == TFormCsvExport ============================================================ }

procedure TFormCsvExport.StartForm;
begin
  inherited;

  LoadListColumns(Self, lvColumns, True);

  PageControlChange(nil);
  UpdateButtons(nil);
end;

procedure TFormCsvExport.FreeForm;
begin
  SaveListColumns(Self, lvColumns, True);

  inherited;
end;

procedure TFormCsvExport.PageControlChange(Sender: TObject);
begin
  if Visible then
  begin
    case PageControl.ActivePageIndex of
      0: edFilename.SetFocus;
      1: lvColumns.SetFocus;
    end;
  end;
end;

procedure TFormCsvExport.UpdateButtons(Sender: TObject);
begin
  OkBtn.Enabled := edFilename.FileName <> '';
end;

procedure TFormCsvExport.FileBtnClick(Sender: TObject);
var
  CursorPos: TPoint;
begin
  CursorPos.X := FileBtn.Left;
  CursorPos.Y := FileBtn.Top + FileBtn.Height;
  CursorPos := ButtonsMovedPanel.ClientToScreen(CursorPos);
  FilePopupMenu.Popup(CursorPos.X, CursorPos.Y);
end;

procedure TFormCsvExport.LoadList_Fields(ADS: TDataSet; Grid: TDbGrid);
var
  i, iCount: Integer;
  Id: TId;
begin
  lvColumns.Items.BeginUpdate;
  try
    lvColumns.Items.Clear;
    // Загружаем список полей TDbGrid
    if Assigned(Grid) then
    begin
      iCount := Grid.Columns.Count - 1;
      for i := 0 to iCount do
        with lvColumns.Items.Add do
        begin
          Checked := True;
          Caption := Grid.Columns[i].Title.Caption;
          Subitems.Add(Grid.Columns[i].Field.FieldName);
          New(Id);
          Id^ := Grid.Columns[i].Field.Index;
          Data := Id;
        end;
    end;
    // Загружаем оставшиеся поля
    iCount := ADS.FieldCount - 1;
    for i := 0 to iCount do
      if LV_FindKindId(lvColumns, ADS.Fields[i].Index) = nil then
        with lvColumns.Items.Add do
        begin
          Caption := ADS.Fields[i].DisplayName;
          Checked := ADS.Fields[i].Visible and not Assigned(Grid);
          Subitems.Add(ADS.Fields[i].FieldName);
          New(Id);
          Id^ := ADS.Fields[i].Index;
          Data := Id;
        end;
  finally
    lvColumns.Items.EndUpdate;
  end;
end;

procedure TFormCsvExport.LoadList_Columns(LV: TListView);
var
  i, iCount: Integer;
  Id: TId;
begin
  lvColumns.Items.BeginUpdate;
  try
    lvColumns.Items.Clear;
    iCount := LV.Columns.Count - 1;
    for i := 0 to iCount do
      with lvColumns.Items.Add do
      begin
        Checked := True;
        Caption := LV.Columns[i].Caption;
        Subitems.Add(LV.Columns[i].Caption);
        New(Id);
        Id^ := i;
        Data := Id;
      end;
  finally
    lvColumns.Items.EndUpdate;
  end;
end;

procedure TFormCsvExport.LoadExportParameters(const FileName: string; const LoadState: Boolean);
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
      SectionName := Format(iniSectionName, [AnsiUpperCase(sFormName), AnsiUpperCase(sDataName)]);
      Ini := TMemIniFile.Create(FileName);
      try
        if Ini.SectionExists(SectionName) then
        begin
          if LoadState then
            SaveCheckBox.Checked := Ini.ReadBool(SectionName, iniState, SaveCheckBox.Checked);
          if not LoadState or SaveCheckBox.Checked then
          begin
            rgDivider.ItemIndex := Ini.ReadInteger(SectionName, iniDivider, rgDivider.ItemIndex);
            edCodePage.ItemIndex := Ini.ReadInteger(SectionName, iniCodePage, edCodePage.ItemIndex);
            cbExportHeaders.Checked := Ini.ReadBool(SectionName, iniHeaders, cbExportHeaders.Checked);
            cbQuotes.Checked := Ini.ReadBool(SectionName, iniQuoted, cbQuotes.Checked);
            // Считываем столбцы
            iCount := lvColumns.Items.Count - 1;
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
                    Item := LV_FindKindId(lvColumns, FieldId);
                    if Assigned(Item) and SameText(Item.SubItems[siFieldName], FieldName) then
                    begin
                      if Item.Index <> i then Item := MoveItemTo(lvColumns, Item, i);
                      Item.Caption := ExtractWord(3, ItemStr, chDelims);
                      Item.Checked := StrToBoolDef(Trim(ExtractWord(4, ItemStr, chDelims)), Item.Checked);
                    end
                    else begin
                      Ini.DeleteKey(SectionName, Format(iniItem, [i]));
                      // raise Exception.CreateFmt(SErrFieldNotFound, [FieldName, FieldId, SectionName]);
                    end
                  end
                  else begin
                    Ini.DeleteKey(SectionName, Format(iniItem, [i]));
                    // raise Exception.CreateFmt(SErrStringNotCorrected, [ItemStr]);
                  end;
                end;
              except
                on E: Exception do
                  HandleExcept(E, nil, Format(SErrLoadFieldParameters, [i]));
              end;
            end;
          end;
        end;
        // else if not LoadState then raise Exception.CreateFmt(SErrParamsNotFound, [DataName, FileName]);
      finally
        Ini.Free;
      end;
    except
      on E: Exception do
        HandleExcept(E, nil, Format(SErrLoadExportParameters, [FileName]));
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TFormCsvExport.OpenFileUpdate(Sender: TObject);
begin
  OpenFile.Enabled := IsNotWait and SaveCheckBox.Enabled;
end;

procedure TFormCsvExport.OpenFileExecute(Sender: TObject);
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(Self);
  try
    Dialog.Filter := SDlgFilter;
    Dialog.DefaultExt := SDlgDefaultExt;
    Dialog.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
    Dialog.InitialDir := ExtractFilePath(GetApplicationFileName);
    if Dialog.Execute then LoadExportParameters(Dialog.FileName, False);
  finally
    Dialog.Free;
  end;
end;

procedure TFormCsvExport.SaveExportParameters(const FileName: string; const SaveState: Boolean);
var
  Ini: TMemIniFile;
  SectionName: string;
  i, iCount: Integer;
begin
  StartWait;
  ShowInStatusBar(SMsgSaveDataWait);
  try
    try
      SectionName := Format(iniSectionName, [AnsiUpperCase(sFormName), AnsiUpperCase(sDataName)]);
      Ini := TMemIniFile.Create(FileName);
      try
        // Удаляем секцию
        Ini.EraseSection(SectionName);
        // Сохраняем параметры
        if SaveState then
          Ini.WriteBool(SectionName, iniState, SaveCheckBox.Checked);
        Ini.WriteInteger(SectionName, iniDivider, rgDivider.ItemIndex);
        Ini.WriteInteger(SectionName, iniCodePage, edCodePage.ItemIndex);
        Ini.WriteBool(SectionName, iniHeaders, cbExportHeaders.Checked);
        Ini.WriteBool(SectionName, iniQuoted, cbQuotes.Checked);
        // Сохраняем поля
        iCount := lvColumns.Items.Count - 1;
        for i := 0 to iCount do
          Ini.WriteString(SectionName,
            Format(iniItem, [i]),
            Format(iniValue,
              [TId(lvColumns.Items[i].Data)^,
               Trim(lvColumns.Items[i].SubItems[siFieldName]),
               lvColumns.Items[i].Caption,
               BoolToStr(lvColumns.Items[i].Checked)]));
      finally
        Ini.UpdateFile;
        Ini.Free;
      end;
    except
      on E: Exception do
        HandleExcept(E, nil, Format(SErrSaveExportParameters, [FileName]));
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TFormCsvExport.SaveFileUpdate(Sender: TObject);
begin
  SaveFile.Enabled := IsNotWait and SaveCheckBox.Enabled;
end;

procedure TFormCsvExport.SaveFileExecute(Sender: TObject);
var
  Dialog: TSaveDialog;
begin
  Dialog := TSaveDialog.Create(Self);
  try
    Dialog.Filter := SDlgFilter;
    Dialog.DefaultExt := SDlgDefaultExt;
    Dialog.Options := [ofHideReadOnly, ofPathMustExist, ofCreatePrompt, ofShareAware, ofEnableSizing];
    Dialog.InitialDir := ExtractFilePath(Application.ExeName);
    if Dialog.Execute then SaveExportParameters(Dialog.FileName, False);
  finally
    Dialog.Free;
  end;
end;

procedure TFormCsvExport.MoveUpUpdate(Sender: TObject);
begin
  MoveUp.Enabled := IsNotWait and Assigned(lvColumns.Selected)
    and (lvColumns.Selected.Index > 0);
end;

procedure TFormCsvExport.MoveUpExecute(Sender: TObject);
begin
  StartWait;
  try
    MoveSelectedItemUp(lvColumns);
    MoveUpCheckedItems(lvColumns);
    lvColumns.SetFocus;
  finally
    StopWait;
  end;
end;

procedure TFormCsvExport.MoveDownUpdate(Sender: TObject);
begin
  MoveDown.Enabled := IsNotWait and Assigned(lvColumns.Selected)
    and (lvColumns.Selected.Index < lvColumns.Items.Count - 1);
end;

procedure TFormCsvExport.MoveDownExecute(Sender: TObject);
begin
  StartWait;
  try
    MoveSelectedItemDown(lvColumns);
    MoveUpCheckedItems(lvColumns);
    lvColumns.SetFocus;
  finally
    StopWait;
  end;
end;

procedure TFormCsvExport.lvColumnsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
  HitTest: THitTests;
begin
  Item := lvColumns.GetItemAt(x, y);
  HitTest := lvColumns.GetHitTestInfoAt(x, y);
  if Assigned(Item) and (htOnStateIcon in HitTest)
  then ItemChecked := Item
  else ItemChecked := nil;
end;

procedure TFormCsvExport.lvColumnsClick(Sender: TObject);
begin
  if Assigned(ItemChecked) then
  begin
    StartWait;
    try
      lvColumns.Selected := ItemChecked;
      MoveUpCheckedItems(lvColumns);
      ScrollToSelectedItem(lvColumns);
      ItemChecked := nil;
    finally
      StopWait;
    end;
  end;
end;

procedure TFormCsvExport.lvColumnsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 32 then
  begin
    StartWait;
    try
      MoveUpCheckedItems(lvColumns);
      ScrollToSelectedItem(lvColumns);
    finally
      StopWait;
    end;
  end;
end;

procedure TFormCsvExport.SendModeComboBoxChange(Sender: TObject);
begin
  case SendModeComboBox.ItemIndex of
    0: RecNum.Caption := IntToStr(fRowsTotal);
    1: RecNum.Caption := IntToStr(fRowsSelected);
  end;
end;

end.

