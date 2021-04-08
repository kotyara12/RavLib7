unit RDbImport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplMaster, StdCtrls, Buttons, ExtCtrls, ComCtrls, RavListView,
  ImgList, Menus, Db, DbTables, RDbEditor, Grids, DBGrids, ActnList;

type
  TFormDbImport = class(TMasterTemplate)
    Notebook: TNotebook;
    lblFileName: TLabel;
    edFileName: TStaticText;
    OpenBtn: TBitBtn;
    SelectFieldsLabel: TLabel;
    lvFields: TRSortListView;
    ImageList: TImageList;
    ShowProgressLabel: TLabel;
    ProgressBar: TProgressBar;
    Log: TMemo;
    FieldsPopupMenu: TPopupMenu;
    LogPopupMenu: TPopupMenu;
    OpenDialog: TOpenDialog;
    ActionList: TActionList;
    SetKeyField: TAction;
    itemSetKeyField: TMenuItem;
    SetImportProp: TAction;
    itemSetImportProp: TMenuItem;
    divFields: TMenuItem;
    SaveLog: TAction;
    itemSaveLog: TMenuItem;
    SaveDialog: TSaveDialog;
    lblSource: TLabel;
    cbFirstLine: TCheckBox;
    lvSource: TRSortListView;
    procedure NotebookPageChanged(Sender: TObject);
    procedure PrevBtnClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure SetKeyFieldUpdate(Sender: TObject);
    procedure SetKeyFieldExecute(Sender: TObject);
    procedure SetImportPropUpdate(Sender: TObject);
    procedure SetImportPropExecute(Sender: TObject);
    procedure lvFieldsDblClick(Sender: TObject);
    procedure SaveLogUpdate(Sender: TObject);
    procedure SaveLogExecute(Sender: TObject);
    procedure FirstLineClick(Sender: TObject);
  private
    fEditor: TRDbEditor;
    fOwnerId: Integer;
    procedure OpenExcelDocument(const FileName: string);
    function  FindSourceColumn(const ColumnName: string): Integer;
    procedure LoadDataSetFields(const OwnerId: Integer = 0);
    procedure FillRecordData(Sender: TObject; const Data: Pointer; var Complete: Boolean);
    procedure ImportOneRecord(LI: TListItem; var AddCnt, IgnCnt: Integer; var State: Boolean);
  protected
    procedure UpdateButtons(const BlockButtons: Boolean = False); override;
    procedure ImportData;
  end;

procedure ImportExternalData(Editor: TRDbEditor; const OwnerId: Integer = 0);

implementation

{$R *.dfm}

uses
  StrUtils, RVclUtils, RMsgRu, RExHandlers, RDbData, RStrings, RDialogs, RMsExcel, RProgress,
  RDbImportField;

const
  intFieldName   = 0;
  intTypeData    = 1;
  intImportField = 2;
  intConstant    = 3;

  fmtColIndex    = 'Column %d';
  fmtColValue    = '[%s]="%s"';

resourcestring
  SMsgLoadExtData         = 'Загрузка данных из файла "%s"...';
  SMsgImportRun           = 'Импорт данных из файла "%s"...';
  SMsgImportComplete      = 'Импорт данных завершен!';

  SQryBreakImport         = 'Прервать импорт данных из внешнего источника данных?';

  SErrOpenExtData         = 'Ошибка загрузки данных из "%s"!';
  SErrImportRecord        = 'Ошибка импорта записи из внешнего источника данных!';
  SErrLookupValueNotFound = 'Для поля "%s" не найдено значение в lookup-таблице!';

  SLogImportStart         = 'Запущен импорт данных из файла: %s';
  SLogRecordAdded         = '[+] добавлена запись: %s';
  SLogRecordIgnored       = '[-] запись игнорирована: %s';
  SLogRecordError         = '[!] ошибка "%s" при импорте записи: %s';
  SLogImportEnd           = 'Импорт из внешнего файла завершен';
  SLogImportCount         = 'Добавлено записей: %d, игнорировано записей: %d';


procedure ImportExternalData(Editor: TRDbEditor; const OwnerId: Integer = 0);
begin
  with TFormDbImport.Create(Editor.Owner) do
  begin
    try
      StartWait;
      ShowInStatusBar(SMsgPrepareOperation);
      try
        fEditor := Editor;
        fOwnerId := OwnerId;
      finally
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

{ == TFormDbImport ============================================================= }

procedure TFormDbImport.UpdateButtons(const BlockButtons: Boolean = False);
begin
  PrevBtn.Enabled := not BlockButtons and (Notebook.PageIndex = 1);
  NextBtn.Enabled := not BlockButtons and (lvSource.Items.Count > 0);
  CancelBtn.Enabled := not BlockButtons and (Notebook.PageIndex in [0, 1]);
  if Notebook.PageIndex < 2 then SetNext else SetComplete;
end;

procedure TFormDbImport.PrevBtnClick(Sender: TObject);
begin
  if Notebook.PageIndex > 0 then
    Notebook.PageIndex := Notebook.PageIndex - 1;
  UpdateButtons;
end;

procedure TFormDbImport.NextBtnClick(Sender: TObject);
begin
  if Notebook.PageIndex < (Notebook.Pages.Count - 1) then
  begin
    Notebook.PageIndex := Notebook.PageIndex + 1;

    if Notebook.PageIndex < Notebook.Pages.Count - 1
    then UpdateButtons
    else begin
      UpdateButtons(True);
      try
        ImportData;
      finally
        UpdateButtons(False);
      end;
    end;
  end
  else begin
    Close;
    ModalResult := mrOk;
  end;
end;

procedure TFormDbImport.FormActivate(Sender: TObject);
begin
  inherited;

  HeaderLabel.Caption := Notebook.ActivePage;
end;

procedure TFormDbImport.NotebookPageChanged(Sender: TObject);
begin
  HeaderLabel.Caption := Notebook.ActivePage;
end;

procedure TFormDbImport.OpenExcelDocument(const FileName: string);
var
  iCol, iRow, iRowFirst, iRowLast: Integer;
  Sheet: Variant;
begin
  try
    lvSource.Columns.Clear;
    lvSource.Items.Clear;

    if ConnectToMsExcel then
    begin
      try
        Workbook := OpenWorkbook(FileName);
        if not VarIsEmpty(Workbook) then
        begin
          Sheet := Workbook.Sheets[1];
          if not VarIsEmpty(Sheet) then
          begin
            try
              for iCol := 1 to Sheet.UsedRange.Columns.Count do
              begin
                with lvSource.Columns.Add do
                begin
                  if cbFirstLine.Checked
                  then Caption := Sheet.Cells[1, iCol].Value
                  else Caption := Format(fmtColIndex, [iCol]);
                  Width := Sheet.UsedRange.Columns[iCol].Width;
                end;
              end;

              if cbFirstLine.Checked
              then iRowFirst := 2
              else iRowFirst := 1;
              iRowLast := Sheet.UsedRange.Rows.Count;

              ShowProgress(Format(SMsgLoadExtData, [ExtractFileName(FileName)]), iRowLast - iRowFirst + 1);
              try
                for iRow := iRowFirst to iRowLast do
                begin
                  with lvSource.Items.Add do
                  begin
                    Caption := VarToStr(Sheet.Cells[iRow, 1].Value);
                    for iCol := 2 to Sheet.UsedRange.Columns.Count do
                      SubItems.Add(VarToStr(Sheet.Cells[iRow, iCol].Value));
                  end;
                  UpdateProgressStep(1);
                end;
              finally
                CloseProgress;
              end;
            finally
              VarClear(Sheet);
            end;
          end;
        end;
      finally
        CloseMsExcelAndDiconnect;
      end;
    end;
  except
    on E: Exception do
      HandleExcept(E, Self, Format(SErrOpenExtData, [FileName]));
  end;
end;

function TFormDbImport.FindSourceColumn(const ColumnName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to lvSource.Columns.Count - 1 do
    if SameText(lvSource.Columns[i].Caption, ColumnName) then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TFormDbImport.LoadDataSetFields(const OwnerId: Integer = 0);
var
  i: Integer;
  Fld: TField;
begin
  lvFields.Items.BeginUpdate;
  try
    lvFields.Items.Clear;

    if Assigned(fEditor) and Assigned(fEditor.DataSet) then
    begin
      with fEditor do
      begin
        for i := 0 to DataSet.FieldCount - 1 do
        begin
          Fld := DataSet.Fields[i];
          if Fld.FieldKind in [fkData, fkLookup] then
          begin
            with lvFields.Items.Add do
            begin
              Data := Fld;
              Checked := Fld.Required and (Fld <> GetKeyField);
              if Fld = GetKeyField then ImageIndex := 0 else ImageIndex := -1;
              Caption := Fld.DisplayName;
              Subitems.Add(Fld.FieldName);
              Subitems.Add(FieldTypeNames[Fld.DataType]);
              if FindSourceColumn(Fld.FieldName) >-1
              then Subitems.Add(Fld.FieldName)
              else Subitems.Add(EmptyStr);
              if (Fld = GetOwnerField)
              then Subitems.Add(IntToStr(OwnerId))
              else if Fld = GetKeyField
                   then Subitems.Add(EmptyStr)
                   else Subitems.Add(SNullText);
            end;
          end;
        end;
      end;
    end;
  finally
    lvFields.Items.EndUpdate;
  end;
end;

procedure TFormDbImport.OpenBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    StartWait;
    ShowInStatusBar(Format(SMsgLoadExtData, [OpenDialog.FileName]));
    try
      edFileName.Caption := OpenDialog.FileName;
      OpenExcelDocument(OpenDialog.FileName);
      LoadDataSetFields(fOwnerId);
    finally
      UpdateButtons;
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  end;
end;

procedure TFormDbImport.FirstLineClick(Sender: TObject);
begin
  if (edFileName.Caption <> '') and FileExists(edFileName.Caption) then
  begin
    StartWait;
    ShowInStatusBar(Format(SMsgLoadExtData, [edFileName.Caption]));
    try
      OpenExcelDocument(edFileName.Caption);
      LoadDataSetFields(fOwnerId);
    finally
      UpdateButtons;
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  end;
end;

procedure TFormDbImport.SetKeyFieldUpdate(Sender: TObject);
begin
  SetKeyField.Enabled := IsNotWait and (lvFields.Selected <> nil);
  SetKeyField.Checked := (lvFields.Selected <> nil) and (lvFields.Selected.ImageIndex > -1);
end;

procedure TFormDbImport.SetKeyFieldExecute(Sender: TObject);
var
  i: Integer;
begin
  if lvFields.Selected.ImageIndex > -1
  then lvFields.Selected.ImageIndex := -1
  else begin
    lvFields.Items.BeginUpdate;
    try
      for i := 0 to lvFields.Items.Count - 1 do
        lvFields.Items[i].ImageIndex := -1;
      lvFields.Selected.ImageIndex := 0;
    finally
      lvFields.Items.EndUpdate;
    end;
  end;
end;

procedure TFormDbImport.SetImportPropUpdate(Sender: TObject);
begin
  SetImportProp.Enabled := IsNotWait and (lvFields.Selected <> nil);
end;

procedure TFormDbImport.SetImportPropExecute(Sender: TObject);
var
  LI: TListItem;
  i: Integer;
begin
  LI := lvFields.Selected;

  with TFormImportField.Create(Self) do
  begin
    try
      StartWait;
      try
        FieldNameText.Caption := LI.Caption;
        FieldRadioButton.Enabled := lvSource.Items.Count > 0;
        FieldRadioButton.Checked := LI.SubItems[intImportField] <> EmptyStr;
        FieldComboBox.Enabled := lvSource.Items.Count > 0;
        for i := 0 to lvSource.Columns.Count - 1 do
        begin
          FieldComboBox.Items.Add(lvSource.Columns[i].Caption);
          if lvSource.Columns[i].Caption = LI.SubItems[intImportField] then
            FieldComboBox.ItemIndex := i;
        end;
        ConstRadioButton.Checked := not FieldRadioButton.Checked;
        ConstComboBox.Text := LI.SubItems[intConstant];
        ConstComboBox.Items.Add(SNullText);
      finally
        StopWait;
      end;
      if ShowModal = mrOk then
      begin
        StartWait;
        try
          if FieldRadioButton.Checked and (FieldComboBox.Text <> EmptyStr) then
          begin
            LI.SubItems[intImportField] := FieldComboBox.Text;
            LI.SubItems[intConstant] := EmptyStr;
          end
          else begin
            LI.SubItems[intImportField] := EmptyStr;
            LI.SubItems[intConstant] := ConstComboBox.Text;
          end;
        finally
          StopWait;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TFormDbImport.lvFieldsDblClick(Sender: TObject);
begin
  if SetImportProp.Enabled then SetImportPropExecute(Sender);
end;

procedure TFormDbImport.SaveLogUpdate(Sender: TObject);
begin
  SaveLog.Enabled := IsNotWait;
end;

procedure TFormDbImport.SaveLogExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    StartWait;
    ShowInStatusBar(SMsgSaveDataWait);
    try
      SaveStrings(SaveDialog.FileName, Log.Lines);
    finally
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  end;
end;

procedure TFormDbImport.FillRecordData(Sender: TObject; const Data: Pointer; var Complete: Boolean);
var
  Fld: TField;
  LI: TListItem;
  i, iColId: Integer;
  sValue: string;

  procedure SetValue(Fld: TField; const sValue: string);
  begin
    case Fld.DataType of
      ftUnknown, ftString, ftFixedChar, ftWideString,
      ftMemo, ftFmtMemo:
      begin
        Fld.AsString := sValue;
      end;
      ftBoolean:
      begin
        Fld.AsBoolean := (sValue = '1') or (sValue = '-1')
                      or (AnsiUpperCase(sValue) = 'TRUE')
                      or (AnsiUpperCase(sValue) = 'T');
      end;
      ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint:
      begin
        Fld.AsInteger := StrToIntDef(AnsiReplaceStr(Trim(sValue), ' ', ''), 0);
      end;
      ftBCD, ftFloat, ftCurrency:
      begin
        Fld.AsFloat := RStrToFloatDef(AnsiReplaceStr(Trim(sValue), ' ', ''), 0.0);
      end;
      ftDate, ftTime, ftDateTime:
      begin
        Fld.AsString := Trim(sValue);
      end;
      else begin
        (* Не переносятся поля типов
        ftBytes, ftVarBytes, ftBlob, ftGraphic,
        ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
        ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
        ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd
        *)
      end;
    end;
  end;

begin
  LI := Data;

  for i := 0 to lvFields.Items.Count - 1 do
  begin
    if lvFields.Items[i].Checked then
    begin
      // Определяем поле
      Fld := lvFields.Items[i].Data;

      // Считываем значение из входного списка или константу
      sValue := lvFields.Items[i].Subitems[intConstant];
      if lvFields.Items[i].Subitems[intImportField] <> EmptyStr then
      begin
        iColId := FindSourceColumn(lvFields.Items[i].Subitems[intImportField]);
        if iColId > -1 then
        begin
          if iColId = 0
          then sValue := LI.Caption
          else sValue := LI.Subitems[iColId - 1];
        end;                                              
      end;

      // Устанавливаем значения в поля
      if (sValue <> EmptyStr) and (sValue <> SNullText) then
      begin
        if Fld.FieldKind = fkLookup then
        begin
          if Fld.LookupDataSet.Locate(Fld.LookupResultField, sValue, []) then
            Fld.DataSet.FieldByName(Fld.KeyFields).AsVariant :=
              Fld.LookupDataSet.FieldByName(Fld.LookupKeyFields).AsVariant
          else
            raise Exception.CreateFmt(SErrLookupValueNotFound, [Fld.FieldName]);
        end
        else SetValue(Fld, sValue);
      end
      else begin
        if Fld.Lookup
        then Fld.DataSet.FieldByName(Fld.KeyFields).Clear
        else Fld.Clear;
      end;
    end;
  end;
end;

procedure TFormDbImport.ImportOneRecord(LI: TListItem; var AddCnt, IgnCnt: Integer; var State: Boolean);
var
  i, KeyColId: Integer;
  KeyField, KeyColumn, KeyValue: string;
  RecValue: string;
begin
  State := True;
  try
    for i := 0 to lvSource.Columns.Count - 1 do
    begin
      if i = 0
      then RecValue := Format(fmtColValue, [lvSource.Columns[i].Caption, LI.Caption])
      else RecValue := RecValue + '; ' + Format(fmtColValue, [lvSource.Columns[i].Caption, LI.Subitems[i - 1]]);
    end;

    KeyField := EmptyStr;
    for i := 0 to lvFields.Items.Count - 1 do
    begin
      if lvFields.Items[i].ImageIndex > -1 then
      begin
        KeyField := lvFields.Items[i].SubItems[intFieldName];
        KeyColumn := lvFields.Items[i].SubItems[intImportField];
        KeyValue := lvFields.Items[i].SubItems[intConstant];
        if KeyColumn <> EmptyStr then
        begin
          KeyColId := FindSourceColumn(KeyColumn);
          if KeyColId > -1 then
          begin
            if KeyColId = 0
            then KeyValue := LI.Caption
            else KeyValue := LI.SubItems[KeyColId - 1];
          end;
        end;
      end;
    end;

    if (KeyField <> EmptyStr) and (KeyValue <> EmptyStr)
    and fEditor.DataSet.Locate(KeyField, KeyValue, [loCaseInsensitive]) then
    begin
      Log.Lines.Add(Format(SLogRecordIgnored, [RecValue]));
      Inc(IgnCnt);
    end
    else begin
      State := fEditor.ImportRecord(fOwnerId, FillRecordData, LI, Format(SMsgImportRun, [edFileName.Caption]));
      Log.Lines.Add(Format(SLogRecordAdded, [RecValue]));
      Inc(AddCnt);
    end;
  except
    on E: Exception do
    begin
      State := False;
      Log.Lines.Add(Format(SLogRecordError, [E.Message, RecValue]));
      HandleExcept(E, Self, SErrImportRecord);
    end;
  end;
end;

procedure TFormDbImport.ImportData;
var
  i, iCount: Integer;
  iCntAdd, iCntIgn: Integer;
  State: Boolean;
begin
  StartWait;
  ShowInStatusBar(Format(SMsgImportRun, [edFileName.Caption]));

  fEditor.DataSet.DisableControls;
  try
    iCount := lvSource.Items.Count;

    Log.Lines.Clear;
    Log.Lines.Add(Format(SLogImportStart, [edFileName.Caption]));

    fEditor.SaveLogMsg(etImport, Format(SLogImportStart, [edFileName.Caption, iCount]));

    try
      ProgressBar.Max := iCount;
      ProgressBar.Position := 0;

      iCntAdd := 0;
      iCntIgn := 0;

      for i := 0 to iCount - 1 do
      begin
        ImportOneRecord(lvSource.Items[i], iCntAdd, iCntIgn, State);
        if not State and (QueryBoxStdNY(SQryBreakImport) = IDYES) then Break;

        ProgressBar.Position := i + 1;
        Application.ProcessMessages;
      end;
    finally
      Log.Lines.Add(Format(SLogImportCount, [iCntAdd, iCntIgn]));
      Log.Lines.Add(SLogImportEnd);

      fEditor.SaveLogMsg(etImport, Format(SLogImportCount, [iCntAdd, iCntIgn]));
      fEditor.SaveLogMsg(etImport, SLogImportEnd);
    end;
  finally
    fEditor.DataSet.EnableControls;

    ShowInStatusBar(SMsgImportComplete);
    StopWait;
  end;
end;

end.
