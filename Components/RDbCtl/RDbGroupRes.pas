unit RDbGroupRes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplEditors, Menus, ActnList, ImgList, ComCtrls, ToolWin, RDbEditor,
  TmplStorage, TmplDialog, RavListView, StdCtrls, Buttons, ExtCtrls,
  TeEngine, Series, TeeProcs, Chart;

type
  TFormGroupRes = class(TDialogTemplate)
    ListView: TRSortListView;
    PageControl: TPageControl;
    tsList: TTabSheet;
    tsChart: TTabSheet;
    ImageList: TImageList;
    Chart: TChart;
    procedure OkBtnClick(Sender: TObject);
  private
    fEditor: TRDbCustomEditor;
    fGroups, fFunctions: TListView;
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    procedure CreateFields;
    procedure CalcDbStatistic;
  end;

const
  fsSum      = 0;
  fsMin      = 1;
  fsMax      = 2;
  fsAvg      = 3;
  fsCount    = 4;

  sFncFormat  : array [fsSum..fsCount] of string = ('Sum(%s)', 'Min(%s)', 'Max(%s)', 'Avg(%s)', 'Count(%s)');

procedure ShowDbGrouping(Editor: TRDbCustomEditor; const lvGroups, lvFunctions: TListView);

implementation

{$R *.dfm}

uses
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  Db, RDialogs, RVclUtils, RMsgRu, RExpExcel, RProgress;

resourcestring
  STitle        = 'Группировка "%s"';
  SCountField   = 'Кол-во';

procedure ShowDbGrouping(Editor: TRDbCustomEditor; const lvGroups, lvFunctions: TListView);
begin
  with TFormGroupRes.Create(Editor.Owner) do
  begin
    try
      fEditor := Editor;
      fGroups := lvGroups;
      fFunctions := lvFunctions;
      Caption := Format(STitle, [Editor.GetObjectDesc(etView)]);
      // CreateFields;
      CalcDbStatistic;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

{ TFormGroupRes }

{$IFDEF STYLES}
procedure TFormGroupRes.SetStyle;
begin
  inherited;
  ListView.Color := ApplicationStyle.DataForm.DataColor;
  FontDataToFont(ApplicationStyle.DataForm.DataFont, ListView.Font);
  Chart.Color := ApplicationStyle.DataForm.DataColor;
  FontDataToFont(ApplicationStyle.DataForm.DataFont, Chart.Legend.Font);
  FontDataToFont(ApplicationStyle.DataForm.DataFont, Chart.Title.Font);
  FontDataToFont(ApplicationStyle.DataForm.DataFont, Chart.Foot.Font);
end;
{$ENDIF}

procedure TFormGroupRes.CreateFields;
var
  i, iCount: Integer;
  fField: TField;
begin
  StartWait;
  ListView.Columns.BeginUpdate;
  try
    ListView.Columns.Clear;
    if Assigned(fEditor.DataSet) then
    begin
      iCount := fGroups.Items.Count - 1;
      for i := 0 to iCount do
      begin
        fField := fGroups.Items[i].Data;
        with ListView.Columns.Add do
        begin
          Caption := fGroups.Items[i].Caption;
          Alignment := taLeftJustify;
          Width := fField.DisplayWidth * ListView.Font.Size;
          AutoSize := True;
        end;
      end;

      iCount := fFunctions.Items.Count - 1;
      for i := 0 to iCount do
      begin
        with ListView.Columns.Add do
        begin
          Caption := fFunctions.Items[i].Caption;
          Alignment := taRightJustify;
          Width := 100;
          AutoSize := False;
        end;
      end;
    end;
  finally
    ListView.Columns.EndUpdate;
    StopWait;
  end;
end;

procedure TFormGroupRes.CalcDbStatistic;
type
  tFuncItem = record
    fField: TField;
    iFncType: Integer;
  end;

  tFuncData = record
    iCount: LongInt;
    fSumma: Variant;
    fMin: Variant;
    fMax: Variant;
  end;

  tItemData = record
    aHead: array of string;
    aData: array of tFuncData;
  end;

var
  i, j, iGrpCnt, iFncCnt, iDataIdx, CurrKey: Integer;
  CurrBmk: TBookmark;
  aGroups: array of TField;
  aFunctions: array of tFuncItem;
  aData: array of tItemData;
  bHeadCheck: Boolean;
  Serie: TBarSeries;

  function GetFormatValue(const fValue: Variant; fField: TField): string;
  var
    Format: TFloatFormat;
    FmtStr: string;
    Digits: Integer;
  begin
    if VarIsNull(fValue) or VarIsEmpty(fValue) then
      Result := EmptyStr
    else begin
      // Для числовых форматов считаем настройки из самих полей
      if fField is TFloatField then
      begin
        with TFloatField(fField) do
        begin
          FmtStr := DisplayFormat;
          if FmtStr = '' then
          begin
            if Currency then
            begin
              Format := ffCurrency;
              Digits := CurrencyDecimals;
            end
            else begin
              Format := ffGeneral;
              Digits := 0;
            end;
            Result := FloatToStrF(fValue, Format, Precision, Digits);
          end
          else Result := FormatFloat(FmtStr, fValue);
        end;
      end
      else begin
        if fField is TNumericField then
        begin
          with TNumericField(fField) do
          begin
            FmtStr := DisplayFormat;
            if FmtStr = '' then
              Result := FormatFloat(FmtStr, fValue)
            else
              Result := IntToStr(fValue);
          end;
        end
        else Result := VarToStr(fValue);
      end;
    end;
  end;

begin
  StartWait;
  ShowProgress(SMsgWorkingWait, fEditor.DataSet.RecordCount + 3);
  fEditor.DataSet.DisableControls;
  try
    SetLength(aGroups, 0);
    SetLength(aFunctions, 0);
    SetLength(aData, 0);
    try
      // Переносим поля из ListView в массивы для удобства работы
      iGrpCnt := fGroups.Items.Count;
      iFncCnt := fFunctions.Items.Count;

      SetLength(aGroups, iGrpCnt);
      for i := 0 to iGrpCnt - 1 do
        aGroups[i] := fGroups.Items[i].Data;

      SetLength(aFunctions, iFncCnt);
      for i := 0 to iFncCnt - 1 do
      begin
        aFunctions[i].fField := fFunctions.Items[i].Data;
        aFunctions[i].iFncType := fFunctions.Items[i].StateIndex;
      end;

      UpdateProgressStep(1);

      // Запоминаем текущую позицию
      CurrBmk := fEditor.DataSet.GetBookmark;
      CurrKey := fEditor.GetKeyValue;
      try
        fEditor.DataSet.First;
        while not fEditor.DataSet.Eof do
        begin
          // Поиск строки в массиве данных
          iDataIdx := -1;
          for j := Low(aData) to High(aData) do
          begin
            bHeadCheck := True;
            for i := 0 to iGrpCnt - 1 do
            begin
              if not SameText(aData[j].aHead[i], aGroups[i].DisplayText) then
              begin
                bHeadCheck := False;
                Break;
              end;
            end;
            if bHeadCheck then
            begin
              iDataIdx := j;
              Break;
            end;
          end;

          // Добавление данных в массив
          if iDataIdx = -1 then
          begin
            // Новая запись
            SetLength(aData, Length(aData) + 1);
            iDataIdx := High(aData);

            SetLength(aData[iDataIdx].aHead, iGrpCnt);
            for i := 0 to iGrpCnt - 1 do
              aData[iDataIdx].aHead[i] := aGroups[i].DisplayText;

            SetLength(aData[iDataIdx].aData, iFncCnt);
            for i := 0 to iFncCnt - 1 do
            begin
              aData[iDataIdx].aData[i].iCount := 1;
              if aFunctions[i].fField.IsNull then
              begin
                aData[iDataIdx].aData[i].fSumma := 0;
                aData[iDataIdx].aData[i].fMin := 0;
                aData[iDataIdx].aData[i].fMax := 0;
              end
              else begin
                aData[iDataIdx].aData[i].fSumma := aFunctions[i].fField.Value;
                aData[iDataIdx].aData[i].fMin := aFunctions[i].fField.Value;
                aData[iDataIdx].aData[i].fMax := aFunctions[i].fField.Value;
              end;
            end;
          end
          else begin
            // Добавляем к существующим счетчикам
            for i := 0 to iFncCnt - 1 do
            begin
              aData[iDataIdx].aData[i].iCount := aData[iDataIdx].aData[i].iCount + 1;
              if not aFunctions[i].fField.IsNull then
              begin
                aData[iDataIdx].aData[i].fSumma := aData[iDataIdx].aData[i].fSumma + aFunctions[i].fField.Value;
                if aFunctions[i].fField.Value < aData[iDataIdx].aData[i].fMin then
                  aData[iDataIdx].aData[i].fMin := aFunctions[i].fField.Value;
                if aFunctions[i].fField.Value > aData[iDataIdx].aData[i].fMax then
                  aData[iDataIdx].aData[i].fMax := aFunctions[i].fField.Value;
              end;
            end;
          end;

          UpdateProgressStep(1);
          fEditor.DataSet.Next;
        end;

        // Создаем заголовки данных
        ListView.Columns.BeginUpdate;
        try
          ListView.Columns.Clear;

          for i := 0 to iGrpCnt - 1 do
          begin
            with ListView.Columns.Add do
            begin
              Caption := aGroups[i].DisplayName;
              Alignment := taLeftJustify;
              Width := aGroups[i].DisplayWidth * ListView.Font.Size;
              AutoSize := True;
            end;
          end;

          for i := 0 to iFncCnt - 1 do
          begin
            with ListView.Columns.Add do
            begin
              Caption := Format(sFncFormat[aFunctions[i].iFncType],
                [aFunctions[i].fField.FieldName, aFunctions[i].fField.DisplayName]);
              Alignment := taRightJustify;
              Width := 100;
              AutoSize := False;
            end;
          end;

          UpdateProgressStep(1);
        finally
          ListView.Columns.EndUpdate;
        end;

        // Выводим полученный массив данных в список
        ListView.Items.BeginUpdate;
        try
          ListView.Items.Clear;
          Chart.SeriesList.Clear;

          for i := Low(aData) to High(aData) do
          begin
            Serie := TBarSeries.Create(Chart);
            Serie.Title := EmptyStr;
            for j := 0 to iGrpCnt - 1 do
            begin
              if Serie.Title = EmptyStr
              then Serie.Title := aData[i].aHead[j]
              else Serie.Title := Serie.Title + ' + ' + aData[i].aHead[j];
            end;
            if Serie.Title = EmptyStr then
              Serie.Title := ' ';
            Serie.Marks.Style := smsValue;
            Chart.AddSeries(Serie);

            with ListView.Items.Add do
            begin
              Caption := aData[i].aHead[0];
              for j := 1 to iGrpCnt - 1 do
                Subitems.Add(aData[i].aHead[j]);
              for j := 0 to iFncCnt - 1 do
              begin
                case aFunctions[j].iFncType of
                  fsSum:
                  begin
                    Subitems.Add(GetFormatValue(aData[i].aData[j].fSumma, aFunctions[j].fField));
                    Chart.Series[i].Add(aData[i].aData[j].fSumma,
                      Format(sFncFormat[aFunctions[j].iFncType], [aFunctions[j].fField.FieldName, aFunctions[j].fField.DisplayName]));
                  end;
                  fsMin:
                  begin
                    Subitems.Add(GetFormatValue(aData[i].aData[j].fMin, aFunctions[j].fField));
                    Chart.Series[i].Add(aData[i].aData[j].fMin);
                  end;
                  fsMax:
                  begin
                    Subitems.Add(GetFormatValue(aData[i].aData[j].fMax, aFunctions[j].fField));
                    Chart.Series[i].Add(aData[i].aData[j].fMax);
                  end;
                  fsAvg:
                  begin
                    Subitems.Add(FloatToStr(aData[i].aData[j].fSumma / aData[i].aData[j].iCount));
                    Chart.Series[i].Add(aData[i].aData[j].fSumma / aData[i].aData[j].iCount);
                  end;
                  fsCount:
                  begin
                    Subitems.Add(IntToStr(aData[i].aData[j].iCount));
                    Chart.Series[i].Add(aData[i].aData[j].iCount);
                  end;
                  else Subitems.Add('#ERROR#');
                end;
              end;
            end;
          end;

          UpdateProgressStep(1);
        finally
          ListView.Items.EndUpdate;
        end;
      finally
        // Переходим на прежнее место в таблице
        if Assigned(CurrBmk) then
        begin
          try
            try
              fEditor.DataSet.GotoBookmark(CurrBmk);
            except
              fEditor.LocateKey(CurrKey);
            end;
          finally
            fEditor.DataSet.FreeBookmark(CurrBmk);
          end;
        end
        else fEditor.LocateKey(CurrKey);
      end;
    finally
      SetLength(aGroups, 0);
      SetLength(aFunctions, 0);
      for i := Low(aData) to High(aData) do
      begin
        SetLength(aData[i].aHead, 0);
        SetLength(aData[i].aData, 0);
      end;
      SetLength(aData, 0);
    end;
  finally
    fEditor.DataSet.EnableControls;
    CloseProgress;
    StopWait;
  end;
end;

procedure TFormGroupRes.OkBtnClick(Sender: TObject);
begin
  if ExportListViewToExcel(ListView, Caption,
    fEditor.GetObjectName(etView),
    EmptyStr, EmptyStr, 0, False) then
      Close;
end;

end.
