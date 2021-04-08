unit RDbStatRes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplEditors, Menus, ActnList, ImgList, ComCtrls, ToolWin, RDbEditor,
  TmplStorage, TmplDialog, RavListView, StdCtrls, Buttons, ExtCtrls,
  TeEngine, Series, TeeProcs, Chart;

type
  TFormStatRes = class(TDialogTemplate)
    ListView: TRSortListView;
    PageControl: TPageControl;
    tsList: TTabSheet;
    tsChart: TTabSheet;
    ImageList: TImageList;
    Chart: TChart;
    Series1: TPieSeries;
    procedure OkBtnClick(Sender: TObject);
  private
    fEditor: TRDbCustomEditor;
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    procedure CreateFields(Editor: TRDbCustomEditor; const SelectedList: string);
    procedure CalcDbStatistic(Editor: TRDbCustomEditor; const SelectedList: string);
  end;

procedure ShowDbStatistic(Editor: TRDbCustomEditor; const SelectedList: string);

implementation

{$R *.dfm}

uses
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  Db, RDialogs, RVclUtils, RDbConst, RxStrUtils, RProgress, RMsgRu, RExpExcel;

resourcestring
  STitle        = 'Статистика "%s"';
  SCountField   = 'Кол-во';
  SPercentField = '%%';

procedure ShowDbStatistic(Editor: TRDbCustomEditor; const SelectedList: string);
begin
  with TFormStatRes.Create(Editor.Owner) do
  begin
    try
      fEditor := Editor;
      Caption := Format(STitle, [Editor.GetObjectDesc(etView)]);
      CreateFields(Editor, SelectedList);
      CalcDbStatistic(Editor, SelectedList);
      ShowModal;
    finally
      Free;
    end;
  end;
end;

{ TFormStatRes }

{$IFDEF STYLES}
procedure TFormStatRes.SetStyle;
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

procedure TFormStatRes.CreateFields(Editor: TRDbCustomEditor; const SelectedList: string);
var
  i: Integer;
  FldItem: TField;
  FldName: string;
begin
  StartWait;
  ListView.Columns.BeginUpdate;
  try
    ListView.Columns.Clear;
    if Assigned(Editor.DataSet) then
    begin
      for i := 1 to WordCount(SelectedList, FieldsDivChars) do
      begin
        FldName := Trim(ExtractWord(i, SelectedList, FieldsDivChars));
        FldItem := Editor.DataSet.FindField(FldName);
        if Assigned(FldItem) then
        begin
          with ListView.Columns.Add do
          begin
            Caption := FldItem.DisplayName;
            Width := FldItem.DisplayWidth * ListView.Font.Size;
            Tag := FldItem.Index;
            AutoSize := True;
          end;
        end;
      end;
    end;
    with ListView.Columns.Add do
    begin
      Caption := SCountField;
      Alignment := taRightJustify;
      Width := 60;
      Tag := -1;
    end;
    with ListView.Columns.Add do
    begin
      Caption := SPercentField;
      Alignment := taRightJustify;
      Width := 60;
      Tag := -1;
    end;
  finally
    ListView.Columns.EndUpdate;
    StopWait;
  end;
end;

procedure TFormStatRes.CalcDbStatistic(Editor: TRDbCustomEditor; const SelectedList: string);
var
  i, j, CurrKey: Integer;
  CurrBmk: TBookmark;
  FldItem: TField;
  LstItem: TListItem;
  RecExists: Boolean;
begin
  StartWait;
  ShowProgress(SMsgWorkingWait, Editor.DataSet.RecordCount);
  ListView.Items.BeginUpdate;
  Editor.DataSet.DisableControls;
  try
    ListView.Items.Clear;
    // Запоминаем текущую позицию
    CurrBmk := Editor.DataSet.GetBookmark;
    CurrKey := Editor.GetKeyValue;
    try
      Editor.DataSet.First;
      while not Editor.DataSet.Eof do
      begin
        // Поиск элемента в списке
        LstItem := nil;
        for j := 0 to ListView.Items.Count - 1 do
        begin
          RecExists := True;
          for i := 0 to ListView.Columns.Count - 3 do
          begin
            FldItem := Editor.DataSet.Fields[ListView.Columns[i].Tag];
            if i = 0
            then RecExists := SameText(FldItem.DisplayText, ListView.Items[j].Caption)
            else RecExists := SameText(FldItem.DisplayText, ListView.Items[j].SubItems[i - 1]);
            if not RecExists then Break;
          end;
          if RecExists then
          begin
            LstItem := ListView.Items[j];
            Break;
          end;
        end;
        if Assigned(LstItem) then
          // Запись найдена - увеличиваем счетчик
          LstItem.SubItems[LstItem.SubItems.Count - 1] := IntToStr(StrToIntDef(LstItem.SubItems[LstItem.SubItems.Count - 1], 1) + 1)
        else begin
          // Запись не найдена - создаем новую
          LstItem := ListView.Items.Add;
          for i := 0 to ListView.Columns.Count - 3 do
          begin
            FldItem := Editor.DataSet.Fields[ListView.Columns[i].Tag];
            if i = 0
            then LstItem.Caption := FldItem.DisplayText
            else LstItem.Subitems.Add(FldItem.DisplayText);
          end;
          LstItem.ImageIndex := -1;
          LstItem.Subitems.Add('1');
        end;
        UpdateProgressStep(1);
        Editor.DataSet.Next;
      end;
      // Пересчитываем проценты и рисуем диааграмму
      for i := 0 to ListView.Items.Count - 1 do
      begin
        j := StrToIntDef(ListView.Items[i].SubItems[ListView.Items[i].SubItems.Count - 1], 0);
        ListView.Items[i].SubItems.Add(Format('%.1f %%', [100 * j / Editor.DataSet.RecordCount]));

        // 2015-03-04: Добавлена диаграмма
        Chart.Series[0].Add(j, ListView.Items[i].Caption);
      end;
    finally
      // Переходим на прежнее место в таблице
      if Assigned(CurrBmk) then
      begin
        try
          try
            Editor.DataSet.GotoBookmark(CurrBmk);
          except
            Editor.LocateKey(CurrKey);
          end;
        finally
          Editor.DataSet.FreeBookmark(CurrBmk);
        end;
      end
      else Editor.LocateKey(CurrKey);
    end;
  finally
    Editor.DataSet.EnableControls;
    ListView.Items.EndUpdate;
    CloseProgress;
    StopWait;
  end;
end;

procedure TFormStatRes.OkBtnClick(Sender: TObject);
begin
  if ExportListViewToExcel(ListView, Caption,
    fEditor.GetObjectName(etView),
    EmptyStr, EmptyStr, 0, False) then
      Close;
end;

end.
