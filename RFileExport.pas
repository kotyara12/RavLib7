unit RFileExport;

interface

uses
  Db, ComCtrls;

function ExportDataSetToCsv(DataSet: TDataSet; var FileName: string): Integer;
function ExportListViewToCsv(ListView: TListView; var FileName: string): Integer;

implementation

uses
  SysUtils, Forms, Dialogs, Windows,
  RVclUtils, RMsgRu, RExHandlers, RProgress, RDialogs;

resourcestring
  SCsvExt                    = '.csv';
  SCsvFilter                 = 'Файлы с разделителем [точка с запятой] (*.csv)|*.csv|' +
                               'Файлы с разделителем [TAB] (*.csv)|*.csv|' +
                               'Все файлы (*.*)|*.*';
  SMsgExportCount            = 'В файл "%s" выгружено %d записей';
  SErrExportDataToCsvFile    = 'Ошибка экспорта данных из "%s" в файл "%s"';
  SQuerySaveAllFields        = 'Выгрузить все столбцы таблицы, кроме BLOB'#13'("Да" - все столбцы, "Нет" - только имеющие статус "видимый")?';

function ExportDataSetToCsv(DataSet: TDataSet; var FileName: string): Integer;
var
  Dialog: TSaveDialog;
  LogFile: TextFile;
  LogStr: string;
  DivChar: Char;
  i: Integer;
  Bmk: TBookmark;
  AllFields: Boolean;
begin
  Result := -1;
  if Assigned(DataSet) and DataSet.Active then
  begin
    Dialog := TSaveDialog.Create(Application);
    try
      Dialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
      Dialog.DefaultExt := SCsvExt;
      Dialog.Filter := SCsvFilter;
      if Dialog.Execute then begin
        FileName := Dialog.FileName;
        AllFields := (QueryBoxStdNY(SQuerySaveAllFields) = ID_YES);
        try
          StartWait;
          ShowInStatusBar(SMsgExportDataWait);
          ShowProgress(SMsgExportDataWait, DataSet.RecordCount);
          try
            DataSet.DisableControls;
            Bmk := DataSet.GetBookmark;
            try
              AssignFile(LogFile, FileName);
              Rewrite(LogFile);
              try
                if Dialog.FilterIndex = 2 then DivChar := #9 else DivChar := ';';
                LogStr := EmptyStr;
                for i := 0 to DataSet.FieldCount - 1 do
                  if not DataSet.Fields[i].IsBlob
                    and (AllFields or DataSet.Fields[i].Visible) then
                      LogStr := LogStr + DataSet.Fields[i].DisplayName + DivChar;
                if Length(LogStr) > 0 then LogStr := Copy(LogStr, 1, Length(LogStr) - 1);
                WriteLn(LogFile, LogStr);
                DataSet.First;
                while not DataSet.Eof do
                begin
                  LogStr := EmptyStr;
                  for i := 0 to DataSet.FieldCount - 1 do
                    if not DataSet.Fields[i].IsBlob
                      and (AllFields or DataSet.Fields[i].Visible) then
                        LogStr := LogStr + DataSet.Fields[i].AsString + DivChar;
                  if Length(LogStr) > 0 then LogStr := Copy(LogStr, 1, Length(LogStr) - 1);
                  WriteLn(LogFile, LogStr);
                  DataSet.Next;
                  UpdateProgressStep(1);
                end;
              finally
                CloseFile(LogFile);
              end;
            finally
              try
                DataSet.GotoBookmark(Bmk);
              except
              end;
              DataSet.EnableControls;
              DataSet.FreeBookmark(Bmk);
            end;
          finally
            CloseProgress;
            ShowInStatusBar(EmptyStr);
            StopWait;
          end;
          Result := DataSet.RecordCount;
          InfoBox(Format(SMsgExportCount, [FileName, Result]));
        except
          on E: Exception do
            HandleExcept(E, DataSet, Format(SErrExportDataToCsvFile,
              [DataSet.Name, FileName]));
        end;
      end;
    finally
      Dialog.Free;
    end;
  end;
end;

function ExportListViewToCsv(ListView: TListView; var FileName: string): Integer;
var
  Dialog: TSaveDialog;
  LogFile: TextFile;
  LogStr: string;
  DivChar: Char;
  i, j: Integer;
begin
  Result := -1;
  if Assigned(ListView) then begin
    Dialog := TSaveDialog.Create(Application);
    try
      Dialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
      Dialog.DefaultExt := SCsvExt;
      Dialog.Filter := SCsvFilter;
      if Dialog.Execute then begin
        FileName := Dialog.FileName;
        try
          StartWait;
          ShowInStatusBar(SMsgExportDataWait);
          ShowProgress(SMsgExportDataWait, ListView.Items.Count);
          try
            ListView.Items.BeginUpdate;
            try
              AssignFile(LogFile, FileName);
              Rewrite(LogFile);
              try
                if Dialog.FilterIndex = 2 then DivChar := #9 else DivChar := ';';
                LogStr := EmptyStr;
                for i := 0 to ListView.Columns.Count - 1 do
                  LogStr := LogStr + ListView.Columns[i].Caption + DivChar;
                if Length(LogStr) > 0 then LogStr := Copy(LogStr, 1, Length(LogStr) - 1);
                WriteLn(LogFile, LogStr);
                for j := 0 to ListView.Items.Count - 1 do
                begin
                  LogStr := ListView.Items[j].Caption;
                  for i := 0 to ListView.Items[j].SubItems.Count - 1 do
                    LogStr := LogStr + DivChar + ListView.Items[j].SubItems[i];
                  WriteLn(LogFile, LogStr);
                  UpdateProgressStep(1);
                end;
              finally
                CloseFile(LogFile);
              end;
            finally
              ListView.Items.EndUpdate;
            end;
          finally
            CloseProgress;
            ShowInStatusBar(EmptyStr);
            StopWait;
          end;
          Result := ListView.Items.Count;
          InfoBox(Format(SMsgExportCount, [FileName, Result]));
        except
          on E: Exception do
            HandleExcept(E, ListView, Format(SErrExportDataToCsvFile,
              [ListView.Name, FileName]));
        end;
      end;
    finally
      Dialog.Free;
    end;
  end;
end;

end.
