unit RDbEx;

interface

uses
  Db, AdoDb;

{ == Открыть набор данных с обработкой ошибок ================================== }
procedure OpenDataSet(DataSet: TDataSet; const OperID: Integer = 0);
procedure OpenAdoDataSet(Connect: TAdoConnection; DataSet: TCustomAdoDataSet;
  const OperID: Integer = 0);
{ == Обновить набор данных с обработкой ошибок ================================= }
procedure RefreshDataSet(DataSet: TDataSet; const StorePosition: Boolean = True;
  const OperID: Integer = 0);
procedure RefreshAdoDataSet(Connect: TAdoConnection; DataSet: TCustomAdoDataSet;
  const StorePosition: Boolean = True; const OperID: Integer = 0);
{ == Открыть или обновить набор данных ========================================= }
procedure ReopenDataSet(DataSet: TDataSet; const StorePosition: Boolean = True;
  const OperID: Integer = 0);
procedure ReopenDataSetWait(DataSet: TDataSet; const StorePosition: Boolean = True;
  const OperID: Integer = 0);
procedure ReopenDataSetMsg(DataSet: TDataSet; const StorePosition: Boolean = True;
  const OperID: Integer = 0);
procedure ReopenAdoDataSet(Connect: TAdoConnection; DataSet: TCustomAdoDataSet;
  const StorePosition: Boolean = True; const OperID: Integer = 0);
procedure ReopenAdoDataSetWait(Connect: TAdoConnection; DataSet: TCustomAdoDataSet;
  const StorePosition: Boolean = True; const OperID: Integer = 0);
procedure ReopenAdoDataSetMsg(Connect: TAdoConnection; DataSet: TCustomAdoDataSet;
  const StorePosition: Boolean = True; const OperID: Integer = 0);

implementation

uses
  SysUtils, RDbConst, RDbUtils, RVclUtils, RMsgRu, RExHandlers;

{ == Открыть набор данных с обработкой ошибок ================================== }
procedure OpenDataSet(DataSet: TDataSet; const OperID: Integer = 0);
begin
  DataSet.DisableControls;
  try
    try
      if OperID > 0 then DataSet.Tag := OperID;
      DataSet.Open;
    except
      on E: Exception do
        HandleExcept(E, DataSet, Format(SErrOpenDataSet, [DataSet.Name]));
    end;
  finally
    DataSet.EnableControls;
  end;
end;

procedure OpenAdoDataSet(Connect: TAdoConnection; DataSet: TCustomAdoDataSet;
  const OperID: Integer = 0);
begin
  DataSet.DisableControls;
  try
    try
      DataSet.Connection := Connect;
      if OperID > 0 then DataSet.Tag := OperID;
      DataSet.Open;
    except
      on E: Exception do
        HandleExcept(E, DataSet, Format(SErrOpenDataSet, [DataSet.Name]));
    end;
  finally
    DataSet.EnableControls;
  end;
end;

{ == Обновить набор данных ===================================================== }
procedure RefreshDataSet(DataSet: TDataSet; const StorePosition: Boolean = True;
  const OperID: Integer = 0);
var
  LastId: Integer;
  LastBmk: TBookmark;
begin
  DataSet.DisableControls;
  try
    LastId := -1;
    LastBmk := nil;
    try
      if StorePosition then begin
        if Assigned(DataSet.FindField(fnID))
        then LastId := DataSet.FieldByName(fnID).AsInteger
        else LastBmk := DataSet.GetBookmark;
      end;
      if OperID > 0 then DataSet.Tag := OperID;
      if DataSet is TAdoQuery then
        TAdoQuery(DataSet).Requery
      else begin
        DataSet.Close;
        DataSet.Open;
      end;
      if DataSet.Active then begin
        try
          if LastId > -1 then
            DataSet.Locate(fnID, LastId, [])
          else
            if LastBmk <> nil then begin
              try
                DataSet.GotoBookmark(LastBmk);
              finally
                DataSet.FreeBookmark(LastBmk);
              end;
            end;
        except
        end;
      end;
    except
      on E: Exception do
        HandleExcept(E, DataSet, Format(SErrRefreshDataSet, [DataSet.Name]));
    end;
  finally
    DataSet.EnableControls;
  end;
end;

procedure RefreshAdoDataSet(Connect: TAdoConnection; DataSet: TCustomAdoDataSet;
  const StorePosition: Boolean = True; const OperID: Integer = 0);
var
  LastId: Integer;
  LastBmk: TBookmark;
begin
  DataSet.DisableControls;
  try
    LastId := -1;
    LastBmk := nil;
    try
      if StorePosition then begin
        if Assigned(DataSet.FindField(fnID))
        then LastId := DataSet.FieldByName(fnID).AsInteger
        else LastBmk := DataSet.GetBookmark;
      end;
      DataSet.Connection := Connect;
      if OperID > 0 then DataSet.Tag := OperID;
      if DataSet is TAdoQuery then
        TAdoQuery(DataSet).Requery
      else begin
        DataSet.Close;
        DataSet.Open;
      end;
      if DataSet.Active then begin
        try
          if LastId > -1 then
            DataSet.Locate(fnID, LastId, [])
          else
            if LastBmk <> nil then begin
              try
                DataSet.GotoBookmark(LastBmk);
              finally
                DataSet.FreeBookmark(LastBmk);
              end;
            end;
        except
        end;
      end;
    except
      on E: Exception do
        HandleExcept(E, DataSet, Format(SErrRefreshDataSet, [DataSet.Name]));
    end;
  finally
    DataSet.EnableControls;
  end;
end;

{ == Открыть или обновить набор данных ========================================= }
procedure ReopenDataSet(DataSet: TDataSet; const StorePosition: Boolean = True;
  const OperID: Integer = 0);
begin
  if DataSet.Active
  then RefreshDataSet(DataSet, StorePosition, OperID)
  else OpenDataSet(DataSet, OperID);
end;

procedure ReopenDataSetWait(DataSet: TDataSet; const StorePosition: Boolean = True;
  const OperID: Integer = 0);
begin
  StartWait;
  try
    ReopenDataSet(DataSet, StorePosition, OperID);
  finally
    StopWait;
  end;
end;

procedure ReopenDataSetMsg(DataSet: TDataSet; const StorePosition: Boolean = True;
  const OperID: Integer = 0);
begin
  StartWait;
  ShowInStatusBar(SMsgLoadDataWait);
  try
    ReopenDataSet(DataSet, StorePosition, OperID);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure ReopenAdoDataSet(Connect: TAdoConnection; DataSet: TCustomAdoDataSet;
  const StorePosition: Boolean = True; const OperID: Integer = 0);
begin
  if DataSet.Active
  then RefreshAdoDataSet(Connect, DataSet, StorePosition, OperID)
  else OpenAdoDataSet(Connect, DataSet, OperID);
end;

procedure ReopenAdoDataSetWait(Connect: TAdoConnection; DataSet: TCustomAdoDataSet;
  const StorePosition: Boolean = True; const OperID: Integer = 0);
begin
  StartWait;
  try
    ReopenAdoDataSet(Connect, DataSet, StorePosition, OperID);
  finally
    StopWait;
  end;
end;

procedure ReopenAdoDataSetMsg(Connect: TAdoConnection; DataSet: TCustomAdoDataSet;
  const StorePosition: Boolean = True; const OperID: Integer = 0);
begin
  StartWait;
  ShowInStatusBar(SMsgLoadDataWait);
  try
    ReopenAdoDataSet(Connect, DataSet, StorePosition, OperID);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

end.
