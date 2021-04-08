unit RDbListView;

interface

uses
  Classes, ComCtrls, Db, RDbData, RDbConst;

type
  TFillDataLVOptions = (foAll, foSelected, foIndexed, foChecked);

{ «агрузка из списка набора данных }
procedure LoadDbListViewID(LV: TListView; DS: TDataSet; const Fields: string;
  const Image: Integer; const ClearItems: Boolean = True);
procedure LoadDbListViewRecData(LV: TListView; DS: TDataSet; const Fields: string;
  const Image: Integer; const ClearItems: Boolean = True);

implementation

uses
  Forms, SysUtils, RxStrUtils, RVclUtils;

const
  FieldsDelimiters = [';'];

{ «агрузка из списка набора данных }
procedure LoadDbListViewID(LV: TListView; DS: TDataSet; const Fields: string;
  const Image: Integer; const ClearItems: Boolean = True);
var
  i: Integer;
  ID: TID;
begin
  LV.Items.BeginUpdate;
  try
    if ClearItems then LV.Items.Clear;
    DS.DisableControls;
    try
      DS.First;
      while not DS.Eof do
      begin
        with LV.Items.Add do
        begin
          if Assigned(LV.SmallImages) or Assigned(LV.LargeImages) then ImageIndex := Image;
          if Assigned(DS.FindField(fnID)) then begin
            New(ID);
            ID^ := DS.FindField(fnID).AsInteger;
            Data := ID;
          end;
          Caption := DS.FieldByName(Trim(ExtractWord(1, Fields, FieldsDelimiters))).AsString;
          for i := 2 to WordCount(Fields, FieldsDelimiters) do
            Subitems.Add(DS.FieldByName(Trim(ExtractWord(i, Fields, FieldsDelimiters))).AsString);
        end;
        DS.Next;
        Application.ProcessMessages;
      end;
    finally
      DS.EnableControls;
    end;
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure LoadDbListViewRecData(LV: TListView; DS: TDataSet; const Fields: string;
  const Image: Integer; const ClearItems: Boolean = True);
var
  i: Integer;
begin
  LV.Items.BeginUpdate;
  try
    if ClearItems then LV.Items.Clear;
    DS.DisableControls;
    try
      DS.First;
      while not DS.Eof do
      begin
        with LV.Items.Add do
        begin
          if Assigned(LV.SmallImages) or Assigned(LV.LargeImages) then ImageIndex := Image;
          Data := GetRecordData(DS);
          Caption := DS.FieldByName(Trim(ExtractWord(1, Fields, FieldsDelimiters))).AsString;
          for i := 2 to WordCount(Fields, FieldsDelimiters) do
            Subitems.Add(DS.FieldByName(Trim(ExtractWord(i, Fields, FieldsDelimiters))).AsString);
        end;
        DS.Next;
        Application.ProcessMessages;
      end;
    finally
      DS.EnableControls;
    end;
  finally
    LV.Items.EndUpdate;
  end;
end;

{ == √енераци€ списка записей со ссылками на данные ============================ }
procedure FillDbRecordDataLV(LV: TListView; List: TStrings; const LVOptions: TFillDataLVOptions;
  const Index: Integer; const ROOptions: TGetRecordTextOptions);
var
  i: Integer;
begin
  if Assigned(List) then begin
    List.BeginUpdate;
    LV.Items.BeginUpdate;
    try
      List.Clear;
      for i := 0 to LV.Items.Count - 1 do
        if (LVOptions = foAll)
        or ((LVOptions = foSelected) and (LV.Items[i].Selected))
        or ((LVOptions = foChecked) and (LV.Items[i].Checked))
        or ((LVOptions = foIndexed) and (LV.Items[i].ImageIndex = Index))
        then begin
          if Assigned(LV.Items[i].Data) then begin
            List.AddObject(GetRecordText(TRecordData(LV.Items[i].Data), ROOptions),
              TObject(LV.Items[i].Data));
            LV.Items[i].Data := nil;
          end
          else List.Add(LV.Items[i].Caption);
        end;
    finally
      LV.Items.EndUpdate;
      List.EndUpdate;
    end;
  end;
end;

end.
