unit RDbFastSeek;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, Grids, DBGrids,
  RDbColorGrid, DB, ComCtrls, RavListView;

type
  TFormFastSeek = class(TDialogTemplate)
    FindEditLabel: TLabel;
    FindEdit: TEdit;
    DbGridLabel: TLabel;
    FindBtn: TBitBtn;
    ListView: TRSortListView;
    RegCheckBox: TCheckBox;
    procedure UpdateControls(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FindBtnClick(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListViewDblClick(Sender: TObject);
  private
    FDS: TDataSet;
    FKeyField: TField;
    FFindField: TField;
    FListFields: array of TField;
  public
    procedure PrepareForm(DS: TDataSet; const KeyField, FindField, ListFields: string);
  end;

function FastSeek(DS: TDataSet; const KeyField, FindField, ListFields: string): Integer;

implementation

{$R *.dfm}
uses
  RVclUtils, RMsgRu, RxStrUtils, RDbConst, RListView;

function FastSeek(DS: TDataSet; const KeyField, FindField, ListFields: string): Integer;
begin
  Result := intDisable;
  with TFormFastSeek.Create(Application.MainForm) do
  begin
    try
      PrepareForm(DS, KeyField, FindField, ListFields);
      if ShowModal = mrOk then
        Result := GetItemId(ListView.Selected);
    finally
      Free;
    end;
  end;
end;

procedure TFormFastSeek.PrepareForm(DS: TDataSet; const KeyField, FindField, ListFields: string);
var
  i: Integer;
  ListField: TField;
begin
  StartWait;
  ShowInStatusBar(SMsgPrepareOperation);
  try
    FDS := DS;
    FKeyField := FDS.FindField(KeyField);
    FFindField := FDS.FindField(FindField);
    SetLength(FListFields, 0);
    ListView.Columns.BeginUpdate;
    try
      ListView.Columns.Clear;
      for i := 1 to WordCount(ListFields, chDivChars) do
      begin
        ListField := FDS.FindField(ExtractWord(i, ListFields, chDivChars));
        if Assigned(ListField) then
        begin
          SetLength(FListFields, Length(FListFields) + 1);
          FListFields[High(FListFields)] := ListField;
          with ListView.Columns.Add do
          begin
            Caption := ListField.DisplayName;
            Width := ListField.DisplayWidth * intFieldToColumnWidth;
          end;
        end;
      end;
    finally
      ListView.Columns.EndUpdate;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TFormFastSeek.UpdateControls(Sender: TObject);
begin
  FindBtn.Enabled := Assigned(FDS) and FDS.Active and
    (FindEdit.Text <> EmptyStr) and Assigned(FFindField);
  OkBtn.Enabled := Assigned(ListView.Selected)
    and Assigned(FKeyField);
end;

procedure TFormFastSeek.FormActivate(Sender: TObject);
begin
  inherited;
  UpdateControls(Sender);
end;

procedure TFormFastSeek.FindBtnClick(Sender: TObject);
var
  FindValue: string;
  i: Integer;
  ID: TId;
  Bmk: TBookmark;
begin
  StartWait;
  ShowInStatusBar(SMsgFindData);
  try
    FDS.DisableControls;
    ListView.Items.BeginUpdate;
    Bmk := FDS.GetBookmark;
    try
      ListView.Items.Clear;
      FindValue := FindEdit.Text;
      FDS.First;
      while not FDS.EOF do
      begin
        if (RegCheckBox.Checked and (Pos(FindEdit.Text, FFindField.AsString) > 0))
        or (not RegCheckBox.Checked and (Pos(AnsiUpperCase(FindEdit.Text),
            AnsiUpperCase(FFindField.AsString)) > 0)) then
          with ListView.Items.Add do
          begin
            New(ID);
            if Assigned(FKeyField) then ID^ := FKeyField.AsInteger else ID^ := intDisable;
            Data := ID;
            if Length(FListFields) > 0 then
              Caption := FListFields[Low(FListFields)].AsString;
            for i := Low(FListFields) + 1 to High(FListFields) do
              Subitems.Add(FListFields[i].AsString);
          end;
        FDS.Next;
      end;
      if ListView.Items.Count > 0
      then ListView.SetFocus
      else FindEdit.SetFocus;
    finally
      try
        try
          FDS.GotoBookmark(Bmk);
        except
        end;
        FDS.FreeBookmark(Bmk);
      finally
        ListView.Items.EndUpdate;
        FDS.EnableControls;
      end;
    end;
  finally
    UpdateControls(Sender);
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TFormFastSeek.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateControls(Sender);
end;

procedure TFormFastSeek.ListViewDblClick(Sender: TObject);
begin
  if Assigned(ListView.Selected) then OkBtn.Click;
end;

end.
