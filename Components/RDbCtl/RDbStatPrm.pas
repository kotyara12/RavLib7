unit RDbStatPrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls, RavListView, RDbEditor;

type
  TFormStatPrm = class(TDialogTemplate)
    ListView: TRSortListView;
    ListViewLabel: TLabel;
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  public
    procedure LoadFieldList(Editor: TRDbCustomEditor; const SelectedList: string);
    function  GetSelectedList: string;
  end;

function SelectStatFields(Editor: TRDbCustomEditor; var SelectedList: string): Boolean;

implementation

uses
  Db, RVclUtils, RxStrUtils, RDbConst;

{$R *.dfm}

function SelectStatFields(Editor: TRDbCustomEditor; var SelectedList: string): Boolean;
begin
  if WordCount(SelectedList, FieldsDivChars) > 0 then
  begin
    if WordCount(SelectedList, FieldsDivChars) > 1 then
    begin
      with TFormStatPrm.Create(Application) do
      begin
        try
          LoadFieldList(Editor, SelectedList);
          Result := ShowModal = mrOk;
          if Result then
            SelectedList := GetSelectedList;
        finally
          Free;
        end;
      end;
    end
    else Result := True;
  end
  else Result := False;
end;

{ TFormStatPrm }

procedure TFormStatPrm.LoadFieldList(Editor: TRDbCustomEditor; const SelectedList: string);
var
  i: Integer;
  FldItem: TField;
  FldName: string;
begin
  StartWait;
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
    if Assigned(Editor.DataSet) then
      for i := 1 to WordCount(Editor.StatisticFields, FieldsDivChars) do
      begin
        FldName := Trim(ExtractWord(i, Editor.StatisticFields, FieldsDivChars));
        FldItem := Editor.DataSet.FindField(FldName);
        if Assigned(FldItem) then
        begin
          with ListView.Items.Add do
          begin
            Caption := FldItem.DisplayName;
            Subitems.Add(FldItem.FieldName);
            Checked := IsWordPresent(FldItem.FieldName, SelectedList, FieldsDivChars);
          end;
        end;
      end;
  finally
    ListView.Items.EndUpdate;
    StopWait;
  end;
end;

function TFormStatPrm.GetSelectedList: string;
var
  i: Integer;
begin
  StartWait;
  ListView.Items.BeginUpdate;
  try
    Result := EmptyStr;
    for i := 0 to ListView.Items.Count - 1 do
      if ListView.Items[i].Checked then
      begin
        if Result = EmptyStr
        then Result := ListView.Items[i].SubItems[0]
        else Result := Result + ';' + ListView.Items[i].SubItems[0];
      end;
  finally
    ListView.Items.EndUpdate;
    StopWait;
  end;
end;


procedure TFormStatPrm.ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
var
  i: Integer;
begin
  OkBtn.Enabled := False;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Checked then
    begin
      OkBtn.Enabled := True;
      Break;
    end;
end;

end.
