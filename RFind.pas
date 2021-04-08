unit RFind;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TFormFind = class(TDialogTemplate)
    FindGroupBox: TGroupBox;
    FindEditLabel: TLabel;
    FindEdit: TEdit;
    CaseCheckBox: TCheckBox;
    FullCheckBox: TCheckBox;
    ListViewLabel: TLabel;
    ListView: TListView;
    FindBtn: TBitBtn;
    procedure FormActivate(Sender: TObject);
    procedure FindBtnClick(Sender: TObject);
    procedure UpdateFind(Sender: TObject);
    procedure UpdateList(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListViewDblClick(Sender: TObject);
  private
    procedure GoTreeFind(TreeView: TTreeView);
    procedure GoListFind(ListFind: TListView);
  public
    Control: TWinControl;
  end;

{ == Поиск в "дереве" ========================================================== }
procedure FindInTree(TreeView: TTreeView);
{ == Поиск в ListView ========================================================== }
procedure FindInList(ListFind: TListView);

implementation

{$R *.dfm}

uses
  RVclUtils, RMsgRu, RDialogs, RStrUtils;

const
  ScrollWidth = 21;

{ == Поиск в TreeView ========================================================== }
procedure FindInTree(TreeView: TTreeView);
begin
  with TFormFind.Create(Application.MainForm) do
  begin
    try
      Control := TreeView;
      ListView.Columns.BeginUpdate;
      try
        ListView.Columns.Clear;
        ListView.ShowColumnHeaders := False;
        with ListView.Columns.Add do
          Width := ListView.Width - ScrollWidth;
      finally
        ListView.Columns.EndUpdate;
      end;
      ListView.SmallImages := TreeView.Images;
      if ShowModal = mrOk then
        TreeView.Selected := TTreeNode(ListView.Selected.Data);
    finally
      Free;
    end;
  end;
end;

{ == Поиск в ListView ========================================================== }
procedure FindInList(ListFind: TListView);
begin
  with TFormFind.Create(Application.MainForm) do
  begin
    try
      Control := ListFind;
      ListView.Columns.Assign(ListFind.Columns);
      ListView.SmallImages := ListFind.SmallImages;
      if ShowModal = mrOk then
        ListFind.Selected := TListItem(ListView.Selected.Data);
    finally
      Free;
    end;
  end;
end;

{ == TFormFind ================================================================= }

{ == Обработка свойств элементов управления ==================================== }
procedure TFormFind.FormActivate(Sender: TObject);
begin
  inherited;
  UpdateFind(Sender);
end;

procedure TFormFind.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateList(Sender);
end;

procedure TFormFind.UpdateFind(Sender: TObject);
begin
  FindBtn.Default := True;
  FindBtn.Enabled := FindEdit.Text <> EmptyStr;
  OkBtn.Default := False;
  OkBtn.Enabled := False;
  ButtonsPanel.TabOrder := 1;
end;

procedure TFormFind.UpdateList(Sender: TObject);
begin
  FindBtn.Default := False;
  FindBtn.Enabled := False;
  OkBtn.Default := True;
  OkBtn.Enabled := ListView.Selected <> nil;
  ButtonsPanel.TabOrder := 2;
end;

procedure TFormFind.ListViewDblClick(Sender: TObject);
begin
  if OkBtn.Enabled then OkBtn.Click;
end;

{ == Поиск ===================================================================== }
procedure TFormFind.FindBtnClick(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgFindData);
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
    DisableControls;
    try
      if Control is TTreeView then GoTreeFind(TTreeView(Control));
      if Control is TListView then GoListFind(TListView(Control));
    finally
      EnableControls;
    end;
    Application.ProcessMessages;
    if ListView.Items.Count > 0 then
    begin
      ListView.Selected := ListView.Items[0];
      ListView.SetFocus;
    end
    else begin
      StrNotFoundBox(FindEdit.Text);
      FindEdit.SetFocus;
    end;
  finally
    ListView.Items.EndUpdate;
    ShowInStatusBar(EmptyStr);
    FormActivate(Sender);
    StopWait;
  end;
end;

{ == Поиск в дереве ============================================================ }
procedure TFormFind.GoTreeFind(TreeView: TTreeView);
var
  i, iCount: Integer;
begin
  iCount := TreeView.Items.Count - 1;
  for i := 0 to iCount do
  begin
    if AnsiCompareFind(FindEdit.Text, TreeView.Items[i].Text, CaseCheckBox.Checked, not FullCheckBox.Checked) then
      with ListView.Items.Add do
      begin
        ImageIndex := TreeView.Items[i].ImageIndex;
        Caption := TreeView.Items[i].Text;
        Data := TreeView.Items[i];
      end;
  end;
end;

{ == Поиск в списке ============================================================ }
procedure TFormFind.GoListFind(ListFind: TListView);
var
  i, iCount, j, jCount: Integer;
  bFind: Boolean;
begin
  iCount := ListFind.Items.Count - 1;
  for i := 0 to iCount do
  begin
    bFind := AnsiCompareFind(FindEdit.Text, ListFind.Items[i].Caption, CaseCheckBox.Checked, not FullCheckBox.Checked);
    // 2012-04-16: NEW: поиск по любом столбцу TListView
    if not bFind then
    begin
      jCount := ListFind.Items[i].SubItems.Count - 1;
      for j := 0 to jCount do
      begin
        bFind := AnsiCompareFind(FindEdit.Text, ListFind.Items[i].SubItems[j], CaseCheckBox.Checked, not FullCheckBox.Checked);
        if bFind then Break;
      end;
    end;
    if bFind then
    begin
      with ListView.Items.Add do
      begin
        ImageIndex := ListFind.Items[i].ImageIndex;
        Caption := ListFind.Items[i].Caption;
        Data := ListFind.Items[i];
        jCount := ListFind.Items[i].SubItems.Count - 1;
        for j := 0 to jCount do
          Subitems.Add(ListFind.Items[i].SubItems[j]);
      end;
    end;
  end;
end;

end.
