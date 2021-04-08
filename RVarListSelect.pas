unit RVarListSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls, RavListView,
  ImgList;

type
  TFormVarList = class(TDialogTemplate)
    ListPanel: TPanel;
    VarListLabel: TLabel;
    Bevel: TBevel;
    VarListView: TRSortListView;
    ImageList: TImageList;
    procedure UpdateControls(Sender: TObject);
    procedure VarListViewDblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    fTagsChar: Char;
    function  GetVarName: string;
    procedure SetVarName(const Value: string);
  public
    procedure LoadVarList(List: TStrings);
    property  SelectedVarName: string read GetVarName write SetVarName;
  end;


function SelectVarName(VarList: TStrings; const TagsChar: Char; var VarName: string): Boolean;

implementation

uses
  RVclUtils, RVarListEx, RListView, RDialogs;

{$R *.dfm}

function SelectVarName(VarList: TStrings; const TagsChar: Char; var VarName: string): Boolean;
begin
  with TFormVarList.Create(Application.MainForm) do
  begin
    try
      fTagsChar := TagsChar;
      LoadVarList(VarList);
      SelectedVarName := VarName;
      Result := ShowModal = mrOk;
      if Result then VarName := SelectedVarName;
    finally
      Free;
    end;
  end;
end;

{ TFormScriptVars }

procedure TFormVarList.FormActivate(Sender: TObject);
begin
  inherited;
  VarListView.Columns[1].AutoSize := True;
  ScrollToSelectedItem(VarListView);
end;

function TFormVarList.GetVarName: string;
begin
  Result := VarListView.Selected.Caption;
end;

procedure TFormVarList.SetVarName(const Value: string);
var
  i: Integer;
begin
  StartWait;
  VarListView.Items.BeginUpdate;
  try
    VarListView.Selected := nil;
    for i := 0 to VarListView.Items.Count - 1 do
      if SameText(Value, VarListView.Items[i].Caption) then
      begin
        VarListView.Selected := VarListView.Items[i];
        Break;
      end;
  finally
    VarListView.Items.EndUpdate;
    StopWait;
    UpdateControls(nil);
  end;
end;

procedure TFormVarList.LoadVarList(List: TStrings);
var
  i: Integer;
begin
  StartWait;
  VarListView.Items.BeginUpdate;
  try
    VarListView.Items.Clear;

    with VarListView.Items.Add do
    begin
      ImageIndex := 0;
      Caption := fTagsChar + 'Date' + DateChar + 'YYYYMMDD-HHNNSS' + fTagsChar;
      Subitems.Add(FormatDateTime('YYYYMMDD-HHNNSS', Now));
    end;

    for i := 0 to List.Count - 1 do
      with VarListView.Items.Add do
      begin
        ImageIndex := 0;
        Caption := UpdateVarName(List.Names[i]);
        Subitems.Add(List.ValueFromIndex[i]);
      end;
  finally
    VarListView.Items.EndUpdate;
    StopWait;
    UpdateControls(nil);
  end;
end;

procedure TFormVarList.UpdateControls(Sender: TObject);
begin
  OkBtn.Enabled := VarListView.Selected <> nil;
end;

procedure TFormVarList.VarListViewDblClick(Sender: TObject);
begin
  if (VarListView.Selected <> nil) then OkBtn.Click;
end;

end.
