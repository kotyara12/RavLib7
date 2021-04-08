unit RDbUpdaterItem_Combo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbUpdater, RDbUpdaterItem, Buttons, DBCtrls, StdCtrls, ExtCtrls, DB;

type
  TFormDbUpdaterItem_Combo = class(TFormDbUpdaterItem)
    DBLookupComboBox: TDBLookupComboBox;
    DataSource: TDataSource;
    BtnSelectKey: TSpeedButton;
    procedure ChangeState(Sender: TObject);
    procedure BtnSelectKeyClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
  public
    SelectKeyProc: TOnSelectKeyNotifyEvent;
  end;

implementation

{$R *.dfm}

{ TFormDbUpdaterItem_Combo }

procedure TFormDbUpdaterItem_Combo.FormActivate(Sender: TObject);
begin
  inherited;
  ChangeState(Sender);
end;

procedure TFormDbUpdaterItem_Combo.ChangeState(Sender: TObject);
begin
  DBLookupComboBox.Enabled := SetRadioButton.Checked;
  BtnSelectKey.Enabled := SetRadioButton.Checked;
  OkBtn.Enabled := ClearRadioButton.Checked or (Trim(DBLookupComboBox.Text) <> EmptyStr);
end;

procedure TFormDbUpdaterItem_Combo.BtnSelectKeyClick(Sender: TObject);
var
  bSelOk: Boolean;
  iSelKey: Integer;
begin
  if Assigned(SelectKeyProc) then
  begin
    iSelKey := DBLookupComboBox.KeyValue;
    SelectKeyProc(Self, iSelKey, bSelOk);
    if bSelOk then
      DBLookupComboBox.KeyValue := iSelKey;
  end;
end;

end.
