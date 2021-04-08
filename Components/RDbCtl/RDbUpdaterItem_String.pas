unit RDbUpdaterItem_String;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbUpdaterItem, StdCtrls, Buttons, ExtCtrls, DB;

type
  TFormDbUpdaterItem_String = class(TFormDbUpdaterItem)
    ComboBox: TComboBox;
    ReplaceRadioButton: TRadioButton;
    FindEdit: TEdit;
    ReplaceEdit: TEdit;
    procedure ChangeState(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormDbUpdaterItem_String.FormActivate(Sender: TObject);
begin
  inherited;
  ChangeState(Sender);
end;

procedure TFormDbUpdaterItem_String.ChangeState(Sender: TObject);
begin
  ComboBox.Enabled := SetRadioButton.Checked;
  FindEdit.Enabled := ReplaceRadioButton.Checked;
  ReplaceEdit.Enabled := ReplaceRadioButton.Checked;
  OkBtn.Enabled := ClearRadioButton.Checked
    or (SetRadioButton.Checked and (Trim(ComboBox.Text) <> EmptyStr))
    or (ReplaceRadioButton.Checked and (Trim(FindEdit.Text) <> EmptyStr));
end;

end.
