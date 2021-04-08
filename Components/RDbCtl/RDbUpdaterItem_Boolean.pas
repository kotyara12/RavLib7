unit RDbUpdaterItem_Boolean;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbUpdaterItem, StdCtrls, Buttons, ExtCtrls;

type
  TFormDbUpdaterItem_Boolean = class(TFormDbUpdaterItem)
    ValueComboBox: TComboBox;
    procedure ChangeState(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFormDbUpdaterItem_Boolean }

procedure TFormDbUpdaterItem_Boolean.FormActivate(Sender: TObject);
begin
  inherited;
  ChangeState(Sender);
end;

procedure TFormDbUpdaterItem_Boolean.ChangeState(Sender: TObject);
begin
  ValueComboBox.Enabled := SetRadioButton.Checked;
  OkBtn.Enabled := ClearRadioButton.Checked or (ValueComboBox.ItemIndex > -1);
end;

end.
