unit RDbUpdaterItem_Integer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbUpdaterItem, StdCtrls, Spin, Buttons, ExtCtrls;

type
  TFormDbUpdaterItem_Integer = class(TFormDbUpdaterItem)
    SpinEdit: TSpinEdit;
    procedure ChangeState(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFormDbUpdaterItem_Integer }

procedure TFormDbUpdaterItem_Integer.FormActivate(Sender: TObject);
begin
  inherited;
  ChangeState(Sender);
end;

procedure TFormDbUpdaterItem_Integer.ChangeState(Sender: TObject);
begin
  SpinEdit.Enabled := SetRadioButton.Checked;
end;

end.
