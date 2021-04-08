unit RDbUpdaterItem_Int64;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbUpdaterItem, StdCtrls, Spin, Buttons, ExtCtrls, RavSpin64;

type
  TFormDbUpdaterItem_Int64 = class(TFormDbUpdaterItem)
    SpinEdit: TSpinEdit64;
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

procedure TFormDbUpdaterItem_Int64.FormActivate(Sender: TObject);
begin
  inherited;
  ChangeState(Sender);
end;

procedure TFormDbUpdaterItem_Int64.ChangeState(Sender: TObject);
begin
  SpinEdit.Enabled := SetRadioButton.Checked;
end;

end.
