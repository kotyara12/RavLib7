unit RDbUpdaterItem_Float;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbUpdaterItem, StdCtrls, Buttons, ExtCtrls, Spin, RavFloatEdit,
  Mask, ToolEdit, CurrEdit;

type
  TFormDbUpdaterItem_Float = class(TFormDbUpdaterItem)
    FloatEdit: TRxCalcEdit;
    procedure ChangeState(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFormDbUpdaterItem_Float }

procedure TFormDbUpdaterItem_Float.FormActivate(Sender: TObject);
begin
  inherited;
  ChangeState(Sender);
end;

procedure TFormDbUpdaterItem_Float.ChangeState(Sender: TObject);
begin
  FloatEdit.Enabled := SetRadioButton.Checked;
end;

end.
