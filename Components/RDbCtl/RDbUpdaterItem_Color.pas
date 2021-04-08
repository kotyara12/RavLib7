unit RDbUpdaterItem_Color;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbUpdaterItem, StdCtrls, Buttons, ExtCtrls, RavClrCombo;

type
  TFormDbUpdaterItem_Color = class(TFormDbUpdaterItem)
    RColorCombo: TRColorCombo;
    procedure ChangeState(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormDbUpdaterItem_Color.FormActivate(Sender: TObject);
begin
  inherited;
  ChangeState(Sender);
end;

procedure TFormDbUpdaterItem_Color.ChangeState(Sender: TObject);
begin
  RColorCombo.Enabled := SetRadioButton.Checked;
end;

end.
