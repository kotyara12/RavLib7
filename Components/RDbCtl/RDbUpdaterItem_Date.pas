unit RDbUpdaterItem_Date;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbUpdaterItem, StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TFormDbUpdaterItem_Date = class(TFormDbUpdaterItem)
    DatePicker: TDateTimePicker;
    TimePicker: TDateTimePicker;
    procedure ChangeState(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormDbUpdaterItem_Date.FormActivate(Sender: TObject);
begin
  inherited;
  ChangeState(Sender);
end;

procedure TFormDbUpdaterItem_Date.ChangeState(Sender: TObject);
begin
  DatePicker.Enabled := SetRadioButton.Checked;
  TimePicker.Enabled := SetRadioButton.Checked;
end;

end.
