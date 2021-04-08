unit RDbFilterItem_LinkComboDate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbFilterItem, StdCtrls, Buttons, ExtCtrls, ComCtrls, DB,
  DBCtrls;

type
  TFormDbFilterItem_LinkComboDate = class(TFormDbFilterItem)
    ModeLabel: TLabel;
    ModeComboBox: TComboBox;
    Edit1Label: TLabel;
    Edit2Label: TLabel;
    DateTimePicker1: TDateTimePicker;
    DateTimePicker2: TDateTimePicker;
    LookupLabel: TLabel;
    DBLookupComboBox: TDBLookupComboBox;
    ClearButton: TSpeedButton;
    DataSource: TDataSource;
    procedure DateTimePicker1Change(Sender: TObject);
    procedure DateTimePicker2Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
  protected
    procedure InitForm; override;
    procedure DoneForm; override;
  public
    CheckControls: procedure (Edit1, Edit2: TControl; const AMode: Integer) of object;
  end;

implementation

{$R *.dfm}

procedure TFormDbFilterItem_LinkComboDate.InitForm;
begin
  @CheckControls := nil;
end;

procedure TFormDbFilterItem_LinkComboDate.DoneForm;
begin
  @CheckControls := nil;
end;

procedure TFormDbFilterItem_LinkComboDate.ClearButtonClick(Sender: TObject);
begin
  DBLookupComboBox.KeyValue := Null;
end;

procedure TFormDbFilterItem_LinkComboDate.DateTimePicker1Change(Sender: TObject);
begin
  if DateTimePicker2.Enabled and (DateTimePicker1.DateTime > DateTimePicker2.DateTime) then
    DateTimePicker2.DateTime := DateTimePicker1.DateTime;
end;

procedure TFormDbFilterItem_LinkComboDate.DateTimePicker2Change(Sender: TObject);
begin
  if DateTimePicker1.Enabled and (DateTimePicker1.DateTime > DateTimePicker2.DateTime) then
    DateTimePicker1.DateTime := DateTimePicker2.DateTime;
end;

procedure TFormDbFilterItem_LinkComboDate.FormShow(Sender: TObject);
begin
  inherited;
  if Assigned(CheckControls) then CheckControls(DateTimePicker1, DateTimePicker2, ModeComboBox.ItemIndex);
end;

end.
