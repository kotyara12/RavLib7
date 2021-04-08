unit RDbFilterItem_Integer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbFilterItem, StdCtrls, Spin, Buttons, ExtCtrls;

type
  TFormDbFilterItem_Integer = class(TFormDbFilterItem)
    ModeLabel: TLabel;
    ModeComboBox: TComboBox;
    SpinEdit1Label: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2Label: TLabel;
    SpinEdit2: TSpinEdit;
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    procedure InitForm; override;
    procedure DoneForm; override;
  public
    CheckControls: procedure (Edit1, Edit2: TControl; const AMode: Integer) of object;
  end;

implementation

{$R *.dfm}

procedure TFormDbFilterItem_Integer.InitForm;
begin
  @CheckControls := nil;
end;

procedure TFormDbFilterItem_Integer.DoneForm;
begin
  @CheckControls := nil;
end;

procedure TFormDbFilterItem_Integer.SpinEdit1Change(Sender: TObject);
begin
  if SpinEdit2.Enabled and (SpinEdit1.Value > SpinEdit2.Value) then
    SpinEdit2.Value := SpinEdit1.Value;
end;

procedure TFormDbFilterItem_Integer.SpinEdit2Change(Sender: TObject);
begin
  if SpinEdit1.Enabled and (SpinEdit1.Value > SpinEdit2.Value) then
    SpinEdit1.Value := SpinEdit2.Value;
end;

procedure TFormDbFilterItem_Integer.FormShow(Sender: TObject);
begin
  inherited;
  if Assigned(CheckControls) then CheckControls(SpinEdit1, SpinEdit2, ModeComboBox.ItemIndex);
end;

end.
