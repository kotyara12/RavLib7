unit RDbFilterItem_Float;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbFilterItem, StdCtrls, Buttons, ExtCtrls, Mask, RavFloatEdit;

type
  TFormDbFilterItem_Float = class(TFormDbFilterItem)
    ModeLabel: TLabel;
    ModeComboBox: TComboBox;
    Edit1Label: TLabel;
    Edit2Label: TLabel;
    RFloatEdit1: TRFloatEdit;
    RFloatEdit2: TRFloatEdit;
    procedure FormShow(Sender: TObject);
    procedure RFloatEdit1Change(Sender: TObject);
    procedure RFloatEdit2Change(Sender: TObject);
  protected
    procedure InitForm; override;
    procedure DoneForm; override;
  public
    CheckControls: procedure (Edit1, Edit2: TControl; const AMode: Integer) of object;
  end;

implementation

{$R *.dfm}

procedure TFormDbFilterItem_Float.InitForm;
begin
  @CheckControls := nil;
end;

procedure TFormDbFilterItem_Float.DoneForm;
begin
  @CheckControls := nil;
end;

procedure TFormDbFilterItem_Float.RFloatEdit1Change(Sender: TObject);
begin
  if RFloatEdit2.Enabled and (RFloatEdit1.Value > RFloatEdit2.Value) then
    RFloatEdit2.Value := RFloatEdit1.Value;
end;

procedure TFormDbFilterItem_Float.RFloatEdit2Change(Sender: TObject);
begin
  if RFloatEdit1.Enabled and (RFloatEdit1.Value > RFloatEdit2.Value) then
    RFloatEdit1.Value := RFloatEdit2.Value;
end;

procedure TFormDbFilterItem_Float.FormShow(Sender: TObject);
begin
  inherited;
  if Assigned(CheckControls) then CheckControls(RFloatEdit1, RFloatEdit2, ModeComboBox.ItemIndex);
end;

end.
