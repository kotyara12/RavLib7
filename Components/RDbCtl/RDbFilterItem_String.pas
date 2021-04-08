unit RDbFilterItem_String;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbFilterItem, StdCtrls, Buttons, ExtCtrls;

type
  TFormDbFilterItem_String = class(TFormDbFilterItem)
    ModeLabel: TLabel;
    ModeComboBox: TComboBox;
    EditLabel: TLabel;
    Edit: TComboBox;
    CaseCheckBox: TCheckBox;
    procedure FormShow(Sender: TObject);
  protected
    procedure InitForm; override;
    procedure DoneForm; override;
  public
    CheckControls: procedure (Edit1, Edit2: TControl; const AMode: Integer) of object;
  end;

implementation

{$R *.dfm}

uses
  RDialogs;

procedure TFormDbFilterItem_String.InitForm;
begin
  @CheckControls := nil;
end;

procedure TFormDbFilterItem_String.DoneForm;
begin
  @CheckControls := nil;
end;

procedure TFormDbFilterItem_String.FormShow(Sender: TObject);
begin
  inherited;
  if Assigned(CheckControls) then CheckControls(Edit, CaseCheckBox, ModeComboBox.ItemIndex);
end;

end.
