unit RDbImportField;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls;

type
  TFormImportField = class(TDialogTemplate)
    FieldComboBox: TComboBox;
    FieldRadioButton: TRadioButton;
    ConstRadioButton: TRadioButton;
    FieldNameText: TStaticText;
    FieldNameTextLabel: TLabel;
    ConstComboBox: TComboBox;
    procedure FieldComboBoxCloseUp(Sender: TObject);
    procedure ConstComboBoxChange(Sender: TObject);
  private
  public
  end;

implementation

{$R *.dfm}

procedure TFormImportField.FieldComboBoxCloseUp(Sender: TObject);
begin
  if FieldComboBox.Text <> EmptyStr
  then FieldRadioButton.Checked := True;
end;

procedure TFormImportField.ConstComboBoxChange(Sender: TObject);
begin
  if ConstComboBox.Text <> EmptyStr
  then ConstRadioButton.Checked := True;
end;

end.
