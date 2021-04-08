unit RDbOrderItem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, Db;

type
  TOrderItem = record
    Field: TField;
    Asc: Boolean;
  end;

  TFormDbOrderItem = class(TDialogTemplate)
    ComboBoxLabel: TLabel;
    ComboBox: TComboBox;
    AscRadioButtonLabel: TLabel;
    AscRadioButton: TRadioButton;
    DescRadioButton: TRadioButton;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function EditOrderItem(DS: TDataSet; var OrderItem: TOrderItem): Boolean;

implementation

{$R *.dfm}

uses
  RVclUtils, RDbCustom;

function EditOrderItem(DS: TDataSet; var OrderItem: TOrderItem): Boolean;
begin
  with TFormDbOrderItem.Create(Application) do
  begin
    try
      if Assigned(OrderItem.Field)
      then LoadKeyFieldsToCmbBox(ComboBox, DS, OrderItem.Field.FieldName, False, False, True)
      else LoadKeyFieldsToCmbBox(ComboBox, DS, EmptyStr, False, False, True);
      if OrderItem.Asc
      then AscRadioButton.Checked := True
      else DescRadioButton.Checked := True;
      Result := ShowModal = mrOk;
      if Result then begin
        OrderItem.Field := TField(ComboBox.Items.Objects[ComboBox.ItemIndex]);
        OrderItem.Asc := AscRadioButton.Checked;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TFormDbOrderItem.FormShow(Sender: TObject);
begin
  OkBtn.Enabled := (ComboBox.Items.Count > 0) and (ComboBox.ItemIndex > -1);
end;

end.
