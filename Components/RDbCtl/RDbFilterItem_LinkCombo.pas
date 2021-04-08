unit RDbFilterItem_LinkCombo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbFilterItem, StdCtrls, Buttons, ExtCtrls, DB, DBCtrls;

type
  TFormDbFilterItem_LinkCombo = class(TFormDbFilterItem)
    LookupLabel: TLabel;
    DBLookupComboBox: TDBLookupComboBox;
    DataSource: TDataSource;
    ClearButton: TSpeedButton;
    procedure ClearButtonClick(Sender: TObject);
  private
  public
  end;

implementation

{$R *.dfm}

procedure TFormDbFilterItem_LinkCombo.ClearButtonClick(Sender: TObject);
begin
  DBLookupComboBox.KeyValue := Null;
end;

end.
