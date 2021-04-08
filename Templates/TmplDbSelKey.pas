unit TmplDbSelKey;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, DB, Buttons, StdCtrls, DBCtrls, ExtCtrls;

type
  TDbSelKeyTemplate = class(TDialogTemplate)
    ItemsDBLookupComboBoxLabel: TLabel;
    ItemsBox: TDBLookupComboBox;
    InfoGroupBox: TGroupBox;
    RefButton: TSpeedButton;
    DataSource: TDataSource;
    procedure UpdateButtons(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure SetKeyValue(const Value: Integer);
    function  GetKeyValue: Integer;
  public
    property KeyValue: Integer read GetKeyValue write SetKeyValue;
  end;

implementation

{$R *.dfm}

function TDbSelKeyTemplate.GetKeyValue: Integer;
begin
  Result := ItemsBox.KeyValue;
end;

procedure TDbSelKeyTemplate.SetKeyValue(const Value: Integer);
begin
  ItemsBox.KeyValue := Value;
end;

procedure TDbSelKeyTemplate.UpdateButtons(Sender: TObject);
begin
  inherited;
  OkBtn.Enabled := DataSource.DataSet.Active and not DataSource.DataSet.IsEmpty
    and DataSource.DataSet.Locate(ItemsBox.KeyField, ItemsBox.KeyValue, []);
end;

procedure TDbSelKeyTemplate.FormActivate(Sender: TObject);
begin
  inherited;
  UpdateButtons(Sender);
end;

end.
