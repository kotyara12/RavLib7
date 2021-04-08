unit DbSelectForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, DBCtrls, AdoDB, DB;

type
  TFormDbSelect = class(TDialogTemplate)
    lblSelectId: TLabel;
    DataSource: TDataSource;
    deSelectName: TDBLookupComboBox;
    deSelectId: TEdit;
    procedure deSelectClick(Sender: TObject);
    procedure deSelectIdClick(Sender: TObject);
    procedure deSelectNameClick(Sender: TObject);
    procedure deSelectIdKeyPress(Sender: TObject; var Key: Char);
  private
  protected
    procedure StartForm; override;
  public
  end;

function DbSelectId(DS: TDataSet; const Title, FieldId, FieldName: string; var KeyId: Integer): Boolean; overload;
function DbSelectId(Db: TAdoConnection; const Title, SqlQuery: string; var KeyId: Integer): Boolean; overload;

implementation

uses
  RDbUtils, RExHandlers, RMsgRu, RDialogs;

{$R *.dfm}

function DbSelectId(DS: TDataSet; const Title, FieldId, FieldName: string; var KeyId: Integer): Boolean;
begin
  Result := False;

  with TFormDbSelect.Create(Application) do
  begin
    try
      Caption := Title;
      lblSelectId.Caption := Title + ':';
      // deSelectId.Hint := Title;
      deSelectName.Hint := Title;

      DataSource.DataSet := DS;

      deSelectName.KeyField := FieldId;
      deSelectName.ListField := FieldName;

      if KeyId > -1 then
        deSelectName.KeyValue := KeyId;

      if ShowModal = mrOk then
      begin
        KeyId := deSelectName.KeyValue;
        Result := True;
      end;
    finally
      Free;
    end;
  end;
end;

function DbSelectId(Db: TAdoConnection; const Title, SqlQuery: string; var KeyId: Integer): Boolean;
var
  qryData: TAdoQuery;
begin
  Result := False;

  try
    qryData := OpenDynamicQuery(Db, SqlQuery);
    try
      if DataSetIsOpen(qryData) then
        Result := DbSelectId(qryData, Title, qryData.Fields[0].FieldName, qryData.Fields[1].FieldName, KeyId);
    finally
      FreeDynamicQuery(qryData);
    end;
  except
    on E: Exception do
      HandleSqlExcept(E, nil, SqlQuery, SErrLoadData);
  end;
end;

procedure TFormDbSelect.StartForm;
begin
  deSelectClick(nil);
end;

procedure TFormDbSelect.deSelectClick(Sender: TObject);
begin
  OkBtn.Enabled := deSelectName.KeyValue > 0;
end;

procedure TFormDbSelect.deSelectIdClick(Sender: TObject);
begin
  if DataSource.DataSet.Locate(deSelectName.KeyField, deSelectId.Text, []) then
    deSelectName.KeyValue := DataSource.DataSet.FieldByName(deSelectName.KeyField).AsInteger;

  deSelectClick(nil);
end;

procedure TFormDbSelect.deSelectNameClick(Sender: TObject);
begin
  deSelectId.Text := IntToStr(deSelectName.KeyValue);

  deSelectClick(nil);
end;

procedure TFormDbSelect.deSelectIdKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#1..#31, '0'..'9']) then
    Key := #0;
end;

end.
