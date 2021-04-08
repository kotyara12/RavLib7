unit DbEditorIdForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, StdCtrls, DBCtrls, Mask, DB, Buttons, ExtCtrls;

type
  TFormDbEditorId = class(TDbDialogTemplate)
    lblId: TLabel;
    lblName: TLabel;
    lblNotes: TLabel;
    deId: TDBEdit;
    deName: TDBEdit;
    deNotes: TDBMemo;
  protected
    procedure InitControls(const EditMode: Boolean); override;
  end;


implementation

{$R *.dfm}

{ TFormDbEditorId }

procedure TFormDbEditorId.InitControls(const EditMode: Boolean);
begin
  inherited;

  deId.ReadOnly := not (DataSource.State = dsInsert);
  deId.ParentColor := deId.ReadOnly;
end;

end.
