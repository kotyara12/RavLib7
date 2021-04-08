unit DbEditorAudit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplAuditDialog, DB, StdCtrls, Mask, DBCtrls, ComCtrls, Buttons,
  ExtCtrls;

type
  TFormDbEditorAudit = class(TDbAuditDialogTemplate)
    lblId: TLabel;
    deId: TDBEdit;
    lblName: TLabel;
    deName: TDBEdit;
    lblNotes: TLabel;
    deNotes: TDBMemo;
    procedure PageControlChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure InitComponents(const EditMode: Boolean); override;
  end;

implementation

{$R *.dfm}

uses
  BaseDbUnit;

procedure TFormDbEditorAudit.InitComponents(const EditMode: Boolean);
begin
  inherited;

  PageControlChange(nil);
end;

procedure TFormDbEditorAudit.PageControlChange(Sender: TObject);
begin
  inherited;

  if PageControl.ActivePageIndex = 0 then
    deName.SetFocus;
end;

end.
