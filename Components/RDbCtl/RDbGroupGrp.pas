unit RDbGroupGrp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, Db, RDbEditor;

type
  TFormGroupGrp = class(TDialogTemplate)
    lblFields: TLabel;
    cbFields: TComboBox;
    procedure CheckButtons(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormGroupGrp.FormActivate(Sender: TObject);
begin
  inherited;

  CheckButtons(nil);
end;

procedure TFormGroupGrp.CheckButtons(Sender: TObject);
begin
  OkBtn.Enabled := cbFields.ItemIndex > -1;
end;

end.
