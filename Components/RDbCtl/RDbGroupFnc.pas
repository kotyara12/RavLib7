unit RDbGroupFnc;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, Db, RDbEditor;

type
  TFormGroupFnc = class(TDialogTemplate)
    lblFields: TLabel;
    cbFields: TComboBox;
    lblFunc: TLabel;
    cbFunc: TComboBox;
    procedure CheckButtons(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormGroupFnc.FormActivate(Sender: TObject);
begin
  inherited;

  CheckButtons(nil);
end;

procedure TFormGroupFnc.CheckButtons(Sender: TObject);
begin
  OkBtn.Enabled := (cbFields.ItemIndex > -1) and (cbFunc.ItemIndex > -1);
end;

end.
