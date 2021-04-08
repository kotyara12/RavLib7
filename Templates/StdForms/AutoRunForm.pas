unit AutoRunForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, RUserRights;

type
  TFormAutoRun = class(TDialogTemplate)
    AutoStartCheckBox: TCheckBox;
    LoginEditLabel: TLabel;
    LoginEdit: TEdit;
    MinimizeCheckBox: TCheckBox;
    CheckBtn: TBitBtn;
    FullNameLabel: TLabel;
    FullName: TStaticText;
    procedure CheckButtons(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBtnClick(Sender: TObject);
    procedure LoginEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    UserInt: TUserRights;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  BaseDbUnit;

procedure TFormAutoRun.FormCreate(Sender: TObject);
begin
  inherited;
  UserInt.Registration := False;
end;

procedure TFormAutoRun.CheckButtons(Sender: TObject);
begin
  MinimizeCheckBox.Enabled := AutoStartCheckBox.Checked;
  LoginEdit.Enabled := AutoStartCheckBox.Checked;
  CheckBtn.Enabled := LoginEdit.Enabled and not UserInt.Registration and (LoginEdit.Text <> EmptyStr);
  OkBtn.Enabled := UserInt.Registration or not AutoStartCheckBox.Checked;
end;

procedure TFormAutoRun.CheckBtnClick(Sender: TObject);
begin
  UserInt := BaseData.LoadUserData(LoginEdit.Text);
  if UserInt.Registration then FullName.Caption := UserInt.FullName;
  CheckButtons(Sender);
end;

procedure TFormAutoRun.LoginEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if UserInt.Registration
  and not SameText(UserInt.UserName, LoginEdit.Text) then
  begin
    UserInt.Registration := False;
    FullName.Caption := EmptyStr;
  end;
  CheckButtons(Sender);
end;

end.
