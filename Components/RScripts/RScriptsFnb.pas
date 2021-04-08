unit RScriptsFnb;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, RScripts, RScrCnst;

type
  TFormScriptFnb = class(TDialogTemplate)
    OperGroupBox: TGroupBox;
    NotCheckBox: TCheckBox;
    NoneRadioButton: TRadioButton;
    AndRadioButton: TRadioButton;
    OrRadioButton: TRadioButton;
  private
    function GetInversed: Boolean;
    function GetOperator: TRFncOperator;
    procedure SetFirstNode(const Value: Boolean);
    procedure SetInversed(const Value: Boolean);
    procedure SetOperator(const Value: TRFncOperator);
  public
    property FncFirstNode: Boolean write SetFirstNode;
    property FncOperator: TRFncOperator read GetOperator write SetOperator;
    property FncInversed: Boolean read GetInversed write SetInversed;
  end;

function EditFnbCommand(const FirstNode: Boolean;
  var Inversed: Boolean; var Operator: TRFncOperator): Boolean;

implementation

{$R *.dfm}

function EditFnbCommand(const FirstNode: Boolean;
  var Inversed: Boolean; var Operator: TRFncOperator): Boolean;
begin
  with TFormScriptFnb.Create(Application.MainForm) do
  begin
    try
      FncFirstNode := FirstNode;
      FncOperator := Operator;
      FncInversed := Inversed;
      Result := ShowModal = mrOk;
      if Result then
      begin
        Operator := FncOperator;
        Inversed := FncInversed;
      end;
    finally
      Free;
    end;
  end;
end;

{ TFormScriptFnb }

procedure TFormScriptFnb.SetFirstNode(const Value: Boolean);
begin
  if Value then
  begin
    NoneRadioButton.Checked := True;
    AndRadioButton.Enabled := False;
    OrRadioButton.Enabled := False;
  end
  else begin
    NoneRadioButton.Enabled := False;
    if NoneRadioButton.Checked then AndRadioButton.Checked := True;
  end;
end;

function TFormScriptFnb.GetInversed: Boolean;
begin
  Result := NotCheckBox.Checked;
end;

procedure TFormScriptFnb.SetInversed(const Value: Boolean);
begin
  NotCheckBox.Checked := Value;
end;

function TFormScriptFnb.GetOperator: TRFncOperator;
begin
  Result := coNone;
  if AndRadioButton.Checked then Result := coAnd;
  if OrRadioButton.Checked then Result := coOr;
end;

procedure TFormScriptFnb.SetOperator(const Value: TRFncOperator);
begin
  case Value of
    coAnd: AndRadioButton.Checked := True;
    coOr: OrRadioButton.Checked := True;
    else NoneRadioButton.Checked := True;
  end;
end;

end.
