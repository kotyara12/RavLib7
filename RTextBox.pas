unit RTextBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls;

type
  TFormTextBox = class(TDialogTemplate)
    Memo: TMemo;
    MemoLabel: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function RInputText(const ATitle: string; const AMaxLength: Integer; var AText: string): Boolean;
function RInputStrings(const ATitle: string; const AMaxLength: Integer; var ALines: TStrings): Boolean;

implementation

{$R *.dfm}

function RInputText(const ATitle: string; const AMaxLength: Integer; var AText: string): Boolean;
begin
  with TFormTextBox.Create(Application.MainForm) do
  begin
    try
      if ATitle <> EmptyStr then
        Caption := ATitle;
      Memo.MaxLength := AMaxLength;
      Memo.Text := AText;
      Result := ShowModal = mrOk;
      if Result then AText := Memo.Text;
    finally
      Free;
    end;
  end;
end;

function RInputStrings(const ATitle: string; const AMaxLength: Integer; var ALines: TStrings): Boolean;
begin
  with TFormTextBox.Create(Application.MainForm) do
  begin
    try
      if ATitle <> EmptyStr then
        Caption := ATitle;
      Memo.MaxLength := AMaxLength;
      Memo.Lines.Assign(ALines);
      Result := ShowModal = mrOk;
      if Result then ALines.Assign(Memo.Lines);
    finally
      Free;
    end;
  end;
end;

end.
