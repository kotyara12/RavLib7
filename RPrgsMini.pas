unit RPrgsMini;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type
  TFormProgress = class(TForm)
    Panel: TPanel;
    Text: TLabel;
    Bar: TProgressBar;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowProgress(const AMessage: string; const AMax: Integer);
procedure ShowProgressDefault(const AMax: Integer);
procedure CloseProgress;
function  IsShowProgress: Boolean;

procedure UpdateProgressMessage(const AMessage: string);
procedure UpdateProgressPosition(const AValue: Integer);
procedure UpdateProgressStep(const AValue: Integer);
procedure UpdateProgressMax(const AValue: Integer);

implementation

{$R *.DFM}

resourcestring
  SWaitMessageDefault         = 'Пожалуйста подожите...';
  SErrorProgressAlreadyExists = 'Окно индикатора уже создано!';
  SErrorProgressNotExists     = 'Окно индикатора не существует!';

var
  Progress: TFormProgress;

procedure InternalUpdate;
begin
  if Progress <> nil then
  begin
    Progress.Update;
    Progress.BringToFront;
    Application.ProcessMessages;
  end;
end;

{ == Показать форму с индиктором прогресса ===================================== }
procedure ShowProgress(const AMessage: string; const AMax: Integer);
begin
  if Progress <> nil then
    raise Exception.Create(SErrorProgressAlreadyExists);
  Progress := TFormProgress.Create(Application);
  try
    Progress.Font := Screen.IconFont;
    Progress.Text.Caption := AMessage;
    Progress.Bar.Max := AMax;
    Progress.Bar.Position := 0;
    Progress.Show;
    InternalUpdate;
  except
    FreeAndNil(Progress);
    raise;
  end;
end;

procedure ShowProgressDefault(const AMax: Integer);
begin
  ShowProgress(SWaitMessageDefault, AMax);
end;

function IsShowProgress: Boolean;
begin
  Result := Progress <> nil;
end;

{ == Убрать окно прогресса ===================================================== }
procedure CloseProgress;
begin
  if Progress <> nil then
  begin
    Progress.Free;
    Progress := nil;
  end;
end;

{ == Обновление окна прогресса ================================================= }
procedure UpdateProgressMessage(const AMessage: string);
begin
  if Progress <> nil then
  begin
    Progress.Text.Caption := AMessage;
    InternalUpdate;
  end;
end;

procedure UpdateProgressPosition(const AValue: Integer);
begin
  if Progress <> nil then
  begin
    if AValue <= Progress.Bar.Max
    then Progress.Bar.Position := AValue
    else Progress.Bar.Position := Progress.Bar.Max;
    InternalUpdate;
  end;
end;

procedure UpdateProgressStep(const AValue: Integer);
begin
  if Progress <> nil then
  begin
    if (Progress.Bar.Position + AValue) <= Progress.Bar.Max
    then Progress.Bar.Position := Progress.Bar.Position + AValue
    else Progress.Bar.Position := Progress.Bar.Max;
    InternalUpdate;
  end;
end;

procedure UpdateProgressMax(const AValue: Integer);
begin
  if Progress <> nil then
  begin
    Progress.Bar.Max := AValue;
    if Progress.Bar.Position > Progress.Bar.Max
    then Progress.Bar.Position := Progress.Bar.Max;
    InternalUpdate;
  end;
end;

initialization
  Progress := nil;

finalization
  if Progress <> nil then Progress.Free;

end.
