unit RProgress2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Gauges, StdCtrls, RMsgTypes;

type
  TFormProgress2 = class(TForm)
    Panel: TPanel;
    Text: TLabel;
    Gauge1: TGauge;
    Gauge2: TGauge;
    BtnStop: TButton;
    procedure BtnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetStopButton(const Value: Boolean);
    function  GetStopButton: Boolean;
    function  GetStopFlag: Boolean;
    procedure SetStopFlag(const Value: Boolean);
  public
    property StopButton: Boolean read GetStopButton write SetStopButton;
    property StopFlag: Boolean read GetStopFlag write SetStopFlag;
  end;

procedure ShowProgress(const AMessage: string; const AMax1, AMax2: Integer;
  const ShowStopButton: Boolean = False);
procedure ShowProgressDefault(const AMax1, AMax2: Integer);
procedure CloseProgress;
function  IsShowProgress: Boolean;
function  IsStopProgress: Boolean;

procedure UpdateProgressMessage(const AMessage: string);
procedure UpdateProgressPosition1(const AValue: Integer);
procedure UpdateProgressPosition2(const AValue: Integer);
procedure UpdateProgressStep1(const AValue: Integer);
procedure UpdateProgressStep2(const AValue: Integer);
procedure UpdateProgressMax1(const AValue: Integer);
procedure UpdateProgressMax2(const AValue: Integer);

implementation

{$R *.DFM}

resourcestring
  SWaitMessageDefault         = 'Пожалуйста подожите...';
  SErrorProgressAlreadyExists = 'Окно индикатора уже создано!';
  SErrorProgressNotExists     = 'Окно индикатора не существует!';

var
  Progress: TFormProgress2;

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
procedure ShowProgress(const AMessage: string; const AMax1, AMax2: Integer;
  const ShowStopButton: Boolean = False);
begin
  if Progress <> nil then
    raise Exception.Create(SErrorProgressAlreadyExists);
  Progress := TFormProgress2.Create(Application);
  try
    Progress.StopButton := ShowStopButton;
    Progress.StopFlag := False;
    Progress.Text.Caption := AMessage;
    Progress.Gauge1.MaxValue := AMax1;
    Progress.Gauge1.Progress := 0;
    Progress.Gauge2.MaxValue := AMax2;
    Progress.Gauge2.Progress := 0;
    Progress.Show;
    InternalUpdate;
  except
    FreeAndNil(Progress);
    raise;
  end;
end;

procedure ShowProgressDefault(const AMax1, AMax2: Integer);
begin
  ShowProgress(SWaitMessageDefault, AMax1, AMax2);
end;

function IsShowProgress: Boolean;
begin
  Result := Progress <> nil;
end;

function IsStopProgress: Boolean;
begin
  Result := (Progress <> nil) and Progress.StopFlag;
  InternalUpdate;
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

procedure UpdateProgressPosition1(const AValue: Integer);
begin
  if Progress <> nil then
  begin
    if AValue <= Progress.Gauge1.MaxValue
    then Progress.Gauge1.Progress := AValue
    else Progress.Gauge1.Progress := Progress.Gauge1.MaxValue;
    InternalUpdate;
  end;
end;

procedure UpdateProgressPosition2(const AValue: Integer);
begin
  if Progress <> nil then
  begin
    if AValue <= Progress.Gauge2.MaxValue
    then Progress.Gauge2.Progress := AValue
    else Progress.Gauge2.Progress := Progress.Gauge2.MaxValue;
    InternalUpdate;
  end;
end;

procedure UpdateProgressStep1(const AValue: Integer);
begin
  if Progress <> nil then
  begin
    if (Progress.Gauge1.Progress + AValue) <= Progress.Gauge1.MaxValue
    then Progress.Gauge1.Progress := Progress.Gauge1.Progress + AValue
    else Progress.Gauge1.Progress := Progress.Gauge1.MaxValue;
    InternalUpdate;
  end;
end;

procedure UpdateProgressStep2(const AValue: Integer);
begin
  if Progress <> nil then
  begin
    if (Progress.Gauge2.Progress + AValue) <= Progress.Gauge2.MaxValue
    then Progress.Gauge2.Progress := Progress.Gauge2.Progress + AValue
    else Progress.Gauge2.Progress := Progress.Gauge2.MaxValue;
    InternalUpdate;
  end;
end;

procedure UpdateProgressMax1(const AValue: Integer);
begin
  if Progress <> nil then
  begin
    Progress.Gauge1.MaxValue := AValue;
    if Progress.Gauge1.Progress > Progress.Gauge1.MaxValue
    then Progress.Gauge1.Progress := Progress.Gauge1.MaxValue;
    InternalUpdate;
  end;
end;

procedure UpdateProgressMax2(const AValue: Integer);
begin
  if Progress <> nil then
  begin
    Progress.Gauge2.MaxValue := AValue;
    if Progress.Gauge2.Progress > Progress.Gauge2.MaxValue
    then Progress.Gauge2.Progress := Progress.Gauge2.MaxValue;
    InternalUpdate;
  end;
end;

{ TFormProgress }

procedure TFormProgress2.FormCreate(Sender: TObject);
begin
  Font := Screen.IconFont;
end;

function TFormProgress2.GetStopButton: Boolean;
begin
  Result := BtnStop.Visible;
end;

procedure TFormProgress2.SetStopButton(const Value: Boolean);
begin
  if BtnStop.Visible <> Value then
  begin
    BtnStop.Visible := Value;
    if BtnStop.Visible
    then Height := 137
    else Height := 100;
  end;
end;

procedure TFormProgress2.BtnStopClick(Sender: TObject);
begin
  StopFlag := True;
end;

function TFormProgress2.GetStopFlag: Boolean;
begin
  Result := not BtnStop.Enabled;
end;

procedure TFormProgress2.SetStopFlag(const Value: Boolean);
begin
  BtnStop.Enabled := not Value;
end;

initialization
  Progress := nil;

finalization
  if Progress <> nil then Progress.Free;

end.
