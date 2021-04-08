unit RScrViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RMsgTypes, RScripts, RScrCnst, ExtCtrls, ComCtrls, StdCtrls, Buttons,
  Gauges;

type
  TFormScrViewer = class(TForm)
    ProgressPanel: TPanel;
    LogView: TRichEdit;
    TransactionPanel: TPanel;
    TransactionBottomBevel: TBevel;
    TransactionTopBevel: TBevel;
    TransactionLeftBevel: TBevel;
    TransactionRightBevel: TBevel;
    TransactionInnerPanel: TPanel;
    TransactionCompletePanel: TPanel;
    TransactionInfoBevel: TBevel;
    TransactionInfo: TLabel;
    CurrentFilePanel: TPanel;
    CurrentFileBevel: TBevel;
    TransactionProgressPanel: TPanel;
    TransactionStatistic: TLabel;
    TransactionProgressBevel: TBevel;
    TransactionBar: TProgressBar;
    TopPanel: TPanel;
    OperationPanel: TPanel;
    OperationLeftBevel: TBevel;
    OpertionRightBevel: TBevel;
    OperationBottomBevel: TBevel;
    OperationTopBevel: TBevel;
    OperationInnerPanel: TPanel;
    OperationInfo: TLabel;
    OperationStatistic: TLabel;
    OperationProgressBevel: TBevel;
    OperationInfoBevel: TBevel;
    OperationBar: TProgressBar;
    ButtonsPanel: TPanel;
    LogOpenBtn: TBitBtn;
    CloseBtn: TBitBtn;
    LogCloseBtn: TBitBtn;
    StopBtn: TBitBtn;
    FileGauge: TGauge;
    StatusPanel: TPanel;
    GlobalShape: TShape;
    ScriptShape: TShape;
    procedure FormCreate(Sender: TObject);
    procedure LogOpenBtnClick(Sender: TObject);
    procedure LogCloseBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fLogHeight: Integer;
    fCanClose: Boolean;
    procedure SetCanClose(const Value: Boolean);
  public
    procedure CloseLog;
    procedure ShowLog;
    property CanCloseViewer: Boolean read fCanClose write SetCanClose;
  end;

  TRShotViewer = packed record
    OpText: string;
    OpStat: string;
    OpPrgMax: Integer;
    OpPrgPos: Integer;
    TrText: string;
    TrStat: string;
    TrPrgMax: Integer;
    TrPrgPos: Integer;
  end;

  TCloseOnExitState = (ceAuto, ceAlways, ceManual);

  TRScriptViewer = class (TRScriptLogger)
  private
    fViewer: TFormScrViewer;
    fShotBuffer: TRShotViewer;
    fShowMode: TScriptShowMode;
    fLastState: TRScriptState;
    fCloseOnExit: TCloseOnExitState;
    procedure ShowViewer(const ShowMode: TScriptShowMode);
    procedure CloseViewer;
  private
    fScriptStart: TDateTime;
    fOperationStart: TDateTime;
    procedure DoCloseViewer(Sender: TObject);
    procedure DoScriptBreak(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoShowInfo(Sender: TObject; const TimeStamp: TDateTime;
      const MsgClass: TRMsgClass; const MsgState: TRMsgState; const MsgText: string); override;
    procedure DoShowState(Sender: TObject; const Global: Boolean;
      const State: TRScriptState); override;
    procedure DoShowProgress(Sender: TObject; const MsgClass: TRMsgClass;
      const CurrPos, MaxPos: Integer); override;
    procedure DoFileProgress(Sender: TObject; const CurrPos, MaxPos: Integer); override;
    procedure DoCheckBreak(Sender: TObject; var IsBreak: Boolean); override;
    procedure DoUpdateControls(Sender: TObject; const BreakEnabled, CloseEnabled: Boolean); override;
    procedure DoInitVisualComponents(Sender: TObject); override;
    procedure DoDoneVisualComponents(Sender: TObject); override;
    procedure DoCompileStart(Sender: TObject); override;
    procedure DoExecuteStart(Sender: TObject; const BreakEnabled: Boolean); override;
    procedure DoShotCreate;
    procedure DoShotRestore;
  published
    property ShowMode: TScriptShowMode read fShowMode write fShowMode default smLogHidden;
    property CloseOnExit: TCloseOnExitState read fCloseOnExit write fCloseOnExit default ceAlways;
  end;

procedure Register;

implementation

{$R *.dfm}

uses
  RDialogs, RMsgViewer;

{ Register }

procedure Register;
begin
  RegisterComponents('Rav Soft', [TRScriptViewer]);
end;

{ TRScriptViewer }

constructor TRScriptViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fViewer := nil;
  fShowMode := smLogHidden;
  fCloseOnExit := ceAlways;
  fLastState := stOk;
end;

destructor TRScriptViewer.Destroy;
begin
  if fViewer <> nil then
    fViewer.Free;
  inherited Destroy;
end;

procedure TRScriptViewer.ShowViewer(const ShowMode: TScriptShowMode);
begin
  if fViewer <> nil then
    fViewer.Free;
  if ShowMode <> smHideAll then
  begin
    fViewer := TFormScrViewer.Create(Owner);
    fViewer.CanCloseViewer := False;
    fViewer.OnDestroy := DoCloseViewer;
    fViewer.StopBtn.OnClick := DoScriptBreak;
    fViewer.LogOpenBtn.Enabled := ShowMode in [smLogHidden, smLogShowing, smLogAlways];
    fViewer.LogCloseBtn.Enabled := ShowMode in [smLogDisable, smLogHidden, smLogShowing];
    if ShowMode in [smLogDisable, smLogHidden] then
      fViewer.CloseLog;
    if ShowMode in [smLogShowing, smLogAlways] then
      fViewer.ShowLog;
    fViewer.Show;
    fViewer.BringToFront;
    fViewer.Update;
  end;
end;

procedure TRScriptViewer.CloseViewer;
begin
  if fViewer <> nil then
  begin
    fViewer.CanCloseViewer := True;

    if (fCloseOnExit = ceAlways)
    or ((fCloseOnExit = ceAuto) and (fLastState = stOk)) then
      fViewer.Close;
  end;
end;

procedure TRScriptViewer.DoCloseViewer(Sender: TObject);
begin
  if fViewer <> nil then
    fViewer.OnDestroy := nil;
  fViewer := nil;
end;

procedure TRScriptViewer.DoInitVisualComponents(Sender: TObject);
begin
  fScriptStart := Now;
  if fViewer <> nil then
    fViewer.OperationBar.Max := 0;
  ShowViewer(fShowMode);
  inherited DoInitVisualComponents(Sender);
end;

procedure TRScriptViewer.DoDoneVisualComponents(Sender: TObject);
begin
  inherited DoDoneVisualComponents(Sender);
  CloseViewer;
end;

procedure TRScriptViewer.DoCompileStart(Sender: TObject);
begin
  // fScriptStart := Now;
  // fOperationOffset := 0;
  if (fViewer <> nil) and (Sender is TRScriptCustom) then
    fViewer.Caption := TRScriptCustom(Sender).GetScriptTitle;
  inherited DoCompileStart(Sender);
end;

procedure TRScriptViewer.DoExecuteStart(Sender: TObject; const BreakEnabled: Boolean);
begin
  // fScriptStart := Now;
  // fOperationOffset := 0;
  if fViewer <> nil then
    fViewer.StopBtn.Enabled := BreakEnabled;
  if (fViewer <> nil) and (Sender is TRScriptCustom) then
    fViewer.Caption := TRScriptCustom(Sender).GetScriptTitle;
  inherited DoExecuteStart(Sender, BreakEnabled);
end;

procedure TRScriptViewer.DoShowInfo(Sender: TObject;
  const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
  const MsgState: TRMsgState; const MsgText: string);
var
  ViewText: string;
begin
  inherited DoShowInfo(Sender, TimeStamp, MsgClass, MsgState, MsgText);
  if fViewer <> nil then
  begin
    if (MsgClass in [mcSystem, mcCommand, mcOperation])
      and (MsgState = msTitle) then
        fOperationStart := Now;
    // Отображение информации в окне индикатора
    if Trim(MsgText) = EmptyStr
    then ViewText := SViewerWait
    else ViewText := Trim(MsgText);
    case MsgClass of
      mcSystem, mcCommand:
      begin
        if MsgState in [msTitle] then
        begin
          fViewer.OperationInfo.Caption := ViewText;
          fViewer.TransactionInfo.Caption := SViewerWait;
        end;
        if MsgState in [msOk..msBreak] then
          fViewer.TransactionInfo.Caption := ViewText;
      end;
      mcTransaction:
        if MsgState = msTitle then
          fViewer.TransactionInfo.Caption := ViewText;
    end;
    // Отображение в окне протокола
    if MsgIsLog(MsgClass, MsgState) then
    begin
      if MsgClass in [mcSystem, mcCommand, mcOperation] then
        fViewer.LogView.SelAttributes.Style := [fsBold];
      if (MsgClass in [mcCommand]) and (MsgState in [msInfo]) then
        fViewer.LogView.SelAttributes.Style := [fsBold, fsUnderline];
      fViewer.LogView.SelAttributes.Color := CRMsgState[MsgState];
      fViewer.LogView.Lines.Add(MsgFormatEx(MsgClass, MsgState, MsgText));
    end;
    // fViewer.BringToFront;
    fViewer.Update;
  end;
  Application.ProcessMessages;
end;

procedure TRScriptViewer.DoShowState(Sender: TObject;
  const Global: Boolean; const State: TRScriptState);
resourcestring
  sGlobalHeader = 'Задание - текущий статус "%s"';
  sScriptHeader = 'Сценарий - текущий статус "%s"';
var
  clState: TColor;
begin
  inherited DoShowState(Sender, Global, State);
  if fViewer <> nil then
  begin
    case State of
      stOk:
        clState := $0000D500;
      stWarning:
        clState := $0000BAD5;
      else
        clState := $000000FF;
    end;
    if Global then
    begin
      fViewer.GlobalShape.Brush.Color := clState;
      fViewer.GlobalShape.Hint := Format(sGlobalHeader, [SRScriptState[State]]);

      fLastState := State;
    end
    else begin
      fViewer.ScriptShape.Brush.Color := clState;
      fViewer.ScriptShape.Hint := Format(sScriptHeader, [SRScriptState[State]]);
    end;
    fViewer.Update;
  end;
  Application.ProcessMessages;
end;

procedure TRScriptViewer.DoShowProgress(Sender: TObject;
  const MsgClass: TRMsgClass; const CurrPos, MaxPos: Integer);
var
  Percent: Double;
  TimeRun, TimeEst: string;
begin
  inherited DoShowProgress(Sender, MsgClass, CurrPos, MaxPos);
  if (fViewer <> nil) then
  begin
    case MsgClass of
      mcOperation:
      begin
        with fViewer.OperationBar do
        begin
          Max := MaxPos;
          Position := CurrPos;
          try
            if Max > 0
            then Percent := 100 * Position / Max
            else Percent := 0;
            TimeRun := Format(SViewerStatComplete, [Percent, Position, Max,
              TimeToStr(Now - fScriptStart)]);
          except
            TimeRun := SViewerEmpty;
          end;
        end;
        fViewer.OperationStatistic.Caption := TimeRun;
        fViewer.TransactionStatistic.Caption := SViewerEmpty;
      end;
      mcTransaction:
      begin
        with fViewer.TransactionBar do
        begin
          Max := MaxPos;
          if CurrPos < 0
          then Position := 0
          else Position := CurrPos;
          if CurrPos < 1 then
          begin
            if (CurrPos = 0) and (MaxPos > 0) then
              fOperationStart := Now;
            TimeRun := SViewerEmpty;
            TimeEst := SViewerEmpty;
          end
          else begin
            try
              if MaxPos > 0
              then Percent := 100 * CurrPos / MaxPos
              else Percent := 0;
              TimeRun := Format(SViewerStatComplete, [Percent, CurrPos, MaxPos,
                TimeToStr(Now - fOperationStart)]);
            except
              TimeRun := SViewerEmpty;
            end;
            try
              TimeEst := Format(SViewerStatElapsed,
                [TimeToStr((MaxPos - CurrPos) * (Now - fOperationStart) / CurrPos)]);
            except
              TimeEst := SViewerEmpty;
            end;
          end;
        end;
        fViewer.TransactionStatistic.Caption := TimeRun + TimeEst;
      end;
    end;
    // fViewer.BringToFront;
    fViewer.Update;
  end;
  Application.ProcessMessages;
end;

procedure TRScriptViewer.DoFileProgress(Sender: TObject; const CurrPos, MaxPos: Integer);
begin
  inherited DoFileProgress(Sender, CurrPos, MaxPos);
  if (fViewer <> nil) then
  begin
    if MaxPos > 0 then
    begin
      fViewer.FileGauge.Visible := True;
      fViewer.FileGauge.MaxValue := MaxPos;
      fViewer.FileGauge.Progress := CurrPos;
    end
    else begin
      fViewer.FileGauge.Visible := False;
      fViewer.FileGauge.MaxValue := 1;
      fViewer.FileGauge.Progress := 0;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TRScriptViewer.DoUpdateControls(Sender: TObject;
  const BreakEnabled, CloseEnabled: Boolean);
begin
  inherited DoUpdateControls(Sender, BreakEnabled, CloseEnabled);
  if fViewer <> nil then
  begin
    fViewer.StopBtn.Enabled := BreakEnabled;
    fViewer.CanCloseViewer := CloseEnabled;
    // fViewer.BringToFront;
    fViewer.Update;
  end;
  Application.ProcessMessages;
end;

procedure TRScriptViewer.DoScriptBreak(Sender: TObject);
begin
  if QueryBoxStdNY(SExecuteBreakQry) = ID_YES then
  begin
    if fViewer <> nil then
      fViewer.StopBtn.Enabled := False;
    BreakScript;
  end;
end;

procedure TRScriptViewer.DoCheckBreak(Sender: TObject; var IsBreak: Boolean);
begin
  inherited DoCheckBreak(Sender, IsBreak);
  Application.ProcessMessages;
end;

procedure TRScriptViewer.DoShotCreate;
begin
  if fViewer <> nil then
  begin
    fShotBuffer.OpText := fViewer.OperationInfo.Caption;
    fShotBuffer.OpStat := fViewer.OperationStatistic.Caption;
    fShotBuffer.OpPrgMax := fViewer.OperationBar.Max;
    fShotBuffer.OpPrgPos := fViewer.OperationBar.Position;
    fShotBuffer.TrText := fViewer.TransactionInfo.Caption;
    fShotBuffer.TrStat := fViewer.TransactionStatistic.Caption;
    fShotBuffer.TrPrgMax := fViewer.TransactionBar.Max;
    fShotBuffer.TrPrgPos := fViewer.TransactionBar.Position;
  end;
end;

procedure TRScriptViewer.DoShotRestore;
begin
  if fViewer <> nil then
  begin
    fViewer.OperationInfo.Caption := fShotBuffer.OpText;
    fViewer.OperationStatistic.Caption := fShotBuffer.OpStat;
    fViewer.OperationBar.Max := fShotBuffer.OpPrgMax;
    fViewer.OperationBar.Position := fShotBuffer.OpPrgPos;
    fViewer.TransactionInfo.Caption := fShotBuffer.TrText;
    fViewer.TransactionStatistic.Caption := fShotBuffer.OpStat;
    fViewer.TransactionBar.Max := fShotBuffer.TrPrgMax;
    fViewer.TransactionBar.Position := fShotBuffer.TrPrgPos;
  end;
end;

{ TFormScrViewer }

procedure TFormScrViewer.FormCreate(Sender: TObject);
begin
  Font.Name := Screen.MenuFont.Name;
  OperationInfo.Font.Name := Screen.MenuFont.Name;
  TransactionInfo.Font.Name := Screen.MenuFont.Name;
  CanCloseViewer := True;
  fLogHeight := 300;
  CloseLog;
end;

procedure TFormScrViewer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := fCanClose;
  if CanClose then
  begin
    while IsShowMsgViewer do
    begin
      ActivateMsgViewer;
      Application.ProcessMessages;
    end;
    BringToFront;
  end;
end;

procedure TFormScrViewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormScrViewer.ShowLog;
begin
  Constraints.MinHeight := ProgressPanel.Height +
    (Height - ProgressPanel.Height - LogView.Height);
  Constraints.MaxHeight := 0;
  LogOpenBtn.Visible := False;
  LogCloseBtn.Visible := True;
  Height := ProgressPanel.Height + fLogHeight;
  ActiveControl := LogView;
  Update;
end;

procedure TFormScrViewer.CloseLog;
begin
  LogOpenBtn.Visible := True;
  LogCloseBtn.Visible := False;
  if WindowState = wsMaximized then
    WindowState := wsNormal;
  if LogView.Height > 0 then
    fLogHeight := Height - ProgressPanel.Height;
  Constraints.MinHeight := 0;
  Constraints.MaxHeight := 0;
  try
    ClientHeight := ProgressPanel.Height;
  finally
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
  end;
  Update;
end;

procedure TFormScrViewer.LogOpenBtnClick(Sender: TObject);
begin
  ShowLog;
end;

procedure TFormScrViewer.LogCloseBtnClick(Sender: TObject);
begin
  CloseLog;
end;

procedure TFormScrViewer.SetCanClose(const Value: Boolean);
begin
  fCanClose := Value;
  StopBtn.Visible := not fCanClose;
  CloseBtn.Visible := fCanClose;
end;

procedure TFormScrViewer.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

end.
