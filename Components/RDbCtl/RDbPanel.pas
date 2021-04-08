unit RDbPanel;

interface

uses
  Classes, Controls, SysUtils, Graphics, ExtCtrls, Db;

type
  TRDbInfoPanel = class (TPanel)
  private
    FUpdateCount: Integer;
    FLabelFont: TFont;
    FTextFont: TFont;
    FTextColor: TColor;
    FExtOnResize: TNotifyEvent;
    procedure SetLabelFont(Value: TFont);
    procedure SetTextFont(Value: TFont);
    procedure SetTextColor(Value: TColor);
    function  IsFontLabelsStore: Boolean;
    function  IsFontTextsStore: Boolean;
    function  IsTextColorsStore: Boolean;
    procedure IntOnResize(Sender: TObject);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateControls;
    procedure UpdateDbHints;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property FontLabels: TFont read FLabelFont write SetLabelFont stored IsFontLabelsStore;
    property FontTexts: TFont read FTextFont write SetTextFont stored IsFontTextsStore;
    property ColorText: TColor read FTextColor write SetTextColor stored IsTextColorsStore;
    property OnResize: TNotifyEvent read FExtOnResize write FExtOnResize;
  end;


implementation

uses
  StdCtrls, DbCtrls, RDbUtils, RDbText;

{ == TRDbInfoPanel ============================================================= }

constructor TRDbInfoPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited OnResize := IntOnResize;
  Align := alBottom;
  BevelInner := bvNone;
  BevelOuter := bvLowered;
  FUpdateCount := 0;
  FLabelFont := TFont.Create;
  FLabelFont.Assign(Font);
  FTextFont := TFont.Create;
  FTextFont.Assign(Font);
  FTextColor := clWindow;
  UpdateControls;
  UpdateDbHints;
end;

destructor TRDbInfoPanel.Destroy;
begin
  FLabelFont.Free;
  FTextFont.Free;
  inherited Destroy;
end;

function TRDbInfoPanel.IsFontLabelsStore: Boolean;
begin
  Result := FLabelFont.Handle <> Font.Handle;
end;

function TRDbInfoPanel.IsFontTextsStore: Boolean;
begin
  Result := FTextFont.Handle <> Font.Handle;
end;

function TRDbInfoPanel.IsTextColorsStore: Boolean;
begin
  Result := FTextColor <> clWindow;
end;

procedure TRDbInfoPanel.Loaded;
begin
  BeginUpdate;
  try
    inherited Loaded;
    UpdateDbHints;
  finally
    EndUpdate;
  end;
end;

procedure TRDbInfoPanel.SetLabelFont(Value: TFont);
begin
  FLabelFont.Assign(Value);
  UpdateControls;
end;

procedure TRDbInfoPanel.SetTextFont(Value: TFont);
begin
  FTextFont.Assign(Value);
  UpdateControls;
end;

procedure TRDbInfoPanel.SetTextColor(Value: TColor);
begin
  if FTextColor <> Value then begin
    FTextColor := Value;
    UpdateControls;
  end;
end;

procedure TRDbInfoPanel.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TRDbInfoPanel.EndUpdate;
begin
  if FUpdateCount > 0 then Dec(FUpdateCount);
  if FUpdateCount = 0 then UpdateControls;
end;

procedure TRDbInfoPanel.UpdateControls;

  procedure UpdateWinControl(WC: TWinControl);
  var
    i: Integer;
    Control: TControl;
  begin
    for i := 0 to WC.ControlCount - 1 do
    begin
      Control := WC.Controls[i];
      if Control is TWinControl then
        UpdateWinControl(TWinControl(Control));
      if Control is TLabel then
      begin
        TLabel(Control).Font := FLabelFont;
      end;
      if Control is TImage then
      begin
        if not Assigned(TImage(Control).PopupMenu) then
          TImage(Control).PopupMenu := PopupMenu;
      end;
      if Control is TDbText then
      begin
        TDbText(Control).Font := FTextFont;
        TDbText(Control).Color := FTextColor;
        if not Assigned(TDbText(Control).PopupMenu) then
          TDbText(Control).PopupMenu := PopupMenu;
      end;
      if Control is TDbMemo then
      begin
        TDbMemo(Control).Font := FTextFont;
        TDbMemo(Control).Color := FTextColor;
        if not Assigned(TDbText(Control).PopupMenu) then
          TDbMemo(Control).PopupMenu := PopupMenu;
      end;
      if Control is TRDbText then
      begin
        TRDbText(Control).Font := FTextFont;
        TRDbText(Control).Color := FTextColor;
        if not Assigned(TDbText(Control).PopupMenu) then
          TRDbText(Control).PopupMenu := PopupMenu;
      end;
      if Control is TStaticText then
      begin
        TStaticText(Control).Font := FTextFont;
        TStaticText(Control).Color := FTextColor;
        if not Assigned(TDbText(Control).PopupMenu) then
          TStaticText(Control).PopupMenu := PopupMenu;
      end;
    end;
  end;

begin
  if FUpdateCount < 1 then
    UpdateWinControl(Self);
end;

procedure TRDbInfoPanel.UpdateDbHints;
begin
  FillDbHints(Self);
end;

procedure TRDbInfoPanel.IntOnResize(Sender: TObject);

  procedure UpdateWinControl(WC: TWinControl);
  var
    i: Integer;
    Control: TControl;
  begin
    for i := 0 to WC.ControlCount - 1 do
    begin
      Control := WC.Controls[i];
      if Control is TWinControl then
        UpdateWinControl(TWinControl(Control));
      if Control.Tag < 0 then
        Control.Width := Width - Control.Left + Control.Tag;
    end;
  end;

begin
  if FUpdateCount < 1 then
    UpdateWinControl(Self);
  if Assigned(FExtOnResize) then
    FExtOnResize(Sender);
end;

end.
