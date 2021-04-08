unit RLvLogHandler;

interface

uses
  RExHandlers, ComCtrls;

type
  TExHandlerListView = class (TExHandlerStd)
  private
    FColumnDataTime: Integer;
    FColumnMessage: Integer;
    FImageError: Integer;
    FLogListView: TListView;
  protected
    procedure LogError; override;
  public
    property ColumnDataTime: Integer read FColumnDataTime write FColumnDataTime;
    property ColumnMessage: Integer read FColumnMessage write FColumnMessage;
    property ImageError: Integer read FImageError write FImageError;
    property LogListView: TListView read FLogListView write FLogListView;
    constructor Create(Log: TListView);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, RDialogs;

constructor TExHandlerListView.Create(Log: TListView);
begin
  inherited Create;
  FLogListView := Log;
  FColumnDataTime := -1;
  FColumnMessage := -1;
  FImageError := -1;
  if Assigned(FLogListView) then
  begin
    if FLogListView.Columns.Count > 1
    then FColumnDataTime := 0;
    if FLogListView.Columns.Count > 1
    then FColumnMessage := 1
    else if FLogListView.Columns.Count = 1
         then FColumnMessage := 0;
  end;
end;

destructor TExHandlerListView.Destroy;
begin
  FLogListView := nil;
  inherited Destroy;
end;

procedure TExHandlerListView.LogError;
begin
  if Assigned(FLogListView) then
  begin
    with FLogListView.Items.Add do
    begin
      ImageIndex := FImageError;
      if FColumnDataTime = 0 then Caption := DateTimeToStr(Now);
      if FColumnMessage = 0 then Caption := GetErrorLineMessage;
      if FColumnDataTime > 0 then
      begin
        if Subitems.Count < FColumnDataTime then
        begin
          while Subitems.Count < FColumnDataTime - 1 do
            Subitems.Add(EmptyStr);
          Subitems.Add(DateTimeToStr(Now));
        end
        else Subitems[FColumnDataTime - 1] := DateTimeToStr(Now);
      end;
      if FColumnMessage > 0 then
      begin
        if Subitems.Count < FColumnMessage then
        begin
          while Subitems.Count < FColumnMessage - 1 do
            Subitems.Add(EmptyStr);
          Subitems.Add(GetErrorLineMessage);
        end
        else Subitems[FColumnMessage - 1] := GetErrorLineMessage;
      end;
    end;
  end;
end;

end.
