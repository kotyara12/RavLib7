unit RToolTip;

interface

uses
  Controls, Graphics;

type
  TBallonTipIconType = (itNoIcon, itInformation, itWarning, itError);

procedure ShowBalloonTip(Control: TWinControl; Icon: TBallonTipIconType;
  Title: PChar; Text: PChar; BackCL, TextCL: TColor);

implementation

uses
  Commctrl, Messages, Types, Windows, RSysUtils, RDialogs;

procedure ShowBalloonTip(Control: TWinControl; Icon: TBallonTipIconType;
  Title: PChar; Text: PChar; BackCL, TextCL: TColor);
const
  TOOLTIPS_CLASS = 'tooltips_class32';
  TTS_ALWAYSTIP = $01;
  TTS_NOPREFIX = $02;
  TTS_BALLOON = $40;
  TTF_SUBCLASS = $0010;
  TTF_TRANSPARENT = $0100;
  TTF_CENTERTIP = $0002;
  TTM_ADDTOOL = $0400 + 50;
  TTM_SETTITLE = (WM_USER + 32);
var
  hWndTip: THandle;
  ti: TToolInfo;
  hWnd: THandle;
  buffer: array[0..255] of char;
begin
  hWnd := Control.Handle;
  hWndTip := CreateWindow(TOOLTIPS_CLASS, nil, TTS_BALLOON or TTS_ALWAYSTIP,
    0, 0, 0, 0, hWnd, 0, HInstance, nil);
  if hWndTip <> 0 then
  begin
    SetWindowPos(hWndTip, HWND_TOPMOST, 0, 0, 0, 0,
      SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
    ti.hInst := HInstance;
    ti.cbSize := SizeOf(ti);
    ti.uFlags := TTF_CENTERTIP or TTF_TRANSPARENT or TTF_SUBCLASS;
    ti.hwnd := hWnd;
    ti.lpszText := Text;
    Windows.GetClientRect(hWnd, ti.rect);
    FillChar(buffer, SizeOf(buffer), #0);
    LStrCpy(buffer, Title);
    SendMessage(hWndTip, TTM_SETTIPBKCOLOR, BackCL, 0);
    SendMessage(hWndTip, TTM_SETTIPTEXTCOLOR, TextCL, 0);
    SendMessage(hWndTip, TTM_ADDTOOL, 0, Integer(@ti));
    SendMessage(hWndTip, TTM_SETTITLE, Integer(Icon), Integer(@buffer));
  end
  else RaiseSystemError;
end;

end.
