unit RHints;

interface

uses
  Types, Controls, Forms, Windows, SysUtils;

function RevealHint(Control: TControl): THintWindow;
procedure RemoveHint(var Hint: THintWindow);

implementation

function RevealHint(Control: TControl): THintWindow;

{----------------------------------------------------------------}
{ ������������� ����������� ��������� ��� ������������� �������� }
{ ���������� (Control), ���������� ������ �� hint-������,        }
{ ������� � ���������� ��������� ����� ���� �������� �������     }
{ RemoveHint (������ ����).                                      }
{----------------------------------------------------------------}

var
  ShortHint: string;
  AShortHint: array[0..255] of Char;
  HintPos: TPoint;
  HintBox: TRect;

begin
  { ������� ����: }
  Result := THintWindow.Create(Control);

  { �������� ������ ����� ��������� �� '|': }
  ShortHint := GetShortHint(Control.Hint);

  { ��������� ����������������� � ������ ���� ��������� }
  HintPos := Control.ClientOrigin;
  Inc(HintPos.Y, Control.Height + 2);
  HintBox := Bounds(0, 0, Screen.Width, 0);
  DrawText(Result.Canvas.Handle,
     StrPCopy(AShortHint, ShortHint), -1, HintBox,
     DT_CALCRECT or DT_LEFT or DT_WORDBREAK or DT_NOPREFIX);
  OffsetRect(HintBox, HintPos.X, HintPos.Y);
  Inc(HintBox.Right, 6);
  Inc(HintBox.Bottom, 2);

  { ������ ���������� ����: }
  Result.ActivateHint(HintBox, ShortHint);
end; {RevealHint}

procedure RemoveHint(var Hint: THintWindow);
{----------------------------------------------------------------}
{ ����������� ���������� ���� ����������� ���������, ����������  }
{ ���������� RevealHint.                                         }
{----------------------------------------------------------------}
begin
  Hint.ReleaseHandle;
  Hint.Free;
  Hint := nil;
end; {RemoveHint}

end.
