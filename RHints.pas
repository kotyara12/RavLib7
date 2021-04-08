unit RHints;

interface

uses
  Types, Controls, Forms, Windows, SysUtils;

function RevealHint(Control: TControl): THintWindow;
procedure RemoveHint(var Hint: THintWindow);

implementation

function RevealHint(Control: TControl): THintWindow;

{----------------------------------------------------------------}
{ Демонстрирует всплывающую подсказку для определенного элемента }
{ управления (Control), возвращает ссылку на hint-объект,        }
{ поэтому в дальнейшем подсказка может быть спрятана вызовом     }
{ RemoveHint (смотри ниже).                                      }
{----------------------------------------------------------------}

var
  ShortHint: string;
  AShortHint: array[0..255] of Char;
  HintPos: TPoint;
  HintBox: TRect;

begin
  { Создаем окно: }
  Result := THintWindow.Create(Control);

  { Получаем первую часть подсказки до '|': }
  ShortHint := GetShortHint(Control.Hint);

  { Вычисляем месторасположение и размер окна подсказки }
  HintPos := Control.ClientOrigin;
  Inc(HintPos.Y, Control.Height + 2);
  HintBox := Bounds(0, 0, Screen.Width, 0);
  DrawText(Result.Canvas.Handle,
     StrPCopy(AShortHint, ShortHint), -1, HintBox,
     DT_CALCRECT or DT_LEFT or DT_WORDBREAK or DT_NOPREFIX);
  OffsetRect(HintBox, HintPos.X, HintPos.Y);
  Inc(HintBox.Right, 6);
  Inc(HintBox.Bottom, 2);

  { Теперь показываем окно: }
  Result.ActivateHint(HintBox, ShortHint);
end; {RevealHint}

procedure RemoveHint(var Hint: THintWindow);
{----------------------------------------------------------------}
{ Освобождаем дескриптор окна всплывающей подсказки, выведенной  }
{ предыдущим RevealHint.                                         }
{----------------------------------------------------------------}
begin
  Hint.ReleaseHandle;
  Hint.Free;
  Hint := nil;
end; {RemoveHint}

end.
