object FormMsgViewer: TFormMsgViewer
  Left = 236
  Top = 195
  Width = 524
  Height = 375
  ActiveControl = MsgList
  Caption = #1057#1086#1086#1073#1097#1077#1085#1080#1103
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Icon.Data = {
    0000010001001010000001000800680500001600000028000000100000002000
    000001000800000000004001000000000000000000000000000000000000FFFF
    FF00EEE7E300B89A8B009A6D530095614300945E3F0097684D00B5958500EAE2
    DD00C3A99C009F6F5100C5987000DCB18500DEAE8300D6A07400C58B6000A86C
    46008F583A00BDA29300AE8C7A00B88B6A00F2D2A900F2CFA300F1D5B400D39A
    6D00C2855B00AF714A008C502F00A6806C00FDFDFC00C3AA9D00BA907000FEE9
    CC00F4DDC000EECEA800F3DDC300D69F7200C5895F00B0734C009E5F3C008347
    2600B99B8C00EFE8E4009B6A4B00F4D4B000F7E5D100F5E1CA00EFD0AB00EBC7
    9B00E1B38600D59E7100B2754E009B5D3A00894C2B007A412300E9DFDB00B595
    8600C1926A00F0CDA500F1D4B300EECBA400EBC69A00E7BD9100E2B99200C386
    5C00B0724C009C5E3A00874A2900753B1E00B1907F0096664C00D8A77B00E8BE
    9200E9C39900EBC69C00EDCDA700CE926600BC7F5700AA6D4600975936008549
    270072391E008A5B44008F593B00D7A57900E0B28500E3B78A00E4B98C00E4B8
    8B00C4885D00B4775000A36540009053310080452300703A1E0078442B008D56
    3500CD976C00D7A47800DAA87C00DBAA7C00DAA77A00B87C5400AA6D4700995B
    3900884B2B007C4120006D371E0076432B00905E4400BF845B00CB936700CD96
    6B00CF976C00CD956900AD6F48009F623D00905230007F432300783E1D006532
    1D0086563F00AF8D7C00A2664200BF825900C0845A00C1845B00A0623D009255
    3300834626007A3F1E006E371D00632F1B00AB887500E9E0DB00844E31009F66
    4400B2744D00B1734C00AE704A0091543100844827007B411E0070381D006230
    1D006A341B00E2D6CF00BA9D8D007B452A008A5336009E613D00AC765600773D
    1E006E371E0063301D0064311A00AB887700FDFCFB00A37B6500733D2400713E
    28007B442A007E4629007B422500743C21006D361D0065331E00602F1D006330
    1B00976C5700FAF7F600B4948300743E230066321D0064321D0064311D006531
    1C00AC897700E4D8D200AD8B790087584000774229007743290085553D00A986
    7300E0D3CB000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000BABA
    BABABAB3B4B5B6B7B8BABABABABABABABAABACADAE9A9AAFB06BB1BABABABABA
    9E9FA0A1A2A3A4A5A6A7A8A9BABABA93949596970000000098999A9B9CBABA87
    88898A8B8B00008C8D8E8F9091BA7A7B7C7D7E7C7C00007F8081828384856D6E
    6F70717272000073747576777879606162636465650000666768696A6B6C5354
    55565758580000595A5B5C5D5E5F464748494A4B0000004C4D4E4F5051523839
    3A173B3C3D3E3F18404142434445BA2B2C2D2E2F303031322533343536BABA1E
    1F202122230000242526272829BABABA1314151617000018191A1B1CBABABABA
    BA090A0B0C0D0E0F101112BABABABABABABABA020304050607BABABABABAF81F
    0000E0070000C003000080010000800100000000000000000000000000000000
    000000000000000000008001000080010000C0030000E0070000F81F0000}
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 307
    Width = 516
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object MsgList: TRichEdit
    Left = 0
    Top = 0
    Width = 516
    Height = 307
    Align = alClient
    BevelKind = bkFlat
    BorderStyle = bsNone
    PopupMenu = PopupMenu
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object ActionList: TActionList
    Left = 12
    Top = 12
    object CloseMsgs: TAction
      Caption = #1047#1072#1082#1088#1099#1090#1100
      Hint = #1047#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086
      ImageIndex = 10
      ShortCut = 16499
      OnExecute = CloseMsgsExecute
      OnUpdate = CloseMsgsUpdate
    end
    object EditCopy: TEditCopy
      Category = 'Edit'
      Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100
      Hint = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1081' '#1092#1088#1072#1075#1084#1077#1085#1090' '#1074' '#1073#1091#1074#1077#1088' '#1086#1073#1084#1077#1085#1072
      ImageIndex = 1
      ShortCut = 16429
    end
    object EditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077
      Hint = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1077#1089#1100' '#1090#1077#1082#1089#1090
      ShortCut = 16449
    end
  end
  object MainMenu: TMainMenu
    Left = 40
    Top = 12
    object itemEditSelectAll: TMenuItem
      Action = EditSelectAll
    end
    object itemEditCopy: TMenuItem
      Action = EditCopy
    end
    object itemCloseMsgs: TMenuItem
      Action = CloseMsgs
    end
  end
  object PopupMenu: TPopupMenu
    Left = 68
    Top = 12
    object itemEditCopyP: TMenuItem
      Action = EditCopy
    end
    object itemEditSelectAllP: TMenuItem
      Action = EditSelectAll
    end
  end
end