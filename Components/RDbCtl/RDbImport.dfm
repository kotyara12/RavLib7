inherited FormDbImport: TFormDbImport
  Left = 300
  Top = 187
  Caption = #1052#1072#1089#1090#1077#1088' '#1080#1084#1087#1086#1088#1090#1072' '#1076#1072#1085#1085#1099#1093
  ClientHeight = 480
  ClientWidth = 641
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 429
    Width = 641
  end
  inherited HeaderBevel: TBevel
    Width = 641
  end
  inherited ButtonsPanel: TPanel
    Top = 431
    Width = 641
    TabOrder = 2
    inherited ButtonsMovedPanel: TPanel
      Left = 315
      inherited PrevBtn: TBitBtn
        OnClick = PrevBtnClick
      end
      inherited NextBtn: TBitBtn
        OnClick = NextBtnClick
      end
    end
  end
  object Notebook: TNotebook [3]
    Left = 0
    Top = 51
    Width = 641
    Height = 378
    Align = alClient
    TabOrder = 1
    OnPageChanged = NotebookPageChanged
    object TPage
      Left = 0
      Top = 0
      Caption = #1042#1099#1073#1086#1088' '#1080#1089#1090#1086#1095#1085#1080#1082#1072' '#1076#1072#1085#1085#1099#1093
      object lblFileName: TLabel
        Left = 16
        Top = 16
        Width = 341
        Height = 13
        Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1092#1072#1081#1083' Microsoft Excel, '#1089#1086#1076#1077#1088#1078#1072#1097#1077#1081' '#1085#1077#1086#1073#1093#1086#1076#1080#1084#1099#1077' '#1076#1072#1085#1085#1099#1077':'
        FocusControl = edFileName
      end
      object lblSource: TLabel
        Left = 16
        Top = 92
        Width = 151
        Height = 13
        Caption = #1044#1072#1085#1085#1099#1077' '#1074#1085#1077#1096#1085#1077#1075#1086' '#1080#1089#1090#1086#1095#1085#1080#1082#1072':'
        FocusControl = lvSource
      end
      object edFileName: TStaticText
        Left = 16
        Top = 32
        Width = 581
        Height = 21
        Hint = #1048#1084#1103' '#1092#1072#1081#1083#1072', '#1089#1086#1076#1077#1088#1078#1072#1097#1077#1075#1086' '#1080#1084#1087#1086#1088#1090#1080#1088#1091#1077#1084#1099#1077' '#1076#1072#1085#1085#1099#1077
        AutoSize = False
        BevelInner = bvSpace
        BevelKind = bkSoft
        BorderStyle = sbsSunken
        Color = clWindow
        ParentColor = False
        TabOrder = 0
      end
      object OpenBtn: TBitBtn
        Left = 604
        Top = 32
        Width = 23
        Height = 21
        Hint = #1042#1099#1073#1086#1088' '#1080#1089#1090#1086#1095#1085#1080#1082#1072' '#1076#1072#1085#1085#1099#1093
        Caption = '...'
        TabOrder = 1
        OnClick = OpenBtnClick
      end
      object cbFirstLine: TCheckBox
        Left = 16
        Top = 64
        Width = 613
        Height = 17
        Caption = #1055#1077#1088#1074#1072#1103' '#1089#1090#1088#1086#1082#1072' '#1089#1086#1076#1077#1088#1078#1080#1090' '#1080#1084#1077#1085#1072' '#1089#1090#1086#1083#1073#1094#1086#1074
        TabOrder = 2
        OnClick = FirstLineClick
      end
      object lvSource: TRSortListView
        Left = 16
        Top = 108
        Width = 609
        Height = 253
        Columns = <>
        ColumnClick = False
        FlatScrollBars = True
        GridLines = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        SortType = stData
        TabOrder = 3
        ViewStyle = vsReport
        SortColumn = -1
        SortDirection = sdAscending
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = #1054#1087#1088#1077#1076#1077#1083#1077#1085#1080#1077' '#1080#1084#1087#1086#1088#1090#1080#1088#1091#1077#1084#1099#1093' '#1087#1086#1083#1077#1081
      object SelectFieldsLabel: TLabel
        Left = 16
        Top = 16
        Width = 609
        Height = 49
        AutoSize = False
        Caption = 
          #1042#1099#1073#1077#1088#1080#1090#1077' '#1080#1084#1087#1086#1088#1090#1080#1088#1091#1077#1084#1099#1077' '#1087#1086#1083#1103' '#1080' '#1080#1093' '#1089#1086#1086#1090#1074#1077#1090#1089#1090#1074#1080#1077' '#1084#1077#1078#1076#1091' '#1090#1072#1073#1083#1080#1094#1077#1081' '#1041#1044' ' +
          #1080' '#1074#1085#1077#1096#1085#1080#1084' '#1080#1089#1090#1086#1095#1085#1080#1082#1086#1084'. '#1053#1077#1082#1086#1090#1086#1088#1099#1077' '#1087#1086#1083#1103' '#1084#1086#1075#1091#1090' '#1073#1099#1090#1100' '#1079#1072#1087#1086#1083#1085#1077#1085#1099' '#1087#1086#1089#1090#1086#1103 +
          #1085#1085#1099#1084#1080' '#1079#1085#1072#1095#1077#1085#1080#1103#1084#1080' ('#1082#1086#1085#1089#1090#1072#1085#1090#1086#1081'). '#1044#1083#1103' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103' '#1076#1072#1085#1085#1099#1093' '#1074' '#1090#1072#1073#1083#1080#1094#1077' '#1074 +
          #1099#1073#1077#1088#1080#1090#1077' '#1082#1083#1102#1095#1077#1074#1086#1077' '#1087#1086#1083#1077', '#1079#1085#1072#1095#1077#1085#1080#1077' '#1082#1086#1090#1086#1088#1086#1075#1086' '#1073#1091#1076#1077#1090' '#1087#1088#1086#1074#1077#1088#1103#1090#1089#1103' '#1087#1088#1080' '#1080#1084 +
          #1087#1086#1088#1090#1077'.'
        WordWrap = True
      end
      object lvFields: TRSortListView
        Left = 16
        Top = 68
        Width = 609
        Height = 293
        Checkboxes = True
        Columns = <
          item
            Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1087#1086#1083#1103
            Width = 211
          end
          item
            Caption = #1048#1084#1103' '#1087#1086#1083#1103
            Width = 100
          end
          item
            Caption = #1058#1080#1087' '#1076#1072#1085#1085#1099#1093
            Width = 75
          end
          item
            Caption = #1048#1089#1090#1086#1095#1085#1080#1082
            Width = 100
          end
          item
            Caption = #1050#1086#1085#1089#1090#1072#1085#1090#1072
            Width = 100
          end>
        FlatScrollBars = True
        GridLines = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        PopupMenu = FieldsPopupMenu
        SmallImages = ImageList
        SortType = stData
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvFieldsDblClick
        SortColumn = -1
        SortDirection = sdAscending
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = #1047#1072#1075#1088#1091#1079#1082#1072' '#1076#1072#1085#1085#1099#1093' '#1074' '#1090#1072#1073#1083#1080#1094#1091
      object ShowProgressLabel: TLabel
        Left = 16
        Top = 16
        Width = 215
        Height = 13
        Caption = #1055#1077#1088#1077#1085#1086#1089' '#1076#1072#1085#1085#1099#1093' '#1080#1079' '#1074#1085#1077#1096#1085#1077#1075#1086' '#1080#1089#1090#1086#1095#1085#1080#1082#1072'...'
      end
      object ProgressBar: TProgressBar
        Left = 16
        Top = 32
        Width = 609
        Height = 17
        Step = 1
        TabOrder = 0
      end
      object Log: TMemo
        Left = 16
        Top = 68
        Width = 609
        Height = 293
        PopupMenu = LogPopupMenu
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
      end
    end
  end
  inherited HeaderPanel: TPanel
    Width = 641
    TabOrder = 0
    inherited HeaderImage: TImage
      Picture.Data = {
        055449636F6E0000010001002020000001000800A80800001600000028000000
        2000000040000000010008000000000000000000000000000000000000000000
        0000000000000000FFFFFF0000000000D6A78900EAD2C200FFFBF800EDBC9400
        F4D2B500E5C5AA00F7E0CD00F8E2D000F7C49600B09A8700B6A89C00FDF4EC00
        FCF3EB00FFCE9C00FBCA9A00FFCF9F00FFCFA000FFD0A200FFD1A300FFD2A400
        FFD3A700FFD4A900FFD5AB00FFD6AC00FFD7AF00FFD8B100FFD9B300FFDBB700
        FFDDBB00FFE1C300FFE5CB00FFECD900FFEEDD00FDEEDF00FBEDDF00FFF2E500
        FFF9F300FFFBF700FFFCF900FFD19E00F5CC9F00FFE9D200FFF6EC00FFBF7300
        FFD4A100FFDDB400FFF1E100FEB65500FFD9A600EECEA600FFDEAB00FFE0AD00
        FFECCF00FFE8B600FFEFBC00FFF5C200FFFFE900FFFFF200D4D5BC00BBBFAE00
        D7DED30031513100647764000DA61A00479050004461470014AD2900B7C7BA00
        1FB83D0058B76B005BBB6E0025BA490025A24200278A3E0027C04D0029BF5200
        2CC5570035CB66003DD6700045DD77004AE37D0057F08A0061BD7F002BAE5900
        30B25D0047C07B00BEE3D400088A5C00B0DDD100EDF8F7008FF5EF0099FFFF00
        A5FFFF00A8FFFF00AAFEFF00ADFFFF00B3FFFF00BDFEFF00C3FFFF00D3FFFF00
        D6FFFF00DBFFFF00F4FFFF00EFF5F500FAFFFF0086ECEF0097FDFF0096F3F600
        99F5F700AAFCFF00CBFEFF00E4FEFF0093F9FF00AEFBFF0099F8FF0098F1F800
        7EB3B700CDF6F90073E6F2007EE6F2007FE5EF008BF2FE008EF4FF0091E4EE00
        B9F7FE00D7FBFF0085EEFC008CF2FF00A1F4FF0089EFFF00D2F9FF00DFF2F500
        50A9BC0075D9EC0083E9FF0085EAFF0086ECFF009AEEFF00B2F2FF00EDFBFE00
        80E6FF0099CFDB00C2F1FC003BB9DC0046C4E70069D8F50076DFFC007CE2FF00
        7EE4FF007EE3FF0080E5FF0084E6FF0080DDF50083DFF9008AE4FE001093BE00
        1386AB001BA1CA0023A3CC0026AFDC001B7B9A002EB4DC001F75910032B7E100
        31ACD40035BAE10032A0C40025728B0042BFE80051C9ED0042A2C1003C92AC00
        4DB7DA0056B8D800336D800063D3F4006FD9FC0056A5BF0074DDFC0078DEFF00
        457F92007ADFFF007BE1FF0070C6E200426975008CD3EA009DD9EC00BFE6F200
        CFECF5000099CC000B8AB50016A5D5001593BE001BA7D9001CA6D5001DA8DA00
        21AADC0022A9D90024ABDD0026A9D6002CADDD002176930047B9DF0050BBE200
        51B9DC005CCDF40064D1F9005CBFE2005DBEDF0060BFDF0073D9FF0066C2E200
        76DBFF0077DDFF00386473007FCCE700B2EBFF00AFDFEF0055656B00D4EFF900
        DFF2F9002DAFE20030B1E30038B5E8003DB8EA0059C6F40062CEF9006DD3FE00
        70D6FF006BCAF200A2E4FF004E5D63004ABEF00052C1F30062C6F20066C8F200
        6A737700E2F4FC005AC6F90063CBFE0066CCFF0080D4FC0086D6FE008FD9FE00
        99DEFF00747474006C6C6C006B6B6B006767670064646400626262005E5E5E00
        FFFFFF0002020202020202020202020202020202020202020202020202020202
        0202020202020202020202020202020202020202020202020202020202020202
        0202020202020202020202020202020202020202020202020202020202020202
        020202020202FBFA020202020202020202020202020202020202020202020202
        0202020202FBCEA3B1BBFBFA0202020202020202020202020202020202020202
        0202020202CEDCF5D0E3A79FB1BBFBFA02020202020202020202020202020202
        0202020202CEDADBD5D5D5D5D1EBA79FB1BBFBFB020202020202020202020202
        0202020202CECE72B8B8B8B8B8B8B8B894ACA7AAF80202020202020202020202
        0202020202CEE4BF848A8A8A8A8A8A8A8A8A8AABEA0202020202020202020202
        0202020202CEF3DA7F7D7D7D7D7D7D7D7C7D7D8FB7F902020202020202020202
        0202020202CEE8CE677373737373737373ACCAA8A1EAFDF8F802020202020202
        0202020202CEE6EBDF5E5E5E5E5E5E5E5EACBAF5E5ABA79FB1BBFBF802020202
        0202020202CEF4F3BD67645F5F5F5F5F5F94CDDBB6B6B6B694ACA79FB1BBF902
        0202020202CE9DD5ACBCBCBE728E656564CEE3DC8F8F8F8F8F8F8F8FB5ACA3F8
        0202020202CE9D99998F8FB5ACD2726868CEEDBC84848484848484978499CEEA
        0202020202CE7D8282827C58886CD6BDBECEE8CE8D7D7D7D7D7D9C907C8BCEB1
        F802020202CE5E5E5E5E77424A5D5E5E81CEF3CB715E5E5E5E6C3D10BD8283A9
        FD02020202CE5F6D6D544545454E5D5E5ECEF4ECBC605E5F733D2F332B6C8DCE
        D9F8020202CE665E774747474747577865CEF5E7A6BABDE98030333836089168
        CEFD02020202CECE4E4F4F4F4F4F4F5677CE9D999999BA0D77BD30333935088E
        CEEAF8020202020202024F505056020202CE7D827D7E3E141577BADADCBF2C0F
        DFCEF8020202020202024E515143020202CE5E5E5E46171710131210CECECECE
        CECE02020202020202024F525243020202CE626D4614221C14141410142C2207
        06020202020202020202024F5453430202CE8506342226311A121515151F2406
        0202020202020202020202024F5443020202CECE06252D2D311815121B270602
        020202020202020202020202024F4E4343020202020629292922161231060202
        02020202020202020202020202020202020202020202060101291F2206020202
        020202020202020202020202020202020202020202020206272E270602020202
        0202020202020202020202020202020202020202020202020606060202020202
        0202020202020202020202020202020202020202020202020202020202020202
        0202020202020202020202020202020202020202020202020202020202020202
        0202020202020202020202020202020202020202020202020202020202020202
        02020202FFFFFFFFFFFFFFFFFFFFFFFFCFFFFFFF80FFFFFF800FFFFF8000FFFF
        80007FFF80007FFF80003FFF800007FF800000FF8000001F8000000F8000000F
        80000007800000078000000380000003C0000001FC380001FC380003FC380007
        FE18000FFF1C001FFF87803FFFFFC07FFFFFE0FFFFFFF1FFFFFFFFFFFFFFFFFF
        FFFFFFFF}
    end
  end
  object ImageList: TImageList
    Left = 312
    Top = 8
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000FCFBFA06A2856BDB926C4EFFF4F0EE170000000000000000000000000000
      0000F1F4F611D2DCE43500000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C1A288A7C5AF9CFFAC8D76FF947356EC000000000000000000000000E2EC
      F2283A7295F81E628AFFEEF0F215000000000000000000000000000000000000
      00000000000000000000000000004040A0004040A00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BC9374C6D6C5B8FFC4AC99FF906E51ED000000000000000000000000528F
      B1E137C2F0FF1A8EBEFFE0E4E72A000000000000000000000000000000008080
      C0000000000000000000000000000000A0000000A00000000000000000000000
      00000000A0008080C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BD9576C4E8D9CFFF937152F8CCBCAE6C0000000000000000C5DEEC50327F
      A5FF27B6E7FE3C7290FDD0DAE0450000000000000000000000008080FF000000
      C0004040C00000000000000000000000A0000000A00000000000000000004040
      A0000000A0000000A00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BF9778C4E7DAD1FFD4BEAFFF966945FF00000000000000004A90B4F22FC4
      F6FF307197ECE3E3E62B00000000000000000000000000000000000000004040
      C0000000C0004040C000000000000000A0000000A000000000004040A0000000
      A0004040A0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C19A7BC0E8D5C6FFB28E75EFCCBDB26900000000ABCDE172317FA5FF19B8
      ECFF32799DFFF0F3F41500000000000000000000000000000000000000000000
      00004040C0000000C0004040C0000000A0000000A0000000A0000000A0004040
      A000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BE9373CCD1BAAAFF7A6F74FF558CAFD9387FA9EB267CA7FF37BBE8FF668A
      A2C6E1E3E7290000000000000000000000000000000000000000000000000000
      0000000000004040C0000000C0000000A0000000A0000000A0000000A0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E9E1
      DB45BF8E6DFF4D7597FF7BCFF4FF5DEBFFFF39D1FFFF22BEFAFF02AAE8FF759C
      B7AE00000000000000000000000000000000000000008080FF000000C0000000
      C0000000C0000000C0000000C0000000C0000000A0000000A0000000A0000000
      A0000000A0000000A0008080C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F4F2EF1CCA9B
      7CFF9596A0FF86C1E5FF7BF1FFFF69E8FFFF63E1FFFF30D1FFFF04ABF2FF217F
      ACFFF4F6F90F000000000000000000000000000000008080FF000000C0000000
      C0000000C0000000C0000000C0000000C0000000C0000000A0000000A0000000
      A0000000A0000000A0008080C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C3A68DBDF1CA
      B7FF93A4B7FFA4EEFFFF81EEFFFF437D8BFF938C7FFF52BFE0FF22A4D7FF117A
      AAFFD7E5F02F0000000000000000000000000000000000000000000000000000
      0000000000004040C0000000C0000000C0000000C0000000C0004040C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BD9D83E8F8D9
      CDFF88A6BCFFAFF5FFFF5CBDD3FF57392AFF904223FFBF906EFFC19371FFBC8F
      6EFFDCD1C8560000000000000000000000000000000000000000000000000000
      00004040FF000000C0004040C0000000C0000000C0004040C0000000C0004040
      C000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A48DE8F3DD
      D2FFBB9F8CFF76D1DDFF93F1FFFF66B6C8FF4296AFFF4FE3FFFF1EC2F6FFC3A7
      95FFBA8864FDE8D8CC4F00000000000000000000000000000000000000004040
      FF000000C0004040FF00000000000000C0000000C000000000004040C0000000
      C0004040C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CCB09BB4F5E9
      E3FFEBCCBCFFB1A89AFF71E6FFFF6BF1FFFF43E3FFFF23D6FFFF2193CBDEECED
      EE1AD2B29EE9CFB49FA2000000000000000000000000000000008080FF000000
      FF004040FF0000000000000000000000C0000000C00000000000000000004040
      C0000000C0008080C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFD03CBB1
      9BC4F4EEE9FFFFF2EBFFD5D4D4FF61B8CDFF43AFD1CAA3C4D66700000000F7F6
      F60DB6825BFFE2DAD15000000000000000000000000000000000000000008080
      FF000000000000000000000000000000C0000000C00000000000000000000000
      00008080FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6BEAB7CCDB39EAEC7A086D5D59376FFC2A28EC7C4B2A592C0977BCAB581
      5CFFBFAB9A9B0000000000000000000000000000000000000000000000000000
      00000000000000000000000000004040FF004040FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000F7F5F316D4BFB585D7B8A7DDCFAA90FBBF9272FEC7AE
      9CB0F7F4F2130000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00F0F3FFFF00000000F0E1FE7F00000000
      F0E1EE7300000000F0C1C66300000000F0C3E24700000000F083F00F00000000
      F007F81F00000000E00F800100000000C007800100000000C007F81F00000000
      C007F00F00000000C003E24700000000C003C66300000000C023EE7700000000
      F007FE7F00000000FC07FFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object FieldsPopupMenu: TPopupMenu
    Left = 384
    Top = 8
    object itemSetKeyField: TMenuItem
      Action = SetKeyField
    end
    object divFields: TMenuItem
      Caption = '-'
    end
    object itemSetImportProp: TMenuItem
      Action = SetImportProp
      Default = True
    end
  end
  object LogPopupMenu: TPopupMenu
    Left = 420
    Top = 8
    object itemSaveLog: TMenuItem
      Action = SaveLog
    end
  end
  object OpenDialog: TOpenDialog
    Filter = #1060#1072#1081#1083#1099' Microsoft Excel (*.xls*)|*.xls*|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 276
    Top = 8
  end
  object ActionList: TActionList
    Left = 348
    Top = 8
    object SetKeyField: TAction
      Caption = #1050#1083#1102#1095#1077#1074#1086#1077' '#1087#1086#1083#1077
      Hint = 
        #1050#1083#1102#1095#1077#1074#1086#1077' '#1087#1086#1083#1077' - '#1087#1086#1083#1077' '#1087#1086' '#1082#1086#1090#1086#1088#1086#1084#1091' '#1086#1087#1088#1077#1076#1077#1083#1103#1077#1090#1089#1103' '#1085#1077#1086#1073#1093#1086#1076#1080#1084#1086#1089#1090#1100' '#1080#1084#1087#1086 +
        #1088#1090#1072' '#1079#1072#1087#1080#1089#1080
      ShortCut = 16459
      OnExecute = SetKeyFieldExecute
      OnUpdate = SetKeyFieldUpdate
    end
    object SetImportProp: TAction
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100
      Hint = #1042#1099#1073#1086#1088' '#1080#1084#1087#1086#1088#1090#1080#1088#1091#1077#1084#1086#1075#1086' '#1087#1086#1083#1103' '#1080#1083#1080' '#1082#1086#1085#1089#1090#1072#1085#1090#1099
      ShortCut = 16397
      OnExecute = SetImportPropExecute
      OnUpdate = SetImportPropUpdate
    end
    object SaveLog: TAction
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1092#1072#1081#1083#1077
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1087#1088#1086#1090#1086#1082#1086#1083' '#1080#1084#1087#1086#1088#1090#1072' '#1074' '#1090#1077#1082#1090#1086#1074#1086#1084' '#1092#1072#1081#1083#1077
      OnExecute = SaveLogExecute
      OnUpdate = SaveLogUpdate
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.txt'
    Filter = #1058#1077#1082#1089#1090#1086#1074#1099#1077' '#1092#1072#1081#1083#1099' (*.txt)|*.txt|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 244
    Top = 8
  end
end
