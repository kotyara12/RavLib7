object FormScrViewer: TFormScrViewer
  Left = 371
  Top = 312
  Width = 519
  Height = 468
  Caption = #1057#1082#1088#1080#1087#1090
  Color = clBtnFace
  Constraints.MinHeight = 195
  Constraints.MinWidth = 450
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010040000000000280100001600000028000000100000002000
    0000010004000000000080000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000B00000000000
    00008B0000000000000078B0000000000000078B0000000000000008B0000000
    00078BBBBB000000000078B0000000000000078B0000000000000780B0000000
    000000780B0000000000000780B000000000000000000000000000000000FFFF
    0000FFFF0000833F0000FF1F0000870F0000FF07000083830000FE0100008600
    0000FF07000083830000FF91000083C80000FFE40000FFFF0000FFFF0000}
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressPanel: TPanel
    Left = 0
    Top = 0
    Width = 511
    Height = 171
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 0
    OnResize = CloseBtnClick
    object TransactionPanel: TPanel
      Left = 8
      Top = 71
      Width = 495
      Height = 92
      Align = alClient
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 0
      object TransactionBottomBevel: TBevel
        Left = 2
        Top = 86
        Width = 491
        Height = 4
        Align = alBottom
        Shape = bsSpacer
      end
      object TransactionTopBevel: TBevel
        Left = 2
        Top = 2
        Width = 491
        Height = 2
        Align = alTop
        Shape = bsSpacer
      end
      object TransactionLeftBevel: TBevel
        Left = 2
        Top = 4
        Width = 4
        Height = 82
        Align = alLeft
        Shape = bsSpacer
      end
      object TransactionRightBevel: TBevel
        Left = 489
        Top = 4
        Width = 4
        Height = 82
        Align = alRight
        Shape = bsSpacer
      end
      object TransactionInnerPanel: TPanel
        Left = 6
        Top = 4
        Width = 483
        Height = 82
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object TransactionInfoBevel: TBevel
          Left = 0
          Top = 44
          Width = 483
          Height = 2
          Align = alBottom
          Shape = bsTopLine
        end
        object TransactionInfo: TLabel
          Left = 0
          Top = 0
          Width = 462
          Height = 44
          Align = alClient
          AutoSize = False
          WordWrap = True
        end
        object TransactionCompletePanel: TPanel
          Left = 0
          Top = 46
          Width = 483
          Height = 36
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          object CurrentFileBevel: TBevel
            Left = 450
            Top = 0
            Width = 4
            Height = 36
            Align = alRight
            Shape = bsSpacer
          end
          object CurrentFilePanel: TPanel
            Left = 454
            Top = 0
            Width = 29
            Height = 36
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 0
            object FileGauge: TGauge
              Left = 0
              Top = 7
              Width = 29
              Height = 29
              Align = alBottom
              BackColor = clBtnFace
              BorderStyle = bsNone
              Color = clBtnFace
              ForeColor = clGreen
              Kind = gkPie
              ParentColor = False
              Progress = 0
              Visible = False
            end
          end
          object TransactionProgressPanel: TPanel
            Left = 0
            Top = 0
            Width = 450
            Height = 36
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            object TransactionStatistic: TLabel
              Left = 0
              Top = 0
              Width = 450
              Height = 13
              Align = alTop
            end
            object TransactionProgressBevel: TBevel
              Left = 0
              Top = 13
              Width = 450
              Height = 2
              Align = alTop
              Shape = bsSpacer
            end
            object TransactionBar: TProgressBar
              Left = 0
              Top = 15
              Width = 450
              Height = 21
              Hint = #1048#1085#1076#1080#1082#1072#1090#1086#1088' '#1074#1099#1087#1086#1083#1085#1077#1085#1080#1103' '#1079#1072#1076#1072#1085#1080#1103
              Align = alClient
              TabOrder = 0
            end
          end
        end
        object StatusPanel: TPanel
          Left = 462
          Top = 0
          Width = 21
          Height = 44
          Align = alRight
          BevelOuter = bvNone
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          object GlobalShape: TShape
            Left = 0
            Top = 21
            Width = 21
            Height = 21
            Align = alTop
            Brush.Color = clBtnFace
            Pen.Color = clBtnFace
            Pen.Style = psInsideFrame
            Shape = stRoundRect
          end
          object ScriptShape: TShape
            Left = 0
            Top = 0
            Width = 21
            Height = 21
            Align = alTop
            Brush.Color = clBtnFace
            Pen.Color = clBtnFace
            Pen.Style = psInsideFrame
            Shape = stRoundRect
          end
        end
      end
    end
    object TopPanel: TPanel
      Left = 8
      Top = 8
      Width = 495
      Height = 63
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object OperationPanel: TPanel
        Left = 0
        Top = 0
        Width = 462
        Height = 63
        Align = alClient
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object OperationLeftBevel: TBevel
          Left = 2
          Top = 4
          Width = 4
          Height = 53
          Align = alLeft
          Shape = bsSpacer
        end
        object OpertionRightBevel: TBevel
          Left = 456
          Top = 4
          Width = 4
          Height = 53
          Align = alRight
          Shape = bsSpacer
        end
        object OperationBottomBevel: TBevel
          Left = 2
          Top = 57
          Width = 458
          Height = 4
          Align = alBottom
          Shape = bsSpacer
        end
        object OperationTopBevel: TBevel
          Left = 2
          Top = 2
          Width = 458
          Height = 2
          Align = alTop
          Shape = bsSpacer
        end
        object OperationInnerPanel: TPanel
          Left = 6
          Top = 4
          Width = 450
          Height = 53
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object OperationInfo: TLabel
            Left = 0
            Top = 0
            Width = 450
            Height = 15
            Align = alTop
            AutoSize = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object OperationStatistic: TLabel
            Left = 0
            Top = 17
            Width = 450
            Height = 13
            Align = alTop
          end
          object OperationProgressBevel: TBevel
            Left = 0
            Top = 30
            Width = 450
            Height = 2
            Align = alTop
            Shape = bsSpacer
          end
          object OperationInfoBevel: TBevel
            Left = 0
            Top = 15
            Width = 450
            Height = 2
            Align = alTop
            Shape = bsTopLine
          end
          object OperationBar: TProgressBar
            Left = 0
            Top = 32
            Width = 450
            Height = 21
            Hint = #1048#1085#1076#1080#1082#1072#1090#1086#1088' '#1074#1099#1087#1086#1083#1085#1077#1085#1080#1103' '#1079#1072#1076#1072#1085#1080#1103
            Align = alClient
            TabOrder = 0
          end
        end
      end
      object ButtonsPanel: TPanel
        Left = 462
        Top = 0
        Width = 33
        Height = 63
        Align = alRight
        Anchors = [akTop, akRight]
        BevelInner = bvRaised
        BevelOuter = bvLowered
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        object LogOpenBtn: TBitBtn
          Left = 4
          Top = 32
          Width = 25
          Height = 25
          Hint = #1055#1086#1082#1072#1079#1072#1090#1100' '#1078#1091#1088#1085#1072#1083
          TabOrder = 1
          OnClick = LogOpenBtnClick
          Glyph.Data = {
            36080000424D3608000000000000360000002800000020000000100000000100
            2000000000000008000000000000000000000000000000000000FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF0099663300FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF005B5B5B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00999966009999
            6600999966009999660099996600999966009999660099996600999966009999
            660099663300CC66330099333300FFFFFF00FFFFFF00FFFFFF00888888008888
            8800888888008888880088888800888888008888880088888800888888008888
            88005B5B5B00606060003E3E3E00FFFFFF00FFFFFF00FFFFFF0099666600FFCC
            CC00FFCCCC00FFCCCC00FFCCCC00FFCCCC00FFCCCC00FFCCCC00FFCCCC009966
            3300FF996600CC666600CC66330099333300FFFFFF00FFFFFF006C6C6C00D2D2
            D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D2005B5B
            5B009393930071717100606060003E3E3E00FFFFFF00FFFFFF0099996600F1F1
            F100F1F1F100EAEAEA00EAEAEA00EAEAEA00E3E3E300DDDDDD00CC663300FF99
            6600FF996600FF996600CC663300CC66330099333300FFFFFF0088888800F1F1
            F100F1F1F100EAEAEA00EAEAEA00EAEAEA00E3E3E300DDDDDD00606060009393
            9300939393009393930060606000606060003E3E3E00FFFFFF0099996600F1F1
            F100F1F1F100F1F1F100EAEAEA00EAEAEA00DDDDDD00FF996600FF996600CC99
            6600FF999900FF996600CC99660099663300996633009933330088888800F1F1
            F100F1F1F100F1F1F100EAEAEA00EAEAEA00DDDDDD0093939300939393008E8E
            8E00A4A4A400939393008E8E8E005B5B5B005B5B5B003E3E3E0099996600F1F1
            F100F1F1F100F1F1F100F1F1F100EAEAEA00EAEAEA00EAEAEA00DDDDDD00CC99
            6600FF999900FF996600FF99660099663300FFFFFF00FFFFFF0088888800F1F1
            F100F1F1F100F1F1F100F1F1F100EAEAEA00EAEAEA00EAEAEA00DDDDDD008E8E
            8E00A4A4A40093939300939393005B5B5B00FFFFFF00FFFFFF00CC996600F8F8
            F800F1F1F100F1F1F100F1F1F100F1F1F100EAEAEA00EAEAEA00DDDDDD00CC99
            6600F0CAA600FF996600FF99660099333300FFFFFF00FFFFFF008E8E8E00F8F8
            F800F1F1F100F1F1F100F1F1F100F1F1F100EAEAEA00EAEAEA00DDDDDD008E8E
            8E00C2C2C20093939300939393003E3E3E00FFFFFF00FFFFFF00CC999900FFFF
            FF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1F100EAEAEA00DDDDDD00CC99
            6600F0CAA600FFCC9900FF99990099333300FFFFFF00FFFFFF009F9F9F00FFFF
            FF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1F100EAEAEA00DDDDDD008E8E
            8E00C2C2C200C1C1C100A4A4A4003E3E3E00FFFFFF00FFFFFF00CC999900FFFF
            FF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1F100DDDDDD00FF99
            6600CC996600CC996600CC996600CC996600FFFFFF00FFFFFF009F9F9F00FFFF
            FF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1F100DDDDDD009393
            93008E8E8E008E8E8E008E8E8E008E8E8E00FFFFFF00FFFFFF00CC999900FFFF
            FF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1F100EAEA
            EA00FFECCC0099996600FFFFFF00FFFFFF00FFFFFF00FFFFFF009F9F9F00FFFF
            FF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1F100EAEA
            EA00E4E4E40088888800FFFFFF00FFFFFF00FFFFFF00FFFFFF00CC999900FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1
            F100FFECCC0099996600FFFFFF00FFFFFF00FFFFFF00FFFFFF009F9F9F00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1
            F100E4E4E40088888800FFFFFF00FFFFFF00FFFFFF00FFFFFF00CC999900FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1
            F100FFECCC0099996600FFFFFF00FFFFFF00FFFFFF00FFFFFF009F9F9F00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1
            F100E4E4E40088888800FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1
            F100FFECCC0099996600FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1
            F100E4E4E40088888800FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1
            F100FFECCC0099996600FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1
            F100E4E4E40088888800FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8
            F800FFECCC0099996600FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8
            F800E4E4E40088888800FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000CC99
            9900CC999900CC999900CC999900CC999900CC999900CC999900CC999900CC99
            9900CC999900CC999900FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0009F9F
            9F009F9F9F009F9F9F009F9F9F009F9F9F009F9F9F009F9F9F009F9F9F009F9F
            9F009F9F9F009F9F9F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
          NumGlyphs = 2
        end
        object CloseBtn: TBitBtn
          Left = 4
          Top = 4
          Width = 25
          Height = 25
          Hint = #1047#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086
          TabOrder = 3
          Visible = False
          OnClick = CloseBtnClick
          Glyph.Data = {
            36060000424D3606000000000000360400002800000020000000100000000100
            08000000000000020000610B0000610B00000001000000000000000000003300
            00006600000099000000CC000000FF0000000033000033330000663300009933
            0000CC330000FF33000000660000336600006666000099660000CC660000FF66
            000000990000339900006699000099990000CC990000FF99000000CC000033CC
            000066CC000099CC0000CCCC0000FFCC000000FF000033FF000066FF000099FF
            0000CCFF0000FFFF000000003300330033006600330099003300CC003300FF00
            330000333300333333006633330099333300CC333300FF333300006633003366
            33006666330099663300CC663300FF6633000099330033993300669933009999
            3300CC993300FF99330000CC330033CC330066CC330099CC3300CCCC3300FFCC
            330000FF330033FF330066FF330099FF3300CCFF3300FFFF3300000066003300
            66006600660099006600CC006600FF0066000033660033336600663366009933
            6600CC336600FF33660000666600336666006666660099666600CC666600FF66
            660000996600339966006699660099996600CC996600FF99660000CC660033CC
            660066CC660099CC6600CCCC6600FFCC660000FF660033FF660066FF660099FF
            6600CCFF6600FFFF660000009900330099006600990099009900CC009900FF00
            990000339900333399006633990099339900CC339900FF339900006699003366
            99006666990099669900CC669900FF6699000099990033999900669999009999
            9900CC999900FF99990000CC990033CC990066CC990099CC9900CCCC9900FFCC
            990000FF990033FF990066FF990099FF9900CCFF9900FFFF99000000CC003300
            CC006600CC009900CC00CC00CC00FF00CC000033CC003333CC006633CC009933
            CC00CC33CC00FF33CC000066CC003366CC006666CC009966CC00CC66CC00FF66
            CC000099CC003399CC006699CC009999CC00CC99CC00FF99CC0000CCCC0033CC
            CC0066CCCC0099CCCC00CCCCCC00FFCCCC0000FFCC0033FFCC0066FFCC0099FF
            CC00CCFFCC00FFFFCC000000FF003300FF006600FF009900FF00CC00FF00FF00
            FF000033FF003333FF006633FF009933FF00CC33FF00FF33FF000066FF003366
            FF006666FF009966FF00CC66FF00FF66FF000099FF003399FF006699FF009999
            FF00CC99FF00FF99FF0000CCFF0033CCFF0066CCFF0099CCFF00CCCCFF00FFCC
            FF0000FFFF0033FFFF0066FFFF0099FFFF00CCFFFF00FFFFFF00000080000080
            000000808000800000008000800080800000C0C0C00080808000191919004C4C
            4C00B2B2B200E5E5E5005A1E1E00783C3C0096646400C8969600FFC8C800465F
            82005591B9006EB9D7008CD2E600B4E6F000D8E9EC0099A8AC00646F7100E2EF
            F100C56A31000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000EEEEEEEEEEEE
            F1EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEF1EEEEEEEEEEEEEEEEEEEEEEEEF1E3AC
            E3F1EEEEEEEEEEEEEEEEEEEEEEF1EEACE3F1EEEEEEEEEEEEEEEEEEF1E3E28257
            57E2ACE3F1EEEEEEEEEEEEF1EEE2818181E2ACEEF1EEEEEEEEEEE382578282D7
            578181E2E3EEEEEEEEEEEE81818181D7818181E2EEEEEEEEEEEE57828989ADD7
            57797979F1EEEEEEEEEE8181DEDEACD781818181F1EEEEEEEEEE57898989ADD7
            57AAAAA2D7ADEEEEEEEE81DEDEDEACD781DEDE81D7ACEEEEEEEE57898989ADD7
            57AACEA3AD10EEEEEEEE81DEDEDEACD781DEAC81AC81EEEEEEEE5789825EADD7
            57ABCFE21110EEEEEEEE81DE8181ACD781ACACE28181EEEEEEEE578957D7ADD7
            57ABDE101010101010EE81DE56D7ACD781ACDE818181818181EE57898257ADD7
            57EE10101010101010EE81DE8156ACD781E381818181818181EE57898989ADD7
            57EE82101010101010EE81DEDEDEACD781E381818181818181EE57898989ADD7
            57ACF1821110EEEEEEEE81DEDEDEACD781ACF1818181EEEEEEEE57898989ADD7
            57ABEEAB8910EEEEEEEE81DEDEDEACD781ACE3ACDE81EEEEEEEE57828989ADD7
            57ACEEA3EE89EEEEEEEE8181DEDEACD781ACE381EEDEEEEEEEEEEEDE5E8288D7
            57A2A2A2EEEEEEEEEEEEEEDE8181DED781818181EEEEEEEEEEEEEEEEEEAC8257
            57EEEEEEEEEEEEEEEEEEEEEEEEAC818181EEEEEEEEEEEEEEEEEE}
          NumGlyphs = 2
        end
        object LogCloseBtn: TBitBtn
          Left = 4
          Top = 32
          Width = 25
          Height = 25
          Hint = #1057#1082#1088#1099#1090#1100' '#1078#1091#1088#1085#1072#1083
          TabOrder = 2
          Visible = False
          OnClick = LogCloseBtnClick
          Glyph.Data = {
            36080000424D3608000000000000360000002800000020000000100000000100
            2000000000000008000000000000000000000000000000000000FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CC99
            660099663300993333009933330099333300FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008E8E
            8E005B5B5B003E3E3E003E3E3E003E3E3E00FFFFFF00FFFFFF00999966009999
            660099996600999966009999660099996600999966009999660099996600CC99
            6600CC996600CC663300CC66330099333300FFFFFF00FFFFFF00888888008888
            8800888888008888880088888800888888008888880088888800888888008E8E
            8E008E8E8E0060606000606060003E3E3E00FFFFFF00FFFFFF0099666600FFCC
            CC00FFCCCC00FFCCCC00FFCCCC00FFCCCC00FFCCCC00FFCCCC00FFCCCC00CC99
            6600FF999900CC666600CC66330099333300FFFFFF00FFFFFF006C6C6C00D2D2
            D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D2008E8E
            8E00A4A4A40071717100606060003E3E3E00FFFFFF00FFFFFF0099996600F1F1
            F100F1F1F100EAEAEA00EAEAEA00EAEAEA00E3E3E300DDDDDD00DDDDDD00CC99
            6600FF996600CC996600CC66330099663300FFFFFF00FFFFFF0088888800F1F1
            F100F1F1F100EAEAEA00EAEAEA00EAEAEA00E3E3E300DDDDDD00DDDDDD008E8E
            8E00939393008E8E8E00606060005B5B5B00FFFFFF00FFFFFF0099996600F1F1
            F100F1F1F100F1F1F100EAEAEA00EAEAEA00EAEAEA00FF996600CC663300CC66
            3300FFCC9900FF996600FF99660099663300996633009933330088888800F1F1
            F100F1F1F100F1F1F100EAEAEA00EAEAEA00EAEAEA0093939300606060006060
            6000C1C1C10093939300939393005B5B5B005B5B5B003E3E3E0099996600F1F1
            F100F1F1F100F1F1F100F1F1F100EAEAEA00EAEAEA00DDDDDD00FF996600F0CA
            A600FFCC9900FF996600FF996600CC66330099333300FFFFFF0088888800F1F1
            F100F1F1F100F1F1F100F1F1F100EAEAEA00EAEAEA00DDDDDD0093939300C2C2
            C200C1C1C1009393930093939300606060003E3E3E00FFFFFF00CC996600F8F8
            F800F1F1F100F1F1F100F1F1F100F1F1F100EAEAEA00EAEAEA00DDDDDD00FF99
            6600F0CAA600FFCC9900CC99660099333300FFFFFF00FFFFFF008E8E8E00F8F8
            F800F1F1F100F1F1F100F1F1F100F1F1F100EAEAEA00EAEAEA00DDDDDD009393
            9300C2C2C200C1C1C1008E8E8E003E3E3E00FFFFFF00FFFFFF00CC999900FFFF
            FF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1F100EAEAEA00EAEAEA00DDDD
            DD00CC663300F0CAA60099333300FFFFFF00FFFFFF00FFFFFF009F9F9F00FFFF
            FF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1F100EAEAEA00EAEAEA00DDDD
            DD0060606000C2C2C2003E3E3E00FFFFFF00FFFFFF00FFFFFF00CC999900FFFF
            FF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1F100EAEAEA00EAEA
            EA00FFECCC00CC996600FFFFFF00FFFFFF00FFFFFF00FFFFFF009F9F9F00FFFF
            FF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1F100EAEAEA00EAEA
            EA00E4E4E4008E8E8E00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CC999900FFFF
            FF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1F100EAEA
            EA00FFECCC00CC666600FFFFFF00FFFFFF00FFFFFF00FFFFFF009F9F9F00FFFF
            FF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1F100EAEA
            EA00E4E4E40071717100FFFFFF00FFFFFF00FFFFFF00FFFFFF00CC999900FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1
            F100FFECCC0099996600FFFFFF00FFFFFF00FFFFFF00FFFFFF009F9F9F00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1F100F1F1
            F100E4E4E40088888800FFFFFF00FFFFFF00FFFFFF00FFFFFF00CC999900FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1
            F100FFECCC0099996600FFFFFF00FFFFFF00FFFFFF00FFFFFF009F9F9F00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1F100F1F1
            F100E4E4E40088888800FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1
            F100FFECCC0099996600FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1F100F1F1
            F100E4E4E40088888800FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1
            F100FFECCC0099996600FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8F800F1F1
            F100E4E4E40088888800FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8
            F800FFECCC0099996600FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFF
            FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F8F8
            F800E4E4E40088888800FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000CC99
            9900CC999900CC999900CC999900CC999900CC999900CC999900CC999900CC99
            9900CC999900CC999900FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0009F9F
            9F009F9F9F009F9F9F009F9F9F009F9F9F009F9F9F009F9F9F009F9F9F009F9F
            9F009F9F9F009F9F9F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
          NumGlyphs = 2
        end
        object StopBtn: TBitBtn
          Left = 4
          Top = 4
          Width = 25
          Height = 25
          Hint = #1054#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1074#1099#1087#1086#1083#1085#1077#1085#1080#1077' '#1089#1082#1088#1080#1087#1090#1072
          TabOrder = 0
          Glyph.Data = {
            36080000424D3608000000000000360000002800000020000000100000000100
            2000000000000008000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006B72D1003145D4001A37DC001A37DC003044
            D4006970D100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF0091919100727272006A6A6A006A6A6A007171
            71008F8F8F00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF006E73CF000427E1000032FC000035FF000035FF000035FF000034
            FF000032FD000327E3006A70CE00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF0091919100616161006F6F6F007272720072727200727272007171
            71006F6F6F00616161008E8E8E00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF004C55C900002CF2000032FF000031FF000032FD000032FD000032FD000032
            FD000031FE000032FF00002DF3004751C800FF00FF00FF00FF00FF00FF00FF00
            FF007A7A7A006868680070707000707070006F6F6F006F6F6F006F6F6F006F6F
            6F006F6F6F00707070006969690077777700FF00FF00FF00FF00FF00FF006F73
            CD000029EC00002DF600001EE100001CD8000029F8000034FF000034FF00002A
            F900001CD900001DDF00002DF6000029EC00696DCC00FF00FF00FF00FF009090
            9000656565006A6A6A005B5B5B00575757006969690071717100717171006A6A
            6A00575757005A5A5A006A6A6A00656565008C8C8C00FF00FF00FF00FF00041F
            D200002AF000011BDB007F7ECD00BDB8DC002C38C7000020F2000023F5002431
            C800BAB5DC008B89CF00021BD800002AF000031FD300FF00FF00FF00FF005757
            5700676767005858580098989800C4C4C4006666660062626200646464006161
            6100C2C2C200A0A0A000575757006767670057575700FF00FF006E72CD000024
            E1000026E8000215CD00C4C0E000FFFFFF00DFDBEC00252EC2001F26C100D8D4
            E900FFFFFF00D4CFE6000518CA000025E8000024E100696ECC00909090005E5E
            5E006262620050505000CBCBCB00FFFFFF00E1E1E1005E5E5E0058585800DBDB
            DB00FFFFFF00D7D7D70051515100616161005E5E5E008C8C8C00333FC4000021
            DD000023DF00001DE0002B32C000DDDBED00FFFFFF00DBD9ED00D6D4EA00FFFF
            FF00E4E2F000353AC000001BDE000023E0000022DD002F3BC5006A6A6A005B5B
            5B005D5D5D005A5A5A0060606000E1E1E100FFFFFF00E0E0E000DBDBDB00FFFF
            FF00E7E7E70066666600585858005E5E5E005C5C5C00676767001B29C200001D
            D500001ED7000020DC000012D600232ABA00D7D5EC00FFFFFF00FFFFFF00DEDC
            EF002B31BB000010D3000021DC00001FD700001ED5001A29C2005A5A5A005757
            5700585858005B5B5B005151510059595900DDDDDD00FFFFFF00FFFFFF00E2E2
            E2005E5E5E004F4F4F005B5B5B0058585800575757005A5A5A001D2ABE00061F
            CF00051FD0000621D3000013CE001C23B700D4D3EB00FFFFFF00FFFFFF00DCDA
            EF00232AB8000010CC00041FD200031DCF00031CCE001A28BF00595959005656
            560057575700595959004F4F4F0053535300DBDBDB00FFFFFF00FFFFFF00E1E1
            E100585858004C4C4C00575757005555550054545400585858003940BE00192D
            CC00182CCC001024C900292FB800D7D6EB00FFFFFF00DFDEF100DBDAEF00FFFF
            FF00DEDDEF003035B9000C20C7001529CB001428CB00343BBD00696969005F5F
            5F005F5F5F00585858005C5C5C00DDDDDD00FFFFFF00E4E4E400E1E1E100FFFF
            FF00E3E3E30060606000555555005C5C5C005C5C5C00656565007172C9002634
            C5002D3CCA001621B700BCBBE100FFFFFF00E0DFF1003439B6002C30B300D9D8
            EE00FFFFFF00CCCAE7001721B6002837C9002231C5006B6CC8008F8F8F006262
            62006969690051515100C8C8C800FFFFFF00E5E5E500626262005B5B5B00DFDF
            DF00FFFFFF00D4D4D4005151510066666600606060008A8A8A00FF00FF001F26
            B500444FCA002B32B9008585CE00BFBFE4004244B8002B35BF002D38C1003A3D
            B600BCBCE3009191D2002830B800404BCA001D25B600FF00FF00FF00FF005454
            5400767676005E5E5E009D9D9D00CBCBCB006A6A6A0061616100646464006565
            6500C9C9C900A6A6A6005C5C5C007474740054545400FF00FF00FF00FF007475
            C800383FBD00585EC9003A3EB9003336B4004349C0005056C6004F56C5004349
            C1003235B400373BB800535AC800383EBE006E6FC600FF00FF00FF00FF009090
            90006868680081818100666666005F5F5F00707070007A7A7A007A7A7A007070
            70005F5F5F00646464007E7E7E00686868008C8C8C00FF00FF00FF00FF00FF00
            FF005556BC004347BC006B6ECB00696BC9006366C6006163C4006163C4006264
            C600676AC800696CCA004346BC005152BB00FF00FF00FF00FF00FF00FF00FF00
            FF00787878006D6D6D008C8C8C008A8A8A008585850083838300838383008484
            8400898989008B8B8B006D6D6D0075757500FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF007474C7003132AF005D5EC0007373C8007878C9007878C9007272
            C8005D5DC0003132AE007070C500FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF008F8F8F005B5B5B007E7E7E008F8F8F0093939300939393008E8E
            8E007E7E7E005B5B5B008C8C8C00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF007272C6004747B4003D3DB0003C3CB0004646
            B4007070C500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF008E8E8E006B6B6B0063636300626262006A6A
            6A008C8C8C00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
          NumGlyphs = 2
        end
      end
    end
  end
  object LogView: TRichEdit
    Left = 0
    Top = 171
    Width = 511
    Height = 263
    Align = alClient
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
end