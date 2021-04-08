inherited FormUsersGroup: TFormUsersGroup
  Left = 402
  Top = 262
  HelpKeyword = 'IDH_USERSGROUP'
  ActiveControl = NAMEDBEdit
  Caption = #1043#1088#1091#1087#1087#1072' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077#1081
  ClientHeight = 247
  ClientWidth = 474
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 196
    Width = 474
  end
  object IDDBEditLabel: TLabel [1]
    Left = 12
    Top = 12
    Width = 81
    Height = 13
    AutoSize = False
    Caption = #1050#1086#1076' (ID):'
    FocusControl = IDDBEdit
  end
  object NAMEDBEditLabel: TLabel [2]
    Left = 12
    Top = 60
    Width = 86
    Height = 13
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077': *'
    FocusControl = NAMEDBEdit
  end
  object NOTESDBMemoLabel: TLabel [3]
    Left = 12
    Top = 108
    Width = 142
    Height = 13
    Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103':'
    FocusControl = NOTESDBMemo
  end
  object OWNER_IDDBEditLabel: TLabel [4]
    Left = 104
    Top = 12
    Width = 81
    Height = 13
    AutoSize = False
    Caption = #1042#1083#1072#1076#1077#1083#1077#1094':'
    FocusControl = OWNER_IDDBEdit
  end
  inherited ButtonsPanel: TPanel
    Top = 198
    Width = 474
    TabOrder = 4
    inherited ButtonsMovedPanel: TPanel
      Left = 252
    end
  end
  object IDDBEdit: TDBEdit [6]
    Left = 12
    Top = 28
    Width = 81
    Height = 21
    DataField = 'ID'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object NAMEDBEdit: TDBEdit [7]
    Left = 12
    Top = 76
    Width = 449
    Height = 21
    DataField = 'NAME'
    DataSource = DataSource
    TabOrder = 2
  end
  object NOTESDBMemo: TDBMemo [8]
    Left = 12
    Top = 124
    Width = 450
    Height = 53
    DataField = 'NOTES'
    DataSource = DataSource
    TabOrder = 3
    WantReturns = False
  end
  object OWNER_IDDBEdit: TDBEdit [9]
    Left = 104
    Top = 28
    Width = 81
    Height = 21
    DataField = 'OWNER_ID'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
  inherited DataSource: TDataSource
    DataSet = FormUsers.SU_GROUPS
  end
end
