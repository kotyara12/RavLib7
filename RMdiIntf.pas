unit RMdiIntf;

interface

uses
  Db, RVclUtils, RavTreeView, RDbTree, TmplData, TmplDbSelect;

procedure ShowMdiForm(FormClass: TDataFormClass; const ATag: Integer;
  const FindPreviousWins: Boolean; const Data: Pointer = nil);
procedure ShowMdiNamedForm(FormClass: TDataFormClass; const ATag: Integer;
  const FormCaption: string; const FindPreviousWins: Boolean; const Data: Pointer = nil);
function  ShowModalForm(FormClass: TDataFormClass; const ATag: Integer;
  const Data: Pointer = nil): Boolean;
function  ShowModalNamedForm(FormClass: TDataFormClass; const ATag: Integer;
  const FormCaption: string; const Data: Pointer = nil): Boolean;
function  ShowModalTreeDetailForm(FormClass: TDataFormClass; const ATag: Integer;
  const DefNodeType: TNodeType; DefNodeId: Integer; const Data: Pointer = nil): Boolean;
function  ShowModalTreeDetailNamedForm(FormClass: TDataFormClass; const ATag: Integer;
  const FormCaption: string; const DefNodeType: TNodeType; DefNodeId: Integer; const Data: Pointer = nil): Boolean;
function  ShowSelectForm(FormClass: TDataFormClass; const ATag: Integer; var Value: Integer;
  const Data: Pointer = nil): Boolean;
function  ShowSelectNamedForm(FormClass: TDataFormClass; const ATag: Integer; var Value: Integer;
  const FormCaption: string; const Data: Pointer = nil): Boolean;
function  ShowGroupForm(FormClass: TDataFormClass; const ATag: Integer; var GroupValue, ItemValue: Integer;
  const Data: Pointer = nil): Boolean;
function  ShowTreeForm(FormClass: TDataFormClass; const ATag: Integer;
  const LoadMode: TTreeLoadMode; SelectedTypes: TNodeTypes; var GroupValue, ItemValue: Integer;
  const Data: Pointer = nil): Boolean;
function  ShowTreeGroupForm(FormClass: TDataFormClass; const ATag: Integer; var Value: Integer;
  const Data: Pointer = nil): Boolean;
function  ShowTreeItemForm(FormClass: TDataFormClass; const ATag: Integer; var Value: Integer;
  const Data: Pointer = nil): Boolean;
function  ShowFieldForm(FormClass: TDataFormClass; const ATag: Integer; Field: TField; const Data: Pointer = nil;
  const LookupDS1: TDataSet = nil; const LookupDS2: TDataSet = nil; const LookupDS3: TDataSet = nil): Boolean;
function  ShowTreeGroupFieldForm(FormClass: TDataFormClass; const ATag: Integer; Field: TField; const Data: Pointer = nil;
  const LookupDS1: TDataSet = nil; const LookupDS2: TDataSet = nil; const LookupDS3: TDataSet = nil): Boolean;
function  ShowTreeFieldForm(FormClass: TDataFormClass; const ATag: Integer;
  const LoadMode: TTreeLoadMode; SelectedTypes: TNodeTypes;
  GroupsField, ItemsField: TField; const Data: Pointer = nil;
  const LookupDS1: TDataSet = nil; const LookupDS2: TDataSet = nil; const LookupDS3: TDataSet = nil): Boolean;
function  DbSelectDialog(FormClass: TDbSelectClass; var AGroupKey, AItemKey: Integer;
  const Param0, Param1: Integer): Boolean;

implementation

uses
  SysUtils, Controls, Forms, RDialogs, RDbOpenDS,
  TmplTreeDetail, TmplDbTree;

resourcestring
  ECannotCreateChildForm = 'Ошибка создания формы %s! Главная форма не является управляющей MDI формой.';
  EWindowAlreadyOpen     = 'Запрашиваемая форма уже создана. Загрузка данных, подождите...';

procedure ShowMdiForm(FormClass: TDataFormClass; const ATag: Integer;
  const FindPreviousWins: Boolean; const Data: Pointer = nil);
var
  i: Integer;
  bFound: Boolean;
begin
  if not IsNotWait then
  begin
    WarningBox(EWindowAlreadyOpen);
    Exit;
  end;
  if Assigned(Application.MainForm)
  and (Application.MainForm.FormStyle = fsMdiForm) then
  begin
    bFound := False;
    if FindPreviousWins then
    begin
      for i := 0 to Application.MainForm.MDIChildCount - 1 do
        if Application.MainForm.MDIChildren[i] is FormClass then
        begin
          bFound := True;
          Application.MainForm.MDIChildren[i].BringToFront;
          Break;
        end;
    end;
    if not bFound then
    begin
      with FormClass.Create(Application.MainForm) do
      begin
        try
          Application.ProcessMessages;
          FormData := Data;
          Tag := ATag;
          FormStyle := fsMdiChild;
          Application.ProcessMessages;
          if not LoadData then Close;
        except
          Free;
          raise;
        end;
      end;
    end;
  end
  else raise Exception.CreateFmt(ECannotCreateChildForm, [FormClass.ClassName]);
end;

procedure ShowMdiNamedForm(FormClass: TDataFormClass; const ATag: Integer;
  const FormCaption: string; const FindPreviousWins: Boolean; const Data: Pointer = nil);
var
  i: Integer;
  bFound: Boolean;
begin
  if not IsNotWait then
  begin
    WarningBox(EWindowAlreadyOpen);
    Exit;
  end;
  if Assigned(Application.MainForm)
  and (Application.MainForm.FormStyle = fsMdiForm) then
  begin
    bFound := False;
    if FindPreviousWins then
    begin
      for i := 0 to Application.MainForm.MDIChildCount - 1 do
        if Application.MainForm.MDIChildren[i] is FormClass then
        begin
          bFound := True;
          Application.MainForm.MDIChildren[i].BringToFront;
          Break;
        end;
    end;
    if not bFound then
    begin
      with FormClass.Create(Application.MainForm) do
      begin
        try
          Application.ProcessMessages;
          Caption := FormCaption;
          FormData := Data;
          Tag := ATag;
          FormStyle := fsMdiChild;
          Application.ProcessMessages;
          if not LoadData then Close;
        except
          Free;
          raise;
        end;
      end;
    end;
  end
  else raise Exception.CreateFmt(ECannotCreateChildForm, [FormClass.ClassName]);
end;

function ShowModalForm(FormClass: TDataFormClass; const ATag: Integer;
  const Data: Pointer = nil): Boolean;
begin
  Result := False;
  if not IsNotWait then
  begin
    WarningBox(EWindowAlreadyOpen);
    Exit;
  end;
  if Assigned(Application.MainForm) then
  begin
    with FormClass.Create(Application.MainForm) do
    begin
      try
        Application.ProcessMessages;
        FormData := Data;
        Tag := ATag;
        FormStyle := fsNormal;
        SelectMode := False;
        Application.ProcessMessages;
        Result := LoadData and (ShowModal > intDisable);
      finally
        Free;
      end;
    end;
  end
  else raise Exception.CreateFmt(ECannotCreateChildForm, [FormClass.ClassName]);
end;

function ShowModalNamedForm(FormClass: TDataFormClass; const ATag: Integer;
  const FormCaption: string; const Data: Pointer = nil): Boolean;
begin
  Result := False;
  if not IsNotWait then
  begin
    WarningBox(EWindowAlreadyOpen);
    Exit;
  end;
  if Assigned(Application.MainForm) then
  begin
    with FormClass.Create(Application.MainForm) do
    begin
      try
        Application.ProcessMessages;
        Caption := FormCaption;
        FormData := Data;
        Tag := ATag;
        FormStyle := fsNormal;
        SelectMode := False;
        Application.ProcessMessages;
        Result := LoadData and (ShowModal > intDisable);
      finally
        Free;
      end;
    end;
  end
  else raise Exception.CreateFmt(ECannotCreateChildForm, [FormClass.ClassName]);
end;

function ShowModalTreeDetailForm(FormClass: TDataFormClass; const ATag: Integer;
  const DefNodeType: TNodeType; DefNodeId: Integer; const Data: Pointer = nil): Boolean;
var
  Form: TDataTemplate;
begin
  Result := False;
  if not IsNotWait then
  begin
    WarningBox(EWindowAlreadyOpen);
    Exit;
  end;
  if Assigned(Application.MainForm) then
  begin
    Form := FormClass.Create(Application.MainForm);
    try
      Application.ProcessMessages;
      Form.FormData := Data;
      Form.Tag := ATag;
      Form.FormStyle := fsNormal;
      Form.SelectMode := False;
      Application.ProcessMessages;
      if Form.LoadData then
      begin
        if Form is TTreeDetailTemplate then
          TTreeDetailTemplate(Form).GotoTreeNode([DefNodeType], DefNodeId);
        Result := Form.ShowModal > intDisable;
      end;
    finally
      Form.Free;
    end;
  end
  else raise Exception.CreateFmt(ECannotCreateChildForm, [FormClass.ClassName]);
end;

function ShowModalTreeDetailNamedForm(FormClass: TDataFormClass; const ATag: Integer;
  const FormCaption: string; const DefNodeType: TNodeType; DefNodeId: Integer; const Data: Pointer = nil): Boolean;
var
  Form: TDataTemplate;
begin
  Result := False;
  if not IsNotWait then
  begin
    WarningBox(EWindowAlreadyOpen);
    Exit;
  end;
  if Assigned(Application.MainForm) then
  begin
    Form := FormClass.Create(Application.MainForm);
    try
      Application.ProcessMessages;
      Form.FormData := Data;
      Form.Caption := FormCaption;
      Form.Tag := ATag;
      Form.FormStyle := fsNormal;
      Form.SelectMode := False;
      Application.ProcessMessages;
      if Form.LoadData then
      begin
        if Form is TTreeDetailTemplate then
          TTreeDetailTemplate(Form).GotoTreeNode([DefNodeType], DefNodeId);
        Result := Form.ShowModal > intDisable;
      end;
    finally
      Form.Free;
    end;
  end
  else raise Exception.CreateFmt(ECannotCreateChildForm, [FormClass.ClassName]);
end;

function ShowSelectForm(FormClass: TDataFormClass; const ATag: Integer; var Value: Integer;
  const Data: Pointer = nil): Boolean;
begin
  Result := False;
  if not IsNotWait then
  begin
    WarningBox(EWindowAlreadyOpen);
    Exit;
  end;
  if Assigned(Application.MainForm) then
  begin
    with FormClass.Create(Application.MainForm) do
    begin
      try
        Application.ProcessMessages;
        FormData := Data;
        Tag := ATag;
        FormStyle := fsNormal;
        SelectMode := True;
        Application.ProcessMessages;
        if LoadData then begin
          Application.ProcessMessages;
          SetSelectedValue(Value);
          Application.ProcessMessages;
          Result := ShowModal = mrOk;
          Application.ProcessMessages;
          if Result then Value := GetSelectedValue;
        end
        else Result := False;
      finally
        Free;
      end;
    end;
  end
  else raise Exception.CreateFmt(ECannotCreateChildForm, [FormClass.ClassName]);
end;

function ShowSelectNamedForm(FormClass: TDataFormClass; const ATag: Integer; var Value: Integer;
  const FormCaption: string; const Data: Pointer = nil): Boolean;
begin
  Result := False;
  if not IsNotWait then
  begin
    WarningBox(EWindowAlreadyOpen);
    Exit;
  end;
  if Assigned(Application.MainForm) then
  begin
    with FormClass.Create(Application.MainForm) do
    begin
      try
        Application.ProcessMessages;
        Caption := FormCaption;
        FormData := Data;
        Tag := ATag;
        FormStyle := fsNormal;
        SelectMode := True;
        Application.ProcessMessages;
        if LoadData then begin
          Application.ProcessMessages;
          SetSelectedValue(Value);
          Application.ProcessMessages;
          Result := ShowModal = mrOk;
          Application.ProcessMessages;
          if Result then Value := GetSelectedValue;
        end
        else Result := False;
      finally
        Free;
      end;
    end;
  end
  else raise Exception.CreateFmt(ECannotCreateChildForm, [FormClass.ClassName]);
end;

function ShowGroupForm(FormClass: TDataFormClass; const ATag: Integer; var GroupValue, ItemValue: Integer;
  const Data: Pointer = nil): Boolean;
begin
  Result := False;
  if not IsNotWait then
  begin
    WarningBox(EWindowAlreadyOpen);
    Exit;
  end;
  if Assigned(Application.MainForm) then
  begin
    with FormClass.Create(Application.MainForm) do
    begin
      try
        Application.ProcessMessages;
        FormData := Data;
        Tag := ATag;
        FormStyle := fsNormal;
        SelectMode := True;
        Application.ProcessMessages;
        if LoadData then begin
          Application.ProcessMessages;
          SetSelectedValues(GroupValue, ItemValue);
          Application.ProcessMessages;
          Result := ShowModal = mrOk;
          Application.ProcessMessages;
          if Result then
            Result := GetSelectedValues(GroupValue, ItemValue);
        end
        else Result := False;
      finally
        Free;
      end;
    end;
  end
  else raise Exception.CreateFmt(ECannotCreateChildForm, [FormClass.ClassName]);
end;

function ShowTreeForm(FormClass: TDataFormClass; const ATag: Integer;
  const LoadMode: TTreeLoadMode; SelectedTypes: TNodeTypes; var GroupValue, ItemValue: Integer;
  const Data: Pointer = nil): Boolean;
begin
  Result := False;
  if not IsNotWait then
  begin
    WarningBox(EWindowAlreadyOpen);
    Exit;
  end;
  if Assigned(Application.MainForm) then
  begin
    with TDbTreeTemplate(FormClass.Create(Application.MainForm)) do
    begin
      try
        Application.ProcessMessages;
        FormData := Data;
        Tag := ATag;
        FormStyle := fsNormal;
        SelectMode := True;
        TreeLoader.LoadMode := LoadMode;
        SelectedNodes := SelectedTypes;
        Application.ProcessMessages;
        if LoadData then begin
          Application.ProcessMessages;
          SetSelectedValues(GroupValue, ItemValue);
          Application.ProcessMessages;
          Result := ShowModal = mrOk;
          Application.ProcessMessages;
          if Result then
            Result := GetSelectedValues(GroupValue, ItemValue);
        end
        else Result := False;
      finally
        Free;
      end;
    end;
  end
  else raise Exception.CreateFmt(ECannotCreateChildForm, [FormClass.ClassName]);
end;

function ShowTreeGroupForm(FormClass: TDataFormClass; const ATag: Integer; var Value: Integer;
  const Data: Pointer = nil): Boolean;
begin
  Result := False;
  if not IsNotWait then
  begin
    WarningBox(EWindowAlreadyOpen);
    Exit;
  end;
  if Assigned(Application.MainForm) then
  begin
    with TDbTreeTemplate(FormClass.Create(Application.MainForm)) do
    begin
      try
        Application.ProcessMessages;
        FormData := Data;
        Tag := ATag;
        FormStyle := fsNormal;
        SelectMode := True;
        TreeLoader.LoadMode := lmGroups;
        SelectedNodes := [ntGroup];
        NewItem.Visible := False;
        Application.ProcessMessages;
        if LoadData then
        begin
          Application.ProcessMessages;
          SetSelectedValue(Value);
          Application.ProcessMessages;
          Result := ShowModal = mrOk;
          Application.ProcessMessages;
          if Result then Value := GetSelectedValue;
        end
        else Result := False;
      finally
        Free;
      end;
    end;
  end
  else raise Exception.CreateFmt(ECannotCreateChildForm, [FormClass.ClassName]);
end;

function  ShowTreeItemForm(FormClass: TDataFormClass; const ATag: Integer; var Value: Integer;
  const Data: Pointer = nil): Boolean;
begin
  Result := False;
  if not IsNotWait then
  begin
    WarningBox(EWindowAlreadyOpen);
    Exit;
  end;
  if Assigned(Application.MainForm) then
  begin
    with TDbTreeTemplate(FormClass.Create(Application.MainForm)) do
    begin
      try
        Application.ProcessMessages;
        FormData := Data;
        Tag := ATag;
        FormStyle := fsNormal;
        SelectMode := True;
        TreeLoader.LoadMode := lmAll;
        SelectedNodes := [ntItem];
        Application.ProcessMessages;
        if LoadData then begin
          Application.ProcessMessages;
          SetSelectedValues(intDisable, Value);
          Application.ProcessMessages;
          Result := ShowModal = mrOk;
          Application.ProcessMessages;
          if Result then Value := GetSelectedValue;
        end
        else Result := False;
      finally
        Free;
      end;
    end;
  end
  else raise Exception.CreateFmt(ECannotCreateChildForm, [FormClass.ClassName]);
end;

function ShowFieldForm(FormClass: TDataFormClass; const ATag: Integer; Field: TField; const Data: Pointer = nil;
  const LookupDS1: TDataSet = nil; const LookupDS2: TDataSet = nil; const LookupDS3: TDataSet = nil): Boolean;
var
  FieldValue: Integer;
begin
  FieldValue := Field.AsInteger;
  Application.ProcessMessages;
  Result := ShowSelectForm(FormClass, ATag, FieldValue, Data);
  Application.ProcessMessages;
  if Result then begin
    if FieldValue = intDisable
    then Field.Clear
    else begin
      if Assigned(LookupDS1) then
        OpenDS_Static(nil, LookupDS1);
      if Assigned(LookupDS2) then
        OpenDS_Static(nil, LookupDS2);
      if Assigned(LookupDS3) then
        OpenDS_Static(nil, LookupDS3);
      Field.AsInteger := FieldValue;
    end;
  end;
end;

function ShowTreeGroupFieldForm(FormClass: TDataFormClass; const ATag: Integer; Field: TField; const Data: Pointer = nil;
  const LookupDS1: TDataSet = nil; const LookupDS2: TDataSet = nil; const LookupDS3: TDataSet = nil): Boolean;
var
  FieldValue: Integer;
begin
  FieldValue := Field.AsInteger;
  Application.ProcessMessages;
  Result := ShowTreeGroupForm(FormClass, ATag, FieldValue, Data);
  Application.ProcessMessages;
  if Result then begin
    if FieldValue = intDisable
    then Field.Clear
    else begin
      if Assigned(LookupDS1) then
        OpenDS_Static(nil, LookupDS1);
      if Assigned(LookupDS2) then
        OpenDS_Static(nil, LookupDS2);
      if Assigned(LookupDS3) then
        OpenDS_Static(nil, LookupDS3);
      Field.AsInteger := FieldValue;
    end;
  end;
end;

function ShowTreeFieldForm(FormClass: TDataFormClass; const ATag: Integer;
  const LoadMode: TTreeLoadMode; SelectedTypes: TNodeTypes;
  GroupsField, ItemsField: TField; const Data: Pointer = nil;
  const LookupDS1: TDataSet = nil; const LookupDS2: TDataSet = nil; const LookupDS3: TDataSet = nil): Boolean;
var
  GroupValue, ItemValue: Integer;
begin
  if Assigned(GroupsField)
  then GroupValue := GroupsField.AsInteger
  else GroupValue := intDisable;
  Application.ProcessMessages;
  if Assigned(ItemsField)
  then ItemValue := ItemsField.AsInteger
  else ItemValue := intDisable;
  Application.ProcessMessages;
  Result := ShowTreeForm(FormClass, ATag, LoadMode, SelectedTypes, GroupValue, ItemValue, Data);
  Application.ProcessMessages;
  if Result and Assigned(GroupsField) then begin
    if GroupValue = intDisable
    then GroupsField.Clear
    else begin
      if Assigned(LookupDS1) then
        OpenDS_Static(nil, LookupDS1);
      if Assigned(LookupDS2) then
        OpenDS_Static(nil, LookupDS2);
      if Assigned(LookupDS3) then
        OpenDS_Static(nil, LookupDS3);
      GroupsField.AsInteger := GroupValue;
    end;
  end;
  Application.ProcessMessages;
  if Result and Assigned(ItemsField) then begin
    if ItemValue = intDisable
    then ItemsField.Clear
    else begin
      if Assigned(LookupDS1) then
        OpenDS_Static(nil, LookupDS1);
      if Assigned(LookupDS2) then
        OpenDS_Static(nil, LookupDS2);
      if Assigned(LookupDS3) then
        OpenDS_Static(nil, LookupDS3);
       ItemsField.AsInteger := ItemValue;
    end
  end;
end;

function DbSelectDialog(FormClass: TDbSelectClass; var AGroupKey, AItemKey: Integer;
  const Param0, Param1: Integer): Boolean;
begin
  with FormClass.Create(Application.MainForm) do
  begin
    GroupKeyValue := AGroupKey;
    ItemKeyValue := AItemKey;
    QryParam0 := Param0;
    QryParam1 := Param1;
    Application.ProcessMessages;
    Result := ShowModal = mrOk;
    Application.ProcessMessages;
    if Result then begin
      AGroupKey := GroupKeyValue;
      AItemKey := ItemKeyValue;
    end;
  end;
end;

end.
