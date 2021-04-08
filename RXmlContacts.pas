unit RXmlContacts;

interface

uses
  SysUtils, ComCtrls;

type
  TContactType = (ctPhoneCell, ctPhoneWork, ctPhoneHome, ctFax, ctEMail, ctUrl, ctAddress, ctOther);

  TContactData = packed record
    fType: TContactType;
    fName: string;
    fData: string;
  end;

  TContactList = array of TContactData;

const
  SContactType: array [TContactType] of string =
    ('Мобильный телефон', 'Служебный телефон', 'Домашний телефон', 'Факс', 'Электронная почта', 'URL', 'Почтовый адрес', 'Прочее');

procedure LoadXmlContacts(const XML: string; var List: TContactList); overload;
procedure LoadXmlContacts(const XML: string; List: TListView); overload;

function SaveXmlContacts(List: TContactList): string; overload;
function SaveXmlContacts(List: TListView): string; overload;

function NewXmlContacts(List: TListView): Boolean;
function EditXmlContacts(List: TListView): Boolean;
function DelXmlContacts(List: TListView): Boolean;

implementation

uses
  Forms, Controls,
  XMLIntf, XMLDoc,
  RDialogs,
  RExHandlers,
  ContactForm;

resourcestring
  SErrorCreateXML      = 'Ошибка генерации XML-документа!';
  SErrorParseXML       = 'Ошибка декодирования XML-документа!';

const
  naDocName            = 'Contacts';
  naName               = 'Item_%d';
  naType               = 'ValueType';
  naData               = 'Value';
  naDescription        = 'Description';

procedure LoadXmlContacts(const XML: string; var List: TContactList);
var
  xmlDoc: IXMLDocument;
  xmlRoot: IXMLNode;
  xmlNode: IXMLNode;
begin
  SetLength(List, 0);
  if Trim(XML) <> '' then
  begin
    try
      xmlDoc := LoadXMLData(XML);
      xmlRoot := xmlDoc.DocumentElement;
      xmlNode := xmlRoot.ChildNodes.First;
      while xmlNode <> nil do
      begin
        SetLength(List, Length(List) + 1);

        List[High(List)].fData := xmlNode.Attributes[naData];
        List[High(List)].fType := xmlNode.Attributes[naType];
        List[High(List)].fName := xmlNode.Attributes[naDescription];

        xmlNode := xmlRoot.ChildNodes.FindSibling(xmlNode, 1);
      end;
    except
      on E: Exception do
        HandleExcept(E, nil, SErrorParseXML);
    end;
  end;
end;

function SaveXmlContacts(List: TContactList): string;
var
  i: Integer;
  xmlDoc: IXMLDocument;
  xmlRoot: IXMLNode;
  xmlNode: IXMLNode;
begin
  Result := '';
  try
    xmlDoc := NewXMLDocument;
    xmlRoot := XMLDoc.DocumentElement;
    if xmlRoot = nil then
      xmlRoot := xmlDoc.AddChild(naDocName, WideString(''));

    for i := Low(List) to High(List) do
    begin
      xmlNode := xmlRoot.AddChild(WideString(Format(naName, [i + 1])));
      xmlNode.Attributes[naData] := WideString(List[i].fData);
      xmlNode.Attributes[naType] := Integer(List[i].fType);
      xmlNode.Attributes[naDescription] := WideString(List[i].fName);
    end;

    xmlRoot.Normalize;
    Result := xmlDoc.XML.Text;
  except
    on E: Exception do
      HandleExcept(E, nil, SErrorCreateXML);
  end;
end;

procedure LoadXmlContacts(const XML: string; List: TListView);
var
  i: Integer;
  BufList: TContactList;
begin
  LoadXmlContacts(XML, BufList);
  List.Items.BeginUpdate;
  try
    List.Items.Clear;
    for i := Low(BufList) to High(BufList) do
      with List.Items.Add do
      begin
        ImageIndex := Integer(BufList[i].fType);
        Caption := SContactType[BufList[i].fType];
        Subitems.Add(BufList[i].fData);
        Subitems.Add(BufList[i].fName);
      end;
  finally
    List.Items.EndUpdate;
    SetLength(BufList, 0);
  end;
end;

function SaveXmlContacts(List: TListView): string;
var
  i: Integer;
  BufList: TContactList;
begin
  Result := '';
  SetLength(BufList, 0);
  try
    for i := 0 to List.Items.Count - 1 do
    begin
      SetLength(BufList, Length(BufList) + 1);

      BufList[High(BufList)].fType := TContactType(List.Items[i].ImageIndex);
      BufList[High(BufList)].fName := List.Items[i].SubItems[1];
      BufList[High(BufList)].fData := List.Items[i].SubItems[0];
    end;

    Result := SaveXmlContacts(BufList);
  finally
    SetLength(BufList, 0);
  end;
end;

function NewXmlContacts(List: TListView): Boolean;
var
  NewItem: TListItem;
begin
  Result := False;

  with TFormContact.Create(Application.MainForm) do
  begin
    try
      if ShowModal = mrOk then
      begin
        List.Items.BeginUpdate;
        try
          NewItem := List.Items.Add;
          NewItem.ImageIndex := Integer(ContType);
          NewItem.Caption := SContactType[ContType];
          NewItem.Subitems.Add(ContData);
          NewItem.Subitems.Add(ContName);
          List.Selected := NewItem;

          Result := True;
        finally
          List.Items.EndUpdate;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function EditXmlContacts(List: TListView): Boolean;
begin
  Result := False;

  with TFormContact.Create(Application.MainForm) do
  begin
    try
      ContType := TContactType(List.Selected.ImageIndex);
      ContData := List.Selected.Subitems[0];
      ContName := List.Selected.Subitems[1];
      if ShowModal = mrOk then
      begin
        List.Items.BeginUpdate;
        try
          List.Selected.ImageIndex := Integer(ContType);
          List.Selected.Caption := SContactType[ContType];
          List.Selected.Subitems[0] := ContData;
          List.Selected.Subitems[1] := ContName;

          Result := True;
        finally
          List.Items.EndUpdate;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function DelXmlContacts(List: TListView): Boolean;
begin
  Result := False;

  if DeleteQueryMulti(List.SelCount) then
  begin
    List.Items.BeginUpdate;
    try
      List.DeleteSelected;

      Result := True;
    finally
      List.Items.EndUpdate;
    end;
  end;
end;

end.
