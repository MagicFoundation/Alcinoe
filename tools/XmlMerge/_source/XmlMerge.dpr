
//
//
// This program was made originaly by http://grijjy.com/  (Allen Drennan)
// for this article: https://blog.grijjy.com/2017/01/30/embed-facebook-sdk-for-android-in-your-delphi-mobile-app-part-2/
//
//

program XmlMerge;

{$APPTYPE CONSOLE}

{$R *.res}

{ Merges multiple Android XML resource directories and files }

uses System.SysUtils,
     System.StrUtils,
     System.IOUtils,
     System.Generics.Defaults,
     System.Generics.Collections,
     OXmlUtils,
     OXmlPDOM;

var MergedPath: String = '';
    MergedXmlDocs: TDictionary<String, IXMLDocument>;

{******************************************}
function CreateXmlResourceDoc: IXMLDocument;
var aNode: PXMLNode;
begin
  result := CreateXMLDoc('resources', true);
  //ExpandEntities
  //  true: entities will be directly expanded to text
  //  false: entities will be detected as separate tokens
  //    Please note that not expanding entity references may cause text content
  //    problems in the DOM (in combination with normalization and/or automatic whitespace handling).
  //    Therefore when not expanding entities, always use "wsPreserveAll" for WhiteSpaceHandling in the DOM
  result.ReaderSettings.ExpandEntities := False;
  //process line breaks (#10, #13, #13#10) to the LineBreak character
  //  set to lbDoNotProcess if you don't want any processing
  //  default is your OS line break (XmlDefaultLineBreak)
  result.ReaderSettings.LineBreak := lbDoNotProcess;
  //indent type - default is none (no indent)
  result.WriterSettings.IndentType := itNone;
  //should single quote (') and double quote (") characters be escaped? Default = False
  result.WriterSettings.EscapeQuotes := False;
  //should the '>' character be saved as an entity? (it's completely fine according to XML spec to use the '>' character in text; Default = True
  result.WriterSettings.UseGreaterThanEntity := False;
  //should be #9, #10 and #13 in attributes saved as entities? Default = True
  result.WriterSettings.UseTabCRLFEntitiesInAttributes := false; // << i don't really know what it's this doesn't matter better to leave like the original
  //document whitespace handling
  result.WhiteSpaceHandling := wsPreserveAll;

  aNode := result.CreateTextNode(#13#10);
  result.Node.InsertBefore(aNode,result.DocumentElement);
end;

{*************************************************************************************}
function IsXmlResourceDoc(const AFileName: String; out AXmlDoc: IXMLDocument): Boolean;
begin
  AXmlDoc := CreateXmlResourceDoc;
  AXmlDoc.LoadFromFile(AFileName);
  if AXmlDoc.DocumentElement <> nil then Result := AXmlDoc.DocumentElement.NodeName.ToLower = 'resources'
  else Result := False;
end;

{***********************************************************************}
procedure Include(const ARootPath: String; const ASubPath: String = '\');

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function nodeIsAlreadyPresent(const aNodeToAdd: PXMLNode; const aDestXmlDoc: IXMLDocument): boolean;
  var aNode, aAttributeNode: PXMLNode;
  begin
    result := false;
    if aNodeToAdd.GetAttribute('name') = '' then exit;
    aNode := nil;
    while aDestXmlDoc.DocumentElement.GetNextChild(aNode) do begin
      if aNode.GetAttribute('name') <> aNodeToAdd.GetAttribute('name') then continue;
      //----
      if aNode.NodeName <> aNodeToAdd.NodeName then continue;
      //----
      if aNode.NodeValue <> aNodeToAdd.NodeValue then
        raise Exception.Create('Resource entry '+aNodeToAdd.GetAttribute('name')+' is already defined with a different value');
      //----
      if aNode.AttributeCount <> aNodeToAdd.AttributeCount then
        raise Exception.Create('Resource entry '+aNodeToAdd.GetAttribute('name')+' is already defined with different attributes');
      //----
      for aAttributeNode in aNode.AttributeEnumerator do
        if aAttributeNode.NodeValue <> aNodeToAdd.GetAttribute(aAttributeNode.NodeName) then
          raise Exception.Create('Resource entry '+aNodeToAdd.GetAttribute('name')+' is already defined with different attributes');
      //----
      result := true;
      break;
    end;
  end;

var aSrcXmlDoc: IXMLDocument;
    aDestXmlDoc: IXMLDocument;
    aDestFileName: String;
    aNode, afoundNode, aCloneNode: PXMLNode;
    sr: TSearchRec;

begin
  if not DirectoryExists(ARootPath + ASubPath) then raise Exception.Create('Directory '+ARootPath+ASubPath+' does not exist');
  if FindFirst(ARootPath + ASubPath + '*.*', faAnyFile, sr) = 0 then begin
    try
      repeat

        //if it's a directory
        if (sr.Attr and faDirectory) > 0 then begin

          //create the same directory in MergedPath
          //and recurse this function in this directory
          if (sr.Name <> '') and (sr.Name[1] <> '.') then begin
            TDirectory.CreateDirectory(MergedPath + ASubPath + sr.Name + '\');
            Include(ARootPath, ASubPath + sr.Name + '\');
          end;

        end

        //if it's a file
        else begin

          //it's an resource xml file
          if (TPath.GetExtension(sr.Name).ToLower = '.xml') and
             (IsXmlResourceDoc(ARootPath + ASubPath + sr.Name, aSrcXmlDoc)) then begin

            //-----
            Writeln('Merging ' + ARootPath + ASubPath + sr.Name);
            aDestFileName := MergedPath + ASubPath + sr.Name;

            //-----
            if not MergedXmlDocs.TryGetValue(aDestFileName, aDestXmlDoc) then begin
              aDestXmlDoc := CreateXmlResourceDoc;
              MergedXmlDocs.AddOrSetValue(aDestFileName, aDestXmlDoc);
            end;

            //-----
            for aNode in ASrcXmlDoc.DocumentElement.AttributeEnumerator do begin
              if not aDestXmlDoc.DocumentElement.FindAttribute(aNode.NodeName, afoundNode) then aDestXmlDoc.DocumentElement.AddAttribute(aNode.NodeName, aNode.NodeValue)
              else if afoundNode.NodeValue <> aNode.NodeValue then raise Exception.Create('Merging of documentElement attributes failed!');
            end;

            //-----
            aNode := aDestXmlDoc.CreateTextNode(#13#10);
            aDestXmlDoc.DocumentElement.AppendChild(aNode);

            //-----
            aNode := nil;
            while aSrcXmlDoc.DocumentElement.GetNextChild(aNode) do begin
              aCloneNode := aNode.CloneNode(True, aDestXmlDoc);
              if (not nodeIsAlreadyPresent(aCloneNode, aDestXmlDoc)) then aDestXmlDoc.DocumentElement.AppendChild(aCloneNode);
            end;

            //-----
            aNode := aDestXmlDoc.CreateTextNode(#13#10);
            aDestXmlDoc.DocumentElement.AppendChild(aNode);

          end

          // not xml that we can merge so just copy the file
          else begin
            Writeln('Copying ' + ARootPath + ASubPath + sr.Name);
            TFile.Copy(ARootPath + ASubPath + sr.Name,
            MergedPath + ASubPath + sr.Name); // << Raise an Exception if the file already exist
          end;

        end;

      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;

{*************}
procedure Save;
var aXmlDocPair: TPair<String, IXMLDocument>;
begin
  for aXmlDocPair in MergedXmlDocs do begin
    aXmlDocPair.Value.SaveToFile(aXmlDocPair.Key);
    Writeln('Saving merged ' + MergedPath + aXmlDocPair.Key);
  end;
end;


var Index: Integer;

begin
  try

    if ParamCount < 2 then begin
      Writeln('Usage: XmlMerge <OutputPath> <ResourcePath1> <ResourcePath2> ...');
      Writeln(' Example: XmlMerge .\Merged\res .\Twitter\res .\Facebook\res');
      Exit;
    end;

    MergedPath := ExcludeTrailingPathDelimiter(ParamStr(1));
    Writeln('Creating directory ' + MergedPath);
    TDirectory.CreateDirectory(MergedPath);

    MergedXmlDocs := TDictionary<String, IXMLDocument>.Create;
    try

      for Index := 2 to ParamCount do Include(ExcludeTrailingPathDelimiter(ParamStr(Index)));
      Save;

    finally
      MergedXmlDocs.Free;
    end;

  except
    on E: Exception do begin
      Writeln(E.ClassName, ': ', E.Message);
      halt(1);
    end;
  end;
end.
