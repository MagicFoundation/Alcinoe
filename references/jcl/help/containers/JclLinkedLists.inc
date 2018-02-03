(*$JPPDEFINEMACRO JCLLINKEDLISTTYPESINT(ITEMCLASSNAME)
@@ITEMCLASSNAME
<copy TJclLinkedListItem>
\ \

@@!!MEMBEROVERVIEW_ITEMCLASSNAME
<copy !!MEMBEROVERVIEW_TJclLinkedListItem>
\ \

@@ITEMCLASSNAME.Next
<copy TJclLinkedListItem.Next>
\ \

@@ITEMCLASSNAME.Previous
<copy TJclLinkedListItem.Previous>
\ \

@@ITEMCLASSNAME.Value
<copy TJclLinkedListItem.Value>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT()}
*)

(*$JPPDEFINEMACRO JCLLINKEDLISTINT(SELFCLASSNAME, COLLECTIONINTERFACENAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME)
@@SELFCLASSNAME
<copy TJclLinkedList>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclLinkedList>
\ \

@@SELFCLASSNAME.Add@TYPENAME
<copy TJclLinkedList.Add@TObject>
\ \

@@SELFCLASSNAME.AddAll@COLLECTIONINTERFACENAME
<copy TJclLinkedList.AddAll@IJclCollection>
\ \

@@SELFCLASSNAME.AssignDataTo@TJclAbstractContainerBase
<copy TJclLinkedList.AssignDataTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.Clear
<copy TJclLinkedList.Clear>
\ \

@@SELFCLASSNAME.CollectionEquals@COLLECTIONINTERFACENAME
<copy TJclLinkedList.CollectionEquals@IJclCollection>
\ \

@@SELFCLASSNAME.Contains@TYPENAME
<copy TJclLinkedList.Contains@TObject>
\ \

@@SELFCLASSNAME.ContainsAll@COLLECTIONINTERFACENAME
<copy TJclLinkedList.ContainsAll@IJclCollection>
\ \

@@SELFCLASSNAME.Create@COLLECTIONINTERFACENAME
<copy TJclLinkedList.Create@IJclCollection@Boolean>
\ \

@@SELFCLASSNAME.CreateEmptyContainer
<copy TJclLinkedList.CreateEmptyContainer>
\ \

@@SELFCLASSNAME.Delete@Integer
<copy TJclLinkedList.Delete@Integer>
\ \

@@SELFCLASSNAME.Destroy
<copy TJclLinkedList.Destroy>
\ \

@@SELFCLASSNAME.Extract@TYPENAME
<copy TJclLinkedList.Extract@TObject>
\ \

@@SELFCLASSNAME.ExtractAll@COLLECTIONINTERFACENAME
<copy TJclLinkedList.ExtractAll@IJclCollection>
\ \

@@SELFCLASSNAME.ExtractIndex@Integer
<copy TJclLinkedList.ExtractIndex@Integer>
\ \

@@SELFCLASSNAME.First
<copy TJclLinkedList.First>
\ \

@@SELFCLASSNAME.GetEnumerator
<copy TJclLinkedList.GetEnumerator>
\ \

@@SELFCLASSNAME.GETTERFUNCTIONNAME@Integer
<copy TJclLinkedList.GetObject@Integer>
\ \

@@SELFCLASSNAME.IndexOf@TYPENAME
<copy TJclLinkedList.IndexOf@TObject>
\ \

@@SELFCLASSNAME.Insert@Integer@TYPENAME
<copy TJclLinkedList.Insert@Integer@TObject>
\ \

@@SELFCLASSNAME.InsertAll@Integer@COLLECTIONINTERFACENAME
<copy TJclLinkedList.InsertAll@Integer@IJclCollection>
\ \

@@SELFCLASSNAME.IsEmpty
<copy TJclLinkedList.IsEmpty>
\ \

@@SELFCLASSNAME.Last
<copy TJclLinkedList.Last>
\ \

@@SELFCLASSNAME.LastIndexOf@TYPENAME
<copy TJclLinkedList.LastIndexOf@TObject>
\ \

@@SELFCLASSNAME.Remove@TYPENAME
<copy TJclLinkedList.Remove@TObject>
\ \

@@SELFCLASSNAME.RemoveAll@COLLECTIONINTERFACENAME
<copy TJclLinkedList.RemoveAll@IJclCollection>
\ \

@@SELFCLASSNAME.RetainAll@COLLECTIONINTERFACENAME
<copy TJclLinkedList.RetainAll@IJclCollection>
\ \

@@SELFCLASSNAME.SETTERPROCEDURENAME@Integer@TYPENAME
<copy TJclLinkedList.SetObject@Integer@TObject>
\ \

@@SELFCLASSNAME.Size
<copy TJclLinkedList.Size>
\ \

@@SELFCLASSNAME.SubList@Integer@Integer
<copy TJclLinkedList.SubList@Integer@Integer>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLLINKEDLISTINT(,,,,)}
*)

(*$JPPDEFINEMACRO JCLLINKEDLISTITRINT(SELFCLASSNAME, ITRINTERFACENAME, LISTCLASSNAME, ITEMCLASSNAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME)
@@SELFCLASSNAME
<copy TJclLinkedListIterator>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclLinkedListIterator>
\ \

@@SELFCLASSNAME.Current
<copy TJclLinkedListIterator.Current>
\ \

@@SELFCLASSNAME.Add@TYPENAME
<copy TJclLinkedListIterator.Add@TObject>
\ \

@@SELFCLASSNAME.AssignPropertiesTo@TJclAbstractIterator
<copy TJclLinkedListIterator.AssignPropertiesTo@TJclAbstractIterator>
\ \

@@SELFCLASSNAME.Create@LISTCLASSNAME@ITEMCLASSNAME@Boolean@TItrStart
<copy TJclLinkedListIterator.Create@TJclLinkedList@TJclLinkedListItem@Boolean@TItrStart>
\ \

@@SELFCLASSNAME.CreateEmptyIterator
<copy TJclLinkedListIterator.CreateEmptyIterator>
\ \

@@SELFCLASSNAME.Extract
<copy TJclLinkedListIterator.Extract>
\ \

@@SELFCLASSNAME.GETTERFUNCTIONNAME
<copy TJclLinkedListIterator.GetObject>
\ \

@@SELFCLASSNAME.HasNext
<copy TJclLinkedListIterator.HasNext>
\ \

@@SELFCLASSNAME.HasPrevious
<copy TJclLinkedListIterator.HasPrevious>
\ \

@@SELFCLASSNAME.Insert@TYPENAME
<copy TJclLinkedListIterator.Insert@TObject>
\ \

@@SELFCLASSNAME.IteratorEquals@ITRINTERFACENAME
<copy TJclLinkedListIterator.IteratorEquals@IJclIterator>
\ \

@@SELFCLASSNAME.MoveNext
<copy TJclLinkedListIterator.MoveNext>
\ \

@@SELFCLASSNAME.Next
<copy TJclLinkedListIterator.Next>
\ \

@@SELFCLASSNAME.NextIndex
<copy TJclLinkedListIterator.NextIndex>
\ \

@@SELFCLASSNAME.Previous
<copy TJclLinkedListIterator.Previous>
\ \

@@SELFCLASSNAME.PreviousIndex
<copy TJclLinkedListIterator.PreviousIndex>
\ \

@@SELFCLASSNAME.Remove
<copy TJclLinkedListIterator.Remove>
\ \

@@SELFCLASSNAME.Reset
<copy TJclLinkedListIterator.Reset>
\ \

@@SELFCLASSNAME.SETTERPROCEDURENAME@TYPENAME
<copy TJclLinkedListIterator.SetObject@TObject>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLLINKEDLISTITRINT(,,,,,,)}
*)
