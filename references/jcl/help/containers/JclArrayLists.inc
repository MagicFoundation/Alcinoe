(*$JPPDEFINEMACRO JCLARRAYLISTINT(SELFCLASSNAME, COLLECTIONINTERFACENAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME)
@@SELFCLASSNAME
<copy TJclArrayList>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclArrayList>
\ \

@@SELFCLASSNAME.Add@TYPENAME
<copy TJclArrayList.Add@TObject>
\ \

@@SELFCLASSNAME.AddAll@COLLECTIONINTERFACENAME
<copy TJclArrayList.AddAll@IJclCollection>
\ \

@@SELFCLASSNAME.AssignDataTo@TJclAbstractContainerBase
<copy TJclArrayList.AssignDataTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.Clear
<copy TJclArrayList.Clear>
\ \

@@SELFCLASSNAME.CollectionEquals@COLLECTIONINTERFACENAME
<copy TJclArrayList.CollectionEquals@IJclCollection>
\ \

@@SELFCLASSNAME.Contains@TYPENAME
<copy TJclArrayList.Contains@TObject>
\ \

@@SELFCLASSNAME.ContainsAll@COLLECTIONINTERFACENAME
<copy TJclArrayList.ContainsAll@IJclCollection>
\ \

@@SELFCLASSNAME.Create@COLLECTIONINTERFACENAME
<copy TJclArrayList.Create@IJclCollection@Boolean>
\ \

@@SELFCLASSNAME.Create@Integer
<copy TJclArrayList.Create@Integer@Boolean>
\ \

@@SELFCLASSNAME.CreateEmptyContainer
<copy TJclArrayList.CreateEmptyContainer>
\ \

@@SELFCLASSNAME.Delete@Integer
<copy TJclArrayList.Delete@Integer>
\ \

@@SELFCLASSNAME.Destroy
<copy TJclArrayList.Destroy>
\ \

@@SELFCLASSNAME.Extract@TYPENAME
<copy TJclArrayList.Extract@TObject>
\ \

@@SELFCLASSNAME.ExtractAll@COLLECTIONINTERFACENAME
<copy TJclArrayList.ExtractAll@IJclCollection>
\ \

@@SELFCLASSNAME.ExtractIndex@Integer
<copy TJclArrayList.ExtractIndex@Integer>
\ \

@@SELFCLASSNAME.First
<copy TJclArrayList.First>
\ \

@@SELFCLASSNAME.GetEnumerator
<copy TJclArrayList.GetEnumerator>
\ \

@@SELFCLASSNAME.GETTERFUNCTIONNAME@Integer
<copy TJclArrayList.GetObject@Integer>
\ \

@@SELFCLASSNAME.IndexOf@TYPENAME
<copy TJclArrayList.IndexOf@TObject>
\ \

@@SELFCLASSNAME.Insert@Integer@TYPENAME
<copy TJclArrayList.Insert@Integer@TObject>
\ \

@@SELFCLASSNAME.InsertAll@Integer@COLLECTIONINTERFACENAME
<copy TJclArrayList.InsertAll@Integer@IJclCollection>
\ \

@@SELFCLASSNAME.IsEmpty
<copy TJclArrayList.IsEmpty>
\ \

@@SELFCLASSNAME.Last
<copy TJclArrayList.Last>
\ \

@@SELFCLASSNAME.LastIndexOf@TYPENAME
<copy TJclArrayList.LastIndexOf@TObject>
\ \

@@SELFCLASSNAME.Remove@TYPENAME
<copy TJclArrayList.Remove@TObject>
\ \

@@SELFCLASSNAME.RemoveAll@COLLECTIONINTERFACENAME
<copy TJclArrayList.RemoveAll@IJclCollection>
\ \

@@SELFCLASSNAME.RetainAll@COLLECTIONINTERFACENAME
<copy TJclArrayList.RetainAll@IJclCollection>
\ \

@@SELFCLASSNAME.SetCapacity@Integer
<copy TJclArrayList.SetCapacity@Integer>
\ \

@@SELFCLASSNAME.SETTERPROCEDURENAME@Integer@TYPENAME
<copy TJclArrayList.SetObject@Integer@TObject>
\ \

@@SELFCLASSNAME.Size
<copy TJclArrayList.Size>
\ \

@@SELFCLASSNAME.SubList@Integer@Integer
<copy TJclArrayList.SubList@Integer@Integer>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLARRAYLISTINT(,,,,)}
*)
(*$JPPDEFINEMACRO JCLARRAYLISTITRINT(SELFCLASSNAME, ITRINTERFACENAME, LISTCLASSNAME,
  TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME)
@@SELFCLASSNAME
<copy TJclArrayIterator>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclArrayIterator>
\ \

@@SELFCLASSNAME.Current
<copy TJclArrayIterator.Current>
\ \

@@SELFCLASSNAME.Current
<copy TJclArrayIterator.Current>
\ \

@@SELFCLASSNAME.Add@TYPENAME
<copy TJclArrayIterator.Add@TObject>
\ \

@@SELFCLASSNAME.AssignPropertiesTo@TJclAbstractIterator
<copy TJclArrayIterator.AssignPropertiesTo@TJclAbstractIterator>
\ \

@@SELFCLASSNAME.Create@LISTCLASSNAME@Integer@Boolean@TItrStart
<copy TJclArrayIterator.Create@TJclArray@Integer@Boolean@TItrStart>
\ \

@@SELFCLASSNAME.CreateEmptyIterator
<copy TJclArrayIterator.CreateEmptyIterator>
\ \

@@SELFCLASSNAME.Extract
<copy TJclArrayIterator.Extract>
\ \

@@SELFCLASSNAME.GETTERFUNCTIONNAME
<copy TJclArrayIterator.GetObject>
\ \

@@SELFCLASSNAME.HasNext
<copy TJclArrayIterator.HasNext>
\ \

@@SELFCLASSNAME.HasPrevious
<copy TJclArrayIterator.HasPrevious>
\ \

@@SELFCLASSNAME.Insert@TYPENAME
<copy TJclArrayIterator.Insert@TObject>
\ \

@@SELFCLASSNAME.IteratorEquals@ITRINTERFACENAME
<copy TJclArrayIterator.IteratorEquals@IJclIterator>
\ \

@@SELFCLASSNAME.MoveNext
<copy TJclArrayIterator.MoveNext>
\ \

@@SELFCLASSNAME.Next
<copy TJclArrayIterator.Next>
\ \

@@SELFCLASSNAME.NextIndex
<copy TJclArrayIterator.NextIndex>
\ \

@@SELFCLASSNAME.Previous
<copy TJclArrayIterator.Previous>
\ \

@@SELFCLASSNAME.PreviousIndex
<copy TJclArrayIterator.PreviousIndex>
\ \

@@SELFCLASSNAME.Remove
<copy TJclArrayIterator.Remove>
\ \

@@SELFCLASSNAME.Reset
<copy TJclArrayIterator.Reset>
\ \

@@SELFCLASSNAME.SETTERPROCEDURENAME@TYPENAME
<copy TJclArrayIterator.SetObject@TObject>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(,,,,,)}
*)
