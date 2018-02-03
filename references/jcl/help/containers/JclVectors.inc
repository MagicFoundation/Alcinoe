(*$JPPDEFINEMACRO JCLVECTORINT(SELFCLASSNAME, COLLECTIONINTERFACENAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME)
@@SELFCLASSNAME
<copy TJclVector>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclVector>
\ \

@@SELFCLASSNAME.Items
<copy TJclVector.Items>
\ \

@@SELFCLASSNAME.Add@TYPENAME
<copy TJclVector.Add@TObject>
\ \

@@SELFCLASSNAME.AddAll@COLLECTIONINTERFACENAME
<copy TJclVector.AddAll@IJclCollection>
\ \

@@SELFCLASSNAME.AssignDataTo@TJclAbstractContainerBase
<copy TJclVector.AssignDataTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.Clear
<copy TJclVector.Clear>
\ \

@@SELFCLASSNAME.CollectionEquals@COLLECTIONINTERFACENAME
<copy TJclVector.CollectionEquals@IJclCollection>
\ \

@@SELFCLASSNAME.Contains@TYPENAME
<copy TJclVector.Contains@TObject>
\ \

@@SELFCLASSNAME.ContainsAll@COLLECTIONINTERFACENAME
<copy TJclVector.ContainsAll@IJclCollection>
\ \

@@SELFCLASSNAME.Create@COLLECTIONINTERFACENAME
<copy TJclVector.Create@IJclCollection@Boolean>
\ \

@@SELFCLASSNAME.Create@Integer
<copy TJclVector.Create@Integer@Boolean>
\ \

@@SELFCLASSNAME.CreateEmptyContainer
<copy TJclVector.CreateEmptyContainer>
\ \

@@SELFCLASSNAME.Delete@Integer
<copy TJclVector.Delete@Integer>
\ \

@@SELFCLASSNAME.Destroy
<copy TJclVector.Destroy>
\ \

@@SELFCLASSNAME.Extract@TYPENAME
<copy TJclVector.Extract@TObject>
\ \

@@SELFCLASSNAME.ExtractAll@COLLECTIONINTERFACENAME
<copy TJclVector.ExtractAll@IJclCollection>
\ \

@@SELFCLASSNAME.ExtractIndex@Integer
<copy TJclVector.ExtractIndex@Integer>
\ \

@@SELFCLASSNAME.First
<copy TJclVector.First>
\ \

@@SELFCLASSNAME.GetEnumerator
<copy TJclVector.GetEnumerator>
\ \

@@SELFCLASSNAME.GETTERFUNCTIONNAME@Integer
<copy TJclVector.GetObject@Integer>
\ \

@@SELFCLASSNAME.IndexOf@TYPENAME
<copy TJclVector.IndexOf@TObject>
\ \

@@SELFCLASSNAME.Insert@Integer@TYPENAME
<copy TJclVector.Insert@Integer@TObject>
\ \

@@SELFCLASSNAME.InsertAll@Integer@COLLECTIONINTERFACENAME
<copy TJclVector.InsertAll@Integer@IJclCollection>
\ \

@@SELFCLASSNAME.IsEmpty
<copy TJclVector.IsEmpty>
\ \

@@SELFCLASSNAME.Last
<copy TJclVector.Last>
\ \

@@SELFCLASSNAME.LastIndexOf@TYPENAME
<copy TJclVector.LastIndexOf@TObject>
\ \

@@SELFCLASSNAME.RaiseOutOfBoundsError
<copy TJclVector.RaiseOutOfBoundsError>
\ \

@@SELFCLASSNAME.Remove@TYPENAME
<copy TJclVector.Remove@TObject>
\ \

@@SELFCLASSNAME.RemoveAll@COLLECTIONINTERFACENAME
<copy TJclVector.RemoveAll@IJclCollection>
\ \

@@SELFCLASSNAME.RetainAll@COLLECTIONINTERFACENAME
<copy TJclVector.RetainAll@IJclCollection>
\ \

@@SELFCLASSNAME.SetCapacity@Integer
<copy TJclVector.SetCapacity@Integer>
\ \

@@SELFCLASSNAME.SETTERPROCEDURENAME@Integer@TYPENAME
<copy TJclVector.SetObject@Integer@TObject>
\ \

@@SELFCLASSNAME.Size
<copy TJclVector.Size>
\ \

@@SELFCLASSNAME.SubList@Integer@Integer
<copy TJclVector.SubList@Integer@Integer>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLVECTORINT(,,,,)}
*)

(*$JPPDEFINEMACRO JCLVECTORITRINT(SELFCLASSNAME, ITRINTERFACENAME, LISTCLASSNAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME)
@@SELFCLASSNAME
<copy TJclVectorIterator>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclVectorIterator>
\ \

@@SELFCLASSNAME.Current
<copy TJclVectorIterator.Current>
\ \

@@SELFCLASSNAME.Add@TYPENAME
<copy TJclVectorIterator.Add@TObject>
\ \

@@SELFCLASSNAME.AssignPropertiesTo@TJclAbstractIterator
<copy TJclVectorIterator.AssignPropertiesTo@TJclAbstractIterator>
\ \

@@SELFCLASSNAME.Create@LISTCLASSNAME@Integer@Boolean@TItrStart
<copy TJclVectorIterator.Create@TJclVector@Integer@Boolean@TItrStart>
\ \

@@SELFCLASSNAME.CreateEmptyIterator
<copy TJclVectorIterator.CreateEmptyIterator>
\ \

@@SELFCLASSNAME.Extract
<copy TJclVectorIterator.Extract>
\ \

@@SELFCLASSNAME.GETTERFUNCTIONNAME
<copy TJclVectorIterator.GetObject>
\ \

@@SELFCLASSNAME.HasNext
<copy TJclVectorIterator.HasNext>
\ \

@@SELFCLASSNAME.HasPrevious
<copy TJclVectorIterator.HasPrevious>
\ \

@@SELFCLASSNAME.Insert@TYPENAME
<copy TJclVectorIterator.Insert@TObject>
\ \

@@SELFCLASSNAME.IteratorEquals@ITRINTERFACENAME
<copy TJclVectorIterator.IteratorEquals@IJclIterator>
\ \

@@SELFCLASSNAME.MoveNext
<copy TJclVectorIterator.MoveNext>
\ \

@@SELFCLASSNAME.Next
<copy TJclVectorIterator.Next>
\ \

@@SELFCLASSNAME.NextIndex
<copy TJclVectorIterator.NextIndex>
\ \

@@SELFCLASSNAME.Previous
<copy TJclVectorIterator.Previous>
\ \

@@SELFCLASSNAME.PreviousIndex
<copy TJclVectorIterator.PreviousIndex>
\ \

@@SELFCLASSNAME.Remove
<copy TJclVectorIterator.Remove>
\ \

@@SELFCLASSNAME.Reset
<copy TJclVectorIterator.Reset>
\ \

@@SELFCLASSNAME.SETTERPROCEDURENAME@TYPENAME
<copy TJclVectorIterator.SetObject@TObject>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLVECTORITRINT(,,,,,)}
*)
