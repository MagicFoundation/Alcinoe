(*$JPPDEFINEMACRO JCLTREETYPESINT(NODETYPENAME, EQUALITYCOMPARERINTERFACENAME, TYPENAME)
@@NODETYPENAME
<copy TJclTreeNode>
\ \

@@!!MEMBEROVERVIEW_NODETYPENAME
<copy !!MEMBEROVERVIEW_TJclTreeNode>
\ \

@@NODETYPENAME.Children
<copy TJclTreeNode.Children>
\ \

@@NODETYPENAME.ChildrenCount
<copy TJclTreeNode.ChildrenCount>
\ \

@@NODETYPENAME.Parent
<copy TJclTreeNode.Parent>
\ \

@@NODETYPENAME.Value
<copy TJclTreeNode.Value>
\ \

@@NODETYPENAME.IndexOfChild@NODETYPENAME
<copy TJclTreeNode.IndexOfChild@TJclTreeNode>
\ \

@@NODETYPENAME.IndexOfValue@TYPENAME@EQUALITYCOMPARERINTERFACENAME
<copy TJclTreeNode.IndexOfValue@TObject@IJclEqualityComparer>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLTREETYPESINT(,,)}
*)

(*$JPPDEFINEMACRO JCLTREEINT(NODETYPENAME, SELFCLASSNAME, COLLECTIONINTERFACENAME, TYPENAME)
@@SELFCLASSNAME
<copy TJclTree>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclTree>
\ \

@@SELFCLASSNAME.Root
<copy TJclTree.Root>
\ \

@@SELFCLASSNAME.TraverseOrder
<copy TJclTree.TraverseOrder>
\ \

@@SELFCLASSNAME.Add@TYPENAME
<copy TJclTree.Add@TObject>
\ \

@@SELFCLASSNAME.AddAll@COLLECTIONINTERFACENAME
<copy TJclTree.AddAll@IJclCollection>
\ \

@@SELFCLASSNAME.AssignDataTo@TJclAbstractContainerBase
<copy TJclTree.AssignDataTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.AssignPropertiesTo@TJclAbstractContainerBase
<copy TJclTree.AssignPropertiesTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.Clear
<copy TJclTree.Clear>
\ \

@@SELFCLASSNAME.CloneNode@NODETYPENAME@NODETYPENAME
<copy TJclTree.CloneNode@TJclTreeNode@TJclTreeNode>
\ \

@@SELFCLASSNAME.CollectionEquals@COLLECTIONINTERFACENAME
<copy TJclTree.CollectionEquals@IJclCollection>
\ \

@@SELFCLASSNAME.Contains@TYPENAME
<copy TJclTree.Contains@TObject>
\ \

@@SELFCLASSNAME.ContainsAll@COLLECTIONINTERFACENAME
<copy TJclTree.ContainsAll@IJclCollection>
\ \

@@SELFCLASSNAME.Create
<copy TJclTree.Create@Boolean>
\ \

@@SELFCLASSNAME.CreateEmptyContainer
<copy TJclTree.CreateEmptyContainer>
\ \

@@SELFCLASSNAME.Destroy
<copy TJclTree.Destroy>
\ \

@@SELFCLASSNAME.Extract@TYPENAME
<copy TJclTree.Extract@TObject>
\ \

@@SELFCLASSNAME.ExtractAll@COLLECTIONINTERFACENAME
<copy TJclTree.ExtractAll@IJclCollection>
\ \

@@SELFCLASSNAME.ExtractNode@NODETYPENAME
<copy TJclTree.ExtractNode@TJclTreeNode>
\ \

@@SELFCLASSNAME.First
<copy TJclTree.First>
\ \

@@SELFCLASSNAME.GetEnumerator
<copy TJclTree.GetEnumerator>
\ \

@@SELFCLASSNAME.GetRoot
<copy TJclTree.GetRoot>
\ \

@@SELFCLASSNAME.GetTraverseOrder
<copy TJclTree.GetTraverseOrder>
\ \

@@SELFCLASSNAME.IsEmpty
<copy TJclTree.IsEmpty>
\ \

@@SELFCLASSNAME.Last
<copy TJclTree.Last>
\ \

@@SELFCLASSNAME.NodeContains@NODETYPENAME@TYPENAME
<copy TJclTree.NodeContains@TJclTreeNode@TObject>
\ \

@@SELFCLASSNAME.Pack
<copy TJclTree.Pack>
\ \

@@SELFCLASSNAME.PackNode@NODETYPENAME
<copy TJclTree.PackNode@TJclTreeNode>
\ \

@@SELFCLASSNAME.Remove@TYPENAME
<copy TJclTree.Remove@TObject>
\ \

@@SELFCLASSNAME.RemoveAll@COLLECTIONINTERFACENAME
<copy TJclTree.RemoveAll@IJclCollection>
\ \

@@SELFCLASSNAME.RemoveNode@NODETYPENAME
<copy TJclTree.RemoveNode@TJclTreeNode>
\ \

@@SELFCLASSNAME.RetainAll@COLLECTIONINTERFACENAME
<copy TJclTree.RetainAll@IJclCollection>
\ \

@@SELFCLASSNAME.SetCapacity@Integer
<copy TJclTree.SetCapacity@Integer>
\ \

@@SELFCLASSNAME.SetTraverseOrder@TJclTraverseOrder
<copy TJclTree.SetTraverseOrder@TJclTraverseOrder>
\ \

@@SELFCLASSNAME.Size
<copy TJclTree.Size>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLTREEINT(,,,)}
*)

(*$JPPDEFINEMACRO JCLTREEITRINT(BASEITRCLASSNAME, PREORDERITRCLASSNAME, POSTORDERITRCLASSNAME,
  NODETYPENAME, TREECLASSNAME, STDITRINTERFACENAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME)
@@BASEITRCLASSNAME
<copy TJclTreeIterator>
\ \

@@!!MEMBEROVERVIEW_BASEITRCLASSNAME
<copy !!MEMBEROVERVIEW_TJclTreeIterator>
\ \

@@BASEITRCLASSNAME.Current
<copy TJclTreeIterator.Current>
\ \

@@BASEITRCLASSNAME.Add@TYPENAME
<copy TJclTreeIterator.Add@TObject>
\ \

@@BASEITRCLASSNAME.AddChild@TYPENAME
<copy TJclTreeIterator.AddChild@TObject>
\ \

@@BASEITRCLASSNAME.AssignPropertiesTo@TJclAbstractIterator
<copy TJclTreeIterator.AssignPropertiesTo@TJclAbstractIterator>
\ \

@@BASEITRCLASSNAME.ChildrenCount
<copy TJclTreeIterator.ChildrenCount>
\ \

@@BASEITRCLASSNAME.Create@TREECLASSNAME@NODETYPENAME@Boolean@TItrStart
<copy TJclTreeIterator.Create@TJclTree@TJclTreeNode@Boolean@TItrStart>
\ \

@@BASEITRCLASSNAME.DeleteChild@Integer
<copy TJclTreeIterator.DeleteChild@Integer>
\ \

@@BASEITRCLASSNAME.DeleteChildren
<copy TJclTreeIterator.DeleteChildren>
\ \

@@BASEITRCLASSNAME.Extract
<copy TJclTreeIterator.Extract>
\ \

@@BASEITRCLASSNAME.ExtractChild@Integer
<copy TJclTreeIterator.ExtractChild@Integer>
\ \

@@BASEITRCLASSNAME.ExtractChildren
<copy TJclTreeIterator.ExtractChildren>
\ \

@@BASEITRCLASSNAME.GetChild@Integer
<copy TJclTreeIterator.GetChild@Integer>
\ \

@@BASEITRCLASSNAME.GetNextCursor
<copy TJclTreeIterator.GetNextCursor>
\ \

@@BASEITRCLASSNAME.GetNextSibling
<copy TJclTreeIterator.GetNextSibling>
\ \

@@BASEITRCLASSNAME.GETTERFUNCTIONNAME
<copy TJclTreeIterator.GetObject>
\ \

@@BASEITRCLASSNAME.GetPreviousCursor
<copy TJclTreeIterator.GetPreviousCursor>
\ \

@@BASEITRCLASSNAME.HasChild@Integer
<copy TJclTreeIterator.HasChild@Integer>
\ \

@@BASEITRCLASSNAME.HasNext
<copy TJclTreeIterator.HasNext>
\ \

@@BASEITRCLASSNAME.HasParent
<copy TJclTreeIterator.HasParent>
\ \

@@BASEITRCLASSNAME.HasPrevious
<copy TJclTreeIterator.HasPrevious>
\ \

@@BASEITRCLASSNAME.IndexOfChild@TYPENAME
<copy TJclTreeIterator.IndexOfChild@TObject>
\ \

@@BASEITRCLASSNAME.Insert@TYPENAME
<copy TJclTreeIterator.Insert@TObject>
\ \

@@BASEITRCLASSNAME.InsertChild@Integer@TYPENAME
<copy TJclTreeIterator.InsertChild@Integer@TObject>
\ \

@@BASEITRCLASSNAME.IteratorEquals@STDITRINTERFACENAME
<copy TJclTreeIterator.IteratorEquals@IJclIterator>
\ \

@@BASEITRCLASSNAME.MoveNext
<copy TJclTreeIterator.MoveNext>
\ \

@@BASEITRCLASSNAME.Next
<copy TJclTreeIterator.Next>
\ \

@@BASEITRCLASSNAME.NextIndex
<copy TJclTreeIterator.NextIndex>
\ \

@@BASEITRCLASSNAME.Parent
<copy TJclTreeIterator.Parent>
\ \

@@BASEITRCLASSNAME.Previous
<copy TJclTreeIterator.Previous>
\ \

@@BASEITRCLASSNAME.PreviousIndex
<copy TJclTreeIterator.PreviousIndex>
\ \

@@BASEITRCLASSNAME.Remove
<copy TJclTreeIterator.Remove>
\ \

@@BASEITRCLASSNAME.Reset
<copy TJclTreeIterator.Reset>
\ \

@@BASEITRCLASSNAME.SetChild@Integer@TYPENAME
<copy TJclTreeIterator.SetChild@Integer@TObject>
\ \

@@BASEITRCLASSNAME.SETTERPROCEDURENAME@TYPENAME
<copy TJclTreeIterator.SetObject@TObject>
\ \

@@PREORDERITRCLASSNAME
<copy TJclPreOrderTreeIterator>
\ \

@@!!MEMBEROVERVIEW_PREORDERITRCLASSNAME
<copy !!MEMBEROVERVIEW_TJclPreOrderTreeIterator>
\ \

@@PREORDERITRCLASSNAME.CreateEmptyIterator
<copy TJclPreOrderTreeIterator.CreateEmptyIterator>
\ \

@@PREORDERITRCLASSNAME.GetNextCursor
<copy TJclPreOrderTreeIterator.GetNextCursor>
\ \

@@PREORDERITRCLASSNAME.GetNextSibling
<copy TJclPreOrderTreeIterator.GetNextSibling>
\ \

@@PREORDERITRCLASSNAME.GetPreviousCursor
<copy TJclPreOrderTreeIterator.GetPreviousCursor>
\ \

@@POSTORDERITRCLASSNAME
<copy TJclPostOrderTreeIterator>
\ \

@@!!MEMBEROVERVIEW_POSTORDERITRCLASSNAME
<copy !!MEMBEROVERVIEW_TJclPostOrderTreeIterator>
\ \

@@POSTORDERITRCLASSNAME.CreateEmptyIterator
<copy TJclPostOrderTreeIterator.CreateEmptyIterator>
\ \

@@POSTORDERITRCLASSNAME.GetNextCursor
<copy TJclPostOrderTreeIterator.GetNextCursor>
\ \

@@POSTORDERITRCLASSNAME.GetNextSibling
<copy TJclPostOrderTreeIterator.GetNextSibling>
\ \

@@POSTORDERITRCLASSNAME.GetPreviousCursor
<copy TJclPostOrderTreeIterator.GetPreviousCursor>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLTREEITRINT(,,,,,,,,)}
*)
