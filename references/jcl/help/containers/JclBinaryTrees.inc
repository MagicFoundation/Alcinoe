(*$JPPDEFINEMACRO JCLBINARYTREETYPESINT(NODETYPENAME)
@@NODETYPENAME
<copy TJclBinaryNode>
\ \

@@!!MEMBEROVERVIEW_NODETYPENAME
<copy !!MEMBEROVERVIEW_TJclBinaryNode>
\ \

@@NODETYPENAME.Left
<copy TJclBinaryNode.Left>
\ \

@@NODETYPENAME.Parent
<copy TJclBinaryNode.Parent>
\ \

@@NODETYPENAME.Right
<copy TJclBinaryNode.Right>
\ \

@@NODETYPENAME.Value
<copy TJclBinaryNode.Value>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLBINARYTREETYPESINT()}
*)

(*$JPPDEFINEMACRO JCLBINARYTREEINT(SELFCLASSNAME, COLLECTIONINTERFACENAME, COMPAREFUNCTIONTYPENAME, TYPENAME)
@@SELFCLASSNAME
<copy TJclBinaryTree>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclBinaryTree>
\ \

@@SELFCLASSNAME.Add@TYPENAME
<copy TJclBinaryTree.Add@TObject>
\ \

@@SELFCLASSNAME.AddAll@COLLECTIONINTERFACENAME
<copy TJclBinaryTree.AddAll@IJclCollection>
\ \

@@SELFCLASSNAME.Clear
<copy TJclBinaryTree.Clear>
\ \

@@SELFCLASSNAME.Contains@TYPENAME
<copy TJclBinaryTree.Contains@TObject>
\ \

@@SELFCLASSNAME.ContainsAll@COLLECTIONINTERFACENAME
<copy TJclBinaryTree.ContainsAll@IJclCollection>
\ \

@@SELFCLASSNAME.Root
<copy TJclBinaryTree.Root>
\ \

@@SELFCLASSNAME.TraverseOrder
<copy TJclBinaryTree.TraverseOrder>
\ \

@@SELFCLASSNAME.Destroy
<copy TJclBinaryTree.Destroy>
\ \

@@SELFCLASSNAME.AssignDataTo@TJclAbstractContainerBase
<copy TJclBinaryTree.AssignDataTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.AssignPropertiesTo@TJclAbstractContainerBase
<copy TJclBinaryTree.AssignPropertiesTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.AutoPack
<copy TJclBinaryTree.AutoPack>
\ \

@@SELFCLASSNAME.First
<copy TJclBinaryTree.First>
\ \

@@SELFCLASSNAME.CollectionEquals@COLLECTIONINTERFACENAME
<copy TJclBinaryTree.CollectionEquals@IJclCollection>
\ \

@@SELFCLASSNAME.GetTraverseOrder
<copy TJclBinaryTree.GetTraverseOrder>
\ \

@@SELFCLASSNAME.IsEmpty
<copy TJclBinaryTree.IsEmpty>
\ \

@@SELFCLASSNAME.Last
<copy TJclBinaryTree.Last>
\ \

@@SELFCLASSNAME.Create@COMPAREFUNCTIONTYPENAME
<copy TJclBinaryTree.Create@TCompare@Boolean>
\ \

@@SELFCLASSNAME.Remove@TYPENAME
<copy TJclBinaryTree.Remove@TObject>
\ \

@@SELFCLASSNAME.CreateEmptyContainer
<copy TJclBinaryTree.CreateEmptyContainer>
\ \

@@SELFCLASSNAME.RemoveAll@COLLECTIONINTERFACENAME
<copy TJclBinaryTree.RemoveAll@IJclCollection>
\ \

@@SELFCLASSNAME.RetainAll@COLLECTIONINTERFACENAME
<copy TJclBinaryTree.RetainAll@IJclCollection>
\ \

@@SELFCLASSNAME.Extract@TYPENAME
<copy TJclBinaryTree.Extract@TObject>
\ \

@@SELFCLASSNAME.ExtractAll@COLLECTIONINTERFACENAME
<copy TJclBinaryTree.ExtractAll@IJclCollection>
\ \

@@SELFCLASSNAME.SetTraverseOrder@TJclTraverseOrder
<copy TJclBinaryTree.SetTraverseOrder@TJclTraverseOrder>
\ \

@@SELFCLASSNAME.GetEnumerator
<copy TJclBinaryTree.GetEnumerator>
\ \

@@SELFCLASSNAME.Size
<copy TJclBinaryTree.Size>
\ \

@@SELFCLASSNAME.GetRoot
<copy TJclBinaryTree.GetRoot>
\ \

@@SELFCLASSNAME.Pack
<copy TJclBinaryTree.Pack>
\ \

@@SELFCLASSNAME.SetCapacity@Integer
<copy TJclBinaryTree.SetCapacity@Integer>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLBINARYTREEINT(,,,)}
*)

(*$JPPDEFINEMACRO JCLBINARYTREEITRINT(BASEITRCLASSNAME, PREORDERITRCLASSNAME, INORDERITRCLASSNAME, POSTORDERITRCLASSNAME,
  STDITRINTERFACENAME, COLLECTIONINTERFACENAME, NODETYPENAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME)
@@BASEITRCLASSNAME
<copy TJclBinaryTreeIterator>
\ \

@@!!MEMBEROVERVIEW_BASEITRCLASSNAME
<copy !!MEMBEROVERVIEW_TJclBinaryTreeIterator>
\ \

@@BASEITRCLASSNAME.Current
<copy TJclBinaryTreeIterator.Current>
\ \

@@BASEITRCLASSNAME.Add@TYPENAME
<copy TJclBinaryTreeIterator.Add@TObject>
\ \

@@BASEITRCLASSNAME.AddChild@TYPENAME
<copy TJclBinaryTreeIterator.AddChild@TObject>
\ \

@@BASEITRCLASSNAME.AssignPropertiesTo@TJclAbstractIterator
<copy TJclBinaryTreeIterator.AssignPropertiesTo@TJclAbstractIterator>
\ \

@@BASEITRCLASSNAME.ChildrenCount
<copy TJclBinaryTreeIterator.ChildrenCount>
\ \

@@BASEITRCLASSNAME.Create@COLLECTIONINTERFACENAME@NODETYPENAME@Boolean@TItrStart
<copy TJclBinaryTreeIterator.Create@IJclCollection@TJclBinaryNode@Boolean@TItrStart>
\ \

@@BASEITRCLASSNAME.DeleteChild@Integer
<copy TJclBinaryTreeIterator.DeleteChild@Integer>
\ \

@@BASEITRCLASSNAME.DeleteChildren
<copy TJclBinaryTreeIterator.DeleteChildren>
\ \

@@BASEITRCLASSNAME.Extract
<copy TJclBinaryTreeIterator.Extract>
\ \

@@BASEITRCLASSNAME.ExtractChild@Integer
<copy TJclBinaryTreeIterator.ExtractChild@Integer>
\ \

@@BASEITRCLASSNAME.ExtractChildren
<copy TJclBinaryTreeIterator.ExtractChildren>
\ \

@@BASEITRCLASSNAME.GetChild@Integer
<copy TJclBinaryTreeIterator.GetChild@Integer>
\ \

@@BASEITRCLASSNAME.GetNextCursor
<copy TJclBinaryTreeIterator.GetNextCursor>
\ \

@@BASEITRCLASSNAME.GETTERFUNCTIONNAME
<copy TJclBinaryTreeIterator.GetObject>
\ \

@@BASEITRCLASSNAME.GetPreviousCursor
<copy TJclBinaryTreeIterator.GetPreviousCursor>
\ \

@@BASEITRCLASSNAME.HasChild@Integer
<copy TJclBinaryTreeIterator.HasChild@Integer>
\ \

@@BASEITRCLASSNAME.HasLeft
<copy TJclBinaryTreeIterator.HasLeft>
\ \

@@BASEITRCLASSNAME.HasNext
<copy TJclBinaryTreeIterator.HasNext>
\ \

@@BASEITRCLASSNAME.HasParent
<copy TJclBinaryTreeIterator.HasParent>
\ \

@@BASEITRCLASSNAME.HasPrevious
<copy TJclBinaryTreeIterator.HasPrevious>
\ \

@@BASEITRCLASSNAME.HasRight
<copy TJclBinaryTreeIterator.HasRight>
\ \

@@BASEITRCLASSNAME.IndexOfChild@TYPENAME
<copy TJclBinaryTreeIterator.IndexOfChild@TObject>
\ \

@@BASEITRCLASSNAME.Insert@TYPENAME
<copy TJclBinaryTreeIterator.Insert@TObject>
\ \

@@BASEITRCLASSNAME.InsertChild@Integer@TYPENAME
<copy TJclBinaryTreeIterator.InsertChild@Integer@TObject>
\ \

@@BASEITRCLASSNAME.IteratorEquals@STDITRINTERFACENAME
<copy TJclBinaryTreeIterator.IteratorEquals@IJclIterator>
\ \

@@BASEITRCLASSNAME.Left
<copy TJclBinaryTreeIterator.Left>
\ \

@@BASEITRCLASSNAME.MoveNext
<copy TJclBinaryTreeIterator.MoveNext>
\ \

@@BASEITRCLASSNAME.Next
<copy TJclBinaryTreeIterator.Next>
\ \

@@BASEITRCLASSNAME.NextIndex
<copy TJclBinaryTreeIterator.NextIndex>
\ \

@@BASEITRCLASSNAME.Parent
<copy TJclBinaryTreeIterator.Parent>
\ \

@@BASEITRCLASSNAME.Previous
<copy TJclBinaryTreeIterator.Previous>
\ \

@@BASEITRCLASSNAME.PreviousIndex
<copy TJclBinaryTreeIterator.PreviousIndex>
\ \

@@BASEITRCLASSNAME.Remove
<copy TJclBinaryTreeIterator.Remove>
\ \

@@BASEITRCLASSNAME.Reset
<copy TJclBinaryTreeIterator.Reset>
\ \

@@BASEITRCLASSNAME.Right
<copy TJclBinaryTreeIterator.Right>
\ \

@@BASEITRCLASSNAME.SetChild@Integer@TYPENAME
<copy TJclBinaryTreeIterator.SetChild@Integer@TObject>
\ \

@@BASEITRCLASSNAME.SETTERPROCEDURENAME@TYPENAME
<copy TJclBinaryTreeIterator.SetObject@TObject>
\ \

@@PREORDERITRCLASSNAME
<copy TJclPreOrderBinaryTreeIterator>
\ \

@@!!MEMBEROVERVIEW_PREORDERITRCLASSNAME
<copy !!MEMBEROVERVIEW_TJclPreOrderBinaryTreeIterator>
\ \

@@PREORDERITRCLASSNAME.CreateEmptyIterator
<copy TJclPreOrderBinaryTreeIterator.CreateEmptyIterator>
\ \

@@PREORDERITRCLASSNAME.GetNextCursor
<copy TJclPreOrderBinaryTreeIterator.GetNextCursor>
\ \

@@PREORDERITRCLASSNAME.GetPreviousCursor
<copy TJclPreOrderBinaryTreeIterator.GetPreviousCursor>
\ \

@@INORDERITRCLASSNAME
<copy TJclInOrderBinaryTreeIterator>
\ \

@@!!MEMBEROVERVIEW_INORDERITRCLASSNAME
<copy !!MEMBEROVERVIEW_TJclInOrderBinaryTreeIterator>
\ \

@@INORDERITRCLASSNAME.CreateEmptyIterator
<copy TJclInOrderBinaryTreeIterator.CreateEmptyIterator>
\ \

@@INORDERITRCLASSNAME.GetNextCursor
<copy TJclInOrderBinaryTreeIterator.GetNextCursor>
\ \

@@INORDERITRCLASSNAME.GetPreviousCursor
<copy TJclInOrderBinaryTreeIterator.GetPreviousCursor>
\ \

@@POSTORDERITRCLASSNAME
<copy TJclPostOrderBinaryTreeIterator>
\ \

@@!!MEMBEROVERVIEW_POSTORDERITRCLASSNAME
<copy !!MEMBEROVERVIEW_TJclPostOrderBinaryTreeIterator>
\ \

@@POSTORDERITRCLASSNAME.CreateEmptyIterator
<copy TJclPostOrderBinaryTreeIterator.CreateEmptyIterator>
\ \

@@POSTORDERITRCLASSNAME.GetNextCursor
<copy TJclPostOrderBinaryTreeIterator.GetNextCursor>
\ \

@@POSTORDERITRCLASSNAME.GetPreviousCursor
<copy TJclPostOrderBinaryTreeIterator.GetPreviousCursor>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(,,,,,,,,,)}
*)
