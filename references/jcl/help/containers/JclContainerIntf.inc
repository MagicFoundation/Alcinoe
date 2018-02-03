(*$JPPDEFINEMACRO ITERPROCEDURE(ITERATEPROCEDURETYPENAME)
@@ITERATEPROCEDURETYPENAME
<copy TIterateProcedure>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO ITERPROCEDURE()}
*)

(*$JPPDEFINEMACRO APPLYFUNCTION(APPLYFUNCTIONTYPENAME)
@@APPLYFUNCTIONTYPENAME
<copy TApplyFunction>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO APPLYFUNCTION()}
*)

(*$JPPDEFINEMACRO COMPAREFUNCTION(COMPAREFUNCTIONTYPENAME)
@@COMPAREFUNCTIONTYPENAME
<copy TCompare>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO COMPAREFUNCTION()}
*)

(*$JPPDEFINEMACRO EQUALITYCOMPAREFUNCTION(EQUALITYCOMPAREFUNCTIONTYPENAME)
@@EQUALITYCOMPAREFUNCTIONTYPENAME
<copy TEqualityCompare>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION()}
*)

(*$JPPDEFINEMACRO HASHFUNCTION(HASHCONVERTFUNCTIONTYPENAME)
@@HASHCONVERTFUNCTIONTYPENAME
<copy THashConvert>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO HASHFUNCTION()}
*)

(*$JPPDEFINEMACRO SORTPROC(SORTPROCEDURETYPENAME)
@@SORTPROCEDURETYPENAME
<copy TSortProc>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO SORTPROC()}
*)

(*$JPPDEFINEMACRO EQUALITYCOMPARER(INTERFACENAME, EQUALITYCOMPAREFUNCTIONTYPENAME, TYPENAME)
@@INTERFACENAME
<copy IJclEqualityComparer>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclEqualityComparer>
\ \

@@INTERFACENAME.EqualityCompare
<copy IJclEqualityComparer.EqualityCompare>
\ \

@@INTERFACENAME.GetEqualityCompare
<copy IJclEqualityComparer.GetEqualityCompare>
\ \

@@INTERFACENAME.ItemsEqual@TYPENAME@TYPENAME
<copy IJclEqualityComparer.ItemsEqual@TObject@TObject>
\ \

@@INTERFACENAME.SetEqualityCompare@EQUALITYCOMPAREFUNCTIONTYPENAME
<copy IJclEqualityComparer.SetEqualityCompare@TEqualityCompare>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO EQUALITYCOMPARER(,,)}
*)

(*$JPPDEFINEMACRO COMPARER(INTERFACENAME, COMPAREFUNCTIONTYPENAME, TYPENAME)
@@INTERFACENAME
<copy IJclComparer>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclComparer>
\ \

@@INTERFACENAME.Compare
<copy IJclComparer.Compare>
\ \

@@INTERFACENAME.GetCompare
<copy IJclComparer.GetCompare>
\ \

@@INTERFACENAME.ItemsCompare@TYPENAME@TYPENAME
<copy IJclComparer.ItemsCompare@TObject@TObject>
\ \

@@INTERFACENAME.SetCompare@COMPAREFUNCTIONTYPENAME
<copy IJclComparer.SetCompare@TCompare>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO COMPARER(,,)}
*)

(*$JPPDEFINEMACRO HASHCONVERTER(INTERFACENAME, HASHCONVERTFUNCTIONTYPENAME, TYPENAME)
@@INTERFACENAME
<copy IJclHashConverter>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclHashConverter>
\ \

@@INTERFACENAME.HashConvert
<copy IJclHashConverter.HashConvert>
\ \

@@INTERFACENAME.GetHashConvert
<copy IJclHashConverter.GetHashConvert>
\ \

@@INTERFACENAME.Hash@TYPENAME
<copy IJclHashConverter.Hash@TObject>
\ \

@@INTERFACENAME.SetHashConvert@HASHCONVERTFUNCTIONTYPENAME
<copy IJclHashConverter.SetHashConvert@THashConvert>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO HASHCONVERTER(,,)}
*)

(*$JPPDEFINEMACRO ITERATOR(INTERFACENAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME)
@@INTERFACENAME
<copy IJclIterator>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclIterator>
\ \

@@INTERFACENAME.Current
<copy IJclIterator.Current>
\ \

@@INTERFACENAME.Add@TYPENAME
<copy IJclIterator.Add@TObject>
\ \

@@INTERFACENAME.Extract
<copy IJclIterator.Extract>
\ \

@@INTERFACENAME.GETTERFUNCTIONNAME
<copy IJclIterator.GetObject>
\ \

@@INTERFACENAME.HasNext
<copy IJclIterator.HasNext>
\ \

@@INTERFACENAME.HasPrevious
<copy IJclIterator.HasPrevious>
\ \

@@INTERFACENAME.Insert@TYPENAME
<copy IJclIterator.Insert@TObject>
\ \

@@INTERFACENAME.IteratorEquals@INTERFACENAME
<copy IJclIterator.IteratorEquals@IJclIterator>
\ \

@@INTERFACENAME.MoveNext
<copy IJclIterator.MoveNext>
\ \

@@INTERFACENAME.Next
<copy IJclIterator.Next>
\ \

@@INTERFACENAME.NextIndex
<copy IJclIterator.NextIndex>
\ \

@@INTERFACENAME.Previous
<copy IJclIterator.Previous>
\ \

@@INTERFACENAME.PreviousIndex
<copy IJclIterator.PreviousIndex>
\ \

@@INTERFACENAME.Remove
<copy IJclIterator.Remove>
\ \

@@INTERFACENAME.Reset
<copy IJclIterator.Reset>
\ \

@@INTERFACENAME.SETTERPROCEDURENAME@TYPENAME
<copy IJclIterator.SetObject@TObject>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO ITERATOR(,,,)}
*)

(*$JPPDEFINEMACRO TREEITERATOR(INTERFACENAME, TYPENAME)
@@INTERFACENAME
<copy IJclTreeIterator>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclTreeIterator>
\ \

@@INTERFACENAME.Children
<copy IJclTreeIterator.Children>
\ \

@@INTERFACENAME.AddChild@TYPENAME
<copy IJclTreeIterator.AddChild@TObject>
\ \

@@INTERFACENAME.ChildrenCount
<copy IJclTreeIterator.ChildrenCount>
\ \

@@INTERFACENAME.DeleteChild@Integer
<copy IJclTreeIterator.DeleteChild@Integer>
\ \

@@INTERFACENAME.DeleteChildren
<copy IJclTreeIterator.DeleteChildren>
\ \

@@INTERFACENAME.ExtractChild@Integer
<copy IJclTreeIterator.ExtractChild@Integer>
\ \

@@INTERFACENAME.ExtractChildren
<copy IJclTreeIterator.ExtractChildren>
\ \

@@INTERFACENAME.GetChild@Integer
<copy IJclTreeIterator.GetChild@Integer>
\ \

@@INTERFACENAME.HasChild@Integer
<copy IJclTreeIterator.HasChild@Integer>
\ \

@@INTERFACENAME.HasParent
<copy IJclTreeIterator.HasParent>
\ \

@@INTERFACENAME.IndexOfChild@TYPENAME
<copy IJclTreeIterator.IndexOfChild@TObject>
\ \

@@INTERFACENAME.InsertChild@Integer@TYPENAME
<copy IJclTreeIterator.InsertChild@Integer@TObject>
\ \

@@INTERFACENAME.Parent
<copy IJclTreeIterator.Parent>
\ \

@@INTERFACENAME.SetChild@Integer@TYPENAME
<copy IJclTreeIterator.SetChild@Integer@TObject>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO TREEITERATOR(,)}
*)

(*$JPPDEFINEMACRO BINTREEITERATOR(INTERFACENAME)
@@INTERFACENAME
<copy IJclBinaryTreeIterator>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclBinaryTreeIterator>
\ \

@@INTERFACENAME.HasLeft
<copy IJclBinaryTreeIterator.HasLeft>
\ \

@@INTERFACENAME.HasRight
<copy IJclBinaryTreeIterator.HasRight>
\ \

@@INTERFACENAME.Left
<copy IJclBinaryTreeIterator.Left>
\ \

@@INTERFACENAME.Right
<copy IJclBinaryTreeIterator.Right>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO BINTREEITERATOR()}
*)

(*$JPPDEFINEMACRO COLLECTION(INTERFACENAME, TYPENAME)
@@INTERFACENAME
<copy IJclCollection>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclCollection>
\ \

@@INTERFACENAME.Add@TYPENAME
<copy IJclCollection.Add@TObject>
\ \

@@INTERFACENAME.AddAll@INTERFACENAME
<copy IJclCollection.AddAll@IJclCollection>
\ \

@@INTERFACENAME.Clear
<copy IJclCollection.Clear>
\ \

@@INTERFACENAME.CollectionEquals@INTERFACENAME
<copy IJclCollection.CollectionEquals@IJclCollection>
\ \

@@INTERFACENAME.Contains@TYPENAME
<copy IJclCollection.Contains@TObject>
\ \

@@INTERFACENAME.ContainsAll@INTERFACENAME
<copy IJclCollection.ContainsAll@IJclCollection>
\ \

@@INTERFACENAME.Extract@TYPENAME
<copy IJclCollection.Extract@TObject>
\ \

@@INTERFACENAME.ExtractAll@INTERFACENAME
<copy IJclCollection.ExtractAll@IJclCollection>
\ \

@@INTERFACENAME.First
<copy IJclCollection.First>
\ \

@@INTERFACENAME.GetEnumerator
<copy IJclCollection.GetEnumerator>
\ \

@@INTERFACENAME.IsEmpty
<copy IJclCollection.IsEmpty>
\ \

@@INTERFACENAME.Last
<copy IJclCollection.Last>
\ \

@@INTERFACENAME.Remove@TYPENAME
<copy IJclCollection.Remove@TObject>
\ \

@@INTERFACENAME.RemoveAll@INTERFACENAME
<copy IJclCollection.RemoveAll@IJclCollection>
\ \

@@INTERFACENAME.RetainAll@INTERFACENAME
<copy IJclCollection.RetainAll@IJclCollection>
\ \

@@INTERFACENAME.Size
<copy IJclCollection.Size>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO COLLECTION(,)}
*)

(*$JPPDEFINEMACRO LIST(LISTINTERFACENAME, COLLECTIONINTERFACENAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME, ARRAYPROPERTYNAME)
@@LISTINTERFACENAME
<copy IJclList>
\ \

@@!!MEMBEROVERVIEW_LISTINTERFACENAME
<copy !!MEMBEROVERVIEW_IJclList>
\ \

@@LISTINTERFACENAME.ARRAYPROPERTYNAME
<copy IJclList.Objects>
\ \

@@LISTINTERFACENAME.Delete@Integer
<copy IJclList.Delete@Integer>
\ \

@@LISTINTERFACENAME.ExtractIndex@Integer
<copy IJclList.ExtractIndex@Integer>
\ \

@@LISTINTERFACENAME.GETTERFUNCTIONNAME@Integer
<copy IJclList.GetObject@Integer>
\ \

@@LISTINTERFACENAME.IndexOf@TYPENAME
<copy IJclList.IndexOf@TObject>
\ \

@@LISTINTERFACENAME.Insert@Integer@TYPENAME
<copy IJclList.Insert@Integer@TObject>
\ \

@@LISTINTERFACENAME.InsertAll@Integer@COLLECTIONINTERFACENAME
<copy IJclList.InsertAll@Integer@IJclCollection>
\ \

@@LISTINTERFACENAME.LastIndexOf@TYPENAME
<copy IJclList.LastIndexOf@TObject>
\ \

@@LISTINTERFACENAME.SETTERPROCEDURENAME@Integer@TYPENAME
<copy IJclList.SetObject@Integer@TObject>
\ \

@@LISTINTERFACENAME.SubList@Integer@Integer
<copy IJclList.SubList@Integer@Integer>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO LIST(,,,,,)}
*)

(*$JPPDEFINEMACRO ARRAY(INTERFACENAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME, ARRAYPROPERTYNAME)
@@INTERFACENAME
<copy IJclArray>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclArray>
\ \

@@INTERFACENAME.ARRAYPROPERTYNAME
<copy IJclArray.Objects>
\ \

@@INTERFACENAME.GETTERFUNCTIONNAME@Integer
<copy IJclArray.GetObject@Integer>
\ \

@@INTERFACENAME.SETTERPROCEDURENAME@Integer@TYPENAME
<copy IJclArray.SetObject@Integer@TObject>
\ \
*)
(*$JPPDEFINEMACRO SET(SETINTERFACENAME, COLLECTIONINTERFACENAME)
@@SETINTERFACENAME
<copy IJclSet>
\ \

@@!!MEMBEROVERVIEW_SETINTERFACENAME
<copy !!MEMBEROVERVIEW_IJclSet>
\ \

@@SETINTERFACENAME.Intersect@COLLECTIONINTERFACENAME
<copy IJclSet.Intersect@IJclCollection>
\ \

@@SETINTERFACENAME.Subtract@COLLECTIONINTERFACENAME
<copy IJclSet.Subtract@IJclCollection>
\ \

@@SETINTERFACENAME.Union@COLLECTIONINTERFACENAME
<copy IJclSet.Union@IJclCollection>
\ \
*)
(*$JPPDEFINEMACRO TREE(INTERFACENAME)
@@INTERFACENAME
<copy IJclTree>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclTree>
\ \

@@INTERFACENAME.Root
<copy IJclTree.Root>
\ \

@@INTERFACENAME.TraverseOrder
<copy IJclTree.TraverseOrder>
\ \

@@INTERFACENAME.GetRoot
<copy IJclTree.GetRoot>
\ \

@@INTERFACENAME.GetTraverseOrder
<copy IJclTree.GetTraverseOrder>
\ \

@@IJclTree.SetTraverseOrder@TJclTraverseOrder
<copy IJclTree.SetTraverseOrder@TJclTraverseOrder>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO ARRAY(,,,,)}
*)

(*$JPPDEFINEMACRO MAP(INTERFACENAME, KEYTYPENAME, VALUETYPENAME)
@@INTERFACENAME
<copy IJclMap>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclMap>
\ \

@@INTERFACENAME.Items
<copy IJclMap.Items>
\ \

@@INTERFACENAME.Clear
<copy IJclMap.Clear>
\ \

@@INTERFACENAME.ContainsKey@KEYTYPENAME
<copy IJclMap.ContainsKey@TObject>
\ \

@@INTERFACENAME.ContainsValue@VALUETYPENAME
<copy IJclMap.ContainsValue@TObject>
\ \

@@INTERFACENAME.Extract@KEYTYPENAME
<copy IJclMap.Extract@TObject>
\ \

@@INTERFACENAME.GetValue@KEYTYPENAME
<copy IJclMap.GetValue@TObject>
\ \

@@INTERFACENAME.IsEmpty
<copy IJclMap.IsEmpty>
\ \

@@INTERFACENAME.KeyOfValue@VALUETYPENAME
<copy IJclMap.KeyOfValue@TObject>
\ \

@@INTERFACENAME.KeySet
<copy IJclMap.KeySet>
\ \

@@INTERFACENAME.MapEquals@INTERFACENAME
<copy IJclMap.MapEquals@IJclMap>
\ \

@@INTERFACENAME.PutAll@INTERFACENAME
<copy IJclMap.PutAll@IJclMap>
\ \

@@INTERFACENAME.PutValue@KEYTYPENAME@VALUETYPENAME
<copy IJclMap.PutValue@TObject@TObject>
\ \

@@INTERFACENAME.Remove@KEYTYPENAME
<copy IJclMap.Remove@TObject>
\ \

@@INTERFACENAME.Size
<copy IJclMap.Size>
\ \

@@INTERFACENAME.Values
<copy IJclMap.Values>
\ \
*)
(*$JPPLOOP TRUEMAPINDEX HELPTRUEMAPCOUNT
{$JPPEXPANDMACRO MAP(,,)}
*)

(*$JPPDEFINEMACRO QUEUE(INTERFACENAME, TYPENAME)
@@INTERFACENAME
<copy IJclQueue>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclQueue>
\ \

@@INTERFACENAME.Clear
<copy IJclQueue.Clear>
\ \

@@INTERFACENAME.Contains@TYPENAME
<copy IJclQueue.Contains@TObject>
\ \

@@INTERFACENAME.Dequeue
<copy IJclQueue.Dequeue>
\ \

@@INTERFACENAME.Empty
<copy IJclQueue.Empty>
\ \

@@INTERFACENAME.Enqueue@TYPENAME
<copy IJclQueue.Enqueue@TObject>
\ \

@@INTERFACENAME.Peek
<copy IJclQueue.Peek>
\ \

@@INTERFACENAME.Size
<copy IJclQueue.Size>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO QUEUE(,)}
*)

(*$JPPDEFINEMACRO SORTEDMAP(INTERFACENAME, KEYTYPENAME)
@@INTERFACENAME
<copy IJclSortedMap>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclSortedMap>
\ \

@@INTERFACENAME.FirstKey
<copy IJclSortedMap.FirstKey>
\ \

@@INTERFACENAME.HeadMap@KEYTYPENAME
<copy IJclSortedMap.HeadMap@TObject>
\ \

@@INTERFACENAME.LastKey
<copy IJclSortedMap.LastKey>
\ \

@@INTERFACENAME.SubMap@KEYTYPENAME@KEYTYPENAME
<copy IJclSortedMap.SubMap@TObject@TObject>
\ \

@@INTERFACENAME.TailMap@KEYTYPENAME
<copy IJclSortedMap.TailMap@TObject>
\ \
*)
(*$JPPLOOP TRUEMAPINDEX HELPTRUEMAPCOUNT
{$JPPEXPANDMACRO SORTEDMAP(,)}
*)

(*$JPPDEFINEMACRO SORTEDSET(INTERFACENAME, TYPENAME)
@@INTERFACENAME
<copy IJclSortedSet>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclSortedSet>
\ \

@@INTERFACENAME.HeadSet@TYPENAME
<copy IJclSortedSet.HeadSet@TObject>
\ \

@@INTERFACENAME.SubSet@TYPENAME@TYPENAME
<copy IJclSortedSet.SubSet@TObject@TObject>
\ \

@@INTERFACENAME.TailSet@TYPENAME
<copy IJclSortedSet.TailSet@TObject>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO SORTEDSET(,)}
*)

(*$JPPDEFINEMACRO STACK(INTERFACENAME, TYPENAME)
@@INTERFACENAME
<copy IJclStack>
\ \

@@!!MEMBEROVERVIEW_INTERFACENAME
<copy !!MEMBEROVERVIEW_IJclStack>
\ \

@@INTERFACENAME.Clear
<copy IJclStack.Clear>
\ \

@@INTERFACENAME.Contains@TYPENAME
<copy IJclStack.Contains@TObject>
\ \

@@INTERFACENAME.Empty
<copy IJclStack.Empty>
\ \

@@INTERFACENAME.Peek
<copy IJclStack.Peek>
\ \

@@INTERFACENAME.Pop
<copy IJclStack.Pop>
\ \

@@INTERFACENAME.Push@TYPENAME
<copy IJclStack.Push@TObject>
\ \

@@INTERFACENAME.Size
<copy IJclStack.Size>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO STACK(,)}
*)

