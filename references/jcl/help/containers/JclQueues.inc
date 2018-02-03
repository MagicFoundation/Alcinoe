(*$JPPDEFINEMACRO JCLQUEUEINT(SELFCLASSNAME, TYPENAME)
@@SELFCLASSNAME
<copy TJclQueue>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclQueue>
\ \

@@SELFCLASSNAME.AssignDataTo@TJclAbstractContainerBase
<copy TJclQueue.AssignDataTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.Clear
<copy TJclQueue.Clear>
\ \

@@SELFCLASSNAME.Contains@TYPENAME
<copy TJclQueue.Contains@TObject>
\ \

@@SELFCLASSNAME.Create@Integer
<copy TJclQueue.Create@Integer@Boolean>
\ \

@@SELFCLASSNAME.CreateEmptyContainer
<copy TJclQueue.CreateEmptyContainer>
\ \

@@SELFCLASSNAME.Dequeue
<copy TJclQueue.Dequeue>
\ \

@@SELFCLASSNAME.Destroy
<copy TJclQueue.Destroy>
\ \

@@SELFCLASSNAME.Empty
<copy TJclQueue.Empty>
\ \

@@SELFCLASSNAME.Enqueue@TYPENAME
<copy TJclQueue.Enqueue@TObject>
\ \

@@SELFCLASSNAME.Pack
<copy TJclQueue.Pack>
\ \

@@SELFCLASSNAME.Peek
<copy TJclQueue.Peek>
\ \

@@SELFCLASSNAME.SetCapacity@Integer
<copy TJclQueue.SetCapacity@Integer>
\ \

@@SELFCLASSNAME.Size
<copy TJclQueue.Size>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLQUEUEINT(,)}
*)
