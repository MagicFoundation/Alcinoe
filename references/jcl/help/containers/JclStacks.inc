(*$JPPDEFINEMACRO JCLSTACKINT(SELFCLASSNAME, TYPENAME)
@@SELFCLASSNAME
<copy TJclStack>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclStack>
\ \

@@SELFCLASSNAME.AssignDataTo@TJclAbstractContainerBase
<copy TJclStack.AssignDataTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.Clear
<copy TJclStack.Clear>
\ \

@@SELFCLASSNAME.Contains@TYPENAME
<copy TJclStack.Contains@TObject>
\ \

@@SELFCLASSNAME.Create@Integer
<copy TJclStack.Create@Integer@Boolean>
\ \

@@SELFCLASSNAME.CreateEmptyContainer
<copy TJclStack.CreateEmptyContainer>
\ \

@@SELFCLASSNAME.Destroy
<copy TJclStack.Destroy>
\ \

@@SELFCLASSNAME.Empty
<copy TJclStack.Empty>
\ \

@@SELFCLASSNAME.Peek
<copy TJclStack.Peek>
\ \

@@SELFCLASSNAME.Pop
<copy TJclStack.Pop>
\ \

@@SELFCLASSNAME.Push@TYPENAME
<copy TJclStack.Push@TObject>
\ \

@@SELFCLASSNAME.SetCapacity@Integer
<copy TJclStack.SetCapacity@Integer>
\ \

@@SELFCLASSNAME.Size
<copy TJclStack.Size>
\ \
*)
(*$JPPLOOP TRUETYPEINDEX HELPTRUETYPECOUNT
{$JPPEXPANDMACRO JCLSTACKINT(,)}
*)
