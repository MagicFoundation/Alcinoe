{$JPPUNDEF KEYOWNABLE}
{$JPPUNDEF VALUEOWNABLE}
(*$JPPDEFINEMACRO JCLHASHMAPTYPESINT(ENTRYTYPENAME, ENTRYARRAYTYPENAME, BUCKETTYPENAME)
@@ENTRYTYPENAME
<copy TJclHashEntry>
\ \

@@ENTRYTYPENAME.Key
<copy TJclHashEntry.Key>
\ \

@@ENTRYTYPENAME.Value
<copy TJclHashEntry.Value>
\ \

@@BUCKETTYPENAME
<copy TJclBucket>
\ \

@@!!MEMBEROVERVIEW_BUCKETTYPENAME
<copy !!MEMBEROVERVIEW_TJclBucket>
\ \

@@BUCKETTYPENAME.Entries
<copy TJclBucket.Entries>
\ \

@@BUCKETTYPENAME.Size
<copy TJclBucket.Size>
\ \

{$JPPEXPANDMACRO MOVEARRAYINT(BUCKETTYPENAME.MoveArray,ENTRYARRAYTYPENAME)}
*)
(*$JPPLOOP TRUEMAPINDEX HELPTRUEMAPCOUNT
{$JPPEXPANDMACRO JCLHASHMAPTYPESINT(,,)}
*)

(*$JPPDEFINEMACRO JCLHASHMAPINT(SELFCLASSNAME, MAPINTERFACENAME, KEYTYPENAME, VALUETYPENAME)
@@SELFCLASSNAME
<copy TJclHashMap>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclHashMap>
\ \

@@SELFCLASSNAME.HashToRangeFunction
<copy TJclHashMap.HashToRangeFunction>
\ \

{$IFDEF KEYOWNABLE}
@@SELFCLASSNAME.OwnsKeys
<copy TJclHashMap.OwnsKeys>
\ \

{$ENDIF KEYOWNABLE}
{$IFDEF VALUEOWNABLE}
@@SELFCLASSNAME.OwnsValues
<copy TJclHashMap.OwnsValues>
\ \

{$ENDIF VALUEOWNABLE}
@@SELFCLASSNAME.AssignDataTo@TJclAbstractContainerBase
<copy TJclHashMap.AssignDataTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.AssignPropertiesTo@TJclAbstractContainerBase
<copy TJclHashMap.AssignPropertiesTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.Clear
<copy TJclHashMap.Clear>
\ \

@@SELFCLASSNAME.ContainsKey@KEYTYPENAME
<copy TJclHashMap.ContainsKey@TObject>
\ \

@@SELFCLASSNAME.ContainsValue@VALUETYPENAME
<copy TJclHashMap.ContainsValue@TObject>
\ \

@@SELFCLASSNAME.Create@Integer{$IFDEF KEYOWNABLE}@Boolean{$ENDIF}{$IFDEF VALUEOWNABLE}@Boolean{$ENDIF}
<copy TJclHashMap.Create@Integer@Boolean>
\ \

@@SELFCLASSNAME.CreateEmptyContainer
<copy TJclHashMap.CreateEmptyContainer>
\ \

@@SELFCLASSNAME.Destroy
<copy TJclHashMap.Destroy>
\ \

@@SELFCLASSNAME.Extract@KEYTYPENAME
<copy TJclHashMap.Extract@TObject>
\ \

@@SELFCLASSNAME.FreeKey@KEYTYPENAME
<copy TJclHashMap.FreeKey@TObject>
\ \

@@SELFCLASSNAME.FreeValue@VALUETYPENAME
<copy TJclHashMap.FreeValue@TObject>
\ \

{$IFDEF KEYOWNABLE}
@@SELFCLASSNAME.GetOwnsKeys
<copy TJclHashMap.GetOwnsKeys>
\ \

{$ENDIF KEYOWNABLE}
{$IFDEF VALUEOWNABLE}
@@SELFCLASSNAME.OwnsValues
<copy TJclHashMap.OwnsValues>
\ \

{$ENDIF VALUEOWNABLE}
@@SELFCLASSNAME.GetValue@KEYTYPENAME
<copy TJclHashMap.GetValue@TObject>
\ \

@@SELFCLASSNAME.IsEmpty
<copy TJclHashMap.IsEmpty>
\ \

@@SELFCLASSNAME.KeyOfValue@VALUETYPENAME
<copy TJclHashMap.KeyOfValue@TObject>
\ \

@@SELFCLASSNAME.KeysEqual@KEYTYPENAME@KEYTYPENAME
<copy TJclHashMap.KeysEqual@TObject@TObject>
\ \

@@SELFCLASSNAME.KeySet
<copy TJclHashMap.KeySet>
\ \

@@SELFCLASSNAME.MapEquals@MAPINTERFACENAME
<copy TJclHashMap.MapEquals@IJclMap>
\ \

@@SELFCLASSNAME.Pack
<copy TJclHashMap.Pack>
\ \

@@SELFCLASSNAME.PutAll@MAPINTERFACENAME
<copy TJclHashMap.PutAll@IJclMap>
\ \

@@SELFCLASSNAME.PutValue@KEYTYPENAME@VALUETYPENAME
<copy TJclHashMap.PutValue@TObject@TObject>
\ \

@@SELFCLASSNAME.Remove@KEYTYPENAME
<copy TJclHashMap.Remove@TObject>
\ \

@@SELFCLASSNAME.SetCapacity@Integer
<copy TJclHashMap.SetCapacity@Integer>
\ \

@@SELFCLASSNAME.Size
<copy TJclHashMap.Size>
\ \

@@SELFCLASSNAME.Values
<copy TJclHashMap.Values>
\ \

@@SELFCLASSNAME.ValuesEqual@VALUETYPENAME@VALUETYPENAME
<copy TJclHashMap.ValuesEqual@TObject@TObject>
\ \
*)
(*$JPPLOOP TRUEMAPINDEX HELPTRUEMAPCOUNT
{$JPPEXPANDMACRO JCLHASHMAPINT(,,,)}
*)
