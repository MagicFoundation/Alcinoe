{$JPPUNDEF KEYOWNABLE}
{$JPPUNDEF VALUEOWNABLE}
(*$JPPDEFINEMACRO JCLSORTEDMAPTYPESINT(ENTRYTYPENAME)
@@ENTRYTYPENAME
<copy TJclSortedEntry>
\ \

@@ENTRYTYPENAME.Key
<copy TJclSortedEntry.Key>
\ \

@@ENTRYTYPENAME.Value
<copy TJclSortedEntry.Value>
\ \
*)
(*$JPPLOOP TRUEMAPINDEX HELPTRUEMAPCOUNT
{$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT()}
*)

(*$JPPDEFINEMACRO JCLSORTEDMAPINT(SELFCLASSNAME, STDMAPINTERFACENAME, ENTRYARRAYTYPENAME, KEYTYPENAME, VALUETYPENAME)
@@SELFCLASSNAME
<copy TJclSortedMap>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclSortedMap>
\ \

{$IFDEF KEYOWNABLE}
@@SELFCLASSNAME.OwnsKeys
<copy TJclSortedMap.OwnsKeys>
\ \

{$ENDIF KEYOWNABLE}
{$IFDEF VALUEOWNABLE}
@@SELFCLASSNAME.OwnsValues
<copy TJclSortedMap.OwnsValues>
\ \

{$ENDIF VALUEOWNABLE}
@@SELFCLASSNAME.AssignDataTo@TJclAbstractContainerBase
<copy TJclSortedMap.AssignDataTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.BinarySearch@KEYTYPENAME
<copy TJclSortedMap.BinarySearch@TObject>
\ \

@@SELFCLASSNAME.Clear
<copy TJclSortedMap.Clear>
\ \

@@SELFCLASSNAME.ContainsKey@KEYTYPENAME
<copy TJclSortedMap.ContainsKey@TObject>
\ \

@@SELFCLASSNAME.ContainsValue@VALUETYPENAME
<copy TJclSortedMap.ContainsValue@TObject>
\ \

@@SELFCLASSNAME.Create@Integer{$IFDEF KEYOWNABLE}@Boolean{$ENDIF}{$IFDEF VALUEOWNABLE}@Boolean{$ENDIF}
<copy TJclSortedMap.Create@Integer@Boolean@Boolean>
\ \

@@SELFCLASSNAME.CreateEmptyContainer
<copy TJclSortedMap.CreateEmptyContainer>
\ \

@@SELFCLASSNAME.Destroy
<copy TJclSortedMap.Destroy>
\ \

@@SELFCLASSNAME.Extract@KEYTYPENAME
<copy TJclSortedMap.Extract@TObject>
\ \

@@SELFCLASSNAME.FirstKey
<copy TJclSortedMap.FirstKey>
\ \

@@SELFCLASSNAME.FreeKey@KEYTYPENAME
<copy TJclSortedMap.FreeKey@TObject>
\ \

@@SELFCLASSNAME.FreeValue@VALUETYPENAME
<copy TJclSortedMap.FreeValue@TObject>
\ \

{$IFDEF KEYOWNABLE}
@@SELFCLASSNAME.GetOwnsKeys
<copy TJclSortedMap.GetOwnsKeys>
\ \

{$ENDIF KEYOWNABLE}
{$IFDEF VALUEOWNABLE}
@@SELFCLASSNAME.GetOwnsValues
<copy TJclSortedMap.GetOwnsValues>
\ \

{$ENDIF VALUEOWNABLE}
@@SELFCLASSNAME.GetValue@KEYTYPENAME
<copy TJclSortedMap.GetValue@TObject>
\ \

@@SELFCLASSNAME.HeadMap@KEYTYPENAME
<copy TJclSortedMap.HeadMap@TObject>
\ \

@@SELFCLASSNAME.IsEmpty
<copy TJclSortedMap.IsEmpty>
\ \

@@SELFCLASSNAME.KeyOfValue@VALUETYPENAME
<copy TJclSortedMap.KeyOfValue@TObject>
\ \

@@SELFCLASSNAME.KeysCompare@KEYTYPENAME@KEYTYPENAME
<copy TJclSortedMap.KeysCompare@TObject@TObject>
\ \

@@SELFCLASSNAME.KeySet
<copy TJclSortedMap.KeySet>
\ \

@@SELFCLASSNAME.LastKey
<copy TJclSortedMap.LastKey>
\ \

@@SELFCLASSNAME.MapEquals@STDMAPINTERFACENAME
<copy TJclSortedMap.MapEquals@IJclMap>
\ \

{$JPPEXPANDMACRO MOVEARRAYINT(SELFCLASSNAME.MoveArray,ENTRYARRAYTYPENAME)}

@@SELFCLASSNAME.PutAll@STDMAPINTERFACENAME
<copy TJclSortedMap.PutAll@IJclMap>
\ \

@@SELFCLASSNAME.PutValue@KEYTYPENAME@VALUETYPENAME
<copy TJclSortedMap.PutValue@TObject@TObject>
\ \

@@SELFCLASSNAME.Remove@KEYTYPENAME
<copy TJclSortedMap.Remove@TObject>
\ \

@@SELFCLASSNAME.SetCapacity@Integer
<copy TJclSortedMap.SetCapacity@Integer>
\ \

@@SELFCLASSNAME.Size
<copy TJclSortedMap.Size>
\ \

@@SELFCLASSNAME.SubMap@KEYTYPENAME@KEYTYPENAME
<copy TJclSortedMap.SubMap@TObject@TObject>
\ \

@@SELFCLASSNAME.TailMap@KEYTYPENAME
<copy TJclSortedMap.TailMap@TObject>
\ \

@@SELFCLASSNAME.Values
<copy TJclSortedMap.Values>
\ \

@@SELFCLASSNAME.ValuesCompare@VALUETYPENAME@VALUETYPENAME
<copy TJclSortedMap.ValuesCompare@TObject@TObject>
\ \
*)
(*$JPPLOOP TRUEMAPINDEX HELPTRUEMAPCOUNT
{$JPPEXPANDMACRO JCLSORTEDMAPINT(,,,)}
*)
