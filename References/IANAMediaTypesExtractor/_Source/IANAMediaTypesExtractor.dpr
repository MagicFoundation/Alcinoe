program IANAMediaTypesExtractor;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  system.Classes,
  system.AnsiStrings,
  System.IOUtils,
  System.SysUtils,
  Alcinoe.Common,
  Alcinoe.StringList,
  Alcinoe.HTTP.Client.WinHTTP,
  Alcinoe.StringUtils,
  Alcinoe.files;

begin
  try
    var LHttpClient := ALCreateWinHttpClient;
    try
      var LHttpResponse := LHttpClient.Get('https://raw.githubusercontent.com/apache/httpd/refs/heads/trunk/docs/conf/mime.types');
      Try
        if LHttpResponse.StatusCode <> 200 then raise Exception.Create('Error A042F0D0-CE40-4203-A0B3-BB38080434D5');
        var LSrcLst := TALStringListA.Create;
        var LAlMimeContentTypeByExtA := TALStringListA.Create;
        var LAlExtbyMimeContentTypeA := TALStringListA.Create;
        try
          LAlMimeContentTypeByExtA.Duplicates := DupError;
          LAlMimeContentTypeByExtA.Sorted := True;
          LAlExtbyMimeContentTypeA.Duplicates := DupError;
          LAlExtbyMimeContentTypeA.Sorted := True;
          LSrcLst.Text := ALLowercase(LHttpResponse.BodyString);
          for var I := 0 to LSrcLst.Count - 1 do begin
            var LLine := ALTrim(LSrcLst[i]);
            if (LLine = '') or (LLine[low(AnsiString)] = '#') then continue;
            while alposA(#9#9, LLine) > 0 do LLine := ALStringReplaceA(LLine,#9#9, #9, [RfReplaceALL]);
            var LLineLst := TALStringListA.Create;
            try
              LLineLst.LineBreak := #9;
              LLineLst.Text := LLine;
              if LLineLst.Count <> 2 then raise Exception.Create('Error A4B579F4-B080-4475-ADE7-432C882E7CAB - ' + String(LLine));
              var LExtLst := TALStringListA.Create;
              try
                LExtLst.LineBreak := ' ';
                LExtLst.Text := ALTrim(LLineLst[1]);
                if LExtLst.Count = 0 then raise Exception.Create('Error 43D2E03C-5778-4FC4-AC78-70F82AD344E4');
                for var J := 0 to LExtLst.Count - 1 do begin
                  if (LExtLst[j] = 'sub') or
                     (LExtLst[j] = 'wmz') then
                    LAlMimeContentTypeByExtA.AddObject('  AlMimeContentTypeByExtA.Add(''.' + LExtLst[j] + ''',''' + LLineLst[0] + ''');', pointer(integer(1)))
                  else
                    LAlMimeContentTypeByExtA.AddObject('  AlMimeContentTypeByExtA.Add(''.' + LExtLst[j] + ''',''' + LLineLst[0] + ''');', pointer(integer(0)));
                  var LStr := LLineLst[0] + '=' + LExtLst[j];
                       If (Lstr = 'application/mathematica=ma') or
                          (Lstr = 'application/mathematica=mb') or
                          (Lstr = 'application/mp21=m21') or
                          (Lstr = 'application/msword=dot') or
                          (Lstr = 'application/octet-stream=bpk') or
                          (Lstr = 'application/octet-stream=deploy') or
                          (Lstr = 'application/octet-stream=dist') or
                          (Lstr = 'application/octet-stream=distz') or
                          (Lstr = 'application/octet-stream=dms') or
                          (Lstr = 'application/octet-stream=dump') or
                          (Lstr = 'application/octet-stream=elc') or
                          (Lstr = 'application/octet-stream=lrf') or
                          (Lstr = 'application/octet-stream=mar') or
                          (Lstr = 'application/octet-stream=pkg') or
                          (Lstr = 'application/octet-stream=so') or
                          (Lstr = 'application/onenote=onetmp') or
                          (Lstr = 'application/onenote=onetoc') or
                          (Lstr = 'application/onenote=onetoc2') or
                          (Lstr = 'application/pgp-signature=sig') or
                          (Lstr = 'application/pkcs7-mime=p7c') or
                          (Lstr = 'application/postscript=ai') or
                          (Lstr = 'application/postscript=eps') or
                          (Lstr = 'application/smil+xml=smi') or
                          (Lstr = 'application/tei+xml=teicorpus') or
                          (Lstr = 'application/vnd.acucorp=acutc') or
                          (Lstr = 'application/vnd.adobe.fxp=fxpl') or
                          (Lstr = 'application/vnd.clonk.c4group=c4d') or
                          (Lstr = 'application/vnd.clonk.c4group=c4f') or
                          (Lstr = 'application/vnd.clonk.c4group=c4p') or
                          (Lstr = 'application/vnd.clonk.c4group=c4u') or
                          (Lstr = 'application/vnd.dece.data=uvd') or
                          (Lstr = 'application/vnd.dece.data=uvvd') or
                          (Lstr = 'application/vnd.dece.data=uvvf') or
                          (Lstr = 'application/vnd.dece.ttml+xml=uvvt') or
                          (Lstr = 'application/vnd.dece.unspecified=uvvx') or
                          (Lstr = 'application/vnd.dece.zip=uvvz') or
                          (Lstr = 'application/vnd.eszigno3+xml=et3') or
                          (Lstr = 'application/vnd.fdsn.seed=dataless') or
                          (Lstr = 'application/vnd.framemaker=book') or
                          (Lstr = 'application/vnd.framemaker=frame') or
                          (Lstr = 'application/vnd.framemaker=maker') or
                          (Lstr = 'application/vnd.geometry-explorer=gre') or
                          (Lstr = 'application/vnd.grafeq=gqs') or
                          (Lstr = 'application/vnd.ibm.modcap=list3820') or
                          (Lstr = 'application/vnd.ibm.modcap=listafp') or
                          (Lstr = 'application/vnd.iccprofile=icm') or
                          (Lstr = 'application/vnd.intercon.formnet=xpx') or
                          (Lstr = 'application/vnd.kahootz=ktr') or
                          (Lstr = 'application/vnd.kde.kpresenter=kpt') or
                          (Lstr = 'application/vnd.kde.kword=kwt') or
                          (Lstr = 'application/vnd.kinar=knp') or
                          (Lstr = 'application/vnd.koan=skd') or
                          (Lstr = 'application/vnd.koan=skm') or
                          (Lstr = 'application/vnd.koan=skt') or
                          (Lstr = 'application/vnd.ms-excel=xla') or
                          (Lstr = 'application/vnd.ms-excel=xlc') or
                          (Lstr = 'application/vnd.ms-excel=xlm') or
                          (Lstr = 'application/vnd.ms-excel=xlt') or
                          (Lstr = 'application/vnd.ms-excel=xlw') or
                          (Lstr = 'application/vnd.ms-powerpoint=pot') or
                          (Lstr = 'application/vnd.ms-powerpoint=pps') or
                          (Lstr = 'application/vnd.ms-project=mpt') or
                          (Lstr = 'application/vnd.ms-works=wcm') or
                          (Lstr = 'application/vnd.ms-works=wdb') or
                          (Lstr = 'application/vnd.ms-works=wks') or
                          (Lstr = 'application/vnd.nitf=ntf') or
                          (Lstr = 'application/vnd.palm=oprc') or
                          (Lstr = 'application/vnd.palm=pqa') or
                          (Lstr = 'application/vnd.quark.quarkxpress=qwd') or
                          (Lstr = 'application/vnd.quark.quarkxpress=qwt') or
                          (Lstr = 'application/vnd.quark.quarkxpress=qxb') or
                          (Lstr = 'application/vnd.quark.quarkxpress=qxl') or
                          (Lstr = 'application/vnd.quark.quarkxpress=qxt') or
                          (Lstr = 'application/vnd.simtech-mindmapper=twds') or
                          (Lstr = 'application/vnd.solent.sdkm+xml=sdkd') or
                          (Lstr = 'application/vnd.stardivision.writer=vor') or
                          (Lstr = 'application/vnd.sus-calendar=susp') or
                          (Lstr = 'application/vnd.symbian.install=sis') or
                          (Lstr = 'application/vnd.tcpdump.pcap=cap') or
                          (Lstr = 'application/vnd.tcpdump.pcap=dmp') or
                          (Lstr = 'application/vnd.ufdl=ufdl') or
                          (Lstr = 'application/vnd.visio=vss') or
                          (Lstr = 'application/vnd.visio=vst') or
                          (Lstr = 'application/vnd.visio=vsw') or
                          (Lstr = 'application/vnd.zul=zirz') or
                          (Lstr = 'application/x-authorware-bin=u32') or
                          (Lstr = 'application/x-authorware-bin=vox') or
                          (Lstr = 'application/x-authorware-bin=x32') or
                          (Lstr = 'application/x-blorb=blb') or
                          (Lstr = 'application/x-bzip2=boz') or
                          (Lstr = 'application/x-cbr=cb7') or
                          (Lstr = 'application/x-cbr=cba') or
                          (Lstr = 'application/x-cbr=cbr') or
                          (Lstr = 'application/x-cbr=cbt') or
                          (Lstr = 'application/x-debian-package=udeb') or
                          (Lstr = 'application/x-director=cct') or
                          (Lstr = 'application/x-director=cst') or
                          (Lstr = 'application/x-director=cxt') or
                          (Lstr = 'application/x-director=dir') or
                          (Lstr = 'application/x-director=dxr') or
                          (Lstr = 'application/x-director=fgd') or
                          (Lstr = 'application/x-director=swa') or
                          (Lstr = 'application/x-director=w3d') or
                          (Lstr = 'application/x-font-type1=afm') or
                          (Lstr = 'application/x-font-type1=pfa') or
                          (Lstr = 'application/x-font-type1=pfm') or
                          (Lstr = 'application/x-lzh-compressed=lha') or
                          (Lstr = 'application/x-mobipocket-ebook=prc') or
                          (Lstr = 'application/x-msdownload=bat') or
                          (Lstr = 'application/x-msdownload=com') or
                          (Lstr = 'application/x-msdownload=dll') or
                          (Lstr = 'application/x-msdownload=msi') or
                          (Lstr = 'application/x-msmediaview=m13') or
                          (Lstr = 'application/x-msmediaview=m14') or
                          (Lstr = 'application/x-msmetafile=emf') or
                          (Lstr = 'application/x-msmetafile=emz') or
                          (Lstr = 'application/x-msmetafile=wmz') or
                          (Lstr = 'application/x-netcdf=cdf') or
                          (Lstr = 'application/x-pkcs12=p12') or
                          (Lstr = 'application/x-pkcs7-certificates=spc') or
                          (Lstr = 'application/x-texinfo=texinfo') or
                          (Lstr = 'application/x-x509-ca-cert=der') or
                          (Lstr = 'application/x-zmachine=z1') or
                          (Lstr = 'application/x-zmachine=z2') or
                          (Lstr = 'application/x-zmachine=z3') or
                          (Lstr = 'application/x-zmachine=z4') or
                          (Lstr = 'application/x-zmachine=z6') or
                          (Lstr = 'application/x-zmachine=z7') or
                          (Lstr = 'application/x-zmachine=z8') or
                          (Lstr = 'application/xhtml+xml=xht') or
                          (Lstr = 'application/xml=xsl') or
                          (Lstr = 'application/xv+xml=mxml') or
                          (Lstr = 'application/xv+xml=xhvml') or
                          (Lstr = 'application/xv+xml=xvml') or
                          (Lstr = 'audio/basic=snd') or
                          (Lstr = 'audio/midi=kar') or
                          (Lstr = 'audio/midi=midi') or
                          (Lstr = 'audio/midi=rmi') or
                          (Lstr = 'audio/mp4=mp4a') or
                          (Lstr = 'audio/mpeg=m2a') or
                          (Lstr = 'audio/mpeg=m3a') or
                          (Lstr = 'audio/mpeg=mp2') or
                          (Lstr = 'audio/mpeg=mp2a') or
                          (Lstr = 'audio/mpeg=mpga') or
                          (Lstr = 'audio/ogg=oga') or
                          (Lstr = 'audio/ogg=opus') or
                          (Lstr = 'audio/ogg=spx') or
                          (Lstr = 'audio/vnd.dece.audio=uvva') or
                          (Lstr = 'audio/x-aiff=aif') or
                          (Lstr = 'audio/x-aiff=aifc') or
                          (Lstr = 'audio/x-pn-realaudio=ram') or
                          (Lstr = 'image/jpeg=jpe') or
                          (Lstr = 'image/jpeg=jpeg') or
                          (Lstr = 'image/svg+xml=svgz') or
                          (Lstr = 'image/tiff=tiff') or
                          (Lstr = 'image/vnd.dece.graphic=uvg') or
                          (Lstr = 'image/vnd.dece.graphic=uvi') or
                          (Lstr = 'image/vnd.dece.graphic=uvvg') or
                          (Lstr = 'image/vnd.dece.graphic=uvvi') or
                          (Lstr = 'image/vnd.djvu=djv') or
                          (Lstr = 'image/x-freehand=fh4') or
                          (Lstr = 'image/x-freehand=fh5') or
                          (Lstr = 'image/x-freehand=fh7') or
                          (Lstr = 'image/x-freehand=fhc') or
                          (Lstr = 'image/x-pict=pct') or
                          (Lstr = 'message/rfc822=mime') or
                          (Lstr = 'model/iges=iges') or
                          (Lstr = 'model/mesh=mesh') or
                          (Lstr = 'model/mesh=silo') or
                          (Lstr = 'model/vrml=vrml') or
                          (Lstr = 'model/x3d+binary=x3dbz') or
                          (Lstr = 'model/x3d+vrml=x3dvz') or
                          (Lstr = 'model/x3d+xml=x3dz') or
                          (Lstr = 'text/calendar=ifb') or
                          (Lstr = 'text/html=htm') or
                          (Lstr = 'text/javascript=mjs') or
                          (Lstr = 'text/plain=conf') or
                          (Lstr = 'text/plain=def') or
                          (Lstr = 'text/plain=in') or
                          (Lstr = 'text/plain=list') or
                          (Lstr = 'text/plain=log') or
                          (Lstr = 'text/plain=text') or
                          (Lstr = 'text/sgml=sgm') or
                          (Lstr = 'text/troff=man') or
                          (Lstr = 'text/troff=me') or
                          (Lstr = 'text/troff=ms') or
                          (Lstr = 'text/troff=roff') or
                          (Lstr = 'text/troff=t') or
                          (Lstr = 'text/uri-list=uris') or
                          (Lstr = 'text/uri-list=urls') or
                          (Lstr = 'text/x-asm=s') or
                          (Lstr = 'text/x-c=cc') or
                          (Lstr = 'text/x-c=cpp') or
                          (Lstr = 'text/x-c=cxx') or
                          (Lstr = 'text/x-c=dic') or
                          (Lstr = 'text/x-c=h') or
                          (Lstr = 'text/x-c=hh') or
                          (Lstr = 'text/x-fortran=f77') or
                          (Lstr = 'text/x-fortran=f90') or
                          (Lstr = 'text/x-fortran=for') or
                          (Lstr = 'text/x-pascal=p') or
                          (Lstr = 'video/jpm=jpgm') or
                          (Lstr = 'video/mj2=mjp2') or
                          (Lstr = 'video/mp2t=m2t') or
                          (Lstr = 'video/mp2t=m2ts') or
                          (Lstr = 'video/mp2t=mts') or
                          (Lstr = 'video/mp4=mp4v') or
                          (Lstr = 'video/mp4=mpg4') or
                          (Lstr = 'video/mpeg=m1v') or
                          (Lstr = 'video/mpeg=m2v') or
                          (Lstr = 'video/mpeg=mpe') or
                          (Lstr = 'video/mpeg=mpeg') or
                          (Lstr = 'video/quicktime=qt') or
                          (Lstr = 'video/vnd.dece.hd=uvvh') or
                          (Lstr = 'video/vnd.dece.mobile=uvvm') or
                          (Lstr = 'video/vnd.dece.pd=uvvp') or
                          (Lstr = 'video/vnd.dece.sd=uvvs') or
                          (Lstr = 'video/vnd.dece.video=uvvv') or
                          (Lstr = 'video/vnd.mpegurl=mxu') or
                          (Lstr = 'video/vnd.uvvu.mp4=uvvu') or
                          (Lstr = 'video/x-matroska=mk3d') or
                          (Lstr = 'video/x-matroska=mks') or
                          (Lstr = 'video/x-ms-asf=asx') or
                          (Lstr = 'application/inkml+xml=ink') then
                    LAlExtbyMimeContentTypeA.addObject('  AlExtbyMimeContentTypeA.Add('''+LLineLst[0] + ''',''.' + LExtLst[j] + ''');', pointer(integer(1)))
                  else
                    LAlExtbyMimeContentTypeA.addObject('  AlExtbyMimeContentTypeA.Add('''+LLineLst[0] + ''',''.' + LExtLst[j] + ''');', pointer(integer(0)))
                end;
              finally
                ALFreeAndNil(LExtLst);
              end;
            finally
              ALFreeAndNil(LLineLst);
            end;
          end;
          LAlMimeContentTypeByExtA.Sorted := False;
          LAlExtbyMimeContentTypeA.Sorted := False;
          for var I := 0 to LAlMimeContentTypeByExtA.Count - 1 do
            if integer(LAlMimeContentTypeByExtA.Objects[I]) = 1 then
              LAlMimeContentTypeByExtA[i] := '  //' + ALTrim(LAlMimeContentTypeByExtA[i]);
          for var I := 0 to LAlExtbyMimeContentTypeA.Count - 1 do
            if integer(LAlExtbyMimeContentTypeA.Objects[I]) = 1 then
              LAlExtbyMimeContentTypeA[i] := '  //' + ALTrim(LAlExtbyMimeContentTypeA[i]);
          LAlMimeContentTypeByExtA.SaveToFile(ALGetModulePathW + '\AlMimeContentTypeByExtA.pas');
          LAlExtbyMimeContentTypeA.SaveToFile(ALGetModulePathW + '\AlExtbyMimeContentTypeA.pas');
        finally
          ALFreeAndNil(LSrcLst);
          ALFreeAndNil(LAlMimeContentTypeByExtA);
          ALFreeAndNil(LAlExtbyMimeContentTypeA);
        end;
      Finally
        ALFreeAndNil(LHttpResponse);
      End;
    finally
      ALFreeAndNil(LHttpClient);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
