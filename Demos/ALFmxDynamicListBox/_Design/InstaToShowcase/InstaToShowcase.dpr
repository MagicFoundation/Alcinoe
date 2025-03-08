program InstaToShowcase;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  system.Net.HttpClient,
  System.IOUtils,
  System.SysUtils,
  System.Classes,
  FMX.Graphics,
  Alcinoe.Common,
  Alcinoe.JSONDoc,
  Alcinoe.Cipher,
  Alcinoe.Files,
  Alcinoe.StringUtils,
  Alcinoe.ImageMagick;

{***************************************************}
procedure ResizeImageTo1080(const aFilename: String);
begin

  // Initialize MagickWand and PixelWand objects
  var LWand: PMagickWand := ALImageMagickLib.NewMagickWand;
  try

    // Load the source image into the MagickWand
    if ALImageMagickLib.MagickReadImage(LWand, pansiChar(AnsiString(aFilename))) <> MagickTrue then RaiseLastMagickWandError(LWand);

    // Retrieve width and height
    var LImgWidth := ALImageMagickLib.MagickGetImageWidth(LWand);
    var LImgHeight := ALImageMagickLib.MagickGetImageHeight(LWand);
    if LImgWidth <= 1080 then exit;

    // Resize the image to the specified icon size
    if ALImageMagickLib.MagickResizeImage(LWand, 1080, round(LImgHeight / (LImgWidth / 1080)), Lanczos2SharpFilter) <> MagickTrue then RaiseLastMagickWandError(LWand);

    // Set the output image format (e.g., PNG, ICO)
    if ALImageMagickLib.MagickSetImageFormat(LWand, PAnsiChar(AnsiString('jpg'))) <> MagickTrue then RaiseLastMagickWandError(LWand);

    // Save the composited image to the specified file location
    TFile.Delete(aFilename);
    if ALImageMagickLib.MagickWriteImage(LWand, pansiChar(AnsiString(aFilename))) <> MagickTrue then RaiseLastMagickWandError(LWand);

  finally
    ALImageMagickLib.DestroyMagickWand(LWand);
  end;

end;

begin
  try

    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    //Create the ImageMagick Library
    {$IFDEF WIN32}
    alCreateImageMagickLibrary(TPath.GetAppPath + '\..\..\..\..\Libraries\dll\imagemagick\win32\');
    {$ELSE}
    alCreateImageMagickLibrary(TPath.GetAppPath + '\..\..\..\..\Libraries\dll\imagemagick\win64\');
    {$ENDIF}
    try

      var LResourceJsonData := TALJSONArrayNodeA.Create;
      try
        var Lfiles := Tdirectory.GetFiles(ALGetModulePathW + '：saved', '*.json');
        for var Lfile in LFiles do begin
          var LPostJsonData := TALJsonDocumentA.CreateFromJSONFile(Lfile);
          //{
          //  "id":"4",
          //  "username":"cinema_streets",
          //  "profile_pic_url":"https://github.com/MagicFoundation/Alcinoe/blob/master/Demos/ALFmxDynamicListBox/_Design/Showcase/connectwithchery.jpg?raw=true",
          //  "geotag":"California",
          //  "caption":"https://github.com/MagicFoundation/Alcinoe/blob/master/Demos/ALFmxDynamicListBox/_Design/Showcase/2023-12-26_20-20-28_UTC_1.jpg?raw=true",
          //  "medias":[
          //    "https://github.com/MagicFoundation/Alcinoe/blob/master/Demos/ALFmxDynamicListBox/_Design/Showcase/2023-12-26_20-20-28_UTC_1.jpg?raw=true",
          //    "https://github.com/MagicFoundation/Alcinoe/blob/master/Demos/ALFmxDynamicListBox/_Design/Showcase/2023-12-26_20-20-28_UTC_2.jpg?raw=true",
          //    "https://github.com/MagicFoundation/Alcinoe/blob/master/Demos/ALFmxDynamicListBox/_Design/Showcase/2023-12-26_20-20-28_UTC_3.jpg?raw=true"],
          //  "media_width":NumberInt(1080),
          //  "media_height":NumberInt(1350),
          //  "like_count":NumberInt(377),
          //  "is_video":false,
          //  "comment_count":NumberInt(129),
          //  "reshare_count":NumberInt(81)
          //}
          With LResourceJsonData.AddChild(ntObject) do begin
            SetChildNodeValueInt64('id', ALRandom64(ALMaxInt64));
            var lUserName := LPostJsonData.GetChildNodeValueText(['node', 'iphone_struct','user','username'], '');
            if lUserName = '' then raise Exception.Create('Error CEE296F2-9E79-4100-9B8F-48B16EF3863E | ' + Lfile);
            SetChildNodeValueText('username', lUserName);
            var LGeoTag: AnsiString;
            Case ALRandom32(200) of
              001: LGeoTag := 'Ulaanbaatar, Mongolia';
              002: LGeoTag := 'Singapore';
              003: LGeoTag := 'Casablanca, Morocco';
              004: LGeoTag := 'Nice, France';
              005: LGeoTag := 'Asia';
              006: LGeoTag := 'Guayaquil, Ecuador';
              007: LGeoTag := 'San Francisco, USA';
              008: LGeoTag := 'Edinburgh, Scotland';
              009: LGeoTag := 'Phoenix, USA';
              010: LGeoTag := 'Stockholm, Sweden';
              011: LGeoTag := 'Caracas, Venezuela';
              012: LGeoTag := 'Amsterdam, Netherlands';
              013: LGeoTag := 'Rio de Janeiro, Brazil';
              014: LGeoTag := 'Volgograd, Russia';
              015: LGeoTag := 'Seoul, South Korea';
              016: LGeoTag := 'Lyon, France';
              017: LGeoTag := 'Hanoi, Vietnam';
              018: LGeoTag := 'Tehran, Iran';
              019: LGeoTag := 'Dublin, Ireland';
              020: LGeoTag := 'Sydney, Australia';
              021: LGeoTag := 'Belgrade, Serbia';
              022: LGeoTag := 'Cairo, Egypt';
              023: LGeoTag := 'Calgary, Canada';
              024: LGeoTag := 'Tbilisi, Georgia';
              025: LGeoTag := 'Toulouse, France';
              026: LGeoTag := 'Strasbourg, France';
              027: LGeoTag := 'Quebec City, Canada';
              028: LGeoTag := 'Bangkok, Thailand';
              029: LGeoTag := 'Africa';
              030: LGeoTag := 'Bucharest, Romania';
              031: LGeoTag := 'Astana, Kazakhstan';
              032: LGeoTag := 'Portland, USA';
              033: LGeoTag := 'Khabarovsk, Russia';
              034: LGeoTag := 'Honolulu, Hawaii, USA';
              035: LGeoTag := 'New Orleans, USA';
              036: LGeoTag := 'Zagreb, Croatia';
              037: LGeoTag := 'Córdoba, Argentina';
              038: LGeoTag := 'Toronto, Canada';
              039: LGeoTag := 'Marseille, France';
              040: LGeoTag := 'Jeddah, Saudi Arabia';
              041: LGeoTag := 'Helsinki, Finland';
              042: LGeoTag := 'Moscow, Russia';
              043: LGeoTag := 'Lille, France';
              044: LGeoTag := 'Ljubljana, Slovenia';
              045: LGeoTag := 'Monterrey, Mexico';
              046: LGeoTag := 'Petropavlovsk-Kamchatsky, Russia';
              047: LGeoTag := 'Edmonton, Canada';
              048: LGeoTag := 'Rome, Italy';
              049: LGeoTag := 'Austin, USA';
              050: LGeoTag := 'Houston, USA';
              051: LGeoTag := 'Montpellier, France';
              052: LGeoTag := 'Port-au-Prince, Haiti';
              053: LGeoTag := 'Denver, USA';
              054: LGeoTag := 'Seattle, USA';
              055: LGeoTag := 'Addis Ababa, Ethiopia';
              056: LGeoTag := 'North America';
              057: LGeoTag := 'Johannesburg, South Africa';
              058: LGeoTag := 'Kampala, Uganda';
              059: LGeoTag := 'Buenos Aires, Argentina';
              060: LGeoTag := 'Antarctica';
              061: LGeoTag := 'Oceania';
              062: LGeoTag := 'Asunción, Paraguay';
              063: LGeoTag := 'Madrid, Spain';
              064: LGeoTag := 'San Salvador, El Salvador';
              065: LGeoTag := 'La Paz, Bolivia';
              066: LGeoTag := 'Lisbon, Portugal';
              067: LGeoTag := 'Lima, Peru';
              068: LGeoTag := 'Prague, Czech Republic';
              069: LGeoTag := 'San José, Costa Rica';
              070: LGeoTag := 'Mexico City, Mexico';
              071: LGeoTag := 'Montreal, Canada';
              072: LGeoTag := 'Auckland, New Zealand';
              073: LGeoTag := 'Nairobi, Kenya';
              074: LGeoTag := 'Nashville, USA';
              075: LGeoTag := 'Managua, Nicaragua';
              076: LGeoTag := 'Oslo, Norway';
              077: LGeoTag := 'Salt Lake City, USA';
              078: LGeoTag := 'Bratislava, Slovakia';
              079: LGeoTag := 'Bordeaux, France';
              080: LGeoTag := 'Tegucigalpa, Honduras';
              081: LGeoTag := 'Minneapolis, USA';
              082: LGeoTag := 'South America';
              083: LGeoTag := 'Magadan, Russia';
              084: LGeoTag := 'Brussels, Belgium';
              085: LGeoTag := 'Lagos, Nigeria';
              086: LGeoTag := 'New York, USA';
              087: LGeoTag := 'San Diego, USA';
              088: LGeoTag := 'São Paulo, Brazil';
              089: LGeoTag := 'Charlotte, USA';
              090: LGeoTag := 'Boston, USA';
              091: LGeoTag := 'Suva, Fiji';
              092: LGeoTag := 'Kiev, Ukraine';
              093: LGeoTag := 'Doha, Qatar';
              094: LGeoTag := 'Maracaibo, Venezuela';
              095: LGeoTag := 'Ashgabat, Turkmenistan';
              096: LGeoTag := 'Orlando, USA';
              097: LGeoTag := 'Memphis, USA';
              098: LGeoTag := 'Dushanbe, Tajikistan';
              099: LGeoTag := 'Kuwait City, Kuwait';
              100: LGeoTag := 'Reykjavik, Iceland';
              101: LGeoTag := 'Philadelphia, USA';
              102: LGeoTag := 'Antananarivo, Madagascar';
              103: LGeoTag := 'Almaty, Kazakhstan';
              104: LGeoTag := 'Perm, Russia';
              105: LGeoTag := 'Kansas City, USA';
              106: LGeoTag := 'Port Louis, Mauritius';
              107: LGeoTag := 'Dallas, USA';
              108: LGeoTag := 'Irkutsk, Russia';
              109: LGeoTag := 'Guadalajara, Mexico';
              110: LGeoTag := 'Havana, Cuba';
              111: LGeoTag := 'Rosario, Argentina';
              112: LGeoTag := 'Atlanta, USA';
              113: LGeoTag := 'Mumbai, India';
              114: LGeoTag := 'Gaborone, Botswana';
              115: LGeoTag := 'Cape Town, South Africa';
              116: LGeoTag := 'Baku, Azerbaijan';
              117: LGeoTag := 'Milwaukee, USA';
              118: LGeoTag := 'Sofia, Bulgaria';
              119: LGeoTag := 'Los Angeles, USA';
              120: LGeoTag := 'Sochi, Russia';
              121: LGeoTag := 'Athens, Greece';
              122: LGeoTag := 'Yerevan, Armenia';
              123: LGeoTag := 'Yakutsk, Russia';
              124: LGeoTag := 'Damascus, Syria';
              125: LGeoTag := 'Georgetown, Guyana';
              126: LGeoTag := 'Copenhagen, Denmark';
              127: LGeoTag := 'Tokyo, Japan';
              128: LGeoTag := 'Berlin, Germany';
              129: LGeoTag := 'Ho Chi Minh City, Vietnam';
              130: LGeoTag := 'San Juan, Puerto Rico';
              131: LGeoTag := 'Accra, Ghana';
              132: LGeoTag := 'Chihuahua, Mexico';
              133: LGeoTag := 'Saint Petersburg, Russia';
              134: LGeoTag := 'Vladivostok, Russia';
              135: LGeoTag := 'Bern, Switzerland';
              136: LGeoTag := 'Anchorage, Alaska, USA';
              137: LGeoTag := 'St. Louis, USA';
              138: LGeoTag := 'Paris, France';
              139: LGeoTag := 'Texas, USA';
              140: LGeoTag := 'Melbourne, Australia';
              141: LGeoTag := 'Harare, Zimbabwe';
              142: LGeoTag := 'Europe';
              143: LGeoTag := 'San Antonio, USA';
              144: LGeoTag := 'Medellín, Colombia';
              145: LGeoTag := 'Beijing, China';
              146: LGeoTag := 'Miami, USA';
              147: LGeoTag := 'Rostov-on-Don, Russia';
              148: LGeoTag := 'Muscat, Oman';
              149: LGeoTag := 'Taipei, Taiwan';
              150: LGeoTag := 'Baghdad, Iraq';
              151: LGeoTag := 'Paramaribo, Suriname';
              152: LGeoTag := 'Santiago, Chile';
              153: LGeoTag := 'Tashkent, Uzbekistan';
              154: LGeoTag := 'Apia, Samoa';
              155: LGeoTag := 'Lusaka, Zambia';
              156: LGeoTag := 'Dakar, Senegal';
              157: LGeoTag := 'Bishkek, Kyrgyzstan';
              158: LGeoTag := 'Istanbul, Turkey';
              159: LGeoTag := 'Jakarta, Indonesia';
              160: LGeoTag := 'Warsaw, Poland';
              161: LGeoTag := 'Nantes, France';
              162: LGeoTag := 'Nizhny Novgorod, Russia';
              163: LGeoTag := 'Budapest, Hungary';
              164: LGeoTag := 'Novosibirsk, Russia';
              165: LGeoTag := 'Hong Kong';
              166: LGeoTag := 'Rennes, France';
              167: LGeoTag := 'Santa Cruz, Bolivia';
              168: LGeoTag := 'Vancouver, Canada';
              169: LGeoTag := 'Indianapolis, USA';
              170: LGeoTag := 'Nouméa, New Caledonia';
              171: LGeoTag := 'Chicago, USA';
              172: LGeoTag := 'Samara, Russia';
              173: LGeoTag := 'Puebla, Mexico';
              174: LGeoTag := 'Barcelona, Spain';
              175: LGeoTag := 'Geneva, Switzerland';
              176: LGeoTag := 'Omsk, Russia';
              177: LGeoTag := 'Quito, Ecuador';
              178: LGeoTag := 'Belmopan, Belize';
              179: LGeoTag := 'Detroit, USA';
              180: LGeoTag := 'Wellington, New Zealand';
              181: LGeoTag := 'Halifax, Canada';
              182: LGeoTag := 'Vienna, Austria';
              183: LGeoTag := 'Grenoble, France';
              184: LGeoTag := 'Port Moresby, Papua New Guinea';
              185: LGeoTag := 'Delhi, India';
              186: LGeoTag := 'Kuala Lumpur, Malaysia';
              187: LGeoTag := 'Riyadh, Saudi Arabia';
              188: LGeoTag := 'Bogotá, Colombia';
              189: LGeoTag := 'Murmansk, Russia';
              190: LGeoTag := 'Montevideo, Uruguay';
              191: LGeoTag := 'Winnipeg, Canada';
              192: LGeoTag := 'Las Vegas, USA';
              193: LGeoTag := 'London, UK';
              194: LGeoTag := 'Amman, Jordan';
              195: LGeoTag := 'Kazan, Russia';
              196: LGeoTag := 'Manila, Philippines';
              197: LGeoTag := 'California, USA';
              198: LGeoTag := 'Dubai, UAE';
              199: LGeoTag := 'Beirut, Lebanon';
              200: LGeoTag := 'Yekaterinburg, Russia';
            End;
            SetChildNodeValueText('geotag', LGeoTag);
            SetChildNodeValueText('caption', LPostJsonData.GetChildNodeValueText(['node', 'iphone_struct','caption', 'text'], ''));
            SetChildNodeValueInt32('like_count', LPostJsonData.GetChildNodeValueInt32(['node','edge_liked_by', 'count'], 0));
            SetChildNodeValueInt32('comment_count', LPostJsonData.GetChildNodeValueInt32(['node','edge_media_to_comment', 'count'], 0));
            SetChildNodeValueInt32('share_count ', LPostJsonData.GetChildNodeValueInt32(['node','iphone_struct', 'reshare_count'], 0));
            //--
            var LFilenameWithoutExt := ALStringReplaceW(Lfile, '.json', '', [RFignoreCase]);
            with AddChild('medias', ntarray) do begin
              For var i := 0 to maxint do begin
                var LIsVideo: boolean := true;
                var LImgFilename: String := LFilenameWithoutExt + ALIfThenW(i=0,'','_'+alinttostrW(i)) + '.mp4';
                if not TFile.Exists(LImgFilename) then begin
                  LIsVideo := False;
                  LImgFilename := LFilenameWithoutExt + ALIfThenW(i=0,'','_'+alinttostrW(i)) + '.jpg';
                end;
                if not TFile.Exists(LImgFilename) then begin
                  if i = 0 then continue
                  else break;
                end;
                With addchild(TALJsonNodeType.ntObject) do begin
                  if LIsVideo then begin
                    SetChildNodeValueBool('is_video', true);
                    AddChild('url').Text := 'https://github.com/MagicFoundation/Alcinoe/blob/master/Demos/ALFmxDynamicListBox/_Design/Showcase/' + ALExtractFileName(AnsiString(LImgFilename))+'?raw=true';
                    LImgFilename := LFilenameWithoutExt + ALIfThenW(i=0,'','_'+alinttostrW(i)) + '.jpg';
                    if not TFile.Exists(LImgFilename) then raise Exception.Create('Error 1F1F7C6C-9D49-479B-ABB9-B6B5A476CF23 | ' + Lfile);
                    AddChild('preview_url').Text := 'https://github.com/MagicFoundation/Alcinoe/blob/master/Demos/ALFmxDynamicListBox/_Design/Showcase/' + ALExtractFileName(AnsiString(LImgFilename))+'?raw=true';
                    Var LBitmap := Tbitmap.create;
                    try
                      LBitmap.LoadFromFile(LImgFilename);
                      SetChildNodeValueInt32('width', LBitmap.Width);
                      SetChildNodeValueInt32('height', LBitmap.Height);
                    finally
                      ALfreeAndNil(Lbitmap);
                    end;
                  end
                  else begin
                    ResizeImageTo1080(LImgFilename);
                    AddChild('url').Text := 'https://github.com/MagicFoundation/Alcinoe/blob/master/Demos/ALFmxDynamicListBox/_Design/Showcase/' + ALExtractFileName(AnsiString(LImgFilename))+'?raw=true';
                    Var LBitmap := Tbitmap.create;
                    try
                      LBitmap.LoadFromFile(LImgFilename);
                      SetChildNodeValueInt32('width', LBitmap.Width);
                      SetChildNodeValueInt32('height', LBitmap.Height);
                    finally
                      ALfreeAndNil(Lbitmap);
                    end;
                  end;

                end;
              end;
              if childnodes.Count = 0 then
                raise Exception.Create('Error 045F3A93-8E39-4038-A4A7-45BED154EE41 | ' + Lfile);
            end;
            //--
            if not Tfile.exists(ALGetModulePathW + '：saved\'+String(lUserName)+'.jpg') then begin
              var LHttpClient := THTTPClient.Create;
              try
                var LFileStream := TFileStream.Create(ALGetModulePathW + '：saved\'+String(lUserName)+'.jpg', fmCreate);
                try
                  LHttpClient.Get(String(LPostJsonData.GetChildNodeValueText(['node', 'iphone_struct','user','profile_pic_url'], '')), LFileStream);
                finally
                  LFileStream.Free;
                end;
              finally
                LHttpClient.Free;
              end;
            end;
            SetChildNodeValueText('profile_pic_url', 'https://github.com/MagicFoundation/Alcinoe/blob/master/Demos/ALFmxDynamicListBox/_Design/Showcase/' + ansiString(lUserName)+'.jpg?raw=true');
          end;
        end;

        LResourceJsonData.SaveToJSONFile(ALGetModulePathW + 'data.json', [soNodeAutoIndent]);

      finally
        ALFreeAndNil(LResourceJsonData);
      end;

    finally
      //free the ImageMagickLibrary
      alFreeImageMagickLibrary;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
