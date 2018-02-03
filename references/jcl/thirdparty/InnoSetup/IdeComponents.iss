[Components]
; IDE selection
Name: "IDE"; Description: "Install for IDE"
#ifdef Include_Delphi6
Name: "IDE\Delphi6"; Description: "Install for Delphi 6"; Types: full prefered; Check: IsDelphiInstalled(6)
#endif
#ifdef Include_BCB6
Name: "IDE\BCB6"; Description: "Install for C++Builder 6"; Types: full prefered; Check: IsBCBInstalled(6)
#endif
#ifdef Include_Delphi7
Name: "IDE\Delphi7"; Description: "Install for Delphi 7"; Types: full prefered; Check: IsDelphiInstalled(7)
#endif
#ifdef Include_Delphi9
Name: "IDE\Delphi9"; Description: "Install for Delphi 2005"; Types: full prefered; Check: IsDelphiInstalled(9)
#endif
#ifdef Include_Delphi10
Name: "IDE\Delphi10"; Description: "Install for Delphi/C++Builder 2006"; Types: full prefered; Check: IsDelphiInstalled(10)
#endif
#ifdef Include_Delphi11
Name: "IDE\Delphi11"; Description: "Install for RAD Studio 2007"; Types: full prefered; Check: IsDelphiInstalled(11)
#endif
#ifdef Include_Delphi12
Name: "IDE\Delphi12"; Description: "Install for RAD Studio 2009"; Types: full prefered; Check: IsDelphiInstalled(12)
#endif
#ifdef Include_Delphi14
Name: "IDE\Delphi14"; Description: "Install for RAD Studio 2010"; Types: full prefered; Check: IsDelphiInstalled(14)
#endif
#ifdef Include_Delphi15
Name: "IDE\Delphi15"; Description: "Install for RAD Studio XE"; Types: full prefered; Check: IsDelphiInstalled(15)
#endif
#ifdef Include_Delphi16
Name: "IDE\Delphi16"; Description: "Install for RAD Studio XE2"; Types: full prefered; Check: IsDelphiInstalled(16)
#endif
#ifdef Include_Delphi17
Name: "IDE\Delphi17"; Description: "Install for RAD Studio XE3"; Types: full prefered; Check: IsDelphiInstalled(17)
#endif
#ifdef Include_Delphi18
Name: "IDE\Delphi18"; Description: "Install for RAD Studio XE4"; Types: full prefered; Check: IsDelphiInstalled(18)
#endif
#ifdef Include_Delphi19
Name: "IDE\Delphi19"; Description: "Install for RAD Studio XE5"; Types: full prefered; Check: IsDelphiInstalled(19)
#endif
#ifdef Include_Delphi20
Name: "IDE\Delphi20"; Description: "Install for RAD Studio XE6"; Types: full prefered; Check: IsDelphiInstalled(20)
#endif
#ifdef Include_Delphi21
Name: "IDE\Delphi21"; Description: "Install for RAD Studio XE7"; Types: full prefered; Check: IsDelphiInstalled(21)
#endif
#ifdef Include_Delphi22
Name: "IDE\Delphi22"; Description: "Install for RAD Studio XE8"; Types: full prefered; Check: IsDelphiInstalled(22)
#endif
#ifdef Include_Delphi23
Name: "IDE\Delphi23"; Description: "Install for RAD Studio 10 Seattle"; Types: full prefered; Check: IsDelphiInstalled(23)
#endif
#ifdef Include_Delphi24
Name: "IDE\Delphi24"; Description: "Install for RAD Studio 10.1 Berlin"; Types: full prefered; Check: IsDelphiInstalled(24)
#endif
#ifdef Include_Delphi25
Name: "IDE\Delphi25"; Description: "Install for RAD Studio 10.2"; Types: full prefered; Check: IsDelphiInstalled(25)
#endif
