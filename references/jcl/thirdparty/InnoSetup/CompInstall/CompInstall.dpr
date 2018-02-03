library CompInstall;

{$IF CompilerVersion >= 21.0} // 2010+
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

uses
  CompInst in 'CompInst.pas';

{$R *.res}

exports
  compinst_init,

  compinst_isDelphiInstalled,
  compinst_isBCBInstalled,
  compinst_isBDSInstalled,

  compinst_installDelphiDesignPackage,
  compinst_installBCBDesignPackage,
  compinst_uninstallDelphiDesignPackage,
  compinst_uninstallBCBDesignPackage,
  compinst_uninstallDelphiDesignPackagesPrefixed,
  compinst_uninstallBCBDesignPackagesPrefixed,

  compinst_installDelphiExpert,
  compinst_installBCBExpert,
  compinst_uninstallDelphiExpert,
  compinst_uninstallBCBExpert,
  compinst_uninstallDelphiExpertsPrefixed,
  compinst_uninstallBCBExpertsPrefixed,

  compinst_addDelphiSearchPaths,
  compinst_addBCBSearchPaths,
  compinst_removeDelphiSearchPaths,
  compinst_removeBCBSearchPaths;

begin

end.



