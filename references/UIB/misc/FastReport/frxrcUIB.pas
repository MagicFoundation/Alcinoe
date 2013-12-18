{******************************************}
{                                          }
{             FastReport v4.x              }
{          Language resource file          }
{                                          }
{         Copyright (c) 2005-2007          }
{            by Pierre Yager.              }
{                                          }
{******************************************}

unit frxrcUIB;

interface

implementation

uses frxRes;

const resStr =
'obUIBComps=UIB Components' + #13#10 +
'obUIBDB=UIB Database' + #13#10 +
'obUIBT=UIB Transaction' + #13#10 +
'obUIBQ=UIB Query' + #13#10 +
'';

initialization
  frxResources.AddStrings(resStr);

end.
