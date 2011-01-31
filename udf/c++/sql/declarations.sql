/*****************************************
 * aludf_httpencode
 * Converts a string into a form that contains 
 * no values that are illegal in an HTTP message header.   
 * aludf_httpencode converts all characters in the parameter except (RFC 1738) 
 * for the letters A through Z (and a through z), numerals 0 through 9, 
 * the asterisk (*), dollar sign ($), exclamation point (!), at sign (@), 
 * period (.), underscore (_), single quote ('), comma (,) parentheses, 
 * and hyphen (-). Spaces are converted to plus characters (+), and all 
 * other characters are converted into hex values preceded by the 
 * percent sign (%).
 *****************************************/
DECLARE EXTERNAL FUNCTION aludf_httpencode
  CSTRING(8191) NULL
  RETURNS CSTRING(8191) FREE_IT
  ENTRY_POINT 'aludf_httpencode' MODULE_NAME 'aludf';
  
  
/*****************************************
 * aludf_utf8lowercase
 * UTF8LowerCase converts all characters in the given UTF8String 
 * to lower case. The conversion is not locale specific. 
 *****************************************/
DECLARE EXTERNAL FUNCTION aludf_utf8lowercase
  CSTRING(8191) NULL
  RETURNS CSTRING(8191) FREE_IT
  ENTRY_POINT 'aludf_utf8lowercase' MODULE_NAME 'aludf';  
  
  
/*****************************************
 * aludf_utf8uppercase
 * aludf_utf8uppercase converts all characters in the given UTF8String 
 * to upper case. The conversion is not locale specific. 
 *****************************************/
DECLARE EXTERNAL FUNCTION aludf_utf8uppercase
  CSTRING(8191) NULL
  RETURNS CSTRING(8191) FREE_IT
  ENTRY_POINT 'aludf_utf8uppercase' MODULE_NAME 'aludf';    
  
  
/*****************************************
 * aludf_utf8normalize
 * convert a string like L'été Sur La Plage 
 * in l-ete-sur-le-plage (only "-" and Lower Case 
 * Alpha Numeric char)
 *****************************************/
DECLARE EXTERNAL FUNCTION aludf_utf8normalize
  CSTRING(8191) NULL
  RETURNS CSTRING(8191) FREE_IT
  ENTRY_POINT 'aludf_utf8normalize' MODULE_NAME 'aludf';      
  
  
/*****************************************
 * aludf_zerotoone
 * return 1 if parameter is zero, else return
 * the parameter. usefull in situation like 
 * z = x / aludf_zerotoone(y)
 *****************************************/
DECLARE EXTERNAL FUNCTION aludf_zerotoone
  INTEGER
  RETURNS INTEGER BY VALUE
  ENTRY_POINT 'aludf_zerotoone' MODULE_NAME 'aludf';        
  
  
/*****************************************
 * aludf_utf8titlecase
 * the first letter of each word is capitalized,
 * the rest are lower case
 *****************************************/
DECLARE EXTERNAL FUNCTION aludf_utf8titlecase
  CSTRING(8191) NULL
  RETURNS CSTRING(8191) FREE_IT
  ENTRY_POINT 'aludf_utf8titlecase' MODULE_NAME 'aludf';        