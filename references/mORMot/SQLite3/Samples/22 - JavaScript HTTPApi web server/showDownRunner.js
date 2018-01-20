/**
* this function is called from TTestServer.Process method in case we point to *.md file
* loadFile here is a NATIVE (realisation in Delphi) function
* @param {String} pathToFile file to process
*/
function showDownRunner(pathToFile){
  var src = loadFile(pathToFile);
  var converter = new Showdown.converter();
  return converter.makeHtml(src);
}