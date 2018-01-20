SpiderMonkey execution sample
=============================
Point your browser to <http://localhost:888/root/readme.md> and see the *Markdown*
template rendered using *SpiderMonkey*.

This is not a production HTTP server, just a sample of *SpiderMonkey* usage within
*mORMot*.

# Features shown:
* `TSMEngineManager` for thread-safe engine creation;
* Add external JavaScript libs to engine (`showdown.js` - javascript port of the *Markdown* 
library);
* Add native function to engine (`loadFile`);
* Garbage collection management.

Take a look at the following methods: 

    procedure TTestServer.DoOnNewEngine(const Engine: TSMEngine);
    function TTestServer.Process(Ctxt: THttpServerRequest): cardinal; (last lines)
