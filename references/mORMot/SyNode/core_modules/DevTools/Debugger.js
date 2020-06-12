// original FireFox implementation is in:
// git clone https://github.com/mozilla/gecko-dev.git
// cd gecko-dev/devtools/server 
// FF debugger Protocol: https://searchfox.org/mozilla-central/source/devtools/docs/backend/protocol.md
import * as DevToolsUtils from 'DevTools/DevToolsUtils.js';
import {JSPropertyProvider} from 'DevTools/js-property-provider.js';
import {ObjectActorPreviewers} from 'DevTools/ObjectActorPreviewers.js';
import {stringify} from 'DevTools/stringify.js';

let actorManager = null,
    dbg_binding = process.binding('debugger');

const LONG_STRING_INITIAL_LENGTH = 1000;

class ActorManager {
    constructor(threadID) {
        this.threadID = threadID;
    }
    init(){
        this.console = new ConsoleActor('console' + this.threadID);
        this.addon = new AddonActor('thread_' + dbg_binding.threadId);
    }
    getActor(actorName){
        let actor = this;
        actorName.split('.').forEach(function(elem){
            if (actor)
                actor = actor[elem];
        });
        return actor;
    }
}

class Actor {
    constructor (actorName, parent) {
        parent = parent ? parent : actorManager;
        this._actor = actorName;
        this._parent = parent;
        this._grips = 0;
        this._gripDepth = 0;
        parent[actorName] = this;
    }
    get fullActor() {
        let res = this._parent.fullActor;
        return res ? res + '.' + this._actor : this._actor;
    }
    getGrip(value) {
        var type = typeof(value);
        if (type === "boolean")
            return value;
        if (type === "number") {
            if (value === Infinity) {
                return { type: "Infinity" };
            } else if (value === -Infinity) {
                return { type: "-Infinity" };
            } else if (Number.isNaN(value)) {
                return { type: "NaN" };
            } else if (1 / value === -Infinity) {
                return { type: "-0" };
            }
            return value;
        }
        if (type === "string") {
            if (value.length > LONG_STRING_INITIAL_LENGTH)
                return (new LStrActor(value, this))._resp;
            else
                return value;
        }
        if (type === "undefined")
            return { type: "undefined" };
        if (type === "object") {
            if (value === null) {
                return { type: "null" };
            }
            if (value.optimizedOut ||
                value.uninitialized ||
                value.missingArguments) {
                // The slot is optimized out, an uninitialized binding, or
                // arguments on a dead scope
                return {
                    type: "null",
                    optimizedOut: value.optimizedOut,
                    uninitialized: value.uninitialized,
                    missingArguments: value.missingArguments
                };
            }
            return (new ObjectActor(value, this))._resp;
        }
        if (type === "symbol") {
            let form = {
                type: "symbol"
            };
            let name = getSymbolName(value);
            if (name !== undefined) {
                form.name = this.getGrip(name);
            }
            return form;
        }
        throw new Error('getGrip failed: ' + type + ' : ' + value);
        //return null;
    }
    createProtocolCompletionValue(aCompletion) {
        let protoValue = {};
        if (aCompletion == null) {
            protoValue.terminated = true;
        } else if ("return" in aCompletion) {
            protoValue.return = this.getGrip(aCompletion.return);
        } else if ("throw" in aCompletion) {
            protoValue.throw = this.getGrip(aCompletion.throw);
        } else {
            protoValue.return = this.getGrip(aCompletion.yield);
        }
        return protoValue;
    }
}

const symbolProtoToString = Symbol.prototype.toString;
function getSymbolName(symbol) {
  const name = symbolProtoToString.call(symbol).slice("Symbol(".length, -1);
  return name || undefined;
}

class ConsoleActor extends Actor {
    constructor (actorName) {
        super(actorName);
        this.welcomeShowed = false;

        this.consoleCommands = {
            help: "Welcome to SM debugger console",
            test: {
                command: function(){
                    return '!!!test called!!!';
                },
                description: "Test function for debugger. Returns '!!!test called!!!'"
            }
        }

    }
    startListeners(aRequest) {
        return {
            "startedListeners": ["ConsoleAPI"],
            "nativeConsoleAPI": true,
            "traits": {evaluateJSAsync: true}
        };
    }
    stopListeners(aRequest) {
        this.welcomeShowed = false;
        return {
            stoppedListeners: []
        }
    }
    setPreferences(aRequest) {
        return { updated: Object.keys(aRequest.preferences) };
    }
    getCachedMessages(aRequest) {
        if(!this.welcomeShowed) {
            newConsoleMessage(`Connected... Type ? for help`);
            this.welcomeShowed = true;
        }
        return {
            messages: []
        };
    }
    evaluateJS(aRequest) {
        let input = aRequest.text;
        let timestamp = Date.now();

        let evalOptions = {
            bindObjectActor: aRequest.bindObjectActor,
            frameActor: aRequest.frameActor,
            url: aRequest.url,
            selectedNodeActor: aRequest.selectedNodeActor,
            selectedObjectActor: aRequest.selectedObjectActor
        };

        let evalInfo = this._evalWithDebugger(input, evalOptions);
        let evalResult = evalInfo.result;
        //let helperResult = evalInfo.helperResult;

        let result, errorMessage, errorGrip = null;
        if (evalResult) {
            if ("return" in evalResult) {
                result = evalResult.return;
            } else if ("yield" in evalResult) {
                result = evalResult.yield;
            } else if ("throw" in evalResult) {
                let error = evalResult.throw;
                errorGrip = this.getGrip(error);
				errorMessage = String(error); 
				
				if (typeof error === "object" && error !== null) {
				  try {
					errorMessage = DevToolsUtils.callPropertyOnObject(error, "toString");
				  } catch (e) {
					// If the debuggee is not allowed to access the "toString" property
					// of the error object, calling this property from the debuggee's
					// compartment will fail. The debugger should show the error object
					// as it is seen by the debuggee, so this behavior is correct.
					//
					// Unfortunately, we have at least one test that assumes calling the
					// "toString" property of an error object will succeed if the
					// debugger is allowed to access it, regardless of whether the
					// debuggee is allowed to access it or not.
					//
					// To accomodate these tests, if calling the "toString" property
					// from the debuggee compartment fails, we rewrap the error object
					// in the debugger's compartment, and then call the "toString"
					// property from there.
					if (typeof error.unsafeDereference === "function") {
					  errorMessage = error.unsafeDereference().toString();
					}
				  }				
				}  
/*				
                // XXXworkers: Calling unsafeDereference() returns an object with no
                // toString method in workers. See Bug 1215120.
                let unsafeDereference = error && (typeof error === "object") &&
                    error.unsafeDereference();
                errorMessage = unsafeDereference && unsafeDereference.toString
                    ? unsafeDereference.toString()
                    : "" + error;
*/	
            }
        }

        // If a value is encountered that the debugger server doesn't support yet,
        // the console should remain functional.
        let resultGrip;
        try {
            //resultGrip = result === undefined ? result : this.getGrip(result);
            resultGrip = result === undefined ? result : (aRequest.frameActor ? actorManager.getActor(aRequest.frameActor) : this).getGrip(result);

        } catch (e) {
            errorMessage = e;
        }

        this._lastConsoleInputEvaluation = result;

        return {
            input: input,
            result: resultGrip,
            timestamp: timestamp,
            exception: errorGrip,
            exceptionMessage: errorMessage===undefined ? errorMessage : this.getGrip(errorMessage),
            //helperResult: helperResult
            helperResult: null
        };

    }
    evaluateJSAsync(aRequest) {
        // We want to be able to run console commands without waiting
        // for the first to return (see Bug 1088861).

        // First, send a response packet with the id only.
        let resultID = Date.now();
        dbg_binding.send({
            from: this.fullActor,
            resultID: resultID
        });

        // Then, execute the script that may pause.
        let response = this.evaluateJS(aRequest);
        response.resultID = resultID;

        response.type = "evaluationResult";

        return response;
    }
    _evalWithDebugger(aString, aOptions = {}){
        let trimmedString = aString.trim();
        // The help function needs to be easy to guess, so we make the () optional.
        if (trimmedString == "help" || trimmedString == "?") {
            aString = "help()";
        }

        // Add easter egg for console.mihai().
        if (trimmedString == "console.mihai()" || trimmedString == "console.mihai();") {
            aString = "\"http://incompleteness.me/blog/2015/02/09/console-dot-mihai/\"";
        }

        // Find the Debugger.Frame of the given FrameActor.
        let frame = null, frameActor = null;
        if (aOptions.frameActor) {
            frameActor = actorManager.getActor(aOptions.frameActor);
            if (frameActor) {
                frame = frameActor._frame;
            }
            else {
                DevToolsUtils.reportException("evalWithDebugger",
                    Error("The frame actor was not found: " + aOptions.frameActor)
                );
            }
        }

        // If we've been given a frame actor in whose scope we should evaluate the
        // expression, be sure to use that frame's Debugger (that is, the JavaScript
        // debugger's Debugger) for the whole operation, not the console's Debugger.
        // (One Debugger will treat a different Debugger's Debugger.Object instances
        // as ordinary objects, not as references to be followed, so mixing
        // debuggers causes strange behaviors.)
        //let dbg = frame ? frameActor.threadActor.dbg : this.dbg;
        let dbg = actorManager.thread.dbg;
        let dbgWindow = dbg.makeGlobalObjectReference(dbg_binding.global);

        // If we have an object to bind to |_self|, create a Debugger.Object
        // referring to that object, belonging to dbg.
        let bindSelf = null;
        if (aOptions.bindObjectActor || aOptions.selectedObjectActor) {
            //throw new Error('todo'+aString+'|'+JSON.stringify(aOptions));
            let objActor = actorManager.getActor(aOptions.bindObjectActor || aOptions.selectedObjectActor);
            if (objActor) {
                let jsObj = objActor._obj.unsafeDereference();
                bindSelf = dbg.findAllGlobals()[0].makeDebuggeeValue(jsObj);
            }
        }
        let bindings = {},
            command,
            res = [],
            maxlen = 0;


        for (command in this.consoleCommands) {
            if (command !== 'help' && this.consoleCommands[command].command) {
                bindings[command] = dbg.findAllGlobals()[0].makeDebuggeeValue(this.consoleCommands[command].command);
                let _command = typeof this.consoleCommands[command].command === 'function' ? command + '()' : command;
                if (this.consoleCommands[command].description) {
                    maxlen = maxlen > _command.length ? maxlen : _command.length
                }
            }
        }

        if ((this.consoleCommands.help) && (typeof this.consoleCommands.help === 'string' || typeof this.consoleCommands.help === 'function')) {
            res.push(typeof this.consoleCommands.help === 'string' ? this.consoleCommands.help : this.consoleCommands.help() + ' Commands:');
        }
        for (command in this.consoleCommands) {
            if (command !== 'help' && this.consoleCommands[command].command && this.consoleCommands[command].description) {
                let _command = typeof this.consoleCommands[command].command === 'function' ? command + '()' : command;
                res.push(_command + ' '.repeat(maxlen - _command.length) +
                    '\t' + this.consoleCommands[command].description
                );
            }
        }

        bindings.help = dbg.findAllGlobals()[0].makeDebuggeeValue(() => res.join('\r\n'));

/*
        // Get the Web Console commands for the given debugger window.
        let helpers = this._getWebConsoleCommands(dbgWindow);
        let bindings = helpers.sandbox;*/
        if (bindSelf) {
            bindings._self = bindSelf;
        }

        if (aOptions.selectedNodeActor) {
            throw new Error('todo');/*
            let actor = this.conn.getActor(aOptions.selectedNodeActor);
            if (actor) {
                helpers.selectedNode = actor.rawNode;
            }*/
        }
/*
        // Check if the Debugger.Frame or Debugger.Object for the global include
        // $ or $$. We will not overwrite these functions with the Web Console
        // commands.
        let found$ = false, found$$ = false;
        if (frame) {
            let env = frame.environment;
            if (env) {
                found$ = !!env.find("$");
                found$$ = !!env.find("$$");
            }
        }
        else {
            found$ = !!dbgWindow.getOwnPropertyDescriptor("$");
            found$$ = !!dbgWindow.getOwnPropertyDescriptor("$$");
        }

        let $ = null, $$ = null;
        if (found$) {
            $ = bindings.$;
            delete bindings.$;
        }
        if (found$$) {
            $$ = bindings.$$;
            delete bindings.$$;
        }

        // Ready to evaluate the string.
        helpers.evalInput = aString;
*/
        let evalOptions;
        if (typeof aOptions.url == "string") {
            evalOptions = { url: aOptions.url };
        }

        let result;
        if (frame) {
            result = frame.evalWithBindings(aString, bindings, evalOptions);
        }
        else {
            result = dbgWindow.executeInGlobalWithBindings(aString, bindings, evalOptions);
        }

/*        let helperResult = helpers.helperResult;
        delete helpers.evalInput;
        delete helpers.helperResult;
        delete helpers.selectedNode;

        if ($) {
            bindings.$ = $;
        }
        if ($$) {
            bindings.$$ = $$;
        }

        if (bindings._self) {
            delete bindings._self;
        }
*/
        return {
            result: result,
//            helperResult: helperResult,
            dbg: dbg,
            frame: frame
//            window: dbgWindow
        };
    }
    autocomplete(aRequest)
    {
        let frameActorId = aRequest.frameActor;
        let dbgObject = null;
        let environment = null;
        let hadDebuggee = false;

        // This is the case of the paused debugger
        if (frameActorId) {
            let frameActor = actorManager.getActor(frameActorId);
            if (frameActor) {
                let frame = frameActor._frame;
                environment = frame.environment;
            }
            else {
                DevToolsUtils.reportException("onAutocomplete",
                    Error("The frame actor was not found: " + frameActorId));
            }
        }
        // This is the general case (non-paused debugger)
        else {
/*            hadDebuggee = this.dbg.hasDebuggee(this.evalWindow);
            dbgObject = this.dbg.addDebuggee(this.evalWindow);*/
            let dbg = actorManager.thread.dbg;
            dbgObject =  dbg.makeGlobalObjectReference(dbg_binding.global);
        }

        let result = JSPropertyProvider(dbgObject, environment, aRequest.text,
                aRequest.cursor, frameActorId) || {};

        //if (!hadDebuggee && dbgObject) {
        //    this.dbg.removeDebuggee(this.evalWindow);
        //}

        let matches = result.matches || [];
        let reqText = aRequest.text.substr(0, aRequest.cursor);

        // We consider '$' as alphanumerc because it is used in the names of some
        // helper functions.
        let lastNonAlphaIsDot = /[.][a-zA-Z0-9$]*$/.test(reqText);
/*        if (!lastNonAlphaIsDot) {
            if (!this._webConsoleCommandsCache) {
                let helpers = {
                    sandbox: Object.create(null)
                };
                addWebConsoleCommands(helpers);
                this._webConsoleCommandsCache =
                    Object.getOwnPropertyNames(helpers.sandbox);
            }
            matches = matches.concat(this._webConsoleCommandsCache
                .filter(n => n.startsWith(result.matchProp)));
        }*/

        return {
//            from: this.actorID,
            matches: matches.sort(),
            matchProp: result.matchProp
        };
    }
    clearMessagesCache(aRequest){
        return {}
    }
}

class ThreadActor extends Actor {
    constructor() {
        super("thread");
        this.dbg = new Debugger(dbg_binding.global);
        this.dbg.enabled = false;
    }
    attach(aRequest) {
        this.dbg.enabled = true;
        this.dbg.onNewScript = this._onNewScript;
        this.dbg.onDebuggerStatement = this._onDebuggerStatement;
        this._options = {};
        this._prevPosition = {
            frame: null,
            line: null
        };

        return {
            "type": "paused",
            "why": {"type": "attached"}
        }
    }
    detach(aRequest) {
        this.dbg.enabled = false;
        dbg_binding.paused = false;
        return {
            type: "detached"
        };
    }
    sources(aRequest) {
        let sources = new SourcesActor(),
            res = [];
        this.dbg.findScripts().forEach(function (script) {
            let resp = sources._addSource(script.source);
            if (resp)
                res.push(resp);
        });
        return {"sources": res};
    }
    frames(aRequest) {
        let resp = [],
            frame = actorManager.getActor('frame');
        while (frame) {
            resp.push(frame._resp);
            frame = frame.older;
        }
        return {
            frames: resp
        };
    }
    reconfigure(aRequest) {
        return {};
    }
    clientEvaluate(aRequest) {
        if (!dbg_binding.paused) {
            return { error: "wrongState",
                message: "Debuggee must be paused to evaluate code." };
        }

        let frame = actorManager.getActor(aRequest.frame)._frame;
        if (!frame) {
            return { error: "unknownFrame",
                message: "Evaluation frame not found" };
        }

        if (!frame.environment) {
            return { error: "notDebuggee",
                message: "cannot access the environment of this frame." };
        }

        let youngest = actorManager.getActor('frame');

        // Put ourselves back in the running state and inform the client.
        //let resumedaRequest = this._resumed();
        //this.conn.send(resumedaRequest);
        dbg_binding.send({
            from: this.fullActor,
            type: "resumed"
        });

        // Run the expression.
        // XXX: test syntax errors
        let completion = frame.eval(aRequest.expression);

        // Return back to our previous pause's event loop.
        return this._paused(youngest, {type: "clientEvaluated",
            frameFinished: this.createProtocolCompletionValue(completion) });
    }
    interrupt(aRequest) {
        /*if (this.state == "exited") {
            return { type: "exited" };
        } else if (this.state == "paused") {*/
        if (dbg_binding.paused) {
            // TODO: return the actual reason for the existing pause.
            return { type: "paused", why: { type: "alreadyPaused" } };
        } /*else if (this.state != "running") {
            return { error: "wrongState",
                message: "Received interrupt request in " + this.state +
                " state." };
        }*/

        try {
            // If execution should pause just before the next JavaScript bytecode is
            // executed, just set an onEnterFrame handler.
            if (aRequest.when == "onNext") {
                let youngest = this.dbg.getNewestFrame();
                if (youngest) {
                    youngest.onStep = function() {
                        return actorManager.thread._pauseAndRespond(this, {type: "interrupted", onNext: true});
                    }
                } else {
                    let onEnterFrame = (aFrame) => {
                        return this._pauseAndRespond(aFrame, {type: "interrupted", onNext: true});
                    };
                    this.dbg.onEnterFrame = onEnterFrame;
                }
                return { type: "willInterrupt" };
            }

            // If execution should pause immediately, just put ourselves in the paused
            // state.
            //let resp = this._paused(undefined, { type: "interrupted" });
            this._pauseAndRespond(this.dbg.getNewestFrame(), { type: "interrupted"});
            return {};
            //if (!aRequest) {
            //    return { error: "notInterrupted" };
            //}
            //aRequest.why = { type: "interrupted" };

            //// Send the response to the interrupt request now (rather than
            //// returning it), because we're going to start a nested event loop
            //// here.
            //this.conn.send(aRequest);
            //
            //// Start a nested event loop.
            //this._pushThreadPause();
            //
            //// We already sent a response to this request, don't send one
            //// now.
            //return null;
        } catch (e) {
            DevToolsUtils.reportException('thread.interrupt', e);
            return { error: "notInterrupted", message: e.toString() };
        }
    }
    resume(aRequest) {
        dbg_binding.paused = false;
        if (aRequest.resumeLimit) {
            this._handleResumeLimit(aRequest);
        } else {
            this._clearSteppingHooks(this.dbg.getNewestFrame());
        }

        this._options.pauseOnExceptions = aRequest.pauseOnExceptions;
        this._options.ignoreCaughtExceptions = aRequest.ignoreCaughtExceptions;
        this._maybePauseOnExceptions();

        return {
            "type": "resumed"
        }
    }
    _handleResumeLimit(aRequest) {

        let steppingType = aRequest.resumeLimit.type;
        if (["break", "step", "next", "finish"].indexOf(steppingType) == -1) {
            return {
                error: "badParameterType",
                message: "Unknown resumeLimit type"
            };
        }

        //const generatedLocation = actorManager.getActor('frame')._frame;
        //return this.sources.getOriginalLocation(generatedLocation)
        //    .then(originalLocation => {
        //        const { onEnterFrame, onPop, onStep } = this._makeSteppingHooks(originalLocation,
        //            steppingType);

        // Make sure there is still a frame on the stack if we are to continue
        // stepping.
        let stepFrame = actorManager.getActor('frame')._frame;
        if (stepFrame.reportedPop) {
            stepFrame = stepFrame.older;
        }
        if (!stepFrame || !stepFrame.script) {
            stepFrame = stepFrame = null;
        }
        //let stepFrame = this._getNextStepFrame(this.youngestFrame);
        if (stepFrame) {
            switch (steppingType) {
                case "step":
                    this.dbg.onEnterFrame = this._onEnterFrame;
                // Fall through.
                case "break":
                case "next":
                    if (steppingType === "break") {
                        this._prevPosition = {
                            frame: null,
                            line: null
                        }
                    }
                    if (stepFrame.script) {
                        stepFrame.onStep = this._onStep;
                    }
                //stepFrame.onPop = onPop;
                //break;
                case "finish":
                    stepFrame.onPop = this._onPop;
            }
        }
        //
        //    return true;
        //});

    }
    _clearSteppingHooks(frame){
        if (frame && frame.live) {
            while (frame) {
                frame.onStep = undefined;
                frame.onPop = undefined;
                frame = frame.older;
            }
        }
    }
    _maybePauseOnExceptions() {
        if (this._options.pauseOnExceptions) {
            this.dbg.onExceptionUnwind = this._onExceptionUnwind.bind(this);
        }
    }
    _paused(frameActor, why){
        let frame = frameActor ? frameActor._frame : null;
        // Clear stepping hooks.
        this.dbg.onEnterFrame = undefined;
        this.dbg.onExceptionUnwind = undefined;
        if (frame) {
            frame.onStep = undefined;
            frame.onPop = undefined;
        }
        return {
            "type": "paused",
            "frame": frameActor ? frameActor._resp : undefined,
            "poppedFrames": [],
            "why": why
        };
    }
    _onNewScript(script, global) {
        let resp = actorManager.source._addSource(script.source);
        if (resp){
            //resp.from = actorManager.thread.fullActor;
            dbg_binding.send({
                from: actorManager.thread.fullActor,
                type: "newSource",
                source: resp
            });
        }
    }
    _onDebuggerStatement(frame) {
        return actorManager.thread._pauseAndRespond(frame, {"type": "debuggerStatement"});
    }
    _onEnterFrame(frame) {
        return actorManager.thread._pauseAndRespond(frame, {"type": "resumeLimit"});
    }
    _onExceptionUnwind(aFrame, aValue) {
        let willBeCaught = false;
        for (let frame = aFrame; frame != null; frame = frame.older) {
            if (frame.script.isInCatchScope(frame.offset)) {
                willBeCaught = true;
                break;
            }
        }

        if (willBeCaught && this._options.ignoreCaughtExceptions) {
            return undefined;
        }

        //const generatedLocation = this.sources.getFrameLocation(aFrame);
        //const { sourceActor } = this.unsafeSynchronize(this.sources.getOriginalLocation(
        //    generatedLocation));
        //const url = sourceActor ? sourceActor.url : null;
        //
        //if (this.sources.isBlackBoxed(url)) {
        //    return undefined;
        //}

        //try {
        //    let aRequest = this._paused(aFrame);
        //    if (!aRequest) {
        //        return undefined;
        //    }
        //
        //    aRequest.why = { type: "exception",
        //        exception: this.getGrip(aValue)
        //    };
        //    this.conn.send(aRequest);
        //
        //    this._pushThreadPause();
        //} catch(e) {
        //    reportError(e, "Got an exception during TA_onExceptionUnwind: ");
        //}
        //
        //return undefined;
        return this._pauseAndRespond(aFrame, function(frameActor){
            return {
                type: "exception",
                exception: frameActor.getGrip(aValue)
            };
        })
    }
    _onStep(){
        return actorManager.thread._pauseAndRespond(this, {"type": "resumeLimit"});
    }
    _onPop(aCompletion) {
        this.reportedPop = true;

        actorManager.thread._prevPosition = {
            frame: null,
            line: null
        };

        return actorManager.thread._pauseAndRespond(this, function(frameActor){
            let why = {
                "type": "resumeLimit",
                frameFinished: {}
            };
            if (!aCompletion) {
                why.frameFinished.terminated = true;
            } else if (aCompletion.hasOwnProperty("return")) {
                why.frameFinished.return = frameActor.getGrip(aCompletion.return);
            } else if (aCompletion.hasOwnProperty("yield")) {
                why.frameFinished.return = frameActor.getGrip(aCompletion.yield);
            } else {
                why.frameFinished.throw = frameActor.getGrip(aCompletion.throw);
            }
            return why;
        });
    }
    _isPrevPosition(frame) {
        return (this._prevPosition.frame !== frame ||
            this._prevPosition.line !== frame.script.getOffsetLocation(frame.offset).lineNumber);
    }
    _pauseAndRespond(frame, why){
        if (!dbg_binding.listen)
            return undefined;
        if (frame && !actorManager.thread._isPrevPosition(frame))
            return undefined;
        dbg_binding.paused = true;
        if (frame) {
            actorManager.thread._prevPosition = {
                frame: frame,
                line: frame.script.getOffsetLocation(frame.offset).lineNumber
            };
        }
        let frameActor = frame ? new FrameActor(frame): null,
            msg = actorManager.thread._paused(frameActor, typeof(why)==="function" ? why(frameActor) : why);
        msg.from = this.fullActor;
        dbg_binding.send(msg);
        do {
            msg = dbg_binding.read();
            newMessage(msg);
            while (msg = dbg_binding.read(false)) {
                newConsoleMessage(msg);
            }
        } while (dbg_binding.paused);
        return undefined;
    }
}

class SourcesActor extends Actor {
    constructor () {
        super('source');
        this._sourcesMap = new Map()
        this.scriptCounterID = 1
    }
    _addSource(source) {
        if (this._sourcesMap.get(source))
            return undefined;
        else
            return new SourceActor(this.scriptCounterID++, source, this)._resp
    }
}

class SourceActor extends Actor {
    constructor (actorID, source, parent) {
        super(actorID, parent);
        parent._sourcesMap.set(source, this)
        this._source = source;
        this._breakpoints = {};
    }
    source() {
        return {
            "contentType": "text/javascript",
            "source": this.getGrip(this._source.text)
        }
    }
    setBreakpoint(aRequest){
        let { location: { line, column }, condition } = aRequest,
            breakpointActor = this._getOrCreateBreakpointActor(line, condition);
        return breakpointActor ? breakpointActor._resp : {};
    }
    _getOrCreateBreakpointActor(line, condition){
        let scripts = actorManager.thread.dbg.findScripts({source: this._source, line: line}),
            entryPoints;

        do {
            entryPoints = [];
            for (let script of scripts) {
                let offsets = script.getLineOffsets(line);
                if (offsets.length > 0)
                    entryPoints.push({
                        script: script,
                        offsets: offsets
                    });
            }
            //exit conditions
            //1 - find entryPoints
            // or
            //2 - not find scripts for line
            if (entryPoints.length == 0)
                scripts = actorManager.thread.dbg.findScripts({source: this._source, line: ++line});
        } while( entryPoints.length == 0 && scripts.length != 0);
        if (entryPoints.length > 0) {
            let actor = this[line] ? this[line] : new BreakpointActor(this, line, entryPoints);
            actor._condition = condition;
            return actor;
        } else {
            return null;
        }
    }
    getProperPath(p) {
        let res = p;
        // replace single backslash with url slash (every occurrence)
        res = res.replace(/\\/g, '/');
        if (res.startsWith('//')) {
            res = 'file:' + res
        }
        else if (res.charAt(1) == ':') {
            res = 'file:///' + res
        } 
        return res;
    }
    findSourceMapData(data) {
        let result = undefined;
        let searchResult = new RegExp('\/\/# ?sourceMappingURL ?= ?(.*)', 'i').exec(data);
        if (searchResult) {
            result = searchResult.pop();
        }
        return result;
    }
    get _resp(){
        let source = this._source;// || this.generatedSource;
        // This might not have a source or a generatedSource because we
        // treat HTML pages with inline scripts as a special SourceActor
        // that doesn't have either
        let introductionUrl = null;
        if (source && source.introductionScript) {
            introductionUrl = source.introductionScript.source.url;
        }
        let addonPath = undefined;
        let url = this._source.url;
        if (!url || url === 'debugger eval code') {
            url = undefined;
        } else {
            url = this.getProperPath(url.split(" -> ").pop());
            if (url.startsWith('file:')) {
                let webAppRootPath = this.getProperPath(dbg_binding.webAppRootPath);
                if (url.startsWith(webAppRootPath)) {
                    addonPath = url.substr(webAppRootPath.length);
                }
            }
        }
        return {
            "actor": this.fullActor,
            "url": url,
            "addonPath" : addonPath,
            "addonID": addonPath ? dbg_binding.addonID : undefined,
            "isBlackBoxed": false,
            "isPrettyPrinted": false,
            "introductionUrl": introductionUrl ? introductionUrl.split(' -> ').pop() : undefined,
            introductionType: source ? source.introductionType : '',
            "sourceMapURL": this.findSourceMapData(source.text)
        }
    }
}

class BreakpointActor extends Actor{
    constructor (sourceActor, lineNo, entryPoints) {
        super(lineNo, sourceActor);
        this._lineNo = lineNo;
        this._sourceActor = sourceActor;
        this._scripts = [];
        for (let {script, offsets} of entryPoints) {
            for (let offset of offsets) {
                script.setBreakpoint(offset, this);
                this._scripts.push(script);
            }
        }
    }
    delete(aRequest) {
        for (let script of this._scripts) {
            script.clearBreakpoint(this);
        }
        delete this._sourceActor[this._actor];
        return {};
    }
    checkCondition(frame) {
        let completion = frame.eval(this._condition);
        if (completion) {
            if (completion.throw) {
                // The evaluation failed and threw
                let message = "Unknown exception";
                try {
                    if (completion.throw.getOwnPropertyDescriptor) {
                        message = completion.throw.getOwnPropertyDescriptor("message").value;
                    } else if (completion.toString) {
                        message = completion.toString();
                    }
                } catch (ex) {}
                return {
                    result: true,
                    message: message
                };
            } else if (completion.yield) {
                return { result: undefined };
                //assert(false, "Shouldn't ever get yield completions from an eval");
            } else {
                return { result: completion.return ? true : false };
            }
        } else {
            // The evaluation was killed (possibly by the slow script dialog)
            return { result: undefined };
        }
    }
    hit(frame) {
        if (frame.onStep) {
            return undefined
        }
        let reason = {};
        if (!this._condition) {
            reason.type = "breakpoint";
            // TODO: add the rest of the breakpoints on that line (bug 676602).
            reason.actors = [ this.fullActor ];
        } else {
            let { result, message } = this.checkCondition(frame);
            if (result) {
                if (!message) {
                    reason.type = "breakpoint";
                } else {
                    reason.type = "breakpointConditionThrown";
                    reason.message = message;
                }
                reason.actors = [ this.fullActor ];
            } else {
                return undefined;
            }
        }
        return actorManager.thread._pauseAndRespond(frame, reason);
    }
    get _resp() {
        return {
            actor: this.fullActor,
            actualLocation: {
                line: this._lineNo,
                source: {
                    actor: this._sourceActor.fullActor
                }
            },
            isPending: false
        };
    }
}

class LStrActor extends Actor {
    constructor (str, parent) {
        super('_grip'+ parent._grips++, parent);
        this._str = str;
    }
    substring(aRequest) {
        return {
            "substring": this._str.substr(aRequest.start, aRequest.end)
        }
    }
    release(aRequest) {
        delete this._parent[this._actor];
        return {}
    }
    get _resp(){
        return {
            "type": "longString",
            "initial": this._str.substr(0, LONG_STRING_INITIAL_LENGTH),
            "length": this._str.length,
            "actor": this.fullActor
        }
    }

}

class ObjectActor extends Actor {
    constructor(obj, parent) {
        super('_grip'+ parent._grips++, parent);
        this._obj = obj;
    }
    get _resp(){
        this._parent._gripDepth++;
        let resp = {
            "type": "object",
            "class": this._obj.class,
            "actor": this.fullActor,
            "extensible": this._obj.isExtensible(),
            "frozen": this._obj.isFrozen(),
            "sealed": this._obj.isSealed()
        };

        try {
            // Bug 1163520: Assert on internal functions
            if (this._obj.class != "Function") {
                resp.ownPropertyLength = this._obj.getOwnPropertyNames().length;

            }
        } catch(e) {}

        let previewers = ObjectActorPreviewers[this._obj.class] || ObjectActorPreviewers.Object || [];

        for (let fn of previewers) {
            try {
                if (fn(this, resp)) {
                    break;
                }
            } catch (e) {
                //let msg = "ObjectActor.prototype.grip previewer function";
                //DevToolsUtils.reportException(msg, e);
            }
        }
        this._parent._gripDepth--;
        return resp;
    }
    prototypeAndProperties(aRequest) {
        let ownProperties = {},//Object.create(null),
            names;

        try {
            names = this._obj.getOwnPropertyNames();
        } catch (ex) {
            // The above can throw if this.obj points to a dead object.
            // TODO: we should use Cu.isDeadWrapper() - see bug 885800.
            return {
                prototype: this.getGrip(null),
                ownProperties: ownProperties,
                safeGetterValues: {}//Object.create(null)
            };
        }

        for (let name of names) {
            ownProperties[name] = this._propertyDescriptor(name);
        }

        return {
            prototype: this.getGrip(this._obj.proto),
            ownProperties: ownProperties,
            safeGetterValues: this._findSafeGetterValues(names)
        };
    }
    enumProperties(aRequest) {
        let actor = new PropertyIteratorActor(this, aRequest.options);
        //this.registeredPool.addActor(actor);
        //this.iterators.add(actor);
        return { iterator: actor._resp() };
    }
    scope(aRequest) {
        if (this._obj.class !== "Function") {
            return {
                error: "objectNotFunction",
                message: "scope request is only valid for object grips with a" +
                " 'Function' class."
            };
        }
        let env;
        if (!this._obj.environment || !(env = (new EnvironmentActor(this._obj.environment, this))._resp)) {
            return {
                error: "notDebuggee",
                message: "cannot access the environment of this function."
            };
        }
        return {scope: env};
    }
    release(aRequest) {
        delete this._parent[this._actor];
        return {}
    }
    displayString(aRequest) {
        const string = stringify(this._obj);
        return { displayString: this.getGrip(string) };
    }
    getGrip(value){
        return this._parent.getGrip(value);
    }
    getGripDepth() {
        return this._parent._gripDepth;
    }
    _propertyDescriptor(name, onlyEnumerable) {
        let desc;
        try {
            desc = this._obj.getOwnPropertyDescriptor(name);
        } catch (e) {
            // Calling getOwnPropertyDescriptor on wrapped native prototypes is not
            // allowed (bug 560072). Inform the user with a bogus, but hopefully
            // explanatory, descriptor.
            return {
                configurable: false,
                writable: false,
                enumerable: false,
                value: e.name
            };
        }

        if (!desc || onlyEnumerable && !desc.enumerable) {
            return undefined;
        }

        let retval = {
            configurable: desc.configurable,
            enumerable: desc.enumerable
        };

        if ("value" in desc) {
            retval.writable = desc.writable;
            retval.value = this._parent.getGrip(desc.value);
        } else {
            if ("get" in desc) {
                retval.get = this._parent.getGrip(desc.get);
            }
            if ("set" in desc) {
                retval.set = this._parent.getGrip(desc.set);
            }
        }
        return retval;
    }
    _findSafeGetterValues(ownProperties, limit = 0) {
        let safeGetterValues = {} //Object.create(null);
        let obj = this._obj;
        let level = 0, i = 0;

        while (obj) {
            let getters = this._findSafeGetters(obj);
            for (let name of getters) {
                // Avoid overwriting properties from prototypes closer to this.obj. Also
                // avoid providing safeGetterValues from prototypes if property |name|
                // is already defined as an own property.
                if (name in safeGetterValues ||
                    (obj != this._obj && ownProperties.indexOf(name) !== -1)) {
                    continue;
                }

                // Ignore __proto__ on Object.prototye.
                if (!obj.proto && name == "__proto__") {
                    continue;
                }

                let desc = null, getter = null;
                try {
                    desc = obj.getOwnPropertyDescriptor(name);
                    getter = desc.get;
                } catch (ex) {
                    // The above can throw if the cache becomes stale.
                }
                if (!getter) {
                    obj._safeGetters = null;
                    continue;
                }

                let result = getter.call(this._obj);
                if (result && !("throw" in result)) {
                    let getterValue = undefined;
                    if ("return" in result) {
                        getterValue = result.return;
                    } else if ("yield" in result) {
                        getterValue = result.yield;
                    }
                    // WebIDL attributes specified with the LenientThis extended attribute
                    // return undefined and should be ignored.
                    if (getterValue !== undefined) {
                        safeGetterValues[name] = {
                            getterValue: this._parent.getGrip(getterValue),
                            getterPrototypeLevel: level,
                            enumerable: desc.enumerable,
                            writable: level == 0 ? desc.writable : true
                        };
                        if (limit && ++i == limit) {
                            break;
                        }
                    }
                }
            }
            if (limit && i == limit) {
                break;
            }
            obj = obj.proto;
            level++;
        }
        return safeGetterValues;
    }
    _findSafeGetters(object) {
        if (object._safeGetters) {
            return object._safeGetters;
        }

        let getters = new Set();
        let names = [];
        try {
            names = object.getOwnPropertyNames()
        } catch (ex) {
            // Calling getOwnPropertyNames() on some wrapped native prototypes is not
            // allowed: "cannot modify properties of a WrappedNative". See bug 952093.
        }

        for (let name of names) {
            let desc = null;
            try {
                desc = object.getOwnPropertyDescriptor(name);
            } catch (e) {
                // Calling getOwnPropertyDescriptor on wrapped native prototypes is not
                // allowed (bug 560072).
            }
            if (!desc || desc.value !== undefined || !("get" in desc)) {
                continue;
            }

            if (DevToolsUtils.hasSafeGetter(desc)) {
                getters.add(name);
            }
        }

        object._safeGetters = getters;
        return getters;
    }
}

class PropertyIteratorActor extends Actor {
    constructor(objectActor, options) {
        super('propertyIterator', objectActor);
        this.objectActor = objectActor;
        let ownProperties = {},
            names = [],
            safeGetterValues = {},
            safeGetterNames = [];
        try {
            names = this.objectActor._obj.getOwnPropertyNames();
        } catch (ex) {}

        if (!options.ignoreSafeGetters) {
            // Merge the safe getter values into the existing properties list.
            safeGetterValues = this.objectActor._findSafeGetterValues(names);
            safeGetterNames = Object.keys(safeGetterValues);
            for (let name of safeGetterNames) {
                if (names.indexOf(name) === -1) {
                    names.push(name);
                }
            }
        }

        if (options.ignoreIndexedProperties || options.ignoreNonIndexedProperties) {
            let length = DevToolsUtils.getProperty(this.objectActor._obj, "length");
            if (typeof(length) !== "number") {
                // Pseudo arrays are flagged as ArrayLike if they have
                // subsequent indexed properties without having any length attribute.
                length = 0;
                for (let key of names) {
                    if (isNaN(key) || key != length++) {
                        break;
                    }
                }
            }

            if (options.ignoreIndexedProperties) {
                names = names.filter(i => {
                    // Use parseFloat in order to reject floats...
                    // (parseInt converts floats to integer)
                    // (Number(str) converts spaces to 0)
                    i = parseFloat(i);
                    return !Number.isInteger(i) || i < 0 || i >= length;
                });
            }

            if (options.ignoreNonIndexedProperties) {
                names = names.filter(i => {
                    i = parseFloat(i);
                    return Number.isInteger(i) && i >= 0 && i < length;
                });
            }
        }

        if (options.query) {
            let { query } = options;
            query = query.toLowerCase();
            names = names.filter(name => {
                // Filter on attribute names
                if (name.toLowerCase().includes(query)) {
                    return true;
                }
                // and then on attribute values
                let desc;
                try {
                    desc = this._obj.getOwnPropertyDescriptor(name);
                } catch(e) {}
                if (desc && desc.value &&
                    String(desc.value).includes(query)) {
                    return true;
                }
                return false;
            });
        }

        if (options.sort) {
            names.sort();
        }

        // Now build the descriptor list
        for (let name of names) {
            let desc = this.objectActor._propertyDescriptor(name);
            if (!desc) {
                desc = safeGetterValues[name];
            }
            else if (name in safeGetterValues) {
                // Merge the safe getter values into the existing properties list.
                let { getterValue, getterPrototypeLevel } = safeGetterValues[name];
                desc.getterValue = getterValue;
                desc.getterPrototypeLevel = getterPrototypeLevel;
            }
            ownProperties[name] = desc;
        }

        this.names = names;
        this.ownProperties = ownProperties;
    }
    slice(aRequest) {
        let { start, count } = aRequest;
        let names = this.names.slice(start, start + count);
        let props = {};
        for (let name of names) {
            props[name] = this.ownProperties[name];
        }
        return {
            ownProperties: props
        };
    }
    _resp() {
        return {
            type: "propertyIterator",
            actor: this.fullActor,
            count: this.names.length
        };
    }
}

class EnvironmentActor extends Actor {
    constructor(environment, parent) {
        super(parent instanceof EnvironmentActor ? 'parent' : 'env' , parent);
        this._env = environment;
        if (this._env.parent) new EnvironmentActor(this._env.parent, this);
    }
    get _resp(){
        let resp = {
            actor: this.fullActor
        };
        if (this._env.type == "declarative") {
            resp.type = this._env.callee ? "function" : "block";
        } else {
            resp.type = this._env.type;
        }
        if (this._env.type == "object" || this._env.type == "with") {
            resp.object = this.getGrip(this._env.object);
        }
        if (this._env.callee) {
            resp.function = this.getGrip(this._env.callee);
        }
        if (this._env.type == "declarative") {
            resp.bindings = this.bindings()
        }
        if (this.parent)
            resp.parent = this.parent._resp;
        return resp;
    }
    bindings(){
        let bindings = { arguments: [], variables: {} };
        // TODO: this part should be removed in favor of the commented-out part
        // below when getVariableDescriptor lands (bug 725815).
        if (typeof this._env.getVariable != "function") {
            //if (typeof this._env.getVariableDescriptor != "function") {
            return bindings;
        }

        let parameterNames;
        if (this._env.callee) {
            parameterNames = this._env.callee.parameterNames;
        } else {
            parameterNames = [];
        }
        for (let name of parameterNames) {
            let arg = {};
            let value = this._env.getVariable(name);

            // TODO: this part should be removed in favor of the commented-out part
            // below when getVariableDescriptor lands (bug 725815).
            let desc = {
                value: value,
                configurable: false,
                writable: !(value && value.optimizedOut),
                enumerable: true
            };

            // let desc = this._env.getVariableDescriptor(name);
            let descForm = {
                enumerable: true,
                configurable: desc.configurable
            };
            if ("value" in desc) {
                descForm.value = this.getGrip(desc.value);
                descForm.writable = desc.writable;
            } else {
                descForm.get = this.getGrip(desc.get);
                descForm.set = this.getGrip(desc.set);
            }
            arg[name] = descForm;
            bindings.arguments.push(arg);
        }

        for (let name of this._env.names()) {
            if (bindings.arguments.some(function exists(element) {
                    return !!element[name];
                })) {
                continue;
            }

            let value = this._env.getVariable(name);

            // TODO: this part should be removed in favor of the commented-out part
            // below when getVariableDescriptor lands.
            let desc = {
                value: value,
                configurable: false,
                writable: !(value &&
                (value.optimizedOut ||
                value.uninitialized ||
                value.missingArguments)),
                enumerable: true
            };

            //let desc = this._env.getVariableDescriptor(name);
            let descForm = {
                enumerable: true,
                configurable: desc.configurable
            };
            if ("value" in desc) {
                descForm.value = this.getGrip(desc.value);
                descForm.writable = desc.writable;
            } else {
                descForm.get = this.getGrip(desc.get || undefined);
                descForm.set = this.getGrip(desc.set || undefined);
            }
            bindings.variables[name] = descForm;
        }
        return bindings;
    }
}

class FrameActor extends Actor {
    constructor(frame, parent) {
        super(parent ? 'older' : 'frame', parent);
        this._frame = frame;
        this._depth = parent ? parent._depth + 1 : 0;
        if (this._frame.older)
            new FrameActor(this._frame.older, this);
    }
    get _resp(){
        let resp = {
            actor: this.fullActor,
            type: this._frame.type,
            depth: this._depth,
            'this': this.getGrip(this._frame.this),
            arguments: this._args()
        };
        if (this._frame.type) {
            resp.callee = this.getGrip(this._frame.callee);
        }
        if (this._frame.environment) {
            resp.environment = (new EnvironmentActor(this._frame.environment, this))._resp;
        }
        if (this._frame.script) {
            let script = this._frame.script,
                location = script.getOffsetLocation(this._frame.offset),
                sourceResp = actorManager.getActor('source')._sourcesMap.get(script.source)._resp;
            resp.where = {
                source: sourceResp,
                line: location.lineNumber,
                column: location.columnNumber
            };
            resp.source = sourceResp;
        }
        if (!this._frame.older) {
            resp.oldest = true;
        }

            return resp;
    }
    _args() {
        if (!this._frame.arguments) {
            return [];
        }

        return this._frame.arguments.map(arg => this.getGrip(arg));
    }
}

class AddonActor extends Actor {
    constructor(actorName) {
        let serverActor = new Actor(dbg_binding.debuggerName),
            connActor = new Actor('conn1', serverActor);
        super(actorName, connActor);
        new ThreadActor();
    }
    attach(aRequest){
        return {
            type: "tabAttached",
            threadActor: actorManager.thread.fullActor,
            traits: {reconfigure: false}
        }
    }
    detach(aRequest) {
        return {
            "type": "detached"
        }
    }
    reconfigure(aRequest) {
        return {};
    }
    listWorkers(aRequest) {
        return { from: this.fullActor, "workers":[] }
    }
    focus(aRequest) {
       return {}
    }
}

export function newMessage (msg) {
    var inRequest,
        outRequest,
        actorName,
        actor,
        handler;
    try {
        if (msg === null) { // debugger client close socket
            // emulate detach without sending responses to detached client
            // {"to":"thread","type":"detach"}
            actor = actorManager.getActor('thread')
            if (actor) {
                actor.detach()
            }
            return
        }
        inRequest = (typeof msg === "string") ? JSON.parse(msg) : msg;
        actorName = inRequest.to;
        actor = actorManager.getActor(actorName);
        if (actor) {
            handler = actor[inRequest.type];
            outRequest = handler ? actor[inRequest.type](inRequest) : {};
            if (!handler)
                DevToolsUtils.reportException('newMessage: ' + inRequest.type + ' not found in ' + actorName + ' Class ' + actor.constructor.name, msg);
            if (outRequest) {
                outRequest.from = actorName;
                dbg_binding.send(outRequest);
            } else {
                DevToolsUtils.reportException('newMessage: ' + actorName + '.' + inRequest.type + '! outRequest', msg);
            }
        } else {
            DevToolsUtils.reportException('newMessage: ' + actorName + ' not found', msg);
        }
    } catch(e) {
        DevToolsUtils.reportException('newMessage', e);
    }
}

//available log levels
/*
{category: "webdev",  level: "error"}
{category: "webdev",  level: "warn"}
{category: "webdev",  level: "info"}
{category: "webdev",  level: "log"}
{category: "server",  level: "error"}
{category: "server",  level: "warn"}
{category: "server",  level: "info"}
{category: "server",  level: "log"}
{category: "js",      level: "error"}
{category: "js",      level: "warn"}
{category: "css",     level: "error"}
{category: "css",     level: "warn"}
{category: "network", level: "error"}
{category: "network", level: "warn"}
{category: "network", level: "log"}
*/
let consoleMessageResolver = (msg) => ({level: "log", category: "webdev", msg: msg, timeStamp: Date.now()});

export function newConsoleMessage(_msg) {
    let {category, level, msg, timeStamp} = consoleMessageResolver(_msg);
    dbg_binding.send({
        from: actorManager.console._actor,
        type: "consoleAPICall",
        message: {
            "arguments": [actorManager.console.getGrip(msg)],
            "columnNumber": 1,
            "counter": null,
            "filename": "debugger eval code",
            "functionName": "",
            "groupName": "",
            "level": level,
            "lineNumber": 1,
            "private": false,
            "styles": [],
            "timeStamp": timeStamp,
            "timer": null,
            "workerType": "none",
            "category": category
        }
    });
}

export function init(engNo, interruptOnNext) {
    if (!actorManager) {
        actorManager = new ActorManager(engNo);
        actorManager.init();
    }
    let sources = new SourcesActor();
    if (dbg_binding.listen) {
        actorManager.thread.attach();
    }

    if (interruptOnNext) {
        let onEnterFrame = (aFrame) => {
            return actorManager.thread._pauseAndRespond(aFrame, {type: "interrupted", onNext: true});
        };
        actorManager.thread.dbg.onEnterFrame = onEnterFrame;
    }
    if (dbg_binding.listen) {
        actorManager.thread.dbg.findScripts().forEach(function (script) {
            let resp = sources._addSource(script.source);
            if (resp) {
                dbg_binding.send({
                    from: actorManager.thread.fullActor,
                    type: "newSource",
                    source: resp
                });
            }
        });
    }
}

export function uninit() {
    if (dbg_binding.paused) {
        //actorManager.thread.dbg.enabled = false;
        let resp = actorManager.thread.resume({});

        resp.from = actorManager.thread.fullActor;
        dbg_binding.send(resp);
    }

    dbg_binding.send({
        from: actorManager.addon.fullActor,
        "type": "tabNavigated",
        "nativeConsoleAPI": true,
        "state": "start",
        "isFrameSwitching": false
    });

    dbg_binding.send({
        from: actorManager.addon.fullActor,
        "type": "tabNavigated",
        "nativeConsoleAPI": true,
        "state": "stop",
        "isFrameSwitching": false
    });
}

/**
 * list of console additional commands
 * property help contains welcome message
 * other properties must be an objects with 2 properties
 * value - value for console
 * description(optional) - description for help() console function
 *
 * @type {{help: string|Function:string, test: {command: Function, description: string}}}
 */
export function setConsoleCommands(aCommands) {
    actorManager.console.consoleCommands = aCommands;
}

export function setConsoleMessageResolver(aResolver) {
    consoleMessageResolver = aResolver;
}


export function doInterupt() {
    let msg;
    while (msg = dbg_binding.read(true)) {
        newMessage(msg);
        while (msg = dbg_binding.read(false)) {
            newConsoleMessage(msg);
        }
    };
    while (msg = dbg_binding.read(false)) {
        newConsoleMessage(msg);
    }
}

process.dbg = {
    doInterupt: doInterupt,
    init: init,
    uninit: uninit
};