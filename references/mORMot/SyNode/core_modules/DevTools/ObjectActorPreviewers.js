import * as DevToolsUtils from 'DevTools/DevToolsUtils.js';
let {global} = process.binding('debugger');

const TYPED_ARRAY_CLASSES = ["Uint8Array", "Uint8ClampedArray", "Uint16Array",
    "Uint32Array", "Int8Array", "Int16Array", "Int32Array", "Float32Array",
    "Float64Array"];


// Number of items to preview in objects, arrays, maps, sets, lists,
// collections, etc.
const OBJECT_PREVIEW_MAX_ITEMS = 10;

let _ObjectActorPreviewers = {
    String: [function (objectActor, grip) {
        return wrappedPrimitivePreviewer("String", String, objectActor, grip);
    }],

    Boolean: [function (objectActor, grip) {
        return wrappedPrimitivePreviewer("Boolean", Boolean, objectActor, grip);
    }],

    Number: [function (objectActor, grip) {
        return wrappedPrimitivePreviewer("Number", Number, objectActor, grip);
    }],

    Function: [function (objectActor, grip) {
        let {_obj} = objectActor;
        if (_obj.name) {
            grip.name = _obj.name;
        }

        if (_obj.displayName) {
            grip.displayName = _obj.displayName.substr(0, 500);
        }

        if (_obj.parameterNames) {
            grip.parameterNames = _obj.parameterNames;
        }

        // Check if the developer has added a de-facto standard displayName
        // property for us to use.
        let userDisplayName;
        try {
            userDisplayName = _obj.getOwnPropertyDescriptor("displayName");
        } catch (e) {
            // Calling getOwnPropertyDescriptor with displayName might throw
            // with "permission denied" errors for some functions.
            //dumpn(e);
        }

        if (userDisplayName && typeof userDisplayName.value == "string" &&
            userDisplayName.value) {
            grip.userDisplayName = objectActor.getGrip(userDisplayName.value);
        }

        //let dbgGlobal = hooks.getGlobalDebugObject();
        //if (dbgGlobal) {
        //let script = dbgGlobal.makeDebuggeeValue(_obj.unsafeDereference()).script;
        let script = _obj.script;
        if (script) {
            grip.location = {
                url: script.url,
                line: script.startLine
            };
        }
        //}

        return true;
    }],

    RegExp: [function (objectActor, grip) {
        let {_obj} = objectActor;
        // Avoid having any special preview for the RegExp.prototype itself.
        if (!_obj.proto || _obj.proto.class != "RegExp") {
            return false;
        }

        let str = RegExp.prototype.toString.call(_obj.unsafeDereference());
        grip.displayString = objectActor.getGrip(str);
        return true;
    }],

    Date: [function (objectActor, grip) {
        let {_obj} = objectActor;
        let time = Date.prototype.getTime.call(_obj.unsafeDereference());

        grip.preview = {
            timestamp: objectActor.getGrip(time)
        };
        return true;
    }],

    Array: [function (objectActor, grip) {
        let {_obj} = objectActor;
        let length = DevToolsUtils.getProperty(_obj, "length");
        if (typeof length != "number") {
            return false;
        }

        grip.preview = {
            kind: "ArrayLike",
            length: length
        };

        if (objectActor.getGripDepth() > 1) {
            return true;
        }

        let raw = _obj.unsafeDereference();
        let items = grip.preview.items = [];

        for (let i = 0; i < length; ++i) {
            // Array Xrays filter out various possibly-unsafe properties (like
            // functions, and claim that the value is undefined instead. This
            // is generally the right thing for privileged code accessing untrusted
            // objects, but quite confusing for Object previews. So we manually
            // override this protection by waiving Xrays on the array, and re-applying
            // Xrays on any indexed value props that we pull off of it.
            //let desc = Object.getOwnPropertyDescriptor(Cu.waiveXrays(raw), i);
            let desc = Object.getOwnPropertyDescriptor(raw, i);
            if (desc && !desc.get && !desc.set) {
                //let value = Cu.unwaiveXrays(desc.value);
                let value = desc.value;
                value = makeDebuggeeValueIfNeeded(_obj, value);
                items.push(objectActor.getGrip(value));
            } else {
                items.push(null);
            }

            if (items.length == OBJECT_PREVIEW_MAX_ITEMS) {
                break;
            }
        }

        return true;
    }],

    Set: [function (objectActor, grip) {
        let {_obj} = objectActor;
        let size = DevToolsUtils.getProperty(_obj, "size");
        if (typeof size != "number") {
            return false;
        }

        grip.preview = {
            kind: "ArrayLike",
            length: size
        };

        // Avoid recursive object grips.
        if (objectActor.getGripDepth() > 1) {
            return true;
        }

        let raw = _obj.unsafeDereference();
        let items = grip.preview.items = [];
        // We currently lack XrayWrappers for Set, so when we iterate over
        // the values, the temporary iterator objects get created in the target
        // compartment. However, we _do_ have Xrays to Object now, so we end up
        // Xraying those temporary objects, and filtering access to |it.value|
        // based on whether or not it's Xrayable and/or callable, which breaks
        // the for/of iteration.
        //
        // This code is designed to handle untrusted objects, so we can safely
        // waive Xrays on the iterable, and relying on the Debugger machinery to
        // make sure we handle the resulting objects carefully.
        //for (let item of Cu.waiveXrays(Set.prototype.values.call(raw))) {
        for (let item of Set.prototype.values.call(raw)) {
            //item = Cu.unwaiveXrays(item);
            item = makeDebuggeeValueIfNeeded(_obj, item);
            items.push(objectActor.getGrip(item));
            if (items.length == OBJECT_PREVIEW_MAX_ITEMS) {
                break;
            }
        }

        return true;
    }],
    /*WeakSet: [function(objectActor, grip) {
     let {_obj} = objectActor;
     let raw = _obj.unsafeDereference();

     // We currently lack XrayWrappers for WeakSet, so when we iterate over
     // the values, the temporary iterator objects get created in the target
     // compartment. However, we _do_ have Xrays to Object now, so we end up
     // Xraying those temporary objects, and filtering access to |it.value|
     // based on whether or not it's Xrayable and/or callable, which breaks
     // the for/of iteration.
     //
     // This code is designed to handle untrusted objects, so we can safely
     // waive Xrays on the iterable, and relying on the Debugger machinery to
     // make sure we handle the resulting objects carefully.
     //let keys = Cu.waiveXrays(ThreadSafeChromeUtils.nondeterministicGetWeakSetKeys(raw));
     let keys = Cu.waiveXrays(ThreadSafeChromeUtils.nondeterministicGetWeakSetKeys(raw));
     grip.preview = {
     kind: "ArrayLike",
     length: keys.length
     };

     //// Avoid recursive object grips.
     //if (hooks.getGripDepth() > 1) {
     //return true;
     //}

     let items = grip.preview.items = [];
     for (let item of keys) {
     //item = Cu.unwaiveXrays(item);
     item = makeDebuggeeValueIfNeeded(obj, item);
     items.push(hooks.createValueGrip(item));
     if (items.length == OBJECT_PREVIEW_MAX_ITEMS) {
     break;
     }
     }

     return true;
     }],*/

    Map: [function (objectActor, grip) {
        let {_obj} = objectActor;
        let size = DevToolsUtils.getProperty(_obj, "size");
        if (typeof size != "number") {
            return false;
        }

        grip.preview = {
            kind: "MapLike",
            size: size
        };

        if (objectActor.getGripDepth() > 1) {
            return true;
        }

        let raw = _obj.unsafeDereference();
        let entries = grip.preview.entries = [];
        // Iterating over a Map via .entries goes through various intermediate
        // objects - an Iterator object, then a 2-element Array object, then the
        // actual values we care about. We don't have Xrays to Iterator objects,
        // so we get Opaque wrappers for them. And even though we have Xrays to
        // Arrays, the semantics often deny access to the entires based on the
        // nature of the values. So we need waive Xrays for the iterator object
        // and the tupes, and then re-apply them on the underlying values until
        // we fix bug 1023984.
        //
        // Even then though, we might want to continue waiving Xrays here for the
        // same reason we do so for Arrays above - this filtering behavior is likely
        // to be more confusing than beneficial in the case of Object previews.
        //for (let keyValuePair of Cu.waiveXrays(Map.prototype.entries.call(raw))) {
        for (let keyValuePair of Map.prototype.entries.call(raw)) {
            //let key = Cu.unwaiveXrays(keyValuePair[0]);
            let key = keyValuePair[0];
            //let value = Cu.unwaiveXrays(keyValuePair[1]);
            let value = keyValuePair[1];
            key = makeDebuggeeValueIfNeeded(_obj, key);
            value = makeDebuggeeValueIfNeeded(_obj, value);
            entries.push([objectActor.getGrip(key),
                objectActor.getGrip(value)]);
            if (entries.length == OBJECT_PREVIEW_MAX_ITEMS) {
                break;
            }
        }

        return true;
    }]/*,

     WeakMap: [function({obj, hooks}, grip) {
     let raw = obj.unsafeDereference();
     // We currently lack XrayWrappers for WeakMap, so when we iterate over
     // the values, the temporary iterator objects get created in the target
     // compartment. However, we _do_ have Xrays to Object now, so we end up
     // Xraying those temporary objects, and filtering access to |it.value|
     // based on whether or not it's Xrayable and/or callable, which breaks
     // the for/of iteration.
     //
     // This code is designed to handle untrusted objects, so we can safely
     // waive Xrays on the iterable, and relying on the Debugger machinery to
     // make sure we handle the resulting objects carefully.
     let rawEntries = Cu.waiveXrays(ThreadSafeChromeUtils.nondeterministicGetWeakMapKeys(raw));

     grip.preview = {
     kind: "MapLike",
     size: rawEntries.length,
     };

     if (hooks.getGripDepth() > 1) {
     return true;
     }

     let entries = grip.preview.entries = [];
     for (let key of rawEntries) {
     let value = Cu.unwaiveXrays(WeakMap.prototype.get.call(raw, key));
     key = Cu.unwaiveXrays(key);
     key = makeDebuggeeValueIfNeeded(obj, key);
     value = makeDebuggeeValueIfNeeded(obj, value);
     entries.push([hooks.createValueGrip(key),
     hooks.createValueGrip(value)]);
     if (entries.length == OBJECT_PREVIEW_MAX_ITEMS) {
     break;
     }
     }

     return true;
     }],

     DOMStringMap: [function({obj, hooks}, grip, rawObj) {
     if (!rawObj) {
     return false;
     }

     let keys = obj.getOwnPropertyNames();
     grip.preview = {
     kind: "MapLike",
     size: keys.length,
     };

     if (hooks.getGripDepth() > 1) {
     return true;
     }

     let entries = grip.preview.entries = [];
     for (let key of keys) {
     let value = makeDebuggeeValueIfNeeded(obj, rawObj[key]);
     entries.push([key, hooks.createValueGrip(value)]);
     if (entries.length == OBJECT_PREVIEW_MAX_ITEMS) {
     break;
     }
     }

     return true;
     }],*/
};

/**
 * Generic previewer for classes wrapping primitives, like String,
 * Number and Boolean.
 *
 * @param string className
 *        Class name to expect.
 * @param object classObj
 *        The class to expect, eg. String. The valueOf() method of the class is
 *        invoked on the given object.
 * @param ObjectActor objectActor
 *        The object actor
 * @param Object grip
 *        The result grip to fill in
 * @return Booolean true if the object was handled, false otherwise
 */
function wrappedPrimitivePreviewer(className, classObj, objectActor, grip) {
    let {_obj} = objectActor;

    if (!_obj.proto || _obj.proto.class != className) {
        return false;
    }

    let raw = _obj.unsafeDereference();
    let v = null;
    try {
        v = classObj.prototype.valueOf.call(raw);
    } catch (ex) {
        // valueOf() can throw if the raw JS object is "misbehaved".
        return false;
    }

    if (v === null) {
        return false;
    }

    let canHandle = GenericObject(objectActor, grip, className === "String");
    if (!canHandle) {
        return false;
    }

    grip.preview.wrappedValue = objectActor.getGrip(makeDebuggeeValueIfNeeded(_obj, v));
    return true;
}

function GenericObject(objectActor, grip, specialStringBehavior = false) {
    let {_obj} = objectActor;
    if (grip.preview || grip.displayString || objectActor.getGripDepth() > 1) {
        return false;
    }

    let i = 0, names = [];
    let preview = grip.preview = {
        kind: "Object",
        ownProperties: {}//Object.create(null)
    };

    try {
        names = _obj.getOwnPropertyNames();
    } catch (ex) {
        // Calling getOwnPropertyNames() on some wrapped native prototypes is not
        // allowed: "cannot modify properties of a WrappedNative". See bug 952093.
    }

    preview.ownPropertiesLength = names.length;

    let length;
    if (specialStringBehavior) {
        length = DevToolsUtils.getProperty(_obj, "length");
        if (typeof length != "number") {
            specialStringBehavior = false;
        }
    }

    for (let name of names) {
        if (specialStringBehavior && /^[0-9]+$/.test(name)) {
            let num = parseInt(name, 10);
            if (num.toString() === name && num >= 0 && num < length) {
                continue;
            }
        }

        let desc = objectActor._propertyDescriptor(name, true);
        if (!desc) {
            continue;
        }

        preview.ownProperties[name] = desc;
        if (++i == OBJECT_PREVIEW_MAX_ITEMS) {
            break;
        }
    }

    if (i < OBJECT_PREVIEW_MAX_ITEMS) {
        preview.safeGetterValues = objectActor._findSafeGetterValues(
            Object.keys(preview.ownProperties),
            OBJECT_PREVIEW_MAX_ITEMS - i);
    }

    return true;
}

/**
 * Make a debuggee value for the given object, if needed. Primitive values
 * are left the same.
 *
 * Use case: you have a raw JS object (after unsafe dereference) and you want to
 * send it to the client. In that case you need to use an ObjectActor which
 * requires a debuggee value. The Debugger.Object.prototype.makeDebuggeeValue()
 * method works only for JS objects and functions.
 *
 * @param Debugger.Object obj
 * @param any value
 * @return object
 */
function makeDebuggeeValueIfNeeded(obj, value) {
    if (value && (typeof value == "object" || typeof value == "function")) {
        return obj.makeDebuggeeValue(value);
    }
    return value;
}

// Preview functions that do not rely on the object class.
_ObjectActorPreviewers.Object = [
    function TypedArray(objectActor, grip) {
        let {_obj} = objectActor;
        if (TYPED_ARRAY_CLASSES.indexOf(_obj.class) == -1) {
            return false;
        }

        let length = DevToolsUtils.getProperty(_obj, "length");
        if (typeof length != "number") {
            return false;
        }

        grip.preview = {
            kind: "ArrayLike",
            length: length
        };

        if (objectActor.getGripDepth() > 1) {
            return true;
        }

        let raw = _obj.unsafeDereference();
        //let global = Cu.getGlobalForObject(DebuggerServer);

        let classProto = global[_obj.class].prototype;
        // The Xray machinery for TypedArrays denies indexed access on the grounds
        // that it's slow, and advises callers to do a structured clone instead.
        //let safeView = Cu.cloneInto(classProto.subarray.call(raw, 0,
        //    OBJECT_PREVIEW_MAX_ITEMS), global);
        let safeView = classProto.subarray.call(raw, 0,
            OBJECT_PREVIEW_MAX_ITEMS);
        let items = grip.preview.items = [];
        for (let i = 0; i < safeView.length; i++) {
            items.push(safeView[i]);
        }

        return true;
    },

    function Error(objectActor, grip) {
        let {_obj} = objectActor;
        switch (_obj.class) {
            case "Error":
            case "EvalError":
            case "RangeError":
            case "ReferenceError":
            case "SyntaxError":
            case "TypeError":
            case "URIError":
                let name = DevToolsUtils.getProperty(_obj, "name");
                let msg = DevToolsUtils.getProperty(_obj, "message");
                let stack = DevToolsUtils.getProperty(_obj, "stack");
                let fileName = DevToolsUtils.getProperty(_obj, "fileName");
                let lineNumber = DevToolsUtils.getProperty(_obj, "lineNumber");
                let columnNumber = DevToolsUtils.getProperty(_obj, "columnNumber");
                grip.preview = {
                    kind: "Error",
                    name: objectActor.getGrip(name),
                    message: objectActor.getGrip(msg),
                    stack: objectActor.getGrip(stack),
                    fileName: objectActor.getGrip(fileName),
                    lineNumber: objectActor.getGrip(lineNumber),
                    columnNumber: objectActor.getGrip(columnNumber)
                };
                return true;
            default:
                return false;
        }
    },

    /*function CSSMediaRule({obj, hooks}, grip, rawObj) {
     if (isWorker || !rawObj || !(rawObj instanceof Ci.nsIDOMCSSMediaRule)) {
     return false;
     }
     grip.preview = {
     kind: "ObjectWithText",
     text: hooks.createValueGrip(rawObj.conditionText),
     };
     return true;
     },

     function CSSStyleRule({obj, hooks}, grip, rawObj) {
     if (isWorker || !rawObj || !(rawObj instanceof Ci.nsIDOMCSSStyleRule)) {
     return false;
     }
     grip.preview = {
     kind: "ObjectWithText",
     text: hooks.createValueGrip(rawObj.selectorText),
     };
     return true;
     },

     function ObjectWithURL({obj, hooks}, grip, rawObj) {
     if (isWorker || !rawObj || !(rawObj instanceof Ci.nsIDOMCSSImportRule ||
     rawObj instanceof Ci.nsIDOMCSSStyleSheet ||
     rawObj instanceof Ci.nsIDOMLocation ||
     rawObj instanceof Ci.nsIDOMWindow)) {
     return false;
     }

     let url;
     if (rawObj instanceof Ci.nsIDOMWindow && rawObj.location) {
     url = rawObj.location.href;
     } else if (rawObj.href) {
     url = rawObj.href;
     } else {
     return false;
     }

     grip.preview = {
     kind: "ObjectWithURL",
     url: hooks.createValueGrip(url),
     };

     return true;
     },*/

    /*function ArrayLike(objectActor, grip, rawObj) {
     let {_obj} = objectActor;
     if (isWorker || !rawObj ||
     obj.class != "DOMStringList" &&
     obj.class != "DOMTokenList" && !(rawObj instanceof Ci.nsIDOMMozNamedAttrMap ||
     rawObj instanceof Ci.nsIDOMCSSRuleList ||
     rawObj instanceof Ci.nsIDOMCSSValueList ||
     rawObj instanceof Ci.nsIDOMFileList ||
     rawObj instanceof Ci.nsIDOMFontFaceList ||
     rawObj instanceof Ci.nsIDOMMediaList ||
     rawObj instanceof Ci.nsIDOMNodeList ||
     rawObj instanceof Ci.nsIDOMStyleSheetList)) {
     return false;
     }

     if (typeof rawObj.length != "number") {
     return false;
     }

     grip.preview = {
     kind: "ArrayLike",
     length: rawObj.length,
     };

     if (hooks.getGripDepth() > 1) {
     return true;
     }

     let items = grip.preview.items = [];

     for (let i = 0; i < rawObj.length &&
     items.length < OBJECT_PREVIEW_MAX_ITEMS; i++) {
     let value = makeDebuggeeValueIfNeeded(obj, rawObj[i]);
     items.push(hooks.createValueGrip(value));
     }

     return true;
     },*/

    /*function CSSStyleDeclaration({obj, hooks}, grip, rawObj) {
     if (isWorker || !rawObj || !(rawObj instanceof Ci.nsIDOMCSSStyleDeclaration)) {
     return false;
     }

     grip.preview = {
     kind: "MapLike",
     size: rawObj.length,
     };

     let entries = grip.preview.entries = [];

     for (let i = 0; i < OBJECT_PREVIEW_MAX_ITEMS &&
     i < rawObj.length; i++) {
     let prop = rawObj[i];
     let value = rawObj.getPropertyValue(prop);
     entries.push([prop, hooks.createValueGrip(value)]);
     }

     return true;
     },

     function DOMNode({obj, hooks}, grip, rawObj) {
     if (isWorker || obj.class == "Object" || !rawObj || !(rawObj instanceof Ci.nsIDOMNode)) {
     return false;
     }

     let preview = grip.preview = {
     kind: "DOMNode",
     nodeType: rawObj.nodeType,
     nodeName: rawObj.nodeName,
     };

     if (rawObj instanceof Ci.nsIDOMDocument && rawObj.location) {
     preview.location = hooks.createValueGrip(rawObj.location.href);
     } else if (rawObj instanceof Ci.nsIDOMDocumentFragment) {
     preview.childNodesLength = rawObj.childNodes.length;

     if (hooks.getGripDepth() < 2) {
     preview.childNodes = [];
     for (let node of rawObj.childNodes) {
     let actor = hooks.createValueGrip(obj.makeDebuggeeValue(node));
     preview.childNodes.push(actor);
     if (preview.childNodes.length == OBJECT_PREVIEW_MAX_ITEMS) {
     break;
     }
     }
     }
     } else if (rawObj instanceof Ci.nsIDOMElement) {
     // Add preview for DOM element attributes.
     if (rawObj instanceof Ci.nsIDOMHTMLElement) {
     preview.nodeName = preview.nodeName.toLowerCase();
     }

     let i = 0;
     preview.attributes = {};
     preview.attributesLength = rawObj.attributes.length;
     for (let attr of rawObj.attributes) {
     preview.attributes[attr.nodeName] = hooks.createValueGrip(attr.value);
     if (++i == OBJECT_PREVIEW_MAX_ITEMS) {
     break;
     }
     }
     } else if (rawObj instanceof Ci.nsIDOMAttr) {
     preview.value = hooks.createValueGrip(rawObj.value);
     } else if (rawObj instanceof Ci.nsIDOMText ||
     rawObj instanceof Ci.nsIDOMComment) {
     preview.textContent = hooks.createValueGrip(rawObj.textContent);
     }

     return true;
     },

     function DOMEvent({obj, hooks}, grip, rawObj) {
     if (isWorker || !rawObj || !(rawObj instanceof Ci.nsIDOMEvent)) {
     return false;
     }

     let preview = grip.preview = {
     kind: "DOMEvent",
     type: rawObj.type,
     properties: Object.create(null),
     };

     if (hooks.getGripDepth() < 2) {
     let target = obj.makeDebuggeeValue(rawObj.target);
     preview.target = hooks.createValueGrip(target);
     }

     let props = [];
     if (rawObj instanceof Ci.nsIDOMMouseEvent) {
     props.push("buttons", "clientX", "clientY", "layerX", "layerY");
     } else if (rawObj instanceof Ci.nsIDOMKeyEvent) {
     let modifiers = [];
     if (rawObj.altKey) {
     modifiers.push("Alt");
     }
     if (rawObj.ctrlKey) {
     modifiers.push("Control");
     }
     if (rawObj.metaKey) {
     modifiers.push("Meta");
     }
     if (rawObj.shiftKey) {
     modifiers.push("Shift");
     }
     preview.eventKind = "key";
     preview.modifiers = modifiers;

     props.push("key", "charCode", "keyCode");
     } else if (rawObj instanceof Ci.nsIDOMTransitionEvent) {
     props.push("propertyName", "pseudoElement");
     } else if (rawObj instanceof Ci.nsIDOMAnimationEvent) {
     props.push("animationName", "pseudoElement");
     } else if (rawObj instanceof Ci.nsIDOMClipboardEvent) {
     props.push("clipboardData");
     }

     // Add event-specific properties.
     for (let prop of props) {
     let value = rawObj[prop];
     if (value && (typeof value == "object" || typeof value == "function")) {
     // Skip properties pointing to objects.
     if (hooks.getGripDepth() > 1) {
     continue;
     }
     value = obj.makeDebuggeeValue(value);
     }
     preview.properties[prop] = hooks.createValueGrip(value);
     }

     // Add any properties we find on the event object.
     if (!props.length) {
     let i = 0;
     for (let prop in rawObj) {
     let value = rawObj[prop];
     if (prop == "target" || prop == "type" || value === null ||
     typeof value == "function") {
     continue;
     }
     if (value && typeof value == "object") {
     if (hooks.getGripDepth() > 1) {
     continue;
     }
     value = obj.makeDebuggeeValue(value);
     }
     preview.properties[prop] = hooks.createValueGrip(value);
     if (++i == OBJECT_PREVIEW_MAX_ITEMS) {
     break;
     }
     }
     }

     return true;
     },

     function DOMException({obj, hooks}, grip, rawObj) {
     if (isWorker || !rawObj || !(rawObj instanceof Ci.nsIDOMDOMException)) {
     return false;
     }

     grip.preview = {
     kind: "DOMException",
     name: hooks.createValueGrip(rawObj.name),
     message: hooks.createValueGrip(rawObj.message),
     code: hooks.createValueGrip(rawObj.code),
     result: hooks.createValueGrip(rawObj.result),
     filename: hooks.createValueGrip(rawObj.filename),
     lineNumber: hooks.createValueGrip(rawObj.lineNumber),
     columnNumber: hooks.createValueGrip(rawObj.columnNumber),
     };

     return true;
     },*/

    /*function PseudoArray({obj, hooks}, grip, rawObj) {
     let length = 0;

     // Making sure all keys are numbers from 0 to length-1
     let keys = obj.getOwnPropertyNames();
     if (keys.length == 0) {
     return false;
     }
     for (let key of keys) {
     if (isNaN(key) || key != length++) {
     return false;
     }
     }

     grip.preview = {
     kind: "ArrayLike",
     length: length,
     };

     // Avoid recursive object grips.
     if (hooks.getGripDepth() > 1) {
     return true;
     }

     let items = grip.preview.items = [];

     let i = 0;
     for (let key of keys) {
     if (rawObj.hasOwnProperty(key) && i++ < OBJECT_PREVIEW_MAX_ITEMS) {
     let value = makeDebuggeeValueIfNeeded(obj, rawObj[key]);
     items.push(hooks.createValueGrip(value));
     }
     }

     return true;
     },*/

    GenericObject
];

export const ObjectActorPreviewers = _ObjectActorPreviewers;