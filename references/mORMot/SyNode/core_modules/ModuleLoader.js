/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// A basic synchronous module loader for testing the shell.
let {coreModulesPath, parseModule, setModuleResolveHook} = process.binding('modules')
let {loadFile, relToAbs} = process.binding('fs')

Reflect.Loader = new class {
    constructor() {
        this.registry = new Map();
        this.loadPath = coreModulesPath;
    }

    resolve(name) {
	return relToAbs(this.loadPath,name);
    }

    fetch(path) {
        //return os.file.readFile(path);
        return loadFile(path);
    }

    loadAndParse(name) {
        let path = this.resolve(name);

        if (this.registry.has(path))
            return this.registry.get(path);

        let source = this.fetch(path);
        let module = parseModule(source, path);
        this.registry.set(path, module);
        return module;
    }

    ["import"](name, referrer) {
        let module = this.loadAndParse(name);
        module.declarationInstantiation();
        return module.evaluation();
    }
};
setModuleResolveHook((module, requestName) => Reflect.Loader.loadAndParse(requestName));

