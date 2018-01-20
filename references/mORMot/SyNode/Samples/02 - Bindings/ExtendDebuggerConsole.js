import {setConsoleCommands, setConsoleMessageResolver} from 'DevTools/Debugger.js';
let dbg_binding = process.binding('debugger'),
    global = dbg_binding.global;

setConsoleCommands({
    help: () => 'Welcome to SyNode console.',
    '$cwd': {
        command: () => {return global.process.cwd(); },
	description: 'Show current working dir'
    },
    '$mormot': {
        command: () => 'http://synopse.info',
	description: 
	`Show where the mORMot live. 
	Remember - RTFM before write to forum :)`
    }
});

setConsoleMessageResolver((msg) => {
    let timestamp = (new Date(msg.substr(0,4)+'-'+msg.substr(4,2)+'-'+msg.substr(6,2)+'T'+msg.substr(9,2)+':'+msg.substr(11,2)+':'+msg.substr(13,2)+'.'+msg.substr(15,2)+'0Z')).getTime(),
        logLevel = msg.substr(20,7),
        level,
        category;

    if (timestamp) {
        msg = msg.substr(20).replace(/\t/gi, '  ');
        const logLevelMap = {
            '       ': {category: 'webdev', level: 'log'},
            ' info  ': {category: 'webdev', level: 'log'},
            ' debug ': {category: 'server', level: 'info'},
            ' trace ': {category: 'webdev', level: 'log'},
            ' warn  ': {category: 'webdev', level: 'warn'},
            ' ERROR ': {category: 'webdev', level: 'error'},
            '  +    ': {category: 'webdev', level: 'log'},
            '  -    ': {category: 'webdev', level: 'log'},
            ' OSERR ': {category: 'webdev', level: 'error'},
            ' EXC   ': {category: 'webdev', level: 'error'},
            ' EXCOS ': {category: 'webdev', level: 'error'},
            ' mem   ': {category: 'webdev', level: 'log'},
            ' stack ': {category: 'webdev', level: 'log'},
            ' fail  ': {category: 'webdev', level: 'error'},
            ' SQL   ': {category: 'webdev', level: 'info'},
            ' cache ': {category: 'webdev', level: 'log'},
            ' res   ': {category: 'webdev', level: 'log'},
            ' DB    ': {category: 'webdev', level: 'log'},
            ' http  ': {category: 'network', level: 'log'},
            ' clnt  ': {category: 'network', level: 'warn'},
            ' srvr  ': {category: 'network', level: 'warn'},
            ' call  ': {category: 'webdev', level: 'log'},
            ' ret   ': {category: 'webdev', level: 'log'},
            ' auth  ': {category: 'server', level: 'log'},
            ' cust1 ': {category: 'js', level: 'warn'},
            ' cust2 ': {category: 'server', level: 'warn'},
            ' cust3 ': {category: 'webdev', level: 'log'},
            ' cust4 ': {category: 'webdev', level: 'log'},
            ' rotat ': {category: 'webdev', level: 'log'},
            ' dddER ': {category: 'webdev', level: 'log'},
            ' dddIN ': {category: 'webdev', level: 'log'},
            ' mon   ': {category: 'webdev', level: 'log'}
        };
        level = logLevelMap[logLevel].level;
        category = logLevelMap[logLevel].category;
    } else {
        timestamp = Date.now();
        category = 'webdev';
        level =  'log';
    }
    return {level: level, category: category, msg: msg, timeStamp: timestamp}
});