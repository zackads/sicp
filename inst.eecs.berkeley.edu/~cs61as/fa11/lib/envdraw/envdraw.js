var common = {
    connector: ["Flowchart", {midpoint:0.9}],
    anchor: ["Left", "Right"],
    endpoint: "Blank"
};

var drawArrow = function(left, right) {
  jsPlumb.connect({
    source: left,
    target: right,
    paintStyle:{ strokeStyle:"black", lineWidth:1 },
    endpointStyle:{ fillStyle:"black", outlineColor:"black" },
    overlays:[ 
      ["PlainArrow" , { width:8, length:8, direction: -1, location:1 }]
    ]
  }, common);
}

$(document).ready(function() {
  $('#main-input').keypress(submit);
  $('#main-input').keydown(navHist);
});

var inputHistory = [];
var historyIndex = 0; // Reverse index
var storedCurrent = false;

var submit = function(event) {
  if (event.keyCode === 13) {
    var input = $('#main-input').val();
    $('#main-input').val('');
    $('#output-container').append('<div class="prompt">&#8594; ' + input + '</div>');
    if (input === '(help)') {
      showErrorMessage('<b>Minimal Environment Diagram Drawer for Racket<br>' +
                       'Version 0.0.3</b><br><br>' +
                       'This interpreter understands a tiny subset of all valid Racket expressions.<br>' +
                       'Its output may not exactly match that of the official Racket interpreter. <br>' +
                       'You should use it for checking your environment diagrams, but not for "real" coding.<br><br>' +
                       'Please report bugs to Allen Guo at <a href="mailto:guoguo12@gmail.com">guoguo12@gmail.com</a>.<br><br>' +
                       'Built using jQuery, Underscore.js, and jsPlumb.'
                        );
      return;
    } else if (input === '(apropos)') {
      showErrorMessage('define<br>let<br>set!<br>lambda<br>if<br>cond<br><br>identity<br>null<br>null?<br>not<br>cons<br>car<br>cdr<br>list<br>reverse<br>length<br>foldl<br>map<br>pi<br>=<br>+<br>-<br>*<br>expt<br>add1<br>sub1');
      return;
    }
    try {
      evalInput(input);
    } catch (err) {
      if (err.name === 'RangeError') {
        showErrorMessage('Maximum recursion limit reached; consider using tail recursion instead :^)');
      } else {
        showErrorMessage('Unexpected JS error: ' + err.message);
        throw err;
      }
    }
    if (storedCurrent) {
      inputHistory.splice(inputHistory.length - 1, 1);
    }
    inputHistory.push(input);
    historyIndex = 0;
    storedCurrent = false;
    draw();
  }
}

var draw = function() {
  var stringifyFunction = function(fn) {
    if (typeof(fn) === 'function') {
      return '(built-in proc)';
    } else if (typeof(fn) === 'object') {
      if (fn === null || (fn.constructor && fn.constructor.name === 'Pair')) {
        return stringify(fn);
      } else {
        return '(' + (fn.name || '&lambda;') + ' ' + fn.params.join(' ') + ')';
      }
    } else {
      return '??';
    }
  };
  
  $('#draw-container').empty();
  jsPlumb.deleteEveryEndpoint();
  
  var arrowsToDraw = [];    
  var drawnFunctions = {};
  environments.forEach(function(e) {
    if (globalEnvInitBindings.indexOf(e.caller) !== -1 || (e.parent && globalEnvInitBindings.indexOf(e.parent.caller) !== -1)) {
      return;
    }

    var elem = $('<div class="environment"></div>');
    
    if (e.envId === 'global') {
      var bindings = _.omit(e.bindings, globalEnvInitBindings);
    } else {
      var bindings = e.bindings;
    }

    if (e.envId === 'global') {
      var callerStr = '';
    } else if (e.caller === undefined) {
      var callerStr = ': &lambda;';
    } else {
      var callerStr = ': ' + e.caller;
    }
    var parentStr = e.parent === null || e.parent.envId === 'global' ? '' : '[parent=' + e.parent.envId + ']';
    elem.append('<b>' + e.envId + callerStr + '</b>');
    elem.append('<b style="float: right">' + parentStr + '</b>');

    _.pairs(bindings).forEach(function(b) {
      var key = b[0];
      var value = b[1];
      if (['number', 'boolean'].indexOf(typeof(value)) !== -1) {
        elem.append('<div class="binding"><span class="binding-key">'
          + b[0] + '</span><span class="binding-value">'
          + stringify(b[1]) + '</span></div>');
      } else {
        var varId = e.envId + '-' + b[0];
        if (drawnFunctions[b[1].toString()] === undefined) {
          var functionId = b[1] === null ? 'null' : b[1].toString();
          elem.append('<div class="binding"><span class="binding-key" id="' + varId + '">'
            + b[0] + '</span><span class="binding-value binding-fn" id="' + functionId + '">'
            + stringifyFunction(b[1]) + '</span></div>');
          drawnFunctions[b[1].toString()] = functionId;
        } else {
          var functionId = drawnFunctions[b[1].toString()];
          elem.append('<div class="binding"><span class="binding-key" id="' + varId + '">'
            + b[0] + '</span></div>');
        }
        arrowsToDraw.push([varId, functionId]);
      }
    });
    
    if (e.returnVal) {
      elem.append('<div class="binding"><span class="binding-key"><i>return value</i></span><span class="binding-value">'
        + stringify(e.returnVal) + '</span></div>');
    }

    $('#draw-container').append(elem);
    arrowsToDraw.forEach(function(pair) {
      console.log('drawing from %o to %o', pair[0], pair[1])
      drawArrow(pair[0], pair[1]);
    });
  });
}

// ??
var navHist = function(event) {
  if (event.keyCode === 38) {
    var index = inputHistory.length - 1 - historyIndex;
    if (index < inputHistory.length && index >= 0) {
      if (!storedCurrent) {
        inputHistory.push($('#main-input').val());
        historyIndex++;
        storedCurrent = true;
      }
      $('#main-input').val(inputHistory[index]);
      historyIndex++;
    }
  } else if (event.keyCode === 40) {
    var index = inputHistory.length - 1 - (historyIndex - 1);
    if (index < inputHistory.length && index >= 0) {
      $('#main-input').val(inputHistory[index]);
      historyIndex--;
    }
  }
}

var saveHistory = function(str) {
  var hist = localStorage.getItem('history');
  if (hist !== null) {
    hist.push(str);
    localStorage.setItem('history', hist);
  } else {
    localStorage.setItem('history', [str]);
  }
}

var results = [];
var silent = false;

BUILT_INS = {
'null': null,
'#f': false,
'#t': true,
'null?': function(args) {
  if (args.length !== 1) {
    return new Error('null?: arity mismatch (expected 1 args, got ' + args.length + ')');    
  }
  return args[0] === null;
},
'cons': function(args) {
  if (args.length !== 2) {
    return new Error('cons: arity mismatch (expected 2 args, got ' + args.length + ')');    
  }
  return new Pair(args[0], args[1]);
},
'car': function(args) {
  if (args.length !== 1) {
    return new Error('car: arity mismatch (expected 1 args, got ' + args.length + ')');    
  }
  if (!(typeof(args[0]) === 'object' && args[0].constructor.name === 'Pair')) {
    return new Error('car: contract violation; argument should be pair');    
  }
  return args[0].car;
},
'cdr': function(args) {
  if (args.length !== 1) {
    return new Error('cdr: arity mismatch (expected 1 args, got ' + args.length + ')');    
  }
  if (!(typeof(args[0]) === 'object' && args[0].constructor.name === 'Pair')) {
    return new Error('cdr: contract violation; argument should be pair');    
  }
  return args[0].cdr;
},
'+': function(args) {
  if (args.length === 0) {
    return new Error('+: arity mismatch (expected 1+ args, got 0)');
  }
  return _.reduce(args, function(a, b) { return a + b; });
},
'-': function(args) {
  if (args.length === 0) {
    return new Error('-: arity mismatch (expected 1+ args, got 0)');
  }
  if (args.length === 1) {
    return -1 * args[0];
  }
  return _.reduce(args, function(a, b) { return a - b; });
},
'*': function(args) {
  if (args.length === 0) {
    return new Error('*: arity mismatch (expected 1+ args, got 0)');
  }
  return _.reduce(args, function(a, b) { return a * b; });
},
'expt': function(args) {
  if (args.length !== 2) {
    return new Error('expt: arity mismatch (expected 2 args, got ' + args.length + ')');  
  }
  return Math.pow(args[0], args[1]);
},
'=': function(args) {
  if (args.length < 2) {
    return new Error('=: arity mismatch (expected 2+ args, got ' + args.length + ')');
  }
  var value = args[0];
  for (var i = 0; i < args.length; i++) {
    if (typeof(args[i]) !== 'number') {
      return new Error('=: contract violation (all arguments should be numbers)');      
    } else if (args[i] !== value) {
      return false;
    }
  } 
  return true;
}
}

var nextId = 0;
var genNextId = function() {
  var id = nextId;
  nextId++;
  return id;
}

function Error(msg) {
  this.err = msg;
}

var initBaseDefinitions = function() {
  silent = true;

  evalInput(
    "(define pi 3.14159265)" +
    "(define (identity x) x)" +
    "(define (not b) (if b #f #t))" +
    "(define (add1 x) (+ 1 x))" +
    "(define (sub1 x) (- x 1))" +
    "(define (list . args) args)" +
    "(define (foldl f acc xs) (if (null? xs) acc (foldl f (f (car xs) acc) (cdr xs))))" +
    "(define (reverse xs) (foldl cons (list) xs))" +
    "(define (map f xs) (reverse (foldl (lambda (x y) (cons (f x) y)) (list) xs)))" +
    "(define (length xs) (foldl (lambda (x y) (+ y 1)) 0 xs))"
  );

  globalEnvInitBindings = _.keys(globalEnv.bindings);

  silent = false;
};

var evalInput = function(input) {
  var tokens = tokenize(input);
  if (tokens.length === 0) {
    return '';
  }
  console.log('%cStarting evaluation of %s', 'color: green', input);
  while (tokens.length > 0) {
    var rawResult = evalNext(tokens, globalEnv);
    if (rawResult === null) {
      showResult("'()"); // Special case since null is not technically a Pair instance
    } else {
      showResult(stringify(rawResult));
    }
  }
}

var tokenize = function(str) {
  str = $.trim(str);
  var chars = str.split('');
  var tokens = [];
  var currentToken = '';
  var parenLevel = 0;
  for (var i = 0; i < chars.length; i++) {
    var currentChar = chars[i];
    if (currentChar.match(/\s/) && currentToken.length > 0) {
      tokens.push(currentToken);
      currentToken = '';
    } else if (currentChar === '(' || currentChar === ')') {
      if (currentToken.length > 0) {
        tokens.push(currentToken);
      }
      tokens.push(currentChar);
      currentToken = '';
      parenLevel += currentChar === '(' ? 1 : -1;
    } else if (!currentChar.match(/\s/)) {
      currentToken += currentChar;
    }
    if (i === chars.length - 1 && currentToken.length > 0) {
      tokens.push(currentToken);
    }
  }
  if (parenLevel !== 0) {
    showWarningMessage('WARNING: unmatched parentheses; code not executed');
    return [];
  }
  console.log("tokens = %o", tokens)
  return tokens;
}

var nextEnvId = 0;

function Environment(parent, caller) {
  var bindings = parent === null ? BUILT_INS : {};
  this.parent = parent;
  this.caller = caller;
  this.bind = function(key, value) {
    bindings[key] = value;
  };
  this.set = function(key, value) {
    if (bindings[key] !== undefined) {
      bindings[key] = value;
    } else if (!parent) {
      showErrorMessage("set!: assignment disallowed; cannot set undefined variable " + key);
    } else {
      parent.set(key, value);
    }
  };
  this.lookup = function(key) {
    var result = bindings[key];
    if (result !== undefined) {
      return result;
    }
    if (parent !== null) {
      return parent.lookup(key);
    } else {
      showErrorMessage(key + ': undefined; cannot reference undefined identifier');
      return '';
    }
  };
  this.bindings = bindings;
  this.envId = nextEnvId === 0 ? 'global' : 'f' + nextEnvId;
  nextEnvId++;
}

function SpecialForm(fn) {
  this.fn = fn;
}

function Lambda(params, bodyTokens, env, name) {
  var id = genNextId();

  this.params = params;
  this.bodyTokens = bodyTokens;
  this.env = env;
  this.name = name; // undefined means anonymous, null means global
  this.toString = function() {
    return 'procedure-' + id;
  }
  this.toHumanString = function() {
    return '#&lt;procedure&gt;';
  }
}

function Pair(car, cdr) {
  var id = genNextId();

  this.car = car;
  this.cdr = cdr;
  this.toString = function() {
    return 'list-' + id;
  }
  this.toHumanString = function() {
    var left = stringify(this.car);
    var right = stringify(this.cdr);
    if (left === '' || right === '') {
      return '';
    }
    if (left.charAt(0) === "'") {
      left = left.substring(1);
    }
    if (right.charAt(0) === "'") {
      right = right.substring(1);
    }
    if (this.cdr === null) {
      return "'(" + left + ')';
    }
    if (typeof(this.cdr) === 'object' && this.cdr.constructor.name === 'Pair') {
      return "'(" + left + ' ' + right.substring(1, right.length - 1) + ')';
    }
    return "'(" + left + ' . ' + right + ')';
  }
}

var stringify = function(o) {
  if (typeof(o) === 'object') {
    if (o === null) {
       return "'()";
    } else if (o.toHumanString) {
      return o.toHumanString();
    } else if (o.toString) {
      return o.toString();
    } else {
      console.error('Unrecognized JS object: %o', o);
      return '<js-object>';
    }
  } else if (typeof(o) === 'function') {
    return '#&lt;procedure&gt;';
  } else if (typeof(o) === 'boolean') {
    return o ? '#t' : '#f';
  } else {
    return String(o);
  }
}

var globalEnv = new Environment(null, null);
var environments = [globalEnv];

var evalNext = function(tokens, env) {
  var popHead = function() {
    return tokens.splice(0, 1)[0];
  }
  if (tokens[0] === '(') {
    popHead(); // Open paren
    var level = 0;
    var inner = [];
    while (!(tokens[0] == ')' && level === 0)) {
      if (tokens[0] === '(') {
        level++;
      } else if (tokens[0] === ')') {
        level--;
      }
      inner.push(popHead());
    }
    popHead(); // Close paren
    var proc = evalNext(inner, env);

    if (typeof(proc) === 'object' && proc.constructor.name === 'SpecialForm') {
      return proc.fn(inner);
    } else {
      var args = [];
      while (inner.length > 0) {
        args.push(evalNext(inner, env));
      }
      return apply(proc, args);
    }
  }
  var next = popHead();

  // If numerical, parse into JS number
  if (next.match(/^[\-\+\.0-9]*$/)) {
    var floatVal = parseFloat(next);
    if (!isNaN(floatVal)) {
      return floatVal;
    }
  }

  // If definition, return JS function that adds binding to current environment
  if (next === 'define') {
    return new SpecialForm(function(tokens) {
      if (tokens.length < 2) {
        showErrorMessage('define: bad syntax');
        return '';
      }
      if (tokens[0] === '(') {
        tokens.splice(0, 1); // Open paren
        var innerTokens = [];
        while (tokens[0] !== ')') {
          innerTokens.push(tokens[0]);
          tokens.splice(0, 1);
        }
        tokens.splice(0, 1); // Close paren
        if (tokens.length === 0) {
          showErrorMessage('define: bad syntax');
          return '';        
        }
        var key = innerTokens.splice(0, 1)[0]; // TODO: Check validity of identifier (e.g. cannot be dot)
        var params = innerTokens;
        if (params.indexOf('.') !== -1) {
          var dotCount = 0;
          params.forEach(function(param) {
            if (param === '.') {
              dotCount++;
            }
          });
          if (dotCount !== 1 || params[params.length - 2] !== '.') {
            showErrorMessage('define: malformed dotted-tail');
            return '';
          }
        }
        env.bind(key, new Lambda(params, tokens, env, key)); // Rest of tokens is body
        return '';
      } else {
        var key = tokens[0];
        tokens.splice(0, 1);
        var value = evalNext(tokens, env);
        if (value === '') {
          console.log('Empty definition expression; skipping');
          return '';
        }
        if (tokens.length > 0) {
          showErrorMessage('define: bad syntax; multiple expressions after identifier');
        }
        env.bind(key, value);
        return '';        
      }
    });
  } else if (next === 'let') {
    return new SpecialForm(function(tokens) {
      var newEnv = new Environment(env, 'let-&lambda;');
      tokens.splice(0, 1); // Open paren
      while (tokens[0] !== ')') {
        tokens.splice(0, 1); // Open paren
        newEnv.bind(tokens.splice(0, 1), evalNext(tokens, env));
        tokens.splice(0, 1); // Close paren
      }
      tokens.splice(0, 1); // Close paren
      environments.push(newEnv);
      var lastResult;
      while (tokens.length > 0) {
        lastResult = evalNext(tokens, newEnv);
      }
      newEnv.returnVal = lastResult;
      return lastResult;      
    });
  } else if (next === 'set!') {
    return new SpecialForm(function(tokens) {
      if (tokens.length < 2) {
        showErrorMessage('set!: bad syntax');
        return '';
      }
      var key = tokens[0];
      tokens.splice(0, 1);
      var value = evalNext(tokens, env);
      if (tokens.length > 0) {
        showErrorMessage('set!: bad syntax; multiple parts after keyword');
      }
      env.set(key, value);
      return '';
    });
  } else if (next === 'lambda') {
    return new SpecialForm(function(tokens) {
      if (tokens.length < 2 || tokens[0] !== '(') {
        showErrorMessage('lambda: bad syntax');
        return '';
      }
      tokens.splice(0, 1); // Open paren
      var params = [];
      while (tokens[0] !== ')') {
        params.push(tokens[0]);
        tokens.splice(0, 1);
      }
      tokens.splice(0, 1); // Close paren
      if (tokens.length === 0) {
        showErrorMessage('lambda: bad syntax');
        return '';        
      }
      return new Lambda(params, tokens, env); // Rest of tokens is body
    });
  } else if (next === 'if') {
    return new SpecialForm(function(tokens) {
      if (tokens.length < 3) {
        showErrorMessage('if: bad syntax');
        return '';
      }
      if (tokens.indexOf('define') !== -1) {
        showErrorMessage('define: not allowed in expression context'); // Why?
        return '';
      }
      var bool = evalNext(tokens, env) !== false;
      if (bool) {
        return evalNext(tokens, env);
      } else {
        discardNext(tokens);
        return evalNext(tokens, env);
      }
    });
  } else if (next === 'cond') {
    return new SpecialForm(function(tokens) {
      if (tokens.length === 0) {
        return null; // TODO: Return actual null
      }
      while (tokens.length >= 4) {
        tokens.splice(0, 1); // Open paren
        if (tokens[0] === 'else') {
          tokens.splice(0, 1);
          return evalNext(tokens, env);
        }
        var bool = evalNext(tokens, env) !== false;
        if (bool) {
          return evalNext(tokens, env);
        } else {
          discardNext(tokens);
        }
        tokens.splice(0, 1); // Close paren
      }
      return null;
    });
  }
  return env.lookup(next);
}

var discardNext = function(tokens) {
  var popHead = function() {
    return tokens.splice(0, 1)[0];
  }  
  if (tokens[0] === '(') {
    popHead(); // Open paren
    var level = 0;
    while (!(tokens[0] == ')' && level === 0)) {
      if (tokens[0] === '(') {
        level++;
      } else if (tokens[0] === ')') {
        level--;
      }
      popHead();
    }
    popHead(); // Close paren
  } else {
    popHead();
  }
}

var apply = function(proc, args) {
  // console.log('APPLY with proc=' + proc + ' and args=' + args + ' (args-length=' + args.length + ')')
  var result;
  if (proc === '') {
    console.log('Empty proc in apply; skipping');
    return '';
  }
  if (args.filter(function(arg) { return arg === ''; }).length > 0) {
    console.log('Empty argument in apply; skipping');
    return '';
  }
  if (typeof(proc) === 'function') {
    result = proc(args);
  } else if (typeof(proc) === 'object') {
    if (proc.constructor.name === 'Lambda') {
      var newEnv = new Environment(proc.env, proc.name);
      if (proc.params.length >= 2 && proc.params[proc.params.length - 2] === '.') {
        // Dotted-tail case
        for (var i = 0; i < proc.params.length - 2; i++) {
          newEnv.bind(proc.params[i], args[i]);
        }
        newEnv.bind(proc.params[proc.params.length - 1], toRacketList(args.slice(proc.params.length - 2)));
      } else if (proc.params.length !== args.length) {
        showErrorMessage('#&lt;procedure&gt;: arity mismatch (expected ' + proc.params.length + ' args, got ' + args.length + ')');
        return '';          
      } else {
        // Normal case
        for (var i = 0; i < proc.params.length; i++) {
          newEnv.bind(proc.params[i], args[i]);
        }        
      }
      environments.push(newEnv);

      var tokens = proc.bodyTokens.slice();
      var lastResult = '';
      while (tokens.length > 0) {
        lastResult = evalNext(tokens, newEnv);
      }
      newEnv.returnVal = lastResult;
      return lastResult;
    }
  } else {
    showErrorMessage('application: not a procedure; ' + proc + ' cannot be applied');
    return '';    
  }
  if (typeof(result) === 'object' && result !== null && result.constructor.name === 'Error') {
    showErrorMessage(result.err);
    return '';
  }
  return result;
}

var toRacketList = function(list) {
  if (list.length === 0) {
    return null;
  }
  if (list.length === 1) {
    return new Pair(list[0], null);
  }
  return new Pair(list[0], toRacketList(list.slice(1)));
}

var showResult = function(result) {
  if (silent) {
    results.push(result);
  } else {
    $('#output-container').append('<div class="result">' + result + '</div>');
  }
}

var showWarningMessage = function(msg) {
  if (silent) {
    // results.push(msg);
  } else {
    $('#output-container').append('<span class="warn">' + msg + '</span>');
  }
}

var showErrorMessage = function(msg) {
  if (silent) {
    // results.push(msg);
  } else {
    $('#output-container').append('<span class="error">' + msg + '</span>');
  }  
}

initBaseDefinitions();
draw();
