(defun jscomp-js-var-sniffer-boilerplate-1 ()
  "Returns boilerplate javascript code to allow sniffing of the
type of a named variable.

The way it works: when a user asks for completion, jscomp
produces a javascript source file containing this boilerplate
code, followed by all code in the buffer being edited, plus some
trailer boilerplate code. The plan is to evaluate all of that
javascript, and get a string that describes the properties of the
thing being completed.

This works if the js-mode buffer is curly-brace complete, that is
to say, if it can be compiled. It also assumes that evaluating
the code doesn't emit anything to the StdOut.

It works in simple cases, not sure about more complex ones.

Some notes:

The boilerplate uses a random number to uniquify the variable
names, to avoid possibility of collision.

The properties of an object are enumerated with a for..in
loop. This doesn't work for built-in members, like those on the
String or RegExp objects. For that reason this logic
special-cases String.  Not so much with RegExp.

"
  (let ((v (concat "emacsJscompHelper_" (jscomp-random-id)))
        (s (concat "stringProps_" (jscomp-random-id))))
    (concat "(function() {
      function say(x) {WScript.Echo(x);}
      var " s
      "= [
     ['charAt', '${1:ix}'],
     ['charCodeAt', '${1:ix}'],
     ['concat', '${1:str2 ${2:, str3}}'],
     ['indexOf', '${1:str}'],
     ['lastIndexOf','${1:str}'],
     ['match','${1:re}'],
     ['replace','${1:re}, ${2:newString}'],
     ['search','${1:re}'],
     ['slice','${1:begin ${2:, optEnd}}'],
     ['split','${1:separator ${2:, optLimit}}'],
     ['substr','${1:start ${2:, optLength}}'],
     ['substring','${1:from ${2:, optionalTo}}'],
     ['toLowerCase'],
     ['toUpperCase']], " v
      "= {
        argString : function(fn) {
            var re1 = new RegExp('[\\n\\t]','g'),
                fnStr = fn.toString().replace(re1,' '),
                re2 = new RegExp('\\\\(.+?\\\\)'),
                argStr = fnStr.match(re2),
                s = argStr.toString();
            return s.substring(1, s.length - 1);
        },
        emitPropListAsSexp: function() {
          var i, p=" v ".propList, L=p.length;
          WScript.StdOut.Write('(list ');
          for(i=0;i<L;i++){
            WScript.StdOut.Write('\\'(\"' + p[i].name + '\" \"' + p[i].type + '\" ');
            if (typeof p[i].args != 'undefined') {
              WScript.StdOut.Write('\"' + p[i].args + '\" ');
            }
            WScript.StdOut.WriteLine(')');
          }
          WScript.StdOut.WriteLine(')');
        },
        propList: null,

        setPropList: function(identifier) {
            var r=[], m=identifier, t, n, e;
            if (typeof m == 'string') {
                e = " s ";
                for(t = 0;t<e.length;t++) {
                    n = {name:e[t][0], type:'function'};
                    if (typeof e[t][1] != 'undefined') {
                      n.args = e[t][1];
                    }
                    r.push(n);
                }
                r.push({name:'length', type:'number'});
            }
            else {
                for(var p in m){
                    t = typeof p;
                    if (isNaN(parseInt(t, 10))) {
                        t = 'index';
                    }
                    else {
                        t = typeof m.p;
                    }
                    n = {name:p, type:t};
                    if (t=='function') { n.args= " v ".argString(m.p);}
                    r.push(n);
                }
            }
            " v ".propList = r;
        }
     };")))


(defun jscomp-js-var-sniffer-boilerplate-2 ()
  "retrieve the var holding the property list. "
  (concat "emacsJscompHelper_" (jscomp-random-id)
          ".emitPropListAsSexp();}());"))

(defun jscomp-get-js-call-set-prop-list (identifier)
  "Returns a string containing Javascript code that, when
invoked, returns the properties of the object in the JS Shell
referred to as IDENTIFIER.

This is used to support completion in the shell.

"
  (concat "emacsJscompHelper_" (jscomp-random-id)
          ".setPropList(" identifier ");"))


