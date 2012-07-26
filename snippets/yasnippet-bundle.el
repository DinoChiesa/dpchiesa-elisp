;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '((nil "do\n{\n    $0\n} while (${1:condition});" "do { ... } while (...)" nil nil nil nil nil nil)
		       (nil "if (${1:condition})\n{\n    $0\n}" "if (...) { ... }" nil nil nil nil nil nil)
		       (nil "#include \"$1\"\n" "#include \"...\"" nil nil nil nil nil nil)
		       (nil "#include <$1>\n" "#include <...>" nil nil nil nil nil nil)
		       (nil "int main(int argc, char const *argv)\n{\n    $0\n    return 0;\n}" "int main(argc, argv) { ... }" nil nil nil nil nil nil)
		       (nil "#ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}\n#define $1\n\n$0\n\n#endif /* $1 */" "#ifndef XXX; #define XXX; #endif" nil nil nil nil nil nil)
		       (nil "typedef struct _${1:name}\n{\n    $0\n} $1;" "struct ... { ... }" nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '((nil "${1:v}.begin(), $1.end" "v.begin(), v.end()" nil nil nil nil nil nil)
		       (nil "class ${1:Name}\n{\npublic:\n    $1($2);\n    virtual ~$1();\n};" "class ... { ... }" nil nil nil nil nil nil)
		       (nil "for (int ${1:index}=0; $1 < ${2:Limit};  $1++) \n{\n    ${3:// body...}\n}\n" "for (...; ...; ...) { ... }" nil nil nil nil nil nil)
		       (nil "using namespace ${std};\n$0" "using namespace ... " nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '((nil "FILE *${fp} = fopen(${\"file\"}, \"${r}\");\n" "FILE *fp = fopen(..., ...);" nil nil nil nil nil nil)
		       (nil "for (${1:index}=0; $1 < ${2:Limit}; $1++)\n{\n    ${3:// body...}\n}\n" "for (...; ...; ...) { ... }" nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '(("args" "for (int i=0; i < args.Length; i++)\n{\n    switch (args[i])\n    {\n        case \"-f\":\n            i++;\n            if (args.Length <= i) throw new ArgumentException(args[i]);\n            _file = args[i];\n            break;\n\n        case \"-p\":\n            i++;\n            if (args.Length <= i) throw new ArgumentException(args[i]);\n            _password = args[i];\n            break;\n\n        case \"-d\":\n            i++;\n            if (args.Length <= i) throw new ArgumentException(args[i]);\n            _dir = args[i];\n            break;\n\n        case \"-i\":\n            i++;\n            if (args.Length <= i) throw new ArgumentException(args[i]);\n            if (_intParam != DefaultIntParamValue)\n                throw new ArgumentException(args[i]);\n            if (args[i].StartsWith(\"0x\"))\n                _intParam = System.Int32.Parse(args[i].Substring(2), System.Globalization.NumberStyles.AllowHexSpecifier );\n            else\n                _intParam = System.Int32.Parse(args[i]);\n            break;\n\n\n        case \"-s\":\n            i++;\n            if (args.Length <= i) throw new Exception(args[i-1]);\n            if (args[i].ToUpper().EndsWith(\"K\"))\n                _size = System.Int32.Parse(args[i].Substring(0,args[i].Length-1)) * 1024;\n            else if (args[i].ToUpper().EndsWith(\"KB\"))\n                _size = System.Int32.Parse(args[i].Substring(0,args[i].Length-2)) * 1024;\n            else if (args[i].ToUpper().EndsWith(\"M\"))\n                _size = System.Int32.Parse(args[i].Substring(0,args[i].Length-1)) * 1024*1024;\n            else if (args[i].ToUpper().EndsWith(\"MB\"))\n                _size = System.Int32.Parse(args[i].Substring(0,args[i].Length-2)) * 1024*1024;\n            else\n                _size = Int32.Parse(args[i]);\n            break;\n\n\n\n        case \"-?\":\n            throw new ArgumentException(args[i]);\n\n        default:\n            if (_positionalArg != null)\n                throw new ArgumentException(args[i]);\n\n            _positionalArg = args[i];\n            break;\n    }\n}\n" "switch on args" nil nil nil nil nil nil)
		       ("ass" "\n[assembly: AssemblyTitle(\"$1\")]\n[assembly: AssemblyCompany(\"${2:Dino Chiesa}\")]\n[assembly: AssemblyProduct(\"$3\")]\n[assembly: AssemblyCopyright(\"Copyright © Dino Chiesa 2009\")]\n[assembly: AssemblyTrademark(\"\")]\n[assembly: AssemblyCulture(\"\")]\n[assembly: AssemblyConfiguration(\"\")]\n[assembly: AssemblyDescription(\"$4\")]\n[assembly: AssemblyVersion(\"${5:1.0.1.0}\")]\n[assembly: AssemblyFileVersion(\"${6:1.0.1.0}\")]\n\n" "assembly info" nil nil nil nil nil nil)
		       ("bsd" "// Licensed under the Simplified BSD License.\n//\n// Redistribution and use in source and binary forms, with or without\n// modification, is permitted provided that the following conditions\n// are met: Redistributions of source code must retain the above\n// copyright notice, this list of conditions and the following\n// disclaimer.  Redistributions in binary form must reproduce the above\n// copyright notice, this list of conditions and the following\n// disclaimer in the documentation and/or other materials provided with\n// the distribution.\n//\n// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n// \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\n// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT\n// HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,\n// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,\n// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS\n// OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED\n// AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT\n// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY\n// WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n// POSSIBILITY OF SUCH DAMAGE.\n//\n// ------------------------------------------------------------------\n" "BSD License" nil nil nil nil nil nil)
		       ("ca" "  List<String> values = new List<String>() { \"7\", \"13\", \"41\", \"3\" };\n\n  // ConvertAll maps the given delegate across all the List elements\n  var foo = values.ConvertAll((s) => { return System.Convert.ToInt32(s); }) ;\n\n  System.Console.WriteLine(\"typeof(foo) = {0}\", foo.GetType().ToString());\n\n  var a = values.ToArray();\n  Array.ForEach(a, Console.WriteLine);\n\n  Array.ConvertAll(a, x=> System.Convert.ToInt32(x));\n" "ConvertAll((s) => { ... });" nil nil nil nil nil nil)
		       ("cla" "public class ${1:Classname}\n{\n  // default ctor\n  public ${1:Classname}()\n  {\n  }\n\n  ${2:// methods here}\n}" "class ... { ... }" nil nil nil nil nil nil)
		       ("conv" "  List<String> Values = new List<String>() { \"7\", \"13\", \"41\", \"3\" };\n\n  // ConvertAll maps the given delegate across all the List elements\n  var foo = Values.ConvertAll((s) => { return System.Convert.ToInt32(s); }) ;\n\n  System.Console.WriteLine(\"typeof(foo) = {0}\", foo.GetType().ToString());\n\n  Array.ForEach(foo.ToArray(),Console.WriteLine);\n" "ConvertAll((s) => { ... });" nil nil nil nil nil nil)
		       ("cconvert" "  List<String> Values = new List<String>() { \"7\", \"13\", \"41\", \"3\" };\n\n  // ConvertAll maps the given delegate across all the List elements\n  var foo = Values.ConvertAll((s) => { return System.Convert.ToInt32(s); }) ;\n\n  System.Console.WriteLine(\"typeof(foo) = {0}\", foo.GetType().ToString());\n\n  Array.ForEach(foo.ToArray(),Console.WriteLine);\n" "ConvertAll((s) => { ... });" nil nil nil nil nil nil)
		       ("doc" "/// <summary>\n/// ${1:summary}\n/// </summary>\n///\n/// <remarks>\n/// ${2:remarks}\n/// </remarks>\n$0" "XML Documentation" nil nil nil nil nil nil)
		       ("for" "for (int ${1:index}=0; $1 < ${2:Limit}; $1++)\n{\n    ${3:// body...}\n}" "for (...) { ... }" nil nil nil nil nil nil)
		       ("fore" "foreach (${1:type} ${2:var} in ${3:IEnumerable})\n{\n    ${4:// body...}\n}" "foreach ... { ... }" nil nil nil nil nil nil)
		       ("fs" "using(FileStream ${1:src} = File.OpenRead(${2:fname}))\n{\n\n}\n\n" "File.OpenRead(...)" nil nil nil nil nil nil)
		       ("gs" "  { get; set; }\n" "getset" nil nil nil nil nil nil)
		       ("ife" "if (${1:predicate})\n{\n  ${2:// then clause}\n}\nelse\n{\n  ${3:// else clause}\n}" "if (...) { ... } else { ... }" nil nil nil nil nil nil)
		       ("ofd" "var dlg = new System.Windows.Forms.OpenFileDialog();\ndlg.Filter = \"${1:filter string}\"; // ex: \"C# (*.cs)|*.cs|Text (*.txt)|*.txt\";\nif (dlg.ShowDialog() == DialogResult.OK)\n{\n    string fileName = dlg.FileName;\n    $0\n}\n" "new OpenFileDialog; if (DialogResult.OK) { ... }" nil nil nil nil nil nil)
		       ("pa" "        for (int i=0; i < args.Length; i++)\n        {\n            switch (args[i])\n            {\n            case \"-?\":\n            case \"-help\":\n                throw new ArgumentException(args[i]);\n\n            default: // positional args\n                if ($2 != 0)\n                    // we have all the args we need\n                    throw new ArgumentException(args[i]);\n\n                if ($1 == null)\n                    $1 = args[i];\n\n                else\n                    $2 = System.Int32.Parse(args[i]);\n\n                break;\n            }\n        }\n\n        // check values\n        if ($1 == null)\n            throw new ArgumentException();\n\n        if ($2 == 0)\n            throw new ArgumentException();\n\n\n" "switch(args[0]) {...}" nil nil nil nil nil nil)
		       ("prop" "private $1 _$2;\npublic ${1:Type} ${2:Name}\n{\n  get\n  {\n    ${3:get impl};\n  }\n\n  set\n  {\n    ${4:set impl};\n  }\n\n}" "property ... { ... }" nil nil nil nil nil nil)
		       ("pump" "        using(Stream src = File.OpenRead(${1:fname1}),\n              dest= File.Create(${2:fname2}))\n          {\n            if (compress)\n              Compress(src, dest);\n            else\n              Decompress(src, dest);\n          }\n\n" "File.OpenRead(...)" nil nil nil nil nil nil)
		       ("setting" " #region Property${1:PropName}\n\n    private string default$1 = ${2:defaultValue};\n    private string _$1;\n    public string $1\n    {\n        get\n        {\n                if (_$1 == null)\n                {\n                    _$1 = System.Configuration.ConfigurationManager.AppSettings[\"$1\"];\n\n                    if (string.IsNullOrEmpty(_$1))\n                    {\n                        _$1 = default$1;\n                    }\n                }\n                return this._$1;\n            }\n        set\n        {\n            string new$1 = value;\n            // optional validation:\n            //Validation.EnforceXxxx(new$1, \"$1\");\n            _$1 = new$1;\n        }\n    }\n\n#endregion\n" "config setting" nil nil nil nil nil nil)
		       ("singleton" "#region Singleton ${1:className}\npublic sealed class $1\n{\n    private readonly static $1 _instance = new $1();\n\n    public static $1 Instance  { get { return _instance; } }\n\n    private $1()\n    {\n        // implementation here\n    }\n}\n\n#endregion\n" "public sealed class Singleton {...}" nil nil nil nil nil nil)
		       ("sum" "/// <summary>\n///  ${1:description}\n/// </summary>\n///\n/// <remarks>\n///  ${2:comments}\n/// </remarks>\n" "/// <summary>..." nil nil nil nil nil nil)
		       ("try" "try\n{\n  $0\n}\ncatch (System.Exception exc1)\n{\n  throw new Exception(\"uncaught exception\", exc1);\n}" "try { ... } catch { ... }" nil nil nil nil nil nil)
		       ("using" "using (${1:type} ${2:var} = new ${1:type}(${3:ctor args}))\n{\n    ${4:// body...}\n}" "using ... { ... }" nil nil nil nil nil nil)
		       ("while" "while (${1:condition})\n{\n    ${0://thing to do}\n}" "while (...) { ... }" nil nil nil nil nil nil)
		       ("wl" "Console.WriteLine(${0:\"{0}\"});\n" "WriteLine (...) { ... }" nil nil nil nil nil nil)
		       ("xmls" "{\n      XmlSerializer s1 = new XmlSerializer(typeof(${1:type}));\n\n      // use this to \"suppress\" the default xsd and xsd-instance namespaces\n      XmlSerializerNamespaces ns = new XmlSerializerNamespaces();\n      ns.Add(\"\", \"\");\n\n      s1.Serialize(new XTWFND(System.Console.Out), object, ns);\n      System.Console.WriteLine(\"\\n\");\n}\n\n  $0\n  /// XmlTextWriterFormattedNoDeclaration\n  /// helper class : eliminates the XML Documentation at the\n  /// start of a XML doc.\n  /// XTWFND = XmlTextWriterFormattedNoDeclaration\n  /// usage:       s1.Serialize(new XTWFND(System.Console.Out), thing, ns);\n\n  public class XTWFND : System.Xml.XmlTextWriter\n  {\n    public XTWFND(System.IO.StringWriter w) : base(w) { Formatting=System.Xml.Formatting.Indented;  }\n    public XTWFND(System.IO.TextWriter w) : base(w) { Formatting = System.Xml.Formatting.Indented; }\n    public XTWFND(System.IO.Stream s) : base(s, null) { Formatting = System.Xml.Formatting.Indented; }\n    public XTWFND(string filename) : base(filename, null) { Formatting = System.Xml.Formatting.Indented; }\n    public override void WriteStartDocument() { }\n  }\n\n" "xmlserializer { ... }" nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '((nil "background-color: #${1:DDD};" "background-color: ..." nil nil nil nil nil nil)
		       (nil "background-image: url($1);" "bgi: ..." nil nil nil nil nil nil)
		       (nil "border: ${1:1px} ${2:solid} #${3:999};" "border size style color" nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '(("bsd" ";;; License:\n;;\n;; Licensed under the Simplified BSD License.\n;;\n;; Redistribution and use in source and binary forms, with or without\n;; modification, is permitted provided that the following conditions\n;; are met: Redistributions of source code must retain the above\n;; copyright notice, this list of conditions and the following\n;; disclaimer.  Redistributions in binary form must reproduce the above\n;; copyright notice, this list of conditions and the following\n;; disclaimer in the documentation and/or other materials provided with\n;; the distribution.\n;;\n;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n;; \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\n;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT\n;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,\n;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,\n;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS\n;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED\n;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT\n;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY\n;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n;; POSSIBILITY OF SUCH DAMAGE.\n;;\n;; ------------------------------------------------------------------\n" "BSD License" nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '(("body" "<body>\n  $0\n</body>\n" "<body> ...</body>" nil nil nil nil nil nil)
		       ("div" "div${1: id=\"${2:div1}\"}>$0</div>\n" "<div> ...</div>" nil nil nil nil nil nil)
		       ("div" "<div class='$1'>$0</div>\n" "<div class='...'>...</div>" nil nil nil nil nil nil)
		       ("div" "<div id='$1'>$0</div>\n" "<div id='...'>...</div>" nil nil nil nil nil nil)
		       ("doctype" "<!DOCTYPE HTML>" "DocType HTML 5" nil nil nil nil nil nil)
		       ("doctype" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" "DocType HTML 4.01 Strict" nil nil nil nil nil nil)
		       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset" nil nil nil nil nil nil)
		       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil nil nil nil nil nil)
		       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil nil nil nil nil nil)
		       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil nil nil nil nil nil)
		       ("form" "<form action='$1' method='${2:GET}'>\n  <fieldset>\n    <ul>\n      <li>\n        <label>$3</label>\n        <input type='text' id='$4' name='$4' value=''/>\n      </li>\n    </ul>\n  </fieldset>\n</form>\n" "<form> ...</form>" nil nil nil nil nil nil)
		       ("html" "<html>\n  <head>\n  $0\n  </head>\n  <body>\n  </body>\n</html>\n" "<html> ...</html>" nil nil nil nil nil nil)
		       ("jqplot" "<script type='text/javascript'\n        src='http://cachedcommons.org/cache/jqplot/0.9.7/javascripts/jqplot.js'>\n</script>\n" "<script src='...jqplot.js'> ...</script>" nil nil nil nil nil nil)
		       ("jquery" "<script type='text/javascript'\n        src='https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.js'>\n</script>\n" "<script src='...jquery.js'> ...</script>" nil nil nil nil nil nil)
		       ("jquery-min" "<script type='text/javascript'\n        src='http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js'>\n</script>\n" "<script src='...jquery.min.js'> ...</script>" nil nil nil nil nil nil)
		       ("jquery-ui" "<script type='text/javascript'\n        src='https://ajax.googleapis.com/ajax/libs/jqueryui/1.8.18/jquery-ui.min.js'>\n</script>\n<link rel=\"stylesheet\"\n      href=\"https://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/${1:$$(yas/choose-value '(\"redmond\" \"humanity\" \"start\" \"ui-lightness\" \"smoothness\" \"base\"))}/jquery-ui.css\"\n      type=\"text/css\"\n      media=\"screen\">\n$0" "<script src='...jquery-ui.js'> ...</script>" nil nil nil nil nil nil)
		       ("link" "<link rel='stylesheet' type='text/css' media='screen'\n      href='$1'>\n$0\n" "<link..> ...</link>" nil nil nil nil nil nil)
		       ("link" "<link href='http://fonts.googleapis.com/css?family=$1'\n      rel='stylesheet' type='text/css'>\n$0\n" "<link..google font> ...</link>" nil nil nil nil nil nil)
		       ("reset" "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://yui.yahooapis.com/3.0.0/build/cssreset/reset-min.css\">\n" "<link href='...yahoo-reset....css'>" nil nil nil nil nil nil)
		       ("script" "<script type='text/javascript' src='$1'>\n  $0\n</script>\n" "<script..> ...</script>" nil nil nil nil nil nil)
		       ("span" "<span>$0</span>\n" "<span> ...</span>" nil nil nil nil nil nil)
		       ("span" "<span class='$1'>$0</span>\n" "<span class='...'> ...</span>" nil nil nil nil nil nil)
		       ("span" "<span id='$1'>$0</span>\n" "<span id='...'> ...</span>" nil nil nil nil nil nil)
		       ("style" "<style type='text/css'>\n  $0\n</style>\n" "<style..> ...</style>" nil nil nil nil nil nil)
		       ("ui" "<script type='text/javascript'\n        src='https://ajax.googleapis.com/ajax/libs/jqueryui/1.8.18/jquery-ui.min.js'>\n</script>\n<link rel=\"stylesheet\"\n      href=\"https://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/${1:$$(yas/choose-value '(\"redmond\" \"humanity\" \"start\" \"ui-lightness\" \"smoothness\" \"base\"))}/jquery-ui.css\"\n      type=\"text/css\"\n      media=\"screen\">\n$0" "<script src='...jquery-ui.js'> ...</script> <link...>" nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '(("args" "var L = WScript.Arguments.length,\n    i, file, password, intParam, positionalArg = null, arg;\nif (L > 0) {\n  for (i=0; i < L; i++) {\n    arg = WScript.Arguments(i);\n    switch (arg) {\n      case \"-f\":\n          i++;\n          if (L <= i) { throw { \"error\" : \"missing argument: -f\"}; }\n          file = WScript.Arguments(i);\n          break;\n\n      case \"-p\":\n          i++;\n          if (L <= i) { throw { \"error\" : \"missing argument: -p\"}; }\n          password = WScript.Arguments(i);\n          break;\n\n      case \"-i\":\n          i++;\n          if (L <= i) { throw { \"error\" : \"missing argument: -i\"}; }\n          intParam = parseInt(WScript.Arguments(i), 10);\n          break;\n\n      case \"-?\":\n          throw {\"error\" : \"User wants help\"};\n\n      default:\n          if (positionalArg !== null) {\n              throw  {\"error\" : \"positional arg specified more than once\"};\n          }\n\n          positionalArg = arg;\n          break;\n    }\n  }\n}\n" "switch on args" nil nil nil nil nil nil)
		       ("bsd" "// Licensed under the Simplified BSD License.\n//\n// Redistribution and use in source and binary forms, with or without\n// modification, is permitted provided that the following conditions\n// are met: Redistributions of source code must retain the above\n// copyright notice, this list of conditions and the following\n// disclaimer.  Redistributions in binary form must reproduce the above\n// copyright notice, this list of conditions and the following\n// disclaimer in the documentation and/or other materials provided with\n// the distribution.\n//\n// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n// \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\n// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT\n// HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,\n// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,\n// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS\n// OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED\n// AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT\n// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY\n// WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n// POSSIBILITY OF SUCH DAMAGE.\n//\n// ------------------------------------------------------------------\n" "BSD License" nil nil nil nil nil nil)
		       ("ctf" "f = fso.CreateTextFile(${1:filename});\n\n\n" "fso.CreateTextFile" nil nil nil nil nil nil)
		       ("ctor" "var ${1:fname} =  function() {\n    var instance = {}; // empty object\n\n    // private members\n    var privateField = 0;\n    var privateMethod = function (a,b,c) {}; // empty fn\n\n    // public members\n    instance.publicField = 39;\n    instance.publicMethod = function(a, b, c) {\n      // can call privateMethod here\n    }\n    return instance;\n}\n" "ctor ... { ... }" nil nil nil nil nil nil)
		       ("dompp" "function prettyPrintXmlDom(dom) {\n    // dom is MSXML2.DomDocument\n    var writer = new ActiveXObject(\"MSXML2.MXXMLWriter\"),\n        reader = new ActiveXObject(\"MSXML2.SAXXMLReader\");\n    writer.indent = true;\n    writer.omitXMLDeclaration = true;\n    reader.contentHandler = writer;\n    reader.parse(dom);\n    return writer.output;\n}\n$0\n" "function prettyPrintXmlDom(dom) {...}" nil nil nil nil nil nil)
		       ("domsave" "function saveDomWithIndent(dom, filename) {\n    var writer = new ActiveXObject(\"MSXML2.MXXMLWriter\"),\n        reader = new ActiveXObject(\"MSXML2.SAXXMLReader\"),\n        fso = new ActiveXObject(\"Scripting.FileSystemObject\"),\n        textStream = fso.CreateTextFile(filename, true);\n    writer.indent = true;\n    writer.omitXMLDeclaration = true;\n    reader.contentHandler = writer;\n    reader.parse(dom);\n    textStream.Write(writer.output);\n    textStream.Close();\n}\n$0\n" "function saveDomWithIndent(dom) {...}" nil nil nil nil nil nil)
		       ("eachFile" "function eachFile (dirName, a2, a3) {\n    var fso = new ActiveXObject(\"Scripting.FileSystemObject\"),\n        f = fso.GetFolder(dirName),\n        e, fileName, ext,\n        count = 0, regex, fn;\n\n    if (typeof a2 == \"function\") {\n        fn = a2;\n    }\n    else if (typeof a2 == \"string\") {\n        regex = (a2) ? new RegExp(a2) : null;\n        if (typeof a3 == \"function\") {\n            fn = a3;\n        }\n    }\n    if (typeof fn != \"function\") {\n        throw {'error':'fn must be a function'};\n    }\n\n    for (e= new Enumerator(f.files); !e.atEnd(); e.moveNext()) {\n        fileName = e.item().name;\n        if ( (regex === null) || regex.test(fileName)) {\n            ext = fileName.substring(fileName.lastIndexOf('.') + 1);\n            fn(fileName, count, e.item().path, ext);\n            count++;\n        }\n    }\n}\n$0\n\n" "function eachFile (dirName, fn) {...fso...}" nil nil nil nil nil nil)
		       ("eachSubdir" "function eachSubdir (dirName, a2, a3) {\n    var fso = new ActiveXObject(\"Scripting.FileSystemObject\"),\n        f = fso.GetFolder(dirName), e,\n        subdirName, ext, count = 0,\n        fn, regex;\n\n    if (typeof a2 == \"function\") {\n        fn = a2;\n    }\n    else if (typeof a2 == \"string\") {\n        regex = (a2) ? new RegExp(a2) : null;\n        if (typeof a3 == \"function\") {\n            fn = a3;\n        }\n    }\n    if (typeof fn != \"function\") {\n        throw {'error':'fn must be a function'};\n    }\n\n    for (e = new Enumerator(f.Subfolders); !e.atEnd(); e.moveNext()) {\n        subdirName = e.item().name;\n        if ( (regex === null) || regex.test(subdirName)) {\n          ext = subdirName.substring(subdirName.lastIndexOf('.') + 1);\n          fn(subdirName, count, e.item().path, ext);\n          count++;\n        }\n    }\n}\n$0\n\n" "function eachSubdir (dirName, fn) {...fso...}" nil nil nil nil nil nil)
		       ("fn" "function ${1:fnName} (${2:args}) {\n    ${3:// body...}\n}" "function fnName (...) { ... }" nil nil nil nil nil nil)
		       ("for" "for (var ${1:index}=0; $1<${2:Limit}; $1++) {\n    ${3:// body...}\n}" "for (...) { ... }" nil nil nil nil nil nil)
		       ("fori" "for (var ${1:var} in  ${2:object}) {\n    ${3:// body...}\n}" "for (var x in foo) { ... }" nil nil nil nil nil nil)
		       ("fs" "var fileStream = fso.openTextFile(filename);\n" "fso.OpenTextFile" nil nil nil nil nil nil)
		       ("fso" "var fso = new ActiveXObject(\"Scripting.FileSystemObject\");\n\n" "new Scripting.FileSystemObject" nil nil nil nil nil nil)
		       ("fn" "function ${1:name} (${2:args}) {\n    ${3:// body...}\n}" "function X (...) { ... }" nil nil nil nil nil nil)
		       ("ife" "if (${1:predicate}) {\n  ${2:// then clause}\n} else {\n  ${3:// else clause}\n}" "if (...) { ... } else { ... }" nil nil nil nil nil nil)
		       ("inc" "function readAllText(filename) {\n    var fso = new ActiveXObject(\"Scripting.FileSystemObject\"),\n        fs = fso.openTextFile(filename),\n        fileData = fs.readAll();\n    fs.Close();\n    return fileData;\n}\nfunction includeFile(filename) {\n    /*jslint evil: true */\n    eval(readAllText(filename));\n}\n\nincludeFile(${1:\"c:\\\\\\\\dev\\\\\\\\js\\\\\\\\dateExtensions.js\"});\n" "includeFile" nil nil nil nil nil nil)
		       ("jqajax" "$.ajax({\n  type: 'GET',\n  url: ${1:url},\n  dataType: \"${2:$$(yas/choose-value '(\"html\" \"json\" \"jsonp\" \"script\" \"text\" \"xml\"))}\",\n  cache: ${3:$$(yas/choose-value '(\"true\" \"false\"))},\n  success: function(data, textStatus, jqXHR) {\n    $0\n  }\n});\n\n\n" "$.ajax({...});" nil nil nil nil nil nil)
		       ("class" "// makeClass - By John Resig (MIT Licensed)\n// Allows either new User() or User() to be employed for construction.\nif ( typeof makeClass !== \"function\" )\n  function makeClass(){\n    return function(args){\n      if ( this instanceof arguments.callee ) {\n        if ( typeof this.init == \"function\" ) {\n          this.init.apply( this, args.callee ? args : arguments );\n        }\n      } else {\n        return new arguments.callee( arguments );\n      }\n    };\n  }\n}\n\n// usage:\n// ------\n// class implementer:\n//   var MyType = makeClass();\n//   MyType.prototype.init = function(a,b,c) {/* ... */};\n// ------\n// class user:\n//   var instance = new MyType(\"cats\", 17, \"September\");\n//      -or-\n//   var instance = MyType(\"cats\", 17, \"September\");\n//\n" "Resig's makeClass { ... }" nil nil nil nil nil nil)
		       ("move" "fso.MoveFile(${1:source},${2:destination});\n\n\n" "fso.MoveFile" nil nil nil nil nil nil)
		       ("mem" "${1:Type}.prototype.${2:fnName} = function () {\n  $3\n}\n" "prototype fn" nil nil nil nil nil nil)
		       ("re" "var ${1:re} = new RegExp(${2:\"[^abc]+\"}, ${3:'gi'});\n\n" "new RegExp()" nil nil nil nil nil nil)
		       ("readAll" "function readAllText(filename) {\n    var fso = new ActiveXObject(\"Scripting.FileSystemObject\"),\n        fs = fso.openTextFile(filename),\n        fileData = fs.readAll();\n    fs.Close();\n    return fileData;\n}\n" "function readAll(..) {..fso...}" nil nil nil nil nil nil)
		       ("eachLine" "function eachLine(filename, fn) {\n    var fso = new ActiveXObject(\"Scripting.FileSystemObject\"),\n        textStream = fso.OpenTextFile(r.outputfile, OpenMode.ForReading),\n        results = [];\n\n    // Read from the file and parse the results.\n    while (!textStream.AtEndOfStream) {\n        oneLine = textStream.ReadLine();\n        results.push(fn(oneLine));\n    }\n    textStream.Close();\n    return results;\n}\n" "function eachLine(f,fn) {..fso...}" nil nil nil nil nil nil)
		       ("say" "function say(x) {\n    WScript.Echo(x);\n}\n" "function say(...)" nil nil nil nil nil nil)
		       ("sing" "var ${1:fname} = (function () {\n    var privateVariable;\n    function privateFunction(x) {\n        ...privateVariable...\n    }\n\n    // return an object, that contains all the\n    // public functions and properties\n    return {\n        firstPublicMethod: function (a, b) {\n            ...privateVariable...\n        },\n        secondPublicMethod: function (c) {\n            ...privateFunction()...\n        }\n    };\n})();\n" "singleton { ... }" nil nil nil nil nil nil)
		       ("sub" "${1:S}.substr(${2:start},${3:optionalLength});\n\n" "S.substr(...)" nil nil nil nil nil nil)
		       ("sub" "${1:S}.substring(${2:from},${3:optionalTo});\n\n" "S.substring(...)" nil nil nil nil nil nil)
		       ("try" "try {\n  $0\n}\ncatch (exc1) {\n  //\n}" "try { ... } catch { ... }" nil nil nil nil nil nil)
		       ("wrap" "(function (globalScope) {\n  ${0:...}\n}(this));\n" "fn wrapper" nil nil nil nil nil nil)
		       ("xhr" "function getXhr() {\n  if (typeof XMLHttpRequest != \"undefined\") {\n    return new XMLHttpRequest();\n  }\n  else if (typeof ActiveXObject != \"undefined\") {\n    return new ActiveXObject('MSXML2.XMLHTTP');\n  }\n  else {\n    throw {error: \"Could not create XMLHttpRequest\"};\n  }\n}\n\nvar xhr = getXhr();\n\nxhr.open(${1:method}, ${2:url}, ${3:asynchronous});\nxhr.setRequestHeader('X-Requested-With','XMLHttpRequest');\nxhr.setRequestHeader('Something', 'Whatever');\nxhr.send(query);\n" "fn getXhr(){...};" nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '(("cdata" "<![CDATA[\n $0\n]]>\n" "CDATA" nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '((nil "eval {\n    ${1:# do something risky...}\n};\nif (\\$@) {\n    ${2:# handle failure...}\n}" "eval { ... } if ($@) { ... }" nil nil nil nil nil nil)
		       (nil "for (my \\$${1:var} = 0; \\$$1 < ${2:expression}; \\$$1++) {\n    ${3:# body...}\n}" "for (...) { ... }" nil nil nil nil nil nil)
		       (nil "foreach my \\$${1:x} (@${2:array}) {\n    ${3:# body...}\n}" "foreach ... { ... }" nil nil nil nil nil nil)
		       (nil "if ($1) {\n    $0\n}" "if (...) { ... }" nil nil nil nil nil nil)
		       (nil "if ($1) {\n    $2\n} else {\n    $3\n}" "if (...) { ... } else { ... }" nil nil nil nil nil nil)
		       (nil "if ($1) {\n	${2:# body...}\n} elsif ($3) {\n	${4:# elsif...}\n} else {\n	${5:# else...}\n}" "if, elsif, else ..." nil nil nil nil nil nil)
		       (nil "sub ${1:function_name} {\n    $0\n}" "sub ... { ... }" nil nil nil nil nil nil)
		       (nil "unless ($1) {\n    $0\n}" "unless (...) { ... }" nil nil nil nil nil nil)
		       (nil "while ($1) {\n    $0\n}" "while (...) { ... }" nil nil nil nil nil nil)
		       (nil "${1:expression} foreach @${2:array};" "... foreach ..." nil nil nil nil nil nil)
		       (nil "${1:expression} if ${2:condition}" "... if ..." nil nil nil nil nil nil)
		       (nil "${1:expression} unless ${2:condition}" "... unless ..." nil nil nil nil nil nil)
		       (nil "${1:expression} while ${2:condition};" "... while ..." nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '(("abs" "abs(${number})$0\n" "abs(...)" nil nil nil nil nil nil)
		       ("acos" "acos(${arg})$0\n" "acos(...)" nil nil nil nil nil nil)
		       ("acosh" "acosh(${arg})$0\n" "acosh(...)" nil nil nil nil nil nil)
		       ("addcslashes" "addcslashes(${str}, ${charlist})$0\n" "addcslashes(..., ...)" nil nil nil nil nil nil)
		       ("addslashes" "addslashes(${str})$0\n" "addslashes(...)" nil nil nil nil nil nil)
		       ("array" "array(${[mixed ...]})$0\n" "array(...)" nil nil nil nil nil nil)
		       ("array.change.key.case" "array_change_key_case(${input[, int case]})$0\n" "array_change_key_case(...)" nil
			("array")
			nil nil nil nil)
		       ("array.chunk" "array_chunk(${input}, ${size[, bool preserve_keys]})$0\n" "array_chunk(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.count.values" "array_count_values(${input})$0\n" "array_count_values(...)" nil
			("array")
			nil nil nil nil)
		       ("array.diff" "array_diff(${array1}, ${array2[, array ...]})$0\n" "array_diff(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.diff.assoc" "array_diff_assoc(${array1}, ${array2[, array ...]})$0\n" "array_diff_assoc(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.diff.key" "array_diff_key(${array1}, ${array2[, array ...]})$0\n" "array_diff_key(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.diff.ukey" "array_diff_ukey(${array1}, ${array2[, array ..., callback key_compare_func]})$0\n" "array_diff_ukey(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.fill" "array_fill(${start_index}, ${num}, ${value})$0\n" "array_fill(..., ..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.filter" "array_filter(${input[, callback callback]})$0\n" "array_filter(...)" nil
			("array")
			nil nil nil nil)
		       ("array.flip" "array_flip(${trans})$0\n" "array_flip(...)" nil
			("array")
			nil nil nil nil)
		       ("array.intersect" "array_intersect(${array1}, ${array2[, array ...]})$0\n" "array_intersect(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.intersect.assoc" "array_intersect_assoc(${array1}, ${array2[, array ...]})$0\n" "array_intersect_assoc(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.intersect.key" "array_intersect_key(${array1}, ${array2[, array ...]})$0\n" "array_intersect_key(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.intersect.ukey" "array_intersect_ukey(${array1}, ${array2[, array ..., callback key_compare_func]})$0\n" "array_intersect_ukey(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.key.exists" "array_key_exists(${key}, ${search})$0\n" "array_key_exists(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.keys" "array_keys(${input[, mixed search_value]})$0\n" "array_keys(...)" nil
			("array")
			nil nil nil nil)
		       ("array.map" "array_map(${callback}, ${arr1[, array ...]})$0\n" "array_map(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.merge" "array_merge(${array1[, array array2]})$0\n" "array_merge(...)" nil
			("array")
			nil nil nil nil)
		       ("array.merge.recursive" "array_merge_recursive(${array1[, array ...]})$0\n" "array_merge_recursive(...)" nil
			("array")
			nil nil nil nil)
		       ("array.multisort" "array_multisort(${ar1[, mixed arg]})$0\n" "array_multisort(...)" nil
			("array")
			nil nil nil nil)
		       ("array.pad" "array_pad(${input}, ${pad_size}, ${pad_value})$0\n" "array_pad(..., ..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.pop" "array_pop(${&array})$0\n" "array_pop(...)" nil
			("array")
			nil nil nil nil)
		       ("array.product" "array_product(${array})$0\n" "array_product(...)" nil
			("array")
			nil nil nil nil)
		       ("array.push" "array_push(${&array}, ${var[, mixed ...]})$0\n" "array_push(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.rand" "array_rand(${input[, int num_req]})$0\n" "array_rand(...)" nil
			("array")
			nil nil nil nil)
		       ("array.reduce" "array_reduce(${input}, ${function[, int initial]})$0\n" "array_reduce(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.reverse" "array_reverse(${array[, bool preserve_keys]})$0\n" "array_reverse(...)" nil
			("array")
			nil nil nil nil)
		       ("array.search" "array_search(${needle}, ${haystack[, bool strict]})$0\n" "array_search(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.shift" "array_shift(${&array})$0\n" "array_shift(...)" nil
			("array")
			nil nil nil nil)
		       ("array.slice" "array_slice(${array}, ${offset[, int length]})$0\n" "array_slice(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.splice" "array_splice(${&input}, ${offset[, int length]})$0\n" "array_splice(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.sum" "array_sum(${array})$0\n" "array_sum(...)" nil
			("array")
			nil nil nil nil)
		       ("array.unique" "array_unique(${array})$0\n" "array_unique(...)" nil
			("array")
			nil nil nil nil)
		       ("array.unshift" "array_unshift(${&array}, ${var[, mixed ...]})$0\n" "array_unshift(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("array.values" "array_values(${input})$0\n" "array_values(...)" nil
			("array")
			nil nil nil nil)
		       ("array.walk" "array_walk(${&array}, ${funcname[, mixed userdata]})$0\n" "array_walk(..., ...)" nil
			("array")
			nil nil nil nil)
		       ("asin" "asin(${arg})$0\n" "asin(...)" nil nil nil nil nil nil)
		       ("asinh" "asinh(${arg})$0\n" "asinh(...)" nil nil nil nil nil nil)
		       ("atan" "atan(${arg})$0\n" "atan(...)" nil nil nil nil nil nil)
		       ("atan2" "atan2(${y}, ${x})$0\n" "atan2(..., ...)" nil nil nil nil nil nil)
		       ("atanh" "atanh(${arg})$0\n" "atanh(...)" nil nil nil nil nil nil)
		       ("base.convert" "base_convert(${number}, ${frombase}, ${tobase})$0\n" "base_convert(..., ..., ...)" nil
			("base")
			nil nil nil nil)
		       ("base64.decode" "base64_decode(${encoded_data})$0\n" "base64_decode(...)" nil
			("base64")
			nil nil nil nil)
		       ("base64.encode" "base64_encode(${data})$0\n" "base64_encode(...)" nil
			("base64")
			nil nil nil nil)
		       ("basename" "basename(${path[, string suffix]})$0\n" "basename(...)" nil nil nil nil nil nil)
		       ("bin2hex" "bin2hex(${str})$0\n" "bin2hex(...)" nil nil nil nil nil nil)
		       ("bindec" "bindec(${binary_string})$0\n" "bindec(...)" nil nil nil nil nil nil)
		       ("cal.days.in.month" "cal_days_in_month(${calendar}, ${month}, ${year})$0\n" "cal_days_in_month(..., ..., ...)" nil
			("cal")
			nil nil nil nil)
		       ("cal.from.jd" "cal_from_jd(${jd}, ${calendar})$0\n" "cal_from_jd(..., ...)" nil
			("cal")
			nil nil nil nil)
		       ("cal.info" "cal_info(${[int calendar]})$0\n" "cal_info(...)" nil
			("cal")
			nil nil nil nil)
		       ("cal.to.jd" "cal_to_jd(${calendar}, ${month}, ${day}, ${year})$0\n" "cal_to_jd(..., ..., ..., ...)" nil
			("cal")
			nil nil nil nil)
		       ("call.user.func" "call_user_func(${function[, mixed parameter]})$0\n" "call_user_func(...)" nil
			("call")
			nil nil nil nil)
		       ("call.user.func.array" "call_user_func_array(${function}, ${param_arr})$0\n" "call_user_func_array(..., ...)" nil
			("call")
			nil nil nil nil)
		       ("case" "case ${condition}:\n  $0\n  break;\n\n" "case ...: ... break;" nil nil nil nil nil nil)
		       ("catch" "catch (${exception}) {\n  $0\n}\n" "catch ( ... ) { ... }" nil nil nil nil nil nil)
		       ("ceil" "ceil(${value})$0\n" "ceil(...)" nil nil nil nil nil nil)
		       ("chdir" "chdir(${directory})$0\n" "chdir(...)" nil nil nil nil nil nil)
		       ("checkdate" "checkdate(${month}, ${day}, ${year})$0\n" "checkdate(..., ..., ...)" nil nil nil nil nil nil)
		       ("checkdnsrr" "checkdnsrr(${host[, string type]})$0\n" "checkdnsrr(...)" nil nil nil nil nil nil)
		       ("chgrp" "chgrp(${filename}, ${group})$0\n" "chgrp(..., ...)" nil nil nil nil nil nil)
		       ("chmod" "chmod(${filename}, ${mode})$0\n" "chmod(..., ...)" nil nil nil nil nil nil)
		       ("chown" "chown(${filename}, ${user})$0\n" "chown(..., ...)" nil nil nil nil nil nil)
		       ("chr" "chr(${ascii})$0\n" "chr(...)" nil nil nil nil nil nil)
		       ("chroot" "chroot(${directory})$0\n" "chroot(...)" nil nil nil nil nil nil)
		       ("chunk.split" "chunk_split(${body[, int chunklen]})$0\n" "chunk_split(...)" nil
			("chunk")
			nil nil nil nil)
		       ("class" "class ${class_name} {\n   public function __construct() {\n      $0\n   }\n}\n" "class ... " nil nil nil nil nil nil)
		       ("clearstatcache" "clearstatcache()$0\n" "clearstatcache()" nil nil nil nil nil nil)
		       ("closedir" "closedir(${dir_handle})$0\n" "closedir(...)" nil nil nil nil nil nil)
		       ("closelog" "closelog()$0\n" "closelog()" nil nil nil nil nil nil)
		       ("connection.aborted" "connection_aborted()$0\n" "connection_aborted()" nil
			("connection")
			nil nil nil nil)
		       ("connection.status" "connection_status()$0\n" "connection_status()" nil
			("connection")
			nil nil nil nil)
		       ("connection.timeout" "connection_timeout()$0\n" "connection_timeout()" nil
			("connection")
			nil nil nil nil)
		       ("constant" "constant(${name})$0\n" "constant(...)" nil nil nil nil nil nil)
		       ("convert.cyr.string" "convert_cyr_string(${str}, ${from}, ${to})$0\n" "convert_cyr_string(..., ..., ...)" nil
			("convert")
			nil nil nil nil)
		       ("copy" "copy(${source}, ${dest})$0\n" "copy(..., ...)" nil nil nil nil nil nil)
		       ("cos" "cos(${arg})$0\n" "cos(...)" nil nil nil nil nil nil)
		       ("cosh" "cosh(${arg})$0\n" "cosh(...)" nil nil nil nil nil nil)
		       ("count.chars" "count_chars(${string[, int mode]})$0\n" "count_chars(...)" nil
			("count")
			nil nil nil nil)
		       ("crc32" "crc32(${str})$0\n" "crc32(...)" nil nil nil nil nil nil)
		       ("create.function" "create_function(${args}, ${code})$0\n" "create_function(..., ...)" nil
			("create")
			nil nil nil nil)
		       ("crypt" "crypt(${str[, string salt]})$0\n" "crypt(...)" nil nil nil nil nil nil)
		       ("ctype.alnum" "ctype_alnum(${text})$0\n" "ctype_alnum(...)" nil
			("ctype")
			nil nil nil nil)
		       ("ctype.alpha" "ctype_alpha(${text})$0\n" "ctype_alpha(...)" nil
			("ctype")
			nil nil nil nil)
		       ("ctype.cntrl" "ctype_cntrl(${text})$0\n" "ctype_cntrl(...)" nil
			("ctype")
			nil nil nil nil)
		       ("ctype.digit" "ctype_digit(${text})$0\n" "ctype_digit(...)" nil
			("ctype")
			nil nil nil nil)
		       ("ctype.graph" "ctype_graph(${text})$0\n" "ctype_graph(...)" nil
			("ctype")
			nil nil nil nil)
		       ("ctype.lower" "ctype_lower(${text})$0\n" "ctype_lower(...)" nil
			("ctype")
			nil nil nil nil)
		       ("ctype.print" "ctype_print(${text})$0\n" "ctype_print(...)" nil
			("ctype")
			nil nil nil nil)
		       ("ctype.punct" "ctype_punct(${text})$0\n" "ctype_punct(...)" nil
			("ctype")
			nil nil nil nil)
		       ("ctype.space" "ctype_space(${text})$0\n" "ctype_space(...)" nil
			("ctype")
			nil nil nil nil)
		       ("ctype.upper" "ctype_upper(${text})$0\n" "ctype_upper(...)" nil
			("ctype")
			nil nil nil nil)
		       ("ctype.xdigit" "ctype_xdigit(${text})$0\n" "ctype_xdigit(...)" nil
			("ctype")
			nil nil nil nil)
		       ("date" "date(${format[, int timestamp]})$0\n" "date(...)" nil nil nil nil nil nil)
		       ("date.default.timezone.get" "date_default_timezone_get()$0\n" "date_default_timezone_get()" nil
			("date")
			nil nil nil nil)
		       ("date.default.timezone.set" "date_default_timezone_set(${timezone_identifier})$0\n" "date_default_timezone_set(...)" nil
			("date")
			nil nil nil nil)
		       ("dba.close" "dba_close(${handle})$0\n" "dba_close(...)" nil
			("dba")
			nil nil nil nil)
		       ("dba.delete" "dba_delete(${key}, ${handle})$0\n" "dba_delete(..., ...)" nil
			("dba")
			nil nil nil nil)
		       ("dba.exists" "dba_exists(${key}, ${handle})$0\n" "dba_exists(..., ...)" nil
			("dba")
			nil nil nil nil)
		       ("dba.fetch" "dba_fetch(${key}, ${handle})$0\n" "dba_fetch(..., ...)" nil
			("dba")
			nil nil nil nil)
		       ("dba.firstkey" "dba_firstkey(${handle})$0\n" "dba_firstkey(...)" nil
			("dba")
			nil nil nil nil)
		       ("dba.handlers" "dba_handlers(${[bool full_info]})$0\n" "dba_handlers(...)" nil
			("dba")
			nil nil nil nil)
		       ("dba.insert" "dba_insert(${key}, ${value}, ${handle})$0\n" "dba_insert(..., ..., ...)" nil
			("dba")
			nil nil nil nil)
		       ("dba.list" "dba_list()$0\n" "dba_list()" nil
			("dba")
			nil nil nil nil)
		       ("dba.nextkey" "dba_nextkey(${handle})$0\n" "dba_nextkey(...)" nil
			("dba")
			nil nil nil nil)
		       ("dba.open" "dba_open(${path}, ${mode[, string handler]})$0\n" "dba_open(..., ...)" nil
			("dba")
			nil nil nil nil)
		       ("dba.optimize" "dba_optimize(${handle})$0\n" "dba_optimize(...)" nil
			("dba")
			nil nil nil nil)
		       ("dba.popen" "dba_popen(${path}, ${mode[, string handler]})$0\n" "dba_popen(..., ...)" nil
			("dba")
			nil nil nil nil)
		       ("dba.replace" "dba_replace(${key}, ${value}, ${handle})$0\n" "dba_replace(..., ..., ...)" nil
			("dba")
			nil nil nil nil)
		       ("dba.sync" "dba_sync(${handle})$0\n" "dba_sync(...)" nil
			("dba")
			nil nil nil nil)
		       ("debug.zval.dump" "debug_zval_dump(${variable})$0\n" "debug_zval_dump(...)" nil
			("debug")
			nil nil nil nil)
		       ("debugger.off" "debugger_off()$0\n" "debugger_off()" nil
			("debugger")
			nil nil nil nil)
		       ("debugger.on" "debugger_on(${address})$0\n" "debugger_on(...)" nil
			("debugger")
			nil nil nil nil)
		       ("decbin" "decbin(${number})$0\n" "decbin(...)" nil nil nil nil nil nil)
		       ("dechex" "dechex(${number})$0\n" "dechex(...)" nil nil nil nil nil nil)
		       ("decoct" "decoct(${number})$0\n" "decoct(...)" nil nil nil nil nil nil)
		       ("define" "define(${name}, ${value[, bool case_insensitive]})$0\n" "define(..., ...)" nil nil nil nil nil nil)
		       ("define.syslog.variables" "define_syslog_variables()$0\n" "define_syslog_variables()" nil
			("define")
			nil nil nil nil)
		       ("defined" "defined(${name})$0\n" "defined(...)" nil nil nil nil nil nil)
		       ("deg2rad" "deg2rad(${number})$0\n" "deg2rad(...)" nil nil nil nil nil nil)
		       ("dio.close" "dio_close(${fd})$0\n" "dio_close(...)" nil
			("dio")
			nil nil nil nil)
		       ("dio.fcntl" "dio_fcntl(${fd}, ${cmd[, mixed args]})$0\n" "dio_fcntl(..., ...)" nil
			("dio")
			nil nil nil nil)
		       ("dio.open" "dio_open(${filename}, ${flags[, int mode]})$0\n" "dio_open(..., ...)" nil
			("dio")
			nil nil nil nil)
		       ("dio.read" "dio_read(${fd[, int len]})$0\n" "dio_read(...)" nil
			("dio")
			nil nil nil nil)
		       ("dio.seek" "dio_seek(${fd}, ${pos[, int whence]})$0\n" "dio_seek(..., ...)" nil
			("dio")
			nil nil nil nil)
		       ("dio.stat" "dio_stat(${fd})$0\n" "dio_stat(...)" nil
			("dio")
			nil nil nil nil)
		       ("dio.tcsetattr" "dio_tcsetattr(${fd}, ${options})$0\n" "dio_tcsetattr(..., ...)" nil
			("dio")
			nil nil nil nil)
		       ("dio.truncate" "dio_truncate(${fd}, ${offset})$0\n" "dio_truncate(..., ...)" nil
			("dio")
			nil nil nil nil)
		       ("dio.write" "dio_write(${fd}, ${data[, int len]})$0\n" "dio_write(..., ...)" nil
			("dio")
			nil nil nil nil)
		       ("dirname" "dirname(${path})$0\n" "dirname(...)" nil nil nil nil nil nil)
		       ("disk.free.space" "disk_free_space(${directory})$0\n" "disk_free_space(...)" nil
			("disk")
			nil nil nil nil)
		       ("disk.total.space" "disk_total_space(${directory})$0\n" "disk_total_space(...)" nil
			("disk")
			nil nil nil nil)
		       ("easter.date" "easter_date(${[int year]})$0\n" "easter_date(...)" nil
			("easter")
			nil nil nil nil)
		       ("easter.days" "easter_days(${[int year]})$0\n" "easter_days(...)" nil
			("easter")
			nil nil nil nil)
		       ("echo" "echo(${arg1[, string ...]})$0\n" "echo(...)" nil nil nil nil nil nil)
		       ("empty" "empty(${var})$0\n" "empty(...)" nil nil nil nil nil nil)
		       ("ereg" "ereg(${pattern}, ${string[, array &regs]})$0\n" "ereg(..., ...)" nil nil nil nil nil nil)
		       ("ereg.replace" "ereg_replace(${pattern}, ${replacement}, ${string})$0\n" "ereg_replace(..., ..., ...)" nil
			("ereg")
			nil nil nil nil)
		       ("eregi" "eregi(${pattern}, ${string[, array &regs]})$0\n" "eregi(..., ...)" nil nil nil nil nil nil)
		       ("eregi.replace" "eregi_replace(${pattern}, ${replacement}, ${string})$0\n" "eregi_replace(..., ..., ...)" nil
			("eregi")
			nil nil nil nil)
		       ("escapeshellarg" "escapeshellarg(${arg})$0\n" "escapeshellarg(...)" nil nil nil nil nil nil)
		       ("escapeshellcmd" "escapeshellcmd(${command})$0\n" "escapeshellcmd(...)" nil nil nil nil nil nil)
		       ("eval" "eval(${code_str})$0\n" "eval(...)" nil nil nil nil nil nil)
		       ("exec" "exec(${command[, array &output]})$0\n" "exec(...)" nil nil nil nil nil nil)
		       ("exit" "exit(${[string status]})$0\n" "exit(...)" nil nil nil nil nil nil)
		       ("exp" "exp(${arg})$0\n" "exp(...)" nil nil nil nil nil nil)
		       ("explode" "explode(${separator}, ${string[, int limit]})$0\n" "explode(..., ...)" nil nil nil nil nil nil)
		       ("expm1" "expm1(${number})$0\n" "expm1(...)" nil nil nil nil nil nil)
		       ("ezmlm.hash" "ezmlm_hash(${addr})$0\n" "ezmlm_hash(...)" nil
			("ezmlm")
			nil nil nil nil)
		       ("fclose" "fclose(${handle})$0\n" "fclose(...)" nil nil nil nil nil nil)
		       ("feof" "feof(${handle})$0\n" "feof(...)" nil nil nil nil nil nil)
		       ("fflush" "fflush(${handle})$0\n" "fflush(...)" nil nil nil nil nil nil)
		       ("fgetc" "fgetc(${handle})$0\n" "fgetc(...)" nil nil nil nil nil nil)
		       ("fgetcsv" "fgetcsv(${handle[, int length]})$0\n" "fgetcsv(...)" nil nil nil nil nil nil)
		       ("fgets" "fgets(${handle[, int length]})$0\n" "fgets(...)" nil nil nil nil nil nil)
		       ("fgetss" "fgetss(${handle[, int length]})$0\n" "fgetss(...)" nil nil nil nil nil nil)
		       ("file" "file(${filename[, int use_include_path]})$0\n" "file(...)" nil nil nil nil nil nil)
		       ("file.exists" "file_exists(${filename})$0\n" "file_exists(...)" nil
			("file")
			nil nil nil nil)
		       ("file.get.contents" "file_get_contents(${filename[, bool use_include_path]})$0\n" "file_get_contents(...)" nil
			("file")
			nil nil nil nil)
		       ("floatval" "floatval(${var})$0\n" "floatval(...)" nil nil nil nil nil nil)
		       ("floor" "floor(${value})$0\n" "floor(...)" nil nil nil nil nil nil)
		       ("flush" "flush()$0\n" "flush()" nil nil nil nil nil nil)
		       ("fmod" "fmod(${x}, ${y})$0\n" "fmod(..., ...)" nil nil nil nil nil nil)
		       ("for" "for (${initial}; ${condition}; ${next}) {\n   $0\n}\n\n" "for (...) {...}" nil nil nil nil nil nil)
		       ("foreach" "foreach (${collection} as ${var}) {\n   $0\n}\n" "foreach (...) {...}" nil nil nil nil nil nil)
		       ("frenchtojd" "frenchtojd(${month}, ${day}, ${year})$0\n" "frenchtojd(..., ..., ...)" nil nil nil nil nil nil)
		       ("fsockopen" "fsockopen(${target[, int port]})$0\n" "fsockopen(...)" nil nil nil nil nil nil)
		       ("ftp.cdup" "ftp_cdup(${ftp_stream})$0\n" "ftp_cdup(...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.chdir" "ftp_chdir(${ftp_stream}, ${directory})$0\n" "ftp_chdir(..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.close" "ftp_close(${ftp_stream})$0\n" "ftp_close(...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.connect" "ftp_connect(${host[, int port]})$0\n" "ftp_connect(...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.delete" "ftp_delete(${ftp_stream}, ${path})$0\n" "ftp_delete(..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.exec" "ftp_exec(${ftp_stream}, ${command})$0\n" "ftp_exec(..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.fget" "ftp_fget(${ftp_stream}, ${handle}, ${remote_file}, ${mode[, int resumepos]})$0\n" "ftp_fget(..., ..., ..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.fput" "ftp_fput(${ftp_stream}, ${remote_file}, ${handle}, ${mode[, int startpos]})$0\n" "ftp_fput(..., ..., ..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.get" "ftp_get(${ftp_stream}, ${local_file}, ${remote_file}, ${mode[, int resumepos]})$0\n" "ftp_get(..., ..., ..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.get.option" "ftp_get_option(${ftp_stream}, ${option})$0\n" "ftp_get_option(..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.login" "ftp_login(${ftp_stream}, ${username}, ${password})$0\n" "ftp_login(..., ..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.mdtm" "ftp_mdtm(${ftp_stream}, ${remote_file})$0\n" "ftp_mdtm(..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.mkdir" "ftp_mkdir(${ftp_stream}, ${directory})$0\n" "ftp_mkdir(..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.nb.continue" "ftp_nb_continue(${ftp_stream})$0\n" "ftp_nb_continue(...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.nb.fget" "ftp_nb_fget(${ftp_stream}, ${handle}, ${remote_file}, ${mode[, int resumepos]})$0\n" "ftp_nb_fget(..., ..., ..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.nb.fput" "ftp_nb_fput(${ftp_stream}, ${remote_file}, ${handle}, ${mode[, int startpos]})$0\n" "ftp_nb_fput(..., ..., ..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.nb.get" "ftp_nb_get(${ftp_stream}, ${local_file}, ${remote_file}, ${mode[, int resumepos]})$0\n" "ftp_nb_get(..., ..., ..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.nb.put" "ftp_nb_put(${ftp_stream}, ${remote_file}, ${local_file}, ${mode[, int startpos]})$0\n" "ftp_nb_put(..., ..., ..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.nlist" "ftp_nlist(${ftp_stream}, ${directory})$0\n" "ftp_nlist(..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.pasv" "ftp_pasv(${ftp_stream}, ${pasv})$0\n" "ftp_pasv(..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.put" "ftp_put(${ftp_stream}, ${remote_file}, ${local_file}, ${mode[, int startpos]})$0\n" "ftp_put(..., ..., ..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.pwd" "ftp_pwd(${ftp_stream})$0\n" "ftp_pwd(...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.rawlist" "ftp_rawlist(${ftp_stream}, ${directory[, bool recursive]})$0\n" "ftp_rawlist(..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.rename" "ftp_rename(${ftp_stream}, ${oldname}, ${newname})$0\n" "ftp_rename(..., ..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.rmdir" "ftp_rmdir(${ftp_stream}, ${directory})$0\n" "ftp_rmdir(..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.set.option" "ftp_set_option(${ftp_stream}, ${option}, ${value})$0\n" "ftp_set_option(..., ..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.site" "ftp_site(${ftp_stream}, ${command})$0\n" "ftp_site(..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.size" "ftp_size(${ftp_stream}, ${remote_file})$0\n" "ftp_size(..., ...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.ssl.connect" "ftp_ssl_connect(${host[, int port]})$0\n" "ftp_ssl_connect(...)" nil
			("ftp")
			nil nil nil nil)
		       ("ftp.systype" "ftp_systype(${ftp_stream})$0\n" "ftp_systype(...)" nil
			("ftp")
			nil nil nil nil)
		       ("func.get.arg" "func_get_arg(${arg_num})$0\n" "func_get_arg(...)" nil
			("func")
			nil nil nil nil)
		       ("func.get.args" "func_get_args()$0\n" "func_get_args()" nil
			("func")
			nil nil nil nil)
		       ("func.num.args" "func_num_args()$0\n" "func_num_args()" nil
			("func")
			nil nil nil nil)
		       ("funct" "function ${function_name}(${function_args}) {\n   $0\n}\n" "funct ...(...)" nil nil nil nil nil nil)
		       ("function" "function ${function_name}(${function_args}) {\n   $0\n}\n" "function ...(...)" nil nil nil nil nil nil)
		       ("function.exists" "function_exists(${function_name})$0\n" "function_exists(...)" nil
			("function")
			nil nil nil nil)
		       ("gd.info" "gd_info()$0\n" "gd_info()" nil
			("gd")
			nil nil nil nil)
		       ("get.browser" "get_browser(${[string user_agent]})$0\n" "get_browser(...)" nil
			("get")
			nil nil nil nil)
		       ("get.defined.functions" "get_defined_functions()$0\n" "get_defined_functions()" nil
			("get")
			nil nil nil nil)
		       ("get.defined.vars" "get_defined_vars()$0\n" "get_defined_vars()" nil
			("get")
			nil nil nil nil)
		       ("get.html.translation.table" "get_html_translation_table(${[int table]})$0\n" "get_html_translation_table(...)" nil
			("get")
			nil nil nil nil)
		       ("get.meta.tags" "get_meta_tags(${filename[, bool use_include_path]})$0\n" "get_meta_tags(...)" nil
			("get")
			nil nil nil nil)
		       ("get.resource.type" "get_resource_type(${handle})$0\n" "get_resource_type(...)" nil
			("get")
			nil nil nil nil)
		       ("getcwd" "getcwd()$0\n" "getcwd()" nil nil nil nil nil nil)
		       ("getdate" "getdate(${[int timestamp]})$0\n" "getdate(...)" nil nil nil nil nil nil)
		       ("gethostbyaddr" "gethostbyaddr(${ip_address})$0\n" "gethostbyaddr(...)" nil nil nil nil nil nil)
		       ("gethostbyname" "gethostbyname(${hostname})$0\n" "gethostbyname(...)" nil nil nil nil nil nil)
		       ("gethostbynamel" "gethostbynamel(${hostname})$0\n" "gethostbynamel(...)" nil nil nil nil nil nil)
		       ("getimagesize" "getimagesize(${filename[, array &imageinfo]})$0\n" "getimagesize(...)" nil nil nil nil nil nil)
		       ("getmxrr" "getmxrr(${hostname}, ${&mxhosts[, array &weight]})$0\n" "getmxrr(..., ...)" nil nil nil nil nil nil)
		       ("getprotobyname" "getprotobyname(${name})$0\n" "getprotobyname(...)" nil nil nil nil nil nil)
		       ("getprotobynumber" "getprotobynumber(${number})$0\n" "getprotobynumber(...)" nil nil nil nil nil nil)
		       ("getrandmax" "getrandmax()$0\n" "getrandmax()" nil nil nil nil nil nil)
		       ("getservbyname" "getservbyname(${service}, ${protocol})$0\n" "getservbyname(..., ...)" nil nil nil nil nil nil)
		       ("getservbyport" "getservbyport(${port}, ${protocol})$0\n" "getservbyport(..., ...)" nil nil nil nil nil nil)
		       ("gettimeofday" "gettimeofday(${[bool return_float]})$0\n" "gettimeofday(...)" nil nil nil nil nil nil)
		       ("gettype" "gettype(${var})$0\n" "gettype(...)" nil nil nil nil nil nil)
		       ("gmdate" "gmdate(${format[, int timestamp]})$0\n" "gmdate(...)" nil nil nil nil nil nil)
		       ("gmmktime" "gmmktime(${[int hour]})$0\n" "gmmktime(...)" nil nil nil nil nil nil)
		       ("gmstrftime" "gmstrftime(${format[, int timestamp]})$0\n" "gmstrftime(...)" nil nil nil nil nil nil)
		       ("gregoriantojd" "gregoriantojd(${month}, ${day}, ${year})$0\n" "gregoriantojd(..., ..., ...)" nil nil nil nil nil nil)
		       ("header" "header(${string[, bool replace]})$0\n" "header(...)" nil nil nil nil nil nil)
		       ("headers.sent" "headers_sent(${[string &file]})$0\n" "headers_sent(...)" nil
			("headers")
			nil nil nil nil)
		       ("hebrev" "hebrev(${hebrew_text[, int max_chars_per_line]})$0\n" "hebrev(...)" nil nil nil nil nil nil)
		       ("hebrevc" "hebrevc(${hebrew_text[, int max_chars_per_line]})$0\n" "hebrevc(...)" nil nil nil nil nil nil)
		       ("hexdec" "hexdec(${hex_string})$0\n" "hexdec(...)" nil nil nil nil nil nil)
		       ("highlight.file" "highlight_file(${filename[, bool return]})$0\n" "highlight_file(...)" nil
			("highlight")
			nil nil nil nil)
		       ("highlight.string" "highlight_string(${str[, bool return]})$0\n" "highlight_string(...)" nil
			("highlight")
			nil nil nil nil)
		       ("html.entity.decode" "html_entity_decode(${string[, int quote_style]})$0\n" "html_entity_decode(...)" nil
			("html")
			nil nil nil nil)
		       ("htmlentities" "htmlentities(${string[, int quote_style]})$0\n" "htmlentities(...)" nil nil nil nil nil nil)
		       ("htmlspecialchars" "htmlspecialchars(${string[, int quote_style]})$0\n" "htmlspecialchars(...)" nil nil nil nil nil nil)
		       ("htmlspecialchars.decode" "htmlspecialchars_decode(${string[, int quote_style]})$0\n" "htmlspecialchars_decode(...)" nil
			("htmlspecialchars")
			nil nil nil nil)
		       ("hypot" "hypot(${x}, ${y})$0\n" "hypot(..., ...)" nil nil nil nil nil nil)
		       ("if" "if (${condition}) {\n   $0\n}\n\n" "if (...) {...}" nil nil nil nil nil nil)
		       ("ifelse" "if (${condition}) {\n   ${consequent}\n}\nelse {\n   $0\n}\n" "if (...) {...} else { ... }" nil nil nil nil nil nil)
		       ("ignore.user.abort" "ignore_user_abort(${[bool setting]})$0\n" "ignore_user_abort(...)" nil
			("ignore")
			nil nil nil nil)
		       ("image.type.to.mime.type" "image_type_to_mime_type(${imagetype})$0\n" "image_type_to_mime_type(...)" nil
			("image")
			nil nil nil nil)
		       ("image2wbmp" "image2wbmp(${image[, string filename]})$0\n" "image2wbmp(...)" nil nil nil nil nil nil)
		       ("imagealphablending" "imagealphablending(${image}, ${blendmode})$0\n" "imagealphablending(..., ...)" nil nil nil nil nil nil)
		       ("imageantialias" "imageantialias(${im}, ${on})$0\n" "imageantialias(..., ...)" nil nil nil nil nil nil)
		       ("imagearc" "imagearc(${image}, ${cx}, ${cy}, ${w}, ${h}, ${s}, ${e}, ${color})$0\n" "imagearc(..., ..., ..., ..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagechar" "imagechar(${image}, ${font}, ${x}, ${y}, ${c}, ${color})$0\n" "imagechar(..., ..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecharup" "imagecharup(${image}, ${font}, ${x}, ${y}, ${c}, ${color})$0\n" "imagecharup(..., ..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecolorallocate" "imagecolorallocate(${image}, ${red}, ${green}, ${blue})$0\n" "imagecolorallocate(..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecolorallocatealpha" "imagecolorallocatealpha(${image}, ${red}, ${green}, ${blue}, ${alpha})$0\n" "imagecolorallocatealpha(..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecolorat" "imagecolorat(${image}, ${x}, ${y})$0\n" "imagecolorat(..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecolorclosest" "imagecolorclosest(${image}, ${red}, ${green}, ${blue})$0\n" "imagecolorclosest(..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecolorclosestalpha" "imagecolorclosestalpha(${image}, ${red}, ${green}, ${blue}, ${alpha})$0\n" "imagecolorclosestalpha(..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecolorclosesthwb" "imagecolorclosesthwb(${image}, ${red}, ${green}, ${blue})$0\n" "imagecolorclosesthwb(..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecolordeallocate" "imagecolordeallocate(${image}, ${color})$0\n" "imagecolordeallocate(..., ...)" nil nil nil nil nil nil)
		       ("imagecolorexact" "imagecolorexact(${image}, ${red}, ${green}, ${blue})$0\n" "imagecolorexact(..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecolorexactalpha" "imagecolorexactalpha(${image}, ${red}, ${green}, ${blue}, ${alpha})$0\n" "imagecolorexactalpha(..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecolormatch" "imagecolormatch(${image1}, ${image2})$0\n" "imagecolormatch(..., ...)" nil nil nil nil nil nil)
		       ("imagecolorresolve" "imagecolorresolve(${image}, ${red}, ${green}, ${blue})$0\n" "imagecolorresolve(..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecolorresolvealpha" "imagecolorresolvealpha(${image}, ${red}, ${green}, ${blue}, ${alpha})$0\n" "imagecolorresolvealpha(..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecolorset" "imagecolorset(${image}, ${index}, ${red}, ${green}, ${blue})$0\n" "imagecolorset(..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecolorsforindex" "imagecolorsforindex(${image}, ${index})$0\n" "imagecolorsforindex(..., ...)" nil nil nil nil nil nil)
		       ("imagecolorstotal" "imagecolorstotal(${image})$0\n" "imagecolorstotal(...)" nil nil nil nil nil nil)
		       ("imagecolortransparent" "imagecolortransparent(${image[, int color]})$0\n" "imagecolortransparent(...)" nil nil nil nil nil nil)
		       ("imageconvolution" "imageconvolution(${image}, ${matrix3x3}, ${div}, ${offset})$0\n" "imageconvolution(..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecopy" "imagecopy(${dst_im}, ${src_im}, ${dst_x}, ${dst_y}, ${src_x}, ${src_y}, ${src_w}, ${src_h})$0\n" "imagecopy(..., ..., ..., ..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecopymerge" "imagecopymerge(${dst_im}, ${src_im}, ${dst_x}, ${dst_y}, ${src_x}, ${src_y}, ${src_w}, ${src_h}, ${pct})$0\n" "imagecopymerge(..., ..., ..., ..., ..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecopymergegray" "imagecopymergegray(${dst_im}, ${src_im}, ${dst_x}, ${dst_y}, ${src_x}, ${src_y}, ${src_w}, ${src_h}, ${pct})$0\n" "imagecopymergegray(..., ..., ..., ..., ..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecopyresampled" "imagecopyresampled(${dst_image}, ${src_image}, ${dst_x}, ${dst_y}, ${src_x}, ${src_y}, ${dst_w}, ${dst_h}, ${src_w}, ${src_h})$0\n" "imagecopyresampled(..., ..., ..., ..., ..., ..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecopyresized" "imagecopyresized(${dst_image}, ${src_image}, ${dst_x}, ${dst_y}, ${src_x}, ${src_y}, ${dst_w}, ${dst_h}, ${src_w}, ${src_h})$0\n" "imagecopyresized(..., ..., ..., ..., ..., ..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecreate" "imagecreate(${x_size}, ${y_size})$0\n" "imagecreate(..., ...)" nil nil nil nil nil nil)
		       ("imagecreatefromgd" "imagecreatefromgd(${filename})$0\n" "imagecreatefromgd(...)" nil nil nil nil nil nil)
		       ("imagecreatefromgd2" "imagecreatefromgd2(${filename})$0\n" "imagecreatefromgd2(...)" nil nil nil nil nil nil)
		       ("imagecreatefromgd2part" "imagecreatefromgd2part(${filename}, ${srcX}, ${srcY}, ${width}, ${height})$0\n" "imagecreatefromgd2part(..., ..., ..., ..., ...)" nil nil nil nil nil nil)
		       ("imagecreatefromgif" "imagecreatefromgif(${filename})$0\n" "imagecreatefromgif(...)" nil nil nil nil nil nil)
		       ("imagecreatefromjpeg" "imagecreatefromjpeg(${filename})$0\n" "imagecreatefromjpeg(...)" nil nil nil nil nil nil)
		       ("imap.8bit" "imap_8bit(${string})$0\n" "imap_8bit(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.alerts" "imap_alerts()$0\n" "imap_alerts()" nil
			("imap")
			nil nil nil nil)
		       ("imap.append" "imap_append(${imap_stream}, ${mbox}, ${message[, string options]})$0\n" "imap_append(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.base64" "imap_base64(${text})$0\n" "imap_base64(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.binary" "imap_binary(${string})$0\n" "imap_binary(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.body" "imap_body(${imap_stream}, ${msg_number[, int options]})$0\n" "imap_body(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.bodystruct" "imap_bodystruct(${stream_id}, ${msg_no}, ${section})$0\n" "imap_bodystruct(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.check" "imap_check(${imap_stream})$0\n" "imap_check(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.clearflag.full" "imap_clearflag_full(${stream}, ${sequence}, ${flag[, string options]})$0\n" "imap_clearflag_full(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.close" "imap_close(${imap_stream[, int flag]})$0\n" "imap_close(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.createmailbox" "imap_createmailbox(${imap_stream}, ${mbox})$0\n" "imap_createmailbox(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.delete" "imap_delete(${imap_stream}, ${msg_number[, int options]})$0\n" "imap_delete(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.deletemailbox" "imap_deletemailbox(${imap_stream}, ${mbox})$0\n" "imap_deletemailbox(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.errors" "imap_errors()$0\n" "imap_errors()" nil
			("imap")
			nil nil nil nil)
		       ("imap.expunge" "imap_expunge(${imap_stream})$0\n" "imap_expunge(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.fetch.overview" "imap_fetch_overview(${imap_stream}, ${sequence[, int options]})$0\n" "imap_fetch_overview(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.fetchbody" "imap_fetchbody(${imap_stream}, ${msg_number}, ${part_number[, int options]})$0\n" "imap_fetchbody(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.fetchheader" "imap_fetchheader(${imap_stream}, ${msgno[, int options]})$0\n" "imap_fetchheader(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.fetchstructure" "imap_fetchstructure(${imap_stream}, ${msg_number[, int options]})$0\n" "imap_fetchstructure(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.get.quota" "imap_get_quota(${imap_stream}, ${quota_root})$0\n" "imap_get_quota(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.get.quotaroot" "imap_get_quotaroot(${imap_stream}, ${quota_root})$0\n" "imap_get_quotaroot(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.getmailboxes" "imap_getmailboxes(${imap_stream}, ${ref}, ${pattern})$0\n" "imap_getmailboxes(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.getsubscribed" "imap_getsubscribed(${imap_stream}, ${ref}, ${pattern})$0\n" "imap_getsubscribed(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.headerinfo" "imap_headerinfo(${imap_stream}, ${msg_number[, int fromlength]})$0\n" "imap_headerinfo(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.headers" "imap_headers(${imap_stream})$0\n" "imap_headers(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.last.error" "imap_last_error()$0\n" "imap_last_error()" nil
			("imap")
			nil nil nil nil)
		       ("imap.list" "imap_list(${imap_stream}, ${ref}, ${pattern})$0\n" "imap_list(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.lsub" "imap_lsub(${imap_stream}, ${ref}, ${pattern})$0\n" "imap_lsub(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.mail" "imap_mail(${to}, ${subject}, ${message[, string additional_headers]})$0\n" "imap_mail(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.mail.compose" "imap_mail_compose(${envelope}, ${body})$0\n" "imap_mail_compose(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.mail.copy" "imap_mail_copy(${imap_stream}, ${msglist}, ${mbox[, int options]})$0\n" "imap_mail_copy(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.mail.move" "imap_mail_move(${imap_stream}, ${msglist}, ${mbox[, int options]})$0\n" "imap_mail_move(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.mailboxmsginfo" "imap_mailboxmsginfo(${imap_stream})$0\n" "imap_mailboxmsginfo(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.mime.header.decode" "imap_mime_header_decode(${text})$0\n" "imap_mime_header_decode(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.msgno" "imap_msgno(${imap_stream}, ${uid})$0\n" "imap_msgno(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.num.msg" "imap_num_msg(${imap_stream})$0\n" "imap_num_msg(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.num.recent" "imap_num_recent(${imap_stream})$0\n" "imap_num_recent(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.open" "imap_open(${mailbox}, ${username}, ${password[, int options]})$0\n" "imap_open(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.ping" "imap_ping(${imap_stream})$0\n" "imap_ping(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.qprint" "imap_qprint(${string})$0\n" "imap_qprint(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.renamemailbox" "imap_renamemailbox(${imap_stream}, ${old_mbox}, ${new_mbox})$0\n" "imap_renamemailbox(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.reopen" "imap_reopen(${imap_stream}, ${mailbox[, int options]})$0\n" "imap_reopen(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.rfc822.parse.adrlist" "imap_rfc822_parse_adrlist(${address}, ${default_host})$0\n" "imap_rfc822_parse_adrlist(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.rfc822.parse.headers" "imap_rfc822_parse_headers(${headers[, string defaulthost]})$0\n" "imap_rfc822_parse_headers(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.rfc822.write.address" "imap_rfc822_write_address(${mailbox}, ${host}, ${personal})$0\n" "imap_rfc822_write_address(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.search" "imap_search(${imap_stream}, ${criteria[, int options]})$0\n" "imap_search(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.set.quota" "imap_set_quota(${imap_stream}, ${quota_root}, ${quota_limit})$0\n" "imap_set_quota(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.setacl" "imap_setacl(${stream_id}, ${mailbox}, ${id}, ${rights})$0\n" "imap_setacl(..., ..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.setflag.full" "imap_setflag_full(${stream}, ${sequence}, ${flag[, string options]})$0\n" "imap_setflag_full(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.sort" "imap_sort(${stream}, ${criteria}, ${reverse[, int options]})$0\n" "imap_sort(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.status" "imap_status(${imap_stream}, ${mailbox}, ${options})$0\n" "imap_status(..., ..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.subscribe" "imap_subscribe(${imap_stream}, ${mbox})$0\n" "imap_subscribe(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.thread" "imap_thread(${stream_id[, int options]})$0\n" "imap_thread(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.timeout" "imap_timeout(${timeout_type[, int timeout]})$0\n" "imap_timeout(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.uid" "imap_uid(${imap_stream}, ${msgno})$0\n" "imap_uid(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.undelete" "imap_undelete(${imap_stream}, ${msg_number[, int flags]})$0\n" "imap_undelete(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.unsubscribe" "imap_unsubscribe(${imap_stream}, ${mbox})$0\n" "imap_unsubscribe(..., ...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.utf7.decode" "imap_utf7_decode(${text})$0\n" "imap_utf7_decode(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.utf7.encode" "imap_utf7_encode(${data})$0\n" "imap_utf7_encode(...)" nil
			("imap")
			nil nil nil nil)
		       ("imap.utf8" "imap_utf8(${mime_encoded_text})$0\n" "imap_utf8(...)" nil
			("imap")
			nil nil nil nil)
		       ("implode" "implode(${glue}, ${pieces})$0\n" "implode(..., ...)" nil nil nil nil nil nil)
		       ("import.request.variables" "import_request_variables(${types[, string prefix]})$0\n" "import_request_variables(...)" nil
			("import")
			nil nil nil nil)
		       ("inet.ntop" "inet_ntop(${in_addr})$0\n" "inet_ntop(...)" nil
			("inet")
			nil nil nil nil)
		       ("inet.pton" "inet_pton(${address})$0\n" "inet_pton(...)" nil
			("inet")
			nil nil nil nil)
		       ("intval" "intval(${var[, int base]})$0\n" "intval(...)" nil nil nil nil nil nil)
		       ("ip2long" "ip2long(${ip_address})$0\n" "ip2long(...)" nil nil nil nil nil nil)
		       ("is.array" "is_array(${var})$0\n" "is_array(...)" nil
			("is")
			nil nil nil nil)
		       ("is.bool" "is_bool(${var})$0\n" "is_bool(...)" nil
			("is")
			nil nil nil nil)
		       ("is.callable" "is_callable(${var[, bool syntax_only]})$0\n" "is_callable(...)" nil
			("is")
			nil nil nil nil)
		       ("is.finite" "is_finite(${val})$0\n" "is_finite(...)" nil
			("is")
			nil nil nil nil)
		       ("is.float" "is_float(${var})$0\n" "is_float(...)" nil
			("is")
			nil nil nil nil)
		       ("is.infinite" "is_infinite(${val})$0\n" "is_infinite(...)" nil
			("is")
			nil nil nil nil)
		       ("is.int" "is_int(${var})$0\n" "is_int(...)" nil
			("is")
			nil nil nil nil)
		       ("is.nan" "is_nan(${val})$0\n" "is_nan(...)" nil
			("is")
			nil nil nil nil)
		       ("is.null" "is_null(${var})$0\n" "is_null(...)" nil
			("is")
			nil nil nil nil)
		       ("is.numeric" "is_numeric(${var})$0\n" "is_numeric(...)" nil
			("is")
			nil nil nil nil)
		       ("is.object" "is_object(${var})$0\n" "is_object(...)" nil
			("is")
			nil nil nil nil)
		       ("is.resource" "is_resource(${var})$0\n" "is_resource(...)" nil
			("is")
			nil nil nil nil)
		       ("is.scalar" "is_scalar(${var})$0\n" "is_scalar(...)" nil
			("is")
			nil nil nil nil)
		       ("is.string" "is_string(${var})$0\n" "is_string(...)" nil
			("is")
			nil nil nil nil)
		       ("isset" "isset(${var[, mixed var]})$0\n" "isset(...)" nil nil nil nil nil nil)
		       ("jddayofweek" "jddayofweek(${julianday[, int mode]})$0\n" "jddayofweek(...)" nil nil nil nil nil nil)
		       ("jdmonthname" "jdmonthname(${julianday}, ${mode})$0\n" "jdmonthname(..., ...)" nil nil nil nil nil nil)
		       ("jdtofrench" "jdtofrench(${juliandaycount})$0\n" "jdtofrench(...)" nil nil nil nil nil nil)
		       ("jdtogregorian" "jdtogregorian(${julianday})$0\n" "jdtogregorian(...)" nil nil nil nil nil nil)
		       ("jdtojewish" "jdtojewish(${juliandaycount[, bool hebrew]})$0\n" "jdtojewish(...)" nil nil nil nil nil nil)
		       ("jdtojulian" "jdtojulian(${julianday})$0\n" "jdtojulian(...)" nil nil nil nil nil nil)
		       ("jdtounix" "jdtounix(${jday})$0\n" "jdtounix(...)" nil nil nil nil nil nil)
		       ("jewishtojd" "jewishtojd(${month}, ${day}, ${year})$0\n" "jewishtojd(..., ..., ...)" nil nil nil nil nil nil)
		       ("juliantojd" "juliantojd(${month}, ${day}, ${year})$0\n" "juliantojd(..., ..., ...)" nil nil nil nil nil nil)
		       ("lcg.value" "lcg_value()$0\n" "lcg_value()" nil
			("lcg")
			nil nil nil nil)
		       ("levenshtein" "levenshtein(${str1}, ${str2[, int cost_ins, int cost_rep, int cost_del]})$0\n" "levenshtein(..., ...)" nil nil nil nil nil nil)
		       ("localeconv" "localeconv()$0\n" "localeconv()" nil nil nil nil nil nil)
		       ("localtime" "localtime(${[int timestamp]})$0\n" "localtime(...)" nil nil nil nil nil nil)
		       ("log" "log(${arg[, float base]})$0\n" "log(...)" nil nil nil nil nil nil)
		       ("log10" "log10(${arg})$0\n" "log10(...)" nil nil nil nil nil nil)
		       ("log1p" "log1p(${number})$0\n" "log1p(...)" nil nil nil nil nil nil)
		       ("long2ip" "long2ip(${proper_address})$0\n" "long2ip(...)" nil nil nil nil nil nil)
		       ("ltrim" "ltrim(${str[, string charlist]})$0\n" "ltrim(...)" nil nil nil nil nil nil)
		       ("mail" "mail(${to}, ${subject}, ${message[, string additional_headers]})$0\n" "mail(..., ..., ...)" nil nil nil nil nil nil)
		       ("max" "max(${arg1}, ${arg2[, number ...]})$0\n" "max(..., ...)" nil nil nil nil nil nil)
		       ("md5" "md5(${str[, bool raw_output]})$0\n" "md5(...)" nil nil nil nil nil nil)
		       ("md5.file" "md5_file(${filename[, bool raw_output]})$0\n" "md5_file(...)" nil
			("md5")
			nil nil nil nil)
		       ("metaphone" "metaphone(${str[, int phones]})$0\n" "metaphone(...)" nil nil nil nil nil nil)
		       ("mhash" "mhash(${hash}, ${data[, string key]})$0\n" "mhash(..., ...)" nil nil nil nil nil nil)
		       ("mhash.count" "mhash_count()$0\n" "mhash_count()" nil
			("mhash")
			nil nil nil nil)
		       ("mhash.get.block.size" "mhash_get_block_size(${hash})$0\n" "mhash_get_block_size(...)" nil
			("mhash")
			nil nil nil nil)
		       ("mhash.get.hash.name" "mhash_get_hash_name(${hash})$0\n" "mhash_get_hash_name(...)" nil
			("mhash")
			nil nil nil nil)
		       ("mhash.keygen.s2k" "mhash_keygen_s2k(${hash}, ${password}, ${salt}, ${bytes})$0\n" "mhash_keygen_s2k(..., ..., ..., ...)" nil
			("mhash")
			nil nil nil nil)
		       ("microtime" "microtime(${[bool get_as_float]})$0\n" "microtime(...)" nil nil nil nil nil nil)
		       ("min" "min(${arg1}, ${arg2[, number ...]})$0\n" "min(..., ...)" nil nil nil nil nil nil)
		       ("mktime" "mktime(${[int hour]})$0\n" "mktime(...)" nil nil nil nil nil nil)
		       ("money.format" "money_format(${format}, ${number})$0\n" "money_format(..., ...)" nil
			("money")
			nil nil nil nil)
		       ("mt.getrandmax" "mt_getrandmax()$0\n" "mt_getrandmax()" nil
			("mt")
			nil nil nil nil)
		       ("mt.rand" "mt_rand(${[int min, int max]})$0\n" "mt_rand(...)" nil
			("mt")
			nil nil nil nil)
		       ("mt.srand" "mt_srand(${[int seed]})$0\n" "mt_srand(...)" nil
			("mt")
			nil nil nil nil)
		       ("mysql.affected.rows" "mysql_affected_rows(${[resource link_identifier]})$0\n" "mysql_affected_rows(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.change.user" "mysql_change_user(${user}, ${password[, string database]})$0\n" "mysql_change_user(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.client.encoding" "mysql_client_encoding(${[resource link_identifier]})$0\n" "mysql_client_encoding(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.close" "mysql_close(${[resource link_identifier]})$0\n" "mysql_close(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.connect" "mysql_connect(${[string server]})$0\n" "mysql_connect(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.create.db" "mysql_create_db(${database_name[, resource link_identifier]})$0\n" "mysql_create_db(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.data.seek" "mysql_data_seek(${result}, ${row_number})$0\n" "mysql_data_seek(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.db.name" "mysql_db_name(${result}, ${row[, mixed field]})$0\n" "mysql_db_name(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.db.query" "mysql_db_query(${database}, ${query[, resource link_identifier]})$0\n" "mysql_db_query(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.drop.db" "mysql_drop_db(${database_name[, resource link_identifier]})$0\n" "mysql_drop_db(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.errno" "mysql_errno(${[resource link_identifier]})$0\n" "mysql_errno(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.error" "mysql_error(${[resource link_identifier]})$0\n" "mysql_error(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.escape.string" "mysql_escape_string(${unescaped_string})$0\n" "mysql_escape_string(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.fetch.array" "mysql_fetch_array(${result[, int result_type]})$0\n" "mysql_fetch_array(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.fetch.assoc" "mysql_fetch_assoc(${result})$0\n" "mysql_fetch_assoc(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.fetch.field" "mysql_fetch_field(${result[, int field_offset]})$0\n" "mysql_fetch_field(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.fetch.lengths" "mysql_fetch_lengths(${result})$0\n" "mysql_fetch_lengths(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.fetch.object" "mysql_fetch_object(${result})$0\n" "mysql_fetch_object(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.fetch.row" "mysql_fetch_row(${result})$0\n" "mysql_fetch_row(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.field.flags" "mysql_field_flags(${result}, ${field_offset})$0\n" "mysql_field_flags(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.field.len" "mysql_field_len(${result}, ${field_offset})$0\n" "mysql_field_len(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.field.name" "mysql_field_name(${result}, ${field_offset})$0\n" "mysql_field_name(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.field.seek" "mysql_field_seek(${result}, ${field_offset})$0\n" "mysql_field_seek(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.field.table" "mysql_field_table(${result}, ${field_offset})$0\n" "mysql_field_table(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.field.type" "mysql_field_type(${result}, ${field_offset})$0\n" "mysql_field_type(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.free.result" "mysql_free_result(${result})$0\n" "mysql_free_result(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.get.client.info" "mysql_get_client_info()$0\n" "mysql_get_client_info()" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.get.host.info" "mysql_get_host_info(${[resource link_identifier]})$0\n" "mysql_get_host_info(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.get.proto.info" "mysql_get_proto_info(${[resource link_identifier]})$0\n" "mysql_get_proto_info(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.get.server.info" "mysql_get_server_info(${[resource link_identifier]})$0\n" "mysql_get_server_info(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.info" "mysql_info(${[resource link_identifier]})$0\n" "mysql_info(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.insert.id" "mysql_insert_id(${[resource link_identifier]})$0\n" "mysql_insert_id(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.list.dbs" "mysql_list_dbs(${[resource link_identifier]})$0\n" "mysql_list_dbs(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.list.fields" "mysql_list_fields(${database_name}, ${table_name[, resource link_identifier]})$0\n" "mysql_list_fields(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.list.processes" "mysql_list_processes(${[resource link_identifier]})$0\n" "mysql_list_processes(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.list.tables" "mysql_list_tables(${database[, resource link_identifier]})$0\n" "mysql_list_tables(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.num.fields" "mysql_num_fields(${result})$0\n" "mysql_num_fields(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.num.rows" "mysql_num_rows(${result})$0\n" "mysql_num_rows(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.pconnect" "mysql_pconnect(${[string server]})$0\n" "mysql_pconnect(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.ping" "mysql_ping(${[resource link_identifier]})$0\n" "mysql_ping(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.query" "mysql_query(${query[, resource link_identifier]})$0\n" "mysql_query(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.real.escape.string" "mysql_real_escape_string(${unescaped_string[, resource link_identifier]})$0\n" "mysql_real_escape_string(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.result" "mysql_result(${result}, ${row[, mixed field]})$0\n" "mysql_result(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.select.db" "mysql_select_db(${database_name[, resource link_identifier]})$0\n" "mysql_select_db(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.stat" "mysql_stat(${[resource link_identifier]})$0\n" "mysql_stat(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.tablename" "mysql_tablename(${result}, ${i})$0\n" "mysql_tablename(..., ...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.thread.id" "mysql_thread_id(${[resource link_identifier]})$0\n" "mysql_thread_id(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysql.unbuffered.query" "mysql_unbuffered_query(${query[, resource link_identifier]})$0\n" "mysql_unbuffered_query(...)" nil
			("mysql")
			nil nil nil nil)
		       ("mysqli.embedded.connect" "mysqli_embedded_connect(${[string dbname]})$0\n" "mysqli_embedded_connect(...)" nil
			("mysqli")
			nil nil nil nil)
		       ("mysqli.server.end" "mysqli_server_end()$0\n" "mysqli_server_end()" nil
			("mysqli")
			nil nil nil nil)
		       ("mysqli.server.init" "mysqli_server_init(${[array server]})$0\n" "mysqli_server_init(...)" nil
			("mysqli")
			nil nil nil nil)
		       ("nl.langinfo" "nl_langinfo(${item})$0\n" "nl_langinfo(...)" nil
			("nl")
			nil nil nil nil)
		       ("nl2br" "nl2br(${string})$0\n" "nl2br(...)" nil nil nil nil nil nil)
		       ("number.format" "number_format(${number[, int decimals]})$0\n" "number_format(...)" nil
			("number")
			nil nil nil nil)
		       ("ob.clean" "ob_clean()$0\n" "ob_clean()" nil
			("ob")
			nil nil nil nil)
		       ("ob.end.clean" "ob_end_clean()$0\n" "ob_end_clean()" nil
			("ob")
			nil nil nil nil)
		       ("ob.end.flush" "ob_end_flush()$0\n" "ob_end_flush()" nil
			("ob")
			nil nil nil nil)
		       ("ob.flush" "ob_flush()$0\n" "ob_flush()" nil
			("ob")
			nil nil nil nil)
		       ("ob.get.clean" "ob_get_clean()$0\n" "ob_get_clean()" nil
			("ob")
			nil nil nil nil)
		       ("ob.get.contents" "ob_get_contents()$0\n" "ob_get_contents()" nil
			("ob")
			nil nil nil nil)
		       ("ob.get.flush" "ob_get_flush()$0\n" "ob_get_flush()" nil
			("ob")
			nil nil nil nil)
		       ("ob.get.length" "ob_get_length()$0\n" "ob_get_length()" nil
			("ob")
			nil nil nil nil)
		       ("ob.get.level" "ob_get_level()$0\n" "ob_get_level()" nil
			("ob")
			nil nil nil nil)
		       ("ob.get.status" "ob_get_status(${[bool full_status=FALSE]})$0\n" "ob_get_status(...)" nil
			("ob")
			nil nil nil nil)
		       ("ob.gzhandler" "ob_gzhandler(${buffer}, ${mode})$0\n" "ob_gzhandler(..., ...)" nil
			("ob")
			nil nil nil nil)
		       ("ob.implicit.flush" "ob_implicit_flush(${[int flag]})$0\n" "ob_implicit_flush(...)" nil
			("ob")
			nil nil nil nil)
		       ("ob.list.handlers" "ob_list_handlers()$0\n" "ob_list_handlers()" nil
			("ob")
			nil nil nil nil)
		       ("ob.start" "ob_start(${[callback output_callback]})$0\n" "ob_start(...)" nil
			("ob")
			nil nil nil nil)
		       ("octdec" "octdec(${octal_string})$0\n" "octdec(...)" nil nil nil nil nil nil)
		       ("odbc.autocommit" "odbc_autocommit(${connection_id[, bool OnOff]})$0\n" "odbc_autocommit(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.binmode" "odbc_binmode(${result_id}, ${mode})$0\n" "odbc_binmode(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.close" "odbc_close(${connection_id})$0\n" "odbc_close(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.close.all" "odbc_close_all()$0\n" "odbc_close_all()" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.columnprivileges" "odbc_columnprivileges(${connection_id}, ${qualifier}, ${owner}, ${table_name}, ${column_name})$0\n" "odbc_columnprivileges(..., ..., ..., ..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.columns" "odbc_columns(${connection_id[, string qualifier]})$0\n" "odbc_columns(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.commit" "odbc_commit(${connection_id})$0\n" "odbc_commit(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.connect" "odbc_connect(${dsn}, ${user}, ${password[, int cursor_type]})$0\n" "odbc_connect(..., ..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.cursor" "odbc_cursor(${result_id})$0\n" "odbc_cursor(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.data.source" "odbc_data_source(${connection_id}, ${fetch_type})$0\n" "odbc_data_source(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.do" "odbc_do(${conn_id}, ${query})$0\n" "odbc_do(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.error" "odbc_error(${[resource connection_id]})$0\n" "odbc_error(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.errormsg" "odbc_errormsg(${[resource connection_id]})$0\n" "odbc_errormsg(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.exec" "odbc_exec(${connection_id}, ${query_string[, int flags]})$0\n" "odbc_exec(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.execute" "odbc_execute(${result_id[, array parameters_array]})$0\n" "odbc_execute(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.fetch.array" "odbc_fetch_array(${result[, int rownumber]})$0\n" "odbc_fetch_array(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.fetch.into" "odbc_fetch_into(${result_id}, ${&result_array[, int rownumber]})$0\n" "odbc_fetch_into(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.fetch.object" "odbc_fetch_object(${result[, int rownumber]})$0\n" "odbc_fetch_object(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.fetch.row" "odbc_fetch_row(${result_id[, int row_number]})$0\n" "odbc_fetch_row(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.field.len" "odbc_field_len(${result_id}, ${field_number})$0\n" "odbc_field_len(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.field.name" "odbc_field_name(${result_id}, ${field_number})$0\n" "odbc_field_name(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.field.num" "odbc_field_num(${result_id}, ${field_name})$0\n" "odbc_field_num(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.field.precision" "odbc_field_precision(${result_id}, ${field_number})$0\n" "odbc_field_precision(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.field.scale" "odbc_field_scale(${result_id}, ${field_number})$0\n" "odbc_field_scale(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.field.type" "odbc_field_type(${result_id}, ${field_number})$0\n" "odbc_field_type(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.foreignkeys" "odbc_foreignkeys(${connection_id}, ${pk_qualifier}, ${pk_owner}, ${pk_table}, ${fk_qualifier}, ${fk_owner}, ${fk_table})$0\n" "odbc_foreignkeys(..., ..., ..., ..., ..., ..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.free.result" "odbc_free_result(${result_id})$0\n" "odbc_free_result(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.gettypeinfo" "odbc_gettypeinfo(${connection_id[, int data_type]})$0\n" "odbc_gettypeinfo(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.longreadlen" "odbc_longreadlen(${result_id}, ${length})$0\n" "odbc_longreadlen(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.next.result" "odbc_next_result(${result_id})$0\n" "odbc_next_result(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.num.fields" "odbc_num_fields(${result_id})$0\n" "odbc_num_fields(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.num.rows" "odbc_num_rows(${result_id})$0\n" "odbc_num_rows(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.pconnect" "odbc_pconnect(${dsn}, ${user}, ${password[, int cursor_type]})$0\n" "odbc_pconnect(..., ..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.prepare" "odbc_prepare(${connection_id}, ${query_string})$0\n" "odbc_prepare(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.primarykeys" "odbc_primarykeys(${connection_id}, ${qualifier}, ${owner}, ${table})$0\n" "odbc_primarykeys(..., ..., ..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.procedurecolumns" "odbc_procedurecolumns(${connection_id[, string qualifier, string owner, string proc, string column]})$0\n" "odbc_procedurecolumns(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.procedures" "odbc_procedures(${connection_id[, string qualifier, string owner, string name]})$0\n" "odbc_procedures(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.result" "odbc_result(${result_id}, ${field})$0\n" "odbc_result(..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.result.all" "odbc_result_all(${result_id[, string format]})$0\n" "odbc_result_all(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.rollback" "odbc_rollback(${connection_id})$0\n" "odbc_rollback(...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.setoption" "odbc_setoption(${id}, ${function}, ${option}, ${param})$0\n" "odbc_setoption(..., ..., ..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.specialcolumns" "odbc_specialcolumns(${connection_id}, ${type}, ${qualifier}, ${owner}, ${table}, ${scope}, ${nullable})$0\n" "odbc_specialcolumns(..., ..., ..., ..., ..., ..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.statistics" "odbc_statistics(${connection_id}, ${qualifier}, ${owner}, ${table_name}, ${unique}, ${accuracy})$0\n" "odbc_statistics(..., ..., ..., ..., ..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.tableprivileges" "odbc_tableprivileges(${connection_id}, ${qualifier}, ${owner}, ${name})$0\n" "odbc_tableprivileges(..., ..., ..., ...)" nil
			("odbc")
			nil nil nil nil)
		       ("odbc.tables" "odbc_tables(${connection_id[, string qualifier]})$0\n" "odbc_tables(...)" nil
			("odbc")
			nil nil nil nil)
		       ("opendir" "opendir(${path[, resource context]})$0\n" "opendir(...)" nil nil nil nil nil nil)
		       ("openlog" "openlog(${ident}, ${option}, ${facility})$0\n" "openlog(..., ..., ...)" nil nil nil nil nil nil)
		       ("ord" "ord(${string})$0\n" "ord(...)" nil nil nil nil nil nil)
		       ("output.add.rewrite.var" "output_add_rewrite_var(${name}, ${value})$0\n" "output_add_rewrite_var(..., ...)" nil
			("output")
			nil nil nil nil)
		       ("output.reset.rewrite.vars" "output_reset_rewrite_vars()$0\n" "output_reset_rewrite_vars()" nil
			("output")
			nil nil nil nil)
		       ("pack" "pack(${format[, mixed args]})$0\n" "pack(...)" nil nil nil nil nil nil)
		       ("parse.str" "parse_str(${str[, array &arr]})$0\n" "parse_str(...)" nil
			("parse")
			nil nil nil nil)
		       ("parse.url" "parse_url(${url})$0\n" "parse_url(...)" nil
			("parse")
			nil nil nil nil)
		       ("passthru" "passthru(${command[, int &return_var]})$0\n" "passthru(...)" nil nil nil nil nil nil)
		       ("pcntl.alarm" "pcntl_alarm(${seconds})$0\n" "pcntl_alarm(...)" nil
			("pcntl")
			nil nil nil nil)
		       ("pcntl.exec" "pcntl_exec(${path[, array args]})$0\n" "pcntl_exec(...)" nil
			("pcntl")
			nil nil nil nil)
		       ("pcntl.fork" "pcntl_fork()$0\n" "pcntl_fork()" nil
			("pcntl")
			nil nil nil nil)
		       ("pcntl.signal" "pcntl_signal(${signo}, ${handle[, bool restart_syscalls]})$0\n" "pcntl_signal(..., ...)" nil
			("pcntl")
			nil nil nil nil)
		       ("pcntl.waitpid" "pcntl_waitpid(${pid}, ${&status[, int options]})$0\n" "pcntl_waitpid(..., ...)" nil
			("pcntl")
			nil nil nil nil)
		       ("pcntl.wexitstatus" "pcntl_wexitstatus(${status})$0\n" "pcntl_wexitstatus(...)" nil
			("pcntl")
			nil nil nil nil)
		       ("pcntl.wifexited" "pcntl_wifexited(${status})$0\n" "pcntl_wifexited(...)" nil
			("pcntl")
			nil nil nil nil)
		       ("pcntl.wifsignaled" "pcntl_wifsignaled(${status})$0\n" "pcntl_wifsignaled(...)" nil
			("pcntl")
			nil nil nil nil)
		       ("pcntl.wifstopped" "pcntl_wifstopped(${status})$0\n" "pcntl_wifstopped(...)" nil
			("pcntl")
			nil nil nil nil)
		       ("pcntl.wstopsig" "pcntl_wstopsig(${status})$0\n" "pcntl_wstopsig(...)" nil
			("pcntl")
			nil nil nil nil)
		       ("pcntl.wtermsig" "pcntl_wtermsig(${status})$0\n" "pcntl_wtermsig(...)" nil
			("pcntl")
			nil nil nil nil)
		       ("pfsockopen" "pfsockopen(${hostname[, int port]})$0\n" "pfsockopen(...)" nil nil nil nil nil nil)
		       ("php.check.syntax" "php_check_syntax(${file_name[, string &error_message]})$0\n" "php_check_syntax(...)" nil
			("php")
			nil nil nil nil)
		       ("pi" "pi()$0\n" "pi()" nil nil nil nil nil nil)
		       ("posix.access" "posix_access(${file[, int mode]})$0\n" "posix_access(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.ctermid" "posix_ctermid()$0\n" "posix_ctermid()" nil
			("posix")
			nil nil nil nil)
		       ("posix.get.last.error" "posix_get_last_error()$0\n" "posix_get_last_error()" nil
			("posix")
			nil nil nil nil)
		       ("posix.getcwd" "posix_getcwd()$0\n" "posix_getcwd()" nil
			("posix")
			nil nil nil nil)
		       ("posix.getegid" "posix_getegid()$0\n" "posix_getegid()" nil
			("posix")
			nil nil nil nil)
		       ("posix.geteuid" "posix_geteuid()$0\n" "posix_geteuid()" nil
			("posix")
			nil nil nil nil)
		       ("posix.getgid" "posix_getgid()$0\n" "posix_getgid()" nil
			("posix")
			nil nil nil nil)
		       ("posix.getgrgid" "posix_getgrgid(${gid})$0\n" "posix_getgrgid(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.getgrnam" "posix_getgrnam(${name})$0\n" "posix_getgrnam(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.getgroups" "posix_getgroups()$0\n" "posix_getgroups()" nil
			("posix")
			nil nil nil nil)
		       ("posix.getlogin" "posix_getlogin()$0\n" "posix_getlogin()" nil
			("posix")
			nil nil nil nil)
		       ("posix.getpgid" "posix_getpgid(${pid})$0\n" "posix_getpgid(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.getpgrp" "posix_getpgrp()$0\n" "posix_getpgrp()" nil
			("posix")
			nil nil nil nil)
		       ("posix.getpid" "posix_getpid()$0\n" "posix_getpid()" nil
			("posix")
			nil nil nil nil)
		       ("posix.getppid" "posix_getppid()$0\n" "posix_getppid()" nil
			("posix")
			nil nil nil nil)
		       ("posix.getpwnam" "posix_getpwnam(${username})$0\n" "posix_getpwnam(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.getpwuid" "posix_getpwuid(${uid})$0\n" "posix_getpwuid(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.getrlimit" "posix_getrlimit()$0\n" "posix_getrlimit()" nil
			("posix")
			nil nil nil nil)
		       ("posix.getsid" "posix_getsid(${pid})$0\n" "posix_getsid(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.getuid" "posix_getuid()$0\n" "posix_getuid()" nil
			("posix")
			nil nil nil nil)
		       ("posix.isatty" "posix_isatty(${fd})$0\n" "posix_isatty(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.kill" "posix_kill(${pid}, ${sig})$0\n" "posix_kill(..., ...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.mkfifo" "posix_mkfifo(${pathname}, ${mode})$0\n" "posix_mkfifo(..., ...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.mknod" "posix_mknod(${pathname}, ${mode[, int major]})$0\n" "posix_mknod(..., ...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.setegid" "posix_setegid(${gid})$0\n" "posix_setegid(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.seteuid" "posix_seteuid(${uid})$0\n" "posix_seteuid(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.setgid" "posix_setgid(${gid})$0\n" "posix_setgid(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.setpgid" "posix_setpgid(${pid}, ${pgid})$0\n" "posix_setpgid(..., ...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.setsid" "posix_setsid()$0\n" "posix_setsid()" nil
			("posix")
			nil nil nil nil)
		       ("posix.setuid" "posix_setuid(${uid})$0\n" "posix_setuid(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.strerror" "posix_strerror(${errno})$0\n" "posix_strerror(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.times" "posix_times()$0\n" "posix_times()" nil
			("posix")
			nil nil nil nil)
		       ("posix.ttyname" "posix_ttyname(${fd})$0\n" "posix_ttyname(...)" nil
			("posix")
			nil nil nil nil)
		       ("posix.uname" "posix_uname()$0\n" "posix_uname()" nil
			("posix")
			nil nil nil nil)
		       ("pow" "pow(${base}, ${exp})$0\n" "pow(..., ...)" nil nil nil nil nil nil)
		       ("print" "print(${arg})$0\n" "print(...)" nil nil nil nil nil nil)
		       ("print.r" "print_r(${expression[, bool return]})$0\n" "print_r(...)" nil
			("print")
			nil nil nil nil)
		       ("printf" "printf(${format[, mixed args]})$0\n" "printf(...)" nil nil nil nil nil nil)
		       ("proc.close" "proc_close(${process})$0\n" "proc_close(...)" nil
			("proc")
			nil nil nil nil)
		       ("proc.open" "proc_open(${cmd}, ${descriptorspec}, ${&pipes[, string cwd]})$0\n" "proc_open(..., ..., ...)" nil
			("proc")
			nil nil nil nil)
		       ("quoted.printable.decode" "quoted_printable_decode(${str})$0\n" "quoted_printable_decode(...)" nil
			("quoted")
			nil nil nil nil)
		       ("quotemeta" "quotemeta(${str})$0\n" "quotemeta(...)" nil nil nil nil nil nil)
		       ("rad2deg" "rad2deg(${number})$0\n" "rad2deg(...)" nil nil nil nil nil nil)
		       ("rand" "rand(${[int min, int max]})$0\n" "rand(...)" nil nil nil nil nil nil)
		       ("rawurldecode" "rawurldecode(${str})$0\n" "rawurldecode(...)" nil nil nil nil nil nil)
		       ("rawurlencode" "rawurlencode(${str})$0\n" "rawurlencode(...)" nil nil nil nil nil nil)
		       ("readdir" "readdir(${dir_handle})$0\n" "readdir(...)" nil nil nil nil nil nil)
		       ("register.shutdown.function" "register_shutdown_function(${function[, mixed parameter]})$0\n" "register_shutdown_function(...)" nil
			("register")
			nil nil nil nil)
		       ("register.tick.function" "register_tick_function(${function[, mixed arg]})$0\n" "register_tick_function(...)" nil
			("register")
			nil nil nil nil)
		       ("rewinddir" "rewinddir(${dir_handle})$0\n" "rewinddir(...)" nil nil nil nil nil nil)
		       ("round" "round(${val[, int precision]})$0\n" "round(...)" nil nil nil nil nil nil)
		       ("rtrim" "rtrim(${str[, string charlist]})$0\n" "rtrim(...)" nil nil nil nil nil nil)
		       ("serialize" "serialize(${value})$0\n" "serialize(...)" nil nil nil nil nil nil)
		       ("session.cache.expire" "session_cache_expire(${[int new_cache_expire]})$0\n" "session_cache_expire(...)" nil
			("session")
			nil nil nil nil)
		       ("session.cache.limiter" "session_cache_limiter(${[string cache_limiter]})$0\n" "session_cache_limiter(...)" nil
			("session")
			nil nil nil nil)
		       ("session.decode" "session_decode(${data})$0\n" "session_decode(...)" nil
			("session")
			nil nil nil nil)
		       ("session.destroy" "session_destroy()$0\n" "session_destroy()" nil
			("session")
			nil nil nil nil)
		       ("session.encode" "session_encode()$0\n" "session_encode()" nil
			("session")
			nil nil nil nil)
		       ("session.get.cookie.params" "session_get_cookie_params()$0\n" "session_get_cookie_params()" nil
			("session")
			nil nil nil nil)
		       ("session.id" "session_id(${[string id]})$0\n" "session_id(...)" nil
			("session")
			nil nil nil nil)
		       ("session.is.registered" "session_is_registered(${name})$0\n" "session_is_registered(...)" nil
			("session")
			nil nil nil nil)
		       ("session.module.name" "session_module_name(${[string module]})$0\n" "session_module_name(...)" nil
			("session")
			nil nil nil nil)
		       ("session.name" "session_name(${[string name]})$0\n" "session_name(...)" nil
			("session")
			nil nil nil nil)
		       ("session.regenerate.id" "session_regenerate_id(${[bool delete_old_session]})$0\n" "session_regenerate_id(...)" nil
			("session")
			nil nil nil nil)
		       ("session.register" "session_register(${name[, mixed ...]})$0\n" "session_register(...)" nil
			("session")
			nil nil nil nil)
		       ("session.save.path" "session_save_path(${[string path]})$0\n" "session_save_path(...)" nil
			("session")
			nil nil nil nil)
		       ("session.set.cookie.params" "session_set_cookie_params(${lifetime[, string path]})$0\n" "session_set_cookie_params(...)" nil
			("session")
			nil nil nil nil)
		       ("session.set.save.handler" "session_set_save_handler(${open}, ${close}, ${read}, ${write}, ${destroy}, ${gc})$0\n" "session_set_save_handler(..., ..., ..., ..., ..., ...)" nil
			("session")
			nil nil nil nil)
		       ("session.start" "session_start()$0\n" "session_start()" nil
			("session")
			nil nil nil nil)
		       ("session.unregister" "session_unregister(${name})$0\n" "session_unregister(...)" nil
			("session")
			nil nil nil nil)
		       ("session.unset" "session_unset()$0\n" "session_unset()" nil
			("session")
			nil nil nil nil)
		       ("session.write.close" "session_write_close()$0\n" "session_write_close()" nil
			("session")
			nil nil nil nil)
		       ("setcookie" "setcookie(${name[, string value]})$0\n" "setcookie(...)" nil nil nil nil nil nil)
		       ("setlocale" "setlocale(${category}, ${locale[, string ...]})$0\n" "setlocale(..., ...)" nil nil nil nil nil nil)
		       ("settype" "settype(${&var}, ${type})$0\n" "settype(..., ...)" nil nil nil nil nil nil)
		       ("shell.exec" "shell_exec(${cmd})$0\n" "shell_exec(...)" nil
			("shell")
			nil nil nil nil)
		       ("sin" "sin(${arg})$0\n" "sin(...)" nil nil nil nil nil nil)
		       ("sinh" "sinh(${arg})$0\n" "sinh(...)" nil nil nil nil nil nil)
		       ("sleep" "sleep(${seconds})$0\n" "sleep(...)" nil nil nil nil nil nil)
		       ("split" "split(${pattern}, ${string[, int limit]})$0\n" "split(..., ...)" nil nil nil nil nil nil)
		       ("spliti" "spliti(${pattern}, ${string[, int limit]})$0\n" "spliti(..., ...)" nil nil nil nil nil nil)
		       ("sql.regcase" "sql_regcase(${string})$0\n" "sql_regcase(...)" nil
			("sql")
			nil nil nil nil)
		       ("sqrt" "sqrt(${arg})$0\n" "sqrt(...)" nil nil nil nil nil nil)
		       ("srand" "srand(${[int seed]})$0\n" "srand(...)" nil nil nil nil nil nil)
		       ("strftime" "strftime(${format[, int timestamp]})$0\n" "strftime(...)" nil nil nil nil nil nil)
		       ("strptime" "strptime(${date}, ${format})$0\n" "strptime(..., ...)" nil nil nil nil nil nil)
		       ("strtotime" "strtotime(${time[, int now]})$0\n" "strtotime(...)" nil nil nil nil nil nil)
		       ("strval" "strval(${var})$0\n" "strval(...)" nil nil nil nil nil nil)
		       ("switch" "switch (${on}) {\n  $0\n  default:\n}\n" "switch (...) {...}" nil nil nil nil nil nil)
		       ("syslog" "syslog(${priority}, ${message})$0\n" "syslog(..., ...)" nil nil nil nil nil nil)
		       ("system" "system(${command[, int &return_var]})$0\n" "system(...)" nil nil nil nil nil nil)
		       ("tan" "tan(${arg})$0\n" "tan(...)" nil nil nil nil nil nil)
		       ("tanh" "tanh(${arg})$0\n" "tanh(...)" nil nil nil nil nil nil)
		       ("time" "time()$0\n" "time()" nil nil nil nil nil nil)
		       ("time.sleep.until" "time_sleep_until(${timestamp})$0\n" "time_sleep_until(...)" nil
			("time")
			nil nil nil nil)
		       ("trim" "trim(${str[, string charlist]})$0\n" "trim(...)" nil nil nil nil nil nil)
		       ("try" "try {\n  $0\n}\n" "try { ... }" nil nil nil nil nil nil)
		       ("ucfirst" "ucfirst(${str})$0\n" "ucfirst(...)" nil nil nil nil nil nil)
		       ("ucwords" "ucwords(${str})$0\n" "ucwords(...)" nil nil nil nil nil nil)
		       ("uniqid" "uniqid(${[string prefix]})$0\n" "uniqid(...)" nil nil nil nil nil nil)
		       ("unixtojd" "unixtojd(${[int timestamp]})$0\n" "unixtojd(...)" nil nil nil nil nil nil)
		       ("unpack" "unpack(${format}, ${data})$0\n" "unpack(..., ...)" nil nil nil nil nil nil)
		       ("unregister.tick.function" "unregister_tick_function(${function_name})$0\n" "unregister_tick_function(...)" nil
			("unregister")
			nil nil nil nil)
		       ("unserialize" "unserialize(${str})$0\n" "unserialize(...)" nil nil nil nil nil nil)
		       ("unset" "unset(${var[, mixed var]})$0\n" "unset(...)" nil nil nil nil nil nil)
		       ("urldecode" "urldecode(${str})$0\n" "urldecode(...)" nil nil nil nil nil nil)
		       ("urlencode" "urlencode(${str})$0\n" "urlencode(...)" nil nil nil nil nil nil)
		       ("usleep" "usleep(${micro_seconds})$0\n" "usleep(...)" nil nil nil nil nil nil)
		       ("var.dump" "var_dump(${expression[, mixed expression]})$0\n" "var_dump(...)" nil
			("var")
			nil nil nil nil)
		       ("var.export" "var_export(${expression[, bool return]})$0\n" "var_export(...)" nil
			("var")
			nil nil nil nil)
		       ("vprintf" "vprintf(${format}, ${args})$0\n" "vprintf(..., ...)" nil nil nil nil nil nil)
		       ("vsprintf" "vsprintf(${format}, ${args})$0\n" "vsprintf(..., ...)" nil nil nil nil nil nil)
		       ("while" "while (${condition}) {\n   $0\n}\n\n" "while (...) {...}" nil nil nil nil nil nil)
		       ("wordwrap" "wordwrap(${str[, int width]})$0\n" "wordwrap(...)" nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '((nil "__${init}__" "__...__" nil nil nil nil nil nil)
		       ("class" "class ${1:ClassName}(${2:object}):\n   \"\"\"$3\n   \"\"\"\n\n   def __init__(self, $4):\n       \"\"\"$5\n       ${4:$\n       (let* ((indent\n               (concat \"\\n\" (make-string (current-column) 32)))\n              (args\n               (mapconcat\n                '(lambda (x)\n                   (if (not (string= (nth 0 x) \"\"))\n                       (concat \"- \" (char-to-string 96) (nth 0 x)\n                               (char-to-string 96) \":\")))\n                (mapcar\n                 '(lambda (x)\n                    (mapcar\n                     (lambda (x)\n                       (replace-regexp-in-string \"\\s*$\" \"\"\n                        (replace-regexp-in-string \"^\\s*\" \"\" x))) x))\n                 (mapcar '(lambda (x) (split-string x \"=\"))\n                         (split-string text \",\")))\n                indent)))\n         (if (string= args \"\")\n             (make-string 3 34)\n           (mapconcat\n            'identity\n            (list \"\" \"Arguments:\" args (make-string 3 34))\n            indent)))\n       }\n       ${4:$\n       (mapconcat\n        '(lambda (x)\n           (if (not (string= (nth 0 x) \"\"))\n               (concat \"self._\" (nth 0 x) \" = \" (nth 0 x))))\n        (mapcar\n         '(lambda (x)\n            (mapcar\n             '(lambda (x)\n                (replace-regexp-in-string \"\\s*$\" \"\"\n                 (replace-regexp-in-string \"^\\s*\" \"\" x)))\n             x))\n         (mapcar '(lambda (x) (split-string x \"=\"))\n                 (split-string text \",\")))\n        (concat \"\\n\" (make-string (current-column) 32)))\n       }\n       $0\n" "class" nil nil nil nil nil nil)
		       ("key" "def ${1:name}($2):\n   \"\"\"$3\n   ${2:$\n   (let* ((indent\n           (concat \"\\n\" (make-string (current-column) 32)))\n          (args\n           (mapconcat\n            '(lambda (x)\n               (if (not (string= (nth 0 x) \"\"))\n                   (concat \"- \" (char-to-string 96) (nth 0 x)\n                           (char-to-string 96) \":\")))\n            (mapcar\n             '(lambda (x)\n                (mapcar\n                 '(lambda (x)\n                    (replace-regexp-in-string \"\\s*$\" \"\"\n                     (replace-regexp-in-string \"^\\s*\" \"\" x)))\n                 x))\n             (mapcar '(lambda (x) (split-string x \"=\"))\n                     (split-string text \",\")))\n            indent)))\n     (if (string= args \"\")\n         (make-string 3 34)\n       (mapconcat\n        'identity\n        (list \"\" \"Arguments:\" args (make-string 3 34))\n        indent)))\n   }\n   $0\n" "def" nil nil nil nil nil nil)
		       ("defm" "def ${1:name}(self, $2):\n   \"\"\"$3\n   ${2:$\n   (let* ((indent\n           (concat \"\\n\" (make-string (current-column) 32)))\n          (args\n           (mapconcat\n            '(lambda (x)\n               (if (not (string= (nth 0 x) \"\"))\n                   (concat \"- \" (char-to-string 96) (nth 0 x)\n                           (char-to-string 96) \":\")))\n            (mapcar\n             '(lambda (x)\n                (mapcar\n                 '(lambda (x)\n                    (replace-regexp-in-string \"\\s*$\" \"\"\n                     (replace-regexp-in-string \"^\\s*\" \"\" x)))\n                 x))\n             (mapcar '(lambda (x) (split-string x \"=\"))\n                     (split-string text \",\")))\n            indent)))\n     (if (string= args \"\")\n         (make-string 3 34)\n       (mapconcat\n        'identity\n        (list \"\" \"Arguments:\" args (make-string 3 34))\n        indent)))\n   }\n   $0\n" "defm" nil nil nil nil nil nil)
		       ("for" "for ${var} in ${collection}:\n    $0" "for ... in ... : ..." nil nil nil nil nil nil)
		       ("ifmain" "if __name__ == '__main__':\n    $0" "if __name__ == '__main__': ..." nil nil nil nil nil nil)
		       ("propg" "def _get_${1:foo}(self):\n   return self._$1\n\n$1 = property(_get_$1)\n\n$0\n" "_get_foo ... foo=property(...)" nil nil nil nil nil nil)
		       ("propsg" "def _set_${1:foo}(self, value):\n   self._$1 = value\n\ndef _get_$1(self):\n   return self._$1\n\n$1 = property(_get_$1, _set_$1)\n\n$0\n" "_get_foo ... _set_foo ... foo=property(...)" nil nil nil nil nil nil)
		       ("while" "while ${condition}:\n    $0" "while ... : ..." nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '(("blog" "<a href=\"http://verylongwebsitename.com/wp-content/uploads/2012/${1:mm}/${2:folder}/fig${3:x}_l.png\">\n  <img class=\"alignnone\"\n       title=\"fig$3\"\n       src=\"http://verylongwebsitename.com/wp-content/uploads/2012/$1/$2/fig$3_m.png\"\n       alt=\"${5:alttext}\"\n       width=\"${6:700}\"\n       height=\"${7:500}\"/>\n</a>\n$0\n\n" "blog picture link" nil nil nil nil nil nil)
		       ("curtime" "`(current-time-string)`" "(current time)" nil nil nil nil nil nil)
		       ("email" "`user-mail-address`" "(user's email)" nil nil nil nil nil nil)
		       ("lorem" "`(lorem-ipsum)`\n" "lorem ipsum" nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;;;; yasnippet-bundle.el - automatically compiled snippets for `nil' , do not edit!
;;;
(yas/define-snippets 'nil
		     '(("args" "        Dim i As Integer\n        For i = 0 To args.Length - 1\n            Select Case(args(i))\n                Case \"-b\":\n                    If (_boolValue = True) Then\n                        Throw New ArgumentException(args(i))\n                    End If\n                    _boolValue = True\n\n                Case \"-s\":\n                    i += 1\n                    If (args.Length <= i) Then\n                        Throw New ArgumentException(args(i))\n                    End If\n                    If Not (Me._stringValue Is Nothing) Then\n                        Throw New ArgumentException(args((i - 1)))\n                    End If\n                    _stringValue = args(i)\n\n                Case \"-n\":\n                    i += 1\n                    If (args.Length <= i) Then\n                        Throw New ArgumentException(args(i))\n                    End If\n                    If (Me._intValue <> 0) Then\n                        Throw New ArgumentException(args((i - 1)))\n                    End If\n                    If args(i).StartsWith(\"0x\") Then\n                        Me._intValue = Integer.Parse(args(i).Substring(2), NumberStyles.AllowHexSpecifier)\n                    Else\n                        Me._intValue = Integer.Parse(args(i))\n                    End If\n\n                case \"-?\":\n                    Throw New ArgumentException(args(i))\n\n                Case Else:\n                    Throw New ArgumentException(args(i))\n\n            End Select\n        Next i\n        If (Me._intValue = 0) Then\n            Me._intValue = Me.DefaultIntValue\n        End If\n\n" "Select Case(args(i) ..." nil nil nil nil nil nil)
		       ("bsd" "' Licensed under the Simplified BSD License.\n'\n' Redistribution and use in source and binary forms, with or without\n' modification, is permitted provided that the following conditions\n' are met: Redistributions of source code must retain the above\n' copyright notice, this list of conditions and the following\n' disclaimer.  Redistributions in binary form must reproduce the above\n' copyright notice, this list of conditions and the following\n' disclaimer in the documentation and/or other materials provided with\n' the distribution.\n'\n' THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n' \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\n' LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n' A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT\n' HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,\n' INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,\n' BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS\n' OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED\n' AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT\n' LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY\n' WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n' POSSIBILITY OF SUCH DAMAGE.\n'\n' ------------------------------------------------------------------\n" "BSD License" nil nil nil nil nil nil)
		       ("for" "For ${1:index} As Integer = 0 To ${2:finish}\n    ${3:''body}\nNext $1\n\n" "for (...) { ... }" nil nil nil nil nil nil)
		       ("fore" "Dim ${1:var} As ${2:type}\nFor Each $1 In ${3:IEnumerable}\n    ${4:'' body...}\nNext\n\n" "For Each ... Next" nil nil nil nil nil nil)
		       ("if" "If ${1:predicate} Then\n  ${2:// then clause}\nEnd If\n" "If ... Then  ... End If" nil nil nil nil nil nil)
		       ("ife" "If ${1:predicate} Then\n  ${2:// then clause}\nElse\n  ${3:// else clause}\nEnd If" "If ... Then  ... Else  ... End If" nil nil nil nil nil nil)
		       ("prop" "   Private m${1:Name} as ${2:Type}\n   Public Property ${1:Name}() As ${2:Type}\n\n      Get\n         Return m${1:Name}\n      End Get\n\n      Set(ByVal value As ${2:Type})\n            m${1:Name} = value\n      End Set\n\n   End Property ' ${1:Name}\n\n" "property ... { ... }" nil nil nil nil nil nil)
		       ("writeline" "System.Console.WriteLine(${0://thing to do})\n" "WriteLine (...) { ... }" nil nil nil nil nil nil))
		     'nil)


;;; yasnippet-bundle.el - automatically compiled snippets for `nil' end here
;;;