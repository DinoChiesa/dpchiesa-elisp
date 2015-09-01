;;; Compiled snippets and support files for `sh-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sh-mode
                     '(("case" "case ${1:word} in\n  ${2:pattern} )\n    $0;;\nesac\n" "case ... esac" nil nil nil nil nil nil)
                       ("elif" "elif ${2:[[ ${1:condition} ]]}; then\n  ${0:#statements}\n" "elif ..." nil nil nil nil nil nil)
                       ("for" "for (( i = 0; i < ${1:10}; i++ )); do\n        ${0:#statements}\ndone\n" "for ... done" nil nil nil nil nil nil)
                       ("forin" "for ${1:i} in 0..${2:last}; do\n        ${0:#statements}\ndone\n" "for ... in ... done" nil nil nil nil nil nil)
                       ("getopt" "while getopts \"hp:n:\" opt; do\n  case $opt in\n    h) usage ;;\n    p) PSWD=$OPTARG ;;\n    n) NEWPSWD=$OPTARG ;;\n    *) usage ;;\n  esac\ndone\n" "while getopts...; do...done" nil nil nil nil nil nil)
                       ("here" "<<-${2:'${1:TOKEN}'}\n        $0\n${1/['\"`](.+)['\"`]/$1/}\n" "Here Document" nil nil nil nil nil nil)
                       ("if" "if ${2:[[ ${1:condition} ]]}; then\n  ${0:#statements}\nfi\n" "if ... fi" nil nil nil nil nil nil)
                       ("mycurl" "## function MYCURL\n## side effects:\n## 1. puts curl output into file named ${CURL_OUT}. The caller be set the environment\n##    variable prior to calling this function.\n## 2. puts curl http_status into CURL_RC\nfunction MYCURL() {\n  echo \"curl $@\"\n  CURL_RC=`curl -s -w \"%{http_code}\" -o ${CURL_OUT} \"$@\"`\n  echo \"==> ${CURL_RC}\"\n}\n\n# usage example:\n#\n#  MYCURL -u ${ID}:${PSWD} -X PUT http://${MSIP}:${MSPORT}/v1/users/${EMAILID}\n#\n#  if [ \"x${CURL_RC}\" != \"x200\" ]; then\n#    echo \"Error: Failed to change the system admin password.\"\n#    echo \"HTTP status: ${CURL_RC}\"\n#    echo \"Payload: \"\n#    cat ${CURL_OUT}\n#    exit 1\n#  fi\n" "function MYCURL {...}" nil nil nil nil nil nil)
                       ("shebang" "#!/bin/bash\n# -*- mode:shell-script; coding:utf-8; -*-\n" "!/bin/bash" nil nil nil nil nil nil)
                       ("temp" "${1:TMPFILE}=\"$(mktemp -t ${2:`echo \"${TM_FILENAME%.*}\" | sed -e 's/[^a-zA-Z]/_/g' -e 's/^$/untitled/'`})\"\n${3:${4/(.+)/trap \"/}${4:rm -f '\\$${1/.*\\s//}'}${4/(.+)/\" 0               # EXIT\n/}${5/(.+)/trap \"/}${5:rm -f '\\$${1/.*\\s//}'; exit 1}${5/(.+)/\" 2       # INT\n/}${6/(.+)/trap \"/}${6:rm -f '\\$${1/.*\\s//}'; exit 1}${6/(.+)/\" 1 15    # HUP TERM\n/}}\n" "Tempfile" nil nil nil nil nil nil)
                       ("until" "until ${2:[[ ${1:condition} ]]}; do\n        ${0:#statements}\ndone\n" "until ... done" nil nil nil nil nil nil)
                       ("usage" "\nfunction usage() {\n  local CMD=\\`basename \\$0\\`\n  echo \"$CMD: Tickle Apigee Gateway, and maybe change the system admin password.\"\n  echo \"usage: \"\n  echo \"   $CMD -p password  [-n new_password]\"\n  echo\n  exit 1\n}\n$0" "function usage() {...}" nil nil nil nil nil nil)
                       ("while" "while ${2:[[ ${1:condition} ]]}; do\n        ${0:#statements}\ndone\n" "while ... done" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Fri Jul 24 18:52:09 2015
