;;; Compiled snippets and support files for `nxml-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'nxml-mode
                     '(("cdata" "<![CDATA[\n $0\n]]>\n" "CDATA" nil nil nil nil nil nil)
                       ("flow" "<Flow name=\"${1:example flow name}\">\n  <Description>$2</Description>\n  <Request>\n    <Step>\n      <FaultRules/>\n      <Name>${3:PolicyName}</Name>\n    </Step>\n  </Request>\n  <Response/>\n  <Condition>(proxy.pathsuffix MatchesPath \"/${4:urlpath}\") and (request.verb = \"${5:$$(yas/choose-value '(\"PATCH\" \"OPTIONS\" \"DELETE\" \"PUT\" \"POST\" \"GET\"))}\")</Condition>\n</Flow>\n" "<Flow...>" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Fri Jul 24 18:52:09 2015
