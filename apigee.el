;;; apigee.el --- utility functions for working with Apigee platform
;;
;; Copyright (C) 2013 Dino Chiesa and Apigee Corporation
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : May 2013
;; Modified   : May 2013
;; Version    : 1.0
;; Keywords   : apigee
;; Requires   : s.el
;; License    : New BSD
;; X-URL      : https://github.com/dpchiesa/elisp
;; Last-saved : <2013-July-30 11:59:54>
;;
;;; Commentary:
;;
;; This module defines a few elisp functions that are handy for working
;; with API Proxy definition bundles within emacs. Per ejemplo,
;;  - creating a new blank proxy
;;  - zipping and importing a bundle
;;  - validaitng a bundle (coming soon)
;;  - adding a new policy to a bundle (coming soon)
;;
;;
;;; License
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; Neither the name of the author or any contributors, nor the names of
;; any organizations they belong to, may be used to endorse or promote
;; products derived from this software without specific prior written
;; permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;

(require 's) ;; magnars' long lost string library

(defgroup apigee nil
  "Utility fns for use with the Apigee platform.")

(defcustom apigee-apiproxies-home "~/dev/apiproxies/"
  "The directory to contain newly-created API proxy bundle directories.

   (require 'apigee)
   (setq apigee-apiproxies-home \"~/dev/apiproxies/\")

Or you can customize this variable.
"
  :group 'apigee)

(defcustom apigee-upload-bundle-pgm "~/dev/apiproxies/pushapi"
  "The script or program that uploads proxies to the Apigee gateway.
Can be a python or bash script or other program. Should be executable.
Specify the full path. Use this in your emacs:

   (require 'apigee)
   (setq apigee-upload-bundle-pgm \"~/dev/apiproxies/pushapi\"
         apigee-upload-bundle-args \"-v\")

Or you can customize this variable.
"
  :group 'apigee)


(defcustom apigee-upload-bundle-args "-v"
  "The arguments to pass to the script or program that
uploads proxies to the Apigee gateway. Use this in your emacs:

   (require 'apigee)
   (setq apigee-upload-bundle-pgm \"~/dev/apiproxies\"
         apigee-upload-bundle-args \"-v\")

Or you can customize this variable.
"
  :group 'apigee)

(defcustom apigee-temp-dir "/tmp"
  "The temporary directory to use for zip bundles. "
  :type 'string
  :group 'apigee)


(defcustom apigee-prompt-mechanism 'x-popup-menu
  "The mechanism used to prompt the user for his choice.
Options: 'x-popup-menu, or 'dropdown-list.  When setting
this, set it to the symbol, not to the string or the actual
function.  Eg

  (setq apigee-prompt-mechanism 'x-popup-menu)

"
  :type 'symbol
  :options '('x-popup-menu 'dropdown-list)
  :group 'apigee)


;; The command "template" to use when creating the API bundle zip. In
;; this template, the %f is replaced with the name of the zip file to
;; create. This template probably never needs to be customized.
(defvar apigee-create-bundle-zip-command-template "zip %f -r apiproxy/ -x \"*.*~\" ")

;;(setq apigee-create-bundle-zip-command-template "zip %f -r apiproxy/ -x \"*.*~\"")



(defun apigee-path-of-apiproxy ()
  "Returns the path of apiproxy that acts as the parent of
the current file or directory.

If the apiproxy is defined in a dir structure like:

~/dev/apiproxies/APINAME/apiproxy
~/dev/apiproxies/APINAME/apiproxy/APINAME.xml
~/dev/apiproxies/APINAME/apiproxy/resources
~/dev/apiproxies/APINAME/apiproxy/resources/...
~/dev/apiproxies/APINAME/apiproxy/targets
~/dev/apiproxies/APINAME/apiproxy/targets/..
~/dev/apiproxies/APINAME/apiproxy/proxies
~/dev/apiproxies/APINAME/apiproxy/proxies/..
..

then the return value is: ~/dev/apiproxies/APINAME


"
  (interactive)
  (let ((elts (reverse (split-string (file-name-directory default-directory) "/")))
        r)
    (while (and elts (not r))
      (if (string= (car elts) "apiproxy")
          (setq r (reverse (cdr elts)))
        (setq elts (cdr elts))))
    (if r
        (mapconcat 'identity r "/") )))



(defun apigee-apiproxy-name ()
  "Get a name for the API Proxy bundle that contains
the file or directory currently being edited.
"
  (interactive)
  (let ((apiproxy-dir (apigee-path-of-apiproxy)))
    (if apiproxy-dir
        (file-name-nondirectory apiproxy-dir))))


(defun apigee-get-name-for-api-bundle-zip ()
  "Get a timestamped name for the API bundle zip that contains
the file or directory currently being edited.
"
  (interactive)
  (let ((apiproxy-dir (apigee-path-of-apiproxy)))
    (if apiproxy-dir
        (let ((api-name (file-name-nondirectory apiproxy-dir))
              (timestamp (format-time-string "%Y%m%d-%H%M%S")))
          (concat api-name "-" timestamp ".zip")))))


;; The following fn can be used to help create the zip and upload from
;; emacs.  Currently this module delegates that to the pushapi script,
;; so this fn is unnecessary.
(defun apigee-get-create-api-bundle-zip-cmd ()
  "Get the command that creates an API bundle zip. for the file
or directory currently being edited.
"
  (interactive)
  (let ((api-bundle-name (concat apigee-temp-dir "/" (apigee-get-name-for-api-bundle-zip)))
        (txt apigee-create-bundle-zip-command-template))
    (if (and api-bundle-name
             (string-match "^\\(.+ \\)\\(%f\\)\\( .+\\)" txt))
        (concat
         (match-string 1 txt)
         api-bundle-name
         (match-string 3 txt)))))

(defun apigee-insure-trailing-slash (path)
  "Insure the given path ends with a slash.
This is usedful with `default-directory'.  Setting
`default-directory' to a value that does not end with a slash
causes it to use the parent directory.
"
  (if (s-ends-with? "/" path)
      path)
  (concat path "/"))


;; The following fn can be used to create the zip and upload from emacs.
;; Currently this module delegates that to the pushapi script, so this
;; fn is unnecessary.
(defun apigee-create-api-bundle-zip ()
  "Create the API bundle zip that contains the file or directory
currently being edited.
"
  (interactive)
  (let ((cmd (apigee-get-create-api-bundle-zip-cmd))
        (bundle-dir (apigee-path-of-apiproxy))
        buffer)
    (setq buffer (get-buffer-create
                  (concat "*Bundle Create "
                          (file-name-nondirectory bundle-dir) "*")))
    (message "Creating zip via %s" cmd)
    (with-current-buffer buffer
      (setq default-directory (apigee-insure-trailing-slash bundle-dir))
      (goto-char (point-max))
      (insert "\n\n============================================\n")
      (goto-char (point-max))
      (shell-command "pwd" t nil)
      (goto-char (point-max))
      (shell-command (concat "echo " cmd) t nil)
      (goto-char (point-max))
      (shell-command cmd t nil))

    (switch-to-buffer-other-window buffer)))



(defun apigee-upload-bundle-with-pushapi ()
  "Interactive fn that uses the pushapi script to upload the bundle
that contains the file or directory currently being edited.
"
  (interactive)
  (let ((proxy-dir (apigee-path-of-apiproxy)))
    (if proxy-dir
        (if (file-exists-p apigee-upload-bundle-pgm)
            (progn
              (set (make-local-variable 'compile-command)
                   (concat
                    apigee-upload-bundle-pgm " "
                    apigee-upload-bundle-args " "
                    proxy-dir))
              (call-interactively 'compile))))))


(defun apigee--is-directory (dir-name)
  "Tests to see whether a name refers to a directory"
  (and
   (file-exists-p dir-name)
   (car (file-attributes dir-name))))

(defun apigee--java-get-time-in-millis ()
  "Returns a string that contains a number equal in value to
what is returned from the java snippet:
      (new GregrianCalendar()).getTimeInMillis()
"
  (let ((ct (current-time)))
    (format "%d" (+ (* (+ (* (car ct) 65536) (cadr ct)) 1000) (/ (caddr ct) 1000)))))


(defun apigee-policy-name-is-available (pname)
  "Return true if the passed policy name is unused, in other words
if no file exists by that name in the given proxy.
"
  (let* ((proxy-dir (apigee-path-of-apiproxy))
         (policy-dir (concat proxy-dir "/apiproxy/policies/"))
         (filename-to-check (concat policy-dir pname ".xml")))
    (not (file-exists-p filename-to-check))))


(defun apigee--default-val-for-policy-name (ptype)
  "Returns a string that contains a default policy name, uses a counter
that is indexed per policy type within each API Proxy.
"
  (let* ((proxy-name (apigee-apiproxy-name))
         (key (concat proxy-name "." ptype))
         (val 1)
         (pname (concat ptype "-" (format "%d" val))))

    (while (not (apigee-policy-name-is-available pname))
      (setq val (1+ val)
            pname (concat ptype "-" (format "%d" val))))

    pname))


(defun apigee--get-createdby ()
  "Returns a string that contains a username, useful for
applying as the CreatedBy element in an API Proxy.
"
  (or (getenv "LOGNAME") (getenv "USER") "orgAdmin"))



(defun apigee--random-string (&optional len)
  "produce a string of length LEN containing random characters,
or of length 8 if the len is not specified.
"
  (let (s '())
    (if (not len) (setq len 8))
    (if (> len 144) (setq len 8)) ;; sanity
    (while (> len 0)
      (setq s (cons (+ (random 26) 97) s)
            len (- len 1)))
    (mapconcat 'string s "")))


(defun apigee-new-proxy (proxy-name)
  "Interactive fn that creates a new exploded proxy bundle directory
structure, in the `apigee-apiproxies-home' directory.
"
  (interactive "Mproxy name: ")
  (if (not (s-ends-with-p "/" apigee-apiproxies-home))
      (setq apigee-apiproxies-home (concat apigee-apiproxies-home "/")))

  (let ((proxy-dir (concat apigee-apiproxies-home proxy-name))
        apiproxy-dir
        file-attrs)

    (if (file-exists-p proxy-dir)
        (error "proxy dir already exists")
      (if (not (apigee--is-directory apigee-apiproxies-home))
          (error "set apigee-apiproxies-home to the name of an existing directory.")
        (setq apiproxy-dir (concat proxy-dir "/apiproxy/"))
        (make-directory apiproxy-dir t)
        ;; create the sub-directories
        (let ((subdirs (list "proxies" "targets" "resources" "policies"
                             "resources/java" "resources/xsl"
                             "resources/node" "resources/jsc" )))
          (while subdirs
            (make-directory (concat apiproxy-dir (car subdirs)))
            (setq subdirs (cdr subdirs))))
        ;; create the toplevel xml file
        (with-temp-file (concat apiproxy-dir proxy-name ".xml")
          (insert
           (concat
            "<APIProxy revision='1' name='" proxy-name "'>\n"
            "  <ConfigurationVersion minorVersion='0' majorVersion='4'/>\n"
            "  <CreatedAt>" (apigee--java-get-time-in-millis) "</CreatedAt>\n"
            "  <CreatedBy>" (apigee--get-createdby) "</CreatedBy>\n"
            "  <Description></Description>\n"
            "  <DisplayName>" proxy-name "</DisplayName>\n"
            "  <LastModifiedAt>" (apigee--java-get-time-in-millis) "</LastModifiedAt>\n"
            "  <LastModifiedBy>orgAdmin</LastModifiedBy>\n"
            "  <TargetEndpoints>\n"
            "    <TargetEndpoint>default</TargetEndpoint>\n"
            "  </TargetEndpoints>\n"
            "</APIProxy>\n")))

        (with-temp-file (concat apiproxy-dir "targets/default.xml")
          (insert "<TargetEndpoint name='default'>
  <Description>Apigee auto generated target endpoint</Description>
  <FaultRules/>
  <Flows/>
  <PreFlow name='PreFlow'>
    <Request>
      <Step>
        <FaultRules/>
        <Name>RaiseFault.For.Unknown.Request</Name>
      </Step>
    </Request>
    <Response/>
  </PreFlow>

  <HTTPTargetConnection>
    <Properties/>
    <URL>http://internal.example.com/v1/XYZ/something</URL>
  </HTTPTargetConnection>
</TargetEndpoint>\n"))

        (with-temp-file (concat apiproxy-dir "proxies/default.xml")
          (insert
           "<ProxyEndpoint name='default'>
  <Description>Default Proxy</Description>
  <HTTPProxyConnection>
    <BasePath>/v1/" (apigee--random-string) "</BasePath>
    <Properties/>
    <VirtualHost>default</VirtualHost>
    <VirtualHost>secure</VirtualHost>
  </HTTPProxyConnection>

  <FaultRules/>

  <PreFlow name=\"PreFlow\">
      <Request/>
      <Response/>
  </PreFlow>
  <PostFlow name=\"PostFlow\">
      <Request/>
      <Response/>
  </PostFlow>

  <Flows>
    <Flow name='test " (apigee--random-string) " " (apigee--random-string) "'>
      <Description>insert description here</Description>
      <Request>
        <Step>
          <FaultRules/>
          <Name>InsertPolicyNameHere</Name>
        </Step>
      </Request>
      <Response/>
      <Condition>(proxy.pathsuffix MatchesPath \"/foo\") and (request.verb = \"GET\")</Condition>
    </Flow>
  </Flows>

  <RouteRule name='InvokeRouteRule'>
    <TargetEndpoint>default</TargetEndpoint>
  </RouteRule>

</ProxyEndpoint>\n"))

        (find-file-existing apiproxy-dir)
        ))))


(defconst apigee--policy-alist
    (list
     '("AssignMessage"
       "AssignMessage"
       "<AssignMessage name='##'>
  <AssignTo createNew='false' type='${1:$$(yas/choose-value '(\"request\" \"response\"))}'/>
  <Set>
    <QueryParams>
      <QueryParam name='${2:outgoingParamName}'>{request.queryparam.url}</QueryParam>
      <QueryParam name='apiKey'>Something</QueryParam>
    </QueryParams>
    <Verb>GET</Verb>
  </Set>
  <!-- Set other flow variables for use in the final response -->
  <AssignVariable>
    <Name>urlshortener.longUrl</Name>
    <Ref>request.queryparam.url</Ref>
  </AssignVariable>
</AssignMessage>\n")

     '("AssignMessage - Store Original Accept"
     "AssignMessage"
     "<AssignMessage name='##'>
  <AssignVariable>
    <Name>${1}.originalAccept</Name>
    <Ref>request.header.Accept</Ref>
  </AssignVariable>
</AssignMessage>\n")

     '("AssignMessage - Set Content-Type"
       "AssignMessage"
       "<AssignMessage name='##'>
  <AssignTo createNew='false' type='response'/>
  <!-- force content-type. This allows ExtractVariables to work. -->
  <Set>
    <Headers>
      <Header name='Content-Type'>application/json</Header>
    </Headers>
  </Set>
</AssignMessage>\n")

     '("KVM - Put"
       "KVM-PUT"
       "<KeyValueMapOperations name='##'
                       mapIdentifier='${1:nameOfMap}'>
  <Scope>${2:$$(yas/choose-value '(\"organization\" \"environment\" \"apiproxy\"))}</Scope>
  <Put override='true'>
    <Key>
      <Parameter ref='${3:variable.containing.key}'/>
    </Key>
    <Value ref='${4:variable.containing.value.to.store}'/>
    <Value ref='${5:another.variable.with.a.value.to.store}'/>
  </Put>
</KeyValueMapOperations>\n")

     '("KVM - Get"
       "KVM-Get"
       "<KeyValueMapOperations name='##'
                       mapIdentifier='${1:nameOfMap}'>
  <Scope>${2:$$(yas/choose-value '(\"organization\" \"environment\" \"apiproxy\"))}</Scope>
  <Get assignTo='${3:variable.to.set}' index='2'>
    <Key>
      <Parameter ref='${4:variable.containing.key}'/>
    </Key>
  </Get>

</KeyValueMapOperations>\n")

     '("Quota"
       "Quota"

       "<Quota async='false' continueOnError='false' enabled='true' name='##'>
    <DisplayName>##</DisplayName>
    <FaultRules/>
    <Properties/>
    <Allow count='${1:1000}' countRef='request.header.allowed_quota'/>
    <Interval ref='request.header.quota_count'>1</Interval>
    <Distributed>false</Distributed>
    <Synchronous>false</Synchronous>
    <TimeUnit ref='request.header.quota_timeout'>${2:$$(yas/choose-value '(\"second\" \"minute\" \"hour\" \"day\" \"month\"))}</TimeUnit>
</Quota>")

     '("Quota - Product"
       "Quota"
     "<Quota name='##'>
  <Interval ref='apiproduct.developer.quota.interval'/>
  <TimeUnit ref='apiproduct.developer.quota.timeunit'/>
  <Allow countRef='apiproduct.developer.quota.limit'/>
  <Identifier ref='client_id'/>
</Quota>\n")

     '("GetAPIProduct - fixed"
       "GetAPIProduct"
       "<GetAPIProduct name='##'>
    <!-- http://apigee.com/docs/api-platform/content/retrieve-api-product-settings-using-getapiproduct -->
    <!-- values will be stored in getapiproduct.##.apiproduct.name ... etc -->
    <APIProduct>${1:PremiumAPIProductName}</APIProduct>
</GetAPIProduct>\n")

     '("GetAPIProduct - variable"
       "GetAPIProduct"
       "<GetAPIProduct name='##'>
    <APIProduct ref='{${1:variable.name}'/>
</GetAPIProduct>\n")

     '("JSONToXML"
       "JSONToXML"
       "<JSONToXML name='##'>
  <Options>
  </Options>
</JSONToXML>")

     '("XMLToJSON"
       "XMLToJSON"
       "<XMLToJSON name='##'>
    <DisplayName>XML2JSON Mediation</DisplayName>
    <Description>This policy converts the XML returned by origin servers
    to JSON Message. it can be applied on the response when the Accept
    Encoding on the original request is application/json</Description>
        <Format>yahoo</Format>
    <!--<Options>
        <RecognizeNumber>true</RecognizeNumber>
        <RecognizeBoolean>true</RecognizeBoolean>
        <RecognizeNull>true</RecognizeNull>
        <NullValue>NULL</NullValue>
        <NamespaceSeparator>***</NamespaceSeparator>
        <NamespaceBlockName>#namespaces</NamespaceBlockName>
        <DefaultNamespaceNodeName>&amp;</DefaultNamespaceNodeName>
        <TextAlwaysAsProperty>false</TextAlwaysAsProperty>
        <TextNodeName>TEXT</TextNodeName>
        <AttributeBlockName>ATT_BLOCK</AttributeBlockName>
        <AttributePrefix>ATT_</AttributePrefix>
    </Options>-->
</XMLToJSON>\n")


     '("ExtractVariables - JSON"
       "Extract"
       "<ExtractVariables name='##'>
  <VariablePrefix>$1</VariablePrefix>
  <JSONPayload>
    <Variable name='$2'>
       <JSONPath>\\$.$3</JSONPath>
    </Variable>
  </JSONPayload>
</ExtractVariables>")

     '("ExtractVariables - OpenID Connect"
       "Extract"
       "<ExtractVariables name='##'>
  <VariablePrefix>openidconnect</VariablePrefix>
  <Header name='Authorization'>
    <Pattern>Bearer {access_token}</Pattern>
  </Header>
  <QueryParam name='access_token'>
    <Pattern>{access_token}</Pattern>
  </QueryParam>
  <QueryParam name='token'>
    <Pattern>{access_token}</Pattern>
  </QueryParam>
  <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
</ExtractVariables>\n")

     '("ServiceCallout"
       "ServiceCallout"
       "<ServiceCallout continueOnError='false' async='false' name='##'>
    <DisplayName>##</DisplayName>
    <FaultRules/>
    <Properties/>
      <Request variable='authenticationRequest' clearPayload='false'>
        <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
    </Request>
    <Response>authenticationResponse</Response>
    <HTTPTargetConnection>
        <Properties/>
        <URL>${1:https://api.usergrid.com/globo/sandbox/token}</URL>
    </HTTPTargetConnection>
</ServiceCallout>\n")

     '("OAuthV2 - GenerateAccessToken"
       "OAuthV2"
       "<OAuthV2 name='##'>
    <DisplayName>RefreshAccessToken</DisplayName>
    <FaultRules/>
    <Properties/>
    <Operation>GenerateAccessToken</Operation>
    <ExpiresIn>2400000</ExpiresIn>
    <SupportedGrantTypes>
        <GrantType>authorization_code</GrantType>
        <GrantType>password</GrantType>
        <GrantType>client_credentials</GrantType>
    </SupportedGrantTypes>
    <GenerateResponse/>
</OAuthV2>\n")

     '("OAuthV2 - RefreshAccessToken"
       "OAuthV2"
       "<OAuthV2 name='##' enabled='true' continueOnError='false' async='false' >
    <DisplayName>RefreshAccessToken</DisplayName>
    <FaultRules/>
    <Properties/>
    <Attributes/>
    <ExpiresIn>100000000000</ExpiresIn>
    <ExternalAuthorization>false</ExternalAuthorization>
    <GrantType>request.formparam.grant_type</GrantType>
    <Operation>RefreshAccessToken</Operation>
    <RefreshToken>request.formparam.refresh_token</RefreshToken>
    <GenerateResponse enabled='true'>
        <Format>FORM_PARAM</Format>
    </GenerateResponse>
    <SupportedGrantTypes/>
</OAuthV2>\n")


     '("GetOAuthV2Info"
       "GetOAuthV2Info"
     "<GetOAuthV2Info name='##'>
  <AccessToken ref='openidconnect.access_token'/>
</GetOAuthV2Info>\n")

     '("LookupCache"
       "LookupCache"
       "<LookupCache enabled='true' continueOnError='false' async='false' name='##'>
    <CacheResource>${1:ApigeeCache}</CacheResource>
    <AssignTo>${2:flowvariable}</AssignTo> <!-- name of flow variable -->
    <Scope>${3:$$(yas/choose-value '(\"Exclusive\" \"Global\" \"Application\" \"Proxy\" \"Target\"))}</Scope>
    <CacheKey>
      <KeyFragment ref='${4:flowvariable.name}' />
    </CacheKey>
</LookupCache>")

     '("PopulateCache"
       "PopulateCache"
       "<PopulateCache name='##'>
  <CacheResource>${1:ApigeeCache}</CacheResource>
  <Source>${2:variable.containing.value}</Source>
  <Scope>${3:$$(yas/choose-value '(\"Exclusive\" \"Global\" \"Application\" \"Proxy\" \"Target\"))}</Scope>
  <CacheKey>
    <KeyFragment ref='${4:variable.containing.keyfrag' />
  </CacheKey>
  <ExpirySettings>
    <TimeoutInSec>864000</TimeoutInSec> <!-- 864000 = 10 days -->
  </ExpirySettings>
</PopulateCache>\n")


     '("RaiseFault"
       "RaiseFault"
       "<RaiseFault name='##'>
  <DisplayName>$1</DisplayName>
  <Description>$2</Description>
  <IgnoreUnresolvedVariables>true</IgnoreUnresolvedVariables>
  <FaultResponse>
   <Set>
    <Payload contentType='application/json' variablePrefix='%' variableSuffix='#'><![CDATA[
{
%jsonResponse#
    $0
}
]]></Payload>
     <StatusCode>${3:$$(yas/choose-value '(\"200\" \"400\" \"404\" \"500\" \"503\"))}</StatusCode>
     <ReasonPhrase>OK</ReasonPhrase>
   </Set>
 </FaultResponse>
</RaiseFault>")


     '("RaiseFault - Redirect"
       "RaiseFault"
       "<RaiseFault name='##' enabled='true' continueOnError='false' async='false'>
    <DisplayName>Redirect To...</DisplayName>
    <FaultRules/>
    <Properties/>
    <FaultResponse>
      <Set>
        <Headers>
          <Header name='Location'>${1:http://target.to-redirect.to}</Header>
        </Headers>
        <Payload contentType='text/plain'>
$1
</Payload>
            <StatusCode>302</StatusCode>
            <ReasonPhrase>Redirect</ReasonPhrase>
        </Set>
    </FaultResponse>
    <IgnoreUnresolvedVariables>true</IgnoreUnresolvedVariables>
</RaiseFault>\n")


     '("JavaScript"
       "JavaScript"
       "<Javascript enabled='true' continueOnError='false' async='false' name='##'>
  <FaultRules/>
  <Properties/>
  <ResourceURL>jsc://${1:doWork}.js</ResourceURL>
</Javascript>")

     '("ResponseCache"
     "ResponseCache"
     "<ResponseCache enabled='true' continueOnError='false' async='false' name='##'>
  <DisplayName>$1</DisplayName>
  <FaultRules/>
  <Properties/>
  <CacheKey>
    <Prefix></Prefix>
    <KeyFragment ref='${2:request.uri}' /> <!-- item to use as cache key -->
  </CacheKey>
  <CacheResource>${3:ApigeeCache}</CacheResource>
  <Scope>${4:$$(yas/choose-value '(\"Exclusive\" \"Global\" \"Application\" \"Proxy\" \"Target\"))}</Scope>
  <ExpirySettings>
    <ExpiryDate></ExpiryDate>
    <TimeOfDay></TimeOfDay>
    <TimeoutInSec ref=''>${5:6000}</TimeoutInSec>
  </ExpirySettings>
  <SkipCacheLookup>request.header.x-bypass-cache = 'true'</SkipCacheLookup>
  <SkipCachePopulation>request.header.x-bypass-cache = 'true'</SkipCachePopulation>
</ResponseCache>")

     '("XSL"
       "XSL"
       "<XSL enabled='true' continueOnError='false' async='false' name='##'>
  <DisplayName>XSL - $1</DisplayName>
  <FaultRules/>
  <Properties/>
  <OutputVariable>request.content</OutputVariable>
  <ResourceURL>xsl://$2.xsl</ResourceURL>
  <Source>${3:$$(yas/choose-value '(\"request\" \"response\"))}</Source>
</XSL>\n")


     '("JavaCallout"
       "JavaCallout"
       "<JavaCallout name='##' enabled='true'
             continueOnError='false' async='false'>
  <DisplayName>$1</DisplayName>
  <Properties/>
  <FaultRules/>
  <ClassName>$2</ClassName>
  <ResourceURL>java://$3.jar</ResourceURL>
</JavaCallout>")

))


(defun apigee-get-menu-position ()
  "get the position for the popup menu"
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (point)))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))


(defun apigee--generate-menu (candidates)
  "Generate a menu suitable for use in `x-popup-menu' from the
list of candidates. Each item in the list of candidates is a
list, (KEY TEMPLATE), where KEY is one of {Quota, XMLToJSON,
Javascript, etc}, TEMPLATE is the template to fill in a new policy file.

The output is a list like this:

  (\"Insert...\"
    (\"Ignored pane title\"
      (\"Quota\" \"value to return if thing 1 is selected\")
      (\"Javascript\" \"value if thing 2 is selected\")
      ....))

"
  (let ((items (mapcar '(lambda (elt)
                          (cons
                           (nth 0 elt)
                           elt))
                        candidates)))

    ;; this works with x-popup-menu
    (setq items (cons "Ignored pane title" items))
    (list "Insert..." items)))



(defun apigee-prompt-user-with-choices (candidates)
  "Prompt the user with the available choices.
In this context the list of choices is the list of available Policies.

"
  (cond
   ((not candidates)
    nil)
   ((and (eq apigee-prompt-mechanism 'dropdown-list)
         (featurep 'dropdown-list))
    (let ((choice-n (dropdown-list (mapcar '(lambda (elt) (nth 0 elt)) candidates))))
      (if choice-n
          (nth choice-n candidates)
        (keyboard-quit))))

   (t
    ;; NB:
    ;; x-popup-menu displays in the proper location, near
    ;; the cursor.
    ;;
    ;; x-popup-dialog always displays in the center
    ;; of the frame, which makes for an annoying
    ;; user-experience.
    (x-popup-menu (apigee-get-menu-position)
                  (apigee--generate-menu candidates)))))



;;;###autoload
(defun apigee-add-policy ()
  "Invoke this interactively, and the fn will prompt the user to
choose a policy type to insert. It will then ask for a name for the policy,
create the appropriate XML file, and yas/snippet expand the template for
that policy file.

Bug: Does not check for name clashes by newly added policies.

"
  (interactive)
  (let ((apiproxy-dir (apigee-path-of-apiproxy)))
    (if apiproxy-dir
        (progn
          (if (not (s-ends-with-p "/" apiproxy-dir))
              (setq apiproxy-dir (concat apiproxy-dir "/")))
          (let ((chosen (apigee-prompt-user-with-choices apigee--policy-alist)))
            (when chosen
              (let ((policy-dir (concat apiproxy-dir "apiproxy/policies/"))
                    (ptype (cadr chosen))
                    (have-name nil)
                    (policy-name-prompt "policy name: ")
                    (raw-template (caddr chosen)))

                (let* ((default-value (apigee--default-val-for-policy-name ptype))
                       (policy-name
                        (let (n)
                          (while (not have-name)
                            (setq n (read-string policy-name-prompt default-value nil default-value)
                                  have-name (apigee-policy-name-is-available n)
                                  policy-name-prompt "That name is in use. Policy name: " ))
                          n))

                       (elaborated-template
                        (if (string-match "##" raw-template)
                            (progn
                            (while (string-match "##" raw-template)
                              (setq raw-template
                            (replace-match policy-name t t raw-template)))
                            raw-template)
                          raw-template)))
                  (let* ((filename (concat policy-dir
                                           policy-name
                                           ".xml"))
                         (myBuffer (find-file filename)))
                    (yas/expand-snippet elaborated-template (point) (point)))))))))))



(provide 'apigee)

;;; apigee.el ends here
