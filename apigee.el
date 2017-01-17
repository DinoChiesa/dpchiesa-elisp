;;; apigee.el --- utility functions for working with Apigee Edge platform
;;
;; Copyright (C) 2013-2016 Dino Chiesa and Apigee Corporation
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : May 2013
;; Modified   : April 2016
;; Version    : 1.5
;; Keywords   : apigee edge
;; Requires   : s.el, request.el, dino-netrc.el, xml.el
;; License    : New BSD
;; X-URL      : https://github.com/DinoChiesa/dpchiesa-elisp
;; Last-saved : <2017-January-13 17:44:09>
;;
;;; Commentary:
;;
;; This module defines a few elisp functions that are handy for working
;; with API Proxy definition bundles within emacs. Per ejemplo,
;;  - creating a new blank proxy
;;  - zipping and importing a bundle
;;  - adding a new policy to a bundle
;;  - adding a new target to a bundle
;;  - retrieving / updating a single resource in an existing API Proxy
;;  - validating a bundle (coming soon)
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
;;; Code:
;;

(require 's) ;; magnars' long lost string library
(require 'request)
(require 'dino-netrc)
(require 'xml)

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
         apigee-upload-bundle-args \"-v -d\")

Or you can customize this variable.
"
  :group 'apigee)


(defcustom apigee-upload-bundle-args "-v -d -o ORG -e ENV"
  "The arguments to pass to the script or program that
uploads proxies to the Apigee gateway. Use this in your emacs:

   (require 'apigee)
   (setq apigee-upload-bundle-pgm \"~/dev/apiproxies\"
         apigee-upload-bundle-args \"-v -d\")

Or you can customize this variable.
"
  :group 'apigee)

(defcustom apigee-temp-dir "/tmp"
  "The temporary directory to use for zip bundles. "
  :type 'string
  :group 'apigee)

(defcustom apigee-cached-mgmt-server "https://api.enterprise.apigee.com"
  "The mgmt server to use for API calls. Buffer local."
  :type 'string
  :group 'apigee)

(defvar apigee-cached-org-name nil
  "The Edge organization to use for API calls. Buffer local.")

(defvar apigee-cached-apiproxy nil
  "The Edge apiproxy to use for API calls. Buffer local.")

(defvar apigee-cached-revision nil
  "The Edge apiproxy revision to use for API calls. Buffer local.")

(defvar apigee-cached-rsrc-name nil
  "The resource name to use for API calls. Buffer local.")

(defvar apigee-cached-rsrc-type nil
  "The resource type to use for API calls. Buffer local.")

(defvar apigee--most-recently-used-upload-command nil
  "the most recently used upload command")

;; (defcustom apigee-prompt-mechanism 'x-popup-menu
;;   "The mechanism used to prompt the user for his choice.
;; Options: 'x-popup-menu, or 'dropdown-list.  When setting
;; this, set it to the symbol, not to the string or the actual
;; function.  Eg
;;
;;   (setq apigee-prompt-mechanism 'x-popup-menu)
;;
;; "
;;   :type 'symbol
;;   :options '('x-popup-menu 'dropdown-list)
;;   :group 'apigee)

(defvar apigee-prompt-mechanism 'x-popup-menu
  "The mechanism used to prompt the user for his choice.
Originally this was going to be settable, but x-popup-menu is
the only possible value currently.")


;; The command "template" to use when creating the API bundle zip. In
;; this template, the %f is replaced with the name of the zip file to
;; create. This template probably never needs to be customized.
(defvar apigee-create-bundle-zip-command-template "zip %f -r apiproxy/ -x \"*.*~\" "
  "command template to use when creating an API bundle zip")

;;(setq apigee-create-bundle-zip-command-template "zip %f -r apiproxy/ -x \"*.*~\"")

(defconst apigee-message-payload-template-alist
  (list
   '("application/json" "<![CDATA[{
  \"status\" : true,
  \"message\" : \"whatever\",
  \"clientId\" : \"%parsedRequest.client_id#\"
}
]]>")
   '("application/x-www-form-urlencoded" "status=true&clientId=%parsedRequest.client_id#")
   '("application/xml" "<message><here>%parsedRequest.client_id#</here></message>")))


(defconst apigee--target-template-alist
    (list
     '("HTTPTarget"
       "<TargetEndpoint name='##'>
  <!-- <Description>${1:##}</Description> -->
  <PreFlow name='PreFlow'>
    <Request>
    </Request>
    <Response>
    </Response>
  </PreFlow>
  <PostFlow name='PostFlow'>
    <Request>
    </Request>
    <Response>
    </Response>
  </PostFlow>
  <Flows/>
  <FaultRules/>
  <HTTPTargetConnection>
    <URL>${2:http://example.com/getSearchResults}</URL>
  </HTTPTargetConnection>
</TargetEndpoint>
")
     '("ScriptTarget"
       "<TargetEndpoint name='##'>
  <!-- <Description>${1:##}</Description> -->
  <PreFlow name='PreFlow'>
    <Request>
    </Request>
    <Response>
    </Response>
  </PreFlow>
  <PostFlow name='PostFlow'>
    <Request>
    </Request>
    <Response>
    </Response>
  </PostFlow>
  <Flows/>
  <FaultRules/>
  <ScriptTarget>
      <ResourceURL>node://${2:nodeServer.js}</ResourceURL>
  </ScriptTarget>
</TargetEndpoint>
")
))


(defconst apigee-http-status-message-alist
  (list
   '("200" "OK")
   '("201" "Created")
   '("302" "Moved")
   '("304" "Not Modified")
   '("400" "Bad Request")
   '("401" "Not Authorized")
   '("403" "Forbidden")
   '("404" "Not Found")
   '("410" "Gone")
   '("429" "Too Many Requests")
   '("500" "Server Error")
   '("501" "Not Implemented")
   '("503" "Server Busy")))

(defconst apigee-entity-to-entity-id-types-alist
  (list
   '("apiproduct" ("apiproductname" "appname" "appid" "consumerkey"))
   '("app" ("appname" "appid" "consumerkey"))
   '("authorizationcode" ("authorizationcode"))
   '("company" ("companyname" "appid" "consumerkey"))
   '("companydeveloper" ("companyname"))
   '("consumerkey" ("consumerkey"))
   '("consumerkey_scope" ("consumerkey"))
   '("requesttoken" ("requesttoken"))
   '("verifier" ("verifier"))
   '("developer" ("developeremail" "developerid" "appid" "consumerkey"))))

(defconst apigee-common-variable-list
  '("environment.name"
    "requestMessageUri"
    "request.verb"
    "response.status.code"
    "api_key"
    "system.time.second"
    "error.state"
    "error.content"
    "error.state"
    "responsecache.responseCache.cachehit"
    "client.received.start.timestamp"
    "system.timestamp"
    "target.sent.start.timestamp"
    "target.received.end.timestamp"
    "target.copy.pathsuffix"
    "target.url"
    "request.header.accept"
    "request.header.x-apikey"
    "request.header.apikey"
    "request.header.X-Forwarded-For"
    "target.received.content.length")
  "A list of common variables, can be used to prompt expansion in yas snippets")

(defconst apigee--policy-alist
    (list
     ;; the "created at" comment in the following templates works around a bug in
     ;; yas--eval-lisp, which causes a weird error in the first field expansion, for
     ;; large snippets, when yas-choose-value us the function. i think.  adding this
     ;; extra "dummy" field avoids the problem.
     '("AccessEntity"
     "AE"
     "<AccessEntity name='##'>
    <!-- created at ${1:$$(format-time-string \"%Y-%m-%dT%T%z\" (current-time) t)} -->
  <EntityType value='${2:$$(yas-choose-value '(\"apiproduct\" \"app\" \"company\" \"companydeveloper\" \"consumerkey\" \"developer\"))}' />
  <EntityIdentifier type='${3:$$(yas-choose-value (let ((field1 (apigee--snippet-field 1))) (apigee-entity-id-types (buffer-substring-no-properties (yas--field-start field1) (yas--field-end field1)))))}' ref='${4:varName}' />
  <!-- SecondaryIdentifier is not always required -->
  <SecondaryIdentifier type='$5' ref='$6' />
<!--
  The result is stored in a variable:  AccessEntity.##
  Next step is to use ExtractVariables to get a value from that entity.
-->
</AccessEntity>\n")

     '("AccessEntity - App"
     "AE"
     "<AccessEntity name='##'>
  <EntityType value='app' />
  <EntityIdentifier type='consumerkey' ref='${1:client_id}' />
</AccessEntity>\n")

     '("AccessEntity - Developer"
     "AE"
     "<AccessEntity name='##'>
  <EntityType value='developer' />
  <EntityIdentifier type='consumerkey' ref='${1:client_id}' />
</AccessEntity>\n")

     '("AssignMessage - remove query param or header"
       "AM"
       "<AssignMessage name='##'>
  <DisplayName>##</DisplayName>
  <Remove>
    <QueryParams>
      <QueryParam name='${1:apikey}'/>
    </QueryParams>
    <Headers>
      <Header name='${2:apikey}'/>
    </Headers>
  </Remove>
  <IgnoreUnresolvedVariables>${3:$$(yas-choose-value '(\"true\" \"false\" ))}</IgnoreUnresolvedVariables>
  <AssignTo createNew='false' transport='http' type='${4:$$(yas-choose-value '(\"request\" \"response\" ))}'></AssignTo>
</AssignMessage>\n")

     '("AssignMessage - clean response headers"
       "AM"
       "<AssignMessage name='##'>
  <Remove>
    <Headers>
      <Header name='Accept'/>
      <Header name='user-agent'/>
      <Header name='X-Powered-By'/>
      <Header name='X-Forwarded-For'/>
      <Header name='X-Forwarded-Port'/>
      <Header name='X-Forwarded-Proto'/>
    </Headers>
  </Remove>
  <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
  <AssignTo createNew='false' transport='http' type='response'></AssignTo>
</AssignMessage>\n")

     '("AssignMessage - set query param and/or headers"
       "AM"
       "<AssignMessage name='##'>
  <AssignTo createNew='false' type='${1:$$(yas-choose-value '(\"request\" \"response\"))}'/>
  <Set>
    <QueryParams>
      <QueryParam name='${2:outgoingParamName}'>{request.queryparam.url}</QueryParam>
      <QueryParam name='apiKey'>Something</QueryParam>
    </QueryParams>
    <Headers>
      <Header name='Content-Type'>application/json</Header>
    </Headers>
    <Verb>GET</Verb>
  </Set>
  <!-- Set other flow variables for use in the final response -->
  <AssignVariable>
    <Name>urlshortener.longUrl</Name>
    <Ref>request.queryparam.url</Ref>
    <Value>this-value-is-used-when-the-Ref-is-unresolvable</Value>
  </AssignVariable>
</AssignMessage>\n")

     '("AssignMessage - assign variable"
     "AV"
     "<AssignMessage name='##'>
  <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
  <AssignVariable>
    <Name>parsedRequest.client_id</Name>
    <Ref>request.queryparam.client_id</Ref>
    <Value>BADDBEEF</Value>
  </AssignVariable>
</AssignMessage>\n")

     '("AssignMessage - Store Original header"
     "AM"
     "<AssignMessage name='##'>
  <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
  <AssignVariable>
    <Name>${1:originalRequestHeaders}_${2:$$(yas-choose-value '(\"Content-Type\" \"Accept\"))}</Name>
    <Ref>request.header.$2</Ref>
  </AssignVariable>
</AssignMessage>\n")

     '("AssignMessage - Set Content-Type"
       "AssignMessage"
       "<AssignMessage name='##'>
  <AssignTo createNew='false' type='response'/>
  <!-- force content-type. This allows subsequent XMLToJSON or ExtractVariables to work. -->
  <Set>
    <Headers>
      <Header name='Content-Type'>application/json</Header>
    </Headers>
  </Set>
</AssignMessage>\n")

     '("AssignMessage - full response"
       "AM"
     "<AssignMessage name='##'>
  <DisplayName>##</DisplayName>
  <Description>$1</Description>
  <!-- <AssignTo createNew='false' transport='http' type='request'></AssignTo> -->
  <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
  <Set>
    <Payload contentType='${2:$$(yas-choose-value (reverse (apigee--sort-strings (mapcar 'car apigee-message-payload-template-alist))))}'
             variablePrefix='%' variableSuffix='#'>${2:$(cadr (assoc yas-text apigee-message-payload-template-alist))}</Payload>
    <StatusCode>${3:$$(yas-choose-value (reverse (apigee--sort-strings (mapcar 'car apigee-http-status-message-alist))))}</StatusCode>
    <ReasonPhrase>${3:$(cadr (assoc yas-text apigee-http-status-message-alist))}</ReasonPhrase>
  </Set>

  <!-- Set this flow variable to indicate the response has been set -->
  <AssignVariable>
    <Name>customResponse.assigned</Name>
    <Value>true</Value>
  </AssignVariable>

</AssignMessage>\n")

     '("BasicAuthentication - Decode Inbound"
       "BasicAuth"
     "<BasicAuthentication name='##'>
   <DisplayName>Decode Basic Authentication Header</DisplayName>
   <Operation>Decode</Operation>
   <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
   <User ref='inbound.username' />
   <Password ref='inbound.password' />
   <Source>request.header.Authorization</Source>
</BasicAuthentication>\n")

     '("BasicAuthentication - Encode Outbound"
       "BasicAuth"
     "<BasicAuthentication name='##'>
   <DisplayName>Encode Basic Authentication Header</DisplayName>
   <Operation>Encode</Operation>
   <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
   <User ref='credentials.username'/>
   <Password ref='credentials.password'/>
   <AssignTo createNew='false'>request.header.Authorization</AssignTo>
</BasicAuthentication>\n")

     '("KVM - Put"
       "KVM-PUT"
       "<KeyValueMapOperations name='##' mapIdentifier='${1:nameOfMap}'>
  <Scope>${2:$$(yas-choose-value '(\"organization\" \"environment\" \"apiproxy\"))}</Scope>
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
       "<KeyValueMapOperations name='##' mapIdentifier='${1:nameOfMap}'>
  <Scope>${2:$$(yas-choose-value '(\"organization\" \"environment\" \"apiproxy\"))}</Scope>
  <Get assignTo='${3:variable.to.set}' index='2'>
    <Key>
      <Parameter ref='${4:variable.containing.key}'/>
    </Key>
  </Get>
</KeyValueMapOperations>\n")

;    <FaultRules/>
;    <Properties/>

     '("SpikeArrest"
       "SA"
"<SpikeArrest name='##'>
    <DisplayName>##</DisplayName>
    <!-- Identifier: optional -->
    <Identifier ref='request.header.some-header-name'/>
    <!-- MessageWeight: optional -->
    <MessageWeight ref='request.header.weight'/>
    <Rate>30ps</Rate>
</SpikeArrest>")


     '("Quota - Enforce after VerifyAPIKey"
       "Quota"
       "<Quota name='##'>
    <DisplayName>##</DisplayName>
    <Identifier ref='${1:request.queryparam.apikey}' />
    <!-- the count specified is used unless overridden by the variable referenced here -->
    <Allow countRef='verifyapikey.${2:VerifyAPIKey-1}.apiproduct.developer.quota.limit' count='1000'/>
    <!-- use the interval in the variable; if not present use the value specified here. -->
    <Interval ref='verifyapikey.$2.apiproduct.developer.quota.interval'>1</Interval>
    <!-- use the timeunit provided in the variable; if not present use the value specified here. -->
    <TimeUnit ref='verifyapikey.$2.apiproduct.developer.quota.timeunit'>${3:$$(yas-choose-value '(\"second\" \"minute\" \"hour\" \"day\" \"month\"))}</TimeUnit>
    <Distributed>true</Distributed>
    <Synchronous>false</Synchronous>
    <PreciseAtSecondsLevel>false</PreciseAtSecondsLevel>
<!--
variables set by this policy include:

ratelimit.{policy_name}.allowed.count
ratelimit.{policy_name}.used.count
ratelimit.{policy_name}.available.count
ratelimit.{policy_name}.exceed.count
ratelimit.{policy_name}.total.exceed.count
ratelimit.{policy_name}.expiry.time
ratelimit.{policy_name}.identifier
ratelimit.{policy_name}.class
ratelimit.{policy_name}.class.allowed.count
ratelimit.{policy_name}.class.used.count
ratelimit.{policy_name}.class.available.count
ratelimit.{policy_name}.class.exceed.count
ratelimit.{policy_name}.class.total.exceed.count
-->
</Quota>")


     '("Quota - Enforce after OAuth2 VerifyAccessToken"
       "Quota"
       "<Quota name='##'>
    <DisplayName>##</DisplayName>
    <!-- use the client_id that is set after OAuthV2/VerifyAPIKey -->
    <Identifier ref='client_id' />
    <!-- the count specified is used unless overridden by the variable referenced here -->
    <Allow countRef='apiproduct.developer.quota.limit' count='1000'/>
    <!-- use the interval in the variable; if not present use the value specified here. -->
    <Interval ref='apiproduct.developer.quota.interval'>1</Interval>
    <!-- use the timeunit provided in the variable; if not present use the value specified here. -->
    <TimeUnit ref='apiproduct.developer.quota.timeunit'>${2:$$(yas-choose-value '(\"second\" \"minute\" \"hour\" \"day\" \"month\"))}</TimeUnit>
    <Distributed>true</Distributed>
    <Synchronous>false</Synchronous>
    <PreciseAtSecondsLevel>false</PreciseAtSecondsLevel>
<!--
variables set by this policy include:

ratelimit.{policy_name}.allowed.count
ratelimit.{policy_name}.used.count
ratelimit.{policy_name}.available.count
ratelimit.{policy_name}.exceed.count
ratelimit.{policy_name}.total.exceed.count
ratelimit.{policy_name}.expiry.time
ratelimit.{policy_name}.identifier
ratelimit.{policy_name}.class
ratelimit.{policy_name}.class.allowed.count
ratelimit.{policy_name}.class.used.count
ratelimit.{policy_name}.class.available.count
ratelimit.{policy_name}.class.exceed.count
ratelimit.{policy_name}.class.total.exceed.count
-->
</Quota>")


     '("Quota - Enforce on Product"
       "Quota"
     "<Quota name='##'>
  <Interval ref='apiproduct.developer.quota.interval'/>
  <TimeUnit ref='apiproduct.developer.quota.timeunit'/>
  <Allow countRef='apiproduct.developer.quota.limit'/>
  <Identifier ref='client_id'/>
  <Distributed>true</Distributed>
  <Synchronous>false</Synchronous>
  <PreciseAtSecondsLevel>false</PreciseAtSecondsLevel>
<!--
variables set by this policy include:

ratelimit.{policy_name}.allowed.count
ratelimit.{policy_name}.used.count
ratelimit.{policy_name}.available.count
ratelimit.{policy_name}.exceed.count
ratelimit.{policy_name}.total.exceed.count
ratelimit.{policy_name}.expiry.time
ratelimit.{policy_name}.identifier
ratelimit.{policy_name}.class
ratelimit.{policy_name}.class.allowed.count
ratelimit.{policy_name}.class.used.count
ratelimit.{policy_name}.class.available.count
ratelimit.{policy_name}.class.exceed.count
ratelimit.{policy_name}.class.total.exceed.count
-->
</Quota>\n")

          '("Quota - Reset"
       "Quota-Reset"
       "<ResetQuota name='##'>
  <!-- name of the Quota policy being reset -->
  <Quota name='request.header.quotapolicy'>
    <Identifier name='_default'>
      <!-- use one of the following  -->
      <Allow>100</Allow>
      <Allow ref='flow.variable.containing.number.to.allow' />
    </Identifier>
  </Quota>
</ResetQuota>\n")

     ;; Wednesday, 17 June 2015
     ;; the "created at" comment works around a bug in yas--eval-lisp, which
     ;; causes a weird error in the first field expansion, for large snippets,
     ;; when yas-choose-value us the function. i think.  adding this extra
     ;; "dummy" field avoids the problem.

     '("VerifyAPIKey" ;;  - query param or header
       "VerifyAPIKey"
     "<VerifyAPIKey name='##'>
    <!-- created at ${1:$$(format-time-string \"%Y-%m-%dT%T%z\" (current-time) t)} -->
    <DisplayName>Verify API Key</DisplayName>
    <APIKey ref='${2:$$(yas-choose-value '(\"request.queryparam.apikey\" \"request.header.X-Apikey\"))}'></APIKey>
<!--
Variables populated by this policy: verifyapikey.{policy_name}.

client_id: The consumer key (aka API key or app key) supplied by the requesting app
client_secret: The consumer secret associated with the consumer key
redirection_uris: Any redirect URIs in the request
developer.app.name: The app name of the developer app making the request
developer.id: The developer ID of the developer registered as the owner of the requesting app
failed: Set when API Key validation fails
developer.app.{custom_attribute_name} ?? - custom attribute on the app
{custom_attribute_name_of_app}: Any custom attribute derived from the app profile?
{custom_attribute_name_of_appkey}: Any custom attributes derived from the app key profile
apiproduct.name*: The name of the API product used to validate the request
apiproduct.{custom_attribute_name}*: Any custom attribute derived from the API product profile
apiproduct.developer.quota.limit*
apiproduct.developer.quota.interval*
apiproduct.developer.quota.timeunit*

-->
</VerifyAPIKey>\n")

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
  <Source>${1:$$(yas-choose-value '(\"request\" \"response\" ))}</Source>
  <OutputVariable>${2:$$(yas-choose-value '(\"request\" \"response\" ))}</OutputVariable>
  <Options/>
</JSONToXML>")

     '("JSONThreatProtection"
       "JSONThreatProtection"
       "<JSONThreatProtection name='##'>
   <ArrayElementCount>20</ArrayElementCount>
   <ContainerDepth>10</ContainerDepth>
   <ObjectEntryCount>15</ObjectEntryCount>
   <ObjectEntryNameLength>50</ObjectEntryNameLength>
   <Source>request</Source>
   <StringValueLength>500</StringValueLength>
</JSONThreatProtection>")

     '("XMLToJSON - full options"
       "XMLToJSON"
       "<XMLToJSON name='##'>
  <Source>${1:$$(yas-choose-value '(\"request\" \"response\" ))}</Source>
  <OutputVariable>${1:$(yas-text)}.content</OutputVariable>
  <Format>yahoo</Format>
  <!--
  <Options>
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
  </Options>
  -->
</XMLToJSON>\n")

     '("XMLToJSON - strip levels"
       "XMLToJSON"
       "<XMLToJSON name='##'>
  <Source>${1:$$(yas-choose-value '(\"request\" \"response\" ))}</Source>
  <OutputVariable>${1:$(yas-text)}.content</OutputVariable>
  <Options>
    <StripLevels>3</StripLevels>
  </Options>
</XMLToJSON>\n")

     '("XMLToJSON - always treat as array"
       "XMLToJSON"
       "<XMLToJSON name='##'>
  <Source>${1:$$(yas-choose-value '(\"request\" \"response\" ))}</Source>
  <OutputVariable>${1:$(yas-text)}.content</OutputVariable>
  <!--
  <Options>
    <TreatAsArray>
      <Path unwrap='true'>${1:school/teachers}</Path>
      <Path unwrap='false'>${2:school/teachers/students}</Path>
    </TreatAsArray>
  </Options>
  -->
</XMLToJSON>\n")

     '("ExtractVariables - from AccessEntity"
       "Extract"
       "<ExtractVariables name='##'>
  <Source>AccessEntity.AccessEntity-${1:1}</Source>
  <VariablePrefix>entity</VariablePrefix>
  <XMLPayload>
    <Variable name='${2:varname1}' type='string'>
      <XPath>/Developer/Attributes/Attribute[Name='$2']/Value/text()</XPath>
    </Variable>
    <Variable name='${3:varname2}' type='string'>
      <XPath>/Developer/Attributes/Attribute[Name='$3']/Value/text()</XPath>
    </Variable>
  </XMLPayload>
</ExtractVariables>")

     '("ExtractVariables - errorMessage after fault"
       "Extract"
     "<ExtractVariables name='##'>
    <Source>error</Source>
    <JSONPayload>
        <Variable name='errorMessage' type='string'>
            <JSONPath>$.fault.faultstring</JSONPath>
        </Variable>
    </JSONPayload>
</ExtractVariables>")

     '("ExtractVariables - URIPath"
       "Extract"
     "<ExtractVariables name='##'>
   <DisplayName>Extract a portion of the url path</DisplayName>
   <Source>request</Source>
   <URIPath>
      <Pattern ignoreCase='true'>/accounts/{id}</Pattern>
   </URIPath>
   <VariablePrefix>extracted</VariablePrefix>
   <IgnoreUnresolvedVariables>true</IgnoreUnresolvedVariables>
</ExtractVariables>")

     '("ExtractVariables - XML"
       "Extract"
       "<ExtractVariables name='##'>
  <Source>$1</Source>
  <!-- <VariablePrefix>entity</VariablePrefix> -->
  <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
  <XMLPayload>
    <Namespaces>
      <Namespace prefix='apigee'>http://www.apigee.com</Namespace>
    </Namespaces>
    <Variable name='${2:varname1}' type='string'>
      <XPath>/apigee:$2/text()</XPath>
    </Variable>
    <Variable name='${3:varname2}' type='string'>
      <XPath>/Developer/Attributes/Attribute[Name='$3']/Value/text()</XPath>
    </Variable>
    <Variable name='${4:varname3}' type='string'>
      <XPath>/Developer/Attributes/Attribute[Name='$4']/Value/text()</XPath>
    </Variable>
  </XMLPayload>
</ExtractVariables>")

     '("ExtractVariables - SOAP"
       "Extract"
       "<ExtractVariables name='##'>
  <Source>{$1:request}</Source>
  <!-- <VariablePrefix>soap</VariablePrefix> -->
  <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
  <XMLPayload>
    <Namespaces>
      <Namespace prefix='soap'>http://schemas.xmlsoap.org/soap/envelope/</Namespace>
    </Namespaces>
    <Variable name='topLevelRequestElement' type='string'>
      <XPath>local-name(/soap:Envelope/soap:Body/*[1])</XPath>
    </Variable>
  </XMLPayload>
</ExtractVariables>")

     '("ExtractVariables - from JSON response"
       "Extract"
       "<ExtractVariables name='##'>
  <Source>tokenResponse</Source>
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
  <!-- <VariablePrefix>openidconnect</VariablePrefix> -->
  <Header name='Authorization'>
    <Pattern>Bearer {oidc_access_token}</Pattern>
  </Header>
  <QueryParam name='access_token'>
    <Pattern>{oidc_access_token}</Pattern>
  </QueryParam>
  <QueryParam name='token'>
    <Pattern>{oidc_access_token}</Pattern>
  </QueryParam>
  <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
</ExtractVariables>\n")

          '("ExtractVariables - from header"
       "Extract"
       "<ExtractVariables name='##'>
  <Source>request</Source>
  <VariablePrefix>extracted</VariablePrefix>
  <Header name='Authorization'>
    <Pattern>Bearer {access_token}</Pattern>
  </Header>
  <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
</ExtractVariables>\n")

          '("ExtractVariables - from queryparam"
       "Extract"
       "<ExtractVariables name='##'>
  <Source>request</Source>
  <VariablePrefix>extracted</VariablePrefix>
  <QueryParam name='code'>
      <Pattern ignoreCase='true'>DBN{dbncode}</Pattern>
   </QueryParam>
  <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
</ExtractVariables>\n")

     '("ServiceCallout - json payload"
       "SC"
       "<ServiceCallout name='##'>
  <DisplayName>##</DisplayName>
  <Request variable='authenticationRequest'>
        <Set>
          <!-- Shows how to request a token from usergrid. -->
          <!-- FIXME: should retrieve secrets from vault -->
           <Payload contentType='application/json'>{
    \"grant_type\":\"client_credentials\",
    \"client_id\":\"whatever\",
    \"client_secret\":\"something-secret\"
}</Payload>
         <Verb>POST</Verb>
         <Path>/demo24/wagov1/token</Path>
      </Set>
  </Request>
  <Response>tokenResponse</Response>
  <HTTPTargetConnection>
    <Properties>
      <Property name='success.codes'>2xx, 4xx, 5xx</Property>
    </Properties>
    <URL>${1:https://api.usergrid.com/}</URL>
  </HTTPTargetConnection>
</ServiceCallout>\n")

     '("ServiceCallout - usergrid user auth"
       "SC"
       "<ServiceCallout name='##'>
  <DisplayName>##</DisplayName>
  <Request variable='authenticationRequest'>
        <Set>
          <!-- Shows how to request a token from usergrid. -->
          <!-- FIXME: should retrieve secrets from vault -->
           <Payload contentType='application/json'>
  { \"grant_type\":\"password\", \"username\":\"{authn.uid}\", \"password\":\"{authn.pwd}\" }
</Payload>
         <Verb>POST</Verb>
         <Path>/${1:BAAS_ORG}/${2:BAAS_APP}/token</Path>
      </Set>
  </Request>
  <Response>tokenResponse</Response>
  <HTTPTargetConnection>
    <Properties>
      <Property name='success.codes'>2xx, 4xx</Property>
    </Properties>
    <URL>${3:https://api.usergrid.com/}</URL>
  </HTTPTargetConnection>
</ServiceCallout>\n")

          '("ServiceCallout - form payload"
       "SC"
       "<ServiceCallout name='##'>
  <Request>
    <Set>
     <Headers>
       <Header name='content-type'>application/x-www-form-urlencoded</Header>
     </Headers>
     <FormParams>
       <FormParam name='code'>{request.queryparam.code}</FormParam>
       <FormParam name='client_id'>{goog_client_id}</FormParam>
       <FormParam name='client_secret'>{goog_client_secret}</FormParam>
       <FormParam name='redirect_uri'>{goog_redirect_uri}</FormParam>
       <FormParam name='grant_type'>authorization_code</FormParam>
     </FormParams>
     <Verb>POST</Verb>
    </Set>
  </Request>
  <Response>tokenResponse</Response>
  <HTTPTargetConnection>
    <Properties>
      <Property name='success.codes'>2xx, 3xx</Property>
    </Properties>
    <URL>https://www.googleapis.com/oauth2/v3/token</URL>
  </HTTPTargetConnection>
</ServiceCallout>\n")


     ;; Wednesday, 17 June 2015
     ;; the "created at" comment in the below works around a bug in yas--eval-lisp,
     ;; which causes a weird error in the first field expansion, for large snippets,
     ;; when yas-choose-value us the function. i think.  adding this extra "dummy" field
     ;; avoids the problem.
          '("OAuthV2 - GenerateAccessToken auth_code, client creds"
       "OAuthV2-GenerateAccessToken"
       "<OAuthV2 name='##'>
    <!-- created at ${1:$$(format-time-string \"%Y-%m-%dT%T%z\" (current-time) t)} -->
    <Operation>GenerateAccessToken</Operation>
    <!--
    ExpiresIn, in milliseconds. The ref is optional. The explicitly specified
    value is the default, when the variable reference cannot be resolved.
      2400000 = 40 minutes
      3600000 = 60 minutes
    -->
    <ExpiresIn ref='flow.variable'>2400000</ExpiresIn>

    <!--
    RefreshTokenExpiresIn, in milliseconds. Optional; if it is not
    specified, the default value will be used which is -1 (no expiration).
      691200000 = 8 days
      2592000000 = 30 days
    -->
    <RefreshTokenExpiresIn>691200000</RefreshTokenExpiresIn>

    <SupportedGrantTypes>
        <!-- keep one or more of the following -->
        <GrantType>authorization_code</GrantType>
        <GrantType>password</GrantType>
        <GrantType>client_credentials</GrantType>
    </SupportedGrantTypes>

    <!-- variable that specifies the requested grant type -->
    <GrantType>${2:$$(yas-choose-value '(\"request.queryparam.grant_type\" \"request.formparam.grant_type\" \"flowVariable.something\" ))}</GrantType>

    <!--
    ExternalAuthorization is used to support external authorization. It is
    optional; if not present, the implied value is false. If it is present and
    true:
        - this policy looks for a flow variable with the fixed name
          'oauth_external_authorization_status', which indicates the
          external authorization status.

        - if 'oauth_external_authorization_status' is true, the policy does
          not explicitly validate the client_id and client_secret.
          Still, the client_id is expected to be present in the request.

        - if 'oauth_external_authorization_status' is false, thi signals
          that external authorization has failed and the policy throws
          an appropriate fault.

    If ExternalAuthorization is set to false or if the element is not present, then
    the policy validates the client_id and secret against the internal key store.
    -->

    <ExternalAuthorization>${3:$$(yas-choose-value '(\"true\" \"false\" ))}</ExternalAuthorization>

    <!--
    Optional: these attributes get associated to the token.
    They will be available to the api proxy whenever the token is
    subsequently validated.
    -->
    <Attributes>
      <Attribute name='attr_name1' ref='flow.variable1' display='true|false'>value1</Attribute>
      <Attribute name='attr_name2' ref='flow.variable2' display='true|false'>value2</Attribute>
    </Attributes>

    <GenerateResponse enabled='true'/>
    <!--

    If you include GenerateResponse and have enabled='true', then
    the response is sent directly to the caller. The payload looks like
    this:

    {
     \"issued_at\": \"1420262924658\",
     \"scope\": \"READ\",
     \"application_name\": \"ce1e94a2-9c3e-42fa-a2c6-1ee01815476b\",
     \"refresh_token_issued_at\": \"1420262924658\",
     \"status\": \"approved\",
     \"refresh_token_status\": \"approved\",
     \"api_product_list\": \"[PremiumWeatherAPI]\",
     \"expires_in\": \"1799\",
     \"developer.email\": \"tesla@weathersample.com\",
     \"organization_id\": \"0\",
     \"token_type\": \"BearerToken\",
     \"refresh_token\": \"fYACGW7OCPtCNDEnRSnqFlEgogboFPMm\",
     \"client_id\": \"5jUAdGv9pBouF0wOH5keAVI35GBtx3dT\",
     \"access_token\": \"2l4IQtZXbn5WBJdL6EF7uenOWRsi\",
     \"organization_name\": \"docs\",
     \"refresh_token_expires_in\": \"0\",
     \"refresh_count\": \"0\"
    }

    If you omit GenerateResponse or have enabled='false', then
    these flow variables are set on success:

      oauthv2accesstoken.##.access_token
      oauthv2accesstoken.##.token_type
      oauthv2accesstoken.##.expires_in
      oauthv2accesstoken.##.refresh_token
      oauthv2accesstoken.##.refresh_token_expires_in
      oauthv2accesstoken.##.refresh_token_issued_at
      oauthv2accesstoken.##.refresh_token_status
    -->

</OAuthV2>\n")

          '("OAuthV2 - GenerateAccessToken password"
            "OAuthV2-GenerateAccessToken"
            "<OAuthV2 name='##'>
    <!-- created at ${1:$$(format-time-string \"%Y-%m-%dT%T%z\" (current-time) t)} -->
    <Operation>GenerateAccessToken</Operation>
    <!--
    ExpiresIn, in milliseconds. The ref is optional. The explicitly specified
    value is the default, when the variable reference cannot be resolved.
      2400000 = 40 minutes
      3600000 = 60 minutes
    -->
    <ExpiresIn ref='flow.variable'>2400000</ExpiresIn>

    <!--
    RefreshTokenExpiresIn, in milliseconds. Optional; if it is not
    specified, the default value will be used which is -1 (no expiration).
      691200000 = 8 days
      2592000000 = 30 days
    -->
    <RefreshTokenExpiresIn>691200000</RefreshTokenExpiresIn>

    <SupportedGrantTypes>
        <GrantType>password</GrantType>
    </SupportedGrantTypes>

    <!-- variable that specifies the requested grant type -->
    <GrantType>${2:$$(yas-choose-value '(\"request.queryparam.grant_type\" \"request.formparam.grant_type\" \"flowVariable.something\" ))}</GrantType>

  <UserName>login</UserName>
  <PassWord>password</PassWord>

    <!--
    ExternalAuthorization is used to support external authorization. It is
    optional; if not present, the implied value is false. If it is present and
    true:
        - this policy looks for a flow variable with the fixed name
          'oauth_external_authorization_status', which indicates the
          external authorization status.

        - if 'oauth_external_authorization_status' is true, the policy does
          not explicitly validate the client_id and client_secret.
          Still, the client_id is expected to be present in the request.

        - if 'oauth_external_authorization_status' is false, thi signals
          that external authorization has failed and the policy throws
          an appropriate fault.

    If ExternalAuthorization is set to false or if the element is not present, then
    the policy validates the client_id and secret against the internal key store.
    -->

    <ExternalAuthorization>${3:$$(yas-choose-value '(\"true\" \"false\" ))}</ExternalAuthorization>

    <!--
    Optional: these attributes get associated to the token.
    They will be available to the api proxy whenever the token is
    subsequently validated.
    -->
    <Attributes>
      <Attribute name='attr_name1' ref='flow.variable1' display='true|false'>value1</Attribute>
      <Attribute name='attr_name2' ref='flow.variable2' display='true|false'>value2</Attribute>
    </Attributes>

    <GenerateResponse enabled='true'/>
    <!--

    If you include GenerateResponse and have enabled='true', then
    the response is sent directly to the caller. The payload looks like
    this:

    {
     \"issued_at\": \"1420262924658\",
     \"scope\": \"READ\",
     \"application_name\": \"ce1e94a2-9c3e-42fa-a2c6-1ee01815476b\",
     \"refresh_token_issued_at\": \"1420262924658\",
     \"status\": \"approved\",
     \"refresh_token_status\": \"approved\",
     \"api_product_list\": \"[PremiumWeatherAPI]\",
     \"expires_in\": \"1799\",
     \"developer.email\": \"tesla@weathersample.com\",
     \"organization_id\": \"0\",
     \"token_type\": \"BearerToken\",
     \"refresh_token\": \"fYACGW7OCPtCNDEnRSnqFlEgogboFPMm\",
     \"client_id\": \"5jUAdGv9pBouF0wOH5keAVI35GBtx3dT\",
     \"access_token\": \"2l4IQtZXbn5WBJdL6EF7uenOWRsi\",
     \"organization_name\": \"docs\",
     \"refresh_token_expires_in\": \"0\",
     \"refresh_count\": \"0\"
    }

    If you omit GenerateResponse or have enabled='false', then
    these flow variables are set on success:

      oauthv2accesstoken.##.access_token
      oauthv2accesstoken.##.token_type
      oauthv2accesstoken.##.expires_in
      oauthv2accesstoken.##.refresh_token
      oauthv2accesstoken.##.refresh_token_expires_in
      oauthv2accesstoken.##.refresh_token_issued_at
      oauthv2accesstoken.##.refresh_token_status
    -->

</OAuthV2>\n")

     '("OAuthV2 - GenerateAccessToken - Implicit Grant"
       "OAuthV2-GenerateAccessToken"
       "<OAuthV2 name='##'>
    <DisplayName>OAuthV2 - GenerateAccessTokenImplicitGrant</DisplayName>
    <Operation>GenerateAccessTokenImplicitGrant</Operation>
    <!--
    ExpiresIn, in milliseconds. The ref is optional. The explicitly specified
    value is the default, when the variable reference cannot be resolved.
      2400000 = 40 minutes
      3600000 = 60 minutes
    -->
    <ExpiresIn ref='flow.variable'>2400000</ExpiresIn>

    <!--
    RefreshTokenExpiresIn, in milliseconds. Optional; if it is not
    specified, the default value will be used which is -1 (no expiration).
      691200000 = 8 days
      2592000000 = 30 days
    -->
    <RefreshTokenExpiresIn>691200000</RefreshTokenExpiresIn>

    <ResponseType>flow.variable</ResponseType> <!-- Optional -->
    <ClientId>flow.variable</ClientId> <!-- Optional -->
    <RedirectUri>flow.variable</RedirectUri> <!-- Optional -->
    <Scope>flow.variable</Scope> <!-- Optional, space-separated list -->
    <State>flow.variable</State> <!-- Optional -->
    <AppEndUser>flow.variable</AppEndUser> <!-- Optional -->

    <!--
    Optional: these attributes get associated to the token.
    They will be available to the api proxy whenever the token is
    subsequently validated.
    -->
    <Attributes>
      <Attribute name='attr_name1' ref='flow.variable1' display='true|false'>value1</Attribute>
      <Attribute name='attr_name2' ref='flow.variable2' display='true|false'>value2</Attribute>
    </Attributes>

    <!--
    ExternalAuthorization is used to support external authorization. It is
    optional; if not present, the implied value is false. If it is present and
    true:
        - this policy looks for a flow variable with the fixed name
          'oauth_external_authorization_status', which indicates the
          external authorization status.

        - if 'oauth_external_authorization_status' is true, the policy does not
          explicitly validate the client_id and client_secret.
          Still, the client_id is expected to be present in the request.

        - if 'oauth_external_authorization_status' is false,
          this signals that external authorization has failed and the policy throws
          an appropriate fault.

    If ExternalAuthorization is set to false or if the element is not present, then
    the policy validates the client_id and secret against the internal key store.
    -->

    <ExternalAuthorization>${1:$$(yas-choose-value '(\"true\" \"false\" ))}</ExternalAuthorization>

    <!--
      If <GenerateResponse/> is omitted, the policy sets these variables on success:
        oauthv2accesstoken.##.access_token
        oauthv2accesstoken.##.token_type
        oauthv2accesstoken.##.expires_in
        oauthv2accesstoken.##.refresh_token
    -->
    <GenerateResponse enabled='true'/>
</OAuthV2>\n")

     '("OAuthV2 - GenerateAuthorizationCode"
       "OAuthV2-GenerateAuthorizationCode"
       "<OAuthV2 name='##'>
    <Operation>GenerateAuthorizationCode</Operation>
    <!--
    ExpiresIn, in milliseconds. The ref is optional. The explicitly specified
    value is the default, when the variable reference cannot be resolved.
      2400000 = 40 minutes
      3600000 = 60 minutes
    -->
    <ExpiresIn ref='flow.variable'>2400000</ExpiresIn>

    <!-- The following are Optional -->
    <ResponseType>flow.variable</ResponseType>
    <ClientId>flow.variable</ClientId>
    <RedirectUri>flow.variable</RedirectUri>
    <Code>flow.variable</Code>
    <Scope>flow.variable</Scope>
    <State>flow.variable</State>

    <!--
    Optional: these attributes get associated to the code+token.
    They will be available to the api proxy whenever the token is
    subsequently validated.
    -->
    <Attributes>
      <!-- If set to false, the attribute wont be delivered in the auth code response. -->
      <Attribute name='attr_name1' ref='flow.variable1' display='true|false'>value1</Attribute>
      <Attribute name='attr_name2' ref='flow.variable2' display='true|false'>value2</Attribute>
    </Attributes>

    <!--
      With <GenerateResponse enabled='true'/>, a response will be generated and returned.
      With enabled='false', then these flow variables are set on success:
        oauthv2authcode.<PolicyName>.code
        oauthv2authcode.<PolicyName>.redirect_uri
        oauthv2authcode.<PolicyName>.scope
        oauthv2authcode.<PolicyName>.client_id
    -->
    <GenerateResponse enabled='true'/>
</OAuthV2>\n")

     '("OAuthV2 - VerifyAccessToken"
       "OAuthV2-VerifyAccessToken"
       "<OAuthV2 name='##'>
    <DisplayName>##</DisplayName>
    <Operation>VerifyAccessToken</Operation>
    <!-- by default, pulls token from Authorization header as per OAuthV2.0 spec -->
    <AccessToken>flow.variable</AccessToken> <!-- Optional -->
    <AccessTokenPrefix>Bearer</AccessTokenPrefix> <!-- Optional -->
    <!--
    This policy sets the following flow variables:
      organization_name
      developer.id
      developer.app.name
      client_id
      grant_type
      token_type
      access_token
      accesstoken.{custom_attribute}
      issued_at
      expires_in
      status
      scope
      apiproduct.name*
      apiproduct.<custom_attribute_name>*
    -->
</OAuthV2>\n")

     '("OAuthV2 - RefreshAccessToken"
       "OAuthV2-RefreshAccessToken"
       "<OAuthV2 enabled='true' name='##'>
    <DisplayName>OAuthV2 - RefreshAccessToken</DisplayName>
    <Operation>RefreshAccessToken</Operation>
    <Attributes/> <!-- not sure if valid here -->
    <!--
    client_id and client_secret are expected in the Authorization Header
    passed as Basic Auth (concatenated with colon, then base64 encoded).
    -->

    <!--
    ExpiresIn, in milliseconds. The ref is optional. The explicitly specified
    value is the default, when the variable reference cannot be resolved.
      2400000 = 40 minutes
      3600000 = 60 minutes
    -->
    <ExpiresIn ref='flow.variable'>3600000</ExpiresIn>

    <ExternalAuthorization>false</ExternalAuthorization>
    <GrantType>request.formparam.grant_type</GrantType> <!-- must be \"refresh_token\" -->
    <RefreshToken>request.formparam.refresh_token</RefreshToken>
    <SupportedGrantTypes/>

    <GenerateResponse/>

    <!--
    NB: If <GenerateResponse/> is omitted, then the policy implicitly sets
    the following variables:
      oauthv2accesstoken.<PolicyName>.access_token
      oauthv2accesstoken.<PolicyName>.token_type
      oauthv2accesstoken.<PolicyName>.expires_in
      oauthv2accesstoken.<PolicyName>.refresh_token
      oauthv2accesstoken.<PolicyName>.refresh_token_expires_in
      oauthv2accesstoken.<PolicyName>.refresh_token_issued_at
      oauthv2accesstoken.<PolicyName>.refresh_token_status
    -->
</OAuthV2>\n")


     '("OAuthV2 - InvalidateToken"
       "OAuthV2-InvalidateToken"
       "<OAuthV2 name='##'>
    <Operation>InvalidateToken</Operation> <!-- aka \"Revoke\" token -->
    <Tokens>
        <Token type='${1:$$(yas-choose-value '(\"accesstoken\" \"refreshtoken\"))}'
               cascade='${2:$$(yas-choose-value '(\"true\" \"false\"))}'>${3:flow.variable}</Token>
    </Tokens>
</OAuthV2>\n")

     '("OAuthV2 - Delete Token"
       "OAuthV2-DeleteToken"
     "<DeleteOAuthV2Info name='##'>
    <!-- keep one of the following -->
    <AccessToken ref='access_token.flow.variable'/>
    <AccessToken>{access_token}</AccessToken>
</DeleteOAuthV2Info>\n")

     '("OAuthV2 - Delete Code"
       "OAuthV2-DeleteCode"
"<DeleteOAuthV2Info name='##'>
    <!-- keep one of the following -->
    <AuthorizationCode ref='flow.variable'/>
    <AuthorizationCode>{flow.variable}</AuthorizationCode>
</DeleteOAuthV2Info>\n")


     '("OAuthV2 - GetClientInfo"
       "OAuthV2-GetInfo"
"<GetOAuthV2Info name='##'>
    <!--
    See http://apigee.com/docs/api-services/reference/get-oauth-v2-info-policy

    Use one of the following forms: a referenced variable or
    an explicitly passed client_id.
     -->
    <ClientId ref='{flow.variable}'/>
    <ClientId>abcdefghijklmnop</ClientId>
    <!--
    On Success, the following flow variables will be set.
      oauthv2client.##.client_id
      oauthv2client.##.client_secret
      oauthv2client.##.redirection_uris
      oauthv2client.##.developer.email
      oauthv2client.##.developer.app.name
      oauthv2client.##.developer.id
      oauthv2client.##.{custom_attribute_name}
    -->
</GetOAuthV2Info>\n")

     '("OAuthV2 - GetAccessTokenInfo"
       "OAuthV2-GetTokenInfo"
     "<GetOAuthV2Info name='##'>
    <!-- http://apigee.com/docs/api-services/content/authorize-requests-using-oauth-20 -->
    <!-- use one of the following: a referenced variable or -->
    <!-- an explicitly passed access_token -->
    <AccessToken ref='${1:request.queryparam.access_token}'/>
    <AccessToken>${2:BAADBEEF}</AccessToken>
    <RefreshToken ref='{flow.variable}'/>
    <!--
    On Success, the following flow variables will be set.
      oauthv2accesstoken.##.access_token
      oauthv2accesstoken.##.scope
      oauthv2accesstoken.##.refresh_token
      oauthv2accesstoken.##.accesstoken.{custom_attribute_name}
      oauthv2accesstoken.##.developer.id
      oauthv2accesstoken.##.developer.app.name
      oauthv2accesstoken.##.expires_in
      oauthv2accesstoken.##.status
    -->
</GetOAuthV2Info>\n")


     '("OAuthV2 - SetAccessTokenInfo"
       "OAuthV2-SetTokenInfo"
     "<SetOAuthV2Info name='##'>
    <!--
      http://apigee.com/docs/api-services/reference/set-oauth-v2-info-policy
    -->
    <AccessToken ref='${1:request.queryparam.access_token}'/>
    <Attributes>
      <Attribute name='department.id' ref='request.queryparam.department_id'></Attribute>
      <Attribute name='scope' ref=''>READ, WRITE</Attribute>
    </Attributes>

</SetOAuthV2Info>\n")

     '("OAuthV2 - GetRefreshTokenAttributes"
       "OAuthV2-GetOAuthV2Info"
     "<GetOAuthV2Info name='##'>
    <!-- use one of the following: a referenced variable or -->
    <!-- an explicitly passed refresh_token -->
    <RefreshToken ref='${1:flow.variable}'/>
    <RefreshToken>${2:refresh_token}</RefreshToken>
    <!--
    On Success, the following flow/context variables will be set.
      oauthv2accesstoken.##.refresh_token
      oauthv2accesstoken.##.refresh_token_expires_in
      oauthv2accesstoken.##.refresh_token_issued_at
      oauthv2accesstoken.##.refresh_token_status
    -->
</GetOAuthV2Info>\n")

     '("OAuthV2 - GetAuthorizationCodeInfo"
       "OAuthV2-GetOAuthV2Info"
     "<GetOAuthV2Info name='##'>
    <!-- use one of the following: a referenced variable or -->
    <!-- an explicitly passed authorization_code -->
    <AuthorizationCode ref='${1:flow.variable}'/>
    <AuthorizationCode>${2:BAADBEEF}</AuthorizationCode>
    <!--
    On Success, the following flow variables will be set.
      oauthv2authcode.##.client_id
      oauthv2authcode.##.organization_id
      oauthv2authcode.##.issued_at
      oauthv2authcode.##.expires_in
      oauthv2authcode.##.redirect_uri
      oauthv2authcode.##.status
      oauthv2authcode.##.state
      oauthv2authcode.##.scope
      oauthv2authcode.##.id
      oauthv2authcode.##.{custom_attribute_name}
    -->
</GetOAuthV2Info>\n")

     '("OAuthV1 - GetInfo - AppKey"
       "OAuthV1-GetOAuthV1Info"
     "<GetOAuthV1Info name='##'>
     <!-- deprecated - use VerifyAPIKey -->
  <AppKey ref='request.formparam.apikey'/>
</GetOAuthV1Info>\n")

     '("OAuthV1 - GetInfo - ConsumerKey"
       "OAuthV1-GetOAuthV1Info"
     "<GetOAuthV1Info name='##'>
  <ConsumerKey ref='request.formparam.oauth_consumer_key'/>
</GetOAuthV1Info>\n")

     '("OAuthV1 - GetInfo - Token"
       "OAuthV1-GetOAuthV1Info"
     "<GetOAuthV1Info name='##'>
  <${1:$$(yas-choose-value '(\"AccessToken\" \"RequestToken\" ))} ref='request.formparam.oauth_token'/>
  <!--
  On Success, these variables will be populated:
    oauth_token
    oauth_token_secret
    developer.app.name
    developer.id
    ??
    apiproduct.name
    apiproduct.<custom_attribute_name>
  -->
</GetOAuthV1Info>\n")

     '("OAuthV1 - GenerateRequestToken"
       "OAuthV1-GenerateRequestToken"
"<OAuthV1 name='##'>
  <Operation>GenerateRequestToken</Operation>
  <GenerateResponse enabled='${1:$$(yas-choose-value '(\"true\" \"false\" ))}'>
    <Format>${2:$$(yas-choose-value '(\"FORM_PARAM\" \"XML\" ))}</Format>
  </GenerateResponse>
  <GenerateErrorResponse enabled='${3:$$(yas-choose-value '(\"true\" \"false\" ))}'>
    <Format>${4:$$(yas-choose-value '(\"FORM_PARAM\" \"XML\" ))}</Format>
    <Realm>http://oauth.apigee.com/oauth/1/</Realm>
  </GenerateErrorResponse>
</OAuthV1>\n")

     '("OAuthV1 - GenerateAccessToken"
       "OAuthV1-GenerateAccessToken"
"<OAuthV1 name='##'>
  <!--
    ExpiresIn, in milliseconds. The ref is optional. The explicitly specified
    value is the default, when the variable reference cannot be resolved.
      2400000 = 40 minutes
      3600000 = 60 minutes
  -->
  <ExpiresIn ref='flow.variable'>2400000</ExpiresIn>
  <Operation>GenerateAccessToken</Operation>
  <GenerateResponse enabled='${1:$$(yas-choose-value '(\"true\" \"false\" ))}'>
    <Format>${2:$$(yas-choose-value '(\"FORM_PARAM\" \"XML\" ))}</Format>
  </GenerateResponse>
  <GenerateErrorResponse enabled='${3:$$(yas-choose-value '(\"true\" \"false\" ))}'>
    <Format>${4:$$(yas-choose-value '(\"FORM_PARAM\" \"XML\" ))}</Format>
    <Realm>http://oauth.apigee.com/oauth/1/</Realm>
  </GenerateErrorResponse>
</OAuthV1>\n")

     '("OAuthV1 - VerifyAccessToken"
       "OAuthV1-VerifyAccessToken"
 "<OAuthV1 name='##'>
  <Operation>VerifyAccessToken</Operation>

<!--

By default, the access token is expected to be presented by the
app in the Authorization HTTP header, according to the OAuth 2.0
specification. Use the AccessToken element if the access token is
available in a non-standard location, such as a query parameter,
in the payload, or an HTTP header with a name other than
Authorization.

-->
  <AccessToken>request.queryparam.token</AccessToken>

  <GenerateErrorResponse enabled='${1:$$(yas-choose-value '(\"true\" \"false\" ))}'>
    <Format>${2:$$(yas-choose-value '(\"FORM_PARAM\" \"XML\" ))}</Format>
    <Realm>http://oauth.apigee.com/oauth/1/</Realm>
  </GenerateErrorResponse>
</OAuthV1>\n")


     '("MessageLogging - SysLog"
       "ML-SysLog"
       "<MessageLogging enabled='true' continueOnError='true' name='##'>
    <DisplayName>##</DisplayName>
    <BufferMessage>false</BufferMessage>
    <Syslog async='true'>
        <Host>${1:hostname.domain.com}</Host>
        <Message>4G:{${2:$$(yas-choose-value apigee-common-variable-list)}}</Message>
        <Port>514</Port>
        <Protocol>TCP</Protocol>
        <SSLInfo>
            <Enabled>true</Enabled> <!-- true / false -->
        </SSLInfo>
    </Syslog>
    <IgnoreUnresolvedVariables>true</IgnoreUnresolvedVariables>
    <logLevel>INFO</logLevel>
    <NotificationIntervalInSec>0</NotificationIntervalInSec>
</MessageLogging>\n")

     '("MessageLogging - Log file"
       "ML-File"
       "<MessageLogging name='##'>
    <DisplayName>##</DisplayName>
   <File>
        <Message>{system.time},{request.path},{response.header.X-time-total-elapsed},{response.header.X-time-target-elapsed},{response.status.code}
</Message>
        <FileName>atlantis-perf.log</FileName>
        <FileRotationOptions rotateFileOnStartup='true'>
            <FileRotationType>SIZE</FileRotationType>
            <MaxFileSizeInMB>10</MaxFileSizeInMB>
            <MaxFilesToRetain>10</MaxFilesToRetain>
        </FileRotationOptions>
    </File>
</MessageLogging>\n")

     '("SAML - Validate"
       "SAML"
       "<ValidateSAMLAssertion name='SAML-Validate' ignoreContentType='false'>
  <Source name='request'>
    <Namespaces>
      <Namespace prefix='soap'>http://schemas.xmlsoap.org/soap/envelope/</Namespace>
      <Namespace prefix='wsse'>http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd</Namespace>
      <Namespace prefix='saml'>urn:oasis:names:tc:SAML:2.0:assertion</Namespace>
    </Namespaces>
    <XPath>/Envelope/Header/Security/saml:Assertion</XPath>
  </Source>
  <TrustStore>TrustStoreName</TrustStore>
  <RemoveAssertion>false</RemoveAssertion>
</ValidateSAMLAssertion>\n")

     '("SAML - Generate"
       "SAML"
       "<GenerateSAMLAssertion name=\"SAML\" ignoreContentType=\"false\">
  <CanonicalizationAlgorithm />
  <Issuer ref='reference'>Issuer name</Issuer>
  <KeyStore>
    <Name>keystorename</Name>
    <Alias>alias</Alias>
  </KeyStore>
  <OutputVariable>
    <FlowVariable>flow.variable.name</FlowVariable>
    <Message name='request'>
      <XPath>XPath to element that will contain the assertion</XPath>
    </Message>
  </OutputVariable>
  <SignatureAlgorithm />
  <Subject ref='reference'>Subject name</Subject>
  <Template ignoreUnresolvedVariables='false'>
    <!-- A lot of XML goes here, in CDATA, with {} around each variable -->
  </Template>
</GenerateSAMLAssertion>\n")

     '("Cache - ResponseCache"
     "RC"
     "<ResponseCache name='##'>
  <DisplayName>${1:##}</DisplayName>
  <!-- composite item to use as cache key -->
  <CacheKey>
    <Prefix>anything</Prefix>
    <KeyFragment ref='${2:request.uri}' />
  </CacheKey>
  <CacheResource>${3:ApigeeCache}</CacheResource>
  <Scope>${4:$$(yas-choose-value '(\"Exclusive\" \"Global\" \"Application\" \"Proxy\" \"Target\"))}</Scope>
  <UseAcceptHeader>true</UseAcceptHeader>
  <ExpirySettings>
    <ExpiryDate></ExpiryDate>
    <TimeOfDay></TimeOfDay>
    <TimeoutInSec ref='insert.variable.here'>${5:6000}</TimeoutInSec>
  </ExpirySettings>
  <SkipCacheLookup>NOT (request.verb ~~ \"(GET|HEAD)\")</SkipCacheLookup>
  <SkipCachePopulation>NOT (request.verb ~~ \"(GET|HEAD)\")</SkipCachePopulation>
</ResponseCache>")

     '("Cache - PopulateCache"
       "CP"
       "<PopulateCache name='##'>
  <CacheResource>${1:ApigeeCache}</CacheResource>
  <Source>${2:variable.containing.value}</Source>
  <Scope>${3:$$(yas-choose-value '(\"Exclusive\" \"Global\" \"Application\" \"Proxy\" \"Target\"))}</Scope>
  <CacheKey>
    <KeyFragment ref='${4:variable.containing.keyfrag}' />
  </CacheKey>
  <ExpirySettings>
    <!-- include one of the following... -->
    <TimeOfDay ref='time_variable'>hh:mm:ss</TimeOfDay>
    <TimeoutInSec ref='duration_variable'>864000</TimeoutInSec> <!-- 864000 = 10 days -->
    <ExpiryDate ref='date_variable'>mm-dd-yyyy</ExpiryDate>
  </ExpirySettings>
</PopulateCache>\n")

          '("Cache - LookupCache"
       "CL"
       "<LookupCache name='##'>
    <CacheResource>${1:ApigeeCache}</CacheResource>
    <AssignTo>${2:flowvariable}</AssignTo> <!-- name of flow variable -->
    <Scope>${3:$$(yas-choose-value '(\"Exclusive\" \"Global\" \"Application\" \"Proxy\" \"Target\"))}</Scope>
    <CacheKey>
      <!--  <Prefix>apiAccessToken</Prefix> -->
      <KeyFragment ref='${4:flowvariable.name}' />
    </CacheKey>
</LookupCache>")

          '("Cache - InvalidateCache"
       "CI"
          "<InvalidateCache name='##'>
    <CacheResource>${1:ApigeeCache}</CacheResource>
    <Scope>${2:$$(yas-choose-value '(\"Exclusive\" \"Global\" \"Application\" \"Proxy\" \"Target\"))}</Scope>
    <CacheKey>
      <!--  <Prefix>apiAccessToken</Prefix> -->
      <KeyFragment ref='${3:flowvariable.name}' />
    </CacheKey>
    <PurgeChildEntries>true</PurgeChildEntries>
</InvalidateCache>")

     '("RaiseAlert"
       "RaiseAlert"
       "<RaiseAlert name='##'>
    <Syslog>
        <Message>4G: message text here {${1:$$(yas-choose-value apigee-common-variable-list)}}</Message>
        <Host>IP</Host>
        <Port>514</Port>
    </Syslog>
</RaiseAlert>\n")


     '("RaiseFault"
       "RF"
       "<RaiseFault name='##'>
  <DisplayName>${1:##}</DisplayName>
  <Description>$2</Description>
  <IgnoreUnresolvedVariables>true</IgnoreUnresolvedVariables>
  <FaultResponse>
    <Set>
      <Payload contentType='${3:$$(yas-choose-value (reverse (apigee--sort-strings (mapcar 'car apigee-message-payload-template-alist))))}'
               variablePrefix='%' variableSuffix='#'>${3:$(cadr (assoc yas-text apigee-message-payload-template-alist))}$0</Payload>
      <StatusCode>${4:$$(yas-choose-value (reverse (apigee--sort-strings (mapcar 'car apigee-http-status-message-alist))))}</StatusCode>
      <ReasonPhrase>${4:$(cadr (assoc yas-text apigee-http-status-message-alist))}</ReasonPhrase>
    </Set>
  </FaultResponse>
</RaiseFault>")


     '("RaiseFault - 302 Redirect"
       "RF-302"
       "<RaiseFault name='##'>
    <DisplayName>${1:##}</DisplayName>
    <FaultResponse>
      <Set>
        <Headers>
          <Header name='Location'>${2:http://target.to-redirect.to}</Header>
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

     '("RaiseFault - SOAP Fault"
       "RF"
       "<RaiseFault name='##'>
  <Description>$1</Description>
  <IgnoreUnresolvedVariables>true</IgnoreUnresolvedVariables>
  <FaultResponse>
    <Set>
      <Payload contentType='application/xml'
               variablePrefix='%' variableSuffix='#'>
        <soap:Envelope xmlns:soap='http://schemas.xmlsoap.org/soap/envelope/'>
          <soap:Body>
            <soap:Fault>
              <faultcode>102</faultcode>
              <faultstring>Invalid Request</faultstring>
              <detail>
              </detail>
            </soap:Fault>
          </soap:Body>
        </soap:Envelope>
      </Payload>
      <StatusCode>400</StatusCode>
      <ReasonPhrase>Bad Request</ReasonPhrase>
    </Set>
  </FaultResponse>
</RaiseFault>\n")

     '("Javascript"
       "JS"
       "<Javascript name='##' timeLimit='200' >
  <!-- <DisplayName>${1:##}</DisplayName> -->
  <Properties>
    <!-- to retrieve properties in js code:   properties.prop1 -->
    <Property name='prop1'>value-here</Property>
  </Properties>
  <IncludeURL>jsc://URI.js</IncludeURL> <!-- optionally specify a shared resource here -->
  <ResourceURL>jsc://${2:$$(apigee--fixup-script-name \"##\" \"JS\")}.js</ResourceURL>
</Javascript>")


     '("Python"
       "Py"
       "<Script name='##'>
    <DisplayName>${1:##}</DisplayName>
  <ResourceURL>py://${2:$$(apigee--fixup-script-name \"##\" \"PY\")}.py</ResourceURL>
</Script>")


     '("XSL"
       "XSL"
       "<XSL name='##'>
  <DisplayName>${1:##}</DisplayName>
  <Source>${2:$$(yas-choose-value '(\"request\" \"response\"))}</Source>
  <OutputVariable>request.content</OutputVariable>
  <ResourceURL>xsl://${3:$$(apigee--fixup-script-name \"##\" \"XSL\")}.xsl</ResourceURL>

  <!--
  Parameters are optional. reference them in the XSL as:
    <xsl:param name=\"uid\" select=\"''\"/>
    <xsl:param name=\"pwd\" select=\"''\"/>
  -->
  <Parameters ignoreUnresolvedVariables='true'>
    <Parameter name='uid' ref='authn.uid'/>
    <Parameter name='pwd' ref='authn.pwd'/>
  </Parameters>
</XSL>\n")

     '("XSL - StripSoap"
       "XSL"
       "<XSL name='##'>
  <DisplayName>${1:##}</DisplayName>
  <Source>${2:$$(yas-choose-value '(\"request\" \"response\"))}</Source>
  <OutputVariable>request.content</OutputVariable>
  <ResourceURL>xsl://stripSoap.xsl</ResourceURL>
</XSL>\n")

     '("AccessControl"
       "AccessControl"
       "<AccessControl name='ACL'>
    <IPRules noRuleMatchAction='DENY'>
        <MatchRule action='ALLOW'>
            <SourceAddress mask='24'>10.10.20.0</SourceAddress>
            <SourceAddress mask='24'>10.10.30.0</SourceAddress>
            <SourceAddress mask='24'>10.10.40.0</SourceAddress>
        </MatchRule>
    </IPRules>
</AccessControl>\n")

     '("JavaCallout"
       "Java"
       "<JavaCallout name='##'>
  <DisplayName>${1:##}</DisplayName>
  <Properties>
    <Property name='propName1'>value-goes-here</Property>
    <Property name='propName2'>another-value-here</Property>
  </Properties>
  <!--
  To access properties in the callout, define a ctor that accepts a
  java.util.Map argument:

    import java.util.Map;
    ...
      private Map properties; // read-only
      public CalloutClassName (Map properties) { this.properties = properties; }

      ....
        String propName1 = (String) this.properties.get(\"propName1\");
  -->
  <ClassName>${2:com.company.ClassName}</ClassName>
  <ResourceURL>java://${3:$$(apigee--fixup-script-name \"##\" \"Java\")}.jar</ResourceURL>
</JavaCallout>")
))


(defvar apigee-upload-command-alist nil
  "alist used at runtime to cache the upload commands")


(defun apigee-insure-trailing-slash (path)
  "Insure the given path ends with a slash. This is useful with
`default-directory'. Setting `default-directory' to a value that
does not end with a slash causes it to use the parent directory.
"
  (and path
       (if (s-ends-with? "/" path) path (concat path "/"))))

(defun apigee-insure-no-trailing-slash (path)
  "Insure the given path does not end with a slash. This is usedful with
`file-name-nondirectory'.
"
  (and path
       (if (s-ends-with? "/" path)
           (substring path 0 (1- (length path)))
         path)))


(defun apigee-path-of-apiproxy ()
  "Returns the path of the directory that contains the
apiproxy directory.

If the apiproxy is defined in a structure like this:

~/dev/apiproxies/APINAME/apiproxy
~/dev/apiproxies/APINAME/apiproxy/APINAME.xml
~/dev/apiproxies/APINAME/apiproxy/resources
~/dev/apiproxies/APINAME/apiproxy/resources/...
~/dev/apiproxies/APINAME/apiproxy/targets
~/dev/apiproxies/APINAME/apiproxy/targets/..
~/dev/apiproxies/APINAME/apiproxy/proxies
~/dev/apiproxies/APINAME/apiproxy/proxies/..
..

... and this function is invoked from anywhere in one of those directories,
then the return value is: ~/dev/apiproxies/APINAME/

It always ends in slash.

"
  (interactive)

  (let ((path
         (apigee-insure-trailing-slash
          (let ((maybe-this (concat (file-name-directory default-directory) "apiproxy")))
            (if (apigee--is-directory maybe-this)
                (file-name-directory default-directory)
              (let ((elts (reverse (split-string (file-name-directory default-directory) "/")))
                    r)
                (while (and elts (not r))
                  (if (string= (car elts) "apiproxy")
                      (setq r (reverse (cdr elts)))
                    (setq elts (cdr elts))))
                (if r
                    (mapconcat 'identity r "/") )))))))
    (and path (file-truename path))))


(defun apigee-all-files-in-apiproxy ()
  "returns a list of all files in an apiproxy"
  (let* ((proxydir (s-chop-suffix "/" (apigee-path-of-apiproxy)))
         (command (concat "find " proxydir " -name \\*.xml"))
         (result (s-trim-right (shell-command-to-string command))))
    (if (s-blank? result) nil
      (reverse (s-split "\n" result)))))


(defun apigee-metadata-file-name ()
  "Get the full path of the file that holds the metadata for the API Proxy
bundle that contains the file or directory currently being edited.
eg
  /Users/dino/dev/apiproxies/wagov-corpdata-2/apiproxy/corpdata2.xml

"
  (let ((apiproxy-dir (apigee-insure-trailing-slash (apigee-path-of-apiproxy))))
    (if apiproxy-dir
        (let ((files
               (directory-files
                (concat apiproxy-dir "apiproxy/") t ".*\.xml$")))
          (car files)))))

(defun apigee--update-last-modified()
  "updates the last modified time in the metadata file for the current proxy."

  (let ((path-to-metadata-file (apigee-metadata-file-name))
        fileChanged-p )
    (with-temp-buffer
      (insert-file-contents path-to-metadata-file)
      ;; process text
      (goto-char (point-min))
      (if (re-search-forward "\\(<LastModifiedAt>\\)\\([^<]+\\)\\(</LastModifiedAt>\\)" nil t)
          (setq fileChanged-p
                (or
                 (replace-match (concat "\\1"
                                        (apigee--java-get-time-in-millis)
                                        "\\3"))
                 t)))
      (when fileChanged-p (write-region 1 (point-max) path-to-metadata-file)))))


(defun apigee-apiproxy-name ()
  "Get a name for the API Proxy bundle that contains
the file or directory currently being edited.
"
  (let ((apiproxy-dir (apigee-insure-no-trailing-slash (apigee-path-of-apiproxy))))
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
;; emacs. Currently this module delegates that to the pushapi script,
;; so this fn is unnecessary. But eventually all the upload logic could
;; be implemented in elisp.
(defun apigee-get-create-api-bundle-zip-cmd ()
  "Get the command that creates an API bundle zip. For the file
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



;; The following fn can be used to create the zip and upload from emacs.
;; Currently this module delegates that to the pushapi script, so this
;; fn is unnecessary. But eventually all the upload logic could
;; be implemented in elisp.
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


(defun apigee--guess-upload-command (proxy-dir)
  "non-interative function that returns a guess for the upload
command. It derives the guess from the most recently used
command, replacing the final element of that command, which is
the path of the apiproxy bundle, with the path of the current api
proxy bundle."
  (if apigee--most-recently-used-upload-command
      (let ((cmd-elts (reverse (split-string apigee--most-recently-used-upload-command))))
        (setcar cmd-elts proxy-dir)
        (s-join " " (reverse cmd-elts)))))


(defun apigee-upload-bundle-with-pushapi ()
  "Interactive fn that uses the pushapi script to upload the bundle
that contains the file or directory currently being edited.
"
  (interactive)
  (let ((proxy-dir (apigee-path-of-apiproxy)))
    (and proxy-dir
         (file-exists-p apigee-upload-bundle-pgm)
         (let ((this-cmd (assoc proxy-dir apigee-upload-command-alist)))
           ;; modify the LastModifiedAt time in the bundle file
           (apigee--update-last-modified)
           ;; interactively invoke the command (and allow user to change)
           (set (make-local-variable 'compile-command)
                (or (cdr this-cmd)
                    (apigee--guess-upload-command proxy-dir)
                    (concat
                     apigee-upload-bundle-pgm " "
                     apigee-upload-bundle-args " "
                     proxy-dir)))
           (call-interactively 'compile)
           (if this-cmd
               (setcdr this-cmd compile-command)
             (add-to-list 'apigee-upload-command-alist (cons proxy-dir compile-command)))
           ;; save for next time:
           (setq apigee--most-recently-used-upload-command compile-command)
           ))))


(defun apigee--is-directory (dir-name)
  "Tests to see whether a name refers to a directory"
  (and
   (file-exists-p dir-name)
   (let ((attrs (file-attributes dir-name)))
     (and
      (car attrs)
      (not (stringp (car attrs)))))))


(defun apigee--is-policy-file (file-name)
  "Tests to see whether a fully-qualified FILE-NAME refers to a file in the policies directory"
  (and
   (file-exists-p file-name)
   (s-equals-p
    (file-name-nondirectory
     (s-chop-suffix "/"
                    (file-name-directory file-name))) "policies")
   (s-equals-p
    (file-name-nondirectory
     (s-chop-suffix "/"
                    (file-name-directory
                     (s-chop-suffix "/"
                                    (file-name-directory file-name))))) "apiproxy")
   (s-ends-with? ".xml" file-name)))


(defun apigee--java-get-time-in-millis ()
  "Returns a string that contains a number equal in value to
what is returned from the java snippet:
      (new GregrianCalendar()).getTimeInMillis()
"
  (let ((ct (current-time)))
    (format "%d" (+ (* (+ (* (car ct) 65536) (cadr ct)) 1000) (/ (caddr ct) 1000)))))


(defun apigee-insert-java-time-in-millis ()
  "inserts a string into the current buffer that contains a number equal in value to
what is returned from the java snippet:
      (new GregrianCalendar()).getTimeInMillis()
"
  (interactive)
  (insert (apigee--java-get-time-in-millis)))


(defun apigee-policy-name-is-available (pname)
  "Return true if the passed policy name is unused, in other words
if no file exists by that name in the given proxy.
"
  (let ((filename-to-check
         (concat (apigee-path-of-apiproxy) "apiproxy/policies/" pname ".xml")))
    (not (file-exists-p filename-to-check))))


(defun apigee--suggested-policy-name (ptype)
  "Returns a string that contains a default policy name, uses a counter
that is indexed per policy type within each API Proxy.
"
  (let ((val 1)
        ;; lambda in lieu of flet
        (next-name (lambda (v) (concat ptype "-" (format "%d" v)))))
    (let ((pname (funcall next-name val)))
      (while (not (apigee-policy-name-is-available pname))
        (setq val (1+ val)
              pname (funcall next-name val)))
      pname)))


(defun apigee-target-name-is-available (tname)
  (let* ((proxy-dir (apigee-path-of-apiproxy))
         (targets-dir (concat proxy-dir "/apiproxy/targets/"))
         (filename-to-check (concat targets-dir tname ".xml")))
    (not (file-exists-p filename-to-check))))


(defun apigee--suggested-target-name ()
  "Returns a string that contains a suggested target name, uses a counter.
"
  (let ((val 1)
        (next-name (lambda (v) (concat "target-" (format "%d" v))))) ;; in lieu of flet
    (let ((tname (funcall next-name val)))
      (while (not (apigee-target-name-is-available tname))
        (setq val (1+ val)
              tname (funcall next-name val)))
      tname)))


(defun apigee--get-createdby ()
  "Returns a string that contains a username, useful for
applying as the CreatedBy element in an API Proxy.
"
  (or (getenv "LOGNAME") (getenv "USER") "orgAdmin"))



(defun apigee-entity-id-types (entity-type)
  "return a list of id types for a given entity-type."
  (let ((id-types (assoc entity-type apigee-entity-to-entity-id-types-alist)))
    (if id-types (cadr id-types))))



;; (defun apigee--random-string (&optional len)
;;   "produce a string of length LEN containing random characters,
;; or of length 8 if the len is not specified.
;; "
;;   (let (s '())
;;     (if (not len) (setq len 8))
;;     (if (> len 144) (setq len 8)) ;; sanity
;;     (while (> len 0)
;;       (setq s (cons (+ (random 26) 97) s)
;;             len (- len 1)))
;;     (mapconcat 'string s "")))

(defun apigee--insert-policy-clean-response-headers (apiproxy-dir)
  "inserts a policy for clean response headers"
  (with-temp-file (concat apiproxy-dir "policies/AM-CleanResponseHeaders.xml")
    (insert "<AssignMessage name='AM-CleanResponseHeaders'>
  <Remove>
    <Headers>
      <Header name='Accept'/>
      <Header name='user-agent'/>
      <Header name='Host'/>
      <Header name='x-forwarded-for'/>
      <Header name='X-Forwarded-Proto'/>
      <Header name='X-Forwarded-Port'/>
      <Header name='apikey'/>
      <Header name='date'/>
      <Header name='Authorization'/>
      <Header name='Signature'/>
      <Header name='X-Powered-By'/>
    </Headers>
  </Remove>
  <IgnoreUnresolvedVariables>true</IgnoreUnresolvedVariables>
  <AssignTo createNew='false' transport='http' type='response'/>
</AssignMessage>
")))

(defun apigee--insert-policy-invalid-api-key-fault-response (apiproxy-dir)
  "inserts a policy for assigning a message for invalid api key"
  (with-temp-file (concat apiproxy-dir "policies/AM-InvalidApiKey.xml")
    (insert "<AssignMessage name='AM-InvalidApiKey'>
    <Remove>
        <Headers>
            <Header name='Accept'/>
            <Header name='user-agent'/>
            <Header name='Authorization'/>
            <Header name='Signature'/>
            <Header name='Date'/>
            <Header name='Host'/>
            <Header name='X-Powered-By'/>
            <Header name='X-Forwarded-Port'/>
            <Header name='X-Forwarded-Proto'/>
        </Headers>
    </Remove>
    <Set>
        <Payload contentType='application/json'>
{ \"error\" : { \"code\":152000, \"message\":\"Invalid client.\" } }
</Payload>
        <StatusCode>401</StatusCode>
        <ReasonPhrase>Unauthorized</ReasonPhrase>
    </Set>
    <IgnoreUnresolvedVariables>true</IgnoreUnresolvedVariables>
    <AssignTo createNew='false' transport='http' type='response'/>
</AssignMessage>
")))

(defun apigee--insert-policy-expired-api-key-fault-response (apiproxy-dir)
  "inserts a policy for assigning a message for expired api key"
  (with-temp-file (concat apiproxy-dir "policies/AM-ExpiredApiKey.xml")
    (insert "<AssignMessage name='AM-ExpiredApiKey'>
    <Remove>
        <Headers>
            <Header name='Accept'/>
            <Header name='user-agent'/>
            <Header name='Authorization'/>
            <Header name='Signature'/>
            <Header name='Date'/>
            <Header name='Host'/>
            <Header name='X-Powered-By'/>
            <Header name='X-Forwarded-Port'/>
            <Header name='X-Forwarded-Proto'/>
        </Headers>
    </Remove>
    <Set>
        <Payload contentType='application/json' variablePrefix='{' variableSuffix='}'>
{ \"error\" : { \"code\":152001, \"message\":\"The API Key is expired.\" } }
</Payload>
        <StatusCode>401</StatusCode>
        <ReasonPhrase>Unauthorized</ReasonPhrase>
    </Set>
    <IgnoreUnresolvedVariables>true</IgnoreUnresolvedVariables>
    <AssignTo createNew='false' transport='http' type='response'/>
</AssignMessage>
")))

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
        (error (format "proxy dir %s already exists" proxy-dir))
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
  <Description>default target endpoint</Description>
  <FaultRules>
    <FaultRule name='other-fault'>
      <!-- This FaultRule always catches all uncaught faults. -->
      <Step>
        <Name>JS-MaybeFormatFault</Name>
      </Step>
    </FaultRule>
  </FaultRules>

  <Flows/>
  <PreFlow name='PreFlow'>
    <Request>
      <!-- remove this to allow requests -->
      <Step><Name>RF-UnknownRequest</Name></Step>
    </Request>
    <Response>
    </Response>
  </PreFlow>

  <HTTPTargetConnection>
    <Properties/>
    <!-- modify this URL to point to something valid -->
    <URL>http://internal.example.com/v1/XYZ/something</URL>
  </HTTPTargetConnection>

  <!--
  <ScriptTarget>
      <ResourceURL>node://yahoo-weather.js</ResourceURL>
      <Properties/>
  </ScriptTarget>
  -->
</TargetEndpoint>\n"))

        (with-temp-file (concat apiproxy-dir "policies/RF-UnknownRequest.xml")
          (insert "<RaiseFault name='RF-UnknownRequest'>
  <IgnoreUnresolvedVariables>true</IgnoreUnresolvedVariables>
  <FaultResponse>
    <Set>
      <Payload contentType='application/json'>{
  \"error\" : {
    \"code\" : 404.01,
    \"message\" : \"that request was unknown; try a different request.\"
  }
}
</Payload>
      <StatusCode>404</StatusCode>
      <ReasonPhrase>Not Found</ReasonPhrase>
    </Set>
  </FaultResponse>
</RaiseFault>
"))

        (with-temp-file (concat apiproxy-dir "policies/JS-MaybeFormatFault.xml")
          (insert "<Javascript name='JS-MaybeFormatFault' timeLimit='200' >
  <ResourceURL>jsc://maybeFormatFault.js</ResourceURL>
</Javascript>
"))

        (with-temp-file (concat apiproxy-dir "resources/jsc/maybeFormatFault.js")
          (insert "// maybeFormatFault.js
// ------------------------------------------------------------------
//
// maybe format a fault message if one is not present.
//
// created: Tue Jan 26 14:07:19 2016
// last saved: <2016-January-26 14:17:11>

var handled = context.getVariable('fault_handled');
if ( ! handled ) {
  var error = response.content.asXML.error;
  var t = typeof error;
  print('typeof error: ' + t);
  if (t == 'undefined') {
    response.content = '<error><code>1001</code><message>unknown error</message></error>';
  }
  context.setVariable('fault_handled', true);
}
"))

        (apigee--insert-policy-clean-response-headers apiproxy-dir)
        (apigee--insert-policy-invalid-api-key-fault-response apiproxy-dir)
        (apigee--insert-policy-expired-api-key-fault-response apiproxy-dir)

        (with-temp-file (concat apiproxy-dir "proxies/default.xml")
          (insert
           "<ProxyEndpoint name='default'>
  <Description>Default Proxy</Description>
  <HTTPProxyConnection>
    <BasePath>/" proxy-name "</BasePath>
    <Properties/>
    <!-- <VirtualHost>default</VirtualHost> -->
    <VirtualHost>secure</VirtualHost>
  </HTTPProxyConnection>

  <FaultRules>
    <FaultRule name='invalid-key'>
      <Step>
        <Name>AM-InvalidApiKey</Name>
      </Step>
      <Condition>fault.name = \"InvalidApiKeyForGivenResource\" OR fault.name = \"InvalidApiKey\" OR fault.name = \"DeveloperStatusNotActive\" OR fault.name = \"invalid_client-app_not_approved\"</Condition>
    </FaultRule>

    <FaultRule name='expired-key'>
      <Step>
        <Name>AM-ExpiredApiKey</Name>
      </Step>
      <Condition>fault.name = \"consumer_key_expired\"</Condition>
    </FaultRule>
  </FaultRules>

  <PreFlow name='PreFlow'>
      <Request/>
      <Response/>
  </PreFlow>
  <PostFlow name='PostFlow'>
      <Request/>
      <Response>
        <Step><Name>AM-CleanResponseHeaders</Name></Step>
      </Response>
  </PostFlow>

  <Flows>
    <Flow name='test 1'>
      <Description>insert description here</Description>
      <Request>
        <!-- insert flow-specific policies here -->
      </Request>
      <Response>
        <!-- and others here -->
      </Response>
      <Condition>(proxy.pathsuffix MatchesPath \"/t1\") and (request.verb = \"GET\")</Condition>
    </Flow>

    <Flow name='unknown request'>
      <Request>
        <Step><Name>RF-UnknownRequest</Name></Step>
      </Request>
      <Response/>
    </Flow>

  </Flows>

  <!-- keep this if using a target -->
  <RouteRule name='InvokeRouteRule'>
    <TargetEndpoint>default</TargetEndpoint>
  </RouteRule>

  <!-- keep this if no target (eg, for oauth token generation and refresh) -->
  <RouteRule name='NoRouteRule'/>

</ProxyEndpoint>\n"))

        (find-file-existing apiproxy-dir)
        ))))



(defun apigee--snippet-field (field-num)
  "returns the FIELD-NUMth field from the currently
active YAS snippet. This is a utility fn for use within
apigee snippets, to allow expansion for field (N) to depend on the
value that was expanded for field (N-1). "
  (nth (- field-num 1) (yas--snippet-fields snippet)))


(defun apigee--fixup-script-name (name &optional prefix)
  "returns a stripped name suitable for use for a file in the resources/jsc directory,
or resources/py, or resources/xsl."

  (let* ((default-prefix "Javascript")
         (real-prefix (concat (downcase (if (stringp prefix) prefix default-prefix)) "-"))
         (pos (length real-prefix)))
    (if (and (>= (length name) (length real-prefix))
             (string= real-prefix (downcase (substring name 0 pos))))
        (let ((s (substring name pos)))
          (concat (downcase (substring s 0 1)) (substring s 1)))
      name)))


(defun apigee-get-menu-position ()
  "get the position for the popup menu"
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (point)))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))


(defun apigee--remove-string-duplicates (list)
  "Given a LIST of strings, generate a new list whose members
consist only of the unique members of the original list.
"
  (let ((new-list nil))
    (while list
      (when (and (car list) (not (member (car list) new-list)))
        (setq new-list (cons (car list) new-list)))
      (setq list (cdr list)))
    (nreverse new-list)))

(defun apigee--sort-strings (strings)
  "lexicographically sort a list of strings"
  (sort (copy-sequence strings)
        (lambda (a b) (string<  a b ))))


(defun apigee--policy-type-for-menu-item (candidate)
  "returns the policy type for the given menu ITEM."
  (car (split-string (car candidate))))


;; (defun apigee--menu-item (candidate)
;;   "works for `x-popup-menu'"
;;   (list (car candidate) candidate))


;; (defun apigee--generate-menu (candidates)
;;   "Generate a menu suitable for use in `x-popup-menu' from the
;; list of candidates. Each item in the list of candidates is a
;; list, (KEY TEMPLATE), where KEY is one of {Quota, XMLToJSON,
;; Javascript, etc}, TEMPLATE is the template to fill in a new policy file.
;;
;; The output is a multi-leveled hierarchy, like this:
;;
;;   (\"Insert a policy...\"
;;     (\"AssignMessage\"
;;       (\"AssignMessage - remove query param\" 1 )
;;       (\"AssignMessage - set header\" 2 ))
;;     (\"AccessEntity\"
;;       (\"AccessEntity - developer\" 3)
;;       (\"AccessEntity - app\" 4))
;;       ....)
;;
;; "
;;   (let ((categories (apigee--sort-strings
;;                      (apigee--remove-string-duplicates
;;                       (mapcar 'apigee--policy-type-for-menu-item candidates))))
;;         menu)
;;
;;     (while categories
;;       (let ((cat (car categories))
;;             (n 0)
;;             (len (length candidates))
;;             pane-items)
;;
;;         (while (< n len)
;;           (let ((candidate (nth n candidates)))
;;             (if (string= cat (apigee--policy-type-for-menu-item candidate))
;;                  (setq pane-items (cons (list (car candidate) n) pane-items))))
;;           (setq n (1+ n)))
;;
;;         (setq pane-items (nreverse pane-items))
;;         (setq menu (cons
;;                     (if (> (length pane-items) 1)
;;                         (cons cat pane-items)
;;                       (car pane-items))
;;                     menu))
;;       (setq categories (cdr categories))))
;;
;;     ;; this works with x-popup-menu
;;     (cons "Insert a policy..." (nreverse menu))))

(defun apigee--generate-policy-menu (candidates)
  "From the list of candidates, generate a keymap suitable for
use as a menu in `popup-menu' . Each item in the list of
candidates is a list, (KEY TEMPLATE), where KEY is one of {Quota,
XMLToJSON, Javascript, etc}, TEMPLATE is the template to fill in
a new policy file.

The intention is to display a cascading (multi-level) popup menu.

The output is a multi-leveled hierarchy, like this:

   (\"Insert a policy...\"
     (\"AssignMessage\"
       (\"AssignMessage - remove query param\" 1 )
       (\"AssignMessage - set header\" 2 ))
     (\"AccessEntity\"
       (\"AccessEntity - developer\" 3)
       (\"AccessEntity - app\" 4))
       ....)

"
  (let ((categories (nreverse (apigee--sort-strings
                     (apigee--remove-string-duplicates
                      (mapcar 'apigee--policy-type-for-menu-item candidates)))))
        (keymap (make-sparse-keymap "Insert a policy...")))

    (while categories
      (let ((cat (car categories))
            (n 0)
            (len (length candidates))
            pane-items
            (sort-by-name (lambda (a b) (not (string< (downcase (car a)) (downcase (car b)))))))

        (while (< n len)
          (let ((candidate (nth n candidates)))
            (if (string= cat (apigee--policy-type-for-menu-item candidate))
                 (setq pane-items (cons (list (car candidate) n) pane-items))))
          (setq n (1+ n)))

        ;; sort by description here
        (setq pane-items (sort pane-items sort-by-name))

        (if (= (length pane-items) 1)
            (let ((item (car pane-items)))
              (define-key keymap (vector (cadr item)) item))
          (define-key keymap
            (vector (intern cat))
            (cons cat (make-sparse-keymap cat)))
          (while pane-items
            (let ((item (car pane-items)))
              (define-key keymap
                (vector (intern cat) (cadr item))
                item))
            (setq pane-items (cdr pane-items))))

      (setq categories (cdr categories))))

    ;; this works with popup-menu
    keymap))


(defun apigee--generate-target-menu (candidates)
  "From the list of candidates, generate a keymap suitable for
use as a menu in `x=popup-menu' . Each item in the list of
candidates is a list, (KEY TEMPLATE), where KEY is one of
{\"ScriptTarget\", \"HttpTarget\"}, and TEMPLATE is the template
to fill in a new target file.

The intention is to display a single-level popup menu.

The output is a single-level hierarchy, like this:

   (\"Insert a target...\"
     (\"HttpTarget\" ... )
     (\"ScriptTarget\" ... )
   )
"
  (let ((keymap (make-sparse-keymap "Insert a target...")))

    (let ((n 0)
          (limit (length candidates))
          (tags (mapcar 'car candidates)))

      ;; sort by tag name here
      (setq tags
            (sort tags
                  (lambda (a b) (not (string< (downcase a) (downcase  b) )) )))

      (while (< n limit)
        (let* ((tag (nth n tags))
               (item (assoc tag candidates)))
          (define-key keymap
            (vector (intern (car item)))
            (cons (car item) n)))
        (setq n (1+ n)))

    ;; this works with popup-menu
    keymap)))



(defun apigee-prompt-user-with-policy-choices ()
  "Prompt the user with the available choices.
In this context the available choices is the hierarchical list
of available policies.
"
    ;; NB:
    ;; x-popup-menu displays in the proper location, near
    ;; the cursor.
    ;;
    ;; x-popup-dialog always displays in the center
    ;; of the frame, which makes for an annoying
    ;; user-experience.
    (x-popup-menu (apigee-get-menu-position)
                  (apigee--generate-policy-menu apigee--policy-alist)))


(defun apigee-prompt-user-with-target-choices ()
  "Prompt the user with the available choices for targets.
In this context the available choices is the list
of available target templates.
"
    ;; NB:
    ;; x-popup-menu displays in the proper location, near
    ;; the cursor.
    ;;
    ;; x-popup-dialog always displays in the center
    ;; of the frame, which makes for an annoying
    ;; user-experience.
    (x-popup-menu (apigee-get-menu-position)
                  (apigee--generate-target-menu apigee--target-template-alist)))


(defun apigee-add-target ()
  "Invoke this interactively, and the fn will prompt the user to
specify the name of a new target. The fn will then create the appropriate
XML file, and using yas-snippet, will prompt the user to fill in appropriate
information for the target.

"
  (interactive)
  (let ((apiproxy-dir (apigee-path-of-apiproxy)))
    (if apiproxy-dir
        ;; Here, prompt user for options for targets.
        ;; scripttarget, or regular.
        (let* ((choice (apigee-prompt-user-with-target-choices))
               (tag (symbol-name (car choice)))
               (chosen (assoc tag apigee--target-template-alist)))
          (when chosen
            (let ((targets-dir (concat apiproxy-dir "apiproxy/targets/"))
                  (have-name nil)
                  (raw-template (cadr chosen))
                  (target-name-prompt "target name: "))

              ;; insure exists
              (and (not (file-exists-p targets-dir))
                   (make-directory targets-dir))

              (let* ((default-value (apigee--suggested-target-name))
                     (target-name
                      (let (n)
                        (while (not have-name)
                          (setq n (read-string target-name-prompt default-value nil default-value)
                                have-name (apigee-target-name-is-available n)
                                target-name-prompt "That name is in use. Target name: " ))
                        n))

                     (elaborated-template
                      (progn
                        (while (string-match "##" raw-template)
                          (setq raw-template
                                (replace-match target-name t t raw-template)))
                        raw-template)))

                ;; create the file, expand the snippet, save it.
                (find-file (concat targets-dir target-name ".xml"))
                ;; yas-expand-snippet-sync does not return until the snip is expanded.
                (yas-expand-snippet-sync elaborated-template )
                (save-buffer)
                (apigee-mode 1)

                ;; TODO: search in default proxy and add the target to the
                ;; top of the list of targets.
                (kill-new (concat "<RouteRule name='" target-name "-rule'>
  <TargetEndpoint>" target-name "</TargetEndpoint>
</RouteRule>"))
                (message "yank to add the routerule...")
                )))))))




;;;###autoload
(defun apigee-add-policy ()
  "Invoke this interactively, and the fn will prompt the user to
choose a policy type to insert. It will then ask for a name for
the policy, create the appropriate XML file, and using
yas-snippet, expand the template associated to the chosen policy,
into the policy file. It then will open any resource files as
appropriate.

"
  (interactive)
  (let ((apiproxy-dir (apigee-path-of-apiproxy)))
    (if apiproxy-dir
        (let* ((choice (apigee-prompt-user-with-policy-choices))
               (chosen (nth (elt choice (1- (length choice))) apigee--policy-alist)))
          (when chosen
            (let ((policy-dir (concat apiproxy-dir "apiproxy/policies/"))
                  (ptype (cadr chosen))
                  (have-name nil)
                  (policy-name-prompt "policy name: ")
                  (raw-template (caddr chosen)))

              (and (not (file-exists-p policy-dir))
                   (make-directory policy-dir))

              (let* ((default-value (apigee--suggested-policy-name ptype))
                     (policy-name
                      (let (n)
                        (while (not have-name)
                          (setq n (read-string policy-name-prompt default-value nil default-value)
                                have-name (apigee-policy-name-is-available n)
                                policy-name-prompt "That name is in use. Policy name: " ))
                        n))

                     (elaborated-template
                      (progn
                        (while (string-match "##" raw-template)
                          (setq raw-template
                                (replace-match policy-name t t raw-template)))
                        raw-template)))

                ;; create the file, expand the snippet, save it.
                (find-file (concat policy-dir policy-name ".xml"))
                ;; yas-expand-snippet-sync does not return until the snip is expanded.
                (yas-expand-snippet-sync elaborated-template (point) (point))
                (save-buffer)
                (apigee-mode 1)

                ;; here, optionally open the resource file, if any
                (cond
                 ((or (string= ptype "Javascript") (string= ptype "XSL") (string= ptype "Python"))
                    (save-excursion
                      (goto-char (point-min))
                      (if (re-search-forward "<ResourceURL>\\(jsc\\|xsl\\|py\\)://\\(.+\\)</ResourceURL>" (point-max) t)
                          (let ((resource-type (match-string-no-properties 1))
                                (resource-basename (match-string-no-properties 2)))
                            (if resource-basename
                                (let ((resource-dir
                                       (concat apiproxy-dir "apiproxy/resources/" resource-type "/")))
                                  (and (not (file-exists-p resource-dir))
                                       (make-directory resource-dir))
                                  (find-file-other-window (concat resource-dir resource-basename))
                                  (apigee--maybe-insert-base-content resource-basename resource-type)
                                  (apigee-mode 1)))))))

                 (t nil))

                (kill-new policy-name)
                (kill-new
                 (concat "<Step><Name>" policy-name "</Name></Step>"))
                (message "yank to add the step declaration...")
                )))))))


(defun apigee--maybe-insert-base-content (rsrc-basename rsrc-type)
  "maybe inserts some base content if the resource is a special and known name."
  (cond
   ((string= (downcase rsrc-basename) "stripsoap.xsl")
    (delete-region (point-min) (point-max))
    (insert
     "<xsl:stylesheet version='1.0'
                xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
                xmlns:soapenv='http://schemas.xmlsoap.org/soap/envelope/'
                exclude-result-prefixes='soapenv'>

  <xsl:strip-space elements='*'/>
  <xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>

  <!-- remove all elements in the soapenv namespace -->
  <xsl:template match='soapenv:*'>
    <xsl:apply-templates select='node()'/>
  </xsl:template>

  <!-- for the remaining elements (i.e. elements in the default namespace) ... -->
  <xsl:template match='*'>
    <!-- ... create a new element with similar name in no-namespace -->
    <xsl:element name='{local-name()}'>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:element>
  </xsl:template>

  <!-- also, convert all XML attributes to elements -->
  <xsl:template match='@*'>
    <xsl:element name='{local-name()}'>
      <xsl:value-of select='.' />
    </xsl:element>
  </xsl:template>

</xsl:stylesheet>"))
   (t nil)))



;;;###autoload
(defun apigee-open-resource-file-around-point ()
  "Opens the resource file that is specified on the current line.
This can be used interactively when editing a policy that includes a resource
file, such as a Javascript, Python, or XSL policy.
"
  (interactive)
  (let ((fullpath
    (save-excursion
      (goto-char (point-at-bol))
      (if (re-search-forward "<ResourceURL>\\(jsc\\|xsl\\)://\\(.+\\)</ResourceURL>" (point-max) t)
          (let ((resource-type (match-string-no-properties 1))
                (resource-basename  (match-string-no-properties 2))
                resource-dir)
            (setq resource-dir
                  (concat (apigee-path-of-apiproxy) "apiproxy/resources/" resource-type))
            (and (not (file-exists-p resource-dir)) (make-directory resource-dir))
            (concat resource-dir "/" resource-basename))))))
    (and fullpath (find-file fullpath))))


(defconst apigee-policy-help-alist
  (list
   '("AccessEntity"
     "/api-services/content/retrieve-entity-profiles-using-accessentity")
   '("AssignMessage"
     "/api-services/reference/assign-message-policy")
   '("KeyValueMapOperations"
     "/api-services/content/persist-data-using-keyvaluemap")
   '("GetOAuthV2Info"
     "/api-services/reference/get-oauth-v2-info-policy")
   '("SetOAuthV2Info"
     "/api-services/reference/set-oauth-v2-info-policy")
   '("ExtractVariables"
     "/api-services/content/extract-message-content-using-extractvariables")
   '("ResponseCache"
     "/api-services/content/reduce-latency-using-responsecache")
   '("PopulateCache"
     "/api-services/content/optimize-performance-using-cache")
   '("InvalidateCache"
     "/api-services/content/optimize-performance-using-cache")
   '("LookupCache"
     "/api-services/content/optimize-performance-using-cache")
   '("Javascript"
     "/api-services/reference/javascript-policy")
   '("Python"
     "/api-services/reference/python-script-policy")
   '("OAuthV2"
     "/api-services/content/oauthv2-policy")
   ;; "/api-services/content/authorize-requests-using-oauth-20")
   '("Java"
     "/api-services/reference/java-callout-policy")
   '("JSONToXML"
     "/api-services/reference/json-xml-policy")
   '("XMLToJSON"
     "/api-services/reference/xml-json-policy")
   '("XSL"
     "/api-services/reference/xsl-transform-policy")
   '("JSONThreatProtection"
     "/api-services/reference/json-threat-protection-policy")
   '("ServiceCallout"
     "/api-services/reference/service-callout-policy")
   '("Quota"
     "/api-services/content/rate-limit-api-traffic-using-quota"))

  "list of urls for help, associated to each policy type")


(defun apigee--help-page-for-policy-type (policy-type)
  "Return a URL for help given the POLICY-TYPE."
  (concat "http://apigee.com/docs"
        (or
         (cadr (assoc policy-type apigee-policy-help-alist))
          "/content/policy-reference-overview")))


(defun apigee-open-help-intelligently ()
  "interactive fn to open help for the policy in the current buffer."
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (and (looking-at "[:space:]*<\\?xml")
         (re-search-forward "\\?>"))
    (forward-word)
    (backward-word)
    (let ((help-page (apigee--help-page-for-policy-type (thing-at-point 'word))))
        (and help-page
             (start-process (concat "open " help-page) nil "open" help-page)))))


;; using API to update ONE resource
(defun apigee--join-path-elements (root &rest dirs)
  "Joins a series of directories together, inserting slashes as necessary,
like Python's os.path.join."

  (if (not dirs)
      root
    (apply 'apigee--join-path-elements
           (let ((first (car dirs)))
             (if (s-suffix? "/" root)
                 (concat root
                         (if (s-prefix? "/" first)
                             (substring first 1 (length first))
                           first))
               (if (s-prefix? "/" first)
                   (concat root first)
                 (concat root "/" (car dirs)))))
           (cdr dirs))))


(defun apigee-update-resource-do-request (org api revision rsrc-type rsrc-name &optional mgmt-server)
  "updates a resource in an API proxy. MGMT-SERVER should
include the scheme and domain name. If not passed or not a string, then the
value defaults to https://api.enterprise.apigee.com
"
  ;; get the mgmt server
  (let ((default-mgmt "https://api.enterprise.apigee.com"))
    (if (not (stringp mgmt-server))
        (setq mgmt-server default-mgmt)))

  (let ((hostname
         (and
          (numberp (string-match "\\<https?://\\([^/]+\\)" mgmt-server))
          (match-string 1 mgmt-server)))
        (rsrc-url (apigee--join-path-elements
                   mgmt-server
                   "/v1/o" org "apis" api "revisions" revision "resources" rsrc-type rsrc-name))
        (record nil))

    (and hostname

         (if (setq record (dino-netrc-find hostname))
             (progn
               (request
                rsrc-url
                :headers `(("authorization" . ,(dino-netrc-basic-auth-header (cdr record)))
                           ("content-type" . "application/octet-stream")
                           ("accept" . "application/json"))
                :type "PUT"
                :data (buffer-substring-no-properties (point-min) (point-max))
                :parser 'buffer-string
                :complete (function*
                           (lambda (&key response &allow-other-keys)
                             (let ((status (request-response-status-code response))
                                   (data (request-response-data response))
                                   (e-thrown (request-response-error-thrown response)))
                               (message "COMPLETE Got status: %d" status)
                               (if data
                                 (with-current-buffer (get-buffer-create "*request response*")
                                   (erase-buffer)
                                   (insert data)
                                   (goto-char (point-min))
                                   ;; on success, the response looks like:
                                   ;; {
                                   ;;   "name" : "model.json",
                                   ;;   "type" : "node"
                                   ;; }
                                   (setq data (json-read))
                                   ;;(pop-to-buffer (current-buffer))
                                   (message "Status: %d  Data: %s" status (prin1-to-string data))
                                   )
                                 (message "Response: %s" (prin1-to-string response))

                                 ))))
                :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                                    (message "Got error: %S" error-thrown))))
               t)
           (message "No record in .netrc found for host %s" hostname))
         )))

(defun apigee-retrieve-resource-do-request (org api revision rsrc-type rsrc-name &optional mgmt-server)
  "retrieves a resource from an API proxy. MGMT-SERVER should
include the scheme and domain name. If not passed or not a string, then the
value defaults to https://api.enterprise.apigee.com
"
  ;; get the mgmt server
  (let ((default-mgmt "https://api.enterprise.apigee.com"))
    (if (not (stringp mgmt-server))
        (setq mgmt-server default-mgmt)))

  (let ((hostname
         (and
          (numberp (string-match "\\<https?://\\([^/]+\\)" mgmt-server))
          (match-string 1 mgmt-server)))
        (rsrc-url (apigee--join-path-elements
                   mgmt-server
                   "/v1/o" org "apis" api "revisions" revision "resources" rsrc-type rsrc-name))
        (record nil))

    (and hostname
         (if (setq record (dino-netrc-find hostname))
             (request
              rsrc-url
              :headers `(("authorization" . ,(dino-netrc-basic-auth-header (cdr record)))
                         ("accept" . "application/json"))
              :type "GET"
              :data (buffer-substring-no-properties (point-min) (point-max))
              :parser 'buffer-string
              :sync 't
              )
           (message "No record in .netrc found for host %s" hostname)
           nil)
         )))

;; to upload a modified jar file:
;;
;; curl -i -X PUT -n \
;;   -H content-type:application/octet-stream \
;;   -H accept:application/json \
;;   https://api.enterprise.apigee.com/v1/o/iloveapis2015/apis/jwt_signed/revisions/7/resources/java/jwt-edge-callout.jar \
;;   --upload-file ../apiproxy/apiproxy/resources/java/jwt-edge-callout.jar
;;
;; will also need to undeploy and redeploy after doing this....
;;
;; to download a jar file:
;;
;; curl -i -X GET -n \
;;   https://api.enterprise.apigee.com/v1/o/iloveapis2015/apis/jwt_signed/revisions/7/resources/java/jwt-edge-callout.jar
;;
;; will also need to undeploy and redeploy after doing this....

(defun apigee--get-mgmt-server (iactive)
  "prompts the user for the Edge management server."
  (let ((default-mgmt "https://api.enterprise.apigee.com"))
    (if apigee-cached-mgmt-server
        (setq default-mgmt apigee-cached-mgmt-server))

    (if iactive ;; implies the caller was called interactively
        (let ((prompt "Edge mgmt server API endpoint: ")
              (good-answer nil)
              n)
          (while (not good-answer)
            (setq n (read-string prompt default-mgmt nil default-mgmt)
                  good-answer (or (s-prefix? "http://" n)
                                  (s-prefix? "https://" n))
                  prompt "Provide a valid Edge Mgmt API Endpoint: " ))
          n)
      default-mgmt)))


(defun apigee--get-org-name (iactive)
  "prompts the user for the Edge organization to use."
  (let (org
        (prompt "organization: ")
        (default-org (or apigee-cached-org-name "unknown")))
    (setq org
          (if iactive
              (read-string prompt default-org nil default-org)
            default-org))
    (if (or (not org)
            (not (stringp org))
            (< (length org) 1))
        (error "no organization name")
      org)))

(defun apigee--get-list-from-edge-api (rsrc-url)
  "uses the Edge management API to return a list of ... something.
apiproxies, revisions, resources, etc. This requires netork access.
It will return in about 1 second."
  (and
   (let ((hostname
          (and
           (numberp (string-match "\\<https?://\\([^/]+\\)" rsrc-url))
           (match-string 1 rsrc-url)))
         (record nil))

     (and hostname
          (if (setq record (dino-netrc-find hostname))
              (let ((result
                     (request
                      rsrc-url
                      :headers `(("authorization" . ,(dino-netrc-basic-auth-header (cdr record)))
                                 ("accept" . "application/json"))
                      :type "GET"
                      :parser 'buffer-string
                      :sync 't
                      :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                                          (message "Got error: %S" error-thrown))))))
                (if result
                    (with-current-buffer (get-buffer-create "*request response*")
                      (erase-buffer)
                      (insert (aref result 3))
                      (goto-char (point-min))
                      (let ((json-array-type 'list))
                        ;;(pop-to-buffer (current-buffer))
                        (setq result (apigee--sort-strings (json-read)))))))
            (message "No record in .netrc found for host %s" hostname)
            nil)
          ))))


(defun apigee--list-apiproxies (mgmt-server org)
  "uses the Edge management API to return the list of API proxies at a given org.
This requires network access and it will return in about 1 second."
  ;; (apigee--list-apiproxies "https://api.enterprise.apigee.com" "cheeso")
  (and
   (stringp mgmt-server)
   (let ((rsrc-url (apigee--join-path-elements mgmt-server "/v1/o" org "apis")))
     (apigee--get-list-from-edge-api rsrc-url))))


(defun apigee--list-apiproxy-revisions (mgmt-server org apiproxy)
  "uses the Edge management API to return the list of revisions
for a given API proxy at a given org. This requires network
access and it will return in about 1 second."
  (and
   (stringp mgmt-server)
   (let ((rsrc-url (apigee--join-path-elements mgmt-server "/v1/o" org "apis" apiproxy "revisions")))
     (apigee--get-list-from-edge-api rsrc-url))))

(defun apigee--list-apiproxy-resources (mgmt-server org apiproxy revision)
  "uses the Edge management API to return the list of resources
for a given revision of an API proxy at a given org. This
requires network access and it will return in about 1 second."
  ;; (apigee--list-apiproxy-resources "https://api.enterprise.apigee.com" "cheeso" "runload2" "1")
  (and
   (stringp mgmt-server)
   (let ((rsrc-url (apigee--join-path-elements mgmt-server "/v1/o" org "apis" apiproxy "revisions" revision "resources")))
     (apigee--get-list-from-edge-api rsrc-url))))

(defun apigee--get-menu-structure-for-list (menu-title candidates)
  "generate a keymap from a simple list of strings"
  (let (item item-list)
    (while candidates
      (setq item (car candidates))
      (setq candidates (cdr candidates))
      (setq item-list (cons (cons item item) item-list)))

    (list menu-title (cons menu-title (nreverse item-list)))))



(defun apigee--get-apiproxy-name (mgmt-server org)
  "prompts the user for the apiproxy name. Uses `x-popup-menu'
to present the list of apiproxies from the given org.
"
  ;; (apigee--get-apiproxy-name "https://api.enterprise.apigee.com" "cheeso")

  (let ((menu-struct
         (apigee--get-menu-structure-for-list
          "Select an apiproxy..."
          (apigee--list-apiproxies mgmt-server org))))
    (x-popup-menu (apigee-get-menu-position) menu-struct)))


(defun apigee--get-apiproxy-revision (mgmt-server org apiproxy)
  "prompts the user for the revision of the named apiproxy. Uses `x-popup-menu'
to present the list of revisions. If there is only one option,
just returns that item."

  ;; (apigee--get-apiproxy-revision "https://api.enterprise.apigee.com" "cheeso" "runload2")
  (let ((revisions (apigee--list-apiproxy-revisions mgmt-server org apiproxy)))
    (and revisions
         (if (eq (length revisions) 1)
             (car revisions)
           (x-popup-menu (apigee-get-menu-position)
                         (apigee--get-menu-structure-for-list
                          "Select a revision..." revisions))))))


(defun apigee--get-apiproxy-revision-resource (mgmt-server org apiproxy revision)
  "prompts the user for a resource available in the given
revision of the named apiproxy. Uses `x-popup-menu' to present
the list of resources. If there is only one option, just returns
that item."
  ;; https://api.enterprise.apigee.com/v1/o/cheeso/apis/runload2/revisions/1/resources
  ;; (apigee--get-apiproxy-revision-resource "https://api.enterprise.apigee.com" "cheeso" "runload2" "1")
  (let ((resources (apigee--list-apiproxy-resources mgmt-server org apiproxy revision)))
    (cond
     ((not resources)
      (error "There are no resources available in that apiproxy."))
     ((eq (length resources) 1)
      (message "selecting resource %s" (car resources))
      (car resources))
     (t
      (x-popup-menu (apigee-get-menu-position)
                    (apigee--get-menu-structure-for-list
                     "Select a resource..." resources))))))

(defun apigee--resource-type (resource-path)
  "returns the resource type, one of (\"node\" \"xsl\" \"java\" \"jsc\"),
given the resource path. For example, given \"node://model.json\", returns \"node\" "
  (let ((re "\\<\\([a-z]+\\)://\\(.+\\)"))
    (and resource-path
         (string-match re resource-path)
         (match-string 1 resource-path))))

(defun apigee--resource-name (resource-path)
  "returns the resource name given the resource path. For example, given
\"node://model.json\", returns \"model.json\" "
  (let ((re "\\<\\([a-z]+\\)://\\(.+\\)"))
    (and resource-path
         (string-match re resource-path)
         (match-string 2 resource-path))))

(defun apigee--apply-appropriate-mode (rsrc-type)
  "sets the appropriate mode for the newly created buffer"
  (cond
   ((string= rsrc-type "xsl")
    (xml-mode))
   ((string= rsrc-type "java")
    (java-mode))
   ((string= rsrc-type "jsc")
    (js-mode))
   ((string= rsrc-type "node")
    (js-mode))
   (t nil)))



(defun apigee-maybe-sync-policy-filename ()
  "synchronizes the name of the file with the name specified in the name
attribute in the root element, if the file is a policy file.

This function is normally used as an after-save-hook, and normally only in an
xml-mode (such as `nxml-mode'), since policy files are XML files.

With this hook, the human can modify the name attribute inside the XML, and
save the file; and then that change gets reflected in the filename for the buffer
and also in every other file in the API-proxy bundle.
"
  (interactive)
  (let ((orig-filename (buffer-file-name)))
    (if (apigee--is-policy-file orig-filename)
        (let* ((orig-policyname (file-name-sans-extension (file-name-nondirectory orig-filename)))
               (root (xml-parse-region))
               (policy (car root))
               (attrs (xml-node-attributes policy))
               (new-policyname (cdr (assq 'name attrs))))
          (if (and new-policyname
                   (not (s-equals-p orig-policyname new-policyname)))
              (let* ((new-short-filename (concat new-policyname ".xml"))
                     (new-filename
                      (concat (file-name-directory orig-filename) new-short-filename)))
                (rename-file orig-filename new-filename 1)
                (rename-buffer new-short-filename t) ;; get unique buffer name
                (set-visited-file-name new-filename) ;; sets modified flag t
                (set-buffer-modified-p nil)
                ;; now, search/replace all files in the apiproxy to replace
                ;; that old policy name with the new policy name.
                (let ((policy-buffer-name (buffer-name))
                      (file-list (apigee-all-files-in-apiproxy))
                      (re1 (concat "\\<" (regexp-quote orig-policyname) "\\>")))
                  (while file-list
                    (let* ((one-file (car file-list))
                           (base-filename (file-name-nondirectory one-file)))
                      (if (not (or (s-starts-with? "#" base-filename)
                                   (s-starts-with? ".#" base-filename)))
                          (with-current-buffer (find-file-noselect one-file)
                            (save-excursion
                              (goto-char (point-min))
                              (let ((need-save nil)
                                    (original-modified (buffer-modified-p)))
                                (while (re-search-forward re1 nil t)
                                  (replace-match new-policyname)
                                  (setq need-save t))
                                (if (and need-save
                                         (not original-modified)
                                         (not (s-equals-p policy-buffer-name (buffer-name))))
                                    (save-buffer))))))
                      (setq file-list (cdr file-list)))))))))))

(add-hook
 'nxml-mode-hook
 (lambda ()
   (let ((BUFFER-LOCAL t) (APPEND t))
     (add-hook 'after-save-hook 'apigee-maybe-sync-policy-filename APPEND BUFFER-LOCAL))))


(defun apigee-retrieve-resource ()
  "retrieve a particular resource in Edge via the API. See also
`apigee-update-current-resource' . "
  (interactive)
  (let (mgmt-server org apiproxy revision resource rsrc-type rsrc-name buf1
                    (iactive (called-interactively-p 'any)))

    ;; get a new buffer
    (with-current-buffer (generate-new-buffer "*apigee*")
      (setq buf1 (current-buffer))
      (erase-buffer)
      (setq buffer-file-name nil)
      (goto-char (point-min))
      (normal-mode t)
      (setq mgmt-server (apigee--get-mgmt-server iactive)
            org (apigee--get-org-name iactive)))

    (setq apiproxy (apigee--get-apiproxy-name mgmt-server org)
          revision (apigee--get-apiproxy-revision mgmt-server org apiproxy)
          resource (apigee--get-apiproxy-revision-resource mgmt-server org apiproxy revision)
          rsrc-type (apigee--resource-type resource)
          rsrc-name (apigee--resource-name resource))

    ;; validate the resource type
    (if (or (not rsrc-type)
            (not (member rsrc-type '("xsl" "node" "jsc" "java"))))
        (error (format "Unexpected resource type: %s" (or rsrc-type "??"))))

    (message "getting %s/apis/%s/revisions/%s/resources/%s/%s"
             org apiproxy revision rsrc-type rsrc-name)

    ;; invoke the API call
    (let ((result
           (apigee-retrieve-resource-do-request org apiproxy revision rsrc-type
                                                rsrc-name mgmt-server)))
      (if result
          (with-current-buffer buf1
            (goto-char (point-min))
            (insert (aref result 3))
            (goto-char (point-min))
            (rename-buffer (format "%s-%s-%s-%s" org apiproxy revision resource) t)

            (make-local-variable 'apigee-cached-mgmt-server)
            (make-local-variable 'apigee-cached-org-name)
            (make-local-variable 'apigee-cached-apiproxy)
            (make-local-variable 'apigee-cached-revision)
            (make-local-variable 'apigee-cached-rsrc-type)
            (make-local-variable 'apigee-cached-rsrc-name)

            (setq buffer-file-name rsrc-name
                  default-directory "~/"
                  apigee-cached-mgmt-server mgmt-server
                  apigee-cached-org-name org
                  apigee-cached-apiproxy apiproxy
                  apigee-cached-revision revision
                  apigee-cached-rsrc-type rsrc-type
                  apigee-cached-rsrc-name rsrc-name
                  )
            (run-hooks 'find-file-hook)
            (apigee--apply-appropriate-mode rsrc-type)
            (apigee-mode t)
            (pop-to-buffer (current-buffer))
            )))))



(defun apigee-update-current-resource ()
  "update the current resource in Edge via the API. See also
`apigee-retrieve-resource' ."
  (interactive)
  (let (org apiproxy (revision "1") rsrc-type rsrc-name mgmt-server
            (iactive (called-interactively-p 'any)))

            (make-local-variable 'apigee-cached-mgmt-server)
            (make-local-variable 'apigee-cached-org-name)
            (make-local-variable 'apigee-cached-apiproxy)
            (make-local-variable 'apigee-cached-rsrc-type)
            (make-local-variable 'apigee-cached-rsrc-name)
            (make-local-variable 'apigee-cached-revision)

    (setq mgmt-server (or apigee-cached-mgmt-server
                          (apigee--get-mgmt-server iactive))
          org (or apigee-cached-org-name
                  (apigee--get-org-name iactive))
          apiproxy (or apigee-cached-apiproxy (apigee-apiproxy-name))
          rsrc-type (or apigee-cached-rsrc-type
                        (file-name-nondirectory
                         (apigee-insure-no-trailing-slash
                          (file-name-directory default-directory))))
          rsrc-name (or apigee-cached-rsrc-name
                        (file-name-nondirectory buffer-file-name)))

    ;; validate the resource type
    (if (or (not rsrc-type)
            (not (member rsrc-type '("xsl" "node" "jsc" "java"))))
        (error (format "Unexpected resource type: %s" (or rsrc-type "??"))))

    ;; confirm revision
    (if apigee-cached-revision
        (setq revision apigee-cached-revision)
      (if (called-interactively-p 'any)
          (let ((prompt "revision: "))
            (setq revision (read-string prompt revision nil revision)))))

    (if (or (not revision)
            (not (> (string-to-number revision) 0)))
        (error "invalid revision"))

    (message "updating %s/apis/%s/revisions/%s/resources/%s/%s"
             org apiproxy revision rsrc-type rsrc-name)

    ;; invoke the API call
    (apigee-update-resource-do-request org apiproxy revision rsrc-type rsrc-name mgmt-server)

    ;; cache the values for next time
    (setq apigee-cached-mgmt-server mgmt-server
          apigee-cached-org-name org
          apigee-cached-apiproxy apiproxy
          apigee-cached-rsrc-type rsrc-type
          apigee-cached-rsrc-name rsrc-name
          apigee-cached-revision revision)

    ))


(define-minor-mode apigee-mode
  "When invoked with no argument, toggle Apigee Edge mode.

Invoke with t to turn apigee-mode on; nil to turn off.

The mode provides functions specific to Apigee Edge when editing files
for API Proxies. This includes policy, proxy, resource, and target files. "
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Apigee"
  ;; The minor mode bindings.
  :keymap
  '(([f7] . apigee-open-resource-file-around-point)
    ([f8] . apigee-upload-bundle-with-pushapi)
    ("\C-c?" . apigee-open-help-intelligently)
    ("\C-ch" . apigee-open-help-intelligently)
    ("\C-cu" . apigee-update-current-resource)
    ("\C-cr" . apigee-retrieve-resource)
    ("\C-cp" . apigee-upload-bundle-with-pushapi)
    )
  :version "1.4"
  :group 'apigee

  ;; body code that runs each time mode is enabled or disabled
  (progn
    ;; set up buffer-local variables
    (if apigee-mode
        (progn
          (make-local-variable 'apigee-cached-mgmt-server)
          (make-local-variable 'apigee-cached-org-name)))))


(provide 'apigee)

;;; apigee.el ends here
