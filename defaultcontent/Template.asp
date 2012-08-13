<%@ language="Javascript" %>

<script language="javascript" runat="server" src='json2.js'></script>
<script language="javascript" runat="server" src='asp.utils.js'></script>

<script language="javascript" runat="server">

(function() {
    processRequest = function() {
        var method = (Request.ServerVariables('REQUEST_METHOD') + '')
            .toLowerCase();

        switch (method) {
            case 'get':
            try {
              handleGet();
            }
            catch (exc1) {
                echo("Exception. " + JSON.stringify(exc1));
            }
              break;
            case 'options':
              AspUtils.header('Allow: GET, OPTIONS');
              break;
            default:
              AspUtils.status('405 Method Not Allowed');
              break;
          }
    };

}());



try {
    processRequest();
}
catch(e) {
    Response.Write(e.message);
}

</script>
