# mORMot
Java client for Synopse mORMot server

Java REST client for fantastic Synopse API. This client is focused to Interface based services.


Example usage:
<pre>
SynClient client = new SynClient("127.0.0.1", 8080, "service", false);
if (client.login("User", "synopse")) {
        JSONArray params = new JSONArray().put(2).put(3);  // Add(2, 3)
        JSONObject response = client.call("Calculator.Add", params);
        System.out.println("Summ: " + response);
        client.logout();
}
</pre>
