import org.json.JSONArray;
import org.json.JSONObject;

public class SynTest {

    public static void main(String[] args) throws Exception {

        JSONArray params;
        JSONObject response;

        SynClient client = new SynClient("127.0.0.1", 8080, "service", false);
        if (client.login("User", "synopse")) {
            System.out.println("Timestamp: " + client.getTimestamp());

            //TEST 1
            params = new JSONArray().put(2).put(3);  // Add(2, 3)
            response = client.call("Calculator.Add", params);
            System.out.println("Summ: " + response);

            //TEST 2
            params = new JSONArray().put(".\\"); // GetFileList('.\')
            response = client.call("Calculator.GetFileList", params);
            System.out.println("List: " + response);

            client.logout();
        }
    }
}
