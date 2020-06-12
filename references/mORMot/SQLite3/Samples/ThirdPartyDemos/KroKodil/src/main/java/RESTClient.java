import okhttp3.*;
import org.json.JSONArray;
import org.json.JSONObject;

public class RESTClient {

    public int statusCode;
    private String server;
    public static final MediaType JSON = MediaType.get("application/json");

    private OkHttpClient client;

   public RESTClient(String server) {
       this.server = server;
       this.client = new OkHttpClient();
   }

   public String doGet(String url) throws Exception {
       Request request = new Request.Builder()
               .url(server + url)
               .build();

       Response response = client.newCall(request).execute();
       statusCode = response.code();
       if (response.body() != null) {
           return response.body().string();
       }
       return "";
   }

    public JSONObject doPost(String url, final JSONArray data) throws Exception {
        //System.out.println("Post data: " + data.toString() );
        RequestBody body = RequestBody.create(JSON, data.toString());
        Request request = new Request.Builder()
                .url(server + url)
                .post(body)
                .build();

        Response response = client.newCall(request).execute();
        statusCode = response.code();
        if (response.body() != null) {
            String json = response.body().string();
            return new JSONObject(json);
        }
        return new JSONObject();
    }
}
